library(dplyr)
library(car) # Levene's test
library(ARTool) # ART untuk nonparametrik
library(lme4) # Mixed-effects
library(lmerTest) # p-value untuk lmer
library(emmeans) # Post-hoc
library(broom)

# 1. Cek Asumsi Normalitas & Homogenitas
# ----------------------------
check_assumptions <- function(data) {
  # Uji normalitas per kombinasi faktor
  normality <- data %>%
    group_by(region, district) %>%
    summarise(
      p_shapiro = tryCatch(
        shapiro.test(idl_percent)$p.value,
        error = function(e) NA_real_
      ),
      .groups = "drop"
    )

  # Levene's test untuk homogenitas varians
  lev_p <- leveneTest(
    idl_percent ~ region * district,
    data = data
  )$`Pr(>F)`[1]

  list(
    normality = normality,
    normal_pass = all(normality$p_shapiro > 0.05, na.rm = TRUE),
    levene_pass = lev_p > 0.05
  )
}

cont_test <- function(assump, data) {
  if (assump$normal_pass || assump$levene_pass) {
    cat("\n✅ Data memenuhi asumsi ANOVA\n")

    df_aov <- data
    mod <- aov(
      idl_percent ~ region * treatment_duration * district,
      data = df_aov
    )

    result <- summary(mod)

    # Post-hoc untuk interaksi signifikan
    emm <- emmeans(mod, ~ region * treatment_duration * district)
    bonferroni <- pairs(emm, adjust = "bonferroni")
  } else {
    cat("\n⚠ Data tidak memenuhi asumsi, pakai ART (nonparametrik)\n")

    df_art <- data %>%
      # filter(kecamatan != "kbj") %>%
      mutate(
        region = factor(region, levels = c("VIV", "RC")),
        district = factor(district, levels = c("Banda Aceh", "Aceh Besar")),
        treatment_duration = factor(
          treatment_duration,
          levels = c("one_year", "two_year")
        )
      )

    mod <- art(idl_percent ~ region * district, data = df_art)

    result <- anova(mod)

    # Post-hoc nonparametrik
    emm <- emmeans(artlm(mod, "region:district"), ~ region * district)
    bonferroni <- pairs(emm, adjust = "bonferroni")
  }

  list(
    result = result,
    post_hoc = bonferroni
  )
}

# biner_test <- function(
#     data,
#     status_col = "idl_status",
#     region_col = "region",
#     district_col = "district") {
#   # Pastikan faktor
#   data <- data %>%
#     mutate(
#       !!status_col := factor(.data[[status_col]]),
#       !!region_col := factor(.data[[region_col]]),
#       !!district_col := factor(.data[[district_col]])
#     )
#
#   # Uji Chi-square / Fisher untuk region
#   tab_region <- table(data[[status_col]], data[[region_col]])
#   if (any(tab_region < 5)) {
#     test_region <- fisher.test(tab_region)
#     test_region$type <- "Fisher"
#   } else {
#     test_region <- chisq.test(tab_region)
#     test_region$type <- "Chi-square"
#   }
#
#   # Uji Chi-square / Fisher untuk district
#   tab_district <- table(data[[status_col]], data[[district_col]])
#   if (any(tab_district < 5)) {
#     test_district <- fisher.test(tab_district)
#     test_district$type <- "Fisher"
#   } else {
#     test_district <- chisq.test(tab_district)
#     test_district$type <- "Chi-square"
#   }
#
#   # Model regresi logistik
#   form <- as.formula(paste(status_col, "~", region_col, "*", district_col))
#   model <- glm(form, data = data, family = binomial)
#   or_table <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE)
#
#   list(
#     chi_region = list(test = test_region, table = tab_region),
#     chi_district = list(test = test_district, table = tab_district),
#     logistic_model = summary(model),
#     odds_ratio = or_table
#   )
# }

biner_test <- function(
  data,
  status_col = "idl_status",
  region_col = "region",
  district_col = "district",
  method = c("firth", "bayes")
) {
  method <- match.arg(method)

  # ---- 1. Sanitize data (NO NA allowed)
  data <- data %>%
    dplyr::filter(
      !is.na(.data[[status_col]]),
      !is.na(.data[[region_col]]),
      !is.na(.data[[district_col]])
    ) %>%
    dplyr::mutate(
      !!status_col := factor(.data[[status_col]]),
      !!region_col := factor(.data[[region_col]]),
      !!district_col := factor(.data[[district_col]])
    )

  # ---- 2. Chi-square / Fisher: region
  tab_region <- table(data[[status_col]], data[[region_col]])
  test_region <- if (any(tab_region < 5)) {
    out <- fisher.test(tab_region)
    out$type <- "Fisher"
    out
  } else {
    out <- chisq.test(tab_region)
    out$type <- "Chi-square"
    out
  }

  # ---- 3. Chi-square / Fisher: district
  tab_district <- table(data[[status_col]], data[[district_col]])
  test_district <- if (any(tab_district < 5)) {
    out <- fisher.test(tab_district)
    out$type <- "Fisher"
    out
  } else {
    out <- chisq.test(tab_district)
    out$type <- "Chi-square"
    out
  }

  # ---- 4. Model formula
  form <- as.formula(
    paste(status_col, "~", region_col, "*", district_col)
  )

  # ---- 5. Logistic model (separation-safe)
  if (method == "firth") {
    mod <- logistf::logistf(form, data = data)

    or_table <- broom::tidy(
      mod,
      exponentiate = TRUE,
      conf.int = TRUE
    )

    model_out <- mod
  } else {
    mod <- brms::brm(
      form,
      data = data,
      family = bernoulli(),
      prior = brms::prior(normal(0, 2.5), class = "b"),
      refresh = 0
    )

    or_table <- brms::posterior_summary(mod, exponentiate = TRUE)
    model_out <- mod
  }

  # ---- 6. Return clean structure
  list(
    chi_region = list(test = test_region, table = tab_region),
    chi_district = list(test = test_district, table = tab_district),
    model_type = method,
    logistic_model = model_out,
    odds_ratio = or_table
  )
}

run_vaccine_completion_chisq <- function(data, intended_cols) {
  # Total anak
  n_children <- nrow(data)

  # Hitung kelengkapan tiap anak per kelompok vaksin
  child_completion_matrix <- sapply(intended_cols, function(cols) {
    data %>%
      select(all_of(cols)) %>%
      mutate(
        completed = rowSums(!is.na(across(everything()))) == length(cols)
      ) %>%
      pull(completed)
  })

  # Observed: berapa anak yang benar-benar melengkapi tiap kelompok
  observed_completion <- colSums(child_completion_matrix)

  # Expected: semua anak diharapkan melengkapi semua kelompok vaksin
  expected_completion <- rep(n_children, length(intended_cols))
  names(expected_completion) <- names(intended_cols)

  # Ubah expected menjadi proporsi untuk chisq.test
  expected_proportions <- expected_completion / sum(expected_completion)

  # Uji Chi-square goodness-of-fit
  chi_test <- chisq.test(
    x = observed_completion,
    p = expected_proportions
  )

  list(
    observed_completion = observed_completion,
    expected_proportions = expected_proportions,
    chi_test = chi_test
  )
}
