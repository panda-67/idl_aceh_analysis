library(dplyr)
library(car) # Levene's test
library(ARTool) # ART untuk nonparametrik
library(lme4) # Mixed-effects
library(lmerTest) # p-value untuk lmer
library(emmeans) # Post-hoc

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


run_vaccine_completion_chisq <- function(data,
                                         intended_cols,
                                         year_column = NULL,
                                         year_filter = NULL) {
  # Optionally filter by year
  if (!is.null(year_column) && !is.null(year_filter)) {
    data <- data %>% filter(.data[[year_column]] %in% year_filter)
  }

  # Total number of children (rows)
  n_children <- nrow(data)

  # Calculate completion per vaccine group
  child_completion_matrix <- sapply(intended_cols, function(cols) {
    data %>%
      select(all_of(cols)) %>%
      mutate(
        completed = rowSums(!is.na(across(everything()))) == length(cols)
      ) %>%
      pull(completed)
  })

  # Observed completions
  observed_completion <- colSums(child_completion_matrix)

  # Expected proportions based on number of doses in each group
  expected_doses <- sapply(intended_cols, length)
  expected_prop <- expected_doses / sum(expected_doses)

  # Expected counts assuming equal number of children
  expected_completion <- rep(n_children, length(intended_cols))
  names(expected_completion) <- names(intended_cols)

  # Run chi-square test
  chi_test <- chisq.test(x = observed_completion, p = expected_prop)

  # Return a list of useful results
  return(list(
    observed_completion = observed_completion,
    expected_prop = expected_prop,
    expected_counts = expected_prop * sum(observed_completion),
    chi_test = chi_test
  ))
}
