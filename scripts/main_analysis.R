# 0. Pre
source(here::here("scripts/pre_analysis.R"))

idl_summary <- idl_age_summary(all_data, required_vaccines, dob_col = "dob")

# 1. Statistical tests
all_data[, c(endsWith(names(all_data), "_doses"))]

table(!is.na(all_data$idl_status))

all_data %>% count(IDL_year)

all_data[, c("dob", as.character(unlist(required_vaccines)))]

all_data$idl_status

chi_test <- run_vaccine_completion_chisq(
  data = all_data,
  intended_cols = vaccine_groups,
  year_column = NULL, # or "Tahun Lahir", or other column
  year_filter = NULL # or just 2023
)

library(lubridate)

vax_span <- all_data %>%
  select(all_of(required_vaccines)) %>%
  mutate(across(everything(), ~ as.Date(.))) %>%
  rowwise() %>%
  mutate(
    first_date = min(c_across(everything()), na.rm = TRUE),
    last_date = max(c_across(everything()), na.rm = TRUE),
    span_cross_year = year(first_date) != year(last_date)
  ) %>%
  ungroup()

table(vax_span$span_cross_year)

vaccine_df <- tibble(
  VaccineGroup = names(observed_completion),
  Completed = observed_completion,
  Missed = nrow(all_data) - observed_completion
) %>%
  mutate(Total = Completed + Missed)

raccine_order <- vaccine_df %>%
  mutate(CompletedProp = Completed / Total) %>%
  arrange(desc(CompletedProp)) %>%
  pull(VaccineGroup)

vaccine_df <- vaccine_df %>%
  pivot_longer(
    cols = c("Completed", "Missed"),
    names_to = "Status", values_to = "Count"
  ) %>%
  mutate(
    Proportion = Count / Total,
    Status = factor(Status, levels = c("Completed", "Missed")),
    VaccineGroup = factor(VaccineGroup, levels = vaccine_order)
  )

p_chi_sq <- ggplot(vaccine_df, aes(
  x = reorder(VaccineGroup, Proportion),
  y = Proportion, fill = Status
)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Status, nrow = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Completed" = "#1f77b4", # blue
    "Missed" = "orange"
  )) +
  labs(
    title = "Proportion of Children per Vaccine Group",
    x = "Vaccine Group",
    y = "Proportion of Children"
  ) +
  theme_pubclean() +
  theme(text = element_text(family = "Times", size = 10))

p_chi_sq

rmarkdown::render(
  input = here("scripts/md/vaccine-completion.Rmd"),
  output_dir = "reports/html"
)

rmarkdown::render(
  input = here("scripts/md/vaccine-completion.Rmd"),
  output_format = "pdf_document",
  output_dir = "reports/pdf"
)


library(rcompanion)
posthoc <- pairwiseNominalIndependence(
  table(
    all_data$region, all_data$idl_status
  ),
  method = "fdr"
)

library(gtsummary)
all_data %>%
  mutate(IDL_year = as.factor(IDL_year)) %>%
  tbl_summary(
    by = IDL_year,
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  )

library(brglm)
df <- all_data %>%
  mutate(
    completed = if_else(
      HB0_doses >= 1 & BCG_doses >= 1 & OPV_doses >= 4 &
        IPV_doses >= 1 & DTP_doses >= 3 & MR_doses >= 1, 1, 0
    )
  )

glm_firth <- brglm::brglm(
  completed ~ region,
  data = df, family = binomial("logit"), type = "AS_mean"
)
summary(glm_firth)

library(MASS)
df$idl_status <- factor(df$idl_status,
  ordered = TRUE,
  levels = c("Tidak Lengkap", "Sebagian", "Lengkap")
)

polr_model <- polr(idl_status ~ region, data = df, Hess = TRUE)
summary(polr_model)

df <- all_data %>%
  mutate(
    vaccine_score = rowMeans(across(
      all_of(required_vaccines),
      ~ !is.na(.)
    ), na.rm = TRUE)
  )

# Simple linear regression
lm_score <- lm(vaccine_score ~ region, data = df)
summary(lm_score)

library(ggplot2)
vaccine_long <- all_data %>%
  pivot_longer(
    cols = c(
      "HB-0", "BCG", "OPV 1", "OPV 2", "OPV 3", "OPV 4",
      "DTP-HB-Hib 1", "DTP-HB-Hib 2", "DTP-HB-Hib 3",
      "MR 1", "IPV 1"
    ),
    names_to = "vaccine", values_to = "date"
  ) %>%
  mutate(received = !is.na(date))

ggplot(vaccine_long, aes(x = IDL_year, fill = received)) +
  geom_bar(position = "fill") +
  facet_wrap(~vaccine) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Drop-off by Vaccine Over Years",
    y = "Proportion", fill = "Received"
  )
