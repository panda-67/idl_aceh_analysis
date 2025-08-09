# 0. Pre
source(here::here("scripts/pre_analysis.R"))

idl <- make_idl_completion_summary(all_data, required_vaccines)
age_table <- make_vaccine_age_table(all_data, vaccine_groups)

write.csv(idl$detail, here("output", "tables", "idl_detail.csv"),
  row.names = FALSE
)

write.csv(idl$summary, here("output", "tables", "idl_summary.csv"),
  row.names = FALSE
)

# ===>>>> HERE  <<<<===
source(here("scripts/plots.R"))

# 1. Statistical tests
names(all_data)
all_data[, c(
  "idl_status", names(all_data)[endsWith(names(all_data), "_doses")]
)]

all_data <- all_data %>% filter(kecamatan != "kbj")

assump <- check_assumptions(all_data)
print(assump$normality)

# 2. Pilih Metode Analisis
# ----------------------------
if (assump$normal_pass && assump$levene_pass) {
  cat("\n✅ Data memenuhi asumsi ANOVA\n")

  df_aov <- all_data
  mod <- aov(idl_percent ~ region * treatment_duration * district,
    data = df_aov
  )
  print(summary(mod))

  # Post-hoc untuk interaksi signifikan
  emm <- emmeans(mod, ~ region * treatment_duration * district)
  print(pairs(emm, adjust = "tukey"))
} else {
  cat("\n⚠ Data tidak memenuhi asumsi, pakai ART (nonparametrik)\n")

  df_art <- all_data %>%
    filter(kecamatan != "kbj") %>%
    mutate(
      region = factor(region, levels = c("VIV", "RC")),
      district = factor(district, levels = c("Banda Aceh", "Aceh Besar")),
      treatment_duration = factor(
        treatment_duration,
        levels = c("one_year", "two_year")
      )
    )

  mod <- art(idl_percent ~ region * district, data = df_art)
  print(anova(mod))

  # Post-hoc nonparametrik
  emm <- emmeans(artlm(mod, "region:district"), ~ region * district)
  print(pairs(emm, adjust = "bonferroni"))
}

this
# ====>>>> Lanjut binary <<<<====


chi_test <- run_vaccine_completion_chisq(
  data = all_data,
  intended_cols = vaccine_groups,
  year_column = NULL, # or "Tahun Lahir", or other column
  year_filter = NULL # or just 2023
)

vaccine_df <- tibble(
  VaccineGroup = names(observed_completion),
  Completed = observed_completion,
  Missed = nrow(all_data) - observed_completion
) %>%
  mutate(Total = Completed + Missed)

vaccine_order <- vaccine_df %>%
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
