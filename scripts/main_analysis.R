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

source(here("scripts/plots.R"))

# 1. Statistical tests
all_data[, c(endsWith(names(all_data), "_doses"))]

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
