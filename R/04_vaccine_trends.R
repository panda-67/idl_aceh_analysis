# — Trend, drop-off, outliers, and chi-square helpers
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggplot2)

idl_summary <- function(df) {
  df %>%
    filter(!is.na(region), !is.na(idl_status)) %>%
    count(region, idl_status) %>%
    pivot_wider(names_from = idl_status, values_from = n, values_fill = 0)
}

trend_data <- function(df) {
  df %>%
    mutate(year_birth = format(as.Date(dob), "%Y")) %>%
    filter(!is.na(year_birth)) %>%
    group_by(year_birth, idl_status) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(year_birth) %>%
    mutate(percent = n / sum(n) * 100)
}

dropoff_data <- function(df, cols) {
  df %>%
    summarise(
      across(
        all_of(cols),
        .fns = list(
          Completion = ~ mean(!is.na(.)) * 100,
          Dropoff = ~ mean(is.na(.)) * 100
        ),
        .names = "{col}_{fn}"
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("Vaccine", "Status"),
      names_sep = "_",
      values_to = "Rate"
    ) %>%
    pivot_wider(names_from = Status, values_from = Rate) %>%
    arrange(Dropoff) %>% # ← order from least to most drop-off
    pivot_longer(
      cols = c(Completion, Dropoff),
      names_to = "Status",
      values_to = "Rate"
    ) %>%
    mutate(
      Vaccine = factor(Vaccine, levels = as.character(unlist(vaccine_groups)))
    )
}

chi_sq_data <- function(data, chi_test) {
  vaccine_df <- tibble(
    VaccineGroup = names(chi_test$observed_completion),
    Completed = chi_test$observed_completion,
    Missed = nrow(data) - chi_test$observed_completion
  ) %>%
    mutate(Total = Completed + Missed)

  vaccine_order <- vaccine_df %>%
    mutate(CompletedProp = Completed / Total) %>%
    arrange(desc(CompletedProp)) %>%
    pull(VaccineGroup)

  vaccine_df <- vaccine_df %>%
    pivot_longer(
      cols = c("Completed", "Missed"),
      names_to = "Status",
      values_to = "Count"
    ) %>%
    mutate(
      Proportion = Count / Total * 100,
      Status = factor(Status, levels = c("Completed", "Missed")),
      VaccineGroup = factor(VaccineGroup, levels = vaccine_order)
    )

  vaccine_df
}
