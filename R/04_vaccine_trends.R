# — Trend, drop-off, outliers, and chi-square helpers
library(dplyr)
library(tidyr)
library(ggplot2)

idl_summary <- function(df) {
  df %>%
    filter(!is.na(region), !is.na(idl_status)) %>%
    count(region, idl_status) %>%
    pivot_wider(names_from = idl_status, values_from = n, values_fill = 0)
}

trend_data <- function(df) {
  df %>%
    filter(!is.na(IDL_year)) %>%
    count(IDL_year, idl_status) %>%
    group_by(IDL_year) %>%
    mutate(percent = n / sum(n) * 100)
}

dropoff_data <- function(df, required_vaccines) {
  df %>%
    summarise(across(all_of(required_vaccines), ~ mean(!is.na(.)) * 100)) %>%
    pivot_longer(everything(),
      names_to = "Vaccine",
      values_to = "CompletionRate"
    )
}
