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

plot_vaccine_age <- function(vax_table) {
  # Get month columns in numeric order
  month_cols <- as.character(sort(
    suppressWarnings(as.numeric(setdiff(
      names(vax_table),
      c("vaccine_group", "dose_name")
    )))
  ))

  # Convert wide → long
  df_long <- vax_table %>%
    pivot_longer(
      cols = all_of(month_cols),
      names_to = "age_months",
      values_to = "count"
    ) %>%
    filter(age_months <= 36) %>%
    mutate(
      age_months = as.numeric(age_months),
      dose_name = factor(dose_name, levels = vaccine_order, ordered = TRUE)
    )

  ## 1️⃣ Facet by vaccine_group (sum over doses)
  df_group <- df_long %>%
    group_by(vaccine_group, age_months) %>%
    summarise(count = sum(count), .groups = "drop")

  p_group <- ggplot(df_group, aes(x = age_months, y = count)) +
    geom_col(fill = "#2c7bb6") +
    facet_wrap(~vaccine_group, scales = "free_y", ncol = 2) +
    labs(
      title = "Vaccination Counts by Group & Age (Months)",
      x = "Age (Months)",
      y = "Count"
    ) +
    theme_pubclean()

  ##  Facet by idl_dose
  df_idl <- df_long %>%
    filter(dose_name %in% required_vaccines)

  p_idl <- ggplot(df_idl, aes(x = age_months, y = count)) +
    geom_col(fill = "#d7891c") +
    facet_wrap(~dose_name, scales = "free_y", ncol = 3) +
    labs(
      title = "Vaccination Counts by IDL Dose & Age (Months)",
      x = "Age (Months)",
      y = "Count"
    ) +
    theme_pubclean()

  ## 2️⃣ Facet by dose_name (no sum)
  p_dose <- ggplot(df_long, aes(x = age_months, y = count)) +
    geom_col(fill = "#d7191c") +
    facet_wrap(~dose_name, scales = "free_y", ncol = 4) +
    labs(
      title = "Vaccination Counts by Dose & Age (Months)",
      x = "Age (Months)",
      y = "Count"
    ) +
    theme_pubclean()

  list(by_group = p_group, by_idl = p_idl, by_dose = p_dose)
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
          Dropoff    = ~ mean(is.na(.)) * 100
        ),
        .names = "{col}_{fn}"
      )
    ) %>%
    pivot_longer(
      cols      = everything(),
      names_to  = c("Vaccine", "Status"),
      names_sep = "_",
      values_to = "Rate"
    ) %>%
    pivot_wider(names_from = Status, values_from = Rate) %>%
    arrange(Dropoff) %>% # ← order from least to most drop-off
    pivot_longer(
      cols = c(Completion, Dropoff), names_to = "Status",
      values_to = "Rate"
    ) %>%
    mutate(Vaccine = factor(Vaccine, levels = unique(Vaccine)))
}
