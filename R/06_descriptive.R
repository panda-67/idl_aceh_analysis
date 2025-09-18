library(dplyr)
library(tidyr)
library(ggpubr)
library(ggplot2)

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
      dose_name = factor(
        dose_name,
        levels = as.character(unlist(vaccine_groups)), ordered = TRUE
      )
    )

  ## 1️⃣ Facet by vaccine_group (sum over doses)
  df_group <- df_long %>%
    group_by(vaccine_group, age_months) %>%
    summarise(count = sum(count), .groups = "drop")

  p_group <- ggplot(df_group, aes(x = age_months, y = count)) +
    geom_col(fill = "#2c7bb6") +
    facet_wrap(~vaccine_group, scales = "free_y", ncol = 2) +
    labs(
      title = "Vaccination Counts by Group & Age (Years)",
      x = "Age (Years)",
      y = "Count"
    ) +
    scale_x_continuous(
      breaks = seq(0, max(df_group$age_months, na.rm = TRUE), by = 12),
      labels = function(x) paste0(x / 12, " yr") # 👈 here
    ) +
    theme_pubclean() +
    theme(text = element_text(family = "Times", size = 10))

  ##  Facet by idl_dose
  df_idl <- df_long %>%
    filter(dose_name %in% required_vaccines)

  p_idl <- ggplot(df_idl, aes(x = age_months, y = count)) +
    geom_col(fill = "#d7891c") +
    facet_wrap(~dose_name, scales = "free_y", ncol = 3) +
    labs(
      title = "Vaccination Counts by IDL Dose & Age (Years)",
      x = "Age (Years)",
      y = "Count"
    ) +
    scale_x_continuous(
      breaks = seq(0, max(df_group$age_months, na.rm = TRUE), by = 12),
      labels = function(x) paste0(x / 12, " yr") # 👈 here
    ) +
    theme_pubclean() +
    theme(text = element_text(family = "Times", size = 10))


  ## 2️⃣ Facet by dose_name (no sum)
  p_dose <- ggplot(df_long, aes(x = age_months, y = count)) +
    geom_col(fill = "#d7191c") +
    facet_wrap(~dose_name, scales = "free_y", ncol = 3) +
    labs(
      title = "Vaccination Counts by Dose & Age (Years)",
      x = "Age (Years)",
      y = "Count"
    ) +
    scale_x_continuous(
      breaks = seq(0, max(df_group$age_months, na.rm = TRUE), by = 12),
      labels = function(x) paste0(x / 12, " yr") # 👈 here
    ) +
    theme_pubclean() +
    theme(text = element_text(family = "Times", size = 10))

  list(by_group = p_group, by_idl = p_idl, by_dose = p_dose)
}

plot_percet <- function(data) {
  summary <- data %>%
    group_by(region, treatment_duration, district) %>%
    summarise(
      n = n(),
      mean_percent = mean(idl_percent, na.rm = TRUE),
      sd_percent = sd(idl_percent, na.rm = TRUE),
      .groups = "drop"
    )
  plot <- ggplot(
    summary,
    aes(x = treatment_duration, y = mean_percent, fill = region)
  ) +
    geom_col(position = position_dodge()) +
    geom_errorbar(
      aes(ymin = mean_percent - sd_percent, ymax = mean_percent + sd_percent),
      position = position_dodge(width = 0.9),
      width = 0.2
    ) +
    facet_wrap(~district) +
    scale_x_discrete(labels = c(one_year = "1 Tahun", two_year = "2 Tahun")) +
    scale_fill_manual(name = "", values = c(VIV = "#1b9e77", RC = "#d95f02")) +
    labs(
      title = "Persentase IDL per Durasi & Region",
      x = "Durasi Perlakuan",
      y = "Persentase IDL (%)"
    ) +
    theme_pubclean() +
    theme(
      legend.position = "right",
      text = element_text(family = "Times", size = 11)
    )

  list(summary = summary, plot = plot)
}

plot_biner <- function(data) {
  summary <- data %>%
    group_by(region, treatment_duration, district) %>%
    summarise(
      n = n(),
      lengkap_n = sum(idl_status == "Lengkap", na.rm = TRUE),
      lengkap_percent = 100 * lengkap_n / n,
      .groups = "drop"
    )

  plot <- ggplot(
    summary,
    aes(x = treatment_duration, y = lengkap_percent, fill = region)
  ) +
    geom_col(position = position_dodge()) +
    facet_wrap(~district) +
    scale_x_discrete(labels = c(one_year = "1 Tahun", two_year = "2 Tahun")) +
    scale_fill_manual(name = "", values = c(VIV = "#1b9e77", RC = "#d95f02")) +
    labs(
      title = "Proporsi Anak dengan IDL Lengkap",
      x = "Durasi Perlakuan",
      y = "Persentase Lengkap (%)"
    ) +
    theme_pubclean() +
    theme(
      legend.position = "right",
      text = element_text(family = "Times", size = 11)
    )

  list(summary = summary, plot = plot)
}
