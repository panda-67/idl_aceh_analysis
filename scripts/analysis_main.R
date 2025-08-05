# 0. Load library
library(here)

# 1. Source utils
source(here("R/setup_packages.R"))
source(here("R/01_read_parse.R"))
source(here("R/02_dose_count.R"))
source(here("R/03_idl_metrics.R"))
source(here("R/04_vaccine_trends.R"))

# 2. Source config
source(here("R/variables.R"))
path <- here("data", "dataset.ods")

# 3. Build all_data
all_data <- read_and_parse(path, sheets, vaccine_groups, extra_dates) %>%
  count_doses(vaccine_groups) %>%
  idl_metrics(required_vaccines) %>%
  mutate(
    region = case_when(
      kecamatan %in% unlist(region_groups$RC) ~ "RC",
      kecamatan %in% unlist(region_groups$VIV) ~ "VIV",
      kecamatan %in% region_groups$control ~ "Control",
      TRUE ~ NA_character_
    ),
    district = case_when(
      kecamatan %in% region_groups$banda_aceh ~ "Banda Aceh",
      kecamatan %in% region_groups$aceh_besar ~ "Aceh Besar",
      TRUE ~ NA_character_
    ),
    treatment_duration = case_when(
      kecamatan %in% region_groups$RC$one_year ~ "one_year",
      kecamatan %in% region_groups$RC$two_year ~ "two_year",
      kecamatan %in% region_groups$VIV$one_year ~ "one_year",
      kecamatan %in% region_groups$VIV$two_year ~ "two_year",
      TRUE ~ NA_character_
    )
  )

# 4. Trim to only needed columns
all_data <- all_data %>% select(any_of(required_cols))

write.csv(all_data, here("output", "tables", "all_data.csv"), row.names = FALSE)

# 5. Statistical tests
chi_all <- chisq.test(as.matrix(idl_summary(all_data)[, -1]))
chi_2024 <- chisq.test(table(
  filter(all_data, IDL_year == "2024")$region,
  filter(all_data, IDL_year == "2024")$idl_status
))



# 6. Plots
p_idl_trend <- ggplot(
  trend_data(all_data),
  aes(x = year_birth, y = percent, fill = idl_status)
) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(percent, 1), "%\n(n=", n, ")")),
    vjust = -0.5, size = 3
  ) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "red") +
  annotate("text",
    x = 1, y = 92, label = "Target 90%",
    hjust = 0, color = "red", size = 3.5
  ) +
  facet_wrap(~idl_status, ncol = 1) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100)
  ) +
  scale_fill_manual(
    values = c(
      "Lengkap" = "#4CAF50",
      "Sebagian" = "#FFC107",
      "Tidak" = "#F44336"
    )
  ) +
  labs(
    title = "Tren Cakupan IDL Anak per Tahun Lahir dan Status",
    x = "Tahun Lahir Anak",
    y = "Persentase Anak per Status"
  ) +
  theme_pubclean() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_dropoff <- ggplot(
  dropoff_data(all_data, required_vaccines),
  aes(x = Vaccine, y = Rate, fill = Status)
) +
  geom_col(position = position_dodge(width = 0.7)) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Completion" = "#1f77b4", # blue
      "Dropoff"    = "#d62728" # red
    )
  ) +
  labs(
    title = "Vaccine Completion vs Drop-off (Ordered by Drop-off)",
    x = NULL,
    y = "Percentage (%)"
  ) +
  theme_pubclean()

# 7. Export
ggsave(here("output", "plots", "trend_idl.png"), p_idl_trend,
  width = 8, height = 12
)

write.csv(idl_summary(all_data),
  here("output", "tables", "idl_summary.csv"),
  row.names = FALSE
)
