# 0. Load library
library(ggplot2)
library(ggpubr)
library(dplyr)
library(here)

# 1. Source utils
source(here("R/01_read_parse.R"))
source(here("R/02_dose_count.R"))
source(here("R/03_idl_metrics.R"))
source(here("R/04_vaccine_trends.R"))
source(here("R/05_filters.R"))

# 2. Source config
source(here("R/variables.R"))
path <- here("data", "dataset.ods")

# 3. Build all_data
# 3a. Read & parse everything
all_data <- read_and_parse(path, sheets, vaccine_groups, extra_dates)

# 3b. Filter out any rows whose vaccine dates all fall outside 2016–2025
# all_data <- filter_by_vaccine_year(
#   all_data,
#   unlist(vaccine_groups),
#   years = 2016:2025
# )

# 3c. Build other data
all_data <- all_data %>%
  count_doses(vaccine_groups) %>%
  idl_metrics(required_vaccines) %>%
  filter_by_idl_year(2016, 2025) %>%
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

# 5. Statistical tests
chi_all <- chisq.test(as.matrix(idl_summary(all_data)[, -1]))
chi_2024 <- chisq.test(table(
  filter(all_data, IDL_year == "2024")$region,
  filter(all_data, IDL_year == "2024")$idl_status
))

# 6. Plots
p_trend <- ggplot(
  trend_data(all_data),
  aes(IDL_year, percent, fill = idl_status)
) +
  geom_col(position = "stack") +
  theme_pubclean()

p_dropoff <- ggplot(
  dropoff_data(all_data, required_vaccines),
  aes(
    reorder(Vaccine, -CompletionRate),
    CompletionRate
  )
) +
  geom_col() +
  coord_flip() +
  theme_pubclean()

# 7. Export
ggsave(here("output", "plots", "trend_idl.png"), p_trend, width = 8, height = 5)
write.csv(idl_summary(all_data),
  here("output", "tables", "idl_summary.csv"),
  row.names = FALSE
)
