# 0. Load library
library(here)

# 1. Source utils
source(here("R/00_utils.R"))
source(here("R/01_read_parse.R"))
source(here("R/02_dose_count.R"))
source(here("R/03_idl_metrics.R"))
source(here("R/04_vaccine_trends.R"))
source(here("R/05_idl_age_summary.R"))
source(here("R/06_descriptive.R"))
source(here("R/07_inferial_stats.R"))

# 2. Source config
source(here("R/variables.R"))
path <- here("data", "dataset.ods")

# 3. Build all_data
all_data <-
  read_and_parse(path, sheets, vaccine_groups, extra_dates) %>%
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
    ),
    dob = if_else(
      is.na(`Tanggal Lahir Anak`) & !is.na(`HB-0`),
      as.Date(`HB-0`),
      as.Date(`Tanggal Lahir Anak`)
    )
  ) %>%
  filter(as.Date(`HB-0`) >= as.Date(dob)) %>%
  select(any_of(required_cols))
