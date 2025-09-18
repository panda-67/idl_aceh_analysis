library(here)

source(here("R/00_utils.R"))
source(here("R/01_read_parse.R"))
source(here("R/02_dose_count.R"))
source(here("R/03_idl_metrics.R"))
source(here("R/04_vaccine_trends.R"))
source(here("R/05_idl_age_summary.R"))
source(here("R/06_descriptive.R"))
source(here("R/variables.R"))


files <- list.files(
  "/home/arba/CodeLabs/data/drive_xl/output/",
  pattern = "\\.xlsx$|\\.ods$", full.names = TRUE
)

selected_cols <- c(
  "nama anak", "tgl lahir", "jenis kelamin",
  # as.character(unlist(vaccine_groups)),
  vaccine_order,
  "HB0_doses", "BCG_doses", "OPV_doses", "IPV_doses",
  "DTP_doses", "MR_doses", "PCV_doses", "RV_doses",
  "Kecamatan", "IDL_year", "idl_percent", "idl_status"
)

df_all <- read_and_parse_files(
  paths = files,
  vaccine_groups = vaccine_order,
  extra_dates = c("tgl lahir", "IDL")
) %>%
  count_doses(vaccine_groups) %>%
  idl_metrics(required_vaccines) %>%
  select(any_of(selected_cols)) %>%
  rename(
    `Tanggal Lahir Anak` = `tgl lahir`,
    `Nama Anak` = `nama anak`
  ) %>%
  mutate(dob = if_else(
    is.na(`Tanggal Lahir Anak`) & !is.na(`HB-0`),
    as.Date(`HB-0`),
    as.Date(`Tanggal Lahir Anak`)
  ))
