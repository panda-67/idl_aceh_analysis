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

sapply(df_all, class)

idl <- make_idl_completion_summary(
  df_all, required_vaccines, "dob"
)

age_table <- make_vaccine_age_table(
  df_all, vaccine_groups, "dob"
)

## $by_group, $by_idl, $by_dose
age_plots <- plot_vaccine_age(age_table)

p_idl_trend <- ggplot(
  trend_data(df_all),
  aes(x = year_birth, y = percent, fill = idl_status)
) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = paste0(round(percent, 1), "%\n(n=", n, ")")),
    vjust = -0.5, size = 3
  ) +
  # geom_hline(yintercept = 90, linetype = "dashed", color = "red") +
  # annotate("text",
  #   x = 1, y = 92, label = "Target 90%",
  #   hjust = 0, color = "red", size = 3.5
  # ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100)
  ) +
  scale_fill_manual(
    values = c(
      "Lengkap" = "#4CAF50",
      "Tidak" = "#FFC107"
    )
  ) +
  labs(
    title = "Tren Cakupan IDL Anak per Tahun Lahir dan Status",
    x = "Tahun Lahir Anak",
    y = "Persentase Anak per Status",
    fill = "Status"
  ) +
  theme_pubclean() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(family = "Times", size = 10)
  )

p_dropoff <- ggplot(
  dropoff_data(df_all, as.character(unlist(required_vaccines))),
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
  theme_pubclean() +
  theme(
    legend.position = "right",
    text = element_text(family = "Times", size = 10)
  )


write.csv(
  df_all, "/home/arba/Downloads/abes_idl.csv",
  row.names = FALSE
)
write.csv(
  idl$detail, "/home/arba/Downloads/abes_idl_detail.csv",
  row.names = FALSE
)
write.csv(
  idl$summary, "/home/arba/Downloads/abes_idl_summary.csv",
  row.names = FALSE
)

rmarkdown::render(
  input = here("scripts/md/abes.Rmd"),
  output_format = "pdf_document",
  output_dir = "reports/pdf"
)
