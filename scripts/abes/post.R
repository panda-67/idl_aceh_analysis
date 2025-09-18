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
