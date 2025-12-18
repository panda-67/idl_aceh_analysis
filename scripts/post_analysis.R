write.csv(all_data, here("output", "tables", "all_data.csv"), row.names = FALSE)
write.csv(
  idl$detail,
  here("output", "tables", "idl_detail.csv"),
  row.names = FALSE
)
write.csv(
  idl$summary,
  here("output", "tables", "idl_summary.csv"),
  row.names = FALSE
)
write.csv(
  age_table,
  here("output", "tables", "vaccine_by_month.csv"),
  row.names = FALSE
)

# if TRUE, that will include pdf report otherwise it only html report
render_report(TRUE)
