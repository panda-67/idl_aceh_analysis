# — Compute IDL percent, status, and year
idl_metrics <- function(df, required_vaccines) {
  library(dplyr)
  df <- df %>%
    mutate(
      idl_percent = (
        (HB0_doses >= 1) +
          (BCG_doses >= 1) +
          (OPV_doses >= 4) +
          (IPV_doses >= 1) +
          (DTP_HB_Hib_doses >= 3) +
          (MR_doses >= 1)
      ) / 6 * 100,
      idl_status = case_when(
        idl_percent == 100 ~ "Lengkap",
        idl_percent >= 50 ~ "Sebagian",
        TRUE ~ "Tidak"
      )
    )

  # derive year of last required vaccine
  df$IDL_year <- apply(df, 1, function(r) {
    dates <- as.Date(unlist(r[intersect(required_vaccines, names(df))]))
    if (all(is.na(dates))) {
      return(NA_character_)
    }
    format(max(dates, na.rm = TRUE), "%Y")
  })
  df
}
