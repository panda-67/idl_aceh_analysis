# Compute IDL percent, status, and year
idl_metrics <- function(df, required_vaccines) {
  library(dplyr)

  df <- df %>%
    mutate(
      idl_percent = (
        (HB0_doses >= 1) +
          (BCG_doses >= 1) +
          (OPV_doses >= 4) +
          (IPV_doses >= 1) +
          (DTP_doses >= 3) +
          (MR_doses >= 1)
      ) / 6 * 100,
      idl_status = case_when(
        idl_percent == 100 ~ "Lengkap",
        TRUE ~ "Tidak"
      )
    )

  # Assign IDL_year only for those with complete vaccination
  df$IDL_year <- apply(df, 1, function(r) {
    # Only calculate if fully complete
    if ((as.numeric(r["idl_percent"]) == 100)) {
      dates <- as.Date(unlist(r[intersect(required_vaccines, names(df))]))
      if (all(is.na(dates))) {
        return(NA_character_)
      }
      format(max(dates, na.rm = TRUE), "%Y")
    } else {
      NA_character_
    }
  })

  df
}
