read_sheet <- function(file_path, sheet_name) {
  read_ods(file_path, sheet = sheet_name)
}

parse_dates <- function(df, vaccine_groups) {
  cols <- c(unlist(vaccine_groups), "Tanggal Lahir Anak")
  for (col in cols) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.Date(df[[col]], format = "%d/%m/%Y"))
    }
  }
  df
}

filter_by_vaccine_year <- function(df, date_cols, years) {
  # Ensure date_cols is a character vector
  date_cols <- as.character(unlist(date_cols))

  # Subset to only existing date columns
  valid_cols <- date_cols[date_cols %in% names(df)]

  if (length(valid_cols) == 0) {
    return(df[0, ])
  } # Return empty tibble if no matches

  # Filter rows with any date in the given year range
  df %>%
    filter(
      if_any(
        all_of(valid_cols),
        ~ !is.na(.x) & format(.x, "%Y") %in% as.character(years)
      )
    )
}

count_doses <- function(df, vaccine_groups) {
  for (group in names(vaccine_groups)) {
    cols <- vaccine_groups[[group]]
    existing <- cols[cols %in% names(df)]
    df[[paste0(group, "_doses")]] <- if (length(existing) > 0) {
      rowSums(!is.na(df[, existing, drop = FALSE]))
    } else {
      0
    }
  }
  df
}

add_idl_status <- function(df) {
  df$idl_percent <- with(df, (
    (HB0_doses >= 1) +
      (BCG_doses >= 1) +
      (OPV_doses >= 4) +
      (IPV_doses >= 1) +
      (DTP_HB_Hib_doses >= 3) +
      (MR_doses >= 1)
  ) / 6 * 100)

  df$idl_status <- case_when(
    df$idl_percent == 100 ~ "Lengkap",
    df$idl_percent >= 50 ~ "Sebagian",
    TRUE ~ "Tidak"
  )
  df
}

extract_idl_year <- function(df, required_vaccines) {
  existing <- required_vaccines[required_vaccines %in% names(df)]
  df$IDL_year <- apply(df, 1, function(row) {
    dates <- as.Date(unlist(row[existing]))
    if (length(dates) == 0 || all(is.na(dates))) {
      return(NA_character_)
    }
    format(max(dates, na.rm = TRUE), "%Y")
  })
  df
}
