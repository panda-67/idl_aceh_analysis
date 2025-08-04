# R/05_filters.R

filter_by_vaccine_year <- function(df, date_cols, years = 2016:2025) {
  # 1. Flatten your incoming list and intersect with df names
  wanted <- as.character(unlist(date_cols))
  candidates <- intersect(wanted, names(df))
  if (length(candidates) == 0) {
    return(df[0, ])
  }

  # 2. Keep only those that are Date class
  is_date <- sapply(df[candidates], inherits, what = "Date")
  valid_cols <- candidates[is_date]
  if (length(valid_cols) == 0) {
    return(df[0, ])
  }

  # 3. Perform the filter
  df %>%
    filter(
      if_any(
        all_of(valid_cols),
        ~ !is.na(.x) & format(.x, "%Y") %in% as.character(years)
      )
    )
}

filter_by_idl_year <- function(df, min_year = 2016, max_year = 2025) {
  df %>%
    filter(
      !is.na(IDL_year),
      as.numeric(IDL_year) >= min_year,
      as.numeric(IDL_year) <= max_year
    )
}
