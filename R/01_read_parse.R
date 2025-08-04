# R/01_read_parse.R

library(readODS)
library(dplyr)
library(purrr)
library(here)

read_and_parse <- function(path, sheets, vaccine_groups, extra_dates = NULL) {
  # 1. What columns we’ll attempt to parse
  all_date_cols <- unique(c(unlist(vaccine_groups), extra_dates))
  # 2. A regex for dd/mm/YYYY
  date_pattern <- "^\\s*\\d{1,2}/\\d{1,2}/\\d{4}\\s*$"

  map_dfr(
    .x = names(sheets),
    .f = function(kec) {
      df <- read_ods(path, sheet = sheets[[kec]])

      # only try to parse cols that actually exist
      target_cols <- intersect(all_date_cols, names(df))

      for (col in target_cols) {
        vec <- df[[col]]
        # prepare a Date vector of NAs
        parsed_vec <- rep(as.Date(NA), length(vec))

        # only parse character/factor entries matching dd/mm/YYYY
        if (is.factor(vec)) vec <- as.character(vec)
        if (is.character(vec)) {
          ok <- grepl(date_pattern, vec)
          parsed_dates <- as.Date(vec[ok], "%d/%m/%Y")
          parsed_vec[ok] <- parsed_dates
        }

        df[[col]] <- parsed_vec
      }

      df$kecamatan <- kec
      df
    },
    .id = "sheet"
  )
}
