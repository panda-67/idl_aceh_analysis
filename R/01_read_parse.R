library(readODS)
library(googlesheets4)
library(dplyr)
library(purrr)

read_and_parse <- function(path, sheets, vaccine_groups, extra_dates = NULL) {
  # 0. Google Sheet Id
  sheet_id <- "1DpZUSfL5eouUMygS25cqNmRvcFvgh4y1wEunqS3uTxk"
  # 1. What columns we’ll attempt to parse
  all_date_cols <- unique(c(unlist(vaccine_groups), extra_dates))
  # 2. A regex for dd/mm/YYYY
  date_pattern <- "^\\s*\\d{1,2}/\\d{1,2}/\\d{4}\\s*$"
  # 3. Relevant year of vaccined
  min_allowed_date <- as.Date("2017-01-01")
  max_allowed_date <- as.Date("2024-12-31")

  ## main function
  map_dfr(
    .x = names(sheets),
    .f = function(kec) {
      # df <- read_sheet(sheet_id, sheet = sheets[[kec]])
      df <- read_ods(path, sheet = sheets[[kec]])

      # only try to parse cols that actually exist
      target_cols <- intersect(all_date_cols, names(df))

      for (col in target_cols) {
        vec <- df[[col]]

        if (is.factor(vec)) vec <- as.character(vec)

        parsed_vec <- rep(as.Date(NA), length(vec))

        if (is.character(vec)) {
          vec <- trimws(vec)
          vec <- gsub("^\\?", "", vec)

          ok <- grepl(date_pattern, vec)
          possible_dates <-
            suppressWarnings(as.Date(vec[ok], format = "%d/%m/%Y"))

          # Check for valid and <= 2025-12-31
          valid <- !is.na(possible_dates) & possible_dates >= min_allowed_date &
            possible_dates <= max_allowed_date
          parsed_vec[which(ok)[valid]] <- possible_dates[valid]
        }

        df[[col]] <- parsed_vec
      }

      df$kecamatan <- kec

      df
    },
    .id = "sheet"
  )
}

read_and_parse_files <- function(paths, vaccine_groups, extra_dates = NULL) {
  # 0. Normalize name
  normalize_name <- function(x) gsub("_", " ", x)

  # 1. What columns we’ll attempt to parse
  all_date_cols <- unique(c(unlist(vaccine_groups), extra_dates))

  # 2. A regex for dd/mm/YYYY
  date_pattern <- "^\\s*\\d{1,2}/\\d{1,2}/\\d{4}\\s*$"

  # 3. Relevant year of vaccined
  min_allowed_date <- as.Date("2019-01-01")
  max_allowed_date <- as.Date("2024-12-31")

  map_dfr(
    .x = paths,
    .f = function(path) {
      # read Excel/ODS (pick based on extension)
      if (grepl("\\.ods$", path, ignore.case = TRUE)) {
        df <- readODS::read_ods(path)
      } else {
        df <- readxl::read_excel(path)
      }

      # normalize columns name
      names(df) <- normalize_name(names(df))

      # only try to parse cols that actually exist
      target_cols <- intersect(all_date_cols, names(df))

      for (col in target_cols) {
        vec <- df[[col]]

        if (is.factor(vec)) vec <- as.character(vec)

        parsed_vec <- rep(as.Date(NA), length(vec))

        if (is.character(vec)) {
          vec <- trimws(vec)
          vec <- gsub("^\\?", "", vec)

          ok <- grepl(date_pattern, vec)
          possible_dates <-
            suppressWarnings(as.Date(vec[ok], format = "%d/%m/%Y"))

          valid <- !is.na(possible_dates) &
            possible_dates >= min_allowed_date &
            possible_dates <= max_allowed_date

          parsed_vec[which(ok)[valid]] <- possible_dates[valid]
        }

        df[[col]] <- parsed_vec
      }

      df
    },
    .id = "file"
  )
}
