library(dplyr)
library(lubridate)

make_idl_completion_summary <- function(data, intend_cols, dob_col = "dob") {
  data %>%
    filter(idl_status == "Lengkap") %>%
    select("Nama Anak", "Kecamatan", all_of(dob_col), all_of(intend_cols)) %>%
    mutate(across(all_of(intend_cols), as.Date)) %>%
    rowwise() %>%
    mutate(
      first_date = min(c_across(all_of(intend_cols)), na.rm = TRUE),
      last_date = max(c_across(all_of(intend_cols)), na.rm = TRUE),
      span_days = as.integer(difftime(last_date, first_date, units = "days")),
      span_months = as.integer(span_days / 30.44),
      span_cross_year = lubridate::year(
        first_date
      ) != lubridate::year(last_date),
      age_months_completion = as.integer(
        lubridate::interval(.data[[dob_col]], last_date) %/% months(1)
      ),
      age_group = case_when(
        age_months_completion >= 0 &
          age_months_completion <= 11 ~ "0–11 bulan",
        age_months_completion >= 12 &
          age_months_completion <= 23 ~ "12–23 bulan",
        age_months_completion >= 24 &
          age_months_completion <= 59 ~ "24–59 bulan",
        TRUE ~ "Other"
      )
    ) %>%
    ungroup() %>%
    {
      tib <- .
      list(
        detail = tib,
        summary = tib %>%
          count(age_group) %>%
          mutate(percent = paste0(round(100 * n / sum(n), 1), "%"))
      )
    }
}

make_vaccine_age_table <- function(data, intend_cols, dob_col = "dob") {
  all_vaccine_cols <- unlist(intend_cols, use.names = FALSE)

  df <- data %>%
    select(all_of(dob_col), all_of(all_vaccine_cols)) %>%
    pivot_longer(
      cols = all_of(all_vaccine_cols),
      names_to = "dose_name",
      values_to = "date_given"
    ) %>%
    filter(!is.na(date_given)) %>%
    filter(date_given >= .data[[dob_col]]) %>%
    mutate(
      vaccine_group = purrr::map_chr(dose_name, function(dose) {
        grp <- names(intend_cols)[purrr::map_lgl(
          intend_cols, ~ any(dose %in% .x)
        )]
        grp[1]
      }),
      age_months = interval(.data[[dob_col]], date_given) %/% months(1)
    ) %>%
    filter(age_months <= 60) %>%
    count(vaccine_group, dose_name, age_months) %>%
    pivot_wider(
      names_from = age_months,
      values_from = n,
      values_fill = 0
    )

  # Build month columns sorted numerically
  month_cols <- as.character(sort(
    suppressWarnings(as.numeric(setdiff(names(df), c(
      "vaccine_group", "dose_name"
    ))))
  ))

  # Return with sorted columns
  df %>%
    select(vaccine_group, dose_name, all_of(month_cols))
}
