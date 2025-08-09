library(dplyr)

run_vaccine_completion_chisq <- function(data,
                                         intended_cols,
                                         year_column = NULL,
                                         year_filter = NULL) {
  # Optionally filter by year
  if (!is.null(year_column) && !is.null(year_filter)) {
    data <- data %>% filter(.data[[year_column]] %in% year_filter)
  }

  # Total number of children (rows)
  n_children <- nrow(data)

  # Calculate completion per vaccine group
  child_completion_matrix <- sapply(intended_cols, function(cols) {
    data %>%
      select(all_of(cols)) %>%
      mutate(
        completed = rowSums(!is.na(across(everything()))) == length(cols)
      ) %>%
      pull(completed)
  })

  # Observed completions
  observed_completion <- colSums(child_completion_matrix)

  # Expected proportions based on number of doses in each group
  expected_doses <- sapply(intended_cols, length)
  expected_prop <- expected_doses / sum(expected_doses)

  # Expected counts assuming equal number of children
  expected_completion <- rep(n_children, length(intended_cols))
  names(expected_completion) <- names(intended_cols)

  # Run chi-square test
  chi_test <- chisq.test(x = observed_completion, p = expected_prop)

  # Return a list of useful results
  return(list(
    observed_completion = observed_completion,
    expected_prop = expected_prop,
    expected_counts = expected_prop * sum(observed_completion),
    chi_test = chi_test
  ))
}
