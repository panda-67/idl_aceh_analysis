# Count doses per group
count_doses <- function(df, vaccine_groups) {
  library(dplyr)
  for (grp in names(vaccine_groups)) {
    cols <- intersect(vaccine_groups[[grp]], names(df))
    df[[paste0(grp, "_doses")]] <- if (length(cols)) {
      rowSums(!is.na(df[cols]))
    } else {
      0
    }
  }
  df
}
