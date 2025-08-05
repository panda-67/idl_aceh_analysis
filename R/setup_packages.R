required <- c(
  "readODS", "googlesheets4", "dplyr",
  "tidyr", "purrr", "here", "ggplot2", "ggpubr"
)

lapply(required, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
})
