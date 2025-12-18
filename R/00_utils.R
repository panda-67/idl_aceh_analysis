required <- c(
  "readODS",
  "readxl",
  # "logistf",
  "googlesheets4",
  "dplyr",
  "tidyr",
  "purrr",
  "here",
  "ggplot2",
  "ggpubr",
  "lmerTest",
  "lme4",
  "ARTool",
  "lubridate",
  "rmarkdown"
)

lapply(required, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
})

normalize_name <- function(x) gsub(" ", "_", x)

render_report <- function(pdf = FALSE) {
  if (pdf) {
    rmarkdown::render(
      input = here("scripts/md/vaccine-completion.Rmd"),
      output_format = "pdf_document",
      output_dir = "reports/pdf"
    )
    # rmarkdown::render(
    #   input = here("scripts/md/vaccine-completion.Rmd"),
    #   output_format = "word_document",
    #   output_dir = "reports/doc"
    # )
  }

  rmarkdown::render(
    input = here("scripts/md/vaccine-completion.Rmd"),
    output_dir = "reports/html"
  )
}
