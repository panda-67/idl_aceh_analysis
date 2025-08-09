required <- c(
  "readODS", "googlesheets4", "dplyr", "tidyr", "purrr", "here", "ggplot2",
  "ggpubr", "lmerTest", "lme4", "ARTool"
)

lapply(required, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
})

render_report <- function(pdf = FALSE) {
  if (pdf) {
    rmarkdown::render(
      input = here("scripts/md/vaccine-completion.Rmd"),
      output_format = "pdf_document",
      output_dir = "reports/pdf"
    )
  }

  rmarkdown::render(
    input = here("scripts/md/vaccine-completion.Rmd"),
    output_dir = "reports/html"
  )
}
