# 03_validate.R
# Check CEDEN output for common submission errors before export.
# Prints a summary of issues; does not stop the pipeline.

library(dplyr)

validate_chemistry <- function(df) {
  issues <- list()

  # Required fields must not be NA
  required <- c("StationCode", "SampleDate", "Analyte", "Result",
                "ResQualCode", "Units", "MatrixName", "MethodName")
  for (col in required) {
    n_missing <- sum(is.na(df[[col]]))
    if (n_missing > 0)
      issues[[col]] <- glue::glue("{n_missing} rows missing {col}")
  }

  # Result must be numeric
  if (any(!is.numeric(df$Result)))
    issues[["Result_type"]] <- "Result column contains non-numeric values"

  # ND results should equal RL
  nd_rows <- df %>% filter(ResQualCode == "ND")
  bad_nd  <- nd_rows %>% filter(Result != RL & !is.na(RL))
  if (nrow(bad_nd) > 0)
    issues[["ND_result"]] <- glue::glue(
      "{nrow(bad_nd)} ND rows where Result != RL: ",
      paste(bad_nd$Analyte, collapse = ", ")
    )

  # Analytes not mapped (no CEDEN name)
  unmapped <- df %>% filter(is.na(Analyte))
  if (nrow(unmapped) > 0)
    issues[["unmapped_analyte"]] <- glue::glue(
      "{nrow(unmapped)} rows with no CEDEN analyte mapping"
    )

  # Matrix not mapped
  unmapped_matrix <- df %>% filter(is.na(MatrixName))
  if (nrow(unmapped_matrix) > 0)
    issues[["unmapped_matrix"]] <- glue::glue(
      "{nrow(unmapped_matrix)} rows with no CEDEN matrix mapping"
    )

  if (length(issues) == 0) {
    message("Validation PASSED — no issues found in chemistry table.")
  } else {
    message("Validation found ", length(issues), " issue(s):")
    for (nm in names(issues)) message("  [", nm, "] ", issues[[nm]])
  }

  invisible(issues)
}

validate_field <- function(df) {
  issues <- list()

  required <- c("StationCode", "SampleDate", "ProjectCode")
  for (col in required) {
    n_missing <- sum(is.na(df[[col]]))
    if (n_missing > 0)
      issues[[col]] <- glue::glue("{n_missing} rows missing {col}")
  }

  if (length(issues) == 0) {
    message("Validation PASSED — no issues found in field table.")
  } else {
    message("Field table validation found ", length(issues), " issue(s):")
    for (nm in names(issues)) message("  [", nm, "] ", issues[[nm]])
  }

  invisible(issues)
}

if (!exists("SOURCED_BY_MASTER")) {
  library(glue)
  ceden_chem  <- readRDS("data/processed/ceden_chemistry.rds")
  ceden_field <- readRDS("data/processed/ceden_field.rds")

  validate_chemistry(ceden_chem)
  validate_field(ceden_field)
}
