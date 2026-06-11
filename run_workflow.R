# run_workflow.R
# Master script — runs all four pipeline steps in order.
# Usage: Rscript run_workflow.R
#        Or open in RStudio and source().

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # works in RStudio
# If running via Rscript, set your working directory to the repo root first.

SOURCED_BY_MASTER <- TRUE

library(glue)

message("=== Step 1: Parse Bend Genetics CSVs ===")
source("R/01_parse_csv.R")
csv_files <- list.files("data/raw", pattern = "_results\\.csv$", full.names = TRUE)
parsed_list <- lapply(csv_files, parse_bend_csv)
bend_parsed <- dplyr::bind_rows(parsed_list)
saveRDS(bend_parsed, "data/processed/bend_parsed.rds")
message(glue("  {nrow(bend_parsed)} samples parsed from {length(csv_files)} file(s)."))

message("\n=== Step 2: Transform to CEDEN format ===")
source("R/02_transform_ceden.R")
long_df <- bend_parsed %>%
  pivot_to_long() %>%
  map_result() %>%
  join_analyte_map() %>%
  join_matrix_map()
ceden_chem  <- build_chemistry(long_df)
ceden_field <- build_field(long_df)
saveRDS(ceden_chem,  "data/processed/ceden_chemistry.rds")
saveRDS(ceden_field, "data/processed/ceden_field.rds")
message(glue("  {nrow(ceden_chem)} chemistry rows, {nrow(ceden_field)} field rows."))

message("\n=== Step 3: Validate ===")
source("R/03_validate.R")
library(glue)
validate_chemistry(ceden_chem)
validate_field(ceden_field)

message("\n=== Step 4: Export ===")
source("R/04_export.R")
export_ceden(ceden_chem, ceden_field)
export_csv(ceden_chem, ceden_field)

message("\nWorkflow complete. Check data/output/ for submission files.")
