# 04_export.R
# Write final CEDEN submission files to data/output/.
#
# Two export targets:
#   export_ceden()    — legacy-style workbook (WaterChemistry + FieldResults sheets)
#   export_ceden_v2() — official CEDEN 2.0 template (Chemistry_Results sheet,
#                       preserving all other template sheets intact)
#   export_csv()      — flat CSVs for both formats

library(dplyr)
library(openxlsx)
library(lubridate)

export_ceden <- function(chem_df, field_df, output_dir = "data/output") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # One workbook per ProjectCode + report date
  projects <- unique(chem_df$ProjectCode)

  for (proj in projects) {
    chem_sub  <- chem_df  %>% filter(ProjectCode == proj)
    field_sub <- field_df %>% filter(ProjectCode == proj)

    today     <- format(Sys.Date(), "%Y%m%d")
    filename  <- file.path(output_dir,
                           paste0("CEDEN_", proj, "_", today, ".xlsx"))

    wb <- createWorkbook()

    addWorksheet(wb, "WaterChemistry")
    writeDataTable(wb, "WaterChemistry", chem_sub, tableStyle = "TableStyleMedium9")

    addWorksheet(wb, "FieldResults")
    writeDataTable(wb, "FieldResults", field_sub, tableStyle = "TableStyleMedium9")

    # Freeze top row, auto-width columns
    freezePane(wb, "WaterChemistry", firstRow = TRUE)
    freezePane(wb, "FieldResults",   firstRow = TRUE)
    setColWidths(wb, "WaterChemistry", cols = 1:ncol(chem_sub),  widths = "auto")
    setColWidths(wb, "FieldResults",   cols = 1:ncol(field_sub), widths = "auto")

    saveWorkbook(wb, filename, overwrite = TRUE)
    message("Exported -> ", filename)
  }
}

# ---------- CEDEN 2.0 export -----------------------------------------------
# Writes data into the official CEDEN 2.0 Chemistry template, preserving the
# Format Information, Constituent_Index, and Advanced_Vocabulary_Request sheets.

export_ceden_v2 <- function(chem_v2_df, output_dir = "data/output",
                             template = "templates/ceden-2.0-chemistry-data-submission-template.xlsx") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  if (!file.exists(template))
    stop("CEDEN 2.0 template not found at: ", template)

  projects <- unique(chem_v2_df$ProjectCode)

  for (proj in projects) {
    sub_df  <- chem_v2_df %>% filter(ProjectCode == proj)
    today   <- format(Sys.Date(), "%Y%m%d")
    filename <- file.path(output_dir,
                          paste0("CEDEN2_", proj, "_", today, ".xlsx"))

    # Load a fresh copy of the template for each project
    wb <- loadWorkbook(template)

    # Write data starting at row 2 of Chemistry_Results (row 1 = headers)
    # deleteData first to avoid stale cells from the template example rows
    deleteData(wb, sheet = "Chemistry_Results",
               rows = 2:10000, cols = 1:39, gridExpand = TRUE)

    writeData(wb, sheet = "Chemistry_Results", x = sub_df,
              startRow = 2, startCol = 1,
              colNames = FALSE)   # headers already in template row 1

    # Style: freeze pane, auto-width
    freezePane(wb, "Chemistry_Results", firstRow = TRUE)
    setColWidths(wb, "Chemistry_Results", cols = 1:ncol(sub_df), widths = "auto")

    saveWorkbook(wb, filename, overwrite = TRUE)
    message("Exported CEDEN 2.0 -> ", filename)
  }
}

# Also export flat CSVs for scripted / CEDEN uploader use
export_csv <- function(chem_df, field_df, output_dir = "data/output") {
  today <- format(Sys.Date(), "%Y%m%d")
  readr::write_csv(chem_df,
    file.path(output_dir, paste0("CEDEN_WaterChemistry_", today, ".csv")))
  readr::write_csv(field_df,
    file.path(output_dir, paste0("CEDEN_FieldResults_", today, ".csv")))
  message("CSV exports written to ", output_dir)
}

if (!exists("SOURCED_BY_MASTER")) {
  ceden_chem    <- readRDS("data/processed/ceden_chemistry.rds")
  ceden_chem_v2 <- readRDS("data/processed/ceden_chemistry_v2.rds")
  ceden_field   <- readRDS("data/processed/ceden_field.rds")

  export_ceden(ceden_chem, ceden_field)     # legacy format
  export_ceden_v2(ceden_chem_v2)            # CEDEN 2.0 official template
  export_csv(ceden_chem, ceden_field)
}
