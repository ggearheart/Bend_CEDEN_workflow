# 04_export.R
# Write final CEDEN submission files to data/output/.
# Creates one Excel workbook per batch with WaterChemistry and FieldResults sheets.

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
  ceden_chem  <- readRDS("data/processed/ceden_chemistry.rds")
  ceden_field <- readRDS("data/processed/ceden_field.rds")

  export_ceden(ceden_chem, ceden_field)
  export_csv(ceden_chem, ceden_field)
}
