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
# Builds the CEDEN 2.0 workbook from scratch using the official column order.
# Avoids loadWorkbook/deleteData/writeData on a pre-existing sheet, which is
# unreliable on shinyapps.io.

# Official CEDEN 2.0 Chemistry_Results column order (39 columns)
CEDEN2_COLS <- c(
  "#StationCode", "ProjectCode", "LabSampleID", "CollectionDateTime",
  "SampleAgencyCode", "SampleTypeCode", "MatrixCode", "CollectionDepth",
  "UnitCollectionDepth", "SampleComments", "PrepPreservationName",
  "PrepPreservationDateTime", "DigestExtractMethod", "DigestExtractDateTime",
  "LabBatch", "LabAgencyCode", "AnalysisDateTime", "MethodName",
  "AnalyteName", "FractionName", "DilutionFactor", "TestType",
  "ResultTypeCode", "Result", "UnitName", "DetectedAboveMDL",
  "MethodDetectionLimit", "MinimumReportingLimit", "QACode",
  "ExpectedValue", "PercentRecovery", "RelativePercentDifference",
  "RelativeStandardDeviation", "LabComments", "ParticleSizeRange",
  "QC_OriginalConc", "EQuIS_Sample_ID", "Parent_SampleID", "SampleID"
)

export_ceden_v2 <- function(chem_v2_df, output_dir = "data/output",
                             template = "templates/ceden2_chemistry_template.xlsx") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  projects <- unique(chem_v2_df$ProjectCode)

  for (proj in projects) {
    sub_df  <- chem_v2_df %>% filter(ProjectCode == proj)
    today   <- format(Sys.Date(), "%Y%m%d")
    filename <- file.path(output_dir,
                          paste0("CEDEN2_", proj, "_", today, ".xlsx"))

    wb <- createWorkbook()

    # ---- Chemistry_Results sheet (data) ----
    addWorksheet(wb, "Chemistry_Results")

    # Ensure columns are in the official order; add any missing as NA
    for (col in CEDEN2_COLS) {
      if (!col %in% names(sub_df)) sub_df[[col]] <- NA
    }
    sub_df <- sub_df[, CEDEN2_COLS]

    writeDataTable(wb, sheet = "Chemistry_Results", x = sub_df,
                   tableStyle = "TableStyleMedium9", withFilter = TRUE)
    freezePane(wb, "Chemistry_Results", firstRow = TRUE)
    setColWidths(wb, "Chemistry_Results", cols = seq_along(CEDEN2_COLS), widths = "auto")

    # ---- Constituent_Index and Advanced_Vocabulary_Request stubs ----
    # Copy from the official template if available, otherwise add blank tabs
    if (file.exists(template)) {
      tmpl_sheets <- c("Format Information", "Constituent_Index",
                       "Advanced_Vocabulary_Request")
      for (sn in tmpl_sheets) {
        tryCatch({
          tmpl_data <- readWorkbook(template, sheet = sn, colNames = TRUE)
          addWorksheet(wb, sn)
          if (nrow(tmpl_data) > 0)
            writeDataTable(wb, sheet = sn, x = tmpl_data,
                           tableStyle = "None", withFilter = FALSE)
          else
            writeData(wb, sheet = sn, x = tmpl_data)
        }, error = function(e) {
          addWorksheet(wb, sn)  # add empty sheet if read fails
        })
      }
    }

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
