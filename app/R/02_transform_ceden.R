# 02_transform_ceden.R
# Pivot Bend wide-format results to CEDEN long format and map fields.
# Outputs:
#   data/processed/ceden_chemistry.rds
#   data/processed/ceden_field.rds

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)

# ---------- load lookups ---------------------------------------------------

analyte_map <- read_csv("lookup/analyte_map.csv", show_col_types = FALSE)
matrix_map  <- read_csv("lookup/matrix_map.csv",  show_col_types = FALSE)

# CEDEN reporting limit column names (RL not stored in CSV - use Bend defaults)
# These must be kept in sync with Bend's published MDL/RL table.
rl_defaults <- tribble(
  ~ceden_analyte,              ~default_rl,  ~default_mdl,
  "Anatoxin-a",                0.15,         0.10,
  "Microcystin",               0.20,         0.10,
  "Cylindrospermopsin",        0.05,         0.03,
  "Saxitoxin",                 0.02,         0.01
)

# ---------- pivot to long --------------------------------------------------

pivot_to_long <- function(df) {
  # Identify the analyte columns (everything after BG_ID / Notes)
  id_cols <- c("SampleID", "Batch", "Project", "Location", "SampleType",
               "CollectDate", "CollectTime", "ReceivedDate", "ReceivedTime",
               "Completed", "CompletedDate", "Customer", "CustomerSample", "BG_ID", "Notes")

  analyte_cols <- setdiff(names(df), id_cols)

  df %>%
    pivot_longer(
      cols      = all_of(analyte_cols),
      names_to  = "bend_analyte",
      values_to = "raw_result"
    ) %>%
    filter(!is.na(raw_result), trimws(raw_result) != "")  # drop blank/NA cells
}

# ---------- map result values ----------------------------------------------

map_result <- function(df) {
  df %>%
    mutate(
      # Detect non-detect flag
      nd_flag    = raw_result == "ND",
      # Numeric result: ND -> NA for now, will fill from RL
      result_num = suppressWarnings(as.numeric(raw_result)),
      ResQualCode = case_when(
        nd_flag                         ~ "ND",
        !is.na(result_num)              ~ "=",
        TRUE                            ~ "NR"
      )
    )
}

# ---------- join analyte map -----------------------------------------------

join_analyte_map <- function(df) {
  df %>%
    left_join(analyte_map, by = "bend_analyte") %>%
    left_join(rl_defaults,  by = "ceden_analyte") %>%
    mutate(
      # ND results reported at RL value per CEDEN convention
      Result = case_when(
        nd_flag & !is.na(default_rl) ~ default_rl,
        nd_flag                      ~ 0,
        TRUE                         ~ result_num
      ),
      MDL = default_mdl,
      RL  = default_rl
    )
}

# ---------- join matrix map ------------------------------------------------

join_matrix_map <- function(df) {
  df %>%
    left_join(matrix_map, by = c("SampleType" = "bend_matrix"))
}

# ---------- build CEDEN WaterChemistry table --------------------------------

build_chemistry <- function(df) {
  df %>%
    transmute(
      # Station / sample identity
      StationCode      = CustomerSample,
      StationName      = Location,
      SampleDate       = format(CollectDate, "%m/%d/%Y"),
      SampleTime       = CollectTime,
      ProjectCode      = Project,
      # Lab info
      LabAgencyCode    = "BendGenetics",
      LabSampleID      = SampleID,
      LabBatch         = as.character(Batch),
      BG_ID            = BG_ID,
      # Analyte result
      Analyte          = ceden_analyte,
      Result           = Result,
      ResQualCode      = ResQualCode,
      Units            = ceden_units,
      Fraction         = ceden_fraction,
      MDL              = MDL,
      RL               = RL,
      # Method / matrix
      MethodName       = method_name,
      MatrixName       = ceden_matrix,
      SampleTypeCode   = ceden_sample_type,
      # QA
      QACode           = "None",
      ComplianceCode   = "Applicable",
      # Dates
      LabSubmitDate    = format(ReceivedDate, "%m/%d/%Y"),
      LabCompletionDate = format(CompletedDate, "%m/%d/%Y")
    )
}

# ---------- build CEDEN 2.0 Chemistry_Results table ------------------------
# Column order matches the official template exactly (39 columns).

build_chemistry_v2 <- function(df) {
  df %>%
    transmute(
      # --- Sample identity ---
      `#StationCode`           = CustomerSample,
      ProjectCode              = Project,
      LabSampleID              = SampleID,
      # CollectionDateTime: CEDEN 2.0 wants MM/DD/YYYY HH:MM
      # CollectTime is already HH:MM (24-hr) after parse_bend_csv conversion
      CollectionDateTime       = paste(format(CollectDate, "%m/%d/%Y"), CollectTime),
      SampleAgencyCode         = "SWRCB",           # submitting agency — adjust as needed
      SampleTypeCode           = ceden_sample_type,
      MatrixCode               = ceden_matrix_code,
      CollectionDepth          = NA_real_,
      UnitCollectionDepth      = NA_character_,
      SampleComments           = NA_character_,

      # --- Prep / extraction ---
      PrepPreservationName     = "Not Applicable",
      PrepPreservationDateTime = NA_character_,
      DigestExtractMethod      = "Not Applicable",
      DigestExtractDateTime    = NA_character_,

      # --- Lab batch / method ---
      LabBatch                 = as.character(Batch),
      LabAgencyCode            = "BendGenetics",
      AnalysisDateTime         = format(CompletedDate, "%m/%d/%Y"),
      MethodName               = method_name,

      # --- Analyte result ---
      AnalyteName              = ceden_analyte,
      FractionName             = ceden_fraction,
      DilutionFactor           = 1,
      TestType                 = "Result",
      ResultTypeCode           = "Actual",
      Result                   = Result,
      UnitName                 = ceden_units,

      # ND flag: "No" = not detected, "Yes" = detected above MDL
      DetectedAboveMDL         = if_else(nd_flag, "No", "Yes"),

      MethodDetectionLimit     = MDL,
      MinimumReportingLimit    = RL,

      # --- QA ---
      QACode                   = "None",
      ExpectedValue            = NA_real_,
      PercentRecovery          = NA_real_,
      RelativePercentDifference = NA_real_,
      RelativeStandardDeviation = NA_real_,
      LabComments              = NA_character_,
      ParticleSizeRange        = NA_character_,
      QC_OriginalConc          = NA_real_,

      # --- IDs ---
      EQuIS_Sample_ID          = NA_character_,
      Parent_SampleID          = NA_character_,
      SampleID                 = BG_ID
    )
}

# ---------- build CEDEN FieldResults / StationVisit table ------------------

build_field <- function(df) {
  df %>%
    distinct(CustomerSample, Location, CollectDate, CollectTime,
             Project, SampleType, ceden_matrix, ceden_sample_type) %>%
    transmute(
      StationCode    = CustomerSample,
      StationName    = Location,
      SampleDate     = format(CollectDate, "%m/%d/%Y"),
      SampleTime     = CollectTime,
      ProjectCode    = Project,
      MatrixName     = ceden_matrix,
      SampleTypeCode = ceden_sample_type,
      # Fields to be completed from field sheets:
      Collectors     = NA_character_,
      EventCode      = "Grab",
      ProtocolCode   = "Not Recorded",
      SampleComments = NA_character_
    )
}
