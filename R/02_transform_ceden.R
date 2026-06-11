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
               "CompletedDate", "Customer", "CustomerSample", "BG_ID", "Notes")

  analyte_cols <- setdiff(names(df), id_cols)

  df %>%
    pivot_longer(
      cols      = all_of(analyte_cols),
      names_to  = "bend_analyte",
      values_to = "raw_result"
    ) %>%
    filter(!is.na(raw_result))  # drop blank cells (analyte not applicable to matrix)
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

# ---------- run -------------------------------------------------------------

if (!exists("SOURCED_BY_MASTER")) {
  bend_parsed <- readRDS("data/processed/bend_parsed.rds")

  long_df <- bend_parsed %>%
    pivot_to_long() %>%
    map_result() %>%
    join_analyte_map() %>%
    join_matrix_map()

  ceden_chem  <- build_chemistry(long_df)
  ceden_field <- build_field(long_df)

  saveRDS(ceden_chem,  "data/processed/ceden_chemistry.rds")
  saveRDS(ceden_field, "data/processed/ceden_field.rds")

  message("Chemistry rows: ", nrow(ceden_chem))
  message("Field rows:     ", nrow(ceden_field))
  message("Saved -> data/processed/ceden_chemistry.rds")
  message("Saved -> data/processed/ceden_field.rds")
}
