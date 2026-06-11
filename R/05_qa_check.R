# 05_qa_check.R
# QA spot-check: randomly sample 5% of chemistry rows and evaluate
# completeness and correctness. Returns a list with $summary and $detail.

library(dplyr)

qa_spot_check <- function(chem_df, format = c("v1", "v2"), pct = 0.05) {
  format <- match.arg(format)
  set.seed(42)  # reproducible sample within a session; caller can override

  n_total  <- nrow(chem_df)
  n_sample <- max(5, round(n_total * pct))
  n_sample <- min(n_sample, n_total)

  sampled <- chem_df %>% slice_sample(n = n_sample)

  if (format == "v1") {
    detail <- sampled %>%
      mutate(
        row_id           = row_number(),
        ck_station       = !is.na(StationCode)  & nchar(trimws(StationCode)) > 0,
        ck_date          = !is.na(SampleDate),
        ck_analyte       = !is.na(Analyte)      & nchar(trimws(Analyte)) > 0,
        ck_result        = is.numeric(Result)   & !is.na(Result),
        ck_units         = !is.na(Units)        & nchar(trimws(Units)) > 0,
        ck_matrix        = !is.na(MatrixName)   & nchar(trimws(MatrixName)) > 0,
        ck_method        = !is.na(MethodName)   & nchar(trimws(MethodName)) > 0,
        ck_resqual       = ResQualCode %in% c("=", "ND", "NR", "J", "C1", "C2", "D"),
        ck_nd_at_rl      = !(ResQualCode == "ND" & !is.na(RL) & Result != RL),
        .keep = "all"
      )
    check_cols <- c("ck_station","ck_date","ck_analyte","ck_result",
                    "ck_units","ck_matrix","ck_method","ck_resqual","ck_nd_at_rl")
  } else {
    detail <- sampled %>%
      mutate(
        row_id           = row_number(),
        ck_station       = !is.na(`#StationCode`)  & nchar(trimws(`#StationCode`)) > 0,
        ck_datetime      = !is.na(CollectionDateTime),
        ck_analyte       = !is.na(AnalyteName)     & nchar(trimws(AnalyteName)) > 0,
        ck_result        = is.numeric(Result)      & !is.na(Result),
        ck_units         = !is.na(UnitName)        & nchar(trimws(UnitName)) > 0,
        ck_matrix        = !is.na(MatrixCode)      & nchar(trimws(MatrixCode)) > 0,
        ck_method        = !is.na(MethodName)      & nchar(trimws(MethodName)) > 0,
        ck_detected_flag = DetectedAboveMDL %in% c("Yes", "No"),
        ck_nd_at_mrl     = !(DetectedAboveMDL == "No" & !is.na(MinimumReportingLimit) &
                               Result != MinimumReportingLimit),
        ck_lab_agency    = !is.na(LabAgencyCode)   & nchar(trimws(LabAgencyCode)) > 0,
        .keep = "all"
      )
    check_cols <- c("ck_station","ck_datetime","ck_analyte","ck_result",
                    "ck_units","ck_matrix","ck_method","ck_detected_flag",
                    "ck_nd_at_mrl","ck_lab_agency")
  }

  detail <- detail %>%
    mutate(
      n_checks_run    = length(check_cols),
      n_checks_passed = rowSums(across(all_of(check_cols))),
      row_pass        = n_checks_passed == n_checks_run,
      pct_pass        = round(100 * n_checks_passed / n_checks_run, 1)
    )

  # Per-check summary
  check_summary <- tibble(
    Check       = check_cols,
    Description = check_label(check_cols, format),
    Pass        = colSums(detail[check_cols]),
    Fail        = n_sample - colSums(detail[check_cols]),
    Pct_Pass    = round(100 * Pass / n_sample, 1)
  )

  overall_pass_rate <- round(100 * sum(detail$row_pass) / n_sample, 1)

  list(
    n_total          = n_total,
    n_sampled        = n_sample,
    pct_sampled      = round(100 * n_sample / n_total, 1),
    overall_pass_rate = overall_pass_rate,
    check_summary    = check_summary,
    detail           = detail
  )
}

check_label <- function(cols, format) {
  labels_v1 <- c(
    ck_station   = "StationCode present",
    ck_date      = "SampleDate present",
    ck_analyte   = "Analyte name present",
    ck_result    = "Result is numeric",
    ck_units     = "Units present",
    ck_matrix    = "MatrixName present",
    ck_method    = "MethodName present",
    ck_resqual   = "ResQualCode is valid CEDEN value",
    ck_nd_at_rl  = "ND results equal RL value"
  )
  labels_v2 <- c(
    ck_station       = "StationCode present",
    ck_datetime      = "CollectionDateTime present",
    ck_analyte       = "AnalyteName present",
    ck_result        = "Result is numeric",
    ck_units         = "UnitName present",
    ck_matrix        = "MatrixCode present",
    ck_method        = "MethodName present",
    ck_detected_flag = "DetectedAboveMDL is Yes/No",
    ck_nd_at_mrl     = "Non-detects equal MinimumReportingLimit",
    ck_lab_agency    = "LabAgencyCode present"
  )
  labels <- if (format == "v1") labels_v1 else labels_v2
  unname(labels[cols])
}
