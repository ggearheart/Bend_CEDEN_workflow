# Bend Genetics → CEDEN Workflow

Converts Bend Genetics cyanotoxin lab results (CSV + PDF) into CEDEN-formatted
submission tables for the SWAMP FHAB program.

## Outputs

| File | CEDEN table |
|------|-------------|
| `CEDEN_WaterChemistry_YYYYMMDD.csv/.xlsx` | Chemistry results (long format) |
| `CEDEN_FieldResults_YYYYMMDD.csv/.xlsx`   | Station visits / field metadata |

## Quick start

```r
# In RStudio, open run_workflow.R and click Source
# Or from terminal:
Rscript run_workflow.R
```

## Input

Drop Bend Genetics results CSV files into `data/raw/`. PDFs (COC, sample receipt,
results report) can also go there for archiving — they are not parsed by the workflow.

## Workflow steps

| Script | Purpose |
|--------|---------|
| `R/01_parse_csv.R`       | Read and date-parse all CSVs in `data/raw/` |
| `R/02_transform_ceden.R` | Pivot wide → long; map analytes, matrices, results |
| `R/03_validate.R`        | Check required fields, ND handling, unmapped values |
| `R/04_export.R`          | Write Excel + CSV to `data/output/` |

## Lookup tables

| File | Purpose |
|------|---------|
| `lookup/analyte_map.csv` | Bend analyte name → CEDEN analyte name + units + method |
| `lookup/matrix_map.csv`  | Bend matrix string → CEDEN MatrixName + SampleTypeCode |

Edit these files to add new analytes or correct CEDEN controlled vocabulary terms.

## ND (non-detect) handling

Results reported as `ND` are stored at the Reporting Limit value with `ResQualCode = "ND"`,
per CEDEN submission conventions. Default RLs are in `R/02_transform_ceden.R` → `rl_defaults`.

## Required R packages

```r
install.packages(c("readr", "dplyr", "tidyr", "lubridate",
                   "openxlsx", "glue", "stringr"))
```
