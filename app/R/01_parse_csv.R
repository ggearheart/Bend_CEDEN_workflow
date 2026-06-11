# 01_parse_csv.R
# Read and lightly clean the Bend Genetics results CSV.
# Output: data/processed/bend_parsed.rds

library(readr)
library(dplyr)
library(lubridate)

parse_bend_csv <- function(csv_path) {
  raw <- read_csv(csv_path, show_col_types = FALSE)

  # Bend CSV has two columns both named "Time" (collection time and received time).
  # readr auto-deduplicates them to Time...N positionally.
  time_cols <- grep("^Time", names(raw), value = TRUE)
  if (length(time_cols) < 2)
    stop("Expected two 'Time' columns in: ", csv_path)

  raw <- raw %>%
    rename(
      SampleID       = `Sample ID`,
      CollectDate    = Collected,
      CollectTime    = !!sym(time_cols[1]),
      ReceivedDate   = Received,
      ReceivedTime   = !!sym(time_cols[2]),
      CustomerSample = `Customer Sample`,
      SampleType     = `Sample Type`
    )

  # Convert time columns: readr reads Bend's time cells as seconds since midnight
  secs_to_hhmm <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    ifelse(is.na(x), NA_character_,
           sprintf("%02d:%02d", as.integer(x %/% 3600), as.integer((x %% 3600) %/% 60)))
  }

  raw <- raw %>%
    mutate(
      CollectDate   = mdy(CollectDate),
      ReceivedDate  = mdy(ReceivedDate),
      CompletedDate = mdy(Completed),
      CollectTime   = secs_to_hhmm(CollectTime),
      ReceivedTime  = secs_to_hhmm(ReceivedTime)
    )

  raw
}

# Run when sourced directly
if (!exists("SOURCED_BY_MASTER")) {
  csv_files <- list.files("data/raw", pattern = "_results\\.csv$", full.names = TRUE)
  if (length(csv_files) == 0) stop("No results CSV found in data/raw/")

  parsed_list <- lapply(csv_files, parse_bend_csv)
  bend_parsed <- bind_rows(parsed_list)

  saveRDS(bend_parsed, "data/processed/bend_parsed.rds")
  message("Parsed ", nrow(bend_parsed), " samples from ", length(csv_files), " CSV file(s).")
  message("Saved -> data/processed/bend_parsed.rds")
}
