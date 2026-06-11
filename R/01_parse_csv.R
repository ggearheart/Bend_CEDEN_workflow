# 01_parse_csv.R
# Read and lightly clean the Bend Genetics results CSV.
# Output: data/processed/bend_parsed.rds

library(readr)
library(dplyr)
library(lubridate)

parse_bend_csv <- function(csv_path) {
  raw <- read_csv(csv_path, show_col_types = FALSE)

  # Normalise date + time columns (Bend uses two "Time" columns; rename on read)
  # Actual header: Sample ID, ..., Collected, Time, Received, Time, Completed, ...
  # readr auto-deduplicates to Time...7 and Time...9
  raw <- raw %>%
    rename(
      SampleID       = `Sample ID`,
      CollectDate     = Collected,
      CollectTime     = starts_with("Time") %>% first(),
      ReceivedDate    = Received,
      ReceivedTime    = starts_with("Time") %>% last(),
      CustomerSample  = `Customer Sample`,
      BG_ID           = BG_ID,
      SampleType      = `Sample Type`
    )

  # Parse dates
  raw <- raw %>%
    mutate(
      CollectDate  = mdy(CollectDate),
      ReceivedDate = mdy(ReceivedDate),
      CompletedDate = mdy(Completed)
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
