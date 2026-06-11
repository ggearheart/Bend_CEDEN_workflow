# launch_app.R — run from repo root
# In RStudio: source("launch_app.R")
# Terminal:   Rscript launch_app.R

setwd(dirname(normalizePath(if (interactive()) {
  tryCatch(rstudioapi::getActiveDocumentContext()$path,
           error = function(e) "launch_app.R")
} else {
  commandArgs(trailingOnly = FALSE) |>
    grep("--file=", x = _, value = TRUE) |>
    sub("--file=", "", x = _)
})))

# Install missing packages into ~/R/library if needed
pkgs <- c("shiny","bslib","DT","shinyFiles","readr","dplyr","tidyr",
          "lubridate","openxlsx","glue","stringr","reactable","zip")
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing)) {
  user_lib <- file.path(Sys.getenv("HOME"), "R", "library")
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(user_lib, .libPaths()))
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, lib = user_lib, repos = "https://cloud.r-project.org")
}

shiny::runApp("app", launch.browser = TRUE)
