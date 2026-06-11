# app.R — Bend Genetics → CEDEN Submission Tool
# Run with: shiny::runApp("app/")

library(shiny)
library(bslib)
library(DT)
library(shinyFiles)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(openxlsx)
library(glue)
library(stringr)
library(reactable)
library(zip)

# Source workflow functions (paths relative to repo root, set via setwd in run block)
source("R/01_parse_csv.R")
source("R/02_transform_ceden.R")
source("R/03_validate.R")
source("R/04_export.R")
source("R/05_qa_check.R")

CEDEN_VERSION_CHOICES <- c("CEDEN 2.0 (recommended)" = "v2",
                            "CEDEN Legacy"             = "v1")

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title = div(
    img(src = "logo.png", height = "30px", style = "margin-right:8px;"),
    "Bend Genetics → CEDEN"
  ),
  theme = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#2c7fb8",
    heading_font = font_google("Inter")
  ),
  bg = "#2c7fb8",

  # ── TAB 1: Process ──────────────────────────────────────────────────────────
  nav_panel(
    "Process",
    icon = icon("upload"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        h5("Input", class = "mt-2"),
        radioButtons("input_mode", NULL,
          choices  = c("Single file / email attachment" = "single",
                       "Batch folder"                   = "batch"),
          selected = "single"
        ),

        # Single file
        conditionalPanel(
          "input.input_mode == 'single'",
          fileInput("file_upload", "Upload Bend Genetics CSV",
                    accept = ".csv", buttonLabel = "Browse / drop file")
        ),

        # Batch folder
        conditionalPanel(
          "input.input_mode == 'batch'",
          shinyDirButton("batch_dir", "Choose folder", "Select folder containing CSVs"),
          verbatimTextOutput("batch_dir_label", placeholder = TRUE)
        ),

        hr(),
        h5("Output format"),
        radioButtons("ceden_version", NULL,
                     choices = CEDEN_VERSION_CHOICES, selected = "v2"),

        hr(),
        h5("Options"),
        checkboxInput("run_qa",      "Run QA spot-check (5%)", value = TRUE),
        checkboxInput("export_xlsx", "Export Excel workbook",  value = TRUE),
        checkboxInput("export_csv",  "Export CSV",             value = TRUE),

        hr(),
        actionButton("btn_process", "Process",
                     icon = icon("play"),
                     class = "btn-primary w-100"),

        br(), br(),
        downloadButton("btn_download", "Download outputs (.zip)",
                       class = "btn-success w-100"),
      ),

      # Main area
      navset_card_tab(
        nav_panel(
          "Preview",
          icon = icon("table"),
          uiOutput("process_status"),
          br(),
          DTOutput("preview_table")
        ),
        nav_panel(
          "Validation",
          icon = icon("check-circle"),
          uiOutput("validation_ui")
        ),
        nav_panel(
          "QA Report",
          icon = icon("magnifying-glass-chart"),
          uiOutput("qa_ui")
        )
      )
    )
  ),

  # ── TAB 2: Mapping ──────────────────────────────────────────────────────────
  nav_panel(
    "Mapping",
    icon = icon("arrows-left-right"),
    layout_columns(
      col_widths = c(8, 4),

      card(
        card_header("Analyte Mapping  (Bend → CEDEN)"),
        card_body(
          p(class = "text-muted small",
            "Edit cells to correct analyte names, units, or methods.
             Click Save to write changes back to lookup/analyte_map.csv."),
          DTOutput("analyte_map_table"),
          br(),
          actionButton("save_analyte_map", "Save analyte mapping",
                       icon = icon("floppy-disk"), class = "btn-sm btn-primary")
        )
      ),

      card(
        card_header("Matrix Mapping"),
        card_body(
          p(class = "text-muted small",
            "Maps Bend matrix strings to CEDEN MatrixName / MatrixCode."),
          DTOutput("matrix_map_table"),
          br(),
          actionButton("save_matrix_map", "Save matrix mapping",
                       icon = icon("floppy-disk"), class = "btn-sm btn-primary")
        )
      )
    )
  ),

  # ── TAB 3: About ────────────────────────────────────────────────────────────
  nav_panel(
    "About",
    icon = icon("circle-info"),
    card(
      card_body(
        h4("Bend Genetics → CEDEN Submission Workflow"),
        p("Developed for the SWAMP Freshwater HABs program, State Water Resources Control Board."),
        hr(),
        h5("Output files"),
        tags$table(class = "table table-sm",
          tags$thead(tags$tr(tags$th("File"), tags$th("Format"))),
          tags$tbody(
            tags$tr(tags$td("CEDEN2_<project>_<date>.xlsx"),
                    tags$td("CEDEN 2.0 official template (Chemistry_Results sheet)")),
            tags$tr(tags$td("CEDEN_<project>_<date>.xlsx"),
                    tags$td("Legacy workbook (WaterChemistry + FieldResults sheets)")),
            tags$tr(tags$td("CEDEN_WaterChemistry_<date>.csv"),
                    tags$td("Flat CSV — chemistry")),
            tags$tr(tags$td("CEDEN_FieldResults_<date>.csv"),
                    tags$td("Flat CSV — field visits"))
          )
        ),
        hr(),
        h5("ND (non-detect) convention"),
        p("Non-detects are stored at the Reporting Limit value with ResQualCode = 'ND'
           (CEDEN v1) or DetectedAboveMDL = 'No' (CEDEN 2.0)."),
        hr(),
        h5("Required R packages"),
        tags$code("install.packages(c('shiny','bslib','DT','shinyFiles','readr',
                   'dplyr','tidyr','lubridate','openxlsx','glue','stringr',
                   'reactable','zip'))")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Reactive: list of CSV paths to process
  csv_paths <- reactiveVal(character(0))

  # Single file upload
  observeEvent(input$file_upload, {
    req(input$file_upload)
    csv_paths(input$file_upload$datapath)
  })

  # Batch folder selection
  roots <- c(Home = path.expand("~"), Root = "/")
  shinyDirChoose(input, "batch_dir", roots = roots, filetypes = c("", "csv"))

  batch_dir_path <- reactive({
    req(input$batch_dir)
    parseDirPath(roots, input$batch_dir)
  })

  output$batch_dir_label <- renderText({
    p <- batch_dir_path()
    if (length(p) == 0) "No folder selected" else p
  })

  observeEvent(batch_dir_path(), {
    p <- batch_dir_path()
    if (length(p) > 0) {
      files <- list.files(p, pattern = "_results\\.csv$",
                          full.names = TRUE, recursive = FALSE)
      csv_paths(files)
      showNotification(glue("{length(files)} results CSV(s) found in folder."),
                       type = "message")
    }
  })

  # ── Process pipeline ────────────────────────────────────────────────────────

  results <- reactiveValues(
    long_df   = NULL,
    chem      = NULL,
    chem_v2   = NULL,
    field     = NULL,
    val_issues = NULL,
    qa        = NULL,
    out_dir   = NULL
  )

  observeEvent(input$btn_process, {
    paths <- csv_paths()
    validate(need(length(paths) > 0, "Please upload a file or select a folder first."))

    withProgress(message = "Processing…", value = 0, {

      incProgress(0.1, detail = "Parsing CSV(s)")
      parsed_list <- lapply(paths, parse_bend_csv)
      bend_parsed <- bind_rows(parsed_list)

      incProgress(0.3, detail = "Transforming to CEDEN format")
      SOURCED_BY_MASTER <<- TRUE
      long_df <- bend_parsed %>%
        pivot_to_long() %>%
        map_result() %>%
        join_analyte_map() %>%
        join_matrix_map()

      results$long_df  <- long_df
      results$chem     <- build_chemistry(long_df)
      results$chem_v2  <- build_chemistry_v2(long_df)
      results$field    <- build_field(long_df)

      incProgress(0.5, detail = "Validating")
      results$val_issues <- list(
        chem  = validate_chemistry(results$chem),
        field = validate_field(results$field)
      )

      if (input$run_qa) {
        incProgress(0.65, detail = "Running QA spot-check")
        fmt <- input$ceden_version
        df  <- if (fmt == "v2") results$chem_v2 else results$chem
        results$qa <- qa_spot_check(df, format = fmt)
      }

      incProgress(0.8, detail = "Exporting files")
      out_dir <- tempfile("ceden_output")
      dir.create(out_dir)

      if (input$export_xlsx) {
        if (input$ceden_version == "v2") {
          export_ceden_v2(results$chem_v2, output_dir = out_dir)
        } else {
          export_ceden(results$chem, results$field, output_dir = out_dir)
        }
      }
      if (input$export_csv) {
        export_csv(results$chem, results$field, output_dir = out_dir)
      }
      results$out_dir <- out_dir

      incProgress(1, detail = "Done")
    })

    showNotification(
      glue("Processed {nrow(results$chem)} chemistry rows from {length(paths)} file(s)."),
      type = "message", duration = 6
    )
  })

  # ── Status banner ───────────────────────────────────────────────────────────

  output$process_status <- renderUI({
    req(results$chem)
    n     <- nrow(results$chem)
    nf    <- length(csv_paths())
    fmt   <- if (input$ceden_version == "v2") "CEDEN 2.0" else "CEDEN Legacy"
    div(class = "alert alert-success",
      icon("circle-check"), " ",
      glue("{n} chemistry rows ready ({fmt}) from {nf} file(s).")
    )
  })

  # ── Preview table ───────────────────────────────────────────────────────────

  output$preview_table <- renderDT({
    req(results$chem)
    df <- if (input$ceden_version == "v2") results$chem_v2 else results$chem
    datatable(df,
      options = list(scrollX = TRUE, pageLength = 15,
                     dom = "lBfrtip",
                     buttons = list("copy", "csv")),
      extensions = "Buttons",
      rownames = FALSE,
      class = "table-sm table-striped"
    )
  })

  # ── Validation UI ───────────────────────────────────────────────────────────

  output$validation_ui <- renderUI({
    req(results$val_issues)
    issues <- results$val_issues

    make_card <- function(title, issue_list) {
      if (length(issue_list) == 0) {
        card(
          card_header(class = "bg-success text-white", icon("check"), " ", title),
          card_body(p("All checks passed — no issues found."))
        )
      } else {
        rows <- lapply(names(issue_list), function(nm) {
          tags$li(class = "text-danger",
            tags$strong(nm), ": ", issue_list[[nm]])
        })
        card(
          card_header(class = "bg-warning", icon("triangle-exclamation"), " ", title),
          card_body(tags$ul(rows))
        )
      }
    }

    layout_columns(
      col_widths = c(6, 6),
      make_card("Chemistry table", issues$chem),
      make_card("Field table",     issues$field)
    )
  })

  # ── QA Report UI ────────────────────────────────────────────────────────────

  output$qa_ui <- renderUI({
    if (!input$run_qa) {
      return(p(class = "text-muted mt-3",
               "QA spot-check is disabled. Enable it in the sidebar and re-process."))
    }
    req(results$qa)
    qa <- results$qa

    pass_color <- if (qa$overall_pass_rate >= 95) "success"
                  else if (qa$overall_pass_rate >= 80) "warning"
                  else "danger"

    tagList(
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box("Records reviewed",
                  glue("{qa$n_sampled} of {qa$n_total}"),
                  showcase = icon("vials"),
                  theme = "primary"),
        value_box("Sample rate",
                  glue("{qa$pct_sampled}%"),
                  showcase = icon("percent"),
                  theme = "primary"),
        value_box("Overall pass rate",
                  glue("{qa$overall_pass_rate}%"),
                  showcase = icon("chart-bar"),
                  theme = pass_color),
        value_box("Checks per record",
                  qa$check_summary$Check %>% length(),
                  showcase = icon("list-check"),
                  theme = "secondary")
      ),
      br(),
      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("Check summary"),
          card_body(
            reactable(
              qa$check_summary %>% select(Description, Pass, Fail, Pct_Pass),
              columns = list(
                Pct_Pass = colDef(name = "% Pass",
                  cell = function(v) {
                    color <- if (v == 100) "#27ae60" else if (v >= 90) "#f39c12" else "#e74c3c"
                    div(style = glue("color:{color};font-weight:bold"), glue("{v}%"))
                  }
                )
              ),
              striped = TRUE, highlight = TRUE, compact = TRUE
            )
          )
        ),
        card(
          card_header("Sampled records detail"),
          card_body(
            DTOutput("qa_detail_table")
          )
        )
      )
    )
  })

  output$qa_detail_table <- renderDT({
    req(results$qa)
    detail <- results$qa$detail

    # Show key identity cols + pass/fail cols only
    check_cols <- names(detail)[startsWith(names(detail), "ck_")]
    id_cols    <- intersect(c("StationCode","#StationCode","Analyte","AnalyteName",
                               "SampleDate","CollectionDateTime","Result","ResQualCode",
                               "DetectedAboveMDL"), names(detail))
    show_cols  <- c(id_cols, check_cols, "row_pass", "pct_pass")
    show_cols  <- intersect(show_cols, names(detail))

    datatable(
      detail[show_cols],
      options  = list(scrollX = TRUE, pageLength = 10, dom = "tip"),
      rownames = FALSE,
      class    = "table-sm table-striped"
    ) %>%
      formatStyle(
        columns    = "row_pass",
        target     = "row",
        backgroundColor = styleEqual(c(TRUE, FALSE), c("#d4edda", "#f8d7da"))
      )
  })

  # ── Mapping tables ──────────────────────────────────────────────────────────

  analyte_map_rv <- reactiveVal(
    read_csv("lookup/analyte_map.csv", show_col_types = FALSE)
  )
  matrix_map_rv  <- reactiveVal(
    read_csv("lookup/matrix_map.csv", show_col_types = FALSE)
  )

  output$analyte_map_table <- renderDT({
    datatable(analyte_map_rv(),
      editable = list(target = "cell", disable = list(columns = 0)),
      options  = list(scrollX = TRUE, pageLength = 25, dom = "tip"),
      rownames = FALSE,
      class    = "table-sm table-striped"
    )
  })

  observeEvent(input$analyte_map_table_cell_edit, {
    info <- input$analyte_map_table_cell_edit
    df   <- analyte_map_rv()
    df[info$row, info$col + 1] <- info$value
    analyte_map_rv(df)
  })

  observeEvent(input$save_analyte_map, {
    write_csv(analyte_map_rv(), "lookup/analyte_map.csv")
    # Reload into global analyte_map so next process run picks it up
    analyte_map <<- analyte_map_rv()
    showNotification("Analyte mapping saved.", type = "message")
  })

  output$matrix_map_table <- renderDT({
    datatable(matrix_map_rv(),
      editable = list(target = "cell"),
      options  = list(scrollX = TRUE, pageLength = 10, dom = "tip"),
      rownames = FALSE,
      class    = "table-sm table-striped"
    )
  })

  observeEvent(input$matrix_map_table_cell_edit, {
    info <- input$matrix_map_table_cell_edit
    df   <- matrix_map_rv()
    df[info$row, info$col + 1] <- info$value
    matrix_map_rv(df)
  })

  observeEvent(input$save_matrix_map, {
    write_csv(matrix_map_rv(), "lookup/matrix_map.csv")
    matrix_map <<- matrix_map_rv()
    showNotification("Matrix mapping saved.", type = "message")
  })

  # ── Download zip ─────────────────────────────────────────────────────────────

  output$btn_download <- downloadHandler(
    filename = function() glue("CEDEN_outputs_{format(Sys.Date(), '%Y%m%d')}.zip"),
    content  = function(file) {
      req(results$out_dir)
      out_files <- list.files(results$out_dir, full.names = TRUE)
      validate(need(length(out_files) > 0, "No output files to download."))
      zip::zipr(file, out_files, recurse = FALSE)
    }
  )
}

# ── Launch ─────────────────────────────────────────────────────────────────────

if (interactive() || !exists("SHINY_PORT")) {
  # Set working directory to repo root so relative paths work
  repo_root <- normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."),
                              mustWork = FALSE)
  if (dir.exists(repo_root)) setwd(repo_root)

  shinyApp(ui, server)
}
