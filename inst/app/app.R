library(shiny)
library(bslib)
library(DT)
library(kobocheck)

options(shiny.maxRequestSize = 300 * 1024^2)

check_labels <- c(
  duplicates = "Duplicate uuids and identical surveys",
  near_duplicates = "Near-duplicate surveys (same enumerator)",
  duration = "Survey duration",
  outliers = "Numeric outliers",
  others = "'Other, specify' responses",
  skip_logic = "Skip logic (from XLSForm 'relevant')",
  required = "Required questions left empty",
  constraints = "Constraints (from XLSForm 'constraint')",
  choices = "Values not in the choice list",
  select_multiple = "select_multiple consistency",
  logical = "Custom logical checks (logical_checks sheet)",
  special_values = "Don't-know codes (-999, 99, 88...)",
  missing_share = "Surveys with many applicable questions empty",
  pii = "Personal data (PII) screening"
)

dt <- function(df, page = 15) {
  datatable(df, rownames = FALSE, filter = "top", class = "compact stripe",
            options = list(pageLength = page, scrollX = TRUE, dom = "ltip"))
}

ui <- page_navbar(
  title = "kobocheck",
  theme = bs_theme(version = 5, primary = "#1F3864", base_font = font_collection("Segoe UI", "Arial", "sans-serif")),
  nav_panel("1. Run checks",
    layout_sidebar(
      sidebar = sidebar(width = 340,
        fileInput("data", "Kobo export (.xlsx or .csv)", accept = c(".xlsx", ".xls", ".csv")),
        fileInput("form", "XLSForm (.xlsx)", accept = ".xlsx"),
        fileInput("audit", "Audit files (.zip, optional)", accept = ".zip"),
        uiOutput("col_pickers"),
        accordion(open = FALSE,
          accordion_panel("Settings",
            numericInput("min_dur", "Minimum duration (minutes)", 15, min = 0),
            numericInput("max_dur", "Maximum duration (minutes)", 120, min = 1),
            numericInput("z", "Outlier threshold (robust z)", 3.5, min = 1, step = 0.5),
            textInput("dk", "Codes to ignore in outliers", "-999, -998, -99, -98, -1, 999, 9999"),
            textInput("excl", "Exclusive select_multiple options", "dk, pnta, none, dont_know, unknown, refused"),
            numericInput("nd", "Near-duplicate threshold (% of answers different)", 5, min = 0, max = 50)
          ),
          accordion_panel("Checks to run",
            checkboxGroupInput("checks", NULL, choices = setNames(names(check_labels), check_labels),
                               selected = names(check_labels))
          )
        ),
        actionButton("run", "Run checks", class = "btn-primary w-100"),
        downloadButton("dl_log", "Download cleaning log", class = "w-100 mt-2")
      ),
      uiOutput("status"),
      uiOutput("boxes"),
      navset_card_underline(
        nav_panel("Cleaning log", DTOutput("log")),
        nav_panel("By check", DTOutput("summary")),
        nav_panel("Enumerators", p(class = "text-muted small",
          "Screening signals for follow-up with field teams, not conclusions about individual enumerators."),
          DTOutput("enum")),
        nav_panel("Durations", DTOutput("dur")),
        nav_panel("PII", p(class = "text-muted small",
          "Columns that may contain personal data. Remove them from the shared dataset (step 2 can do this and records it in the variable tracker)."),
          DTOutput("pii")),
        nav_panel("Not checked", p(class = "text-muted small",
          "Form expressions kobocheck could not evaluate. Check these by hand or simplify them."),
          DTOutput("notchk"))
      )
    )
  ),
  nav_panel("2. Apply reviewed log",
    layout_sidebar(
      sidebar = sidebar(width = 340,
        p("Upload the cleaning log after the team has filled change_type, new_value and comment."),
        fileInput("rev_log", "Reviewed cleaning log (.xlsx)", accept = ".xlsx"),
        p(class = "small text-muted", "The Kobo export and XLSForm from step 1 are reused."),
        actionButton("apply", "Apply log", class = "btn-primary w-100"),
        downloadButton("dl_clean", "Download clean data", class = "w-100 mt-2"),
        hr(),
        h6("Final dataset package"),
        p(class = "small text-muted", "Raw and clean data, cleaning and deletion logs, tool, variable tracker and checklist, in the sheet layout checked by IMPACT's Argus validator."),
        selectInput("pkg_prefix", "Sheet prefix", c("hh_ (MSNA household)" = "hh_", "none (other programmes)" = "")),
        checkboxInput("pkg_pii", "Remove PII columns from clean data", TRUE),
        downloadButton("dl_pkg", "Download dataset package", class = "w-100")
      ),
      uiOutput("apply_status"),
      navset_card_underline(
        nav_panel("Applied log", DTOutput("applied")),
        nav_panel("Deletion log", DTOutput("deleted"))
      )
    )
  ),
  nav_panel("Help",
    card(card_body(
      h4("What kobocheck does"),
      p("It reads your Kobo export, the XLSForm and (optionally) the audit zip, runs the checks below and produces a cleaning log in the usual REACH/IMPACT format. Skip-logic and constraint checks come directly from your XLSForm, so you do not write them yourself."),
      tags$ul(lapply(check_labels, tags$li)),
      h4("Custom checks without code"),
      p("Add a sheet called ", code("logical_checks"), " to your XLSForm with the columns ",
        code("check_id"), ", ", code("description"), ", ", code("expression"), " and ", code("questions"),
        ". Write the expression in XLSForm syntax; a row is flagged when it is true, e.g. ",
        code("${hh_size} = 1 and ${children_u5} >= 1"), "."),
      h4("Limits of this version"),
      tags$ul(tags$li("Repeat groups are not checked."),
              tags$li("Some XLSForm functions (jr:choice-name, format-date, sums over repeats) are listed under 'Not checked'."),
              tags$li("Flags are prompts for review, not confirmed errors."))
    ))
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(res = NULL, form = NULL, data = NULL, applied = NULL, msg = NULL)

  inputs <- reactive({
    req(input$data, input$form)
    form <- read_xlsform(input$form$datapath)
    path <- input$data$datapath
    ext <- tolower(tools::file_ext(input$data$name))
    if (!grepl(paste0("\\.", ext, "$"), path)) { np <- paste0(path, ".", ext); file.copy(path, np); path <- np }
    data <- read_kobo_data(path, form)
    list(form = form, data = data)
  })

  output$col_pickers <- renderUI({
    d <- tryCatch(inputs()$data, error = function(e) NULL)
    if (is.null(d)) return(NULL)
    cfg <- kobocheck:::resolve_columns(d, kc_config())
    tagList(
      selectInput("uuid_col", "uuid column", names(d), selected = cfg$uuid_col),
      selectInput("enum_col", "Enumerator column", c("(none)", names(d)),
                  selected = if (is.null(cfg$enumerator_col)) "(none)" else cfg$enumerator_col)
    )
  })

  num_list <- function(s) { v <- suppressWarnings(as.numeric(trimws(strsplit(s, ",")[[1]]))); v[!is.na(v)] }
  chr_list <- function(s) { v <- trimws(strsplit(s, ",")[[1]]); v[nzchar(v)] }

  observeEvent(input$run, {
    rv$msg <- NULL
    tryCatch({
      withProgress(message = "Running checks", value = 0.2, {
        inp <- inputs()
        audit <- if (!is.null(input$audit)) {
          incProgress(0.2, detail = "reading audit files")
          read_audits(input$audit$datapath)
        } else NULL
        cfg <- kc_config(
          uuid_col = input$uuid_col,
          enumerator_col = if (is.null(input$enum_col) || input$enum_col == "(none)") NULL else input$enum_col,
          min_duration = input$min_dur, max_duration = input$max_dur, outlier_z = input$z,
          dk_codes = num_list(input$dk), exclusive_options = chr_list(input$excl),
          near_dup_share = input$nd / 100, checks = input$checks)
        incProgress(0.3, detail = "checking")
        rv$res <- run_checks(inp$data, inp$form, cfg, audit)
        rv$form <- inp$form; rv$data <- inp$data
      })
    }, error = function(e) rv$msg <- conditionMessage(e))
  })

  output$status <- renderUI({
    if (!is.null(rv$msg)) return(div(class = "alert alert-danger", rv$msg))
    if (is.null(rv$res)) div(class = "alert alert-light",
      "Upload the Kobo export and the XLSForm, then press Run checks.")
  })

  log_df <- reactive({ req(rv$res); build_cleaning_log(rv$res) })

  output$boxes <- renderUI({
    req(rv$res)
    r <- rv$res; lg <- log_df()
    layout_column_wrap(width = 1/4, fill = FALSE,
      value_box("Surveys", nrow(r$data), theme = "primary"),
      value_box("Flags", nrow(lg), theme = "secondary"),
      value_box("Surveys flagged", sprintf("%d (%.0f%%)", length(unique(lg$uuid)),
                100 * length(unique(lg$uuid)) / max(nrow(r$data), 1)), theme = "secondary"),
      value_box("Not checked", nrow(r$not_checked), theme = if (nrow(r$not_checked)) "warning" else "secondary")
    )
  })

  output$log <- renderDT(dt(log_df()[, c("uuid", "enumerator", "question", "old_value", "issue", "check_type")]))
  output$summary <- renderDT({
    lg <- log_df(); req(nrow(lg))
    t <- aggregate(list(flags = lg$uuid), list(check_type = lg$check_type, check_id = lg$check_id), length)
    t$surveys <- aggregate(lg$uuid, list(lg$check_type, lg$check_id), function(x) length(unique(x)))$x
    dt(t[order(-t$flags), ])
  })
  output$enum <- renderDT({ s <- enumerator_summary(rv$res); req(s); dt(s) })
  output$dur <- renderDT({ req(rv$res$durations); dt(rv$res$durations) })
  output$notchk <- renderDT(dt(req(rv$res)$not_checked))
  output$pii <- renderDT(dt(req(rv$res)$pii))

  output$dl_log <- downloadHandler(
    filename = function() paste0(format(Sys.Date()), "_cleaning_log.xlsx"),
    content = function(file) { req(rv$res); write_cleaning_log(rv$res, file) })

  observeEvent(input$apply, {
    rv$applied <- NULL
    tryCatch({
      req(rv$res, input$rev_log)
      rv$applied <- apply_cleaning_log(rv$data, input$rev_log$datapath, rv$form, rv$res$cfg$uuid_col)
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  output$apply_status <- renderUI({
    if (is.null(rv$res)) return(div(class = "alert alert-light", "Run the checks in step 1 first."))
    a <- rv$applied; if (is.null(a)) return(NULL)
    st <- table(a$log$apply_status)
    div(class = "alert alert-info", paste(names(st), st, sep = ": ", collapse = " | "),
        tags$br(), sprintf("Clean data: %d surveys (removed %d)", nrow(a$data), nrow(a$deletion_log)))
  })
  output$applied <- renderDT({ req(rv$applied); dt(rv$applied$log) })
  output$deleted <- renderDT({ req(rv$applied); dt(rv$applied$deletion_log) })
  output$dl_pkg <- downloadHandler(
    filename = function() paste0(format(Sys.Date()), "_dataset_package.xlsx"),
    content = function(file) {
      req(rv$applied)
      export_dataset_package(rv$applied, rv$data, rv$form, file, prefix = input$pkg_prefix,
                             uuid_col = rv$res$cfg$uuid_col, result = rv$res,
                             enumerator_performance = enumerator_summary(rv$res),
                             remove_pii = if (isTRUE(input$pkg_pii)) rv$res$pii$question else NULL)
    })
  output$dl_clean <- downloadHandler(
    filename = function() paste0(format(Sys.Date()), "_clean_data.xlsx"),
    content = function(file) { req(rv$applied); write_clean_data(rv$applied, file) })
}

shinyApp(ui, server)
