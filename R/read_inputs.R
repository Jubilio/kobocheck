#' Read an XLSForm
#'
#' Reads the `survey` and `choices` sheets of an XLSForm and, if present, a
#' `logical_checks` sheet of custom checks written in XLSForm syntax.
#' Group nesting is resolved so that every question carries the combined
#' relevance of the groups that enclose it.
#'
#' @param path Path to the XLSForm (.xlsx).
#' @param label_col Label column to use for question labels. Defaults to the
#'   first column whose name starts with "label".
#' @return A list with `survey`, `choices`, `logical_checks` (or NULL) and
#'   `questions` (one row per data-bearing question).
#' @export
read_xlsform <- function(path, label_col = NULL) {
  sheets <- readxl::excel_sheets(path)
  if (!"survey" %in% sheets) stop("The XLSForm has no 'survey' sheet.")
  survey <- as.data.frame(readxl::read_excel(path, sheet = "survey", col_types = "text"),
                          stringsAsFactors = FALSE)
  names(survey) <- tolower(trimws(names(survey)))
  choices <- if ("choices" %in% sheets) {
    ch <- as.data.frame(readxl::read_excel(path, sheet = "choices", col_types = "text"),
                        stringsAsFactors = FALSE)
    names(ch) <- tolower(trimws(names(ch)))
    names(ch)[names(ch) == "list name"] <- "list_name"
    ch[!is.na(ch$list_name) & !is.na(ch$name), , drop = FALSE]
  } else {
    data.frame(list_name = character(), name = character(), stringsAsFactors = FALSE)
  }
  lc_sheet <- sheets[tolower(sheets) == "logical_checks"]
  logical_checks <- if (length(lc_sheet)) {
    lc <- as.data.frame(readxl::read_excel(path, sheet = lc_sheet[1], col_types = "text"),
                        stringsAsFactors = FALSE)
    names(lc) <- tolower(trimws(names(lc)))
    lc[!is.na(lc$expression), , drop = FALSE]
  } else NULL

  for (col in c("type", "name", "relevant", "constraint", "required", "calculation")) {
    if (!col %in% names(survey)) survey[[col]] <- NA_character_
  }
  if (is.null(label_col)) {
    lab <- grep("^label", names(survey), value = TRUE)
    label_col <- if (length(lab)) lab[1] else NA
  }
  survey$label <- if (!is.na(label_col)) survey[[label_col]] else NA_character_
  survey <- survey[!is.na(survey$type), , drop = FALSE]
  survey$type <- trimws(gsub("\\s+", " ", survey$type))

  # Resolve group relevance
  stack <- character()
  group_rel <- character(nrow(survey))
  for (i in seq_len(nrow(survey))) {
    t <- survey$type[i]
    group_rel[i] <- if (length(stack)) paste0("(", stack, ")", collapse = " and ") else NA
    if (grepl("^begin[ _](group|repeat)", t)) {
      stack <- c(stack, if (is.na(survey$relevant[i])) "true()" else survey$relevant[i])
    } else if (grepl("^end[ _](group|repeat)", t)) {
      stack <- stack[-length(stack)]
    }
  }
  survey$group_relevant <- group_rel
  survey$in_repeat <- FALSE
  depth <- 0
  for (i in seq_len(nrow(survey))) {
    if (grepl("^begin[ _]repeat", survey$type[i])) depth <- depth + 1
    if (grepl("^end[ _]repeat", survey$type[i])) depth <- depth - 1
    survey$in_repeat[i] <- depth > 0 && !grepl("^begin[ _]repeat", survey$type[i])
  }

  base_type <- sub(" .*$", "", survey$type)
  survey$base_type <- base_type
  survey$list_name <- ifelse(base_type %in% c("select_one", "select_multiple"),
                             sub(" or_other$", "", sub("^\\S+\\s+", "", survey$type)), NA)
  survey$or_other <- grepl("or_other$", survey$type)

  data_types <- c("integer", "decimal", "text", "select_one", "select_multiple", "date",
                  "time", "datetime", "geopoint", "calculate", "range", "barcode", "note",
                  "image", "audio", "video", "file", "geotrace", "geoshape", "acknowledge")
  q <- survey[base_type %in% data_types & !is.na(survey$name), , drop = FALSE]
  q$effective_relevant <- mapply(function(own, grp) {
    parts <- c(if (!is.na(grp)) grp, if (!is.na(own)) paste0("(", own, ")"))
    if (length(parts)) paste(parts, collapse = " and ") else NA_character_
  }, q$relevant, q$group_relevant, USE.NAMES = FALSE)
  q$is_required <- tolower(trimws(q$required)) %in% c("yes", "true", "true()")
  rownames(q) <- NULL

  list(survey = survey, choices = choices, logical_checks = logical_checks,
       questions = q, label_col = label_col)
}

#' Read a Kobo data export
#'
#' Reads the main sheet of a Kobo .xlsx export (or a .csv) as text, and
#' normalises column names to the XLSForm `name` values: group prefixes are
#' removed and select_multiple child columns become `question/option`.
#'
#' @param path Path to the export (.xlsx, .xls or .csv).
#' @param form Output of [read_xlsform()], used to normalise names.
#' @param sheet Sheet to read for Excel files. Defaults to the first sheet.
#' @return A data.frame of character columns.
#' @export
read_kobo_data <- function(path, form = NULL, sheet = 1) {
  ext <- tolower(tools::file_ext(path))
  df <- if (ext %in% c("xlsx", "xls")) {
    as.data.frame(readxl::read_excel(path, sheet = sheet, col_types = "text",
                                     .name_repair = "minimal"),
                  stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    first <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
    sep <- if (lengths(regmatches(first, gregexpr(";", first))) >
               lengths(regmatches(first, gregexpr(",", first)))) ";" else ","
    utils::read.csv(path, sep = sep, colClasses = "character", check.names = FALSE,
                    na.strings = c("", "NA"), encoding = "UTF-8")
  }
  names(df) <- sub("^\ufeff", "", names(df))
  if (!is.null(form)) names(df) <- normalise_names(names(df), form)
  df[] <- lapply(df, function(x) { x <- trimws(as.character(x)); x[x %in% c("", "NA")] <- NA; x })
  df
}

normalise_names <- function(cols, form) {
  q <- form$questions
  qnames <- q$name
  sm <- q$name[q$base_type == "select_multiple"]
  out <- vapply(cols, function(cl) {
    parts <- strsplit(cl, "[/]")[[1]]
    if (length(parts) == 1) parts <- strsplit(cl, ".", fixed = TRUE)[[1]]
    n <- length(parts)
    if (n == 1) return(cl)
    if (parts[n - 1] %in% sm) return(paste0(parts[n - 1], "/", parts[n]))
    if (parts[n] %in% qnames) return(parts[n])
    if (cl %in% qnames) return(cl)
    parts[n]
  }, character(1), USE.NAMES = FALSE)
  # never create duplicate names
  dup <- duplicated(out) | duplicated(out, fromLast = TRUE)
  out[dup] <- cols[dup]
  out
}

#' Read Kobo audit files
#'
#' Accepts a .zip downloaded from Kobo (one `<uuid>/audit.csv` per
#' submission), a folder with the same layout, or a vector of audit.csv paths.
#' Returns one row per submission with total and active (time spent on
#' questions) duration in minutes.
#'
#' @param path Path to a .zip, a directory, or audit.csv files.
#' @return data.frame with `uuid`, `audit_total_min`, `audit_active_min`,
#'   `n_questions_answered` and `median_sec_per_question`.
#' @export
read_audits <- function(path) {
  files <- character()
  if (length(path) == 1 && grepl("\\.zip$", path, ignore.case = TRUE)) {
    exdir <- file.path(tempdir(), paste0("audit_", as.integer(stats::runif(1, 1, 1e9))))
    utils::unzip(path, exdir = exdir)
    files <- list.files(exdir, pattern = "audit\\.csv$", recursive = TRUE, full.names = TRUE)
  } else if (length(path) == 1 && dir.exists(path)) {
    files <- list.files(path, pattern = "audit\\.csv$", recursive = TRUE, full.names = TRUE)
  } else {
    files <- path
  }
  if (!length(files)) stop("No audit.csv files found.")
  rows <- lapply(files, function(f) {
    uuid <- basename(dirname(f))
    a <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(a) || !all(c("event", "start", "end") %in% names(a))) return(NULL)
    a$start <- suppressWarnings(as.numeric(a$start))
    a$end <- suppressWarnings(as.numeric(a$end))
    qev <- a[a$event %in% c("question", "group questions") & !is.na(a$end), , drop = FALSE]
    secs <- (qev$end - qev$start) / 1000
    data.frame(
      uuid = sub("^uuid:", "", uuid),
      audit_total_min = (max(a$end, a$start, na.rm = TRUE) - min(a$start, na.rm = TRUE)) / 60000,
      audit_active_min = sum(secs, na.rm = TRUE) / 60,
      n_questions_answered = nrow(qev),
      median_sec_per_question = if (length(secs)) stats::median(secs) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
