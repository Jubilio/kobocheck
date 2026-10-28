change_types <- c("change_response", "blank_response", "remove_survey", "no_action")

#' Build the cleaning log
#'
#' Turns the flags from [run_checks()] into a cleaning log with the columns
#' used by REACH / IMPACT cleaning workflows (`uuid`, `question`,
#' `old_value`, `new_value`, `change_type`, `issue`, `check_id`,
#' `check_binding`). `new_value`, `change_type` and `comment` are left empty
#' for the reviewer.
#'
#' @param result Output of [run_checks()].
#' @return data.frame.
#' @export
build_cleaning_log <- function(result) {
  f <- result$flags
  cfg <- result$cfg
  d <- result$data
  if (!nrow(f)) {
    return(data.frame(uuid = character(), enumerator = character(), question = character(),
                      question_label = character(), old_value = character(), new_value = character(),
                      change_type = character(), issue = character(), check_id = character(),
                      check_type = character(), check_binding = character(), comment = character(),
                      stringsAsFactors = FALSE))
  }
  enum <- if (!is.null(cfg$enumerator_col)) d[[cfg$enumerator_col]][match(f$uuid, d[[cfg$uuid_col]])] else NA
  q <- result$form$questions
  base_q <- sub("/.*$", "", f$question)
  lab <- q$label[match(base_q, q$name)]
  log <- data.frame(
    uuid = f$uuid, enumerator = enum, question = f$question, question_label = lab,
    old_value = f$old_value, new_value = NA_character_, change_type = NA_character_,
    issue = f$issue, check_id = f$check_id, check_type = f$check_type,
    check_binding = ifelse(nzchar(f$question), paste(f$question, f$uuid, sep = " ~/~ "),
                           paste(f$check_id, f$uuid, sep = " ~/~ ")),
    comment = NA_character_, stringsAsFactors = FALSE)
  # one row (one decision) per uuid and question: merge flags on the same cell
  key <- ifelse(nzchar(log$question) & log$check_id != "dup_uuid",
                paste(log$uuid, log$question, sep = "\r"), paste0("row", seq_len(nrow(log))))
  if (any(duplicated(key))) {
    grp <- split(seq_len(nrow(log)), factor(key, levels = unique(key)))
    log <- do.call(rbind, lapply(grp, function(ix) {
      r <- log[ix[1], , drop = FALSE]
      if (length(ix) > 1) {
        r$issue <- paste(unique(log$issue[ix]), collapse = " | ")
        r$check_id <- paste(unique(log$check_id[ix]), collapse = ", ")
        r$check_type <- paste(unique(log$check_type[ix]), collapse = ", ")
      }
      r
    }))
  }
  ord <- order(ifelse(is.na(log$enumerator), "", log$enumerator), log$uuid, log$check_type)
  log <- log[ord, , drop = FALSE]
  rownames(log) <- NULL
  log
}

#' Summarise flags by enumerator
#'
#' Flags in this table are screening signals, not proof of a problem: they
#' show where to look first during follow-up with field teams.
#'
#' @param result Output of [run_checks()].
#' @return data.frame with one row per enumerator.
#' @export
enumerator_summary <- function(result) {
  cfg <- result$cfg
  d <- result$data
  if (is.null(cfg$enumerator_col)) return(NULL)
  enum <- d[[cfg$enumerator_col]]; enum[is.na(enum)] <- "(missing)"
  u <- d[[cfg$uuid_col]]
  f <- result$flags
  f$enum <- enum[match(f$uuid, u)]
  types <- sort(unique(f$check_type))
  dur <- result$durations
  rows <- lapply(sort(unique(enum)), function(e) {
    uu <- u[enum == e]
    fe <- f[f$enum %in% e, , drop = FALSE]
    dd <- if (!is.null(dur)) dur$duration_min[dur$uuid %in% uu] else NA
    r <- data.frame(enumerator = e, surveys = length(uu),
                    median_duration_min = if (all(is.na(dd))) NA else round(stats::median(dd, na.rm = TRUE), 1),
                    short_surveys = sum(dd < cfg$min_duration, na.rm = TRUE),
                    total_flags = nrow(fe),
                    surveys_with_flags = length(unique(fe$uuid)),
                    stringsAsFactors = FALSE)
    r$flags_per_survey <- round(r$total_flags / r$surveys, 2)
    for (t in types) r[[paste0("n_", t)]] <- sum(fe$check_type == t)
    r
  })
  s <- do.call(rbind, rows)
  team_med <- stats::median(s$flags_per_survey)
  pct_short <- s$short_surveys / s$surveys
  reasons <- mapply(function(fps, ps, nd) {
    r <- character()
    if (team_med > 0 && fps > 2 * team_med) r <- c(r, "flags per survey > 2x team median")
    if (ps > 0.2) r <- c(r, sprintf("%.0f%% surveys too short", 100 * ps))
    if (nd > 0) r <- c(r, "duplicate / near-duplicate surveys")
    paste(r, collapse = "; ")
  }, s$flags_per_survey, pct_short,
  (if ("n_duplicates" %in% names(s)) s$n_duplicates else 0) +
    (if ("n_near_duplicates" %in% names(s)) s$n_near_duplicates else 0))
  s$follow_up <- ifelse(nzchar(reasons), reasons, "")
  s[order(-s$flags_per_survey), , drop = FALSE]
}

# ---- Excel output ------------------------------------------------------------

.kc_styles <- function() list(
  header = openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = "#1F3864", textDecoration = "bold",
                                 halign = "left", valign = "center", border = "TopBottomLeftRight",
                                 borderColour = "#BFBFBF", wrapText = TRUE),
  band = openxlsx::createStyle(fgFill = "#F2F2F2"),
  edit = openxlsx::createStyle(fgFill = "#FFF2CC", border = "TopBottomLeftRight", borderColour = "#BFBFBF"),
  title = openxlsx::createStyle(fontSize = 14, textDecoration = "bold", fontColour = "#1F3864"),
  bold = openxlsx::createStyle(textDecoration = "bold")
)

.write_table <- function(wb, sheet, df, widths = NULL, st = .kc_styles()) {
  openxlsx::addWorksheet(wb, sheet)
  if (is.null(df) || !ncol(df)) df <- data.frame(note = "Nothing to show")
  openxlsx::writeData(wb, sheet, df, headerStyle = st$header, withFilter = TRUE)
  if (nrow(df) > 1) {
    even <- seq(3, nrow(df) + 1, by = 2)
    if (length(even)) openxlsx::addStyle(wb, sheet, st$band, rows = even, cols = seq_len(ncol(df)),
                                         gridExpand = TRUE, stack = TRUE)
  }
  openxlsx::freezePane(wb, sheet, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(df)),
                         widths = if (is.null(widths)) "auto" else widths)
  invisible(wb)
}

#' Write the cleaning log workbook
#'
#' Writes a styled .xlsx with README, cleaning_log (with a drop-down list for
#' `change_type`), summary, enumerators, durations and not_checked sheets.
#'
#' @param result Output of [run_checks()].
#' @param path Output path (.xlsx).
#' @return The path, invisibly.
#' @export
write_cleaning_log <- function(result, path) {
  st <- .kc_styles()
  log <- build_cleaning_log(result)
  wb <- openxlsx::createWorkbook()

  # README
  openxlsx::addWorksheet(wb, "README")
  cfg <- result$cfg
  info <- data.frame(item = c(
    "Generated", "Surveys checked", "Flags raised", "Surveys with at least one flag",
    "uuid column", "Enumerator column", "Duration bounds (min)", "Outlier threshold (modified z)",
    "Codes excluded from outliers", "Expressions not evaluated"),
    value = c(format(Sys.time(), "%Y-%m-%d %H:%M"), nrow(result$data), nrow(log),
              length(unique(log$uuid)), cfg$uuid_col,
              if (is.null(cfg$enumerator_col)) "not found" else cfg$enumerator_col,
              paste(cfg$min_duration, "-", cfg$max_duration), cfg$outlier_z,
              paste(cfg$dk_codes, collapse = ", "), nrow(result$not_checked)),
    stringsAsFactors = FALSE)
  openxlsx::writeData(wb, "README", "Cleaning log generated by kobocheck", startRow = 1)
  openxlsx::addStyle(wb, "README", st$title, rows = 1, cols = 1)
  openxlsx::writeData(wb, "README", info, startRow = 3, headerStyle = st$header)
  guide <- data.frame(
    step = c("1", "2", "3", "4", "5"),
    instruction = c(
      "Review each row of the cleaning_log sheet with the field team.",
      "Fill change_type (yellow column) using the drop-down: change_response, blank_response, remove_survey or no_action.",
      "For change_response, write the corrected value in new_value. For select_multiple questions write the full space-separated list (e.g. 'water food').",
      "Explain the decision in comment, for example the enumerator's answer during follow-up.",
      "Load the reviewed file back into kobocheck (apply_cleaning_log() or the 'Apply log' tab) to produce the clean dataset and deletion log."),
    stringsAsFactors = FALSE)
  openxlsx::writeData(wb, "README", guide, startRow = nrow(info) + 6, headerStyle = st$header)
  notes <- c("Flags are prompts for review, not confirmed errors.",
             "Skip-logic and constraint checks are derived from the XLSForm; Kobo enforces these at collection, so hits usually point to form-version changes or edited submissions.",
             "Questions inside repeat groups are not checked in this version.",
             "pii_review lists columns that may hold personal data: remove them from the shared dataset and record this in the variable tracker.")
  openxlsx::writeData(wb, "README", data.frame(notes = notes), startRow = nrow(info) + nrow(guide) + 9,
                      headerStyle = st$header)
  openxlsx::setColWidths(wb, "README", cols = 1:2, widths = c(34, 110))

  # cleaning log
  .write_table(wb, "cleaning_log", log,
               widths = c(38, 14, 26, 40, 22, 18, 18, 60, 22, 16, 40, 40)[seq_len(ncol(log))])
  if (nrow(log)) {
    ec <- match(c("new_value", "change_type", "comment"), names(log))
    openxlsx::addStyle(wb, "cleaning_log", st$edit, rows = 2:(nrow(log) + 1), cols = ec,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::addWorksheet(wb, "lists", visible = FALSE)
    openxlsx::writeData(wb, "lists", data.frame(change_type = change_types))
    suppressWarnings(openxlsx::dataValidation(wb, "cleaning_log", cols = match("change_type", names(log)),
                             rows = 2:(nrow(log) + 1), type = "list",
                             value = "'lists'!$A$2:$A$5"))
  }

  # summary
  if (nrow(log)) {
    tab <- stats::aggregate(list(flags = log$uuid), by = list(check_type = log$check_type, check_id = log$check_id),
                            FUN = length)
    sv <- stats::aggregate(list(surveys = log$uuid), by = list(check_type = log$check_type, check_id = log$check_id),
                           FUN = function(x) length(unique(x)))
    tab <- merge(tab, sv)
    tab$pct_of_surveys <- round(100 * tab$surveys / nrow(result$data), 1)
    tab <- tab[order(-tab$flags), ]
  } else tab <- data.frame(note = "No flags raised")
  .write_table(wb, "summary", tab)

  .write_table(wb, "enumerators", enumerator_summary(result))
  .write_table(wb, "pii_review", if (!is.null(result$pii) && nrow(result$pii)) result$pii else
    data.frame(note = "No potential PII columns found"))
  .write_table(wb, "durations", result$durations)
  .write_table(wb, "not_checked", if (nrow(result$not_checked)) result$not_checked else
    data.frame(note = "All form expressions were evaluated"))
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

# ---- applying a reviewed log ------------------------------------------------

#' Apply a reviewed cleaning log
#'
#' @param data Raw Kobo data (as read by [read_kobo_data()]).
#' @param log Reviewed cleaning log: a data.frame or a path to the .xlsx
#'   written by [write_cleaning_log()].
#' @param form Optional XLSForm; when supplied, changes to a select_multiple
#'   question also update its `question/option` 0/1 columns.
#' @param uuid_col Name of the uuid column.
#' @return A list with `data` (clean data), `log` (the log with an
#'   `apply_status` column) and `deletion_log`.
#' @export
apply_cleaning_log <- function(data, log, form = NULL, uuid_col = "_uuid") {
  if (is.character(log)) log <- as.data.frame(readxl::read_excel(log, sheet = "cleaning_log",
                                                                 col_types = "text"), stringsAsFactors = FALSE)
  if (!uuid_col %in% names(data)) stop("uuid column '", uuid_col, "' not in data")
  for (cc in c("new_value", "comment", "issue", "check_id", "enumerator", "old_value"))
    if (!cc %in% names(log)) log[[cc]] <- NA_character_
  log$change_type <- trimws(tolower(log$change_type))
  log$apply_status <- ifelse(is.na(log$change_type) | !nzchar(log$change_type), "pending review", NA)
  bad_type <- is.na(log$apply_status) & !log$change_type %in% change_types
  log$apply_status[bad_type] <- "unknown change_type"
  sm <- if (!is.null(form)) form$questions$name[form$questions$base_type == "select_multiple"] else character()

  u <- data[[uuid_col]]
  # rows to remove: by uuid, or a single record by _id for duplicated uuids
  has_id <- "_id" %in% names(data)
  rm_rows <- rep(FALSE, nrow(data))
  rm_idx <- which(is.na(log$apply_status) & log$change_type == "remove_survey" & log$uuid %in% u)
  for (i in rm_idx) {
    by_id <- has_id && grepl("dup_uuid", log$check_id[i] %||% "") && !is.na(log$old_value[i]) &&
      log$old_value[i] %in% data[["_id"]][u == log$uuid[i]]
    rm_rows <- rm_rows | if (by_id) (u == log$uuid[i] & data[["_id"]] %in% log$old_value[i]) else u == log$uuid[i]
  }
  remove <- unique(u[rm_rows])
  fully_removed <- setdiff(remove, u[!rm_rows])
  auto <- list()
  log_child <- function(rows, q, newvals, ct, i) {
    for (k in grep(paste0("^", q, "/"), names(data), value = TRUE)) {
      old <- data[rows, k]
      nv <- if (is.function(newvals)) newvals(k) else newvals
      changed <- !(old %in% nv & !is.na(old)) & !(is.na(old) & is.na(nv))
      if (any(changed)) auto[[length(auto) + 1]] <<- data.frame(
        uuid = u[rows][changed], question = k, old_value = old[changed],
        new_value = if (is.na(nv)) NA_character_ else nv, change_type = ct,
        issue = paste0("Option column updated to match the change to ", q),
        check_id = "sm_child_update", comment = log$comment[i], apply_status = "applied",
        enumerator = log$enumerator[i], stringsAsFactors = FALSE)
      data[rows, k] <<- nv
    }
  }
  for (i in which(is.na(log$apply_status))) {
    ct <- log$change_type[i]; q <- log$question[i]; id <- log$uuid[i]
    rows <- which(u == id)
    if (!length(rows)) { log$apply_status[i] <- "uuid not found"; next }
    if (ct == "remove_survey") { log$apply_status[i] <- "survey removed"; next }
    if (id %in% fully_removed) { log$apply_status[i] <- "not applied: survey removed"; next }
    rows <- rows[!rm_rows[rows]]
    if (ct == "no_action") { log$apply_status[i] <- "no action"; next }
    if (is.na(q) || !q %in% names(data)) { log$apply_status[i] <- "question not found"; next }
    if (ct == "blank_response") {
      data[rows, q] <- NA
      if (q %in% sm) log_child(rows, q, NA_character_, "blank_response", i)
      log$apply_status[i] <- "applied"
    } else if (ct == "change_response") {
      nv <- log$new_value[i]
      if (is.na(nv) || !nzchar(nv)) { log$apply_status[i] <- "new_value missing"; next }
      same <- all(data[rows, q] %in% nv)
      data[rows, q] <- nv
      if (same) log$apply_status[i] <- "applied: value unchanged"
      if (q %in% sm) {
        tok <- strsplit(nv, "\\s+")[[1]]
        log_child(rows, q, function(k) if (sub("^.*/", "", k) %in% tok) "1" else "0", "change_response", i)
      }
      if (is.na(log$apply_status[i])) log$apply_status[i] <- "applied"
    }
  }
  if (length(auto)) {
    a <- do.call(rbind, auto)
    for (cc in setdiff(names(log), names(a))) a[[cc]] <- NA
    log <- rbind(log, a[, names(log)])
  }
  # one deletion-log row per removed record
  first <- log[log$change_type %in% "remove_survey" & log$apply_status %in% "survey removed", , drop = FALSE]
  drop_rows <- which(rm_rows)
  pick <- vapply(drop_rows, function(r) {
    cand <- which(first$uuid == u[r])
    if (has_id) { m <- cand[first$old_value[cand] %in% data[["_id"]][r] & grepl("dup_uuid", first$check_id[cand])]
                  if (length(m)) return(m[1]) }
    if (length(cand)) cand[1] else NA_integer_
  }, integer(1))
  deletion_log <- first[pick, , drop = FALSE]
  deletion_log$uuid <- u[drop_rows]
  if (has_id) deletion_log$`_id` <- data[["_id"]][drop_rows]
  rownames(deletion_log) <- NULL
  clean <- data[!rm_rows, , drop = FALSE]
  rownames(clean) <- NULL
  list(data = clean, log = log, deletion_log = deletion_log)
}

#' Write clean data, applied log and deletion log to one workbook
#' @param applied Output of [apply_cleaning_log()].
#' @param path Output path (.xlsx).
#' @export
write_clean_data <- function(applied, path) {
  wb <- openxlsx::createWorkbook()
  .write_table(wb, "clean_data", applied$data)
  .write_table(wb, "cleaning_log_applied", applied$log)
  .write_table(wb, "deletion_log", if (nrow(applied$deletion_log)) applied$deletion_log else
    data.frame(note = "No surveys removed"))
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

#' Run the whole workflow from file paths
#'
#' @param data_path Kobo export (.xlsx or .csv).
#' @param form_path XLSForm (.xlsx).
#' @param audit_path Optional audit .zip or folder.
#' @param output_path Output .xlsx for the cleaning log.
#' @param cfg Settings from [kc_config()].
#' @return The `kc_result`, invisibly.
#' @export
kobocheck <- function(data_path, form_path, audit_path = NULL,
                      output_path = paste0(format(Sys.Date()), "_cleaning_log.xlsx"),
                      cfg = kc_config()) {
  form <- read_xlsform(form_path)
  data <- read_kobo_data(data_path, form)
  audit <- if (!is.null(audit_path)) read_audits(audit_path) else NULL
  res <- run_checks(data, form, cfg, audit)
  write_cleaning_log(res, output_path)
  message("Cleaning log written to ", output_path)
  invisible(res)
}

#' Launch the kobocheck Shiny app
#' @param ... Passed to [shiny::runApp()].
#' @export
run_app <- function(...) {
  for (p in c("shiny", "bslib", "DT")) if (!requireNamespace(p, quietly = TRUE))
    stop("Install the '", p, "' package to use the app.")
  shiny::runApp(system.file("app", package = "kobocheck"), ...)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a
