#' Export the final dataset package
#'
#' Writes the workbook that is submitted with a cleaned dataset: raw data,
#' clean data, cleaning log, deletion log, XLSForm survey and choices and a
#' variable tracker. Sheet and column names follow the IMPACT Argus schemas,
#' so the file can be validated with Argus before submission.
#'
#' Only rows that changed the data (`change_response`, `blank_response`) go
#' into the cleaning log sheet. Removed surveys go into the deletion log with
#' `reason_deletion` taken from `comment` (or `issue` when no comment was
#' written). With `drop_no_consent = TRUE`, surveys without consent are
#' removed from the clean data and recorded in the deletion log.
#'
#' @param applied Output of [apply_cleaning_log()].
#' @param raw Raw data (as read by [read_kobo_data()]).
#' @param form XLSForm from [read_xlsform()] (the original survey and choices
#'   sheets are written).
#' @param path Output .xlsx.
#' @param prefix Sheet prefix. Use `"hh_"` for MSNA household data (Argus
#'   `msna` schema) or `""` for other programmes.
#' @param uuid_col uuid column name.
#' @param consent_col Consent column; `NULL` detects `consent`.
#' @param consent_yes Values meaning consent was given.
#' @param drop_no_consent Remove surveys without consent from clean data.
#' @param variable_tracker Optional data.frame with `variable`, `action`,
#'   `rationale` (e.g. columns removed for PII).
#' @param read_me Optional character vector written to the read_me sheet.
#' @param enumerator_performance Optional data.frame (e.g. from
#'   [enumerator_summary()]).
#' @param result Optional output of [run_checks()]; used to fill the
#'   data-cleaning checklist and, with `remove_pii`, the PII columns.
#' @param remove_pii Column names to drop from the clean data (for example
#'   `result$pii$question`). Each is recorded in the variable tracker.
#' @param sm_sep Separator between a select_multiple question and its option
#'   in column names. IMPACT standard is ".".
#' @return The path, invisibly.
#' @export
export_dataset_package <- function(applied, raw, form, path, prefix = "hh_",
                                   uuid_col = "_uuid", consent_col = NULL,
                                   consent_yes = c("yes", "oui", "sim"),
                                   drop_no_consent = TRUE, variable_tracker = NULL,
                                   read_me = NULL, enumerator_performance = NULL,
                                   result = NULL, remove_pii = NULL, sm_sep = ".") {
  clean <- applied$data
  log <- applied$log
  if (is.null(consent_col)) consent_col <- detect_col(raw, c("consent", "consentement", "consentimento"))

  # Argus expects `uuid`; Kobo exports have `_uuid`
  to_uuid <- function(df) {
    if (uuid_col != "uuid") {
      if ("uuid" %in% names(df)) names(df)[names(df) == "uuid"] <- "uuid_meta"
      names(df)[names(df) == uuid_col] <- "uuid"
    }
    df
  }

  del <- applied$deletion_log
  del <- if (nrow(del)) data.frame(
    uuid = del$uuid,
    enumerator = if ("enumerator" %in% names(del)) del$enumerator else NA,
    reason_deletion = ifelse(!is.na(del$comment) & nzchar(del$comment), del$comment, del$issue),
    stringsAsFactors = FALSE) else
    data.frame(uuid = character(), enumerator = character(), reason_deletion = character())

  if (drop_no_consent && !is.null(consent_col) && consent_col %in% names(clean)) {
    nc <- !clean[[consent_col]] %in% consent_yes
    if (any(nc)) {
      del <- rbind(del, data.frame(uuid = clean[[uuid_col]][nc], enumerator = NA,
                                   reason_deletion = "No consent", stringsAsFactors = FALSE))
      clean <- clean[!nc, , drop = FALSE]
    }
  }

  # PII columns removed from the clean data and documented
  vt_pii <- NULL
  if (length(remove_pii)) {
    rp <- intersect(remove_pii, names(clean))
    rp_all <- unique(c(rp, grep(paste0("^(", paste(gsub("([.])", "\\\\\\1", rp), collapse = "|"), ")/"),
                                names(clean), value = TRUE)))
    clean <- clean[, setdiff(names(clean), rp_all), drop = FALSE]
    if (length(rp)) vt_pii <- data.frame(variable = rp, action = "removed",
      rationale = "Personally identifiable information", stringsAsFactors = FALSE)
  }

  cl <- log[log$change_type %in% c("change_response", "blank_response") &
              log$apply_status %in% "applied", , drop = FALSE]
  keep <- intersect(c("uuid", "enumerator", "question", "old_value", "new_value", "change_type",
                      "issue", "check_id", "check_binding", "comment"), names(cl))
  cl <- cl[, keep, drop = FALSE]
  cl$new_value[cl$change_type == "blank_response"] <- NA

  if (is.null(variable_tracker)) variable_tracker <- data.frame(
    variable = character(), action = character(), rationale = character())
  variable_tracker <- rbind(variable_tracker, vt_pii)

  # select_multiple option columns: question/option -> question<sm_sep>option
  sm <- form$questions$name[form$questions$base_type == "select_multiple"]
  ren <- function(x) {
    p <- sub("/.*$", "", x)
    ifelse(grepl("/", x) & p %in% sm, paste0(p, sm_sep, sub("^[^/]*/", "", x)), x)
  }
  raw_out <- raw; names(raw_out) <- ren(names(raw_out))
  names(clean) <- ren(names(clean))
  if (nrow(cl)) cl$question <- ren(cl$question)

  # `or_other` lists: Kobo stores "other", so the choice must exist in the choices sheet
  ch <- form$choices
  oo <- unique(form$questions$list_name[form$questions$or_other])
  for (ln in oo) if (!any(ch$list_name == ln & ch$name == "other")) {
    add <- ch[0, , drop = FALSE][1, , drop = FALSE]
    add[1, ] <- NA; add$list_name <- ln; add$name <- "other"
    for (lc in grep("^label", names(ch), value = TRUE)) add[[lc]] <- "Other"
    ch <- rbind(ch, add)
  }

  checklist <- if (!is.null(result)) {
    ck <- result$cfg$checks
    lab <- c(duplicates = "Duplicate surveys", near_duplicates = "Near-duplicate surveys",
             duration = "Survey duration", outliers = "Outliers", others = "'Other' responses recoded",
             skip_logic = "Skip logic", required = "Required questions", constraints = "Constraints",
             choices = "Values against choice lists", select_multiple = "select_multiple consistency",
             logical = "Logical checks", special_values = "Don't-know / special codes",
             missing_share = "Share of missing answers", pii = "Personal data (PII)")
    n <- vapply(names(lab), function(k) sum(result$flags$check_type == k), numeric(1))
    n["pii"] <- if (!is.null(result$pii)) nrow(result$pii) else 0
    data.frame(check = unname(lab), done = ifelse(names(lab) %in% ck, "yes", "no"),
               flags_raised = ifelse(names(lab) %in% ck, n, NA),
               tool = "kobocheck", stringsAsFactors = FALSE)
  } else data.frame(check = character(), done = character(), flags_raised = numeric(), tool = character())
  if (is.null(read_me)) read_me <- c(
    "Dataset package generated by kobocheck",
    paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")),
    paste("Surveys in raw data:", nrow(raw)),
    paste("Surveys in clean data:", nrow(clean)),
    paste("Surveys deleted:", nrow(del)),
    paste("Values changed or blanked:", nrow(cl)))

  st <- .kc_styles()
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "read_me")
  openxlsx::writeData(wb, "read_me", data.frame(read_me = read_me), headerStyle = st$header)
  openxlsx::setColWidths(wb, "read_me", 1, 90)
  .write_table(wb, paste0(prefix, "raw_data"), to_uuid(raw_out))
  .write_table(wb, paste0(prefix, "clean_data"), to_uuid(clean))
  .write_table(wb, paste0(prefix, "cleaning_log"), cl)
  .write_table(wb, paste0(prefix, "deletion_log"), del)
  sv <- form$survey[, setdiff(names(form$survey),
                              c("label", "group_relevant", "in_repeat", "base_type", "list_name", "or_other")),
                    drop = FALSE]
  .write_table(wb, "survey", sv)
  .write_table(wb, "choices", ch)
  .write_table(wb, "variable_tracker", variable_tracker)
  .write_table(wb, "checklist_data_cleaning", checklist)
  if (!is.null(enumerator_performance))
    .write_table(wb, "enumerator_performance_log", enumerator_performance)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}
