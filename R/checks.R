#' Default settings for the checks
#'
#' @param ... Settings to override. See Details.
#' @details
#' * `uuid_col`, `enumerator_col`, `start_col`, `end_col`: column names. `NULL`
#'   means detect automatically.
#' * `min_duration`, `max_duration`: survey duration bounds in minutes.
#' * `outlier_z`: modified z-score threshold (median/MAD) for outliers.
#' * `outlier_min_n`: minimum number of answers before outliers are checked.
#' * `dk_codes`: numeric codes treated as "don't know" and excluded from
#'   outlier detection (e.g. -999, 99).
#' * `exclusive_options`: select_multiple options that must not be combined
#'   with other options (e.g. `dk`, `pnta`, `none`).
#' * `near_dup_share`: flag a survey when it differs from another survey by the
#'   same enumerator on less than this share of answered questions.
#' * `min_answered_dup`: surveys with fewer answered questions than this
#'   (e.g. no consent) are ignored by the duplicate checks.
#' * `special_codes`: numeric codes flagged as possible "don't know" values
#'   (the list IMPACT's Argus validator rejects in clean data).
#' * `pii_terms`: extra column-name terms to treat as PII.
#' * `checks`: which checks to run.
#' @return A named list.
#' @export
kc_config <- function(...) {
  cfg <- list(
    uuid_col = NULL, enumerator_col = NULL, start_col = NULL, end_col = NULL,
    min_duration = 15, max_duration = 120,
    outlier_z = 3.5, outlier_min_n = 10,
    dk_codes = c(-999, -998, -99, -98, -1, 999, 9999),
    exclusive_options = c("dk", "pnta", "none", "dont_know", "do_not_know", "unknown",
                          "prefer_not_to_answer", "no_answer", "refused"),
    near_dup_share = 0.05,
    min_answered_dup = 5,
    special_codes = c(-999, -99, 99, 999, -88, -888, 88, 888),
    pii_terms = NULL,
    checks = c("duplicates", "near_duplicates", "duration", "outliers", "others",
               "skip_logic", "required", "constraints", "choices", "select_multiple",
               "logical", "special_values", "missing_share", "pii")
  )
  over <- list(...)
  cfg[names(over)] <- over
  cfg
}

meta_cols <- c("start", "end", "today", "deviceid", "simserial", "phonenumber", "subscriberid",
               "username", "audit", "_id", "_uuid", "_index", "_submission_time",
               "_validation_status", "_notes", "_status", "_submitted_by", "__version__",
               "_tags", "_parent_index", "instanceID", "instanceName", "formhub/uuid",
               "_version_", "_xform_id_string", "_attachments", "_geolocation")

detect_col <- function(data, candidates) {
  hit <- candidates[candidates %in% names(data)]
  if (length(hit)) return(hit[1])
  low <- tolower(names(data))
  for (c in tolower(candidates)) if (c %in% low) return(names(data)[match(c, low)])
  NULL
}

resolve_columns <- function(data, cfg) {
  if (is.null(cfg$uuid_col)) cfg$uuid_col <- detect_col(data, c("_uuid", "uuid", "instanceID", "meta/instanceID"))
  if (is.null(cfg$uuid_col)) stop("Could not find a uuid column; set uuid_col.")
  if (is.null(cfg$enumerator_col)) cfg$enumerator_col <- detect_col(data,
    c("enumerator_id", "enumerator", "enum_id", "enum_code", "enumerator_code", "enumerator_name",
      "interviewer", "username", "_submitted_by"))
  if (is.null(cfg$start_col)) cfg$start_col <- detect_col(data, c("start"))
  if (is.null(cfg$end_col)) cfg$end_col <- detect_col(data, c("end"))
  cfg
}

new_flags <- function(uuid = character(), question = character(), old_value = character(),
                      issue = character(), check_id = character(), check_type = character()) {
  n <- max(length(uuid), 0)
  data.frame(uuid = as.character(uuid), question = rep_len(as.character(question), n),
             old_value = rep_len(as.character(old_value), n), issue = rep_len(as.character(issue), n),
             check_id = rep_len(as.character(check_id), n), check_type = rep_len(as.character(check_type), n),
             stringsAsFactors = FALSE)
}
bind_flags <- function(lst) {
  lst <- Filter(function(x) !is.null(x) && nrow(x) > 0, lst)
  if (!length(lst)) return(new_flags())
  do.call(rbind, lst)
}
has_value <- function(x) !is.na(x) & nzchar(x)

# ---- individual checks -------------------------------------------------------

#' @rdname run_checks
#' @export
check_duplicates <- function(data, form, cfg) {
  u <- data[[cfg$uuid_col]]
  out <- list()
  d <- duplicated(u) | duplicated(u, fromLast = TRUE)
  if (any(d)) {
    id <- if ("_id" %in% names(data)) data[["_id"]][d] else rep(NA, sum(d))
    out$uuid <- new_flags(u[d], "_id", id,
      ifelse(is.na(id), "Duplicated uuid",
             paste0("Duplicated uuid: to remove only this record use remove_survey on this row (_id ", id, ")")),
      "dup_uuid", "duplicates")
  }
  ans <- answer_columns(data, form)
  if (length(ans)) {
    key <- do.call(paste, c(lapply(data[ans], function(x) ifelse(is.na(x), "", x)), sep = "\u001f"))
    enough <- rowSums(!is.na(data[ans])) >= cfg$min_answered_dup
    key[!enough] <- paste0("\u001e", seq_len(sum(!enough)))
    d2 <- (duplicated(key) | duplicated(key, fromLast = TRUE)) & !d
    if (any(d2)) {
      grp <- match(key[d2], unique(key[d2]))
      out$rows <- new_flags(u[d2], "", "",
        paste0("All answers identical to another survey (duplicate group ", grp, ")"),
        "dup_answers", "duplicates")
    }
  }
  bind_flags(out)
}

answer_columns <- function(data, form) {
  q <- form$questions
  keep <- q$name[!q$base_type %in% c("note", "calculate", "start", "end", "today",
                                     "deviceid", "audit", "acknowledge") & !q$in_repeat]
  cols <- names(data)[names(data) %in% keep]
  cols <- setdiff(cols, meta_cols)
  if (!is.null(form$extra_exclude)) cols <- setdiff(cols, form$extra_exclude)
  cols
}

#' @rdname run_checks
#' @export
check_near_duplicates <- function(data, form, cfg) {
  ans <- answer_columns(data, form)
  if (length(ans) < 5 || nrow(data) < 3) return(new_flags())
  M <- as.matrix(as.data.frame(lapply(data[ans], function(x) ifelse(is.na(x), "", x)),
                               stringsAsFactors = FALSE))
  grp <- if (!is.null(cfg$enumerator_col)) data[[cfg$enumerator_col]] else rep("all", nrow(data))
  grp[is.na(grp)] <- "(missing)"
  u <- data[[cfg$uuid_col]]
  res <- list()
  enough <- rowSums(M != "") >= cfg$min_answered_dup
  for (g in unique(grp)) {
    idx <- which(grp == g & enough)
    if (length(idx) < 2 || length(idx) > 3000) next
    sub <- M[idx, , drop = FALSE]
    answered <- sub != ""
    for (k in seq_along(idx)) {
      others <- setdiff(seq_along(idx), k)
      both <- sweep(answered[others, , drop = FALSE], 2, answered[k, ], `|`)
      differ <- sweep(sub[others, , drop = FALSE], 2, sub[k, ], `!=`) & both
      share <- rowSums(differ) / pmax(rowSums(both), 1)
      j <- which.min(share)
      if (share[j] > 0 && share[j] < cfg$near_dup_share) {
        res[[length(res) + 1]] <- new_flags(u[idx[k]], "", sprintf("%.1f%%", 100 * share[j]),
          sprintf("Near-duplicate: differs from survey %s on only %.1f%% of answered questions",
                  u[idx[others[j]]], 100 * share[j]), "near_duplicate", "near_duplicates")
      }
    }
  }
  bind_flags(res)
}

#' Compute survey durations
#'
#' Uses audit files when supplied (active time on questions), otherwise the
#' difference between the `start` and `end` columns.
#' @inheritParams run_checks
#' @return data.frame with uuid, enumerator, duration_min and source.
#' @export
compute_durations <- function(data, cfg, audit = NULL) {
  u <- data[[cfg$uuid_col]]
  u_clean <- sub("^uuid:", "", u)
  enum <- if (!is.null(cfg$enumerator_col)) data[[cfg$enumerator_col]] else NA
  se <- rep(NA_real_, nrow(data))
  if (!is.null(cfg$start_col) && !is.null(cfg$end_col)) {
    st <- parse_kobo_time(data[[cfg$start_col]])
    en <- parse_kobo_time(data[[cfg$end_col]])
    se <- as.numeric(difftime(en, st, units = "mins"))
  }
  out <- data.frame(uuid = u, enumerator = enum, start_end_min = round(se, 1),
                    stringsAsFactors = FALSE)
  out$audit_active_min <- NA_real_
  out$median_sec_per_question <- NA_real_
  if (!is.null(audit) && nrow(audit)) {
    m <- match(u_clean, sub("^uuid:", "", audit$uuid))
    out$audit_active_min <- round(audit$audit_active_min[m], 1)
    out$median_sec_per_question <- round(audit$median_sec_per_question[m], 1)
  }
  out$duration_min <- ifelse(!is.na(out$audit_active_min), out$audit_active_min, out$start_end_min)
  out$source <- ifelse(!is.na(out$audit_active_min), "audit", ifelse(!is.na(out$start_end_min), "start/end", NA))
  out
}

parse_kobo_time <- function(x) {
  x <- sub("\\.\\d+", "", as.character(x))
  tz <- regmatches(x, regexpr("[+-]\\d{2}:?\\d{2}$|Z$", x))
  base <- sub("([+-]\\d{2}:?\\d{2}|Z)$", "", x)
  t <- as.POSIXct(base, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  alt <- is.na(t) & !is.na(x)
  t[alt] <- as.POSIXct(base[alt], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  num <- is.na(t) & grepl("^\\d+(\\.\\d+)?$", x)
  if (any(num)) t[num] <- as.POSIXct(as.numeric(x[num]) * 86400, origin = "1899-12-30", tz = "UTC")
  off <- rep(0, length(x))
  has_off <- grepl("[+-]\\d{2}:?\\d{2}$", x)
  if (any(has_off)) {
    o <- regmatches(x[has_off], regexpr("[+-]\\d{2}:?\\d{2}$", x[has_off]))
    sgn <- ifelse(substr(o, 1, 1) == "-", -1, 1)
    hh <- as.numeric(substr(o, 2, 3)); mm <- as.numeric(substr(gsub(":", "", o), 4, 5))
    off[has_off] <- sgn * (hh * 3600 + mm * 60)
  }
  t - off
}

#' @rdname run_checks
#' @export
check_duration <- function(data, form, cfg, audit = NULL) {
  d <- compute_durations(data, cfg, audit)
  short <- which(!is.na(d$duration_min) & d$duration_min < cfg$min_duration)
  long <- which(!is.na(d$duration_min) & d$duration_min > cfg$max_duration)
  neg <- which(!is.na(d$start_end_min) & d$start_end_min < 0)
  bind_flags(list(
    new_flags(d$uuid[short], "duration", d$duration_min[short],
              sprintf("Survey too short: %s min (%s), minimum is %s min", d$duration_min[short],
                      d$source[short], cfg$min_duration), "duration_short", "duration"),
    new_flags(d$uuid[long], "duration", d$duration_min[long],
              sprintf("Survey too long: %s min (%s), maximum is %s min", d$duration_min[long],
                      d$source[long], cfg$max_duration), "duration_long", "duration"),
    new_flags(d$uuid[neg], "end", d$start_end_min[neg], "End time is before start time",
              "duration_negative", "duration")
  ))
}

#' @rdname run_checks
#' @export
check_outliers <- function(data, form, cfg) {
  q <- form$questions
  num_q <- q$name[q$base_type %in% c("integer", "decimal", "range") & q$name %in% names(data)]
  u <- data[[cfg$uuid_col]]
  res <- lapply(num_q, function(v) {
    x <- suppressWarnings(as.numeric(data[[v]]))
    ok <- !is.na(x) & !x %in% c(cfg$dk_codes, cfg$special_codes)
    if (sum(ok) < cfg$outlier_min_n) return(NULL)
    robust_z <- function(y) {
      med <- stats::median(y[ok]); s <- stats::mad(y[ok])
      if (is.na(s) || s == 0) s <- stats::sd(y[ok])
      if (is.na(s) || s == 0) return(rep(0, length(y)))
      z <- rep(0, length(y)); z[ok] <- (y[ok] - med) / s; z
    }
    # High values are judged on the log scale (right-skewed counts and amounts),
    # low values on the raw scale, so that small counts such as 1 are not flagged.
    z_raw <- robust_z(x)
    z <- if (all(x[ok] >= 0)) {
      yl <- x; yl[ok] <- log1p(x[ok]); z_log <- robust_z(yl)
      ifelse(z_raw > 0, z_log, z_raw)
    } else z_raw
    thr <- cfg$outlier_z
    f <- which(ok & abs(z) > thr)
    if (!length(f)) return(NULL)
    new_flags(u[f], v, data[[v]][f],
              sprintf("Possible outlier (%s; median %s)", ifelse(z[f] > 0, "high", "low"),
                      format(stats::median(x[ok]), digits = 4)),
              "outlier", "outliers")
  })
  bind_flags(res)
}

#' Identify "other, specify" text questions
#' @inheritParams run_checks
#' @return data.frame with `question` and `parent` columns.
#' @export
other_questions <- function(form) {
  q <- form$questions
  txt <- q[q$base_type == "text", , drop = FALSE]
  is_other <- grepl("(_other|_oth|_autre|_outro|_specify)$", txt$name) |
    grepl("selected\\s*\\([^)]*['\"]other['\"]\\s*\\)|=\\s*['\"]other['\"]",
          ifelse(is.na(txt$relevant), "", txt$relevant))
  txt <- txt[is_other, , drop = FALSE]
  parent <- vapply(txt$relevant, function(r) {
    if (is.na(r)) return(NA_character_)
    m <- regmatches(r, regexpr("\\$\\{[^}]+\\}", r))
    if (length(m)) gsub("[${}]", "", m) else NA_character_
  }, character(1), USE.NAMES = FALSE)
  # or_other questions produce <name>_other columns automatically
  oo <- q$name[q$or_other]
  data.frame(question = c(txt$name, paste0(oo, "_other")), parent = c(parent, oo),
             stringsAsFactors = FALSE)
}

#' @rdname run_checks
#' @export
check_others <- function(data, form, cfg) {
  oq <- other_questions(form)
  oq <- oq[oq$question %in% names(data), , drop = FALSE]
  u <- data[[cfg$uuid_col]]
  res <- lapply(seq_len(nrow(oq)), function(i) {
    v <- oq$question[i]
    f <- which(has_value(data[[v]]))
    if (!length(f)) return(NULL)
    par <- oq$parent[i]
    new_flags(u[f], v, data[[v]][f],
              if (is.na(par)) "'Other' response to review and recode"
              else sprintf("'Other' response to review and recode (parent question: %s)", par),
              "other_response", "others")
  })
  bind_flags(res)
}

#' @rdname run_checks
#' @export
check_skip_logic <- function(data, form, cfg) {
  q <- form$questions
  q <- q[!is.na(q$effective_relevant) & q$name %in% names(data) &
         !q$base_type %in% c("note", "calculate") & !q$in_repeat, , drop = FALSE]
  u <- data[[cfg$uuid_col]]
  rel_cache <- list()
  res <- list(); err <- list()
  for (i in seq_len(nrow(q))) {
    v <- q$name[i]; ex <- q$effective_relevant[i]
    rel <- rel_cache[[ex]]
    if (is.null(rel)) {
      rel <- tryCatch(.as_bool(eval_xpath(ex, data)), error = function(e) e)
      rel_cache[[ex]] <- rel
    }
    if (inherits(rel, "error")) {
      err[[length(err) + 1]] <- data.frame(question = v, check = "skip logic", expression = ex,
                                           reason = conditionMessage(rel), stringsAsFactors = FALSE)
      next
    }
    f <- which(!rel & has_value(data[[v]]))
    if (length(f)) res[[length(res) + 1]] <- new_flags(u[f], v, data[[v]][f],
      "Answered although the skip logic says the question should not have been asked",
      "skip_logic_answered", "skip_logic")
    if ("required" %in% cfg$checks && q$is_required[i]) {
      f2 <- which(rel & !has_value(data[[v]]))
      if (length(f2)) res[[length(res) + 1]] <- new_flags(u[f2], v, "",
        "Required question is empty although its skip logic applies", "required_missing", "required")
    }
  }
  # required questions without any relevant
  if ("required" %in% cfg$checks) {
    q2 <- form$questions
    q2 <- q2[is.na(q2$effective_relevant) & q2$is_required & q2$name %in% names(data) &
             !q2$base_type %in% c("note", "calculate") & !q2$in_repeat, , drop = FALSE]
    for (v in q2$name) {
      f <- which(!has_value(data[[v]]))
      if (length(f)) res[[length(res) + 1]] <- new_flags(u[f], v, "",
        "Required question is empty", "required_missing", "required")
    }
  }
  structure(bind_flags(res), errors = if (length(err)) do.call(rbind, err) else NULL)
}

#' @rdname run_checks
#' @export
check_constraints <- function(data, form, cfg) {
  q <- form$questions
  q <- q[!is.na(q$constraint) & q$name %in% names(data) & !q$in_repeat, , drop = FALSE]
  u <- data[[cfg$uuid_col]]
  res <- list(); err <- list()
  for (i in seq_len(nrow(q))) {
    v <- q$name[i]
    ok <- tryCatch(.as_bool(eval_xpath(q$constraint[i], data, self = data[[v]])),
                   error = function(e) e)
    if (inherits(ok, "error")) {
      err[[length(err) + 1]] <- data.frame(question = v, check = "constraint",
        expression = q$constraint[i], reason = conditionMessage(ok), stringsAsFactors = FALSE)
      next
    }
    f <- which(!ok & has_value(data[[v]]))
    if (length(f)) res[[length(res) + 1]] <- new_flags(u[f], v, data[[v]][f],
      sprintf("Value breaks the form constraint: %s", q$constraint[i]), "constraint", "constraints")
  }
  structure(bind_flags(res), errors = if (length(err)) do.call(rbind, err) else NULL)
}

#' @rdname run_checks
#' @export
check_choices <- function(data, form, cfg) {
  q <- form$questions
  q <- q[q$base_type %in% c("select_one", "select_multiple") & q$name %in% names(data) &
         !grepl("^select_(one|multiple)_from_file", q$type), , drop = FALSE]
  ch <- form$choices
  u <- data[[cfg$uuid_col]]
  res <- lapply(seq_len(nrow(q)), function(i) {
    v <- q$name[i]
    allowed <- ch$name[ch$list_name == q$list_name[i]]
    if (!length(allowed)) return(NULL)
    if (q$or_other[i]) allowed <- c(allowed, "other")
    x <- data[[v]]
    bad <- if (q$base_type[i] == "select_one") {
      has_value(x) & !x %in% allowed
    } else {
      vapply(strsplit(ifelse(is.na(x), "", x), "\\s+"),
             function(t) any(nzchar(t) & !t %in% allowed), logical(1))
    }
    f <- which(bad)
    if (!length(f)) return(NULL)
    new_flags(u[f], v, x[f], sprintf("Value not in the choice list '%s'", q$list_name[i]),
              "choice_not_in_list", "choices")
  })
  bind_flags(res)
}

#' @rdname run_checks
#' @export
check_select_multiple <- function(data, form, cfg) {
  q <- form$questions
  q <- q[q$base_type == "select_multiple" & q$name %in% names(data), , drop = FALSE]
  u <- data[[cfg$uuid_col]]
  res <- list()
  for (v in q$name) {
    x <- data[[v]]
    tok <- strsplit(ifelse(is.na(x), "", x), "\\s+")
    # exclusive options
    excl <- vapply(tok, function(t) {
      t <- t[nzchar(t)]
      length(t) > 1 && any(t %in% cfg$exclusive_options)
    }, logical(1))
    f <- which(excl)
    if (length(f)) res[[length(res) + 1]] <- new_flags(u[f], v, x[f],
      "Exclusive option (e.g. dk / pnta / none) selected together with other options",
      "sm_exclusive", "select_multiple")
    # parent / child consistency
    kids <- grep(paste0("^", gsub("([.])", "\\\\\\1", v), "/"), names(data), value = TRUE)
    if (length(kids)) {
      opts <- sub("^.*/", "", kids)
      mism <- logical(length(x))
      for (k in seq_along(kids)) {
        kv <- data[[kids[k]]]
        sel <- vapply(tok, function(t) opts[k] %in% t, logical(1))
        child_on <- kv %in% c("1", "TRUE", "true", "1.0")
        answered <- has_value(x)
        mism <- mism | (answered & sel != child_on) | (!answered & child_on)
      }
      f2 <- which(mism)
      if (length(f2)) res[[length(res) + 1]] <- new_flags(u[f2], v, x[f2],
        "select_multiple text and its 0/1 option columns do not match",
        "sm_parent_child", "select_multiple")
    }
  }
  bind_flags(res)
}

#' @rdname run_checks
#' @export
check_logical <- function(data, form, cfg, logical_checks = NULL) {
  lc <- if (!is.null(logical_checks)) logical_checks else form$logical_checks
  if (is.null(lc) || !nrow(lc)) return(new_flags())
  if (!"check_id" %in% names(lc)) lc$check_id <- paste0("logic_", seq_len(nrow(lc)))
  if (!"description" %in% names(lc)) lc$description <- lc$expression
  if (!"questions" %in% names(lc)) lc$questions <- NA
  u <- data[[cfg$uuid_col]]
  res <- list(); err <- list()
  for (i in seq_len(nrow(lc))) {
    fl <- tryCatch(.as_bool(eval_xpath(lc$expression[i], data)), error = function(e) e)
    if (inherits(fl, "error")) {
      err[[length(err) + 1]] <- data.frame(question = lc$check_id[i], check = "logical check",
        expression = lc$expression[i], reason = conditionMessage(fl), stringsAsFactors = FALSE)
      next
    }
    f <- which(fl)
    if (!length(f)) next
    qs <- if (is.na(lc$questions[i])) {
      m <- regmatches(lc$expression[i], gregexpr("\\$\\{[^}]+\\}", lc$expression[i]))[[1]]
      unique(gsub("[${}]", "", m))
    } else trimws(strsplit(lc$questions[i], ",")[[1]])
    qs <- qs[qs %in% names(data)]
    if (!length(qs)) qs <- ""
    for (qq in qs) {
      res[[length(res) + 1]] <- new_flags(u[f], qq, if (nzchar(qq)) data[[qq]][f] else "",
        lc$description[i], lc$check_id[i], "logical")
    }
  }
  structure(bind_flags(res), errors = if (length(err)) do.call(rbind, err) else NULL)
}

#' Run all checks
#'
#' @param data Kobo data from [read_kobo_data()].
#' @param form XLSForm from [read_xlsform()].
#' @param cfg Settings from [kc_config()].
#' @param audit Optional audit summary from [read_audits()].
#' @param logical_checks Optional data.frame of custom checks (`check_id`,
#'   `description`, `expression`, `questions`). Defaults to the form's
#'   `logical_checks` sheet.
#' @return A list of class `kc_result` with `flags`, `durations`, `pii`,
#'   `not_checked` (expressions that could not be evaluated), `cfg` and
#'   `data`.
#' @export
run_checks <- function(data, form, cfg = kc_config(), audit = NULL, logical_checks = NULL) {
  cfg <- resolve_columns(data, cfg)
  ck <- cfg$checks
  steps <- list(
    duplicates = function() check_duplicates(data, form, cfg),
    near_duplicates = function() check_near_duplicates(data, form, cfg),
    duration = function() check_duration(data, form, cfg, audit),
    outliers = function() check_outliers(data, form, cfg),
    others = function() check_others(data, form, cfg),
    skip_logic = function() check_skip_logic(data, form, cfg),
    constraints = function() check_constraints(data, form, cfg),
    choices = function() check_choices(data, form, cfg),
    select_multiple = function() check_select_multiple(data, form, cfg),
    logical = function() check_logical(data, form, cfg, logical_checks),
    special_values = function() check_special_values(data, form, cfg),
    missing_share = function() check_missing_share(data, form, cfg)
  )
  if ("required" %in% ck && !"skip_logic" %in% ck) {
    steps$skip_logic <- function() {
      r <- check_skip_logic(data, form, cfg)
      out <- r[r$check_type == "required", , drop = FALSE]
      attr(out, "errors") <- attr(r, "errors"); out
    }
    ck <- c(ck, "skip_logic")
  }
  results <- list(); errors <- list()
  for (nm in intersect(names(steps), ck)) {
    r <- tryCatch(steps[[nm]](), error = function(e) {
      errors[[length(errors) + 1]] <<- data.frame(question = "", check = nm, expression = "",
        reason = paste("Check failed:", conditionMessage(e)), stringsAsFactors = FALSE)
      NULL
    })
    if (!is.null(attr(r, "errors"))) errors[[length(errors) + 1]] <- attr(r, "errors")
    results[[nm]] <- r
  }
  flags <- bind_flags(results)
  structure(list(
    flags = flags,
    durations = if ("duration" %in% ck) compute_durations(data, cfg, audit) else NULL,
    pii = if ("pii" %in% ck) check_pii(data, form, cfg) else NULL,
    not_checked = if (length(errors)) do.call(rbind, errors) else
      data.frame(question = character(), check = character(), expression = character(),
                 reason = character(), stringsAsFactors = FALSE),
    cfg = cfg, data = data, form = form
  ), class = "kc_result")
}

#' @export
print.kc_result <- function(x, ...) {
  cat("kobocheck result:", nrow(x$data), "surveys,", nrow(x$flags), "flags\n")
  if (nrow(x$flags)) print(table(x$flags$check_type))
  if (nrow(x$not_checked)) cat(nrow(x$not_checked), "expressions could not be evaluated (see $not_checked)\n")
  invisible(x)
}
