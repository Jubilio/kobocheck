#' @rdname run_checks
#' @export
check_special_values <- function(data, form, cfg) {
  q <- form$questions
  num_q <- q$name[q$base_type %in% c("integer", "decimal", "range") & q$name %in% names(data)]
  u <- data[[cfg$uuid_col]]
  codes <- as.character(cfg$special_codes)
  res <- lapply(num_q, function(v) {
    x <- data[[v]]
    xn <- suppressWarnings(as.numeric(x))
    f <- which(!is.na(xn) & as.character(xn) %in% codes)
    if (!length(f)) return(NULL)
    new_flags(u[f], v, x[f],
              "Possible 'don't know' or missing code (e.g. -999, 99, 88): confirm, or set to blank",
              "special_value", "special_values")
  })
  bind_flags(res)
}

#' Screen for personally identifiable information
#'
#' Looks for PII in column names (English, French and Portuguese terms) and
#' in values (e-mail addresses, international phone numbers and Mozambican
#' mobile numbers written without a prefix, e.g. 84 123 4567).
#'
#' @inheritParams run_checks
#' @return data.frame with one row per column: `question`, `reason`,
#'   `n_values`, `example` (masked).
#' @export
check_pii <- function(data, form = NULL, cfg = kc_config()) {
  name_terms <- c("name", "nome", "nom", "phone", "telefone", "telephone", "celular", "contact",
                  "contacto", "contato", "email", "e_mail", "gps", "latitude", "longitude",
                  "altitude", "precision", "geopoint", "address", "endereco", "morada", "bairro_nome",
                  "id_number", "bi_numero", "numero_bi", "nuit", "passport", "passaporte", "dob",
                  "birth", "nascimento", cfg$pii_terms)
  skip <- c(meta_cols, "_geolocation")
  cols <- setdiff(names(data), skip)
  q <- if (!is.null(form)) form$questions else NULL
  geo <- if (!is.null(q)) q$name[q$base_type %in% c("geopoint", "geotrace", "geoshape", "image",
                                                   "audio", "video", "file")] else character()
  pats <- list(
    email = "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}",
    phone_international = "^\\s*(\\+|00)\\d[\\d\\s()-]{7,}$",
    phone_mozambique = "^\\s*(\\+?258)?\\s*8[2-7][\\s-]?\\d{3}[\\s-]?\\d{4}\\s*$")
  mask <- function(x) if (nchar(x) <= 4) "****" else paste0(substr(x, 1, 2), strrep("*", nchar(x) - 4), substr(x, nchar(x) - 1, nchar(x)))
  rows <- list()
  for (cl in cols) {
    base <- sub("^.*/", "", tolower(cl))
    base_q <- sub("/.*$", "", cl)
    reasons <- character()
    hit_terms <- name_terms[vapply(name_terms, function(t) grepl(paste0("(^|_)", t, "($|_)"), base), logical(1))]
    if (length(hit_terms)) reasons <- c(reasons, paste0("column name contains '", hit_terms[1], "'"))
    if (base_q %in% geo) reasons <- c(reasons, "GPS or media question")
    x <- data[[cl]]; x <- x[has_value(x)]
    n <- 0; ex <- ""
    if (length(x) && !grepl("/", cl)) {
      for (p in names(pats)) {
        m <- grepl(pats[[p]], x, perl = TRUE)
        if (sum(m) > 0 && (sum(m) >= 2 || sum(m) / length(x) > 0.05)) {
          reasons <- c(reasons, sprintf("%d values look like %s", sum(m), gsub("_", " ", p)))
          n <- max(n, sum(m)); if (!nzchar(ex)) ex <- mask(x[m][1])
        }
      }
    }
    if (length(reasons)) rows[[length(rows) + 1]] <- data.frame(
      question = cl, reason = paste(reasons, collapse = "; "), n_values = n, example = ex,
      stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(data.frame(question = character(), reason = character(),
                                       n_values = numeric(), example = character()))
  do.call(rbind, rows)
}

#' @rdname run_checks
#' @export
check_missing_share <- function(data, form, cfg) {
  q <- form$questions
  q <- q[q$name %in% names(data) & !q$base_type %in% c("note", "calculate", "acknowledge", "start",
          "end", "today", "deviceid", "audit") & !q$in_repeat & !q$name %in% meta_cols, , drop = FALSE]
  if (nrow(q) < 10) return(new_flags())
  n <- nrow(data)
  applicable <- matrix(TRUE, n, nrow(q))
  cache <- list()
  for (j in seq_len(nrow(q))) {
    ex <- q$effective_relevant[j]
    if (is.na(ex)) next
    r <- cache[[ex]]
    if (is.null(r)) { r <- tryCatch(.as_bool(eval_xpath(ex, data)), error = function(e) rep(TRUE, n)); cache[[ex]] <- r }
    applicable[, j] <- r
  }
  answered <- vapply(q$name, function(v) has_value(data[[v]]), logical(n))
  if (n == 1) answered <- matrix(answered, nrow = 1)
  n_app <- rowSums(applicable)
  share <- ifelse(n_app > 0, rowSums(applicable & !answered) / n_app, NA)
  ok <- !is.na(share) & n_app >= 10
  if (sum(ok) < cfg$outlier_min_n) return(new_flags())
  med <- stats::median(share[ok]); s <- stats::mad(share[ok])
  if (is.na(s) || s == 0) s <- max(stats::sd(share[ok]), 0.02, na.rm = TRUE)
  z <- (share - med) / s
  f <- which(ok & z > cfg$outlier_z & share > 0.1)
  u <- data[[cfg$uuid_col]]
  new_flags(u[f], "", sprintf("%.0f%%", 100 * share[f]),
            sprintf("%.0f%% of applicable questions left empty (median %.0f%%)", 100 * share[f], 100 * med),
            "missing_share", "missing_share")
}
