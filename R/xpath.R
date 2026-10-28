# Translation and vectorised evaluation of XLSForm (XPath) expressions.
#
# Approach: protect string literals, rewrite XLSForm syntax into R syntax,
# parse it with R's parser (so operator precedence is correct), then walk the
# syntax tree and swap comparison, boolean and arithmetic operators for
# functions that follow XPath semantics on Kobo's text values (empty string
# for missing, numeric comparison when both sides are numbers).

#' Translate an XLSForm expression into an R expression
#'
#' @param expr XLSForm expression, e.g. `selected(${q1}, 'other') and ${age} >= 18`.
#' @return An R language object. Errors if the expression cannot be translated.
#' @export
translate_xpath <- function(expr) {
  if (is.na(expr) || !nzchar(trimws(expr))) return(quote(TRUE))
  s <- expr
  # 1. protect string literals
  lits <- character()
  m <- gregexpr("'[^']*'|\"[^\"]*\"", s)
  found <- regmatches(s, m)[[1]]
  if (length(found)) {
    lits <- substr(found, 2, nchar(found) - 1)
    for (k in seq_along(found)) {
      s <- sub(found[k], paste0(" .L", k, ". "), s, fixed = TRUE)
    }
  }
  # 2. references and self
  s <- gsub("\\$\\{\\s*([A-Za-z_][A-Za-z0-9_.-]*)\\s*\\}", " .v(\"\\1\") ", s, perl = TRUE)
  s <- gsub("(?<![A-Za-z0-9_.\"])\\.(?![A-Za-z0-9_.])", " .self ", s, perl = TRUE)
  # 3. hyphenated / namespaced functions
  fun_map <- c("count-selected" = ".x_count_selected", "string-length" = ".x_strlen",
               "selected-at" = ".x_selected_at", "jr:choice-name" = ".x_unsupported",
               "boolean-from-string" = ".x_bool_from_string", "format-date" = ".x_format_date",
               "decimal-date-time" = ".x_unsupported", "normalize-space" = ".x_trim",
               "starts-with" = ".x_starts_with", "ends-with" = ".x_ends_with")
  for (f in names(fun_map)) s <- gsub(paste0(f, "\\s*\\("), paste0(fun_map[[f]], "("), s)
  simple <- c("selected", "if", "concat", "contains", "regex", "number", "int", "round",
              "today", "now", "not", "true", "false", "coalesce", "string", "substr",
              "count", "sum", "min", "max", "date", "pow", "abs", "uuid", "position")
  for (f in simple) s <- gsub(paste0("(?<![A-Za-z0-9_.])", f, "\\s*\\("),
                              paste0(".x_", f, "("), s, perl = TRUE)
  # 4. operators
  s <- gsub("(?<![A-Za-z0-9_.])and(?![A-Za-z0-9_.])", " & ", s, perl = TRUE)
  s <- gsub("(?<![A-Za-z0-9_.])or(?![A-Za-z0-9_.])", " | ", s, perl = TRUE)
  s <- gsub("(?<![A-Za-z0-9_.])div(?![A-Za-z0-9_.])", " / ", s, perl = TRUE)
  s <- gsub("(?<![A-Za-z0-9_.])mod(?![A-Za-z0-9_.])", " %% ", s, perl = TRUE)
  s <- gsub("(?<![!<>=])=(?!=)", " == ", s, perl = TRUE)
  # 5. restore literals
  for (k in rev(seq_along(lits))) {
    s <- gsub(paste0(".L", k, "."), deparse(lits[k]), s, fixed = TRUE)
  }
  ex <- parse(text = s, keep.source = FALSE)
  if (length(ex) != 1) stop("Expression did not parse to a single statement")
  rewrite_ops(ex[[1]])
}

rewrite_ops <- function(e) {
  if (!is.call(e)) return(e)
  op_map <- c("==" = ".x_eq", "!=" = ".x_ne", ">" = ".x_gt", "<" = ".x_lt",
              ">=" = ".x_ge", "<=" = ".x_le", "&" = ".x_and", "|" = ".x_or",
              "!" = ".x_not", "+" = ".x_add", "-" = ".x_sub", "*" = ".x_mul",
              "/" = ".x_div", "%%" = ".x_mod")
  fn <- e[[1]]
  args <- as.list(e)[-1]
  args <- lapply(args, rewrite_ops)
  if (is.name(fn)) {
    nm <- as.character(fn)
    if (nm == "(") return(as.call(c(fn, args)))
    if (nm %in% names(op_map)) {
      if (nm == "-" && length(args) == 1) return(as.call(c(as.name(".x_neg"), args)))
      return(as.call(c(as.name(op_map[[nm]]), args)))
    }
  }
  as.call(c(fn, args))
}

# ---- XPath-like helper functions --------------------------------------------

.as_str <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
.as_num <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  suppressWarnings(as.numeric(as.character(x)))
}
.as_bool <- function(x) {
  if (is.logical(x)) { x[is.na(x)] <- FALSE; return(x) }
  if (is.numeric(x)) { r <- !is.na(x) & x != 0; return(r) }
  s <- .as_str(x)
  nzchar(s) & s != "false" & s != "0"
}

.cmp <- function(a, b, op) {
  if (is.logical(a) || is.logical(b)) {
    r <- op(.as_num(.as_bool(a)), .as_num(.as_bool(b)))
    r[is.na(r)] <- FALSE
    return(r)
  }
  an <- .as_num(a); bn <- .as_num(b)
  as_ <- .as_str(a); bs <- .as_str(b)
  n <- max(length(an), length(bn))
  an <- rep_len(an, n); bn <- rep_len(bn, n); as_ <- rep_len(as_, n); bs <- rep_len(bs, n)
  both_num <- !is.na(an) & !is.na(bn)
  r <- logical(n)
  r[both_num] <- op(an[both_num], bn[both_num])
  r[!both_num] <- op(as_[!both_num], bs[!both_num])
  r[is.na(r)] <- FALSE
  r
}
.ord <- function(a, b, op) {
  an <- .as_num(a); bn <- .as_num(b)
  r <- op(an, bn); r[is.na(r)] <- FALSE; r
}

.x_eq <- function(a, b) .cmp(a, b, `==`)
.x_ne <- function(a, b) !.cmp(a, b, `==`)
.x_gt <- function(a, b) .ord(a, b, `>`)
.x_lt <- function(a, b) .ord(a, b, `<`)
.x_ge <- function(a, b) .ord(a, b, `>=`)
.x_le <- function(a, b) .ord(a, b, `<=`)
.x_and <- function(a, b) .as_bool(a) & .as_bool(b)
.x_or <- function(a, b) .as_bool(a) | .as_bool(b)
.x_not <- function(a) !.as_bool(a)
.x_add <- function(a, b) .as_num(a) + .as_num(b)
.x_sub <- function(a, b) .as_num(a) - .as_num(b)
.x_mul <- function(a, b) .as_num(a) * .as_num(b)
.x_div <- function(a, b) .as_num(a) / .as_num(b)
.x_mod <- function(a, b) .as_num(a) %% .as_num(b)
.x_neg <- function(a) -.as_num(a)

.split_sm <- function(x) strsplit(.as_str(x), "\\s+")
.x_selected <- function(x, opt) {
  opt <- .as_str(opt)
  sp <- .split_sm(x)
  opt <- rep_len(opt, length(sp))
  vapply(seq_along(sp), function(i) opt[i] %in% sp[[i]], logical(1))
}
.x_count_selected <- function(x) vapply(.split_sm(x), function(v) sum(nzchar(v)), numeric(1))
.x_selected_at <- function(x, i) {
  sp <- .split_sm(x); i <- rep_len(.as_num(i), length(sp))
  vapply(seq_along(sp), function(k) { v <- sp[[k]][nzchar(sp[[k]])]
    if (!is.na(i[k]) && i[k] + 1 <= length(v)) v[i[k] + 1] else "" }, character(1))
}
.x_strlen <- function(x) nchar(.as_str(x))
.x_if <- function(cond, a, b) {
  cond <- .as_bool(cond)
  n <- length(cond)
  a <- rep_len(a, n); b <- rep_len(b, n)
  out <- b; out[cond] <- a[cond]; out
}
.x_concat <- function(...) do.call(paste0, lapply(list(...), .as_str))
.x_contains <- function(x, y) grepl(.as_str(y)[1], .as_str(x), fixed = TRUE)
.x_starts_with <- function(x, y) startsWith(.as_str(x), .as_str(y)[1])
.x_ends_with <- function(x, y) endsWith(.as_str(x), .as_str(y)[1])
.x_regex <- function(x, pattern) grepl(.as_str(pattern)[1], .as_str(x), perl = TRUE)
.x_number <- function(x) .as_num(x)
.x_int <- function(x) trunc(.as_num(x))
.x_round <- function(x, d = 0) round(.as_num(x), .as_num(d))
.x_abs <- function(x) abs(.as_num(x))
.x_pow <- function(x, y) .as_num(x)^.as_num(y)
.x_today <- function() as.numeric(Sys.Date())
.x_now <- function() as.numeric(Sys.time()) / 86400
.x_date <- function(x) {
  s <- .as_str(x)
  out <- .as_num(s)
  isd <- grepl("^\\d{4}-\\d{2}-\\d{2}", s)
  out[isd] <- as.numeric(as.Date(substr(s[isd], 1, 10)))
  out
}
.x_true <- function() TRUE
.x_false <- function() FALSE
.x_string <- function(x) .as_str(x)
.x_trim <- function(x) trimws(gsub("\\s+", " ", .as_str(x)))
.x_substr <- function(x, s, e = NULL) {
  x <- .as_str(x); s <- .as_num(s) + 1
  e <- if (is.null(e)) nchar(x) else .as_num(e)
  substr(x, s, e)
}
.x_coalesce <- function(a, b) { a2 <- .as_str(a); b2 <- .as_str(b)
  n <- max(length(a2), length(b2)); a2 <- rep_len(a2, n); b2 <- rep_len(b2, n)
  ifelse(nzchar(a2), a2, b2) }
.x_bool_from_string <- function(x) .as_str(x) %in% c("true", "1")
.x_unsupported <- function(...) stop("function not supported")
.x_format_date <- function(...) stop("format-date() not supported")
.x_count <- function(...) stop("count() over repeats not supported")
.x_sum <- function(...) stop("sum() over repeats not supported")
.x_min <- function(...) stop("min() not supported")
.x_max <- function(...) stop("max() not supported")
.x_uuid <- function(...) stop("uuid() not supported")
.x_position <- function(...) stop("position() not supported")

# Convert date-like Kobo values to day numbers so that date comparisons work.
.prep_value <- function(x) {
  x <- .as_str(x)
  is_date <- grepl("^\\d{4}-\\d{2}-\\d{2}", x)
  if (any(is_date) && all(is_date | !nzchar(x))) {
    d <- rep(NA_real_, length(x))
    d[is_date] <- as.numeric(as.Date(substr(x[is_date], 1, 10)))
    out <- as.character(d); out[!is_date] <- ""
    return(out)
  }
  # Excel serial dates read as text (e.g. "45567") stay numeric; fine.
  x
}

#' Evaluate an XLSForm expression over a dataset
#'
#' @param expr XLSForm expression (character) or a translated R expression.
#' @param data data.frame of the Kobo export (normalised names).
#' @param self Optional vector bound to `.` (the current question's value).
#' @return Logical vector (one value per row); for non-logical results the
#'   raw vector is returned. Errors propagate so callers can record them.
#' @export
eval_xpath <- function(expr, data, self = NULL) {
  e <- if (is.character(expr)) translate_xpath(expr) else expr
  n <- nrow(data)
  env <- new.env(parent = asNamespace("kobocheck"))
  cache <- new.env()
  env$.v <- function(name) {
    if (!is.null(cache[[name]])) return(cache[[name]])
    if (!name %in% names(data)) stop(sprintf("question '%s' not found in the data", name))
    val <- .prep_value(data[[name]])
    assign(name, val, envir = cache)
    val
  }
  env$.self <- if (is.null(self)) rep("", n) else .prep_value(self)
  out <- eval(e, env)
  if (length(out) == 1 && n > 1) out <- rep(out, n)
  out
}
