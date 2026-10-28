# Builds the example XLSForm, Kobo export and audit zip in inst/extdata.
# Every planted error is listed in inst/extdata/example_answer_key.csv so the
# tests can confirm the checks find them.
set.seed(2026)
out <- "inst/extdata"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

survey <- data.frame(
  type = c("start", "end", "today", "audit", "select_one enumerators", "begin_group",
           "select_one districts", "select_one yn", "begin_group", "integer", "integer",
           "integer", "select_one yn", "integer", "select_one shelter or_other",
           "select_multiple needs", "text", "select_one income", "decimal", "integer",
           "integer", "note", "end_group", "end_group"),
  name = c("start", "end", "today", "audit", "enumerator_id", "hh_info", "district", "consent",
           "main", "hoh_age", "hh_size", "children_u5", "displaced", "displacement_months",
           "shelter_type", "priority_needs", "priority_needs_other", "income_main",
           "income_amount", "water_litres", "n_meals", "thanks", "", ""),
  `label::English (en)` = c(NA, NA, NA, NA, "Enumerator", "Household information", "District",
           "Does the respondent consent?", "Main module", "Age of head of household",
           "Household size", "Children under 5", "Is the household displaced?",
           "Months since displacement", "Shelter type", "Priority needs",
           "Other priority need, specify", "Main income source", "Income last month (MZN)",
           "Litres of water used yesterday", "Meals eaten yesterday", "Thank you", NA, NA),
  `label::Portuguese (pt)` = c(NA, NA, NA, NA, "Inquiridor", "Informação do agregado", "Distrito",
           "O respondente consente?", "Módulo principal", "Idade do chefe do agregado",
           "Tamanho do agregado", "Crianças menores de 5", "O agregado está deslocado?",
           "Meses desde o deslocamento", "Tipo de abrigo", "Necessidades prioritárias",
           "Outra necessidade, especifique", "Principal fonte de rendimento",
           "Rendimento do último mês (MZN)", "Litros de água usados ontem",
           "Refeições ontem", "Obrigado", NA, NA),
  required = c(NA, NA, NA, NA, "yes", NA, "yes", "yes", NA, "yes", "yes", NA, "yes", NA, NA,
               "yes", NA, NA, NA, NA, NA, NA, NA, NA),
  relevant = c(NA, NA, NA, NA, NA, NA, NA, NA, "${consent} = 'yes'", NA, NA, NA, NA,
               "${displaced} = 'yes'", NA, NA, "selected(${priority_needs}, 'other')", NA,
               "${income_main} != 'none'", NA, NA, NA, NA, NA),
  constraint = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, ". >= 15 and . <= 110", ". >= 1 and . <= 30",
                 ". <= ${hh_size}", NA, ". >= 0", NA,
                 "not(selected(., 'dk') and count-selected(.) > 1)", NA, NA, ". >= 0", NA,
                 ". <= 6", NA, NA, NA),
  check.names = FALSE, stringsAsFactors = FALSE)
survey$name[survey$name == ""] <- NA
newrow <- survey[1, ]; newrow[] <- NA
newrow$type <- "text"; newrow$name <- "resp_phone"
newrow$`label::English (en)` <- "Respondent phone number (for follow-up)"
newrow$`label::Portuguese (pt)` <- "Telefone do respondente (para seguimento)"
survey <- rbind(survey[1:8, ], newrow, survey[9:nrow(survey), ])

choices <- data.frame(
  list_name = c(rep("enumerators", 6), rep("districts", 4), "yn", "yn",
                rep("shelter", 4), rep("needs", 8), rep("income", 5)),
  name = c(sprintf("enum_%02d", 1:6), "pemba", "montepuez", "mueda", "macomia", "yes", "no",
           "house", "tent", "makeshift", "collective_centre",
           "food", "water", "shelter", "health", "education", "none", "dk", "other",
           "agriculture", "casual_labour", "trade", "aid", "none"),
  stringsAsFactors = FALSE)
choices$`label::English (en)` <- choices$name

logical_checks <- data.frame(
  check_id = c("lc_single_with_child", "lc_no_needs_low_meals"),
  description = c("Single-person household reports a child under 5",
                  "No priority needs reported but at most one meal yesterday"),
  expression = c("${hh_size} = 1 and ${children_u5} >= 1",
                 "selected(${priority_needs}, 'none') and ${n_meals} <= 1"),
  questions = c("hh_size, children_u5", "priority_needs, n_meals"),
  stringsAsFactors = FALSE)

wb <- openxlsx::createWorkbook()
for (s in c("survey", "choices", "logical_checks")) {
  openxlsx::addWorksheet(wb, s)
  openxlsx::writeData(wb, s, get(s))
}
openxlsx::saveWorkbook(wb, file.path(out, "example_form.xlsx"), overwrite = TRUE)

# ---- data -------------------------------------------------------------------
n <- 240
uuid <- vapply(seq_len(n), function(i) paste(sample(c(0:9, letters[1:6]), 32, TRUE), collapse = ""), "")
uuid <- sub("^(.{8})(.{4})(.{4})(.{4})(.{12})$", "\\1-\\2-\\3-\\4-\\5", uuid)
enum <- sprintf("enum_%02d", sample(1:6, n, TRUE))
consent <- ifelse(runif(n) < 0.95, "yes", "no")
dur <- round(rnorm(n, 38, 8), 1)
fast <- enum == "enum_06" & runif(n) < 0.7
dur[fast] <- round(runif(sum(fast), 6, 11), 1)
start <- as.POSIXct("2026-10-05 07:00:00", tz = "UTC") + sort(sample(0:(10 * 86400), n))
end <- start + dur * 60
fmt <- function(t) paste0(format(t, "%Y-%m-%dT%H:%M:%S"), ".000+02:00")

hh_size <- pmax(1, round(rlnorm(n, log(5), 0.4)))
hh_size <- pmin(hh_size, 25)
children <- pmin(hh_size - 1, rpois(n, 1))
children[hh_size == 1] <- 0
displaced <- ifelse(runif(n) < 0.6, "yes", "no")
disp_months <- ifelse(displaced == "yes", sample(1:60, n, TRUE), NA)
shelter <- sample(c("house", "tent", "makeshift", "collective_centre"), n, TRUE)
other_rows <- sample(which(consent == "yes"), 8)
shelter[other_rows] <- "other"
shelter_other <- ifelse(shelter == "other",
  sample(c("casa de familiar", "barraca de caniço", "escola", "igreja"), n, TRUE), NA)
needs_opts <- c("food", "water", "shelter", "health", "education")
needs <- vapply(seq_len(n), function(i) paste(sort(sample(needs_opts, sample(1:3, 1))), collapse = " "), "")
needs_other_rows <- sample(setdiff(which(consent == "yes"), other_rows), 5)
needs[needs_other_rows] <- paste(needs[needs_other_rows], "other")
needs_other <- ifelse(grepl("other", needs), sample(c("documentos", "sementes", "redes mosquiteiras"), n, TRUE), NA)
income <- sample(c("agriculture", "casual_labour", "trade", "aid", "none"), n, TRUE)
income_amt <- ifelse(income == "none", NA, round(rlnorm(n, log(3000), 0.6)))
water <- round(rlnorm(n, log(80), 0.4))
meals <- sample(1:3, n, TRUE, prob = c(0.2, 0.5, 0.3))
age <- round(runif(n, 18, 75))

df <- data.frame(start = fmt(start), end = fmt(end), today = format(start, "%Y-%m-%d"),
                 enumerator_id = enum, district = sample(c("pemba", "montepuez", "mueda", "macomia"), n, TRUE),
                 consent = consent,
                 resp_phone = ifelse(consent == "yes", sprintf("8%d %03d %04d", sample(2:7, n, TRUE),
                   sample(0:999, n, TRUE), sample(0:9999, n, TRUE)), NA),
                 hoh_age = age, hh_size = hh_size, children_u5 = children,
                 displaced = displaced, displacement_months = disp_months, shelter_type = shelter,
                 shelter_type_other = shelter_other, priority_needs = needs,
                 priority_needs_other = needs_other, income_main = income, income_amount = income_amt,
                 water_litres = water, n_meals = meals, stringsAsFactors = FALSE)
main_cols <- c("hoh_age", "hh_size", "children_u5", "displaced", "displacement_months", "shelter_type",
               "shelter_type_other", "priority_needs", "priority_needs_other", "income_main",
               "income_amount", "water_litres", "n_meals")
df[df$consent == "no", main_cols] <- NA
df$resp_phone[df$consent == "no"] <- NA
df[] <- lapply(df, as.character)

key <- list()
plant <- function(row, question, check_id) key[[length(key) + 1]] <<- data.frame(row = row, question = question, check_id = check_id)
ok <- which(df$consent == "yes" & !seq_len(n) %in% c(other_rows, needs_other_rows, 10, 20, 30, n - 2, n - 1, n) & !fast)
pick <- function() { r <- ok[1]; ok <<- ok[-1]; r }

r <- pick(); df$displaced[r] <- "no"; df$displacement_months[r] <- "14"; plant(r, "displacement_months", "skip_logic_answered")
r <- pick(); df$displaced[r] <- "no"; df$displacement_months[r] <- "3";  plant(r, "displacement_months", "skip_logic_answered")
r <- pick(); df$hh_size[r] <- NA; plant(r, "hh_size", "required_missing")
r <- pick(); df$hoh_age[r] <- "12"; plant(r, "hoh_age", "constraint")
r <- pick(); df$n_meals[r] <- "9"; plant(r, "n_meals", "constraint")
r <- pick(); df$district[r] <- "nampula"; plant(r, "district", "choice_not_in_list")
r <- pick(); df$priority_needs[r] <- "food dk"; plant(r, "priority_needs", "sm_exclusive")
r <- pick(); df$water_litres[r] <- "5000"; plant(r, "water_litres", "outlier")
r <- pick(); df$income_main[r] <- "trade"; df$income_amount[r] <- "9000000"; plant(r, "income_amount", "outlier")
r <- pick(); df$hh_size[r] <- "1"; df$children_u5[r] <- "1"; plant(r, "hh_size", "lc_single_with_child")
r <- pick(); df$priority_needs[r] <- "none"; df$n_meals[r] <- "1"; plant(r, "priority_needs", "lc_no_needs_low_meals")
dk_rows <- c(pick(), pick(), pick()); df$water_litres[dk_rows] <- "-999"   # must NOT be flagged
r <- pick(); df$end[r] <- fmt(start[r] - 600); plant(r, "end", "duration_negative")
for (i in other_rows) plant(i, "shelter_type_other", "other_response")
for (i in needs_other_rows) plant(i, "priority_needs_other", "other_response")

# select_multiple 0/1 columns
sm_mismatch <- pick()
sm <- sapply(c(needs_opts, "none", "dk", "other"), function(o)
  ifelse(is.na(df$priority_needs), NA, ifelse(vapply(strsplit(df$priority_needs, " "), function(t) o %in% t, TRUE), "1", "0")))
df$priority_needs[sm_mismatch] <- "water"; sm[sm_mismatch, "food"] <- "1"; sm[sm_mismatch, "water"] <- "1"
plant(sm_mismatch, "priority_needs", "sm_parent_child")
colnames(sm) <- paste0("priority_needs/", colnames(sm))

# duplicates
df$`_uuid` <- uuid
df$`_uuid`[n] <- df$`_uuid`[10]; plant(n, "", "dup_uuid"); plant(10, "", "dup_uuid")
df[n - 1, main_cols] <- df[20, main_cols]; df[n - 1, c("district", "enumerator_id", "consent", "resp_phone")] <- df[20, c("district", "enumerator_id", "consent", "resp_phone")]
sm[n - 1, ] <- sm[20, ]
plant(n - 1, "", "dup_answers"); plant(20, "", "dup_answers")
nd_src <- 30
df[n - 2, main_cols] <- df[nd_src, main_cols]; df[n - 2, c("district", "enumerator_id", "consent", "resp_phone")] <- df[nd_src, c("district", "enumerator_id", "consent", "resp_phone")]
df$n_meals[n - 2] <- as.character((as.numeric(df$n_meals[nd_src]) %% 3) + 1)
sm[n - 2, ] <- sm[nd_src, ]
plant(n - 2, "", "near_duplicate")

# Kobo-style headers with group paths
grp <- function(x) ifelse(x %in% c("district", "consent", "resp_phone"), paste0("hh_info/", x),
                   ifelse(x %in% main_cols, paste0("hh_info/main/", x), x))
names(df) <- grp(names(df))
sm_df <- as.data.frame(sm, stringsAsFactors = FALSE)
names(sm_df) <- paste0("hh_info/main/", names(sm_df))
pos <- match("hh_info/main/priority_needs", names(df))
export <- cbind(df[, 1:pos], sm_df, df[, (pos + 1):ncol(df)])
export$`_id` <- 100000 + seq_len(n)
export$`_submission_time` <- format(end + 3600, "%Y-%m-%dT%H:%M:%S")
export$`_index` <- seq_len(n)
openxlsx::write.xlsx(export, file.path(out, "example_data.xlsx"), sheetName = "MOZ_HH_2026")

# audit files (active time slightly below start-end time)
adir <- file.path(tempdir(), "audit_build"); unlink(adir, recursive = TRUE)
for (i in seq_len(n)) {
  if (i %in% c(5, 6)) next                       # two submissions without audit
  d <- file.path(adir, export$`_uuid`[i]); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  total_ms <- max(dur[i], 1) * 60000 * 0.9
  k <- 15
  lens <- diff(c(0, sort(runif(k - 1)), 1)) * total_ms
  st <- 1.7e12 + cumsum(c(0, lens[-k])) + seq_len(k) * 500
  a <- data.frame(event = c("form start", rep("question", k)), node = c("", paste0("/q", 1:k)),
                  start = c(1.7e12 - 1000, round(st)), end = c(NA, round(st + lens)))
  utils::write.csv(a, file.path(d, "audit.csv"), row.names = FALSE, na = "")
}
zf <- normalizePath(file.path(out), mustWork = TRUE)
old <- setwd(adir); utils::zip(file.path(zf, "example_audit.zip"), list.files(".", recursive = TRUE), flags = "-q"); setwd(old)

key <- do.call(rbind, key)
key$uuid <- export$`_uuid`[key$row]
utils::write.csv(key, file.path(out, "example_answer_key.csv"), row.names = FALSE)
cat("Planted:", nrow(key), "| fast enum_06 surveys:", sum(fast), "\n")
