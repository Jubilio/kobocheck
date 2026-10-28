ex <- system.file("extdata", package = "kobocheck")
form <- read_xlsform(file.path(ex, "example_form.xlsx"))
data <- read_kobo_data(file.path(ex, "example_data.xlsx"), form)
audit <- read_audits(file.path(ex, "example_audit.zip"))
res <- run_checks(data, form, kc_config(near_dup_share = 0.1), audit)
key <- utils::read.csv(file.path(ex, "example_answer_key.csv"), stringsAsFactors = FALSE)
key$question[is.na(key$question)] <- ""

test_that("group prefixes are removed from column names", {
  expect_true(all(c("hoh_age", "priority_needs", "priority_needs/food", "district") %in% names(data)))
})

test_that("audit files are read", {
  # 240 submissions, 2 without audit, and one duplicated uuid shares a folder
  expect_equal(nrow(audit), 237)
  expect_true(all(audit$audit_active_min > 0))
})

test_that("every planted error is found", {
  found <- mapply(function(u, q, ck) any(res$flags$uuid == u & res$flags$check_id == ck &
                  (q == "" | res$flags$question == q)), key$uuid, key$question, key$check_id)
  expect_true(all(found), info = paste(key$check_id[!found], collapse = ", "))
})

test_that("don't-know codes are not reported as outliers", {
  dk <- data$`_uuid`[data$water_litres %in% "-999"]
  expect_false(any(res$flags$uuid %in% dk & res$flags$check_id == "outlier"))
})

test_that("non-consent surveys are not treated as duplicates", {
  nc <- data$`_uuid`[data$consent %in% "no"]
  expect_false(any(res$flags$uuid %in% nc & res$flags$check_id == "dup_answers"))
})

test_that("the fast enumerator is singled out", {
  s <- enumerator_summary(res)
  expect_equal(s$enumerator[1], "enum_06")
  expect_match(s$follow_up[1], "too short")
})

test_that("all form expressions were evaluated", {
  expect_equal(nrow(res$not_checked), 0)
})

test_that("cleaning log workbook is written and can be applied", {
  f <- tempfile(fileext = ".xlsx")
  write_cleaning_log(res, f)
  expect_true(all(c("README", "cleaning_log", "summary", "enumerators", "durations", "not_checked")
                  %in% readxl::excel_sheets(f)))
  log <- as.data.frame(readxl::read_excel(f, "cleaning_log", col_types = "text"))
  expect_lte(nrow(log), nrow(res$flags))
  key <- paste(log$uuid, log$question)[nzchar(log$question) & log$check_id != "dup_uuid"]
  expect_false(any(duplicated(key)))   # one decision per cell

  i_age <- which(grepl("constraint", log$check_id) & log$question == "hoh_age")[1]
  log$change_type[i_age] <- "change_response"; log$new_value[i_age] <- "21"
  i_sm <- which(grepl("sm_exclusive", log$check_id))[1]
  log$change_type[i_sm] <- "change_response"; log$new_value[i_sm] <- "food"
  i_dup <- which(grepl("dup_answers", log$check_id))[1]
  log$change_type[i_dup] <- "remove_survey"
  i_sk <- which(grepl("skip_logic_answered", log$check_id))[1]
  log$change_type[i_sk] <- "blank_response"

  a <- apply_cleaning_log(data, log, form, "_uuid")
  expect_equal(nrow(a$data), nrow(data) - 1)
  expect_equal(nrow(a$deletion_log), 1)
  expect_equal(a$data$hoh_age[a$data$`_uuid` == log$uuid[i_age]], "21")
  row_sm <- a$data$`_uuid` == log$uuid[i_sm]
  expect_equal(a$data$priority_needs[row_sm], "food")
  expect_equal(a$data$`priority_needs/dk`[row_sm], "0")
  expect_equal(a$data$`priority_needs/food`[row_sm], "1")
  expect_true(is.na(a$data$displacement_months[a$data$`_uuid` == log$uuid[i_sk]]))
  expect_true(sum(a$log$apply_status == "pending review") > 0)
  # option-column changes are logged so raw + log = clean
  child <- a$log[a$log$check_id %in% "sm_child_update" & a$log$uuid == log$uuid[i_sm], ]
  expect_setequal(child$question, c("priority_needs/dk"))

  # re-running the checks on the clean data clears the fixed issues
  res2 <- run_checks(a$data, form, kc_config(near_dup_share = 0.1), audit)
  expect_false(any(res2$flags$uuid == log$uuid[i_age] & res2$flags$question == "hoh_age"))
  expect_false(any(res2$flags$uuid == log$uuid[i_sm] & res2$flags$check_id == "sm_exclusive"))
})

test_that("csv exports with dot separators are read", {
  f <- tempfile(fileext = ".csv")
  raw <- as.data.frame(readxl::read_excel(file.path(ex, "example_data.xlsx"), col_types = "text"),
                       check.names = FALSE)
  names(raw) <- gsub("/", ".", names(raw))
  utils::write.table(raw, f, sep = ";", row.names = FALSE, na = "")
  d2 <- read_kobo_data(f, form)
  expect_true(all(c("hoh_age", "priority_needs/food") %in% names(d2)))
})

test_that("one record of a duplicated uuid can be removed by _id", {
  log <- build_cleaning_log(res)
  du <- which(log$check_id == "dup_uuid")
  expect_length(du, 2)
  k <- du[which.max(as.numeric(log$old_value[du]))]
  log$change_type[k] <- "remove_survey"
  a <- apply_cleaning_log(data, log, form, "_uuid")
  expect_equal(nrow(a$data), nrow(data) - 1)
  expect_equal(sum(a$data$`_uuid` == log$uuid[k]), 1)
  expect_equal(a$deletion_log$`_id`, log$old_value[k])
})

test_that("special codes and PII are detected", {
  expect_true(all(data$`_uuid`[data$water_litres %in% "-999"] %in%
                  res$flags$uuid[res$flags$check_id == "special_value"]))
  expect_true("resp_phone" %in% res$pii$question)
  expect_false(any(grepl("\\d{3} \\d{4}", res$pii$example)))   # examples are masked
})

test_that("dataset package follows the Argus layout", {
  log <- build_cleaning_log(res)
  log$change_type <- "no_action"
  log$change_type[grepl("sm_exclusive", log$check_id)] <- "change_response"
  log$new_value[grepl("sm_exclusive", log$check_id)] <- "food"
  log$change_type[grepl("duration_short", log$check_id)] <- "remove_survey"
  a <- apply_cleaning_log(data, log, form, "_uuid")
  f <- tempfile(fileext = ".xlsx")
  export_dataset_package(a, data, form, f, result = res, remove_pii = res$pii$question)
  sh <- readxl::excel_sheets(f)
  expect_true(all(c("read_me", "hh_raw_data", "hh_clean_data", "hh_cleaning_log", "hh_deletion_log",
                    "survey", "choices", "variable_tracker", "checklist_data_cleaning") %in% sh))
  cd <- readxl::read_excel(f, "hh_clean_data", col_types = "text")
  dl <- readxl::read_excel(f, "hh_deletion_log", col_types = "text")
  cl <- readxl::read_excel(f, "hh_cleaning_log", col_types = "text")
  expect_true("priority_needs.food" %in% names(cd))
  expect_false("resp_phone" %in% names(cd))
  expect_true(all(cd$consent == "yes"))
  expect_equal(nrow(cd) + nrow(dl), nrow(data))
  expect_false(any(cl$uuid %in% dl$uuid))
  expect_true(all(cl$change_type %in% c("change_response", "blank_response")))
  ch <- readxl::read_excel(f, "choices", col_types = "text")
  expect_true(any(ch$list_name == "shelter" & ch$name == "other"))
  vt <- readxl::read_excel(f, "variable_tracker", col_types = "text")
  expect_true("resp_phone" %in% vt$variable)
})
