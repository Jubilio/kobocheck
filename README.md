# kobocheck

[![R-CMD-check](https://github.com/Jubilio/kobocheck/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Jubilio/kobocheck/actions/workflows/R-CMD-check.yaml)

Automated cleaning logs for KoboToolbox data. Give it a Kobo export, the
XLSForm and (optionally) the audit zip; it returns a cleaning log in the
REACH/IMPACT format, ready for review with field teams.

The skip-logic and constraint checks are read directly from the XLSForm
`relevant` and `constraint` columns, so nobody has to write them in R.

## Install (Windows, R 4.1 or later)

```r
install.packages(c("readxl", "openxlsx", "shiny", "bslib", "DT"))
install.packages("kobocheck_0.2.0.tar.gz", repos = NULL, type = "source")
```

## Use the app

```r
kobocheck::run_app()
```

1. **Run checks**: upload the export, the XLSForm and the audit zip, confirm the
   uuid and enumerator columns, press *Run checks*, then download the log.
2. Review the log with the team: fill `change_type`, `new_value` and `comment`.
3. **Apply reviewed log**: upload the reviewed log to get the clean data,
   the applied log (with the status of each row) and the deletion log.
4. **Download dataset package**: one workbook with raw and clean data,
   cleaning and deletion logs, tool, variable tracker and cleaning checklist,
   laid out for IMPACT's Argus validator (see below).

## Use it from R

```r
library(kobocheck)
res <- kobocheck("data/MOZ_MSNA_2026.xlsx", "tools/MOZ_MSNA_2026_kobo.xlsx",
                 audit_path = "audit/audit.zip",
                 cfg = kc_config(min_duration = 20, dk_codes = c(-999, 99)))

# after review
form  <- read_xlsform("tools/MOZ_MSNA_2026_kobo.xlsx")
data  <- read_kobo_data("data/MOZ_MSNA_2026.xlsx", form)
clean <- apply_cleaning_log(data, "2026-10-27_cleaning_log_reviewed.xlsx", form)
write_clean_data(clean, "2026-10-27_clean_data.xlsx")

# final package for submission (Argus layout)
export_dataset_package(clean, data, form, "MOZ_MSNA_2026_dataset.xlsx", prefix = "hh_",
                       result = res, remove_pii = res$pii$question,
                       enumerator_performance = enumerator_summary(res))
```

Try it on the bundled example (240 surveys with 31 planted errors):

```r
ex <- system.file("extdata", package = "kobocheck")
kobocheck(file.path(ex, "example_data.xlsx"), file.path(ex, "example_form.xlsx"),
          file.path(ex, "example_audit.zip"), "example_cleaning_log.xlsx")
```

## Checks

| Check | check_id | Source |
|---|---|---|
| Duplicated uuid | `dup_uuid` | data |
| All answers identical to another survey | `dup_answers` | data |
| Near-duplicate survey by the same enumerator | `near_duplicate` | data |
| Survey too short / too long / end before start | `duration_short`, `duration_long`, `duration_negative` | audit files, else start/end |
| Numeric outliers (robust z; log scale for high values) | `outlier` | integer/decimal questions |
| "Other, specify" responses to recode | `other_response` | text questions linked to `other` |
| Answered although skip logic excludes it | `skip_logic_answered` | XLSForm `relevant` (incl. groups) |
| Required question empty | `required_missing` | XLSForm `required` + `relevant` |
| Constraint broken | `constraint` | XLSForm `constraint` |
| Value not in choice list | `choice_not_in_list` | XLSForm `choices` |
| Exclusive option combined (dk, pnta, none) | `sm_exclusive` | select_multiple |
| Text and 0/1 columns disagree | `sm_parent_child` | select_multiple |
| Custom logical checks | your `check_id` | `logical_checks` sheet |
| Don't-know / special codes (-999, 99, 88...) | `special_value` | integer/decimal questions |
| Many *applicable* questions left empty | `missing_share` | XLSForm `relevant` (skipped questions are not counted as missing) |
| Personal data in column names or values | `pii_review` sheet | names (EN/FR/PT), e-mails, phone numbers incl. Mozambican mobiles |

Flags on the same uuid and question are merged into one log row, so each
value gets one decision.

### Custom checks without code

Add a `logical_checks` sheet to the XLSForm:

| check_id | description | expression | questions |
|---|---|---|---|
| lc_single_with_child | Single-person household reports a child under 5 | `${hh_size} = 1 and ${children_u5} >= 1` | hh_size, children_u5 |

A survey is flagged when the expression is true. Expressions use the same
syntax as the form (`selected()`, `count-selected()`, `and`/`or`, `if()`,
`regex()`, `string-length()`, dates, arithmetic).

## Reviewing and applying the log

* `change_response`: write the corrected value in `new_value`. For a
  select_multiple question, write the full list; the 0/1 option columns are
  updated **and each change is added to the log** (`sm_child_update`), so
  raw data + cleaning log = clean data.
* `blank_response`, `no_action`, `remove_survey` as usual. A duplicated uuid
  appears once per record with its `_id` in `old_value`; `remove_survey` on
  that row removes only that record.
* Changes to a removed survey are not applied and do not reach the final
  cleaning log.

## Argus compatibility

`export_dataset_package()` writes the sheets and columns defined in
[argus_schemas](https://github.com/impact-initiatives/argus_schemas):
`read_me`, `hh_raw_data`, `hh_clean_data`, `hh_cleaning_log`,
`hh_deletion_log` (`reason_deletion`), `survey`, `choices`,
`variable_tracker`, `checklist_data_cleaning` and
`enumerator_performance_log`. It uses `uuid` and the IMPACT
`question.option` separator, adds `other` to choice lists used with
`or_other`, keeps only applied changes in the cleaning log and removes
no-consent surveys.

The example package was run through Argus (version 2026.06.21, schemas of
18 Oct 2026, programme `msna`): 18 rules pass, with 2 warnings (no
`sampling_info` or `weight`, expected without weights). One error remains
and comes from Argus itself: it matches raw and clean data by uuid only, so
when the raw data has two records with the same uuid it compares the clean
record with the removed one.

Known Argus issues worth reporting upstream:

* Consent values accepted are `yes` and `oui`. A form coded in Portuguese
  (`sim`) makes every clean survey fail the consent check.
* The phone pattern only matches numbers starting with `+` or `0`;
  Mozambican mobiles written as `84 123 4567` are not caught (kobocheck
  catches them).

To validate a package yourself:

```bash
git clone https://github.com/impact-initiatives/argus && cd argus && uv sync
uv run main.py --programme-type msna --output_type dataset MOZ_MSNA_2026_dataset.xlsx
```

## Output workbook

`README`, `cleaning_log` (yellow columns to fill, drop-down on `change_type`),
`summary`, `enumerators`, `pii_review`, `durations`, `not_checked` (form expressions that
could not be evaluated, so nothing is silently skipped).

## Known limits (v0.2.0)

* Repeat groups are not checked.
* `jr:choice-name()`, `format-date()` and sums/counts over repeats are not
  evaluated; they are listed under `not_checked`.
* Because Kobo enforces `relevant`, `required` and `constraint` during data
  collection, hits on these checks usually come from form-version changes,
  edited submissions or imported data.
* Enumerator flags are screening signals for follow-up, not conclusions.

## Tests

`R CMD check` passes with no errors, warnings or notes. The tests confirm that
all 31 planted errors in the example data are found, that don't-know codes and
non-consent surveys do not cause false flags, and that an applied log fixes the
issues when the checks are re-run.
