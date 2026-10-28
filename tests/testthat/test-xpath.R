d <- data.frame(a = c("yes", "no", NA), n = c("5", "20", NA), sm = c("food water", "dk", NA),
                d = c("2026-01-10", "2025-05-01", NA), stringsAsFactors = FALSE)

test_that("comparisons follow XPath rules on text values", {
  expect_equal(eval_xpath("${a} = 'yes'", d), c(TRUE, FALSE, FALSE))
  expect_equal(eval_xpath("${a} != 'yes'", d), c(FALSE, TRUE, TRUE))
  expect_equal(eval_xpath("${n} >= 10", d), c(FALSE, TRUE, FALSE))
  expect_equal(eval_xpath("${n} = ''", d), c(FALSE, FALSE, TRUE))
  expect_equal(eval_xpath("${n} > 3 and ${a} = 'yes' or ${a} = 'no'", d), c(TRUE, TRUE, FALSE))
})

test_that("arithmetic keeps operator precedence", {
  expect_equal(eval_xpath("${n} + 2 * 3 > 10", d), c(TRUE, TRUE, FALSE))
  expect_equal(eval_xpath("${n} div 5 = 4", d), c(FALSE, TRUE, FALSE))
  expect_equal(eval_xpath("${n} mod 2 = 0", d), c(FALSE, TRUE, FALSE))
})

test_that("select_multiple functions work", {
  expect_equal(eval_xpath("selected(${sm}, 'water')", d), c(TRUE, FALSE, FALSE))
  expect_equal(eval_xpath("count-selected(${sm}) = 2", d), c(TRUE, FALSE, FALSE))
  expect_equal(eval_xpath("not(selected(${sm}, 'dk'))", d), c(TRUE, FALSE, TRUE))
})

test_that("self reference, strings and dates", {
  expect_equal(eval_xpath(". >= 10", d, self = d$n), c(FALSE, TRUE, FALSE))
  expect_equal(eval_xpath("string-length(${a}) > 2", d), c(TRUE, FALSE, FALSE))
  expect_equal(eval_xpath("regex(${a}, '^y')", d), c(TRUE, FALSE, FALSE))
  expect_equal(eval_xpath("${d} > date('2025-12-31')", d), c(TRUE, FALSE, FALSE))
  expect_equal(eval_xpath("if(${a} = 'yes', 1, 0) = 1", d), c(TRUE, FALSE, FALSE))
  expect_equal(eval_xpath("${a} = 'a or b and c'", d), c(FALSE, FALSE, FALSE))
})

test_that("unknown references and unsupported functions raise errors", {
  expect_error(eval_xpath("${missing} = 1", d), "not found")
  expect_error(eval_xpath("jr:choice-name(${a}, '${a}') = 'x'", d))
})
