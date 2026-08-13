# Regression cover for the defects found during the reportcat release review.
# Each block fails against the pre-review backend.

library(testthat)

rc_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<br>", " | ", as.character(x)))

test_that("missing values are not rendered as a category", {
  # summary.factor() names its missing-count row "NAs" - no apostrophe - but the
  # backend filtered on "NA's", so the filter never matched and the missing count
  # was listed as though it were a level, with a percentage of missing/valid:
  #
  #   g has 6 rows and 2 levels.
  #   NAs: n = 3, 100% of valid cases.   <- missing, not a category
  #   B:   n = 2, 67% of valid cases.
  #   A:   n = 1, 33% of valid cases.
  #
  # The percentage exceeds 100% whenever missing outnumber observed cases.
  d <- data.frame(g = factor(c("A", "B", "B", NA, NA, NA)))
  out <- rc_txt(reportcat(data = d, vars = "g")$text$content)

  expect_false(grepl("NAs:", out, fixed = TRUE))
  expect_false(grepl("NA's:", out, fixed = TRUE))
  expect_match(out, "Missing values: 3")
  expect_match(out, "A: n = 1")
  expect_match(out, "B: n = 2")

  # percentages are of valid cases and must not exceed 100 in total
  pcts <- as.numeric(gsub("%", "", regmatches(out, gregexpr("[0-9]+%", out))[[1]]))
  expect_lte(sum(pcts), 101)   # allow 1 point of rounding slack
})

test_that("percentages of valid cases sum to 100 when missingness is heavy", {
  # 1 valid case out of 10; the missing row previously claimed 100%.
  d <- data.frame(g = factor(c("A", rep(NA, 9))))
  out <- rc_txt(reportcat(data = d, vars = "g")$text$content)

  expect_match(out, "A: n = 1, 100% of valid cases")
  expect_match(out, "Missing values: 9")
  expect_false(grepl("n = 9", out, fixed = TRUE))
})

test_that("the level count matches the levels actually listed", {
  # A factor with a declared but unobserved level reported "has 4 rows and 2
  # levels" and then listed three.
  f <- factor(c("Present", "Absent", "Present", "Absent"),
              levels = c("Present", "Absent", "Equivocal"))
  out <- rc_txt(reportcat(data = data.frame(status = f), vars = "status")$text$content)

  expect_match(out, "3 levels")
  expect_match(out, "1 of these levels has no observations")
  expect_match(out, "Equivocal: n = 0")

  # no such clause when every level is observed
  out2 <- rc_txt(reportcat(data = data.frame(status = droplevels(f)),
                           vars = "status")$text$content)
  expect_match(out2, "2 levels")
  expect_false(grepl("no observations", out2))
})

test_that("the sparse-category warning counts observed categories only", {
  # table() keeps declared-but-unused levels at 0, and 0 < 5, so five declared
  # levels of which only two occur (30 cases each - nothing rare) produced
  # "3 categories with <5 cases. Consider combining rare categories."
  # Empty categories cannot be combined.
  f <- factor(rep(c("A", "B"), each = 30), levels = c("A", "B", "C", "D", "E"))
  guidance <- as.character(reportcat(data = data.frame(g = f), vars = "g")$assumptions$content)
  expect_false(grepl("Consider combining rare categories", guidance))

  # but genuinely rare observed categories are still flagged
  f2 <- factor(c(rep("A", 30), rep("B", 30), "C", "D"))
  guidance2 <- as.character(reportcat(data = data.frame(g = f2), vars = "g")$assumptions$content)
  expect_match(guidance2, "2 categories with")
})

test_that("counts and percentages match a hand-computed frequency table", {
  set.seed(42)
  g <- factor(sample(c("G1", "G2", "G3"), 97, replace = TRUE,
                     prob = c(0.5, 0.3, 0.2)))
  g[1:7] <- NA
  d <- data.frame(grade = g)
  out <- rc_txt(reportcat(data = d, vars = "grade")$text$content)

  tab <- table(g, useNA = "no")
  n_valid <- sum(!is.na(g))
  expect_equal(sum(tab), n_valid)

  # scales::percent() picks its accuracy from the WHOLE vector, so the expected
  # strings must be built vector-wise (48.9%), not element by element (49%).
  pct <- scales::percent(as.numeric(tab) / n_valid)
  names(pct) <- names(tab)

  for (lv in names(tab)) {
    expect_match(out, sprintf("%s: n = %d", lv, tab[[lv]]), fixed = TRUE)
    expect_match(out, sprintf("%s: n = %d, %s of valid cases", lv, tab[[lv]], pct[[lv]]),
                 fixed = TRUE)
  }
  expect_match(out, sprintf("has %d rows", length(g)))
  expect_match(out, sprintf("Missing values: %d", sum(is.na(g))))
})

test_that("variable names containing a space survive the formula round-trip", {
  # constructFormula()/decomposeFormula() can mangle names with spaces, which
  # would break the downstream mydata[[var]] lookups.
  d <- data.frame(check.names = FALSE,
                  `Tumor Grade` = factor(c("G1", "G2", "G3", "G2")))
  out <- rc_txt(reportcat(data = d, vars = "Tumor Grade")$text$content)

  expect_match(out, "Tumor Grade")
  expect_match(out, "G2: n = 2")
  expect_false(grepl("NA", out, fixed = TRUE))
})

test_that("a high-cardinality variable is not lumped into '(Other)'", {
  # summary.factor() defaults to maxsum = 100 and collapses the tail; the
  # replacement uses table(), which has no such limit.
  d <- data.frame(id = factor(sprintf("L%03d", 1:150)))
  out <- rc_txt(reportcat(data = d, vars = "id")$text$content)

  expect_false(grepl("(Other)", out, fixed = TRUE))
  expect_match(out, "150 levels")
  expect_match(out, "L150: n = 1")
})
