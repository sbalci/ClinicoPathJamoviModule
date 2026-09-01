# Regression cover for the defects found during the tableone release review.
# Each block fails against the pre-review backend.

library(testthat)

test_that("publication and comprehensive styles retain verified statistics", {
  d <- data.frame(x = c(1:19, 100),
                  g = factor(c(rep("A", 10), rep("B", 8), NA, NA)))
  plain <- function(content) {
    gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(content)))
  }
  # Mean = 290/20; sample SD = sqrt(sum((x - 14.5)^2)/19).
  # gtsummary uses type-2 quartiles (5.5, 10.5, 15.5), displayed
  # at its default precision as 6, 11, 16 for integer measurements.
  publication <- tableone(d, c("x", "g"), sty = "t2")
  p <- plain(publication$tablestyle2$content)
  expect_match(p, "N = 20", fixed = TRUE)
  expect_match(p, "11 (6, 16)", fixed = TRUE)
  expect_match(p, "B 8 (44%)", fixed = TRUE)
  expect_match(p, "Unknown 2", fixed = TRUE)

  comprehensive <- tableone(d, c("x", "g"), sty = "t3")
  a <- plain(comprehensive$tablestyle3$content)
  expect_match(a, "Overall (N=20)", fixed = TRUE)
  expect_match(a, "14.5 (20.9)", fixed = TRUE)
  expect_match(a, "1.0 - 100.0", fixed = TRUE)
  expect_match(a, "8 (44.4%)", fixed = TRUE)
})

t1 <- function(res) gsub("[[:space:]]+", " ",
                         as.character(res$tablestyle1$content))
t4 <- function(res) gsub("[[:space:]]+", " ",
                         gsub("<[^>]*>", " ", as.character(res$tablestyle4$content)))

test_that("janitor style survives a single selected variable", {
  # data[rows, ] drops a one-column frame to a vector, so every janitor run with
  # exactly one variable died inside the tabulation. A clinician selecting one
  # variable is the most ordinary thing there is.
  skip_if_not_installed("janitor")
  data(tableone_test, package = "ClinicoPath")

  for (v in c("Sex", "TumorStage")) {
    # !! is required: jmvcore resolves a bare symbol to its own NAME, so a plain
    # `vars = v` asks for a column literally called "v" and dies in select().
    res <- tableone(data = tableone_test, vars = !!v, sty = "t4")
    expect_true(nzchar(trimws(t4(res))), info = v)
    expect_match(t4(res), v, info = v)
  }

  # and still works with two, which is what previously masked the bug
  expect_match(t4(tableone(data = tableone_test,
                           vars = c("Sex", "TumorStage"), sty = "t4")), "Sex")
})

test_that("the default style discloses missing values", {
  # t1 reported "n 120" beside a mean computed from 112 observations, with no
  # missing-data column at all - t2 and t3 both showed it. A Table One whose n
  # does not match the rows behind its own statistics is transcribed into
  # manuscripts.
  data(tableone_test, package = "ClinicoPath")
  d <- tableone_test[1:120, ]
  d$Age[1:8] <- NA

  out <- t1(tableone(data = d, vars = c("Age", "Sex")))
  expect_match(out, "Missing")
  # 8 of 120 = 6.7%
  expect_match(out, "6\\.7")
})

test_that("janitor skips continuous variables instead of listing every value", {
  # A frequency table of a continuous variable produced one row per patient,
  # labelled with the full unrounded value ("41.9504137110896").
  skip_if_not_installed("janitor")
  data(tableone_test, package = "ClinicoPath")

  out <- t4(tableone(data = tableone_test, vars = c("Age", "TumorSize"), sty = "t4"))
  expect_match(out, "Not tabulated")
  expect_match(out, "not categorical")
  expect_false(grepl("[0-9]\\.[0-9]{6,}", out))
})

test_that("janitor requires explicit categorical typing for numeric codes", {
  skip_if_not_installed("janitor")
  data(tableone_test, package = "ClinicoPath")
  d <- tableone_test
  d$score <- rep(1:5, length.out = nrow(d))

  out <- t4(tableone(data = d, vars = "score", sty = "t4"))
  expect_match(out, "score")
  expect_match(out, "Not tabulated")
  expect_match(out, "factor()", fixed = TRUE)

  d$score <- ordered(d$score, levels = 1:5)
  out <- t4(tableone(data = d, vars = "score", sty = "t4"))
  expect_false(grepl("Not tabulated", out))
  expect_match(out, "20.0%", fixed = TRUE)
})

test_that("small continuous samples are never inferred to be categories", {
  d <- data.frame(Age = c(seq(30, 40), NA_real_),
                  Sex = factor(rep(c("F", "M"), 6)))
  res <- tableone(data = d, vars = c("Age", "Sex"), sty = "t4")
  out <- as.character(res$tablestyle4$content)
  expect_match(out, "Age (not categorical", fixed = TRUE)
  expect_false(grepl("Frequency Table for 'Age'", out, fixed = TRUE))
  expect_match(out, "Frequency Table for 'Sex'", fixed = TRUE)
})

test_that("omitted janitor measurements cannot exclude cases or enter report text", {
  d <- data.frame(Age = c(30, NA_real_, 40, NA_real_),
                  Sex = factor(c("F", "F", "M", "M")))
  res <- tableone(data = d, vars = c("Age", "Sex"), sty = "t4", excl = TRUE,
                  showSummary = TRUE, showReportSentence = TRUE)
  expect_match(res$summary$content, "4 cases with 1 selected variable", fixed = TRUE)
  expect_match(res$summary$content, "4 cases (no exclusions applied)", fixed = TRUE)
  expect_match(res$reportSentence$content, "Variables included Sex.", fixed = TRUE)
  expect_false(grepl("Variables included Age", res$reportSentence$content, fixed = TRUE))
})

test_that("listwise deletion reports one denominator for every included variable", {
  d <- data.frame(Age = c(30, 40, NA_real_, 60, 70, 80),
                  Sex = factor(c("F", NA, "M", "F", "M", "M")))
  for (style in c("t1", "t2", "t3")) {
    res <- tableone(data = d, vars = c("Age", "Sex"), sty = style, excl = TRUE,
                    showSummary = TRUE, showReportSentence = TRUE)
    expect_match(res$summary$content, "Final N = 4", fixed = TRUE, info = style)
    expect_match(res$summary$content, "same complete-case denominator", fixed = TRUE)
    expect_false(grepl("Per-variable denominators may differ", res$summary$content,
                       fixed = TRUE))
    expect_match(res$reportSentence$content, "4 cases with complete data", fixed = TRUE)
  }
})

test_that("janitor retains categorical, logical and missing-value frequencies", {
  d <- data.frame(Group = factor(c("A", "A", "B", NA)),
                  Flag = c(TRUE, FALSE, TRUE, NA))
  res <- tableone(data = d, vars = c("Group", "Flag"), sty = "t4")
  out <- t4(res)
  expect_match(out, "50.0%", fixed = TRUE)
  expect_match(out, "66.7%", fixed = TRUE)
  expect_match(out, "Valid Percent", fixed = TRUE)
  expect_match(out, "Flag", fixed = TRUE)
  expect_false(grepl("Not tabulated", out, fixed = TRUE))
})

test_that("the About panel states the overall-only scope and binary display rule", {
  d <- data.frame(Sex = factor(c("Female", "Male")))
  res <- tableone(data = d, vars = "Sex", showAbout = TRUE)
  expect_match(res$about$content, "overall cohort", fixed = TRUE)
  expect_match(res$about$content, "does not stratify", fixed = TRUE)
  expect_match(res$about$content, "second level of a binary factor", fixed = TRUE)
})

test_that("an all-missing variable is named rather than silently dropped", {
  data(tableone_test, package = "ClinicoPath")
  d <- tableone_test
  d$NeverMeasured <- NA

  res <- tableone(data = d, vars = c("Age", "NeverMeasured"))
  expect_match(as.character(res$todo$content), "Not included")
  expect_match(as.character(res$todo$content), "NeverMeasured")

  # no notice when nothing was dropped
  res2 <- tableone(data = d, vars = c("Age", "Sex"))
  expect_false(grepl("Not included", as.character(res2$todo$content)))
})

test_that("the refusal message reads cleanly", {
  # was "No valid variables.. Check that ..." - the analysis appended a period to
  # a message that already ended in one.
  data(tableone_test, package = "ClinicoPath")
  d <- tableone_test[1:10, ]
  d$Hemoglobin <- NA

  res <- tableone(data = d, vars = "Hemoglobin")
  expect_match(as.character(res$todo$content), "Nothing to summarise")
  expect_false(grepl("\\.\\.", as.character(res$todo$content)))
})
