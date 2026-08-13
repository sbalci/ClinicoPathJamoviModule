# Regression cover for the defects found during the tableone release review.
# Each block fails against the pre-review backend.

library(testthat)

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
  expect_match(out, "distinct values")
  expect_false(grepl("[0-9]\\.[0-9]{6,}", out))
})

test_that("a low-cardinality numeric variable is still tabulated by janitor", {
  # The skip is on distinct values, not on storage type: an integer score with a
  # handful of levels is exactly what a frequency table is for.
  skip_if_not_installed("janitor")
  data(tableone_test, package = "ClinicoPath")
  d <- tableone_test
  d$score <- rep(1:5, length.out = nrow(d))

  out <- t4(tableone(data = d, vars = "score", sty = "t4"))
  expect_match(out, "score")
  expect_false(grepl("Not tabulated", out))
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

  err <- tryCatch(tableone(data = d, vars = c("Age", "Hemoglobin"), excl = TRUE),
                  error = conditionMessage)
  expect_false(grepl("\\.\\.", err))
})
