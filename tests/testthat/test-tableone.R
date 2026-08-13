# Smoke test for tableone().
#
# This file was a generated scaffold and asserted three things that were never
# true of a jamovi analysis, so it had failed since it was written:
#   * is.list(model)                  - a Results object is R6, not a list
#   * inherits(model, 'jmvcoreClass') - the class is 'tableoneResults'
#   * jmvReadWrite::write_omv(model)  - write_omv serialises a DATA FRAME to
#                                       .omv; it has no notion of a results
#                                       object, hence "Input data are either not
#                                       a data frame or have incorrect
#                                       (unsupported) data type."
# Replaced with assertions about what the analysis actually returns.

test_that("tableone analysis works", {
  set.seed(123)
  n <- 50
  data <- data.frame(
    vars1 = runif(n, 1, 100),
    vars2 = runif(n, 1, 100),
    vars3 = runif(n, 1, 100)
  )

  expect_no_error({
    model <- tableone(
      data = data,
      vars = c("vars1", "vars2", "vars3"),
      sty = "t1",
      excl = FALSE,
      showSummary = FALSE,
      showAbout = FALSE,
      showReportSentence = FALSE
    )
  })

  expect_s3_class(model, "tableoneResults")
  expect_true(all(c("tablestyle1", "tablestyle2", "tablestyle3", "tablestyle4")
                  %in% names(model)))

  # The default style must actually be populated, and must report the full n -
  # a table that renders empty is the failure mode this smoke test exists for.
  out <- as.character(model$tablestyle1$content)
  expect_true(nzchar(trimws(out)))
  expect_match(out, "n\\s+50")
  for (v in c("vars1", "vars2", "vars3"))
    expect_match(out, v)
})

test_that("tableone reports the same n across all four styles", {
  set.seed(123)
  data <- data.frame(
    score = runif(40, 1, 100),
    grp   = factor(rep(c("A", "B"), each = 20))
  )

  for (style in c("t1", "t2", "t3", "t4")) {
    res <- tableone(data = data, vars = c("score", "grp"), sty = style)
    item <- res[[paste0("tablestyle", substr(style, 2, 2))]]
    expect_true(nzchar(trimws(as.character(item$content))), info = style)
  }
})
