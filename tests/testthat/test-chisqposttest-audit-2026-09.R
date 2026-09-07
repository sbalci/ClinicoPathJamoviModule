# Regression cover for the 2026-09 audit fixes to chisqposttest:
# fractional weighted counts no longer abort the run, the mild expected-count
# case gets a default-visible warning, test-method labels and the Methods
# sentence are whole phrases, and listwise deletion goes through jmvcore.

library(testthat)

# 3x3 with a strong diagonal association: omnibus significant, post-hoc runs
audit_data <- function() {
  set.seed(21)
  rows <- rep(c("R1", "R2", "R3"), each = 60)
  cols <- c(sample(c("C1", "C2", "C3"), 60, TRUE, c(.7, .15, .15)),
            sample(c("C1", "C2", "C3"), 60, TRUE, c(.15, .7, .15)),
            sample(c("C1", "C2", "C3"), 60, TRUE, c(.15, .15, .7)))
  data.frame(rows = factor(rows), cols = factor(cols))
}

# Aggregated 3x3 with every expected count above 5, optionally shifted off the
# integers to exercise the weighted-count prose
audit_counts <- function(frac = 0) {
  agg <- expand.grid(rows = factor(c("R1", "R2", "R3")),
                     cols = factor(c("C1", "C2", "C3")))
  agg$n <- c(20, 5, 3, 4, 18, 5, 3, 6, 16) + frac
  agg
}

cq <- function(d, ...) ClinicoPath::chisqposttest(data = d, rows = "rows", cols = "cols",
                                                  counts = NULL, ...)

test_that("fractional weighted counts are refused, whole-number counts run", {
  # Superseded policy: fractional weights used to run with a warning. X^2 on a
  # frequency-weighted table scales linearly with the total, so a fractional or
  # rescaled weight makes the p-value a function of scale rather than of the
  # association; survey weights need a Rao-Scott correction this analysis does
  # not compute. They are now refused outright.
  small <- data.frame(rows = factor(c("A", "A", "B", "B")),
                      cols = factor(c("X", "Y", "X", "Y")),
                      n = c(4.5, 2.5, 3, 5.5))
  res <- ClinicoPath::chisqposttest(data = small, rows = "rows", cols = "cols",
                                    counts = "n", showAssumptionsCheck = TRUE)
  expect_match(res$notices$content, "whole numbers")
  expect_match(as.character(res$todo$content), "whole numbers")

  res <- ClinicoPath::chisqposttest(data = audit_counts(0.5), rows = "rows", cols = "cols",
                                    counts = "n", showClinicalSummary = TRUE,
                                    copyReadySentences = TRUE, showAssumptionsCheck = TRUE,
                                    exportResults = TRUE)
  expect_match(res$notices$content, "whole numbers")
  # nothing downstream is computed from a refused table
  expect_false(grepl("n = 84.5", as.character(res$clinicalSummary$content), fixed = TRUE))

  # whole-number totals still run and still print as integers
  res <- ClinicoPath::chisqposttest(data = audit_counts(0), rows = "rows", cols = "cols",
                                    counts = "n", copyReadySentences = TRUE)
  expect_match(as.character(res$reportSentences$content), "n = 80", fixed = TRUE)
  expect_false(grepl("whole numbers", res$notices$content))
})

test_that("a mild expected-count violation gets a default-visible warning", {
  # exactly one of six cells has 1 <= E < 5 (E = 3): inside Cochran's 20% limit,
  # so neither STRONG_WARNING fires and, before the fix, nothing at all did
  d <- data.frame(
    rows = factor(rep(c("R1", "R2", "R3"), c(10, 40, 150))),
    cols = factor(c(rep(c("C1", "C2"), c(3, 7)),
                    rep(c("C1", "C2"), c(12, 28)),
                    rep(c("C1", "C2"), c(45, 105)))))
  ex <- suppressWarnings(chisq.test(table(d$rows, d$cols)))$expected
  expect_true(sum(ex < 5) == 1 && all(ex >= 1))

  notes <- cq(d)$notices$content
  expect_match(notes, "WARNING: Some expected counts are below 5", fixed = TRUE)
  expect_match(notes, "1 of 6 cells (17%)", fixed = TRUE)
  expect_false(grepl("too low for the chi-square approximation", notes, fixed = TRUE))
  expect_false(grepl("At least one expected count is below 1", notes, fixed = TRUE))

  # ... and it stays silent when every expected count is adequate
  expect_false(grepl("Some expected counts are below 5", cq(audit_data())$notices$content, fixed = TRUE))
})

test_that("test-method labels are whole phrases in the table, export and detail panels", {
  d <- audit_data()
  chi <- cq(d, testSelection = "chisquare", showDetailedTables = TRUE, exportResults = TRUE)
  expect_true(all(chi$posthocTable$asDF$test_method == "Chi-square"))
  expect_match(as.character(chi$detailedComparisons$content), "Chi-square test", fixed = TRUE)
  expect_true(any(chi$exportTable$asDF$interpretation == "Test method: Chi-square"))

  fis <- cq(d, testSelection = "fisher", showDetailedTables = TRUE, exportResults = TRUE)
  expect_true(all(startsWith(fis$posthocTable$asDF$test_method, "Fisher's exact")))
  detail <- as.character(fis$detailedComparisons$content)
  expect_match(detail, "Fisher's exact test", fixed = TRUE)
  expect_false(grepl("exact test test", detail, fixed = TRUE))
  expect_true(any(startsWith(fis$exportTable$asDF$value, "Fisher's exact")))
})

test_that("the Methods sentence is a complete sentence for 2x2 and larger tables", {
  two <- data.frame(rows = factor(rep(c("A", "B"), each = 40)),
                    cols = factor(c(rep(c("X", "Y"), c(30, 10)), rep(c("X", "Y"), c(10, 30)))))
  s2 <- as.character(cq(two, copyReadySentences = TRUE)$reportSentences$content)
  expect_match(s2, "between two binary categorical variables.", fixed = TRUE)

  s3 <- as.character(cq(audit_data(), copyReadySentences = TRUE)$reportSentences$content)
  expect_match(s3, "between two categorical variables.", fixed = TRUE)
  expect_false(grepl("two binary", s3, fixed = TRUE))
})

test_that("listwise deletion through jmvcore reports and applies the same drop", {
  d <- audit_data()
  d$cols[1:2] <- NA
  res <- cq(d)
  expect_match(res$notices$content, "2 of 180 rows (1.1%)", fixed = TRUE)
  expect_match(res$notices$content, "178 observations in a 3 x 3 table", fixed = TRUE)
  expect_equal(res$chisqTable$asDF$value,
               unname(chisq.test(table(d$rows, d$cols), correct = FALSE)$statistic),
               tolerance = 1e-10)
})

test_that("comparison labels keep the 'a vs b' form the other tests parse", {
  df <- cq(audit_data())$posthocTable$asDF
  expect_equal(nrow(df), 6L)
  expect_true(all(grepl("^[RC][123] vs [RC][123]$", df$comparison)))
})
