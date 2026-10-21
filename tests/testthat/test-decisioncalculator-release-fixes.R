# meddecide 1.1.1 release fixes (independent review 2026-10-21): DC2-01 / XA-01 zero-cell ratios,
# DC2-02 Fagan verb and plot LR, DC2-03 never-positive / never-negative wording.

rf_notices <- function(r) r$notices$content
rf_strip <- function(h) gsub("\\s+", " ", gsub("<[^>]+>", " ", h))
rf_ci <- function(r, key) {
  df <- r$epirTable_number$asDF
  df[gsub('^"|"$', "", rownames(df)) == key, c("est", "lower", "upper")]
}

test_that("TP = 0: LR+ and DOR are exactly 0, LR- is raw, and no nomogram is drawn", {
  r <- decisioncalculator(TP = 0, FN = 10, FP = 1, TN = 99, ci = TRUE, fagan = TRUE, showSummary = TRUE)
  x <- r$ratioTable$asDF
  expect_identical(x$LRP[1], 0)                                # Haldane gave 3.06
  expect_equal(x$LRN[1], (10 / 10) / (99 / 100), tolerance = 1e-12)
  expect_identical(r$advancedMetricsTable$asDF$dor[1], 0)      # Haldane gave 3.16
  # Updated 2026-10-21: a ratio of 0 has the interval [0, U] (U from the +0.5 table), not blank
  expect_identical(unlist(rf_ci(r, "lr.pos")[, c("est", "lower")], use.names = FALSE), c(0, 0))
  expect_match(r$epirTable_number$getCell(rowKey = "lr.pos", col = "upper")$footnotes,
               "runs from 0 to the upper limit", fixed = TRUE)
  expect_identical(unlist(rf_ci(r, "diag.or")[, c("est", "lower")], use.names = FALSE), c(0, 0))
  expect_false(isTRUE(r$plot1$state$drawable))
  fs <- rf_strip(r$faganSummary$content)
  expect_false(grepl("raises the probability", fs, fixed = TRUE))
  expect_match(fs, "positive likelihood ratio is exactly 0", fixed = TRUE)
  expect_match(rf_notices(r), "Fagan nomogram not drawn", fixed = TRUE)
  # the plot state still carries the table's LR- (DC2-02)
  s <- r$plot1$state
  expect_equal((1 - s$Sens) / s$Spec, x$LRN[1], tolerance = 1e-12)
})

test_that("TN = 0 mirror: DOR is 0, and a corrected LR- that would reverse direction is blank", {
  r <- decisioncalculator(TP = 99, FN = 1, FP = 10, TN = 0, ci = TRUE)
  x <- r$ratioTable$asDF
  expect_identical(r$advancedMetricsTable$asDF$dor[1], 0)
  expect_equal(x$LRP[1], (99 / 100) / (10 / 10), tolerance = 1e-12)
  expect_identical(r$ratioTable$getCell(rowNo = 1, col = "LRN")$value, "")   # Haldane gave 0.327
  expect_match(rf_notices(r), "Zero-cell ratio not estimable", fixed = TRUE)
})

test_that("FN = 0: LR- is exactly 0 and LR+ is exactly 10", {
  r <- decisioncalculator(TP = 50, FP = 5, FN = 0, TN = 45, ci = TRUE, fagan = TRUE)
  x <- r$ratioTable$asDF
  expect_identical(x$LRN[1], 0)
  expect_equal(x$LRP[1], 10, tolerance = 1e-12)
  expect_false(isTRUE(r$plot1$state$drawable))
})

test_that("FP = 0 keeps the corrected LR+, and the nomogram uses the table's ratios", {
  r <- decisioncalculator(TP = 45, FN = 5, FP = 0, TN = 50, ci = TRUE, fagan = TRUE)
  x <- r$ratioTable$asDF
  expect_equal(x$LRP[1], (45.5 / 51) / (0.5 / 51), tolerance = 1e-12)   # 91
  expect_equal(x$LRN[1], 0.1, tolerance = 1e-12)
  expect_equal(r$advancedMetricsTable$asDF$dor[1], 45.5 * 50.5 / (5.5 * 0.5), tolerance = 1e-12)
  expect_match(rf_notices(r), "Continuity correction applied", fixed = TRUE)
  s <- r$plot1$state
  expect_true(s$drawable)
  expect_equal(c(s$Sens / (1 - s$Spec), (1 - s$Sens) / s$Spec), c(x$LRP[1], x$LRN[1]), tolerance = 1e-10)
  fs <- rf_strip(r$faganSummary$content)
  expect_match(fs, "likelihood ratio of 91.00 and raises", fixed = TRUE)
  expect_match(fs, "likelihood ratio of 0.100 and lowers", fixed = TRUE)
})

test_that("never-positive / never-negative notices say the other LR is exactly 1", {
  n <- decisioncalculator(TP = 0, FN = 10, FP = 0, TN = 10)
  expect_identical(n$ratioTable$asDF$LRN[1], 1)
  expect_false(grepl("differs from 1", rf_notices(n), fixed = TRUE))
  expect_match(rf_notices(n), "negative likelihood ratio is exactly 1", fixed = TRUE)
  p <- decisioncalculator(TP = 10, FN = 0, FP = 10, TN = 0)
  expect_identical(p$ratioTable$asDF$LRP[1], 1)
  expect_match(rf_notices(p), "positive likelihood ratio is exactly 1", fixed = TRUE)
})

# Upper limit of a zero ratio's interval, by hand: Simel (LRs) / Woolf (DOR) log-scale interval on
# the table with 0.5 added to every cell. Same oracle as test-decision-release-fixes.R and
# test-decisioncombine-release-fixes.R, so the three analyses print one interval per 2x2.
rf_zero_upper <- function(ratio, TP, FP, FN, TN) {
  a <- TP + .5; b <- FP + .5; c <- FN + .5; d <- TN + .5; z <- qnorm(.975)
  switch(ratio,
    lr.pos = exp(log((a / (a + c)) / (b / (b + d))) + z * sqrt(1 / a - 1 / (TP + FN + 1) + 1 / b - 1 / (FP + TN + 1))),
    lr.neg = exp(log((c / (a + c)) / (d / (b + d))) + z * sqrt(1 / c - 1 / (TP + FN + 1) + 1 / d - 1 / (FP + TN + 1))),
    diag.or = exp(log(a * d / (b * c)) + z * sqrt(1 / a + 1 / b + 1 / c + 1 / d)))
}

test_that("zero ratio interval is [0, U], U on the 0.5-corrected table (hand oracle, 2026-10-21)", {
  # TP 50 FP 5 FN 0 TN 45: LR- = 0, U = 0.17360 (arithmetic in test-decision-release-fixes.R).
  ci <- rf_ci(decisioncalculator(TP = 50, FP = 5, FN = 0, TN = 45, ci = TRUE), "lr.neg")
  expect_identical(c(ci$est, ci$lower), c(0, 0))
  expect_equal(ci$upper, rf_zero_upper("lr.neg", 50, 5, 0, 45), tolerance = 1e-10)
  r <- decisioncalculator(TP = 0, FP = 5, FN = 10, TN = 85, ci = TRUE)
  for (k in c("lr.pos", "diag.or")) {
    ci <- rf_ci(r, k)
    expect_identical(c(ci$est, ci$lower), c(0, 0))
    expect_equal(ci$upper, rf_zero_upper(k, 0, 5, 10, 85), tolerance = 1e-10)
  }
})
