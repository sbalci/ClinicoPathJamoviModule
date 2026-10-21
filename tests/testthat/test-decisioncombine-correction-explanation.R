test_that("combination notes describe selective correction without changing the estimator", {
  d <- data.frame(y = factor(rep(c("P", "N"), c(20, 30))),
                  a = factor(rep(c("P", "N", "N"), c(18, 2, 30))))
  r <- decisioncombine(data = d, gold = "y", goldPositive = "P",
                       test1 = "a", test1Positive = "P",
                       test2Positive = NULL, test3Positive = NULL)
  notes <- paste(vapply(r$combinationTable$notes, function(x) x$note, ""), collapse = " ")
  # Wording follows the 2026-10-21 zero-cell rule (XA-01): a ratio is corrected only when its
  # denominator cell is empty (FP for LR+, TN for LR-); an empty numerator gives exactly 0.
  expect_match(notes, "corrected only when the cell in its denominator is zero", fixed = TRUE)
  expect_match(notes, "(FP for LR+, TN for LR-, FP or FN for the odds ratio)", fixed = TRUE)
  expect_match(notes, "A zero numerator gives exactly 0, uncorrected", fixed = TRUE)
  df <- r$combinationTable$asDF
  expect_equal(df$lrNeg[df$rowType == "Single test"], .1)
})
