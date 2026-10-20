test_that("combination notes describe selective correction without changing the estimator", {
  d <- data.frame(y = factor(rep(c("P", "N"), c(20, 30))),
                  a = factor(rep(c("P", "N", "N"), c(18, 2, 30))))
  r <- decisioncombine(data = d, gold = "y", goldPositive = "P",
                       test1 = "a", test1Positive = "P",
                       test2Positive = NULL, test3Positive = NULL)
  notes <- paste(vapply(r$combinationTable$notes, function(x) x$note, ""), collapse = " ")
  expect_match(notes, "LR+ only when TP or FP is zero", fixed = TRUE)
  expect_match(notes, "LR- only when FN or TN is zero", fixed = TRUE)
  expect_match(notes, "need not equal", fixed = TRUE)
  df <- r$combinationTable$asDF
  expect_equal(df$lrNeg[df$rowType == "Single test"], .1)
})
