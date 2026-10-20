test_that("decision reports diagnostic measures from a synthetic 2 by 2 table", {
  # TP=40, FN=10, FP=6, TN=24: an arithmetic oracle, independent of epiR.
  d <- data.frame(gold = factor(rep(c("P", "P", "N", "N"), c(40, 10, 6, 24))),
                  test = factor(rep(c("P", "N", "P", "N"), c(40, 10, 6, 24))))
  r <- suppressWarnings(decision(data = d, gold = "gold", goldPositive = "P",
    goldNegative = "N", newtest = "test", testPositive = "P", testNegative = "N"))
  x <- r$ratioTable$asDF
  expect_equal(x$Sens, 40 / 50)
  expect_equal(x$Spec, 24 / 30)
  expect_equal(x$PPV, 40 / 46)
  expect_equal(x$NPV, 24 / 34)
  expect_equal(x$LRP, 4)
  expect_equal(x$LRN, .25)
})
