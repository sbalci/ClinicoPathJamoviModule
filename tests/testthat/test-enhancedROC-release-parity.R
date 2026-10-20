test_that("installed ROC wrappers agree with an independent pair-count AUC", {
  # Fixed overlapping scores avoid perfect-separation warnings.
  d <- data.frame(y = factor(rep(c("N", "P"), each = 40)),
                  score = c(seq(0, .8, length.out = 40), seq(.2, 1, length.out = 40)))
  neg <- d$score[d$y == "N"]
  pos <- d$score[d$y == "P"]
  differences <- outer(pos, neg, "-")
  expected <- mean((differences > 0) + .5 * (differences == 0))
  e <- suppressWarnings(enhancedROC(data = d, outcome = "y", positiveClass = "P",
                                     predictors = "score", direction = "higher"))
  expect_equal(e$results$aucSummary$asDF$auc[1], expected)
  ns <- environment(enhancedROC)
  # enhancedROC also ships independently in JamoviTest while under repair.
  if (exists("psychopdaROC", envir = ns, inherits = FALSE)) {
    p <- suppressWarnings(ns$psychopdaROC(data = d, classVar = "y", positiveClass = "P",
      negativeClass = "N", dependentVars = "score", direction = ">=", run = TRUE))
    expect_equal(p$aucSummaryTable$asDF$auc[1], expected)
  }
})
