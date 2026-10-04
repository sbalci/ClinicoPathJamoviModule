# Oracle counts: independent determinate counts published in the thyroid FNA study.
comparator_fixture <- function() {
  data.frame(gold = factor(c(rep("+", 171), rep("-", 80))),
    test = factor(c(rep("+", 108), rep("-", 63), rep("+", 13), rep("-", 67))))
}
comparator_decision <- function(data = comparator_fixture(), ...) {
  do.call(ClinicoPath::decision, c(list(data = data, gold = "gold", goldPositive = "+",
    goldNegative = "-", newtest = "test", testPositive = "+", testNegative = "-"), list(...)))
}

test_that("binary consensus metrics preserve denominators and distinguish symmetric agreement", {
  r <- comparator_decision(referenceType = "consensus", ci = TRUE, pp = TRUE, pprob = .1,
    showMisclassified = TRUE)
  m <- r$comparatorMetrics$asDF
  expect_equal(m$numerator, c(108, 67, 175, 108, 67, 216, 134))
  expect_equal(m$denominator, c(171, 80, 251, 121, 130, 292, 210))
  expect_equal(m$lower[1], unname(binom.test(108, 171)$conf.int[1]))
  expect_true(all(is.na(m$lower[6:7])))
  expect_false(r$ratioTable$visible)
  expect_false(r$plot1$visible)
  expect_match(r$comparatorReport$content, "PPA 63.2%", fixed = TRUE)
  expect_false(grepl("probability of disease|sensitivity|specificity", r$comparatorReport$content))
  expect_true(all(r$comparatorCases$asDF$classification %in%
    c("Positive call / negative reference", "Negative call / positive reference")))
})

test_that("directional agreement changes on reversal but symmetric agreement does not", {
  d <- comparator_fixture()
  a <- ClinicoPath:::.diagComparatorMetrics(d$test == "+", d$gold == "+")$metrics
  b <- ClinicoPath:::.diagComparatorMetrics(d$gold == "+", d$test == "+")$metrics
  expect_equal(a$estimate[1], 108 / 171)
  expect_equal(b$estimate[1], 108 / 121)
  expect_equal(a$estimate[6:7], b$estimate[6:7])
  expect_equal(ClinicoPath:::.diagComparatorMetrics(c(TRUE, FALSE), c(FALSE, FALSE))$
    metrics$denominator[1], 0)
})

test_that("explicit disease and legacy retain existing numerical outputs", {
  a <- comparator_decision(referenceType = "legacy", ci = TRUE)
  b <- comparator_decision(referenceType = "disease", ci = TRUE)
  expect_equal(a$ratioTable$asDF, b$ratioTable$asDF)
  expect_equal(a$ratioTable$asDF$Sens[1], 108 / 171)
  expect_false(a$comparatorMetrics$visible)
})

test_that("comparison uses common pairs, Holm family and neutral plot states", {
  d <- data.frame(gold = factor(c(rep("+", 6), rep("-", 6), "+", NA)),
    x = factor(c("+", "+", "+", "+", "+", "-", "-", "-", "-", "-", "+", "+", NA, "+")),
    y = factor(c("+", "+", "+", "-", "-", "-", "-", "-", "-", "-", "-", "+", "+", "-")))
  args <- list(data = d, gold = "gold", goldPositive = "+", goldNegative = "-",
    test1 = "x", test1Positive = "+", test1Negative = "-", test2 = "y",
    test2Positive = "+", test2Negative = "-", test3 = NULL, test3Positive = NULL,
    test3Negative = NULL, referenceType = "method", statComp = TRUE, ci = TRUE,
    plot = TRUE, heatmap = TRUE, radarplot = TRUE)
  r <- do.call(ClinicoPath::decisioncompare, args)
  expect_equal(r$comparatorPopulation$asDF$analyzed, c(12, 13))
  m <- r$comparatorMetrics$asDF
  expect_equal(m$estimate[c(1, 2, 8, 9)], c(5/6, 4/6, 4/7, 5/6))
  dif <- r$comparatorDifferences$asDF
  expect_equal(dif$n, c(6, 6, 12))
  expect_equal(dif$diff, c(2/6, -1/6, 1/12))
  expect_equal(dif$p, c(.25, .5, .625))
  expect_equal(dif$adjusted, p.adjust(c(.25, .5, .625), "holm"))
  expect_false(r$comparisonTable$visible)
  expect_true(all(r$comparatorPlot$state$data$metric %in% c("PPA", "NPA", "OPA")))
  expect_false(any(grepl("True|False", r$comparatorHeatmap$state$data$result)))
})

test_that("unselected levels and missingness never become nondiagnostic binary counts", {
  x <- factor(c("+", "-", "ND", NA), exclude = NULL)
  expect_identical(ClinicoPath:::.diagComparatorBinary(x, "+", "-", TRUE),
    c(TRUE, FALSE, NA, NA))
  expect_error(ClinicoPath:::.diagComparatorBinary(x, "+", "-", FALSE),
    "outside the selected positive/negative pair")
  expect_error(ClinicoPath:::.diagComparatorBinary(x, "+", "+", TRUE), "different")
})

test_that("mode changes clear disease plots and save aligned comparator classifications", {
  d <- comparator_fixture()
  rownames(d) <- seq_len(nrow(d)) * 2L
  opts <- ClinicoPath:::decisionOptions$new(gold = "gold", goldPositive = "+",
    goldNegative = "-", newtest = "test", testPositive = "+", testNegative = "-",
    referenceType = "disease", fagan = TRUE, showNaturalLanguage = TRUE)
  output <- opts$option("saveClassifications")
  output$value <- list(value = TRUE)
  analysis <- ClinicoPath:::decisionClass$new(options = opts, data = d)
  analysis$run()
  expect_false(is.null(analysis$results$plot1$state))
  mode <- opts$option("referenceType")
  mode$value <- "consensus"
  opts$.__enclos_env__$private$.env$referenceType <- "consensus"
  analysis$optionsChangedHandler("referenceType")
  analysis$run()
  expect_false(opts$eval("(referenceType:legacy && fagan || referenceType:disease && fagan)"))
  expect_null(analysis$results$plot1$state)
  expect_true(is.na(analysis$results$ratioTable$asDF$Sens[1]))
  expect_identical(analysis$results$naturalLanguageSummary$content, "")
  saved <- analysis$results$saveClassifications$.__enclos_env__$private
  expect_equal(saved$.rowNums, seq_len(nrow(d)) * 2L)
  expect_identical(as.character(saved$.values[[1]]), c(rep("Positive match", 108),
    rep("Negative call / positive reference", 63),
    rep("Positive call / negative reference", 13), rep("Negative match", 67)))
  mode$value <- "disease"
  opts$.__enclos_env__$private$.env$referenceType <- "disease"
  analysis$optionsChangedHandler("referenceType")
  analysis$run()
  expect_length(analysis$results$comparatorMetrics$getRows(), 0L)
  expect_equal(analysis$results$ratioTable$asDF$Sens[1], 108/171)
})

test_that("comparator plot renderers export from neutral cached states", {
  d <- comparator_fixture()
  d$second <- rev(d$test)
  opts <- ClinicoPath:::decisioncompareOptions$new(gold = "gold", goldPositive = "+",
    goldNegative = "-", test1 = "test", test1Positive = "+", test1Negative = "-",
    test2 = "second", test2Positive = "+", test2Negative = "-",
    referenceType = "consensus", plot = TRUE, radarplot = TRUE, heatmap = TRUE)
  a <- ClinicoPath:::decisioncompareClass$new(options = opts, data = d)
  a$run()
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit({ grDevices::dev.off(); unlink(file) }, add = TRUE)
  p <- a$.__enclos_env__$private
  expect_true(p$.plotComparatorBar(a$results$comparatorPlot, ggplot2::theme_minimal()))
  expect_true(p$.plotComparatorRadar(a$results$comparatorRadar, ggplot2::theme_minimal()))
  expect_true(p$.plotComparatorHeatmap(a$results$comparatorHeatmap, ggplot2::theme_minimal()))
  mode <- opts$option("referenceType")
  mode$value <- "disease"
  opts$.__enclos_env__$private$.env$referenceType <- "disease"
  a$optionsChangedHandler("referenceType")
  a$run()
  expect_null(a$results$comparatorPlot$state)
  expect_null(a$results$comparatorRadar$state)
  expect_null(a$results$comparatorHeatmap$state)
  expect_identical(a$results$comparatorReport$content, "")
})

test_that("ambiguous comparator levels request an explicit binary mapping", {
  d <- comparator_fixture()
  d$gold <- as.character(d$gold)
  d <- rbind(d, data.frame(gold = "Unclassified", test = "+"))
  r <- ClinicoPath::decision(data = d, gold = "gold", goldPositive = "+",
    goldNegative = NULL, newtest = "test", testPositive = "+", testNegative = "-",
    referenceType = "consensus")
  expect_equal(r$comparatorMetrics$rowCount, 0)
  expect_match(r$notices$content, "reference-negative level", ignore.case = TRUE)
  expect_false(grepl("disease-absent|estimate specificity", r$notices$content))
})
