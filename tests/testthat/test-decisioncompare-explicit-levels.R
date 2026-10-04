# Independent counts; ND is a result status, not a negative binary call.
dc_explicit_data <- function() {
  data.frame(
    gold = factor(rep(c("P", "P", "N", "N", "P", "N"), c(108, 63, 13, 67, 9, 25))),
    a = factor(rep(c("P", "N", "P", "N", "ND", "ND"), c(108, 63, 13, 67, 9, 25))),
    b = factor(rep(c("P", "N", "P", "N", "ND", "ND"), c(108, 63, 13, 67, 9, 25))))
}

dc_explicit_analysis <- function(data = dc_explicit_data(), ...) {
  args <- utils::modifyList(list(gold = "gold", goldPositive = "P", goldNegative = "N",
    test1 = "a", test1Positive = "P", test1Negative = "N",
    test2 = "b", test2Positive = "P", test2Negative = "N"), list(...), keep.null = TRUE)
  options <- do.call(ClinicoPath:::decisioncompareOptions$new, args)
  ClinicoPath:::decisioncompareClass$new(options = options, data = data)
}

test_that("all reference modes require both levels even on binary data", {
  data <- dc_explicit_data()[1:251, ]
  for (mode in c("legacy", "disease", "consensus", "method")) {
    for (exclude in c(FALSE, TRUE)) {
      for (option in c("goldPositive", "goldNegative", "test1Positive", "test1Negative",
                       "test2Positive", "test2Negative")) {
        args <- c(list(data = data, referenceType = mode, excludeIndeterminate = exclude),
          stats::setNames(list(NULL), option))
        a <- do.call(dc_explicit_analysis, args)
        expect_error(a$run(), "Validation failed", info = paste(mode, exclude, option))
        expect_equal(a$results$comparisonTable$rowCount, 0)
        expect_equal(a$results$comparatorMetrics$rowCount, 0)
        expect_equal(a$results$levelMapping$rowCount, 0)
      }
    }
  }
})

test_that("extra observed levels stop every mode until exclusion is explicitly selected", {
  for (mode in c("legacy", "disease", "consensus", "method")) {
    blocked <- dc_explicit_analysis(referenceType = mode)
    expect_error(blocked$run(), "ND \\(n=34\\)")
    expect_equal(blocked$results$comparisonTable$rowCount, 0)
    expect_equal(blocked$results$comparatorMetrics$rowCount, 0)
    a <- dc_explicit_analysis(referenceType = mode, excludeIndeterminate = TRUE)
    expect_no_error(a$run())
    if (mode %in% c("consensus", "method")) {
      metrics <- a$results$comparatorMetrics$asDF
      expect_equal(metrics$numerator[1:3], c(108, 67, 175))
      expect_equal(metrics$denominator[1:3], c(171, 80, 251))
      expect_equal(a$results$comparatorPopulation$asDF$excluded, c(34, 34))
    } else {
      metrics <- a$results$comparisonTable$asDF
      metrics <- metrics[metrics$test %in% c("a", "b"), ]
      expect_equal(metrics$Sens, rep(108/171, 2))
      expect_equal(metrics$Spec, rep(67/80, 2))
      expect_equal(metrics$n, c(251, 251))
      expect_equal(metrics$excluded, c(34, 34))
    }
    mapping <- a$results$levelMapping$asDF
    expect_equal(mapping$otherN, c(0, 34, 34))
    expect_equal(mapping$negativeN, c(105, 130, 130))
    expect_match(a$results$notices$content, "Estimates Conditional on Selected Levels")
    expect_match(a$results$referenceDefinition$asDF$value[4], "negative = N")
  }
})

test_that("missing or stale negative selections cannot produce asymmetric policies", {
  for (mode in c("legacy", "disease", "consensus", "method")) {
    a <- dc_explicit_analysis(referenceType = mode, excludeIndeterminate = TRUE,
      test2Negative = NULL)
    expect_error(a$run(), "b: Select an explicit negative level")
    for (bad in c("P", "absent")) {
      a <- dc_explicit_analysis(referenceType = mode, test2Negative = bad,
        excludeIndeterminate = TRUE)
      expect_error(a$run(), "b:.*(different|not available)")
    }
  }
  data <- dc_explicit_data()[1:251, ]
  data$c <- data$b
  a <- dc_explicit_analysis(data, test1 = NULL, test1Positive = NULL, test1Negative = NULL,
    test3 = "c", test3Positive = "P", test3Negative = NULL)
  expect_error(a$run(), "c: Select an explicit negative level")
  a <- dc_explicit_analysis(data, test1 = NULL, test1Positive = NULL, test1Negative = NULL,
    test3 = "c", test3Positive = "P", test3Negative = "N")
  expect_no_error(a$run())
  expect_equal(a$results$comparisonTable$asDF$test[c(1, 3)], c("b", "c"))
})

test_that("unavailable reference states are never included as reference negatives", {
  data <- data.frame(gold = factor(rep(c("P", "P", "N", "N", "Unavailable"),
    c(10, 5, 5, 10, 10))), a = factor(rep(c("P", "N", "P", "N", "N"),
    c(10, 5, 5, 10, 10))))
  data$b <- data$a
  for (mode in c("legacy", "disease", "consensus", "method")) {
    a <- dc_explicit_analysis(data, referenceType = mode)
    expect_error(a$run(), "gold:.*Unavailable")
    a <- dc_explicit_analysis(data, referenceType = mode, excludeIndeterminate = TRUE)
    expect_no_error(a$run())
    if (mode %in% c("consensus", "method")) {
      expect_equal(a$results$comparatorMetrics$asDF$estimate[2], 10/15)
      expect_equal(a$results$comparatorPopulation$asDF$analyzed, c(30, 30))
    } else {
      tab <- a$results$comparisonTable$asDF
      tab <- tab[tab$test %in% c("a", "b"), ]
      expect_equal(tab$Spec, rep(10/15, 2))
      expect_equal(tab$n, c(30, 30))
    }
  }
})

test_that("missing, unused and ordinal levels have explicit semantics", {
  f <- ClinicoPath:::.diagComparatorBinary
  x <- factor(c("P", "N", "ND", "Missing", NA), exclude = NULL)
  expect_error(f(x, "P", "N"), "Missing.*ND")
  expect_identical(f(x, "P", "N", TRUE), c(TRUE, FALSE, NA, NA, NA))
  expect_error(f(factor(c("P", "ND")), "P"), "explicit negative")
  expect_error(f(x, "P", NA_character_), "explicit negative")
  expect_error(f(x, "P", c("N", "ND")), "explicit negative")
  unused <- factor(c("P", "N", NA), levels = c("P", "N", "ND"), exclude = NULL)
  expect_identical(f(unused, "P", "N"), c(TRUE, FALSE, NA))
  ordinal <- factor(c("II", "III", "IV", "V", "VI"), ordered = TRUE)
  expect_error(f(ordinal, "III", "II"), "outside the selected")
  expect_identical(f(ordinal, "III", "II", TRUE), c(FALSE, TRUE, NA, NA, NA))
})

test_that("paired comparison cannot reward abstention by turning it into a negative match", {
  data <- data.frame(gold = rep(c("N", "N", "P"), each = 10),
    a = rep(c("P", "N", "P"), each = 10), b = rep(c("ND", "N", "P"), each = 10))
  for (mode in c("consensus", "method")) {
    a <- dc_explicit_analysis(data, referenceType = mode, statComp = TRUE)
    expect_error(a$run(), "b:.*ND")
    expect_equal(a$results$comparatorDifferences$rowCount, 0)
    a <- dc_explicit_analysis(data, referenceType = mode, statComp = TRUE,
      excludeIndeterminate = TRUE)
    expect_no_error(a$run())
    dif <- a$results$comparatorDifferences$asDF
    expect_equal(dif$n, c(10, 10, 20))
    expect_equal(dif$diff, c(0, 0, 0))
    expect_equal(dif$p, c(1, 1, 1))
  }
})

test_that("invalid mapping reruns clear previous tables, plots and reports in every mode", {
  for (mode in c("legacy", "disease", "consensus", "method")) {
    a <- dc_explicit_analysis(referenceType = mode, excludeIndeterminate = TRUE,
      plot = TRUE, heatmap = TRUE, radarplot = TRUE, statComp = TRUE)
    a$run()
    expect_equal(a$results$levelMapping$rowCount, 3)
    options <- a$options
    option <- options$option("test2Negative")
    option$value <- NULL
    options$.__enclos_env__$private$.env$test2Negative <- NULL
    a$optionsChangedHandler("test2Negative")
    expect_error(a$run(), "b: Select an explicit negative")
    for (name in c("levelMapping", "comparisonTable", "comparatorMetrics",
                   "mcnemarTable", "diffTable", "comparatorDifferences"))
      expect_equal(a$results[[name]]$rowCount, 0)
    for (name in c("plot1", "plotRadar", "plotHeatmap", "comparatorPlot",
                   "comparatorRadar", "comparatorHeatmap"))
      expect_null(a$results[[name]]$state)
    expect_identical(a$results$comparatorReport$content, "")
    expect_match(a$results$notices$content, "b: Select an explicit negative")
  }
})

test_that("an NA positive selection produces an actionable validation error", {
  for (mode in c("legacy", "disease", "consensus", "method")) {
    a <- dc_explicit_analysis(referenceType = mode, test1Positive = NA_character_)
    expect_error(a$run(), "Validation failed")
    expect_match(a$results$notices$content, "Missing Positive Levels")
    expect_equal(a$results$levelMapping$rowCount, 0)
  }
})
