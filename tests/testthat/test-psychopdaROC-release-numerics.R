.releaseROC_data <- function() data.frame(
  outcome = factor(rep(c("negative", "positive"), each = 10)),
  marker = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9))

.releaseROC_run <- function(data = .releaseROC_data()) {
  suppressMessages(ClinicoPath::psychopdaROC(data = data, dependentVars = "marker",
    classVar = "outcome", positiveClass = "positive", negativeClass = "negative",
    refVar = NULL, clinicalUtilityAnalysis = TRUE, treatmentThreshold = "0.1,0.9,0.2"))
}

test_that("decision curves are invariant to affine changes of marker units", {
  d <- .releaseROC_data()
  y <- as.integer(d$outcome == "positive")
  # Independent reference: fit the original well-scaled marker, then count each decision.
  risk <- stats::fitted(stats::glm(y ~ marker, data = d, family = stats::binomial()))
  threshold <- seq(0.1, 0.9, by = 0.2)
  reference <- vapply(threshold, function(t) {
    treated <- risk >= t
    mean(treated & y == 1) - mean(treated & y == 0) * t / (1 - t)
  }, numeric(1))
  expect_equal(reference[3], 0.1)

  original <- .releaseROC_run(d)
  for (marker in list(d$marker + 1e12, d$marker * 1e-12, d$marker * 1e12)) {
    transformed <- d
    transformed$marker <- marker
    result <- .releaseROC_run(transformed)
    expect_equal(result$aucSummaryTable$asDF, original$aucSummaryTable$asDF)
    expect_equal(result$decisionCurveTable$asDF$net_benefit, reference, tolerance = 1e-12)
    expect_equal(result$clinicalUtilityTable$asDF, original$clinicalUtilityTable$asDF)
    state <- result$decisionCurvePlot$get(key = "marker")$state
    expect_equal(state$risk, as.numeric(risk), tolerance = 1e-8)
    expect_equal(state$threshold_range, threshold)
  }
})

test_that("decision-curve fitting warnings reach native notices and clear on recovery", {
  d <- .releaseROC_data()
  options <- ClinicoPath:::psychopdaROCOptions$new(dependentVars = "marker",
    classVar = "outcome", positiveClass = "positive", negativeClass = "negative",
    refVar = NULL, clinicalUtilityAnalysis = TRUE)
  analysis <- ClinicoPath:::psychopdaROCClass$new(options = options, data = d)
  analysis$init()
  private <- analysis$.__enclos_env__$private
  original_glm <- stats::glm
  local({
    testthat::local_mocked_bindings(glm = function(...) {
      warning("fitted probabilities numerically 0 or 1 occurred", call. = FALSE)
      original_glm(...)
    }, .package = "stats")
    expect_no_warning(suppressMessages(private$.run()))
  })
  expect_gt(analysis$results$decisionCurveTable$rowCount, 0)
  expect_match(analysis$results$runSummary$content, "net benefit may be unreliable", fixed = TRUE)
  notices <- Filter(function(x) inherits(x, "Notice") && isTRUE(x$visible),
    analysis$results$items)
  expect_true(any(vapply(notices, function(x)
    grepl("net benefit may be unreliable", x$content, fixed = TRUE), logical(1))))
  for (table in list(analysis$results$clinicalUtilityTable, analysis$results$decisionCurveTable))
    expect_true("fit_marker" %in% names(table$.__enclos_env__$private$.notes))

  suppressMessages(private$.run())
  expect_false(grepl("net benefit may be unreliable", analysis$results$runSummary$content,
    fixed = TRUE))
  for (table in list(analysis$results$clinicalUtilityTable, analysis$results$decisionCurveTable))
    expect_false("fit_marker" %in% names(table$.__enclos_env__$private$.notes))
})

test_that("a failed clinical-risk fit is disclosed instead of silently dropping its curve", {
  options <- ClinicoPath:::psychopdaROCOptions$new(dependentVars = "marker",
    classVar = "outcome", positiveClass = "positive", negativeClass = "negative",
    refVar = NULL, clinicalUtilityAnalysis = TRUE)
  analysis <- ClinicoPath:::psychopdaROCClass$new(options = options, data = .releaseROC_data())
  analysis$init()
  private <- analysis$.__enclos_env__$private
  suppressMessages(private$.run())
  expect_length(analysis$results$decisionCurvePlot$itemKeys, 1)
  local({
    testthat::local_mocked_bindings(glm = function(...) stop("fit unavailable"), .package = "stats")
    expect_no_error(suppressMessages(private$.run()))
  })
  result <- analysis$results
  expect_gt(result$aucSummaryTable$rowCount, 0)
  expect_equal(result$clinicalUtilityTable$rowCount, 0)
  expect_equal(result$decisionCurveTable$rowCount, 0)
  expect_length(result$decisionCurvePlot$itemKeys, 0)
  expect_match(result$runSummary$content, "Clinical utility analysis", fixed = TRUE)
  expect_match(result$runSummary$content, "fit unavailable", fixed = TRUE)
  suppressMessages(private$.run())
  expect_gt(result$decisionCurveTable$rowCount, 0)
  expect_length(result$decisionCurvePlot$itemKeys, 1)
  expect_false(grepl("fit unavailable", result$runSummary$content, fixed = TRUE))
})

test_that("multiple DeLong comparisons retain raw inference and add Holm adjustment", {
  d <- .releaseROC_data()
  d$other <- d$marker + rep(c(-2, 1, 2, -1), 5)
  d$third <- rev(d$marker)
  vars <- c("marker", "other", "third")
  result <- suppressMessages(ClinicoPath::psychopdaROC(data = d,
    dependentVars = c("marker", "other", "third"),
    classVar = "outcome", positiveClass = "positive", negativeClass = "negative",
    refVar = NULL, delongTest = TRUE))
  rocs <- lapply(vars, function(v) pROC::roc(d$outcome, d[[v]],
    levels = c("negative", "positive"), direction = "<", quiet = TRUE))
  expected <- apply(utils::combn(3, 2), 2, function(pair)
    pROC::roc.test(rocs[[pair[1]]], rocs[[pair[2]]], method = "delong")$p.value)
  expect_equal(result$delongComparisonTable$asDF$p, expected, tolerance = 1e-12)
  note <- result$delongComparisonTable$.__enclos_env__$private$.notes$multiplicity$note
  expect_match(note, "Holm-adjusted pairwise p-values", fixed = TRUE)
  expect_match(result$delongTest$content, "Holm-adjusted pairwise p-values", fixed = TRUE)
})
