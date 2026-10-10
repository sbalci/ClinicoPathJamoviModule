.relROC_data <- function() data.frame(
  status = factor(rep(c("negative", "positive"), each = 6)),
  marker = c(-2, 0, 1, 1, 3, 7, -1, 1, 2, 4, 4, 8))

.relROC_run <- function(data = .relROC_data(), ...) {
  args <- utils::modifyList(list(data = data, dependentVars = "marker", classVar = "status",
    positiveClass = "positive", negativeClass = "negative", refVar = NULL), list(...), keep.null = TRUE)
  if (!("run" %in% names(args))) args$run <- TRUE
  suppressMessages(suppressWarnings(do.call(ClinicoPath::psychopdaROC, args)))
}

test_that("release profile leaves optional calculations and plots unchecked", {
  schema <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "psychopdaROC.a.yaml"))
  checkboxes <- Filter(function(x) x$type == "Bool", schema$options)
  options <- ClinicoPath:::psychopdaROCOptions$new()
  expect_true(all(vapply(checkboxes, function(x) identical(x$default, FALSE), logical(1))))
  expect_true(all(vapply(checkboxes, function(x)
    identical(options$option(x$name)$value, FALSE), logical(1))))
  result <- .relROC_run()
  expect_equal(nrow(result$aucSummaryTable$asDF), 1)
  expect_false(isTRUE(result$plotROC$visible))
  expect_false(isTRUE(result$bootstrapCITable$visible))
  expect_false(isTRUE(result$delongComparisonTable$visible))
  expect_false(isTRUE(.relROC_run(interactiveROC = TRUE)$interactivePlot$visible))
  requested <- .relROC_run(plotROC = TRUE, combinePlots = FALSE, interactiveROC = TRUE,
    showOptimalPoint = TRUE, fixedSensSpecAnalysis = TRUE, showFixedROC = TRUE)
  expect_equal(requested$aucSummaryTable$asDF, result$aucSummaryTable$asDF)
  expect_gt(length(requested$plotROC$itemKeys), 0)
  expect_gt(length(requested$fixedSensSpecROC$itemKeys), 0)
  expect_true(isTRUE(requested$interactivePlot$visible))
})

test_that("release welcome explains the required mapping and clears after selection", {
  empty <- .relROC_run(data = data.frame(), dependentVars = NULL,
    classVar = NULL, positiveClass = NULL)
  expect_match(empty$instructions$content, "jmv-welcome", fixed = TRUE)
  expect_match(empty$instructions$content, "explicitly choose Positive Class", fixed = TRUE)
  expect_match(empty$instructions$content, "independent observation", fixed = TRUE)
  expect_match(empty$instructions$content, "checkboxes start unchecked", fixed = TRUE)
  expect_true(isTRUE(empty$instructions$visible))
  ready <- .relROC_run()
  expect_false(isTRUE(ready$instructions$visible))
})

test_that("diagnostic plots work with ROC curves unchecked", {
  for (combined in c(FALSE, TRUE)) {
    result <- .relROC_run(showCriterionPlot = TRUE, showPrevalencePlot = TRUE,
      showDotPlot = TRUE, combinePlots = combined)
    key <- if (combined) 1 else "marker"
    expect_false(isTRUE(result$plotROC$visible))
    expect_gt(nrow(result$criterionPlot$get(key = key)$state), 0)
    expect_gt(length(result$prevalencePlot$get(key = key)$state$optimal), 0)
    if (combined) {
      expect_match(result$dotPlotMessage$content, "aren't available in combined plot mode",
        fixed = TRUE)
    } else {
      expect_equal(nrow(result$dotPlot$get(key = key)$state), nrow(.relROC_data()))
    }
  }
})

test_that("dot plot cutpoint changes invalidate the saved image", {
  restore <- function(changed = character()) {
    results <- ClinicoPath:::psychopdaROCResults$new(ClinicoPath:::psychopdaROCOptions$new())
    results$dotPlot$addItem(key = "marker")
    image <- results$dotPlot$get(key = "marker")
    image$fromProtoBuf(list(state = raw(), image = list(path = "previous.png")),
      oChanges = changed, vChanges = character())
    image$.__enclos_env__$private$.filePath
  }
  expect_identical(restore(), "previous.png")
  for (changed in c("method", "metric", "specifyCutScore", "tol_metric", "break_ties",
    "costratioFP", "usePriorPrev", "priorPrev", "seed", "boot_runs", "showDotPlot")) {
    expect_null(restore(changed), info = changed)
  }
})

test_that("release name mapping preserves labelled factor levels without collisions", {
  d <- .relROC_data()
  d$other <- d$marker + c(0, 1, 0, 2, 0, 1, 0, 1, 0, 2, 0, 1)
  names(d) <- c("Outcome <gold>", "Tümör score (µg)", "Tümör_score_µg")
  d[[1]] <- factor(d[[1]], levels = c("positive", "negative", "unused"))
  labelled::var_label(d[[2]]) <- "Assay label with spaces"
  result <- .relROC_run(d, dependentVars = names(d)[2:3], classVar = names(d)[1])
  expect_setequal(result$aucSummaryTable$asDF$variable, names(d)[2:3])
  expect_equal(result$aucSummaryTable$asDF$auc[1], .relROC_run()$aucSummaryTable$asDF$auc)
  expect_setequal(result$outcomeCoding$asDF$level, c("positive", "negative"))
  expect_false(grepl("Outcome <gold>", result$procedureNotes$content, fixed = TRUE))
  expect_match(result$procedureNotes$content, "Outcome &lt;gold&gt;", fixed = TRUE)
})
