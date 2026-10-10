# Regressions for power estimands, prevalence labels, and jamovi execution state.
.applyROC_data <- function() {
  data.frame(outcome = factor(rep(c("negative", "positive"), each = 10)),
    marker = c(1:10, 4:13), reference = c(1:10, 4:13))
}

.applyROC_analysis <- function(data = .applyROC_data(), ...) {
  args <- utils::modifyList(list(dependentVars = "marker", classVar = "outcome",
    positiveClass = "positive", negativeClass = "negative", refVar = NULL, run = TRUE),
    list(...), keep.null = TRUE)
  options <- do.call(ClinicoPath:::psychopdaROCOptions$new, args)
  a <- ClinicoPath:::psychopdaROCClass$new(options = options, data = data)
  suppressMessages(suppressWarnings(a$run()))
  a
}

.applyROC_note <- function(table, key) {
  notes <- table$.__enclos_env__$private$.notes
  if (key %in% names(notes)) notes[[key]]$note else ""
}

.applyROC_plot <- function(a, array, renderer) {
  image <- a$results[[array]]$get(a$results[[array]]$itemKeys[[1]])
  render <- a$.__enclos_env__$private[[renderer]]
  captured <- NULL
  env <- new.env(parent = environment(render))
  env$print <- function(x, ...) { captured <<- x; invisible(x) }
  environment(render) <- env
  expect_true(render(image, ggplot2::theme_minimal(), list(palette = "jmv")))
  captured
}

test_that("single-AUC planning cannot gain power by adding another marker", {
  # Independent integration of the declared two-sided normal approximation.
  auc <- 0.6
  variance <- (auc * (1 - auc) + 9 * (auc / (2 - auc) - auc^2) +
    9 * (2 * auc^2 / (1 + auc) - auc^2)) / 100
  mean_z <- (auc - 0.5) / sqrt(variance)
  critical <- stats::qnorm(0.975)
  expected <- stats::integrate(function(z) stats::dnorm(z, mean_z),
    -Inf, -critical, rel.tol = 1e-12)$value +
    stats::integrate(function(z) stats::dnorm(z, mean_z),
      critical, Inf, rel.tol = 1e-12)$value
  for (type in c("prospective", "sample_size")) {
    single <- .applyROC_analysis(powerAnalysis = TRUE, powerAnalysisType = type)
    baseline <- single$results$powerAnalysisTable$asDF
    expect_equal(baseline$observed_power, expected, tolerance = 1e-10)
    expect_identical(baseline$required_n, 250L)
    for (correlation in c(-1, 0, 0.5, 0.99, 1)) {
      multiple <- .applyROC_analysis(dependentVars = c("marker", "reference"),
        powerAnalysis = TRUE, powerAnalysisType = type, correlationROCs = correlation)
      tab <- multiple$results$powerAnalysisTable$asDF
      expect_equal(tab$observed_power, rep(expected, 2), tolerance = 1e-10)
      expect_identical(tab$required_n, rep(250L, 2))
      expect_match(.applyROC_note(multiple$results$powerAnalysisTable, "approximation"),
        "Each marker is tested separately against AUC = 0.5", fixed = TRUE)
      plot <- .applyROC_plot(multiple, "powerCurvePlot", ".plotPowerCurves")
      target <- plot$data[plot$data$auc == auc, ]
      at_n <- target[target$n == 20, "power"]
      expect_equal(at_n, expected, tolerance = 1e-10)
    }
  }
})

test_that("target prevalence is identified in tables and the prevalence plot", {
  a <- .applyROC_analysis(usePriorPrev = TRUE, priorPrev = 0.01,
    fixedSensSpecAnalysis = TRUE, showPrevalencePlot = TRUE)
  for (table in list(a$results$resultsTable$get("marker"), a$results$fixedSensSpecTable)) {
    note <- .applyROC_note(table, "predictive_values_prevalence")
    expect_match(note, "user-specified target prevalence (1.0%)", fixed = TRUE)
    expect_false(grepl("this sample is strongly imbalanced", note, fixed = TRUE))
  }
  plot <- .applyROC_plot(a, "prevalencePlot", ".plotPrevalence")
  annotation <- tail(ggplot2::ggplot_build(plot)$data, 1)[[1]]
  expect_identical(annotation$label, "Target prevalence: 0.01")
  option <- a$options$option("usePriorPrev"); option$value <- FALSE
  suppressMessages(suppressWarnings(a$run()))
  expect_match(.applyROC_note(a$results$resultsTable$get("marker"),
    "predictive_values_prevalence"), "prevalence of this sample (50.0%)", fixed = TRUE)
  plot <- .applyROC_plot(a, "prevalencePlot", ".plotPrevalence")
  annotation <- tail(ggplot2::ggplot_build(plot)$data, 1)[[1]]
  expect_identical(annotation$label, "Sample prevalence: 0.50")
})

test_that("patient summaries treat blank and missing outcomes consistently", {
  d <- .applyROC_data()
  d$id <- seq_len(nrow(d))
  d <- rbind(d, d[1, ])
  d$outcome <- as.character(d$outcome)
  d$outcome[nrow(d)] <- ""
  blank <- .applyROC_analysis(d, patientID = "id", analysisUnit = "patientMean")
  d$outcome[nrow(d)] <- NA_character_
  missing <- .applyROC_analysis(d, patientID = "id", analysisUnit = "patientMean")
  expect_equal(blank$results$aucSummaryTable$asDF, missing$results$aucSummaryTable$asDF)
  expect_equal(blank$results$resultsTable$get("marker")$asDF,
    missing$results$resultsTable$get("marker")$asDF)
  d$outcome[nrow(d)] <- "third level"
  expect_error(.applyROC_analysis(d, patientID = "id", analysisUnit = "patientMean"),
    "conflicting outcome")
})

test_that("exported syntax still runs after jamovi releases its dataset", {
  d <- .applyROC_data()[rep(seq_len(20), 501), ]
  a <- .applyROC_analysis(d, run = FALSE)
  expect_equal(a$results$aucSummaryTable$rowCount, 0)
  a$.__enclos_env__$private$.data <- NULL
  code <- parse(text = a$asSource())[[1]]
  expect_true(as.list(code)$run)
  result <- suppressMessages(eval(code, envir = list(data = d)))
  expect_equal(result$aucSummaryTable$rowCount, 1)
})

test_that("Run invalidates saved image files for every ROC output", {
  results <- ClinicoPath:::psychopdaROCResults$new(ClinicoPath:::psychopdaROCOptions$new())
  for (item in results$items) {
    if (inherits(item, "Array")) {
      item$addItem(key = "probe")
      item <- item$get("probe")
    }
    if (!inherits(item, "Image")) next
    payload <- list(state = raw(), image = list(path = "previous.png"))
    item$fromProtoBuf(payload, oChanges = character(), vChanges = character())
    expect_identical(item$.__enclos_env__$private$.filePath, "previous.png")
    item$.__enclos_env__$private$.filePath <- NULL
    item$fromProtoBuf(payload, oChanges = "run", vChanges = character())
    expect_null(item$.__enclos_env__$private$.filePath, info = item$name)
  }
})

test_that("a hidden ROC plot never refits a saved smoothing overlay", {
  a <- .applyROC_analysis(plotROC = FALSE, smoothing = TRUE, run = FALSE)
  image <- a$results$plotROC$get("marker")
  saved <- data.frame(var = "marker", sensitivity = c(0, 0.5, 1),
    specificity = c(1, 0.5, 0))
  image$setState(saved)
  calls <- 0L
  testthat::local_mocked_bindings(ggplot_build = function(...) {
    calls <<- calls + 1L
    stop("Unexpected hidden smoothing fit")
  }, .package = "ggplot2")
  a$.__enclos_env__$private$.attachRocOverlayState()
  expect_equal(calls, 0L)
  expect_identical(image$state, saved)
})
