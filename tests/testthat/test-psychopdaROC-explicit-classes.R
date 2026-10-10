# Regression coverage for explicit binary outcomes and name-independent pair identities.
.classes_data <- function() {
  set.seed(881)
  outcome <- rep(c("control", "case", "unselected"), each = 40)
  data.frame(outcome = factor(outcome),
    m1 = rnorm(120, rep(c(0, 0.8, 4), each = 40)),
    m2 = rnorm(120, rep(c(0, 0.5, -4), each = 40)),
    m3 = rnorm(120, rep(c(0, -0.5, 3), each = 40)),
    m4 = rnorm(120, rep(c(0, -0.8, -3), each = 40)))
}
.classes_run <- function(data = .classes_data(), ...) {
  args <- utils::modifyList(list(data = data, dependentVars = c("m1", "m2"),
    classVar = "outcome", positiveClass = "case", negativeClass = "control",
    refVar = NULL, seed = 88), list(...), keep.null = TRUE)
  if (!("run" %in% names(args))) args$run <- TRUE
  suppressMessages(suppressWarnings(do.call(ClinicoPath::psychopdaROC, args)))
}
.classes_capture <- function(result, array, renderer, key = NULL) {
  if (is.null(key)) key <- result[[array]]$itemKeys[[1]]
  f <- result$analysis$.__enclos_env__$private[[renderer]]
  env <- new.env(parent = environment(f))
  plotted <- NULL
  env$print <- function(x, ...) { plotted <<- x; invisible(x) }
  environment(f) <- env
  expect_true(f(result[[array]]$get(key), ggplot2::theme_minimal(), list(palette = "jmv")))
  plotted
}

test_that("both classes require explicit distinct observed selections", {
  schema <- yaml::read_yaml(test_path("..", "..", "jamovi", "psychopdaROC.a.yaml"))
  for (name in c("positiveClass", "negativeClass")) {
    option <- Filter(function(x) identical(x$name, name), schema$options)[[1]]
    expect_identical(option$type, "Level")
    expect_true(option$allowNone)
    expect_null(option$default)
    args <- setNames(list(NULL), name)
    result <- do.call(.classes_run, args)
    expect_equal(result$aucSummaryTable$rowCount, 0L)
    expect_match(result$runSummary$content, "both are selected", fixed = TRUE)
  }
  expect_error(.classes_run(negativeClass = "case"), "must be different observed levels")
  expect_error(.classes_run(negativeClass = "absent"), "Negative Class is set to absent")
  expect_error(.classes_run(positiveClass = "absent"), "Positive Class is set to absent")
  d <- .classes_data(); d$outcome <- as.integer(d$outcome == "case")
  result <- .classes_run(d, positiveClass = NULL, negativeClass = NULL)
  expect_equal(result$aucSummaryTable$rowCount, 0L)
})

test_that("all optional statistics use exactly the selected pair", {
  d <- .classes_data()
  d$outcome <- as.character(d$outcome)
  d$outcome[c(119, 120)] <- c(NA, "")
  options <- list(dependentVars = c("m1", "m2", "m3"), delongTest = TRUE,
    partialAUC = TRUE, bootstrapCI = TRUE, bootstrapReps = 100,
    effectSizeAnalysis = TRUE, powerAnalysis = TRUE, bayesianAnalysis = TRUE,
    clinicalUtilityAnalysis = TRUE,
    calculateIDI = TRUE, calculateNRI = TRUE, idiNriBootRuns = 100,
    compareClassifiers = TRUE, metaAnalysis = TRUE, overrideMetaAnalysisWarning = TRUE)
  full <- do.call(.classes_run, c(list(data = d), options))
  selected <- do.call(.classes_run, c(list(data = d[d$outcome %in% c("case", "control"), ]), options))
  for (name in c("aucSummaryTable", "delongComparisonTable", "partialAUCTable",
    "bootstrapCITable", "effectSizeTable", "powerAnalysisTable", "bayesianROCTable",
    "clinicalUtilityTable", "decisionCurveTable", "idiTable", "nriTable",
    "rocComparisonTable", "metaAnalysisTable")) {
    expect_true(full[[name]]$rowCount > 0, info = name)
    expect_equal(full[[name]]$asDF, selected[[name]]$asDF, tolerance = 1e-12, info = name)
  }
  coding <- full$outcomeCoding$asDF
  expect_equal(coding$n[coding$role == "Excluded: unselected outcome level"], 38)
  expect_equal(coding$n[coding$role == "Excluded: missing outcome"], 2)
  expect_match(full$runSummary$content, "50", fixed = TRUE)
})

test_that("literal sentinel names and punctuation are ordinary outcome labels", {
  d <- .classes_data()
  original <- .classes_run(d, partialAUC = TRUE, delongTest = TRUE)
  d$outcome <- as.character(d$outcome)
  d$outcome[d$outcome == "case"] <- ".__rest__"
  d$outcome[d$outcome == "control"] <- "control [< & >]"
  renamed <- .classes_run(d, positiveClass = ".__rest__", negativeClass = "control [< & >]",
    partialAUC = TRUE, delongTest = TRUE)
  expect_equal(renamed$aucSummaryTable$asDF, original$aucSummaryTable$asDF)
  expect_equal(renamed$partialAUCTable$asDF, original$partialAUCTable$asDF)
  expect_equal(renamed$delongComparisonTable$asDF, original$delongComparisonTable$asDF)
})

test_that("renaming markers cannot change comparisons, export or plotted effects", {
  d <- .classes_data()
  original <- .classes_run(d, dependentVars = paste0("m", 1:4),
    delongTest = TRUE, effectSizeAnalysis = TRUE)
  for (names in list(c("a", "b vs. c", "a vs. b", "c"),
    c("a", "b_vs_c", "a_vs_b", "c"), c('a"', "b\\c", 'a" vs. b\\c', "c"))) {
    renamed <- d; colnames(renamed)[-1] <- names
    result <- .classes_run(renamed, dependentVars = names,
      delongTest = TRUE, effectSizeAnalysis = TRUE)
    expect_false(result$analysis$.__enclos_env__$private$.delongUsedFallback)
    for (table in c("delongComparisonTable", "effectSizeTable")) {
      expect_length(unique(result[[table]]$rowKeys), 6L)
      observed <- result[[table]]$asDF
      expect_length(unique(observed$comparison), 6L)
      expect_equal(observed[-1], original[[table]]$asDF[-1], tolerance = 1e-12)
      expect_equal(unserialize(serialize(observed, NULL)), observed)
    }
    plot <- .classes_capture(result, "effectSizePlot", ".plotEffectSize")
    expect_equal(plot$data$cohens_d, result$effectSizeTable$asDF$cohens_d)
    expect_length(unique(plot$data$comparison), 6L)
    expect_s3_class(ggplot2::ggplot_build(plot), "ggplot_built")
  }
})

test_that("fallback DeLong agrees with the primary path in either direction", {
  d <- .classes_data(); d <- d[d$outcome %in% c("case", "control"), ]
  for (direction in c(">=", "<=")) {
    result <- .classes_run(d, direction = direction)
    p <- result$analysis$.__enclos_env__$private
    for (reference in list(NULL, 2L)) {
      primary <- p$.enhancedDelongTest(d[-1], d$outcome, "case", ref = reference)
      fallback <- p$.deLongTest(d[-1], d$outcome, "case", ref = reference)
      expect_equal(fallback$AUC$AUC, primary$AUC$AUC, tolerance = 1e-12)
      expect_equal(fallback$difference, primary$difference[names(fallback$difference)],
        tolerance = 1e-12)
      expect_equal(fallback$global.p, primary$global.p, tolerance = 1e-12)
      expect_equal(fallback$comparison_labels, primary$comparison_labels)
    }
  }
})

test_that("post-hoc power reports and plots the exact effective target", {
  for (direction in c(">=", "<=")) {
    result <- .classes_run(dependentVars = "m1", powerAnalysis = TRUE,
      powerAnalysisType = "post_hoc", direction = direction, expectedAUCDifference = 0.1)
    observed <- result$aucSummaryTable$asDF$auc
    target <- result$powerAnalysisTable$asDF$target_effect_size
    expect_equal(target, abs(observed - 0.5), tolerance = 1e-12)
    state <- result$powerCurvePlot$get("m1")$state
    expect_equal(state$auc_target, observed, tolerance = 1e-12)
    plot <- .classes_capture(result, "powerCurvePlot", ".plotPowerCurves")
    expect_true(state$auc_target %in% plot$data$auc)
    expect_true(all(grepl("target", plot$data$curve[plot$data$auc == state$auc_target])))
    expect_match(plot$labels$title, "different from 0.5", fixed = TRUE)
  }
})

test_that("changing the negative class recomputes and clearing it removes results", {
  result <- .classes_run(plotROC = TRUE, combinePlots = FALSE)
  previous <- result$aucSummaryTable$asDF$auc
  option <- result$analysis$options$option("negativeClass")
  option$value <- "unselected"
  suppressMessages(suppressWarnings(result$analysis$run()))
  reference <- .classes_run(negativeClass = "unselected")
  expect_equal(result$aucSummaryTable$asDF, reference$aucSummaryTable$asDF)
  expect_false(isTRUE(all.equal(result$aucSummaryTable$asDF$auc, previous)))
  option$value <- NULL
  result$analysis$run()
  expect_equal(result$aucSummaryTable$rowCount, 0L)
  expect_match(result$runSummary$content, "both are selected", fixed = TRUE)
  image <- result$plotROC$get("m1")
  expect_false(isTRUE(image$state$ready))
})

test_that("optional explanations do not hide essential scope notices", {
  result <- .classes_run()
  expect_false(result$procedureNotes$visible)
  expect_false(result$clinicalInterpretationTable$visible)
  notices <- Filter(function(x) inherits(x, "Notice") && isTRUE(x$visible), result$items)
  expect_gt(length(notices), 0L)
  explained <- .classes_run(showExplanations = TRUE)
  expect_true(explained$procedureNotes$visible)
  expect_true(explained$clinicalInterpretationTable$visible)
  expect_equal(explained$aucSummaryTable$asDF, result$aucSummaryTable$asDF)
})

test_that("negative class changes invalidate every cached statistical plot", {
  schema <- yaml::read_yaml(test_path("..", "..", "jamovi", "psychopdaROC.r.yaml"))
  for (item in schema$items) {
    array_image <- identical(item$type, "Array") && identical(item$template$type, "Image")
    if (!array_image && !identical(item$type, "Image")) next
    results <- ClinicoPath:::psychopdaROCResults$new(ClinicoPath:::psychopdaROCOptions$new())
    image <- results[[item$name]]
    if (array_image) { image$addItem(key = "test"); image <- image$get("test") }
    image$fromProtoBuf(list(state = raw(), image = list(path = "old.png")),
      oChanges = "negativeClass", vChanges = character())
    expect_null(image$.__enclos_env__$private$.filePath, info = item$name)
  }
})

test_that("unavailable DeLong inference uses missing cells for native serialization", {
  d <- .classes_data()
  d$m2 <- d$m1
  result <- .classes_run(d, delongTest = TRUE)
  expect_equal(result$delongComparisonTable$asDF$auc_diff, 0)
  for (column in c("ci_lower", "ci_upper", "z", "p")) {
    # A logical NA becomes a protobuf MISSING cell; numeric NA is rendered as NaN.
    expect_identical(result$delongComparisonTable$getCell(
      rowKey = "pair_1_2", col = column)$value, NA)
  }
})
