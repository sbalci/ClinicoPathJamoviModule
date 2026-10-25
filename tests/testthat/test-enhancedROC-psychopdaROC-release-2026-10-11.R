source(testthat::test_path("helper-roc-release-oracles.R"), local = TRUE)

.dualRunE <- function(d, direction = "higher", ...) suppressMessages(
  ClinicoPath::enhancedROC(validationNegative = NULL, data = d, outcome = "disease", positiveClass = "Case",
    predictors = "score1", direction = direction, rocCurve = FALSE,
    detectImbalance = TRUE, ...))
.dualRunP <- function(d, direction = "higher", ...) suppressMessages(
  ClinicoPath::psychopdaROC(data = d, classVar = "disease", positiveClass = "Case",
    negativeClass = "Control", dependentVars = "score1", refVar = NULL, run = TRUE,
    direction = if (direction == "higher") ">=" else "<=", compareClassifiers = TRUE, ...))

test_that("both ROC analyses use interpolated PR area on ties and constant scores", {
  for (f in .dualROCOracle$fixtures()) {
    expected <- .dualROCOracle$pr(f$data$disease, f$data$score1, f$direction)
    e <- .dualRunE(f$data, f$direction)
    p <- .dualRunP(f$data, f$direction)
    expect_equal(e$results$precisionRecallTable$asDF$auc_pr, expected$interpolated,
      tolerance = 1e-11, info = f$description)
    expect_equal(p$rocComparisonTable$asDF$auprc, expected$interpolated,
      tolerance = 1e-11, info = f$description)
    expect_equal(e$results$precisionRecallTable$asDF$average_precision,
      expected$average_precision, tolerance = 1e-11)
  }
  d <- data.frame(disease = factor(c(rep("Control", 90), rep("Case", 10))), score1 = 2)
  expect_equal(.dualRunE(d)$results$precisionRecallTable$asDF$auc_pr, .1)
  expect_equal(.dualRunP(d)$rocComparisonTable$asDF$auprc, .1)
})

test_that("matched DeLong intervals agree while default logit intervals retain their estimand", {
  d <- .dualROCOracle$fixtures()$balanced_complete_higher$data
  truth <- .dualROCOracle$auc(d$disease, d$score1)
  e <- .dualRunE(d)$results$aucSummary$asDF
  p <- .dualRunP(d, aucInference = "delong")$aucSummaryTable$asDF
  expect_equal(c(e$auc, e$std_error, e$auc_lower, e$auc_upper),
    unname(truth[c("auc", "se", "ci_lower", "ci_upper")]), tolerance = 1e-11)
  expect_equal(c(p$auc, p$ci_lower, p$ci_upper),
    unname(truth[c("auc", "ci_lower", "ci_upper")]), tolerance = 1e-11)
  logit <- .dualRunP(d)$aucSummaryTable$asDF
  expected <- plogis(qlogis(truth["auc"]) + c(-1, 1) * qnorm(.975) *
    truth["se"] / (truth["auc"] * (1 - truth["auc"])))
  expect_equal(c(logit$ci_lower, logit$ci_upper), unname(expected), tolerance = 1e-11)
})

test_that("PR plots show average-precision steps and safely redraw old trapezoidal state", {
  d <- .dualROCOracle$fixtures()$mixed_highest_higher$data
  truth <- .dualROCOracle$pr(d$disease, d$score1)
  for (combined in c(FALSE, TRUE)) {
    p <- .dualRunP(d, precisionRecallCurve = TRUE, combinePlots = combined)
    image <- p$precisionRecallPlot$get(if (combined) 1 else "score1")
    state <- image$state
    expect_equal(unique(state$auprc), truth$interpolated, tolerance = 1e-11)
    expect_equal(unique(state$avg_precision), truth$average_precision, tolerance = 1e-11)
    # Reproduce a saved image from 1.0.91: no AP field, stale trapezoidal AUPRC.
    state$avg_precision <- NULL
    state$auprc <- truth$trapezoidal
    image$setState(state)
    analysis <- p$analysis
    analysis$.__enclos_env__$private$.data <- NULL
    grDevices::pdf(NULL)
    tryCatch({
      expect_true(analysis$.__enclos_env__$private$.plotPrecisionRecall(image,
        ggplot2::theme_minimal(), list(palette = "jmv")))
      plot <- ggplot2::last_plot()
      expect_s3_class(plot$layers[[1]]$geom, "GeomStep")
      expect_identical(plot$layers[[1]]$geom_params$direction, "vh")
      labels <- unlist(lapply(ggplot2::ggplot_build(plot)$data, function(x) x$label))
      expect_true(sprintf("Average precision = %.3f", truth$average_precision) %in% labels)
      expect_false(any(grepl("AUPRC", labels, fixed = TRUE)))
    }, finally = grDevices::dev.off())
  }
})

test_that("fixed-rate and effect-size plots redraw without sibling tables or source data", {
  d <- .dualROCOracle$fixtures()$balanced_complete_higher$data
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (type in c("sensitivity", "specificity")) {
    p <- suppressMessages(ClinicoPath::psychopdaROC(data = d, classVar = "disease",
      positiveClass = "Case", negativeClass = "Control", refVar = NULL,
      dependentVars = c("score1", "score2"), run = TRUE,
      fixedSensSpecAnalysis = TRUE, showFixedROC = TRUE, fixedAnalysisType = type,
      effectSizeAnalysis = TRUE))
    analysis <- p$analysis
    private <- analysis$.__enclos_env__$private
    images <- list(p$fixedSensSpecROC$get("score1"), p$effectSizePlot$get("all"))
    renderers <- list(private$.plotFixedSensSpecROC, private$.plotEffectSize)
    before <- Map(function(image, render) {
      expect_identical(image$state$schema, 2L)
      expect_true(render(image, ggplot2::theme_minimal(), list(palette = "jmv")))
      ggplot2::ggplot_build(ggplot2::last_plot())$data
    }, images, renderers)
    ClinicoPath:::.deleteTableRows(p$fixedSensSpecTable)
    ClinicoPath:::.deleteTableRows(p$effectSizeTable)
    private$.data <- NULL
    for (i in seq_along(images)) {
      # A serialized state must contain no live table cells or analysis references.
      images[[i]]$setState(unserialize(serialize(images[[i]]$state, NULL)))
      expect_true(renderers[[i]](images[[i]], ggplot2::theme_minimal(), list(palette = "jmv")))
      expect_equal(ggplot2::ggplot_build(ggplot2::last_plot())$data, before[[i]])
    }
  }
})

test_that("dot-plot exports preserve the random stream and point positions", {
  d <- .dualROCOracle$fixtures()$balanced_complete_higher$data
  p <- .dualRunP(d, showDotPlot = TRUE)
  analysis <- p$analysis
  image <- p$dotPlot$get("score1")
  set.seed(614)
  before <- .Random.seed
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  render <- analysis$.__enclos_env__$private$.plotDot
  expect_true(render(image, ggplot2::theme_minimal(), list(palette = "jmv")))
  first <- ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]
  expect_identical(.Random.seed, before)
  expect_true(render(image, ggplot2::theme_minimal(), list(palette = "jmv")))
  expect_equal(ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]], first)
  expect_identical(.Random.seed, before)
})

test_that("native numeric-coded factors survive filtering and patient summaries", {
  d <- .dualROCOracle$fixtures()$balanced_complete_higher$data
  coded <- d
  for (marker in c("score1", "score2")) {
    values <- sort(unique(d[[marker]]))
    coded[[marker]] <- factor(d[[marker]], levels = rev(values))
    attr(coded[[marker]], "values") <- rev(values)
  }
  run <- function(data, ...) suppressMessages(ClinicoPath::psychopdaROC(data = data,
    classVar = "disease", positiveClass = "Case", negativeClass = "Control",
    dependentVars = c("score1", "score2"), refVar = "score2", run = TRUE,
    fixedSensSpecAnalysis = TRUE, showFixedROC = TRUE, compareClassifiers = TRUE,
    effectSizeAnalysis = TRUE, ...))
  numeric <- run(d)
  native <- run(coded)
  for (name in c("aucSummaryTable", "fixedSensSpecTable", "rocComparisonTable", "effectSizeTable"))
    expect_equal(native$get(name)$asDF, numeric$get(name)$asDF)
  expect_true(all(is.finite(native$fixedSensSpecTable$asDF$achieved_sensitivity)))
  # R's factor row-subset strips values even when every row is selected.
  expect_null(attr(coded$score1[seq_len(nrow(coded))], "values"))
  for (method in c("rows", "patientMean", "patientMedian")) {
    original <- d
    original$patient <- rep(seq_len(nrow(d) / 2), each = 2)
    coded$patient <- original$patient
    included <- seq_len(nrow(d)) %% 7L != 0L
    id <- if (method == "rows") NULL else "patient"
    expect_equal(ClinicoPath:::.psychopdaPatientData(coded, included, id,
      c("score1", "score2"), "disease", method = method)$data,
      ClinicoPath:::.psychopdaPatientData(original, included, id,
        c("score1", "score2"), "disease", method = method)$data)
  }
})
