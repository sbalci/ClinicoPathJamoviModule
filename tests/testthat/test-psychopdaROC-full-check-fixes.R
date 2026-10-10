# Regressions for the full audit, including corrections on a reused analysis.
.fullROC_data <- function() {
  d <- utils::read.csv(testthat::test_path("..", "..", "data",
    "psychopdaROC_release.csv"), check.names = FALSE)
  d$outcome <- factor(d$outcome)
  d$marker_c <- d$marker_a * 0.4 + cos(seq_len(nrow(d)))
  d
}
.fullROC_run <- function(data = .fullROC_data(), ...) {
  args <- utils::modifyList(list(dependentVars = c("marker_a", "marker_b"),
    classVar = "outcome", positiveClass = "Positive", negativeClass = "Negative", refVar = NULL),
    list(...), keep.null = TRUE)
  suppressMessages(suppressWarnings(do.call(ClinicoPath::psychopdaROC,
    c(list(data = data), args))))
}
.fullROC_change <- function(r, name, value) {
  option <- r$analysis$options$option(name)
  option$value <- value
  suppressMessages(suppressWarnings(r$analysis$run()))
  invisible(r)
}
.fullROC_capture <- function(r, array, fun, key = NULL) {
  if (is.null(key)) key <- r[[array]]$itemKeys[[1]]
  f <- r$analysis$.__enclos_env__$private[[fun]]
  env <- new.env(parent = environment(f))
  plot <- NULL
  env$print <- function(x, ...) { plot <<- x; invisible(x) }
  environment(f) <- env
  value <- f(r[[array]]$get(key), ggplot2::theme_minimal(), list(palette = "jmv"))
  list(value = value, plot = plot)
}
.fullROC_notices <- function(r) Filter(function(x)
  inherits(x, "Notice") && isTRUE(x$visible), r$items)

test_that("F01 meta-analysis disables and revokes all dependent outputs", {
  r <- .fullROC_run(dependentVars = c("marker_a", "marker_b", "marker_c"),
    metaAnalysis = TRUE, overrideMetaAnalysisWarning = TRUE, forestPlot = TRUE)
  expect_false(r$metaAnalysisTable$visible)
  expect_false(r$metaAnalysisForestPlot$visible)
  .fullROC_change(r, "metaAnalysis", FALSE)
  expect_false(r$metaAnalysisTable$visible)
  expect_false(r$metaAnalysisForestPlot$visible)
  expect_length(r$metaAnalysisForestPlot$itemKeys, 0)
  .fullROC_change(r, "metaAnalysis", TRUE)
  expect_false(r$metaAnalysisForestPlot$visible)
  .fullROC_change(r, "overrideMetaAnalysisWarning", FALSE)
  expect_true(r$metaAnalysisWarning$visible)
  expect_false(r$metaAnalysisTable$visible)
  expect_false(r$metaAnalysisForestPlot$visible)
  expect_length(r$metaAnalysisForestPlot$itemKeys, 0)
  .fullROC_change(r, "overrideMetaAnalysisWarning", TRUE)
  .fullROC_change(r, "dependentVars", c("marker_a", "marker_b"))
  expect_false(r$metaAnalysisForestPlot$visible)
  expect_match(r$runSummary$content, "not independent studies")
})

test_that("F02 bootstrap traces invalidate changed seed and direction", {
  restore <- function(changed = character()) {
    r <- ClinicoPath:::psychopdaROCResults$new(ClinicoPath:::psychopdaROCOptions$new())
    r$bayesianTracePlot$addItem(key = "marker_a")
    image <- r$bayesianTracePlot$get("marker_a")
    image$fromProtoBuf(list(state = raw(), image = list(path = "previous.png")),
      oChanges = changed, vChanges = character())
    image$.__enclos_env__$private$.filePath
  }
  expect_identical(restore(), "previous.png")
  for (option in c("seed", "direction", "bayesianAnalysis"))
    expect_null(restore(option), info = option)
})

test_that("F03 invalid power does not draw a substitute model", {
  for (type in c("prospective", "sample_size")) {
    r <- .fullROC_run(powerAnalysis = TRUE, powerAnalysisType = type, correlationROCs = 1)
    expect_true(all(is.na(r$powerAnalysisTable$asDF$required_n)))
    expect_false(.fullROC_capture(r, "powerCurvePlot", ".plotPowerCurves")$value)
    expect_match(r$runSummary$content, "power curve.*unavailable")
    .fullROC_change(r, "correlationROCs", 0.5)
    expect_true(all(is.finite(r$powerAnalysisTable$asDF$required_n)))
    expect_true(.fullROC_capture(r, "powerCurvePlot", ".plotPowerCurves")$value)
    expect_false("degenerate_correlation" %in% names(r$powerAnalysisTable$notes))
    .fullROC_change(r, "expectedAUCDifference", 0.5)
    expect_true(all(is.na(r$powerAnalysisTable$asDF$required_n)))
    expect_false(.fullROC_capture(r, "powerCurvePlot", ".plotPowerCurves")$value)
  }
})

test_that("F04 conditional notes and notices clear after correction", {
  r <- .fullROC_run(dependentVars = "marker_inverse", plotROC = TRUE,
    quantileCIs = TRUE, tol_metric = 0.1)
  expect_true("auc_below_chance" %in% names(r$aucSummaryTable$notes))
  .fullROC_change(r, "direction", "<=")
  expect_false("auc_below_chance" %in% names(r$aucSummaryTable$notes))
  expect_false("psychopdaROC_belowChance" %in% names(.fullROC_notices(r)))
  .fullROC_change(r, "quantileCIs", FALSE)
  expect_false("roc_pointwise_ci" %in% names(r$aucSummaryTable$notes))
  .fullROC_change(r, "tol_metric", 1e-6)
  expect_false("metric_tolerance" %in% names(r$resultsTable$get("marker_inverse")$notes))
  expect_identical(anyDuplicated(names(r$items)), 0L)
})

test_that("F05 line and Wilson ribbon colours identify the same marker", {
  for (vars in list("marker_a", c("marker_b", "marker_a", "marker_c"))) {
    r <- .fullROC_run(dependentVars = vars, plotROC = TRUE,
      combinePlots = TRUE, showConfidenceBands = TRUE)
    plot <- .fullROC_capture(r, "plotROC", ".plotROC")$plot
    built <- ggplot2::ggplot_build(plot)
    lines <- which(vapply(plot$layers, function(x) inherits(x$geom, "GeomLine"), logical(1)))
    ribbons <- which(vapply(plot$layers, function(x) inherits(x$geom, "GeomRibbon"), logical(1)))
    line <- unique(built$data[[lines[1]]][c("group", "colour")])
    ribbon <- unique(built$data[[ribbons[1]]][c("group", "fill")])
    expect_equal(unname(line$colour[order(line$group)]),
      unname(ribbon$fill[order(ribbon$group)]))
    expect_match(plot$labels$caption, "\n", fixed = TRUE)
    expect_identical(plot$theme$plot.caption.position, "plot")
    expect_equal(plot$theme$plot.caption$hjust, 0)
  }
})

test_that("F06 nearby observed cutpoints export with unique row names", {
  r <- .fullROC_run(allObserved = TRUE)
  d <- .fullROC_data()
  for (v in c("marker_a", "marker_b")) {
    tab <- r$resultsTable$get(v)
    expect_identical(tab$title, v)
    expect_identical(anyDuplicated(tab$names), 0L)
    frame <- tab$asDF
    expect_setequal(frame$cutpoint, unique(d[[v]][is.finite(d[[v]])]))
  }
})

test_that("F07 NRI canonicalizes duplicate and unsorted boundaries explicitly", {
  a <- .fullROC_run(calculateNRI = TRUE, nriThresholds = "0.7,0.2,0.2", idiNriBootRuns = 100)
  b <- .fullROC_run(calculateNRI = TRUE, nriThresholds = "0.2,0.7", idiNriBootRuns = 100)
  expect_equal(a$nriTable$asDF, b$nriTable$asDF)
  expect_match(a$runSummary$content, "Repeated NRI")
  expect_match(a$nriTable$notes$nri_variant$note, "0.2, 0.7", fixed = TRUE)
})

test_that("F08 subgroup labels are never parsed as identities", {
  d <- .fullROC_data()
  d[["Marker ::: A"]] <- d$marker_a
  d$site <- paste0("Site ::: ", d$site)
  r <- .fullROC_run(data = d, dependentVars = "Marker ::: A", subGroup = "site")
  for (g in unique(d$site)) {
    expected <- .fullROC_run(data = d[d$site == g, ], dependentVars = "Marker ::: A")
    key <- paste("Marker ::: A", g, sep = " ::: ")
    got <- r$aucSummaryTable$asDF
    expect_equal(got$auc[got$variable == key], expected$aucSummaryTable$asDF$auc)
  }
  d$site <- NA_character_
  expect_error(.fullROC_run(data = d, subGroup = "site"), "subgroup variable has no observed")
})

test_that("literal smoothing suffixes cannot replace another marker's overlay data", {
  d <- .fullROC_data()
  d$marker_a_smooth <- d$marker_b
  r <- .fullROC_run(data = d, dependentVars = c("marker_a", "marker_a_smooth"),
    plotROC = TRUE, combinePlots = TRUE, rocSmoothingMethod = "binormal",
    showConfidenceBands = TRUE)
  st <- r$plotROC$get(1)$state
  expect_setequal(st$counts$var, c("marker_a", "marker_a_smooth"))
  expect_setequal(st$smooth$var, st$counts$var)
  .fullROC_change(r, "rocSmoothingMethod", "none")
  expect_null(r$plotROC$get(1)$state$smooth)
  expect_setequal(r$plotROC$get(1)$state$counts$var, st$counts$var)
})

test_that("methodological fallback has a native notice and clears after recovery", {
  r <- testthat::with_mocked_bindings(.fullROC_run(dependentVars = "marker_a"),
    roc = function(...) stop("injected ROC failure"), .package = "pROC")
  expect_match(.fullROC_notices(r)$psychopdaROC_aucFallback$content, "Hanley-McNeil")
  .fullROC_change(r, "seed", 234)
  expect_false("psychopdaROC_aucFallback" %in% names(.fullROC_notices(r)))
  expect_false("ci_method_fallback" %in% names(r$aucSummaryTable$notes))
})

test_that("F09 partial AUC requires a positive width range", {
  for (to in c(0.9, 0.8))
    expect_error(.fullROC_run(partialAUC = TRUE, partialAUCfrom = 0.9,
      partialAUCto = to), "From value to be smaller")
})

test_that("F10 invalid quantiles are disclosed and plot state has accepted values", {
  r <- .fullROC_run(plotROC = TRUE, quantileCIs = TRUE, quantiles = "-1,abc,2")
  expect_match(r$runSummary$content, "Only 0 of 3 ROC quantiles")
  expect_false("roc_pointwise_ci" %in% names(r$aucSummaryTable$notes))
  notices <- lapply(.fullROC_notices(r), function(x) x$content)
  expect_true(.fullROC_capture(r, "plotROC", ".plotROC")$value)
  expect_equal(lapply(.fullROC_notices(r), function(x) x$content), notices)
  .fullROC_change(r, "quantiles", "0.25,abc,0.75")
  expect_equal(unique(r$plotROC$get("marker_a")$state$quantiles$prob), c(0.25, 0.75))
  .fullROC_change(r, "quantiles", "0.25,0.75")
  expect_false(grepl("Only .*ROC quantiles", r$runSummary$content))
})

test_that("F11 one-marker effect sizes give an actionable notice", {
  r <- .fullROC_run(dependentVars = "marker_a", effectSizeAnalysis = TRUE)
  expect_match(r$runSummary$content, "Effect size comparisons require at least two")
  .fullROC_change(r, "effectSizeAnalysis", FALSE)
  expect_false(grepl("Effect size comparisons", r$runSummary$content))
})

test_that("F12 decision curve grids are bounded before allocation and preserved", {
  for (text in c("0.2,0.2,0.05", "0.05,0.5,1e-12", "0.05,0.5,1e-320")) {
    r <- .fullROC_run(clinicalUtilityAnalysis = TRUE, treatmentThreshold = text)
    grid <- r$decisionCurvePlot$get("marker_a")$state$threshold_range
    expect_lte(length(grid), 100L)
    plot <- .fullROC_capture(r, "decisionCurvePlot", ".plotDecisionCurve")$plot
    expect_equal(sort(unique(plot$data$threshold)), grid)
    if (startsWith(text, "0.2")) {
      expect_equal(grid, 0.2)
      expect_s3_class(plot$layers[[1]]$geom, "GeomPoint")
    } else {
      expect_length(grid, 100)
      expect_equal(range(grid), c(0.05, 0.5))
    }
  }
  r <- .fullROC_run(clinicalUtilityAnalysis = TRUE, treatmentThreshold = "abc")
  expect_true("range_fallback" %in% names(r$decisionCurveTable$notes))
  .fullROC_change(r, "treatmentThreshold", "0.1,0.3,0.1")
  expect_false("range_fallback" %in% names(r$decisionCurveTable$notes))
})

test_that("F13 LOESS is explicit even above ggplot's automatic GAM boundary", {
  r <- .fullROC_run(plotROC = TRUE, smoothing = TRUE, displaySE = TRUE)
  plot <- .fullROC_capture(r, "plotROC", ".plotROC")$plot
  smooth <- Filter(function(x) inherits(x$stat, "StatSmooth"), plot$layers)[[1]]
  expect_identical(smooth$stat_params$method, "loess")
  expect_equal(smooth$stat_params$level, 0.95)
  expect_true(smooth$stat_params$se)
  dense <- data.frame(specificity = seq(1, 0, length.out = 1200),
    sensitivity = sqrt(seq(0, 1, length.out = 1200)), var = "marker_a", AUC = 2/3)
  r$plotROC$get("marker_a")$setState(list(curve = dense))
  plot <- .fullROC_capture(r, "plotROC", ".plotROC")$plot
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(plot)))
})

test_that("F14 threshold table respects the cap while retaining Youden optimum", {
  for (limit in c(5L, 20L)) {
    r <- .fullROC_run(showThresholdTable = TRUE, maxThresholds = limit)
    tab <- r$thresholdTable$asDF
    for (v in unique(tab$variable)) {
      expect_lte(sum(tab$variable == v), limit)
      curve <- r$analysis$.__enclos_env__$private$.rocDataList[[v]]
      expect_true(curve$threshold[which.max(curve$youden)] %in% tab$threshold[tab$variable == v])
    }
  }
})

test_that("F15 and F17 UI controls and dataset routing include their consumers", {
  ui <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "psychopdaROC.u.yaml"))
  find <- function(x, name) {
    if (identical(x$name, name)) return(x)
    for (child in x$children) { got <- find(child, name); if (!is.null(got)) return(got) }
    NULL
  }
  enable <- find(ui, "tol_metric")$enable
  expect_match(enable, "maximize_boot_metric", fixed = TRUE)
  expect_match(enable, "minimize_boot_metric", fixed = TRUE)
  config <- yaml::read_yaml(testthat::test_path("..", "..", "_updateModules_config.yaml"))
  expect_true("psychopdaROC_release.rda" %in% config$modules$JamoviTest$data_files)
})

test_that("F18 supplied data with no mappings reaches welcome and can be selected later", {
  r <- .fullROC_run(dependentVars = NULL, classVar = NULL, positiveClass = NULL)
  expect_true(r$instructions$visible)
  expect_match(r$instructions$content, "jmv-welcome", fixed = TRUE)
  # Set all required mappings before rerunning, as a normal UI batch does.
  for (x in list(c("dependentVars", "marker_a"), c("classVar", "outcome"),
    c("positiveClass", "Positive"))) {
    option <- r$analysis$options$option(x[1]); option$value <- x[2]
  }
  suppressMessages(r$analysis$run())
  expect_equal(nrow(r$aucSummaryTable$asDF), 1L)
  expect_false(r$instructions$visible)
})
