# Regression checks for syntax export, bounded DeLong contrasts, and explicit execution.
.secROC_data <- function() {
  set.seed(730)
  y <- rep(c("negative", "positive"), each = 20)
  data.frame(outcome = factor(y), marker = rnorm(40) + (y == "positive"),
    reference = rnorm(40) + .5 * (y == "positive"))
}

.secROC_analysis <- function(data = .secROC_data(), ...) {
  args <- utils::modifyList(list(dependentVars = "marker", classVar = "outcome",
    positiveClass = "positive", negativeClass = "negative", refVar = NULL),
    list(...), keep.null = TRUE)
  options <- do.call(ClinicoPath:::psychopdaROCOptions$new, args)
  analysis <- ClinicoPath:::psychopdaROCClass$new(options = options, data = data)
  analysis$init()
  analysis
}

.secROC_note <- function(table, key) {
  notes <- table$.__enclos_env__$private$.notes
  if (key %in% names(notes)) notes[[key]]$note else ""
}

.secROC_replace <- function(private, name, value) {
  unlockBinding(name, private)
  private[[name]] <- value
  lockBinding(name, private)
}

.secROC_set <- function(analysis, name, value) {
  option <- analysis$options$option(name)
  option$value <- value
}

test_that("exported names are literals and safely round-trip through the R wrapper", {
  markers <- c('stop("security sentinel")', 'assay [raw]', 'weird"name',
    'path\\to\\thing', 'weird`name', '<i>assay</i>', 'marker{unmatched')
  d <- .secROC_data()
  baseline <- .secROC_analysis(d)
  baseline$run()
  for (marker in markers) {
    names(d)[2] <- marker
    a <- .secROC_analysis(d, dependentVars = marker)
    a$run()
    code <- parse(text = a$asSource())[[1]]
    expect_identical(as.list(code)$dependentVars, marker)
    result <- eval(code, envir = list(data = d))
    expect_equal(result$aucSummaryTable$asDF$auc,
      baseline$results$aucSummaryTable$asDF$auc)
  }
  names(d)[1:2] <- c('Outcome "quoted"', markers[1])
  a <- .secROC_analysis(d, classVar = names(d)[1], dependentVars = names(d)[2:3],
    refVar = "reference", delongTest = TRUE, run = TRUE)
  a$run()
  .secROC_set(a, "run", FALSE)
  code <- parse(text = a$asSource())[[1]]
  expect_true(as.list(code)$run)
  result <- eval(code, envir = list(data = d))
  expect_equal(result$delongComparisonTable$asDF, a$results$delongComparisonTable$asDF)
  a$options$.addOption(jmvcore::OptionString$new(
    'unknown); stop("security sentinel"); #', "untrusted"))
  expect_false(grepl("untrusted", a$asSource(), fixed = TRUE))
  expect_no_error(parse(text = a$asSource()))
})

test_that("both DeLong paths use small global matrices and preserve pairwise inference", {
  skip_if_not_installed("pROC")
  d <- .secROC_data()
  set.seed(871)
  markers <- as.data.frame(replicate(8, rnorm(nrow(d)) + (d$outcome == "positive")))
  a <- .secROC_analysis(d)
  p <- a$.__enclos_env__$private
  original <- p$.delongGlobalInference
  dimensions <- list()
  .secROC_replace(p, ".delongGlobalInference", function(difference, covariance) {
    dimensions[[length(dimensions) + 1L]] <<- dim(covariance)
    original(difference, covariance)
  })
  primary <- p$.enhancedDelongTest(markers, d$outcome, "positive")
  fallback <- p$.deLongTest(markers, d$outcome, "positive")
  expect_equal(dimensions, rep(list(c(7L, 7L)), 2))
  expect_equal(primary$difference, fallback$difference, tolerance = 1e-10)
  expect_equal(primary$global.p, fallback$global.p, tolerance = 1e-10)

  pairs <- t(combn(seq_len(ncol(markers)), 2))
  L <- matrix(0, nrow(pairs), ncol(markers))
  L[cbind(seq_len(nrow(L)), pairs[, 1])] <- 1
  L[cbind(seq_len(nrow(L)), pairs[, 2])] <- -1
  auc <- primary$AUC[, "AUC"]
  previous <- original(L %*% auc, L %*% primary$covariance %*% t(L))
  expect_equal(primary$global.z, previous$statistic, tolerance = 1e-10)
  expect_equal(primary$global.p, previous$p, tolerance = 1e-10)

  for (reference in c(1L, 3L, 8L)) {
    primary_ref <- p$.enhancedDelongTest(markers, d$outcome, "positive", ref = reference)
    fallback_ref <- p$.deLongTest(markers, d$outcome, "positive", ref = reference)
    expect_equal(primary_ref$difference, fallback_ref$difference, tolerance = 1e-10)
    expect_equal(primary_ref$global.p, primary$global.p, tolerance = 1e-10)
    expect_equal(fallback_ref$global.p, fallback$global.p, tolerance = 1e-10)
  }
})

test_that("bracketed marker names cannot truncate diagnostic notes", {
  d <- .secROC_data()
  names(d)[2] <- "assay [raw]"
  d[[2]] <- 5
  a <- .secROC_analysis(d, dependentVars = names(d)[2])
  suppressMessages(a$run())
  note <- .secROC_note(a$results$aucSummaryTable, "constant_marker")
  expect_match(note, "assay (raw)", fixed = TRUE)
  expect_match(note, "Remove them from the analysis.", fixed = TRUE)

  small <- .secROC_data()[c(1:4, 21:24), ]
  names(small)[2] <- "assay [raw]"
  a <- .secROC_analysis(small, dependentVars = names(small)[2:3],
    refVar = "reference", calculateIDI = TRUE, calculateNRI = TRUE, run = TRUE)
  suppressMessages(suppressWarnings(a$run()))
  for (table in list(a$results$idiTable, a$results$nriTable)) {
    note <- .secROC_note(table, paste0("insufficient_", make.names(names(small)[2])))
    expect_match(note, "assay (raw)", fixed = TRUE)
    expect_match(note, "fewer than 10 complete cases were available.", fixed = TRUE)
  }
})

test_that("intensive settings wait for Run before entering analysis", {
  configurations <- c(lapply(c("delongTest", "bootstrapCI", "calculateIDI", "calculateNRI",
    "bayesianAnalysis", "clinicalUtilityAnalysis", "powerAnalysis", "effectSizeAnalysis",
    "partialAUC", "allObserved", "manualRun"), function(name) setNames(list(TRUE), name)),
    list(list(boot_runs = 1L), list(method = "maximize_boot_metric"),
      list(method = "minimize_boot_metric"), list(method = "maximize_loess_metric"),
      list(method = "minimize_loess_metric"), list(method = "oc_youden_kernel"),
      list(rocSmoothingMethod = "binormal"), list(plotROC = TRUE, smoothing = TRUE)))
  for (configuration in configurations) {
    a <- do.call(.secROC_analysis, configuration)
    entered <- FALSE
    .secROC_replace(a$.__enclos_env__$private, ".validateInputs", function() {
      entered <<- TRUE
      stop("Analysis started before Run")
    })
    expect_no_error(a$run())
    expect_false(entered)
    expect_equal(a$results$aucSummaryTable$rowCount, 0L)
    expect_true(a$results$instructions$visible)
    expect_match(a$results$instructions$content, "Run selected analyses", fixed = TRUE)
  }
  large <- .secROC_data()[rep(seq_len(40), 251), ]
  a <- .secROC_analysis(large)
  a$run()
  expect_equal(a$results$aucSummaryTable$rowCount, 0L)
  expect_match(a$results$runSummary$content, "not been updated", fixed = TRUE)
})

test_that("basic analysis remains automatic and each intensive rerun needs an action", {
  a <- .secROC_analysis()
  a$run()
  expect_equal(a$results$aucSummaryTable$rowCount, 1L)
  p <- a$.__enclos_env__$private
  calls <- 0L
  .secROC_replace(p, ".calculateBootstrapCI", function(...) {
    calls <<- calls + 1L
    NULL
  })
  .secROC_set(a, "bootstrapCI", TRUE)
  p$.run()
  expect_equal(calls, 0L)
  .secROC_set(a, "run", TRUE)
  p$.run()
  expect_equal(calls, 1L)
  expect_false(a$results$instructions$visible)
  previous <- a$results$aucSummaryTable$asDF

  # A subsequent option/data update arrives without the one-shot Run action.
  .secROC_set(a, "run", FALSE)
  .secROC_set(a, "direction", "<=")
  p$.run()
  expect_equal(calls, 1L)
  expect_equal(a$results$aucSummaryTable$asDF, previous)
  expect_true(a$results$instructions$visible)
  expect_match(a$results$runSummary$content, "previous run", fixed = TRUE)
  .secROC_set(a, "run", TRUE)
  p$.run()
  expect_equal(calls, 2L)
  expect_equal(a$results$aucSummaryTable$asDF$auc, 1 - previous$auc, tolerance = 1e-12)
})

test_that("LOESS is saved during Run and renderers only draw cached coordinates", {
  skip_if_not_installed("ggplot2")
  a <- .secROC_analysis(dependentVars = c("marker", "reference"), plotROC = TRUE,
    smoothing = TRUE, displaySE = TRUE, combinePlots = TRUE, run = TRUE)
  suppressMessages(suppressWarnings(a$run()))
  image <- a$results$plotROC$get(key = a$results$plotROC$itemKeys[[1]])
  expect_true(all(c("x", "y", "ymin", "ymax", "var") %in% names(image$state$loess)))
  expect_setequal(unique(image$state$loess$var), c("marker", "reference"))
  saved <- unserialize(serialize(image$state, NULL))
  .secROC_set(a, "run", FALSE)
  image$setState(saved)
  render <- a$.__enclos_env__$private$.plotROC
  captured <- NULL
  env <- new.env(parent = environment(render))
  env$print <- function(plot, ...) { captured <<- plot; invisible(plot) }
  environment(render) <- env
  expect_true(render(image, ggplot2::theme_minimal(), list(palette = "jmv")))
  expect_false(any(vapply(captured$layers, function(layer)
    inherits(layer$stat, "StatSmooth"), logical(1))))
  expect_equal(image$state$loess, saved$loess)
})

test_that("the Run button is available independently of the manual-mode checkbox", {
  ui <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "psychopdaROC.u.yaml"))
  buttons <- Filter(function(item) identical(item$type, "ActionButton"), ui$children)
  expect_length(buttons, 1L)
  expect_identical(buttons[[1]]$name, "run")
  expect_null(buttons[[1]]$enable)
})
