# Regressions for the independently reproduced IR01--IR05 failures.
.irROC_data <- function() data.frame(
  y = factor(rep(c("negative", "positive"), each = 6)),
  x = c(-2, 0, 1, 1, 3, 7, -1, 1, 2, 4, 4, 8),
  z = c(0, 2, 1, 3, 2, 6, -2, 0, 3, 2, 5, 7))

.irROC_run <- function(d = .irROC_data(), vars = "x", ...) {
  args <- utils::modifyList(list(data = d, dependentVars = vars, classVar = "y",
    positiveClass = "positive", negativeClass = "negative", refVar = NULL, plotROC = FALSE, seed = 314159),
    list(...), keep.null = TRUE)
  suppressMessages(suppressWarnings(do.call(ClinicoPath::psychopdaROC, args)))
}

.irROC_note <- function(table, key) {
  notes <- table$.__enclos_env__$private$.notes
  if (key %in% names(notes)) as.character(notes[[key]]$note) else ""
}

.irROC_private <- function(d = .irROC_data(), vars = c("x", "z")) {
  options <- ClinicoPath:::psychopdaROCOptions$new(dependentVars = vars, classVar = "y",
    positiveClass = "positive", negativeClass = "negative", refVar = NULL, plotROC = FALSE, delongTest = TRUE)
  analysis <- ClinicoPath:::psychopdaROCClass$new(options = options, data = d)
  analysis$init()
  list(analysis = analysis, private = analysis$.__enclos_env__$private, options = options)
}

test_that("IR01 affine marker units cannot create NRI", {
  d <- .irROC_data()
  for (scale in c(100, 1e-6, -100)) {
    d$shift <- scale * d$x + 50
    for (thresholds in c("", "0.25,0.5,0.75")) {
      r <- .irROC_run(d, c("x", "shift"), calculateNRI = TRUE, calculateIDI = TRUE,
        refVar = "x", nriThresholds = thresholds, idiNriBootRuns = 100)
      row <- r$nriTable$asDF
      expect_equal(unlist(row[c("nri", "event_nri", "non_event_nri",
        "ci_lower", "ci_upper")], use.names = FALSE), rep(0, 5))
      expect_equal(row$p, 1)
      expect_equal(r$idiTable$asDF$idi, 0, tolerance = 1e-9)
      expect_match(.irROC_note(r$nriTable, "numerical_ties"), "1.5e-8", fixed = TRUE)
    }
  }
  d$shift <- 100 * d$x + 50
  r <- .irROC_run(d[12:1, ], c("x", "shift"), calculateNRI = TRUE,
    refVar = "x", idiNriBootRuns = 100, seed = 923)
  expect_equal(r$nriTable$asDF$nri, 0)
})

test_that("IR01 numerical ties include category boundaries but retain real movement", {
  y <- c(0, 0, 1, 1)
  near <- c(.5 - 1e-12, .5 + 1e-12, .5 + 1e-12, .5 - 1e-12)
  for (threshold in list(NULL, .5)) {
    result <- testthat::with_mocked_bindings(
      ClinicoPath:::computeNRI(near, rep(.5, 4), y, thresholds = threshold),
      raw_to_prob = function(values, ...) values, .package = "ClinicoPath")
    expect_equal(result$nri, 0)
    movement <- testthat::with_mocked_bindings(
      ClinicoPath:::computeNRI(c(.49, .49, .51, .51), rep(.5, 4), y,
        thresholds = threshold),
      raw_to_prob = function(values, ...) values, .package = "ClinicoPath")
    expect_equal(movement$nri, if (is.null(threshold)) 2 else 1)
  }
})

test_that("IR02 degenerate DeLong contrasts preserve estimates and withhold inference", {
  d <- .irROC_data()
  for (opposite in c(TRUE, FALSE)) {
    d$x <- seq_len(nrow(d))
    d$z <- if (opposite) -d$x else d$x
    r <- .irROC_run(d, c("x", "z"), delongTest = TRUE)
    row <- r$delongComparisonTable$asDF
    expect_equal(row$auc_diff, if (opposite) 1 else 0)
    expect_true(all(is.na(row[c("ci_lower", "ci_upper", "z", "p")])))
    expect_match(r$delongTest$content, "Overall test: unavailable", fixed = TRUE)
    expect_match(.irROC_note(r$delongComparisonTable, "degenerate_contrast"),
      "do not establish equality", fixed = TRUE)
  }
})

test_that("IR02 redundant contrasts retain estimable joint inference", {
  d <- .irROC_data(); d$same <- d$x
  p <- .irROC_private(d, c("x", "same", "z"))$private
  three <- p$.enhancedDelongTest(d[c("x", "same", "z")], d$y, "positive")
  two <- p$.enhancedDelongTest(d[c("x", "z")], d$y, "positive")
  expect_equal(three$global.p, two$global.p, tolerance = 1e-12)
  expect_true(is.na(three$difference["pair_1_2", "P.Value"]))
  expect_equal(three$difference["pair_1_3", "P.Value"], two$difference$P.Value)
  reference <- p$.enhancedDelongTest(d[c("x", "same", "z")], d$y,
    "positive", ref = 1)
  expect_true(is.na(reference$difference["pair_1_2", "P.Value"]))
  expect_equal(reference$global.p, two$global.p, tolerance = 1e-12)

  # Known covariance geometries: a nonzero component along a zero-variance axis
  # cannot be dropped by a generalized inverse to manufacture a small statistic.
  expect_true(is.na(p$.delongGlobalInference(c(0, 1), diag(c(1, 0)))$p))
  expect_true(is.na(p$.delongGlobalInference(c(0, 0), matrix(0, 2, 2))$p))
  expect_equal(p$.delongGlobalInference(c(2, 0), diag(c(1, 0)))$p,
    pchisq(4, df = 1, lower.tail = FALSE))
  expect_equal(p$.delongGlobalInference(c(2, 3), diag(c(1, 9)))$p,
    pchisq(5, df = 2, lower.tail = FALSE))
  expect_true(is.na(p$.delongGlobalInference(c(2, 3), diag(c(1, -1)))$p))
  expect_true(is.na(p$.delongGlobalInference(c(2, 3), diag(c(1, NA)))$p))
})

test_that("IR02 fallback also withholds zero-variance comparison inference", {
  d <- .irROC_data(); d$same <- d$x
  r <- testthat::with_mocked_bindings(
    .irROC_run(d, c("x", "same"), delongTest = TRUE),
    cov = function(...) stop("injected covariance failure"), .package = "pROC")
  expect_equal(r$delongComparisonTable$asDF$auc_diff, 0)
  expect_true(is.na(r$delongComparisonTable$asDF$p))
  expect_match(r$delongTest$content, "Overall test: unavailable", fixed = TRUE)
  expect_match(.irROC_note(r$delongComparisonTable, "delong_method"), "fallback")
})

test_that("IR03 fallback uncertainty uses the effective complete-case counts", {
  d <- .irROC_data(); d$x[c(1, 2, 7, 8)] <- NA_real_
  r <- testthat::with_mocked_bindings(.irROC_run(d),
    roc = function(...) stop("injected ROC failure"), .package = "pROC")
  row <- r$aucSummaryTable$asDF
  # Eight complete observations, four per class; exact rank AUC = 3/4.
  # Hanley-McNeil variance = (a(1-a)+3(Q1-a^2)+3(Q2-a^2))/16 = 303/8960.
  variance <- 303 / 8960
  expect_equal(row$auc, .75)
  expect_equal(c(row$ci_lower, row$ci_upper),
    c(.75 - qnorm(.975) * sqrt(variance), 1), tolerance = 1e-12)
  expect_equal(row$p, 2 * pnorm(-.25 / sqrt(variance)), tolerance = 1e-12)
  expect_match(.irROC_note(r$aucSummaryTable, "effective_n"),
    "4 positive, 4 negative", fixed = TRUE)
  expect_match(.irROC_note(r$aucSummaryTable, "small_sample"), "Only 4", fixed = TRUE)
  normal <- .irROC_run(d)
  expect_false(isTRUE(all.equal(normal$aucSummaryTable$asDF$p, row$p)))
  expect_match(.irROC_note(normal$aucSummaryTable, "effective_n"),
    "4 positive, 4 negative", fixed = TRUE)
})

test_that("IR04 and IR05 disclose the actual inference assumptions and method", {
  for (mode in c("basic", "comprehensive")) {
    r <- .irROC_run(clinicalMode = mode)
    expect_match(r$procedureNotes$content, "independent observation", fixed = TRUE)
    for (name in c("aucSummaryTable", "delongComparisonTable", "partialAUCTable",
      "bootstrapCITable", "nriTable", "idiTable", "bayesianROCTable")) {
      expect_match(.irROC_note(r[[name]], "independent_observations"),
        "clustering within patients", fixed = TRUE)
    }
    expect_false(grepl("Bayesian analysis|research-grade", r$procedureNotes$content))
  }
})

test_that("IR03 effective-count caveats refresh when the selected marker changes", {
  d <- rbind(.irROC_data(), .irROC_data())
  for (level in levels(d$y)) d$x[head(which(d$y == level), 8)] <- NA_real_
  object <- .irROC_private(d)
  comparison <- object$options$option("delongTest"); comparison$value <- FALSE
  selected <- object$options$option("dependentVars")
  for (marker in c("x", "z", "x")) {
    selected$value <- marker
    suppressMessages(suppressWarnings(object$private$.run()))
    table <- object$analysis$results$aucSummaryTable
    if (marker == "x") {
      expect_match(.irROC_note(table, "small_sample"), "Only 4", fixed = TRUE)
      expect_match(.irROC_note(table, "effective_n"), "4 positive, 4 negative", fixed = TRUE)
    } else {
      expect_equal(.irROC_note(table, "small_sample"), "")
      expect_match(.irROC_note(table, "effective_n"), "12 positive, 12 negative", fixed = TRUE)
    }
  }
})

test_that("IR02 degenerate warning clears when the same analysis becomes estimable", {
  d <- .irROC_data(); d$same <- d$x
  object <- .irROC_private(d, c("x", "same", "z"))
  selected <- object$options$option("dependentVars"); selected$value <- c("x", "same")
  suppressMessages(suppressWarnings(object$private$.run()))
  expect_match(.irROC_note(object$analysis$results$delongComparisonTable,
    "degenerate_contrast"), "unavailable")
  selected$value <- c("x", "z")
  suppressMessages(suppressWarnings(object$private$.run()))
  expect_equal(.irROC_note(object$analysis$results$delongComparisonTable,
    "degenerate_contrast"), "")
  expect_true(is.finite(object$analysis$results$delongComparisonTable$asDF$p))
})
