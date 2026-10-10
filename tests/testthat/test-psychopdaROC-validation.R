# Independent numerical regressions from the 2026-10-10 validation.
# These assertions cover the repaired VAL-psychopdaROC findings.

.valROC_data <- function() data.frame(
  status = factor(rep(c("neg", "pos"), each = 6), levels = c("neg", "pos")),
  x = c(0, 1, 2, 3, 3, 5, 1, 2, 3, 4, 5, 6),
  z = c(1, 0, 3, 2, 4, 4, 0, 2, 4, 3, 6, 5))

.valROC_run <- function(d = .valROC_data(), vars = "x", ...) {
  args <- utils::modifyList(list(data = d, dependentVars = vars, classVar = "status",
    positiveClass = "pos", negativeClass = "neg", refVar = NULL, plotROC = FALSE), list(...), keep.null = TRUE)
  suppressMessages(suppressWarnings(do.call(ClinicoPath::psychopdaROC, args)))
}

.valROC_counts <- function(x, y, cut, direction = ">=", prevalence = mean(y)) {
  positive <- if (direction == ">=") x >= cut else x <= cut
  tp <- sum(positive & y); fp <- sum(positive & !y)
  tn <- sum(!positive & !y); fn <- sum(!positive & y)
  se <- tp / sum(y); sp <- tn / sum(!y)
  c(sensitivity = se, specificity = sp, accuracy = (tp + tn) / length(y),
    ppv = se * prevalence / (se * prevalence + (1 - sp) * (1 - prevalence)),
    npv = sp * (1 - prevalence) / (sp * (1 - prevalence) + (1 - se) * prevalence))
}

.valROC_note <- function(table, key) {
  notes <- table$.__enclos_env__$private$.notes
  if (!key %in% names(notes)) "" else as.character(notes[[key]]$note)
}

test_that("ROC AUC and DeLong uncertainty match independent placements", {
  # Of 36 case-control pairs, 49/2 wins; placement variance = 347/12960.
  expected_auc <- 49 / 72
  variance <- 347 / 12960
  # The reported DeLong interval is intersected with the AUC parameter space.
  expected_ci <- pmax(0, pmin(1, expected_auc + c(-1, 1) * qnorm(.975) * sqrt(variance)))
  r <- .valROC_run(aucInference = "delong")$aucSummaryTable$asDF
  expect_equal(r$auc, expected_auc, tolerance = 1e-12)
  expect_equal(c(r$ci_lower, r$ci_upper), expected_ci, tolerance = 1e-12)
  expect_equal(r$p, 2 * pnorm(-abs((expected_auc - .5) / sqrt(variance))), tolerance = 1e-12)
})

test_that("ROC paired contrast uses shared placements and correct sign", {
  r <- .valROC_run(vars = c("x", "z"), delongTest = TRUE)$delongComparisonTable$asDF
  delta <- 1 / 36; se <- sqrt(11 / 1296)
  expect_equal(r$auc_diff, delta, tolerance = 1e-12)
  expect_equal(c(r$ci_lower, r$ci_upper), delta + c(-1, 1) * qnorm(.975) * se, tolerance = 1e-12)
  expect_equal(r$z, 1 / sqrt(11), tolerance = 1e-12)
  expect_equal(r$p, 2 * pnorm(-1 / sqrt(11)), tolerance = 1e-12)
})

test_that("ROC positive class direction and input order preserve the estimand", {
  d <- .valROC_data()
  expect_equal(.valROC_run(d[12:1, ])$aucSummaryTable$asDF$auc, 49 / 72, tolerance = 1e-12)
  expect_equal(.valROC_run(direction = "<=")$aucSummaryTable$asDF$auc, 23 / 72, tolerance = 1e-12)
  expect_equal(.valROC_run(positiveClass = "neg", negativeClass = "pos")$aucSummaryTable$asDF$auc, 23 / 72, tolerance = 1e-12)
  d$status <- factor(d$status, levels = c("pos", "neg"))
  names(d)[2] <- "Tümör score (µg)"
  expect_equal(.valROC_run(d, "Tümör score (µg)")$aucSummaryTable$asDF$auc, 49 / 72, tolerance = 1e-12)
})

test_that("ROC missing outcomes and predictors use observed complete pairs", {
  d <- .valROC_data(); d$x[1] <- NA; d$status[12] <- NA
  good <- complete.cases(d$x, d$status)
  x <- d$x[good]; y <- d$status[good] == "pos"
  h <- outer(x[y], x[!y], function(a, b) (a > b) + .5 * (a == b))
  expected <- mean(h)
  expect_equal(.valROC_run(d)$aucSummaryTable$asDF$auc, expected, tolerance = 1e-12)
  d$status <- addNA(d$status)
  expect_equal(.valROC_run(d)$aucSummaryTable$asDF$auc, expected, tolerance = 1e-12)
})

test_that("ROC manual counts and external prevalence obey Bayes theorem", {
  for (direction in c(">=", "<=")) {
    d <- .valROC_data(); cutoff <- 3
    if (direction == "<=") { d$x <- -d$x; cutoff <- -3 }
    r <- .valROC_run(d, direction = direction, method = "oc_manual",
      specifyCutScore = as.character(cutoff), usePriorPrev = TRUE, priorPrev = .1)$resultsTable$get("x")$asDF
    expect_equal(r$sensitivity, 2 / 3, tolerance = 1e-12)
    expect_equal(r$specificity, 1 / 2, tolerance = 1e-12)
    expect_equal(r$ppv, 4 / 31, tolerance = 1e-12)
    expect_equal(r$npv, 27 / 29, tolerance = 1e-12)
  }
})

test_that("ROC partial area and McClish correction match independent integrals", {
  r <- .valROC_run(partialAUC = TRUE, partialAUCfrom = .6)$partialAUCTable$asDF
  expect_equal(r$pAUC, 619 / 3600, tolerance = 1e-12)
  expect_equal(r$pAUC_normalized, 1483 / 2304, tolerance = 1e-12)
})

test_that("ROC stepwise fixed targets report empirical operating points", {
  d <- .valROC_data(); y <- d$status == "pos"
  for (type in c("sensitivity", "specificity")) {
    r <- .valROC_run(fixedSensSpecAnalysis = TRUE, fixedAnalysisType = type,
      fixedInterpolation = "stepwise")$fixedSensSpecTable$asDF
    truth <- .valROC_counts(d$x, y, r$cutpoint)
    expect_equal(r$achieved_sensitivity, unname(truth["sensitivity"]))
    expect_equal(r$achieved_specificity, unname(truth["specificity"]))
    expect_gte(r[[paste0("achieved_", type)]], .9)
  }
})

test_that("ROC effect sizes retain direction and explicit delta convention", {
  d <- .valROC_data(); y <- d$status == "pos"
  s <- function(x) (mean(x[y]) - mean(x[!y])) / sqrt((var(x[y]) + var(x[!y])) / 2)
  r <- .valROC_run(vars = c("x", "z"), effectSizeAnalysis = TRUE)$effectSizeTable$asDF
  expect_equal(r$d_first, s(d$x), tolerance = 1e-12)
  expect_equal(r$d_second, s(d$z), tolerance = 1e-12)
  expect_equal(r$cohens_d, s(d$z) - s(d$x), tolerance = 1e-12)
  expect_equal(r$hedges_g, (s(d$z) - s(d$x)) * (1 - 3 / 39), tolerance = 1e-12)
})

test_that("ROC identical markers have zero reclassification improvement", {
  d <- .valROC_data(); d$z <- d$x
  r <- .valROC_run(d, c("x", "z"), calculateIDI = TRUE, calculateNRI = TRUE, idiNriBootRuns = 100)
  expect_equal(r$idiTable$asDF$idi, 0)
  expect_equal(r$nriTable$asDF$nri, 0)
  expect_equal(r$nriTable$asDF$event_nri, 0)
  expect_equal(r$nriTable$asDF$non_event_nri, 0)
  expect_true(all(is.na(r$idiTable$asDF$p)))
  expect_true(all(is.na(r$nriTable$asDF$p)))
})

test_that("ROC requires a declared positive level and guards non-independent pooling", {
  r <- .valROC_run(positiveClass = NULL)
  expect_equal(nrow(r$aucSummaryTable$asDF), 0)
  expect_match(r$runSummary$content, "Choose the positive and negative classes")
  d <- .valROC_data(); d$w <- -d$x
  r <- .valROC_run(d, c("x", "z", "w"), metaAnalysis = TRUE)
  expect_match(r$metaAnalysisWarning$content, "independent studies")
})

test_that("ROC bootstrap is seeded and restores the calling RNG", {
  set.seed(76543); old_seed <- .Random.seed
  a <- .valROC_run(bootstrapCI = TRUE, bootstrapReps = 200, seed = 765)
  expect_identical(.Random.seed, old_seed)
  b <- .valROC_run(bootstrapCI = TRUE, bootstrapReps = 200, seed = 765)
  expect_identical(a$bootstrapCITable$asDF, b$bootstrapCITable$asDF)
})

test_that("ROC Wilson ribbon is a pointwise score interval", {
  r <- .valROC_run(plotROC = TRUE, showConfidenceBands = TRUE, combinePlots = FALSE)
  img <- r$plotROC$get("x"); dest <- tempfile(fileext = ".png")
  on.exit(unlink(dest), add = TRUE)
  expect_true(img$saveAs(dest))
  p <- ggplot2::last_plot()
  ribbon <- Filter(function(l) inherits(l$geom, "GeomRibbon"), p$layers)[[1]]$data
  curve <- img$state$curve
  expected <- suppressWarnings(prop.test(round(curve$sensitivity[4] * 6), 6, correct = FALSE)$conf.int)
  expect_equal(as.numeric(ribbon[4, c("ymin", "ymax")]), as.numeric(expected), tolerance = 1e-7)
  expect_match(p$labels$caption, "not a simultaneous")
})

test_that("ROC exported syntax round-trips in two clean R sessions", {
  d <- .valROC_data()
  opts <- list(dependentVars = c("x", "z"), classVar = "status", positiveClass = "pos", negativeClass = "neg",
    refVar = NULL, method = "oc_manual", specifyCutScore = "3", plotROC = FALSE,
    delongTest = TRUE, usePriorPrev = TRUE, priorPrev = .1)
  # asSource belongs to the analysis instance, not the public wrapper's Results.
  a <- ClinicoPath:::psychopdaROCClass$new(
    options = do.call(ClinicoPath:::psychopdaROCOptions$new, opts), data = d)
  a$init(); suppressMessages(suppressWarnings(a$run()))
  syntax <- a$asSource()
  reference <- .valROC_run(d, c("x", "z"), method = "oc_manual", specifyCutScore = "3",
    delongTest = TRUE, usePriorPrev = TRUE, priorPrev = .1)
  expected <- list(auc = reference$aucSummaryTable$asDF,
    cut = reference$resultsTable$get("x")$asDF, pair = reference$delongComparisonTable$asDF)
  repo <- normalizePath(testthat::test_path("..", ".."))
  scratch <- tempfile("psychopdaROC-export-"); dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  script <- file.path(scratch, "round-trip.R"); output <- file.path(scratch, "result.rds")
  writeLines(c(paste0("setwd(", deparse(repo), ")"),
    "suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))",
    paste0("data <- ", paste(capture.output(dput(d)), collapse = "\n")),
    paste0("res <- suppressMessages(suppressWarnings(", syntax, "))"),
    paste0("saveRDS(list(auc=res$aucSummaryTable$asDF, cut=res$resultsTable$get('x')$asDF, ",
      "pair=res$delongComparisonTable$asDF), ", deparse(output), ")")), script)
  previous <- NULL
  for (i in 1:2) {
    log <- file.path(scratch, paste0("run", i, ".log"))
    status <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)),
      stdout = log, stderr = log)
    expect_identical(status, 0L, info = paste(readLines(log, warn = FALSE), collapse = "\n"))
    if (status != 0L) break
    got <- readRDS(output)
    expect_equal(got, expected, tolerance = 1e-12)
    if (!is.null(previous)) expect_identical(got, previous)
    previous <- got
  }
})

test_that("VAL-psychopdaROC-01 partial-AUC intervals use the partial estimand", {
  d <- .valROC_data()
  r <- .valROC_run(partialAUC = TRUE, partialAUCfrom = .6,
    bootstrapCI = TRUE, bootstrapReps = 200)$partialAUCTable$asDF
  roc <- pROC::roc(d$status, d$x, levels = c("neg", "pos"), direction = "<", quiet = TRUE)
  expected <- withr::with_seed(123, pROC::ci.auc(roc, partial.auc = c(.6, 1),
    partial.auc.focus = "specificity", reuse.auc = FALSE, method = "bootstrap", boot.n = 200))
  expect_equal(c(r$ci_lower, r$ci_upper), as.numeric(expected[c(1, 3)]))
  expect_lte(r$ci_upper, .4)
})

test_that("VAL-psychopdaROC-02 achieved fixed rates describe actual classifications", {
  d <- .valROC_data(); y <- d$status == "pos"
  r <- .valROC_run(fixedSensSpecAnalysis = TRUE, fixedInterpolation = "linear")$fixedSensSpecTable$asDF
  ex <- .valROC_counts(d$x, y, r$cutpoint)
  expect_equal(r$achieved_sensitivity, unname(ex["sensitivity"]))
  expect_equal(r$achieved_specificity, unname(ex["specificity"]))
})

test_that("VAL-psychopdaROC-03 classifier summaries do not recycle tied thresholds", {
  d <- data.frame(status = factor(rep(c("neg", "pos"), each = 10)),
    x = c(1, 2, 3, 3, 4, 5, 6, 7, 8, 10, 3, 5, 6, 8, 9, 11, 12, 13, 14, 15))
  a <- .valROC_run(d, compareClassifiers = TRUE)$rocComparisonTable$asDF
  b <- .valROC_run(d[20:1, ], compareClassifiers = TRUE)$rocComparisonTable$asDF
  expect_equal(a[, c("f1_score", "accuracy", "balanced_accuracy")],
    b[, c("f1_score", "accuracy", "balanced_accuracy")])
})

test_that("VAL-psychopdaROC-04 fixed-point predictive values honor prevalence", {
  d <- .valROC_data()
  r <- .valROC_run(fixedSensSpecAnalysis = TRUE, fixedInterpolation = "stepwise",
    usePriorPrev = TRUE, priorPrev = .1)$fixedSensSpecTable$asDF
  ex <- .valROC_counts(d$x, d$status == "pos", r$cutpoint, prevalence = .1)
  expect_equal(r$ppv, unname(ex["ppv"]))
  expect_equal(r$npv, unname(ex["npv"]))
})

test_that("VAL-psychopdaROC-05 bootstrap coordinates are returned or explicitly unavailable", {
  r <- .valROC_run(bootstrapCI = TRUE, bootstrapReps = 200)
  expect_setequal(tolower(r$bootstrapCITable$asDF$parameter),
    c("auc", "threshold", "sensitivity", "specificity"))
})

test_that("VAL-psychopdaROC-06 plots use the selected operating point", {
  r <- .valROC_run(method = "oc_manual", specifyCutScore = "3", plotROC = TRUE,
    showPrevalencePlot = TRUE, combinePlots = FALSE)
  expect_equal(r$prevalencePlot$get("x")$state$optimal$threshold,
    r$resultsTable$get("x")$asDF$cutpoint)
})

test_that("VAL-psychopdaROC-07 classification is invariant to a translated score scale", {
  d <- .valROC_data()
  a <- .valROC_run(d, method = "oc_manual", specifyCutScore = "3.1")$resultsTable$get("x")$asDF
  d$x <- d$x + 1e12
  b <- .valROC_run(d, method = "oc_manual", specifyCutScore = "1000000000003.1")$resultsTable$get("x")$asDF
  expect_equal(unname(as.matrix(a[, c("sensitivity", "specificity", "ppv", "npv")])),
    unname(as.matrix(b[, c("sensitivity", "specificity", "ppv", "npv")])))
})

test_that("VAL-psychopdaROC-08 power uses available marker observations", {
  d <- .valROC_data(); d$x[c(1, 7, 8)] <- NA
  a <- .valROC_run(d, powerAnalysis = TRUE, powerAnalysisType = "prospective")$powerAnalysisTable$asDF
  b <- .valROC_run(d[!is.na(d$x), ], powerAnalysis = TRUE,
    powerAnalysisType = "prospective")$powerAnalysisTable$asDF
  expect_equal(a[, c("observed_power", "required_n")], b[, c("observed_power", "required_n")])
})

test_that("VAL-psychopdaROC-09 required N is symmetric for a two-sided contrast", {
  d <- .valROC_data()
  a <- .valROC_run(d, powerAnalysis = TRUE)$powerAnalysisTable$asDF$required_n
  d$x <- -d$x
  b <- .valROC_run(d, powerAnalysis = TRUE)$powerAnalysisTable$asDF$required_n
  expect_equal(a, b)
})

test_that("VAL-psychopdaROC-10 a chi-squared statistic is not its p-value", {
  r <- .valROC_run(method = "oc_manual", specifyCutScore = "3", metric = "p_chisquared")
  expected <- suppressWarnings(chisq.test(matrix(c(4, 2, 3, 3), 2), correct = FALSE)$statistic)
  expect_equal(r$resultsTable$get("x")$asDF$cutpoint, 3)
  expect_false(grepl("built-in Youden", r$runSummary$content, fixed = TRUE))
  expect_equal(r$resultsTable$get("x")$asDF$metricValue, unname(expected))
})

test_that("VAL-psychopdaROC-11 empty predicted groups have undefined predictive values", {
  r <- .valROC_run(showThresholdTable = TRUE, maxThresholds = 100)$thresholdTable$asDF
  expect_true(is.na(r$ppv[is.infinite(r$threshold)]))
  expect_true(is.na(r$npv[r$threshold == min(.valROC_data()$x)]))
})

test_that("VAL-psychopdaROC-12 quantile points use the quantile decision rule", {
  d <- .valROC_data()
  r <- .valROC_run(plotROC = TRUE, combinePlots = FALSE, quantileCIs = TRUE)
  dest <- tempfile(fileext = ".png"); on.exit(unlink(dest), add = TRUE)
  r$plotROC$get("x")$saveAs(dest)
  pts <- Filter(function(l) identical(l$aes_params$shape, 4), ggplot2::last_plot()$layers)[[1]]$data
  cut <- unname(quantile(d$x, .75))
  expect_equal(pts$y[pts$q_label == "q=0.75"],
    unname(.valROC_counts(d$x, d$status == "pos", cut)["sensitivity"]))
})

test_that("VAL-psychopdaROC-13 requested fixed ROC produces an operating-point image", {
  r <- .valROC_run(fixedSensSpecAnalysis = TRUE, showFixedROC = TRUE, combinePlots = TRUE)
  expect_gt(length(r$fixedSensSpecROC$itemKeys), 0)
})

test_that("VAL-psychopdaROC-14 strict two-sided power includes both rejection tails", {
  d <- data.frame(status = factor(rep(c("neg", "pos"), each = 6)), x = rep(1:6, 2))
  r <- .valROC_run(d, powerAnalysis = TRUE)$powerAnalysisTable$asDF
  expect_equal(r$observed_power, .05)
})

test_that("VAL-psychopdaROC-15 finite-sample approximation is explicitly qualified", {
  d <- withr::with_seed(20261010, data.frame(
    status = factor(c(rep("neg", 50), rep("pos", 10))),
    x = c(rnorm(50), rnorm(10, .8))))
  r <- .valROC_run(d)
  notes <- r$aucSummaryTable$.__enclos_env__$private$.notes
  text <- paste(vapply(notes, function(n) as.character(n$note), character(1)), collapse = " ")
  expect_match(text, "asymptotic|approximation|coverage", ignore.case = TRUE)
})

test_that("classifier ties use one declared rule without recycling warnings", {
  d <- data.frame(status = factor(rep(c("neg", "pos"), each = 10)),
    x = c(1, 2, 3, 3, 4, 5, 6, 7, 8, 10, 3, 5, 6, 8, 9, 11, 12, 13, 14, 15))
  d <- rbind(d, data.frame(status = factor(c("neg", "neg", "pos", "pos")),
    x = c(0, 0, 16, 16)))
  i <- c(14, 9, 15, 10, setdiff(1:24, c(14, 9, 15, 10)))
  for (z in list(d, d[i, ])) {
    expect_no_warning(r <- suppressMessages(ClinicoPath::psychopdaROC(data = z,
      dependentVars = "x", classVar = "status", positiveClass = "pos", negativeClass = "neg", refVar = NULL,
      plotROC = FALSE, compareClassifiers = TRUE)))
    expect_equal(r$rocComparisonTable$asDF$accuracy, 19 / 24)
    expect_equal(r$rocComparisonTable$asDF$f1_score, 18 / 23)
  }
})

test_that("bootstrap coordinate limits are scalar percentile limits with declared ties", {
  for (perfect in c(FALSE, TRUE)) {
    d <- .valROC_data()
    if (perfect) d$x <- 1:12
    r <- .valROC_run(d, bootstrapCI = TRUE, bootstrapReps = 200)
    tab <- r$bootstrapCITable$asDF
    expect_equal(nrow(tab), 4L)
    expect_true(all(is.finite(tab$ci_lower)))
    expect_true(all(is.finite(tab$ci_upper)))
    expect_true(all(tab$ci_lower <= tab$ci_upper))
    expect_match(.valROC_note(r$bootstrapCITable, "estimand"), "separate Youden reference")
    expect_false(grepl("value is not atomic", r$runSummary$content, fixed = TRUE))
  }
})

test_that("fixed-cutoff rules agree with counts for either direction", {
  d <- .valROC_data()
  for (direction in c(">=", "<=")) {
    for (kind in c("sensitivity", "specificity")) {
      for (method in c("linear", "nearest", "stepwise")) {
        r <- .valROC_run(d, direction = direction, fixedSensSpecAnalysis = TRUE,
          fixedAnalysisType = kind, fixedInterpolation = method, usePriorPrev = TRUE,
          priorPrev = .1, fixedSensitivityValue = .95, fixedSpecificityValue = .95)
        t <- r$fixedSensSpecTable$asDF
        ex <- .valROC_counts(d$x, d$status == "pos", t$cutpoint, direction, .1)
        expect_equal(t$achieved_sensitivity, unname(ex["sensitivity"]))
        expect_equal(t$achieved_specificity, unname(ex["specificity"]))
        expect_equal(t$accuracy, unname(ex["accuracy"]))
        expect_equal(t$ppv, unname(ex["ppv"]))
        expect_equal(t$npv, unname(ex["npv"]))
      }
    }
  }
})

test_that("power tails are coherent away from the null in imbalanced samples", {
  d <- .valROC_data(); d <- d[c(1:6, 7:10), ]
  for (alpha in c(.01, .05, .1)) {
    r <- .valROC_run(d, powerAnalysis = TRUE, significanceLevel = alpha)
    a <- r$aucSummaryTable$asDF$auc
    np <- sum(d$status == "pos"); nn <- sum(d$status == "neg")
    v <- (a * (1 - a) + (np - 1) * (a / (2 - a) - a^2) +
      (nn - 1) * (2 * a^2 / (1 + a) - a^2)) / (np * nn)
    mu <- abs(a - .5) / sqrt(v); z <- qnorm(1 - alpha / 2)
    expect_equal(r$powerAnalysisTable$asDF$observed_power, pnorm(mu - z) + pnorm(-mu - z))
  }
})

test_that("combined ROC and criterion plots retain the selected cutoff", {
  r <- .valROC_run(vars = c("x", "z"), method = "oc_manual", specifyCutScore = "3",
    plotROC = TRUE, combinePlots = TRUE, showOptimalPoint = TRUE,
    directLabel = TRUE, compareClassifiers = TRUE)
  dest <- tempfile(fileext = ".png"); on.exit(unlink(dest), add = TRUE)
  expect_true(r$plotROC$get(1)$saveAs(dest))
  points <- Filter(function(l) identical(l$aes_params$shape, 18),
    ggplot2::last_plot()$layers)[[1]]$data
  expect_equal(nrow(points), 2L)
  for (v in c("x", "z")) {
    expected <- .valROC_counts(.valROC_data()[[v]], .valROC_data()$status == "pos", 3)
    expect_equal(points$sensitivity[points$var == v], unname(expected["sensitivity"]))
    expect_equal(points$specificity[points$var == v], unname(expected["specificity"]))
  }
  expect_match(.valROC_note(r$rocComparisonTable, "classifier_rule"),
    "a normalized assay is not necessarily a probability", fixed = TRUE)
  s <- .valROC_run(method = "oc_manual", specifyCutScore = "3.1", plotROC = TRUE,
    combinePlots = FALSE, showCriterionPlot = TRUE)
  expect_true(s$criterionPlot$get("x")$saveAs(dest))
  line <- Filter(function(l) inherits(l$geom, "GeomVline"), ggplot2::last_plot()$layers)[[1]]
  expect_equal(line$data$xintercept, 3.1)
  inverse <- transform(.valROC_data(), x = -x, z = -z)
  t <- .valROC_run(inverse, vars = c("x", "z"), plotROC = TRUE,
    combinePlots = TRUE, directLabel = TRUE)
  expect_true(t$plotROC$get(1)$saveAs(dest))
})
