# Exercise the current generated bases, not an older installed wrapper.
gas_live_env <- new.env(parent = asNamespace("jmvcore"))
gas_live_root <- testthat::test_path("..", "..")
for (file in c("utils.R", "utils-diagnostic-validation.R", "enhancedROC-validation.R",
               "utils-diagnostic-comparator.R"))
  sys.source(file.path(gas_live_root, "R", file), gas_live_env)
gas_live <- function(name, data = data.frame(), options = list()) {
  for (suffix in c(".h.R", ".b.R"))
    sys.source(file.path(gas_live_root, "R", paste0(name, suffix)), gas_live_env)
  opts <- do.call(get(paste0(name, "Options"), gas_live_env)$new, options)
  analysis <- get(paste0(name, "Class"), gas_live_env)$new(options = opts, data = data)
  analysis$run()
  analysis
}

gas_live_data <- function() data.frame(y = factor(rep(c("P", "N", "P", "N"), c(8, 1, 2, 9))),
  score = rep(c(.9, .8, .2, .1), c(8, 1, 2, 9)), patient = seq_len(20))

test_that("compiled impact outputs separate cutoff, prevalence and utility", {
  run <- function(...) gas_live("enhancedROC", gas_live_data(), utils::modifyList(
    list(outcome = "y", positiveClass = "P", predictors = "score", clinicalImpact = TRUE,
      impactCutoffs = "0.5", decisionImpactTable = TRUE, useObservedPrevalence = FALSE,
      prevalence = .001, workloadPopulationSize = 10000), list(...)))
  a <- run()
  w <- a$results$results$screeningWorkload$asDF
  expect_equal(w$tp, 8)
  expect_equal(w$fp, 999)
  expect_equal(w$ppv, 8/1007)
  b <- run(prevalence = .0001)
  expect_equal(b$results$results$screeningWorkload$asDF$tp, .8)
  expect_equal(b$results$results$screeningWorkload$asDF$cutoff, .5)
  c <- run(utilityThreshold = .3)
  w2 <- c$results$results$screeningWorkload$asDF
  expect_equal(w[setdiff(names(w), 'net_benefit')], w2[setdiff(names(w2), 'net_benefit')])
  expect_lt(w2$net_benefit, w$net_benefit)
  expect_equal(a$results$results$decisionImpactSummary$asDF$n_high_risk, w$flags)
  # Run the same instance twice: no appended rows or stale notes.
  a$run()
  expect_equal(a$results$results$screeningWorkload$rowCount, 1)
  expect_null(formals(gas_live_env$enhancedROC)$patientID)
  expect_null(formals(gas_live_env$enhancedROC)$siteID)
  expect_null(formals(gas_live_env$enhancedROC)$partitionID)
})

test_that("binary decision analyses display their declared validation context", {
  for (name in c("decision", "decisioncompare")) {
    a <- gas_live(name, options = list(showValidationContext = TRUE,
      modelVersion = "model-v2", cohortRole = "external", analysisUnit = "patient"))
    t <- a$results$validationContext$asDF
    expect_equal(t$value[t$setting == "Model version"], "model-v2")
    expect_equal(t$value[t$setting == "Unit of observation"], "patient")
    a$run()
    expect_equal(a$results$validationContext$rowCount, 7)
  }
})

test_that("compiled frozen report supplies denominators and keeps bootstrap paired", {
  d <- gas_live_data(); d$other <- d$score
  a <- gas_live("enhancedROC", d, list(outcome = "y", positiveClass = "P",
    predictors = c("score", "other"), validationPanel = TRUE, validationNegative = "N",
    modelVersion = "frozen-v1", patientID = "patient", validationBootstrap = TRUE,
    validationDraws = 100, impactCutoffs = "0.5", clinicalImpact = TRUE,
    prevalenceScenarios = "0.0001"))
  m <- a$results$results$frozenMetrics$asDF
  expect_equal(nrow(m), 39)
  expect_equal(unique(m$n), 20)
  expect_equal(unique(m$positives), 10)
  expect_equal(unique(m$clusters), 20)
  expect_equal(m$estimate[m$model == "score" & m$metric == "Sensitivity"], .8)   # display labels since the release review
  expect_equal(m$estimate[m$model == "score" & m$metric == "Specificity"], .9)
  paired <- m[m$model == "other minus score", ]
  expect_true(all(is.na(paired$lower)))
  expect_equal(a$results$results$screeningWorkload$rowCount, 4)
  expect_equal(a$results$results$aucSummary$rowCount, 0)
  a$run()
  expect_equal(a$results$results$frozenMetrics$rowCount, 39)
})

test_that("actual psychopdaROC PR helper fixes the perfect classifier regression", {
  for (suffix in c(".h.R", ".b.R"))
    sys.source(file.path(gas_live_root, "R", paste0("psychopdaROC", suffix)), gas_live_env)
  fn <- gas_live_env$psychopdaROCClass$private_methods$.calculatePrecisionRecall
  bound <- new.env(parent = gas_live_env)
  bound$self <- list(options = list(direction = ">="))
  environment(fn) <- bound
  expect_equal(fn(c(.1, .2, .8, .9), c("N", "N", "P", "P"), "P")$auprc, 1)
})

test_that("compiled calculator preserves projected fractions below 0.1 percent prevalence", {
  a <- gas_live("decisioncalculator", options = list(TP = 8, FP = 1, FN = 2, TN = 9,
    showWorkload = TRUE, prevalenceScenarios = "0.0001", workloadPopulationSize = 10000))
  w <- a$results$screeningWorkload$asDF
  expect_equal(w$source, c("Observed", "Projected"))
  expect_equal(w$tp, c(8, .8))
  expect_equal(w$fp, c(1, 999.9))
})

test_that("compiled precision planner uses independent positives and the exact lower bound", {
  a <- gas_live("diagnosticsamplesize", options = list(zeroMissPlanning = TRUE,
    minimumSensitivityBound = .95, prevalence = .0001))
  t <- a$results$zeroMissPlan$asDF
  expect_equal(t$positives, 72)
  expect_gte(t$lower, .95)
  expect_lt(.025^(1/71), .95)
  expect_equal(t$total, 720000)
})

test_that("compiled PR draft honors area method and paired permutation", {
  d <- data.frame(y = factor(c("P", "N", "P", "N")), a = c(.9, .8, .7, .6))
  d$b <- d$a
  a <- gas_live("precisionrecall", d, list(outcome = "y", positiveClass = "P",
    scores = c("a", "b"), aucMethod = "average_precision", comparison = TRUE,
    comparisonMethod = "permutation", ciSamples = 100))
  expect_equal(a$results$aucTable$asDF$auc, rep(5/6, 2))
  expect_equal(a$results$comparisonTable$asDF$pValue, 1)
  a$run()
  expect_equal(a$results$aucTable$rowCount, 2)
})


test_that("PR plot baselines use the same rows as each model's curve", {
  d <- data.frame(y = factor(c("P", "P", "N", "N")),
    a = c(.9, .8, .2, .1), b = c(.9, NA, .2, .1))
  a <- gas_live("precisionrecall", d, list(outcome = "y", positiveClass = "P",
    scores = c("a", "b")))
  expect_equal(a$results$aucTable$asDF$baseline, c(.5, 1/3))
  expect_equal(unname(a$results$prcPlot$state$baseline), c(.5, 1/3))
})
