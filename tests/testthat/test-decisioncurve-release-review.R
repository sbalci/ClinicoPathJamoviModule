# ═══════════════════════════════════════════════════════════
# Release-review regression tests: decisioncurve
# ═══════════════════════════════════════════════════════════
#
# Net benefit, net interventions avoided and standardized net benefit are
# checked against the dcurves package (Sjoberg), not against the module's own
# arithmetic. dcurves::df_binary is a 750-row cohort with 14% prevalence.

library(testthat)

dca_fixture <- function() {
  skip_if_not_installed("dcurves")
  d <- dcurves::df_binary
  d$outcome <- factor(ifelse(d$cancer, "Cancer", "No cancer"),
                      levels = c("No cancer", "Cancer"))
  d$rule <- factor(d$famhistory, levels = c(0, 1))
  d
}

dca_thresholds <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30)

dca_reference <- function(d) {
  ref <- dcurves::dca(cancer ~ cancerpredmarker + famhistory, data = d,
                      thresholds = dca_thresholds)
  list(
    nb  = as.data.frame(dplyr::as_tibble(ref)),
    nia = as.data.frame(dplyr::as_tibble(dcurves::net_intervention_avoided(ref, nper = 100))),
    snb = as.data.frame(dplyr::as_tibble(dcurves::standardized_net_benefit(ref)))
  )
}

run_release_dca <- function(d, ...) {
  # do.call: the jamovi wrapper resolves bare symbols as column names, so passing a
  # variable that holds NULL or a name through a helper needs already-evaluated values.
  args <- list(
    data = d, outcome = "outcome", outcomePositive = "Cancer",
    models = c("cancerpredmarker", "famhistory"),
    thresholdRange = "custom", thresholdMin = 0.05, thresholdMax = 0.30,
    thresholdStep = 0.05,
    selectedThresholds = "0.05, 0.10, 0.15, 0.20, 0.25, 0.30"
  )
  extra <- list(...)
  if (!"decisionRuleVar" %in% names(extra)) args["decisionRuleVar"] <- list(NULL)
  if (!"decisionRulePositive" %in% names(extra)) args["decisionRulePositive"] <- list(NULL)
  do.call(ClinicoPath::decisioncurve, c(args, extra))
}

test_that("net benefit for models and treat-all matches dcurves exactly", {
  d <- dca_fixture(); ref <- dca_reference(d)
  res <- run_release_dca(d, showTable = TRUE)
  got <- res$resultsTable$asDF
  expect_equal(nrow(got), length(dca_thresholds))
  pick <- function(v) ref$nb$net_benefit[ref$nb$variable == v][order(ref$nb$threshold[ref$nb$variable == v])]
  expect_equal(got$model_cancerpredmarker, pick("cancerpredmarker"), tolerance = 1e-10)
  expect_equal(got$model_famhistory,       pick("famhistory"),       tolerance = 1e-10)
  expect_equal(got$treat_all,              pick("all"),              tolerance = 1e-10)
  expect_equal(got$treat_none, rep(0, length(dca_thresholds)))
})

test_that("net interventions avoided per 100 matches dcurves", {
  d <- dca_fixture(); ref <- dca_reference(d)
  res <- run_release_dca(d, calculateClinicalImpact = TRUE, populationSize = 100)
  got <- res$clinicalImpactTable$asDF
  got <- got[got$model == "cancerpredmarker", ]
  got <- got[order(got$threshold), ]
  want <- ref$nia[ref$nia$variable == "cancerpredmarker", ]
  want <- want[order(want$threshold), ]
  expect_equal(got$interventions_avoided, want$net_intervention_avoided, tolerance = 1e-10)
  # projected counts on a population of 100 are the observed rates * 100
  expect_equal(got$true_positives_per_100 / 100, want$tp_rate, tolerance = 1e-10)
  expect_equal(got$false_positives_per_100 / 100, want$fp_rate, tolerance = 1e-10)
})

test_that("standardized net benefit (state fed to the plot) matches dcurves", {
  d <- dca_fixture(); ref <- dca_reference(d)
  res <- run_release_dca(d, showStandardizedNetBenefit = TRUE)
  st <- res$standardizedNetBenefitPlot$state
  expect_false(is.null(st$plotData))
  prev <- mean(st$analysisOutcomes == st$outcomePositive)
  expect_equal(prev, 0.14)
  pd <- st$plotData[st$plotData$model == "cancerpredmarker", ]
  pd <- pd[order(pd$threshold), ]
  want <- ref$snb[ref$snb$variable == "cancerpredmarker", ]
  want <- want[order(want$threshold), ]
  expect_equal(pd$net_benefit / prev, want$standardized_net_benefit, tolerance = 1e-10)
})

test_that("decision consequences agree with dcurves rates and the average NB is the trapezoid", {
  d <- dca_fixture(); ref <- dca_reference(d)
  res <- run_release_dca(d, showDecisionConsequences = TRUE, weightedAUC = TRUE)
  dc <- res$decisionConsequencesTable$asDF
  dc <- dc[dc$model == "cancerpredmarker", ]; dc <- dc[order(dc$threshold), ]
  want <- ref$nb[ref$nb$variable == "cancerpredmarker", ]; want <- want[order(want$threshold), ]
  n <- nrow(d)
  expect_equal(dc$true_positive / n, want$tp_rate, tolerance = 1e-10)
  expect_equal(dc$false_positive / n, want$fp_rate, tolerance = 1e-10)
  expect_equal(dc$sensitivity, want$tp_rate / 0.14, tolerance = 1e-10)
  expect_equal(dc$specificity, 1 - want$fp_rate / 0.86, tolerance = 1e-10)

  # average NB over the range = trapezoid integral / range width, computed from dcurves values
  th <- want$threshold; nb <- want$net_benefit
  trap <- sum(diff(th) * (head(nb, -1) + tail(nb, -1)) / 2) / (max(th) - min(th))
  wa <- res$weightedAUCTable$asDF
  expect_equal(wa$weighted_auc[wa$model == "cancerpredmarker"], trap, tolerance = 1e-10)
})

test_that("the decision curve renders without a ggplot warning when a clinical rule has no CI", {
  # The rule is never bootstrapped, so its rows carry NA interval bounds. geom_ribbon() used
  # to receive them and warn "Removed n rows containing missing values" on every render.
  d <- dca_fixture()
  res <- run_release_dca(
    d, confidenceIntervals = TRUE, bootReps = 100,
    clinicalDecisionRule = TRUE, decisionRuleVar = "rule", decisionRulePositive = "1",
    decisionRuleLabel = "Family history"
  )
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_warning(res$dcaPlot$.render())
  st <- res$dcaPlot$state
  expect_true("Family history" %in% st$plotData$model)
  # the rule is bootstrapped like any model now; only the reference strategies carry NA bounds
  expect_true(all(!is.na(st$plotData$ci_lower[st$plotData$model == "Family history"])))
  expect_true(all(!is.na(st$plotData$ci_lower[st$plotData$model == "cancerpredmarker"])))
  expect_true(all(is.na(st$plotData$ci_lower[st$plotData$model %in% c("Treat All", "Treat None")])))
})

test_that("modelNames = NULL falls back to variable names instead of erroring", {
  d <- dca_fixture()
  res <- run_release_dca(d, modelNames = NULL, showTable = TRUE)
  expect_true(all(c("model_cancerpredmarker", "model_famhistory") %in% names(res$resultsTable$asDF)))
})

# ── Bootstrap bands: clinical rule included, simultaneous band available ──────

test_that("the clinical rule curve carries a bootstrap interval like every model", {
  d <- dca_fixture()
  res <- run_release_dca(
    d, confidenceIntervals = TRUE, bootReps = 200,
    clinicalDecisionRule = TRUE, decisionRuleVar = "rule", decisionRulePositive = "1",
    decisionRuleLabel = "Family history"
  )
  pd <- res$dcaPlot$state$plotData
  rule <- pd[pd$model == "Family history", ]
  expect_true(all(is.finite(rule$ci_lower)) && all(is.finite(rule$ci_upper)))
  expect_true(all(rule$ci_lower <= rule$net_benefit + 1e-12))
  expect_true(all(rule$ci_upper >= rule$net_benefit - 1e-12))
  # reference strategies are deterministic and get no band
  expect_true(all(is.na(pd$ci_lower[pd$model %in% c("Treat All", "Treat None")])))
})

test_that("the simultaneous sup-t band contains the curve and is wider than the pointwise interval", {
  d <- dca_fixture()
  res <- run_release_dca(d, confidenceIntervals = TRUE, bootReps = 500)
  pd <- res$dcaPlot$state$plotData
  m <- pd[pd$model == "cancerpredmarker", ]
  expect_true(all(is.finite(m$sim_lower)) && all(is.finite(m$sim_upper)))
  expect_true(all(m$sim_lower <= m$net_benefit & m$sim_upper >= m$net_benefit))
  # symmetric about the observed curve by construction
  expect_equal(m$sim_upper - m$net_benefit, m$net_benefit - m$sim_lower, tolerance = 1e-10)
  # a band covering the whole curve jointly must be at least as wide as the per-threshold interval
  expect_gt(mean(m$sim_upper - m$sim_lower), mean(m$ci_upper - m$ci_lower))
  expect_true(mean((m$sim_lower <= m$ci_lower) & (m$sim_upper >= m$ci_upper)) >= 0.8)
})

test_that("the band selector defaults to pointwise until the ciBand option is compiled", {
  d <- dca_fixture()
  mk <- function(...) {
    opts <- ClinicoPath:::decisioncurveOptions$new(
      outcome = "outcome", outcomePositive = "Cancer",
      models = c("cancerpredmarker", "famhistory"),
      decisionRuleVar = NULL, decisionRulePositive = NULL,
      confidenceIntervals = TRUE, bootReps = 100, ...)
    an <- ClinicoPath:::decisioncurveClass$new(options = opts, data = d)
    an$.__enclos_env__$private$.run()
    an
  }
  an <- mk()
  expect_equal(an$.__enclos_env__$private$.ciBand(), "pointwise")
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_warning(an$.__enclos_env__$private$.plotDCA(an$results$dcaPlot,
                    ggtheme = ggplot2::theme_bw(), theme = list(fill = "#ffffff")))

  if ("ciBand" %in% names(formals(ClinicoPath::decisioncurve))) {
    an2 <- mk(ciBand = "simultaneous")
    expect_equal(an2$.__enclos_env__$private$.ciBand(), "simultaneous")
    expect_no_warning(an2$.__enclos_env__$private$.plotDCA(an2$results$dcaPlot,
                      ggtheme = ggplot2::theme_bw(), theme = list(fill = "#ffffff")))
  } else {
    skip("ciBand not yet compiled into decisioncurve.h.R; run jmvtools::prepare()")
  }
})
