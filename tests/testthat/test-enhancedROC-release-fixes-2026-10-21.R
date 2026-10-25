# Release fixes 2026-10-21 (independent review IR26-01..04, cross-analysis review XA-03, XA-05).

library(testthat)

rf_run <- function(...) suppressWarnings(do.call(ClinicoPath::enhancedROC, utils::modifyList(list(validationNegative = NULL), list(...), keep.null = TRUE)))
rf_private <- function(d, ...) {
    o <- do.call(ClinicoPath:::enhancedROCOptions$new, list(...))
    a <- ClinicoPath:::enhancedROCClass$new(options = o, data = d)
    a$init()
    list(a = a, p = a$.__enclos_env__$private)
}

# F5 fixture: AUC 0.918 read `higher`, but one outlying control (250) makes mean(cases) <
# mean(controls), so the fitted logistic slope is negative.
rf_f5 <- function() {
    cases <- 2 + qnorm(ppoints(30)); ctrls <- qnorm(ppoints(70)); ctrls[70] <- 250
    data.frame(y = factor(rep(c("pos", "neg"), c(30, 70)), levels = c("neg", "pos")), m = c(cases, ctrls))
}
rf_risk_options <- list(outcome = "y", positiveClass = "pos", predictors = "m",
    calibrationAnalysis = TRUE, brierScore = TRUE, hosmerLemeshow = TRUE, calibrationPlot = TRUE,
    decisionImpactCurves = TRUE, clinicalImpact = TRUE, clinicalUtilityCurve = TRUE, impactCutoffs = "1")

test_that("IR26-01: a slope against Direction keeps the fitted risk model and is disclosed", {
    skip_if_not_installed("pROC")
    d <- rf_f5()
    r <- do.call(rf_run, c(list(data = d), rf_risk_options))
    grid <- r$results$decisionImpactPlot$state$curves$grid
    prev <- mean(d$y == "pos")
    treat_all <- pmax(prev - (1 - prev) * grid$Threshold / (1 - grid$Threshold), 0)
    # was: identical to max(treat-all, treat-none) at every threshold (constant-prevalence model)
    expect_false(isTRUE(all.equal(grid$NetBenefit, treat_all)))
    # the curve is the fitted glm's: oracle at pt = 0.3
    fit <- stats::fitted(stats::glm(I(d$y == "pos") ~ d$m, family = stats::binomial()))
    yb <- d$y == "pos"
    nb <- function(t) mean(fit >= t & yb) - mean(fit >= t & !yb) * t / (1 - t)
    expect_equal(grid$NetBenefit[grid$Threshold == 0.3], nb(0.3), tolerance = 1e-10)
    # Brier is the fitted model's, no longer p(1 - p)
    expect_equal(r$results$calibrationSummary$getCell(rowNo = 1, "brier_score")$value, mean((fit - yb)^2), tolerance = 1e-10)
    html <- r$results$notices$content
    expect_match(html, "Risk Model Runs Against the ROC Direction: m", fixed = TRUE)
    expect_match(html, "ranks patients well (AUC 0.918)", fixed = TRUE)
})

test_that("IR26-01: data opposite to a fixed Direction say the marker performs worse than shown", {
    skip_if_not_installed("pROC")
    set.seed(7)
    d <- data.frame(y = factor(rep(c("neg", "pos"), each = 40), levels = c("neg", "pos")),
                    m = c(rnorm(40, 1), rnorm(40, 0)))
    r <- rf_run(data = d, outcome = "y", positiveClass = "pos", predictors = "m", direction = "higher",
                calibrationAnalysis = TRUE, brierScore = TRUE)
    html <- r$results$notices$content
    expect_match(html, "Risk Model Runs Against the ROC Direction: m", fixed = TRUE)
    expect_match(html, "performs worse than those outputs show", fixed = TRUE)
    # same marker read the supported way: no reversal notice
    r2 <- rf_run(data = d, outcome = "y", positiveClass = "pos", predictors = "m", direction = "lower",
                 calibrationAnalysis = TRUE, brierScore = TRUE)
    expect_false(grepl("Risk Model Runs Against", r2$results$notices$content, fixed = TRUE))
    expect_equal(r$results$calibrationSummary$getCell(rowNo = 1, "brier_score")$value,
                 r2$results$calibrationSummary$getCell(rowNo = 1, "brier_score")$value)
})

test_that("IR26-02/03: HL runs on the fitted model, and internal validation shares it", {
    skip_if_not_installed("pROC")
    r <- do.call(rf_run, c(list(data = rf_f5(), internalValidation = TRUE, validationMethod = "bootstrap",
                                bootstrapSamples = 100), rf_risk_options))
    expect_equal(r$results$hosmerLemeshowTable$rowCount, 1)   # was: skipped, "very few unique values"
    expect_false(grepl("very few unique values", r$results$notices$content, fixed = TRUE))
    expect_match(r$results$analysisSummary$content, "Apparent AUC = 0.082", fixed = TRUE)
})

test_that("IR26-02: a constant marker keeps the prevalence and the HL notice names the real cause", {
    skip_if_not_installed("pROC")
    pr <- rf_private(rf_f5(), outcome = "y", positiveClass = "pos", predictors = "m")
    risk <- pr$p$.riskProbabilities(rep(3, 100), rep(c(0, 1), c(70, 30)))
    expect_true(risk$constant)
    expect_equal(unique(risk$probs), 0.3)
})

test_that("IR26-04: every predictor failing rejects the analysis", {
    skip_if_not_installed("pROC")
    pr <- rf_private(rf_f5(), outcome = "y", positiveClass = "pos", predictors = "m")
    env <- pr$p
    unlockBinding(".calculateOptimalCutoff", env)
    assign(".calculateOptimalCutoff", function(roc_obj, data, predictor) stop("boom"), envir = env)
    expect_error(env$.run(), "No Predictor Could Be Analyzed.*boom")
})

test_that("XA-03: an entered prevalence of 0.0004 is not shown as 0.000", {
    skip_if_not_installed("pROC")
    r <- rf_run(data = rf_f5(), outcome = "y", positiveClass = "pos", predictors = "m",
                useObservedPrevalence = FALSE, prevalence = 0.0004, clinicalContext = "screening",
                clinicalMetrics = TRUE)
    txt <- r$results$optimalCutoffSummary$getCell(rowNo = 1, "clinical_recommendation")$value
    expect_match(txt, "the prevalence you entered (0.0004)", fixed = TRUE)
    expect_false(grepl("(0.000)", txt, fixed = TRUE))
})

test_that("XA-05: the AUC interval method is named with table notes off (the default)", {
    skip_if_not_installed("pROC")
    r <- rf_run(data = rf_f5(), outcome = "y", positiveClass = "pos", predictors = "m")
    note <- r$results$aucSummary$notes$ci_method$note
    expect_match(note, "DeLong's method", fixed = TRUE)
    expect_match(note, "Wald intervals on the AUC scale", fixed = TRUE)
})
