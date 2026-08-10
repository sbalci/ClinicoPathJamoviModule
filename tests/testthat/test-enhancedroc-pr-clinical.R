# Precision-recall and clinical-metric behaviour for enhancedROC.
#
# This file used to build a hand-rolled jmvcore (MockTable/MockHtml/MockImage), assign a fake
# `enhancedROCBase` into the global environment and source() the backend. That harness drifted
# from the real backend -- MockTable implemented only addRow/setRow/asDF -- and the whole file
# died with "attempt to apply non-function" the moment the backend called anything else. It
# tested the mock, not the analysis. The three assertions it was making are worth keeping, so
# they are made here against the real function.

run_er <- function(...) {
    args <- utils::modifyList(
        list(outcome = "outcome", positiveClass = "Case", analysisType = "single"),
        list(...))
    do.call(ClinicoPath::enhancedROC, args)
}

imbalanced_data <- function(seed = 123) {
    set.seed(seed)
    data.frame(
        outcome = factor(c(rep("Case", 20), rep("Control", 80)), levels = c("Control", "Case")),
        pred    = c(rnorm(20, 2), rnorm(80, 0)))
}

balanced_data <- function(seed = 123) {
    set.seed(seed)
    data.frame(
        outcome = factor(c(rep("Case", 30), rep("Control", 20)), levels = c("Control", "Case")),
        marker  = c(rnorm(30, 2), rnorm(20, 0)))
}


test_that("precision-recall metrics are produced for imbalanced data", {
    d <- imbalanced_data()
    pr <- run_er(data = d, predictors = "pred", detectImbalance = TRUE)$results$precisionRecallTable$asDF

    expect_equal(nrow(pr), 1L)
    expect_gte(pr$auc_pr[1], 0)
    expect_lte(pr$auc_pr[1], 1)
    # a genuinely discriminating marker on 20% prevalence should beat the PR baseline,
    # which is the positive-class proportion
    expect_gt(pr$auc_pr[1], 0.20)
    for (col in c("f1_score", "precision", "recall", "average_precision")) {
        expect_gte(pr[[col]][1], 0, label = col)
        expect_lte(pr[[col]][1], 1, label = col)
    }
})


test_that("clinical metrics use the observed prevalence, or the supplied one", {
    d <- balanced_data()
    observed <- mean(d$outcome == "Case")

    obs <- run_er(data = d, predictors = "marker", clinicalMetrics = TRUE,
                  useObservedPrevalence = TRUE)$results$clinicalApplicationMetrics$asDF
    expect_equal(as.numeric(obs$prevalence[1]), observed, tolerance = 1e-8)

    override <- run_er(data = d, predictors = "marker", clinicalMetrics = TRUE,
                       useObservedPrevalence = FALSE,
                       prevalence = 0.2)$results$clinicalApplicationMetrics$asDF
    expect_equal(as.numeric(override$prevalence[1]), 0.2, tolerance = 1e-8)

    # PPV must move with prevalence -- that is the whole point of the option. The observed
    # prevalence here is 0.6, so forcing 0.2 must lower PPV.
    expect_lt(as.numeric(override$ppv[1]), as.numeric(obs$ppv[1]))

    # ...and PPV must equal Bayes at the stated prevalence, given the table's own LR+
    lr_pos <- as.numeric(override$lr_positive[1])
    p <- 0.2
    expect_equal(as.numeric(override$ppv[1]),
                 (p * lr_pos) / (p * lr_pos + (1 - p)), tolerance = 1e-6)
})


test_that("the clinical impact table populates with net benefit", {
    d <- balanced_data()
    impact <- run_er(data = d, predictors = "marker", clinicalImpact = TRUE,
                     decisionImpactTable = TRUE)$results$clinicalImpactTable$asDF

    expect_gte(nrow(impact), 1L)
    expect_true("net_benefit_per_100" %in% colnames(impact))
    expect_true(all(is.finite(impact$net_benefit_per_100)))
})
