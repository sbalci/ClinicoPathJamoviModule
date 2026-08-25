# Defects found by cross-validating psychopdaROC against its sibling enhancedROC. Each test
# names the wrong number the user used to be shown.

library(testthat)

pc_run <- function(...) suppressWarnings(do.call(ClinicoPath::psychopdaROC, list(...)))
pc_data <- function(seed = 42, n = 300) {
    set.seed(seed)
    g <- rbinom(n, 1, 0.34)
    data.frame(gold = factor(ifelse(g == 1, "Positive", "Negative"), levels = c("Negative", "Positive")),
               m1 = g * 1.2 + rnorm(n))
}
# Vickers & Elkin (Med Decis Making 2006;26:565-74) on the same logistic risk the module fits.
pc_hand_nb <- function(d, pt) {
    y <- as.integer(d$gold == "Positive")
    risk <- as.numeric(stats::fitted(stats::glm(y ~ d$m1, family = stats::binomial())))
    treat <- risk >= pt
    sum(treat & y == 1) / length(y) - sum(treat & y == 0) / length(y) * (pt / (1 - pt))
}

test_that("net benefit matches Vickers & Elkin, not a frozen Youden rule", {
    # clinicalUtilityTable used to hold sens/spec at the Youden cutoff and vary only the odds
    # weight - a different estimand, reading 0.2152 where the published formula gives 0.2759.
    d <- pc_data()
    for (pt in c(0.10, 0.20, 0.30)) {
        r <- pc_run(data = d, classVar = "gold", dependentVars = "m1", positiveClass = "Positive",
                    refVar = NULL, clinicalUtilityAnalysis = TRUE,
                    treatmentThreshold = sprintf("%s,%s,0.1", pt, pt))
        cu <- as.data.frame(r$clinicalUtilityTable)
        expect_equal(as.numeric(cu$net_benefit[1]), pc_hand_nb(d, pt), tolerance = 1e-9,
                     info = paste("pt =", pt))
    }
})

test_that("psychopdaROC and enhancedROC agree on net benefit at a matched threshold", {
    d <- pc_data()
    P <- pc_run(data = d, classVar = "gold", dependentVars = "m1", positiveClass = "Positive",
                refVar = NULL, clinicalUtilityAnalysis = TRUE, treatmentThreshold = "0.1,0.1,0.1")
    E <- suppressWarnings(do.call(ClinicoPath::enhancedROC, list(
        data = d, outcome = "gold", predictors = "m1", positiveClass = "Positive",
        clinicalImpact = TRUE, useObservedPrevalence = FALSE, prevalence = 0.10)))
    p_nb <- as.numeric(as.data.frame(P$clinicalUtilityTable)$net_benefit[1])
    e_nb <- as.numeric(as.data.frame(E$results$clinicalImpactTable)$net_benefit_per_100[1]) / 100
    expect_equal(p_nb, e_nb, tolerance = 1e-9)
    expect_equal(p_nb, pc_hand_nb(d, 0.10), tolerance = 1e-9)
})

test_that("the treatment threshold range actually drives the decision curve", {
    # decisionCurveTable used a hardcoded seq(0.01, 0.90, length.out = 20) and was byte-identical
    # whatever the user typed.
    d <- pc_data()
    grab <- function(rng) {
        dc <- as.data.frame(pc_run(data = d, classVar = "gold", dependentVars = "m1",
                                   positiveClass = "Positive", refVar = NULL,
                                   clinicalUtilityAnalysis = TRUE,
                                   treatmentThreshold = rng)$decisionCurveTable)
        round(as.numeric(dc$threshold), 6)
    }
    expect_equal(grab("0.1,0.3,0.1"), c(0.1, 0.2, 0.3))
    expect_equal(grab("0.2,0.4,0.05"), c(0.20, 0.25, 0.30, 0.35, 0.40))
    expect_false(identical(grab("0.1,0.3,0.1"), grab("0.2,0.4,0.05")))
})

test_that("a malformed threshold range falls back instead of aborting", {
    d <- pc_data()
    r <- expect_no_error(pc_run(data = d, classVar = "gold", dependentVars = "m1",
                                positiveClass = "Positive", refVar = NULL,
                                clinicalUtilityAnalysis = TRUE, treatmentThreshold = "garbage"))
    expect_gt(nrow(as.data.frame(r$decisionCurveTable)), 0)
})

test_that("each decision-curve row names the strategies it has to beat", {
    d <- pc_data()
    dc <- as.data.frame(pc_run(data = d, classVar = "gold", dependentVars = "m1",
                               positiveClass = "Positive", refVar = NULL,
                               clinicalUtilityAnalysis = TRUE,
                               treatmentThreshold = "0.1,0.2,0.1")$decisionCurveTable)
    expect_true(all(grepl("treat all", dc$clinical_value, fixed = TRUE)))
    expect_true(all(grepl("treat none", dc$clinical_value, fixed = TRUE)))
})

test_that("accuracy stays empirical when a prior prevalence is assumed", {
    # The prior is documented as applying to PREDICTIVE VALUES. Letting it reach the Accuracy
    # column turned it into expected accuracy in the assumed population (0.849 vs 0.767) under
    # an unchanged heading, contradicting the analysis's own fixedSensSpecTable.
    d <- pc_data()
    grab <- function(...) {
        tt <- as.data.frame(pc_run(data = d, classVar = "gold", dependentVars = "m1",
                                   positiveClass = "Positive", refVar = NULL,
                                   showThresholdTable = TRUE, ...)$thresholdTable)
        as.numeric(tt$accuracy)
    }
    plain <- grab()
    prior <- grab(usePriorPrev = TRUE, priorPrev = 0.10)
    expect_equal(plain, prior, tolerance = 1e-12)
})

test_that("AUC verbal bands match enhancedROC across the whole range", {
    for (target in c(0.95, 0.85, 0.75, 0.65, 0.55, 0.30)) {
        set.seed(7)
        n <- 400
        g <- rbinom(n, 1, 0.5)
        sh <- qnorm(if (target >= 0.5) target else 1 - target) * sqrt(2)
        m <- g * sh + rnorm(n)
        if (target < 0.5) m <- -m
        d <- data.frame(y = factor(ifelse(g == 1, "P", "N"), levels = c("N", "P")), m = m)
        E <- suppressWarnings(do.call(ClinicoPath::enhancedROC, list(
            data = d, outcome = "y", predictors = "m", positiveClass = "P", direction = "higher")))
        P <- pc_run(data = d, classVar = "y", dependentVars = "m", positiveClass = "P", refVar = NULL)
        eb <- as.character(as.data.frame(E$results$aucSummary)$auc_interpretation[1])
        pb <- as.character(as.data.frame(P$clinicalInterpretationTable)$performance_level[1])
        expect_identical(eb, pb, info = paste("target AUC", target))
    }
})

test_that("neither analysis calls a below-0.5 marker 'reversed' from the point estimate alone", {
    # Under the null the AUC is symmetric about 0.5, so ~49% of uninformative markers land below
    # it by chance (simulated). Asserting reversal there would tell users to flip Direction on
    # noise, flipping the cutpoint and sens/spec with it.
    src_p <- paste(readLines(test_path("..", "..", "R", "psychopdaROC.b.R"), warn = FALSE), collapse = "\n")
    src_e <- paste(readLines(test_path("..", "..", "R", "enhancedROC.b.R"), warn = FALSE), collapse = "\n")
    expect_false(grepl('Below chance (reversed)', src_p, fixed = TRUE))
    expect_false(grepl('Below chance (reversed)', src_e, fixed = TRUE))
})

test_that("a non-finite cut-point is suppressed rather than printed", {
    # A backwards marker used to produce "cutpoint Inf, sensitivity 0, PPV 0" - an operating
    # point no clinician can apply to a patient.
    set.seed(55)
    n <- 300
    g <- rbinom(n, 1, 0.45)
    d <- data.frame(y = factor(ifelse(g == 1, "P", "N"), levels = c("N", "P")),
                    m = -(g * 1.4 + rnorm(n)))                    # LOWER = disease
    P <- pc_run(data = d, classVar = "y", dependentVars = "m", positiveClass = "P", refVar = NULL)
    rt <- as.data.frame(P$resultsTable$get(key = "m"))
    expect_equal(nrow(rt), 0L)
    # the AUC is still reported - only the unusable operating point is withheld
    expect_true(is.finite(as.numeric(as.data.frame(P$aucSummaryTable)$auc[1])))
    # and the correctly-oriented marker still gets a real cut-point
    d2 <- data.frame(y = d$y, m = -d$m)
    P2 <- pc_run(data = d2, classVar = "y", dependentVars = "m", positiveClass = "P", refVar = NULL)
    rt2 <- as.data.frame(P2$resultsTable$get(key = "m"))
    expect_equal(nrow(rt2), 1L)
    expect_true(is.finite(as.numeric(rt2$cutpoint[1])))
})

test_that("the seed option really is wired (it is - do not 'fix' it)", {
    # Recorded because a grep for set.seed finds nothing: the file uses withr::local_seed.
    d <- pc_data()
    ci <- function(sd) {
        b <- as.data.frame(pc_run(data = d, classVar = "gold", dependentVars = "m1",
                                  positiveClass = "Positive", refVar = NULL,
                                  bootstrapCI = TRUE, bootstrapReps = 200, seed = sd)$bootstrapCITable)
        as.numeric(b$ci_lower[1])
    }
    expect_equal(ci(123), ci(123))
    expect_false(isTRUE(all.equal(ci(123), ci(999))))
})
