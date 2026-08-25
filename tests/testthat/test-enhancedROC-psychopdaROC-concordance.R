# The two ROC analyses in meddecide > ROC must agree wherever they compute the same thing.
# Each is an independent implementation (enhancedROC on pROC, psychopdaROC on cutpointr), so
# agreement is a mutual validation and a divergence is a defect in one of them.

library(testthat)

cc_run  <- function(f, ...) suppressWarnings(do.call(f, list(...)))
cc_strip <- function(x) trimws(gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", if (is.null(x)) "" else x)))

cc_data <- function(seed = 101, n = 300) {
    set.seed(seed)
    g <- rbinom(n, 1, 0.42)
    data.frame(gold = factor(ifelse(g == 1, "Positive", "Negative"), levels = c("Negative", "Positive")),
               m1 = g * 1.5 + rnorm(n), m2 = g * 0.8 + rnorm(n))
}

# ── The shared estimators must agree exactly ─────────────────────────────────

test_that("AUC and its DeLong interval agree between the two analyses and with pROC", {
    d <- cc_data()
    E <- cc_run(ClinicoPath::enhancedROC, data = d, outcome = "gold", predictors = c("m1", "m2"),
                positiveClass = "Positive")
    P <- cc_run(ClinicoPath::psychopdaROC, data = d, classVar = "gold", dependentVars = c("m1", "m2"),
                positiveClass = "Positive", refVar = NULL)
    ea <- as.data.frame(E$results$aucSummary)
    pa <- as.data.frame(P$aucSummaryTable)
    for (i in 1:2) {
        v <- c("m1", "m2")[i]
        r <- pROC::roc(d$gold, d[[v]], levels = c("Negative", "Positive"), direction = "<", quiet = TRUE)
        ci <- as.numeric(pROC::ci.auc(r))
        expect_equal(as.numeric(ea$auc[i]), as.numeric(r$auc), tolerance = 1e-9, info = v)
        expect_equal(as.numeric(pa$auc[i]), as.numeric(r$auc), tolerance = 1e-9, info = v)
        expect_equal(as.numeric(ea$auc_lower[i]), ci[1], tolerance = 1e-9, info = v)
        expect_equal(as.numeric(pa$ci_lower[i]), ci[1], tolerance = 1e-9, info = v)
    }
})

test_that("the DeLong comparison agrees between the two analyses and with pROC", {
    d <- cc_data()
    E <- cc_run(ClinicoPath::enhancedROC, data = d, outcome = "gold", predictors = c("m1", "m2"),
                positiveClass = "Positive", analysisType = "comparative", pairwiseComparisons = TRUE)
    P <- cc_run(ClinicoPath::psychopdaROC, data = d, classVar = "gold", dependentVars = c("m1", "m2"),
                positiveClass = "Positive", refVar = NULL, delongTest = TRUE)
    er <- as.data.frame(E$results$rocComparisons)
    pd <- as.data.frame(P$delongComparisonTable)
    r1 <- pROC::roc(d$gold, d$m1, levels = c("Negative", "Positive"), direction = "<", quiet = TRUE)
    r2 <- pROC::roc(d$gold, d$m2, levels = c("Negative", "Positive"), direction = "<", quiet = TRUE)
    ref <- pROC::roc.test(r1, r2, method = "delong")
    expect_equal(as.numeric(er$test_statistic[1]), unname(ref$statistic), tolerance = 1e-7)
    expect_equal(as.numeric(pd$z[1]),              unname(ref$statistic), tolerance = 1e-7)
    expect_equal(as.numeric(er$p_value[1]),        ref$p.value, tolerance = 1e-9)
    expect_equal(as.numeric(pd$p[1]),              ref$p.value, tolerance = 1e-9)
})

test_that("partial AUC agrees between the two analyses and with pROC", {
    d <- cc_data()
    E <- cc_run(ClinicoPath::enhancedROC, data = d, outcome = "gold", predictors = c("m1", "m2"),
                positiveClass = "Positive", partialAuc = TRUE, partialRange = "0.8,1.0")
    P <- cc_run(ClinicoPath::psychopdaROC, data = d, classVar = "gold", dependentVars = c("m1", "m2"),
                positiveClass = "Positive", refVar = NULL, partialAUC = TRUE,
                partialAUCfrom = 0.8, partialAUCto = 1.0)
    ep <- as.data.frame(E$results$partialAucAnalysis)
    pp <- as.data.frame(P$partialAUCTable)
    for (i in 1:2) {
        v <- c("m1", "m2")[i]
        r <- pROC::roc(d$gold, d[[v]], levels = c("Negative", "Positive"), direction = "<", quiet = TRUE)
        ref <- as.numeric(pROC::auc(r, partial.auc = c(1, 0.8), partial.auc.focus = "specificity"))
        expect_equal(as.numeric(ep$partial_auc[i]), ref, tolerance = 1e-9, info = v)
        expect_equal(as.numeric(pp$pAUC[i]),        ref, tolerance = 1e-9, info = v)
    }
})

test_that("the two cut-point conventions describe the same decision rule", {
    # pROC (enhancedROC) reports the midpoint between adjacent observed values; cutpointr
    # (psychopdaROC) reports the observed value itself. Different numbers, same rule - but if
    # they ever stop partitioning the patients identically, one of them has a real bug.
    skip_if_not_installed("cutpointr")
    d <- cc_data()
    r <- pROC::roc(d$gold, d$m1, levels = c("Negative", "Positive"), direction = "<", quiet = TRUE)
    co <- pROC::coords(r, "all", ret = c("threshold", "sensitivity", "specificity"))
    J <- co$sensitivity + co$specificity - 1
    fin <- is.finite(J)
    proc_cut <- co$threshold[fin][which.max(J[fin])]
    cp <- cutpointr::cutpointr(d, m1, gold, pos_class = "Positive", neg_class = "Negative",
                               direction = ">=", method = cutpointr::maximize_metric,
                               metric = cutpointr::youden, silent = TRUE)
    expect_false(isTRUE(all.equal(proc_cut, cp$optimal_cutpoint)))     # they differ numerically
    expect_identical(d$m1 >= proc_cut, d$m1 >= cp$optimal_cutpoint)    # and classify identically
})

# ── enhancedROC: prevalence default now matches its sibling ──────────────────

test_that("enhancedROC defaults to the observed prevalence, as psychopdaROC does", {
    # It used to ship useObservedPrevalence = false with prevalence = 0.1, so out of the box it
    # reported PPV for a hypothetical 10%-prevalence population while psychopdaROC reported the
    # observed-sample PPV - 0.293 against 0.738 on the same marker and cut-point.
    a <- yaml::read_yaml(test_path("..", "..", "jamovi", "enhancedROC.a.yaml"))
    o <- Filter(function(x) identical(x$name, "useObservedPrevalence"), a$options)[[1]]
    expect_true(isTRUE(o$default))
})

# ── enhancedROC: a below-chance marker is described as reversed, not weak ────

test_that("a below-chance AUC is reported as a direction problem, not 'limited performance'", {
    set.seed(55)
    g <- rbinom(300, 1, 0.45)
    d <- data.frame(y = factor(ifelse(g == 1, "P", "N"), levels = c("N", "P")), m = g * 1.4 + rnorm(300))
    E <- cc_run(ClinicoPath::enhancedROC, data = d, outcome = "y", predictors = "m",
                positiveClass = "P", direction = "lower")
    auc <- as.numeric(as.data.frame(E$results$aucSummary)$auc[1])
    expect_lt(auc, 0.5)
    txt <- cc_strip(E$results$notices$content)
    expect_match(txt, "Marker Reads Backwards")
    expect_match(txt, "opposite direction")
    expect_match(txt, sprintf("%.3f", 1 - auc))     # states what flipping would give
    expect_false(grepl("Limited Diagnostic Performance", txt, fixed = TRUE))
})

test_that("a merely weak marker still gets the limited-performance wording", {
    set.seed(3)
    g <- rbinom(300, 1, 0.5)
    d <- data.frame(y = factor(ifelse(g == 1, "P", "N"), levels = c("N", "P")), m = g * 0.4 + rnorm(300))
    E <- cc_run(ClinicoPath::enhancedROC, data = d, outcome = "y", predictors = "m", positiveClass = "P")
    auc <- as.numeric(as.data.frame(E$results$aucSummary)$auc[1])
    expect_true(auc > 0.5 && auc < 0.7)
    txt <- cc_strip(E$results$notices$content)
    expect_match(txt, "Limited Diagnostic Performance")
    expect_false(grepl("Marker Reads Backwards", txt, fixed = TRUE))
})
