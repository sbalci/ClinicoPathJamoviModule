# Regression tests from the `nogoldstandard` release review.
#
# Each case corresponds to a defect confirmed in the shipped code. The statistical ones are
# checked against poLCA or against the known truth of simulated data, not against the
# module's own arithmetic.

sim_tests <- function(prev, sens, spec, n = 1500, seed = 11) {
    set.seed(seed)
    truth <- rbinom(n, 1, prev)
    mk <- function(se, sp) ifelse(truth == 1, rbinom(n, 1, se), rbinom(n, 1, 1 - sp))
    d <- as.data.frame(lapply(seq_along(sens), function(i) {
        factor(ifelse(mk(sens[i], spec[i]) == 1, "pos", "neg"), levels = c("neg", "pos"))
    }))
    names(d) <- paste0("t", seq_along(sens))
    d
}

run_ngs <- function(dat, ...) {
    args <- list(data = dat,
                 test1 = "t1", test1Positive = "pos",
                 test2 = "t2", test2Positive = "pos",
                 test3 = NULL, test3Positive = NULL,
                 test4 = NULL, test4Positive = NULL,
                 test5 = NULL, test5Positive = NULL)
    if ("t3" %in% names(dat)) { args$test3 <- "t3"; args$test3Positive <- "pos" }
    do.call(ClinicoPath::nogoldstandard, utils::modifyList(args, list(...)))
}

notices_of <- function(res)
    gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(res$notices$content, collapse = " ")))


test_that("latent_class recovers the true sensitivity and specificity, not their transpose", {
    skip_if_not_installed("poLCA")
    # .runLCA identified the diseased class with probs[[i]][class, outcome] but extracted
    # with probs[[i]][2, disease_class] -- the transpose. [2,2] and [1,1] coincide, so the
    # swap only surfaced when poLCA labelled the diseased group as class 1 (about half of
    # runs), which is why it survived. seed 11 is one such dataset.
    TRUE_SENS <- c(.90, .80, .70)
    TRUE_SPEC <- c(.95, .85, .75)
    d <- sim_tests(0.30, TRUE_SENS, TRUE_SPEC, n = 4000, seed = 11)

    tm <- run_ngs(d, method = "latent_class")$test_metrics$asDF

    # recovers the truth within sampling error
    expect_equal(tm$sensitivity, TRUE_SENS, tolerance = 0.05)
    expect_equal(tm$specificity, TRUE_SPEC, tolerance = 0.05)
    # and is NOT the swapped pair, which is what it used to print
    expect_false(isTRUE(all.equal(tm$sensitivity, TRUE_SPEC, tolerance = 0.02)))

    # cross-check against poLCA on the identical data
    lc <- as.data.frame(lapply(d, function(x) factor(as.integer(x == "pos"),
                                                     levels = c(0, 1), labels = c("no", "yes"))))
    set.seed(1)
    m <- poLCA::poLCA(cbind(t1, t2, t3) ~ 1, data = lc, nclass = 2,
                      maxiter = 1000, nrep = 20, graphs = FALSE, verbose = FALSE)
    dis <- which.max(rowMeans(sapply(m$probs, function(x) x[, 2])))
    expect_equal(tm$sensitivity, unname(sapply(m$probs, function(x) x[dis, 2])), tolerance = 0.02)
    expect_equal(tm$specificity, unname(sapply(m$probs, function(x) x[3 - dis, 1])), tolerance = 0.02)
})


test_that("all_positive does not report a sensitivity that is 1 by construction", {
    # The reference is TRUE only when every test is positive, so a diseased case can never
    # be test-negative: FN is identically 0, giving sensitivity == 1 and NPV == 1 for every
    # test on every dataset. It used to print "100% (95% CI 100-100%)".
    d <- sim_tests(0.30, c(.60, .50, .90), c(.70, .80, .40))
    res <- run_ngs(d, method = "all_positive")
    tm <- res$test_metrics$asDF

    expect_true(all(is.na(tm$sensitivity)))
    expect_true(all(is.na(tm$sens_ci_lower)), label = "no [1,1] interval")
    expect_true(all(is.na(tm$npv)))
    # specificity still varies between tests and is retained
    expect_false(any(is.na(tm$specificity)))
    expect_gt(diff(range(tm$specificity)), 0.05)

    expect_match(notices_of(res), "cannot estimate accuracy")
    expect_match(notices_of(res), "fixed at 100% by construction")
})


test_that("any_positive does not report a specificity that is 1 by construction", {
    d <- sim_tests(0.30, c(.60, .50, .90), c(.70, .80, .40))
    tm <- run_ngs(d, method = "any_positive")$test_metrics$asDF

    expect_true(all(is.na(tm$specificity)))
    expect_true(all(is.na(tm$spec_ci_lower)))
    expect_true(all(is.na(tm$ppv)))
    expect_false(any(is.na(tm$sensitivity)))
})


test_that("composite with two tests is recognised as any_positive", {
    # rowMeans >= 0.5 with k = 2 passes on a 1-of-2 tie, making the composite identical to
    # "any test positive": FP identically 0, specificity and PPV 1.000 with a [1,1] interval.
    d2 <- sim_tests(0.30, c(.80, .70), c(.90, .85))
    res <- run_ngs(d2, method = "composite")
    tm <- res$test_metrics$asDF

    expect_true(all(is.na(tm$specificity)))
    expect_true(all(is.na(tm$ppv)))
    expect_match(notices_of(res), "no majority")

    # with three tests a majority is meaningful and specificity is reported again
    d3 <- sim_tests(0.30, c(.80, .70, .60), c(.90, .85, .95))
    tm3 <- run_ngs(d3, method = "composite")$test_metrics$asDF
    expect_false(any(is.na(tm3$specificity)))
})


test_that("bayesian warns that two tests cannot identify the model", {
    # 2k+1 = 5 parameters against 2^k - 1 = 3 degrees of freedom.
    d2 <- sim_tests(0.30, c(.80, .70), c(.90, .85))
    expect_match(notices_of(run_ngs(d2, method = "bayesian")),
                 "cannot identify this model")
})


test_that("the Bayesian priors are disclosed", {
    # Beta(2,1) on both sensitivity and specificity has mean 2/3 and increases toward 1, so
    # it pulls estimates up. Nothing in the output mentioned any prior.
    d3 <- sim_tests(0.30, c(.80, .70, .60), c(.90, .85, .95))
    n <- notices_of(run_ngs(d3, method = "bayesian"))
    expect_match(n, "Beta")
    expect_match(n, "pulls both estimates upward")
    expect_match(n, "not draws from a posterior")
})


test_that("Wald intervals use the diseased/non-diseased denominators, not the total n", {
    # se was sqrt(p(1-p)/n_total) for BOTH metrics; the denominators are n*prevalence and
    # n*(1-prevalence), so every interval was too narrow -- by ~1.8x for sensitivity at 30%
    # prevalence.
    d3 <- sim_tests(0.30, c(.80, .70, .60), c(.90, .85, .95))
    res <- run_ngs(d3, method = "composite")
    tm <- res$test_metrics$asDF
    prev <- res$prevalence$asDF$estimate[1]
    n <- nrow(d3)

    z <- qnorm(0.975)
    expected_half <- z * sqrt(tm$sensitivity[1] * (1 - tm$sensitivity[1]) / (n * prev))
    got_half <- (tm$sens_ci_upper[1] - tm$sens_ci_lower[1]) / 2
    expect_equal(got_half, expected_half, tolerance = 1e-6)

    # and it must be WIDER than the old total-n version
    old_half <- z * sqrt(tm$sensitivity[1] * (1 - tm$sensitivity[1]) / n)
    expect_gt(got_half, old_half)
})


test_that("an invalid positive level produces a readable message", {
    # jmvcore::reject(formats, code = NULL, ...) -- the 2nd POSITIONAL argument is `code`.
    # Passing substitution values positionally swallowed the first and shifted the rest:
    #   "Level 'test1_result' not found in variable 'negative, positive'. Available levels: {}"
    d <- sim_tests(0.30, c(.80, .70, .60), c(.90, .85, .95))
    err <- tryCatch(run_ngs(d, test1Positive = "NOSUCH"), error = conditionMessage)

    expect_match(err, "Level 'NOSUCH' not found in variable 't1'")
    expect_match(err, "Available levels: neg, pos")
    expect_false(grepl("{}", err, fixed = TRUE))
})


test_that("PPV/NPV do not abort the analysis when a metric is undefined", {
    # `if (ppv_denominator > 0)` with an NA sensitivity threw "missing value where
    # TRUE/FALSE needed", killing the whole analysis instead of blanking one cell.
    prv <- ClinicoPath:::nogoldstandardClass$new(
        options = ClinicoPath:::nogoldstandardOptions$new(
            test1 = "t1", test1Positive = "pos", test2 = "t2", test2Positive = "pos",
            test3 = NULL, test3Positive = NULL, test4 = NULL, test4Positive = NULL,
            test5 = NULL, test5Positive = NULL),
        data = sim_tests(0.3, c(.8, .7), c(.9, .85)))$.__enclos_env__$private

    expect_no_error(out <- prv$.calculatePPVNPV(NA_real_, 0.9, 0.3))
    expect_true(is.na(out$ppv) || is.numeric(out$ppv))
    expect_no_error(prv$.calculatePPVNPV(0.8, NA_real_, 0.3))
    expect_no_error(prv$.calculatePPVNPV(NA_real_, NA_real_, NA_real_))

    # sanity: the formula itself is Bayes
    ok <- prv$.calculatePPVNPV(0.80, 0.90, 0.30)
    expect_equal(ok$ppv, 0.8 * 0.3 / (0.8 * 0.3 + 0.1 * 0.7), tolerance = 1e-10)
    expect_equal(ok$npv, 0.9 * 0.7 / (0.2 * 0.3 + 0.9 * 0.7), tolerance = 1e-10)
})


test_that("the method guide does not claim LCA handles conditional dependence", {
    # poLCA(nclass = 2, ~ 1) assumes conditional independence. The always-visible guide
    # advertised the opposite, and claimed "No identifiability issues".
    d3 <- sim_tests(0.30, c(.80, .70, .60), c(.90, .85, .95))
    txt <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ",
        paste(run_ngs(d3, method = "latent_class")$method_guide$content, collapse = " ")))

    expect_false(grepl("Handles conditional dependence", txt, fixed = TRUE))
    expect_false(grepl("No identifiability issues", txt, fixed = TRUE))
    expect_match(txt, "conditionally independent")
    expect_match(txt, "does NOT model conditional dependence")
})


test_that("the clinical summary does not announce a 100% sensitivity range", {
    d <- sim_tests(0.30, c(.60, .50, .90), c(.70, .80, .40))
    txt <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ",
        paste(run_ngs(d, method = "all_positive")$clinical_summary$content, collapse = " ")))

    expect_false(grepl("Range from 100.0% to 100.0%", txt, fixed = TRUE))
    expect_match(txt, "not estimable")
    # the all-agree fraction is not disease prevalence
    expect_match(txt, "not an estimate of disease prevalence")
})
