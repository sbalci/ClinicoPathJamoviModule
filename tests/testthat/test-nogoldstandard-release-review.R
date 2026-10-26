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


test_that("composite uses a strict majority and discloses two-test degeneracy", {
    # A strict majority uses > 0.5. With two tests, a 1-of-2 tie is rule negative and the
    # composite is identical to all-positive, so sensitivity and NPV are fixed at one.
    d2 <- sim_tests(0.30, c(.80, .70), c(.90, .85))
    res <- run_ngs(d2, method = "composite")
    tm <- res$test_metrics$asDF

    expect_true(all(is.na(tm$sensitivity)))
    expect_true(all(is.na(tm$npv)))
    expect_false(any(is.na(tm$specificity)))
    expect_match(notices_of(res), "all-positive rule")
    expect_match(notices_of(res), "tie is rule negative")

    # with three tests a majority is meaningful and specificity is reported again
    d3 <- sim_tests(0.30, c(.80, .70, .60), c(.90, .85, .95))
    tm3 <- run_ngs(d3, method = "composite")$test_metrics$asDF
    expect_false(any(is.na(tm3$specificity)))
})


test_that("penalized EM rejects an unidentified two-test model", {
    # 2k+1 = 5 parameters against 2^k - 1 = 3 degrees of freedom.
    d2 <- sim_tests(0.30, c(.80, .70), c(.90, .85))
    expect_error(
        run_ngs(d2, method = "bayesian"),
        "Penalized EM requires at least 3 tests"
    )
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


test_that("intervals match the uncertainty source", {
    d3 <- sim_tests(0.30, c(.80, .70, .60), c(.90, .85, .95))

    # Latent-model plug-in binomial intervals ignore class-estimation uncertainty and are
    # therefore omitted unless bootstrap refitting is requested.
    latent <- run_ngs(d3, method = "bayesian")
    expect_true(all(is.na(latent$prevalence$asDF$ci_lower)))
    expect_true(all(is.na(latent$test_metrics$asDF$sens_ci_lower)))
    expect_true(all(is.na(latent$test_metrics$asDF$spec_ci_upper)))

    # Rule-based quantities are observed proportions and use Wilson score intervals.
    res <- run_ngs(d3, method = "composite")
    tm <- res$test_metrics$asDF
    prev <- res$prevalence$asDF$estimate[1]
    n <- nrow(d3)
    z <- qnorm(0.975)
    total <- round(n * prev)
    success <- round(tm$sensitivity[1] * total)
    p <- success / total
    denominator <- 1 + z^2 / total
    centre <- (p + z^2 / (2 * total)) / denominator
    half_width <- z * sqrt(p * (1 - p) / total + z^2 / (4 * total^2)) / denominator
    expect_equal(tm$sens_ci_lower[1], max(0, centre - half_width), tolerance = 1e-10)
    expect_equal(tm$sens_ci_upper[1], min(1, centre + half_width), tolerance = 1e-10)
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
        paste(run_ngs(d3, method = "latent_class", showMethodGuide = TRUE)$method_guide$content,
              collapse = " ")))

    expect_false(grepl("Handles conditional dependence", txt, fixed = TRUE))
    expect_false(grepl("No identifiability issues", txt, fixed = TRUE))
    expect_match(txt, "conditionally independent")
    expect_match(txt, "does NOT model conditional dependence")
})


test_that("the clinical summary does not announce a 100% sensitivity range", {
    d <- sim_tests(0.30, c(.60, .50, .90), c(.70, .80, .40))
    txt <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ",
        paste(run_ngs(d, method = "all_positive", showSummary = TRUE)$clinical_summary$content,
              collapse = " ")))

    expect_false(grepl("Range from 100.0% to 100.0%", txt, fixed = TRUE))
    expect_match(txt, "not reported")
    # the all-agree fraction is not disease prevalence
    expect_match(txt, "not an estimate of disease prevalence")
})


test_that("pairwise Kappa uses selected binary meanings, not literal labels", {
    state <- rep(c(FALSE, TRUE, TRUE, FALSE), 30)
    d <- data.frame(
        first = factor(ifelse(state, "pos", "neg"), levels = c("neg", "pos")),
        second = factor(ifelse(state, "positive", "negative"),
                        levels = c("negative", "positive"))
    )

    res <- ClinicoPath::nogoldstandard(
        data = d,
        test1 = "first", test1Positive = "pos",
        test2 = "second", test2Positive = "positive",
        test3 = NULL, test3Positive = NULL,
        test4 = NULL, test4Positive = NULL,
        test5 = NULL, test5Positive = NULL,
        method = "composite"
    )
    agreement <- res$agreement_stats$asDF

    expect_equal(agreement$agreement, 1)
    expect_equal(agreement$kappa, 1)
    expect_equal(agreement$p_value, 0)
})


test_that("pairwise Kappa p-value uses the null variance for H0 kappa equals zero", {
    skip_if_not_installed("irr")
    counts <- matrix(c(44, 3, 1, 1), nrow = 2, byrow = TRUE)
    first <- rep(c(FALSE, FALSE, TRUE, TRUE), times = as.vector(counts))
    second <- rep(c(FALSE, TRUE, FALSE, TRUE), times = as.vector(counts))
    d <- data.frame(
        t1 = factor(ifelse(first, "pos", "neg"), levels = c("neg", "pos")),
        t2 = factor(ifelse(second, "pos", "neg"), levels = c("neg", "pos"))
    )

    agreement <- run_ngs(d, method = "composite")$agreement_stats$asDF
    ratings <- data.frame(
        first = factor(first, levels = c(FALSE, TRUE)),
        second = factor(second, levels = c(FALSE, TRUE))
    )
    expected <- irr::kappa2(ratings, weight = "unweighted")

    expect_equal(agreement$kappa, expected$value, tolerance = 1e-12)
    expect_equal(agreement$p_value, expected$p.value, tolerance = 1e-12)
    expect_lt(agreement$p_value, 0.05)
})


test_that("bivariate residuals include expected counts for unobserved response patterns", {
    skip_if_not_installed("poLCA")
    d <- sim_tests(
        0.25,
        c(.92, .83, .78, .74),
        c(.96, .91, .86, .82),
        n = 80,
        seed = 616
    )
    binary <- as.data.frame(lapply(d, function(x) as.numeric(x == "pos")))
    options <- ClinicoPath:::nogoldstandardOptions$new(
        test1 = "t1", test1Positive = "pos",
        test2 = "t2", test2Positive = "pos",
        test3 = "t3", test3Positive = "pos",
        test4 = "t4", test4Positive = "pos",
        test5 = NULL, test5Positive = NULL,
        method = "latent_class",
        seed = 17
    )
    private <- ClinicoPath:::nogoldstandardClass$new(
        options = options,
        data = d
    )$.__enclos_env__$private
    fit <- private$.runLCA(binary, names(binary), n_starts = 5L)
    model <- fit$model

    expect_lt(nrow(model$predcell), 2^ncol(binary))
    pair_names <- utils::combn(names(binary), 2, simplify = FALSE)
    differences_from_truncated_method <- numeric(length(pair_names))

    for (i in seq_along(pair_names)) {
        a <- pair_names[[i]][1]
        b <- pair_names[[i]][2]
        actual <- private$.bivariateResidual(model, a, b)

        probs_a <- model$probs[[a]]
        probs_b <- model$probs[[b]]
        observed <- table(
            factor(as.character(model$y[[a]]), levels = colnames(probs_a)),
            factor(as.character(model$y[[b]]), levels = colnames(probs_b))
        )
        expected_probability <- matrix(0, 2, 2)
        for (class in seq_along(model$P)) {
            expected_probability <- expected_probability + model$P[class] *
                outer(probs_a[class, ], probs_b[class, ])
        }
        expected <- sum(observed) * expected_probability
        independent <- sum((observed - expected)^2 / pmax(expected, 1e-9))
        expect_equal(actual, independent, tolerance = 1e-12)

        pc <- model$predcell
        truncated <- stats::aggregate(
            cbind(observed, expected) ~ pc[[a]] + pc[[b]],
            data = pc,
            FUN = sum
        )
        old_value <- sum((truncated$observed - truncated$expected)^2 /
            pmax(truncated$expected, 1e-9))
        differences_from_truncated_method[i] <- abs(actual - old_value)
    }

    expect_true(any(differences_from_truncated_method > 1e-6))
})


test_that("latent-class output discloses its class-label convention", {
    skip_if_not_installed("poLCA")
    # Below-chance tests make the label-switching risk concrete: an automatic orientation
    # can invert the clinical meaning even when the numerical mixture fit is valid.
    d <- sim_tests(0.30, rep(0.20, 3), rep(0.20, 3), n = 1200, seed = 27)
    res <- run_ngs(d, method = "latent_class", showSummary = TRUE)
    summary_text <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ",
        paste(res$clinical_summary$content, collapse = " ")))

    expect_match(notices_of(res), "Latent classes are unlabeled")
    expect_match(notices_of(res), "clinical interpretation is inverted")
    expect_match(summary_text, "High-positive latent-class proportion")
    expect_false(grepl("Disease prevalence", summary_text, fixed = TRUE))

    penalized <- run_ngs(d, method = "bayesian")
    penalized_metrics <- penalized$test_metrics$asDF
    expect_gte(mean(penalized_metrics$sensitivity),
               mean(1 - penalized_metrics$specificity))
    expect_match(notices_of(penalized), "Latent classes are unlabeled")
})


test_that("bootstrap estimator warnings are rendered once, not once per resample", {
    d <- sim_tests(0.30, c(.80, .70, .60), c(.90, .85, .95), n = 180, seed = 14)
    count_fixed <- function(text, pattern) {
        hits <- gregexpr(pattern, text, fixed = TRUE)[[1]]
        if (identical(hits, -1L)) 0L else length(hits)
    }

    composite_text <- notices_of(run_ngs(
        d, method = "composite", bootstrap = TRUE, nboot = 100, seed = 31
    ))
    expect_equal(count_fixed(composite_text, "Composite reference has incorporation bias"), 1L)

    penalized_text <- notices_of(run_ngs(
        d, method = "bayesian", bootstrap = TRUE, nboot = 100, seed = 31
    ))
    expect_equal(count_fixed(penalized_text, "Fixed priors used by penalized EM"), 1L)
    expect_equal(count_fixed(penalized_text, "Latent classes are unlabeled"), 1L)
})


test_that("boundary fits produce an estimability statement rather than NA percent", {
    d <- data.frame(
        t1 = factor(rep("neg", 90), levels = c("neg", "pos")),
        t2 = factor(rep("neg", 90), levels = c("neg", "pos")),
        t3 = factor(rep("neg", 90), levels = c("neg", "pos"))
    )
    text <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ",
        paste(run_ngs(d, method = "bayesian", showSummary = TRUE)$clinical_summary$content,
              collapse = " ")))

    expect_match(text, "not estimable")
    expect_false(grepl("NA%", text, fixed = TRUE))
})


test_that("removing configured tests restores instructions and hides stale summary", {
    d <- sim_tests(0.30, c(.80, .70, .60), c(.90, .85, .95), n = 120, seed = 7)
    options <- ClinicoPath:::nogoldstandardOptions$new(
        test1 = "t1", test1Positive = "pos",
        test2 = "t2", test2Positive = "pos",
        test3 = "t3", test3Positive = "pos",
        test4 = NULL, test4Positive = NULL,
        test5 = NULL, test5Positive = NULL,
        method = "composite",
        showSummary = TRUE
    )
    analysis <- ClinicoPath:::nogoldstandardClass$new(options = options, data = d)
    analysis$run()
    expect_false(analysis$results$instructions$visible)
    expect_true(analysis$results$clinical_summary$visible)

    # A complete selection must leave real content behind, not just visible headings.
    expect_gt(analysis$results$agreement_stats$rowCount, 0)
    expect_gt(analysis$results$crosstab$rowCount, 0)
    expect_true(nzchar(analysis$results$clinical_summary$content))

    for (name in c("test2", "test2Positive", "test3", "test3Positive")) {
        option <- options$option(name)
        option$value <- NULL
    }
    analysis$run()

    expect_true(analysis$results$instructions$visible)
    # clinical_summary's visibility is owned by the .r.yaml `visible: (showSummary)`
    # binding, which is still TRUE here -- emptying the content is what removes the stale
    # summary, so that is what is asserted.
    expect_identical(analysis$results$clinical_summary$content, "")
    # .run() empties .noticeList and renders on exit, so the previous run's "Important
    # Information" text cannot survive an early return. This guards the R results object;
    # in the jamovi client the item's own clearWith would also clear it.
    expect_identical(analysis$results$notices$content, "")
})


test_that("scenario examples and method labels describe their actual scope", {
    a_yaml <- paste(readLines(testthat::test_path("..", "..", "jamovi", "nogoldstandard.a.yaml"),
                              warn = FALSE), collapse = "\n")
    u_yaml <- paste(readLines(testthat::test_path("..", "..", "jamovi", "nogoldstandard.u.yaml"),
                              warn = FALSE), collapse = "\n")
    r_yaml <- paste(readLines(testthat::test_path("..", "..", "jamovi", "nogoldstandard.r.yaml"),
                              warn = FALSE), collapse = "\n")

    expect_match(a_yaml, "Illustrative scenario examples only")
    expect_match(a_yaml, "not clinical guides")
    expect_match(a_yaml, "Penalized EM \\(MAP-like; fixed priors; 3\\+ tests\\)")
    expect_match(a_yaml, "Composite reference \\(strict majority\\)")
    # The ComboBox takes its titles from the .a.yaml; a copy in the .u.yaml could only drift.
    expect_false(grepl("Composite Reference", u_yaml, fixed = TRUE))
    expect_false(grepl("name: alpha[\\s\\S]{0,180}enable: \\(bootstrap\\)", u_yaml))
    expect_false(grepl("name: verbose[\\s\\S]{0,180}enable: \\(bootstrap\\)", u_yaml))
    expect_match(r_yaml, "title: Estimated Class or Rule Proportion")
})


test_that("explanatory outputs are optional and use non-clinical labels", {
    d <- sim_tests(0.30, c(.80, .70, .60), c(.90, .85, .95), n = 120, seed = 9)
    default <- run_ngs(d, method = "composite")
    expect_false(default$clinical_summary$visible)
    expect_false(default$method_guide$visible)

    shown <- run_ngs(
        d,
        method = "composite",
        showSummary = TRUE,
        showMethodGuide = TRUE
    )
    summary_text <- paste(shown$clinical_summary$content, collapse = " ")
    guide_text <- paste(shown$method_guide$content, collapse = " ")
    expect_match(summary_text, "Plain-Language Summary")
    expect_match(summary_text, "Positive agreement with the rule")
    expect_match(guide_text, "illustrative examples only")
    expect_match(guide_text, "not clinical guides")
})


test_that("method guide populates when enabled after analysis initialization", {
    d <- sim_tests(0.30, c(.80, .70, .60), c(.90, .85, .95), n = 120, seed = 9)
    options <- ClinicoPath:::nogoldstandardOptions$new(
        test1 = "t1", test1Positive = "pos",
        test2 = "t2", test2Positive = "pos",
        test3 = "t3", test3Positive = "pos",
        test4 = NULL, test4Positive = NULL,
        test5 = NULL, test5Positive = NULL,
        method = "composite",
        showMethodGuide = FALSE
    )
    analysis <- ClinicoPath:::nogoldstandardClass$new(options = options, data = d)
    analysis$run()
    expect_identical(analysis$results$method_guide$content, "")

    guide_option <- options$option("showMethodGuide")
    guide_option$value <- TRUE
    analysis$run()

    expect_match(analysis$results$method_guide$content, "Method Selection Guide")
    expect_match(analysis$results$method_guide$content, "not clinical guides")
})


test_that("latent-class RNG is reproducible without changing the caller's stream", {
    skip_if_not_installed("poLCA")
    d <- sim_tests(0.30, c(.80, .70, .60), c(.90, .85, .95), n = 250, seed = 19)
    set.seed(814)
    state_before <- .Random.seed
    first <- run_ngs(d, method = "latent_class", seed = 37)$test_metrics$asDF
    expect_identical(.Random.seed, state_before)
    second <- run_ngs(d, method = "latent_class", seed = 37)$test_metrics$asDF
    expect_equal(first, second)
})


test_that("poLCA automatic restarts are not treated as nonconvergence", {
    d <- sim_tests(0.30, c(.80, .70, .60), c(.90, .85, .95), n = 100, seed = 29)
    private <- ClinicoPath:::nogoldstandardClass$new(
        options = ClinicoPath:::nogoldstandardOptions$new(
            test1 = "t1", test1Positive = "pos",
            test2 = "t2", test2Positive = "pos",
            test3 = "t3", test3Positive = "pos",
            test4 = NULL, test4Positive = NULL,
            test5 = NULL, test5Positive = NULL
        ),
        data = d
    )$.__enclos_env__$private

    model <- list(numiter = 12L, llik = -123.4, eflag = TRUE)
    expect_true(private$.lcaConverged(model))
    model$numiter <- 1000L
    expect_false(private$.lcaConverged(model))
})


# Regressions from /check-function --profile release, 2026-10-04.

test_that("one variable in two test slots is rejected, not fitted as an all-NA column", {
    d <- sim_tests(0.4, c(.9, .8, .75), c(.9, .85, .9), n = 200)
    for (m in c("composite", "latent_class")) {
        # Before: composite stopped with "missing value where TRUE/FALSE needed";
        # latent_class returned duplicate row keys.
        expect_error(run_ngs(d, test2 = "t1", method = m),
                     "'t1' is selected more than once", fixed = TRUE)
    }
})

test_that("a fractional bootstrap count is refused by the option, not by sprintf", {
    d <- sim_tests(0.4, c(.9, .8, .75), c(.9, .85, .9), n = 200)
    # Before: nboot = 150.5 passed the Number option and aborted on sprintf("%d").
    err <- tryCatch(run_ngs(d, method = "composite", bootstrap = TRUE, nboot = 150.5),
                    error = function(e) conditionMessage(e))
    expect_false(grepl("invalid format", err, fixed = TRUE))
    expect_match(err, "nboot")
})


# Regressions from /check-function-full, 2026-10-04.

dep_tests <- function(n, prev, sens, spec, dep, seed = 1) {
    # Test B copies test A in a `dep` share of cases: conditional dependence by construction.
    set.seed(seed)
    z <- rbinom(n, 1, prev)
    out <- lapply(seq_along(sens), function(i)
        ifelse(z == 1, rbinom(n, 1, sens[i]), rbinom(n, 1, 1 - spec[i])))
    flip <- runif(n) < dep
    out[[2]][flip] <- out[[1]][flip]
    d <- as.data.frame(lapply(out, function(x) factor(x, 0:1, c("neg", "pos"))))
    names(d) <- paste0("t", seq_along(sens))
    d
}

test_that("identical tests are not reported as perfect without a boundary warning", {
    skip_if_not_installed("poLCA")
    d <- sim_tests(0.35, c(.9, .8, .75), c(.92, .88, .93), n = 300)
    d$t2 <- d$t1
    d$t3 <- d$t1
    # Before: sensitivity = specificity = 100% with a 100-100% interval and no warning.
    r <- run_ngs(d, bootstrap = TRUE, nboot = 100, showSummary = TRUE)
    expect_match(notices_of(r), "Estimates at the parameter boundary", fixed = TRUE)
    expect_match(notices_of(r), "Some bootstrap intervals left blank", fixed = TRUE)
    tm <- r$test_metrics$asDF
    expect_true(all(is.na(c(tm$sens_ci_lower, tm$sens_ci_upper,
                            tm$spec_ci_lower, tm$spec_ci_upper))))
    expect_match(r$clinical_summary$content, "at least one estimate is at 0% or 100%", fixed = TRUE)

    # an interior fit raises neither notice and keeps its intervals
    ok <- run_ngs(sim_tests(0.35, c(.9, .8, .75), c(.92, .88, .93), n = 300),
                  bootstrap = TRUE, nboot = 100)
    expect_false(grepl("parameter boundary", notices_of(ok), fixed = TRUE))
    expect_false(grepl("intervals left blank", notices_of(ok), fixed = TRUE))
    expect_true(all(is.finite(ok$test_metrics$asDF$sens_ci_lower)))
})

test_that("three-test residuals are not reported as evidence of independence", {
    skip_if_not_installed("poLCA")
    d <- dep_tests(400, .35, c(.85, .8, .75), c(.9, .88, .9), dep = .6)
    cd <- run_ngs(d)$conditional_dependence$asDF
    expect_equal(nrow(cd), 3L)
    # Before: "Consistent with independence" on every row although t2 copies t1.
    expect_true(all(startsWith(cd$verdict, "Not assessable")))
})

test_that("four-test dependence warning does not imply the flagged pair is the dependent one", {
    skip_if_not_installed("poLCA")
    d <- dep_tests(400, .35, c(.85, .8, .75, .8), c(.9, .88, .9, .85), dep = .6)
    r <- run_ngs(d, test4 = "t4", test4Positive = "pos")
    cd <- r$conditional_dependence$asDF
    expect_false(any(startsWith(cd$verdict, "Not assessable")))
    expect_true(any(cd$bvr > stats::qchisq(.95, 1)))
    expect_match(notices_of(r), "not necessarily the dependent pair", fixed = TRUE)
})


# Regressions from /review-function, 2026-10-05.

test_that("positive-level selectors start empty instead of on the first level", {
    # Without allowNone the client pre-selects levels[0], usually the NEGATIVE level, and
    # every test runs reversed. Unset levels must leave the analysis on its welcome screen.
    options <- yaml::read_yaml(test_path("..", "..", "jamovi", "nogoldstandard.a.yaml"))$options
    levels <- Filter(function(o) identical(o$type, "Level"), options)
    expect_length(levels, 5L)
    expect_true(all(vapply(levels, function(o) isTRUE(o$allowNone), logical(1))))

    d <- sim_tests(0.35, c(.9, .8, .75), c(.92, .88, .93), n = 120)
    # run_ngs() would drop the NULLs (modifyList), so call the wrapper directly
    r <- ClinicoPath::nogoldstandard(data = d, test1 = "t1", test1Positive = NULL,
        test2 = "t2", test2Positive = NULL, test3 = "t3", test3Positive = NULL,
        test4Positive = NULL, test5Positive = NULL)
    expect_true(r$instructions$visible)
    expect_equal(r$test_metrics$rowCount, 0L)
})

test_that("penalized-EM bootstrap replicates reuse the full-sample solution", {
    d <- sim_tests(0.35, c(.9, .8, .75), c(.92, .88, .93), n = 300)
    options <- ClinicoPath:::nogoldstandardOptions$new(
        test1 = "t1", test1Positive = "pos", test2 = "t2", test2Positive = "pos",
        test3 = "t3", test3Positive = "pos", test4 = NULL, test4Positive = NULL,
        test5 = NULL, test5Positive = NULL, method = "bayesian")
    private <- ClinicoPath:::nogoldstandardClass$new(options = options, data = d)$.__enclos_env__$private
    bd <- as.data.frame(lapply(d, function(x) as.numeric(x == "pos")))
    full <- private$.runBayesian(bd)
    resample <- bd[withr::with_seed(3, sample.int(nrow(bd), replace = TRUE)), ]
    # the 3-start warm search reaches the same optimum as the full 10-start search
    expect_equal(private$.runBayesian(resample, start_par = full$par)[c("prevalence", "sensitivities", "specificities")],
                 private$.runBayesian(resample)[c("prevalence", "sensitivities", "specificities")],
                 tolerance = 1e-6)

    r <- run_ngs(d, method = "bayesian", bootstrap = TRUE, nboot = 100)
    expect_match(r$prevalence$notes$boot_warm_start$note, "penalized-EM resample", fixed = TRUE)
    expect_true(all(is.finite(r$test_metrics$asDF$sens_ci_lower)))
})

test_that("every Turkish sprintf template for this analysis formats without error", {
    po <- test_path("..", "..", "jamovi", "i18n", "tr.po")
    skip_if_not(file.exists(po))
    # entries are separated by blank lines; join continuation lines of msgid / msgstr
    blocks <- strsplit(paste(readLines(po, encoding = "UTF-8"), collapse = "\n"), "\n\n")[[1]]
    field <- function(b, key) {
        m <- regmatches(b, regexpr(paste0("(?s)\n?", key, " \".*?\"(\n\"[^\n]*\")*"), b, perl = TRUE))
        if (!length(m)) return("")
        q <- regmatches(m, gregexpr('"(?:[^"\\\\]|\\\\.)*"', m))[[1]]
        gsub('\\\\"', '"', paste(substr(q, 2, nchar(q) - 1), collapse = ""))
    }
    conv <- "%(\\d+\\$)?[-+0#]*\\d*(\\.\\d+)?[sdifeEgGxX]"
    checked <- 0L
    for (b in blocks[grepl("#: R/nogoldstandard.b.R", blocks, fixed = TRUE)]) {
        id <- field(b, "msgid"); str <- field(b, "msgstr")
        cs <- regmatches(gsub("%%", "", id), gregexpr(conv, gsub("%%", "", id)))[[1]]
        if (!length(cs) || !nzchar(str)) next
        # one argument per msgid slot (numbered slots: one per distinct number), typed by conversion
        n <- if (all(grepl("\\$", cs))) max(as.integer(sub("%(\\d+)\\$.*", "\\1", cs))) else length(cs)
        types <- if (all(grepl("\\$", cs)))
            vapply(seq_len(n), function(k) sub(".*([a-zA-Z])$", "\\1", cs[grepl(paste0("^%", k, "\\$"), cs)][1]), "")
            else sub(".*([a-zA-Z])$", "\\1", cs)
        args <- lapply(types, function(t) switch(t, d = , i = 3L, s = "x", 2.5))
        expect_no_error(do.call(sprintf, c(list(str), args)), message = id)
        checked <- checked + 1L
    }
    expect_gte(checked, 8L)
})


# Regressions from /release-review-function, 2026-10-05 (second pass).

test_that("a test whose positive level is unset is named, not silently dropped", {
    d <- sim_tests(0.35, c(.9, .8, .75), c(.92, .88, .93), n = 150)
    partial <- function(method)
        ClinicoPath::nogoldstandard(data = d, test1 = "t1", test1Positive = "pos",
            test2 = "t2", test2Positive = "pos", test3 = "t3", test3Positive = NULL,
            test4Positive = NULL, test5Positive = NULL, method = method)
    # Before: latent class rejected with "add more tests" although three were selected, and
    # the rule methods ran on t1 and t2 alone without a word about t3.
    for (m in c("latent_class", "composite", "all_positive")) {
        r <- partial(m)
        expect_true(r$instructions$visible)
        expect_equal(r$test_metrics$rowCount, 0L)
        expect_match(notices_of(r), "Choose the positive level for Test 3 (t3)", fixed = TRUE)
    }
})

test_that("a column blank by construction does not raise the zero-width bootstrap notice", {
    d <- sim_tests(0.35, c(.9, .8, .75), c(.92, .88, .93), n = 200)
    for (m in c("all_positive", "any_positive")) {
        r <- run_ngs(d, method = m, bootstrap = TRUE, nboot = 100)
        # Before: every run said "Some bootstrap intervals left blank".
        expect_false(grepl("intervals left blank", notices_of(r), fixed = TRUE), info = m)
        tm <- r$test_metrics$asDF
        kept <- if (m == "all_positive") tm$spec_ci_lower else tm$sens_ci_lower
        expect_true(all(is.finite(kept)), info = m)
    }
})

test_that("the prior notice describes the pseudo-counts the penalty adds", {
    # The Beta(2,1) penalty on specificity adds a pseudo true NEGATIVE, not a positive.
    r <- run_ngs(sim_tests(0.35, c(.9, .8, .75), c(.92, .88, .93), n = 150),
                 method = "bayesian", showSummary = TRUE)
    expect_match(notices_of(r), "one pseudo true-positive and one pseudo true-negative", fixed = TRUE)
    # "(N=3)" next to the test list read as a sample size of three
    expect_false(grepl("(N=", r$clinical_summary$content, fixed = TRUE))
})


# Regressions from /security-audit-function, 2026-10-05.

test_that("the syntax-pane call quotes test variables and levels, so it parses and runs", {
    d <- sim_tests(0.35, c(.9, .8, .75), c(.92, .88, .93), n = 150)
    names(d) <- c("HPV DNA", "p16\"x\\y", "x); stop('injected'); (y")
    opts <- ClinicoPath:::nogoldstandardOptions$new(
        test1 = names(d)[1], test1Positive = "pos", test2 = names(d)[2], test2Positive = "pos",
        test3 = names(d)[3], test3Positive = "pos", test4 = NULL, test4Positive = NULL,
        test5 = NULL, test5Positive = NULL, method = "composite")
    src <- ClinicoPath:::nogoldstandardClass$new(options = opts, data = d)$asSource()
    # Before: `test1 = HPV DNA` -- unexpected symbol -- and the third name as live code.
    expr <- parse(text = src)[[1]]
    expect_identical(expr$test1, "HPV DNA")
    expect_identical(expr$test2, "p16\"x\\y")
    expect_identical(expr$test3, "x); stop('injected'); (y")
    # a NULL Level is still written, the wrapper needs it
    expect_true("test4Positive" %in% names(expr))
    expect_null(expr[["test4Positive"]])
    data <- d
    expect_s3_class(eval(expr), "nogoldstandardResults")
})

test_that("a column name with '<' before a letter is not swallowed by the cell filter", {
    d <- sim_tests(0.35, c(.9, .8, .75), c(.92, .88, .93), n = 150)
    names(d) <- c("<LOD flag", "a<b", "Ki67 <20")
    r <- ClinicoPath::nogoldstandard(data = d, test1 = names(d)[1], test1Positive = "pos",
        test2 = names(d)[2], test2Positive = "pos", test3 = names(d)[3], test3Positive = "pos",
        test4Positive = NULL, test5Positive = NULL, method = "composite")
    # The client keeps only em/i/sub/sup: "<LOD flag" rendered blank, "a<b" as "a".
    expect_equal(r$test_metrics$asDF$test, c("< LOD flag", "a< b", "Ki67 <20"))
    expect_equal(r$agreement_stats$asDF$test_pair[1], "< LOD flag vs a< b")
    expect_false(any(grepl("<[A-Za-z]", r$crosstab$asDF$test_combination)))
    # the values were still computed for every row (rowKeys kept the raw names)
    expect_true(all(is.finite(r$test_metrics$asDF$specificity)))
})


# Regressions from /fix-function --apply, 2026-10-05.

test_that("latent class names a constant test instead of failing inside the optimizer", {
    skip_if_not_installed("poLCA")
    d <- sim_tests(0.35, c(.9, .8, .75), c(.92, .88, .93), n = 150)
    d$t1 <- factor(rep("neg", nrow(d)), c("neg", "pos"))   # positive level never observed
    # Before: "gradient function must return a numeric vector of length 5" -- poLCA had
    # dropped t1 and the refinement got starting values for two tests instead of three.
    expect_error(run_ngs(d), "Only one result was observed for: 't1'", fixed = TRUE)
    d$t1 <- factor(rep("pos", nrow(d)), c("neg", "pos"))
    expect_error(run_ngs(d), "Only one result was observed for: 't1'", fixed = TRUE)
    # penalized EM keeps the column and reports it as not estimable rather than failing
    expect_no_warning(r <- run_ngs(d, method = "bayesian"))
    expect_match(notices_of(r), "Latent parameters not estimable", fixed = TRUE)
    # a constant rater: kappa is exactly 0 and its null-variance z-test is undefined
    ag <- r$agreement_stats$asDF
    expect_equal(ag$kappa[1:2], c(0, 0))
    expect_true(all(is.na(ag$p_value[1:2])))
    expect_true(is.finite(ag$p_value[3]))
})

test_that("pattern probabilities and their Jacobian match the direct row-product formula", {
    set.seed(4)
    k <- 4
    patterns <- as.matrix(expand.grid(rep(list(0:1), k)))
    par <- c(.37, runif(k, .5, .99), runif(k, .01, .5))
    got <- ClinicoPath:::nogoldstandard_probabilities(par, patterns)
    lik <- function(q) apply(patterns, 1, function(x) prod(ifelse(x == 1, q, 1 - q)))
    q1 <- par[1 + seq_len(k)]; q0 <- par[1 + k + seq_len(k)]
    expect_equal(got$prob, par[1] * lik(q1) + (1 - par[1]) * lik(q0), tolerance = 1e-14)
    # the analytic Jacobian against central differences
    num <- vapply(seq_along(par), function(i) {
        h <- 1e-6; up <- dn <- par; up[i] <- up[i] + h; dn[i] <- dn[i] - h
        (ClinicoPath:::nogoldstandard_probabilities(up, patterns)$prob -
         ClinicoPath:::nogoldstandard_probabilities(dn, patterns)$prob) / (2 * h)
    }, numeric(nrow(patterns)))
    expect_equal(got$jacobian, num, tolerance = 1e-7)
})


# Regressions from "fix remaining issues", 2026-10-05.

test_that("latent-class bootstrap keeps resamples in which a rare test is constant", {
    skip_if_not_installed("poLCA")
    d <- sim_tests(0.35, c(.9, .8, .85), c(.92, .88, .9), n = 150, seed = 21)
    t4 <- rep(0, nrow(d)); t4[which(d$t1 == "pos")[1:2]] <- 1
    d$t4 <- factor(c("neg", "pos")[t4 + 1], c("neg", "pos"))
    r <- run_ngs(d, test4 = "t4", test4Positive = "pos", bootstrap = TRUE, nboot = 100, seed = 3)
    # Before: every resample without a t4 positive failed (18% here) and was discarded, so
    # t4's sensitivity interval could not reach 0 although those resamples estimate 0.
    expect_false(grepl("Some bootstrap replicates failed", notices_of(r), fixed = TRUE))
    tm <- r$test_metrics$asDF
    expect_equal(tm$sens_ci_lower[4], 0)
    expect_true(all(is.finite(tm$sens_ci_lower[1:3])))
})

test_that("the kappa table explains a blank p-value only when a test is constant", {
    d <- sim_tests(0.35, c(.9, .8, .75), c(.92, .88, .93), n = 150)
    ok <- run_ngs(d, method = "composite")
    expect_null(ok$agreement_stats$notes$kappa_constant)
    d$t1 <- factor(rep("pos", nrow(d)), c("neg", "pos"))
    r <- run_ngs(d, method = "composite")
    expect_match(r$agreement_stats$notes$kappa_constant$note, "only one observed result", fixed = TRUE)
    expect_true(all(is.na(r$agreement_stats$asDF$p_value[1:2])))
})


# PPV/NPV confidence intervals, 2026-10-05.

test_that("rule-method PPV/NPV intervals are Wilson intervals on the test's own counts", {
    d <- sim_tests(0.35, c(.9, .8, .75), c(.92, .88, .93), n = 300)
    r <- run_ngs(d, method = "composite")
    x <- as.data.frame(lapply(d, function(v) as.numeric(v == "pos")))
    ref <- rowMeans(x) > 0.5
    tm <- r$test_metrics$asDF
    for (j in 1:3) {
        pos <- x[[j]] == 1
        expect_equal(c(tm$ppv_ci_lower[j], tm$ppv_ci_upper[j]),
                     as.numeric(prop.test(sum(ref & pos), sum(pos), correct = FALSE)$conf.int), tolerance = 1e-10)
        expect_equal(c(tm$npv_ci_lower[j], tm$npv_ci_upper[j]),
                     as.numeric(prop.test(sum(!ref & !pos), sum(!pos), correct = FALSE)$conf.int), tolerance = 1e-10)
    }
    expect_match(r$test_metrics$notes$predictive$note, "different prevalence", fixed = TRUE)
})

test_that("bootstrap PPV/NPV intervals equal an independent empirical bootstrap", {
    d <- sim_tests(0.35, c(.9, .8, .75), c(.92, .88, .93), n = 120)
    r <- run_ngs(d, method = "composite", bootstrap = TRUE, nboot = 300, seed = 11)
    x <- as.matrix(as.data.frame(lapply(d, function(v) as.numeric(v == "pos"))))
    # the module draws one sample.int(n, n, TRUE) per resample from the seeded stream
    idx <- withr::with_seed(11L, lapply(seq_len(300), function(b) sample.int(nrow(x), nrow(x), TRUE)))
    pv <- vapply(idx, function(i) {
        xb <- x[i, ]; ref <- rowMeans(xb) > 0.5
        unlist(lapply(1:3, function(j) { p <- xb[, j] == 1
            c(if (any(p)) mean(ref[p]) else NA, if (any(!p)) mean(!ref[!p]) else NA) }))
    }, numeric(6))
    q <- function(v) stats::quantile(v[is.finite(v)], c(.025, .975), names = FALSE)
    tm <- r$test_metrics$asDF
    for (j in 1:3) {
        expect_equal(c(tm$ppv_ci_lower[j], tm$ppv_ci_upper[j]), q(pv[2 * j - 1, ]), tolerance = 1e-12)
        expect_equal(c(tm$npv_ci_lower[j], tm$npv_ci_upper[j]), q(pv[2 * j, ]), tolerance = 1e-12)
    }
})

test_that("predictive values stay exact when a resample has an empty rule group", {
    p <- ClinicoPath:::nogoldstandardClass$new(
        options = ClinicoPath:::nogoldstandardOptions$new(test1Positive = NULL, test2Positive = NULL,
            test3Positive = NULL, test4Positive = NULL, test5Positive = NULL),
        data = data.frame())$.__enclos_env__$private
    # prevalence 0: no positive group, sensitivity NA -- PPV is 0 and NPV 1 (not dropped)
    v <- p$.calculatePPVNPV(c(NA, .8, NA), c(.7, .9, 1), c(0, .3, 0))
    expect_equal(v$ppv, c(0, .8 * .3 / (.8 * .3 + .1 * .7), NA))
    expect_equal(v$npv, c(1, .9 * .7 / (.9 * .7 + .2 * .3), 1))
    # prevalence 1: no negative group, specificity NA -- PPV is 1 and NPV 0
    v <- p$.calculatePPVNPV(.6, NA, 1)
    expect_equal(c(v$ppv, v$npv), c(1, 0))
})

test_that("predictive-value intervals: blank by construction, by method, and present with bootstrap", {
    d <- sim_tests(0.35, c(.9, .8, .75), c(.92, .88, .93), n = 200)
    ap <- run_ngs(d, method = "all_positive", bootstrap = TRUE, nboot = 100)$test_metrics$asDF
    expect_true(all(is.na(c(ap$npv_ci_lower, ap$npv_ci_upper))))     # NPV fixed at 100%
    expect_true(all(is.finite(ap$ppv_ci_lower)))
    an <- run_ngs(d, method = "any_positive", bootstrap = TRUE, nboot = 100)
    expect_true(all(is.na(an$test_metrics$asDF$ppv_ci_lower)))     # PPV fixed at 100%
    expect_true(all(is.finite(an$test_metrics$asDF$npv_ci_lower)))
    expect_false(grepl("intervals left blank", notices_of(an), fixed = TRUE))

    skip_if_not_installed("poLCA")
    lc <- run_ngs(d)
    expect_true(all(is.na(lc$test_metrics$asDF$ppv_ci_lower)))     # latent: bootstrap only
    expect_match(lc$test_metrics$notes$ci_provenance$note, "predictive values", fixed = TRUE)
    lb <- run_ngs(d, bootstrap = TRUE, nboot = 100)$test_metrics$asDF
    expect_true(all(lb$ppv_ci_lower <= lb$ppv & lb$ppv <= lb$ppv_ci_upper))
    expect_true(all(lb$npv_ci_lower <= lb$npv & lb$npv <= lb$npv_ci_upper))
})

test_that("a test with sensitivity + specificity < 1 is named (2026-10-21)", {
    skip_if_not_installed("poLCA")
    # Orientation follows the tests on average, so one test whose positive level was chosen
    # the wrong way round reads as a poor test; only a generic warning used to appear.
    d <- sim_tests(0.30, c(.90, .80, .70), c(.95, .85, .75), n = 500, seed = 5)
    rev <- run_ngs(d, method = "latent_class", test3Positive = "neg")
    tm <- rev$test_metrics$asDF
    expect_lt(tm$sensitivity[3] + tm$specificity[3], 1)
    expect_match(notices_of(rev), "below 1 for: t3", fixed = TRUE)
    expect_false(grepl("below 1 for", notices_of(run_ngs(d, method = "latent_class")), fixed = TRUE))
})
