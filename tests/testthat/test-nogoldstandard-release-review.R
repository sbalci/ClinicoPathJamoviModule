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

    for (name in c("test2", "test2Positive", "test3", "test3Positive")) {
        option <- options$option(name)
        option$value <- NULL
    }
    analysis$run()

    expect_true(analysis$results$instructions$visible)
    expect_false(analysis$results$clinical_summary$visible)
    expect_identical(analysis$results$clinical_summary$content, "")
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
    expect_match(a_yaml, "Composite Reference \\(strict majority\\)")
    expect_match(u_yaml, "Composite Reference \\(strict majority\\)")
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
