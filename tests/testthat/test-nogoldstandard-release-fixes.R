# Regression tests for the 2026-10-21 independent review (VAL-NGS-01, -02, -04).
# Fixture: within each class T2 copies T1 with probability .7 (marginal Se .8/.8/.75/.85,
# Sp .9/.9/.9/.85, prevalence .3) -- the review's cdsim().

rf_cdsim <- function(n, k, seed) {
    set.seed(seed); dz <- rbinom(n, 1, .3)
    t1 <- ifelse(dz == 1, rbinom(n, 1, .8), rbinom(n, 1, .1))
    t2i <- ifelse(dz == 1, rbinom(n, 1, .8), rbinom(n, 1, .1)); cp <- rbinom(n, 1, .7)
    t2 <- ifelse(cp == 1, t1, t2i)
    t3 <- ifelse(dz == 1, rbinom(n, 1, .75), rbinom(n, 1, .1))
    t4 <- ifelse(dz == 1, rbinom(n, 1, .85), rbinom(n, 1, .15))
    fac <- function(v) factor(v, levels = 0:1, labels = c("neg", "pos"))
    d <- data.frame(t1 = fac(t1), t2 = fac(t2), t3 = fac(t3), t4 = fac(t4))
    d[, seq_len(k)]
}

rf_run <- function(d, method) {
    args <- list(data = d, method = method,
                 test1 = "t1", test1Positive = "pos", test2 = "t2", test2Positive = "pos",
                 test3 = "t3", test3Positive = "pos", test4 = NULL, test4Positive = NULL,
                 test5 = NULL, test5Positive = NULL)
    if ("t4" %in% names(d)) { args$test4 <- "t4"; args$test4Positive <- "pos" }
    do.call(ClinicoPath::nogoldstandard, args)
}

rf_notices <- function(res)
    gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(res$notices$content, collapse = " ")))

THREE_TEST <- "Conditional independence cannot be checked with three tests"

test_that("penalized EM checks conditional dependence (VAL-NGS-01)", {
    r <- rf_run(rf_cdsim(3000, 4, 301), "bayesian")
    cd <- r$conditional_dependence$asDF
    # Before: no rows, no notice, while latent_class flags the same data (BVR 94.6).
    expect_equal(nrow(cd), 6L)
    expect_true(all(is.finite(cd$bvr)))
    expect_true(any(cd$bvr > stats::qchisq(.95, 1)))
    expect_match(rf_notices(r), "Tests do not appear to err independently", fixed = TRUE)
    notes <- vapply(r$conditional_dependence$notes, function(n) n$note, "")
    expect_true(any(grepl("unpenalized", notes, fixed = TRUE)))
})

test_that("penalized EM residuals equal the latent_class residuals (same ML model)", {
    skip_if_not_installed("poLCA")
    d <- rf_cdsim(3000, 4, 301)
    bvr_b <- rf_run(d, "bayesian")$conditional_dependence$asDF$bvr
    bvr_l <- rf_run(d, "latent_class")$conditional_dependence$asDF$bvr
    expect_equal(bvr_b, bvr_l, tolerance = 1e-4)
})

test_that("three-test latent models warn that dependence cannot be checked (VAL-NGS-02)", {
    skip_if_not_installed("poLCA")
    d3 <- rf_cdsim(3000, 3, 302)
    r3 <- rf_run(d3, "latent_class")
    expect_match(rf_notices(r3), THREE_TEST, fixed = TRUE)
    expect_match(rf_notices(rf_run(d3, "bayesian")), THREE_TEST, fixed = TRUE)
    expect_false(grepl(THREE_TEST, rf_notices(rf_run(rf_cdsim(3000, 4, 301), "latent_class")),
                       fixed = TRUE))
    # VAL-NGS-04: the exact three-test fit shows G-squared 0, not +/-1e-13 rounding noise.
    fit <- r3$model_fit$asDF
    expect_identical(fit$value[fit$statistic == "G-squared"], 0)
})

test_that("an unflagged residual is not worded as confirming independence (VAL-NGS-04)", {
    skip_if_not_installed("poLCA")
    cd <- rf_run(rf_cdsim(3000, 4, 301), "latent_class")$conditional_dependence$asDF
    below <- cd$verdict[cd$bvr <= stats::qchisq(.95, 1)]
    expect_gt(length(below), 0)
    expect_true(all(below == "No evidence against conditional independence (descriptive)"))
})
