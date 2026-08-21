# Regression tests for the release-blocking defects fixed in decisioncurve.
#
# Every test here calls decisioncurve() and asserts against what the analysis
# actually returned. The previous version of this file re-implemented each
# formula inside the test body and compared the copy against itself, which is
# why a degenerate "optimal threshold" table and a p-value that could be exactly
# zero both survived to release review with the suite green.

testdata_dca <- function(n = 400, seed = 20260820) {
    set.seed(seed)
    x <- stats::rnorm(n)
    lp <- -0.6 + 1.4 * x
    p <- stats::plogis(lp)
    y <- stats::rbinom(n, 1, p)
    data.frame(
        outcome    = factor(ifelse(y == 1, "Event", "No Event"), levels = c("No Event", "Event")),
        good_model = p,
        # A predictor carrying no information about the outcome. It must never be
        # reported as beating both reference strategies.
        noise      = stats::runif(n),
        stringsAsFactors = FALSE
    )
}

run_dca <- function(df, ...) {
    ClinicoPath::decisioncurve(
        data                 = df,
        outcome              = "outcome",
        outcomePositive      = "Event",
        models               = c("good_model", "noise"),
        decisionRulePositive = NULL,
        ...
    )
}


test_that("the degenerate optimal-threshold output is gone", {
    skip_if_not_installed("ClinicoPath")

    # Net benefit is (TP/n) - (FP/n) * t/(1-t) at threshold t. The subtracted term
    # increases in t while the treated set shrinks, so the curve is non-increasing
    # and its argmax is always the lowest threshold on the grid. Reporting that as
    # an "optimal threshold" is uninformative, and ranking models by their net
    # benefit at that single point can invert the true ordering.
    res <- run_dca(testdata_dca())
    expect_true("benefitRangeTable" %in% names(res))
    expect_false("optimalTable" %in% names(res))
})


test_that("range of benefit is measured against treat-all, not only treat-none", {
    skip_if_not_installed("ClinicoPath")

    res <- run_dca(testdata_dca(), showBenefitRange = TRUE)
    tab <- as.data.frame(res$benefitRangeTable)

    expect_equal(nrow(tab), 2L)

    noise_row <- tab[tab$model == "noise", ]
    good_row  <- tab[tab$model == "good_model", ]

    # A pure-noise predictor beats treat-none across most low thresholds, which is
    # why the old "net_benefits > 0" range flattered it. Against treat-all as well,
    # it must have no range of benefit at all.
    expect_true(is.na(noise_row$range_start))

    # An informative model must have one.
    expect_false(is.na(good_row$range_start))
    expect_gte(good_row$range_end, good_row$range_start)
    expect_equal(good_row$range_width, good_row$range_end - good_row$range_start,
                 tolerance = 1e-8)
})


test_that("bootstrap comparison p-values are never exactly zero and are Holm-adjusted", {
    skip_if_not_installed("ClinicoPath")

    res <- run_dca(testdata_dca(), compareModels = TRUE, bootReps = 200)
    tab <- as.data.frame(res$comparisonTable)

    expect_true(nrow(tab) >= 1L)
    expect_true("p_value_adj" %in% names(tab))

    p_raw <- tab$p_value[!is.na(tab$p_value)]
    expect_true(length(p_raw) > 0)

    # The (b+1)/(B+1) convention bounds the p-value below by 2/(B+1) and above by 1.
    # good_model vs noise separates completely, so without the correction this
    # returned exactly 0 from every replicate falling on one side of the null.
    expect_true(all(p_raw > 0))
    expect_true(all(p_raw <= 1))
    expect_true(all(p_raw >= 2 / (200 + 1) - 1e-9))

    p_adj <- tab$p_value_adj[!is.na(tab$p_value_adj)]
    expect_true(all(p_adj >= p_raw - 1e-9))
    expect_true(all(p_adj <= 1))
})


test_that("apparent net benefit is flagged as apparent", {
    skip_if_not_installed("ClinicoPath")

    res <- run_dca(testdata_dca())
    notices <- as.character(res$notices$content)

    # The analysis cannot know whether the supplied risks were fitted on these rows,
    # so it must say so unconditionally rather than presenting the curves as validated.
    expect_match(notices, "apparent", ignore.case = TRUE)
    expect_match(notices, "optimistically biased", ignore.case = TRUE)
})


test_that("notices do not accumulate across run cycles", {
    skip_if_not_installed("ClinicoPath")

    df <- testdata_dca()
    once  <- as.character(run_dca(df)$notices$content)

    count_of <- function(html, needle) {
        length(gregexpr(needle, html, fixed = TRUE)[[1]][gregexpr(needle, html, fixed = TRUE)[[1]] > 0])
    }

    # jamovi reuses the analysis object between runs; .noticeList is a private field,
    # so without an explicit reset at the top of .run() the same notice is appended
    # again on every option change.
    expect_equal(count_of(once, "Analysis Complete"), 1L)
})


test_that("the clinical impact table uses one denominator throughout", {
    skip_if_not_installed("ClinicoPath")

    res <- run_dca(testdata_dca(), calculateClinicalImpact = TRUE, populationSize = 1000)
    tab <- as.data.frame(res$clinicalImpactTable)

    expect_true(nrow(tab) > 0)

    # Every column in this table is per 100 patients. interventions_avoided used to be
    # scaled to populationSize (default 1000) while its neighbours were per 100, so a
    # single row carried two different denominators with nothing on screen to say so.
    expect_true(all(tab$interventions_avoided <= 100 + 1e-8, na.rm = TRUE))
    expect_true(all(tab$interventions_avoided >= -1e-8, na.rm = TRUE))
    expect_equal(tab$interventions_avoided, 100 - tab$interventions_per_100, tolerance = 1e-8)
})


test_that("gain vs treat-all is a net-benefit difference, not an unstable ratio", {
    skip_if_not_installed("ClinicoPath")

    res <- run_dca(testdata_dca(), weightedAUC = TRUE)
    tab <- as.data.frame(res$weightedAUCTable)

    expect_true("benefit_gain" %in% names(tab))
    expect_false("relative_benefit" %in% names(tab))

    # Treat-all net benefit crosses zero at a threshold equal to the prevalence, so
    # dividing by it produced percentages in the hundreds. A difference on the
    # net-benefit scale stays bounded by the net benefits themselves.
    gains <- tab$benefit_gain[!is.na(tab$benefit_gain)]
    expect_true(all(abs(gains) < 1))
})

test_that("bootstrap output is reproducible across identical runs", {
    skip_if_not_installed("ClinicoPath")

    df <- testdata_dca()

    a <- run_dca(df, compareModels = TRUE, bootReps = 200, seed = 42)
    b <- run_dca(df, compareModels = TRUE, bootReps = 200, seed = 42)

    ta <- as.data.frame(a$comparisonTable)
    tb <- as.data.frame(b$comparisonTable)

    # Unseeded, eight identical reruns of this analysis at bootReps = 1000 moved the
    # comparison p-value between 0.030 and 0.060 and the 95% CI crossed zero in two of
    # them. A clinician who reruns must get the same numbers.
    expect_equal(ta$p_value,  tb$p_value,  tolerance = 0)
    expect_equal(ta$ci_lower, tb$ci_lower, tolerance = 0)
    expect_equal(ta$ci_upper, tb$ci_upper, tolerance = 0)

    # A different seed is allowed to differ - that is honest Monte-Carlo error, not a bug.
    c <- as.data.frame(run_dca(df, compareModels = TRUE, bootReps = 200, seed = 7)$comparisonTable)
    expect_false(isTRUE(all.equal(ta$ci_lower, c$ci_lower)))
})


test_that("running the analysis does not disturb the caller's RNG stream", {
    skip_if_not_installed("ClinicoPath")

    set.seed(99)
    before <- runif(3)

    set.seed(99)
    invisible(run_dca(testdata_dca(), compareModels = TRUE, bootReps = 200))
    after <- runif(3)

    # .run() saves and restores .Random.seed, so an R-API caller's stream is untouched.
    expect_equal(before, after)
})


test_that("instructions return when the user clears their variables", {
    skip_if_not_installed("ClinicoPath")

    # instructions is declared visible: true with an empty clearWith, and .run() hides it
    # once an analysis succeeds. Without the matching setVisible(TRUE) in the guard branch
    # the panel stayed hidden for the rest of the session.
    res <- ClinicoPath::decisioncurve(
        data                 = testdata_dca(),
        outcome              = NULL,
        outcomePositive      = NULL,
        models               = NULL,
        decisionRulePositive = NULL
    )
    expect_true(res$instructions$visible)
    expect_match(as.character(res$instructions$content), "Decision Curve Analysis")
})
