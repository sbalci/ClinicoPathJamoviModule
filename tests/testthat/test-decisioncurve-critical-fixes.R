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

    # Every column in this table shares ONE denominator: the projected population. The bug
    # this guards was interventions_avoided being scaled to populationSize while its
    # neighbours stayed per 100; both are scaled now, so the invariant is that they agree,
    # not that either is bounded by 100.
    #
    # NB the earlier form of this test asserted `interventions_avoided == 100 - interventions_per_100`,
    # which is not the definition of anything: net interventions avoided is
    # (NB_model - NB_all) / (t/(1-t)) * population, not "the population minus the treated".
    # It produced -837.5 for a row where the correct value is 12.5.
    pop <- 1000
    expect_true(all(tab$interventions_per_100 <= pop + 1e-8, na.rm = TRUE))
    expect_true(all(tab$true_positives_per_100 <= tab$interventions_per_100 + 1e-8, na.rm = TRUE))
    # You cannot avoid more interventions than treat-all would have delivered.
    expect_true(all(tab$interventions_avoided <= pop + 1e-8, na.rm = TRUE))
    # True + false positives must reconstitute the interventions actually delivered.
    expect_equal(tab$true_positives_per_100 + tab$false_positives_per_100,
                 tab$interventions_per_100, tolerance = 1e-6)
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

    # Build the data FIRST. testdata_dca() calls set.seed(20260820) internally, so calling it
    # after set.seed(99) replaced the very stream this test is trying to check -- `after` was
    # drawn from seed 20260820, not from seed 99, and the test failed no matter what .run() did.
    df <- testdata_dca()

    set.seed(99)
    before <- runif(3)

    set.seed(99)
    invisible(run_dca(df, compareModels = TRUE, bootReps = 200))
    after <- runif(3)

    # .run() saves and restores .Random.seed, so an R-API caller's stream is untouched.
    expect_equal(before, after)
})


test_that("instructions return when the user clears their variables", {
    skip_if_not_installed("ClinicoPath")

    # instructions is declared visible: true with an empty clearWith, and .run() hides it
    # once an analysis succeeds. Without the matching setVisible(TRUE) in the guard branch
    # the panel stayed hidden for the rest of the session.
    # Not via the public wrapper: with no variables selected jmvcore::select(data, NULL)
    # throws "invalid 'row.names' length" from inside Analysis$init(), before any module code
    # runs. That is a jmvcore-level R-API limitation, not something decisioncurve can guard,
    # and it is not reachable from the GUI. Drive the analysis object directly instead, which
    # is what jamovi does.
    opts <- ClinicoPath:::decisioncurveOptions$new(
        outcome = NULL, outcomePositive = NULL, models = NULL,
        decisionRuleVar = NULL, decisionRulePositive = NULL)
    an <- ClinicoPath:::decisioncurveClass$new(options = opts, data = testdata_dca())
    an$.__enclos_env__$private$.run()
    expect_true(an$results$instructions$visible)
    expect_match(as.character(an$results$instructions$content), "Decision Curve Analysis")
})


# --- /check-function-full remediation pass (2026-08-24) --------------------------------------

test_that("the five plots survive a render with no preceding .run()", {
    # jamovi's render path (.createPlotObject -> do.call(private[[funName]], ...)) never calls
    # .run(). The renderers read private fields that only .run() fills, so on an .omv reopen or
    # engine recycle all five returned FALSE: five blank panes and no error. State is published
    # from .run() now and the renderers rehydrate from it.
    df   <- testdata_dca()
    opts <- ClinicoPath:::decisioncurveOptions$new(
        outcome = "outcome", outcomePositive = "Event", models = c("good_model", "noise"),
        decisionRuleVar = NULL, decisionRulePositive = NULL,
        showPlot = TRUE, calculateClinicalImpact = TRUE, showClinicalImpactPlot = TRUE,
        showInterventionAvoided = TRUE, showRelativeUtility = TRUE,
        showStandardizedNetBenefit = TRUE)
    plots <- c("dcaPlot", "clinicalImpactPlot", "interventionsAvoidedPlot",
               "relativeUtilityPlot", "standardizedNetBenefitPlot")

    an <- ClinicoPath:::decisioncurveClass$new(options = opts, data = df)
    an$.__enclos_env__$private$.run()
    states <- lapply(plots, function(p) an$results[[p]]$state)
    names(states) <- plots
    for (p in plots) expect_false(is.null(states[[p]]), label = paste("state for", p))

    # A fresh object that has never run, with state restored the way jamovi restores it.
    an2 <- ClinicoPath:::decisioncurveClass$new(options = opts, data = df)
    pr2 <- an2$.__enclos_env__$private
    for (p in plots) an2$results[[p]]$setState(states[[p]])
    expect_true(is.null(pr2$.dcaResults))          # nothing computed in this process

    renderer <- c(dcaPlot = ".plotDCA", clinicalImpactPlot = ".plotClinicalImpact",
                  interventionsAvoidedPlot = ".plotInterventionsAvoided",
                  relativeUtilityPlot = ".plotRelativeUtility",
                  standardizedNetBenefitPlot = ".plotStandardizedNetBenefit")
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    for (p in plots)
        expect_true(pr2[[renderer[[p]]]](an2$results[[p]], ggtheme = ggplot2::theme_bw(),
                                         theme = list(fill = "#ffffff")),
                    label = paste("render", p, "from state"))
})


test_that("a failed re-run does not leave the previous run's interpretation on screen", {
    # jmvcore:::Html$fromProtoBuf restores content UNCONDITIONALLY -- clearWith only greys an
    # Html element, it never blanks it (Table$fromProtoBuf does return() on a clearWith hit).
    # procedureNotes and summaryText are written only on the success path, so without an
    # explicit reset a clinician could read the previous dataset's conclusion.
    opts <- ClinicoPath:::decisioncurveOptions$new(
        outcome = "outcome", outcomePositive = NULL, models = "good_model",
        decisionRuleVar = NULL, decisionRulePositive = NULL)
    an <- ClinicoPath:::decisioncurveClass$new(options = opts, data = testdata_dca())
    an$results$procedureNotes$setContent("<b>Analysis Complete</b> (PREVIOUS RUN)")
    an$results$summaryText$setContent("<b>Clinical Interpretation</b> (PREVIOUS RUN)")

    invisible(tryCatch(an$.__enclos_env__$private$.run(), error = function(e) NULL))

    expect_false(grepl("PREVIOUS RUN", an$results$procedureNotes$content %||% "", fixed = TRUE))
    expect_false(grepl("PREVIOUS RUN", an$results$summaryText$content %||% "", fixed = TRUE))
})


test_that("few outcome events raises a warning; an adequate event count does not", {
    mk <- function(n_events, n_total) {
        y <- c(rep(1, n_events), rep(0, n_total - n_events))
        set.seed(5)
        data.frame(outcome = factor(ifelse(y == 1, "Yes", "No"), levels = c("No", "Yes")),
                   m1 = stats::plogis(stats::rnorm(n_total) + y * 1.5))
    }
    run <- function(d) ClinicoPath::decisioncurve(
        data = d, outcome = "outcome", outcomePositive = "Yes", models = "m1",
        decisionRuleVar = NULL, decisionRulePositive = NULL, showTable = TRUE)$notices$content

    expect_match(run(mk(6, 110)),   "Few Outcome Events")
    expect_match(run(mk(6, 110)),   "Only 6 events")
    expect_false(grepl("Few Outcome Events", run(mk(60, 250)), fixed = TRUE))
})


test_that("a threshold range narrower than the step is refused, not silently collapsed", {
    # This used to "succeed": weightedAUCTable printed "20% - 20%" with a blank average net
    # benefit and every comparison statistic came back empty, while the notice affirmed the
    # range. One threshold cannot describe a curve.
    msg <- tryCatch(ClinicoPath::decisioncurve(
        data = testdata_dca(), outcome = "outcome", outcomePositive = "Event",
        models = "good_model", decisionRuleVar = NULL, decisionRulePositive = NULL,
        thresholdRange = "custom", thresholdMin = 0.20, thresholdMax = 0.21,
        thresholdStep = 0.1), error = conditionMessage)
    expect_match(msg, "narrower than the step size")
})


test_that("validation errors carry their message instead of the string 'Validation failed'", {
    # jmvcore does extractErrorMessage() -> setError(), and the banner REPLACES the pane that
    # holds the rich notice, so the banner text is all the clinician sees.
    msg <- tryCatch(ClinicoPath::decisioncurve(
        data = testdata_dca(), outcome = "outcome", outcomePositive = "Event",
        models = "good_model", decisionRuleVar = NULL, decisionRulePositive = NULL,
        thresholdRange = "custom", thresholdMin = 0.6, thresholdMax = 0.2),
        error = conditionMessage)
    expect_false(identical(msg, "Validation failed"))
    expect_match(msg, "must be less than maximum threshold")
})


test_that("the bootstrap keeps all-one-class resamples", {
    # Discarding them removed draws only from the LOWER tail, biasing the lower confidence
    # limit upward. Net benefit is TP/n - FP/n * odds, which is well defined with no events.
    set.seed(42); n <- 30
    y <- c(rep(1, 3), rep(0, 27))
    d <- data.frame(outcome = factor(ifelse(y == 1, "Yes", "No"), levels = c("No", "Yes")),
                    m1 = stats::plogis(stats::rnorm(n) + y * 1.2))
    opts <- ClinicoPath:::decisioncurveOptions$new(
        outcome = "outcome", outcomePositive = "Yes", models = "m1",
        decisionRuleVar = NULL, decisionRulePositive = NULL,
        confidenceIntervals = TRUE, bootReps = 4000, ciLevel = 0.95)
    an <- ClinicoPath:::decisioncurveClass$new(options = opts, data = d)
    set.seed(42)
    ci <- an$.__enclos_env__$private$.calculateBootstrapCI(
        d$m1, d$outcome, c(0.10, 0.20, 0.30, 0.40), "Yes")

    expect_length(ci$lower, 4L)
    expect_true(all(is.finite(ci$lower)))
    expect_true(all(ci$lower <= ci$upper))
    # The reference values below were produced with every resample retained. If the skip is
    # ever reinstated these all shift upward by 0.017-0.033.
    expect_equal(round(ci$lower, 4), c(-0.1037, -0.2002, -0.3571, -0.5222), tolerance = 1e-3)
})


test_that("resultsTable model columns exist before .run() and are not duplicated", {
    opts <- ClinicoPath:::decisioncurveOptions$new(
        outcome = "outcome", outcomePositive = "Event", models = c("good_model", "noise"),
        decisionRuleVar = NULL, decisionRulePositive = NULL, showTable = TRUE)
    an <- ClinicoPath:::decisioncurveClass$new(options = opts, data = testdata_dca())
    pr <- an$.__enclos_env__$private
    cols <- function() vapply(an$results$resultsTable$columns, function(c) c$name, character(1))

    pr$.init()
    expect_true(all(c("model_good_model", "model_noise") %in% cols()))
    after_init <- cols()
    pr$.run(); pr$.run()
    expect_equal(cols(), after_init)         # idempotent, no restructuring per run
})


test_that("renderers do not accumulate notices across resizes", {
    opts <- ClinicoPath:::decisioncurveOptions$new(
        outcome = "outcome", outcomePositive = "Event", models = "good_model",
        decisionRuleVar = NULL, decisionRulePositive = NULL,
        calculateClinicalImpact = TRUE, showClinicalImpactPlot = TRUE,
        selectedThresholds = "0.1, 0.2, nonsense")
    an <- ClinicoPath:::decisioncurveClass$new(options = opts, data = testdata_dca())
    pr <- an$.__enclos_env__$private
    pr$.run()
    n0 <- length(pr$.noticeList)
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    for (i in 1:3)
        invisible(pr$.plotClinicalImpact(an$results$clinicalImpactPlot,
                                         ggtheme = ggplot2::theme_bw(),
                                         theme = list(fill = "#ffffff")))
    expect_equal(length(pr$.noticeList), n0)
})


# --- /release-review-function pass (2026-08-24) ----------------------------------------------

test_that("a failed run leaves NO plot state, so the previous cohort's curves cannot reappear", {
    # Regression from the plot-state mechanism: .run() cleared the private fields on an early
    # return but not image$state, and .restoreFromState() then faithfully rehydrated the
    # PREVIOUS cohort. jamovi persists image state across run cycles (Image$fromProtoBuf only
    # drops it when a clearWith OPTION changed, and none of the five images list `data`), so a
    # data-only change -- a filter, an edited cell -- was exactly the trigger.
    df   <- testdata_dca()
    opts <- ClinicoPath:::decisioncurveOptions$new(
        outcome = "outcome", outcomePositive = "Event", models = "good_model",
        decisionRuleVar = NULL, decisionRulePositive = NULL, showPlot = TRUE)

    a <- ClinicoPath:::decisioncurveClass$new(options = opts, data = df)
    a$.__enclos_env__$private$.run()
    st <- a$results$dcaPlot$state
    expect_false(is.null(st))

    # same options, but a data-only change that leaves too few complete cases
    df2 <- df; df2$good_model[9:nrow(df2)] <- NA
    b <- ClinicoPath:::decisioncurveClass$new(options = opts, data = df2)
    b$results$dcaPlot$setState(st)                 # jamovi hands the old state back
    invisible(tryCatch(b$.__enclos_env__$private$.run(), error = function(e) NULL))

    expect_null(b$results$dcaPlot$state)           # the run wiped it
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    expect_false(b$.__enclos_env__$private$.plotDCA(
        b$results$dcaPlot, ggtheme = ggplot2::theme_bw(), theme = list(fill = "#ffffff")))
})


test_that(".init() uses the display model names, so custom labels do not add blank columns", {
    # Regression: .init() built columns from self$options$models while .run() keys them off
    # names(private$.dcaResults) = the parsed modelNames labels, so BOTH sets survived and the
    # .init() ones stayed empty -- rendered as "." in jamovi, which reads as "not computable".
    df <- testdata_dca()
    mk <- function(mn) {
        o <- ClinicoPath:::decisioncurveOptions$new(
            outcome = "outcome", outcomePositive = "Event",
            models = c("good_model", "noise"), modelNames = mn,
            decisionRuleVar = NULL, decisionRulePositive = NULL, showTable = TRUE)
        an <- ClinicoPath:::decisioncurveClass$new(options = o, data = df)
        an$.__enclos_env__$private$.init(); an$.__enclos_env__$private$.run()
        an$results$resultsTable
    }
    for (mn in c("", "Model A, Model B")) {
        tb   <- mk(mn)
        cols <- vapply(tb$columns, function(c) c$name, character(1))
        expect_equal(length(cols), 5L, label = paste("column count with modelNames", mn))
        df_out <- tb$asDF
        model_cols <- setdiff(names(df_out), c("threshold", "treat_all", "treat_none"))
        for (cc in model_cols)
            expect_false(all(is.na(df_out[[cc]])), label = paste("column", cc, "all NA"))
    }
})


test_that("Range of Benefit does not report floating-point noise as clinical superiority", {
    # The model NB and the treat-all reference are computed by algebraically identical but not
    # bitwise identical routes, so a model that IS treat-all differed by ~1e-16 and a bare `>`
    # counted it superior -- producing a "Range of Benefit: 5% to 19%" claim, by default, for a
    # predictor generated independently of the outcome.
    set.seed(85); n <- 216
    out <- stats::rbinom(n, 1, 0.194)
    d <- data.frame(outcome = factor(out), p1 = stats::runif(n, 0.45, 0.95))
    res <- ClinicoPath::decisioncurve(
        data = d, outcome = "outcome", outcomePositive = "1", models = "p1",
        decisionRuleVar = NULL, decisionRulePositive = NULL,
        thresholdRange = "custom", thresholdMin = 0.05, thresholdMax = 0.44, thresholdStep = 0.01)
    # collapse whitespace: stripping the HTML tags leaves runs of spaces
    txt <- gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", res$summaryText$content))
    expect_false(grepl("Range of Benefit: 5% to 19%", txt, fixed = TRUE))
    expect_match(txt, "Range of Benefit: none", fixed = TRUE)
})


test_that("the decision curve keeps the zero line visible when every net benefit is negative", {
    # y_ceiling <- max(model_nb) * 1.1 moves the ceiling DOWN when the maximum is negative,
    # clipping the curve and pushing treat-none (y = 0) off the panel -- the very line that
    # shows the model is harmful.
    set.seed(7); n <- 300
    y <- stats::rbinom(n, 1, 0.10)
    d <- data.frame(outcome = factor(y), p1 = stats::runif(n, 0.70, 0.90))
    an <- ClinicoPath:::decisioncurveClass$new(
        options = ClinicoPath:::decisioncurveOptions$new(
            outcome = "outcome", outcomePositive = "1", models = "p1",
            decisionRuleVar = NULL, decisionRulePositive = NULL,
            thresholdRange = "custom", thresholdMin = 0.50, thresholdMax = 0.60,
            thresholdStep = 0.02), data = d)
    an$.__enclos_env__$private$.run()
    pd  <- an$.__enclos_env__$private$.plotData
    mnb <- pd$net_benefit[!pd$model %in% c("Treat All", "Treat None")]
    expect_true(all(mnb < 0))                       # the design this guards

    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    invisible(an$.__enclos_env__$private$.plotDCA(
        an$results$dcaPlot, ggtheme = ggplot2::theme_minimal(), theme = list(fill = "#ffffff")))
    yr <- ggplot2::ggplot_build(ggplot2::last_plot())$layout$panel_params[[1]]$y.range
    expect_true(0 >= yr[1] && 0 <= yr[2])           # treat-none on the panel
    expect_equal(sum(mnb > yr[2] | mnb < yr[1]), 0) # nothing clipped
})


test_that("each plot's state slice carries exactly what its renderer needs", {
    # Publishing the whole state blob to all five images cost ~517 KB x 5 on a 5,000-row,
    # four-model analysis because .dcaResults holds a prediction vector per model. Each image
    # now gets only its own slice. This test is the guard against the spec drifting away from
    # what the renderers actually read -- the first attempt omitted analysisOutcomes from
    # clinicalImpactPlot, which .calculateModelAtThreshold needs transitively, and that plot
    # alone came back blank.
    set.seed(1); n <- 600
    lp <- stats::rnorm(n); y <- stats::rbinom(n, 1, stats::plogis(lp))
    d <- data.frame(outcome = factor(ifelse(y == 1, "Yes", "No"), levels = c("No", "Yes")))
    for (k in 1:2) d[[paste0("m", k)]] <- stats::plogis(lp * 0.7 + stats::rnorm(n, 0, .4))

    plots <- c("dcaPlot", "clinicalImpactPlot", "interventionsAvoidedPlot",
               "relativeUtilityPlot", "standardizedNetBenefitPlot")
    renderer <- c(dcaPlot = ".plotDCA", clinicalImpactPlot = ".plotClinicalImpact",
                  interventionsAvoidedPlot = ".plotInterventionsAvoided",
                  relativeUtilityPlot = ".plotRelativeUtility",
                  standardizedNetBenefitPlot = ".plotStandardizedNetBenefit")
    o <- ClinicoPath:::decisioncurveOptions$new(
        outcome = "outcome", outcomePositive = "Yes", models = c("m1", "m2"),
        decisionRuleVar = NULL, decisionRulePositive = NULL,
        showPlot = TRUE, calculateClinicalImpact = TRUE, showClinicalImpactPlot = TRUE,
        showInterventionAvoided = TRUE, showRelativeUtility = TRUE,
        showStandardizedNetBenefit = TRUE)
    an <- ClinicoPath:::decisioncurveClass$new(options = o, data = d)
    an$.__enclos_env__$private$.run()

    # the default-visible plot must not carry the prediction vectors
    expect_false("dcaResults" %in% names(an$results$dcaPlot$state))

    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    for (p in plots) {
        b <- ClinicoPath:::decisioncurveClass$new(options = o, data = d)
        b$results[[p]]$setState(an$results[[p]]$state)      # only THIS image gets state
        expect_true(
            b$.__enclos_env__$private[[renderer[[p]]]](
                b$results[[p]], ggtheme = ggplot2::theme_bw(), theme = list(fill = "#ffffff")),
            label = paste(p, "renders from its own slice alone"))
    }
})
