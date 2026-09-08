# Regressions for the residual-review findings on jjhistostats.
# Each test asserts what the USER SEES (rendered panel text / raised message),
# not the internal mechanism.

run_analysis <- function(data, ...) {
    o <- do.call(ClinicoPath:::jjhistostatsOptions$new, list(...))
    a <- ClinicoPath:::jjhistostatsClass$new(options = o, data = data)
    a$init()
    a$.__enclos_env__$private$.run()
    a
}

# quiet data: n = 200, spans zero (so the "testing against zero" note stays silent),
# no MAD outliers, > 5 unique values, no grouping -> zero clinical warnings
quiet_data <- function() data.frame(v = seq(-100, 100, length.out = 200))


test_that("the To Do panel never shows the internal progress message", {
    # .prepareData()/.prepareOptions() used to setContent() a progress string into the
    # To Do panel, which is called AFTER .run() has composed the panel. On any dataset
    # producing no warnings the user was left reading
    # "Preparing histogram analysis options..." as the final result.
    a <- run_analysis(quiet_data(), dep = "v", resultssubtitle = FALSE)
    todo <- a$results$todo$content
    expect_false(grepl("Preparing histogram analysis options", todo, fixed = TRUE))
    expect_false(grepl("Processing data for histogram analysis", todo, fixed = TRUE))
    expect_match(todo, "You have selected to make a histogram")

    # and it survives a plot render, which calls the same cached helpers
    a$.__enclos_env__$private$.plot(image = NULL, ggtheme = NULL, theme = NULL)
    todo2 <- a$results$todo$content
    expect_false(grepl("Preparing histogram analysis options", todo2, fixed = TRUE))
    expect_match(todo2, "You have selected to make a histogram")
})


test_that("the distribution-shape bullet does not contradict the normality bullet", {
    shape <- function(x) {
        html <- run_analysis(data.frame(v = x), dep = "v", showInterpretation = TRUE,
                             resultssubtitle = FALSE)$results$interpretation$content
        sub(".*<strong>Distribution shape:</strong> ([^<]*)<.*", "\\1", html)
    }
    set.seed(11)
    bimodal <- c(rnorm(60, 2, 0.3), rnorm(60, 8, 0.3))   # |g1| ~ 0, Shapiro rejects
    expect_lt(shapiro.test(bimodal)$p.value, 0.05)

    # was: "Approximately symmetric (suitable for parametric tests)" while the two bullets
    # below it reported evidence against normality
    s <- shape(bimodal)
    expect_false(grepl("suitable for parametric tests", s, fixed = TRUE))
    expect_match(s, "Symmetric but not normal")

    # a constant column has no shape at all
    expect_match(shape(rep(50, 40)), "Constant")

    # ordinary normal data is still called symmetric
    set.seed(3)
    expect_match(shape(rnorm(200, 5, 1)), "Approximately symmetric")

    # the standing Note describes the rule actually in use
    html <- run_analysis(data.frame(v = rnorm(50)), dep = "v", showInterpretation = TRUE,
                         resultssubtitle = FALSE)$results$interpretation$content
    expect_match(html, "Shapiro-Wilk")
})


test_that("labels that are discarded are reported instead of silently dropped", {
    # These match the notice prose in sentence case, matching the .a.yaml control titles
    # ("Split by (optional)", "Statistical results"). Reword the notice and update here too.
    d <- quiet_data()

    # subtitle is overwritten by ggstatsplot whenever Statistical Results is on
    todo <- run_analysis(d, dep = "v", subtitle = "MY SUBTITLE",
                         resultssubtitle = TRUE)$results$todo$content
    expect_match(todo, "subtitle was not used")

    # ... and is honoured (so unreported) when Statistical Results is off
    todo <- run_analysis(d, dep = "v", subtitle = "MY SUBTITLE",
                         resultssubtitle = FALSE)$results$todo$content
    expect_false(grepl("subtitle was not used", todo, fixed = TRUE))

    # title/xlab are replaced per panel with more than one variable selected
    d2 <- data.frame(v = seq(-100, 100, length.out = 200),
                     w = seq(-50, 50, length.out = 200))
    todo <- run_analysis(d2, dep = c("v", "w"), title = "MY TITLE", xlab = "MY XLAB",
                         resultssubtitle = FALSE)$results$todo$content
    expect_match(todo, "title was not used")
    expect_match(todo, "X-axis label was not used")

    todo <- run_analysis(d, dep = "v", title = "MY TITLE", xlab = "MY XLAB",
                         resultssubtitle = FALSE)$results$todo$content
    expect_false(grepl("was not used", todo, fixed = TRUE))

    # caption: ggstatsplot writes the Bayes factor into the CAPTION slot for every
    # non-Bayesian test, so bf.message silently overwrote a user caption. This was the
    # one member of the set with no warning.
    cap <- function(...) run_analysis(d, dep = "v", caption = "MY CAPTION", ...)$results$todo$content
    expect_match(cap(resultssubtitle = TRUE, bf.message = TRUE), "caption was not used")

    # ...and every combination that leaves the caption alone stays quiet
    expect_false(grepl("caption was not used", cap(resultssubtitle = TRUE,  bf.message = FALSE)))
    expect_false(grepl("caption was not used", cap(resultssubtitle = FALSE, bf.message = TRUE)))
    # in Bayesian mode the factor goes to the subtitle, so the caption survives
    expect_false(grepl("caption was not used",
                       cap(resultssubtitle = TRUE, bf.message = TRUE,
                           typestatistics = "bayes")))
})


test_that("degenerate bin widths and self-grouping are refused with a readable message", {
    d <- quiet_data()

    # 1e-6 over a range of 200 asks for 200 million bins; ggplot2 refuses above 1e6 and
    # its error goes to stderr, so the user was shown an empty panel and nothing else
    expect_error(
        ClinicoPath::jjhistostats(data = d, dep = "v", changebinwidth = TRUE,
                                  binwidth = 1e-6),
        "bins")

    # a workable width is untouched
    expect_no_error(
        ClinicoPath::jjhistostats(data = d, dep = "v", changebinwidth = TRUE,
                                  binwidth = 5))

    # The same column in both boxes made stat_bin() fail with "requires a continuous x
    # aesthetic". (The public wrapper stops earlier, on jmvcore's own type check; this is
    # the analysis-object path the review reproduced it on.)
    dg <- data.frame(v = seq(-100, 100, length.out = 60))
    expect_error(run_analysis(dg, dep = "v", grvar = "v"), "Split by")
})


test_that("a constant variable says the histogram panel will be empty", {
    todo <- run_analysis(data.frame(v = rep(50, 60)), dep = "v",
                         resultssubtitle = FALSE)$results$todo$content
    expect_match(todo, "constant values")
    expect_match(todo, "will be empty")
})


test_that("option changes on a reused instance are not masked by a stale cache", {
    # .prepareData()/.prepareOptions()/.prepareAesthetics() used to share ONE
    # side-effecting guard: .shouldRefreshCache() rewrote the stored options hash and
    # returned TRUE, so the first caller in a run cycle consumed the "changed" signal
    # and the other two saw "unchanged" and returned the FIRST run's values - forever.
    # jamovi reuses the analysis object across option changes, so in the GUI every
    # plot option (bin colours, digits, test type, labels, bin width) was dead after
    # the first render. Invisible to the R wrapper, which builds a fresh instance per
    # call, which is why the whole existing suite passed with the bug in place.
    a <- run_analysis(quiet_data(), dep = "v", binfill = "skyblue", digits = 2)
    priv <- a$.__enclos_env__$private

    expect_equal(priv$.prepareAesthetics()$bin.args$fill, "skyblue")
    expect_equal(priv$.prepareOptions()$digits, 2)

    # simulate the user changing options in the GUI on the SAME analysis object
    binfill <- a$options$option("binfill"); binfill$value <- "red"
    digits  <- a$options$option("digits");  digits$value  <- 4
    priv$.run()

    expect_equal(priv$.prepareAesthetics()$bin.args$fill, "red")
    expect_equal(priv$.prepareOptions()$digits, 4)
})


test_that("a parametric test on non-normal data is flagged in the default configuration", {
    # Shapiro-Wilk used to be computed ONLY inside .generateClinicalInterpretation(), which
    # returns early unless showInterpretation is on - and that defaults to FALSE. So the
    # default configuration ran a one-sample t-test, reported it in the subtitle, and
    # surfaced no assumption check anywhere.
    set.seed(42)
    skewed <- data.frame(w = rlnorm(120, 1, 0.4))   # Shapiro-Wilk p = 3.4e-06
    a <- run_analysis(skewed, dep = "w", resultssubtitle = TRUE, typestatistics = "parametric")
    todo <- a$results$todo$content
    expect_match(todo, "not normally distributed")
    expect_match(todo, "Wilcoxon")   # the message must name the remedy

    # silent when the reported test makes no normality assumption
    b <- run_analysis(skewed, dep = "w", resultssubtitle = TRUE, typestatistics = "nonparametric")
    expect_false(grepl("not normally distributed", b$results$todo$content))

    # silent when no test is being reported at all
    d <- run_analysis(skewed, dep = "w", resultssubtitle = FALSE, typestatistics = "parametric")
    expect_false(grepl("not normally distributed", d$results$todo$content))

    # silent on a variable that passes
    normal <- data.frame(v = qnorm(seq(0.005, 0.995, length.out = 120), 5.2, 0.9))
    e <- run_analysis(normal, dep = "v", resultssubtitle = TRUE, typestatistics = "parametric")
    expect_false(grepl("not normally distributed", e$results$todo$content))
})

test_that("the analytic n is stated whenever rows are dropped as missing", {
    # ggstatsplot drops NAs silently, so the n behind the reported statistic was never
    # stated: 37 of 120 rows missing and nothing said the test ran on 83. The "Small
    # sample size" line only fires below n = 30, so the ordinary clinical case - a few
    # hundred rows with some missingness - reported nothing.
    d <- data.frame(v = c(rep(NA_real_, 37), seq(1, 83)))
    a <- run_analysis(d, dep = "v", resultssubtitle = TRUE)
    expect_match(a$results$todo$content, "n = 83 of 120")
    expect_match(a$results$todo$content, "30.8% missing", fixed = TRUE)

    # and stays quiet when the column is complete
    b <- run_analysis(data.frame(v = seq(1, 120)), dep = "v", resultssubtitle = TRUE)
    expect_false(grepl("were excluded", b$results$todo$content))
})

test_that("clinical warnings are ordered by severity, not by code position", {
    # These were emitted in generator order, so "the subtitle is testing against zero" -
    # which invalidates the headline statistic - could render below "Detected 3 extreme
    # outlier(s)".
    set.seed(1)
    d <- data.frame(w = c(rlnorm(100, 1, 0.4), 400, 500, 600))  # outliers + skew + all > 0
    a <- run_analysis(d, dep = "w", resultssubtitle = TRUE, typestatistics = "parametric")
    todo <- a$results$todo$content
    p_zero    <- regexpr("testing against zero", todo, fixed = TRUE)
    p_outlier <- regexpr("extreme outlier", todo, fixed = TRUE)
    expect_gt(p_zero, 0)
    expect_gt(p_outlier, 0)
    expect_lt(p_zero, p_outlier)
})
