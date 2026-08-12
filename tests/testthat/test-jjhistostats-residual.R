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
    d <- quiet_data()

    # subtitle is overwritten by ggstatsplot whenever Statistical Results is on
    todo <- run_analysis(d, dep = "v", subtitle = "MY SUBTITLE",
                         resultssubtitle = TRUE)$results$todo$content
    expect_match(todo, "Subtitle was not used")

    # ... and is honoured (so unreported) when Statistical Results is off
    todo <- run_analysis(d, dep = "v", subtitle = "MY SUBTITLE",
                         resultssubtitle = FALSE)$results$todo$content
    expect_false(grepl("Subtitle was not used", todo, fixed = TRUE))

    # title/xlab are replaced per panel with more than one variable selected
    d2 <- data.frame(v = seq(-100, 100, length.out = 200),
                     w = seq(-50, 50, length.out = 200))
    todo <- run_analysis(d2, dep = c("v", "w"), title = "MY TITLE", xlab = "MY XLAB",
                         resultssubtitle = FALSE)$results$todo$content
    expect_match(todo, "Title was not used")
    expect_match(todo, "X-axis label was not used")

    todo <- run_analysis(d, dep = "v", title = "MY TITLE", xlab = "MY XLAB",
                         resultssubtitle = FALSE)$results$todo$content
    expect_false(grepl("was not used", todo, fixed = TRUE))
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
    expect_error(run_analysis(dg, dep = "v", grvar = "v"), "Split By")
})


test_that("a constant variable says the histogram panel will be empty", {
    todo <- run_analysis(data.frame(v = rep(50, 60)), dep = "v",
                         resultssubtitle = FALSE)$results$todo$content
    expect_match(todo, "constant values")
    expect_match(todo, "will be empty")
})
