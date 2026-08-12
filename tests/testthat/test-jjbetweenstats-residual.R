# Residual-review regression tests for jjbetweenstats.
#
# Two defects, both "the option/label says one thing, the figure does another":
#
#  1. With 3+ groups and typestatistics = "bayes", ggstatsplot 1.0.0 swallows an
#     upstream statsExpressions/performance failure and returns a plot whose
#     subtitle is NULL - a figure with NO statistics at all - while the Results
#     Summary went on announcing "Bayesian ANOVA" and told the reader to "See
#     the plot subtitle for the test statistic, p-value, and effect size".
#  2. `colorblindSafe` is applied by .applyTheme() (viridis scales) which the
#     ggpubr companion renderers never call, so the accessibility option was
#     inert on that panel: byte-identical PNG at both values.
#
# Both expectations were observed failing on the unfixed code first.

library(testthat)

jbr_3group <- function(seed = 1) {
    set.seed(seed)
    data.frame(
        y = as.integer(c(rnorm(80, 10, 2), rnorm(80, 12, 2), rnorm(80, 14, 2))),
        g = factor(rep(c("A", "B", "C"), each = 80))
    )
}
jbr_2group <- function(seed = 1) {
    set.seed(seed)
    data.frame(
        y = as.integer(c(rnorm(80, 10, 2), rnorm(80, 12, 2))),
        g = factor(rep(c("A", "B"), each = 80))
    )
}
jbr_run <- function(data, ...) {
    opts <- do.call(ClinicoPath:::jjbetweenstatsOptions$new,
                    utils::modifyList(list(dep = "y", group = "g"), list(...)))
    a <- ClinicoPath:::jjbetweenstatsClass$new(options = opts, data = data)
    a$init()
    a$.__enclos_env__$private$.run()
    a
}
jbr_text <- function(html) gsub("\\s+", " ", gsub("<[^>]+>", " ", html))
jbr_md5 <- function(a, method) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, 700, 550)
    on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
    try(suppressWarnings(a$.__enclos_env__$private[[method]](
        a$results$ggpubrPlot, ggtheme = ggplot2::theme_bw(), theme = NULL)),
        silent = TRUE)
    grDevices::dev.off(); on.exit()
    unname(tools::md5sum(f))
}


test_that("a Bayesian test that yields no subtitle is disclosed, not announced", {
    d <- jbr_3group()
    a <- jbr_run(d, typestatistics = "bayes", resultssubtitle = TRUE)
    p <- a$.__enclos_env__$private

    # The probe must agree with what ggstatsplot actually renders - it is only
    # trustworthy because it calls the very function ggbetweenstats calls.
    unavailable <- p$.bayesProbeFails(p$.prepareData(), "g", "y")
    rendered <- suppressWarnings(tryCatch(
        ggstatsplot::ggbetweenstats(data = d, x = g, y = y, type = "bayes")$labels$subtitle,
        error = function(e) NULL))
    expect_identical(unavailable, is.null(rendered))

    diag <- jbr_text(a$results$diagnostics$content)
    summ <- jbr_text(a$results$clinicalSummary$content)
    if (unavailable) {
        # user-visible text, not the flag
        expect_true(grepl("Bayesian test could not be computed", diag, fixed = TRUE))
        expect_true(grepl("no test statistic", tolower(summ), fixed = TRUE))
        expect_false(grepl("See the plot subtitle for the test statistic",
                           summ, fixed = TRUE))
    } else {
        expect_true(grepl("See the plot subtitle for the test statistic",
                          summ, fixed = TRUE))
    }
})


test_that("a Bayesian test that DOES compute is left alone", {
    # The two-group Bayesian t-test works; it must keep pointing at the subtitle
    # and must not pick up the new warning.
    a <- jbr_run(jbr_2group(), typestatistics = "bayes", resultssubtitle = TRUE)
    expect_false(a$.__enclos_env__$private$.bayesNoStatistic)
    expect_false(grepl("Bayesian test could not be computed",
                       jbr_text(a$results$diagnostics$content), fixed = TRUE))
    expect_true(grepl("See the plot subtitle for the test statistic",
                      jbr_text(a$results$clinicalSummary$content), fixed = TRUE))
})


test_that("colorblindSafe changes the ggpubr companion panel", {
    d <- jbr_3group()
    off <- jbr_md5(jbr_run(d, addGGPubrPlot = TRUE, colorblindSafe = FALSE), ".plotGGPubr")
    on  <- jbr_md5(jbr_run(d, addGGPubrPlot = TRUE, colorblindSafe = TRUE),  ".plotGGPubr")
    expect_false(identical(off, on))   # was byte-identical before the fix

    # ... with the same viridis colours the main figure uses, one per group
    a <- jbr_run(d, addGGPubrPlot = TRUE, colorblindSafe = TRUE)
    pa <- a$.__enclos_env__$private
    expect_identical(pa$.ggpubrPaletteFor(pa$.prepareData()),
                     grDevices::hcl.colors(3, palette = "viridis"))

    # and the ggpubr palette selector still wins when the option is off
    b <- jbr_run(d, addGGPubrPlot = TRUE, ggpubrPalette = "jco", colorblindSafe = FALSE)
    pb <- b$.__enclos_env__$private
    expect_identical(pb$.ggpubrPaletteFor(pb$.prepareData()), "jco")
})
