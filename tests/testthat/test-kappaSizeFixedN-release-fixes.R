# Release fixes 2026-10-21 (development-ideas/kappaSizeFixedN-independent-review-2026-10-21.md).

fnr_run <- function(...) {
    args <- utils::modifyList(
        list(outcome = "2", kappa0 = 0.60, props = "0.20, 0.80", raters = "2", alpha = 0.05, n = 100L),
        list(...))
    suppressWarnings(do.call(ClinicoPath::kappaSizeFixedN, args))
}
fnr_flat <- function(x) gsub("\\s+", " ", x)

test_that("FN-01: the default view names the published figure when re-estimation lowers the bound", {
    # RD2012 design: headline 0.354 (re-estimated), kappaSize / Rotondi-Donner 0.355
    r <- fnr_run(outcome = "3", kappa0 = 0.44, props = "0.11, 0.70, 0.19", raters = "3", n = 171L)
    t1 <- fnr_flat(r$text1$content)
    expect_match(t1, "lower limit for kappa of 0.354.", fixed = TRUE)
    expect_match(t1, "as the published calculation and kappaSize do, gives 0.355;", fixed = TRUE)
    expect_match(t1, "re-estimates them", fixed = TRUE)
    # published figure >= 0.40, headline < 0.40: the amber verdict now comes with its reason
    r <- fnr_run(outcome = "4", kappa0 = 0.5, props = "0.05, 0.15, 0.3, 0.5", raters = "6", n = 50L)
    t1 <- fnr_flat(r$text1$content)
    expect_match(t1, "lower limit for kappa of 0.399.", fixed = TRUE)
    expect_match(t1, "kappaSize do, gives 0.401;", fixed = TRUE)
    # two raters, binary: no re-estimation, the headline stands alone
    expect_identical(fnr_run()$text1$content,
                     "A total of 100 subjects can produce an expected one-sided 95% lower limit for kappa of 0.419.")
})

test_that("FN-02: the plug-in warning no longer says balanced categories keep 0.95 coverage", {
    r <- fnr_run(outcome = "4", kappa0 = 0.6, props = "0.25, 0.25, 0.25, 0.25", raters = "4", n = 80L,
                 showExplanations = TRUE)
    m <- fnr_flat(gsub("<[^>]+>", " ", r$methodology$content))
    expect_false(grepl("stayed close to 0.95", m, fixed = TRUE))
    expect_false(grepl("about a fifth", m, fixed = TRUE))
    expect_match(m, "about 0.86 to 0.94 instead of 0.95", fixed = TRUE)
    expect_match(m, "four equally common categories and four raters", fixed = TRUE)
})
