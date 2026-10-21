# Release fixes 2026-10-21 (development-ideas/kappaSizePower-independent-review-2026-10-21.md).

kpr_run <- function(...) {
    args <- utils::modifyList(
        list(outcome = "3", kappa0 = 0.4, kappa1 = 0.6, props = "0.3, 0.3, 0.4", raters = "6",
             alpha = 0.05, power = 0.80),
        list(...))
    do.call(ClinicoPath::kappaSizePower, args)
}
kpr_cat <- function(r) grepl("Expected proportions treated as known.", r$notices$content, fixed = TRUE)

test_that("KSP-21-01: the 3-5 category note fires on the under-powered 3+ rater designs", {
    # Simulated power at the published N for a stated 0.80 (review block F-CAT/F-X, R = 2000): the old
    # projection trigger fired in 1 of these 8 (only .1/.1/.8 with 6 raters).
    short <- list(
        list(raters = "6", props = "0.3, 0.3, 0.4"),                  # N 30, 0.6845 (ADE Table IV)
        list(raters = "6", props = "0.2, 0.3, 0.5"),                  # 0.672
        list(raters = "5", props = "0.1, 0.1, 0.2, 0.6", outcome = "4"),  # 0.670
        list(raters = "4", props = "0.1, 0.3, 0.6"),                  # 0.707
        list(raters = "4", props = "0.1, 0.1, 0.8"),                  # 0.7205
        list(raters = "3", props = "0.1, 0.1, 0.8"),                  # 0.7245
        list(raters = "4", props = "0.3, 0.3, 0.4"),                  # 0.7375
        list(raters = "6", props = "0.1, 0.1, 0.8"))                  # 0.636
    for (d in short) {
        r <- do.call(kpr_run, d)
        expect_true(kpr_cat(r), info = paste(d, collapse = " "))
        expect_match(gsub("\\s+", " ", r$notices$content), "below 0.75 in 8 of 12 designs", fixed = TRUE)
    }
    # two raters reached the target (0.785, 0.805): no note
    expect_false(kpr_cat(kpr_run(raters = "2", props = "0.1, 0.3, 0.6")))
    expect_false(kpr_cat(kpr_run(raters = "2", props = "0.05, 0.15, 0.8")))
    # binary designs keep the simulation, never this note
    expect_false(kpr_cat(kpr_run(outcome = "2", props = "0.3", raters = "6")))
})

test_that("KSP-21-01: the Methodology no longer claims the note flags only large losses", {
    m <- gsub("\\s+", " ", kpr_run()$notices$content)
    expect_false(grepl("a note flags designs where the loss is likely to be large", m, fixed = TRUE))
    expect_match(m, "with three to five categories and three or more raters", fixed = TRUE)
    t2 <- gsub("\\s+", " ", kpr_run()$text2$content)
    expect_match(t2, "this sample size can fall well short of power 0.80", fixed = TRUE)
})
