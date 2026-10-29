# Regressions from the 2026-10-29 jsurvival release check (survivalcont).
#
# - With findcut on, RMST ran twice and an unsupported tau produced two
#   "Unsupported RMST horizon" messages with different limits.
# - A negative / zero / non-numeric survival time point vanished without a word.
# - A time point beyond a cut-off group's follow-up showed "." cells with no reason.

sc_release_data <- function() {
    set.seed(42); n <- 300
    marker <- round(rnorm(n, 50, 12), 1)
    t_ev <- rexp(n, 0.02 * exp(ifelse(marker > 55, 0.9, 0))); t_c <- runif(n, 10, 120)
    data.frame(time = round(pmin(t_ev, t_c), 2), status = as.integer(t_ev <= t_c), marker = marker)
}
run_sc <- function(...) {
    args <- list(data = sc_release_data(), elapsedtime = "time", contexpl = "marker",
                 outcome = "status", outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL)
    do.call(survivalcont, utils::modifyList(args, list(...), keep.null = TRUE))
}
count_fixed <- function(pattern, x) lengths(regmatches(x, gregexpr(pattern, x, fixed = TRUE)))

test_that("an unsupported RMST horizon is reported once when findcut is on", {
    r <- run_sc(findcut = TRUE, rmst_analysis = TRUE, rmst_tau = 500)
    msgs <- paste(r$warnings$content, r$errors$content, r$strongWarnings$content)
    expect_equal(count_fixed("Unsupported RMST horizon", msgs), 1L)
})

test_that("ignored survival time points are reported, once, and the valid ones kept", {
    r <- run_sc(findcut = TRUE, multiple_cutoffs = TRUE, cutp = "-5, 12, abc, 24")
    expect_equal(count_fixed("Some survival time points were ignored", r$warnings$content), 1L)
    expect_match(r$warnings$content, "2 of the entries", fixed = TRUE)
    expect_setequal(unique(r$multipleSurvTable$asDF$time_point), c(12, 24))
})

test_that("a time point beyond a group's follow-up is blank and footnoted, not '.'", {
    r <- run_sc(multiple_cutoffs = TRUE, cutp = "12, 500")
    t <- r$multipleSurvTable
    beyond <- which(t$asDF$time_point == 500)
    expect_gt(length(beyond), 0)
    for (i in beyond) {
        expect_identical(t$getCell(rowNo = i, col = "survival_prob")$value, "")
        expect_match(t$getCell(rowNo = i, col = "survival_prob")$footnotes, "Beyond the observed follow-up")
    }
})
