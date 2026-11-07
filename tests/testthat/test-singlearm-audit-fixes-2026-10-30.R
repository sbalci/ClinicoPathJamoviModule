# Regressions from the 2026-10-30 jsurvival module audit (singlearm).
#
# - A landmark that excludes nobody still shifts every time; that went undisclosed.
# - setContent("") on a narrative panel is skipped by the jamovi client, so a run
#   that ends early left the previous run's text on screen.

sa_data <- function() {
    set.seed(11); n <- 120
    t_ev <- rexp(n, 0.03) + 10; t_c <- runif(n, 15, 90)   # everyone followed past 10
    data.frame(time = round(pmin(t_ev, t_c), 2), status = as.integer(t_ev <= t_c))
}
run_sa <- function(...) {
    args <- list(data = sa_data(), elapsedtime = "time", outcome = "status", outcomeLevel = "1",
                 dod = NULL, dooc = NULL, awd = NULL, awod = NULL)
    do.call(singlearm, utils::modifyList(args, list(...), keep.null = TRUE))
}

test_that("a landmark that excludes nobody is still disclosed", {
    r <- run_sa(uselandmark = TRUE, landmark = 3)
    expect_match(r$info$content, "no subject was excluded", fixed = TRUE)
    expect_match(r$info$content, "measured from the landmark", fixed = TRUE)
    r0 <- run_sa()
    expect_false(grepl("measured from the landmark", paste(r0$info$content), fixed = TRUE))
})

test_that("an emptied Preformatted narrative panel holds no literal markup", {
    r <- run_sa(cutp = "500, 600", showSummaries = TRUE)   # no time point inside follow-up
    expect_identical(r$survTableSummary$content, "")
})
