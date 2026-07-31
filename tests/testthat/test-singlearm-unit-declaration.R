# Regression test for the declared-unit sanity check in .definemytime()
# (R/singlearm.b.R).
#
# With pre-calculated elapsed time, "Time Type in Output" is a DECLARATION of the
# unit the column is already in -- nothing converts it and nothing used to check
# it. A column of days declared as "years" produced a table headed
# "1, 3, 5 year Survival" whose rows were read off days 1, 3 and 5 (100% survival
# throughout) and an info line reading "Median follow-up: 1393.0 years". The
# implausibility was computed and printed; it was simply never flagged.
#
# Warn-only, median-based (immune to a single 9999 sentinel), threshold 100 years.
# Adjacent-unit swaps (months read as years) are undetectable in principle and
# are deliberately NOT flagged.

days_cohort <- function(n = 60, seed = 3) {
    set.seed(seed)
    data.frame(time   = runif(n, 1000, 2000),
               status = factor(sample(c("Dead", "Alive"), n, TRUE)))
}

run_with_unit <- function(d, unit) {
    do.call(singlearm, list(
        data = d, elapsedtime = "time", tint = FALSE,
        outcome = "status", outcomeLevel = "Dead",
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
        timetypeoutput = unit))
}

test_that("a mis-declared time unit is flagged", {
    d <- days_cohort()   # median follow-up ~1402 days

    for (unit in c("months", "years")) {
        txt <- as.character(run_with_unit(d, unit)$warnings$content)
        expect_true(grepl("longer than a human lifetime", txt, fixed = TRUE),
                    info = paste("unit:", unit))
    }
})

test_that("a truthfully declared time unit is not flagged", {
    d <- days_cohort()   # 1402 days is 3.8 years -- entirely ordinary
    txt <- as.character(run_with_unit(d, "days")$warnings$content)
    expect_false(grepl("longer than a human lifetime", txt, fixed = TRUE))
})
