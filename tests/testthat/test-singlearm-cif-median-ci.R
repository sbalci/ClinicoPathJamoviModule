# Regression test for the competing-risk median in R/singlearm.b.R.
#
# .medianSurv() deliberately writes NA to x0_95lcl / x0_95ucl in the
# competing-risk branch: a valid interval for a CIF quantile needs a confidence
# BAND, and the previous code substituted median * 0.8 / median * 1.2 and printed
# that as a 95% CI. The NA is correct. What was NOT correct is that
# .generateClinicalSummary() interpolated those NAs with sprintf("%.1f", ...),
# so the block labelled "Copy-ready for clinical reports" printed the literal
# string "(95% CI: NA-NA months)" -- and called a CIF median "Median survival".

make_cr_data <- function(n = 120, seed = 7) {
    set.seed(seed)
    tt <- rexp(n, 0.05)
    cc <- rexp(n, 0.03)
    oc <- factor(ifelse(tt <= cc, sample(c("DOD", "DOOC"), n, TRUE, c(0.7, 0.3)), "AWD"),
                 levels = c("AWD", "AWOD", "DOD", "DOOC"))
    data.frame(time = pmin(tt, cc), oc = oc)
}

run_cr <- function(d) {
    do.call(singlearm, list(
        data = d, elapsedtime = "time", tint = FALSE,
        outcome = "oc", outcomeLevel = NULL,
        multievent = TRUE, analysistype = "compete",
        dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
        timetypeoutput = "months", showSummaries = TRUE))
}

test_that("the competing-risk clinical summary never prints an NA confidence interval", {
    skip_if_not_installed("cmprsk")
    r <- run_cr(make_cr_data())
    txt <- as.character(r$clinicalSummary$content)

    expect_true(nzchar(txt))
    # Was: "Median survival was 19.2 months (95% CI: NA-NA months)"
    expect_false(grepl("NA-NA", txt, fixed = TRUE))
    expect_false(grepl("CI: NA", txt, fixed = TRUE))
    # A CIF median is the time cumulative incidence reaches 50%, not median survival.
    expect_true(grepl("Median time to event of interest", txt, fixed = TRUE))
})

test_that("the ordinary Kaplan-Meier summary still reports its confidence interval", {
    set.seed(11)
    n <- 150
    tt <- rexp(n, 0.08); cc <- rexp(n, 0.02)
    d <- data.frame(time = pmin(tt, cc),
                    oc = factor(ifelse(tt <= cc, "Dead", "Alive")))
    r <- do.call(singlearm, list(
        data = d, elapsedtime = "time", tint = FALSE,
        outcome = "oc", outcomeLevel = "Dead",
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
        timetypeoutput = "months", showSummaries = TRUE))
    txt <- as.character(r$clinicalSummary$content)

    # A bare Alive/Dead factor does not declare WHAT the event is, so the
    # summary must not call it survival (survival_utils.R gives this path the
    # estimand "Kaplan-Meier survival for the selected event", which
    # .estimandMeta() labels "Median event-free time"). The point of this test
    # is the interval, which must still be a real one.
    expect_true(grepl("Median event-free time was", txt, fixed = TRUE))
    expect_true(grepl("95% CI:", txt, fixed = TRUE))
    expect_match(txt, "95% CI: [0-9.]+-[0-9.]+ months")
    expect_false(grepl("NA-NA", txt, fixed = TRUE))
})

test_that("declaring overall survival makes the summary say overall survival", {
    set.seed(11)
    n <- 150
    tt <- rexp(n, 0.08); cc <- rexp(n, 0.02)
    d <- data.frame(time = pmin(tt, cc),
                    oc = factor(ifelse(tt <= cc, "DOD", "AWD"),
                                levels = c("AWD", "AWOD", "DOD", "DOOC")))
    r <- do.call(singlearm, list(
        data = d, elapsedtime = "time", tint = FALSE,
        outcome = "oc", outcomeLevel = NULL,
        multievent = TRUE, analysistype = "overall",
        dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
        timetypeoutput = "months", showSummaries = TRUE))
    txt <- as.character(r$clinicalSummary$content)

    expect_true(grepl("Median overall survival was", txt, fixed = TRUE))
    expect_match(txt, "95% CI: [0-9.]+-[0-9.]+ months")
})
