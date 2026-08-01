library(testthat)

# Numerical verification for `survival()`.
#
# These tests compare what the analysis PUTS IN ITS TABLES against values
# computed independently with the survival package. Asserting only that a table
# has rows (which this file used to do) passes just as happily when every number
# in it is wrong.

# Table notes are R6 `Note` objects held in an environment, keyed by note key.
table_notes <- function(table) {
    paste(vapply(as.list(table$notes), function(n) n$note, character(1)),
          collapse = " ")
}

make_verification_df <- function(n = 200, seed = 2024) {
    set.seed(seed)
    data.frame(
        time   = round(rexp(n, rate = 0.03) + 0.5, 2),
        status = rbinom(n, 1, 0.7),
        grp    = factor(sample(c("A", "B"), n, replace = TRUE))
    )
}

test_that("median survival matches survival::survfit", {
    df <- make_verification_df()

    res <- run_survival(
        data = df, elapsedtime = "time", outcome = "status",
        explanatory = "grp", cutp = "12, 36, 60"
    )

    ref <- summary(survival::survfit(
        survival::Surv(time, status) ~ grp, data = df))$table
    got <- res$medianTable$asDF

    expect_equal(nrow(got), nrow(ref))
    expect_equal(as.numeric(got$records), unname(ref[, "records"]))
    expect_equal(as.numeric(got$events),  unname(ref[, "events"]))
    expect_equal(as.numeric(got$median),  unname(ref[, "median"]), tolerance = 1e-6)
    expect_equal(as.numeric(got$x0_95lcl), unname(ref[, "0.95LCL"]), tolerance = 1e-6)
    expect_equal(as.numeric(got$x0_95ucl), unname(ref[, "0.95UCL"]), tolerance = 1e-6)
})

test_that("survival probabilities at cutpoints match summary.survfit", {
    df <- make_verification_df()

    res <- run_survival(
        data = df, elapsedtime = "time", outcome = "status",
        explanatory = "grp", cutp = "12, 36, 60"
    )

    ref <- summary(survival::survfit(survival::Surv(time, status) ~ grp, data = df),
                   times = c(12, 36, 60), extend = TRUE)
    got <- res$survTable$asDF

    expect_equal(nrow(got), length(ref$surv))
    expect_equal(as.numeric(got$surv),   ref$surv,   tolerance = 1e-8)
    expect_equal(as.numeric(got$lower),  ref$lower,  tolerance = 1e-8)
    expect_equal(as.numeric(got$upper),  ref$upper,  tolerance = 1e-8)
    expect_equal(as.numeric(got$n.risk), ref$n.risk)
})

test_that("Cox hazard ratio, CI and p-value match survival::coxph", {
    df <- make_verification_df()

    res <- run_survival(
        data = df, elapsedtime = "time", outcome = "status", explanatory = "grp"
    )

    ref <- summary(survival::coxph(survival::Surv(time, status) ~ grp, data = df))
    got <- res$coxTable$asDF

    # finalfit renders "HR (lower-upper, p=x)" on the non-reference row.
    hr_cell <- got$HR_univariable[got$HR_univariable != "-"]
    expect_length(hr_cell, 1L)

    parsed <- as.numeric(regmatches(hr_cell,
        gregexpr("[0-9]+\\.[0-9]+", hr_cell))[[1]])
    expect_equal(parsed[1], unname(ref$conf.int[1, "exp(coef)"]),  tolerance = 0.005)
    expect_equal(parsed[2], unname(ref$conf.int[1, "lower .95"]),  tolerance = 0.005)
    expect_equal(parsed[3], unname(ref$conf.int[1, "upper .95"]),  tolerance = 0.005)
    expect_equal(parsed[4], unname(ref$coefficients[1, 5]),        tolerance = 0.002)
})

test_that("RMST matches survfit rmean at the requested horizon", {
    df <- make_verification_df()

    res <- run_survival(
        data = df, elapsedtime = "time", outcome = "status", explanatory = "grp",
        rmst_analysis = TRUE, rmst_tau = 30
    )

    ref <- summary(survival::survfit(survival::Surv(time, status) ~ grp, data = df),
                   rmean = 30)$table
    got <- res$rmstTable$asDF

    expect_equal(as.numeric(got$rmst), unname(round(ref[, "rmean"], 2)), tolerance = 1e-8)
    expect_equal(as.numeric(got$se),   unname(round(ref[, "se(rmean)"], 2)), tolerance = 1e-8)
    expect_true(all(as.numeric(got$tau) == 30))
})

test_that("person-time incidence rate and exact Poisson CI match poisson.test", {
    df <- make_verification_df()

    res <- run_survival(
        data = df, elapsedtime = "time", outcome = "status", explanatory = "grp",
        person_time = TRUE, rate_multiplier = 100
    )

    got <- res$personTimeTable$asDF
    overall <- got[got$interval == "Overall (0-max)", ]
    expect_equal(nrow(overall), 1L)

    ev <- sum(df$status)
    pt <- sum(df$time)
    ref <- stats::poisson.test(ev, pt)

    expect_equal(as.numeric(overall$events), ev)
    expect_equal(as.numeric(overall$rate), round(ev / pt * 100, 2), tolerance = 1e-8)
    expect_equal(as.numeric(overall$rate_ci_lower),
                 round(ref$conf.int[1] * 100, 2), tolerance = 1e-8)
    expect_equal(as.numeric(overall$rate_ci_upper),
                 round(ref$conf.int[2] * 100, 2), tolerance = 1e-8)
})

test_that("pairwise log-rank p-values match survminer::pairwise_survdiff", {
    skip_if_not_installed("survminer")
    df <- make_verification_df()
    set.seed(9)
    df$grp3 <- factor(sample(c("A", "B", "C"), nrow(df), replace = TRUE))

    res <- run_survival(
        data = df, elapsedtime = "time", outcome = "status",
        explanatory = "grp3", pw = TRUE
    )

    ref <- survminer::pairwise_survdiff(
        survival::Surv(time, status) ~ grp3, data = df,
        p.adjust.method = "holm")$p.value
    got <- res$pairwiseTable$asDF

    expect_equal(nrow(got), sum(!is.na(ref)))
    for (i in seq_len(nrow(got)))
        expect_equal(as.numeric(got$value[i]),
                     ref[got$rowname[i], got$name[i]], tolerance = 1e-8)
})

# ---------------------------------------------------------------- regressions

test_that("survTable never reports survival beyond a group's observed follow-up", {
    # Regression: `summary(fit, times = ..., extend = TRUE)` carries the last
    # Kaplan-Meier estimate forward indefinitely, so a cutpoint past the end of
    # follow-up used to be reported as a survival probability -- with a
    # confidence interval -- computed from zero patients at risk.
    df <- make_verification_df()
    max_time <- max(df$time)

    res <- run_survival(
        data = df, elapsedtime = "time", outcome = "status",
        explanatory = "grp", cutp = paste0("12, ", ceiling(max_time) + 100)
    )

    got <- res$survTable$asDF
    expect_true(all(as.numeric(got$time) <= max_time))
    expect_true(all(as.numeric(got$n.risk) > 0))
    # the omission must be disclosed, not silent
    expect_match(table_notes(res$survTable),
                 "exceed the observed follow-up")
})

test_that("survTable drops an unsupported cutpoint only for the group lacking follow-up", {
    df <- make_verification_df()
    # Truncate group B's follow-up so 60 is supported in A but not in B.
    df$time[df$grp == "B"] <- pmin(df$time[df$grp == "B"], 40)

    res <- run_survival(
        data = df, elapsedtime = "time", outcome = "status",
        explanatory = "grp", cutp = "12, 60"
    )

    got <- res$survTable$asDF
    expect_true(any(got$strata == "A" & as.numeric(got$time) == 60))
    expect_false(any(got$strata == "B" & as.numeric(got$time) == 60))
})

test_that("non-numeric cutpoints are ignored instead of aborting the analysis", {
    # Regression: as.numeric("abc") is NA, summary.survfit() then raised
    # "times contains missing values" and took the ENTIRE analysis down with it,
    # median table and Cox output included, for one typo in a free-text box.
    df <- make_verification_df()

    expect_no_error({
        res <- run_survival(
            data = df, elapsedtime = "time", outcome = "status",
            explanatory = "grp", cutp = "abc"
        )
    })
    expect_gt(nrow(res$survTable$asDF), 0)
    expect_gt(nrow(res$medianTable$asDF), 0)
    expect_match(table_notes(res$survTable), "Ignored cutpoint")

    # A mix of valid and invalid tokens keeps the valid ones.
    res2 <- run_survival(
        data = df, elapsedtime = "time", outcome = "status",
        explanatory = "grp", cutp = "12, oops, 36"
    )
    expect_setequal(unique(as.numeric(res2$survTable$asDF$time)), c(12, 36))
})

test_that("cutpoints are de-duplicated, sorted, and negatives rejected", {
    df <- make_verification_df()

    res <- run_survival(
        data = df, elapsedtime = "time", outcome = "status",
        explanatory = "grp", cutp = "36, 12, 36, -5"
    )
    got <- res$survTable$asDF
    expect_setequal(unique(as.numeric(got$time)), c(12, 36))
    expect_match(table_notes(res$survTable), "Ignored cutpoint")
})

test_that("empty cutpoint string falls back to the documented default", {
    df <- make_verification_df()

    res <- run_survival(
        data = df, elapsedtime = "time", outcome = "status",
        explanatory = "grp", cutp = ""
    )
    expect_setequal(unique(as.numeric(res$survTable$asDF$time)), c(12, 36, 60))
})

test_that("edge cases: single-level factor and low event count stay graceful", {
    small_df <- data.frame(
        time   = c(10, 20, 30, 40, 50, 60, 70, 80),
        status = c(1, 0, 1, 0, 0, 0, 0, 0),
        grp    = factor(rep("GroupA", 8))
    )

    expect_no_error({
        res <- run_survival(
            data = small_df, elapsedtime = "time",
            outcome = "status", explanatory = "grp"
        )
    })
    # Descriptive output survives; model-based output is suppressed, not faked.
    expect_gt(nrow(res$medianTable$asDF), 0)
    expect_equal(nrow(res$coxTable$asDF), 0)
})
