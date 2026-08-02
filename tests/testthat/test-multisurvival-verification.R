library(testthat)

# Numerical verification for `multisurvival()`.
#
# These tests compare what the analysis PUTS IN ITS TABLES against values
# computed independently with survival / riskRegression. The previous version of
# this file asserted only `!is.null(res$text)` and `!is.null(res$riskScoreTable)`
# under the heading "Numerical verification" -- which passes just as happily when
# every number in the output is wrong.

table_notes <- function(table) {
    paste(vapply(as.list(table$notes), function(n) n$note, character(1)),
          collapse = " ")
}

msv_df <- function(n = 220, seed = 77) {
    set.seed(seed)
    data.frame(
        time   = round(rexp(n, 0.025) + 1, 2),
        status = rbinom(n, 1, 0.68),
        age    = round(rnorm(n, 62, 11), 1),
        stage  = factor(sample(c("I", "II", "III"), n, TRUE)),
        trt    = factor(sample(c("A", "B"), n, TRUE)),
        site   = factor(sample(c("S1", "S2"), n, TRUE))
    )
}

# multisurvival renders the Cox table as HTML; pull the HR cells back out.
msv_hr_cells <- function(res) {
    txt <- paste(as.character(res$text$content), collapse = " ")
    regmatches(txt, gregexpr("[0-9]+\\.[0-9]{2} \\([0-9.]+-[0-9.]+, p=[0-9.]+\\)", txt))[[1]]
}
msv_nums <- function(cell) as.numeric(regmatches(cell, gregexpr("[0-9]+\\.[0-9]+", cell))[[1]])

test_that("multivariable Cox HRs, CIs and p-values match survival::coxph", {
    df <- msv_df()
    res <- .run_multisurvival(
        data = df, elapsedtime = "time", outcome = "status",
        explanatory = c("stage", "trt"), contexpl = "age")

    ref <- summary(survival::coxph(
        survival::Surv(time, status) ~ stage + trt + age, data = df))

    cells <- msv_hr_cells(res)
    # 3 covariate contrasts (stageII, stageIII, trtB) + age, each printed for
    # the univariable and the multivariable column.
    expect_gte(length(cells), 4)

    # The multivariable column is the last HR cell of each variable block; match
    # on value instead of position so a layout change cannot silently pass.
    got <- do.call(rbind, lapply(cells, msv_nums))
    for (i in seq_len(nrow(ref$conf.int))) {
        want <- unname(round(ref$conf.int[i, c("exp(coef)", "lower .95", "upper .95")], 2))
        hit <- which(abs(got[, 1] - want[1]) < 0.006 &
                     abs(got[, 2] - want[2]) < 0.006 &
                     abs(got[, 3] - want[3]) < 0.006)
        expect_gt(length(hit), 0,
                  label = paste("multivariable HR row", rownames(ref$conf.int)[i]))
    }
})

test_that("survival metrics match survival::concordance and riskRegression::Score", {
    skip_if_not_installed("riskRegression")
    df <- msv_df()
    res <- .run_multisurvival(
        data = df, elapsedtime = "time", outcome = "status",
        explanatory = c("stage", "trt"), contexpl = "age",
        show_survmetrics = TRUE, survmetrics_timepoints = "12, 24, 36")

    got <- res$survMetricsTable$asDF
    fit <- survival::coxph(survival::Surv(time, status) ~ stage + trt + age,
                           data = df, x = TRUE, y = TRUE)

    # Harrell's C and its Wald CI
    cidx <- survival::concordance(fit)
    row_c <- got[grepl("Concordance", got$metric), ]
    expect_equal(as.numeric(row_c$value), unname(cidx$concordance), tolerance = 1e-8)
    se <- sqrt(cidx$var)
    expect_equal(as.numeric(row_c$ci_lower), unname(cidx$concordance - 1.96 * se), tolerance = 1e-6)
    expect_equal(as.numeric(row_c$ci_upper), unname(cidx$concordance + 1.96 * se), tolerance = 1e-6)

    # Score() resolves the response type by name, so `survival` must be attached
    # rather than reached through `survival::`.
    sc <- suppressWarnings(withr::with_package("survival", riskRegression::Score(
        list(cox = fit), formula = Surv(time, status) ~ 1, data = df,
        times = c(12, 24, 36), metrics = c("brier", "auc"),
        summary = NULL, conf.int = FALSE, se.fit = FALSE)))
    brier <- as.data.frame(sc$Brier$score)
    brier <- brier[brier$model == "cox", ]
    auc <- as.data.frame(sc$AUC$score)

    for (tp in c(12, 24, 36)) {
        b <- got[got$metric == sprintf("Brier score (t = %d months)", tp), ]
        a <- got[got$metric == sprintf("Time-dependent AUC (t = %d months)", tp), ]
        expect_equal(as.numeric(b$value), brier$Brier[brier$times == tp], tolerance = 1e-8)
        expect_equal(as.numeric(a$value), auc$AUC[auc$times == tp], tolerance = 1e-8)
    }
})

test_that("adjusted survival curves reproduce the g-formula standardisation", {
    df <- msv_df()
    res <- .run_multisurvival(
        data = df, elapsedtime = "time", outcome = "status",
        explanatory = c("stage", "trt"), contexpl = "age",
        ac = TRUE, adjexplanatory = "trt", ac_summary = TRUE,
        ac_method = "average", cutp = "12, 36, 60")

    fit <- survival::coxph(survival::Surv(time, status) ~ stage + trt + age, data = df)
    standardised <- function(level) {
        nd <- df; nd$trt <- factor(level, levels = levels(df$trt))
        sf <- survival::survfit(fit, newdata = nd)
        list(time = sf$time, surv = rowMeans(sf$surv))
    }
    at <- function(g, t) g$surv[max(which(g$time <= t))]

    got <- res$adjustedSurvTable$asDF
    pc <- function(x) as.numeric(sub("%", "", x, fixed = TRUE)) / 100
    for (lv in c("A", "B")) {
        g <- standardised(lv)
        for (tp in c(12, 36, 60)) {
            cell <- got[got$strata == lv & as.numeric(got$time) == tp, "surv"]
            # The cell is a percentage rounded to one decimal, so compare the
            # reference at that same displayed precision.
            expect_equal(pc(cell), round(at(g, tp), 3), tolerance = 1e-8,
                         label = paste("adjusted S(", tp, ") for", lv))
        }
    }
    # The default method has no closed-form interval; it must SAY so rather than
    # leaving the CI columns silently blank.
    expect_match(table_notes(res$adjustedSurvTable), "g-computation")
    expect_match(table_notes(res$adjustedSurvTable), "left blank")
})

test_that("Fine-Gray subdistribution HRs match survival::finegray + coxph", {
    df <- msv_df()
    set.seed(5)
    df$cr <- factor(sample(c("Alive", "DOD", "DOOC"), nrow(df), TRUE,
                           prob = c(.45, .35, .20)))

    res <- multisurvival(
        data = df, elapsedtime = "time", outcome = "cr", outcomeLevel = NULL,
        multievent = TRUE, analysistype = "compete",
        dod = "DOD", dooc = "DOOC", awd = NULL, awod = "Alive",
        explanatory = "trt", contexpl = "age")

    st <- ifelse(df$cr == "DOD", 1, ifelse(df$cr == "DOOC", 2, 0))
    fgd <- data.frame(time = df$time, st = st, trt = df$trt, age = df$age)
    fg <- survival::finegray(
        survival::Surv(time, factor(st, 0:2, c("censor", "dod", "dooc"))) ~ .,
        data = fgd, etype = "dod")
    ref <- summary(survival::coxph(
        survival::Surv(fgstart, fgstop, fgstatus) ~ trt + age,
        weights = fgwt, data = fg))$conf.int

    txt <- paste(as.character(res$text$content), collapse = " ")
    expect_match(txt, "Fine-Gray")
    # sHR for trt: B vs A
    got <- as.numeric(regmatches(
        txt, gregexpr("[0-9]\\.[0-9]{2} \\([0-9.]+-[0-9.]+\\)", txt))[[1]][1] |>
        (\(s) regmatches(s, gregexpr("[0-9]+\\.[0-9]+", s))[[1]])())
    expect_equal(got[1], unname(round(ref["trtB", "exp(coef)"], 2)), tolerance = 0.011)
})

test_that("person-time rates and exact Poisson CIs match poisson.test", {
    df <- msv_df()
    res <- .run_multisurvival(
        data = df, elapsedtime = "time", outcome = "status",
        explanatory = "trt", contexpl = "age",
        person_time = TRUE, rate_multiplier = 100, time_intervals = "12, 36, 60")

    got <- res$personTimeTable$asDF
    overall <- got[got$interval == "Overall (0-max)", ]
    expect_equal(nrow(overall), 1L)

    ev <- sum(df$status); pt <- sum(df$time)
    ref <- stats::poisson.test(ev, pt)
    expect_equal(as.numeric(overall$events), ev)
    expect_equal(as.numeric(overall$person_time), round(pt, 2), tolerance = 1e-8)
    expect_equal(as.numeric(overall$rate), round(ev / pt * 100, 2), tolerance = 1e-8)
    expect_equal(as.numeric(overall$rate_ci_lower), round(ref$conf.int[1] * 100, 2), tolerance = 1e-8)
    expect_equal(as.numeric(overall$rate_ci_upper), round(ref$conf.int[2] * 100, 2), tolerance = 1e-8)
})

test_that("optimism-corrected C-index equals apparent minus mean bootstrap optimism", {
    df <- msv_df()
    res <- .run_multisurvival(
        data = df, elapsedtime = "time", outcome = "status",
        explanatory = c("stage", "trt"), contexpl = "age",
        ci_optimism = TRUE, ci_optimism_boot = 60)

    got <- res$cindexValidation$asDF
    fit <- survival::coxph(survival::Surv(time, status) ~ stage + trt + age, data = df)

    apparent <- as.numeric(got$value[grepl("Apparent", got$metric)])
    optimism <- as.numeric(got$value[grepl("Optimism \\(", got$metric)])
    corrected <- as.numeric(got$value[grepl("corrected", got$metric)])

    expect_equal(apparent, unname(survival::concordance(fit)$concordance), tolerance = 1e-8)
    expect_equal(corrected, apparent - optimism, tolerance = 1e-8)
    # Optimism must be a real (positive) penalty, and the correction must bite.
    expect_gt(optimism, 0)
    expect_lt(corrected, apparent)
})

test_that("stratified Cox matches coxph with strata()", {
    df <- msv_df()
    res <- .run_multisurvival(
        data = df, elapsedtime = "time", outcome = "status",
        explanatory = "trt", contexpl = "age",
        use_stratify = TRUE, stratvar = "site")

    ref <- summary(survival::coxph(
        survival::Surv(time, status) ~ trt + age + survival::strata(site),
        data = df))$conf.int

    got <- do.call(rbind, lapply(msv_hr_cells(res), msv_nums))
    want <- unname(round(ref["trtB", c("exp(coef)", "lower .95", "upper .95")], 2))
    expect_gt(sum(abs(got[, 1] - want[1]) < 0.006 &
                  abs(got[, 2] - want[2]) < 0.006 &
                  abs(got[, 3] - want[3]) < 0.006), 0)
})

# ---------------------------------------------------------------- regressions

test_that("a multi-df interaction reports a JOINT test, not only per-coefficient rows", {
    # Regression: the table shows one row per interaction COEFFICIENT, each a
    # 1-df Wald test. For a 3-level x 2-level interaction that read as borderline
    # (p = 0.077 on one row) while the joint 2-df test over the same model is
    # p = 0.154. The joint test is now reported alongside the rows.
    df <- msv_df()
    res <- .run_multisurvival(
        data = df, elapsedtime = "time", outcome = "status",
        explanatory = c("stage", "trt"), contexpl = "age",
        interactions = list(list("stage", "trt")))

    expect_equal(nrow(res$interactionTest$asDF), 2L)

    m0 <- survival::coxph(survival::Surv(time, status) ~ stage + trt + age, data = df)
    m1 <- survival::coxph(survival::Surv(time, status) ~ stage + trt + age + stage:trt, data = df)
    lrt_p <- anova(m0, m1)[["Pr(>|Chi|)"]][2]

    notes <- table_notes(res$interactionTest)
    expect_match(notes, "Joint test of effect modification")
    expect_match(notes, "df = 2")
    # The joint Wald test must agree with the LRT to the displayed precision.
    shown <- as.numeric(sub(".*stage:trt: chi-square = [0-9.]+, df = 2, p = ([0-9]+\\.[0-9]+).*",
                            "\\1", notes))
    expect_equal(shown, round(lrt_p, 3), tolerance = 0.004)
})

test_that("a single-df interaction gets no redundant joint-test note", {
    # With one interaction coefficient the joint test IS the row already shown;
    # emitting it again would be noise.
    df <- msv_df()
    res <- .run_multisurvival(
        data = df, elapsedtime = "time", outcome = "status",
        explanatory = c("trt", "site"), contexpl = "age",
        interactions = list(list("trt", "site")))

    expect_equal(nrow(res$interactionTest$asDF), 1L)
    expect_false(grepl("Joint test of effect modification",
                       table_notes(res$interactionTest)))
})

test_that("person-time depends on the covariate set and is rebuilt, not appended", {
    # Regression: person-time is computed on the complete-case set over ALL
    # selected variables, so adding a covariate with missing values changes the
    # denominator -- but `contexpl` was missing from personTimeTable's clearWith
    # and the table never cleared its rows, so old rates could survive a re-run.
    set.seed(3); n <- 200
    df <- data.frame(
        time = round(rexp(n, 0.025) + 1, 2), status = rbinom(n, 1, 0.7),
        trt = factor(sample(c("A", "B"), n, TRUE)),
        biomarker = round(rnorm(n, 5, 1), 2))
    df$biomarker[sample(n, 60)] <- NA

    bare <- .run_multisurvival(data = df, elapsedtime = "time", outcome = "status",
                               explanatory = "trt", person_time = TRUE)
    with_cov <- .run_multisurvival(data = df, elapsedtime = "time", outcome = "status",
                                   explanatory = "trt", contexpl = "biomarker",
                                   person_time = TRUE)

    a <- bare$personTimeTable$asDF
    b <- with_cov$personTimeTable$asDF
    ao <- a[a$interval == "Overall (0-max)", ]
    bo <- b[b$interval == "Overall (0-max)", ]

    # The covariate genuinely changes the risk set ...
    expect_gt(as.numeric(ao$events), as.numeric(bo$events))
    # ... and each run reports exactly one overall row, never a stack of them.
    expect_equal(nrow(ao), 1L)
    expect_equal(nrow(bo), 1L)
    expect_equal(sum(b$interval == "Overall (0-max)"), 1L)

    # complete-case totals must match a hand computation
    cc <- df[!is.na(df$biomarker), ]
    expect_equal(as.numeric(bo$events), sum(cc$status))
    expect_equal(as.numeric(bo$person_time), round(sum(cc$time), 2), tolerance = 1e-8)
})
