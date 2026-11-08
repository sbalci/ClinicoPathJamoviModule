# Regression tests for the 2026-10-30 release review of survival.
# Reports: development-ideas/survival-independent-review-2026-10-30.md and the
# cross-analysis summary. Each block failed before the fix.
library(testthat)
suppressMessages(library(survival))

.rr_run <- function(data, ...) {
    a <- list(data = data, outcomeLevel = NULL, dod = NULL, dooc = NULL, awd = NULL, awod = NULL)
    u <- list(...)
    a[names(u)] <- u
    suppressWarnings(suppressMessages(do.call(ClinicoPath::survival, a)))
}
.rr_ss <- function(d, ...) .rr_run(d, elapsedtime = "time", outcome = "ev", outcomeLevel = "Dead",
                                   explanatory = "grp", ...)
.rr_notes <- function(tb) {
    n <- tb$notes
    if (length(n)) paste(vapply(n, function(x) x$note, ""), collapse = " | ") else ""
}
.rr_txt <- function(x) if (inherits(x, "Table")) x$asString() else gsub("<[^>]+>", " ", paste(x$content, collapse = " "))
.rr_data <- function(n = 300, seed = 1) {
    set.seed(seed)
    d <- data.frame(id = seq_len(n), grp = factor(sample(c("A", "B", "C"), n, TRUE)),
                    age = round(rnorm(n, 60, 10)))
    rate <- c(A = .02, B = .04, C = .08)[as.character(d$grp)]
    tt <- rexp(n, rate)
    cc <- runif(n, 10, 100)
    d$time <- pmin(tt, cc)
    d$ev <- factor(ifelse(tt <= cc, "Dead", "Alive"))
    d$sex <- factor(sample(c("F", "M"), n, TRUE))
    d
}

test_that("adjusted curves are directly standardized, and say so", {
    set.seed(2)
    n <- 1000
    a <- data.frame(grp = factor(rep(c("Young", "Old"), each = n / 2), levels = c("Young", "Old")))
    a$age <- round(ifelse(a$grp == "Young", rnorm(n, 50, 5), rnorm(n, 75, 5)))
    tt <- rexp(n, .01 * exp(.08 * (a$age - 60)))
    cc <- runif(n, 20, 120)
    a$time <- pmin(tt, cc)
    a$ev <- factor(ifelse(tt <= cc, "Dead", "Alive"))
    fit <- coxph(Surv(time, ev == "Dead") ~ grp + age, a)
    H <- basehaz(fit, centered = FALSE)
    H60 <- max(H$hazard[H$time <= 60])
    b <- coef(fit)
    oracle <- function(g) mean(exp(-H60 * exp(b[["grpOld"]] * g + b[["age"]] * a$age)))

    r <- .rr_ss(a, age_adjustment = TRUE, age_variable = "age", adjusted_curves = TRUE)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    r$adjustedCurvesPlot$saveAs(tempfile(fileext = ".png"))
    p <- ggplot2::last_plot()
    at <- function(g) {
        s <- p$data[as.character(p$data$variable) == g & p$data$time <= 60, ]
        s$surv[which.max(s$time)]
    }
    expect_lt(abs(at("Young") - oracle(0)), .02)
    expect_lt(abs(at("Old") - oracle(1)), .02)
    expect_match(p$labels$title, "Standardized")
    expect_match(p$labels$subtitle, "Direct standardization")
})

test_that("an ordered explanatory gives one labelled HR per level", {
    d <- .rr_data()
    d$grp <- factor(d$grp, ordered = TRUE)
    ct <- .rr_ss(d)$coxTable$asDF
    u <- d
    u$grp <- factor(u$grp, ordered = FALSE)
    expect_equal(ct$Levels, c("A", "B", "C"))
    expect_equal(ct$HR_univariable, .rr_ss(u)$coxTable$asDF$HR_univariable)
})

test_that("a level emptied by exclusions is dropped and the new reference is named", {
    d <- .rr_data()
    d$time[d$grp == "A"] <- NA
    r <- .rr_ss(d, showSummaries = TRUE)
    ct <- r$coxTable$asDF
    expect_false("A" %in% ct$Levels)
    expect_false(any(grepl("NA (NA", ct$HR_univariable, fixed = TRUE)))
    expect_match(.rr_notes(r$coxTable), "relative to B")
    expect_false(grepl("reference group (A)", paste(r$coxSummary$content, collapse = " "), fixed = TRUE))

    q <- d[!is.na(d$time), ]
    truth <- summary(coxph(Surv(time, ev == "Dead") ~ droplevels(grp), q))$conf.int[1, 1]
    expect_equal(as.numeric(sub(" .*", "", ct$HR_univariable[ct$Levels == "C"])), truth, tolerance = .01)
})

test_that("the landmark keeps T > L only and every time-based table says so", {
    d <- .rr_data()
    d$time[1:5] <- 12
    d$ev[1:5] <- "Dead"
    r <- .rr_ss(d, uselandmark = TRUE, landmark = 12, cutp = "12,24", showSummaries = TRUE,
                rmst_analysis = TRUE, person_time = TRUE)
    expect_equal(sum(r$medianTable$asDF$records), sum(d$time > 12))
    for (tb in list(r$medianTable, r$survTable, r$rmstTable, r$personTimeTable))
        expect_match(.rr_notes(tb), "Times are measured from the landmark at 12 months", fixed = TRUE)
    expect_match(.rr_txt(r$copyReadySentencesExplanation), "event-free at the landmark", fixed = TRUE)
    expect_match(paste(r$survTableSummary$content, collapse = " "), "event-free at the landmark", fixed = TRUE)
})

test_that("a negative landmark is refused", {
    d <- .rr_data()
    r <- tryCatch(.rr_ss(d, uselandmark = TRUE, landmark = -12), error = function(e) NULL)
    if (!is.null(r)) expect_equal(r$medianTable$rowCount, 0)
    else succeed()
})

test_that("numeric date columns other than epoch seconds are refused", {
    d <- .rr_data()
    d$dx <- 18000 + d$id
    d$fu <- d$dx + d$time * 30
    r <- tryCatch(.rr_run(d, tint = TRUE, dxdate = "dx", fudate = "fu", outcome = "ev",
                          outcomeLevel = "Dead", explanatory = "grp"),
                  error = function(e) e)
    if (inherits(r, "error")) expect_match(conditionMessage(r), "cannot be read as dates", fixed = TRUE)
    else {
        expect_equal(r$medianTable$rowCount, 0)
        expect_match(r$asString(), "cannot be read as dates", fixed = TRUE)
    }
})

test_that("placeholder dates are refused and unreadable dates reported", {
    d <- .rr_data()
    d$dx <- as.character(as.Date("2015-01-01") + d$id)
    d$fu <- as.character(as.Date(d$dx) + round(d$time * 30))
    bad <- d
    bad$fu[1:3] <- "2199-01-01"
    r <- tryCatch(.rr_run(bad, tint = TRUE, dxdate = "dx", fudate = "fu", outcome = "ev",
                          outcomeLevel = "Dead", explanatory = "grp", timetypedata = "ymd"),
                  error = function(e) e)
    if (inherits(r, "error")) expect_match(conditionMessage(r), "plausible range", fixed = TRUE)
    else expect_match(r$asString(), "plausible range", fixed = TRUE)

    d$fu[1:10] <- "not a date"
    r <- .rr_run(d, tint = TRUE, dxdate = "dx", fudate = "fu", outcome = "ev", outcomeLevel = "Dead",
                 explanatory = "grp", timetypedata = "ymd")
    expect_match(.rr_txt(r$warnings), "10 of 600 dates could not be read", fixed = TRUE)
})

test_that("survival table: interval events named, running total in the sentence, zero kept", {
    d <- .rr_data()
    r <- .rr_ss(d, cutp = "12,24,36", showSummaries = TRUE)
    st <- r$survTable$asDF
    A36 <- st[st$strata == "A" & st$time == 36, ]
    between <- sum(d$grp == "A" & d$ev == "Dead" & d$time > 24 & d$time <= 36)
    total <- sum(d$grp == "A" & d$ev == "Dead" & d$time <= 36)
    expect_equal(A36$n.event, between)
    expect_match(paste(r$survTableSummary$content, collapse = " "),
                 paste0(between, " events occurred since the previous time point, ", total, " in total"))
    expect_match(r$survTable$title, "Selected Time Points")

    x <- data.frame(time = c(1:10, 1:10), ev = factor(c(rep("Dead", 10), rep(c("Dead", "Alive"), 5))),
                    grp = factor(rep(c("A", "B"), each = 10)))
    x$time[x$grp == "B"] <- x$time[x$grp == "B"] + 20
    st <- .rr_ss(x, cutp = "15")$survTable$asDF
    expect_equal(as.numeric(st$surv[st$strata == "A"]), 0)
})

test_that("fractional time points are not rounded", {
    r <- .rr_ss(.rr_data(), cutp = "1.5,2.5,6.5")
    lines <- strsplit(r$survTable$asString(), "\n")[[1]]
    shown <- vapply(strsplit(trimws(grep("^ +A ", lines, value = TRUE)), " +"), function(x) x[2], "")
    expect_identical(shown, c("1.5", "2.5", "6.5"))
    big <- .rr_data()
    big$time <- big$time * 1e5
    expect_identical(unique(.rr_ss(big, cutp = "100000")$survTable$asDF$time), "100000")
})

test_that("pre-calculated time discloses that no conversion happens", {
    expect_match(.rr_notes(.rr_ss(.rr_data())$medianTable), "no unit conversion is performed", fixed = TRUE)
})

test_that("survival plots name their groups", {
    src <- paste(deparse(ClinicoPath:::survivalClass$private_methods$.plot), collapse = " ")
    expect_false(grepl("legend = \"none\"", src, fixed = TRUE))
    expect_true(grepl("legend.labs", src, fixed = TRUE))
    d <- .rr_data()
    r <- .rr_ss(d, sc = TRUE)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    expect_no_error(r$plot$saveAs(tempfile(fileext = ".png")))
})

test_that("the age interaction verdict is the joint test, and keeps the strata", {
    set.seed(5)
    n <- 400
    e <- data.frame(g = factor(sample(LETTERS[1:4], n, TRUE)), age = rnorm(n, 60, 10))
    tt <- rexp(n, .03 * exp(.03 * (e$age - 60) + (e$g == "B") * .04 * (e$age - 60)))
    cc <- runif(n, 5, 80)
    e$time <- pmin(tt, cc)
    e$ev <- factor(ifelse(tt <= cc, "D", "A"))
    e$s <- factor(sample(c("x", "y"), n, TRUE))
    run <- function(dat, ...) .rr_run(dat, elapsedtime = "time", outcome = "ev", outcomeLevel = "D",
                                      explanatory = "g", age_adjustment = TRUE, age_variable = "age",
                                      age_interaction = TRUE, ...)
    verdicts <- vapply(LETTERS[1:4], function(ref) {
        e2 <- e
        e2$g <- relevel(e2$g, ref)
        grepl("Significant age interaction", run(e2)$ageAdjustedInterpretation$content)
    }, TRUE)
    p <- anova(coxph(Surv(time, ev == "D") ~ g + age, e), coxph(Surv(time, ev == "D") ~ g * age, e))[2, 4]
    expect_true(all(verdicts == (p < .05)))

    r <- run(e, stratified_cox = TRUE, strata_variable = "s")
    expect_equal(r$ageInteractionTable$asDF$coef[1],
                 unname(coef(coxph(Surv(time, ev == "D") ~ g * age + strata(s), e))[1]), tolerance = 1e-6)
    expect_match(.rr_notes(r$ageInteractionTable), "age 0", fixed = TRUE)
    expect_match(.rr_notes(r$ageInteractionTable), "Joint likelihood-ratio", fixed = TRUE)
})

.rr_cr_data <- function() {
    set.seed(3)
    n <- 200
    c1 <- data.frame(g = factor(sample(c("X", "Y"), n, TRUE)), t = rexp(n, .03))
    c1$st <- factor(sample(c("DOD", "DOC", "AWD", "NED"), n, TRUE, c(.4, .3, .15, .15)))
    c1$st[c1$g == "Y" & c1$st == "DOD"] <- "DOC"
    c1
}
.rr_cr <- function(dat, ...) .rr_run(dat, elapsedtime = "t", outcome = "st", explanatory = "g",
                                     multievent = TRUE, analysistype = "compete", dod = "DOD",
                                     dooc = "DOC", awd = "AWD", awod = "NED", ...)

test_that("competing risks: no median survival text, and the estimands are named", {
    r <- .rr_cr(.rr_cr_data(), showSummaries = TRUE)
    expect_false(grepl("Median survival", .rr_txt(r$copyReadySentencesExplanation)))
    expect_false(grepl("median survival", paste(r$medianSummary$content, collapse = " ")))
    msg <- .rr_txt(r$infoMessages)
    expect_match(msg, "cause-specific hazard, the event rate", fixed = TRUE)
    expect_match(msg, "subdistribution hazard", fixed = TRUE)
    expect_match(msg, "censors competing events", fixed = TRUE)
})

test_that("competing risks: no event of interest still gives a table", {
    d <- .rr_cr_data()
    all_censored <- d
    all_censored$st <- factor(rep(c("AWD", "NED"), length.out = nrow(d)), levels = levels(d$st))
    expect_equal(.rr_cr(all_censored)$medianTable$rowCount, 2)
    none <- d
    none$st[none$st == "DOD"] <- "DOC"
    r <- .rr_cr(none)
    expect_equal(r$medianTable$rowCount, 2)
    expect_match(.rr_notes(r$medianTable), "No event of interest", fixed = TRUE)
})

test_that("the saved competing outcome covers every row and round-trips (summary F15)", {
    d <- .rr_cr_data()
    d$t[1:5] <- NA
    a <- ClinicoPath:::survivalClass$new(
        options = ClinicoPath:::survivalOptions$new(
            elapsedtime = "t", outcome = "st", explanatory = "g", multievent = TRUE,
            analysistype = "compete", dod = "DOD", dooc = "DOC", awd = "AWD", awod = "NED",
            uselandmark = TRUE, landmark = 5, outcomeredefined = TRUE),
        data = d)
    a$init()
    suppressWarnings(a$run())
    saved <- a$results$outcomeredefined$.__enclos_env__$private$.values[[1]]
    expect_s3_class(saved, "factor")
    expect_identical(levels(saved), c("Censored", "Event", "Competing"))
    expect_length(saved, nrow(d))
    expect_equal(as.character(saved),
                 unname(c(DOD = "Event", DOC = "Competing", AWD = "Censored", NED = "Censored")[as.character(d$st)]))

    direct <- .rr_cr(d)$medianTable$asDF
    back <- .rr_run(data.frame(t = d$t, o = saved, g = d$g), elapsedtime = "t", outcome = "o",
                    explanatory = "g")$medianTable$asDF
    expect_equal(back$events, direct$events)
    expect_equal(back$median, direct$median)
})

test_that("a single observed group leaves pairwise with a note, not an error", {
    d <- .rr_data()
    d <- d[d$grp == "A", ]
    d$grp <- droplevels(d$grp)
    r <- .rr_ss(d, pw = TRUE)
    expect_equal(r$medianTable$rowCount, 1)
    expect_match(.rr_notes(r$pairwiseTable), "at least two observed groups", fixed = TRUE)
})

test_that("calibration assesses the stratified model that is reported", {
    i <- 1:80
    d <- data.frame(x = (17 * i) %% 41, status = as.integer(i %% 3 != 0),
                    s = factor(ifelse(i > 40, "hi", "lo")), time = ifelse(i <= 40, i, i + 60))
    d$ev <- factor(ifelse(d$status == 1, "Dead", "Alive"))
    d$grp <- factor(ifelse(d$x > 20, "H", "L"))
    r <- .rr_ss(d, stratified_cox = TRUE, strata_variable = "s", calibration_curves = TRUE,
                calibration_timepoint = 30)
    fit <- coxph(Surv(time, status) ~ grp + strata(s), d)
    d$at <- 30
    nd <- d
    nd$time <- 30
    pred <- predict(fit, newdata = nd, type = "survival")
    expect_equal(sum(r$calibrationGroupTable$asDF$n * r$calibrationGroupTable$asDF$predicted),
                 sum(pred), tolerance = 1e-6)
    expect_equal(r$calibrationTable$asDF$value[1], 1, tolerance = 1e-4)
})

test_that("calibration on an unstratified model keeps its predictions", {
    d <- .rr_data()
    r <- .rr_ss(d, calibration_curves = TRUE, calibration_timepoint = 30)
    fit <- coxph(Surv(time, ev == "Dead") ~ grp, d)
    bh <- basehaz(fit, centered = TRUE)
    pred <- exp(-max(bh$hazard[bh$time <= 30]) * exp(predict(fit, type = "lp")))
    expect_equal(sum(r$calibrationGroupTable$asDF$n * r$calibrationGroupTable$asDF$predicted),
                 sum(pred), tolerance = 1e-6)
})

test_that("a risk group exhausted before t has observed survival 0, not a blank", {
    set.seed(9)
    n <- 300
    k <- data.frame(grp = factor(sample(c("L", "H"), n, TRUE)))
    tt <- ifelse(k$grp == "H", runif(n, 1, 20), rexp(n, .01))
    cc <- runif(n, 70, 100)
    k$time <- pmin(tt, cc)
    k$ev <- factor(ifelse(tt <= cc, "Dead", "Alive"))
    gt <- .rr_ss(k, calibration_curves = TRUE, calibration_timepoint = 40)$calibrationGroupTable$asDF
    expect_equal(as.numeric(gt$observed[1]), 0)
})

test_that("REMARK has the 20 published items and reports only what ran", {
    d <- .rr_data()
    h <- .rr_txt(.rr_ss(d, remark_checklist = TRUE)$remarkItems)
    expect_equal(.rr_ss(d, remark_checklist = TRUE)$remarkItems$rowCount, 20)
    expect_match(h, "Discuss implications for future research and clinical value.", fixed = TRUE)
    expect_match(h, "Give rationale for sample size", fixed = TRUE)

    ticks <- function(res) sum(res$remarkItems$asDF[[4]] == "Reported in this output")

    # apparent calibration is not validation
    r <- .rr_ss(d, remark_checklist = TRUE, calibration_curves = TRUE, calibration_timepoint = 30)
    expect_match(.rr_txt(r$remarkItems), "is not validation", fixed = TRUE)
    expect_equal(ticks(r), 0)

    # the plot, the hazard ratios and the probabilities together make item 15
    expect_equal(ticks(.rr_ss(d, remark_checklist = TRUE, sc = TRUE)), 1)

    # suppressed bootstrap does not count
    l <- d
    l$ev <- factor(ifelse(seq_len(nrow(d)) <= 6, "Dead", "Alive"))
    r <- .rr_ss(l, remark_checklist = TRUE, bootstrapValidation = TRUE, sc = TRUE)
    expect_equal(ticks(r), 0)

    # nothing Cox-based under competing risks
    r <- .rr_cr(.rr_cr_data(), remark_checklist = TRUE, sc = TRUE)
    expect_equal(ticks(r), 0)
    expect_match(.rr_txt(r$remarkItems), "cumulative incidence", fixed = TRUE)
})

test_that("time-zero rows are disclosed by the models that drop them", {
    z <- .rr_data()
    z$time[1:40] <- 0
    z$ev[1:40] <- "Dead"
    r <- .rr_ss(z, use_parametric = TRUE, age_adjustment = TRUE, age_variable = "age", age_time_scale = TRUE)
    expect_match(.rr_notes(r$parametricModelSummary), "40 patients with survival time 0", fixed = TRUE)
    expect_match(.rr_notes(r$ageTimeScaleTable), "40 patients with survival time 0", fixed = TRUE)
})

test_that("residuals name the source row", {
    d <- .rr_data(n = 60)
    d$time[1:3] <- NA
    expect_equal(.rr_ss(d, residual_diagnostics = TRUE)$residualsTable$asDF$observation[1], 4)
})

test_that("an explanatory named age_group survives the age-band working column", {
    d <- .rr_data()
    d$age_group <- d$grp
    r <- .rr_run(d, elapsedtime = "time", outcome = "ev", outcomeLevel = "Dead", explanatory = "age_group",
                 age_adjustment = TRUE, age_variable = "age", age_standardization = TRUE)
    expect_equal(as.character(r$ageStandardizationTable$asDF$group), c("A", "B", "C"))
})

test_that("stratified Cox text is not called unadjusted univariable", {
    r <- .rr_ss(.rr_data(), stratified_cox = TRUE, strata_variable = "sex", showSummaries = TRUE)
    expect_false(any(grepl("unadjusted, univariable", r$coxSummary$content)))
    expect_true(any(grepl("stratified by sex", r$coxSummary$content)))
})

test_that("the pairwise note names the test actually run", {
    d <- .rr_data()
    d <- d[d$grp != "C", ]
    d$grp <- droplevels(d$grp)
    n <- .rr_notes(.rr_ss(d, pw = TRUE, survivalTestType = "fh_rho1")$pairwiseTable)
    expect_match(n, "Peto-Peto", fixed = TRUE)
    expect_false(grepl("log-rank test above", n, fixed = TRUE))
})

test_that("the rate table states its denominator", {
    r <- .rr_ss(.rr_data(), person_time = TRUE)
    expect_match(.rr_notes(r$personTimeTable), "Rates are per 100 person-months.", fixed = TRUE)
    expect_match(r$personTimeSummary$content, "per 100 person-months", fixed = TRUE)
})

test_that("the outcome time is refused as a spline predictor", {
    r <- .rr_ss(.rr_data(), rcs_analysis = TRUE, rcs_variable = "time")
    expect_match(.rr_notes(r$rcsTestTable), "cannot be used as a spline predictor", fixed = TRUE)
})

test_that("no stale Cox text survives an unfitted model; plots clear with their options", {
    src <- paste(vapply(ClinicoPath:::survivalClass$private_methods, function(f) paste(deparse(f), collapse = " "), character(1)), collapse = " ")
    expect_true(grepl("self$results$coxSummary$setContent(\"\")", src, fixed = TRUE))
    r <- .rr_ss(.rr_data(), stratified_cox = TRUE)
    expect_equal(r$coxTable$rowCount, 0)
    expect_false(nzchar(paste(r$coxSummary$content, collapse = "")))
    y <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "survival.r.yaml"))
    for (nm in c("plot2", "plot3")) {
        item <- Filter(function(i) identical(i$name, nm), y$items)[[1]]
        expect_true(all(c("pplot", "medianline") %in% unlist(item$clearWith)))
    }
})

test_that("copy-ready medians are rounded", {
    r <- .rr_ss(.rr_data(), showSummaries = TRUE)
    expect_false(grepl("[0-9]\\.[0-9]{3,} months", .rr_txt(r$copyReadySentencesExplanation)))
})

test_that("a small incidence rate keeps its value in the table and the summary", {
    d <- .rr_data()
    d$time <- d$time * 30.44
    r <- .rr_ss(d, person_time = TRUE, rate_multiplier = 1, timetypeoutput = "days")
    pt <- r$personTimeTable$asDF
    ev <- sum(d$ev == "Dead")
    expect_equal(pt$rate[1], ev / sum(d$time), tolerance = 1e-8)
    expect_gt(pt$rate_ci_lower[1], 0)
    expect_no_match(.rr_txt(r$personTimeSummary), "incidence rate: 0 per", fixed = TRUE)
})
