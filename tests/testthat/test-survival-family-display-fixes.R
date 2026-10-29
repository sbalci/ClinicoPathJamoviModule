# Regressions from the 2026-10-29 jsurvival comprehensive check (second round).
#
# survival:      the Kaplan-Meier table showed empty in competing-risks mode; age outputs
#                showed empty when age adjustment was off; the log-log plot's main path
#                always errored into a fallback; p-values read "p=<0.001" and "p = 0.000".
# survivalcont:  Html text printed literal **markdown** and "p = < 0.001".
# multisurvival: the clinical summary printed raw coefficient names ("treatmentB").
# singlearm:     tolower() printed "kaplan-meier event-free probability".

survival_data <- function() {
    set.seed(2026); n <- 300
    grp <- factor(sample(c("A", "B", "C"), n, TRUE)); age <- round(rnorm(n, 62, 10))
    tt <- rexp(n, 0.02 * c(A = 1, B = 1.8, C = 3)[as.character(grp)] * exp(0.02 * (age - 62)))
    cc <- runif(n, 10, 90); ev <- as.integer(tt <= cc)
    cause <- ifelse(ev == 1, ifelse(runif(n) < 0.65, "DoD", "DoOC"),
                    ifelse(runif(n) < 0.5, "AwD", "AwoD"))
    data.frame(time = round(pmin(tt, cc), 2), status = factor(ifelse(ev == 1, "Dead", "Alive")),
               cause = factor(cause), grp = grp, age = age,
               sex = factor(sample(c("F", "M"), n, TRUE)))
}
run_survival <- function(...) {
    args <- list(data = survival_data(), elapsedtime = "time", explanatory = "grp", outcome = "status",
                 outcomeLevel = "Dead", dod = NULL, dooc = NULL, awd = NULL, awod = NULL)
    do.call(survival, utils::modifyList(args, list(...), keep.null = TRUE))
}
note_text <- function(t) vapply(t$notes, function(n) n$note, "")
bad_p <- "p ?= ?<|p ?= ?0\\.000\\b"
good_p <- "p < 0\\.001|p = [01]\\.[0-9]{3}"

test_that("survival: competing-risks mode hides the KM table and points to the CIF table", {
    cr <- list(outcome = "cause", outcomeLevel = "DoD", multievent = TRUE, dod = "DoD", dooc = "DoOC",
               awd = "AwD", awod = "AwoD", analysistype = "compete")
    off <- do.call(run_survival, c(cr, list(cif_all_causes = FALSE)))
    expect_false(isTRUE(off$survTable$visible))
    expect_true(any(grepl("All event types at selected times", note_text(off$medianTable))))
    gray <- grep("Gray's test", note_text(off$medianTable), value = TRUE)
    expect_length(gray, 1)
    expect_match(gray, good_p)
    expect_false(grepl(bad_p, gray))
    on <- do.call(run_survival, c(cr, list(cif_all_causes = TRUE)))
    expect_false(any(grepl("All event types at selected times", note_text(on$medianTable))))
    expect_true(isTRUE(run_survival()$survTable$visible))
})

test_that("survival: age outputs are hidden unless age adjustment is on", {
    r <- run_survival(age_standardization = TRUE, age_interaction = TRUE, age_variable = "age")
    expect_false(isTRUE(r$ageStandardizationTable$visible))
    expect_false(isTRUE(r$ageInteractionTable$visible))
    r <- run_survival(age_adjustment = TRUE, age_standardization = TRUE, age_variable = "age")
    expect_true(isTRUE(r$ageStandardizationTable$visible))
    expect_gt(r$ageStandardizationTable$rowCount, 0)
})

test_that("survival: the log-log plot draws the KM steps at the event times", {
    r <- run_survival(loglog = TRUE)
    f <- tempfile(fileext = ".png")
    r$plot7$saveAs(f)
    p <- ggplot2::last_plot()
    expect_s3_class(p$layers[[1]]$geom, "GeomStep")
    expect_setequal(unique(p$data$strata), c("A", "B", "C"))
})

test_that("survival: p-values in the Cox text read 'p < 0.001' / 'p = 0.xxx'", {
    for (r in list(run_survival(), run_survival(stratified_cox = TRUE, strata_variable = "sex"))) {
        txt <- r$tCoxtext2$content
        expect_match(txt, good_p)
        expect_false(grepl(bad_p, txt))
        expect_false(grepl("( Max", txt, fixed = TRUE))
    }
})

test_that("survivalcont: Cox text is HTML, not markdown, and the log-rank note reads cleanly", {
    set.seed(42); m <- 300; marker <- round(rnorm(m, 50, 12), 1)
    t_ev <- rexp(m, 0.02 * exp(ifelse(marker > 55, 0.9, 0))); t_c <- runif(m, 10, 120)
    d <- data.frame(time = round(pmin(t_ev, t_c), 2), status = as.integer(t_ev <= t_c), marker = marker)
    r <- survivalcont(data = d, elapsedtime = "time", contexpl = "marker", outcome = "status",
                      outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
                      multiple_cutoffs = TRUE, uselandmark = TRUE, landmark = 3)
    h <- r$tCoxtext2$content
    expect_match(h, "<b>Model Metrics:</b>", fixed = TRUE)
    expect_match(h, "<b>Landmark Analysis:</b>", fixed = TRUE)
    expect_false(grepl("**", h, fixed = TRUE))
    expect_false(grepl(bad_p, h))
    lr <- grep("Log-rank", note_text(r$multipleCutTable), value = TRUE)
    expect_length(lr, 1)
    expect_match(lr, good_p)
    expect_false(grepl(bad_p, lr))
})

test_that("multisurvival: the clinical summary names variables as the user wrote them", {
    set.seed(42); m <- 300
    d <- data.frame(treatment = factor(sample(c("A", "B"), m, TRUE)),
                    `Tumour grade` = factor(sample(c("G1", "G2", "G3"), m, TRUE)), check.names = FALSE)
    t_ev <- rexp(m, 0.02 * exp(1.2 * (d$treatment == "B")))
    t_c <- runif(m, 10, 120)
    d$time <- round(pmin(t_ev, t_c), 2); d$status <- factor(ifelse(t_ev <= t_c, "Dead", "Alive"))
    r <- multisurvival(data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
                       dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
                       explanatory = c("treatment", "Tumour grade"), showSummaries = TRUE)
    s <- r$text$content
    expect_match(s, "Strongest predictor:", fixed = TRUE)
    expect_match(s, "treatment: B (vs A)", fixed = TRUE)
    expect_false(grepl("treatmentB|tumour_gradeG", s))
})

test_that("singlearm: the estimand label keeps its capitals in the time-point summary", {
    set.seed(42); m <- 300
    t_ev <- rexp(m, 0.02); t_c <- runif(m, 10, 120)
    d <- data.frame(time = round(pmin(t_ev, t_c), 2), status = as.integer(t_ev <= t_c))
    r <- singlearm(data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
                   dod = NULL, dooc = NULL, awd = NULL, awod = NULL, showSummaries = TRUE)
    s <- r$survTableSummary$content
    expect_match(s, "Kaplan-Meier event-free probability at", fixed = TRUE)
    expect_false(grepl("kaplan-meier", s))
})
