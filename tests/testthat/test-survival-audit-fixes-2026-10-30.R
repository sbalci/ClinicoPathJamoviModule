# Regressions from the 2026-10-30 jsurvival audit (survival).
# Runs against the umbrella (devtools::test) and an installed jsurvival
# (testthat::test_file(package = "jsurvival", load_package = "installed")).
#
# - A strata column name reached glue::glue() as template text: `Stage {1+1}`
#   printed "Stage 2" and `Stage {I-IV` crashed the analysis.
# - .survivalDisplayName() returned NA for an original (non snake_case) name, so the
#   pairwise title read "Pairwise comparisons: NA".
# - With stratified_cox on, the age-adjusted table's "unadjusted" HR came from an
#   unstratified model, so the change was blamed on age.
# - The calculatedtime column held landmark-shifted times, and only for rows kept.
# - Syntax mode dropped unset Level options (dod/dooc/awd/awod), which the wrapper requires.

sv_audit_data <- function() {
    set.seed(2026); n <- 300
    grp <- factor(sample(c("A", "B", "C"), n, TRUE))
    tt <- rexp(n, 0.02 * c(A = 1, B = 1.8, C = 3)[as.character(grp)]); cc <- runif(n, 10, 90)
    d <- data.frame(time = round(pmin(tt, cc), 2), status = factor(ifelse(tt <= cc, "Dead", "Alive")),
                    grp = grp, check.names = FALSE)
    d[["Stage {1+1}"]] <- factor(sample(c("I", "II"), n, TRUE))
    d[["Stage {I-IV"]] <- d[["Stage {1+1}"]]
    d$Grade <- d$grp
    d$age <- round(rnorm(n, 60, 10))
    d
}
sv_audit_run <- function(data, ...) {
    args <- list(data = data, elapsedtime = "time", explanatory = "grp", outcome = "status",
                 outcomeLevel = "Dead", dod = NULL, dooc = NULL, awd = NULL, awod = NULL)
    suppressWarnings(do.call(survival, utils::modifyList(args, list(...), keep.null = TRUE)))
}
sv_audit_text <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(x, collapse = " ")))

test_that("a strata column name with braces is shown literally, never evaluated", {
    d <- sv_audit_data()
    r <- sv_audit_run(d, stratified_cox = TRUE, strata_variable = "Stage {1+1}")
    txt <- sv_audit_text(r$tCoxtext2$content)
    expect_true(grepl("Stratified by Stage {1+1}", txt, fixed = TRUE))
    expect_false(grepl("Stratified by Stage 2", txt, fixed = TRUE))

    r2 <- sv_audit_run(d, stratified_cox = TRUE, strata_variable = "Stage {I-IV")
    expect_true(grepl("Stratified by Stage {I-IV", sv_audit_text(r2$tCoxtext2$content), fixed = TRUE))
})

test_that("display names resolve from the original or the cleaned name", {
    expect_identical(.survivalDisplayName("Grade", c(grade = "Grade")), "Grade")
    expect_identical(.survivalDisplayName("grade", c(grade = "Grade")), "Grade")
    r <- sv_audit_run(sv_audit_data(), explanatory = "Grade", pw = TRUE)
    expect_identical(r$pairwiseTable$title, "Pairwise comparisons: Grade")
    summary_txt <- sv_audit_text(r$pairwiseSummary$content)
    expect_true(grepl("difference of Grade between", summary_txt, fixed = TRUE))
    expect_false(grepl("difference of NA", summary_txt, fixed = TRUE))
})

test_that("age adjustment compares models with the same stratified_cox strata", {
    d <- sv_audit_data()
    r <- sv_audit_run(d, stratified_cox = TRUE, strata_variable = "Stage {1+1}",
                      age_adjustment = TRUE, age_variable = "age")
    ref <- survival::coxph(survival::Surv(time, status == "Dead") ~ grp + strata(`Stage {1+1}`), data = d)
    ci <- summary(ref)$conf.int
    expect_identical(r$ageAdjustedCoxTable$getCell(rowNo = 1, "hr_unadjusted")$value,
                     sprintf("%.2f (%.2f-%.2f)", ci[1, 1], ci[1, 3], ci[1, 4]))
})

test_that("calculatedtime holds every row's unshifted time under a landmark", {
    dd <- data.frame(dx = format(as.Date("2020-01-01") + 0:59),
                     status = factor(rep(c("Dead", "Alive"), 30)),
                     g = factor(rep(c("a", "b"), each = 30)))
    dd$fu <- format(as.Date(dd$dx) + round(seq(31, 900, length.out = 60)))
    o <- survivalOptions$new(tint = TRUE, dxdate = "dx", fudate = "fu", timetypedata = "ymd",
        timetypeoutput = "months", explanatory = "g", outcome = "status", outcomeLevel = "Dead",
        uselandmark = TRUE, landmark = 6)
    o$.__enclos_env__$private[["..calculatedtime"]]$.__enclos_env__$private$.value <-
        list(value = TRUE, vars = "calculatedtime")
    a <- survivalClass$new(options = o, data = dd)
    a$init(); suppressWarnings(a$run())
    v <- unlist(a$results$calculatedtime$.__enclos_env__$private$.values, use.names = FALSE)
    want <- lubridate::time_length(lubridate::interval(as.Date(dd$dx), as.Date(dd$fu)), "months")
    expect_length(v, nrow(dd))
    expect_equal(v, want)
})

test_that("the Syntax-mode call keeps unset Level options as NULL and runs", {
    d <- sv_audit_data()
    o <- survivalOptions$new(elapsedtime = "time", explanatory = "grp", outcome = "status", outcomeLevel = "Dead")
    a <- survivalClass$new(options = o, data = d)
    a$init(); suppressWarnings(a$run())
    src <- a$asSource()
    for (lv in c("dod", "dooc", "awd", "awod")) expect_match(src, paste0(lv, " = NULL"), fixed = TRUE)
    call <- parse(text = sub("^[A-Za-z.]+::", "", src))[[1]]
    env <- new.env(parent = environment()); env$data <- d
    expect_error(suppressWarnings(eval(call, env)), NA)  # the copied call, as a user would run it
})
