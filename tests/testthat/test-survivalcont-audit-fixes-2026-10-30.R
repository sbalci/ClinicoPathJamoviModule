# Regressions from the 2026-10-30 jsurvival audit (survivalcont).
#
# - survTable wrote numeric NA (shown as "NaN") for a time point beyond one cut-off
#   group's follow-up, under a note claiming "blank/NA".
# - "Analysis Complete" listed RMST / residuals / person-time / Cox from the OPTIONS,
#   so an unsupported RMST horizon was still announced as completed.
# - Html panels were reset with "", which the jamovi client skips, so a run that never
#   refilled them kept the previous run's text.

sc_fix_data <- function() {
    set.seed(5); m <- 300; marker <- round(rnorm(m, 50, 12), 1)
    hi <- marker > 55
    t_ev <- rexp(m, ifelse(hi, 0.06, 0.01)); t_c <- ifelse(hi, runif(m, 1, 30), runif(m, 1, 120))
    data.frame(time = round(pmin(t_ev, t_c), 2), status = as.integer(t_ev <= t_c), marker = marker)
}
run_sc_fix <- function(...) {
    args <- list(data = sc_fix_data(), elapsedtime = "time", contexpl = "marker",
                 outcome = "status", outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL)
    do.call(survivalcont, utils::modifyList(args, list(...), keep.null = TRUE))
}
sc_nan_cells <- function(t) {
    out <- character()
    for (col in t$.__enclos_env__$private$.columns) for (i in seq_len(t$rowCount)) {
        v <- col$getCell(i)$value
        if (is.double(v) && length(v) == 1 && is.na(v)) out <- c(out, paste0(col$name, "@", i))
    }
    out
}

test_that("survTable blanks and footnotes estimates beyond a group's follow-up (no NaN cells)", {
    r <- run_sc_fix(findcut = TRUE, cutp = "12, 60, 100")
    t <- r$survTable
    expect_length(sc_nan_cells(t), 0L)
    blank_rows <- which(vapply(seq_len(t$rowCount), function(i)
        identical(t$getCell(rowNo = i, "surv")$value, ""), logical(1)))
    expect_gt(length(blank_rows), 0L)
    for (i in blank_rows) for (col in c("surv", "lower", "upper")) {
        expect_identical(t$getCell(rowNo = i, col)$value, "")
        expect_match(paste(t$getCell(rowNo = i, col)$footnotes, collapse = " "),
                     "Beyond the observed follow-up", fixed = TRUE)
    }
    note <- t$notes$stratum_horizon$note
    expect_match(note, "blank cells", fixed = TRUE)
    expect_false(grepl("NA)", note, fixed = TRUE))
})

test_that("an unsupported RMST horizon is not listed as successfully completed", {
    r <- run_sc_fix(findcut = TRUE, rmst_analysis = TRUE, rmst_tau = 100)
    expect_equal(r$rmstTable$rowCount, 0L)
    expect_match(r$errors$content, "Unsupported RMST horizon", fixed = TRUE)
    done <- sub(".*(Successfully completed:[^.]*).*", "\\1", r$infoMessages$content)
    expect_match(done, "Cox regression", fixed = TRUE)
    expect_false(grepl("RMST analysis", done, fixed = TRUE))
})

test_that("a supported RMST horizon is still listed as completed", {
    r <- run_sc_fix(findcut = TRUE, rmst_analysis = TRUE, rmst_tau = 10)
    expect_gt(r$rmstTable$rowCount, 0L)
    expect_match(r$infoMessages$content, "RMST analysis", fixed = TRUE)
})

test_that("Html panels are reset with non-empty markup, never \"\"", {
    # No explanatory variable: .run() clears the panels and stops at the todo screen.
    r <- run_sc_fix(contexpl = NULL)
    for (nm in c("tCoxtext2", "coxSummary", "personTimeSummary", "eventRecodeInfo"))
        expect_identical(r[[nm]]$content, "<div></div>", info = nm)
})
