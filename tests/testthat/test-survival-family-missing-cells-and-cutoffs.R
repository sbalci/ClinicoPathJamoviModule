# Regressions from the 2026-10-29 jsurvival comprehensive check.
#
# 1. A not-reached median (or an interval that was never computed) was written as numeric
#    NA, which jamovi shows as "NaN" - in survival, singlearm, survivalcont and multisurvival.
# 2. survivalcont `survivalPlotsExplanation` had `visible: ((...` - jmvcore evaluates only
#    `^\([$A-Za-z]`, so it was always shown.
# 3. survivalcont quantile cut-offs under a minimum group size were reduced by deleting
#    cut-points, ending at one 80/20 split where tertiles meet a 25% minimum.

# Visible table cells holding a numeric NA - what jamovi renders as "NaN".
nan_cells <- function(x, path = "") {
    items <- tryCatch(x$items, error = function(e) NULL)
    if (length(items)) {
        nm <- names(items); if (is.null(nm)) nm <- seq_along(items)
        return(unlist(lapply(seq_along(items), function(i)
            nan_cells(items[[i]], paste0(path, "/", nm[i])))))
    }
    if (!inherits(x, "Table") || !isTRUE(x$visible)) return(character())
    out <- character()
    for (col in x$.__enclos_env__$private$.columns) {
        if (!isTRUE(col$visible)) next
        for (r in seq_len(x$rowCount)) {
            v <- col$getCell(r)$value
            if (is.double(v) && length(v) == 1 && is.na(v))
                out <- c(out, sprintf("%s[%d,%s]", path, r, col$name))
        }
    }
    out
}

# 19 events in 150: no median is reached.
low_event_data <- function() {
    set.seed(7); n <- 150
    tt <- rexp(n, 1 / 200); cz <- runif(n, 1, 60)
    data.frame(time = pmin(tt, cz), ev = as.integer(tt <= cz),
               marker = round(rnorm(n, 50, 12), 1),
               grp = factor(sample(c("A", "B"), n, TRUE)))
}

step_effect_data <- function() {
    set.seed(42); n <- 300
    marker <- round(rnorm(n, 50, 12), 1)
    t_ev <- rexp(n, 0.02 * exp(ifelse(marker > 55, 0.9, 0))); t_c <- runif(n, 10, 120)
    data.frame(time = round(pmin(t_ev, t_c), 2), status = as.integer(t_ev <= t_c), marker = marker)
}

no_events <- list(dod = NULL, dooc = NULL, awd = NULL, awod = NULL)

test_that("singlearm: a not-reached median is blank and footnoted, not NaN", {
    r <- do.call(singlearm, c(list(data = low_event_data(), elapsedtime = "time", outcome = "ev",
                                   outcomeLevel = "1", cutp = "0.5, 12"), no_events))
    expect_length(nan_cells(r), 0)
    expect_identical(r$medianTable$getCell(rowNo = 1, col = "median")$value, "")
    expect_length(r$medianTable$getCell(rowNo = 1, col = "median")$footnotes, 1)
})

test_that("survival: not-reached medians and parametric shape/scale p are blank, not NaN", {
    r <- do.call(survival, c(list(data = low_event_data(), elapsedtime = "time", explanatory = "grp",
                                  outcome = "ev", outcomeLevel = "1", use_parametric = TRUE), no_events))
    expect_length(nan_cells(r), 0)
    expect_identical(r$parametricModelSummary$getCell(rowNo = 1, col = "pvalue")$value, "")
})

test_that("survivalcont: medians, multiple-cut-off medians and residuals show no NaN", {
    r <- do.call(survivalcont, c(list(data = low_event_data(), elapsedtime = "time", contexpl = "marker",
                                      outcome = "ev", outcomeLevel = "1", findcut = TRUE,
                                      multiple_cutoffs = TRUE, residual_diagnostics = TRUE), no_events))
    expect_length(nan_cells(r), 0)
    expect_false("schoenfeld" %in% names(r$residualsTable$asDF))
})

test_that("survivalcont: the survival-plots explanation follows showExplanations", {
    for (expl in c(FALSE, TRUE)) {
        r <- do.call(survivalcont, c(list(data = step_effect_data(), elapsedtime = "time",
                                          contexpl = "marker", outcome = "status", outcomeLevel = "1",
                                          findcut = TRUE, sc = TRUE, showExplanations = expl), no_events))
        expect_identical(isTRUE(r$survivalPlotsExplanation$visible), expl)
    }
})

test_that("survivalcont: quantile cut-offs fall back to fewer quantiles, not to an uneven split", {
    d <- step_effect_data()
    r <- do.call(survivalcont, c(list(data = d, elapsedtime = "time", contexpl = "marker",
                                      outcome = "status", outcomeLevel = "1", multiple_cutoffs = TRUE,
                                      cutoff_method = "quantile", num_cutoffs = "four",
                                      min_group_size = 25), no_events))
    cuts <- r$multipleCutTable$asDF$cutpoint_value
    sizes <- as.integer(table(cut(d$marker, c(-Inf, cuts, Inf))))
    expect_length(cuts, 2)                       # tertiles; the old greedy loop left one cut
    expect_true(min(sizes) >= ceiling(0.25 * nrow(d)))
})

test_that("multisurvival: survival metrics without intervals are blank and say why", {
    set.seed(42); n <- 300
    d <- data.frame(treatment = factor(sample(c("A", "B"), n, TRUE)),
                    grade = factor(sample(c("G1", "G2", "G3"), n, TRUE)))
    t_ev <- rexp(n, 0.02 * exp(0.5 * (d$treatment == "B") + 0.6 * (d$grade == "G3")))
    t_c <- runif(n, 10, 120)
    d$time <- round(pmin(t_ev, t_c), 2); d$status <- factor(ifelse(t_ev <= t_c, "Dead", "Alive"))
    r <- do.call(multisurvival, c(list(data = d, elapsedtime = "time", outcome = "status",
                                       outcomeLevel = "Dead", explanatory = c("treatment", "grade"),
                                       show_survmetrics = TRUE), no_events))
    expect_gt(r$survMetricsTable$rowCount, 1)
    expect_length(nan_cells(r), 0)
    expect_true(any(grepl("concordance index only",
                          vapply(r$survMetricsTable$notes, function(n) n$note, ""))))
})
