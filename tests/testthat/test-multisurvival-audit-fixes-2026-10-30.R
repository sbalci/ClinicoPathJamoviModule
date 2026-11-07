# Regressions from the 2026-10-30 jsurvival audit (multisurvival).
#
# - The optimism bootstrap discarded every resample whose coxph refit WARNED
#   (monotone likelihood), biasing the corrected C-index upward at low EPV, and the
#   table never said how many resamples were used.
# - An aliased interaction coefficient (empty cross-cell) rendered "NaN".
# - The joint interaction test printed "p = <0.001".

ms_internal <- function(name) get(name, envir = environment(multisurvival))
ms_nan_cells <- function(t) {
    out <- character()
    for (col in t$.__enclos_env__$private$.columns) for (i in seq_len(t$rowCount)) {
        v <- col$getCell(i)$value
        if (is.double(v) && length(v) == 1 && is.na(v)) out <- c(out, paste0(col$name, "@", i))
    }
    out
}
run_ms_fix <- function(data, ...) {
    args <- list(data = data, elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
                 dod = NULL, dooc = NULL, awd = NULL, awod = NULL)
    do.call(multisurvival, utils::modifyList(args, list(...), keep.null = TRUE))
}

test_that("optimism bootstrap keeps resamples whose refit only warns (low EPV)", {
    set.seed(42); n <- 90
    low <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n), x4 = rnorm(n),
                      b1 = rbinom(n, 1, 0.08), b2 = rbinom(n, 1, 0.1))
    tt <- rexp(n, 0.02 * exp(0.5 * low$x1)); cc <- runif(n, 5, 40)
    low$time <- pmin(tt, cc); low$status <- as.integer(tt <= cc)
    fit <- survival::coxph(survival::Surv(time, status) ~ x1 + x2 + x3 + x4 + b1 + b2, data = low)
    res <- ms_internal(".multisurvivalOptimismCIndex")(fit, low, low$status, B = 200, seed = 1234)
    # 10 of these 200 refits warn (monotone likelihood); all have finite coefficients.
    expect_equal(res$n_boot, 200L)
    expect_identical(res$n_excluded, 0L)
    expect_equal(res$corrected, res$apparent - res$optimism, tolerance = 1e-12)
})

test_that("excluded resamples are counted and reported in the C-index table", {
    set.seed(3); n <- 120
    d <- data.frame(age = rnorm(n, 60, 10),
                    grp = factor(c(rep("rare", 2), sample(c("a", "b"), n - 2, TRUE))))
    tt <- rexp(n, 0.03); cc <- runif(n, 5, 80)
    d$time <- round(pmin(tt, cc), 2); d$status <- factor(ifelse(tt <= cc, "Dead", "Alive"))
    r <- run_ms_fix(d, explanatory = "grp", contexpl = "age", ci_optimism = TRUE, ci_optimism_boot = 60)
    tbl <- r$cindexValidation
    detail <- tbl$getCell(rowKey = "optimism", "detail")$value
    expect_match(detail, "of 60 resamples", fixed = TRUE)
    used <- as.integer(sub("Mean over ([0-9]+) of.*", "\\1", detail))
    expect_lt(used, 60L)
    expect_match(tbl$notes$excluded$note, paste(60L - used, "of 60 bootstrap resamples were excluded"), fixed = TRUE)
})

test_that("an aliased interaction coefficient is blanked and footnoted, not NaN", {
    set.seed(11); n <- 240
    g3 <- factor(sample(c("A", "B", "C"), n, TRUE)); g2 <- factor(sample(c("X", "Y"), n, TRUE))
    g2[g3 == "C"] <- "X"                      # empty C:Y cell -> aliased g3C:g2Y
    tt <- rexp(n, 0.03 * ifelse(g3 == "B", 1.5, 1)); cc <- runif(n, 5, 80)
    d <- data.frame(time = round(pmin(tt, cc), 2), status = factor(ifelse(tt <= cc, "Dead", "Alive")),
                    g3 = g3, g2 = g2, age = rnorm(n, 60, 10))
    r <- run_ms_fix(d, explanatory = c("g3", "g2"), contexpl = "age",
                    interactions = list(c("g3", "g2")), compare_models = TRUE)
    it <- r$interactionTest
    expect_length(ms_nan_cells(it), 0L)
    expect_length(ms_nan_cells(r$modelContributionTable), 0L)
    row <- which(vapply(seq_len(it$rowCount), function(i)
        identical(it$getCell(rowNo = i, "term")$value, "g3C:g2Y"), logical(1)))
    expect_length(row, 1L)
    for (col in c("hr", "ci_lower", "ci_upper", "p")) {
        expect_identical(it$getCell(rowNo = row, col)$value, "")
        expect_match(paste(it$getCell(rowNo = row, col)$footnotes, collapse = " "), "aliased", fixed = TRUE)
    }
})

test_that("joint interaction test p-text never reads 'p = <'", {
    set.seed(8); n <- 400
    g3 <- factor(sample(c("A", "B", "C"), n, TRUE)); g2 <- factor(sample(c("X", "Y"), n, TRUE))
    hr <- ifelse(g2 == "Y" & g3 == "C", 6, ifelse(g2 == "Y" & g3 == "B", 0.3, 1))
    tt <- rexp(n, 0.03 * hr); cc <- runif(n, 5, 80)
    d <- data.frame(time = round(pmin(tt, cc), 2), status = factor(ifelse(tt <= cc, "Dead", "Alive")),
                    g3 = g3, g2 = g2)
    r <- run_ms_fix(d, explanatory = c("g3", "g2"), interactions = list(c("g3", "g2")))
    note <- r$interactionTest$notes$joint$note
    expect_match(note, "p < 0.001", fixed = TRUE)
    expect_false(grepl("p = <", note, fixed = TRUE))
})
