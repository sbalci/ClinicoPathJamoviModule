# Wilson confidence intervals and the notices channel of diagnosticmeta.
#
# This file previously documented intended behavior the code did not deliver
# (Wilson CIs in the Individual Study Results table, jmvcore::Notice objects)
# through vacuous expect_s3_class() checks. It now tests what the module
# actually does: Wilson score CIs in the individual-studies table (matching an
# independent implementation), zero-cell corrections under their renamed keys,
# and warnings/information delivered through the dedicated `notices` Html item.

# Independent Wilson score interval implementation (Wilson 1927), used as the
# reference the module's table values must reproduce.
wilson_ref <- function(x, n, conf = 0.95) {
    z <- qnorm(1 - (1 - conf) / 2)
    p <- x / n
    denom <- 1 + z^2 / n
    center <- (p + z^2 / (2 * n)) / denom
    margin <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
    c(max(0, center - margin), min(1, center + margin))
}

studies_df <- function(n = 6) {
    set.seed(42)
    data.frame(
        study = paste0("Study", seq_len(n)),
        tp = c(80, 85, 90, 75, 88, 92)[seq_len(n)],
        fp = c(20, 15, 10, 25, 12, 8)[seq_len(n)],
        fn = c(20, 15, 10, 25, 12, 8)[seq_len(n)],
        tn = c(80, 85, 90, 75, 88, 92)[seq_len(n)],
        stringsAsFactors = FALSE
    )
}

run_dm <- function(data, ...) {
    diagnosticmeta(
        data = data, study = "study",
        true_positives = "tp", false_positives = "fp",
        false_negatives = "fn", true_negatives = "tn", ...)
}

notices_text <- function(res) {
    gsub("<[^>]+>", " ", paste(res$notices$content, collapse = " "))
}

# ==============================================================================
# WILSON CIs IN THE INDIVIDUAL STUDY RESULTS TABLE
# ==============================================================================

test_that("Individual study table reports Wilson score CIs matching an independent implementation", {
    skip_if_not_installed("mada")

    d <- studies_df()
    res <- run_dm(d, show_individual_studies = TRUE)
    tab <- res$individualstudies$asDF

    expect_equal(nrow(tab), nrow(d))
    expect_true(all(c("sens_ci_lower", "sens_ci_upper",
                      "spec_ci_lower", "spec_ci_upper") %in% names(tab)))

    for (i in seq_len(nrow(d))) {
        sens_ref <- wilson_ref(d$tp[i], d$tp[i] + d$fn[i]) * 100
        spec_ref <- wilson_ref(d$tn[i], d$tn[i] + d$fp[i]) * 100
        expect_equal(tab$sens_ci_lower[i], sens_ref[1], tolerance = 1e-8)
        expect_equal(tab$sens_ci_upper[i], sens_ref[2], tolerance = 1e-8)
        expect_equal(tab$spec_ci_lower[i], spec_ref[1], tolerance = 1e-8)
        expect_equal(tab$spec_ci_upper[i], spec_ref[2], tolerance = 1e-8)
    }

    # CIs must bracket the point estimate
    expect_true(all(tab$sens_ci_lower <= tab$sensitivity))
    expect_true(all(tab$sens_ci_upper >= tab$sensitivity))
})

test_that("Individual study Wilson CIs honour the confidence level option", {
    skip_if_not_installed("mada")

    d <- studies_df()
    res90 <- run_dm(d, show_individual_studies = TRUE, confidence_level = 90)
    tab90 <- res90$individualstudies$asDF

    ref90 <- wilson_ref(d$tp[1], d$tp[1] + d$fn[1], conf = 0.90) * 100
    expect_equal(tab90$sens_ci_lower[1], ref90[1], tolerance = 1e-8)
    expect_equal(tab90$sens_ci_upper[1], ref90[2], tolerance = 1e-8)

    # 90% interval is strictly narrower than the 95% one
    res95 <- run_dm(d, show_individual_studies = TRUE, confidence_level = 95)
    tab95 <- res95$individualstudies$asDF
    expect_true(all(tab90$sens_ci_upper - tab90$sens_ci_lower <
                    tab95$sens_ci_upper - tab95$sens_ci_lower))
})

test_that("Wilson CIs remain inside [0, 100] at extreme proportions", {
    skip_if_not_installed("mada")

    d <- studies_df()
    d$fn[1] <- 0   # perfect sensitivity in study 1 (also a zero cell)
    res <- run_dm(d, show_individual_studies = TRUE)
    tab <- res$individualstudies$asDF

    expect_true(all(tab$sens_ci_lower >= 0 & tab$sens_ci_upper <= 100))
    expect_true(all(tab$spec_ci_lower >= 0 & tab$spec_ci_upper <= 100))
    # Perfect proportion: upper bound 100, lower bound < 100 (Wilson, not Wald)
    expect_equal(tab$sens_ci_upper[1], 100, tolerance = 1e-8)
    expect_lt(tab$sens_ci_lower[1], 100)
})

# ==============================================================================
# ZERO-CELL CORRECTIONS (renamed keys: zero_cells, reciprocal_n)
# ==============================================================================

test_that("all four zero-cell correction settings run and are disclosed", {
    skip_if_not_installed("mada")

    d <- studies_df()
    d$fp[3] <- 0

    for (corr in c("none", "constant", "zero_cells", "reciprocal_n")) {
        res <- run_dm(d, zero_cell_correction = corr)
        expect_s3_class(res, "diagnosticmetaResults")
        expect_gt(res$bivariateresults$rowCount, 0)

        if (corr == "none") {
            # model-level correction disclosed on the bivariate table
            notes <- vapply(res$bivariateresults$notes,
                            function(n) n$note, character(1))
            expect_true(any(grepl("zero cell", notes, ignore.case = TRUE)))
        } else {
            # data-level corrections disclosed in the always-visible notices,
            # under their human-readable labels (not the raw option keys)
            labels <- c(constant = "+0.5 to all cells of zero-cell studies",
                        zero_cells = "+0.5 to the zero cells only",
                        reciprocal_n = "+1/N to all cells")
            expect_match(notices_text(res), "Zero-cell correction applied",
                         fixed = TRUE)
            expect_match(notices_text(res), labels[[corr]], fixed = TRUE)
        }
    }
})

test_that("the corrections modify the data differently (none vs constant vs zero_cells)", {
    skip_if_not_installed("mada")

    d <- studies_df()
    d$fp[3] <- 0

    est <- function(corr) {
        run_dm(d, zero_cell_correction = corr)$bivariateresults$asDF$estimate[2]
    }
    spec_none <- est("none")
    spec_constant <- est("constant")
    spec_zero_cells <- est("zero_cells")

    # "none" (mada 'single': +0.5 to all cells of the affected study at fit
    # time) and "constant" (+0.5 to all cells before analysis) are the same
    # arithmetic here; "zero_cells" corrects only the zero cell and must
    # differ from both.
    expect_equal(spec_none, spec_constant, tolerance = 1e-6)
    expect_false(isTRUE(all.equal(spec_zero_cells, spec_constant,
                                  tolerance = 1e-6)))
})

# ==============================================================================
# NOTICES CHANNEL
# ==============================================================================

test_that("fewer than 3 studies is a hard reject, not a silent note", {
    expect_error(run_dm(studies_df(2)), "At least 3 studies")
})

test_that("meta-regression without a covariate raises an INFO notice", {
    skip_if_not_installed("mada")

    res <- run_dm(studies_df(), meta_regression = TRUE, covariate = NULL)
    expect_match(notices_text(res), "Meta-regression requires a covariate",
                 fixed = TRUE)
})

test_that("Deeks' test below 10 studies raises the power caution notice", {
    skip_if_not_installed("mada")
    skip_if_not_installed("metafor")

    res <- run_dm(studies_df(), publication_bias = TRUE)
    expect_match(notices_text(res), "fewer than 10 studies", fixed = TRUE)
})

test_that("excluded studies are counted in a WARNING notice", {
    skip_if_not_installed("mada")

    d <- studies_df()
    d$tp[2] <- NA
    res <- run_dm(d)
    txt <- notices_text(res)
    expect_match(txt, "Studies excluded", fixed = TRUE)
    expect_match(txt, "1 of 6 studies were excluded", fixed = TRUE)
})

test_that("notices do not accumulate across run cycles", {
    skip_if_not_installed("mada")

    d <- studies_df()
    d$tp[2] <- NA

    options <- ClinicoPath:::diagnosticmetaOptions$new(
        study = "study", true_positives = "tp", false_positives = "fp",
        false_negatives = "fn", true_negatives = "tn")
    analysis <- ClinicoPath:::diagnosticmetaClass$new(options = options, data = d)
    analysis$run()
    analysis$run()   # second cycle on the same instance

    content <- analysis$results$notices$content
    hits <- gregexpr("Studies excluded", content, fixed = TRUE)[[1]]
    expect_equal(sum(hits > 0), 1)
})

test_that("instructions panel stays pure onboarding (no warning banners)", {
    skip_if_not_installed("mada")

    d <- studies_df()
    d$tp[2] <- NA
    res <- run_dm(d)
    expect_false(grepl("Studies excluded", res$instructions$content, fixed = TRUE))
})
