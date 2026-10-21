# Regression tests for the meddecide 1.1.1 release fixes to decisioncombine (2026-10-21).
#
#   XA-01 (S2)  zero-cell LR/DOR: an empty NUMERATOR cell gives an exact 0, only an empty
#               DENOMINATOR is corrected, and a correction that would land at or below 1 is blank
#               (shared rule .diagRatioRules(), R/utils-diagnostic-ratios.R)
#   DC-01 (S3)  cells undefined by design are written as "" (an empty cell), not NA_real_,
#               which jamovi prints as "NaN"
#   M1          an empty notices panel is "<div></div>", not "" (the client skips "")
#   XA-04 (S3)  the proportions CI table names its interval method (Wilson)
#   DC-02 (S4)  the About panel no longer says correlated tests make Parallel/Serial "worse"
#
# Sources: development-ideas/decisioncombine-independent-review-2026-10-21.md and
# development-ideas/meddecide-cross-analysis-review-2026-10-21.md. Expected values are hand
# arithmetic on the fixture counts (written out beside each assertion).

.rf_mk <- function(cD, cN) {
    pats <- c(names(cD), names(cN))
    counts <- c(cD, cN)
    x <- rep(pats, counts)
    d <- data.frame(gold = factor(rep(rep(c("D", "N"), c(length(cD), length(cN))), counts),
                                  levels = c("D", "N")))
    for (j in seq_len(lengths(strsplit(pats[1], "/", fixed = TRUE)))) {
        sign <- substr(x, 2 * j - 1, 2 * j - 1)
        d[[paste0("t", j)]] <- factor(ifelse(sign == "+", "pos", "neg"), levels = c("pos", "neg"))
    }
    d
}

.rf_object <- function(d, t2 = "t2", ...) {
    options <- decisioncombineOptions$new(
        gold = "gold", goldPositive = "D", test1 = "t1", test1Positive = "pos",
        test2 = t2, test2Positive = if (is.null(t2)) NULL else "pos",
        test3 = NULL, test3Positive = NULL, ...)
    a <- decisioncombineClass$new(options = options, data = d)
    suppressMessages(suppressWarnings({
        a$init()
        a$run()
    }))
    a
}

.rf_row <- function(df, key) df[gsub('^"|"$', "", rownames(df)) == key, , drop = FALSE]
.rf_cell <- function(table, col, key) table$getCell(col, rowKey = key)$value

# Test 1 alone: TP 0, FP 1, FN 10, TN 99 -- a test that found no diseased patient.
# D (10): "-/+" 6, "-/-" 4.  N (100): "+/-" 1, "-/+" 10, "-/-" 89.
.rf_xa01 <- function(...) {
    .rf_object(.rf_mk(c("+/+" = 0, "+/-" = 0, "-/+" = 6, "-/-" = 4),
                      c("+/+" = 0, "+/-" = 1, "-/+" = 10, "-/-" = 89)), ...)
}

test_that("XA-01 an empty numerator gives LR+ and DOR of exactly 0, not the Haldane 3.06 / 3.16", {
    a <- .rf_xa01()
    res <- a$results
    row <- .rf_row(res$combinationTable$asDF, "Test 1 alone")
    expect_equal(unname(unlist(row[, c("tp", "fp", "fn", "tn")])), c(0, 1, 10, 99))
    # LR+ = 0 / (1/100) = 0; DOR = 0 * 99 / (1 * 10) = 0; LR- = (10/10) / (99/100) = 100/99.
    expect_identical(c(row$lrPos, row$dor), c(0, 0))
    expect_equal(row$lrNeg, 100 / 99, tolerance = 1e-12)
    # Not corrected, so not listed in the continuity notice. Updated 2026-10-21: its interval
    # is [0, U] (U from the +0.5 table) instead of blank, and the table note says so.
    ratios <- res$combinationTableCIRatios
    expect_identical(.rf_cell(ratios, "lower", "Test 1 alone_lrPos"), 0)
    expect_true(.rf_cell(ratios, "upper", "Test 1 alone_dor") > 0)
    expect_match(ratios$notes$zero_ratio_ci$note, "Test 1 alone", fixed = TRUE)
    expect_match(ratios$notes$zero_ratio_ci$note, "runs from 0 to the upper limit", fixed = TRUE)
    # No ratio in this fixture has an empty denominator with a non-empty numerator.
    expect_false(grepl("Continuity Correction", res$notices$content, fixed = TRUE))
})

test_that("XA-01 no displayed LR+ or DOR points the opposite way to the 2x2 it comes from", {
    tab <- .rf_xa01()$results$combinationTable$asDF
    sens <- tab$tp / (tab$tp + tab$fn)
    fpr <- tab$fp / (tab$fp + tab$tn)
    ok_lrp <- is.na(tab$lrPos) | sign(log(pmax(tab$lrPos, 1e-300))) == sign(sens - fpr) |
        tab$lrPos == 0
    ok_dor <- is.na(tab$dor) | sign(log(pmax(tab$dor, 1e-300))) ==
        sign(tab$tp * tab$tn - tab$fp * tab$fn) | tab$dor == 0
    expect_true(all(ok_lrp))
    expect_true(all(ok_dor))
})

test_that("XA-01 an empty denominator is still corrected, and the notes state the new rule", {
    # "+/+" has fp = 0, tp = 20: corrected cells 20.5, 0.5, 20.5, 60.5 -> LR+ 61, DOR 121.
    a <- .rf_object(.rf_mk(c("+/+" = 20, "+/-" = 8, "-/+" = 6, "-/-" = 6),
                           c("+/+" = 0, "+/-" = 7, "-/+" = 10, "-/-" = 43)))
    row <- .rf_row(a$results$combinationTable$asDF, "+/+")
    expect_equal(c(row$lrPos, row$dor), c(61, 121), tolerance = 1e-12)
    note <- a$results$combinationTable$notes$haldane$note
    expect_match(note, "A ratio is corrected only when the cell in its denominator is zero", fixed = TRUE)
    expect_match(note, "A zero numerator gives exactly 0, uncorrected", fixed = TRUE)
    expect_match(note, "a corrected value of 1 or below is left blank as not estimable", fixed = TRUE)
    expect_false(grepl("whenever any cell is zero", note, fixed = TRUE))
})

test_that("XA-01 a correction that would not exceed 1 is blank, not a number", {
    # Single test: TP 1, FP 0, FN 99, TN 1. Corrected LR+ = (1.5/101)/(0.5/2) = 0.059 < 1 and
    # corrected DOR = 1.5*1.5/(0.5*99.5) = 0.045 < 1 -- both "undefined", so blank.
    d <- data.frame(gold = factor(c(rep("D", 100), "N"), levels = c("D", "N")),
                    t1 = factor(c("pos", rep("neg", 99), "neg"), levels = c("pos", "neg")))
    a <- .rf_object(d, t2 = NULL)
    tab <- a$results$combinationTable
    expect_identical(.rf_cell(tab, "lrPos", "Test 1"), "")
    expect_identical(.rf_cell(tab, "dor", "Test 1"), "")
    expect_true(is.na(.rf_row(tab$asDF, "Test 1")$lrPos))
})

test_that("DC-01 undefined-by-design cells are empty strings, never a numeric NA", {
    # Paired comparison: the p of every Parallel/Serial row is blank by design.
    a <- .rf_object(.rf_mk(c("+/+" = 20, "+/-" = 8, "-/+" = 6, "-/-" = 6),
                           c("+/+" = 3, "+/-" = 7, "-/+" = 10, "-/-" = 40)),
                    showPairedComparison = TRUE)
    paired <- a$results$pairedComparisonTable
    expect_identical(.rf_cell(paired, "sensP", "parallel_1"), "")
    expect_identical(.rf_cell(paired, "specP", "serial_2"), "")
    # Values that exist keep their numeric type: Parallel gains 6 of 40 over Test 1.
    expect_equal(.rf_cell(paired, "sensChange", "parallel_1"), 6 / 40, tolerance = 1e-12)

    # Empty pattern "+/+" (nobody shows it): LR+, DOR and PPV are 0/0 -> "" ; LR- exactly 1.
    b <- .rf_xa01()
    comb <- b$results$combinationTable
    for (col in c("lrPos", "dor", "ppv")) {
        expect_identical(.rf_cell(comb, col, "+/+"), "")
    }
    expect_equal(.rf_cell(comb, "lrNeg", "+/+"), 1)
    # $asDF reads "" back as NA, so the ranking and the plots still see a missing value.
    expect_true(is.na(.rf_row(comb$asDF, "+/+")$lrPos))
    expect_true(is.numeric(comb$asDF$lrPos))
})

test_that("M1 an empty notices panel is '<div></div>', which the client does not skip", {
    a <- .rf_xa01()
    priv <- a$.__enclos_env__$private
    priv$.noticeList <- list()
    priv$.renderNotices()
    expect_identical(a$results$notices$content, "<div></div>")
})

test_that("XA-04 the proportions CI table names the Wilson method", {
    note <- .rf_xa01()$results$combinationTableCI$notes$method$note
    expect_match(note, "Wilson score 95% confidence intervals", fixed = TRUE)
    expect_match(note, "Clopper-Pearson", fixed = TRUE)
})

test_that("DC-02 About: correlation lowers only the metric each strategy targets", {
    about <- .rf_xa01(showAbout = TRUE)$results$about$content
    expect_false(grepl("usually be worse", about, fixed = TRUE))
    expect_match(about, "Parallel raises sensitivity less, Serial raises specificity less",
                 fixed = TRUE)
})

# Upper limit of a zero ratio's interval, by hand: Simel (LRs) / Woolf (DOR) log-scale interval on
# the table with 0.5 added to every cell. Same oracle as the decision and decisioncalculator tests.
.rf_zero_upper <- function(ratio, TP, FP, FN, TN) {
    a <- TP + .5; b <- FP + .5; c <- FN + .5; d <- TN + .5; z <- qnorm(.975)
    switch(ratio,
        lrPos = exp(log((a / (a + c)) / (b / (b + d))) + z * sqrt(1 / a - 1 / (TP + FN + 1) + 1 / b - 1 / (FP + TN + 1))),
        lrNeg = exp(log((c / (a + c)) / (d / (b + d))) + z * sqrt(1 / c - 1 / (TP + FN + 1) + 1 / d - 1 / (FP + TN + 1))),
        dor = exp(log(a * d / (b * c)) + z * sqrt(1 / a + 1 / b + 1 / c + 1 / d)))
}

test_that("zero ratio interval is [0, U], U on the 0.5-corrected table (hand oracle, 2026-10-21)", {
    # Test 1 alone: TP 50 FP 5 FN 0 TN 45 -> LR- = 0, U = 0.17360.
    ratios <- .rf_object(.rf_mk(c("+/+" = 25, "+/-" = 25, "-/+" = 0, "-/-" = 0),
                                c("+/+" = 2, "+/-" = 3, "-/+" = 10, "-/-" = 35)))$results$combinationTableCIRatios
    expect_identical(c(.rf_cell(ratios, "estimate", "Test 1 alone_lrNeg"),
                       .rf_cell(ratios, "lower", "Test 1 alone_lrNeg")), c(0, 0))
    expect_equal(.rf_cell(ratios, "upper", "Test 1 alone_lrNeg"),
                 .rf_zero_upper("lrNeg", 50, 5, 0, 45), tolerance = 1e-10)
    # Test 1 alone: TP 0 FP 5 FN 10 TN 85 -> LR+ = 0 and DOR = 0.
    ratios <- .rf_object(.rf_mk(c("+/+" = 0, "+/-" = 0, "-/+" = 4, "-/-" = 6),
                                c("+/+" = 2, "+/-" = 3, "-/+" = 10, "-/-" = 75)))$results$combinationTableCIRatios
    for (k in c("lrPos", "dor")) {
        key <- paste0("Test 1 alone_", k)
        expect_identical(c(.rf_cell(ratios, "estimate", key), .rf_cell(ratios, "lower", key)), c(0, 0))
        expect_equal(.rf_cell(ratios, "upper", key), .rf_zero_upper(k, 0, 5, 10, 85), tolerance = 1e-10)
    }
})
