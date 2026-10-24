# Regression tests for the 2026-10-21 independent review of decisioncompare (meddecide 1.1.1):
# IR2-decisioncompare-01..05. Expected values are hand fractions from the review, not the
# module's own arithmetic.

rf_dc <- function(data, ...) {
    args <- utils::modifyList(list(
        gold = "gold", goldPositive = "D", goldNegative = "N",
        test1 = "t1", test1Positive = "+", test1Negative = "-",
        test2 = "t2", test2Positive = "+", test2Negative = "-",
        test3 = NULL, test3Positive = NULL, test3Negative = NULL, stratify = NULL),
        list(...), keep.null = TRUE)
    opts <- do.call(ClinicoPath:::decisioncompareOptions$new, args)
    a <- ClinicoPath:::decisioncompareClass$new(options = opts, data = data)
    a$init()
    err <- tryCatch({ suppressWarnings(a$run()); NULL }, error = function(e) conditionMessage(e))
    list(res = a$results, error = err)
}
rf_text <- function(x) {
    x <- gsub("<[^>]+>", " ", paste(x, collapse = " "))
    for (e in list(c("&#x2F;", "/"), c("&#x27;", "'"), c("&quot;", "\""), c("&lt;", "<"),
                   c("&gt;", ">"), c("&amp;", "&")))
        x <- gsub(e[1], e[2], x, fixed = TRUE)
    trimws(gsub("[[:space:]]+", " ", x))
}
# t1 carries the 2x2 under test; t2 is a fixed informative second test.
rf_2x2 <- function(TP, FP, FN, TN) {
    n <- TP + FP + FN + TN
    d <- data.frame(gold = factor(rep(c("D", "N", "D", "N"), c(TP, FP, FN, TN)), levels = c("D", "N")),
                    t1 = factor(rep(c("+", "+", "-", "-"), c(TP, FP, FN, TN)), levels = c("+", "-")))
    d$t2 <- factor(ifelse(seq_len(n) %% 3 == 0, "-", "+"), levels = c("+", "-"))
    d
}
rf_row <- function(res, key = "test_1") {
    ct <- res$comparisonTable$asDF
    ct[gsub('^"|"$', "", rownames(ct)) == key, , drop = FALSE]
}

test_that("IR2-01: LR+ is exactly 0 for TP = 0, FP > 0, with no correction notice", {
    r <- rf_dc(rf_2x2(0, 3, 10, 80), fnote = TRUE)
    expect_null(r$error)
    expect_identical(rf_row(r$res)$LRP, 0)                          # 0 / (3/83)
    expect_equal(rf_row(r$res)$LRN, (10 / 10) / (80 / 83), tolerance = 1e-12)
    expect_false(grepl("Zero Cell Continuity Correction", r$res$notices$content, fixed = TRUE))
})

test_that("IR2-01: LR- is exactly 0 for FN = 0, TN > 0, and the report prints 0.00", {
    r <- rf_dc(rf_2x2(50, 10, 0, 40), statComp = TRUE, showDescriptiveReport = TRUE)
    expect_null(r$error)
    expect_equal(rf_row(r$res)$LRP, 5, tolerance = 1e-12)           # 1 / (10/50)
    expect_identical(rf_row(r$res)$LRN, 0)
    expect_false(grepl("Zero Cell Continuity Correction", r$res$notices$content, fixed = TRUE))
    expect_match(rf_text(r$res$clinicalReport$content),
                 "for negative results was 0.00", fixed = TRUE)
})

test_that("IR2-01: an empty denominator is still corrected, and only that ratio", {
    r <- rf_dc(rf_2x2(50, 0, 5, 45))
    expect_equal(rf_row(r$res)$LRP, 2323 / 28, tolerance = 1e-12)   # (50.5/56) / (0.5/46)
    expect_equal(rf_row(r$res)$LRN, 1 / 11, tolerance = 1e-12)      # raw
    expect_match(r$res$notices$content, "Zero Cell Continuity Correction", fixed = TRUE)
    r <- rf_dc(rf_2x2(55, 5, 5, 0))
    expect_equal(rf_row(r$res)$LRN, (5.5 / 61) / (0.5 / 6), tolerance = 1e-12)
})

test_that("IR2-01: an infinite ratio whose correction would fall to 1 or below is blank, not 0/0", {
    # TP 99 FN 1 FP 10 TN 0: LR- = (1/100)/(0/10) is infinite; corrected (1.5/101)/(0.5/11) = 0.33
    # would read as rule-out evidence, so .diagRatioRules() returns "undefined".
    r <- rf_dc(rf_2x2(99, 10, 1, 0))
    expect_null(r$error)
    lrn <- rf_row(r$res)$LRN
    expect_true(identical(lrn, "") || is.na(lrn))
    foot <- paste(r$res$comparisonTable$getCell(rowKey = "test_1", col = "LRN")$footnotes, collapse = " ")
    expect_match(foot, "TN = 0 makes LR- infinite", fixed = TRUE)
    expect_false(grepl("never negative", foot, fixed = TRUE))
    expect_match(rf_text(r$res$notices$content), "no true negative was observed, so LR- is infinite", fixed = TRUE)
})

# Fixture P (partial testing): 40 D / 40 N. t1 was not done on 12 diseased cases on which t2 is
# negative. Own rows: t1 Se 22/28 (n 68), t2 Se 27/40 (n 80), Sp 0.9 both, so t1 leads on
# standalone balanced accuracy. On the 68 common rows t2 leads: (27/28 + 0.9)/2 vs (22/28 + 0.9)/2.
rf_fixP <- function() {
    t1D <- c(rep("+", 22), rep("-", 6), rep(NA, 12))
    t2D <- c(rep("+", 27), "-", rep("-", 12))
    neg <- c(rep("-", 36), rep("+", 4))
    data.frame(gold = factor(rep(c("D", "N"), each = 40), levels = c("D", "N")),
               t1 = factor(c(t1D, neg), levels = c("+", "-")),
               t2 = factor(c(t2D, neg), levels = c("+", "-")))
}

test_that("IR2-02: with unequal case sets the ranking uses the common cases and says so", {
    r <- rf_dc(rf_fixP(), statComp = TRUE, showDescriptiveReport = TRUE)
    expect_null(r$error)
    # standalone figures are unchanged
    expect_equal(rf_row(r$res, "test_1")$Sens, 22 / 28, tolerance = 1e-12)
    expect_equal(rf_row(r$res, "test_2")$Sens, 27 / 40, tolerance = 1e-12)
    results <- sub(".*Results Section:", "", rf_text(r$res$clinicalReport$content))
    expect_match(results, "t2 had the highest observed balanced accuracy", fixed = TRUE)
    expect_false(grepl("t1 had the highest observed balanced accuracy", results, fixed = TRUE))
    expect_match(results, "on the 68 cases with a determinate result for every test", fixed = TRUE)
    # the conflict sentence (prevalence weighting) no longer fires: t2 also wins on accuracy
    expect_false(grepl("classified significantly fewer cases correctly", results, fixed = TRUE))
})

test_that("IR2-02: the three-test Summary and Report rank on the common cases too", {
    # The two-test Summary and Report name no ranking; with three tests and a significant
    # Cochran's Q they do. t3 calls every diseased case negative.
    d <- rf_fixP()
    d$t3 <- factor(ifelse(d$gold == "D", "-", as.character(d$t2)), levels = c("+", "-"))
    r <- rf_dc(d, test3 = "t3", test3Positive = "+", test3Negative = "-", statComp = TRUE,
               showSummary = TRUE, showReportSentence = TRUE)
    expect_null(r$error)
    for (panel in c("summaryReport", "reportSentence")) {
        txt <- rf_text(r$res[[panel]]$content)
        expect_match(txt, "t2 had the highest observed balanced accuracy", fixed = TRUE, info = panel)
        expect_false(grepl("t1 had the highest observed balanced accuracy", txt, fixed = TRUE), info = panel)
        expect_match(txt, "68 cases with a determinate result for every test", fixed = TRUE, info = panel)
    }
})

test_that("IR2-02: equal case sets keep the standalone ranking and add no basis sentence", {
    d <- rf_fixP(); d$t1[is.na(d$t1)] <- "-"
    r <- rf_dc(d, statComp = TRUE, showDescriptiveReport = TRUE)
    expect_false(grepl("different sets of cases", rf_text(r$res$clinicalReport$content), fixed = TRUE))
})

test_that("IR2-03: the prevalence sentence names its denominator when it is not the sample", {
    # ranked test t2 uses every row: the sample prevalence 50.0%
    r <- rf_dc(rf_fixP(), statComp = TRUE, showDescriptiveReport = TRUE)
    expect_match(rf_text(r$res$clinicalReport$content),
                 "At the 50.0% disease prevalence observed in this sample", fixed = TRUE)
    # only t1 is ranked-and-incomplete when t2 is made the weaker test on every case set
    d <- rf_fixP(); d$t2 <- factor(ifelse(d$gold == "D", "-", "+"), levels = c("+", "-"))
    r <- rf_dc(d, statComp = TRUE, showDescriptiveReport = TRUE)
    txt <- rf_text(r$res$clinicalReport$content)
    expect_match(txt, "At the 41.2% disease prevalence among the 68 cases analyzed for t1", fixed = TRUE)
    expect_false(grepl("41.2% disease prevalence observed in this sample", txt, fixed = TRUE))
})

test_that("IR2-04: variables chosen before their levels give a quiet INFO, not an error", {
    r <- rf_dc(rf_fixP(), goldPositive = NULL, goldNegative = NULL,
               test1Positive = NULL, test1Negative = NULL,
               test2Positive = NULL, test2Negative = NULL)
    expect_null(r$error)
    notices <- rf_text(r$res$notices$content)
    expect_match(notices, "Levels Not Yet Selected", fixed = TRUE)
    expect_match(notices, "Reference positive level, Reference negative level, Test 1 positive level", fixed = TRUE)
    expect_false(grepl("Validation failed|Missing Positive Levels", notices))
    # library-audit 2026-10-23 meddecide [MEDIUM] DONE: rows come from .init(); nothing is computed
    expect_true(all(is.na(r$res$comparisonTable$asDF$Sens)))
    # The run stops early, so the reports reset above it must REPLACE old content: an Html ""
    # is skipped by the jamovi client, and NA_real_ cells render "NaN" (Codex review).
    expect_identical(r$res$summaryReport$content, "<div></div>")
    # library-audit 2026-10-23 meddecide [HIGH] DONE: original data are Tables now (goldFrequency, originalCrossTab)
    expect_equal(r$res$originalCrossTab$rowCount, 0)
    expect_identical(r$res$cTable1$getCell(rowKey = "Test Positive", "GP")$value, "")
})

test_that("IR2-05: the extra-level error names the current checkbox title", {
    d <- rf_fixP(); d$t1 <- factor(ifelse(is.na(d$t1), "ND", as.character(d$t1)))
    r <- rf_dc(d)
    expect_false(is.null(r$error))
    notices <- rf_text(r$res$notices$content)
    expect_match(notices, "Select 'Selected positive and negative levels only'", fixed = TRUE)
    expect_false(grepl("Exclude other observed levels", notices, fixed = TRUE))
})
