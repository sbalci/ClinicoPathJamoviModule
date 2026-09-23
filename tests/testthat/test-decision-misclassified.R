# ═══════════════════════════════════════════════════════════
# Regression tests: decision -- misclassified cases & saved classifications
#
# The four confusionMatrixSummary rows are added in .init(), so asserting that
# the table has 4 rows with the right labels proves nothing about .run() (and
# test-meddecide-library-audit.R already pins those labels). These blocks pin
# the COUNTS instead, against a 2x2 computed by hand from the shipped fixture,
# and pin the row alignment of the saveClassifications Output under a filter.
# ═══════════════════════════════════════════════════════════
library(testthat)
data("histopathology", package = "ClinicoPath")

# Hand-computed from histopathology (n = 250, no missing in either column):
#           New Test = 0   New Test = 1
#   Gold 0      115             37
#   Gold 1       28             70
# => TP 70, FP 37, FN 28, TN 115.
EXPECTED <- list(tp = 70L, fp = 37L, fn = 28L, tn = 115L, n = 250L)

# Per-row truth, using the same rule as .analyzeMisclassifiedCases():
# anything that is not the positive level counts as negative.
classify <- function(gold, test) {
    gold <- as.character(gold)
    test <- as.character(test)
    ifelse(test == "1",
           ifelse(gold == "1", "True Positive",  "False Positive"),
           ifelse(gold == "1", "False Negative", "True Negative"))
}

TRUTH <- classify(histopathology[["Golden Standart"]], histopathology[["New Test"]])


test_that("the histopathology fixture still has the columns these tests rely on", {
    # If the dataset is renamed or recoded, fail here rather than letting the
    # blocks below fail for a reason that looks like a backend defect.
    expect_true(all(c("Golden Standart", "New Test") %in% names(histopathology)))
    expect_setequal(as.character(histopathology[["Golden Standart"]]), c("0", "1"))
    expect_setequal(as.character(histopathology[["New Test"]]), c("0", "1"))
    expect_equal(nrow(histopathology), EXPECTED$n)
})


test_that("showMisclassified reports the true confusion-matrix counts", {
    # Row keys and labels are the .init() scaffold and are covered by
    # test-meddecide-library-audit.R; this block is only about the numbers.
    result <- decision(
        data = histopathology,
        gold = "Golden Standart", goldPositive = "1", goldNegative = "0",
        newtest = "New Test",     testPositive = "1", testNegative = "0",
        showMisclassified = TRUE
    )

    cm <- as.data.frame(result$confusionMatrixSummary)
    counts <- setNames(cm$count, cm$classification)

    expect_equal(counts[["True Positive"]],  EXPECTED$tp)
    expect_equal(counts[["False Positive"]], EXPECTED$fp)
    expect_equal(counts[["False Negative"]], EXPECTED$fn)
    expect_equal(counts[["True Negative"]],  EXPECTED$tn)

    # The four cells partition the analysed cases: no row may be counted twice
    # or dropped, and `percentage` must be a proportion of that same total.
    expect_equal(sum(counts), EXPECTED$n)
    expect_equal(sum(cm$percentage), 1)
    expect_equal(cm$percentage, cm$count / EXPECTED$n)
})


test_that("the misclassified-case tables list every disagreement", {
    # No maxCasesShow here on purpose: the default cap (50) is above both
    # counts, so nothing is truncated and the test stays independent of the
    # cap's schema limits. Truncation is covered by the next block.
    result <- decision(
        data = histopathology,
        gold = "Golden Standart", goldPositive = "1", goldNegative = "0",
        newtest = "New Test",     testPositive = "1", testNegative = "0",
        showMisclassified = TRUE
    )

    fp <- as.data.frame(result$falsePositiveTable)
    fn <- as.data.frame(result$falseNegativeTable)

    expect_equal(nrow(fp), EXPECTED$fp)
    expect_equal(nrow(fn), EXPECTED$fn)

    # case_id must be the spreadsheet row, so a clinician can find the patient.
    expect_setequal(fp$case_id, which(TRUTH == "False Positive"))
    expect_setequal(fn$case_id, which(TRUTH == "False Negative"))

    # And the printed values must be the levels that made it a disagreement.
    expect_true(all(fp$test_value == "1"))
    expect_true(all(fp$gold_value == "0"))
    expect_true(all(fn$test_value == "0"))
    expect_true(all(fn$gold_value == "1"))
})


test_that("maxCasesShow truncates the case tables from the top", {
    # 10 is the schema minimum and is below both counts (37 FP, 28 FN), so the
    # truncation branch runs. The note says "Showing first N", so the rows kept
    # must be the first N in data order -- not an arbitrary N of them.
    result <- decision(
        data = histopathology,
        gold = "Golden Standart", goldPositive = "1", goldNegative = "0",
        newtest = "New Test",     testPositive = "1", testNegative = "0",
        showMisclassified = TRUE,
        maxCasesShow = 10
    )

    fp <- as.data.frame(result$falsePositiveTable)
    fn <- as.data.frame(result$falseNegativeTable)

    expect_equal(nrow(fp), 10)
    expect_equal(nrow(fn), 10)
    expect_equal(fp$case_id, head(which(TRUTH == "False Positive"), 10))
    expect_equal(fn$case_id, head(which(TRUTH == "False Negative"), 10))
})


test_that("saveClassifications writes each label against its own spreadsheet row", {
    # A real filter: only the even spreadsheet rows reach the analysis, as a
    # jamovi row filter would deliver them. as.data.frame() first -- a tibble
    # renumbers row names on subsetting, which would defeat the point.
    keep <- seq(2L, EXPECTED$n, by = 2L)
    filtered <- as.data.frame(histopathology)[keep, ]

    # saveClassifications is an Output option, so it is not a constructor
    # argument of the generated wrapper -- jamovi sets it when the user names an
    # output column. OptionOutput$value reads `isTRUE(private$.value$value)`.
    # showMisclassified stays FALSE: decision.b.R gates the analysis on
    # `showMisclassified || has_output_var`, and a TRUE on the left short-
    # circuits the has_output_var branch this block exists to guard.
    opts <- ClinicoPath:::decisionOptions$new(
        gold = "Golden Standart", goldPositive = "1", goldNegative = "0",
        newtest = "New Test",     testPositive = "1", testNegative = "0",
        showMisclassified = FALSE
    )
    # Two steps, not `opts$option(...)$value <- ...`: R's complex assignment
    # needs a `$<-` method on the returned object and OptionOutput has none.
    # The option is an R6 object, so assigning through the local binding
    # mutates the one held by `opts`.
    out_opt <- opts$option("saveClassifications")
    out_opt$value <- list(value = TRUE)
    expect_true(opts$saveClassifications)

    analysis <- ClinicoPath:::decisionClass$new(options = opts, data = filtered)
    analysis$run()

    out <- analysis$results$saveClassifications
    expect_true(out$isFilled())

    # .values / .rowNums have no public accessor; jamovi reads them over
    # protobuf. The invariant that matters is that they line up: value i is
    # written to spreadsheet row rowNums[i]. With a filter active that is NOT
    # the same as position i, which is what the last expectation pins.
    priv    <- out$.__enclos_env__$private
    rowNums <- priv$.rowNums
    values  <- as.character(priv$.values[[1]])

    expect_equal(rowNums, keep)
    expect_equal(values, TRUTH[rowNums])

    # Non-vacuity: 84 of the 125 filtered rows carry a different label from the
    # row at the same position, so a positional (unfiltered) rowNums would fail
    # the alignment check above rather than pass it by coincidence.
    expect_false(identical(values, TRUTH[seq_along(values)]))
})
