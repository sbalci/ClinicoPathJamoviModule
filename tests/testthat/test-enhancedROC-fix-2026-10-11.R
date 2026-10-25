# /fix-function --apply pass 2026-10-11 (TODO.md, "enhancedROC - /fix-function --apply").
# One test_that per defect, each written to FAIL on the code before its fix.

fx_run <- function(data, ..., .fatal = FALSE) {
    options <- do.call(enhancedROCOptions$new, list(...))
    analysis <- enhancedROCClass$new(options = options, data = data)
    suppressWarnings(analysis$run(noThrow = .fatal))
    if (.fatal) {
        expect_identical(analysis$status, "error")
        expect_true(analysis$errored)
        expect_false(analysis$complete)
    }
    analysis
}
fx_notices <- function(analysis, title) {
    Filter(function(n) identical(n$title, title), analysis$.__enclos_env__$private$.noticeList)
}
fx_asah <- function() {
    skip_if_not_installed("pROC")
    d <- pROC::aSAH
    d$wfns <- as.numeric(as.character(d$wfns))
    d
}

test_that("X-01: the Detailed Cutoff Analysis lists only usable (finite) cut-offs", {
    a <- fx_run(fx_asah(), outcome = "outcome", positiveClass = "Poor", predictors = c("s100b", "wfns"),
                cutoffTable = TRUE)
    df <- a$results$results$cutoffAnalysis$asDF
    expect_true(all(is.finite(df$cutoff)))
    # sampling still spans the curve: both predictors keep their sampled rows
    for (p in c("s100b", "wfns")) expect_gt(sum(df$predictor == p & df$cutoff_type == "Sampled"), 3L)
})

test_that("X-02: an invalid Partial AUC range warns and skips only the partial AUC", {
    # library-audit 2026-10-23 meddecide [MEDIUM] DONE: two Number options; only their order can be wrong
    for (rng in list(c(0.9, 0.8), c(0.8, 0.8))) {
        a <- fx_run(fx_asah(), outcome = "outcome", positiveClass = "Poor", predictors = "s100b",
                    partialAuc = TRUE, partialAucFrom = rng[1], partialAucTo = rng[2],
                    cutoffTable = TRUE, comprehensive_output = TRUE)
        r <- a$results$results
        notice <- fx_notices(a, "Invalid Partial AUC Range")
        expect_length(notice, 1L)
        expect_identical(notice[[1]]$type, "WARNING", info = paste(rng, collapse = ","))
        expect_gt(r$aucSummary$rowCount, 0L)
        expect_gt(r$cutoffAnalysis$rowCount, 0L)
        expect_equal(r$partialAucAnalysis$rowCount, 0)
        expect_false(grepl("partial AUC", r$methodsExplanation$content, ignore.case = TRUE), info = paste(rng, collapse = ","))
    }
    ok <- fx_run(fx_asah(), outcome = "outcome", positiveClass = "Poor", predictors = "s100b",
                 partialAuc = TRUE, partialAucFrom = 0.8, partialAucTo = 1.0, comprehensive_output = TRUE)
    expect_length(fx_notices(ok, "Invalid Partial AUC Range"), 0L)
    expect_equal(ok$results$results$partialAucAnalysis$rowCount, 1)
    expect_match(ok$results$results$methodsExplanation$content, "partial AUC", fixed = TRUE)   # the check above is not vacuous
})

test_that("X-03: a frozen-panel input problem reaches the engine and retains a translated ERROR notice", {
    set.seed(5)
    d <- data.frame(y = factor(sample(c("case", "ctrl"), 80, TRUE)), p = runif(80))
    frozen <- function(...) fx_run(d, outcome = "y", positiveClass = "case", predictors = "p",
                                   validationPanel = TRUE, direction = "higher", .fatal = TRUE, ...)
    no_negative <- expect_no_error(frozen(modelVersion = "v1"))
    n <- fx_notices(no_negative, "Frozen-Model Evaluation Not Run")
    expect_length(n, 1L)
    expect_identical(n[[1]]$type, "ERROR")
    expect_match(n[[1]]$content, "negative reference level", fixed = TRUE)

    no_version <- frozen(validationNegative = "ctrl")
    expect_match(fx_notices(no_version, "Frozen-Model Evaluation Not Run")[[1]]$content, "model version", fixed = TRUE)

    # the dynamic message keeps the offending levels
    d$y <- factor(ifelse(seq_len(80) == 1, "unknown", as.character(d$y)))
    unmapped <- frozen(validationNegative = "ctrl", modelVersion = "v1")
    expect_match(fx_notices(unmapped, "Frozen-Model Evaluation Not Run")[[1]]$content, "unknown", fixed = TRUE)
})
