# Security review 2026-09-25: user-supplied names and labels in table notes, and the
# case-clustering size limit.
#
# A table note is rendered through jamovi's small HTML allow-list, so a rater name or a
# category label is markup unless escaped. .noteSafe() now escapes; 45 of its callers
# used to escape first and 26 did not. Each test below pins one side of that split.

test_that(".noteSafe escapes once, maps brackets and keeps NULL and NA", {
    a <- agreement_run(data.frame(R1 = factor(c("a", "b", "a")), R2 = factor(c("a", "b", "b"))),
                       vars = c("R1", "R2"))
    ns <- a$.__enclos_env__$private$.noteSafe
    expect_identical(ns("<b>A&B</b> [x]"), "&lt;b&gt;A&amp;B&lt;/b&gt; (x)")
    expect_null(ns(NULL))
    expect_identical(ns(c("<1%", NA)), c("&lt;1%", NA))
})

test_that("a free-text positive category and data labels are escaped in the not-found note", {
    set.seed(3)
    lv <- c("<1%", "1-49%", ">=50%")
    r1 <- factor(sample(lv, 30, TRUE), lv)
    r2 <- r1; r2[1:6] <- sample(lv, 6, TRUE)
    a <- agreement_run(data.frame(R1 = r1, R2 = r2), vars = c("R1", "R2"),
                       specificAgreement = TRUE, specificAllCategories = FALSE,
                       specificPositiveCategory = "<b>x</b>")
    note <- agreement_notes(a$results$specificAgreementTable)[["error"]]
    expect_true(nzchar(note))
    expect_match(note, "'&lt;b&gt;x&lt;/b&gt;' not found", fixed = TRUE)
    expect_match(note, "&lt;1%", fixed = TRUE)
    expect_match(note, "&gt;=50%", fixed = TRUE)
    expect_no_match(note, "<b>", fixed = TRUE)
    expect_no_match(note, "&amp;lt;", fixed = TRUE)
})

test_that("the reference rater name is escaped once in both of its notes", {
    set.seed(4)
    r1 <- factor(sample(c("G1", "G2", "G3"), 40, TRUE))
    r2 <- r1; r2[1:8] <- sample(c("G1", "G2", "G3"), 8, TRUE)
    ref <- "Ref <A&B>"
    esc <- "Ref &lt;A&amp;B&gt;"

    # Continuous reference: the type_error note, which used to print the name raw.
    d <- data.frame(R1 = r1, R2 = r2, rnorm(40, 50, 10), check.names = FALSE)
    names(d)[3] <- ref
    a <- agreement_run(d, vars = c("R1", "R2"), pairwiseKappa = TRUE, referenceRater = ref)
    note <- agreement_notes(a$results$pairwiseKappaTable)[["type_error"]]
    expect_match(note, esc, fixed = TRUE)
    expect_no_match(note, ref, fixed = TRUE)

    # Categorical reference also selected under Raters: the ref_in_raters note, which
    # already escaped before the helper did. It must not be escaped twice.
    d[[ref]] <- r1
    a <- agreement_run(d, vars = c("R1", ref), pairwiseKappa = TRUE, referenceRater = ref)
    note <- agreement_notes(a$results$pairwiseKappaTable)[["ref_in_raters"]]
    expect_match(note, esc, fixed = TRUE)
    expect_no_match(note, "&amp;lt;", fixed = TRUE)
})

test_that("case clustering refuses more than 5000 cases instead of building an n x n matrix", {
    set.seed(5)
    n <- 5001
    r1 <- factor(sample(c("G1", "G2", "G3"), n, TRUE))
    r2 <- r1; r2[1:500] <- sample(c("G1", "G2", "G3"), 500, TRUE)
    a <- agreement_run(data.frame(R1 = r1, R2 = r2), vars = c("R1", "R2"),
                       caseClustering = TRUE)
    note <- agreement_notes(a$results$caseClusterTable)[["error"]]
    expect_match(note, "limited to 5000 cases", fixed = TRUE)
    expect_match(note, "5001 cases were supplied", fixed = TRUE)
    # Without the limit this run builds the matrix (about 140 s) and fills 1000 rows.
    expect_equal(a$results$caseClusterTable$rowCount, 0)
})

# setError() text and table cells are shown as plain text (jamovi's client renders an
# item error with textContent), so an escaped message printed "&lt;" and "&amp;".
sec_error_of <- function(item) item$.__enclos_env__$private$.error

sec_failing_helper <- function(a, name) {
    penv <- a$.__enclos_env__$private
    unlockBinding(name, penv)
    assign(name, function(...) stop("bad <1% & x"), envir = penv)
    penv
}

test_that("plot errors show the message as written, not HTML-escaped", {
    set.seed(6)
    r1 <- factor(sample(c("G1", "G2", "G3"), 30, TRUE))
    r2 <- r1; r2[1:6] <- sample(c("G1", "G2", "G3"), 6, TRUE)
    ratings <- data.frame(R1 = r1, R2 = r2)
    a <- agreement_run(ratings, vars = c("R1", "R2"))
    penv <- sec_failing_helper(a, ".ratingsAreContinuous")

    penv$.populateAgreementHeatmap(ratings)
    expect_identical(sec_error_of(a$results$agreementHeatmapPlot),
                     "Error generating heatmap: bad <1% & x")
    penv$.populateRaterProfiles(ratings)
    expect_identical(sec_error_of(a$results$raterProfilePlot),
                     "Error generating rater profile plot: bad <1% & x")
})

test_that("the homogeneity test's failure reason is not HTML-escaped in its table cell", {
    set.seed(11)
    site <- rep(c("A", "B", "C", "D"), each = 10)
    true <- rnorm(40, 50, 8) + c(A = 0, B = 4, C = -3, D = 6)[site]
    d <- data.frame(R1 = true + rnorm(40, 0, 2), R2 = true + 1 + rnorm(40, 0, 2),
                    R3 = true - 1 + rnorm(40, 0, 2), site = factor(site))
    # Fail only the reduced-model refit inside the homogeneity test; every other
    # .quietly() call (the model fits themselves) runs as normal.
    testthat::local_mocked_bindings(
        .quietly = function(expr, ...) {
            if (any(grepl("update(", deparse(substitute(expr)), fixed = TRUE)))
                stop("bad <1% & x")
            expr
        },
        .package = "ClinicoPath")
    a <- agreement_run(d, vars = c("R1", "R2", "R3"), hierarchicalKappa = TRUE,
                       clusterVariable = "site", testClusterHomogeneity = TRUE)
    expect_identical(a$results$homogeneityTestTable$asDF$conclusion[1],
                     "Test could not be computed: bad <1% & x")
})

test_that("no caller escapes a value before .noteSafe(), which now escapes it", {
    src <- testthat::test_path("..", "..", "R", "agreement.b.R")
    skip_if_not(file.exists(src), "package source not available")
    expect_false(any(grepl("noteSafe(jmvcore::htmlEscape(", readLines(src, warn = FALSE), fixed = TRUE)))
})
