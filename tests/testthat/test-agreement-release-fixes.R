# Release fixes 2026-10-21 (agreement-independent-review-2026-10-21.md, AR-01..AR-03).

.ar_sub <- function(d, ...) {
    agreement_run(d, vars = setdiff(names(d), "site"), agreementBySubgroup = TRUE,
                  subgroupVariable = "site", ...)$results$subgroupAgreementTable
}

test_that("AR-03: a subgroup rated by 2 of 3 readers is estimated, not dropped", {
    skip_if_not_installed("irrCAC")
    withr::local_seed(3); n <- 80
    truth <- sample(1:3, n, TRUE)
    mk <- function() factor(pmin(3, pmax(1, truth + sample(-1:1, n, TRUE, prob = c(.15, .7, .15)))),
                            levels = 1:3, ordered = TRUE)
    d <- data.frame(R1 = mk(), R2 = mk(), R3 = mk(),
                    site = factor(rep(c("partial", "complete"), each = 40)))
    for (i in 1:40) d[i, sample(1:3, 1)] <- NA   # every "partial" case misses one rating
    tab <- .ar_sub(d)
    df <- tab$asDF
    expect_true("partial" %in% df$subgroup)
    row <- df[df$subgroup == "partial", ]
    codes <- as.data.frame(lapply(d[1:40, 1:3], as.integer))
    est <- irrCAC::fleiss.kappa.raw(codes, categ.labels = 1:3)$est
    expect_equal(row$agreement_stat, est$coeff.val, tolerance = 1e-4)
    expect_equal(row$n_cases, 40)
    expect_false("skipped" %in% names(agreement_notes(tab)))

    # Gate still applies, and the note names what it counts.
    tab <- .ar_sub(d, subgroupMinCases = 41)
    note <- agreement_notes(tab)[["skipped"]]
    expect_true(grepl("cases rated by at least two raters", note, fixed = TRUE))
    expect_false(grepl("complete cases", note, fixed = TRUE))
})

test_that("AR-01: 3+-rater subgroup table names its interval method; AR-02: no false 3+ clause", {
    skip_if_not_installed("irrCAC")
    withr::local_seed(5); n <- 120
    truth <- sample(1:4, n, TRUE)
    mk <- function() factor(pmin(4, pmax(1, truth + sample(-1:1, n, TRUE, prob = c(.2, .6, .2)))),
                            levels = 1:4, ordered = TRUE)
    d <- data.frame(R1 = mk(), R2 = mk(), R3 = mk(), R4 = mk(), site = factor(sample(c("A", "B", "C"), n, TRUE)))
    for (m in c("wald", "fisherz")) {
        tab <- .ar_sub(d, kappaCIMethod = m)
        note <- agreement_notes(tab)[["ci_method_fleiss"]]
        expect_true(grepl("standard error", note) && grepl("degrees of freedom", note) &&
                    grepl("t quantile", note), info = m)
        expect_true(grepl("Fisher z", note, fixed = TRUE))
    }
    # Fisher z must not change 3+-rater rows (the note says so).
    expect_equal(.ar_sub(d, kappaCIMethod = "wald")$asDF, .ar_sub(d, kappaCIMethod = "fisherz")$asDF)

    # Two raters: no Gwet note.
    tab2 <- .ar_sub(d[, c("R1", "R2", "site")])
    expect_false("ci_method_fleiss" %in% names(agreement_notes(tab2)))

    # AR-02: trigger the 2-rater null-SE fallback (perfect agreement, vcd ASE unavailable);
    # its footnote must not claim that three or more raters use it.
    lv <- c("neg", "pos")
    a <- c(rep("neg", 18), rep("pos", 2))
    withr::local_seed(7); b1 <- sample(lv, 30, TRUE); b2 <- ifelse(runif(30) < .8, b1, sample(lv, 30, TRUE))
    d2 <- data.frame(R1 = factor(c(a, b1), levels = lv), R2 = factor(c(a, b2), levels = lv),
                     site = factor(rep(c("A", "B"), c(20, 30))))
    n2 <- agreement_notes(.ar_sub(d2))
    expect_true("ci_null_se" %in% names(n2))
    expect_false(grepl("three or more raters", n2[["ci_null_se"]], fixed = TRUE))
})
