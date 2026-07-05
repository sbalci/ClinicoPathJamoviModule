# Validation of chisqposttest against established base-R references.
# ---------------------------------------------------------------------------
# chisqposttest ships flagged in its own 0000.yaml: "No automated validation
# against established packages exists. Use with caution for clinical decision-
# making." This test removes that gap for its two headline outputs by pinning
#   (1) the omnibus chi-square  -> stats::chisq.test(correct = FALSE)
#   (2) the pairwise p-values   -> stats::chisq.test on each subtable
#   (3) the multiple-testing correction -> stats::p.adjust
# and checks that degenerate (zero-margin) tables are rejected with an actionable
# message rather than emitting NaN statistics.

# Deterministic 3x3 table (as weighted/count data). All cell counts are large
# enough that every 2x3 / 3x2 pairwise subtable has expected counts >= 5, so the
# module uses the chi-square test throughout (no Fisher fallback to reason about).
.chisq_validation_data <- function() {
  d <- expand.grid(rowvar = c("R1", "R2", "R3"),
                   colvar = c("C1", "C2", "C3"),
                   stringsAsFactors = TRUE)
  # expand.grid varies rowvar fastest, so this is column-major over the 3x3 table:
  #        C1  C2  C3
  #  R1    30  20  10
  #  R2    15  25  20
  #  R3    10  20  30
  d$n <- c(30, 15, 10,   20, 25, 20,   10, 20, 30)
  d
}

test_that("omnibus chi-square matches stats::chisq.test", {
  d <- .chisq_validation_data()
  tab <- xtabs(n ~ rowvar + colvar, data = d)
  ref <- chisq.test(tab, correct = FALSE)

  res <- chisqposttest(data = d, rows = "rowvar", cols = "colvar",
                       counts = "n", posthoc = "bonferroni")
  cs <- res$chisqTable$asDF

  expect_equal(as.numeric(cs$value[1]), as.numeric(ref$statistic), tolerance = 1e-6)
  expect_equal(as.integer(cs$df[1]),    as.integer(ref$parameter))
  expect_equal(as.numeric(cs$p[1]),     as.numeric(ref$p.value),   tolerance = 1e-6)
})

test_that("pairwise raw p-values and Bonferroni adjustment match base R", {
  d <- .chisq_validation_data()
  tab <- xtabs(n ~ rowvar + colvar, data = d)

  res <- chisqposttest(data = d, rows = "rowvar", cols = "colvar",
                       counts = "n", posthoc = "bonferroni")
  ph <- res$posthocTable$asDF
  expect_gt(nrow(ph), 0)

  # design guarantees expected >= 5 everywhere -> all comparisons use chi-square
  expect_true(all(grepl("chi", ph$test_method, ignore.case = TRUE)))

  # (3) adjusted p == stats::p.adjust of the reported raw p (Bonferroni)
  expect_equal(as.numeric(ph$padj),
               p.adjust(as.numeric(ph$p), method = "bonferroni"),
               tolerance = 1e-6)

  # (2) each reported raw p == chisq.test on the reconstructed subtable
  rn <- rownames(tab); cn <- colnames(tab)
  for (i in seq_len(nrow(ph))) {
    parts <- trimws(strsplit(ph$comparison[i], "vs", fixed = TRUE)[[1]])
    expect_length(parts, 2)
    a <- parts[1]; b <- parts[2]
    sub <- if (all(c(a, b) %in% rn)) tab[c(a, b), , drop = FALSE]
           else                      tab[, c(a, b), drop = FALSE]
    ref_p <- chisq.test(sub, correct = FALSE)$p.value
    expect_equal(as.numeric(ph$p[i]), as.numeric(ref_p), tolerance = 1e-6,
                 info = paste("comparison:", ph$comparison[i]))
  }
})

test_that("a zero-margin table is rejected, never returns NaN statistics", {
  # 2x3 table where column C3 has no observations in either row. dim() is 2x3 so
  # the existing < 2 dimension check passes, but the column margin is zero, which
  # makes chi-square undefined -- the guard must reject rather than emit NaN.
  d <- data.frame(
    rowvar = factor(c("R1", "R1", "R1", "R2", "R2", "R2")),
    colvar = factor(c("C1", "C2", "C3", "C1", "C2", "C3"),
                    levels = c("C1", "C2", "C3")),
    n = c(20, 15, 0, 18, 12, 0)
  )
  res <- tryCatch(
    chisqposttest(data = d, rows = "rowvar", cols = "colvar", counts = "n"),
    error = function(e) e)

  if (inherits(res, "error")) {
    # Preferred: rejected with an actionable message.
    expect_match(conditionMessage(res), "no observations|undefined|empty|at least 2",
                 ignore.case = TRUE)
  } else {
    # If the analysis still completes, it must NOT report NaN statistics.
    cs <- res$chisqTable$asDF
    expect_false(any(is.nan(as.numeric(cs$value))))
    expect_false(any(is.nan(as.numeric(cs$p))))
  }
})
