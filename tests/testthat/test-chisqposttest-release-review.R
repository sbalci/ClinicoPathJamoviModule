# Regression cover for the defects found during the chisqposttest release review.
# The seeding and warning blocks fail against the pre-review backend; the rest
# pin the statistics the review verified by hand.

library(testthat)

# 3x3 with a strong diagonal association, so the omnibus test is significant and
# post-hoc comparisons actually run
cq_data <- function() {
  set.seed(21)
  rows <- rep(c("R1", "R2", "R3"), each = 60)
  cols <- c(sample(c("C1", "C2", "C3"), 60, TRUE, c(.7, .15, .15)),
            sample(c("C1", "C2", "C3"), 60, TRUE, c(.15, .7, .15)),
            sample(c("C1", "C2", "C3"), 60, TRUE, c(.15, .15, .7)))
  data.frame(rows = factor(rows), cols = factor(cols))
}

cq <- function(d, ...) chisqposttest(data = d, rows = "rows", cols = "cols",
                                     counts = NULL, ...)

test_that("pairwise p-values and Bonferroni adjustment match hand computation", {
  d <- cq_data()
  tab <- table(d$rows, d$cols)
  df <- cq(d, posthoc = "bonferroni")$posthocTable$asDF

  # 3 row pairs + 3 column pairs
  expect_equal(nrow(df), 6L)

  ps <- c(); chis <- c(); labs <- c()
  rn <- rownames(tab); cn <- colnames(tab)
  for (i in 1:2) for (j in (i + 1):3) {
    ct <- chisq.test(tab[c(i, j), , drop = FALSE], correct = FALSE)
    ps <- c(ps, ct$p.value); chis <- c(chis, unname(ct$statistic))
    labs <- c(labs, paste(rn[i], "vs", rn[j]))
  }
  for (i in 1:2) for (j in (i + 1):3) {
    ct <- chisq.test(tab[, c(i, j), drop = FALSE], correct = FALSE)
    ps <- c(ps, ct$p.value); chis <- c(chis, unname(ct$statistic))
    labs <- c(labs, paste(cn[i], "vs", cn[j]))
  }

  expect_equal(as.character(df$comparison), labs)
  expect_equal(df$chi, chis, tolerance = 1e-8)
  expect_equal(df$p, ps, tolerance = 1e-10)
  # the adjustment must use m = 6, i.e. every comparison performed
  expect_equal(df$padj, p.adjust(ps, "bonferroni"), tolerance = 1e-10)
})

test_that("the comparison count is the sum over both dimensions", {
  ns <- asNamespace("ClinicoPath")
  priv <- get("chisqposttestClass", ns)$new(
    options = get("chisqposttestOptions", ns)$new(rows = "rows", cols = "cols"),
    data = cq_data())$.__enclos_env__$private

  expect_equal(priv$.pairwiseComparisonCount(matrix(0, 3, 3)), 6)   # 3 + 3
  expect_equal(priv$.pairwiseComparisonCount(matrix(0, 2, 2)), 2)   # 1 + 1
  expect_equal(priv$.pairwiseComparisonCount(matrix(0, 4, 2)), 7)   # 6 + 1
  expect_equal(priv$.pairwiseComparisonCount(matrix(0, 1, 3)), 3)   # 0 + 3
})

test_that("other adjustment methods match p.adjust", {
  d <- cq_data()
  tab <- table(d$rows, d$cols)
  raw <- cq(d, posthoc = "bonferroni")$posthocTable$asDF$p

  # the option's levels are bonferroni / holm / fdr / none; p.adjust treats
  # "fdr" as an alias for Benjamini-Hochberg, so the name passes straight through.
  # "none" is excluded on purpose: it means "run no pairwise tests at all" here,
  # not "no adjustment" - a deliberate, documented choice covered by its own test
  # in test-chisqposttest-integration.R.
  for (m in c("holm", "fdr")) {
    got <- cq(d, posthoc = m)$posthocTable$asDF$padj
    expect_equal(got, p.adjust(raw, method = m), tolerance = 1e-10, info = m)
  }
})

test_that("effect size is sqrt(chi-square / n) for each sub-table", {
  d <- cq_data()
  df <- cq(d, posthoc = "bonferroni")$posthocTable$asDF
  tab <- table(d$rows, d$cols)

  # first comparison is the R1 vs R2 row pair
  sub <- tab[c(1, 2), , drop = FALSE]
  expected <- sqrt(unname(chisq.test(sub, correct = FALSE)$statistic) / sum(sub))
  expect_equal(df$effect_size[1], round(expected, 3), tolerance = 1e-3)
})

test_that("the bootstrap phi CI is reproducible across runs", {
  # Unseeded, the interval moved on every run of the SAME analysis on the SAME
  # data: [0.251, 0.573], [0.261, 0.578], [0.260, 0.580]. In jamovi any option
  # toggle re-runs the analysis, so a reported interval could change underneath
  # the user.
  skip_if_not_installed("boot")
  d <- cq_data()

  runs <- replicate(3, paste(cq(d, phiCI = TRUE)$posthocTable$asDF$phi_ci,
                             collapse = " "))
  expect_equal(length(unique(runs)), 1L)
  expect_true(all(nzchar(runs)))
  expect_match(runs[1], "^\\[")     # an interval was actually produced
})

test_that("seeding the bootstrap does not disturb the caller's RNG stream", {
  skip_if_not_installed("boot")
  d <- cq_data()

  set.seed(99); a <- runif(3)
  invisible(cq(d, phiCI = TRUE))
  b <- runif(3)

  set.seed(99); ref <- runif(6)
  expect_equal(c(a, b), ref)
})

test_that("the expected-count warning does not leak to the caller", {
  # chisq.test() warns whenever an expected count is small. The Assumptions panel
  # already reports that condition with exact cell counts and a recommendation,
  # so the raw warning is redundant noise a GUI user never sees.
  data(histopathology, package = "ClinicoPath")
  h <- as.data.frame(histopathology)

  expect_silent(invisible(chisqposttest(data = h, rows = "Grade", cols = "LVI",
                                        counts = NULL)))

  # ... and the condition IS still reported to the user
  res <- chisqposttest(data = h, rows = "Grade", cols = "LVI", counts = NULL,
                       showAssumptionsCheck = TRUE)
  panel <- gsub("<[^>]*>", " ", as.character(res$assumptionsCheck$content))
  expect_match(panel, "Expected counts")
  expect_match(panel, "Fisher")
})

test_that("a non-significant omnibus test yields no post-hoc comparisons", {
  # Post-hoc testing is gated on the omnibus result, which is the correct
  # protected procedure - an empty table here is intended, not a failure.
  set.seed(11); n <- 180
  d <- data.frame(rows = factor(sample(c("R1", "R2", "R3"), n, TRUE)),
                  cols = factor(sample(c("C1", "C2", "C3"), n, TRUE)))
  expect_gt(chisq.test(table(d$rows, d$cols))$p.value, 0.05)
  expect_equal(nrow(cq(d, posthoc = "bonferroni")$posthocTable$asDF), 0L)
})
