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
  expect_equal(priv$.pairwiseComparisonCount(matrix(0, 2, 2)), 0)   # 0 + 0 (2-level dimension is tested by omnibus)
  expect_equal(priv$.pairwiseComparisonCount(matrix(0, 4, 2)), 6)   # 6 + 0
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

  expect_silent(invisible(chisqposttest(data = h, rows = "Race", cols = "LVI",
                                        counts = NULL)))

  # ... and the condition IS still reported to the user
  res <- chisqposttest(data = h, rows = "Race", cols = "LVI", counts = NULL,
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

test_that("variable names with spaces, punctuation and Unicode run end to end", {
  # Raw-rows path indexes data[[name]]; the weighted path builds an xtabs()
  # formula via jmvcore::constructFormula(), which is where an unescaped name
  # would break. Both must give the same table as plain names.
  d <- cq_data()
  names(d) <- c("Tumour grade (WHO)", "LVI: yes/no \u{2013} \u{F6}zet")
  plain <- cq(cq_data(), posthoc = "bonferroni")$posthocTable$asDF

  raw <- chisqposttest(data = d, rows = "Tumour grade (WHO)",
                       cols = "LVI: yes/no \u{2013} \u{F6}zet", counts = NULL,
                       posthoc = "bonferroni")
  expect_equal(raw$posthocTable$asDF$padj, plain$padj)
  expect_equal(raw$chisqTable$asDF$value, cq(cq_data())$chisqTable$asDF$value)

  agg <- as.data.frame(table(d[[1]], d[[2]]), stringsAsFactors = TRUE)
  names(agg) <- c("Tumour grade (WHO)", "LVI: yes/no \u{2013} \u{F6}zet", "n (weight)")
  weighted <- chisqposttest(data = agg, rows = "Tumour grade (WHO)",
                            cols = "LVI: yes/no \u{2013} \u{F6}zet",
                            counts = "n (weight)", posthoc = "bonferroni")
  expect_equal(weighted$posthocTable$asDF$padj, plain$padj)
  expect_equal(weighted$chisqTable$asDF$value, raw$chisqTable$asDF$value)
})

test_that("a severe expected-count violation is not downgraded by a small sample", {
  # Expected counts below 1 and n < 20 co-occur in exactly the tables where the
  # severe flag matters. The small-n check used to overwrite "severe" with
  # "moderate", which changed the panel's colour and severity.
  d <- data.frame(rows = factor(c(rep("A", 6), rep("B", 6), "C")),
                  cols = factor(c(rep("X", 6), rep("Y", 6), "X")))
  expect_lt(min(suppressWarnings(chisq.test(table(d$rows, d$cols)))$expected), 1)
  html <- as.character(cq(d, showAssumptionsCheck = TRUE)$assumptionsCheck$content)
  expect_match(html, "#dc3545", fixed = TRUE)   # the severe border colour
  expect_match(html, "Critical")
})

test_that("the Fisher method notice reflects a forced selection", {
  res <- cq(cq_data(), posthoc = "bonferroni", testSelection = "fisher")
  expect_true(all(startsWith(res$posthocTable$asDF$test_method, "Fisher")))
  html <- as.character(res$multipleTestingInfo$content)
  expect_match(html, "selected in the options")
  expect_false(grepl("automatically", html))
  detail <- as.character(cq(cq_data(), posthoc = "bonferroni", testSelection = "fisher",
                            showDetailedTables = TRUE)$detailedComparisons$content)
  expect_match(detail, "chosen in the options")
  expect_false(grepl("below 5", detail))
})

test_that("prose panels name the adjustment method rather than its option key", {
  html <- as.character(cq(cq_data(), posthoc = "fdr", showClinicalSummary = TRUE)$clinicalSummary$content)
  expect_match(html, "Benjamini-Hochberg")
  expect_false(grepl("after fdr", html))
})

test_that("the weighted-data panel is written before the table is built", {
  d <- cq_data()
  agg <- as.data.frame(table(d$rows, d$cols), stringsAsFactors = TRUE)
  names(agg) <- c("rows", "cols", "n")
  # counts chosen but rows/cols not yet: the panel is visible:(counts) and used
  # to be a titled, empty box in this state
  res <- chisqposttest(data = agg, rows = NULL, cols = NULL, counts = "n")
  expect_match(as.character(res$weightedDataInfo$content), "frequency counts")
})

test_that("an expected count below 1 is reported even when most cells are adequate", {
  # The > 20%-of-cells rule does not fire on a large table with one sparse
  # category, and the Assumptions panel that reports it is opt-in.
  set.seed(5)
  big <- data.frame(rows = factor(sample(paste0("R", 1:5), 600, TRUE, c(.3, .3, .3, .09, .01))),
                    cols = factor(sample(paste0("C", 1:2), 600, TRUE, c(.85, .15))))
  big$cols[big$rows == "R1"] <- sample(c("C1", "C2"), sum(big$rows == "R1"), TRUE, c(.6, .4))
  ex <- suppressWarnings(chisq.test(table(big$rows, big$cols)))$expected
  expect_true(any(ex < 1) && mean(ex < 5) <= 0.2)     # exactly the gap being covered
  expect_match(cq(big)$notices$content, "expected count below 1")
})

test_that("error paths write the notices panel as well as the red box", {
  d <- cq_data()
  one <- d; one$cols <- factor("C1")
  res <- cq(one)
  expect_match(res$notices$content, "ERROR: The analysis cannot proceed")
  expect_match(res$notices$content, "at least 2 rows and 2 columns")
  expect_match(as.character(res$todo$content), "at least 2 rows and 2 columns")
  expect_match(cq(d[0, ])$notices$content, "no \\(complete\\) rows")
})

test_that("non-integer counts raise a warning notice, whole-number counts do not", {
  d <- cq_data()
  agg <- as.data.frame(table(d$rows, d$cols), stringsAsFactors = TRUE)
  names(agg) <- c("rows", "cols", "n")
  ok <- chisqposttest(data = agg, rows = "rows", cols = "cols", counts = "n")
  expect_false(grepl("not whole numbers", ok$notices$content))
  agg$n <- agg$n + 0.5
  frac <- chisqposttest(data = agg, rows = "rows", cols = "cols", counts = "n")
  expect_match(frac$notices$content, "not whole numbers")
})

test_that("every run ends with a one-line analysis summary", {
  s <- cq(cq_data())$notices$content
  expect_match(s, "NOTE: Analysis summary")
  expect_match(s, "180 observations in a 3 x 3 table")
  expect_match(s, "6 pairwise comparisons with Bonferroni adjustment")
  expect_match(cq(cq_data(), posthoc = "none")$notices$content, "post-hoc method set to None")
})

test_that("the bootstrap seed option is honoured by the wrapper", {
  skip_if_not_installed("boot")
  d <- cq_data()
  ci <- function(...) cq(d, phiCI = TRUE, ...)$posthocTable$asDF$phi_ci
  expect_identical(ci(seed = 42), ci(seed = 42))
  other <- ci(seed = 7)
  expect_length(other, 6L)
  expect_false(identical(ci(seed = 42), other))
})
