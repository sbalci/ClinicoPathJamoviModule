# ── Tests for nonparametric function ──────────────────────────────────────────
# Covers: test types, effect sizes, post-hoc, paired analysis, edge cases,
# and agreement with base R.

# Helper: calls nonparametric with sensible defaults for all required args.
# The auto-generated wrapper requires all Variable-type options explicitly.
np <- function(data, ...) {
  defaults <- list(
    data = data,
    deps = NULL, outcome = NULL, groups = NULL,
    paired_variable = NULL, blocking_variable = NULL,
    test_type = "mann_whitney",
    effect_size = FALSE, effect_size_method = "eta_squared",
    confidence_intervals = FALSE, confidence_level = 0.95,
    post_hoc = FALSE, post_hoc_method = "dunn",
    p_adjustment = "holm", globalTestCount = 1,
    robust_method = "standard", trim_proportion = 0.1,
    winsorize_proportion = 0.1,
    bootstrap_ci = FALSE, bootstrap_samples = 100,
    test_assumptions = FALSE, normality_tests = FALSE,
    assumption_checks = FALSE, homogeneity_test = "levene",
    exact_test = FALSE, exact_p_values = FALSE,
    continuity_correction = TRUE, tie_correction = TRUE,
    ties_method = "average",
    show_boxplots = FALSE, show_violin_plots = FALSE,
    show_rank_plots = FALSE, show_effect_plots = FALSE,
    descriptive_plots = FALSE, show_qqplots = FALSE,
    show_descriptives = FALSE, show_test_statistics = TRUE,
    show_post_hoc_table = FALSE, show_effect_sizes = FALSE,
    show_assumptions = FALSE, show_robust_statistics = FALSE,
    show_power_analysis = FALSE,
    show_instructions = FALSE, show_explanations = FALSE,
    show_interpretation = FALSE, show_recommendations = FALSE,
    clinical_context = "general",
    set_seed = TRUE, seed_value = 42,
    missing_data_handling = "listwise", alpha_level = 0.05,
    minimum_sample_size = 3, outlier_method = "none",
    small_sample_exact = FALSE,
    report_standardized_statistics = FALSE
  )
  args <- modifyList(defaults, list(...))
  do.call(nonparametric, args)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 1. Smoke Test
# ═══════════════════════════════════════════════════════════════════════════════

test_that("nonparametric runs without error", {
  set.seed(123)
  d <- data.frame(x = runif(50), g = factor(sample(c("A","B"), 50, TRUE)))
  expect_no_error(np(d, outcome = "x", groups = "g"))
})

# ═══════════════════════════════════════════════════════════════════════════════
# 2. Mann-Whitney — agrees with base R
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Mann-Whitney agrees with wilcox.test", {
  set.seed(42)
  d <- data.frame(y = c(rnorm(30, 5, 2), rnorm(30, 7, 2)),
                  g = factor(rep(c("A","B"), each = 30)))
  result <- np(d, outcome = "y", groups = "g", test_type = "mann_whitney")
  ref <- wilcox.test(y ~ g, data = d)
  t <- result$tests$asDF
  expect_equal(unname(t$statistic[1]), unname(ref$statistic), tolerance = 0.1)
  expect_equal(t$p[1], ref$p.value, tolerance = 0.01)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 3. Kruskal-Wallis — agrees with base R
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Kruskal-Wallis agrees with kruskal.test", {
  set.seed(42)
  d <- data.frame(y = c(rnorm(25, 5), rnorm(25, 7), rnorm(25, 6)),
                  g = factor(rep(c("A","B","C"), each = 25)))
  result <- np(d, outcome = "y", groups = "g", test_type = "kruskal_wallis")
  ref <- kruskal.test(y ~ g, data = d)
  t <- result$tests$asDF
  expect_equal(unname(t$statistic[1]), unname(ref$statistic), tolerance = 0.1)
  expect_equal(t$p[1], ref$p.value, tolerance = 0.01)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 4. Wilcoxon Signed-Rank — proper pairing via subject ID
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Wilcoxon signed-rank pairs correctly via subject ID", {
  set.seed(42)
  n <- 20; pre <- rnorm(n, 10, 3); post <- pre + rnorm(n, 2, 1.5)
  d <- data.frame(
    subj = factor(rep(paste0("S", 1:n), 2)),
    time = factor(rep(c("Pre","Post"), each = n)),
    val = c(pre, post))
  d <- d[sample(nrow(d)), ]  # shuffle

  result <- np(d, outcome = "val", groups = "time",
               paired_variable = "subj", test_type = "wilcoxon_signed")
  ref <- wilcox.test(post, pre, paired = TRUE)
  t <- result$tests$asDF
  expect_equal(unname(t$statistic[1]), unname(ref$statistic), tolerance = 0.1)
  expect_equal(t$p[1], ref$p.value, tolerance = 0.01)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 5. Jonckheere-Terpstra — detects ordered trend
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Jonckheere-Terpstra detects ordered trend", {
  set.seed(42)
  d <- data.frame(y = c(rnorm(30, 3), rnorm(30, 5), rnorm(30, 7)),
                  g = factor(rep(c("Low","Med","High"), each = 30),
                             levels = c("Low","Med","High"), ordered = TRUE))
  result <- np(d, outcome = "y", groups = "g", test_type = "jonckheere_terpstra")
  expect_true(result$tests$asDF$p[1] < 0.05)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 6. Van der Waerden
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Van der Waerden test runs correctly", {
  set.seed(42)
  d <- data.frame(y = c(rnorm(30, 5), rnorm(30, 7), rnorm(30, 6)),
                  g = factor(rep(c("A","B","C"), each = 30)))
  result <- np(d, outcome = "y", groups = "g", test_type = "van_der_waerden")
  t <- result$tests$asDF
  expect_false(is.na(t$statistic[1]))
  expect_equal(t$df[1], 2)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 7. Effect Sizes
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Cliff delta is in [-1, 1]", {
  set.seed(42)
  d <- data.frame(y = c(rnorm(30, 5), rnorm(30, 7)),
                  g = factor(rep(c("A","B"), each = 30)))
  result <- np(d, outcome = "y", groups = "g",
               effect_size = TRUE, effect_size_method = "cliff_delta")
  es <- result$tests$asDF$effect_size[1]
  expect_true(es >= -1 && es <= 1)
})

test_that("Eta-squared is in [0, 1]", {
  set.seed(42)
  d <- data.frame(y = c(rnorm(30, 5), rnorm(30, 7), rnorm(30, 9)),
                  g = factor(rep(c("A","B","C"), each = 30)))
  result <- np(d, outcome = "y", groups = "g", test_type = "kruskal_wallis",
               effect_size = TRUE, effect_size_method = "eta_squared")
  es <- result$tests$asDF$effect_size[1]
  expect_true(es >= 0 && es <= 1)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 8. Post-hoc
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Post-hoc produces C(k,2) pairwise comparisons", {
  set.seed(42)
  d <- data.frame(y = c(rnorm(25, 3), rnorm(25, 5), rnorm(25, 7), rnorm(25, 9)),
                  g = factor(rep(c("G1","G2","G3","G4"), each = 25)))
  result <- np(d, outcome = "y", groups = "g", test_type = "kruskal_wallis",
               post_hoc = TRUE, show_post_hoc_table = TRUE)
  expect_equal(nrow(result$posthoc$asDF), 6)  # C(4,2)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 9. Multiple dependent variables
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Multiple deps produce one row each", {
  set.seed(42)
  d <- data.frame(x1 = rnorm(60), x2 = rnorm(60), x3 = rnorm(60),
                  g = factor(rep(c("A","B"), each = 30)))
  result <- np(d, deps = c("x1","x2","x3"), groups = "g")
  expect_equal(nrow(result$tests$asDF), 3)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 10. Global multiplicity correction
# ═══════════════════════════════════════════════════════════════════════════════

test_that("globalTestCount multiplies p-values", {
  set.seed(42)
  d <- data.frame(y = c(rnorm(30, 5), rnorm(30, 7)),
                  g = factor(rep(c("A","B"), each = 30)))
  r1 <- np(d, outcome = "y", groups = "g", globalTestCount = 1)
  r5 <- np(d, outcome = "y", groups = "g", globalTestCount = 5)
  expect_equal(r5$tests$asDF$p[1], min(r1$tests$asDF$p[1] * 5, 1), tolerance = 0.001)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 11. Edge cases
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Missing data handled without crash", {
  set.seed(42)
  d <- data.frame(y = c(rnorm(30, 5), rnorm(30, 7)),
                  g = factor(rep(c("A","B"), each = 30)))
  d$y[c(5, 15, 45)] <- NA
  result <- np(d, outcome = "y", groups = "g")
  expect_false(is.na(result$tests$asDF$p[1]))
})

test_that("Single group does not crash", {
  d <- data.frame(y = rnorm(20), g = factor(rep("A", 20)))
  expect_no_error(np(d, outcome = "y", groups = "g"))
})

# ═══════════════════════════════════════════════════════════════════════════════
# 12. Median and Mood's Median tests
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Median test runs", {
  set.seed(42)
  d <- data.frame(y = c(rnorm(30, 5), rnorm(30, 8)),
                  g = factor(rep(c("A","B"), each = 30)))
  result <- np(d, outcome = "y", groups = "g", test_type = "median_test")
  expect_false(is.na(result$tests$asDF$p[1]))
})

test_that("Mood median test runs", {
  set.seed(42)
  d <- data.frame(y = c(rnorm(30, 5), rnorm(30, 8)),
                  g = factor(rep(c("A","B"), each = 30)))
  result <- np(d, outcome = "y", groups = "g", test_type = "mood_median")
  expect_false(is.na(result$tests$asDF$p[1]))
})

# ═══════════════════════════════════════════════════════════════════════════════
# 13. Sign test
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Sign test runs with paired data", {
  set.seed(42)
  n <- 20; pre <- rnorm(n, 10, 3); post <- pre + rnorm(n, 2, 1)
  d <- data.frame(
    subj = factor(rep(paste0("S", 1:n), 2)),
    time = factor(rep(c("Pre","Post"), each = n)),
    val = c(pre, post))
  result <- np(d, outcome = "val", groups = "time",
               paired_variable = "subj", test_type = "sign_test")
  expect_false(is.na(result$tests$asDF$p[1]))
})
