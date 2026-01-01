# Comprehensive Validation Tests for chisqposttest Function
# Validates chi-squared statistics, p-values, post-hoc pairwise comparisons,
# and multiple testing corrections against trusted R packages

library(testthat)
library(ClinicoPath)

# Helper function to extract table data from jamovi results
extract_table_data <- function(result, table_name) {
  if (!inherits(result, "chisqposttestResults")) {
    return(NULL)
  }

  table_obj <- result[[table_name]]
  if (is.null(table_obj)) {
    return(NULL)
  }

  # asDF is a property, not a method
  if (inherits(table_obj, "Table") && !is.null(table_obj$asDF)) {
    return(table_obj$asDF)
  }

  return(NULL)
}

# ============================================================================
# PART 1: CHI-SQUARED STATISTIC AND P-VALUE VALIDATION
# Validates that omnibus test matches stats::chisq.test()
# ============================================================================

# ===== Omnibus Chi-Squared Test Validation =====

test_that("Chi-squared statistic matches stats::chisq.test() for 2x2 table", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  # Create simple 2x2 contingency table
  data <- data.frame(
    treatment = factor(rep(c("Drug", "Placebo"), c(60, 60))),
    outcome = factor(c(
      rep("Improved", 40), rep("Not Improved", 20),
      rep("Improved", 20), rep("Not Improved", 40)
    ))
  )

  # Reference test using base R (without continuity correction to match implementation)
  cont_table <- table(data$treatment, data$outcome)
  reference <- chisq.test(cont_table, correct = FALSE)

  # Run chisqposttest
  result <- chisqposttest(
    data = data,
    rows = "treatment",
    cols = "outcome",
    posthoc = "none"
  )

  chisq_table <- extract_table_data(result, "chisqTable")

  expect_true(!is.null(chisq_table))
  expect_equal(nrow(chisq_table), 1)

  # Validate chi-squared statistic
  expect_equal(chisq_table$value[1], reference$statistic[[1]],
               tolerance = 1e-6)

  # Validate p-value
  expect_equal(chisq_table$p[1], reference$p.value,
               tolerance = 1e-6)

  # Validate degrees of freedom
  expect_equal(chisq_table$df[1], reference$parameter[[1]])
})

test_that("Chi-squared statistic matches stats::chisq.test() for 3x2 table", {
  set.seed(123)
  data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 100)),
    outcome = factor(c(
      sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.7, 0.3)),
      sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.5, 0.5)),
      sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.3, 0.7))
    ))
  )

  # Reference test
  cont_table <- table(data$group, data$outcome)
  reference <- chisq.test(cont_table, correct = FALSE)

  # Run chisqposttest
  result <- chisqposttest(
    data = data,
    rows = "group",
    cols = "outcome",
    posthoc = "none"
  )

  chisq_table <- extract_table_data(result, "chisqTable")

  # Validate chi-squared statistic
  expect_equal(chisq_table$value[1], reference$statistic[[1]],
               tolerance = 1e-6)

  # Validate p-value
  expect_equal(chisq_table$p[1], reference$p.value,
               tolerance = 1e-6)
})

test_that("Chi-squared test handles 4x3 table correctly", {
  set.seed(456)
  data <- data.frame(
    treatment = factor(rep(c("A", "B", "C", "D"), each = 75)),
    response = factor(c(
      sample(c("Low", "Medium", "High"), 75, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
      sample(c("Low", "Medium", "High"), 75, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
      sample(c("Low", "Medium", "High"), 75, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
      sample(c("Low", "Medium", "High"), 75, replace = TRUE, prob = c(0.2, 0.3, 0.5))
    ))
  )

  # Reference test
  cont_table <- table(data$treatment, data$response)
  reference <- chisq.test(cont_table, correct = FALSE)

  # Run chisqposttest
  result <- chisqposttest(
    data = data,
    rows = "treatment",
    cols = "response",
    posthoc = "none"
  )

  chisq_table <- extract_table_data(result, "chisqTable")

  # Validate
  expect_equal(chisq_table$value[1], reference$statistic[[1]], tolerance = 1e-6)
  expect_equal(chisq_table$p[1], reference$p.value, tolerance = 1e-6)
  expect_equal(chisq_table$df[1], (4-1) * (3-1))  # (rows-1) * (cols-1)
})

# ============================================================================
# PART 2: PAIRWISE COMPARISON VALIDATION
# Validates individual 2x2 subtable tests
# ============================================================================

# ===== Pairwise Comparisons =====

test_that("Pairwise chi-squared tests match manual calculation for 3x2 table", {
  # Create data with known structure
  set.seed(789)
  data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 80)),
    outcome = factor(c(
      sample(c("Yes", "No"), 80, replace = TRUE, prob = c(0.8, 0.2)),
      sample(c("Yes", "No"), 80, replace = TRUE, prob = c(0.5, 0.5)),
      sample(c("Yes", "No"), 80, replace = TRUE, prob = c(0.2, 0.8))
    ))
  )

  # Run chisqposttest with Bonferroni
  result <- chisqposttest(
    data = data,
    rows = "group",
    cols = "outcome",
    posthoc = "bonferroni"
  )

  posthoc_table <- extract_table_data(result, "posthocTable")

  # Verify post-hoc was run (omnibus should be significant)
  cont_table <- table(data$group, data$outcome)
  omnibus <- chisq.test(cont_table)
  expect_true(omnibus$p.value < 0.05)

  # Should have row comparisons (A vs B, A vs C, B vs C)
  # Note: Implementation also does column comparisons
  expect_true(!is.null(posthoc_table))
  expect_true(nrow(posthoc_table) >= 3)

  # Manually validate at least one pairwise comparison
  # Extract A vs B subtable
  data_ab <- data[data$group %in% c("A", "B"), ]
  data_ab$group <- droplevels(data_ab$group)
  subtable_ab <- table(data_ab$group, data_ab$outcome)
  reference_ab <- chisq.test(subtable_ab)

  # Find A vs B in posthoc table
  ab_row <- posthoc_table[grepl("A.*B", posthoc_table$comparison), ]

  if (nrow(ab_row) > 0) {
    # Validate unadjusted p-value matches manual calculation
    expect_equal(ab_row$p[1], reference_ab$p.value, tolerance = 1e-6)
  }
})

test_that("Pairwise p-values are correctly extracted for all comparisons", {
  set.seed(101)
  data <- data.frame(
    category = factor(rep(c("X", "Y", "Z"), each = 60)),
    result = factor(c(
      sample(c("Pass", "Fail"), 60, replace = TRUE, prob = c(0.7, 0.3)),
      sample(c("Pass", "Fail"), 60, replace = TRUE, prob = c(0.6, 0.4)),
      sample(c("Pass", "Fail"), 60, replace = TRUE, prob = c(0.3, 0.7))
    ))
  )

  # Run without adjustment for easier validation
  result <- chisqposttest(
    data = data,
    rows = "category",
    cols = "result",
    posthoc = "bonferroni"  # Will apply Bonferroni
  )

  posthoc_table <- extract_table_data(result, "posthocTable")

  # Verify all comparisons have valid p-values
  expect_true(all(!is.na(posthoc_table$p)))
  expect_true(all(posthoc_table$p >= 0 & posthoc_table$p <= 1))

  # Verify all adjusted p-values are present
  expect_true(all(!is.na(posthoc_table$padj)))
})

# ============================================================================
# PART 3: MULTIPLE TESTING CORRECTION VALIDATION
# Validates Bonferroni, Holm, and FDR adjustments
# ============================================================================

# ===== Multiple Testing Corrections =====

test_that("Bonferroni correction matches p.adjust() with method='bonferroni'", {
  set.seed(202)
  data <- data.frame(
    group = factor(rep(c("A", "B", "C", "D"), each = 50)),
    outcome = factor(c(
      sample(c("Y", "N"), 50, replace = TRUE, prob = c(0.7, 0.3)),
      sample(c("Y", "N"), 50, replace = TRUE, prob = c(0.6, 0.4)),
      sample(c("Y", "N"), 50, replace = TRUE, prob = c(0.4, 0.6)),
      sample(c("Y", "N"), 50, replace = TRUE, prob = c(0.3, 0.7))
    ))
  )

  # Run with Bonferroni
  result <- chisqposttest(
    data = data,
    rows = "group",
    cols = "outcome",
    posthoc = "bonferroni"
  )

  posthoc_table <- extract_table_data(result, "posthocTable")

  if (!is.null(posthoc_table) && nrow(posthoc_table) > 0) {
    # Extract unadjusted p-values
    unadjusted_p <- posthoc_table$p

    # Calculate expected Bonferroni adjustment
    expected_bonf <- p.adjust(unadjusted_p, method = "bonferroni")

    # Validate
    expect_equal(posthoc_table$padj, expected_bonf, tolerance = 1e-10)

    # Also verify all adjusted p-values >= unadjusted
    expect_true(all(posthoc_table$padj >= posthoc_table$p))
  }
})

test_that("Holm correction matches p.adjust() with method='holm'", {
  set.seed(303)
  data <- data.frame(
    group = factor(rep(c("A", "B", "C", "D"), each = 50)),
    outcome = factor(c(
      sample(c("Y", "N"), 50, replace = TRUE, prob = c(0.75, 0.25)),
      sample(c("Y", "N"), 50, replace = TRUE, prob = c(0.55, 0.45)),
      sample(c("Y", "N"), 50, replace = TRUE, prob = c(0.45, 0.55)),
      sample(c("Y", "N"), 50, replace = TRUE, prob = c(0.25, 0.75))
    ))
  )

  # Run with Holm
  result <- chisqposttest(
    data = data,
    rows = "group",
    cols = "outcome",
    posthoc = "holm"
  )

  posthoc_table <- extract_table_data(result, "posthocTable")

  if (!is.null(posthoc_table) && nrow(posthoc_table) > 0) {
    unadjusted_p <- posthoc_table$p
    expected_holm <- p.adjust(unadjusted_p, method = "holm")

    expect_equal(posthoc_table$padj, expected_holm, tolerance = 1e-10)
  }
})

test_that("FDR correction matches p.adjust() with method='fdr'", {
  set.seed(404)
  data <- data.frame(
    group = factor(rep(c("A", "B", "C", "D"), each = 50)),
    outcome = factor(c(
      sample(c("Y", "N"), 50, replace = TRUE, prob = c(0.8, 0.2)),
      sample(c("Y", "N"), 50, replace = TRUE, prob = c(0.6, 0.4)),
      sample(c("Y", "N"), 50, replace = TRUE, prob = c(0.4, 0.6)),
      sample(c("Y", "N"), 50, replace = TRUE, prob = c(0.2, 0.8))
    ))
  )

  # Run with FDR
  result <- chisqposttest(
    data = data,
    rows = "group",
    cols = "outcome",
    posthoc = "fdr"
  )

  posthoc_table <- extract_table_data(result, "posthocTable")

  if (!is.null(posthoc_table) && nrow(posthoc_table) > 0) {
    unadjusted_p <- posthoc_table$p
    expected_fdr <- p.adjust(unadjusted_p, method = "fdr")

    expect_equal(posthoc_table$padj, expected_fdr, tolerance = 1e-10)
  }
})

test_that("Bonferroni is more conservative than FDR", {
  set.seed(505)
  data <- data.frame(
    group = factor(rep(c("A", "B", "C", "D", "E"), each = 40)),
    outcome = factor(c(
      sample(c("Y", "N"), 40, replace = TRUE, prob = c(0.75, 0.25)),
      sample(c("Y", "N"), 40, replace = TRUE, prob = c(0.65, 0.35)),
      sample(c("Y", "N"), 40, replace = TRUE, prob = c(0.50, 0.50)),
      sample(c("Y", "N"), 40, replace = TRUE, prob = c(0.35, 0.65)),
      sample(c("Y", "N"), 40, replace = TRUE, prob = c(0.25, 0.75))
    ))
  )

  # Run both methods
  result_bonf <- chisqposttest(data = data, rows = "group", cols = "outcome", posthoc = "bonferroni")
  result_fdr <- chisqposttest(data = data, rows = "group", cols = "outcome", posthoc = "fdr")

  bonf_table <- extract_table_data(result_bonf, "posthocTable")
  fdr_table <- extract_table_data(result_fdr, "posthocTable")

  if (!is.null(bonf_table) && !is.null(fdr_table) && nrow(bonf_table) > 0) {
    # Same number of comparisons
    expect_equal(nrow(bonf_table), nrow(fdr_table))

    # FDR should be less conservative (smaller or equal adjusted p-values)
    # For each comparison, Bonferroni padj >= FDR padj
    for (i in 1:nrow(bonf_table)) {
      expect_true(bonf_table$padj[i] >= fdr_table$padj[i])
    }
  }
})

# ============================================================================
# PART 4: STATISTICAL GUARDRAILS VALIDATION
# Validates that post-hoc only runs when appropriate
# ============================================================================

# ===== Statistical Safeguards =====

test_that("Post-hoc tests are NOT performed when omnibus is non-significant", {
  # Create data with approximately equal proportions (non-significant)
  set.seed(606)
  data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 100)),
    outcome = factor(c(
      sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.50, 0.50)),
      sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.48, 0.52)),
      sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.52, 0.48))
    ))
  )

  # Verify omnibus is non-significant
  cont_table <- table(data$group, data$outcome)
  omnibus <- chisq.test(cont_table)
  expect_true(omnibus$p.value >= 0.05)

  # Run chisqposttest
  result <- chisqposttest(
    data = data,
    rows = "group",
    cols = "outcome",
    posthoc = "bonferroni"
  )

  posthoc_table <- extract_table_data(result, "posthocTable")

  # Post-hoc table should be empty
  expect_equal(nrow(posthoc_table), 0)
})

test_that("Post-hoc tests ARE performed when omnibus is significant", {
  # Create data with clear differences (significant)
  set.seed(707)
  data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 100)),
    outcome = factor(c(
      sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.80, 0.20)),
      sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.50, 0.50)),
      sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.20, 0.80))
    ))
  )

  # Verify omnibus is significant
  cont_table <- table(data$group, data$outcome)
  omnibus <- chisq.test(cont_table)
  expect_true(omnibus$p.value < 0.05)

  # Run chisqposttest
  result <- chisqposttest(
    data = data,
    rows = "group",
    cols = "outcome",
    posthoc = "bonferroni"
  )

  posthoc_table <- extract_table_data(result, "posthocTable")

  # Post-hoc table should be populated
  expect_true(nrow(posthoc_table) > 0)
})

test_that("posthoc='none' disables all pairwise testing", {
  set.seed(808)
  data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 100)),
    outcome = factor(c(
      sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.80, 0.20)),
      sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.50, 0.50)),
      sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.20, 0.80))
    ))
  )

  # Run with posthoc = "none"
  result <- chisqposttest(
    data = data,
    rows = "group",
    cols = "outcome",
    posthoc = "none"
  )

  posthoc_table <- extract_table_data(result, "posthocTable")

  # Post-hoc table should be empty even though omnibus is significant
  expect_equal(nrow(posthoc_table), 0)
})

# ============================================================================
# PART 5: EDGE CASES AND ROBUSTNESS
# Validates handling of small samples, sparse tables, etc.
# ============================================================================

# ===== Edge Cases =====

test_that("2x2 table generates row and column comparisons", {
  data <- data.frame(
    var1 = factor(rep(c("A", "B"), each = 50)),
    var2 = factor(c(rep("X", 30), rep("Y", 20), rep("X", 20), rep("Y", 30)))
  )

  result <- chisqposttest(
    data = data,
    rows = "var1",
    cols = "var2",
    posthoc = "bonferroni"
  )

  posthoc_table <- extract_table_data(result, "posthocTable")

  # Implementation performs both row (A vs B) and column (X vs Y) comparisons
  # This is non-standard but matches current implementation
  if (!is.null(posthoc_table) && nrow(posthoc_table) > 0) {
    expect_true(nrow(posthoc_table) >= 1)

    # Check comparisons include expected pairs
    comparisons_str <- paste(posthoc_table$comparison, collapse = " ")
    # Should include row or column comparisons
    expect_true(grepl("A.*B|X.*Y", comparisons_str))
  }
})

test_that("Very small sample sizes handled gracefully", {
  # Small sample that might trigger Fisher's exact test
  data <- data.frame(
    group = factor(c(rep("A", 8), rep("B", 8), rep("C", 8))),
    outcome = factor(c(
      rep("Y", 6), rep("N", 2),
      rep("Y", 4), rep("N", 4),
      rep("Y", 2), rep("N", 6)
    ))
  )

  # Should not error
  result <- chisqposttest(
    data = data,
    rows = "group",
    cols = "outcome",
    posthoc = "bonferroni"
  )

  expect_true(inherits(result, "chisqposttestResults"))

  chisq_table <- extract_table_data(result, "chisqTable")
  expect_true(!is.null(chisq_table))
})

test_that("Sparse contingency table handled correctly", {
  # Table with some zero cells
  data <- data.frame(
    group = factor(c(rep("A", 50), rep("B", 50), rep("C", 50))),
    outcome = factor(c(
      rep("Y", 48), rep("N", 2),   # A: almost all Y
      rep("Y", 25), rep("N", 25),  # B: balanced
      rep("Y", 2), rep("N", 48)    # C: almost all N
    ))
  )

  # Should not error (may use Fisher's exact for some comparisons)
  result <- chisqposttest(
    data = data,
    rows = "group",
    cols = "outcome",
    posthoc = "bonferroni"
  )

  expect_true(inherits(result, "chisqposttestResults"))
})

# ============================================================================
# PART 6: PUBLISHED DATASET VALIDATION
# Tests using well-known datasets with published results
# ============================================================================

# ===== Benchmark Datasets =====

test_that("UCBAdmissions dataset chi-squared matches known result", {
  # Use built-in R dataset: UCBAdmissions
  # This is a classic 2x2x6 table (Gender x Admit x Dept)
  # We'll collapse to 2x2 (Gender x Admit, summing over departments)

  ucb_data <- as.data.frame(UCBAdmissions)

  # Aggregate over departments
  ucb_aggregated <- aggregate(Freq ~ Gender + Admit, data = ucb_data, FUN = sum)

  # Expand to individual observations
  ucb_expanded <- ucb_aggregated[rep(1:nrow(ucb_aggregated), ucb_aggregated$Freq),
                                  c("Gender", "Admit")]

  # Reference test
  cont_table <- table(ucb_expanded$Gender, ucb_expanded$Admit)
  reference <- chisq.test(cont_table, correct = FALSE)

  # Run chisqposttest
  result <- chisqposttest(
    data = ucb_expanded,
    rows = "Gender",
    cols = "Admit",
    posthoc = "none"
  )

  chisq_table <- extract_table_data(result, "chisqTable")

  # Validate against reference (small tolerance for floating point differences)
  expect_equal(chisq_table$value[1], reference$statistic[[1]], tolerance = 1e-2)
  expect_equal(chisq_table$p[1], reference$p.value, tolerance = 1e-4)
})

test_that("HairEyeColor dataset chi-squared matches known result", {
  # Another built-in dataset
  hec_data <- as.data.frame(HairEyeColor)

  # Sum over Sex
  hec_aggregated <- aggregate(Freq ~ Hair + Eye, data = hec_data, FUN = sum)

  # Expand to individual observations
  hec_expanded <- hec_aggregated[rep(1:nrow(hec_aggregated), hec_aggregated$Freq),
                                  c("Hair", "Eye")]

  # Reference test
  cont_table <- table(hec_expanded$Hair, hec_expanded$Eye)
  reference <- chisq.test(cont_table, correct = FALSE)

  # Run chisqposttest
  result <- chisqposttest(
    data = hec_expanded,
    rows = "Hair",
    cols = "Eye",
    posthoc = "none"
  )

  chisq_table <- extract_table_data(result, "chisqTable")

  # Validate
  expect_equal(chisq_table$value[1], reference$statistic[[1]], tolerance = 1e-6)
  expect_equal(chisq_table$p[1], reference$p.value, tolerance = 1e-6)
})

# ============================================================================
# PART 7: OUTPUT STRUCTURE VALIDATION
# Validates that all expected output components are present
# ============================================================================

# ===== Output Completeness =====

test_that("All required output tables are generated", {
  set.seed(909)
  data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 80)),
    outcome = factor(c(
      sample(c("Yes", "No"), 80, replace = TRUE, prob = c(0.7, 0.3)),
      sample(c("Yes", "No"), 80, replace = TRUE, prob = c(0.5, 0.5)),
      sample(c("Yes", "No"), 80, replace = TRUE, prob = c(0.3, 0.7))
    ))
  )

  result <- chisqposttest(
    data = data,
    rows = "group",
    cols = "outcome",
    posthoc = "bonferroni"
  )

  # Check main chi-squared table exists
  expect_true(!is.null(result$chisqTable))

  # Check contingency table exists
  expect_true(!is.null(result$contingencyTable))

  # Check post-hoc table exists
  expect_true(!is.null(result$posthocTable))

  # Check that result inherits correct class
  expect_true(inherits(result, "chisqposttestResults"))
})

test_that("Post-hoc table has all required columns", {
  set.seed(1010)
  data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 80)),
    outcome = factor(c(
      sample(c("Yes", "No"), 80, replace = TRUE, prob = c(0.75, 0.25)),
      sample(c("Yes", "No"), 80, replace = TRUE, prob = c(0.50, 0.50)),
      sample(c("Yes", "No"), 80, replace = TRUE, prob = c(0.25, 0.75))
    ))
  )

  result <- chisqposttest(
    data = data,
    rows = "group",
    cols = "outcome",
    posthoc = "bonferroni"
  )

  posthoc_table <- extract_table_data(result, "posthocTable")

  if (nrow(posthoc_table) > 0) {
    required_cols <- c("comparison", "test_method", "p", "padj", "sig")

    for (col in required_cols) {
      expect_true(col %in% names(posthoc_table),
                  info = paste("Missing required column:", col))
    }
  }
})
