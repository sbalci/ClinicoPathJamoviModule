# Test Suite for jjpubr
# These tests validate the correctness of the publication-ready plots function

library(testthat)
library(ClinicoPath)

# Test 1: Data Validation - Numeric X for Boxplot Should Fail ----
test_that("jjpubr rejects numeric patient IDs as boxplot X variable", {
  test_data <- data.frame(
    patient_id = 1:20,  # Numeric IDs - should be rejected
    biomarker = rnorm(20, mean = 100, sd = 15)
  )

  expect_error(
    jjpubr(
      data = test_data,
      plotType = "boxplot",
      xvar = "patient_id",
      yvar = "biomarker"
    ),
    regexp = "numeric.*categorical"
  )
})

# Test 2: Data Validation - Factor Y for Boxplot Should Fail ----
test_that("jjpubr rejects categorical Y variable for boxplot", {
  test_data <- data.frame(
    treatment = factor(rep(c("A", "B"), each = 10)),
    response = factor(rep(c("CR", "PR"), 10))  # Factor - should be rejected
  )

  expect_error(
    jjpubr(
      data = test_data,
      plotType = "boxplot",
      xvar = "treatment",
      yvar = "response"
    ),
    regexp = "must be numeric"
  )
})

# Test 3: Data Validation - Factor X for Scatter Should Fail ----
test_that("jjpubr rejects categorical X for scatter plot", {
  test_data <- data.frame(
    group = factor(rep(c("A", "B"), each = 15)),
    value1 = rnorm(30),
    value2 = rnorm(30)
  )

  expect_error(
    jjpubr(
      data = test_data,
      plotType = "scatter",
      xvar = "group",  # Factor - should be rejected for scatter
      yvar = "value1"
    ),
    regexp = "both X and Y to be numeric"
  )
})

# Test 4: Data Validation - Factor Y for Scatter Should Fail ----
test_that("jjpubr rejects categorical Y for scatter plot", {
  test_data <- data.frame(
    value1 = rnorm(30),
    category = factor(rep(c("Low", "High"), 15))
  )

  expect_error(
    jjpubr(
      data = test_data,
      plotType = "scatter",
      xvar = "value1",
      yvar = "category"  # Factor - should be rejected
    ),
    regexp = "both X and Y to be numeric"
  )
})

# Test 5: Valid Boxplot with Categorical X and Numeric Y ----
test_that("jjpubr creates valid boxplot with proper data types", {
  test_data <- data.frame(
    treatment = factor(rep(c("Control", "Drug A", "Drug B"), each = 10)),
    response = c(rnorm(10, 50, 10), rnorm(10, 65, 12), rnorm(10, 72, 15))
  )

  result <- jjpubr(
    data = test_data,
    plotType = "boxplot",
    xvar = "treatment",
    yvar = "response"
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 6: Valid Scatter Plot with Both Numeric Variables ----
test_that("jjpubr creates valid scatter plot with numeric X and Y", {
  test_data <- data.frame(
    age = rnorm(50, 60, 12),
    biomarker = rnorm(50, 120, 20)
  )

  result <- jjpubr(
    data = test_data,
    plotType = "scatter",
    xvar = "age",
    yvar = "biomarker",
    addCorr = TRUE,
    corrMethod = "pearson"
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 7: Correlation with Missing Values - Complete Cases ----
test_that("jjpubr handles NAs correctly in correlation analysis", {
  test_data <- data.frame(
    x = c(1:18, NA, NA),
    y = c(NA, 2:19, NA)
  )

  # Should handle NAs and compute correlation on complete pairs
  result <- jjpubr(
    data = test_data,
    plotType = "scatter",
    xvar = "x",
    yvar = "y",
    addCorr = TRUE
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 8: Statistical Tests with Missing Values ----
test_that("jjpubr handles NAs in statistical comparisons", {
  test_data <- data.frame(
    group = factor(rep(c("A", "B"), each = 15)),
    value = c(rnorm(13, 50, 10), NA, NA, rnorm(14, 60, 10), NA)
  )

  result <- jjpubr(
    data = test_data,
    plotType = "boxplot",
    xvar = "group",
    yvar = "value",
    addStats = TRUE,
    statMethod = "t.test"
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 9: Pairwise Comparisons with Multiple Testing ----
test_that("jjpubr performs pairwise comparisons with correction", {
  test_data <- data.frame(
    treatment = factor(rep(c("Control", "Low", "High"), each = 15)),
    outcome = c(rnorm(15, 50, 8), rnorm(15, 58, 10), rnorm(15, 68, 12))
  )

  result <- jjpubr(
    data = test_data,
    plotType = "boxplot",
    xvar = "treatment",
    yvar = "outcome",
    addStats = TRUE,
    pairwiseComparisons = TRUE
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 10: Histogram with Numeric X Variable ----
test_that("jjpubr creates histogram for continuous variable", {
  test_data <- data.frame(
    measurement = rnorm(100, 75, 15)
  )

  result <- jjpubr(
    data = test_data,
    plotType = "histogram",
    xvar = "measurement",
    bins = 20,
    addDensity = TRUE
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 11: Histogram with Mean and Median Lines ----
test_that("jjpubr adds mean and median lines to histogram", {
  test_data <- data.frame(
    value = c(rnorm(80, 100, 20), rnorm(20, 150, 15))  # Bimodal
  )

  result <- jjpubr(
    data = test_data,
    plotType = "histogram",
    xvar = "value",
    addMean = TRUE,
    addMedian = TRUE
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 12: Violin Plot with Statistics ----
test_that("jjpubr creates violin plot with statistical annotations", {
  test_data <- data.frame(
    cohort = factor(rep(c("Discovery", "Validation"), each = 25)),
    expression = c(rnorm(25, 5.5, 1.2), rnorm(25, 6.8, 1.5))
  )

  result <- jjpubr(
    data = test_data,
    plotType = "violin",
    xvar = "cohort",
    yvar = "expression",
    addStats = TRUE,
    addPoints = TRUE
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 13: Scatter Plot with Smooth Line ----
test_that("jjpubr adds regression line to scatter plot", {
  test_data <- data.frame(
    dose = seq(0, 100, length.out = 40),
    response = seq(0, 100, length.out = 40) + rnorm(40, 0, 10)
  )

  result <- jjpubr(
    data = test_data,
    plotType = "scatter",
    xvar = "dose",
    yvar = "response",
    addSmoothLine = TRUE,
    addCorr = TRUE
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 14: Scatter Plot with Grouping Variable ----
test_that("jjpubr handles color grouping in scatter plots", {
  test_data <- data.frame(
    x = rnorm(60),
    y = rnorm(60),
    group = factor(rep(c("Group1", "Group2", "Group3"), each = 20))
  )

  result <- jjpubr(
    data = test_data,
    plotType = "scatter",
    xvar = "x",
    yvar = "y",
    groupvar = "group",
    palette = "jco"
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 15: Faceted Plot ----
test_that("jjpubr creates faceted plots", {
  test_data <- data.frame(
    treatment = factor(rep(c("A", "B"), 30)),
    outcome = rnorm(60, 50, 15),
    site = factor(rep(c("Site1", "Site2", "Site3"), each = 20))
  )

  result <- jjpubr(
    data = test_data,
    plotType = "boxplot",
    xvar = "treatment",
    yvar = "outcome",
    facetvar = "site"
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 16: Density Plot ----
test_that("jjpubr creates density plot for distribution analysis", {
  test_data <- data.frame(
    measurement = c(rnorm(100, 50, 10), rnorm(100, 70, 12))
  )

  result <- jjpubr(
    data = test_data,
    plotType = "density",
    xvar = "measurement",
    fillColor = "#0073C2FF"
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 17: Bar Plot ----
test_that("jjpubr creates bar plot for categorical data", {
  test_data <- data.frame(
    category = factor(rep(c("A", "B", "C", "D"), c(25, 35, 20, 30)))
  )

  result <- jjpubr(
    data = test_data,
    plotType = "barplot",
    xvar = "category",
    palette = "npg"
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 18: Statistical Method - Automatic Selection ----
test_that("jjpubr automatically selects appropriate test", {
  test_data <- data.frame(
    group = factor(rep(c("A", "B"), each = 20)),
    value = c(rnorm(20, 50, 10), rnorm(20, 60, 12))
  )

  result <- jjpubr(
    data = test_data,
    plotType = "boxplot",
    xvar = "group",
    yvar = "value",
    addStats = TRUE,
    statMethod = "auto"  # Should use t-test for 2 groups
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 19: Nonparametric Test ----
test_that("jjpubr performs Wilcoxon test correctly", {
  test_data <- data.frame(
    condition = factor(rep(c("Pre", "Post"), each = 18)),
    score = c(rpois(18, 5), rpois(18, 8))  # Non-normal
  )

  result <- jjpubr(
    data = test_data,
    plotType = "boxplot",
    xvar = "condition",
    yvar = "score",
    addStats = TRUE,
    statMethod = "wilcox.test"
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 20: Correlation Methods - Spearman ----
test_that("jjpubr computes Spearman correlation", {
  test_data <- data.frame(
    x = seq(1, 50),
    y = seq(1, 50)^2 + rnorm(50, 0, 50)  # Nonlinear relationship
  )

  result <- jjpubr(
    data = test_data,
    plotType = "scatter",
    xvar = "x",
    yvar = "y",
    addCorr = TRUE,
    corrMethod = "spearman"
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 21: Correlation Methods - Kendall ----
test_that("jjpubr computes Kendall correlation", {
  test_data <- data.frame(
    rank1 = 1:30,
    rank2 = c(1:15, 30:16) + rnorm(30, 0, 2)
  )

  result <- jjpubr(
    data = test_data,
    plotType = "scatter",
    xvar = "rank1",
    yvar = "rank2",
    addCorr = TRUE,
    corrMethod = "kendall"
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 22: Clinical Preset - Prognostic Biomarker ----
test_that("jjpubr applies prognostic biomarker preset with warning", {
  test_data <- data.frame(
    outcome = factor(rep(c("Poor", "Good"), each = 25)),
    marker = c(rnorm(25, 50, 15), rnorm(25, 75, 18))
  )

  expect_warning(
    jjpubr(
      data = test_data,
      plotType = "histogram",  # Will be overridden to boxplot
      xvar = "outcome",
      yvar = "marker",
      clinicalPreset = "prognostic_biomarker"
    ),
    regexp = "CLINICAL PRESET OVERRIDE"
  )
})

# Test 23: Clinical Preset - Diagnostic Test ----
test_that("jjpubr applies diagnostic test preset", {
  test_data <- data.frame(
    diagnosis = factor(rep(c("Healthy", "Disease"), each = 30)),
    test_value = c(rnorm(30, 10, 3), rnorm(30, 18, 5))
  )

  result <- jjpubr(
    data = test_data,
    plotType = "boxplot",
    xvar = "diagnosis",
    yvar = "test_value",
    clinicalPreset = "diagnostic_test"
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 24: Clinical Preset - Correlation Analysis ----
test_that("jjpubr applies correlation analysis preset", {
  test_data <- data.frame(
    biomarker1 = rnorm(45, 100, 20),
    biomarker2 = rnorm(45, 150, 25)
  )

  result <- jjpubr(
    data = test_data,
    plotType = "scatter",
    xvar = "biomarker1",
    yvar = "biomarker2",
    clinicalPreset = "correlation_analysis"
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 25: Color Palettes ----
test_that("jjpubr applies different color palettes", {
  test_data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 15)),
    value = c(rnorm(15, 50, 8), rnorm(15, 60, 10), rnorm(15, 55, 9))
  )

  # Test multiple palettes
  for (pal in c("jco", "npg", "lancet", "nejm")) {
    result <- jjpubr(
      data = test_data,
      plotType = "boxplot",
      xvar = "group",
      yvar = "value",
      palette = pal
    )
    expect_true(inherits(result, "jjpubrResults"))
  }
})

# Test 26: Themes ----
test_that("jjpubr applies different themes", {
  test_data <- data.frame(
    x = rnorm(40),
    y = rnorm(40)
  )

  for (thm in c("pubr", "classic", "minimal", "light")) {
    result <- jjpubr(
      data = test_data,
      plotType = "scatter",
      xvar = "x",
      yvar = "y",
      theme = thm
    )
    expect_true(inherits(result, "jjpubrResults"))
  }
})

# Test 27: Custom Labels and Title ----
test_that("jjpubr accepts custom labels and title", {
  test_data <- data.frame(
    dose = c(0, 10, 50, 100, 500),
    response = c(0, 15, 45, 75, 95)
  )

  result <- jjpubr(
    data = test_data,
    plotType = "scatter",
    xvar = "dose",
    yvar = "response",
    title = "Dose-Response Curve",
    xlab = "Drug Dose (mg)",
    ylab = "% Response"
  )

  expect_true(inherits(result, "jjpubrResults"))
})

# Test 28: Insufficient Data for Statistics ----
test_that("jjpubr handles insufficient sample size gracefully", {
  test_data <- data.frame(
    group = factor(c("A", "B")),
    value = c(50, 60)  # Only 1 per group
  )

  expect_error(
    jjpubr(
      data = test_data,
      plotType = "boxplot",
      xvar = "group",
      yvar = "value",
      addStats = TRUE
    ),
    regexp = "Insufficient"
  )
})

# Test 29: Small Sample Size Warning ----
test_that("jjpubr warns about small sample sizes", {
  test_data <- data.frame(
    group = factor(rep(c("A", "B", "C"), c(3, 3, 3))),
    value = rnorm(9, 50, 10)
  )

  expect_warning(
    jjpubr(
      data = test_data,
      plotType = "boxplot",
      xvar = "group",
      yvar = "value",
      addStats = TRUE,
      pairwiseComparisons = TRUE
    ),
    regexp = "Small sample size"
  )
})

# Test 30: Correlation with Faceting Warning ----
test_that("jjpubr warns when correlation used with faceting", {
  test_data <- data.frame(
    x = rnorm(60),
    y = rnorm(60),
    facet = factor(rep(c("F1", "F2", "F3"), each = 20))
  )

  expect_warning(
    jjpubr(
      data = test_data,
      plotType = "scatter",
      xvar = "x",
      yvar = "y",
      facetvar = "facet",
      addCorr = TRUE
    ),
    regexp = "facet"
  )
})

# ============================================================================
# OMNIBUS TEST FUNCTIONALITY TESTS
# ============================================================================

# Test 31: Omnibus ANOVA Performed for 3+ Groups ----
test_that("jjpubr performs omnibus ANOVA before pairwise tests for 3+ groups", {
  set.seed(123)
  test_data <- data.frame(
    treatment = factor(rep(c("Control", "Drug A", "Drug B"), each = 20)),
    outcome = c(
      rnorm(20, mean = 100, sd = 10),  # Control
      rnorm(20, mean = 110, sd = 10),  # Drug A (higher)
      rnorm(20, mean = 115, sd = 10)   # Drug B (even higher)
    )
  )

  result <- jjpubr(
    data = test_data,
    plotType = "boxplot",
    xvar = "treatment",
    yvar = "outcome",
    addStats = TRUE,
    statMethod = "t.test"
  )

  # Check that statistics table exists and has rows
  expect_true(!is.null(result$statistics))
  stats_table <- result$statistics$asDF

  # Should have omnibus test row
  expect_true(nrow(stats_table) >= 1)

  # First row should be omnibus ANOVA
  expect_true(grepl("Overall F", stats_table$comparison[1]) ||
              grepl("One-way ANOVA", stats_table$method[1]))
})

# Test 32: Omnibus Kruskal-Wallis for Non-parametric ----
test_that("jjpubr performs Kruskal-Wallis omnibus test for non-parametric analysis", {
  set.seed(456)
  test_data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 15)),
    value = c(
      rexp(15, rate = 0.1),   # Skewed distributions
      rexp(15, rate = 0.15),
      rexp(15, rate = 0.2)
    )
  )

  result <- jjpubr(
    data = test_data,
    plotType = "boxplot",
    xvar = "group",
    yvar = "value",
    addStats = TRUE,
    statMethod = "wilcox.test"
  )

  stats_table <- result$statistics$asDF

  # First row should be Kruskal-Wallis
  expect_true(grepl("Overall H", stats_table$comparison[1]) ||
              grepl("Kruskal-Wallis", stats_table$method[1]))
})

# Test 33: Two Groups - Direct Comparison (No Omnibus) ----
test_that("jjpubr performs direct t-test for 2 groups without omnibus test", {
  set.seed(789)
  test_data <- data.frame(
    group = factor(rep(c("Control", "Treatment"), each = 25)),
    response = c(
      rnorm(25, mean = 50, sd = 8),
      rnorm(25, mean = 55, sd = 8)
    )
  )

  result <- jjpubr(
    data = test_data,
    plotType = "boxplot",
    xvar = "group",
    yvar = "response",
    addStats = TRUE,
    statMethod = "auto"
  )

  stats_table <- result$statistics$asDF

  # For 2 groups, should have direct comparison (no "Overall F")
  expect_false(any(grepl("Overall F", stats_table$comparison)))

  # Should have one comparison row
  expect_true(grepl("vs", stats_table$comparison[1]))
})

# Test 34: Post-hoc Only if Omnibus Significant ----
test_that("jjpubr shows post-hoc tests only when omnibus is significant", {
  set.seed(111)
  # Create data where groups are NOT different (omnibus should be non-significant)
  test_data <- data.frame(
    group = factor(rep(c("A", "B", "C", "D"), each = 20)),
    value = rnorm(80, mean = 100, sd = 15)  # Same mean for all groups
  )

  result <- jjpubr(
    data = test_data,
    plotType = "boxplot",
    xvar = "group",
    yvar = "value",
    addStats = TRUE,
    statMethod = "auto",
    pairwiseComparisons = FALSE  # Don't request pairwise
  )

  stats_table <- result$statistics$asDF

  # Should have only omnibus test row (no pairwise)
  expect_equal(nrow(stats_table), 1)
  expect_true(grepl("Overall", stats_table$comparison[1]))
})

# Test 35: Pairwise Tests Labeled as Post-hoc ----
test_that("jjpubr correctly labels pairwise tests as post-hoc for 3+ groups", {
  set.seed(222)
  test_data <- data.frame(
    treatment = factor(rep(c("Placebo", "Low", "High"), each = 20)),
    score = c(
      rnorm(20, 50, 10),
      rnorm(20, 60, 10),
      rnorm(20, 70, 10)
    )
  )

  result <- jjpubr(
    data = test_data,
    plotType = "boxplot",
    xvar = "treatment",
    yvar = "score",
    addStats = TRUE,
    statMethod = "t.test",
    pairwiseComparisons = TRUE
  )

  stats_table <- result$statistics$asDF

  # Check that pairwise tests are labeled as post-hoc
  pairwise_rows <- stats_table[grepl("vs", stats_table$comparison), ]
  expect_true(any(grepl("post-hoc", pairwise_rows$method, ignore.case = TRUE)))
})

# Test 36: Omnibus Test Statistic is Numeric ----
test_that("jjpubr omnibus test returns valid F or H statistic", {
  set.seed(333)
  test_data <- data.frame(
    category = factor(rep(c("Cat1", "Cat2", "Cat3"), each = 15)),
    measurement = c(rnorm(15, 20), rnorm(15, 25), rnorm(15, 30))
  )

  result <- jjpubr(
    data = test_data,
    plotType = "boxplot",
    xvar = "category",
    yvar = "measurement",
    addStats = TRUE,
    statMethod = "auto"
  )

  stats_table <- result$statistics$asDF
  omnibus_row <- stats_table[1, ]

  # Statistic should be numeric and positive
  expect_true(is.numeric(omnibus_row$statistic))
  expect_true(omnibus_row$statistic > 0)

  # P-value should be between 0 and 1
  expect_true(omnibus_row$pvalue >= 0 && omnibus_row$pvalue <= 1)
})

# Test 37: Bonferroni Correction Applied to Pairwise ----
test_that("jjpubr applies Bonferroni correction to post-hoc pairwise tests", {
  set.seed(444)
  test_data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 20)),
    value = c(
      rnorm(20, 100, 15),
      rnorm(20, 110, 15),
      rnorm(20, 120, 15)
    )
  )

  result <- jjpubr(
    data = test_data,
    plotType = "boxplot",
    xvar = "group",
    yvar = "value",
    addStats = TRUE,
    statMethod = "t.test",
    pairwiseComparisons = TRUE
  )

  stats_table <- result$statistics$asDF

  # Check for adjusted p-values in significance column
  pairwise_rows <- stats_table[grepl("vs", stats_table$comparison), ]
  expect_true(any(grepl("adj", pairwise_rows$significance)))
})

# Test 38: Hierarchical Testing for Clinical Trial Data ----
test_that("jjpubr hierarchical testing works for realistic clinical trial scenario", {
  set.seed(555)
  # Simulate clinical trial with 4 dose groups
  test_data <- data.frame(
    dose_group = factor(
      rep(c("Placebo", "Low Dose", "Medium Dose", "High Dose"), each = 30),
      levels = c("Placebo", "Low Dose", "Medium Dose", "High Dose")
    ),
    tumor_reduction = c(
      rnorm(30, mean = 10, sd = 5),   # Placebo
      rnorm(30, mean = 15, sd = 5),   # Low dose
      rnorm(30, mean = 22, sd = 5),   # Medium dose
      rnorm(30, mean = 28, sd = 5)    # High dose (best)
    )
  )

  result <- jjpubr(
    data = test_data,
    plotType = "boxplot",
    xvar = "dose_group",
    yvar = "tumor_reduction",
    addStats = TRUE,
    statMethod = "auto",
    pairwiseComparisons = TRUE,
    palette = "jco"
  )

  stats_table <- result$statistics$asDF

  # Should have omnibus + pairwise comparisons
  expect_true(nrow(stats_table) > 1)

  # First row should be omnibus ANOVA
  expect_true(grepl("Overall", stats_table$comparison[1]))

  # Omnibus should be significant (large dose effect)
  expect_true(stats_table$pvalue[1] < 0.05)

  # Should have 6 pairwise comparisons (4 choose 2)
  pairwise_count <- sum(grepl("vs", stats_table$comparison))
  expect_equal(pairwise_count, 6)
})
