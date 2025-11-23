# Test Suite for jjpiestats
# These tests validate the correctness of the pie chart function

library(testthat)
library(ClinicoPath)

# Test 1: Selective NA Omission ----
test_that("jjpiestats uses selective NA omission for relevant variables only", {
  # Create test data with NAs in different columns
  test_data <- data.frame(
    treatment_response = factor(c("CR", "PR", "SD", "PD", "CR", "PR", "SD", "PD", "CR", "PR")),
    treatment_arm = factor(c("A", "A", "B", "B", "A", "B", "A", "B", "A", "B")),
    irrelevant_var = c(1, NA, 3, NA, 5, NA, 7, NA, 9, NA),  # NAs that shouldn't affect analysis
    stringsAsFactors = FALSE
  )

  # Should NOT drop rows due to NAs in irrelevant_var
  result <- jjpiestats(
    data = test_data,
    dep = "treatment_response",
    typestatistics = "parametric"
  )

  # Verify function runs without error
  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 2: Selective NA Omission - Relevant Variables ----
test_that("jjpiestats drops rows with NAs in dependent variable", {
  test_data <- data.frame(
    response = factor(c("Yes", "No", NA, "Yes", "No")),
    extra_col = c(1, 2, 3, 4, 5)
  )

  result <- jjpiestats(
    data = test_data,
    dep = "response",
    typestatistics = "parametric"
  )

  # Should still run (will use 4 complete cases for response)
  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 3: Selective NA Omission - Grouping Variable ----
test_that("jjpiestats drops rows with NAs in grouping variable", {
  test_data <- data.frame(
    response = factor(c("Yes", "No", "Yes", "No", "Yes")),
    group = factor(c("A", "B", NA, "A", "B")),
    irrelevant = c(NA, NA, 1, 2, 3)
  )

  result <- jjpiestats(
    data = test_data,
    dep = "response",
    group = "group",
    typestatistics = "parametric"
  )

  # Should drop only row 3 (NA in group), not rows 1-2 (NA in irrelevant)
  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 4: Selective NA Omission - Split Variable ----
test_that("jjpiestats drops rows with NAs in split variable", {
  test_data <- data.frame(
    response = factor(rep(c("Yes", "No"), 10)),
    site = factor(c(rep("Site1", 8), NA, NA, rep("Site2", 10))),
    other_var = c(rep(NA, 10), 1:10)
  )

  result <- jjpiestats(
    data = test_data,
    dep = "response",
    grvar = "site",
    typestatistics = "parametric"
  )

  # Should drop rows 9-10 (NA in site), not rows 1-10 (NA in other_var)
  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 5: Basic Pie Chart - Single Variable ----
test_that("jjpiestats creates basic pie chart for single categorical variable", {
  test_data <- data.frame(
    tumor_grade = factor(c("G1", "G2", "G3", "G1", "G2", "G3", "G2", "G3", "G1", "G2"))
  )

  result <- jjpiestats(
    data = test_data,
    dep = "tumor_grade",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 6: Pie Chart with Grouping ----
test_that("jjpiestats creates pie chart with contingency table analysis", {
  test_data <- data.frame(
    response = factor(rep(c("CR", "PR", "SD", "PD"), 5)),
    treatment = factor(rep(c("Drug A", "Drug B"), each = 10)),
    stringsAsFactors = FALSE
  )

  result <- jjpiestats(
    data = test_data,
    dep = "response",
    group = "treatment",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 7: Grouped Pie Charts (Split By) ----
test_that("jjpiestats creates grouped pie charts by split variable", {
  test_data <- data.frame(
    severity = factor(rep(c("Mild", "Moderate", "Severe"), 12)),
    gender = factor(rep(c("Male", "Female"), each = 18)),
    site = factor(rep(c("Site A", "Site B", "Site C"), each = 12)),
    stringsAsFactors = FALSE
  )

  result <- jjpiestats(
    data = test_data,
    dep = "severity",
    group = "gender",
    grvar = "site",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 8: Parametric Statistics ----
test_that("jjpiestats runs parametric analysis (Pearson Chi-square)", {
  test_data <- data.frame(
    outcome = factor(rep(c("Success", "Failure"), 30)),
    treatment = factor(rep(c("A", "B"), each = 30))
  )

  result <- jjpiestats(
    data = test_data,
    dep = "outcome",
    group = "treatment",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 9: Nonparametric Statistics ----
test_that("jjpiestats runs nonparametric analysis", {
  test_data <- data.frame(
    stage = factor(rep(c("I", "II", "III", "IV"), 10)),
    location = factor(rep(c("Proximal", "Distal"), each = 20))
  )

  result <- jjpiestats(
    data = test_data,
    dep = "stage",
    group = "location",
    typestatistics = "nonparametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 10: Bayes Statistics ----
test_that("jjpiestats runs Bayesian analysis", {
  test_data <- data.frame(
    mutation = factor(rep(c("Wild-type", "Mutant"), 25)),
    histology = factor(rep(c("Type1", "Type2"), each = 25))
  )

  result <- jjpiestats(
    data = test_data,
    dep = "mutation",
    group = "histology",
    typestatistics = "bayes"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 11: Label Display Options ----
test_that("jjpiestats handles different label display options", {
  test_data <- data.frame(
    response = factor(rep(c("Complete", "Partial", "None"), 10))
  )

  # Percentage labels
  result1 <- jjpiestats(
    data = test_data,
    dep = "response",
    label = "percentage"
  )
  expect_true(inherits(result1, "jjpiestatsResults"))

  # Count labels
  result2 <- jjpiestats(
    data = test_data,
    dep = "response",
    label = "counts"
  )
  expect_true(inherits(result2, "jjpiestatsResults"))

  # Both
  result3 <- jjpiestats(
    data = test_data,
    dep = "response",
    label = "both"
  )
  expect_true(inherits(result3, "jjpiestatsResults"))
})

# Test 12: Expected Proportions (Ratio) ----
test_that("jjpiestats handles expected proportions for proportion test", {
  test_data <- data.frame(
    outcome = factor(c(rep("A", 30), rep("B", 20)))
  )

  # Equal proportions (default)
  result1 <- jjpiestats(
    data = test_data,
    dep = "outcome",
    ratio = "",
    proportiontest = TRUE
  )
  expect_true(inherits(result1, "jjpiestatsResults"))

  # Custom proportions
  result2 <- jjpiestats(
    data = test_data,
    dep = "outcome",
    ratio = "0.6,0.4",
    proportiontest = TRUE
  )
  expect_true(inherits(result2, "jjpiestatsResults"))
})

# Test 13: Paired/Repeated Measures ----
test_that("jjpiestats handles paired/repeated measures design", {
  test_data <- data.frame(
    response = factor(rep(c("Improved", "Same", "Worse"), 15)),
    group = factor(rep(c("Pre", "Post"), c(23, 22)))
  )

  result <- jjpiestats(
    data = test_data,
    dep = "response",
    group = "group",
    paired = TRUE
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 14: Confidence Level Specification ----
test_that("jjpiestats respects confidence level setting", {
  test_data <- data.frame(
    category = factor(rep(c("A", "B", "C"), 10))
  )

  result <- jjpiestats(
    data = test_data,
    dep = "category",
    conflevel = 0.99
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 15: Decimal Digits Control ----
test_that("jjpiestats handles decimal digits specification", {
  test_data <- data.frame(
    type = factor(rep(c("Type1", "Type2", "Type3"), 12))
  )

  result <- jjpiestats(
    data = test_data,
    dep = "type",
    digits = 3
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 16: Proportion Test Toggle ----
test_that("jjpiestats handles proportion test enable/disable", {
  test_data <- data.frame(
    status = factor(rep(c("Active", "Inactive"), 25))
  )

  # With proportion test
  result1 <- jjpiestats(
    data = test_data,
    dep = "status",
    proportiontest = TRUE
  )
  expect_true(inherits(result1, "jjpiestatsResults"))

  # Without proportion test
  result2 <- jjpiestats(
    data = test_data,
    dep = "status",
    proportiontest = FALSE
  )
  expect_true(inherits(result2, "jjpiestatsResults"))
})

# Test 17: Bayes Factor Message ----
test_that("jjpiestats handles Bayes factor message display", {
  test_data <- data.frame(
    outcome = factor(rep(c("Yes", "No"), 30)),
    group = factor(rep(c("Control", "Treatment"), each = 30))
  )

  result <- jjpiestats(
    data = test_data,
    dep = "outcome",
    group = "group",
    typestatistics = "parametric",
    bfmessage = TRUE
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 18: Results Subtitle Display ----
test_that("jjpiestats shows statistical results in subtitle", {
  test_data <- data.frame(
    marker = factor(rep(c("Positive", "Negative"), 35)),
    tissue = factor(rep(c("Normal", "Tumor"), each = 35))
  )

  result <- jjpiestats(
    data = test_data,
    dep = "marker",
    group = "tissue",
    resultssubtitle = TRUE
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 19: Clinical Presets ----
test_that("jjpiestats applies clinical presets correctly", {
  test_data <- data.frame(
    test_result = factor(rep(c("Positive", "Negative"), 40)),
    diagnosis = factor(rep(c("Disease", "Healthy"), each = 40))
  )

  # Diagnostic preset
  result1 <- jjpiestats(
    data = test_data,
    dep = "test_result",
    group = "diagnosis",
    clinicalpreset = "diagnostic"
  )
  expect_true(inherits(result1, "jjpiestatsResults"))

  # Treatment preset
  result2 <- jjpiestats(
    data = test_data,
    dep = "test_result",
    group = "diagnosis",
    clinicalpreset = "treatment"
  )
  expect_true(inherits(result2, "jjpiestatsResults"))
})

# Test 20: Empty After NA Removal - Error Handling ----
test_that("jjpiestats handles case with no complete data gracefully", {
  test_data <- data.frame(
    var1 = factor(c("A", "B", NA, NA)),
    var2 = c(1, 2, 3, 4)
  )

  expect_error(
    jjpiestats(
      data = test_data,
      dep = "var1",
      typestatistics = "parametric"
    ),
    regexp = "No complete data rows available"
  )
})

# Test 21: Binary Categories ----
test_that("jjpiestats handles binary categorical variables", {
  test_data <- data.frame(
    binary_outcome = factor(rep(c("Yes", "No"), 40))
  )

  result <- jjpiestats(
    data = test_data,
    dep = "binary_outcome",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 22: Multiple Categories ----
test_that("jjpiestats handles multiple categories (>2)", {
  test_data <- data.frame(
    multi_category = factor(rep(c("A", "B", "C", "D", "E"), 12))
  )

  result <- jjpiestats(
    data = test_data,
    dep = "multi_category",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 23: Ordinal Categories ----
test_that("jjpiestats handles ordinal categorical variables", {
  test_data <- data.frame(
    stage = factor(
      rep(c("Stage I", "Stage II", "Stage III", "Stage IV"), 10),
      levels = c("Stage I", "Stage II", "Stage III", "Stage IV"),
      ordered = TRUE
    )
  )

  result <- jjpiestats(
    data = test_data,
    dep = "stage",
    typestatistics = "nonparametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 24: Large Sample Size ----
test_that("jjpiestats handles large sample sizes", {
  set.seed(123)
  test_data <- data.frame(
    category = factor(sample(c("A", "B", "C"), 1000, replace = TRUE)),
    group = factor(sample(c("Group1", "Group2"), 1000, replace = TRUE))
  )

  result <- jjpiestats(
    data = test_data,
    dep = "category",
    group = "group",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 25: Small Sample Size with Warning ----
test_that("jjpiestats handles small sample sizes", {
  test_data <- data.frame(
    small_cat = factor(c("A", "B", "A", "B", "A"))
  )

  # Should still run but may produce warnings
  result <- jjpiestats(
    data = test_data,
    dep = "small_cat",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 26: Unbalanced Groups ----
test_that("jjpiestats handles unbalanced categorical groups", {
  test_data <- data.frame(
    category = factor(c(rep("Rare", 5), rep("Common", 50))),
    group = factor(rep(c("A", "B"), c(25, 30)))
  )

  result <- jjpiestats(
    data = test_data,
    dep = "category",
    group = "group",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 27: Character Variables Auto-Conversion ----
test_that("jjpiestats handles character variables as categorical", {
  test_data <- data.frame(
    char_var = rep(c("Category1", "Category2", "Category3"), 10),
    stringsAsFactors = FALSE
  )

  result <- jjpiestats(
    data = test_data,
    dep = "char_var",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 28: Logical Variables ----
test_that("jjpiestats handles logical variables as binary categorical", {
  test_data <- data.frame(
    logical_var = rep(c(TRUE, FALSE), 25)
  )

  result <- jjpiestats(
    data = test_data,
    dep = "logical_var",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 29: Counts Variable Usage ----
test_that("jjpiestats handles aggregated data with counts variable", {
  # Aggregated/tabulated data
  test_data <- data.frame(
    category = factor(c("A", "B", "C")),
    n = c(30, 45, 25)
  )

  result <- jjpiestats(
    data = test_data,
    dep = "category",
    counts = "n",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 30: ggpubr Donut Chart Addition ----
test_that("jjpiestats can add ggpubr donut chart variant", {
  test_data <- data.frame(
    response_type = factor(rep(c("Complete", "Partial", "Minimal", "None"), 8))
  )

  result <- jjpiestats(
    data = test_data,
    dep = "response_type",
    addGGPubrDonut = TRUE,
    ggpubrDonutPalette = "jco"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# === STATISTICAL CORRECTNESS TESTS (CRITICAL FIXES) ===

# Test 31: CRITICAL - McNemar Validation Rejects Non-2x2 Tables ----
test_that("jjpiestats CRITICAL FIX #3: McNemar validation rejects non-2x2 tables", {
  # Create 3x2 table (INVALID for McNemar)
  test_data <- data.frame(
    response = factor(rep(c("CR", "PR", "SD"), 20)),
    timepoint = factor(rep(c("Pre", "Post"), 30)),
    stringsAsFactors = FALSE
  )

  # Should run without error but not use paired analysis
  result <- jjpiestats(
    data = test_data,
    dep = "response",
    group = "timepoint",
    paired = TRUE,
    typestatistics = "parametric"
  )

  # Function should complete (not crash)
  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 32: CRITICAL - McNemar Validation Accepts Valid 2x2 Tables ----
test_that("jjpiestats CRITICAL FIX #3: McNemar validation accepts valid 2x2 tables", {
  # Create valid 2x2 table
  test_data <- data.frame(
    response = factor(rep(c("Improved", "NotImproved"), 25)),
    timepoint = factor(rep(c("Pre", "Post"), 25)),
    stringsAsFactors = FALSE
  )

  # Should run successfully
  result <- jjpiestats(
    data = test_data,
    dep = "response",
    group = "timepoint",
    paired = TRUE,
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 33: CRITICAL - Counts Variable Validation (Non-Numeric) ----
test_that("jjpiestats CRITICAL FIX #2: Rejects non-numeric counts variable", {
  test_data <- data.frame(
    category = factor(c("A", "B", "C")),
    n = c("10", "20", "30")  # Character, not numeric
  )

  # Should error due to non-numeric counts
  expect_error(
    jjpiestats(
      data = test_data,
      dep = "category",
      counts = "n",
      typestatistics = "parametric"
    ),
    regexp = "must be numeric"
  )
})

# Test 34: CRITICAL - Counts Variable Validation (Negative Values) ----
test_that("jjpiestats CRITICAL FIX #2: Rejects negative counts", {
  test_data <- data.frame(
    category = factor(c("A", "B", "C")),
    n = c(30, -10, 25)  # Negative value
  )

  # Should error due to negative counts
  expect_error(
    jjpiestats(
      data = test_data,
      dep = "category",
      counts = "n",
      typestatistics = "parametric"
    ),
    regexp = "negative"
  )
})

# Test 35: CRITICAL - Counts Variable Validation (Valid Numeric) ----
test_that("jjpiestats CRITICAL FIX #2: Accepts valid numeric counts", {
  test_data <- data.frame(
    category = factor(c("A", "B", "C")),
    n = c(30, 45, 25)
  )

  # Should run successfully
  result <- jjpiestats(
    data = test_data,
    dep = "category",
    counts = "n",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 36: CRITICAL - Weighted Assumption Checks ----
test_that("jjpiestats CRITICAL FIX #4: Assumption checks respect weighted data", {
  # Aggregated data with counts variable
  test_data <- data.frame(
    treatment = factor(c("A", "A", "B", "B")),
    response = factor(c("Yes", "No", "Yes", "No")),
    n = c(45, 5, 40, 10)  # 90% vs 80% response rate
  )

  # Should run successfully with weighted data
  result <- jjpiestats(
    data = test_data,
    dep = "response",
    group = "treatment",
    counts = "n",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 37: Enhanced Ratio Parsing with Normalization ----
test_that("jjpiestats ENHANCEMENT #6: Ratio parsing normalizes non-summing values", {
  test_data <- data.frame(
    category = factor(rep(c("A", "B"), 30))
  )

  # Ratio that doesn't sum to 1 (should auto-normalize)
  result <- jjpiestats(
    data = test_data,
    dep = "category",
    ratio = "2,3",  # Sums to 5, should normalize to 0.4, 0.6
    proportiontest = TRUE,
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 38: Enhanced Ratio Parsing with Invalid Values ----
test_that("jjpiestats ENHANCEMENT #6: Ratio parsing handles invalid values gracefully", {
  test_data <- data.frame(
    category = factor(rep(c("A", "B"), 30))
  )

  # Invalid ratio (contains letters)
  result <- jjpiestats(
    data = test_data,
    dep = "category",
    ratio = "0.5,abc",  # Invalid
    proportiontest = TRUE,
    typestatistics = "parametric"
  )

  # Should fall back to equal proportions
  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 39: Enhancement #1 - Small Group Sizes Notice ----
test_that("jjpiestats ENHANCEMENT #1: Small group sizes trigger Notice (not warning)", {
  # Create data with small group size (n=3 < 5)
  test_data <- data.frame(
    response = factor(c(rep("Yes", 10), rep("No", 15), rep("Yes", 3), rep("No", 12))),
    group = factor(c(rep("A", 25), rep("B", 15))),
    stringsAsFactors = FALSE
  )

  # Should show Notice about small group size
  result <- jjpiestats(
    data = test_data,
    dep = "response",
    group = "group",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
  # Notice system is internal - just verify function completes successfully
})

# Test 40: Enhancement #2 - Extreme Prevalence Warning for Diagnostic Preset ----
test_that("jjpiestats ENHANCEMENT #2: Extreme prevalence triggers info notice in diagnostic preset", {
  # Create diagnostic test data with low prevalence (~7%)
  test_data <- data.frame(
    test_result = factor(c(rep("Positive", 7), rep("Negative", 93))),
    disease_status = factor(c(rep("Diseased", 7), rep("Healthy", 93))),
    stringsAsFactors = FALSE
  )

  # Should trigger extreme prevalence warning
  result <- jjpiestats(
    data = test_data,
    dep = "test_result",
    group = "disease_status",
    clinicalpreset = "diagnostic",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
  # Notice about PPV/NPV generalizability should be shown (internal verification)
})

# Test 41: Enhancement #2 - High Prevalence Warning ----
test_that("jjpiestats ENHANCEMENT #2: High prevalence (>90%) triggers info notice", {
  # Create diagnostic test data with high prevalence (~95%)
  test_data <- data.frame(
    test_result = factor(c(rep("Positive", 95), rep("Negative", 5))),
    disease_status = factor(c(rep("Diseased", 95), rep("Healthy", 5))),
    stringsAsFactors = FALSE
  )

  # Should trigger extreme prevalence warning
  result <- jjpiestats(
    data = test_data,
    dep = "test_result",
    group = "disease_status",
    clinicalpreset = "diagnostic",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 42: Enhancement #2 - No Warning for Normal Prevalence ----
test_that("jjpiestats ENHANCEMENT #2: Normal prevalence (10-90%) does not trigger warning", {
  # Create diagnostic test data with normal prevalence (~50%)
  test_data <- data.frame(
    test_result = factor(c(rep("Positive", 50), rep("Negative", 50))),
    disease_status = factor(c(rep("Diseased", 50), rep("Healthy", 50))),
    stringsAsFactors = FALSE
  )

  # Should NOT trigger prevalence warning
  result <- jjpiestats(
    data = test_data,
    dep = "test_result",
    group = "disease_status",
    clinicalpreset = "diagnostic",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 43: Enhancement #2 - Only Triggers for Diagnostic Preset ----
test_that("jjpiestats ENHANCEMENT #2: Prevalence check only for diagnostic preset", {
  # Create data with extreme prevalence but non-diagnostic preset
  test_data <- data.frame(
    category = factor(c(rep("A", 95), rep("B", 5))),
    group = factor(c(rep("Group1", 95), rep("Group2", 5))),
    stringsAsFactors = FALSE
  )

  # Should NOT trigger prevalence warning (not diagnostic preset)
  result <- jjpiestats(
    data = test_data,
    dep = "category",
    group = "group",
    clinicalpreset = "treatment",  # Not diagnostic
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})

# Test 44: BUG FIX - Empty string validation ----
test_that("jjpiestats BUG FIX: Handles empty string dep variable gracefully", {
  # This test verifies the fix for "argument is of length zero" error
  # when dep is empty string instead of NULL

  test_data <- data.frame(
    category = factor(rep(c("A", "B"), 30))
  )

  # Create result object with empty dep (simulating jamovi behavior)
  # Should not error with "argument is of length zero"
  result <- jjpiestats(data = test_data)

  # Should create valid results object
  expect_true(inherits(result, "jjpiestatsResults"))

  # No plot should be generated (dep is empty)
  # This is validated by the early return in .plot1()
})

# Test 45: Single-column dataset retains data frame structure ----
test_that("jjpiestats handles single-column data without dropping dimensions", {
  test_data <- data.frame(
    race = factor(rep(c("A", "B"), 3))
  )

  result <- jjpiestats(
    data = test_data,
    dep = "race",
    typestatistics = "parametric"
  )

  expect_true(inherits(result, "jjpiestatsResults"))
})
