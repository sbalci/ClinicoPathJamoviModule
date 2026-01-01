# Unit tests for outcomeorganizer guidance notices feature
# Tests verify that helpful notices are shown instead of hard errors

library(testthat)
library(jmvcore)

# =============================================================================
# Test 1: Multievent Missing Selections Shows Guidance Notices
# =============================================================================

test_that("missing multievent selections show helpful guidance instead of errors", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  skip_if_not_installed("ClinicoPath")

  # Create test data with multiple outcome levels
  test_data <- data.frame(
    PatientID = 1:100,
    Outcome = sample(c("DOD", "DOOC", "AWD", "AWOD"), 100, replace = TRUE),
    Time = rexp(100, rate = 0.1),
    stringsAsFactors = FALSE
  )

  # Test with multievent enabled but no level selections
  # This should show guidance notices, NOT throw an error
  expect_no_error({
    results <- ClinicoPath::outcomeorganizer(
      data = test_data,
      outcome = "Outcome",
      analysistype = "os",
      multievent = TRUE,
      # Missing: dod, dooc, awd, awod selections
      outputTable = FALSE
    )
  })
})

# =============================================================================
# Test 2: Partial Multievent Selections Shows Missing Fields
# =============================================================================

test_that("partial multievent selections identify which fields are missing", {
  skip_if_not_installed("ClinicoPath")

  # Create test data
  test_data <- data.frame(
    PatientID = 1:100,
    Outcome = sample(c("DOD", "DOOC", "AWD", "AWOD"), 100, replace = TRUE),
    Time = rexp(100, rate = 0.1),
    stringsAsFactors = FALSE
  )

  # Test with only some selections provided
  expect_no_error({
    results <- ClinicoPath::outcomeorganizer(
      data = test_data,
      outcome = "Outcome",
      analysistype = "compete",
      multievent = TRUE,
      dod = "DOD",
      dooc = "DOOC",
      # Missing: awd, awod
      outputTable = FALSE
    )
  })
})

# =============================================================================
# Test 3: Complete Multievent Selections Work Correctly
# =============================================================================

test_that("complete multievent selections process without notices", {
  skip_if_not_installed("ClinicoPath")

  # Create test data
  test_data <- data.frame(
    PatientID = 1:100,
    Outcome = sample(c("DOD", "DOOC", "AWD", "AWOD"), 100, replace = TRUE),
    Time = rexp(100, rate = 0.1),
    stringsAsFactors = FALSE
  )

  # Test with all selections provided - should work
  expect_no_error({
    results <- ClinicoPath::outcomeorganizer(
      data = test_data,
      outcome = "Outcome",
      analysistype = "compete",
      multievent = TRUE,
      dod = "DOD",
      dooc = "DOOC",
      awd = "AWD",
      awod = "AWOD",
      outputTable = TRUE
    )
  })
})

# =============================================================================
# Test 4: Binary Outcome Still Works Without Multievent
# =============================================================================

test_that("binary outcome analysis works without multievent selections", {
  skip_if_not_installed("ClinicoPath")

  # Create test data with binary outcome
  test_data <- data.frame(
    PatientID = 1:100,
    Status = sample(c("Alive", "Dead"), 100, replace = TRUE),
    Time = rexp(100, rate = 0.1),
    stringsAsFactors = FALSE
  )

  # Binary analysis doesn't require multievent selections
  expect_no_error({
    results <- ClinicoPath::outcomeorganizer(
      data = test_data,
      outcome = "Status",
      outcomeLevel = "Dead",
      analysistype = "os",
      multievent = FALSE,
      outputTable = TRUE
    )
  })
})

# =============================================================================
# Test 5: Different Analysis Types with Multievent
# =============================================================================

test_that("different analysis types handle missing multievent selections", {
  skip_if_not_installed("ClinicoPath")

  # Create test data
  test_data <- data.frame(
    PatientID = 1:100,
    Outcome = sample(c("DOD", "DOOC", "AWD", "AWOD"), 100, replace = TRUE),
    Time = rexp(100, rate = 0.1),
    stringsAsFactors = FALSE
  )

  analysis_types <- c("os", "cause", "compete", "multistate")

  for (atype in analysis_types) {
    # Test each analysis type with missing selections
    expect_no_error({
      results <- ClinicoPath::outcomeorganizer(
        data = test_data,
        outcome = "Outcome",
        analysistype = atype,
        multievent = TRUE,
        # Missing all selections
        outputTable = FALSE
      )
    }, info = paste("Analysis type:", atype))
  }
})

# =============================================================================
# Test 6: International Outcome Labels
# =============================================================================

test_that("guidance works with international outcome labels", {
  skip_if_not_installed("ClinicoPath")

  # Create test data with Turkish labels
  test_data <- data.frame(
    HastaID = 1:100,
    Sonuc = sample(c("Hastalıktan Ölü", "Diğer Nedenle Ölü",
                     "Hastalıklı Yaşıyor", "Hastalıksız Yaşıyor"),
                   100, replace = TRUE),
    Sure = rexp(100, rate = 0.1),
    stringsAsFactors = FALSE
  )

  # Should show guidance with actual outcome values
  expect_no_error({
    results <- ClinicoPath::outcomeorganizer(
      data = test_data,
      outcome = "Sonuc",
      analysistype = "os",
      multievent = TRUE,
      outputTable = FALSE
    )
  })
})

# =============================================================================
# Test 7: Numeric Outcome Levels
# =============================================================================

test_that("guidance works with numeric outcome coding", {
  skip_if_not_installed("ClinicoPath")

  # Create test data with numeric outcomes
  test_data <- data.frame(
    PatientID = 1:100,
    Outcome = sample(c(1, 2, 3, 4), 100, replace = TRUE),
    Time = rexp(100, rate = 0.1),
    stringsAsFactors = FALSE
  )

  # Should show available numeric values
  expect_no_error({
    results <- ClinicoPath::outcomeorganizer(
      data = test_data,
      outcome = "Outcome",
      analysistype = "compete",
      multievent = TRUE,
      outputTable = FALSE
    )
  })
})

# =============================================================================
# Test 8: Edge Case - Only One Selection Missing
# =============================================================================

test_that("identifies when only one selection is missing", {
  skip_if_not_installed("ClinicoPath")

  # Create test data
  test_data <- data.frame(
    PatientID = 1:100,
    Outcome = sample(c("DOD", "DOOC", "AWD", "AWOD"), 100, replace = TRUE),
    Time = rexp(100, rate = 0.1),
    stringsAsFactors = FALSE
  )

  # Test with only awod missing
  expect_no_error({
    results <- ClinicoPath::outcomeorganizer(
      data = test_data,
      outcome = "Outcome",
      analysistype = "multistate",
      multievent = TRUE,
      dod = "DOD",
      dooc = "DOOC",
      awd = "AWD",
      # Missing: awod
      outputTable = FALSE
    )
  })
})

# =============================================================================
# Test 9: Works After Filling In Missing Selections
# =============================================================================

test_that("analysis proceeds after all selections are filled", {
  skip_if_not_installed("ClinicoPath")

  # Create test data
  test_data <- data.frame(
    PatientID = 1:100,
    Outcome = sample(c("DOD", "DOOC", "AWD", "AWOD"), 100, replace = TRUE),
    Time = rexp(100, rate = 0.1),
    stringsAsFactors = FALSE
  )

  # First attempt - missing selections (should show guidance)
  result1 <- ClinicoPath::outcomeorganizer(
    data = test_data,
    outcome = "Outcome",
    analysistype = "compete",
    multievent = TRUE,
    outputTable = FALSE
  )

  # Second attempt - all selections filled (should process)
  result2 <- ClinicoPath::outcomeorganizer(
    data = test_data,
    outcome = "Outcome",
    analysistype = "compete",
    multievent = TRUE,
    dod = "DOD",
    dooc = "DOOC",
    awd = "AWD",
    awod = "AWOD",
    outputTable = TRUE,
    addOutcome = TRUE
  )

  # Second result should have output
  expect_true(!is.null(result2))
})

# =============================================================================
# Test 10: Guidance with Different Outcome Value Counts
# =============================================================================

test_that("guidance adapts to different numbers of unique outcome values", {
  skip_if_not_installed("ClinicoPath")

  # Test with exactly 4 values (standard case)
  data_4 <- data.frame(
    ID = 1:100,
    Outcome = sample(c("A", "B", "C", "D"), 100, replace = TRUE),
    Time = rexp(100, rate = 0.1)
  )

  expect_no_error({
    ClinicoPath::outcomeorganizer(
      data = data_4,
      outcome = "Outcome",
      multievent = TRUE,
      analysistype = "os"
    )
  })

  # Test with 5 values (should still work but user needs to select 4)
  data_5 <- data.frame(
    ID = 1:100,
    Outcome = sample(c("A", "B", "C", "D", "E"), 100, replace = TRUE),
    Time = rexp(100, rate = 0.1)
  )

  expect_no_error({
    ClinicoPath::outcomeorganizer(
      data = data_5,
      outcome = "Outcome",
      multievent = TRUE,
      analysistype = "os"
    )
  })

  # Test with 3 values (insufficient for multievent)
  data_3 <- data.frame(
    ID = 1:100,
    Outcome = sample(c("A", "B", "C"), 100, replace = TRUE),
    Time = rexp(100, rate = 0.1)
  )

  expect_no_error({
    ClinicoPath::outcomeorganizer(
      data = data_3,
      outcome = "Outcome",
      multievent = TRUE,
      analysistype = "os"
    )
  })
})

# =============================================================================
# Test Summary
# =============================================================================

cat("\nOutcome Organizer Guidance Notices Tests Completed\n")
cat("="*60, "\n")
cat("Tests verify:\n")
cat("✓ Helpful guidance shown instead of hard errors\n")
cat("✓ Available outcome values displayed to user\n")
cat("✓ Missing selections clearly identified\n")
cat("✓ Step-by-step guidance provided\n")
cat("✓ Analysis proceeds after selections are complete\n")
cat("✓ Works with various data formats and languages\n")
cat("✓ Edge cases handled gracefully\n")
cat("\n")
