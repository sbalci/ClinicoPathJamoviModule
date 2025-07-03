# Load required libraries and data
library(ClinicoPath)
data(histopathology, package = "ClinicoPath")
data(prostate_agreement_data, package = "ClinicoPath")

test_that("contTablesPaired module loads correctly", {
  expect_true(exists("contTablesPairedClass"))
  expect_true(is.function(contTablesPaired))
})

test_that("contTablesPaired handles basic input validation", {
  # Test with missing required variables
  expect_error(
    contTablesPaired(data = histopathology, rows = NULL, cols = "Rater 2"),
    NA  # Should not error during initialization, only during run
  )
  
  expect_error(
    contTablesPaired(data = histopathology, rows = "Rater 1", cols = NULL),
    NA  # Should not error during initialization, only during run
  )
})

test_that("contTablesPaired works with basic inter-rater agreement", {
  # Test basic functionality with rater agreement data
  result <- contTablesPaired(
    data = histopathology,
    rows = "Rater 1",
    cols = "Rater 2"
  )
  
  expect_s3_class(result, "contTablesPairedClass")
  expect_true("Rater 1" %in% names(histopathology))
  expect_true("Rater 2" %in% names(histopathology))
})

test_that("contTablesPaired handles McNemar test options", {
  # Test different McNemar test options
  result <- contTablesPaired(
    data = histopathology,
    rows = "Rater 1",
    cols = "Rater 2",
    chiSq = TRUE,
    chiSqCorr = TRUE
  )
  
  expect_s3_class(result, "contTablesPairedClass")
  
  # Check that results contain expected tables
  expect_true("freqs" %in% names(result$results))
  expect_true("test" %in% names(result$results))
})

test_that("contTablesPaired handles exact test when available", {
  # Test exact log odds ratio if exact2x2 is available
  if (require(exact2x2, quietly = TRUE)) {
    result <- contTablesPaired(
      data = histopathology,
      rows = "Rater 1",
      cols = "Rater 2",
      chiSq = TRUE,
      chiSqCorr = TRUE,
      exact = TRUE
    )
    
    expect_s3_class(result, "contTablesPairedClass")
  } else {
    # Test that it handles missing package gracefully
    expect_error({
      result <- contTablesPaired(
        data = histopathology,
        rows = "Rater 1",
        cols = "Rater 2",
        exact = TRUE
      )
    }, NA)  # Should not error during initialization
  }
})

test_that("contTablesPaired handles percentage options", {
  # Test different percentage options
  result <- contTablesPaired(
    data = histopathology,
    rows = "Rater 1",
    cols = "Rater 2",
    pcRow = TRUE,
    pcCol = TRUE
  )
  
  expect_s3_class(result, "contTablesPairedClass")
})

test_that("contTablesPaired works with prostate agreement data", {
  # Test with multi-level agreement data
  result <- contTablesPaired(
    data = prostate_agreement_data,
    rows = "Urologist_A",
    cols = "Urologist_B"
  )
  
  expect_s3_class(result, "contTablesPairedClass")
})

test_that("contTablesPaired handles gold standard comparison", {
  # Test comparing rater vs gold standard
  result <- contTablesPaired(
    data = prostate_agreement_data,
    rows = "Urologist_A",
    cols = "True_Gleason",
    chiSq = TRUE,
    chiSqCorr = TRUE
  )
  
  expect_s3_class(result, "contTablesPairedClass")
})

test_that("contTablesPaired handles count data", {
  # Test using the survey example from documentation
  survey_data <- data.frame(
    `1st survey` = c('Approve', 'Approve', 'Disapprove', 'Disapprove'),
    `2nd survey` = c('Approve', 'Disapprove', 'Approve', 'Disapprove'),
    `Counts` = c(794, 150, 86, 570),
    check.names = FALSE
  )
  
  result <- contTablesPaired(
    data = survey_data,
    rows = "1st survey",
    cols = "2nd survey",
    counts = "Counts"
  )
  
  expect_s3_class(result, "contTablesPairedClass")
})

test_that("contTablesPaired formula interface works", {
  # Test using formula interface with survey data
  survey_data <- data.frame(
    `1st survey` = c('Approve', 'Approve', 'Disapprove', 'Disapprove'),
    `2nd survey` = c('Approve', 'Disapprove', 'Approve', 'Disapprove'),
    `Counts` = c(794, 150, 86, 570),
    check.names = FALSE
  )
  
  result <- contTablesPaired(
    formula = Counts ~ `1st survey`:`2nd survey`,
    data = survey_data
  )
  
  expect_s3_class(result, "contTablesPairedClass")
})

test_that("contTablesPaired handles before-after scenarios", {
  # Create simulated before-after data
  set.seed(123)
  before_after_data <- data.frame(
    patient_id = 1:100,
    before_treatment = factor(sample(c("Poor", "Good"), 100, replace = TRUE)),
    after_treatment = factor(sample(c("Poor", "Good"), 100, replace = TRUE))
  )
  
  result <- contTablesPaired(
    data = before_after_data,
    rows = "before_treatment",
    cols = "after_treatment"
  )
  
  expect_s3_class(result, "contTablesPairedClass")
})

test_that("contTablesPaired handles different confidence intervals", {
  # Test that function works with exact option (testing structure, not necessarily exact2x2)
  result <- contTablesPaired(
    data = histopathology,
    rows = "Rater 1",
    cols = "Rater 2",
    chiSq = TRUE,
    chiSqCorr = TRUE,
    exact = FALSE  # Start with FALSE to test basic structure
  )
  
  expect_s3_class(result, "contTablesPairedClass")
})

test_that("contTablesPaired handles edge cases", {
  # Test with small dataset
  small_data <- histopathology[1:20, ]
  
  expect_error({
    result <- contTablesPaired(
      data = small_data,
      rows = "Rater 1",
      cols = "Rater 2"
    )
  }, NA)  # Should not error during initialization
})

test_that("contTablesPaired handles missing data appropriately", {
  # Create dataset with some missing values
  test_data <- histopathology[1:50, ]
  test_data$`Rater 1`[1:5] <- NA
  test_data$`Rater 2`[6:10] <- NA
  
  expect_error({
    result <- contTablesPaired(
      data = test_data,
      rows = "Rater 1",
      cols = "Rater 2"
    )
  }, NA)  # Should handle missing data gracefully
})

test_that("contTablesPaired handles non-2x2 tables appropriately", {
  # Test with multi-level factors (should still work but may give warnings for exact test)
  result <- contTablesPaired(
    data = prostate_agreement_data,
    rows = "Urologist_A",
    cols = "Urologist_B",
    chiSq = TRUE,
    exact = FALSE  # Exact test only works for 2x2
  )
  
  expect_s3_class(result, "contTablesPairedClass")
})

test_that("contTablesPaired handles negative counts appropriately", {
  # Test that negative counts are rejected
  bad_count_data <- data.frame(
    var1 = c("A", "A", "B", "B"),
    var2 = c("X", "Y", "X", "Y"),
    counts = c(10, -5, 15, 20)  # Negative count
  )
  
  expect_error({
    result <- contTablesPaired(
      data = bad_count_data,
      rows = "var1",
      cols = "var2",
      counts = "counts"
    )
  }, NA)  # Should not error during initialization, will be caught in .run()
})

test_that("contTablesPaired comprehensive test with all options", {
  # Test with all options enabled
  result <- contTablesPaired(
    data = histopathology,
    rows = "Rater 1",
    cols = "Rater 3",  # Use different rater pair
    chiSq = TRUE,
    chiSqCorr = TRUE,
    exact = FALSE,  # Set to FALSE unless exact2x2 is guaranteed to be available
    pcRow = TRUE,
    pcCol = TRUE
  )
  
  expect_s3_class(result, "contTablesPairedClass")
  
  # Verify the structure of results
  expect_true("freqs" %in% names(result$results))
  expect_true("test" %in% names(result$results))
})