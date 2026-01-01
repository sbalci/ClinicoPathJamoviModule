testthat::test_that("retracted works", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  
  # Create test data with DOI strings
  test_data <- data.frame(
    doi = c(
      "10.1126/science.aac4716",  # Example DOI (format: bare DOI)
      "doi:10.1038/nature12373",  # Example DOI (format: with prefix)
      "https://doi.org/10.1016/j.cell.2014.09.045",  # Example DOI (format: full URL)
      "10.invalid/format",        # Invalid DOI format
      "",                         # Empty string
      NA                          # Missing value
    ),
    title = c("Paper 1", "Paper 2", "Paper 3", "Paper 4", "Paper 5", "Paper 6"),
    stringsAsFactors = FALSE
  )
  
  # Test 1: Basic functionality
  testthat::expect_no_error({
    result <- ClinicoPath::retracted(
      data = test_data,
      doi = "doi"
    )
  })
  
  # Test 2: With PMID lookup (if rcrossref available)
  testthat::expect_no_error({
    result <- ClinicoPath::retracted(
      data = test_data,
      doi = "doi",
      pmid = TRUE
    )
  })
  
  # Test 3: Different database option
  testthat::expect_no_error({
    result <- ClinicoPath::retracted(
      data = test_data,
      doi = "doi",
      database = "rw"
    )
  })
  
  # Test 4: Minimal valid data
  minimal_data <- data.frame(
    doi = "10.1126/science.aac4716"
  )
  
  testthat::expect_no_error({
    result <- ClinicoPath::retracted(
      data = minimal_data,
      doi = "doi"
    )
  })
  
  # Test 5: Edge case - all invalid DOIs
  invalid_data <- data.frame(
    doi = c("not-a-doi", "also-invalid", "")
  )
  
  testthat::expect_no_error({
    result <- ClinicoPath::retracted(
      data = invalid_data,
      doi = "doi"
    )
  })
  
  # Test 6: Edge case - all missing values
  missing_data <- data.frame(
    doi = c(NA, NA, NA)
  )
  
  testthat::expect_no_error({
    result <- ClinicoPath::retracted(
      data = missing_data,
      doi = "doi"
    )
  })
  
  # Test 7: Empty data frame should produce error
  empty_data <- data.frame()
  
  testthat::expect_error({
    ClinicoPath::retracted(
      data = empty_data,
      doi = "nonexistent"
    )
  })
  
  # Test 8: Non-existent column should produce error
  testthat::expect_error({
    ClinicoPath::retracted(
      data = test_data,
      doi = "nonexistent_column"
    )
  })
})

testthat::test_that("retracted results structure", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  
  # Create simple test data
  test_data <- data.frame(
    doi = "10.1126/science.aac4716"
  )
  
  # Run analysis
  result <- ClinicoPath::retracted(
    data = test_data,
    doi = "doi"
  )
  
  # Test that result has expected structure
  testthat::expect_true("todo" %in% names(result))
  testthat::expect_true("summary" %in% names(result))
  testthat::expect_true("details" %in% names(result))
  testthat::expect_true("pmids" %in% names(result))
  
  # Test that summary table has expected columns
  testthat::expect_true(length(result$summary$columns) >= 2)  # At least DOI and Status columns
})

testthat::test_that("DOI validation function works", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  
  # Test the internal validation function through the main function
  # Valid DOI formats
  valid_data <- data.frame(
    doi = c(
      "10.1126/science.aac4716",
      "10.1038/nature12373", 
      "10.1016/j.cell.2014.09.045"
    )
  )
  
  testthat::expect_no_error({
    result <- ClinicoPath::retracted(
      data = valid_data,
      doi = "doi"
    )
  })
  
  # Invalid DOI formats should not cause errors (handled gracefully)
  invalid_data <- data.frame(
    doi = c(
      "not-a-doi",
      "10.invalid",
      "random-string"
    )
  )
  
  testthat::expect_no_error({
    result <- ClinicoPath::retracted(
      data = invalid_data,
      doi = "doi"
    )
  })
})

testthat::test_that("retracted handles different DOI formats", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  
  # Test different DOI formats
  format_data <- data.frame(
    doi = c(
      "10.1126/science.aac4716",                    # Bare DOI
      "doi:10.1126/science.aac4716",               # DOI with prefix
      "https://doi.org/10.1126/science.aac4716",   # Full DOI URL
      "http://doi.org/10.1126/science.aac4716"     # HTTP DOI URL
    )
  )
  
  testthat::expect_no_error({
    result <- ClinicoPath::retracted(
      data = format_data,
      doi = "doi"
    )
  })
})

testthat::test_that("retracted with pmid option", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  
  # Test data with known DOIs
  pmid_data <- data.frame(
    doi = c("10.1126/science.aac4716", "10.1038/nature12373")
  )
  
  # Test with PMID lookup enabled
  testthat::expect_no_error({
    result <- ClinicoPath::retracted(
      data = pmid_data,
      doi = "doi",
      pmid = TRUE
    )
  })
  
  # Test with PMID lookup disabled
  testthat::expect_no_error({
    result <- ClinicoPath::retracted(
      data = pmid_data,
      doi = "doi",
      pmid = FALSE
    )
  })
})

testthat::test_that("retracted handles network issues gracefully", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  
  # This test uses real DOIs but should handle any network issues gracefully
  network_test_data <- data.frame(
    doi = "10.1126/science.aac4716"
  )
  
  # Should not error even if network is unavailable
  testthat::expect_no_error({
    result <- ClinicoPath::retracted(
      data = network_test_data,
      doi = "doi"
    )
  })
})

testthat::test_that("retracted performance with multiple DOIs", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  
  # Create data with multiple DOIs to test rate limiting
  multi_data <- data.frame(
    doi = c(
      "10.1126/science.aac4716",
      "10.1038/nature12373",
      "10.1016/j.cell.2014.09.045",
      "10.1126/science.abc1234",  # Likely non-existent
      "10.1038/nature56789"      # Likely non-existent
    )
  )
  
  # Test should complete without errors
  testthat::expect_no_error({
    result <- ClinicoPath::retracted(
      data = multi_data,
      doi = "doi"
    )
  })
})
