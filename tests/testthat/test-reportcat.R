testthat::test_that("reportcat works", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  
  # Create test data with categorical variables
  test_data <- data.frame(
    treatment = factor(c("A", "B", "A", "C", "B", "A", NA)),
    grade = factor(c("Low", "High", "Medium", "Low", "High", "Medium", "Low")),
    status = factor(c("Complete", "Partial", "Complete", "None", "Partial", "Complete", "None")),
    stringsAsFactors = FALSE
  )
  
  # Test 1: Basic functionality with multiple variables
  testthat::expect_no_error({
    result <- ClinicoPath::reportcat(
      data = test_data,
      vars = c("treatment", "grade")
    )
  })
  
  # Test 2: Single variable analysis
  testthat::expect_no_error({
    result <- ClinicoPath::reportcat(
      data = test_data,
      vars = "grade"
    )
  })
  
  # Test 3: Enhanced sumvar style format
  testthat::expect_no_error({
    result <- ClinicoPath::reportcat(
      data = test_data,
      vars = c("treatment", "grade"),
      sumvar_style = TRUE
    )
  })
  
  # Test 4: Sort by frequency option
  testthat::expect_no_error({
    result <- ClinicoPath::reportcat(
      data = test_data,
      vars = "status",
      sort_by_frequency = TRUE
    )
  })
  
  # Test 5: Hide proportions option
  testthat::expect_no_error({
    result <- ClinicoPath::reportcat(
      data = test_data,
      vars = "grade",
      show_proportions = FALSE
    )
  })
  
  # Test 6: Edge case - all missing values
  test_data_missing <- data.frame(
    all_missing = factor(c(NA, NA, NA, NA))
  )
  
  testthat::expect_no_error({
    result <- ClinicoPath::reportcat(
      data = test_data_missing,
      vars = "all_missing"
    )
  })
  
  # Test 7: Edge case - single category
  test_data_single <- data.frame(
    single_cat = factor(c("A", "A", "A", "A"))
  )
  
  testthat::expect_no_error({
    result <- ClinicoPath::reportcat(
      data = test_data_single,
      vars = "single_cat"
    )
  })
  
  # Test 8: Empty data frame should produce error
  empty_data <- data.frame()
  
  testthat::expect_error({
    ClinicoPath::reportcat(
      data = empty_data,
      vars = character(0)
    )
  })
  
  # Test 9: Character variables should be converted to factors
  test_data_char <- data.frame(
    char_var = c("cat", "dog", "cat", "bird", "dog"),
    stringsAsFactors = FALSE
  )
  
  testthat::expect_no_error({
    result <- ClinicoPath::reportcat(
      data = test_data_char,
      vars = "char_var"
    )
  })
  
  # Test 10: Multiple edge cases combined
  complex_data <- data.frame(
    mixed = factor(c("A", "B", NA, "A", "C", "B", NA, "A")),
    uniform = factor(rep("Same", 8)),
    all_na = factor(rep(NA, 8))
  )
  
  testthat::expect_no_error({
    result <- ClinicoPath::reportcat(
      data = complex_data,
      vars = c("mixed", "uniform", "all_na"),
      sumvar_style = TRUE,
      sort_by_frequency = TRUE
    )
  })
})

testthat::test_that("reportcat results structure", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  
  # Create simple test data
  test_data <- data.frame(
    category = factor(c("A", "B", "A", "C"))
  )
  
  # Run analysis
  result <- ClinicoPath::reportcat(
    data = test_data,
    vars = "category"
  )
  
  # Test that result has expected structure
  testthat::expect_true("todo" %in% names(result))
  testthat::expect_true("text" %in% names(result))
  testthat::expect_true("text1" %in% names(result))
  
  # Test that results are not NULL
  testthat::expect_false(is.null(result$text))
  testthat::expect_false(is.null(result$text1))
})

testthat::test_that("reportcat handles invalid inputs", {
  
  # Skip if required packages are not available
  testthat::skip_if_not_installed("ClinicoPath")
  
  # Create test data
  test_data <- data.frame(
    valid_var = factor(c("A", "B", "C"))
  )
  
  # Test with non-existent variable
  testthat::expect_no_error({
    result <- ClinicoPath::reportcat(
      data = test_data,
      vars = "nonexistent_var"
    )
  })
  
  # Test with numeric variable (should be converted to factor)
  test_data_numeric <- data.frame(
    numeric_var = c(1, 2, 1, 3, 2)
  )
  
  testthat::expect_no_error({
    result <- ClinicoPath::reportcat(
      data = test_data_numeric,
      vars = "numeric_var"
    )
  })
})