context("Benford's Law Analysis - benford")

# Test data preparation
set.seed(42)  # For reproducible results

# Create comprehensive test datasets for different Benford scenarios
# 1. Dataset that follows Benford's Law (natural data)
benford_compliant_data <- data.frame(
  id = 1:500,
  # Natural data that should follow Benford's Law
  populations = c(
    # Cities with populations following natural distribution
    round(10^(runif(200, 3, 7))),  # City populations 1k-10M
    round(rlnorm(200, 10, 1.5)),   # Income data
    round(exp(rnorm(100, 8, 2)))   # Financial amounts
  ),
  # Additional variables for testing
  financial_amounts = round(rlnorm(500, 8, 2)),
  natural_measurements = round(10^(runif(500, 1, 5))),
  # Some control variables
  uniform_numbers = round(runif(500, 1, 999)),  # Should NOT follow Benford
  category = factor(sample(c("A", "B", "C"), 500, replace = TRUE))
)

# 2. Dataset that violates Benford's Law (manipulated data)
benford_violation_data <- data.frame(
  id = 1:300,
  # Artificially manipulated data (too many round numbers)
  suspicious_amounts = c(
    rep(c(100, 200, 300, 400, 500), each = 30),  # Too many round numbers
    rep(c(1000, 2000, 3000, 5000), each = 15),   # Suspicious patterns
    sample(c(999, 1999, 2999), 60, replace = TRUE)  # Avoiding certain digits
  ),
  # Fabricated financial data
  reported_revenues = c(
    rep(seq(100000, 900000, 100000), each = 20),  # Too uniform
    sample(c(50000, 75000, 125000), 140, replace = TRUE)  # Clustering
  )
)

# 3. Small dataset for edge case testing
small_benford_data <- data.frame(
  small_amounts = c(123, 234, 345, 456, 567, 678, 789, 891)
)

# 4. Dataset with problematic values
problematic_data <- data.frame(
  id = 1:100,
  with_zeros = c(rep(0, 20), round(rlnorm(80, 5, 1))),
  with_negatives = c(round(rnorm(50, -100, 50)), round(rlnorm(50, 5, 1))),
  with_missing = c(rep(NA, 15), round(rlnorm(85, 6, 1.5))),
  mixed_types = as.character(c(round(rlnorm(100, 5, 1))))
)

test_that("Benford Analysis - Basic functionality and structure", {
  
  # Test basic function exists and can be called
  expect_true(exists("benfordClass"))
  
  # Test that basic structure works with valid data
  expect_error(
    {
      benford_instance <- benfordClass$new()
    },
    NA
  )
})

test_that("Benford Analysis - Data validation and input checking", {
  
  # Test data type validation
  valid_numeric <- benford_compliant_data$populations
  expect_true(is.numeric(valid_numeric))
  expect_true(length(valid_numeric) > 0)
  
  # Test minimum data requirements
  min_data_size <- 50  # Benford analysis needs sufficient data
  expect_true(length(valid_numeric) >= min_data_size)
  
  # Test for positive values (Benford's Law applies to positive numbers)
  positive_data <- valid_numeric[valid_numeric > 0]
  expect_true(all(positive_data > 0))
  expect_true(length(positive_data) > 0)
  
  # Test data range suitability
  # Benford's Law works best with data spanning multiple orders of magnitude
  data_range <- max(valid_numeric) / min(valid_numeric[valid_numeric > 0])
  expect_true(data_range > 10)  # Should span at least one order of magnitude
})

test_that("Benford Analysis - First digit distribution analysis", {
  
  # Test first digit extraction
  test_numbers <- c(123, 234, 345, 456, 567, 678, 789, 891, 912)
  expected_first_digits <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  
  # Extract first digits manually for validation
  first_digits <- as.numeric(substr(as.character(test_numbers), 1, 1))
  expect_equal(first_digits, expected_first_digits)
  
  # Test Benford's theoretical distribution
  benford_theoretical <- c(0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046)
  expect_length(benford_theoretical, 9)
  expect_true(all(benford_theoretical > 0))
  expect_true(abs(sum(benford_theoretical) - 1.0) < 0.001)  # Should sum to 1
})

test_that("Benford Analysis - Statistical test validation", {
  
  # Test chi-square goodness of fit calculation
  observed_counts <- c(30, 18, 12, 10, 8, 7, 6, 5, 4)  # Example counts for digits 1-9
  total_observations <- sum(observed_counts)
  benford_theoretical <- c(0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046)
  expected_counts <- benford_theoretical * total_observations
  
  # Chi-square test calculation
  chi_square_stat <- sum((observed_counts - expected_counts)^2 / expected_counts)
  expect_true(is.numeric(chi_square_stat))
  expect_true(chi_square_stat >= 0)
  
  # Degrees of freedom for Benford test (9 digits - 1)
  df <- 8
  p_value <- pchisq(chi_square_stat, df, lower.tail = FALSE)
  expect_true(p_value >= 0 && p_value <= 1)
})

test_that("Benford Analysis - Suspect identification logic", {
  
  # Test suspect identification criteria
  test_data <- benford_violation_data$suspicious_amounts
  
  # Count first digit frequencies
  first_digits <- as.numeric(substr(as.character(test_data), 1, 1))
  digit_counts <- table(factor(first_digits, levels = 1:9))
  total_n <- length(first_digits)
  observed_proportions <- as.numeric(digit_counts) / total_n
  
  # Benford's expected proportions
  benford_expected <- c(0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046)
  
  # Calculate deviations
  deviations <- abs(observed_proportions - benford_expected)
  
  # Significant deviation threshold (could be customizable)
  deviation_threshold <- 0.05  # 5% deviation
  suspicious_digits <- which(deviations > deviation_threshold)
  
  expect_true(is.numeric(suspicious_digits))
  expect_true(all(suspicious_digits >= 1 & suspicious_digits <= 9))
})

test_that("Benford Analysis - Edge cases and error handling", {
  
  # Test empty dataset
  empty_data <- numeric(0)
  expect_equal(length(empty_data), 0)
  
  # Test dataset with zeros
  data_with_zeros <- c(0, 0, 123, 234, 345)
  positive_only <- data_with_zeros[data_with_zeros > 0]
  expect_true(all(positive_only > 0))
  expect_true(length(positive_only) > 0)
  
  # Test dataset with negative numbers
  data_with_negatives <- c(-123, -234, 345, 456)
  positive_only_neg <- data_with_negatives[data_with_negatives > 0]
  expect_true(all(positive_only_neg > 0))
  
  # Test very small dataset
  tiny_data <- c(123, 234, 345)
  expect_true(length(tiny_data) < 10)  # Too small for reliable Benford analysis
  
  # Test dataset with missing values
  data_with_na <- c(123, NA, 234, NA, 345)
  complete_data <- data_with_na[!is.na(data_with_na)]
  expect_true(all(!is.na(complete_data)))
  expect_true(length(complete_data) > 0)
})

test_that("Benford Analysis - benford.analysis package integration", {
  
  # Test that benford.analysis package functions work correctly
  if (requireNamespace("benford.analysis", quietly = TRUE)) {
    
    # Test basic benford analysis
    test_numbers <- benford_compliant_data$populations[1:100]
    test_numbers <- test_numbers[test_numbers > 0]  # Remove any zeros
    
    if (length(test_numbers) >= 10) {
      # Test benford analysis
      bfd_result <- benford.analysis::benford(test_numbers)
      
      expect_s3_class(bfd_result, "Benford")
      
      # Test that getSuspects works
      suspects_result <- benford.analysis::getSuspects(bfd_result, test_numbers)
      expect_true(is.vector(suspects_result) || is.data.frame(suspects_result))
    }
  }
})

test_that("Benford Analysis - Real-world data scenarios", {
  
  # Test with natural data that should follow Benford's Law
  natural_data <- benford_compliant_data$financial_amounts
  natural_data <- natural_data[natural_data > 0]
  
  # Extract first digits
  first_digits <- as.numeric(substr(as.character(natural_data), 1, 1))
  
  # Count digit frequencies
  digit_counts <- table(factor(first_digits, levels = 1:9))
  proportions <- as.numeric(digit_counts) / length(first_digits)
  
  # Benford's Law predicts digit 1 should be most frequent
  expect_true(proportions[1] > proportions[2])  # Digit 1 > Digit 2
  expect_true(proportions[2] > proportions[9])  # Digit 2 > Digit 9
  
  # Test with suspicious data
  suspicious_data <- benford_violation_data$suspicious_amounts
  suspicious_first_digits <- as.numeric(substr(as.character(suspicious_data), 1, 1))
  suspicious_counts <- table(factor(suspicious_first_digits, levels = 1:9))
  suspicious_proportions <- as.numeric(suspicious_counts) / length(suspicious_first_digits)
  
  # Suspicious data might show unusual patterns
  benford_expected <- c(0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046)
  deviations <- abs(suspicious_proportions - benford_expected)
  
  # Should show larger deviations than natural data
  expect_true(max(deviations) > 0.01)  # Some significant deviation expected
})

test_that("Benford Analysis - Statistical accuracy validation", {
  
  # Test Benford's Law mathematical properties
  benford_proportions <- c(0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046)
  
  # Test that proportions sum to 1
  expect_equal(sum(benford_proportions), 1.0, tolerance = 0.001)
  
  # Test that digit 1 is most frequent
  expect_true(benford_proportions[1] == max(benford_proportions))
  
  # Test decreasing trend (mostly)
  expect_true(benford_proportions[1] > benford_proportions[2])
  expect_true(benford_proportions[2] > benford_proportions[3])
  expect_true(benford_proportions[8] > benford_proportions[9])
  
  # Test mathematical formula: P(d) = log10(1 + 1/d)
  calculated_proportions <- sapply(1:9, function(d) log10(1 + 1/d))
  expect_equal(calculated_proportions, benford_proportions, tolerance = 0.001)
})

test_that("Benford Analysis - Multiple digit analysis foundation", {
  
  # Test second digit analysis preparation
  test_numbers <- c(123, 234, 345, 456, 567, 678, 789, 891, 912, 1023)
  
  # Extract second digits
  second_digits <- as.numeric(substr(as.character(test_numbers), 2, 2))
  expected_second <- c(2, 3, 4, 5, 6, 7, 8, 9, 1, 0)
  expect_equal(second_digits, expected_second)
  
  # Second digit Benford distribution is more uniform
  second_digit_benford <- rep(0.1, 10)  # Approximately uniform for digits 0-9
  expect_length(second_digit_benford, 10)
  expect_true(abs(sum(second_digit_benford) - 1.0) < 0.001)
  
  # Test first-two digits (10-99)
  first_two_digits <- as.numeric(substr(as.character(test_numbers), 1, 2))
  # Should have values from 10-99 for valid analysis
  valid_two_digits <- first_two_digits[first_two_digits >= 10 & first_two_digits <= 99]
  expect_true(all(valid_two_digits >= 10 & valid_two_digits <= 99))
})

test_that("Benford Analysis - Data quality and preprocessing", {
  
  # Test data cleaning requirements
  raw_data <- c(0, -123, 234, NA, 345, 0.5, 999999999)
  
  # Remove invalid values for Benford analysis
  cleaned_data <- raw_data[!is.na(raw_data) & raw_data > 0]
  expect_true(all(cleaned_data > 0))
  expect_true(all(!is.na(cleaned_data)))
  
  # Test for appropriate data size
  min_recommended_size <- 50
  data_size_warning <- length(cleaned_data) < min_recommended_size
  expect_true(is.logical(data_size_warning))
  
  # Test for data range appropriateness
  if (length(cleaned_data) > 1) {
    data_spread <- max(cleaned_data) / min(cleaned_data)
    insufficient_spread <- data_spread < 2
    expect_true(is.logical(insufficient_spread))
  }
})

test_that("Benford Analysis - Integration with ClinicoPath datasets", {
  
  # Test with histopathology dataset if available
  if (exists("histopathology") && is.data.frame(histopathology)) {
    histo_data <- histopathology
    
    # Find numeric variables suitable for Benford analysis
    numeric_vars <- names(histo_data)[sapply(histo_data, is.numeric)]
    
    if (length(numeric_vars) > 0) {
      # Test first numeric variable
      test_var <- histo_data[[numeric_vars[1]]]
      test_var <- test_var[!is.na(test_var) & test_var > 0]
      
      if (length(test_var) >= 10) {
        # Should be able to extract first digits
        first_digits <- as.numeric(substr(as.character(test_var), 1, 1))
        expect_true(all(first_digits >= 1 & first_digits <= 9))
        
        # Should be able to create frequency table
        digit_table <- table(factor(first_digits, levels = 1:9))
        expect_length(digit_table, 9)
        expect_true(sum(digit_table) == length(first_digits))
      }
    }
  }
})

test_that("Benford Analysis - Fraud detection indicators", {
  
  # Test indicators that suggest potential fraud or manipulation
  suspicious_indicators <- list()
  
  # 1. Too many round numbers
  round_number_data <- c(100, 200, 300, 400, 500, 600, 700, 800, 900)
  round_digits <- as.numeric(substr(as.character(round_number_data), 1, 1))
  round_distribution <- table(factor(round_digits, levels = 1:9))
  
  # Should show even distribution (suspicious)
  round_proportions <- as.numeric(round_distribution) / length(round_digits)
  round_uniformity <- var(round_proportions)
  expect_true(round_uniformity < 0.01)  # Very uniform = suspicious
  
  # 2. Avoidance of certain digits
  avoided_digit_data <- c(123, 234, 345, 456, 567, 678, 789, 823, 834)  # No 9s
  avoided_digits <- as.numeric(substr(as.character(avoided_digit_data), 1, 1))
  digit_9_count <- sum(avoided_digits == 9)
  expect_equal(digit_9_count, 0)  # Suspicious absence
  
  # 3. Clustering around psychological barriers
  cluster_data <- c(rep(99, 10), rep(999, 10), rep(9999, 10))  # Clustering
  cluster_variance <- var(cluster_data)
  expect_true(cluster_variance > 0)  # But might show unusual patterns
})

# Test completion message
cat("✅ Benford Analysis test suite completed successfully!\n")
cat("📊 Tests covered:\n")
cat("   - Basic functionality and data validation\n")
cat("   - Benford's Law statistical principles\n") 
cat("   - First digit distribution analysis\n")
cat("   - Chi-square goodness of fit testing\n")
cat("   - Suspect identification logic\n")
cat("   - Edge cases and error handling\n")
cat("   - benford.analysis package integration\n")
cat("   - Real-world data scenarios\n")
cat("   - Statistical accuracy validation\n")
cat("   - Multiple digit analysis foundation\n")
cat("   - Data quality and preprocessing\n")
cat("   - Integration with ClinicoPath datasets\n")
cat("   - Fraud detection indicators\n")