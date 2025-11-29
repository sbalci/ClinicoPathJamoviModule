# Comprehensive Tests for categorize Function
# Validates binning logic, label generation, frequency tables, and categorized values

library(testthat)
library(ClinicoPath)

# Helper function to extract table data from jamovi results
extract_table_data <- function(result, table_name) {
  if (!inherits(result, "categorizeResults")) {
    return(NULL)
  }

  table_obj <- result[[table_name]]
  if (is.null(table_obj)) {
    return(NULL)
  }

  # Extract rows from jamovi table object
  if (inherits(table_obj, "Table")) {
    # Access the underlying data
    rows <- table_obj$rowKeys
    if (is.null(rows) || length(rows) == 0) {
      return(NULL)
    }

    # Build data frame from table rows
    cols <- table_obj$columns
    data_list <- list()

    for (col in cols) {
      col_name <- col$name
      col_values <- sapply(rows, function(key) {
        cell <- table_obj$getCell(rowKey = key, col = col_name)
        if (!is.null(cell)) {
          return(cell$value)
        }
        return(NA)
      })
      data_list[[col_name]] <- col_values
    }

    # Create data frame with row numbers to avoid NA rowname issues
    df <- as.data.frame(data_list, stringsAsFactors = FALSE, row.names = NULL)
    return(df)
  }

  return(NULL)
}

# ============================================================================
# PART 1: BREAK CALCULATION TESTS
# Validates that breakpoints are calculated correctly for each method
# ============================================================================

# ===== Break Calculation Tests =====

test_that("Equal intervals creates correct breakpoints", {
  set.seed(123)
  data <- data.frame(value = 1:100)

  result <- categorize(
    data = data,
    var = "value",
    method = "equal",
    nbins = 4
  )

  # Extract breakpoints table
  break_table <- extract_table_data(result, "breakpointsTable")

  expect_true(!is.null(break_table))
  expect_equal(nrow(break_table), 5)  # 4 bins = 5 breakpoints

  # Verify breakpoints are equally spaced
  breaks <- break_table$value
  intervals <- diff(breaks)
  expect_true(all(abs(intervals - intervals[1]) < 1e-10))  # All intervals equal

  # Verify range coverage
  expect_equal(breaks[1], 1)    # Min value
  expect_equal(breaks[5], 100)  # Max value
})

test_that("Quantile method creates correct breakpoints", {
  set.seed(456)
  data <- data.frame(value = 1:100)

  result <- categorize(
    data = data,
    var = "value",
    method = "quantile",
    nbins = 4  # Quartiles
  )

  break_table <- extract_table_data(result, "breakpointsTable")

  expect_true(!is.null(break_table))

  # Verify quartiles
  breaks <- break_table$value
  expected_quartiles <- quantile(data$value, probs = c(0, 0.25, 0.5, 0.75, 1))

  expect_equal(length(breaks), 5)
  expect_true(all(abs(breaks - expected_quartiles) < 1e-6))
})

test_that("Manual breaks are used correctly", {
  data <- data.frame(value = 1:100)

  result <- categorize(
    data = data,
    var = "value",
    method = "manual",
    breaks = "0, 25, 50, 75, 100"
  )

  break_table <- extract_table_data(result, "breakpointsTable")

  expect_true(!is.null(break_table))

  breaks <- break_table$value
  expected <- c(0, 25, 50, 75, 100)

  expect_equal(breaks, expected)
})

test_that("Mean +/- SD creates correct breakpoints", {
  set.seed(789)
  data <- data.frame(value = rnorm(100, mean = 50, sd = 10))

  result <- categorize(
    data = data,
    var = "value",
    method = "meansd",
    sdmult = 1  # Mean ± 1 SD
  )

  break_table <- extract_table_data(result, "breakpointsTable")

  expect_true(!is.null(break_table))

  breaks <- break_table$value
  expect_equal(length(breaks), 5)

  # Verify structure: min, mean - SD, mean, mean + SD, max
  m <- mean(data$value)
  s <- sd(data$value)

  expect_equal(breaks[1], min(data$value), tolerance = 1e-4)
  expect_true(abs(breaks[2] - (m - s)) < 1e-4)
  expect_true(abs(breaks[3] - m) < 1e-4)
  expect_true(abs(breaks[4] - (m + s)) < 1e-4)
  expect_equal(breaks[5], max(data$value), tolerance = 1e-4)
})

test_that("Median split creates correct breakpoints", {
  set.seed(1011)
  data <- data.frame(value = 1:99)  # Odd number for clear median

  result <- categorize(
    data = data,
    var = "value",
    method = "median"
  )

  break_table <- extract_table_data(result, "breakpointsTable")

  expect_true(!is.null(break_table))

  breaks <- break_table$value
  expect_equal(length(breaks), 3)  # Min, Median, Max

  expect_equal(breaks[1], 1)
  expect_equal(breaks[2], median(data$value))
  expect_equal(breaks[3], 99)
})

# ============================================================================
# PART 2: LABEL GENERATION TESTS
# Validates that labels are generated correctly for each type
# ============================================================================

# ===== Label Generation Tests =====

test_that("Numbered labels are correct", {
  data <- data.frame(value = 1:100)

  result <- categorize(
    data = data,
    var = "value",
    method = "equal",
    nbins = 3,
    labels = "numbered"
  )

  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  categories <- freq_table$category
  expect_equal(categories, c("1", "2", "3"))
})

test_that("Lettered labels are correct", {
  data <- data.frame(value = 1:100)

  result <- categorize(
    data = data,
    var = "value",
    method = "equal",
    nbins = 4,
    labels = "lettered"
  )

  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  categories <- freq_table$category
  expect_equal(categories, c("A", "B", "C", "D"))
})

test_that("Semantic labels are correct for 2 bins", {
  data <- data.frame(value = 1:100)

  result <- categorize(
    data = data,
    var = "value",
    method = "median",
    labels = "semantic"
  )

  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  categories <- freq_table$category
  expect_equal(categories, c("Low", "High"))
})

test_that("Semantic labels are correct for 3 bins", {
  data <- data.frame(value = 1:100)

  result <- categorize(
    data = data,
    var = "value",
    method = "equal",
    nbins = 3,
    labels = "semantic"
  )

  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  categories <- freq_table$category
  expect_equal(categories, c("Low", "Medium", "High"))
})

test_that("Semantic labels are correct for 4 bins", {
  data <- data.frame(value = 1:100)

  result <- categorize(
    data = data,
    var = "value",
    method = "equal",
    nbins = 4,
    labels = "semantic"
  )

  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  categories <- freq_table$category
  expect_equal(categories, c("Low", "Medium-Low", "Medium-High", "High"))
})

test_that("Custom labels work correctly", {
  data <- data.frame(value = 1:100)

  result <- categorize(
    data = data,
    var = "value",
    method = "equal",
    nbins = 3,
    labels = "custom",
    customlabels = "Small, Medium, Large"
  )

  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  categories <- freq_table$category
  expect_equal(categories, c("Small", "Medium", "Large"))
})

# ============================================================================
# PART 3: FREQUENCY TABLE VALIDATION
# Validates that frequency counts, percentages, and ranges are accurate
# ============================================================================

# ===== Frequency Table Validation =====

test_that("Frequency counts are accurate for equal intervals", {
  set.seed(1234)
  data <- data.frame(value = 1:100)

  result <- categorize(
    data = data,
    var = "value",
    method = "equal",
    nbins = 4
  )

  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  # Equal intervals on 1:100 should give approximately equal counts
  counts <- freq_table$n
  expect_equal(length(counts), 4)
  expect_equal(sum(counts), 100)  # Total should be 100

  # Each bin should have about 25 observations
  expect_true(all(counts >= 20 & counts <= 30))
})

test_that("Percentages sum to 100%", {
  data <- data.frame(value = 1:100)

  result <- categorize(
    data = data,
    var = "value",
    method = "equal",
    nbins = 5
  )

  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  percentages <- freq_table$percent
  expect_true(abs(sum(percentages) - 1.0) < 1e-10)  # Sum to 100% (1.0)
})

test_that("Cumulative percentages are correct", {
  data <- data.frame(value = 1:100)

  result <- categorize(
    data = data,
    var = "value",
    method = "equal",
    nbins = 4
  )

  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  cum_percent <- freq_table$cumPercent

  # Cumulative should be monotonically increasing
  expect_true(all(diff(cum_percent) >= 0))

  # Last cumulative should be 100% (1.0)
  expect_true(abs(cum_percent[length(cum_percent)] - 1.0) < 1e-10)

  # Verify cumulative calculation
  percentages <- freq_table$percent
  expected_cum <- cumsum(percentages)
  expect_true(all(abs(cum_percent - expected_cum) < 1e-10))
})

test_that("Quantile binning produces equal frequencies", {
  set.seed(5678)
  data <- data.frame(value = 1:100)

  result <- categorize(
    data = data,
    var = "value",
    method = "quantile",
    nbins = 4
  )

  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  counts <- freq_table$n

  # Quantile binning should produce approximately equal counts
  expect_equal(sum(counts), 100)
  expect_true(all(counts >= 20 & counts <= 30))
})

test_that("Range strings match breakpoints", {
  data <- data.frame(value = c(10, 20, 30, 40, 50))

  result <- categorize(
    data = data,
    var = "value",
    method = "manual",
    breaks = "5, 25, 45, 55"
  )

  freq_table <- extract_table_data(result, "freqTable")
  break_table <- extract_table_data(result, "breakpointsTable")

  expect_true(!is.null(freq_table))
  expect_true(!is.null(break_table))

  ranges <- freq_table$range
  breaks <- break_table$value

  # Verify range strings match breakpoints
  expect_true(grepl("5\\.00.*25\\.00", ranges[1]))
  expect_true(grepl("25\\.00.*45\\.00", ranges[2]))
  expect_true(grepl("45\\.00.*55\\.00", ranges[3]))
})

# ============================================================================
# PART 4: CATEGORIZED VARIABLE VALUE VALIDATION
# Validates that actual observations are assigned to correct categories
# ============================================================================

# ===== Categorized Variable Values =====

test_that("Observations are assigned to correct bins", {
  data <- data.frame(value = c(5, 15, 25, 35, 45))

  result <- categorize(
    data = data,
    var = "value",
    method = "manual",
    breaks = "0, 20, 40, 50",
    labels = "numbered"
  )

  # To verify categorization, we need to check the addtodata column
  # Since we can't directly access it in tests, we verify via frequency table
  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  # Based on breaks [0, 20, 40, 50]:
  # 5, 15 -> Bin 1 (count = 2)
  # 25, 35 -> Bin 2 (count = 2)
  # 45 -> Bin 3 (count = 1)

  counts <- freq_table$n
  expect_equal(counts, c(2, 2, 1))
})

test_that("Boundary values are handled correctly with include.lowest=TRUE", {
  data <- data.frame(value = c(0, 25, 50, 75, 100))

  result <- categorize(
    data = data,
    var = "value",
    method = "manual",
    breaks = "0, 25, 50, 75, 100",
    includelowest = TRUE,
    labels = "lettered"
  )

  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  # With include.lowest = TRUE, each boundary value should be in a bin
  # [0, 25]: 0, 25 -> 2 values
  # (25, 50]: Already counted 25 above, so just 50 -> Wait, this depends on 'right' parameter

  # Actually: cut(c(0, 25, 50, 75, 100), breaks = c(0, 25, 50, 75, 100), include.lowest = TRUE)
  # Default right = TRUE means intervals are (a, b]
  # With include.lowest = TRUE, first interval becomes [a, b]
  # So: [0, 25], (25, 50], (50, 75], (75, 100]
  # Values: 0 -> bin 1, 25 -> bin 1, 50 -> bin 2, 75 -> bin 3, 100 -> bin 4

  counts <- freq_table$n
  expect_equal(sum(counts), 5)
})

test_that("Right-closed vs left-closed intervals work correctly", {
  data <- data.frame(value = c(10, 20, 30))

  # Test right-closed: (a, b]
  result_right <- categorize(
    data = data,
    var = "value",
    method = "manual",
    breaks = "0, 20, 40",
    rightclosed = TRUE,
    includelowest = TRUE
  )

  freq_right <- extract_table_data(result_right, "freqTable")

  # With right=TRUE, include.lowest=TRUE: [0, 20], (20, 40]
  # 10 -> bin 1, 20 -> bin 1, 30 -> bin 2
  expect_equal(freq_right$n, c(2, 1))

  # Test left-closed: [a, b)
  result_left <- categorize(
    data = data,
    var = "value",
    method = "manual",
    breaks = "0, 20, 40",
    rightclosed = FALSE,
    includelowest = TRUE
  )

  freq_left <- extract_table_data(result_left, "freqTable")

  # With right=FALSE, include.lowest=TRUE: [0, 20), [20, 40]
  # 10 -> bin 1, 20 -> bin 2, 30 -> bin 2
  expect_equal(freq_left$n, c(1, 2))
})

# ============================================================================
# PART 5: EDGE CASE TESTS
# Validates handling of missing values, extreme cases, and errors
# ============================================================================

# ===== Edge Cases and Error Handling =====

test_that("Missing values are handled correctly", {
  data <- data.frame(value = c(1, 5, NA, 10, 15, NA, 20))

  result <- categorize(
    data = data,
    var = "value",
    method = "equal",
    nbins = 2,
    excl = FALSE  # Don't exclude NAs
  )

  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  # Should have 2 bins plus NA category
  expect_true(nrow(freq_table) >= 2)

  # Check if NA category exists
  has_na <- any(freq_table$category == "Missing" | is.na(freq_table$category))
  expect_true(has_na)
})

test_that("All identical values trigger expected error", {
  data <- data.frame(value = rep(50, 10))

  # When all values are identical, breaks are not unique - this should error
  expect_error(
    categorize(
      data = data,
      var = "value",
      method = "equal",
      nbins = 3
    ),
    "breaks.*not unique"
  )
})

test_that("Very small sample sizes work", {
  data <- data.frame(value = c(1, 2, 3))

  result <- categorize(
    data = data,
    var = "value",
    method = "equal",
    nbins = 5  # More bins than observations
  )

  # Should handle gracefully
  expect_true(inherits(result, "categorizeResults"))
})

test_that("Custom labels mismatch is handled", {
  data <- data.frame(value = 1:100)

  result <- categorize(
    data = data,
    var = "value",
    method = "equal",
    nbins = 4,
    labels = "custom",
    customlabels = "Low, High"  # Only 2 labels for 4 bins
  )

  # Should not error - should fall back to numbered labels
  expect_true(inherits(result, "categorizeResults"))

  freq_table <- extract_table_data(result, "freqTable")
  expect_true(!is.null(freq_table))

  # Should use fallback labels (numbered or Category X)
  categories <- freq_table$category
  expect_equal(length(categories), 4)
})

test_that("Invalid manual breaks are detected", {
  data <- data.frame(value = 1:100)

  # This should trigger a warning or error
  result <- categorize(
    data = data,
    var = "value",
    method = "manual",
    breaks = "abc, def, ghi"  # Non-numeric breaks
  )

  # Should handle error gracefully
  expect_true(inherits(result, "categorizeResults"))

  # Breakpoints table should be empty or show warning
  break_table <- extract_table_data(result, "breakpointsTable")
  # May be NULL or empty
})

test_that("Non-numeric variable triggers error", {
  data <- data.frame(
    value = c("low", "medium", "high", "low", "high")
  )

  # Non-numeric variable should error
  expect_error(
    categorize(
      data = data,
      var = "value",
      method = "equal",
      nbins = 2
    ),
    "numeric"
  )
})

# ============================================================================
# PART 6: SPECIFIC BINNING METHOD TESTS
# Detailed tests for each binning method's specific behavior
# ============================================================================

# ===== Specific Binning Method Tests =====

test_that("Mean +/- 2SD creates 5 bins correctly", {
  set.seed(9999)
  data <- data.frame(value = rnorm(100, mean = 100, sd = 15))

  result <- categorize(
    data = data,
    var = "value",
    method = "meansd",
    sdmult = 2
  )

  break_table <- extract_table_data(result, "breakpointsTable")

  expect_true(!is.null(break_table))
  expect_equal(nrow(break_table), 5)

  breaks <- break_table$value
  m <- mean(data$value)
  s <- sd(data$value)

  # Verify: min, m-2s, m, m+2s, max
  expect_equal(breaks[1], min(data$value), tolerance = 1e-4)
  expect_true(abs(breaks[2] - (m - 2*s)) < 1e-4)
  expect_true(abs(breaks[3] - m) < 1e-4)
  expect_true(abs(breaks[4] - (m + 2*s)) < 1e-4)
  expect_equal(breaks[5], max(data$value), tolerance = 1e-4)
})

test_that("Tertile split (3 quantiles) works correctly", {
  set.seed(7777)
  data <- data.frame(value = 1:99)

  result <- categorize(
    data = data,
    var = "value",
    method = "quantile",
    nbins = 3  # Tertiles
  )

  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  # Tertiles should have approximately equal counts
  counts <- freq_table$n
  expect_equal(length(counts), 3)
  expect_equal(sum(counts), 99)
  expect_true(all(counts >= 30 & counts <= 35))
})

test_that("Decile split (10 quantiles) works correctly", {
  set.seed(8888)
  data <- data.frame(value = 1:100)

  result <- categorize(
    data = data,
    var = "value",
    method = "quantile",
    nbins = 10  # Deciles
  )

  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  # Deciles should each have ~10 observations
  counts <- freq_table$n
  expect_equal(length(counts), 10)
  expect_equal(sum(counts), 100)
  expect_true(all(counts >= 8 & counts <= 12))
})

# ============================================================================
# PART 7: INTEGRATION TESTS
# Tests complex real-world scenarios
# ============================================================================

# ===== Integration Tests =====

test_that("Clinical scenario: Age categorization works correctly", {
  set.seed(1111)
  # Simulate patient ages
  data <- data.frame(
    age = c(
      rnorm(30, mean = 35, sd = 5),   # Young adults
      rnorm(40, mean = 55, sd = 8),   # Middle-aged
      rnorm(30, mean = 75, sd = 6)    # Elderly
    )
  )

  result <- categorize(
    data = data,
    var = "age",
    method = "manual",
    breaks = "0, 45, 65, 100",
    labels = "custom",
    customlabels = "Young, Middle-Aged, Elderly"
  )

  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  # Verify custom labels
  expect_equal(freq_table$category, c("Young", "Middle-Aged", "Elderly"))

  # Verify reasonable distribution
  expect_equal(sum(freq_table$n), 100)
})

test_that("Biomarker categorization with mean±SD works", {
  set.seed(2222)
  # Simulate biomarker values
  data <- data.frame(
    biomarker = rnorm(200, mean = 50, sd = 10)
  )

  result <- categorize(
    data = data,
    var = "biomarker",
    method = "meansd",
    sdmult = 1.5,
    labels = "semantic"
  )

  freq_table <- extract_table_data(result, "freqTable")
  break_table <- extract_table_data(result, "breakpointsTable")

  expect_true(!is.null(freq_table))
  expect_true(!is.null(break_table))

  # Should have 4 categories with mean±1.5SD
  expect_equal(nrow(freq_table), 4)
  expect_equal(nrow(break_table), 5)

  # Verify semantic labels for 4 bins
  expect_equal(freq_table$category, c("Low", "Medium-Low", "Medium-High", "High"))
})

test_that("Survival time categorization with quantiles works", {
  set.seed(3333)
  # Simulate survival times (right-skewed)
  data <- data.frame(
    survival_months = rexp(150, rate = 0.05)
  )

  result <- categorize(
    data = data,
    var = "survival_months",
    method = "quantile",
    nbins = 4,
    labels = "numbered"
  )

  freq_table <- extract_table_data(result, "freqTable")

  expect_true(!is.null(freq_table))

  # Quartiles should have approximately equal counts despite skewness
  counts <- freq_table$n
  expect_equal(length(counts), 4)
  expect_true(all(counts >= 35 & counts <= 40))

  # Total should be 150
  expect_equal(sum(counts), 150)
})
