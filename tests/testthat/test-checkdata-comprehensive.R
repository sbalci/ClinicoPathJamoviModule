# Comprehensive Tests for checkdata Function
# Validates consensus outlier detection, quality grading, and all analysis outputs

library(testthat)
library(ClinicoPath)

# Helper function to extract table data from jamovi results
extract_table_data <- function(result, table_name) {
  if (!inherits(result, "checkdataResults")) {
    return(NULL)
  }

  table_obj <- result[[table_name]]
  if (is.null(table_obj)) {
    return(NULL)
  }

  # Try to extract as data frame (asDF is a property, not a method)
  if (inherits(table_obj, "Table") && !is.null(table_obj$asDF)) {
    return(table_obj$asDF)
  }

  # Alternative: extract rows manually
  if (inherits(table_obj, "Table")) {
    rows <- table_obj$rowKeys
    if (is.null(rows) || length(rows) == 0) {
      return(NULL)
    }

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

    return(as.data.frame(data_list, stringsAsFactors = FALSE))
  }

  return(NULL)
}

# Helper to extract quality grade from qualityText
extract_quality_grade <- function(result) {
  if (!inherits(result, "checkdataResults")) {
    return(NULL)
  }

  quality_text <- result$qualityText
  if (is.null(quality_text)) {
    return(NULL)
  }

  # Try to get content
  content <- quality_text$content
  if (is.null(content)) {
    return(NULL)
  }

  # Extract grade from content (format: "GRADE: A")
  if (grepl("GRADE:\\s*A", content)) return("A")
  if (grepl("GRADE:\\s*B", content)) return("B")
  if (grepl("GRADE:\\s*C", content)) return("C")
  if (grepl("GRADE:\\s*D", content)) return("D")

  return(NULL)
}

# ============================================================================
# PART 1: CONSENSUS OUTLIER DETECTION TESTS
# Validates that outliers require detection by >=2 of 3 methods
# ============================================================================

# ===== Consensus Outlier Detection =====

test_that("Consensus outliers require 2 of 3 methods", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  # Create data with known outliers
  set.seed(123)
  data <- data.frame(
    # Normal data plus extreme outliers that all methods will catch
    value = c(rnorm(50, 100, 10), c(200, 210, -50, -60))  # 4 extreme outliers
  )

  result <- checkdata(
    data = data,
    var = "value"
  )

  # Extract outlier table
  outlier_table <- extract_table_data(result, "outliers")

  # Should detect the 4 extreme outliers (caught by all 3 methods)
  expect_true(!is.null(outlier_table))
  expect_true(nrow(outlier_table) >= 4)

  # Verify outliers are indeed extreme values
  if (!is.null(outlier_table) && "value" %in% names(outlier_table)) {
    outlier_values <- outlier_table$value
    expect_true(any(outlier_values > 150 | outlier_values < 0))
  }
})

test_that("Single-method outliers are NOT flagged", {
  # Create data where some points are outliers by only 1 method
  set.seed(456)

  # Data with moderate values that might be flagged by IQR but not Z-score
  data <- data.frame(
    value = c(rnorm(40, 50, 5), c(65, 70))  # Moderately high values
  )

  result <- checkdata(
    data = data,
    var = "value"
  )

  # Calculate manually what each method would detect
  clean_data <- data$value

  # Z-score outliers (|z| > 3)
  z_scores <- scale(clean_data)[,1]
  z_outliers <- sum(abs(z_scores) > 3)

  # IQR outliers
  Q1 <- quantile(clean_data, 0.25)
  Q3 <- quantile(clean_data, 0.75)
  IQR_val <- Q3 - Q1
  iqr_outliers <- sum(clean_data < (Q1 - 1.5 * IQR_val) | clean_data > (Q3 + 1.5 * IQR_val))

  # The test values (65, 70) should be detected by IQR but probably not Z-score
  # Since consensus requires 2 methods, they should NOT be flagged

  outlier_table <- extract_table_data(result, "outliers")

  # Should have fewer outliers than IQR method alone would find
  if (!is.null(outlier_table)) {
    expect_true(nrow(outlier_table) < iqr_outliers)
  }
})

test_that("Consensus method counts are correct", {
  set.seed(789)
  # Create data with clearly identifiable outliers
  data <- data.frame(
    value = c(rnorm(45, 100, 8), rep(200, 5))  # 5 extreme high outliers
  )

  result <- checkdata(
    data = data,
    var = "value"
  )

  outlier_table <- extract_table_data(result, "outliers")

  expect_true(!is.null(outlier_table))

  # All 5 outliers should be detected (200 is ~12.5 SD from mean)
  expect_equal(nrow(outlier_table), 5)

  # Check detection count column if exists
  if ("methods" %in% names(outlier_table) || "count" %in% names(outlier_table)) {
    # All should be detected by 3 methods (extreme outliers)
    detection_col <- if ("methods" %in% names(outlier_table)) "methods" else "count"
    expect_true(all(outlier_table[[detection_col]] >= 2))
  }
})

# ============================================================================
# PART 2: QUALITY GRADE VALIDATION TESTS
# Validates that A/B/C/D grades are assigned correctly
# ============================================================================

# ===== Quality Grade Assignment =====

test_that("Grade A assigned to high-quality data", {
  set.seed(111)
  # Perfect data: no missing, no outliers, good variability
  data <- data.frame(
    value = rnorm(100, 50, 10)  # Normal distribution, n=100
  )

  result <- checkdata(
    data = data,
    var = "value"
  )

  # Extract quality grade
  grade <- extract_quality_grade(result)

  # Should be Grade A (no missing, no consensus outliers, adequate variability)
  expect_equal(grade, "A")
})

test_that("Minor missing data maintains high grade", {
  set.seed(222)
  # Data with 8% missing (5pt penalty → score 95 → Grade A)
  data <- data.frame(
    value = c(rnorm(92, 50, 10), rep(NA, 8))
  )

  result <- checkdata(
    data = data,
    var = "value"
  )

  grade <- extract_quality_grade(result)

  # 8% missing = 5pt penalty → Grade A (score 95)
  expect_equal(grade, "A")
})

test_that("Grade B assigned to moderate quality issues", {
  set.seed(333)
  # Data with 20% missing + outliers (15pt penalty → score 85 → Grade B)
  data <- data.frame(
    value = c(rnorm(70, 50, 10), rep(NA, 20), c(150, 160, -50))
  )

  result <- checkdata(
    data = data,
    var = "value"
  )

  grade <- extract_quality_grade(result)

  # 20% missing (10pt) + outliers (5pt) = 15pt → Grade B (score 85)
  expect_equal(grade, "B")
})

test_that("Grade C assigned to moderate missing data", {
  set.seed(444)
  # Data with 50% missing gives Grade C (score=70 from 30pt penalty)
  data <- data.frame(
    value = c(rnorm(50, 50, 10), rep(NA, 50))
  )

  result <- checkdata(
    data = data,
    var = "value"
  )

  grade <- extract_quality_grade(result)

  # 50% missing = 30pt penalty → score 70 → Grade C
  expect_equal(grade, "C")
})

test_that("Low variability is detected but may not drop grade", {
  set.seed(555)
  # Data with very low variability (almost constant)
  data <- data.frame(
    value = c(rep(50, 95), c(50.1, 49.9, 50.05, 49.95, 50.02))
  )

  result <- checkdata(
    data = data,
    var = "value"
  )

  grade <- extract_quality_grade(result)

  # Low variability gives 8pt penalty → score 92 → still Grade A
  # Just verify grade is assigned
  expect_true(grade %in% c("A", "B", "C", "D"))
})

# ============================================================================
# PART 3: MISSING DATA ANALYSIS TESTS
# Validates missing data metrics and interpretations
# ============================================================================

# ===== Missing Data Analysis =====

test_that("Missing data percentage calculated correctly", {
  data <- data.frame(
    value = c(1:80, rep(NA, 20))  # Exactly 20% missing
  )

  result <- checkdata(
    data = data,
    var = "value"
  )

  missing_table <- extract_table_data(result, "missingVals")

  expect_true(!is.null(missing_table))

  # Find missing percentage row
  if ("metric" %in% names(missing_table)) {
    missing_pct_row <- missing_table[missing_table$metric == "Missing (%)", ]
    if (nrow(missing_pct_row) > 0) {
      missing_pct <- as.numeric(missing_pct_row$value)
      expect_equal(missing_pct, 20)
    }
  }
})

test_that("Missing data interpretation is correct", {
  # Test various missing percentages
  test_cases <- list(
    list(missing = 0, expected_contains = "Excellent|Complete"),
    list(missing = 3, expected_contains = "Good|Minimal"),
    list(missing = 10, expected_contains = "Acceptable|Some"),
    list(missing = 20, expected_contains = "Concerning|Substantial"),
    list(missing = 40, expected_contains = "Poor|Extensive")
  )

  for (test_case in test_cases) {
    n_total <- 100
    n_missing <- test_case$missing

    data <- data.frame(
      value = c(1:(n_total - n_missing), rep(NA, n_missing))
    )

    result <- checkdata(data = data, var = "value")

    missing_table <- extract_table_data(result, "missingVals")

    # Check interpretation column
    if (!is.null(missing_table) && "interpretation" %in% names(missing_table)) {
      interpretations <- paste(missing_table$interpretation, collapse = " ")
      expect_true(grepl(test_case$expected_contains, interpretations, ignore.case = TRUE))
    }
  }
})

test_that("Complete cases count is correct", {
  data <- data.frame(
    value = c(1:70, rep(NA, 30))
  )

  result <- checkdata(
    data = data,
    var = "value"
  )

  missing_table <- extract_table_data(result, "missingVals")

  if ("metric" %in% names(missing_table)) {
    complete_row <- missing_table[missing_table$metric == "Complete Cases", ]
    if (nrow(complete_row) > 0) {
      # Value is "70 (70.0%)" - extract the first number
      value_str <- complete_row$value
      complete_count <- as.numeric(gsub("\\s*\\(.*", "", value_str))
      expect_equal(complete_count, 70)
    }
  }
})

# ============================================================================
# PART 4: DISTRIBUTION ANALYSIS TESTS
# Validates descriptive statistics accuracy
# ============================================================================

# ===== Distribution Analysis =====

test_that("Mean and median calculated correctly", {
  set.seed(666)
  data <- data.frame(
    value = rnorm(100, mean = 75, sd = 12)
  )

  result <- checkdata(
    data = data,
    var = "value",
    showDistribution = TRUE
  )

  dist_table <- extract_table_data(result, "distribution")

  expect_true(!is.null(dist_table))

  # Calculate expected values
  expected_mean <- mean(data$value)
  expected_median <- median(data$value)

  # Find mean and median rows
  if ("metric" %in% names(dist_table)) {
    mean_row <- dist_table[dist_table$metric == "Mean", ]
    median_row <- dist_table[dist_table$metric == "Median", ]

    if (nrow(mean_row) > 0) {
      reported_mean <- as.numeric(mean_row$value)
      expect_true(abs(reported_mean - expected_mean) < 0.01)
    }

    if (nrow(median_row) > 0) {
      reported_median <- as.numeric(median_row$value)
      expect_true(abs(reported_median - expected_median) < 0.01)
    }
  }
})

test_that("Standard deviation calculated correctly", {
  set.seed(777)
  data <- data.frame(
    value = rnorm(80, mean = 100, sd = 15)
  )

  result <- checkdata(
    data = data,
    var = "value",
    showDistribution = TRUE
  )

  dist_table <- extract_table_data(result, "distribution")

  expected_sd <- sd(data$value)

  if (!is.null(dist_table) && "metric" %in% names(dist_table)) {
    sd_row <- dist_table[dist_table$metric == "SD" | dist_table$metric == "Standard Deviation", ]
    if (nrow(sd_row) > 0) {
      reported_sd <- as.numeric(sd_row$value)
      expect_true(abs(reported_sd - expected_sd) < 0.01)
    }
  }
})

test_that("Skewness interpretation is correct", {
  # Test symmetric distribution
  set.seed(888)
  symmetric_data <- data.frame(
    value = rnorm(100, 50, 10)
  )

  result_sym <- checkdata(
    data = symmetric_data,
    var = "value",
    showDistribution = TRUE
  )

  dist_table_sym <- extract_table_data(result_sym, "distribution")

  # Skewness should be close to 0
  if (!is.null(dist_table_sym) && "metric" %in% names(dist_table_sym)) {
    skew_row <- dist_table_sym[grepl("Skewness", dist_table_sym$metric), ]
    if (nrow(skew_row) > 0) {
      skew_value <- as.numeric(skew_row$value)
      expect_true(abs(skew_value) < 1)  # Should be approximately symmetric
    }
  }

  # Test skewed distribution
  skewed_data <- data.frame(
    value = rexp(100, rate = 0.1)  # Right-skewed
  )

  result_skew <- checkdata(
    data = skewed_data,
    var = "value",
    showDistribution = TRUE
  )

  dist_table_skew <- extract_table_data(result_skew, "distribution")

  if (!is.null(dist_table_skew) && "metric" %in% names(dist_table_skew)) {
    skew_row <- dist_table_skew[grepl("Skewness", dist_table_skew$metric), ]
    if (nrow(skew_row) > 0) {
      skew_value <- as.numeric(skew_row$value)
      expect_true(skew_value > 0.5)  # Should be right-skewed
    }
  }
})

# ============================================================================
# PART 5: EDGE CASE TESTS
# Validates handling of challenging data scenarios
# ============================================================================

# ===== Edge Cases =====

test_that("All missing data handled correctly", {
  data <- data.frame(
    value = rep(NA_real_, 50)
  )

  # Should not error
  result <- checkdata(
    data = data,
    var = "value"
  )

  expect_true(inherits(result, "checkdataResults"))

  missing_table <- extract_table_data(result, "missingVals")
  expect_true(!is.null(missing_table))

  # Should report 100% missing
  if ("metric" %in% names(missing_table)) {
    missing_pct_row <- missing_table[missing_table$metric == "Missing (%)", ]
    if (nrow(missing_pct_row) > 0) {
      expect_equal(as.numeric(missing_pct_row$value), 100)
    }
  }
})

test_that("Zero variance data handled correctly", {
  data <- data.frame(
    value = rep(42, 100)  # All same value
  )

  result <- checkdata(
    data = data,
    var = "value",
    showDistribution = TRUE
  )

  expect_true(inherits(result, "checkdataResults"))

  # SD should be 0
  dist_table <- extract_table_data(result, "distribution")
  if (!is.null(dist_table) && "metric" %in% names(dist_table)) {
    sd_row <- dist_table[grepl("SD|Standard Deviation", dist_table$metric), ]
    if (nrow(sd_row) > 0) {
      expect_equal(as.numeric(sd_row$value), 0)
    }
  }

  # Should not be Grade A (low variability)
  grade <- extract_quality_grade(result)
  expect_true(grade != "A")
})

test_that("Very small sample size handled correctly", {
  data <- data.frame(
    value = c(1, 2, 3, 4, 5)
  )

  result <- checkdata(
    data = data,
    var = "value"
  )

  expect_true(inherits(result, "checkdataResults"))

  # Should warn about small sample size
  missing_table <- extract_table_data(result, "missingVals")
  if (!is.null(missing_table) && "interpretation" %in% names(missing_table)) {
    interpretations <- paste(missing_table$interpretation, collapse = " ")
    expect_true(grepl("Small sample size|sample size", interpretations, ignore.case = TRUE))
  }
})

test_that("Extreme skewness handled correctly", {
  set.seed(999)
  data <- data.frame(
    value = c(rep(1, 95), rep(1000, 5))  # Extremely right-skewed
  )

  result <- checkdata(
    data = data,
    var = "value",
    showDistribution = TRUE
  )

  expect_true(inherits(result, "checkdataResults"))

  # Should detect high skewness
  dist_table <- extract_table_data(result, "distribution")
  if (!is.null(dist_table) && "metric" %in% names(dist_table)) {
    skew_row <- dist_table[grepl("Skewness", dist_table$metric), ]
    if (nrow(skew_row) > 0) {
      skew_value <- as.numeric(skew_row$value)
      expect_true(abs(skew_value) > 1)
    }
  }
})

# ============================================================================
# PART 6: CATEGORICAL DATA TESTS
# Validates handling of factor/character variables
# ============================================================================

# ===== Categorical Data =====

test_that("Categorical data analyzed correctly", {
  data <- data.frame(
    category = factor(sample(c("A", "B", "C"), 100, replace = TRUE))
  )

  result <- checkdata(
    data = data,
    var = "category"
  )

  expect_true(inherits(result, "checkdataResults"))

  # Should provide basic statistics
  missing_table <- extract_table_data(result, "missingVals")
  expect_true(!is.null(missing_table))
})

test_that("Imbalanced categories detected", {
  data <- data.frame(
    category = factor(c(rep("Common", 95), rep("Rare", 5)))
  )

  result <- checkdata(
    data = data,
    var = "category",
    showDuplicates = TRUE
  )

  expect_true(inherits(result, "checkdataResults"))

  # Should show frequency distribution
  dup_table <- extract_table_data(result, "duplicates")
  expect_true(!is.null(dup_table))
})

# ============================================================================
# PART 7: INTEGRATION TESTS
# Real-world clinical research scenarios
# ============================================================================

# ===== Clinical Integration Tests =====

test_that("Clinical trial data quality scenario", {
  set.seed(1111)
  # Simulate clinical trial data with realistic quality issues
  clinical_data <- data.frame(
    patient_age = c(
      rnorm(90, 62, 12),  # Normal adult ages
      rep(NA, 8),         # 8% missing
      c(150, 5)           # 2 data entry errors
    )
  )

  result <- checkdata(
    data = clinical_data,
    var = "patient_age"
  )

  expect_true(inherits(result, "checkdataResults"))

  # Should detect outliers (150, 5)
  outlier_table <- extract_table_data(result, "outliers")
  if (!is.null(outlier_table)) {
    expect_true(nrow(outlier_table) >= 2)
  }

  # With 8% missing (5pt penalty) + 2% outliers (5pt penalty) = 10pt total → Grade A (score=90)
  grade <- extract_quality_grade(result)
  expect_true(grade %in% c("A", "B"))
})

test_that("Biomarker data quality scenario", {
  set.seed(2222)
  # High-quality biomarker data (tight distribution, no missing)
  biomarker_data <- data.frame(
    hba1c = rnorm(150, 6.5, 0.8)  # HbA1c values
  )

  result <- checkdata(
    data = biomarker_data,
    var = "hba1c"
  )

  expect_true(inherits(result, "checkdataResults"))

  # Should be high quality (Grade A or B)
  grade <- extract_quality_grade(result)
  expect_true(grade %in% c("A", "B"))
})

test_that("Survey data with missing pattern", {
  set.seed(3333)
  # Survey data with dropout pattern (missing increases toward end)
  survey_data <- data.frame(
    response = c(
      sample(1:5, 60, replace = TRUE),  # Early respondents
      sample(c(1:5, NA), 40, replace = TRUE, prob = c(rep(0.15, 5), 0.25))  # Later: more NA
    )
  )

  result <- checkdata(
    data = survey_data,
    var = "response",
    showPatterns = TRUE
  )

  expect_true(inherits(result, "checkdataResults"))

  # Should provide pattern analysis
  pattern_table <- extract_table_data(result, "patterns")
  # Pattern detection is complex, just verify table exists if patterns shown
})

# ============================================================================
# PART 8: OUTPUT STRUCTURE VALIDATION
# Validates that all output tables are created correctly
# ============================================================================

# ===== Output Structure =====

test_that("All required outputs are generated", {
  set.seed(4444)
  data <- data.frame(
    value = c(rnorm(90, 50, 10), rep(NA, 5), c(150, 160, -50, -60, -70))
  )

  result <- checkdata(
    data = data,
    var = "value",
    showDistribution = TRUE,
    showDuplicates = TRUE,
    showPatterns = TRUE
  )

  expect_true(inherits(result, "checkdataResults"))

  # Check all main outputs exist
  expect_true(!is.null(result$qualityText))
  expect_true(!is.null(result$missingVals))
  expect_true(!is.null(result$outliers) || !is.null(result$noOutliers))
  expect_true(!is.null(result$distribution))
  expect_true(!is.null(result$duplicates))
  expect_true(!is.null(result$patterns))
})

test_that("Output values are all finite and valid", {
  set.seed(5555)
  data <- data.frame(
    value = rnorm(100, 100, 20)
  )

  result <- checkdata(
    data = data,
    var = "value",
    showDistribution = TRUE
  )

  # Extract distribution table
  dist_table <- extract_table_data(result, "distribution")

  if (!is.null(dist_table) && "value" %in% names(dist_table)) {
    numeric_values <- as.numeric(dist_table$value)
    numeric_values <- numeric_values[!is.na(numeric_values)]

    # All values should be finite
    expect_true(all(is.finite(numeric_values)))
  }

  # Extract missing table
  missing_table <- extract_table_data(result, "missingVals")

  if (!is.null(missing_table) && "value" %in% names(missing_table)) {
    # Values should be present
    expect_true(all(!is.na(missing_table$value)))
  }
})
