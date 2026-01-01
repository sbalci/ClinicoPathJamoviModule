test_that("agepyramid module loads correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  expect_true(exists("agepyramidClass"))
  expect_true(is.function(agepyramid))
})

test_that("agepyramid handles basic input validation", {
  # Test with missing required variables
  expect_error(
    agepyramid(data = histopathology, age = NULL, gender = "Sex", female = "Female"),
    NA  # Should not error during initialization, only during run
  )
  
  expect_error(
    agepyramid(data = histopathology, age = "Age", gender = NULL, female = "Female"),
    NA  # Should not error during initialization, only during run
  )
  
  expect_error(
    agepyramid(data = histopathology, age = "Age", gender = "Sex", female = NULL),
    NA  # Should not error during initialization, only during run
  )
})

test_that("agepyramid works with valid inputs", {
  # Test basic functionality
  result <- agepyramid(
    data = histopathology,
    age = "Age",
    gender = "Sex",
    female = "Female"
  )
  
  expect_s3_class(result, "agepyramidClass")
  expect_true("Age" %in% names(histopathology))
  expect_true("Sex" %in% names(histopathology))
})

test_that("agepyramid handles different bin widths correctly", {
  # Test different bin width values
  bin_widths <- c(2, 5, 10, 15)
  
  for (bin_width in bin_widths) {
    expect_error({
      result <- agepyramid(
        data = histopathology,
        age = "Age",
        gender = "Sex",
        female = "Female",
        bin_width = bin_width
      )
    }, NA, info = paste("bin_width:", bin_width))
  }
})

test_that("agepyramid handles custom plot titles correctly", {
  # Test custom plot titles
  titles <- c("Custom Title", "Age Distribution", "Population Pyramid", "")
  
  for (title in titles) {
    expect_error({
      result <- agepyramid(
        data = histopathology,
        age = "Age",
        gender = "Sex",
        female = "Female",
        plot_title = title
      )
    }, NA, info = paste("plot_title:", title))
  }
})

test_that("agepyramid handles custom colors correctly", {
  # Test different color specifications
  color_combinations <- list(
    list(color1 = "#FF5733", color2 = "#3498DB"),  # Hex codes
    list(color1 = "red", color2 = "blue"),         # Named colors
    list(color1 = "#E74C3C", color2 = "#2ECC71"), # Different hex codes
    list(color1 = "darkgreen", color2 = "orange") # More named colors
  )
  
  for (i in seq_along(color_combinations)) {
    colors <- color_combinations[[i]]
    expect_error({
      result <- agepyramid(
        data = histopathology,
        age = "Age",
        gender = "Sex",
        female = "Female",
        color1 = colors$color1,
        color2 = colors$color2
      )
    }, NA, info = paste("color combination", i))
  }
})

test_that("agepyramid handles different gender levels correctly", {
  # Test with different female level specifications
  # First check what levels exist in the data
  gender_levels <- unique(histopathology$Sex)
  
  if ("Female" %in% gender_levels) {
    expect_error({
      result <- agepyramid(
        data = histopathology,
        age = "Age",
        gender = "Sex",
        female = "Female"
      )
    }, NA)
  }
  
  if ("Male" %in% gender_levels) {
    # Test with Male as "female" level (should work but flip the pyramid)
    expect_error({
      result <- agepyramid(
        data = histopathology,
        age = "Age",
        gender = "Sex",
        female = "Male"
      )
    }, NA)
  }
})

test_that("agepyramid handles missing data appropriately", {
  # Create dataset with missing values
  test_data <- histopathology
  test_data$Age[1:5] <- NA
  test_data$Sex[6:10] <- NA
  
  expect_error({
    result <- agepyramid(
      data = test_data,
      age = "Age",
      gender = "Sex",
      female = "Female"
    )
  }, NA)
})

test_that("agepyramid parameter combinations work correctly", {
  # Test complex parameter combinations
  expect_error({
    result <- agepyramid(
      data = histopathology,
      age = "Age",
      gender = "Sex",
      female = "Female",
      bin_width = 8,
      plot_title = "Comprehensive Age Analysis",
      color1 = "#9B59B6",
      color2 = "#1ABC9C"
    )
  }, NA)
})

test_that("agepyramid handles edge cases for bin width", {
  # Test boundary values for bin width
  expect_error({
    result <- agepyramid(
      data = histopathology,
      age = "Age",
      gender = "Sex",
      female = "Female",
      bin_width = 1  # Very small bins
    )
  }, NA)
  
  expect_error({
    result <- agepyramid(
      data = histopathology,
      age = "Age",
      gender = "Sex",
      female = "Female",
      bin_width = 20  # Large bins
    )
  }, NA)
})

test_that("agepyramid handles different data types", {
  # Test with factor gender variable
  test_data <- histopathology
  test_data$Sex <- factor(test_data$Sex)
  
  expect_error({
    result <- agepyramid(
      data = test_data,
      age = "Age",
      gender = "Sex",
      female = "Female"
    )
  }, NA)
  
  # Test with numeric age variable (should work)
  expect_error({
    result <- agepyramid(
      data = histopathology,
      age = "Age",
      gender = "Sex",
      female = "Female"
    )
  }, NA)
})

test_that("agepyramid handles small datasets", {
  # Test with small dataset
  small_data <- histopathology[1:20, ]
  
  expect_error({
    result <- agepyramid(
      data = small_data,
      age = "Age",
      gender = "Sex",
      female = "Female"
    )
  }, NA)
})

test_that("agepyramid results have expected structure", {
  # Test that results object has expected components
  result <- agepyramid(
    data = histopathology,
    age = "Age",
    gender = "Sex",
    female = "Female"
  )
  
  # Check for expected result components
  expect_true(exists("pyramidTable", envir = result))
  expect_true(exists("plot", envir = result))
})

test_that("agepyramid handles synthetic data correctly", {
  # Create synthetic dataset for testing
  set.seed(123)
  synthetic_data <- data.frame(
    age = sample(18:80, 100, replace = TRUE),
    gender = sample(c("F", "M"), 100, replace = TRUE, prob = c(0.6, 0.4))
  )
  
  expect_error({
    result <- agepyramid(
      data = synthetic_data,
      age = "age",
      gender = "gender",
      female = "F",
      bin_width = 10,
      plot_title = "Synthetic Data Age Pyramid"
    )
  }, NA)
})

test_that("agepyramid handles different age ranges", {
  # Test with different age data ranges
  # Young population
  young_data <- data.frame(
    age = sample(18:35, 50, replace = TRUE),
    gender = sample(c("Female", "Male"), 50, replace = TRUE)
  )
  
  expect_error({
    result <- agepyramid(
      data = young_data,
      age = "age",
      gender = "gender",
      female = "Female",
      bin_width = 5
    )
  }, NA)
  
  # Older population
  older_data <- data.frame(
    age = sample(50:90, 50, replace = TRUE),
    gender = sample(c("Female", "Male"), 50, replace = TRUE)
  )
  
  expect_error({
    result <- agepyramid(
      data = older_data,
      age = "age",
      gender = "gender",
      female = "Female",
      bin_width = 10
    )
  }, NA)
})

test_that("agepyramid default values work correctly", {
  # Test that default values are applied correctly
  result <- agepyramid(
    data = histopathology,
    age = "Age",
    gender = "Sex",
    female = "Female"
    # Using all default values for optional parameters
  )
  
  expect_s3_class(result, "agepyramidClass")
})

test_that("agepyramid handles unbalanced gender data", {
  # Create dataset with unbalanced gender distribution
  unbalanced_data <- data.frame(
    age = c(sample(25:65, 80, replace = TRUE), sample(30:70, 20, replace = TRUE)),
    gender = c(rep("Female", 80), rep("Male", 20))
  )
  
  expect_error({
    result <- agepyramid(
      data = unbalanced_data,
      age = "age",
      gender = "gender",
      female = "Female",
      bin_width = 10,
      plot_title = "Unbalanced Gender Distribution"
    )
  }, NA)
})

test_that("agepyramid handles special characters in plot title", {
  # Test plot titles with special characters
  special_titles <- c(
    "Age Pyramid: 2024 Analysis",
    "Population (N=250)",
    "Age Distribution - Clinical Study",
    "Pyramid Chart: α=0.05"
  )
  
  for (title in special_titles) {
    expect_error({
      result <- agepyramid(
        data = histopathology,
        age = "Age",
        gender = "Sex",
        female = "Female",
        plot_title = title
      )
    }, NA, info = paste("special title:", title))
  }
})

test_that("agepyramid dependency handling", {
  # Test function behavior when required packages are available
  required_packages <- c("ggcharts", "dplyr", "tidyr", "tibble")
  
  for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      expect_error({
        result <- agepyramid(
          data = histopathology,
          age = "Age",
          gender = "Sex",
          female = "Female"
        )
      }, NA, info = paste("package:", pkg))
    } else {
      skip(paste("Package", pkg, "not available"))
    }
  }
})

test_that("agepyramid vs CLAUDE.md parameter usage", {
  # Test the specific parameter pattern mentioned in CLAUDE.md
  # "agepyramid: Use gender = "Sex", female = "Female" (not sex = "Sex")"
  
  # Correct usage according to CLAUDE.md
  expect_error({
    result <- agepyramid(
      data = histopathology,
      age = "Age",
      gender = "Sex",      # Use 'gender' parameter
      female = "Female"    # Use 'female' level
    )
  }, NA)
  
  # Test that results object has expected structure
  result <- agepyramid(
    data = histopathology,
    age = "Age",
    gender = "Sex",
    female = "Female"
  )
  
  expect_true(exists("pyramidTable", envir = result))
  expect_true(exists("plot", envir = result))
})

test_that("agepyramid output is correct and stable", {
  # 1. Snapshot test for the pyramidTable
  result <- agepyramid(
    data = histopathology,
    age = "Age",
    gender = "Sex",
    female = "Female",
    bin_width = 10
  )
  
  # To update snapshots, run testthat::snapshot_review()
  expect_snapshot(result$pyramidTable$asDF)
  
  # 2. Test dataInfo HTML content for a standard run
  expect_true(grepl("Data Summary", result$dataInfo$content))
  expect_true(grepl("Initial observations", result$dataInfo$content))
  
  # 3. Test single-gender cohort functionality
  female_only_data <- histopathology[histopathology$Sex == "Female", ]
  result_single_gender <- agepyramid(
    data = female_only_data,
    age = "Age",
    gender = "Sex",
    female = "Female"
  )
  
  # Check for single-gender message in dataInfo
  expect_true(grepl("Single-gender cohort detected", result_single_gender$dataInfo$content))
  
  # Check that the plot is a simple bar chart (not a pyramid)
  # We can't directly test the plot object, but we can check the plot's state
  plot_state <- result_single_gender$plot$state
  expect_equal(ncol(plot_state), 3) # Should have Pop, Gender, n
  expect_equal(unique(plot_state$Gender), "Single Gender Cohort")
  
  # 4. Test correctness of age bin labels
  result_geriatric <- agepyramid(
    data = histopathology,
    age = "Age",
    gender = "Sex",
    female = "Female",
    age_groups = "geriatric"
  )
  
  table_df <- result_geriatric$pyramidTable$asDF
  # The last age group should be "95+"
  expect_true("95+" %in% table_df$Pop)
})

# REGRESSION TESTS for critical fixes (2025-01-18)
# These tests prevent the reintroduction of two serious bugs that undermined
# statistical accuracy and user trust

test_that("REGRESSION: age bin labels accurately reflect bin boundaries (right=TRUE)", {
  # Issue: Labels showed "0-4" but actual bins were (0,5] which includes age 5
  # Fix: Labels now show "1-5" to correctly represent interval (0,5]

  # Create test data with exact age values at bin boundaries
  test_data <- data.frame(
    age = c(5, 10, 15, 20, 25, 30),  # All at upper bin boundaries
    gender = rep(c("Female", "Male"), 3)
  )

  result <- agepyramid(
    data = test_data,
    age = "age",
    gender = "gender",
    female = "Female",
    bin_width = 5
  )

  table_df <- result$pyramidTable$asDF

  # With right=TRUE, cut() creates bins (0,5], (5,10], (10,15], etc.
  # Age 5 should be in bin (0,5] labeled "1-5"
  # Age 10 should be in bin (5,10] labeled "6-10"
  # Age 15 should be in bin (10,15] labeled "11-15"

  # Check that labels reflect inclusive upper bounds
  age_groups <- as.character(table_df$Pop[table_df$Pop != "Total"])

  # Expected labels for bin_width=5 starting at 0
  # (0,5] -> "1-5", (5,10] -> "6-10", (10,15] -> "11-15", etc.
  expect_true(any(grepl("1-5", age_groups)),
              info = "Label should show '1-5' for interval (0,5]")
  expect_true(any(grepl("6-10", age_groups)),
              info = "Label should show '6-10' for interval (5,10]")
  expect_true(any(grepl("11-15", age_groups)),
              info = "Label should show '11-15' for interval (10,15]")

  # Verify that old incorrect pattern "0-4", "5-9" is NOT present
  expect_false(any(grepl("^0-4$", age_groups)),
               info = "Old incorrect label '0-4' should not appear")
  expect_false(any(grepl("^5-9$", age_groups)),
               info = "Old incorrect label '5-9' should not appear")

  # Test open-ended category for preset groups
  result_geriatric <- agepyramid(
    data = data.frame(
      age = c(65, 70, 75, 80, 85, 90, 95, 100),
      gender = rep(c("Female", "Male"), 4)
    ),
    age = "age",
    gender = "gender",
    female = "Female",
    age_groups = "geriatric"  # Has breaks: c(0, 65, 70, 75, 80, 85, 90, 95, Inf)
  )

  table_df_geriatric <- result_geriatric$pyramidTable$asDF
  age_groups_geriatric <- as.character(table_df_geriatric$Pop[table_df_geriatric$Pop != "Total"])

  # Last bin (95, Inf] should be labeled "96+" not "95+"
  expect_true(any(grepl("96\\+", age_groups_geriatric)),
              info = "Open-ended interval (95,Inf] should be labeled '96+'")
  expect_false(any(grepl("95\\+", age_groups_geriatric)),
               info = "Old incorrect label '95+' should not appear (should be '96+')")
})

test_that("REGRESSION: reported sample size matches aggregated table data", {
  # Issue: Rows with non-numeric ages stayed in mydata during n_final calculation
  #        but disappeared during cut()/aggregation, causing N mismatch
  # Fix: Explicitly filter NA ages before calculating n_final

  # Create test data with non-numeric age values
  test_data <- data.frame(
    age = c("25", "30", "35", "abc", "xyz", "40", "45", "50", "invalid", "55"),
    gender = c("Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male"),
    stringsAsFactors = FALSE
  )

  result <- agepyramid(
    data = test_data,
    age = "age",
    gender = "gender",
    female = "Female",
    bin_width = 10
  )

  # Extract reported counts from data summary HTML
  data_info <- result$dataInfo$content

  # Parse n_initial and n_final from HTML (they're in table rows)
  n_initial_match <- regmatches(data_info, regexpr("Initial observations.*?<td[^>]*>(\\d+)</td>", data_info, perl = TRUE))
  n_final_match <- regmatches(data_info, regexpr("Final observations.*?<td[^>]*>(\\d+)</td>", data_info, perl = TRUE))

  n_initial <- as.numeric(sub(".*?(\\d+)</td>", "\\1", n_initial_match))
  n_final <- as.numeric(sub(".*?(\\d+)</td>", "\\1", n_final_match))

  # Get actual counts from table (sum of Female + Male, excluding Total row)
  table_df <- result$pyramidTable$asDF
  table_data <- table_df[table_df$Pop != "Total", ]
  actual_female <- sum(table_data$Female, na.rm = TRUE)
  actual_male <- sum(table_data$Male, na.rm = TRUE)
  actual_total <- actual_female + actual_male

  # CRITICAL TEST: Reported n_final must equal sum of table counts
  expect_equal(n_final, actual_total,
               info = paste("Reported N (", n_final, ") must match table sum (", actual_total, ")",
                          "to avoid user confusion about analyzed sample size"))

  # Verify that invalid ages were actually excluded
  # We had 3 invalid values: "abc", "xyz", "invalid"
  # So n_final should be n_initial - 3
  expect_equal(n_final, n_initial - 3,
               info = "Should have excluded exactly 3 non-numeric age values")

  # Check that exclusion is properly reported in HTML
  expect_true(grepl("Non-numeric ages", data_info) || grepl("non-numeric age values", data_info),
              info = "Data quality message should mention non-numeric age exclusions")
})

test_that("REGRESSION: exclusion breakdown in summary is accurate and complete", {
  # Test that all exclusion sources are tracked and reported correctly

  # Create data with multiple exclusion types
  test_data <- data.frame(
    age = c("25", "30", "NA", "35", "abc", "40", "45", "50"),
    gender = c("Female", "Male", "Unknown", NA, "Female", "Male", "Female", "Other"),
    stringsAsFactors = FALSE
  )

  result <- agepyramid(
    data = test_data,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    bin_width = 10
  )

  data_info <- result$dataInfo$content

  # Should report both age and gender exclusions
  expect_true(grepl("Non-numeric ages|non-numeric age values", data_info),
              info = "Should report age conversion failures")
  expect_true(grepl("Missing/unrecognized gender|Gender Exclusions", data_info),
              info = "Should report gender exclusions")

  # Parse exclusion counts from HTML table
  # Look for "- Non-numeric ages:" and "- Missing/unrecognized gender:" rows
  age_excl_match <- regmatches(data_info,
                                regexpr("Non-numeric ages.*?<td[^>]*color: #d32f2f[^>]*>(\\d+)</td>",
                                       data_info, perl = TRUE))
  gender_excl_match <- regmatches(data_info,
                                   regexpr("Missing/unrecognized gender.*?<td[^>]*color: #d32f2f[^>]*>(\\d+)</td>",
                                          data_info, perl = TRUE))

  # Verify exclusion counts are shown in breakdown
  if (length(age_excl_match) > 0) {
    expect_true(grepl("\\d+", age_excl_match),
                info = "Age exclusion count should be displayed")
  }

  if (length(gender_excl_match) > 0) {
    expect_true(grepl("\\d+", gender_excl_match),
                info = "Gender exclusion count should be displayed")
  }

  # Verify table sum still matches reported n_final
  table_df <- result$pyramidTable$asDF
  table_data <- table_df[table_df$Pop != "Total", ]
  actual_total <- sum(table_data$Female, na.rm = TRUE) + sum(table_data$Male, na.rm = TRUE)

  # Extract n_final from HTML
  n_final_match <- regmatches(data_info, regexpr("Final observations.*?<td[^>]*>(\\d+)</td>", data_info, perl = TRUE))
  n_final <- as.numeric(sub(".*?(\\d+)</td>", "\\1", n_final_match))

  expect_equal(n_final, actual_total,
               info = "Even with multiple exclusion types, reported N must match table sum")
})
