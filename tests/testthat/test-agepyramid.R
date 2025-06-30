test_that("agepyramid module loads correctly", {
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