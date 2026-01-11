library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("icccoeff module loads correctly", {
  expect_true(exists("icccoeffClass"))
  expect_true(is.function(icccoeff))
})

test_that("icccoeff handles basic input validation", {
  # Test with missing required variables
  expect_error(
    icccoeff(data = histopathology, vars = NULL),
    NA  # Should not error during initialization, only during run
  )
  
  # Test with insufficient variables (less than 2)
  expect_error(
    icccoeff(data = histopathology, vars = c("Age")),
    NA  # Should not error during initialization, only during run
  )
})

test_that("icccoeff works with basic continuous data", {
  # Test basic functionality with 2 continuous raters
  result <- icccoeff(
    data = histopathology,
    vars = c("Age", "LVI")
  )
  
  expect_true(inherits(result, "icccoeffResults"))
  expect_true("Age" %in% names(histopathology))
  expect_true("LVI" %in% names(histopathology))
})

test_that("icccoeff works with multiple continuous variables", {
  # Test with 3 continuous variables
  expect_error({
    result <- icccoeff(
      data = histopathology,
      vars = c("Age", "LVI", "PNI")
    )
  }, NA)
  
  # Test with 4 continuous variables
  expect_error({
    result <- icccoeff(
      data = histopathology,
      vars = c("Age", "LVI", "PNI", "Grade")
    )
  }, NA)
})

test_that("icccoeff different ICC types work", {
  # Test different ICC types
  icc_types <- c("icc1_1", "icc2_1", "icc3_1", "icc1_k", "icc2_k", "icc3_k")
  
  for (icc_type in icc_types) {
    expect_error({
      result <- icccoeff(
        data = histopathology,
        vars = c("Age", "LVI"),
        icc_type = icc_type
      )
    }, NA, info = paste("icc_type:", icc_type))
  }
})

test_that("icccoeff agreement types work", {
  # Test different agreement types
  agreement_types <- c("consistency", "agreement")
  
  for (agreement_type in agreement_types) {
    expect_error({
      result <- icccoeff(
        data = histopathology,
        vars = c("Age", "LVI"),
        agreement_type = agreement_type
      )
    }, NA, info = paste("agreement_type:", agreement_type))
  }
})

test_that("icccoeff confidence levels work", {
  # Test different confidence levels
  conf_levels <- c("0.90", "0.95", "0.99")
  
  for (conf_level in conf_levels) {
    expect_error({
      result <- icccoeff(
        data = histopathology,
        vars = c("Age", "LVI"),
        confidence_level = conf_level
      )
    }, NA, info = paste("confidence_level:", conf_level))
  }
})

test_that("icccoeff missing value handling works", {
  # Test different missing value handling
  missing_options <- c("complete", "pairwise")
  
  for (missing_option in missing_options) {
    expect_error({
      result <- icccoeff(
        data = histopathology,
        vars = c("Age", "LVI"),
        missing_values = missing_option
      )
    }, NA, info = paste("missing_values:", missing_option))
  }
})

test_that("icccoeff output options work", {
  # Test APA format
  expect_error({
    result <- icccoeff(
      data = histopathology,
      vars = c("Age", "LVI"),
      show_apa_format = TRUE
    )
  }, NA)
  
  # Test interpretation
  expect_error({
    result <- icccoeff(
      data = histopathology,
      vars = c("Age", "LVI"),
      show_interpretation = TRUE
    )
  }, NA)
  
  # Test confidence intervals
  expect_error({
    result <- icccoeff(
      data = histopathology,
      vars = c("Age", "LVI"),
      show_confidence_intervals = TRUE
    )
  }, NA)
  
  # Test F-test
  expect_error({
    result <- icccoeff(
      data = histopathology,
      vars = c("Age", "LVI"),
      show_f_test = TRUE
    )
  }, NA)
  
  # Test descriptive statistics
  expect_error({
    result <- icccoeff(
      data = histopathology,
      vars = c("Age", "LVI"),
      show_descriptive_stats = TRUE
    )
  }, NA)
})

test_that("icccoeff works with breast agreement data", {
  # Test with breast cancer Ki-67 data
  expect_error({
    result <- icccoeff(
      data = breast_agreement_data,
      vars = c("Ki67_Pathologist_1", "Ki67_Pathologist_2", "Ki67_Pathologist_3"),
      icc_type = "icc2_k",
      confidence_level = "0.95"
    )
  }, NA)
})

test_that("icccoeff handles missing data appropriately", {
  # Create dataset with missing values
  test_data <- histopathology
  test_data$Age[1:10] <- NA
  test_data$LVI[5:15] <- NA
  
  expect_error({
    result <- icccoeff(
      data = test_data,
      vars = c("Age", "LVI"),
      missing_values = "complete"
    )
  }, NA)
})

test_that("icccoeff complex parameter combinations work", {
  # Test comprehensive parameter combination
  expect_error({
    result <- icccoeff(
      data = histopathology,
      vars = c("Age", "LVI", "PNI"),
      icc_type = "icc2_k",
      agreement_type = "consistency",
      confidence_level = "0.95",
      missing_values = "complete",
      show_apa_format = TRUE,
      show_interpretation = TRUE,
      show_confidence_intervals = TRUE,
      show_f_test = TRUE,
      show_descriptive_stats = TRUE,
      decimal_places = 3,
      alpha_level = 0.05
    )
  }, NA)
})

test_that("icccoeff handles different variable types correctly", {
  # Test with continuous variables
  expect_error({
    result <- icccoeff(
      data = histopathology,
      vars = c("Age", "LVI")  # Continuous variables
    )
  }, NA)
})

test_that("icccoeff handles small datasets", {
  # Test with small dataset
  small_data <- histopathology[1:20, ]
  
  expect_error({
    result <- icccoeff(
      data = small_data,
      vars = c("Age", "LVI")
    )
  }, NA)
})

test_that("icccoeff results have expected structure", {
  # Test that results object has expected components
  result <- icccoeff(
    data = histopathology,
    vars = c("Age", "LVI")
  )
  
  # Check for expected result components
  expect_true(exists("instructions", envir = result))
  expect_true(exists("icc_results", envir = result))
})

test_that("icccoeff handles synthetic ICC data correctly", {
  # Create synthetic ICC data
  set.seed(123)
  synthetic_data <- data.frame(
    case_id = 1:100,
    rater1 = rnorm(100, 50, 10),
    rater2 = rnorm(100, 50, 10),
    rater3 = rnorm(100, 50, 10)
  )
  
  expect_error({
    result <- icccoeff(
      data = synthetic_data,
      vars = c("rater1", "rater2", "rater3"),
      icc_type = "icc2_k"
    )
  }, NA)
})

test_that("icccoeff dependency handling", {
  # Test function behavior when required packages are available
  required_packages <- c("irr")
  
  for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      expect_error({
        result <- icccoeff(
          data = histopathology,
          vars = c("Age", "LVI")
        )
      }, NA, info = paste("package:", pkg))
    } else {
      skip(paste("Package", pkg, "not available"))
    }
  }
})

test_that("icccoeff edge cases work correctly", {
  # Test with perfect correlation data
  perfect_data <- data.frame(
    case = 1:50,
    rater1 = rep(c(10, 20, 30), length.out = 50),
    rater2 = rep(c(10, 20, 30), length.out = 50),
    rater3 = rep(c(10, 20, 30), length.out = 50)
  )
  
  expect_error({
    result <- icccoeff(
      data = perfect_data,
      vars = c("rater1", "rater2", "rater3")
    )
  }, NA)
  
  # Test with completely random data
  random_data <- data.frame(
    case = 1:50,
    rater1 = runif(50, 0, 100),
    rater2 = runif(50, 0, 100),
    rater3 = runif(50, 0, 100)
  )
  
  expect_error({
    result <- icccoeff(
      data = random_data,
      vars = c("rater1", "rater2", "rater3")
    )
  }, NA)
})
