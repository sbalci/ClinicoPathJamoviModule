# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: agepyramid
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

# Load test data
data(agepyramid_test, package = "ClinicoPath")

test_that("agepyramid function exists and runs", {
  # Basic execution test
  result <- agepyramid(
    data = agepyramid_test,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male"
  )

  expect_s3_class(result, "agepyramidResults")
  expect_true(!is.null(result$plot))
})

test_that("agepyramid handles required arguments", {
  # Test with minimal required arguments
  result <- agepyramid(
    data = agepyramid_test,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male"
  )

  expect_no_error(result)
})

test_that("agepyramid errors on missing required arguments", {
  # Missing age variable
  expect_error(
    agepyramid(
      data = agepyramid_test,
      gender = "gender",
      female = "Female",
      male = "Male"
    ),
    regexp = "age.*missing",
    ignore.case = TRUE
  )

  # Missing gender variable
  expect_error(
    agepyramid(
      data = agepyramid_test,
      age = "age",
      female = "Female",
      male = "Male"
    ),
    regexp = "gender.*missing",
    ignore.case = TRUE
  )
})

test_that("agepyramid produces expected outputs", {
  result <- agepyramid(
    data = agepyramid_test,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male"
  )

  # Check that plot exists
  expect_true(!is.null(result$plot))

  # Result should have options
  expect_true(!is.null(result$options))
})

test_that("agepyramid works with complete cases only", {
  # Remove NA values
  complete_data <- na.omit(agepyramid_test)

  result <- agepyramid(
    data = complete_data,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male"
  )

  expect_s3_class(result, "agepyramidResults")
})



test_that("agepyramid works with different age group presets", {
  # Test custom bins
  result1 <- agepyramid(
    data = agepyramid_test,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    age_groups = "custom",
    bin_width = 10
  )
  expect_no_error(result1)

  # Test lifecourse preset
  result2 <- agepyramid(
    data = agepyramid_test,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    age_groups = "lifecourse"
  )
  expect_no_error(result2)
})
