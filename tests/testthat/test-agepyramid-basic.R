# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: agepyramid
# ═══════════════════════════════════════════════════════════

library(testthat)

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

test_that("agepyramid handles omitted variables and required Level arguments", {
  # `age` carries default: NULL, so omitting it is a SUPPORTED state: .run()
  # shows the Getting Started panel rather than throwing. expect_error() here
  # asserted the opposite of the intended behaviour.
  res <- agepyramid(
    data = agepyramid_test,
    gender = "gender",
    female = "Female",
    male = "Male"
  )
  expect_s3_class(res, "agepyramidResults")
  expect_true(res$welcome$visible)
  expect_match(as.character(res$welcome$content), "Age Pyramid Analysis")
  # and nothing was tabulated
  expect_equal(nrow(res$pyramidTable$asDF), 0L)

  # Same for gender: default: NULL, so the welcome panel is the intended result.
  res2 <- agepyramid(
    data = agepyramid_test,
    age = "age",
    female = "Female",
    male = "Male"
  )
  expect_true(res2$welcome$visible)
  expect_equal(nrow(res2$pyramidTable$asDF), 0L)

  # `female` and `male` are type: Level, which the compiler forbids a default on,
  # so they ARE required arguments of the wrapper - that is what genuinely errors.
  expect_error(
    agepyramid(data = agepyramid_test, age = "age", gender = "gender", male = "Male"),
    "female",
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
