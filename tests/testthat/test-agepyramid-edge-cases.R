# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: agepyramid
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

test_that("agepyramid handles missing data correctly", {
  # Create data with missing age values
  test_data_na_age <- agepyramid_test
  test_data_na_age$age[1:10] <- NA

  # Should handle missing data (may warn or error)
  expect_condition(
    agepyramid(
      data = test_data_na_age,
      age = "age",
      gender = "gender",
      female = "Female",
      male = "Male"
    )
  )

  # Create data with missing gender values
  test_data_na_gender <- agepyramid_test
  test_data_na_gender$gender[1:10] <- NA

  # Should handle missing data
  expect_condition(
    agepyramid(
      data = test_data_na_gender,
      age = "age",
      gender = "gender",
      female = "Female",
      male = "Male"
    )
  )
})

test_that("agepyramid handles small sample sizes", {
  # Very small dataset (n=10)
  small_data <- agepyramid_test[1:10, ]

  result <- agepyramid(
    data = small_data,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male"
  )

  # Should complete (may produce a simple plot)
  expect_s3_class(result, "agepyramidClass")
})

test_that("agepyramid handles extremely small sample size", {
  # Minimal dataset (n=2)
  minimal_data <- data.frame(
    age = c(25, 30),
    gender = c("Female", "Male")
  )

  result <- agepyramid(
    data = minimal_data,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male"
  )

  # Should handle minimal data
  expect_s3_class(result, "agepyramidClass")
})

test_that("agepyramid handles single gender data", {
  # Only females
  female_only <- data.frame(
    age = runif(50, 20, 80),
    gender = rep("Female", 50)
  )

  result1 <- agepyramid(
    data = female_only,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male"
  )

  # Should complete (one side of pyramid will be empty)
  expect_s3_class(result1, "agepyramidClass")

  # Only males
  male_only <- data.frame(
    age = runif(50, 20, 80),
    gender = rep("Male", 50)
  )

  result2 <- agepyramid(
    data = male_only,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male"
  )

  expect_s3_class(result2, "agepyramidClass")
})

test_that("agepyramid handles unbalanced gender distribution", {
  # Load unbalanced dataset (70% female)
  data(agepyramid_unbalanced, package = "ClinicoPath")

  result <- agepyramid(
    data = agepyramid_unbalanced,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    plot_title = "Unbalanced Gender Distribution"
  )

  expect_s3_class(result, "agepyramidClass")
})

test_that("agepyramid handles extreme age values", {
  # Very young ages (neonates)
  young_data <- data.frame(
    age = runif(50, 0, 1),
    gender = sample(c("Female", "Male"), 50, replace = TRUE)
  )

  result1 <- agepyramid(
    data = young_data,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    bin_width = 0.25
  )
  expect_no_error(result1)

  # Very old ages (centenarians)
  old_data <- data.frame(
    age = runif(50, 95, 110),
    gender = sample(c("Female", "Male"), 50, replace = TRUE)
  )

  result2 <- agepyramid(
    data = old_data,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male"
  )
  expect_no_error(result2)
})

test_that("agepyramid handles narrow age range", {
  # All ages within a narrow range (e.g., college students)
  narrow_data <- data.frame(
    age = rnorm(100, mean = 20, sd = 2),
    gender = sample(c("Female", "Male"), 100, replace = TRUE)
  )

  result <- agepyramid(
    data = narrow_data,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    bin_width = 1
  )

  expect_s3_class(result, "agepyramidClass")
})

test_that("agepyramid handles constant age", {
  # All same age (should create a single bin)
  constant_age <- data.frame(
    age = rep(50, 100),
    gender = sample(c("Female", "Male"), 100, replace = TRUE)
  )

  result <- agepyramid(
    data = constant_age,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male"
  )

  # Should handle (creates pyramid with single age bin)
  expect_s3_class(result, "agepyramidClass")
})

test_that("agepyramid handles non-standard gender labels", {
  # Different gender labels
  custom_labels <- data.frame(
    age = runif(100, 0, 100),
    gender = sample(c("F", "M"), 100, replace = TRUE)
  )

  result <- agepyramid(
    data = custom_labels,
    age = "age",
    gender = "gender",
    female = "F",
    male = "M"
  )

  expect_s3_class(result, "agepyramidClass")
})

test_that("agepyramid handles negative ages gracefully", {
  # Invalid data with negative ages
  negative_age <- data.frame(
    age = c(-5, 10, 20, 30, 40),
    gender = sample(c("Female", "Male"), 5, replace = TRUE)
  )

  # Should either error or handle gracefully
  expect_condition(
    agepyramid(
      data = negative_age,
      age = "age",
      gender = "gender",
      female = "Female",
      male = "Male"
    )
  )
})

test_that("agepyramid handles very large bin widths", {
  # Bin width larger than age range
  result <- agepyramid(
    data = agepyramid_test,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    bin_width = 200
  )

  # Should create a single bin
  expect_s3_class(result, "agepyramidClass")
})

test_that("agepyramid handles very small bin widths", {
  # Very small bin width
  result <- agepyramid(
    data = agepyramid_test,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    bin_width = 0.5
  )

  # Should create many bins
  expect_s3_class(result, "agepyramidClass")
})

test_that("agepyramid handles wrong gender level specification", {
  # Swapped female/male levels
  result <- agepyramid(
    data = agepyramid_test,
    age = "age",
    gender = "gender",
    female = "Male",  # Intentionally swapped
    male = "Female"   # Intentionally swapped
  )

  # Should still run (but pyramid will be backwards)
  expect_s3_class(result, "agepyramidClass")
})

test_that("agepyramid handles data with outliers", {
  # Add extreme outliers
  outlier_data <- agepyramid_test
  outlier_data$age[1:3] <- c(0.01, 120, 150)

  result <- agepyramid(
    data = outlier_data,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male"
  )

  # Should handle outliers (may warn or adjust bins)
  expect_s3_class(result, "agepyramidClass")
})

test_that("agepyramid handles special characters in variable names", {
  # Variable names with spaces (should work with backticks)
  special_data <- agepyramid_test
  names(special_data)[names(special_data) == "age"] <- "age in years"
  names(special_data)[names(special_data) == "gender"] <- "patient gender"

  result <- agepyramid(
    data = special_data,
    age = "age in years",
    gender = "patient gender",
    female = "Female",
    male = "Male"
  )

  expect_no_error(result)
})
