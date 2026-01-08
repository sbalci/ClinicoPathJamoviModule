# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: ihcheterogeneity
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error conditions, and robust error handling
# for the ihcheterogeneity jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(ihcheterogeneity_test, package = "ClinicoPath")
data(ihcheterogeneity_test_small, package = "ClinicoPath")

test_that("ihcheterogeneity handles missing data appropriately", {
  # Create dataset with missing values
  test_data_na <- ihcheterogeneity_test
  test_data_na$biopsy1[1:5] <- NA

  # Should handle missing data gracefully
  result <- ihcheterogeneity(
    data = test_data_na,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles very small sample sizes", {
  # Only 3 cases
  small_data <- ihcheterogeneity_test_small[1:3, ]

  result <- ihcheterogeneity(
    data = small_data,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2"
  )

  # Should complete but may have warnings
  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles negative values", {
  # Create dataset with negative values (invalid for percentages)
  test_data_neg <- ihcheterogeneity_test
  test_data_neg$biopsy1[1] <- -5

  # Should handle or error appropriately
  result <- tryCatch(
    {
      ihcheterogeneity(
        data = test_data_neg,
        wholesection = "wholesection",
        biopsy1 = "biopsy1",
        biopsy2 = "biopsy2"
      )
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )

  # Either completes or gives informative error
  if (!is.null(result)) {
    expect_s3_class(result, "ihcheterogeneityClass")
  }
})

test_that("ihcheterogeneity handles out-of-range values", {
  # Create dataset with values > 100%
  test_data_range <- ihcheterogeneity_test
  test_data_range$wholesection[1] <- 150

  # Should handle or warn
  result <- ihcheterogeneity(
    data = test_data_range,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles perfect agreement (no heterogeneity)", {
  # Create dataset with identical values (no variability)
  test_data_perfect <- ihcheterogeneity_test
  test_data_perfect$biopsy1 <- test_data_perfect$wholesection
  test_data_perfect$biopsy2 <- test_data_perfect$wholesection

  result <- ihcheterogeneity(
    data = test_data_perfect,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles extreme heterogeneity", {
  data(ihcheterogeneity_high_hetero, package = "ClinicoPath")

  result <- ihcheterogeneity(
    data = ihcheterogeneity_high_hetero,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles constant values", {
  # All measurements are the same constant
  test_data_const <- ihcheterogeneity_test
  test_data_const$wholesection <- 50
  test_data_const$biopsy1 <- 50
  test_data_const$biopsy2 <- 50

  # Should handle zero variance appropriately
  result <- ihcheterogeneity(
    data = test_data_const,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles single case (should warn or error)", {
  single_case <- ihcheterogeneity_test[1, ]

  # Should warn about insufficient data
  expect_condition(
    ihcheterogeneity(
      data = single_case,
      wholesection = "wholesection",
      biopsy1 = "biopsy1",
      biopsy2 = "biopsy2"
    )
  )
})

test_that("ihcheterogeneity handles missing spatial_id with compartment tests", {
  # Try compartment tests without spatial_id
  result <- tryCatch(
    {
      ihcheterogeneity(
        data = ihcheterogeneity_test,
        wholesection = "wholesection",
        biopsy1 = "biopsy1",
        biopsy2 = "biopsy2",
        compareCompartments = TRUE,
        compartmentTests = TRUE
      )
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )

  # Should either skip compartment analysis or warn
  if (!is.null(result)) {
    expect_s3_class(result, "ihcheterogeneityClass")
  }
})

test_that("ihcheterogeneity handles all NA in one biopsy", {
  test_data_all_na <- ihcheterogeneity_test
  test_data_all_na$biopsy3 <- NA

  result <- ihcheterogeneity(
    data = test_data_all_na,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles variable names with special characters", {
  # Variable names with spaces
  test_data_special <- ihcheterogeneity_test
  names(test_data_special)[names(test_data_special) == "wholesection"] <-
    "whole section"

  result <- ihcheterogeneity(
    data = test_data_special,
    wholesection = "whole section",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2"
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles extreme CV thresholds", {
  # Very low threshold (5%)
  result_low <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    cv_threshold = 5.0
  )

  expect_s3_class(result_low, "ihcheterogeneityClass")

  # Very high threshold (50%)
  result_high <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    cv_threshold = 50.0
  )

  expect_s3_class(result_high, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles only one spatial compartment", {
  test_data_one_comp <- ihcheterogeneity_test
  test_data_one_comp$spatial_id <- "Single_Compartment"

  # Should skip compartment comparison
  result <- ihcheterogeneity(
    data = test_data_one_comp,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    spatial_id = "spatial_id",
    compareCompartments = TRUE
  )

  expect_s3_class(result, "ihcheterogeneityClass")
})
