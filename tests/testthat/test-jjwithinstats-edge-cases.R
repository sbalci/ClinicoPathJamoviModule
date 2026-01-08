# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: jjwithinstats
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

test_that("jjwithinstats handles missing data correctly", {
  devtools::load_all()

  # Test data already has ~3% missing in week4 and week12
  data(jjwithinstats_test)

  result <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12"
  )

  # Should complete successfully (listwise deletion is standard)
  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles data with high proportion of missing values", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Create dataset with 50% missing in one timepoint
  test_data_missing <- jjwithinstats_test
  n_missing <- round(nrow(test_data_missing) * 0.5)
  test_data_missing$week4[1:n_missing] <- NA

  # Should warn but complete
  expect_warning(
    jjwithinstats(
      data = test_data_missing,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12"
    ),
    regexp = "missing|NA|removed|incomplete",
    ignore.case = TRUE
  )
})

test_that("jjwithinstats handles small sample sizes", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Very small dataset (n=10)
  small_data <- jjwithinstats_test[1:10, ]

  result <- jjwithinstats(
    data = small_data,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12"
  )

  # Should complete but may have reduced power
  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles minimal sample size (n=3)", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Minimal dataset (n=3)
  minimal_data <- jjwithinstats_test[1:3, ]

  # May error or warn with very small n
  expect_condition(
    jjwithinstats(
      data = minimal_data,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12"
    )
  )
})

test_that("jjwithinstats handles no variance in a timepoint", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Create constant variable
  const_data <- jjwithinstats_test
  const_data$week12 <- 50  # All same value

  # Should error or warn about zero variance
  expect_condition(
    jjwithinstats(
      data = const_data,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12"
    )
  )
})

test_that("jjwithinstats handles extreme outliers", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Add extreme outliers
  outlier_data <- jjwithinstats_test
  outlier_data$week12[1:3] <- c(1000, 2000, 3000)  # Extreme values

  # Parametric (sensitive to outliers)
  result1 <- jjwithinstats(
    data = outlier_data,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Robust (resistant to outliers)
  result2 <- jjwithinstats(
    data = outlier_data,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    typestatistics = "robust"
  )
  expect_s3_class(result2, "jjwithinstatsResults")
})

test_that("jjwithinstats handles negative values", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Create negative values
  negative_data <- jjwithinstats_test
  negative_data$baseline <- negative_data$baseline - 100
  negative_data$week4 <- negative_data$week4 - 100
  negative_data$week12 <- negative_data$week12 - 100

  result <- jjwithinstats(
    data = negative_data,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles zero values", {
  devtools::load_all()

  data(jjwithinstats_paired)

  # Some post-treatment values are already 0 (pain completely resolved)
  result <- jjwithinstats(
    data = jjwithinstats_paired,
    dep1 = "pre_treatment",
    dep2 = "post_treatment"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles perfect correlation (no change)", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Create perfect correlation (no change between timepoints)
  no_change_data <- jjwithinstats_test
  no_change_data$week4 <- no_change_data$baseline
  no_change_data$week12 <- no_change_data$baseline

  # Should complete but show no significant difference
  result <- jjwithinstats(
    data = no_change_data,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles very large effect size", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Create massive treatment effect
  large_effect_data <- jjwithinstats_test
  large_effect_data$week4 <- large_effect_data$baseline * 0.2
  large_effect_data$week12 <- large_effect_data$baseline * 0.1

  result <- jjwithinstats(
    data = large_effect_data,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles variables with special characters in names", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Create variables with spaces and special characters
  special_data <- jjwithinstats_test
  names(special_data)[names(special_data) == "baseline"] <- "baseline measurement"
  names(special_data)[names(special_data) == "week4"] <- "week 4 value"
  names(special_data)[names(special_data) == "week12"] <- "week-12-value"

  result <- jjwithinstats(
    data = special_data,
    dep1 = "baseline measurement",
    dep2 = "week 4 value",
    dep3 = "week-12-value"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles all NA in optional third timepoint", {
  devtools::load_all()

  data(jjwithinstats_test)

  # All NA in week12
  all_na_data <- jjwithinstats_test
  all_na_data$week12 <- NA_real_

  # Should fall back to two-timepoint analysis or error
  expect_condition(
    jjwithinstats(
      data = all_na_data,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12"
    )
  )
})

test_that("jjwithinstats handles reversed pattern (values increasing)", {
  devtools::load_all()

  data(jjwithinstats_qol)  # QoL increases over time

  # This is a valid pattern (quality of life improving)
  result <- jjwithinstats(
    data = jjwithinstats_qol,
    dep1 = "qol_baseline",
    dep2 = "qol_month1",
    dep3 = "qol_month3"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles non-monotonic pattern (U-shaped)", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Create U-shaped pattern (decrease then increase)
  u_shaped_data <- jjwithinstats_test
  u_shaped_data$week4 <- u_shaped_data$baseline * 0.7  # Decrease
  u_shaped_data$week12 <- u_shaped_data$baseline * 0.9  # Partial recovery

  result <- jjwithinstats(
    data = u_shaped_data,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles bounded data at limits", {
  devtools::load_all()

  data(jjwithinstats_qol)  # Bounded 0-100

  # Some values may already be at boundaries
  result <- jjwithinstats(
    data = jjwithinstats_qol,
    dep1 = "qol_baseline",
    dep2 = "qol_month1",
    dep3 = "qol_month3"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles integer vs numeric variables", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Convert to integer
  integer_data <- jjwithinstats_test
  integer_data$baseline <- as.integer(round(integer_data$baseline))
  integer_data$week4 <- as.integer(round(integer_data$week4))
  integer_data$week12 <- as.integer(round(integer_data$week12))

  result <- jjwithinstats(
    data = integer_data,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles highly skewed data", {
  devtools::load_all()

  data(jjwithinstats_biomarker)  # Log-normal distribution (right-skewed)

  # Parametric (may not be appropriate for skewed data)
  result1 <- jjwithinstats(
    data = jjwithinstats_biomarker,
    dep1 = "month0",
    dep2 = "month1",
    dep3 = "month3",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Nonparametric (appropriate for skewed data)
  result2 <- jjwithinstats(
    data = jjwithinstats_biomarker,
    dep1 = "month0",
    dep2 = "month1",
    dep3 = "month3",
    typestatistics = "nonparametric"
  )
  expect_s3_class(result2, "jjwithinstatsResults")
})

test_that("jjwithinstats handles wide range of values", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Create wide range (0.01 to 10000)
  wide_range_data <- jjwithinstats_test
  wide_range_data$baseline <- wide_range_data$baseline * 100
  wide_range_data$week4 <- wide_range_data$week4 / 10
  wide_range_data$week12 <- wide_range_data$week12 / 100

  result <- jjwithinstats(
    data = wide_range_data,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles very small decimal values", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Create very small values (0.001 to 0.1)
  small_values_data <- jjwithinstats_test
  small_values_data$baseline <- small_values_data$baseline / 1000
  small_values_data$week4 <- small_values_data$week4 / 1000
  small_values_data$week12 <- small_values_data$week12 / 1000

  result <- jjwithinstats(
    data = small_values_data,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles mixed positive and negative changes", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Some subjects improve, some worsen
  mixed_data <- jjwithinstats_test
  # First half improves
  mixed_data$week12[1:40] <- mixed_data$baseline[1:40] * 0.7
  # Second half worsens
  mixed_data$week12[41:80] <- mixed_data$baseline[41:80] * 1.3

  result <- jjwithinstats(
    data = mixed_data,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles Inf and -Inf values", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Create Inf values
  inf_data <- jjwithinstats_test
  inf_data$week12[1] <- Inf
  inf_data$week12[2] <- -Inf

  # Should error or handle gracefully
  expect_condition(
    jjwithinstats(
      data = inf_data,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12"
    )
  )
})

test_that("jjwithinstats handles NaN values", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Create NaN values
  nan_data <- jjwithinstats_test
  nan_data$week12[1:3] <- NaN

  # Should handle like NA
  expect_condition(
    jjwithinstats(
      data = nan_data,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12"
    )
  )
})
