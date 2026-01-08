# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: jjdotplotstats
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

test_that("jjdotplotstats handles missing data correctly", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Create data with missing values
  test_data_na <- jjdotplotstats_test
  test_data_na$tumor_reduction[1:5] <- NA

  # Should handle missing data (warn or complete)
  result <- jjdotplotstats(
    data = test_data_na,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles data with high proportion of missing values", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Create dataset with 30% missing
  test_data_missing <- jjdotplotstats_test
  n_missing <- round(nrow(test_data_missing) * 0.3)
  test_data_missing$tumor_reduction[1:n_missing] <- NA

  # Should complete (may warn)
  result <- jjdotplotstats(
    data = test_data_missing,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles small sample sizes", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Small dataset (n=20, ~7 per group)
  small_data <- jjdotplotstats_test[1:20, ]

  result <- jjdotplotstats(
    data = small_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles minimal sample size per group (n=5)", {
  devtools::load_all()

  data(jjdotplotstats_twogroup)

  # Minimal dataset (10 total, 5 per group)
  minimal_data <- jjdotplotstats_twogroup[c(1:5, 41:45), ]

  result <- jjdotplotstats(
    data = minimal_data,
    dep = "pain_score",
    group = "timepoint"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles very small groups (n=3)", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Very small (9 total, 3 per group)
  tiny_data <- jjdotplotstats_test[c(1:3, 41:43, 81:83), ]

  result <- jjdotplotstats(
    data = tiny_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles constant variables", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Create constant variable
  const_data <- jjdotplotstats_test
  const_data$constant_var <- 50  # All same value

  # Should error or warn about zero variance
  expect_condition(
    jjdotplotstats(
      data = const_data,
      dep = "constant_var",
      group = "treatment"
    )
  )
})

test_that("jjdotplotstats handles near-constant variables", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Create near-constant variable (one outlier)
  near_const_data <- jjdotplotstats_test
  near_const_data$near_constant <- 50
  near_const_data$near_constant[1] <- 51  # Tiny variance

  result <- jjdotplotstats(
    data = near_const_data,
    dep = "near_constant",
    group = "treatment"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles extreme outliers", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Add extreme outliers
  outlier_data <- jjdotplotstats_test
  outlier_data$tumor_reduction[1:3] <- c(500, 600, 700)  # Extreme values

  # Parametric (sensitive)
  result1 <- jjdotplotstats(
    data = outlier_data,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # Robust (resistant)
  result2 <- jjdotplotstats(
    data = outlier_data,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "robust"
  )
  expect_s3_class(result2, "jjdotplotstatsResults")

  # Nonparametric (rank-based)
  result3 <- jjdotplotstats(
    data = outlier_data,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "nonparametric"
  )
  expect_s3_class(result3, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles highly skewed data", {
  devtools::load_all()

  data(jjdotplotstats_skewed)

  # Parametric (assumes normality)
  result1 <- jjdotplotstats(
    data = jjdotplotstats_skewed,
    dep = "biomarker_level",
    group = "treatment",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # Nonparametric (no distribution assumptions)
  result2 <- jjdotplotstats(
    data = jjdotplotstats_skewed,
    dep = "biomarker_level",
    group = "treatment",
    typestatistics = "nonparametric"
  )
  expect_s3_class(result2, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles negative values", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Create negative values
  negative_data <- jjdotplotstats_test
  negative_data$tumor_reduction <- negative_data$tumor_reduction - 50

  result <- jjdotplotstats(
    data = negative_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles variables with special characters in names", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Create variables with spaces and special characters
  special_data <- jjdotplotstats_test
  names(special_data)[names(special_data) == "tumor_reduction"] <- "tumor reduction"
  names(special_data)[names(special_data) == "treatment"] <- "treatment group"

  result <- jjdotplotstats(
    data = special_data,
    dep = "tumor reduction",
    group = "treatment group"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles unbalanced groups", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Create highly unbalanced groups (90:20:10)
  unbalanced_data <- jjdotplotstats_test[c(1:90, 41:60, 81:90), ]

  result <- jjdotplotstats(
    data = unbalanced_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles very unbalanced groups (extreme)", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Extreme imbalance (100:10:3)
  extreme_unbalanced <- jjdotplotstats_test[c(1:100, 41:50, 81:83), ]

  result <- jjdotplotstats(
    data = extreme_unbalanced,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles all missing in one group", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # All missing in one group
  one_group_missing <- jjdotplotstats_test
  one_group_missing$tumor_reduction[1:40] <- NA  # All Control group

  # Should error or handle gracefully
  expect_condition(
    jjdotplotstats(
      data = one_group_missing,
      dep = "tumor_reduction",
      group = "treatment"
    )
  )
})

test_that("jjdotplotstats handles very wide range of values", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Create wide range (0.01 to 10000)
  wide_range_data <- jjdotplotstats_test
  wide_range_data$tumor_reduction <- wide_range_data$tumor_reduction * 100

  result <- jjdotplotstats(
    data = wide_range_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles very small decimal values", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Create very small values (0.0001 to 0.1)
  small_values_data <- jjdotplotstats_test
  small_values_data$tumor_reduction <- small_values_data$tumor_reduction / 1000

  result <- jjdotplotstats(
    data = small_values_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles integer vs numeric variables", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Convert to integer
  integer_data <- jjdotplotstats_test
  integer_data$tumor_reduction <- as.integer(round(integer_data$tumor_reduction))

  result <- jjdotplotstats(
    data = integer_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles grouped analysis with many groups", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Create many grouping levels
  many_groups_data <- jjdotplotstats_test
  many_groups_data$many_levels <- sample(paste0("Level", 1:10),
                                          nrow(many_groups_data),
                                          replace = TRUE)

  result <- jjdotplotstats(
    data = many_groups_data,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = "many_levels"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles grouped analysis with small groups", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Small dataset with grouping
  small_grouped <- jjdotplotstats_test[1:30, ]

  result <- jjdotplotstats(
    data = small_grouped,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = "hospital"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles Inf and -Inf values", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Create Inf values
  inf_data <- jjdotplotstats_test
  inf_data$tumor_reduction[1] <- Inf
  inf_data$tumor_reduction[2] <- -Inf

  # Should error or handle gracefully
  expect_condition(
    jjdotplotstats(
      data = inf_data,
      dep = "tumor_reduction",
      group = "treatment"
    )
  )
})

test_that("jjdotplotstats handles NaN values", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Create NaN values
  nan_data <- jjdotplotstats_test
  nan_data$tumor_reduction[1:3] <- NaN

  # Should handle like NA
  expect_condition(
    jjdotplotstats(
      data = nan_data,
      dep = "tumor_reduction",
      group = "treatment"
    )
  )
})

test_that("jjdotplotstats handles test value at extreme of data range", {
  devtools::load_all()

  data(jjdotplotstats_reference)

  # Test value far above data range
  result1 <- jjdotplotstats(
    data = jjdotplotstats_reference,
    dep = "bp_reduction",
    group = "drug",
    testvalue = 100,
    testvalueline = TRUE
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # Test value far below data range
  result2 <- jjdotplotstats(
    data = jjdotplotstats_reference,
    dep = "bp_reduction",
    group = "drug",
    testvalue = -50,
    testvalueline = TRUE
  )
  expect_s3_class(result2, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles test value equal to all data points", {
  devtools::load_all()

  data(jjdotplotstats_reference)

  # Test value at mean of all data
  mean_value <- mean(jjdotplotstats_reference$bp_reduction, na.rm = TRUE)

  result <- jjdotplotstats(
    data = jjdotplotstats_reference,
    dep = "bp_reduction",
    group = "drug",
    testvalue = mean_value,
    testvalueline = TRUE
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles zero test value", {
  devtools::load_all()

  data(jjdotplotstats_test)

  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    testvalue = 0,
    testvalueline = TRUE
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles negative test value", {
  devtools::load_all()

  data(jjdotplotstats_test)

  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    testvalue = -10,
    testvalueline = TRUE
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles single observation per group", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # One observation per group (3 total)
  single_obs <- jjdotplotstats_test[c(1, 41, 81), ]

  # Should error (cannot compute statistics)
  expect_condition(
    jjdotplotstats(
      data = single_obs,
      dep = "tumor_reduction",
      group = "treatment"
    )
  )
})

test_that("jjdotplotstats handles perfect separation between groups", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Create perfect separation (no overlap)
  perfect_sep <- jjdotplotstats_test
  perfect_sep$tumor_reduction[1:40] <- runif(40, 0, 10)      # Control: 0-10
  perfect_sep$tumor_reduction[41:80] <- runif(40, 50, 60)    # Treatment A: 50-60
  perfect_sep$tumor_reduction[81:120] <- runif(40, 90, 100)  # Treatment B: 90-100

  result <- jjdotplotstats(
    data = perfect_sep,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles complete overlap between groups", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Create complete overlap (identical distributions)
  complete_overlap <- jjdotplotstats_test
  complete_overlap$tumor_reduction <- rnorm(nrow(complete_overlap), 50, 10)

  result <- jjdotplotstats(
    data = complete_overlap,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles bounded data (0-100)", {
  devtools::load_all()

  data(jjdotplotstats_qol)

  # Quality of life is bounded 0-100
  result <- jjdotplotstats(
    data = jjdotplotstats_qol,
    dep = "qol_score",
    group = "intervention",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles data at boundaries", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Force some values to exact boundaries
  boundary_data <- jjdotplotstats_test
  boundary_data$tumor_reduction[1:10] <- 0    # Lower boundary
  boundary_data$tumor_reduction[11:20] <- 100  # Upper boundary

  result <- jjdotplotstats(
    data = boundary_data,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})
