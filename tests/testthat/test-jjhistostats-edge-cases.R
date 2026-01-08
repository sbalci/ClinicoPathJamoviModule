# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: jjhistostats
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

test_that("jjhistostats handles missing data correctly", {
  devtools::load_all()

  data(jjhistostats_test)

  # ~3% missing in psa_level
  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "psa_level"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles data with high proportion of missing values", {
  devtools::load_all()

  data(jjhistostats_test)

  # Create dataset with 40% missing
  test_data_missing <- jjhistostats_test
  n_missing <- round(nrow(test_data_missing) * 0.4)
  test_data_missing$age_years[1:n_missing] <- NA

  result <- jjhistostats(
    data = test_data_missing,
    dep = "age_years"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles small sample sizes", {
  devtools::load_all()

  data(jjhistostats_small)

  # Small dataset (n=25)
  result <- jjhistostats(
    data = jjhistostats_small,
    dep = "measurement"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles very small sample (n=10)", {
  devtools::load_all()

  data(jjhistostats_test)

  # Very small subset
  tiny_data <- jjhistostats_test[1:10, ]

  result <- jjhistostats(
    data = tiny_data,
    dep = "age_years"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles minimal sample (n=5)", {
  devtools::load_all()

  data(jjhistostats_test)

  # Minimal subset
  minimal_data <- jjhistostats_test[1:5, ]

  result <- jjhistostats(
    data = minimal_data,
    dep = "age_years"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles constant variables", {
  devtools::load_all()

  data(jjhistostats_test)

  # Create constant variable
  const_data <- jjhistostats_test
  const_data$constant_var <- 50

  # Should error or warn about zero variance
  expect_condition(
    jjhistostats(
      data = const_data,
      dep = "constant_var"
    )
  )
})

test_that("jjhistostats handles near-constant variables", {
  devtools::load_all()

  data(jjhistostats_test)

  # Create near-constant variable (one outlier)
  near_const_data <- jjhistostats_test
  near_const_data$near_constant <- 50
  near_const_data$near_constant[1] <- 51

  result <- jjhistostats(
    data = near_const_data,
    dep = "near_constant"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles extreme outliers", {
  devtools::load_all()

  data(jjhistostats_test)

  # Add extreme outliers
  outlier_data <- jjhistostats_test
  outlier_data$age_years[1:3] <- c(200, 250, 300)

  # Parametric
  result1 <- jjhistostats(
    data = outlier_data,
    dep = "age_years",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # Robust
  result2 <- jjhistostats(
    data = outlier_data,
    dep = "age_years",
    typestatistics = "robust"
  )
  expect_s3_class(result2, "jjhistostatsResults")
})

test_that("jjhistostats handles highly skewed data", {
  devtools::load_all()

  data(jjhistostats_skewed)

  # Parametric (may be inappropriate)
  result1 <- jjhistostats(
    data = jjhistostats_skewed,
    dep = "ca199",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # Nonparametric (more appropriate)
  result2 <- jjhistostats(
    data = jjhistostats_skewed,
    dep = "ca199",
    typestatistics = "nonparametric"
  )
  expect_s3_class(result2, "jjhistostatsResults")
})

test_that("jjhistostats handles negative values", {
  devtools::load_all()

  data(jjhistostats_test)

  # Create negative values
  negative_data <- jjhistostats_test
  negative_data$age_years <- negative_data$age_years - 70

  result <- jjhistostats(
    data = negative_data,
    dep = "age_years"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles variables with special characters in names", {
  devtools::load_all()

  data(jjhistostats_test)

  # Create variables with spaces and special characters
  special_data <- jjhistostats_test
  names(special_data)[names(special_data) == "age_years"] <- "age (years)"

  result <- jjhistostats(
    data = special_data,
    dep = "age (years)"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles bimodal distribution", {
  devtools::load_all()

  data(jjhistostats_bimodal)

  # Bimodal should still complete
  result <- jjhistostats(
    data = jjhistostats_bimodal,
    dep = "age_bimodal",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles uniform distribution", {
  devtools::load_all()

  data(jjhistostats_uniform)

  # Uniform distribution
  result <- jjhistostats(
    data = jjhistostats_uniform,
    dep = "uniform_score",
    typestatistics = "nonparametric"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles very wide range of values", {
  devtools::load_all()

  data(jjhistostats_test)

  # Create wide range (0.01 to 10000)
  wide_range_data <- jjhistostats_test
  wide_range_data$tumor_size_mm <- wide_range_data$tumor_size_mm * 100

  result <- jjhistostats(
    data = wide_range_data,
    dep = "tumor_size_mm"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles very small decimal values", {
  devtools::load_all()

  data(jjhistostats_test)

  # Create very small values (0.001 to 0.1)
  small_values_data <- jjhistostats_test
  small_values_data$tumor_size_mm <- small_values_data$tumor_size_mm / 1000

  result <- jjhistostats(
    data = small_values_data,
    dep = "tumor_size_mm"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles integer vs numeric variables", {
  devtools::load_all()

  data(jjhistostats_test)

  # Convert to integer
  integer_data <- jjhistostats_test
  integer_data$age_years <- as.integer(round(integer_data$age_years))

  result <- jjhistostats(
    data = integer_data,
    dep = "age_years"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles discrete variables", {
  devtools::load_all()

  data(jjhistostats_pathology)

  # Mitotic count is discrete
  result <- jjhistostats(
    data = jjhistostats_pathology,
    dep = "mitotic_count"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles grouped analysis with small groups", {
  devtools::load_all()

  data(jjhistostats_small)

  # Small dataset with grouping (very small groups)
  result <- jjhistostats(
    data = jjhistostats_small,
    dep = "measurement",
    grvar = "group"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles grouped analysis with unbalanced groups", {
  devtools::load_all()

  data(jjhistostats_test)

  # Create unbalanced grouping variable
  unbalanced_data <- jjhistostats_test
  unbalanced_data$unbalanced_group <- c(
    rep("A", 120),
    rep("B", 20),
    rep("C", 10)
  )

  result <- jjhistostats(
    data = unbalanced_data,
    dep = "age_years",
    grvar = "unbalanced_group"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles grouped analysis with many groups", {
  devtools::load_all()

  data(jjhistostats_test)

  # Create many grouping levels
  many_groups_data <- jjhistostats_test
  many_groups_data$many_levels <- sample(paste0("Level", 1:10),
                                          nrow(many_groups_data),
                                          replace = TRUE)

  result <- jjhistostats(
    data = many_groups_data,
    dep = "age_years",
    grvar = "many_levels"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles Inf and -Inf values", {
  devtools::load_all()

  data(jjhistostats_test)

  # Create Inf values
  inf_data <- jjhistostats_test
  inf_data$age_years[1] <- Inf
  inf_data$age_years[2] <- -Inf

  # Should error or handle gracefully
  expect_condition(
    jjhistostats(
      data = inf_data,
      dep = "age_years"
    )
  )
})

test_that("jjhistostats handles NaN values", {
  devtools::load_all()

  data(jjhistostats_test)

  # Create NaN values
  nan_data <- jjhistostats_test
  nan_data$age_years[1:3] <- NaN

  # Should handle like NA
  expect_condition(
    jjhistostats(
      data = nan_data,
      dep = "age_years"
    )
  )
})

test_that("jjhistostats handles test value at extreme of data range", {
  devtools::load_all()

  data(jjhistostats_labvalues)

  # Test value far above range
  result1 <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "cholesterol",
    enableOneSampleTest = TRUE,
    test.value = 500
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # Test value far below range
  result2 <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "cholesterol",
    enableOneSampleTest = TRUE,
    test.value = 50
  )
  expect_s3_class(result2, "jjhistostatsResults")
})

test_that("jjhistostats handles test value equal to mean", {
  devtools::load_all()

  data(jjhistostats_labvalues)

  # Test value at mean
  mean_value <- mean(jjhistostats_labvalues$cholesterol, na.rm = TRUE)

  result <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "cholesterol",
    enableOneSampleTest = TRUE,
    test.value = mean_value
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles zero test value", {
  devtools::load_all()

  data(jjhistostats_test)

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    enableOneSampleTest = TRUE,
    test.value = 0
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles negative test value", {
  devtools::load_all()

  data(jjhistostats_test)

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    enableOneSampleTest = TRUE,
    test.value = -10
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles very small bin width", {
  devtools::load_all()

  data(jjhistostats_test)

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    changebinwidth = TRUE,
    binwidth = 0.1
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles very large bin width", {
  devtools::load_all()

  data(jjhistostats_test)

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    changebinwidth = TRUE,
    binwidth = 100
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles data at boundaries", {
  devtools::load_all()

  data(jjhistostats_test)

  # Force some values to exact boundaries
  boundary_data <- jjhistostats_test
  boundary_data$bmi[1:10] <- 15   # Lower boundary
  boundary_data$bmi[11:20] <- 45  # Upper boundary

  result <- jjhistostats(
    data = boundary_data,
    dep = "bmi"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles all values identical in one group", {
  devtools::load_all()

  data(jjhistostats_grouped)

  # Make one disease stage have identical values
  identical_group_data <- jjhistostats_grouped
  identical_group_data$biomarker_level[
    identical_group_data$disease_stage == "Early"
  ] <- 20

  result <- jjhistostats(
    data = identical_group_data,
    dep = "biomarker_level",
    grvar = "disease_stage"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles complete separation between groups", {
  devtools::load_all()

  data(jjhistostats_grouped)

  # Create perfect separation (no overlap)
  perfect_sep <- jjhistostats_grouped
  perfect_sep$biomarker_level[perfect_sep$disease_stage == "Early"] <-
    runif(sum(perfect_sep$disease_stage == "Early"), 0, 20)
  perfect_sep$biomarker_level[perfect_sep$disease_stage == "Intermediate"] <-
    runif(sum(perfect_sep$disease_stage == "Intermediate"), 40, 60)
  perfect_sep$biomarker_level[perfect_sep$disease_stage == "Advanced"] <-
    runif(sum(perfect_sep$disease_stage == "Advanced"), 80, 100)

  result <- jjhistostats(
    data = perfect_sep,
    dep = "biomarker_level",
    grvar = "disease_stage"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles complete overlap between groups", {
  devtools::load_all()

  data(jjhistostats_grouped)

  # Create complete overlap (identical distributions)
  complete_overlap <- jjhistostats_grouped
  complete_overlap$biomarker_level <- rnorm(nrow(complete_overlap), 50, 10)

  result <- jjhistostats(
    data = complete_overlap,
    dep = "biomarker_level",
    grvar = "disease_stage"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles extreme confidence levels", {
  devtools::load_all()

  data(jjhistostats_test)

  # Very low confidence
  result1 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    conf.level = 0.50
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # Very high confidence
  result2 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    conf.level = 0.999
  )
  expect_s3_class(result2, "jjhistostatsResults")
})

test_that("jjhistostats handles high transparency (near invisible)", {
  devtools::load_all()

  data(jjhistostats_test)

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    binalpha = 0.1
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles zero transparency (opaque)", {
  devtools::load_all()

  data(jjhistostats_test)

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    binalpha = 1.0
  )

  expect_s3_class(result, "jjhistostatsResults")
})
