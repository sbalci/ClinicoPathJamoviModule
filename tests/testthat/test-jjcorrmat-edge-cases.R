# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: jjcorrmat
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

test_that("jjcorrmat handles missing data correctly", {
  devtools::load_all()

  # Test data already has ~3% missing in ki67_index and necrosis_percent
  data(jjcorrmat_test)

  # Listwise deletion
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "necrosis_percent"),
    naHandling = "listwise"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Pairwise deletion
  result2 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "necrosis_percent"),
    naHandling = "pairwise"
  )
  expect_s3_class(result2, "jjcorrmatResults")
})

test_that("jjcorrmat handles data with high proportion of missing values", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Create dataset with 50% missing in one variable
  test_data_missing <- jjcorrmat_test
  n_missing <- round(nrow(test_data_missing) * 0.5)
  test_data_missing$ki67_index[1:n_missing] <- NA

  # Should complete with warning for listwise
  expect_warning(
    jjcorrmat(
      data = test_data_missing,
      dep = c("tumor_size", "ki67_index", "mitotic_count"),
      naHandling = "listwise"
    ),
    regexp = "missing|NA|removed|incomplete",
    ignore.case = TRUE
  )

  # Pairwise should handle better
  result <- jjcorrmat(
    data = test_data_missing,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    naHandling = "pairwise"
  )
  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles small sample sizes", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Small dataset (n=20)
  small_data <- jjcorrmat_test[1:20, ]

  result <- jjcorrmat(
    data = small_data,
    dep = c("tumor_size", "ki67_index", "mitotic_count")
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles minimal sample size (n=10)", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Minimal dataset (n=10)
  minimal_data <- jjcorrmat_test[1:10, ]

  result <- jjcorrmat(
    data = minimal_data,
    dep = c("tumor_size", "ki67_index", "mitotic_count")
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles constant variables", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Create constant variable
  const_data <- jjcorrmat_test
  const_data$constant_var <- 50  # All same value

  # Should error or warn about zero variance
  expect_condition(
    jjcorrmat(
      data = const_data,
      dep = c("tumor_size", "ki67_index", "constant_var")
    )
  )
})

test_that("jjcorrmat handles perfect correlations", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Create perfect correlation
  perfect_data <- jjcorrmat_test
  perfect_data$perfect_copy <- perfect_data$tumor_size

  result <- jjcorrmat(
    data = perfect_data,
    dep = c("tumor_size", "perfect_copy", "ki67_index")
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles near-zero correlations", {
  devtools::load_all()

  data(jjcorrmat_mixed)

  # var_e has near-zero correlations with others
  result <- jjcorrmat(
    data = jjcorrmat_mixed,
    dep = c("var_e", "var_a", "var_b", "var_c")
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles strong negative correlations", {
  devtools::load_all()

  data(jjcorrmat_mixed)

  # var_a and var_g have strong negative correlation (r ≈ -0.60)
  result <- jjcorrmat(
    data = jjcorrmat_mixed,
    dep = c("var_a", "var_g", "var_f")
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles extreme outliers", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Add extreme outliers
  outlier_data <- jjcorrmat_test
  outlier_data$tumor_size[1:3] <- c(500, 600, 700)  # Extreme values

  # Parametric (sensitive to outliers)
  result1 <- jjcorrmat(
    data = outlier_data,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Robust (resistant to outliers)
  result2 <- jjcorrmat(
    data = outlier_data,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    typestatistics = "robust"
  )
  expect_s3_class(result2, "jjcorrmatResults")

  # Nonparametric (rank-based, less affected)
  result3 <- jjcorrmat(
    data = outlier_data,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    typestatistics = "nonparametric"
  )
  expect_s3_class(result3, "jjcorrmatResults")
})

test_that("jjcorrmat handles highly skewed data", {
  devtools::load_all()

  data(jjcorrmat_biomarker)

  # Log-normal biomarker data (right-skewed)

  # Parametric (assumes normality)
  result1 <- jjcorrmat(
    data = jjcorrmat_biomarker,
    dep = c("cea", "ca199", "afp", "ldh"),
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Nonparametric (no distribution assumptions)
  result2 <- jjcorrmat(
    data = jjcorrmat_biomarker,
    dep = c("cea", "ca199", "afp", "ldh"),
    typestatistics = "nonparametric"
  )
  expect_s3_class(result2, "jjcorrmatResults")
})

test_that("jjcorrmat handles negative values", {
  devtools::load_all()

  data(jjcorrmat_mixed)

  # Variables can have negative values
  result <- jjcorrmat(
    data = jjcorrmat_mixed,
    dep = c("var_a", "var_b", "var_c", "var_d")
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles variables with special characters in names", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Create variables with spaces and special characters
  special_data <- jjcorrmat_test
  names(special_data)[names(special_data) == "tumor_size"] <- "tumor size"
  names(special_data)[names(special_data) == "ki67_index"] <- "ki-67 index"

  result <- jjcorrmat(
    data = special_data,
    dep = c("tumor size", "ki-67 index", "mitotic_count")
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles all variables with missing data", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Create dataset where all selected variables have some missing
  all_missing_data <- jjcorrmat_test
  n <- nrow(all_missing_data)
  all_missing_data$tumor_size[sample(n, 5)] <- NA
  all_missing_data$ki67_index[sample(n, 5)] <- NA
  all_missing_data$mitotic_count[sample(n, 5)] <- NA

  # Listwise deletion
  result1 <- jjcorrmat(
    data = all_missing_data,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    naHandling = "listwise"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Pairwise deletion
  result2 <- jjcorrmat(
    data = all_missing_data,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    naHandling = "pairwise"
  )
  expect_s3_class(result2, "jjcorrmatResults")
})

test_that("jjcorrmat handles very wide range of values", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Create wide range (0.01 to 10000)
  wide_range_data <- jjcorrmat_test
  wide_range_data$tumor_size <- wide_range_data$tumor_size * 100
  wide_range_data$ki67_index <- wide_range_data$ki67_index / 10
  wide_range_data$mitotic_count <- wide_range_data$mitotic_count / 100

  result <- jjcorrmat(
    data = wide_range_data,
    dep = c("tumor_size", "ki67_index", "mitotic_count")
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles very small decimal values", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Create very small values (0.0001 to 0.1)
  small_values_data <- jjcorrmat_test
  small_values_data$tumor_size <- small_values_data$tumor_size / 1000
  small_values_data$ki67_index <- small_values_data$ki67_index / 1000
  small_values_data$mitotic_count <- small_values_data$mitotic_count / 1000

  result <- jjcorrmat(
    data = small_values_data,
    dep = c("tumor_size", "ki67_index", "mitotic_count")
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles integer vs numeric variables", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Convert to integer
  integer_data <- jjcorrmat_test
  integer_data$tumor_size <- as.integer(round(integer_data$tumor_size))
  integer_data$ki67_index <- as.integer(round(integer_data$ki67_index))
  integer_data$mitotic_count <- as.integer(integer_data$mitotic_count)

  result <- jjcorrmat(
    data = integer_data,
    dep = c("tumor_size", "ki67_index", "mitotic_count")
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles mixed correlation strengths", {
  devtools::load_all()

  data(jjcorrmat_mixed)

  # Dataset designed with correlations from -0.60 to +0.90
  result <- jjcorrmat(
    data = jjcorrmat_mixed,
    dep = c("var_a", "var_b", "var_c", "var_d", "var_e", "var_f", "var_g"),
    matrixtype = "full"
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles grouped analysis with unbalanced groups", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Create unbalanced grouping variable
  unbalanced_data <- jjcorrmat_test
  unbalanced_data$unbalanced_group <- sample(c("A", "B", "C"),
                                              nrow(unbalanced_data),
                                              replace = TRUE,
                                              prob = c(0.7, 0.2, 0.1))

  result <- jjcorrmat(
    data = unbalanced_data,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    grvar = "unbalanced_group"
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles grouped analysis with small groups", {
  devtools::load_all()

  data(jjcorrmat_imaging)

  # Imaging modality groups may be small
  result <- jjcorrmat(
    data = jjcorrmat_imaging,
    dep = c("tumor_volume", "tumor_longest_diameter", "suv_max"),
    grvar = "imaging_modality"
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles partial correlations with many variables", {
  devtools::load_all()

  data(jjcorrmat_labvalues)

  # Partial correlations controlling for 6 variables
  result <- jjcorrmat(
    data = jjcorrmat_labvalues,
    dep = c("glucose", "cholesterol", "triglycerides", "hdl",
            "ldl", "creatinine"),
    partial = TRUE
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles Inf and -Inf values", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Create Inf values
  inf_data <- jjcorrmat_test
  inf_data$tumor_size[1] <- Inf
  inf_data$ki67_index[2] <- -Inf

  # Should error or handle gracefully
  expect_condition(
    jjcorrmat(
      data = inf_data,
      dep = c("tumor_size", "ki67_index", "mitotic_count")
    )
  )
})

test_that("jjcorrmat handles NaN values", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Create NaN values
  nan_data <- jjcorrmat_test
  nan_data$tumor_size[1:3] <- NaN

  # Should handle like NA
  expect_condition(
    jjcorrmat(
      data = nan_data,
      dep = c("tumor_size", "ki67_index", "mitotic_count")
    )
  )
})

test_that("jjcorrmat handles very strict significance level", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Very strict alpha (0.001)
  result <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count", "age"),
    siglevel = 0.001,
    padjustmethod = "bonferroni"
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles very lenient significance level", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Very lenient alpha (0.20)
  result <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "age", "bmi"),  # Weak correlations
    siglevel = 0.20
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles only two variables (minimum)", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Minimum correlation matrix (2x2)
  result <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index")
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles maximum variables (large matrix)", {
  devtools::load_all()

  data(jjcorrmat_labvalues)

  # 8 variables = 28 pairwise correlations
  result <- jjcorrmat(
    data = jjcorrmat_labvalues,
    dep = c("glucose", "cholesterol", "triglycerides", "hdl",
            "ldl", "creatinine", "alt", "ast"),
    padjustmethod = "bonferroni"
  )

  expect_s3_class(result, "jjcorrmatResults")
})
