# Comprehensive tests for jforestmodel function
devtools::load_all()

# Load test datasets
data(histopathology, package = "ClinicoPath")
data(BreastCancer, package = "ClinicoPath")
data(colon, package = "ClinicoPath")
data(melanoma, package = "ClinicoPath")

test_that("jforestmodel module loads correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  expect_true(exists("jforestmodelClass"))
  expect_true(is.function(jforestmodel))
})

test_that("jforestmodel handles basic input validation", {
  # Test with insufficient variables
  expect_error(
    jforestmodel(data = histopathology, dependent_var = NULL),
    NA  # Should not error during initialization, only during run
  )
  
  # Test with no data
  expect_error(
    jforestmodel(data = NULL, dependent_var = "Grade"),
    NA  # Should not error during initialization
  )
})

test_that("jforestmodel works with linear regression models", {
  # Test basic linear regression with continuous outcome
  result <- jforestmodel(
    data = histopathology,
    dependent_var = "Age",
    predictor_vars = c("Grade", "TStage", "LVI"),
    model_type = "lm",
    show_summary = TRUE,
    show_interpretation = TRUE
  )
  
  expect_s3_class(result, "jforestmodelClass")
  expect_equal(result$options$model_type, "lm")
  expect_true("Age" %in% names(histopathology))
  expect_true("Grade" %in% names(histopathology))
})

test_that("jforestmodel works with logistic regression models", {
  # Test binary outcome logistic regression
  result_binary <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade", "TStage"),
    model_type = "glm",
    family = "binomial",
    exponentiate = TRUE,
    show_summary = TRUE
  )
  
  expect_s3_class(result_binary, "jforestmodelClass")
  expect_equal(result_binary$options$model_type, "glm")
  expect_equal(result_binary$options$family, "binomial")
  expect_true(result_binary$options$exponentiate)
})

test_that("jforestmodel works with BreastCancer dataset", {
  # Test with Wisconsin Breast Cancer dataset
  result_bc <- jforestmodel(
    data = BreastCancer,
    dependent_var = "Class",
    predictor_vars = c("Cl.thickness", "Cell.size", "Cell.shape", "Marg.adhesion"),
    model_type = "glm",
    family = "binomial",
    exponentiate = TRUE,
    confidence_level = 0.95
  )
  
  expect_s3_class(result_bc, "jforestmodelClass")
  expect_true("Class" %in% names(BreastCancer))
  expect_true("Cl.thickness" %in% names(BreastCancer))
})

test_that("jforestmodel works with Cox proportional hazards models", {
  # Test Cox regression with survival data
  result_cox <- jforestmodel(
    data = colon,
    dependent_var = "status",  # Not used directly in Cox, but required by interface
    predictor_vars = c("age", "sex", "obstruct", "perfor"),
    model_type = "coxph",
    time_var = "time",
    event_var = "status",
    exponentiate = TRUE,
    show_summary = TRUE
  )
  
  expect_s3_class(result_cox, "jforestmodelClass")
  expect_equal(result_cox$options$model_type, "coxph")
  expect_true("time" %in% names(colon))
  expect_true("status" %in% names(colon))
})

test_that("jforestmodel works with melanoma survival data", {
  # Test Cox regression with melanoma dataset
  result_melanoma <- jforestmodel(
    data = melanoma,
    dependent_var = "status",
    predictor_vars = c("age", "sex", "thickness", "ulcer"),
    model_type = "coxph",
    time_var = "time",
    event_var = "status",
    exponentiate = TRUE,
    confidence_level = 0.90
  )
  
  expect_s3_class(result_melanoma, "jforestmodelClass")
  expect_equal(result_melanoma$options$confidence_level, 0.90)
  expect_true("thickness" %in% names(melanoma))
})

test_that("jforestmodel handles different GLM families", {
  # Test Poisson regression
  result_poisson <- jforestmodel(
    data = histopathology,
    dependent_var = "Age",  # Treating age as count for testing
    predictor_vars = c("Grade", "TStage"),
    model_type = "glm",
    family = "poisson",
    exponentiate = FALSE
  )
  
  expect_s3_class(result_poisson, "jforestmodelClass")
  expect_equal(result_poisson$options$family, "poisson")
  
  # Test Gaussian GLM
  result_gaussian <- jforestmodel(
    data = histopathology,
    dependent_var = "Age",
    predictor_vars = c("Grade", "TStage"),
    model_type = "glm",
    family = "gaussian",
    exponentiate = FALSE
  )
  
  expect_s3_class(result_gaussian, "jforestmodelClass")
  expect_equal(result_gaussian$options$family, "gaussian")
})

test_that("jforestmodel handles confidence levels", {
  # Test 99% confidence level
  result_99 <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade"),
    model_type = "glm",
    family = "binomial",
    confidence_level = 0.99
  )
  
  expect_s3_class(result_99, "jforestmodelClass")
  expect_equal(result_99$options$confidence_level, 0.99)
  
  # Test 90% confidence level
  result_90 <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade"),
    model_type = "glm",
    family = "binomial",
    confidence_level = 0.90
  )
  
  expect_s3_class(result_90, "jforestmodelClass")
  expect_equal(result_90$options$confidence_level, 0.90)
})

test_that("jforestmodel handles exponentiation options", {
  # Test with exponentiation (odds ratios)
  result_exp <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade", "TStage"),
    model_type = "glm",
    family = "binomial",
    exponentiate = TRUE
  )
  
  expect_s3_class(result_exp, "jforestmodelClass")
  expect_true(result_exp$options$exponentiate)
  
  # Test without exponentiation (log odds)
  result_no_exp <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade", "TStage"),
    model_type = "glm",
    family = "binomial",
    exponentiate = FALSE
  )
  
  expect_s3_class(result_no_exp, "jforestmodelClass")
  expect_false(result_no_exp$options$exponentiate)
})

test_that("jforestmodel handles plot customization options", {
  # Test plot title customization
  result_title <- jforestmodel(
    data = histopathology,
    dependent_var = "Grade",
    predictor_vars = c("Age", "LVI", "PNI"),
    model_type = "lm",
    plot_title = "Custom Forest Plot Title",
    x_axis_label = "Effect Size (Beta Coefficient)"
  )
  
  expect_s3_class(result_title, "jforestmodelClass")
  expect_equal(result_title$options$plot_title, "Custom Forest Plot Title")
  expect_equal(result_title$options$x_axis_label, "Effect Size (Beta Coefficient)")
})

test_that("jforestmodel handles color schemes", {
  # Test blue color scheme
  result_blue <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade"),
    model_type = "glm",
    family = "binomial",
    color_scheme = "blue"
  )
  
  expect_s3_class(result_blue, "jforestmodelClass")
  expect_equal(result_blue$options$color_scheme, "blue")
  
  # Test custom color
  result_custom <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade"),
    model_type = "glm",
    family = "binomial",
    color_scheme = "custom",
    custom_color = "#FF5722"
  )
  
  expect_s3_class(result_custom, "jforestmodelClass")
  expect_equal(result_custom$options$color_scheme, "custom")
  expect_equal(result_custom$options$custom_color, "#FF5722")
})

test_that("jforestmodel handles factor options", {
  # Test factor separate line option
  result_factor <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade", "TStage"),
    model_type = "glm",
    family = "binomial",
    factor_separate_line = TRUE
  )
  
  expect_s3_class(result_factor, "jforestmodelClass")
  expect_true(result_factor$options$factor_separate_line)
  
  # Test factor on same line
  result_same_line <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade", "TStage"),
    model_type = "glm",
    family = "binomial",
    factor_separate_line = FALSE
  )
  
  expect_s3_class(result_same_line, "jforestmodelClass")
  expect_false(result_same_line$options$factor_separate_line)
})

test_that("jforestmodel handles reference lines", {
  # Test with reference line
  result_ref <- jforestmodel(
    data = histopathology,
    dependent_var = "Age",
    predictor_vars = c("Grade", "LVI"),
    model_type = "lm",
    show_reference_line = TRUE,
    reference_value = 0
  )
  
  expect_s3_class(result_ref, "jforestmodelClass")
  expect_true(result_ref$options$show_reference_line)
  expect_equal(result_ref$options$reference_value, 0)
  
  # Test without reference line
  result_no_ref <- jforestmodel(
    data = histopathology,
    dependent_var = "Age",
    predictor_vars = c("Grade", "LVI"),
    model_type = "lm",
    show_reference_line = FALSE
  )
  
  expect_s3_class(result_no_ref, "jforestmodelClass")
  expect_false(result_no_ref$options$show_reference_line)
})

test_that("jforestmodel handles visualization options", {
  # Test point and line sizes
  result_sizes <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade"),
    model_type = "glm",
    family = "binomial",
    point_size = 3.0,
    line_size = 1.0
  )
  
  expect_s3_class(result_sizes, "jforestmodelClass")
  expect_equal(result_sizes$options$point_size, 3.0)
  expect_equal(result_sizes$options$line_size, 1.0)
  
  # Test p-values and confidence intervals display
  result_display <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade"),
    model_type = "glm",
    family = "binomial",
    show_p_values = TRUE,
    show_confidence_intervals = TRUE
  )
  
  expect_s3_class(result_display, "jforestmodelClass")
  expect_true(result_display$options$show_p_values)
  expect_true(result_display$options$show_confidence_intervals)
})

test_that("jforestmodel handles model summary and interpretation", {
  # Test with full reporting
  result_full <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade", "TStage", "PNI"),
    model_type = "glm",
    family = "binomial",
    exponentiate = TRUE,
    show_summary = TRUE,
    show_interpretation = TRUE
  )
  
  expect_s3_class(result_full, "jforestmodelClass")
  expect_true(result_full$options$show_summary)
  expect_true(result_full$options$show_interpretation)
  
  # Test without summary/interpretation
  result_minimal <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade"),
    model_type = "glm",
    family = "binomial",
    show_summary = FALSE,
    show_interpretation = FALSE
  )
  
  expect_s3_class(result_minimal, "jforestmodelClass")
  expect_false(result_minimal$options$show_summary)
  expect_false(result_minimal$options$show_interpretation)
})

test_that("jforestmodel handles sorting options", {
  # Test coefficient sorting
  result_coef_sort <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade", "TStage"),
    model_type = "glm",
    family = "binomial",
    sort_variables = "coefficient"
  )
  
  expect_s3_class(result_coef_sort, "jforestmodelClass")
  expect_equal(result_coef_sort$options$sort_variables, "coefficient")
  
  # Test p-value sorting
  result_pval_sort <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade", "TStage"),
    model_type = "glm",
    family = "binomial",
    sort_variables = "pvalue"
  )
  
  expect_s3_class(result_pval_sort, "jforestmodelClass")
  expect_equal(result_pval_sort$options$sort_variables, "pvalue")
  
  # Test alphabetical sorting
  result_alpha_sort <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade", "TStage"),
    model_type = "glm",
    family = "binomial",
    sort_variables = "alphabetical"
  )
  
  expect_s3_class(result_alpha_sort, "jforestmodelClass")
  expect_equal(result_alpha_sort$options$sort_variables, "alphabetical")
})

test_that("jforestmodel handles panel width ratio", {
  # Test custom panel width ratio
  result_ratio <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = c("Age", "Grade"),
    model_type = "glm",
    family = "binomial",
    panel_width_ratio = "2:3:1"
  )
  
  expect_s3_class(result_ratio, "jforestmodelClass")
  expect_equal(result_ratio$options$panel_width_ratio, "2:3:1")
})

test_that("jforestmodel handles edge cases", {
  # Test with small dataset
  small_data <- histopathology[1:20, ]
  
  expect_error({
    result_small <- jforestmodel(
      data = small_data,
      dependent_var = "LVI",
      predictor_vars = c("Age", "Grade"),
      model_type = "glm",
      family = "binomial"
    )
  }, NA)  # Should not error during initialization
  
  # Test with single predictor
  result_single <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",
    predictor_vars = "Age",
    model_type = "glm",
    family = "binomial"
  )
  
  expect_s3_class(result_single, "jforestmodelClass")
  expect_equal(length(result_single$options$predictor_vars), 1)
})

test_that("jforestmodel validates data types", {
  # Test with valid data types
  result_valid <- jforestmodel(
    data = histopathology,
    dependent_var = "LVI",  # binary
    predictor_vars = c("Age", "Grade", "TStage"),  # mixed types
    model_type = "glm",
    family = "binomial"
  )
  
  expect_s3_class(result_valid, "jforestmodelClass")
})

test_that("jforestmodel comprehensive test with BreastCancer data", {
  # Test comprehensive analysis with all features
  result_comprehensive <- jforestmodel(
    data = BreastCancer,
    dependent_var = "Class",
    predictor_vars = c("Cl.thickness", "Cell.size", "Cell.shape", 
                      "Marg.adhesion", "Epith.c.size", "Bare.nuclei"),
    model_type = "glm",
    family = "binomial",
    exponentiate = TRUE,
    confidence_level = 0.95,
    show_p_values = TRUE,
    show_confidence_intervals = TRUE,
    factor_separate_line = TRUE,
    sort_variables = "coefficient",
    plot_title = "Breast Cancer Prediction Model",
    x_axis_label = "Odds Ratio (95% CI)",
    color_scheme = "blue",
    point_size = 2.5,
    line_size = 0.8,
    show_reference_line = TRUE,
    reference_value = 1,
    show_summary = TRUE,
    show_interpretation = TRUE
  )
  
  expect_s3_class(result_comprehensive, "jforestmodelClass")
  
  # Verify all options are set correctly
  expect_equal(result_comprehensive$options$model_type, "glm")
  expect_equal(result_comprehensive$options$family, "binomial")
  expect_true(result_comprehensive$options$exponentiate)
  expect_equal(result_comprehensive$options$confidence_level, 0.95)
  expect_equal(result_comprehensive$options$sort_variables, "coefficient")
})

test_that("jforestmodel performance with different data sizes", {
  # Test with medium dataset (colon data)
  expect_error({
    result_medium <- jforestmodel(
      data = colon,
      dependent_var = "status",
      predictor_vars = c("age", "sex", "obstruct"),
      model_type = "coxph",
      time_var = "time",
      event_var = "status"
    )
  }, NA)
  
  # Test with large number of predictors
  many_predictors <- c("Age", "Grade", "TStage", "LVI", "PNI")
  
  expect_error({
    result_many <- jforestmodel(
      data = histopathology,
      dependent_var = "Death",
      predictor_vars = many_predictors,
      model_type = "glm",
      family = "binomial"
    )
  }, NA)
})
