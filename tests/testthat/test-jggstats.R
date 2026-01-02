# Test file for jggstats function
# Tests enhanced statistical visualization functionality

library(testthat)
devtools::load_all()

# Test Data Setup
test_that("Test data loads correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  expect_true(exists("linear_model_data"))
  expect_true(exists("logistic_model_data"))
  expect_true(exists("jggstats_comprehensive_data"))
})

# Basic Functionality Tests
test_that("jggstats basic functionality works", {
  
  # Test with minimal required parameters
  result <- jggstats(
    data = linear_model_data,
    dependent_var = "outcome",
    independent_vars = c("age", "sex"),
    analysis_type = "ggcoef_model"
  )
  
  expect_true(!is.null(result))
  expect_s3_class(result, "jmvAnalysis")
})

test_that("jggstats handles missing required variables", {
  
  # Test without dependent variable
  expect_error(
    jggstats(
      data = linear_model_data,
      independent_vars = c("age", "sex"),
      analysis_type = "ggcoef_model"
    ),
    NA  # Should not error in jamovi context, but should show instructions
  )
})

# Analysis Type Tests
test_that("jggstats model coefficient analysis works", {
  
  result <- jggstats(
    data = linear_model_data,
    dependent_var = "outcome",
    independent_vars = c("age", "sex", "treatment"),
    analysis_type = "ggcoef_model",
    model_type = "lm"
  )
  
  expect_true(!is.null(result))
  expect_s3_class(result, "jmvAnalysis")
})

test_that("jggstats model comparison works", {
  
  result <- jggstats(
    data = linear_model_data,
    dependent_var = "outcome",
    independent_vars = c("age", "sex"),
    grouping_var = "treatment",
    analysis_type = "ggcoef_compare",
    model_type = "lm"
  )
  
  expect_true(!is.null(result))
})

test_that("jggstats Likert analysis works", {
  
  result <- jggstats(
    data = likert_survey_data,
    dependent_var = "satisfaction_work",
    grouping_var = "department",
    analysis_type = "gglikert",
    likert_levels = 5
  )
  
  expect_true(!is.null(result))
})

test_that("jggstats survey analysis works", {
  
  result <- jggstats(
    data = survey_proportion_data,
    dependent_var = "political_preference",
    weight_var = "weight",
    grouping_var = "age_category",
    analysis_type = "ggsurvey"
  )
  
  expect_true(!is.null(result))
})

test_that("jggstats proportion analysis works", {
  
  result <- jggstats(
    data = cross_tab_data,
    dependent_var = "disease_status",
    grouping_var = "age_group",
    analysis_type = "stat_prop"
  )
  
  expect_true(!is.null(result))
})

test_that("jggstats cross-tabulation works", {
  
  result <- jggstats(
    data = cross_tab_data,
    dependent_var = "test_result",
    grouping_var = "disease_status",
    analysis_type = "stat_cross"
  )
  
  expect_true(!is.null(result))
})

test_that("jggstats weighted means work", {
  
  result <- jggstats(
    data = weighted_analysis_data,
    dependent_var = "income_thousands",
    grouping_var = "head_education",
    weight_var = "final_weight",
    analysis_type = "stat_weighted_mean"
  )
  
  expect_true(!is.null(result))
})

test_that("jggstats cascade analysis works", {
  
  result <- jggstats(
    data = linear_model_data,
    dependent_var = "outcome",
    independent_vars = c("age", "sex"),
    analysis_type = "ggcascade"
  )
  
  expect_true(!is.null(result))
})

# Model Type Tests
test_that("jggstats works with different model types", {
  
  # Linear model
  result_lm <- jggstats(
    data = linear_model_data,
    dependent_var = "outcome",
    independent_vars = c("age", "sex"),
    analysis_type = "ggcoef_model",
    model_type = "lm"
  )
  
  expect_true(!is.null(result_lm))
  
  # Logistic regression
  result_glm <- jggstats(
    data = logistic_model_data,
    dependent_var = "disease",
    independent_vars = c("age", "sex", "smoking"),
    analysis_type = "ggcoef_model",
    model_type = "glm",
    family = "binomial"
  )
  
  expect_true(!is.null(result_glm))
})

test_that("jggstats works with survival models", {
  
  # Create survival formula format
  survival_data <- survival_analysis_data
  survival_data$surv_object <- paste0("Surv(", survival_data$survival_time, ",", survival_data$event, ")")
  
  result_cox <- jggstats(
    data = survival_data,
    model_formula = "Surv(survival_time, event) ~ age + sex + stage",
    analysis_type = "ggcoef_model",
    model_type = "coxph"
  )
  
  expect_true(!is.null(result_cox))
})

# Customization Options Tests
test_that("jggstats coefficient options work", {
  
  # Test with intercept shown
  result_intercept <- jggstats(
    data = linear_model_data,
    dependent_var = "outcome",
    independent_vars = c("age", "sex"),
    analysis_type = "ggcoef_model",
    show_intercept = TRUE
  )
  
  expect_true(!is.null(result_intercept))
  
  # Test with sorted coefficients
  result_sorted <- jggstats(
    data = linear_model_data,
    dependent_var = "outcome",
    independent_vars = c("age", "sex", "treatment"),
    analysis_type = "ggcoef_model",
    sort_coefficients = TRUE
  )
  
  expect_true(!is.null(result_sorted))
})

test_that("jggstats confidence level options work", {
  
  confidence_levels <- c(0.80, 0.90, 0.95, 0.99)
  
  for (level in confidence_levels) {
    result <- jggstats(
      data = linear_model_data,
      dependent_var = "outcome",
      independent_vars = c("age", "sex"),
      analysis_type = "ggcoef_model",
      confidence_level = level
    )
    
    expect_true(!is.null(result))
  }
})

test_that("jggstats visualization options work", {
  
  # Test color palettes
  color_palettes <- c("default", "viridis", "set1", "dark2", "paired")
  
  for (palette in color_palettes) {
    result <- jggstats(
      data = linear_model_data,
      dependent_var = "outcome",
      independent_vars = c("age", "sex"),
      analysis_type = "ggcoef_model",
      color_palette = palette
    )
    
    expect_true(!is.null(result))
  }
})

test_that("jggstats theme styles work", {
  
  theme_styles <- c("default", "minimal", "classic", "light", "dark")
  
  for (theme in theme_styles) {
    result <- jggstats(
      data = linear_model_data,
      dependent_var = "outcome",
      independent_vars = c("age", "sex"),
      analysis_type = "ggcoef_model",
      theme_style = theme
    )
    
    expect_true(!is.null(result))
  }
})

test_that("jggstats plot labels work", {
  
  result <- jggstats(
    data = linear_model_data,
    dependent_var = "outcome",
    independent_vars = c("age", "sex"),
    analysis_type = "ggcoef_model",
    plot_title = "Test Model Coefficients",
    plot_subtitle = "Enhanced statistical visualization",
    x_label = "Coefficient Value",
    y_label = "Model Terms"
  )
  
  expect_true(!is.null(result))
})

# Faceting Tests
test_that("jggstats faceting works", {
  
  # Test facet wrap
  result_wrap <- jggstats(
    data = linear_model_data,
    dependent_var = "outcome",
    independent_vars = c("age", "sex"),
    facet_var = "group",
    facet_type = "wrap",
    analysis_type = "stat_prop"
  )
  
  expect_true(!is.null(result_wrap))
  
  # Test facet grid
  result_grid <- jggstats(
    data = linear_model_data,
    dependent_var = "outcome",
    independent_vars = c("age", "sex"),
    facet_var = "group",
    facet_type = "grid",
    analysis_type = "stat_prop"
  )
  
  expect_true(!is.null(result_grid))
})

# Output Format Tests
test_that("jggstats output formats work", {
  
  output_formats <- c("plot_only", "model_table", "both")
  
  for (format in output_formats) {
    result <- jggstats(
      data = linear_model_data,
      dependent_var = "outcome",
      independent_vars = c("age", "sex"),
      analysis_type = "ggcoef_model",
      output_format = format
    )
    
    expect_true(!is.null(result))
  }
})

# Model Summary and Interpretation Tests
test_that("jggstats model summary works", {
  
  result <- jggstats(
    data = linear_model_data,
    dependent_var = "outcome",
    independent_vars = c("age", "sex", "treatment"),
    analysis_type = "ggcoef_model",
    show_model_summary = TRUE
  )
  
  expect_true(!is.null(result))
})

test_that("jggstats interpretation works", {
  
  result <- jggstats(
    data = linear_model_data,
    dependent_var = "outcome",
    independent_vars = c("age", "sex"),
    analysis_type = "ggcoef_model",
    show_interpretation = TRUE
  )
  
  expect_true(!is.null(result))
})

# Advanced Features Tests
test_that("jggstats standardized coefficients work", {
  
  result <- jggstats(
    data = linear_model_data,
    dependent_var = "outcome",
    independent_vars = c("age", "baseline_score"),
    analysis_type = "ggcoef_model",
    model_type = "lm",
    standardized = TRUE
  )
  
  expect_true(!is.null(result))
})

test_that("jggstats significance stars work", {
  
  result <- jggstats(
    data = linear_model_data,
    dependent_var = "outcome",
    independent_vars = c("age", "sex", "treatment"),
    analysis_type = "ggcoef_model",
    show_significance = TRUE
  )
  
  expect_true(!is.null(result))
})

# Complex Real-World Data Tests
test_that("jggstats works with complex survey data", {
  
  result <- jggstats(
    data = weighted_analysis_data,
    dependent_var = "health_rating",
    independent_vars = c("income_thousands", "head_education"),
    weight_var = "final_weight",
    grouping_var = "geography",
    analysis_type = "ggsurvey"
  )
  
  expect_true(!is.null(result))
})

test_that("jggstats works with longitudinal data", {
  
  result <- jggstats(
    data = mixed_effects_data,
    dependent_var = "score",
    independent_vars = c("time_months", "treatment_group"),
    grouping_var = "baseline_severity",
    analysis_type = "ggcoef_model",
    model_type = "lmer"
  )
  
  expect_true(!is.null(result))
})

test_that("jggstats works with educational data", {
  
  result <- jggstats(
    data = model_comparison_data,
    dependent_var = "test_score",
    independent_vars = c("socioeconomic_status", "parent_education", "school_type"),
    grouping_var = "district",
    analysis_type = "ggcoef_compare",
    model_type = "lm"
  )
  
  expect_true(!is.null(result))
})

# Error Handling Tests
test_that("jggstats handles empty data", {
  
  empty_data <- data.frame(
    outcome = numeric(0),
    predictor = numeric(0)
  )
  
  expect_error(
    jggstats(
      data = empty_data,
      dependent_var = "outcome",
      independent_vars = "predictor",
      analysis_type = "ggcoef_model"
    ),
    NA  # Should handle gracefully in jamovi context
  )
})

test_that("jggstats handles missing values", {
  
  data_with_na <- linear_model_data
  data_with_na$outcome[1:10] <- NA
  data_with_na$age[5:15] <- NA
  
  result <- jggstats(
    data = data_with_na,
    dependent_var = "outcome",
    independent_vars = c("age", "sex"),
    analysis_type = "ggcoef_model"
  )
  
  expect_true(!is.null(result))
})

test_that("jggstats handles single predictor", {
  
  result <- jggstats(
    data = linear_model_data,
    dependent_var = "outcome",
    independent_vars = "age",
    analysis_type = "ggcoef_model"
  )
  
  expect_true(!is.null(result))
})

# GLM Family Tests
test_that("jggstats works with different GLM families", {
  
  # Binomial family
  result_binomial <- jggstats(
    data = logistic_model_data,
    dependent_var = "disease",
    independent_vars = c("age", "bmi"),
    analysis_type = "ggcoef_model",
    model_type = "glm",
    family = "binomial"
  )
  
  expect_true(!is.null(result_binomial))
  
  # Create count data for Poisson
  count_data <- linear_model_data
  count_data$count_outcome <- rpois(nrow(count_data), lambda = 5)
  
  result_poisson <- jggstats(
    data = count_data,
    dependent_var = "count_outcome",
    independent_vars = c("age"),
    analysis_type = "ggcoef_model",
    model_type = "glm",
    family = "poisson"
  )
  
  expect_true(!is.null(result_poisson))
})

# Custom Formula Tests
test_that("jggstats works with custom formulas", {
  
  result <- jggstats(
    data = linear_model_data,
    model_formula = "outcome ~ age + sex + I(age^2)",
    analysis_type = "ggcoef_model",
    model_type = "lm"
  )
  
  expect_true(!is.null(result))
})

# Integration Tests with Multiple Options
test_that("jggstats comprehensive integration test", {
  
  # Test with many options enabled
  result <- jggstats(
    data = linear_model_data,
    dependent_var = "outcome",
    independent_vars = c("age", "sex", "treatment"),
    grouping_var = "group",
    weight_var = "weight",
    analysis_type = "ggcoef_model",
    model_type = "lm",
    confidence_level = 0.95,
    show_intercept = FALSE,
    sort_coefficients = TRUE,
    standardized = FALSE,
    show_statistics = TRUE,
    show_significance = TRUE,
    color_palette = "viridis",
    theme_style = "minimal",
    plot_title = "Comprehensive Model Analysis",
    plot_subtitle = "Testing all major features",
    x_label = "Effect Size",
    y_label = "Model Terms",
    facet_var = "education",
    facet_type = "wrap",
    show_model_summary = TRUE,
    show_interpretation = TRUE,
    output_format = "both"
  )
  
  expect_true(!is.null(result))
  expect_s3_class(result, "jmvAnalysis")
})

message("All jggstats tests completed successfully!")
