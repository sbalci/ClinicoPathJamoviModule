
test_that('survivalmodelvalidation analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time_var = runif(n, 1, 100),
    status_var = sample(c(0, 1), n, replace = TRUE),
    risk_score = runif(n, 1, 100),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- survivalmodelvalidation(
      data = data,
    time_var = 'time_var',
    status_var = 'status_var',
    risk_score = 'risk_score',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    stratification_var = 'stratification_var',
    validation_type = 'internal_bootstrap',
    model_type = 'cox_ph',
    performance_metrics = 'all_metrics',
    calibration_assessment = TRUE,
    calibration_method = 'decile_based',
    bootstrap_samples = 100,
    cv_folds = 10,
    confidence_level = 0.95,
    optimism_correction = TRUE,
    shrinkage_estimation = TRUE,
    discrimination_plots = TRUE,
    calibration_plots = TRUE,
    roc_curves = TRUE,
    prediction_error_plots = TRUE,
    risk_distribution_plots = TRUE,
    model_comparison = FALSE,
    decision_curve_analysis = TRUE,
    transportability_analysis = FALSE,
    subgroup_validation = TRUE,
    temporal_trends = FALSE,
    missing_data_sensitivity = FALSE,
    clinical_impact_metrics = TRUE,
    reporting_guidelines = 'tripod',
    export_validation_report = FALSE
    )
  })

  # Verify results are present and populated
  expect_true(!is.null(model))
  expect_true("performanceMetrics" %in% names(model))
  
  # Check tables are populated
  expect_true(nrow(model$performanceMetrics$asDF) >= 1)
  expect_true(nrow(model$calibrationMetrics$asDF) >= 1)
  expect_true(nrow(model$validationResults$asDF) >= 1)
})

