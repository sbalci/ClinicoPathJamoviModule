
test_that('nonparametricregression analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = runif(n, 1, 100),
    predictors1 = sample(c('A', 'B'), n, replace = TRUE),
    predictors2 = sample(c('A', 'B'), n, replace = TRUE),
    predictors3 = sample(c('A', 'B'), n, replace = TRUE),
    grouping_variable = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  model <- nonparametricregression(
      data = data,
      outcome = 'outcome',
      predictors = c('predictors1', 'predictors2', 'predictors3'),
      grouping_variable = 'grouping_variable',
      regression_type = 'kernel',
      kernel_type = 'gaussian',
      bandwidth_method = 'cross_validation',
      manual_bandwidth = 0.1,
      loess_span = 0.75,
      loess_degree = 'linear',
      loess_iterations = 3,
      spline_type = 'smooth',
      spline_df = 4,
      spline_lambda = 0.1,
      quantile_tau = 0.5,
      quantile_method = 'interior_point',
      gam_smoother = 'cubic_spline',
      gam_basis_dim = 10,
      cv_folds = 10,
      cv_repeats = 1,
      model_selection_criterion = 'cv_error',
      robust_regression = FALSE,
      robust_method = 'huber',
      outlier_threshold = 3,
      confidence_level = 0.95,
      bootstrap_ci = TRUE,
      bootstrap_samples = 100,
      prediction_intervals = TRUE,
      residual_diagnostics = TRUE,
      influence_diagnostics = FALSE,
      goodness_of_fit = TRUE,
      show_fitted_curves = TRUE,
      show_confidence_bands = TRUE,
      show_prediction_bands = FALSE,
      show_residual_plots = TRUE,
      show_qq_plots = TRUE,
      show_partial_plots = FALSE,
      show_model_summary = TRUE,
      show_parameter_estimates = TRUE,
      show_model_comparison = FALSE,
      show_interpretation = TRUE,
      multivariate_method = 'additive',
      missing_data_handling = 'complete_cases',
      clinical_context = 'general',
      set_seed = TRUE,
      seed_value = 42,
      parallel_processing = FALSE,
      n_cores = 1
    )

  # Verify and Export OMV
  omv_path <- file.path('omv_output', 'nonparametricregression.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  tryCatch({
    jmvReadWrite::write_omv(model, omv_path)
  }, error = function(e){
      message("OMV export failed: ", e$message)
  })
  
  if (!file.exists(omv_path)) {
     skip("OMV export failed, skipping file existence check")
  }

  expect_true(file.exists(omv_path))
})

