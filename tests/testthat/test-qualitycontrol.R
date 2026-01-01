
test_that('qualitycontrol analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    measurement_var = runif(n, 1, 100),
    time_var = runif(n, 1, 100),
    control_level = sample(c('A', 'B'), n, replace = TRUE),
    batch_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- qualitycontrol(
      data = data,
    measurement_var = 'measurement_var',
    time_var = 'time_var',
    control_level = 'control_level',
    batch_var = 'batch_var',
    analysis_type = 'control_charts',
    control_chart_type = 'all_charts',
    target_value = 1,
    target_sd = 1,
    control_limits_method = 'classical',
    custom_lower_limit = 1,
    custom_upper_limit = 1,
    cusum_target = 1,
    cusum_k_value = 0.5,
    cusum_h_value = 5,
    ewma_lambda = 0.2,
    sigma_calculation = 'total_error',
    allowable_error = 10,
    reference_method = 'nonparametric',
    reference_percentile = '95',
    outlier_detection = 'tukey',
    pt_target_value = 1,
    pt_acceptable_range = 1,
    validation_components = 'comprehensive',
    precision_design = 'ep5_a3',
    replicate_days = 20,
    replicates_per_day = 2,
    confidence_level = 0.95,
    westgard_rules = TRUE,
    trend_analysis = TRUE,
    capability_analysis = FALSE,
    generate_plots = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'qualitycontrol.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

