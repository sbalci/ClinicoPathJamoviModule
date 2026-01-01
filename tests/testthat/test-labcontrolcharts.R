
test_that('labcontrolcharts analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    measurement = runif(n, 1, 100),
    run_number = sample(c('A', 'B'), n, replace = TRUE),
    batch_id = sample(c('A', 'B'), n, replace = TRUE),
    control_level = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- labcontrolcharts(
      data = data,
    measurement = 'measurement',
    run_number = 'run_number',
    batch_id = 'batch_id',
    control_level = 'control_level',
    chart_type = 'shewhart',
    control_limits = '3sigma',
    target_mean = 0,
    target_sd = 0,
    cusum_k = 0.5,
    cusum_h = 4,
    ewma_lambda = 0.2,
    baseline_runs = 20,
    violation_detection = TRUE,
    trend_analysis = TRUE,
    shift_detection = TRUE,
    performance_metrics = TRUE,
    corrective_actions = TRUE,
    qc_summary = TRUE,
    export_violations = FALSE,
    control_plots = TRUE,
    histogram_plot = TRUE,
    trend_plot = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'labcontrolcharts.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

