
test_that('landmarkanalysis analysis works', {

  # Synthetic data generation
  set.seed(123)
  n <- 50
  test_df <- data.frame(
    time = runif(n, 1, 100),
    status = sample(c('A', 'B'), n, replace = TRUE),
    predictors1 = sample(c('A', 'B'), n, replace = TRUE),
    predictors2 = sample(c('A', 'B'), n, replace = TRUE),
    predictors3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- landmarkanalysis(
      data = test_df,
      time = 'time',
      status = 'status',
      predictors = c('predictors1', 'predictors2', 'predictors3'),
      prediction_window = 12,
      min_events = 10,
      include_baseline = TRUE,
      dynamic_prediction = TRUE,
      calibration_plot = TRUE,
      discrimination_plot = TRUE,
      supermodel = FALSE,
      bootstrap_validation = FALSE,
      n_bootstrap = 200
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'landmarkanalysis.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})
