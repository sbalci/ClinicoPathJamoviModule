
test_that('predmodel analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    predictors1 = runif(n, 1, 100),
    predictors2 = runif(n, 1, 100),
    predictors3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- predmodel(
      data = data,
    outcome = 'outcome',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    modelSelection = 'none',
    validationMethod = 'bootstrap',
    nBootstrap = 200,
    nFolds = 10,
    showCalibration = TRUE,
    showDiscrimination = TRUE,
    showRiskGroups = TRUE,
    ciLevel = 0.95
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'predmodel.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

