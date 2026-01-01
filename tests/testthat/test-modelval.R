
test_that('modelval analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    predicted = runif(n, 1, 100),
    subgroup = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- modelval(
      data = data,
    outcome = 'outcome',
    predicted = 'predicted',
    validationType = 'general',
    subgroup = 'subgroup',
    showCalibrationLarge = TRUE,
    showCalibrationSlope = TRUE,
    showFlexibleCalibration = TRUE,
    calibrationGroups = 10,
    showDiscrimination = TRUE,
    showNetBenefit = TRUE,
    showSubgroupAnalysis = FALSE,
    ciLevel = 0.95
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'modelval.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

