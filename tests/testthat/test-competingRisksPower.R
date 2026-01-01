
test_that('competingRisksPower analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(

  )

  # Run analysis
  expect_no_error({
    model <- competingRisksPower(
      data = data,
    analysisType = 'power',
    alpha = 0.05,
    power = 0.8,
    totalSampleSize = 200,
    followUpTime = 5,
    accrualTime = 2,
    eventRate1 = 0.3,
    competingRate1 = 0.2,
    eventRate2 = 0.4,
    competingRate2 = 0.2,
    hazardRatio = 1.5,
    testType = 'gray',
    distributionType = 'exponential',
    shape1 = 1,
    shape2 = 1,
    numberOfSimulations = 1000,
    showSimulationDetails = FALSE,
    showEducational = TRUE,
    plotPowerCurve = TRUE,
    plotEventRates = FALSE,
    sensitivityAnalysis = FALSE,
    confidenceLevel = 0.95
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'competingRisksPower.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

