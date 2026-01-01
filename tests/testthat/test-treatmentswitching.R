
test_that('treatmentswitching analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time = runif(n, 1, 100),
    event = sample(c('A', 'B'), n, replace = TRUE),
    treatment = sample(c('A', 'B'), n, replace = TRUE),
    switchingTime = runif(n, 1, 100),
    actualTreatment = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- treatmentswitching(
      data = data,
    time = 'time',
    event = 'event',
    treatment = 'treatment',
    switchingTime = 'switchingTime',
    actualTreatment = 'actualTreatment',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    switchingMethod = 'ipcw',
    switchingDirection = 'control_to_treatment',
    censoringAssumption = 'common_treatment',
    confidenceLevel = 95,
    treatmentEffect = TRUE,
    switchingPatterns = TRUE,
    survivalCurves = TRUE,
    sensitivityAnalysis = FALSE,
    causalEstimate = TRUE,
    methodComparison = TRUE,
    bootstrapSamples = 1000,
    accelerationFactor = 1,
    treatmentLag = 0,
    plotWidth = 700,
    plotHeight = 500
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'treatmentswitching.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

