
test_that('waterfallrecist analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    patientID = sample(c('A', 'B'), n, replace = TRUE),
    lesionID = sample(c('A', 'B'), n, replace = TRUE),
    visitTime = runif(n, 1, 100),
    lesionType = sample(c('A', 'B'), n, replace = TRUE),
    location = sample(c('A', 'B'), n, replace = TRUE),
    diameter = runif(n, 1, 100),
    isNewLesion = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- waterfallrecist(
      data = data,
    patientID = 'patientID',
    lesionID = 'lesionID',
    visitTime = 'visitTime',
    lesionType = 'lesionType',
    location = 'location',
    diameter = 'diameter',
    isNewLesion = 'isNewLesion',
    baselineTimepoint = 0,
    confirmationInterval = 4,
    maxTargetLesions = 5,
    maxLesionsPerOrgan = 2,
    showWaterfallPlot = TRUE,
    showSpiderPlot = TRUE,
    showLesionTable = TRUE,
    showTargetSumTable = TRUE,
    showBestResponseTable = TRUE,
    showRecistComplianceReport = TRUE,
    colorScheme = 'recist'
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'waterfallrecist.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

