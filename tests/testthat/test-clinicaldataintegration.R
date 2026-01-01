
test_that('clinicaldataintegration analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    patientIdVar = sample(c('A', 'B'), n, replace = TRUE),
    dateVars1 = runif(n, 1, 100),
    dateVars2 = runif(n, 1, 100),
    dateVars3 = runif(n, 1, 100),
    clinicalVars1 = sample(c('A', 'B'), n, replace = TRUE),
    clinicalVars2 = sample(c('A', 'B'), n, replace = TRUE),
    clinicalVars3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- clinicaldataintegration(
      data = data,
    dataSource = 'csv',
    patientIdVar = 'patientIdVar',
    dateVars = c('dateVars1', 'dateVars2', 'dateVars3'),
    clinicalVars = c('clinicalVars1', 'clinicalVars2', 'clinicalVars3'),
    qualityCheck = TRUE,
    completenessThreshold = 80,
    consistencyCheck = TRUE,
    outlierDetection = TRUE,
    terminologyMapping = 'none',
    exportFormat = 'csv',
    generateReport = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'clinicaldataintegration.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

