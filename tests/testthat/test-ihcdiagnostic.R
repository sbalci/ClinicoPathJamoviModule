
test_that('ihcdiagnostic analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    markers1 = sample(c('A', 'B'), n, replace = TRUE),
    markers2 = sample(c('A', 'B'), n, replace = TRUE),
    markers3 = sample(c('A', 'B'), n, replace = TRUE),
    diagnosis = sample(c('A', 'B'), n, replace = TRUE),
    id = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- ihcdiagnostic(
      data = data,
    markers = c('markers1', 'markers2', 'markers3'),
    diagnosis = 'diagnosis',
    id = 'id',
    differentialDiagnosis = TRUE,
    antibodyOptimization = FALSE,
    calculateDiagnosticMetrics = TRUE,
    clusterMethod = 'hierarchical',
    cutpointMethod = 'optimal',
    confidenceLevel = 0.95,
    crossValidation = TRUE,
    minimumGroupSize = 10
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'ihcdiagnostic.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

