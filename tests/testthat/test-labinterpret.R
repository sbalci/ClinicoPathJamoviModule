
test_that('labinterpret analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    labValues1 = runif(n, 1, 100),
    labValues2 = runif(n, 1, 100),
    labValues3 = runif(n, 1, 100),
    patientDemo1 = sample(c('A', 'B'), n, replace = TRUE),
    patientDemo2 = sample(c('A', 'B'), n, replace = TRUE),
    patientDemo3 = sample(c('A', 'B'), n, replace = TRUE),
    testDates1 = runif(n, 1, 100),
    testDates2 = runif(n, 1, 100),
    testDates3 = runif(n, 1, 100),
    clinicalHistory1 = sample(c('A', 'B'), n, replace = TRUE),
    clinicalHistory2 = sample(c('A', 'B'), n, replace = TRUE),
    clinicalHistory3 = sample(c('A', 'B'), n, replace = TRUE),
    medicationVars1 = sample(c('A', 'B'), n, replace = TRUE),
    medicationVars2 = sample(c('A', 'B'), n, replace = TRUE),
    medicationVars3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- labinterpret(
      data = data,
    labValues = c('labValues1', 'labValues2', 'labValues3'),
    patientDemo = c('patientDemo1', 'patientDemo2', 'patientDemo3'),
    testDates = c('testDates1', 'testDates2', 'testDates3'),
    clinicalHistory = c('clinicalHistory1', 'clinicalHistory2', 'clinicalHistory3'),
    medicationVars = c('medicationVars1', 'medicationVars2', 'medicationVars3'),
    reference_ranges = TRUE,
    critical_values = TRUE,
    trend_analysis = TRUE,
    delta_checks = TRUE,
    reference_source = 'demographic_adjusted',
    interpretation_level = 'comprehensive',
    confidence_interval = 0.95,
    delta_threshold = 50,
    trend_window = 90,
    age_adjustment = TRUE,
    gender_adjustment = TRUE,
    ethnicity_adjustment = FALSE,
    medication_interaction = TRUE,
    clinical_correlation = TRUE,
    quality_assessment = TRUE,
    interpretation_plots = TRUE,
    trend_plots = TRUE,
    reference_visualization = TRUE,
    delta_plots = FALSE,
    correlation_matrix = FALSE,
    clinical_guidelines = TRUE,
    interpretation_report = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'labinterpret.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

