
test_that('clinicalalerts analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    clinicalVars1 = runif(n, 1, 100),
    clinicalVars2 = runif(n, 1, 100),
    clinicalVars3 = runif(n, 1, 100),
    patientId = sample(c('A', 'B'), n, replace = TRUE),
    timeVar = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- clinicalalerts(
      data = data,
    clinicalVars = c('clinicalVars1', 'clinicalVars2', 'clinicalVars3'),
    patientId = 'patientId',
    timeVar = 'timeVar',
    alert_summary = TRUE,
    detailed_alerts = TRUE,
    clinical_recommendations = TRUE,
    patient_analysis = FALSE,
    trend_analysis = FALSE,
    use_clinical_defaults = TRUE,
    custom_thresholds = FALSE,
    include_medium_priority = TRUE,
    include_low_priority = FALSE,
    confidence_level = 0.95,
    alert_dashboard = TRUE,
    threshold_plots = FALSE,
    trend_plots = FALSE,
    interpretation_guide = TRUE,
    evidence_references = FALSE,
    quality_metrics = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'clinicalalerts.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

