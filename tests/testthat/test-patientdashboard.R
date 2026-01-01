
test_that('patientdashboard analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    patientID = sample(c('A', 'B'), n, replace = TRUE),
    vitals1 = runif(n, 1, 100),
    vitals2 = runif(n, 1, 100),
    vitals3 = runif(n, 1, 100),
    labValues1 = runif(n, 1, 100),
    labValues2 = runif(n, 1, 100),
    labValues3 = runif(n, 1, 100),
    timestamps1 = runif(n, 1, 100),
    timestamps2 = runif(n, 1, 100),
    timestamps3 = runif(n, 1, 100),
    clinicalNotes1 = sample(c('A', 'B'), n, replace = TRUE),
    clinicalNotes2 = sample(c('A', 'B'), n, replace = TRUE),
    clinicalNotes3 = sample(c('A', 'B'), n, replace = TRUE),
    medications1 = sample(c('A', 'B'), n, replace = TRUE),
    medications2 = sample(c('A', 'B'), n, replace = TRUE),
    medications3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- patientdashboard(
      data = data,
    patientID = 'patientID',
    vitals = c('vitals1', 'vitals2', 'vitals3'),
    labValues = c('labValues1', 'labValues2', 'labValues3'),
    timestamps = c('timestamps1', 'timestamps2', 'timestamps3'),
    clinicalNotes = c('clinicalNotes1', 'clinicalNotes2', 'clinicalNotes3'),
    medications = c('medications1', 'medications2', 'medications3'),
    realtime_monitoring = TRUE,
    alert_system = TRUE,
    trend_analysis = TRUE,
    risk_stratification = TRUE,
    dashboard_type = 'comprehensive',
    monitoring_frequency = 'hourly',
    alert_thresholds = 'standard',
    time_window = 24,
    alert_priority = TRUE,
    predictive_analytics = FALSE,
    clinical_pathways = TRUE,
    medication_reconciliation = FALSE,
    family_communication = FALSE,
    quality_metrics = TRUE,
    workflow_optimization = TRUE,
    interactive_plots = TRUE,
    trend_plots = TRUE,
    alert_dashboard = TRUE,
    risk_dashboard = TRUE,
    medication_dashboard = FALSE,
    summary_reports = TRUE,
    performance_analytics = FALSE,
    patient_outcomes = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'patientdashboard.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

