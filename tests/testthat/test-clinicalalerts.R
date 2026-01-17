
test_that('clinicalalerts analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 100
  data <- data.frame(
    PatientID = paste0('P', 1:n),
    Age = rnorm(n, 65, 10),
    HR = rnorm(n, 80, 15),
    Time = 1:n
  )

  # Run analysis
  expect_no_error({
    model <- clinicalalerts(
      data = data,
      clinicalVars = c('Age', 'HR'),
      patientId = 'PatientID',
      timeVar = 'Time',
      alert_summary = TRUE,
      detailed_alerts = TRUE,
      clinical_recommendations = TRUE,
      patient_analysis = TRUE,
      trend_analysis = TRUE,
      use_clinical_defaults = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(inherits(model, 'R6'))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  # omv_path <- file.path('omv_output', 'clinicalalerts.omv')
  # if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  # expect_no_error({
  #   jmvReadWrite::write_omv(model, omv_path)
  # })

  # expect_true(file.exists(omv_path))
})
