
test_that('clinicaldashboard analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    patientId = sample(c('A', 'B'), n, replace = TRUE),
    timeVar = runif(n, 1, 100),
    outcomeVars1 = sample(c('A', 'B'), n, replace = TRUE),
    outcomeVars2 = sample(c('A', 'B'), n, replace = TRUE),
    outcomeVars3 = sample(c('A', 'B'), n, replace = TRUE),
    groupVar = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- clinicaldashboard(
      data = data,
    patientId = 'patientId',
    timeVar = 'timeVar',
    outcomeVars = c('outcomeVars1', 'outcomeVars2', 'outcomeVars3'),
    groupVar = 'groupVar',
    dashboardType = 'population',
    timeWindow = 'last_90_days',
    showTrends = TRUE,
    showAlerts = TRUE,
    showSummaryStats = TRUE,
    showDistributions = TRUE,
    realTimeUpdate = FALSE,
    exportDashboard = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'clinicaldashboard.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

