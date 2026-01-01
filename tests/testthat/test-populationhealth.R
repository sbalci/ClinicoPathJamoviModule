
test_that('populationhealth analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    patientID = sample(c('A', 'B'), n, replace = TRUE),
    healthOutcomes1 = runif(n, 1, 100),
    healthOutcomes2 = runif(n, 1, 100),
    healthOutcomes3 = runif(n, 1, 100),
    demographics1 = sample(c('A', 'B'), n, replace = TRUE),
    demographics2 = sample(c('A', 'B'), n, replace = TRUE),
    demographics3 = sample(c('A', 'B'), n, replace = TRUE),
    geographic1 = sample(c('A', 'B'), n, replace = TRUE),
    geographic2 = sample(c('A', 'B'), n, replace = TRUE),
    geographic3 = sample(c('A', 'B'), n, replace = TRUE),
    timeVariable1 = runif(n, 1, 100),
    timeVariable2 = runif(n, 1, 100),
    timeVariable3 = runif(n, 1, 100),
    riskFactors1 = sample(c('A', 'B'), n, replace = TRUE),
    riskFactors2 = sample(c('A', 'B'), n, replace = TRUE),
    riskFactors3 = sample(c('A', 'B'), n, replace = TRUE),
    healthcareUtilization1 = runif(n, 1, 100),
    healthcareUtilization2 = runif(n, 1, 100),
    healthcareUtilization3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- populationhealth(
      data = data,
    patientID = 'patientID',
    healthOutcomes = c('healthOutcomes1', 'healthOutcomes2', 'healthOutcomes3'),
    demographics = c('demographics1', 'demographics2', 'demographics3'),
    geographic = c('geographic1', 'geographic2', 'geographic3'),
    timeVariable = c('timeVariable1', 'timeVariable2', 'timeVariable3'),
    riskFactors = c('riskFactors1', 'riskFactors2', 'riskFactors3'),
    healthcareUtilization = c('healthcareUtilization1', 'healthcareUtilization2', 'healthcareUtilization3'),
    population_stratification = TRUE,
    risk_stratification = TRUE,
    geographic_analysis = TRUE,
    temporal_trends = TRUE,
    health_disparities = TRUE,
    predictive_modeling = TRUE,
    population_type = 'general',
    analysis_scope = 'comprehensive',
    geographic_level = 'county',
    time_window = 12,
    risk_threshold = 0.7,
    disparity_analysis = TRUE,
    social_determinants = TRUE,
    intervention_analysis = FALSE,
    comparative_analysis = TRUE,
    quality_metrics = TRUE,
    surveillance_system = TRUE,
    outcome_prediction = TRUE,
    resource_allocation = FALSE,
    population_visualization = TRUE,
    geographic_mapping = TRUE,
    trend_visualization = TRUE,
    disparity_plots = TRUE,
    interactive_dashboard = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'populationhealth.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

