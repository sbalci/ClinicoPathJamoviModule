
test_that('treatmentoptim analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    patientVars1 = sample(c('A', 'B'), n, replace = TRUE),
    patientVars2 = sample(c('A', 'B'), n, replace = TRUE),
    patientVars3 = sample(c('A', 'B'), n, replace = TRUE),
    treatmentOptions = sample(c('A', 'B'), n, replace = TRUE),
    responseVar = sample(c('A', 'B'), n, replace = TRUE),
    medicationVars1 = sample(c('A', 'B'), n, replace = TRUE),
    medicationVars2 = sample(c('A', 'B'), n, replace = TRUE),
    medicationVars3 = sample(c('A', 'B'), n, replace = TRUE),
    labVars1 = runif(n, 1, 100),
    labVars2 = runif(n, 1, 100),
    labVars3 = runif(n, 1, 100),
    comorbidityVars1 = sample(c('A', 'B'), n, replace = TRUE),
    comorbidityVars2 = sample(c('A', 'B'), n, replace = TRUE),
    comorbidityVars3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- treatmentoptim(
      data = data,
    patientVars = c('patientVars1', 'patientVars2', 'patientVars3'),
    treatmentOptions = 'treatmentOptions',
    responseVar = 'responseVar',
    medicationVars = c('medicationVars1', 'medicationVars2', 'medicationVars3'),
    labVars = c('labVars1', 'labVars2', 'labVars3'),
    comorbidityVars = c('comorbidityVars1', 'comorbidityVars2', 'comorbidityVars3'),
    treatment_selection = TRUE,
    drug_interaction = TRUE,
    dose_optimization = FALSE,
    safety_assessment = TRUE,
    treatment_comparison = TRUE,
    prediction_model = 'logistic',
    interaction_severity = 'major_critical',
    dose_adjustment = TRUE,
    confidence_level = 0.95,
    treatment_plots = TRUE,
    interaction_network = FALSE,
    dose_response_plots = FALSE,
    clinical_guidelines = TRUE,
    pharmacokinetic_model = FALSE,
    population_model = TRUE,
    individual_factors = TRUE,
    evidence_level = 'high_moderate',
    clinical_interpretation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'treatmentoptim.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

