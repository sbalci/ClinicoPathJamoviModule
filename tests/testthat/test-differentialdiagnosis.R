
test_that('differentialdiagnosis analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    clinicalFindings1 = sample(c('A', 'B'), n, replace = TRUE),
    clinicalFindings2 = sample(c('A', 'B'), n, replace = TRUE),
    clinicalFindings3 = sample(c('A', 'B'), n, replace = TRUE),
    confirmedDiagnosis = sample(c('A', 'B'), n, replace = TRUE),
    demographicVars1 = sample(c('A', 'B'), n, replace = TRUE),
    demographicVars2 = sample(c('A', 'B'), n, replace = TRUE),
    demographicVars3 = sample(c('A', 'B'), n, replace = TRUE),
    labResults1 = runif(n, 1, 100),
    labResults2 = runif(n, 1, 100),
    labResults3 = runif(n, 1, 100),
    imagingFindings1 = sample(c('A', 'B'), n, replace = TRUE),
    imagingFindings2 = sample(c('A', 'B'), n, replace = TRUE),
    imagingFindings3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- differentialdiagnosis(
      data = data,
    clinicalFindings = c('clinicalFindings1', 'clinicalFindings2', 'clinicalFindings3'),
    confirmedDiagnosis = 'confirmedDiagnosis',
    demographicVars = c('demographicVars1', 'demographicVars2', 'demographicVars3'),
    labResults = c('labResults1', 'labResults2', 'labResults3'),
    imagingFindings = c('imagingFindings1', 'imagingFindings2', 'imagingFindings3'),
    diagnostic_probability = TRUE,
    likelihood_ratios = TRUE,
    prevalence_adjustment = TRUE,
    differential_ranking = TRUE,
    bayesian_network = FALSE,
    reasoning_method = 'naive_bayes',
    prevalence_source = 'population_based',
    confidence_threshold = 0.5,
    max_diagnoses = 10,
    clinical_context = TRUE,
    uncertainty_analysis = TRUE,
    sensitivity_analysis = FALSE,
    confidence_level = 0.95,
    diagnostic_plots = TRUE,
    network_diagram = FALSE,
    probability_heatmap = FALSE,
    clinical_guidelines = TRUE,
    differential_explanation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'differentialdiagnosis.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

