
test_that('measurementuncertainty analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    measurement = runif(n, 1, 100),
    reference_value = runif(n, 1, 100),
    replicate_group = sample(c('A', 'B'), n, replace = TRUE),
    calibrator = sample(c('A', 'B'), n, replace = TRUE),
    operator = sample(c('A', 'B'), n, replace = TRUE),
    instrument = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- measurementuncertainty(
      data = data,
    measurement = 'measurement',
    reference_value = 'reference_value',
    replicate_group = 'replicate_group',
    calibrator = 'calibrator',
    operator = 'operator',
    instrument = 'instrument',
    uncertainty_method = 'gum_approach',
    confidence_level = 0.95,
    coverage_factor = 2,
    calibration_uncertainty = 1,
    reference_material_uncertainty = 0.5,
    temperature_uncertainty = 0.1,
    pipetting_uncertainty = 0.2,
    sample_stability_uncertainty = 0.3,
    matrix_effects_uncertainty = 0.2,
    monte_carlo_simulations = 100000,
    correlation_analysis = TRUE,
    sensitivity_analysis = TRUE,
    budget_optimization = TRUE,
    measurement_model = TRUE,
    validation_experiments = FALSE,
    proficiency_testing_data = FALSE,
    interlaboratory_comparison = FALSE,
    clinical_significance = TRUE,
    iso15189_compliance = TRUE,
    uncertainty_plots = TRUE,
    budget_plots = TRUE,
    monte_carlo_plots = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'measurementuncertainty.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

