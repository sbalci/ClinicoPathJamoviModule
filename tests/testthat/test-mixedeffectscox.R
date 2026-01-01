
test_that('mixedeffectscox analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    fixed_effects1 = sample(c('A', 'B'), n, replace = TRUE),
    fixed_effects2 = sample(c('A', 'B'), n, replace = TRUE),
    fixed_effects3 = sample(c('A', 'B'), n, replace = TRUE),
    random_effects1 = sample(c('A', 'B'), n, replace = TRUE),
    random_effects2 = sample(c('A', 'B'), n, replace = TRUE),
    random_effects3 = sample(c('A', 'B'), n, replace = TRUE),
    offset_variable = runif(n, 1, 100),
    weights_variable = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- mixedeffectscox(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    fixed_effects = c('fixed_effects1', 'fixed_effects2', 'fixed_effects3'),
    random_effects = c('random_effects1', 'random_effects2', 'random_effects3'),
    random_structure = 'random_intercept',
    correlation_structure = 'independent',
    variance_structure = 'homoscedastic',
    estimation_method = 'reml',
    sparse_matrix = TRUE,
    ties_method = 'efron',
    offset_variable = 'offset_variable',
    weights_variable = 'weights_variable',
    confidence_level = 0.95,
    max_iterations = 10,
    convergence_tolerance = 1e-8,
    random_effects_prediction = TRUE,
    variance_components_test = TRUE,
    bootstrap_ci = FALSE,
    bootstrap_samples = 100,
    show_model_summary = TRUE,
    show_fixed_effects = TRUE,
    show_random_effects = TRUE,
    show_diagnostics = TRUE,
    show_comparison = TRUE,
    show_residual_plots = TRUE,
    show_random_effects_plots = TRUE,
    show_survival_plots = TRUE,
    show_forest_plot = TRUE,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'mixedeffectscox.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

