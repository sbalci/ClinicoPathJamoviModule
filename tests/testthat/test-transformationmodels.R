
test_that('transformationmodels analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    stratify_variable = sample(c('A', 'B'), n, replace = TRUE),
    weights_variable = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- transformationmodels(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    transformation = 'boxcox',
    distribution = 'normal',
    method = 'ml',
    support = 'automatic',
    lambda_search = TRUE,
    lambda_range_min = -2,
    lambda_range_max = 2,
    stratify_variable = 'stratify_variable',
    weights_variable = 'weights_variable',
    confidence_level = 0.95,
    max_iterations = 10,
    convergence_tolerance = 1e-8,
    transformation_validation = TRUE,
    model_selection = FALSE,
    bootstrap_ci = FALSE,
    bootstrap_samples = 100,
    show_model_summary = TRUE,
    show_coefficients = TRUE,
    show_transformation = TRUE,
    show_diagnostics = TRUE,
    show_comparison = TRUE,
    show_transformation_plots = TRUE,
    show_residual_plots = TRUE,
    show_survival_plots = TRUE,
    show_qq_plots = TRUE,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'transformationmodels.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

