
test_that('coxphw analysis works', {
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
    offset_variable = runif(n, 1, 100),
    stratify_variable = sample(c('A', 'B'), n, replace = TRUE),
    cluster_variable = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- coxphw(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    weight_method = 'average',
    alpha = 0.5,
    offset_variable = 'offset_variable',
    stratify_variable = 'stratify_variable',
    cluster_variable = 'cluster_variable',
    confidence_level = 0.95,
    max_iterations = 10,
    convergence_tolerance = 1e-8,
    bootstrap_ci = FALSE,
    bootstrap_samples = 100,
    show_model_summary = TRUE,
    show_coefficients = TRUE,
    show_weights = TRUE,
    show_diagnostics = TRUE,
    show_comparison = TRUE,
    show_weight_plots = TRUE,
    show_residual_plots = TRUE,
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
  omv_path <- file.path('omv_output', 'coxphw.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

