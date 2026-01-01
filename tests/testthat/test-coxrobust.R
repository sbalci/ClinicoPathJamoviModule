
test_that('coxrobust analysis works', {
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
    model <- coxrobust(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    robust_method = 'huber',
    tuning_constant = 1.345,
    efficiency_target = 0.95,
    max_iterations = 10,
    convergence_tolerance = 1e-8,
    outlier_detection = TRUE,
    outlier_threshold = 3,
    stratify_variable = 'stratify_variable',
    weights_variable = 'weights_variable',
    bootstrap_ci = FALSE,
    bootstrap_samples = 100,
    confidence_level = 0.95,
    show_model_summary = TRUE,
    show_coefficients = TRUE,
    show_outliers = TRUE,
    show_influence = TRUE,
    show_comparison = TRUE,
    show_residual_plots = TRUE,
    show_influence_plots = TRUE,
    show_weight_plots = TRUE,
    show_survival_plots = TRUE,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'coxrobust.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

