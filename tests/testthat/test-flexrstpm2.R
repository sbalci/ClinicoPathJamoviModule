
test_that('flexrstpm2 analysis works', {
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
    time_varying_covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    time_varying_covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    time_varying_covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    bhazard = runif(n, 1, 100),
    group_variable = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- flexrstpm2(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    scale = 'hazard',
    df = 4,
    time_varying_covariates = c('time_varying_covariates1', 'time_varying_covariates2', 'time_varying_covariates3'),
    tvc_df = 3,
    cure_fraction = FALSE,
    bhazard = 'bhazard',
    group_variable = 'group_variable',
    link_function = 'log',
    extrapolation_time = 20,
    confidence_level = 0.95,
    robust_se = FALSE,
    time_ratio = FALSE,
    relative_survival = FALSE,
    model_comparison = TRUE,
    goodness_of_fit = TRUE,
    hazard_analysis = TRUE,
    derivative_analysis = FALSE,
    bootstrap_validation = FALSE,
    bootstrap_samples = 100,
    show_model_summary = TRUE,
    show_coefficients_table = TRUE,
    show_survival_curves = TRUE,
    show_hazard_curves = TRUE,
    show_time_varying_plots = FALSE,
    show_model_diagnostics = TRUE,
    show_residuals = FALSE,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'flexrstpm2.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

