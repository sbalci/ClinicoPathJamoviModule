
test_that('intervalsurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    left_time = runif(n, 1, 100),
    right_time = runif(n, 1, 100),
    status_var = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- intervalsurvival(
      data = data,
    left_time = 'left_time',
    right_time = 'right_time',
    status_var = 'status_var',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    stratification_vars = c('stratification_vars1', 'stratification_vars2', 'stratification_vars3'),
    model_type = 'aft_weibull',
    estimation_method = 'em_algorithm',
    confidence_level = 0.95,
    convergence_tolerance = 1e-04,
    max_iterations = 50,
    baseline_hazard_smooth = FALSE,
    smoothing_bandwidth = 1,
    survival_curves = TRUE,
    hazard_function = FALSE,
    model_diagnostics = TRUE,
    residual_analysis = TRUE,
    goodness_of_fit = TRUE,
    model_comparison = FALSE,
    bootstrap_samples = 50,
    mcmc_iterations = 5000,
    mcmc_burnin = 1000,
    imputation_method = 'midpoint'
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'intervalsurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

