
test_that('frailtysurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time_var = runif(n, 1, 100),
    status_var = sample(c('A', 'B'), n, replace = TRUE),
    cluster_var = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    strata_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    strata_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    strata_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    random_effects1 = sample(c('A', 'B'), n, replace = TRUE),
    random_effects2 = sample(c('A', 'B'), n, replace = TRUE),
    random_effects3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- frailtysurvival(
      data = data,
    time_var = 'time_var',
    status_var = 'status_var',
    cluster_var = 'cluster_var',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    strata_vars = c('strata_vars1', 'strata_vars2', 'strata_vars3'),
    frailty_type = 'shared',
    frailty_distribution = 'gamma',
    estimation_method = 'penalized_likelihood',
    confidence_level = 0.95,
    baseline_hazard = 'cox',
    random_effects = c('random_effects1', 'random_effects2', 'random_effects3'),
    test_frailty = TRUE,
    variance_components = TRUE,
    cluster_diagnostics = TRUE,
    prediction_intervals = FALSE,
    survival_curves = TRUE,
    residual_analysis = TRUE,
    model_comparison = TRUE,
    mcmc_iterations = 10000,
    burnin_samples = 2000,
    convergence_diagnostics = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'frailtysurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

