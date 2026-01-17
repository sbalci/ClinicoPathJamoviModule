
test_that('bayesianci analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = runif(n, 0, 100),
    group_var = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- bayesianci(
      data = data,
    outcome = 'outcome',
    group_var = 'group_var',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    outcome_type = 'continuous',
    credible_level = 0.95,
    prior_type = 'uniform',
    beta_alpha = 1,
    beta_beta = 1,
    normal_mean = 0,
    normal_sd = 1,
    gamma_shape = 1,
    gamma_rate = 1,
    mcmc_method = 'analytical',
    mcmc_samples = 10000,
    burnin_samples = 2000,
    thinning = 1,
    chains = 3,
    posterior_summary = TRUE,
    convergence_diagnostics = TRUE,
    prior_posterior_comparison = TRUE,
    credible_vs_confidence = TRUE,
    hpd_intervals = TRUE,
    sensitivity_analysis = TRUE,
    robustness_check = TRUE,
    clinical_interpretation = TRUE,
    posterior_plots = TRUE,
    trace_plots = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(inherits(model, "R6"))
  expect_true(inherits(model, "bayesianciResults"))

  # Define output path
  omv_path <- file.path('omv_output', 'bayesianci.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  # expect_no_error({
  #   jmvReadWrite::write_omv(model, omv_path)
  # })

  # expect_true(file.exists(omv_path))
})

