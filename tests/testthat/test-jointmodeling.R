
test_that('jointmodeling analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    id = sample(c('A', 'B'), n, replace = TRUE),
    time_longitudinal = runif(n, 1, 100),
    biomarker = runif(n, 1, 100),
    survival_time = runif(n, 1, 100),
    survival_status = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    multiple_biomarkers1 = runif(n, 1, 100),
    multiple_biomarkers2 = runif(n, 1, 100),
    multiple_biomarkers3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- jointmodeling(
      data = data,
    id = 'id',
    time_longitudinal = 'time_longitudinal',
    biomarker = 'biomarker',
    survival_time = 'survival_time',
    survival_status = 'survival_status',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    functional_form = 'linear',
    random_effects = 'intercept_slope',
    error_structure = 'independent',
    survival_model = 'cox',
    association_structure = 'current_value',
    estimation_method = 'bayesian',
    mcmc_chains = 3,
    mcmc_iterations = 12000,
    mcmc_burnin = 2000,
    mcmc_thin = 5,
    dynamic_prediction = TRUE,
    prediction_window = 2,
    internal_validation = TRUE,
    cv_folds = 5,
    discrimination_metrics = TRUE,
    plot_trajectories = TRUE,
    plot_mean_trajectory = TRUE,
    plot_survival_curves = TRUE,
    plot_dynamic_auc = TRUE,
    plot_residuals = FALSE,
    competing_risks = FALSE,
    left_truncation = FALSE,
    time_varying_effects = FALSE,
    multiple_biomarkers = c('multiple_biomarkers1', 'multiple_biomarkers2', 'multiple_biomarkers3'),
    baseline_hazard = 'unspecified',
    prior_specification = 'default',
    convergence_diagnostics = TRUE,
    parallel_computation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'jointmodeling.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

