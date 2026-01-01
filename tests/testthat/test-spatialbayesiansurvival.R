
test_that('spatialbayesiansurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time = runif(n, 1, 100),
    status = sample(c('A', 'B'), n, replace = TRUE),
    predictors1 = sample(c('A', 'B'), n, replace = TRUE),
    predictors2 = sample(c('A', 'B'), n, replace = TRUE),
    predictors3 = sample(c('A', 'B'), n, replace = TRUE),
    spatial_coords1 = runif(n, 1, 100),
    spatial_coords2 = runif(n, 1, 100),
    spatial_coords3 = runif(n, 1, 100),
    region_id = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- spatialbayesiansurvival(
      data = data,
    time = 'time',
    status = 'status',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    spatial_coords = c('spatial_coords1', 'spatial_coords2', 'spatial_coords3'),
    region_id = 'region_id',
    spatial_model = 'car',
    spatial_prior = 'icar',
    distance_method = 'great_circle',
    neighborhood_threshold = 50,
    num_neighbors = 5,
    baseline_hazard = 'weibull',
    piecewise_intervals = 5,
    spline_knots = 5,
    mcmc_samples = 5000,
    mcmc_burnin = 2000,
    mcmc_thin = 1,
    mcmc_chains = 3,
    covariate_priors = 'normal_weak',
    covariate_prior_sd = 10,
    spatial_precision_prior = 'gamma',
    spatial_precision_shape = 1,
    spatial_precision_rate = 0.5,
    model_comparison = TRUE,
    cross_validation = FALSE,
    cv_folds = 5,
    model_selection_criteria = 'dic',
    spatial_prediction = TRUE,
    prediction_grid_size = 50,
    confidence_level = 0.95,
    show_model_summary = TRUE,
    show_parameter_estimates = TRUE,
    show_spatial_effects = TRUE,
    show_residual_maps = TRUE,
    show_survival_maps = TRUE,
    show_hazard_maps = FALSE,
    show_convergence_diagnostics = TRUE,
    show_model_comparison = TRUE,
    show_interpretation = TRUE,
    standardize_predictors = TRUE,
    include_intercept = TRUE,
    missing_data_handling = 'complete_cases',
    parallel_processing = TRUE,
    n_cores = 1,
    clinical_context = 'cancer_epidemiology',
    set_seed = TRUE,
    seed_value = 42
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'spatialbayesiansurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

