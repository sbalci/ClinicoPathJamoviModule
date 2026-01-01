
test_that('mixedcox analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    dxdate = runif(n, 1, 100),
    fudate = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    fixed_effects1 = sample(c('A', 'B'), n, replace = TRUE),
    fixed_effects2 = sample(c('A', 'B'), n, replace = TRUE),
    fixed_effects3 = sample(c('A', 'B'), n, replace = TRUE),
    continuous_effects1 = runif(n, 1, 100),
    continuous_effects2 = runif(n, 1, 100),
    continuous_effects3 = runif(n, 1, 100),
    cluster_var = sample(c('A', 'B'), n, replace = TRUE),
    random_slope_var = sample(c('A', 'B'), n, replace = TRUE),
    nested_cluster_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- mixedcox(
      data = data,
    elapsedtime = 'elapsedtime',
    tint = FALSE,
    dxdate = 'dxdate',
    fudate = 'fudate',
    timetypedata = 'ymd',
    timetypeoutput = 'months',
    outcome = 'outcome',
    fixed_effects = c('fixed_effects1', 'fixed_effects2', 'fixed_effects3'),
    continuous_effects = c('continuous_effects1', 'continuous_effects2', 'continuous_effects3'),
    cluster_var = 'cluster_var',
    random_effects = 'intercept',
    random_slope_var = 'random_slope_var',
    nested_clustering = FALSE,
    nested_cluster_var = 'nested_cluster_var',
    correlation_structure = 'unstructured',
    sparse_matrix = TRUE,
    optimization_method = 'penalized',
    likelihood_ratio_test = TRUE,
    random_effects_significance = TRUE,
    icc_calculation = TRUE,
    residual_analysis = FALSE,
    influence_diagnostics = FALSE,
    random_effects_prediction = FALSE,
    fixed_effects_plot = TRUE,
    random_effects_plot = FALSE,
    cluster_survival_plot = FALSE,
    n_clusters_plot = 5,
    variance_components = TRUE,
    confidence_intervals = TRUE,
    bootstrap_variance = FALSE,
    bootstrap_samples = 100,
    show_fixed_effects = TRUE,
    show_random_effects = TRUE,
    show_model_comparison = TRUE,
    show_cluster_summary = FALSE,
    showSummaries = FALSE,
    showExplanations = FALSE,
    addClusterEffects = FALSE,
    addFittedValues = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'mixedcox.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

