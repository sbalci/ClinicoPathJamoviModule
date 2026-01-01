
test_that('vusanalysis analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    predictor = runif(n, 1, 100),
    multiclass_outcome = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    stratify_by = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- vusanalysis(
      data = data,
    predictor = 'predictor',
    multiclass_outcome = 'multiclass_outcome',
    vus_method = 'mann_whitney',
    confidence_intervals = TRUE,
    ci_method = 'bootstrap',
    bootstrap_samples = 100,
    confidence_level = 0.95,
    hypothesis_test = TRUE,
    test_method = 'bootstrap',
    pairwise_auc = TRUE,
    pairwise_ci = TRUE,
    pairwise_tests = FALSE,
    min_class_size = 10,
    covariate_adjustment = FALSE,
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    stratified_analysis = FALSE,
    stratify_by = 'stratify_by',
    plot_3d_surface = FALSE,
    plot_pairwise_rocs = TRUE,
    plot_class_distributions = TRUE,
    plot_type = 'boxplot',
    handle_ties = 'average',
    random_seed = 12345
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'vusanalysis.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

