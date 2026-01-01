
test_that('pcacox analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time = runif(n, 1, 100),
    status = sample(c('A', 'B'), n, replace = TRUE),
    predictors1 = runif(n, 1, 100),
    predictors2 = runif(n, 1, 100),
    predictors3 = runif(n, 1, 100),
    clinical_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    clinical_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    clinical_vars3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- pcacox(
      data = data,
    time = 'time',
    status = 'status',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    clinical_vars = c('clinical_vars1', 'clinical_vars2', 'clinical_vars3'),
    pca_method = 'supervised',
    n_components = 5,
    component_selection = 'cv',
    cv_folds = 10,
    variance_threshold = 0.8,
    scaling = TRUE,
    centering = TRUE,
    survival_weighting = TRUE,
    permutation_test = FALSE,
    n_permutations = 100,
    bootstrap_validation = TRUE,
    n_bootstrap = 100,
    plot_scree = TRUE,
    plot_loadings = TRUE,
    plot_biplot = TRUE,
    plot_survival = TRUE,
    risk_score = TRUE,
    pathway_analysis = FALSE,
    feature_importance = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'pcacox.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

