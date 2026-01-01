
test_that('advancedimputation analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    imputation_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    imputation_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    imputation_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    auxiliary_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    auxiliary_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    auxiliary_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    cluster_var = sample(c('A', 'B'), n, replace = TRUE),
    id_var = sample(c('A', 'B'), n, replace = TRUE),
    level1_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    level1_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    level1_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    level2_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    level2_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    level2_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    exclude_vars1 = runif(n, 1, 100),
    exclude_vars2 = runif(n, 1, 100),
    exclude_vars3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- advancedimputation(
      data = data,
    imputation_vars = c('imputation_vars1', 'imputation_vars2', 'imputation_vars3'),
    auxiliary_vars = c('auxiliary_vars1', 'auxiliary_vars2', 'auxiliary_vars3'),
    cluster_var = 'cluster_var',
    id_var = 'id_var',
    n_imputations = 10,
    n_iterations = 10,
    convergence_check = TRUE,
    imputation_method = 'pmm',
    categorical_method = 'logreg',
    mnar_methods = FALSE,
    mnar_type = 'delta_adjustment',
    sensitivity_analysis = TRUE,
    sensitivity_methods = 'all',
    multilevel_imputation = FALSE,
    level1_vars = c('level1_vars1', 'level1_vars2', 'level1_vars3'),
    level2_vars = c('level2_vars1', 'level2_vars2', 'level2_vars3'),
    min_bucket_size = 5,
    exclude_vars = c('exclude_vars1', 'exclude_vars2', 'exclude_vars3'),
    ridge_penalty = 1e-05,
    remove_collinear = TRUE,
    collinearity_threshold = 0.95,
    diagnostic_plots = TRUE,
    imputation_quality = TRUE,
    cross_validation = FALSE,
    ampute_test = FALSE,
    save_imputations = FALSE,
    pool_results = TRUE,
    show_detailed_output = TRUE,
    regulatory_report = FALSE,
    random_seed = 123
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'advancedimputation.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

