
test_that('principalcox analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    highdim_vars1 = runif(n, 1, 100),
    highdim_vars2 = runif(n, 1, 100),
    highdim_vars3 = runif(n, 1, 100),
    clinical_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    clinical_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    clinical_vars3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- principalcox(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    highdim_vars = c('highdim_vars1', 'highdim_vars2', 'highdim_vars3'),
    clinical_vars = c('clinical_vars1', 'clinical_vars2', 'clinical_vars3'),
    pca_method = 'standard',
    n_components = 5,
    variance_threshold = 0.95,
    component_selection = 'fixed_number',
    scaling_method = 'standardize',
    sparse_parameter = 0.1,
    cross_validation = 5,
    confidence_level = 0.95,
    show_pca_summary = TRUE,
    show_component_loadings = TRUE,
    show_cox_results = TRUE,
    show_variable_importance = FALSE,
    show_model_comparison = FALSE,
    scree_plot = TRUE,
    biplot = FALSE,
    loading_plot = TRUE,
    survival_plot = TRUE,
    showSummaries = TRUE,
    showExplanations = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'principalcox.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

