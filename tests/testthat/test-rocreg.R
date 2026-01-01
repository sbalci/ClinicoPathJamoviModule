
test_that('rocreg analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    predictor = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- rocreg(
      data = data,
    predictor = 'predictor',
    outcome = 'outcome',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    covariate_effects = 'both',
    method = 'binormal',
    adjusted_auc = TRUE,
    covariate_specific_auc = TRUE,
    compare_unadjusted = TRUE,
    test_covariate_effects = TRUE,
    variable_selection = FALSE,
    selection_criterion = 'aic',
    test_interactions = FALSE,
    confidence_intervals = TRUE,
    ci_method = 'bootstrap',
    bootstrap_samples = 100,
    confidence_level = 0.95,
    plot_covariate_specific_rocs = TRUE,
    plot_auc_by_covariate = TRUE,
    plot_effect_estimates = TRUE,
    plot_residual_diagnostics = FALSE,
    adjustment_purpose = 'confounder',
    clinical_application = 'general',
    show_interpretation = TRUE,
    standardize_covariates = TRUE,
    missing_handling = 'complete',
    random_seed = 123
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'rocreg.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

