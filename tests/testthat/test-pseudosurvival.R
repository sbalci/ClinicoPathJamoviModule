
test_that('pseudosurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time_var = runif(n, 1, 100),
    status_var = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    cluster_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- pseudosurvival(
      data = data,
    time_var = 'time_var',
    status_var = 'status_var',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    stratification_vars = c('stratification_vars1', 'stratification_vars2', 'stratification_vars3'),
    analysis_type = 'survival_probability',
    tau_rmst = 60,
    jackknife_method = 'standard',
    regression_method = 'ols',
    cluster_var = 'cluster_var',
    confidence_level = 0.95,
    bootstrap_samples = 100,
    robust_se = FALSE,
    model_diagnostics = TRUE,
    prediction_intervals = FALSE,
    survival_curves = TRUE,
    rmst_comparison = TRUE,
    sensitivity_analysis = FALSE,
    competing_risks = FALSE,
    weighted_analysis = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'pseudosurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

