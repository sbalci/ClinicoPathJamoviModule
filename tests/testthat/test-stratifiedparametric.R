
test_that('stratifiedparametric analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    strata_variable = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- stratifiedparametric(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    strata_variable = 'strata_variable',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    parametric_distribution = 'weibull',
    baseline_specification = 'separate_baselines',
    spline_df = 3,
    knot_placement = 'quantiles',
    test_stratification = TRUE,
    confidence_level = 0.95,
    show_model_summary = TRUE,
    show_coefficients = TRUE,
    show_stratification_test = TRUE,
    show_survival_curves = TRUE,
    show_hazard_curves = TRUE,
    show_comparison_plot = FALSE,
    show_diagnostics = TRUE,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'stratifiedparametric.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

