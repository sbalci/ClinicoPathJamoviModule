
test_that('flexparametric analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    grouping_variable = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- flexparametric(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    model_approach = 'automatic',
    distribution = 'gengamma',
    spline_type = 'hazard',
    spline_df = 4,
    knot_placement = 'quantiles',
    confidence_level = 0.95,
    grouping_variable = 'grouping_variable',
    model_comparison = TRUE,
    time_varying_effects = FALSE,
    show_parameters = TRUE,
    show_aic_bic = TRUE,
    show_survival_plot = TRUE,
    show_hazard_plot = FALSE,
    show_spline_plot = FALSE,
    show_diagnostics = TRUE,
    show_clinical_summary = TRUE,
    show_density_plot = FALSE,
    plot_time_max = 0,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'flexparametric.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

