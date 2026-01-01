
test_that('parametricfrailty analysis works', {
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
    frailty_variable = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- parametricfrailty(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    frailty_variable = 'frailty_variable',
    baseline_distribution = 'weibull',
    frailty_distribution = 'gamma',
    estimation_method = 'penalized_likelihood',
    include_se = TRUE,
    include_ci = TRUE,
    conf_level = 0.95,
    frailty_variance = TRUE,
    frailty_predictions = FALSE,
    gof_tests = FALSE,
    plot_hazard = FALSE,
    plot_survival = FALSE,
    plot_frailty = FALSE,
    plot_diagnostics = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'parametricfrailty.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

