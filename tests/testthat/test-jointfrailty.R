
test_that('jointfrailty analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    subjectID = sample(c('A', 'B'), n, replace = TRUE),
    time = runif(n, 1, 100),
    event = sample(c('A', 'B'), n, replace = TRUE),
    terminal_time = runif(n, 1, 100),
    terminal_event = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    recurrent_covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    recurrent_covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    recurrent_covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    terminal_covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    terminal_covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    terminal_covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- jointfrailty(
      data = data,
    subjectID = 'subjectID',
    time = 'time',
    event = 'event',
    terminal_time = 'terminal_time',
    terminal_event = 'terminal_event',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    recurrent_covariates = c('recurrent_covariates1', 'recurrent_covariates2', 'recurrent_covariates3'),
    terminal_covariates = c('terminal_covariates1', 'terminal_covariates2', 'terminal_covariates3'),
    frailty_distribution = 'gamma',
    recurrent_baseline = 'weibull',
    terminal_baseline = 'weibull',
    association_type = 'shared_frailty',
    confidence_level = 0.95,
    max_iterations = 100,
    convergence_tolerance = 1e-8,
    include_predictions = TRUE,
    include_residuals = TRUE,
    plotRecurrentHazard = TRUE,
    plotTerminalHazard = TRUE,
    plotFrailtyDistribution = TRUE,
    plotSurvivalPredictions = FALSE,
    plotResiduals = FALSE,
    showEducation = TRUE,
    showInterpretation = TRUE,
    exportResults = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'jointfrailty.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

