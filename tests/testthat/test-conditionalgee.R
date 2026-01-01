
test_that('conditionalgee analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    subjectID = sample(c('A', 'B'), n, replace = TRUE),
    gap_time = runif(n, 1, 100),
    event = sample(c('A', 'B'), n, replace = TRUE),
    event_number = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    time_varying_covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    time_varying_covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    time_varying_covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    baseline_covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    baseline_covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    baseline_covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- conditionalgee(
      data = data,
    subjectID = 'subjectID',
    gap_time = 'gap_time',
    event = 'event',
    event_number = 'event_number',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    time_varying_covariates = c('time_varying_covariates1', 'time_varying_covariates2', 'time_varying_covariates3'),
    baseline_covariates = c('baseline_covariates1', 'baseline_covariates2', 'baseline_covariates3'),
    distribution_family = 'weibull',
    correlation_structure = 'exchangeable',
    link_function = 'log',
    conditioning_set = 'previous_gap',
    confidence_level = 0.95,
    max_iterations = 50,
    tolerance = 1e-8,
    robust_se = TRUE,
    include_diagnostics = TRUE,
    include_predictions = TRUE,
    plotGapTimes = TRUE,
    plotCorrelation = TRUE,
    plotResiduals = FALSE,
    plotPredictions = FALSE,
    showEducation = TRUE,
    showInterpretation = TRUE,
    exportResults = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'conditionalgee.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

