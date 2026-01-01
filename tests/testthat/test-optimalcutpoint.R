
test_that('optimalcutpoint analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    biomarker = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    time_var = runif(n, 1, 100),
    status_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- optimalcutpoint(
      data = data,
    biomarker = 'biomarker',
    analysis_type = 'binary',
    outcome = 'outcome',
    time_var = 'time_var',
    status_var = 'status_var',
    cutpoint_method = 'youden',
    bootstrap_validation = FALSE,
    bootstrap_runs = 1000,
    cross_validation = FALSE,
    cv_folds = 10,
    performance_metrics = TRUE,
    survival_analysis = TRUE,
    confidence_level = 0.95,
    multiple_testing = 'miller_siegmund',
    plot_roc = TRUE,
    plot_distribution = TRUE,
    plot_survival = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'optimalcutpoint.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

