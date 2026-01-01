
test_that('rmstregression analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    explanatory1 = sample(c('A', 'B'), n, replace = TRUE),
    explanatory2 = sample(c('A', 'B'), n, replace = TRUE),
    explanatory3 = sample(c('A', 'B'), n, replace = TRUE),
    group_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- rmstregression(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    explanatory = c('explanatory1', 'explanatory2', 'explanatory3'),
    group_var = 'group_var',
    tau = 60,
    tau_method = 'fixed',
    tau_percentile = 0.8,
    analysis_type = 'regression',
    regression_method = 'pseudo_observation',
    confidence_level = 0.95,
    bootstrap_se = FALSE,
    bootstrap_reps = 1000,
    adjustment_method = 'none',
    show_rmst_table = TRUE,
    show_difference_table = TRUE,
    show_regression_table = TRUE,
    show_model_diagnostics = FALSE,
    rmst_plot = TRUE,
    difference_plot = FALSE,
    residual_plot = FALSE,
    showSummaries = TRUE,
    showExplanations = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'rmstregression.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

