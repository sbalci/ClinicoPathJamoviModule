
test_that('directregression analysis works', {
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
    explanatory3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- directregression(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    explanatory = c('explanatory1', 'explanatory2', 'explanatory3'),
    regression_type = 'linear',
    link_function = 'identity',
    confidence_level = 0.95,
    bootstrap_se = FALSE,
    bootstrap_reps = 500,
    pseudo_method = 'jackknife',
    show_pseudo_values = FALSE,
    show_residuals = FALSE,
    model_comparison = TRUE,
    survival_plot = TRUE,
    residual_plot = FALSE,
    prediction_plot = TRUE,
    showSummaries = TRUE,
    showExplanations = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'directregression.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

