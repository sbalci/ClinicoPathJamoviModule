
test_that('splinehazard analysis works', {
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
    model <- splinehazard(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    explanatory = c('explanatory1', 'explanatory2', 'explanatory3'),
    knots_method = 'automatic',
    num_knots = 3,
    spline_degree = '3',
    hazard_scale = 'log',
    confidence_level = 0.95,
    show_hazard_plot = TRUE,
    show_survival_plot = TRUE,
    show_cumulative_plot = FALSE,
    show_model_comparison = TRUE,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'splinehazard.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

