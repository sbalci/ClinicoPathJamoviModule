
test_that('mediansurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    explanatory = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- mediansurvival(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    explanatory = 'explanatory',
    confidence_method = 'brookmeyer_crowley',
    confidence_level = 0.95,
    test_method = 'logrank',
    multiple_comparison = 'holm',
    show_survival_plot = TRUE,
    show_median_lines = TRUE,
    show_confidence_bands = TRUE,
    show_risk_table = FALSE,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'mediansurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

