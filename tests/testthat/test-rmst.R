
test_that('rmst analysis works', {
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
    model <- rmst(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    explanatory = 'explanatory',
    tau_method = 'auto',
    tau_value = 365,
    tau_percentile = 80,
    confidence_level = 0.95,
    bootstrap_ci = FALSE,
    bootstrap_n = 1000,
    show_rmst_table = TRUE,
    show_rmst_plot = TRUE,
    show_difference_test = TRUE,
    show_ratio_test = FALSE,
    show_tau_analysis = FALSE,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'rmst.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

