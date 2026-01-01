
test_that('roc2d analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    marker1 = runif(n, 1, 100),
    marker2 = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- roc2d(
      data = data,
    marker1 = 'marker1',
    marker2 = 'marker2',
    outcome = 'outcome',
    decision_rule = 'linear',
    compare_single_markers = TRUE,
    plot_2d_roc_surface = TRUE,
    plot_threshold_region = TRUE,
    show_interpretation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'roc2d.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

