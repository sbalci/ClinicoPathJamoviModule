
test_that('functionalsampling analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    x_coord = runif(n, 1, 100),
    y_coord = runif(n, 1, 100),
    event_type = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- functionalsampling(
      data = data,
    x_coord = 'x_coord',
    y_coord = 'y_coord',
    event_type = 'event_type',
    frequency_threshold = 5,
    calculate_nnd = TRUE,
    test_randomness = TRUE,
    analyze_neighborhoods = TRUE,
    show_summary = TRUE,
    show_plot = TRUE,
    show_methodology = TRUE,
    show_references = FALSE,
    significance_level = 0.05
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'functionalsampling.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

