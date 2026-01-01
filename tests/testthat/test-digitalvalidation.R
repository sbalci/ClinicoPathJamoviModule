
test_that('digitalvalidation analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    reference = runif(n, 1, 100),
    test = runif(n, 1, 100),
    platform_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- digitalvalidation(
      data = data,
    reference = 'reference',
    test = 'test',
    platform_var = 'platform_var',
    validation_type = 'both',
    acceptance_criteria = 'fda_standard',
    custom_correlation_threshold = 0.9,
    custom_icc_threshold = 0.75,
    bias_assessment = TRUE,
    decision_threshold1 = 0,
    decision_threshold2 = 0,
    decision_threshold3 = 0,
    bootstrap_ci = TRUE,
    bootstrap_n = 1000,
    show_validation_plots = TRUE,
    generate_report = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'digitalvalidation.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

