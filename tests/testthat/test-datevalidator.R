
test_that('datevalidator analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    date_vars1 = runif(n, 1, 100),
    date_vars2 = runif(n, 1, 100),
    date_vars3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- datevalidator(
      data = data,
    date_vars = c('date_vars1', 'date_vars2', 'date_vars3'),
    correction_method = 'datefixr',
    date_format = 'auto',
    day_impute = 1,
    month_impute = 7,
    handle_excel = FALSE,
    show_correction_table = FALSE,
    show_quality_assessment = FALSE,
    show_format_analysis = FALSE,
    show_correction_summary = FALSE,
    show_interpretation = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'datevalidator.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

