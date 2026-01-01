
test_that('diagnosticsamplesize analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(

  )

  # Run analysis
  expect_no_error({
    model <- diagnosticsamplesize(
      data = data,
    study_purpose = 'diagnostic',
    target_sensitivity = 0.9,
    target_specificity = 0.9,
    prevalence = 0.1,
    ci_width = 0.1,
    alpha = 0.05,
    nonresponse_rate = 0,
    show_statement = TRUE,
    show_methodology = TRUE,
    show_references = FALSE,
    show_comparison_table = FALSE,
    show_method_comparison = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'diagnosticsamplesize.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

