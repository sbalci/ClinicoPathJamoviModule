
test_that('clinicaltrialdesign analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(

  )

  # Run analysis
  expect_no_error({
    model <- clinicaltrialdesign(
      data = data,
    trial_type = 'superiority',
    outcome_type = 'continuous',
    test_type = 'two_sample_ttest',
    calculation_type = 'sample_size',
    alpha = 0.05,
    power = 0.8,
    sample_size = 100,
    effect_size = 0.5,
    mean_difference = 1,
    common_sd = 2,
    proportion1 = 0.3,
    proportion2 = 0.5,
    allocation_ratio = 1,
    two_sided = TRUE,
    continuity_correction = TRUE,
    margin = 0.1,
    margin_type = 'absolute',
    dropout_rate = 10,
    interim_analyses = 0,
    multiple_comparisons = 'none',
    show_assumptions = TRUE,
    show_interpretation = TRUE,
    show_sensitivity = TRUE,
    show_plots = TRUE,
    regulatory_context = 'ich'
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'clinicaltrialdesign.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

