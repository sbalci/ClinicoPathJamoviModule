
test_that('modalitycomparison analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    modality1_var = sample(c('A', 'B'), n, replace = TRUE),
    modality2_var = sample(c('A', 'B'), n, replace = TRUE),
    case_id = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- modalitycomparison(
      data = data,
    modality1_var = 'modality1_var',
    modality2_var = 'modality2_var',
    case_id = 'case_id',
    show_discordance_analysis = TRUE,
    score_categories = 'auto',
    show_contingency_table = TRUE,
    calculate_weighted_kappa = FALSE,
    confidence_intervals = TRUE,
    directional_analysis = TRUE,
    low_end_focus = FALSE,
    show_plots = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'modalitycomparison.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

