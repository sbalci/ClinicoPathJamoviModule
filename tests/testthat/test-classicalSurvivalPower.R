
test_that('classicalSurvivalPower analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(

  )

  # Run analysis
  expect_no_error({
    model <- classicalSurvivalPower(
      data = data,
    calculation_type = 'sample_size',
    method = 'lachin_foulkes',
    hazard_control = 0.083,
    hazard_treatment = 0.042,
    hazard_ratio = 0.6,
    study_duration = 24,
    accrual_duration = 12,
    dropout_rate = 0,
    allocation_ratio = 1,
    alpha = 0.025,
    beta = 0.1,
    power = 0.9,
    sided = 'one_sided',
    entry_type = 'unif',
    gamma = 0,
    sample_size_input = 100,
    events_input = 50,
    show_summary = TRUE,
    show_formulas = FALSE,
    show_interpretation = TRUE,
    show_power_plot = FALSE,
    show_timeline_plot = FALSE,
    export_results = FALSE,
    export_power_curve = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'classicalSurvivalPower.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

