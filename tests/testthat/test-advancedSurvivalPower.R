
test_that('advancedSurvivalPower analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(

  )

  # Run analysis
  expect_no_error({
    model <- advancedSurvivalPower(
      data = data,
    calc_type = 'sample_size',
    study_design = 'simple',
    hazard_ratio = 0.7,
    power = 0.8,
    alpha = 0.05,
    sample_size = 200,
    allocation_ratio = 1,
    prob_event = 0.5,
    accrual_time = 1,
    follow_up_time = 3,
    median_survival = 5,
    loss_followup = 0.05
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'advancedSurvivalPower.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

