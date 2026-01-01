
test_that('timevarycox analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    timevar_data1 = sample(c('A', 'B'), n, replace = TRUE),
    timevar_data2 = sample(c('A', 'B'), n, replace = TRUE),
    timevar_data3 = sample(c('A', 'B'), n, replace = TRUE),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    fixed_covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    fixed_covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    fixed_covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    subject_id = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- timevarycox(
      data = data,
    elapsedtime = 'elapsedtime',
    timevar_data = c('timevar_data1', 'timevar_data2', 'timevar_data3'),
    outcome = 'outcome',
    fixed_covariates = c('fixed_covariates1', 'fixed_covariates2', 'fixed_covariates3'),
    subject_id = 'subject_id',
    robust_se = TRUE,
    cluster_se = TRUE,
    show_model_summary = TRUE,
    show_coefficients = TRUE,
    show_hazard_ratios = TRUE,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'timevarycox.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

