
test_that('condsurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time = runif(n, 1, 100),
    status = sample(c('A', 'B'), n, replace = TRUE),
    group = sample(c('A', 'B'), n, replace = TRUE),
    id = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- condsurvival(
      data = data,
    time = 'time',
    status = 'status',
    group = 'group',
    id = 'id',
    survival_method = 'kaplan_meier',
    parametric_dist = 'weibull',
    confidence_level = 0.95,
    time_scale = 'years',
    include_plots = TRUE,
    include_tables = TRUE,
    dynamic_prediction = FALSE,
    landmark_analysis = FALSE,
    clinical_interpretation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'condsurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

