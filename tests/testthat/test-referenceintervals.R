
test_that('referenceintervals analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    measurement = runif(n, 1, 100),
    age = runif(n, 1, 100),
    gender = sample(c('A', 'B'), n, replace = TRUE),
    ethnicity = sample(c('A', 'B'), n, replace = TRUE),
    additional_factors1 = sample(c('A', 'B'), n, replace = TRUE),
    additional_factors2 = sample(c('A', 'B'), n, replace = TRUE),
    additional_factors3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- referenceintervals(
      data = data,
    measurement = 'measurement',
    age = 'age',
    gender = 'gender',
    ethnicity = 'ethnicity',
    additional_factors = c('additional_factors1', 'additional_factors2', 'additional_factors3'),
    ri_method = 'robust_nonparametric',
    confidence_level = 0.95,
    minimum_sample_size = 120,
    outlier_detection = 'horn',
    transformation_test = TRUE,
    partitioning_analysis = TRUE,
    age_partitioning = 'none',
    harris_boyd_test = TRUE,
    verification_study = FALSE,
    transferability_assessment = FALSE,
    uncertainty_estimation = TRUE,
    clinical_interpretation = TRUE,
    quality_assessment = TRUE,
    literature_comparison = FALSE,
    distribution_plots = TRUE,
    partitioning_plots = TRUE,
    age_trend_plots = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'referenceintervals.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

