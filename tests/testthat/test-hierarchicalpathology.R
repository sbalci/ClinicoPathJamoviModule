
test_that('hierarchicalpathology analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    dependent = runif(n, 1, 100),
    level_3 = sample(c('A', 'B'), n, replace = TRUE),
    level_2 = sample(c('A', 'B'), n, replace = TRUE),
    level_1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- hierarchicalpathology(
      data = data,
    dependent = 'dependent',
    level_3 = 'level_3',
    level_2 = 'level_2',
    level_1 = 'level_1',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    model_type = 'linear',
    descriptives = TRUE,
    variance_components = TRUE,
    icc_analysis = TRUE,
    random_effects = TRUE,
    model_comparison = FALSE,
    diagnostics = TRUE,
    confidence_level = 0.95,
    optimizer = 'bobyqa',
    max_iterations = 100
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'hierarchicalpathology.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

