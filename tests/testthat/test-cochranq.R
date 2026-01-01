
test_that('cochranq analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    variables1 = sample(c('A', 'B'), n, replace = TRUE),
    variables2 = sample(c('A', 'B'), n, replace = TRUE),
    variables3 = sample(c('A', 'B'), n, replace = TRUE),
    id = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- cochranq(
      data = data,
    variables = c('variables1', 'variables2', 'variables3'),
    id = 'id',
    method = 'asymptotic',
    alpha = 0.05,
    posthoc = TRUE,
    correction = 'bonferroni',
    effect_size = TRUE,
    confidence_level = 0.95,
    show_pattern = TRUE,
    show_assumptions = TRUE,
    clinical_interpretation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'cochranq.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

