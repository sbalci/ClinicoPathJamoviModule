
test_that('friedmantest analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    dependent = sample(c('A', 'B'), n, replace = TRUE),
    subject = sample(c('A', 'B'), n, replace = TRUE),
    within = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- friedmantest(
      data = data,
    dependent = 'dependent',
    subject = 'subject',
    within = 'within',
    method = 'asymptotic',
    alpha = 0.05,
    posthoc = TRUE,
    posthoc_method = 'nemenyi',
    correction = 'bonferroni',
    effect_size = TRUE,
    confidence_level = 0.95,
    show_ranks = TRUE,
    show_descriptives = TRUE,
    show_assumptions = TRUE,
    clinical_interpretation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'friedmantest.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

