
test_that('pagetrendtest analysis works', {
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
    model <- pagetrendtest(
      data = data,
    dependent = 'dependent',
    subject = 'subject',
    within = 'within',
    trend_direction = 'increasing',
    method = 'asymptotic',
    alpha = 0.05,
    effect_size = TRUE,
    confidence_level = 0.95,
    show_ranks = TRUE,
    show_descriptives = TRUE,
    friedman_comparison = TRUE,
    show_assumptions = TRUE,
    clinical_interpretation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'pagetrendtest.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

