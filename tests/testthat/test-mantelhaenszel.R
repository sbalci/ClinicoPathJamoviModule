
test_that('mantelhaenszel analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    rows = sample(c('A', 'B'), n, replace = TRUE),
    cols = sample(c('A', 'B'), n, replace = TRUE),
    strata = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- mantelhaenszel(
      data = data,
    rows = 'rows',
    cols = 'cols',
    strata = 'strata',
    measure = 'or',
    alpha = 0.05,
    show_stratum_tables = TRUE,
    show_homogeneity_tests = TRUE,
    show_crude_analysis = TRUE,
    continuity_correction = TRUE,
    show_methodology = FALSE,
    show_references = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'mantelhaenszel.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

