
test_that('permutationsurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    explanatory = sample(c('A', 'B'), n, replace = TRUE),
    stratify_variable = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- permutationsurvival(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    explanatory = 'explanatory',
    permutation_method = 'approximate',
    n_permutations = 10000,
    test_statistic = 'logrank',
    stratify_variable = 'stratify_variable',
    confidence_level = 0.95,
    multiple_comparison = 'holm',
    show_permutation_distribution = FALSE,
    show_survival_curves = TRUE,
    show_test_progression = FALSE,
    seed_value = 123,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'permutationsurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

