
test_that('cohenskappa analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    rater1 = sample(c('A', 'B'), n, replace = TRUE),
    rater2 = sample(c('A', 'B'), n, replace = TRUE),
    rater3 = sample(c('A', 'B'), n, replace = TRUE),
    rater4 = sample(c('A', 'B'), n, replace = TRUE),
    rater5 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- cohenskappa(
      data = data,
    rater1 = 'rater1',
    rater2 = 'rater2',
    kappa_type = 'cohen',
    confidence_level = 0.95,
    ci_method = 'asymptotic',
    bootstrap_samples = 100,
    exact_agreement = TRUE,
    marginal_homogeneity = TRUE,
    agreement_plot = TRUE,
    confusion_matrix = TRUE,
    category_analysis = FALSE,
    interpretation_guide = TRUE,
    missing_treatment = 'listwise',
    minimum_categories = 2,
    rater3 = 'rater3',
    rater4 = 'rater4',
    rater5 = 'rater5',
    multi_rater_method = 'auto',
    show_pairwise_kappa = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'cohenskappa.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

