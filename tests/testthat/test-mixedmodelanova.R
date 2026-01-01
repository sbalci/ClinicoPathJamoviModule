
test_that('mixedmodelanova analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    dependent = runif(n, 1, 100),
    fixed_factors1 = sample(c('A', 'B'), n, replace = TRUE),
    fixed_factors2 = sample(c('A', 'B'), n, replace = TRUE),
    fixed_factors3 = sample(c('A', 'B'), n, replace = TRUE),
    random_factors1 = sample(c('A', 'B'), n, replace = TRUE),
    random_factors2 = sample(c('A', 'B'), n, replace = TRUE),
    random_factors3 = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = runif(n, 1, 100),
    covariates2 = runif(n, 1, 100),
    covariates3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- mixedmodelanova(
      data = data,
    dependent = 'dependent',
    fixed_factors = c('fixed_factors1', 'fixed_factors2', 'fixed_factors3'),
    random_factors = c('random_factors1', 'random_factors2', 'random_factors3'),
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    model_type = 'random_intercept',
    interaction_terms = FALSE,
    estimation_method = 'reml',
    show_fixed_effects = TRUE,
    show_random_effects = TRUE,
    show_model_fit = TRUE,
    show_assumptions = TRUE,
    show_posthoc = FALSE,
    posthoc_method = 'tukey',
    confidence_level = 0.95,
    show_effect_sizes = TRUE,
    show_icc = TRUE,
    show_plots = TRUE,
    show_methodology = FALSE,
    show_references = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'mixedmodelanova.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

