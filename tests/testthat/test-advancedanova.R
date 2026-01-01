
test_that('advancedanova analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    dependent = runif(n, 1, 100),
    fixed1 = sample(c('A', 'B'), n, replace = TRUE),
    fixed2 = sample(c('A', 'B'), n, replace = TRUE),
    fixed3 = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = runif(n, 1, 100),
    covariates2 = runif(n, 1, 100),
    covariates3 = runif(n, 1, 100),
    wls = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- advancedanova(
      data = data,
    dependent = 'dependent',
    fixed = c('fixed1', 'fixed2', 'fixed3'),
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    wls = 'wls',
    model_type = 'oneway',
    posthoc_method = 'tukey',
    assumptions = TRUE,
    effect_sizes = TRUE,
    descriptives = TRUE,
    show_plots = TRUE,
    plot_type = 'both',
    confidence_level = 0.95,
    alpha_level = 0.05,
    welch_correction = FALSE,
    robust_anova = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'advancedanova.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

