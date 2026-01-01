
test_that('effectsize analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    dep = runif(n, 1, 100),
    group = sample(c('A', 'B'), n, replace = TRUE),
    pairing_variable = sample(c('A', 'B'), n, replace = TRUE),
    study_weights = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- effectsize(
      data = data,
    dep = 'dep',
    group = 'group',
    analysisType = 'two_sample',
    measures_cohens_d = TRUE,
    measures_hedges_g = TRUE,
    measures_glass_delta = FALSE,
    testValue = 0,
    ciWidth = 95,
    interpretation = TRUE,
    descriptives = TRUE,
    testDetails = TRUE,
    assumptionChecks = FALSE,
    plotEffects = TRUE,
    plotType = 'forest',
    plotWidth = 600,
    plotHeight = 400,
    measures_eta_squared = FALSE,
    measures_partial_eta_squared = FALSE,
    measures_omega_squared = FALSE,
    measures_epsilon_squared = FALSE,
    measures_cramers_v = FALSE,
    measures_phi_coefficient = FALSE,
    measures_cohens_w = FALSE,
    measures_rank_biserial = FALSE,
    measures_cliff_delta = FALSE,
    measures_vargha_delaney_a = FALSE,
    measures_common_language = FALSE,
    measures_cohens_u3 = FALSE,
    measures_probability_superiority = FALSE,
    analysis_context = 'ttest',
    correction_method = 'none',
    bootstrap_ci = FALSE,
    bootstrap_samples = 100,
    pooled_sd = TRUE,
    welch_correction = FALSE,
    paired_analysis = FALSE,
    pairing_variable = 'pairing_variable',
    clinical_thresholds = FALSE,
    small_effect_threshold = 0.2,
    medium_effect_threshold = 0.5,
    large_effect_threshold = 0.8,
    power_analysis = FALSE,
    alpha_level = 0.05,
    desired_power = 0.8,
    meta_analysis_format = FALSE,
    study_weights = 'study_weights',
    bluesky_integration = TRUE,
    comprehensive_output = FALSE,
    effect_size_family = 'smd',
    forest_plot_advanced = FALSE,
    effect_size_distribution = FALSE,
    comparison_plot = FALSE,
    export_format = 'standard'
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'effectsize.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

