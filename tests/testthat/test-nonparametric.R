
test_that('nonparametric analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    deps1 = runif(n, 1, 100),
    deps2 = runif(n, 1, 100),
    deps3 = runif(n, 1, 100),
    outcome = runif(n, 1, 100),
    groups = sample(c('A', 'B'), n, replace = TRUE),
    paired_variable = sample(c('A', 'B'), n, replace = TRUE),
    blocking_variable = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- nonparametric(
      data = data,
    deps = c('deps1', 'deps2', 'deps3'),
    outcome = 'outcome',
    groups = 'groups',
    paired_variable = 'paired_variable',
    blocking_variable = 'blocking_variable',
    test_type = 'mann_whitney',
    effect_size = TRUE,
    effect_size_method = 'eta_squared',
    confidence_intervals = TRUE,
    confidence_level = 0.95,
    post_hoc = TRUE,
    post_hoc_method = 'dunn',
    p_adjustment = 'holm',
    robust_method = 'standard',
    trim_proportion = 0.1,
    winsorize_proportion = 0.1,
    bootstrap_ci = FALSE,
    bootstrap_samples = 100,
    test_assumptions = TRUE,
    normality_tests = TRUE,
    assumption_checks = TRUE,
    homogeneity_test = 'levene',
    exact_test = FALSE,
    exact_p_values = TRUE,
    continuity_correction = TRUE,
    tie_correction = TRUE,
    ties_method = 'average',
    show_boxplots = TRUE,
    show_violin_plots = FALSE,
    show_rank_plots = FALSE,
    show_effect_plots = TRUE,
    descriptive_plots = TRUE,
    show_qqplots = FALSE,
    show_descriptives = TRUE,
    show_test_statistics = TRUE,
    show_post_hoc_table = TRUE,
    show_effect_sizes = TRUE,
    show_assumptions = TRUE,
    show_robust_statistics = FALSE,
    show_power_analysis = FALSE,
    show_instructions = TRUE,
    show_explanations = FALSE,
    show_interpretation = FALSE,
    show_recommendations = FALSE,
    clinical_context = 'general',
    set_seed = TRUE,
    seed_value = 42,
    missing_data_handling = 'listwise',
    alpha_level = 0.05,
    minimum_sample_size = 5,
    outlier_method = 'iqr',
    small_sample_exact = TRUE,
    report_standardized_statistics = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'nonparametric.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

