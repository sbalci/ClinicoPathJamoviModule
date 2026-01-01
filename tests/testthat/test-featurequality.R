
test_that('featurequality analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    features1 = runif(n, 1, 100),
    features2 = runif(n, 1, 100),
    features3 = runif(n, 1, 100),
    group_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- featurequality(
      data = data,
    features = c('features1', 'features2', 'features3'),
    group_var = 'group_var',
    analysis_scope = 'comprehensive',
    missing_data_analysis = TRUE,
    distribution_analysis = TRUE,
    outlier_detection = TRUE,
    outlier_method = 'multiple',
    outlier_threshold = 3,
    correlation_analysis = TRUE,
    correlation_threshold = 0.8,
    variance_analysis = TRUE,
    low_variance_threshold = 0.01,
    normality_testing = TRUE,
    normality_method = 'multiple',
    skewness_analysis = TRUE,
    feature_importance = FALSE,
    importance_method = 'random_forest',
    data_transformation = TRUE,
    quality_score = TRUE,
    detailed_plots = TRUE,
    plot_distributions = TRUE,
    plot_correlations = TRUE,
    plot_outliers = TRUE,
    plot_missing = TRUE,
    export_recommendations = FALSE,
    clinical_context = TRUE,
    batch_processing = TRUE,
    confidence_level = 0.95,
    random_seed = 123
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'featurequality.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

