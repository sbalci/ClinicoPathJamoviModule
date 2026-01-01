
test_that('haralicktexture analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    texture_features1 = runif(n, 1, 100),
    texture_features2 = runif(n, 1, 100),
    texture_features3 = runif(n, 1, 100),
    x_coord = runif(n, 1, 100),
    y_coord = runif(n, 1, 100),
    group_var = sample(c('A', 'B'), n, replace = TRUE),
    outcome_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- haralicktexture(
      data = data,
    texture_features = c('texture_features1', 'texture_features2', 'texture_features3'),
    x_coord = 'x_coord',
    y_coord = 'y_coord',
    group_var = 'group_var',
    outcome_var = 'outcome_var',
    analysis_focus = 'comprehensive',
    feature_selection = 'all',
    correlation_threshold = 0.9,
    normality_testing = TRUE,
    outlier_detection = TRUE,
    outlier_method = 'iqr',
    show_distribution_plots = TRUE,
    show_correlation_plot = TRUE,
    show_spatial_plot = FALSE,
    texture_interpretation = 'clinical',
    biomarker_context = 'general'
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'haralicktexture.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

