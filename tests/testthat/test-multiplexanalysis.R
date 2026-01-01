
test_that('multiplexanalysis analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    marker_vars1 = runif(n, 1, 100),
    marker_vars2 = runif(n, 1, 100),
    marker_vars3 = runif(n, 1, 100),
    x_coord = runif(n, 1, 100),
    y_coord = runif(n, 1, 100),
    cell_type_var = sample(c('A', 'B'), n, replace = TRUE),
    sample_id_var = sample(c('A', 'B'), n, replace = TRUE),
    group_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- multiplexanalysis(
      data = data,
    marker_vars = c('marker_vars1', 'marker_vars2', 'marker_vars3'),
    x_coord = 'x_coord',
    y_coord = 'y_coord',
    cell_type_var = 'cell_type_var',
    sample_id_var = 'sample_id_var',
    group_var = 'group_var',
    analysis_focus = 'comprehensive',
    correlation_method = 'pearson',
    positivity_cutpoint = 0.1,
    perform_clustering = TRUE,
    n_clusters = 0,
    perform_pca = TRUE,
    show_loadings = TRUE,
    immune_contexture = FALSE,
    diversity_metrics = TRUE,
    spatial_analysis = FALSE,
    proximity_threshold = 20,
    show_plots = TRUE,
    show_clustering_plots = TRUE,
    biomarker_panel = 'custom',
    normalization_method = 'zscore',
    multiple_testing = 'fdr',
    confidence_level = 0.95
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'multiplexanalysis.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

