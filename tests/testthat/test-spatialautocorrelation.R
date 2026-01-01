
test_that('spatialautocorrelation analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    measurement = runif(n, 1, 100),
    x_coordinate = runif(n, 1, 100),
    y_coordinate = runif(n, 1, 100),
    region_id = sample(c('A', 'B'), n, replace = TRUE),
    time_point = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- spatialautocorrelation(
      data = data,
    measurement = 'measurement',
    x_coordinate = 'x_coordinate',
    y_coordinate = 'y_coordinate',
    region_id = 'region_id',
    time_point = 'time_point',
    autocorr_method = 'morans_i',
    spatial_weights = 'k_nearest',
    distance_threshold = 50,
    k_neighbors = 8,
    bandwidth = 25,
    significance_test = 'permutation_test',
    permutations = 999,
    confidence_level = 0.95,
    local_analysis = TRUE,
    cluster_detection = TRUE,
    hotspot_analysis = TRUE,
    spatial_regimes = FALSE,
    temporal_analysis = FALSE,
    multivariate_analysis = FALSE,
    robustness_check = TRUE,
    edge_effects = TRUE,
    clinical_interpretation = TRUE,
    spatial_plots = TRUE,
    autocorr_plots = TRUE,
    lisa_plots = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'spatialautocorrelation.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

