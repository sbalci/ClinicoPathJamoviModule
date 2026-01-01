
test_that('spatialanalysis analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    coords_x = runif(n, 1, 100),
    coords_y = runif(n, 1, 100),
    cell_types = sample(c('A', 'B'), n, replace = TRUE),
    groups = sample(c('A', 'B'), n, replace = TRUE),
    roi_id = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- spatialanalysis(
      data = data,
    coords_x = 'coords_x',
    coords_y = 'coords_y',
    cell_types = 'cell_types',
    groups = 'groups',
    roi_id = 'roi_id',
    perform_ripley = TRUE,
    perform_nnd = TRUE,
    perform_hotspot = TRUE,
    perform_interaction = TRUE,
    show_plots = TRUE,
    analysis_scope = 'comprehensive',
    distance_method = 'euclidean',
    correction_method = 'both',
    significance_level = 0.05,
    min_points = 10
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'spatialanalysis.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

