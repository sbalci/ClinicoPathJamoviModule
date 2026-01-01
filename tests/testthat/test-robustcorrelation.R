
test_that('robustcorrelation analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    dep1 = runif(n, 1, 100),
    dep2 = runif(n, 1, 100),
    dep3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- robustcorrelation(
      data = data,
    dep = c('dep1', 'dep2', 'dep3'),
    method = 'spearman',
    matrix_type = 'upper',
    show_pvalues = TRUE,
    sig_level = 0.05,
    p_adjust_method = 'none',
    outlier_detection = FALSE,
    outlier_method = 'mcd',
    outlier_threshold = 2.5,
    bootstrap_ci = FALSE,
    n_bootstrap = 1000,
    confidence_level = 0.95,
    show_heatmap = TRUE,
    heatmap_colors = 'blue_white_red',
    show_diagnostics = FALSE,
    plot_width = 600,
    plot_height = 450,
    decimal_places = 3
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'robustcorrelation.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

