
test_that('categoricaladvanced analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    rows = sample(c('A', 'B'), n, replace = TRUE),
    cols = sample(c('A', 'B'), n, replace = TRUE),
    stratify = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- categoricaladvanced(
      data = data,
    rows = 'rows',
    cols = 'cols',
    stratify = 'stratify',
    test_type = 'enhanced',
    fisher_exact = TRUE,
    effect_sizes = TRUE,
    association_measures = TRUE,
    residual_analysis = TRUE,
    posthoc_comparisons = FALSE,
    correction_method = 'bonferroni',
    confidence_level = 0.95,
    exact_threshold = 50,
    simulation_runs = 5000,
    show_expected = TRUE,
    plot_mosaic = TRUE,
    plot_residuals = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'categoricaladvanced.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

