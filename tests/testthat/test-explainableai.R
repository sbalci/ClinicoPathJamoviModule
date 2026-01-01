
test_that('explainableai analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    features1 = sample(c('A', 'B'), n, replace = TRUE),
    features2 = sample(c('A', 'B'), n, replace = TRUE),
    features3 = sample(c('A', 'B'), n, replace = TRUE),
    target_var = sample(c('A', 'B'), n, replace = TRUE),
    model_predictions = runif(n, 1, 100),
    image_paths = sample(c('A', 'B'), n, replace = TRUE),
    attention_maps = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- explainableai(
      data = data,
    analysis_type = 'feature_importance',
    features = c('features1', 'features2', 'features3'),
    target_var = 'target_var',
    model_predictions = 'model_predictions',
    image_paths = 'image_paths',
    attention_maps = 'attention_maps',
    shap_method = 'kernel_explainer',
    lime_method = 'tabular',
    n_samples = 100,
    n_features = 20,
    plot_type = 'summary',
    overlay_original = TRUE,
    confidence_level = 0.95,
    background_samples = 100,
    perturbation_method = 'random',
    clustering_method = 'none',
    interaction_analysis = FALSE,
    local_explanations = TRUE,
    global_explanations = TRUE,
    save_explanations = FALSE,
    attention_threshold = 0.1,
    colormap = 'viridis'
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'explainableai.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

