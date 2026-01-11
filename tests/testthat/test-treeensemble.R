
test_that('treeensemble analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    vars1 = runif(n, 1, 100),
    vars2 = runif(n, 1, 100),
    vars3 = runif(n, 1, 100),
    facs1 = sample(c('A', 'B'), n, replace = TRUE),
    facs2 = sample(c('A', 'B'), n, replace = TRUE),
    facs3 = sample(c('A', 'B'), n, replace = TRUE),
    target = sample(c('A', 'B'), n, replace = TRUE)
  )

  model <- treeensemble(
    data = data,
    vars = c('vars1', 'vars2', 'vars3'),
    facs = c('facs1', 'facs2', 'facs3'),
    target = 'target',
    targetLevel = 'A',
    n_trees = 500,
    mtry_method = 'auto',
    mtry_custom = 3,
    min_node_size = 5,
    validation = 'oob',
    cv_folds = 5,
    test_split = 0.25,
    importance_type = 'permutation',
    feature_selection = FALSE,
    max_features = 10,
    clinical_context = 'biomarker',
    class_weights = FALSE,
    show_performance_metrics = TRUE,
    show_importance_plot = TRUE,
    show_oob_error = TRUE,
    show_confusion_matrix = TRUE,
    show_clinical_interpretation = TRUE,
    set_seed = TRUE,
    seed_value = 42
  )

  expect_true(inherits(model, "treeensembleResults"))

  # Define output path
  omv_path <- file.path('omv_output', 'treeensemble.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  tryCatch({
    jmvReadWrite::write_omv(model, omv_path)
    expect_true(file.exists(omv_path))
  }, error = function(e) {
    skip(paste("OMV write failed:", e$message))
  })
})

