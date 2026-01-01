
test_that('treemedical analysis works', {
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

  # Run analysis
  expect_no_error({
    model <- treemedical(
      data = data,
    vars = c('vars1', 'vars2', 'vars3'),
    facs = c('facs1', 'facs2', 'facs3'),
    target = 'target',
    validation = 'cv',
    cv_folds = 5,
    bootstrap_samples = 50,
    stratified_sampling = TRUE,
    holdout_split = 0.75,
    handle_missing = 'remove',
    max_depth = 5,
    min_samples_split = 20,
    cost_complexity = 0.01,
    use_1se_rule = TRUE,
    clinical_context = 'diagnosis',
    cost_sensitive = FALSE,
    fn_fp_ratio = 2,
    show_tree_plot = TRUE,
    show_performance_metrics = TRUE,
    show_confusion_matrix = TRUE,
    show_importance_plot = TRUE,
    show_clinical_interpretation = TRUE,
    set_seed = TRUE,
    seed_value = 42
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'treemedical.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

