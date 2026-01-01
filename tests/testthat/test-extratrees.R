
test_that('extratrees analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time = runif(n, 1, 100),
    event = sample(c('A', 'B'), n, replace = TRUE),
    predictors1 = sample(c('A', 'B'), n, replace = TRUE),
    predictors2 = sample(c('A', 'B'), n, replace = TRUE),
    predictors3 = sample(c('A', 'B'), n, replace = TRUE),
    strata = sample(c('A', 'B'), n, replace = TRUE),
    case_weights = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- extratrees(
      data = data,
    time = 'time',
    event = 'event',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    strata = 'strata',
    num_trees = 500,
    mtry = 'sqrt',
    mtry_custom = 1,
    min_node_size = 10,
    max_depth = 0,
    splitrule = 'extratrees',
    num_random_splits = 1,
    sample_fraction = 1,
    replace = TRUE,
    case_weights = 'case_weights',
    importance = 'permutation',
    scale_permutation = TRUE,
    keep_inbag = FALSE,
    oob_error = TRUE,
    probability = FALSE,
    show_forest_summary = TRUE,
    show_importance = TRUE,
    show_oob_predictions = FALSE,
    plot_importance = TRUE,
    plot_oob_error = TRUE,
    plot_survival = FALSE,
    plot_partial = FALSE,
    regularization = 1,
    alpha = 0.05,
    num_threads = 0,
    random_seed = 123
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'extratrees.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

