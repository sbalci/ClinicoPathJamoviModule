
test_that('conditionalinference analysis works', {
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
    strata = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- conditionalinference(
      data = data,
    time = 'time',
    event = 'event',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    strata = 'strata',
    teststat = 'quad',
    testtype = 'Bonferroni',
    mincriterion = 0.95,
    minsplit = 20,
    minbucket = 7,
    maxdepth = 5,
    nresample = 9999,
    logrank_scores = TRUE,
    show_splits = TRUE,
    show_nodes = TRUE,
    show_importance = TRUE,
    plot_tree = TRUE,
    plot_survival = TRUE,
    plot_importance = FALSE,
    mtry = 1,
    replace = FALSE,
    subset = 1
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'conditionalinference.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

