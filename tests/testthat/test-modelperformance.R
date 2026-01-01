
test_that('modelperformance analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    timeVar = runif(n, 1, 100),
    model1vars1 = sample(c('A', 'B'), n, replace = TRUE),
    model1vars2 = sample(c('A', 'B'), n, replace = TRUE),
    model1vars3 = sample(c('A', 'B'), n, replace = TRUE),
    model2vars1 = sample(c('A', 'B'), n, replace = TRUE),
    model2vars2 = sample(c('A', 'B'), n, replace = TRUE),
    model2vars3 = sample(c('A', 'B'), n, replace = TRUE),
    model3vars1 = sample(c('A', 'B'), n, replace = TRUE),
    model3vars2 = sample(c('A', 'B'), n, replace = TRUE),
    model3vars3 = sample(c('A', 'B'), n, replace = TRUE),
    model4vars1 = sample(c('A', 'B'), n, replace = TRUE),
    model4vars2 = sample(c('A', 'B'), n, replace = TRUE),
    model4vars3 = sample(c('A', 'B'), n, replace = TRUE),
    model5vars1 = sample(c('A', 'B'), n, replace = TRUE),
    model5vars2 = sample(c('A', 'B'), n, replace = TRUE),
    model5vars3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- modelperformance(
      data = data,
    modelType = 'cox',
    outcome = 'outcome',
    timeVar = 'timeVar',
    model1vars = c('model1vars1', 'model1vars2', 'model1vars3'),
    model2vars = c('model2vars1', 'model2vars2', 'model2vars3'),
    model3vars = c('model3vars1', 'model3vars2', 'model3vars3'),
    model4vars = c('model4vars1', 'model4vars2', 'model4vars3'),
    model5vars = c('model5vars1', 'model5vars2', 'model5vars3'),
    showAIC = TRUE,
    showRSquared = TRUE,
    showCIndex = TRUE,
    showLogLik = FALSE,
    showMCC = TRUE,
    crossValidation = FALSE,
    cvFolds = 5,
    showForestPlot = TRUE,
    showROC = FALSE,
    showCalibration = FALSE,
    autoRecommend = TRUE,
    recommendBy = 'aic'
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'modelperformance.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

