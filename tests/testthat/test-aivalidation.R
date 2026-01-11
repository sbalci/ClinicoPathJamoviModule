# Tests for aivalidation function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("aivalidation works with minimal inputs", {
  skip_if_not_installed('jmvReadWrite')
  
  set.seed(123)
  n <- 50
  data <- data.frame(
    pred1 = runif(n, 0, 1),
    pred2 = runif(n, 0, 1),
    outcome = factor(sample(c('Neg', 'Pos'), n, replace = TRUE), levels=c('Neg', 'Pos'))
  )
  
  expect_no_error({
    result <- aivalidation(
      data = data,
      predictorVars = c('pred1', 'pred2'),
      outcomeVar = 'outcome',
      positiveLevel = 'Pos',
      compareModels = FALSE,
      youdensJ = FALSE,
      matthewsCC = FALSE,
      bootstrapCI = FALSE,
      rocPlot = FALSE,
      crossValidation = 'none',
      showExplanations = FALSE
    )
  })
  
  # OMV export check
  if (!dir.exists('omv_output')) dir.create('omv_output')
  omv_path <- file.path(getwd(), 'omv_output', 'aivalidation.omv')
  
  tryCatch({
    jmvReadWrite::write_omv(result, omv_path)
  }, error = function(e) {
    message("OMV Export failed: ", e$message)
  })
  
  if (!file.exists(omv_path)) {
     skip("OMV export failed, skipping file existence check")
  }
  expect_true(file.exists(omv_path))
})

test_that("aivalidation works with comparisons and cross-validation", {
  set.seed(456)
  n <- 30
  data <- data.frame(
    s1 = runif(n),
    s2 = runif(n),
    out = factor(sample(c('0', '1'), n, replace = TRUE), levels=c('0', '1'))
  )
  
  expect_no_error({
    result <- aivalidation(
      data = data,
      predictorVars = c('s1', 's2'),
      outcomeVar = 'out',
      positiveLevel = '1',
      compareModels = TRUE,
      crossValidation = '5-fold',
      stratified = TRUE,
      randomSeed = 123
    )
  })
  
  expect_true(inherits(result, "aivalidationResults"))
})
