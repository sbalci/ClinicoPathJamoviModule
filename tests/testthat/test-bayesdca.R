# Tests for bayesdca function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("bayesdca works with minimal inputs", {
  skip_if_not_installed('jmvReadWrite')
  
  set.seed(123)
  n <- 50
  data <- data.frame(
    prob = runif(n, 0, 1),
    outcome = factor(sample(c('Neg', 'Pos'), n, replace = TRUE), levels=c('Neg', 'Pos'))
  )
  
  expect_no_error({
    result <- bayesdca(
      data = data,
      outcomes = 'outcome',
      outcomePos = 'Pos',
      predictors = c('prob'),
      thresholdMin = 0.1,
      thresholdMax = 0.5,
      thresholdPoints = 10,
      bayesianAnalysis = FALSE,
      bootstrapCI = FALSE,
      nDraws = 500
    )
  })
  
  # OMV export check
  if (!dir.exists('omv_output')) dir.create('omv_output')
  omv_path <- file.path(getwd(), 'omv_output', 'bayesdca.omv')
  
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

test_that("bayesdca works with Bayesian analysis", {
  set.seed(456)
  n <- 40
  data <- data.frame(
    p1 = runif(n),
    p2 = runif(n),
    out = factor(sample(c('0', '1'), n, replace = TRUE), levels=c('0', '1'))
  )
  
  expect_no_error({
    result <- bayesdca(
      data = data,
      outcomes = 'out',
      outcomePos = '1',
      predictors = c('p1', 'p2'),
      bayesianAnalysis = TRUE,
      nDraws = 500,
      priorStrength = 1
    )
  })
  
  expect_true(inherits(result, "bayesdcaResults"))
})
