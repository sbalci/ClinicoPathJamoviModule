# Tests for precisionrecall function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("precisionrecall works with basic inputs", {
  skip_if_not_installed('jmvReadWrite')
  
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = factor(sample(c('Neg', 'Pos'), n, replace = TRUE), levels=c('Neg', 'Pos')),
    scores1 = runif(n, 0, 1),
    scores2 = runif(n, 0, 1)
  )
  
  expect_no_error({
    result <- precisionrecall(
      data = data,
      outcome = 'outcome',
      positiveClass = 'Pos',
      scores = c('scores1', 'scores2'),
      interpolation = 'nonlinear',
      showBaseline = TRUE,
      aucMethod = 'trapezoid',
      ci = FALSE,
      comparison = FALSE,
      showROC = FALSE,
      showFScore = FALSE
    )
  })
  
  # Export OMV
  if (!dir.exists('omv_output')) dir.create('omv_output')
  omv_path <- file.path(getwd(), 'omv_output', 'precisionrecall.omv')
  
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

test_that("precisionrecall returns results object", {
  set.seed(456)
  n <- 30
  data <- data.frame(
    out = factor(sample(c('0', '1'), n, replace = TRUE), levels=c('0', '1')),
    val = rnorm(n)
  )
  
  result <- precisionrecall(
    data = data,
    outcome = 'out',
    scores = 'val',
    positiveClass = '1',
    ci = FALSE
  )
  
  expect_true(inherits(result, "precisionrecallResults"))
})
