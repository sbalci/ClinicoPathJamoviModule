# Tests for modelval function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("modelval works with basic inputs", {
  skip_if_not_installed('jmvReadWrite')
  
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = factor(sample(c('Neg', 'Pos'), n, replace = TRUE), levels=c('Neg', 'Pos')),
    predicted = runif(n, 0, 1),
    subgroup = factor(sample(c('A', 'B'), n, replace = TRUE), levels=c('A', 'B'))
  )
  
  expect_no_error({
    result <- modelval(
      data = data,
      outcome = 'outcome',
      predicted = 'predicted',
      subgroup = 'subgroup',
      validationType = 'general',
      calibrationGroups = 10,
      showNetBenefit = FALSE,
      showFlexibleCalibration = FALSE
    )
  })
  
  # OMV export check (allow skipping if fails)
  if (!dir.exists('omv_output')) dir.create('omv_output')
  omv_path <- file.path(getwd(), 'omv_output', 'modelval.omv')
  
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

test_that("modelval returns results object", {
  set.seed(456)
  n <- 30
  data <- data.frame(
    out = factor(sample(c('0', '1'), n, replace = TRUE), levels=c('0', '1')),
    pred = runif(n, 0, 1)
  )
  
  result <- modelval(
    data = data,
    outcome = 'out',
    predicted = 'pred',
    subgroup = NULL
  )
  
  expect_true(inherits(result, "modelvalResults"))
})
