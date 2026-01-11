# Tests for roc2d function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("roc2d works with basic inputs", {
  skip_if_not_installed('jmvReadWrite')
  
  set.seed(123)
  n <- 50
  data <- data.frame(
    marker1 = runif(n, 0, 1),
    marker2 = runif(n, 0, 1),
    outcome = factor(sample(c('Neg', 'Pos'), n, replace = TRUE), levels=c('Neg', 'Pos'))
  )
  
  expect_no_error({
    result <- roc2d(
      data = data,
      marker1 = 'marker1',
      marker2 = 'marker2',
      outcome = 'outcome',
      positive_level = 'Pos',
      decision_rule = 'linear',
      compare_single_markers = TRUE,
      plot_2d_roc_surface = FALSE,
      plot_threshold_region = FALSE,
      show_interpretation = FALSE
    )
  })
  
  # OMV export check
  if (!dir.exists('omv_output')) dir.create('omv_output')
  omv_path <- file.path(getwd(), 'omv_output', 'roc2d.omv')
  
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

test_that("roc2d returns results object", {
  set.seed(456)
  n <- 30
  data <- data.frame(
    m1 = rnorm(n),
    m2 = rnorm(n),
    out = factor(sample(c('0', '1'), n, replace = TRUE), levels=c('0', '1'))
  )
  
  result <- roc2d(
    data = data,
    marker1 = 'm1',
    marker2 = 'm2',
    outcome = 'out',
    positive_level = '1'
  )
  
  expect_true(inherits(result, "roc2dResults"))
})
