# Tests for treemedical function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("treemedical works with basic inputs", {
  set.seed(123)
  n <- 100
  data <- data.frame(
    vars1 = runif(n, 1, 100),
    vars2 = runif(n, 1, 100),
    facs1 = factor(sample(c('A', 'B'), n, replace = TRUE)),
    target = factor(sample(c('Positive', 'Negative'), n, replace = TRUE))
  )
  
  expect_no_error({
    result <- treemedical(
      data = data,
      vars = c('vars1', 'vars2'),
      facs = 'facs1',
      target = 'target',
      targetLevel = 'Positive',
      validation = 'cv',
      cv_folds = 5,
      max_depth = 5,
      clinical_context = 'diagnosis'
    )
  })
})

test_that("treemedical returns results object", {
  set.seed(456)
  n <- 80
  data <- data.frame(
    age = rnorm(n, 60, 10),
    marker = rnorm(n, 5, 2),
    stage = factor(sample(c('I', 'II', 'III'), n, replace = TRUE)),
    diagnosis = factor(sample(c('Cancer', 'Benign'), n, replace = TRUE))
  )
  
  result <- treemedical(
    data = data,
    vars = c('age', 'marker'),
    facs = 'stage',
    target = 'diagnosis',
    targetLevel = 'Cancer',
    validation = 'holdout',
    holdout_split = 0.75,
    show_tree_plot = FALSE,
    show_performance_metrics = TRUE
  )
  
  expect_true(inherits(result, "treemedicalResults"))
  expect_true(!is.null(result$performance_table))
})

test_that("treemedical handles cost sensitive learning", {
  set.seed(789)
  n <- 100
  data <- data.frame(
    x = rnorm(n),
    y = factor(sample(c(0, 1), n, replace = TRUE))
  )
  
  result <- treemedical(
    data = data,
    vars = 'x',
    target = 'y',
    targetLevel = '1',
    cost_sensitive = TRUE,
    fn_fp_ratio = 5
  )
  
  expect_true(inherits(result, "treemedicalResults"))
})
