# Tests for decisionpanel function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("decisionpanel works with basic inputs", {
  set.seed(123)
  n <- 50
  
  data <- data.frame(
    t1 = factor(sample(c('Neg', 'Pos'), n, replace=TRUE), levels=c('Neg', 'Pos')),
    t2 = factor(sample(c('Neg', 'Pos'), n, replace=TRUE), levels=c('Neg', 'Pos')),
    gold = factor(sample(c('No', 'Yes'), n, replace=TRUE), levels=c('No', 'Yes'))
  )
  
  expect_no_error({
    result <- decisionpanel(
      data = data,
      test1 = 't1',
      test2 = 't2',
      test3 = NULL,
      test4 = NULL,
      test5 = NULL,
      test1Positive = 'Pos',
      test2Positive = 'Pos',
      test3Positive = NULL,
      test4Positive = NULL,
      test5Positive = NULL,
      gold = 'gold',
      goldPositive = 'Yes',
      strategies = 'all',
      optimizationCriteria = 'accuracy'
    )
  })
})

test_that("decisionpanel returns results object", {
  set.seed(456)
  n <- 30
  
  data <- data.frame(
    tA = factor(sample(c('0', '1'), n, replace=TRUE), levels=c('0', '1')),
    tB = factor(sample(c('0', '1'), n, replace=TRUE), levels=c('0', '1')),
    D = factor(sample(c('Absent', 'Present'), n, replace=TRUE), levels=c('Absent', 'Present'))
  )
  
  result <- decisionpanel(
     data = data,
     test1 = 'tA',
     test2 = 'tB',
     test3 = NULL,
     test4 = NULL,
     test5 = NULL,
     test1Positive = '1',
     test2Positive = '1',
     test3Positive = NULL,
     test4Positive = NULL,
     test5Positive = NULL,
     gold = 'D',
     goldPositive = 'Present',
     strategies = 'single'
  )
  
  expect_true(inherits(result, "decisionpanelResults"))
})
