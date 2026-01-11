
test_that("Helper functions are available", {
  devtools::load_all()
  
  expect_true(exists("validateDecisionAnalysisInputs"))
  expect_true(is.function(validateDecisionAnalysisInputs))
  
  expect_true(exists("createDecisionTreePlot"))
  expect_true(is.function(createDecisionTreePlot))
  
  expect_true(exists("calculateMarkovTransitionMatrix"))
  expect_true(is.function(calculateMarkovTransitionMatrix))
})
