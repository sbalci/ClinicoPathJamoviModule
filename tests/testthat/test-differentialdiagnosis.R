# Tests for differentialdiagnosis function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("differentialdiagnosis works with basic inputs", {
  set.seed(123)
  n <- 50
  
  data <- data.frame(
    symptom1 = factor(sample(c('Absent', 'Present'), n, replace=TRUE), levels=c('Absent', 'Present')),
    symptom2 = factor(sample(c('Absent', 'Present'), n, replace=TRUE), levels=c('Absent', 'Present')),
    diagnosis = factor(sample(c('D1', 'D2', 'D3'), n, replace=TRUE))
  )
  
  expect_no_error({
    result <- differentialdiagnosis(
      data = data,
      clinicalFindings = c('symptom1', 'symptom2'),
      confirmedDiagnosis = 'diagnosis',
      diagnostic_probability = TRUE,
      likelihood_ratios = TRUE,
      demographicVars = NULL,
      labResults = NULL,
      imagingFindings = NULL
    )
  })
})

test_that("differentialdiagnosis returns results object", {
  set.seed(456)
  n <- 30
  
  data <- data.frame(
    f1 = factor(sample(c('0', '1'), n, replace=TRUE)),
    d = factor(sample(c('A', 'B'), n, replace=TRUE))
  )
  
  result <- differentialdiagnosis(
     data = data,
     clinicalFindings = 'f1',
     confirmedDiagnosis = 'd',
     demographicVars = NULL,
     labResults = NULL,
     imagingFindings = NULL
  )
  
  expect_true(inherits(result, "differentialdiagnosisResults"))
})
