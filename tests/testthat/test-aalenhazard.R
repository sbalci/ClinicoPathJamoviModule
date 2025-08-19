test_that("aalenhazard loads correctly", {
  # Test that the function can be loaded
  expect_true(requireNamespace("jmvcore", quietly = TRUE))
  
  # Source the files
  source(file.path("..", "..", "R", "aalenhazard.h.R"))
  source(file.path("..", "..", "R", "aalenhazard.b.R"))
  
  # Test that the class exists
  expect_true(exists("aalenhazardClass"))
  expect_true(methods::is(aalenhazardClass, "R6ClassGenerator"))
})

test_that("aalenhazard handles basic input validation", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("timereg")
  
  # Source the files
  source(file.path("..", "..", "R", "aalenhazard.h.R"))
  source(file.path("..", "..", "R", "aalenhazard.b.R"))
  
  # Create test data with survival structure
  set.seed(123)
  n <- 100
  
  test_data <- data.frame(
    time = rexp(n, 0.1),
    event = rbinom(n, 1, 0.7),
    age = rnorm(n, 60, 10),
    sex = factor(sample(c("M", "F"), n, replace = TRUE)),
    treatment = factor(sample(c("A", "B"), n, replace = TRUE))
  )
  
  # Test that the analysis can be created (without running)
  expect_no_error({
    analysis <- aalenhazard(
      data = test_data,
      elapsedtime = "time", 
      outcome = "event",
      covariates = c("age", "sex", "treatment")
    )
  })
})