test_that("highdimcox loads correctly", {
  # Test that the function can be loaded
  expect_true(requireNamespace("jmvcore", quietly = TRUE))
  
  # Source the files
  source(file.path("..", "..", "R", "highdimcox.h.R"))
  source(file.path("..", "..", "R", "highdimcox.b.R"))
  
  # Test that the class exists
  expect_true(exists("highdimcoxClass"))
  expect_true(methods::is(highdimcoxClass, "R6ClassGenerator"))
})

test_that("highdimcox handles basic input validation", {
  skip_if_not_installed("jmvcore")
  
  # Source the files
  source(file.path("..", "..", "R", "highdimcox.h.R"))
  source(file.path("..", "..", "R", "highdimcox.b.R"))
  
  # Create test data
  set.seed(123)
  n <- 100
  p <- 50
  
  test_data <- data.frame(
    time = rexp(n, 0.1),
    event = rbinom(n, 1, 0.6),
    matrix(rnorm(n * p), n, p, dimnames = list(NULL, paste0("X", 1:p)))
  )
  
  # Test that the analysis can be created (without running)
  expect_no_error({
    analysis <- highdimcox(
      data = test_data,
      elapsedtime = "time", 
      outcome = "event",
      predictors = paste0("X", 1:10)
    )
  })
})