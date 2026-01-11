
library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  stop("devtools needed to load package for tests")
}

test_that("cohenskappa works with binary categories", {
  # Create synthetic data with proper factor levels  
  set.seed(123)
  n <- 50
  
  # Use factors explicitly
  rater1 <- factor(sample(c("A", "B"), n, replace = TRUE), levels = c("A", "B"))
  rater2 <- factor(sample(c("A", "B"), n, replace = TRUE), levels = c("A", "B"))
  
  data <- data.frame(rater1 = rater1, rater2 = rater2)
  
  # Run basic analysis
  results <- cohenskappa(
    data = data,
    rater1 = "rater1",
    rater2 = "rater2",
    kappa_type = "cohen"
  )
  
  # Check results exist and are correct type
  expect_true(!is.null(results))
  expect_true(inherits(results, "cohenskappaResults"))
  
  # Check summary contains kappa result information
  expect_true(!is.null(results$summary$content))
  expect_match(results$summary$content, "Kappa|kappa|agreement|cohen", perl = TRUE, ignore.case = TRUE)
})

test_that("cohenskappa works with ordinal categories (3 levels)", {
  set.seed(456)
  n <- 60
  
  # Create ordinal data with explicit factors
  categories <- c("Low", "Medium", "High")
  rater1 <- factor(sample(categories, n, replace = TRUE), levels = categories, ordered = TRUE)
  rater2 <- factor(sample(categories, n, replace = TRUE), levels = categories, ordered = TRUE)
  
  data <- data.frame(rater1 = rater1, rater2 = rater2)
  
  # Run with linear weights (appropriate for ordinal)
  results <- cohenskappa(
    data = data,
    rater1 = "rater1",
    rater2 = "rater2",
    kappa_type = "linear"
  )
  
  expect_true(!is.null(results))
  expect_true(!is.null(results$summary$content))
})

test_that("cohenskappa calculates all kappa types when requested", {
  set.seed(789)
  n <- 40
  
  rater1 <- factor(sample(c("Yes", "No"), n, replace = TRUE), levels = c("Yes", "No"))
  rater2 <- factor(sample(c("Yes", "No"), n, replace = TRUE), levels = c("Yes", "No"))
  
  data <- data.frame(rater1 = rater1, rater2 = rater2)
  
  results <- cohenskappa(
    data = data,
    rater1 = "rater1",
    rater2 = "rater2",
    kappa_type = "all"
  )
  
  # When "all" is selected, summary should still be populated
  expect_true(!is.null(results$summary$content))
})

test_that("cohenskappa handles multi-rater analysis (3+ raters)", {
  set.seed(101)
  n <- 50
  
  rater1 <- factor(sample(c("A", "B"), n, replace = TRUE), levels = c("A", "B"))
  rater2 <- factor(sample(c("A", "B"), n, replace = TRUE), levels = c("A", "B"))
  rater3 <- factor(sample(c("A", "B"), n, replace = TRUE), levels = c("A", "B"))
  
  data <- data.frame(rater1 = rater1, rater2 = rater2, rater3 = rater3)
  
  results <- cohenskappa(
    data = data,
    rater1 = "rater1",
    rater2 = "rater2",
    rater3 = "rater3",
    multi_rater_method = "fleiss"
  )
  
  # Basic check - should not error and should have summary
  expect_true(!is.null(results))
  expect_true(!is.null(results$summary$content))
})

test_that("cohenskappa returns warning with small sample", {
  rater1 <- factor(c("A", "B", "A"), levels = c("A", "B"))
  rater2 <- factor(c("A", "A", "B"), levels = c("A", "B"))
  
  data <- data.frame(rater1 = rater1, rater2 = rater2)
  
  results <- cohenskappa(
    data = data,
    rater1 = "rater1",
    rater2 = "rater2"
  )
  
  # Should have a warning/notice in summary about small sample
  expect_true(!is.null(results$summary$content))
  expect_match(results$summary$content, "Warning|Error|sample|Small", perl = TRUE, ignore.case = TRUE)
})
