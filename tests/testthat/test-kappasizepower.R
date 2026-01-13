
library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  stop("devtools needed to load package for tests")
}

test_that("kappaSizePower works for binary outcomes (2 categories)", {
  # Basic binary case: 2 raters, 2 categories
  results <- kappaSizePower(
    outcome = "2",
    kappa0 = 0.40,
    kappa1 = 0.60,
    props = "0.30, 0.70",
    raters = "2",
    alpha = 0.05,
    power = 0.80
  )
  
  # Check if results are populated
  expect_true(!is.null(results$text1$content))
  expect_true(!is.null(results$text2$content))
  
  # Check content format - should contain a result (not an error)
  content1 <- results$text1$content
  expect_true(nchar(content1) > 0)
  
  # Explanation should contain key parameters
  content2 <- results$text2$content
  expect_match(content2, "POWER ANALYSIS", fixed = FALSE)
  expect_match(content2, "Number of raters: 2", fixed = FALSE)
})

test_that("kappaSizePower works for 3 categories", {
  results <- kappaSizePower(
    outcome = "3",
    kappa0 = 0.50,
    kappa1 = 0.70,
    props = "0.20, 0.30, 0.50",
    raters = "3",
    alpha = 0.05,
    power = 0.80
  )
  
  expect_true(nchar(results$text1$content) > 0)
  expect_match(results$text2$content, "Outcome categories: 3")
  expect_match(results$text2$content, "Number of raters: 3")
})

test_that("kappaSizePower validates kappa1 > kappa0", {
  expect_error(kappaSizePower(
      outcome = "2",
      kappa0 = 0.60,
      kappa1 = 0.40,
      props = "0.5, 0.5",
      power = 0.80
  ), "kappa1 must be greater")
})

test_that("kappaSizePower handles proportion validation", {
  expect_error(kappaSizePower(
      outcome = "2",
      kappa0 = 0.4,
      kappa1 = 0.6,
      props = "0.20, 0.20",
      power = 0.80
  ), "Proportions must sum to 1")
})

test_that("kappaSizePower validates power parameter", {
  expect_error(kappaSizePower(
      outcome = "2",
      kappa0 = 0.4,
      kappa1 = 0.6,
      props = "0.5, 0.5",
      power = 0.30
  ), "Power should be at least 0.5")
})
