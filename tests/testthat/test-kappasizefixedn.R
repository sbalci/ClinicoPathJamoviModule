
library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  stop("devtools needed to load package for tests")
}

test_that("kappaSizeFixedN works for binary outcomes (2 categories)", {
  # Basic binary case: 2 raters, 2 categories, n=100
  results <- kappaSizeFixedN(
    outcome = "2",
    kappa0 = 0.60,
    props = "0.30, 0.70",
    raters = "2",
    alpha = 0.05,
    n = 100
  )
  
  # Check if results are populated
  expect_true(!is.null(results$text1$content))
  expect_true(!is.null(results$text2$content))
  
  # Check content format - should contain a numeric result
  content1 <- results$text1$content
  expect_true(nchar(content1) > 0)
  
  # Explanation should contain key parameters
  content2 <- results$text2$content
  expect_match(content2, "Outcome categories: 2", fixed = FALSE)
  expect_match(content2, "Number of raters: 2", fixed = FALSE)
  expect_match(content2, "Sample size: 100", fixed = FALSE)
})

test_that("kappaSizeFixedN works for 3 categories", {
  results <- kappaSizeFixedN(
    outcome = "3",
    kappa0 = 0.50,
    props = "0.20, 0.30, 0.50",
    raters = "3",
    alpha = 0.05,
    n = 150
  )
  
  expect_true(nchar(results$text1$content) > 0)
  expect_match(results$text2$content, "Outcome categories: 3")
  expect_match(results$text2$content, "Number of raters: 3")
})

test_that("kappaSizeFixedN handles validation errors: proportions", {
  expect_error(kappaSizeFixedN(
      outcome = "2",
      props = "0.20, 0.20",
      n = 100
  ), "Proportions must sum to 1")
})

test_that("kappaSizeFixedN handles validation errors: matched categories", {
  expect_error(kappaSizeFixedN(
      outcome = "3",
      props = "0.5, 0.5",
      n = 100
  ), "Expected 3 proportions")
})

test_that("kappaSizeFixedN integration with kappaSize package calculations", {
    results <- kappaSizeFixedN(
        outcome = "2",
        kappa0 = 0.6,
        props = "0.5, 0.5",
        raters = "2",
        alpha = 0.05,
        n = 100
    )
    
    output_str <- results$text1$content
    expect_true(nchar(output_str) > 0)
    expect_match(results$text2$content, "STUDY DESIGN ANALYSIS")
})
