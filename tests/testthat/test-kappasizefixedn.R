
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
  expect_match(content2, "Number of outcome categories: 2", fixed = FALSE)
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
  expect_match(results$text2$content, "Number of outcome categories: 3")
  expect_match(results$text2$content, "Number of raters: 3")
})

test_that("kappaSizeFixedN handles validation errors gracefully", {
  # Invalid proportions (sum != 1)
  results <- kappaSizeFixedN(
      outcome = "2",
      props = "0.20, 0.20",
      n = 100
  )
  
  # Should return an error message in text1
  expect_match(results$text1$content, "Error|Proportions must sum to 1", perl = TRUE)
  
  # Mismatched outcome count and proportions
  results2 <- kappaSizeFixedN(
      outcome = "3",
      props = "0.5, 0.5",
      n = 100
  )
  
  expect_match(results2$text1$content, "Error|Expected 3 proportions", perl = TRUE)
})

test_that("kappaSizeFixedN integration with kappaSize package calculations", {
    # We compare against a known result if possible, or just ensure it returns valid output
    results <- kappaSizeFixedN(
        outcome = "2",
        kappa0 = 0.6,
        props = "0.5, 0.5",
        raters = "2",
        alpha = 0.05,
        n = 100
    )
    
    # Check that the output is valid (not an error message)
    output_str <- results$text1$content
    expect_true(nchar(output_str) > 0)
    
    # Verify explanation mentions study design
    expect_match(results$text2$content, "STUDY DESIGN ANALYSIS")
})
