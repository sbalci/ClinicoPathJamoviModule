
library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  stop("devtools needed to load package for tests")
}

test_that("kappaSizeCI works for binary outcomes (2 categories)", {
  # Basic binary case: 2 raters, 2 categories, kappa0=0.6, kappaL=0.4, kappaU=0.8
  results <- kappaSizeCI(
    outcome = "2",
    kappa0 = 0.60,
    kappaL = 0.40,
    kappaU = 0.80,
    props = "0.30, 0.70",
    raters = "2",
    alpha = 0.05
  )
  
  # Check if results are populated
  expect_true(!is.null(results$text1$content))
  expect_true(!is.null(results$text2$content))
  
  # Check content format
  content1 <- results$text1$content
  expect_match(content1, "Required sample size", fixed = FALSE)
  
  # Explanation should contain key parameters
  content2 <- results$text2$content
  expect_match(content2, "Number of outcome categories: 2")
  expect_match(content2, "Number of raters: 2")
})

test_that("kappaSizeCI works for 3 categories", {
  results <- kappaSizeCI(
    outcome = "3",
    kappa0 = 0.50,
    kappaL = 0.30,
    kappaU = 0.70,
    props = "0.20, 0.30, 0.50",
    raters = "3",
    alpha = 0.05
  )
  
  expect_match(results$text1$content, "Required sample size")
  expect_match(results$text2$content, "Number of outcome categories: 3")
  expect_match(results$text2$content, "Number of raters: 3")
})

test_that("kappaSizeCI handles validation errors gracefully", {
  # Invalid proportions (sum != 1)
  expect_error(
    kappaSizeCI(
        outcome = "2",
        props = "0.20, 0.20" 
    ),
    regexp = "Proportions should sum to 1.0"
  )
  
  # Mismatched outcome count and proportions
  expect_error(
    kappaSizeCI(
        outcome = "3",
        props = "0.5, 0.5"
    ),
    regexp = "Expected 3 proportions"
  )
  
  # Invalid kappa range (L > U)
  expect_error(
      kappaSizeCI(
          outcome = "2",
          kappaL = 0.9,
          kappaU = 0.8
      ),
      regexp = "kappaL must be less than kappaU"
  )
})

test_that("kappaSizeCI integration with kappaSize package calculations", {
    # We compare against a known result if possible, or just ensure it returns a valid number
    # For binary: k0=0.6, kL=0.4, kU=0.8, props=0.5,0.5, raters=2, alpha=0.05
    # kappaSize::CIBinary(kappa0=0.6, kappaL=0.4, kappaU=0.8, props=c(0.5,0.5), alpha=0.05, raters=2)
    
    results <- kappaSizeCI(
        outcome = "2",
        kappa0 = 0.6,
        kappaL = 0.4,
        kappaU = 0.8,
        props = "0.5, 0.5",
        raters = "2",
        alpha = 0.05
    )
    
    # Extract number from "Required sample size: X"
    output_str <- results$text1$content
    # Expected pattern: "Required sample size: [numbers]"
    expect_match(output_str, "Required sample size: \\d+")
    
    number <- as.numeric(gsub("[^0-9]", "", output_str))
    expect_true(number > 0)
})
