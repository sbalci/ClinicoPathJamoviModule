
library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  stop("devtools needed to load package for tests")
}

test_that("kappaSizeFixedN works for binary outcomes (2 categories)", {
  results <- kappaSizeFixedN(
    outcome = "2",
    kappa0 = 0.60,
    props = "0.30, 0.70",
    raters = "2",
    alpha = 0.05,
    n = 100
  )

  expect_true(!is.null(results$text1$content))
  expect_true(!is.null(results$text2$content))
  expect_true(!is.null(results$text_summary$content))
  expect_true(nchar(results$text1$content) > 0)

  # Explanation should contain the descriptive sentence
  content2 <- results$text2$content
  expect_match(content2, "expected lower bound")
  expect_match(content2, "100 subjects")
  expect_match(content2, "2 raters")

  # Summary should contain kappaSize summary output
  summary_content <- results$text_summary$content
  expect_match(summary_content, "Kappa0")
  expect_match(summary_content, "Lower Expected Limit")
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
  expect_match(results$text2$content, "expected lower bound")
  expect_match(results$text2$content, "150 subjects")
  expect_match(results$text2$content, "3 raters")
})

test_that("kappaSizeFixedN works for 4 categories", {
  results <- kappaSizeFixedN(
    outcome = "4",
    kappa0 = 0.50,
    props = "0.10, 0.20, 0.30, 0.40",
    raters = "2",
    alpha = 0.05,
    n = 200
  )

  expect_true(nchar(results$text1$content) > 0)
  expect_match(results$text2$content, "expected lower bound")
})

test_that("kappaSizeFixedN works for 5 categories", {
  results <- kappaSizeFixedN(
    outcome = "5",
    kappa0 = 0.50,
    props = "0.10, 0.20, 0.20, 0.25, 0.25",
    raters = "2",
    alpha = 0.05,
    n = 200
  )

  expect_true(nchar(results$text1$content) > 0)
  expect_match(results$text2$content, "expected lower bound")
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

  expect_true(nchar(results$text1$content) > 0)
  expect_true(nchar(results$text2$content) > 0)
})
