
library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
} else {
  stop("devtools needed to load package for tests")
}

test_that("kappaSizeCI works for binary outcomes (2 categories)", {
  results <- kappaSizeCI(
    outcome = "2",
    citype = "two_sided",
    kappa0 = 0.60,
    kappaL = 0.40,
    kappaU = 0.80,
    props = "0.30, 0.70",
    raters = "2",
    alpha = 0.05
  )

  expect_true(!is.null(results$text1$content))
  expect_true(!is.null(results$text2$content))
  expect_true(!is.null(results$text_summary$content))

  content1 <- results$text1$content
  expect_match(content1, "Required sample size", fixed = FALSE)

  content2 <- results$text2$content
  expect_match(content2, "Number of outcome categories: 2")
  expect_match(content2, "Number of raters: 2")
  expect_match(content2, "Two-sided")

  # Summary should contain kappaSize summary output
  summary_content <- results$text_summary$content
  expect_match(summary_content, "Kappa0")
  expect_match(summary_content, "KappaL")
})

test_that("kappaSizeCI works for 3 categories", {
  results <- kappaSizeCI(
    outcome = "3",
    citype = "two_sided",
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

test_that("kappaSizeCI works for 4 categories", {
  results <- kappaSizeCI(
    outcome = "4",
    citype = "two_sided",
    kappa0 = 0.50,
    kappaL = 0.30,
    kappaU = 0.70,
    props = "0.10, 0.20, 0.30, 0.40",
    raters = "2",
    alpha = 0.05
  )

  expect_match(results$text1$content, "Required sample size")
})

test_that("kappaSizeCI works for 5 categories", {
  results <- kappaSizeCI(
    outcome = "5",
    citype = "two_sided",
    kappa0 = 0.50,
    kappaL = 0.30,
    kappaU = 0.70,
    props = "0.10, 0.20, 0.20, 0.25, 0.25",
    raters = "2",
    alpha = 0.05
  )

  expect_match(results$text1$content, "Required sample size")
})

test_that("kappaSizeCI one-sided CI requires fewer subjects", {
  two_sided <- kappaSizeCI(
    outcome = "2",
    citype = "two_sided",
    kappa0 = 0.60,
    kappaL = 0.40,
    kappaU = 0.80,
    props = "0.30, 0.70",
    raters = "2",
    alpha = 0.05
  )

  one_sided <- kappaSizeCI(
    outcome = "2",
    citype = "one_sided",
    kappa0 = 0.60,
    kappaL = 0.40,
    kappaU = 0.80,
    props = "0.30, 0.70",
    raters = "2",
    alpha = 0.05
  )

  # One-sided should require fewer subjects
  expect_match(two_sided$text1$content, "Required sample size: 94")
  expect_match(one_sided$text1$content, "Required sample size: 66")

  # Explanation text should reflect CI type
  expect_match(two_sided$text2$content, "Two-sided")
  expect_match(one_sided$text2$content, "One-sided")
})

test_that("kappaSizeCI one-sided CI works for multi-category outcomes", {
  results <- kappaSizeCI(
    outcome = "3",
    citype = "one_sided",
    kappa0 = 0.50,
    kappaL = 0.30,
    kappaU = 0.70,
    props = "0.20, 0.30, 0.50",
    raters = "2",
    alpha = 0.05
  )

  expect_match(results$text1$content, "Required sample size: \\d+")
  expect_match(results$text2$content, "One-sided")
})

test_that("kappaSizeCI integration with kappaSize package calculations", {
  results <- kappaSizeCI(
    outcome = "2",
    citype = "two_sided",
    kappa0 = 0.6,
    kappaL = 0.4,
    kappaU = 0.8,
    props = "0.5, 0.5",
    raters = "2",
    alpha = 0.05
  )

  output_str <- results$text1$content
  expect_match(output_str, "Required sample size: \\d+")
})

test_that("kappaSizeCI accepts single prevalence input for binary outcomes", {
  results <- kappaSizeCI(
    outcome = "2",
    citype = "two_sided",
    kappa0 = 0.60,
    kappaL = 0.40,
    kappaU = 0.80,
    props = "0.30",
    raters = "2",
    alpha = 0.05
  )

  expect_match(results$text1$content, "Required sample size:")
})

test_that("kappaSizeCI supports 6 raters", {
  results <- kappaSizeCI(
    outcome = "2",
    citype = "two_sided",
    kappa0 = 0.60,
    kappaL = 0.40,
    kappaU = 0.80,
    props = "0.30, 0.70",
    raters = "6",
    alpha = 0.05
  )

  expect_match(results$text1$content, "Required sample size:")
})
