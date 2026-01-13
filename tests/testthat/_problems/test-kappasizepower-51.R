# Extracted from test-kappasizepower.R:51

# prequel ----------------------------------------------------------------------
library(testthat)
library(jmvcore)
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  stop("devtools needed to load package for tests")
}

# test -------------------------------------------------------------------------
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
expect_match(results$text2$content, "Number of outcome categories: 3")
expect_match(results$text2$content, "Number of raters: 3")
