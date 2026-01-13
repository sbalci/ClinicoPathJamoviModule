# Extracted from test-kappasizepower.R:35

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
    outcome = "2",
    kappa0 = 0.40,
    kappa1 = 0.60,
    props = "0.30, 0.70",
    raters = "2",
    alpha = 0.05,
    power = 0.80
  )
expect_true(!is.null(results$text1$content))
expect_true(!is.null(results$text2$content))
content1 <- results$text1$content
expect_true(nchar(content1) > 0)
content2 <- results$text2$content
expect_match(content2, "POWER ANALYSIS", fixed = FALSE)
expect_match(content2, "Number of raters: 2", fixed = FALSE)
