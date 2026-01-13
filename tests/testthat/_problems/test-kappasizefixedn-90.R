# Extracted from test-kappasizefixedn.R:90

# prequel ----------------------------------------------------------------------
library(testthat)
library(jmvcore)
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  stop("devtools needed to load package for tests")
}

# test -------------------------------------------------------------------------
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
