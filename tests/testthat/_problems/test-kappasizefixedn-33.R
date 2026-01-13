# Extracted from test-kappasizefixedn.R:33

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
    kappa0 = 0.60,
    props = "0.30, 0.70",
    raters = "2",
    alpha = 0.05,
    n = 100
  )
expect_true(!is.null(results$text1$content))
expect_true(!is.null(results$text2$content))
content1 <- results$text1$content
expect_true(nchar(content1) > 0)
content2 <- results$text2$content
expect_match(content2, "Number of outcome categories: 2", fixed = FALSE)
