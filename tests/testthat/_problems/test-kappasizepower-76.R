# Extracted from test-kappasizepower.R:76

# prequel ----------------------------------------------------------------------
library(testthat)
library(jmvcore)
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  stop("devtools needed to load package for tests")
}
expect_error(kappaSizePower(
      outcome = "2",
      kappa0 = 0.60,
      kappa1 = 0.40,  # Less than kappa0
      props = "0.5, 0.5",
      power = 0.80
  ), "kappa1 must be greater")

# test -------------------------------------------------------------------------
expect_error(kappaSizePower(
      outcome = "2",
      kappa0 = 0.4,
      kappa1 = 0.6,
      props = "0.5, 0.5",
      power = 0.30  # Below 0.5 threshold
  ), "Power should be at least 0.5")
