# Extracted from test-kappasizefixedn.R:59

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
      props = "0.20, 0.20",
      n = 100
  )
