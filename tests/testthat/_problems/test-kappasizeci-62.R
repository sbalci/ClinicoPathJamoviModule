# Extracted from test-kappasizeci.R:62

# prequel ----------------------------------------------------------------------
library(testthat)
library(jmvcore)
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  stop("devtools needed to load package for tests")
}

# test -------------------------------------------------------------------------
expect_error(
    kappaSizeCI(
        outcome = "2",
        props = "0.20, 0.20" 
    ),
    regexp = "Proportions should sum to 1.0"
  )
