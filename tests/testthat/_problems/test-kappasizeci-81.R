# Extracted from test-kappasizeci.R:81

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
expect_error(
    kappaSizeCI(
        outcome = "3",
        props = "0.5, 0.5"
    ),
    regexp = "Expected 3 proportions"
  )
expect_error(
      kappaSizeCI(
          outcome = "2",
          kappaL = 0.9,
          kappaU = 0.8
      ),
      regexp = "kappaL must be less than kappaU"
  )
