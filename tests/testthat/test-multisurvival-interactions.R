# Fast, harness-free unit tests for multisurvival interaction helpers.
# Source only the two self-contained helper files (no devtools::load_all,
# no R6/jamovi harness needed) — see feedback_testing_strategy.
suppressMessages({
  library(testthat)
  library(survival)
  library(jmvcore)
})

# Resolve the repo root robustly regardless of how this file is executed
# (testthat::test_file() chdirs into the test file's directory before
# running the tests, so getwd() is normally ".../tests/testthat"; other
# runners such as testthat::test_dir()/devtools::test() may leave the
# working directory at the package root instead). Walk up from both
# candidate starting points looking for the marker file R/utils.R.
.find_root <- function(start) {
  d <- suppressWarnings(normalizePath(start, mustWork = FALSE))
  for (i in seq_len(8)) {
    if (file.exists(file.path(d, "R", "utils.R"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  NA_character_
}

.root <- .find_root(file.path(dirname(dirname(getwd()))))
if (is.na(.root)) .root <- .find_root(getwd())
if (is.na(.root)) stop("Could not locate repo root (marker file R/utils.R not found)")

source(file.path(.root, "R", "utils.R"))
source(file.path(.root, "R", "multisurvival-interactions.R"))

# Local-only verification shim (does NOT touch production code): the CRAN
# release of jmvcore (2.7.7, what a plain system R installs) does not export
# asFormula() — it ships only in jamovi's own bundled jmvcore (>= 2.7.12),
# which cannot be loaded into a newer system R. See project memory
# "feedback_phantom_private_method_nonfunction" -> "Local-verification
# caveat". `.asSurvivalFormula()` in R/utils.R is left exactly as written
# (still calls jmvcore::asFormula for real inside jamovi); here we only
# swap the lookup used by THIS TEST RUN so `.buildSurvivalFormula()` can be
# exercised outside the jamovi app. This is faithful for well-formed
# formulas: asFormula() returns the same parsed formula as as.formula(), it
# only adds a security allow-list on top.
if (!exists("asFormula", envir = asNamespace("jmvcore"), inherits = FALSE)) {
  assign(".asSurvivalFormula", function(x) stats::as.formula(x), envir = .GlobalEnv)
}

test_that(".buildSurvivalFormula appends escaped interaction terms", {
  f <- .buildSurvivalFormula(
    time_var = "mytime", outcome_var = "myoutcome",
    predictors = c("arm", "bio"),
    interaction_terms = "`arm`:`bio`"
  )
  rhs <- as.character(f)[3]
  expect_true(grepl("arm", rhs))
  expect_true(grepl("bio", rhs))
  expect_true(grepl(":", rhs))
})

test_that(".buildSurvivalFormula still works with no interactions", {
  f <- .buildSurvivalFormula("mytime", "myoutcome", c("arm", "bio"))
  rhs <- as.character(f)[3]
  expect_false(grepl(":", rhs))
  expect_true(grepl("arm", rhs) && grepl("bio", rhs))
})

test_that(".buildSurvivalFormula keeps strata after interactions", {
  f <- .buildSurvivalFormula("mytime", "myoutcome", c("arm"),
                             strata_vars = "site",
                             interaction_terms = "`arm`:`bio`")
  rhs <- as.character(f)[3]
  expect_true(grepl("strata\\(", rhs))
  expect_true(grepl(":", rhs))
})
