# Standalone verification entry point for the real singlearm analysis.
#
# Run from the package root with:
#   Rscript tests/verify_singlearm.R
#
# This script never installs packages and does not mock jamovi. Missing
# development dependencies are reported as a failure so CI and local runs use
# the same implementation and dependency set.

required <- c("devtools", "testthat", "jmvcore", "survival", "cmprsk")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop(
    "Cannot verify singlearm; missing package(s): ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}

root <- if (file.exists("DESCRIPTION")) "." else ".."
if (!file.exists(file.path(root, "DESCRIPTION")))
  stop("Run this script from the package root or tests directory.", call. = FALSE)

old <- setwd(root)
on.exit(setwd(old), add = TRUE)
devtools::load_all(quiet = TRUE)

files <- c(
  "test-singlearm-critical-fixes.R",
  "test-singlearm-lifecycle-schema.R",
  "test-singlearm-domain-validation.R",
  "test-singlearm-time-landmark-fixes.R",
  "test-singlearm-zero-event-and-estimand.R",
  "test-singlearm-hazard-weighting.R",
  "test-baseline-hazard.R",
  "test-event-indicator.R",
  "test-survival-censoring-disclosure.R"
)

for (file in files) {
  testthat::test_file(
    file.path("tests", "testthat", file),
    reporter = "summary",
    stop_on_failure = TRUE,
    stop_on_warning = FALSE
  )
}
