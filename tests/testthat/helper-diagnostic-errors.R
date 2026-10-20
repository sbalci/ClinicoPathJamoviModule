# Inspect refused analyses through the real jmvcore error handler. Public R API
# refusal is tested separately; this helper additionally checks engine state.
diagnostic_error_results <- function(analysis, data = data.frame(), ...) {
  ns <- asNamespace("ClinicoPath")
  options <- do.call(get(paste0(analysis, "Options"), ns)$new, list(...))
  object <- get(paste0(analysis, "Class"), ns)$new(options = options, data = data)
  object$init()
  suppressWarnings(object$run(noThrow = TRUE))
  testthat::expect_identical(object$status, "error")
  testthat::expect_true(object$errored)
  testthat::expect_false(object$complete)
  object$results
}
