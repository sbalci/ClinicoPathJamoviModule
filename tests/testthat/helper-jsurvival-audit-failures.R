# Inspect retained guidance through the same noThrow/status contract as the engine.
.audit_failed_result <- function(name, ...) {
  args <- list(...)
  pkg <- if ("jsurvival" %in% loadedNamespaces()) "jsurvival" else "ClinicoPath"
  ns <- asNamespace(pkg)
  data <- args$data
  args$data <- NULL
  options <- do.call(get(paste0(name, "Options"), ns)$new, args)
  analysis <- get(paste0(name, "Class"), ns)$new(options = options, data = data)
  analysis$init(noThrow = TRUE)
  analysis$run(noThrow = TRUE)
  testthat::expect_identical(analysis$status, "error")
  analysis$results
}
run_singlearm_failed <- function(...) .audit_failed_result("singlearm", ...)
