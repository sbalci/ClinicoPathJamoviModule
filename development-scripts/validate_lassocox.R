# Run from the repository root: Rscript --vanilla development-scripts/validate_lassocox.R
# Source the real generated wrapper/backend through the existing regression file.
# This deliberately does not load or validate the entire umbrella package namespace.
required <- c("testthat", "jmvcore", "R6", "glmnet", "survival", "htmltools", "ncvreg", "withr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing validation dependencies: ", paste(missing, collapse = ", "))
path <- "tests/testthat/test-lassocox-validation.R"
if (!file.exists(path)) stop("Run from the repository root")
cat("Source commit: ", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n", sep = "")
for (pkg in required) cat(pkg, as.character(utils::packageVersion(pkg)), "\n")
result <- testthat::test_file(path, reporter = "summary", stop_on_failure = FALSE)
summary <- as.data.frame(result)
columns <- c("file", "test", "nb", "failed", "skipped", "error", "warning", "passed")
print(summary[, columns], row.names = FALSE)
out <- Sys.getenv("LASSOCOX_REPORT_DIR", "")
if (nzchar(out)) {
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(summary[, columns], file.path(out, "results.csv"), row.names = FALSE)
  writeLines(capture.output(utils::sessionInfo()), file.path(out, "sessionInfo.txt"))
}
print(utils::sessionInfo())
if (!nrow(summary) || any(summary$failed > 0 | summary$error | summary$skipped))
  stop("LASSO Cox validation failed, errored, skipped, or ran no tests")
