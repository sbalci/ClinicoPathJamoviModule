test_files <- list.files("tests/testthat", pattern = "test-.*\\.R$", full.names = TRUE)
for(f in test_files) {
  lines <- readLines(f)
  lines <- gsub("args\\$refVar <- NULL", "args\\['refVar'\\] <- if (is.null(args[['dependentVars']])) '' else args[['dependentVars']][1]", lines)
  writeLines(lines, f)
}
cat("Updated refVar wrapper to supply a valid dependent variable.\n")
