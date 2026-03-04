test_files <- list.files("tests/testthat", pattern = "test-.*\\.R$", full.names = TRUE)
for(f in test_files) {
  lines <- readLines(f)
  lines <- gsub("args\\['positiveClass'\\] <- list\\(NULL\\)", "args['positiveClass'] <- ''", lines)
  lines <- gsub("args\\['refVar'\\] <- list\\(NULL\\)", "args['refVar'] <- ''", lines)
  writeLines(lines, f)
}
cat("Updated wrapper defaults to empty strings.\n")
