test_files <- list.files("tests/testthat", pattern = "test-.*\\.R$", full.names = TRUE)
for(f in test_files) {
  lines <- readLines(f)
  lines <- gsub("args\\['refVar'\\] <- ''", "args\\$refVar <- NULL", lines)
  writeLines(lines, f)
}
cat("Updated refVar wrapper to omit argument.\n")
