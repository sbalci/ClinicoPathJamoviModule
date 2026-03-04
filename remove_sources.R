test_files <- list.files("tests/testthat", pattern = "\\.R$", full.names = TRUE)
files_changed <- 0
for(f in test_files) {
  lines <- readLines(f)
  # Remove testing manual sources
  idx <- grep("source\\(.*\\.h\\.R\"\\)|source\\(.*\\.b\\.R\"\\)", lines)
  if (length(idx) > 0) {
    lines <- lines[-idx]
    writeLines(lines, f)
    files_changed <- files_changed + 1
  }
}
cat(sprintf("Removed source calls from %d files.\n", files_changed))
