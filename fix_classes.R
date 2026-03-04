test_files <- list.files("tests/testthat", pattern = "test-.*\\.R$", full.names = TRUE)
for(f in test_files) {
  lines <- readLines(f)
  lines <- gsub('"enhancedROCClass"', '"enhancedROCResults"', lines)
  lines <- gsub('"psychopdaROCClass"', '"psychopdaROCResults"', lines)
  lines <- gsub('"agreementClass"', '"agreementResults"', lines)
  writeLines(lines, f)
}
cat("Updated wrong class assertions.\n")
