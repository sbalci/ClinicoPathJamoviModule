
# Script to check test coverage for Jamovi modules

library(fs)
library(stringr)

# Paths
jamovi_dir <- "jamovi"
test_dir <- "tests/testthat"

# Get list of module definitions
yaml_files <- dir_ls(jamovi_dir, glob = "*.a.yaml")
module_names <- path_file(yaml_files) %>% 
  str_remove("\\.a\\.yaml$")

# Check for corresponding test files
results <- data.frame(
  module = module_names,
  yaml_file = as.character(yaml_files),
  test_file_exists = FALSE,
  test_file_path = NA_character_
)

for (i in 1:nrow(results)) {
  module_name <- results$module[i]
  # Expected test file name: test-<module>.R
  test_file <- path(test_dir, paste0("test-", module_name, ".R"))
  
  if (file_exists(test_file)) {
    results$test_file_exists[i] <- TRUE
    results$test_file_path[i] <- as.character(test_file)
  } else {
      # Try alternative naming? sometimes test-moduleName.R vs test-modulename.R
      # but standard seems to be exact match or lowercase. 
      # Let's check case insensitive match if not found
  }
}

# Print summary
print(paste("Total Modules:", nrow(results)))
print(paste("Modules with Tests:", sum(results$test_file_exists)))
print(paste("Modules Missing Tests:", sum(!results$test_file_exists)))

# List missing tests
missing <- results[results$test_file_exists == FALSE, "module"]
print("Modules missing tests:")
print(missing)

# Save results for agent to read
write.csv(results, "module_test_coverage_report.csv", row.names = FALSE)
