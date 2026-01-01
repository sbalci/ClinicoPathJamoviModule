
# Driver script to batch generate tests for missing modules

library(fs)
library(stringr)

# Source the generator function
source("tests/generate_tests.R")

# Read the coverage report
if (file.exists("module_test_coverage_report.csv")) {
  coverage <- read.csv("module_test_coverage_report.csv", stringsAsFactors = FALSE)
  
  # Filter for missing tests
  missing_modules <- coverage[coverage$test_file_exists == "FALSE", ]
  
  print(paste("Generating tests for", nrow(missing_modules), "modules..."))
  
  success_count <- 0
  error_count <- 0
  
  for (i in 1:nrow(missing_modules)) {
    mod_name <- missing_modules$module[i]
    yaml_path <- missing_modules$yaml_file[i]
    
    tryCatch({
      generate_test_for_module(yaml_path)
      success_count <- success_count + 1
    }, error = function(e) {
      print(paste("Error generating test for", mod_name, ":", e$message))
      error_count <- error_count + 1
    })
  }
  
  print(paste("Batch Complete. Generated:", success_count, "Failed:", error_count))
  
} else {
  print("Error: module_test_coverage_report.csv not found. Run checks first.")
}
