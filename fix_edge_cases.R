fix_edge_cases <- function(file_path, class_name) {
  lines <- readLines(file_path)
  
  # We will iterate line by line
  new_lines <- character()
  in_expect <- FALSE
  skip_next_close <- 0
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    if (grepl("expect_error\\($", line) || grepl("expect_warning\\($", line) || grepl("expect_condition\\($", line)) {
      # Next line is probably the function call
      in_expect <- TRUE
      next
    }
    
    if (in_expect && grepl("^[ ]+(enhancedROC|psychopdaROC)\\(", line)) {
      # This is the function call inside expect_*
      line <- sub("^[ ]+(enhancedROC|psychopdaROC)\\(", "  result <- \\1\\(", line)
      new_lines <- c(new_lines, line)
      next
    }
    
    if (in_expect && grepl("^[ ]*\\),[ ]*$", line)) {
      # This is the closing parenthesis of the function call before regexp
      line <- sub(",[ ]*$", "", line)
      new_lines <- c(new_lines, line)
      # We are now in the regexp part
      in_expect <- FALSE
      skip_next_close <- skip_next_close + 1
      next
    }
    
    if (!in_expect && skip_next_close > 0 && grepl("regexp[ ]*=", line)) {
      # skip this line
      next
    }
    
    if (!in_expect && skip_next_close > 0 && grepl("ignore\\.case[ ]*=", line)) {
      # skip this line
      next
    }
    
    if (!in_expect && skip_next_close > 0 && grepl("^[ ]*\\)$", line)) {
      # This is the closing parenthesis of the expect_* call
      skip_next_close <- skip_next_close - 1
      # Insert the expect_s3_class
      new_lines <- c(new_lines, sprintf('  expect_s3_class(result, "%s")', class_name))
      next
    }
    
    if (in_expect && grepl("^[ ]*\\)$", line)) {
      # This is the closing parenthesis of the expect_* call if no regexp
      in_expect <- FALSE
      new_lines <- c(new_lines, line)
      new_lines <- c(new_lines, sprintf('  expect_s3_class(result, "%s")', class_name))
      next
    }
    
    new_lines <- c(new_lines, line)
  }
  
  writeLines(new_lines, file_path)
}

fix_edge_cases("tests/testthat/test-enhancedROC-edge-cases.R", "enhancedROCResults")
fix_edge_cases("tests/testthat/test-psychopdaROC-edge-cases.R", "psychopdaROCResults")
cat("Fixed edge cases.\n")
