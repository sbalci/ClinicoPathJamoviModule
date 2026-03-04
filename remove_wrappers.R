test_files <- list.files("tests/testthat", pattern = "^test-(psychopdaROC|enhancedROC|agreement)-.*\\.R$", full.names = TRUE)

for(f in test_files) {
  content <- readLines(f)
  
  # Remove the wrapper from the start of the file.
  # Wrappers look like this:
  # psychopdaROC <- function(...) {
  # ...
  # }
  
  has_wrapper <- FALSE
  start_idx <- grep("^(psychopdaROC|enhancedROC|agreement) <- function\\(", content)
  if(length(start_idx) > 0) {
    # It must be at the very top of the file, around line 1
    if (start_idx[1] <= 5) {
      end_idx <- grep("^\\}", content[start_idx[1]:length(content)])[1] + start_idx[1] - 1
      if (!is.na(end_idx)) {
        content <- content[(end_idx+1):length(content)]
        
        # In integration tests, there might be a SECOND wrapper:
        # psychopdaROC <- function(..., refVar = NULL) {
        #   ClinicoPath::psychopdaROC(refVar = refVar, ...)
        # }
        start_idx2 <- grep("^(psychopdaROC|enhancedROC|agreement) <- function\\(", content)
        if (length(start_idx2) > 0 && start_idx2[1] <= 5) {
          end_idx2 <- grep("^\\}", content[start_idx2[1]:length(content)])[1] + start_idx2[1] - 1
          if (!is.na(end_idx2)) {
            content <- content[(end_idx2+1):length(content)]
          }
        }
        
        has_wrapper <- TRUE
      }
    }
  }
  
  if (has_wrapper) {
    writeLines(content, f)
    cat(sprintf("Removed wrappers from %s\n", f))
  }
}
