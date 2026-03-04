test_files <- list.files("tests/testthat", pattern = "^test-(psychopdaROC|enhancedROC|agreement)-.*\\.R$", full.names = TRUE)

for(f in test_files) {
  content <- readLines(f)
  
  # Find the wrapper function: e.g., psychopdaROC <- function(...) {
  # We will replace the do.call line with a more robust one that adds NULLs for missing formals.
  
  # Replace psychopdaROC wrapper
  if(grepl("psychopdaROC", f)) {
    new_wrapper <- c(
      "psychopdaROC <- function(...) {",
      "  args <- list(...)",
      "  for(arg in names(formals(ClinicoPath::psychopdaROC))) {",
      "    if(!(arg %in% c('...', 'data')) && !(arg %in% names(args))) {",
      "      args[arg] <- list(NULL)",
      "    }",
      "  }",
      "  if (is.null(args[['refVar']])) args[['refVar']] <- if (is.null(args[['dependentVars']])) '' else args[['dependentVars']][1]",
      "  do.call(ClinicoPath::psychopdaROC, args)",
      "}"
    )
    # Find start and end of the wrapper
    start_idx <- grep("^psychopdaROC <- function", content)
    if(length(start_idx) > 0) {
      end_idx <- grep("^}", content[start_idx:length(content)])[1] + start_idx - 1
      content <- c(content[1:(start_idx-1)], new_wrapper, content[(end_idx+1):length(content)])
      writeLines(content, f)
    }
  }
}

cat("Updated psychopdaROC wrappers.\n")
