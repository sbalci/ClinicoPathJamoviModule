test_files <- list.files("tests/testthat", pattern = "^test-(psychopdaROC|enhancedROC|agreement)-.*\\.R$", full.names = TRUE)

for(f in test_files) {
  content <- readLines(f)
  
  if(grepl("psychopdaROC", f)) {
    start_idx <- grep("^psychopdaROC <- function", content)
    if(length(start_idx) > 0) {
      end_idx <- grep("^}", content[start_idx:length(content)])[1] + start_idx - 1
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
      content <- c(content[1:(start_idx-1)], new_wrapper, content[(end_idx+1):length(content)])
    }
  } else if(grepl("enhancedROC", f)) {
    start_idx <- grep("^enhancedROC <- function", content)
    if(length(start_idx) > 0) {
      end_idx <- grep("^}", content[start_idx:length(content)])[1] + start_idx - 1
      new_wrapper <- c(
        "enhancedROC <- function(...) {",
        "  args <- list(...)",
        "  for(arg in names(formals(ClinicoPath::enhancedROC))) {",
        "    if(!(arg %in% c('...', 'data')) && !(arg %in% names(args))) {",
        "      args[arg] <- list(NULL)",
        "    }",
        "  }",
        "  do.call(ClinicoPath::enhancedROC, args)",
        "}"
      )
      content <- c(content[1:(start_idx-1)], new_wrapper, content[(end_idx+1):length(content)])
    }
  } else if(grepl("agreement", f)) {
    start_idx <- grep("^agreement <- function", content)
    if(length(start_idx) > 0) {
      end_idx <- grep("^}", content[start_idx:length(content)])[1] + start_idx - 1
      new_wrapper <- c(
        "agreement <- function(...) {",
        "  args <- list(...)",
        "  for(arg in names(formals(ClinicoPath::agreement))) {",
        "    if(!(arg %in% c('...', 'data')) && !(arg %in% names(args))) {",
        "      args[arg] <- list(NULL)",
        "    }",
        "  }",
        "  do.call(ClinicoPath::agreement, args)",
        "}"
      )
      content <- c(content[1:(start_idx-1)], new_wrapper, content[(end_idx+1):length(content)])
    }
  }
  
  writeLines(content, f)
}

cat("Updated wrappers.\n")
