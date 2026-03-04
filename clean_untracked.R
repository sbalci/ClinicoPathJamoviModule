fix_files <- c(
  "tests/testthat/test-enhancedROC-advanced.R",
  "tests/testthat/test-psychopdaROC-advanced.R",
  "tests/testthat/test-psychopdaROC-integration.R"
)

for(f in fix_files) {
  if (file.exists(f)) {
    lines <- readLines(f)
    # Remove the bad inline arguments
    lines <- gsub("enhancedROC\\(positiveClass = NULL, ", "enhancedROC(", lines)
    lines <- gsub("psychopdaROC\\(refVar = NULL, ", "psychopdaROC(", lines)
    
    # Check if we still have the wrapper at the top
    # The wrapper is 5 lines. Let's just remove anything matching those exact lines.
    lines <- lines[lines != "enhancedROC <- function(...) {"]
    lines <- lines[lines != "  args <- list(...)"]
    lines <- lines[lines != "  if (!'positiveClass' %in% names(args)) args$positiveClass <- NULL"]
    lines <- lines[lines != "  do.call(ClinicoPath::enhancedROC, args)"]
    
    lines <- lines[lines != "psychopdaROC <- function(...) {"]
    lines <- lines[lines != "  if (!'refVar' %in% names(args)) args$refVar <- NULL"]
    lines <- lines[lines != "  do.call(ClinicoPath::psychopdaROC, args)"]
    lines <- lines[lines != "}"]
    
    # We might have left over empty lines at the top, but it's fine.
    
    # Re-apply the clean wrapper at the top for enhancedROC files
    if (grepl("enhancedROC", f)) {
      prefix <- c(
        "enhancedROC <- function(...) {",
        "  args <- list(...)",
        "  if (!'positiveClass' %in% names(args)) args$positiveClass <- NULL",
        "  do.call(ClinicoPath::enhancedROC, args)",
        "}"
      )
      lines <- c(prefix, lines)
    }
    
    if (grepl("psychopdaROC", f)) {
      prefix <- c(
        "psychopdaROC <- function(...) {",
        "  args <- list(...)",
        "  if (!'refVar' %in% names(args)) args$refVar <- NULL",
        "  do.call(ClinicoPath::psychopdaROC, args)",
        "}"
      )
      lines <- c(prefix, lines)
    }
    
    writeLines(lines, f)
  }
}

cat("Cleaned up untracked files.\n")
