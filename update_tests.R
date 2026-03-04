test_files <- list.files("tests/testthat", pattern = "test-enhancedROC.*\\.R$", full.names = TRUE)
for(f in test_files) {
  lines <- readLines(f)
  prefix <- c(
    "enhancedROC <- function(...) {",
    "  args <- list(...)",
    "  if (!'positiveClass' %in% names(args)) args$positiveClass <- NULL",
    "  do.call(ClinicoPath::enhancedROC, args)",
    "}"
  )
  lines <- lines[!grepl("enhancedROC <- function", lines)]
  lines <- lines[!grepl("ClinicoPath::enhancedROC", lines)]
  lines <- lines[!grepl("positiveClass = positiveClass", lines)]
  lines <- lines[!grepl("^}$", lines)]
  # Since we want to just replace the broken prefix, let's reset using git first
  writeLines(c(prefix, lines), f)
}

test_files <- list.files("tests/testthat", pattern = "test-psychopdaROC.*\\.R$", full.names = TRUE)
for(f in test_files) {
  lines <- readLines(f)
  prefix <- c(
    "psychopdaROC <- function(...) {",
    "  args <- list(...)",
    "  if (!'refVar' %in% names(args)) args$refVar <- NULL",
    "  do.call(ClinicoPath::psychopdaROC, args)",
    "}"
  )
  writeLines(c(prefix, lines), f)
}

cat("Updated test files with do.call wrappers.\n")
