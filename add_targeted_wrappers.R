test_files <- list.files("tests/testthat", pattern = "^test-(psychopdaROC|enhancedROC)-.*\\.R$", full.names = TRUE)

wrapper_enhanced <- "enhancedROC <- function(...) {
  args <- list(...)
  f_args <- formals(ClinicoPath::enhancedROC)
  for(arg in names(f_args)) {
    if(arg %in% c('...', 'data')) next
    if(!(arg %in% names(args))) {
      if(is.name(f_args[[arg]]) && as.character(f_args[[arg]]) == '') {
        args[[arg]] <- \"\"
      }
    }
  }
  do.call(ClinicoPath::enhancedROC, args)
}"

wrapper_psycho <- "psychopdaROC <- function(...) {
  args <- list(...)
  f_args <- formals(ClinicoPath::psychopdaROC)
  for(arg in names(f_args)) {
    if(arg %in% c('...', 'data')) next
    if(!(arg %in% names(args))) {
      if(is.name(f_args[[arg]]) && as.character(f_args[[arg]]) == '') {
        args[[arg]] <- \"\"
      }
    }
  }
  do.call(ClinicoPath::psychopdaROC, args)
}"

for(f in test_files) {
  content <- readLines(f)
  
  # Remove old wrappers (by matching lines 1 to 14 roughly)
  # But a safer way is to strip any lines before `# ═════════════════`
  
  idx <- grep("^# ════════════════════════════════", content)[1]
  if (!is.na(idx)) {
    content <- content[idx:length(content)]
  }
  
  if (grepl("enhancedROC", f)) {
    new_content <- c(wrapper_enhanced, "", content)
  } else if (grepl("psychopdaROC", f)) {
    new_content <- c(wrapper_psycho, "", content)
  }
  
  writeLines(new_content, f)
  cat(sprintf("Added targeted wrapper to %s\n", f))
}
