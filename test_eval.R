options(error=traceback)
devtools::load_all()
data(psychopdaROC_test, package = "ClinicoPath")

psychopdaROC <- function(...) {
  args <- list(...)
  f_args <- formals(ClinicoPath::psychopdaROC)
  for(arg in names(f_args)) {
    if(arg %in% c('...', 'data')) next
    if(!(arg %in% names(args))) {
      # If it's empty (no default)
      if(is.name(f_args[[arg]]) && as.character(f_args[[arg]]) == '') {
        args[[arg]] <- NULL
      }
    }
  }
  do.call(ClinicoPath::psychopdaROC, args)
}

print("Running targeted targeted do.call...")
try(psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status"
))
