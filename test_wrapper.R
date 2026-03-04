options(error=traceback)
devtools::load_all()

data(psychopdaROC_test, package = "ClinicoPath")

psychopdaROC <- function(...) {
  args <- list(...)
  if (!'refVar' %in% names(args)) args['refVar'] <- if (is.null(args[['dependentVars']])) '' else args[['dependentVars']][1]
  print("Args names:")
  print(names(args))
  do.call(ClinicoPath::psychopdaROC, args)
}

print("Running test...")
result <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status"
)
print("Finished test...")
