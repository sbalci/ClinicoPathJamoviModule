
tryCatch({
    devtools::load_all(quiet = TRUE)
    library(jmv)
    data("histopathology", package = "ClinicoPath")
    
    print("Running basic agreement...")
    result <- agreement(
        data = histopathology,
        vars = c("Rater 1", "Rater 2")
    )
    print("Basic agreement ran successfully.")
    
    print("Running agreement with weighted kappa and more options...")
    result_complex <- agreement(
      data = histopathology,
      vars = c("Rater A", "Rater B"),
      wght = "squared",
      icc = TRUE
    )
    print("Complex agreement ran successfully.")

}, error = function(e) {
    print("ERROR CAUGHT:")
    print(e)
    traceback()
})
