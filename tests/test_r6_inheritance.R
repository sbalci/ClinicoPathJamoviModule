
library(R6)

Parent <- R6Class("Parent",
  public = list(
    run = function() {
      print("Parent run calling private$.run()")
      private$.run()
    }
  )
)

Child <- R6Class("Child",
  inherit = Parent,
  private = list(
    .run = function() {
      print("Child .run executed")
    }
  )
)

obj <- Child$new()
obj$run()
