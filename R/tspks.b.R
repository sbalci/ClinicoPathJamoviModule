# from https://github.com/FredHasselman/casnet-jmvMAC


tsPKSClass <- if (requireNamespace("jmvcore")) {
  R6::R6Class(
    "tsPKSClass",
    inherit = tsPKSBase,
    private = list(
      .run = function() {

        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
      }
    )
  )
}
