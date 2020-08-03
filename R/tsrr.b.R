# from https://github.com/FredHasselman/casnet-jmvMAC


tsRRClass <- if (requireNamespace("jmvcore")) {
  R6::R6Class(
    "tsRRClass",
    inherit = tsRRBase,
    private = list(
      .run = function() {

        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
      }
    )
  )
}
