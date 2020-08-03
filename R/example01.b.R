# from https://github.com/lago1970/jmvexamples

Example01Class <- R6::R6Class(
    "Example01Class",
    inherit = Example01Base,
    private = list(
        .init = function() {
            preformatted <- jmvcore::Preformatted$new(self$options, 'results')
            self$results$add(preformatted)
        },
        .run = function() {
            results <- self$results$get('results')
            results$content <- 'The fish was delish'
        })
)
