# from https://github.com/lago1970/jmvexamples

# This file is a generated template, your changes will not be overwritten

Example02Class <- R6::R6Class(
    "Example02Class",
    inherit = Example02Base,
    private = list(
        .init = function() {
            preformatted <- jmvcore::Preformatted$new(self$options, 'results')
            self$results$add(preformatted)
        },
        .run = function() {

            if (is.null(self$options$dependent) ||
                is.null(self$options$group))
                    return()

            data <- data.frame(
                x = self$data[[self$options$group]],
                y = self$data[[self$options$dependent]])

            ttest <- t.test(y ~ x, data, var.equal=self$options$equVar)

            results <- self$results$get('results')
            results$content <- paste0(capture.output(ttest), collapse='\n')
        })
)
