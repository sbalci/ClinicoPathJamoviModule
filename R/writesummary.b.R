
# This file is a generated template, your changes will not be overwritten

writesummaryClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "writesummaryClass",
    inherit = writesummaryBase,
    private = list(
        .run = function() {

            
            
            
            if (length(self$options$vars) == 0)
                return()
            
            
            meanfun <- function(x) mean(x, na.rm=TRUE)
            
            mydata <- self$data
            
            myvar <- self$options$vars
            
            mean_x <- meanfun(mydata[[myvar]])
            
            results <- print(mean_x)
            
            self$results$text$setContent(results)

            
            
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
