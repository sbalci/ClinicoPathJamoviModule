#' @title Interclass Correlation Coefficient
#'
#' Also see \url{http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/#ordinal-data-weighted-kappa}
#'
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'



icccoeffClass <- if (requireNamespace("jmvcore")) R6::R6Class("icccoeffClass",
    inherit = icccoeffBase, private = list(.run = function() {





        # # Error Message ----
        #
        # if (nrow(self$data) == 0) stop("Data contains no (complete) rows")
        #
        # if ( (is.null(self$options$vars) || is.null(self$options$facs)) && is.null(self$options$target) ) {
        #     # ToDo Message ----
        #     todo <- "
        #         <br>Welcome to ClinicoPath
        #                   <br><br>
        #                   This tool will help you form an Alluvial Plots.
        #                   "
        #     html <- self$results$todo
        #     html$setContent(todo)
        #
        # } else {
        #     todo <- ""
        #     html <- self$results$todo
        #     html$setContent(todo)
        #
        #
        #
        # }

           mydata <- self$data


           results <- names(mydata)


           self$results$text$setContent(results)



    }

    ))
