#' ROC Analysis
#'
#' @return
#' @export
#'
#' 
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'


rocClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "rocClass",
    inherit = rocBase,
    private = list(
        .run = function() {



            # TODO

            todo <- glue::glue(
                "This Module is still under development
                -
                -
                "
            )

            self$results$todo$setContent(todo)


            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')



        })
)
