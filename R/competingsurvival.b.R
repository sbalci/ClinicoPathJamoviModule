#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'


competingsurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "competingsurvivalClass",
    inherit = competingsurvivalBase,
    private = list(
        .run = function() {



            # TODO

            todo <- glue::glue(
                "This Module is still under development
                🔬🔬🔬🔬 UNDER CONSTRUCTION 🛠⛔️⚠️🔩
                -
                -
                "
            )

            self$results$todo$setContent(todo)

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')



            # https://finalfit.org/articles/survival.html#death-status


            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
