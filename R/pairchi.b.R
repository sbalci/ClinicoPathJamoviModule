#'
#' @importFrom R6 R6Class
#' @import jmvcore


pairchiClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "pairchiClass",
    inherit = pairchiBase,
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



            # RVAideMemoire::chisq.multcomp()
            # RVAideMemoire::fisher.multcomp()
            # rmngb::pairwise.chisq.test(x, ...)
            # rmngb::pairwise.fisher.test(x, ...)

        })
)
