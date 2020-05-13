#' Competing Survival Analysis
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'


competingsurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "competingsurvivalClass",
    inherit = competingsurvivalBase,
    private = list(
        .run = function() {

            # If no variable selected Initial Message ----

            if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$overalltime) ) {

                todo <- glue::glue("

                                This Module is still under development
                                   ",

                # <br>Welcome to ClinicoPath
                # <br><br>
                # This tool will help you calculate median survivals and 1,3,5-yr survivals for a given fisk factor.
                # <br><br>
                # Explanatory variable should be categorical (ordinal or nominal).
                # <br><br>
                # Outcome variable should be coded binary (0 or 1).
                # <br><br>
                # If patient is dead or event (recurrence) occured it is 1.
                # <br><br>
                # If censored (patient is alive or free of disease) at the last visit it is 0.
                # <br><br>
                # Survival should be numeric, continuous, and in months.
                # <br><br>
                # This function uses survival, survminer, and finalfit packages. Please cite jamovi and the packages as given below.
                                   ""
                )

                html <- self$results$todo
                html$setContent(todo)
                return()

            } else {

                # Empty message when all variables selected

                todo <- ""
                html <- self$results$todo
                html$setContent(todo)


                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')





            # https://finalfit.org/articles/survival.html#death-status


}

        })
)
