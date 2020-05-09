#' Variable Tree
#'


#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import vtree
#'


vartreeClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "vartreeClass",
    inherit = vartreeBase,
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



            mydata <- self$data

            mydata <- jmvcore::naOmit(mydata)

            formula <- jmvcore::constructFormula(terms = self$options$vars)

            myvars <- jmvcore::decomposeFormula(formula = formula)

            myvars <- unlist(myvars)

            mydata <- mydata %>%
                dplyr::select(myvars)

            myvars <- paste0(myvars, collapse = " ")

            results <- vtree::vtree(mydata, myvars, pngknit = FALSE)

            self$results$text1$setContent(print(results))

            self$results$text2$setContent(print(results))



        },


        .plot = function(image, ...) {  # <-- the plot function ----

            mydata <- self$data

            mydata <- jmvcore::naOmit(mydata)

            formula <- jmvcore::constructFormula(terms = self$options$vars)

            myvars <- jmvcore::decomposeFormula(formula = formula)

            myvars <- unlist(myvars)

            mydata <- mydata %>%
                dplyr::select(myvars)

            myvars <- paste0(myvars, collapse = " ")


            vtree::vtree(mydata, myvars)

            TRUE

        }


        )
)
