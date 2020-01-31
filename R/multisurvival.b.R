#' @importFrom R6 R6Class
#' @import jmvcore
#'
# This file is a generated template, your changes will not be overwritten

multisurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "multisurvivalClass",
    inherit = multisurvivalBase,
    private = list(
        .run = function() {



            if (length(self$options$explanatory) + length(self$options$outcome) + length(self$options$overalltime) < 3)
                return()

            mydata <- self$data

            formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)

            #
            # jmvcore::constructFormula(terms = list("A", "B", "C"))
            #
            #
            # formula2 <- jmvcore::composeTerm(formula2)

            formulaL <- jmvcore::constructFormula(terms = self$options$overalltime)

            formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

            myformula <- paste("Surv(", formulaL, ",", formulaR, ")")

            finalfit::finalfit(.data = mydata,
                               dependent = myformula,
                               explanatory = formula2) -> tMultivariate



            results1 <- knitr::kable(tMultivariate,
                                     row.names = FALSE,
                                     align = c('l', 'l', 'r', 'r', 'r', 'r'),
                                     format = "html")


            self$results$text$setContent(results1)


            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
