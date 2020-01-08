#' @importFrom R6 R6Class
#' @import jmvcore
#' @import finalfit
#' @import survival
#' @import survminer
# This file is a generated template, your changes will not be overwritten

finalfitClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "finalfitClass",
    inherit = finalfitBase,
    private = list(
        .run = function() {

            if (length(self$options$explanatory) + length(self$options$outcome) + length(self$options$overalltime) < 3)
                return()

            # results 1

            mydata <- self$data

            myoveralltime <- self$options$overalltime

            myoveralltime <- jmvcore::toNumeric(self$data[[myoveralltime]])

            myexplanatory <- self$options$explanatory

            myexplanatory <- self$data[[myexplanatory]]

            myoutcome <- self$options$outcome

            myoutcome <- self$data[[myoutcome]]

            km_fit <- survival::survfit(survival::Surv(myoveralltime, myoutcome) ~ myexplanatory, data = mydata)

            results1 <- summary(km_fit)$table

            # results 2

            formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)

            formula2 <- jmvcore::composeTerm(formula2)

            formulaL <- jmvcore::constructFormula(terms = self$options$overalltime)

            formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

            myformula <- paste("Surv(", formulaL, ",", formulaR, ")")

            finalfit::finalfit(.data = mydata,
                               dependent = myformula,
                               explanatory = formula2) -> tUni

            results2 <- tUni


            # results

            results <- list(results1, results2)

            self$results$text$setContent(results)

        })
)
