
# This file is a generated template, your changes will not be overwritten

multisurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "multisurvivalClass",
    inherit = multisurvivalBase,
    private = list(
        .run = function() {


            if (length(self$options$explanatory) + length(self$options$outcome) + length(self$options$overalltime) < 3)
                return()

            # results 1

            mydata <- self$data

            myoveralltime <- self$options$overalltime

            myoveralltime <- jmvcore::toNumeric(self$data[[myoveralltime]])

            thefactor <- self$options$explanatory

            thefactor <- self$data[[thefactor]]

            myoutcome <- self$options$outcome

            myoutcome <- self$data[[myoutcome]]

            km_fit <- survival::survfit(survival::Surv(myoveralltime, myoutcome) ~ thefactor, data = mydata)

            results1 <- summary(km_fit)$table




            formula <- jmvcore::constructFormula(terms = self$options$vars)
            formula <- paste('~', formula)
            formula <- as.formula(formula)

            table1 <- arsenal::tableby(formula, self$data)









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


            # results 3



            results3 <- knitr::kable(tUni,
                                     row.names = FALSE,
                                     align = c('l', 'l', 'r', 'r', 'r', 'r'))







            self$results$text$setContent(results1)


            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
