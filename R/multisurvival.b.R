#' @importFrom R6 R6Class
#' @import jmvcore
#'
# This file is a generated template, your changes will not be overwritten

multisurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "multisurvivalClass",
    inherit = multisurvivalBase,
    private = list(
        .run = function() {


            # TODO

            todo <- glue::glue(
                "This Module is still under development
                -  correct multivariate formula
                -  or_plot
                - "
            )

            self$results$todo$setContent(todo)


            if (length(self$options$explanatory) < 1 |  (length(self$options$outcome) + length(self$options$overalltime) < 2))
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



            # explanatory = c("age.factor", "sex.factor",
            #                 "obstruct.factor", "perfor.factor")
            # dependent = 'mort_5yr'
            # colon_s %>%
            #     finalfit(dependent, explanatory, metrics=TRUE) -> t2
            # knitr::kable(t2[[1]], row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))
            # knitr::kable(t2[[2]], row.names=FALSE, col.names="")
            #
            #
            # explanatory = c("age.factor", "sex.factor",
            #                 "obstruct.factor", "perfor.factor")
            # dependent = 'mort_5yr'
            # colon_s %>%
            #     or_plot(dependent, explanatory)




            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
