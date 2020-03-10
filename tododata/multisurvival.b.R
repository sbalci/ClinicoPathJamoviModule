#' @importFrom R6 R6Class
#' @import jmvcore
#' @import finalfit
#' @import survival
#' @import survminer
#' @import ggplot2
#'
# This file is a generated template, your changes will not be overwritten

multisurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "multisurvivalClass",
    inherit = multisurvivalBase,
    private = list(
        .run = function() {

            # TODO ----

            todo <- glue::glue(
                "This Module is still under development
                - "
            )

            self$results$todo$setContent(todo)


            if (length(self$options$explanatory) < 1 |  (length(self$options$outcome) + length(self$options$overalltime) < 2))
                return()




            # Check if outcome variable is suitable or stop ----
            myoutcome2 <- self$options$outcome
            myoutcome2 <- self$data[[myoutcome2]]
            myoutcome2 <- na.omit(myoutcome2)
            if (any(myoutcome2 != 0 & myoutcome2 != 1))
                stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')








            mydata <- self$data

            formula2 <- as.vector(self$options$explanatory)

            # formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)
            #
            # formula2 <- jmvcore::decomposeTerms(formula2)


            # self$results$todo2$setContent(formula2)

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

        },


        .plot=function(image, ...) {  # <-- the plot function ----

            # plotData <- image$state

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Check if outcome variable is suitable or stop ----
            myoutcome2 <- self$options$outcome
            myoutcome2 <- self$data[[myoutcome2]]
            myoutcome2 <- na.omit(myoutcome2)
            if (any(myoutcome2 != 0 & myoutcome2 != 1))
                stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')



            mydata <- self$data

            formula2 <- as.vector(self$options$explanatory)

            formulaL <- jmvcore::constructFormula(terms = self$options$overalltime)

            formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

            myformula <- paste("Surv(", formulaL, ",", formulaR, ")")


            plot <- mydata %>%
                finalfit::hr_plot(dependent = myformula,
                        explanatory = formula2,
                        dependent_label = "Survival",
                        table_text_size = 4,
                        title_text_size = 14,
                        plot_opts = list(ggplot2::xlab("HR, 95% CI"),
                                         ggplot2::theme(axis.title = ggplot2::element_text(size = 12)
                                               )))

            # https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcoefstats.html#cox-proportional-hazards-regression-model-coxph
            # # fit a stratified model
            # mod <-
            #     survival::coxph(
            #         formula = Surv(time, status) ~ x + strata(sex),
            #         data = test1
            #     )
            #
            # # plot
            # ggstatsplot::ggcoefstats(
            #     x = mod,
            #     exponentiate = TRUE,
            #     title = "Cox proportional hazards regression model"
            # )


            print(plot)
            TRUE



        }

        )
)
