#' Multivariate Survival Analysis
#'


#'
#' 
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import finalfit
#' @import survival
#' @import survminer
#' @import ggplot2
#'

multisurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "multisurvivalClass",
    inherit = multisurvivalBase,
    private = list(
        .run = function() {


            # If no variable selected Initial Message ----

            if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$overalltime) )
            {

                # TODO ----

                todo <- glue::glue("
                    <br>Welcome to ClinicoPath
                    <br><br>
                        This tool will help you perform a multivariate survival analysis.
                    <br><br>
                        Explanatory variables can be categorical (ordinal or nominal) or continuous.
                    <br><br>
                        Outcome variable should be coded binary (0 or 1).
                    <br><br>
                        If patient is dead or event (recurrence) occured it is 1.
                    <br><br>
                        If censored (patient is alive or free of disease) at the last visit it is 0.
                    <br><br>
                        Survival should be numeric, continuous, and in months.
                    <br><br>
                        This function uses finalfit and ggstatsplot packages. Please cite jamovi and the packages as given below.
                    <br><br>
                    ")
                # https://finalfit.org/articles/all_tables_examples.html#cox-proportional-hazards-model-survival-time-to-event


                html <- self$results$todo
                html$setContent(todo)
                return()

            } else {

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')



            # Check if outcome variable is suitable or stop ----
            myoutcome2 <- self$options$outcome
            myoutcome2 <- self$data[[myoutcome2]]
            myoutcome2 <- na.omit(myoutcome2)

            if (class(myoutcome2) == "factor")
                stop("Please use a continuous variable for outcome.")

            if (any(myoutcome2 != 0 & myoutcome2 != 1))
                stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')



            mydata <- self$data

            formula2 <- as.vector(self$options$explanatory)

            formulaL <- jmvcore::constructFormula(terms = self$options$overalltime)

            formulaL <- jmvcore::toNumeric(formulaL)

            # formulaL <- jmvcore::constructFormula(terms = self$options$overalltime)

            formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

            formulaR <- jmvcore::toNumeric(formulaR)


            myformula <- paste("Surv(", formulaL, ",", formulaR, ")")

            # results1 <- list(
            #     formula2,
            #     formulaL,
            #     formulaR,
            #     myformula
            # )
            #
            # self$results$text$setContent(results1)


            finalfit::finalfit(.data = mydata,
                               dependent = myformula,
                               explanatory = formula2,
                               # ,
                               # metrics = TRUE
                               ) -> tMultivariate

            results1 <- knitr::kable(tMultivariate,
                                     row.names = FALSE,
                                     align = c('l', 'l', 'r', 'r', 'r', 'r'),
                                     format = "html")

            self$results$text$setContent(results1)

            }

        },

        .plot = function(image, ...) {  # <-- the plot function ----

            # plotData <- image$state

            if (is.null(self$options$explanatory) || (length(self$options$outcome) + length(self$options$overalltime) < 2))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Check if outcome variable is suitable or stop ----
            myoutcome2 <- self$options$outcome
            myoutcome2 <- self$data[[myoutcome2]]
            myoutcome2 <- na.omit(myoutcome2)

            if (class(myoutcome2) == "factor")
                stop("Please use a continuous variable for outcome.")


            if (any(myoutcome2 != 0 & myoutcome2 != 1))
                stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')

            mydata <- self$data

            formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)

            formulaL <- jmvcore::constructFormula(terms = self$options$overalltime)

            formulaL <- jmvcore::toNumeric(formulaL)

            formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

            formulaR <- jmvcore::toNumeric(formulaR)


            myformula <- paste("Surv(", formulaL, ",", formulaR, ")")


            # https://finalfit.org/reference/hr_plot.html

            plot <-
                finalfit::hr_plot(
                    .data = mydata,
                    dependent = myformula,
                    explanatory = formula2,
                    dependent_label = "Survival",
                    table_text_size = 4,
                    title_text_size = 14,
                    plot_opts = list(ggplot2::xlab("HR, 95% CI"),
                                     ggplot2::theme(
                                         axis.title =
                                             ggplot2::element_text(size = 12)
                                               )))

            print(plot)
            TRUE

        },

            .plot2 = function(image, ...) {  # <-- the plot function ----

            # plotData <- image$state

                if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$overalltime) )
                    return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Check if outcome variable is suitable or stop ----
            myoutcome2 <- self$options$outcome
            myoutcome2 <- self$data[[myoutcome2]]
            myoutcome2 <- na.omit(myoutcome2)

            if (class(myoutcome2) == "factor")
                stop("Please use a continuous variable for outcome.")

            if (any(myoutcome2 != 0 & myoutcome2 != 1))
                stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')

            # https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcoefstats.html#cox-proportional-hazards-regression-model-coxph
            # fit a stratified model

            mydata <- self$data

            formulaL <- jmvcore::constructFormula(terms = self$options$overalltime)

            formulaL <- jmvcore::toNumeric(formulaL)

            formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

            formulaR <- jmvcore::toNumeric(formulaR)

            formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)

            formula3 <- paste("Surv(", formulaL, ",", formulaR, ") ~ ", formula2)

            formula3 <- as.formula(formula3)

            mod <-
                survival::coxph(
                    formula = formula3,
                    data = mydata
                )

            # plot
            plot2 <- ggstatsplot::ggcoefstats(
                x = mod,
                exponentiate = TRUE,
                title = "Cox proportional hazards regression model"
            )

            print(plot2)
            TRUE

            }

        )
)
