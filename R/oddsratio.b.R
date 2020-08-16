#' @title Odds Ratio Table and Plot
#'
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'

oddsratioClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "oddsratioClass",
    inherit = oddsratioBase,
    private = list(
        .run = function() {


            # # Error Message ----
            #
            # if (nrow(self$data) == 0) stop("Data contains no (complete) rows")
            #
            # if ( (is.null(self$options$vars) || is.null(self$options$facs)) && is.null(self$options$target) ) {
            #     # ToDo Message ----
            #     todo <- "
            #         <br>Welcome to ClinicoPath
            #                   <br><br>
            #                   This tool will help you form an Alluvial Plots.
            #                   "
            #     html <- self$results$todo
            #     html$setContent(todo)
            #
            # } else {
            #     todo <- ""
            #     html <- self$results$todo
            #     html$setContent(todo)
            #
            #
            #
            # }







            # If no variable selected Initial Message ----

            if (is.null(self$options$explanatory) || is.null(self$options$outcome))
            {

                # TODO ----

                todo <- glue::glue("
                    <br>Welcome to ClinicoPath
                    <br><br>
                        This tool will help you produce an odds ratio table and plot.
                    <br><br>
                        Explanatory variables can be categorical (ordinal or nominal) or continuous.
                    <br><br>
                        Outcome variable should be coded binary, defining whether the patient is dead or event (recurrence) occured
                    or censored (patient is alive or free of disease) at the last visit.
                    <br><br>
                        This function uses finalfit package. Please cite jamovi and the packages as given below.
                    <br><br>
                    ")

                # https://finalfit.org/articles/all_tables_examples.html#default-1

                html <- self$results$todo
                html$setContent(todo)
                return()

            } else {

                # Empty message when all variables selected

                todo <- ""

                # glue::glue("Analysis based on:
                # <br>
                # glm(depdendent ~ explanatory, family='binomial')
                # <br>
                #     ")

                html <- self$results$todo
                html$setContent(todo)


                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')


                # Check if outcome variable is suitable or stop ----
                myoutcome2 <- self$options$outcome
                myoutcome2 <- self$data[[myoutcome2]]
                myoutcome2 <- na.omit(myoutcome2)

                # if (class(myoutcome2) == "factor")
                #     stop("Please use a continuous variable for outcome.")
                #
                # if (any(myoutcome2 != 0 & myoutcome2 != 1))
                #     stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')

                mydata <- self$data

                formula2 <- as.vector(self$options$explanatory)

                formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

                # formulaR <- jmvcore::toNumeric(formulaR)


                # results1 <- list(
                #     formulaR,
                #     formula2
                # )
                #
                # self$results$text$setContent(results1)


                # glm(depdendent ~ explanatory, family="binomial")

                finalfit::finalfit(.data = mydata,
                                   dependent = formulaR,
                                   explanatory = formula2,
                                   metrics = TRUE
                                   ) -> tOdds


                text2 <- glue::glue("
                                <br>
                                <b>Model Metrics:</b>
                                  ",
                                unlist(
                                    tOdds[[2]]
                                ),
                                "
                                <br>
                                ")


                self$results$text2$setContent(text2)


                results1 <-  knitr::kable(tOdds[[1]],
                             row.names=FALSE,
                             align=c("l", "l", "r", "r", "r", "r"),
                             format = "html")

                self$results$text$setContent(results1)

            }

        }

        ,

        .plot = function(image, ggtheme, theme, ...) {  # <-- the plot function ----

            # plotData <- image$state

            if (is.null(self$options$explanatory) || is.null(self$options$outcome))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Check if outcome variable is suitable or stop ----
            myoutcome2 <- self$options$outcome
            myoutcome2 <- self$data[[myoutcome2]]
            myoutcome2 <- na.omit(myoutcome2)

            # if (class(myoutcome2) == "factor")
            #     stop("Please use a continuous variable for outcome.")
            #
            #
            # if (any(myoutcome2 != 0 & myoutcome2 != 1))
            #     stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')

            mydata <- self$data

            formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)

            formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

            # formulaR <- jmvcore::toNumeric(formulaR)


            # https://finalfit.org/reference/or_plot.html

            plot <-
                # finalfit::or_plot(
                finalfit::ff_plot(
                    .data = mydata,
                    dependent = formulaR,
                    explanatory = formula2,
                    remove_ref = FALSE,
                    table_text_size = 4,
                    title_text_size = 14,
                    random_effect = NULL,
                    factorlist = NULL,
                    glmfit = NULL,
                    confint_type = NULL,
                    breaks = NULL,
                    column_space = c(-0.5, 0, 0.5),
                    dependent_label = self$options$outcome,
                    prefix = "",
                    suffix = ": OR (95% CI, p-value)",
                    table_opts = NULL,
                    plot_opts = list(
                    ggplot2::xlab("OR, 95% CI"),
                    ggplot2::theme(
                    axis.title = ggplot2::element_text(size = 12)
                    )
                    )
                    )





            print(plot)
            TRUE
        }
#
#         ,
#
#         .plot2 = function(image, ggtheme, theme, ...) {  # <-- the plot function ----
#
#             # plotData <- image$state
#
#             if (nrow(self$data) == 0)
#                 stop('Data contains no (complete) rows')
#
#             if (is.null(self$options$explanatory) || is.null(self$options$outcome))
#                 return()
#
#             # Check if outcome variable is suitable or stop ----
#             myoutcome2 <- self$options$outcome
#             myoutcome2 <- self$data[[myoutcome2]]
#             myoutcome2 <- na.omit(myoutcome2)
#
#             if (class(myoutcome2) == "factor")
#                 stop("Please use a continuous variable for outcome.")
#
#             if (any(myoutcome2 != 0 & myoutcome2 != 1))
#                 stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')
#
#
#
#
#             mydata <- self$data
#
#             formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)
#
#             formulaR <- jmvcore::constructFormula(terms = self$options$outcome)
#
#             formulaR <- jmvcore::toNumeric(formulaR)
#
#             formula <- paste0(formula2, ' ~ ', formulaR)
#
#             formula <- as.formula(formula)
#
#             # https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcoefstats.html#generalized-linear-model-glm-
#
#
#         # model
#         mod <-
#             stats::glm(
#                 formula = formula,
#                 data = mydata,
#                 # weights = df$Freq,
#                 family = stats::binomial(link = "logit")
#             )
#
#         # plot
#         plot <- ggstatsplot::ggcoefstats(
#             x = mod,
#             ggtheme = ggthemes::theme_economist_white(),
#             ggstatsplot.layer = FALSE,
#             title = "generalized linear model (glm)",
#             vline.args = list(color = "red", linetype = "solid"),
#             stats.label.color = c("orangered", "dodgerblue")
#         )
#
#         print(plot)
#         TRUE
#
# }
#
#






        )
)
