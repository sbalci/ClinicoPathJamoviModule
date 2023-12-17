#' @title Odds Ratio Table and Plot
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
                        Variable names with empty spaces or special characters may not work properly. Consider renaming them.
                    <br><br>
                        This function uses finalfit package. Please cite jamovi and the packages as given below.
                    <br><br>
                    ")

                # https://finalfit.org/articles/all_tables_examples.html#default-1

                html <- self$results$todo
                html$setContent(todo)
                self$results$text$setVisible(FALSE)
                self$results$text2$setVisible(FALSE)
                self$results$plot$setVisible(FALSE)
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

                mydata <- self$data

                mydata <- jmvcore::naOmit(mydata)


                # histopathology <- jmvReadWrite::read_omv("~/Downloads/histopathology including analysis.omv")

                original_names <- names(mydata)

                # Save original names as a named vector where the names are the original names,
                # and the values are the labels you want to set, which are also the original names.
                labels <- setNames(original_names, original_names)

                # Clean variable names
                mydata <- mydata %>% janitor::clean_names()

                # Now apply the labels to the cleaned names.
                # Since the variable names have been cleaned, you must match the labels to the cleaned names.
                # The labels vector should have names that are the cleaned names and values that are the original names.
                corrected_labels <- setNames(original_names, names(mydata))

                # Apply the corrected labels
                mydata <- labelled::set_variable_labels(
                    .data = mydata,
                    .labels = corrected_labels)

                # Retrieve all variable labels
                all_labels <- labelled::var_label(mydata)

                # Retrieve the variable name from the label
                dependent_variable_name_from_label <- names(all_labels)[all_labels == self$options$outcome]

                # Retrieve the variable names vector from the label vector
                labels <- self$options$explanatory

                explanatory_variable_names <- names(all_labels)[match(labels, all_labels)]


                formulaDependent <- jmvcore::constructFormula(
                    terms = dependent_variable_name_from_label)

                formulaExplanatory <- jmvcore::composeTerms(
                    listOfComponents = explanatory_variable_names
                )

                # formulaExplanatory <- paste0(formulaExplanatory, collapse = " + ")

                # myformula <- paste0(formulaDependent, " ~ ", formulaExplanatory)

                # myformula <- jmvcore::composeFormula(lht = formulaDependent,
                #                                      rht = formulaExplanatory)

                # myformula <- as.formula(myformula)


                finalfit::finalfit(.data = mydata,
                                   dependent = formulaDependent,
                                   explanatory = formulaExplanatory,
                                   # formula = myformula,
                                   metrics = TRUE
                                   ) -> tOdds







                # outcomeLevel <- self$options$outcomeLevel
                # outcome_name <- self$options$outcome

                # outcome1 <- self$data[[outcome_name]]

                # mydata[["outcome2"]] <-
                #     ifelse(
                #         test = outcome1 == outcomeLevel,
                #         yes = "Event",
                #         no = "NoEvent"
                #     )


                # mydata[[outcome_name]] <-
                #     ifelse(
                #         test = outcome1 == outcomeLevel,
                #         yes = "Event",
                #         no = "NoEvent"
                #     )





                # self$results$textmydata$setContent(
                #     list(
                #         outcomeLevel,
                #         outcome_name,
                #         outcome1,
                #         head(mydata)
                #         )
                # )



                # Check if outcome variable is suitable or stop ----
                # myoutcome2 <- self$options$outcome
                # myoutcome2 <- self$data[[myoutcome2]]
                # myoutcome2 <- na.omit(myoutcome2)

                # if (class(myoutcome2) == "factor")
                #     stop("Please use a continuous variable for outcome.")
                #
                # if (any(myoutcome2 != 0 & myoutcome2 != 1))
                #     stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')


                # formula2 <- as.vector(self$options$explanatory)

                # formulaR <- jmvcore::constructFormula(terms =
                #                                           # outcome_name
                #                                       self$options$outcome
                #                                       )

                # formulaR2 <- jmvcore::composeTerm(components = outcome_name)

                # formulaR3 <- as.vector(self$options$outcome)

                # formulaL <- jmvcore::composeTerms(listOfComponents =
                #                                       self$options$explanatory)

                # formulaL <- as.vector(formulaL)


                # formula2 <- jmvcore::constructFormula(terms = formulaL)

                # formulaL2 <- jmvcore::constructFormula(terms =
                #                                       self$options$explanatory)

                # formulaR <- jmvcore::toNumeric(formulaR)


                # glm(depdendent ~ explanatory, family="binomial")

                # finalfit::finalfit(.data = mydata,
                #                    dependent = formulaR,
                #                    explanatory = formula2,
                #                    metrics = TRUE
                #                    ) -> tOdds


                # self$results$textmydata$setContent(
                #     list(
                #         head = head(mydata),
                #         names_data = names(mydata),
                #         all_labels = all_labels,
                #         explanatory_variable_names = explanatory_variable_names,
                #         dependent_variable_name_from_label = dependent_variable_name_from_label,
                #         formulaDependent = formulaDependent,
                #         formulaExplanatory = formulaExplanatory
                #         # formula2 = formula2,
                #         # formulaR = formulaR,
                #         # formulaL = formulaL,
                #         # formulaL2 = formulaL2,
                #         # formulaR3,
                #         ,
                #         tOdds
                #     )
                # )


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
                             row.names = FALSE,
                             align = c("l", "l", "r", "r", "r", "r"),
                             format = "html")
                self$results$text$setContent(results1)




                plotData <- list(
                    "plotData" = mydata,
                    "formulaDependent" = formulaDependent,
                    "formulaExplanatory" = formulaExplanatory
                )

                image <- self$results$plot
                image$setState(plotData)
















            }

        }

        ,
                .plot = function(image, ggtheme, theme, ...) {
          # -- the plot function ----
                    # plotData <- image$state
                    if (is.null(self$options$explanatory) || is.null(self$options$outcome))
                return()
                    if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')
            # Check if outcome variable is suitable or stop ----
            # myoutcome2 <- self$options$outcome
            # myoutcome2 <- self$data[[myoutcome2]]
            # myoutcome2 <- na.omit(myoutcome2)
                    # if (class(myoutcome2) == "factor")
            #     stop("Please use a continuous variable for outcome.")
            #
            #
            # if (any(myoutcome2 != 0 & myoutcome2 != 1))
            #     stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')
                    # mydata <- self$data
                    # formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)
                    # formulaR <- jmvcore::constructFormula(terms = self$options$outcome)
                    # formulaR <- jmvcore::toNumeric(formulaR)
                    # https://finalfit.org/reference/or_plot.html

                    plotList <- image$state

                    mydata <- plotList$plotData
                    formulaDependent <- plotList$formulaDependent
                    formulaExplanatory <- plotList$formulaExplanatory

                    plot <-
                        # finalfit::or_plot(
                        finalfit::ff_plot(
                            .data = mydata,
                            dependent = formulaDependent,
                            explanatory = formulaExplanatory,
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
