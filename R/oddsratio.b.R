#' @title Odds Ratio Table and Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#'

oddsratioClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "oddsratioClass",
    inherit = oddsratioBase,
    private = list(

        .nom_object = NULL,


        .run = function() {


            # # Error Message
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







            # Initial Message ----

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



                # Check if outcome variable is suitable or stop
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




                ## plot Data ----
                plotData <- list(
                    "plotData" = mydata,
                    "formulaDependent" = formulaDependent,
                    "formulaExplanatory" = formulaExplanatory
                )

                image <- self$results$plot
                image$setState(plotData)




                if (self$options$showNomogram) {
                    # Calculate likelihood ratios
                    lr_results <- private$.calculateLikelihoodRatios(
                        mydata,
                        dependent_variable_name_from_label,
                        explanatory_variable_names[1]  # Start with first variable
                    )

                    # Create diagnostic metrics text
                    metrics_text <- glue::glue("
                    <br>
                    <b>Diagnostic Metrics:</b><br>
                    Sensitivity: {format(lr_results$sensitivity * 100, digits=2)}%<br>
                    Specificity: {format(lr_results$specificity * 100, digits=2)}%<br>
                    Positive LR: {format(lr_results$positive_lr, digits=2)}<br>
                    Negative LR: {format(lr_results$negative_lr, digits=2)}<br>
                    <br>
                ")

                    # Prepare data for nomogram
                    nom_results <- private$.prepareRmsNomogram(
                        mydata,
                        dependent_variable_name_from_label,
                        explanatory_variable_names
                    )

                    # Create nomogram if preparation was successful
                    if (!is.null(nom_results$fit)) {
                        private$.createNomogram(nom_results$fit, nom_results$dd)
                    }

                    # Update results
                    self$results$text2$setContent(paste(text2, metrics_text))
                }



























            }
        }




        ,
        .calculateLikelihoodRatios = function(data, outcome_var, predictor_var) {
            # Create contingency table
            cont_table <- table(data[[predictor_var]], data[[outcome_var]])

            # Calculate sensitivity and specificity
            sensitivity <- cont_table[2,2] / sum(cont_table[,2])
            specificity <- cont_table[1,1] / sum(cont_table[,1])

            # Calculate likelihood ratios
            positive_lr <- sensitivity / (1 - specificity)
            negative_lr <- (1 - sensitivity) / specificity

            return(list(
                positive_lr = positive_lr,
                negative_lr = negative_lr,
                sensitivity = sensitivity,
                specificity = specificity
            ))
        },

        .prepareRmsNomogram = function(data, dependent, explanatory) {
            tryCatch({
                # First create datadist object
                dd <- rms::datadist(data[, explanatory])
                options(datadist = dd)

                # Create formula for model
                formula_str <- paste(dependent, "~", paste(explanatory, collapse = " + "))

                # Fit logistic regression model
                fit <- rms::lrm(
                    formula = as.formula(formula_str),
                    data = data,
                    x = TRUE,
                    y = TRUE
                )

                return(list(fit = fit, dd = dd))
            }, error = function(e) {
                warning(paste("Error preparing nomogram:", e$message))
                return(list(fit = NULL, dd = NULL))
            })
        },

        .createNomogram = function(fit, dd) {
            if (is.null(fit)) return(NULL)

            # Create nomogram
            nom <- try({
                rms::nomogram(fit,
                              fun = stats::plogis,  # Convert from log odds to probability
                              funlabel = "Predicted Probability"
                )
            })

            if (!inherits(nom, "try-error")) {
                private$.nom_object <- nom

                # Create HTML content for display
                html_content <- private$.createNomogramDisplay(nom)
                self$results$nomogram$setContent(html_content)
            }

            # Save model summary and nomogram info for debugging
            # self$results$mydataview_nomogram$setContent(
            #     list(
            #         model_summary = summary(fit),
            #         nomogram = if(!inherits(nom, "try-error")) nom else NULL,
            #         error = if(inherits(nom, "try-error")) attr(nom, "condition") else NULL
            #     )
            # )


            },


        .createNomogramDisplay = function(nom) {
            if(is.null(private$.nom_object)) {
                return(FALSE)
            }

            # Capture the nomogram output
            nom_output <- capture.output(print(nom))

            # Extract technical details
            tech_details <- c()
            i <- 1
            while(i <= length(nom_output) && !grepl("Points$", nom_output[i])) {
                if(nzchar(nom_output[i])) {
                    tech_details <- c(tech_details, nom_output[i])
                }
                i <- i + 1
            }

            # Initialize data structures
            sections <- list()
            current_section <- NULL
            current_lines <- character(0)
            risk_table <- NULL

            # Process each line
            while(i <= length(nom_output)) {
                line <- nom_output[i]

                # Check for new section or risk table
                if(grepl("Total Points Predicted", line)) {
                    # We've hit the risk table - save current section and start collecting risk data
                    if(!is.null(current_section)) {
                        sections[[current_section]] <- current_lines
                    }
                    risk_table <- c(line)  # Start risk table with header
                    while(i < length(nom_output) && nzchar(trimws(nom_output[i + 1]))) {
                        i <- i + 1
                        risk_table <- c(risk_table, nom_output[i])
                    }
                    current_section <- NULL
                    current_lines <- character(0)
                } else if(grepl("Points$", line) && !grepl("Total Points", line)) {
                    # New variable section
                    if(!is.null(current_section)) {
                        sections[[current_section]] <- current_lines
                    }
                    current_section <- trimws(sub("Points$", "", line))
                    current_lines <- character(0)
                } else if(nzchar(trimws(line))) {
                    current_lines <- c(current_lines, line)
                }
                i <- i + 1
            }

            # Add final section if exists
            if(!is.null(current_section) && length(current_lines) > 0) {
                sections[[current_section]] <- current_lines
            }

            # Create HTML content
            html_content <- paste0('
    <style>
        .nomogram-container {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            max-width: 800px;
            margin: 20px auto;
            padding: 20px;
            background-color: #fff;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            border-radius: 8px;
        }
        .tech-details {
            font-family: "Roboto Mono", monospace;
            background-color: #f8f9fa;
            padding: 15px;
            border-radius: 4px;
            margin: 15px 0;
            color: #666;
        }
        .instructions {
            background-color: #e8f5e9;
            padding: 20px;
            margin: 20px 0;
            border-radius: 8px;
        }
        .inputs-section {
            margin-top: 30px;
            border: 1px solid #e9ecef;
            border-radius: 8px;
            padding: 20px;
        }
        .variable-section {
            margin: 15px 0;
            padding: 15px;
            background-color: #f8f9fa;
            border-left: 4px solid #2196f3;
            border-radius: 4px;
        }
        .section-title {
            font-size: 1.2em;
            font-weight: 600;
            color: #2c3e50;
            margin-bottom: 10px;
        }
        .values {
            font-family: "Roboto Mono", monospace;
            white-space: pre-wrap;
            line-height: 1.5;
            color: #34495e;
            padding-left: 20px;
        }
        .outputs-section {
            margin-top: 30px;
            background-color: #fff3e0;
            border: 1px solid #ffe0b2;
            border-radius: 8px;
            padding: 20px;
        }
        .prediction-section {
            background-color: #fff3e0;
            border-left: 4px solid #ff9800;
            padding: 20px;
        }
        .notes {
            background-color: #fffde7;
            padding: 15px;
            margin-top: 20px;
            border-radius: 4px;
        }
        .risk-table {
            width: 100%;
            border-collapse: separate;
            border-spacing: 0;
            margin-top: 10px;
            font-family: "Roboto Mono", monospace;
        }
        .risk-table th, .risk-table td {
            padding: 8px 16px;
            text-align: right;
            border-bottom: 1px solid #ddd;
        }
        .risk-table th {
            font-weight: 600;
            background-color: #fff3e0;
            text-align: center;
        }
        .risk-header {
            padding: 10px 0;
            margin-bottom: 10px;
            border-bottom: 2px solid #ff9800;
            font-weight: 600;
        }
    </style>
    <div class="nomogram-container">
        <h2>Nomogram Scoring Guide</h2>

        <div class="tech-details">
            ', paste(tech_details, collapse="<br>"), '
        </div>

        <div class="instructions">
            <h3>How to Use This Nomogram:</h3>
            <ol>
                <li>For each variable below, find your patient\'s value</li>
                <li>Read across to the Points scale to determine points for that variable</li>
                <li>Add up total points from all variables</li>
                <li>Use total points to find predicted risk in the Risk Prediction section</li>
            </ol>
        </div>

        <div class="inputs-section">
            <h3>Input Variables</h3>')

            # Add variable sections
            for(section_name in names(sections)) {
                html_content <- paste0(html_content, '
            <div class="variable-section">
                <div class="section-title">', section_name, '</div>
                <div class="values">',
                                       paste(sections[[section_name]], collapse="<br>"),
                                       '</div>
            </div>')
            }

            # Add risk prediction section
            html_content <- paste0(html_content, '
        </div>

        <div class="outputs-section">
            <h3>Risk Prediction</h3>
            <div class="prediction-section">
                <div class="risk-header">Points to Risk Conversion</div>
                <div class="values">',
                                   if(!is.null(risk_table)) paste(risk_table, collapse="<br>") else "",
                                   '</div>
            </div>
        </div>

        <div class="notes">
            <h3>Important Notes:</h3>
            <ul>
                <li>For continuous variables, interpolate between given values</li>
                <li>For categorical variables, use exact points shown</li>
                <li>The predicted risk is based on total points from all variables</li>
                <li>Risk predictions are estimates and should be used in conjunction with clinical judgment</li>
            </ul>
        </div>
    </div>')

            return(html_content)
        }

        ,
        # Plotting nomogram
        .plot_nomogram = function(image, ggtheme, theme, ...) {
            if(is.null(private$.nom_object)) {
                return(FALSE)
            }

            par(mar = c(4, 4, 2, 2))
            plot(private$.nom_object)
            return(TRUE)
        }








        # plot ----
        ,
        .plot = function(image, ggtheme, theme, ...) {
          # -- the plot function ----
                    # plotData <- image$state
                    if (is.null(self$options$explanatory) || is.null(self$options$outcome))
                return()
                    if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')
            # Check if outcome variable is suitable or stop
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
#         .plot2 = function(image, ggtheme, theme, ...) {
#
#             # plotData <- image$state
#
#             if (nrow(self$data) == 0)
#                 stop('Data contains no (complete) rows')
#
#             if (is.null(self$options$explanatory) || is.null(self$options$outcome))
#                 return()
#
#             # Check if outcome variable is suitable or stop
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
