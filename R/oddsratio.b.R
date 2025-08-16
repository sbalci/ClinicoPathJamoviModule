#' @title Odds Ratio Analysis for Binary Outcomes
#' @description
#' Performs logistic regression analysis to calculate odds ratios for binary outcomes.
#' This function provides comprehensive odds ratio tables, forest plots, and optional
#' nomogram generation for clinical prediction. It supports both categorical and
#' continuous explanatory variables and includes diagnostic metrics for binary predictors.
#'
#' @details
#' The function performs the following analyses:
#' \itemize{
#'   \item Logistic regression using finalfit package
#'   \item Odds ratio calculation with 95% confidence intervals
#'   \item Forest plot generation for visualization
#'   \item Optional nomogram creation for clinical prediction
#'   \item Likelihood ratio calculations for diagnostic metrics
#'   \item Sensitivity and specificity analysis for binary predictors
#' }
#'
#' The function automatically cleans variable names using janitor::clean_names()
#' and preserves original variable labels for display. It handles missing data
#' through complete case analysis.
#'
#' @section International Usage:
#' For international users, the function includes an outcomeLevel parameter to
#' explicitly specify which outcome level represents the positive case. This is
#' important for correct interpretation of likelihood ratios and diagnostic metrics.
#'
#' @section Nomogram Features:
#' When showNomogram is enabled, the function generates:
#' \itemize{
#'   \item Interactive nomogram for risk prediction
#'   \item Diagnostic metrics (sensitivity, specificity, likelihood ratios)
#'   \item Contingency table analysis
#'   \item User guidance for interpretation
#' }
#'
#' @examples
#' \dontrun{
#' # Basic odds ratio analysis
#' result <- oddsratio(
#'   data = clinical_data,
#'   explanatory = c("age", "gender", "smoking"),
#'   outcome = "mortality"
#' )
#'
#' # With nomogram and specified outcome level
#' result <- oddsratio(
#'   data = clinical_data,
#'   explanatory = c("age", "treatment"),
#'   outcome = "recurrence",
#'   outcomeLevel = "Yes",
#'   showNomogram = TRUE
#' )
#' }
#'
#' @references
#' Harrison, E., Drake, T., & Ots, R. (2019). finalfit: Quickly create elegant
#' regression results tables and plots when modelling. R package version 0.9.7.
#'
#' @author ClinicoPath Development Team
#' @seealso \code{\link{finalfit}}, \code{\link{rms}}
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'

oddsratioClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "oddsratioClass",
    inherit = oddsratioBase,
    private = list(

        .nom_object = NULL,

        # init ----
        .init = function() {
            # Initialize all explanation outputs to FALSE first
            self$results$oddsRatioExplanation$setVisible(FALSE)
            self$results$riskMeasuresExplanation$setVisible(FALSE)
            self$results$diagnosticTestExplanation$setVisible(FALSE)
            self$results$nomogramAnalysisExplanation$setVisible(FALSE)

            # Handle showExplanations visibility
            if (self$options$showExplanations) {
                # Section headings for explanations
                self$results$oddsRatioHeading3$setVisible(TRUE)
                
                # Explanation content
                self$results$oddsRatioExplanation$setVisible(TRUE)
                self$results$riskMeasuresExplanation$setVisible(TRUE)
                self$results$diagnosticTestExplanation$setVisible(TRUE)
                
                # Nomogram explanation requires both showExplanations AND showNomogram
                if (self$options$showNomogram) {
                    self$results$nomogramAnalysisExplanation$setVisible(TRUE)
                }
            }

            # Note: Tables and plots remain visible based on data availability and their specific options
            # Nomogram plots are controlled by showNomogram option in the .r.yaml
        },

        # Enhanced input validation for data quality and user inputs
        # Returns validation results with errors, warnings, and informational messages
        .validateInputs = function(mydata, dependent_var, explanatory_vars, user_outcome_level = NULL) {
            validation_results <- list(
                errors = character(0),
                warnings = character(0),
                info = character(0),
                should_stop = FALSE
            )
            
            # 1. Binary outcome validation
            if (!is.null(dependent_var) && dependent_var %in% names(mydata)) {
                outcome_data <- mydata[[dependent_var]]
                outcome_data <- outcome_data[!is.na(outcome_data)]
                
                if (length(outcome_data) == 0) {
                    validation_results$errors <- c(validation_results$errors,
                        "Outcome variable contains no non-missing values.")
                    validation_results$should_stop <- TRUE
                } else {
                    # Check if outcome is factor or can be converted to factor
                    if (is.factor(outcome_data)) {
                        outcome_levels <- levels(outcome_data)
                        outcome_counts <- table(outcome_data)
                    } else {
                        unique_vals <- unique(outcome_data)
                        outcome_levels <- as.character(unique_vals)
                        outcome_counts <- table(outcome_data)
                    }
                    
                    # Check for binary nature
                    if (length(outcome_levels) < 2) {
                        validation_results$errors <- c(validation_results$errors,
                            "Outcome variable must have at least 2 different values for logistic regression.")
                        validation_results$should_stop <- TRUE
                    } else if (length(outcome_levels) > 2) {
                        validation_results$errors <- c(validation_results$errors,
                            paste("Outcome variable has", length(outcome_levels), "levels. For odds ratio analysis, the outcome must be binary (exactly 2 levels). Consider creating a binary variable or using multinomial regression."))
                        validation_results$should_stop <- TRUE
                    } else {
                        # Binary outcome - check for severe imbalance
                        min_count <- min(outcome_counts)
                        total_count <- sum(outcome_counts)
                        min_proportion <- min_count / total_count
                        
                        if (min_count < 5) {
                            validation_results$warnings <- c(validation_results$warnings,
                                paste("Outcome variable has very few observations in one category (", min_count, " out of ", total_count, "). Results may be unreliable.", sep=""))
                        } else if (min_proportion < 0.05) {
                            validation_results$warnings <- c(validation_results$warnings,
                                paste("Outcome variable is severely imbalanced (", round(min_proportion * 100, 1), "% in minority class). Consider using specialized methods for imbalanced data.", sep=""))
                        }
                        
                        # Validate user-specified outcome level
                        if (!is.null(user_outcome_level)) {
                            if (!user_outcome_level %in% outcome_levels) {
                                validation_results$warnings <- c(validation_results$warnings,
                                    paste("Specified positive outcome level '", user_outcome_level, "' not found in outcome variable. Available levels: ", paste(outcome_levels, collapse=", "), sep=""))
                            }
                        }
                        
                        validation_results$info <- c(validation_results$info,
                            paste("Outcome variable summary: ", paste(names(outcome_counts), "=", outcome_counts, collapse=", "), sep=""))
                    }
                }
            }
            
            # 2. Explanatory variable validation
            if (length(explanatory_vars) > 0) {
                for (var_name in explanatory_vars) {
                    if (var_name %in% names(mydata)) {
                        var_data <- mydata[[var_name]]
                        var_data_clean <- var_data[!is.na(var_data)]
                        
                        if (length(var_data_clean) == 0) {
                            validation_results$warnings <- c(validation_results$warnings,
                                paste("Explanatory variable '", var_name, "' contains no non-missing values.", sep=""))
                        } else if (length(unique(var_data_clean)) == 1) {
                            validation_results$warnings <- c(validation_results$warnings,
                                paste("Explanatory variable '", var_name, "' has no variation (all values are the same). It will not contribute to the model.", sep=""))
                        } else if (is.factor(var_data)) {
                            # Factor variable validation
                            factor_levels <- levels(var_data)
                            factor_counts <- table(var_data)
                            
                            if (length(factor_levels) > 10) {
                                validation_results$warnings <- c(validation_results$warnings,
                                    paste("Explanatory variable '", var_name, "' has ", length(factor_levels), " levels. Consider grouping categories or using as continuous if ordinal.", sep=""))
                            }
                            
                            # Check for sparse categories
                            sparse_categories <- sum(factor_counts < 5)
                            if (sparse_categories > 0) {
                                validation_results$warnings <- c(validation_results$warnings,
                                    paste("Explanatory variable '", var_name, "' has ", sparse_categories, " categories with fewer than 5 observations. Consider combining categories.", sep=""))
                            }
                        } else if (is.numeric(var_data)) {
                            # Numeric variable validation
                            if (any(is.infinite(var_data_clean))) {
                                validation_results$warnings <- c(validation_results$warnings,
                                    paste("Explanatory variable '", var_name, "' contains infinite values.", sep=""))
                            }
                            
                            # Check for extreme values
                            q99 <- quantile(var_data_clean, 0.99, na.rm = TRUE)
                            q01 <- quantile(var_data_clean, 0.01, na.rm = TRUE)
                            extreme_high <- sum(var_data_clean > q99 + 3 * (q99 - q01), na.rm = TRUE)
                            extreme_low <- sum(var_data_clean < q01 - 3 * (q99 - q01), na.rm = TRUE)
                            
                            if (extreme_high + extreme_low > 0) {
                                validation_results$info <- c(validation_results$info,
                                    paste("Explanatory variable '", var_name, "' may contain extreme outliers (", extreme_high + extreme_low, " potential outliers).", sep=""))
                            }
                        }
                    }
                }
            }
            
            # 3. Data quality checks
            total_rows <- nrow(mydata)
            complete_rows <- sum(complete.cases(mydata))
            missing_proportion <- (total_rows - complete_rows) / total_rows
            
            if (missing_proportion > 0.1) {
                validation_results$warnings <- c(validation_results$warnings,
                    paste("Large amount of missing data: ", round(missing_proportion * 100, 1), "% of rows will be removed (", total_rows - complete_rows, " out of ", total_rows, " rows).", sep=""))
            } else if (missing_proportion > 0) {
                validation_results$info <- c(validation_results$info,
                    paste("Missing data: ", round(missing_proportion * 100, 1), "% of rows will be removed (", total_rows - complete_rows, " out of ", total_rows, " rows).", sep=""))
            }
            
            if (complete_rows < 50) {
                validation_results$warnings <- c(validation_results$warnings,
                    paste("Small sample size after removing missing data: ", complete_rows, " observations. Results may be unreliable.", sep=""))
            }
            
            # Check for perfect separation risk
            if (complete_rows < length(explanatory_vars) * 10) {
                validation_results$warnings <- c(validation_results$warnings,
                    "Sample size is small relative to number of explanatory variables. Risk of overfitting or convergence issues.")
            }
            
            return(validation_results)
        },

        # Main execution function that orchestrates the entire odds ratio analysis
        # Handles data preprocessing, model fitting, and result generation
        .run = function() {









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


                if (nrow(self$data) == 0) {
                    error_msg <- paste(
                        "<div style='background-color: #f8d7da; padding: 15px; border-radius: 8px; margin: 10px 0;'>",
                        "<b>❌ Data Error:</b> No data available for analysis<br><br>",
                        "<b>💡 Possible reasons:</b><br>",
                        "• Dataset has no rows<br>",
                        "• All rows contain missing values<br>",
                        "• Data filtering has removed all observations<br><br>",
                        "<b>🔧 Solutions:</b><br>",
                        "• Check your data import process<br>",
                        "• Verify variable selections<br>",
                        "• Review data quality and missing value patterns<br>",
                        "• Ensure your dataset contains complete observations",
                        "</div>",
                        collapse = ""
                    )
                    stop(error_msg)
                }

                # CHECKPOINT: Before data preprocessing - which can be time-consuming
                private$.checkpoint()

                mydata <- self$data

                # Perform input validation before processing
                validation_results <- private$.validateInputs(
                    mydata,
                    self$options$outcome,
                    self$options$explanatory,
                    self$options$outcomeLevel
                )
                
                # Handle validation errors - stop execution if critical errors found
                if (validation_results$should_stop) {
                    error_msg <- paste(
                        "<div style='background-color: #f8d7da; padding: 15px; border-radius: 8px; margin: 10px 0;'>",
                        "<b>❌ Critical Error(s) Detected:</b><br>",
                        paste(validation_results$errors, collapse = "<br>"),
                        "<br><br><b>💡 Suggestions:</b><br>",
                        "• Check your data format and variable types<br>",
                        "• Ensure outcome variable has exactly 2 levels<br>",
                        "• Verify that explanatory variables have sufficient variation<br>",
                        "• Consider removing rows with missing data",
                        "</div>",
                        collapse = ""
                    )
                    stop(error_msg)
                }
                
                # Create validation summary for display
                validation_summary <- ""
                if (length(validation_results$warnings) > 0) {
                    validation_summary <- paste0(validation_summary, 
                        "<div style='background-color: #fff3cd; padding: 10px; margin: 10px 0; border-radius: 5px;'>",
                        "<b>⚠️ Warnings:</b><br>", 
                        paste(validation_results$warnings, collapse = "<br>"),
                        "</div>")
                }
                if (length(validation_results$info) > 0) {
                    validation_summary <- paste0(validation_summary,
                        "<div style='background-color: #d1ecf1; padding: 10px; margin: 10px 0; border-radius: 5px;'>",
                        "<b>ℹ️ Information:</b><br>", 
                        paste(validation_results$info, collapse = "<br>"),
                        "</div>")
                }

                mydata <- jmvcore::naOmit(mydata)

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

                # CHECKPOINT: Before running finalfit - which can be computationally intensive
                private$.checkpoint()

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


                # Include validation summary in text2 output
                text2_with_validation <- paste0(validation_summary, text2)
                self$results$text2$setContent(text2_with_validation)

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
                    private$.checkpoint()


                    # Calculate likelihood ratios
                    lr_results <- private$.calculateLikelihoodRatios(
                        mydata,
                        dependent_variable_name_from_label,
                        explanatory_variable_names[1],  # Start with first variable
                        self$options$outcomeLevel  # User-specified positive outcome level
                    )

                    # Create diagnostic metrics text with explanatory information
                    metrics_text <- glue::glue("
                    <br>
                    <div style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0;'>
                        <b>Diagnostic Metrics:</b><br>
                        Sensitivity: {format(lr_results$sensitivity * 100, digits=2)}%<br>
                        Specificity: {format(lr_results$specificity * 100, digits=2)}%<br>
                        Positive LR: {format(lr_results$positive_lr, digits=2)}<br>
                        Negative LR: {format(lr_results$negative_lr, digits=2)}<br>
                    </div>
                    
                    <div style='background-color: #e8f5e9; padding: 15px; border-radius: 8px; margin: 10px 0;'>
                        <b>⚠️ Important: Please Verify These Interpretations</b><br>
                        <small>
                        <b>Positive outcome level:</b> '{lr_results$positive_outcome_used}' 
                        <span style='color: #666;'>({lr_results$outcome_determination_method})</span><br>
                        <b>Positive predictor level:</b> '{lr_results$positive_predictor_used}' 
                        <span style='color: #666;'>({lr_results$predictor_determination_method})</span><br><br>
                        
                        <b>📊 Contingency Table:</b><br>
                        <table style='border-collapse: collapse; margin: 5px 0;'>
                            <tr><th style='border: 1px solid #ddd; padding: 5px;'></th>
                                <th style='border: 1px solid #ddd; padding: 5px;'>{names(lr_results$contingency_table)[1]}</th>
                                <th style='border: 1px solid #ddd; padding: 5px;'>{names(lr_results$contingency_table)[2]}</th></tr>
                            <tr><td style='border: 1px solid #ddd; padding: 5px;'><b>{rownames(lr_results$contingency_table)[1]}</b></td>
                                <td style='border: 1px solid #ddd; padding: 5px;'>{lr_results$contingency_table[1,1]}</td>
                                <td style='border: 1px solid #ddd; padding: 5px;'>{lr_results$contingency_table[1,2]}</td></tr>
                            <tr><td style='border: 1px solid #ddd; padding: 5px;'><b>{rownames(lr_results$contingency_table)[2]}</b></td>
                                <td style='border: 1px solid #ddd; padding: 5px;'>{lr_results$contingency_table[2,1]}</td>
                                <td style='border: 1px solid #ddd; padding: 5px;'>{lr_results$contingency_table[2,2]}</td></tr>
                        </table>
                        TP: {lr_results$tp}, FP: {lr_results$fp}, FN: {lr_results$fn}, TN: {lr_results$tn}<br><br>
                        
                        <b>📝 How to Use:</b><br>
                        1. Check that the positive outcome level is correct for your study<br>
                        2. If incorrect, use the 'Positive Outcome Level' dropdown to specify the correct level<br>
                        3. The nomogram calculations depend on these interpretations being correct<br>
                        4. Different languages/coding may require manual specification
                        </small>
                    </div>
                    <br>
                ")

                    private$.checkpoint()

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

                    # Update results with validation information
                    self$results$text2$setContent(paste(text2_with_validation, metrics_text))
                }



























            }

        # Educational Explanations ----
        if (self$options$showExplanations) {
            private$.addExplanations()
        }
        }




        ,
        # Calculates likelihood ratios, sensitivity, and specificity for binary predictors
        # Supports user-specified positive outcome levels for international data
        # Returns diagnostic metrics including sensitivity, specificity, and likelihood ratios
        .calculateLikelihoodRatios = function(data, outcome_var, predictor_var, user_positive_outcome = NULL) {
            # Ensure we have factor variables
            predictor <- factor(data[[predictor_var]])
            outcome <- factor(data[[outcome_var]])
            
            # Create contingency table
            cont_table <- table(predictor, outcome)
            
            # Ensure we have a 2x2 table for binary variables
            if (nrow(cont_table) != 2 || ncol(cont_table) != 2) {
                warning(paste(
                    "⚠️ Likelihood Ratio Calculation Error:",
                    "This calculation requires both predictor and outcome variables to be binary (exactly 2 levels each).",
                    "Current situation:",
                    paste("- Predictor variable '", predictor_var, "' has", nrow(cont_table), "levels"),
                    paste("- Outcome variable '", outcome_var, "' has", ncol(cont_table), "levels"),
                    "📋 Solutions:",
                    "• For continuous variables: Create binary categories (e.g., above/below median)",
                    "• For categorical variables: Combine categories to create binary grouping",
                    "• Use a different analysis method for multi-level variables",
                    sep = "\n"
                ))
                return(list(
                    positive_lr = NA,
                    negative_lr = NA,
                    sensitivity = NA,
                    specificity = NA,
                    diagnostic_info = "Error: Non-binary variables detected",
                    positive_outcome_used = NA,
                    positive_predictor_used = NA
                ))
            }
            
            # Get factor levels
            predictor_levels <- levels(predictor)
            outcome_levels <- levels(outcome)
            
            # Determine positive outcome level
            if (!is.null(user_positive_outcome) && user_positive_outcome %in% outcome_levels) {
                # User specified positive outcome level
                positive_outcome_level <- user_positive_outcome
                positive_outcome_idx <- which(outcome_levels == positive_outcome_level)
                outcome_determination_method <- "User-specified"
            } else {
                # Fallback to automatic detection
                positive_outcome_indicators <- c("Dead", "Event", "Positive", "Yes", "Present", "Recurrence", "1", "TRUE")
                positive_outcome_idx <- which(outcome_levels %in% positive_outcome_indicators)
                
                if (length(positive_outcome_idx) == 1) {
                    positive_outcome_level <- outcome_levels[positive_outcome_idx]
                    outcome_determination_method <- "Automatic detection"
                } else {
                    # Default to second level (alphabetically last)
                    positive_outcome_idx <- 2
                    positive_outcome_level <- outcome_levels[positive_outcome_idx]
                    outcome_determination_method <- "Default (second level alphabetically)"
                }
            }
            
            # Determine positive predictor level (usually second level alphabetically)
            positive_predictor_indicators <- c("Positive", "Yes", "Present", "Exposed", "1", "TRUE", "Bad")
            positive_predictor_idx <- which(predictor_levels %in% positive_predictor_indicators)
            
            if (length(positive_predictor_idx) == 1) {
                positive_predictor_level <- predictor_levels[positive_predictor_idx]
                predictor_determination_method <- "Automatic detection"
            } else {
                # Default to second level (alphabetically last)
                positive_predictor_idx <- 2
                positive_predictor_level <- predictor_levels[positive_predictor_idx]
                predictor_determination_method <- "Default (second level alphabetically)"
            }
            
            # Calculate 2x2 table components
            tp <- cont_table[positive_predictor_idx, positive_outcome_idx]
            fp <- cont_table[positive_predictor_idx, -positive_outcome_idx]
            fn <- cont_table[-positive_predictor_idx, positive_outcome_idx]
            tn <- cont_table[-positive_predictor_idx, -positive_outcome_idx]
            
            # Calculate sensitivity and specificity
            sensitivity <- tp / (tp + fn)  # True Positive Rate
            specificity <- tn / (tn + fp)  # True Negative Rate
            
            # Handle edge cases
            if (is.na(sensitivity) || !is.finite(sensitivity)) {
                sensitivity <- 0
            }
            if (is.na(specificity) || !is.finite(specificity)) {
                specificity <- 0
            }
            
            # Calculate likelihood ratios with proper handling of edge cases
            if (specificity == 1) {
                positive_lr <- Inf
            } else {
                positive_lr <- sensitivity / (1 - specificity)
            }
            
            if (specificity == 0) {
                negative_lr <- Inf
            } else {
                negative_lr <- (1 - sensitivity) / specificity
            }
            
            # Create diagnostic information
            diagnostic_info <- paste0(
                "Positive outcome level: '", positive_outcome_level, "' (", outcome_determination_method, ")\n",
                "Positive predictor level: '", positive_predictor_level, "' (", predictor_determination_method, ")\n",
                "Contingency table:\n",
                "  ", predictor_levels[1], " → ", outcome_levels[1], ": ", cont_table[1,1], 
                " | ", outcome_levels[2], ": ", cont_table[1,2], "\n",
                "  ", predictor_levels[2], " → ", outcome_levels[1], ": ", cont_table[2,1], 
                " | ", outcome_levels[2], ": ", cont_table[2,2], "\n",
                "True Positives: ", tp, ", False Positives: ", fp, ", False Negatives: ", fn, ", True Negatives: ", tn
            )
            
            return(list(
                positive_lr = positive_lr,
                negative_lr = negative_lr,
                sensitivity = sensitivity,
                specificity = specificity,
                diagnostic_info = diagnostic_info,
                positive_outcome_used = positive_outcome_level,
                positive_predictor_used = positive_predictor_level,
                outcome_determination_method = outcome_determination_method,
                predictor_determination_method = predictor_determination_method,
                contingency_table = cont_table,
                tp = tp, fp = fp, fn = fn, tn = tn
            ))
        },

        # Prepares data and fits logistic regression model for nomogram creation
        # Uses rms package to create datadist object and fit lrm model
        .prepareRmsNomogram = function(data, dependent, explanatory) {
            tryCatch({
                # First create datadist object
                dd <- rms::datadist(data[, explanatory])
                options(datadist = dd)

                # Create formula for model
                formula_str <- paste(dependent, "~", paste(explanatory, collapse = " + "))

                # Fit logistic regression model
                private$.checkpoint()

                fit <- rms::lrm(
                    formula = as.formula(formula_str),
                    data = data,
                    x = TRUE,
                    y = TRUE
                )

                return(list(fit = fit, dd = dd))
            }, error = function(e) {
                detailed_error <- paste(
                    "🔧 Nomogram Preparation Error:",
                    paste("Technical error:", e$message),
                    "",
                    "💡 Common causes and solutions:",
                    "• Perfect separation: Some variable levels perfectly predict the outcome",
                    "  → Try combining categories or removing problematic variables",
                    "• Convergence issues: Model failed to converge",
                    "  → Check for multicollinearity or try simpler model",
                    "• Insufficient data: Too few observations per variable",
                    "  → Increase sample size or reduce number of variables",
                    "• Missing values: Incomplete data after cleaning",
                    "  → Review data preprocessing steps",
                    "",
                    "🔄 Suggested next steps:",
                    "• Check model summary for convergence warnings",
                    "• Review variable distributions for separation issues",
                    "• Consider using fewer explanatory variables",
                    "• Verify data quality and completeness",
                    sep = "\n"
                )
                warning(detailed_error)
                return(list(fit = NULL, dd = NULL))
            })
        },

        # Creates nomogram from fitted lrm model and generates HTML display
        .createNomogram = function(fit, dd) {
            if (is.null(fit)) return(NULL)

            private$.checkpoint()

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


        # Generates HTML content for nomogram display with interactive styling
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
        # Plots the nomogram using base R graphics
        .plot_nomogram = function(image, ggtheme, theme, ...) {
            if(is.null(private$.nom_object)) {
                return(FALSE)
            }

            private$.checkpoint()


            par(mar = c(4, 4, 2, 2))
            plot(private$.nom_object)
            return(TRUE)
        }








        # Creates forest plot for odds ratios using finalfit
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

                    private$.checkpoint()

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

        # Educational Explanations ----
        ,
        .addExplanations = function() {
            # Helper function to set explanation content
            private$.setExplanationContent <- function(result_name, content) {
                tryCatch({
                    self$results[[result_name]]$setContent(content)
                }, error = function(e) {
                    # Silently ignore if result doesn't exist
                })
            }
            
            # Odds Ratio Analysis Explanation
            private$.setExplanationContent("oddsRatioExplanation", '
            <div style="margin-bottom: 20px; padding: 15px; background-color: #e8f4f8; border-left: 4px solid #17a2b8;">
                <h4 style="margin-top: 0; color: #2c3e50;">Understanding Odds Ratio Analysis</h4>
                <p><strong>Odds Ratio (OR):</strong> Measures the strength of association between risk factors and binary outcomes.</p>
                <ul>
                    <li><strong>Interpretation:</strong> OR > 1 indicates increased odds, OR < 1 indicates decreased odds</li>
                    <li><strong>Magnitude:</strong> Distance from 1.0 indicates strength of association</li>
                    <li><strong>Confidence Intervals:</strong> Provide precision estimates and statistical significance</li>
                    <li><strong>Case-Control Studies:</strong> Primary measure for retrospective study designs</li>
                </ul>
                <p><em>Clinical interpretation:</em> An OR of 2.0 means the odds of the outcome are twice as high in the exposed group.</p>
            </div>
            ')
            
            # Risk Measures Explanation
            private$.setExplanationContent("riskMeasuresExplanation", '
            <div style="margin-bottom: 20px; padding: 15px; background-color: #d4edda; border-left: 4px solid #28a745;">
                <h4 style="margin-top: 0; color: #2c3e50;">Understanding Risk Measures</h4>
                <p><strong>Risk Measures:</strong> Different ways to quantify the relationship between risk factors and outcomes.</p>
                <ul>
                    <li><strong>Risk Ratio (RR):</strong> Ratio of risks between exposed and unexposed groups</li>
                    <li><strong>Risk Difference (RD):</strong> Absolute difference in risk between groups</li>
                    <li><strong>Number Needed to Treat (NNT):</strong> Inverse of risk difference (1/RD)</li>
                    <li><strong>Attributable Risk:</strong> Proportion of disease attributable to the exposure</li>
                </ul>
                <p><em>Clinical utility:</em> Each measure provides different insights for clinical decision-making and public health planning.</p>
            </div>
            ')
            
            # Diagnostic Test Performance Explanation
            private$.setExplanationContent("diagnosticTestExplanation", '
            <div style="margin-bottom: 20px; padding: 15px; background-color: #fff3cd; border-left: 4px solid #ffc107;">
                <h4 style="margin-top: 0; color: #2c3e50;">Understanding Diagnostic Test Performance</h4>
                <p><strong>Diagnostic Metrics:</strong> Evaluate how well a test distinguishes between disease and non-disease states.</p>
                <ul>
                    <li><strong>Sensitivity:</strong> Proportion of true positives correctly identified (true positive rate)</li>
                    <li><strong>Specificity:</strong> Proportion of true negatives correctly identified (true negative rate)</li>
                    <li><strong>Positive Predictive Value (PPV):</strong> Probability of disease given a positive test</li>
                    <li><strong>Negative Predictive Value (NPV):</strong> Probability of no disease given a negative test</li>
                    <li><strong>Likelihood Ratios:</strong> How much a test result changes the probability of disease</li>
                </ul>
                <p><em>Clinical application:</em> These metrics help determine the clinical utility of diagnostic tests and biomarkers.</p>
            </div>
            ')
            
            # Nomogram Analysis Explanation
            private$.setExplanationContent("nomogramAnalysisExplanation", '
            <div style="margin-bottom: 20px; padding: 15px; background-color: #f8d7da; border-left: 4px solid #dc3545;">
                <h4 style="margin-top: 0; color: #721c24;">Understanding Nomogram Analysis</h4>
                <p><strong>Diagnostic Nomogram:</strong> Visual tool for converting likelihood ratios to post-test probabilities.</p>
                <ul>
                    <li><strong>Pre-test Probability:</strong> Prior probability of disease before testing</li>
                    <li><strong>Likelihood Ratio:</strong> How much the test result changes the odds</li>
                    <li><strong>Post-test Probability:</strong> Updated probability after incorporating test results</li>
                    <li><strong>Clinical Decision Making:</strong> Helps determine if further testing or treatment is warranted</li>
                </ul>
                <p><em>Usage:</em> Draw a line from pre-test probability through likelihood ratio to find post-test probability.</p>
            </div>
            ')
        }




        )
)
