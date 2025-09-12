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

        # Memory cleanup ----
        .finalize = function() {
            private$.nom_object <- NULL
            super$.finalize()
        },

        # init ----
        .init = function() {
            # Initialize main outputs to FALSE first
            self$results$text$setVisible(FALSE)
            self$results$text2$setVisible(FALSE)
            self$results$plot$setVisible(FALSE)
            
            # Initialize summary outputs and headings
            self$results$oddsRatioSummaryHeading$setVisible(FALSE)
            self$results$oddsRatioSummary$setVisible(FALSE)
            self$results$nomogramSummaryHeading$setVisible(FALSE)
            self$results$nomogramSummary$setVisible(FALSE)
            
            # Initialize explanation outputs and headings
            self$results$oddsRatioExplanationHeading$setVisible(FALSE)
            self$results$oddsRatioExplanation$setVisible(FALSE)
            self$results$riskMeasuresExplanation$setVisible(FALSE)
            self$results$diagnosticTestExplanation$setVisible(FALSE)
            self$results$nomogramExplanationHeading$setVisible(FALSE)
            self$results$nomogramAnalysisExplanation$setVisible(FALSE)

            # Handle showSummaries visibility
            if (self$options$showSummaries) {
                self$results$oddsRatioSummaryHeading$setVisible(TRUE)
                self$results$oddsRatioSummary$setVisible(TRUE)
                
                # Nomogram summary requires both showSummaries AND showNomogram
                if (self$options$showNomogram) {
                    self$results$nomogramSummaryHeading$setVisible(TRUE)
                    self$results$nomogramSummary$setVisible(TRUE)
                }
            }

            # Handle showExplanations visibility
            if (self$options$showExplanations) {
                # Odds ratio explanation section
                self$results$oddsRatioExplanationHeading$setVisible(TRUE)
                self$results$oddsRatioExplanation$setVisible(TRUE)
                self$results$riskMeasuresExplanation$setVisible(TRUE)
                self$results$diagnosticTestExplanation$setVisible(TRUE)
                
                # Nomogram explanation requires both showExplanations AND showNomogram
                if (self$options$showNomogram) {
                    self$results$nomogramExplanationHeading$setVisible(TRUE)
                    self$results$nomogramAnalysisExplanation$setVisible(TRUE)
                }
            }

            # Note: Main analysis outputs (text, text2, plot) will be set visible in .run() after validation
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
                        .("Outcome variable contains no non-missing values."))
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
                            .("Outcome variable must have at least 2 different values for logistic regression."))
                        validation_results$should_stop <- TRUE
                    } else if (length(outcome_levels) > 2) {
                        validation_results$errors <- c(validation_results$errors,
                            paste(.("Outcome variable has"), length(outcome_levels), .("levels. For odds ratio analysis, the outcome must be binary (exactly 2 levels). Consider creating a binary variable or using multinomial regression.")))
                        validation_results$should_stop <- TRUE
                    } else {
                        # Binary outcome - check for severe imbalance
                        min_count <- min(outcome_counts)
                        total_count <- sum(outcome_counts)
                        min_proportion <- min_count / total_count
                        
                        if (min_count < 5) {
                            validation_results$warnings <- c(validation_results$warnings,
                                .(paste("Outcome variable has very few observations in one category ({min_count} out of {total_count}). Results may be unreliable.")))
                        } else if (min_proportion < 0.05) {
                            validation_results$warnings <- c(validation_results$warnings,
                                .(paste("Outcome variable is severely imbalanced ({proportion}% in minority class). Consider using specialized methods for imbalanced data.")))
                        }
                        
                        # Require and validate user-specified outcome level
                        if (is.null(user_outcome_level)) {
                            validation_results$errors <- c(validation_results$errors,
                                .("Please select the positive outcome level from the dropdown menu below the outcome variable."))
                            validation_results$should_stop <- TRUE
                        } else if (!user_outcome_level %in% outcome_levels) {
                            validation_results$errors <- c(validation_results$errors,
                                paste("Specified positive outcome level '", user_outcome_level, "' not found in outcome variable. Available levels: ", paste(outcome_levels, collapse=", "), sep=""))
                            validation_results$should_stop <- TRUE
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
                                paste(.("Explanatory variable '{var_name}' contains no non-missing values.")))
                        } else if (length(unique(var_data_clean)) == 1) {
                            validation_results$warnings <- c(validation_results$warnings,
                                paste(.("Explanatory variable '{var_name}' has no variation (all values are the same). It will not contribute to the model.")))
                        } else if (is.factor(var_data)) {
                            # Factor variable validation
                            factor_levels <- levels(var_data)
                            factor_counts <- table(var_data)
                            
                            if (length(factor_levels) > 10) {
                                validation_results$warnings <- c(validation_results$warnings,
                                    paste(.("Explanatory variable '{var_name}' has {levels} levels. Consider grouping categories or using as continuous if ordinal.")))
                            }
                            
                            # Check for sparse categories
                            sparse_categories <- sum(factor_counts < 5)
                            if (sparse_categories > 0) {
                                validation_results$warnings <- c(validation_results$warnings,
                                    paste(.("Explanatory variable '{var_name}' has {sparse_categories} categories with fewer than 5 observations. Consider combining categories.")))
                            }
                        } else if (is.numeric(var_data)) {
                            # Numeric variable validation
                            if (any(is.infinite(var_data_clean))) {
                                validation_results$warnings <- c(validation_results$warnings,
                                    paste(.("Explanatory variable '{var_name}' contains infinite values.")))
                            }
                            
                            # Check for extreme values
                            q99 <- quantile(var_data_clean, 0.99, na.rm = TRUE)
                            q01 <- quantile(var_data_clean, 0.01, na.rm = TRUE)
                            extreme_high <- sum(var_data_clean > q99 + 3 * (q99 - q01), na.rm = TRUE)
                            extreme_low <- sum(var_data_clean < q01 - 3 * (q99 - q01), na.rm = TRUE)
                            
                            if (extreme_high + extreme_low > 0) {
                                validation_results$info <- c(validation_results$info,
                                    paste(.("Explanatory variable '{var_name}' may contain extreme outliers ({outliers} potential outliers).")))
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
                        Variable names with spaces or special characters are automatically cleaned for analysis while preserving original names in output.
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

            } else if (is.null(self$options$outcomeLevel)) {
                
                # Require outcome level selection
                todo <- glue::glue("
                    <br><b>Positive Outcome Level Required</b>
                    <br><br>
                    Please select which level of your outcome variable represents the 'positive' case
                    (e.g., 'Dead', 'Event', 'Yes', 'Positive').
                    <br><br>
                    This is required for correct calculation of:
                    <br>• Odds ratios interpretation
                    <br>• Likelihood ratios
                    <br>• Sensitivity and specificity
                    <br>• Diagnostic test performance metrics
                    <br><br>
                    Use the dropdown menu below the outcome variable to make your selection.
                ")
                
                html <- self$results$todo
                html$setContent(todo)
                self$results$text$setVisible(FALSE)
                self$results$text2$setVisible(FALSE)
                self$results$plot$setVisible(FALSE)
                return()
                
            } else {

                # Empty message when all variables selected and set main outputs visible
                todo <- ""
                
                # Set main analysis outputs visible after validation passes
                self$results$text$setVisible(TRUE)
                self$results$text2$setVisible(TRUE)
                self$results$plot$setVisible(TRUE)

                # glue::glue("Analysis based on:
                # <br>
                # glm(depdendent ~ explanatory, family='binomial')
                # <br>
                #     ")

                html <- self$results$todo
                html$setContent(todo)


                if (nrow(self$data) == 0) {
                    error_msg <- private$.formatErrorMessage(
                        "data_error",
                        "No data available for analysis",
                        "Dataset has no rows or all observations have been filtered out.",
                        c(
                            "Check your data import process",
                            "Verify variable selections",
                            "Review data quality and missing value patterns",
                            "Ensure your dataset contains complete observations"
                        )
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
                    error_msg <- private$.formatErrorMessage(
                        "validation_error",
                        "Critical validation errors detected",
                        paste(validation_results$errors, collapse = "; "),
                        c(
                            "Check your data format and variable types",
                            "Ensure outcome variable has exactly 2 levels", 
                            "Verify that explanatory variables have sufficient variation",
                            "Consider removing rows with missing data"
                        )
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
                
                # Monitor memory usage for large datasets
                memory_status <- private$.checkMemoryUsage(operation = "odds ratio analysis")

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
                
                # Restore original variable names in the finalfit output table
                if (!is.null(tOdds[[1]]) && nrow(tOdds[[1]]) > 0) {
                    tOdds[[1]] <- private$.restoreOriginalNamesInTable(tOdds[[1]], all_labels)
                }







                # Main analysis execution starts here


















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
                
                # Generate natural language summary if requested
                if (self$options$showSummaries) {
                    oddsRatioSummary <- private$.generateOddsRatioSummary(tOdds, formulaDependent, formulaExplanatory)
                    self$results$oddsRatioSummary$setContent(oddsRatioSummary)
                }




                ## plot Data ----
                plotData <- list(
                    "plotData" = mydata,
                    "formulaDependent" = formulaDependent,
                    "formulaExplanatory" = formulaExplanatory,
                    "originalNames" = all_labels,
                    "originalOutcomeName" = self$options$outcome,
                    "originalExplanatoryNames" = self$options$explanatory
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
                    
                    # Check if likelihood ratio calculation failed
                    if (!is.null(lr_results$error) && lr_results$error) {
                        stop(lr_results$message)
                    }

                    # Create diagnostic metrics text with explanatory information
                    # Include statistical warnings and recommendations if present
                    statistical_warnings <- if (!is.null(lr_results$statistical_warnings) && lr_results$statistical_warnings != "") lr_results$statistical_warnings else ""
                    statistical_recommendations <- if (!is.null(lr_results$statistical_recommendations) && lr_results$statistical_recommendations != "") lr_results$statistical_recommendations else ""
                    
                    metrics_text <- glue::glue("
                    <br>
                    <div style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0;'>
                        <b>Diagnostic Metrics:</b><br>
                        Sensitivity: {format(lr_results$sensitivity * 100, digits=2)}%<br>
                        Specificity: {format(lr_results$specificity * 100, digits=2)}%<br>
                        Positive LR: {format(lr_results$positive_lr, digits=2)}<br>
                        Negative LR: {format(lr_results$negative_lr, digits=2)}<br>
                    </div>
                    
                    {statistical_warnings}
                    {statistical_recommendations}
                    
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
                    .("⚠️ Likelihood Ratio Calculation Error:"),
                    .("This calculation requires both predictor and outcome variables to be binary (exactly 2 levels each)."),
                    .("Current situation:"),
                    paste(.("- Predictor variable '{predictor_var}' has {levels} levels")),
                    paste(.("- Outcome variable '{outcome_var}' has {levels} levels")),
                    .("📋 Solutions:"),
                    .("• For continuous variables: Create binary categories (e.g., above/below median)"),
                    .("• For categorical variables: Combine categories to create binary grouping"),
                    .("• Use a different analysis method for multi-level variables"),
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
            
            # Require user to specify positive outcome level
            if (is.null(user_positive_outcome) || !user_positive_outcome %in% outcome_levels) {
                return(list(
                    error = TRUE,
                    message = paste("Please select the positive outcome level. Available levels:", 
                                   paste(outcome_levels, collapse=", "))
                ))
            }
            
            # Use user-specified positive outcome level
            positive_outcome_level <- user_positive_outcome
            positive_outcome_idx <- which(outcome_levels == positive_outcome_level)
            outcome_determination_method <- "User-specified"
            
            # Determine positive predictor level using configurable detection
            detection_result <- private$.detectPositiveLevels(predictor_levels)
            positive_predictor_level <- detection_result$level
            predictor_determination_method <- detection_result$method
            positive_predictor_idx <- which(predictor_levels == positive_predictor_level)
            
            # Calculate 2x2 table components
            tp <- cont_table[positive_predictor_idx, positive_outcome_idx]
            fp <- cont_table[positive_predictor_idx, -positive_outcome_idx]
            fn <- cont_table[-positive_predictor_idx, positive_outcome_idx]
            tn <- cont_table[-positive_predictor_idx, -positive_outcome_idx]
            
            # Check statistical assumptions and provide recommendations
            assumption_check <- private$.checkStatisticalAssumptions(cont_table)
            
            # Add assumption warnings to diagnostic info if present
            diagnostic_warnings <- ""
            if (length(assumption_check$warnings) > 0) {
                diagnostic_warnings <- paste0(
                    "<div style='background-color: #fff3cd; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                    "<b>Statistical Assumptions Check:</b><br>",
                    paste(assumption_check$warnings, collapse = "<br>"),
                    "</div>"
                )
            }
            
            # Add recommendations if any
            recommendation_text <- ""
            if (length(assumption_check$recommendations) > 0) {
                recommendations_list <- lapply(assumption_check$recommendations, function(rec) {
                    if (is.list(rec)) {
                        paste0("• <b>", rec$test, ":</b> ", rec$reason, " (Use: ", rec$code, ")")
                    } else {
                        paste0("• ", rec)
                    }
                })
                
                recommendation_text <- paste0(
                    "<div style='background-color: #d4edda; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                    "<b>💡 Statistical Recommendations:</b><br>",
                    paste(recommendations_list, collapse = "<br>"),
                    "</div>"
                )
            }
            
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
                
                # Generate natural language summary if requested
                if (self$options$showSummaries) {
                    nomogramSummary <- private$.generateNomogramSummary(nom, fit, dd)
                    self$results$nomogramSummary$setContent(nomogramSummary)
                }
            }
        },

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
                    originalNames <- plotList$originalNames
                    
                    # Create a temporary dataset with restored variable names for plotting
                    plotDataWithOriginalNames <- private$.createPlotDataWithOriginalNames(mydata, originalNames)

                    private$.checkpoint()

                    plot <-
                        # finalfit::or_plot(
                        finalfit::ff_plot(
                            .data = plotDataWithOriginalNames$data,
                            dependent = plotDataWithOriginalNames$formulaDependent,
                            explanatory = plotDataWithOriginalNames$formulaExplanatory,
                            remove_ref = FALSE,
                            table_text_size = 4,
                            title_text_size = 14,
                            random_effect = NULL,
                            factorlist = NULL,
                            glmfit = NULL,
                            confint_type = NULL,
                            breaks = NULL,
                            column_space = c(-0.5, 0, 0.5),
                            dependent_label = plotList$originalOutcomeName,
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




        # Natural Language Summary Generation ----
        ,
        # Helper function to extract significant predictors from odds ratio table
        .extractSignificantPredictors = function(or_table) {
            if (is.null(or_table) || length(or_table) == 0 || is.null(or_table[[1]])) {
                return(list(significant_count = 0, total_predictors = 0, strongest = NULL))
            }
            
            significant_predictors <- list()
            strongest_or <- 1
            strongest_predictor <- ""
            
            for (i in 2:nrow(or_table)) {  # Skip header row
                if (!is.na(or_table[i, "p"]) && or_table[i, "p"] != "") {
                    p_value <- as.numeric(or_table[i, "p"])
                    if (!is.na(p_value) && p_value < 0.05) {
                        or_value <- or_table[i, "OR"]
                        if (!is.na(or_value) && or_value != "" && or_value != "-") {
                            or_numeric <- as.numeric(or_value)
                            if (!is.na(or_numeric)) {
                                # Track strongest association (furthest from 1)
                                if (abs(log(or_numeric)) > abs(log(strongest_or))) {
                                    strongest_or <- or_numeric
                                    strongest_predictor <- or_table[i, 1]  # First column is predictor name
                                }
                                significant_predictors[[length(significant_predictors) + 1]] <- list(
                                    name = or_table[i, 1],
                                    or = or_numeric,
                                    p_value = p_value
                                )
                            }
                        }
                    }
                }
            }
            
            return(list(
                significant_count = length(significant_predictors),
                total_predictors = nrow(or_table) - 1,  # Exclude header
                significant_predictors = significant_predictors,
                strongest_or = strongest_or,
                strongest_predictor = strongest_predictor
            ))
        }

        # Standardized error message formatter
        ,
        .formatErrorMessage = function(error_type, message, details = NULL, suggestions = NULL) {
            icons <- list(
                "data_error" = "❌",
                "validation_error" = "⚠️", 
                "calculation_error" = "🔢",
                "memory_error" = "💾",
                "statistical_error" = "📊"
            )
            
            icon <- if (error_type %in% names(icons)) {
                icons[[error_type]]
            } else {
                "⚠️"
            }
            
            # Base error HTML with consistent styling
            error_html <- paste0(
                '<div style="background-color: #fff5f5; border-left: 4px solid #e53e3e; padding: 12px; margin: 10px 0; border-radius: 6px;">',
                '<p style="margin: 0; color: #721c24; font-weight: bold;">', 
                icon, ' ', stringr::str_to_title(gsub("_", " ", error_type)), '</p>',
                '<p style="margin: 8px 0 0 0; color: #2d3748;">', message, '</p>'
            )
            
            # Add details if provided
            if (!is.null(details)) {
                error_html <- paste0(error_html,
                    '<p style="margin: 8px 0 0 0; color: #4a5568; font-size: 0.9em;"><em>', details, '</em></p>'
                )
            }
            
            # Add suggestions if provided
            if (!is.null(suggestions) && length(suggestions) > 0) {
                error_html <- paste0(error_html,
                    '<div style="margin-top: 10px; padding: 8px; background-color: #f0fff4; border-radius: 4px;">',
                    '<p style="margin: 0; color: #276749; font-weight: bold;">💡 Suggestions:</p>',
                    '<ul style="margin: 4px 0 0 0; color: #2f855a; font-size: 0.9em;">'
                )
                
                for (suggestion in suggestions) {
                    error_html <- paste0(error_html, '<li>', suggestion, '</li>')
                }
                
                error_html <- paste0(error_html, '</ul></div>')
            }
            
            error_html <- paste0(error_html, '</div>')
            return(error_html)
        }

        # Helper function to format odds ratio interpretation text
        ,
        .formatOddsRatioText = function(or_value, predictor_name) {
            if (or_value > 1) {
                return(paste0('increased odds (OR = ', round(or_value, 2), ')'))
            } else if (or_value < 1) {
                return(paste0('decreased odds (OR = ', round(or_value, 2), ')'))
            } else {
                return('no significant association')
            }
        }

        # Clinical preset configurations for common scenarios
        ,
        .applyClinicalPreset = function(preset_name, data_info) {
            presets <- list(
                "diagnostic_test" = list(
                    focus = "sensitivity_specificity",
                    interpretation = "diagnostic",
                    recommendations = c("Consider ROC analysis", "Evaluate predictive values", "Check diagnostic accuracy"),
                    terminology = list(positive = "Disease Present", negative = "Disease Absent")
                ),
                "risk_factor" = list(
                    focus = "odds_ratios",
                    interpretation = "epidemiological", 
                    recommendations = c("Examine dose-response", "Consider confounders", "Evaluate effect modification"),
                    terminology = list(positive = "Exposed", negative = "Not Exposed")
                ),
                "treatment_response" = list(
                    focus = "efficacy",
                    interpretation = "therapeutic",
                    recommendations = c("Calculate NNT", "Assess clinical significance", "Consider adverse effects"),
                    terminology = list(positive = "Response", negative = "No Response")
                ),
                "biomarker" = list(
                    focus = "prediction",
                    interpretation = "prognostic",
                    recommendations = c("Validate biomarker", "Assess clinical utility", "Consider cost-effectiveness"),
                    terminology = list(positive = "High Expression", negative = "Low Expression")
                ),
                "screening" = list(
                    focus = "detection",
                    interpretation = "population_health",
                    recommendations = c("Evaluate screening performance", "Consider false positive rate", "Assess population benefit"),
                    terminology = list(positive = "Screen Positive", negative = "Screen Negative")
                )
            )
            
            if (preset_name %in% names(presets)) {
                return(presets[[preset_name]])
            }
            return(NULL)
        }

        # Helper function to detect likely clinical scenario based on variable names
        ,
        .detectClinicalScenario = function(outcome_var, explanatory_vars) {
            outcome_lower <- tolower(outcome_var)
            explanatory_lower <- tolower(paste(explanatory_vars, collapse = " "))
            
            # Diagnostic test scenario
            if (any(grepl("(diagnos|test|positive|negative|disease|pathology)", outcome_lower)) ||
                any(grepl("(test|assay|marker|score)", explanatory_lower))) {
                return("diagnostic_test")
            }
            
            # Treatment response scenario
            if (any(grepl("(response|treatment|therapy|outcome|recovery|improvement)", outcome_lower)) ||
                any(grepl("(drug|treatment|therapy|dose|intervention)", explanatory_lower))) {
                return("treatment_response") 
            }
            
            # Biomarker scenario
            if (any(grepl("(biomarker|marker|expression|level|concentration)", explanatory_lower)) ||
                any(grepl("(gene|protein|metabolite|cytokine)", explanatory_lower))) {
                return("biomarker")
            }
            
            # Screening scenario
            if (any(grepl("(screen|detect|early|prevention)", outcome_lower))) {
                return("screening")
            }
            
            # Risk factor scenario (default for epidemiological studies)
            if (any(grepl("(risk|factor|exposure|smoking|diet|lifestyle)", explanatory_lower))) {
                return("risk_factor")
            }
            
            # Default to risk factor if unclear
            return("risk_factor")
        }

        # Helper function to build summary HTML content with clinical context
        ,
        .buildSummaryHTML = function(findings, metrics = NULL, outcome_var = NULL, explanatory_vars = NULL) {
            # Detect clinical scenario and apply appropriate preset
            clinical_scenario <- if (!is.null(outcome_var) && !is.null(explanatory_vars)) {
                private$.detectClinicalScenario(outcome_var, explanatory_vars)
            } else {
                "risk_factor" # Default
            }
            
            preset <- private$.applyClinicalPreset(clinical_scenario, NULL)
            
            # Context-aware summary title and description
            context_descriptions <- list(
                "diagnostic_test" = "diagnostic test evaluation",
                "treatment_response" = "treatment efficacy assessment", 
                "biomarker" = "biomarker analysis",
                "screening" = "screening performance evaluation",
                "risk_factor" = "risk factor analysis"
            )
            
            analysis_context <- if (clinical_scenario %in% names(context_descriptions)) {
                context_descriptions[[clinical_scenario]]
            } else {
                "statistical analysis"
            }
            
            summary_html <- paste0(
                '<div style="background-color: #f0f8ff; padding: 15px; border-radius: 8px; margin: 10px 0;">',
                '<h4 style="color: #2c5282; margin-top: 0;">📊 ', stringr::str_to_title(analysis_context), ' Summary</h4>',
                '<p style="margin: 10px 0;"><strong>Analysis Overview:</strong> Logistic regression was performed for ',
                analysis_context, ' examining ', findings$total_predictors, ' explanatory variable(s) and their relationship to the outcome.</p>'
            )
            
            # Add key findings section
            if (findings$significant_count > 0) {
                interpretation <- private$.formatOddsRatioText(findings$strongest_or, findings$strongest_predictor)
                
                summary_html <- paste0(summary_html,
                    '<p style="margin: 10px 0;"><strong>Key Findings:</strong></p>',
                    '<ul style="margin: 5px 0; padding-left: 20px;">',
                    '<li>', findings$significant_count, ' out of ', findings$total_predictors, ' predictor(s) showed statistically significant associations (p < 0.05)</li>'
                )
                
                if (findings$strongest_predictor != "") {
                    summary_html <- paste0(summary_html,
                        '<li>Strongest association: <em>', findings$strongest_predictor, '</em> with ', interpretation, '</li>'
                    )
                }
                
                summary_html <- paste0(summary_html, '</ul>')
            } else {
                summary_html <- paste0(summary_html,
                    '<p style="margin: 10px 0;"><strong>Key Findings:</strong> None of the ', findings$total_predictors, 
                    ' predictor(s) showed statistically significant associations with the outcome (all p ≥ 0.05).</p>'
                )
            }
            
            # Add model performance if available
            if (!is.null(metrics) && metrics != "") {
                summary_html <- paste0(summary_html,
                    '<p style="margin: 10px 0;"><strong>Model Performance:</strong> ', metrics, '</p>'
                )
            }
            
            # Add clinical recommendations based on detected scenario
            if (!is.null(preset) && !is.null(preset$recommendations)) {
                summary_html <- paste0(summary_html,
                    '<div style="background-color: #fff3e0; padding: 10px; border-radius: 5px; margin: 10px 0;">',
                    '<strong>🩺 Clinical Recommendations:</strong>',
                    '<ul style="margin: 5px 0; padding-left: 20px; font-size: 0.95em;">'
                )
                
                for (rec in preset$recommendations) {
                    summary_html <- paste0(summary_html, '<li>', rec, '</li>')
                }
                
                summary_html <- paste0(summary_html, '</ul></div>')
            }
            
            # Add interpretation guide
            summary_html <- paste0(summary_html,
                '<div style="background-color: #e6f7ff; padding: 10px; border-radius: 5px; margin-top: 10px;">',
                '<strong>💡 Interpretation Guide:</strong>',
                '<ul style="margin: 5px 0; padding-left: 20px; font-size: 0.95em;">',
                '<li>OR > 1: Factor increases the odds of the outcome</li>',
                '<li>OR < 1: Factor decreases the odds of the outcome</li>',
                '<li>OR = 1: No association between factor and outcome</li>',
                '<li>95% CI not crossing 1.0 indicates statistical significance</li>',
                '</ul>',
                '</div>',
                '</div>'
            )
            
            return(summary_html)
        }

        # Main function now simplified and focused
        ,
        .generateOddsRatioSummary = function(tOdds, formulaDependent, formulaExplanatory) {
            tryCatch({
                # Use helper functions for cleaner, maintainable code
                findings <- private$.extractSignificantPredictors(tOdds[[1]])
                metrics <- if (!is.null(tOdds[[2]])) tOdds[[2]] else NULL
                
                # Extract variable names for clinical scenario detection
                explanatory_vars <- if (!is.null(formulaExplanatory)) {
                    strsplit(formulaExplanatory, " \\+ ")[[1]]
                } else {
                    character(0)
                }
                
                return(private$.buildSummaryHTML(findings, metrics, formulaDependent, explanatory_vars))
                
            }, error = function(e) {
                return(private$.formatErrorMessage(
                    "calculation_error",
                    "Unable to generate analysis summary",
                    "There was an error processing the odds ratio results",
                    c("Check that your data meets analysis requirements", "Verify variable types and completeness")
                ))
            })
        }
        ,
        .generateNomogramSummary = function(nom, fit, mydata) {
            tryCatch({
                # Extract model information
                n_subjects <- nrow(mydata)
                n_predictors <- length(fit$coefficients) - 1  # Exclude intercept
                
                # Generate summary
                summary_html <- paste0(
                    '<div style="background-color: #fff3cd; padding: 15px; border-radius: 8px; margin: 10px 0;">',
                    '<h4 style="color: #856404; margin-top: 0;">📈 Nomogram Analysis Summary</h4>',
                    '<p style="margin: 10px 0;">A diagnostic nomogram has been generated based on the logistic regression model with ',
                    n_predictors, ' predictor(s) and ', n_subjects, ' subjects.</p>',
                    
                    '<p style="margin: 10px 0;"><strong>How to Use the Nomogram:</strong></p>',
                    '<ol style="margin: 5px 0; padding-left: 25px;">',
                    '<li>Find your patient\'s value for each predictor variable on its scale</li>',
                    '<li>Draw a vertical line up to the "Points" axis to get the points for that predictor</li>',
                    '<li>Sum all points from all predictors to get the "Total Points"</li>',
                    '<li>Draw a vertical line down from the Total Points to find the predicted probability</li>',
                    '</ol>',
                    
                    '<div style="background-color: #ffeaa7; padding: 10px; border-radius: 5px; margin-top: 10px;">',
                    '<strong>⚠️ Clinical Note:</strong> This nomogram provides probability estimates based on the current dataset. ',
                    'External validation is recommended before clinical implementation. Consider local prevalence rates and ',
                    'clinical context when interpreting results.',
                    '</div>',
                    '</div>'
                )
                
                return(summary_html)
                
            }, error = function(e) {
                return(private$.formatErrorMessage(
                    "calculation_error", 
                    "Unable to generate nomogram summary",
                    "There was an error processing the nomogram results",
                    c("Ensure sufficient data for nomogram creation", "Verify model convergence", "Check variable completeness")
                ))
            })
        }
        ,
        .createNomogramDisplay = function(nom) {
            # Create HTML display for the nomogram information
            html_content <- '<div style="background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0;">'
            html_content <- paste0(html_content, '<h4 style="color: #495057; margin-top: 0;">Nomogram Information</h4>')
            html_content <- paste0(html_content, '<p>The nomogram plot above provides a visual tool for risk prediction based on the logistic regression model.</p>')
            html_content <- paste0(html_content, '<p><strong>Components:</strong></p>')
            html_content <- paste0(html_content, '<ul style="margin: 5px 0; padding-left: 20px;">')
            html_content <- paste0(html_content, '<li><strong>Points:</strong> Top scale showing point values for each predictor</li>')
            html_content <- paste0(html_content, '<li><strong>Predictor Scales:</strong> Individual scales for each variable in the model</li>')
            html_content <- paste0(html_content, '<li><strong>Total Points:</strong> Sum of all individual predictor points</li>')
            html_content <- paste0(html_content, '<li><strong>Predicted Probability:</strong> Bottom scale showing the predicted outcome probability</li>')
            html_content <- paste0(html_content, '</ul>')
            html_content <- paste0(html_content, '</div>')
            
            return(html_content)
        }
        
        # Educational Explanations ----
        ,
        .addExplanations = function() {
            # Odds Ratio Analysis Explanation
            tryCatch({
                self$results$oddsRatioExplanation$setContent('
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
            
            }, error = function(e) {
                # Silently ignore if result doesn't exist
            })
            
            # Risk Measures Explanation
            tryCatch({
                self$results$riskMeasuresExplanation$setContent('
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
            
            }, error = function(e) {
                # Silently ignore if result doesn't exist
            })
            
            # Diagnostic Test Performance Explanation
            tryCatch({
                self$results$diagnosticTestExplanation$setContent('
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
            
            }, error = function(e) {
                # Silently ignore if result doesn't exist
            })
            
            # Nomogram Analysis Explanation
            tryCatch({
                self$results$nomogramAnalysisExplanation$setContent('
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
            
            }, error = function(e) {
                # Silently ignore if result doesn't exist
            })
        }

        # Helper function to restore original variable names in finalfit output table
        ,
        .restoreOriginalNamesInTable = function(table_data, all_labels) {
            if (is.null(table_data) || nrow(table_data) == 0) return(table_data)
            
            # Create a mapping from cleaned names to original names
            name_mapping <- setNames(unlist(all_labels), names(all_labels))
            
            # Restore names in the first column (which typically contains variable names)
            if (ncol(table_data) > 0) {
                first_col <- table_data[[1]]
                
                # Process each row in the first column
                for (i in seq_along(first_col)) {
                    current_name <- first_col[i]
                    
                    # Skip if it's not a string or is empty
                    if (is.na(current_name) || current_name == "" || !is.character(current_name)) next
                    
                    # Handle different finalfit naming patterns:
                    # 1. Direct variable name match
                    if (current_name %in% names(name_mapping)) {
                        first_col[i] <- name_mapping[current_name]
                    }
                    # 2. Variable name with factor level (e.g., "variable_nameLevel1")
                    else {
                        # Try to find a matching cleaned name that's a prefix
                        for (clean_name in names(name_mapping)) {
                            if (startsWith(current_name, clean_name)) {
                                # Replace the cleaned prefix with original name
                                suffix <- substring(current_name, nchar(clean_name) + 1)
                                first_col[i] <- paste0(name_mapping[clean_name], suffix)
                                break
                            }
                        }
                    }
                }
                
                table_data[[1]] <- first_col
            }
            
            return(table_data)
        }

        # Helper function to check statistical assumptions and recommend alternatives
        ,
        .checkStatisticalAssumptions = function(cont_table) {
            assumptions_ok <- TRUE
            recommendations <- list()
            warnings <- list()
            
            # Check minimum expected cell counts for chi-square assumptions
            if (is.matrix(cont_table) && nrow(cont_table) == 2 && ncol(cont_table) == 2) {
                # Calculate expected counts under independence assumption
                row_totals <- rowSums(cont_table)
                col_totals <- colSums(cont_table)
                total_n <- sum(cont_table)
                
                expected_counts <- matrix(0, nrow = 2, ncol = 2)
                for (i in 1:2) {
                    for (j in 1:2) {
                        expected_counts[i, j] <- (row_totals[i] * col_totals[j]) / total_n
                    }
                }
                
                min_expected <- min(expected_counts)
                
                if (min_expected < 5) {
                    assumptions_ok <- FALSE
                    warnings <- append(warnings, paste0(
                        "⚠️ Small expected cell counts detected (minimum = ", round(min_expected, 2), "). ",
                        "Chi-square assumptions may be violated."
                    ))
                    
                    recommendations <- append(recommendations, list(
                        test = "Fisher's exact test",
                        reason = "More reliable for small cell counts",
                        code = "fisher.test()",
                        interpretation = "Provides exact p-values regardless of sample size"
                    ))
                }
                
                # Check for very small total sample size
                if (total_n < 20) {
                    warnings <- append(warnings, paste0(
                        "⚠️ Very small sample size (n = ", total_n, "). ",
                        "Results should be interpreted with extreme caution."
                    ))
                }
                
                # Check for zero cells
                if (any(cont_table == 0)) {
                    warnings <- append(warnings, 
                        "⚠️ Zero cells detected in contingency table. This may affect odds ratio calculation."
                    )
                }
            }
            
            return(list(
                assumptions_ok = assumptions_ok,
                warnings = warnings,
                recommendations = recommendations,
                expected_counts = if (exists("expected_counts")) expected_counts else NULL
            ))
        }

        # Helper function to monitor memory usage for large datasets
        ,
        .checkMemoryUsage = function(data_size_mb, operation = "analysis") {
            # Get current memory usage
            current_memory <- as.numeric(object.size(self$data)) / 1024^2  # Convert to MB
            
            # Define memory thresholds
            warning_threshold <- 100  # MB
            critical_threshold <- 500  # MB
            
            if (current_memory > critical_threshold) {
                warning(paste0(
                    "⚠️ Large Dataset Warning: Dataset size is ", round(current_memory, 1), " MB. ",
                    "This may cause performance issues or memory problems during ", operation, ". ",
                    "Consider:\n",
                    "• Working with a representative sample\n",
                    "• Reducing the number of variables\n", 
                    "• Checking available system memory"
                ))
                return("critical")
            } else if (current_memory > warning_threshold) {
                message(paste0(
                    "ℹ️ Dataset size: ", round(current_memory, 1), " MB. ",
                    "Analysis may take longer than usual."
                ))
                return("warning")
            }
            return("normal")
        }

        # Helper function for configurable positive level detection
        ,
        .detectPositiveLevels = function(levels, language = "auto") {
            # Configure positive indicators by language
            if (language == "auto") {
                # Detect language from levels or use default
                language <- if (any(grepl("[ıüğşçöİÜĞŞÇÖ]", levels))) "tr" else "en"
            }
            
            # Positive indicators by language
            indicators <- switch(language,
                "en" = c("Positive", "Yes", "Present", "Exposed", "High", "Abnormal", "1", "TRUE", "Bad", "Dead", "Event"),
                "tr" = c("Pozitif", "Evet", "Mevcut", "Maruz", "Yüksek", "Anormal", "1", "DOĞRU", "Kötü", "Ölü", "Olay", 
                        "Positive", "Yes", "Present", "Exposed", "High", "Abnormal", "TRUE", "Bad", "Dead", "Event"), # Fallback to English
                c("Positive", "Yes", "Present", "Exposed", "High", "Abnormal", "1", "TRUE", "Bad", "Dead", "Event") # Default
            )
            
            # Try to find positive level
            positive_matches <- levels[levels %in% indicators]
            
            if (length(positive_matches) == 1) {
                return(list(
                    level = positive_matches[1],
                    method = paste("Automatic detection (", language, ")", sep = "")
                ))
            } else if (length(positive_matches) > 1) {
                # Multiple matches - use first priority match
                return(list(
                    level = positive_matches[1],
                    method = paste("Automatic detection - first match (", language, ")", sep = "")
                ))
            } else {
                # No matches - use default (second level alphabetically)
                return(list(
                    level = levels[min(2, length(levels))],
                    method = "Default (second level alphabetically)"
                ))
            }
        }

        # Helper function to create plot data with original variable names
        ,
        .createPlotDataWithOriginalNames = function(mydata, all_labels) {
            if (is.null(all_labels) || length(all_labels) == 0) {
                # Fallback: return data as-is if no labels available
                return(list(
                    data = mydata,
                    formulaDependent = names(mydata)[1],  # Fallback
                    formulaExplanatory = names(mydata)[-1]  # Fallback
                ))
            }
            
            # Create a copy of the data with original column names
            plotData <- mydata
            name_mapping <- setNames(unlist(all_labels), names(all_labels))
            
            # Restore original column names
            original_names <- character(ncol(plotData))
            for (i in seq_along(names(plotData))) {
                clean_name <- names(plotData)[i]
                if (clean_name %in% names(name_mapping)) {
                    original_names[i] <- name_mapping[clean_name]
                } else {
                    original_names[i] <- clean_name  # Keep as-is if not found
                }
            }
            
            names(plotData) <- original_names
            
            # Create formulas with original names
            original_dependent <- original_names[1]  # Assuming first column is dependent
            original_explanatory <- original_names[-1]  # Rest are explanatory
            
            # Find the correct dependent and explanatory variables
            # Match with the labels to find the right variables
            for (clean_name in names(name_mapping)) {
                original_name <- name_mapping[clean_name]
                if (original_name %in% original_names) {
                    # This helps maintain the correct variable identification
                }
            }
            
            return(list(
                data = plotData,
                formulaDependent = original_dependent,
                formulaExplanatory = original_explanatory
            ))
        }




        )
)
