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
        .notices = list(),

        # Notice management helpers ----
        .addNotice = function(type, message, name = NULL) {
            if (is.null(name)) {
                name <- paste0('notice', length(private$.notices) + 1)
            }
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = name,
                type = type
            )
            notice$setContent(message)
            priority <- switch(
                as.character(type),
                "1" = 1,  # ERROR
                "2" = 2,  # STRONG_WARNING
                "3" = 3,  # WARNING
                "4" = 4,  # INFO
                3         # Default to WARNING
            )
            private$.notices[[length(private$.notices) + 1]] <- list(
                notice = notice,
                priority = priority
            )
        },

        .insertNotices = function() {
            if (length(private$.notices) == 0) return()
            notices_sorted <- private$.notices[order(sapply(private$.notices, function(x) x$priority))]
            position <- 1
            for (n in notices_sorted) {
                self$results$insert(position, n$notice)
                position <- position + 1
            }
        },

        .resetNotices = function() {
            private$.notices <- list()
        },

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

            # Initialize explanation outputs
            self$results$oddsRatioExplanation$setVisible(FALSE)
            self$results$riskMeasuresExplanation$setVisible(FALSE)
            self$results$diagnosticTestExplanation$setVisible(FALSE)
            self$results$nomogramAnalysisExplanation$setVisible(FALSE)

            # Handle showExplanations visibility
            if (self$options$showExplanations) {
                # Odds ratio explanation section
                self$results$oddsRatioExplanation$setVisible(TRUE)
                self$results$riskMeasuresExplanation$setVisible(TRUE)
                self$results$diagnosticTestExplanation$setVisible(TRUE)

                # Nomogram explanation requires both showExplanations AND showNomogram
                if (self$options$showNomogram) {
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
                strong_warnings = character(0),
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
                            validation_results$strong_warnings <- c(validation_results$strong_warnings,
                                glue::glue("Outcome variable has very few observations in one category ({min_count} out of {total_count}). Results may be unreliable."))
                        } else if (min_proportion < 0.05) {
                            validation_results$strong_warnings <- c(validation_results$strong_warnings,
                                glue::glue("Outcome variable is severely imbalanced ({sprintf('%.1f%%', min_proportion * 100)} in minority class). Consider using specialized methods for imbalanced data."))
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
                        } else {
                            validation_results$info <- c(validation_results$info,
                                glue::glue("Outcome level modeled as the event: '{user_outcome_level}'."))
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
                                glue::glue("Explanatory variable '{var_name}' contains no non-missing values."))
                        } else if (length(unique(var_data_clean)) == 1) {
                            validation_results$warnings <- c(validation_results$warnings,
                                glue::glue("Explanatory variable '{var_name}' has no variation (all values are the same). It will not contribute to the model."))
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
                        } else if (is.ordered(var_data)) {
                            validation_results$info <- c(validation_results$info,
                                glue::glue("Ordered factor '{var_name}' will be treated as nominal (unordered) for modeling and output."))
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

            # Reset notices at start of each run
            private$.resetNotices()







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

                # Insert accumulated notices before main analysis outputs
                private$.insertNotices()

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
                if (length(dependent_variable_name_from_label) > 1) {
                    # Ambiguous label; pick first but warn
                    validation_results$warnings <- c(validation_results$warnings,
                        glue::glue("Outcome label matches multiple variables after cleaning; using '{dependent_variable_name_from_label[1]}'. Please verify selection."))
                    dependent_variable_name_from_label <- dependent_variable_name_from_label[1]
                }

                # FIX: Relevel outcome variable to match user's selected positive outcome level
                # This ensures logistic regression models the correct event
                if (!is.null(self$options$outcomeLevel) && !is.null(dependent_variable_name_from_label)) {
                    outcome_var <- mydata[[dependent_variable_name_from_label]]

                    # Convert to factor if not already
                    if (!is.factor(outcome_var)) {
                        outcome_var <- as.factor(outcome_var)
                    }

                    # Get the user's selected positive level
                    positive_level <- self$options$outcomeLevel

                    # Verify the positive level exists in the data
                    if (positive_level %in% levels(outcome_var)) {
                        # Relevel so positive outcome is the second level (what glm models as "1")
                        # Get all levels except the positive one
                        other_levels <- setdiff(levels(outcome_var), positive_level)

                        # Create new level order: reference levels first, then positive level
                        new_levels <- c(other_levels, positive_level)

                        # Relevel the outcome
                        mydata[[dependent_variable_name_from_label]] <- factor(
                            outcome_var,
                            levels = new_levels
                        )

                        # Add info message to inform user
                        outcome_releveling_message <- paste0(
                            "Outcome variable releveled: '", positive_level,
                            "' is now modeled as the positive outcome (event)."
                        )
                    } else {
                        # Warn if selected level doesn't exist
                        warning_msg <- paste0(
                            "Warning: Selected positive outcome level '", positive_level,
                            "' not found in data. Available levels: ",
                            paste(levels(outcome_var), collapse = ", ")
                        )
                    }
                }

                # Add validation strong warnings, warnings, and info as notices
                if (length(validation_results$strong_warnings) > 0) {
                    for (warn_msg in validation_results$strong_warnings) {
                        private$.addNotice(jmvcore::NoticeType$STRONG_WARNING, warn_msg)
                    }
                }
                if (length(validation_results$warnings) > 0) {
                    for (warn_msg in validation_results$warnings) {
                        private$.addNotice(jmvcore::NoticeType$WARNING, warn_msg)
                    }
                }
                if (length(validation_results$info) > 0) {
                    for (info_msg in validation_results$info) {
                        private$.addNotice(jmvcore::NoticeType$INFO, info_msg)
                    }
                }

                # Retrieve the variable names vector from the label vector
                labels <- self$options$explanatory

                explanatory_variable_names <- names(all_labels)[match(labels, all_labels)]
                # Handle ambiguous mappings
                if (any(is.na(explanatory_variable_names))) {
                    missing_labels <- labels[is.na(explanatory_variable_names)]
                    validation_results$warnings <- c(validation_results$warnings,
                        glue::glue("Could not map some explanatory variables after cleaning: {paste(missing_labels, collapse=', ')}"))
                    explanatory_variable_names <- explanatory_variable_names[!is.na(explanatory_variable_names)]
                }

                # Convert ordered factors to unordered factors to avoid polynomial contrasts / mislabeling
                if (!is.null(explanatory_variable_names)) {
                    for (v in explanatory_variable_names) {
                        if (!is.null(v) && v %in% names(mydata) && is.ordered(mydata[[v]])) {
                            mydata[[v]] <- factor(mydata[[v]], ordered = FALSE)
                        }
                    }
                }

                # Additional diagnostics: EPV and separation checks
                extra_warnings <- c()
                if (!is.null(dependent_variable_name_from_label) && !is.null(self$options$outcomeLevel)) {
                    evt_count <- sum(mydata[[dependent_variable_name_from_label]] == self$options$outcomeLevel, na.rm = TRUE)
                    df_predictors <- 0
                    for (v in explanatory_variable_names) {
                        if (!is.null(v) && v %in% names(mydata)) {
                            if (is.factor(mydata[[v]])) {
                                df_predictors <- df_predictors + max(1, nlevels(mydata[[v]]) - 1)
                            } else {
                                df_predictors <- df_predictors + 1
                            }
                        }
                    }
                    if (df_predictors > 0) {
                        epv <- evt_count / df_predictors
                        if (epv < 5) {
                            # Use STRONG_WARNING for critically low EPV
                            private$.addNotice(jmvcore::NoticeType$STRONG_WARNING,
                                glue::glue("Low events-per-variable (EPV ≈ {round(epv,2)}). Odds ratios may be unstable; consider penalized/Firth logistic regression."))
                        } else if (epv < 10) {
                            extra_warnings <- c(extra_warnings,
                                glue::glue("Borderline events-per-variable (EPV ≈ {round(epv,2)}). Interpret odds ratios with caution."))
                        }
                    }

                    # Simple separation check for binary predictors
                    for (v in explanatory_variable_names) {
                        if (!is.null(v) && v %in% names(mydata) && is.factor(mydata[[v]]) && nlevels(mydata[[v]]) == 2) {
                            tab <- table(mydata[[v]], mydata[[dependent_variable_name_from_label]])
                            if (any(tab == 0)) {
                                extra_warnings <- c(extra_warnings,
                                    glue::glue("Possible separation detected for '{v}' (zero cells in 2x2 table). Consider penalized/Firth logistic regression."))
                            }
                        }
                    }
                }

                # Merge extra warnings into validation results now that they're populated
                if (length(extra_warnings) > 0) {
                    validation_results$warnings <- c(validation_results$warnings, extra_warnings)
                }

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


                # Note: text2 will be updated with diagnostic metrics if nomogram is enabled
                # Set model metrics output initially (may be updated later in nomogram block)
                self$results$text2$setContent(text2)

                results1 <-  knitr::kable(tOdds[[1]],
                             row.names = FALSE,
                             align = c("l", "l", "r", "r", "r", "r"),
                             format = "html")
                self$results$text$setContent(results1)




                ## plot Data ----
                # Filter out dependent variable rows from the finalfit table for plotting
                # The dependent variable shouldn't appear in an odds ratio plot
                tOdds_for_plot <- tOdds[[1]]
                if (!is.null(tOdds_for_plot) && nrow(tOdds_for_plot) > 0) {
                    # Remove rows where the first column matches the outcome variable name
                    # finalfit includes the dependent variable levels in the output
                    outcome_var_name <- self$options$outcome
                    tOdds_for_plot <- tOdds_for_plot[tOdds_for_plot[[1]] != outcome_var_name, , drop = FALSE]
                }

                plotData <- list(
                    "plotData" = mydata,
                    "formulaDependent" = formulaDependent,
                    "formulaExplanatory" = formulaExplanatory,
                    "originalNames" = all_labels,
                    "originalOutcomeName" = self$options$outcome,
                    "originalExplanatoryNames" = self$options$explanatory,
                    "filteredTable" = tOdds_for_plot
                )

                image <- self$results$plot
                image$setState(plotData)




                if (self$options$showNomogram) {
                    private$.checkpoint()

                    # Select predictor for diagnostic metrics
                    diagnostic_predictor <- NULL
                    diagnostic_predictor_original_name <- NULL
                    user_selected_predictor <- FALSE

                    # Check if user explicitly selected a diagnostic predictor
                    if (!is.null(self$options$diagnosticPredictor)) {
                        diagnostic_predictor <- names(all_labels)[match(self$options$diagnosticPredictor, all_labels)]
                        diagnostic_predictor_original_name <- self$options$diagnosticPredictor
                        user_selected_predictor <- TRUE

                        if (length(diagnostic_predictor) > 1) {
                            warn_msg <- glue::glue("Diagnostic predictor label matches multiple variables; using '{diagnostic_predictor_original_name}'.")
                            private$.addNotice(jmvcore::NoticeType$WARNING, warn_msg)
                            diagnostic_predictor <- diagnostic_predictor[1]
                        }

                        # Check if selected predictor is in explanatory variables
                        if (diagnostic_predictor_original_name %in% self$options$explanatory) {
                            private$.addNotice(jmvcore::NoticeType$INFO,
                                glue::glue("Using '{diagnostic_predictor_original_name}' (from model) for diagnostic metrics (sensitivity, specificity, likelihood ratios)."))
                        } else {
                            private$.addNotice(jmvcore::NoticeType$INFO,
                                glue::glue("Using '{diagnostic_predictor_original_name}' for diagnostic metrics. Note: This variable is NOT in the logistic regression model. Diagnostic metrics are calculated independently of the odds ratio model."))
                        }
                    }

                    # Default to first explanatory variable if not specified
                    if (is.null(diagnostic_predictor) && length(explanatory_variable_names) > 0) {
                        diagnostic_predictor <- explanatory_variable_names[1]
                        diagnostic_predictor_original_name <- self$options$explanatory[1]

                        if (length(explanatory_variable_names) > 1) {
                            private$.addNotice(jmvcore::NoticeType$INFO,
                                glue::glue("Using '{diagnostic_predictor_original_name}' (first explanatory variable) for diagnostic metrics. To use a different variable, specify it in the 'Diagnostic Predictor' box."))
                        } else {
                            private$.addNotice(jmvcore::NoticeType$INFO,
                                glue::glue("Using '{diagnostic_predictor_original_name}' for diagnostic metrics (sensitivity, specificity, likelihood ratios)."))
                        }
                    }

                    # Ensure diagnostic predictor is available
                    if (is.null(diagnostic_predictor) || !(diagnostic_predictor %in% names(mydata))) {
                        private$.addNotice(jmvcore::NoticeType$WARNING,
                            "No diagnostic predictor available; skipping likelihood ratios/nomogram. Please select a binary variable for diagnostic metrics.")
                        return()
                    }

                    # Convert to factor if needed
                    if (!is.factor(mydata[[diagnostic_predictor]])) {
                        mydata[[diagnostic_predictor]] <- factor(mydata[[diagnostic_predictor]])
                    }

                    # Ensure diagnostic predictor is binary
                    if (nlevels(mydata[[diagnostic_predictor]]) != 2) {
                        private$.addNotice(jmvcore::NoticeType$WARNING,
                            glue::glue("Diagnostic predictor '{diagnostic_predictor_original_name}' has {nlevels(mydata[[diagnostic_predictor]])} levels but must be binary (exactly 2 levels) for likelihood ratio calculations. Please select a different variable or recode this variable to binary."))
                        return()
                    }

                    # Calculate likelihood ratios
                    lr_results <- private$.calculateLikelihoodRatios(
                        mydata,
                        dependent_variable_name_from_label,
                        diagnostic_predictor,
                        self$options$outcomeLevel  # User-specified positive outcome level
                    )
                    
                    # Check if likelihood ratio calculation failed
                    if (!is.null(lr_results$error) && lr_results$error) {
                        stop(lr_results$message)
                    }

                    # Create diagnostic metrics text with explanatory information
                    # Using paste0() for reliability (glue had template issues)

                    # Get predictor level warning if present
                    predictor_warning <- if (!is.null(lr_results$predictor_level_warning)) lr_results$predictor_level_warning else ""

                    # Get statistical warnings and recommendations if present
                    statistical_warnings <- if (!is.null(lr_results$statistical_warnings) && lr_results$statistical_warnings != "") lr_results$statistical_warnings else ""
                    statistical_recommendations <- if (!is.null(lr_results$statistical_recommendations) && lr_results$statistical_recommendations != "") lr_results$statistical_recommendations else ""

                    # Get contingency table details
                    cont_table <- lr_results$contingency_table
                    predictor_levels <- rownames(cont_table)
                    outcome_levels <- colnames(cont_table)

                    # Build full metrics text with all features
                    metrics_text <- paste0(
                        "<br>",
                        predictor_warning,

                        "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0;'>",
                        "<b>Diagnostic Metrics:</b><br>",
                        "Sensitivity: ", round(lr_results$sensitivity * 100, 1), "%<br>",
                        "Specificity: ", round(lr_results$specificity * 100, 1), "%<br>",
                        "Positive LR: ", round(lr_results$positive_lr, 2), "<br>",
                        "Negative LR: ", round(lr_results$negative_lr, 2), "<br>",
                        "</div>",

                        statistical_warnings,
                        statistical_recommendations,

                        "<div style='background-color: #e8f5e9; padding: 15px; border-radius: 8px; margin: 10px 0;'>",
                        "<b>⚠️ Important: Please Verify These Interpretations</b><br>",
                        "<small>",
                        "<b>Positive outcome level:</b> '", lr_results$positive_outcome_used, "' ",
                        "<span style='color: #666;'>(", lr_results$outcome_determination_method, ")</span><br>",
                        "<b>Positive predictor level:</b> '", lr_results$positive_predictor_used, "' ",
                        "<span style='color: #666;'>(", lr_results$predictor_determination_method, ")</span><br><br>",

                        "<b>📊 Contingency Table:</b><br>",
                        "<table style='border-collapse: collapse; margin: 5px 0;'>",
                        "<tr><th style='border: 1px solid #ddd; padding: 5px;'></th>",
                        "<th style='border: 1px solid #ddd; padding: 5px;'>", outcome_levels[1], "</th>",
                        "<th style='border: 1px solid #ddd; padding: 5px;'>", outcome_levels[2], "</th></tr>",
                        "<tr><td style='border: 1px solid #ddd; padding: 5px;'><b>", predictor_levels[1], "</b></td>",
                        "<td style='border: 1px solid #ddd; padding: 5px;'>", cont_table[1,1], "</td>",
                        "<td style='border: 1px solid #ddd; padding: 5px;'>", cont_table[1,2], "</td></tr>",
                        "<tr><td style='border: 1px solid #ddd; padding: 5px;'><b>", predictor_levels[2], "</b></td>",
                        "<td style='border: 1px solid #ddd; padding: 5px;'>", cont_table[2,1], "</td>",
                        "<td style='border: 1px solid #ddd; padding: 5px;'>", cont_table[2,2], "</td></tr>",
                        "</table>",
                        "TP: ", lr_results$tp, ", FP: ", lr_results$fp, ", FN: ", lr_results$fn, ", TN: ", lr_results$tn, "<br><br>",

                        "<b>📝 How to Use:</b><br>",
                        "1. Check that the positive outcome level is correct for your study<br>",
                        "2. If incorrect, use the 'Positive Outcome Level' dropdown to specify the correct level<br>",
                        "3. The nomogram calculations depend on these interpretations being correct<br>",
                        "4. Different languages/coding may require manual specification",
                        "</small>",
                        "</div>",
                        "<br>"
                    )

                    # metrics_text now complete with all features
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
                    } else {
                        # Nomogram preparation failed - add user-friendly notice
                        private$.addNotice(jmvcore::NoticeType$WARNING,
                            "Nomogram could not be generated due to model fitting issues. The odds ratio analysis completed successfully, but the nomogram visualization is not available. This may occur with: (1) perfect separation in the data, (2) convergence issues, or (3) insufficient sample size. The main analysis results are still valid.")
                    }

                    # Update results with diagnostic metrics
                    # Set the separate diagnosticMetrics output
                    self$results$diagnosticMetrics$setContent(metrics_text)
                }
                # Educational Explanations ----
                if (self$options$showExplanations) {
                    private$.addExplanations()
                }

                # Add completion notice for successful analysis
                private$.addNotice(jmvcore::NoticeType$INFO,
                    "Odds ratio analysis completed successfully.")

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
            # FIX: Add warning that this is automatically detected and may be wrong
            detection_result <- private$.detectPositiveLevels(predictor_levels)
            positive_predictor_level <- detection_result$level
            predictor_determination_method <- detection_result$method
            positive_predictor_idx <- which(predictor_levels == positive_predictor_level)

            # Create warning message for automatic predictor level detection
            predictor_level_warning <- paste0(
                "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px; margin: 10px 0; border-radius: 4px;'>",
                "<h4 style='margin-top: 0; color: #856404;'>⚠️ Automatic Predictor Level Detection</h4>",
                "<p><strong>The positive predictor level was automatically detected as: '", positive_predictor_level, "'</strong></p>",
                "<p>Method: ", predictor_determination_method, "</p>",
                "<p style='color: #856404;'><strong>Important:</strong> This automatic detection may be incorrect. ",
                "Please verify that '", positive_predictor_level, "' is the correct positive level for your predictor.</p>",
                "<p>If this is wrong, your diagnostic metrics (sensitivity, specificity, likelihood ratios) will be inverted!</p>",
                "<p><strong>Available predictor levels:</strong> ", paste(predictor_levels, collapse = ", "), "</p>",
                "</div>"
            )
            
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

            # FIX: Include predictor level warning in the return
            return(list(
                positive_lr = positive_lr,
                negative_lr = negative_lr,
                sensitivity = sensitivity,
                specificity = specificity,
                diagnostic_info = diagnostic_info,
                predictor_level_warning = predictor_level_warning,  # Add warning to return value
                statistical_warnings = diagnostic_warnings,  # Add statistical warnings
                statistical_recommendations = recommendation_text,  # Add recommendations
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
                    filteredTable <- plotList$filteredTable

                    # Create a temporary dataset with restored variable names for plotting
                    plotDataWithOriginalNames <- private$.createPlotDataWithOriginalNames(
                        mydata,
                        originalNames,
                        formulaDependent,
                        formulaExplanatory
                    )

                    private$.checkpoint()

                    # Use or_plot with original names
                    # The function returns formulas with original variable names that match the restored data
                    plot <- finalfit::or_plot(
                        .data = plotDataWithOriginalNames$data,
                        dependent = plotDataWithOriginalNames$formulaDependent,
                        explanatory = plotDataWithOriginalNames$formulaExplanatory,
                        remove_ref = FALSE,
                        table_text_size = 4,
                        title_text_size = 14,
                        breaks = NULL,
                        column_space = c(-0.5, 0, 0.5),
                        dependent_label = plotList$originalOutcomeName,
                        prefix = "",
                        suffix = ": OR (95% CI, p-value)",
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




        # Nomogram Display ----
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
            
            # Odds Ratio vs Risk Ratio Explanation
            tryCatch({
                self$results$riskMeasuresExplanation$setContent('
            <div style="margin-bottom: 20px; padding: 15px; background-color: #d4edda; border-left: 4px solid #28a745;">
                <h4 style="margin-top: 0; color: #2c3e50;">Understanding Odds Ratio vs Risk Ratio</h4>
                <p><strong>Odds Ratio (OR):</strong> The measure calculated by this analysis.</p>
                <ul>
                    <li><strong>Definition:</strong> Ratio of the odds of outcome in exposed vs unexposed groups</li>
                    <li><strong>Formula:</strong> OR = (a/b) / (c/d) where a,b,c,d are from 2×2 contingency table</li>
                    <li><strong>Interpretation:</strong> OR = 2.0 means the odds of outcome are twice as high in exposed group</li>
                    <li><strong>Use case:</strong> Logistic regression, case-control studies, cross-sectional studies</li>
                </ul>
                <p><strong>Risk Ratio (RR) - NOT calculated by this function:</strong></p>
                <ul>
                    <li><strong>Definition:</strong> Ratio of risks (proportions) between exposed and unexposed groups</li>
                    <li><strong>Use case:</strong> Cohort studies, randomized trials with follow-up data</li>
                    <li><strong>Note:</strong> When outcome is rare (<10%), OR approximates RR</li>
                </ul>
                <p><em>Clinical note:</em> This analysis provides Odds Ratios from logistic regression. For Risk Ratios, use cohort analysis tools.</p>
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
                <p><strong>Diagnostic Metrics Calculated:</strong> This analysis evaluates how well a binary predictor distinguishes between outcome states.</p>
                <ul>
                    <li><strong>Sensitivity (True Positive Rate):</strong> Proportion of actual positives correctly identified
                        <ul style="margin-top: 5px;">
                            <li>Formula: TP / (TP + FN)</li>
                            <li>Example: If sensitivity = 80%, the test detects 80% of cases with the outcome</li>
                        </ul>
                    </li>
                    <li><strong>Specificity (True Negative Rate):</strong> Proportion of actual negatives correctly identified
                        <ul style="margin-top: 5px;">
                            <li>Formula: TN / (TN + FP)</li>
                            <li>Example: If specificity = 90%, the test correctly identifies 90% of cases without the outcome</li>
                        </ul>
                    </li>
                    <li><strong>Positive Likelihood Ratio (LR+):</strong> How much a positive test increases the odds
                        <ul style="margin-top: 5px;">
                            <li>Formula: Sensitivity / (1 - Specificity)</li>
                            <li>LR+ > 10: Strong evidence for diagnosis</li>
                            <li>LR+ = 5-10: Moderate evidence</li>
                            <li>LR+ = 2-5: Weak evidence</li>
                        </ul>
                    </li>
                    <li><strong>Negative Likelihood Ratio (LR-):</strong> How much a negative test decreases the odds
                        <ul style="margin-top: 5px;">
                            <li>Formula: (1 - Sensitivity) / Specificity</li>
                            <li>LR- < 0.1: Strong evidence against diagnosis</li>
                            <li>LR- = 0.1-0.2: Moderate evidence</li>
                            <li>LR- = 0.2-0.5: Weak evidence</li>
                        </ul>
                    </li>
                </ul>
                <p><strong>Note:</strong> PPV and NPV are NOT calculated by this function as they depend on disease prevalence in your specific population.</p>
                <p><em>Clinical application:</em> These metrics help evaluate diagnostic tests and biomarkers. Use the nomogram to convert likelihood ratios into post-test probabilities.</p>
            </div>
            ')

            }, error = function(e) {
                # Silently ignore if result doesn't exist
            })
            
            # Nomogram Analysis Explanation
            tryCatch({
                self$results$nomogramAnalysisExplanation$setContent('
            <div style="margin-bottom: 20px; padding: 15px; background-color: #f8d7da; border-left: 4px solid #dc3545;">
                <h4 style="margin-top: 0; color: #721c24;">Understanding Diagnostic Nomogram</h4>

                <p><strong>What is a Diagnostic Nomogram?</strong></p>
                <p>A Fagan nomogram is a visual tool for converting pre-test probability to post-test probability using likelihood ratios from a diagnostic test. Unlike the risk prediction nomogram (which uses all variables), the diagnostic nomogram evaluates how well a SINGLE binary predictor performs as a diagnostic test.</p>

                <h5 style="color: #721c24;">Key Components:</h5>
                <ul>
                    <li><strong>Pre-test Probability:</strong> Baseline probability of the outcome before testing (e.g., population prevalence)</li>
                    <li><strong>Likelihood Ratio (LR):</strong> How much the test result changes the odds
                        <ul style="margin-top: 5px;">
                            <li>LR+ > 1: Positive test increases probability</li>
                            <li>LR- < 1: Negative test decreases probability</li>
                        </ul>
                    </li>
                    <li><strong>Post-test Probability:</strong> Updated probability after incorporating test results</li>
                </ul>

                <h5 style="color: #721c24;">How to Use the Nomogram:</h5>
                <ol>
                    <li>Start with pre-test probability (left axis)</li>
                    <li>Draw a straight line through the likelihood ratio (middle axis)</li>
                    <li>Read the post-test probability (right axis)</li>
                    <li>Determine if this information is sufficient for clinical decision-making</li>
                </ol>

                <hr style="margin: 15px 0; border: none; border-top: 1px solid #f5c6cb;">

                <h5 style="color: #721c24; margin-top: 15px;">What is a Diagnostic Predictor?</h5>

                <p><strong>The diagnostic predictor is the single binary variable you want to evaluate as a diagnostic test.</strong></p>

                <div style="background-color: #fff3cd; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong>Example:</strong> If you select "LVI" (Lymphovascular Invasion: Absent/Present) as the diagnostic predictor,
                    the nomogram answers: <em>"How good is LVI at predicting my outcome?"</em>
                </div>

                <p><strong>Requirements:</strong></p>
                <ul>
                    <li>✅ Must be <strong>binary</strong> (exactly 2 levels): Yes/No, Present/Absent, Positive/Negative</li>
                    <li>✅ Examples: Sex (Male/Female), LVI (Absent/Present), Treatment (Control/Treated)</li>
                    <li>❌ Cannot use continuous variables: Age, Tumor Size (infinite possible values)</li>
                    <li>❌ Cannot use multi-category: Grade 1/2/3, Stage I/II/III/IV</li>
                </ul>

                <p><strong>Why Binary Only?</strong></p>
                <p>Diagnostic test performance metrics (sensitivity, specificity, likelihood ratios) are calculated from a 2×2 contingency table:</p>

                <table style="border-collapse: collapse; margin: 10px 0; font-size: 0.9em;">
                    <tr><th style="border: 1px solid #ddd; padding: 5px;"></th>
                        <th style="border: 1px solid #ddd; padding: 5px;">Outcome +</th>
                        <th style="border: 1px solid #ddd; padding: 5px;">Outcome -</th></tr>
                    <tr><td style="border: 1px solid #ddd; padding: 5px;">Test +</td>
                        <td style="border: 1px solid #ddd; padding: 5px;">True Positive</td>
                        <td style="border: 1px solid #ddd; padding: 5px;">False Positive</td></tr>
                    <tr><td style="border: 1px solid #ddd; padding: 5px;">Test -</td>
                        <td style="border: 1px solid #ddd; padding: 5px;">False Negative</td>
                        <td style="border: 1px solid #ddd; padding: 5px;">True Negative</td></tr>
                </table>

                <p><small><em>Sensitivity = TP/(TP+FN), Specificity = TN/(TN+FP), LR+ = Sensitivity/(1-Specificity)</em></small></p>

                <p><strong>Selection Guidelines:</strong></p>
                <ul>
                    <li><strong>Not specified:</strong> Uses first explanatory variable automatically</li>
                    <li><strong>In your regression model:</strong> Evaluates its performance while controlling for other variables</li>
                    <li><strong>Not in your model:</strong> Evaluates it independently (useful for comparing tests)</li>
                </ul>

                <div style="background-color: #d1ecf1; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong>💡 Clinical Tip:</strong> If your first variable is continuous (e.g., Age), you must manually select
                    a binary variable for the diagnostic predictor, or the nomogram will not be generated.
                </div>
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
                    trimmed_name <- trimws(current_name)
                    
                    # Skip if it's not a string or is empty
                    if (is.na(trimmed_name) || trimmed_name == "" || !is.character(trimmed_name)) next
                    
                    # Handle different finalfit naming patterns:
                    # 1. Direct variable name match
                    if (trimmed_name %in% names(name_mapping)) {
                        first_col[i] <- name_mapping[trimmed_name]
                    }
                    # 2. Variable name with factor level (e.g., "variable_nameLevel1")
                    else {
                        # Try to find a matching cleaned name that's a prefix
                        for (clean_name in names(name_mapping)) {
                            if (startsWith(trimmed_name, clean_name)) {
                                # Replace the cleaned prefix with original name
                                suffix <- substring(trimmed_name, nchar(clean_name) + 1)
                                suffix <- trimws(gsub("^[:=]", "", suffix))
                                first_col[i] <- paste0(name_mapping[clean_name], if (suffix != "") paste0(" ", suffix) else "")
                                break
                            }
                        }
                        # 3. For ordered/indented rows (leading spaces/dashes), try loose match
                        if (first_col[i] == current_name && grepl(" ", trimmed_name, fixed = TRUE)) {
                            for (clean_name in names(name_mapping)) {
                                if (grepl(paste0("^", clean_name, "\\b"), trimmed_name)) {
                                    level_part <- trimws(sub(clean_name, "", trimmed_name, fixed = TRUE))
                                    first_col[i] <- paste0(name_mapping[clean_name], if (level_part != "") paste0(" ", level_part) else "")
                                    break
                                }
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
        .createPlotDataWithOriginalNames = function(mydata, all_labels, dep_clean, exp_clean) {
            if (is.null(all_labels) || length(all_labels) == 0) {
                # Fallback: return data as-is if no labels available
                return(list(
                    data = mydata,
                    formulaDependent = dep_clean,
                    formulaExplanatory = exp_clean
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

            # Map dependent and explanatory from cleaned names to original names
            # Use unname() to avoid named vectors which cause "Can't rename variables" error
            original_dependent <- if (!is.null(dep_clean) && dep_clean %in% names(name_mapping)) {
                unname(name_mapping[dep_clean])
            } else {
                dep_clean
            }

            original_explanatory <- unname(sapply(exp_clean, function(clean_name) {
                if (!is.null(clean_name) && clean_name %in% names(name_mapping)) {
                    name_mapping[clean_name]
                } else {
                    clean_name
                }
            }, USE.NAMES = FALSE))

            return(list(
                data = plotData,
                formulaDependent = original_dependent,
                formulaExplanatory = original_explanatory
            ))
        }




        )
)
