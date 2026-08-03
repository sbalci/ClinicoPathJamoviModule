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
#' and preserves original variable labels for display. The regression uses
#' complete cases for the selected outcome and explanatory variables only.
#'
#' @section International Usage:
#' For international users, the function includes an outcomeLevel parameter to
#' explicitly specify which outcome level represents the positive case. This is
#' important for correct interpretation of likelihood ratios and diagnostic metrics.
#'
#' @section Nomogram Features:
#' When showNomogram is enabled, the function generates:
#' \itemize{
#'   \item Prediction nomogram based on the fitted logistic model
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
#' @seealso \code{\link[finalfit]{finalfit}}, \code{\link[rms]{rms}}
#'
#' @importFrom R6 R6Class
#'
#' @return An \code{R6} class generator object for the \code{oddsratioClass} backend; used internally by the jamovi analysis wrapper and not called directly.

oddsratioClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "oddsratioClass",
    inherit = oddsratioBase,
    private = list(

        .nom_object = NULL,

        # Notice management helpers ----
        # Notices are rendered to dedicated Html outputs (errors / strongWarnings /
        # warnings / infoMessages) to avoid the protobuf serialization error caused
        # by jmvcore::Notice objects passed to self$results$insert(). The legacy
        # function signature is retained so callers don't need to change.
        .addNotice = function(type, message, name = NULL) {
            # jmvcore::NoticeType constants: ERROR=0, STRONG_WARNING=1, WARNING=2, INFO=3
            type_str <- switch(
                as.character(type),
                "0" = "error",
                "1" = "strongWarning",
                "2" = "warning",
                "3" = "info",
                "warning"
            )
            title <- switch(
                type_str,
                "error" = "Error",
                "strongWarning" = "Strong warning",
                "warning" = "Warning",
                "info" = "Information",
                "Notice"
            )
            private$.addHtmlMessage(type_str, title, message)
        },

        .addHtmlMessage = function(type, title, message) {
            output_name <- switch(type,
                "error" = "errors",
                "strongWarning" = "strongWarnings",
                "warning" = "warnings",
                "info" = "infoMessages",
                "warnings"
            )
            border_color <- switch(type,
                "error" = "#d9534f",
                "strongWarning" = "#e67e22",
                "warning" = "#f0ad4e",
                "info" = "#5bc0de",
                "#f0ad4e"
            )
            current_content <- self$results[[output_name]]$content
            if (is.null(current_content)) current_content <- ""
            new_message <- sprintf(
                '<div style="margin: 10px 0; padding: 10px; border-left: 4px solid %s; background-color: #f8f9fa;"><strong>%s:</strong> %s</div>',
                border_color,
                htmltools::htmlEscape(title),
                htmltools::htmlEscape(message)
            )
            self$results[[output_name]]$setContent(paste0(current_content, new_message))
            self$results[[output_name]]$setVisible(TRUE)
        },

        # Legacy no-ops kept so existing call sites don't break
        .insertNotices = function() invisible(NULL),

        .resetNotices = function() {
            for (out in c("errors", "strongWarnings", "warnings", "infoMessages")) {
                if (!is.null(self$results[[out]])) {
                    self$results[[out]]$setContent("")
                    self$results[[out]]$setVisible(FALSE)
                }
            }
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
                if (is.factor(outcome_data)) {
                    outcome_data <- droplevels(outcome_data)
                }
                
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
                        if (is.factor(var_data_clean)) {
                            var_data_clean <- droplevels(var_data_clean)
                        }
                        
                        if (length(var_data_clean) == 0) {
                            validation_results$warnings <- c(validation_results$warnings,
                                glue::glue("Explanatory variable '{var_name}' contains no non-missing values."))
                        } else if (length(unique(var_data_clean)) == 1) {
                            validation_results$warnings <- c(validation_results$warnings,
                                glue::glue("Explanatory variable '{var_name}' has no variation (all values are the same). It will not contribute to the model."))
                        } else if (is.factor(var_data)) {
                            # Factor variable validation
                            factor_levels <- levels(var_data_clean)
                            factor_counts <- table(var_data_clean)

                            if (is.ordered(var_data)) {
                                validation_results$info <- c(validation_results$info,
                                    glue::glue("Ordered factor '{var_name}' will be treated as nominal (unordered) for modeling and output."))
                            }
                            
                            if (length(factor_levels) > 10) {
                                validation_results$warnings <- c(validation_results$warnings,
                                    glue::glue("Explanatory variable '{var_name}' has {length(factor_levels)} levels. Consider grouping categories or using as continuous if ordinal."))
                            }
                            
                            # Check for sparse categories
                            sparse_categories <- sum(factor_counts < 5)
                            if (sparse_categories > 0) {
                                validation_results$warnings <- c(validation_results$warnings,
                                    glue::glue("Explanatory variable '{var_name}' has {sparse_categories} categories with fewer than 5 observations. Consider combining categories."))
                            }
                        } else if (is.numeric(var_data)) {
                            # Numeric variable validation
                            if (any(is.infinite(var_data_clean))) {
                                validation_results$warnings <- c(validation_results$warnings,
                                    glue::glue("Explanatory variable '{var_name}' contains infinite values."))
                            }
                            
                            # Check for extreme values
                            q99 <- quantile(var_data_clean, 0.99, na.rm = TRUE)
                            q01 <- quantile(var_data_clean, 0.01, na.rm = TRUE)
                            extreme_high <- sum(var_data_clean > q99 + 3 * (q99 - q01), na.rm = TRUE)
                            extreme_low <- sum(var_data_clean < q01 - 3 * (q99 - q01), na.rm = TRUE)
                            
                            if (extreme_high + extreme_low > 0) {
                                validation_results$info <- c(validation_results$info,
                                    glue::glue("Explanatory variable '{var_name}' may contain extreme outliers ({extreme_high + extreme_low} potential outliers)."))
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

            # Reset notices at start of each run
            private$.resetNotices()
            private$.nom_object <- NULL







            # Initial Message ----

            if (is.null(self$options$explanatory) || is.null(self$options$outcome))
            {

                todo <- glue::glue("
                    <br>Welcome to ClinicoPath
                    <br><br>
                    Select one binary outcome, identify its positive/event level,
                    and add one or more categorical or continuous explanatory variables.
                    The analysis reports logistic-regression odds ratios with confidence
                    intervals and a matching forest plot.
                    <br><br>
                    Missing observations are removed only for variables used by each
                    calculation. Variable names with spaces or special characters are
                    handled automatically and restored in the output.
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
                    <br>\u2022 Odds ratios interpretation
                    <br>\u2022 Likelihood ratios
                    <br>\u2022 Sensitivity and specificity
                    <br>\u2022 Diagnostic test performance metrics
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
                    jmvcore::reject("No data available for analysis. The dataset has no rows or all observations have been filtered out. Check your data import, verify variable selections, and review missing-value patterns.")
                }

                # CHECKPOINT: Before data preprocessing - which can be time-consuming
                private$.checkpoint()

                # Restrict all preprocessing to variables used by this analysis.
                # In particular, missing values in unrelated dataset columns must
                # not remove observations from the regression.
                model_columns <- unique(c(self$options$outcome, self$options$explanatory))
                selected_columns <- model_columns
                if (isTRUE(self$options$showNomogram) &&
                    !is.null(self$options$diagnosticPredictor)) {
                    selected_columns <- unique(c(
                        selected_columns,
                        self$options$diagnosticPredictor
                    ))
                }

                missing_columns <- setdiff(selected_columns, names(self$data))
                if (length(missing_columns) > 0) {
                    jmvcore::reject(paste0(
                        "Selected variable(s) not found in the data: ",
                        paste(missing_columns, collapse = ", "), "."
                    ))
                }

                mydata <- self$data[, selected_columns, drop = FALSE]

                # Perform input validation before processing
                validation_results <- private$.validateInputs(
                    mydata[, model_columns, drop = FALSE],
                    self$options$outcome,
                    self$options$explanatory,
                    self$options$outcomeLevel
                )
                
                # Handle validation errors - stop execution if critical errors found
                if (validation_results$should_stop) {
                    validation_error <- sub(
                        "[.]+$",
                        "",
                        paste(validation_results$errors, collapse = "; ")
                    )
                    critical_message <- paste0(
                        "Critical validation errors detected: ",
                        validation_error,
                        ". Ensure the outcome variable has exactly 2 levels, explanatory variables have sufficient variation, and consider removing rows with missing data.")
                    # Surface the critical error in the dedicated 'errors' Html output
                    # before aborting, so the previously-empty item is populated.
                    private$.addNotice(jmvcore::NoticeType$ERROR, critical_message)
                    jmvcore::reject(critical_message)
                }

                original_names <- names(mydata)

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
                if (length(dependent_variable_name_from_label) == 0) {
                    jmvcore::reject(
                        "The selected outcome could not be mapped after variable-name normalization."
                    )
                }
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

                        # Inform user which level is modeled as the positive outcome
                        private$.addNotice(
                            jmvcore::NoticeType$INFO,
                            paste0(
                                "Outcome variable releveled: '", positive_level,
                                "' is now modeled as the positive outcome (event)."
                            )
                        )
                    } else {
                        # Warn if selected level doesn't exist
                        private$.addNotice(
                            jmvcore::NoticeType$WARNING,
                            paste0(
                                "Selected positive outcome level '", positive_level,
                                "' not found in data. Available levels: ",
                                paste(levels(outcome_var), collapse = ", ")
                            )
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

                # Keep a cleaned copy for the independent 2x2 diagnostic
                # calculation. Its complete cases are outcome + diagnostic
                # predictor, whereas the regression complete cases are outcome +
                # explanatory variables. Neither path is affected by unrelated
                # columns or by missingness in the other path.
                diagnostic_source <- mydata

                model_names <- unique(c(
                    dependent_variable_name_from_label,
                    explanatory_variable_names
                ))
                model_names <- model_names[!is.na(model_names) & nzchar(model_names)]
                mydata <- mydata[, model_names, drop = FALSE]
                mydata <- mydata[stats::complete.cases(mydata), , drop = FALSE]

                if (nrow(mydata) == 0) {
                    jmvcore::reject(
                        "No complete observations remain for the selected outcome and explanatory variables."
                    )
                }

                for (v in names(mydata)) {
                    if (is.factor(mydata[[v]])) {
                        mydata[[v]] <- droplevels(mydata[[v]])
                    }
                }

                observed_outcome <- mydata[[dependent_variable_name_from_label]]
                if (!is.factor(observed_outcome)) {
                    observed_outcome <- factor(observed_outcome)
                    mydata[[dependent_variable_name_from_label]] <- observed_outcome
                }
                if (nlevels(observed_outcome) != 2 ||
                    !(self$options$outcomeLevel %in% levels(observed_outcome))) {
                    jmvcore::reject(paste0(
                        "After removing incomplete cases for the selected model, the outcome must retain exactly two observed levels including the selected positive level '",
                        self$options$outcomeLevel, "'."
                    ))
                }

                no_variation <- vapply(
                    explanatory_variable_names,
                    function(v) length(unique(mydata[[v]])) < 2,
                    logical(1)
                )
                if (any(no_variation)) {
                    jmvcore::reject(paste0(
                        "The following explanatory variable(s) have no variation after complete-case filtering: ",
                        paste(self$options$explanatory[no_variation], collapse = ", "),
                        "."
                    ))
                }

                non_finite <- vapply(
                    explanatory_variable_names,
                    function(v) is.numeric(mydata[[v]]) && any(!is.finite(mydata[[v]])),
                    logical(1)
                )
                if (any(non_finite)) {
                    jmvcore::reject(paste0(
                        "The following numeric explanatory variable(s) contain infinite values: ",
                        paste(self$options$explanatory[non_finite], collapse = ", "),
                        ". Replace infinite values before fitting the model."
                    ))
                }

                # Additional diagnostics: EPV and separation checks
                extra_warnings <- c()
                if (!is.null(dependent_variable_name_from_label) && !is.null(self$options$outcomeLevel)) {
                    # Events-per-variable uses the rarer of the two outcome classes:
                    # logistic-regression EPV is driven by the minority count, which is
                    # not necessarily the user-selected positive level.
                    n_event <- sum(mydata[[dependent_variable_name_from_label]] == self$options$outcomeLevel, na.rm = TRUE)
                    n_nonevent <- sum(!is.na(mydata[[dependent_variable_name_from_label]])) - n_event
                    evt_count <- min(n_event, n_nonevent)
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
                                glue::glue("Low events-per-variable (EPV \u2248 {round(epv,2)}). Odds ratios may be unstable; consider penalized/Firth logistic regression."))
                        } else if (epv < 10) {
                            extra_warnings <- c(extra_warnings,
                                glue::glue("Borderline events-per-variable (EPV \u2248 {round(epv,2)}). Interpret odds ratios with caution."))
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
                    for (warn_msg in extra_warnings) {
                        private$.addNotice(jmvcore::NoticeType$WARNING, warn_msg)
                    }
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

                # myformula <- .asSurvivalFormula(myformula)

                # CHECKPOINT: Before running finalfit - which can be computationally intensive
                private$.checkpoint()

                fit_standard_model <- function() {
                    tryCatch(
                        finalfit::finalfit(
                            .data = mydata,
                            dependent = formulaDependent,
                            explanatory = formulaExplanatory,
                            metrics = TRUE
                        ),
                        error = function(e) {
                            message <- paste0(
                                "Standard logistic regression could not be fitted: ",
                                conditionMessage(e),
                                ". Review outcome coding, predictor variation, sparse categories, and separation; consider Firth penalized regression when appropriate."
                            )
                            private$.addNotice(jmvcore::NoticeType$ERROR, message)
                            jmvcore::reject(message)
                        }
                    )
                }

                # Use Firth penalized logistic regression if requested and package is available
                if (isTRUE(self$options$usePenalized)) {
                    if (requireNamespace("logistf", quietly = TRUE)) {
                        private$.checkpoint()
                        # Use our custom Firth implementation that mimics finalfit structure
                        tOdds <- private$.fitFirthModel(
                            .data = mydata,
                            dependent = dependent_variable_name_from_label,
                            explanatory = explanatory_variable_names
                        )
                        
                        private$.addNotice(jmvcore::NoticeType$INFO,
                            "Firth penalized likelihood logistic regression used to reduce bias and handle potential separation.")
                    } else {
                        private$.addNotice(jmvcore::NoticeType$STRONG_WARNING,
                            "The 'logistf' package is required for Firth penalized regression but is not installed. Falling back to standard logistic regression.")
                        
                        tOdds <- fit_standard_model()
                    }
                } else {
                    tOdds <- fit_standard_model()
                }
                
                # Restore original variable names in the finalfit output table
                if (!is.null(tOdds[[1]]) && nrow(tOdds[[1]]) > 0) {
                    tOdds[[1]] <- private$.restoreOriginalNamesInTable(tOdds[[1]], all_labels)
                }

                # Replace odds ratios that the fit could not actually identify.
                # See .markNonEstimableOR(): under separation glm returns an
                # arbitrary large coefficient with an unbounded interval, and
                # printing exp() of it states an odds ratio of ~1e23 as fact.
                nonest <- private$.markNonEstimableOR(tOdds[[1]])
                tOdds[[1]] <- nonest$table
                if (length(nonest$flagged) > 0) {
                    private$.addNotice(
                        jmvcore::NoticeType$STRONG_WARNING,
                        paste0(
                            "The odds ratio could not be estimated for: ",
                            paste(nonest$flagged, collapse = ", "),
                            ". The confidence interval is unbounded, which means the data separate the outcome ",
                            "perfectly (or nearly so) for that variable and the maximum-likelihood estimate does ",
                            "not exist. The cell is shown as 'not estimable' rather than as the arbitrary large ",
                            "number the fitting algorithm stopped at. Enable Firth penalized logistic regression ",
                            "to obtain a finite estimate, or combine sparse categories. Note that the forest plot ",
                            "below is drawn by finalfit from the same unpenalized fit and will still show the ",
                            "unbounded estimate."
                        )
                    )
                }







                # Main analysis execution starts here


















                text2 <- paste0(
                    "<br><b>Model Metrics:</b> ",
                    paste(htmltools::htmlEscape(unlist(tOdds[[2]])), collapse = " "),
                    "<br>"
                )


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
                    diagnostics_ok <- TRUE

                    # Check if user explicitly selected a diagnostic predictor
                    if (!is.null(self$options$diagnosticPredictor)) {
                        diagnostic_predictor <- names(all_labels)[match(self$options$diagnosticPredictor, all_labels)]
                        diagnostic_predictor_original_name <- self$options$diagnosticPredictor

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
                    if (is.null(diagnostic_predictor) ||
                        !(diagnostic_predictor %in% names(diagnostic_source))) {
                        private$.addNotice(jmvcore::NoticeType$WARNING,
                            "No diagnostic predictor is available. Diagnostic metrics were skipped; the prediction nomogram can still be generated from the regression model.")
                        self$results$diagnosticMetrics$setContent(
                            "<p>Diagnostic metrics were not calculated because no diagnostic predictor was available.</p>"
                        )
                        diagnostics_ok <- FALSE
                    }

                    if (diagnostics_ok) {
                        diagnostic_data <- diagnostic_source[, c(
                            dependent_variable_name_from_label,
                            diagnostic_predictor
                        ), drop = FALSE]
                        diagnostic_data <- diagnostic_data[
                            stats::complete.cases(diagnostic_data),
                            , drop = FALSE
                        ]

                        diagnostic_data[[dependent_variable_name_from_label]] <- droplevels(factor(
                            diagnostic_data[[dependent_variable_name_from_label]]
                        ))
                        diagnostic_data[[diagnostic_predictor]] <- droplevels(factor(
                            diagnostic_data[[diagnostic_predictor]]
                        ))

                        diagnostic_levels <- nlevels(diagnostic_data[[diagnostic_predictor]])
                        outcome_levels_n <- nlevels(diagnostic_data[[dependent_variable_name_from_label]])
                        if (nrow(diagnostic_data) == 0 || diagnostic_levels != 2 ||
                            outcome_levels_n != 2) {
                            private$.addNotice(jmvcore::NoticeType$WARNING,
                                glue::glue("Diagnostic metrics for '{diagnostic_predictor_original_name}' require paired complete observations with exactly two observed predictor levels and two observed outcome levels. The prediction nomogram is unaffected."))
                            self$results$diagnosticMetrics$setContent(
                                "<p>Diagnostic metrics were not calculated because a valid paired 2×2 table could not be formed.</p>"
                            )
                            diagnostics_ok <- FALSE
                        }
                    }

                    if (diagnostics_ok) {
                        # Calculate likelihood ratios with support for specified predictor level
                        lr_results <- private$.calculateLikelihoodRatios(
                            diagnostic_data,
                            dependent_variable_name_from_label,
                            diagnostic_predictor,
                            self$options$outcomeLevel,
                            self$options$predictorLevel
                        )
                    
                        # A diagnostic configuration error should not discard a
                        # successfully fitted regression model or prediction nomogram.
                        if (!is.null(lr_results$error) && lr_results$error) {
                            private$.addNotice(jmvcore::NoticeType$WARNING, lr_results$message)
                            self$results$diagnosticMetrics$setContent(paste0(
                                "<p>", htmltools::htmlEscape(lr_results$message), "</p>"
                            ))
                            diagnostics_ok <- FALSE
                        }
                    }

                    if (diagnostics_ok) {
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

                    # Format diagnostic metrics, rendering undefined (NA) values
                    # explicitly rather than as a misleading 0% or numeric. LRs
                    # derived from an undefined metric are also shown as undefined.
                    # A point estimate on its own overstates what a small 2x2
                    # supports, so each metric carries its interval; when the
                    # interval is not computable the text says so rather than
                    # leaving a blank that reads as certainty.
                    lr_ci <- lr_results$ci
                    ci_pct <- function(key) {
                        v <- if (is.null(lr_ci)) NULL else lr_ci[[key]]
                        if (is.null(v) || length(v) != 2 || anyNA(v)) return("")
                        sprintf(" (95%% CI %.1f-%.1f%%)", v[1] * 100, v[2] * 100)
                    }
                    ci_num <- function(key) {
                        v <- if (is.null(lr_ci)) NULL else lr_ci[[key]]
                        if (is.null(v) || length(v) != 2 || anyNA(v) || any(!is.finite(v))) return("")
                        sprintf(" (95%% CI %.2f-%.2f)", v[1], v[2])
                    }

                    sens_txt <- if (is.na(lr_results$sensitivity)) "undefined (no positive cases)" else paste0(sprintf("%.1f%%", lr_results$sensitivity * 100), ci_pct("sensitivity"))
                    spec_txt <- if (is.na(lr_results$specificity)) "undefined (no negative cases)" else paste0(sprintf("%.1f%%", lr_results$specificity * 100), ci_pct("specificity"))
                    # Inf is a real (diverging) value, not a missing one, so it
                    # needs its own wording -- previously only is.na was caught
                    # and "Inf" was printed verbatim.
                    fmt_lr <- function(v) {
                        if (is.null(v) || length(v) == 0 || is.na(v)) "undefined (no informative cells)"
                        else if (is.infinite(v)) "infinite (zero false results in this cell)"
                        else sprintf("%.2f", v)
                    }
                    plr_txt  <- paste0(fmt_lr(lr_results$positive_lr), ci_num("positive_lr"))
                    nlr_txt  <- paste0(fmt_lr(lr_results$negative_lr), ci_num("negative_lr"))

                    # Build full metrics text with all features
                    metrics_text <- paste0(
                        "<br>",
                        predictor_warning,

                        "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0;'>",
                        "<b>Diagnostic Metrics:</b><br>",
                        "Sensitivity: ", sens_txt, "<br>",
                        "Specificity: ", spec_txt, "<br>",
                        "Positive LR: ", plr_txt, "<br>",
                        "Negative LR: ", nlr_txt, "<br>",
                        "<small style='color:#555;'>Unadjusted 2&times;2 estimates. Wilson score intervals for sensitivity and specificity; log method for the likelihood ratios (epiR::epi.tests).</small>",
                        "</div>",

                        statistical_warnings,
                        statistical_recommendations,

                        "<div style='background-color: #e8f5e9; padding: 15px; border-radius: 8px; margin: 10px 0;'>",
                        "<b> Important: Please Verify These Interpretations</b><br>",
                        "<small>",
                        "<b>Positive outcome level:</b> '", htmltools::htmlEscape(lr_results$positive_outcome_used), "' ",
                        "<span style='color: #666;'>(", htmltools::htmlEscape(lr_results$outcome_determination_method), ")</span><br>",
                        "<b>Positive predictor level:</b> '", htmltools::htmlEscape(lr_results$positive_predictor_used), "' ",
                        "<span style='color: #666;'>(", htmltools::htmlEscape(lr_results$predictor_determination_method), ")</span><br><br>",

                        "<b> Contingency Table:</b><br>",
                        "<table style='border-collapse: collapse; margin: 5px 0;'>",
                        "<tr><th style='border: 1px solid #ddd; padding: 5px;'></th>",
                        "<th style='border: 1px solid #ddd; padding: 5px;'>", htmltools::htmlEscape(outcome_levels[1]), "</th>",
                        "<th style='border: 1px solid #ddd; padding: 5px;'>", htmltools::htmlEscape(outcome_levels[2]), "</th></tr>",
                        "<tr><td style='border: 1px solid #ddd; padding: 5px;'><b>", htmltools::htmlEscape(predictor_levels[1]), "</b></td>",
                        "<td style='border: 1px solid #ddd; padding: 5px;'>", cont_table[1,1], "</td>",
                        "<td style='border: 1px solid #ddd; padding: 5px;'>", cont_table[1,2], "</td></tr>",
                        "<tr><td style='border: 1px solid #ddd; padding: 5px;'><b>", htmltools::htmlEscape(predictor_levels[2]), "</b></td>",
                        "<td style='border: 1px solid #ddd; padding: 5px;'>", cont_table[2,1], "</td>",
                        "<td style='border: 1px solid #ddd; padding: 5px;'>", cont_table[2,2], "</td></tr>",
                        "</table>",
                        "TP: ", lr_results$tp, ", FP: ", lr_results$fp, ", FN: ", lr_results$fn, ", TN: ", lr_results$tn, "<br><br>",

                        "<b> How to Use:</b><br>",
                        "1. Check that the positive outcome level is correct for your study<br>",
                        "2. If incorrect, use the 'Positive Outcome Level' dropdown to specify the correct level<br>",
                        "3. These unadjusted diagnostic metrics depend on these interpretations being correct<br>",
                        "4. Different languages/coding may require manual specification",
                        "</small>",
                        "</div>",
                        "<br>"
                    )

                        self$results$diagnosticMetrics$setContent(metrics_text)
                    }

                    private$.checkpoint()

                    # rms::nomogram represents an ordinary maximum-likelihood
                    # lrm model. Do not display it beside Firth estimates because
                    # that would silently present two different fitted models.
                    if (isTRUE(self$options$usePenalized)) {
                        self$results$nomogram$setContent(
                            paste0(
                                "<p><strong>Prediction nomogram not generated.</strong> ",
                                "The odds-ratio model uses Firth penalized likelihood, ",
                                "whereas the available nomogram implementation uses ",
                                "ordinary maximum-likelihood logistic regression.</p>"
                            )
                        )
                        private$.addNotice(jmvcore::NoticeType$WARNING,
                            "The prediction nomogram was not generated because Firth penalized regression is selected. Diagnostic metrics, when available, remain unadjusted 2x2 estimates.")
                    } else {
                        # Prepare data for the prediction nomogram.
                        nom_results <- private$.prepareRmsNomogram(
                            mydata,
                            dependent_variable_name_from_label,
                            explanatory_variable_names
                        )

                        if (!is.null(nom_results$fit)) {
                            private$.createNomogram(nom_results$fit, nom_results$dd)
                        } else {
                            private$.addNotice(jmvcore::NoticeType$WARNING,
                                "Prediction nomogram could not be generated due to model fitting issues. The odds ratio analysis completed successfully. The nomogram is an unvalidated visualization and its failure does not alter the fitted odds-ratio table.")
                        }

                        # Persist only serializable ingredients needed to rebuild
                        # the nomogram after save/reload.
                        self$results$plot_nomogram$setState(list(
                            data = mydata,
                            dependent = dependent_variable_name_from_label,
                            explanatory = explanatory_variable_names
                        ))
                    }
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
        .calculateLikelihoodRatios = function(data, outcome_var, predictor_var, user_positive_outcome = NULL, user_positive_predictor = NULL) {
            # Ensure we have factor variables
            predictor <- factor(data[[predictor_var]])
            outcome <- factor(data[[outcome_var]])
            
            # Create contingency table
            cont_table <- table(predictor, outcome)
            
            # Ensure we have a 2x2 table for binary variables
            if (nrow(cont_table) != 2 || ncol(cont_table) != 2) {
                # ... (error handling remains same)
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
            
            # Determine positive predictor level
            if (!is.null(user_positive_predictor) &&
                !(user_positive_predictor %in% predictor_levels)) {
                return(list(
                    error = TRUE,
                    message = paste0(
                        "The selected positive predictor level '",
                        user_positive_predictor,
                        "' is not present among paired complete observations. Available levels: ",
                        paste(predictor_levels, collapse = ", "), "."
                    )
                ))
            }

            if (!is.null(user_positive_predictor)) {
                positive_predictor_level <- user_positive_predictor
                predictor_determination_method <- "User-specified"
            } else {
                # Fallback to automatic detection if not specified or not found
                detection_result <- private$.detectPositiveLevels(predictor_levels)
                positive_predictor_level <- detection_result$level
                predictor_determination_method <- detection_result$method
            }
            positive_predictor_idx <- which(predictor_levels == positive_predictor_level)

            # Create messaging for predictor level determination
            if (predictor_determination_method == "User-specified") {
                 predictor_level_warning <- paste0(
                    "<div style='background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 15px; margin: 10px 0; border-radius: 4px;'>",
                    "<b>Predictor Level Modeling:</b><br>",
                    "The level '", htmltools::htmlEscape(positive_predictor_level), "' is used as the positive/exposure category as specified.",
                    "</div>"
                )
            } else {
                predictor_level_warning <- paste0(
                    "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px; margin: 10px 0; border-radius: 4px;'>",
                    "<h4 style='margin-top: 0; color: #856404;'> Automatic Predictor Level Detection</h4>",
                    "<p><strong>The positive predictor level was automatically detected as: '", htmltools::htmlEscape(positive_predictor_level), "'</strong></p>",
                    "<p>Method: ", htmltools::htmlEscape(predictor_determination_method), "</p>",
                    "<p style='color: #856404;'><strong>Important:</strong> Please verify that '", htmltools::htmlEscape(positive_predictor_level), "' is the correct positive level. ",
                    "If this is wrong, diagnostic metrics will be inverted.</p>",
                    "<p>Use the 'Predictor Positive Level' option to set this manually.</p>",
                    "</div>"
                )
            }
            
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
                        paste0("\u2022 <b>", rec$test, ":</b> ", rec$reason, " (Use: ", rec$code, ")")
                    } else {
                        paste0("\u2022 ", rec)
                    }
                })
                
                recommendation_text <- paste0(
                    "<div style='background-color: #d4edda; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                    "<b> Statistical Recommendations:</b><br>",
                    paste(recommendations_list, collapse = "<br>"),
                    "</div>"
                )
            }
            
            # Calculate sensitivity and specificity.
            # These are undefined when the 2x2 table has no actual positives
            # (tp + fn == 0) or no actual negatives (tn + fp == 0). Keep them NA
            # rather than silently coercing to 0, which would masquerade as real
            # (perfect-miss) diagnostic performance.
            sensitivity <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)  # True Positive Rate
            specificity <- if ((tn + fp) == 0) NA_real_ else tn / (tn + fp)  # True Negative Rate

            # Likelihood ratios are undefined if either component metric is undefined.
            if (is.na(sensitivity) || is.na(specificity)) {
                positive_lr <- NA_real_
                negative_lr <- NA_real_
            } else {
                # Distinguish a diverging ratio from an indeterminate one.
                #
                # The old guards short-circuited on specificity alone, so they
                # fired regardless of the numerator. That reported Inf -- i.e.
                # infinitely strong evidence -- for the two genuinely 0/0 cases:
                # LR+ when the test flagged nobody (fp == 0 and tp == 0), and
                # LR- when it missed nobody (tn == 0 and fn == 0). Those are
                # undefined, not infinite.
                positive_lr <- if (specificity == 1) {
                    if (sensitivity == 0) NA_real_ else Inf
                } else sensitivity / (1 - specificity)

                negative_lr <- if (specificity == 0) {
                    if (sensitivity == 1) NA_real_ else Inf
                } else (1 - sensitivity) / specificity
            }
            
            # Confidence intervals for the diagnostic metrics.
            #
            # These were reported as bare point estimates. A sensitivity of 63.8%
            # from 20 patients and from 2000 are not the same claim, and the panel
            # gave a clinician no way to tell them apart -- uncertainty that is
            # simply absent reads as uncertainty that is small.
            #
            # epiR::epi.tests() is what the sibling diagnostic analyses in this
            # module already use (decision, decisioncalculator, decisioncompare,
            # decisioncombine, digitalvalidation), so the intervals here agree with
            # theirs by construction: Wilson score for sensitivity/specificity and
            # the standard log method (Simel) for the likelihood ratios. It takes
            # the 2x2 as test-positive-first by outcome-positive-first, which is
            # exactly the tp/fp/fn/tn already resolved above -- so the orientation
            # cannot drift away from the point estimates computed from them.
            ci <- tryCatch({
                et <- epiR::epi.tests(matrix(c(tp, fp, fn, tn), nrow = 2, byrow = TRUE))
                det <- as.data.frame(et$detail)
                grab <- function(stat) {
                    row <- det[det$statistic == stat, , drop = FALSE]
                    if (nrow(row) == 0) c(NA_real_, NA_real_)
                    else c(as.numeric(row$lower[1]), as.numeric(row$upper[1]))
                }
                list(sensitivity = grab("se"), specificity = grab("sp"),
                     positive_lr  = grab("lr.pos"), negative_lr = grab("lr.neg"))
            }, error = function(e) NULL)

            if (is.null(ci))
                ci <- list(sensitivity = c(NA_real_, NA_real_),
                           specificity = c(NA_real_, NA_real_),
                           positive_lr = c(NA_real_, NA_real_),
                           negative_lr = c(NA_real_, NA_real_))

            # Create diagnostic information
            diagnostic_info <- paste0(
                "Positive outcome level: '", positive_outcome_level, "' (", outcome_determination_method, ")\n",
                "Positive predictor level: '", positive_predictor_level, "' (", predictor_determination_method, ")\n",
                "Contingency table:\n",
                "  ", predictor_levels[1], " \u2192 ", outcome_levels[1], ": ", cont_table[1,1],
                " | ", outcome_levels[2], ": ", cont_table[1,2], "\n",
                "  ", predictor_levels[2], " \u2192 ", outcome_levels[1], ": ", cont_table[2,1],
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
                ci = ci,
                positive_outcome_used = positive_outcome_level,
                positive_predictor_used = positive_predictor_level,
                outcome_determination_method = outcome_determination_method,
                predictor_determination_method = predictor_determination_method,
                contingency_table = cont_table,
                tp = tp, fp = fp, fn = fn, tn = tn
            ))
        },

        # Odds ratios from a non-converged fit are numerical noise, not estimates.
        #
        # Under (quasi-)separation glm's IRLS stops wherever the iteration limit
        # leaves it, and finalfit renders exp() of that coefficient to two
        # decimals. A perfectly separated 2x2 printed
        #     "118848049086800030859264.00 (0.00-Inf, p=1.000)"
        # i.e. an odds ratio of 1.19e23 shown to a clinician as though it were an
        # estimate, when the parameter is simply not identified.
        #
        # The reliable tell is the CONFIDENCE INTERVAL, not the point estimate: a
        # finite odds ratio cannot have an infinite upper confidence limit unless
        # the likelihood is flat at the boundary. Large but genuinely estimable
        # odds ratios keep a finite upper bound -- the Firth fit on that same
        # separated table gives 3721.00 (181.52-1160619.10) -- so this rule leaves
        # them untouched and cannot fire on an ordinary analysis.
        .markNonEstimableOR = function(tbl) {
            empty <- list(table = tbl, flagged = character(0))
            if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) return(empty)

            or_cols <- which(grepl("^OR", names(tbl)))
            if (length(or_cols) == 0) return(empty)

            # finalfit prints the variable name only on the first row of each
            # block, so forward-fill it to name the offending term in the notice.
            var_col <- as.character(tbl[[1]])
            var_col[is.na(var_col) | !nzchar(trimws(var_col))] <- NA_character_
            for (k in seq_along(var_col))
                if (is.na(var_col[k]) && k > 1) var_col[k] <- var_col[k - 1]

            flagged <- character(0)
            for (j in or_cols) {
                cells <- as.character(tbl[[j]])
                for (i in seq_along(cells)) {
                    cell <- cells[i]
                    if (is.na(cell) || !nzchar(cell) || identical(trimws(cell), "-")) next
                    # "<OR> (<lo>-<hi>, p=<p>)"
                    m <- regmatches(cell, regexec("^([^ ]+) \\(([^-]+)-([^,]+),", cell))[[1]]
                    if (length(m) != 4) next
                    hi <- suppressWarnings(as.numeric(m[4]))
                    if (!is.na(hi) && is.finite(hi)) next
                    cells[i] <- "not estimable"
                    if (!is.na(var_col[i])) flagged <- unique(c(flagged, var_col[i]))
                }
                tbl[[j]] <- cells
            }
            list(table = tbl, flagged = flagged)
        },

        # Prepares data and fits logistic regression model for nomogram creation
        # Uses rms package to create datadist object and fit lrm model
        # Convention: both this helper and .fitFirthModel take RAW variable names and escape
        # them internally via jmvcore::composeTerm/composeTerms. Keep any new formula-building
        # helper consistent with this (pass raw names in, escape inside) so callers never have
        # to reason about whether a name is already escaped. (Verified equivalent to the former
        # constructFormula/composeTerms caller-side escaping across names with spaces/./()// .)
        .prepareRmsNomogram = function(data, dependent, explanatory) {
            tryCatch({
                # First create datadist object
                dd <- rms::datadist(data[, explanatory, drop = FALSE])
                old_datadist <- getOption("datadist")
                on.exit(options(datadist = old_datadist), add = TRUE)
                options(datadist = dd)

                # Create formula for model
                formula_str <- paste(jmvcore::composeTerm(dependent), "~", paste(jmvcore::composeTerms(as.list(explanatory)), collapse = " + "))

                # Fit logistic regression model
                private$.checkpoint()

                fit <- rms::lrm(
                    formula = .asSurvivalFormula(formula_str),
                    data = data,
                    x = TRUE,
                    y = TRUE
                )

                return(list(fit = fit, dd = dd))
            }, error = function(e) {
                detailed_error <- paste(
                    " Nomogram Preparation Error:",
                    paste("Technical error:", e$message),
                    "",
                    " Common causes and solutions:",
                    "\u2022 Perfect separation: Some variable levels perfectly predict the outcome",
                    "  \u2192 Try combining categories or removing problematic variables",
                    "\u2022 Convergence issues: Model failed to converge",
                    "  \u2192 Check for multicollinearity or try simpler model",
                    "\u2022 Insufficient data: Too few observations per variable",
                    "  \u2192 Increase sample size or reduce number of variables",
                    "\u2022 Missing values: Incomplete data after cleaning",
                    "  \u2192 Review data preprocessing steps",
                    "",
                    " Suggested next steps:",
                    "\u2022 Check model summary for convergence warnings",
                    "\u2022 Review variable distributions for separation issues",
                    "\u2022 Consider using fewer explanatory variables",
                    "\u2022 Verify data quality and completeness",
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
            if (isTRUE(self$options$usePenalized)) {
                return(FALSE)
            }

            oldpar <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(oldpar), add = TRUE)

            # Fast path: reuse the nomogram built during .run() when available.
            nom <- private$.nom_object

            # Fallback for serialization/reload: the private field is not persisted,
            # so refit the model + nomogram from the state stored on this image.
            if (is.null(nom) && !is.null(image$state)) {
                st <- image$state
                prep <- private$.prepareRmsNomogram(st$data, st$dependent, st$explanatory)
                if (!is.null(prep$fit)) {
                    nom <- try(
                        rms::nomogram(prep$fit, fun = stats::plogis,
                                      funlabel = "Predicted Probability"),
                        silent = TRUE)
                    if (inherits(nom, "try-error")) nom <- NULL
                }
            }

            if (is.null(nom)) {
                return(FALSE)
            }

            private$.checkpoint()

            par(mar = c(4, 4, 2, 2))
            plot(nom)
            return(TRUE)
        }








        # Creates forest plot for odds ratios using finalfit
        ,
        .plot = function(image, ggtheme, theme, ...) {
          # - the plot function ----
                    # plotData <- image$state
                    if (is.null(self$options$explanatory) || is.null(self$options$outcome))
                return()
                    if (nrow(self$data) == 0)
                jmvcore::reject('Data contains no (complete) rows')
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

                    # finalfit::or_plot() fits its own unpenalized glm internally
                    # and has no way to accept a logistf object (there is no
                    # fit2df.logistf method). With Firth enabled the plot
                    # therefore drew maximum-likelihood odds ratios beside a
                    # table of penalized ones -- two different answers for the
                    # same model on the same page, which is exactly the case
                    # where they diverge most (sparse data and separation).
                    if (isTRUE(self$options$usePenalized) &&
                        requireNamespace("logistf", quietly = TRUE)) {

                        firth_plot <- private$.firthOrPlot(
                            .data       = plotDataWithOriginalNames$data,
                            dependent   = plotDataWithOriginalNames$formulaDependent,
                            explanatory = plotDataWithOriginalNames$formulaExplanatory,
                            outcome_label = plotList$originalOutcomeName
                        )

                        if (!is.null(firth_plot)) {
                            # arrangeGrob() returns a gtable, which must be drawn
                            # with grid.draw(); print() would not render it.
                            grid::grid.newpage()
                            grid::grid.draw(firth_plot)
                            return(TRUE)
                        }
                        # Fall through only if the penalized fit failed; the
                        # notice below keeps the mismatch from being silent.
                        private$.addNotice(jmvcore::NoticeType$WARNING,
                            "The penalized (Firth) forest plot could not be produced, so the plot below shows unpenalized maximum-likelihood odds ratios. These will not match the penalized estimates in the table above.")
                    }

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
            html_content <- paste0(html_content, '<p>The nomogram plot above provides a visual tool for prediction based on the maximum-likelihood logistic regression model.</p>')
            html_content <- paste0(html_content, '<p><strong>Components:</strong></p>')
            html_content <- paste0(html_content, '<ul style="margin: 5px 0; padding-left: 20px;">')
            html_content <- paste0(html_content, '<li><strong>Points:</strong> Top scale showing point values for each predictor</li>')
            html_content <- paste0(html_content, '<li><strong>Predictor Scales:</strong> Individual scales for each variable in the model</li>')
            html_content <- paste0(html_content, '<li><strong>Total Points:</strong> Sum of all individual predictor points</li>')
            html_content <- paste0(html_content, '<li><strong>Predicted Probability:</strong> Bottom scale showing the predicted outcome probability</li>')
            html_content <- paste0(html_content, '</ul>')
            html_content <- paste0(html_content, '<p><strong>Validation note:</strong> This display does not establish calibration, discrimination, or external validity. Validate the fitted model before clinical use.</p>')
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
                    <li><strong>Confidence Intervals:</strong> Quantify uncertainty around each estimated odds ratio</li>
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
                    <li><strong>Formula:</strong> OR = (a/b) / (c/d) where a,b,c,d are from 2\u00d72 contingency table</li>
                    <li><strong>Interpretation:</strong> OR = 2.0 means the odds of outcome are twice as high in exposed group</li>
                    <li><strong>Use case:</strong> Logistic regression, case-control studies, cross-sectional studies</li>
                </ul>
                <p><strong>Risk Ratio (RR) - NOT calculated by this function:</strong></p>
                <ul>
                    <li><strong>Definition:</strong> Ratio of risks (proportions) between exposed and unexposed groups</li>
                    <li><strong>Use case:</strong> Cohort studies, randomized trials with follow-up data</li>
                    <li><strong>Note:</strong> OR approaches RR as outcome risk becomes low; the degree of approximation also depends on baseline risk and effect size</li>
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
                <p><small>The LR magnitude bands above are rough interpretive guides, not universal clinical decision thresholds.</small></p>
                <p><strong>Note:</strong> PPV and NPV are NOT calculated by this function as they depend on disease prevalence in your specific population.</p>
                <p><em>Clinical application:</em> These unadjusted metrics help evaluate a binary test or biomarker. The prediction nomogram is a separate multivariable model display and does not convert likelihood ratios into post-test probabilities.</p>
            </div>
            ')

            }, error = function(e) {
                # Silently ignore if result doesn't exist
            })
            
            # Nomogram Analysis Explanation
            tryCatch({
                self$results$nomogramAnalysisExplanation$setContent('
            <div style="margin-bottom: 20px; padding: 15px; background-color: #f8d7da; border-left: 4px solid #dc3545;">
                <h4 style="margin-top: 0; color: #721c24;">Understanding Prediction and Diagnostic Outputs</h4>

                <p><strong>Prediction nomogram:</strong> The plotted nomogram assigns points to all explanatory variables in the maximum-likelihood logistic regression model and maps total points to predicted outcome probability. It is not a Fagan nomogram and does not display pre-test-to-post-test probability conversion.</p>

                <h5 style="color: #721c24;">Prediction Nomogram Components:</h5>
                <ul>
                    <li><strong>Points:</strong> Contribution assigned to each predictor value</li>
                    <li><strong>Total Points:</strong> Sum of predictor contributions</li>
                    <li><strong>Predicted Probability:</strong> Model-based probability corresponding to total points</li>
                </ul>

                <p><strong>Important:</strong> The plot is a visual representation of the fitted model, not evidence of calibration, discrimination, transportability, or clinical utility. Internal and external validation are required before clinical use. It is not generated when Firth regression is selected because that would mix different estimation methods.</p>

                <hr style="margin: 15px 0; border: none; border-top: 1px solid #f5c6cb;">

                <h5 style="color: #721c24; margin-top: 15px;">What is a Diagnostic Predictor?</h5>

                <p><strong>The diagnostic predictor is the single binary variable you want to evaluate as a diagnostic test.</strong></p>

                <div style="background-color: #fff3cd; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong>Example:</strong> If you select "LVI" (Lymphovascular Invasion: Absent/Present) as the diagnostic predictor,
                    the diagnostic table answers: <em>"How well does LVI alone distinguish the selected outcome states?"</em>
                </div>

                <p><strong>Requirements:</strong></p>
                <ul>
                    <li> Must be <strong>binary</strong> (exactly 2 levels): Yes/No, Present/Absent, Positive/Negative</li>
                    <li> Examples: Sex (Male/Female), LVI (Absent/Present), Treatment (Control/Treated)</li>
                    <li> Cannot use continuous variables: Age, Tumor Size (infinite possible values)</li>
                    <li> Cannot use multi-category: Grade 1/2/3, Stage I/II/III/IV</li>
                </ul>

                <p><strong>Why Binary Only?</strong></p>
                <p>Diagnostic test performance metrics (sensitivity, specificity, likelihood ratios) are calculated from a 2\u00d72 contingency table:</p>

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
                    <li><strong>In your regression model:</strong> Diagnostic metrics are still unadjusted and use only its paired 2×2 table</li>
                    <li><strong>Not in your model:</strong> Evaluates it independently using paired complete outcome/test observations</li>
                </ul>

                <div style="background-color: #d1ecf1; padding: 10px; border-radius: 5px; margin: 10px 0;">
                    <strong> Clinical Tip:</strong> If your first variable is continuous (e.g., Age), you must manually select
                    a binary diagnostic predictor to obtain sensitivity, specificity, and likelihood ratios. The prediction nomogram can still be generated from the regression variables.
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
                        " Small expected cell counts detected (minimum = ", round(min_expected, 2), "). ",
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
                        " Very small sample size (n = ", total_n, "). ",
                        "Results should be interpreted with extreme caution."
                    ))
                }
                
                # Check for zero cells
                if (any(cont_table == 0)) {
                    warnings <- append(warnings, 
                        " Zero cells detected in contingency table. This may affect odds ratio calculation."
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


        # Helper function for configurable positive level detection
        ,
        .detectPositiveLevels = function(levels, language = "auto") {
            # Configure positive indicators by language
            if (language == "auto") {
                # Detect language from levels or use default
                language <- if (any(grepl("[\u0131\u00fc\u011f\u015f\u00e7\u00f6\u0130\u00dc\u011e\u015e\u00c7\u00d6]", levels))) "tr" else "en"
            }
            
            # Positive indicators by language
            indicators <- switch(language,
                "en" = c("Positive", "Yes", "Present", "Exposed", "High", "Abnormal", "1", "TRUE", "Bad", "Dead", "Event"),
                "tr" = c("Pozitif", "Evet", "Mevcut", "Maruz", "Y\u00fcksek", "Anormal", "1", "DO\u011eRU", "K\u00f6t\u00fc", "\u00d6l\u00fc", "Olay", 
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

        # Helper function to fit Firth penalized logistic regression
        # Mimics the output structure of finalfit::finalfit for compatibility
        ,
        # Forest plot drawn from the PENALIZED fit.
        #
        # finalfit::or_plot() cannot render a logistf object, so when Firth is
        # requested the plot is built here from the penalized coefficients and
        # their profile-likelihood intervals. Returns NULL on any failure, and
        # the caller then falls back to the unpenalized plot WITH a warning
        # rather than silently showing mismatched numbers.
        # finalfit's p-value convention: "p<0.001" below the threshold.
        .fmtP = function(p) {
            if (is.na(p)) return("p=NA")
            if (p < 0.001) "p<0.001" else sprintf("p=%.3f", p)
        }
        ,
        .firthOrPlot = function(.data, dependent, explanatory, outcome_label = NULL) {
            tryCatch({
                # These are restored ORIGINAL variable names, so they may contain
                # spaces or other non-syntactic characters. composeTerm
                # backtick-quotes them, which is correct here because this is a
                # formula string (never use it as a data[[ ]] key).
                fml <- stats::as.formula(paste0(
                    jmvcore::composeTerm(dependent), " ~ ",
                    paste(vapply(explanatory, jmvcore::composeTerm, character(1)),
                          collapse = " + ")))
                fit <- logistf::logistf(fml, data = .data)

                cf <- stats::coef(fit)
                nm <- names(cf)
                est <- data.frame(
                    term  = nm,
                    or    = exp(unname(cf)),
                    lower = exp(unname(fit$ci.lower)),
                    upper = exp(unname(fit$ci.upper)),
                    p     = unname(fit$prob),
                    stringsAsFactors = FALSE)
                est <- est[est$term != "(Intercept)", , drop = FALSE]
                if (nrow(est) == 0) return(NULL)

                # Rebuild the variable/level structure finalfit's or_plot shows.
                # logistf reports coefficients as paste0(variable, level), so the
                # split is by longest matching variable name.
                n_total <- nrow(.data)
                rows <- list()
                for (v in explanatory) {
                    col <- .data[[v]]
                    if (is.null(col)) next

                    if (is.factor(col) || is.character(col) || is.logical(col)) {
                        col <- as.factor(col)
                        lvls <- levels(col)
                        for (j in seq_along(lvls)) {
                            lv <- lvls[j]
                            n_lv <- sum(!is.na(col) & col == lv)
                            pct  <- if (n_total > 0) 100 * n_lv / n_total else NA_real_
                            if (j == 1) {
                                # reference level: no estimate, shown as "-"
                                rows[[length(rows) + 1]] <- data.frame(
                                    variable = v, level = lv,
                                    n_show = sprintf("%d (%.1f)", n_lv, pct),
                                    or = NA_real_, lower = NA_real_, upper = NA_real_,
                                    or_text = "-", stringsAsFactors = FALSE)
                            } else {
                                hit <- est[est$term == paste0(v, lv), , drop = FALSE]
                                if (nrow(hit) == 0) next
                                rows[[length(rows) + 1]] <- data.frame(
                                    variable = v, level = lv,
                                    n_show = sprintf("%d (%.1f)", n_lv, pct),
                                    or = hit$or[1], lower = hit$lower[1], upper = hit$upper[1],
                                    or_text = sprintf("%.2f (%.2f-%.2f, %s)",
                                                      hit$or[1], hit$lower[1], hit$upper[1],
                                                      private$.fmtP(hit$p[1])),
                                    stringsAsFactors = FALSE)
                            }
                        }
                    } else {
                        hit <- est[est$term == v, , drop = FALSE]
                        if (nrow(hit) == 0) next
                        num <- jmvcore::toNumeric(col)
                        rows[[length(rows) + 1]] <- data.frame(
                            variable = v, level = "Mean (SD)",
                            n_show = sprintf("%.1f (%.1f)", mean(num, na.rm = TRUE),
                                             stats::sd(num, na.rm = TRUE)),
                            or = hit$or[1], lower = hit$lower[1], upper = hit$upper[1],
                            or_text = sprintf("%.2f (%.2f-%.2f, %s)",
                                              hit$or[1], hit$lower[1], hit$upper[1],
                                              private$.fmtP(hit$p[1])),
                            stringsAsFactors = FALSE)
                    }
                }
                if (length(rows) == 0) return(NULL)
                df <- do.call(rbind, rows)

                # Only the first row of each variable block carries its name, as
                # in the finalfit table.
                df$var_show <- ifelse(duplicated(df$variable), "", df$variable)

                # Top row first, like or_plot.
                df$y <- rev(seq_len(nrow(df)))
                ylim <- c(0.4, nrow(df) + 1.1)
                hdr  <- nrow(df) + 0.85

                sz <- 3.3
                t1 <- ggplot2::ggplot(df) +
                    ggplot2::geom_text(ggplot2::aes(x = 0,    y = y, label = var_show),
                                       hjust = 0, size = sz) +
                    ggplot2::geom_text(ggplot2::aes(x = 1.05, y = y, label = level),
                                       hjust = 0, size = sz) +
                    ggplot2::geom_text(ggplot2::aes(x = 2.05, y = y, label = n_show),
                                       hjust = 1, size = sz) +
                    ggplot2::geom_text(ggplot2::aes(x = 2.25, y = y, label = or_text),
                                       hjust = 0, size = sz) +
                    ggplot2::annotate("text", x = 0,    y = hdr, label = "Variable",
                                      hjust = 0, fontface = "bold", size = sz) +
                    ggplot2::annotate("text", x = 2.05, y = hdr, label = "all",
                                      hjust = 1, fontface = "bold", size = sz) +
                    ggplot2::annotate("text", x = 2.25, y = hdr,
                                      label = "OR (95% CI, p-value)",
                                      hjust = 0, fontface = "bold", size = sz) +
                    ggplot2::scale_x_continuous(limits = c(0, 4.3), expand = c(0, 0)) +
                    ggplot2::scale_y_continuous(limits = ylim, expand = c(0, 0)) +
                    ggplot2::labs(x = "OR, 95% CI (Firth penalized)", y = NULL) +
                    ggplot2::theme_classic() +
                    # The right-hand panel carries an x-axis, which consumes
                    # vertical space. theme_void() here would make this panel
                    # taller, and every row would then sit at a different height
                    # from its own confidence interval. Keeping the same axis
                    # furniture but drawing it in invisible ink makes the two
                    # panel bodies exactly the same height.
                    ggplot2::theme(
                        axis.title.x = ggplot2::element_text(size = 12, colour = NA),
                        axis.text.x  = ggplot2::element_text(colour = NA),
                        axis.ticks.x = ggplot2::element_line(colour = NA),
                        axis.line.x  = ggplot2::element_line(colour = NA),
                        axis.title.y = ggplot2::element_blank(),
                        axis.text.y  = ggplot2::element_blank(),
                        axis.ticks.y = ggplot2::element_blank(),
                        axis.line.y  = ggplot2::element_blank())

                fin <- is.finite(df$or) & is.finite(df$lower) & is.finite(df$upper)
                xr  <- range(c(df$lower[fin], df$upper[fin], 1), na.rm = TRUE)

                g1 <- ggplot2::ggplot(df[fin, , drop = FALSE]) +
                    ggplot2::geom_vline(xintercept = 1, linetype = "longdash",
                                        colour = "black") +
                    ggplot2::geom_errorbarh(
                        ggplot2::aes(x = or, y = y, xmin = lower, xmax = upper),
                        height = 0.2, na.rm = TRUE) +
                    ggplot2::geom_point(ggplot2::aes(x = or, y = y),
                                        size = 2.4, shape = 22, fill = "black",
                                        na.rm = TRUE) +
                    ggplot2::scale_x_continuous(trans = "log10", limits = xr) +
                    ggplot2::scale_y_continuous(limits = ylim, expand = c(0, 0)) +
                    ggplot2::labs(x = "OR, 95% CI (Firth penalized)", y = NULL) +
                    ggplot2::theme_classic() +
                    ggplot2::theme(
                        axis.title.x = ggplot2::element_text(size = 12),
                        axis.text.y  = ggplot2::element_blank(),
                        axis.ticks.y = ggplot2::element_blank(),
                        axis.line.y  = ggplot2::element_blank())

                ttl <- if (!is.null(outcome_label))
                    grid::textGrob(paste0(outcome_label, ": OR (95% CI, p-value)"),
                                   gp = grid::gpar(fontsize = 14)) else NULL

                gridExtra::arrangeGrob(
                    t1, g1, ncol = 2, widths = c(3, 2), top = ttl,
                    bottom = grid::textGrob(
                        "Firth penalized likelihood; profile-likelihood confidence intervals.",
                        gp = grid::gpar(fontsize = 9, col = "grey30")))
            }, error = function(e) NULL)
        }
        ,
        .fitFirthModel = function(.data, dependent, explanatory) {
            # Construct formula. Convention (unified with .prepareRmsNomogram): callers pass
            # RAW variable names; this helper escapes them via jmvcore::composeTerm/composeTerms,
            # so names with spaces or special characters are handled safely and callers never
            # have to reason about whether a name is already escaped.
            f <- .asSurvivalFormula(paste(
                jmvcore::composeTerm(dependent), "~",
                paste(jmvcore::composeTerms(as.list(explanatory)), collapse = " + ")))
            
            # Fit Firth model using logistf
            # logistf doesn't directly support data frames in the same way for labels
            # so we use the clean names from mydata
            fit <- tryCatch({
                logistf::logistf(f, data = .data)
            }, error = function(e) {
                jmvcore::reject(paste0("Error fitting Firth model: ", e$message))
            })
            
            # Extract coefficients and CIs
            coefs <- coef(fit)
            # logistf provides profile likelihood CIs which are more robust
            # Transpose to get 2 columns (lower, upper)
            conf_ints <- confint(fit)
            if (is.vector(conf_ints)) {
                # Handle single-predictor case if needed
                conf_ints <- matrix(conf_ints, nrow = 1)
            }
            
            # Calculate Odds Ratios and CIs
            or <- exp(coefs)
            or_lower <- exp(conf_ints[, 1])
            or_upper <- exp(conf_ints[, 2])
            
            # Extract p-values (using Wald or PL test if available)
            # logistf uses likelihood ratio test by default for p-values
            p_vals <- fit$prob
            
            # Build a table structure similar to finalfit's tOdds[[1]]
            # We'll create a simplified version first
            # Column 1: Label, 2: Levels, 3: Count/Total (placeholder)
            # 4: OR (univariable - NA here), 5: OR (multivariable)
            
            # Get variable names from the model
            var_names <- names(coefs)

            # Drop the intercept: finalfit omits it from the OR table, and an
            # exponentiated intercept is not an odds ratio for any predictor.
            keep <- var_names != "(Intercept)"
            var_names <- var_names[keep]
            or        <- or[keep]
            or_lower  <- or_lower[keep]
            or_upper  <- or_upper[keep]
            p_vals    <- p_vals[keep]

            # Create the result table
            summary_table <- data.frame(
                Dependent = character(length(var_names)),
                Levels = character(length(var_names)),
                Counts = character(length(var_names)),
                OR_Uni = rep("-", length(var_names)),
                OR_Multi = sprintf("%.2f (%.2f-%.2f, p=%.3f)", or, or_lower, or_upper, p_vals),
                stringsAsFactors = FALSE
            )
            
            # Fill label column (index 1)
            summary_table[[1]] <- var_names
            
            # Calculate model metrics for tOdds[[2]]. Use names rather than
            # positional indices and delegate the penalized AIC definition to
            # logistf's extractAIC method.
            loglik_full <- unname(fit$loglik["full"])
            loglik_null <- unname(fit$loglik["null"])
            penalized_aic <- unname(stats::extractAIC(fit)[2])
            metrics <- list(
                paste("Observations: ", length(fit$y)),
                paste("Firth Log-Likelihood: ", round(loglik_full, 2)),
                paste("Penalized AIC (logistf): ", round(penalized_aic, 2)),
                paste("Penalized likelihood-ratio statistic vs null: ",
                      round(2 * (loglik_full - loglik_null), 3))
            )
            
            return(list(summary_table, metrics))
        }

        )
)
