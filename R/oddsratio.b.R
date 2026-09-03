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
#'   outcome = "mortality",
#'   outcomeLevel = "Yes",
#'   predictorLevel = NULL
#' )
#'
#' # With nomogram and specified outcome level
#' result <- oddsratio(
#'   data = clinical_data,
#'   explanatory = c("age", "treatment"),
#'   outcome = "recurrence",
#'   outcomeLevel = "Yes",
#'   predictorLevel = NULL,
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
                "error" = .("Error"),
                "strongWarning" = .("Strong warning"),
                "warning" = .("Warning"),
                "info" = .("Information"),
                .("Notice")
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
                '<div style="margin: 10px 0; padding: 10px; border-left: 4px solid %s; background-color: rgba(138, 155, 172, 0.06); color: inherit;"><strong>%s:</strong> %s</div>',
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

        # Map janitor-cleaned variable names back to the names the user selected.
        #
        # Look the name up in `all_labels` (cleaned -> original) rather than
        # indexing `self$options$explanatory` by position:
        # `explanatory_variable_names` has entries removed when a label fails to
        # map, while `self$options$explanatory` keeps all of them, so a position
        # in one does not address the same variable in the other. Every
        # user-facing message naming an explanatory variable goes through here.
        .originalNames = function(cleaned, all_labels) {
            out <- unlist(all_labels)[cleaned]
            ifelse(is.na(out), cleaned, unname(out))
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
                            .fmt(
                                .("Outcome variable has {n} levels. For odds ratio analysis, the outcome must be binary (exactly 2 levels). Consider creating a binary variable or using multinomial regression."),
                                n = length(outcome_levels)))
                        validation_results$should_stop <- TRUE
                    } else {
                        # Binary outcome - check for severe imbalance
                        min_count <- min(outcome_counts)
                        total_count <- sum(outcome_counts)
                        min_proportion <- min_count / total_count
                        
                        if (min_count < 5) {
                            validation_results$strong_warnings <- c(validation_results$strong_warnings,
                                .fmt(
                                    .("Outcome variable has very few observations in one category ({minCount} out of {totalCount}). Results may be unreliable."),
                                    minCount = min_count, totalCount = total_count))
                        } else if (min_proportion < 0.05) {
                            minority_pct <- sprintf("%.1f%%", min_proportion * 100)
                            validation_results$strong_warnings <- c(validation_results$strong_warnings,
                                .fmt(
                                    .("Outcome variable is severely imbalanced ({percent} in minority class). Consider using specialized methods for imbalanced data."),
                                    percent = minority_pct))
                        }
                        
                        # Require and validate user-specified outcome level
                        if (is.null(user_outcome_level)) {
                            validation_results$errors <- c(validation_results$errors,
                                .("Please select the positive outcome level from the dropdown menu below the outcome variable."))
                            validation_results$should_stop <- TRUE
                        } else if (!user_outcome_level %in% outcome_levels) {
                            validation_results$errors <- c(validation_results$errors,
                                .fmt(
                                    .("Specified positive outcome level '{level}' not found in outcome variable. Available levels: {levels}"),
                                    level = user_outcome_level,
                                    levels = paste(outcome_levels, collapse = ", ")))
                            validation_results$should_stop <- TRUE
                        } else {
                            validation_results$info <- c(validation_results$info,
                                .fmt(.("Outcome level modeled as the event: '{level}'."), level = user_outcome_level))
                        }
                        
                        validation_results$info <- c(validation_results$info,
                            .fmt(
                                .("Outcome variable summary: {counts}"),
                                counts = paste(names(outcome_counts), "=", outcome_counts, collapse = ", ")))
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
                                .fmt(.("Explanatory variable '{variable}' contains no non-missing values."), variable = var_name))
                        } else if (length(unique(var_data_clean)) == 1) {
                            validation_results$warnings <- c(validation_results$warnings,
                                .fmt(
                                    .("Explanatory variable '{variable}' has no variation (all values are the same). It will not contribute to the model."),
                                    variable = var_name))
                        } else if (is.factor(var_data)) {
                            # Factor variable validation
                            factor_levels <- levels(var_data_clean)
                            factor_counts <- table(var_data_clean)

                            if (is.ordered(var_data)) {
                                validation_results$info <- c(validation_results$info,
                                    .fmt(
                                        .("Ordered factor '{variable}' will be treated as nominal (unordered) for modeling and output."),
                                        variable = var_name))
                            }
                            
                            if (length(factor_levels) > 10) {
                                validation_results$warnings <- c(validation_results$warnings,
                                    .fmt(
                                        .("Explanatory variable '{variable}' has {n} levels. Consider grouping categories or using as continuous if ordinal."),
                                        variable = var_name, n = length(factor_levels)))
                            }
                            
                            # Check for sparse categories
                            sparse_categories <- sum(factor_counts < 5)
                            if (sparse_categories > 0) {
                                validation_results$warnings <- c(validation_results$warnings,
                                    .fmt(
                                        .("Explanatory variable '{variable}' has {n} categories with fewer than 5 observations. Consider combining categories."),
                                        variable = var_name, n = sparse_categories))
                            }
                        } else if (is.numeric(var_data)) {
                            # Numeric variable validation
                            #
                            # A low-cardinality numeric is fitted as a linear
                            # trend, so a value carried by only a handful of
                            # patients has high leverage on the whole slope. The
                            # factor branch above already warns about sparse
                            # categories; the same column typed as a number used
                            # to get no warning at all, and after the switch to
                            # cont_cut = 0 that silence also swallowed the
                            # separation / "not estimable" signal such a level
                            # would have produced under the old factor coding.
                            n_distinct_num <- length(unique(var_data_clean))
                            if (n_distinct_num > 2 && n_distinct_num <= 10) {
                                sparse_values <- sum(table(var_data_clean) < 5)
                                if (sparse_values > 0) {
                                    validation_results$warnings <- c(validation_results$warnings,
                                        .fmt(
                                            .("Explanatory variable '{variable}' is entered as continuous and has {n} value(s) carried by fewer than 5 observations; those rows have high leverage on the fitted trend."),
                                            variable = var_name, n = sparse_values))
                                }
                            }

                            if (any(is.infinite(var_data_clean))) {
                                validation_results$warnings <- c(validation_results$warnings,
                                    .fmt(.("Explanatory variable '{variable}' contains infinite values."), variable = var_name))
                            }
                            
                            # Check for extreme values
                            q99 <- stats::quantile(var_data_clean, 0.99, na.rm = TRUE)
                            q01 <- stats::quantile(var_data_clean, 0.01, na.rm = TRUE)
                            extreme_high <- sum(var_data_clean > q99 + 3 * (q99 - q01), na.rm = TRUE)
                            extreme_low <- sum(var_data_clean < q01 - 3 * (q99 - q01), na.rm = TRUE)
                            
                            if (extreme_high + extreme_low > 0) {
                                validation_results$info <- c(validation_results$info,
                                    .fmt(
                                        .("Explanatory variable '{variable}' may contain extreme outliers ({n} potential outliers)."),
                                        variable = var_name, n = extreme_high + extreme_low))
                            }
                        }
                    }
                }
            }
            
            # 3. Data quality checks
            total_rows <- nrow(mydata)
            complete_rows <- sum(stats::complete.cases(mydata))
            missing_proportion <- (total_rows - complete_rows) / total_rows
            
            if (missing_proportion > 0.1) {
                validation_results$warnings <- c(validation_results$warnings,
                    .fmt(
                        .("Large amount of missing data: {percent}% of rows will be removed ({removed} out of {total} rows)."),
                        percent = round(missing_proportion * 100, 1),
                        removed = total_rows - complete_rows,
                        total = total_rows))
            } else if (missing_proportion > 0) {
                validation_results$info <- c(validation_results$info,
                    .fmt(
                        .("Missing data: {percent}% of rows will be removed ({removed} out of {total} rows)."),
                        percent = round(missing_proportion * 100, 1),
                        removed = total_rows - complete_rows,
                        total = total_rows))
            }
            
            if (complete_rows < 50) {
                validation_results$warnings <- c(validation_results$warnings,
                    .fmt(
                        .("Small sample size after removing missing data: {n} observations. Results may be unreliable."),
                        n = complete_rows))
            }
            
            # Check for perfect separation risk
            if (complete_rows < length(explanatory_vars) * 10) {
                validation_results$warnings <- c(validation_results$warnings,
                    .("Sample size is small relative to number of explanatory variables. Risk of overfitting or convergence issues."))
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

                todo <- paste0(
                    "<br>", .("Welcome to ClinicoPath"),
                    "<br><br>",
                    .("Select one binary outcome, identify its positive/event level, and add one or more categorical or continuous explanatory variables. The analysis reports logistic-regression odds ratios with confidence intervals and a matching forest plot."),
                    "<br><br>",
                    .("Missing observations are removed only for variables used by each calculation. Variable names with spaces or special characters are handled automatically and restored in the output.")
                )

                # https://finalfit.org/articles/all_tables_examples.html#default-1

                html <- self$results$todo
                html$setContent(todo)
                return()

            } else if (is.null(self$options$outcomeLevel)) {
                
                # Require outcome level selection
                todo <- paste0(
                    "<br><b>", .("Positive Outcome Level Required"), "</b>",
                    "<br><br>",
                    .("Please select which level of your outcome variable represents the 'positive' case (e.g., 'Dead', 'Event', 'Yes', 'Positive')."),
                    "<br><br>",
                    .("This is required for correct calculation of:"),
                    "<br>\u2022 ", .("Odds ratios interpretation"),
                    "<br>\u2022 ", .("Likelihood ratios"),
                    "<br>\u2022 ", .("Sensitivity and specificity"),
                    "<br>\u2022 ", .("Diagnostic test performance metrics"),
                    "<br><br>",
                    .("Use the dropdown menu below the outcome variable to make your selection.")
                )
                
                html <- self$results$todo
                html$setContent(todo)
                return()
                
            } else {

                # All required selections are present: clear the To Do banner and let
                # the main outputs fill in below.
                todo <- ""

                # Insert accumulated notices before main analysis outputs
                private$.insertNotices()

                html <- self$results$todo
                html$setContent(todo)


                if (nrow(self$data) == 0) {
                    jmvcore::reject(.("No data available for analysis. The dataset has no rows or all observations have been filtered out. Check your data import, verify variable selections, and review missing-value patterns."))
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
                    jmvcore::reject(.fmt(
                        .("Selected variable(s) not found in the data: {variables}."),
                        variables = paste(missing_columns, collapse = ", ")
                    ))
                }

                mydata <- jmvcore::select(self$data, selected_columns)

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
                    critical_message <- .fmt(
                        .("Critical validation errors detected: {errors}. Ensure the outcome variable has exactly 2 levels, explanatory variables have sufficient variation, and consider removing rows with missing data."),
                        errors = validation_error)
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
                        .("The selected outcome could not be mapped after variable-name normalization.")
                    )
                }
                if (length(dependent_variable_name_from_label) > 1) {
                    # Ambiguous label; pick first but warn
                    validation_results$warnings <- c(validation_results$warnings,
                        .fmt(
                            .("Outcome label matches multiple variables after cleaning; using '{variable}'. Please verify selection."),
                            variable = all_labels[[dependent_variable_name_from_label[1]]]))
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

                        # The event level is already reported by .validateInputs().
                    } else {
                        # Warn if selected level doesn't exist
                        private$.addNotice(
                            jmvcore::NoticeType$WARNING,
                            .fmt(
                                .("Selected positive outcome level '{level}' not found in data. Available levels: {levels}"),
                                level = positive_level,
                                levels = paste(levels(outcome_var), collapse = ", ")
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
                    # Emitted directly: this site is AFTER the loop that turns
                    # validation_results into notices, so appending to
                    # validation_results$warnings here would never be shown.
                    private$.addNotice(jmvcore::NoticeType$WARNING,
                        .fmt(
                            .("Could not map some explanatory variables after cleaning: {variables}."),
                            variables = paste(missing_labels, collapse = ", ")))
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
                mydata <- jmvcore::naOmit(mydata)

                if (nrow(mydata) == 0) {
                    jmvcore::reject(
                        .("No complete observations remain for the selected outcome and explanatory variables.")
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
                    jmvcore::reject(.fmt(
                        .("After removing incomplete cases for the selected model, the outcome must retain exactly two observed levels including the selected positive level '{level}'."),
                        level = self$options$outcomeLevel
                    ))
                }

                no_variation <- vapply(
                    explanatory_variable_names,
                    function(v) length(unique(mydata[[v]])) < 2,
                    logical(1)
                )
                if (any(no_variation)) {
                    jmvcore::reject(.fmt(
                        .("The following explanatory variable(s) have no variation after complete-case filtering: {variables}."),
                        variables = paste(private$.originalNames(
                            explanatory_variable_names[no_variation],
                            all_labels), collapse = ", ")
                    ))
                }

                non_finite <- vapply(
                    explanatory_variable_names,
                    function(v) is.numeric(mydata[[v]]) && any(!is.finite(mydata[[v]])),
                    logical(1)
                )
                if (any(non_finite)) {
                    jmvcore::reject(.fmt(
                        .("The following numeric explanatory variable(s) contain infinite values: {variables}. Replace infinite values before fitting the model."),
                        variables = paste(private$.originalNames(
                            explanatory_variable_names[non_finite],
                            all_labels), collapse = ", ")
                    ))
                }

                # A numeric with exactly two distinct values is the SAME model
                # whether it is fitted as a 0/1 term or as a two-level factor --
                # identical odds ratio, interval and p-value -- so cont_cut = 0
                # buys nothing here and costs the descriptive column: a 0/1
                # diagnostic marker was summarised as "Mean (SD) 0.2 (0.4)"
                # instead of the per-level n (%) cross-tab a pathologist reads
                # off that row, in the table AND in the forest plot. Coerce once,
                # before any fit, so finalfit, logistf and rms::lrm still all see
                # the same term, and before the separation check below so a
                # 0/1 marker with an empty 2x2 cell is warned about too. (Do NOT
                # express this as cont_cut = 3 instead: that puts or_plot's
                # factorlist back out of step with its own glmmulti fit and
                # re-breaks the fit_id join for binaries.)
                for (v in explanatory_variable_names) {
                    if (is.numeric(mydata[[v]]) &&
                        length(unique(mydata[[v]])) == 2L) {
                        lab <- attr(mydata[[v]], "label", exact = TRUE)
                        mydata[[v]] <- factor(mydata[[v]])
                        if (!is.null(lab)) attr(mydata[[v]], "label") <- lab
                    }
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
                        epv_rounded <- round(epv, 2)
                        if (epv < 5) {
                            # Use STRONG_WARNING for critically low EPV
                            private$.addNotice(jmvcore::NoticeType$STRONG_WARNING,
                                .fmt(
                                    .("Low events-per-variable (EPV \u2248 {epv}). Odds ratios may be unstable; consider penalized/Firth logistic regression."),
                                    epv = epv_rounded))
                        } else if (epv < 10) {
                            extra_warnings <- c(extra_warnings,
                                .fmt(
                                    .("Borderline events-per-variable (EPV \u2248 {epv}). Interpret odds ratios with caution."),
                                    epv = epv_rounded))
                        }
                    }

                    # Simple separation check for binary predictors
                    for (v in explanatory_variable_names) {
                        if (!is.null(v) && v %in% names(mydata) && is.factor(mydata[[v]]) && nlevels(mydata[[v]]) == 2) {
                            tab <- table(mydata[[v]], mydata[[dependent_variable_name_from_label]])
                            if (any(tab == 0)) {
                                extra_warnings <- c(extra_warnings,
                                    .fmt(
                                        .("Possible separation detected for '{variable}' (zero cells in 2x2 table). Consider penalized/Firth logistic regression."),
                                        variable = all_labels[[v]]))
                            }
                        }
                    }
                }

                # Declare the coding actually used for ordinal-looking numeric
                # predictors. Passing cont_cut = 0 to finalfit stops it silently
                # re-specifying the model, but silence about the resulting linear
                # trend would just trade a hidden choice for an undeclared one: a
                # pathologist who typed Grade as 1/2/3 usually means three groups,
                # not a constant step in log odds between consecutive grades.
                #
                # The cutoff is deliberately NOT finalfit's retired `< 5`. Every
                # numeric is now fitted linearly, so a Gleason grade group (1-5),
                # a Nottingham score (3-9) or an Allred score (0-8) needs the same
                # disclosure that a 1-3 grade does; inheriting the old threshold
                # would have named T stage and stayed silent about Gleason in the
                # same model, implying Gleason had been handled some other way.
                few_level_numeric <- Filter(
                    function(v) {
                        x <- mydata[[v]]
                        u <- length(unique(x))
                        is.numeric(x) && u > 2 && u <= 10 &&
                            all(x == floor(x))
                    },
                    explanatory_variable_names)
                if (length(few_level_numeric) > 0) {
                    private$.addNotice(
                        jmvcore::NoticeType$INFO,
                        .fmt(
                            .("Entered as continuous: {variables}. Each is modelled as one odds ratio per one-unit increase, which assumes a constant step in log odds between consecutive values. To estimate a separate odds ratio for each level instead, change the variable's measure type to Nominal or Ordinal in the data setup."),
                            variables = paste(vapply(few_level_numeric, function(v)
                                sprintf("%s (%d values)",
                                        private$.originalNames(v, all_labels),
                                        length(unique(mydata[[v]]))),
                                character(1)), collapse = ", ")))
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

                # CHECKPOINT: Before running finalfit - which can be computationally intensive
                private$.checkpoint()

                fit_standard_model <- function() {
                    tryCatch(
                        # cont_cut = 0 is load-bearing, and .quietly() keeps
                        # third-party chatter out of the results pane.
                        #
                        # finalfit's default is cont_cut = 5: it runs
                        #   select(contains(explanatory)) %>% summarise_if(is.numeric, n_distinct)
                        #     %>% keep(~ .x < cont_cut) %>% mutate_at(as.factor)
                        # on its OWN copy of the data and then fits the model on the
                        # mutated frame. A numeric score with fewer than 5 distinct
                        # values (Grade 1/2/3, Gleason group, budding tier) was
                        # therefore reported here as a set of level-wise odds ratios,
                        # while .fitFirthModel() (logistf) and .prepareRmsNomogram()
                        # (rms::lrm) fitted the SAME column linearly. Ticking the
                        # Firth checkbox silently respecified the model rather than
                        # only changing the estimator, and the nomogram described a
                        # different model from the table printed above it.
                        # cont_cut = 0 disables the rewrite so every path fits what
                        # the analyst actually selected. Same fix, same reason, as
                        # R/survivalcont.b.R:2041 and R/multisurvival.b.R:6940.
                        #
                        # .quietly() suppresses the 5 message()s this call emits
                        # (3x MASS "Waiting for profiling to be done...", plus pROC's
                        # "Setting levels"/"Setting direction" from the C-statistic),
                        # which jamovi otherwise prints in Analysis Notes. It muffles
                        # only deprecation-flavoured WARNINGS, so the substantive
                        # "glm.fit: fitted probabilities numerically 0 or 1 occurred"
                        # separation warning still reaches the user.
                        .quietly(finalfit::finalfit(
                            .data = mydata,
                            dependent = formulaDependent,
                            explanatory = formulaExplanatory,
                            metrics = TRUE,
                            cont_cut = 0
                        )),
                        error = function(e) {
                            message <- .fmt(
                                .("Standard logistic regression could not be fitted: {message}. Review outcome coding, predictor variation, sparse categories, and separation; consider Firth penalized regression when appropriate."),
                                message = conditionMessage(e)
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
                            .("Firth penalized likelihood logistic regression used to reduce bias and handle potential separation."))
                    } else {
                        private$.addNotice(jmvcore::NoticeType$STRONG_WARNING,
                            .("The 'logistf' package is required for Firth penalized regression but is not installed. Falling back to standard logistic regression."))
                        
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
                        .fmt(
                            .("The odds ratio could not be estimated for: {variables}. The confidence interval is unbounded, which means the data separate the outcome perfectly (or nearly so) for that variable and the maximum-likelihood estimate does not exist. The cell is shown as 'not estimable' rather than as the arbitrary large number the fitting algorithm stopped at. Enable Firth penalized logistic regression to obtain a finite estimate, or combine sparse categories. Note that the forest plot below is drawn by finalfit from the same unpenalized fit and will still show the unbounded estimate."),
                            variables = paste(nonest$flagged, collapse = ", ")
                        )
                    )
                }


                # Main analysis execution starts here

                text2 <- paste0(
                    "<br><b>", .("Model Metrics:"), "</b> ",
                    paste(htmltools::htmlEscape(unlist(tOdds[[2]])), collapse = " "),
                    "<br>"
                )


                self$results$text2$setContent(text2)

                results1 <-  knitr::kable(tOdds[[1]],
                             row.names = FALSE,
                             align = c("l", "l", "r", "r", "r", "r"),
                             format = "html")
                self$results$text$setContent(results1)




                ## plot Data ----
                # `filteredTable` used to be built here and stored below. Nothing
                # ever read it: .plot() assigned it to a local and then drew the
                # plot from finalfit::or_plot(), which refits from `plotData`.
                # It cost ~5.8 KB of serialized state in every saved .omv.
                plotData <- list(
                    "plotData" = mydata,
                    "formulaDependent" = formulaDependent,
                    "formulaExplanatory" = formulaExplanatory,
                    "originalNames" = all_labels,
                    "originalOutcomeName" = self$options$outcome,
                    "originalExplanatoryNames" = self$options$explanatory
                )

                # The penalized forest plot is verified here, in .run(), not in
                # .plot(): the results tree is serialized after .run(), and
                # renderers re-run on every resize and on .omv reopen while
                # .resetNotices() fires only in .run() -- so a notice written from
                # a renderer is both unreliably propagated and duplicated once per
                # render.
                # ponytail: this fits the penalized model once more than strictly
                # necessary (the renderer refits it to draw); cache the grob if the
                # second fit ever shows up in a profile.
                if (isTRUE(self$options$usePenalized) &&
                    requireNamespace("logistf", quietly = TRUE)) {
                    private$.checkpoint()
                    penalizedPlotData <- private$.createPlotDataWithOriginalNames(
                        mydata,
                        all_labels,
                        formulaDependent,
                        formulaExplanatory
                    )
                    if (is.null(private$.firthOrPlot(
                            .data       = penalizedPlotData$data,
                            dependent   = penalizedPlotData$formulaDependent,
                            explanatory = penalizedPlotData$formulaExplanatory,
                            outcome_label = self$options$outcome))) {
                        private$.addNotice(jmvcore::NoticeType$WARNING,
                            "The penalized (Firth) forest plot could not be produced, so the forest plot shows unpenalized maximum-likelihood odds ratios. These will not match the penalized estimates in the table above.")
                    }
                }

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
                            warn_msg <- .fmt(
                                .("Diagnostic predictor label matches multiple variables; using '{variable}'."),
                                variable = diagnostic_predictor_original_name)
                            private$.addNotice(jmvcore::NoticeType$WARNING, warn_msg)
                            diagnostic_predictor <- diagnostic_predictor[1]
                        }

                        # Check if selected predictor is in explanatory variables
                        if (diagnostic_predictor_original_name %in% self$options$explanatory) {
                            private$.addNotice(jmvcore::NoticeType$INFO,
                                .fmt(
                                    .("Using '{variable}' (from model) for diagnostic metrics (sensitivity, specificity, likelihood ratios)."),
                                    variable = diagnostic_predictor_original_name))
                        } else {
                            private$.addNotice(jmvcore::NoticeType$INFO,
                                .fmt(
                                    .("Using '{variable}' for diagnostic metrics. Note: This variable is NOT in the logistic regression model. Diagnostic metrics are calculated independently of the odds ratio model."),
                                    variable = diagnostic_predictor_original_name))
                        }
                    }

                    # Default to first explanatory variable if not specified
                    if (is.null(diagnostic_predictor) && length(explanatory_variable_names) > 0) {
                        diagnostic_predictor <- explanatory_variable_names[1]
                        # Not self$options$explanatory[1]: entries that failed to map
                        # are dropped from explanatory_variable_names above without
                        # being dropped from self$options$explanatory, so position 1
                        # is not guaranteed to be the same variable in both.
                        diagnostic_predictor_original_name <- private$.originalNames(
                            diagnostic_predictor, all_labels)

                        if (length(explanatory_variable_names) > 1) {
                            private$.addNotice(jmvcore::NoticeType$INFO,
                                .fmt(
                                    .("Using '{variable}' (first explanatory variable) for diagnostic metrics. To use a different variable, specify it in the 'Diagnostic Predictor' box."),
                                    variable = diagnostic_predictor_original_name))
                        } else {
                            private$.addNotice(jmvcore::NoticeType$INFO,
                                .fmt(
                                    .("Using '{variable}' for diagnostic metrics (sensitivity, specificity, likelihood ratios)."),
                                    variable = diagnostic_predictor_original_name))
                        }
                    }

                    # Ensure diagnostic predictor is available
                    if (is.null(diagnostic_predictor) ||
                        !(diagnostic_predictor %in% names(diagnostic_source))) {
                        private$.addNotice(jmvcore::NoticeType$WARNING,
                            .("No diagnostic predictor is available. Diagnostic metrics were skipped; the prediction nomogram can still be generated from the regression model."))
                        self$results$diagnosticMetrics$setContent(
                            paste0("<p>", .("Diagnostic metrics were not calculated because no diagnostic predictor was available."), "</p>")
                        )
                        diagnostics_ok <- FALSE
                    }

                    if (diagnostics_ok) {
                        diagnostic_data <- diagnostic_source[, c(
                            dependent_variable_name_from_label,
                            diagnostic_predictor
                        ), drop = FALSE]
                        diagnostic_data <- jmvcore::naOmit(diagnostic_data)

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
                                .fmt(
                                    .("Diagnostic metrics for '{variable}' require paired complete observations with exactly two observed predictor levels and two observed outcome levels. The prediction nomogram is unaffected."),
                                    variable = diagnostic_predictor_original_name))
                            self$results$diagnosticMetrics$setContent(
                                paste0("<p>", .("Diagnostic metrics were not calculated because a valid paired 2\u{00D7}2 table could not be formed."), "</p>")
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

                    sens_txt <- if (is.na(lr_results$sensitivity)) .("undefined (no positive cases)") else paste0(sprintf("%.1f%%", lr_results$sensitivity * 100), ci_pct("sensitivity"))
                    spec_txt <- if (is.na(lr_results$specificity)) .("undefined (no negative cases)") else paste0(sprintf("%.1f%%", lr_results$specificity * 100), ci_pct("specificity"))
                    # Inf is a real (diverging) value, not a missing one, so it
                    # needs its own wording -- previously only is.na was caught
                    # and "Inf" was printed verbatim.
                    fmt_lr <- function(v) {
                        if (is.null(v) || length(v) == 0 || is.na(v)) .("undefined (no informative cells)")
                        else if (is.infinite(v)) .("infinite (zero false results in this cell)")
                        else sprintf("%.2f", v)
                    }
                    plr_txt  <- paste0(fmt_lr(lr_results$positive_lr), ci_num("positive_lr"))
                    nlr_txt  <- paste0(fmt_lr(lr_results$negative_lr), ci_num("negative_lr"))

                    # Build full metrics text with all features
                    metrics_text <- paste0(
                        "<br>",
                        predictor_warning,

                        "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;'>",
                        "<b>", .("Diagnostic Metrics:"), "</b><br>",
                        .fmt(.("Sensitivity: {value}"), value = sens_txt), "<br>",
                        .fmt(.("Specificity: {value}"), value = spec_txt), "<br>",
                        .fmt(.("Positive LR: {value}"), value = plr_txt), "<br>",
                        .fmt(.("Negative LR: {value}"), value = nlr_txt), "<br>",
                        "<small style='opacity: 0.75; color: inherit;'>", .("Unadjusted 2\u{00D7}2 estimates. Clopper-Pearson exact intervals for sensitivity and specificity; log method (Simel et al. 1991) for the likelihood ratios. Computed as in epiR::epi.tests() with its default settings."), "</small>",
                        "</div>",

                        statistical_warnings,
                        statistical_recommendations,

                        "<div style='background-color: rgba(33, 159, 43, 0.1); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;'>",
                        "<b> ", .("Important: Please Verify These Interpretations"), "</b><br>",
                        "<small>",
                        "<b>", .("Positive outcome level:"), "</b> '", htmltools::htmlEscape(lr_results$positive_outcome_used), "' ",
                        "<span style='opacity: 0.75; color: inherit;'>(", htmltools::htmlEscape(lr_results$outcome_determination_method), ")</span><br>",
                        "<b>", .("Positive predictor level:"), "</b> '", htmltools::htmlEscape(lr_results$positive_predictor_used), "' ",
                        "<span style='opacity: 0.75; color: inherit;'>(", htmltools::htmlEscape(lr_results$predictor_determination_method), ")</span><br><br>",

                        "<b> ", .("Contingency Table:"), "</b><br>",
                        "<table style='border-collapse: collapse; margin: 5px 0;'>",
                        "<tr><th style='border: 1px solid #ddd; padding: 5px;'></th>",
                        "<th style='border: 1px solid #ddd; padding: 5px;'>", htmltools::htmlEscape(outcome_levels[1]), " (+)</th>",
                        "<th style='border: 1px solid #ddd; padding: 5px;'>", htmltools::htmlEscape(outcome_levels[2]), " (\u2212)</th></tr>",
                        "<tr><td style='border: 1px solid #ddd; padding: 5px;'><b>", htmltools::htmlEscape(predictor_levels[1]), " (+)</b></td>",
                        "<td style='border: 1px solid #ddd; padding: 5px;'>", cont_table[1,1], "</td>",
                        "<td style='border: 1px solid #ddd; padding: 5px;'>", cont_table[1,2], "</td></tr>",
                        "<tr><td style='border: 1px solid #ddd; padding: 5px;'><b>", htmltools::htmlEscape(predictor_levels[2]), " (\u2212)</b></td>",
                        "<td style='border: 1px solid #ddd; padding: 5px;'>", cont_table[2,1], "</td>",
                        "<td style='border: 1px solid #ddd; padding: 5px;'>", cont_table[2,2], "</td></tr>",
                        "</table>",
                        .fmt(
                            .("TP: {tp}, FP: {fp}, FN: {fn}, TN: {tn}"),
                            tp = lr_results$tp, fp = lr_results$fp,
                            fn = lr_results$fn, tn = lr_results$tn), "<br>",
                        "<span style='opacity: 0.75; color: inherit;'>",
                        .fmt(
                            .("Based on {n} observations with both the outcome and this predictor recorded; the regression model above uses {nmodel} rows. The two sets need not be the same patients."),
                            n = sum(cont_table), nmodel = nrow(mydata)),
                        "</span><br><br>",

                        "<b> ", .("How to Use:"), "</b><br>",
                        "1. ", .("Check that the positive outcome level is correct for your study"), "<br>",
                        "2. ", .("If incorrect, use the 'Positive Outcome Level' dropdown to specify the correct level"), "<br>",
                        "3. ", .("These unadjusted diagnostic metrics depend on these interpretations being correct"), "<br>",
                        "4. ", .("Different languages/coding may require manual specification"),
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
                                "<p><strong>", .("Prediction nomogram not generated."), "</strong> ",
                                .("The odds-ratio model uses Firth penalized likelihood, whereas the available nomogram implementation uses ordinary maximum-likelihood logistic regression."),
                                "</p>"
                            )
                        )
                        private$.addNotice(jmvcore::NoticeType$WARNING,
                            .("The prediction nomogram was not generated because Firth penalized regression is selected. Diagnostic metrics, when available, remain unadjusted 2x2 estimates."))
                    } else {
                        # Prepare data for the prediction nomogram.
                        nom_results <- private$.prepareRmsNomogram(
                            mydata,
                            dependent_variable_name_from_label,
                            explanatory_variable_names
                        )

                        if (!is.null(nom_results$fit)) {
                            private$.createNomogram(nom_results$fit, nom_results$dd, all_labels)
                        } else {
                            # Notices are single-line, so the multi-line
                            # troubleshooting detail returned by
                            # .prepareRmsNomogram() goes to the nomogram Html
                            # output and the notice points at it.
                            private$.addNotice(jmvcore::NoticeType$WARNING,
                                .("Prediction nomogram could not be generated due to model fitting issues; see the Nomogram panel for details. The odds ratio analysis completed successfully, and the nomogram is an unvalidated visualization whose failure does not alter the fitted odds-ratio table."))
                            nomogram_detail <- ""
                            if (!is.null(nom_results$error) && nzchar(nom_results$error)) {
                                nomogram_detail <- paste0(
                                    "<pre style='white-space: pre-wrap; margin: 0; color: inherit;'>",
                                    htmltools::htmlEscape(nom_results$error),
                                    "</pre>"
                                )
                            }
                            self$results$nomogram$setContent(paste0(
                                "<div style='padding: 10px; border-left: 4px solid #f0ad4e; background-color: rgba(138, 155, 172, 0.06); color: inherit;'>",
                                "<p><strong>Prediction nomogram not generated.</strong></p>",
                                nomogram_detail,
                                "</div>"
                            ))
                        }

                        # Persist only serializable ingredients needed to rebuild
                        # the nomogram after save/reload.
                        self$results$plot_nomogram$setState(list(
                            data = mydata,
                            dependent = dependent_variable_name_from_label,
                            explanatory = explanatory_variable_names,
                            labels = all_labels
                        ))
                    }
                }
                # Educational Explanations ----
                if (self$options$showExplanations) {
                    private$.addExplanations()
                }

                # Add completion notice for successful analysis
                private$.addNotice(jmvcore::NoticeType$INFO,
                    .("Odds ratio analysis completed successfully."))

            }

        }




        ,
        # Calculates likelihood ratios, sensitivity, and specificity for binary predictors
        # Supports user-specified positive outcome levels for international data
        # Returns diagnostic metrics including sensitivity, specificity, and likelihood ratios
        # Confidence intervals for sensitivity, specificity and the likelihood
        # ratios of a 2x2 diagnostic table.
        #
        # ---------------------------------------------------------------------
        # ADAPTED FROM epiR, with thanks.
        #   Source : epiR::epi.tests() and epiR's internal zexact(), version 2.0.95
        #   Author : Mark Stevenson and Evan Sergeant, with contributors
        #            (Cord Heuer, Telmo Nunes, Jonathon Marshall, Javier Sanchez,
        #            Ron Thornton, Jeno Reiczigel, Jim Robison-Cox,
        #            Paola Sebastiani, Peter Solymos, Kazuki Yoshida, Geoff Jones,
        #            Sarah Pirikahu, Simon Firestone, Ryan Kyle, Johann Popp,
        #            Mathew Jay, Allison Cheung, Nagendra Singanallur, Aniko Szabo,
        #            Ahmad Rabiee)
        #   Licence: GPL (>= 2) -- compatible with this package's GPL-2.
        #   URL    : https://CRAN.R-project.org/package=epiR
        #
        # WHY VENDORED RATHER THAN IMPORTED. oddsratio ships to the jsurvival
        # submodule, and it is the ONLY analysis routed there that would need
        # epiR. Adding epiR to jsurvival's Imports makes every jamovi user of a
        # survival module install a full epidemiology package for one call, and
        # _updateModules.R correctly refuses to ship an undeclared dependency.
        # Reproducing the ~15 lines of arithmetic keeps the numbers identical to
        # the sibling analyses that DO call epiR, at no dependency cost.
        #
        # The methods are epiR's defaults, matching what those siblings get when
        # they call epi.tests() without a `method` argument:
        #   * sensitivity / specificity : method = "exact", i.e. the
        #     Clopper-Pearson interval in epiR's 1 - qbeta() form (zexact).
        #     NOTE this is NOT the Wilson score interval -- epi.tests()'s default
        #     is "exact"; "wilson" is one of four non-default alternatives.
        #   * likelihood ratios : the log method (Simel et al. 1991), written in
        #     epiR's algebraically identical form.
        #
        # Cell convention matches epi.tests(matrix(c(tp,fp,fn,tn), 2, byrow=TRUE)):
        #   a = tp, b = fp, c = fn, d = tn, M1 = a + c, M0 = b + d.
        # ---------------------------------------------------------------------
        .diagnosticCIs = function(tp, fp, fn, tn, conf.level = 0.95) {
            none <- c(NA_real_, NA_real_)
            out <- list(sensitivity = none, specificity = none,
                        positive_lr = none, negative_lr = none)

            res <- tryCatch({
                z <- stats::qnorm(1 - (1 - conf.level) / 2)
                alpha2 <- 0.5 * (1 - conf.level)

                # epiR zexact(): Clopper-Pearson exact interval for a / n.
                exact_ci <- function(a, n) {
                    if (!is.finite(a) || !is.finite(n) || n <= 0) return(none)
                    lb <- if (a == 0) 1 else a
                    ub <- if (a == n) n - 1 else a
                    low <- 1 - stats::qbeta(1 - alpha2, n + 1 - a, lb)
                    upp <- 1 - stats::qbeta(alpha2, n - ub, a + 1)
                    if (a == 0) low <- 0
                    if (a == n) upp <- 1
                    c(low, upp)
                }

                M1 <- tp + fn      # actual positives
                M0 <- fp + tn      # actual negatives

                se_ci <- exact_ci(tp, M1)
                sp_ci <- exact_ci(tn, M0)

                se <- if (M1 > 0) tp / M1 else NA_real_
                sp <- if (M0 > 0) tn / M0 else NA_real_

                # Log-method intervals. The two ratios become undefined at
                # DIFFERENT boundaries, so they need separate guards -- a single
                # combined guard (se and sp both strictly inside 0..1) throws away
                # intervals that are perfectly well defined. Concretely, with
                # tp/fp/fn/tn = 20/5/0/15 the sensitivity is exactly 1, yet LR+ is
                # 4.00 (95% CI 1.87-8.55): only the specificity term contributes
                # to its standard error. Rejecting that on account of se == 1 lost
                # a real interval, which an exhaustive comparison against epiR
                # over 4156 tables surfaced.
                #
                #   LR+ = se / (1 - sp)      needs se > 0 (log) and sp < 1 (denominator)
                #   LR- = (1 - se) / sp      needs se < 1 (log) and sp > 0 (denominator)
                #
                # When se == 1 and sp == 0 together, the standard error collapses
                # to zero and the interval is the degenerate [1, 1]. That is what
                # epiR reports, so it is reproduced here for consistency; such a
                # table also has empty cells, which the assumption check flags
                # separately.
                base_ok <- is.finite(se) && is.finite(sp) && M1 > 0 && M0 > 0

                plr_ci <- none
                if (base_ok && se > 0 && sp < 1) {
                    plr <- se / (1 - sp)
                    se_log_plr <- sqrt((1 - se) / (M1 * se) + sp / (M0 * (1 - sp)))
                    plr_ci <- exp(log(plr) + c(-1, 1) * z * se_log_plr)
                }

                nlr_ci <- none
                if (base_ok && se < 1 && sp > 0) {
                    nlr <- (1 - se) / sp
                    se_log_nlr <- sqrt(se / (M1 * (1 - se)) + (1 - sp) / (M0 * sp))
                    nlr_ci <- exp(log(nlr) + c(-1, 1) * z * se_log_nlr)
                }

                list(sensitivity = se_ci, specificity = sp_ci,
                     positive_lr = plr_ci, negative_lr = nlr_ci)
            }, error = function(e) NULL)

            if (is.null(res)) return(out)
            lapply(res, function(v) {
                if (length(v) != 2 || anyNA(v) || any(!is.finite(v))) none else v
            })
        },

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
                    message = .fmt(
                        .("Please select the positive outcome level. Available levels: {levels}"),
                        levels = paste(outcome_levels, collapse = ", "))
                ))
            }
            
            # Use user-specified positive outcome level
            positive_outcome_level <- user_positive_outcome
            positive_outcome_idx <- which(outcome_levels == positive_outcome_level)
            outcome_determination_method <- .("User-specified")
            
            # Determine positive predictor level
            if (!is.null(user_positive_predictor) &&
                !(user_positive_predictor %in% predictor_levels)) {
                return(list(
                    error = TRUE,
                    message = .fmt(
                        .("The selected positive predictor level '{level}' is not present among paired complete observations. Available levels: {levels}."),
                        level = user_positive_predictor,
                        levels = paste(predictor_levels, collapse = ", ")
                    )
                ))
            }

            if (!is.null(user_positive_predictor)) {
                positive_predictor_level <- user_positive_predictor
                predictor_determination_method <- .("User-specified")
            } else {
                # Fallback to automatic detection if not specified or not found
                detection_result <- private$.detectPositiveLevels(predictor_levels)
                positive_predictor_level <- detection_result$level
                predictor_determination_method <- detection_result$method
            }
            positive_predictor_idx <- which(predictor_levels == positive_predictor_level)

            # Create messaging for predictor level determination
            if (!is.null(user_positive_predictor)) {
                 predictor_level_warning <- paste0(
                    "<div style='background-color: rgba(33, 163, 188, 0.21); border-left: 4px solid #0c5460; padding: 15px; margin: 10px 0; border-radius: 4px; color: inherit;'>",
                    "<b>", .("Predictor Level Modeling:"), "</b><br>",
                    .fmt(
                        .("The level '{level}' is used as the positive/exposure category as specified."),
                        level = htmltools::htmlEscape(positive_predictor_level)),
                    "</div>"
                )
            } else {
                predictor_level_warning <- paste0(
                    "<div style='background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; padding: 15px; margin: 10px 0; border-radius: 4px; color: inherit;'>",
                    "<h4 style='margin-top: 0; color: inherit;'> ", .("Automatic Predictor Level Detection"), "</h4>",
                    "<p><strong>", .fmt(
                        .("The positive predictor level was automatically detected as: '{level}'"),
                        level = htmltools::htmlEscape(positive_predictor_level)), "</strong></p>",
                    "<p>", .fmt(
                        .("Method: {method}"),
                        method = htmltools::htmlEscape(predictor_determination_method)), "</p>",
                    "<p style='color: inherit;'><strong>", .("Important:"), "</strong> ", .fmt(
                        .("Please verify that '{level}' is the correct positive level. If this is wrong, diagnostic metrics will be inverted."),
                        level = htmltools::htmlEscape(positive_predictor_level)), "</p>",
                    "<p>", .("Use the 'Predictor Positive Level' option to set this manually."), "</p>",
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
                    "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
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
                    "<div style='background-color: rgba(33, 162, 64, 0.19); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
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
            # Computed by .diagnosticCIs() below, which reproduces
            # epiR::epi.tests() exactly so that these intervals agree with the
            # sibling diagnostic analyses (decision, decisioncalculator,
            # decisioncompare, decisioncombine, digitalvalidation) that call it.
            ci <- private$.diagnosticCIs(tp = tp, fp = fp, fn = fn, tn = tn)

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

            # Present the 2x2 positive-first, the way a diagnostic table is
            # conventionally read: test-positive on top, outcome-positive on the
            # left, so the cells ARE tp/fp/fn/tn reading left-to-right and the
            # TP/FP/FN/TN line printed beneath the table lines up with it.
            # Previously the table came out in factor-level order, so with
            # Absent/Present against Alive/Dead the true positives sat in the
            # bottom-right and the reader had to map the corners by hand.
            #
            # Safe to reorder here: tp/fp/fn/tn were already extracted by index
            # above, and .checkStatisticalAssumptions() -- which has also already
            # run -- depends only on row/column totals, the minimum expected
            # count and any(== 0), all of which are permutation-invariant.
            display_table <- cont_table[
                c(positive_predictor_idx, setdiff(seq_len(2), positive_predictor_idx)),
                c(positive_outcome_idx, setdiff(seq_len(2), positive_outcome_idx)),
                drop = FALSE]
            names(dimnames(display_table)) <- names(dimnames(cont_table))

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
                contingency_table = display_table,
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
                formula_str <- jmvcore::constructFormula(dependent, as.list(explanatory))

                # Fit logistic regression model
                private$.checkpoint()

                fit <- .quietly(rms::lrm(
                    formula = .asSurvivalFormula(formula_str),
                    data = data,
                    x = TRUE,
                    y = TRUE
                ))

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
                # jamovi shows R warnings only in the undifferentiated "Analysis
                # Notes" panel, mixed with third-party package chatter. Hand the
                # diagnostic back to the caller, which surfaces it as a notice plus
                # the nomogram Html output.
                return(list(fit = NULL, dd = NULL, error = detailed_error))
            })
        },

        # Restore the user's variable names on the nomogram's axes.
        #
        # The model is deliberately fitted on the janitor-CLEANED names: rms is
        # not merely fussy about non-syntactic names, it fails outright --
        # rms::lrm() on a frame whose columns are "New Test"/"Rater 1" dies with
        # "subscript out of bounds", so fitting on the original names would trade
        # a cosmetic axis label for no nomogram at all. Renaming the finished
        # nomogram object touches nothing the fit depends on: the returned object
        # is a list of per-predictor axis tables keyed by name, so the rename is
        # display-only and rms::plot.nomogram reads the names straight back out.
        # Verified to render for plain, spaced, and hyphenated names.
        .relabelNomogram = function(nom, all_labels) {
            if (is.null(nom) || is.null(all_labels) || !length(all_labels)) return(nom)
            mapping <- unlist(all_labels)
            nm <- names(nom)
            hit <- !is.na(nm) & nm %in% names(mapping)
            if (any(hit)) names(nom)[hit] <- unname(mapping[nm[hit]])
            nom
        },

        # Creates nomogram from fitted lrm model and generates HTML display
        .createNomogram = function(fit, dd, all_labels = NULL) {
            if (is.null(fit)) return(NULL)

            # Create nomogram
            nom <- try({
                .quietly(rms::nomogram(fit,
                              fun = stats::plogis,  # Convert from log odds to probability
                              funlabel = "Predicted Probability"
                ))
            })

            if (!inherits(nom, "try-error")) {
                nom <- private$.relabelNomogram(nom, all_labels)
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
                        .quietly(rms::nomogram(prep$fit, fun = stats::plogis,
                                      funlabel = "Predicted Probability")),
                        silent = TRUE)
                    if (inherits(nom, "try-error")) nom <- NULL
                    nom <- private$.relabelNomogram(nom, st$labels)
                }
            }

            if (is.null(nom)) {
                return(FALSE)
            }

            private$.checkpoint()

            graphics::par(mar = c(4, 4, 2, 2))
            graphics::plot(nom)
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
                jmvcore::reject(.("Data contains no (complete) rows"))
                    plotList <- image$state

                    if (is.null(plotList))
                        return(FALSE)

                    mydata <- plotList$plotData
                    formulaDependent <- plotList$formulaDependent
                    formulaExplanatory <- plotList$formulaExplanatory
                    originalNames <- plotList$originalNames

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

                        # .firthOrPlot escapes its terms with jmvcore::composeTerm
                        # and prints the variable name it is handed, so it is fed
                        # the ORIGINAL (possibly non-syntactic) names on a data
                        # frame renamed to match.
                        plotDataWithOriginalNames <- private$.createPlotDataWithOriginalNames(
                            mydata,
                            originalNames,
                            formulaDependent,
                            formulaExplanatory
                        )

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
                        # Fall through only if the penalized fit failed. The
                        # warning about the resulting mismatch is emitted from
                        # .run(): notices must never be written from a renderer
                        # (results are serialized after .run(), and renderers
                        # re-run on resize/reopen without .resetNotices()).
                    }

                    # finalfit::or_plot() both string-parses `dependent`/`explanatory`
                    # into a formula AND uses them as dplyr select() keys, so they
                    # must be the syntactic janitor-cleaned names: an original name
                    # containing a space fails to parse, and backtick-quoting it
                    # fails the column lookup. Display names come from the variable
                    # labels, which or_plot honours via ff_label -- re-attach them
                    # here because complete-case subsetting and droplevels() strip
                    # the label attribute set in .run().
                    for (nm in intersect(names(mydata), names(originalNames)))
                        labelled::var_label(mydata[[nm]]) <- originalNames[[nm]]

                    # Build the descriptive column ourselves so that cont_cut = 0
                    # applies to the PLOT as well as to the table.
                    #
                    # or_plot() has no cont_cut argument and does not forward its
                    # `...` to summary_factorlist -- it hardcodes
                    #   summary_factorlist(.data, dependent, explanatory,
                    #                      total_col = TRUE, fit_id = TRUE)
                    # at finalfit's default cont_cut = 5, and then joins that
                    # against its own glmmulti() fitted on the RAW column. For a
                    # numeric predictor with fewer than 5 distinct values the two
                    # sides disagree and the join finds nothing: the factorlist
                    # offers fit_id "Grade1"/"Grade2"/"Grade3" while the model term
                    # is plain "Grade". The rendered plot then showed three
                    # labelled rows with NO estimate and a fourth, unlabelled row
                    # carrying the only odds ratio. Precomputing the factorlist at
                    # cont_cut = 0 makes fit_id match the model term.
                    # Falls back to or_plot's own default if this fails, rather
                    # than losing the plot entirely. Mirrors R/multisurvival.b.R:4249.
                    or_factorlist <- tryCatch(
                        finalfit::summary_factorlist(
                            mydata, formulaDependent, formulaExplanatory,
                            cont_cut = 0, total_col = TRUE, fit_id = TRUE),
                        error = function(e) NULL)

                    plot <- finalfit::or_plot(
                        .data = mydata,
                        dependent = formulaDependent,
                        explanatory = formulaExplanatory,
                        factorlist = or_factorlist,
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
            html_content <- '<div style="background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;">'
            html_content <- paste0(html_content, '<h4 style="color: inherit; margin-top: 0;">Nomogram Information</h4>')
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
            <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 149, 188, 0.1); border-left: 4px solid #17a2b8; color: inherit;">
                <h4 style="margin-top: 0; color: inherit;">Understanding Odds Ratio Analysis</h4>
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
            <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 162, 64, 0.19); border-left: 4px solid #28a745; color: inherit;">
                <h4 style="margin-top: 0; color: inherit;">Understanding Odds Ratio vs Risk Ratio</h4>
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
            <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; color: inherit;">
                <h4 style="margin-top: 0; color: inherit;">Understanding Diagnostic Test Performance</h4>
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
            <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; color: inherit;">
                <h4 style="margin-top: 0; color: inherit;">Understanding Prediction and Diagnostic Outputs</h4>

                <p><strong>Prediction nomogram:</strong> The plotted nomogram assigns points to all explanatory variables in the maximum-likelihood logistic regression model and maps total points to predicted outcome probability. It is not a Fagan nomogram and does not display pre-test-to-post-test probability conversion.</p>

                <h5 style="color: inherit;">Prediction Nomogram Components:</h5>
                <ul>
                    <li><strong>Points:</strong> Contribution assigned to each predictor value</li>
                    <li><strong>Total Points:</strong> Sum of predictor contributions</li>
                    <li><strong>Predicted Probability:</strong> Model-based probability corresponding to total points</li>
                </ul>

                <p><strong>Important:</strong> The plot is a visual representation of the fitted model, not evidence of calibration, discrimination, transportability, or clinical utility. Internal and external validation are required before clinical use. It is not generated when Firth regression is selected because that would mix different estimation methods.</p>

                <hr style="margin: 15px 0; border: none; border-top: 1px solid #f5c6cb;">

                <h5 style="color: inherit; margin-top: 15px;">What is a Diagnostic Predictor?</h5>

                <p><strong>The diagnostic predictor is the single binary variable you want to evaluate as a diagnostic test.</strong></p>

                <div style="background-color: rgba(255, 202, 33, 0.23); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
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
                    <li><strong>In your regression model:</strong> Diagnostic metrics are still unadjusted and use only its paired 2\u{00D7}2 table</li>
                    <li><strong>Not in your model:</strong> Evaluates it independently using paired complete outcome/test observations</li>
                </ul>

                <div style="background-color: rgba(33, 163, 188, 0.21); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
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

            # Restore the outcome's name in the first COLUMN HEADER.
            #
            # finalfit writes that header as paste0(dependent_label_prefix, <name>)
            # -- the prefix defaults to "Dependent: " -- from the janitor-cleaned
            # name, because the "label" attribute set in .run() does not survive
            # complete-case row subsetting. .fitFirthModel() reproduces the same
            # header shape for the Firth path, and both paths route through here,
            # so this one edit covers both. Only column 1 ever carries a variable
            # name: the remaining headers are outcome LEVELS and static OR labels.
            # Match on the trailing segment rather than hardcoding the prefix, and
            # try the longest cleaned name first so "age" cannot claim "stage".
            header <- names(table_data)[1]
            if (!is.na(header) && nzchar(header)) {
                for (clean_name in names(name_mapping)[order(-nchar(names(name_mapping)))]) {
                    if (base::endsWith(header, clean_name)) {
                        names(table_data)[1] <- paste0(
                            substr(header, 1L, nchar(header) - nchar(clean_name)),
                            name_mapping[[clean_name]])
                        break
                    }
                }
            }

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
                            if (base::startsWith(trimmed_name, clean_name)) {
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
            expected_counts <- NULL
            
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
                    warnings <- c(warnings, paste0(
                        "Small expected cell counts detected (minimum = ", round(min_expected, 2), "). ",
                        "Chi-square assumptions may be violated."
                    ))
                    
                    # c(x, list(<record>)) appends one record. append(x, list(...))
                    # flattens the record into four separate elements, which made
                    # the structured branch of the consumer unreachable and printed
                    # four orphan bullets.
                    recommendations <- c(recommendations, list(list(
                        test = "Fisher's exact test",
                        reason = "More reliable for small cell counts",
                        code = "fisher.test()",
                        interpretation = "Provides exact p-values regardless of sample size"
                    )))
                }
                
                # Check for very small total sample size
                if (total_n < 20) {
                    warnings <- c(warnings, paste0(
                        "Very small sample size (n = ", total_n, "). ",
                        "Results should be interpreted with extreme caution."
                    ))
                }
                
                # Check for zero cells
                if (any(cont_table == 0)) {
                    warnings <- append(warnings, 
                        "Zero cells detected in contingency table. This may affect odds ratio calculation."
                    )
                }
            }
            
            return(list(
                assumptions_ok = assumptions_ok,
                warnings = warnings,
                recommendations = recommendations,
                expected_counts = expected_counts
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
                    method = .fmt(.("Automatic detection ({language})"), language = language)
                ))
            } else if (length(positive_matches) > 1) {
                # Multiple matches - use first priority match
                return(list(
                    level = positive_matches[1],
                    method = .fmt(.("Automatic detection - first match ({language})"), language = language)
                ))
            } else {
                # No matches - fall back to the second FACTOR LEVEL (level order,
                # which is not necessarily alphabetical).
                return(list(
                    level = levels[min(2, length(levels))],
                    method = .("Default (second factor level)")
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
        # Coefficients, profile-likelihood CIs and p-values from a logistf fit,
        # with the intercept dropped (an exponentiated intercept is not an odds
        # ratio for any predictor).
        .firthEstimates = function(fit) {
            cf <- stats::coef(fit)
            est <- data.frame(
                term  = names(cf),
                or    = exp(unname(cf)),
                lower = exp(unname(fit$ci.lower)),
                upper = exp(unname(fit$ci.upper)),
                p     = unname(fit$prob),
                stringsAsFactors = FALSE)
            est[est$term != "(Intercept)", , drop = FALSE]
        }
        ,
        # Rebuilds the variable / level / count skeleton that finalfit prints,
        # for a Firth fit. Shared by the Firth OR table and the Firth forest
        # plot so the two can never drift apart.
        #
        # `est` is a .firthEstimates() frame; logistf names its coefficients
        # paste0(variable, level), which is how a row is matched to an estimate.
        # When `dependent` is supplied, one count column per outcome level is
        # built as well (row percentages for factors, mean (SD) for numerics),
        # matching finalfit's split count columns.
        #
        # Returns list(rows = <data.frame>, counts = <named list or NULL>).
        .firthRows = function(.data, explanatory, est, dependent = NULL) {
            n_total <- nrow(.data)
            outcome <- if (is.null(dependent)) NULL else as.factor(.data[[dependent]])
            olv     <- if (is.null(outcome)) character(0) else levels(outcome)

            mkRow <- function(variable, level, n_show, hit) data.frame(
                variable = variable, level = level, n_show = n_show,
                or      = if (is.null(hit)) NA_real_ else hit$or[1],
                lower   = if (is.null(hit)) NA_real_ else hit$lower[1],
                upper   = if (is.null(hit)) NA_real_ else hit$upper[1],
                or_text = if (is.null(hit)) "-" else sprintf(
                    "%.2f (%.2f-%.2f, %s)", hit$or[1], hit$lower[1], hit$upper[1],
                    private$.fmtP(hit$p[1])),
                stringsAsFactors = FALSE)

            rows <- list()
            cnts <- list()
            for (v in explanatory) {
                col <- .data[[v]]
                if (is.null(col)) next

                if (is.factor(col) || is.character(col) || is.logical(col)) {
                    col  <- as.factor(col)
                    lvls <- levels(col)
                    for (j in seq_along(lvls)) {
                        lv   <- lvls[j]
                        inlv <- !is.na(col) & col == lv
                        n_lv <- sum(inlv)
                        # The first level is the reference: no estimate, "-".
                        hit <- if (j == 1) NULL else est[est$term == paste0(v, lv), , drop = FALSE]
                        if (!is.null(hit) && nrow(hit) == 0) next
                        pct <- if (n_total > 0) 100 * n_lv / n_total else NA_real_
                        rows[[length(rows) + 1]] <- mkRow(
                            v, lv, sprintf("%d (%.1f)", n_lv, pct), hit)
                        cnts[[length(cnts) + 1]] <- vapply(olv, function(o) {
                            n_o <- sum(inlv & !is.na(outcome) & outcome == o)
                            sprintf("%d (%.1f)", n_o,
                                    if (n_lv > 0) 100 * n_o / n_lv else NA_real_)
                        }, character(1))
                    }
                } else {
                    hit <- est[est$term == v, , drop = FALSE]
                    if (nrow(hit) == 0) next
                    num <- jmvcore::toNumeric(col)
                    rows[[length(rows) + 1]] <- mkRow(
                        v, "Mean (SD)",
                        sprintf("%.1f (%.1f)", mean(num, na.rm = TRUE),
                                stats::sd(num, na.rm = TRUE)), hit)
                    cnts[[length(cnts) + 1]] <- vapply(olv, function(o) {
                        x <- num[!is.na(outcome) & outcome == o]
                        sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), stats::sd(x, na.rm = TRUE))
                    }, character(1))
                }
            }
            if (length(rows) == 0)
                return(list(rows = mkRow("", "", "", NULL)[0, , drop = FALSE],
                            counts = NULL))

            counts <- NULL
            if (length(olv) > 0) {
                m <- do.call(rbind, cnts)
                counts <- stats::setNames(
                    lapply(seq_along(olv), function(k) m[, k]), olv)
            }
            list(rows = do.call(rbind, rows), counts = counts)
        }
        ,
        .firthOrPlot = function(.data, dependent, explanatory, outcome_label = NULL) {
            tryCatch({
                # These are restored ORIGINAL variable names, so they may contain
                # spaces or other non-syntactic characters. composeTerm
                # backtick-quotes them, which is correct here because this is a
                # formula string (never use it as a data[[ ]] key).
                fml <- .asSurvivalFormula(
                    jmvcore::constructFormula(dependent, as.list(explanatory)))
                fit <- logistf::logistf(fml, data = .data)

                est <- private$.firthEstimates(fit)
                if (nrow(est) == 0) return(NULL)

                # Variable/level/count skeleton, shared with the Firth OR table.
                rows <- private$.firthRows(.data, explanatory, est)
                if (nrow(rows$rows) == 0) return(NULL)
                df <- rows$rows

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
                    # geom_errorbarh() and its `height` argument were both
                    # deprecated in ggplot2 4.0.0; each emitted a lifecycle
                    # warning that jamovi surfaces to the user in Analysis Notes
                    # on every render of this plot.
                    ggplot2::geom_errorbar(
                        ggplot2::aes(x = or, y = y, xmin = lower, xmax = upper),
                        orientation = "y", width = 0.2, na.rm = TRUE) +
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
            f <- .asSurvivalFormula(
                jmvcore::constructFormula(dependent, as.list(explanatory)))
            
            # Fit Firth model using logistf
            # logistf doesn't directly support data frames in the same way for labels
            # so we use the clean names from mydata
            fit <- tryCatch({
                logistf::logistf(f, data = .data)
            }, error = function(e) {
                jmvcore::reject(.fmt(
                    .("Error fitting Firth model: {message}"), message = conditionMessage(e)))
            })
            
            # Estimates for the multivariable model.
            est_multi <- private$.firthEstimates(fit)

            # Univariable Firth fits, one per predictor -- the same column
            # finalfit reports. A predictor whose single-variable fit fails is
            # simply absent from est_uni and its cell then reads "-".
            est_uni <- do.call(rbind, c(
                list(est_multi[0, , drop = FALSE]),
                lapply(explanatory, function(v) {
                    tryCatch(
                        private$.firthEstimates(logistf::logistf(
                            .asSurvivalFormula(
                                jmvcore::constructFormula(dependent, list(v))),
                            data = .data)),
                        error = function(e) NULL)
                })))

            # Same variable/level/count skeleton the Firth forest plot uses.
            multi <- private$.firthRows(.data, explanatory, est_multi, dependent = dependent)
            uni   <- private$.firthRows(.data, explanatory, est_uni)
            rows  <- multi$rows

            uni_text <- rep("-", nrow(rows))
            if (nrow(rows) > 0 && nrow(uni$rows) > 0) {
                idx <- match(paste(rows$variable, rows$level, sep = "\r"),
                             paste(uni$rows$variable, uni$rows$level, sep = "\r"))
                uni_text <- ifelse(is.na(idx), "-", uni$rows$or_text[idx])
            }

            # Build the table with the same shape and headers finalfit uses:
            # variable | level | one count column per outcome level | ORs.
            # The variable name appears only on the first row of its block,
            # which is also what .restoreOriginalNamesInTable() expects.
            summary_table <- data.frame(
                ifelse(duplicated(rows$variable), "", rows$variable),
                rows$level,
                stringsAsFactors = FALSE, check.names = FALSE)
            names(summary_table) <- c(paste0("Dependent: ", dependent), "")
            for (lv in names(multi$counts))
                summary_table[[lv]] <- unname(multi$counts[[lv]])
            summary_table[["OR (univariable, Firth)"]]   <- uni_text
            summary_table[["OR (multivariable, Firth)"]] <- rows$or_text

            # Model metrics for tOdds[[2]].
            #
            # NOT stats::extractAIC(fit): logistf's extractAIC method returns
            # (likelihood-ratio statistic vs null) + 2*df, which for this model
            # is a single-digit number sitting next to finalfit's -2logL + 2p
            # AIC of several hundred. A clinician toggling the Firth checkbox
            # read that as a dramatic improvement in fit. The AIC below is on
            # the usual scale, and is labelled with the fact that it comes from
            # the penalized likelihood and so is not interchangeable with a
            # maximum-likelihood AIC.
            loglik_full <- unname(fit$loglik["full"])
            loglik_null <- unname(fit$loglik["null"])
            n_par       <- length(stats::coef(fit))
            lr_stat     <- 2 * (loglik_full - loglik_null)
            lr_df       <- unname(fit$df)
            lr_p        <- stats::pchisq(lr_stat, df = lr_df, lower.tail = FALSE)
            metrics <- list(
                paste0("Observations: ", nrow(.data)),
                paste0("Firth penalized log-likelihood: ", round(loglik_full, 2)),
                # Keep the value immediately after the "AIC" label: a parenthetical
                # formula between the two makes the metrics line hard to read and
                # makes the figure ambiguous to anything parsing it.
                paste0("Penalized AIC: ", sprintf("%.1f", -2 * loglik_full + 2 * n_par),
                       " (-2 x penalized log-likelihood + 2 x ", n_par, " parameters)."),
                paste0("This AIC is computed from the penalized (Firth) likelihood. ",
                       "Compare it only with other Firth models fitted to these same ",
                       "observations, not with the maximum-likelihood AIC of an unpenalized fit."),
                paste0("Penalized likelihood-ratio test vs null model: chi-square = ",
                       round(lr_stat, 3), " on ", lr_df, " df, ", private$.fmtP(lr_p), ".")
            )

            return(list(summary_table, metrics))
        }

        )
)
