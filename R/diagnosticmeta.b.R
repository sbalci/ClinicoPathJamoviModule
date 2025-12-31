#' @title Diagnostic Test Meta-Analysis for Pathology
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import mada
#' @import metafor
#' @importFrom stats qnorm pnorm qt pt
#' @export


diagnosticmetaClass <- R6::R6Class(
    "diagnosticmetaClass",
    inherit = diagnosticmetaBase,
    private = list(
        # Cache variables for performance optimization
        .mada_data = NULL,
        .analysis_data = NULL,
        .data_cache_valid = FALSE,
        .continuity_correction = FALSE,
        .correction_method_used = "none",
        .corrected_study_names = character(0),
        .pooled_sensitivity = NULL,
        .pooled_specificity = NULL,
        .n_studies = 0,
        .biv_model = NULL,

        # Helper for null-safe operations
        `%||%` = function(x, y) {
            if (is.null(x)) y else x
        },

        # Helper function to escape variable names with special characters
        .escapeVar = function(x) {
            if (is.null(x)) return(NULL)
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        # Helper function to get color palette for accessibility
        .getColorPalette = function() {
            palette_option <- self$options$color_palette %||% "standard"

            switch(palette_option,
                "standard" = list(
                    primary = "darkblue",
                    secondary = "darkgreen",
                    tertiary = "darkred",
                    study_points = "gray"
                ),
                "colorblind_safe" = list(
                    primary = "#0173B2",     # Blue
                    secondary = "#029E73",   # Green
                    tertiary = "#CC78BC",    # Pink
                    study_points = "#56B4E9" # Light blue
                ),
                "high_contrast" = list(
                    primary = "#000000",     # Black
                    secondary = "#FFFFFF",   # White
                    tertiary = "#808080",    # Gray
                    study_points = "#404040" # Dark gray
                ),
                "viridis" = list(
                    primary = "#440154",     # Dark purple
                    secondary = "#21908C",   # Teal
                    tertiary = "#FDE725",    # Yellow
                    study_points = "#35B779"  # Green
                ),
                "plasma" = list(
                    primary = "#0D0887",     # Dark blue
                    secondary = "#CC4678",   # Pink
                    tertiary = "#F0F921",    # Yellow
                    study_points = "#7E03A8"  # Purple
                ),
                # Default fallback
                list(
                    primary = "darkblue",
                    secondary = "darkgreen",
                    tertiary = "darkred",
                    study_points = "gray"
                )
            )
        },

        .init = function() {

            # Initialize content for all Html outputs
            private$.populateWelcome()
            private$.populateInstructions()
            private$.populateAboutPanel()
            private$.populateInterpretation()

            # Set initial visibility based on data state
            self$results$welcome$setVisible(FALSE)
            self$results$instructions$setVisible(TRUE)
            self$results$summary$setVisible(FALSE)
            self$results$about$setVisible(self$options$show_methodology)
            self$results$interpretation$setVisible(FALSE)

            # Initialize results tables with proper columns
            private$.initializeResultsTables()

        },
        
        .run = function() {

            # Invalidate cache when data changes
            private$.data_cache_valid <- FALSE
            private$.mada_data <- NULL
            private$.analysis_data <- NULL
            private$.continuity_correction <- FALSE
            private$.pooled_sensitivity <- NULL
            private$.pooled_specificity <- NULL
            private$.biv_model <- NULL

            # Check if data is ready
            if (is.null(self$data) || nrow(self$data) == 0) {
                # Show instructions when no data
                self$results$welcome$setVisible(FALSE)
                self$results$instructions$setVisible(TRUE)
                return()
            }
            
            # Get variables with safe escaping for special characters
            study_var <- private$.escapeVar(self$options$study)
            tp_var <- private$.escapeVar(self$options$true_positives)
            fp_var <- private$.escapeVar(self$options$false_positives)
            fn_var <- private$.escapeVar(self$options$false_negatives)
            tn_var <- private$.escapeVar(self$options$true_negatives)

            # Check if all required variables are provided
            all_provided <- !is.null(study_var) && !is.null(tp_var) &&
                          !is.null(fp_var) && !is.null(fn_var) && !is.null(tn_var)

            if (!all_provided) {
                # Show instructions if any variables are missing
                self$results$welcome$setVisible(FALSE)
                self$results$instructions$setVisible(TRUE)
                return()
            }

            # All variables provided - hide instructions and proceed with analysis
            self$results$welcome$setVisible(FALSE)
            self$results$instructions$setVisible(FALSE)
            
            # Extract data
            data <- self$data
            
            # Validate data completeness
            missing_data <- any(is.na(data[[tp_var]]) | is.na(data[[fp_var]]) | 
                              is.na(data[[fn_var]]) | is.na(data[[tn_var]]))
            
            if (missing_data) {
                self$results$instructions$setContent(
                    "<p><strong>Data Warning:</strong> Missing values detected in diagnostic test data.</p>
                     <p>Please ensure all true positives, false positives, false negatives, and true negatives are complete.</p>"
                )
                return()
            }
            
            # Create diagnostic test data structure
            meta_data <- data.frame(
                row_id = seq_len(nrow(data)),
                study = as.character(data[[study_var]]),
                tp = as.numeric(data[[tp_var]]),
                fp = as.numeric(data[[fp_var]]),
                fn = as.numeric(data[[fn_var]]),
                tn = as.numeric(data[[tn_var]]),
                stringsAsFactors = FALSE
            )
            
            # Store original row count for validation
            original_n <- nrow(meta_data)

            # Remove any rows with zero or negative counts
            meta_data <- meta_data[meta_data$tp >= 0 & meta_data$fp >= 0 &
                                  meta_data$fn >= 0 & meta_data$tn >= 0, , drop = FALSE]

            # Enhanced validation with user-friendly warnings
            validation_result <- private$.validateStudyData(meta_data, original_n)
            if (!validation_result) {
                return()
            }

            # Store number of studies for summary
            private$.n_studies <- nrow(meta_data)
            
            if (nrow(meta_data) < 3) {
                self$results$instructions$setContent(
                    "<p><strong>Error:</strong> Insufficient valid studies for meta-analysis.</p>
                     <p>At least 3 studies with complete diagnostic test data are required.</p>"
                )
                return()
            }

            prepared_data <- private$.prepareAnalysisData(meta_data)
            analysis_data <- prepared_data$analysis_data
            mada_data <- prepared_data$mada_data

            # mydataview <- self$results$mydataview
            # mydataview$setContent(
            #     list(
            #         debug_prepared_data = head(prepared_data),
            #         debug_analysis_data = head(analysis_data),
            #         debug_mada_data = head(mada_data)
            #         )
            # )



            # Prepare data for analysis - debug info removed for clean output

            # Perform bivariate meta-analysis when enabled
            if (isTRUE(self$options$bivariate_analysis)) {
                tryCatch({
                    private$.performBivariateMetaAnalysis(meta_data = meta_data,
                                                          analysis_data = analysis_data,
                                                          mada_data = mada_data)
                    private$.generateSummary(meta_data)
                }, error = function(e) {
                    private$.pooled_sensitivity <- NULL
                    private$.pooled_specificity <- NULL
                    # SERIALIZATION FIX: Don't insert Notice objects (causes serialization errors)
                    # Instead, use table note
                    self$results$bivariateresults$setNote("error", sprintf('Bivariate analysis error: %s', e$message))
                    private$.generateSummary(meta_data)
                })
            } else {
                private$.pooled_sensitivity <- NULL
                private$.pooled_specificity <- NULL
                self$results$bivariateresults$setNote("disabled", "Bivariate analysis disabled by user option")
                private$.generateBasicSummary(meta_data)
            }
            self$results$summary$setVisible(self$options$show_analysis_summary)

            # Perform HSROC analysis if requested
            if (isTRUE(self$options$hsroc_analysis)) {
                tryCatch({
                    private$.performHSROCAnalysis(meta_data = meta_data,
                                                  mada_data = mada_data)
                }, error = function(e) {
                    # SERIALIZATION FIX: Don't insert Notice objects (causes serialization errors)
                    # Use table note instead
                    self$results$hsrocresults$setNote("error", sprintf('HSROC analysis error: %s', e$message))
                })
            }

            # Perform heterogeneity analysis
            if (isTRUE(self$options$heterogeneity_analysis)) {
                tryCatch({
                    private$.performHeterogeneityAnalysis(meta_data = meta_data,
                                                          analysis_data = analysis_data)
                }, error = function(e) {
                    # SERIALIZATION FIX: Don't insert Notice objects (causes serialization errors)
                    # Use table note instead
                    self$results$heterogeneity$setNote("error", sprintf('Heterogeneity analysis error: %s', e$message))
                })
            }

            # Perform meta-regression if covariate specified
            if (isTRUE(self$options$meta_regression)) {
                tryCatch({
                    private$.performMetaRegression(meta_data = meta_data,
                                                   analysis_data = analysis_data)
                }, error = function(e) {
                    # SERIALIZATION FIX: Don't insert Notice objects (causes serialization errors)
                    # Use table note instead
                    self$results$metaregression$setNote("error", sprintf('Meta-regression error: %s', e$message))
                })
            }

            # Perform publication bias assessment
            if (isTRUE(self$options$publication_bias)) {
                tryCatch({
                    private$.performPublicationBiasAssessment(meta_data = meta_data,
                                                              analysis_data = analysis_data)
                }, error = function(e) {
                    # SERIALIZATION FIX: Don't insert Notice objects (causes serialization errors)
                    # Use table note instead
                    self$results$publicationbias$setNote("error", sprintf('Publication bias analysis error: %s', e$message))
                })
            }
            
            # Generate plots
            if (self$options$forest_plot) {
                private$.populateForestPlot(analysis_data)
            }

            if (self$options$sroc_plot) {
                private$.populateSROCPlot(meta_data)
            }

            if (self$options$funnel_plot && self$options$publication_bias) {
                private$.populateFunnelPlot(analysis_data)
            }

            # Generate plot explanations if requested
            if (self$options$show_plot_explanations) {
                if (self$options$forest_plot) {
                    private$.populateForestPlotExplanation()
                }
                if (self$options$sroc_plot) {
                    private$.populateSROCPlotExplanation()
                }
                if (self$options$funnel_plot && self$options$publication_bias) {
                    private$.populateFunnelPlotExplanation()
                }
            }

            # Populate individual studies table
            if (self$options$show_individual_studies) {
                private$.populateIndividualStudies(meta_data)
            }

            # Set visibility for interpretation and about based on options and data completeness
            if (!is.null(study_var) && !is.null(tp_var) && !is.null(fp_var) &&
                !is.null(fn_var) && !is.null(tn_var)) {
                self$results$interpretation$setVisible(self$options$show_interpretation)
                self$results$about$setVisible(self$options$show_methodology)
            }

            # SERIALIZATION FIX: Removed completion notice (causes serialization errors)
            # Analysis completion info is shown in summary table instead
        },
        
        .performBivariateMetaAnalysis = function(meta_data, analysis_data, mada_data) {

            # Check data availability
            if (is.null(analysis_data) || nrow(analysis_data) == 0) {
                self$results$bivariateresults$setNote("data_error", "Analysis data is missing or empty")
                return()
            }

            if (!requireNamespace("mada", quietly = TRUE)) {
                self$results$bivariateresults$setNote("package_error", "mada package is not available")
                return()
            }

            if (is.null(mada_data) || nrow(mada_data) < 3) {
                n_rows <- if (is.null(mada_data)) 0 else nrow(mada_data)
                self$results$bivariateresults$setNote("insufficient_data", paste("Insufficient data for meta-analysis. Found", n_rows, "studies, need at least 3"))
                return()
            }

            method_option <- self$options$method %||% "reml"

            # Ensure we only use methods supported by mada::reitsma
            valid_methods <- c("fixed", "ml", "reml", "mm", "vc")
            if (!(method_option %in% valid_methods)) {
                method_used <- "reml"
                # SERIALIZATION FIX: Use table note instead of inserting Notice
                self$results$bivariateresults$setNote("method_warning", sprintf(
                    "Method '%s' is not supported for bivariate diagnostic meta-analysis. Using REML instead.",
                    method_option
                ))
            } else {
                method_used <- method_option
            }

            conf_level <- (self$options$confidence_level %||% 95) / 100
            conf_level <- max(min(conf_level, 0.999), 0.5)

            biv_model <- mada::reitsma(mada_data, method = method_used)
            private$.biv_model <- biv_model
            summary_results <- summary(biv_model, level = conf_level)
            coefficients <- summary_results[["coefficients"]]

            if (is.null(coefficients) || !is.matrix(coefficients)) {
                self$results$bivariateresults$setNote("model_error", "Reitsma model failed - coefficient matrix missing")
                private$.appendInstructionMessage(
                    "<div class='alert alert-warning'><h4>⚠️ Bivariate Output Missing</h4><p>The Reitsma model did not return coefficient estimates, so pooled sensitivity and specificity are unavailable.</p></div>"
                )
                return()
            }

            coef_rows <- rownames(coefficients)
            get_row <- function(target) {
                if (is.null(coef_rows)) {
                    return(NULL)
                }
                idx <- which(coef_rows == target)
                if (length(idx) == 0) {
                    idx <- which(tolower(coef_rows) == tolower(target))
                }
                if (length(idx) == 0) {
                    return(NULL)
                }
                coefficients[idx[1], , drop = FALSE]
            }

            sens_prob_row <- get_row("sensitivity")
            fpr_prob_row <- get_row("false pos. rate")
            sens_logit_row <- get_row("tsens.(Intercept)")
            fpr_logit_row <- get_row("tfpr.(Intercept)")

            if (is.null(sens_prob_row) || is.null(fpr_prob_row) ||
                is.null(sens_logit_row) || is.null(fpr_logit_row)) {
                self$results$bivariateresults$setNote(
                    "model_error",
                    "Reitsma model returned unexpected coefficient structure"
                )
                private$.appendInstructionMessage(
                    "<div class='alert alert-warning'><h4>⚠️ Bivariate Output Missing</h4><p>The Reitsma model did not return the expected coefficient estimates, so pooled sensitivity and specificity are unavailable.</p></div>"
                )
                return()
            }

            safe_lr <- function(numer, denom) {
                if (!is.finite(numer) || !is.finite(denom) || denom <= 0) {
                    return(NA_real_)
                }
                numer / denom
            }

            z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)

            bivariate_table <- self$results$bivariateresults
            bivariate_table$deleteRows()

            ci_lower_col <- grep("ci\\.lb$", colnames(coefficients), value = TRUE)
            ci_upper_col <- grep("ci\\.ub$", colnames(coefficients), value = TRUE)
            ci_lower_col <- ci_lower_col[1]
            ci_upper_col <- ci_upper_col[1]

            pooled_sens <- sens_prob_row[1, "Estimate"]
            sens_ci <- c(NA_real_, NA_real_)
            if (!is.null(ci_lower_col) && !is.null(ci_upper_col)) {
                sens_ci <- c(sens_prob_row[1, ci_lower_col], sens_prob_row[1, ci_upper_col])
            }
            private$.pooled_sensitivity <- pooled_sens

            fpr_estimate <- fpr_prob_row[1, "Estimate"]
            pooled_spec <- if (is.finite(fpr_estimate)) 1 - fpr_estimate else NA_real_
            spec_ci <- c(NA_real_, NA_real_)
            if (!is.null(ci_lower_col) && !is.null(ci_upper_col)) {
                fpr_ci_lower <- fpr_prob_row[1, ci_lower_col]
                fpr_ci_upper <- fpr_prob_row[1, ci_upper_col]
                if (is.finite(fpr_ci_lower) && is.finite(fpr_ci_upper)) {
                    spec_ci <- c(1 - fpr_ci_upper, 1 - fpr_ci_lower)
                }
            }
            private$.pooled_specificity <- pooled_spec

            # Heterogeneity assessment: Use Q-statistic from bivariate model
            # Note: Univariate I² values ignore bivariate correlation and overstate precision
            # Proper assessment requires examining the bivariate covariance structure
            sens_i2 <- NA_real_
            spec_i2 <- NA_real_

            # SERIALIZATION FIX: Use table note instead of inserting Notice
            self$results$bivariateresults$setNote("heterogeneity_info",
                "I² values are not included in the bivariate table as univariate I² calculations ignore within-study correlation and bivariate model structure. Please refer to the Heterogeneity Assessment table for proper evaluation using Q-statistics and tau-squared."
            )

            bivariate_table$addRow(rowKey = "sensitivity", values = list(
                parameter = "Pooled Sensitivity",
                estimate = pooled_sens * 100,  # Convert to percentage
                ci_lower = sens_ci[1] * 100,   # Convert to percentage
                ci_upper = sens_ci[2] * 100,   # Convert to percentage
                i_squared = sens_i2,
                p_value = sens_logit_row[1, "Pr(>|z|)"]
            ))

            bivariate_table$addRow(rowKey = "specificity", values = list(
                parameter = "Pooled Specificity",
                estimate = pooled_spec * 100,  # Convert to percentage
                ci_lower = spec_ci[1] * 100,   # Convert to percentage
                ci_upper = spec_ci[2] * 100,   # Convert to percentage
                i_squared = spec_i2,
                p_value = fpr_logit_row[1, "Pr(>|z|)"]
            ))

            pooled_plr <- safe_lr(pooled_sens, 1 - pooled_spec)
            pooled_nlr <- safe_lr(1 - pooled_sens, pooled_spec)
            pooled_dor <- safe_lr(pooled_plr, pooled_nlr)

            lr_ci <- list(plr = c(NA_real_, NA_real_),
                          nlr = c(NA_real_, NA_real_),
                          dor = c(NA_real_, NA_real_))

            vcov_matrix <- biv_model$vcov
            if (is.matrix(vcov_matrix) && all(dim(vcov_matrix) >= 2) &&
                all(is.finite(vcov_matrix[1:2, 1:2])) &&
                is.finite(pooled_sens) && is.finite(pooled_spec)) {

                var_logit_sens <- vcov_matrix[1, 1]
                var_logit_spec <- vcov_matrix[2, 2]
                cov_sens_spec <- vcov_matrix[1, 2]

                if (var_logit_sens >= 0 && var_logit_spec >= 0) {
                    if (is.finite(pooled_plr) && pooled_plr > 0) {
                        var_log_plr <- ((1 - pooled_sens)^2 * var_logit_sens) +
                                       (pooled_spec^2 * var_logit_spec) +
                                       (2 * (1 - pooled_sens) * pooled_spec * cov_sens_spec)
                        if (is.finite(var_log_plr) && var_log_plr >= 0) {
                            se_log_plr <- sqrt(var_log_plr)
                            lr_ci$plr <- exp(log(pooled_plr) + c(-1, 1) * z_crit * se_log_plr)
                        }
                    }

                    if (is.finite(pooled_nlr) && pooled_nlr > 0) {
                        var_log_nlr <- (pooled_sens^2 * var_logit_sens) +
                                       ((1 - pooled_spec)^2 * var_logit_spec) +
                                       (2 * pooled_sens * (1 - pooled_spec) * cov_sens_spec)
                        if (is.finite(var_log_nlr) && var_log_nlr >= 0) {
                            se_log_nlr <- sqrt(var_log_nlr)
                            lr_ci$nlr <- exp(log(pooled_nlr) + c(-1, 1) * z_crit * se_log_nlr)
                        }
                    }

                    if (is.finite(pooled_dor) && pooled_dor > 0) {
                        var_log_dor <- var_logit_sens + var_logit_spec + 2 * cov_sens_spec
                        if (is.finite(var_log_dor) && var_log_dor >= 0) {
                            se_log_dor <- sqrt(var_log_dor)
                            lr_ci$dor <- exp(log(pooled_dor) + c(-1, 1) * z_crit * se_log_dor)
                        }
                    }
                }
            }

            bivariate_table$addRow(rowKey = "plr", values = list(
                parameter = "Positive Likelihood Ratio",
                estimate = pooled_plr,
                ci_lower = lr_ci$plr[1],
                ci_upper = lr_ci$plr[2],
                i_squared = NA_real_,
                p_value = NA_real_
            ))

            bivariate_table$addRow(rowKey = "nlr", values = list(
                parameter = "Negative Likelihood Ratio",
                estimate = pooled_nlr,
                ci_lower = lr_ci$nlr[1],
                ci_upper = lr_ci$nlr[2],
                i_squared = NA_real_,
                p_value = NA_real_
            ))

            bivariate_table$addRow(rowKey = "dor", values = list(
                parameter = "Diagnostic Odds Ratio",
                estimate = pooled_dor,
                ci_lower = lr_ci$dor[1],
                ci_upper = lr_ci$dor[2],
                i_squared = NA_real_,
                p_value = NA_real_
            ))

            # Analysis completed successfully - table should be populated
            bivariate_table$setNote("success", "Analysis completed successfully - table populated")
            bivariate_table$setNote("method", paste("Reitsma model estimated via", method_used))
        },
        
        .performHSROCAnalysis = function(meta_data, mada_data) {

            if (requireNamespace("mada", quietly = TRUE)) {

                hsroc_table <- self$results$hsrocresults
                hsroc_table$deleteRows()

                # Validate input data
                if (is.null(meta_data) || nrow(meta_data) == 0) {
                    self$results$hsrocresults$setNote("insufficient", "Insufficient data for HSROC analysis")
                    return()
                }

                # Check for required columns
                required_cols <- c("tp", "fp", "fn", "tn")
                if (!all(required_cols %in% names(meta_data))) {
                    self$results$hsrocresults$setNote("missing", "Missing required columns for HSROC analysis")
                    return()
                }

                if (is.null(mada_data) || nrow(mada_data) == 0) {
                    self$results$hsrocresults$setNote("invalid", "Processed data unavailable for HSROC analysis")
                    return()
                }

                # Check if we have enough data
                if (nrow(mada_data) < 3) {
                    self$results$hsrocresults$setNote("toofew", "HSROC requires at least 3 studies")
                    return()
                }

                # Check for zero cells in MADA data which cause problems
                zero_cells <- any(meta_data$tp == 0 | meta_data$fp == 0 |
                                  meta_data$fn == 0 | meta_data$tn == 0)
                if (zero_cells) {
                    self$results$hsrocresults$setNote("zerocells", "Zero cells detected - HSROC may be unreliable")
                }

                # Attempt HSROC model fitting

                # Fit HSROC model with enhanced error handling
                hsroc_model <- tryCatch({
                    result <- mada::phm(mada_data)
                    if (is.null(result)) {
                        stop("HSROC model fitting returned NULL")
                    }
                    # Model fitted successfully
                    result
                }, warning = function(w) {
                    # Try with warnings suppressed
                    tryCatch({
                        result <- suppressWarnings(mada::phm(mada_data))
                        if (is.null(result)) {
                            stop("HSROC model fitting returned NULL after warning")
                        }
                        result
                    }, error = function(e2) {
                        self$results$hsrocresults$setNote("error", paste("HSROC fitting failed:", e2$message))
                        return(NULL)
                    })
                }, error = function(e) {
                    self$results$hsrocresults$setNote("error", paste("HSROC fitting error:", e$message))
                    return(NULL)
                })

                # Validate model object
                if (is.null(hsroc_model)) {
                    self$results$hsrocresults$setNote("failed", "HSROC model fitting failed - check data quality")
                    return()
                }

                # Get summary with error handling
                hsroc_summary <- tryCatch({
                    result <- summary(hsroc_model)
                    if (is.null(result)) {
                        stop("HSROC summary is NULL")
                    }
                    result
                }, error = function(e) {
                    self$results$hsrocresults$setNote("summary_error", paste("HSROC summary error:", e$message))
                    return(NULL)
                })

                if (is.null(hsroc_summary)) {
                    return()
                }

                # Extract HSROC summary information

                # Extract HSROC parameters - coefficients are in hsroc_summary$object$coefficients
                coefficients <- NULL
                tryCatch({
                    if (!is.null(hsroc_summary$object) && "coefficients" %in% names(hsroc_summary$object)) {
                        coefficients <- hsroc_summary$object$coefficients
                    } else if (!is.null(hsroc_summary) && "coefficients" %in% names(hsroc_summary)) {
                        coefficients <- hsroc_summary$coefficients
                    } else if (!is.null(hsroc_summary) && "coef" %in% names(hsroc_summary)) {
                        coefficients <- hsroc_summary$coef
                    }
                }, error = function(e) {
                    self$results$hsrocresults$setNote("coef_access_error", paste("Cannot access coefficients:", e$message))
                })

                if (is.null(coefficients)) {
                    self$results$hsrocresults$setNote("no_coefficients", "HSROC summary contains no coefficients")
                    return()
                }

                # Validate coefficient structure
                if (length(coefficients) == 0) {
                    self$results$hsrocresults$setNote("empty_coefficients", "HSROC coefficients are empty")
                    return()
                }

                # Handle coefficients as named vector (typical for mada phm)
                if (is.vector(coefficients) && !is.null(names(coefficients))) {
                    # Define parameter labels
                    param_labels <- list(
                        "theta" = "HSROC Threshold (θ)",
                        "Lambda" = "HSROC Accuracy (Λ)",
                        "taus_sq" = "Between-Study Variance (τ²)"
                    )

                    hsroc_table <- self$results$hsrocresults
                    hsroc_table$deleteRows()

                    # Process each coefficient
                    for (param_name in names(coefficients)) {
                        display_name <- param_labels[[param_name]] %||% param_name
                        estimate <- coefficients[param_name]

                        # Get variance/SE from vcov matrix if available
                        std_error <- NA_real_
                        z_value <- NA_real_
                        p_value <- NA_real_

                        if (!is.null(hsroc_summary$object$vcov)) {
                            vcov_matrix <- hsroc_summary$object$vcov
                            vcov_names <- rownames(vcov_matrix)

                            # Find the index of the parameter in the vcov matrix
                            param_idx <- which(names(coefficients) == param_name)

                            if (length(param_idx) > 0 && param_idx <= nrow(vcov_matrix)) {
                                variance <- vcov_matrix[param_idx, param_idx]
                                if (is.finite(variance) && variance > 0) {
                                    std_error <- sqrt(variance)
                                    if (is.finite(std_error) && std_error > 0 && is.finite(estimate)) {
                                        z_value <- estimate / std_error
                                        p_value <- 2 * (1 - stats::pnorm(abs(z_value)))
                                    }
                                }
                            }
                        }

                        hsroc_table$addRow(rowKey = param_name, values = list(
                            parameter = display_name,
                            estimate = estimate,
                            std_error = std_error,
                            z_value = z_value,
                            p_value = p_value
                        ))
                    }

                    hsroc_table$setNote("success", "HSROC analysis completed successfully")
                } else {
                    self$results$hsrocresults$setNote("unsupported_format", "HSROC coefficients format not supported")
                }
            }
        },
        
        .performHeterogeneityAnalysis = function(meta_data, analysis_data) {

            if (is.null(analysis_data) || nrow(analysis_data) == 0) {
                return()
            }

            if (requireNamespace("metafor", quietly = TRUE)) {

                analysis_data$sens <- analysis_data$tp / (analysis_data$tp + analysis_data$fn)
                analysis_data$spec <- analysis_data$tn / (analysis_data$tn + analysis_data$fp)

                analysis_data$logit_sens <- stats::qlogis(analysis_data$sens)
                analysis_data$logit_spec <- stats::qlogis(analysis_data$spec)

                analysis_data$var_logit_sens <- 1 / analysis_data$tp + 1 / analysis_data$fn
                analysis_data$var_logit_spec <- 1 / analysis_data$tn + 1 / analysis_data$fp

                sens_meta <- metafor::rma(yi = logit_sens, vi = var_logit_sens,
                                          data = analysis_data, method = "REML")

                spec_meta <- metafor::rma(yi = logit_spec, vi = var_logit_spec,
                                          data = analysis_data, method = "REML")

                het_table <- self$results$heterogeneity
                het_table$deleteRows()

                het_table$addRow(rowKey = "sensitivity", values = list(
                    measure = "Sensitivity",
                    q_statistic = sens_meta$QE,
                    df = sens_meta$k - 1,
                    p_value = sens_meta$QEp,
                    i_squared = max(0, (sens_meta$QE - (sens_meta$k - 1)) / sens_meta$QE * 100),
                    tau_squared = sens_meta$tau2
                ))

                het_table$addRow(rowKey = "specificity", values = list(
                    measure = "Specificity",
                    q_statistic = spec_meta$QE,
                    df = spec_meta$k - 1,
                    p_value = spec_meta$QEp,
                    i_squared = max(0, (spec_meta$QE - (spec_meta$k - 1)) / spec_meta$QE * 100),
                    tau_squared = spec_meta$tau2
                ))
            }
        },
        
        .performMetaRegression = function(meta_data, analysis_data) {

            if (is.null(analysis_data) || nrow(analysis_data) == 0) {
                return()
            }

            covariate_var <- self$options$covariate
            if (is.null(covariate_var)) {
                # Add message when meta-regression is enabled but no covariate is selected
                if (isTRUE(self$options$meta_regression)) {
                    private$.appendInstructionMessage(
                        "<div class='alert alert-info'><h4>ℹ️ Meta-Regression Requires a Covariate</h4><p>To perform meta-regression analysis, please select a covariate variable (e.g., study year, population type, method) that may explain heterogeneity between studies. The covariate should be a study-level characteristic that varies across included studies.</p></div>"
                    )
                }
                return()
            }

            covariate_values <- self$data[[covariate_var]]
            if (is.null(covariate_values)) return()

            if (!"row_id" %in% names(meta_data)) {
                private$.appendInstructionMessage(
                    "<div class='alert alert-warning'><h4>⚠️ Meta-Regression Skipped</h4><p>Row identifiers were not preserved during preprocessing, so the covariate could not be aligned with the filtered studies.</p></div>"
                )
                return()
            }

            analysis_data$covariate <- covariate_values[meta_data$row_id]
            analysis_data <- analysis_data[!is.na(analysis_data$covariate), , drop = FALSE]

            if (nrow(analysis_data) < 3) {
                # SERIALIZATION FIX: Use table note instead of inserting Notice
                self$results$metaregression$setNote("insufficient_data",
                    'Meta-regression not run: fewer than three studies remain after removing missing covariate values.')
                return()
            }

            # Stability warning for small sample sizes
            if (nrow(analysis_data) < 10) {
                # SERIALIZATION FIX: Use table note instead of inserting Notice
                self$results$metaregression$setNote("small_sample_warning", sprintf(
                    'Meta-regression with only %d studies may produce unstable estimates. Results should be interpreted with extreme caution. Confidence intervals may be unreliable and parameter estimates may be biased. Consider reporting as exploratory analysis only.',
                    nrow(analysis_data)
                ))
            }

            if (requireNamespace("metafor", quietly = TRUE)) {

                analysis_data$sens <- analysis_data$tp / (analysis_data$tp + analysis_data$fn)
                analysis_data$spec <- analysis_data$tn / (analysis_data$tn + analysis_data$fp)

                analysis_data$logit_sens <- stats::qlogis(analysis_data$sens)
                analysis_data$logit_spec <- stats::qlogis(analysis_data$spec)

                analysis_data$var_logit_sens <- 1 / analysis_data$tp + 1 / analysis_data$fn
                analysis_data$var_logit_spec <- 1 / analysis_data$tn + 1 / analysis_data$fp

                metareg_table <- self$results$metaregression
                metareg_table$deleteRows()

                sens_metareg <- tryCatch({
                    metafor::rma(yi = logit_sens, vi = var_logit_sens,
                                 mods = ~ covariate, data = analysis_data, method = "REML")
                }, error = function(e) {
                    private$.appendInstructionMessage(
                        paste0("<div class='alert alert-warning'><h4>⚠️ Sensitivity Meta-Regression Failed</h4><p>",
                               e$message, "</p></div>")
                    )
                    return(NULL)
                })

                spec_metareg <- tryCatch({
                    metafor::rma(yi = logit_spec, vi = var_logit_spec,
                                 mods = ~ covariate, data = analysis_data, method = "REML")
                }, error = function(e) {
                    private$.appendInstructionMessage(
                        paste0("<div class='alert alert-warning'><h4>⚠️ Specificity Meta-Regression Failed</h4><p>",
                               e$message, "</p></div>")
                    )
                    return(NULL)
                })

                if (!is.null(sens_metareg)) {
                    metareg_table$addRow(rowKey = "sens_intercept", values = list(
                        measure = "Sensitivity",
                        parameter = "Intercept",
                        estimate = sens_metareg$beta[1],
                        std_error = sens_metareg$se[1],
                        z_value = sens_metareg$zval[1],
                        p_value = sens_metareg$pval[1]
                    ))

                    if (length(sens_metareg$beta) >= 2) {
                        metareg_table$addRow(rowKey = "sens_covariate", values = list(
                            measure = "Sensitivity",
                            parameter = covariate_var,
                            estimate = sens_metareg$beta[2],
                            std_error = sens_metareg$se[2],
                            z_value = sens_metareg$zval[2],
                            p_value = sens_metareg$pval[2]
                        ))
                    }
                }

                if (!is.null(spec_metareg)) {
                    metareg_table$addRow(rowKey = "spec_intercept", values = list(
                        measure = "Specificity",
                        parameter = "Intercept",
                        estimate = spec_metareg$beta[1],
                        std_error = spec_metareg$se[1],
                        z_value = spec_metareg$zval[1],
                        p_value = spec_metareg$pval[1]
                    ))

                    if (length(spec_metareg$beta) >= 2) {
                        metareg_table$addRow(rowKey = "spec_covariate", values = list(
                            measure = "Specificity",
                            parameter = covariate_var,
                            estimate = spec_metareg$beta[2],
                            std_error = spec_metareg$se[2],
                            z_value = spec_metareg$zval[2],
                            p_value = spec_metareg$pval[2]
                        ))
                    }
                }
            }
        },
        
        .performPublicationBiasAssessment = function(meta_data, analysis_data) {

            if (is.null(analysis_data) || nrow(analysis_data) == 0) {
                return()
            }

            if (requireNamespace("metafor", quietly = TRUE)) {

                if (nrow(analysis_data) < 10) {
                    private$.appendInstructionMessage(
                        "<div class='alert alert-info'><h4>ℹ️ Publication Bias Caution</h4><p>Deeks' test is unreliable with fewer than 10 studies; interpret asymmetry results cautiously.</p></div>"
                    )
                }

                # CRITICAL FIX: Use effective sample size (ESS) for Deeks' test
                # Deeks' method requires ESS = 4/(1/TP + 1/FN + 1/FP + 1/TN) - the harmonic mean
                # Using arithmetic total (TP+FP+FN+TN) overweights large but imbalanced studies
                analysis_data$ess <- 4 / (1 / analysis_data$tp + 1 / analysis_data$fn +
                                          1 / analysis_data$fp + 1 / analysis_data$tn)

                analysis_data$log_dor <- log((analysis_data$tp * analysis_data$tn) /
                                              (analysis_data$fp * analysis_data$fn))
                analysis_data$se_log_dor <- sqrt(1 / analysis_data$tp + 1 / analysis_data$fp +
                                                  1 / analysis_data$fn + 1 / analysis_data$tn)

                # Use 1/sqrt(ESS) as the predictor (correct Deeks' specification)
                deeks_test <- metafor::rma(yi = log_dor, vi = se_log_dor^2,
                                           mods = ~ I(1 / sqrt(ess)), data = analysis_data, method = "FE")

                bias_table <- self$results$publicationbias
                bias_table$deleteRows()

                bias_table$addRow(rowKey = "deeks_test", values = list(
                    test = "Deeks' Funnel Plot Asymmetry Test",
                    statistic = deeks_test$zval[2],
                    p_value = deeks_test$pval[2],
                    interpretation = ifelse(deeks_test$pval[2] < 0.05,
                                          "Significant asymmetry detected",
                                          "No significant asymmetry")
                ))
            }
        },
        
        .populateForestPlot = function(meta_data) {

            image <- self$results$forestplot
            # SERIALIZATION FIX: Ensure it's a plain data frame
            if (!is.null(meta_data) && is.data.frame(meta_data)) {
                meta_data <- as.data.frame(meta_data, stringsAsFactors = FALSE)
                # Remove any attributes that might contain functions
                attributes(meta_data) <- attributes(meta_data)[c("names", "row.names", "class")]
            }
            image$setState(meta_data)
        },
        
        .forestplot = function(image, ggtheme, theme, ...) {

            meta_data <- image$state

            # Validate meta_data
            if (is.null(meta_data) || !is.data.frame(meta_data) || nrow(meta_data) == 0) {
                return(FALSE)
            }

            if (requireNamespace("ggplot2", quietly = TRUE)) {

                # Ensure meta_data is a proper data frame
                meta_data <- as.data.frame(meta_data)
                
                # Calculate sens and spec with CIs
                meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
                meta_data$spec <- meta_data$tn / (meta_data$tn + meta_data$fp)
                
                conf_level <- self$options$confidence_level
                if (is.null(conf_level) || length(conf_level) == 0 || !is.finite(conf_level)) {
                    conf_level <- 95
                }
                conf_level <- min(max(conf_level, 50), 99) / 100
                alpha <- 1 - conf_level
                z_crit <- stats::qnorm(1 - alpha / 2)

                # Wilson score interval (proper CI for proportions, not Wald)
                # More accurate for extreme proportions near 0 or 1
                wilson_ci <- function(x, n, z) {
                    if (n == 0 || !is.finite(x) || !is.finite(n)) {
                        return(c(0, 1))
                    }
                    p <- x / n
                    denominator <- 1 + z^2 / n
                    center <- (p + z^2 / (2 * n)) / denominator
                    margin <- z * sqrt((p * (1 - p) / n + z^2 / (4 * n^2))) / denominator

                    lower <- pmax(0, center - margin)
                    upper <- pmin(1, center + margin)
                    c(lower, upper)
                }

                # Calculate Wilson CIs for sensitivity
                meta_data$sens_ci_lower <- NA_real_
                meta_data$sens_ci_upper <- NA_real_
                for (i in seq_len(nrow(meta_data))) {
                    ci <- wilson_ci(meta_data$tp[i], meta_data$tp[i] + meta_data$fn[i], z_crit)
                    meta_data$sens_ci_lower[i] <- ci[1]
                    meta_data$sens_ci_upper[i] <- ci[2]
                }

                # Calculate Wilson CIs for specificity
                meta_data$spec_ci_lower <- NA_real_
                meta_data$spec_ci_upper <- NA_real_
                for (i in seq_len(nrow(meta_data))) {
                    ci <- wilson_ci(meta_data$tn[i], meta_data$tn[i] + meta_data$fp[i], z_crit)
                    meta_data$spec_ci_lower[i] <- ci[1]
                    meta_data$spec_ci_upper[i] <- ci[2]
                }

                # Reshape data to long format for faceting
                sens_data <- meta_data[, c("study", "sens", "sens_ci_lower", "sens_ci_upper")]
                sens_data$metric <- "Sensitivity"
                colnames(sens_data) <- c("study", "estimate", "ci_lower", "ci_upper", "metric")

                spec_data <- meta_data[, c("study", "spec", "spec_ci_lower", "spec_ci_upper")]
                spec_data$metric <- "Specificity"
                colnames(spec_data) <- c("study", "estimate", "ci_lower", "ci_upper", "metric")

                plot_data <- rbind(sens_data, spec_data)

                # Get color palette for accessibility
                colors <- private$.getColorPalette()

                # Add pooled estimates as diamond (standard meta-analysis convention)
                pooled_data <- NULL
                if (!is.null(private$.pooled_sensitivity) && !is.null(private$.pooled_specificity)) {
                    # Get bivariate model summary for pooled CIs
                    pooled_sens_ci <- c(NA_real_, NA_real_)
                    pooled_spec_ci <- c(NA_real_, NA_real_)

                    if (!is.null(private$.biv_model)) {
                        tryCatch({
                            summary_results <- summary(private$.biv_model)
                            coefficients <- summary_results$coefficients

                            # Helper to get CI from coefficients
                            get_ci <- function(param_name) {
                                if (is.null(rownames(coefficients))) return(c(NA_real_, NA_real_))
                                idx <- which(rownames(coefficients) == param_name)
                                if (length(idx) == 0) return(c(NA_real_, NA_real_))
                                estimate <- coefficients[idx[1], 1]
                                se <- coefficients[idx[1], 2]
                                z <- stats::qnorm(1 - (1 - conf_level) / 2)
                                c(estimate - z * se, estimate + z * se)
                            }

                            # Get CIs for sensitivity and specificity
                            sens_ci_logit <- get_ci("sensitivity")
                            if (!any(is.na(sens_ci_logit))) {
                                pooled_sens_ci <- stats::plogis(sens_ci_logit)
                            }

                            fpr_ci_logit <- get_ci("false pos. rate")
                            if (!any(is.na(fpr_ci_logit))) {
                                pooled_spec_ci <- 1 - stats::plogis(fpr_ci_logit)
                            }
                        }, error = function(e) {
                            # Silently use NA CIs if extraction fails
                        })
                    }

                    # Create pooled estimate rows
                    pooled_sens_row <- data.frame(
                        study = "POOLED ESTIMATE",
                        estimate = private$.pooled_sensitivity,
                        ci_lower = pooled_sens_ci[1],
                        ci_upper = pooled_sens_ci[2],
                        metric = "Sensitivity",
                        stringsAsFactors = FALSE
                    )

                    pooled_spec_row <- data.frame(
                        study = "POOLED ESTIMATE",
                        estimate = private$.pooled_specificity,
                        ci_lower = pooled_spec_ci[1],
                        ci_upper = pooled_spec_ci[2],
                        metric = "Specificity",
                        stringsAsFactors = FALSE
                    )

                    pooled_data <- rbind(pooled_sens_row, pooled_spec_row)
                }

                # Create forest plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = estimate, y = reorder(study, estimate))) +
                    ggplot2::geom_point(size = 3, color = colors$primary) +
                    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
                                          height = 0.2, color = colors$primary) +
                    ggplot2::facet_wrap(~ metric) +
                    ggplot2::labs(
                        title = "Forest Plot: Sensitivity and Specificity by Study",
                        x = "Estimate",
                        y = "Study"
                    ) +
                    ggplot2::xlim(0, 1) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        axis.text = ggplot2::element_text(size = 10),
                        panel.spacing = ggplot2::unit(2, "lines")
                    )

                # Add pooled estimate diamond (standard meta-analysis convention)
                if (!is.null(pooled_data)) {
                    # Create diamond shape data for each metric
                    diamond_data <- do.call(rbind, lapply(1:nrow(pooled_data), function(i) {
                        row <- pooled_data[i, ]
                        y_pos <- "POOLED ESTIMATE"
                        y_offset <- 0.3  # Half-height of diamond

                        data.frame(
                            x = c(row$ci_lower, row$estimate, row$ci_upper, row$estimate),
                            y = c(y_pos, y_pos, y_pos, y_pos),
                            y_numeric = c(0, y_offset, 0, -y_offset),  # For proper diamond shape
                            metric = rep(row$metric, 4),
                            group = rep(i, 4),
                            stringsAsFactors = FALSE
                        )
                    }))

                    p <- p +
                        ggplot2::geom_polygon(
                            data = diamond_data,
                            ggplot2::aes(x = x, y = y, group = interaction(metric, group)),
                            fill = colors$secondary,
                            alpha = 0.6,
                            inherit.aes = FALSE
                        ) +
                        ggplot2::geom_point(
                            data = pooled_data,
                            ggplot2::aes(x = estimate, y = "POOLED ESTIMATE"),
                            size = 4,
                            color = colors$secondary,
                            shape = 18,  # Diamond shape
                            inherit.aes = FALSE
                        )
                }
                
                print(p)
                TRUE
            } else {
                FALSE
            }
        },
        
        .populateSROCPlot = function(meta_data) {

            image <- self$results$srocplot

            # Check if bivariate model is available
            if (is.null(private$.biv_model)) {
                # Set empty state to avoid errors
                image$setState(NULL)
                return()
            }

            # CRITICAL FIX: Extract only serializable data from model
            # Do NOT store the model object itself (contains non-serializable functions)
            biv_model <- private$.biv_model
            summary_results <- summary(biv_model)
            coefficients <- summary_results$coefficients

            # Helper to safely get coefficient by name
            get_coef <- function(target) {
                if (is.null(rownames(coefficients))) return(NULL)
                idx <- which(rownames(coefficients) == target)
                if (length(idx) == 0) idx <- which(tolower(rownames(coefficients)) == tolower(target))
                if (length(idx) > 0) coefficients[idx[1], 1] else NULL
            }

            sum_sens <- get_coef("sensitivity")
            sum_fpr <- get_coef("false pos. rate")

            # Fallback to intercept transformation if named rows missing
            if (is.null(sum_sens)) {
                tsens <- get_coef("tsens.(Intercept)")
                if (!is.null(tsens)) sum_sens <- stats::plogis(tsens)
            }

            if (is.null(sum_fpr)) {
                tfpr <- get_coef("tfpr.(Intercept)")
                if (!is.null(tfpr)) sum_fpr <- stats::plogis(tfpr)
            }

            # Store only serializable data (no model object!)
            # SERIALIZATION FIX: Ensure meta_data is a plain data frame
            if (!is.null(meta_data) && is.data.frame(meta_data)) {
                meta_data <- as.data.frame(meta_data, stringsAsFactors = FALSE)
                # Remove any attributes that might contain functions
                attributes(meta_data) <- attributes(meta_data)[c("names", "row.names", "class")]
            }

            plot_state <- list(
                data = meta_data,
                pooled_sens = as.numeric(sum_sens),
                pooled_fpr = as.numeric(sum_fpr)
            )

            image$setState(plot_state)
        },
        
        .srocplot = function(image, ggtheme, theme, ...) {

            state <- image$state
            if (is.null(state)) {
                return(FALSE)
            }

            # Extract pre-computed values from state (no model object)
            meta_data <- state$data
            sum_sens <- state$pooled_sens
            sum_fpr <- state$pooled_fpr

            # Validate data
            if (is.null(meta_data) || !is.data.frame(meta_data) || nrow(meta_data) == 0) {
                return(FALSE)
            }

            # Check if we have summary point values
            has_summary <- !is.null(sum_sens) && !is.null(sum_fpr)

            if (requireNamespace("ggplot2", quietly = TRUE)) {

                # Individual study points
                meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
                meta_data$spec <- meta_data$tn / (meta_data$tn + meta_data$fp)
                meta_data$fpr <- 1 - meta_data$spec
                meta_data$n <- meta_data$tp + meta_data$fn + meta_data$fp + meta_data$tn

                # Get color palette for accessibility
                colors <- private$.getColorPalette()

                # Create a simple SROC plot without confidence regions (since mada summary may not have them)
                p <- ggplot2::ggplot(meta_data, ggplot2::aes(x = fpr, y = sens)) +
                    ggplot2::geom_point(ggplot2::aes(size = n), color = colors$study_points, alpha = 0.7) + # Study points
                    ggplot2::geom_point(data = data.frame(fpr = sum_fpr, sens = sum_sens),
                                      color = colors$primary, size = 5, shape = 17) + # Summary point
                    ggplot2::scale_x_continuous(limits = c(0, 1), name = "False Positive Rate (1 - Specificity)") +
                    ggplot2::scale_y_continuous(limits = c(0, 1), name = "Sensitivity") +
                    ggplot2::labs(
                        title = "Summary ROC Plot",
                        subtitle = "Individual studies (circles) and pooled estimate (triangle)",
                        size = "Sample Size"
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        axis.text = ggplot2::element_text(size = 10)
                    )

                print(p)
                TRUE
            } else {
                FALSE
            }
        },
        
        .populateFunnelPlot = function(meta_data) {

            image <- self$results$funnelplot
            # SERIALIZATION FIX: Ensure it's a plain data frame
            if (!is.null(meta_data) && is.data.frame(meta_data)) {
                meta_data <- as.data.frame(meta_data, stringsAsFactors = FALSE)
                # Remove any attributes that might contain functions
                attributes(meta_data) <- attributes(meta_data)[c("names", "row.names", "class")]
            }
            image$setState(meta_data)
        },
        
        .funnelplot = function(image, ggtheme, theme, ...) {

            meta_data <- image$state

            # Validate meta_data
            if (is.null(meta_data) || !is.data.frame(meta_data) || nrow(meta_data) == 0) {
                return(FALSE)
            }

            if (requireNamespace("ggplot2", quietly = TRUE)) {

                # Ensure meta_data is a proper data frame
                meta_data <- as.data.frame(meta_data)

                # Calculate diagnostic odds ratio and standard error
                meta_data$log_dor <- log((meta_data$tp * meta_data$tn) / (meta_data$fp * meta_data$fn))
                meta_data$se_log_dor <- sqrt(1/meta_data$tp + 1/meta_data$fp + 1/meta_data$fn + 1/meta_data$tn)

                # Get color palette for accessibility
                colors <- private$.getColorPalette()

                # Create funnel plot
                p <- ggplot2::ggplot(meta_data, ggplot2::aes(x = log_dor, y = 1/se_log_dor)) +
                    ggplot2::geom_point(size = 3, alpha = 0.7, color = colors$secondary) +
                    ggplot2::labs(
                        title = "Funnel Plot: Publication Bias Assessment",
                        x = "Log Diagnostic Odds Ratio",
                        y = "Precision (1/SE)"
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        axis.text = ggplot2::element_text(size = 10)
                    )
                
                print(p)
                TRUE
            } else {
                FALSE
            }
        },
        
        .populateWelcome = function() {
            welcome_html <- paste0(
                "<div class='jmv-welcome' style='padding: 20px; background: #f8f9fa; border-radius: 8px; margin: 10px 0;'>",
                "<h3 style='color: #2c3e50; margin-top: 0;'>Diagnostic Test Meta-Analysis</h3>",
                "<p style='margin-bottom: 15px;'>Welcome to the comprehensive meta-analysis tool for diagnostic test accuracy studies.</p>",
                "<div style='background: white; padding: 15px; border-radius: 5px; border-left: 4px solid #3498db;'>",
                "<h4 style='margin-top: 0; color: #3498db;'>Getting Started</h4>",
                "<ol style='margin-bottom: 0;'>",
                "<li><strong>Select Study Identifier:</strong> Choose the variable containing unique study names</li>",
                "<li><strong>Define 2x2 Table:</strong> Select variables for TP, FP, FN, TN counts</li>",
                "<li><strong>Choose Analysis Options:</strong> Enable bivariate analysis, HSROC, or publication bias assessment</li>",
                "<li><strong>Configure Plots:</strong> Select forest plots, SROC curves, or funnel plots as needed</li>",
                "</ol>",
                "</div>",
                "<div style='background: #e8f5e8; padding: 10px; border-radius: 5px; margin-top: 10px;'>",
                "<strong>Quick Tip:</strong> Start with bivariate analysis for pooled sensitivity and specificity estimates",
                "</div>",
                "</div>"
            )
            self$results$welcome$setContent(welcome_html)
        },

        .populateInstructions = function() {
            
            html <- "
            <h2>Diagnostic Test Meta-Analysis for Pathology</h2>
            
            <h3>Purpose</h3>
            <p>This module performs comprehensive meta-analysis of diagnostic test accuracy studies, specifically designed for pathology research including AI algorithm validation and biomarker diagnostic accuracy synthesis.</p>
            
            <h3>Required Data Structure</h3>
            <p><strong>Essential Variables (Required):</strong></p>
            <ul>
                <li><strong>Study identifier:</strong> Unique name or ID for each study (e.g., 'Smith_2020', 'Study_1')</li>
                <li><strong>True positives (TP):</strong> Number correctly identified as positive</li>
                <li><strong>False positives (FP):</strong> Number incorrectly identified as positive</li>
                <li><strong>False negatives (FN):</strong> Number incorrectly identified as negative</li>
                <li><strong>True negatives (TN):</strong> Number correctly identified as negative</li>
            </ul>
            
            <p><strong>Optional Variables for Meta-Regression:</strong></p>
            <ul>
                <li><strong>Patient population:</strong> Disease stage, demographics (e.g., 'early_stage', 'advanced', 'mixed')</li>
                <li><strong>Technical method:</strong> Staining protocol (e.g., 'automated', 'manual')</li>
                <li><strong>Geographic region:</strong> Study location for population analysis</li>
                <li><strong>Publication year:</strong> For temporal trend investigation</li>
            </ul>
            
            <h3>Data Preparation Checklist</h3>
            <div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 10px 0;'>
                <p><strong>Before running analysis, verify:</strong></p>
                <ul>
                    <li>✅ No missing values in TP, FP, FN, TN columns</li>
                    <li>✅ All values are non-negative integers</li>
                    <li>✅ At least 2 studies with complete data</li>
                    <li>✅ Study identifiers are unique</li>
                    <li>✅ Sample sizes are realistic (TP+FP+FN+TN = total cases per study)</li>
                </ul>
            </div>
            
            <h3>Example Data Format</h3>
            <table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>
                <tr style='background-color: #f1f1f1;'>
                    <th style='border: 1px solid #ddd; padding: 8px;'>study_name</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>true_positives</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>false_positives</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>false_negatives</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>true_negatives</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>population</th>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Smith_2020</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>47</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>101</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>9</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>738</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>mixed</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Johnson_2021</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>126</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>272</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>51</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>1543</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>early_stage</td>
                </tr>
            </table>
            
            <h3>Analysis Methods</h3>
            <ul>
                <li><strong>Bivariate Random-Effects Model (Recommended):</strong> Jointly analyzes sensitivity and specificity accounting for correlation</li>
                <li><strong>HSROC Analysis:</strong> Hierarchical summary ROC curve modeling</li>
                <li><strong>Meta-Regression:</strong> Investigates sources of heterogeneity using study-level covariates</li>
                <li><strong>Publication Bias Assessment:</strong> Deeks' funnel plot asymmetry test</li>
            </ul>

            <h3>Statistical Method Selection Guide</h3>
            <div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #28a745; margin: 10px 0;'>
                <p><strong>Choose the appropriate estimation method for your meta-analysis:</strong></p>
                <ul>
                    <li><strong>REML (Recommended):</strong> Default choice for most diagnostic meta-analyses. Most robust for random effects modeling with good performance across different scenarios.</li>
                    <li><strong>Maximum Likelihood:</strong> Alternative estimation approach when maximum likelihood estimation is specifically preferred or required by study protocol.</li>
                    <li><strong>Fixed Effects:</strong> Use when between-study heterogeneity is minimal or you want to assume all studies estimate the same underlying effect size.</li>
                    <li><strong>Method of Moments:</strong> Classical moment-based estimation method, useful for comparison with older meta-analyses or when computational resources are limited.</li>
                    <li><strong>Variance Components:</strong> Specialized approach for variance component estimation, typically used in advanced methodological research.</li>
                    <li><strong>DerSimonian-Laird:</strong> Popular classical method familiar to many researchers (automatically optimized to use REML for better performance).</li>
                </ul>
                <p><strong>💡 Recommendation:</strong> Start with REML unless you have specific methodological requirements. It provides the best balance of statistical properties and computational stability for diagnostic test meta-analysis.</p>
            </div>
            
            <h3>Clinical Applications</h3>
            <ul>
                <li>IHC marker validation across multiple pathology studies</li>
                <li>AI algorithm performance meta-analysis for clinical implementation</li>
                <li>Biomarker diagnostic accuracy synthesis for guideline development</li>
                <li>Cross-population comparison of diagnostic test performance</li>
                <li>Assessment of test performance heterogeneity and variation sources</li>
            </ul>
            "
            
            self$results$instructions$setContent(html)
        },
        
        .initializeResultsTables = function() {
            # Tables are now predefined in .r.yaml file
            # No need to add columns dynamically
        },
        
        .populateIndividualStudies = function(meta_data) {

            table <- self$results$individualstudies

            table$deleteRows()

            # Calculate sensitivity and specificity for each study
            meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
            meta_data$spec <- meta_data$tn / (meta_data$tn + meta_data$fp)
            meta_data$sample_size <- meta_data$tp + meta_data$fp + meta_data$fn + meta_data$tn

            for (i in 1:nrow(meta_data)) {
                table$addRow(rowKey = i, values = list(
                    study = as.character(meta_data$study[i]),
                    sensitivity = meta_data$sens[i],
                    specificity = meta_data$spec[i],
                    tp = meta_data$tp[i],
                    fp = meta_data$fp[i],
                    fn = meta_data$fn[i],
                    tn = meta_data$tn[i],
                    sample_size = meta_data$sample_size[i]
                ))
            }
        },

        .populateInterpretation = function() {
            
            html <- "
            <h2>Clinical Interpretation Guidelines</h2>
            
            <h3>📊 Primary Results Interpretation</h3>
            
            <h4>Pooled Sensitivity and Specificity</h4>
            <ul>
                <li><strong>Pooled Sensitivity:</strong> Proportion of diseased cases correctly identified
                    <ul>
                        <li>High sensitivity (>90%): Excellent for screening - few diseased cases missed</li>
                        <li>Moderate sensitivity (80-90%): Good for screening with acceptable miss rate</li>
                        <li>Low sensitivity (<80%): Limited screening utility - many cases missed</li>
                    </ul>
                </li>
                <li><strong>Pooled Specificity:</strong> Proportion of non-diseased cases correctly identified
                    <ul>
                        <li>High specificity (>90%): Excellent for confirmation - few false alarms</li>
                        <li>Moderate specificity (80-90%): Good confirmatory value with some false positives</li>
                        <li>Low specificity (<80%): Limited confirmatory utility - many false alarms</li>
                    </ul>
                </li>
            </ul>
            
            <h4>Likelihood Ratios for Clinical Decision-Making</h4>
            <table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>
                <tr style='background-color: #f1f1f1;'>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Likelihood Ratio</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Value Range</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Clinical Interpretation</th>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Positive LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>>10</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Strong evidence FOR disease when test positive</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Positive LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>5-10</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Moderate evidence for disease</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Positive LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>2-5</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Weak evidence for disease</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Negative LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'><0.1</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Strong evidence AGAINST disease when test negative</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Negative LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>0.1-0.2</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Moderate evidence against disease</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Negative LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>0.2-0.5</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Weak evidence against disease</td>
                </tr>
            </table>
            
            <h4>Diagnostic Odds Ratio (DOR)</h4>
            <ul>
                <li><strong>DOR > 25:</strong> Excellent overall discriminative ability</li>
                <li><strong>DOR 10-25:</strong> Good discriminative ability</li>
                <li><strong>DOR 5-10:</strong> Moderate discriminative ability</li>
                <li><strong>DOR < 5:</strong> Limited discriminative ability</li>
            </ul>
            
            <h3>🔍 Heterogeneity Assessment</h3>
            
            <div style='background-color: #fff3cd; padding: 15px; border-left: 4px solid #ffc107; margin: 10px 0;'>
                <h4>I² Statistic Interpretation:</h4>
                <ul>
                    <li><strong>I² < 25%:</strong> Low heterogeneity - results can be reliably pooled</li>
                    <li><strong>I² 25-50%:</strong> Moderate heterogeneity - investigate potential sources</li>
                    <li><strong>I² 50-75%:</strong> Substantial heterogeneity - pooling questionable</li>
                    <li><strong>I² > 75%:</strong> Considerable heterogeneity - avoid pooling, use subgroup analysis</li>
                </ul>
            </div>
            
            <h4>Common Sources of Heterogeneity:</h4>
            <ul>
                <li><strong>Patient Population:</strong> Disease stage, severity, demographics</li>
                <li><strong>Technical Factors:</strong> Staining protocols, antibody sources, automation</li>
                <li><strong>Methodological:</strong> Reference standards, blinding, cut-off thresholds</li>
                <li><strong>Geographic/Temporal:</strong> Population differences, technology evolution</li>
            </ul>
            
            <h3>📈 Publication Bias Assessment</h3>
            
            <h4>Deeks' Funnel Plot Test:</h4>
            <ul>
                <li><strong>p ≥ 0.05:</strong> No significant asymmetry - low risk of publication bias</li>
                <li><strong>p < 0.05:</strong> Significant asymmetry - potential publication bias detected</li>
            </ul>
            
            <div style='background-color: #f8d7da; padding: 15px; border-left: 4px solid #dc3545; margin: 10px 0;'>
                <p><strong>⚠️ When Publication Bias is Detected:</strong></p>
                <ul>
                    <li>Pooled estimates may be overoptimistic</li>
                    <li>Search for unpublished studies or negative results</li>
                    <li>Consider contacting study authors for additional data</li>
                    <li>Report limitations and interpret results cautiously</li>
                </ul>
            </div>
            
            <h3>🏥 Clinical Application Guidance</h3>
            
            <h4>IHC Marker Validation:</h4>
            <ul>
                <li><strong>Screening Applications:</strong> Prioritize high sensitivity (≥90%)</li>
                <li><strong>Confirmatory Testing:</strong> Prioritize high specificity (≥90%)</li>
                <li><strong>Balanced Performance:</strong> Consider clinical costs of false positives vs false negatives</li>
            </ul>
            
            <h4>AI Algorithm Implementation:</h4>
            <ul>
                <li><strong>Consistent Performance:</strong> Low heterogeneity supports broad implementation</li>
                <li><strong>Variable Performance:</strong> High heterogeneity suggests population-specific validation needed</li>
                <li><strong>External Validation:</strong> Meta-analysis provides evidence for regulatory approval</li>
            </ul>
            
            <h4>Predictive Values in Clinical Practice:</h4>
            <p><strong>Important:</strong> Sensitivity and specificity are test characteristics, but clinicians need predictive values that depend on disease prevalence in their population.</p>
            
            <table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>
                <tr style='background-color: #f1f1f1;'>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Disease Prevalence</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>PPV (Sen=90%, Spe=80%)</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>NPV (Sen=90%, Spe=80%)</th>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>5%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>19%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>99%</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>20%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>53%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>97%</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>50%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>82%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>89%</td>
                </tr>
            </table>
            
            <h3>📋 Reporting Recommendations</h3>
            
            <p>When reporting your meta-analysis results, include:</p>
            <ul>
                <li>✅ <strong>Study Selection:</strong> Number of studies included and excluded</li>
                <li>✅ <strong>Pooled Estimates:</strong> Sensitivity and specificity with 95% confidence intervals</li>
                <li>✅ <strong>Likelihood Ratios:</strong> For clinical decision-making context</li>
                <li>✅ <strong>Heterogeneity:</strong> I² values and potential sources investigated</li>
                <li>✅ <strong>Publication Bias:</strong> Deeks' test results and visual assessment</li>
                <li>✅ <strong>Clinical Implications:</strong> Population-specific predictive values</li>
                <li>✅ <strong>Limitations:</strong> Study quality, missing data, generalizability</li>
            </ul>
            
            <div style='background-color: #d1ecf1; padding: 15px; border-left: 4px solid #17a2b8; margin: 10px 0;'>
                <p><strong>💡 Pro Tip:</strong> Always interpret meta-analysis results in the context of your specific clinical population and intended use. A test excellent for one application may be inappropriate for another.</p>
            </div>
            "
            
            self$results$interpretation$setContent(html)
        },

        .appendInstructionMessage = function(message) {

            if (is.null(message) || !nzchar(message)) {
                return()
            }

            current <- self$results$instructions$content
            if (!is.null(current) && nzchar(current)) {
                self$results$instructions$setContent(paste(current, message))
            } else {
                self$results$instructions$setContent(message)
            }
        },

        # Enhanced data validation with user-friendly warnings
        .validateStudyData = function(meta_data, original_n) {

            # Check minimum study requirement first (critical error)
            if (nrow(meta_data) < 3) {
                # SERIALIZATION FIX: Use table note instead of inserting Notice
                error_msg <- sprintf(
                    'Meta-analysis requires at least 3 studies with complete data. Current studies: %d. Please add more studies or use individual study analysis instead.',
                    nrow(meta_data)
                )
                self$results$bivariateresults$setNote("insufficient_data", error_msg)
                self$results$hsrocresults$setNote("insufficient_data", error_msg)
                self$results$heterogeneity$setNote("insufficient_data", error_msg)
                return(FALSE)
            }

            # Check for zero cells (store flag for continuity correction)
            zero_cells <- any(meta_data$tp == 0 | meta_data$fp == 0 |
                            meta_data$fn == 0 | meta_data$tn == 0)
            if (zero_cells) {
                private$.continuity_correction <- TRUE
            }

            return(TRUE)
        },

        # Generate natural language summary
        .generateSummary = function(meta_data) {

            # If pooled estimates are not available, provide basic summary
            if (is.null(private$.pooled_sensitivity) || is.null(private$.pooled_specificity)) {
                private$.generateBasicSummary(meta_data)
                return()
            }

            # Calculate confidence intervals (these should be set by bivariate analysis)
            sens_pct <- round(private$.pooled_sensitivity * 100, 1)
            spec_pct <- round(private$.pooled_specificity * 100, 1)

            # Calculate positive and negative likelihood ratios
            lr_pos <- NA_real_
            lr_neg <- NA_real_

            if (is.finite(private$.pooled_sensitivity) && is.finite(private$.pooled_specificity)) {
                denom_plr <- 1 - private$.pooled_specificity
                denom_nlr <- private$.pooled_specificity

                if (is.finite(denom_plr) && denom_plr > 0) {
                    lr_pos <- private$.pooled_sensitivity / denom_plr
                }

                if (is.finite(denom_nlr) && denom_nlr > 0) {
                    lr_neg <- (1 - private$.pooled_sensitivity) / denom_nlr
                }
            }

            inv_lr_neg <- if (is.finite(lr_neg) && lr_neg > 0) 1 / lr_neg else NA_real_

            plr_text <- if (is.finite(lr_pos)) {
                sprintf("<p><strong>Positive Likelihood Ratio:</strong> %.2f - A positive test is %.1fx more likely in disease than healthy</p>",
                        lr_pos, lr_pos)
            } else {
                "<p><strong>Positive Likelihood Ratio:</strong> Not estimable with the current data (specificity ≈ 100% or model unstable).</p>"
            }

            nlr_text <- if (is.finite(lr_neg)) {
                if (is.finite(inv_lr_neg)) {
                    sprintf("<p><strong>Negative Likelihood Ratio:</strong> %.2f - A negative test is %.1fx more likely in healthy than disease</p>",
                            lr_neg, inv_lr_neg)
                } else {
                    sprintf("<p><strong>Negative Likelihood Ratio:</strong> %.2f - Interpretation unstable (sensitivity ≈ 100%%).</p>",
                            lr_neg)
                }
            } else {
                "<p><strong>Negative Likelihood Ratio:</strong> Not estimable with the current data (sensitivity ≈ 100% or model unstable).</p>"
            }

            copy_text <- sprintf(
                "Meta-analysis of %d diagnostic accuracy studies shows pooled sensitivity of %.1f%% and specificity of %.1f%%.",
                private$.n_studies, sens_pct, spec_pct
            )

            if (is.finite(lr_pos) && is.finite(lr_neg)) {
                copy_text <- sprintf(
                    "%s Positive LR %.2f and negative LR %.2f.",
                    copy_text, lr_pos, lr_neg
                )
            }

            # Build zero-cell correction disclosure
            correction_disclosure <- ""
            if (!is.null(private$.correction_method_used) && private$.correction_method_used != "none") {
                n_corrected <- length(private$.corrected_study_names)
                if (n_corrected > 0) {
                    method_label <- switch(private$.correction_method_used,
                                         constant = "constant +0.5 to all cells",
                                         treatment_arm = "treatment-arm (+0.5 to zero cells only)",
                                         empirical = "empirical (1/N) correction",
                                         "unknown method")

                    # SERIALIZATION FIX: Use table note instead of inserting Notice
                    warning_msg <- sprintf(
                        'Zero-cell correction applied (%s) to %d of %d studies (%s). Results should be interpreted with caution as corrections can introduce bias, especially in large studies.',
                        method_label, n_corrected, private$.n_studies,
                        paste(head(private$.corrected_study_names, 3), collapse = ', ')
                    )
                    self$results$bivariateresults$setNote("zero_cell_warning", warning_msg)

                    correction_disclosure <- sprintf(
                        "<div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                            <h5>⚠️ Zero-Cell Correction Applied</h5>
                            <p><strong>Method:</strong> %s</p>
                            <p><strong>Studies corrected:</strong> %d of %d (%s)</p>
                            <p><em>Note:</em> Zero-cell corrections can introduce bias, especially in large studies. Results should be interpreted with caution.</p>
                        </div>",
                        method_label,
                        n_corrected,
                        private$.n_studies,
                        paste(head(private$.corrected_study_names, 5), collapse = ", ")
                    )
                }
            }

            summary_html <- sprintf("
            <div class='analysis-summary' style='background-color: #e8f4f8; padding: 20px; border-radius: 8px; margin: 10px 0;'>
                <h4>Meta-Analysis Summary</h4>
                <p><strong>Analysis Type:</strong> Diagnostic test accuracy meta-analysis of %d studies</p>

                <div style='background-color: white; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>Pooled Test Performance</h5>
                    <p><strong>Sensitivity:</strong> %.1f%% - The test correctly identifies %.0f out of 100 patients with disease</p>
                    <p><strong>Specificity:</strong> %.1f%% - The test correctly identifies %.0f out of 100 healthy individuals</p>
                </div>

                %s

                <div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>Clinical Decision Metrics</h5>
                    %s
                    %s
                </div>

                <div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>Interpretation Guide</h5>
                    <p>%s</p>
                </div>

                <div style='margin-top: 15px;'>
                    <button onclick='navigator.clipboard.writeText(this.getAttribute(\"data-text\"))'
                            data-text='%s'
                            style='background-color: #007bff; color: white; border: none; padding: 8px 16px; border-radius: 4px; cursor: pointer;'>
                        Copy Summary to Clipboard
                    </button>
                </div>
            </div>
            ",
            private$.n_studies,
            sens_pct, sens_pct,
            spec_pct, spec_pct,
            correction_disclosure,
            plr_text,
            nlr_text,
            private$.getInterpretationText(sens_pct, spec_pct, lr_pos, lr_neg),
            copy_text
            )

            self$results$summary$setContent(summary_html)
        },

        # Generate basic summary when pooled estimates are not available
        .generateBasicSummary = function(meta_data) {

            if (is.null(meta_data) || nrow(meta_data) == 0) {
                return()
            }

            # Calculate individual study statistics
            meta_data$sensitivity <- meta_data$tp / (meta_data$tp + meta_data$fn)
            meta_data$specificity <- meta_data$tn / (meta_data$tn + meta_data$fp)
            meta_data$sample_size <- meta_data$tp + meta_data$fp + meta_data$fn + meta_data$tn

            # Calculate basic descriptive statistics
            n_studies <- nrow(meta_data)
            total_sample <- sum(meta_data$sample_size, na.rm = TRUE)

            sens_mean <- mean(meta_data$sensitivity, na.rm = TRUE) * 100
            sens_range <- range(meta_data$sensitivity, na.rm = TRUE) * 100
            spec_mean <- mean(meta_data$specificity, na.rm = TRUE) * 100
            spec_range <- range(meta_data$specificity, na.rm = TRUE) * 100

            # Determine why pooled estimates failed
            reason <- ""
            if (!isTRUE(self$options$bivariate_analysis)) {
                reason <- "Bivariate analysis was not enabled. Enable bivariate analysis for pooled estimates."
            } else if (n_studies < 3) {
                reason <- sprintf("Only %d studies available. At least 3 studies are required for meta-analysis.", n_studies)
            } else {
                reason <- "Bivariate meta-analysis encountered an error. Check individual study results and data quality."
            }

            summary_html <- sprintf("
            <div class='analysis-summary' style='background-color: #e8f4f8; padding: 20px; border-radius: 8px; margin: 10px 0;'>
                <h4>Meta-Analysis Summary</h4>
                <p><strong>Analysis Status:</strong> %s</p>

                <div style='background-color: white; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>Study Overview</h5>
                    <p><strong>Number of Studies:</strong> %d</p>
                    <p><strong>Total Sample Size:</strong> %d participants</p>
                    <p><strong>Sample Size Range:</strong> %d - %d per study</p>
                </div>

                <div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>Individual Study Performance (Descriptive)</h5>
                    <p><strong>Sensitivity:</strong> Mean %.1f%% (Range: %.1f%% - %.1f%%)</p>
                    <p><strong>Specificity:</strong> Mean %.1f%% (Range: %.1f%% - %.1f%%)</p>
                    <p><em>Note: These are simple averages, not meta-analytic pooled estimates.</em></p>
                </div>

                <div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>Recommendation</h5>
                    <p>%s</p>
                    <p>Individual study results are available in the table below for detailed examination.</p>
                </div>
            </div>
            ",
            reason,
            n_studies,
            total_sample,
            min(meta_data$sample_size, na.rm = TRUE),
            max(meta_data$sample_size, na.rm = TRUE),
            sens_mean, sens_range[1], sens_range[2],
            spec_mean, spec_range[1], spec_range[2],
            reason
            )

            self$results$summary$setContent(summary_html)
        },

        # Helper function for dynamic interpretation text with actual values
        .getInterpretationText = function(sens, spec, lr_pos, lr_neg) {
            # Classify sensitivity performance
            sens_class <- if (sens >= 90) {
                "excellent"
            } else if (sens >= 80) {
                "good"
            } else if (sens >= 70) {
                "moderate"
            } else {
                "limited"
            }

            # Classify specificity performance
            spec_class <- if (spec >= 90) {
                "excellent"
            } else if (spec >= 80) {
                "good"
            } else if (spec >= 70) {
                "moderate"
            } else {
                "limited"
            }

            # Classify positive LR
            plr_class <- if (is.finite(lr_pos)) {
                if (lr_pos > 10) "strong"
                else if (lr_pos >= 5) "moderate"
                else if (lr_pos >= 2) "weak"
                else "minimal"
            } else {
                "not estimable"
            }

            # Classify negative LR
            nlr_class <- if (is.finite(lr_neg)) {
                if (lr_neg < 0.1) "strong"
                else if (lr_neg <= 0.2) "moderate"
                else if (lr_neg <= 0.5) "weak"
                else "minimal"
            } else {
                "not estimable"
            }

            # Build dynamic interpretation with actual values
            interpretation <- sprintf(
                "<strong>Your pooled sensitivity of %.1f%%</strong> is classified as <em>%s</em> for screening purposes. ",
                sens, sens_class
            )

            # Add sensitivity-specific guidance
            if (sens >= 90) {
                interpretation <- paste0(interpretation,
                    sprintf("With %.1f%% sensitivity, this test will detect %.0f out of 100 patients with disease, missing only %.0f. ",
                            sens, sens, 100 - sens),
                    "<strong>Clinical implication:</strong> Excellent for ruling OUT disease when test is negative (SnNout principle). "
                )
            } else if (sens >= 80) {
                interpretation <- paste0(interpretation,
                    sprintf("With %.1f%% sensitivity, approximately %.0f out of 100 diseased patients will be correctly identified. ",
                            sens, sens),
                    "<strong>Clinical implication:</strong> Acceptable for screening, but negative results should be interpreted with caution. "
                )
            } else {
                interpretation <- paste0(interpretation,
                    sprintf("With %.1f%% sensitivity, up to %.0f out of 100 diseased patients may be missed. ",
                            sens, 100 - sens),
                    "<strong>Clinical implication:</strong> Limited screening utility - negative results do NOT effectively rule out disease. "
                )
            }

            # Add specificity interpretation
            interpretation <- paste0(interpretation,
                sprintf("<br><br><strong>Your pooled specificity of %.1f%%</strong> is classified as <em>%s</em> for confirmatory testing. ",
                        spec, spec_class)
            )

            if (spec >= 90) {
                interpretation <- paste0(interpretation,
                    sprintf("With %.1f%% specificity, only %.0f out of 100 healthy individuals will test positive (false alarms). ",
                            spec, 100 - spec),
                    "<strong>Clinical implication:</strong> Excellent for ruling IN disease when test is positive (SpPin principle). "
                )
            } else if (spec >= 80) {
                interpretation <- paste0(interpretation,
                    sprintf("With %.1f%% specificity, approximately %.0f out of 100 healthy individuals will be correctly classified. ",
                            spec, spec),
                    "<strong>Clinical implication:</strong> Acceptable for confirmation, but positive results may include false positives. "
                )
            } else {
                interpretation <- paste0(interpretation,
                    sprintf("With %.1f%% specificity, up to %.0f out of 100 healthy individuals may test positive. ",
                            spec, 100 - spec),
                    "<strong>Clinical implication:</strong> Limited confirmatory value - positive results do NOT strongly confirm disease. "
                )
            }

            # Add likelihood ratio interpretation if available
            if (plr_class != "not estimable") {
                interpretation <- paste0(interpretation,
                    sprintf("<br><br><strong>Your positive LR of %.2f</strong> provides <em>%s</em> evidence FOR disease when test is positive. ",
                            lr_pos, plr_class)
                )

                if (lr_pos > 10) {
                    interpretation <- paste0(interpretation,
                        sprintf("A positive result increases disease probability substantially (multiplies pre-test odds by %.1fx). ",
                                lr_pos)
                    )
                } else if (lr_pos >= 5) {
                    interpretation <- paste0(interpretation,
                        "A positive result moderately increases disease probability. "
                    )
                } else {
                    interpretation <- paste0(interpretation,
                        "A positive result provides only weak evidence - clinical context is essential. "
                    )
                }
            }

            if (nlr_class != "not estimable") {
                interpretation <- paste0(interpretation,
                    sprintf("<strong>Your negative LR of %.2f</strong> provides <em>%s</em> evidence AGAINST disease when test is negative. ",
                            lr_neg, nlr_class)
                )

                if (lr_neg < 0.1) {
                    interpretation <- paste0(interpretation,
                        sprintf("A negative result substantially decreases disease probability (divides pre-test odds by %.1fx). ",
                                1/lr_neg)
                    )
                } else if (lr_neg <= 0.2) {
                    interpretation <- paste0(interpretation,
                        "A negative result moderately decreases disease probability. "
                    )
                } else {
                    interpretation <- paste0(interpretation,
                        "A negative result provides only weak evidence - clinical context is essential. "
                    )
                }
            }

            # Overall recommendation
            if (sens >= 90 && spec >= 90) {
                interpretation <- paste0(interpretation,
                    "<br><br><strong>Overall Assessment:</strong> This test demonstrates <em>excellent</em> diagnostic accuracy suitable for both screening and confirmatory use in appropriate clinical populations."
                )
            } else if (sens >= 80 && spec >= 80) {
                interpretation <- paste0(interpretation,
                    "<br><br><strong>Overall Assessment:</strong> This test demonstrates <em>good</em> diagnostic accuracy and can be useful for clinical decision-making when combined with other clinical information."
                )
            } else if (sens >= 90 || spec >= 90) {
                interpretation <- paste0(interpretation,
                    "<br><br><strong>Overall Assessment:</strong> This test has <em>asymmetric</em> performance - excellent for one purpose (rule-in OR rule-out) but limited for the other. Use strategically based on clinical goals."
                )
            } else {
                interpretation <- paste0(interpretation,
                    "<br><br><strong>Overall Assessment:</strong> This test demonstrates <em>moderate</em> diagnostic accuracy. Consider using in combination with other tests or as part of a broader diagnostic algorithm rather than as a standalone test."
                )
            }

            return(interpretation)
        },

        # Populate About This Analysis panel
        .populateAboutPanel = function() {

            html <- "
            <div class='about-panel' style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin: 10px 0;'>
                <h4>🎯 About Diagnostic Test Meta-Analysis</h4>

                <div style='margin: 15px 0;'>
                    <h5>What This Analysis Does</h5>
                    <p>Combines results from multiple diagnostic accuracy studies to estimate overall test performance through:</p>
                    <ul>
                        <li>📊 <strong>Bivariate modeling</strong> - Jointly analyzes sensitivity and specificity</li>
                        <li>📈 <strong>HSROC curves</strong> - Models the trade-off between sensitivity and specificity</li>
                        <li>🔍 <strong>Heterogeneity assessment</strong> - Evaluates consistency across studies</li>
                        <li>📉 <strong>Publication bias</strong> - Checks for selective reporting</li>
                    </ul>
                </div>

                <div style='margin: 15px 0; background-color: #e3f2fd; padding: 15px; border-radius: 5px; border-left: 4px solid #2196F3;'>
                    <h5>📘 Understanding Bivariate vs HSROC Models</h5>
                    <p><strong>Both models are valuable and provide complementary information:</strong></p>

                    <p><strong>Bivariate Random-Effects Model (Recommended Primary Approach):</strong></p>
                    <ul>
                        <li>✅ Provides <em>intuitive</em> pooled sensitivity and specificity estimates</li>
                        <li>✅ Accounts for correlation between sensitivity and specificity</li>
                        <li>✅ Directly interpretable for clinical decision-making</li>
                        <li>✅ Preferred for meta-analyses with <em>homogeneous thresholds</em></li>
                        <li>📊 <strong>Use this when:</strong> Studies use the same diagnostic threshold</li>
                    </ul>

                    <p><strong>HSROC Model (Hierarchical Summary ROC):</strong></p>
                    <ul>
                        <li>✅ Models the <em>entire ROC curve</em> across different thresholds</li>
                        <li>✅ Provides threshold-independent accuracy estimates (Λ parameter)</li>
                        <li>✅ Captures asymmetry in diagnostic accuracy (β parameter)</li>
                        <li>✅ Better for studies with <em>varying cutoffs or thresholds</em></li>
                        <li>📊 <strong>Use this when:</strong> Studies use different diagnostic thresholds</li>
                    </ul>

                    <p><strong>💡 Clinical Insight:</strong> The SROC plot shown by this module is derived from the <em>bivariate model</em>, which provides a simpler and more intuitive visualization. The HSROC table provides additional parametric information (θ = threshold, Λ = accuracy) that can be useful for understanding test performance across different threshold scenarios. <strong>Both approaches are statistically valid</strong> - the choice depends on your research question and whether thresholds vary across studies.</p>
                </div>

                <div style='margin: 15px 0;'>
                    <h5>When to Use This Analysis</h5>
                    <ul>
                        <li>✅ Evaluating AI algorithms for pathology diagnosis</li>
                        <li>✅ Assessing biomarker diagnostic accuracy</li>
                        <li>✅ Comparing imaging modalities</li>
                        <li>✅ Synthesizing evidence for clinical guidelines</li>
                    </ul>
                </div>

                <div style='margin: 15px 0; background-color: #fff3cd; padding: 15px; border-radius: 5px;'>
                    <h5>⚠️ Key Requirements & Assumptions</h5>
                    <ul>
                        <li>Minimum 3 studies with 2×2 diagnostic data</li>
                        <li>Studies should evaluate the same test and target condition</li>
                        <li>Reference standard should be consistent across studies</li>
                        <li>Patient spectrum should be clinically relevant</li>
                    </ul>
                </div>

                <div style='margin: 15px 0;'>
                    <h5>📖 Quick Start Guide</h5>
                    <ol>
                        <li><strong>Step 1:</strong> Select your study identifier variable</li>
                        <li><strong>Step 2:</strong> Assign TP, FP, FN, TN count variables</li>
                        <li><strong>Step 3:</strong> Choose analysis options (bivariate recommended)</li>
                        <li><strong>Step 4:</strong> Review pooled estimates and heterogeneity</li>
                        <li><strong>Step 5:</strong> Interpret in your clinical context</li>
                    </ol>
                </div>

                <div style='background-color: #d1ecf1; padding: 15px; border-radius: 5px; margin: 15px 0;'>
                    <p><strong>💡 Tip:</strong> Start with the bivariate model and forest plot to understand overall performance, then explore heterogeneity sources with meta-regression if needed.</p>
                </div>
            </div>
            "

            self$results$about$setContent(html)
        },

        # Optimized data preparation with caching
        .prepareAnalysisData = function(meta_data) {

            if (private$.data_cache_valid &&
                !is.null(private$.analysis_data) &&
                !is.null(private$.mada_data)) {
                return(list(
                    analysis_data = private$.analysis_data,
                    mada_data = private$.mada_data,
                    continuity_correction = private$.continuity_correction
                ))
            }

            if (is.null(meta_data) || nrow(meta_data) == 0) {
                private$.analysis_data <- meta_data
                private$.mada_data <- data.frame()
                private$.continuity_correction <- FALSE
                private$.data_cache_valid <- TRUE
                return(list(
                    analysis_data = private$.analysis_data,
                    mada_data = private$.mada_data,
                    continuity_correction = private$.continuity_correction
                ))
            }

            analysis_data <- meta_data
            numeric_cols <- c("tp", "fp", "fn", "tn")

            for (col in numeric_cols) {
                analysis_data[[col]] <- as.numeric(analysis_data[[col]])
            }

            # Apply zero-cell correction based on user choice
            correction_method <- self$options$zero_cell_correction %||% "none"
            correction_flags <- rep(FALSE, nrow(analysis_data))
            corrected_studies <- character(0)

            for (i in seq_len(nrow(analysis_data))) {
                row_counts <- as.numeric(analysis_data[i, numeric_cols])
                if (any(!is.finite(row_counts))) {
                    next
                }
                if (any(row_counts < 0)) {
                    next
                }

                has_zero <- any(row_counts == 0)

                if (has_zero && correction_method != "none") {
                    if (correction_method == "constant") {
                        # Global +0.5 to all cells (biased for large studies)
                        analysis_data[i, numeric_cols] <- row_counts + 0.5
                        correction_flags[i] <- TRUE
                        corrected_studies <- c(corrected_studies, as.character(analysis_data[i, "study"]))

                    } else if (correction_method == "treatment_arm") {
                        # Add 0.5 only to zero cells (treatment-arm correction)
                        corrected_row <- row_counts
                        corrected_row[row_counts == 0] <- 0.5
                        analysis_data[i, numeric_cols] <- corrected_row
                        correction_flags[i] <- TRUE
                        corrected_studies <- c(corrected_studies, as.character(analysis_data[i, "study"]))

                    } else if (correction_method == "empirical") {
                        # Empirical correction: use 1/N where N is total sample size
                        total_n <- sum(row_counts, na.rm = TRUE)
                        if (total_n > 0) {
                            correction <- 1 / total_n
                            analysis_data[i, numeric_cols] <- row_counts + correction
                            correction_flags[i] <- TRUE
                            corrected_studies <- c(corrected_studies, as.character(analysis_data[i, "study"]))
                        }
                    }
                }
            }

            # Store correction info for reporting
            private$.correction_method_used <- correction_method
            private$.corrected_study_names <- corrected_studies

            private$.analysis_data <- analysis_data
            private$.mada_data <- data.frame(
                TP = analysis_data$tp,
                FP = analysis_data$fp,
                FN = analysis_data$fn,
                TN = analysis_data$tn
            )
            private$.continuity_correction <- any(correction_flags)
            private$.data_cache_valid <- TRUE

            list(
                analysis_data = private$.analysis_data,
                mada_data = private$.mada_data,
                continuity_correction = private$.continuity_correction,
                corrected_rows = which(correction_flags)
            )
        },

        # Plot explanation functions
        .populateForestPlotExplanation = function() {
            html <- "
            <div class='plot-explanation' style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin: 10px 0;'>
                <h4>🌳 Forest Plot Interpretation Guide</h4>

                <div style='background-color: white; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>What This Plot Shows</h5>
                    <p><strong>Forest Plot:</strong> Displays individual study results for sensitivity and specificity with confidence intervals. Each study is represented by a point (estimate) with horizontal lines (confidence intervals).</p>

                    <ul>
                        <li><strong>Left Panel (Sensitivity):</strong> Proportion of diseased cases correctly identified</li>
                        <li><strong>Right Panel (Specificity):</strong> Proportion of healthy cases correctly identified</li>
                        <li><strong>Horizontal Lines:</strong> 95% confidence intervals showing precision of estimates</li>
                        <li><strong>Point Position:</strong> Higher on Y-axis = higher study estimate</li>
                    </ul>
                </div>

                <div style='background-color: #e8f5e8; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>Clinical Interpretation</h5>
                    <ul>
                        <li><strong>Consistent Results:</strong> Points clustered together = low heterogeneity</li>
                        <li><strong>Wide Spread:</strong> Points scattered = high heterogeneity (investigate sources)</li>
                        <li><strong>Narrow CIs:</strong> Large studies with precise estimates</li>
                        <li><strong>Wide CIs:</strong> Small studies with less precise estimates</li>
                    </ul>
                </div>

                <div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>💡 Quick Assessment Tips</h5>
                    <ul>
                        <li>Look for outlier studies (points far from others)</li>
                        <li>Check if confidence intervals overlap substantially</li>
                        <li>Consider whether variation reflects true differences or chance</li>
                        <li>Use this plot to identify studies for sensitivity analysis</li>
                    </ul>
                </div>
            </div>
            "

            self$results$forestplot_explanation$setContent(html)
        },

        .populateSROCPlotExplanation = function() {
            html <- "
            <div class='plot-explanation' style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin: 10px 0;'>
                <h4>📈 Summary ROC Plot Interpretation Guide</h4>

                <div style='background-color: white; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>What This Plot Shows</h5>
                    <p><strong>SROC Plot:</strong> Summary Receiver Operating Characteristic curve showing the trade-off between sensitivity and specificity across all studies.</p>

                    <ul>
                        <li><strong>X-axis:</strong> False Positive Rate (1 - Specificity) - lower is better</li>
                        <li><strong>Y-axis:</strong> True Positive Rate (Sensitivity) - higher is better</li>
                        <li><strong>Individual Studies:</strong> Circles sized by sample size</li>
                        <li><strong>Pooled Estimate:</strong> Large triangle showing meta-analytic summary</li>
                    </ul>
                </div>

                <div style='background-color: #e8f5e8; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>Clinical Interpretation</h5>
                    <ul>
                        <li><strong>Upper Left Corner:</strong> Ideal performance (high sensitivity, low false positive rate)</li>
                        <li><strong>Diagonal Line:</strong> Represents random chance (no discriminative ability)</li>
                        <li><strong>Above Diagonal:</strong> Better than chance performance</li>
                        <li><strong>Point Scatter:</strong> Studies clustered tightly = consistent test performance</li>
                    </ul>
                </div>

                <div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>💡 Quick Assessment Tips</h5>
                    <ul>
                        <li>Closer to upper-left corner = better overall diagnostic accuracy</li>
                        <li>Wide scatter of points = substantial between-study heterogeneity</li>
                        <li>Triangle position shows where your pooled test performance lies</li>
                        <li>Compare triangle position to individual studies for consistency</li>
                    </ul>
                </div>

                <div style='background-color: #d1ecf1; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>🎯 Clinical Decision Making</h5>
                    <p><strong>Use this plot to:</strong> Visualize test performance trade-offs, identify optimal operating points, and assess consistency across different study populations and settings.</p>
                </div>
            </div>
            "

            self$results$srocplot_explanation$setContent(html)
        },

        .populateFunnelPlotExplanation = function() {
            html <- "
            <div class='plot-explanation' style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin: 10px 0;'>
                <h4>🔍 Funnel Plot Interpretation Guide</h4>

                <div style='background-color: white; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>What This Plot Shows</h5>
                    <p><strong>Funnel Plot:</strong> Assesses publication bias by plotting study precision against effect size (log diagnostic odds ratio).</p>

                    <ul>
                        <li><strong>X-axis:</strong> Log Diagnostic Odds Ratio (effect size)</li>
                        <li><strong>Y-axis:</strong> Precision (1/Standard Error) - higher = more precise</li>
                        <li><strong>Each Point:</strong> One study in your meta-analysis</li>
                        <li><strong>Expected Pattern:</strong> Inverted funnel shape if no bias present</li>
                    </ul>
                </div>

                <div style='background-color: #f8d7da; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>⚠️ Publication Bias Indicators</h5>
                    <ul>
                        <li><strong>Asymmetric Funnel:</strong> Missing studies on one side (usually left = negative results)</li>
                        <li><strong>Gap in Lower Region:</strong> Small studies with negative/null results missing</li>
                        <li><strong>Deeks' Test p < 0.05:</strong> Statistical evidence of funnel plot asymmetry</li>
                    </ul>
                </div>

                <div style='background-color: #d4edda; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>✅ No Bias Indicators</h5>
                    <ul>
                        <li><strong>Symmetric Funnel:</strong> Studies distributed evenly on both sides</li>
                        <li><strong>Deeks' Test p ≥ 0.05:</strong> No statistical evidence of asymmetry</li>
                        <li><strong>Small Studies Present:</strong> Range of precision levels represented</li>
                    </ul>
                </div>

                <div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>💡 Interpretation Caveats</h5>
                    <ul>
                        <li><strong>Small Sample:</strong> Funnel plot unreliable with <10 studies</li>
                        <li><strong>Heterogeneity:</strong> Clinical differences can mimic publication bias</li>
                        <li><strong>Other Causes:</strong> Language bias, database bias, or chance</li>
                        <li><strong>Action Needed:</strong> If bias detected, search for unpublished studies</li>
                    </ul>
                </div>
            </div>
            "

            self$results$funnelplot_explanation$setContent(html)
        }
    )
)
