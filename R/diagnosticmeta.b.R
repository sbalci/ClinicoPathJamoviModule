#' @title Diagnostic Test Meta-Analysis for Pathology
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import mada
#' @import meta
#' @import metafor
#' @importFrom stats qnorm pnorm qt pt
#' @export


diagnosticmetaClass <- R6::R6Class(
    "diagnosticmetaClass",
    inherit = diagnosticmetaBase,
    private = list(
        # Cache variables for performance optimization
        .mada_data = NULL,
        .data_cache_valid = FALSE,
        .pooled_sensitivity = NULL,
        .pooled_specificity = NULL,
        .n_studies = 0,

        # Helper for null-safe operations
        `%||%` = function(x, y) {
            if (is.null(x)) y else x
        },

        .init = function() {

            # Initialize instructions
            private$.populateInstructions()

            # Initialize About panel
            private$.populateAboutPanel()

            # Initialize results tables with proper columns
            private$.initializeResultsTables()

        },
        
        .run = function() {

            # Invalidate cache when data changes
            private$.data_cache_valid <- FALSE
            private$.mada_data <- NULL
            private$.pooled_sensitivity <- NULL
            private$.pooled_specificity <- NULL

            # Check if data is ready
            if (is.null(self$data) || nrow(self$data) == 0) {
                self$results$instructions$setContent(
                    "<p>Welcome to Diagnostic Test Meta-Analysis for Pathology!</p>
                     <p>This module performs comprehensive meta-analysis of diagnostic test accuracy studies.</p>
                     <p>Please provide your data to begin the analysis.</p>"
                )
                return()
            }
            
            # Get variables
            study_var <- self$options$study
            tp_var <- self$options$true_positives
            fp_var <- self$options$false_positives
            fn_var <- self$options$false_negatives
            tn_var <- self$options$true_negatives
            
            # Validate variables
            if (is.null(study_var) || is.null(tp_var) || is.null(fp_var) || 
                is.null(fn_var) || is.null(tn_var)) {
                return()
            }
            
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
                study = data[[study_var]],
                tp = as.numeric(data[[tp_var]]),
                fp = as.numeric(data[[fp_var]]),
                fn = as.numeric(data[[fn_var]]),
                tn = as.numeric(data[[tn_var]])
            )
            
            # Store original row count for validation
            original_n <- nrow(meta_data)

            # Remove any rows with zero or negative counts
            meta_data <- meta_data[meta_data$tp >= 0 & meta_data$fp >= 0 &
                                  meta_data$fn >= 0 & meta_data$tn >= 0, ]

            # Enhanced validation with user-friendly warnings
            if (!private$.validateStudyData(meta_data, original_n)) {
                return()
            }

            # Store number of studies for summary
            private$.n_studies <- nrow(meta_data)
            
            if (nrow(meta_data) < 2) {
                self$results$instructions$setContent(
                    "<p><strong>Error:</strong> Insufficient valid studies for meta-analysis.</p>
                     <p>At least 2 studies with complete diagnostic test data are required.</p>"
                )
                return()
            }
            
            # Perform bivariate meta-analysis
            tryCatch({
                private$.performBivariateMetaAnalysis(meta_data)

                # Generate natural language summary after successful analysis
                private$.generateSummary(meta_data)
            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<p><strong>Analysis Error:</strong> ", e$message, "</p>")
                )
            })
            
            # Perform HSROC analysis if requested
            if (self$options$hsroc_analysis) {
                tryCatch({
                    private$.performHSROCAnalysis(meta_data)
                }, error = function(e) {
                    self$results$instructions$setContent(
                        paste0("<p><strong>HSROC Analysis Error:</strong> ", e$message, "</p>")
                    )
                })
            }
            
            # Perform heterogeneity analysis
            if (self$options$heterogeneity_analysis) {
                tryCatch({
                    private$.performHeterogeneityAnalysis(meta_data)
                }, error = function(e) {
                    self$results$instructions$setContent(
                        paste0("<p><strong>Heterogeneity Analysis Error:</strong> ", e$message, "</p>")
                    )
                })
            }
            
            # Perform meta-regression if covariate specified
            if (self$options$meta_regression && !is.null(self$options$covariate)) {
                tryCatch({
                    private$.performMetaRegression(meta_data)
                }, error = function(e) {
                    self$results$instructions$setContent(
                        paste0("<p><strong>Meta-Regression Error:</strong> ", e$message, "</p>")
                    )
                })
            }
            
            # Perform publication bias assessment
            if (self$options$publication_bias) {
                tryCatch({
                    private$.performPublicationBiasAssessment(meta_data)
                }, error = function(e) {
                    self$results$instructions$setContent(
                        paste0("<p><strong>Publication Bias Analysis Error:</strong> ", e$message, "</p>")
                    )
                })
            }
            
            # Generate plots
            if (self$options$forest_plot) {
                private$.populateForestPlot(meta_data)
            }
            
            if (self$options$sroc_plot) {
                private$.populateSROCPlot(meta_data)
            }
            
            if (self$options$funnel_plot && self$options$publication_bias) {
                private$.populateFunnelPlot(meta_data)
            }

            # Populate individual studies table
            if (self$options$show_individual_studies) {
                private$.populateIndividualStudies(meta_data)
            }

            # Provide clinical interpretation
            private$.populateInterpretation()
        },
        
        .performBivariateMetaAnalysis = function(meta_data) {
            
            # Calculate sensitivity and specificity for each study
            meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
            meta_data$spec <- meta_data$tn / (meta_data$tn + meta_data$fp)
            meta_data$fpr <- 1 - meta_data$spec
            
            # Perform bivariate random-effects meta-analysis using mada package
            if (requireNamespace("mada", quietly = TRUE)) {

                # Validate input data
                if (is.null(meta_data) || nrow(meta_data) == 0) {
                    return()
                }

                # Create data frame with uppercase column names for mada package
                mada_data <- data.frame(
                    TP = meta_data$tp,
                    FP = meta_data$fp,
                    FN = meta_data$fn,
                    TN = meta_data$tn
                )

                # Remove rows with any NA values and ensure positive counts
                mada_data <- mada_data[complete.cases(mada_data), ]
                mada_data <- mada_data[mada_data$TP >= 0 & mada_data$FP >= 0 &
                                      mada_data$FN >= 0 & mada_data$TN >= 0, ]

                # Check if we have enough data
                if (nrow(mada_data) < 2) {
                    return()
                }

                # Fit bivariate model
                biv_model <- mada::reitsma(mada_data, correction.control = "single")
                
                # Extract results
                summary_results <- summary(biv_model)
                
                # Populate bivariate results table
                bivariate_table <- self$results$bivariateresults

                # Check if coefficients exist
                if (is.null(summary_results$coefficients) || nrow(summary_results$coefficients) < 2) {
                    return()
                }

                # Pooled sensitivity
                sens_estimate <- summary_results$coefficients[1, "Estimate"]
                sens_se <- summary_results$coefficients[1, "Std. Error"]

                # Store pooled values for summary generation
                private$.pooled_sensitivity <- plogis(sens_estimate)  # Convert from logit scale

                # Check for valid values
                if (is.na(sens_estimate) || is.na(sens_se) || sens_se <= 0) {
                    sens_estimate <- 0
                    sens_se <- 1
                    sens_ci_lower <- -Inf
                    sens_ci_upper <- Inf
                } else {
                    alpha <- 1 - self$options$confidence_level
                    z_crit <- qnorm(1 - alpha/2)
                    sens_ci_lower <- sens_estimate - z_crit * sens_se
                    sens_ci_upper <- sens_estimate + z_crit * sens_se
                }
                
                # Transform from logit scale
                pooled_sens <- plogis(sens_estimate)
                pooled_sens_ci_lower <- plogis(sens_ci_lower)
                pooled_sens_ci_upper <- plogis(sens_ci_upper)
                
                # Pooled specificity
                spec_estimate <- summary_results$coefficients[2, "Estimate"]
                spec_se <- summary_results$coefficients[2, "Std. Error"]

                # Store pooled values for summary generation
                private$.pooled_specificity <- plogis(spec_estimate)  # Convert from logit scale

                # Check for valid values
                if (is.na(spec_estimate) || is.na(spec_se) || spec_se <= 0) {
                    spec_estimate <- 0
                    spec_se <- 1
                    spec_ci_lower <- -Inf
                    spec_ci_upper <- Inf
                } else {
                    alpha <- 1 - self$options$confidence_level
                    z_crit <- qnorm(1 - alpha/2)
                    spec_ci_lower <- spec_estimate - z_crit * spec_se
                    spec_ci_upper <- spec_estimate + z_crit * spec_se
                }
                
                # Transform from logit scale
                pooled_spec <- plogis(spec_estimate)
                pooled_spec_ci_lower <- plogis(spec_ci_lower)
                pooled_spec_ci_upper <- plogis(spec_ci_upper)
                
                # Add rows to table
                bivariate_table$addRow(rowKey = "sensitivity", values = list(
                    parameter = "Pooled Sensitivity",
                    estimate = pooled_sens,
                    ci_lower = pooled_sens_ci_lower,
                    ci_upper = pooled_sens_ci_upper,
                    i_squared = NA_real_,
                    p_value = summary_results$coefficients[1, "Pr(>|z|)"]
                ))
                
                bivariate_table$addRow(rowKey = "specificity", values = list(
                    parameter = "Pooled Specificity", 
                    estimate = pooled_spec,
                    ci_lower = pooled_spec_ci_lower,
                    ci_upper = pooled_spec_ci_upper,
                    i_squared = NA_real_,
                    p_value = summary_results$coefficients[2, "Pr(>|z|)"]
                ))
                
                # Calculate additional diagnostic accuracy measures
                pooled_plr <- pooled_sens / (1 - pooled_spec)
                pooled_nlr <- (1 - pooled_sens) / pooled_spec
                pooled_dor <- pooled_plr / pooled_nlr
                
                bivariate_table$addRow(rowKey = "plr", values = list(
                    parameter = "Positive Likelihood Ratio",
                    estimate = pooled_plr,
                    ci_lower = NA_real_,
                    ci_upper = NA_real_,
                    i_squared = NA_real_,
                    p_value = NA_real_
                ))
                
                bivariate_table$addRow(rowKey = "nlr", values = list(
                    parameter = "Negative Likelihood Ratio",
                    estimate = pooled_nlr,
                    ci_lower = NA_real_,
                    ci_upper = NA_real_,
                    i_squared = NA_real_,
                    p_value = NA_real_
                ))
                
                bivariate_table$addRow(rowKey = "dor", values = list(
                    parameter = "Diagnostic Odds Ratio",
                    estimate = pooled_dor,
                    ci_lower = NA_real_,
                    ci_upper = NA_real_,
                    i_squared = NA_real_,
                    p_value = NA_real_
                ))
            }
        },
        
        .performHSROCAnalysis = function(meta_data) {

            if (requireNamespace("mada", quietly = TRUE)) {

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

                # Use optimized data preparation
                mada_data <- private$.prepareMADAData(meta_data)

                # Check if we have enough data
                if (nrow(mada_data) < 3) {
                    self$results$hsrocresults$setNote("toofew", "HSROC requires at least 3 studies")
                    return()
                }

                # Check for zero cells in MADA data which cause problems
                zero_cells <- any(mada_data == 0)
                if (zero_cells) {
                    self$results$hsrocresults$setNote("zerocells", "Zero cells detected - HSROC may be unreliable")
                }

                # Fit HSROC model with enhanced error handling
                hsroc_model <- tryCatch({
                    result <- mada::phm(mada_data)
                    if (is.null(result)) {
                        stop("HSROC model fitting returned NULL")
                    }
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

                # Populate HSROC results table
                hsroc_table <- self$results$hsrocresults

                # Extract HSROC parameters with comprehensive validation
                coefficients <- NULL
                tryCatch({
                    if (!is.null(hsroc_summary) && "coefficients" %in% names(hsroc_summary)) {
                        coefficients <- hsroc_summary$coefficients
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

                # Try to convert to matrix
                coef_matrix <- tryCatch({
                    if (is.matrix(coefficients)) {
                        coefficients
                    } else if (is.data.frame(coefficients)) {
                        as.matrix(coefficients)
                    } else if (is.vector(coefficients)) {
                        matrix(coefficients, nrow = length(coefficients), ncol = 1)
                    } else {
                        stop("Unknown coefficient structure")
                    }
                }, error = function(e) {
                    self$results$hsrocresults$setNote("matrix_conversion_error", paste("Cannot convert coefficients to matrix:", e$message))
                    return(NULL)
                })

                if (is.null(coef_matrix)) {
                    return()
                }

                # Validate matrix dimensions
                if (nrow(coef_matrix) == 0) {
                    self$results$hsrocresults$setNote("empty_matrix", "Coefficient matrix has no rows")
                    return()
                }

                # Define parameter labels for clarity
                param_labels <- list(
                    "meanmu" = "Mean Threshold (μ)",
                    "meanpsi" = "Mean Accuracy (ψ)",
                    "hetaccuracy" = "Between-Study SD in Accuracy",
                    "hetthreshold" = "Between-Study SD in Threshold"
                )

                # Get coefficient names safely
                coef_names <- rownames(coef_matrix)
                if (is.null(coef_names) || length(coef_names) == 0) {
                    # Use default names if missing
                    default_names <- c("meanmu", "meanpsi", "hetaccuracy", "hetthreshold")
                    coef_names <- default_names[seq_len(min(nrow(coef_matrix), length(default_names)))]
                    if (length(coef_names) < nrow(coef_matrix)) {
                        coef_names <- c(coef_names, paste0("param", seq_len(nrow(coef_matrix) - length(coef_names))))
                    }
                }

                # Extract coefficients safely
                for (i in seq_len(nrow(coef_matrix))) {
                    param_name <- coef_names[i] %||% paste0("param", i)
                    display_name <- param_labels[[param_name]] %||% param_name

                    # Use position-based access with extensive validation
                    estimate <- tryCatch({
                        val <- coef_matrix[i, 1]
                        if (is.finite(val)) val else 0
                    }, error = function(e) 0)

                    std_error <- tryCatch({
                        if (ncol(coef_matrix) >= 2) {
                            val <- coef_matrix[i, 2]
                            if (is.finite(val)) val else NA
                        } else {
                            NA
                        }
                    }, error = function(e) NA)

                    z_value <- tryCatch({
                        if (ncol(coef_matrix) >= 3) {
                            val <- coef_matrix[i, 3]
                            if (is.finite(val)) val else NA
                        } else {
                            NA
                        }
                    }, error = function(e) NA)

                    p_value <- tryCatch({
                        if (ncol(coef_matrix) >= 4) {
                            val <- coef_matrix[i, 4]
                            if (is.finite(val)) val else NA
                        } else {
                            NA
                        }
                    }, error = function(e) NA)

                    # Add row to table with error handling
                    tryCatch({
                        hsroc_table$addRow(rowKey = param_name, values = list(
                            parameter = display_name,
                            estimate = estimate,
                            std_error = std_error,
                            z_value = z_value,
                            p_value = p_value
                        ))
                    }, error = function(e) {
                        # If adding row fails, note it but continue
                        self$results$hsrocresults$setNote("row_error", paste("Error adding parameter", param_name, ":", e$message))
                    })
                }

                # Add derived measures if coefficients are available
                tryCatch({
                    coef_row_names <- rownames(coef_matrix)
                    if (!is.null(coef_row_names)) {
                        # Lambda (diagnostic accuracy parameter)
                        if ("meanpsi" %in% coef_row_names) {
                            psi_idx <- which(coef_row_names == "meanpsi")
                            if (length(psi_idx) > 0 && psi_idx <= nrow(coef_matrix)) {
                                psi_val <- coef_matrix[psi_idx[1], 1]
                                if (is.finite(psi_val)) {
                                    lambda <- exp(psi_val)
                                    hsroc_table$addRow(rowKey = "lambda", values = list(
                                        parameter = "Lambda (Diagnostic Accuracy)",
                                        estimate = lambda,
                                        std_error = NA,
                                        z_value = NA,
                                        p_value = NA
                                    ))
                                }
                            }
                        }

                        # Theta (threshold parameter)
                        if ("meanmu" %in% coef_row_names) {
                            mu_idx <- which(coef_row_names == "meanmu")
                            if (length(mu_idx) > 0 && mu_idx <= nrow(coef_matrix)) {
                                mu_val <- coef_matrix[mu_idx[1], 1]
                                if (is.finite(mu_val)) {
                                    theta <- exp(mu_val)
                                    hsroc_table$addRow(rowKey = "theta", values = list(
                                        parameter = "Theta (Threshold)",
                                        estimate = theta,
                                        std_error = NA,
                                        z_value = NA,
                                        p_value = NA
                                    ))
                                }
                            }
                        }
                    }
                }, error = function(e) {
                    # Don't fail if derived measures can't be computed
                    self$results$hsrocresults$setNote("derived_error", "Could not compute derived measures")
                })
            }
        },
        
        .performHeterogeneityAnalysis = function(meta_data) {

            # Perform separate meta-analyses for sensitivity and specificity
            if (requireNamespace("metafor", quietly = TRUE)) {

                # Calculate sensitivity and specificity for each study
                meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
                meta_data$spec <- meta_data$tn / (meta_data$tn + meta_data$fp)

                # Calculate logit transformed sensitivity and specificity
                meta_data$logit_sens <- qlogis(meta_data$sens)
                meta_data$logit_spec <- qlogis(meta_data$spec)
                
                # Calculate sampling variances
                meta_data$var_logit_sens <- 1/(meta_data$tp) + 1/(meta_data$fn)
                meta_data$var_logit_spec <- 1/(meta_data$tn) + 1/(meta_data$fp)
                
                # Meta-analysis for sensitivity
                sens_meta <- metafor::rma(yi = logit_sens, vi = var_logit_sens, 
                                        data = meta_data, method = "REML")
                
                # Meta-analysis for specificity
                spec_meta <- metafor::rma(yi = logit_spec, vi = var_logit_spec, 
                                        data = meta_data, method = "REML")
                
                # Populate heterogeneity table
                het_table <- self$results$heterogeneity
                
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
        
        .performMetaRegression = function(meta_data) {

            covariate_var <- self$options$covariate
            if (is.null(covariate_var)) return()

            # Add covariate to meta_data
            meta_data$covariate <- self$data[[covariate_var]]

            if (requireNamespace("metafor", quietly = TRUE)) {

                # Calculate sensitivity and specificity for each study
                meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
                meta_data$spec <- meta_data$tn / (meta_data$tn + meta_data$fp)

                # Calculate logit transformed sensitivity and specificity
                meta_data$logit_sens <- qlogis(meta_data$sens)
                meta_data$logit_spec <- qlogis(meta_data$spec)

                # Calculate sampling variances
                meta_data$var_logit_sens <- 1/(meta_data$tp) + 1/(meta_data$fn)
                meta_data$var_logit_spec <- 1/(meta_data$tn) + 1/(meta_data$fp)

                # Meta-regression for sensitivity
                sens_metareg <- metafor::rma(yi = logit_sens, vi = var_logit_sens,
                                           mods = ~ covariate, data = meta_data, method = "REML")
                
                # Meta-regression for specificity
                spec_metareg <- metafor::rma(yi = logit_spec, vi = var_logit_spec, 
                                           mods = ~ covariate, data = meta_data, method = "REML")
                
                # Populate meta-regression table
                metareg_table <- self$results$metaregression
                
                metareg_table$addRow(rowKey = "sens_intercept", values = list(
                    measure = "Sensitivity",
                    parameter = "Intercept",
                    estimate = sens_metareg$beta[1],
                    std_error = sens_metareg$se[1],
                    z_value = sens_metareg$zval[1],
                    p_value = sens_metareg$pval[1]
                ))
                
                metareg_table$addRow(rowKey = "sens_covariate", values = list(
                    measure = "Sensitivity",
                    parameter = covariate_var,
                    estimate = sens_metareg$beta[2],
                    std_error = sens_metareg$se[2],
                    z_value = sens_metareg$zval[2],
                    p_value = sens_metareg$pval[2]
                ))
                
                metareg_table$addRow(rowKey = "spec_intercept", values = list(
                    measure = "Specificity",
                    parameter = "Intercept",
                    estimate = spec_metareg$beta[1],
                    std_error = spec_metareg$se[1],
                    z_value = spec_metareg$zval[1],
                    p_value = spec_metareg$pval[1]
                ))
                
                metareg_table$addRow(rowKey = "spec_covariate", values = list(
                    measure = "Specificity",
                    parameter = covariate_var,
                    estimate = spec_metareg$beta[2],
                    std_error = spec_metareg$se[2],
                    z_value = spec_metareg$zval[2],
                    p_value = spec_metareg$pval[2]
                ))
            }
        },
        
        .performPublicationBiasAssessment = function(meta_data) {
            
            if (requireNamespace("metafor", quietly = TRUE)) {
                
                # Calculate effective sample sizes
                meta_data$n_total <- meta_data$tp + meta_data$fp + meta_data$fn + meta_data$tn
                
                # Deeks' funnel plot asymmetry test
                meta_data$log_dor <- log((meta_data$tp * meta_data$tn) / (meta_data$fp * meta_data$fn))
                meta_data$se_log_dor <- sqrt(1/meta_data$tp + 1/meta_data$fp + 1/meta_data$fn + 1/meta_data$tn)
                
                # Regression test for asymmetry
                deeks_test <- metafor::rma(yi = log_dor, vi = se_log_dor^2, 
                                         mods = ~ I(1/sqrt(n_total)), data = meta_data, method = "FE")
                
                # Populate publication bias table
                bias_table <- self$results$publicationbias
                
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
            image$setState(meta_data)
        },
        
        .forestplot = function(image, ggtheme, theme, ...) {

            meta_data <- image$state

            # Validate meta_data
            if (is.null(meta_data) || !is.data.frame(meta_data) || nrow(meta_data) == 0) {
                return(FALSE)
            }

            if (requireNamespace("mada", quietly = TRUE) &&
                requireNamespace("ggplot2", quietly = TRUE)) {

                # Ensure meta_data is a proper data frame
                meta_data <- as.data.frame(meta_data)
                
                # Create forest plot for sensitivity
                meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
                meta_data$spec <- meta_data$tn / (meta_data$tn + meta_data$fp)
                
                # Calculate confidence intervals for sensitivity
                alpha <- 1 - self$options$confidence_level
                z_crit <- qnorm(1 - alpha/2)
                meta_data$sens_ci_lower <- with(meta_data,
                    pmax(0, sens - z_crit * sqrt(sens * (1-sens) / (tp + fn))))
                meta_data$sens_ci_upper <- with(meta_data,
                    pmin(1, sens + z_crit * sqrt(sens * (1-sens) / (tp + fn))))
                
                # Create forest plot
                p <- ggplot2::ggplot(meta_data, ggplot2::aes(x = sens, y = reorder(study, sens))) +
                    ggplot2::geom_point(size = 3, color = "darkblue") +
                    ggplot2::geom_errorbarh(ggplot2::aes(xmin = sens_ci_lower, xmax = sens_ci_upper), 
                                          height = 0.2, color = "darkblue") +
                    ggplot2::labs(
                        title = "Forest Plot: Sensitivity by Study",
                        x = "Sensitivity",
                        y = "Study"
                    ) +
                    ggplot2::xlim(0, 1) +
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
        
        .populateSROCPlot = function(meta_data) {
            
            image <- self$results$srocplot
            image$setState(meta_data)
        },
        
        .srocplot = function(image, ggtheme, theme, ...) {

            meta_data <- image$state

            # Validate meta_data
            if (is.null(meta_data) || !is.data.frame(meta_data) || nrow(meta_data) == 0) {
                return(FALSE)
            }

            if (requireNamespace("mada", quietly = TRUE) &&
                requireNamespace("ggplot2", quietly = TRUE)) {

                # Ensure meta_data is a proper data frame
                meta_data <- as.data.frame(meta_data)
                
                # Calculate sensitivity and specificity
                meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
                meta_data$fpr <- meta_data$fp / (meta_data$fp + meta_data$tn)
                
                # Create SROC plot
                p <- ggplot2::ggplot(meta_data, ggplot2::aes(x = fpr, y = sens)) +
                    ggplot2::geom_point(size = 3, alpha = 0.7, color = "darkred") +
                    ggplot2::geom_abline(intercept = 1, slope = -1, linetype = "dashed", 
                                       color = "gray50", alpha = 0.5) +
                    ggplot2::labs(
                        title = "Summary ROC Plot",
                        x = "False Positive Rate (1 - Specificity)",
                        y = "True Positive Rate (Sensitivity)"
                    ) +
                    ggplot2::xlim(0, 1) +
                    ggplot2::ylim(0, 1) +
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
                
                # Create funnel plot
                p <- ggplot2::ggplot(meta_data, ggplot2::aes(x = log_dor, y = 1/se_log_dor)) +
                    ggplot2::geom_point(size = 3, alpha = 0.7, color = "darkgreen") +
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

        # Enhanced data validation with user-friendly warnings
        .validateStudyData = function(meta_data, original_n) {

            # Check for data removal
            if (original_n > nrow(meta_data)) {
                removed_n <- original_n - nrow(meta_data)
                warning_msg <- sprintf(
                    "<div class='alert alert-warning'>
                    <h4>⚠️ Data Cleaning Notice</h4>
                    <p>%d studies removed due to missing values. %d studies remain for analysis.</p>
                    </div>",
                    removed_n, nrow(meta_data)
                )
                self$results$instructions$setContent(warning_msg)
            }

            # Check minimum study requirement
            if (nrow(meta_data) < 3) {
                error_msg <- sprintf(
                    "<div class='alert alert-danger'>
                    <h4>❌ Insufficient Data</h4>
                    <p>Meta-analysis requires at least 3 studies with complete data.</p>
                    <p>You have provided: <strong>%d studies</strong></p>
                    <p><strong>Solution:</strong> Add more studies or use individual study analysis instead.</p>
                    </div>",
                    nrow(meta_data)
                )
                self$results$instructions$setContent(error_msg)
                return(FALSE)
            }

            # Check for zero cells
            zero_cells <- any(meta_data$tp == 0 | meta_data$fp == 0 |
                            meta_data$fn == 0 | meta_data$tn == 0)
            if (zero_cells) {
                warning_msg <- "<div class='alert alert-info'>
                    <h4>ℹ️ Zero Cell Warning</h4>
                    <p>Some studies have zero values in their 2×2 tables.</p>
                    <p>Continuity correction (0.5) will be applied automatically for analysis stability.</p>
                    </div>"
                # Don't overwrite previous messages, append
                current <- self$results$instructions$content
                if (!is.null(current)) {
                    self$results$instructions$setContent(paste(current, warning_msg))
                } else {
                    self$results$instructions$setContent(warning_msg)
                }
            }

            # Check for extreme values
            very_small <- any(meta_data$tp + meta_data$fn < 10 |
                            meta_data$fp + meta_data$tn < 10)
            if (very_small) {
                info_msg <- "<div class='alert alert-info'>
                    <h4>ℹ️ Small Sample Notice</h4>
                    <p>Some studies have very small sample sizes (< 10 in disease or healthy groups).</p>
                    <p>Results may have wide confidence intervals. Consider interpreting with caution.</p>
                    </div>"
                current <- self$results$instructions$content
                if (!is.null(current)) {
                    self$results$instructions$setContent(paste(current, info_msg))
                }
            }

            return(TRUE)
        },

        # Generate natural language summary
        .generateSummary = function(meta_data) {

            if (is.null(private$.pooled_sensitivity) || is.null(private$.pooled_specificity)) {
                return()
            }

            # Calculate confidence intervals (these should be set by bivariate analysis)
            sens_pct <- round(private$.pooled_sensitivity * 100, 1)
            spec_pct <- round(private$.pooled_specificity * 100, 1)

            # Calculate positive and negative likelihood ratios
            lr_pos <- private$.pooled_sensitivity / (1 - private$.pooled_specificity)
            lr_neg <- (1 - private$.pooled_sensitivity) / private$.pooled_specificity

            summary_html <- sprintf("
            <div class='analysis-summary' style='background-color: #e8f4f8; padding: 20px; border-radius: 8px; margin: 10px 0;'>
                <h4>📊 Meta-Analysis Summary</h4>
                <p><strong>Analysis Type:</strong> Diagnostic test accuracy meta-analysis of %d studies</p>

                <div style='background-color: white; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>🎯 Pooled Test Performance</h5>
                    <p><strong>Sensitivity:</strong> %.1f%% - The test correctly identifies %.0f out of 100 patients with disease</p>
                    <p><strong>Specificity:</strong> %.1f%% - The test correctly identifies %.0f out of 100 healthy individuals</p>
                </div>

                <div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>📈 Clinical Decision Metrics</h5>
                    <p><strong>Positive Likelihood Ratio:</strong> %.2f - A positive test is %.1fx more likely in disease than healthy</p>
                    <p><strong>Negative Likelihood Ratio:</strong> %.2f - A negative test is %.1fx more likely in healthy than disease</p>
                </div>

                <div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>💡 Interpretation Guide</h5>
                    <p>%s</p>
                </div>

                <div style='margin-top: 15px;'>
                    <button onclick='navigator.clipboard.writeText(this.getAttribute(\"data-text\"))'
                            data-text='Meta-analysis of %d diagnostic accuracy studies shows pooled sensitivity of %.1f%% and specificity of %.1f%%, with positive LR of %.2f and negative LR of %.2f.'
                            style='background-color: #007bff; color: white; border: none; padding: 8px 16px; border-radius: 4px; cursor: pointer;'>
                        📋 Copy Summary to Clipboard
                    </button>
                </div>
            </div>
            ",
            private$.n_studies,
            sens_pct, sens_pct,
            spec_pct, spec_pct,
            lr_pos, lr_pos,
            lr_neg, 1/lr_neg,
            private$.getInterpretationText(sens_pct, spec_pct, lr_pos, lr_neg),
            private$.n_studies, sens_pct, spec_pct, lr_pos, lr_neg
            )

            self$results$summary$setContent(summary_html)
        },

        # Helper function for interpretation text
        .getInterpretationText = function(sens, spec, lr_pos, lr_neg) {
            if (sens >= 90 && spec >= 90) {
                return("Excellent diagnostic accuracy - suitable for both ruling in and ruling out disease.")
            } else if (sens >= 90) {
                return("High sensitivity - excellent for ruling out disease when test is negative (SnNout).")
            } else if (spec >= 90) {
                return("High specificity - excellent for ruling in disease when test is positive (SpPin).")
            } else if (sens >= 80 && spec >= 80) {
                return("Good overall diagnostic accuracy - useful for clinical decision making.")
            } else {
                return("Moderate diagnostic accuracy - consider using in combination with other clinical information.")
            }
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
        .prepareMADAData = function(meta_data) {

            # Check cache validity
            if (private$.data_cache_valid && !is.null(private$.mada_data)) {
                return(private$.mada_data)
            }

            # Create data frame with uppercase column names for mada package
            private$.mada_data <- data.frame(
                TP = meta_data$tp,
                FP = meta_data$fp,
                FN = meta_data$fn,
                TN = meta_data$tn
            )

            # Remove rows with NA values and ensure positive counts
            private$.mada_data <- private$.mada_data[complete.cases(private$.mada_data), ]
            private$.mada_data <- private$.mada_data[
                private$.mada_data$TP >= 0 & private$.mada_data$FP >= 0 &
                private$.mada_data$FN >= 0 & private$.mada_data$TN >= 0,
            ]

            # Apply continuity correction for zero cells
            if (any(private$.mada_data == 0)) {
                private$.mada_data <- private$.mada_data + 0.5
            }

            private$.data_cache_valid <- TRUE
            return(private$.mada_data)
        }
    )
)