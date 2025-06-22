#' @title Cox Proportional Hazards Model Diagnostics
#' @description
#' Comprehensive Cox proportional hazards model diagnostic plots using ggcoxdiagnostics
#' from the survminer package. This module provides essential model validation tools
#' for survival analysis, including residual analysis, proportional hazards assumption
#' testing, and influence diagnostics.
#'
#' @details
#' This module implements the diagnostic capabilities requested in GitHub Issue #61,
#' providing comprehensive model validation for Cox regression analysis. The diagnostic
#' plots help identify model violations, influential observations, and assess overall
#' model fit quality essential for clinical research applications.
#'
#' Key diagnostic plot types:
#' - Martingale residuals: Detect non-linear relationships and outliers
#' - Deviance residuals: Identify poorly fitted observations  
#' - Score residuals: Assess influential observations
#' - Schoenfeld residuals: Test proportional hazards assumption
#' - DFBeta plots: Evaluate influence of individual observations
#'
#' @param data A data frame containing survival data.
#' @param time The time-to-event variable.
#' @param event The event indicator variable (1=event, 0=censored).
#' @param covariates Variables to include as covariates in the Cox model.
#' @param strata_var Optional stratification variable.
#' @param show_martingale Whether to show martingale residual plots.
#' @param show_deviance Whether to show deviance residual plots.
#' @param show_score Whether to show score residual plots.
#' @param show_schoenfeld Whether to show Schoenfeld residual plots.
#' @param show_dfbeta Whether to show DFBeta influence plots.
#'
#' @return Comprehensive Cox model diagnostic plots and statistical tests.
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom survival coxph Surv cox.zph
#' @importFrom survminer ggcoxdiagnostics ggcoxzph
#' @importFrom dplyr select all_of
#' @importFrom janitor clean_names
#' @importFrom labelled set_variable_labels var_label
#' @import magrittr
#'

coxdiagnosticsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "coxdiagnosticsClass",
    inherit = coxdiagnosticsBase,
    private = list(
        
        # Internal data storage
        .processed_data = NULL,
        .cox_model = NULL,
        .zph_test = NULL,
        
        .init = function() {
            # Initialize instructions
            instructions_html <- paste(
                "<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                "<h3 style='color: #1565c0; margin-top: 0;'>Cox Proportional Hazards Model Diagnostics</h3>",
                "<div style='margin: 10px 0;'>",
                "<h4 style='color: #1976d2; margin: 10px 0 5px 0;'>Essential Model Validation:</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Martingale Residuals:</strong> Detect non-linear relationships and outliers</li>",
                "<li><strong>Deviance Residuals:</strong> Identify poorly fitted observations</li>",
                "<li><strong>Score Residuals:</strong> Assess influential observations</li>",
                "<li><strong>Schoenfeld Residuals:</strong> Test proportional hazards assumption</li>",
                "<li><strong>DFBeta Plots:</strong> Evaluate influence of individual observations</li>",
                "</ul>",
                "</div>",
                "<div style='margin: 10px 0;'>",
                "<h4 style='color: #1976d2; margin: 10px 0 5px 0;'>Quick Start:</h4>",
                "<ol style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Select Time Variable:</strong> Choose survival time variable</li>",
                "<li><strong>Select Event Variable:</strong> Choose event indicator (1=event, 0=censored)</li>",
                "<li><strong>Select Covariates:</strong> Choose variables for Cox model</li>",
                "<li><strong>Choose Diagnostics:</strong> Select which diagnostic plots to generate</li>",
                "<li><strong>Review Results:</strong> Examine plots and statistical tests</li>",
                "</ol>",
                "</div>",
                "<div style='background-color: #fff3e0; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                "<p style='margin: 0; color: #ef6c00;'><strong>Clinical Note:</strong> These diagnostics are essential for validating Cox model assumptions in clinical research. Violations may require model adjustments or alternative analytical approaches.</p>",
                "</div>",
                "<p style='margin: 10px 0 0 0; color: #666; font-style: italic;'>💡 This module implements the diagnostic capabilities from GitHub Issue #61 using survminer's ggcoxdiagnostics.</p>",
                "</div>"
            )
            
            self$results$instructions$setContent(instructions_html)
        },
        
        .run = function() {
            # Clear instructions if analysis is ready
            if (!is.null(self$options$time) && !is.null(self$options$event) && !is.null(self$options$covariates)) {
                self$results$instructions$setContent("")
            }
            
            # Early validation
            if (is.null(self$options$time)) {
                return()
            }
            
            if (is.null(self$options$event)) {
                return()
            }
            
            if (is.null(self$options$covariates) || length(self$options$covariates) == 0) {
                return()
            }
            
            # Data validation
            if (nrow(self$data) == 0) {
                stop("Data contains no (complete) rows")
            }
            
            # Package requirements check
            if (!requireNamespace("survival", quietly = TRUE)) {
                error_msg <- "Package 'survival' is required for Cox regression analysis."
                self$results$model_summary$setContent(paste("<p style='color: red;'>", error_msg, "</p>"))
                return()
            }
            
            if (!requireNamespace("survminer", quietly = TRUE)) {
                error_msg <- "Package 'survminer' is required for Cox diagnostic plots."
                self$results$model_summary$setContent(paste("<p style='color: red;'>", error_msg, "</p>"))
                return()
            }
            
            # Process data
            private$.processed_data <- private$.process_data()
            
            # Fit Cox model
            private$.fit_cox_model()
            
            # Generate model summary
            if (self$options$show_model_summary) {
                private$.generate_model_summary()
            }
            
            # Generate proportional hazards test
            if (self$options$show_ph_test) {
                private$.generate_ph_test()
            }
            
            # Set plot states for rendering
            if (self$options$show_martingale) {
                self$results$martingale_plot$setState(private$.cox_model)
            }
            
            if (self$options$show_deviance) {
                self$results$deviance_plot$setState(private$.cox_model)
            }
            
            if (self$options$show_score) {
                self$results$score_plot$setState(private$.cox_model)
            }
            
            if (self$options$show_schoenfeld) {
                self$results$schoenfeld_plot$setState(private$.cox_model)
            }
            
            if (self$options$show_dfbeta) {
                self$results$dfbeta_plot$setState(private$.cox_model)
            }
            
            # Generate interpretation
            if (self$options$show_interpretation) {
                private$.generate_interpretation()
            }
        },
        
        .process_data = function() {
            mydata <- self$data
            
            # Store original names and labels
            original_names <- names(mydata)
            labels <- setNames(original_names, original_names)
            
            # Clean variable names
            mydata <- mydata %>% janitor::clean_names()
            
            # Restore labels to cleaned names
            corrected_labels <- setNames(original_names, names(mydata))
            mydata <- labelled::set_variable_labels(.data = mydata, .labels = corrected_labels)
            
            # Handle missing values if requested
            if (self$options$exclude_missing) {
                selected_vars <- c(self$options$time, self$options$event, self$options$covariates)
                if (!is.null(self$options$strata_var)) {
                    selected_vars <- c(selected_vars, self$options$strata_var)
                }
                # Convert to cleaned names
                selected_vars_clean <- janitor::make_clean_names(selected_vars)
                mydata <- mydata[complete.cases(mydata[selected_vars_clean]), ]
            }
            
            return(mydata)
        },
        
        .fit_cox_model = function() {
            mydata <- private$.processed_data
            
            # Convert variable names to cleaned versions
            time_clean <- janitor::make_clean_names(self$options$time)
            event_clean <- janitor::make_clean_names(self$options$event)
            covariates_clean <- janitor::make_clean_names(self$options$covariates)
            
            tryCatch({
                # Build formula
                if (!is.null(self$options$strata_var)) {
                    strata_clean <- janitor::make_clean_names(self$options$strata_var)
                    formula_str <- paste0("Surv(", time_clean, ", ", event_clean, ") ~ ",
                                        paste(covariates_clean, collapse = " + "),
                                        " + strata(", strata_clean, ")")
                } else {
                    formula_str <- paste0("Surv(", time_clean, ", ", event_clean, ") ~ ",
                                        paste(covariates_clean, collapse = " + "))
                }
                
                # Fit Cox model
                formula_obj <- as.formula(formula_str)
                private$.cox_model <- survival::coxph(formula_obj, data = mydata)
                
                # Generate proportional hazards test
                private$.zph_test <- survival::cox.zph(private$.cox_model)
                
            }, error = function(e) {
                error_msg <- paste("Error fitting Cox model:", e$message)
                self$results$model_summary$setContent(paste("<p style='color: red;'>", error_msg, "</p>"))
            })
        },
        
        .generate_model_summary = function() {
            if (is.null(private$.cox_model)) return()
            
            tryCatch({
                cox_summary <- summary(private$.cox_model)
                
                # Create HTML summary
                summary_html <- "<h4>Cox Proportional Hazards Model Summary</h4>"
                summary_html <- paste0(summary_html, "<div style='background-color: #f5f5f5; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
                
                # Model information
                summary_html <- paste0(summary_html, "<p><strong>Model Formula:</strong> ", deparse(private$.cox_model$formula), "</p>")
                summary_html <- paste0(summary_html, "<p><strong>Number of observations:</strong> ", private$.cox_model$n, "</p>")
                summary_html <- paste0(summary_html, "<p><strong>Number of events:</strong> ", private$.cox_model$nevent, "</p>")
                
                # Likelihood ratio test
                summary_html <- paste0(summary_html, "<p><strong>Likelihood ratio test:</strong> ")
                summary_html <- paste0(summary_html, "χ² = ", round(cox_summary$logtest["test"], 3), 
                                     ", df = ", cox_summary$logtest["df"], 
                                     ", p = ", format.pval(cox_summary$logtest["pvalue"], digits = 3))
                summary_html <- paste0(summary_html, "</p>")
                
                # Concordance
                if (!is.null(cox_summary$concordance)) {
                    summary_html <- paste0(summary_html, "<p><strong>Concordance:</strong> ", 
                                         round(cox_summary$concordance[1], 3), 
                                         " (SE = ", round(cox_summary$concordance[2], 3), ")</p>")
                }
                
                summary_html <- paste0(summary_html, "</div>")
                
                # Coefficients table
                if (nrow(cox_summary$coefficients) > 0) {
                    summary_html <- paste0(summary_html, "<h5>Model Coefficients</h5>")
                    summary_html <- paste0(summary_html, "<table class='table table-striped' style='margin: 10px 0; max-width: 800px;'>")
                    summary_html <- paste0(summary_html, "<thead><tr>")
                    summary_html <- paste0(summary_html, "<th>Variable</th><th>Coef</th><th>Exp(coef)</th><th>SE(coef)</th><th>z</th><th>Pr(>|z|)</th>")
                    summary_html <- paste0(summary_html, "</tr></thead><tbody>")
                    
                    for (i in 1:nrow(cox_summary$coefficients)) {
                        row_name <- rownames(cox_summary$coefficients)[i]
                        coef <- round(cox_summary$coefficients[i, "coef"], 3)
                        exp_coef <- round(cox_summary$coefficients[i, "exp(coef)"], 3)
                        se_coef <- round(cox_summary$coefficients[i, "se(coef)"], 3)
                        z_val <- round(cox_summary$coefficients[i, "z"], 3)
                        p_val <- format.pval(cox_summary$coefficients[i, "Pr(>|z|)"], digits = 3)
                        
                        summary_html <- paste0(summary_html, "<tr>")
                        summary_html <- paste0(summary_html, "<td><strong>", row_name, "</strong></td>")
                        summary_html <- paste0(summary_html, "<td>", coef, "</td>")
                        summary_html <- paste0(summary_html, "<td>", exp_coef, "</td>")
                        summary_html <- paste0(summary_html, "<td>", se_coef, "</td>")
                        summary_html <- paste0(summary_html, "<td>", z_val, "</td>")
                        summary_html <- paste0(summary_html, "<td>", p_val, "</td>")
                        summary_html <- paste0(summary_html, "</tr>")
                    }
                    
                    summary_html <- paste0(summary_html, "</tbody></table>")
                }
                
                self$results$model_summary$setContent(summary_html)
                
            }, error = function(e) {
                error_msg <- paste("Error generating model summary:", e$message)
                self$results$model_summary$setContent(paste("<p style='color: red;'>", error_msg, "</p>"))
            })
        },
        
        .generate_ph_test = function() {
            if (is.null(private$.zph_test)) return()
            
            tryCatch({
                zph <- private$.zph_test
                
                # Create HTML for proportional hazards test
                ph_html <- "<h4>Proportional Hazards Assumption Test</h4>"
                ph_html <- paste0(ph_html, "<div style='background-color: #fff3e0; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
                
                ph_html <- paste0(ph_html, "<p><strong>Schoenfeld Residuals Test (cox.zph):</strong></p>")
                ph_html <- paste0(ph_html, "<p>Tests the null hypothesis that hazards are proportional over time.</p>")
                
                # Test results table
                ph_html <- paste0(ph_html, "<table class='table table-striped' style='margin: 10px 0; max-width: 600px;'>")
                ph_html <- paste0(ph_html, "<thead><tr><th>Variable</th><th>Chi-square</th><th>df</th><th>p-value</th></tr></thead><tbody>")
                
                for (i in 1:nrow(zph$table)) {
                    var_name <- rownames(zph$table)[i]
                    chisq <- round(zph$table[i, "chisq"], 3)
                    df <- zph$table[i, "df"]
                    p_val <- format.pval(zph$table[i, "p"], digits = 3)
                    
                    # Color code p-values
                    p_color <- if (zph$table[i, "p"] < 0.05) "color: red; font-weight: bold;" else "color: green;"
                    
                    ph_html <- paste0(ph_html, "<tr>")
                    ph_html <- paste0(ph_html, "<td><strong>", var_name, "</strong></td>")
                    ph_html <- paste0(ph_html, "<td>", chisq, "</td>")
                    ph_html <- paste0(ph_html, "<td>", df, "</td>")
                    ph_html <- paste0(ph_html, "<td style='", p_color, "'>", p_val, "</td>")
                    ph_html <- paste0(ph_html, "</tr>")
                }
                
                ph_html <- paste0(ph_html, "</tbody></table>")
                
                # Interpretation
                any_violation <- any(zph$table[, "p"] < 0.05, na.rm = TRUE)
                if (any_violation) {
                    ph_html <- paste0(ph_html, "<p style='color: red; font-weight: bold;'>⚠️ Warning: Proportional hazards assumption may be violated (p < 0.05).</p>")
                    ph_html <- paste0(ph_html, "<p>Consider stratification, time-dependent covariates, or alternative models.</p>")
                } else {
                    ph_html <- paste0(ph_html, "<p style='color: green; font-weight: bold;'>✅ Proportional hazards assumption appears to hold (all p ≥ 0.05).</p>")
                }
                
                ph_html <- paste0(ph_html, "</div>")
                
                self$results$ph_test_results$setContent(ph_html)
                
            }, error = function(e) {
                error_msg <- paste("Error generating proportional hazards test:", e$message)
                self$results$ph_test_results$setContent(paste("<p style='color: red;'>", error_msg, "</p>"))
            })
        },
        
        .generate_interpretation = function() {
            interp_html <- "<h4>Cox Model Diagnostic Interpretation Guide</h4>"
            interp_html <- paste0(interp_html, "<div style='background-color: #f3e5f5; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
            
            interp_html <- paste0(interp_html, "<h5>Diagnostic Plot Interpretation:</h5>")
            
            interp_html <- paste0(interp_html, "<div style='margin: 10px 0;'>")
            interp_html <- paste0(interp_html, "<h6 style='color: #7b1fa2;'>Martingale Residuals:</h6>")
            interp_html <- paste0(interp_html, "<ul style='margin: 5px 0; padding-left: 20px;'>")
            interp_html <- paste0(interp_html, "<li>Should be randomly scattered around zero</li>")
            interp_html <- paste0(interp_html, "<li>Patterns suggest non-linear relationships</li>")
            interp_html <- paste0(interp_html, "<li>Extreme values indicate potential outliers</li>")
            interp_html <- paste0(interp_html, "</ul>")
            interp_html <- paste0(interp_html, "</div>")
            
            interp_html <- paste0(interp_html, "<div style='margin: 10px 0;'>")
            interp_html <- paste0(interp_html, "<h6 style='color: #7b1fa2;'>Deviance Residuals:</h6>")
            interp_html <- paste0(interp_html, "<ul style='margin: 5px 0; padding-left: 20px;'>")
            interp_html <- paste0(interp_html, "<li>More symmetric than martingale residuals</li>")
            interp_html <- paste0(interp_html, "<li>Values > 2 or < -2 indicate poorly fitted observations</li>")
            interp_html <- paste0(interp_html, "<li>Should be approximately normally distributed</li>")
            interp_html <- paste0(interp_html, "</ul>")
            interp_html <- paste0(interp_html, "</div>")
            
            interp_html <- paste0(interp_html, "<div style='margin: 10px 0;'>")
            interp_html <- paste0(interp_html, "<h6 style='color: #7b1fa2;'>Schoenfeld Residuals:</h6>")
            interp_html <- paste0(interp_html, "<ul style='margin: 5px 0; padding-left: 20px;'>")
            interp_html <- paste0(interp_html, "<li>Test proportional hazards assumption</li>")
            interp_html <- paste0(interp_html, "<li>Should show no trend over time</li>")
            interp_html <- paste0(interp_html, "<li>Slopes indicate time-varying effects</li>")
            interp_html <- paste0(interp_html, "</ul>")
            interp_html <- paste0(interp_html, "</div>")
            
            interp_html <- paste0(interp_html, "<div style='margin: 10px 0;'>")
            interp_html <- paste0(interp_html, "<h6 style='color: #7b1fa2;'>DFBeta Plots:</h6>")
            interp_html <- paste0(interp_html, "<ul style='margin: 5px 0; padding-left: 20px;'>")
            interp_html <- paste0(interp_html, "<li>Assess influence of individual observations</li>")
            interp_html <- paste0(interp_html, "<li>Values > 2/√n suggest influential observations</li>")
            interp_html <- paste0(interp_html, "<li>Consider removing/investigating extreme values</li>")
            interp_html <- paste0(interp_html, "</ul>")
            interp_html <- paste0(interp_html, "</div>")
            
            interp_html <- paste0(interp_html, "<h5 style='color: #4a148c;'>Clinical Recommendations:</h5>")
            interp_html <- paste0(interp_html, "<ul style='margin: 5px 0; padding-left: 20px;'>")
            interp_html <- paste0(interp_html, "<li><strong>Model violations:</strong> Consider stratification or time-dependent covariates</li>")
            interp_html <- paste0(interp_html, "<li><strong>Outliers:</strong> Investigate extreme residuals for data quality</li>")
            interp_html <- paste0(interp_html, "<li><strong>Non-linearity:</strong> Consider splines or transformations</li>")
            interp_html <- paste0(interp_html, "<li><strong>Documentation:</strong> Report diagnostic results in publications</li>")
            interp_html <- paste0(interp_html, "</ul>")
            
            interp_html <- paste0(interp_html, "</div>")
            
            self$results$interpretation$setContent(interp_html)
        },
        
        # Plot rendering functions
        .plot_martingale = function(image, ggtheme, theme, ...) {
            if (!self$options$show_martingale) return()
            
            cox_model <- image$state
            if (is.null(cox_model)) return()
            
            tryCatch({
                plot <- survminer::ggcoxdiagnostics(
                    cox_model,
                    type = "martingale",
                    ox.scale = self$options$ox_scale,
                    point.size = self$options$point_size,
                    point.alpha = self$options$alpha_level
                )
                
                if (self$options$add_reference) {
                    plot <- plot + ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7)
                }
                
                plot <- plot + ggtheme
                plot <- plot + ggplot2::ggtitle("Martingale Residuals")
                
                print(plot)
                TRUE
            }, error = function(e) {
                # Fallback to basic plot if ggcoxdiagnostics fails
                warning("ggcoxdiagnostics failed, using basic residual plot")
                residuals_mart <- residuals(cox_model, type = "martingale")
                linear_pred <- predict(cox_model, type = "lp")
                
                plot_data <- data.frame(
                    x = linear_pred,
                    y = residuals_mart
                )
                
                plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
                    ggplot2::geom_point(size = self$options$point_size, alpha = self$options$alpha_level) +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                    ggplot2::labs(
                        title = "Martingale Residuals",
                        x = "Linear Predictor",
                        y = "Martingale Residuals"
                    ) +
                    ggtheme
                
                if (self$options$add_smooth) {
                    plot <- plot + ggplot2::geom_smooth(se = TRUE, color = "blue")
                }
                
                print(plot)
                TRUE
            })
        },
        
        .plot_deviance = function(image, ggtheme, theme, ...) {
            if (!self$options$show_deviance) return()
            
            cox_model <- image$state
            if (is.null(cox_model)) return()
            
            tryCatch({
                plot <- survminer::ggcoxdiagnostics(
                    cox_model,
                    type = "deviance",
                    ox.scale = self$options$ox_scale,
                    point.size = self$options$point_size,
                    point.alpha = self$options$alpha_level
                )
                
                if (self$options$add_reference) {
                    plot <- plot + ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7)
                }
                
                plot <- plot + ggtheme
                plot <- plot + ggplot2::ggtitle("Deviance Residuals")
                
                print(plot)
                TRUE
            }, error = function(e) {
                # Fallback plot
                residuals_dev <- residuals(cox_model, type = "deviance")
                linear_pred <- predict(cox_model, type = "lp")
                
                plot_data <- data.frame(
                    x = linear_pred,
                    y = residuals_dev
                )
                
                plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
                    ggplot2::geom_point(size = self$options$point_size, alpha = self$options$alpha_level) +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                    ggplot2::labs(
                        title = "Deviance Residuals",
                        x = "Linear Predictor", 
                        y = "Deviance Residuals"
                    ) +
                    ggtheme
                
                if (self$options$add_smooth) {
                    plot <- plot + ggplot2::geom_smooth(se = TRUE, color = "blue")
                }
                
                print(plot)
                TRUE
            })
        },
        
        .plot_score = function(image, ggtheme, theme, ...) {
            if (!self$options$show_score) return()
            
            cox_model <- image$state
            if (is.null(cox_model)) return()
            
            tryCatch({
                plot <- survminer::ggcoxdiagnostics(
                    cox_model,
                    type = "score",
                    ox.scale = self$options$ox_scale,
                    point.size = self$options$point_size,
                    point.alpha = self$options$alpha_level
                )
                
                plot <- plot + ggtheme
                plot <- plot + ggplot2::ggtitle("Score Residuals")
                
                print(plot)
                TRUE
            }, error = function(e) {
                # Score residuals fallback
                message("Score residuals plot not available with current data")
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                    label = "Score residuals plot not available\nwith current data", 
                                    hjust = 0.5, vjust = 0.5, size = 6) +
                    ggplot2::theme_void() +
                    ggtheme
                
                print(plot)
                TRUE
            })
        },
        
        .plot_schoenfeld = function(image, ggtheme, theme, ...) {
            if (!self$options$show_schoenfeld) return()
            
            cox_model <- image$state
            if (is.null(cox_model)) return()
            
            tryCatch({
                plot <- survminer::ggcoxdiagnostics(
                    cox_model,
                    type = "schoenfeld",
                    ox.scale = "time",
                    point.size = self$options$point_size,
                    point.alpha = self$options$alpha_level
                )
                
                plot <- plot + ggtheme
                plot <- plot + ggplot2::ggtitle("Schoenfeld Residuals")
                
                print(plot)
                TRUE
            }, error = function(e) {
                # Alternative: use ggcoxzph if available
                if (!is.null(private$.zph_test)) {
                    plot <- survminer::ggcoxzph(private$.zph_test)
                    plot <- plot + ggtheme
                    print(plot)
                    TRUE
                } else {
                    plot <- ggplot2::ggplot() +
                        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                        label = "Schoenfeld residuals plot not available", 
                                        hjust = 0.5, vjust = 0.5, size = 6) +
                        ggplot2::theme_void() +
                        ggtheme
                    print(plot)
                    TRUE
                }
            })
        },
        
        .plot_dfbeta = function(image, ggtheme, theme, ...) {
            if (!self$options$show_dfbeta) return()
            
            cox_model <- image$state
            if (is.null(cox_model)) return()
            
            tryCatch({
                plot <- survminer::ggcoxdiagnostics(
                    cox_model,
                    type = "dfbeta",
                    ox.scale = self$options$ox_scale,
                    point.size = self$options$point_size,
                    point.alpha = self$options$alpha_level
                )
                
                plot <- plot + ggtheme
                plot <- plot + ggplot2::ggtitle("DFBeta Influence Diagnostics")
                
                print(plot)
                TRUE
            }, error = function(e) {
                # DFBeta fallback
                message("DFBeta plot not available with current data")
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                    label = "DFBeta influence plot not available\nwith current data", 
                                    hjust = 0.5, vjust = 0.5, size = 6) +
                    ggplot2::theme_void() +
                    ggtheme
                
                print(plot)
                TRUE
            })
        }
    )
)