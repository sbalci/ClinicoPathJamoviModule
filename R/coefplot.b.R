#' @title Coefficient Plots
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr
#' @importFrom coefplot coefplot
#' @importFrom jtools plot_summs plot_coefs
#' @import ggplot2

coefplotClass <- if (requireNamespace("jmvcore")) {
    R6::R6Class(
        "coefplotClass",
        inherit = coefplotBase,
        private = list(

            # init ----
            .init = function() {
                # Initialize visibility
                if (!self$options$show_coefficient_plot) {
                    self$results$coefficient_plot$setVisible(FALSE)
                }
                
                if (!self$options$show_model_summary) {
                    self$results$model_summary$setVisible(FALSE)
                }
                
                if (!self$options$show_coefficient_table) {
                    self$results$coefficient_table$setVisible(FALSE)
                }
                
                # Adjust plot size based on number of variables
                if (!is.null(self$options$covs)) {
                    n_vars <- length(self$options$covs)
                    if (self$options$include_intercept) n_vars <- n_vars + 1
                    
                    height <- max(400, n_vars * 50 + 100)
                    self$results$coefficient_plot$setSize(700, height)
                }
            },

            # run ----
            .run = function() {
                
                # Check for required packages
                required_packages <- c("coefplot", "jtools", "ggplot2")
                for (pkg in required_packages) {
                    if (!requireNamespace(pkg, quietly = TRUE)) {
                        self$results$instructions$setContent(
                            paste0("<div style='color: red; font-weight: bold;'>
                            Error: The '", pkg, "' package is required but not installed.
                            <br><br>
                            Please install it using: install.packages('", pkg, "')
                            </div>")
                        )
                        return()
                    }
                }
                
                # Early return with instructions if no variables selected
                if (is.null(self$options$dep) || is.null(self$options$covs) || length(self$options$covs) == 0) {
                    instructions_html <- "
                    <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 10px 0;'>
                        <h3>Coefficient Plots for Regression Models</h3>
                        <p><strong>Create professional forest plots to visualize regression coefficients and confidence intervals.</strong></p>
                        
                        <h4>Features:</h4>
                        <ul>
                            <li><strong>Multiple Model Types:</strong> Linear, logistic, Cox, and Poisson regression</li>
                            <li><strong>Coefficient Visualization:</strong> Points with confidence intervals</li>
                            <li><strong>Model Comparison:</strong> Compare up to 3 models simultaneously</li>
                            <li><strong>Statistical Options:</strong> Standardization, robust standard errors</li>
                            <li><strong>Customization:</strong> Sorting, colors, layout options</li>
                        </ul>
                        
                        <h4>To Get Started:</h4>
                        <ol>
                            <li><strong>Select Dependent Variable:</strong> Choose your outcome variable</li>
                            <li><strong>Select Covariates:</strong> Choose predictor variables for the model</li>
                            <li><strong>Choose Model Type:</strong> Select appropriate regression type</li>
                            <li><strong>Customize Plot:</strong> Adjust confidence levels, sorting, and appearance</li>
                        </ol>
                        
                        <h4>Model Types:</h4>
                        <ul>
                            <li><strong>Linear:</strong> Continuous outcomes (shows coefficients)</li>
                            <li><strong>Logistic:</strong> Binary outcomes (shows odds ratios)</li>
                            <li><strong>Cox:</strong> Survival outcomes (shows hazard ratios)</li>
                            <li><strong>Poisson:</strong> Count outcomes (shows rate ratios)</li>
                        </ul>
                        
                        <p><em>Note: For Cox regression, also select a time variable. Coefficients will be automatically transformed for logistic and Cox models.</em></p>
                    </div>"
                    
                    self$results$instructions$setContent(instructions_html)
                    return()
                } else {
                    self$results$instructions$setContent("")
                }
                
                # Get data
                data <- self$data
                if (nrow(data) == 0) {
                    stop("Data contains no rows")
                }
                
                # Perform analysis
                tryCatch({
                    
                    # Fit primary model
                    model1 <- private$.fitModel(data, self$options$covs)
                    
                    # Generate outputs
                    if (self$options$show_coefficient_plot) {
                        private$.generateCoefficientPlot(model1, data)
                    }
                    
                    if (self$options$show_model_summary) {
                        private$.generateModelSummary(model1)
                    }
                    
                    if (self$options$show_coefficient_table) {
                        private$.generateCoefficientTable(model1)
                    }
                    
                }, error = function(e) {
                    error_msg <- paste0(
                        "<div style='color: red; font-weight: bold;'>",
                        "Error in regression analysis: ", e$message,
                        "<br><br>",
                        "Please check your variable selections and model specification.",
                        "</div>"
                    )
                    self$results$instructions$setContent(error_msg)
                })
            },
            
            # Fit regression model
            .fitModel = function(data, covariates) {
                
                dep_var <- self$options$dep
                
                # Build formula
                formula_str <- paste(dep_var, "~", paste(covariates, collapse = " + "))
                formula_obj <- as.formula(formula_str)
                
                # Fit model based on type
                model_type <- self$options$model_type
                
                if (model_type == "linear") {
                    model <- lm(formula_obj, data = data)
                } else if (model_type == "logistic") {
                    # Check if dependent variable is binary
                    dep_data <- data[[dep_var]]
                    unique_vals <- unique(dep_data[!is.na(dep_data)])
                    if (length(unique_vals) != 2) {
                        stop("Logistic regression requires a binary dependent variable with exactly 2 levels")
                    }
                    model <- glm(formula_obj, data = data, family = binomial(link = "logit"))
                } else if (model_type == "cox") {
                    if (is.null(self$options$time_var)) {
                        stop("Cox regression requires a time variable")
                    }
                    if (!requireNamespace("survival", quietly = TRUE)) {
                        stop("The 'survival' package is required for Cox regression")
                    }
                    time_var <- self$options$time_var
                    # Create survival object
                    surv_formula_str <- paste("survival::Surv(", time_var, ",", dep_var, ") ~", paste(covariates, collapse = " + "))
                    surv_formula <- as.formula(surv_formula_str)
                    model <- survival::coxph(surv_formula, data = data)
                } else if (model_type == "poisson") {
                    model <- glm(formula_obj, data = data, family = poisson(link = "log"))
                } else {
                    stop("Unsupported model type")
                }
                
                return(model)
            },
            
            # Generate coefficient plot
            .generateCoefficientPlot = function(model, data) {
                
                # Determine if we should exponentiate
                exp_coefs <- self$options$exp_transform || 
                           self$options$model_type %in% c("logistic", "cox", "poisson")
                
                # Create plot using jtools for more customization options
                tryCatch({
                    
                    plot_args <- list(
                        model,
                        ci_level = self$options$ci_level,
                        exp = exp_coefs,
                        point.size = self$options$point_size
                    )
                    
                    # Add inner CI if specified
                    if (self$options$inner_ci_level > 0.5 && self$options$inner_ci_level < self$options$ci_level) {
                        plot_args$inner_ci_level <- self$options$inner_ci_level
                    }
                    
                    # Handle intercept
                    if (!self$options$include_intercept) {
                        plot_args$omit.coefs <- c("(Intercept)", "Intercept")
                    } else {
                        plot_args$omit.coefs <- NULL
                    }
                    
                    # Handle coefficient selection
                    if (self$options$coef_selection != "all" && 
                        nchar(trimws(self$options$specific_coefs)) > 0) {
                        coef_list <- trimws(strsplit(self$options$specific_coefs, ",")[[1]])
                        if (self$options$coef_selection == "specific") {
                            plot_args$coefs <- coef_list
                        } else if (self$options$coef_selection == "exclude") {
                            plot_args$omit.coefs <- c(plot_args$omit.coefs, coef_list)
                        }
                    }
                    
                    # Generate plot
                    if (requireNamespace("jtools", quietly = TRUE)) {
                        p <- do.call(jtools::plot_coefs, plot_args)
                    } else {
                        # Fallback to coefplot package
                        p <- coefplot::coefplot(
                            model,
                            title = if (nchar(self$options$custom_title) > 0) self$options$custom_title else "Coefficient Plot",
                            xlab = if (nchar(self$options$custom_x_label) > 0) self$options$custom_x_label else "Coefficient Value",
                            outerCI = self$options$ci_level,
                            innerCI = if (self$options$inner_ci_level > 0.5) self$options$inner_ci_level else 0,
                            intercept = self$options$include_intercept,
                            sort = self$options$sort_coefs,
                            decreasing = self$options$decreasing_sort,
                            pointSize = self$options$point_size
                        )
                    }
                    
                    # Customize plot
                    if (nchar(self$options$custom_title) > 0) {
                        p <- p + ggplot2::ggtitle(self$options$custom_title)
                    } else {
                        title_text <- switch(self$options$model_type,
                                           "linear" = "Linear Regression Coefficients",
                                           "logistic" = "Logistic Regression (Odds Ratios)",
                                           "cox" = "Cox Regression (Hazard Ratios)",
                                           "poisson" = "Poisson Regression (Rate Ratios)",
                                           "Coefficient Plot")
                        p <- p + ggplot2::ggtitle(title_text)
                    }
                    
                    if (nchar(self$options$custom_x_label) > 0) {
                        p <- p + ggplot2::xlab(self$options$custom_x_label)
                    } else {
                        x_label <- if (exp_coefs) {
                            switch(self$options$model_type,
                                   "logistic" = "Odds Ratio",
                                   "cox" = "Hazard Ratio", 
                                   "poisson" = "Rate Ratio",
                                   "Ratio")
                        } else {
                            "Coefficient"
                        }
                        p <- p + ggplot2::xlab(x_label)
                    }
                    
                    # Add reference line
                    if (exp_coefs) {
                        p <- p + ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.7)
                    } else {
                        p <- p + ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.7)
                    }
                    
                    # Apply theme
                    p <- p + ggplot2::theme_minimal() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                            axis.title = ggplot2::element_text(size = 12),
                            axis.text = ggplot2::element_text(size = 10)
                        )
                    
                    # Store plot
                    self$results$coefficient_plot$setState(p)
                    
                }, error = function(e) {
                    stop(paste("Error creating coefficient plot:", e$message))
                })
            },
            
            # Generate model summary
            .generateModelSummary = function(model) {
                
                html_content <- "<div style='font-family: Arial, sans-serif;'>"
                html_content <- paste0(html_content, "<h3>Model Summary</h3>")
                
                # Model type and formula
                model_type_name <- switch(self$options$model_type,
                                        "linear" = "Linear Regression",
                                        "logistic" = "Logistic Regression",
                                        "cox" = "Cox Proportional Hazards",
                                        "poisson" = "Poisson Regression",
                                        "Regression Model")
                
                html_content <- paste0(html_content, "<h4>", model_type_name, "</h4>")
                html_content <- paste0(html_content, "<p><strong>Formula:</strong> ", 
                                     deparse(formula(model)), "</p>")
                
                # Sample size
                n_obs <- nobs(model)
                html_content <- paste0(html_content, "<p><strong>Observations:</strong> ", n_obs, "</p>")
                
                # Model fit statistics
                html_content <- paste0(html_content, "<h4>Model Fit Statistics</h4>")
                html_content <- paste0(html_content, "<table border='1' cellpadding='5' cellspacing='0' style='border-collapse: collapse;'>")
                html_content <- paste0(html_content, "<tr style='background-color: #f0f0f0; font-weight: bold;'>")
                html_content <- paste0(html_content, "<th>Statistic</th><th>Value</th></tr>")
                
                if (self$options$model_type == "linear") {
                    # R-squared
                    r_squared <- summary(model)$r.squared
                    adj_r_squared <- summary(model)$adj.r.squared
                    html_content <- paste0(html_content, "<tr><td>R-squared</td><td>", round(r_squared, 4), "</td></tr>")
                    html_content <- paste0(html_content, "<tr><td>Adjusted R-squared</td><td>", round(adj_r_squared, 4), "</td></tr>")
                    
                    # F-statistic
                    f_stat <- summary(model)$fstatistic
                    if (!is.null(f_stat)) {
                        f_value <- f_stat[1]
                        f_p <- pf(f_value, f_stat[2], f_stat[3], lower.tail = FALSE)
                        html_content <- paste0(html_content, "<tr><td>F-statistic</td><td>", round(f_value, 2), "</td></tr>")
                        html_content <- paste0(html_content, "<tr><td>F p-value</td><td>", round(f_p, 4), "</td></tr>")
                    }
                } else if (self$options$model_type %in% c("logistic", "poisson")) {
                    # Deviance
                    null_dev <- model$null.deviance
                    resid_dev <- model$deviance
                    if (!is.null(null_dev) && !is.null(resid_dev)) {
                        pseudo_r2 <- 1 - (resid_dev / null_dev)
                        html_content <- paste0(html_content, "<tr><td>Null Deviance</td><td>", round(null_dev, 2), "</td></tr>")
                        html_content <- paste0(html_content, "<tr><td>Residual Deviance</td><td>", round(resid_dev, 2), "</td></tr>")
                        html_content <- paste0(html_content, "<tr><td>Pseudo R-squared</td><td>", round(pseudo_r2, 4), "</td></tr>")
                    }
                } else if (self$options$model_type == "cox") {
                    # Concordance
                    if (requireNamespace("survival", quietly = TRUE)) {
                        concordance <- survival::concordance(model)
                        if (!is.null(concordance$concordance)) {
                            html_content <- paste0(html_content, "<tr><td>Concordance</td><td>", round(concordance$concordance, 4), "</td></tr>")
                        }
                    }
                    
                    # Likelihood ratio test
                    lr_test <- anova(model, test = "Chisq")
                    if (!is.null(lr_test) && nrow(lr_test) > 0) {
                        lr_stat <- lr_test$Chisq[2]
                        lr_p <- lr_test$`Pr(>Chi)`[2]
                        if (!is.na(lr_stat)) {
                            html_content <- paste0(html_content, "<tr><td>Likelihood Ratio</td><td>", round(lr_stat, 2), "</td></tr>")
                            html_content <- paste0(html_content, "<tr><td>LR p-value</td><td>", round(lr_p, 4), "</td></tr>")
                        }
                    }
                }
                
                # AIC
                aic_value <- AIC(model)
                if (!is.null(aic_value) && !is.na(aic_value)) {
                    html_content <- paste0(html_content, "<tr><td>AIC</td><td>", round(aic_value, 2), "</td></tr>")
                }
                
                html_content <- paste0(html_content, "</table>")
                html_content <- paste0(html_content, "</div>")
                
                self$results$model_summary$setContent(html_content)
            },
            
            # Generate coefficient table
            .generateCoefficientTable = function(model) {
                
                # Get coefficient summary
                coef_summary <- summary(model)$coefficients
                
                html_content <- "<div style='font-family: Arial, sans-serif;'>"
                html_content <- paste0(html_content, "<h3>Coefficient Table</h3>")
                html_content <- paste0(html_content, "<table border='1' cellpadding='5' cellspacing='0' style='border-collapse: collapse;'>")
                
                # Determine if we should exponentiate
                exp_coefs <- self$options$exp_transform || 
                           self$options$model_type %in% c("logistic", "cox", "poisson")
                
                # Header
                html_content <- paste0(html_content, "<tr style='background-color: #f0f0f0; font-weight: bold;'>")
                html_content <- paste0(html_content, "<th>Variable</th>")
                
                if (exp_coefs) {
                    coef_name <- switch(self$options$model_type,
                                      "logistic" = "Odds Ratio",
                                      "cox" = "Hazard Ratio",
                                      "poisson" = "Rate Ratio",
                                      "Ratio")
                    html_content <- paste0(html_content, "<th>", coef_name, "</th>")
                } else {
                    html_content <- paste0(html_content, "<th>Coefficient</th>")
                }
                
                html_content <- paste0(html_content, "<th>Std. Error</th>")
                html_content <- paste0(html_content, "<th>", colnames(coef_summary)[ncol(coef_summary)], "</th>")
                
                ci_level <- self$options$ci_level
                ci_percent <- round(ci_level * 100)
                html_content <- paste0(html_content, "<th>", ci_percent, "% CI</th>")
                html_content <- paste0(html_content, "</tr>")
                
                # Calculate confidence intervals
                ci_results <- confint(model, level = ci_level)
                
                # Table rows
                for (i in 1:nrow(coef_summary)) {
                    var_name <- rownames(coef_summary)[i]
                    
                    # Skip intercept if not requested
                    if (!self$options$include_intercept && var_name %in% c("(Intercept)", "Intercept")) {
                        next
                    }
                    
                    coef_val <- coef_summary[i, 1]
                    se_val <- coef_summary[i, 2]
                    p_val <- coef_summary[i, ncol(coef_summary)]
                    
                    # Transform if needed
                    if (exp_coefs) {
                        coef_val <- exp(coef_val)
                        ci_lower <- exp(ci_results[i, 1])
                        ci_upper <- exp(ci_results[i, 2])
                    } else {
                        ci_lower <- ci_results[i, 1]
                        ci_upper <- ci_results[i, 2]
                    }
                    
                    html_content <- paste0(html_content, "<tr>")
                    html_content <- paste0(html_content, "<td><strong>", var_name, "</strong></td>")
                    html_content <- paste0(html_content, "<td>", round(coef_val, 4), "</td>")
                    html_content <- paste0(html_content, "<td>", round(se_val, 4), "</td>")
                    html_content <- paste0(html_content, "<td>", round(p_val, 4), "</td>")
                    html_content <- paste0(html_content, "<td>(", round(ci_lower, 4), ", ", round(ci_upper, 4), ")</td>")
                    html_content <- paste0(html_content, "</tr>")
                }
                
                html_content <- paste0(html_content, "</table>")
                html_content <- paste0(html_content, "</div>")
                
                self$results$coefficient_table$setContent(html_content)
            }
        ),
        
        # Plot function
        active = list(
            .plot = function() {
                plot_state <- self$results$coefficient_plot$state
                if (!is.null(plot_state)) {
                    print(plot_state)
                    return(TRUE)
                }
                return(FALSE)
            }
        )
    )
}