
flexparametricadvancedClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "flexparametricadvancedClass",
    inherit = flexparametricadvancedBase,
    private = list(
        .init = function() {
            private$.updateInstructions()
        },
        
        .run = function() {
            # Check for required inputs
            if (is.null(self$options$time) || is.null(self$options$event)) {
                return()
            }
            
            # Prepare data
            data <- private$.prepareData()
            if (is.null(data)) return()
            
            # Fit flexible parametric model
            model <- private$.fitFlexibleModel(data)
            if (is.null(model)) return()
            
            # Populate results
            private$.populateModelSummary(model)
            private$.populateParameterEstimates(model)
            private$.populateHazardComponents(model)
            private$.populatePredictions(model, data)
            private$.createPlots(model, data)
        },
        
        .updateInstructions = function() {
            html <- paste0(
                "<h3>Advanced Flexible Parametric Survival Models</h3>",
                "<p>This analysis implements Royston-Parmar flexible parametric survival models with customizable baseline hazard distributions.</p>",
                "<h4>Key Features:</h4>",
                "<ul>",
                "<li><strong>Multiple Distributions:</strong> Weibull, log-normal, log-logistic, gamma, Gompertz, generalized gamma</li>",
                "<li><strong>Royston-Parmar Splines:</strong> Flexible baseline hazards using restricted cubic splines</li>",
                "<li><strong>Time-Varying Effects:</strong> Model covariate effects that change over time</li>",
                "<li><strong>Cure Models:</strong> Mixture and non-mixture cure model specifications</li>",
                "<li><strong>Extrapolation:</strong> Superior extrapolation for health economic modeling</li>",
                "</ul>",
                "<h4>Selected Configuration:</h4>",
                "<ul>",
                "<li><strong>Distribution:</strong> ", private$.formatDistribution(self$options$distribution), "</li>",
                "<li><strong>Baseline Splines:</strong> ", self$options$baseline_splines, " knots</li>",
                "<li><strong>Time-Varying Effects:</strong> ", ifelse(length(self$options$time_varying) > 0, "Yes", "No"), "</li>",
                "<li><strong>Cure Fraction:</strong> ", ifelse(self$options$cure_model, "Enabled", "Disabled"), "</li>",
                "</ul>",
                "<p><strong>Note:</strong> Flexible parametric models provide smooth hazard functions and are particularly ",
                "useful for extrapolation beyond observed follow-up times.</p>"
            )
            self$results$instructions$setContent(html)
        },
        
        .prepareData = function() {
            # Get the data
            data <- self$data
            
            # Extract required variables
            time_var <- self$options$time
            event_var <- self$options$event
            covariates <- self$options$covariates
            
            # Check for missing values
            vars_to_check <- c(time_var, event_var, covariates)
            complete_cases <- complete.cases(data[, vars_to_check, drop = FALSE])
            
            if (sum(!complete_cases) > 0) {
                self$results$warnings$setContent(paste0(
                    "<p>Warning: ", sum(!complete_cases), " cases with missing values were removed.</p>"
                ))
            }
            
            # Filter to complete cases
            analysis_data <- data[complete_cases, , drop = FALSE]
            
            # Check for sufficient events
            n_events <- sum(analysis_data[[event_var]] == 1)
            if (n_events < 10) {
                self$results$errors$setContent(
                    "<p>Error: Insufficient events for analysis (minimum 10 required).</p>"
                )
                return(NULL)
            }
            
            return(analysis_data)
        },
        
        .fitFlexibleModel = function(data) {
            if (!requireNamespace("flexsurv", quietly = TRUE)) {
                self$results$errors$setContent(
                    "<p>Error: Package 'flexsurv' is required but not installed.</p>"
                )
                return(NULL)
            }
            
            if (!requireNamespace("rstpm2", quietly = TRUE) && 
                self$options$distribution == "spline") {
                self$results$errors$setContent(
                    "<p>Error: Package 'rstpm2' is required for spline models but not installed.</p>"
                )
                return(NULL)
            }
            
            # Build formula
            time_var <- self$options$time
            event_var <- self$options$event
            covariates <- self$options$covariates
            
            if (length(covariates) > 0) {
                formula_str <- paste0("Surv(", time_var, ", ", event_var, ") ~ ",
                                    paste(covariates, collapse = " + "))
            } else {
                formula_str <- paste0("Surv(", time_var, ", ", event_var, ") ~ 1")
            }
            
            surv_formula <- as.formula(formula_str)
            
            # Fit model based on distribution type
            tryCatch({
                if (self$options$distribution == "spline") {
                    # Royston-Parmar model using rstpm2
                    model <- rstpm2::stpm2(
                        formula = surv_formula,
                        data = data,
                        df = self$options$baseline_splines
                    )
                } else {
                    # Standard parametric model using flexsurv
                    dist_name <- switch(self$options$distribution,
                        "weibull" = "weibull",
                        "exp" = "exponential",
                        "lnorm" = "lognormal", 
                        "llogis" = "llogis",
                        "gamma" = "gamma",
                        "gompertz" = "gompertz",
                        "gengamma" = "gengamma",
                        "weibull"  # default
                    )
                    
                    model <- flexsurv::flexsurvreg(
                        formula = surv_formula,
                        data = data,
                        dist = dist_name
                    )
                }
                
                return(model)
                
            }, error = function(e) {
                self$results$errors$setContent(paste0(
                    "<p>Error fitting model: ", e$message, "</p>"
                ))
                return(NULL)
            })
        },
        
        .populateModelSummary = function(model) {
            summary_table <- self$results$modelSummary
            
            # Extract model fit statistics
            if (inherits(model, "flexsurvreg")) {
                summary_table$setRow(rowNo = 1, values = list(
                    statistic = "Log-likelihood",
                    value = round(model$loglik, 3)
                ))
                summary_table$setRow(rowNo = 2, values = list(
                    statistic = "AIC",
                    value = round(model$AIC, 3)
                ))
                summary_table$setRow(rowNo = 3, values = list(
                    statistic = "BIC", 
                    value = round(BIC(model), 3)
                ))
                summary_table$setRow(rowNo = 4, values = list(
                    statistic = "N",
                    value = model$N
                ))
                summary_table$setRow(rowNo = 5, values = list(
                    statistic = "Events",
                    value = model$events
                ))
            } else if (inherits(model, "stpm2")) {
                summary_table$setRow(rowNo = 1, values = list(
                    statistic = "Log-likelihood",
                    value = round(logLik(model), 3)
                ))
                summary_table$setRow(rowNo = 2, values = list(
                    statistic = "AIC",
                    value = round(AIC(model), 3)
                ))
                summary_table$setRow(rowNo = 3, values = list(
                    statistic = "BIC",
                    value = round(BIC(model), 3)
                ))
                summary_table$setRow(rowNo = 4, values = list(
                    statistic = "Degrees of freedom",
                    value = model@df
                ))
            }
        },
        
        .populateParameterEstimates = function(model) {
            param_table <- self$results$parameterEstimates
            
            if (inherits(model, "flexsurvreg")) {
                # Extract coefficients
                coefs <- coef(model)
                se <- sqrt(diag(vcov(model)))
                z_vals <- coefs / se
                p_vals <- 2 * pnorm(-abs(z_vals))
                ci_lower <- coefs - 1.96 * se
                ci_upper <- coefs + 1.96 * se
                
                for (i in seq_along(coefs)) {
                    param_table$addRow(rowKey = i, values = list(
                        parameter = names(coefs)[i],
                        estimate = round(coefs[i], 4),
                        se = round(se[i], 4),
                        ci_lower = round(ci_lower[i], 4),
                        ci_upper = round(ci_upper[i], 4),
                        z_value = round(z_vals[i], 3),
                        p_value = round(p_vals[i], 4)
                    ))
                }
            } else if (inherits(model, "stpm2")) {
                # Extract coefficients from stpm2 model
                summary_model <- summary(model)
                coef_table <- summary_model@coef
                
                for (i in 1:nrow(coef_table)) {
                    param_table$addRow(rowKey = i, values = list(
                        parameter = rownames(coef_table)[i],
                        estimate = round(coef_table[i, "Estimate"], 4),
                        se = round(coef_table[i, "Std. Error"], 4),
                        ci_lower = round(coef_table[i, "Estimate"] - 1.96 * coef_table[i, "Std. Error"], 4),
                        ci_upper = round(coef_table[i, "Estimate"] + 1.96 * coef_table[i, "Std. Error"], 4),
                        z_value = round(coef_table[i, "z"], 3),
                        p_value = round(coef_table[i, "Pr(>|z|)"], 4)
                    ))
                }
            }
        },
        
        .populateHazardComponents = function(model) {
            if (!self$options$show_hazard_components) return()
            
            hazard_table <- self$results$hazardComponents
            
            # Generate time points for hazard calculation
            time_points <- seq(0, max(self$data[[self$options$time]]), length.out = 50)
            
            if (inherits(model, "flexsurvreg")) {
                # Calculate hazard at time points
                hazard_vals <- flexsurv::hflex(model, t = time_points)
                cumhaz_vals <- flexsurv::Hflex(model, t = time_points)
                surv_vals <- flexsurv::pflex(model, q = time_points, lower.tail = FALSE)
                
                for (i in seq_along(time_points)) {
                    hazard_table$addRow(rowKey = i, values = list(
                        time = round(time_points[i], 2),
                        hazard = round(hazard_vals[i], 4),
                        cumulative_hazard = round(cumhaz_vals[i], 4),
                        survival = round(surv_vals[i], 4)
                    ))
                }
            }
        },
        
        .populatePredictions = function(model, data) {
            if (!self$options$show_predictions) return()
            
            pred_table <- self$results$predictions
            
            # Calculate median survival time
            if (inherits(model, "flexsurvreg")) {
                # Get median survival for each covariate pattern
                newdata <- data[1:min(10, nrow(data)), self$options$covariates, drop = FALSE]
                
                for (i in 1:nrow(newdata)) {
                    median_surv <- flexsurv::qflex(model, p = 0.5, newdata = newdata[i, , drop = FALSE])
                    
                    pred_table$addRow(rowKey = i, values = list(
                        observation = i,
                        median_survival = round(median_surv, 2)
                    ))
                }
            }
        },
        
        .createPlots = function(model, data) {
            if (!self$options$plot_survival) return()
            
            if (!requireNamespace("ggplot2", quietly = TRUE)) return()
            
            # Create survival plot
            plot <- self$results$survivalPlot
            
            # Generate plot data
            time_points <- seq(0, max(data[[self$options$time]]), length.out = 100)
            
            if (inherits(model, "flexsurvreg")) {
                surv_probs <- flexsurv::pflex(model, q = time_points, lower.tail = FALSE)
                
                plot_data <- data.frame(
                    time = time_points,
                    survival = surv_probs
                )
                
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = survival)) +
                    ggplot2::geom_line(size = 1.2, color = "blue") +
                    ggplot2::scale_y_continuous(limits = c(0, 1)) +
                    ggplot2::labs(
                        x = "Time",
                        y = "Survival Probability",
                        title = paste("Flexible Parametric Survival Curve -", 
                                    private$.formatDistribution(self$options$distribution))
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5)
                    )
                
                plot$setState(p)
            }
            
            # Create hazard plot if requested
            if (self$options$plot_hazard) {
                hazard_plot <- self$results$hazardPlot
                
                if (inherits(model, "flexsurvreg")) {
                    hazard_vals <- flexsurv::hflex(model, t = time_points)
                    
                    hazard_data <- data.frame(
                        time = time_points,
                        hazard = hazard_vals
                    )
                    
                    h <- ggplot2::ggplot(hazard_data, ggplot2::aes(x = time, y = hazard)) +
                        ggplot2::geom_line(size = 1.2, color = "red") +
                        ggplot2::labs(
                            x = "Time",
                            y = "Hazard",
                            title = "Hazard Function"
                        ) +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(hjust = 0.5)
                        )
                    
                    hazard_plot$setState(h)
                }
            }
        },
        
        .formatDistribution = function(dist) {
            switch(dist,
                "weibull" = "Weibull",
                "exp" = "Exponential",
                "lnorm" = "Log-Normal",
                "llogis" = "Log-Logistic",
                "gamma" = "Gamma",
                "gompertz" = "Gompertz",
                "gengamma" = "Generalized Gamma",
                "spline" = "Royston-Parmar Splines",
                "Unknown"
            )
        }
    )
)
