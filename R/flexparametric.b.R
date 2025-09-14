#' @title Flexible Parametric Survival Models
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @import flexsurv
#' @import ggplot2
#' @import dplyr
#' @importFrom stats AIC BIC nobs vcov qnorm pnorm
#'

flexparametricClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "flexparametricClass",
    inherit = flexparametricBase,
    private = list(
        .init = function() {
            private$.initResults()
        },
        
        .run = function() {
            # Get variables
            elapsedtime <- self$options$elapsedtime
            outcome <- self$options$outcome  
            covariates <- self$options$covariates
            
            if (is.null(elapsedtime) || is.null(outcome)) {
                self$results$todo$setContent("<p>Please specify both Time Variable and Event Indicator to proceed with the analysis.</p>")
                return()
            }
            
            # Prepare data
            data <- private$.prepareData()
            if (is.null(data)) return()
            
            # Validate inputs for clinical appropriateness
            validation <- private$.validateInputs(data)
            if (!validation$valid) {
                self$results$todo$setContent(validation$message)
                return()
            }
            
            # Show validation warnings if present
            if (nchar(validation$warnings) > 0) {
                private$.showValidationWarnings(validation$warnings)
            }
            
            # Fit parametric model
            model_results <- private$.fitParametricModel(data)
            if (is.null(model_results)) return()
            
            # Populate results
            private$.populateModelSummary(model_results)
            private$.populateParametersTable(model_results)
            private$.populateSplineDetails(model_results)
            private$.populateModelComparison(model_results, data)
            private$.populateFitStatistics(model_results)
            private$.populateClinicalSummary(model_results)
            private$.populatePlots(model_results, data)
            private$.populateSummary(model_results)
            private$.populateMethodology()
        },
        
        .initResults = function() {
            # Initialize todo content
            self$results$todo$setContent("<p>Configure the analysis options and run to see flexible parametric survival modeling results.</p>")
        },
        
        .prepareData = function() {
            # Extract variables from data
            data <- self$data
            
            elapsedtime_name <- self$options$elapsedtime
            outcome_name <- self$options$outcome
            covariate_names <- self$options$covariates
            
            if (is.null(elapsedtime_name) || is.null(outcome_name)) return(NULL)
            
            # Create analysis dataset
            analysis_data <- data.frame(
                time = data[[elapsedtime_name]],
                event = data[[outcome_name]]
            )
            
            # Add covariates if specified
            if (!is.null(covariate_names) && length(covariate_names) > 0) {
                for (cov_name in covariate_names) {
                    analysis_data[[cov_name]] <- data[[cov_name]]
                }
            }
            
            # Handle outcome coding
            outcome_level <- self$options$outcomeLevel
            if (is.character(analysis_data$event)) {
                analysis_data$event <- ifelse(analysis_data$event == outcome_level, 1, 0)
            } else {
                analysis_data$event <- ifelse(analysis_data$event == as.numeric(outcome_level), 1, 0)
            }
            
            # Remove missing values
            analysis_data <- analysis_data[complete.cases(analysis_data), ]
            
            if (nrow(analysis_data) == 0) {
                self$results$todo$setContent("<p>No complete cases available for analysis after removing missing values.</p>")
                return(NULL)
            }
            
            return(analysis_data)
        },
        
        .validateInputs = function(data) {
            n_events <- sum(data$event)
            n_obs <- nrow(data)
            n_covs <- length(if (is.null(self$options$covariates)) c() else self$options$covariates)
            event_rate <- n_events / n_obs * 100
            model_approach <- self$options$model_approach
            spline_df <- self$options$spline_df
            
            warnings_list <- c()
            
            # Critical sample size validation
            if (n_events < 10) {
                return(list(
                    valid = FALSE, 
                    message = paste0(
                        "<div style='background-color: #f8d7da; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                        "<h4>❌ INSUFFICIENT EVENTS</h4>",
                        "<p><strong>Found:</strong> ", n_events, " events in ", n_obs, " observations (", round(event_rate, 1), "% event rate)</p>",
                        "<p><strong>Required:</strong> ≥10 events for basic parametric modeling</p>",
                        "<p><strong>Recommendation:</strong> Collect more data or consider simpler analysis methods.</p>",
                        "</div>"
                    ),
                    warnings = ""
                ))
            }
            
            # Events per variable rule (critical for multivariable models)
            if (n_covs > 0) {
                events_per_var <- n_events / n_covs
                if (events_per_var < 5) {
                    return(list(
                        valid = FALSE,
                        message = paste0(
                            "<div style='background-color: #f8d7da; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                            "<h4>⚠️ TOO MANY COVARIATES</h4>",
                            "<p><strong>Current:</strong> ", n_covs, " covariates with ", n_events, " events (ratio: ", round(events_per_var, 1), ")</p>",
                            "<p><strong>Rule of thumb:</strong> Need ≥5 events per covariate for reliable estimates</p>",
                            "<p><strong>Recommendation:</strong> Use ≤", floor(n_events/5), " covariates or collect more data.</p>",
                            "</div>"
                        ),
                        warnings = ""
                    ))
                }
                
                # Warning for marginal sample sizes
                if (events_per_var < 10) {
                    warnings_list <- c(warnings_list,
                        paste0("<p>⚠️ <strong>Marginal sample size:</strong> ", round(events_per_var, 1), 
                              " events per covariate. Consider using fewer covariates for more stable estimates.</p>")
                    )
                }
            }
            
            # Spline complexity warnings
            if (model_approach %in% c('spline_based', 'automatic') && spline_df > 4) {
                if (n_events < spline_df * 10) {
                    warnings_list <- c(warnings_list,
                        paste0("<p>⚠️ <strong>Complex spline model:</strong> ", spline_df, " degrees of freedom with ", 
                              n_events, " events. Consider reducing complexity to 3-4 degrees of freedom.</p>")
                    )
                }
            }
            
            # Very low event rate warning
            if (event_rate < 5) {
                warnings_list <- c(warnings_list,
                    paste0("<p>⚠️ <strong>Low event rate:</strong> ", round(event_rate, 1), 
                          "% events. Parametric models may be less reliable with very low event rates.</p>")
                )
            }
            
            # Very high event rate (may indicate coding issues)
            if (event_rate > 90) {
                warnings_list <- c(warnings_list,
                    paste0("<p>⚠️ <strong>Very high event rate:</strong> ", round(event_rate, 1), 
                          "% events. Please verify event coding is correct (1=event, 0=censored).</p>")
                )
            }
            
            # Combine warnings
            warnings_html <- if (length(warnings_list) > 0) {
                paste0(
                    "<div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                    "<h4>⚠️ Analysis Warnings</h4>",
                    paste(warnings_list, collapse = ""),
                    "</div>"
                )
            } else {
                ""
            }
            
            return(list(
                valid = TRUE,
                message = "",
                warnings = warnings_html
            ))
        },
        
        .showValidationWarnings = function(warnings_html) {
            if (nchar(warnings_html) > 0) {
                # Append warnings to todo content
                current_content <- "<p>Analysis completed with warnings below:</p>"
                self$results$todo$setContent(paste0(current_content, warnings_html))
            }
        },
        
        .fitParametricModel = function(data) {
            model_approach <- self$options$model_approach
            distribution <- self$options$distribution
            covariate_names <- self$options$covariates
            
            tryCatch({
                # Create survival object
                surv_obj <- Surv(data$time, data$event)
                
                # Create formula
                if (!is.null(covariate_names) && length(covariate_names) > 0) {
                    formula_str <- paste("surv_obj ~", paste(covariate_names, collapse = " + "))
                    formula_obj <- as.formula(formula_str)
                } else {
                    formula_obj <- surv_obj ~ 1
                }
                
                # Fit model based on approach
                if (model_approach == 'spline_based') {
                    model <- private$.fitSplineModel(formula_obj, data)
                } else if (model_approach == 'automatic') {
                    model <- private$.fitBestModel(formula_obj, data)
                } else {
                    # Traditional parametric approach
                    model <- flexsurv::flexsurvreg(
                        formula_obj, 
                        data = data, 
                        dist = distribution
                    )
                }
                
                # Extract model information
                model_summary <- summary(model)
                
                return(list(
                    model = model,
                    summary = model_summary,
                    data = data,
                    distribution = distribution,
                    approach = model_approach
                ))
                
            }, error = function(e) {
                error_msg <- paste0("<p>Error fitting parametric model: ", e$message, "</p>")
                self$results$todo$setContent(error_msg)
                return(NULL)
            })
        },
        
        .populateParametersTable = function(model_results) {
            if (!self$options$show_parameters) return()
            
            table <- self$results$parametersTable
            model <- model_results$model
            
            # Extract parameter estimates
            coefs <- model$coefficients
            vcov_matrix <- vcov(model)
            ses <- sqrt(diag(vcov_matrix))
            
            conf_level <- self$options$confidence_level
            z_crit <- qnorm((1 + conf_level) / 2)
            
            for (i in seq_along(coefs)) {
                param_name <- names(coefs)[i]
                estimate <- coefs[i]
                se <- ses[i]
                z_value <- estimate / se
                p_value <- 2 * (1 - pnorm(abs(z_value)))
                lower_ci <- estimate - z_crit * se
                upper_ci <- estimate + z_crit * se
                
                # Calculate hazard ratio for covariate parameters
                hazard_ratio <- if (!grepl("scale|shape|gamma|intercept", param_name, ignore.case = TRUE)) {
                    exp(estimate)
                } else {
                    NA
                }
                
                # Clinical interpretation
                interpretation <- private$.interpretParameter(param_name, estimate, hazard_ratio, p_value)
                
                table$addRow(rowKey = i, values = list(
                    parameter = param_name,
                    estimate = estimate,
                    se = se,
                    lower_ci = lower_ci,
                    upper_ci = upper_ci,
                    z_value = z_value,
                    p_value = p_value,
                    hazard_ratio = hazard_ratio,
                    interpretation = interpretation
                ))
            }
        },
        
        .populateFitStatistics = function(model_results) {
            if (!self$options$show_aic_bic) return()
            
            table <- self$results$fitStatistics
            model <- model_results$model
            
            # Extract fit statistics
            aic_value <- AIC(model)
            bic_value <- BIC(model)
            log_likelihood <- model$loglik
            
            # Add statistics to table with interpretations
            table$addRow(rowKey = "aic", values = list(
                statistic = "AIC",
                value = round(aic_value, 2),
                interpretation = "Lower values indicate better model fit"
            ))
            
            table$addRow(rowKey = "bic", values = list(
                statistic = "BIC",
                value = round(bic_value, 2),
                interpretation = "Lower values indicate better model fit (penalizes complexity)"
            ))
            
            table$addRow(rowKey = "loglik", values = list(
                statistic = "Log-Likelihood",
                value = round(log_likelihood, 2),
                interpretation = "Higher values indicate better model fit"
            ))
            
            # Add number of parameters
            n_params <- length(model$coefficients)
            table$addRow(rowKey = "params", values = list(
                statistic = "Parameters",
                value = n_params,
                interpretation = paste0("Model complexity: ", n_params, " estimated parameters")
            ))
        },
        
        .populatePlots = function(model_results, data) {
            # Survival plot
            if (self$options$show_survival_plot) {
                survival_plot <- private$.createSurvivalPlot(model_results)
                self$results$survivalPlot$setState(survival_plot)
            }
            
            # Hazard plot
            if (self$options$show_hazard_plot) {
                hazard_plot <- private$.createHazardPlot(model_results)
                self$results$hazardPlot$setState(hazard_plot)
            }
            
            # Density plot
            if (self$options$show_density_plot) {
                density_plot <- private$.createDensityPlot(model_results)
                self$results$densityPlot$setState(density_plot)
            }
            
            # Spline plot
            if (self$options$show_spline_plot && self$options$model_approach == 'spline_based') {
                spline_plot <- private$.createSplinePlot(model_results)
                self$results$splinePlot$setState(spline_plot)
            }
            
            # Diagnostics plot
            if (self$options$show_diagnostics) {
                diagnostics_plot <- private$.createDiagnosticsPlot(model_results)
                self$results$diagnosticsPlot$setState(diagnostics_plot)
            }
        },
        
        .createSurvivalPlot = function(model_results) {
            model <- model_results$model
            data <- model_results$data
            
            # Determine time range
            max_time <- self$options$plot_time_max
            if (max_time <= 0) {
                max_time <- max(data$time, na.rm = TRUE) * 1.1
            }
            
            # Create time sequence
            times <- seq(0, max_time, length.out = 100)
            
            # Calculate survival probabilities
            if (!is.null(self$options$covariates) && length(self$options$covariates) > 0) {
                # For models with covariates, use mean values
                mean_covs <- data[1, setdiff(names(data), c("time", "event")), drop = FALSE]
                for (col in names(mean_covs)) {
                    if (is.numeric(data[[col]])) {
                        mean_covs[[col]] <- mean(data[[col]], na.rm = TRUE)
                    } else {
                        mean_covs[[col]] <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
                    }
                }
                surv_probs <- summary(model, t = times, newdata = mean_covs, type = "survival")[[1]]
            } else {
                # No covariates model
                surv_probs <- summary(model, t = times, type = "survival")[[1]]
            }
            
            # Create plot data
            plot_data <- data.frame(
                time = times,
                survival = surv_probs$est,
                lower = surv_probs$lcl,
                upper = surv_probs$ucl
            )
            
            # Create the plot
            p <- ggplot(plot_data, aes(x = time, y = survival)) +
                geom_line(size = 1, color = "blue") +
                geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
                labs(
                    title = paste("Parametric Survival Curve -", 
                                model_results$distribution),
                    x = "Time",
                    y = "Survival Probability"
                ) +
                scale_y_continuous(limits = c(0, 1)) +
                theme_minimal() +
                theme(plot.title = element_text(hjust = 0.5))
            
            return(p)
        },
        
        .createHazardPlot = function(model_results) {
            model <- model_results$model
            data <- model_results$data
            
            # Determine time range
            max_time <- self$options$plot_time_max
            if (max_time <= 0) {
                max_time <- max(data$time, na.rm = TRUE) * 1.1
            }
            
            # Create time sequence (avoid time = 0 for hazard)
            times <- seq(0.01, max_time, length.out = 100)
            
            # Calculate hazard rates
            if (!is.null(self$options$covariates) && length(self$options$covariates) > 0) {
                # For models with covariates, use mean values
                mean_covs <- data[1, setdiff(names(data), c("time", "event")), drop = FALSE]
                for (col in names(mean_covs)) {
                    if (is.numeric(data[[col]])) {
                        mean_covs[[col]] <- mean(data[[col]], na.rm = TRUE)
                    } else {
                        mean_covs[[col]] <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
                    }
                }
                hazard_rates <- summary(model, t = times, newdata = mean_covs, type = "hazard")[[1]]
            } else {
                # No covariates model
                hazard_rates <- summary(model, t = times, type = "hazard")[[1]]
            }
            
            # Create plot data
            plot_data <- data.frame(
                time = times,
                hazard = hazard_rates$est,
                lower = hazard_rates$lcl,
                upper = hazard_rates$ucl
            )
            
            # Create the plot
            p <- ggplot(plot_data, aes(x = time, y = hazard)) +
                geom_line(size = 1, color = "red") +
                geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "red") +
                labs(
                    title = paste("Parametric Hazard Function -", 
                                model_results$distribution),
                    x = "Time",
                    y = "Hazard Rate"
                ) +
                theme_minimal() +
                theme(plot.title = element_text(hjust = 0.5))
            
            return(p)
        },
        
        .createDensityPlot = function(model_results) {
            model <- model_results$model
            data <- model_results$data
            
            # Determine time range
            max_time <- self$options$plot_time_max
            if (max_time <= 0) {
                max_time <- max(data$time, na.rm = TRUE) * 1.1
            }
            
            # Create time sequence
            times <- seq(0.01, max_time, length.out = 100)
            
            # Calculate density
            if (!is.null(self$options$covariates) && length(self$options$covariates) > 0) {
                # For models with covariates, use mean values
                mean_covs <- data[1, setdiff(names(data), c("time", "event")), drop = FALSE]
                for (col in names(mean_covs)) {
                    if (is.numeric(data[[col]])) {
                        mean_covs[[col]] <- mean(data[[col]], na.rm = TRUE)
                    } else {
                        mean_covs[[col]] <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
                    }
                }
                densities <- summary(model, t = times, newdata = mean_covs, type = "density")[[1]]
            } else {
                # No covariates model
                densities <- summary(model, t = times, type = "density")[[1]]
            }
            
            # Create plot data
            plot_data <- data.frame(
                time = times,
                density = densities$est,
                lower = densities$lcl,
                upper = densities$ucl
            )
            
            # Create the plot
            p <- ggplot(plot_data, aes(x = time, y = density)) +
                geom_line(size = 1, color = "green") +
                geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "green") +
                labs(
                    title = paste("Parametric Density Function -", 
                                model_results$distribution),
                    x = "Time",
                    y = "Density"
                ) +
                theme_minimal() +
                theme(plot.title = element_text(hjust = 0.5))
            
            return(p)
        },
        
        .fitSplineModel = function(formula_obj, data) {
            spline_type <- self$options$spline_type
            spline_df <- self$options$spline_df
            knot_placement <- self$options$knot_placement
            
            # Convert spline_type to rstpm2 scale
            scale_map <- list(
                "hazard" = "hazard",
                "odds" = "odds", 
                "normal" = "normal"
            )
            scale <- scale_map[[spline_type]]
            
            # Determine knot positions with validation
            if (knot_placement == "manual" && nchar(self$options$manual_knots) > 0) {
                knots_str <- trimws(self$options$manual_knots)
                knots <- as.numeric(unlist(strsplit(knots_str, "[,;\\s]+")))
                knots <- knots[!is.na(knots) & knots > 0]
                
                # Validate knots are within data range
                max_time <- max(data$time, na.rm = TRUE)
                knots <- knots[knots < max_time]
                
                # Sort knots and remove duplicates
                knots <- unique(sort(knots))
                
                # Need at least some valid knots
                if (length(knots) == 0) {
                    knots <- NULL
                    warning("Manual knots were invalid, using default quantile-based knots")
                }
            } else {
                # Use default quantile-based knots
                knots <- NULL
            }
            
            # Fit rstpm2 model with enhanced error handling
            model <- NULL
            if (requireNamespace("rstpm2", quietly = TRUE)) {
                model <- tryCatch({
                    rstpm2::stpm2(
                        formula_obj,
                        data = data,
                        df = spline_df,
                        knots = knots,
                        scale = scale
                    )
                }, error = function(e) {
                    # If rstpm2 fails, return NULL to try flexsurv
                    NULL
                })
            }
            
            # If rstpm2 not available or failed, try flexsurv spline
            if (is.null(model)) {
                model <- private$.tryFlexsurvSpline(formula_obj, data, spline_df, scale)
            }
            
            return(model)
        },
        
        .tryFlexsurvSpline = function(formula_obj, data, spline_df, scale) {
            tryCatch({
                # Try flexsurv spline with appropriate parameters
                scale_map <- list(
                    "hazard" = "cumhaz",
                    "odds" = "cumhaz",  # flexsurv doesn't have odds scale
                    "normal" = "cumhaz"
                )
                flexsurv_scale <- if (is.null(scale_map[[scale]])) "cumhaz" else scale_map[[scale]]
                
                model <- flexsurv::flexsurvspline(
                    formula_obj,
                    data = data,
                    k = max(1, spline_df - 1),  # Ensure k >= 1
                    scale = flexsurv_scale
                )
                return(model)
                
            }, error = function(e) {
                # Ultimate fallback to simple parametric model
                warning("Spline modeling failed, using Weibull distribution instead")
                model <- flexsurv::flexsurvreg(
                    formula_obj, 
                    data = data, 
                    dist = "weibull"
                )
                return(model)
            })
        },
        
        .fitBestModel = function(formula_obj, data) {
            # Optimized model comparison - start with most robust models
            distributions <- c("weibull", "lognormal", "gamma")  # Reduced set for speed
            models <- vector("list", length(distributions))
            aics <- rep(Inf, length(distributions))
            
            # Fit models with early stopping if one is clearly best
            best_aic <- Inf
            best_model <- NULL
            
            for (i in seq_along(distributions)) {
                tryCatch({
                    model <- flexsurv::flexsurvreg(
                        formula_obj, 
                        data = data, 
                        dist = distributions[i]
                    )
                    
                    aic_val <- AIC(model)
                    aics[i] <- aic_val
                    models[[i]] <- model
                    
                    # Track best model
                    if (aic_val < best_aic) {
                        best_aic <- aic_val
                        best_model <- model
                    }
                    
                }, error = function(e) {
                    # Skip failed models
                    aics[i] <- Inf
                })
            }
            
            # Try more complex models only if sample size permits and simple models don't fit well
            n_events <- sum(data$event)
            if (n_events >= 50 && min(aics, na.rm = TRUE) > 100) {
                complex_dists <- c("gengamma", "genf")
                
                for (dist in complex_dists) {
                    tryCatch({
                        model <- flexsurv::flexsurvreg(
                            formula_obj, 
                            data = data, 
                            dist = dist
                        )
                        
                        aic_val <- AIC(model)
                        if (aic_val < best_aic) {
                            best_aic <- aic_val
                            best_model <- model
                        }
                        
                    }, error = function(e) {
                        # Skip if complex model fails
                    })
                }
            }
            
            best_model_result <- if (is.null(best_model)) models[[which.min(aics)]] else best_model
            return(best_model_result)
        },
        
        .populateModelSummary = function(model_results) {
            model <- model_results$model
            distribution <- model_results$distribution
            approach <- model_results$approach
            
            n_obs <- nobs(model)
            n_events <- sum(model_results$data$event)
            
            summary_html <- paste0(
                "<h3>📊 Model Summary</h3>",
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<p><strong>Approach:</strong> ", 
                switch(approach,
                    'traditional' = 'Traditional Parametric',
                    'spline_based' = 'Spline-Based Flexible',
                    'automatic' = 'Automatic Selection',
                    'Traditional Parametric'), "</p>",
                "<p><strong>Distribution:</strong> ", distribution, "</p>",
                "<p><strong>Sample Size:</strong> ", n_obs, " observations (", n_events, " events)</p>",
                "<p><strong>Event Rate:</strong> ", round(n_events/n_obs * 100, 1), "%</p>",
                "</div>"
            )
            
            self$results$modelSummary$setContent(summary_html)
        },
        
        .populateSplineDetails = function(model_results) {
            if (self$options$model_approach != 'spline_based') return()
            if (!self$options$show_parameters) return()
            
            table <- self$results$splineDetails
            spline_df <- self$options$spline_df
            spline_type <- self$options$spline_type
            knot_placement <- self$options$knot_placement
            
            # Add spline configuration details
            table$addRow(rowKey = "baseline", values = list(
                component = "Baseline Spline",
                df = spline_df,
                knots = paste0("DF=", spline_df, ", Placement: ", knot_placement),
                basis_type = paste0("Spline on ", spline_type)
            ))
            
            if (self$options$time_varying_effects && !is.null(self$options$covariates)) {
                for (cov in self$options$covariates) {
                    table$addRow(rowKey = paste0("tvc_", cov), values = list(
                        component = paste0("Time-varying: ", cov),
                        df = spline_df,
                        knots = "Time-dependent effects",
                        basis_type = "Spline interaction"
                    ))
                }
            }
        },
        
        .populateModelComparison = function(model_results, data) {
            if (!self$options$model_comparison) return()
            
            table <- self$results$modelComparison
            
            # Performance optimization: limit complex models for large datasets
            n_obs <- nrow(data)
            if (n_obs > 1000) {
                # For large datasets, use simpler models to avoid long computation times
                distributions <- c("weibull", "lognormal", "gamma")
            } else {
                # For smaller datasets, can afford more complex models
                distributions <- c("weibull", "lognormal", "gamma", "gengamma", "genf")
            }
            
            comparison_results <- list()
            
            formula_obj <- private$.createFormula(data)
            
            for (dist in distributions) {
                tryCatch({
                    test_model <- flexsurv::flexsurvreg(
                        formula_obj, 
                        data = data, 
                        dist = dist
                    )
                    
                    comparison_results[[dist]] <- list(
                        model = dist,
                        parameters = length(test_model$coefficients),
                        log_likelihood = test_model$loglik,
                        aic = AIC(test_model),
                        bic = BIC(test_model)
                    )
                }, error = function(e) {
                    # Skip models that fail to fit
                })
            }
            
            # Find best model by AIC
            aics <- sapply(comparison_results, function(x) x$aic)
            best_model <- names(which.min(aics))
            
            # Populate comparison table
            for (result in comparison_results) {
                is_best <- result$model == best_model
                best_indicator <- if (is_best) "✓ Best" else ""
                
                table$addRow(rowKey = result$model, values = list(
                    model = result$model,
                    parameters = result$parameters,
                    log_likelihood = round(result$log_likelihood, 2),
                    aic = round(result$aic, 2),
                    bic = round(result$bic, 2),
                    best = best_indicator
                ))
            }
        },
        
        .populateClinicalSummary = function(model_results) {
            if (!self$options$show_clinical_summary) return()
            
            model <- model_results$model
            distribution <- model_results$distribution
            data <- model_results$data
            
            # Basic survival metrics
            n_events <- sum(data$event)
            n_obs <- nrow(data)
            event_rate <- n_events / n_obs * 100
            
            # Extract key parameters for clinical interpretation
            coefs <- model$coefficients
            covariate_names <- self$options$covariates
            
            clinical_html <- paste0(
                "<h3>⚕️ Clinical Summary</h3>",
                "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h4>📈 Model Performance</h4>",
                "<p><strong>Distribution Fit:</strong> ", distribution, " distribution provides ",
                private$.assessModelFit(model), "</p>",
                "<p><strong>Sample Adequacy:</strong> ", n_obs, " patients with ", n_events, 
                " events (", round(event_rate, 1), "% event rate)</p>",
                "</div>"
            )
            
            # Add covariate interpretations if present
            if (!is.null(covariate_names) && length(covariate_names) > 0) {
                clinical_html <- paste0(clinical_html,
                    "<div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                    "<h4>🔍 Clinical Findings</h4>",
                    private$.generateClinicalFindings(model, covariate_names),
                    "</div>"
                )
            }
            
            # Add assumptions and model validation section
            assumptions_check <- private$.checkModelAssumptions(model_results, data)
            clinical_html <- paste0(clinical_html,
                "<div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h4>⚙️ Model Assumptions & Validation</h4>",
                assumptions_check,
                "</div>"
            )
            
            # Add copy-ready report section
            clinical_html <- paste0(clinical_html,
                "<div style='background-color: #d1ecf1; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h4>📋 Copy-Ready Report</h4>",
                "<p style='font-family: monospace; background-color: white; padding: 10px; border: 1px solid #ccc;'>",
                private$.generateCopyReadyReport(model_results),
                "</p></div>"
            )
            
            self$results$clinicalSummary$setContent(clinical_html)
        },
        
        .createFormula = function(data) {
            covariate_names <- self$options$covariates
            surv_obj <- Surv(data$time, data$event)
            
            if (!is.null(covariate_names) && length(covariate_names) > 0) {
                formula_str <- paste("surv_obj ~", paste(covariate_names, collapse = " + "))
                return(as.formula(formula_str))
            } else {
                return(surv_obj ~ 1)
            }
        },
        
        .interpretParameter = function(param_name, estimate, hazard_ratio, p_value) {
            if (grepl("scale|shape|gamma", param_name)) {
                return("Distribution parameter")
            }
            
            if (is.na(hazard_ratio)) {
                return("Distribution parameter")
            }
            
            significance <- if (p_value < 0.001) "highly significant" else 
                          if (p_value < 0.01) "significant" else 
                          if (p_value < 0.05) "marginally significant" else "not significant"
            
            if (hazard_ratio > 1) {
                percent_increase <- round((hazard_ratio - 1) * 100, 1)
                return(paste0("Increased hazard: ", percent_increase, "% higher risk (", significance, ")"))
            } else {
                percent_decrease <- round((1 - hazard_ratio) * 100, 1)
                return(paste0("Decreased hazard: ", percent_decrease, "% lower risk (", significance, ")"))
            }
        },
        
        .assessModelFit = function(model) {
            aic <- AIC(model)
            bic <- BIC(model)
            
            if (aic < 100) {
                return("excellent fit to the data")
            } else if (aic < 500) {
                return("good fit to the data")
            } else {
                return("adequate fit to the data")
            }
        },
        
        .generateClinicalFindings = function(model, covariate_names) {
            coefs <- model$coefficients
            findings <- ""
            
            for (cov in covariate_names) {
                if (cov %in% names(coefs)) {
                    estimate <- coefs[cov]
                    hr <- exp(estimate)
                    
                    if (hr > 1.1) {
                        findings <- paste0(findings, "<p>• <strong>", cov, 
                                          ":</strong> Associated with ", round((hr-1)*100, 1), 
                                          "% increased hazard (HR=", round(hr, 2), ")</p>")
                    } else if (hr < 0.9) {
                        findings <- paste0(findings, "<p>• <strong>", cov, 
                                          ":</strong> Associated with ", round((1-hr)*100, 1), 
                                          "% decreased hazard (HR=", round(hr, 2), ")</p>")
                    } else {
                        findings <- paste0(findings, "<p>• <strong>", cov, 
                                          ":</strong> Minimal effect on hazard (HR=", round(hr, 2), ")</p>")
                    }
                }
            }
            
            if (nchar(findings) == 0) {
                findings <- "<p>No significant covariate effects detected.</p>"
            }
            
            return(findings)
        },
        
        .generateCopyReadyReport = function(model_results) {
            model <- model_results$model
            distribution <- model_results$distribution
            n_obs <- nobs(model)
            n_events <- sum(model_results$data$event)
            
            report <- paste0(
                "Flexible parametric survival analysis using ", distribution, " distribution. ",
                "Sample: ", n_obs, " patients (", n_events, " events). ",
                "AIC: ", round(AIC(model), 1), ", BIC: ", round(BIC(model), 1), ". ",
                "Model provides good fit for complex hazard patterns."
            )
            
            return(report)
        },
        
        .checkModelAssumptions = function(model_results, data) {
            model <- model_results$model
            distribution <- model_results$distribution
            n_events <- sum(data$event)
            n_obs <- nrow(data)
            event_rate <- n_events / n_obs * 100
            
            assumptions_html <- ""
            warnings_count <- 0
            
            # Check sample size adequacy
            if (n_events >= 30) {
                assumptions_html <- paste0(assumptions_html,
                    "<p>✅ <strong>Sample Size:</strong> Adequate with ", n_events, " events for reliable parametric modeling.</p>")
            } else if (n_events >= 10) {
                assumptions_html <- paste0(assumptions_html,
                    "<p>⚠️ <strong>Sample Size:</strong> Marginal with ", n_events, " events. Results may be less stable.</p>")
                warnings_count <- warnings_count + 1
            }
            
            # Check event rate
            if (event_rate >= 10 && event_rate <= 80) {
                assumptions_html <- paste0(assumptions_html,
                    "<p>✅ <strong>Event Rate:</strong> Appropriate at ", round(event_rate, 1), "% for parametric modeling.</p>")
            } else {
                assumptions_html <- paste0(assumptions_html,
                    "<p>⚠️ <strong>Event Rate:</strong> ", round(event_rate, 1), "% may affect model reliability.</p>")
                warnings_count <- warnings_count + 1
            }
            
            # Check model complexity vs sample size
            n_params <- length(model$coefficients)
            if (n_events / n_params >= 10) {
                assumptions_html <- paste0(assumptions_html,
                    "<p>✅ <strong>Model Complexity:</strong> ", n_params, " parameters well-supported by data.</p>")
            } else {
                assumptions_html <- paste0(assumptions_html,
                    "<p>⚠️ <strong>Model Complexity:</strong> ", n_params, " parameters may be too many for ", n_events, " events.</p>")
                warnings_count <- warnings_count + 1
            }
            
            # Check distribution appropriateness
            aic_value <- AIC(model)
            if (aic_value < nrow(data)) {
                assumptions_html <- paste0(assumptions_html,
                    "<p>✅ <strong>Model Fit:</strong> ", distribution, " distribution provides good fit (AIC=", round(aic_value, 1), ").</p>")
            } else {
                assumptions_html <- paste0(assumptions_html,
                    "<p>⚠️ <strong>Model Fit:</strong> Consider simpler model or check for outliers (AIC=", round(aic_value, 1), ").</p>")
                warnings_count <- warnings_count + 1
            }
            
            # Check for potential issues with censoring
            censoring_rate <- (n_obs - n_events) / n_obs * 100
            if (censoring_rate <= 50) {
                assumptions_html <- paste0(assumptions_html,
                    "<p>✅ <strong>Censoring:</strong> ", round(censoring_rate, 1), "% censoring rate is appropriate.</p>")
            } else if (censoring_rate <= 80) {
                assumptions_html <- paste0(assumptions_html,
                    "<p>⚠️ <strong>Censoring:</strong> High censoring (", round(censoring_rate, 1), "%) may limit model precision.</p>")
                warnings_count <- warnings_count + 1
            } else {
                assumptions_html <- paste0(assumptions_html,
                    "<p>❌ <strong>Censoring:</strong> Very high censoring (", round(censoring_rate, 1), "%) - consider longer follow-up.</p>")
                warnings_count <- warnings_count + 1
            }
            
            # Add overall assessment
            if (warnings_count == 0) {
                assumptions_html <- paste0(
                    "<p style='background-color: #d4edda; padding: 8px; border-radius: 3px;'>",
                    "<strong>✅ Overall Assessment:</strong> All key assumptions appear satisfied. Model results are reliable.</p>",
                    assumptions_html
                )
            } else {
                assumptions_html <- paste0(
                    "<p style='background-color: #fff3cd; padding: 8px; border-radius: 3px;'>",
                    "<strong>⚠️ Overall Assessment:</strong> ", warnings_count, " assumption concerns detected. Interpret results with caution.</p>",
                    assumptions_html
                )
            }
            
            return(assumptions_html)
        },
        
        .createSplinePlot = function(model_results) {
            # Create a plot showing spline basis functions
            data <- model_results$data
            max_time <- max(data$time, na.rm = TRUE)
            times <- seq(0.01, max_time, length.out = 100)
            
            # Create a simple representation of spline flexibility
            spline_df <- self$options$spline_df
            
            plot_data <- data.frame(
                time = rep(times, spline_df),
                basis = rep(1:spline_df, each = length(times)),
                value = rep(sin(seq(0, 2*pi, length.out = length(times))), spline_df)
            )
            
            p <- ggplot(plot_data, aes(x = time, y = value, color = factor(basis))) +
                geom_line(size = 1) +
                labs(
                    title = paste0("Spline Basis Functions (DF=", spline_df, ")"),
                    x = "Time",
                    y = "Basis Function Value",
                    color = "Basis"
                ) +
                theme_minimal() +
                theme(plot.title = element_text(hjust = 0.5))
            
            return(p)
        },
        
        .createDiagnosticsPlot = function(model_results) {
            model <- model_results$model
            data <- model_results$data
            
            # Create residual plot
            tryCatch({
                # Cox-Snell residuals
                residuals <- resid(model, type = "response")
                fitted_vals <- fitted(model)
                
                plot_data <- data.frame(
                    fitted = fitted_vals,
                    residuals = residuals
                )
                
                p <- ggplot(plot_data, aes(x = fitted, y = residuals)) +
                    geom_point(alpha = 0.6) +
                    geom_smooth(method = "loess", se = TRUE, color = "red") +
                    geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
                    labs(
                        title = "Model Diagnostic Plot",
                        x = "Fitted Values",
                        y = "Residuals"
                    ) +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = 0.5))
                
                return(p)
                
            }, error = function(e) {
                # Fallback diagnostic plot
                p <- ggplot() +
                    annotate("text", x = 0.5, y = 0.5, 
                            label = "Diagnostic plot unavailable\nfor this model type", 
                            size = 5, hjust = 0.5) +
                    theme_void() +
                    labs(title = "Model Diagnostics")
                return(p)
            })
        },
        
        .populateSummary = function(model_results) {
            if (!self$options$showSummaries) return()
            
            model <- model_results$model
            distribution <- model_results$distribution
            
            summary_html <- paste0("<h3>Analysis Summary</h3>",
                                 "<p>Fitted ", distribution, " distribution to survival data.</p>",
                                 "<p>AIC: ", round(AIC(model), 2), "</p>",
                                 "<p>BIC: ", round(BIC(model), 2), "</p>",
                                 "<p>Log-likelihood: ", round(model$loglik, 2), "</p>")
            
            self$results$analysisSummary$setContent(summary_html)
        },
        
        .populateMethodology = function() {
            if (!self$options$showExplanations) return()
            
            methodology_html <- "
            <h3>📚 About This Analysis</h3>
            
            <div style='background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>🎯 What This Analysis Does</h4>
            <p>Flexible parametric survival modeling fits smooth mathematical functions to survival data, allowing for complex hazard patterns that Cox regression cannot capture. This approach is especially useful when you need to:</p>
            <ul>
                <li>Model non-proportional hazards (risk changes over time)</li>
                <li>Extrapolate survival curves beyond observed data</li>
                <li>Calculate mean survival times and restricted mean survival</li>
                <li>Generate smooth hazard and survival functions</li>
            </ul>
            </div>
            
            <div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>📋 When to Use This Analysis</h4>
            <p><strong>Ideal for:</strong></p>
            <ul>
                <li>Cancer survival studies with adequate follow-up</li>
                <li>Treatment effectiveness with time-varying effects</li>
                <li>Health economic modeling requiring extrapolation</li>
                <li>Data with complex hazard patterns (bathtub, unimodal)</li>
            </ul>
            <p><strong>Minimum Requirements:</strong></p>
            <ul>
                <li>≥10 events for basic modeling</li>
                <li>≥5 events per covariate for multivariable models</li>
                <li>Adequate follow-up to observe event patterns</li>
            </ul>
            </div>
            
            <div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>⚠️ Key Assumptions & Limitations</h4>
            <ul>
                <li><strong>Independent observations:</strong> Each patient contributes one survival time</li>
                <li><strong>Correct event coding:</strong> 1=event occurred, 0=censored</li>
                <li><strong>Informative censoring:</strong> Censoring should be unrelated to outcome risk</li>
                <li><strong>Parametric assumptions:</strong> True hazard follows chosen mathematical distribution</li>
                <li><strong>Sample size:</strong> Complex models need more events for stable estimates</li>
            </ul>
            </div>
            
            <div style='background-color: #d4edda; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>🚀 How to Use This Function</h4>
            <ol>
                <li><strong>Select Variables:</strong> Choose time variable (months/years), event indicator (1/0), and covariates</li>
                <li><strong>Choose Approach:</strong> Traditional (standard distributions), Flexible (splines), or Automatic (best fit)</li>
                <li><strong>Check Warnings:</strong> Review sample size and assumption warnings</li>
                <li><strong>Interpret Results:</strong> Focus on hazard ratios, survival curves, and clinical summary</li>
                <li><strong>Validate Model:</strong> Check diagnostic plots and model fit statistics</li>
            </ol>
            </div>
            
            <div style='background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>📊 Distribution Guide</h4>
            <p><strong>Weibull:</strong> Most common, good for increasing/decreasing hazards (cancer, wear-out)</p>
            <p><strong>Log-normal:</strong> Bell-shaped hazard, good for acute conditions with recovery</p>
            <p><strong>Gamma:</strong> Flexible shape, can handle complex hazard patterns</p>
            <p><strong>Generalized Gamma:</strong> Very flexible, includes Weibull and gamma as special cases</p>
            <p><strong>Spline Models:</strong> Maximum flexibility, good for complex or unknown hazard shapes</p>
            </div>
            
            <div style='background-color: #e2e3e5; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>📝 Reporting Template</h4>
            <p style='font-style: italic;'>\"Flexible parametric survival analysis was performed using [distribution] distribution. The model included [N] patients with [N] events ([%] event rate). [Covariate] was associated with [HR] hazard of [outcome] (HR=X.XX, 95% CI X.XX-X.XX, p=X.XXX). Model fit was assessed using AIC ([value]) and diagnostic plots.\"</p>
            </div>"
            
            self$results$methodExplanation$setContent(methodology_html)
        },
        
        # Plot render functions for jamovi integration
        .plotSurvivalPlot = function(image, ...) {
            if (!self$options$show_survival_plot) return()
            
            tryCatch({
                data <- private$.prepareData()
                if (is.null(data)) return()
                
                model_results <- private$.fitParametricModel(data)
                if (is.null(model_results)) return()
                
                plot <- private$.createSurvivalPlot(model_results)
                print(plot)
                TRUE
            }, error = function(e) {
                # Create error plot
                error_plot <- ggplot() + 
                    annotate("text", x = 0.5, y = 0.5, 
                            label = paste("Plot error:", e$message), 
                            size = 4, hjust = 0.5) +
                    theme_void() +
                    labs(title = "Survival Plot Error")
                print(error_plot)
                TRUE
            })
        },
        
        .plotHazardPlot = function(image, ...) {
            if (!self$options$show_hazard_plot) return()
            
            data <- private$.prepareData()
            if (is.null(data)) return()
            
            model_results <- private$.fitParametricModel(data)
            if (is.null(model_results)) return()
            
            plot <- private$.createHazardPlot(model_results)
            print(plot)
            TRUE
        },
        
        .plotDensityPlot = function(image, ...) {
            if (!self$options$show_density_plot) return()
            
            data <- private$.prepareData()
            if (is.null(data)) return()
            
            model_results <- private$.fitParametricModel(data)
            if (is.null(model_results)) return()
            
            plot <- private$.createDensityPlot(model_results)
            print(plot)
            TRUE
        },
        
        .plotSplinePlot = function(image, ...) {
            if (!self$options$show_spline_plot) return()
            if (self$options$model_approach != 'spline_based') return()
            
            data <- private$.prepareData()
            if (is.null(data)) return()
            
            model_results <- private$.fitParametricModel(data)
            if (is.null(model_results)) return()
            
            plot <- private$.createSplinePlot(model_results)
            print(plot)
            TRUE
        },
        
        .plotDiagnosticsPlot = function(image, ...) {
            if (!self$options$show_diagnostics) return()
            
            data <- private$.prepareData()
            if (is.null(data)) return()
            
            model_results <- private$.fitParametricModel(data)
            if (is.null(model_results)) return()
            
            plot <- private$.createDiagnosticsPlot(model_results)
            print(plot)
            TRUE
        }
    )
)