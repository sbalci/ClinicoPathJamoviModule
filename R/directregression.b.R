directregressionClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "directregressionClass",
    inherit = directregressionBase,
    private = list(
        .init = function() {
            if (is.null(self$options$elapsedtime) || is.null(self$options$outcome)) {
                self$results$todo$setContent(
                    "<p>Welcome to Direct Regression on Survival Function</p>
                     <p>To get started:</p>
                     <ol>
                       <li>Select the time variable</li>
                       <li>Select the event indicator</li>
                       <li>Add explanatory variables</li>
                       <li>Specify time points for analysis (comma-separated)</li>
                     </ol>
                     <p><strong>About Direct Regression:</strong></p>
                     <ul>
                       <li>Uses pseudo-observations to enable direct modeling of survival probabilities</li>
                       <li>Alternative to hazard-based models when interest is in survival function</li>
                       <li>Allows standard regression techniques for survival data</li>
                     </ul>"
                )
                self$results$todo$setVisible(TRUE)
                return()
            }
            
            self$results$todo$setVisible(FALSE)
        },

        .run = function() {
            if (is.null(self$options$elapsedtime) || is.null(self$options$outcome)) {
                return()
            }

            # Load required packages
            if (!requireNamespace("survival", quietly = TRUE)) {
                stop("The 'survival' package is required for direct regression analysis. Please install it.")
            }
            
            if (!requireNamespace("pseudo", quietly = TRUE)) {
                stop("The 'pseudo' package is required for pseudo-observations. Please install it.")
            }

            data <- self$data
            time_var <- self$options$elapsedtime
            outcome_var <- self$options$outcome
            explanatory_vars <- self$options$explanatory
            outcome_level <- self$options$outcomeLevel

            # Parse time points
            time_points_str <- self$options$time_points
            time_points <- as.numeric(unlist(strsplit(time_points_str, ",")))
            time_points <- time_points[!is.na(time_points)]

            if (length(time_points) == 0) {
                stop("Please specify valid time points for analysis.")
            }

            # Prepare data
            data_clean <- private$.prepareData(data, time_var, outcome_var, explanatory_vars, outcome_level)

            # Calculate pseudo-observations
            pseudo_results <- private$.calculatePseudoObservations(
                data_clean, time_var, outcome_var, time_points
            )

            # Perform direct regression for each time point
            regression_results <- private$.performDirectRegression(
                pseudo_results, explanatory_vars, time_points
            )

            # Fill results tables
            private$.fillModelSummary(regression_results)

            if (self$options$show_pseudo_values) {
                private$.fillPseudoValues(pseudo_results, time_points)
            }

            if (self$options$model_comparison) {
                private$.fillModelComparison(regression_results, time_points)
            }

            if (self$options$show_residuals) {
                private$.fillResidualAnalysis(regression_results, time_points)
            }

            # Fill explanations
            if (self$options$showExplanations) {
                private$.fillMethodologyExplanation()
            }

            if (self$options$showSummaries) {
                private$.fillAnalysisSummary(regression_results, time_points)
            }

            # Generate plots
            if (self$options$survival_plot) {
                private$.generateSurvivalPlot(data_clean, regression_results, time_points)
            }

            if (self$options$residual_plot) {
                private$.generateResidualPlot(regression_results, time_points)
            }

            if (self$options$prediction_plot) {
                private$.generatePredictionPlot(regression_results, time_points)
            }
        },

        .prepareData = function(data, time_var, outcome_var, explanatory_vars, outcome_level) {
            # Prepare survival data
            data_clean <- data[complete.cases(data[c(time_var, outcome_var, explanatory_vars)]), ]
            
            # Convert outcome to numeric if needed
            if (is.factor(data_clean[[outcome_var]])) {
                data_clean[[paste0(outcome_var, "_numeric")]] <- as.numeric(data_clean[[outcome_var]] == outcome_level)
            } else {
                data_clean[[paste0(outcome_var, "_numeric")]] <- data_clean[[outcome_var]]
            }

            return(data_clean)
        },

        .calculatePseudoObservations = function(data_clean, time_var, outcome_var, time_points) {
            outcome_numeric <- paste0(outcome_var, "_numeric")
            
            # Create survival object
            surv_obj <- survival::Surv(data_clean[[time_var]], data_clean[[outcome_numeric]])
            
            # Calculate Kaplan-Meier estimate
            km_fit <- survival::survfit(surv_obj ~ 1)
            
            # Initialize results list
            pseudo_results <- list()
            
            for (i in seq_along(time_points)) {
                t_point <- time_points[i]
                
                tryCatch({
                    # Calculate pseudo-observations using pseudo package
                    if (self$options$pseudo_method == "jackknife") {
                        pseudo_vals <- pseudo::pseudosurv(time = data_clean[[time_var]], 
                                                         event = data_clean[[outcome_numeric]], 
                                                         tmax = t_point)
                    } else {
                        # Alternative calculation for other methods
                        # Get survival probability at time point
                        surv_prob <- summary(km_fit, times = t_point, extend = TRUE)$surv
                        if (length(surv_prob) == 0) surv_prob <- 0
                        
                        # Simple pseudo-observation calculation
                        n <- nrow(data_clean)
                        pseudo_vals <- rep(surv_prob, n)
                        
                        # Adjust for individual contributions (simplified)
                        for (j in seq_len(n)) {
                            data_minus_j <- data_clean[-j, ]
                            if (nrow(data_minus_j) > 0) {
                                surv_obj_j <- survival::Surv(data_minus_j[[time_var]], data_minus_j[[outcome_numeric]])
                                km_fit_j <- survival::survfit(surv_obj_j ~ 1)
                                surv_prob_j <- summary(km_fit_j, times = t_point, extend = TRUE)$surv
                                if (length(surv_prob_j) == 0) surv_prob_j <- 0
                                pseudo_vals[j] <- n * surv_prob - (n - 1) * surv_prob_j
                            }
                        }
                    }
                    
                    pseudo_results[[paste0("t_", t_point)]] <- list(
                        time_point = t_point,
                        pseudo_values = pseudo_vals,
                        survival_prob = summary(km_fit, times = t_point, extend = TRUE)$surv
                    )
                }, error = function(e) {
                    # Fallback calculation
                    surv_prob <- summary(km_fit, times = t_point, extend = TRUE)$surv
                    if (length(surv_prob) == 0) surv_prob <- 0
                    
                    pseudo_results[[paste0("t_", t_point)]] <<- list(
                        time_point = t_point,
                        pseudo_values = rep(surv_prob, nrow(data_clean)),
                        survival_prob = surv_prob
                    )
                })
            }
            
            return(pseudo_results)
        },

        .performDirectRegression = function(pseudo_results, explanatory_vars, time_points) {
            regression_results <- list()
            
            for (i in seq_along(time_points)) {
                t_point <- time_points[i]
                pseudo_key <- paste0("t_", t_point)
                
                if (!pseudo_key %in% names(pseudo_results)) {
                    next
                }
                
                # Prepare regression data
                pseudo_vals <- pseudo_results[[pseudo_key]]$pseudo_values
                reg_data <- self$data[complete.cases(self$data[c(self$options$elapsedtime, self$options$outcome, explanatory_vars)]), ]
                reg_data$pseudo_value <- pseudo_vals
                
                # Build formula
                if (length(explanatory_vars) > 0) {
                    formula_str <- paste("pseudo_value ~", paste(explanatory_vars, collapse = " + "))
                } else {
                    formula_str <- "pseudo_value ~ 1"
                }
                
                # Fit regression model
                tryCatch({
                    if (self$options$regression_type == "linear") {
                        if (self$options$link_function == "identity") {
                            model <- lm(as.formula(formula_str), data = reg_data)
                        } else {
                            # Use GLM with specified link
                            family_obj <- gaussian(link = self$options$link_function)
                            model <- glm(as.formula(formula_str), data = reg_data, family = family_obj)
                        }
                    } else if (self$options$regression_type == "logistic") {
                        model <- glm(as.formula(formula_str), data = reg_data, family = binomial())
                    } else {
                        # Default to linear
                        model <- lm(as.formula(formula_str), data = reg_data)
                    }
                    
                    # Store results
                    regression_results[[pseudo_key]] <- list(
                        time_point = t_point,
                        model = model,
                        summary = summary(model),
                        pseudo_values = pseudo_vals,
                        data = reg_data
                    )
                }, error = function(e) {
                    # Create dummy results if model fails
                    regression_results[[pseudo_key]] <<- list(
                        time_point = t_point,
                        model = NULL,
                        summary = NULL,
                        pseudo_values = pseudo_vals,
                        data = reg_data,
                        error = e$message
                    )
                })
            }
            
            return(regression_results)
        },

        .fillModelSummary = function(regression_results) {
            conf_level <- self$options$confidence_level
            
            for (key in names(regression_results)) {
                result <- regression_results[[key]]
                
                if (is.null(result$model) || is.null(result$summary)) {
                    next
                }
                
                # Extract coefficients
                coeffs <- result$summary$coefficients
                
                if (is.null(coeffs) || nrow(coeffs) == 0) {
                    next
                }
                
                # Calculate confidence intervals
                tryCatch({
                    ci_matrix <- confint(result$model, level = conf_level)
                    
                    for (i in seq_len(nrow(coeffs))) {
                        term_name <- rownames(coeffs)[i]
                        
                        self$results$modelSummary$addRow(
                            rowKey = paste(result$time_point, term_name, sep = "_"),
                            values = list(
                                time_point = result$time_point,
                                term = term_name,
                                estimate = coeffs[i, 1],
                                se = coeffs[i, 2],
                                statistic = coeffs[i, 3],
                                p = coeffs[i, 4],
                                ci_lower = ci_matrix[i, 1],
                                ci_upper = ci_matrix[i, 2]
                            )
                        )
                    }
                }, error = function(e) {
                    # Add without CI if calculation fails
                    for (i in seq_len(nrow(coeffs))) {
                        term_name <- rownames(coeffs)[i]
                        
                        self$results$modelSummary$addRow(
                            rowKey = paste(result$time_point, term_name, sep = "_"),
                            values = list(
                                time_point = result$time_point,
                                term = term_name,
                                estimate = coeffs[i, 1],
                                se = coeffs[i, 2],
                                statistic = coeffs[i, 3],
                                p = coeffs[i, 4],
                                ci_lower = NA,
                                ci_upper = NA
                            )
                        )
                    }
                })
            }
        },

        .fillPseudoValues = function(pseudo_results, time_points) {
            for (i in seq_along(time_points)) {
                t_point <- time_points[i]
                key <- paste0("t_", t_point)
                
                if (key %in% names(pseudo_results)) {
                    result <- pseudo_results[[key]]
                    pseudo_vals <- result$pseudo_values
                    surv_prob <- ifelse(length(result$survival_prob) > 0, result$survival_prob, NA)
                    
                    for (j in seq_along(pseudo_vals)) {
                        self$results$pseudoValues$addRow(
                            rowKey = paste(t_point, j, sep = "_"),
                            values = list(
                                observation = j,
                                time_point = t_point,
                                pseudo_value = pseudo_vals[j],
                                survival_prob = surv_prob
                            )
                        )
                    }
                }
            }
        },

        .fillModelComparison = function(regression_results, time_points) {
            for (key in names(regression_results)) {
                result <- regression_results[[key]]
                
                if (is.null(result$model)) {
                    next
                }
                
                # Calculate model statistics
                model_summary <- result$summary
                
                tryCatch({
                    r_squared <- ifelse("r.squared" %in% names(model_summary), model_summary$r.squared, NA)
                    adj_r_squared <- ifelse("adj.r.squared" %in% names(model_summary), model_summary$adj.r.squared, NA)
                    aic_val <- AIC(result$model)
                    bic_val <- BIC(result$model)
                    
                    # Calculate RMSE
                    residuals <- residuals(result$model)
                    rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
                    
                    self$results$modelComparison$addRow(
                        rowKey = as.character(result$time_point),
                        values = list(
                            time_point = result$time_point,
                            r_squared = r_squared,
                            adj_r_squared = adj_r_squared,
                            aic = aic_val,
                            bic = bic_val,
                            rmse = rmse
                        )
                    )
                }, error = function(e) {
                    # Add minimal info if calculations fail
                    self$results$modelComparison$addRow(
                        rowKey = as.character(result$time_point),
                        values = list(
                            time_point = result$time_point,
                            r_squared = NA,
                            adj_r_squared = NA,
                            aic = NA,
                            bic = NA,
                            rmse = NA
                        )
                    )
                })
            }
        },

        .fillResidualAnalysis = function(regression_results, time_points) {
            for (key in names(regression_results)) {
                result <- regression_results[[key]]
                
                if (is.null(result$model)) {
                    next
                }
                
                tryCatch({
                    residuals <- residuals(result$model)
                    mean_resid <- mean(residuals, na.rm = TRUE)
                    sd_resid <- sd(residuals, na.rm = TRUE)
                    
                    # Test normality of residuals
                    normality_p <- shapiro.test(residuals)$p.value
                    
                    self$results$residualAnalysis$addRow(
                        rowKey = as.character(result$time_point),
                        values = list(
                            time_point = result$time_point,
                            residual_type = "Raw Residuals",
                            mean_residual = mean_resid,
                            sd_residual = sd_resid,
                            normality_p = normality_p
                        )
                    )
                }, error = function(e) {
                    # Skip if residual analysis fails
                })
            }
        },

        .fillMethodologyExplanation = function() {
            html <- paste0(
                "<h4>Direct Regression on Survival Function</h4>",
                "<p><strong>Method Overview:</strong></p>",
                "<ul>",
                "<li><strong>Pseudo-observations:</strong> This method transforms survival data into pseudo-observations that can be analyzed using standard regression techniques.</li>",
                "<li><strong>Direct modeling:</strong> Unlike Cox regression that models hazards, this approach directly models survival probabilities at specified time points.</li>",
                "<li><strong>Interpretation:</strong> Regression coefficients represent the direct effect of covariates on survival probability.</li>",
                "</ul>",
                "<p><strong>Key Advantages:</strong></p>",
                "<ul>",
                "<li>Direct interpretation in terms of survival probabilities</li>",
                "<li>No proportional hazards assumption required</li>",
                "<li>Allows flexible modeling of covariate effects</li>",
                "<li>Can handle time-varying effects naturally</li>",
                "</ul>",
                "<p><strong>Pseudo-observation Calculation:</strong></p>",
                "<p>For each individual <em>i</em> and time point <em>t</em>:</p>",
                "<p><em>θ̂<sub>i</sub>(t) = n·Ŝ(t) - (n-1)·Ŝ<sub>-i</sub>(t)</em></p>",
                "<p>Where Ŝ(t) is the Kaplan-Meier estimate and Ŝ<sub>-i</sub>(t) is the estimate without observation <em>i</em>.</p>"
            )

            self$results$methodologyExplanation$setContent(html)
        },

        .fillAnalysisSummary = function(regression_results, time_points) {
            n_models <- length(regression_results)
            n_timepoints <- length(time_points)
            successful_models <- sum(sapply(regression_results, function(x) !is.null(x$model)))

            html <- paste0(
                "<h4>Analysis Summary</h4>",
                "<p><strong>Data Overview:</strong></p>",
                "<ul>",
                "<li>Time points analyzed: ", paste(time_points, collapse = ", "), "</li>",
                "<li>Successful models: ", successful_models, " of ", n_timepoints, "</li>",
                "<li>Regression type: ", self$options$regression_type, "</li>",
                "<li>Link function: ", self$options$link_function, "</li>",
                "<li>Pseudo-observation method: ", self$options$pseudo_method, "</li>",
                "</ul>",
                "<p><strong>Interpretation Guide:</strong></p>",
                "<ul>",
                "<li>Coefficients represent the change in survival probability associated with a one-unit change in the covariate</li>",
                "<li>Positive coefficients indicate increased survival probability</li>",
                "<li>Negative coefficients indicate decreased survival probability</li>",
                "</ul>"
            )

            self$results$analysisSummary$setContent(html)
        },

        .generateSurvivalPlot = function(data_clean, regression_results, time_points) {
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                return()
            }

            # Create Kaplan-Meier curve
            outcome_numeric <- paste0(self$options$outcome, "_numeric")
            surv_obj <- survival::Surv(data_clean[[self$options$elapsedtime]], data_clean[[outcome_numeric]])
            km_fit <- survival::survfit(surv_obj ~ 1)

            # Create base survival plot
            plot_data <- data.frame(
                time = km_fit$time,
                surv = km_fit$surv,
                upper = km_fit$upper,
                lower = km_fit$lower
            )

            plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = surv)) +
                ggplot2::geom_step(color = "blue", size = 1) +
                ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
                ggplot2::labs(
                    title = "Survival Function with Direct Regression Time Points",
                    x = "Time",
                    y = "Survival Probability"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::ylim(0, 1)

            # Add vertical lines for analysis time points
            for (t_point in time_points) {
                plot <- plot + ggplot2::geom_vline(xintercept = t_point, color = "red", linetype = "dashed", alpha = 0.7)
            }

            # Add time point labels
            for (i in seq_along(time_points)) {
                t_point <- time_points[i]
                surv_prob <- summary(km_fit, times = t_point, extend = TRUE)$surv
                if (length(surv_prob) > 0) {
                    plot <- plot + ggplot2::annotate("text", x = t_point, y = surv_prob + 0.05, 
                                                   label = paste0("t=", t_point), color = "red", size = 3)
                }
            }

            self$results$survivalPlot$setState(plot)
        },

        .generateResidualPlot = function(regression_results, time_points) {
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                return()
            }

            # Combine residuals from all time points
            all_residuals <- data.frame()
            
            for (key in names(regression_results)) {
                result <- regression_results[[key]]
                if (!is.null(result$model)) {
                    residuals_data <- data.frame(
                        time_point = result$time_point,
                        fitted = fitted(result$model),
                        residuals = residuals(result$model),
                        observation = seq_along(residuals(result$model))
                    )
                    all_residuals <- rbind(all_residuals, residuals_data)
                }
            }

            if (nrow(all_residuals) == 0) {
                return()
            }

            # Create residual plots
            plot <- ggplot2::ggplot(all_residuals, ggplot2::aes(x = fitted, y = residuals)) +
                ggplot2::geom_point(alpha = 0.6) +
                ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
                ggplot2::geom_smooth(method = "loess", se = TRUE, color = "blue") +
                ggplot2::facet_wrap(~ time_point, scales = "free") +
                ggplot2::labs(
                    title = "Residual Plots by Time Point",
                    x = "Fitted Values",
                    y = "Residuals"
                ) +
                ggplot2::theme_minimal()

            self$results$residualPlot$setState(plot)
        },

        .generatePredictionPlot = function(regression_results, time_points) {
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                return()
            }

            # Create prediction plot showing model predictions at each time point
            pred_data <- data.frame()
            
            for (key in names(regression_results)) {
                result <- regression_results[[key]]
                if (!is.null(result$model) && length(self$options$explanatory) > 0) {
                    # Get first explanatory variable for plotting
                    first_var <- self$options$explanatory[1]
                    
                    # Create prediction data
                    var_range <- range(result$data[[first_var]], na.rm = TRUE)
                    pred_values <- seq(var_range[1], var_range[2], length.out = 50)
                    
                    # Create prediction dataframe
                    pred_df <- data.frame(pred_values)
                    names(pred_df) <- first_var
                    
                    # Add other variables at their means
                    for (var in self$options$explanatory[-1]) {
                        if (is.numeric(result$data[[var]])) {
                            pred_df[[var]] <- mean(result$data[[var]], na.rm = TRUE)
                        } else {
                            pred_df[[var]] <- names(sort(table(result$data[[var]]), decreasing = TRUE))[1]
                        }
                    }
                    
                    # Get predictions
                    predictions <- predict(result$model, newdata = pred_df, se.fit = TRUE)
                    
                    temp_data <- data.frame(
                        time_point = result$time_point,
                        x_var = pred_values,
                        prediction = predictions$fit,
                        se = predictions$se.fit,
                        lower = predictions$fit - 1.96 * predictions$se.fit,
                        upper = predictions$fit + 1.96 * predictions$se.fit
                    )
                    
                    pred_data <- rbind(pred_data, temp_data)
                }
            }

            if (nrow(pred_data) == 0) {
                return()
            }

            # Create prediction plot
            plot <- ggplot2::ggplot(pred_data, ggplot2::aes(x = x_var, y = prediction)) +
                ggplot2::geom_line(color = "blue") +
                ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
                ggplot2::facet_wrap(~ time_point, scales = "free") +
                ggplot2::labs(
                    title = "Model Predictions by Time Point",
                    x = self$options$explanatory[1],
                    y = "Predicted Survival Probability"
                ) +
                ggplot2::theme_minimal()

            self$results$predictionPlot$setState(plot)
        }
    )
)