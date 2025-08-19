#' @title Flexible Parametric Survival Models
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @import flexsurv
#' @import ggplot2
#' @import dplyr
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
            
            # Fit parametric model
            model_results <- private$.fitParametricModel(data)
            if (is.null(model_results)) return()
            
            # Populate results
            private$.populateParametersTable(model_results)
            private$.populateFitStatistics(model_results)
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
        
        .fitParametricModel = function(data) {
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
                
                # Fit the model using flexsurv
                if (distribution %in% c("gengamma", "genf", "gengamma_orig", "genf_orig")) {
                    # Flexible parametric distributions
                    model <- flexsurv::flexsurvreg(
                        formula_obj, 
                        data = data, 
                        dist = distribution
                    )
                } else {
                    # Standard parametric distributions
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
                    distribution = distribution
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
                
                table$addRow(rowKey = i, values = list(
                    parameter = param_name,
                    estimate = estimate,
                    se = se,
                    lower_ci = lower_ci,
                    upper_ci = upper_ci,
                    z_value = z_value,
                    p_value = p_value
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
            
            # Add statistics to table
            table$addRow(rowKey = "aic", values = list(
                statistic = "AIC",
                value = aic_value
            ))
            
            table$addRow(rowKey = "bic", values = list(
                statistic = "BIC",
                value = bic_value
            ))
            
            table$addRow(rowKey = "loglik", values = list(
                statistic = "Log-Likelihood",
                value = log_likelihood
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
            <h3>Flexible Parametric Survival Models</h3>
            <p><strong>Generalized Gamma:</strong> Flexible three-parameter distribution that includes exponential, Weibull, and gamma as special cases.</p>
            <p><strong>Generalized F:</strong> Four-parameter distribution providing even greater flexibility in hazard shapes.</p>
            <p><strong>Model Selection:</strong> Use AIC and BIC to compare different distributions and select the best fitting model.</p>
            <p><strong>Applications:</strong> Suitable for complex hazard patterns including increasing, decreasing, bathtub, or unimodal shapes.</p>"
            
            self$results$methodExplanation$setContent(methodology_html)
        }
    )
)