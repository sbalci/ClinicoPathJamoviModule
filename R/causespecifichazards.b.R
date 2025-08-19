#' @title Cause-Specific Hazards Models
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @importFrom stats anova
#' @export


causespecifichazardsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "causespecifichazardsClass",
    inherit = causespecifichazardsBase,
    private = list(

        .init = function() {
            # Initialize results tables and plots
            if (is.null(self$options$elapsedtime) || is.null(self$options$outcome)) {
                return()
            }
            
            # Set up the overview table
            overviewTable <- self$results$overview
            overviewTable$addRow(
                rowKey = "analysis",
                values = list(
                    analysis = "Cause-Specific Hazards Analysis",
                    outcome = paste(self$options$outcome, "~", self$options$elapsedtime),
                    n_subjects = "",
                    n_events = "",
                    n_causes = "",
                    model_type = self$options$model_type,
                    reference_cause = self$options$reference_cause
                )
            )
        },

        .run = function() {
            # Check if required variables are specified
            if (is.null(self$options$elapsedtime) || is.null(self$options$outcome)) {
                return()
            }
            
            # Get data
            data <- self$data
            
            if (nrow(data) == 0) {
                return()
            }
            
            # Extract variables
            timeVar <- self$options$elapsedtime
            eventVar <- self$options$outcome
            causeVar <- self$options$cause_variable
            covariates <- self$options$covariates
            
            # Validate data
            private$.validateData(data, timeVar, eventVar, causeVar, covariates)
            
            # Fit cause-specific hazards models
            tryCatch({
                # Prepare the data
                prepared_data <- private$.prepareData(data, timeVar, eventVar, causeVar, covariates)
                
                # Get unique causes
                causes <- private$.getUniqueCauses(prepared_data)
                
                # Fit models for each cause
                models <- private$.fitCauseSpecificModels(prepared_data, causes)
                
                if (!is.null(models) && length(models) > 0) {
                    # Populate results
                    private$.populateOverview(models, prepared_data, causes)
                    private$.populateCauseSummary(prepared_data, causes)
                    private$.populateModelFit(models, causes)
                    private$.populateCoefficients(models, causes)
                    
                    if (self$options$cumulative_incidence) {
                        private$.populateCumulativeIncidence(prepared_data, causes)
                    }
                    
                    if (self$options$model_comparison) {
                        private$.populateModelComparison(models, causes)
                    }
                    
                    if (self$options$proportional_hazards_test && self$options$model_type == "cox") {
                        private$.populateProportionalHazardsTest(models, causes)
                    }
                    
                    private$.populatePlots(models, prepared_data, causes)
                }
                
            }, error = function(e) {
                jmvcore::reject(paste("Error in cause-specific hazards analysis:", e$message))
            })
        },

        .validateData = function(data, timeVar, eventVar, causeVar, covariates) {
            # Check if variables exist
            if (!timeVar %in% names(data)) {
                jmvcore::reject(paste("Time variable", timeVar, "not found in data"))
            }
            
            if (!eventVar %in% names(data)) {
                jmvcore::reject(paste("Event variable", eventVar, "not found in data"))
            }
            
            if (!is.null(causeVar) && !causeVar %in% names(data)) {
                jmvcore::reject(paste("Cause variable", causeVar, "not found in data"))
            }
            
            # Check for missing covariates
            if (!is.null(covariates)) {
                missing_covs <- setdiff(covariates, names(data))
                if (length(missing_covs) > 0) {
                    jmvcore::reject(paste("Covariates not found in data:", paste(missing_covs, collapse = ", ")))
                }
            }
            
            # Validate time variable
            if (!is.numeric(data[[timeVar]]) || any(data[[timeVar]] <= 0, na.rm = TRUE)) {
                jmvcore::reject("Time variable must be positive numeric values")
            }
            
            # Check for multiple causes
            if (is.null(causeVar)) {
                event_values <- unique(data[[eventVar]][data[[eventVar]] != 0])
            } else {
                event_values <- unique(data[[causeVar]][!is.na(data[[causeVar]])])
            }
            
            if (length(event_values) < 2) {
                jmvcore::reject("At least 2 different causes must be present for competing risks analysis")
            }
        },

        .prepareData = function(data, timeVar, eventVar, causeVar, covariates) {
            # Build data frame for modeling
            model_data <- data.frame(
                time = data[[timeVar]],
                event = data[[eventVar]]
            )
            
            # Determine causes
            if (is.null(causeVar)) {
                # Use event variable directly
                model_data$cause <- data[[eventVar]]
            } else {
                # Use separate cause variable
                model_data$cause <- data[[causeVar]]
                # Set cause to 0 for censored observations
                model_data$cause[data[[eventVar]] == 0] <- 0
            }
            
            # Add covariates
            if (!is.null(covariates) && length(covariates) > 0) {
                for (var in covariates) {
                    model_data[[var]] <- data[[var]]
                }
            }
            
            # Remove rows with missing data
            complete_cases <- complete.cases(model_data)
            model_data <- model_data[complete_cases, ]
            
            return(model_data)
        },

        .getUniqueCauses = function(prepared_data) {
            # Get unique non-zero causes
            causes <- unique(prepared_data$cause[prepared_data$cause != 0])
            causes <- sort(causes)
            return(causes)
        },

        .fitCauseSpecificModels = function(prepared_data, causes) {
            models <- list()
            
            for (cause in causes) {
                # Create cause-specific dataset
                cause_data <- prepared_data
                
                # Create cause-specific event indicator
                # Event = 1 if this cause, 0 if censored or other cause
                cause_data$cause_event <- ifelse(cause_data$cause == cause, 1, 0)
                
                # Create survival object
                surv_obj <- survival::Surv(time = cause_data$time, event = cause_data$cause_event)
                cause_data$surv <- surv_obj
                
                # Build formula
                if (!is.null(self$options$covariates) && length(self$options$covariates) > 0) {
                    cov_formula <- paste(self$options$covariates, collapse = " + ")
                    formula_str <- paste("surv ~", cov_formula)
                } else {
                    formula_str <- "surv ~ 1"
                }
                
                formula_obj <- as.formula(formula_str)
                
                # Fit model based on type
                model <- private$.fitSingleCauseModel(formula_obj, cause_data, self$options$model_type)
                
                if (!is.null(model)) {
                    model$cause <- cause
                    model$cause_data <- cause_data
                    models[[as.character(cause)]] <- model
                }
            }
            
            return(models)
        },

        .fitSingleCauseModel = function(formula, data, model_type) {
            model <- switch(model_type,
                "cox" = {
                    survival::coxph(formula = formula, data = data)
                },
                "weibull" = {
                    survival::survreg(formula = formula, data = data, dist = "weibull")
                },
                "exponential" = {
                    survival::survreg(formula = formula, data = data, dist = "exponential")
                },
                "lognormal" = {
                    survival::survreg(formula = formula, data = data, dist = "lognormal")
                },
                {
                    # Default to Cox
                    survival::coxph(formula = formula, data = data)
                }
            )
            
            return(model)
        },

        .populateOverview = function(models, prepared_data, causes) {
            overviewTable <- self$results$overview
            
            # Calculate summary statistics
            n_subjects <- nrow(prepared_data)
            n_events <- sum(prepared_data$event, na.rm = TRUE)
            n_causes <- length(causes)
            
            # Update the overview table
            overviewTable$setRow(rowKey = "analysis", values = list(
                analysis = "Cause-Specific Hazards Analysis",
                outcome = paste(self$options$outcome, "~", self$options$elapsedtime),
                n_subjects = n_subjects,
                n_events = n_events,
                n_causes = n_causes,
                model_type = self$options$model_type,
                reference_cause = self$options$reference_cause
            ))
        },

        .populateCauseSummary = function(prepared_data, causes) {
            causeSummaryTable <- self$results$cause_summary
            
            for (cause in causes) {
                # Get events for this cause
                cause_events <- prepared_data[prepared_data$cause == cause, ]
                n_events <- nrow(cause_events)
                
                # Calculate proportion
                total_events <- sum(prepared_data$cause != 0)
                proportion <- n_events / total_events
                
                # Calculate quantiles
                times <- cause_events$time
                median_time <- ifelse(length(times) > 0, median(times, na.rm = TRUE), NA)
                q25_time <- ifelse(length(times) > 0, quantile(times, 0.25, na.rm = TRUE), NA)
                q75_time <- ifelse(length(times) > 0, quantile(times, 0.75, na.rm = TRUE), NA)
                
                causeSummaryTable$addRow(rowKey = as.character(cause), values = list(
                    cause = as.character(cause),
                    n_events = n_events,
                    proportion = proportion,
                    median_time = median_time,
                    q25_time = q25_time,
                    q75_time = q75_time
                ))
            }
        },

        .populateModelFit = function(models, causes) {
            modelFitTable <- self$results$model_fit
            
            for (cause in causes) {
                model <- models[[as.character(cause)]]
                
                tryCatch({
                    if (inherits(model, "coxph")) {
                        # Cox model statistics
                        loglik <- model$loglik[2]
                        aic <- -2 * loglik + 2 * length(model$coefficients)
                        concordance <- model$concordance["C"]
                        
                        modelFitTable$addRow(rowKey = paste(cause, "loglik", sep = "_"), 
                                           values = list(cause = as.character(cause), 
                                                       statistic = "Log-likelihood", 
                                                       value = loglik))
                        modelFitTable$addRow(rowKey = paste(cause, "aic", sep = "_"), 
                                           values = list(cause = as.character(cause), 
                                                       statistic = "AIC", 
                                                       value = aic))
                        modelFitTable$addRow(rowKey = paste(cause, "concordance", sep = "_"), 
                                           values = list(cause = as.character(cause), 
                                                       statistic = "Concordance", 
                                                       value = concordance))
                    } else if (inherits(model, "survreg")) {
                        # Parametric model statistics
                        loglik <- model$loglik[2]
                        aic <- -2 * loglik + 2 * length(model$coefficients)
                        bic <- -2 * loglik + log(model$n) * length(model$coefficients)
                        
                        modelFitTable$addRow(rowKey = paste(cause, "loglik", sep = "_"), 
                                           values = list(cause = as.character(cause), 
                                                       statistic = "Log-likelihood", 
                                                       value = loglik))
                        modelFitTable$addRow(rowKey = paste(cause, "aic", sep = "_"), 
                                           values = list(cause = as.character(cause), 
                                                       statistic = "AIC", 
                                                       value = aic))
                        modelFitTable$addRow(rowKey = paste(cause, "bic", sep = "_"), 
                                           values = list(cause = as.character(cause), 
                                                       statistic = "BIC", 
                                                       value = bic))
                    }
                }, error = function(e) {
                    modelFitTable$addRow(rowKey = paste(cause, "note", sep = "_"), 
                                       values = list(cause = as.character(cause), 
                                                   statistic = "Note", 
                                                   value = "Statistics unavailable"))
                })
            }
        },

        .populateCoefficients = function(models, causes) {
            coeffTable <- self$results$coefficients
            
            for (cause in causes) {
                model <- models[[as.character(cause)]]
                
                tryCatch({
                    if (inherits(model, "coxph")) {
                        # Cox model coefficients
                        summary_model <- summary(model)
                        coefs <- summary_model$coefficients
                        
                        for (i in 1:nrow(coefs)) {
                            param_name <- rownames(coefs)[i]
                            estimate <- coefs[i, "coef"]
                            hr <- exp(estimate)
                            se <- coefs[i, "se(coef)"]
                            z_stat <- coefs[i, "z"]
                            p_value <- coefs[i, "Pr(>|z|)"]
                            
                            # Calculate confidence intervals for hazard ratios
                            if (self$options$include_ci) {
                                alpha <- 1 - self$options$conf_level
                                z_crit <- qnorm(1 - alpha/2)
                                lower_ci <- exp(estimate - z_crit * se)
                                upper_ci <- exp(estimate + z_crit * se)
                            } else {
                                lower_ci <- NA
                                upper_ci <- NA
                            }
                            
                            coeffTable$addRow(rowKey = paste(cause, param_name, sep = "_"), values = list(
                                cause = as.character(cause),
                                parameter = param_name,
                                estimate = estimate,
                                hr = hr,
                                se = if (self$options$include_se) se else NA,
                                z_stat = if (self$options$include_se) z_stat else NA,
                                p_value = if (self$options$include_se) p_value else NA,
                                lower_ci = lower_ci,
                                upper_ci = upper_ci
                            ))
                        }
                    } else if (inherits(model, "survreg")) {
                        # Parametric model coefficients
                        summary_model <- summary(model)
                        coefs <- summary_model$table
                        
                        for (i in 1:nrow(coefs)) {
                            param_name <- rownames(coefs)[i]
                            estimate <- coefs[i, "Value"]
                            se <- coefs[i, "Std. Error"]
                            z_stat <- coefs[i, "z"]
                            p_value <- coefs[i, "p"]
                            
                            # For AFT models, exp(-estimate) gives hazard ratio
                            hr <- exp(-estimate)
                            
                            # Calculate confidence intervals
                            if (self$options$include_ci) {
                                alpha <- 1 - self$options$conf_level
                                z_crit <- qnorm(1 - alpha/2)
                                lower_ci <- exp(-(estimate + z_crit * se))
                                upper_ci <- exp(-(estimate - z_crit * se))
                            } else {
                                lower_ci <- NA
                                upper_ci <- NA
                            }
                            
                            coeffTable$addRow(rowKey = paste(cause, param_name, sep = "_"), values = list(
                                cause = as.character(cause),
                                parameter = param_name,
                                estimate = estimate,
                                hr = hr,
                                se = if (self$options$include_se) se else NA,
                                z_stat = if (self$options$include_se) z_stat else NA,
                                p_value = if (self$options$include_se) p_value else NA,
                                lower_ci = lower_ci,
                                upper_ci = upper_ci
                            ))
                        }
                    }
                }, error = function(e) {
                    coeffTable$addRow(rowKey = paste(cause, "note", sep = "_"), values = list(
                        cause = as.character(cause),
                        parameter = "Note",
                        estimate = NA,
                        hr = NA,
                        se = NA,
                        z_stat = NA,
                        p_value = NA,
                        lower_ci = NA,
                        upper_ci = NA
                    ))
                })
            }
        },

        .populateCumulativeIncidence = function(prepared_data, causes) {
            if (!requireNamespace("cmprsk", quietly = TRUE)) {
                # Fallback implementation without cmprsk
                private$.populateCumulativeIncidenceBuiltin(prepared_data, causes)
                return()
            }
            
            tryCatch({
                # Use cmprsk package for proper cumulative incidence
                ci_result <- cmprsk::cuminc(
                    ftime = prepared_data$time,
                    fstatus = prepared_data$cause
                )
                
                cumuTable <- self$results$cumulative_incidence_table
                
                # Extract estimates at key time points
                time_points <- quantile(prepared_data$time[prepared_data$cause != 0], 
                                      probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
                
                for (cause in causes) {
                    cause_name <- paste(cause, "1", sep = " ")
                    if (cause_name %in% names(ci_result)) {
                        ci_data <- ci_result[[cause_name]]
                        
                        for (tp in time_points) {
                            # Find closest time point
                            closest_idx <- which.min(abs(ci_data$time - tp))
                            est_time <- ci_data$time[closest_idx]
                            est_ci <- ci_data$est[closest_idx]
                            est_se <- sqrt(ci_data$var[closest_idx])
                            
                            # Calculate confidence intervals
                            alpha <- 1 - self$options$conf_level
                            z_crit <- qnorm(1 - alpha/2)
                            lower_ci <- max(0, est_ci - z_crit * est_se)
                            upper_ci <- min(1, est_ci + z_crit * est_se)
                            
                            cumuTable$addRow(rowKey = paste(cause, est_time, sep = "_"), values = list(
                                time = est_time,
                                cause = as.character(cause),
                                cumulative_incidence = est_ci,
                                se = est_se,
                                lower_ci = lower_ci,
                                upper_ci = upper_ci
                            ))
                        }
                    }
                }
            }, error = function(e) {
                private$.populateCumulativeIncidenceBuiltin(prepared_data, causes)
            })
        },

        .populateCumulativeIncidenceBuiltin = function(prepared_data, causes) {
            # Fallback cumulative incidence calculation
            cumuTable <- self$results$cumulative_incidence_table
            
            time_points <- quantile(prepared_data$time[prepared_data$cause != 0], 
                                  probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
            
            for (cause in causes) {
                for (tp in time_points) {
                    # Simple cumulative incidence approximation
                    events_by_time <- prepared_data[prepared_data$time <= tp, ]
                    cause_events <- sum(events_by_time$cause == cause)
                    total_risk <- nrow(events_by_time)
                    
                    ci_est <- ifelse(total_risk > 0, cause_events / total_risk, 0)
                    
                    cumuTable$addRow(rowKey = paste(cause, tp, sep = "_"), values = list(
                        time = tp,
                        cause = as.character(cause),
                        cumulative_incidence = ci_est,
                        se = sqrt(ci_est * (1 - ci_est) / total_risk),
                        lower_ci = NA,
                        upper_ci = NA
                    ))
                }
            }
        },

        .populateModelComparison = function(models, causes) {
            # Placeholder for model comparison
            compTable <- self$results$model_comparison_table
            
            # Simple comparison of log-likelihoods
            logliks <- sapply(models, function(m) {
                if (inherits(m, "coxph")) {
                    return(m$loglik[2])
                } else if (inherits(m, "survreg")) {
                    return(m$loglik[2])
                }
                return(NA)
            })
            
            if (length(logliks) > 1) {
                # Compare to reference cause
                ref_cause <- self$options$reference_cause
                if (ref_cause %in% names(logliks)) {
                    ref_loglik <- logliks[[ref_cause]]
                    
                    for (cause in causes) {
                        if (cause != ref_cause && !is.na(logliks[[as.character(cause)]])) {
                            lr_stat <- 2 * (logliks[[as.character(cause)]] - ref_loglik)
                            
                            compTable$addRow(rowKey = paste(cause, "vs", ref_cause, sep = "_"), values = list(
                                comparison = paste("Cause", cause, "vs", ref_cause),
                                statistic = abs(lr_stat),
                                df = 1,
                                p_value = 1 - pchisq(abs(lr_stat), df = 1)
                            ))
                        }
                    }
                }
            }
        },

        .populateProportionalHazardsTest = function(models, causes) {
            phTable <- self$results$proportional_hazards_table
            
            for (cause in causes) {
                model <- models[[as.character(cause)]]
                
                if (inherits(model, "coxph")) {
                    tryCatch({
                        ph_test <- survival::cox.zph(model)
                        
                        for (i in 1:nrow(ph_test$table)) {
                            var_name <- rownames(ph_test$table)[i]
                            rho <- ph_test$table[i, "rho"]
                            chi_square <- ph_test$table[i, "chisq"]
                            p_value <- ph_test$table[i, "p"]
                            
                            phTable$addRow(rowKey = paste(cause, var_name, sep = "_"), values = list(
                                cause = as.character(cause),
                                variable = var_name,
                                rho = rho,
                                chi_square = chi_square,
                                p_value = p_value
                            ))
                        }
                    }, error = function(e) {
                        phTable$addRow(rowKey = paste(cause, "note", sep = "_"), values = list(
                            cause = as.character(cause),
                            variable = "Note",
                            rho = NA,
                            chi_square = NA,
                            p_value = NA
                        ))
                    })
                }
            }
        },

        .populatePlots = function(models, prepared_data, causes) {
            # Plotting functionality would be implemented here
            # This is a placeholder for the comprehensive plotting features
            
            if (self$options$plot_cumulative_incidence) {
                # Cumulative incidence plot implementation
            }
            
            if (self$options$plot_hazards) {
                # Hazard functions plot implementation
            }
            
            if (self$options$plot_diagnostics) {
                # Diagnostic plots implementation
            }
        },

        .plot_cumulative_incidence = function(image, ...) {
            # Placeholder for cumulative incidence plot
            plot <- ggplot2::ggplot() + 
                ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "Cumulative Incidence Plot\n(Implementation Pending)")) +
                ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                ggplot2::theme_void()
            
            print(plot)
            TRUE
        },

        .plot_hazards = function(image, ...) {
            # Placeholder for hazard functions plot
            plot <- ggplot2::ggplot() + 
                ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "Hazard Functions Plot\n(Implementation Pending)")) +
                ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                ggplot2::theme_void()
            
            print(plot)
            TRUE
        },

        .plot_diagnostics = function(image, ...) {
            # Placeholder for diagnostic plots
            plot <- ggplot2::ggplot() + 
                ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "Diagnostic Plots\n(Implementation Pending)")) +
                ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                ggplot2::theme_void()
            
            print(plot)
            TRUE
        }
    )
)