#' @title Parametric Frailty Models
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @importFrom stats anova
#' @export


parametricfrailtyClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "parametricfrailtyClass",
    inherit = parametricfrailtyBase,
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
                    analysis = "Parametric Frailty Model",
                    outcome = paste(self$options$outcome, "~", self$options$elapsedtime),
                    n_subjects = "",
                    n_events = "",
                    frailty_groups = "",
                    baseline_dist = self$options$baseline_distribution,
                    frailty_dist = self$options$frailty_distribution,
                    estimation = self$options$estimation_method
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
            frailtyVar <- self$options$frailty_variable
            covariates <- self$options$covariates
            
            # Validate data
            private$.validateData(data, timeVar, eventVar, frailtyVar, covariates)
            
            # Fit the parametric frailty model
            tryCatch({
                # Prepare the data
                prepared_data <- private$.prepareData(data, timeVar, eventVar, frailtyVar, covariates)
                
                # Try to use frailtySurv package first
                if (requireNamespace("frailtySurv", quietly = TRUE)) {
                    model <- private$.fitFragiltySurvModel(prepared_data)
                } else {
                    # Fallback to survival package with frailty terms
                    model <- private$.fitBuiltinModel(prepared_data)
                }
                
                if (!is.null(model)) {
                    # Populate results
                    private$.populateOverview(model, prepared_data)
                    private$.populateModelFit(model)
                    private$.populateCoefficients(model)
                    private$.populateFrailtyAnalysis(model, prepared_data)
                    private$.populatePlots(model, prepared_data)
                }
                
            }, error = function(e) {
                jmvcore::reject(paste("Error in parametric frailty analysis:", e$message))
            })
        },

        .validateData = function(data, timeVar, eventVar, frailtyVar, covariates) {
            # Check if variables exist
            if (!timeVar %in% names(data)) {
                jmvcore::reject(paste("Time variable", timeVar, "not found in data"))
            }
            
            if (!eventVar %in% names(data)) {
                jmvcore::reject(paste("Event variable", eventVar, "not found in data"))
            }
            
            if (!is.null(frailtyVar) && !frailtyVar %in% names(data)) {
                jmvcore::reject(paste("Frailty variable", frailtyVar, "not found in data"))
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
            
            # Validate event variable
            event_values <- unique(data[[eventVar]])
            if (!all(event_values %in% c(0, 1, FALSE, TRUE, NA))) {
                jmvcore::reject("Event variable must be binary (0/1 or TRUE/FALSE)")
            }
        },

        .prepareData = function(data, timeVar, eventVar, frailtyVar, covariates) {
            # Create survival object
            surv_obj <- survival::Surv(time = data[[timeVar]], event = data[[eventVar]])
            
            # Build data frame for modeling
            model_data <- data.frame(
                time = data[[timeVar]],
                event = data[[eventVar]],
                surv = surv_obj
            )
            
            # Add frailty variable
            if (!is.null(frailtyVar)) {
                model_data$frailty_group <- as.factor(data[[frailtyVar]])
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

        .fitFragiltySurvModel = function(prepared_data) {
            # Construct formula
            if (!is.null(self$options$covariates) && length(self$options$covariates) > 0) {
                cov_formula <- paste(self$options$covariates, collapse = " + ")
                formula_str <- paste("surv ~", cov_formula)
            } else {
                formula_str <- "surv ~ 1"
            }
            
            # Add frailty term if specified
            if (!is.null(self$options$frailty_variable)) {
                if (formula_str == "surv ~ 1") {
                    formula_str <- "surv ~ frailty(frailty_group)"
                } else {
                    formula_str <- paste(formula_str, "+ frailty(frailty_group)")
                }
            }
            
            formula_obj <- as.formula(formula_str)
            
            # Map distribution names
            baseline_dist <- switch(self$options$baseline_distribution,
                "weibull" = "weibull",
                "exponential" = "exponential", 
                "gompertz" = "gompertz",
                "lognormal" = "lognormal",
                "loglogistic" = "loglogistic",
                "gengamma" = "gengamma"
            )
            
            frailty_dist <- switch(self$options$frailty_distribution,
                "gamma" = "gamma",
                "lognormal" = "lognormal",
                "invgauss" = "invgauss",
                "posstab" = "posstab"
            )
            
            # Fit model using frailtySurv
            model <- frailtySurv::frailtySurv(
                formula = formula_obj,
                data = prepared_data,
                distribution = baseline_dist,
                frailty_dist = frailty_dist,
                method = self$options$estimation_method
            )
            
            return(model)
        },

        .fitBuiltinModel = function(prepared_data) {
            # Fallback implementation using survival package
            
            # Construct formula for survival package
            if (!is.null(self$options$covariates) && length(self$options$covariates) > 0) {
                cov_formula <- paste(self$options$covariates, collapse = " + ")
                if (!is.null(self$options$frailty_variable)) {
                    formula_str <- paste("surv ~", cov_formula, "+ frailty(frailty_group)")
                } else {
                    formula_str <- paste("surv ~", cov_formula)
                }
            } else {
                if (!is.null(self$options$frailty_variable)) {
                    formula_str <- "surv ~ frailty(frailty_group)"
                } else {
                    formula_str <- "surv ~ 1"
                }
            }
            
            formula_obj <- as.formula(formula_str)
            
            # Map to survival package distributions
            dist_map <- list(
                "weibull" = "weibull",
                "exponential" = "exponential",
                "lognormal" = "lognormal", 
                "loglogistic" = "loglogistic"
            )
            
            survival_dist <- dist_map[[self$options$baseline_distribution]]
            if (is.null(survival_dist)) {
                survival_dist <- "weibull"  # Default fallback
            }
            
            # Fit parametric survival model
            if (survival_dist %in% c("weibull", "exponential", "lognormal", "loglogistic")) {
                model <- survival::survreg(
                    formula = formula_obj,
                    data = prepared_data,
                    dist = survival_dist
                )
            } else {
                # For distributions not supported by survreg, use coxph with frailty
                model <- survival::coxph(
                    formula = formula_obj,
                    data = prepared_data
                )
            }
            
            # Add metadata for later use
            model$baseline_distribution <- self$options$baseline_distribution
            model$frailty_distribution <- self$options$frailty_distribution
            model$estimation_method <- self$options$estimation_method
            model$is_builtin <- TRUE
            
            return(model)
        },

        .populateOverview = function(model, prepared_data) {
            overviewTable <- self$results$overview
            
            # Calculate summary statistics
            n_subjects <- nrow(prepared_data)
            n_events <- sum(prepared_data$event, na.rm = TRUE)
            
            frailty_groups <- if (!is.null(self$options$frailty_variable)) {
                length(unique(prepared_data$frailty_group))
            } else {
                0
            }
            
            # Update the overview table
            overviewTable$setRow(rowKey = "analysis", values = list(
                analysis = "Parametric Frailty Model",
                outcome = paste(self$options$outcome, "~", self$options$elapsedtime),
                n_subjects = n_subjects,
                n_events = n_events,
                frailty_groups = frailty_groups,
                baseline_dist = self$options$baseline_distribution,
                frailty_dist = self$options$frailty_distribution,
                estimation = self$options$estimation_method
            ))
        },

        .populateModelFit = function(model) {
            modelFitTable <- self$results$model_fit
            
            tryCatch({
                if (!is.null(model$is_builtin) && model$is_builtin) {
                    # For built-in survival models
                    if (inherits(model, "survreg")) {
                        loglik <- model$loglik[2]
                        aic <- -2 * loglik + 2 * length(model$coefficients)
                        bic <- -2 * loglik + log(model$n) * length(model$coefficients)
                        
                        modelFitTable$addRow(rowKey = "loglik", values = list(statistic = "Log-likelihood", value = loglik))
                        modelFitTable$addRow(rowKey = "aic", values = list(statistic = "AIC", value = aic))
                        modelFitTable$addRow(rowKey = "bic", values = list(statistic = "BIC", value = bic))
                        modelFitTable$addRow(rowKey = "n", values = list(statistic = "N", value = model$n))
                    } else if (inherits(model, "coxph")) {
                        loglik <- model$loglik[2]
                        aic <- -2 * loglik + 2 * length(model$coefficients)
                        
                        modelFitTable$addRow(rowKey = "loglik", values = list(statistic = "Log-likelihood", value = loglik))
                        modelFitTable$addRow(rowKey = "aic", values = list(statistic = "AIC", value = aic))
                        modelFitTable$addRow(rowKey = "concordance", values = list(statistic = "Concordance", value = model$concordance["C"]))
                        modelFitTable$addRow(rowKey = "n", values = list(statistic = "N", value = model$n))
                    }
                } else {
                    # For frailtySurv models
                    if (!is.null(model$loglik)) {
                        modelFitTable$addRow(rowKey = "loglik", values = list(statistic = "Log-likelihood", value = model$loglik))
                    }
                    if (!is.null(model$AIC)) {
                        modelFitTable$addRow(rowKey = "aic", values = list(statistic = "AIC", value = model$AIC))
                    }
                    if (!is.null(model$BIC)) {
                        modelFitTable$addRow(rowKey = "bic", values = list(statistic = "BIC", value = model$BIC))
                    }
                }
            }, error = function(e) {
                # If there's an error, add a basic row
                modelFitTable$addRow(rowKey = "note", values = list(statistic = "Note", value = "Model fit statistics unavailable"))
            })
        },

        .populateCoefficients = function(model) {
            coeffTable <- self$results$coefficients
            
            tryCatch({
                if (!is.null(model$is_builtin) && model$is_builtin) {
                    # For built-in survival models
                    if (inherits(model, "survreg")) {
                        summary_model <- summary(model)
                        coefs <- summary_model$table
                        
                        for (i in 1:nrow(coefs)) {
                            param_name <- rownames(coefs)[i]
                            estimate <- coefs[i, "Value"]
                            se <- coefs[i, "Std. Error"]
                            z_stat <- coefs[i, "z"]
                            p_value <- coefs[i, "p"]
                            
                            # Calculate confidence intervals
                            if (self$options$include_ci) {
                                alpha <- 1 - self$options$conf_level
                                z_crit <- qnorm(1 - alpha/2)
                                lower_ci <- estimate - z_crit * se
                                upper_ci <- estimate + z_crit * se
                            } else {
                                lower_ci <- NA
                                upper_ci <- NA
                            }
                            
                            coeffTable$addRow(rowKey = param_name, values = list(
                                parameter = param_name,
                                estimate = estimate,
                                se = if (self$options$include_se) se else NA,
                                z_stat = if (self$options$include_se) z_stat else NA,
                                p_value = if (self$options$include_se) p_value else NA,
                                lower_ci = lower_ci,
                                upper_ci = upper_ci
                            ))
                        }
                    } else if (inherits(model, "coxph")) {
                        summary_model <- summary(model)
                        coefs <- summary_model$coefficients
                        
                        for (i in 1:nrow(coefs)) {
                            param_name <- rownames(coefs)[i]
                            estimate <- coefs[i, "coef"]
                            se <- coefs[i, "se(coef)"]
                            z_stat <- coefs[i, "z"]
                            p_value <- coefs[i, "Pr(>|z|)"]
                            
                            # Calculate confidence intervals for log hazard ratios
                            if (self$options$include_ci) {
                                alpha <- 1 - self$options$conf_level
                                z_crit <- qnorm(1 - alpha/2)
                                lower_ci <- estimate - z_crit * se
                                upper_ci <- estimate + z_crit * se
                            } else {
                                lower_ci <- NA
                                upper_ci <- NA
                            }
                            
                            coeffTable$addRow(rowKey = param_name, values = list(
                                parameter = param_name,
                                estimate = estimate,
                                se = if (self$options$include_se) se else NA,
                                z_stat = if (self$options$include_se) z_stat else NA,
                                p_value = if (self$options$include_se) p_value else NA,
                                lower_ci = lower_ci,
                                upper_ci = upper_ci
                            ))
                        }
                    }
                } else {
                    # For frailtySurv models
                    if (!is.null(model$coefficients)) {
                        coefs <- model$coefficients
                        
                        for (i in 1:length(coefs)) {
                            param_name <- names(coefs)[i]
                            estimate <- coefs[i]
                            
                            # Try to get standard errors if available
                            se <- if (!is.null(model$se)) model$se[i] else NA
                            z_stat <- if (!is.na(se)) estimate / se else NA
                            p_value <- if (!is.na(z_stat)) 2 * (1 - pnorm(abs(z_stat))) else NA
                            
                            # Calculate confidence intervals
                            if (self$options$include_ci && !is.na(se)) {
                                alpha <- 1 - self$options$conf_level
                                z_crit <- qnorm(1 - alpha/2)
                                lower_ci <- estimate - z_crit * se
                                upper_ci <- estimate + z_crit * se
                            } else {
                                lower_ci <- NA
                                upper_ci <- NA
                            }
                            
                            coeffTable$addRow(rowKey = param_name, values = list(
                                parameter = param_name,
                                estimate = estimate,
                                se = if (self$options$include_se) se else NA,
                                z_stat = if (self$options$include_se) z_stat else NA,
                                p_value = if (self$options$include_se) p_value else NA,
                                lower_ci = lower_ci,
                                upper_ci = upper_ci
                            ))
                        }
                    }
                }
            }, error = function(e) {
                # Add a note if coefficient extraction fails
                coeffTable$addRow(rowKey = "note", values = list(
                    parameter = "Note",
                    estimate = NA,
                    se = NA,
                    z_stat = NA,
                    p_value = NA,
                    lower_ci = NA,
                    upper_ci = NA
                ))
            })
        },

        .populateFrailtyAnalysis = function(model, prepared_data) {
            if (!self$options$frailty_variance && !self$options$frailty_predictions) {
                return()
            }
            
            # Frailty variance summary
            if (self$options$frailty_variance) {
                frailtyTable <- self$results$frailty_summary
                
                tryCatch({
                    if (!is.null(model$is_builtin) && model$is_builtin && inherits(model, "coxph")) {
                        # Extract frailty variance from coxph model
                        if ("frailty" %in% names(model$terms) || any(grepl("frailty", names(model$coefficients)))) {
                            # Get frailty terms
                            frailty_info <- model$history$`frailty(frailty_group)`
                            if (!is.null(frailty_info)) {
                                theta <- frailty_info$theta
                                df <- frailty_info$df
                                
                                frailtyTable$addRow(rowKey = "frailty", values = list(
                                    component = "Frailty",
                                    variance = theta,
                                    se_variance = NA,
                                    theta = theta,
                                    kendall_tau = theta / (theta + 1)
                                ))
                            }
                        }
                    } else if (!is.null(model$frailty_variance)) {
                        # For frailtySurv models
                        frailtyTable$addRow(rowKey = "frailty", values = list(
                            component = "Frailty",
                            variance = model$frailty_variance,
                            se_variance = model$frailty_se,
                            theta = model$theta,
                            kendall_tau = model$kendall_tau
                        ))
                    }
                }, error = function(e) {
                    frailtyTable$addRow(rowKey = "note", values = list(
                        component = "Note",
                        variance = NA,
                        se_variance = NA,
                        theta = NA,
                        kendall_tau = NA
                    ))
                })
            }
            
            # Individual frailty predictions
            if (self$options$frailty_predictions) {
                predTable <- self$results$frailty_predictions_table
                
                tryCatch({
                    if (!is.null(model$frailty_predictions)) {
                        # For models with frailty predictions
                        preds <- model$frailty_predictions
                        groups <- unique(prepared_data$frailty_group)
                        
                        for (i in 1:length(groups)) {
                            group_name <- as.character(groups[i])
                            pred_value <- if (i <= length(preds)) preds[i] else NA
                            
                            predTable$addRow(rowKey = group_name, values = list(
                                subject = group_name,
                                frailty_pred = pred_value,
                                se_pred = NA,
                                lower_pred = NA,
                                upper_pred = NA
                            ))
                        }
                    }
                }, error = function(e) {
                    predTable$addRow(rowKey = "note", values = list(
                        subject = "Note",
                        frailty_pred = NA,
                        se_pred = NA,
                        lower_pred = NA,
                        upper_pred = NA
                    ))
                })
            }
        },

        .populatePlots = function(model, prepared_data) {
            # Plotting functionality would be implemented here
            # This is a placeholder for the comprehensive plotting features
            
            if (self$options$plot_hazard) {
                # Hazard function plot implementation
            }
            
            if (self$options$plot_survival) {
                # Survival function plot implementation  
            }
            
            if (self$options$plot_frailty) {
                # Frailty distribution plot implementation
            }
            
            if (self$options$plot_diagnostics) {
                # Diagnostic plots implementation
            }
        },

        .plot_hazard = function(image, ...) {
            # Placeholder for hazard function plot
            plot <- ggplot2::ggplot() + 
                ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "Hazard Function Plot\n(Implementation Pending)")) +
                ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                ggplot2::theme_void()
            
            print(plot)
            TRUE
        },

        .plot_survival = function(image, ...) {
            # Placeholder for survival function plot
            plot <- ggplot2::ggplot() + 
                ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "Survival Function Plot\n(Implementation Pending)")) +
                ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                ggplot2::theme_void()
            
            print(plot)
            TRUE
        },

        .plot_frailty = function(image, ...) {
            # Placeholder for frailty distribution plot
            plot <- ggplot2::ggplot() + 
                ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "Frailty Distribution Plot\n(Implementation Pending)")) +
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