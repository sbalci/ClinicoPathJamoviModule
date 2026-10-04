#' @title Flexible Parametric Survival Models
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom survival Surv
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon labs scale_y_continuous theme_minimal theme
#' @importFrom ggplot2 element_text geom_point geom_smooth geom_hline annotate theme_void
#' @importFrom dplyr setdiff
#' @importFrom stats AIC BIC nobs vcov qnorm pnorm
#'
#' @return An \code{R6} class generator object for the \code{flexparametricClass} backend; used internally by the jamovi analysis wrapper and not called directly.

flexparametricClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
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
                if (is.null(data)) {
                    return()
                }

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
                if (is.null(model_results)) {
                    return()
                }

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
                if (length(self$options$grouping_variable)) {
                    self$results$todo$setContent("Group-specific prediction is unavailable; include the group as a covariate to fit its effect.")
                    return(NULL)
                }
                # Extract variables from data
                data <- self$data

                elapsedtime_name <- self$options$elapsedtime
                outcome_name <- self$options$outcome
                covariate_names <- self$options$covariates

                if (is.null(elapsedtime_name) || is.null(outcome_name)) {
                    return(NULL)
                }

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

                raw_levels <- unique(as.character(analysis_data$event[!is.na(analysis_data$event)]))
                if (length(raw_levels) > 2L ||
                    !as.character(self$options$outcomeLevel) %in% raw_levels) {
                    self$results$todo$setContent(paste(
                        "Select a binary event indicator and an observed event level.",
                        "Competing events require a cumulative-incidence or multistate analysis."))
                    return(NULL)
                }
                if (!is.numeric(analysis_data$time) ||
                    any(!is.finite(analysis_data$time[!is.na(analysis_data$time)]) |
                        analysis_data$time[!is.na(analysis_data$time)] <= 0)) {
                    self$results$todo$setContent("Follow-up times must be finite positive numbers.")
                    return(NULL)
                }

                # Handle outcome coding
                outcome_level <- self$options$outcomeLevel
                if (is.factor(analysis_data$event) || is.character(analysis_data$event)) {
                    # Compare on the character label so text-coded factors
                    # (e.g. "Dead"/"Alive") are matched to outcomeLevel directly
                    # instead of being coerced to NA via as.numeric().
                    analysis_data$event <- ifelse(as.character(analysis_data$event) == outcome_level, 1, 0)
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
                            "<div style='background-color: rgba(216, 33, 50, 0.18); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
                            "<h4> INSUFFICIENT EVENTS</h4>",
                            "<p><strong>Found:</strong> ", n_events, " events in ", n_obs, " observations (", round(event_rate, 1), "% event rate)</p>",
                            "<p><strong>Required:</strong> >=10 events for basic parametric modeling</p>",
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
                                "<div style='background-color: rgba(216, 33, 50, 0.18); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
                                "<h4> TOO MANY COVARIATES</h4>",
                                "<p><strong>Current:</strong> ", n_covs, " covariates with ", n_events, " events (ratio: ", round(events_per_var, 1), ")</p>",
                                "<p><strong>Rule of thumb:</strong> Need >=5 events per covariate for reliable estimates</p>",
                                "<p><strong>Recommendation:</strong> Use <=", floor(n_events / 5), " covariates or collect more data.</p>",
                                "</div>"
                            ),
                            warnings = ""
                        ))
                    }

                    # Warning for marginal sample sizes
                    if (events_per_var < 10) {
                        warnings_list <- c(
                            warnings_list,
                            paste0(
                                "<p> <strong>Marginal sample size:</strong> ", round(events_per_var, 1),
                                " events per covariate. Consider using fewer covariates for more stable estimates.</p>"
                            )
                        )
                    }
                }

                # Spline complexity warnings
                if (model_approach %in% c("spline_based", "automatic") && spline_df > 4) {
                    if (n_events < spline_df * 10) {
                        warnings_list <- c(
                            warnings_list,
                            paste0(
                                "<p> <strong>Complex spline model:</strong> ", spline_df, " degrees of freedom with ",
                                n_events, " events. Consider reducing complexity to 3-4 degrees of freedom.</p>"
                            )
                        )
                    }
                }

                # Very low event rate warning
                if (event_rate < 5) {
                    warnings_list <- c(
                        warnings_list,
                        paste0(
                            "<p> <strong>Low event rate:</strong> ", round(event_rate, 1),
                            "% events. Parametric models may be less reliable with very low event rates.</p>"
                        )
                    )
                }

                # Very high event rate (may indicate coding issues)
                if (event_rate > 90) {
                    warnings_list <- c(
                        warnings_list,
                        paste0(
                            "<p> <strong>Very high event rate:</strong> ", round(event_rate, 1),
                            "% events. Please verify event coding is correct (1=event, 0=censored).</p>"
                        )
                    )
                }

                # Combine warnings
                warnings_html <- if (length(warnings_list) > 0) {
                    paste0(
                        "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
                        "<h4> Analysis Warnings</h4>",
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

                tryCatch(
                    {
                        # Create survival object
                        surv_obj <- Surv(data$time, data$event)

                        # Create formula
                        if (!is.null(covariate_names) && length(covariate_names) > 0) {
                            formula_str <- paste("surv_obj ~", paste(jmvcore::composeTerms(as.list(covariate_names)), collapse = " + "))
                            formula_obj <- jmvcore::asFormula(formula_str)
                        } else {
                            formula_obj <- surv_obj ~ 1
                        }

                        # Fit model based on approach
                        if (model_approach == "spline_based") {
                            model <- private$.fitSplineModel(formula_obj, data)
                        } else if (model_approach == "automatic") {
                            model <- private$.fitBestModel(formula_obj, data)
                        } else {
                            if (self$options$time_varying_effects)
                                stop("Select spline-based modeling for time-varying effects.")
                            # Traditional parametric approach
                            model <- flexsurv::flexsurvreg(
                                formula_obj,
                                data = data,
                                dist = distribution
                            )
                        }

                        # Extract model information
                        if (is.null(model) || model$opt$convergence != 0 ||
                            any(!is.finite(stats::coef(model))) ||
                            any(!is.finite(stats::vcov(model))))
                            stop("The selected model did not produce a converged finite fit.")

                        return(list(
                            model = model,
                            data = data,
                            distribution = if (model_approach == "spline_based")
                                paste("Royston-Parmar spline on", self$options$spline_type, "scale")
                                else model$dlist$name,
                            approach = model_approach
                        ))
                    },
                    error = function(e) {
                        error_msg <- paste0("<p>Error fitting parametric model: ", htmltools::htmlEscape(e$message), "</p>")
                        self$results$todo$setContent(error_msg)
                        return(NULL)
                    }
                )
            },
            .populateParametersTable = function(model_results) {
                if (!self$options$show_parameters) {
                    return()
                }

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
                    hazard_ratio <- if (identical(model_results$approach, "spline_based") &&
                        self$options$spline_type == "hazard" && !self$options$time_varying_effects &&
                        !grepl("^gamma", param_name)) {
                        exp(estimate)
                    } else {
                        NA
                    }

                    # Clinical interpretation
                    interpretation <- if (is.finite(hazard_ratio))
                        "Proportional-hazards coefficient; exponentiated effect is a hazard ratio."
                        else "Parameter on the fitted model's transformed scale; not a global log hazard ratio."

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
                if (!self$options$show_aic_bic) {
                    return()
                }

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
                    interpretation = "Lower is preferred among models fitted to the same observations"
                ))

                table$addRow(rowKey = "bic", values = list(
                    statistic = "BIC",
                    value = round(bic_value, 2),
                    interpretation = "Relative model comparison on the same observations, penalizing complexity"
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
                had_seed <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
                if (had_seed) old_seed <- get(".Random.seed", .GlobalEnv)
                on.exit(if (had_seed) assign(".Random.seed", old_seed, .GlobalEnv)
                    else if (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
                        rm(".Random.seed", envir = .GlobalEnv))
                set.seed(2026)
                for (type in c("survival", "hazard", "density")) {
                    selected <- switch(type, survival = self$options$show_survival_plot,
                        hazard = self$options$show_hazard_plot, density = self$options$show_density_plot)
                    if (!selected) next
                    plot <- switch(type, survival = private$.createSurvivalPlot(model_results),
                        hazard = private$.createHazardPlot(model_results),
                        density = private$.createDensityPlot(model_results))
                    d <- as.data.frame(plot$data)
                    d$estimate <- d[[type]]
                    d$quantity <- type
                    d$observed <- d$time <= max(data$time)
                    d$family <- model_results$distribution
                    self$results[[paste0(type, "Plot")]]$setState(d[, c("time", "estimate", "lower", "upper", "quantity", "observed", "family")])
                }
                self$results$splinePlot$setVisible(FALSE)
                self$results$diagnosticsPlot$setVisible(FALSE)
                if (self$options$show_spline_plot || self$options$show_diagnostics)
                    self$results$todo$setContent("Spline-basis and residual diagnostic plots are unavailable; fitted survival/hazard/density curves and likelihood comparisons are reported.")
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
                    surv_probs <- summary(model, t = times, newdata = mean_covs, type = "survival", cl = self$options$confidence_level)[[1]]
                } else {
                    # No covariates model
                    surv_probs <- summary(model, t = times, type = "survival", cl = self$options$confidence_level)[[1]]
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
                        title = paste(
                            "Parametric Survival Curve -",
                            model_results$distribution
                        ),
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
                    hazard_rates <- summary(model, t = times, newdata = mean_covs, type = "hazard", cl = self$options$confidence_level)[[1]]
                } else {
                    # No covariates model
                    hazard_rates <- summary(model, t = times, type = "hazard", cl = self$options$confidence_level)[[1]]
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
                        title = paste(
                            "Parametric Hazard Function -",
                            model_results$distribution
                        ),
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
                    densities <- summary(model, t = times, newdata = mean_covs, type = "density", cl = self$options$confidence_level)[[1]]
                } else {
                    # No covariates model
                    densities <- summary(model, t = times, type = "density", cl = self$options$confidence_level)[[1]]
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
                        title = paste(
                            "Parametric Density Function -",
                            model_results$distribution
                        ),
                        x = "Time",
                        y = "Density"
                    ) +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = 0.5))

                return(p)
            },
            .fitSplineModel = function(formula_obj, data) {
                scale <- self$options$spline_type
                k <- max(0L, self$options$spline_df - 1L)
                event_times <- data$time[data$event == 1 & data$time > 0]
                if (length(unique(event_times)) < 3)
                    stop("Spline models require at least three distinct positive event times.")
                bounds <- range(log(event_times))
                knots <- NULL
                if (self$options$knot_placement == "manual") {
                    raw <- suppressWarnings(as.numeric(strsplit(
                        self$options$manual_knots, ",", fixed = TRUE)[[1]]))
                    if (!length(raw) || any(!is.finite(raw) | raw <= 0))
                        stop("Manual knots must be positive times separated by commas.")
                    knots <- sort(unique(log(raw)))
                    if (any(knots <= bounds[1] | knots >= bounds[2]))
                        stop("Manual knots must lie strictly inside the observed event-time range.")
                    k <- length(knots)
                } else if (self$options$knot_placement == "equal" && k > 0) {
                    knots <- seq(bounds[1], bounds[2], length.out = k + 2L)[-c(1, k + 2L)]
                }
                anc <- NULL
                if (self$options$time_varying_effects) {
                    rhs <- stats::formula(stats::delete.response(stats::terms(formula_obj)))
                    if (!length(all.vars(rhs))) stop("Time-varying effects require predictors.")
                    anc <- stats::setNames(rep(list(rhs), k + 1L), paste0("gamma", seq_len(k + 1L)))
                }
                model <- flexsurv::flexsurvspline(formula_obj, data = data, k = k,
                    knots = knots, bknots = bounds, scale = scale, anc = anc)
                if (model$opt$convergence != 0)
                    stop("The selected spline model did not converge; no substitute distribution was fitted.")
                model
            },
            .fitBestModel = function(formula_obj, data) {
                if (self$options$time_varying_effects)
                    stop("Select spline-based modeling for time-varying effects.")
                # Optimized model comparison - start with most robust models
                distributions <- c("weibull", "lognormal", "gamma") # Reduced set for speed
                models <- vector("list", length(distributions))
                aics <- rep(Inf, length(distributions))

                # Fit models with early stopping if one is clearly best
                best_aic <- Inf
                best_model <- NULL

                for (i in seq_along(distributions)) {
                    tryCatch(
                        {
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
                        },
                        error = function(e) {
                            # Skip failed models
                            aics[i] <- Inf
                        }
                    )
                }

                # Try more complex models only if sample size permits and simple models don't fit well
                n_events <- sum(data$event)
                if (n_events >= 50) {
                    complex_dists <- c("gengamma", "genf")

                    for (dist in complex_dists) {
                        tryCatch(
                            {
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
                            },
                            error = function(e) {
                                # Skip if complex model fails
                            }
                        )
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
                    "<h3> Model Summary</h3>",
                    "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
                    "<p><strong>Approach:</strong> ",
                    switch(approach,
                        "traditional" = "Traditional Parametric",
                        "spline_based" = "Spline-Based Flexible",
                        "automatic" = "Automatic Selection",
                        "Traditional Parametric"
                    ), "</p>",
                    "<p><strong>Distribution:</strong> ", distribution, "</p>",
                    "<p><strong>Sample Size:</strong> ", n_obs, " observations (", n_events, " events)</p>",
                    "<p><strong>Event Rate:</strong> ", round(n_events / n_obs * 100, 1), "%</p>",
                    "</div>"
                )

                self$results$modelSummary$setContent(summary_html)
            },
            .populateSplineDetails = function(model_results) {
                if (self$options$model_approach != "spline_based") {
                    return()
                }
                if (!self$options$show_parameters) {
                    return()
                }

                table <- self$results$splineDetails
                spline_df <- length(model_results$model$knots) - 1L
                spline_type <- self$options$spline_type
                knot_placement <- self$options$knot_placement

                # Add spline configuration details
                table$addRow(rowKey = "baseline", values = list(
                    component = "Baseline Spline",
                    df = spline_df,
                    knots = paste(signif(exp(model_results$model$knots), 5), collapse = ", "),
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
                if (!self$options$model_comparison) {
                    return()
                }

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

                comparison_results <- list(selected = list(
                    model = paste("Selected:", model_results$distribution),
                    parameters = length(stats::coef(model_results$model)),
                    log_likelihood = model_results$model$loglik,
                    aic = stats::AIC(model_results$model), bic = stats::BIC(model_results$model)))

                formula_obj <- private$.createFormula(data)

                for (dist in distributions) {
                    tryCatch(
                        {
                            test_model <- flexsurv::flexsurvreg(
                                formula_obj,
                                data = data,
                                dist = dist
                            )

                            if (test_model$opt$convergence != 0 ||
                                any(!is.finite(stats::coef(test_model)))) next
                            comparison_results[[dist]] <- list(
                                model = dist,
                                parameters = length(test_model$coefficients),
                                log_likelihood = test_model$loglik,
                                aic = AIC(test_model),
                                bic = BIC(test_model)
                            )
                        },
                        error = function(e) {
                            # Skip models that fail to fit
                        }
                    )
                }

                # Find best model by AIC
                aics <- sapply(comparison_results, function(x) x$aic)
                best_model <- comparison_results[[which.min(aics)]]$model

                # Populate comparison table
                for (result in comparison_results) {
                    is_best <- result$model == best_model
                    best_indicator <- if (is_best) " Best" else ""

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
                self$results$clinicalSummary$setContent(paste0(
                    "<p>Fitted family: ", htmltools::htmlEscape(model_results$distribution),
                    "; ", nrow(model_results$data), " observations and ",
                    sum(model_results$data$event), " events.</p>",
                    "<p>AIC/BIC compare candidate models on the same data; their absolute values do not validate assumptions.</p>",
                    "<p>AFT, proportional-odds, probit, and time-varying coefficients must not be interpreted as constant hazard ratios.</p>",
                    "<p>Curve predictions use numeric covariates at their mean and factors at their most frequent level. Extrapolated tails remain model-dependent.</p>"))
            },
            .createFormula = function(data) {
                covariate_names <- self$options$covariates
                surv_obj <- Surv(data$time, data$event)

                if (!is.null(covariate_names) && length(covariate_names) > 0) {
                    formula_str <- paste("surv_obj ~", paste(jmvcore::composeTerms(as.list(covariate_names)), collapse = " + "))
                    return(jmvcore::asFormula(formula_str))
                } else {
                    return(surv_obj ~ 1)
                }
            },
            .populateSummary = function(model_results) {
                if (!self$options$showSummaries) {
                    return()
                }

                model <- model_results$model
                distribution <- model_results$distribution

                summary_html <- paste0(
                    "<h3>Analysis Summary</h3>",
                    "<p>Fitted ", distribution, " distribution to survival data.</p>",
                    "<p>AIC: ", round(AIC(model), 2), "</p>",
                    "<p>BIC: ", round(BIC(model), 2), "</p>",
                    "<p>Log-likelihood: ", round(model$loglik, 2), "</p>"
                )

                self$results$analysisSummary$setContent(summary_html)
            },
            .populateMethodology = function() {
                if (!self$options$showExplanations) {
                    return()
                }

                methodology_html <- paste(
                    "<p>Fits a parametric distribution or a Royston-Parmar spline to a binary,",
                    "right-censored endpoint. Splines use log time on the selected hazard, odds, or probit scale.</p>",
                    "<p>Time-varying spline effects relax a constant covariate effect. Traditional distributions",
                    "and automatic distribution selection do not fit those interactions.</p>",
                    "<p>Confidence intervals describe fitted parameter uncertainty. AIC/BIC compare models",
                    "on the same cohort; they do not establish adequate follow-up, independent censoring, or cure.</p>",
                    "<p>Predictions beyond observed follow-up are extrapolations. Report the family, predictors,",
                    "prediction profile, uncertainty, and follow-up support. Interpret coefficients on the",
                    "model's scale; only the constant PH effects are displayed as hazard ratios.</p>"
                )

                self$results$methodExplanation$setContent(methodology_html)
            },

            .renderCurve = function(image, ggtheme, theme, ...) {
                d <- image$state
                if (is.null(d) || !nrow(d)) return(FALSE)
                print(ggplot2::ggplot(d, ggplot2::aes(time, estimate)) +
                    ggplot2::geom_line(ggplot2::aes(linetype = observed)) + ggplot2::geom_ribbon(
                        ggplot2::aes(ymin = lower, ymax = upper), alpha = .15) +
                    ggplot2::labs(x = "Time", y = d$quantity[1], title = d$family[1],
                        linetype = "Within follow-up") + ggtheme)
                TRUE
            },
            .plotSurvivalPlot = function(image, ggtheme, theme, ...)
                private$.renderCurve(image, ggtheme, theme, ...),
            .plotHazardPlot = function(image, ggtheme, theme, ...)
                private$.renderCurve(image, ggtheme, theme, ...),
            .plotDensityPlot = function(image, ggtheme, theme, ...)
                private$.renderCurve(image, ggtheme, theme, ...),
            .plotSplinePlot = function(...) FALSE,
            .plotDiagnosticsPlot = function(...) FALSE
        )
    )
}
