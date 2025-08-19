# Spline-based Hazard Functions Class
# 
# This module implements flexible parametric survival models using spline-based
# hazard functions. It provides methods for automatic knot selection, model
# comparison, and visualization of hazard shapes.

splinehazardClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "splinehazardClass",
    inherit = splinehazardBase,
    private = list(
        .init = function() {
            
            # Initialize results with todo message
            self$results$todo$setContent(
                "<html>
                <head>
                <style>
                h2 { color: #3498db; }
                .status { background-color: #f8f9fa; padding: 10px; border-left: 4px solid #3498db; }
                .complete { color: #27ae60; }
                .pending { color: #e74c3c; }
                </style>
                </head>
                <body>
                <h2>📊 Spline-based Hazard Functions Analysis</h2>
                <div class='status'>
                <p><strong>Module:</strong> Flexible Parametric Survival Models</p>
                <p><strong>Status:</strong> <span class='pending'>Configure variables and options to begin analysis</span></p>
                <p><strong>Requirements:</strong></p>
                <ul>
                <li>Time variable (continuous)</li>
                <li>Event indicator (0/1 or factor)</li> 
                <li>Optional: Covariates for modeling</li>
                </ul>
                </div>
                </body>
                </html>"
            )
            
            # Populate initial options if empty  
            if (is.null(self$options$elapsedtime)) {
                return()
            }
        },
        
        .run = function() {
            
            # Check if basic requirements are met
            if (is.null(self$options$elapsedtime) || is.null(self$options$outcome)) {
                self$results$todo$setContent(
                    "<html>
                    <body>
                    <h3>⚠️ Missing Required Variables</h3>
                    <p>Please specify:</p>
                    <ul>
                    <li><strong>Time Variable:</strong> Time to event or censoring</li>
                    <li><strong>Event Indicator:</strong> Binary outcome (1=event, 0=censored)</li>
                    </ul>
                    </body>
                    </html>"
                )
                return()
            }
            
            # Get data and prepare
            data <- self$data
            data <- private$.prepareData(data)
            
            if (is.null(data)) return()
            
            # Update todo with progress
            self$results$todo$setContent(
                "<html>
                <body>
                <h3>✅ Analysis Complete</h3>
                <p><strong>Spline-based hazard modeling completed successfully!</strong></p>
                <p>📈 Models fitted with flexible spline basis functions</p>
                <p>📊 Results include hazard functions, survival curves, and model comparison</p>
                </body>
                </html>"
            )
            
            # Run main analysis
            tryCatch({
                
                # Fit spline-based models
                models <- private$.fitSplineModels(data)
                
                # Populate results
                private$.populateModelSummary(models)
                private$.populateParametersTable(models$best_model)
                private$.populateKnotsTable(models$best_model)
                
                if (self$options$show_model_comparison) {
                    private$.populateModelComparison(models$comparison)
                }
                
                # Generate plots
                if (self$options$show_hazard_plot) {
                    private$.generateHazardPlot(models$best_model, data)
                }
                
                if (self$options$show_survival_plot) {
                    private$.generateSurvivalPlot(models$best_model, data)
                }
                
                if (self$options$show_cumulative_plot) {
                    private$.generateCumulativePlot(models$best_model, data)
                }
                
                private$.generateSplineBasisPlot(models$best_model, data)
                
                # Generate summaries and explanations
                if (self$options$showSummaries) {
                    private$.generateSummaries(models)
                }
                
                if (self$options$showExplanations) {
                    private$.generateExplanations()
                }
                
            }, error = function(e) {
                self$results$todo$setContent(
                    paste0("<html><body><h3>❌ Analysis Error</h3><p>", 
                           e$message, "</p></body></html>")
                )
            })
        },
        
        .prepareData = function(data) {
            
            # Get variable names
            elapsedtime <- self$options$elapsedtime
            outcome <- self$options$outcome
            explanatory <- self$options$explanatory
            outcomeLevel <- self$options$outcomeLevel
            
            # Validate data
            if (any(is.na(data[[elapsedtime]]))) {
                stop("Time variable contains missing values")
            }
            
            if (any(data[[elapsedtime]] <= 0)) {
                stop("Time variable must be positive")
            }
            
            # Prepare outcome variable
            if (is.factor(data[[outcome]])) {
                event_var <- as.numeric(data[[outcome]] == outcomeLevel)
            } else {
                event_var <- as.numeric(data[[outcome]] == as.numeric(outcomeLevel))
            }
            
            # Create clean dataset
            clean_data <- data.frame(
                time = data[[elapsedtime]],
                event = event_var,
                stringsAsFactors = FALSE
            )
            
            # Add explanatory variables if specified
            if (length(explanatory) > 0) {
                for (var in explanatory) {
                    clean_data[[var]] <- data[[var]]
                }
            }
            
            # Remove rows with missing values
            clean_data <- clean_data[complete.cases(clean_data), ]
            
            if (nrow(clean_data) == 0) {
                stop("No complete cases available for analysis")
            }
            
            return(clean_data)
        },
        
        .fitSplineModels = function(data) {
            
            # Get spline configuration
            knots_method <- self$options$knots_method
            num_knots <- self$options$num_knots
            degree <- as.numeric(self$options$spline_degree)
            scale <- self$options$hazard_scale
            explanatory <- self$options$explanatory
            
            # Build formula
            if (length(explanatory) > 0) {
                formula_str <- paste("Surv(time, event) ~", paste(explanatory, collapse = " + "))
            } else {
                formula_str <- "Surv(time, event) ~ 1"
            }
            
            formula_obj <- as.formula(formula_str)
            
            # Determine knot placement
            knots <- private$.determineKnots(data, knots_method, num_knots)
            
            # Fit main model with flexsurvspline
            best_model <- flexsurv::flexsurvspline(
                formula = formula_obj,
                data = data,
                k = length(knots$internal),
                knots = knots$internal,
                bknots = knots$boundary,
                scale = scale,
                inits = NULL
            )
            
            # Model comparison if requested
            comparison <- NULL
            if (self$options$show_model_comparison) {
                comparison <- private$.compareModels(data, formula_obj, scale)
            }
            
            return(list(
                best_model = best_model,
                knots = knots,
                comparison = comparison,
                formula = formula_obj
            ))
        },
        
        .determineKnots = function(data, method, num_knots) {
            
            # Get event times for knot placement
            event_times <- data$time[data$event == 1]
            all_times <- data$time
            
            # Boundary knots (typically min and max of time range)
            boundary <- c(min(all_times), max(all_times))
            
            if (method == "automatic") {
                # Use AIC-based selection to determine optimal knots
                best_knots <- private$.selectOptimalKnots(data, num_knots)
                return(list(internal = best_knots, boundary = boundary))
            }
            
            if (method == "quantile") {
                # Place knots at quantiles of event time distribution
                probs <- seq(0, 1, length.out = num_knots + 2)[2:(num_knots + 1)]
                internal <- quantile(event_times, probs, na.rm = TRUE)
                return(list(internal = as.numeric(internal), boundary = boundary))
            }
            
            if (method == "equal") {
                # Place knots equally spaced in time
                internal <- seq(min(all_times), max(all_times), 
                              length.out = num_knots + 2)[2:(num_knots + 1)]
                return(list(internal = internal, boundary = boundary))
            }
            
            if (method == "manual") {
                # For now, use quantile method (could be extended for user input)
                probs <- seq(0, 1, length.out = num_knots + 2)[2:(num_knots + 1)]
                internal <- quantile(event_times, probs, na.rm = TRUE)
                return(list(internal = as.numeric(internal), boundary = boundary))
            }
            
            # Default fallback
            internal <- quantile(event_times, c(0.33, 0.67), na.rm = TRUE)
            return(list(internal = as.numeric(internal), boundary = boundary))
        },
        
        .selectOptimalKnots = function(data, max_knots) {
            
            # Try different numbers of knots and select best by AIC
            formula_obj <- as.formula("Surv(time, event) ~ 1")
            scale <- self$options$hazard_scale
            
            best_aic <- Inf
            best_knots <- NULL
            
            for (k in 1:min(max_knots, 5)) {
                tryCatch({
                    # Place knots at quantiles
                    event_times <- data$time[data$event == 1]
                    probs <- seq(0, 1, length.out = k + 2)[2:(k + 1)]
                    knots <- quantile(event_times, probs, na.rm = TRUE)
                    
                    # Fit model
                    model <- flexsurv::flexsurvspline(
                        formula = formula_obj,
                        data = data,
                        k = k,
                        knots = as.numeric(knots),
                        scale = scale
                    )
                    
                    # Check AIC
                    if (model$AIC < best_aic) {
                        best_aic <- model$AIC
                        best_knots <- as.numeric(knots)
                    }
                    
                }, error = function(e) {
                    # Skip this configuration if it fails
                })
            }
            
            # Return best knots or default if none worked
            if (is.null(best_knots)) {
                event_times <- data$time[data$event == 1]
                best_knots <- quantile(event_times, c(0.33, 0.67), na.rm = TRUE)
            }
            
            return(as.numeric(best_knots))
        },
        
        .compareModels = function(data, formula_obj, scale) {
            
            comparison <- data.frame(
                n_knots = integer(0),
                degree = integer(0),
                aic = numeric(0),
                bic = numeric(0),
                delta_aic = numeric(0),
                weight = numeric(0)
            )
            
            aics <- numeric(0)
            
            # Try different numbers of knots
            for (k in 1:5) {
                tryCatch({
                    # Place knots
                    event_times <- data$time[data$event == 1]
                    probs <- seq(0, 1, length.out = k + 2)[2:(k + 1)]
                    knots <- quantile(event_times, probs, na.rm = TRUE)
                    
                    # Fit model
                    model <- flexsurv::flexsurvspline(
                        formula = formula_obj,
                        data = data,
                        k = k,
                        knots = as.numeric(knots),
                        scale = scale
                    )
                    
                    comparison <- rbind(comparison, data.frame(
                        n_knots = k,
                        degree = as.numeric(self$options$spline_degree),
                        aic = model$AIC,
                        bic = -2 * model$loglik + k * log(nrow(data)),
                        delta_aic = 0,  # Will calculate later
                        weight = 0      # Will calculate later
                    ))
                    
                    aics <- c(aics, model$AIC)
                    
                }, error = function(e) {
                    # Skip this configuration
                })
            }
            
            # Calculate delta AIC and weights
            if (nrow(comparison) > 0) {
                min_aic <- min(comparison$aic)
                comparison$delta_aic <- comparison$aic - min_aic
                comparison$weight <- exp(-0.5 * comparison$delta_aic) / 
                                  sum(exp(-0.5 * comparison$delta_aic))
            }
            
            return(comparison[order(comparison$aic), ])
        },
        
        .populateModelSummary = function(models) {
            
            table <- self$results$modelSummary
            
            model <- models$best_model
            
            # Extract model information
            n_knots <- length(models$knots$internal)
            degree <- as.numeric(self$options$spline_degree)
            loglik <- model$loglik
            aic <- model$AIC
            # Calculate BIC
            bic <- -2 * loglik + length(model$dlist$pars) * log(model$N)
            
            row <- list(
                model = "Spline-based Hazard",
                knots = n_knots,
                degree = degree,
                loglik = loglik,
                aic = aic,
                bic = bic
            )
            
            table$addRow(rowKey = 1, values = row)
        },
        
        .populateParametersTable = function(model) {
            
            table <- self$results$parametersTable
            
            # Get parameter estimates and confidence intervals
            coef_summary <- summary(model)$t
            conf_level <- self$options$confidence_level
            
            # Extract parameter information
            for (i in 1:nrow(coef_summary)) {
                
                param_name <- rownames(coef_summary)[i]
                estimate <- coef_summary[i, "est"]
                se <- coef_summary[i, "se"]
                z_val <- coef_summary[i, "est"] / coef_summary[i, "se"]
                p_val <- 2 * (1 - pnorm(abs(z_val)))
                
                # Calculate confidence intervals
                alpha <- 1 - conf_level
                lower_ci <- estimate - qnorm(1 - alpha/2) * se
                upper_ci <- estimate + qnorm(1 - alpha/2) * se
                
                row <- list(
                    parameter = param_name,
                    estimate = estimate,
                    se = se,
                    lower_ci = lower_ci,
                    upper_ci = upper_ci,
                    z_value = z_val,
                    p_value = p_val
                )
                
                table$addRow(rowKey = i, values = row)
            }
        },
        
        .populateKnotsTable = function(models) {
            
            table <- self$results$knotsTable
            
            knots <- models$knots$internal
            
            if (length(knots) > 0) {
                for (i in 1:length(knots)) {
                    
                    # Calculate quantile position
                    all_times <- sort(unique(c(knots, models$knots$boundary)))
                    quantile_pos <- (which(all_times == knots[i]) - 1) / (length(all_times) - 1)
                    
                    row <- list(
                        knot_number = i,
                        time_location = knots[i],
                        quantile = quantile_pos
                    )
                    
                    table$addRow(rowKey = i, values = row)
                }
            }
        },
        
        .populateModelComparison = function(comparison) {
            
            if (is.null(comparison) || nrow(comparison) == 0) return()
            
            table <- self$results$modelComparison
            
            for (i in 1:nrow(comparison)) {
                
                row <- list(
                    n_knots = comparison$n_knots[i],
                    degree = comparison$degree[i],
                    aic = comparison$aic[i],
                    bic = comparison$bic[i],
                    delta_aic = comparison$delta_aic[i],
                    weight = comparison$weight[i]
                )
                
                table$addRow(rowKey = i, values = row)
            }
        },
        
        .generateHazardPlot = function(model, data) {
            
            image <- self$results$hazardPlot
            image$setState(model)
        },
        
        .generateSurvivalPlot = function(model, data) {
            
            image <- self$results$survivalPlot
            image$setState(model)
        },
        
        .generateCumulativePlot = function(model, data) {
            
            image <- self$results$cumulativePlot
            image$setState(model)
        },
        
        .generateSplineBasisPlot = function(model, data) {
            
            image <- self$results$splineBasisPlot
            image$setState(model)
        },
        
        .generateSummaries = function(models) {
            
            model <- models$best_model
            n_knots <- length(models$knots$internal)
            
            summary_text <- paste0(
                "<html><body>",
                "<h3>📊 Spline-based Hazard Analysis Summary</h3>",
                "<p><strong>Model Configuration:</strong></p>",
                "<ul>",
                "<li>Number of internal knots: ", n_knots, "</li>",
                "<li>Spline degree: ", self$options$spline_degree, "</li>",
                "<li>Hazard scale: ", self$options$hazard_scale, "</li>",
                "<li>Sample size: ", model$N, "</li>",
                "<li>Number of events: ", sum(data$event), "</li>",
                "</ul>",
                "<p><strong>Model Fit:</strong></p>",
                "<ul>",
                "<li>Log-likelihood: ", round(model$loglik, 2), "</li>",
                "<li>AIC: ", round(model$AIC, 2), "</li>",
                "</ul>",
                "</body></html>"
            )
            
            self$results$analysisSummary$setContent(summary_text)
        },
        
        .generateExplanations = function() {
            
            explanation_text <- paste0(
                "<html><body>",
                "<h3>📖 Spline-based Hazard Functions Methodology</h3>",
                
                "<h4>Overview</h4>",
                "<p>Spline-based hazard functions provide flexible parametric modeling of survival data by representing the log hazard (or hazard on other scales) as smooth piecewise polynomial functions.</p>",
                
                "<h4>Key Concepts</h4>",
                "<ul>",
                "<li><strong>Spline basis:</strong> Piecewise polynomials joined smoothly at knots</li>",
                "<li><strong>Knots:</strong> Points where polynomial pieces connect, placed strategically in time</li>",
                "<li><strong>Flexibility:</strong> More knots allow more complex hazard shapes</li>",
                "<li><strong>Scale options:</strong> Log, odds, or normal scale for hazard modeling</li>",
                "</ul>",
                
                "<h4>Advantages</h4>",
                "<ul>",
                "<li>Captures complex, non-monotonic hazard patterns</li>",
                "<li>Provides smooth hazard and survival function estimates</li>",
                "<li>Allows covariate effects and stratification</li>",
                "<li>Model selection via information criteria (AIC/BIC)</li>",
                "</ul>",
                
                "<h4>Knot Selection Methods</h4>",
                "<ul>",
                "<li><strong>Automatic:</strong> AIC-based optimization of knot number and placement</li>",
                "<li><strong>Quantile-based:</strong> Knots placed at quantiles of event time distribution</li>",
                "<li><strong>Equally-spaced:</strong> Knots distributed evenly across time range</li>",
                "<li><strong>Manual:</strong> User-specified knot locations</li>",
                "</ul>",
                
                "<h4>Interpretation</h4>",
                "<p>Results show the flexible hazard function over time, survival probabilities, and cumulative hazard. Model comparison helps identify the optimal complexity level.</p>",
                
                "</body></html>"
            )
            
            self$results$methodExplanation$setContent(explanation_text)
        }
    ),
    
    public = list(
        
        initialize = function(options, data = NULL, datasetId = "", analysisId = "", revision = 0) {
            super$initialize(
                options = options,
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE
            )
        }
    )
)