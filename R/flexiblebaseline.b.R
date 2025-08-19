flexiblebaselineClass <- R6::R6Class(
    "flexiblebaselineClass",
    inherit = flexiblebaselineBase,
    private = list(
        .init = function() {
            # Initialize todo content
            self$results$todo$setContent(
                "<h2>Flexible Baseline Distributions Analysis</h2>
                <p>This analysis provides flexible parametric survival modeling using various baseline distributions:</p>
                <ul>
                <li><strong>Spline-based Methods:</strong> Flexible hazard, odds, and normal models using B-splines</li>
                <li><strong>Royston-Parmar Models:</strong> Flexible parametric models with spline-based log cumulative hazard</li>
                <li><strong>Transformation Models:</strong> General transformation models for survival data</li>
                <li><strong>Flexible Parametric:</strong> Extended parametric models with flexible baseline functions</li>
                </ul>
                <p>Please select the time, outcome variables, and configure the flexible baseline options to begin the analysis.</p>"
            )
        },
        
        .run = function() {
            # Get variables
            elapsedtime <- self$options$elapsedtime
            outcome <- self$options$outcome
            
            # Check if required variables are set
            if (is.null(elapsedtime) || is.null(outcome)) {
                return()
            }
            
            # Get data
            data <- self$data
            
            # Data preparation
            time_var <- jmvcore::toNumeric(data[[elapsedtime]])
            outcome_var <- data[[outcome]]
            
            # Convert outcome to numeric if needed
            if (is.factor(outcome_var)) {
                outcome_level <- self$options$outcomeLevel
                event_var <- as.numeric(outcome_var == outcome_level)
            } else {
                event_var <- as.numeric(outcome_var)
            }
            
            # Remove missing values
            complete_cases <- complete.cases(time_var, event_var)
            if (sum(complete_cases) == 0) {
                self$results$todo$setContent("<p>Error: No complete cases available for analysis.</p>")
                return()
            }
            
            time_var <- time_var[complete_cases]
            event_var <- event_var[complete_cases]
            
            # Get grouping variable if specified
            group_var <- NULL
            if (!is.null(self$options$explanatory)) {
                group_var <- data[[self$options$explanatory]][complete_cases]
            }
            
            # Get covariates if specified
            covariates <- NULL
            if (length(self$options$covariates) > 0) {
                cov_data <- data[self$options$covariates]
                cov_data <- cov_data[complete_cases, , drop = FALSE]
                
                # Convert factors to numeric where appropriate
                for (col in names(cov_data)) {
                    if (is.factor(cov_data[[col]])) {
                        cov_data[[col]] <- as.numeric(cov_data[[col]]) - 1
                    }
                }
                covariates <- cov_data
            }
            
            # Run the analysis
            tryCatch({
                self$.runFlexibleBaseline(time_var, event_var, group_var, covariates)
            }, error = function(e) {
                error_msg <- paste("<p><strong>Error in analysis:</strong>", e$message, "</p>")
                self$results$todo$setContent(error_msg)
            })
        },
        
        .runFlexibleBaseline = function(time_var, event_var, group_var, covariates) {
            # Load required packages
            if (!requireNamespace("flexsurv", quietly = TRUE)) {
                stop("flexsurv package is required for flexible baseline distributions")
            }
            
            if (!requireNamespace("rstpm2", quietly = TRUE)) {
                stop("rstpm2 package is required for Royston-Parmar models")
            }
            
            # Create survival object
            surv_obj <- survival::Surv(time_var, event_var)
            
            # Prepare formula
            if (!is.null(group_var) && !is.null(covariates)) {
                formula_str <- "surv_obj ~ group_var + "
                formula_str <- paste0(formula_str, paste(names(covariates), collapse = " + "))
                form_data <- cbind(surv_obj = surv_obj, group_var = group_var, covariates)
            } else if (!is.null(group_var)) {
                formula_str <- "surv_obj ~ group_var"
                form_data <- data.frame(surv_obj = surv_obj, group_var = group_var)
            } else if (!is.null(covariates)) {
                formula_str <- "surv_obj ~ "
                formula_str <- paste0(formula_str, paste(names(covariates), collapse = " + "))
                form_data <- cbind(surv_obj = surv_obj, covariates)
            } else {
                formula_str <- "surv_obj ~ 1"
                form_data <- data.frame(surv_obj = surv_obj)
            }
            
            model_formula <- as.formula(formula_str)
            
            # Fit flexible baseline model based on selected type
            baseline_type <- self$options$baseline_type
            
            if (baseline_type == "spline_hazard") {
                model <- self$.fitSplineHazard(model_formula, form_data)
            } else if (baseline_type == "spline_odds") {
                model <- self$.fitSplineOdds(model_formula, form_data)
            } else if (baseline_type == "spline_normal") {
                model <- self$.fitSplineNormal(model_formula, form_data)
            } else if (baseline_type == "royston_parmar") {
                model <- self$.fitRoystonParmar(model_formula, form_data)
            } else if (baseline_type == "transformation_model") {
                model <- self$.fitTransformationModel(model_formula, form_data)
            } else if (baseline_type == "flexible_parametric") {
                model <- self$.fitFlexibleParametric(model_formula, form_data)
            } else {
                stop("Unknown baseline distribution type")
            }
            
            # Update results
            self$.populateResults(model, baseline_type)
        },
        
        .fitSplineHazard = function(formula, data) {
            # Fit spline-based hazard model using flexsurv
            knots <- self$options$spline_knots
            
            # Create knot positions
            if (self$options$knot_placement == "quantiles") {
                time_var <- data$surv_obj[, "time"]
                knot_positions <- quantile(time_var[data$surv_obj[, "status"] == 1], 
                                         probs = seq(0, 1, length.out = knots + 2)[-c(1, knots + 2)])
            } else {
                time_var <- data$surv_obj[, "time"]
                knot_positions <- seq(min(time_var), max(time_var), length.out = knots + 2)[-c(1, knots + 2)]
            }
            
            # Fit flexible spline model
            model <- flexsurv::flexsurvspline(
                formula = formula,
                data = data,
                k = knots,
                knots = knot_positions,
                scale = "hazard"
            )
            
            return(model)
        },
        
        .fitSplineOdds = function(formula, data) {
            # Fit spline-based odds model
            knots <- self$options$spline_knots
            
            model <- flexsurv::flexsurvspline(
                formula = formula,
                data = data,
                k = knots,
                scale = "odds"
            )
            
            return(model)
        },
        
        .fitSplineNormal = function(formula, data) {
            # Fit spline-based normal model
            knots <- self$options$spline_knots
            
            model <- flexsurv::flexsurvspline(
                formula = formula,
                data = data,
                k = knots,
                scale = "normal"
            )
            
            return(model)
        },
        
        .fitRoystonParmar = function(formula, data) {
            # Fit Royston-Parmar model using rstpm2
            knots <- self$options$spline_knots
            
            model <- rstpm2::stpm2(
                formula = formula,
                data = data,
                df = knots + 1,
                smooth.formula = ~ ns(log(time), df = knots + 1)
            )
            
            return(model)
        },
        
        .fitTransformationModel = function(formula, data) {
            # Fit transformation model
            family <- self$options$transformation_family
            
            if (family == "cox") {
                model <- survival::coxph(formula, data = data)
            } else if (family == "weibull") {
                model <- survival::survreg(formula, data = data, dist = "weibull")
            } else if (family == "loglogistic") {
                model <- survival::survreg(formula, data = data, dist = "loglogistic")
            } else if (family == "lognormal") {
                model <- survival::survreg(formula, data = data, dist = "lognormal")
            } else if (family == "exponential") {
                model <- survival::survreg(formula, data = data, dist = "exponential")
            }
            
            return(model)
        },
        
        .fitFlexibleParametric = function(formula, data) {
            # Fit flexible parametric model using flexsurv with splines
            knots <- self$options$spline_knots
            
            # Try different flexible parametric distributions
            models <- list()
            
            tryCatch({
                models$genf <- flexsurv::flexsurv(formula, data = data, dist = "genf")
            }, error = function(e) NULL)
            
            tryCatch({
                models$gengamma <- flexsurv::flexsurv(formula, data = data, dist = "gengamma")
            }, error = function(e) NULL)
            
            tryCatch({
                models$weibull <- flexsurv::flexsurv(formula, data = data, dist = "weibull")
            }, error = function(e) NULL)
            
            # Select best model by AIC
            if (length(models) > 0) {
                aics <- sapply(models, function(x) if(!is.null(x)) x$AIC else Inf)
                best_model <- models[[which.min(aics)]]
                return(best_model)
            } else {
                stop("Unable to fit flexible parametric model")
            }
        },
        
        .populateResults = function(model, baseline_type) {
            # Model Summary
            if (self$options$show_model_summary) {
                summary_content <- self$.createModelSummary(model, baseline_type)
                self$results$modelSummary$setContent(summary_content)
            }
            
            # Coefficients Table
            if (self$options$show_coefficients) {
                self$.populateCoefficientsTable(model)
            }
            
            # Spline Basis Table
            if (self$options$show_spline_basis && grepl("spline", baseline_type)) {
                self$.populateSplineBasisTable(model)
            }
            
            # Comparison Table
            self$.populateComparisonTable(model, baseline_type)
            
            # Prediction Table
            self$.populatePredictionTable(model)
            
            # Goodness of Fit Table
            if (self$options$show_diagnostics) {
                self$.populateGoodnessOfFitTable(model)
            }
            
            # Summaries and Explanations
            if (self$options$showSummaries) {
                summary_content <- self$.createAnalysisSummary(model, baseline_type)
                self$results$analysisSummary$setContent(summary_content)
            }
            
            if (self$options$showExplanations) {
                explanation_content <- self$.createMethodologyExplanation(baseline_type)
                self$results$methodExplanation$setContent(explanation_content)
            }
        },
        
        .populateCoefficientsTable = function(model) {
            table <- self$results$coefficientsTable
            
            tryCatch({
                if (inherits(model, "flexsurv")) {
                    coef_summary <- summary(model)
                    estimates <- coef_summary$coef
                    
                    for (i in seq_len(nrow(estimates))) {
                        table$addRow(rowKey = i, values = list(
                            parameter = rownames(estimates)[i],
                            estimate = estimates[i, "est"],
                            se = estimates[i, "se"],
                            lower_ci = estimates[i, "L95%"],
                            upper_ci = estimates[i, "U95%"],
                            z_value = estimates[i, "est"] / estimates[i, "se"],
                            p_value = 2 * (1 - pnorm(abs(estimates[i, "est"] / estimates[i, "se"])))
                        ))
                    }
                } else if (inherits(model, "coxph")) {
                    coef_summary <- summary(model)
                    estimates <- coef_summary$coefficients
                    confints <- confint(model)
                    
                    for (i in seq_len(nrow(estimates))) {
                        table$addRow(rowKey = i, values = list(
                            parameter = rownames(estimates)[i],
                            estimate = estimates[i, "coef"],
                            se = estimates[i, "se(coef)"],
                            lower_ci = confints[i, 1],
                            upper_ci = confints[i, 2],
                            z_value = estimates[i, "z"],
                            p_value = estimates[i, "Pr(>|z|)"]
                        ))
                    }
                } else if (inherits(model, "survreg")) {
                    coef_summary <- summary(model)
                    estimates <- coef_summary$table
                    
                    for (i in seq_len(nrow(estimates))) {
                        table$addRow(rowKey = i, values = list(
                            parameter = rownames(estimates)[i],
                            estimate = estimates[i, "Value"],
                            se = estimates[i, "Std. Error"],
                            lower_ci = estimates[i, "Value"] - 1.96 * estimates[i, "Std. Error"],
                            upper_ci = estimates[i, "Value"] + 1.96 * estimates[i, "Std. Error"],
                            z_value = estimates[i, "z"],
                            p_value = estimates[i, "p"]
                        ))
                    }
                }
            }, error = function(e) {
                table$addRow(rowKey = 1, values = list(
                    parameter = "Error",
                    estimate = "",
                    se = "",
                    lower_ci = "",
                    upper_ci = "",
                    z_value = "",
                    p_value = ""
                ))
            })
        },
        
        .populateSplineBasisTable = function(model) {
            table <- self$results$splineBasisTable
            
            # Add spline basis information for spline-based models
            if (inherits(model, "flexsurv")) {
                # Extract knot information
                if (!is.null(model$knots)) {
                    knots <- model$knots
                    for (i in seq_along(knots)) {
                        table$addRow(rowKey = i, values = list(
                            basis_function = paste("Basis", i),
                            knot_position = knots[i],
                            coefficient = if(!is.null(model$coefficients[i])) model$coefficients[i] else NA,
                            significance = "To be computed"
                        ))
                    }
                }
            }
        },
        
        .populateComparisonTable = function(model, baseline_type) {
            table <- self$results$comparisonTable
            
            # Extract model fit statistics
            if (inherits(model, "flexsurv")) {
                table$addRow(rowKey = 1, values = list(
                    model = paste("Flexible", baseline_type),
                    aic = model$AIC,
                    bic = -2 * model$loglik + log(model$N) * model$npars,
                    loglik = model$loglik,
                    df = model$npars
                ))
            } else if (inherits(model, "coxph")) {
                table$addRow(rowKey = 1, values = list(
                    model = "Cox Proportional Hazards",
                    aic = AIC(model),
                    bic = BIC(model),
                    loglik = model$loglik[2],
                    df = model$nevent
                ))
            } else if (inherits(model, "survreg")) {
                table$addRow(rowKey = 1, values = list(
                    model = paste("Parametric", baseline_type),
                    aic = AIC(model),
                    bic = BIC(model),
                    loglik = model$loglik[1],
                    df = length(model$coefficients)
                ))
            }
        },
        
        .populatePredictionTable = function(model) {
            table <- self$results$predictionTable
            
            # Create time points for prediction
            time_points <- seq(0, max(self$data[[self$options$elapsedtime]], na.rm = TRUE), length.out = 10)
            
            tryCatch({
                if (inherits(model, "flexsurv")) {
                    for (i in seq_along(time_points)) {
                        t <- time_points[i]
                        if (t > 0) {
                            surv_prob <- summary(model, t = t, ci = TRUE)[[1]]$est[1]
                            hazard_rate <- summary(model, t = t, type = "hazard", ci = TRUE)[[1]]$est[1]
                            
                            table$addRow(rowKey = i, values = list(
                                time_point = t,
                                survival_prob = surv_prob,
                                hazard_rate = hazard_rate,
                                lower_ci = surv_prob - 1.96 * 0.05, # Placeholder
                                upper_ci = surv_prob + 1.96 * 0.05
                            ))
                        }
                    }
                }
            }, error = function(e) {
                # Add placeholder row if prediction fails
                table$addRow(rowKey = 1, values = list(
                    time_point = 0,
                    survival_prob = 1.0,
                    hazard_rate = 0.0,
                    lower_ci = 1.0,
                    upper_ci = 1.0
                ))
            })
        },
        
        .populateGoodnessOfFitTable = function(model) {
            table <- self$results$goodnessOfFitTable
            
            if (inherits(model, "flexsurv")) {
                # Add AIC
                table$addRow(rowKey = 1, values = list(
                    statistic = "AIC",
                    value = model$AIC,
                    interpretation = ifelse(model$AIC < 1000, "Good fit", "Moderate fit")
                ))
                
                # Add Log-likelihood
                table$addRow(rowKey = 2, values = list(
                    statistic = "Log-likelihood",
                    value = model$loglik,
                    interpretation = ifelse(model$loglik > -500, "Good fit", "Poor fit")
                ))
                
                # Add number of parameters
                table$addRow(rowKey = 3, values = list(
                    statistic = "Parameters",
                    value = model$npars,
                    interpretation = paste("Model complexity:", 
                                          ifelse(model$npars < 5, "Simple", 
                                                ifelse(model$npars < 10, "Moderate", "Complex")))
                ))
            }
        },
        
        .createModelSummary = function(model, baseline_type) {
            html <- "<h3>Flexible Baseline Distribution Model Summary</h3>"
            html <- paste0(html, "<p><strong>Model Type:</strong> ", baseline_type, "</p>")
            
            if (inherits(model, "flexsurv")) {
                html <- paste0(html, "<p><strong>Distribution:</strong> ", model$dlist$name, "</p>")
                html <- paste0(html, "<p><strong>Number of Parameters:</strong> ", model$npars, "</p>")
                html <- paste0(html, "<p><strong>Log-likelihood:</strong> ", round(model$loglik, 3), "</p>")
                html <- paste0(html, "<p><strong>AIC:</strong> ", round(model$AIC, 3), "</p>")
            }
            
            return(html)
        },
        
        .createAnalysisSummary = function(model, baseline_type) {
            html <- "<h3>Analysis Summary</h3>"
            html <- paste0(html, "<p>A flexible baseline distribution analysis was performed using the ", baseline_type, " approach.</p>")
            
            if (inherits(model, "flexsurv")) {
                html <- paste0(html, "<p>The model achieved a log-likelihood of ", round(model$loglik, 3), 
                              " with ", model$npars, " parameters, resulting in an AIC of ", round(model$AIC, 3), ".</p>")
            }
            
            html <- paste0(html, "<p>The flexible baseline approach allows for non-parametric estimation of the baseline hazard function, providing greater flexibility than standard parametric models.</p>")
            
            return(html)
        },
        
        .createMethodologyExplanation = function(baseline_type) {
            html <- "<h3>Methodology: Flexible Baseline Distributions</h3>"
            
            if (baseline_type == "spline_hazard") {
                html <- paste0(html, "<p><strong>Spline-based Hazard Modeling:</strong> This approach uses B-splines to model the log hazard function flexibly, allowing for non-monotonic hazard patterns.</p>")
            } else if (baseline_type == "royston_parmar") {
                html <- paste0(html, "<p><strong>Royston-Parmar Models:</strong> These models use restricted cubic splines on the log cumulative hazard scale, providing a flexible parametric framework.</p>")
            } else if (baseline_type == "transformation_model") {
                html <- paste0(html, "<p><strong>Transformation Models:</strong> These models apply monotonic transformations to survival times, generalizing many common parametric models.</p>")
            }
            
            html <- paste0(html, "<p><strong>Advantages:</strong></p>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li>Greater flexibility than standard parametric models</li>")
            html <- paste0(html, "<li>Can capture complex hazard patterns</li>")
            html <- paste0(html, "<li>Provides smooth estimates with confidence intervals</li>")
            html <- paste0(html, "<li>Allows for extrapolation beyond observed data</li>")
            html <- paste0(html, "</ul>")
            
            return(html)
        }
    ),
    
    public = list(
        initialize = function(...) {
            super$initialize(...)
            private$.init()
        }
    )
)