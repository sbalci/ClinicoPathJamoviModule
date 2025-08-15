#' @title Assay Optimization & Experimental Design
#' @importFrom jmvcore .
#' @importFrom stats lm anova aov model.matrix residuals fitted predict
#' @importFrom stats qt qnorm qf pf pt pnorm
#' @importFrom stats median mad IQR quantile sd var
#' @importFrom DoE.base fac.design oa.design
#' @importFrom rsm ccd bbd rsm
#' @importFrom pwr pwr.f2.test pwr.t.test pwr.anova.test
#' @importFrom car Anova leveneTest
#' @importFrom emmeans emmeans contrast
#' @export
assayoptimizationClass <- R6::R6Class(
    "assayoptimizationClass",
    inherit = assayoptimizationBase,
    private = list(
        .init = function() {
            
            if (is.null(self$data) || is.null(self$options$response_var)) {
                self$results$instructions$setContent(
                    "<h3>Welcome to Assay Optimization & Experimental Design</h3>
                    <p>This analysis provides comprehensive tools for laboratory assay optimization and experimental design.</p>
                    <p><b>Getting Started:</b></p>
                    <ol>
                    <li>Select your <b>Response Variable</b> (e.g., concentration, signal intensity)</li>
                    <li>Choose <b>Experimental Factors</b> to optimize (e.g., temperature, primer concentration)</li>
                    <li>Select appropriate <b>Design Type</b> for your experiment</li>
                    <li>Set <b>Optimization Goal</b> (maximize, minimize, or target value)</li>
                    <li>Configure power analysis and quality control options</li>
                    </ol>
                    <p><b>Key Features:</b></p>
                    <ul>
                    <li><b>Design of Experiments:</b> Full/fractional factorial, central composite, Box-Behnken, D-optimal</li>
                    <li><b>Power Analysis:</b> Sample size and power calculations</li>
                    <li><b>Response Surface Methodology:</b> Optimization with contour plots</li>
                    <li><b>Quality Control:</b> Statistical process control charts</li>
                    <li><b>Method Validation:</b> Precision, accuracy, and robustness assessment</li>
                    </ul>
                    <p><b>For jamovi users:</b> This module works with tabular data where rows represent experimental runs and columns represent measured variables.</p>"
                )
                return()
            }
            
            # Initialize with basic information about the selected variables
            response_var <- self$options$response_var
            factors <- self$options$factors
            n_factors <- length(factors)
            
            if (n_factors == 0) {
                self$results$instructions$setContent(
                    "<p><b>Please select at least one experimental factor to proceed with the optimization analysis.</b></p>"
                )
                return()
            }
            
            self$results$instructions$setContent(
                paste0("<h3>Assay Optimization Analysis Ready</h3>
                <p><b>Response Variable:</b> ", response_var, "</p>
                <p><b>Experimental Factors (", n_factors, "):</b> ", paste(factors, collapse = ", "), "</p>
                <p><b>Design Type:</b> ", gsub("_", " ", stringr::str_to_title(self$options$design_type)), "</p>
                <p><b>Optimization Goal:</b> ", gsub("_", " ", stringr::str_to_title(self$options$optimization_goal)), "</p>
                <p>Click <b>Results</b> below to view the analysis results.</p>")
            )
        },
        
        .run = function() {
            
            if (is.null(self$data) || is.null(self$options$response_var)) {
                return()
            }
            
            response_var <- self$options$response_var
            factors <- self$options$factors
            
            if (length(factors) == 0) {
                return()
            }
            
            # Get the data
            data <- self$data
            
            # Check for missing response variable
            if (!(response_var %in% names(data))) {
                self$results$design_summary$setContent("Error: Response variable not found in data.")
                return()
            }
            
            # Check for missing factors
            missing_factors <- factors[!(factors %in% names(data))]
            if (length(missing_factors) > 0) {
                self$results$design_summary$setContent(
                    paste("Error: The following factors were not found in data:", 
                          paste(missing_factors, collapse = ", "))
                )
                return()
            }
            
            tryCatch({
                
                # Prepare data
                response <- data[[response_var]]
                factor_data <- data[factors]
                
                # Remove missing data
                complete_cases <- complete.cases(response, factor_data)
                if (sum(complete_cases) < 3) {
                    self$results$design_summary$setContent("Error: Insufficient complete cases for analysis.")
                    return()
                }
                
                response <- response[complete_cases]
                factor_data <- factor_data[complete_cases, , drop = FALSE]
                
                # Convert factors to appropriate types
                for (i in seq_along(factor_data)) {
                    if (is.character(factor_data[[i]])) {
                        factor_data[[i]] <- as.factor(factor_data[[i]])
                    }
                }
                
                # Perform design analysis
                private$.analyzeDesign(response, factor_data, response_var, factors)
                
                # Perform power analysis if requested
                if (self$options$power_analysis) {
                    private$.performPowerAnalysis(response, factor_data)
                }
                
                # Perform optimization analysis
                private$.performOptimization(response, factor_data, response_var, factors)
                
                # Perform factor effects analysis
                private$.analyzeFactorEffects(response, factor_data, response_var, factors)
                
                # Perform response surface analysis if requested
                if (self$options$response_surface && length(factors) >= 2) {
                    private$.performResponseSurface(response, factor_data, response_var, factors)
                }
                
                # Perform quality control analysis if requested
                if (self$options$quality_control) {
                    private$.performQualityControl(response, factor_data, response_var)
                }
                
                # Perform method validation if requested
                if (self$options$method_validation) {
                    private$.performMethodValidation(response, factor_data, response_var, factors)
                }
                
            }, error = function(e) {
                self$results$design_summary$setContent(paste("Analysis error:", e$message))
            })
        },
        
        .analyzeDesign = function(response, factor_data, response_var, factors) {
            
            n_obs <- length(response)
            n_factors <- length(factors)
            design_type <- self$options$design_type
            replicates <- self$options$replicates
            
            # Basic design information
            design_info <- list(
                n_observations = n_obs,
                n_factors = n_factors,
                factor_names = factors,
                design_type = design_type,
                replicates = replicates
            )
            
            # Calculate design properties
            if (design_type == "factorial") {
                # Full factorial design analysis
                unique_levels <- sapply(factor_data, function(x) {
                    if (is.factor(x)) length(levels(x)) else length(unique(x))
                })
                total_runs <- prod(unique_levels)
                design_info$unique_levels <- unique_levels
                design_info$total_theoretical_runs <- total_runs
                design_info$efficiency <- n_obs / total_runs
                
            } else if (design_type == "fractional") {
                # Fractional factorial analysis
                min_runs <- 2^n_factors / 2  # Half fraction as example
                design_info$minimum_runs <- min_runs
                design_info$resolution <- ifelse(n_factors <= 4, "V", "III")
                
            } else if (design_type %in% c("central_composite", "box_behnken")) {
                # Response surface design analysis
                center_points <- sum(apply(factor_data, 1, function(x) all(x == median(x, na.rm = TRUE))))
                design_info$center_points <- center_points
                design_info$star_points <- n_obs - center_points
            }
            
            # Generate summary HTML
            html <- "<h3>Experimental Design Summary</h3>"
            html <- paste0(html, "<table class='jamovi-table'>")
            html <- paste0(html, "<tr><td><b>Design Type:</b></td><td>", stringr::str_to_title(gsub("_", " ", design_type)), "</td></tr>")
            html <- paste0(html, "<tr><td><b>Number of Factors:</b></td><td>", n_factors, "</td></tr>")
            html <- paste0(html, "<tr><td><b>Observations:</b></td><td>", n_obs, "</td></tr>")
            html <- paste0(html, "<tr><td><b>Replicates:</b></td><td>", replicates, "</td></tr>")
            
            if (!is.null(design_info$efficiency)) {
                html <- paste0(html, "<tr><td><b>Design Efficiency:</b></td><td>", round(design_info$efficiency * 100, 1), "%</td></tr>")
            }
            
            html <- paste0(html, "</table>")
            
            # Factor summary
            html <- paste0(html, "<h4>Factor Summary</h4>")
            html <- paste0(html, "<table class='jamovi-table'>")
            html <- paste0(html, "<tr><th>Factor</th><th>Type</th><th>Levels/Range</th><th>Missing</th></tr>")
            
            for (i in seq_along(factors)) {
                factor_name <- factors[i]
                factor_values <- factor_data[[i]]
                
                if (is.factor(factor_values)) {
                    factor_type <- "Categorical"
                    levels_info <- paste(levels(factor_values), collapse = ", ")
                } else {
                    factor_type <- "Continuous"
                    levels_info <- paste0(round(min(factor_values, na.rm = TRUE), 3), " to ", 
                                        round(max(factor_values, na.rm = TRUE), 3))
                }
                
                missing_count <- sum(is.na(factor_values))
                
                html <- paste0(html, "<tr>")
                html <- paste0(html, "<td>", factor_name, "</td>")
                html <- paste0(html, "<td>", factor_type, "</td>")
                html <- paste0(html, "<td>", levels_info, "</td>")
                html <- paste0(html, "<td>", missing_count, "</td>")
                html <- paste0(html, "</tr>")
            }
            
            html <- paste0(html, "</table>")
            
            self$results$design_summary$setContent(html)
        },
        
        .performPowerAnalysis = function(response, factor_data) {
            
            alpha_level <- self$options$alpha_level
            power_level <- self$options$power_level
            effect_size <- self$options$effect_size
            n_factors <- ncol(factor_data)
            n_obs <- length(response)
            
            html <- "<h3>Power Analysis Results</h3>"
            
            tryCatch({
                
                # ANOVA power analysis
                if (n_factors > 0) {
                    # Calculate current power
                    current_power <- pwr::pwr.f2.test(
                        u = n_factors,
                        v = n_obs - n_factors - 1,
                        f2 = effect_size^2 / (1 - effect_size^2),
                        sig.level = alpha_level
                    )$power
                    
                    # Calculate required sample size
                    required_n <- pwr::pwr.f2.test(
                        u = n_factors,
                        f2 = effect_size^2 / (1 - effect_size^2),
                        sig.level = alpha_level,
                        power = power_level
                    )$v + n_factors + 1
                    
                    html <- paste0(html, "<table class='jamovi-table'>")
                    html <- paste0(html, "<tr><td><b>Current Sample Size:</b></td><td>", n_obs, "</td></tr>")
                    html <- paste0(html, "<tr><td><b>Current Power:</b></td><td>", round(current_power, 3), "</td></tr>")
                    html <- paste0(html, "<tr><td><b>Required Sample Size:</b></td><td>", ceiling(required_n), "</td></tr>")
                    html <- paste0(html, "<tr><td><b>Target Power:</b></td><td>", power_level, "</td></tr>")
                    html <- paste0(html, "<tr><td><b>Effect Size (f²):</b></td><td>", round(effect_size^2, 3), "</td></tr>")
                    html <- paste0(html, "<tr><td><b>Significance Level:</b></td><td>", alpha_level, "</td></tr>")
                    html <- paste0(html, "</table>")
                    
                    # Power interpretation
                    html <- paste0(html, "<h4>Interpretation</h4>")
                    if (current_power >= power_level) {
                        html <- paste0(html, "<p style='color: green;'><b>✓ Adequate Power:</b> The current sample size provides sufficient power (", round(current_power, 3), ") to detect the specified effect size.</p>")
                    } else {
                        additional_n <- ceiling(required_n) - n_obs
                        html <- paste0(html, "<p style='color: orange;'><b>⚠ Insufficient Power:</b> Current power is ", round(current_power, 3), ". Consider adding ", additional_n, " more observations to achieve target power of ", power_level, ".</p>")
                    }
                }
                
            }, error = function(e) {
                html <- paste0(html, "<p>Power analysis error: ", e$message, "</p>")
            })
            
            self$results$power_analysis$setContent(html)
        },
        
        .performOptimization = function(response, factor_data, response_var, factors) {
            
            optimization_goal <- self$options$optimization_goal
            target_value <- self$options$target_value
            
            html <- "<h3>Optimization Results</h3>"
            
            tryCatch({
                
                # Fit linear model
                formula_str <- paste(response_var, "~", paste(factors, collapse = " + "))
                if (length(factors) > 1) {
                    # Add interaction terms for 2-factor interactions
                    interactions <- combn(factors, 2, FUN = function(x) paste(x, collapse = ":"))
                    if (length(interactions) <= 5) {  # Limit interactions to avoid overfitting
                        formula_str <- paste(formula_str, "+", paste(interactions, collapse = " + "))
                    }
                }
                
                model_data <- data.frame(response = response, factor_data)
                names(model_data)[1] <- response_var
                
                model <- lm(as.formula(formula_str), data = model_data)
                
                # Model summary
                model_summary <- summary(model)
                
                html <- paste0(html, "<h4>Model Summary</h4>")
                html <- paste0(html, "<table class='jamovi-table'>")
                html <- paste0(html, "<tr><td><b>R-squared:</b></td><td>", round(model_summary$r.squared, 4), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Adjusted R-squared:</b></td><td>", round(model_summary$adj.r.squared, 4), "</td></tr>")
                html <- paste0(html, "<tr><td><b>F-statistic:</b></td><td>", round(model_summary$fstatistic[1], 2), "</td></tr>")
                html <- paste0(html, "<tr><td><b>p-value:</b></td><td>", format.pval(pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Residual SE:</b></td><td>", round(model_summary$sigma, 4), "</td></tr>")
                html <- paste0(html, "</table>")
                
                # Optimization recommendations
                html <- paste0(html, "<h4>Optimization Recommendations</h4>")
                
                # Calculate predicted values at different factor levels
                predictions <- list()
                
                if (optimization_goal == "maximize_response") {
                    html <- paste0(html, "<p><b>Goal:</b> Maximize response variable</p>")
                    optimal_idx <- which.max(fitted(model))
                } else if (optimization_goal == "minimize_response") {
                    html <- paste0(html, "<p><b>Goal:</b> Minimize response variable</p>")
                    optimal_idx <- which.min(fitted(model))
                } else if (optimization_goal == "target_value") {
                    html <- paste0(html, "<p><b>Goal:</b> Target value = ", target_value, "</p>")
                    optimal_idx <- which.min(abs(fitted(model) - target_value))
                }
                
                if (exists("optimal_idx")) {
                    optimal_conditions <- factor_data[optimal_idx, , drop = FALSE]
                    optimal_response <- fitted(model)[optimal_idx]
                    
                    html <- paste0(html, "<p><b>Optimal Conditions:</b></p>")
                    html <- paste0(html, "<table class='jamovi-table'>")
                    for (factor_name in factors) {
                        html <- paste0(html, "<tr><td><b>", factor_name, ":</b></td><td>", 
                                     round(as.numeric(optimal_conditions[[factor_name]]), 4), "</td></tr>")
                    }
                    html <- paste0(html, "<tr><td><b>Predicted Response:</b></td><td>", round(optimal_response, 4), "</td></tr>")
                    html <- paste0(html, "</table>")
                }
                
            }, error = function(e) {
                html <- paste0(html, "<p>Optimization error: ", e$message, "</p>")
            })
            
            self$results$optimization_results$setContent(html)
        },
        
        .analyzeFactorEffects = function(response, factor_data, response_var, factors) {
            
            html <- "<h3>Factor Effects Analysis</h3>"
            
            tryCatch({
                
                # Fit ANOVA model
                model_data <- data.frame(response = response, factor_data)
                names(model_data)[1] <- response_var
                
                # Convert continuous factors to categorical for ANOVA if they have few unique values
                for (factor_name in factors) {
                    if (is.numeric(model_data[[factor_name]])) {
                        unique_vals <- length(unique(model_data[[factor_name]]))
                        if (unique_vals <= 5) {
                            model_data[[factor_name]] <- as.factor(model_data[[factor_name]])
                        }
                    }
                }
                
                formula_str <- paste(response_var, "~", paste(factors, collapse = " + "))
                model <- aov(as.formula(formula_str), data = model_data)
                
                # ANOVA table
                anova_result <- summary(model)[[1]]
                
                html <- paste0(html, "<h4>Analysis of Variance</h4>")
                html <- paste0(html, "<table class='jamovi-table'>")
                html <- paste0(html, "<tr><th>Source</th><th>Df</th><th>Sum Sq</th><th>Mean Sq</th><th>F value</th><th>Pr(&gt;F)</th><th>Significance</th></tr>")
                
                for (i in 1:(nrow(anova_result) - 1)) {  # Exclude residuals row for significance
                    source <- rownames(anova_result)[i]
                    df <- anova_result[i, "Df"]
                    sum_sq <- round(anova_result[i, "Sum Sq"], 4)
                    mean_sq <- round(anova_result[i, "Mean Sq"], 4)
                    f_val <- round(anova_result[i, "F value"], 4)
                    p_val <- anova_result[i, "Pr(>F)"]
                    p_val_formatted <- format.pval(p_val)
                    
                    significance <- ""
                    if (!is.na(p_val)) {
                        if (p_val < 0.001) significance <- "***"
                        else if (p_val < 0.01) significance <- "**"
                        else if (p_val < 0.05) significance <- "*"
                        else if (p_val < 0.1) significance <- "."
                    }
                    
                    html <- paste0(html, "<tr>")
                    html <- paste0(html, "<td>", source, "</td>")
                    html <- paste0(html, "<td>", df, "</td>")
                    html <- paste0(html, "<td>", sum_sq, "</td>")
                    html <- paste0(html, "<td>", mean_sq, "</td>")
                    html <- paste0(html, "<td>", f_val, "</td>")
                    html <- paste0(html, "<td>", p_val_formatted, "</td>")
                    html <- paste0(html, "<td>", significance, "</td>")
                    html <- paste0(html, "</tr>")
                }
                
                # Add residuals row
                i <- nrow(anova_result)
                html <- paste0(html, "<tr>")
                html <- paste0(html, "<td>Residuals</td>")
                html <- paste0(html, "<td>", anova_result[i, "Df"], "</td>")
                html <- paste0(html, "<td>", round(anova_result[i, "Sum Sq"], 4), "</td>")
                html <- paste0(html, "<td>", round(anova_result[i, "Mean Sq"], 4), "</td>")
                html <- paste0(html, "<td>-</td><td>-</td><td>-</td>")
                html <- paste0(html, "</tr>")
                
                html <- paste0(html, "</table>")
                html <- paste0(html, "<p><i>Significance codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1</i></p>")
                
                # Effect sizes (eta-squared)
                total_ss <- sum(anova_result$"Sum Sq")
                html <- paste0(html, "<h4>Effect Sizes (η²)</h4>")
                html <- paste0(html, "<table class='jamovi-table'>")
                html <- paste0(html, "<tr><th>Factor</th><th>η²</th><th>Interpretation</th></tr>")
                
                for (i in 1:(nrow(anova_result) - 1)) {
                    factor_name <- rownames(anova_result)[i]
                    eta_sq <- anova_result[i, "Sum Sq"] / total_ss
                    
                    interpretation <- ""
                    if (eta_sq < 0.01) interpretation <- "Very small"
                    else if (eta_sq < 0.06) interpretation <- "Small"
                    else if (eta_sq < 0.14) interpretation <- "Medium"
                    else interpretation <- "Large"
                    
                    html <- paste0(html, "<tr>")
                    html <- paste0(html, "<td>", factor_name, "</td>")
                    html <- paste0(html, "<td>", round(eta_sq, 4), "</td>")
                    html <- paste0(html, "<td>", interpretation, "</td>")
                    html <- paste0(html, "</tr>")
                }
                html <- paste0(html, "</table>")
                
            }, error = function(e) {
                html <- paste0(html, "<p>Factor effects analysis error: ", e$message, "</p>")
            })
            
            self$results$factor_effects$setContent(html)
        },
        
        .performResponseSurface = function(response, factor_data, response_var, factors) {
            
            if (length(factors) < 2) {
                self$results$response_surface_summary$setContent(
                    "<p>Response surface analysis requires at least 2 factors.</p>"
                )
                return()
            }
            
            html <- "<h3>Response Surface Analysis</h3>"
            
            tryCatch({
                
                # Fit second-order polynomial model
                model_data <- data.frame(response = response, factor_data)
                names(model_data)[1] <- response_var
                
                # Create formula with quadratic and interaction terms
                linear_terms <- paste(factors, collapse = " + ")
                quadratic_terms <- paste0("I(", factors, "^2)", collapse = " + ")
                
                if (length(factors) >= 2) {
                    interaction_terms <- combn(factors, 2, FUN = function(x) paste(x, collapse = ":"))
                    interaction_str <- paste(interaction_terms, collapse = " + ")
                    formula_str <- paste(response_var, "~", linear_terms, "+", quadratic_terms, "+", interaction_str)
                } else {
                    formula_str <- paste(response_var, "~", linear_terms, "+", quadratic_terms)
                }
                
                rs_model <- lm(as.formula(formula_str), data = model_data)
                rs_summary <- summary(rs_model)
                
                html <- paste0(html, "<h4>Response Surface Model</h4>")
                html <- paste0(html, "<table class='jamovi-table'>")
                html <- paste0(html, "<tr><td><b>R-squared:</b></td><td>", round(rs_summary$r.squared, 4), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Adjusted R-squared:</b></td><td>", round(rs_summary$adj.r.squared, 4), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Model F-statistic:</b></td><td>", round(rs_summary$fstatistic[1], 2), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Model p-value:</b></td><td>", format.pval(pf(rs_summary$fstatistic[1], rs_summary$fstatistic[2], rs_summary$fstatistic[3], lower.tail = FALSE)), "</td></tr>")
                html <- paste0(html, "</table>")
                
                # Stationary point analysis
                if (length(factors) == 2) {
                    coefs <- coef(rs_model)
                    
                    # Extract coefficients (assuming first factor is x1, second is x2)
                    b1 <- coefs[factors[1]]
                    b2 <- coefs[factors[2]]
                    b11 <- coefs[paste0("I(", factors[1], "^2)")]
                    b22 <- coefs[paste0("I(", factors[2], "^2)")]
                    b12 <- coefs[paste0(factors[1], ":", factors[2])]
                    
                    if (!any(is.na(c(b1, b2, b11, b22, b12)))) {
                        # Stationary point
                        det_B <- b11 * b22 - (b12/2)^2
                        
                        if (abs(det_B) > 1e-10) {
                            x1_stat <- -(b1 * b22 - b2 * b12/2) / (2 * det_B)
                            x2_stat <- -(b2 * b11 - b1 * b12/2) / (2 * det_B)
                            
                            # Classify stationary point
                            if (det_B > 0) {
                                if (b11 < 0) {
                                    point_type <- "Maximum"
                                } else {
                                    point_type <- "Minimum"
                                }
                            } else if (det_B < 0) {
                                point_type <- "Saddle point"
                            } else {
                                point_type <- "Ridge system"
                            }
                            
                            html <- paste0(html, "<h4>Stationary Point Analysis</h4>")
                            html <- paste0(html, "<table class='jamovi-table'>")
                            html <- paste0(html, "<tr><td><b>", factors[1], " (stationary):</b></td><td>", round(x1_stat, 4), "</td></tr>")
                            html <- paste0(html, "<tr><td><b>", factors[2], " (stationary):</b></td><td>", round(x2_stat, 4), "</td></tr>")
                            html <- paste0(html, "<tr><td><b>Point Type:</b></td><td>", point_type, "</td></tr>")
                            html <- paste0(html, "</table>")
                        }
                    }
                }
                
            }, error = function(e) {
                html <- paste0(html, "<p>Response surface analysis error: ", e$message, "</p>")
            })
            
            self$results$response_surface_summary$setContent(html)
        },
        
        .performQualityControl = function(response, factor_data, response_var) {
            
            html <- "<h3>Quality Control Analysis</h3>"
            
            tryCatch({
                
                # Basic statistics for control charts
                mean_response <- mean(response, na.rm = TRUE)
                sd_response <- sd(response, na.rm = TRUE)
                n <- length(response)
                
                # Control limits (3-sigma)
                ucl <- mean_response + 3 * sd_response
                lcl <- mean_response - 3 * sd_response
                
                # Warning limits (2-sigma)
                uwl <- mean_response + 2 * sd_response
                lwl <- mean_response - 2 * sd_response
                
                html <- paste0(html, "<h4>Control Chart Parameters</h4>")
                html <- paste0(html, "<table class='jamovi-table'>")
                html <- paste0(html, "<tr><td><b>Center Line (CL):</b></td><td>", round(mean_response, 4), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Upper Control Limit (UCL):</b></td><td>", round(ucl, 4), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Lower Control Limit (LCL):</b></td><td>", round(lcl, 4), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Upper Warning Limit (UWL):</b></td><td>", round(uwl, 4), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Lower Warning Limit (LWL):</b></td><td>", round(lwl, 4), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Standard Deviation:</b></td><td>", round(sd_response, 4), "</td></tr>")
                html <- paste0(html, "</table>")
                
                # Out-of-control detection
                out_of_control <- response < lcl | response > ucl
                warning_zone <- (response < lwl | response > uwl) & !out_of_control
                
                n_ooc <- sum(out_of_control, na.rm = TRUE)
                n_warning <- sum(warning_zone, na.rm = TRUE)
                
                html <- paste0(html, "<h4>Process Control Status</h4>")
                html <- paste0(html, "<table class='jamovi-table'>")
                html <- paste0(html, "<tr><td><b>Total Observations:</b></td><td>", n, "</td></tr>")
                html <- paste0(html, "<tr><td><b>Out of Control:</b></td><td>", n_ooc, " (", round(100 * n_ooc / n, 1), "%)</td></tr>")
                html <- paste0(html, "<tr><td><b>Warning Zone:</b></td><td>", n_warning, " (", round(100 * n_warning / n, 1), "%)</td></tr>")
                html <- paste0(html, "<tr><td><b>In Control:</b></td><td>", n - n_ooc - n_warning, " (", round(100 * (n - n_ooc - n_warning) / n, 1), "%)</td></tr>")
                html <- paste0(html, "</table>")
                
                # Process capability if specification limits are available
                # For now, use ±3σ as rough specification limits
                spec_upper <- mean_response + 3 * sd_response
                spec_lower <- mean_response - 3 * sd_response
                
                cp <- (spec_upper - spec_lower) / (6 * sd_response)
                cpk_upper <- (spec_upper - mean_response) / (3 * sd_response)
                cpk_lower <- (mean_response - spec_lower) / (3 * sd_response)
                cpk <- min(cpk_upper, cpk_lower)
                
                html <- paste0(html, "<h4>Process Capability (Estimated)</h4>")
                html <- paste0(html, "<table class='jamovi-table'>")
                html <- paste0(html, "<tr><td><b>Cp (Potential Capability):</b></td><td>", round(cp, 3), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Cpk (Actual Capability):</b></td><td>", round(cpk, 3), "</td></tr>")
                
                capability_assessment <- ""
                if (cpk >= 1.33) capability_assessment <- "Excellent"
                else if (cpk >= 1.0) capability_assessment <- "Adequate"
                else if (cpk >= 0.67) capability_assessment <- "Poor"
                else capability_assessment <- "Inadequate"
                
                html <- paste0(html, "<tr><td><b>Assessment:</b></td><td>", capability_assessment, "</td></tr>")
                html <- paste0(html, "</table>")
                
            }, error = function(e) {
                html <- paste0(html, "<p>Quality control analysis error: ", e$message, "</p>")
            })
            
            self$results$quality_control_summary$setContent(html)
        },
        
        .performMethodValidation = function(response, factor_data, response_var, factors) {
            
            html <- "<h3>Method Validation Results</h3>"
            
            tryCatch({
                
                # Basic validation metrics
                n <- length(response)
                mean_response <- mean(response, na.rm = TRUE)
                sd_response <- sd(response, na.rm = TRUE)
                cv_percent <- (sd_response / mean_response) * 100
                
                # Precision metrics
                html <- paste0(html, "<h4>Precision Assessment</h4>")
                html <- paste0(html, "<table class='jamovi-table'>")
                html <- paste0(html, "<tr><td><b>Number of Measurements:</b></td><td>", n, "</td></tr>")
                html <- paste0(html, "<tr><td><b>Mean Response:</b></td><td>", round(mean_response, 4), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Standard Deviation:</b></td><td>", round(sd_response, 4), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Coefficient of Variation:</b></td><td>", round(cv_percent, 2), "%</td></tr>")
                html <- paste0(html, "<tr><td><b>Relative Standard Deviation:</b></td><td>", round(cv_percent, 2), "%</td></tr>")
                html <- paste0(html, "</table>")
                
                # Precision interpretation
                precision_assessment <- ""
                if (cv_percent <= 5) precision_assessment <- "Excellent precision"
                else if (cv_percent <= 10) precision_assessment <- "Good precision"
                else if (cv_percent <= 20) precision_assessment <- "Acceptable precision"
                else precision_assessment <- "Poor precision"
                
                html <- paste0(html, "<p><b>Precision Assessment:</b> ", precision_assessment, "</p>")
                
                # Linearity assessment (if continuous factors are present)
                continuous_factors <- sapply(factor_data, is.numeric)
                if (any(continuous_factors)) {
                    cont_factor_name <- names(factor_data)[which(continuous_factors)[1]]
                    cont_factor <- factor_data[[cont_factor_name]]
                    
                    # Fit linear relationship
                    linear_model <- lm(response ~ cont_factor)
                    linear_summary <- summary(linear_model)
                    
                    html <- paste0(html, "<h4>Linearity Assessment (", cont_factor_name, ")</h4>")
                    html <- paste0(html, "<table class='jamovi-table'>")
                    html <- paste0(html, "<tr><td><b>Correlation coefficient (r):</b></td><td>", round(sqrt(linear_summary$r.squared), 4), "</td></tr>")
                    html <- paste0(html, "<tr><td><b>R-squared:</b></td><td>", round(linear_summary$r.squared, 4), "</td></tr>")
                    html <- paste0(html, "<tr><td><b>Slope:</b></td><td>", round(coef(linear_model)[2], 4), "</td></tr>")
                    html <- paste0(html, "<tr><td><b>Intercept:</b></td><td>", round(coef(linear_model)[1], 4), "</td></tr>")
                    html <- paste0(html, "<tr><td><b>p-value (linearity):</b></td><td>", format.pval(linear_summary$coefficients[2, 4]), "</td></tr>")
                    html <- paste0(html, "</table>")
                    
                    # Linearity interpretation
                    r_squared <- linear_summary$r.squared
                    linearity_assessment <- ""
                    if (r_squared >= 0.95) linearity_assessment <- "Excellent linearity"
                    else if (r_squared >= 0.90) linearity_assessment <- "Good linearity"
                    else if (r_squared >= 0.80) linearity_assessment <- "Acceptable linearity"
                    else linearity_assessment <- "Poor linearity"
                    
                    html <- paste0(html, "<p><b>Linearity Assessment:</b> ", linearity_assessment, "</p>")
                }
                
                # Robustness assessment
                html <- paste0(html, "<h4>Robustness Assessment</h4>")
                
                if (length(factors) > 0) {
                    # Calculate factor effects for robustness
                    model_data <- data.frame(response = response, factor_data)
                    names(model_data)[1] <- response_var
                    
                    robustness_model <- lm(as.formula(paste(response_var, "~", paste(factors, collapse = " + "))), data = model_data)
                    robustness_summary <- summary(robustness_model)
                    
                    # Identify significant factors that affect robustness
                    p_values <- robustness_summary$coefficients[-1, 4]  # Exclude intercept
                    significant_factors <- factors[p_values < 0.05]
                    
                    html <- paste0(html, "<table class='jamovi-table'>")
                    html <- paste0(html, "<tr><td><b>Total Factors Tested:</b></td><td>", length(factors), "</td></tr>")
                    html <- paste0(html, "<tr><td><b>Significant Factors (p&lt;0.05):</b></td><td>", length(significant_factors), "</td></tr>")
                    
                    if (length(significant_factors) > 0) {
                        html <- paste0(html, "<tr><td><b>Critical Factors:</b></td><td>", paste(significant_factors, collapse = ", "), "</td></tr>")
                    }
                    
                    html <- paste0(html, "</table>")
                    
                    robustness_assessment <- ""
                    if (length(significant_factors) == 0) robustness_assessment <- "Highly robust method"
                    else if (length(significant_factors) <= length(factors) * 0.3) robustness_assessment <- "Adequately robust method"
                    else robustness_assessment <- "Method requires careful control of conditions"
                    
                    html <- paste0(html, "<p><b>Robustness Assessment:</b> ", robustness_assessment, "</p>")
                }
                
            }, error = function(e) {
                html <- paste0(html, "<p>Method validation error: ", e$message, "</p>")
            })
            
            self$results$validation_metrics$setContent(html)
        }
    )
)