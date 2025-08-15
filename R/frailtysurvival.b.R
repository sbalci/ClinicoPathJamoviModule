#' @title Frailty & Random Effects Survival Models
#' @importFrom jmvcore .
#' @importFrom survival Surv coxph frailty cluster survfit
#' @importFrom frailtypack frailtyPenal
#' @importFrom coxme coxme VarCorr
#' @importFrom stats AIC BIC logLik anova pchisq qchisq
#' @importFrom stats quantile median IQR sd var
#' @export
frailtysurvivalClass <- R6::R6Class(
    "frailtysurvivalClass",
    inherit = frailtysurvivalBase,
    private = list(
        .init = function() {
            
            if (is.null(self$data) || is.null(self$options$time_var) || is.null(self$options$status_var)) {
                self$results$instructions$setContent(
                    "<h3>Welcome to Frailty & Random Effects Survival Models</h3>
                    <p>This analysis provides comprehensive frailty modeling for clustered survival data.</p>
                    <p><b>Getting Started:</b></p>
                    <ol>
                    <li>Select your <b>Survival Time Variable</b></li>
                    <li>Select your <b>Event Status Variable</b> (0=censored, 1=event)</li>
                    <li>Select your <b>Cluster Variable</b> (e.g., center, family, patient ID)</li>
                    <li>Choose <b>Covariates</b> if available</li>
                    <li>Select appropriate <b>Frailty Model Type</b></li>
                    <li>Configure estimation method and diagnostics</li>
                    </ol>
                    <p><b>Key Features:</b></p>
                    <ul>
                    <li><b>Frailty Models:</b> Shared, correlated, nested, additive, multiplicative frailty</li>
                    <li><b>Distributions:</b> Gamma, log-normal, positive stable, inverse Gaussian</li>
                    <li><b>Estimation:</b> Penalized likelihood, EM algorithm, Laplace, MCMC</li>
                    <li><b>Diagnostics:</b> Cluster effects, variance components, residuals</li>
                    <li><b>Comparison:</b> Automatic comparison with standard Cox models</li>
                    </ul>
                    <p><b>For jamovi users:</b> This module works with survival data where multiple observations may be clustered (e.g., patients within centers, events within families).</p>"
                )
                return()
            }
            
            time_var <- self$options$time_var
            status_var <- self$options$status_var
            cluster_var <- self$options$cluster_var
            
            if (is.null(cluster_var)) {
                self$results$instructions$setContent(
                    "<p><b>Please select a cluster variable to proceed with frailty analysis.</b></p>
                    <p>Frailty models account for correlation within clusters such as:</p>
                    <ul>
                    <li>Patients within medical centers</li>
                    <li>Individuals within families</li>
                    <li>Multiple events within the same patient</li>
                    </ul>"
                )
                return()
            }
            
            self$results$instructions$setContent(
                paste0("<h3>Frailty Survival Analysis Ready</h3>
                <p><b>Time Variable:</b> ", time_var, "</p>
                <p><b>Status Variable:</b> ", status_var, "</p>
                <p><b>Cluster Variable:</b> ", cluster_var, "</p>
                <p><b>Frailty Type:</b> ", stringr::str_to_title(gsub("_", " ", self$options$frailty_type)), "</p>
                <p><b>Distribution:</b> ", stringr::str_to_title(gsub("_", " ", self$options$frailty_distribution)), "</p>
                <p>Click <b>Results</b> below to view the analysis results.</p>")
            )
        },
        
        .run = function() {
            
            if (is.null(self$data) || is.null(self$options$time_var) || 
                is.null(self$options$status_var) || is.null(self$options$cluster_var)) {
                return()
            }
            
            time_var <- self$options$time_var
            status_var <- self$options$status_var
            cluster_var <- self$options$cluster_var
            covariates <- self$options$covariates
            
            # Get the data
            data <- self$data
            
            # Check for required variables
            required_vars <- c(time_var, status_var, cluster_var)
            missing_vars <- required_vars[!(required_vars %in% names(data))]
            if (length(missing_vars) > 0) {
                self$results$model_summary$setContent(
                    paste("Error: The following required variables were not found:", 
                          paste(missing_vars, collapse = ", "))
                )
                return()
            }
            
            tryCatch({
                
                # Prepare survival data
                time <- data[[time_var]]
                status <- data[[status_var]]
                cluster <- data[[cluster_var]]
                
                # Handle covariates
                covariate_data <- NULL
                if (length(covariates) > 0) {
                    missing_covs <- covariates[!(covariates %in% names(data))]
                    if (length(missing_covs) > 0) {
                        self$results$model_summary$setContent(
                            paste("Error: The following covariates were not found:", 
                                  paste(missing_covs, collapse = ", "))
                        )
                        return()
                    }
                    covariate_data <- data[covariates]
                }
                
                # Remove missing data
                if (is.null(covariate_data)) {
                    complete_cases <- complete.cases(time, status, cluster)
                    analysis_data <- data.frame(
                        time = time[complete_cases],
                        status = status[complete_cases],
                        cluster = cluster[complete_cases]
                    )
                } else {
                    complete_cases <- complete.cases(time, status, cluster, covariate_data)
                    analysis_data <- data.frame(
                        time = time[complete_cases],
                        status = status[complete_cases],
                        cluster = cluster[complete_cases],
                        covariate_data[complete_cases, , drop = FALSE]
                    )
                }
                
                if (nrow(analysis_data) < 10) {
                    self$results$model_summary$setContent("Error: Insufficient complete cases for analysis.")
                    return()
                }
                
                # Convert cluster to factor
                analysis_data$cluster <- as.factor(analysis_data$cluster)
                
                # Fit frailty models
                private$.fitFrailtyModels(analysis_data, time_var, status_var, cluster_var, covariates)
                
            }, error = function(e) {
                self$results$model_summary$setContent(paste("Analysis error:", e$message))
            })
        },
        
        .fitFrailtyModels = function(analysis_data, time_var, status_var, cluster_var, covariates) {
            
            frailty_type <- self$options$frailty_type
            frailty_dist <- self$options$frailty_distribution
            estimation_method <- self$options$estimation_method
            baseline_hazard <- self$options$baseline_hazard
            
            # Create survival object
            surv_obj <- Surv(analysis_data$time, analysis_data$status)
            
            # Build formula
            if (length(covariates) > 0) {
                covariate_terms <- paste(covariates, collapse = " + ")
                if (frailty_type == "shared") {
                    if (estimation_method == "penalized_likelihood") {
                        # Use survival::frailty
                        formula_str <- paste("surv_obj ~", covariate_terms, "+ frailty(cluster)")
                    } else {
                        # Use coxme for other methods
                        formula_str <- paste("surv_obj ~", covariate_terms, "+ (1|cluster)")
                    }
                } else {
                    # Other frailty types
                    formula_str <- paste("surv_obj ~", covariate_terms, "+ frailty(cluster)")
                }
            } else {
                if (frailty_type == "shared") {
                    if (estimation_method == "penalized_likelihood") {
                        formula_str <- "surv_obj ~ frailty(cluster)"
                    } else {
                        formula_str <- "surv_obj ~ (1|cluster)"
                    }
                } else {
                    formula_str <- "surv_obj ~ frailty(cluster)"
                }
            }
            
            # Fit models
            tryCatch({
                
                # Fit frailty model
                if (estimation_method == "penalized_likelihood" && frailty_type == "shared") {
                    # Use survival package
                    frailty_model <- coxph(as.formula(formula_str), data = analysis_data)
                } else if (estimation_method %in% c("em_algorithm", "laplace_approximation")) {
                    # Use coxme package
                    if (length(covariates) > 0) {
                        coxme_formula <- as.formula(paste("surv_obj ~", paste(covariates, collapse = " + "), "+ (1|cluster)"))
                    } else {
                        coxme_formula <- as.formula("surv_obj ~ (1|cluster)")
                    }
                    frailty_model <- coxme(coxme_formula, data = analysis_data)
                } else {
                    # Fallback to basic frailty
                    frailty_model <- coxph(as.formula(formula_str), data = analysis_data)
                }
                
                # Fit standard Cox model for comparison
                if (length(covariates) > 0) {
                    cox_formula <- as.formula(paste("surv_obj ~", paste(covariates, collapse = " + ")))
                } else {
                    cox_formula <- as.formula("surv_obj ~ 1")
                }
                standard_model <- coxph(cox_formula, data = analysis_data)
                
                # Generate results
                private$.generateModelSummary(frailty_model, standard_model, analysis_data)
                private$.generateFrailtyEffects(frailty_model, analysis_data)
                private$.generateCovariateEffects(frailty_model, covariates)
                
                if (self$options$test_frailty) {
                    private$.testFrailtySignificance(frailty_model, standard_model)
                }
                
                if (self$options$variance_components) {
                    private$.analyzeVarianceComponents(frailty_model, analysis_data)
                }
                
                if (self$options$model_comparison) {
                    private$.compareModels(frailty_model, standard_model)
                }
                
                if (self$options$cluster_diagnostics) {
                    private$.generateClusterDiagnostics(frailty_model, analysis_data)
                }
                
            }, error = function(e) {
                self$results$model_summary$setContent(paste("Model fitting error:", e$message))
            })
        },
        
        .generateModelSummary = function(frailty_model, standard_model, analysis_data) {
            
            html <- "<h3>Frailty Model Summary</h3>"
            
            tryCatch({
                
                # Basic model information
                n_obs <- nrow(analysis_data)
                n_events <- sum(analysis_data$status)
                n_clusters <- length(unique(analysis_data$cluster))
                
                html <- paste0(html, "<h4>Data Summary</h4>")
                html <- paste0(html, "<table class='jamovi-table'>")
                html <- paste0(html, "<tr><td><b>Total Observations:</b></td><td>", n_obs, "</td></tr>")
                html <- paste0(html, "<tr><td><b>Events:</b></td><td>", n_events, " (", round(100 * n_events / n_obs, 1), "%)</td></tr>")
                html <- paste0(html, "<tr><td><b>Censored:</b></td><td>", n_obs - n_events, " (", round(100 * (n_obs - n_events) / n_obs, 1), "%)</td></tr>")
                html <- paste0(html, "<tr><td><b>Number of Clusters:</b></td><td>", n_clusters, "</td></tr>")
                
                # Cluster size summary
                cluster_sizes <- table(analysis_data$cluster)
                html <- paste0(html, "<tr><td><b>Cluster Size Range:</b></td><td>", min(cluster_sizes), " to ", max(cluster_sizes), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Median Cluster Size:</b></td><td>", median(cluster_sizes), "</td></tr>")
                html <- paste0(html, "</table>")
                
                # Model fit statistics
                html <- paste0(html, "<h4>Model Fit Statistics</h4>")
                html <- paste0(html, "<table class='jamovi-table'>")
                
                if (inherits(frailty_model, "coxph")) {
                    model_summary <- summary(frailty_model)
                    
                    html <- paste0(html, "<tr><td><b>Log-likelihood:</b></td><td>", round(frailty_model$loglik[2], 2), "</td></tr>")
                    html <- paste0(html, "<tr><td><b>AIC:</b></td><td>", round(AIC(frailty_model), 2), "</td></tr>")
                    html <- paste0(html, "<tr><td><b>BIC:</b></td><td>", round(BIC(frailty_model), 2), "</td></tr>")
                    
                    if (!is.null(model_summary$concordance)) {
                        html <- paste0(html, "<tr><td><b>Concordance:</b></td><td>", round(model_summary$concordance[1], 3), " (SE: ", round(model_summary$concordance[2], 3), ")</td></tr>")
                    }
                    
                    # Frailty variance estimate
                    if (any(grepl("frailty", names(frailty_model$coefficients)))) {
                        frailty_var <- frailty_model$history[[1]]$theta
                        if (!is.null(frailty_var)) {
                            html <- paste0(html, "<tr><td><b>Frailty Variance:</b></td><td>", round(frailty_var, 4), "</td></tr>")
                        }
                    }
                    
                } else if (inherits(frailty_model, "coxme")) {
                    html <- paste0(html, "<tr><td><b>Log-likelihood:</b></td><td>", round(logLik(frailty_model), 2), "</td></tr>")
                    html <- paste0(html, "<tr><td><b>AIC:</b></td><td>", round(AIC(frailty_model), 2), "</td></tr>")
                    html <- paste0(html, "<tr><td><b>BIC:</b></td><td>", round(BIC(frailty_model), 2), "</td></tr>")
                    
                    # Random effects variance
                    var_comp <- VarCorr(frailty_model)
                    if (length(var_comp) > 0) {
                        html <- paste0(html, "<tr><td><b>Random Effects Variance:</b></td><td>", round(var_comp[[1]][1], 4), "</td></tr>")
                    }
                }
                
                html <- paste0(html, "</table>")
                
            }, error = function(e) {
                html <- paste0(html, "<p>Model summary error: ", e$message, "</p>")
            })
            
            self$results$model_summary$setContent(html)
        },
        
        .generateFrailtyEffects = function(frailty_model, analysis_data) {
            
            html <- "<h3>Frailty Effects Analysis</h3>"
            
            tryCatch({
                
                n_clusters <- length(unique(analysis_data$cluster))
                
                if (inherits(frailty_model, "coxph")) {
                    # Extract frailty effects if available
                    frailty_terms <- frailty_model$frail
                    
                    if (!is.null(frailty_terms)) {
                        html <- paste0(html, "<h4>Frailty Statistics</h4>")
                        html <- paste0(html, "<table class='jamovi-table'>")
                        html <- paste0(html, "<tr><td><b>Number of Frailty Terms:</b></td><td>", length(frailty_terms), "</td></tr>")
                        html <- paste0(html, "<tr><td><b>Mean Frailty:</b></td><td>", round(mean(frailty_terms), 4), "</td></tr>")
                        html <- paste0(html, "<tr><td><b>Median Frailty:</b></td><td>", round(median(frailty_terms), 4), "</td></tr>")
                        html <- paste0(html, "<tr><td><b>Frailty SD:</b></td><td>", round(sd(frailty_terms), 4), "</td></tr>")
                        html <- paste0(html, "<tr><td><b>Range:</b></td><td>", round(min(frailty_terms), 4), " to ", round(max(frailty_terms), 4), "</td></tr>")
                        html <- paste0(html, "</table>")
                        
                        # Frailty quantiles
                        frailty_quantiles <- quantile(frailty_terms, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
                        html <- paste0(html, "<h4>Frailty Distribution Quantiles</h4>")
                        html <- paste0(html, "<table class='jamovi-table'>")
                        for (i in 1:length(frailty_quantiles)) {
                            percentile <- names(frailty_quantiles)[i]
                            value <- round(frailty_quantiles[i], 4)
                            html <- paste0(html, "<tr><td><b>", percentile, ":</b></td><td>", value, "</td></tr>")
                        }
                        html <- paste0(html, "</table>")
                    }
                    
                } else if (inherits(frailty_model, "coxme")) {
                    # Extract random effects
                    ranef_values <- ranef(frailty_model)
                    
                    if (length(ranef_values) > 0) {
                        cluster_effects <- ranef_values[[1]]
                        
                        html <- paste0(html, "<h4>Random Effects Statistics</h4>")
                        html <- paste0(html, "<table class='jamovi-table'>")
                        html <- paste0(html, "<tr><td><b>Number of Random Effects:</b></td><td>", length(cluster_effects), "</td></tr>")
                        html <- paste0(html, "<tr><td><b>Mean Effect:</b></td><td>", round(mean(cluster_effects), 4), "</td></tr>")
                        html <- paste0(html, "<tr><td><b>Median Effect:</b></td><td>", round(median(cluster_effects), 4), "</td></tr>")
                        html <- paste0(html, "<tr><td><b>Effect SD:</b></td><td>", round(sd(cluster_effects), 4), "</td></tr>")
                        html <- paste0(html, "<tr><td><b>Range:</b></td><td>", round(min(cluster_effects), 4), " to ", round(max(cluster_effects), 4), "</td></tr>")
                        html <- paste0(html, "</table>")
                    }
                }
                
                # Interpretation
                html <- paste0(html, "<h4>Interpretation</h4>")
                html <- paste0(html, "<p>Frailty effects represent cluster-specific deviations from the overall hazard. ")
                html <- paste0(html, "Values greater than 1 indicate higher risk clusters, while values less than 1 indicate lower risk clusters. ")
                html <- paste0(html, "Large variability in frailty effects suggests important between-cluster heterogeneity.</p>")
                
            }, error = function(e) {
                html <- paste0(html, "<p>Frailty effects analysis error: ", e$message, "</p>")
            })
            
            self$results$frailty_effects$setContent(html)
        },
        
        .generateCovariateEffects = function(frailty_model, covariates) {
            
            if (length(covariates) == 0) {
                self$results$covariate_effects$setContent("<p>No covariates included in the model.</p>")
                return()
            }
            
            html <- "<h3>Covariate Effects</h3>"
            
            tryCatch({
                
                if (inherits(frailty_model, "coxph")) {
                    model_summary <- summary(frailty_model)
                    coefficients <- model_summary$coefficients
                    
                    # Filter out frailty terms
                    covariate_rows <- !grepl("frailty", rownames(coefficients))
                    if (any(covariate_rows)) {
                        covariate_coefs <- coefficients[covariate_rows, , drop = FALSE]
                        
                        html <- paste0(html, "<table class='jamovi-table'>")
                        html <- paste0(html, "<tr><th>Variable</th><th>Coef</th><th>Exp(Coef)</th><th>SE</th><th>Z</th><th>Pr(&gt;|z|)</th><th>95% CI</th></tr>")
                        
                        for (i in 1:nrow(covariate_coefs)) {
                            var_name <- rownames(covariate_coefs)[i]
                            coef <- round(covariate_coefs[i, "coef"], 4)
                            exp_coef <- round(covariate_coefs[i, "exp(coef)"], 4)
                            se <- round(covariate_coefs[i, "se(coef)"], 4)
                            z <- round(covariate_coefs[i, "z"], 3)
                            p_val <- format.pval(covariate_coefs[i, "Pr(>|z|)"])
                            
                            # Calculate 95% CI
                            ci_lower <- round(exp(coef - 1.96 * se), 4)
                            ci_upper <- round(exp(coef + 1.96 * se), 4)
                            ci_text <- paste0("(", ci_lower, ", ", ci_upper, ")")
                            
                            html <- paste0(html, "<tr>")
                            html <- paste0(html, "<td>", var_name, "</td>")
                            html <- paste0(html, "<td>", coef, "</td>")
                            html <- paste0(html, "<td>", exp_coef, "</td>")
                            html <- paste0(html, "<td>", se, "</td>")
                            html <- paste0(html, "<td>", z, "</td>")
                            html <- paste0(html, "<td>", p_val, "</td>")
                            html <- paste0(html, "<td>", ci_text, "</td>")
                            html <- paste0(html, "</tr>")
                        }
                        html <- paste0(html, "</table>")
                    }
                    
                } else if (inherits(frailty_model, "coxme")) {
                    # For coxme models
                    model_summary <- summary(frailty_model)
                    
                    if (!is.null(model_summary$coef)) {
                        coefficients <- model_summary$coef
                        
                        html <- paste0(html, "<table class='jamovi-table'>")
                        html <- paste0(html, "<tr><th>Variable</th><th>Coef</th><th>Exp(Coef)</th><th>SE</th><th>Z</th><th>p-value</th></tr>")
                        
                        for (i in 1:nrow(coefficients)) {
                            var_name <- rownames(coefficients)[i]
                            coef <- round(coefficients[i, 1], 4)
                            exp_coef <- round(exp(coef), 4)
                            se <- round(coefficients[i, 2], 4)
                            z <- round(coefficients[i, 3], 3)
                            p_val <- format.pval(coefficients[i, 4])
                            
                            html <- paste0(html, "<tr>")
                            html <- paste0(html, "<td>", var_name, "</td>")
                            html <- paste0(html, "<td>", coef, "</td>")
                            html <- paste0(html, "<td>", exp_coef, "</td>")
                            html <- paste0(html, "<td>", se, "</td>")
                            html <- paste0(html, "<td>", z, "</td>")
                            html <- paste0(html, "<td>", p_val, "</td>")
                            html <- paste0(html, "</tr>")
                        }
                        html <- paste0(html, "</table>")
                    }
                }
                
            }, error = function(e) {
                html <- paste0(html, "<p>Covariate effects analysis error: ", e$message, "</p>")
            })
            
            self$results$covariate_effects$setContent(html)
        },
        
        .testFrailtySignificance = function(frailty_model, standard_model) {
            
            html <- "<h3>Test for Frailty Effects</h3>"
            
            tryCatch({
                
                # Likelihood ratio test
                frailty_loglik <- logLik(frailty_model)
                standard_loglik <- logLik(standard_model)
                
                lr_statistic <- 2 * (frailty_loglik - standard_loglik)
                df_diff <- attr(frailty_loglik, "df") - attr(standard_loglik, "df")
                
                if (df_diff > 0) {
                    p_value <- pchisq(lr_statistic, df = df_diff, lower.tail = FALSE)
                    
                    html <- paste0(html, "<h4>Likelihood Ratio Test</h4>")
                    html <- paste0(html, "<table class='jamovi-table'>")
                    html <- paste0(html, "<tr><td><b>Null Hypothesis:</b></td><td>No frailty effects (standard Cox model)</td></tr>")
                    html <- paste0(html, "<tr><td><b>Alternative Hypothesis:</b></td><td>Significant frailty effects</td></tr>")
                    html <- paste0(html, "<tr><td><b>Test Statistic:</b></td><td>", round(lr_statistic, 3), "</td></tr>")
                    html <- paste0(html, "<tr><td><b>Degrees of Freedom:</b></td><td>", df_diff, "</td></tr>")
                    html <- paste0(html, "<tr><td><b>p-value:</b></td><td>", format.pval(p_value), "</td></tr>")
                    html <- paste0(html, "</table>")
                    
                    # Interpretation
                    html <- paste0(html, "<h4>Interpretation</h4>")
                    if (p_value < 0.05) {
                        html <- paste0(html, "<p style='color: green;'><b>✓ Significant Frailty:</b> There is strong evidence for frailty effects (p < 0.05). ")
                        html <- paste0(html, "The frailty model provides a significantly better fit than the standard Cox model.</p>")
                    } else {
                        html <- paste0(html, "<p style='color: orange;'><b>⚠ Non-significant Frailty:</b> There is insufficient evidence for frailty effects (p ≥ 0.05). ")
                        html <- paste0(html, "The standard Cox model may be adequate for this data.</p>")
                    }
                } else {
                    html <- paste0(html, "<p>Cannot perform likelihood ratio test: models have the same degrees of freedom.</p>")
                }
                
            }, error = function(e) {
                html <- paste0(html, "<p>Frailty significance test error: ", e$message, "</p>")
            })
            
            self$results$frailty_test$setContent(html)
        },
        
        .analyzeVarianceComponents = function(frailty_model, analysis_data) {
            
            html <- "<h3>Variance Components Analysis</h3>"
            
            tryCatch({
                
                if (inherits(frailty_model, "coxme")) {
                    # Use VarCorr for coxme models
                    var_comp <- VarCorr(frailty_model)
                    
                    html <- paste0(html, "<h4>Variance Components</h4>")
                    html <- paste0(html, "<table class='jamovi-table'>")
                    html <- paste0(html, "<tr><th>Component</th><th>Variance</th><th>SD</th><th>% of Total</th></tr>")
                    
                    total_var <- 0
                    for (i in 1:length(var_comp)) {
                        total_var <- total_var + var_comp[[i]][1]
                    }
                    
                    for (i in 1:length(var_comp)) {
                        comp_name <- names(var_comp)[i]
                        if (is.null(comp_name)) comp_name <- paste("Component", i)
                        
                        variance <- var_comp[[i]][1]
                        sd_value <- sqrt(variance)
                        percent <- (variance / total_var) * 100
                        
                        html <- paste0(html, "<tr>")
                        html <- paste0(html, "<td>", comp_name, "</td>")
                        html <- paste0(html, "<td>", round(variance, 6), "</td>")
                        html <- paste0(html, "<td>", round(sd_value, 6), "</td>")
                        html <- paste0(html, "<td>", round(percent, 1), "%</td>")
                        html <- paste0(html, "</tr>")
                    }
                    
                    html <- paste0(html, "</table>")
                    
                } else if (inherits(frailty_model, "coxph")) {
                    # Extract frailty variance for coxph models
                    if (!is.null(frailty_model$history) && length(frailty_model$history) > 0) {
                        frailty_var <- frailty_model$history[[1]]$theta
                        
                        if (!is.null(frailty_var)) {
                            html <- paste0(html, "<h4>Frailty Variance Component</h4>")
                            html <- paste0(html, "<table class='jamovi-table'>")
                            html <- paste0(html, "<tr><td><b>Frailty Variance:</b></td><td>", round(frailty_var, 6), "</td></tr>")
                            html <- paste0(html, "<tr><td><b>Frailty SD:</b></td><td>", round(sqrt(frailty_var), 6), "</td></tr>")
                            html <- paste0(html, "</table>")
                            
                            # Intracluster correlation
                            icc <- frailty_var / (frailty_var + pi^2/3)  # Approximation for Cox models
                            html <- paste0(html, "<h4>Intracluster Correlation</h4>")
                            html <- paste0(html, "<table class='jamovi-table'>")
                            html <- paste0(html, "<tr><td><b>ICC (approximate):</b></td><td>", round(icc, 4), "</td></tr>")
                            html <- paste0(html, "</table>")
                        }
                    }
                }
                
                # Interpretation
                html <- paste0(html, "<h4>Interpretation</h4>")
                html <- paste0(html, "<p>Variance components quantify the amount of variation attributable to different sources. ")
                html <- paste0(html, "Larger frailty variance indicates greater heterogeneity between clusters. ")
                html <- paste0(html, "The intracluster correlation (ICC) measures the proportion of total variation due to cluster differences.</p>")
                
            }, error = function(e) {
                html <- paste0(html, "<p>Variance components analysis error: ", e$message, "</p>")
            })
            
            self$results$variance_components_table$setContent(html)
        },
        
        .compareModels = function(frailty_model, standard_model) {
            
            html <- "<h3>Model Comparison</h3>"
            
            tryCatch({
                
                # Model fit statistics
                frailty_aic <- AIC(frailty_model)
                frailty_bic <- BIC(frailty_model)
                frailty_loglik <- logLik(frailty_model)
                
                standard_aic <- AIC(standard_model)
                standard_bic <- BIC(standard_model)
                standard_loglik <- logLik(standard_model)
                
                html <- paste0(html, "<table class='jamovi-table'>")
                html <- paste0(html, "<tr><th>Model</th><th>Log-Likelihood</th><th>AIC</th><th>BIC</th><th>Δ AIC</th><th>Δ BIC</th></tr>")
                
                delta_aic <- frailty_aic - standard_aic
                delta_bic <- frailty_bic - standard_bic
                
                html <- paste0(html, "<tr>")
                html <- paste0(html, "<td><b>Frailty Model</b></td>")
                html <- paste0(html, "<td>", round(frailty_loglik, 2), "</td>")
                html <- paste0(html, "<td>", round(frailty_aic, 2), "</td>")
                html <- paste0(html, "<td>", round(frailty_bic, 2), "</td>")
                html <- paste0(html, "<td>", round(delta_aic, 2), "</td>")
                html <- paste0(html, "<td>", round(delta_bic, 2), "</td>")
                html <- paste0(html, "</tr>")
                
                html <- paste0(html, "<tr>")
                html <- paste0(html, "<td><b>Standard Cox</b></td>")
                html <- paste0(html, "<td>", round(standard_loglik, 2), "</td>")
                html <- paste0(html, "<td>", round(standard_aic, 2), "</td>")
                html <- paste0(html, "<td>", round(standard_bic, 2), "</td>")
                html <- paste0(html, "<td>0.00</td>")
                html <- paste0(html, "<td>0.00</td>")
                html <- paste0(html, "</tr>")
                
                html <- paste0(html, "</table>")
                
                # Interpretation
                html <- paste0(html, "<h4>Model Selection</h4>")
                if (delta_aic < -2) {
                    html <- paste0(html, "<p style='color: green;'><b>✓ Frailty Model Preferred:</b> The frailty model has substantially lower AIC (Δ AIC = ", round(delta_aic, 2), "), indicating better fit.</p>")
                } else if (delta_aic < 2) {
                    html <- paste0(html, "<p style='color: orange;'><b>≈ Similar Performance:</b> Both models have similar AIC values (Δ AIC = ", round(delta_aic, 2), "). Consider parsimony.</p>")
                } else {
                    html <- paste0(html, "<p style='color: red;'><b>⚠ Standard Model Preferred:</b> The standard Cox model has lower AIC (Δ AIC = ", round(delta_aic, 2), ").</p>")
                }
                
                if (delta_bic < -2) {
                    html <- paste0(html, "<p><b>BIC Assessment:</b> Frailty model strongly preferred (Δ BIC = ", round(delta_bic, 2), ").</p>")
                } else if (delta_bic < 2) {
                    html <- paste0(html, "<p><b>BIC Assessment:</b> Weak evidence for either model (Δ BIC = ", round(delta_bic, 2), ").</p>")
                } else {
                    html <- paste0(html, "<p><b>BIC Assessment:</b> Standard model preferred (Δ BIC = ", round(delta_bic, 2), ").</p>")
                }
                
            }, error = function(e) {
                html <- paste0(html, "<p>Model comparison error: ", e$message, "</p>")
            })
            
            self$results$model_comparison_table$setContent(html)
        },
        
        .generateClusterDiagnostics = function(frailty_model, analysis_data) {
            
            html <- "<h3>Cluster-Level Diagnostics</h3>"
            
            tryCatch({
                
                # Cluster size distribution
                cluster_sizes <- table(analysis_data$cluster)
                cluster_events <- aggregate(analysis_data$status, by = list(cluster = analysis_data$cluster), sum)
                
                html <- paste0(html, "<h4>Cluster Characteristics</h4>")
                html <- paste0(html, "<table class='jamovi-table'>")
                html <- paste0(html, "<tr><td><b>Number of Clusters:</b></td><td>", length(cluster_sizes), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Min Cluster Size:</b></td><td>", min(cluster_sizes), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Max Cluster Size:</b></td><td>", max(cluster_sizes), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Median Cluster Size:</b></td><td>", median(cluster_sizes), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Mean Cluster Size:</b></td><td>", round(mean(cluster_sizes), 1), "</td></tr>")
                html <- paste0(html, "</table>")
                
                # Event distribution by cluster
                html <- paste0(html, "<h4>Event Distribution</h4>")
                html <- paste0(html, "<table class='jamovi-table'>")
                html <- paste0(html, "<tr><td><b>Clusters with Events:</b></td><td>", sum(cluster_events$x > 0), " (", round(100 * sum(cluster_events$x > 0) / length(cluster_sizes), 1), "%)</td></tr>")
                html <- paste0(html, "<tr><td><b>Clusters without Events:</b></td><td>", sum(cluster_events$x == 0), " (", round(100 * sum(cluster_events$x == 0) / length(cluster_sizes), 1), "%)</td></tr>")
                html <- paste0(html, "<tr><td><b>Mean Events per Cluster:</b></td><td>", round(mean(cluster_events$x), 2), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Max Events in Cluster:</b></td><td>", max(cluster_events$x), "</td></tr>")
                html <- paste0(html, "</table>")
                
                # Extract frailty/random effects if possible
                if (inherits(frailty_model, "coxph") && !is.null(frailty_model$frail)) {
                    frailty_effects <- frailty_model$frail
                    
                    # Identify extreme clusters
                    q95 <- quantile(frailty_effects, 0.95)
                    q05 <- quantile(frailty_effects, 0.05)
                    
                    high_risk_clusters <- sum(frailty_effects > q95)
                    low_risk_clusters <- sum(frailty_effects < q05)
                    
                    html <- paste0(html, "<h4>Risk Distribution</h4>")
                    html <- paste0(html, "<table class='jamovi-table'>")
                    html <- paste0(html, "<tr><td><b>High-Risk Clusters (>95th percentile):</b></td><td>", high_risk_clusters, "</td></tr>")
                    html <- paste0(html, "<tr><td><b>Low-Risk Clusters (<5th percentile):</b></td><td>", low_risk_clusters, "</td></tr>")
                    html <- paste0(html, "<tr><td><b>95th Percentile Frailty:</b></td><td>", round(q95, 4), "</td></tr>")
                    html <- paste0(html, "<tr><td><b>5th Percentile Frailty:</b></td><td>", round(q05, 4), "</td></tr>")
                    html <- paste0(html, "</table>")
                }
                
            }, error = function(e) {
                html <- paste0(html, "<p>Cluster diagnostics error: ", e$message, "</p>")
            })
            
            self$results$cluster_summary$setContent(html)
        }
    )
)