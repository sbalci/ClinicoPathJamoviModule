plscoxClass <- R6::R6Class(
    "plscoxClass",
    inherit = plscoxBase,
    private = list(
        .init = function() {
            todo <- glue::glue(
            "
            <br>Welcome to ClinicoPath
            <br><br>
            <b>Partial Least Squares Cox Models</b>
            <br><br>
            This analysis performs PLS-based Cox regression for high-dimensional survival data:
            
            <br><br>
            <b>Analysis Steps:</b>
            <ul>
            <li>1. Select your <b>Time Variable</b> (follow-up time)</li>
            <li>2. Select your <b>Status Variable</b> (event indicator: 1=event, 0=censored)</li>
            <li>3. Select your <b>High-dimensional Predictors</b> (genes, proteins, etc.)</li>
            <li>4. Configure PLS parameters (number of components, algorithm)</li>
            <li>5. Choose cross-validation method for component selection</li>
            <li>6. Select validation and visualization options</li>
            </ul>
            
            <br>
            <b>Key Features:</b>
            <ul>
            <li><b>Dimensionality Reduction:</b> PLS extracts latent components from high-dimensional data</li>
            <li><b>Supervised Learning:</b> Components are optimized for survival prediction</li>
            <li><b>Cross-Validation:</b> Automatic selection of optimal number of components</li>
            <li><b>Variable Importance:</b> Identification of influential predictors</li>
            <li><b>Risk Stratification:</b> Patient grouping based on PLS risk scores</li>
            <li><b>Bootstrap Validation:</b> Assessment of model stability and performance</li>
            </ul>
            
            <br>
            <b>When to Use PLS-Cox:</b>
            <ul>
            <li>High-dimensional data (p >> n): genomics, proteomics, metabolomics</li>
            <li>Multicollinearity among predictors</li>
            <li>Need for interpretable components</li>
            <li>Supervised dimensionality reduction for survival outcomes</li>
            </ul>
            
            <br>
            <b>Interpretation:</b>
            <ul>
            <li><b>PLS Components:</b> Linear combinations of original variables optimized for survival prediction</li>
            <li><b>Variable Loadings:</b> Contribution of each variable to PLS components</li>
            <li><b>Component Scores:</b> Patient-specific values for each PLS component</li>
            <li><b>Hazard Ratios:</b> Effect of each PLS component on hazard of event</li>
            </ul>
            
            <br>
            <b>Required Packages:</b> survival, pls, plsRcox, rms, Hmisc
            <br>
            <b>Recommended minimum:</b> At least 10-20 events for reliable results
            <br><br>
            "
            )
            
            self$results$todo$setContent(todo)
        },

        .run = function() {
            
            # Get options
            time <- self$options$time
            status <- self$options$status  
            predictors <- self$options$predictors
            
            # Check if variables are selected
            if (is.null(time) || is.null(status) || length(predictors) == 0) {
                return()
            }
            
            # Check required packages
            if (!requireNamespace("survival", quietly = TRUE)) {
                self$results$modelSummary$setContent("<h3>Error: Package Not Found</h3><p>The <b>survival</b> package is required but not installed. Please install it using:<br><code>install.packages('survival')</code></p>")
                return()
            }
            
            if (!requireNamespace("pls", quietly = TRUE)) {
                self$results$modelSummary$setContent("<h3>Error: Package Not Found</h3><p>The <b>pls</b> package is required but not installed. Please install it using:<br><code>install.packages('pls')</code></p>")
                return()
            }
            
            if (!requireNamespace("plsRcox", quietly = TRUE)) {
                self$results$modelSummary$setContent("<h3>Error: Package Not Found</h3><p>The <b>plsRcox</b> package is required but not installed. Please install it using:<br><code>install.packages('plsRcox')</code></p>")
                return()
            }
            
            # Get data
            data <- self$data
            
            if (nrow(data) == 0) return()
            
            # Prepare variables
            time_var <- data[[time]]
            status_var <- data[[status]]
            
            # Handle status variable (ensure 0/1 coding)
            if (is.factor(status_var)) {
                status_levels <- levels(status_var)
                if (length(status_levels) == 2) {
                    status_var <- as.numeric(status_var) - 1
                } else {
                    self$results$modelSummary$setContent("<h3>Error: Status Variable</h3><p>Status variable must be binary (0/1 or factor with 2 levels)</p>")
                    return()
                }
            }
            
            # Create predictor matrix
            pred_data <- data[, predictors, drop = FALSE]
            pred_matrix <- as.matrix(pred_data)
            
            # Check for missing values
            complete_cases <- complete.cases(cbind(time_var, status_var, pred_matrix))
            if (sum(complete_cases) < nrow(data)) {
                missing_count <- nrow(data) - sum(complete_cases)
                self$results$modelSummary$setContent(glue::glue("<h3>Warning: Missing Data</h3><p>{missing_count} observations with missing values were excluded from analysis.</p>"))
            }
            
            # Filter complete cases
            time_var <- time_var[complete_cases]
            status_var <- status_var[complete_cases]
            pred_matrix <- pred_matrix[complete_cases, , drop = FALSE]
            
            # Check minimum sample size
            n_events <- sum(status_var)
            if (n_events < 10) {
                self$results$modelSummary$setContent("<h3>Warning: Insufficient Events</h3><p>Less than 10 events observed. Results may be unreliable.</p>")
                return()
            }
            
            # Variable scaling
            scaling_method <- self$options$scaling_method
            if (scaling_method == "standardize") {
                pred_matrix <- scale(pred_matrix)
            } else if (scaling_method == "unit_variance") {
                pred_matrix <- scale(pred_matrix, center = FALSE, scale = TRUE)
            } else if (scaling_method == "minmax") {
                pred_matrix <- apply(pred_matrix, 2, function(x) (x - min(x)) / (max(x) - min(x)))
            }
            
            # Perform PLS Cox regression using plsRcox
            tryCatch({
                
                # Cross-validation setup
                cv_method <- self$options$cross_validation
                if (cv_method == "loo") {
                    cv_folds <- nrow(pred_matrix)
                } else if (cv_method == "k10") {
                    cv_folds <- 10
                } else if (cv_method == "k5") {
                    cv_folds <- 5
                } else {
                    cv_folds <- 1
                }
                
                # Fit PLS Cox model with cross-validation
                max_components <- min(self$options$pls_components, ncol(pred_matrix) - 1, nrow(pred_matrix) - 2)
                
                if (cv_method != "none") {
                    # Cross-validated PLS Cox
                    pls_cv <- plsRcox::cv.plsRcox(
                        Xplan = pred_matrix,
                        time = time_var,
                        event = status_var,
                        nt = max_components,
                        K = cv_folds,
                        random = TRUE
                    )
                    
                    # Select optimal number of components
                    optimal_nt <- pls_cv$nt.opt
                    
                    # Fit final model with optimal components
                    pls_model <- plsRcox::plsRcox(
                        Xplan = pred_matrix,
                        time = time_var,
                        event = status_var,
                        nt = optimal_nt,
                        alpha.pvals.Fisher = 0.05
                    )
                    
                    # Store CV results
                    cv_results <- data.frame(
                        n_components = 1:length(pls_cv$cv.error10),
                        cv_score = pls_cv$cv.error10,
                        se_cv_score = pls_cv$cv.se10,
                        c_index = NA,
                        selected = ifelse(1:length(pls_cv$cv.error10) == optimal_nt, "Yes", "No")
                    )
                    
                    self$results$componentSelection$setData(cv_results)
                    
                } else {
                    # No cross-validation
                    pls_model <- plsRcox::plsRcox(
                        Xplan = pred_matrix,
                        time = time_var,
                        event = status_var,
                        nt = max_components,
                        alpha.pvals.Fisher = 0.05
                    )
                    optimal_nt <- max_components
                }
                
                # Extract PLS components
                pls_scores <- pls_model$tt
                colnames(pls_scores) <- paste0("PLS_", 1:ncol(pls_scores))
                
                # Fit Cox model on PLS components
                cox_data <- data.frame(
                    time = time_var,
                    status = status_var,
                    pls_scores
                )
                
                cox_formula <- as.formula(paste("survival::Surv(time, status) ~", 
                                               paste(colnames(pls_scores), collapse = " + ")))
                
                cox_model <- survival::coxph(cox_formula, data = cox_data)
                
                # Model summary
                summary_text <- glue::glue("
                <h3>PLS Cox Model Results</h3>
                <p><b>Analysis Summary:</b></p>
                <ul>
                <li>Sample size: {nrow(pred_matrix)} subjects</li>
                <li>Number of events: {n_events}</li>
                <li>Number of predictors: {ncol(pred_matrix)}</li>
                <li>PLS components used: {optimal_nt}</li>
                <li>Scaling method: {scaling_method}</li>
                <li>Cross-validation: {cv_method}</li>
                </ul>
                
                <p><b>Model Performance:</b></p>
                <ul>
                <li>Concordance index: {round(summary(cox_model)$concordance[1], 3)} (SE: {round(summary(cox_model)$concordance[2], 3)})</li>
                <li>Likelihood ratio test: {round(summary(cox_model)$logtest[1], 2)} (p = {round(summary(cox_model)$logtest[3], 4)})</li>
                <li>Wald test: {round(summary(cox_model)$waldtest[1], 2)} (p = {round(summary(cox_model)$waldtest[3], 4)})</li>
                </ul>
                ")
                
                self$results$modelSummary$setContent(summary_text)
                
                # Model coefficients table
                cox_summary <- summary(cox_model)
                coef_table <- data.frame(
                    component = paste("PLS Component", 1:optimal_nt),
                    coefficient = cox_summary$coefficients[, "coef"],
                    hr = cox_summary$coefficients[, "exp(coef)"],
                    hr_lower = cox_summary$conf.int[, "lower .95"],
                    hr_upper = cox_summary$conf.int[, "upper .95"],
                    se = cox_summary$coefficients[, "se(coef)"],
                    z_value = cox_summary$coefficients[, "z"],
                    p_value = cox_summary$coefficients[, "Pr(>|z|)"]
                )
                
                self$results$modelCoefficients$setData(coef_table)
                
                # Variable loadings and importance
                if (self$options$feature_importance) {
                    loadings_matrix <- pls_model$wwetoile
                    
                    # Calculate variable importance scores
                    importance_scores <- apply(abs(loadings_matrix), 1, function(x) sqrt(sum(x^2)))
                    
                    loadings_table <- data.frame(
                        variable = rownames(loadings_matrix),
                        component_1 = if(ncol(loadings_matrix) >= 1) loadings_matrix[, 1] else NA,
                        component_2 = if(ncol(loadings_matrix) >= 2) loadings_matrix[, 2] else NA,
                        component_3 = if(ncol(loadings_matrix) >= 3) loadings_matrix[, 3] else NA,
                        importance_score = importance_scores
                    )
                    
                    # Sort by importance
                    loadings_table <- loadings_table[order(-loadings_table$importance_score), ]
                    
                    self$results$variableLoadings$setData(loadings_table)
                }
                
                # Model performance metrics
                if (self$options$prediction_accuracy) {
                    
                    # Calculate additional performance metrics
                    linear_pred <- predict(cox_model, type = "lp")
                    
                    # C-index with confidence interval
                    c_index <- summary(cox_model)$concordance[1]
                    c_index_se <- summary(cox_model)$concordance[2]
                    
                    performance_table <- data.frame(
                        metric = c("Concordance Index", "R-squared (Nagelkerke)", "AIC", "BIC"),
                        value = c(
                            c_index,
                            summary(cox_model)$rsq["rsq"],
                            extractAIC(cox_model)[2],
                            extractAIC(cox_model)[2] + (extractAIC(cox_model)[1] * (log(nrow(cox_data)) - 2))
                        ),
                        se = c(c_index_se, NA, NA, NA),
                        lower_ci = c(c_index - 1.96 * c_index_se, NA, NA, NA),
                        upper_ci = c(c_index + 1.96 * c_index_se, NA, NA, NA)
                    )
                    
                    self$results$modelPerformance$setData(performance_table)
                }
                
                # Risk stratification
                risk_groups <- self$options$risk_groups
                linear_pred <- predict(cox_model, type = "lp")
                risk_quantiles <- quantile(linear_pred, probs = seq(0, 1, length.out = risk_groups + 1))
                risk_categories <- cut(linear_pred, breaks = risk_quantiles, 
                                     labels = paste("Risk Group", 1:risk_groups), include.lowest = TRUE)
                
                # Risk group analysis
                risk_data <- data.frame(
                    time = time_var,
                    status = status_var,
                    risk_group = risk_categories
                )
                
                # Survival analysis by risk group
                risk_fit <- survival::survfit(survival::Surv(time, status) ~ risk_group, data = risk_data)
                
                # Risk stratification table
                risk_summary <- data.frame(
                    risk_group = paste("Risk Group", 1:risk_groups),
                    n_subjects = as.vector(table(risk_categories)),
                    n_events = as.vector(by(risk_data$status, risk_data$risk_group, sum)),
                    median_survival = summary(risk_fit)$table[, "median"],
                    survival_se = summary(risk_fit)$table[, "se(median)"],
                    hr_vs_low = c(1.0, rep(NA, risk_groups - 1)),  # Will calculate properly
                    hr_p_value = c(NA, rep(NA, risk_groups - 1))   # Will calculate properly
                )
                
                # Calculate hazard ratios vs lowest risk group
                if (risk_groups > 1) {
                    for (i in 2:risk_groups) {
                        subset_data <- risk_data[risk_data$risk_group %in% paste("Risk Group", c(1, i)), ]
                        subset_data$risk_group <- droplevels(subset_data$risk_group)
                        subset_cox <- survival::coxph(survival::Surv(time, status) ~ risk_group, data = subset_data)
                        risk_summary$hr_vs_low[i] <- exp(coef(subset_cox))
                        risk_summary$hr_p_value[i] <- summary(subset_cox)$coefficients[, "Pr(>|z|)"]
                    }
                }
                
                self$results$riskStratification$setData(risk_summary)
                
                # Store results for plotting
                private$plsResults <- list(
                    pls_model = pls_model,
                    cox_model = cox_model,
                    pred_matrix = pred_matrix,
                    pls_scores = pls_scores,
                    time_var = time_var,
                    status_var = status_var,
                    risk_categories = risk_categories,
                    optimal_nt = optimal_nt,
                    cv_results = if(exists("cv_results")) cv_results else NULL
                )
                
                # Clinical guidance
                guidance_text <- "
                <h3>Clinical Interpretation Guide</h3>
                
                <h4>PLS Components</h4>
                <p>PLS components represent linear combinations of your original predictors that are optimally related to survival outcomes. Each component captures a different aspect of the biological variation in your data.</p>
                
                <h4>Variable Loadings</h4>
                <p>Loadings indicate how much each original variable contributes to each PLS component. Variables with higher absolute loadings have stronger influence on that component.</p>
                
                <h4>Hazard Ratios</h4>
                <p>Each PLS component's hazard ratio indicates the relative risk associated with a one-unit increase in that component score. HR > 1 indicates increased risk, HR < 1 indicates decreased risk.</p>
                
                <h4>Risk Stratification</h4>
                <p>Patients are grouped based on their overall PLS risk score. Higher risk groups should show shorter survival times and more events.</p>
                
                <h4>Model Validation</h4>
                <p>Cross-validation helps select the optimal number of components. Bootstrap validation can assess model stability and provide confidence intervals for performance metrics.</p>
                
                <h4>Clinical Application</h4>
                <p>This model can be used for:</p>
                <ul>
                <li>Risk stratification of patients</li>
                <li>Identification of prognostic biomarker signatures</li>
                <li>Treatment decision support</li>
                <li>Clinical trial stratification</li>
                </ul>
                "
                
                self$results$clinicalGuidance$setContent(guidance_text)
                
                # Technical notes
                technical_text <- "
                <h3>Technical Notes and Assumptions</h3>
                
                <h4>PLS Cox Methodology</h4>
                <p>This analysis combines Partial Least Squares (PLS) dimensionality reduction with Cox proportional hazards regression. PLS finds components that maximize covariance between predictors and the survival outcome.</p>
                
                <h4>Model Assumptions</h4>
                <ul>
                <li><b>Proportional Hazards:</b> The hazard ratio for each component is constant over time</li>
                <li><b>Linear Relationships:</b> Log-hazard is linear in the PLS components</li>
                <li><b>Independence:</b> Observations are independent</li>
                <li><b>Non-informative Censoring:</b> Censoring is independent of the event process</li>
                </ul>
                
                <h4>Component Selection</h4>
                <p>Cross-validation is used to select the optimal number of PLS components to avoid overfitting while maintaining predictive performance.</p>
                
                <h4>Variable Scaling</h4>
                <p>Predictor variables should be scaled when they have different units or vastly different ranges to ensure fair contribution to PLS components.</p>
                
                <h4>Sample Size Considerations</h4>
                <p>For reliable results, aim for at least 10-15 events per PLS component included in the model. With high-dimensional data, cross-validation becomes crucial.</p>
                
                <h4>Interpretation Cautions</h4>
                <ul>
                <li>PLS components are linear combinations - biological interpretation may be complex</li>
                <li>Variable importance should be interpreted in context of component loadings</li>
                <li>External validation is recommended before clinical application</li>
                </ul>
                "
                
                self$results$technicalNotes$setContent(technical_text)
                
                # Bootstrap validation
                if (self$options$bootstrap_validation) {
                    n_bootstrap <- self$options$n_bootstrap
                    
                    bootstrap_c_indices <- numeric(n_bootstrap)
                    valid_bootstraps <- 0
                    
                    # Progress bar or messaging if possible, but for now just loop
                    for (i in 1:n_bootstrap) {
                        tryCatch({
                            # Resample
                            boot_idx <- sample(nrow(pred_matrix), replace = TRUE)
                            boot_X <- pred_matrix[boot_idx, , drop = FALSE]
                            boot_time <- time_var[boot_idx]
                            boot_status <- status_var[boot_idx]
                            
                            # Fit model on bootstrap sample (using optimal_nt from main analysis)
                            # Note: Re-running CV would be better but too slow.
                            # We assess stability of the chosen model configuration.
                            if (sum(boot_status) > 5) { # Ensure enough events
                                boot_pls <- plsRcox::plsRcox(
                                    Xplan = boot_X,
                                    time = boot_time,
                                    event = boot_status,
                                    nt = optimal_nt,
                                    alpha.pvals.Fisher = 0.05,
                                    verbose = FALSE
                                )
                                
                                # Calculate C-index
                                # Need scores for bootstrap sample
                                boot_scores <- boot_pls$tt
                                if (!is.null(boot_scores)) {
                                    boot_cox_data <- data.frame(
                                        time = boot_time,
                                        status = boot_status,
                                        boot_scores
                                    )
                                    colnames(boot_cox_data)[3:ncol(boot_cox_data)] <- paste0("Comp", 1:ncol(boot_scores))
                                    
                                    boot_formula <- as.formula(paste("survival::Surv(time, status) ~", 
                                                                   paste(colnames(boot_cox_data)[3:ncol(boot_cox_data)], collapse = " + ")))
                                    
                                    boot_c <- survival::concordance(boot_formula, data = boot_cox_data)$concordance
                                    valid_bootstraps <- valid_bootstraps + 1
                                    bootstrap_c_indices[valid_bootstraps] <- boot_c
                                }
                            }
                        }, error = function(e) {
                            # Skip failed bootstraps
                        })
                    }
                    
                    # Trim to valid results
                    bootstrap_c_indices <- bootstrap_c_indices[1:valid_bootstraps]
                    
                    if (valid_bootstraps > 0) {
                        mean_boot_c <- mean(bootstrap_c_indices)
                        sd_boot_c <- sd(bootstrap_c_indices)
                        ci_lower <- quantile(bootstrap_c_indices, 0.025)
                        ci_upper <- quantile(bootstrap_c_indices, 0.975)
                        
                        bootstrap_text <- glue::glue("
                        <h3>Bootstrap Validation Results</h3>
                        <p><b>Bootstrap validation with {valid_bootstraps} successful replications</b> (Target: {n_bootstrap})</p>
                        <ul>
                        <li><b>Mean Bootstrap C-Index:</b> {round(mean_boot_c, 3)}</li>
                        <li><b>Standard Error:</b> {round(sd_boot_c, 3)}</li>
                        <li><b>95% Confidence Interval:</b> {round(ci_lower, 3)} - {round(ci_upper, 3)}</li>
                        </ul>
                        <p><i>Note:</i> Confidence intervals reflect the variability of the model performance on resampled data (internal validity).</p>
                        ")
                    } else {
                        bootstrap_text <- "<h3>Bootstrap Validation Failed</h3><p>Could not generate valid bootstrap models (possibly due to small sample size or convergence issues).</p>"
                    }
                    
                    self$results$bootstrapResults$setContent(bootstrap_text)
                }
                
                # Permutation test
                if (self$options$permutation_test) {
                    n_permutations <- self$options$n_permutations
                    
                    # Original C-index
                    original_c <- summary(cox_model)$concordance[1]
                    
                    perm_c_indices <- numeric(n_permutations)
                    valid_perms <- 0
                    
                    for (i in 1:n_permutations) {
                        tryCatch({
                            # Permute outcome
                            perm_idx <- sample(nrow(pred_matrix))
                            perm_time <- time_var[perm_idx]
                            perm_status <- status_var[perm_idx]
                            
                            # Fit PLS on permuted data
                            perm_pls <- plsRcox::plsRcox(
                                Xplan = pred_matrix, # Predictors fixed
                                time = perm_time,
                                event = perm_status,
                                nt = optimal_nt,
                                alpha.pvals.Fisher = 0.05,
                                verbose = FALSE
                            )
                            
                            # Calculate C-index
                            perm_scores <- perm_pls$tt
                            if (!is.null(perm_scores)) {
                                perm_cox_data <- data.frame(
                                    time = perm_time,
                                    status = perm_status,
                                    perm_scores
                                )
                                colnames(perm_cox_data)[3:ncol(perm_cox_data)] <- paste0("Comp", 1:ncol(perm_scores))
                                
                                perm_formula <- as.formula(paste("survival::Surv(time, status) ~", 
                                                               paste(colnames(perm_cox_data)[3:ncol(perm_cox_data)], collapse = " + ")))
                                
                                perm_c <- survival::concordance(perm_formula, data = perm_cox_data)$concordance
                                valid_perms <- valid_perms + 1
                                perm_c_indices[valid_perms] <- perm_c
                            }
                        }, error = function(e) {
                            # Skip failed permutations
                        })
                    }
                    
                    perm_c_indices <- perm_c_indices[1:valid_perms]
                    
                    if (valid_perms > 0) {
                        # Calculate p-value (proportion of permuted C >= original C)
                        # Note: C-index < 0.5 is also "predictive" (inverse), so we test abs(C - 0.5) for 2-sided equivalent?
                        # Usually we want C > 0.5.
                        n_better <- sum(perm_c_indices >= original_c)
                        p_val <- (n_better + 1) / (valid_perms + 1)
                        
                        permutation_text <- glue::glue("
                        <h3>Permutation Test Results</h3>
                        <p><b>Permutation testing with {valid_perms} successful permutations</b> (Target: {n_permutations})</p>
                        <ul>
                        <li><b>Original C-Index:</b> {round(original_c, 3)}</li>
                        <li><b>Mean Permuted C-Index:</b> {round(mean(perm_c_indices), 3)} (expected ~0.5)</li>
                        <li><b>P-value:</b> {round(p_val, 4)}</li>
                        </ul>
                        <p><i>Interpretation:</i> The p-value represents the probability of observing a C-index as high as {round(original_c, 3)} by chance alone (if predictors had no relationship to survival).</p>
                        ")
                    } else {
                        permutation_text <- "<h3>Permutation Test Failed</h3><p>Could not generate valid permutation models.</p>"
                    }
                    
                    self$results$permutationResults$setContent(permutation_text)
                }
                
            }, error = function(e) {
                error_msg <- glue::glue("<h3>Error in PLS Cox Analysis</h3><p>An error occurred during analysis: {e$message}</p><p>Please check your data and parameter settings.</p>")
                self$results$modelSummary$setContent(error_msg)
                return()
            })
        },

        # Plotting functions
        .plotComponents = function(image, ...) {
            if (!exists("plsResults", envir = private) || is.null(private$plsResults)) return()
            
            pls_model <- private$plsResults$pls_model
            
            # Component variance plot
            variance_explained <- pls_model$R2
            
            plot_data <- data.frame(
                Component = 1:length(variance_explained),
                Variance = variance_explained * 100
            )
            
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Component, y = Variance)) +
                ggplot2::geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
                ggplot2::geom_line(color = "red", size = 1) +
                ggplot2::geom_point(color = "red", size = 2) +
                ggplot2::labs(
                    title = "PLS Component Variance Explained",
                    subtitle = "Cumulative variance explained by each component",
                    x = "PLS Component",
                    y = "Cumulative Variance Explained (%)"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                    plot.subtitle = ggplot2::element_text(hjust = 0.5)
                )
            
            print(p)
            return(TRUE)
        },

        .plotLoadings = function(image, ...) {
            if (!exists("plsResults", envir = private) || is.null(private$plsResults)) return()
            if (!self$options$feature_importance) return()
            
            pls_model <- private$plsResults$pls_model
            loadings_matrix <- pls_model$wwetoile
            
            # Plot loadings for first two components
            if (ncol(loadings_matrix) >= 2) {
                plot_data <- data.frame(
                    Variable = rownames(loadings_matrix),
                    PC1 = loadings_matrix[, 1],
                    PC2 = loadings_matrix[, 2]
                )
                
                # Color by importance
                importance_scores <- sqrt(plot_data$PC1^2 + plot_data$PC2^2)
                plot_data$Importance <- importance_scores
                
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PC1, y = PC2, color = Importance)) +
                    ggplot2::geom_point(alpha = 0.7, size = 2) +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
                    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
                    ggplot2::scale_color_gradient(low = "lightblue", high = "darkred") +
                    ggplot2::labs(
                        title = "PLS Variable Loadings",
                        subtitle = "Variable contributions to first two PLS components",
                        x = "PLS Component 1",
                        y = "PLS Component 2"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5)
                    )
                
                # Add variable labels for top contributors
                top_vars <- plot_data[order(-plot_data$Importance)[1:min(10, nrow(plot_data))], ]
                p <- p + ggrepel::geom_text_repel(
                    data = top_vars,
                    ggplot2::aes(label = Variable),
                    size = 3,
                    alpha = 0.8
                )
                
                print(p)
            }
            
            return(TRUE)
        },

        .plotScores = function(image, ...) {
            if (!exists("plsResults", envir = private) || is.null(private$plsResults)) return()
            
            pls_scores <- private$plsResults$pls_scores
            time_var <- private$plsResults$time_var
            status_var <- private$plsResults$status_var
            
            # Plot first component scores vs survival
            if (ncol(pls_scores) >= 1) {
                plot_data <- data.frame(
                    PLS_Score = pls_scores[, 1],
                    Time = time_var,
                    Status = factor(status_var, levels = c(0, 1), labels = c("Censored", "Event"))
                )
                
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PLS_Score, y = Time, color = Status)) +
                    ggplot2::geom_point(alpha = 0.6, size = 2) +
                    ggplot2::scale_color_manual(values = c("Censored" = "blue", "Event" = "red")) +
                    ggplot2::labs(
                        title = "PLS Component 1 Scores vs Survival Time",
                        subtitle = "Relationship between first PLS component and survival outcomes",
                        x = "PLS Component 1 Score",
                        y = "Survival Time"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5)
                    )
                
                print(p)
            }
            
            return(TRUE)
        },

        .plotValidation = function(image, ...) {
            if (!exists("plsResults", envir = private) || is.null(private$plsResults)) return()
            if (is.null(private$plsResults$cv_results)) return()
            
            cv_results <- private$plsResults$cv_results
            
            p <- ggplot2::ggplot(cv_results, ggplot2::aes(x = n_components, y = cv_score)) +
                ggplot2::geom_line(color = "blue", size = 1) +
                ggplot2::geom_point(color = "blue", size = 2) +
                ggplot2::geom_errorbar(
                    ggplot2::aes(ymin = cv_score - se_cv_score, ymax = cv_score + se_cv_score),
                    width = 0.1, alpha = 0.5
                ) +
                ggplot2::geom_vline(
                    xintercept = which(cv_results$selected == "Yes"),
                    linetype = "dashed", color = "red", alpha = 0.7
                ) +
                ggplot2::labs(
                    title = "Cross-Validation Results",
                    subtitle = "Selection of optimal number of PLS components",
                    x = "Number of PLS Components",
                    y = "Cross-Validation Error"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                    plot.subtitle = ggplot2::element_text(hjust = 0.5)
                )
            
            print(p)
            return(TRUE)
        },

        .plotSurvival = function(image, ...) {
            if (!exists("plsResults", envir = private) || is.null(private$plsResults)) return()
            
            time_var <- private$plsResults$time_var
            status_var <- private$plsResults$status_var
            risk_categories <- private$plsResults$risk_categories
            
            # Create survival object
            surv_obj <- survival::Surv(time_var, status_var)
            
            # Fit survival curves by risk group
            risk_fit <- survival::survfit(surv_obj ~ risk_categories)
            
            # Create survival plot
            risk_data <- data.frame(
                time = time_var,
                status = status_var,
                risk_group = risk_categories
            )
            
            # Use survminer if available, otherwise base plot
            if (requireNamespace("survminer", quietly = TRUE)) {
                p <- survminer::ggsurvplot(
                    risk_fit,
                    data = risk_data,
                    risk.table = TRUE,
                    pval = TRUE,
                    conf.int = TRUE,
                    title = "Risk-Stratified Survival Curves",
                    subtitle = "Survival curves by PLS-based risk groups",
                    xlab = "Time",
                    ylab = "Survival Probability",
                    legend.title = "Risk Group"
                )
                print(p$plot)
            } else {
                plot(risk_fit, col = 1:length(unique(risk_categories)),
                     main = "Risk-Stratified Survival Curves",
                     xlab = "Time", ylab = "Survival Probability")
                legend("topright", legend = paste("Risk Group", 1:length(unique(risk_categories))),
                       col = 1:length(unique(risk_categories)), lty = 1)
            }
            
            return(TRUE)
        },

        plsResults = NULL
    )
)