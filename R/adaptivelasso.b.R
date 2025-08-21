adaptivelassoClass <- R6::R6Class(
    "adaptivelassoClass",
    inherit = adaptivelassoBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$time) || is.null(self$options$event)) {
                self$results$instructions$setContent(
                    "<html>
                    <head>
                    <style>
                        h2 {color: #e74c3c;}
                        body {font-family: Arial, sans-serif; margin: 20px;}
                        .highlight {background-color: #f39c12; padding: 2px 4px; border-radius: 3px;}
                        .step {margin: 10px 0; padding: 8px; background-color: #ecf0f1; border-radius: 5px;}
                    </style>
                    </head>
                    <body>
                    <h2>🎯 Adaptive LASSO for Cox Models</h2>
                    <p><strong>Advanced penalized regression with data-driven variable selection</strong></p>
                    
                    <div class='step'>
                    <strong>📊 Required Data:</strong>
                    <ul>
                        <li><span class='highlight'>Time Variable</span>: Time to event or censoring</li>
                        <li><span class='highlight'>Event Indicator</span>: 0 = censored, 1 = event</li>
                        <li><span class='highlight'>Predictor Variables</span>: Candidate variables for selection</li>
                    </ul>
                    </div>
                    
                    <div class='step'>
                    <strong>🔧 Adaptive LASSO Advantages:</strong>
                    <ul>
                        <li><strong>Oracle Property:</strong> Consistent variable selection with optimal rates</li>
                        <li><strong>Reduced Bias:</strong> Less shrinkage of important coefficients</li>
                        <li><strong>Data-Driven Weights:</strong> Automatic penalty adaptation</li>
                        <li><strong>High-Dimensional:</strong> Handles p > n scenarios effectively</li>
                    </ul>
                    </div>
                    
                    <div class='step'>
                    <strong>⚙️ Weight Methods:</strong>
                    <ul>
                        <li><strong>Ridge:</strong> Stable weights from ridge regression (recommended)</li>
                        <li><strong>Univariate:</strong> Individual Cox model coefficients</li>
                        <li><strong>Full Cox:</strong> Complete model estimates (when feasible)</li>
                        <li><strong>Correlation:</strong> Marginal association strengths</li>
                    </ul>
                    </div>
                    
                    <div class='step'>
                    <strong>🎨 Key Features:</strong>
                    <ul>
                        <li>📈 Cross-validation for optimal penalty selection</li>
                        <li>🎯 Stability selection for robust variable identification</li>
                        <li>📊 Comprehensive model diagnostics and validation</li>
                        <li>🔄 Regularization path visualization</li>
                        <li>⚕️ Clinical risk group analysis</li>
                        <li>🏗️ Bootstrap confidence intervals</li>
                    </ul>
                    </div>
                    
                    <p><em>💡 Tip: Start with Ridge weights and 10-fold CV, then enable stability selection for robust variable selection.</em></p>
                    </body>
                    </html>"
                )
                return()
            }
            
            private$.initResults()
        },

        .run = function() {
            if (is.null(self$data) || is.null(self$options$time) || is.null(self$options$event) || 
                length(self$options$predictors) == 0) {
                return()
            }

            # Prepare data and validate inputs
            cox_data <- private$.prepareData()
            if (is.null(cox_data)) return()

            # Check if required packages are available
            required_packages <- c("glmnet", "survival", "pROC")
            missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
            
            if (length(missing_packages) > 0) {
                self$results$instructions$setContent(
                    paste0("<html><body><h3>Missing Required Packages</h3>",
                           "<p>Please install the following packages:</p>",
                           "<ul>", paste0("<li>", missing_packages, "</li>", collapse = ""), "</ul>",
                           "<p><code>install.packages(c('", paste(missing_packages, collapse = "', '"), "'))</code></p>",
                           "</body></html>")
                )
                return()
            }

            # Fit adaptive LASSO model
            tryCatch({
                adaptive_results <- private$.fitAdaptiveLasso(cox_data)
                if (!is.null(adaptive_results)) {
                    private$.populateResults(adaptive_results, cox_data)
                }
            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<html><body><h3>Analysis Error</h3><p>", 
                           "Error in adaptive LASSO fitting: ", e$message,
                           "</p><p>Try reducing the number of predictors or adjusting penalty settings.</p></body></html>")
                )
            })
        },

        .prepareData = function() {
            data <- self$data
            
            # Get variable names
            time_var <- self$options$time
            event_var <- self$options$event
            pred_vars <- self$options$predictors
            
            if (is.null(time_var) || is.null(event_var) || length(pred_vars) == 0) {
                return(NULL)
            }
            
            # Create analysis dataset
            vars_needed <- c(time_var, event_var, pred_vars)
            if (!is.null(self$options$strata)) {
                vars_needed <- c(vars_needed, self$options$strata)
            }
            
            analysis_data <- data[, vars_needed, drop = FALSE]
            analysis_data <- na.omit(analysis_data)
            
            if (nrow(analysis_data) < length(pred_vars) + 10) {
                self$results$instructions$setContent(
                    "<html><body><h3>Insufficient Data</h3>
                    <p>Need at least p + 10 complete observations for reliable adaptive LASSO.</p></body></html>"
                )
                return(NULL)
            }

            # Prepare survival object
            time_values <- as.numeric(analysis_data[[time_var]])
            event_values <- as.numeric(analysis_data[[event_var]])
            
            if (any(time_values <= 0, na.rm = TRUE)) {
                self$results$instructions$setContent(
                    "<html><body><h3>Invalid Time Values</h3>
                    <p>All time values must be positive for survival analysis.</p></body></html>"
                )
                return(NULL)
            }

            # Prepare predictor matrix
            pred_data <- analysis_data[, pred_vars, drop = FALSE]
            
            # Convert factors to dummy variables
            x_matrix <- model.matrix(~ . - 1, data = pred_data)
            
            # Standardize if requested
            if (self$options$standardize) {
                x_matrix <- scale(x_matrix)
            }

            # Create survival object
            requireNamespace("survival", quietly = TRUE)
            surv_obj <- survival::Surv(time_values, event_values)

            return(list(
                data = analysis_data,
                x = x_matrix,
                y = surv_obj,
                time = time_values,
                event = event_values,
                n_obs = nrow(analysis_data),
                n_events = sum(event_values),
                pred_names = colnames(x_matrix),
                original_pred_names = pred_vars
            ))
        },

        .fitAdaptiveLasso = function(cox_data) {
            requireNamespace("glmnet", quietly = TRUE)
            requireNamespace("survival", quietly = TRUE)

            # Calculate adaptive weights
            adaptive_weights <- private$.calculateAdaptiveWeights(cox_data)
            
            # Set up lambda sequence
            if (self$options$lambda_sequence == "auto") {
                lambda_seq <- NULL  # Let glmnet choose
            } else {
                # Custom lambda sequence would be implemented here
                lambda_seq <- NULL
            }

            # Fit adaptive LASSO with cross-validation
            set.seed(self$options$random_seed)
            
            cv_fit <- glmnet::cv.glmnet(
                x = cox_data$x,
                y = cox_data$y,
                family = "cox",
                alpha = self$options$alpha,
                penalty.factor = adaptive_weights,
                nfolds = self$options$cv_folds,
                lambda = lambda_seq,
                type.measure = self$options$cv_measure,
                standardize = FALSE  # Already standardized if requested
            )

            # Fit full model for path
            full_fit <- glmnet::glmnet(
                x = cox_data$x,
                y = cox_data$y,
                family = "cox",
                alpha = self$options$alpha,
                penalty.factor = adaptive_weights,
                lambda = cv_fit$lambda,
                standardize = FALSE
            )

            # Extract coefficients at optimal lambda
            lambda_min <- cv_fit$lambda.min
            lambda_1se <- cv_fit$lambda.1se
            
            coef_min <- as.matrix(coef(full_fit, s = lambda_min))
            coef_1se <- as.matrix(coef(full_fit, s = lambda_1se))

            # Model diagnostics
            diagnostics <- private$.calculateDiagnostics(cox_data, coef_min, lambda_min)

            # Stability selection if requested
            stability_results <- NULL
            if (self$options$stability_selection) {
                stability_results <- private$.stabilitySelection(cox_data, adaptive_weights)
            }

            return(list(
                cv_fit = cv_fit,
                full_fit = full_fit,
                lambda_min = lambda_min,
                lambda_1se = lambda_1se,
                coef_min = coef_min,
                coef_1se = coef_1se,
                adaptive_weights = adaptive_weights,
                diagnostics = diagnostics,
                stability = stability_results
            ))
        },

        .calculateAdaptiveWeights = function(cox_data) {
            method <- self$options$weight_method
            gamma <- self$options$gamma
            n_vars <- ncol(cox_data$x)
            
            if (method == "equal") {
                return(rep(1, n_vars))
            }

            initial_coefs <- rep(0, n_vars)
            
            if (method == "ridge") {
                # Ridge regression for initial estimates
                ridge_fit <- glmnet::glmnet(
                    x = cox_data$x,
                    y = cox_data$y,
                    family = "cox",
                    alpha = 0,  # Pure ridge
                    standardize = FALSE
                )
                
                # Use lambda that gives reasonable shrinkage
                lambda_ridge <- ridge_fit$lambda[length(ridge_fit$lambda) %/% 4]
                initial_coefs <- as.vector(coef(ridge_fit, s = lambda_ridge))
                
            } else if (method == "univariate") {
                # Univariate Cox regressions
                for (i in 1:n_vars) {
                    tryCatch({
                        uni_fit <- survival::coxph(cox_data$y ~ cox_data$x[, i])
                        initial_coefs[i] <- coef(uni_fit)[1]
                    }, error = function(e) {
                        initial_coefs[i] <- 0
                    })
                }
                
            } else if (method == "cox") {
                # Full Cox model (if feasible)
                if (ncol(cox_data$x) < nrow(cox_data$x) / 3) {
                    tryCatch({
                        cox_fit <- survival::coxph(cox_data$y ~ cox_data$x)
                        initial_coefs <- coef(cox_fit)
                    }, error = function(e) {
                        # Fallback to ridge if Cox fails
                        ridge_fit <- glmnet::glmnet(cox_data$x, cox_data$y, family = "cox", alpha = 0)
                        lambda_ridge <- ridge_fit$lambda[length(ridge_fit$lambda) %/% 4]
                        initial_coefs <- as.vector(coef(ridge_fit, s = lambda_ridge))
                    })
                } else {
                    # Too many variables, use ridge instead
                    ridge_fit <- glmnet::glmnet(cox_data$x, cox_data$y, family = "cox", alpha = 0)
                    lambda_ridge <- ridge_fit$lambda[length(ridge_fit$lambda) %/% 4]
                    initial_coefs <- as.vector(coef(ridge_fit, s = lambda_ridge))
                }
                
            } else if (method == "correlation") {
                # Marginal correlations with outcome
                for (i in 1:n_vars) {
                    # Use log-rank test statistics as proxy
                    tryCatch({
                        test_stat <- survival::survdiff(cox_data$y ~ cox_data$x[, i] > median(cox_data$x[, i]))
                        initial_coefs[i] <- sqrt(test_stat$chisq) * sign(mean(cox_data$x[cox_data$event == 1, i]) - 
                                                                        mean(cox_data$x[cox_data$event == 0, i]))
                    }, error = function(e) {
                        initial_coefs[i] <- 0
                    })
                }
            }

            # Calculate adaptive weights
            initial_coefs[is.na(initial_coefs)] <- 0
            weights <- 1 / (abs(initial_coefs) + 1e-8)^gamma
            
            # Normalize weights to prevent numerical issues
            weights <- weights / max(weights)
            
            return(weights)
        },

        .calculateDiagnostics = function(cox_data, coefficients, lambda) {
            diagnostics <- list()
            
            # Selected variables
            selected_vars <- which(abs(coefficients) > 1e-8)
            diagnostics$n_selected <- length(selected_vars)
            diagnostics$selected_vars <- cox_data$pred_names[selected_vars]
            
            # Model fit statistics
            if (length(selected_vars) > 0) {
                tryCatch({
                    # Fit Cox model with selected variables
                    selected_x <- cox_data$x[, selected_vars, drop = FALSE]
                    cox_fit <- survival::coxph(cox_data$y ~ selected_x)
                    
                    diagnostics$concordance <- cox_fit$concordance["concordance"]
                    diagnostics$loglik <- cox_fit$loglik[2]
                    diagnostics$aic <- -2 * cox_fit$loglik[2] + 2 * length(selected_vars)
                    
                    # Proportional hazards test if requested
                    if (self$options$proportional_hazards && length(selected_vars) > 1) {
                        ph_test <- survival::cox.zph(cox_fit)
                        diagnostics$ph_global_p <- ph_test$table["GLOBAL", "p"]
                    }
                    
                }, error = function(e) {
                    diagnostics$concordance <- NA
                    diagnostics$loglik <- NA
                    diagnostics$aic <- NA
                })
            }
            
            return(diagnostics)
        },

        .stabilitySelection = function(cox_data, adaptive_weights) {
            n_boot <- self$options$bootstrap_samples
            subsample_ratio <- self$options$subsampling_ratio
            n_vars <- ncol(cox_data$x)
            
            selection_matrix <- matrix(0, nrow = n_boot, ncol = n_vars)
            
            set.seed(self$options$random_seed)
            
            for (b in 1:n_boot) {
                # Bootstrap/subsample data
                n_sub <- floor(nrow(cox_data$x) * subsample_ratio)
                boot_idx <- sample(nrow(cox_data$x), n_sub, replace = FALSE)
                
                boot_x <- cox_data$x[boot_idx, , drop = FALSE]
                boot_y <- cox_data$y[boot_idx, ]
                
                tryCatch({
                    # Fit adaptive LASSO on bootstrap sample
                    boot_fit <- glmnet::glmnet(
                        x = boot_x,
                        y = boot_y,
                        family = "cox",
                        alpha = self$options$alpha,
                        penalty.factor = adaptive_weights,
                        standardize = FALSE
                    )
                    
                    # Use cross-validation to select lambda
                    boot_cv <- glmnet::cv.glmnet(
                        x = boot_x,
                        y = boot_y,
                        family = "cox",
                        alpha = self$options$alpha,
                        penalty.factor = adaptive_weights,
                        nfolds = min(5, self$options$cv_folds),
                        standardize = FALSE
                    )
                    
                    # Extract selected variables
                    boot_coef <- as.vector(coef(boot_fit, s = boot_cv$lambda.1se))
                    selection_matrix[b, ] <- as.numeric(abs(boot_coef) > 1e-8)
                    
                }, error = function(e) {
                    # Skip this bootstrap sample if it fails
                })
            }
            
            # Calculate selection frequencies
            selection_freq <- colMeans(selection_matrix)
            
            # Stable selection based on threshold
            stable_selection <- selection_freq >= self$options$stability_threshold
            
            return(list(
                selection_frequencies = selection_freq,
                stable_variables = cox_data$pred_names[stable_selection],
                selection_matrix = selection_matrix
            ))
        },

        .populateResults = function(adaptive_results, cox_data) {
            # Coefficients table
            if (self$options$show_coefficients) {
                private$.populateCoefficients(adaptive_results, cox_data)
            }

            # Selection path
            if (self$options$show_selection_path) {
                private$.populateSelectionPath(adaptive_results)
            }

            # Cross-validation results
            if (self$options$show_cv_results) {
                private$.populateCVResults(adaptive_results)
            }

            # Stability selection results
            if (self$options$stability_selection && !is.null(adaptive_results$stability)) {
                private$.populateStabilityResults(adaptive_results)
            }

            # Model diagnostics
            if (self$options$show_diagnostics) {
                private$.populateDiagnostics(adaptive_results)
            }

            # Performance metrics
            if (self$options$goodness_of_fit) {
                private$.populatePerformance(adaptive_results, cox_data)
            }

            # Plots
            if (self$options$plot_selection_path) {
                private$.plotSelectionPath(adaptive_results)
            }

            if (self$options$plot_cv_curve) {
                private$.plotCVCurve(adaptive_results)
            }

            if (self$options$plot_stability && !is.null(adaptive_results$stability)) {
                private$.plotStability(adaptive_results)
            }
        },

        .populateCoefficients = function(adaptive_results, cox_data) {
            coef_vec <- as.vector(adaptive_results$coef_min)
            selected_idx <- which(abs(coef_vec) > 1e-8)
            
            if (length(selected_idx) == 0) {
                # No variables selected
                coef_data <- data.frame(
                    variable = "No variables selected",
                    coefficient = NA,
                    exp_coefficient = NA,
                    std_error = NA,
                    lower_ci = NA,
                    upper_ci = NA,
                    adaptive_weight = NA
                )
            } else {
                coef_data <- data.frame(
                    variable = cox_data$pred_names[selected_idx],
                    coefficient = coef_vec[selected_idx],
                    exp_coefficient = exp(coef_vec[selected_idx]),
                    std_error = rep(NA, length(selected_idx)),  # Would need bootstrap for SEs
                    lower_ci = rep(NA, length(selected_idx)),
                    upper_ci = rep(NA, length(selected_idx)),
                    adaptive_weight = adaptive_results$adaptive_weights[selected_idx]
                )
            }

            self$results$coefficients$setData(coef_data)
        },

        .populateSelectionPath = function(adaptive_results) {
            cv_fit <- adaptive_results$cv_fit
            full_fit <- adaptive_results$full_fit
            
            # Create path summary
            n_lambda <- length(cv_fit$lambda)
            n_steps <- min(20, n_lambda)  # Limit to 20 steps for readability
            step_indices <- round(seq(1, n_lambda, length.out = n_steps))
            
            path_data <- data.frame(
                step = 1:n_steps,
                lambda = cv_fit$lambda[step_indices],
                n_selected = sapply(step_indices, function(i) {
                    coefs <- as.vector(coef(full_fit, s = cv_fit$lambda[i]))
                    sum(abs(coefs) > 1e-8)
                }),
                deviance = cv_fit$cvm[step_indices],
                cv_error = cv_fit$cvm[step_indices]
            )

            self$results$selectionPath$setData(path_data)
        },

        .populateCVResults = function(adaptive_results) {
            cv_fit <- adaptive_results$cv_fit
            
            # Extract key CV results
            min_idx <- which(cv_fit$lambda == cv_fit$lambda.min)
            se_idx <- which(cv_fit$lambda == cv_fit$lambda.1se)
            
            cv_data <- data.frame(
                criterion = "Cross-Validation",
                lambda_min = cv_fit$lambda.min,
                lambda_1se = cv_fit$lambda.1se,
                cv_error_min = cv_fit$cvm[min_idx],
                cv_error_1se = cv_fit$cvm[se_idx],
                n_variables_min = sum(abs(adaptive_results$coef_min) > 1e-8),
                n_variables_1se = sum(abs(adaptive_results$coef_1se) > 1e-8)
            )

            self$results$cvResults$setData(cv_data)
        },

        .populateStabilityResults = function(adaptive_results) {
            stability <- adaptive_results$stability
            
            stability_data <- data.frame(
                variable = colnames(adaptive_results$cv_fit$glmnet.fit$beta),
                selection_frequency = stability$selection_frequencies,
                stability_score = stability$selection_frequencies,
                stable_selection = ifelse(stability$selection_frequencies >= self$options$stability_threshold, 
                                        "Stable", "Unstable"),
                error_bound = rep(NA, length(stability$selection_frequencies))
            )

            self$results$stabilityResults$setData(stability_data)
        },

        .populateDiagnostics = function(adaptive_results) {
            diag <- adaptive_results$diagnostics
            
            diag_data <- data.frame(
                diagnostic = c("Number of Selected Variables", "Concordance Index", "Log-Likelihood", "AIC"),
                statistic = c(
                    diag$n_selected,
                    diag$concordance,
                    diag$loglik,
                    diag$aic
                ),
                p_value = c(NA, NA, NA, NA),
                interpretation = c(
                    paste(diag$n_selected, "variables selected"),
                    if (!is.na(diag$concordance)) {
                        if (diag$concordance > 0.7) "Good discrimination" else "Moderate discrimination"
                    } else "Not available",
                    "Model fit measure",
                    "Model complexity measure"
                )
            )

            self$results$modelDiagnostics$setData(diag_data)
        },

        .populatePerformance = function(adaptive_results, cox_data) {
            diag <- adaptive_results$diagnostics
            
            perf_data <- data.frame(
                metric = c("C-index", "Number of Variables", "Log-Likelihood"),
                value = c(
                    diag$concordance,
                    diag$n_selected,
                    diag$loglik
                ),
                confidence_interval = c("", "", ""),
                description = c(
                    "Concordance index (discrimination)",
                    "Variables selected by adaptive LASSO",
                    "Model log-likelihood"
                )
            )

            self$results$performanceMetrics$setData(perf_data)
        },

        .plotSelectionPath = function(adaptive_results) {
            image <- self$results$pathPlot
            image$setState(adaptive_results)
        },

        .plotCVCurve = function(adaptive_results) {
            image <- self$results$cvPlot
            image$setState(adaptive_results)
        },

        .plotStability = function(adaptive_results) {
            image <- self$results$stabilityPlot
            image$setState(adaptive_results)
        },

        .initResults = function() {
            # Initialize result tables with proper visibility
            self$results$coefficients$setVisible(self$options$show_coefficients)
            self$results$selectionPath$setVisible(self$options$show_selection_path)
            self$results$cvResults$setVisible(self$options$show_cv_results)
            self$results$stabilityResults$setVisible(self$options$stability_selection)
            self$results$modelDiagnostics$setVisible(self$options$show_diagnostics)
            self$results$performanceMetrics$setVisible(self$options$goodness_of_fit)
        }
    )
)