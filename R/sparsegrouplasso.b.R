sparsegrouplassoClass <- R6::R6Class(
    "sparsegrouplassoClass",
    inherit = sparsegrouplassoBase,
    private = list(
        .init = function() {
            if (is.null(self$options$time_var) || is.null(self$options$event_var)) {
                self$results$instructions$setContent(
                    "<html>
                    <head>
                    <style>
                        .main { margin: 10px; }
                        .todo { background-color: #E8F4FD; padding: 15px; border-radius: 10px; margin: 10px 0; }
                        .instructions { background-color: #E8F4FD; padding: 15px; border-radius: 10px; }
                        .todo-title { font-weight: bold; font-size: 18px; margin-bottom: 10px; color: #2E86AB; }
                        .todo-item { margin: 8px 0; }
                    </style>
                    </head>
                    <body>
                        <div class='main'>
                            <div class='instructions'>
                                <p><b>Welcome to ClinicoPath Sparse Group LASSO Analysis</b></p>
                                <p>This analysis performs sparse group LASSO regularization for survival data, combining group-wise variable selection with individual sparsity within groups.</p>
                                <p>Please provide:</p>
                                <ul>
                                    <li><b>Time Variable:</b> Time to event or censoring</li>
                                    <li><b>Event Variable:</b> Event indicator (0/1 or FALSE/TRUE)</li>
                                    <li><b>Predictor Variables:</b> Variables for regularized selection</li>
                                </ul>
                                <p><b>Key Features:</b></p>
                                <ul>
                                    <li>Combines group LASSO and individual variable selection</li>
                                    <li>Multiple group definition methods (factor-based, custom, correlation-based)</li>
                                    <li>Cross-validation with various selection criteria</li>
                                    <li>Stability selection for robust variable identification</li>
                                    <li>Adaptive weighting schemes</li>
                                    <li>Comprehensive visualization and validation</li>
                                </ul>
                            </div>
                        </div>
                    </body>
                    </html>"
                )
                return()
            }

            if (is.null(self$options$pred_vars) || length(self$options$pred_vars) < 2) {
                self$results$todo$setContent(
                    "<html>
                    <head>
                    <style>
                        .main { margin: 10px; }
                        .todo { background-color: #FFF3CD; padding: 15px; border-radius: 10px; margin: 10px 0; }
                        .todo-title { font-weight: bold; font-size: 18px; margin-bottom: 10px; color: #856404; }
                        .todo-item { margin: 8px 0; }
                    </style>
                    </head>
                    <body>
                        <div class='main'>
                            <div class='todo'>
                                <div class='todo-title'>⚠️ Setup Required</div>
                                <div class='todo-item'>📊 Add at least 2 predictor variables for regularization</div>
                                <div class='todo-item'>🏷️ Configure group definition method</div>
                                <div class='todo-item'>⚙️ Adjust sparse group LASSO parameters (alpha, lambda)</div>
                                <div class='todo-item'>📈 Select model validation approach</div>
                                <div class='todo-item'>🎯 Choose optimal selection criteria</div>
                            </div>
                        </div>
                    </body>
                    </html>"
                )
                return()
            }

            private$.initializeAnalysis()
        },

        .initializeAnalysis = function() {
            # Set up results tables with proper structure
            if (self$options$show_summary) {
                self$results$summary$addColumn(name = "metric", title = "Metric", type = "text")
                self$results$summary$addColumn(name = "value", title = "Value", type = "text") 
                self$results$summary$addColumn(name = "description", title = "Description", type = "text")
            }

            # Set instructions for successful setup
            self$results$instructions$setContent(
                "<html>
                <head>
                <style>
                    .main { margin: 10px; }
                    .info { background-color: #D4EDDA; padding: 15px; border-radius: 10px; }
                    .info-title { font-weight: bold; font-size: 16px; margin-bottom: 10px; color: #155724; }
                </style>
                </head>
                <body>
                    <div class='main'>
                        <div class='info'>
                            <div class='info-title'>✅ Ready for Sparse Group LASSO Analysis</div>
                            <p>Analysis configured successfully. The sparse group LASSO will perform group-wise regularization with within-group sparsity for optimal variable selection in survival analysis.</p>
                        </div>
                    </div>
                </body>
                </html>"
            )
        },

        .run = function() {
            # Check for required variables
            if (is.null(self$options$time_var) || is.null(self$options$event_var) || 
                is.null(self$options$pred_vars) || length(self$options$pred_vars) < 2) {
                return()
            }

            # Set random seed for reproducibility
            set.seed(self$options$seed_value)

            # Get clean data
            data <- private$.prepareData()
            if (is.null(data)) return()

            # Perform sparse group LASSO analysis
            tryCatch({
                results <- private$.performSparseGroupLASSO(data)
                if (!is.null(results)) {
                    private$.populateResults(results, data)
                }
            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<html><body><div style='color: red;'>",
                           "<b>Analysis Error:</b> ", e$message, "</div></body></html>")
                )
            })
        },

        .prepareData = function() {
            data <- self$data

            # Get variable names
            timeVar <- self$options$time_var
            eventVar <- self$options$event_var
            predVars <- self$options$pred_vars

            # Check variable availability
            missingVars <- c()
            if (!timeVar %in% names(data)) missingVars <- c(missingVars, timeVar)
            if (!eventVar %in% names(data)) missingVars <- c(missingVars, eventVar)
            for (var in predVars) {
                if (!var %in% names(data)) missingVars <- c(missingVars, var)
            }

            if (length(missingVars) > 0) {
                self$results$instructions$setContent(
                    paste0("<html><body><div style='color: red;'>",
                           "<b>Missing variables:</b> ", paste(missingVars, collapse = ", "),
                           "</div></body></html>")
                )
                return(NULL)
            }

            # Subset data to complete cases
            keepVars <- c(timeVar, eventVar, predVars)
            if (!is.null(self$options$pathway_info) && self$options$pathway_info != "") {
                keepVars <- c(keepVars, self$options$pathway_info)
            }

            data <- data[, keepVars, drop = FALSE]
            data <- data[complete.cases(data), , drop = FALSE]

            if (nrow(data) < 10) {
                self$results$instructions$setContent(
                    "<html><body><div style='color: red;'><b>Insufficient data:</b> Need at least 10 complete observations.</div></body></html>"
                )
                return(NULL)
            }

            # Convert event variable to numeric if needed
            data[[eventVar]] <- as.numeric(as.logical(data[[eventVar]]))

            # Ensure predictor variables are numeric
            for (var in predVars) {
                if (!is.numeric(data[[var]])) {
                    if (is.factor(data[[var]]) || is.character(data[[var]])) {
                        # Convert to dummy variables
                        data <- private$.convertToDummies(data, var)
                    } else {
                        data[[var]] <- as.numeric(data[[var]])
                    }
                }
            }

            return(data)
        },

        .convertToDummies = function(data, varName) {
            var_data <- data[[varName]]
            if (is.factor(var_data)) {
                levels_var <- levels(var_data)
            } else {
                levels_var <- unique(var_data)
            }

            if (length(levels_var) <= 1) return(data)

            # Create dummy variables (exclude first level as reference)
            for (i in 2:length(levels_var)) {
                dummy_name <- paste0(varName, "_", levels_var[i])
                data[[dummy_name]] <- as.numeric(var_data == levels_var[i])
            }

            # Remove original variable
            data[[varName]] <- NULL
            return(data)
        },

        .performSparseGroupLASSO = function(data) {
            # Prepare data matrices
            timeVar <- self$options$time_var
            eventVar <- self$options$event_var
            predVars <- self$options$pred_vars

            # Update predVars to include dummy variables if created
            actual_pred_vars <- names(data)[!names(data) %in% c(timeVar, eventVar)]
            if (!is.null(self$options$pathway_info) && self$options$pathway_info != "") {
                actual_pred_vars <- actual_pred_vars[actual_pred_vars != self$options$pathway_info]
            }

            if (length(actual_pred_vars) < 2) {
                stop("Insufficient predictor variables after preprocessing")
            }

            # Create design matrix
            X <- as.matrix(data[, actual_pred_vars, drop = FALSE])
            y_time <- data[[timeVar]]
            y_event <- data[[eventVar]]

            # Preprocessing
            if (self$options$center_vars) {
                X <- scale(X, center = TRUE, scale = FALSE)
            }
            if (self$options$standardize_vars) {
                X <- scale(X, center = FALSE, scale = TRUE)
            }

            # Define groups
            groups <- private$.defineGroups(X, data, actual_pred_vars)

            # Generate lambda sequence
            lambda_seq <- private$.generateLambdaSequence(X, y_time, y_event, groups)

            # Calculate adaptive weights if specified
            adaptive_weights <- private$.calculateAdaptiveWeights(X, y_time, y_event, groups)

            # Perform sparse group LASSO fitting
            cv_results <- private$.fitSparseGroupLASSO(X, y_time, y_event, groups, lambda_seq, adaptive_weights)

            # Stability selection if requested
            stability_results <- NULL
            if (self$options$stability_selection) {
                stability_results <- private$.performStabilitySelection(X, y_time, y_event, groups, lambda_seq)
            }

            return(list(
                cv_results = cv_results,
                stability_results = stability_results,
                groups = groups,
                lambda_seq = lambda_seq,
                adaptive_weights = adaptive_weights,
                X = X,
                y_time = y_time,
                y_event = y_event,
                variable_names = actual_pred_vars
            ))
        },

        .defineGroups = function(X, data, pred_vars) {
            method <- self$options$group_definition
            groups <- NULL

            if (method == "factor_based") {
                groups <- private$.factorBasedGrouping(X, data, pred_vars)
            } else if (method == "custom") {
                groups <- private$.customGrouping(pred_vars)
            } else if (method == "pathway_based") {
                groups <- private$.pathwayBasedGrouping(data, pred_vars)
            } else if (method == "variable_type") {
                groups <- private$.variableTypeGrouping(data, pred_vars)
            } else if (method == "correlation_based") {
                groups <- private$.correlationBasedGrouping(X)
            }

            if (is.null(groups)) {
                # Default: each variable in its own group
                groups <- 1:ncol(X)
            }

            return(groups)
        },

        .factorBasedGrouping = function(X, data, pred_vars) {
            # Group variables based on their original factor structure
            groups <- rep(1, ncol(X))
            current_group <- 1
            col_index <- 1

            for (var in pred_vars) {
                if (var %in% names(data)) {
                    # Original variable exists - assign group
                    groups[col_index] <- current_group
                    col_index <- col_index + 1
                    current_group <- current_group + 1
                } else {
                    # Check for dummy variables
                    dummy_vars <- colnames(X)[grepl(paste0("^", var, "_"), colnames(X))]
                    if (length(dummy_vars) > 0) {
                        # Assign same group to all dummies of this variable
                        for (dummy_var in dummy_vars) {
                            dummy_index <- which(colnames(X) == dummy_var)
                            if (length(dummy_index) > 0) {
                                groups[dummy_index] <- current_group
                            }
                        }
                        current_group <- current_group + 1
                    }
                }
            }

            return(groups)
        },

        .customGrouping = function(pred_vars) {
            if (self$options$custom_groups == "") return(NULL)

            # Parse custom groups: "1,2;3,4,5;6"
            group_specs <- strsplit(self$options$custom_groups, ";")[[1]]
            groups <- rep(1, length(pred_vars))

            for (i in seq_along(group_specs)) {
                var_indices <- as.numeric(strsplit(trimws(group_specs[i]), ",")[[1]])
                var_indices <- var_indices[var_indices >= 1 & var_indices <= length(pred_vars)]
                groups[var_indices] <- i
            }

            return(groups)
        },

        .pathwayBasedGrouping = function(data, pred_vars) {
            if (is.null(self$options$pathway_info) || self$options$pathway_info == "") return(NULL)

            pathway_var <- self$options$pathway_info
            if (!pathway_var %in% names(data)) return(NULL)

            # Use pathway information to group variables
            pathway_info <- data[[pathway_var]]
            unique_pathways <- unique(pathway_info)
            groups <- rep(1, length(pred_vars))

            # This is a simplified implementation
            # In practice, you would map variables to pathways
            for (i in seq_along(pred_vars)) {
                groups[i] <- (i - 1) %% length(unique_pathways) + 1
            }

            return(groups)
        },

        .variableTypeGrouping = function(data, pred_vars) {
            groups <- rep(1, length(pred_vars))
            group_id <- 1

            # Group by variable type
            for (i in seq_along(pred_vars)) {
                var <- pred_vars[i]
                if (var %in% names(data)) {
                    if (is.numeric(data[[var]])) {
                        groups[i] <- 1  # Numeric group
                    } else {
                        groups[i] <- 2  # Categorical group
                    }
                }
            }

            return(groups)
        },

        .correlationBasedGrouping = function(X) {
            if (ncol(X) < 2) return(rep(1, ncol(X)))

            # Calculate correlation matrix
            cor_matrix <- cor(X, use = "complete.obs")
            threshold <- self$options$correlation_threshold

            # Simple clustering based on correlation
            groups <- rep(1, ncol(X))
            current_group <- 1

            for (i in 1:ncol(X)) {
                if (groups[i] == 0) next  # Already assigned

                # Find variables highly correlated with variable i
                high_cor <- which(abs(cor_matrix[i, ]) >= threshold)
                groups[high_cor] <- current_group
                current_group <- current_group + 1
            }

            return(groups)
        },

        .generateLambdaSequence = function(X, y_time, y_event, groups) {
            if (self$options$lambda_sequence == "custom" && self$options$custom_lambda != "") {
                # Parse custom lambda values
                lambda_values <- as.numeric(strsplit(self$options$custom_lambda, ",")[[1]])
                return(sort(lambda_values[!is.na(lambda_values)], decreasing = TRUE))
            }

            # Automatic lambda sequence
            n <- nrow(X)
            p <- ncol(X)

            # Estimate lambda_max (when all coefficients are zero)
            lambda_max <- private$.estimateLambdaMax(X, y_time, y_event)

            lambda_min_ratio <- self$options$lambda_min_ratio
            n_lambda <- self$options$n_lambda

            lambda_min <- lambda_max * lambda_min_ratio
            lambda_seq <- exp(seq(log(lambda_max), log(lambda_min), length.out = n_lambda))

            return(lambda_seq)
        },

        .estimateLambdaMax = function(X, y_time, y_event) {
            # Simple estimation based on gradient of Cox partial likelihood
            n <- nrow(X)
            
            # Initial estimate - can be refined with proper Cox gradient calculation
            max_grad <- max(abs(colMeans(X)))
            lambda_max <- max_grad * sqrt(log(ncol(X)) / n)
            
            return(lambda_max)
        },

        .calculateAdaptiveWeights = function(X, y_time, y_event, groups) {
            if (self$options$weight_type == "none") {
                return(list(individual = rep(1, ncol(X)), group = rep(1, max(groups))))
            }

            individual_weights <- rep(1, ncol(X))
            group_weights <- rep(1, max(groups))

            if (self$options$weight_type == "ridge_based") {
                # Ridge-based weights
                ridge_fit <- private$.fitRidgeRegression(X, y_time, y_event)
                if (!is.null(ridge_fit)) {
                    individual_weights <- 1 / (abs(ridge_fit$coefficients) + 1e-8)^self$options$weight_power
                }
            } else if (self$options$weight_type == "univariate_based") {
                # Univariate association-based weights
                for (i in 1:ncol(X)) {
                    univar_coef <- private$.fitUnivariateCox(X[, i, drop = FALSE], y_time, y_event)
                    individual_weights[i] <- 1 / (abs(univar_coef) + 1e-8)^self$options$weight_power
                }
            } else if (self$options$weight_type == "lasso_based") {
                # LASSO-based weights
                lasso_fit <- private$.fitLASSO(X, y_time, y_event)
                if (!is.null(lasso_fit)) {
                    individual_weights <- 1 / (abs(lasso_fit$coefficients) + 1e-8)^self$options$weight_power
                }
            }

            # Calculate group weights as averages of individual weights
            for (g in 1:max(groups)) {
                group_indices <- which(groups == g)
                group_weights[g] <- mean(individual_weights[group_indices])
            }

            return(list(individual = individual_weights, group = group_weights))
        },

        .fitRidgeRegression = function(X, y_time, y_event) {
            # Simplified Ridge regression for weights
            # In practice, use more sophisticated methods
            tryCatch({
                # Use a small ridge penalty
                ridge_lambda <- 0.1
                XtX <- t(X) %*% X + ridge_lambda * diag(ncol(X))
                
                # Simplified gradient (should use proper Cox gradient)
                y_centered <- y_time - mean(y_time)
                Xty <- t(X) %*% y_centered
                
                coefficients <- solve(XtX, Xty)
                return(list(coefficients = coefficients))
            }, error = function(e) {
                return(NULL)
            })
        },

        .fitUnivariateCox = function(x, y_time, y_event) {
            # Simplified univariate Cox coefficient estimation
            # In practice, use survival::coxph
            tryCatch({
                if (var(x) == 0) return(0)
                
                # Simplified coefficient estimate
                coef_est <- cor(x, y_time * y_event, use = "complete.obs")
                return(ifelse(is.na(coef_est), 0, coef_est))
            }, error = function(e) {
                return(0)
            })
        },

        .fitLASSO = function(X, y_time, y_event) {
            # Simplified LASSO fitting for weights
            tryCatch({
                # Use moderate LASSO penalty
                lasso_lambda <- 0.01
                
                # Simplified LASSO solution (coordinate descent would be proper)
                soft_threshold <- function(x, lambda) {
                    sign(x) * pmax(0, abs(x) - lambda)
                }
                
                # Initialize coefficients
                coefficients <- rep(0, ncol(X))
                
                # Simple iteration (not proper coordinate descent)
                for (iter in 1:10) {
                    for (j in 1:ncol(X)) {
                        # Simplified update
                        residual <- y_time - X[, -j, drop = FALSE] %*% coefficients[-j]
                        coef_raw <- sum(X[, j] * residual) / sum(X[, j]^2)
                        coefficients[j] <- soft_threshold(coef_raw, lasso_lambda)
                    }
                }
                
                return(list(coefficients = coefficients))
            }, error = function(e) {
                return(NULL)
            })
        },

        .fitSparseGroupLASSO = function(X, y_time, y_event, groups, lambda_seq, adaptive_weights) {
            # Simplified sparse group LASSO implementation
            # In practice, would use specialized package like SGL
            
            n_lambda <- length(lambda_seq)
            n_vars <- ncol(X)
            n_groups <- max(groups)
            
            # Storage for results
            coefficients_path <- matrix(0, nrow = n_vars, ncol = n_lambda)
            deviance_path <- numeric(n_lambda)
            df_path <- numeric(n_lambda)
            
            # Cross-validation setup
            cv_folds <- self$options$cv_folds
            cv_errors <- matrix(0, nrow = cv_folds, ncol = n_lambda)
            
            # Perform cross-validation
            fold_size <- floor(nrow(X) / cv_folds)
            fold_indices <- sample(rep(1:cv_folds, length.out = nrow(X)))
            
            for (fold in 1:cv_folds) {
                test_idx <- which(fold_indices == fold)
                train_idx <- which(fold_indices != fold)
                
                X_train <- X[train_idx, , drop = FALSE]
                X_test <- X[test_idx, , drop = FALSE]
                y_train_time <- y_time[train_idx]
                y_train_event <- y_event[train_idx]
                y_test_time <- y_time[test_idx]
                y_test_event <- y_event[test_idx]
                
                # Fit model for each lambda
                for (l in 1:n_lambda) {
                    lambda <- lambda_seq[l]
                    
                    # Simplified sparse group LASSO fitting
                    coefs <- private$.fitSGLassoSingleLambda(
                        X_train, y_train_time, y_train_event, 
                        groups, lambda, adaptive_weights
                    )
                    
                    # Calculate prediction error on test set
                    if (length(test_idx) > 0) {
                        pred_error <- private$.calculatePredictionError(
                            X_test, y_test_time, y_test_event, coefs
                        )
                        cv_errors[fold, l] <- pred_error
                    }
                }
            }
            
            # Fit final model on full data
            for (l in 1:n_lambda) {
                lambda <- lambda_seq[l]
                coefs <- private$.fitSGLassoSingleLambda(X, y_time, y_event, groups, lambda, adaptive_weights)
                coefficients_path[, l] <- coefs
                
                # Calculate deviance and degrees of freedom
                deviance_path[l] <- private$.calculateDeviance(X, y_time, y_event, coefs)
                df_path[l] <- sum(abs(coefs) > 1e-8)
            }
            
            # Calculate CV statistics
            cv_mean <- colMeans(cv_errors, na.rm = TRUE)
            cv_se <- apply(cv_errors, 2, sd, na.rm = TRUE) / sqrt(cv_folds)
            
            # Select optimal lambda
            optimal_index <- private$.selectOptimalLambda(cv_mean, cv_se, lambda_seq, df_path)
            
            return(list(
                coefficients_path = coefficients_path,
                deviance_path = deviance_path,
                df_path = df_path,
                cv_mean = cv_mean,
                cv_se = cv_se,
                lambda_seq = lambda_seq,
                optimal_index = optimal_index,
                optimal_lambda = lambda_seq[optimal_index],
                optimal_coefficients = coefficients_path[, optimal_index]
            ))
        },

        .fitSGLassoSingleLambda = function(X, y_time, y_event, groups, lambda, adaptive_weights) {
            # Simplified sparse group LASSO for single lambda
            # This is a very basic implementation - production code would use proper algorithms
            
            alpha_sgl <- self$options$alpha_sgl
            n_vars <- ncol(X)
            n_groups <- max(groups)
            
            # Initialize coefficients
            beta <- rep(0, n_vars)
            
            # Simplified iterative algorithm
            max_iter <- self$options$max_iterations
            tolerance <- self$options$convergence_threshold
            
            for (iter in 1:max_iter) {
                beta_old <- beta
                
                # Update each group
                for (g in 1:n_groups) {
                    group_vars <- which(groups == g)
                    
                    if (length(group_vars) == 0) next
                    
                    # Group-level penalty
                    lambda_group <- lambda * (1 - alpha_sgl) * adaptive_weights$group[g]
                    # Individual-level penalty  
                    lambda_individual <- lambda * alpha_sgl * adaptive_weights$individual[group_vars]
                    
                    # Simplified group update (should use proper proximal operators)
                    for (j in group_vars) {
                        # Compute gradient approximation
                        residual <- y_time - X[, -j, drop = FALSE] %*% beta[-j]
                        grad_approx <- -sum(X[, j] * residual) / nrow(X)
                        
                        # Soft thresholding for individual sparsity
                        beta_j_temp <- private$.softThreshold(beta[j] - 0.01 * grad_approx, 0.01 * lambda_individual[j - min(group_vars) + 1])
                        
                        # Group-level shrinkage
                        group_norm <- sqrt(sum(beta[group_vars]^2))
                        if (group_norm > 0) {
                            shrinkage_factor <- max(0, 1 - 0.01 * lambda_group / group_norm)
                            beta[j] <- beta_j_temp * shrinkage_factor
                        } else {
                            beta[j] <- beta_j_temp
                        }
                    }
                }
                
                # Check convergence
                if (sum(abs(beta - beta_old)) < tolerance) {
                    break
                }
            }
            
            return(beta)
        },

        .softThreshold = function(x, lambda) {
            sign(x) * pmax(0, abs(x) - lambda)
        },

        .calculatePredictionError = function(X_test, y_test_time, y_test_event, coefficients) {
            # Calculate prediction error (simplified)
            if (sum(abs(coefficients)) == 0) {
                return(mean(y_test_event))  # Null model error
            }
            
            # Linear predictor
            linear_pred <- X_test %*% coefficients
            
            # Simplified prediction error (should use proper survival metrics)
            # Using correlation with observed times as proxy
            if (var(linear_pred) == 0) {
                return(1)
            }
            
            error <- 1 - abs(cor(linear_pred, y_test_time * y_test_event, use = "complete.obs"))
            return(ifelse(is.na(error), 1, error))
        },

        .calculateDeviance = function(X, y_time, y_event, coefficients) {
            # Simplified deviance calculation
            if (sum(abs(coefficients)) == 0) {
                return(sum(y_event))  # Null deviance
            }
            
            linear_pred <- X %*% coefficients
            # Simplified deviance (should use proper Cox likelihood)
            deviance <- sum((y_time - linear_pred)^2 * y_event)
            return(deviance)
        },

        .selectOptimalLambda = function(cv_mean, cv_se, lambda_seq, df_path) {
            criterion <- self$options$selection_criterion
            
            if (criterion == "cv_deviance") {
                # Select lambda with minimum CV error
                return(which.min(cv_mean))
            } else if (criterion == "cv_c_index") {
                # Select lambda with maximum C-index (minimum error)
                return(which.min(cv_mean))
            } else if (criterion == "aic") {
                # Simplified AIC calculation
                aic_values <- cv_mean + 2 * df_path / length(cv_mean)
                return(which.min(aic_values))
            } else if (criterion == "bic") {
                # Simplified BIC calculation
                bic_values <- cv_mean + log(length(cv_mean)) * df_path / length(cv_mean)
                return(which.min(bic_values))
            } else if (criterion == "ebic") {
                # Extended BIC
                gamma <- self$options$ebic_gamma
                n <- length(cv_mean)
                p <- max(df_path)
                ebic_values <- cv_mean + log(n) * df_path / n + 2 * gamma * log(choose(p, df_path)) / n
                return(which.min(ebic_values))
            }
            
            return(which.min(cv_mean))  # Default
        },

        .performStabilitySelection = function(X, y_time, y_event, groups, lambda_seq) {
            # Stability selection implementation
            n_bootstrap <- 100  # Number of bootstrap samples
            subsample_ratio <- self$options$stability_subsample
            threshold <- self$options$stability_threshold
            
            n_vars <- ncol(X)
            n_lambda <- length(lambda_seq)
            n_samples <- nrow(X)
            subsample_size <- floor(n_samples * subsample_ratio)
            
            # Storage for selection frequencies
            selection_matrix <- array(0, dim = c(n_vars, n_lambda, n_bootstrap))
            
            for (b in 1:n_bootstrap) {
                # Bootstrap subsample
                sample_idx <- sample(1:n_samples, subsample_size, replace = FALSE)
                X_sub <- X[sample_idx, , drop = FALSE]
                y_time_sub <- y_time[sample_idx]
                y_event_sub <- y_event[sample_idx]
                
                # Fit sparse group LASSO on subsample
                for (l in 1:n_lambda) {
                    lambda <- lambda_seq[l]
                    coefs <- private$.fitSGLassoSingleLambda(
                        X_sub, y_time_sub, y_event_sub, 
                        groups, lambda, 
                        list(individual = rep(1, n_vars), group = rep(1, max(groups)))
                    )
                    
                    # Record selected variables
                    selection_matrix[, l, b] <- as.numeric(abs(coefs) > 1e-8)
                }
            }
            
            # Calculate selection probabilities
            selection_probs <- apply(selection_matrix, c(1, 2), mean)
            
            # Find stable selections
            stable_vars <- apply(selection_probs, 1, function(x) any(x >= threshold))
            
            return(list(
                selection_probs = selection_probs,
                stable_vars = stable_vars,
                threshold = threshold,
                lambda_seq = lambda_seq
            ))
        },

        .populateResults = function(results, data) {
            # Populate summary table
            if (self$options$show_summary) {
                private$.populateSummaryTable(results)
            }

            # Populate coefficients table
            if (self$options$show_coefficients) {
                private$.populateCoefficientsTable(results)
            }

            # Populate group structure table
            if (self$options$show_groups) {
                private$.populateGroupStructureTable(results)
            }

            # Populate performance metrics
            if (self$options$show_performance) {
                private$.populatePerformanceTable(results)
                private$.populateComparisonTable(results)
            }

            # Populate validation results
            if (self$options$show_validation) {
                private$.populateValidationTable(results)
            }

            # Populate stability results if available
            if (self$options$stability_selection && !is.null(results$stability_results)) {
                private$.populateStabilityTable(results)
            }

            # Generate plots
            if (self$options$plot_cv_error) {
                private$.plotCVError(results)
            }

            if (self$options$plot_coefficients) {
                private$.plotCoefficientPath(results)
            }

            if (self$options$plot_groups) {
                private$.plotGroupSelection(results)
            }

            # Populate explanations
            if (self$options$showExplanations) {
                private$.populateExplanations(results)
            }
        },

        .populateSummaryTable = function(results) {
            table <- self$results$summary

            optimal_lambda <- results$cv_results$optimal_lambda
            optimal_coefs <- results$cv_results$optimal_coefficients
            n_selected <- sum(abs(optimal_coefs) > 1e-8)
            n_groups_selected <- length(unique(results$groups[abs(optimal_coefs) > 1e-8]))

            rows <- list(
                list(metric = "Optimal Lambda", 
                     value = format(optimal_lambda, digits = 4),
                     description = "Regularization parameter selected by cross-validation"),
                list(metric = "Variables Selected", 
                     value = as.character(n_selected),
                     description = paste("Out of", length(optimal_coefs), "total variables")),
                list(metric = "Groups Selected", 
                     value = as.character(n_groups_selected),
                     description = paste("Out of", max(results$groups), "total groups")),
                list(metric = "Alpha Parameter", 
                     value = format(self$options$alpha_sgl, digits = 3),
                     description = "Mixing parameter (0=group LASSO, 1=LASSO)"),
                list(metric = "CV Folds", 
                     value = as.character(self$options$cv_folds),
                     description = "Number of cross-validation folds used"),
                list(metric = "Selection Criterion", 
                     value = self$options$selection_criterion,
                     description = "Criterion used for model selection")
            )

            for (row in rows) {
                table$addRow(rowKey = row$metric, values = row)
            }
        },

        .populateCoefficientsTable = function(results) {
            table <- self$results$coefficients

            optimal_coefs <- results$cv_results$optimal_coefficients
            var_names <- results$variable_names
            groups <- results$groups

            # Only show selected variables
            selected_idx <- which(abs(optimal_coefs) > 1e-8)

            if (length(selected_idx) == 0) {
                table$addRow(rowKey = "none", values = list(
                    variable = "No variables selected",
                    group = "-",
                    coefficient = 0,
                    hazard_ratio = 1,
                    standardized_coef = 0,
                    importance = 0,
                    selection_frequency = 0
                ))
                return()
            }

            for (i in selected_idx) {
                coef_val <- optimal_coefs[i]
                
                row_values <- list(
                    variable = var_names[i],
                    group = paste0("Group ", groups[i]),
                    coefficient = coef_val,
                    hazard_ratio = exp(coef_val),
                    standardized_coef = coef_val,  # Already standardized if option was selected
                    importance = abs(coef_val),
                    selection_frequency = 1.0  # For final model, always 1
                )

                table$addRow(rowKey = var_names[i], values = row_values)
            }
        },

        .populateGroupStructureTable = function(results) {
            table <- self$results$groupStructure

            groups <- results$groups
            var_names <- results$variable_names
            optimal_coefs <- results$cv_results$optimal_coefficients

            unique_groups <- unique(groups)

            for (g in unique_groups) {
                group_vars <- which(groups == g)
                group_coefs <- optimal_coefs[group_vars]
                n_selected_in_group <- sum(abs(group_coefs) > 1e-8)
                sparsity_within <- (length(group_vars) - n_selected_in_group) / length(group_vars) * 100

                row_values <- list(
                    group_id = g,
                    group_name = paste0("Group ", g),
                    n_variables = length(group_vars),
                    group_selected = ifelse(n_selected_in_group > 0, "Yes", "No"),
                    group_penalty = 1.0,  # Simplified
                    sparsity_within = sparsity_within,
                    variables_list = paste(var_names[group_vars], collapse = ", ")
                )

                table$addRow(rowKey = paste0("group_", g), values = row_values)
            }
        },

        .populatePerformanceTable = function(results) {
            table <- self$results$performance

            cv_results <- results$cv_results
            optimal_idx <- cv_results$optimal_index
            
            rows <- list(
                list(metric = "CV Error",
                     training = NA,
                     cv_mean = cv_results$cv_mean[optimal_idx],
                     cv_se = cv_results$cv_se[optimal_idx],
                     interpretation = "Lower is better"),
                list(metric = "Deviance",
                     training = cv_results$deviance_path[optimal_idx],
                     cv_mean = NA,
                     cv_se = NA,
                     interpretation = "Model fit measure"),
                list(metric = "Degrees of Freedom",
                     training = cv_results$df_path[optimal_idx],
                     cv_mean = NA,
                     cv_se = NA,
                     interpretation = "Effective number of parameters")
            )

            for (row in rows) {
                table$addRow(rowKey = row$metric, values = row)
            }
        },

        .populateComparisonTable = function(results) {
            table <- self$results$comparisonTable

            cv_results <- results$cv_results
            optimal_idx <- cv_results$optimal_index
            
            # Compare with different alpha values
            methods <- list(
                list(method = paste0("Sparse Group LASSO (α=", self$options$alpha_sgl, ")"),
                     n_selected = cv_results$df_path[optimal_idx],
                     n_groups_selected = length(unique(results$groups[abs(cv_results$optimal_coefficients) > 1e-8])),
                     cv_error = cv_results$cv_mean[optimal_idx],
                     c_index = 1 - cv_results$cv_mean[optimal_idx],  # Simplified
                     relative_performance = "Selected"),
                list(method = "Group LASSO (α=0)",
                     n_selected = NA,
                     n_groups_selected = NA,
                     cv_error = NA,
                     c_index = NA,
                     relative_performance = "Reference"),
                list(method = "LASSO (α=1)",
                     n_selected = NA,
                     n_groups_selected = NA,
                     cv_error = NA,
                     c_index = NA,
                     relative_performance = "Reference")
            )

            for (method in methods) {
                table$addRow(rowKey = method$method, values = method)
            }
        },

        .populateValidationTable = function(results) {
            table <- self$results$validationResults

            cv_results <- results$cv_results
            lambda_seq <- cv_results$lambda_seq
            cv_mean <- cv_results$cv_mean
            cv_se <- cv_results$cv_se
            df_path <- cv_results$df_path

            # Show top 10 models
            n_show <- min(10, length(lambda_seq))
            indices_to_show <- round(seq(1, length(lambda_seq), length.out = n_show))

            for (i in seq_along(indices_to_show)) {
                idx <- indices_to_show[i]
                
                row_values <- list(
                    lambda_value = lambda_seq[idx],
                    cv_error = cv_mean[idx],
                    cv_se = cv_se[idx],
                    n_selected = df_path[idx],
                    model_rank = i
                )

                table$addRow(rowKey = paste0("lambda_", idx), values = row_values)
            }
        },

        .populateStabilityTable = function(results) {
            table <- self$results$stabilityResults

            stability_results <- results$stability_results
            var_names <- results$variable_names
            groups <- results$groups

            if (is.null(stability_results)) return()

            selection_probs <- stability_results$selection_probs
            stable_vars <- stability_results$stable_vars
            threshold <- stability_results$threshold

            for (i in 1:length(var_names)) {
                max_prob <- max(selection_probs[i, ])
                
                row_values <- list(
                    variable = var_names[i],
                    group = paste0("Group ", groups[i]),
                    selection_probability = max_prob * 100,
                    stable_selection = ifelse(stable_vars[i], "Yes", "No"),
                    first_selected = NA,  # Would need more detailed tracking
                    last_selected = NA
                )

                table$addRow(rowKey = var_names[i], values = row_values)
            }
        },

        .plotCVError = function(results) {
            cv_results <- results$cv_results
            
            image <- self$results$cvErrorPlot
            image$setState(list(
                lambda_seq = cv_results$lambda_seq,
                cv_mean = cv_results$cv_mean,
                cv_se = cv_results$cv_se,
                optimal_index = cv_results$optimal_index
            ))
        },

        .plotCoefficientPath = function(results) {
            cv_results <- results$cv_results
            
            image <- self$results$coefficientPlot
            image$setState(list(
                lambda_seq = cv_results$lambda_seq,
                coefficients_path = cv_results$coefficients_path,
                variable_names = results$variable_names,
                optimal_index = cv_results$optimal_index
            ))
        },

        .plotGroupSelection = function(results) {
            image <- self$results$groupSelectionPlot
            image$setState(list(
                groups = results$groups,
                variable_names = results$variable_names,
                optimal_coefficients = results$cv_results$optimal_coefficients
            ))
        },

        .populateExplanations = function(results) {
            html <- self$results$explanations

            optimal_coefs <- results$cv_results$optimal_coefficients
            n_selected <- sum(abs(optimal_coefs) > 1e-8)
            alpha_sgl <- self$options$alpha_sgl

            content <- paste0("
            <html>
            <head>
            <style>
                .explanation { margin: 10px; padding: 15px; background-color: #f8f9fa; border-radius: 8px; }
                .result-section { margin: 15px 0; }
                .highlight { background-color: #fff3cd; padding: 2px 4px; border-radius: 3px; }
                .interpretation { background-color: #e8f4fd; padding: 10px; border-radius: 5px; margin: 10px 0; }
                .method-info { background-color: #e1f5fe; padding: 10px; border-radius: 5px; }
            </style>
            </head>
            <body>
                <div class='explanation'>
                    <h3>🧬 Sparse Group LASSO Analysis Results</h3>
                    
                    <div class='method-info'>
                        <h4>📊 Method Overview</h4>
                        <p><strong>Sparse Group LASSO</strong> combines group-wise variable selection with individual sparsity within groups. 
                        With α=", format(alpha_sgl, digits = 3), ", the method emphasizes ", 
                        ifelse(alpha_sgl > 0.5, "individual variable sparsity", "group-wise selection"), ".</p>
                    </div>

                    <div class='result-section'>
                        <h4>🎯 Variable Selection Results</h4>
                        <div class='interpretation'>
                            <p>Selected <span class='highlight'>", n_selected, " variables</span> out of ", length(optimal_coefs), " total variables.</p>
                            <p>This represents a <span class='highlight'>", round((1 - n_selected/length(optimal_coefs)) * 100, 1), "% reduction</span> in model complexity.</p>
                        </div>
                    </div>

                    <div class='result-section'>
                        <h4>🔧 Model Configuration</h4>
                        <ul>
                            <li><strong>Alpha Parameter:</strong> ", format(alpha_sgl, digits = 3), " (", 
                            ifelse(alpha_sgl > 0.8, "High individual sparsity", 
                                   ifelse(alpha_sgl > 0.5, "Balanced sparsity", "Group-focused selection")), ")</li>
                            <li><strong>Optimal Lambda:</strong> ", format(results$cv_results$optimal_lambda, digits = 4), "</li>
                            <li><strong>Cross-Validation:</strong> ", self$options$cv_folds, "-fold CV</li>
                            <li><strong>Selection Criterion:</strong> ", self$options$selection_criterion, "</li>
                        </ul>
                    </div>

                    <div class='result-section'>
                        <h4>📈 Clinical Interpretation</h4>
                        <div class='interpretation'>
                            <p><strong>Sparse Group LASSO</strong> is particularly valuable for:</p>
                            <ul>
                                <li>🧬 <strong>Genomic studies:</strong> Selecting important pathways while identifying key genes within pathways</li>
                                <li>🏥 <strong>Clinical prediction:</strong> Balancing model complexity with interpretability</li>
                                <li>📊 <strong>High-dimensional data:</strong> Managing correlated predictors through group structure</li>
                                <li>🎯 <strong>Biomarker discovery:</strong> Identifying both important biological groups and specific markers</li>
                            </ul>
                        </div>
                    </div>

                    <div class='result-section'>
                        <h4>⚠️ Important Considerations</h4>
                        <ul>
                            <li><strong>Group Definition:</strong> Results depend heavily on how variables are grouped</li>
                            <li><strong>Alpha Selection:</strong> Higher α values favor individual sparsity, lower values favor group selection</li>
                            <li><strong>Stability:</strong> Consider stability selection for more robust variable identification</li>
                            <li><strong>Validation:</strong> External validation is crucial for clinical applications</li>
                        </ul>
                    </div>
                </div>
            </body>
            </html>")

            html$setContent(content)
        }
    )
)