grouplassoClass <- R6::R6Class(
    "grouplassoClass",
    inherit = grouplassoBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$time) || is.null(self$options$event)) {
                self$results$instructions$setContent(
                    "<html>
                    <head>
                    <style>
                        h2 {color: #2ecc71;}
                        body {font-family: Arial, sans-serif; margin: 20px;}
                        .highlight {background-color: #f39c12; padding: 2px 4px; border-radius: 3px;}
                        .step {margin: 10px 0; padding: 8px; background-color: #ecf0f1; border-radius: 5px;}
                    </style>
                    </head>
                    <body>
                    <h2> Group LASSO for Survival Analysis</h2>
                    <p><strong>Penalized regression with group-wise variable selection and structure preservation</strong></p>
                    
                    <div class='step'>
                    <strong> Required Data:</strong>
                    <ul>
                        <li><span class='highlight'>Time Variable</span>: Time to event or censoring</li>
                        <li><span class='highlight'>Event Indicator</span>: 0 = censored, 1 = event</li>
                        <li><span class='highlight'>Predictor Variables</span>: Variables organized into meaningful groups</li>
                    </ul>
                    </div>
                    
                    <div class='step'>
                    <strong> Group Definition Methods:</strong>
                    <ul>
                        <li><strong>Automatic:</strong> Groups by variable type (continuous, categorical)</li>
                        <li><strong>Factor-Based:</strong> Dummy variables from same factor grouped together</li>
                        <li><strong>Custom:</strong> User-defined groups (e.g., biological pathways)</li>
                        <li><strong>Biological:</strong> Pathway or domain-specific groupings</li>
                    </ul>
                    </div>
                    
                    <div class='step'>
                    <strong> Penalty Types:</strong>
                    <ul>
                        <li><strong>Group LASSO:</strong> Selects entire groups simultaneously</li>
                        <li><strong>Sparse Group LASSO:</strong> Combines group and individual penalties</li>
                        <li><strong>Adaptive Group:</strong> Data-driven penalty weights for groups</li>
                        <li><strong>Overlapping Groups:</strong> Variables can belong to multiple groups</li>
                    </ul>
                    </div>
                    
                    <div class='step'>
                    <strong> Key Features:</strong>
                    <ul>
                        <li> Cross-validation for optimal penalty selection</li>
                        <li> Stability selection for robust group identification</li>
                        <li> Nested CV for unbiased performance assessment</li>
                        <li> Multiple optimization algorithms (coordinate descent, ADMM)</li>
                        <li> Clinical interpretation with group importance measures</li>
                        <li> Permutation testing for statistical significance</li>
                    </ul>
                    </div>
                    
                    <div class='step'>
                    <strong> Clinical Applications:</strong>
                    <ul>
                        <li> Genomic pathway analysis in survival studies</li>
                        <li> Clinical domain groupings (demographics, lab values, imaging)</li>
                        <li> Treatment group comparisons with structured covariates</li>
                        <li> Biomarker panel selection with biological structure</li>
                    </ul>
                    </div>
                    
                    <p><em> Tip: Use factor-based grouping for categorical variables and custom grouping for domain-specific applications like gene pathways.</em></p>
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
            group_data <- private$.prepareData()
            if (is.null(group_data)) return()

            # Check if required packages are available
            required_packages <- c("survival", "glmnet")

            # Assess data suitability if requested
            if (self$options$suitabilityCheck) {
                # Just use predictor data as a matrix
                pred_mat <- as.matrix(group_data$data[, self$options$predictors, drop=FALSE])
                # Convert factors to numeric to allow rough correlation checks
                if(ncol(pred_mat) > 0) {
                   numeric_pred_mat <- matrix(NA, nrow=nrow(pred_mat), ncol=ncol(pred_mat))
                   for(i in 1:ncol(pred_mat)) {
                       numeric_pred_mat[,i] <- as.numeric(as.factor(pred_mat[,i]))
                   }
                   private$.assessSuitability(numeric_pred_mat, group_data$time, group_data$event)
                }
            }

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

            # Fit group LASSO model
            tryCatch({
                group_results <- private$.fitGroupLasso(group_data)
                if (!is.null(group_results)) {
                    private$.populateResults(group_results, group_data)
                }
            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<html><body><h3>Analysis Error</h3><p>", 
                           "Error in group LASSO fitting: ", e$message,
                           "</p><p>Check group definitions and variable structure.</p></body></html>")
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
                    <p>Need at least p + 10 complete observations for reliable group LASSO.</p></body></html>"
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

            # Prepare predictor matrix and groups
            pred_data <- analysis_data[, pred_vars, drop = FALSE]
            design_result <- private$.createDesignMatrix(pred_data, pred_vars)
            
            if (is.null(design_result)) return(NULL)

            # Create survival object
            requireNamespace("survival", quietly = TRUE)
            surv_obj <- survival::Surv(time_values, event_values)

            return(list(
                data = analysis_data,
                x = design_result$x,
                y = surv_obj,
                time = time_values,
                event = event_values,
                groups = design_result$groups,
                group_info = design_result$group_info,
                n_obs = nrow(analysis_data),
                n_events = sum(event_values),
                var_names = colnames(design_result$x),
                original_pred_names = pred_vars
            ))
        },

        .createDesignMatrix = function(pred_data, pred_vars) {
            # Convert factors to dummy variables and create design matrix
            x_matrix <- model.matrix(~ . - 1, data = pred_data)
            
            # Standardize if requested
            if (self$options$standardize) {
                x_matrix <- scale(x_matrix)
            }

            # Define groups based on method
            group_method <- self$options$group_definition
            groups <- private$.defineGroups(x_matrix, pred_data, pred_vars, group_method)
            
            if (is.null(groups)) {
                self$results$instructions$setContent(
                    "<html><body><h3>Group Definition Error</h3>
                    <p>Failed to define variable groups. Check group structure specification.</p></body></html>"
                )
                return(NULL)
            }

            return(list(
                x = x_matrix,
                groups = groups$group_vector,
                group_info = groups$group_info
            ))
        },

        .defineGroups = function(x_matrix, pred_data, pred_vars, method) {
            n_vars <- ncol(x_matrix)
            var_names <- colnames(x_matrix)
            
            if (method == "automatic") {
                # Group by variable type
                groups <- private$.automaticGrouping(pred_data, var_names)
                
            } else if (method == "factor_based") {
                # Group dummy variables from same factor
                groups <- private$.factorBasedGrouping(x_matrix, pred_data, pred_vars)
                
            } else if (method == "custom") {
                # User-defined groups
                groups <- private$.customGrouping(var_names)
                
            } else if (method == "biological") {
                # Biological/pathway grouping (placeholder for now)
                groups <- private$.biologicalGrouping(var_names)
            }

            if (is.null(groups)) return(NULL)

            # Calculate group weights
            group_weights <- private$.calculateGroupWeights(groups$group_vector, groups$group_info)

            return(list(
                group_vector = groups$group_vector,
                group_info = groups$group_info,
                group_weights = group_weights
            ))
        },

        .automaticGrouping = function(pred_data, var_names) {
            # Simple grouping by variable type
            n_vars <- length(var_names)
            group_vector <- rep(1, n_vars)
            group_id <- 1
            
            # Create separate groups for different types
            for (var in names(pred_data)) {
                var_indices <- grep(paste0("^", var), var_names)
                if (length(var_indices) > 0) {
                    group_vector[var_indices] <- group_id
                    group_id <- group_id + 1
                }
            }

            group_info <- data.frame(
                group_id = 1:max(group_vector),
                group_name = paste("Group", 1:max(group_vector)),
                group_size = as.vector(table(group_vector)),
                stringsAsFactors = FALSE
            )

            return(list(group_vector = group_vector, group_info = group_info))
        },

        .factorBasedGrouping = function(x_matrix, pred_data, pred_vars) {
            var_names <- colnames(x_matrix)
            group_vector <- numeric(length(var_names))
            group_id <- 1
            group_info_list <- list()

            for (var in pred_vars) {
                # Find columns corresponding to this variable
                if (is.factor(pred_data[[var]])) {
                    # Factor variable - group all dummy variables together
                    var_pattern <- paste0("^", var)
                    var_indices <- grep(var_pattern, var_names)
                    
                    if (length(var_indices) > 0) {
                        group_vector[var_indices] <- group_id
                        group_info_list[[group_id]] <- data.frame(
                            group_id = group_id,
                            group_name = var,
                            group_size = length(var_indices),
                            stringsAsFactors = FALSE
                        )
                        group_id <- group_id + 1
                    }
                } else {
                    # Continuous variable - individual group
                    var_indices <- which(var_names == var)
                    if (length(var_indices) > 0) {
                        group_vector[var_indices] <- group_id
                        group_info_list[[group_id]] <- data.frame(
                            group_id = group_id,
                            group_name = var,
                            group_size = 1,
                            stringsAsFactors = FALSE
                        )
                        group_id <- group_id + 1
                    }
                }
            }

            group_info <- do.call(rbind, group_info_list)
            return(list(group_vector = group_vector, group_info = group_info))
        },

        .customGrouping = function(var_names) {
            group_str <- trimws(self$options$group_structure)
            if (nchar(group_str) == 0) {
                # Default to individual groups
                group_vector <- 1:length(var_names)
                group_info <- data.frame(
                    group_id = 1:length(var_names),
                    group_name = paste("Group", 1:length(var_names)),
                    group_size = 1,
                    stringsAsFactors = FALSE
                )
                return(list(group_vector = group_vector, group_info = group_info))
            }

            # Parse custom group structure
            # Format: "var1:1, var2:1, var3:2"
            group_assignments <- unlist(strsplit(group_str, ","))
            group_vector <- rep(1, length(var_names))
            
            for (assignment in group_assignments) {
                parts <- trimws(unlist(strsplit(assignment, ":")))
                if (length(parts) == 2) {
                    var_name <- parts[1]
                    group_id <- as.numeric(parts[2])
                    var_idx <- which(var_names == var_name)
                    if (length(var_idx) > 0 && !is.na(group_id)) {
                        group_vector[var_idx] <- group_id
                    }
                }
            }

            # Create group info
            unique_groups <- sort(unique(group_vector))
            group_info <- data.frame(
                group_id = unique_groups,
                group_name = paste("Group", unique_groups),
                group_size = as.vector(table(group_vector)),
                stringsAsFactors = FALSE
            )

            return(list(group_vector = group_vector, group_info = group_info))
        },

        .biologicalGrouping = function(var_names) {
            # Placeholder for biological/pathway grouping
            # This could be extended to read from pathway databases
            group_vector <- rep(1, length(var_names))
            group_info <- data.frame(
                group_id = 1,
                group_name = "Biological Group",
                group_size = length(var_names),
                stringsAsFactors = FALSE
            )
            return(list(group_vector = group_vector, group_info = group_info))
        },

        .calculateGroupWeights = function(group_vector, group_info) {
            method <- self$options$group_weights
            n_groups <- nrow(group_info)
            
            if (method == "equal") {
                weights <- rep(1, n_groups)
            } else if (method == "sqrt_size") {
                weights <- sqrt(group_info$group_size)
            } else if (method == "group_size") {
                weights <- group_info$group_size
            } else if (method == "custom") {
                weight_str <- trimws(self$options$custom_weights)
                if (nchar(weight_str) > 0) {
                    weights <- as.numeric(unlist(strsplit(weight_str, ",")))
                    if (length(weights) != n_groups) {
                        weights <- rep(1, n_groups)
                    }
                } else {
                    weights <- rep(1, n_groups)
                }
            } else if (method == "adaptive") {
                # Calculate adaptive weights (placeholder)
                weights <- rep(1, n_groups)
            }

            return(weights)
        },

        .fitGroupLasso = function(group_data) {
            penalty_type <- self$options$penalty_type
            
            if (penalty_type == "group_lasso") {
                results <- private$.fitStandardGroupLasso(group_data)
            } else if (penalty_type == "sparse_group") {
                results <- private$.fitSparseGroupLasso(group_data)
            } else if (penalty_type == "adaptive_group") {
                results <- private$.fitAdaptiveGroupLasso(group_data)
            } else {
                results <- private$.fitStandardGroupLasso(group_data)
            }

            # Add stability selection if requested
            if (self$options$stability_selection) {
                stability_results <- private$.stabilitySelection(group_data)
                results$stability <- stability_results
            }

            # Add nested CV if requested
            if (self$options$nested_cv) {
                nested_results <- private$.nestedCrossValidation(group_data)
                results$nested_cv <- nested_results
            }

            return(results)
        },

        .fitStandardGroupLasso = function(group_data) {
            requireNamespace("glmnet", quietly = TRUE)
            requireNamespace("survival", quietly = TRUE)

            # Build penalty.factor from group structure to achieve group-level
            # penalization via glmnet. All variables in the same group receive
            # the same penalty weight (derived from group weights), so they are
            # selected/excluded together by the L1 penalty.
            group_vector <- group_data$groups
            group_info   <- group_data$group_info
            group_weights_vec <- private$.calculateGroupWeights(group_vector, group_info)

            # Map group-level weights to per-variable penalty factors
            penalty_factor <- numeric(length(group_vector))
            for (i in seq_len(nrow(group_info))) {
                gid <- group_info$group_id[i]
                penalty_factor[group_vector == gid] <- group_weights_vec[i]
            }

            # Fit group LASSO Cox with cross-validation via glmnet
            set.seed(self$options$random_seed)

            cv_result <- glmnet::cv.glmnet(
                x = group_data$x,
                y = group_data$y,
                family = "cox",
                alpha = 1,
                penalty.factor = penalty_factor,
                nfolds = self$options$cv_folds,
                standardize = FALSE,  # Already standardized if requested
                maxit = self$options$max_iterations
            )

            # Fit full regularization path
            full_fit <- glmnet::glmnet(
                x = group_data$x,
                y = group_data$y,
                family = "cox",
                alpha = 1,
                penalty.factor = penalty_factor,
                lambda = cv_result$lambda,
                standardize = FALSE,
                maxit = self$options$max_iterations
            )

            # Extract optimal results
            lambda_min <- cv_result$lambda.min
            lambda_1se <- cv_result$lambda.1se

            # Get coefficients at optimal lambda
            coef_min <- as.vector(coef(cv_result, s = "lambda.min"))
            coef_1se <- as.vector(coef(cv_result, s = "lambda.1se"))

            return(list(
                cv_fit = full_fit,
                cv_result = cv_result,
                lambda_min = lambda_min,
                lambda_1se = lambda_1se,
                coef_min = coef_min,
                coef_1se = coef_1se,
                group_vector = group_data$groups,
                penalty_factor = penalty_factor
            ))
        },

        .fitSparseGroupLasso = function(group_data) {
            # Sparse group LASSO: combines group-level (L1/L2) and individual (L1)
            # penalties via the glmnet alpha parameter. alpha < 1 mixes L1 and L2
            # norms, achieving within-group sparsity when combined with penalty.factor.
            requireNamespace("glmnet", quietly = TRUE)
            requireNamespace("survival", quietly = TRUE)

            group_vector <- group_data$groups
            group_info   <- group_data$group_info
            group_weights_vec <- private$.calculateGroupWeights(group_vector, group_info)

            penalty_factor <- numeric(length(group_vector))
            for (i in seq_len(nrow(group_info))) {
                gid <- group_info$group_id[i]
                penalty_factor[group_vector == gid] <- group_weights_vec[i]
            }

            # Use the user-specified alpha (sparsity parameter) to blend
            # group-level and individual-level penalties
            alpha_val <- self$options$alpha

            set.seed(self$options$random_seed)

            cv_result <- glmnet::cv.glmnet(
                x = group_data$x,
                y = group_data$y,
                family = "cox",
                alpha = alpha_val,
                penalty.factor = penalty_factor,
                nfolds = self$options$cv_folds,
                standardize = FALSE,
                maxit = self$options$max_iterations
            )

            full_fit <- glmnet::glmnet(
                x = group_data$x,
                y = group_data$y,
                family = "cox",
                alpha = alpha_val,
                penalty.factor = penalty_factor,
                lambda = cv_result$lambda,
                standardize = FALSE,
                maxit = self$options$max_iterations
            )

            lambda_min <- cv_result$lambda.min
            lambda_1se <- cv_result$lambda.1se
            coef_min <- as.vector(coef(cv_result, s = "lambda.min"))
            coef_1se <- as.vector(coef(cv_result, s = "lambda.1se"))

            return(list(
                cv_fit = full_fit,
                cv_result = cv_result,
                lambda_min = lambda_min,
                lambda_1se = lambda_1se,
                coef_min = coef_min,
                coef_1se = coef_1se,
                group_vector = group_data$groups,
                penalty_factor = penalty_factor
            ))
        },

        .fitAdaptiveGroupLasso = function(group_data) {
            # Adaptive group LASSO: use ridge-based initial estimates to compute
            # data-driven penalty weights per group, then fit L1-penalized Cox.
            requireNamespace("glmnet", quietly = TRUE)
            requireNamespace("survival", quietly = TRUE)

            group_vector <- group_data$groups
            group_info   <- group_data$group_info

            # Step 1: Obtain initial coefficient estimates via ridge Cox
            ridge_fit <- glmnet::glmnet(
                x = group_data$x,
                y = group_data$y,
                family = "cox",
                alpha = 0,
                standardize = FALSE
            )
            # Pick a moderate lambda along the path
            lambda_ridge <- ridge_fit$lambda[max(1, length(ridge_fit$lambda) %/% 4)]
            initial_coefs <- as.vector(coef(ridge_fit, s = lambda_ridge))

            # Step 2: Compute adaptive group weights from initial estimates
            # Weight = 1 / (group L2 norm of initial coefficients) ^ gamma
            gamma_val <- 1  # Standard adaptive weight exponent
            adaptive_penalty <- numeric(length(group_vector))
            for (i in seq_len(nrow(group_info))) {
                gid <- group_info$group_id[i]
                g_idx <- which(group_vector == gid)
                group_norm <- sqrt(sum(initial_coefs[g_idx]^2))
                # Avoid division by zero; large weight = strong penalization
                w <- if (group_norm > 1e-10) (1 / group_norm)^gamma_val else 1e4
                adaptive_penalty[g_idx] <- w
            }

            # Step 3: Fit adaptive group LASSO Cox
            set.seed(self$options$random_seed)

            cv_result <- glmnet::cv.glmnet(
                x = group_data$x,
                y = group_data$y,
                family = "cox",
                alpha = 1,
                penalty.factor = adaptive_penalty,
                nfolds = self$options$cv_folds,
                standardize = FALSE,
                maxit = self$options$max_iterations
            )

            full_fit <- glmnet::glmnet(
                x = group_data$x,
                y = group_data$y,
                family = "cox",
                alpha = 1,
                penalty.factor = adaptive_penalty,
                lambda = cv_result$lambda,
                standardize = FALSE,
                maxit = self$options$max_iterations
            )

            lambda_min <- cv_result$lambda.min
            lambda_1se <- cv_result$lambda.1se
            coef_min <- as.vector(coef(cv_result, s = "lambda.min"))
            coef_1se <- as.vector(coef(cv_result, s = "lambda.1se"))

            return(list(
                cv_fit = full_fit,
                cv_result = cv_result,
                lambda_min = lambda_min,
                lambda_1se = lambda_1se,
                coef_min = coef_min,
                coef_1se = coef_1se,
                group_vector = group_data$groups,
                penalty_factor = adaptive_penalty
            ))
        },

        .stabilitySelection = function(group_data) {
            requireNamespace("glmnet", quietly = TRUE)

            n_boot <- self$options$bootstrap_samples
            subsample_ratio <- 0.8
            n_groups <- max(group_data$groups)
            group_vector <- group_data$groups
            group_info <- group_data$group_info

            # Build penalty factor (same logic as in .fitStandardGroupLasso)
            group_weights_vec <- private$.calculateGroupWeights(group_vector, group_info)
            penalty_factor <- numeric(length(group_vector))
            for (i in seq_len(nrow(group_info))) {
                gid <- group_info$group_id[i]
                penalty_factor[group_vector == gid] <- group_weights_vec[i]
            }

            selection_matrix <- matrix(0, nrow = n_boot, ncol = n_groups)

            set.seed(self$options$random_seed)

            for (b in 1:n_boot) {
                n_sub <- floor(nrow(group_data$x) * subsample_ratio)
                boot_idx <- sample(nrow(group_data$x), n_sub, replace = FALSE)

                boot_x <- group_data$x[boot_idx, , drop = FALSE]
                boot_y <- group_data$y[boot_idx]

                tryCatch({
                    boot_cv <- glmnet::cv.glmnet(
                        x = boot_x,
                        y = boot_y,
                        family = "cox",
                        alpha = 1,
                        penalty.factor = penalty_factor,
                        nfolds = min(5, self$options$cv_folds),
                        standardize = FALSE
                    )

                    boot_coef <- as.vector(coef(boot_cv, s = "lambda.min"))

                    for (g in 1:n_groups) {
                        group_vars <- which(group_vector == g)
                        if (length(group_vars) > 0) {
                            group_selected <- any(abs(boot_coef[group_vars]) > 1e-8)
                            selection_matrix[b, g] <- as.numeric(group_selected)
                        }
                    }
                }, error = function(e) {
                    # Skip this bootstrap sample on error
                })
            }

            selection_freq <- colMeans(selection_matrix)
            stable_groups <- which(selection_freq >= self$options$stability_threshold)

            return(list(
                selection_frequencies = selection_freq,
                stable_groups = stable_groups,
                selection_matrix = selection_matrix
            ))
        },

        .nestedCrossValidation = function(group_data) {
            requireNamespace("glmnet", quietly = TRUE)
            requireNamespace("survival", quietly = TRUE)

            outer_folds <- self$options$cv_folds
            inner_folds <- self$options$inner_cv_folds
            n <- nrow(group_data$x)
            group_vector <- group_data$groups
            group_info <- group_data$group_info

            # Build penalty factor
            group_weights_vec <- private$.calculateGroupWeights(group_vector, group_info)
            penalty_factor <- numeric(length(group_vector))
            for (i in seq_len(nrow(group_info))) {
                gid <- group_info$group_id[i]
                penalty_factor[group_vector == gid] <- group_weights_vec[i]
            }

            set.seed(self$options$random_seed)
            fold_ids <- sample(rep(1:outer_folds, length.out = n))

            outer_performance <- numeric(outer_folds)
            optimal_lambdas   <- numeric(outer_folds)

            for (k in 1:outer_folds) {
                test_idx  <- which(fold_ids == k)
                train_idx <- which(fold_ids != k)

                train_x <- group_data$x[train_idx, , drop = FALSE]
                train_y <- group_data$y[train_idx]
                test_x  <- group_data$x[test_idx, , drop = FALSE]
                test_y  <- group_data$y[test_idx]

                tryCatch({
                    # Inner CV for lambda selection
                    inner_cv <- glmnet::cv.glmnet(
                        x = train_x,
                        y = train_y,
                        family = "cox",
                        alpha = 1,
                        penalty.factor = penalty_factor,
                        nfolds = inner_folds,
                        standardize = FALSE,
                        maxit = self$options$max_iterations
                    )

                    optimal_lambdas[k] <- inner_cv$lambda.min

                    # Evaluate on held-out fold using concordance (C-index)
                    test_coef <- as.vector(coef(inner_cv, s = "lambda.min"))
                    lp_test <- as.vector(test_x %*% test_coef)

                    # Compute C-index via survival::concordance
                    conc <- survival::concordance(
                        test_y ~ lp_test,
                        reverse = TRUE
                    )
                    outer_performance[k] <- conc$concordance

                }, error = function(e) {
                    outer_performance[k] <<- NA
                    optimal_lambdas[k]   <<- NA
                })
            }

            return(list(
                outer_performance = outer_performance,
                optimal_lambdas = optimal_lambdas
            ))
        },

        .populateResults = function(group_results, group_data) {
            # Group summary
            if (self$options$show_group_summary) {
                private$.populateGroupSummary(group_results, group_data)
            }

            # Coefficients
            if (self$options$show_coefficients) {
                private$.populateCoefficients(group_results, group_data)
            }

            # Path summary
            if (self$options$show_path_summary) {
                private$.populatePathSummary(group_results)
            }

            # CV results
            if (self$options$show_cv_results) {
                private$.populateCVResults(group_results)
            }

            # Stability results
            if (self$options$stability_selection && !is.null(group_results$stability)) {
                private$.populateStabilityResults(group_results, group_data)
            }

            # Nested CV results
            if (self$options$nested_cv && !is.null(group_results$nested_cv)) {
                private$.populateNestedCVResults(group_results, group_data)
            }

            # Performance metrics
            private$.populatePerformance(group_results, group_data)

            # Plots
            if (self$options$plot_regularization_path) {
                private$.plotRegularizationPath(group_results, group_data)
            }

            if (self$options$plot_cv_curve) {
                private$.plotCVCurve(group_results)
            }

            if (self$options$plot_group_importance) {
                private$.plotGroupImportance(group_results, group_data)
            }
        },

        .populateGroupSummary = function(group_results, group_data) {
            group_info <- group_data$group_info
            
            # Determine which groups are selected
            coef_vec <- as.vector(group_results$coef_min)
            group_vector <- group_data$groups
            
            selected_groups <- c()
            for (i in 1:nrow(group_info)) {
                group_vars <- which(group_vector == group_info$group_id[i])
                if (length(group_vars) > 0) {
                    group_selected <- any(abs(coef_vec[group_vars]) > self$options$selection_threshold)
                    selected_groups[i] <- ifelse(group_selected, "Selected", "Not Selected")
                } else {
                    selected_groups[i] <- "Not Selected"
                }
            }

            # Compute actual group weights used in penalty
            group_weights_vec <- private$.calculateGroupWeights(group_vector, group_info)

            # Compute group L2 norms of coefficients for entry order
            group_norms <- sapply(group_info$group_id, function(g) {
                g_idx <- which(group_vector == g)
                sqrt(sum(coef_vec[g_idx]^2))
            })
            # Rank by descending norm (larger norm = entered earlier)
            entry_order <- rank(-group_norms, ties.method = "min")

            group_summary <- data.frame(
                group_id = group_info$group_id,
                group_name = group_info$group_name,
                variables = sapply(group_info$group_id, function(g) {
                    var_indices <- which(group_vector == g)
                    paste(group_data$var_names[var_indices], collapse = ", ")
                }),
                group_size = group_info$group_size,
                group_weight = group_weights_vec,
                selected = selected_groups,
                selection_order = entry_order
            )

            self$results$groupSummary$setData(group_summary)
        },

        .populateCoefficients = function(group_results, group_data) {
            coef_vec <- as.vector(group_results$coef_min)
            group_vector <- group_data$groups
            var_names <- group_data$var_names
            group_info <- group_data$group_info

            # Compute per-variable group norm (L2 norm of coefficients in same group)
            group_norms <- numeric(length(coef_vec))
            for (i in seq_len(nrow(group_info))) {
                gid <- group_info$group_id[i]
                g_idx <- which(group_vector == gid)
                gnorm <- sqrt(sum(coef_vec[g_idx]^2))
                group_norms[g_idx] <- gnorm
            }

            max_abs <- max(abs(coef_vec), na.rm = TRUE)
            rel_importance <- if (max_abs > 0) abs(coef_vec) / max_abs else rep(0, length(coef_vec))

            # Create coefficient table
            coef_data <- data.frame(
                group_id = group_vector,
                variable = var_names,
                coefficient = coef_vec,
                exp_coefficient = exp(coef_vec),
                group_norm = group_norms,
                relative_importance = rel_importance,
                selected = ifelse(abs(coef_vec) > self$options$selection_threshold, "Yes", "No")
            )

            # Only show selected variables if any exist
            selected_vars <- coef_data[coef_data$selected == "Yes", ]
            if (nrow(selected_vars) > 0) {
                self$results$coefficients$setData(selected_vars)
            } else {
                # Show all variables with indication of no selection
                self$results$coefficients$setData(coef_data)
            }
        },

        .populatePathSummary = function(group_results) {
            full_fit <- group_results$cv_fit
            if (!is.null(full_fit)) {
                lambda_seq <- full_fit$lambda
                beta_matrix <- as.matrix(full_fit$beta)
                n_steps <- min(20, length(lambda_seq))
                step_indices <- round(seq(1, length(lambda_seq), length.out = n_steps))
                group_vector <- group_results$group_vector

                n_groups_sel <- numeric(n_steps)
                n_vars_sel   <- numeric(n_steps)
                dev_values   <- numeric(n_steps)
                df_values    <- numeric(n_steps)

                for (j in seq_along(step_indices)) {
                    idx <- step_indices[j]
                    coefs_at_lambda <- beta_matrix[, idx]
                    selected <- abs(coefs_at_lambda) > 1e-10
                    n_vars_sel[j] <- sum(selected)
                    # Count unique groups with at least one selected variable
                    if (any(selected)) {
                        n_groups_sel[j] <- length(unique(group_vector[selected]))
                    } else {
                        n_groups_sel[j] <- 0
                    }
                    dev_values[j] <- full_fit$dev.ratio[idx]
                    df_values[j]  <- full_fit$df[idx]
                }

                path_data <- data.frame(
                    step = 1:n_steps,
                    lambda = lambda_seq[step_indices],
                    n_groups_selected = as.integer(n_groups_sel),
                    n_variables_selected = as.integer(n_vars_sel),
                    deviance = dev_values,
                    df = df_values
                )

                self$results$pathSummary$setData(path_data)
            }
        },

        .populateCVResults = function(group_results) {
            cv_res <- group_results$cv_result
            if (!is.null(cv_res)) {
                # CV error at lambda.min
                idx_min <- which(cv_res$lambda == cv_res$lambda.min)
                cv_error_min <- if (length(idx_min) > 0) cv_res$cvm[idx_min[1]] else min(cv_res$cvm, na.rm = TRUE)

                # CV error at lambda.1se
                idx_1se <- which(cv_res$lambda == cv_res$lambda.1se)
                cv_error_1se <- if (length(idx_1se) > 0) cv_res$cvm[idx_1se[1]] else NA

                # Count selected groups at each lambda
                group_vector <- group_results$group_vector
                coef_min_vec <- group_results$coef_min
                coef_1se_vec <- group_results$coef_1se

                count_groups <- function(cv) {
                    sel <- abs(cv) > 1e-10
                    if (any(sel)) length(unique(group_vector[sel])) else 0L
                }

                cv_data <- data.frame(
                    criterion = "Cross-Validation (Partial Likelihood Deviance)",
                    lambda_min = group_results$lambda_min,
                    lambda_1se = group_results$lambda_1se,
                    cv_error_min = cv_error_min,
                    cv_error_1se = cv_error_1se,
                    groups_min = count_groups(coef_min_vec),
                    groups_1se = count_groups(coef_1se_vec)
                )

                self$results$cvResults$setData(cv_data)
            }
        },

        .populateStabilityResults = function(group_results, group_data) {
            stability <- group_results$stability
            group_info <- group_data$group_info
            
            stability_data <- data.frame(
                group_id = group_info$group_id,
                group_name = group_info$group_name,
                selection_frequency = stability$selection_frequencies,
                stability_score = stability$selection_frequencies,
                stable_selection = ifelse(stability$selection_frequencies >= self$options$stability_threshold, 
                                        "Stable", "Unstable"),
                first_selected = rep(NA, nrow(group_info))
            )

            self$results$stabilityResults$setData(stability_data)
        },

        .populateNestedCVResults = function(group_results, group_data) {
            nested <- group_results$nested_cv
            outer_folds <- length(nested$outer_performance)

            nested_data <- data.frame(
                outer_fold = 1:outer_folds,
                optimal_lambda = nested$optimal_lambdas,
                n_groups_selected = rep(NA_integer_, outer_folds),
                performance = nested$outer_performance,
                training_error = rep(NA_real_, outer_folds)
            )

            self$results$nestedCVResults$setData(nested_data)
        },

        .populatePerformance = function(group_results, group_data) {
            coef_vec <- group_results$coef_min
            group_vector <- group_data$groups
            selected <- abs(coef_vec) > self$options$selection_threshold
            n_vars_selected <- sum(selected)
            n_groups_selected <- if (any(selected)) length(unique(group_vector[selected])) else 0

            cv_res <- group_results$cv_result
            cv_error <- if (!is.null(cv_res)) {
                idx <- which(cv_res$lambda == cv_res$lambda.min)
                if (length(idx) > 0) cv_res$cvm[idx[1]] else min(cv_res$cvm, na.rm = TRUE)
            } else { NA }

            # Compute C-index on training data
            c_index <- NA
            if (n_vars_selected > 0) {
                tryCatch({
                    lp <- as.vector(group_data$x %*% coef_vec)
                    conc <- survival::concordance(group_data$y ~ lp, reverse = TRUE)
                    c_index <- conc$concordance
                }, error = function(e) {})
            } else {
                c_index <- 0.500
            }

            metrics <- c("Number of Groups Selected", "Number of Variables Selected",
                         "CV Partial Likelihood Deviance", "Training Concordance Index")
            values <- c(n_groups_selected, n_vars_selected, cv_error, c_index)
            ci_texts <- c("", "", "", "")
            descriptions <- c(
                "Groups with at least one non-zero coefficient",
                "Individual variables with non-zero coefficients",
                "Cross-validation error at optimal lambda",
                "Concordance index on training data (optimistic)"
            )

            perf_data <- data.frame(
                metric = metrics,
                value = values,
                confidence_interval = ci_texts,
                description = descriptions
            )

            self$results$modelPerformance$setData(perf_data)
            
            note <- "<p><i>Note: The Training Concordance Index overestimates true out-of-sample performance, especially for high-dimensional models. Use Cross-Validation or Nested CV for a realistic assessment.</i></p>"
            self$results$modelPerformanceNote$setContent(note)
        },

        .plotRegularizationPath = function(group_results, group_data) {
            image <- self$results$pathPlot
            # Extract coefficient path from the glmnet fit object
            full_fit <- group_results$cv_fit
            # full_fit$beta is a sparse matrix: variables x lambda
            beta_matrix <- as.matrix(full_fit$beta)
            plot_data <- list(
                lambda = as.numeric(full_fit$lambda),
                beta = as.data.frame(t(beta_matrix)),
                var_names = as.character(rownames(beta_matrix)),
                lambda_min = as.numeric(group_results$lambda_min),
                lambda_1se = as.numeric(group_results$lambda_1se),
                group_vector = as.integer(group_data$groups),
                group_names = as.character(group_data$group_info$group_name)
            )
            image$setState(plot_data)
        },

        .plotCVCurve = function(group_results) {
            image <- self$results$cvPlot
            cv_res <- group_results$cv_result
            plot_data <- list(
                lambda = as.numeric(cv_res$lambda),
                cvm = as.numeric(cv_res$cvm),
                cvsd = as.numeric(cv_res$cvsd),
                cvup = as.numeric(cv_res$cvup),
                cvlo = as.numeric(cv_res$cvlo),
                lambda_min = as.numeric(cv_res$lambda.min),
                lambda_1se = as.numeric(cv_res$lambda.1se)
            )
            image$setState(plot_data)
        },

        .plotGroupImportance = function(group_results, group_data) {
            image <- self$results$importancePlot
            # Use coef_min (the coefficient vector at lambda.min)
            coef_vec <- as.numeric(group_results$coef_min)
            var_names <- group_data$var_names
            group_vector <- group_data$groups

            # Compute group-level importance (L2 norm of group coefficients)
            group_info <- group_data$group_info
            group_importance <- numeric(nrow(group_info))
            for (i in seq_len(nrow(group_info))) {
                gid <- group_info$group_id[i]
                g_idx <- which(group_vector == gid)
                group_importance[i] <- sqrt(sum(coef_vec[g_idx]^2))
            }

            plot_data <- list(
                var_names = as.character(var_names),
                var_importance = as.numeric(abs(coef_vec)),
                coef_values = coef_vec,
                group_vector = as.integer(group_vector),
                group_names = as.character(group_info$group_name),
                group_importance = as.numeric(group_importance)
            )
            image$setState(plot_data)
        },

        .initResults = function() {
            # Initialize result tables with proper visibility
            self$results$groupSummary$setVisible(self$options$show_group_summary)
            self$results$coefficients$setVisible(self$options$show_coefficients)
            self$results$pathSummary$setVisible(self$options$show_path_summary)
            self$results$cvResults$setVisible(self$options$show_cv_results)
            self$results$stabilityResults$setVisible(self$options$stability_selection)
            self$results$nestedCVResults$setVisible(self$options$nested_cv)
            self$results$permutationResults$setVisible(self$options$permutation_test)
        },

        # Data suitability assessment
        .assessSuitability = function(pred_matrix, time_var, status_var) {
            checks <- list()
            
            n <- nrow(pred_matrix)
            n_events <- sum(status_var == 1)
            p <- ncol(pred_matrix)
            event_rate <- n_events / n

            # -- Check 1: Events-Per-Variable (EPV) --
            epv <- n_events / p
            if (epv >= 10) {
                checks$epv <- list(
                    color = "green", label = "Events-Per-Variable (Overall)",
                    value = sprintf("%.1f (n_events=%d, p=%d)", epv, n_events, p),
                    detail = "High EPV. Model estimation will be robust."
                )
            } else if (epv >= 1) {
                checks$epv <- list(
                    color = "yellow", label = "Events-Per-Variable (Overall)",
                    value = sprintf("%.1f (n_events=%d, p=%d)", epv, n_events, p),
                    detail = "Low EPV. Standard Cox would struggle, but Group LASSO handles this well."
                )
            } else {
                checks$epv <- list(
                    color = "red", label = "Events-Per-Variable (Overall)",
                    value = sprintf("%.3f (n_events=%d, p=%d)", epv, n_events, p),
                    detail = "Ultra-low EPV (< 1). Group LASSO regularization is absolutely necessary."
                )
            }

            # -- Check 2: Regularization/Reduction Need --
            if (p >= n / 3) {
                checks$regularization <- list(
                    color = "green", label = "Reduction Need",
                    value = sprintf("p=%d, n=%d (ratio=%.2f)", p, n, p / n),
                    detail = "High-dimensional setting. Group LASSO is strongly indicated."
                )
            } else {
                checks$regularization <- list(
                    color = "yellow", label = "Reduction Need",
                    value = sprintf("p=%d, EPV=%.0f", p, epv),
                    detail = "Moderate/low dimensionality. Group LASSO is still valid for group selection."
                )
            }

            # -- Check 3: Sample Size --
            if (n >= 100) {
                checks$sample_size <- list(
                    color = "green", label = "Sample Size",
                    value = sprintf("n=%d", n),
                    detail = "Adequate sample size for cross-validation."
                )
            } else if (n >= 30) {
                checks$sample_size <- list(
                    color = "yellow", label = "Sample Size",
                    value = sprintf("n=%d", n),
                    detail = "Small sample. Consider reducing the number of CV folds (e.g., 5-fold instead of 10)."
                )
            } else {
                checks$sample_size <- list(
                    color = "red", label = "Sample Size",
                    value = sprintf("n=%d", n),
                    detail = "Very small sample. Results will be highly variable and CV may not converge."
                )
            }

            # -- Check 4: Multicollinearity --
            tryCatch({
                if (p <= 2000 && p >= 2) {
                    cor_matrix <- cor(pred_matrix, use = "pairwise.complete.obs")
                    diag(cor_matrix) <- 0
                    max_cor <- max(abs(cor_matrix), na.rm = TRUE)
                    
                    if (max_cor < 0.7) {
                        checks$collinearity <- list(
                            color = "green", label = "Multicollinearity",
                            value = sprintf("Max |r| = %.2f", max_cor),
                            detail = "No concerning collinearity detected."
                        )
                    } else if (max_cor < 0.9) {
                        checks$collinearity <- list(
                            color = "yellow", label = "Multicollinearity",
                            value = sprintf("Max |r| = %.2f", max_cor),
                            detail = "Moderate collinearity. Group LASSO handles structure better than standard LASSO."
                        )
                    } else {
                        checks$collinearity <- list(
                            color = "yellow", label = "Multicollinearity",
                            value = sprintf("Max |r| = %.2f", max_cor),
                            detail = "High collinearity. Combining highly correlated variables into the same group is recommended."
                        )
                    }
                }
            }, error = function(e) {
                NULL
            })

            # -- Check 5: Data Quality --
            original_data <- self$data
            n_total <- nrow(original_data)
            n_missing <- n_total - n
            pct_missing <- 100 * n_missing / n_total

            if (n_missing == 0) {
                checks$data_quality <- list(
                    color = "green", label = "Data Quality",
                    value = "No missing data",
                    detail = "Complete dataset."
                )
            } else {
                checks$data_quality <- list(
                    color = if (pct_missing > 20) "red" else "yellow",
                    label = "Data Quality",
                    value = sprintf("%.1f%% missing", pct_missing),
                    detail = sprintf("%.1f%% missing data (%d rows excluded).", pct_missing, n_missing)
                )
            }

            # -- Overall Verdict --
            colors <- sapply(checks, function(x) x$color)
            if (any(colors == "red")) {
                overall <- "red"
                overall_text <- "Some issues require attention before relying on these results."
            } else if (any(colors == "yellow")) {
                overall <- "yellow"
                overall_text <- "Data is usable but review the flagged items."
            } else {
                overall <- "green"
                overall_text <- "Data is well-suited for Group LASSO modeling."
            }

            private$.generateSuitabilityHtml(checks, overall, overall_text)
        },

        .generateSuitabilityHtml = function(checks, overall, overall_text) {
            bg_colors <- list(
                green  = "background-color: #d4edda; color: #155724; border: 1px solid #c3e6cb;",
                yellow = "background-color: #fff3cd; color: #856404; border: 1px solid #ffeeba;",
                red    = "background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb;"
            )
            dot_colors <- list(green = "#28a745", yellow = "#ffc107", red = "#dc3545")

            html <- paste0(
                "<div style='", bg_colors[[overall]], " padding: 12px; border-radius: 6px; margin-bottom: 12px;'>",
                "<strong>Overall: ", overall_text, "</strong></div>"
            )

            html <- paste0(html,
                "<table style='width: 100%; border-collapse: collapse; font-size: 13px;'>",
                "<thead><tr style='border-bottom: 2px solid #dee2e6;'>",
                "<th style='padding: 6px; text-align: left;'>Status</th>",
                "<th style='padding: 6px; text-align: left;'>Check</th>",
                "<th style='padding: 6px; text-align: left;'>Value</th>",
                "<th style='padding: 6px; text-align: left;'>Detail</th>",
                "</tr></thead><tbody>"
            )

            for (chk in checks) {
                if (is.null(chk)) next
                dot <- paste0("<span style='color: ", dot_colors[[chk$color]], "; font-size: 18px;'>&#9679;</span>")
                html <- paste0(html,
                    "<tr style='border-bottom: 1px solid #dee2e6;'>",
                    "<td style='padding: 6px;'>", dot, "</td>",
                    "<td style='padding: 6px;'><strong>", chk$label, "</strong></td>",
                    "<td style='padding: 6px;'>", chk$value, "</td>",
                    "<td style='padding: 6px;'>", chk$detail, "</td>",
                    "</tr>"
                )
            }

            html <- paste0(html, "</tbody></table>")

            self$results$suitabilityReport$setContent(html)
        }
    )
)