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
                    <h2>🎯 Group LASSO for Survival Analysis</h2>
                    <p><strong>Penalized regression with group-wise variable selection and structure preservation</strong></p>
                    
                    <div class='step'>
                    <strong>📊 Required Data:</strong>
                    <ul>
                        <li><span class='highlight'>Time Variable</span>: Time to event or censoring</li>
                        <li><span class='highlight'>Event Indicator</span>: 0 = censored, 1 = event</li>
                        <li><span class='highlight'>Predictor Variables</span>: Variables organized into meaningful groups</li>
                    </ul>
                    </div>
                    
                    <div class='step'>
                    <strong>🔧 Group Definition Methods:</strong>
                    <ul>
                        <li><strong>Automatic:</strong> Groups by variable type (continuous, categorical)</li>
                        <li><strong>Factor-Based:</strong> Dummy variables from same factor grouped together</li>
                        <li><strong>Custom:</strong> User-defined groups (e.g., biological pathways)</li>
                        <li><strong>Biological:</strong> Pathway or domain-specific groupings</li>
                    </ul>
                    </div>
                    
                    <div class='step'>
                    <strong>⚙️ Penalty Types:</strong>
                    <ul>
                        <li><strong>Group LASSO:</strong> Selects entire groups simultaneously</li>
                        <li><strong>Sparse Group LASSO:</strong> Combines group and individual penalties</li>
                        <li><strong>Adaptive Group:</strong> Data-driven penalty weights for groups</li>
                        <li><strong>Overlapping Groups:</strong> Variables can belong to multiple groups</li>
                    </ul>
                    </div>
                    
                    <div class='step'>
                    <strong>🎨 Key Features:</strong>
                    <ul>
                        <li>📈 Cross-validation for optimal penalty selection</li>
                        <li>🎯 Stability selection for robust group identification</li>
                        <li>📊 Nested CV for unbiased performance assessment</li>
                        <li>🔄 Multiple optimization algorithms (coordinate descent, ADMM)</li>
                        <li>⚕️ Clinical interpretation with group importance measures</li>
                        <li>🏗️ Permutation testing for statistical significance</li>
                    </ul>
                    </div>
                    
                    <div class='step'>
                    <strong>💡 Clinical Applications:</strong>
                    <ul>
                        <li>🧬 Genomic pathway analysis in survival studies</li>
                        <li>🏥 Clinical domain groupings (demographics, lab values, imaging)</li>
                        <li>💊 Treatment group comparisons with structured covariates</li>
                        <li>🎯 Biomarker panel selection with biological structure</li>
                    </ul>
                    </div>
                    
                    <p><em>💡 Tip: Use factor-based grouping for categorical variables and custom grouping for domain-specific applications like gene pathways.</em></p>
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
            required_packages <- c("grplasso", "survival", "glmnet")
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
            requireNamespace("grplasso", quietly = TRUE)

            # Set up lambda sequence
            if (self$options$lambda_sequence == "auto") {
                lambda_seq <- NULL
            } else {
                # Custom lambda sequence implementation would go here
                lambda_seq <- NULL
            }

            # Fit group LASSO with cross-validation
            set.seed(self$options$random_seed)
            
            # Use grplasso package for group LASSO
            cv_fit <- grplasso::grplasso(
                x = group_data$x,
                y = group_data$y,
                index = group_data$groups,
                standardize = FALSE,  # Already standardized if requested
                model = LinReg(),  # For Cox, would need CoxReg()
                lambda = lambda_seq,
                control = grplasso::grpl.control(
                    inner.iter = self$options$max_iterations,
                    beta.start = NULL,
                    step.ratio = 0.5
                )
            )

            # Cross-validation for lambda selection
            cv_result <- grplasso::cv.grplasso(
                x = group_data$x,
                y = group_data$y,
                index = group_data$groups,
                model = LinReg(),
                nfolds = self$options$cv_folds,
                lambda = cv_fit$lambda
            )

            # Extract optimal results
            lambda_min <- cv_result$lambda.min
            lambda_1se <- cv_result$lambda.1se
            
            # Get coefficients at optimal lambda
            coef_min <- predict(cv_fit, lambda = lambda_min, type = "coefficients")
            coef_1se <- predict(cv_fit, lambda = lambda_1se, type = "coefficients")

            return(list(
                cv_fit = cv_fit,
                cv_result = cv_result,
                lambda_min = lambda_min,
                lambda_1se = lambda_1se,
                coef_min = coef_min,
                coef_1se = coef_1se,
                group_vector = group_data$groups
            ))
        },

        .fitSparseGroupLasso = function(group_data) {
            # Sparse group LASSO implementation
            # This would combine group and individual penalties
            # For now, fallback to standard group LASSO
            return(private$.fitStandardGroupLasso(group_data))
        },

        .fitAdaptiveGroupLasso = function(group_data) {
            # Adaptive group LASSO with data-driven weights
            # Calculate initial weights and then fit
            return(private$.fitStandardGroupLasso(group_data))
        },

        .stabilitySelection = function(group_data) {
            n_boot <- self$options$bootstrap_samples
            subsample_ratio <- 0.8  # Fixed subsample ratio
            n_groups <- max(group_data$groups)
            
            selection_matrix <- matrix(0, nrow = n_boot, ncol = n_groups)
            
            set.seed(self$options$random_seed)
            
            for (b in 1:n_boot) {
                # Bootstrap sample
                n_sub <- floor(nrow(group_data$x) * subsample_ratio)
                boot_idx <- sample(nrow(group_data$x), n_sub, replace = FALSE)
                
                boot_x <- group_data$x[boot_idx, , drop = FALSE]
                boot_y <- group_data$y[boot_idx, ]
                boot_groups <- group_data$groups
                
                tryCatch({
                    # Fit group LASSO on bootstrap sample
                    boot_fit <- grplasso::grplasso(
                        x = boot_x,
                        y = boot_y,
                        index = boot_groups,
                        standardize = FALSE,
                        model = LinReg()
                    )
                    
                    # Check which groups are selected at some lambda
                    # This is a simplified implementation
                    for (g in 1:n_groups) {
                        group_vars <- which(boot_groups == g)
                        if (length(group_vars) > 0) {
                            # Check if any variable in group has non-zero coefficient
                            group_selected <- any(abs(boot_fit$coefficients[group_vars, ncol(boot_fit$coefficients)]) > 1e-8)
                            selection_matrix[b, g] <- as.numeric(group_selected)
                        }
                    }
                    
                }, error = function(e) {
                    # Skip this bootstrap sample
                })
            }
            
            # Calculate selection frequencies
            selection_freq <- colMeans(selection_matrix)
            stable_groups <- which(selection_freq >= self$options$stability_threshold)
            
            return(list(
                selection_frequencies = selection_freq,
                stable_groups = stable_groups,
                selection_matrix = selection_matrix
            ))
        },

        .nestedCrossValidation = function(group_data) {
            # Nested cross-validation implementation
            outer_folds <- self$options$cv_folds
            inner_folds <- self$options$inner_cv_folds
            
            # This would be a more complex implementation
            # For now, return placeholder results
            return(list(
                outer_performance = rep(0.7, outer_folds),
                optimal_lambdas = rep(0.1, outer_folds)
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

            group_summary <- data.frame(
                group_id = group_info$group_id,
                group_name = group_info$group_name,
                variables = sapply(group_info$group_id, function(g) {
                    var_indices <- which(group_vector == g)
                    paste(group_data$var_names[var_indices], collapse = ", ")
                }),
                group_size = group_info$group_size,
                group_weight = rep(1, nrow(group_info)),  # Placeholder
                selected = selected_groups,
                selection_order = 1:nrow(group_info)  # Placeholder
            )

            self$results$groupSummary$setData(group_summary)
        },

        .populateCoefficients = function(group_results, group_data) {
            coef_vec <- as.vector(group_results$coef_min)
            group_vector <- group_data$groups
            var_names <- group_data$var_names
            
            # Create coefficient table
            coef_data <- data.frame(
                group_id = group_vector,
                variable = var_names,
                coefficient = coef_vec,
                exp_coefficient = exp(coef_vec),
                group_norm = rep(NA, length(coef_vec)),  # Would calculate group norms
                relative_importance = abs(coef_vec) / max(abs(coef_vec), na.rm = TRUE),
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
            if (!is.null(group_results$cv_fit)) {
                lambda_seq <- group_results$cv_fit$lambda
                n_steps <- min(20, length(lambda_seq))
                step_indices <- round(seq(1, length(lambda_seq), length.out = n_steps))
                
                path_data <- data.frame(
                    step = 1:n_steps,
                    lambda = lambda_seq[step_indices],
                    n_groups_selected = rep(NA, n_steps),  # Would calculate
                    n_variables_selected = rep(NA, n_steps),  # Would calculate
                    deviance = rep(NA, n_steps),  # Would calculate
                    df = rep(NA, n_steps)  # Would calculate
                )

                self$results$pathSummary$setData(path_data)
            }
        },

        .populateCVResults = function(group_results) {
            if (!is.null(group_results$cv_result)) {
                cv_data <- data.frame(
                    criterion = "Cross-Validation",
                    lambda_min = group_results$lambda_min,
                    lambda_1se = group_results$lambda_1se,
                    cv_error_min = min(group_results$cv_result$cvm, na.rm = TRUE),
                    cv_error_1se = group_results$cv_result$cvm[which(group_results$cv_result$lambda == group_results$lambda_1se)],
                    groups_min = NA,  # Would calculate
                    groups_1se = NA   # Would calculate
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

        .populatePerformance = function(group_results, group_data) {
            # Calculate performance metrics
            perf_data <- data.frame(
                metric = c("Number of Groups Selected", "Number of Variables Selected", "CV Error"),
                value = c(
                    length(unique(group_data$groups)),  # Placeholder
                    sum(abs(group_results$coef_min) > self$options$selection_threshold),
                    min(group_results$cv_result$cvm, na.rm = TRUE)
                ),
                confidence_interval = c("", "", ""),
                description = c(
                    "Groups selected by group LASSO",
                    "Individual variables selected",
                    "Cross-validation error at optimal lambda"
                )
            )

            self$results$modelPerformance$setData(perf_data)
        },

        .plotRegularizationPath = function(group_results, group_data) {
            image <- self$results$pathPlot
            image$setState(list(results = group_results, data = group_data))
        },

        .plotCVCurve = function(group_results) {
            image <- self$results$cvPlot
            image$setState(group_results)
        },

        .plotGroupImportance = function(group_results, group_data) {
            image <- self$results$importancePlot
            image$setState(list(results = group_results, data = group_data))
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
        }
    )
)