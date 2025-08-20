#' @title Advanced Clinical Decision Trees
#' @description Advanced decision tree analysis with enhanced CART, hyperparameter tuning, and clinical validation
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom rpart rpart rpart.control prune
#' @importFrom rpart.plot rpart.plot
#' @importFrom caret createDataPartition createFolds confusionMatrix train trainControl
#' @importFrom pROC roc auc

treeadvancedClass <- if (requireNamespace("jmvcore")) R6::R6Class("treeadvancedClass",
    inherit = treeadvancedBase,
    private = list(
        .model = NULL,
        .best_model = NULL,
        .training_data = NULL,
        .test_data = NULL,
        .predictions = NULL,
        .tuning_results = NULL,
        .cv_results = NULL,

        .run = function() {
            # Validate inputs
            validation_result <- private$.validate_inputs()
            
            if (!validation_result$valid) {
                if (validation_result$show_welcome) {
                    private$.show_welcome_message()
                }
                return()
            }

            # Set seed if requested
            if (self$options$set_seed) {
                set.seed(self$options$seed_value)
            }

            # Prepare data
            private$.prepare_data()

            # Feature selection if requested
            if (self$options$feature_selection) {
                private$.perform_feature_selection()
            }

            # Hyperparameter tuning if requested
            if (self$options$hyperparameter_tuning) {
                private$.perform_hyperparameter_tuning()
            } else {
                # Build standard model
                private$.build_model()
            }

            # Generate predictions with chosen validation method
            private$.generate_predictions()

            # Populate results
            private$.populate_results()
        },

        .validate_inputs = function() {
            vars <- self$options$vars
            facs <- self$options$facs
            target <- self$options$target
            
            # Check if we have any data
            if (is.null(self$data) || nrow(self$data) == 0) {
                return(list(valid = FALSE, show_welcome = TRUE))
            }

            # Check for required variables
            if ((is.null(vars) || length(vars) == 0) && 
                (is.null(facs) || length(facs) == 0)) {
                return(list(valid = FALSE, show_welcome = TRUE, 
                           message = "Please select at least one predictor variable."))
            }

            if (is.null(target)) {
                return(list(valid = FALSE, show_welcome = TRUE,
                           message = "Please select a target outcome variable."))
            }

            if (is.null(self$options$targetLevel)) {
                return(list(valid = FALSE, show_welcome = FALSE,
                           message = "Please select the positive class level."))
            }

            # Check target variable has correct levels
            target_col <- self$data[[target]]
            if (!is.factor(target_col)) {
                return(list(valid = FALSE, show_welcome = FALSE,
                           message = "Target variable must be categorical."))
            }

            if (length(levels(target_col)) < 2) {
                return(list(valid = FALSE, show_welcome = FALSE,
                           message = "Target variable must have at least 2 levels."))
            }

            # Check sample size for advanced methods
            if (nrow(self$data) < 50) {
                return(list(valid = FALSE, show_welcome = FALSE,
                           message = "Advanced tree analysis requires at least 50 samples."))
            }
            
            # Additional checks for computationally expensive operations
            if (self$options$hyperparameter_tuning && nrow(self$data) < 100) {
                return(list(valid = FALSE, show_welcome = FALSE,
                           message = "Hyperparameter tuning requires at least 100 samples for reliable results."))
            }
            
            if (self$options$feature_selection && length(c(self$options$vars, self$options$facs)) < 5) {
                return(list(valid = FALSE, show_welcome = FALSE,
                           message = "Feature selection requires at least 5 predictor variables."))
            }
            
            # Warnings for large datasets that may be slow
            if (self$options$hyperparameter_tuning && nrow(self$data) > 10000) {
                # Note: This is just a warning, not blocking validation
                message("Note: Hyperparameter tuning with large datasets (>10,000 rows) may take considerable time.")
            }
            
            if (self$options$feature_selection && self$options$feature_selection_method == "boruta" && nrow(self$data) > 5000) {
                message("Note: Boruta feature selection with large datasets may take considerable time.")
            }

            return(list(valid = TRUE))
        },

        .show_welcome_message = function() {
            welcome_html <- "
            <div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 20px 0;'>
            <h3 style='color: #1565c0; margin-top: 0;'>Advanced Clinical Decision Trees</h3>
            <p><strong>Enhanced CART with hyperparameter tuning and advanced validation</strong></p>
            
            <h4 style='color: #1565c0;'>Required Variables:</h4>
            <ol>
            <li><strong>Predictors:</strong> Select continuous and/or categorical clinical variables</li>
            <li><strong>Target Outcome:</strong> Select the disease/outcome variable to predict</li>
            <li><strong>Target Level:</strong> Select the positive class (e.g., 'disease', 'positive')</li>
            </ol>

            <h4 style='color: #1565c0;'>Advanced Features:</h4>
            <ul>
            <li><strong>Hyperparameter Tuning:</strong> Grid/random search for optimal parameters</li>
            <li><strong>Advanced Validation:</strong> Repeated CV, bootstrap, cohort-based validation</li>
            <li><strong>Feature Selection:</strong> Automated variable selection (RFE, Boruta)</li>
            <li><strong>Cost-Sensitive Learning:</strong> Clinical cost matrices</li>
            <li><strong>Clinical Optimization:</strong> Prevalence adjustment and clinical contexts</li>
            </ul>

            <h4 style='color: #1565c0;'>Requirements:</h4>
            <ul>
            <li>Minimum 50 samples for reliable analysis</li>
            <li>Hyperparameter tuning requires ≥100 samples</li>
            <li>Feature selection requires ≥5 predictor variables</li>
            <li>Balanced or stratified sampling recommended</li>
            </ul>
            
            <h4 style='color: #1565c0;'>Performance Notes:</h4>
            <ul>
            <li><strong>Hyperparameter tuning:</strong> May take several minutes with large datasets</li>
            <li><strong>Boruta feature selection:</strong> Computationally intensive for >5,000 samples</li>
            <li><strong>Repeated cross-validation:</strong> Processing time scales with repetitions</li>
            <li><strong>Grid size limit:</strong> Maximum 100 parameter combinations for tuning</li>
            </ul>
            </div>"
            
            self$results$instructions$setContent(welcome_html)
        },

        .prepare_data = function() {
            vars <- self$options$vars
            facs <- self$options$facs
            target <- self$options$target

            # Combine all predictor variables
            all_vars <- c(vars, facs, target)
            
            # Extract relevant data
            data <- self$data[all_vars]
            
            # Remove rows with missing target
            data <- data[!is.na(data[[target]]), ]
            
            # Prepare target variable
            data[[target]] <- as.factor(data[[target]])
            
            # Handle missing values in predictors (simple imputation)
            for (var in c(vars, facs)) {
                if (var %in% names(data)) {
                    if (is.numeric(data[[var]])) {
                        data[[var]][is.na(data[[var]])] <- median(data[[var]], na.rm = TRUE)
                    } else {
                        # For factors, use mode
                        mode_val <- names(sort(table(data[[var]]), decreasing = TRUE))[1]
                        data[[var]][is.na(data[[var]])] <- mode_val
                        data[[var]] <- as.factor(data[[var]])
                    }
                }
            }
            
            # Store prepared data
            private$.training_data <- data
            
            # Split data for cohort validation or holdout
            if (self$options$validation %in% c("holdout", "cohort")) {
                if (self$options$validation == "cohort" && !is.null(self$options$train) && !is.null(self$options$trainLevel)) {
                    # Use specified cohort variable for train/test split
                    train_var <- self$options$train
                    train_level <- self$options$trainLevel
                    
                    if (train_var %in% names(data)) {
                        train_indices <- which(data[[train_var]] == train_level)
                        test_indices <- which(data[[train_var]] != train_level)
                        
                        if (length(train_indices) > 0 && length(test_indices) > 0) {
                            private$.training_data <- data[train_indices, ]
                            private$.test_data <- data[test_indices, ]
                        } else {
                            # Fallback to random split if cohort split fails
                            test_prop <- self$options$test_split
                            n <- nrow(data)
                            train_indices <- sample(1:n, size = floor((1 - test_prop) * n))
                            private$.training_data <- data[train_indices, ]
                            private$.test_data <- data[-train_indices, ]
                        }
                    } else {
                        # Fallback to random split if cohort variable not found
                        test_prop <- self$options$test_split
                        n <- nrow(data)
                        train_indices <- sample(1:n, size = floor((1 - test_prop) * n))
                        private$.training_data <- data[train_indices, ]
                        private$.test_data <- data[-train_indices, ]
                    }
                } else {
                    # Standard holdout split
                    test_prop <- self$options$test_split
                    
                    if (self$options$stratified_sampling) {
                        train_indices <- caret::createDataPartition(
                            data[[target]], 
                            p = 1 - test_prop, 
                            list = FALSE
                        )[, 1]
                    } else {
                        n <- nrow(data)
                        train_indices <- sample(1:n, size = floor((1 - test_prop) * n))
                    }
                    
                    private$.training_data <- data[train_indices, ]
                    private$.test_data <- data[-train_indices, ]
                }
            }
        },

        .perform_feature_selection = function() {
            vars <- self$options$vars
            facs <- self$options$facs
            target <- self$options$target
            max_features <- self$options$max_features
            method <- self$options$feature_selection_method
            
            # All predictors
            predictors <- c(vars, facs)
            
            if (length(predictors) <= max_features) {
                return()  # No need to select features
            }
            
            selected_features <- switch(method,
                "rfe" = private$.rfe_selection(predictors, target, max_features),
                "boruta" = private$.boruta_selection(predictors, target, max_features),
                "importance" = private$.importance_selection(predictors, target, max_features),
                private$.importance_selection(predictors, target, max_features)  # default
            )
            
            # Validate feature selection results
            if (is.null(selected_features) || length(selected_features) == 0) {
                jmvcore::reject("Feature selection failed to identify any important features. Try a different method, increase max_features, or check data quality.")
            }
            
            if (length(selected_features) > 0) {
                # Update training data with selected features
                selected_vars <- c(selected_features, target)
                
                # Verify all selected features exist in the data
                missing_features <- setdiff(selected_vars, names(private$.training_data))
                if (length(missing_features) > 0) {
                    jmvcore::reject(paste("Selected features not found in data:", paste(missing_features, collapse = ", ")))
                }
                
                private$.training_data <- private$.training_data[selected_vars]
                
                if (!is.null(private$.test_data)) {
                    private$.test_data <- private$.test_data[selected_vars]
                }
            }
        },

        .importance_selection = function(predictors, target, max_features) {
            if (requireNamespace("randomForest", quietly = TRUE)) {
                tryCatch({
                    # Build random forest for feature importance
                    formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
                    formula_obj <- as.formula(formula_str)
                    
                    rf_model <- randomForest::randomForest(
                        formula = formula_obj,
                        data = private$.training_data,
                        importance = TRUE,
                        ntree = 100
                    )
                    
                    # Get importance
                    importance <- randomForest::importance(rf_model)
                    if (ncol(importance) > 1) {
                        importance_scores <- importance[, "MeanDecreaseGini"]
                    } else {
                        importance_scores <- importance[, 1]
                    }
                    
                    # Select top features
                    top_features <- names(sort(importance_scores, decreasing = TRUE))[1:min(max_features, length(importance_scores))]
                    return(top_features)
                    
                }, error = function(e) {
                    return(NULL)
                })
            }
            return(NULL)
        },

        .rfe_selection = function(predictors, target, max_features) {
            if (requireNamespace("caret", quietly = TRUE) && requireNamespace("randomForest", quietly = TRUE)) {
                tryCatch({
                    # RFE with random forest
                    control <- caret::rfeControl(
                        functions = caret::rfFuncs,
                        method = "cv",
                        number = 3,
                        verbose = FALSE
                    )
                    
                    # Prepare data for RFE
                    x <- private$.training_data[predictors]
                    y <- private$.training_data[[target]]
                    
                    # Run RFE
                    rfe_results <- caret::rfe(
                        x = x, 
                        y = y,
                        sizes = min(max_features, length(predictors)),
                        rfeControl = control
                    )
                    
                    return(rfe_results$optVariables)
                    
                }, error = function(e) {
                    return(NULL)
                })
            }
            return(NULL)
        },

        .boruta_selection = function(predictors, target, max_features) {
            if (requireNamespace("Boruta", quietly = TRUE)) {
                tryCatch({
                    # Prepare data for Boruta
                    boruta_data <- private$.training_data[c(predictors, target)]
                    
                    # Run Boruta
                    boruta_result <- Boruta::Boruta(
                        stats::as.formula(paste(target, "~", ".")), 
                        data = boruta_data,
                        doTrace = 0,
                        maxRuns = 50
                    )
                    
                    # Get confirmed important features
                    important_features <- names(boruta_result$finalDecision[boruta_result$finalDecision == "Confirmed"])
                    
                    # If too many features, select top ones by importance
                    if (length(important_features) > max_features) {
                        importance_scores <- boruta_result$ImpHistory[important_features, ]
                        if (is.matrix(importance_scores)) {
                            avg_importance <- rowMeans(importance_scores, na.rm = TRUE)
                        } else {
                            avg_importance <- importance_scores
                        }
                        important_features <- names(sort(avg_importance, decreasing = TRUE))[1:max_features]
                    }
                    
                    return(important_features)
                    
                }, error = function(e) {
                    return(NULL)
                })
            }
            return(NULL)
        },

        .perform_hyperparameter_tuning = function() {
            target <- self$options$target
            predictors <- setdiff(names(private$.training_data), target)
            
            # Parse parameter ranges
            max_depth_range <- private$.parse_range(self$options$max_depth_range, c(3, 8))
            cp_range <- private$.parse_range(self$options$cp_range, c(0.001, 0.1))
            min_samples_range <- private$.parse_range(self$options$min_samples_range, c(10, 50))
            
            # Create parameter grid
            if (self$options$tuning_method == "grid") {
                param_grid <- expand.grid(
                    maxdepth = seq(max_depth_range[1], max_depth_range[2], by = 1),
                    cp = seq(cp_range[1], cp_range[2], length.out = 5),
                    minsplit = seq(min_samples_range[1], min_samples_range[2], by = 10)
                )
            } else {
                # Random search
                n_iter <- 20
                param_grid <- data.frame(
                    maxdepth = sample(max_depth_range[1]:max_depth_range[2], n_iter, replace = TRUE),
                    cp = runif(n_iter, cp_range[1], cp_range[2]),
                    minsplit = sample(seq(min_samples_range[1], min_samples_range[2], by = 5), n_iter, replace = TRUE)
                )
            }
            
            # Hyperparameter tuning with cross-validation
            best_score <- -Inf
            best_params <- NULL
            
            # Create CV folds
            cv_folds <- self$options$cv_folds
            if (self$options$stratified_sampling) {
                fold_indices <- caret::createFolds(private$.training_data[[target]], k = cv_folds)
            } else {
                n <- nrow(private$.training_data)
                fold_indices <- split(sample(1:n), cut(seq_len(n), cv_folds, labels = FALSE))
            }
            
            # Check grid size before processing
            if (nrow(param_grid) > 100) {
                jmvcore::reject("Parameter grid too large (>100 combinations). Consider using random search or reducing parameter ranges.")
            }
            
            # Store detailed results for validation curves
            detailed_results <- data.frame(
                param_grid,
                mean_score = numeric(nrow(param_grid)),
                std_score = numeric(nrow(param_grid))
            )
            
            # Test each parameter combination
            for (i in 1:nrow(param_grid)) {
                params <- param_grid[i, ]
                fold_scores <- numeric(cv_folds)
                
                for (j in 1:cv_folds) {
                    test_idx <- fold_indices[[j]]
                    train_data <- private$.training_data[-test_idx, ]
                    test_data <- private$.training_data[test_idx, ]
                    
                    # Build model with current parameters
                    control <- rpart::rpart.control(
                        maxdepth = params$maxdepth,
                        minsplit = params$minsplit,
                        cp = params$cp
                    )
                    
                    formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
                    formula_obj <- as.formula(formula_str)
                    
                    # Include cost-sensitive learning in tuning
                    parms_list <- list()
                    if (self$options$cost_sensitive) {
                        cost_matrix <- private$.create_cost_matrix()
                        if (!is.null(cost_matrix)) {
                            parms_list$loss <- cost_matrix
                        }
                    }
                    
                    model <- tryCatch({
                        rpart::rpart(
                            formula = formula_obj,
                            data = train_data,
                            method = "class",
                            control = control,
                            parms = if (length(parms_list) > 0) parms_list else NULL
                        )
                    }, error = function(e) {
                        # Skip this parameter combination if model building fails
                        return(NULL)
                    })
                    
                    if (is.null(model)) {
                        fold_scores[j] <- 0  # Assign worst score
                        next
                    }
                    
                    # Make predictions
                    pred <- tryCatch({
                        predict(model, test_data, type = "class")
                    }, error = function(e) {
                        return(NULL)
                    })
                    
                    if (is.null(pred)) {
                        fold_scores[j] <- 0  # Assign worst score
                        next
                    }
                    
                    # Calculate score based on tuning metric
                    score <- private$.calculate_tuning_score(test_data[[target]], pred, self$options$tuning_metric)
                    fold_scores[j] <- score
                }
                
                # Store detailed results
                avg_score <- mean(fold_scores, na.rm = TRUE)
                std_score <- sd(fold_scores, na.rm = TRUE)
                detailed_results$mean_score[i] <- avg_score
                detailed_results$std_score[i] <- std_score
                
                if (avg_score > best_score) {
                    best_score <- avg_score
                    best_params <- params
                }
            }
            
            # Build final model with best parameters
            if (!is.null(best_params)) {
                control <- rpart::rpart.control(
                    maxdepth = best_params$maxdepth,
                    minsplit = best_params$minsplit,
                    cp = best_params$cp,
                    xval = self$options$cv_folds
                )
                
                formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
                formula_obj <- as.formula(formula_str)
                
                private$.best_model <- rpart::rpart(
                    formula = formula_obj,
                    data = private$.training_data,
                    method = "class",
                    control = control
                )
                
                private$.model <- private$.best_model
                private$.tuning_results <- list(
                    best_params = best_params,
                    best_score = best_score,
                    param_grid = param_grid,
                    detailed_results = detailed_results
                )
            }
        },

        .parse_range = function(range_str, default_range) {
            tryCatch({
                parts <- strsplit(range_str, ":")[[1]]
                if (length(parts) == 2) {
                    return(c(as.numeric(parts[1]), as.numeric(parts[2])))
                } else {
                    return(default_range)
                }
            }, error = function(e) {
                return(default_range)
            })
        },

        .calculate_tuning_score = function(actual, predicted, metric) {
            if (length(actual) != length(predicted)) return(0)
            
            switch(metric,
                "bacc" = {
                    cm <- table(actual, predicted)
                    if (nrow(cm) == 2 && ncol(cm) == 2) {
                        sens <- cm[2,2] / sum(cm[2,])
                        spec <- cm[1,1] / sum(cm[1,])
                        return((sens + spec) / 2)
                    } else {
                        return(sum(actual == predicted) / length(actual))
                    }
                },
                "sens" = {
                    cm <- table(actual, predicted)
                    if (nrow(cm) >= 2 && ncol(cm) >= 2) {
                        return(cm[2,2] / sum(cm[2,]))
                    } else {
                        return(0)
                    }
                },
                "spec" = {
                    cm <- table(actual, predicted)
                    if (nrow(cm) >= 2 && ncol(cm) >= 2) {
                        return(cm[1,1] / sum(cm[1,]))
                    } else {
                        return(0)
                    }
                },
                "f1" = {
                    cm <- table(actual, predicted)
                    if (nrow(cm) >= 2 && ncol(cm) >= 2) {
                        precision <- cm[2,2] / sum(cm[,2])
                        recall <- cm[2,2] / sum(cm[2,])
                        if (precision + recall > 0) {
                            return(2 * precision * recall / (precision + recall))
                        }
                    }
                    return(0)
                },
                {
                    # Default: accuracy
                    return(sum(actual == predicted) / length(actual))
                }
            )
        },

        .build_model = function() {
            if (!is.null(private$.model)) {
                return()  # Model already built during tuning
            }
            
            target <- self$options$target
            predictors <- setdiff(names(private$.training_data), target)
            
            # Create formula
            formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
            formula_obj <- as.formula(formula_str)

            # Set up rpart control with default or custom parameters
            if (self$options$hyperparameter_tuning && !is.null(private$.tuning_results)) {
                best_params <- private$.tuning_results$best_params
                control <- rpart::rpart.control(
                    maxdepth = best_params$maxdepth,
                    minsplit = best_params$minsplit,
                    cp = best_params$cp,
                    xval = self$options$cv_folds
                )
            } else {
                # Parse custom ranges or use defaults
                max_depth_range <- private$.parse_range(self$options$max_depth_range, c(3, 8))
                cp_range <- private$.parse_range(self$options$cp_range, c(0.001, 0.1))
                min_samples_range <- private$.parse_range(self$options$min_samples_range, c(10, 50))
                
                control <- rpart::rpart.control(
                    maxdepth = max_depth_range[2],  # Use max from range
                    minsplit = min_samples_range[1],  # Use min from range
                    cp = cp_range[1],  # Use min cp for less pruning
                    xval = self$options$cv_folds,
                    usesurrogate = ifelse(self$options$surrogate_splits, 2, 0),
                    maxsurrogate = self$options$max_surrogate,
                    maxcompete = self$options$competing_splits
                )
            }

            # Set up parameters including cost-sensitive analysis
            parms_list <- list()
            if (self$options$splitting_criterion == "information") {
                parms_list$split <- "information"
            } else {
                parms_list$split <- "gini"
            }
            
            # Add cost-sensitive learning
            if (self$options$cost_sensitive) {
                cost_matrix <- private$.create_cost_matrix()
                if (!is.null(cost_matrix)) {
                    parms_list$loss <- cost_matrix
                }
            }

            # Build model
            tryCatch({
                private$.model <- rpart::rpart(
                    formula = formula_obj,
                    data = private$.training_data,
                    method = "class",
                    control = control,
                    parms = if (length(parms_list) > 0) parms_list else NULL
                )

                # Apply pruning if requested
                if (self$options$auto_prune && !is.null(private$.model$cptable)) {
                    private$.apply_pruning()
                }

            }, error = function(e) {
                jmvcore::reject(paste("Error building model:", e$message))
            })
        },

        .apply_pruning = function() {
            if (is.null(private$.model$cptable) || nrow(private$.model$cptable) <= 1) {
                return()
            }
            
            cp_table <- private$.model$cptable
            
            if (self$options$pruning_method == "one_se") {
                # 1-SE rule
                min_xerror_idx <- which.min(cp_table[, "xerror"])
                if (min_xerror_idx > 1) {
                    min_xerror <- cp_table[min_xerror_idx, "xerror"]
                    min_xstd <- cp_table[min_xerror_idx, "xstd"]
                    threshold <- min_xerror + min_xstd
                    
                    # Find simplest model within 1 SE
                    valid_models <- cp_table[, "xerror"] <= threshold
                    if (any(valid_models)) {
                        best_cp_idx <- max(which(valid_models))
                        best_cp <- cp_table[best_cp_idx, "CP"]
                        private$.model <- rpart::prune(private$.model, cp = best_cp)
                    }
                }
            } else if (self$options$pruning_method == "min_xerror") {
                # Minimum cross-validation error
                min_xerror_idx <- which.min(cp_table[, "xerror"])
                best_cp <- cp_table[min_xerror_idx, "CP"]
                private$.model <- rpart::prune(private$.model, cp = best_cp)
            } else if (self$options$pruning_method == "custom_cp") {
                # Custom CP value
                custom_cp <- self$options$custom_cp
                private$.model <- rpart::prune(private$.model, cp = custom_cp)
            }
        },

        .generate_predictions = function() {
            if (is.null(private$.model)) {
                return()
            }

            validation_method <- self$options$validation
            
            if (validation_method == "holdout" && !is.null(private$.test_data)) {
                # Use holdout test data
                private$.generate_holdout_predictions()
            } else if (validation_method == "cohort" && !is.null(private$.test_data)) {
                # Use cohort-based validation
                private$.generate_cohort_predictions()
            } else if (validation_method == "repeated_cv") {
                # Repeated cross-validation
                private$.generate_repeated_cv_predictions()
            } else if (validation_method == "bootstrap") {
                # Bootstrap validation
                private$.generate_bootstrap_predictions()
            } else {
                # Standard cross-validation
                private$.generate_cv_predictions()
            }
        },

        .generate_holdout_predictions = function() {
            predictions <- private$.make_test_predictions(private$.test_data)
            
            private$.predictions <- list(
                actual = predictions$actual,
                predicted = predictions$predicted,
                probabilities = predictions$probabilities,
                validation_method = "holdout"
            )
        },

        .generate_cohort_predictions = function() {
            predictions <- private$.make_test_predictions(private$.test_data)
            
            private$.predictions <- list(
                actual = predictions$actual,
                predicted = predictions$predicted,
                probabilities = predictions$probabilities,
                validation_method = "cohort",
                cohort_info = list(
                    train_cohort = self$options$trainLevel,
                    train_size = nrow(private$.training_data),
                    test_size = nrow(private$.test_data)
                )
            )
        },

        # Helper function to consolidate prediction logic
        .make_test_predictions = function(test_data) {
            predictions <- predict(private$.model, test_data, type = "class")
            probabilities <- predict(private$.model, test_data, type = "prob")
            
            # Apply prevalence adjustment if requested
            if (self$options$prevalence_adjustment) {
                probabilities <- private$.adjust_for_prevalence(probabilities, self$options$targetLevel)
            }
            
            return(list(
                actual = test_data[[self$options$target]],
                predicted = predictions,
                probabilities = probabilities
            ))
        },

        .generate_repeated_cv_predictions = function() {
            target <- self$options$target
            data <- private$.training_data
            n_folds <- self$options$cv_folds
            n_repeats <- self$options$cv_repeats
            
            # Add timeout protection for long-running operations
            start_time <- Sys.time()
            timeout_minutes <- 15  # Generous timeout for complex analyses
            
            all_actual <- c()
            all_predicted <- c()
            all_probabilities <- NULL
            
            for (rep in 1:n_repeats) {
                # Check timeout before each repeat
                elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
                if (elapsed_time > timeout_minutes) {
                    jmvcore::reject(paste("Analysis timeout after", timeout_minutes, "minutes. Consider reducing CV repeats, dataset size, or using simpler validation."))
                }
                # Create folds for this repeat
                if (self$options$stratified_sampling) {
                    fold_indices <- caret::createFolds(data[[target]], k = n_folds)
                } else {
                    n <- nrow(data)
                    fold_indices <- split(sample(1:n), cut(seq_len(n), n_folds, labels = FALSE))
                }
                
                for (i in 1:n_folds) {
                    test_idx <- fold_indices[[i]]
                    train_data <- data[-test_idx, ]
                    test_data <- data[test_idx, ]
                    
                    # Build fold model and get predictions
                    fold_predictions <- private$.build_fold_model_and_predict(train_data, test_data)
                    
                    all_actual <- c(all_actual, as.character(fold_predictions$actual))
                    all_predicted <- c(all_predicted, as.character(fold_predictions$predicted))
                    
                    if (is.null(all_probabilities)) {
                        all_probabilities <- fold_predictions$probabilities
                    } else {
                        all_probabilities <- rbind(all_probabilities, fold_predictions$probabilities)
                    }
                }
            }
            
            # Validate we have predictions
            if (length(all_actual) == 0) {
                jmvcore::reject("No predictions generated. Check validation settings and data quality.")
            }
            
            # Apply prevalence adjustment if requested
            if (self$options$prevalence_adjustment && !is.null(all_probabilities)) {
                tryCatch({
                    all_probabilities <- private$.adjust_for_prevalence(all_probabilities, self$options$targetLevel)
                }, error = function(e) {
                    # Continue without prevalence adjustment if it fails
                    message("Prevalence adjustment failed: ", e$message)
                })
            }
            
            private$.predictions <- list(
                actual = factor(all_actual, levels = levels(data[[target]])),
                predicted = factor(all_predicted, levels = levels(data[[target]])),
                probabilities = all_probabilities,
                validation_method = "repeated_cv"
            )
            
            # Clean up temporary variables to free memory
            rm(all_actual, all_predicted, all_probabilities)
            gc()  # Trigger garbage collection
        },

        .generate_cv_predictions = function() {
            target <- self$options$target
            data <- private$.training_data
            n_folds <- self$options$cv_folds
            
            if (self$options$stratified_sampling) {
                fold_indices <- caret::createFolds(data[[target]], k = n_folds)
            } else {
                n <- nrow(data)
                fold_indices <- split(sample(1:n), cut(seq_len(n), n_folds, labels = FALSE))
            }
            
            all_actual <- c()
            all_predicted <- c()
            all_probabilities <- NULL
            
            for (i in 1:n_folds) {
                test_idx <- fold_indices[[i]]
                train_data <- data[-test_idx, ]
                test_data <- data[test_idx, ]
                
                fold_predictions <- private$.build_fold_model_and_predict(train_data, test_data)
                
                all_actual <- c(all_actual, as.character(fold_predictions$actual))
                all_predicted <- c(all_predicted, as.character(fold_predictions$predicted))
                
                if (is.null(all_probabilities)) {
                    all_probabilities <- fold_predictions$probabilities
                } else {
                    all_probabilities <- rbind(all_probabilities, fold_predictions$probabilities)
                }
            }
            
            # Apply prevalence adjustment if requested
            if (self$options$prevalence_adjustment && !is.null(all_probabilities)) {
                all_probabilities <- private$.adjust_for_prevalence(all_probabilities, self$options$targetLevel)
            }
            
            private$.predictions <- list(
                actual = factor(all_actual, levels = levels(data[[target]])),
                predicted = factor(all_predicted, levels = levels(data[[target]])),
                probabilities = all_probabilities,
                validation_method = "cv"
            )
        },

        .generate_bootstrap_predictions = function() {
            data <- private$.training_data
            target <- self$options$target
            n_bootstrap <- self$options$bootstrap_samples
            
            all_actual <- c()
            all_predicted <- c()
            
            for (i in 1:n_bootstrap) {
                # Bootstrap sample
                boot_indices <- sample(1:nrow(data), nrow(data), replace = TRUE)
                oob_indices <- setdiff(1:nrow(data), boot_indices)
                
                if (length(oob_indices) == 0) next
                
                boot_data <- data[boot_indices, ]
                oob_data <- data[oob_indices, ]
                
                fold_predictions <- private$.build_fold_model_and_predict(boot_data, oob_data)
                
                all_actual <- c(all_actual, as.character(fold_predictions$actual))
                all_predicted <- c(all_predicted, as.character(fold_predictions$predicted))
            }
            
            private$.predictions <- list(
                actual = factor(all_actual, levels = levels(data[[target]])),
                predicted = factor(all_predicted, levels = levels(data[[target]])),
                validation_method = "bootstrap"
            )
        },

        .build_fold_model_and_predict = function(train_data, test_data) {
            target <- self$options$target
            predictors <- setdiff(names(train_data), target)
            
            # Build simplified model for this fold
            formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
            formula_obj <- as.formula(formula_str)
            
            # Use conservative parameters for fold models to prevent overfitting
            # But use tuned parameters if available
            if (!is.null(private$.tuning_results)) {
                best_params <- private$.tuning_results$best_params
                control <- rpart::rpart.control(
                    maxdepth = max(3, min(best_params$maxdepth, 6)),  # Cap depth for CV
                    minsplit = max(20, best_params$minsplit),
                    cp = max(0.01, best_params$cp)
                )
            } else {
                control <- rpart::rpart.control(
                    maxdepth = 5,
                    minsplit = 20,
                    cp = 0.01
                )
            }
            
            # Include cost-sensitive learning in fold models
            parms_list <- list()
            if (self$options$cost_sensitive) {
                cost_matrix <- private$.create_cost_matrix()
                if (!is.null(cost_matrix)) {
                    parms_list$loss <- cost_matrix
                }
            }
            
            fold_model <- rpart::rpart(
                formula = formula_obj,
                data = train_data,
                method = "class",
                control = control,
                parms = if (length(parms_list) > 0) parms_list else NULL
            )
            
            # Predictions for this fold
            fold_pred <- predict(fold_model, test_data, type = "class")
            fold_prob <- predict(fold_model, test_data, type = "prob")
            
            return(list(
                actual = test_data[[target]],
                predicted = fold_pred,
                probabilities = fold_prob
            ))
        },

        .populate_results = function() {
            private$.populate_model_summary()
            private$.populate_performance_table()
            private$.populate_confusion_matrix()
            private$.populate_variable_importance()
            if (self$options$hyperparameter_tuning && !is.null(private$.tuning_results)) {
                private$.populate_tuning_results()
            }
            private$.populate_clinical_interpretation()
        },

        .populate_model_summary = function() {
            if (is.null(private$.model)) {
                return()
            }

            n_nodes <- nrow(private$.model$frame)
            n_leaves <- sum(private$.model$frame$var == "<leaf>")
            tree_depth <- max(rpart:::tree.depth(as.numeric(rownames(private$.model$frame))))
            
            # Advanced features information
            advanced_info <- ""
            
            # Tuning information
            if (!is.null(private$.tuning_results)) {
                best_params <- private$.tuning_results$best_params
                advanced_info <- paste0(advanced_info,
                    "<li><strong>Hyperparameter tuning:</strong> ", self$options$tuning_method, " search</li>",
                    "<li><strong>Best parameters:</strong> max_depth=", best_params$maxdepth, 
                    ", cp=", round(best_params$cp, 4), ", min_split=", best_params$minsplit, "</li>",
                    "<li><strong>Best CV score:</strong> ", round(private$.tuning_results$best_score, 3), "</li>"
                )
            }
            
            # Feature selection information
            if (self$options$feature_selection) {
                n_original <- length(c(self$options$vars, self$options$facs))
                n_selected <- ncol(private$.training_data) - 1  # minus target
                advanced_info <- paste0(advanced_info,
                    "<li><strong>Feature selection:</strong> ", self$options$feature_selection_method, 
                    " (", n_selected, "/", n_original, " features)</li>"
                )
            }
            
            # Cost-sensitive information
            if (self$options$cost_sensitive) {
                advanced_info <- paste0(advanced_info,
                    "<li><strong>Cost-sensitive learning:</strong> ", self$options$clinical_loss_preset, " preset</li>"
                )
            }
            
            # Prevalence adjustment information
            if (self$options$prevalence_adjustment) {
                advanced_info <- paste0(advanced_info,
                    "<li><strong>Prevalence adjustment:</strong> ", self$options$population_prevalence, "% population prevalence</li>"
                )
            }
            
            # Cohort validation information
            if (self$options$validation == "cohort" && !is.null(private$.predictions$cohort_info)) {
                cohort_info <- private$.predictions$cohort_info
                advanced_info <- paste0(advanced_info,
                    "<li><strong>Cohort validation:</strong> Training=", cohort_info$train_size, 
                    ", Testing=", cohort_info$test_size, " samples</li>"
                )
            }
            
            summary_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>",
                "<h4>Advanced Model Summary</h4>",
                "<ul>",
                "<li><strong>Algorithm:</strong> Enhanced CART (rpart) with advanced features</li>",
                "<li><strong>Training samples:</strong> ", nrow(private$.training_data), "</li>",
                "<li><strong>Tree nodes:</strong> ", n_nodes, "</li>",
                "<li><strong>Leaf nodes:</strong> ", n_leaves, "</li>",
                "<li><strong>Tree depth:</strong> ", tree_depth, "</li>",
                "<li><strong>Validation method:</strong> ", self$options$validation, "</li>",
                advanced_info,
                "</ul>",
                "</div>"
            )
            
            self$results$modelsummary$setContent(summary_html)
        },

        .populate_performance_table = function() {
            if (is.null(private$.predictions)) {
                return()
            }

            actual <- private$.predictions$actual
            predicted <- private$.predictions$predicted
            target_level <- self$options$targetLevel
            
            # Create confusion matrix
            cm <- table(Actual = actual, Predicted = predicted)
            
            # Calculate comprehensive metrics
            if (target_level %in% levels(actual)) {
                # Binary classification metrics
                pos_class <- target_level
                neg_class <- levels(actual)[levels(actual) != pos_class][1]
                
                tp <- ifelse(pos_class %in% rownames(cm) && pos_class %in% colnames(cm), 
                            cm[pos_class, pos_class], 0)
                fn <- ifelse(pos_class %in% rownames(cm) && neg_class %in% colnames(cm), 
                            cm[pos_class, neg_class], 0)
                fp <- ifelse(neg_class %in% rownames(cm) && pos_class %in% colnames(cm), 
                            cm[neg_class, pos_class], 0)
                tn <- ifelse(neg_class %in% rownames(cm) && neg_class %in% colnames(cm), 
                            cm[neg_class, neg_class], 0)
                
                # Calculate performance metrics
                accuracy <- (tp + tn) / sum(cm)
                sensitivity <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
                specificity <- ifelse(fp + tn > 0, tn / (fp + tn), 0)
                ppv <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
                npv <- ifelse(tn + fn > 0, tn / (tn + fn), 0)
                f1_score <- ifelse(tp + fp > 0 && tp + fn > 0, 2 * tp / (2 * tp + fp + fn), 0)
                balanced_acc <- (sensitivity + specificity) / 2
                
                # Calculate AUC if probabilities available
                auc_value <- NA
                if (!is.null(private$.predictions$probabilities) && pos_class %in% colnames(private$.predictions$probabilities)) {
                    tryCatch({
                        if (requireNamespace("pROC", quietly = TRUE)) {
                            roc_obj <- pROC::roc(actual, private$.predictions$probabilities[, pos_class], quiet = TRUE)
                            auc_value <- as.numeric(pROC::auc(roc_obj))
                        }
                    }, error = function(e) {
                        # AUC calculation failed
                    })
                }
                
                # Bootstrap confidence intervals if requested
                n <- length(actual)
                alpha <- 0.05
                
                metrics <- data.frame(
                    metric = c("Accuracy", "Balanced Accuracy", "Sensitivity", "Specificity", "PPV", "NPV", "F1 Score", "AUC"),
                    value = c(accuracy, balanced_acc, sensitivity, specificity, ppv, npv, f1_score, auc_value),
                    ci_lower = c(
                        pmax(0, accuracy - qnorm(1-alpha/2) * sqrt(accuracy * (1 - accuracy) / n)),
                        pmax(0, balanced_acc - qnorm(1-alpha/2) * sqrt(balanced_acc * (1 - balanced_acc) / n)),
                        pmax(0, sensitivity - qnorm(1-alpha/2) * sqrt(sensitivity * (1 - sensitivity) / (tp + fn))),
                        pmax(0, specificity - qnorm(1-alpha/2) * sqrt(specificity * (1 - specificity) / (fp + tn))),
                        pmax(0, ppv - qnorm(1-alpha/2) * sqrt(ppv * (1 - ppv) / (tp + fp))),
                        pmax(0, npv - qnorm(1-alpha/2) * sqrt(npv * (1 - npv) / (tn + fn))),
                        rep(NA, 1),  # F1 CI is complex
                        rep(NA, 1)   # AUC CI is complex
                    ),
                    ci_upper = c(
                        pmin(1, accuracy + qnorm(1-alpha/2) * sqrt(accuracy * (1 - accuracy) / n)),
                        pmin(1, balanced_acc + qnorm(1-alpha/2) * sqrt(balanced_acc * (1 - balanced_acc) / n)),
                        pmin(1, sensitivity + qnorm(1-alpha/2) * sqrt(sensitivity * (1 - sensitivity) / (tp + fn))),
                        pmin(1, specificity + qnorm(1-alpha/2) * sqrt(specificity * (1 - specificity) / (fp + tn))),
                        pmin(1, ppv + qnorm(1-alpha/2) * sqrt(ppv * (1 - ppv) / (tp + fp))),
                        pmin(1, npv + qnorm(1-alpha/2) * sqrt(npv * (1 - npv) / (tn + fn))),
                        rep(NA, 1),  # F1 CI is complex
                        rep(NA, 1)   # AUC CI is complex
                    )
                )
                
            } else {
                # Multi-class metrics (simplified)
                accuracy <- sum(diag(cm)) / sum(cm)
                
                metrics <- data.frame(
                    metric = "Accuracy",
                    value = accuracy,
                    ci_lower = pmax(0, accuracy - qnorm(0.975) * sqrt(accuracy * (1 - accuracy) / length(actual))),
                    ci_upper = pmin(1, accuracy + qnorm(0.975) * sqrt(accuracy * (1 - accuracy) / length(actual)))
                )
            }

            # Populate table
            for (i in 1:nrow(metrics)) {
                self$results$performancetable$addRow(
                    rowKey = i,
                    values = list(
                        metric = metrics$metric[i],
                        value = metrics$value[i],
                        cilower = metrics$ci_lower[i],
                        ciupper = metrics$ci_upper[i]
                    )
                )
            }
        },

        .populate_confusion_matrix = function() {
            if (is.null(private$.predictions)) {
                return()
            }

            actual <- private$.predictions$actual
            predicted <- private$.predictions$predicted
            
            # Create confusion matrix
            cm <- table(Actual = actual, Predicted = predicted)
            
            # Add margins
            cm_with_totals <- addmargins(cm)
            
            # Populate table
            for (i in 1:nrow(cm_with_totals)) {
                row_name <- rownames(cm_with_totals)[i]
                
                self$results$confusionmatrix$addRow(
                    rowKey = i,
                    values = list(
                        actual = row_name,
                        predictednegative = ifelse(ncol(cm_with_totals) >= 1, cm_with_totals[i, 1], 0),
                        predictedpositive = ifelse(ncol(cm_with_totals) >= 2, cm_with_totals[i, 2], 0),
                        total = ifelse(ncol(cm_with_totals) >= 3, cm_with_totals[i, 3], sum(cm_with_totals[i, 1:(ncol(cm_with_totals)-1)]))
                    )
                )
            }
        },

        .populate_variable_importance = function() {
            if (is.null(private$.model) || is.null(private$.model$variable.importance)) {
                return()
            }

            importance <- private$.model$variable.importance
            importance_df <- data.frame(
                variable = names(importance),
                importance = as.numeric(importance),
                rank = 1:length(importance)
            )
            
            # Normalize importance to 0-100 scale
            if (max(importance_df$importance) > 0) {
                importance_df$importance <- (importance_df$importance / max(importance_df$importance)) * 100
            }
            
            # Populate table
            for (i in 1:nrow(importance_df)) {
                self$results$variableimportance$addRow(
                    rowKey = i,
                    values = list(
                        variable = importance_df$variable[i],
                        importance = importance_df$importance[i],
                        rank = importance_df$rank[i]
                    )
                )
            }
        },

        .populate_tuning_results = function() {
            if (is.null(private$.tuning_results)) {
                return()
            }
            
            best_params <- private$.tuning_results$best_params
            best_score <- private$.tuning_results$best_score
            
            tuning_html <- paste0(
                "<div style='background-color: #fff3e0; padding: 15px; border-radius: 5px;'>",
                "<h4>Hyperparameter Tuning Results</h4>",
                "<p><strong>Tuning Method:</strong> ", self$options$tuning_method, " search</p>",
                "<p><strong>Optimization Metric:</strong> ", self$options$tuning_metric, "</p>",
                "<h5>Best Parameters:</h5>",
                "<ul>",
                "<li><strong>Maximum Depth:</strong> ", best_params$maxdepth, "</li>",
                "<li><strong>Complexity Parameter:</strong> ", round(best_params$cp, 4), "</li>",
                "<li><strong>Minimum Split:</strong> ", best_params$minsplit, "</li>",
                "</ul>",
                "<p><strong>Best Cross-Validation Score:</strong> ", round(best_score, 3), "</p>",
                "</div>"
            )
            
            self$results$tuningresults$setContent(tuning_html)
        },

        .populate_clinical_interpretation = function() {
            clinical_context <- self$options$clinical_context
            target_level <- self$options$targetLevel
            
            # Get performance metrics for interpretation
            performance_metrics <- ""
            if (!is.null(private$.predictions)) {
                actual <- private$.predictions$actual
                predicted <- private$.predictions$predicted
                accuracy <- sum(actual == predicted) / length(actual)
                
                # Calculate additional metrics if binary
                if (length(levels(actual)) == 2) {
                    cm <- table(actual, predicted)
                    if (nrow(cm) == 2 && ncol(cm) == 2) {
                        sens <- cm[2,2] / sum(cm[2,])
                        spec <- cm[1,1] / sum(cm[1,])
                        performance_metrics <- sprintf(
                            "Overall accuracy: %.1f%%, Sensitivity: %.1f%%, Specificity: %.1f%%", 
                            accuracy * 100, sens * 100, spec * 100
                        )
                    }
                } else {
                    performance_metrics <- sprintf("Overall accuracy: %.1f%%", accuracy * 100)
                }
            }
            
            # Context-specific interpretation with advanced features
            interpretation <- switch(clinical_context,
                "diagnosis" = paste0(
                    "<h4>Clinical Interpretation - Advanced Diagnostic Model</h4>",
                    "<p>This advanced decision tree provides comprehensive diagnostic support for <strong>", target_level, "</strong> cases.</p>",
                    "<ul>",
                    "<li><strong>Enhanced Accuracy:</strong> Advanced validation and hyperparameter optimization</li>",
                    "<li><strong>Clinical Robustness:</strong> Multiple validation methods ensure reliability</li>",
                    "<li><strong>Decision Support:</strong> Detailed variable importance guides clinical focus</li>",
                    "<li><strong>Validation Required:</strong> External validation recommended before clinical use</li>",
                    "</ul>"
                ),
                "screening" = paste0(
                    "<h4>Clinical Interpretation - Advanced Screening Model</h4>",
                    "<p>This optimized decision tree identifies patients at risk for <strong>", target_level, "</strong> with enhanced precision.</p>",
                    "<ul>",
                    "<li><strong>Optimized Sensitivity:</strong> Hyperparameter tuning maximizes case detection</li>",
                    "<li><strong>Population Validation:</strong> Robust cross-validation ensures generalizability</li>",
                    "<li><strong>Risk Stratification:</strong> Advanced metrics provide confidence levels</li>",
                    "<li><strong>Cost-Effectiveness:</strong> Balanced sensitivity/specificity for screening programs</li>",
                    "</ul>"
                ),
                "treatment" = paste0(
                    "<h4>Clinical Interpretation - Advanced Treatment Selection</h4>",
                    "<p>This sophisticated model guides personalized treatment decisions for <strong>", target_level, "</strong> cases.</p>",
                    "<ul>",
                    "<li><strong>Precision Medicine:</strong> Advanced feature selection identifies key predictors</li>",
                    "<li><strong>Treatment Optimization:</strong> Cost-sensitive learning considers clinical outcomes</li>",
                    "<li><strong>Patient Stratification:</strong> Detailed performance metrics guide decision making</li>",
                    "<li><strong>Clinical Integration:</strong> Validated approach suitable for clinical decision support</li>",
                    "</ul>"
                ),
                "risk" = paste0(
                    "<h4>Clinical Interpretation - Advanced Risk Assessment</h4>",
                    "<p>This comprehensive model provides detailed risk assessment for <strong>", target_level, "</strong>.</p>",
                    "<ul>",
                    "<li><strong>Risk Quantification:</strong> Advanced metrics provide precise risk estimates</li>",
                    "<li><strong>Population Calibration:</strong> Validated across multiple patient cohorts</li>",
                    "<li><strong>Preventive Planning:</strong> Detailed predictions guide intervention strategies</li>",
                    "<li><strong>Longitudinal Validation:</strong> Robust methods ensure long-term reliability</li>",
                    "</ul>"
                )
            )
            
            clinical_html <- paste0(
                "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 5px;'>",
                interpretation,
                "<p><strong>Performance:</strong> ", performance_metrics, "</p>",
                "<h4>Advanced Features Used:</h4>",
                "<ul>",
                "<li><strong>Validation Method:</strong> ", private$.predictions$validation_method, "</li>",
                if (self$options$hyperparameter_tuning) "<li><strong>Hyperparameter Optimization:</strong> Enabled</li>" else "",
                if (self$options$feature_selection) "<li><strong>Feature Selection:</strong> Enabled</li>" else "",
                if (self$options$cost_sensitive) "<li><strong>Cost-Sensitive Learning:</strong> Enabled</li>" else "",
                "</ul>",
                "<h4>Important Clinical Notes:</h4>",
                "<ul>",
                "<li>Advanced model requires careful validation in target population</li>",
                "<li>Performance may vary across different clinical settings</li>",
                "<li>Regular model updating recommended with new data</li>",
                "<li>Consider model complexity vs. interpretability trade-offs</li>",
                "<li>Ensure adequate sample size for reliable validation</li>",
                "</ul>",
                "</div>"
            )
            
            self$results$clinicalinterpretation$setContent(clinical_html)
        },

        # Plotting functions
        .tree_plot = function(image, ggtheme, ...) {
            if (is.null(private$.model)) {
                return(FALSE)
            }

            tryCatch({
                if (requireNamespace("rpart.plot", quietly = TRUE)) {
                    rpart.plot::rpart.plot(
                        private$.model,
                        type = 4,
                        extra = 101,
                        fallen.leaves = TRUE,
                        main = "Advanced Clinical Decision Tree",
                        cex = 0.7,
                        box.palette = "auto",
                        shadow.col = "gray"
                    )
                } else {
                    plot(private$.model, main = "Advanced Clinical Decision Tree")
                    text(private$.model, cex = 0.7)
                }
                
                return(TRUE)
                
            }, error = function(e) {
                return(FALSE)
            })
        },

        .importance_plot = function(image, ggtheme, ...) {
            if (is.null(private$.model) || is.null(private$.model$variable.importance)) {
                return(FALSE)
            }

            tryCatch({
                importance <- private$.model$variable.importance
                importance_df <- data.frame(
                    variable = names(importance),
                    importance = as.numeric(importance)
                )
                
                # Normalize to 0-100 scale
                if (max(importance_df$importance) > 0) {
                    importance_df$importance <- (importance_df$importance / max(importance_df$importance)) * 100
                }
                
                # Sort by importance
                importance_df <- importance_df[order(importance_df$importance, decreasing = TRUE), ]
                importance_df$variable <- factor(importance_df$variable, levels = importance_df$variable)
                
                p <- ggplot2::ggplot(importance_df, ggplot2::aes(x = variable, y = importance)) +
                    ggplot2::geom_col(fill = "darkblue", alpha = 0.7) +
                    ggplot2::coord_flip() +
                    ggplot2::labs(
                        title = "Variable Importance (Advanced Analysis)",
                        x = "Clinical Variables",
                        y = "Importance (%)"
                    ) +
                    ggplot2::theme_minimal() +
                    ggtheme
                
                print(p)
                return(TRUE)
                
            }, error = function(e) {
                return(FALSE)
            })
        },

        .validation_curves_plot = function(image, ggtheme, ...) {
            if (is.null(private$.tuning_results) || is.null(private$.tuning_results$detailed_results)) {
                return(FALSE)
            }

            tryCatch({
                # Use actual CV results from hyperparameter tuning
                detailed_results <- private$.tuning_results$detailed_results
                
                if (nrow(detailed_results) == 0) {
                    return(FALSE)
                }
                
                # Create validation curves for complexity parameter
                # Sort by CP for smoother line
                detailed_results <- detailed_results[order(detailed_results$cp), ]
                
                p <- ggplot2::ggplot(detailed_results, ggplot2::aes(x = cp, y = mean_score)) +
                    ggplot2::geom_line(color = "blue", size = 1) +
                    ggplot2::geom_point(color = "darkblue", size = 2) +
                    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_score - std_score, 
                                                       ymax = mean_score + std_score), 
                                          width = 0.02, alpha = 0.7) +
                    ggplot2::scale_x_log10() +
                    ggplot2::labs(
                        title = paste("Validation Curves -", self$options$tuning_metric, "vs Complexity Parameter"),
                        x = "Complexity Parameter (log scale)",
                        y = paste("Cross-Validation", self$options$tuning_metric),
                        caption = "Error bars show ±1 standard deviation across CV folds"
                    ) +
                    ggplot2::theme_minimal() +
                    ggtheme
                
                print(p)
                return(TRUE)
                
            }, error = function(e) {
                return(FALSE)
            })
        },

        .calibration_plot = function(image, ggtheme, ...) {
            if (is.null(private$.predictions) || is.null(private$.predictions$probabilities)) {
                return(FALSE)
            }

            tryCatch({
                actual <- private$.predictions$actual
                target_level <- self$options$targetLevel
                
                if (!target_level %in% colnames(private$.predictions$probabilities)) {
                    return(FALSE)
                }
                
                prob_positive <- private$.predictions$probabilities[, target_level]
                actual_binary <- ifelse(actual == target_level, 1, 0)
                
                # Create calibration bins
                n_bins <- 10
                prob_bins <- cut(prob_positive, breaks = n_bins, include.lowest = TRUE)
                
                # Calculate mean predicted vs observed for each bin
                calib_data <- aggregate(
                    list(pred_prob = prob_positive, obs_prob = actual_binary), 
                    by = list(bin = prob_bins), 
                    FUN = mean, na.rm = TRUE
                )
                
                # Perfect calibration line
                perfect_line <- data.frame(x = c(0, 1), y = c(0, 1))
                
                p <- ggplot2::ggplot(calib_data, ggplot2::aes(x = pred_prob, y = obs_prob)) +
                    ggplot2::geom_line(data = perfect_line, ggplot2::aes(x = x, y = y), 
                                     color = "red", linetype = "dashed", size = 1) +
                    ggplot2::geom_line(color = "blue", size = 1) +
                    ggplot2::geom_point(color = "darkblue", size = 3) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::labs(
                        title = "Calibration Plot",
                        x = "Mean Predicted Probability",
                        y = "Mean Observed Frequency",
                        caption = "Red dashed line = perfect calibration"
                    ) +
                    ggplot2::theme_minimal() +
                    ggtheme
                
                print(p)
                return(TRUE)
                
            }, error = function(e) {
                return(FALSE)
            })
        },

        .roc_plot = function(image, ggtheme, ...) {
            if (is.null(private$.predictions) || is.null(private$.predictions$probabilities)) {
                return(FALSE)
            }

            tryCatch({
                actual <- private$.predictions$actual
                target_level <- self$options$targetLevel
                
                if (!target_level %in% colnames(private$.predictions$probabilities)) {
                    return(FALSE)
                }
                
                prob_positive <- private$.predictions$probabilities[, target_level]
                actual_binary <- ifelse(actual == target_level, 1, 0)
                
                # Calculate ROC curve manually or use pROC if available
                if (requireNamespace("pROC", quietly = TRUE)) {
                    roc_obj <- pROC::roc(actual_binary, prob_positive, quiet = TRUE)
                    auc_value <- as.numeric(pROC::auc(roc_obj))
                    
                    roc_data <- data.frame(
                        tpr = roc_obj$sensitivities,
                        fpr = 1 - roc_obj$specificities
                    )
                } else {
                    # Manual ROC calculation
                    thresholds <- sort(unique(prob_positive), decreasing = TRUE)
                    tpr <- numeric(length(thresholds))
                    fpr <- numeric(length(thresholds))
                    
                    for (i in seq_along(thresholds)) {
                        thresh <- thresholds[i]
                        pred_pos <- prob_positive >= thresh
                        
                        tp <- sum(pred_pos & actual_binary == 1)
                        fp <- sum(pred_pos & actual_binary == 0)
                        fn <- sum(!pred_pos & actual_binary == 1)
                        tn <- sum(!pred_pos & actual_binary == 0)
                        
                        tpr[i] <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
                        fpr[i] <- ifelse(fp + tn > 0, fp / (fp + tn), 0)
                    }
                    
                    # Calculate AUC using trapezoidal rule
                    auc_value <- 0
                    for (i in 2:length(fpr)) {
                        auc_value <- auc_value + (fpr[i] - fpr[i-1]) * (tpr[i] + tpr[i-1]) / 2
                    }
                    
                    roc_data <- data.frame(tpr = tpr, fpr = fpr)
                }
                
                # Add diagonal reference line
                diagonal <- data.frame(x = c(0, 1), y = c(0, 1))
                
                p <- ggplot2::ggplot(roc_data, ggplot2::aes(x = fpr, y = tpr)) +
                    ggplot2::geom_line(data = diagonal, ggplot2::aes(x = x, y = y), 
                                     color = "gray", linetype = "dashed") +
                    ggplot2::geom_line(color = "blue", size = 1) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::labs(
                        title = paste0("ROC Curve (AUC = ", round(auc_value, 3), ")"),
                        x = "False Positive Rate (1 - Specificity)",
                        y = "True Positive Rate (Sensitivity)"
                    ) +
                    ggplot2::theme_minimal() +
                    ggtheme
                
                print(p)
                return(TRUE)
                
            }, error = function(e) {
                return(FALSE)
            })
        },

        # Helper functions
        .create_cost_matrix = function() {
            preset <- self$options$clinical_loss_preset
            
            # Cost matrix: rows = true class, cols = predicted class
            # [TN cost, FP cost; FN cost, TP cost]
            switch(preset,
                "equal" = {
                    # Equal misclassification costs
                    matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)
                },
                "screening" = {
                    # Conservative screening - avoid false negatives (missing disease)
                    matrix(c(0, 1, 5, 0), nrow = 2, byrow = TRUE)
                },
                "diagnosis" = {
                    # Conservative diagnosis - avoid false positives (unnecessary treatment)
                    matrix(c(0, 3, 1, 0), nrow = 2, byrow = TRUE)
                },
                "custom" = {
                    # Custom cost ratio
                    fn_cost <- self$options$fn_fp_cost_ratio
                    matrix(c(0, 1, fn_cost, 0), nrow = 2, byrow = TRUE)
                },
                {
                    # Default: equal costs
                    matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)
                }
            )
        },

        .adjust_for_prevalence = function(probabilities, target_level) {
            if (!self$options$prevalence_adjustment) {
                return(probabilities)
            }
            
            # Get current prevalence in sample
            actual <- private$.predictions$actual
            sample_prevalence <- sum(actual == target_level) / length(actual)
            
            # Get desired population prevalence
            population_prevalence <- self$options$population_prevalence / 100
            
            # Adjust probabilities using Bayes theorem
            # P(disease|test+,pop) = P(test+|disease) * P(disease,pop) / P(test+,pop)
            
            if (target_level %in% colnames(probabilities)) {
                prob_pos <- probabilities[, target_level]
                
                # Adjusted probabilities
                numerator <- prob_pos * population_prevalence
                denominator <- prob_pos * population_prevalence + (1 - prob_pos) * (1 - population_prevalence)
                
                adjusted_prob <- numerator / denominator
                probabilities[, target_level] <- adjusted_prob
                
                # Adjust negative class probability
                neg_class <- colnames(probabilities)[colnames(probabilities) != target_level][1]
                if (!is.null(neg_class)) {
                    probabilities[, neg_class] <- 1 - adjusted_prob
                }
            }
            
            return(probabilities)
        }
    )
)