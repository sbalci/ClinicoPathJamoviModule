#' @title Clinical Tree Algorithm Comparison
#' @description Comprehensive comparison of decision tree algorithms for clinical research
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom rpart rpart
#' @importFrom randomForest randomForest
#' @importFrom caret createDataPartition createFolds trainControl train
#' @importFrom pROC roc auc
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_line labs theme_minimal

treecompareClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "treecompareClass",
    inherit = treecompareBase,
    private = list(
        .algorithms = list(),
        .training_data = NULL,
        .performance_data = NULL,
        .comparison_results = NULL,
        .statistical_tests = NULL,
        .ranking_data = NULL,
        .cv_predictions = list(),
        .tuned_parameters = list(),

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

            # Train selected algorithms
            private$.train_algorithms()

            # Compare performance
            private$.compare_performance()

            # Generate statistical tests if requested
            if (self$options$statistical_testing) {
                private$.statistical_comparison()
            }

            # Create ranking and recommendations
            private$.generate_ranking()

            # Populate all results
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

            # Validate target level exists in target variable
            if (!is.null(target) && target %in% names(self$data)) {
                target_levels <- levels(as.factor(self$data[[target]]))
                if (!is.null(self$options$targetLevel) && !self$options$targetLevel %in% target_levels) {
                    return(list(valid = FALSE, show_welcome = FALSE,
                               message = paste0("Target level '", self$options$targetLevel, 
                                               "' not found in target variable. Available levels: ", 
                                               paste(target_levels, collapse = ", "))))
                }
            }

            # Check at least one algorithm selected
            algorithms_selected <- c(
                self$options$include_cart,
                self$options$include_rf,
                self$options$include_gbm,
                self$options$include_xgboost,
                self$options$include_ctree
            )

            if (!any(algorithms_selected)) {
                return(list(valid = FALSE, show_welcome = FALSE,
                           message = "Please select at least one algorithm to compare."))
            }

            # Check minimum sample size
            if (nrow(self$data) < 30) {
                return(list(valid = FALSE, show_welcome = FALSE,
                           message = "Algorithm comparison requires at least 30 samples."))
            }

            # Check class balance if target is available
            if (!is.null(target) && target %in% names(self$data)) {
                target_data <- self$data[[target]]
                if (!is.null(target_data)) {
                    target_factor <- as.factor(target_data[!is.na(target_data)])
                    if (length(levels(target_factor)) == 2) {
                        class_table <- table(target_factor)
                        min_class_size <- min(class_table)
                        class_ratio <- min(class_table) / max(class_table)
                        
                        # Warn about severe class imbalance
                        if (class_ratio < 0.1) {
                            return(list(valid = FALSE, show_welcome = FALSE,
                                       message = paste0("Severe class imbalance detected (ratio: ", 
                                                       round(class_ratio, 3), "). Consider balancing techniques or specialized algorithms.")))
                        }
                        
                        # Check minimum samples per class for cross-validation
                        cv_folds <- ifelse(!is.null(self$options$cv_folds), self$options$cv_folds, 5)
                        min_per_fold <- floor(min_class_size / cv_folds)
                        if (min_per_fold < 2) {
                            return(list(valid = FALSE, show_welcome = FALSE,
                                       message = paste0("Insufficient samples in minority class (", min_class_size, 
                                                       ") for ", cv_folds, "-fold cross-validation. Need at least ", 
                                                       cv_folds * 2, " samples per class.")))
                        }
                        
                        # Warning for moderate imbalance
                        if (class_ratio < 0.3) {
                            # This will be handled as a warning in the prepare_data phase
                        }
                    } else if (length(levels(target_factor)) > 2) {
                        return(list(valid = FALSE, show_welcome = FALSE,
                                   message = "Multi-class classification is not currently supported. Target must be binary."))
                    } else if (length(levels(target_factor)) < 2) {
                        return(list(valid = FALSE, show_welcome = FALSE,
                                   message = "Target variable must have at least 2 distinct values."))
                    }
                }
            }

            return(list(valid = TRUE))
        },

        .show_welcome_message = function() {
            welcome_html <- "
            <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 20px 0;'>
            <h3 style='color: #2e7d32; margin-top: 0;'>Clinical Tree Algorithm Comparison</h3>
            <p><strong>Compare multiple decision tree algorithms for optimal clinical performance</strong></p>
            
            <h4 style='color: #2e7d32;'>Required Setup:</h4>
            <ol>
            <li><strong>Predictors:</strong> Select clinical variables (continuous and/or categorical)</li>
            <li><strong>Target Outcome:</strong> Select the outcome variable to predict</li>
            <li><strong>Target Level:</strong> Select the positive class (e.g., 'disease', 'positive')</li>
            <li><strong>Algorithms:</strong> Choose at least one algorithm to include in comparison</li>
            </ol>

            <h4 style='color: #2e7d32;'>Available Algorithms:</h4>
            <ul>
            <li><strong>CART:</strong> Classification and Regression Trees (rpart)</li>
            <li><strong>Random Forest:</strong> Ensemble of decision trees</li>
            <li><strong>Gradient Boosting:</strong> Sequential ensemble method</li>
            <li><strong>XGBoost:</strong> Optimized gradient boosting (requires xgboost package)</li>
            <li><strong>Conditional Trees:</strong> Unbiased tree algorithm (requires party package)</li>
            </ul>

            <h4 style='color: #2e7d32;'>Key Features:</h4>
            <ul>
            <li><strong>Fair Comparison:</strong> Same validation method for all algorithms</li>
            <li><strong>Statistical Testing:</strong> Compare performance with significance tests</li>
            <li><strong>Clinical Context:</strong> Recommendations based on interpretability and performance</li>
            <li><strong>Comprehensive Metrics:</strong> Accuracy, AUC, sensitivity, specificity, F1 score</li>
            </ul>
            </div>"
            
            self$results$instructions$setContent(welcome_html)
        },

        .prepare_data = function() {
            vars <- self$options$vars
            facs <- self$options$facs
            target <- self$options$target

            # Combine all variables
            all_vars <- c(vars, facs, target)
            
            # Extract relevant data
            data <- self$data[all_vars]
            
            # Remove rows with missing target
            data <- data[!is.na(data[[target]]), ]
            
            # Prepare target variable
            data[[target]] <- as.factor(data[[target]])
            
            # Enhanced missing data handling
            missing_summary <- sapply(c(vars, facs), function(var) {
                if (var %in% names(data)) {
                    sum(is.na(data[[var]])) / nrow(data)
                } else {
                    0
                }
            })
            
            # Warn about high missingness
            high_missing <- missing_summary > 0.3
            if (any(high_missing)) {
                high_missing_vars <- names(missing_summary)[high_missing]
                warning(paste("Variables with >30% missing data:", paste(high_missing_vars, collapse = ", ")))
            }
            
            # Handle missing data with improved strategies
            for (var in c(vars, facs)) {
                if (var %in% names(data)) {
                    missing_pct <- sum(is.na(data[[var]])) / nrow(data)
                    if (missing_pct > 0 && missing_pct <= 0.5) {
                        if (is.numeric(data[[var]])) {
                            # Use median imputation for continuous variables
                            data[[var]][is.na(data[[var]])] <- median(data[[var]], na.rm = TRUE)
                        } else {
                            # Use mode imputation for categorical variables
                            mode_val <- names(sort(table(data[[var]]), decreasing = TRUE))[1]
                            data[[var]][is.na(data[[var]])] <- mode_val
                            data[[var]] <- as.factor(data[[var]])
                        }
                    } else if (missing_pct > 0.5) {
                        warning(paste("Variable", var, "has", round(missing_pct * 100, 1), 
                                     "% missing data. Consider excluding or using advanced imputation."))
                    }
                }
            }
            
            # Check class imbalance after data preparation
            target_factor <- as.factor(data[[target]])
            class_table <- table(target_factor)
            min_class_size <- min(class_table)
            class_ratio <- min(class_table) / max(class_table)
            
            if (class_ratio < 0.3) {
                warning(paste0("Moderate class imbalance detected (ratio: ", round(class_ratio, 3), 
                             "). Consider balancing techniques or class-weighted algorithms."))
            }
            
            # Check if we still have enough data
            if (nrow(data) < 30) {
                jmvcore::reject("Insufficient data after handling missing values. Need at least 30 complete cases.")
            }
            
            # Store prepared data
            private$.training_data <- data
        },

        .train_algorithms = function() {
            # Initialize algorithms list based on user selection
            private$.algorithms <- list()
            
            # Prepare formula for model training
            target <- self$options$target
            vars <- self$options$vars
            facs <- self$options$facs
            predictors <- c(vars, facs)
            
            if (length(predictors) == 0) {
                jmvcore::reject("No predictor variables selected.")
                return()
            }
            
            formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
            formula_obj <- as.formula(formula_str)
            
            # Track algorithm initialization
            n_algorithms <- sum(c(self$options$include_cart, self$options$include_rf, 
                                self$options$include_gbm, self$options$include_xgboost, 
                                self$options$include_ctree))
            current_alg <- 0
            
            # CART Implementation
            if (self$options$include_cart) {
                current_alg <- current_alg + 1
                if (self$options$verbose_output) {
                    message(paste0("Training algorithm ", current_alg, "/", n_algorithms, ": CART"))
                }
                
                tryCatch({
                    # Parameter tuning for CART if requested
                    cart_params <- list(
                        maxdepth = self$options$cart_max_depth,
                        minsplit = self$options$cart_min_split,
                        cp = 0.01
                    )
                    
                    if (self$options$tune_parameters && requireNamespace("caret", quietly = TRUE) && nrow(private$.training_data) >= 30) {
                        # Grid search for optimal parameters
                        tune_grid <- expand.grid(
                            cp = c(0.001, 0.01, 0.1),
                            maxdepth = c(3, 5, 10),
                            minsplit = c(10, 20, 50)
                        )
                        
                        best_performance <- -Inf
                        best_params <- cart_params
                        
                        for (i in 1:min(nrow(tune_grid), 9)) {  # Limit to 9 combinations for speed
                            test_params <- list(
                                maxdepth = tune_grid$maxdepth[i],
                                minsplit = tune_grid$minsplit[i],
                                cp = tune_grid$cp[i]
                            )
                            
                            # Quick cross-validation for parameter tuning with safety checks
                            cv_perf <- private$.quick_cv_performance(formula_obj, "rpart", test_params, 3)
                            if (!is.null(cv_perf) && is.finite(cv_perf) && cv_perf > best_performance) {
                                best_performance <- cv_perf
                                best_params <- test_params
                            }
                        }
                        cart_params <- best_params
                        private$.tuned_parameters[["CART"]] <- best_params
                    } else if (self$options$tune_parameters && nrow(private$.training_data) < 30) {
                        if (self$options$verbose_output) {
                            message("Skipping CART parameter tuning due to small dataset size")
                        }
                    }
                    
                    cart_model <- rpart::rpart(
                        formula = formula_obj,
                        data = private$.training_data,
                        method = "class",
                        control = rpart::rpart.control(
                            maxdepth = cart_params$maxdepth,
                            minsplit = cart_params$minsplit,
                            cp = cart_params$cp
                        )
                    )
                    
                    private$.algorithms[["CART"]] <- list(
                        name = "CART",
                        package = "rpart",
                        interpretability = 0.9,
                        model = cart_model,
                        requires_package = "rpart",
                        tuned_params = cart_params
                    )
                }, error = function(e) {
                    warning(paste("CART training failed:", e$message))
                })
            }
            
            # Random Forest Implementation
            if (self$options$include_rf) {
                current_alg <- current_alg + 1
                if (self$options$verbose_output) {
                    message(paste0("Training algorithm ", current_alg, "/", n_algorithms, ": Random Forest"))
                }
                
                if (!requireNamespace("randomForest", quietly = TRUE)) {
                    warning("randomForest package not installed. Skipping Random Forest.")
                } else {
                    tryCatch({
                        # Determine mtry based on method
                        mtry_value <- switch(self$options$rf_mtry_method,
                            "auto" = floor(sqrt(length(predictors))),
                            "third" = floor(length(predictors) / 3),
                            "all" = length(predictors),
                            floor(sqrt(length(predictors)))
                        )
                        
                        # Parameter tuning for Random Forest if requested
                        rf_params <- list(
                            ntree = self$options$rf_ntrees,
                            mtry = mtry_value
                        )
                        
                        if (self$options$tune_parameters && requireNamespace("caret", quietly = TRUE) && nrow(private$.training_data) >= 30) {
                            # Grid search for optimal parameters
                            tune_grid <- expand.grid(
                                ntree = c(100, 300, 500),
                                mtry = c(max(1, floor(length(predictors) / 4)), 
                                        max(1, floor(sqrt(length(predictors)))), 
                                        max(1, floor(length(predictors) / 2)))
                            )
                            
                            best_performance <- -Inf
                            best_params <- rf_params
                            
                            for (i in 1:min(nrow(tune_grid), 6)) {  # Limit for speed
                                test_params <- list(
                                    ntree = tune_grid$ntree[i],
                                    mtry = tune_grid$mtry[i]
                                )
                                
                                # Quick cross-validation for parameter tuning with safety checks
                                cv_perf <- private$.quick_cv_performance(formula_obj, "randomForest", test_params, 3)
                                if (!is.null(cv_perf) && is.finite(cv_perf) && cv_perf > best_performance) {
                                    best_performance <- cv_perf
                                    best_params <- test_params
                                }
                            }
                            rf_params <- best_params
                            private$.tuned_parameters[["Random Forest"]] <- best_params
                        } else if (self$options$tune_parameters && nrow(private$.training_data) < 30) {
                            if (self$options$verbose_output) {
                                message("Skipping Random Forest parameter tuning due to small dataset size")
                            }
                        }
                        
                        rf_model <- randomForest::randomForest(
                            formula = formula_obj,
                            data = private$.training_data,
                            ntree = rf_params$ntree,
                            mtry = rf_params$mtry,
                            importance = TRUE
                        )
                        
                        private$.algorithms[["Random Forest"]] <- list(
                            name = "Random Forest", 
                            package = "randomForest",
                            interpretability = 0.6,
                            model = rf_model,
                            requires_package = "randomForest",
                            tuned_params = rf_params
                        )
                    }, error = function(e) {
                        warning(paste("Random Forest training failed:", e$message))
                    })
                }
            }
            
            # Gradient Boosting Implementation
            if (self$options$include_gbm) {
                current_alg <- current_alg + 1
                if (self$options$verbose_output) {
                    message(paste0("Training algorithm ", current_alg, "/", n_algorithms, ": GBM"))
                }
                
                if (!requireNamespace("gbm", quietly = TRUE)) {
                    warning("gbm package not installed. Skipping GBM.")
                } else {
                    tryCatch({
                        # Convert target to 0/1 for gbm
                        target_binary <- as.numeric(private$.training_data[[target]] == self$options$targetLevel)
                        train_data_gbm <- private$.training_data
                        train_data_gbm[[target]] <- target_binary
                        
                        gbm_model <- gbm::gbm(
                            formula = formula_obj,
                            data = train_data_gbm,
                            distribution = "bernoulli",
                            n.trees = 500,
                            interaction.depth = 3,
                            shrinkage = 0.01,
                            cv.folds = 0  # No CV here, we'll do it separately
                        )
                        
                        private$.algorithms[["GBM"]] <- list(
                            name = "GBM",
                            package = "gbm", 
                            interpretability = 0.4,
                            model = gbm_model,
                            requires_package = "gbm"
                        )
                    }, error = function(e) {
                        warning(paste("GBM training failed:", e$message))
                    })
                }
            }
            
            # XGBoost Implementation
            if (self$options$include_xgboost) {
                current_alg <- current_alg + 1
                if (self$options$verbose_output) {
                    message(paste0("Training algorithm ", current_alg, "/", n_algorithms, ": XGBoost"))
                }
                
                if (!requireNamespace("xgboost", quietly = TRUE)) {
                    warning("xgboost package not installed. Skipping XGBoost.")
                } else {
                    tryCatch({
                        # Prepare data for xgboost
                        x_matrix <- model.matrix(formula_obj, data = private$.training_data)[, -1]
                        y_vector <- as.numeric(private$.training_data[[target]] == self$options$targetLevel)
                        
                        xgb_model <- xgboost::xgboost(
                            data = x_matrix,
                            label = y_vector,
                            max_depth = 6,
                            eta = 0.3,
                            nrounds = 100,
                            objective = "binary:logistic",
                            verbose = 0
                        )
                        
                        private$.algorithms[["XGBoost"]] <- list(
                            name = "XGBoost",
                            package = "xgboost",
                            interpretability = 0.3,
                            model = xgb_model,
                            model_matrix_cols = colnames(x_matrix),
                            requires_package = "xgboost"
                        )
                    }, error = function(e) {
                        warning(paste("XGBoost training failed:", e$message))
                    })
                }
            }
            
            # Conditional Trees Implementation
            if (self$options$include_ctree) {
                current_alg <- current_alg + 1
                if (self$options$verbose_output) {
                    message(paste0("Training algorithm ", current_alg, "/", n_algorithms, ": Conditional Trees"))
                }
                
                if (!requireNamespace("party", quietly = TRUE)) {
                    warning("party package not installed. Skipping Conditional Trees.")
                } else {
                    tryCatch({
                        ctree_model <- party::ctree(
                            formula = formula_obj,
                            data = private$.training_data
                        )
                        
                        private$.algorithms[["Conditional Trees"]] <- list(
                            name = "Conditional Trees",
                            package = "party",
                            interpretability = 0.8,
                            model = ctree_model,
                            requires_package = "party"
                        )
                    }, error = function(e) {
                        warning(paste("Conditional Trees training failed:", e$message))
                    })
                }
            }
            
            if (length(private$.algorithms) == 0) {
                jmvcore::reject("No algorithms could be trained. Please check package installations and data.")
            }
        },

        .compare_performance = function() {
            # Perform actual cross-validation for algorithm comparison
            if (length(private$.algorithms) == 0) return()
            
            target <- self$options$target
            targetLevel <- self$options$targetLevel
            validation_method <- self$options$validation
            
            # Initialize performance data frame
            algorithm_names <- names(private$.algorithms)
            n_algorithms <- length(algorithm_names)
            
            performance_data <- data.frame(
                algorithm = algorithm_names,
                accuracy = numeric(n_algorithms),
                bacc = numeric(n_algorithms),
                auc = numeric(n_algorithms),
                sensitivity = numeric(n_algorithms),
                specificity = numeric(n_algorithms),
                f1_score = numeric(n_algorithms),
                computation_time = numeric(n_algorithms),
                stringsAsFactors = FALSE
            )
            
            # Store individual predictions for statistical testing
            private$.cv_predictions <- list()
            
            # Perform validation based on selected method
            if (validation_method == "cv" || validation_method == "repeated_cv") {
                # Cross-validation
                n_folds <- self$options$cv_folds
                n_repeats <- ifelse(validation_method == "repeated_cv", self$options$cv_repeats, 1)
                
                # Set up parallel processing if requested
                use_parallel <- self$options$parallel_processing && requireNamespace("parallel", quietly = TRUE)
                if (use_parallel) {
                    n_cores <- min(parallel::detectCores() - 1, n_algorithms)
                    if (n_cores > 1) {
                        cl <- parallel::makeCluster(n_cores)
                        on.exit(parallel::stopCluster(cl), add = TRUE)
                    } else {
                        use_parallel <- FALSE
                    }
                }
                
                for (i in seq_along(algorithm_names)) {
                    alg_name <- algorithm_names[i]
                    alg_info <- private$.algorithms[[alg_name]]
                    
                    if (is.null(alg_info$model)) next
                    
                    start_time <- Sys.time()
                    
                    # Collect CV results and predictions for statistical testing
                    cv_results <- list()
                    alg_predictions <- list()
                    
                    for (repeat_idx in 1:n_repeats) {
                        # Create folds (with stratification if requested)
                        if (self$options$stratified_sampling) {
                            fold_indices <- caret::createFolds(private$.training_data[[target]], k = n_folds, list = TRUE, returnTrain = FALSE)
                        } else {
                            # Simple random folds
                            n_samples <- nrow(private$.training_data)
                            fold_indices <- split(sample(n_samples), rep(1:n_folds, length.out = n_samples))
                        }
                        
                        for (fold_idx in 1:n_folds) {
                            test_idx <- fold_indices[[fold_idx]]
                            train_fold <- private$.training_data[-test_idx, ]
                            test_fold <- private$.training_data[test_idx, ]
                            
                            # Get predictions based on algorithm type
                            predictions <- private$.predict_algorithm(alg_info, train_fold, test_fold)
                            
                            if (!is.null(predictions)) {
                                # Calculate metrics
                                actual <- test_fold[[target]]
                                metrics <- private$.calculate_metrics(actual, predictions$class, predictions$prob, targetLevel)
                                cv_results[[length(cv_results) + 1]] <- metrics
                                
                                # Store predictions for statistical testing
                                pred_data <- data.frame(
                                    actual = actual,
                                    predicted = predictions$class,
                                    correct = (actual == predictions$class),
                                    fold = paste0(repeat_idx, "_", fold_idx),
                                    stringsAsFactors = FALSE
                                )
                                alg_predictions[[length(alg_predictions) + 1]] <- pred_data
                            }
                        }
                    }
                    
                    # Store predictions for this algorithm
                    if (length(alg_predictions) > 0) {
                        private$.cv_predictions[[alg_name]] <- do.call(rbind, alg_predictions)
                    }
                    
                    # Average CV results
                    if (length(cv_results) > 0) {
                        avg_metrics <- Reduce("+", cv_results) / length(cv_results)
                        performance_data[i, c("accuracy", "auc", "sensitivity", "specificity", "f1_score", "bacc")] <- avg_metrics
                    }
                    
                    performance_data$computation_time[i] <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
                }
                
            } else if (validation_method == "holdout") {
                # Hold-out validation
                test_split <- self$options$test_split
                
                # Create train/test split (with stratification if requested)
                if (self$options$stratified_sampling) {
                    train_idx <- caret::createDataPartition(private$.training_data[[target]], 
                                                           p = 1 - test_split, list = FALSE)[, 1]
                } else {
                    # Simple random split
                    n_samples <- nrow(private$.training_data)
                    train_idx <- sample(n_samples, size = floor(n_samples * (1 - test_split)))
                }
                train_data <- private$.training_data[train_idx, ]
                test_data <- private$.training_data[-train_idx, ]
                
                for (i in seq_along(algorithm_names)) {
                    alg_name <- algorithm_names[i]
                    alg_info <- private$.algorithms[[alg_name]]
                    
                    if (is.null(alg_info$model)) next
                    
                    start_time <- Sys.time()
                    
                    # Get predictions
                    predictions <- private$.predict_algorithm(alg_info, train_data, test_data)
                    
                    if (!is.null(predictions)) {
                        # Calculate metrics
                        actual <- test_data[[target]]
                        metrics <- private$.calculate_metrics(actual, predictions$class, predictions$prob, targetLevel)
                        performance_data[i, c("accuracy", "auc", "sensitivity", "specificity", "f1_score", "bacc")] <- metrics
                    }
                    
                    performance_data$computation_time[i] <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
                }
                
            } else if (validation_method == "bootstrap") {
                # Bootstrap validation
                n_bootstrap <- self$options$bootstrap_samples
                
                for (i in seq_along(algorithm_names)) {
                    alg_name <- algorithm_names[i]
                    alg_info <- private$.algorithms[[alg_name]]
                    
                    if (is.null(alg_info$model)) next
                    
                    start_time <- Sys.time()
                    
                    # Collect bootstrap results
                    boot_results <- list()
                    
                    for (boot_idx in 1:n_bootstrap) {
                        # Create bootstrap sample
                        boot_idx <- sample(nrow(private$.training_data), replace = TRUE)
                        train_boot <- private$.training_data[boot_idx, ]
                        test_boot <- private$.training_data[-unique(boot_idx), ]
                        
                        if (nrow(test_boot) > 0) {
                            # Get predictions
                            predictions <- private$.predict_algorithm(alg_info, train_boot, test_boot)
                            
                            if (!is.null(predictions)) {
                                # Calculate metrics
                                actual <- test_boot[[target]]
                                metrics <- private$.calculate_metrics(actual, predictions$class, predictions$prob, targetLevel)
                                boot_results[[length(boot_results) + 1]] <- metrics
                            }
                        }
                    }
                    
                    # Average bootstrap results
                    if (length(boot_results) > 0) {
                        avg_metrics <- Reduce("+", boot_results) / length(boot_results)
                        performance_data[i, c("accuracy", "auc", "sensitivity", "specificity", "f1_score", "bacc")] <- avg_metrics
                    }
                    
                    performance_data$computation_time[i] <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
                }
            }
            
            # Store performance data
            private$.performance_data <- performance_data
        },
        
        .quick_cv_performance = function(formula_obj, package_name, params, n_folds = 3) {
            # Quick cross-validation for parameter tuning
            tryCatch({
                if (nrow(private$.training_data) < n_folds * 10) return(NULL)
                
                target <- self$options$target
                targetLevel <- self$options$targetLevel
                
                # Create simple folds
                fold_size <- floor(nrow(private$.training_data) / n_folds)
                indices <- sample(nrow(private$.training_data))
                
                fold_performances <- numeric(n_folds)
                
                for (k in 1:n_folds) {
                    start_idx <- (k - 1) * fold_size + 1
                    end_idx <- ifelse(k == n_folds, nrow(private$.training_data), k * fold_size)
                    test_idx <- indices[start_idx:end_idx]
                    
                    train_fold <- private$.training_data[-test_idx, ]
                    test_fold <- private$.training_data[test_idx, ]
                    
                    # Train model with test parameters
                    if (package_name == "rpart") {
                        model <- rpart::rpart(
                            formula_obj, data = train_fold, method = "class",
                            control = rpart::rpart.control(
                                maxdepth = params$maxdepth,
                                minsplit = params$minsplit,
                                cp = params$cp
                            )
                        )
                        pred_class <- predict(model, test_fold, type = "class")
                    } else if (package_name == "randomForest") {
                        model <- randomForest::randomForest(
                            formula_obj, data = train_fold,
                            ntree = params$ntree,
                            mtry = params$mtry
                        )
                        pred_class <- predict(model, test_fold, type = "class")
                    } else {
                        return(NULL)
                    }
                    
                    # Calculate balanced accuracy
                    actual <- test_fold[[target]]
                    if (length(levels(actual)) == 2 && length(levels(pred_class)) == 2) {
                        conf_mat <- table(actual, pred_class)
                        if (nrow(conf_mat) == 2 && ncol(conf_mat) == 2) {
                            tp <- conf_mat[targetLevel, targetLevel]
                            tn <- conf_mat[setdiff(levels(actual), targetLevel), setdiff(levels(actual), targetLevel)]
                            fp <- conf_mat[setdiff(levels(actual), targetLevel), targetLevel]
                            fn <- conf_mat[targetLevel, setdiff(levels(actual), targetLevel)]
                            
                            sensitivity <- tp / (tp + fn)
                            specificity <- tn / (tn + fp)
                            fold_performances[k] <- (sensitivity + specificity) / 2
                        }
                    }
                }
                
                return(mean(fold_performances, na.rm = TRUE))
                
            }, error = function(e) {
                return(NULL)
            })
        },
        
        .predict_algorithm = function(alg_info, train_data, test_data) {
            # Helper function to get predictions from different algorithm types
            target <- self$options$target
            targetLevel <- self$options$targetLevel
            
            tryCatch({
                # Re-train model on the fold/split
                formula_str <- paste(target, "~", paste(c(self$options$vars, self$options$facs), collapse = " + "))
                formula_obj <- as.formula(formula_str)
                
                if (alg_info$package == "rpart") {
                    model <- rpart::rpart(formula_obj, data = train_data, method = "class",
                                        control = rpart::rpart.control(
                                            maxdepth = self$options$cart_max_depth,
                                            minsplit = self$options$cart_min_split
                                        ))
                    pred_class <- predict(model, test_data, type = "class")
                    pred_prob <- predict(model, test_data, type = "prob")[, targetLevel]
                    
                } else if (alg_info$package == "randomForest") {
                    mtry_value <- switch(self$options$rf_mtry_method,
                        "auto" = floor(sqrt(length(c(self$options$vars, self$options$facs)))),
                        "third" = floor(length(c(self$options$vars, self$options$facs)) / 3),
                        "all" = length(c(self$options$vars, self$options$facs)),
                        floor(sqrt(length(c(self$options$vars, self$options$facs))))
                    )
                    
                    model <- randomForest::randomForest(formula_obj, data = train_data,
                                                       ntree = min(100, self$options$rf_ntrees),
                                                       mtry = mtry_value)
                    pred_class <- predict(model, test_data, type = "class")
                    pred_prob <- predict(model, test_data, type = "prob")[, targetLevel]
                    
                } else if (alg_info$package == "gbm") {
                    # Prepare data for gbm
                    train_data_gbm <- train_data
                    train_data_gbm[[target]] <- as.numeric(train_data[[target]] == targetLevel)
                    
                    model <- gbm::gbm(formula_obj, data = train_data_gbm,
                                    distribution = "bernoulli",
                                    n.trees = 100,
                                    interaction.depth = 3,
                                    shrinkage = 0.1,
                                    verbose = FALSE)
                    
                    pred_prob <- predict(model, test_data, n.trees = 100, type = "response")
                    pred_class <- factor(ifelse(pred_prob > 0.5, targetLevel, 
                                              levels(test_data[[target]])[levels(test_data[[target]]) != targetLevel][1]),
                                       levels = levels(test_data[[target]]))
                    
                } else if (alg_info$package == "xgboost") {
                    # Prepare data for xgboost
                    x_train <- model.matrix(formula_obj, data = train_data)[, -1]
                    y_train <- as.numeric(train_data[[target]] == targetLevel)
                    x_test <- model.matrix(formula_obj, data = test_data)[, -1]
                    
                    # Ensure test data has same columns as training
                    common_cols <- intersect(colnames(x_train), colnames(x_test))
                    x_train <- x_train[, common_cols]
                    x_test <- x_test[, common_cols]
                    
                    model <- xgboost::xgboost(data = x_train, label = y_train,
                                            max_depth = 6, eta = 0.3, nrounds = 50,
                                            objective = "binary:logistic", verbose = 0)
                    
                    pred_prob <- predict(model, x_test)
                    pred_class <- factor(ifelse(pred_prob > 0.5, targetLevel,
                                              levels(test_data[[target]])[levels(test_data[[target]]) != targetLevel][1]),
                                       levels = levels(test_data[[target]]))
                    
                } else if (alg_info$package == "party") {
                    model <- party::ctree(formula_obj, data = train_data)
                    pred_class <- predict(model, test_data)
                    # Party doesn't provide probabilities easily, so estimate from predictions
                    pred_prob <- as.numeric(pred_class == targetLevel)
                    
                } else {
                    return(NULL)
                }
                
                return(list(class = pred_class, prob = pred_prob))
                
            }, error = function(e) {
                warning(paste("Prediction failed for", alg_info$name, ":", e$message))
                return(NULL)
            })
        },
        
        .calculate_metrics = function(actual, predicted, prob, positive_class) {
            # Calculate performance metrics
            conf_matrix <- table(Actual = actual, Predicted = predicted)
            
            # Handle cases where positive class might not be in predictions
            if (!(positive_class %in% rownames(conf_matrix)) || !(positive_class %in% colnames(conf_matrix))) {
                return(c(accuracy = 0, auc = 0.5, sensitivity = 0, specificity = 0, f1_score = 0, bacc = 0))
            }
            
            # Get negative class
            levels_actual <- levels(actual)
            negative_class <- levels_actual[levels_actual != positive_class][1]
            
            # Calculate metrics
            tp <- ifelse(positive_class %in% rownames(conf_matrix) && positive_class %in% colnames(conf_matrix),
                        conf_matrix[positive_class, positive_class], 0)
            tn <- ifelse(negative_class %in% rownames(conf_matrix) && negative_class %in% colnames(conf_matrix),
                        conf_matrix[negative_class, negative_class], 0)
            fp <- ifelse(negative_class %in% rownames(conf_matrix) && positive_class %in% colnames(conf_matrix),
                        conf_matrix[negative_class, positive_class], 0)
            fn <- ifelse(positive_class %in% rownames(conf_matrix) && negative_class %in% colnames(conf_matrix),
                        conf_matrix[positive_class, negative_class], 0)
            
            accuracy <- (tp + tn) / sum(conf_matrix)
            sensitivity <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
            specificity <- ifelse(tn + fp > 0, tn / (tn + fp), 0)
            precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
            f1_score <- ifelse(precision + sensitivity > 0, 2 * (precision * sensitivity) / (precision + sensitivity), 0)
            balanced_accuracy <- (sensitivity + specificity) / 2
            
            # Calculate AUC if pROC is available
            auc_value <- 0.5
            if (requireNamespace("pROC", quietly = TRUE) && length(unique(prob)) > 1) {
                tryCatch({
                    roc_obj <- pROC::roc(actual, prob, levels = c(negative_class, positive_class), quiet = TRUE)
                    auc_value <- as.numeric(pROC::auc(roc_obj))
                }, error = function(e) {
                    auc_value <- 0.5
                })
            }
            
            return(c(accuracy = accuracy, auc = auc_value, sensitivity = sensitivity, 
                    specificity = specificity, f1_score = f1_score, bacc = balanced_accuracy))
        },

        .statistical_comparison = function() {
            if (nrow(private$.performance_data) < 2 || length(private$.cv_predictions) < 2) return()
            
            # Create pairwise comparisons between algorithms
            algorithm_names <- names(private$.cv_predictions)
            if (length(algorithm_names) < 2) return()
            
            comparisons <- combn(algorithm_names, 2, simplify = FALSE)
            n_comparisons <- length(comparisons)
            
            test_results <- data.frame(
                comparison = character(n_comparisons),
                statistic = numeric(n_comparisons),
                p_value = numeric(n_comparisons),
                test_type = character(n_comparisons),
                stringsAsFactors = FALSE
            )
            
            # Perform actual statistical tests for each pairwise comparison
            for (i in seq_along(comparisons)) {
                alg1_name <- comparisons[[i]][1]
                alg2_name <- comparisons[[i]][2]
                
                test_results$comparison[i] <- paste(alg1_name, "vs", alg2_name)
                
                # Get prediction results for both algorithms
                pred1 <- private$.cv_predictions[[alg1_name]]
                pred2 <- private$.cv_predictions[[alg2_name]]
                
                if (!is.null(pred1) && !is.null(pred2) && nrow(pred1) > 10 && nrow(pred2) > 10) {
                    tryCatch({
                        # Use McNemar's test for paired binary outcomes (when we have same folds)
                        if ("fold" %in% names(pred1) && "fold" %in% names(pred2)) {
                            # Match predictions by fold for paired comparison
                            common_folds <- intersect(pred1$fold, pred2$fold)
                            
                            if (length(common_folds) >= 5) {
                                # Create contingency table for McNemar's test
                                paired_results <- data.frame(
                                    alg1_correct = logical(0),
                                    alg2_correct = logical(0)
                                )
                                
                                for (fold in common_folds) {
                                    fold1_data <- pred1[pred1$fold == fold, ]
                                    fold2_data <- pred2[pred2$fold == fold, ]
                                    
                                    # Match by row (assuming same test samples in each fold)
                                    if (nrow(fold1_data) == nrow(fold2_data)) {
                                        paired_results <- rbind(paired_results, data.frame(
                                            alg1_correct = fold1_data$correct,
                                            alg2_correct = fold2_data$correct
                                        ))
                                    }
                                }
                                
                                if (nrow(paired_results) >= 20) {
                                    # Create 2x2 table for McNemar's test
                                    table_data <- table(paired_results$alg1_correct, paired_results$alg2_correct)
                                    
                                    if (nrow(table_data) == 2 && ncol(table_data) == 2) {
                                        # Check if McNemar's test is appropriate (discordant pairs > 0)
                                        discordant_pairs <- table_data[1,2] + table_data[2,1]
                                        if (discordant_pairs > 0) {
                                            # McNemar's test
                                            mcnemar_result <- mcnemar.test(table_data, correct = TRUE)
                                            test_results$statistic[i] <- mcnemar_result$statistic
                                            test_results$p_value[i] <- mcnemar_result$p.value
                                            test_results$test_type[i] <- "McNemar"
                                        } else {
                                            # No differences between algorithms - perfect agreement
                                            test_results$statistic[i] <- 0
                                            test_results$p_value[i] <- 1.0
                                            test_results$test_type[i] <- "Perfect Agreement"
                                        }
                                    } else {
                                        # Fall back to paired t-test on accuracies if McNemar fails
                                        accuracies1 <- sapply(common_folds, function(f) {
                                            fold_data <- pred1[pred1$fold == f, ]
                                            mean(fold_data$correct)
                                        })
                                        accuracies2 <- sapply(common_folds, function(f) {
                                            fold_data <- pred2[pred2$fold == f, ]
                                            mean(fold_data$correct)
                                        })
                                        
                                        if (length(accuracies1) == length(accuracies2) && length(accuracies1) >= 3) {
                                            t_result <- t.test(accuracies1, accuracies2, paired = TRUE)
                                            test_results$statistic[i] <- t_result$statistic
                                            test_results$p_value[i] <- t_result$p.value
                                            test_results$test_type[i] <- "Paired t-test"
                                        }
                                    }
                                }
                            }
                        }
                        
                        # If no proper test could be performed, use Wilcoxon signed-rank test
                        if (test_results$p_value[i] == 0) {
                            acc1 <- mean(pred1$correct)
                            acc2 <- mean(pred2$correct)
                            
                            # Use difference in accuracies with permutation test
                            n_perms <- min(1000, choose(nrow(pred1) + nrow(pred2), nrow(pred1)))
                            if (n_perms >= 100) {
                                # Simple permutation test
                                obs_diff <- acc1 - acc2
                                combined_correct <- c(pred1$correct, pred2$correct)
                                n1 <- nrow(pred1)
                                
                                perm_diffs <- replicate(100, {
                                    perm_idx <- sample(length(combined_correct), n1)
                                    mean(combined_correct[perm_idx]) - mean(combined_correct[-perm_idx])
                                })
                                
                                p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
                                test_results$statistic[i] <- obs_diff
                                test_results$p_value[i] <- p_val
                                test_results$test_type[i] <- "Permutation"
                            }
                        }
                        
                    }, error = function(e) {
                        # If all tests fail, mark as unable to test
                        test_results$statistic[i] <- NA
                        test_results$p_value[i] <- 1.0  # Conservative approach
                        test_results$test_type[i] <- "Unable to test"
                        warning(paste("Statistical test failed for", test_results$comparison[i], ":", e$message))
                    })
                } else {
                    # Insufficient data for comparison
                    test_results$statistic[i] <- NA
                    test_results$p_value[i] <- 1.0
                    test_results$test_type[i] <- "Insufficient data"
                }
            }
            
            # Apply multiple comparison correction
            correction_method <- self$options$correction_method
            valid_p_values <- !is.na(test_results$p_value)
            
            if (correction_method != "none" && any(valid_p_values)) {
                test_results$p_adjusted <- test_results$p_value
                test_results$p_adjusted[valid_p_values] <- switch(correction_method,
                    "bonferroni" = pmin(1, test_results$p_value[valid_p_values] * sum(valid_p_values)),
                    "holm" = p.adjust(test_results$p_value[valid_p_values], method = "holm"),
                    "fdr" = p.adjust(test_results$p_value[valid_p_values], method = "BH"),
                    test_results$p_value[valid_p_values]
                )
            } else {
                test_results$p_adjusted <- test_results$p_value
            }
            
            # Determine significance
            test_results$significance <- ifelse(
                is.na(test_results$p_adjusted) | test_results$p_adjusted >= 0.05, 
                "No", 
                "Yes"
            )
            
            private$.statistical_tests <- test_results
        },

        .generate_ranking = function() {
            if (is.null(private$.performance_data)) return()
            
            # Get interpretability scores
            interp_scores <- sapply(names(private$.algorithms), function(alg) {
                private$.algorithms[[alg]]$interpretability
            })
            
            # Align with performance data
            algorithm_order <- match(private$.performance_data$algorithm, names(interp_scores))
            interp_scores_aligned <- interp_scores[algorithm_order]
            
            # Combine performance and interpretability
            performance_weight <- 1 - self$options$interpretability_weight
            interpretability_weight <- self$options$interpretability_weight
            
            # Get primary metric scores
            primary_metric <- self$options$primary_metric
            if (!primary_metric %in% names(private$.performance_data)) {
                primary_metric <- "accuracy"  # fallback
            }
            
            performance_scores <- private$.performance_data[[primary_metric]]
            
            # Calculate combined scores
            combined_scores <- performance_weight * performance_scores + 
                             interpretability_weight * interp_scores_aligned
            
            # Create ranking
            ranking_order <- order(combined_scores, decreasing = TRUE)
            
            # Generate recommendations based on rank
            n_algorithms <- length(ranking_order)
            recommendations <- c(
                "Recommended",
                if (n_algorithms > 1) "Good Alternative" else NULL,
                if (n_algorithms > 2) rep("Consider", n_algorithms - 2) else NULL
            )
            
            private$.ranking_data <- data.frame(
                rank = 1:n_algorithms,
                algorithm = private$.performance_data$algorithm[ranking_order],
                performance_score = performance_scores[ranking_order],
                interpretability_score = interp_scores_aligned[ranking_order],
                combined_score = combined_scores[ranking_order],
                recommendation = recommendations,
                stringsAsFactors = FALSE
            )
        },

        .populate_results = function() {
            # Always show algorithm summary if any algorithms were trained
            if (length(private$.algorithms) > 0) {
                private$.populate_algorithm_summary()
            }
            
            # Check visibility options before populating each result
            if (self$options$show_comparison_table) {
                private$.populate_comparison_table()
            }
            
            if (self$options$show_statistical_tests && self$options$statistical_testing) {
                private$.populate_statistical_tests()
            }
            
            if (self$options$show_ranking_table) {
                private$.populate_ranking_table()
            }
            
            if (self$options$show_clinical_recommendations) {
                private$.populate_clinical_recommendations()
            }
            
            # Save best model if requested
            if (self$options$save_best_models) {
                private$.save_best_model()
            }
            
            # Create ensemble if requested
            if (self$options$ensemble_best_models) {
                private$.create_ensemble()
            }
        },

        .populate_algorithm_summary = function() {
            if (length(private$.algorithms) == 0) return()
            
            algorithm_names <- names(private$.algorithms)
            algorithm_count <- length(algorithm_names)
            
            # Format validation method display
            validation_display <- switch(self$options$validation,
                "repeated_cv" = paste0("Repeated ", self$options$cv_repeats, "-fold CV (", self$options$cv_folds, " folds)"),
                "cv" = paste0(self$options$cv_folds, "-fold Cross-Validation"),
                "bootstrap" = paste0("Bootstrap (", self$options$bootstrap_samples, " samples)"),
                "holdout" = paste0("Hold-out (", round(self$options$test_split * 100), "% test set)"),
                self$options$validation
            )
            
            # Add tuned parameters information
            tuning_info <- ""
            if (self$options$tune_parameters && length(private$.tuned_parameters) > 0) {
                tuning_info <- "<h5>Parameter Tuning Results:</h5><ul>"
                for (alg_name in names(private$.tuned_parameters)) {
                    params <- private$.tuned_parameters[[alg_name]]
                    param_str <- paste(names(params), "=", sapply(params, function(x) round(x, 3)), collapse = ", ")
                    tuning_info <- paste0(tuning_info, "<li><strong>", alg_name, ":</strong> ", param_str, "</li>")
                }
                tuning_info <- paste0(tuning_info, "</ul>")
            }
            
            summary_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>",
                "<h4>Algorithm Comparison Summary</h4>",
                "<ul>",
                "<li><strong>Algorithms compared:</strong> ", algorithm_count, "</li>",
                "<li><strong>Selected algorithms:</strong> ", paste(algorithm_names, collapse = ", "), "</li>",
                "<li><strong>Validation method:</strong> ", validation_display, "</li>",
                "<li><strong>Primary metric:</strong> ", toupper(self$options$primary_metric), "</li>",
                "<li><strong>Clinical context:</strong> ", self$options$clinical_context, "</li>",
                "<li><strong>Interpretability weight:</strong> ", round(self$options$interpretability_weight * 100), "%</li>",
                if (self$options$tune_parameters) "<li><strong>Parameter tuning:</strong> Enabled</li>" else "",
                if (self$options$statistical_testing) paste0("<li><strong>Statistical testing:</strong> ", self$options$correction_method, " correction</li>") else "",
                "</ul>",
                tuning_info,
                "</div>"
            )
            
            self$results$algorithm_summary$setContent(summary_html)
        },

        .populate_comparison_table = function() {
            if (is.null(private$.performance_data)) return()
            
            for (i in 1:nrow(private$.performance_data)) {
                self$results$comparison_table$addRow(
                    rowKey = i,
                    values = list(
                        algorithm = private$.performance_data$algorithm[i],
                        accuracy = round(private$.performance_data$accuracy[i], 3),
                        bacc = round(private$.performance_data$bacc[i], 3),
                        auc = round(private$.performance_data$auc[i], 3),
                        sensitivity = round(private$.performance_data$sensitivity[i], 3),
                        specificity = round(private$.performance_data$specificity[i], 3),
                        f1_score = round(private$.performance_data$f1_score[i], 3),
                        computation_time = round(private$.performance_data$computation_time[i], 2)
                    )
                )
            }
        },

        .populate_statistical_tests = function() {
            if (is.null(private$.statistical_tests)) return()
            
            for (i in 1:nrow(private$.statistical_tests)) {
                # Format test type and statistic display
                stat_display <- if (is.na(private$.statistical_tests$statistic[i])) {
                    "N/A"
                } else {
                    round(private$.statistical_tests$statistic[i], 3)
                }
                
                # Combine comparison with test type for clarity
                comparison_display <- paste0(private$.statistical_tests$comparison[i], 
                                           " (", private$.statistical_tests$test_type[i], ")")
                
                self$results$statistical_tests$addRow(
                    rowKey = i,
                    values = list(
                        comparison = comparison_display,
                        statistic = stat_display,
                        p_value = round(private$.statistical_tests$p_value[i], 4),
                        p_adjusted = round(private$.statistical_tests$p_adjusted[i], 4),
                        significance = private$.statistical_tests$significance[i]
                    )
                )
            }
        },

        .populate_ranking_table = function() {
            if (is.null(private$.ranking_data)) return()
            
            for (i in 1:nrow(private$.ranking_data)) {
                self$results$ranking_table$addRow(
                    rowKey = i,
                    values = list(
                        rank = private$.ranking_data$rank[i],
                        algorithm = private$.ranking_data$algorithm[i],
                        performance_score = round(private$.ranking_data$performance_score[i], 3),
                        interpretability_score = round(private$.ranking_data$interpretability_score[i], 3),
                        combined_score = round(private$.ranking_data$combined_score[i], 3),
                        recommendation = private$.ranking_data$recommendation[i]
                    )
                )
            }
        },

        .populate_clinical_recommendations = function() {
            if (is.null(private$.ranking_data)) return()
            
            top_algorithm <- private$.ranking_data$algorithm[1]
            top_combined_score <- private$.ranking_data$combined_score[1]
            top_performance_score <- private$.ranking_data$performance_score[1]
            top_interpretability_score <- private$.ranking_data$interpretability_score[1]
            
            clinical_context <- self$options$clinical_context
            interp_weight <- self$options$interpretability_weight
            
            # Context-specific recommendations
            context_advice <- switch(clinical_context,
                "diagnosis" = "For diagnostic applications, consider both accuracy and interpretability for clinical acceptance. High interpretability facilitates physician trust and adoption.",
                "screening" = "For screening applications, prioritize sensitivity to minimize false negatives. Consider the balance between detection rate and false positive burden.",
                "prognosis" = "For prognostic modeling, balance predictive power with clinical interpretability. Physicians need to understand risk factors for patient counseling.",
                "treatment" = "For treatment selection, interpretability is crucial for clinical decision-making. Physicians must understand the reasoning behind treatment recommendations.",
                "risk" = "For risk assessment, consider both discriminative ability and calibration. Risk stratification tools must be both accurate and trustworthy."
            )
            
            # Additional recommendations based on interpretability weight
            weight_advice <- if (interp_weight > 0.6) {
                "High interpretability weighting suggests preference for transparent, explainable models."
            } else if (interp_weight < 0.3) {
                "Low interpretability weighting prioritizes predictive performance over model transparency."
            } else {
                "Balanced weighting considers both performance and interpretability equally important."
            }
            
            recommendations_html <- paste0(
                "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 5px;'>",
                "<h4>Clinical Algorithm Recommendations</h4>",
                "<div style='background-color: #e8f5e8; padding: 10px; border-radius: 3px; margin: 10px 0;'>",
                "<p><strong>🏆 Recommended Algorithm:</strong> ", top_algorithm, "</p>",
                "<p><strong>Combined Score:</strong> ", round(top_combined_score, 3), 
                " (Performance: ", round(top_performance_score, 3), 
                ", Interpretability: ", round(top_interpretability_score, 3), ")</p>",
                "</div>",
                "<p><strong>Clinical Context:</strong> ", clinical_context, "</p>",
                "<p><strong>Context-Specific Guidance:</strong> ", context_advice, "</p>",
                "<p><strong>Weighting Strategy:</strong> ", weight_advice, "</p>",
                "<h4>Implementation Considerations:</h4>",
                "<ul>",
                "<li><strong>Performance Priority:</strong> ", round((1 - interp_weight) * 100), "%</li>",
                "<li><strong>Interpretability Priority:</strong> ", round(interp_weight * 100), "%</li>",
                "<li><strong>Validation:</strong> External validation required before clinical deployment</li>",
                "<li><strong>Regulatory:</strong> Consider FDA/regulatory requirements for your clinical setting</li>",
                "<li><strong>Integration:</strong> Assess compatibility with existing clinical workflows</li>",
                "<li><strong>Training:</strong> Ensure adequate staff training for algorithm interpretation</li>",
                "</ul>",
                "<h4>Next Steps:</h4>",
                "<ol>",
                "<li>Validate top-ranked algorithm on independent dataset</li>",
                "<li>Conduct prospective clinical evaluation</li>",
                "<li>Assess impact on clinical decision-making and outcomes</li>",
                "<li>Implement with appropriate safeguards and monitoring</li>",
                "</ol>",
                "</div>"
            )
            
            self$results$clinical_recommendations$setContent(recommendations_html)
        },

        .save_best_model = function() {
            # Save the best performing model to disk
            if (is.null(private$.ranking_data) || nrow(private$.ranking_data) == 0) return()
            
            top_algorithm <- private$.ranking_data$algorithm[1]
            
            # Find the model in our algorithms list
            if (top_algorithm %in% names(private$.algorithms)) {
                model_info <- private$.algorithms[[top_algorithm]]
                
                if (!is.null(model_info$model)) {
                    # Create filename
                    safe_name <- tolower(gsub(" ", "_", top_algorithm))
                    filename <- paste0("best_model_", safe_name, "_", format(Sys.Date(), "%Y%m%d"), ".rds")
                    
                    tryCatch({
                        # Save model
                        saveRDS(model_info$model, file = filename)
                        
                        # Update summary to indicate model was saved
                        current_content <- self$results$algorithm_summary$content
                        updated_content <- gsub("</div>$", 
                                              paste0("<p><strong>Model Saved:</strong> ", filename, "</p></div>"),
                                              current_content)
                        self$results$algorithm_summary$setContent(updated_content)
                        
                    }, error = function(e) {
                        warning(paste("Failed to save model:", e$message))
                    })
                }
            }
        },
        
        .create_ensemble = function() {
            # Create an ensemble of the top-performing models
            if (is.null(private$.ranking_data) || nrow(private$.ranking_data) < 2) return()
            
            # Select top 3 models (or fewer if less available)
            n_models <- min(3, nrow(private$.ranking_data))
            top_algorithms <- private$.ranking_data$algorithm[1:n_models]
            top_scores <- private$.ranking_data$combined_score[1:n_models]
            
            # Normalize scores to create weights
            weights <- top_scores / sum(top_scores)
            
            # Store ensemble information
            ensemble_info <- list(
                algorithms = top_algorithms,
                weights = weights,
                models = list()
            )
            
            # Collect models
            for (i in seq_along(top_algorithms)) {
                alg_name <- top_algorithms[i]
                if (alg_name %in% names(private$.algorithms)) {
                    ensemble_info$models[[alg_name]] <- private$.algorithms[[alg_name]]$model
                }
            }
            
            # Update summary with ensemble information
            if (length(ensemble_info$models) > 0) {
                ensemble_text <- paste0(
                    "<div style='background-color: #fff3cd; padding: 10px; border-radius: 3px; margin-top: 10px;'>",
                    "<p><strong>Ensemble Created:</strong></p>",
                    "<ul>"
                )
                
                for (i in seq_along(top_algorithms)) {
                    ensemble_text <- paste0(ensemble_text,
                        "<li>", top_algorithms[i], " (weight: ", round(weights[i], 3), ")</li>"
                    )
                }
                
                ensemble_text <- paste0(ensemble_text,
                    "</ul>",
                    "<p>Note: Ensemble predictions would use weighted voting from these models.</p>",
                    "</div>"
                )
                
                current_content <- self$results$algorithm_summary$content
                updated_content <- gsub("</div>$", 
                                      paste0("</div>", ensemble_text),
                                      current_content)
                self$results$algorithm_summary$setContent(updated_content)
            }
        },

        # Visualization functions
        .performance_plot = function(image, ggtheme, ...) {
            # Create box plots comparing algorithm performance
            if (is.null(private$.performance_data) || nrow(private$.performance_data) == 0) {
                return(FALSE)
            }
            
            # Check for valid performance data
            primary_metric <- self$options$primary_metric
            if (!primary_metric %in% names(private$.performance_data)) {
                primary_metric <- "accuracy"
            }
            
            # Ensure we have valid numeric data
            metric_values <- private$.performance_data[[primary_metric]]
            if (all(is.na(metric_values)) || all(is.infinite(metric_values))) {
                return(FALSE)
            }
            
            tryCatch({
                # Reshape data for plotting
                plot_data <- private$.performance_data
                
                # Convert primary metric name for display
                metric_display <- switch(primary_metric,
                    "bacc" = "Balanced Accuracy",
                    "auc" = "AUC",
                    "accuracy" = "Accuracy",
                    "sensitivity" = "Sensitivity",
                    "specificity" = "Specificity",
                    "f1" = "F1 Score",
                    primary_metric
                )
                
                # Create bar plot with error bars (if we had multiple CV results, we'd show variance)
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = reorder(algorithm, get(primary_metric)), 
                                                             y = get(primary_metric), 
                                                             fill = algorithm)) +
                    ggplot2::geom_col(alpha = 0.8) +
                    ggplot2::coord_flip() +
                    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
                    ggplot2::labs(
                        title = paste("Algorithm Performance Comparison -", metric_display),
                        subtitle = paste("Validation Method:", self$options$validation),
                        x = "Algorithm",
                        y = metric_display,
                        fill = "Algorithm"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        legend.position = "none",
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        plot.subtitle = ggplot2::element_text(size = 11),
                        axis.text = ggplot2::element_text(size = 10),
                        axis.title = ggplot2::element_text(size = 11)
                    ) +
                    ggtheme
                
                # Add value labels on bars
                p <- p + ggplot2::geom_text(
                    ggplot2::aes(label = sprintf("%.3f", get(primary_metric))),
                    hjust = -0.1,
                    size = 3.5
                )
                
                print(p)
                return(TRUE)
                
            }, error = function(e) {
                return(FALSE)
            })
        },

        .roc_plot = function(image, ggtheme, ...) {
            # Create overlaid ROC curves for algorithm comparison
            if (is.null(private$.algorithms) || length(private$.algorithms) == 0) {
                return(FALSE)
            }
            
            # Check if pROC package is available
            if (!requireNamespace("pROC", quietly = TRUE)) {
                return(FALSE)
            }
            
            tryCatch({
                # We need to store ROC data during performance comparison
                # For now, create a simple comparison plot based on sensitivity/specificity
                if (is.null(private$.performance_data)) {
                    return(FALSE)
                }
                
                # Create ROC-style plot using available metrics
                plot_data <- private$.performance_data
                
                # Add diagonal reference line data
                ref_line <- data.frame(
                    fpr = c(0, 1),
                    tpr = c(0, 1)
                )
                
                # Create scatter plot of performance metrics
                p <- ggplot2::ggplot() +
                    # Reference line
                    ggplot2::geom_line(
                        data = ref_line,
                        ggplot2::aes(x = fpr, y = tpr),
                        linetype = "dashed",
                        color = "gray50",
                        size = 0.5
                    ) +
                    # Algorithm points
                    ggplot2::geom_point(
                        data = plot_data,
                        ggplot2::aes(x = 1 - specificity, y = sensitivity, color = algorithm),
                        size = 4
                    ) +
                    # Labels for points
                    ggplot2::geom_text(
                        data = plot_data,
                        ggplot2::aes(x = 1 - specificity, y = sensitivity, label = algorithm, color = algorithm),
                        hjust = -0.1,
                        vjust = -0.5,
                        size = 3
                    ) +
                    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
                    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
                    ggplot2::labs(
                        title = "ROC Space Comparison",
                        subtitle = paste("AUC values shown in legend"),
                        x = "False Positive Rate (1 - Specificity)",
                        y = "True Positive Rate (Sensitivity)",
                        color = "Algorithm"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        plot.subtitle = ggplot2::element_text(size = 11),
                        axis.text = ggplot2::element_text(size = 10),
                        axis.title = ggplot2::element_text(size = 11),
                        legend.position = "bottom"
                    ) +
                    ggtheme
                
                # Update legend to show AUC values
                auc_labels <- paste0(plot_data$algorithm, " (AUC: ", round(plot_data$auc, 3), ")")
                p <- p + ggplot2::scale_color_discrete(labels = auc_labels)
                
                print(p)
                return(TRUE)
                
            }, error = function(e) {
                return(FALSE)
            })
        }
    )
)
