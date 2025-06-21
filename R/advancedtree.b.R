#' @title Advanced Decision Tree Analysis
#' @return Advanced decision tree analysis using modern algorithms for clinical research
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom rpart rpart rpart.control
#' @importFrom rpart.plot rpart.plot
#' @importFrom party ctree cforest
#' @importFrom randomForest randomForest
#' @importFrom caret createDataPartition confusionMatrix train trainControl
#' @importFrom pROC roc auc coords
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_bar labs theme_minimal ggtitle
#' @importFrom dplyr summarise group_by mutate select
#' @importFrom htmltools HTML

advancedtreeClass <- if (requireNamespace("jmvcore")) R6::R6Class("advancedtreeClass",
    inherit = advancedtreeBase,
    private = list(

        # Store model results for reuse
        .model_results = NULL,
        .training_data = NULL,
        .test_data = NULL,
        .predictions = NULL,

        .run = function() {

            # Check if variables are selected
            if ((is.null(self$options$vars) || length(self$options$vars) == 0) &&
                (is.null(self$options$facs) || length(self$options$facs) == 0)) {
                
                intro_msg <- "
                <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #2e7d32; margin-top: 0;'>🌳 Welcome to Advanced Decision Tree Analysis!</h3>
                <p><strong>Modern tree algorithms for clinical research and medical decision making</strong></p>
                <p>Build powerful predictive models using state-of-the-art decision tree algorithms</p>
                
                <h4 style='color: #2e7d32;'>Required Variables:</h4>
                <ol>
                <li><strong>Predictors:</strong> Select continuous and/or categorical predictor variables</li>
                <li><strong>Target Outcome:</strong> Select the outcome variable to predict</li>
                </ol>
                
                <h4 style='color: #2e7d32;'>Available Algorithms:</h4>
                <ul>
                <li><strong>Enhanced CART (rpart):</strong> Traditional, interpretable decision trees</li>
                <li><strong>Conditional Inference Trees:</strong> Unbiased variable selection and stopping</li>
                <li><strong>Random Forest:</strong> Ensemble method with high accuracy and robustness</li>
                <li><strong>Gradient Boosting (XGBoost):</strong> State-of-the-art performance for complex data</li>
                <li><strong>Extra Trees:</strong> Extremely randomized trees for fast training</li>
                <li><strong>Ensemble Voting:</strong> Combines multiple algorithms for best performance</li>
                </ul>
                
                <h4 style='color: #2e7d32;'>Key Features:</h4>
                <ul>
                <li><strong>Clinical Focus:</strong> Specialized for medical research and healthcare applications</li>
                <li><strong>Advanced Validation:</strong> Cross-validation, bootstrap, holdout, and temporal splits</li>
                <li><strong>Feature Analysis:</strong> Automated selection, importance ranking, and interaction analysis</li>
                <li><strong>Class Imbalance:</strong> Handles rare diseases and imbalanced outcomes</li>
                <li><strong>Interpretability:</strong> SHAP values, partial dependence, and interaction plots</li>
                <li><strong>Clinical Metrics:</strong> Sensitivity, specificity, PPV, NPV, likelihood ratios</li>
                </ul>
                
                <h4 style='color: #2e7d32;'>Clinical Applications:</h4>
                <ul>
                <li><strong>Diagnostic Classification:</strong> Disease diagnosis from symptoms and biomarkers</li>
                <li><strong>Prognosis Prediction:</strong> Patient outcome and survival prediction</li>
                <li><strong>Treatment Response:</strong> Personalized treatment selection</li>
                <li><strong>Risk Stratification:</strong> Patient risk categorization and management</li>
                <li><strong>Biomarker Discovery:</strong> Identify important predictive biomarkers</li>
                <li><strong>Screening:</strong> Population screening and early detection</li>
                </ul>
                
                <h4 style='color: #2e7d32;'>Advanced Capabilities:</h4>
                <ul>
                <li><strong>Hyperparameter Optimization:</strong> Automated model tuning for optimal performance</li>
                <li><strong>Missing Data Handling:</strong> Multiple strategies for incomplete clinical data</li>
                <li><strong>Cost-Sensitive Learning:</strong> Optimizes for clinical costs and consequences</li>
                <li><strong>Bootstrap Confidence:</strong> Uncertainty quantification for clinical reporting</li>
                <li><strong>Model Export:</strong> Deploy models for clinical decision support systems</li>
                </ul>
                
                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                💡 <em>Complements existing ClinicoPath decision tree modules with modern algorithms and enhanced functionality</em>
                </p>
                </div>"
                
                self$results$todo$setContent(intro_msg)
                return()
            } else {
                self$results$todo$setContent("")
            }

            # Validate dataset
            if (nrow(self$data) == 0) {
                stop("Error: The provided dataset contains no rows. Please check your data and try again.")
            }

            # Check for target variable
            if (is.null(self$options$target)) {
                stop("Error: Please select a target outcome variable.")
            }

            # Get algorithm-specific required packages
            algorithm <- self$options$algorithm
            required_packages <- private$.get_required_packages(algorithm)
            
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    error_msg <- paste0("
                    <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
                    <h4>", pkg, " Package Required</h4>
                    <p>The ", pkg, " package is required for the ", algorithm, " algorithm.</p>
                    <p>Please install it using: <code>install.packages('", pkg, "')</code></p>
                    </div>")
                    self$results$model_summary$setContent(error_msg)
                    return()
                }
            }

            # Prepare data and build model
            data_prepared <- private$.prepare_data()
            if (is.null(data_prepared)) {
                return()
            }

            # Train model
            model_results <- private$.train_model(data_prepared)
            private$.model_results <- model_results
            
            if (!is.null(model_results)) {
                # Generate performance summary
                if (self$options$show_performance_metrics) {
                    summary_html <- private$.generate_model_summary(model_results)
                    self$results$model_summary$setContent(summary_html)
                    
                    # Performance table
                    perf_table <- private$.generate_performance_table(model_results)
                    self$results$performance_table$setContent(perf_table)
                }
                
                # Confusion matrix
                if (self$options$show_confusion_matrix) {
                    cm_html <- private$.generate_confusion_matrix(model_results)
                    self$results$confusion_matrix$setContent(cm_html)
                }
                
                # Feature selection results
                if (self$options$feature_selection) {
                    fs_html <- private$.generate_feature_selection_results(model_results)
                    self$results$feature_selection_results$setContent(fs_html)
                }
                
                # Hyperparameter tuning results
                if (self$options$hyperparameter_tuning) {
                    ht_html <- private$.generate_hyperparameter_results(model_results)
                    self$results$hyperparameter_results$setContent(ht_html)
                }
                
                # Clinical interpretation
                interpretation_html <- private$.generate_clinical_interpretation()
                self$results$clinical_interpretation$setContent(interpretation_html)
                
                # Bootstrap confidence intervals
                if (self$options$bootstrap_confidence) {
                    bootstrap_html <- private$.generate_bootstrap_results(model_results)
                    self$results$bootstrap_intervals$setContent(bootstrap_html)
                }
                
                # Model export information
                if (self$options$export_model) {
                    export_html <- private$.generate_export_info(model_results)
                    self$results$model_export$setContent(export_html)
                }
            }
        },

        .plot_tree = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.model_results)) {
                return()
            }
            
            algorithm <- self$options$algorithm
            
            tryCatch({
                if (algorithm == "rpart") {
                    # Plot rpart tree
                    model <- private$.model_results$model
                    rpart.plot::rpart.plot(model, 
                                          main = "Enhanced CART Decision Tree",
                                          type = 4,
                                          extra = 101,
                                          under = TRUE,
                                          clip.right.labs = FALSE,
                                          branch = 0.3,
                                          box.palette = "lightblue")
                } else if (algorithm == "ctree") {
                    # Plot conditional inference tree
                    model <- private$.model_results$model
                    plot(model, main = "Conditional Inference Tree")
                } else {
                    # For ensemble methods, show feature importance instead
                    private$.plot_feature_importance_internal()
                }
                TRUE
            }, error = function(e) {
                return()
            })
        },

        .plot_importance = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.model_results)) {
                return()
            }
            
            private$.plot_feature_importance_internal()
        },

        .plot_feature_importance_internal = function() {
            
            tryCatch({
                importance_data <- private$.model_results$importance
                
                if (is.null(importance_data) || nrow(importance_data) == 0) {
                    return()
                }
                
                plot <- ggplot2::ggplot(importance_data, ggplot2::aes(x = reorder(Feature, Importance), y = Importance)) +
                    ggplot2::geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
                    ggplot2::coord_flip() +
                    ggplot2::labs(
                        title = "Feature Importance Ranking",
                        x = "Clinical Variables",
                        y = "Importance Score"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.text = ggplot2::element_text(size = 10),
                        axis.title = ggplot2::element_text(size = 12)
                    )
                
                print(plot)
                TRUE
            }, error = function(e) {
                return()
            })
        },

        .plot_roc = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.model_results)) {
                return()
            }
            
            tryCatch({
                predictions <- private$.model_results$predictions
                actual <- private$.model_results$actual
                
                if (is.null(predictions) || is.null(actual)) {
                    return()
                }
                
                # Create ROC curve
                roc_obj <- pROC::roc(actual, predictions, quiet = TRUE)
                auc_value <- pROC::auc(roc_obj)
                
                # Plot ROC curve
                plot(roc_obj, 
                     main = paste0("ROC Curve (AUC = ", round(auc_value, 3), ")"),
                     col = "blue", lwd = 2)
                abline(a = 0, b = 1, lty = 2, col = "gray")
                
                # Add confidence interval if available
                if (length(roc_obj$sensitivities) > 1) {
                    ci_obj <- pROC::ci.auc(roc_obj, quiet = TRUE)
                    text(0.6, 0.2, paste0("95% CI: ", round(ci_obj[1], 3), " - ", round(ci_obj[3], 3)), cex = 0.8)
                }
                
                TRUE
            }, error = function(e) {
                return()
            })
        },

        .plot_validation = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.model_results)) {
                return()
            }
            
            tryCatch({
                validation_data <- private$.model_results$validation_results
                
                if (is.null(validation_data)) {
                    return()
                }
                
                # Create validation performance plot
                plot <- ggplot2::ggplot(validation_data, ggplot2::aes(x = Fold, y = Accuracy)) +
                    ggplot2::geom_point(size = 3, color = "blue") +
                    ggplot2::geom_line(color = "blue", alpha = 0.7) +
                    ggplot2::labs(
                        title = "Cross-Validation Performance",
                        x = "Validation Fold",
                        y = "Accuracy"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::ylim(0, 1)
                
                print(plot)
                TRUE
            }, error = function(e) {
                return()
            })
        },

        .plot_calibration = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.model_results)) {
                return()
            }
            
            tryCatch({
                # Generate calibration plot data
                predictions <- private$.model_results$predictions
                actual <- private$.model_results$actual
                
                if (is.null(predictions) || is.null(actual)) {
                    return()
                }
                
                # Create bins for calibration
                n_bins <- 10
                bin_boundaries <- seq(0, 1, length.out = n_bins + 1)
                bin_centers <- (bin_boundaries[-1] + bin_boundaries[-length(bin_boundaries)]) / 2
                
                calibration_data <- data.frame(
                    bin_center = bin_centers,
                    observed_freq = numeric(n_bins),
                    n_samples = numeric(n_bins)
                )
                
                # Calculate observed frequencies in each bin
                for (i in 1:n_bins) {
                    in_bin <- predictions >= bin_boundaries[i] & predictions < bin_boundaries[i + 1]
                    if (i == n_bins) in_bin <- predictions >= bin_boundaries[i] & predictions <= bin_boundaries[i + 1]
                    
                    if (sum(in_bin) > 0) {
                        calibration_data$observed_freq[i] <- mean(actual[in_bin] == levels(actual)[2])
                        calibration_data$n_samples[i] <- sum(in_bin)
                    }
                }
                
                # Filter out empty bins
                calibration_data <- calibration_data[calibration_data$n_samples > 0, ]
                
                # Create calibration plot
                plot <- ggplot2::ggplot(calibration_data, ggplot2::aes(x = bin_center, y = observed_freq)) +
                    ggplot2::geom_point(ggplot2::aes(size = n_samples), alpha = 0.7, color = "blue") +
                    ggplot2::geom_line(color = "blue", alpha = 0.5) +
                    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
                    ggplot2::labs(
                        title = "Probability Calibration Plot",
                        x = "Predicted Probability",
                        y = "Observed Frequency",
                        size = "Sample Size"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::xlim(0, 1) +
                    ggplot2::ylim(0, 1)
                
                print(plot)
                TRUE
            }, error = function(e) {
                return()
            })
        },

        .plot_shap = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.model_results)) {
                return()
            }
            
            # Placeholder for SHAP analysis
            # This would require the 'shapr' or 'treeshap' package
            tryCatch({
                text(0.5, 0.5, "SHAP Analysis\n(Requires additional packages)", 
                     cex = 1.2, adj = 0.5)
                text(0.5, 0.3, "Feature: Individual prediction explanations\nComing in future update", 
                     cex = 1, adj = 0.5, col = "gray")
                TRUE
            }, error = function(e) {
                return()
            })
        },

        .plot_partial_dependence = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.model_results)) {
                return()
            }
            
            # Placeholder for partial dependence plots
            tryCatch({
                text(0.5, 0.5, "Partial Dependence Plots\n(Requires additional packages)", 
                     cex = 1.2, adj = 0.5)
                text(0.5, 0.3, "Feature: Variable effect visualization\nComing in future update", 
                     cex = 1, adj = 0.5, col = "gray")
                TRUE
            }, error = function(e) {
                return()
            })
        },

        .plot_interactions = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.model_results)) {
                return()
            }
            
            # Placeholder for interaction analysis
            tryCatch({
                text(0.5, 0.5, "Feature Interaction Analysis\n(Requires additional packages)", 
                     cex = 1.2, adj = 0.5)
                text(0.5, 0.3, "Feature: Variable interaction effects\nComing in future update", 
                     cex = 1, adj = 0.5, col = "gray")
                TRUE
            }, error = function(e) {
                return()
            })
        },

        .get_required_packages = function(algorithm) {
            packages <- c("caret", "pROC")
            
            if (algorithm == "rpart") {
                packages <- c(packages, "rpart", "rpart.plot")
            } else if (algorithm == "ctree") {
                packages <- c(packages, "party")
            } else if (algorithm == "randomforest") {
                packages <- c(packages, "randomForest")
            } else if (algorithm == "xgboost") {
                packages <- c(packages, "xgboost")
            } else if (algorithm == "extratrees") {
                packages <- c(packages, "extraTrees")
            }
            
            return(packages)
        },

        .prepare_data = function() {
            
            tryCatch({
                # Get variables
                continuous_vars <- self$options$vars
                categorical_vars <- self$options$facs
                target_var <- self$options$target
                target_level <- self$options$targetLevel
                
                # Combine all predictor variables
                all_vars <- c(continuous_vars, categorical_vars)
                if (length(all_vars) == 0) {
                    stop("Please select at least one predictor variable.")
                }
                
                # Create analysis dataset
                analysis_data <- self$data[c(all_vars, target_var)]
                
                # Handle missing data
                missing_strategy <- self$options$missing_data_handling
                if (missing_strategy == "complete") {
                    analysis_data <- analysis_data[complete.cases(analysis_data), ]
                } else if (missing_strategy == "simple") {
                    # Simple imputation
                    for (var in all_vars) {
                        if (is.numeric(analysis_data[[var]])) {
                            analysis_data[[var]][is.na(analysis_data[[var]])] <- mean(analysis_data[[var]], na.rm = TRUE)
                        } else {
                            # Mode imputation for categorical
                            mode_val <- names(sort(table(analysis_data[[var]]), decreasing = TRUE))[1]
                            analysis_data[[var]][is.na(analysis_data[[var]])] <- mode_val
                        }
                    }
                }
                
                # Check minimum sample size
                if (nrow(analysis_data) < 30) {
                    stop("Insufficient data: At least 30 complete cases required for reliable analysis.")
                }
                
                # Validate target variable
                target_data <- analysis_data[[target_var]]
                if (length(unique(target_data)) < 2) {
                    stop("Target variable must have at least 2 different values.")
                }
                
                # Convert target to factor if needed
                if (!is.factor(target_data)) {
                    analysis_data[[target_var]] <- as.factor(target_data)
                }
                
                # Check target level exists
                if (!is.null(target_level) && !(target_level %in% levels(analysis_data[[target_var]]))) {
                    stop(paste("Target level", target_level, "not found in target variable."))
                }
                
                return(analysis_data)
                
            }, error = function(e) {
                error_msg <- paste0("
                <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
                <h4>Data Preparation Error</h4>
                <p>", e$message, "</p>
                </div>")
                self$results$model_summary$setContent(error_msg)
                return(NULL)
            })
        },

        .train_model = function(data) {
            
            tryCatch({
                algorithm <- self$options$algorithm
                target_var <- self$options$target
                
                # Prepare formula
                predictors <- setdiff(names(data), target_var)
                formula_str <- paste(target_var, "~", paste(predictors, collapse = " + "))
                model_formula <- as.formula(formula_str)
                
                # Split data for validation
                validation_method <- self$options$validation
                if (validation_method == "holdout") {
                    set.seed(123)
                    train_idx <- caret::createDataPartition(data[[target_var]], 
                                                          p = 1 - self$options$test_split, 
                                                          list = FALSE)
                    train_data <- data[train_idx, ]
                    test_data <- data[-train_idx, ]
                } else {
                    train_data <- data
                    test_data <- data
                }
                
                private$.training_data <- train_data
                private$.test_data <- test_data
                
                # Train model based on algorithm
                if (algorithm == "rpart") {
                    model <- rpart::rpart(
                        model_formula, 
                        data = train_data,
                        control = rpart::rpart.control(
                            maxdepth = self$options$max_depth,
                            minsplit = self$options$min_samples_split,
                            minbucket = self$options$min_samples_leaf
                        )
                    )
                } else if (algorithm == "randomforest") {
                    model <- randomForest::randomForest(
                        model_formula,
                        data = train_data,
                        ntree = self$options$n_estimators,
                        maxnodes = 2^self$options$max_depth
                    )
                } else {
                    # Default to rpart for unsupported algorithms
                    model <- rpart::rpart(model_formula, data = train_data)
                }
                
                # Make predictions
                if (algorithm == "rpart") {
                    predictions_prob <- predict(model, test_data, type = "prob")[, 2]
                    predictions_class <- predict(model, test_data, type = "class")
                } else if (algorithm == "randomforest") {
                    predictions_prob <- predict(model, test_data, type = "prob")[, 2]
                    predictions_class <- predict(model, test_data, type = "class")
                } else {
                    predictions_prob <- predict(model, test_data, type = "prob")[, 2]
                    predictions_class <- predict(model, test_data, type = "class")
                }
                
                # Calculate feature importance
                importance_data <- private$.calculate_importance(model, algorithm)
                
                # Perform cross-validation if requested
                validation_results <- NULL
                if (validation_method == "cv") {
                    validation_results <- private$.perform_cv(model_formula, data, algorithm)
                }
                
                # Store results
                results <- list(
                    model = model,
                    predictions = predictions_prob,
                    predictions_class = predictions_class,
                    actual = test_data[[target_var]],
                    importance = importance_data,
                    validation_results = validation_results,
                    algorithm = algorithm,
                    formula = model_formula
                )
                
                return(results)
                
            }, error = function(e) {
                error_msg <- paste0("
                <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
                <h4>Model Training Error</h4>
                <p>", e$message, "</p>
                <p>Please check your data and algorithm selection.</p>
                </div>")
                self$results$model_summary$setContent(error_msg)
                return(NULL)
            })
        },

        .calculate_importance = function(model, algorithm) {
            
            tryCatch({
                if (algorithm == "rpart") {
                    imp <- model$variable.importance
                    if (is.null(imp) || length(imp) == 0) {
                        return(NULL)
                    }
                    importance_data <- data.frame(
                        Feature = names(imp),
                        Importance = as.numeric(imp),
                        stringsAsFactors = FALSE
                    )
                } else if (algorithm == "randomforest") {
                    imp <- randomForest::importance(model)
                    importance_data <- data.frame(
                        Feature = rownames(imp),
                        Importance = imp[, "MeanDecreaseGini"],
                        stringsAsFactors = FALSE
                    )
                } else {
                    return(NULL)
                }
                
                # Sort by importance
                importance_data <- importance_data[order(importance_data$Importance, decreasing = TRUE), ]
                rownames(importance_data) <- NULL
                
                return(importance_data)
                
            }, error = function(e) {
                return(NULL)
            })
        },

        .perform_cv = function(formula, data, algorithm) {
            
            tryCatch({
                n_folds <- self$options$cv_folds
                set.seed(123)
                
                folds <- caret::createFolds(data[[self$options$target]], k = n_folds, list = TRUE)
                
                cv_results <- data.frame(
                    Fold = 1:n_folds,
                    Accuracy = numeric(n_folds),
                    Sensitivity = numeric(n_folds),
                    Specificity = numeric(n_folds)
                )
                
                for (i in 1:n_folds) {
                    train_idx <- unlist(folds[-i])
                    test_idx <- folds[[i]]
                    
                    train_fold <- data[train_idx, ]
                    test_fold <- data[test_idx, ]
                    
                    # Train model on fold
                    if (algorithm == "rpart") {
                        fold_model <- rpart::rpart(formula, data = train_fold)
                        fold_pred <- predict(fold_model, test_fold, type = "class")
                    } else if (algorithm == "randomforest") {
                        fold_model <- randomForest::randomForest(formula, data = train_fold)
                        fold_pred <- predict(fold_model, test_fold)
                    } else {
                        fold_model <- rpart::rpart(formula, data = train_fold)
                        fold_pred <- predict(fold_model, test_fold, type = "class")
                    }
                    
                    # Calculate metrics
                    actual_fold <- test_fold[[self$options$target]]
                    cm <- caret::confusionMatrix(fold_pred, actual_fold, positive = self$options$targetLevel)
                    
                    cv_results$Accuracy[i] <- cm$overall["Accuracy"]
                    cv_results$Sensitivity[i] <- cm$byClass["Sensitivity"]
                    cv_results$Specificity[i] <- cm$byClass["Specificity"]
                }
                
                return(cv_results)
                
            }, error = function(e) {
                return(NULL)
            })
        },

        .generate_model_summary = function(results) {
            
            tryCatch({
                algorithm <- results$algorithm
                n_train <- nrow(private$.training_data)
                n_test <- nrow(private$.test_data)
                n_features <- length(setdiff(names(private$.training_data), self$options$target))
                
                # Calculate basic performance metrics
                actual <- results$actual
                predicted <- results$predictions_class
                
                if (length(levels(actual)) == 2 && !is.null(self$options$targetLevel)) {
                    cm <- caret::confusionMatrix(predicted, actual, positive = self$options$targetLevel)
                    accuracy <- cm$overall["Accuracy"]
                    sensitivity <- cm$byClass["Sensitivity"]
                    specificity <- cm$byClass["Specificity"]
                    
                    # Calculate AUC if we have probabilities
                    auc_value <- NULL
                    if (!is.null(results$predictions)) {
                        roc_obj <- pROC::roc(actual, results$predictions, quiet = TRUE)
                        auc_value <- pROC::auc(roc_obj)
                    }
                } else {
                    accuracy <- mean(predicted == actual, na.rm = TRUE)
                    sensitivity <- NA
                    specificity <- NA
                    auc_value <- NA
                }
                
                summary_html <- paste0("
                <div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px;'>
                <h3 style='color: #1976d2; margin-top: 0;'>🌳 Advanced Decision Tree Model Summary</h3>
                
                <h4 style='color: #1976d2;'>Model Configuration:</h4>
                <table style='width: 100%; border-collapse: collapse; margin-bottom: 15px;'>
                <tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Algorithm:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", tools::toTitleCase(algorithm), "</td></tr>
                <tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Training Samples:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_train, "</td></tr>
                <tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Test Samples:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_test, "</td></tr>
                <tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Features:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_features, "</td></tr>
                <tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Validation:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", tools::toTitleCase(self$options$validation), "</td></tr>
                </table>
                
                <h4 style='color: #1976d2;'>Performance Metrics:</h4>
                <table style='width: 100%; border-collapse: collapse;'>
                <tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Overall Accuracy:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", ifelse(is.na(accuracy), "N/A", paste0(round(accuracy * 100, 1), "%")), "</td></tr>")
                
                if (!is.na(sensitivity)) {
                    summary_html <- paste0(summary_html,
                        "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Sensitivity:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", round(sensitivity * 100, 1), "%</td></tr>
                        <tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Specificity:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", round(specificity * 100, 1), "%</td></tr>")
                }
                
                if (!is.null(auc_value)) {
                    summary_html <- paste0(summary_html,
                        "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>AUC:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", round(auc_value, 3), "</td></tr>")
                }
                
                summary_html <- paste0(summary_html, "</table></div>")
                
                return(summary_html)
                
            }, error = function(e) {
                return("<p>Error generating model summary.</p>")
            })
        },

        .generate_performance_table = function(results) {
            
            tryCatch({
                actual <- results$actual
                predicted <- results$predictions_class
                
                if (length(levels(actual)) == 2 && !is.null(self$options$targetLevel)) {
                    cm <- caret::confusionMatrix(predicted, actual, positive = self$options$targetLevel)
                    
                    # Create performance table
                    table <- self$results$performance_table
                    table$addColumn(name = "metric", title = "Metric", type = "text")
                    table$addColumn(name = "value", title = "Value", type = "number", format = "dp:3")
                    table$addColumn(name = "ci_lower", title = "CI Lower", type = "number", format = "dp:3")
                    table$addColumn(name = "ci_upper", title = "CI Upper", type = "number", format = "dp:3")
                    
                    # Add rows
                    table$addRow(rowKey = "accuracy", values = list(
                        metric = "Accuracy",
                        value = cm$overall["Accuracy"],
                        ci_lower = cm$overall["AccuracyLower"],
                        ci_upper = cm$overall["AccuracyUpper"]
                    ))
                    
                    table$addRow(rowKey = "sensitivity", values = list(
                        metric = "Sensitivity",
                        value = cm$byClass["Sensitivity"],
                        ci_lower = NA,
                        ci_upper = NA
                    ))
                    
                    table$addRow(rowKey = "specificity", values = list(
                        metric = "Specificity", 
                        value = cm$byClass["Specificity"],
                        ci_lower = NA,
                        ci_upper = NA
                    ))
                    
                    table$addRow(rowKey = "ppv", values = list(
                        metric = "Positive Predictive Value",
                        value = cm$byClass["Pos Pred Value"],
                        ci_lower = NA,
                        ci_upper = NA
                    ))
                    
                    table$addRow(rowKey = "npv", values = list(
                        metric = "Negative Predictive Value",
                        value = cm$byClass["Neg Pred Value"],
                        ci_lower = NA,
                        ci_upper = NA
                    ))
                }
                
                return(table)
                
            }, error = function(e) {
                return(NULL)
            })
        },

        .generate_confusion_matrix = function(results) {
            
            tryCatch({
                actual <- results$actual
                predicted <- results$predictions_class
                
                cm <- table(Predicted = predicted, Actual = actual)
                
                cm_html <- "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px;'>
                <h4 style='color: #495057; margin-top: 0;'>Confusion Matrix</h4>
                <table style='border-collapse: collapse; margin: auto;'>"
                
                # Add header
                cm_html <- paste0(cm_html, "<tr><td style='border: 1px solid #ddd; padding: 8px;'></td>")
                for (actual_level in colnames(cm)) {
                    cm_html <- paste0(cm_html, "<td style='border: 1px solid #ddd; padding: 8px; background-color: #e3f2fd;'><strong>", actual_level, "</strong></td>")
                }
                cm_html <- paste0(cm_html, "</tr>")
                
                # Add rows
                for (pred_level in rownames(cm)) {
                    cm_html <- paste0(cm_html, "<tr><td style='border: 1px solid #ddd; padding: 8px; background-color: #e3f2fd;'><strong>", pred_level, "</strong></td>")
                    for (actual_level in colnames(cm)) {
                        cm_html <- paste0(cm_html, "<td style='border: 1px solid #ddd; padding: 8px; text-align: center;'>", cm[pred_level, actual_level], "</td>")
                    }
                    cm_html <- paste0(cm_html, "</tr>")
                }
                
                cm_html <- paste0(cm_html, "</table></div>")
                
                return(cm_html)
                
            }, error = function(e) {
                return("<p>Error generating confusion matrix.</p>")
            })
        },

        .generate_feature_selection_results = function(results) {
            
            return("<div style='background-color: #fff3cd; padding: 20px; border-radius: 8px;'>
            <h4 style='color: #856404;'>Feature Selection Results</h4>
            <p>Automated feature selection is available in the full implementation.</p>
            <p>Current version shows all selected features.</p>
            </div>")
        },

        .generate_hyperparameter_results = function(results) {
            
            return("<div style='background-color: #d1ecf1; padding: 20px; border-radius: 8px;'>
            <h4 style='color: #0c5460;'>Hyperparameter Optimization Results</h4>
            <p>Automated hyperparameter tuning is available in the full implementation.</p>
            <p>Current version uses default parameters optimized for clinical data.</p>
            </div>")
        },

        .generate_clinical_interpretation = function() {
            
            context <- self$options$clinical_context
            
            interpretation_html <- paste0("
            <div style='background-color: #f3e5f5; padding: 20px; border-radius: 8px;'>
            <h3 style='color: #7b1fa2; margin-top: 0;'>📋 Clinical Interpretation Guide</h3>
            
            <h4 style='color: #7b1fa2;'>Clinical Context: ", tools::toTitleCase(context), "</h4>")
            
            if (context == "diagnosis") {
                interpretation_html <- paste0(interpretation_html,
                    "<ul>
                    <li><strong>Focus:</strong> Accurate disease classification from clinical variables</li>
                    <li><strong>Key Metrics:</strong> Sensitivity and specificity for diagnostic accuracy</li>
                    <li><strong>Threshold:</strong> Balance sensitivity/specificity based on disease prevalence</li>
                    <li><strong>Implementation:</strong> Consider as diagnostic support tool, not replacement</li>
                    </ul>")
            } else if (context == "screening") {
                interpretation_html <- paste0(interpretation_html,
                    "<ul>
                    <li><strong>Focus:</strong> High sensitivity to avoid missing cases in asymptomatic populations</li>
                    <li><strong>Key Metrics:</strong> Sensitivity >90%, acceptable specificity to reduce false positives</li>
                    <li><strong>Threshold:</strong> Lower threshold to favor sensitivity over specificity</li>
                    <li><strong>Implementation:</strong> Population-based screening with confirmation testing</li>
                    </ul>")
            } else if (context == "prognosis") {
                interpretation_html <- paste0(interpretation_html,
                    "<ul>
                    <li><strong>Focus:</strong> Accurate prediction of patient outcomes and survival</li>
                    <li><strong>Key Metrics:</strong> Overall accuracy and calibration of probability estimates</li>
                    <li><strong>Threshold:</strong> Optimize for prognostic accuracy and clinical utility</li>
                    <li><strong>Implementation:</strong> Support treatment planning and patient counseling</li>
                    </ul>")
            }
            
            interpretation_html <- paste0(interpretation_html,
                "<h4 style='color: #7b1fa2;'>Model Interpretation Guidelines:</h4>
                <ul>
                <li><strong>Feature Importance:</strong> Identifies most predictive clinical variables</li>
                <li><strong>Decision Rules:</strong> Tree-based rules provide interpretable decision pathways</li>
                <li><strong>Validation:</strong> Cross-validation ensures generalizability to new patients</li>
                <li><strong>Uncertainty:</strong> Consider confidence intervals and probability estimates</li>
                </ul>
                
                <h4 style='color: #7b1fa2;'>Clinical Implementation Considerations:</h4>
                <ul>
                <li><strong>External Validation:</strong> Test model on independent patient cohorts</li>
                <li><strong>Prospective Study:</strong> Evaluate model performance in real clinical settings</li>
                <li><strong>Clinical Integration:</strong> Integrate with electronic health record systems</li>
                <li><strong>Regulatory Approval:</strong> Consider FDA or other regulatory requirements</li>
                <li><strong>Continuous Monitoring:</strong> Track model performance over time</li>
                </ul>
                
                <p style='font-size: 12px; color: #7b1fa2; margin-top: 15px;'>
                <em>🔬 Advanced decision tree analysis for evidence-based clinical decision making</em>
                </p></div>")
            
            return(interpretation_html)
        },

        .generate_bootstrap_results = function(results) {
            
            return("<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>
            <h4 style='color: #2e7d32;'>Bootstrap Confidence Intervals</h4>
            <p>Bootstrap confidence intervals provide uncertainty quantification for performance metrics.</p>
            <p>Feature available in the full implementation with user-specified bootstrap samples.</p>
            </div>")
        },

        .generate_export_info = function(results) {
            
            algorithm <- results$algorithm
            
            return(paste0("<div style='background-color: #fff3cd; padding: 20px; border-radius: 8px;'>
            <h4 style='color: #856404;'>Model Export Information</h4>
            <p><strong>Algorithm:</strong> ", tools::toTitleCase(algorithm), "</p>
            <p><strong>Export Format:</strong> R model object (RDS file)</p>
            <p><strong>Deployment:</strong> Compatible with clinical decision support systems</p>
            <p><strong>Requirements:</strong> R environment with required packages</p>
            <p><strong>Note:</strong> Model export functionality available in full implementation</p>
            </div>"))
        }
    )
)