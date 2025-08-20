
#' @title Clinical Random Forest Analysis
#' @description Random Forest ensemble methods for clinical research and biomarker discovery
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom randomForest randomForest importance
#' @importFrom caret createDataPartition createFolds confusionMatrix
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point coord_flip labs theme_minimal

treeensembleClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "treeensembleClass",
    inherit = treeensembleBase,
    private = list(
        .model = NULL,
        .training_data = NULL,
        .test_data = NULL,
        .predictions = NULL,
        .oob_errors = NULL,

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

            # Build Random Forest model
            private$.build_ensemble()

            # Generate predictions
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

            # Check sample size for Random Forest
            if (nrow(self$data) < 20) {
                return(list(valid = FALSE, show_welcome = FALSE,
                           message = "Random Forest requires at least 20 samples."))
            }

            # Check for RandomForest package
            if (!requireNamespace("randomForest", quietly = TRUE)) {
                return(list(valid = FALSE, show_welcome = FALSE,
                           message = "randomForest package is required but not installed."))
            }

            return(list(valid = TRUE))
        },

        .show_welcome_message = function() {
            welcome_html <- "
            <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 20px 0;'>
            <h3 style='color: #2e7d32; margin-top: 0;'>Clinical Random Forest Analysis</h3>
            <p><strong>Random Forest ensemble methods for clinical research and biomarker discovery</strong></p>
            
            <h4 style='color: #2e7d32;'>Required Variables:</h4>
            <ol>
            <li><strong>Predictors:</strong> Select continuous and/or categorical clinical variables</li>
            <li><strong>Target Outcome:</strong> Select the disease/outcome variable to predict</li>
            <li><strong>Target Level:</strong> Select the positive class (e.g., 'disease', 'positive')</li>
            </ol>

            <h4 style='color: #2e7d32;'>Key Features:</h4>
            <ul>
            <li><strong>Ensemble Learning:</strong> Combines multiple decision trees for robust predictions</li>
            <li><strong>Feature Importance:</strong> Identifies most important clinical variables</li>
            <li><strong>Out-of-Bag Validation:</strong> Built-in cross-validation without separate test set</li>
            <li><strong>Biomarker Discovery:</strong> Optimized for clinical research applications</li>
            <li><strong>Class Imbalance Handling:</strong> Options for unbalanced clinical datasets</li>
            </ul>

            <h4 style='color: #2e7d32;'>Requirements:</h4>
            <ul>
            <li>Minimum 20 samples for reliable analysis</li>
            <li>randomForest package must be installed</li>
            <li>Categorical target variable with at least 2 levels</li>
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
            
            # Handle missing values in predictors (Random Forest can handle some missing values)
            missing_count <- sum(is.na(data[c(vars, facs)]))
            if (missing_count > 0) {
                # Simple imputation for excessive missing values
                for (var in c(vars, facs)) {
                    if (var %in% names(data)) {
                        missing_pct <- sum(is.na(data[[var]])) / nrow(data)
                        if (missing_pct > 0.5) {
                            if (is.numeric(data[[var]])) {
                                data[[var]][is.na(data[[var]])] <- median(data[[var]], na.rm = TRUE)
                            } else {
                                mode_val <- names(sort(table(data[[var]]), decreasing = TRUE))[1]
                                data[[var]][is.na(data[[var]])] <- mode_val
                                data[[var]] <- as.factor(data[[var]])
                            }
                        }
                    }
                }
            }
            
            # Check if we still have enough data
            if (nrow(data) < 20) {
                jmvcore::reject("Insufficient data after handling missing values. Need at least 20 complete cases.")
            }
            
            # Store prepared data
            private$.training_data <- data
            
            # Split data for holdout validation if requested
            if (self$options$validation == "holdout") {
                test_prop <- self$options$test_split
                
                train_indices <- caret::createDataPartition(
                    data[[target]], 
                    p = 1 - test_prop, 
                    list = FALSE
                )[, 1]
                
                private$.training_data <- data[train_indices, ]
                private$.test_data <- data[-train_indices, ]
            }
        },

        .build_ensemble = function() {
            target <- self$options$target
            vars <- self$options$vars
            facs <- self$options$facs
            
            # Create formula
            predictors <- c(vars, facs)
            formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
            formula_obj <- as.formula(formula_str)

            # Determine mtry (number of variables to try at each split)
            mtry_value <- if (self$options$mtry_method == "custom") {
                min(self$options$mtry_custom, length(predictors))
            } else {
                # Default: sqrt(p) for classification
                max(1, floor(sqrt(length(predictors))))
            }

            # Set up class weights for imbalanced data
            class_weight <- NULL
            if (self$options$class_weights) {
                target_table <- table(private$.training_data[[target]])
                # Inverse frequency weighting
                class_weight <- 1 / target_table[names(target_table)]
                class_weight <- class_weight / sum(class_weight) * length(class_weight)
            }

            # Build Random Forest model
            tryCatch({
                private$.model <- randomForest::randomForest(
                    formula = formula_obj,
                    data = private$.training_data,
                    ntree = self$options$n_trees,
                    mtry = mtry_value,
                    nodesize = self$options$min_node_size,
                    importance = TRUE,
                    keep.forest = TRUE,
                    classwt = class_weight
                )

                # Store OOB error progression for plotting
                private$.oob_errors <- private$.model$err.rate

            }, error = function(e) {
                jmvcore::reject(paste("Error building Random Forest model:", e$message))
            })
        },

        .generate_predictions = function() {
            if (is.null(private$.model)) {
                return()
            }

            validation_method <- self$options$validation
            
            if (validation_method == "holdout" && !is.null(private$.test_data)) {
                # Use holdout test data
                test_data <- private$.test_data
                predictions <- predict(private$.model, test_data, type = "class")
                probabilities <- predict(private$.model, test_data, type = "prob")
                
                private$.predictions <- list(
                    actual = test_data[[self$options$target]],
                    predicted = predictions,
                    probabilities = probabilities,
                    validation_method = "holdout"
                )
                
            } else if (validation_method == "cv") {
                # Cross-validation predictions
                target <- self$options$target
                data <- private$.training_data
                n_folds <- self$options$cv_folds
                
                fold_indices <- caret::createFolds(data[[target]], k = n_folds)
                
                all_actual <- c()
                all_predicted <- c()
                
                for (i in 1:n_folds) {
                    test_idx <- fold_indices[[i]]
                    train_data <- data[-test_idx, ]
                    test_data <- data[test_idx, ]
                    
                    # Build fold model
                    fold_predictions <- private$.build_fold_model_and_predict(train_data, test_data)
                    
                    all_actual <- c(all_actual, as.character(fold_predictions$actual))
                    all_predicted <- c(all_predicted, as.character(fold_predictions$predicted))
                }
                
                private$.predictions <- list(
                    actual = factor(all_actual, levels = levels(data[[target]])),
                    predicted = factor(all_predicted, levels = levels(data[[target]])),
                    validation_method = "cv"
                )
                
            } else {
                # OOB predictions (default)
                predictions <- private$.model$predicted
                
                private$.predictions <- list(
                    actual = private$.training_data[[self$options$target]],
                    predicted = predictions,
                    validation_method = "oob"
                )
            }
        },

        .build_fold_model_and_predict = function(train_data, test_data) {
            target <- self$options$target
            vars <- self$options$vars
            facs <- self$options$facs
            predictors <- c(vars, facs)
            
            # Create formula
            formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
            formula_obj <- as.formula(formula_str)

            # Determine mtry
            mtry_value <- if (self$options$mtry_method == "custom") {
                min(self$options$mtry_custom, length(predictors))
            } else {
                max(1, floor(sqrt(length(predictors))))
            }

            # Build fold model with reduced trees for speed
            fold_model <- randomForest::randomForest(
                formula = formula_obj,
                data = train_data,
                ntree = min(100, self$options$n_trees),  # Reduced for CV
                mtry = mtry_value,
                nodesize = self$options$min_node_size,
                importance = FALSE
            )
            
            # Predictions for this fold
            fold_pred <- predict(fold_model, test_data, type = "class")
            
            return(list(
                actual = test_data[[target]],
                predicted = fold_pred
            ))
        },

        .populate_results = function() {
            private$.populate_model_summary()
            private$.populate_performance_table()
            private$.populate_confusion_matrix()
            private$.populate_importance_table()
            private$.populate_clinical_interpretation()
        },

        .populate_model_summary = function() {
            if (is.null(private$.model)) {
                return()
            }

            n_trees <- private$.model$ntree
            mtry_used <- private$.model$mtry
            n_vars <- length(private$.model$importance[,1])
            oob_error <- tail(private$.model$err.rate[,"OOB"], 1) * 100
            
            # Class distribution info
            class_dist <- table(private$.training_data[[self$options$target]])
            class_info <- paste(names(class_dist), class_dist, sep="=", collapse=", ")
            
            summary_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>",
                "<h4>Random Forest Model Summary</h4>",
                "<ul>",
                "<li><strong>Algorithm:</strong> Random Forest (randomForest)</li>",
                "<li><strong>Training samples:</strong> ", nrow(private$.training_data), "</li>",
                "<li><strong>Number of trees:</strong> ", n_trees, "</li>",
                "<li><strong>Variables per split (mtry):</strong> ", mtry_used, "</li>",
                "<li><strong>Total variables:</strong> ", n_vars, "</li>",
                "<li><strong>Class distribution:</strong> ", class_info, "</li>",
                "<li><strong>OOB Error Rate:</strong> ", round(oob_error, 2), "%</li>",
                "<li><strong>Validation method:</strong> ", self$options$validation, "</li>",
                if (self$options$class_weights) "<li><strong>Class weights:</strong> Applied for imbalanced data</li>" else "",
                "</ul>",
                "</div>"
            )
            
            self$results$model_summary$setContent(summary_html)
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
            if (target_level %in% levels(actual) && length(levels(actual)) == 2) {
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
                
                # Simple confidence intervals
                n <- length(actual)
                alpha <- 0.05
                
                metrics <- data.frame(
                    metric = c("Accuracy", "Sensitivity", "Specificity", "PPV", "NPV", "F1 Score"),
                    value = c(accuracy, sensitivity, specificity, ppv, npv, f1_score),
                    ci_lower = c(
                        pmax(0, accuracy - qnorm(1-alpha/2) * sqrt(accuracy * (1 - accuracy) / n)),
                        pmax(0, sensitivity - qnorm(1-alpha/2) * sqrt(sensitivity * (1 - sensitivity) / (tp + fn))),
                        pmax(0, specificity - qnorm(1-alpha/2) * sqrt(specificity * (1 - specificity) / (fp + tn))),
                        pmax(0, ppv - qnorm(1-alpha/2) * sqrt(ppv * (1 - ppv) / (tp + fp))),
                        pmax(0, npv - qnorm(1-alpha/2) * sqrt(npv * (1 - npv) / (tn + fn))),
                        rep(NA, 1)  # F1 CI is complex
                    ),
                    ci_upper = c(
                        pmin(1, accuracy + qnorm(1-alpha/2) * sqrt(accuracy * (1 - accuracy) / n)),
                        pmin(1, sensitivity + qnorm(1-alpha/2) * sqrt(sensitivity * (1 - sensitivity) / (tp + fn))),
                        pmin(1, specificity + qnorm(1-alpha/2) * sqrt(specificity * (1 - specificity) / (fp + tn))),
                        pmin(1, ppv + qnorm(1-alpha/2) * sqrt(ppv * (1 - ppv) / (tp + fp))),
                        pmin(1, npv + qnorm(1-alpha/2) * sqrt(npv * (1 - npv) / (tn + fn))),
                        rep(NA, 1)  # F1 CI is complex
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
                self$results$performance_table$addRow(
                    rowKey = i,
                    values = list(
                        metric = metrics$metric[i],
                        value = metrics$value[i],
                        ci_lower = metrics$ci_lower[i],
                        ci_upper = metrics$ci_upper[i]
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
                
                self$results$confusion_matrix$addRow(
                    rowKey = i,
                    values = list(
                        actual = row_name,
                        predicted_negative = ifelse(ncol(cm_with_totals) >= 1, cm_with_totals[i, 1], 0),
                        predicted_positive = ifelse(ncol(cm_with_totals) >= 2, cm_with_totals[i, 2], 0),
                        total = ifelse(ncol(cm_with_totals) >= 3, cm_with_totals[i, 3], sum(cm_with_totals[i, 1:(ncol(cm_with_totals)-1)]))
                    )
                )
            }
        },

        .populate_importance_table = function() {
            if (is.null(private$.model) || is.null(private$.model$importance)) {
                return()
            }

            importance_matrix <- private$.model$importance
            
            # Choose importance measure based on user selection
            if (self$options$importance_type == "gini" && "MeanDecreaseGini" %in% colnames(importance_matrix)) {
                importance_scores <- importance_matrix[, "MeanDecreaseGini"]
            } else if (self$options$importance_type == "permutation" && "MeanDecreaseAccuracy" %in% colnames(importance_matrix)) {
                importance_scores <- importance_matrix[, "MeanDecreaseAccuracy"]
            } else {
                # Default to first column if specific type not available
                importance_scores <- importance_matrix[, 1]
            }
            
            importance_df <- data.frame(
                variable = names(importance_scores),
                importance = as.numeric(importance_scores),
                stringsAsFactors = FALSE
            )
            
            # Sort by importance and add ranks
            importance_df <- importance_df[order(importance_df$importance, decreasing = TRUE), ]
            importance_df$rank <- 1:nrow(importance_df)
            
            # Normalize importance to 0-100 scale
            if (max(importance_df$importance) > 0) {
                importance_df$importance <- (importance_df$importance / max(importance_df$importance)) * 100
            }
            
            # Apply feature selection if requested
            if (self$options$feature_selection && nrow(importance_df) > self$options$max_features) {
                importance_df <- importance_df[1:self$options$max_features, ]
            }
            
            # Populate table
            for (i in 1:nrow(importance_df)) {
                self$results$importance_table$addRow(
                    rowKey = i,
                    values = list(
                        variable = importance_df$variable[i],
                        importance = importance_df$importance[i],
                        rank = importance_df$rank[i]
                    )
                )
            }
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
                performance_metrics <- sprintf("Overall accuracy: %.1f%%", accuracy * 100)
            }
            
            # Context-specific interpretation
            interpretation <- switch(clinical_context,
                "biomarker" = paste0(
                    "<h4>Clinical Interpretation - Biomarker Discovery</h4>",
                    "<p>This Random Forest analysis identifies key biomarkers for <strong>", target_level, "</strong> prediction.</p>",
                    "<ul>",
                    "<li><strong>Feature Importance:</strong> Variables ranked by predictive power for biomarker discovery</li>",
                    "<li><strong>Ensemble Robustness:</strong> Multiple trees reduce overfitting and improve generalizability</li>",
                    "<li><strong>Clinical Validation:</strong> Top features should be validated in independent cohorts</li>",
                    "<li><strong>Biomarker Panel:</strong> Consider top 5-10 features for clinical biomarker panel</li>",
                    "</ul>"
                ),
                "diagnosis" = paste0(
                    "<h4>Clinical Interpretation - Diagnostic Application</h4>",
                    "<p>This Random Forest model provides diagnostic support for <strong>", target_level, "</strong> cases.</p>",
                    "<ul>",
                    "<li><strong>Clinical Decision Support:</strong> High-accuracy predictions for diagnostic assistance</li>",
                    "<li><strong>Feature Insights:</strong> Most important diagnostic indicators identified</li>",
                    "<li><strong>Validation Required:</strong> External validation needed before clinical deployment</li>",
                    "<li><strong>Interpretability:</strong> Consider model simplification for clinical acceptance</li>",
                    "</ul>"
                ),
                "prognosis" = paste0(
                    "<h4>Clinical Interpretation - Prognostic Analysis</h4>",
                    "<p>This Random Forest model predicts prognosis for <strong>", target_level, "</strong> outcomes.</p>",
                    "<ul>",
                    "<li><strong>Risk Stratification:</strong> Identifies patients at high risk for poor outcomes</li>",
                    "<li><strong>Prognostic Factors:</strong> Key variables influencing patient prognosis</li>",
                    "<li><strong>Clinical Utility:</strong> Can guide treatment intensity and monitoring frequency</li>",
                    "<li><strong>Temporal Validation:</strong> Should be validated on prospective patient cohorts</li>",
                    "</ul>"
                ),
                "treatment" = paste0(
                    "<h4>Clinical Interpretation - Treatment Response Prediction</h4>",
                    "<p>This Random Forest model predicts treatment response for <strong>", target_level, "</strong>.</p>",
                    "<ul>",
                    "<li><strong>Personalized Medicine:</strong> Identifies patients likely to respond to treatment</li>",
                    "<li><strong>Treatment Selection:</strong> Guides optimal treatment choice based on patient characteristics</li>",
                    "<li><strong>Response Predictors:</strong> Key factors influencing treatment success</li>",
                    "<li><strong>Clinical Implementation:</strong> Consider integration into treatment decision workflows</li>",
                    "</ul>"
                )
            )
            
            clinical_html <- paste0(
                "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 5px;'>",
                interpretation,
                "<p><strong>Performance:</strong> ", performance_metrics, "</p>",
                "<h4>Random Forest Advantages:</h4>",
                "<ul>",
                "<li><strong>Robustness:</strong> Ensemble method reduces overfitting and improves generalization</li>",
                "<li><strong>Feature Selection:</strong> Built-in importance ranking for biomarker discovery</li>",
                "<li><strong>Missing Data:</strong> Handles missing values better than single decision trees</li>",
                "<li><strong>Non-linear Interactions:</strong> Captures complex relationships between variables</li>",
                "</ul>",
                "<h4>Important Clinical Notes:</h4>",
                "<ul>",
                "<li>Model is for research and development purposes</li>",
                "<li>External validation required before clinical use</li>",
                "<li>Consider regulatory requirements for clinical implementation</li>",
                "<li>Regular model updating recommended with new data</li>",
                "<li>Important features should be clinically meaningful and interpretable</li>",
                "</ul>",
                "</div>"
            )
            
            self$results$clinical_interpretation$setContent(clinical_html)
        },

        # Plotting functions
        .importance_plot = function(image, ggtheme, ...) {
            if (is.null(private$.model) || is.null(private$.model$importance)) {
                return(FALSE)
            }

            tryCatch({
                importance_matrix <- private$.model$importance
                
                # Choose importance measure
                if (self$options$importance_type == "gini" && "MeanDecreaseGini" %in% colnames(importance_matrix)) {
                    importance_scores <- importance_matrix[, "MeanDecreaseGini"]
                    importance_title <- "Feature Importance (Gini)"
                } else if (self$options$importance_type == "permutation" && "MeanDecreaseAccuracy" %in% colnames(importance_matrix)) {
                    importance_scores <- importance_matrix[, "MeanDecreaseAccuracy"]
                    importance_title <- "Feature Importance (Permutation)"
                } else {
                    importance_scores <- importance_matrix[, 1]
                    importance_title <- "Feature Importance"
                }
                
                importance_df <- data.frame(
                    variable = names(importance_scores),
                    importance = as.numeric(importance_scores)
                )
                
                # Apply feature selection if requested
                if (self$options$feature_selection && nrow(importance_df) > self$options$max_features) {
                    importance_df <- importance_df[order(importance_df$importance, decreasing = TRUE), ][1:self$options$max_features, ]
                }
                
                # Sort by importance
                importance_df <- importance_df[order(importance_df$importance, decreasing = TRUE), ]
                importance_df$variable <- factor(importance_df$variable, levels = importance_df$variable)
                
                p <- ggplot2::ggplot(importance_df, ggplot2::aes(x = variable, y = importance)) +
                    ggplot2::geom_col(fill = "forestgreen", alpha = 0.7) +
                    ggplot2::coord_flip() +
                    ggplot2::labs(
                        title = importance_title,
                        x = "Clinical Variables",
                        y = "Importance Score"
                    ) +
                    ggplot2::theme_minimal() +
                    ggtheme
                
                print(p)
                return(TRUE)
                
            }, error = function(e) {
                return(FALSE)
            })
        },

        .oob_error_plot = function(image, ggtheme, ...) {
            if (is.null(private$.oob_errors)) {
                return(FALSE)
            }

            tryCatch({
                # Prepare OOB error data
                error_data <- data.frame(
                    tree = 1:nrow(private$.oob_errors),
                    oob_error = private$.oob_errors[, "OOB"] * 100
                )
                
                p <- ggplot2::ggplot(error_data, ggplot2::aes(x = tree, y = oob_error)) +
                    ggplot2::geom_line(color = "darkgreen", size = 1) +
                    ggplot2::labs(
                        title = "Out-of-Bag Error Progression",
                        x = "Number of Trees",
                        y = "OOB Error Rate (%)"
                    ) +
                    ggplot2::theme_minimal() +
                    ggtheme
                
                print(p)
                return(TRUE)
                
            }, error = function(e) {
                return(FALSE)
            })
        }
    )
)
