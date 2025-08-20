#' @title Medical Decision Trees
#' @description Simple decision tree analysis optimized for clinical research
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom rpart rpart rpart.control
#' @importFrom rpart.plot rpart.plot
#' @importFrom caret createDataPartition confusionMatrix
#' @importFrom pROC roc auc
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_minimal

treemedicalClass <- if (requireNamespace("jmvcore")) R6::R6Class("treemedicalClass",
    inherit = treemedicalBase,
    private = list(
        .model = NULL,
        .training_data = NULL,
        .test_data = NULL,
        .predictions = NULL,

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

            # Build model
            private$.build_model()

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
            
            # Check for very large datasets that might cause performance issues
            if (nrow(self$data) > 50000) {
                jmvcore::reject("Dataset too large (>50,000 rows). Consider sampling or using a more scalable algorithm.")
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

            return(list(valid = TRUE))
        },

        .show_welcome_message = function() {
            welcome_html <- "
            <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 20px 0;'>
            <h3 style='color: #2e7d32; margin-top: 0;'>Medical Decision Trees</h3>
            <p><strong>Simple decision tree analysis for clinical research</strong></p>
            
            <h4 style='color: #2e7d32;'>Required Variables:</h4>
            <ol>
            <li><strong>Predictors:</strong> Select continuous and/or categorical clinical variables</li>
            <li><strong>Target Outcome:</strong> Select the disease/outcome variable to predict</li>
            <li><strong>Target Level:</strong> Select the positive class (e.g., 'cancer', 'positive')</li>
            </ol>

            <h4 style='color: #2e7d32;'>Key Features:</h4>
            <ul>
            <li><strong>Clinical Focus:</strong> Optimized for medical decision making</li>
            <li><strong>Simple Interface:</strong> Essential parameters only for ease of use</li>
            <li><strong>Robust Validation:</strong> Cross-validation and performance metrics</li>
            <li><strong>Clinical Interpretation:</strong> Guidelines for medical application</li>
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
            
            # Handle missing values in predictors
            missing_count <- sum(is.na(data[c(vars, facs)]))
            if (missing_count > 0) {
                if (self$options$handle_missing == "impute") {
                    data <- private$.impute_missing(data)
                } else {
                    # Remove rows with any missing predictors
                    data <- data[complete.cases(data[c(vars, facs)]), ]
                }
            }
            
            # Check if we still have enough data
            if (nrow(data) < 10) {
                jmvcore::reject("Insufficient data after handling missing values. Need at least 10 complete cases.")
            }
            
            # Store prepared data
            private$.training_data <- data
            
            # Split data if using holdout validation
            if (self$options$validation == "holdout") {
                train_prop <- self$options$holdout_split
                
                if (self$options$stratified_sampling) {
                    train_indices <- caret::createDataPartition(
                        data[[target]], 
                        p = train_prop, 
                        list = FALSE
                    )[, 1]
                } else {
                    n <- nrow(data)
                    train_indices <- sample(1:n, size = floor(train_prop * n))
                }
                
                private$.training_data <- data[train_indices, ]
                private$.test_data <- data[-train_indices, ]
            }
        },

        .build_model = function() {
            target <- self$options$target
            vars <- self$options$vars
            facs <- self$options$facs
            
            # Create formula
            predictors <- c(vars, facs)
            formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
            formula_obj <- as.formula(formula_str)

            # Set up rpart control
            control <- rpart::rpart.control(
                maxdepth = self$options$max_depth,
                minsplit = self$options$min_samples_split,
                cp = self$options$cost_complexity,
                xval = self$options$cv_folds
            )

            # Set up cost matrix for cost-sensitive learning
            parms_list <- list()
            if (self$options$cost_sensitive) {
                # Create cost matrix: rows = true class, cols = predicted class
                # Cost of false negative (missing positive case) vs false positive
                fn_cost <- self$options$fn_fp_ratio
                fp_cost <- 1
                
                # Cost matrix: [TN cost, FP cost; FN cost, TP cost]
                cost_matrix <- matrix(c(0, fp_cost, fn_cost, 0), nrow = 2, byrow = TRUE)
                parms_list$loss <- cost_matrix
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

                # Apply 1-SE rule for pruning if requested
                if (self$options$use_1se_rule && !is.null(private$.model$cptable)) {
                    cp_table <- private$.model$cptable
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
                }

            }, error = function(e) {
                jmvcore::reject(paste("Error building model:", e$message))
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
                    probabilities = probabilities
                )
                
            } else if (validation_method == "cv") {
                # Cross-validation predictions (simplified)
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
                    
                    # Build fold model using helper method
                    fold_model <- private$.build_fold_model(train_data, include_cost_matrix = TRUE)
                    
                    # Predictions for this fold
                    fold_pred <- predict(fold_model, test_data, type = "class")
                    fold_prob <- predict(fold_model, test_data, type = "prob")
                    
                    all_actual <- c(all_actual, as.character(test_data[[target]]))
                    all_predicted <- c(all_predicted, as.character(fold_pred))
                    
                    if (is.null(all_probabilities)) {
                        all_probabilities <- fold_prob
                    } else {
                        all_probabilities <- rbind(all_probabilities, fold_prob)
                    }
                }
                
                private$.predictions <- list(
                    actual = factor(all_actual, levels = levels(data[[target]])),
                    predicted = factor(all_predicted, levels = levels(data[[target]])),
                    probabilities = all_probabilities
                )
                
            } else {
                # Bootstrap validation (simplified)
                data <- private$.training_data
                target <- self$options$target
                n_bootstrap <- self$options$bootstrap_samples
                
                all_actual <- c()
                all_predicted <- c()
                all_probabilities <- NULL
                
                for (i in 1:n_bootstrap) {
                    # Bootstrap sample
                    boot_indices <- sample(1:nrow(data), nrow(data), replace = TRUE)
                    oob_indices <- setdiff(1:nrow(data), boot_indices)
                    
                    if (length(oob_indices) == 0) next
                    
                    boot_data <- data[boot_indices, ]
                    oob_data <- data[oob_indices, ]
                    
                    # Build bootstrap model using helper method
                    boot_model <- private$.build_fold_model(boot_data, include_cost_matrix = TRUE)
                    
                    # OOB predictions
                    oob_pred <- predict(boot_model, oob_data, type = "class")
                    oob_prob <- predict(boot_model, oob_data, type = "prob")
                    
                    all_actual <- c(all_actual, as.character(oob_data[[target]]))
                    all_predicted <- c(all_predicted, as.character(oob_pred))
                    
                    if (is.null(all_probabilities)) {
                        all_probabilities <- oob_prob
                    } else {
                        all_probabilities <- rbind(all_probabilities, oob_prob)
                    }
                }
                
                private$.predictions <- list(
                    actual = factor(all_actual, levels = levels(data[[target]])),
                    predicted = factor(all_predicted, levels = levels(data[[target]])),
                    probabilities = all_probabilities
                )
            }
        },

        .populate_results = function() {
            private$.populate_model_summary()
            private$.populate_performance_table()
            private$.populate_confusion_matrix()
            private$.populate_variable_importance()
            private$.populate_clinical_interpretation()
        },

        .populate_model_summary = function() {
            if (is.null(private$.model)) {
                return()
            }

            n_nodes <- nrow(private$.model$frame)
            n_leaves <- sum(private$.model$frame$var == "<leaf>")
            tree_depth <- max(rpart:::tree.depth(as.numeric(rownames(private$.model$frame))))
            
            # Add cost-sensitive information
            cost_info <- ""
            if (self$options$cost_sensitive) {
                cost_info <- paste0(
                    "<li><strong>Cost-sensitive analysis:</strong> Enabled (FN:FP ratio = ", 
                    self$options$fn_fp_ratio, ")</li>"
                )
            }
            
            # Add missing data handling information
            missing_info <- paste0(
                "<li><strong>Missing data handling:</strong> ", 
                ifelse(self$options$handle_missing == "impute", "Simple imputation (median/mode)", "Complete case analysis"),
                "</li>"
            )
            
            # Add validation split information
            validation_info <- ""
            if (self$options$validation == "holdout") {
                validation_info <- paste0(
                    "<li><strong>Train/test split:</strong> ", 
                    round(self$options$holdout_split * 100), "%/", 
                    round((1 - self$options$holdout_split) * 100), "%</li>"
                )
            }
            
            summary_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>",
                "<h4>Model Summary</h4>",
                "<ul>",
                "<li><strong>Algorithm:</strong> Enhanced CART (rpart)</li>",
                "<li><strong>Training samples:</strong> ", nrow(private$.training_data), "</li>",
                "<li><strong>Tree nodes:</strong> ", n_nodes, "</li>",
                "<li><strong>Leaf nodes:</strong> ", n_leaves, "</li>",
                "<li><strong>Tree depth:</strong> ", tree_depth, "</li>",
                "<li><strong>Validation method:</strong> ", self$options$validation, "</li>",
                validation_info,
                missing_info,
                cost_info,
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
            
            # Calculate metrics
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
                
                # Calculate AUC if probabilities available
                auc_value <- NA
                if (!is.null(private$.predictions$probabilities) && pos_class %in% colnames(private$.predictions$probabilities)) {
                    tryCatch({
                        if (requireNamespace("pROC", quietly = TRUE)) {
                            roc_obj <- pROC::roc(actual, private$.predictions$probabilities[, pos_class], quiet = TRUE)
                            auc_value <- as.numeric(pROC::auc(roc_obj))
                        } else {
                            # Manual AUC calculation if pROC not available
                            prob_pos <- private$.predictions$probabilities[, pos_class]
                            auc_value <- private$.calculate_auc_manual(actual, prob_pos, pos_class)
                        }
                    }, error = function(e) {
                        # AUC calculation failed - leave as NA
                        cat("AUC calculation error:", e$message, "\n")
                    })
                }
                
                # Simple confidence intervals (bootstrap would be better)
                n <- length(actual)
                
                metrics <- data.frame(
                    metric = c("Accuracy", "Sensitivity", "Specificity", "PPV", "NPV", "AUC"),
                    value = c(accuracy, sensitivity, specificity, ppv, npv, auc_value),
                    ci_lower = c(
                        pmax(0, accuracy - 1.96 * sqrt(accuracy * (1 - accuracy) / n)),
                        pmax(0, sensitivity - 1.96 * sqrt(sensitivity * (1 - sensitivity) / (tp + fn))),
                        pmax(0, specificity - 1.96 * sqrt(specificity * (1 - specificity) / (fp + tn))),
                        pmax(0, ppv - 1.96 * sqrt(ppv * (1 - ppv) / (tp + fp))),
                        pmax(0, npv - 1.96 * sqrt(npv * (1 - npv) / (tn + fn))),
                        rep(NA, 1)  # AUC CI calculation is complex
                    ),
                    ci_upper = c(
                        pmin(1, accuracy + 1.96 * sqrt(accuracy * (1 - accuracy) / n)),
                        pmin(1, sensitivity + 1.96 * sqrt(sensitivity * (1 - sensitivity) / (tp + fn))),
                        pmin(1, specificity + 1.96 * sqrt(specificity * (1 - specificity) / (fp + tn))),
                        pmin(1, ppv + 1.96 * sqrt(ppv * (1 - ppv) / (tp + fp))),
                        pmin(1, npv + 1.96 * sqrt(npv * (1 - npv) / (tn + fn))),
                        rep(NA, 1)  # AUC CI calculation is complex
                    )
                )
                
            } else {
                # Multi-class metrics (simplified)
                accuracy <- sum(diag(cm)) / sum(cm)
                
                metrics <- data.frame(
                    metric = "Accuracy",
                    value = accuracy,
                    ci_lower = pmax(0, accuracy - 1.96 * sqrt(accuracy * (1 - accuracy) / length(actual))),
                    ci_upper = pmin(1, accuracy + 1.96 * sqrt(accuracy * (1 - accuracy) / length(actual)))
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
                self$results$variable_importance$addRow(
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
                "diagnosis" = paste0(
                    "<h4>Clinical Interpretation - Diagnostic Application</h4>",
                    "<p>This decision tree can assist in diagnosing <strong>", target_level, "</strong> cases.</p>",
                    "<ul>",
                    "<li><strong>Clinical Use:</strong> As a diagnostic aid in clinical practice</li>",
                    "<li><strong>Decision Support:</strong> Follow the tree branches based on patient characteristics</li>",
                    "<li><strong>Validation Required:</strong> Always confirm with additional clinical assessment</li>",
                    "</ul>"
                ),
                "screening" = paste0(
                    "<h4>Clinical Interpretation - Screening Application</h4>",
                    "<p>This decision tree can help identify patients at risk for <strong>", target_level, "</strong>.</p>",
                    "<ul>",
                    "<li><strong>Screening Use:</strong> Identify high-risk patients for further testing</li>",
                    "<li><strong>Sensitivity Focus:</strong> Designed to minimize false negatives</li>",
                    "<li><strong>Follow-up Required:</strong> Positive screens need confirmatory testing</li>",
                    "</ul>"
                ),
                "treatment" = paste0(
                    "<h4>Clinical Interpretation - Treatment Selection</h4>",
                    "<p>This decision tree can guide treatment decisions for <strong>", target_level, "</strong> cases.</p>",
                    "<ul>",
                    "<li><strong>Treatment Guide:</strong> Personalized treatment recommendations</li>",
                    "<li><strong>Patient Stratification:</strong> Identify optimal treatment candidates</li>",
                    "<li><strong>Clinical Judgment:</strong> Use alongside clinical expertise</li>",
                    "</ul>"
                ),
                "risk" = paste0(
                    "<h4>Clinical Interpretation - Risk Assessment</h4>",
                    "<p>This decision tree assesses risk for <strong>", target_level, "</strong>.</p>",
                    "<ul>",
                    "<li><strong>Risk Stratification:</strong> Classify patients into risk categories</li>",
                    "<li><strong>Preventive Care:</strong> Guide preventive interventions</li>",
                    "<li><strong>Monitoring:</strong> Inform frequency of follow-up</li>",
                    "</ul>"
                )
            )
            
            # Add cost-sensitive information to interpretation
            cost_sensitive_info <- ""
            if (self$options$cost_sensitive) {
                cost_sensitive_info <- paste0(
                    "<p><strong>Cost-Sensitive Analysis:</strong> Model optimized for clinical costs with FN:FP cost ratio of ", 
                    self$options$fn_fp_ratio, ". This prioritizes ", 
                    ifelse(self$options$fn_fp_ratio > 1, "sensitivity (reducing false negatives)", "specificity (reducing false positives)"),
                    ".</p>"
                )
            }
            
            clinical_html <- paste0(
                "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 5px;'>",
                interpretation,
                "<p><strong>Performance:</strong> ", performance_metrics, "</p>",
                cost_sensitive_info,
                "<h4>Important Notes:</h4>",
                "<ul>",
                "<li>This model is for research and educational purposes</li>",
                "<li>Clinical validation required before clinical use</li>",
                "<li>Consider local population characteristics and prevalence</li>",
                "<li>Regular model updating recommended as new data becomes available</li>",
                "</ul>",
                "</div>"
            )
            
            self$results$clinical_interpretation$setContent(clinical_html)
        },

        # Helper method for building fold models (reduces code duplication)
        .build_fold_model = function(train_data, include_cost_matrix = FALSE) {
            vars <- self$options$vars
            facs <- self$options$facs
            target <- self$options$target
            
            predictors <- c(vars, facs)
            formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
            formula_obj <- as.formula(formula_str)
            
            control <- rpart::rpart.control(
                maxdepth = self$options$max_depth,
                minsplit = self$options$min_samples_split,
                cp = self$options$cost_complexity
            )
            
            # Set up cost matrix if requested and enabled
            parms_list <- list()
            if (include_cost_matrix && self$options$cost_sensitive) {
                fn_cost <- self$options$fn_fp_ratio
                fp_cost <- 1
                cost_matrix <- matrix(c(0, fp_cost, fn_cost, 0), nrow = 2, byrow = TRUE)
                parms_list$loss <- cost_matrix
            }
            
            rpart::rpart(
                formula = formula_obj,
                data = train_data,
                method = "class",
                control = control,
                parms = if (length(parms_list) > 0) parms_list else NULL
            )
        },

        # Helper method for missing data imputation
        .impute_missing = function(data) {
            vars <- self$options$vars
            facs <- self$options$facs
            
            for (var in vars) {
                if (var %in% names(data) && any(is.na(data[[var]]))) {
                    # Use median for numeric variables
                    median_val <- median(data[[var]], na.rm = TRUE)
                    data[[var]][is.na(data[[var]])] <- median_val
                }
            }
            
            for (var in facs) {
                if (var %in% names(data) && any(is.na(data[[var]]))) {
                    # Use mode for categorical variables
                    mode_val <- names(sort(table(data[[var]]), decreasing = TRUE))[1]
                    data[[var]][is.na(data[[var]])] <- mode_val
                    data[[var]] <- as.factor(data[[var]])
                }
            }
            
            return(data)
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
                        type = 3,
                        extra = 104,
                        fallen.leaves = TRUE,
                        main = "Medical Decision Tree",
                        cex = 0.8,
                        box.palette = "auto"
                    )
                } else {
                    # Fallback to base plotting if rpart.plot not available
                    plot(private$.model, main = "Medical Decision Tree")
                    text(private$.model, cex = 0.8)
                    
                    # Add a note about enhanced plotting
                    mtext("Note: Install 'rpart.plot' package for enhanced tree visualization", 
                          side = 1, line = 4, cex = 0.8, col = "gray50")
                }
                
                return(TRUE)
                
            }, error = function(e) {
                # Log error for debugging
                cat("Tree plot error:", e$message, "\n")
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
                    ggplot2::geom_col(fill = "steelblue", alpha = 0.7) +
                    ggplot2::coord_flip() +
                    ggplot2::labs(
                        title = "Variable Importance",
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

        # Helper function for manual AUC calculation
        .calculate_auc_manual = function(actual, probabilities, pos_class) {
            tryCatch({
                # Convert to binary labels (1 for positive class, 0 for negative)
                labels <- ifelse(actual == pos_class, 1, 0)
                
                # Get unique probability thresholds
                thresholds <- sort(unique(probabilities), decreasing = TRUE)
                
                # Calculate TPR and FPR for each threshold
                tpr <- numeric(length(thresholds))
                fpr <- numeric(length(thresholds))
                
                for (i in seq_along(thresholds)) {
                    thresh <- thresholds[i]
                    pred_pos <- probabilities >= thresh
                    
                    tp <- sum(pred_pos & labels == 1)
                    fp <- sum(pred_pos & labels == 0)
                    fn <- sum(!pred_pos & labels == 1)
                    tn <- sum(!pred_pos & labels == 0)
                    
                    tpr[i] <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
                    fpr[i] <- ifelse(fp + tn > 0, fp / (fp + tn), 0)
                }
                
                # Add endpoints
                tpr <- c(0, tpr, 1)
                fpr <- c(0, fpr, 1)
                
                # Calculate AUC using trapezoidal rule
                auc <- 0
                for (i in 2:length(fpr)) {
                    auc <- auc + (fpr[i] - fpr[i-1]) * (tpr[i] + tpr[i-1]) / 2
                }
                
                return(auc)
                
            }, error = function(e) {
                return(NA)
            })
        }
    )
)