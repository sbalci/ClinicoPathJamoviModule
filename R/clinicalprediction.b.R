#' @title Clinical Prediction Models with ML Interpretability
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom stats predict glm binomial
#' @export


clinicalpredictionClass <- R6::R6Class(
    "clinicalpredictionClass",
    inherit = clinicalpredictionBase,
    private = list(
        .model = NULL,
        .data_processed = NULL,
        .train_data = NULL,
        .test_data = NULL,
        .predictions = NULL,
        .shap_values = NULL,
        .feature_importance = NULL,
        
        .init = function() {
            if (is.null(self$options$outcome_var)) {
                self$results$overview$setNote("note", "Outcome variable is required")
                return()
            }
            
            if (length(self$options$predictor_vars) == 0) {
                self$results$overview$setNote("note", "At least one predictor variable is required")
                return()
            }
            
            # Set table titles
            self$results$overview$setTitle("Model Overview")
            self$results$dataset_info$setTitle("Dataset Information")
            self$results$performance_summary$setTitle("Model Performance Summary")
        },
        
        .run = function() {
            if (is.null(self$options$outcome_var) || length(self$options$predictor_vars) == 0) {
                return()
            }
            
            # Check for required packages
            private$.checkPackages()
            
            # Process data
            private$.processData()
            
            # Split data
            private$.splitData()
            
            # Train model
            private$.trainModel()
            
            # Generate predictions
            private$.generatePredictions()
            
            # Calculate performance metrics
            private$.calculatePerformance()
            
            # Interpretability analysis
            if (self$options$interpretability) {
                private$.interpretabilityAnalysis()
            }
            
            # Clinical analysis
            if (self$options$risk_stratification) {
                private$.riskStratification()
            }
            
            # Generate plots
            private$.generatePlots()
            
            # Update tables
            private$.populateTables()
        },
        
        .checkPackages = function() {
            # Install required packages if not available
            required_packages <- c("randomForest", "xgboost", "e1071", "caret", 
                                 "pROC", "calibrate", "rms", "survival")
            
            # Check for interpretability packages
            if (self$options$shap_analysis || self$options$interpretability) {
                required_packages <- c(required_packages, "shapr", "fastshap")
            }
            
            if (self$options$lime_analysis) {
                required_packages <- c(required_packages, "lime")
            }
            
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    tryCatch({
                        install.packages(pkg, repos = "https://cran.rstudio.com/")
                    }, error = function(e) {
                        self$results$overview$setNote("error", paste("Failed to install package:", pkg))
                        return()
                    })
                }
            }
        },
        
        .processData = function() {
            data <- self$data
            
            # Get variables
            outcome_var <- self$options$outcome_var
            predictor_vars <- self$options$predictor_vars
            
            # Create working dataset
            vars_to_include <- c(outcome_var, predictor_vars)
            
            # Add optional variables if specified
            if (!is.null(self$options$patient_id)) {
                vars_to_include <- c(vars_to_include, self$options$patient_id)
            }
            
            if (!is.null(self$options$stratify_var)) {
                vars_to_include <- c(vars_to_include, self$options$stratify_var)
            }
            
            if (self$options$problem_type == "survival") {
                if (!is.null(self$options$time_var)) {
                    vars_to_include <- c(vars_to_include, self$options$time_var)
                }
                if (!is.null(self$options$event_var)) {
                    vars_to_include <- c(vars_to_include, self$options$event_var)
                }
            }
            
            # Extract data
            work_data <- data[, vars_to_include, drop = FALSE]
            
            # Handle missing data
            work_data <- private$.handleMissingData(work_data)
            
            # Feature engineering
            if (self$options$feature_engineering) {
                work_data <- private$.featureEngineering(work_data)
            }
            
            # Feature selection
            if (self$options$feature_selection) {
                work_data <- private$.featureSelection(work_data)
            }
            
            private$.data_processed <- work_data
        },
        
        .handleMissingData = function(data) {
            method <- self$options$handle_missing
            
            if (method == "complete_cases") {
                return(data[complete.cases(data), ])
            } else if (method == "mean_median") {
                for (col in names(data)) {
                    if (is.numeric(data[[col]])) {
                        data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
                    } else if (is.factor(data[[col]])) {
                        # Use mode for categorical variables
                        mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
                        data[[col]][is.na(data[[col]])] <- mode_val
                    }
                }
                return(data)
            } else if (method == "mice_imputation") {
                if (requireNamespace("mice", quietly = TRUE)) {
                    mice_result <- mice::mice(data, m = 5, printFlag = FALSE)
                    return(mice::complete(mice_result))
                } else {
                    # Fallback to mean/median
                    return(private$.handleMissingData_meanMedian(data))
                }
            }
            
            return(data)
        },
        
        .featureEngineering = function(data) {
            # Basic feature engineering
            outcome_var <- self$options$outcome_var
            predictor_vars <- self$options$predictor_vars
            
            # Create interaction terms for top features
            numeric_vars <- predictor_vars[sapply(data[predictor_vars], is.numeric)]
            
            if (length(numeric_vars) >= 2) {
                # Add polynomial terms
                for (var in numeric_vars[1:min(3, length(numeric_vars))]) {
                    data[[paste0(var, "_squared")]] <- data[[var]]^2
                }
                
                # Add interaction terms
                if (length(numeric_vars) >= 2) {
                    data[[paste0(numeric_vars[1], "_x_", numeric_vars[2])]] <- 
                        data[[numeric_vars[1]]] * data[[numeric_vars[2]]]
                }
            }
            
            return(data)
        },
        
        .featureSelection = function(data) {
            method <- self$options$selection_method
            outcome_var <- self$options$outcome_var
            predictor_vars <- self$options$predictor_vars
            
            if (method == "lasso" && requireNamespace("glmnet", quietly = TRUE)) {
                # LASSO feature selection
                x <- as.matrix(data[, predictor_vars, drop = FALSE])
                y <- data[[outcome_var]]
                
                cv_fit <- glmnet::cv.glmnet(x, y, family = "binomial", alpha = 1)
                coef_lasso <- as.matrix(coef(cv_fit, s = "lambda.1se"))
                selected_features <- row.names(coef_lasso)[coef_lasso[, 1] != 0][-1]  # Remove intercept
                
                # Keep selected features plus outcome
                keep_vars <- c(outcome_var, selected_features)
                data <- data[, keep_vars, drop = FALSE]
            } else if (method == "recursive_fe" && requireNamespace("caret", quietly = TRUE)) {
                # Recursive feature elimination
                control <- caret::rfeControl(functions = caret::rfFuncs, method = "cv", number = 5)
                x <- data[, predictor_vars, drop = FALSE]
                y <- data[[outcome_var]]
                
                rfe_result <- caret::rfe(x, y, sizes = c(1:min(10, length(predictor_vars))), rfeControl = control)
                selected_features <- rfe_result$optVariables
                
                keep_vars <- c(outcome_var, selected_features)
                data <- data[, keep_vars, drop = FALSE]
            }
            
            return(data)
        },
        
        .splitData = function() {
            data <- private$.data_processed
            train_prop <- self$options$train_proportion
            
            set.seed(self$options$random_seed)
            
            n <- nrow(data)
            train_indices <- sample(1:n, size = floor(train_prop * n))
            
            private$.train_data <- data[train_indices, ]
            private$.test_data <- data[-train_indices, ]
        },
        
        .trainModel = function() {
            model_type <- self$options$model_type
            outcome_var <- self$options$outcome_var
            train_data <- private$.train_data
            
            # Prepare formula
            predictor_vars <- setdiff(names(train_data), outcome_var)
            formula_str <- paste(outcome_var, "~", paste(predictor_vars, collapse = " + "))
            model_formula <- as.formula(formula_str)
            
            if (model_type == "random_forest") {
                if (requireNamespace("randomForest", quietly = TRUE)) {
                    private$.model <- randomForest::randomForest(
                        model_formula, 
                        data = train_data,
                        ntree = 500,
                        importance = TRUE
                    )
                }
            } else if (model_type == "gradient_boosting") {
                if (requireNamespace("xgboost", quietly = TRUE)) {
                    # Prepare data for xgboost
                    x_train <- as.matrix(train_data[, predictor_vars, drop = FALSE])
                    y_train <- as.numeric(train_data[[outcome_var]]) - 1  # 0-1 encoding
                    
                    private$.model <- xgboost::xgboost(
                        data = x_train,
                        label = y_train,
                        objective = "binary:logistic",
                        nrounds = 100,
                        verbose = 0
                    )
                }
            } else if (model_type == "logistic_regression") {
                private$.model <- glm(model_formula, data = train_data, family = binomial())
            } else if (model_type == "svm") {
                if (requireNamespace("e1071", quietly = TRUE)) {
                    private$.model <- e1071::svm(
                        model_formula, 
                        data = train_data,
                        type = "C-classification",
                        probability = TRUE
                    )
                }
            }
        },
        
        .generatePredictions = function() {
            model <- private$.model
            test_data <- private$.test_data
            model_type <- self$options$model_type
            outcome_var <- self$options$outcome_var
            
            if (is.null(model)) return()
            
            if (model_type == "random_forest") {
                pred_prob <- predict(model, test_data, type = "prob")[, 2]
                pred_class <- predict(model, test_data)
            } else if (model_type == "gradient_boosting") {
                predictor_vars <- setdiff(names(test_data), outcome_var)
                x_test <- as.matrix(test_data[, predictor_vars, drop = FALSE])
                pred_prob <- predict(model, x_test)
                pred_class <- ifelse(pred_prob > 0.5, levels(test_data[[outcome_var]])[2], levels(test_data[[outcome_var]])[1])
            } else if (model_type == "logistic_regression") {
                pred_prob <- predict(model, test_data, type = "response")
                pred_class <- ifelse(pred_prob > 0.5, levels(test_data[[outcome_var]])[2], levels(test_data[[outcome_var]])[1])
            } else if (model_type == "svm") {
                pred_prob <- attr(predict(model, test_data, probability = TRUE), "probabilities")[, 2]
                pred_class <- predict(model, test_data)
            }
            
            private$.predictions <- list(
                probabilities = pred_prob,
                classes = pred_class,
                actual = test_data[[outcome_var]]
            )
        },
        
        .calculatePerformance = function() {
            predictions <- private$.predictions
            if (is.null(predictions)) return()
            
            # Calculate various performance metrics
            actual <- predictions$actual
            pred_prob <- predictions$probabilities
            pred_class <- predictions$classes
            
            # ROC analysis
            if (requireNamespace("pROC", quietly = TRUE)) {
                roc_obj <- pROC::roc(actual, pred_prob, quiet = TRUE)
                auc_value <- as.numeric(pROC::auc(roc_obj))
                
                # Store for plotting
                private$.roc_data <- list(
                    roc = roc_obj,
                    auc = auc_value
                )
            }
            
            # Confusion matrix
            cm <- table(Predicted = pred_class, Actual = actual)
            
            # Calculate metrics
            if (nrow(cm) == 2 && ncol(cm) == 2) {
                tp <- cm[2, 2]
                tn <- cm[1, 1]
                fp <- cm[2, 1]
                fn <- cm[1, 2]
                
                sensitivity <- tp / (tp + fn)
                specificity <- tn / (tn + fp)
                ppv <- tp / (tp + fp)
                npv <- tn / (tn + fn)
                accuracy <- (tp + tn) / sum(cm)
                f1_score <- 2 * tp / (2 * tp + fp + fn)
                
                private$.performance_metrics <- list(
                    auc = auc_value,
                    accuracy = accuracy,
                    sensitivity = sensitivity,
                    specificity = specificity,
                    ppv = ppv,
                    npv = npv,
                    f1_score = f1_score
                )
            }
        },
        
        .interpretabilityAnalysis = function() {
            model <- private$.model
            test_data <- private$.test_data
            model_type <- self$options$model_type
            
            if (is.null(model)) return()
            
            # Feature importance
            if (model_type == "random_forest") {
                importance_scores <- randomForest::importance(model)[, "MeanDecreaseGini"]
                private$.feature_importance <- sort(importance_scores, decreasing = TRUE)
            } else if (model_type == "gradient_boosting" && requireNamespace("xgboost", quietly = TRUE)) {
                importance_matrix <- xgboost::xgb.importance(model = model)
                private$.feature_importance <- setNames(importance_matrix$Gain, importance_matrix$Feature)
            }
            
            # SHAP analysis
            if (self$options$shap_analysis && requireNamespace("fastshap", quietly = TRUE)) {
                tryCatch({
                    # Simplified SHAP calculation
                    predictor_vars <- setdiff(names(test_data), self$options$outcome_var)
                    x_explain <- test_data[1:min(10, nrow(test_data)), predictor_vars, drop = FALSE]
                    
                    # Create prediction function wrapper
                    pred_wrapper <- function(object, newdata) {
                        if (model_type == "random_forest") {
                            return(predict(object, newdata, type = "prob")[, 2])
                        } else if (model_type == "logistic_regression") {
                            return(predict(object, newdata, type = "response"))
                        } else {
                            return(predict(object, newdata))
                        }
                    }
                    
                    shap_values <- fastshap::explain(
                        model, 
                        X = x_explain, 
                        pred_wrapper = pred_wrapper,
                        nsim = 50
                    )
                    
                    private$.shap_values <- shap_values
                }, error = function(e) {
                    # SHAP calculation failed, continue without it
                    private$.shap_values <- NULL
                })
            }
        },
        
        .riskStratification = function() {
            predictions <- private$.predictions
            if (is.null(predictions)) return()
            
            pred_prob <- predictions$probabilities
            n_groups <- self$options$n_risk_groups
            
            # Create risk groups based on quantiles
            quantiles <- quantile(pred_prob, probs = seq(0, 1, length.out = n_groups + 1))
            risk_groups <- cut(pred_prob, breaks = quantiles, include.lowest = TRUE, 
                              labels = paste("Group", 1:n_groups))
            
            # Calculate group statistics
            actual <- predictions$actual
            risk_stats <- data.frame(
                group = levels(risk_groups),
                n_patients = as.numeric(table(risk_groups)),
                event_rate = tapply(as.numeric(actual) - 1, risk_groups, mean, na.rm = TRUE),
                mean_risk_score = tapply(pred_prob, risk_groups, mean, na.rm = TRUE)
            )
            
            private$.risk_stratification <- risk_stats
        },
        
        .generatePlots = function() {
            # Plots will be generated in the plot functions
        },
        
        .populateTables = function() {
            # Overview table
            overview_data <- data.frame(
                metric = c("Model Type", "Problem Type", "Training Samples", "Test Samples"),
                value = c(
                    self$options$model_type,
                    self$options$problem_type,
                    ifelse(is.null(private$.train_data), "0", nrow(private$.train_data)),
                    ifelse(is.null(private$.test_data), "0", nrow(private$.test_data))
                ),
                stringsAsFactors = FALSE
            )
            
            self$results$overview$setData(overview_data)
            
            # Performance summary
            if (!is.null(private$.performance_metrics)) {
                metrics <- private$.performance_metrics
                performance_data <- data.frame(
                    metric = c("AUC-ROC", "Accuracy", "Sensitivity", "Specificity", "F1 Score"),
                    training = c(NA, NA, NA, NA, NA),  # Would need training predictions
                    validation = c(metrics$auc, metrics$accuracy, metrics$sensitivity, 
                                 metrics$specificity, metrics$f1_score),
                    ci_lower = c(NA, NA, NA, NA, NA),  # Would need bootstrap CIs
                    ci_upper = c(NA, NA, NA, NA, NA),
                    stringsAsFactors = FALSE
                )
                
                self$results$performance_summary$setData(performance_data)
            }
            
            # Feature importance
            if (!is.null(private$.feature_importance) && self$options$interpretability) {
                importance_data <- data.frame(
                    feature = names(private$.feature_importance),
                    importance = as.numeric(private$.feature_importance),
                    importance_rank = seq_along(private$.feature_importance),
                    shap_mean = NA,  # Would be filled if SHAP analysis completed
                    permutation_importance = NA,
                    direction = "Positive",
                    stringsAsFactors = FALSE
                )
                
                self$results$feature_importance$setData(importance_data)
            }
            
            # Risk stratification
            if (!is.null(private$.risk_stratification) && self$options$risk_stratification) {
                risk_data <- private$.risk_stratification
                risk_data$risk_range <- paste0(round(risk_data$mean_risk_score - 0.1, 2), "-", 
                                              round(risk_data$mean_risk_score + 0.1, 2))
                risk_data$clinical_interpretation <- ifelse(risk_data$event_rate > 0.5, "High Risk", 
                                                           ifelse(risk_data$event_rate > 0.2, "Medium Risk", "Low Risk"))
                
                self$results$risk_stratification$setData(risk_data)
            }
        },
        
        .plot_roc = function(image, ggtheme, theme, ...) {
            if (is.null(private$.roc_data) || !self$options$roc_analysis) {
                return()
            }
            
            roc_obj <- private$.roc_data$roc
            auc_value <- private$.roc_data$auc
            
            # Create ROC plot data
            roc_data <- data.frame(
                fpr = 1 - roc_obj$specificities,
                tpr = roc_obj$sensitivities
            )
            
            p <- ggplot(roc_data, aes(x = fpr, y = tpr)) +
                geom_line(color = "blue", size = 1.2) +
                geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
                labs(
                    title = paste("ROC Curve (AUC =", round(auc_value, 3), ")"),
                    x = "False Positive Rate (1 - Specificity)",
                    y = "True Positive Rate (Sensitivity)"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14),
                    axis.title = element_text(size = 12),
                    axis.text = element_text(size = 10)
                )
            
            print(p)
            TRUE
        },
        
        .plot_feature_importance = function(image, ggtheme, theme, ...) {
            if (is.null(private$.feature_importance) || !self$options$interpretability) {
                return()
            }
            
            importance_data <- data.frame(
                feature = names(private$.feature_importance),
                importance = as.numeric(private$.feature_importance)
            )
            
            # Take top 10 features
            importance_data <- importance_data[order(importance_data$importance, decreasing = TRUE), ]
            importance_data <- head(importance_data, 10)
            importance_data$feature <- factor(importance_data$feature, levels = importance_data$feature)
            
            p <- ggplot(importance_data, aes(x = reorder(feature, importance), y = importance)) +
                geom_col(fill = "steelblue", alpha = 0.7) +
                coord_flip() +
                labs(
                    title = "Feature Importance",
                    x = "Features",
                    y = "Importance Score"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14),
                    axis.title = element_text(size = 12),
                    axis.text = element_text(size = 10)
                )
            
            print(p)
            TRUE
        },
        
        .plot_risk_distribution = function(image, ggtheme, theme, ...) {
            if (is.null(private$.predictions) || !self$options$risk_stratification) {
                return()
            }
            
            pred_prob <- private$.predictions$probabilities
            actual <- private$.predictions$actual
            
            plot_data <- data.frame(
                risk_score = pred_prob,
                outcome = actual
            )
            
            p <- ggplot(plot_data, aes(x = risk_score, fill = outcome)) +
                geom_histogram(alpha = 0.7, position = "identity", bins = 30) +
                labs(
                    title = "Risk Score Distribution by Outcome",
                    x = "Predicted Risk Score",
                    y = "Count",
                    fill = "Outcome"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14),
                    axis.title = element_text(size = 12),
                    axis.text = element_text(size = 10),
                    legend.position = "bottom"
                )
            
            print(p)
            TRUE
        }
    )
)