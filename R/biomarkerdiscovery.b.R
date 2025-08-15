#' @title Biomarker Discovery Platform with ML Interpretability
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom stats predict glm binomial cor
#' @export


biomarkerdiscoveryClass <- R6::R6Class(
    "biomarkerdiscoveryClass",
    inherit = biomarkerdiscoveryBase,
    private = list(
        .model = NULL,
        .data_processed = NULL,
        .selected_biomarkers = NULL,
        .feature_importance = NULL,
        .shap_values = NULL,
        .stability_results = NULL,
        .performance_metrics = NULL,
        
        .init = function() {
            if (is.null(self$options$outcome_var)) {
                self$results$discovery_overview$setNote("note", "Outcome variable is required")
                return()
            }
            
            if (length(self$options$biomarker_vars) == 0) {
                self$results$discovery_overview$setNote("note", "At least one biomarker variable is required")
                return()
            }
            
            # Set table titles
            self$results$discovery_overview$setTitle("Biomarker Discovery Overview")
            self$results$data_summary$setTitle("Dataset Summary")
            self$results$selected_biomarkers$setTitle("Selected Biomarkers")
        },
        
        .run = function() {
            if (is.null(self$options$outcome_var) || length(self$options$biomarker_vars) == 0) {
                return()
            }
            
            # Check for required packages
            private$.checkPackages()
            
            # Process and preprocess data
            private$.preprocessData()
            
            # Quality control
            if (self$options$quality_control) {
                private$.qualityControl()
            }
            
            # Feature selection
            private$.featureSelection()
            
            # Biomarker discovery
            private$.biomarkerDiscovery()
            
            # Stability analysis
            if (self$options$stability_analysis) {
                private$.stabilityAnalysis()
            }
            
            # Model performance evaluation
            private$.evaluatePerformance()
            
            # Interpretability analysis
            if (self$options$interpretability) {
                private$.interpretabilityAnalysis()
            }
            
            # Clinical translation
            if (self$options$cutpoint_optimization) {
                private$.optimalCutpoints()
            }
            
            # Risk stratification
            if (self$options$risk_stratification) {
                private$.riskStratification()
            }
            
            # Update tables
            private$.populateTables()
        },
        
        .checkPackages = function() {
            # Required packages for biomarker discovery
            required_packages <- c("glmnet", "randomForest", "caret", "pROC", "survival")
            
            # Additional packages for specific methods
            if (self$options$discovery_method == "gradient_boosting") {
                required_packages <- c(required_packages, "xgboost")
            }
            
            if (self$options$batch_correction && self$options$batch_method == "combat") {
                required_packages <- c(required_packages, "sva")
            }
            
            if (self$options$shap_analysis) {
                required_packages <- c(required_packages, "fastshap", "shapr")
            }
            
            if (self$options$pathway_analysis) {
                required_packages <- c(required_packages, "clusterProfiler", "org.Hs.eg.db")
            }
            
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    tryCatch({
                        if (pkg %in% c("clusterProfiler", "org.Hs.eg.db")) {
                            if (!requireNamespace("BiocManager", quietly = TRUE)) {
                                install.packages("BiocManager", repos = "https://cran.rstudio.com/")
                            }
                            BiocManager::install(pkg)
                        } else {
                            install.packages(pkg, repos = "https://cran.rstudio.com/")
                        }
                    }, error = function(e) {
                        self$results$discovery_overview$setNote("error", paste("Failed to install package:", pkg))
                    })
                }
            }
        },
        
        .preprocessData = function() {
            data <- self$data
            
            # Get variables
            outcome_var <- self$options$outcome_var
            biomarker_vars <- self$options$biomarker_vars
            clinical_vars <- self$options$clinical_vars
            
            # Create working dataset
            vars_to_include <- c(outcome_var, biomarker_vars)
            if (length(clinical_vars) > 0) {
                vars_to_include <- c(vars_to_include, clinical_vars)
            }
            
            # Add optional variables
            if (!is.null(self$options$patient_id)) {
                vars_to_include <- c(vars_to_include, self$options$patient_id)
            }
            
            if (!is.null(self$options$batch_var)) {
                vars_to_include <- c(vars_to_include, self$options$batch_var)
            }
            
            # Survival analysis variables
            if (self$options$outcome_type == "survival") {
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
            
            # Data normalization
            if (self$options$data_preprocessing) {
                work_data <- private$.normalizeData(work_data)
            }
            
            # Batch correction
            if (self$options$batch_correction && !is.null(self$options$batch_var)) {
                work_data <- private$.batchCorrection(work_data)
            }
            
            private$.data_processed <- work_data
        },
        
        .handleMissingData = function(data) {
            if (self$options$missing_data_analysis) {
                # Basic missing data handling - could be expanded
                missing_pattern <- is.na(data)
                missing_percent <- colSums(missing_pattern) / nrow(data)
                
                # Remove variables with >50% missing data
                high_missing_vars <- names(missing_percent[missing_percent > 0.5])
                if (length(high_missing_vars) > 0) {
                    data <- data[, !names(data) %in% high_missing_vars, drop = FALSE]
                }
            }
            
            # For now, use complete cases - could implement more sophisticated methods
            return(data[complete.cases(data), ])
        },
        
        .normalizeData = function(data) {
            outcome_var <- self$options$outcome_var
            biomarker_vars <- intersect(self$options$biomarker_vars, names(data))
            method <- self$options$normalization_method
            
            # Apply normalization to biomarker variables only
            for (var in biomarker_vars) {
                if (is.numeric(data[[var]])) {
                    if (method == "z_score") {
                        data[[var]] <- scale(data[[var]])[, 1]
                    } else if (method == "min_max") {
                        min_val <- min(data[[var]], na.rm = TRUE)
                        max_val <- max(data[[var]], na.rm = TRUE)
                        data[[var]] <- (data[[var]] - min_val) / (max_val - min_val)
                    } else if (method == "log_transform") {
                        # Add small constant to avoid log(0)
                        data[[var]] <- log2(data[[var]] + 1)
                    } else if (method == "robust") {
                        median_val <- median(data[[var]], na.rm = TRUE)
                        mad_val <- mad(data[[var]], na.rm = TRUE)
                        data[[var]] <- (data[[var]] - median_val) / mad_val
                    }
                }
            }
            
            return(data)
        },
        
        .batchCorrection = function(data) {
            if (!requireNamespace("sva", quietly = TRUE)) {
                return(data)  # Skip if sva not available
            }
            
            batch_var <- self$options$batch_var
            biomarker_vars <- intersect(self$options$biomarker_vars, names(data))
            outcome_var <- self$options$outcome_var
            
            if (self$options$batch_method == "combat") {
                tryCatch({
                    # Prepare data for ComBat
                    biomarker_matrix <- t(as.matrix(data[, biomarker_vars, drop = FALSE]))
                    batch <- data[[batch_var]]
                    
                    # Create model matrix for outcome
                    mod <- model.matrix(~ 1, data = data)
                    if (!is.null(outcome_var) && outcome_var %in% names(data)) {
                        mod <- model.matrix(~ data[[outcome_var]])
                    }
                    
                    # Apply ComBat
                    corrected_data <- sva::ComBat(biomarker_matrix, batch = batch, mod = mod)
                    
                    # Update data
                    data[, biomarker_vars] <- t(corrected_data)
                }, error = function(e) {
                    # ComBat failed, continue without batch correction
                })
            }
            
            return(data)
        },
        
        .qualityControl = function() {
            data <- private$.data_processed
            biomarker_vars <- intersect(self$options$biomarker_vars, names(data))
            
            # Filter low variance features
            if (self$options$filter_low_variance) {
                variance_threshold <- self$options$variance_threshold
                
                biomarker_variances <- sapply(data[, biomarker_vars, drop = FALSE], var, na.rm = TRUE)
                low_var_features <- names(biomarker_variances[biomarker_variances < variance_threshold])
                
                if (length(low_var_features) > 0) {
                    # Remove low variance features
                    private$.data_processed <- private$.data_processed[, !names(private$.data_processed) %in% low_var_features, drop = FALSE]
                }
            }
            
            # Outlier detection
            if (self$options$outlier_detection) {
                private$.detectOutliers()
            }
        },
        
        .detectOutliers = function() {
            data <- private$.data_processed
            biomarker_vars <- intersect(self$options$biomarker_vars, names(data))
            
            # Simple outlier detection using IQR method
            outlier_indices <- c()
            
            for (var in biomarker_vars) {
                if (is.numeric(data[[var]])) {
                    Q1 <- quantile(data[[var]], 0.25, na.rm = TRUE)
                    Q3 <- quantile(data[[var]], 0.75, na.rm = TRUE)
                    IQR <- Q3 - Q1
                    
                    outliers <- which(data[[var]] < Q1 - 1.5 * IQR | data[[var]] > Q3 + 1.5 * IQR)
                    outlier_indices <- c(outlier_indices, outliers)
                }
            }
            
            # For now, just store outlier information - could implement removal/correction
            private$.outlier_indices <- unique(outlier_indices)
        },
        
        .featureSelection = function() {
            data <- private$.data_processed
            outcome_var <- self$options$outcome_var
            biomarker_vars <- intersect(self$options$biomarker_vars, names(data))
            method <- self$options$feature_selection_method
            n_features <- min(self$options$n_features_select, length(biomarker_vars))
            
            if (method == "univariate_stats") {
                # Univariate statistical tests
                p_values <- numeric(length(biomarker_vars))
                names(p_values) <- biomarker_vars
                
                for (i in seq_along(biomarker_vars)) {
                    var <- biomarker_vars[i]
                    if (is.factor(data[[outcome_var]])) {
                        # t-test or Wilcoxon test for binary outcome
                        test_result <- tryCatch({
                            if (length(unique(data[[outcome_var]])) == 2) {
                                t.test(data[[var]] ~ data[[outcome_var]])
                            } else {
                                kruskal.test(data[[var]] ~ data[[outcome_var]])
                            }
                        }, error = function(e) list(p.value = 1))
                        p_values[i] <- test_result$p.value
                    } else {
                        # Correlation test for continuous outcome
                        test_result <- tryCatch({
                            cor.test(data[[var]], data[[outcome_var]])
                        }, error = function(e) list(p.value = 1))
                        p_values[i] <- test_result$p.value
                    }
                }
                
                # FDR correction
                adjusted_p_values <- p.adjust(p_values, method = "fdr")
                
                # Select features
                significant_features <- names(adjusted_p_values[adjusted_p_values < self$options$fdr_threshold])
                selected_features <- head(significant_features, n_features)
                
                # Store results
                private$.selected_biomarkers <- list(
                    features = selected_features,
                    p_values = p_values[selected_features],
                    adjusted_p_values = adjusted_p_values[selected_features]
                )
            } else if (method == "correlation_filter") {
                # Remove highly correlated features
                cor_matrix <- cor(data[, biomarker_vars, drop = FALSE], use = "complete.obs")
                high_cor_pairs <- which(abs(cor_matrix) > self$options$correlation_threshold & 
                                       upper.tri(cor_matrix), arr.ind = TRUE)
                
                # Remove one feature from each highly correlated pair
                features_to_remove <- unique(rownames(cor_matrix)[high_cor_pairs[, 1]])
                selected_features <- setdiff(biomarker_vars, features_to_remove)
                selected_features <- head(selected_features, n_features)
                
                private$.selected_biomarkers <- list(
                    features = selected_features,
                    method = "correlation_filter"
                )
            }
            
            # Update processed data with selected features
            keep_vars <- c(outcome_var, private$.selected_biomarkers$features)
            if (!is.null(self$options$clinical_vars)) {
                keep_vars <- c(keep_vars, intersect(self$options$clinical_vars, names(data)))
            }
            if (!is.null(self$options$patient_id)) {
                keep_vars <- c(keep_vars, self$options$patient_id)
            }
            
            private$.data_processed <- data[, keep_vars, drop = FALSE]
        },
        
        .biomarkerDiscovery = function() {
            data <- private$.data_processed
            outcome_var <- self$options$outcome_var
            method <- self$options$discovery_method
            
            # Prepare predictor variables
            predictor_vars <- setdiff(names(data), c(outcome_var, self$options$patient_id))
            
            if (method == "elastic_net" && requireNamespace("glmnet", quietly = TRUE)) {
                # Elastic Net regularization
                x <- as.matrix(data[, predictor_vars, drop = FALSE])
                y <- data[[outcome_var]]
                
                # Convert factor outcome to numeric if needed
                if (is.factor(y)) {
                    if (length(levels(y)) == 2) {
                        y <- as.numeric(y) - 1  # Convert to 0/1
                        family <- "binomial"
                    } else {
                        y <- as.numeric(y)
                        family <- "multinomial"
                    }
                } else {
                    family <- "gaussian"
                }
                
                # Cross-validation to find optimal lambda
                cv_fit <- glmnet::cv.glmnet(x, y, family = family, alpha = 0.5, nfolds = 10)
                
                # Fit final model
                final_fit <- glmnet::glmnet(x, y, family = family, alpha = 0.5, lambda = cv_fit$lambda.1se)
                
                # Extract coefficients
                coef_matrix <- as.matrix(coef(final_fit))
                selected_biomarkers <- row.names(coef_matrix)[coef_matrix[, 1] != 0][-1]  # Remove intercept
                
                # Store model and results
                private$.model <- final_fit
                private$.feature_importance <- abs(coef_matrix[selected_biomarkers, 1])
                names(private$.feature_importance) <- selected_biomarkers
                
            } else if (method == "random_forest" && requireNamespace("randomForest", quietly = TRUE)) {
                # Random Forest
                formula_str <- paste(outcome_var, "~", paste(predictor_vars, collapse = " + "))
                model_formula <- as.formula(formula_str)
                
                rf_model <- randomForest::randomForest(
                    model_formula, 
                    data = data,
                    ntree = 500,
                    importance = TRUE
                )
                
                # Extract feature importance
                if (is.factor(data[[outcome_var]])) {
                    importance_scores <- randomForest::importance(rf_model)[, "MeanDecreaseGini"]
                } else {
                    importance_scores <- randomForest::importance(rf_model)[, "%IncMSE"]
                }
                
                # Select top features
                top_features <- names(sort(importance_scores, decreasing = TRUE))[1:min(20, length(importance_scores))]
                
                private$.model <- rf_model
                private$.feature_importance <- importance_scores[top_features]
            }
        },
        
        .stabilityAnalysis = function() {
            if (!self$options$stability_analysis) return()
            
            data <- private$.data_processed
            outcome_var <- self$options$outcome_var
            n_bootstrap <- min(self$options$n_bootstrap_samples, 500)  # Limit for computation
            
            set.seed(self$options$random_seed)
            
            # Bootstrap stability analysis
            selected_features_list <- list()
            
            for (i in 1:n_bootstrap) {
                # Bootstrap sample
                boot_indices <- sample(nrow(data), replace = TRUE)
                boot_data <- data[boot_indices, ]
                
                # Feature selection on bootstrap sample
                boot_selected <- private$.bootstrapFeatureSelection(boot_data, outcome_var)
                selected_features_list[[i]] <- boot_selected
            }
            
            # Calculate selection frequency
            all_features <- unique(unlist(selected_features_list))
            selection_freq <- sapply(all_features, function(feature) {
                sum(sapply(selected_features_list, function(x) feature %in% x)) / n_bootstrap
            })
            
            private$.stability_results <- list(
                selection_frequency = selection_freq,
                stable_features = names(selection_freq[selection_freq > 0.7])  # 70% threshold
            )
        },
        
        .bootstrapFeatureSelection = function(data, outcome_var) {
            # Simplified feature selection for bootstrap
            predictor_vars <- setdiff(names(data), c(outcome_var, self$options$patient_id))
            
            if (self$options$discovery_method == "elastic_net" && requireNamespace("glmnet", quietly = TRUE)) {
                x <- as.matrix(data[, predictor_vars, drop = FALSE])
                y <- data[[outcome_var]]
                
                if (is.factor(y)) {
                    y <- as.numeric(y) - 1
                    family <- "binomial"
                } else {
                    family <- "gaussian"
                }
                
                tryCatch({
                    cv_fit <- glmnet::cv.glmnet(x, y, family = family, alpha = 0.5, nfolds = 5)
                    final_fit <- glmnet::glmnet(x, y, family = family, alpha = 0.5, lambda = cv_fit$lambda.1se)
                    
                    coef_matrix <- as.matrix(coef(final_fit))
                    selected_features <- row.names(coef_matrix)[coef_matrix[, 1] != 0][-1]
                    
                    return(selected_features)
                }, error = function(e) {
                    return(character(0))
                })
            }
            
            return(character(0))
        },
        
        .evaluatePerformance = function() {
            if (is.null(private$.model)) return()
            
            data <- private$.data_processed
            outcome_var <- self$options$outcome_var
            
            # Cross-validation performance
            private$.crossValidatePerformance()
        },
        
        .crossValidatePerformance = function() {
            data <- private$.data_processed
            outcome_var <- self$options$outcome_var
            predictor_vars <- setdiff(names(data), c(outcome_var, self$options$patient_id))
            
            set.seed(self$options$random_seed)
            n_folds <- 10
            folds <- caret::createFolds(data[[outcome_var]], k = n_folds)
            
            cv_results <- list()
            
            for (i in 1:n_folds) {
                train_idx <- unlist(folds[-i])
                test_idx <- folds[[i]]
                
                train_data <- data[train_idx, ]
                test_data <- data[test_idx, ]
                
                # Train model on fold
                fold_performance <- private$.trainAndEvaluateFold(train_data, test_data, outcome_var, predictor_vars)
                cv_results[[i]] <- fold_performance
            }
            
            # Aggregate results
            metrics <- c("auc", "accuracy", "sensitivity", "specificity")
            cv_summary <- sapply(metrics, function(metric) {
                values <- sapply(cv_results, function(x) x[[metric]])
                mean(values, na.rm = TRUE)
            })
            
            private$.performance_metrics <- list(
                cv_results = cv_results,
                cv_summary = cv_summary
            )
        },
        
        .trainAndEvaluateFold = function(train_data, test_data, outcome_var, predictor_vars) {
            # Simple logistic regression for fold evaluation
            tryCatch({
                formula_str <- paste(outcome_var, "~", paste(predictor_vars, collapse = " + "))
                model_formula <- as.formula(formula_str)
                
                fold_model <- glm(model_formula, data = train_data, family = binomial())
                
                # Predictions
                pred_prob <- predict(fold_model, test_data, type = "response")
                pred_class <- ifelse(pred_prob > 0.5, levels(test_data[[outcome_var]])[2], levels(test_data[[outcome_var]])[1])
                
                # Calculate metrics
                actual <- test_data[[outcome_var]]
                
                if (requireNamespace("pROC", quietly = TRUE)) {
                    roc_obj <- pROC::roc(actual, pred_prob, quiet = TRUE)
                    auc <- as.numeric(pROC::auc(roc_obj))
                } else {
                    auc <- NA
                }
                
                # Confusion matrix metrics
                cm <- table(Predicted = pred_class, Actual = actual)
                if (nrow(cm) == 2 && ncol(cm) == 2) {
                    tp <- cm[2, 2]
                    tn <- cm[1, 1]
                    fp <- cm[2, 1]
                    fn <- cm[1, 2]
                    
                    accuracy <- (tp + tn) / sum(cm)
                    sensitivity <- tp / (tp + fn)
                    specificity <- tn / (tn + fp)
                } else {
                    accuracy <- sensitivity <- specificity <- NA
                }
                
                return(list(
                    auc = auc,
                    accuracy = accuracy,
                    sensitivity = sensitivity,
                    specificity = specificity
                ))
            }, error = function(e) {
                return(list(auc = NA, accuracy = NA, sensitivity = NA, specificity = NA))
            })
        },
        
        .interpretabilityAnalysis = function() {
            # Simplified interpretability - would be expanded with full SHAP implementation
            if (!is.null(private$.feature_importance)) {
                # Already have feature importance from model
                return()
            }
        },
        
        .optimalCutpoints = function() {
            # Placeholder for optimal cutpoint analysis
            # Would implement methods like Youden index, ROC01, etc.
        },
        
        .riskStratification = function() {
            # Placeholder for risk stratification
            # Would create risk groups based on biomarker signatures
        },
        
        .populateTables = function() {
            # Discovery overview
            n_original_biomarkers <- length(self$options$biomarker_vars)
            n_selected_biomarkers <- length(private$.selected_biomarkers$features)
            
            overview_data <- data.frame(
                characteristic = c("Discovery Method", "Data Type", "Original Biomarkers", 
                                 "Selected Biomarkers", "Samples"),
                value = c(
                    self$options$discovery_method,
                    self$options$data_type,
                    n_original_biomarkers,
                    n_selected_biomarkers,
                    ifelse(is.null(private$.data_processed), "0", nrow(private$.data_processed))
                ),
                stringsAsFactors = FALSE
            )
            
            self$results$discovery_overview$setData(overview_data)
            
            # Selected biomarkers table
            if (!is.null(private$.selected_biomarkers$features)) {
                selected_data <- data.frame(
                    biomarker = private$.selected_biomarkers$features,
                    importance_score = ifelse(is.null(private$.feature_importance), 
                                            NA, 
                                            private$.feature_importance[private$.selected_biomarkers$features]),
                    p_value = ifelse(is.null(private$.selected_biomarkers$p_values), 
                                   NA, 
                                   private$.selected_biomarkers$p_values),
                    fdr_adjusted_p = ifelse(is.null(private$.selected_biomarkers$adjusted_p_values), 
                                          NA, 
                                          private$.selected_biomarkers$adjusted_p_values),
                    effect_size = NA,
                    stability_index = ifelse(is.null(private$.stability_results), 
                                           NA, 
                                           private$.stability_results$selection_frequency[private$.selected_biomarkers$features]),
                    clinical_relevance = "To be determined",
                    stringsAsFactors = FALSE
                )
                
                # Remove rows with all NA values
                selected_data <- selected_data[!is.na(selected_data$biomarker), ]
                
                self$results$selected_biomarkers$setData(selected_data)
            }
            
            # Discovery performance
            if (!is.null(private$.performance_metrics$cv_summary)) {
                performance_data <- data.frame(
                    metric = c("AUC-ROC", "Accuracy", "Sensitivity", "Specificity"),
                    training = NA,
                    validation = private$.performance_metrics$cv_summary[c("auc", "accuracy", "sensitivity", "specificity")],
                    bootstrap_mean = NA,
                    ci_lower = NA,
                    ci_upper = NA,
                    stringsAsFactors = FALSE
                )
                
                self$results$discovery_performance$setData(performance_data)
            }
        },
        
        .plot_biomarker_importance = function(image, ggtheme, theme, ...) {
            if (is.null(private$.feature_importance)) {
                return()
            }
            
            importance_data <- data.frame(
                biomarker = names(private$.feature_importance),
                importance = as.numeric(private$.feature_importance)
            )
            
            # Take top 15 biomarkers
            importance_data <- importance_data[order(importance_data$importance, decreasing = TRUE), ]
            importance_data <- head(importance_data, 15)
            importance_data$biomarker <- factor(importance_data$biomarker, levels = importance_data$biomarker)
            
            p <- ggplot(importance_data, aes(x = reorder(biomarker, importance), y = importance)) +
                geom_col(fill = "steelblue", alpha = 0.7) +
                coord_flip() +
                labs(
                    title = "Top Biomarker Importance Scores",
                    x = "Biomarkers",
                    y = "Importance Score"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14),
                    axis.title = element_text(size = 12),
                    axis.text = element_text(size = 10),
                    axis.text.y = element_text(size = 8)
                )
            
            print(p)
            TRUE
        },
        
        .plot_biomarker_correlation = function(image, ggtheme, theme, ...) {
            if (is.null(private$.data_processed) || is.null(private$.selected_biomarkers$features)) {
                return()
            }
            
            selected_features <- head(private$.selected_biomarkers$features, 15)  # Limit for readability
            if (length(selected_features) < 2) return()
            
            cor_data <- private$.data_processed[, selected_features, drop = FALSE]
            cor_matrix <- cor(cor_data, use = "complete.obs")
            
            # Convert to long format for ggplot
            cor_long <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
            cor_long$value <- as.vector(cor_matrix)
            
            p <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
                geom_tile() +
                scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                                   name = "Correlation") +
                labs(
                    title = "Selected Biomarker Correlation Matrix",
                    x = "Biomarkers",
                    y = "Biomarkers"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14),
                    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
                    axis.text.y = element_text(size = 8),
                    axis.title = element_text(size = 12)
                )
            
            print(p)
            TRUE
        }
    )
)