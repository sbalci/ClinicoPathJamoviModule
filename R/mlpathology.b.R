#' @title Classification Performance Metrics for Digital Pathology
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import pROC
#' @import caret
#' @importFrom stats pnorm qnorm binom.test prop.test mcnemar.test
#' @export


mlpathologyClass <- R6::R6Class(
    "mlpathologyClass",
    inherit = mlpathologyBase,
    private = list(
        .init = function() {
            
            # Initialize instructions
            private$.populateInstructions()
            
            # Initialize results tables with proper columns
            private$.initializeResultsTables()
            
        },
        
        .run = function() {
            
            # Check if data is ready
            if (is.null(self$data) || nrow(self$data) == 0) {
                self$results$instructions$setContent(
                    "<p>Welcome to Classification Performance Metrics for Digital Pathology!</p>
                     <p>This module provides comprehensive evaluation of machine learning models and algorithms for pathology applications.</p>
                     <p>Please provide your data to begin the analysis.</p>"
                )
                return()
            }
            
            # Get variables based on analysis type
            analysis_type <- self$options$analysis_type
            
            if (analysis_type == "classification") {
                private$.runClassificationAnalysis()
            } else if (analysis_type == "segmentation") {
                private$.runSegmentationAnalysis()
            } else if (analysis_type == "comparison") {
                private$.runComparisonAnalysis()
            }
            
            # Provide clinical interpretation
            private$.populateInterpretation()
        },
        
        .runClassificationAnalysis = function() {
            
            # Get variables
            actual_var <- self$options$actual_labels
            predicted_var <- self$options$predicted_labels
            probabilities_var <- self$options$predicted_probabilities
            
            # Validate variables
            if (is.null(actual_var) || is.null(predicted_var)) {
                self$results$instructions$setContent(
                    "<p><strong>Variable Selection Required:</strong> Please select both actual and predicted labels for classification analysis.</p>"
                )
                return()
            }
            
            # Extract data
            data <- self$data
            actual <- factor(data[[actual_var]])
            predicted <- factor(data[[predicted_var]])
            
            # Ensure levels match
            all_levels <- union(levels(actual), levels(predicted))
            actual <- factor(actual, levels = all_levels)
            predicted <- factor(predicted, levels = all_levels)
            
            # Perform basic classification analysis
            tryCatch({
                private$.performClassificationMetrics(actual, predicted)
            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<p><strong>Classification Analysis Error:</strong> ", e$message, "</p>")
                )
            })
            
            # ROC analysis if probabilities provided
            if (!is.null(probabilities_var) && self$options$roc_analysis) {
                probabilities <- as.numeric(data[[probabilities_var]])
                tryCatch({
                    private$.performROCAnalysis(actual, probabilities)
                }, error = function(e) {
                    self$results$instructions$setContent(
                        paste0("<p><strong>ROC Analysis Error:</strong> ", e$message, "</p>")
                    )
                })
            }
            
            # Generate confusion matrix plot
            if (self$options$confusion_matrix_plot) {
                private$.populateConfusionMatrixPlot(actual, predicted)
            }
            
            # Generate ROC plot
            if (self$options$roc_plot && !is.null(probabilities_var)) {
                probabilities <- as.numeric(data[[probabilities_var]])
                private$.populateROCPlot(actual, probabilities)
            }
        },
        
        .runSegmentationAnalysis = function() {
            
            # Get variables for segmentation metrics
            reference_var <- self$options$reference_segmentation
            predicted_var <- self$options$predicted_segmentation
            
            if (is.null(reference_var) || is.null(predicted_var)) {
                self$results$instructions$setContent(
                    "<p><strong>Variable Selection Required:</strong> Please select both reference and predicted segmentations.</p>"
                )
                return()
            }
            
            # Extract segmentation data (assuming binary masks)
            data <- self$data
            reference <- as.logical(data[[reference_var]])
            predicted <- as.logical(data[[predicted_var]])
            
            # Perform segmentation analysis
            tryCatch({
                private$.performSegmentationMetrics(reference, predicted)
            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<p><strong>Segmentation Analysis Error:</strong> ", e$message, "</p>")
                )
            })
        },
        
        .runComparisonAnalysis = function() {
            
            # Get variables for model comparison
            actual_var <- self$options$actual_labels
            model1_var <- self$options$model1_predictions
            model2_var <- self$options$model2_predictions
            prob1_var <- self$options$model1_probabilities
            prob2_var <- self$options$model2_probabilities
            
            if (is.null(actual_var) || is.null(model1_var) || is.null(model2_var)) {
                self$results$instructions$setContent(
                    "<p><strong>Variable Selection Required:</strong> Please select actual labels and predictions from both models for comparison.</p>"
                )
                return()
            }
            
            # Extract data
            data <- self$data
            actual <- factor(data[[actual_var]])
            model1_pred <- factor(data[[model1_var]])
            model2_pred <- factor(data[[model2_var]])
            
            # Perform comparison analysis
            tryCatch({
                private$.performModelComparison(actual, model1_pred, model2_pred)
            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<p><strong>Model Comparison Error:</strong> ", e$message, "</p>")
                )
            })
            
            # ROC comparison if probabilities provided
            if (!is.null(prob1_var) && !is.null(prob2_var) && self$options$roc_comparison) {
                prob1 <- as.numeric(data[[prob1_var]])
                prob2 <- as.numeric(data[[prob2_var]])
                tryCatch({
                    private$.performROCComparison(actual, prob1, prob2)
                }, error = function(e) {
                    self$results$instructions$setContent(
                        paste0("<p><strong>ROC Comparison Error:</strong> ", e$message, "</p>")
                    )
                })
            }
        },
        
        .performClassificationMetrics = function(actual, predicted) {
            
            # Create confusion matrix
            if (requireNamespace("caret", quietly = TRUE)) {
                
                cm <- caret::confusionMatrix(predicted, actual)
                
                # Populate confusion matrix table
                cm_table <- self$results$confusionmatrix
                
                # Add confusion matrix values
                cm_matrix <- cm$table
                for (i in 1:nrow(cm_matrix)) {
                    for (j in 1:ncol(cm_matrix)) {
                        cm_table$addRow(rowKey = paste0("cell_", i, "_", j), values = list(
                            actual = rownames(cm_matrix)[i],
                            predicted = colnames(cm_matrix)[j],
                            count = cm_matrix[i, j],
                            percentage = cm_matrix[i, j] / sum(cm_matrix) * 100
                        ))
                    }
                }
                
                # Populate performance metrics table
                metrics_table <- self$results$performancemetrics
                
                # Binary classification metrics
                if (length(levels(actual)) == 2) {
                    
                    # Extract metrics from confusionMatrix
                    sensitivity <- cm$byClass["Sensitivity"]
                    specificity <- cm$byClass["Specificity"]
                    precision <- cm$byClass["Precision"]  # Same as PPV
                    recall <- sensitivity  # Same as sensitivity
                    f1_score <- cm$byClass["F1"]
                    accuracy <- cm$overall["Accuracy"]
                    
                    # Calculate additional metrics
                    tp <- cm$table[2, 2]  # True positives
                    fp <- cm$table[1, 2]  # False positives
                    fn <- cm$table[2, 1]  # False negatives
                    tn <- cm$table[1, 1]  # True negatives
                    
                    # Matthews Correlation Coefficient
                    mcc <- ((tp * tn) - (fp * fn)) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
                    
                    # Add metrics to table
                    metrics_table$addRow(rowKey = "accuracy", values = list(
                        metric = "Accuracy",
                        value = accuracy,
                        ci_lower = cm$overall["AccuracyLower"],
                        ci_upper = cm$overall["AccuracyUpper"],
                        interpretation = ifelse(accuracy > 0.8, "Excellent", 
                                              ifelse(accuracy > 0.7, "Good", 
                                                   ifelse(accuracy > 0.6, "Fair", "Poor")))
                    ))
                    
                    metrics_table$addRow(rowKey = "sensitivity", values = list(
                        metric = "Sensitivity (Recall)",
                        value = sensitivity,
                        ci_lower = NA_real_,
                        ci_upper = NA_real_,
                        interpretation = ifelse(sensitivity > 0.8, "High", 
                                              ifelse(sensitivity > 0.6, "Moderate", "Low"))
                    ))
                    
                    metrics_table$addRow(rowKey = "specificity", values = list(
                        metric = "Specificity",
                        value = specificity,
                        ci_lower = NA_real_,
                        ci_upper = NA_real_,
                        interpretation = ifelse(specificity > 0.8, "High", 
                                              ifelse(specificity > 0.6, "Moderate", "Low"))
                    ))
                    
                    metrics_table$addRow(rowKey = "precision", values = list(
                        metric = "Precision (PPV)",
                        value = precision,
                        ci_lower = NA_real_,
                        ci_upper = NA_real_,
                        interpretation = ifelse(precision > 0.8, "High", 
                                              ifelse(precision > 0.6, "Moderate", "Low"))
                    ))
                    
                    metrics_table$addRow(rowKey = "f1_score", values = list(
                        metric = "F1-Score",
                        value = f1_score,
                        ci_lower = NA_real_,
                        ci_upper = NA_real_,
                        interpretation = ifelse(f1_score > 0.8, "Excellent", 
                                              ifelse(f1_score > 0.7, "Good", "Fair"))
                    ))
                    
                    metrics_table$addRow(rowKey = "mcc", values = list(
                        metric = "Matthews Correlation Coefficient",
                        value = mcc,
                        ci_lower = NA_real_,
                        ci_upper = NA_real_,
                        interpretation = ifelse(mcc > 0.5, "Strong", 
                                              ifelse(mcc > 0.3, "Moderate", 
                                                   ifelse(mcc > 0.1, "Weak", "None")))
                    ))
                }
                
                # Multi-class metrics
                if (length(levels(actual)) > 2) {
                    # Overall accuracy
                    accuracy <- cm$overall["Accuracy"]
                    
                    metrics_table$addRow(rowKey = "accuracy", values = list(
                        metric = "Overall Accuracy",
                        value = accuracy,
                        ci_lower = cm$overall["AccuracyLower"],
                        ci_upper = cm$overall["AccuracyUpper"],
                        interpretation = ifelse(accuracy > 0.8, "Excellent", 
                                              ifelse(accuracy > 0.7, "Good", "Fair"))
                    ))
                    
                    # Macro-averaged metrics
                    macro_f1 <- mean(cm$byClass[, "F1"], na.rm = TRUE)
                    macro_precision <- mean(cm$byClass[, "Precision"], na.rm = TRUE)
                    macro_recall <- mean(cm$byClass[, "Recall"], na.rm = TRUE)
                    
                    metrics_table$addRow(rowKey = "macro_f1", values = list(
                        metric = "Macro-averaged F1",
                        value = macro_f1,
                        ci_lower = NA_real_,
                        ci_upper = NA_real_,
                        interpretation = ifelse(macro_f1 > 0.7, "Good", "Fair")
                    ))
                    
                    metrics_table$addRow(rowKey = "macro_precision", values = list(
                        metric = "Macro-averaged Precision",
                        value = macro_precision,
                        ci_lower = NA_real_,
                        ci_upper = NA_real_,
                        interpretation = ifelse(macro_precision > 0.7, "Good", "Fair")
                    ))
                    
                    metrics_table$addRow(rowKey = "macro_recall", values = list(
                        metric = "Macro-averaged Recall",
                        value = macro_recall,
                        ci_lower = NA_real_,
                        ci_upper = NA_real_,
                        interpretation = ifelse(macro_recall > 0.7, "Good", "Fair")
                    ))
                }
            }
        },
        
        .performROCAnalysis = function(actual, probabilities) {
            
            if (requireNamespace("pROC", quietly = TRUE)) {
                
                # Convert to binary if needed
                if (length(levels(actual)) == 2) {
                    
                    # Create ROC object
                    roc_obj <- pROC::roc(actual, probabilities, quiet = TRUE)
                    
                    # Populate ROC results table
                    roc_table <- self$results$rocanalysis
                    
                    # Calculate confidence interval for AUC
                    auc_ci <- pROC::ci.auc(roc_obj, conf.level = self$options$confidence_level)
                    
                    roc_table$addRow(rowKey = "auc", values = list(
                        metric = "Area Under Curve (AUC)",
                        value = as.numeric(roc_obj$auc),
                        ci_lower = auc_ci[1],
                        ci_upper = auc_ci[3],
                        interpretation = ifelse(roc_obj$auc > 0.9, "Excellent", 
                                              ifelse(roc_obj$auc > 0.8, "Good", 
                                                   ifelse(roc_obj$auc > 0.7, "Fair", 
                                                        ifelse(roc_obj$auc > 0.6, "Poor", "Fail"))))
                    ))
                    
                    # Optimal threshold using Youden's index
                    coords_obj <- pROC::coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
                    
                    roc_table$addRow(rowKey = "optimal_threshold", values = list(
                        metric = "Optimal Threshold (Youden)",
                        value = coords_obj$threshold,
                        ci_lower = NA_real_,
                        ci_upper = NA_real_,
                        interpretation = "Maximizes sensitivity + specificity"
                    ))
                    
                    roc_table$addRow(rowKey = "optimal_sensitivity", values = list(
                        metric = "Sensitivity at Optimal Threshold",
                        value = coords_obj$sensitivity,
                        ci_lower = NA_real_,
                        ci_upper = NA_real_,
                        interpretation = paste0("At threshold = ", round(coords_obj$threshold, 3))
                    ))
                    
                    roc_table$addRow(rowKey = "optimal_specificity", values = list(
                        metric = "Specificity at Optimal Threshold",
                        value = coords_obj$specificity,
                        ci_lower = NA_real_,
                        ci_upper = NA_real_,
                        interpretation = paste0("At threshold = ", round(coords_obj$threshold, 3))
                    ))
                }
            }
        },
        
        .performSegmentationMetrics = function(reference, predicted) {
            
            # Calculate segmentation metrics
            seg_table <- self$results$segmentationmetrics
            
            # Intersection and union for IoU/Jaccard
            intersection <- sum(reference & predicted)
            union <- sum(reference | predicted)
            
            # True positives, false positives, false negatives for DSC
            tp <- intersection
            fp <- sum(!reference & predicted)
            fn <- sum(reference & !predicted)
            
            # Dice Similarity Coefficient
            dice <- 2 * tp / (2 * tp + fp + fn)
            
            # Jaccard Index (Intersection over Union)
            jaccard <- intersection / union
            
            # Sensitivity and Specificity for segmentation
            sensitivity <- tp / (tp + fn)
            tn <- sum(!reference & !predicted)
            specificity <- tn / (tn + fp)
            
            # Add metrics to table
            seg_table$addRow(rowKey = "dice", values = list(
                metric = "Dice Similarity Coefficient",
                value = dice,
                interpretation = ifelse(dice > 0.8, "Excellent overlap", 
                                      ifelse(dice > 0.7, "Good overlap", 
                                           ifelse(dice > 0.5, "Moderate overlap", "Poor overlap")))
            ))
            
            seg_table$addRow(rowKey = "jaccard", values = list(
                metric = "Jaccard Index (IoU)",
                value = jaccard,
                interpretation = ifelse(jaccard > 0.7, "Excellent overlap", 
                                      ifelse(jaccard > 0.5, "Good overlap", 
                                           ifelse(jaccard > 0.3, "Moderate overlap", "Poor overlap")))
            ))
            
            seg_table$addRow(rowKey = "seg_sensitivity", values = list(
                metric = "Segmentation Sensitivity",
                value = sensitivity,
                interpretation = ifelse(sensitivity > 0.8, "High detection rate", 
                                      ifelse(sensitivity > 0.6, "Moderate detection rate", "Low detection rate"))
            ))
            
            seg_table$addRow(rowKey = "seg_specificity", values = list(
                metric = "Segmentation Specificity",
                value = specificity,
                interpretation = ifelse(specificity > 0.8, "Low false positive rate", 
                                      ifelse(specificity > 0.6, "Moderate false positive rate", "High false positive rate"))
            ))
        },
        
        .performModelComparison = function(actual, model1_pred, model2_pred) {
            
            # McNemar's test for paired predictions
            if (requireNamespace("caret", quietly = TRUE)) {
                
                # Calculate accuracy for both models
                acc1 <- sum(actual == model1_pred) / length(actual)
                acc2 <- sum(actual == model2_pred) / length(actual)
                
                # Create contingency table for McNemar's test
                # Rows: Model1 correct/incorrect, Cols: Model2 correct/incorrect
                model1_correct <- actual == model1_pred
                model2_correct <- actual == model2_pred
                
                mcnemar_table <- table(model1_correct, model2_correct)
                
                # Perform McNemar's test
                mcnemar_result <- mcnemar.test(mcnemar_table, correct = TRUE)
                
                # Populate comparison results
                comp_table <- self$results$modelcomparison
                
                comp_table$addRow(rowKey = "model1_accuracy", values = list(
                    comparison = "Model 1 Accuracy",
                    value = acc1,
                    p_value = NA_real_,
                    interpretation = paste0("Correctly classified ", round(acc1 * 100, 1), "% of cases")
                ))
                
                comp_table$addRow(rowKey = "model2_accuracy", values = list(
                    comparison = "Model 2 Accuracy", 
                    value = acc2,
                    p_value = NA_real_,
                    interpretation = paste0("Correctly classified ", round(acc2 * 100, 1), "% of cases")
                ))
                
                comp_table$addRow(rowKey = "accuracy_difference", values = list(
                    comparison = "Accuracy Difference (Model2 - Model1)",
                    value = acc2 - acc1,
                    p_value = NA_real_,
                    interpretation = ifelse(acc2 > acc1, "Model 2 performs better", 
                                          ifelse(acc1 > acc2, "Model 1 performs better", "Similar performance"))
                ))
                
                comp_table$addRow(rowKey = "mcnemar_test", values = list(
                    comparison = "McNemar's Test for Paired Predictions",
                    value = mcnemar_result$statistic,
                    p_value = mcnemar_result$p.value,
                    interpretation = ifelse(mcnemar_result$p.value < 0.05, 
                                          "Significant difference between models", 
                                          "No significant difference between models")
                ))
            }
        },
        
        .performROCComparison = function(actual, prob1, prob2) {
            
            if (requireNamespace("pROC", quietly = TRUE) && length(levels(actual)) == 2) {
                
                # Create ROC objects
                roc1 <- pROC::roc(actual, prob1, quiet = TRUE)
                roc2 <- pROC::roc(actual, prob2, quiet = TRUE)
                
                # DeLong's test for ROC curve comparison
                roc_test <- pROC::roc.test(roc1, roc2, method = "delong")
                
                # Populate ROC comparison results
                roc_comp_table <- self$results$roccomparison
                
                roc_comp_table$addRow(rowKey = "model1_auc", values = list(
                    comparison = "Model 1 AUC",
                    value = as.numeric(roc1$auc),
                    p_value = NA_real_,
                    interpretation = ifelse(roc1$auc > 0.8, "Good discriminative ability", "Fair discriminative ability")
                ))
                
                roc_comp_table$addRow(rowKey = "model2_auc", values = list(
                    comparison = "Model 2 AUC",
                    value = as.numeric(roc2$auc),
                    p_value = NA_real_,
                    interpretation = ifelse(roc2$auc > 0.8, "Good discriminative ability", "Fair discriminative ability")
                ))
                
                roc_comp_table$addRow(rowKey = "auc_difference", values = list(
                    comparison = "AUC Difference (Model2 - Model1)",
                    value = as.numeric(roc2$auc) - as.numeric(roc1$auc),
                    p_value = NA_real_,
                    interpretation = ifelse(roc2$auc > roc1$auc, "Model 2 has higher AUC", 
                                          ifelse(roc1$auc > roc2$auc, "Model 1 has higher AUC", "Similar AUC"))
                ))
                
                roc_comp_table$addRow(rowKey = "delong_test", values = list(
                    comparison = "DeLong's Test for ROC Comparison",
                    value = roc_test$statistic,
                    p_value = roc_test$p.value,
                    interpretation = ifelse(roc_test$p.value < 0.05, 
                                          "Significantly different ROC curves", 
                                          "No significant difference in ROC curves")
                ))
            }
        },
        
        .populateConfusionMatrixPlot = function(actual, predicted) {
            
            image <- self$results$confusionmatrixplot
            image$setState(list(actual = actual, predicted = predicted))
        },
        
        .plotConfusionMatrixPlot = function(image, ggtheme, theme, ...) {
            
            state <- image$state
            actual <- state$actual
            predicted <- state$predicted
            
            if (requireNamespace("ggplot2", quietly = TRUE) && 
                requireNamespace("caret", quietly = TRUE)) {
                
                # Create confusion matrix
                cm <- caret::confusionMatrix(predicted, actual)
                cm_df <- as.data.frame(cm$table)
                
                # Create heatmap
                p <- ggplot2::ggplot(cm_df, ggplot2::aes(x = Reference, y = Prediction, fill = Freq)) +
                    ggplot2::geom_tile(color = "white") +
                    ggplot2::geom_text(ggplot2::aes(label = Freq), size = 4, color = "black") +
                    ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
                    ggplot2::labs(
                        title = "Confusion Matrix Heatmap",
                        x = "Actual",
                        y = "Predicted"
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        axis.text = ggplot2::element_text(size = 10)
                    )
                
                print(p)
                TRUE
            } else {
                FALSE
            }
        },
        
        .populateROCPlot = function(actual, probabilities) {
            
            image <- self$results$rocplot
            image$setState(list(actual = actual, probabilities = probabilities))
        },
        
        .plotROCPlot = function(image, ggtheme, theme, ...) {
            
            state <- image$state
            actual <- state$actual
            probabilities <- state$probabilities
            
            if (requireNamespace("ggplot2", quietly = TRUE) && 
                requireNamespace("pROC", quietly = TRUE) && 
                length(levels(actual)) == 2) {
                
                # Create ROC object
                roc_obj <- pROC::roc(actual, probabilities, quiet = TRUE)
                
                # Create ROC curve plot
                p <- ggplot2::ggplot() +
                    ggplot2::geom_line(ggplot2::aes(x = 1 - roc_obj$specificities, 
                                                   y = roc_obj$sensitivities), 
                                      color = "darkred", size = 1) +
                    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
                                       color = "gray50", alpha = 0.5) +
                    ggplot2::labs(
                        title = paste0("ROC Curve (AUC = ", round(roc_obj$auc, 3), ")"),
                        x = "False Positive Rate (1 - Specificity)",
                        y = "True Positive Rate (Sensitivity)"
                    ) +
                    ggplot2::xlim(0, 1) +
                    ggplot2::ylim(0, 1) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        axis.text = ggplot2::element_text(size = 10)
                    )
                
                print(p)
                TRUE
            } else {
                FALSE
            }
        },
        
        .populateInstructions = function() {
            
            html <- "
            <h2>Classification Performance Metrics for Digital Pathology</h2>
            
            <h3>Purpose</h3>
            <p>This module provides comprehensive evaluation of machine learning models and algorithms for digital pathology applications, including AI model validation, algorithm comparison, and segmentation quality assessment.</p>
            
            <h3>Analysis Types</h3>
            
            <h4>Classification Analysis</h4>
            <p>Evaluates the performance of classification models using:</p>
            <ul>
                <li><strong>Confusion Matrix:</strong> Cross-tabulation of predicted vs actual labels</li>
                <li><strong>Performance Metrics:</strong> Accuracy, sensitivity, specificity, precision, recall, F1-score</li>
                <li><strong>ROC Analysis:</strong> ROC curves, AUC calculation, optimal threshold determination</li>
                <li><strong>Statistical Tests:</strong> Confidence intervals for key metrics</li>
            </ul>
            
            <h4>Segmentation Analysis</h4>
            <p>Assesses segmentation quality using:</p>
            <ul>
                <li><strong>Dice Similarity Coefficient:</strong> Overlap-based metric for segmentation accuracy</li>
                <li><strong>Jaccard Index (IoU):</strong> Intersection over union for spatial overlap</li>
                <li><strong>Sensitivity/Specificity:</strong> Detection and false positive rates for segmentation</li>
            </ul>
            
            <h4>Model Comparison</h4>
            <p>Compares two models using:</p>
            <ul>
                <li><strong>McNemar's Test:</strong> Statistical comparison of paired predictions</li>
                <li><strong>DeLong's Test:</strong> ROC curve comparison for discriminative ability</li>
                <li><strong>Performance Differences:</strong> Direct comparison of accuracy and AUC</li>
            </ul>
            
            <h3>Clinical Applications</h3>
            <ul>
                <li>AI algorithm validation for diagnostic pathology</li>
                <li>Deep learning model performance assessment</li>
                <li>Image segmentation quality control</li>
                <li>Multi-algorithm comparison studies</li>
                <li>Clinical decision support system evaluation</li>
            </ul>
            "
            
            self$results$instructions$setContent(html)
        },
        
        .initializeResultsTables = function() {
            
            # Confusion matrix table
            cm_table <- self$results$confusionmatrix
            cm_table$addColumn(name = "actual", title = "Actual", type = "text")
            cm_table$addColumn(name = "predicted", title = "Predicted", type = "text")
            cm_table$addColumn(name = "count", title = "Count", type = "integer")
            cm_table$addColumn(name = "percentage", title = "Percentage", type = "number", format = "pc")
            
            # Performance metrics table
            metrics_table <- self$results$performancemetrics
            metrics_table$addColumn(name = "metric", title = "Metric", type = "text")
            metrics_table$addColumn(name = "value", title = "Value", type = "number")
            metrics_table$addColumn(name = "ci_lower", title = "CI Lower", type = "number")
            metrics_table$addColumn(name = "ci_upper", title = "CI Upper", type = "number")
            metrics_table$addColumn(name = "interpretation", title = "Interpretation", type = "text")
            
            # ROC analysis table
            roc_table <- self$results$rocanalysis
            roc_table$addColumn(name = "metric", title = "Metric", type = "text")
            roc_table$addColumn(name = "value", title = "Value", type = "number")
            roc_table$addColumn(name = "ci_lower", title = "CI Lower", type = "number")
            roc_table$addColumn(name = "ci_upper", title = "CI Upper", type = "number")
            roc_table$addColumn(name = "interpretation", title = "Interpretation", type = "text")
            
            # Segmentation metrics table
            seg_table <- self$results$segmentationmetrics
            seg_table$addColumn(name = "metric", title = "Metric", type = "text")
            seg_table$addColumn(name = "value", title = "Value", type = "number")
            seg_table$addColumn(name = "interpretation", title = "Interpretation", type = "text")
            
            # Model comparison table
            comp_table <- self$results$modelcomparison
            comp_table$addColumn(name = "comparison", title = "Comparison", type = "text")
            comp_table$addColumn(name = "value", title = "Value", type = "number")
            comp_table$addColumn(name = "p_value", title = "p-value", type = "number", format = "zto,pvalue")
            comp_table$addColumn(name = "interpretation", title = "Interpretation", type = "text")
            
            # ROC comparison table
            roc_comp_table <- self$results$roccomparison
            roc_comp_table$addColumn(name = "comparison", title = "Comparison", type = "text")
            roc_comp_table$addColumn(name = "value", title = "Value", type = "number")
            roc_comp_table$addColumn(name = "p_value", title = "p-value", type = "number", format = "zto,pvalue")
            roc_comp_table$addColumn(name = "interpretation", title = "Interpretation", type = "text")
        },
        
        .populateInterpretation = function() {
            
            html <- "
            <h2>Clinical Interpretation Guidelines</h2>
            
            <h3>Performance Metrics Interpretation</h3>
            <ul>
                <li><strong>Accuracy:</strong> Overall correct classification rate (>80% = Excellent, >70% = Good)</li>
                <li><strong>Sensitivity (Recall):</strong> True positive rate - important for screening applications</li>
                <li><strong>Specificity:</strong> True negative rate - important for confirmation tests</li>
                <li><strong>Precision (PPV):</strong> Positive predictive value - relevance of positive results</li>
                <li><strong>F1-Score:</strong> Harmonic mean of precision and recall - balanced performance measure</li>
            </ul>
            
            <h3>ROC Analysis Interpretation</h3>
            <ul>
                <li><strong>AUC > 0.9:</strong> Excellent discriminative ability</li>
                <li><strong>AUC 0.8-0.9:</strong> Good discriminative ability</li>
                <li><strong>AUC 0.7-0.8:</strong> Fair discriminative ability</li>
                <li><strong>AUC 0.6-0.7:</strong> Poor discriminative ability</li>
                <li><strong>AUC < 0.6:</strong> Fails to discriminate</li>
            </ul>
            
            <h3>Segmentation Quality Assessment</h3>
            <ul>
                <li><strong>Dice > 0.8:</strong> Excellent segmentation overlap</li>
                <li><strong>Dice 0.7-0.8:</strong> Good segmentation overlap</li>
                <li><strong>Dice 0.5-0.7:</strong> Moderate segmentation overlap</li>
                <li><strong>Jaccard > 0.7:</strong> Excellent spatial overlap (IoU)</li>
            </ul>
            
            <h3>Statistical Testing</h3>
            <ul>
                <li><strong>McNemar's Test:</strong> p < 0.05 indicates significant difference between paired models</li>
                <li><strong>DeLong's Test:</strong> p < 0.05 indicates significantly different ROC curves</li>
                <li><strong>Confidence Intervals:</strong> Non-overlapping CIs suggest significant differences</li>
            </ul>
            
            <h3>Clinical Decision Making</h3>
            <ul>
                <li>High sensitivity preferred for screening (minimize false negatives)</li>
                <li>High specificity preferred for confirmation (minimize false positives)</li>
                <li>F1-score useful for balanced evaluation in multi-class problems</li>
                <li>ROC-AUC independent of classification threshold</li>
            </ul>
            "
            
            self$results$interpretation$setContent(html)
        }
    )
)