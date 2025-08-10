#' @title Comprehensive Medical Decision Tree Analysis
#' @return Combined decision tree analysis using FFTrees and enhanced CART for clinical research
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom rpart rpart rpart.control
#' @importFrom rpart.plot rpart.plot
#' @importFrom caret createDataPartition confusionMatrix train trainControl
#' @importFrom pROC roc auc coords
#' @importFrom FFTrees FFTrees
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_bar labs theme_minimal ggtitle
#' @importFrom dplyr summarise group_by mutate select
#' @importFrom htmltools HTML

# Progress bar helper function for tree analysis
treeProgressBar <- function(current, total = 100, message = '', width = 400, height = 30) {
    # Ensure current is within bounds
    current <- max(0, min(current, total))
    
    # Calculate progress percentage
    progress_percent <- current / total * 100
    
    # Determine color based on progress
    bar_color <- if (current < 30) "#FF9800" else if (current < 70) "#2196F3" else "#4CAF50"
    
    # Create responsive HTML progress bar that fits jamovi interface
    html_output <- paste0(
        '<div style="margin: 8px auto; width: 100%; max-width: ', width, 'px; text-align: center;">',
        '<div style="background-color: #f0f0f0; border: 1px solid #ccc; border-radius: 4px; height: ', height, 'px; position: relative; overflow: hidden;">',
        '<div style="background-color: ', bar_color, '; height: 100%; width: ', progress_percent, '%; transition: width 0.3s ease; border-radius: 3px;"></div>',
        '<div style="position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); font-family: Arial, sans-serif; font-size: 11px; color: #333; font-weight: bold;">',
        round(progress_percent, 1), '%',
        '</div>',
        '</div>',
        '</div>'
    )
    
    return(html_output)
}

treeClass <- if (requireNamespace("jmvcore")) R6::R6Class("treeClass",
    inherit = treeBase,
    private = list(

        # Store model results for reuse
        .model_results = NULL,
        .training_data = NULL,
        .test_data = NULL,
        .predictions = NULL,
        .feature_selection_results = NULL,

        .run = function() {

            message("[DEBUG] === TREE FUNCTION STARTING ===")
            message("[DEBUG] Algorithm: ", self$options$algorithm)

            # Method-specific parameter validation and guidance
            param_validation <- private$.validate_method_parameters()
            if (length(param_validation$warnings) > 0) {
                for (warning_msg in param_validation$warnings) {
                    message("[PARAMETER WARNING] ", warning_msg)
                }
            }
            if (length(param_validation$recommendations) > 0) {
                for (rec_msg in param_validation$recommendations) {
                    message("[RECOMMENDATION] ", rec_msg)
                }
            }

            # Comprehensive validation and welcome message handling
            validation_result <- private$.validate_inputs()

            message("[DEBUG] Validation result: ", validation_result$valid)

            if (!validation_result$valid) {
                if (validation_result$show_welcome) {
                    intro_msg <- "
                    <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                    <h3 style='color: #2e7d32; margin-top: 0;'>Welcome to Medical Decision Tree Analysis!</h3>
                    <p><strong>Comprehensive decision tree analysis for clinical research and medical decision making</strong></p>
                    <p>Choose between FFTrees (Fast-and-Frugal Trees) and Enhanced CART algorithms for robust clinical decision support</p>

                    <h4 style='color: #2e7d32;'>Required Variables:</h4>
                    <ol>
                    <li><strong>Predictors:</strong> Select continuous and/or categorical predictor variables</li>
                    <li><strong>Target Outcome:</strong> Select the outcome variable to predict</li>
                    <li><strong>Target Level:</strong> Select the positive class level for prediction</li>
                    </ol>

                    <h4 style='color: #2e7d32;'>Available Algorithms:</h4>
                    <ul>
                    <li><strong>FFTrees (Fast-and-Frugal Trees):</strong> Simple, interpretable trees optimized for medical decisions</li>
                    <li><strong>Enhanced CART (rpart):</strong> Traditional decision trees with advanced pruning and validation</li>
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

                    <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                    💡 <em>Combines the best of FFTrees simplicity and CART comprehensiveness for clinical decision support</em>
                    </p>
                    </div>"

                    self$results$todo$setContent(intro_msg)
                } else {
                    self$results$todo$setContent("")
                    # Show specific error message
                    error_msg <- paste0("
                    <div style='color: #d32f2f; background-color: #ffebee; padding: 15px; border-radius: 8px; border-left: 4px solid #d32f2f;'>
                    <h4 style='margin-top: 0; color: #d32f2f;'>⚠️ Input Validation Error</h4>
                    <p><strong>", validation_result$message, "</strong></p>
                    </div>")
                    self$results$model_summary$setContent(error_msg)
                }
                return()
            } else {
                # Clear any previous messages when validation passes
                self$results$todo$setContent("")

                # Initialize progress feedback with finer granularity
                private$.update_progress("initializing", "Initializing analysis...", 5)

                # Display analysis introduction
                intro_msg <- private$.generate_analysis_introduction()
                self$results$text1$setContent(intro_msg)
                self$results$text1$setVisible(TRUE)
            }

            # Check required packages based on algorithm
            algorithm <- self$options$algorithm
            required_packages <- c("rpart", "rpart.plot", "caret", "pROC")

            if (algorithm == "fftrees") {
                required_packages <- c(required_packages, "FFTrees")
            }

            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    error_msg <- paste0("
                    <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
                    <h4>", pkg, " Package Required</h4>
                    <p>The ", pkg, " package is required for this analysis.</p>
                    <p>Please install it using: <code>install.packages('", pkg, "')</code></p>
                    </div>")
                    self$results$model_summary$setContent(error_msg)
                    return()
                }
            }

            # Set reproducible seed if requested
            if (self$options$set_seed %||% TRUE) {
                seed_value <- self$options$seed_value %||% 42
                set.seed(seed_value)
                message("[DEBUG] Random seed set to: ", seed_value)
            }

            # Determine analysis mode
            tree_mode <- self$options$tree_mode %||% "classification"
            message("[DEBUG] Analysis mode: ", tree_mode)

            # Wrap main analysis in error handling
            tryCatch({
                # Prepare data and build model
                private$.update_progress("data_prep", "Preparing and validating data...", 10)
                message("[DEBUG] Preparing data...")
                data_prepared <- private$.prepare_data()
                if (is.null(data_prepared)) {
                    message("[DEBUG] Data preparation failed, exiting")
                    return()
                }
                
                private$.update_progress("data_validated", "Data validation complete...", 25)
                message("[DEBUG] Data prepared successfully")

                # Train model
                private$.update_progress("model_training", "Training model...", 40)
                private$.update_algorithm_progress(self$options$algorithm, "training")
                message("[DEBUG] Training model with algorithm: ", self$options$algorithm)
                message("[DEBUG] Analysis mode: ", tree_mode)
                private$.checkpoint()  # Add checkpoint before expensive model training
                model_results <- private$.train_model(data_prepared)
                private$.model_results <- model_results

                private$.update_progress("model_completed", "Model training completed...", 60)
                message("[DEBUG] Model training completed: ", !is.null(model_results))

                if (!is.null(model_results)) {
                # Always show a basic success message with correct algorithm
                algorithm_name <- ifelse(model_results$algorithm == "fftrees",
                                       "FFTrees (Fast-and-Frugal Trees)",
                                       "Enhanced CART (rpart)")

                success_html <- paste0("
                <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #2e7d32; margin-top: 0;'>[SUCCESS] ", algorithm_name, " Model Training Successful!</h3>
                <p><strong>Algorithm:</strong> ", algorithm_name, "</p>
                <p><strong>Target:</strong> ", self$options$target, " (", self$options$targetLevel, " vs others)</p>
                <p><strong>Variables:</strong> ", length(c(self$options$vars, self$options$facs)), " predictors, ",
                nrow(private$.training_data), " training cases</p>
                <p>Enable output options in the 'Output Options' section to see detailed results, plots, and performance metrics.</p>
                <p><em>Tip: Turn on 'Show Performance Metrics', 'Show Decision Tree Plot', or 'Show Feature Importance' to see results.</em></p>
                </div>")

                # Always set this basic message
                self$results$text1$setContent(success_html)
                self$results$text1$setVisible(TRUE)
                # Generate performance summary
                private$.update_progress("generating_summary", "Generating performance summary...", 70)
                if (self$options$show_performance_metrics) {
                    summary_html <- private$.generate_model_summary(model_results)
                    self$results$model_summary$setContent(summary_html)

                    # Performance table
                    private$.update_progress("performance_metrics", "Calculating performance metrics...", 75)
                    private$.generate_performance_table(model_results)

                    # Clinical metrics table
                    private$.generate_clinical_metrics_table(model_results)

                    # Performance table explanation
                    perf_explanation <- private$.generate_performance_explanation()
                    self$results$performance_table_explanation$setContent(perf_explanation)
                }

                # Confusion matrix
                if (self$options$show_confusion_matrix) {
                    private$.update_progress("confusion_matrix", "Generating confusion matrix...", 80)
                    cm_html <- private$.generate_confusion_matrix(model_results)
                    self$results$confusion_matrix$setContent(cm_html)

                    # Confusion matrix explanation
                    cm_explanation <- private$.generate_confusion_matrix_explanation()
                    self$results$confusion_matrix_explanation$setContent(cm_explanation)
                }

                # Feature selection results
                if (self$options$feature_selection) {
                    fs_html <- private$.generate_feature_selection_results(model_results)
                    self$results$feature_selection_results$setContent(fs_html)
                }

                # Clinical interpretation
                interpretation_html <- private$.generate_clinical_interpretation()
                self$results$clinical_interpretation$setContent(interpretation_html)

                # CP table and analysis
                if (self$options$show_cp_table && self$options$algorithm == "rpart") {
                    cp_table_html <- private$.generate_cp_table_analysis(model_results)
                    if (!is.null(cp_table_html)) {
                        self$results$cp_table_analysis$setContent(cp_table_html)
                    }
                }

                # Detailed variable importance
                if (self$options$show_variable_importance_detailed) {
                    detailed_importance_html <- private$.generate_detailed_importance_analysis(model_results)
                    if (!is.null(detailed_importance_html)) {
                        self$results$detailed_importance_analysis$setContent(detailed_importance_html)
                    }
                }

                # Enhanced rpart analysis
                if (self$options$competing_splits && self$options$algorithm == "rpart") {
                    competing_splits_html <- private$.generate_competing_splits_analysis(model_results)
                    if (!is.null(competing_splits_html)) {
                        self$results$competing_splits_analysis$setContent(competing_splits_html)
                    }
                }

                if (self$options$surrogate_splits && self$options$algorithm == "rpart") {
                    surrogate_splits_html <- private$.generate_surrogate_splits_analysis(model_results)
                    if (!is.null(surrogate_splits_html)) {
                        self$results$surrogate_splits_analysis$setContent(surrogate_splits_html)
                    }
                }

                # Advanced rpart analysis features
                if (self$options$algorithm == "rpart") {
                    if (self$options$cp_sequence_analysis) {
                        cp_sequence_html <- private$.generate_cp_sequence_analysis(model_results)
                        if (!is.null(cp_sequence_html)) {
                            self$results$cp_sequence_analysis_results$setContent(cp_sequence_html)
                        }
                    }

                    if (self$options$xerror_detailed_analysis) {
                        xerror_html <- private$.generate_xerror_detailed_analysis(model_results)
                        if (!is.null(xerror_html)) {
                            self$results$xerror_detailed_analysis_results$setContent(xerror_html)
                        }
                    }

                    if (self$options$impurity_analysis) {
                        impurity_html <- private$.generate_impurity_analysis(model_results)
                        if (!is.null(impurity_html)) {
                            self$results$impurity_analysis_results$setContent(impurity_html)
                        }
                    }

                    if (self$options$recursive_partitioning_trace) {
                        trace_html <- private$.generate_recursive_partitioning_trace(model_results)
                        if (!is.null(trace_html)) {
                            self$results$recursive_partitioning_trace_results$setContent(trace_html)
                        }
                    }
                }

                # Bootstrap confidence intervals
                if (self$options$bootstrap_confidence) {
                    bootstrap_html <- private$.generate_bootstrap_results(model_results)
                    self$results$bootstrap_intervals$setContent(bootstrap_html)
                }

                # Model comparison
                if (self$options$model_comparison) {
                    private$.update_progress("model_comparison", "Comparing multiple algorithms...", 85)
                    private$.checkpoint()  # Add checkpoint before expensive model comparison
                    private$.generate_model_comparison(data_prepared)
                }

                # Model export information
                if (self$options$export_model) {
                    export_html <- private$.generate_export_info(model_results)
                    self$results$model_export$setContent(export_html)
                }

                # Export predictions
                if (self$options$exportPredictions) {
                    export_pred_html <- private$.generate_export_predictions_info(model_results)
                    self$results$exportInfo$setContent(export_pred_html)
                }

                # Generate explanatory content for visualizations
                if (self$options$show_tree_plot) {
                    tree_explanation <- private$.generate_tree_explanation()
                    self$results$tree_plot_explanation$setContent(tree_explanation)
                }

                if (self$options$show_importance_plot) {
                    importance_explanation <- private$.generate_importance_explanation()
                    self$results$importance_plot_explanation$setContent(importance_explanation)

                    # Feature importance table
                    private$.generate_feature_importance_table(model_results)
                }

                if (self$options$show_roc_curve) {
                    roc_explanation <- private$.generate_roc_explanation()
                    self$results$roc_plot_explanation$setContent(roc_explanation)
                }

                # Mark analysis as completed
                # Generate final results
                private$.update_progress("generating_results", "Generating final results and visualizations...", 90)
                
                private$.update_progress("completed", "Analysis completed successfully!", 100,
                                       paste0("Algorithm: ", toupper(gsub("_", " ", self$options$algorithm)),
                                             " | Mode: ", self$options$tree_mode))
                }

            }, error = function(e) {
                # Handle analysis errors with progress feedback
                error_message <- paste("Analysis failed:", e$message)
                details <- paste("Error occurred during", self$options$algorithm, "analysis. Please check your data and settings.")
                private$.update_progress("error", error_message, details = details)
                message("[ERROR] Analysis failed: ", e$message)
                stop(e$message)
            })
        },

        .plot_tree = function(image, ggtheme, theme, ...) {

            if (is.null(private$.model_results)) {
                message("[DEBUG] No model results available for tree plotting")
                return()
            }

            algorithm <- self$options$algorithm
            message(paste("[DEBUG] Plotting tree for algorithm:", algorithm))

            tryCatch({
                if (algorithm == "fftrees") {
                    # Plot FFTrees with appropriate labels
                    model <- private$.model_results$model
                    message("[DEBUG] Plotting FFTrees model")

                    # Get class labels from training data
                    target_levels <- levels(private$.training_data[[self$options$target]])
                    positive_class <- self$options$targetLevel
                    negative_class <- setdiff(target_levels, positive_class)

                    decision_labels <- if (!is.null(positive_class) && length(negative_class) > 0) {
                        c(paste("Predict", negative_class[1]), paste("Predict", positive_class))
                    } else {
                        c("Predict Negative", "Predict Positive")
                    }

                    plot(model,
                         main = "FFTrees Medical Decision Tree",
                         decision.labels = decision_labels,
                         stats = TRUE)

                    # Add subtitle with helpful information
                    mtext(paste("Target:", self$options$target, "| Positive Class:", positive_class),
                          side = 3, line = 0.5, cex = 0.8, col = "darkblue")

                } else if (algorithm == "rpart") {
                    # Plot rpart tree with enhanced styling
                    model <- private$.model_results$model
                    message("[DEBUG] Plotting rpart model")
                    
                    # Check if model is valid
                    if (is.null(model)) {
                        message("[DEBUG] Model is NULL, cannot plot tree")
                        return()
                    }
                    
                    # Check if model has splits
                    if (nrow(model$splits) == 0) {
                        message("[DEBUG] Model has no splits - creating simple root node visualization")
                        plot.new()
                        plot.window(xlim = c(0, 1), ylim = c(0, 1))
                        text(0.5, 0.5, "Single Node Tree\n(No splits found)", 
                             cex = 1.2, col = "darkblue", font = 2)
                        text(0.5, 0.3, paste("All cases classified as:", 
                                            names(sort(table(private$.training_data[[self$options$target]]), 
                                                       decreasing = TRUE))[1]), 
                             cex = 1, col = "darkred")
                        title("Enhanced CART Decision Tree", col.main = "darkblue")
                        return()
                    }
                    
                    plot_style <- self$options$tree_plot_style %||% "standard"
                    show_node_stats <- self$options$show_node_statistics %||% FALSE

                    # Configure plot parameters based on style
                    plot_params <- private$.get_tree_plot_params(plot_style, show_node_stats)

                    # Determine color palette based on target variable type
                    target_levels <- length(unique(private$.training_data[[self$options$target]]))
                    message(paste("[DEBUG] Target has", target_levels, "levels"))
                    
                    # Set appropriate box.palette for multiclass vs binary
                    if (target_levels > 2) {
                        # Multiclass: use "auto" or list of palettes
                        box_palette <- "auto"  # Let rpart.plot handle multiclass automatically
                        message("[DEBUG] Using 'auto' palette for multiclass problem")
                    } else {
                        # Binary: use custom color palette
                        box_palette <- private$.get_color_palette(plot_style)
                        message("[DEBUG] Using custom palette for binary problem")
                    }

                    # Apply enhanced rpart.plot styling
                    do.call(rpart.plot::rpart.plot, c(
                        list(
                            x = model,
                            main = paste("Enhanced CART Decision Tree -", toupper(plot_style), "Style"),
                            under = TRUE,
                            clip.right.labs = FALSE,
                            box.palette = box_palette,
                            shadow.col = "gray85",
                            nn = TRUE  # Show node numbers
                        ),
                        plot_params
                    ))

                    # Add enhanced subtitle with comprehensive information
                    subtitle <- paste("Target:", self$options$target,
                                    "| Positive Class:", self$options$targetLevel,
                                    "| Nodes:", length(unique(model$where)))
                    mtext(subtitle, side = 3, line = 0.5, cex = 0.8, col = "darkblue")
                    
                } else {
                    message(paste("[DEBUG] Unsupported algorithm for tree plotting:", algorithm))
                    return()
                }

                message("[DEBUG] Tree plot completed successfully")
                TRUE
            }, error = function(e) {
                message(paste("[ERROR] Tree plotting failed:", e$message))
                # Show a simple error message plot instead of silent failure
                plot.new()
                plot.window(xlim = c(0, 1), ylim = c(0, 1))
                text(0.5, 0.5, "Tree Plot Error", cex = 1.5, col = "red", font = 2)
                text(0.5, 0.3, paste("Error:", e$message), cex = 0.8, col = "darkred")
                title("Decision Tree Visualization", col.main = "darkblue")
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

        .plot_confusion_matrix = function(image, ggtheme, theme, ...) {

            if (is.null(private$.model_results)) {
                return()
            }

            model_results <- private$.model_results
            actual <- model_results$actual
            predicted <- model_results$predictions_class
            target_level <- self$options$targetLevel

            if (is.null(actual) || is.null(predicted) || is.null(target_level)) {
                return()
            }

            tryCatch({
                # Create confusion matrix
                cm_table <- table(Predicted = predicted, Actual = actual)

                # Convert to data frame for plotting
                cm_df <- as.data.frame(cm_table)
                names(cm_df) <- c("Predicted", "Actual", "Freq")

                # Calculate percentages
                cm_df$Percentage <- round(cm_df$Freq / sum(cm_df$Freq) * 100, 1)

                # Create labels with both frequency and percentage
                cm_df$Label <- paste0(cm_df$Freq, "\n(", cm_df$Percentage, "%)")

                # Create the heatmap plot
                library(ggplot2)

                p <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
                    geom_tile(color = "white", size = 1) +
                    geom_text(aes(label = Label), color = "white", size = 5, fontface = "bold") +
                    scale_fill_gradient(low = "#3f51b5", high = "#f44336",
                                      name = "Count",
                                      guide = guide_colorbar(barwidth = 1, barheight = 6)) +
                    labs(
                        title = "Confusion Matrix Heatmap",
                        subtitle = paste("Target Level:", target_level),
                        x = "Actual Class",
                        y = "Predicted Class"
                    ) +
                    theme_minimal() +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                        plot.subtitle = element_text(hjust = 0.5, size = 12),
                        axis.title = element_text(size = 12, face = "bold"),
                        axis.text = element_text(size = 11),
                        legend.title = element_text(size = 10),
                        panel.grid = element_blank(),
                        plot.margin = margin(20, 20, 20, 20)
                    ) +
                    coord_equal()

                # Add performance annotations if binary classification
                if (nrow(cm_table) == 2 && ncol(cm_table) == 2) {
                    # Calculate key metrics
                    tn <- cm_table[1, 1]
                    fp <- cm_table[1, 2]
                    fn <- cm_table[2, 1]
                    tp <- cm_table[2, 2]

                    accuracy <- (tp + tn) / (tp + tn + fp + fn)
                    sensitivity <- tp / (tp + fn)
                    specificity <- tn / (tn + fp)

                    # Add metrics as subtitle
                    metrics_text <- paste0("Accuracy: ", round(accuracy, 3),
                                         " | Sensitivity: ", round(sensitivity, 3),
                                         " | Specificity: ", round(specificity, 3))
                    p <- p + labs(caption = metrics_text)
                    p <- p + theme(plot.caption = element_text(hjust = 0.5, size = 10))
                }

                print(p)
                TRUE

            }, error = function(e) {
                # Fallback to simple text-based confusion matrix
                cm_table <- table(Predicted = predicted, Actual = actual)

                # Create a simple plot
                par(mar = c(2, 2, 3, 2))
                plot.new()
                plot.window(xlim = c(0, 1), ylim = c(0, 1))

                title(main = "Confusion Matrix\n(Install ggplot2 for enhanced visualization)",
                      cex.main = 1.2)

                # Simple text display
                cm_text <- capture.output(print(cm_table))
                text(0.5, 0.5, paste(cm_text, collapse = "\n"),
                     cex = 1.2, family = "mono")

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

            tryCatch({
                # Enhanced SHAP-like analysis for all algorithms
                # Note: Full SHAP requires additional packages, this is a simplified version
                algorithm <- self$options$algorithm
                model <- private$.model_results$model
                training_data <- private$.training_data
                target_var <- self$options$target

                if (is.null(training_data) || is.null(model)) {
                    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
                    text(1, 1, "SHAP analysis requires trained model\nand training data",
                         cex = 1.2, col = "gray50")
                    return(TRUE)
                }

                # Get feature importance for all algorithms
                importance <- private$.model_results$importance

                if (!is.null(importance) && nrow(importance) > 0) {
                    # Create enhanced SHAP-style visualization with algorithm-specific styling
                    algorithm_colors <- list(
                        "rpart" = "#3498db",
                        "randomforest" = "#2ecc71",
                        "xgboost" = "#e74c3c",
                        "fftrees" = "#9b59b6",
                        "c50" = "#f39c12",
                        "ctree" = "#1abc9c",
                        "mob" = "#95a5a6"
                    )

                    plot_color <- algorithm_colors[[algorithm]] %||% "#3498db"

                    # Create gradient colors for bars
                    n_features <- nrow(importance)
                    bar_colors <- colorRampPalette(c(paste0(plot_color, "40"), plot_color))(n_features)

                    # Create enhanced SHAP-style plot
                    plot <- ggplot2::ggplot(importance, ggplot2::aes(x = reorder(Feature, Importance), y = Importance)) +
                        ggplot2::geom_bar(stat = "identity",
                                        fill = bar_colors[order(importance$Importance)],
                                        alpha = 0.8,
                                        width = 0.7) +
                        ggplot2::coord_flip() +
                        ggplot2::labs(
                            title = paste0("Feature Impact Analysis - ", toupper(algorithm)),
                            subtitle = "SHAP-like feature contribution to model predictions",
                            x = "Clinical Variables",
                            y = "Average Impact on Model Output",
                            caption = paste0("Algorithm: ", algorithm, " | Features: ", nrow(importance))
                        ) +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(size = 14, face = "bold", color = "#2c3e50"),
                            plot.subtitle = ggplot2::element_text(size = 11, color = "#7f8c8d"),
                            plot.caption = ggplot2::element_text(size = 9, color = "#95a5a6"),
                            axis.text = ggplot2::element_text(size = 10),
                            axis.title = ggplot2::element_text(size = 12, color = "#34495e"),
                            panel.grid.major.y = ggplot2::element_blank(),
                            panel.grid.minor = ggplot2::element_blank(),
                            axis.text.y = ggplot2::element_text(size = 10, color = "#2c3e50")
                        )

                    # Add value labels on bars
                    plot <- plot + ggplot2::geom_text(
                        ggplot2::aes(label = sprintf("%.2f", Importance)),
                        hjust = -0.1, size = 3, color = "#2c3e50"
                    )

                    print(plot)
                    return(TRUE)
                } else {
                    # Algorithm-specific fallback messages
                    fallback_msg <- switch(algorithm,
                        "rpart" = "SHAP Analysis - CART Trees\n\nFeature importance shows variable contribution\nto tree splits and predictions",
                        "randomforest" = "SHAP Analysis - Random Forest\n\nEnsemble of trees provides robust\nfeature importance estimates",
                        "xgboost" = "SHAP Analysis - XGBoost\n\nGradient boosting provides gain-based\nfeature importance measures",
                        "fftrees" = "SHAP Analysis - Fast-and-Frugal Trees\n\nSimple tree structure with\ncue-based importance ranking",
                        "ctree" = "SHAP Analysis - Conditional Inference Trees\n\nStatistically-based splits with\nunbiased variable selection",
                        "c50" = "SHAP Analysis - C5.0 Trees\n\nRule-based importance from\nboosted decision trees",
                        "mob" = "SHAP Analysis - Model-based Trees\n\nCombined parametric and tree-based\nfeature contributions",
                        "SHAP Analysis\n\nFeature importance not available\nfor current model configuration"
                    )

                    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
                    text(1, 1, fallback_msg, cex = 1.1, col = "#7f8c8d", font = 1)
                }

                TRUE
            }, error = function(e) {
                return()
            })
        },

        .plot_partial_dependence = function(image, ggtheme, theme, ...) {

            if (is.null(private$.model_results)) {
                return()
            }

            tryCatch({
                algorithm <- self$options$algorithm
                model <- private$.model_results$model
                training_data <- private$.training_data
                target_var <- self$options$target
                target_level <- self$options$targetLevel

                if (is.null(training_data) || is.null(model)) {
                    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
                    text(1, 1, "Partial dependence plots require\ntrained model and training data",
                         cex = 1.2, col = "gray50")
                    return(TRUE)
                }

                # Get continuous predictor variables for PDP
                continuous_vars <- self$options$vars

                if (!is.null(continuous_vars) && length(continuous_vars) > 0) {
                    # Create partial dependence plots for up to 4 variables
                    n_vars <- min(4, length(continuous_vars))

                    # Set up plot layout
                    if (n_vars == 1) {
                        par(mfrow = c(1, 1))
                    } else if (n_vars == 2) {
                        par(mfrow = c(1, 2))
                    } else {
                        par(mfrow = c(2, 2))
                    }

                    # Algorithm-specific colors
                    algorithm_colors <- c("rpart" = "#3498db", "randomforest" = "#2ecc71",
                                        "xgboost" = "#e74c3c", "fftrees" = "#9b59b6",
                                        "c50" = "#f39c12", "ctree" = "#1abc9c", "mob" = "#95a5a6")
                    plot_color <- algorithm_colors[algorithm] %||% "#3498db"

                    for (i in 1:n_vars) {
                        var_name <- continuous_vars[i]

                        if (var_name %in% names(training_data) && is.numeric(training_data[[var_name]])) {
                            # Create range of values for this variable
                            var_values <- training_data[[var_name]]
                            var_range <- seq(min(var_values, na.rm = TRUE),
                                            max(var_values, na.rm = TRUE),
                                            length.out = 30)  # Reduced for performance

                            # Calculate partial dependence
                            pd_predictions <- numeric(length(var_range))

                            for (j in seq_along(var_range)) {
                                # Create modified dataset with variable fixed at current value
                                temp_data <- training_data
                                temp_data[[var_name]] <- var_range[j]

                                # Get predictions based on algorithm
                                if (algorithm %in% c("rpart", "c50", "ctree")) {
                                    if (self$options$tree_mode == "classification") {
                                        preds <- predict(model, temp_data, type = "prob")
                                        if (is.matrix(preds) && ncol(preds) >= 2) {
                                            if (!is.null(target_level) && target_level %in% colnames(preds)) {
                                                pd_predictions[j] <- mean(preds[, target_level], na.rm = TRUE)
                                            } else {
                                                pd_predictions[j] <- mean(preds[, 2], na.rm = TRUE)
                                            }
                                        } else {
                                            pd_predictions[j] <- mean(as.numeric(preds), na.rm = TRUE)
                                        }
                                    } else {
                                        pd_predictions[j] <- mean(predict(model, temp_data), na.rm = TRUE)
                                    }
                                } else if (algorithm == "randomforest") {
                                    if (self$options$tree_mode == "classification") {
                                        preds <- predict(model, temp_data, type = "prob")
                                        if (!is.null(target_level) && target_level %in% colnames(preds)) {
                                            pd_predictions[j] <- mean(preds[, target_level], na.rm = TRUE)
                                        } else {
                                            pd_predictions[j] <- mean(preds[, 2], na.rm = TRUE)
                                        }
                                    } else {
                                        pd_predictions[j] <- mean(predict(model, temp_data), na.rm = TRUE)
                                    }
                                } else if (algorithm == "xgboost") {
                                    # For XGBoost, need to prepare matrix data
                                    pred_vars <- c(self$options$vars, self$options$facs)
                                    temp_x <- temp_data[pred_vars]

                                    # Convert categorical to numeric (same as training)
                                    for (var in self$options$facs) {
                                        if (var %in% names(temp_x) && is.factor(temp_x[[var]])) {
                                            temp_x[[var]] <- as.numeric(temp_x[[var]]) - 1
                                        }
                                    }

                                    temp_matrix <- as.matrix(temp_x)
                                    dtemp <- xgboost::xgb.DMatrix(data = temp_matrix)
                                    pd_predictions[j] <- mean(predict(model, dtemp), na.rm = TRUE)
                                } else {
                                    # Generic prediction for other algorithms
                                    pd_predictions[j] <- mean(predict(model, temp_data), na.rm = TRUE)
                                }
                            }

                            # Create enhanced partial dependence plot
                            y_label <- if (self$options$tree_mode == "classification") {
                                paste("P(", target_level %||% "Positive", ")", sep = "")
                            } else {
                                "Predicted Value"
                            }

                            plot(var_range, pd_predictions, type = "l",
                                 main = paste("Partial Dependence:", var_name),
                                 sub = paste("Algorithm:", toupper(algorithm)),
                                 xlab = var_name,
                                 ylab = y_label,
                                 col = plot_color, lwd = 3,
                                 cex.main = 1.2, cex.lab = 1.1)

                            # Add confidence band (simplified - using range of individual predictions)
                            y_min <- min(pd_predictions, na.rm = TRUE)
                            y_max <- max(pd_predictions, na.rm = TRUE)
                            y_range <- y_max - y_min

                            if (y_range > 0) {
                                polygon(c(var_range, rev(var_range)),
                                        c(pd_predictions - 0.1 * y_range,
                                          rev(pd_predictions + 0.1 * y_range)),
                                        col = paste0(plot_color, "30"), border = NA)

                                lines(var_range, pd_predictions, col = plot_color, lwd = 3)
                            }

                            # Add grid and rug plot
                            grid(col = "lightgray", lty = "dotted")
                            rug(var_values, col = paste0(plot_color, "60"), lwd = 0.5)

                            # Add horizontal line at mean prediction
                            abline(h = mean(pd_predictions, na.rm = TRUE),
                                   col = "red", lty = 2, lwd = 1)
                        }
                    }

                    return(TRUE)
                } else {
                    # Show message based on algorithm
                    fallback_msg <- switch(algorithm,
                        "rpart" = "Partial Dependence - CART Trees\n\nRequires continuous variables\nto show feature effects on predictions",
                        "randomforest" = "Partial Dependence - Random Forest\n\nEnsemble averaging shows\nsmooth feature relationships",
                        "xgboost" = "Partial Dependence - XGBoost\n\nGradient boosting reveals\ncomplex feature interactions",
                        "fftrees" = "Partial Dependence - FFTrees\n\nSimple tree structure with\nstep-function relationships",
                        "ctree" = "Partial Dependence - Conditional Trees\n\nStatistical splits create\nunbiased feature effects",
                        "Partial Dependence Analysis\n\nRequires continuous variables\nto visualize feature effects"
                    )

                    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
                    text(1, 1, fallback_msg, cex = 1.1, col = "#7f8c8d")
                }

                TRUE
            }, error = function(e) {
                return()
            })
        },

        .plot_interactions = function(image, ggtheme, theme, ...) {

            if (is.null(private$.model_results)) {
                return()
            }

            tryCatch({
                algorithm <- self$options$algorithm

                if (algorithm == "rpart") {
                    model <- private$.model_results$model
                    training_data <- private$.training_data
                    target_var <- self$options$target

                    # Get predictor variables
                    all_vars <- c(self$options$vars, self$options$facs)

                    if (length(all_vars) >= 2) {
                        # Simple interaction analysis: correlation matrix for continuous variables
                        continuous_vars <- self$options$vars

                        if (!is.null(continuous_vars) && length(continuous_vars) >= 2) {
                            # Create correlation matrix
                            cor_data <- training_data[continuous_vars]
                            cor_matrix <- cor(cor_data, use = "complete.obs")

                            # Create a simple heatmap-style interaction plot
                            par(mar = c(5, 5, 4, 2))
                            image(1:ncol(cor_matrix), 1:nrow(cor_matrix), t(cor_matrix),
                                  col = colorRampPalette(c("blue", "white", "red"))(50),
                                  main = "Feature Interactions (Correlation Matrix)",
                                  xlab = "", ylab = "", axes = FALSE)

                            # Add variable names
                            axis(1, at = 1:ncol(cor_matrix), labels = colnames(cor_matrix),
                                 las = 2, cex.axis = 0.8)
                            axis(2, at = 1:nrow(cor_matrix), labels = rownames(cor_matrix),
                                 las = 2, cex.axis = 0.8)

                            # Add correlation values
                            for (i in 1:nrow(cor_matrix)) {
                                for (j in 1:ncol(cor_matrix)) {
                                    text(j, i, round(cor_matrix[i, j], 2), cex = 0.8)
                                }
                            }

                            return(TRUE)
                        }
                    }
                }

                # Fallback message
                plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
                text(1, 1, "Interaction analysis requires at least\n2 continuous variables",
                     cex = 1.2, col = "gray50")

                TRUE
            }, error = function(e) {
                return()
            })
        },

        .plot_cp = function(image, ggtheme, theme, ...) {

            if (is.null(private$.model_results)) {
                return()
            }

            model <- private$.model_results$model
            algorithm <- self$options$algorithm

            if (algorithm != "rpart" || !inherits(model, "rpart")) {
                return()
            }

            tryCatch({
                # Plot complexity parameter
                plotcp(model)

                # Add title and labels
                title(main = "Cross-Validation Error vs Complexity Parameter",
                      sub = "Lower values indicate better performance")

                # Add optimal CP line if 1-SE rule is used
                if (self$options$use_1se_rule) {
                    cp_table <- model$cptable
                    if (!is.null(cp_table) && nrow(cp_table) > 0) {
                        min_xerror <- min(cp_table[, "xerror"])
                        min_se <- cp_table[which.min(cp_table[, "xerror"]), "xstd"]
                        optimal_cp_idx <- which(cp_table[, "xerror"] <= min_xerror + min_se)[1]
                        optimal_cp <- cp_table[optimal_cp_idx, "CP"]

                        # Add vertical line at optimal CP
                        abline(v = log10(optimal_cp), col = "red", lty = 2, lwd = 2)
                        legend("topright", legend = "Optimal CP (1-SE rule)",
                               col = "red", lty = 2, lwd = 2, cex = 0.8)
                    }
                }

            }, error = function(e) {
                plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
                text(1, 1, paste("Error creating CP plot:", e$message), cex = 0.8)
            })
        },

        .plot_rsq = function(image, ggtheme, theme, ...) {

            if (is.null(private$.model_results)) {
                return()
            }

            model <- private$.model_results$model
            algorithm <- self$options$algorithm

            if (algorithm != "rpart" || !inherits(model, "rpart")) {
                return()
            }

            # Check if this is a regression tree
            if (self$options$tree_mode != "regression") {
                plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
                text(1, 1, "R-squared plot only available for regression trees", cex = 0.8)
                return()
            }

            tryCatch({
                # Use rsq.rpart to plot R-squared vs tree size
                rsq.rpart(model)

                # Add title and labels
                title(main = "R-squared vs Tree Size",
                      sub = "Shows explained variance as tree complexity increases")

            }, error = function(e) {
                plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
                text(1, 1, paste("Error creating R-squared plot:", e$message), cex = 0.8)
            })
        },

        .plot_xerror_confidence = function(image, ggtheme, theme, ...) {

            if (is.null(private$.model_results)) {
                return()
            }

            model <- private$.model_results$model
            algorithm <- self$options$algorithm

            if (algorithm != "rpart" || !inherits(model, "rpart")) {
                return()
            }

            tryCatch({
                cp_table <- model$cptable
                if (is.null(cp_table) || nrow(cp_table) == 0) {
                    plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
                    text(1, 1, "No CP table available", cex = 0.8)
                    return()
                }

                # Extract data for plotting
                cp_values <- cp_table[, "CP"]
                xerror <- cp_table[, "xerror"]
                xstd <- cp_table[, "xstd"]

                # Calculate confidence bands
                upper_ci <- xerror + xstd
                lower_ci <- xerror - xstd

                # Find optimal points
                min_xerror_idx <- which.min(xerror)
                min_xerror <- xerror[min_xerror_idx]
                se_threshold <- min_xerror + xstd[min_xerror_idx]
                one_se_idx <- which(xerror <= se_threshold)[1]

                # Create the plot
                x_vals <- 1:length(cp_values)

                # Plot xerror with confidence bands
                plot(x_vals, xerror, type = "b", pch = 16, col = "blue", lwd = 2,
                     xlab = "CP Table Row", ylab = "Cross-Validation Error",
                     main = "Cross-Validation Error with Confidence Bands",
                     ylim = range(c(lower_ci, upper_ci), na.rm = TRUE))

                # Add confidence bands
                polygon(c(x_vals, rev(x_vals)), c(upper_ci, rev(lower_ci)),
                        col = rgb(0, 0, 1, 0.2), border = NA)

                # Add confidence band lines
                lines(x_vals, upper_ci, col = "lightblue", lty = 2, lwd = 1)
                lines(x_vals, lower_ci, col = "lightblue", lty = 2, lwd = 1)

                # Mark minimum xerror point
                points(min_xerror_idx, xerror[min_xerror_idx], col = "orange", pch = 19, cex = 2)

                # Mark 1-SE rule selection
                points(one_se_idx, xerror[one_se_idx], col = "green", pch = 19, cex = 2)

                # Add horizontal line for 1-SE threshold
                abline(h = se_threshold, col = "green", lty = 2, lwd = 2)

                # Add grid
                grid(col = "gray90", lty = 1)

                # Add legend
                legend("topright",
                       legend = c("Cross-validation Error",
                                 "±1 Standard Error",
                                 "Minimum Error",
                                 "1-SE Rule Selection",
                                 "1-SE Threshold"),
                       col = c("blue", "lightblue", "orange", "green", "green"),
                       pch = c(16, NA, 19, 19, NA),
                       lty = c(1, 2, NA, NA, 2),
                       lwd = c(2, 1, NA, NA, 2),
                       cex = 0.8)

                # Add text annotations
                text(min_xerror_idx, xerror[min_xerror_idx] + 0.02,
                     paste("Min\n(Row", min_xerror_idx, ")"),
                     col = "orange", cex = 0.8, adj = c(0.5, 0))

                text(one_se_idx, xerror[one_se_idx] - 0.02,
                     paste("1-SE Rule\n(Row", one_se_idx, ")"),
                     col = "green", cex = 0.8, adj = c(0.5, 1))

            }, error = function(e) {
                plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
                text(1, 1, paste("Error creating xerror confidence plot:", e$message), cex = 0.8)
            })
        },

        .prepare_data = function() {

            tryCatch({
                message("[DEBUG-PREPARE] Starting data preparation...")

                # Get variables
                continuous_vars <- self$options$vars
                categorical_vars <- self$options$facs
                target_var <- self$options$target
                target_level <- self$options$targetLevel

                message("[DEBUG-PREPARE] Variables - Continuous: ", paste(continuous_vars %||% "none", collapse = ", "))
                message("[DEBUG-PREPARE] Variables - Categorical: ", paste(categorical_vars %||% "none", collapse = ", "))
                message("[DEBUG-PREPARE] Target: ", target_var %||% "none", " (level: ", target_level %||% "none", ")")
                message("[DEBUG-PREPARE] Data dimensions: ", nrow(self$data), " x ", ncol(self$data))

                # Combine all predictor variables
                all_vars <- c(continuous_vars, categorical_vars)
                if (length(all_vars) == 0) {
                    stop("Please select at least one predictor variable.")
                }

                # Create analysis dataset
                analysis_data <- self$data[c(all_vars, target_var)]

                # Handle missing data with medical-appropriate methods
                missing_strategy <- self$options$missing_data_method %||% "native"

                if (missing_strategy == "clinical_impute") {
                    missing_strategy <- "simple"
                } else if (missing_strategy == "exclude") {
                    missing_strategy <- "complete"
                }

                if (missing_strategy == "complete") {
                    # Record missing data patterns before removal
                    n_complete <- sum(complete.cases(analysis_data))
                    n_total <- nrow(analysis_data)
                    if (n_complete < n_total * 0.7) {
                        warning(paste("High missing data:", round((n_total - n_complete)/n_total * 100, 1),
                                    "% of cases will be excluded. Consider imputation strategies."))
                    }
                    analysis_data <- analysis_data[complete.cases(analysis_data), ]
                } else if (missing_strategy == "simple") {
                    # Medical-appropriate imputation
                    for (var in all_vars) {
                        if (is.numeric(analysis_data[[var]])) {
                            # For biomarkers/lab values, use median within outcome groups when possible
                            if (!is.null(target_var) && length(unique(analysis_data[[target_var]])) <= 5) {
                                # Group-wise median imputation
                                for (group in unique(analysis_data[[target_var]])) {
                                    group_mask <- analysis_data[[target_var]] == group & is.na(analysis_data[[var]])
                                    if (sum(group_mask) > 0) {
                                        group_median <- median(analysis_data[[var]][analysis_data[[target_var]] == group], na.rm = TRUE)
                                        if (!is.na(group_median)) {
                                            analysis_data[[var]][group_mask] <- group_median
                                        } else {
                                            # Fallback to overall median
                                            analysis_data[[var]][group_mask] <- median(analysis_data[[var]], na.rm = TRUE)
                                        }
                                    }
                                }
                            } else {
                                # Overall median for continuous variables (more robust than mean)
                                analysis_data[[var]][is.na(analysis_data[[var]])] <- median(analysis_data[[var]], na.rm = TRUE)
                            }
                        } else {
                            # For categorical variables, use mode within outcome groups when possible
                            if (!is.null(target_var) && length(unique(analysis_data[[target_var]])) <= 5) {
                                # Group-wise mode imputation
                                for (group in unique(analysis_data[[target_var]])) {
                                    group_mask <- analysis_data[[target_var]] == group & is.na(analysis_data[[var]])
                                    if (sum(group_mask) > 0) {
                                        group_vals <- analysis_data[[var]][analysis_data[[target_var]] == group]
                                        group_mode <- names(sort(table(group_vals), decreasing = TRUE))[1]
                                        if (!is.na(group_mode)) {
                                            analysis_data[[var]][group_mask] <- group_mode
                                        } else {
                                            # Fallback to overall mode
                                            mode_val <- names(sort(table(analysis_data[[var]]), decreasing = TRUE))[1]
                                            analysis_data[[var]][group_mask] <- mode_val
                                        }
                                    }
                                }
                            } else {
                                # Overall mode imputation
                                mode_val <- names(sort(table(analysis_data[[var]]), decreasing = TRUE))[1]
                                analysis_data[[var]][is.na(analysis_data[[var]])] <- mode_val
                            }
                        }
                    }
                }

                # Enhanced sample size validation with specific guidance
                n_samples <- nrow(analysis_data)
                if (n_samples < 10) {
                    stop("Critical error: Only ", n_samples, " complete cases available. Decision trees require at least 30 cases for reliable results. Consider:\n• Collecting more data\n• Using simpler statistical methods\n• Reducing the number of predictors")
                } else if (n_samples < 30) {
                    stop("Insufficient data: Only ", n_samples, " complete cases available. Decision trees require at least 30 cases for reliable analysis. Consider:\n• Collecting more data\n• Using different missing data strategies\n• Simplifying the model with fewer predictors")
                } else if (n_samples < 50) {
                    warning("Small sample size (", n_samples, " cases). Results may be unstable. Consider cross-validation and interpret results cautiously.")
                }

                # Enhanced target variable validation
                target_data <- analysis_data[[target_var]]
                unique_targets <- unique(target_data)
                n_unique <- length(unique_targets)

                if (n_unique < 2) {
                    stop("Target variable error: Only ", n_unique, " unique value found ('", unique_targets[1], "'). Classification requires at least 2 different outcome values.")
                } else if (n_unique > 10) {
                    stop("Target variable error: ", n_unique, " unique values found. This appears to be a continuous variable. Use regression methods instead of classification trees.")
                }

                # Check class balance for binary classification
                if (n_unique == 2) {
                    class_counts <- table(target_data)
                    min_class_prop <- min(class_counts) / sum(class_counts)
                    min_class_name <- names(class_counts)[which.min(class_counts)]

                    if (min_class_prop < 0.05) {
                        stop("Severe class imbalance detected: '", min_class_name, "' represents only ",
                             round(min_class_prop * 100, 1), "% of cases (", min(class_counts), "/", sum(class_counts),
                             "). Consider:\n• Collecting more balanced data\n• Using oversampling techniques\n• Different statistical approaches for rare events")
                    } else if (min_class_prop < 0.1) {
                        warning("Class imbalance detected: '", min_class_name, "' represents ",
                                round(min_class_prop * 100, 1), "% of cases. Consider enabling 'Balance Disease Classes' option.")
                    }
                }

                # Convert target to factor if needed and reorder levels
                if (!is.factor(target_data)) {
                    analysis_data[[target_var]] <- as.factor(target_data)
                }

                # Check target level exists
                if (!is.null(target_level) && !(target_level %in% levels(analysis_data[[target_var]]))) {
                    stop(paste("Target level", target_level, "not found in target variable."))
                }

                # Reorder factor levels to make the selected positive class the second level
                # This ensures it's treated as the "positive" class in binary classification
                if (!is.null(target_level)) {
                    current_levels <- levels(analysis_data[[target_var]])
                    if (length(current_levels) == 2) {
                        # For binary classification, ensure positive class is second level
                        other_level <- setdiff(current_levels, target_level)
                        analysis_data[[target_var]] <- factor(analysis_data[[target_var]],
                                                            levels = c(other_level, target_level))

                        # Store level information for use in modeling
                        attr(analysis_data, "positive_class") <- target_level
                        attr(analysis_data, "negative_class") <- other_level
                    } else if (length(current_levels) > 2) {
                        # For multi-class, put the selected level first for reference
                        other_levels <- setdiff(current_levels, target_level)
                        analysis_data[[target_var]] <- factor(analysis_data[[target_var]],
                                                            levels = c(target_level, other_levels))
                        attr(analysis_data, "reference_class") <- target_level
                    }
                }

                # Feature selection if requested
                if (self$options$feature_selection && length(all_vars) > 3) {
                    analysis_data <- private$.perform_feature_selection(analysis_data, all_vars, target_var)
                }

                return(analysis_data)

            }, error = function(e) {
                message("[DEBUG] Data preparation error: ", e$message)
                message("[DEBUG] Continuous vars: ", paste(self$options$vars %||% "none", collapse = ", "))
                message("[DEBUG] Categorical vars: ", paste(self$options$facs %||% "none", collapse = ", "))
                message("[DEBUG] Target var: ", self$options$target %||% "none")
                message("[DEBUG] Target level: ", self$options$targetLevel %||% "none")

                error_msg <- paste0("
                <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
                <h4>Data Preparation Error</h4>
                <p><strong>Error:</strong> ", e$message, "</p>
                <p><strong>Variables:</strong> Continuous: ", paste(self$options$vars %||% "none", collapse = ", "),
                ", Categorical: ", paste(self$options$facs %||% "none", collapse = ", "), "</p>
                <p><strong>Target:</strong> ", self$options$target %||% "none", " (level: ", self$options$targetLevel %||% "none", ")</p>
                <p>Please check that all variables exist in your dataset and that the target variable has at least two categories.</p>
                </div>")
                self$results$model_summary$setContent(error_msg)
                return(NULL)
            })
        },

        .perform_feature_selection = function(data, all_vars, target_var) {

            # Get feature selection method from options (default to tree-based)
            fs_method <- self$options$feature_selection_method %||% "tree_importance"

            tryCatch({
                if (fs_method == "boruta" && requireNamespace("Boruta", quietly = TRUE)) {
                    return(private$.perform_boruta_selection(data, all_vars, target_var))
                } else if (fs_method == "var_imp" && requireNamespace("caret", quietly = TRUE)) {
                    return(private$.perform_varimportance_selection(data, all_vars, target_var))
                } else {
                    # Default: Simple tree-based importance using rpart
                    return(private$.perform_tree_importance_selection(data, all_vars, target_var))
                }

            }, error = function(e) {
                message("Feature selection failed, using all features: ", e$message)
                return(data)
            })
        },

        # Boruta feature selection
        .perform_boruta_selection = function(data, all_vars, target_var) {
            private$.update_progress("data_prep", "Running Boruta feature selection...",
                                   details = paste("Analyzing", length(all_vars), "features for relevance"))

            # Build formula for Boruta
            formula_str <- paste(target_var, "~", paste(all_vars, collapse = " + "))
            temp_formula <- as.formula(formula_str)

            # Run Boruta algorithm
            set.seed(self$options$seed_value %||% 123)
            private$.checkpoint()  # Add checkpoint before expensive Boruta feature selection
            boruta_result <- Boruta::Boruta(temp_formula, data = data,
                                          maxRuns = 100, # Limit runs for performance
                                          doTrace = 0)   # Suppress verbose output

            # Get final decision
            final_decision <- Boruta::TentativeRoughFix(boruta_result)

            # Extract confirmed and tentative features
            confirmed_attrs <- names(final_decision$finalDecision)[final_decision$finalDecision == "Confirmed"]
            tentative_attrs <- names(final_decision$finalDecision)[final_decision$finalDecision == "Tentative"]
            rejected_attrs <- names(final_decision$finalDecision)[final_decision$finalDecision == "Rejected"]

            # Combine confirmed and tentative features
            selected_features <- c(confirmed_attrs, tentative_attrs)

            # Get importance scores
            importance_scores <- final_decision$ImpHistory
            mean_importance <- apply(importance_scores, 2, median, na.rm = TRUE)

            # Store Boruta results
            private$.feature_selection_results <- list(
                original_features = all_vars,
                selected_features = selected_features,
                confirmed_features = confirmed_attrs,
                tentative_features = tentative_attrs,
                rejected_features = rejected_attrs,
                importance_scores = mean_importance[selected_features],
                boruta_stats = list(
                    total_runs = boruta_result$timeTaken,
                    confirmed_count = length(confirmed_attrs),
                    tentative_count = length(tentative_attrs),
                    rejected_count = length(rejected_attrs)
                ),
                method = "Boruta Algorithm"
            )

            # Return data with selected features
            if (length(selected_features) > 0) {
                selected_data <- data[c(selected_features, target_var)]
                message("[DEBUG] Boruta selected ", length(selected_features), " features from ", length(all_vars))
                return(selected_data)
            } else {
                message("[DEBUG] Boruta rejected all features, using original data")
                return(data)
            }
        },

        # Variable importance using caret
        .perform_varimportance_selection = function(data, all_vars, target_var) {
            # Build a model for variable importance
            formula_str <- paste(target_var, "~", paste(all_vars, collapse = " + "))
            temp_formula <- as.formula(formula_str)

            # Use random forest for variable importance if available
            if (requireNamespace("randomForest", quietly = TRUE)) {
                temp_model <- randomForest::randomForest(temp_formula, data = data, importance = TRUE)
                importance_scores <- randomForest::importance(temp_model)[, "MeanDecreaseGini"]
            } else {
                # Fallback to rpart
                temp_model <- rpart::rpart(temp_formula, data = data)
                importance_scores <- temp_model$variable.importance
            }

            if (length(importance_scores) > 0) {
                # Sort by importance
                importance_sorted <- sort(importance_scores, decreasing = TRUE)

                # Select top features using elbow method or top 75%
                threshold <- quantile(importance_sorted, 0.25) # Keep top 75%
                selected_features <- names(importance_sorted)[importance_sorted >= threshold]
                selected_features <- selected_features[1:min(length(selected_features),
                                                           max(3, ceiling(length(all_vars) * 0.75)))]

                private$.feature_selection_results <- list(
                    original_features = all_vars,
                    selected_features = selected_features,
                    importance_scores = importance_sorted[selected_features],
                    threshold_used = threshold,
                    method = "Variable Importance (caret)"
                )

                selected_data <- data[c(selected_features, target_var)]
                return(selected_data)
            } else {
                return(data)
            }
        },

        # Original tree-based importance method (refactored)
        .perform_tree_importance_selection = function(data, all_vars, target_var) {
            tryCatch({
                # Build formula for tree
                formula_str <- paste(target_var, "~", paste(all_vars, collapse = " + "))
                temp_formula <- as.formula(formula_str)

                # Use rpart for feature selection as it's always available
                temp_model <- rpart::rpart(temp_formula, data = data,
                                         control = rpart::rpart.control(cp = 0.01))

                # Get variable importance
                importance <- temp_model$variable.importance

            if (length(importance) > 0) {
                    # Sort by importance and select top features
                    importance_sorted <- sort(importance, decreasing = TRUE)

                    # Select top 75% of features or at least 3 features
                    n_features <- max(3, ceiling(length(importance_sorted) * 0.75))
                    selected_features <- names(importance_sorted)[1:min(n_features, length(importance_sorted))]

                    # Store feature selection results for reporting
                    private$.feature_selection_results <- list(
                        original_features = all_vars,
                        selected_features = selected_features,
                        importance_scores = importance_sorted,
                        method = "Tree-based importance (rpart)"
                    )

                    # Return data with selected features only
                    selected_data <- data[c(selected_features, target_var)]
                    return(selected_data)
                } else {
                    # If no importance available, return original data
                    private$.feature_selection_results <- list(
                        original_features = all_vars,
                        selected_features = all_vars,
                        importance_scores = NULL,
                        method = "No selection - insufficient variation"
                    )
                    return(data)
                }

            }, error = function(e) {
                # On error, return original data
                private$.feature_selection_results <- list(
                    original_features = all_vars,
                    selected_features = all_vars,
                    importance_scores = NULL,
                    method = paste("Feature selection failed:", e$message)
                )
                return(data)
            })
        },

        # Enhanced importance calculation using caret's varImp
        .calculate_varimportance = function(model, algorithm, method) {

            tryCatch({
                # Use caret's varImp for comprehensive importance calculation
                var_imp <- caret::varImp(model, scale = TRUE)

                if (is.null(var_imp)) {
                    return(NULL)
                }

                # Extract importance scores
                if (inherits(var_imp, "varImp.train")) {
                    importance_scores <- var_imp$importance
                } else if (is.data.frame(var_imp)) {
                    importance_scores <- var_imp
                } else if (is.matrix(var_imp)) {
                    importance_scores <- as.data.frame(var_imp)
                } else {
                    return(NULL)
                }

                # Handle different importance score formats
                if (ncol(importance_scores) == 1) {
                    # Single column of importance scores
                    imp_values <- importance_scores[, 1]
                    feature_names <- rownames(importance_scores)
                } else if ("Overall" %in% colnames(importance_scores)) {
                    # Use Overall column if available
                    imp_values <- importance_scores$Overall
                    feature_names <- rownames(importance_scores)
                } else {
                    # Use first column as fallback
                    imp_values <- importance_scores[, 1]
                    feature_names <- rownames(importance_scores)
                }

                # Create formatted result
                if (length(imp_values) > 0 && !is.null(feature_names)) {
                    importance_data <- data.frame(
                        Feature = feature_names,
                        Importance = as.numeric(imp_values),
                        Method = paste("caret::varImp -", method),
                        stringsAsFactors = FALSE
                    )

                    # Sort by importance (descending)
                    importance_data <- importance_data[order(importance_data$Importance, decreasing = TRUE), ]

                    # Add rank
                    importance_data$Rank <- 1:nrow(importance_data)

                    return(importance_data)
                } else {
                    return(NULL)
                }

            }, error = function(e) {
                message("caret::varImp failed: ", e$message)
                return(NULL)
            })
        },

        .calculate_importance = function(model, algorithm) {

            importance_method <- self$options$importance_method %||% "gini"

            tryCatch({
                # First try using caret's varImp for enhanced importance calculation
                if (importance_method == "shap" ||
                    (requireNamespace("caret", quietly = TRUE) &&
                     algorithm %in% c("rpart", "randomforest", "xgboost"))) {

                    varimport_result <- private$.calculate_varimportance(model, algorithm, importance_method)
                    if (!is.null(varimport_result)) {
                        return(varimport_result)
                    }
                    # If varImp fails, fall back to algorithm-specific methods
                }
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
                } else if (algorithm == "fftrees") {
                    # For FFTrees, extract cue importance from the best tree
                    best_tree <- model$tree$best$train
                    if (!is.null(best_tree) && "cues" %in% names(best_tree)) {
                        cues_used <- best_tree$cues
                        if (length(cues_used) > 0) {
                            # Simple importance based on order in tree (first = most important)
                            importance_data <- data.frame(
                                Feature = cues_used,
                                Importance = seq(length(cues_used), 1),
                                stringsAsFactors = FALSE
                            )
                        } else {
                            return(NULL)
                        }
                    } else {
                        return(NULL)
                    }
                } else if (algorithm == "randomforest") {
                    # For Random Forest, use variable importance
                    if (!is.null(model$importance)) {
                        # If importance matrix exists, use MeanDecreaseGini or first column
                        if (is.matrix(model$importance)) {
                            if ("MeanDecreaseGini" %in% colnames(model$importance)) {
                                imp <- model$importance[, "MeanDecreaseGini"]
                            } else if ("MeanDecreaseAccuracy" %in% colnames(model$importance)) {
                                imp <- model$importance[, "MeanDecreaseAccuracy"]
                            } else {
                                imp <- model$importance[, 1]  # Use first column
                            }
                        } else {
                            imp <- model$importance
                        }

                        if (is.null(imp) || length(imp) == 0) {
                            return(NULL)
                        }

                        importance_data <- data.frame(
                            Feature = names(imp),
                            Importance = as.numeric(imp),
                            stringsAsFactors = FALSE
                        )
                    } else {
                        return(NULL)
                    }

                } else if (algorithm == "xgboost") {
                    # For XGBoost, use feature importance
                    if (!is.null(model$feature_names)) {
                        tryCatch({
                            # Get importance using xgb.importance
                            importance_matrix <- xgboost::xgb.importance(
                                feature_names = model$feature_names,
                                model = model
                            )

                            if (is.null(importance_matrix) || nrow(importance_matrix) == 0) {
                                return(NULL)
                            }

                            # Use Gain as the importance measure (could also use Cover or Frequency)
                            importance_data <- data.frame(
                                Feature = importance_matrix$Feature,
                                Importance = importance_matrix$Gain,
                                stringsAsFactors = FALSE
                            )

                        }, error = function(e) {
                            return(NULL)
                        })
                    } else {
                        return(NULL)
                    }
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
                    private$.checkpoint()  # Add checkpoint before expensive CV fold processing
                    
                    train_idx <- unlist(folds[-i])
                    test_idx <- folds[[i]]

                    train_fold <- data[train_idx, ]
                    test_fold <- data[test_idx, ]

                    # Train model on fold based on algorithm
                    if (algorithm == "rpart") {
                        fold_model <- rpart::rpart(formula, data = train_fold)
                        fold_pred <- predict(fold_model, test_fold, type = "class")
                    } else if (algorithm == "fftrees") {
                        message("[DEBUG-CV] FFTrees fold ", i, " - train size: ", nrow(train_fold), ", test size: ", nrow(test_fold))
                        fold_model <- FFTrees::FFTrees(formula, data = train_fold, data.test = test_fold, quiet = list(ini = TRUE, fin = FALSE, mis = FALSE, set = TRUE))
                        message("[DEBUG-CV] FFTrees fold ", i, " model created")
                        fold_pred <- fold_model$pred$test$class
                        message("[DEBUG-CV] FFTrees fold ", i, " predictions extracted")
                    }

                    # Calculate metrics
                    actual_fold <- test_fold[[self$options$target]]

                    # Handle both binary and multi-class scenarios
                    if (length(levels(actual_fold)) == 2 && !is.null(self$options$targetLevel)) {
                        # Binary classification
                        cm <- caret::confusionMatrix(fold_pred, actual_fold, positive = self$options$targetLevel)
                        cv_results$Accuracy[i] <- cm$overall["Accuracy"]
                        cv_results$Sensitivity[i] <- cm$byClass["Sensitivity"]
                        cv_results$Specificity[i] <- cm$byClass["Specificity"]
                    } else {
                        # Multi-class classification
                        cm <- caret::confusionMatrix(fold_pred, actual_fold)
                        cv_results$Accuracy[i] <- cm$overall["Accuracy"]

                        # For multi-class, use macro-averaged sensitivity and specificity
                        if ("byClass" %in% names(cm) && is.matrix(cm$byClass)) {
                            cv_results$Sensitivity[i] <- mean(cm$byClass[, "Sensitivity"], na.rm = TRUE)
                            cv_results$Specificity[i] <- mean(cm$byClass[, "Specificity"], na.rm = TRUE)
                        } else {
                            # Fallback: calculate overall precision/recall
                            cv_results$Sensitivity[i] <- NA  # Not meaningful for multi-class without class specification
                            cv_results$Specificity[i] <- NA
                        }
                    }
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
                <h3 style='color: #1976d2; margin-top: 0;'>Medical Decision Tree Model Summary</h3>

                <p style='margin-bottom: 15px;'><em>Your decision tree has been successfully built! Below are the key results and performance metrics.</em></p>

                <h4 style='color: #1976d2;'>Model Configuration:</h4>
                <table style='width: 100%; border-collapse: collapse; margin-bottom: 15px;'>
                <tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Algorithm:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>",
                ifelse(algorithm == "fftrees", "FFTrees (Fast-and-Frugal Trees)", "Enhanced CART (rpart)"), "</td></tr>
                <tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Training Samples:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_train, " (", round(n_train/(n_train+n_test)*100, 1), "%)</td></tr>
                <tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Test Samples:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_test, " (", round(n_test/(n_train+n_test)*100, 1), "%)</td></tr>
                <tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Predictor Variables:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_features, "</td></tr>
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

                summary_html <- paste0(summary_html, "</table>")

                # Add interpretation guide
                summary_html <- paste0(summary_html, "
                <h4 style='color: #1976d2; margin-top: 20px;'>📖 How to Interpret These Results:</h4>
                <ul style='line-height: 1.8;'>")

                if (!is.na(accuracy)) {
                    summary_html <- paste0(summary_html, "
                    <li><strong>Accuracy (", round(accuracy * 100, 1), "%):</strong> The model correctly classified ",
                    round(accuracy * 100, 1), "% of patients in the test set.</li>")
                }

                if (!is.na(sensitivity)) {
                    summary_html <- paste0(summary_html, "
                    <li><strong>Sensitivity (", round(sensitivity * 100, 1), "%):</strong> Of all actual '",
                    self$options$targetLevel, "' cases, the model correctly identified ",
                    round(sensitivity * 100, 1), "%.</li>")
                }

                if (!is.na(specificity)) {
                    summary_html <- paste0(summary_html, "
                    <li><strong>Specificity (", round(specificity * 100, 1), "%):</strong> Of all non-'",
                    self$options$targetLevel, "' cases, the model correctly identified ",
                    round(specificity * 100, 1), "%.</li>")
                }

                if (!is.null(auc_value)) {
                    auc_interpretation <- ifelse(auc_value >= 0.9, "excellent",
                                               ifelse(auc_value >= 0.8, "good",
                                                     ifelse(auc_value >= 0.7, "acceptable", "poor")))
                    summary_html <- paste0(summary_html, "
                    <li><strong>AUC (", round(auc_value, 3), "):</strong> The model's ability to discriminate between classes is ",
                    auc_interpretation, ".</li>")
                }

                summary_html <- paste0(summary_html, "
                </ul>

                <p style='background-color: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 15px;'>
                ⚠️ <strong>Clinical Note:</strong> These metrics are based on the test set. External validation on independent cohorts is recommended before clinical deployment.
                </p>
                </div>")

                return(summary_html)

            }, error = function(e) {
                return("<p>Error generating model summary.</p>")
            })
        },

        .generate_performance_table = function(results) {

            tryCatch({
                actual <- results$actual
                predicted <- results$predictions_class
                n_levels <- length(levels(actual))
                
                message(paste("[DEBUG] Performance table - Target has", n_levels, "levels"))

                if (n_levels == 2 && !is.null(self$options$targetLevel)) {
                    # Binary classification
                    cm <- caret::confusionMatrix(predicted, actual, positive = self$options$targetLevel)

                    # Use performance table (columns already defined in .r.yaml)
                    table <- self$results$performance_table

                    # Helper function to calculate CI using Wilson score method
                    wilson_ci <- function(x, n, conf.level = 0.95) {
                        if (n == 0) return(c(lower = NA, upper = NA))

                        p <- x / n
                        z <- qnorm((1 + conf.level) / 2)
                        denominator <- 1 + z^2 / n
                        centre <- (p + z^2 / (2 * n)) / denominator
                        margin <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denominator

                        c(lower = pmax(0, centre - margin), upper = pmin(1, centre + margin))
                    }

                    # Extract confusion matrix values
                    tp <- cm$table[self$options$targetLevel, self$options$targetLevel]
                    tn <- cm$table[setdiff(rownames(cm$table), self$options$targetLevel),
                                   setdiff(colnames(cm$table), self$options$targetLevel)]
                    fp <- cm$table[self$options$targetLevel, setdiff(colnames(cm$table), self$options$targetLevel)]
                    fn <- cm$table[setdiff(rownames(cm$table), self$options$targetLevel), self$options$targetLevel]

                    # Calculate CIs for each metric
                    sens_ci <- wilson_ci(tp, tp + fn)
                    spec_ci <- wilson_ci(tn, tn + fp)
                    ppv_ci <- wilson_ci(tp, tp + fp)
                    npv_ci <- wilson_ci(tn, tn + fn)

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
                        ci_lower = sens_ci["lower"],
                        ci_upper = sens_ci["upper"]
                    ))

                    table$addRow(rowKey = "specificity", values = list(
                        metric = "Specificity",
                        value = cm$byClass["Specificity"],
                        ci_lower = spec_ci["lower"],
                        ci_upper = spec_ci["upper"]
                    ))

                    table$addRow(rowKey = "ppv", values = list(
                        metric = "Positive Predictive Value",
                        value = cm$byClass["Pos Pred Value"],
                        ci_lower = ppv_ci["lower"],
                        ci_upper = ppv_ci["upper"]
                    ))

                    table$addRow(rowKey = "npv", values = list(
                        metric = "Negative Predictive Value",
                        value = cm$byClass["Neg Pred Value"],
                        ci_lower = npv_ci["lower"],
                        ci_upper = npv_ci["upper"]
                    ))
                } else if (n_levels > 2) {
                    # Multiclass classification
                    message("[DEBUG] Handling multiclass performance metrics")
                    cm <- caret::confusionMatrix(predicted, actual)
                    table <- self$results$performance_table
                    
                    # Calculate overall accuracy
                    table$addRow(rowKey = "accuracy", values = list(
                        metric = "Overall Accuracy",
                        value = cm$overall["Accuracy"],
                        ci_lower = cm$overall["AccuracyLower"],
                        ci_upper = cm$overall["AccuracyUpper"]
                    ))
                    
                    # Add macro-averaged metrics for multiclass
                    if (!is.null(cm$byClass) && is.matrix(cm$byClass)) {
                        # Calculate macro averages
                        macro_sens <- mean(cm$byClass[, "Sensitivity"], na.rm = TRUE)
                        macro_spec <- mean(cm$byClass[, "Specificity"], na.rm = TRUE)
                        macro_ppv <- mean(cm$byClass[, "Pos Pred Value"], na.rm = TRUE)
                        macro_f1 <- mean(cm$byClass[, "F1"], na.rm = TRUE)
                        
                        table$addRow(rowKey = "macro_sensitivity", values = list(
                            metric = "Macro-Avg Sensitivity",
                            value = macro_sens,
                            ci_lower = "NA",
                            ci_upper = "NA"
                        ))
                        
                        table$addRow(rowKey = "macro_specificity", values = list(
                            metric = "Macro-Avg Specificity", 
                            value = macro_spec,
                            ci_lower = "NA",
                            ci_upper = "NA"
                        ))
                        
                        table$addRow(rowKey = "macro_ppv", values = list(
                            metric = "Macro-Avg Precision",
                            value = macro_ppv,
                            ci_lower = "NA",
                            ci_upper = "NA"
                        ))
                        
                        table$addRow(rowKey = "macro_f1", values = list(
                            metric = "Macro-Avg F1-Score",
                            value = macro_f1,
                            ci_lower = "NA",
                            ci_upper = "NA"
                        ))
                    }
                    
                    # Add Kappa statistic for multiclass agreement
                    # Try to get Kappa CI if available from confusionMatrix
                    kappa_lower <- ifelse("KappaLower" %in% names(cm$overall), 
                                         cm$overall["KappaLower"], "NA")
                    kappa_upper <- ifelse("KappaUpper" %in% names(cm$overall), 
                                         cm$overall["KappaUpper"], "NA")
                    
                    table$addRow(rowKey = "kappa", values = list(
                        metric = "Cohen's Kappa",
                        value = cm$overall["Kappa"],
                        ci_lower = kappa_lower,
                        ci_upper = kappa_upper
                    ))
                } else {
                    message("[DEBUG] Unsupported number of target levels:", n_levels)
                }

                # Table is populated directly, no need to return

            }, error = function(e) {
                message("[ERROR] Performance table generation failed:", e$message)
                return(NULL)
            })
        },

        .generate_clinical_metrics_table = function(results) {

            tryCatch({
                actual <- results$actual
                predicted <- results$predictions_class
                n_levels <- length(levels(actual))
                
                message(paste("[DEBUG] Clinical metrics - Target has", n_levels, "levels"))

                if (n_levels == 2 && !is.null(self$options$targetLevel)) {
                    # Binary classification
                    pos_class <- self$options$targetLevel
                    cm <- caret::confusionMatrix(predicted, actual, positive = pos_class)

                    # Use clinical metrics table
                    table <- self$results$clinicalMetricsTable

                    # Extract confusion matrix values
                    tp <- cm$table[pos_class, pos_class]
                    tn <- cm$table[setdiff(rownames(cm$table), pos_class),
                                   setdiff(colnames(cm$table), pos_class)]
                    fp <- cm$table[pos_class, setdiff(colnames(cm$table), pos_class)]
                    fn <- cm$table[setdiff(rownames(cm$table), pos_class), pos_class]

                    # Calculate clinical metrics
                    sensitivity <- tp / (tp + fn)
                    specificity <- tn / (tn + fp)
                    prevalence <- (tp + fn) / (tp + tn + fp + fn)

                    # Calculate likelihood ratios
                    lr_pos <- sensitivity / (1 - specificity)
                    lr_neg <- (1 - sensitivity) / specificity

                    # Calculate Number Needed to Treat (NNT) approximation
                    # (using difference in event rates as proxy)
                    ppv <- tp / (tp + fp)
                    npv <- tn / (tn + fn)
                    control_event_rate <- 1 - npv  # Approximate
                    treatment_event_rate <- 1 - ppv  # Approximate
                    arr <- abs(control_event_rate - treatment_event_rate)  # Absolute risk reduction
                    nnt <- if (arr > 0) 1 / arr else NA

                    # Add clinical metrics
                    table$addRow(rowKey = "lr_pos", values = list(
                        metric = "Positive Likelihood Ratio",
                        value = lr_pos,
                        interpretation = paste0("A positive test increases odds by ", round(lr_pos, 1), "x")
                    ))

                    table$addRow(rowKey = "lr_neg", values = list(
                        metric = "Negative Likelihood Ratio",
                        value = lr_neg,
                        interpretation = paste0("A negative test decreases odds by ", round(1/lr_neg, 1), "x")
                    ))

                    table$addRow(rowKey = "prevalence", values = list(
                        metric = "Disease Prevalence",
                        value = prevalence,
                        interpretation = paste0(round(prevalence * 100, 1), "% of patients have the condition")
                    ))

                    if (!is.na(nnt) && nnt < 1000) {
                        table$addRow(rowKey = "nnt", values = list(
                            metric = "Number Needed to Treat (approx)",
                            value = nnt,
                            interpretation = paste0("Treat ", round(nnt), " patients to prevent 1 adverse outcome")
                        ))
                    }

                    # Diagnostic accuracy interpretation
                    if (sensitivity > 0.9 && specificity > 0.9) {
                        interpretation <- "Excellent diagnostic accuracy"
                    } else if (sensitivity > 0.8 && specificity > 0.8) {
                        interpretation <- "Good diagnostic accuracy"
                    } else if (sensitivity > 0.7 || specificity > 0.7) {
                        interpretation <- "Moderate diagnostic accuracy"
                    } else {
                        interpretation <- "Limited diagnostic accuracy"
                    }

                    table$addRow(rowKey = "diagnostic_quality", values = list(
                        metric = "Overall Diagnostic Quality",
                        value = (sensitivity + specificity) / 2,
                        interpretation = interpretation
                    ))
                } else if (n_levels > 2) {
                    # Multiclass clinical metrics
                    message("[DEBUG] Handling multiclass clinical metrics")
                    cm <- caret::confusionMatrix(predicted, actual)
                    table <- self$results$clinicalMetricsTable
                    
                    # Overall accuracy interpretation for multiclass
                    accuracy <- cm$overall["Accuracy"]
                    if (accuracy > 0.9) {
                        acc_interpretation <- "Excellent multiclass classification performance"
                    } else if (accuracy > 0.8) {
                        acc_interpretation <- "Good multiclass classification performance"
                    } else if (accuracy > 0.7) {
                        acc_interpretation <- "Moderate multiclass classification performance"
                    } else {
                        acc_interpretation <- "Limited multiclass classification performance"
                    }
                    
                    table$addRow(rowKey = "multiclass_accuracy", values = list(
                        metric = "Overall Classification Accuracy",
                        value = accuracy,
                        interpretation = acc_interpretation
                    ))
                    
                    # Kappa coefficient interpretation
                    kappa <- cm$overall["Kappa"]
                    if (kappa > 0.8) {
                        kappa_interpretation <- "Almost perfect agreement beyond chance"
                    } else if (kappa > 0.6) {
                        kappa_interpretation <- "Substantial agreement beyond chance"
                    } else if (kappa > 0.4) {
                        kappa_interpretation <- "Moderate agreement beyond chance"
                    } else if (kappa > 0.2) {
                        kappa_interpretation <- "Fair agreement beyond chance"
                    } else {
                        kappa_interpretation <- "Slight agreement beyond chance"
                    }
                    
                    table$addRow(rowKey = "kappa_agreement", values = list(
                        metric = "Cohen's Kappa",
                        value = kappa,
                        interpretation = kappa_interpretation
                    ))
                    
                    # Add class-specific metrics for target level if specified
                    if (!is.null(self$options$targetLevel) && 
                        !is.null(cm$byClass) && is.matrix(cm$byClass)) {
                        
                        target_class <- paste("Class:", self$options$targetLevel)
                        if (target_class %in% rownames(cm$byClass)) {
                            target_sens <- cm$byClass[target_class, "Sensitivity"]
                            target_spec <- cm$byClass[target_class, "Specificity"]
                            target_ppv <- cm$byClass[target_class, "Pos Pred Value"]
                            
                            table$addRow(rowKey = "target_sensitivity", values = list(
                                metric = paste("Sensitivity for", self$options$targetLevel),
                                value = target_sens,
                                interpretation = paste0(round(target_sens * 100, 1), "% of true ", 
                                                      self$options$targetLevel, " cases correctly identified")
                            ))
                            
                            table$addRow(rowKey = "target_ppv", values = list(
                                metric = paste("Precision for", self$options$targetLevel),
                                value = target_ppv,
                                interpretation = paste0("When predicting ", self$options$targetLevel, 
                                                      ", correct ", round(target_ppv * 100, 1), "% of the time")
                            ))
                        }
                    }
                    
                    # Multiclass interpretation guidance
                    table$addRow(rowKey = "multiclass_guidance", values = list(
                        metric = "Clinical Application",
                        value = n_levels,
                        interpretation = paste0("This is a ", n_levels, "-class classification problem. ",
                                              "Consider class-specific performance for clinical decisions.")
                    ))
                } else {
                    message("[DEBUG] Unsupported number of target levels for clinical metrics:", n_levels)
                }

            }, error = function(e) {
                message("[ERROR] Clinical metrics generation failed:", e$message)
                return(NULL)
            })
        },

        .generate_feature_importance_table = function(results) {

            tryCatch({
                importance_data <- results$importance

                if (!is.null(importance_data) && nrow(importance_data) > 0) {
                    table <- self$results$featureImportanceTable

                    # Add each feature with its importance
                    for (i in 1:nrow(importance_data)) {
                        table$addRow(rowKey = paste0("feature_", i), values = list(
                            feature = importance_data$Feature[i],
                            importance = importance_data$Importance[i],
                            rank = i
                        ))
                    }
                }

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

            fs_results <- private$.feature_selection_results

            if (is.null(fs_results)) {
                return("<div style='background-color: #fff3cd; padding: 20px; border-radius: 8px;'>
                <h4 style='color: #856404;'>Feature Selection Results</h4>
                <p>Feature selection was not performed or results unavailable.</p>
                </div>")
            }

            html <- "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>
            <h4 style='color: #2e7d32;'>🔬 Feature Selection Results</h4>"

            html <- paste0(html, "<p><strong>Method:</strong> ", fs_results$method, "</p>")
            html <- paste0(html, "<p><strong>Original Features:</strong> ", length(fs_results$original_features), "</p>")
            html <- paste0(html, "<p><strong>Selected Features:</strong> ", length(fs_results$selected_features), "</p>")

            if (length(fs_results$selected_features) < length(fs_results$original_features)) {
                reduction <- round((1 - length(fs_results$selected_features)/length(fs_results$original_features)) * 100, 1)
                html <- paste0(html, "<p><strong>Dimensionality Reduction:</strong> ", reduction, "%</p>")
            }

            # Handle Boruta-specific results
            if (fs_results$method == "Boruta Algorithm") {
                # Show Boruta decision breakdown
                if (!is.null(fs_results$boruta_stats)) {
                    stats <- fs_results$boruta_stats
                    html <- paste0(html, "<h5>Boruta Analysis Summary:</h5>")
                    html <- paste0(html, "<p><span style='color: #4CAF50; font-weight: bold;'>✓ Confirmed:</span> ",
                                  stats$confirmed_count, " features</p>")
                    html <- paste0(html, "<p><span style='color: #FF9800; font-weight: bold;'>? Tentative:</span> ",
                                  stats$tentative_count, " features</p>")
                    html <- paste0(html, "<p><span style='color: #f44336; font-weight: bold;'>✗ Rejected:</span> ",
                                  stats$rejected_count, " features</p>")
                }

                # Show confirmed features
                if (!is.null(fs_results$confirmed_features) && length(fs_results$confirmed_features) > 0) {
                    html <- paste0(html, "<h5 style='color: #4CAF50;'>Confirmed Features:</h5><ul>")
                    for (feature in fs_results$confirmed_features) {
                        html <- paste0(html, "<li><strong>", feature, "</strong> ✓</li>")
                    }
                    html <- paste0(html, "</ul>")
                }

                # Show tentative features
                if (!is.null(fs_results$tentative_features) && length(fs_results$tentative_features) > 0) {
                    html <- paste0(html, "<h5 style='color: #FF9800;'>Tentative Features:</h5><ul>")
                    for (feature in fs_results$tentative_features) {
                        html <- paste0(html, "<li>", feature, " ?</li>")
                    }
                    html <- paste0(html, "</ul>")
                }

                # Show rejected features (first 5 if many)
                if (!is.null(fs_results$rejected_features) && length(fs_results$rejected_features) > 0) {
                    rejected_to_show <- head(fs_results$rejected_features, 5)
                    html <- paste0(html, "<h5 style='color: #f44336;'>Rejected Features (showing first 5):</h5><ul>")
                    for (feature in rejected_to_show) {
                        html <- paste0(html, "<li>", feature, " ✗</li>")
                    }
                    if (length(fs_results$rejected_features) > 5) {
                        html <- paste0(html, "<li><em>... and ", length(fs_results$rejected_features) - 5, " more</em></li>")
                    }
                    html <- paste0(html, "</ul>")
                }
            } else {
                # Standard feature list for other methods
                html <- paste0(html, "<h5>Selected Features:</h5><ul>")
                for (feature in fs_results$selected_features) {
                    html <- paste0(html, "<li>", feature, "</li>")
                }
                html <- paste0(html, "</ul>")
            }

            if (!is.null(fs_results$importance_scores) && length(fs_results$importance_scores) > 0) {
                html <- paste0(html, "<h5>Top Feature Importance Scores:</h5><ul>")
                top_features <- head(fs_results$importance_scores, 5)
                for (i in 1:length(top_features)) {
                    feature_name <- names(top_features)[i]
                    score <- round(top_features[i], 3)
                    html <- paste0(html, "<li><strong>", feature_name, ":</strong> ", score, "</li>")
                }
                html <- paste0(html, "</ul>")
            }

            html <- paste0(html, "</div>")
            return(html)
        },

        .generate_clinical_interpretation = function() {

            context <- self$options$clinical_context
            algorithm <- self$options$algorithm
            target_var <- self$options$target
            target_level <- self$options$targetLevel

            # Get model performance if available
            sensitivity <- NA
            specificity <- NA
            if (!is.null(private$.model_results)) {
                actual <- private$.model_results$actual
                predicted <- private$.model_results$predictions_class
                if (length(levels(actual)) == 2 && !is.null(target_level)) {
                    cm <- caret::confusionMatrix(predicted, actual, positive = target_level)
                    sensitivity <- round(cm$byClass["Sensitivity"] * 100, 1)
                    specificity <- round(cm$byClass["Specificity"] * 100, 1)
                }
            }

            interpretation_html <- paste0("
            <div style='background-color: #f3e5f5; padding: 20px; border-radius: 8px;'>
            <h3 style='color: #7b1fa2; margin-top: 0;'>🏥 Clinical Decision Support Interpretation</h3>

            <p style='margin-bottom: 15px;'><em>This guide helps you understand and apply your decision tree results in clinical practice.</em></p>

            <h4 style='color: #7b1fa2;'>Analysis Context:</h4>
            <table style='width: 100%; margin-bottom: 15px;'>
            <tr><td style='padding: 5px;'><strong>Algorithm:</strong></td><td>", ifelse(algorithm == "fftrees", "FFTrees (Fast-and-Frugal Trees)", "Enhanced CART"), "</td></tr>
            <tr><td style='padding: 5px;'><strong>Clinical Setting:</strong></td><td>", tools::toTitleCase(context), "</td></tr>
            <tr><td style='padding: 5px;'><strong>Target Outcome:</strong></td><td>", target_var, " = '", target_level, "'</td></tr>
            </table>

            <h4 style='color: #7b1fa2;'>Clinical Application Guidelines:</h4>")

            if (context == "diagnosis") {
                interpretation_html <- paste0(interpretation_html,
                    "<ul style='line-height: 1.8;'>
                    <li><strong>Primary Goal:</strong> Accurate disease classification to support clinical diagnosis</li>
                    <li><strong>Key Metrics:</strong> Balance between sensitivity (",
                    ifelse(is.na(sensitivity), "not available", paste0(sensitivity, "%")),
                    ") and specificity (",
                    ifelse(is.na(specificity), "not available", paste0(specificity, "%")), ")</li>
                    <li><strong>Clinical Application:</strong> Use as a diagnostic aid alongside clinical judgment</li>
                    <li><strong>Decision Threshold:</strong> May need adjustment based on local disease prevalence</li>
                    <li><strong>Quality Check:</strong> Verify tree decisions align with clinical knowledge</li>
                    </ul>")
            } else if (context == "screening") {
                interpretation_html <- paste0(interpretation_html,
                    "<ul style='line-height: 1.8;'>
                    <li><strong>Primary Goal:</strong> Maximize sensitivity to catch all potential cases</li>
                    <li><strong>Current Performance:</strong> Sensitivity = ",
                    ifelse(is.na(sensitivity), "not available", paste0(sensitivity, "%")),
                    ifelse(!is.na(sensitivity) && sensitivity < 90, " ⚠️ (target: >90% for screening)", ""), "</li>
                    <li><strong>Clinical Application:</strong> First-line screening with confirmatory testing for positives</li>
                    <li><strong>False Positive Impact:</strong> Consider follow-up testing burden and patient anxiety</li>
                    <li><strong>Quality Check:</strong> Ensure high negative predictive value for reassurance</li>
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

            # Algorithm-specific guidance
            if (algorithm == "fftrees") {
                interpretation_html <- paste0(interpretation_html,
                    "<h4 style='color: #7b1fa2;'>FFTrees Algorithm Advantages:</h4>
                    <ul>
                    <li><strong>Simplicity:</strong> Easy to understand and implement in clinical practice</li>
                    <li><strong>Fast Decisions:</strong> Optimized for quick, sequential decision making</li>
                    <li><strong>Interpretability:</strong> Clear decision rules that clinicians can follow</li>
                    <li><strong>Robustness:</strong> Performs well with limited data and missing values</li>
                    </ul>")
            } else {
                interpretation_html <- paste0(interpretation_html,
                    "<h4 style='color: #7b1fa2;'>Enhanced CART Algorithm Advantages:</h4>
                    <ul>
                    <li><strong>Comprehensive:</strong> Considers all variables simultaneously</li>
                    <li><strong>Flexible:</strong> Handles complex interactions and patterns</li>
                    <li><strong>Validated:</strong> Extensive cross-validation and pruning options</li>
                    <li><strong>Feature Importance:</strong> Provides detailed variable importance rankings</li>
                    </ul>")
            }

            # Add performance-based recommendations
            interpretation_html <- paste0(interpretation_html, "
                <h4 style='color: #7b1fa2;'>Performance-Based Recommendations:</h4>")

            if (!is.na(sensitivity) && !is.na(specificity)) {
                interpretation_html <- paste0(interpretation_html, "<ul style='line-height: 1.8;'>")

                # Sensitivity recommendations
                if (sensitivity >= 90) {
                    interpretation_html <- paste0(interpretation_html,
                        "<li>✅ <strong>High Sensitivity (", sensitivity, "%):</strong> Excellent for ruling out disease when test is negative</li>")
                } else if (sensitivity >= 80) {
                    interpretation_html <- paste0(interpretation_html,
                        "<li>⚠️ <strong>Moderate Sensitivity (", sensitivity, "%):</strong> Good performance, but some cases may be missed</li>")
                } else {
                    interpretation_html <- paste0(interpretation_html,
                        "<li>❌ <strong>Low Sensitivity (", sensitivity, "%):</strong> Risk of missing true positive cases - use with caution</li>")
                }

                # Specificity recommendations
                if (specificity >= 90) {
                    interpretation_html <- paste0(interpretation_html,
                        "<li>✅ <strong>High Specificity (", specificity, "%):</strong> Excellent for confirming disease when test is positive</li>")
                } else if (specificity >= 80) {
                    interpretation_html <- paste0(interpretation_html,
                        "<li>⚠️ <strong>Moderate Specificity (", specificity, "%):</strong> Good performance, but some false positives expected</li>")
                } else {
                    interpretation_html <- paste0(interpretation_html,
                        "<li>❌ <strong>Low Specificity (", specificity, "%):</strong> High false positive rate - consider confirmatory testing</li>")
                }

                interpretation_html <- paste0(interpretation_html, "</ul>")
            }

            interpretation_html <- paste0(interpretation_html,
                "<h4 style='color: #7b1fa2;'>Clinical Implementation Checklist:</h4>
                <ul style='line-height: 1.8;'>
                <li>☐ <strong>External Validation:</strong> Test on independent patient cohorts from different settings</li>
                <li>☐ <strong>Clinical Review:</strong> Have domain experts review the decision rules for clinical validity</li>
                <li>☐ <strong>Pilot Testing:</strong> Run parallel with current practice before full implementation</li>
                <li>☐ <strong>Training:</strong> Ensure clinical staff understand how to use and interpret the tool</li>
                <li>☐ <strong>Monitoring:</strong> Set up continuous performance monitoring and feedback loops</li>
                <li>☐ <strong>Documentation:</strong> Create clear guidelines for when to use/not use the tool</li>
                <li>☐ <strong>Regulatory:</strong> Consider regulatory requirements (FDA, CE mark) if applicable</li>
                </ul>

                <p style='background-color: #e8f5e9; padding: 10px; border-radius: 5px; margin-top: 15px;'>
                💡 <strong>Remember:</strong> This decision tree is a clinical support tool, not a replacement for clinical judgment.
                Always consider the full clinical context when making patient care decisions.
                </p>
                </div>")

            return(interpretation_html)
        },

        .generate_bootstrap_results = function(results) {

            return("<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>
            <h4 style='color: #2e7d32;'>Bootstrap Confidence Intervals</h4>
            <p>Bootstrap confidence intervals provide uncertainty quantification for performance metrics.</p>
            <p>Feature available in the full implementation with user-specified bootstrap samples.</p>
            </div>")
        },

        .generate_model_comparison = function(data) {

            tryCatch({
                target_var <- self$options$target
                target_level <- self$options$targetLevel

                # Test multiple algorithms - comprehensive comparison
                algorithms <- c("rpart", "fftrees", "randomforest", "c50", "ctree")

                # Add XGBoost if available
                if (requireNamespace("xgboost", quietly = TRUE)) {
                    algorithms <- c(algorithms, "xgboost")
                }

                comparison_results <- list()

                # Train each algorithm and collect results
                algo_names <- list(
                    "rpart" = "Enhanced CART",
                    "fftrees" = "FFTrees",
                    "randomforest" = "Random Forest",
                    "c50" = "C5.0 Trees",
                    "ctree" = "Conditional Inference Trees",
                    "xgboost" = "XGBoost"
                )

                for (i in seq_along(algorithms)) {
                    private$.checkpoint()  # Add checkpoint before expensive algorithm comparison
                    
                    algo <- algorithms[i]
                    algo_display <- algo_names[[algo]] %||% algo

                    # Update progress
                    private$.update_comparison_progress(i, length(algorithms), algo_display)

                    temp_results <- private$.train_single_algorithm(data, algo)
                    if (!is.null(temp_results)) {
                        comparison_results[[algo]] <- temp_results
                    }
                }

                if (length(comparison_results) >= 2) {
                    table <- self$results$modelComparison
                    metric_scores <- list()

                    for (algo in names(comparison_results)) {
                        result <- comparison_results[[algo]]

                        # Calculate metrics
                        if (length(levels(result$actual)) == 2 && !is.null(target_level)) {

                            # Use caret for comprehensive metrics
                            cm <- caret::confusionMatrix(result$predictions_class, result$actual, positive = target_level)

                            # Calculate AUC if possible
                            auc_value <- NA
                            if (!is.null(result$predictions)) {
                                tryCatch({
                                    roc_obj <- pROC::roc(result$actual, result$predictions, quiet = TRUE)
                                    auc_value <- as.numeric(pROC::auc(roc_obj))
                                }, error = function(e) {
                                    auc_value <<- NA
                                })
                            }

                            # Extract key metrics
                            accuracy <- as.numeric(cm$overall["Accuracy"])
                            sensitivity <- as.numeric(cm$byClass["Sensitivity"])
                            specificity <- as.numeric(cm$byClass["Specificity"])
                            balanced_accuracy <- (sensitivity + specificity) / 2
                            f1_score <- as.numeric(cm$byClass["F1"])

                            # Store for ranking
                            metric_scores[[algo]] <- list(
                                bacc = balanced_accuracy,
                                auc = auc_value,
                                sens = sensitivity,
                                spec = specificity,
                                f1 = f1_score
                            )

                            # Algorithm display names
                            algo_names <- list(
                                "rpart" = "Enhanced CART",
                                "fftrees" = "FFTrees",
                                "randomforest" = "Random Forest",
                                "c50" = "C5.0 Trees",
                                "ctree" = "Conditional Inference Trees",
                                "xgboost" = "XGBoost"
                            )

                            table$addRow(rowKey = algo, values = list(
                                algorithm = algo_names[[algo]] %||% algo,
                                accuracy = accuracy,
                                sensitivity = sensitivity,
                                specificity = specificity,
                                auc = auc_value,
                                rank = 0  # Will be updated after all algorithms
                            ))
                        }
                    }

                    # Rank algorithms based on selected metric
                    metric <- self$options$modelComparisonMetric

                    # Calculate ranks
                    valid_algos <- names(metric_scores)
                    if (length(valid_algos) > 0) {

                        # Extract metric values for ranking
                        metric_values <- sapply(valid_algos, function(algo) {
                            score <- metric_scores[[algo]][[metric]]
                            if (is.na(score) || is.null(score)) return(-1)  # Handle missing values
                            return(score)
                        })

                        # Rank (higher is better for all our metrics)
                        ranks <- rank(-metric_values, ties.method = "min", na.last = TRUE)

                        # Update table with ranks
                        for (i in seq_along(valid_algos)) {
                            algo <- valid_algos[i]
                            table$setRow(rowNo = i, list(rank = ranks[i]))
                        }
                    }
                }

            }, error = function(e) {
                # Log error for debugging
                message("Model comparison error: ", e$message)
                return(NULL)
            })
        },

        .train_single_algorithm = function(data, algorithm) {

            tryCatch({
                target_var <- self$options$target
                target_level <- self$options$targetLevel

                # Prepare formula
                predictors <- setdiff(names(data), target_var)
                formula_str <- paste(target_var, "~", paste(predictors, collapse = " + "))
                model_formula <- as.formula(formula_str)

                # Split data with stratification (optimal 75:25 ratio)
                set.seed(self$options$seed_value %||% 123)
                train_idx <- caret::createDataPartition(data[[target_var]], p = 0.75, list = FALSE)
                train_data <- data[train_idx, ]
                test_data <- data[-train_idx, ]

                predictions_prob <- NULL
                predictions_class <- NULL
                model <- NULL

                # Train based on algorithm
                if (algorithm == "fftrees") {
                    if (requireNamespace("FFTrees", quietly = TRUE)) {
                        model <- FFTrees::FFTrees(
                            formula = model_formula,
                            data = train_data,
                            data.test = test_data,
                            quiet = list(ini = TRUE, fin = FALSE, mis = FALSE, set = TRUE)
                        )

                        predictions_prob <- model$pred$test$prob
                        predictions_class <- model$pred$test$class
                    }

                } else if (algorithm == "rpart") {
                    model <- rpart::rpart(
                        formula = model_formula,
                        data = train_data,
                        method = "class",
                        control = rpart::rpart.control(cp = 0.01)
                    )
                    predictions_prob <- predict(model, test_data, type = "prob")[, 2]
                    predictions_class <- predict(model, test_data, type = "class")

                } else if (algorithm == "randomforest") {
                    if (requireNamespace("randomForest", quietly = TRUE)) {
                        model <- randomForest::randomForest(
                            formula = model_formula,
                            data = train_data,
                            ntree = 500,
                            importance = TRUE
                        )
                        predictions_prob <- predict(model, test_data, type = "prob")[, 2]
                        predictions_class <- predict(model, test_data, type = "class")
                    }

                } else if (algorithm == "c50") {
                    if (requireNamespace("C50", quietly = TRUE)) {
                        model <- C50::C5.0(
                            formula = model_formula,
                            data = train_data,
                            trials = 1
                        )
                        predictions_prob <- predict(model, test_data, type = "prob")[, 2]
                        predictions_class <- predict(model, test_data, type = "class")
                    }

                } else if (algorithm == "ctree") {
                    if (requireNamespace("party", quietly = TRUE)) {
                        model <- party::ctree(
                            formula = model_formula,
                            data = train_data,
                            controls = party::ctree_control(mincriterion = 0.95)
                        )

                        # For ctree, predictions need different handling
                        pred_response <- predict(model, test_data, type = "response")
                        pred_prob <- predict(model, test_data, type = "prob")

                        predictions_class <- pred_response
                        if (is.list(pred_prob)) {
                            predictions_prob <- sapply(pred_prob, function(x) x[target_level])
                        }
                    }

                } else if (algorithm == "xgboost") {
                    if (requireNamespace("xgboost", quietly = TRUE)) {

                        # Prepare data for XGBoost
                        # Convert categorical variables to numeric
                        train_prepared <- private$.prepare_data_for_xgboost(train_data, target_var)
                        test_prepared <- private$.prepare_data_for_xgboost(test_data, target_var)

                        # Create matrices
                        train_matrix <- xgboost::xgb.DMatrix(
                            data = train_prepared$X,
                            label = train_prepared$y
                        )
                        test_matrix <- xgboost::xgb.DMatrix(
                            data = test_prepared$X,
                            label = test_prepared$y
                        )

                        model <- xgboost::xgb.train(
                            data = train_matrix,
                            objective = "binary:logistic",
                            eval_metric = "auc",
                            nrounds = 100,
                            eta = 0.3,
                            max_depth = 6,
                            verbose = 0
                        )

                        predictions_prob <- predict(model, test_matrix)
                        predictions_class <- factor(
                            ifelse(predictions_prob > 0.5, target_level,
                                   levels(test_data[[target_var]])[levels(test_data[[target_var]]) != target_level]),
                            levels = levels(test_data[[target_var]])
                        )
                    }
                }

                # Only return if we have valid predictions
                if (is.null(predictions_class) || is.null(model)) {
                    return(NULL)
                }

                return(list(
                    model = model,
                    predictions = predictions_prob,
                    predictions_class = predictions_class,
                    actual = test_data[[target_var]],
                    algorithm = algorithm
                ))

            }, error = function(e) {
                message("Error training ", algorithm, ": ", e$message)
                return(NULL)
            })
        },

        # Prepare data for XGBoost (convert factors to numeric)
        .prepare_data_for_xgboost = function(data, target_var) {
            # Separate target and predictors
            y <- data[[target_var]]
            X <- data[, !names(data) %in% target_var, drop = FALSE]

            # Convert target to binary numeric (0/1)
            target_level <- self$options$targetLevel
            y_numeric <- as.numeric(y == target_level)

            # Convert categorical predictors to numeric using model.matrix
            for (col in names(X)) {
                if (is.factor(X[[col]]) || is.character(X[[col]])) {
                    # Create dummy variables for factors
                    formula_str <- paste("~", col, "- 1")
                    dummy_matrix <- model.matrix(as.formula(formula_str), data = X)

                    # Remove original column and add dummy columns
                    X <- X[, !names(X) %in% col, drop = FALSE]
                    X <- cbind(X, dummy_matrix)
                }
            }

            # Ensure all columns are numeric
            X <- data.frame(lapply(X, as.numeric))

            return(list(
                X = as.matrix(X),
                y = y_numeric
            ))
        },

        .generate_export_info = function(results) {

            algorithm <- results$algorithm

            return(paste0("<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>
            <h4 style='color: #2e7d32;'>📁 Model Export Information</h4>
            <p><strong>Algorithm:</strong> ", ifelse(algorithm == "fftrees", "FFTrees", "Enhanced CART"), "</p>
            <p><strong>Export Format:</strong> R model object (RDS file)</p>
            <p><strong>Deployment:</strong> Compatible with clinical decision support systems</p>
            <p><strong>Requirements:</strong> R environment with required packages (",
            ifelse(algorithm == "fftrees", "FFTrees, rpart", "rpart"), ", caret, pROC)</p>
            <p><strong>Usage:</strong> <code>model <- readRDS('exported_model.rds')</code></p>
            <p><strong>Prediction:</strong> <code>predict(model, new_data)</code></p>
            </div>"))
        },

        .generate_export_predictions_info = function(results) {

            return(paste0("<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px;'>
            <h4 style='color: #1976d2;'>📊 Prediction Export Information</h4>
            <p><strong>Available Exports:</strong></p>
            <ul>
            <li><strong>Predicted Classes:</strong> Final classification decisions</li>
            <li><strong>Predicted Probabilities:</strong> Probability estimates for each class</li>
            <li><strong>Confidence Scores:</strong> Model confidence in predictions</li>
            <li><strong>Feature Contributions:</strong> Which variables influenced each prediction</li>
            </ul>
            <p><strong>Export Format:</strong> CSV file with original data plus predictions</p>
            <p><strong>Clinical Use:</strong> Decision support, further analysis, reporting</p>
            </div>"))
        },

        .generate_performance_explanation = function() {
            target_level <- self$options$targetLevel
            context <- self$options$clinical_context

            explanation_html <- paste0("<div style='background-color: #e8f4fd; padding: 20px; border-radius: 8px; margin-top: 10px;'>
                <h4 style='color: #0066cc; margin-top: 0;'>📊 Understanding Your Performance Metrics</h4>

                <p style='margin-bottom: 15px;'>These metrics evaluate how well your decision tree predicts '", target_level, "' outcomes:</p>

                <h5 style='color: #0066cc;'>Core Metrics Explained:</h5>
                <ul style='line-height: 1.8;'>
                    <li><strong>Accuracy:</strong> Overall correct classification rate
                        <br><em style='color: #666; font-size: 0.9em;'>→ What percentage of all predictions were correct?</em></li>

                    <li><strong>Sensitivity (True Positive Rate):</strong> Ability to detect '", target_level, "' cases
                        <br><em style='color: #666; font-size: 0.9em;'>→ Of all actual '", target_level, "' patients, how many did we identify?</em></li>

                    <li><strong>Specificity (True Negative Rate):</strong> Ability to correctly identify non-'", target_level, "' cases
                        <br><em style='color: #666; font-size: 0.9em;'>→ Of all non-'", target_level, "' patients, how many did we correctly rule out?</em></li>

                    <li><strong>PPV (Positive Predictive Value):</strong> Reliability of positive predictions
                        <br><em style='color: #666; font-size: 0.9em;'>→ When the tree predicts '", target_level, "', how often is it correct?</em></li>

                    <li><strong>NPV (Negative Predictive Value):</strong> Reliability of negative predictions
                        <br><em style='color: #666; font-size: 0.9em;'>→ When the tree predicts non-'", target_level, "', how often is it correct?</em></li>
                </ul>

                <h5 style='color: #0066cc; margin-top: 15px;'>Clinical Context: ", tools::toTitleCase(context), "</h5>")

            if (context == "screening") {
                explanation_html <- paste0(explanation_html, "
                <p style='background-color: #fff3cd; padding: 10px; border-radius: 5px;'>
                ⚠️ <strong>For screening:</strong> Prioritize high sensitivity (>90%) to avoid missing cases.
                Lower specificity is acceptable if confirmatory tests are available.
                </p>")
            } else if (context == "diagnosis") {
                explanation_html <- paste0(explanation_html, "
                <p style='background-color: #fff3cd; padding: 10px; border-radius: 5px;'>
                ⚠️ <strong>For diagnosis:</strong> Balance sensitivity and specificity based on clinical consequences.
                Consider the impact of false positives vs false negatives.
                </p>")
            }

            explanation_html <- paste0(explanation_html, "
                <p style='margin-top: 15px;'>
                <strong>95% Confidence Intervals:</strong> Show the range where the true value likely falls.
                Wider intervals indicate more uncertainty (often due to smaller sample sizes).
                </p>
            </div>")

            return(explanation_html)
        },

        .generate_tree_explanation = function() {
            algorithm <- self$options$algorithm

            if (algorithm == "fftrees") {
                return("<div style='background-color: #f0f8ff; padding: 15px; border-radius: 8px; margin-top: 10px;'>
                    <h4 style='color: #2c5aa0; margin-top: 0;'>Reading the FFTrees Decision Tree</h4>
                    <ul style='line-height: 1.8;'>
                        <li><strong>Sequential Decisions:</strong> Tree makes decisions one cue at a time, from top to bottom</li>
                        <li><strong>Exit Rules:</strong> Each level can make a final decision (exit) or continue to next level</li>
                        <li><strong>Cue Information:</strong> Shows the variable, direction, and threshold for each decision</li>
                        <li><strong>Performance Stats:</strong> Displays hit rates, false alarm rates, and accuracy for each exit</li>
                        <li><strong>Optimized Path:</strong> Tree structure optimized for the selected goal (accuracy, sensitivity, etc.)</li>
                    </ul>
                    <p style='margin-top: 10px;'><em>Advantage:</em> Simple, fast decisions that can be easily implemented in clinical practice.</p>
                </div>")
            } else {
                return("<div style='background-color: #f0f8ff; padding: 15px; border-radius: 8px; margin-top: 10px;'>
                    <h4 style='color: #2c5aa0; margin-top: 0;'>Reading the CART Decision Tree</h4>
                    <ul style='line-height: 1.8;'>
                        <li><strong>Root Node (Top):</strong> Starting point with all samples</li>
                        <li><strong>Internal Nodes:</strong> Decision points showing split conditions</li>
                        <li><strong>Branches:</strong> Yes/No paths based on the condition</li>
                        <li><strong>Leaf Nodes (Bottom):</strong> Final predictions with class probabilities</li>
                        <li><strong>Node Information:</strong> Shows condition, number of samples, and dominant class</li>
                    </ul>
                    <p style='margin-top: 10px;'><em>Tip:</em> Follow the path from root to leaf for any sample to understand its prediction.</p>
                </div>")
            }
        },

        .generate_importance_explanation = function() {
            algorithm <- self$options$algorithm
            method <- self$options$importance_method

            return(paste0("<div style='background-color: #fff9e6; padding: 15px; border-radius: 8px; margin-top: 10px;'>
                <h4 style='color: #cc8800; margin-top: 0;'>📈 Understanding Feature Importance</h4>
                <ul style='line-height: 1.8;'>
                    <li><strong>Algorithm:</strong> ", ifelse(algorithm == "fftrees", "FFTrees uses cue order in tree (first=most important)", "CART uses variable importance from splits"), "</li>
                    <li><strong>Importance Score:</strong> Higher values indicate greater influence on predictions</li>
                    <li><strong>Relative Scale:</strong> Scores show relative importance between variables</li>
                </ul>
                <p style='margin-top: 10px;'><em>Clinical Use:</em> Identifies key factors driving predictions, useful for:
                <br>• Feature selection for simpler models
                <br>• Understanding disease mechanisms
                <br>• Focusing data collection efforts</p>
            </div>"))
        },

        .generate_roc_explanation = function() {
            return("<div style='background-color: #f5f5ff; padding: 15px; border-radius: 8px; margin-top: 10px;'>
                <h4 style='color: #4b0082; margin-top: 0;'>📉 Understanding the ROC Curve</h4>
                <ul style='line-height: 1.8;'>
                    <li><strong>ROC Curve:</strong> Shows trade-off between sensitivity and specificity at all thresholds</li>
                    <li><strong>Diagonal Line:</strong> Performance of random guessing (AUC = 0.5)</li>
                    <li><strong>Perfect Classifier:</strong> Would reach top-left corner (100% sensitivity, 100% specificity)</li>
                    <li><strong>AUC (Area Under Curve):</strong> Single number summary:
                        <br>• 0.9-1.0: Excellent
                        <br>• 0.8-0.9: Good
                        <br>• 0.7-0.8: Fair
                        <br>• 0.6-0.7: Poor
                        <br>• 0.5-0.6: Fail</li>
                </ul>
                <p style='margin-top: 10px;'><em>Clinical Decision:</em> Choose threshold based on cost of false positives vs. false negatives.</p>
            </div>")
        },

        .generate_confusion_matrix_explanation = function() {
            positive_class <- self$options$targetLevel

            return(paste0("<div style='background-color: #e6ffe6; padding: 15px; border-radius: 8px; margin-top: 10px;'>
                <h4 style='color: #006600; margin-top: 0;'>🎯 Reading the Confusion Matrix</h4>
                <p>The confusion matrix shows how predictions compare to actual outcomes:</p>
                <ul style='line-height: 1.8;'>
                    <li><strong>True Positives (TP):</strong> Correctly predicted as '", positive_class, "'</li>
                    <li><strong>True Negatives (TN):</strong> Correctly predicted as not '", positive_class, "'</li>
                    <li><strong>False Positives (FP):</strong> Incorrectly predicted as '", positive_class, "' (Type I error)</li>
                    <li><strong>False Negatives (FN):</strong> Incorrectly predicted as not '", positive_class, "' (Type II error)</li>
                </ul>
                <p style='margin-top: 10px;'><em>Key Insight:</em>
                <br>• High numbers on diagonal = good performance
                <br>• High numbers off diagonal = errors to investigate</p>
            </div>"))
        },

        .generate_analysis_introduction = function() {
            # Generate comprehensive analysis introduction
            target_var <- self$options$target
            target_level <- self$options$targetLevel
            algorithm <- self$options$algorithm
            n_vars <- length(c(self$options$vars, self$options$facs))
            n_samples <- nrow(self$data)

            algo_name <- ifelse(algorithm == "fftrees",
                              "FFTrees (Fast-and-Frugal Trees)",
                              "Enhanced CART (Classification and Regression Trees)")

            intro_html <- paste0("
            <div style='background-color: #f5f5f5; padding: 20px; border-radius: 8px; border-left: 4px solid #2196F3; margin-bottom: 20px;'>
            <h3 style='color: #1976d2; margin-top: 0;'>🏥 Medical Decision Tree Analysis Overview</h3>

            <h4 style='color: #424242;'>Analysis Configuration:</h4>
            <table style='width: 100%; margin-bottom: 15px;'>
            <tr>
                <td style='padding: 5px;'><strong>📊 Target Outcome:</strong></td>
                <td style='padding: 5px;'>", target_var, "</td>
            </tr>
            <tr>
                <td style='padding: 5px;'><strong>🎯 Positive Class:</strong></td>
                <td style='padding: 5px;'>", target_level, "</td>
            </tr>
            <tr>
                <td style='padding: 5px;'><strong>Algorithm:</strong></td>
                <td style='padding: 5px;'>", algo_name, "</td>
            </tr>
            <tr>
                <td style='padding: 5px;'><strong>📈 Predictor Variables:</strong></td>
                <td style='padding: 5px;'>", n_vars, " variables</td>
            </tr>
            <tr>
                <td style='padding: 5px;'><strong>👥 Sample Size:</strong></td>
                <td style='padding: 5px;'>", n_samples, " patients</td>
            </tr>
            </table>

            <h4 style='color: #424242;'>What This Analysis Will Do:</h4>
            <ol style='line-height: 1.8;'>
            <li><strong>Build a Decision Tree:</strong> Create a clinical decision tree to predict '", target_level, "' outcomes</li>
            <li><strong>Evaluate Performance:</strong> Calculate sensitivity, specificity, and other clinical metrics</li>
            <li><strong>Identify Key Predictors:</strong> Determine which variables are most important for prediction</li>
            <li><strong>Provide Clinical Guidance:</strong> Offer interpretation for clinical decision-making</li>
            </ol>

            <p style='font-style: italic; color: #666; margin-top: 15px;'>
            💡 <strong>Tip:</strong> ",
            ifelse(algorithm == "fftrees",
                   "FFTrees creates simple, sequential decision rules ideal for clinical practice.",
                   "CART creates comprehensive trees that capture complex interactions between variables."),
            "</p>
            </div>")

            return(intro_html)
        },

        .validate_inputs = function() {
            # Comprehensive input validation

            message("[DEBUG-VALIDATE] Starting validation")
            message("[DEBUG-VALIDATE] Data rows: ", nrow(self$data))

            # Check if dataset exists and has data
            if (nrow(self$data) == 0) {
                message("[DEBUG-VALIDATE] Failed: No data rows")
                return(list(
                    valid = FALSE,
                    show_welcome = FALSE,
                    message = "The provided dataset contains no rows. Please check your data and try again."
                ))
            }

            # Check if any predictor variables are selected
            continuous_vars <- self$options$vars
            categorical_vars <- self$options$facs

            message("[DEBUG-VALIDATE] Continuous vars: ",
                   ifelse(is.null(continuous_vars), "NULL", paste(continuous_vars, collapse=", ")))
            message("[DEBUG-VALIDATE] Categorical vars: ",
                   ifelse(is.null(categorical_vars), "NULL", paste(categorical_vars, collapse=", ")))

            has_predictors <- (!is.null(continuous_vars) && length(continuous_vars) > 0) ||
                            (!is.null(categorical_vars) && length(categorical_vars) > 0)

            if (!has_predictors) {
                message("[DEBUG-VALIDATE] Failed: No predictors selected")
                return(list(
                    valid = FALSE,
                    show_welcome = TRUE,
                    message = "Please select at least one predictor variable (continuous or categorical)."
                ))
            }

            # Check for target variable
            message("[DEBUG-VALIDATE] Target: ",
                   ifelse(is.null(self$options$target), "NULL", self$options$target))

            if (is.null(self$options$target) || self$options$target == "") {
                message("[DEBUG-VALIDATE] Failed: No target variable")
                return(list(
                    valid = FALSE,
                    show_welcome = TRUE,
                    message = "Please select a target outcome variable for prediction."
                ))
            }

            # Check if target variable exists in data
            if (!self$options$target %in% names(self$data)) {
                message("[DEBUG-VALIDATE] Failed: Target not in data")
                return(list(
                    valid = FALSE,
                    show_welcome = FALSE,
                    message = paste("Target variable '", self$options$target, "' not found in the dataset.")
                ))
            }

            # Check for target level (positive class)
            target_data <- self$data[[self$options$target]]

            message("[DEBUG-VALIDATE] Target level: ",
                   ifelse(is.null(self$options$targetLevel), "NULL", self$options$targetLevel))

            # For factor/character variables, check if targetLevel is specified
            if (is.factor(target_data) || is.character(target_data)) {
                unique_levels <- unique(target_data[!is.na(target_data)])

                message("[DEBUG-VALIDATE] Available target levels: ", paste(unique_levels, collapse=", "))

                if (is.null(self$options$targetLevel) || self$options$targetLevel == "") {
                    message("[DEBUG-VALIDATE] Failed: No target level specified")
                    return(list(
                        valid = FALSE,
                        show_welcome = TRUE,
                        message = paste("Please select the positive class level for target variable '",
                                      self$options$target, "'. Available levels: ",
                                      paste(unique_levels, collapse = ", "))
                    ))
                }

                # Check if selected targetLevel exists in the data
                if (!self$options$targetLevel %in% unique_levels) {
                    return(list(
                        valid = FALSE,
                        show_welcome = FALSE,
                        message = paste("Selected target level '", self$options$targetLevel,
                                      "' not found in variable '", self$options$target,
                                      "'. Available levels: ", paste(unique_levels, collapse = ", "))
                    ))
                }

                # Check for sufficient cases in both classes
                level_counts <- table(target_data, useNA = "no")
                if (any(level_counts < 2)) {
                    return(list(
                        valid = FALSE,
                        show_welcome = FALSE,
                        message = paste("Insufficient cases for analysis. Each class needs at least 2 cases. Current counts: ",
                                      paste(names(level_counts), "=", level_counts, collapse = ", "))
                    ))
                }
            }

            # Check predictor variables exist in data (only if predictors are selected)
            all_predictors <- c(continuous_vars, categorical_vars)
            if (length(all_predictors) > 0) {
                missing_predictors <- all_predictors[!all_predictors %in% names(self$data)]

                if (length(missing_predictors) > 0) {
                    return(list(
                        valid = FALSE,
                        show_welcome = FALSE,
                        message = paste("Predictor variables not found in dataset: ",
                                      paste(missing_predictors, collapse = ", "))
                    ))
                }
            }

            # Check for sufficient complete cases (only if we have both predictors and target)
            if (length(all_predictors) > 0 && !is.null(self$options$target) && self$options$target != "") {
                analysis_vars <- c(all_predictors, self$options$target)
                complete_cases <- complete.cases(self$data[analysis_vars])
                n_complete <- sum(complete_cases)

                if (n_complete < 10) {
                    return(list(
                        valid = FALSE,
                        show_welcome = FALSE,
                        message = paste("Insufficient complete cases for analysis (", n_complete,
                                      " complete cases). Need at least 10 complete cases.")
                    ))
                }
            }

            # All validation checks passed
            message("[DEBUG-VALIDATE] All validation checks passed!")
            return(list(
                valid = TRUE,
                show_welcome = FALSE,
                message = "All validation checks passed."
            ))
        },

        # Advanced hyperparameter tuning with algorithm-specific grid search
        .tune_hyperparameters = function(formula, train_data, test_data, algorithm) {
            message("[DEBUG] Starting hyperparameter tuning for ", algorithm)

            if (algorithm == "rpart") {
                return(private$.tune_rpart_hyperparameters(formula, train_data, test_data))
            } else if (algorithm == "randomforest") {
                return(private$.tune_randomforest_hyperparameters(formula, train_data, test_data))
            } else if (algorithm == "xgboost") {
                return(private$.tune_xgboost_hyperparameters(formula, train_data, test_data))
            } else if (algorithm == "c50") {
                return(private$.tune_c50_hyperparameters(formula, train_data, test_data))
            } else {
                # Return basic model for algorithms without tuning
                return(private$.train_basic_model(formula, train_data, algorithm))
            }
        },

        # Random Forest hyperparameter tuning
        .tune_randomforest_hyperparameters = function(formula, train_data, test_data) {
            if (!requireNamespace("randomForest", quietly = TRUE)) {
                return(NULL)
            }

            # Define parameter grid
            ntree_values <- c(100, 300, 500, 1000)
            mtry_values <- c(
                floor(sqrt(ncol(train_data) - 1)),
                floor((ncol(train_data) - 1) / 3),
                floor((ncol(train_data) - 1) / 2)
            )
            mtry_values <- unique(pmax(1, mtry_values))

            best_performance <- -Inf
            best_params <- list(ntree = 500, mtry = mtry_values[1])
            tuning_metric <- self$options$tuning_metric %||% "bacc"

            total_combinations <- length(ntree_values) * length(mtry_values)
            current_combination <- 0

            for (ntree in ntree_values) {
                for (mtry in mtry_values) {
                    current_combination <- current_combination + 1

                    # Update progress
                    private$.update_algorithm_progress("randomforest", "hyperparameter_tuning", current_combination, total_combinations)

                    # Use OOB error for Random Forest tuning (faster than CV)
                    model <- randomForest::randomForest(
                        formula = formula,
                        data = train_data,
                        ntree = ntree,
                        mtry = mtry,
                        importance = TRUE
                    )

                    # Calculate performance using OOB predictions
                    oob_pred <- model$predicted
                    actual <- train_data[[all.vars(formula)[1]]]

                    performance <- private$.calculate_metric(actual, oob_pred, NULL, tuning_metric)

                    if (performance > best_performance) {
                        best_performance <- performance
                        best_params <- list(ntree = ntree, mtry = mtry, performance = performance)
                    }
                }
            }

            # Train final model with best parameters
            final_model <- randomForest::randomForest(
                formula = formula,
                data = train_data,
                ntree = best_params$ntree,
                mtry = best_params$mtry,
                importance = TRUE
            )

            final_model$tuning_results <- best_params
            return(final_model)
        },

        # XGBoost hyperparameter tuning
        .tune_xgboost_hyperparameters = function(formula, train_data, test_data) {
            if (!requireNamespace("xgboost", quietly = TRUE)) {
                return(NULL)
            }

            target_var <- all.vars(formula)[1]

            # Define parameter grid
            nrounds_values <- c(50, 100, 200, 300)
            eta_values <- c(0.1, 0.2, 0.3)
            max_depth_values <- c(3, 6, 9)

            best_performance <- -Inf
            best_params <- list(nrounds = 100, eta = 0.3, max_depth = 6)
            tuning_metric <- self$options$tuning_metric %||% "bacc"

            # Prepare data for XGBoost
            train_prepared <- private$.prepare_data_for_xgboost(train_data, target_var)

            for (nrounds in nrounds_values) {
                for (eta in eta_values) {
                    for (max_depth in max_depth_values) {

                        # Use cross-validation within XGBoost
                        cv_result <- xgboost::xgb.cv(
                            data = xgboost::xgb.DMatrix(train_prepared$X, label = train_prepared$y),
                            objective = "binary:logistic",
                            eval_metric = "auc",
                            nrounds = nrounds,
                            eta = eta,
                            max_depth = max_depth,
                            nfold = 5,
                            verbose = 0,
                            showsd = FALSE
                        )

                        # Get best AUC from CV
                        performance <- max(cv_result$evaluation_log$test_auc_mean)

                        if (performance > best_performance) {
                            best_performance <- performance
                            best_params <- list(
                                nrounds = nrounds,
                                eta = eta,
                                max_depth = max_depth,
                                performance = performance
                            )
                        }
                    }
                }
            }

            # Train final model with best parameters
            final_model <- xgboost::xgb.train(
                data = xgboost::xgb.DMatrix(train_prepared$X, label = train_prepared$y),
                objective = "binary:logistic",
                eval_metric = "auc",
                nrounds = best_params$nrounds,
                eta = best_params$eta,
                max_depth = best_params$max_depth,
                verbose = 0
            )

            final_model$tuning_results <- best_params
            return(final_model)
        },

        # C5.0 hyperparameter tuning
        .tune_c50_hyperparameters = function(formula, train_data, test_data) {
            if (!requireNamespace("C50", quietly = TRUE)) {
                return(NULL)
            }

            # Define parameter grid
            trials_values <- c(1, 5, 10, 20)
            winnow_values <- c(FALSE, TRUE)

            best_performance <- -Inf
            best_params <- list(trials = 1, winnow = FALSE)
            tuning_metric <- self$options$tuning_metric %||% "bacc"

            n_folds <- self$options$cv_folds %||% 5
            target_var <- all.vars(formula)[1]
            folds <- private$.create_balanced_folds(train_data, target_var, n_folds)

            for (trials in trials_values) {
                for (winnow in winnow_values) {

                    # Cross-validation
                    cv_scores <- numeric(n_folds)

                    for (fold in 1:n_folds) {
                        fold_train <- train_data[folds != fold, ]
                        fold_valid <- train_data[folds == fold, ]

                        model <- C50::C5.0(
                            formula = formula,
                            data = fold_train,
                            trials = trials,
                            winnow = winnow
                        )

                        pred_class <- predict(model, fold_valid, type = "class")
                        actual <- fold_valid[[target_var]]

                        cv_scores[fold] <- private$.calculate_metric(actual, pred_class, NULL, tuning_metric)
                    }

                    performance <- mean(cv_scores, na.rm = TRUE)

                    if (performance > best_performance) {
                        best_performance <- performance
                        best_params <- list(trials = trials, winnow = winnow, performance = performance)
                    }
                }
            }

            # Train final model with best parameters
            final_model <- C50::C5.0(
                formula = formula,
                data = train_data,
                trials = best_params$trials,
                winnow = best_params$winnow
            )

            final_model$tuning_results <- best_params
            return(final_model)
        },

        # Train basic model without tuning (fallback)
        .train_basic_model = function(formula, train_data, algorithm) {
            if (algorithm == "fftrees" && requireNamespace("FFTrees", quietly = TRUE)) {
                return(FFTrees::FFTrees(formula = formula, data = train_data, quiet = list(ini = TRUE, fin = FALSE, mis = FALSE, set = TRUE)))
            } else if (algorithm == "ctree" && requireNamespace("party", quietly = TRUE)) {
                return(party::ctree(formula = formula, data = train_data, controls = party::ctree_control(mincriterion = 0.95)))
            } else {
                return(rpart::rpart(formula = formula, data = train_data))
            }
        },

        # Original rpart-specific hyperparameter tuning (kept for compatibility)
        .tune_rpart_hyperparameters = function(formula, train_data, test_data) {
            message("[DEBUG] Starting hyperparameter tuning")

            # Define parameter grid for tuning
            cp_values <- c(0.001, 0.005, 0.01, 0.02, 0.05, 0.1)
            maxdepth_values <- c(3, 5, 7, 10, 15)
            minsplit_values <- c(10, 20, 30, 50)

            # Cross-validation setup
            n_folds <- self$options$cv_folds %||% 5
            target_var <- self$options$target
            target_levels <- levels(train_data[[target_var]])

            # Create folds ensuring balanced target distribution
            folds <- private$.create_balanced_folds(train_data, target_var, n_folds)

            best_performance <- -Inf
            best_params <- list(cp = 0.01, maxdepth = 6, minsplit = 20)
            best_model <- NULL
            tuning_metric <- self$options$tuning_metric %||% "bacc"

            # Grid search with cross-validation
            total_combinations <- length(cp_values) * length(maxdepth_values) * length(minsplit_values)
            current_combination <- 0

            message("[DEBUG] Testing ", total_combinations, " parameter combinations")

            for (cp in cp_values) {
                for (maxdepth in maxdepth_values) {
                    for (minsplit in minsplit_values) {
                        current_combination <- current_combination + 1

                        if (current_combination %% 10 == 0) {
                            message("[DEBUG] Progress: ", current_combination, "/", total_combinations)
                        }

                        # Cross-validation for this parameter combination
                        cv_scores <- numeric(n_folds)

                        for (fold in 1:n_folds) {
                            fold_train <- train_data[folds != fold, ]
                            fold_valid <- train_data[folds == fold, ]

                            # Train model with current parameters
                            fold_model <- rpart::rpart(
                                formula,
                                data = fold_train,
                                control = rpart::rpart.control(
                                    cp = cp,
                                    maxdepth = maxdepth,
                                    minsplit = minsplit,
                                    minbucket = max(1, floor(minsplit/3))
                                )
                            )

                            # Make predictions
                            fold_pred_prob <- predict(fold_model, fold_valid, type = "prob")
                            fold_pred_class <- predict(fold_model, fold_valid, type = "class")
                            fold_actual <- fold_valid[[target_var]]

                            # Calculate performance metric
                            cv_scores[fold] <- private$.calculate_metric(
                                fold_actual, fold_pred_class, fold_pred_prob, tuning_metric
                            )
                        }

                        # Average performance across folds
                        mean_performance <- mean(cv_scores, na.rm = TRUE)
                        se_performance <- sd(cv_scores, na.rm = TRUE) / sqrt(n_folds)

                        # Update best parameters
                        if (mean_performance > best_performance) {
                            best_performance <- mean_performance
                            best_params <- list(
                                cp = cp,
                                maxdepth = maxdepth,
                                minsplit = minsplit,
                                performance = mean_performance,
                                se = se_performance
                            )
                        }
                    }
                }
            }

            message("[DEBUG] Best parameters found: cp=", best_params$cp,
                   ", maxdepth=", best_params$maxdepth,
                   ", minsplit=", best_params$minsplit,
                   ", performance=", round(best_params$performance, 4))

            # Apply 1-SE rule if requested
            if (self$options$use_1se_rule) {
                message("[DEBUG] Applying 1-SE rule for model selection")
                # This would require storing all results and selecting simpler model
                # For now, use best parameters found
            }

            # Train final model with best parameters
            final_model <- rpart::rpart(
                formula,
                data = train_data,
                control = rpart::rpart.control(
                    cp = best_params$cp,
                    maxdepth = best_params$maxdepth,
                    minsplit = best_params$minsplit,
                    minbucket = max(1, floor(best_params$minsplit/3))
                )
            )

            # Store tuning results for reporting
            final_model$tuning_results <- best_params

            return(final_model)
        },

        # Clinical imputation method
        .clinical_imputation = function(data, target_var) {
            message("[DEBUG] Performing clinical imputation")

            for (col_name in names(data)) {
                if (col_name == target_var) next

                col_data <- data[[col_name]]
                missing_idx <- is.na(col_data)

                if (sum(missing_idx) == 0) next

                if (is.numeric(col_data)) {
                    # For continuous variables: median within disease groups
                    for (level in levels(data[[target_var]])) {
                        level_idx <- data[[target_var]] == level & !missing_idx
                        if (sum(level_idx) > 0) {
                            level_median <- median(col_data[level_idx], na.rm = TRUE)
                            missing_in_level <- missing_idx & data[[target_var]] == level
                            data[missing_in_level, col_name] <- level_median
                        }
                    }
                    # For cases where target is also missing, use overall median
                    remaining_missing <- is.na(data[[col_name]])
                    if (sum(remaining_missing) > 0) {
                        overall_median <- median(col_data, na.rm = TRUE)
                        data[remaining_missing, col_name] <- overall_median
                    }
                } else {
                    # For categorical variables: mode within disease groups
                    for (level in levels(data[[target_var]])) {
                        level_idx <- data[[target_var]] == level & !missing_idx
                        if (sum(level_idx) > 0) {
                            level_mode <- names(sort(table(col_data[level_idx]), decreasing = TRUE))[1]
                            missing_in_level <- missing_idx & data[[target_var]] == level
                            data[missing_in_level, col_name] <- level_mode
                        }
                    }
                    # For cases where target is also missing, use overall mode
                    remaining_missing <- is.na(data[[col_name]])
                    if (sum(remaining_missing) > 0) {
                        overall_mode <- names(sort(table(col_data), decreasing = TRUE))[1]
                        data[remaining_missing, col_name] <- overall_mode
                    }
                }
            }

            return(data)
        },

        # Create balanced folds for cross-validation
        .create_balanced_folds = function(data, target_var, n_folds) {
            target <- data[[target_var]]
            n <- nrow(data)
            folds <- numeric(n)

            # Create stratified folds to maintain class balance
            for (level in levels(target)) {
                level_idx <- which(target == level)
                level_folds <- sample(rep(1:n_folds, length.out = length(level_idx)))
                folds[level_idx] <- level_folds
            }

            return(folds)
        },

        # Calculate performance metrics for hyperparameter tuning
        .calculate_metric = function(actual, predicted_class, predicted_prob, metric) {
            if (length(levels(actual)) != 2) return(NA)

            # Create confusion matrix
            cm <- table(actual, predicted_class)
            if (any(dim(cm) != c(2, 2))) return(NA)

            tn <- cm[1, 1]
            fp <- cm[1, 2]
            fn <- cm[2, 1]
            tp <- cm[2, 2]

            # Calculate metrics
            sensitivity <- tp / (tp + fn)
            specificity <- tn / (tn + fp)
            precision <- tp / (tp + fp)
            accuracy <- (tp + tn) / (tp + tn + fp + fn)

            # AUC calculation
            auc <- tryCatch({
                if (!is.null(predicted_prob) && ncol(predicted_prob) >= 2) {
                    positive_probs <- predicted_prob[, 2]
                    pROC::auc(actual, positive_probs, quiet = TRUE)
                } else {
                    NA
                }
            }, error = function(e) NA)

            # Return requested metric
            switch(metric,
                "bacc" = (sensitivity + specificity) / 2,
                "auc" = as.numeric(auc),
                "sens" = sensitivity,
                "spec" = specificity,
                "f1" = 2 * (precision * sensitivity) / (precision + sensitivity),
                "prec" = precision,
                "acc" = accuracy,
                NA
            )
        },

        # Get tree plot parameters based on style
        .get_tree_plot_params = function(style, show_node_stats) {
            params <- list()

            # Get user-specified branch style and margin
            branch_style <- self$options$plot_branch_style %||% 0.2
            plot_margin <- self$options$plot_margin %||% 0.1

            switch(style,
                "standard" = {
                    params$type <- 4  # All four coordinates
                    params$extra <- if(show_node_stats) 101 else 2  # Class name + percentage
                    params$fallen.leaves <- TRUE
                    params$branch <- 0.3
                    params$uniform <- TRUE
                    params$cex <- 0.9
                    params$varlen <- 0  # Use full variable names
                    params$faclen <- 0  # Use full factor level names
                },
                "medical" = {
                    params$type <- 0  # Simple, clean style (Appsilon-inspired)
                    params$extra <- 2  # Show class and percentage only
                    params$fallen.leaves <- TRUE
                    params$branch <- 0.5  # Wider branch angle
                    params$uniform <- TRUE
                    params$cex <- 0.85  # Slightly smaller text for readability
                    params$varlen <- 8   # Truncate variable names for clinical clarity
                    params$faclen <- 4   # Truncate factor levels
                    params$roundint <- FALSE  # Don't round integers
                },
                "clinical" = {
                    params$type <- 4  # Detailed style for researchers
                    params$extra <- 104  # Show all available information
                    params$fallen.leaves <- FALSE  # Keep natural tree structure
                    params$branch <- 0.4
                    params$uniform <- FALSE
                    params$cex <- 0.7  # Smaller text to fit more detail
                    params$varlen <- 0  # Full variable names for detailed analysis
                    params$faclen <- 0  # Full factor level names
                    params$digits <- 3  # More decimal precision
                    params$roundint <- FALSE
                },
                "uniform" = {
                    params$type <- 4  # All coordinates
                    params$extra <- if(show_node_stats) 101 else 2
                    params$fallen.leaves <- TRUE
                    params$uniform <- TRUE  # Equal branch lengths
                    params$branch <- branch_style
                    params$margin <- plot_margin
                    params$cex <- 0.9
                    params$varlen <- 0
                    params$faclen <- 0
                },
                "compressed" = {
                    params$type <- 4
                    params$extra <- if(show_node_stats) 101 else 2
                    params$fallen.leaves <- TRUE
                    params$uniform <- TRUE
                    params$compress <- TRUE  # Compress tree layout
                    params$branch <- branch_style
                    params$margin <- plot_margin
                    params$cex <- 0.8  # Smaller text for compressed layout
                    params$varlen <- 8   # Truncate for space efficiency
                    params$faclen <- 4
                },
                "fancy" = {
                    params$type <- 4
                    params$extra <- if(show_node_stats) 104 else 101  # Enhanced information
                    params$fallen.leaves <- TRUE
                    params$uniform <- TRUE
                    params$compress <- TRUE
                    params$branch <- branch_style
                    params$margin <- plot_margin
                    params$cex <- 0.9
                    params$varlen <- 0  # Full names for publication
                    params$faclen <- 0
                    params$digits <- 3
                    params$all <- TRUE  # Show all node information
                    params$fancy <- TRUE  # Enable fancy formatting if available
                }
            )

            return(params)
        },

        # Get color palette for tree visualization
        .get_color_palette = function(style) {
            switch(style,
                "standard" = c("#E1F5FE", "#B3E5FC", "#81D4FA", "#4FC3F7"),  # Blue gradient
                "medical" = c("#E8F5E8", "#A5D6A7", "#66BB6A", "#43A047"),    # Green gradient (medical)
                "clinical" = c("#FFF3E0", "#FFCC80", "#FF9800", "#F57C00"),   # Orange gradient (research)
                "uniform" = c("#F3E5F5", "#CE93D8", "#BA68C8", "#9C27B0"),    # Purple gradient (uniform)
                "compressed" = c("#E0F2F1", "#80CBC4", "#4DB6AC", "#26A69A"), # Teal gradient (compressed)
                "fancy" = c("#FCE4EC", "#F48FB1", "#EC407A", "#E91E63"),      # Pink gradient (fancy/publication)
                c("#E3F2FD", "#BBDEFB", "#90CAF9", "#64B5F6")  # Default blue
            )
        },

        # Generate CP table analysis
        .generate_cp_table_analysis = function(model_results) {
            if (is.null(model_results) || is.null(model_results$model)) {
                return(NULL)
            }

            model <- model_results$model
            if (!inherits(model, "rpart")) {
                return(NULL)
            }

            # Get CP table
            cp_table <- model$cptable

            if (is.null(cp_table) || nrow(cp_table) == 0) {
                return(NULL)
            }

            # Find optimal CP using 1-SE rule if enabled
            optimal_cp_idx <- if (self$options$use_1se_rule) {
                # Find minimum xerror
                min_xerror <- min(cp_table[, "xerror"])
                min_se <- cp_table[which.min(cp_table[, "xerror"]), "xstd"]

                # Find simplest model within 1 SE
                which(cp_table[, "xerror"] <= min_xerror + min_se)[1]
            } else {
                which.min(cp_table[, "xerror"])
            }

            optimal_cp <- cp_table[optimal_cp_idx, "CP"]

            # Generate HTML table
            html <- paste0("
            <div style='margin: 20px 0;'>
            <h4 style='color: #1976d2;'>Complexity Parameter Analysis</h4>
            <p><strong>Optimal CP:</strong> ", round(optimal_cp, 6), " (Row ", optimal_cp_idx, ")</p>
            <table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>
            <tr style='background-color: #f5f5f5; border: 1px solid #ddd;'>
            <th style='padding: 8px; text-align: left;'>Split</th>
            <th style='padding: 8px; text-align: left;'>CP</th>
            <th style='padding: 8px; text-align: left;'>Node Error</th>
            <th style='padding: 8px; text-align: left;'>X-Error</th>
            <th style='padding: 8px; text-align: left;'>X-Std</th>
            </tr>")

            for (i in 1:nrow(cp_table)) {
                row_style <- if (i == optimal_cp_idx) {
                    "background-color: #e3f2fd; border: 1px solid #ddd; font-weight: bold;"
                } else {
                    "border: 1px solid #ddd;"
                }

                html <- paste0(html, "
                <tr style='", row_style, "'>
                <td style='padding: 8px;'>", cp_table[i, "nsplit"], "</td>
                <td style='padding: 8px;'>", round(cp_table[i, "CP"], 6), "</td>
                <td style='padding: 8px;'>", round(cp_table[i, "rel error"], 4), "</td>
                <td style='padding: 8px;'>", round(cp_table[i, "xerror"], 4), "</td>
                <td style='padding: 8px;'>", round(cp_table[i, "xstd"], 4), "</td>
                </tr>")
            }

            html <- paste0(html, "
            </table>
            <p style='font-size: 12px; color: #666; margin-top: 10px;'>
            <em>Optimal row highlighted. Lower X-Error indicates better cross-validation performance.</em>
            </p>
            </div>")

            return(html)
        },

        # Generate detailed variable importance analysis
        .generate_detailed_importance_analysis = function(model_results) {
            if (is.null(model_results) || is.null(model_results$model)) {
                return(NULL)
            }

            model <- model_results$model
            algorithm <- model_results$algorithm

            # Get variable importance
            importance_data <- model_results$importance
            if (is.null(importance_data) || nrow(importance_data) == 0) {
                return(NULL)
            }

            html <- paste0("
            <div style='margin: 20px 0;'>
            <h4 style='color: #1976d2;'>Detailed Variable Importance Analysis</h4>")

            if (algorithm == "rpart" && inherits(model, "rpart")) {
                # Add surrogate splits information if available
                if (!is.null(model$splits)) {
                    html <- paste0(html, "
                    <h5 style='color: #2c5aa0;'>Primary and Surrogate Splits:</h5>
                    <p>This tree uses surrogate splits to handle missing data effectively.</p>")
                }

                # Add variable usage summary
                var_usage <- model$variable.importance
                if (!is.null(var_usage) && length(var_usage) > 0) {
                    html <- paste0(html, "
                    <h5 style='color: #2c5aa0;'>Variable Usage Summary:</h5>
                    <table style='border-collapse: collapse; margin: 10px 0;'>
                    <tr style='background-color: #f5f5f5;'>
                    <th style='padding: 5px; border: 1px solid #ddd;'>Variable</th>
                    <th style='padding: 5px; border: 1px solid #ddd;'>Usage Score</th>
                    <th style='padding: 5px; border: 1px solid #ddd;'>Relative Importance</th>
                    </tr>")

                    max_importance <- max(var_usage)
                    for (var_name in names(var_usage)) {
                        importance_val <- var_usage[var_name]
                        rel_importance <- round(100 * importance_val / max_importance, 1)

                        html <- paste0(html, "
                        <tr>
                        <td style='padding: 5px; border: 1px solid #ddd;'>", var_name, "</td>
                        <td style='padding: 5px; border: 1px solid #ddd;'>", round(importance_val, 2), "</td>
                        <td style='padding: 5px; border: 1px solid #ddd;'>", rel_importance, "%</td>
                        </tr>")
                    }

                    html <- paste0(html, "</table>")
                }
            }

            html <- paste0(html, "</div>")
            return(html)
        },

        # Model stability analysis
        .stability_analysis = function() {
            model_results <- self$results$model_results
            if (is.null(model_results)) {
                return(NULL)
            }

            model <- model_results$model
            algorithm <- model_results$algorithm

            # Perform bootstrap stability analysis
            n_bootstrap <- min(100, self$options$n_bootstrap)
            stability_results <- list()

            data <- self$data
            target_col <- self$options$target

            if (is.null(target_col) || target_col == "") {
                return(NULL)
            }

            set.seed(self$options$seed_value)

            for (i in 1:n_bootstrap) {
                private$.checkpoint()  # Add checkpoint before expensive bootstrap operation
                
                # Bootstrap sample
                boot_indices <- sample(nrow(data), replace = TRUE)
                boot_data <- data[boot_indices, ]

                # Train model on bootstrap sample
                tryCatch({
                    if (algorithm == "rpart") {
                        boot_model <- rpart::rpart(
                            formula = self$formula,
                            data = boot_data,
                            method = ifelse(self$options$tree_mode == "regression", "anova", "class"),
                            control = rpart::rpart.control(
                                maxdepth = self$options$max_depth,
                                minsplit = self$options$min_samples_split,
                                minbucket = self$options$min_samples_leaf,
                                cp = self$options$cost_complexity
                            )
                        )

                        # Extract key model characteristics
                        stability_results[[i]] <- list(
                            n_nodes = length(unique(boot_model$where)),
                            n_splits = nrow(boot_model$splits),
                            variables_used = names(boot_model$variable.importance)[1:min(5, length(boot_model$variable.importance))],
                            cp_optimal = boot_model$cptable[which.min(boot_model$cptable[, "xerror"]), "CP"]
                        )
                    }
                }, error = function(e) {
                    stability_results[[i]] <<- NULL
                })
            }

            # Remove NULL results
            stability_results <- stability_results[!sapply(stability_results, is.null)]

            if (length(stability_results) == 0) {
                return("Stability analysis could not be completed.")
            }

            # Analyze stability metrics
            n_nodes_vec <- sapply(stability_results, function(x) x$n_nodes)
            n_splits_vec <- sapply(stability_results, function(x) x$n_splits)
            cp_values <- sapply(stability_results, function(x) x$cp_optimal)

            # Variable selection consistency
            all_vars <- unlist(lapply(stability_results, function(x) x$variables_used))
            var_freq <- table(all_vars)
            var_stability <- round(100 * var_freq / length(stability_results), 1)

            html <- paste0("
            <div style='margin: 20px 0;'>
            <h4 style='color: #1976d2;'>Model Stability Analysis (", length(stability_results), " bootstrap samples)</h4>

            <h5 style='color: #2c5aa0;'>Tree Structure Stability:</h5>
            <ul>
            <li><strong>Number of nodes:</strong> Mean = ", round(mean(n_nodes_vec), 1),
            " ± ", round(sd(n_nodes_vec), 1), " (Range: ", min(n_nodes_vec), "-", max(n_nodes_vec), ")</li>
            <li><strong>Number of splits:</strong> Mean = ", round(mean(n_splits_vec), 1),
            " ± ", round(sd(n_splits_vec), 1), " (Range: ", min(n_splits_vec), "-", max(n_splits_vec), ")</li>
            <li><strong>Optimal CP:</strong> Mean = ", round(mean(cp_values), 4),
            " ± ", round(sd(cp_values), 4), "</li>
            </ul>

            <h5 style='color: #2c5aa0;'>Variable Selection Consistency:</h5>
            <table style='border-collapse: collapse; margin: 10px 0;'>
            <tr style='background-color: #f5f5f5;'>
            <th style='padding: 5px; border: 1px solid #ddd;'>Variable</th>
            <th style='padding: 5px; border: 1px solid #ddd;'>Selection Frequency (%)</th>
            <th style='padding: 5px; border: 1px solid #ddd;'>Stability Rating</th>
            </tr>")

            for (var_name in names(var_stability)) {
                freq <- var_stability[var_name]
                rating <- if (freq >= 80) "High" else if (freq >= 50) "Moderate" else "Low"
                color <- if (freq >= 80) "#4caf50" else if (freq >= 50) "#ff9800" else "#f44336"

                html <- paste0(html, "
                <tr>
                <td style='padding: 5px; border: 1px solid #ddd;'>", var_name, "</td>
                <td style='padding: 5px; border: 1px solid #ddd;'>", freq, "%</td>
                <td style='padding: 5px; border: 1px solid #ddd; color: ", color, ";'><strong>", rating, "</strong></td>
                </tr>")
            }

            html <- paste0(html, "</table>

            <h5 style='color: #2c5aa0;'>Clinical Interpretation:</h5>
            <ul>
            <li><strong>High stability (≥80%):</strong> Variables consistently important for clinical decisions</li>
            <li><strong>Moderate stability (50-79%):</strong> Variables with conditional importance</li>
            <li><strong>Low stability (<50%):</strong> Variables may be unreliable for clinical use</li>
            </ul>
            </div>")

            return(html)
        },

        # Learning curves analysis
        .plot_learning_curves = function(image, ggtheme, theme, ...) {
            if (!self$options$learning_curves) {
                return(FALSE)
            }

            model_results <- self$results$model_results
            if (is.null(model_results)) {
                return(FALSE)
            }

            data <- self$data
            target_col <- self$options$target

            if (is.null(target_col) || target_col == "") {
                return(FALSE)
            }

            set.seed(self$options$seed_value)

            # Define training sizes
            n_total <- nrow(data)
            training_sizes <- seq(0.1, 0.9, by = 0.1)
            sample_sizes <- round(training_sizes * n_total)

            # Storage for results
            results_df <- data.frame()

            for (size in sample_sizes) {
                for (rep in 1:10) {  # 10 repetitions for each size
                    private$.checkpoint()  # Add checkpoint before expensive learning curve iteration
                    
                    # Create train/test split
                    train_indices <- sample(n_total, size)
                    test_indices <- setdiff(1:n_total, train_indices)

                    if (length(test_indices) < 10) next  # Need minimum test size

                    train_data <- data[train_indices, ]
                    test_data <- data[test_indices, ]

                    tryCatch({
                        # Train model
                        model <- rpart::rpart(
                            formula = self$formula,
                            data = train_data,
                            method = ifelse(self$options$tree_mode == "regression", "anova", "class"),
                            control = rpart::rpart.control(
                                maxdepth = self$options$max_depth,
                                minsplit = self$options$min_samples_split,
                                minbucket = self$options$min_samples_leaf,
                                cp = self$options$cost_complexity
                            )
                        )

                        # Predictions on training and test sets
                        train_pred <- predict(model, train_data, type = "class")
                        test_pred <- predict(model, test_data, type = "class")

                        # Calculate accuracies
                        train_acc <- mean(train_pred == train_data[[target_col]], na.rm = TRUE)
                        test_acc <- mean(test_pred == test_data[[target_col]], na.rm = TRUE)

                        # Store results
                        results_df <- rbind(results_df, data.frame(
                            training_size = size,
                            rep = rep,
                            train_accuracy = train_acc,
                            test_accuracy = test_acc
                        ))

                    }, error = function(e) {
                        # Skip this iteration if error occurs
                    })
                }
            }

            if (nrow(results_df) == 0) {
                return(FALSE)
            }

            # Aggregate results
            agg_results <- results_df %>%
                dplyr::group_by(training_size) %>%
                dplyr::summarise(
                    train_mean = mean(train_accuracy, na.rm = TRUE),
                    train_se = sd(train_accuracy, na.rm = TRUE) / sqrt(n()),
                    test_mean = mean(test_accuracy, na.rm = TRUE),
                    test_se = sd(test_accuracy, na.rm = TRUE) / sqrt(n()),
                    .groups = 'drop'
                )

            # Create learning curves plot
            plot_data <- tidyr::pivot_longer(
                agg_results,
                cols = c(train_mean, test_mean),
                names_to = "dataset",
                values_to = "accuracy"
            )

            plot_data$se <- ifelse(plot_data$dataset == "train_mean", agg_results$train_se, agg_results$test_se)
            plot_data$dataset <- ifelse(plot_data$dataset == "train_mean", "Training", "Testing")

            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = training_size, y = accuracy, color = dataset)) +
                ggplot2::geom_line(size = 1.2) +
                ggplot2::geom_point(size = 2) +
                ggplot2::geom_errorbar(ggplot2::aes(ymin = accuracy - se, ymax = accuracy + se),
                                     width = 0.02, alpha = 0.7) +
                ggplot2::scale_color_manual(values = c("Training" = "#2196f3", "Testing" = "#ff5722")) +
                ggplot2::labs(
                    title = "Learning Curves: Performance vs Training Size",
                    subtitle = "Optimal training size balances performance and overfitting",
                    x = "Training Set Size (n cases)",
                    y = "Classification Accuracy",
                    color = "Dataset"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(size = 14, face = "bold", color = "#1976d2"),
                    plot.subtitle = ggplot2::element_text(size = 11, color = "#666"),
                    legend.position = "bottom",
                    panel.grid.minor = ggplot2::element_blank()
                )

            print(p)
            return(TRUE)
        },

        # Results functions that are automatically called by jamovi framework
        stability_analysis = function() {
            if (!self$options$model_stability_analysis) {
                return("")
            }
            return(private$.stability_analysis())
        },

        outlier_analysis_results = function() {
            if (!self$options$outlier_analysis) {
                return("")
            }
            return(private$.outlier_analysis())
        },

        prediction_intervals_table = function() {
            if (!self$options$prediction_intervals) {
                return(data.frame())
            }
            return(private$.prediction_intervals())
        },

        pruning_analysis = function() {
            if (self$options$advanced_pruning == "cp") {
                return("")
            }
            return(private$.advanced_pruning_analysis())
        },

        enhanced_cv_results = function() {
            if (!(self$options$validation %in% c("stratified_cv", "repeated_cv"))) {
                return("")
            }
            return(private$.generate_enhanced_cv_results())
        },

        enhanced_cv_table = function() {
            if (!(self$options$validation %in% c("stratified_cv", "repeated_cv"))) {
                return(data.frame())
            }
            return(private$.generate_enhanced_cv_table())
        },

        enhanced_confusion_matrix = function() {
            if (!self$options$show_confusion_matrix) {
                return("")
            }
            return(private$.generate_enhanced_confusion_matrix_results())
        },

        # Outlier detection and influence analysis
        .outlier_analysis = function() {
            model_results <- self$results$model_results
            if (is.null(model_results)) {
                return(NULL)
            }

            model <- model_results$model
            data <- self$data
            target_col <- self$options$target

            if (is.null(target_col) || target_col == "") {
                return(NULL)
            }

            # Calculate influence measures
            tryCatch({
                # Cook's distance-like measure for trees
                predictions <- predict(model, data)
                residuals <- if (self$options$tree_mode == "regression") {
                    data[[target_col]] - predictions
                } else {
                    pred_class <- predict(model, data, type = "class")
                    as.numeric(data[[target_col]] != pred_class)
                }

                # Leverage-like measure: distance from tree structure
                node_assignments <- predict(model, data, type = "vector")

                # Identify potential outliers based on residuals and node assignments
                residual_threshold <- quantile(abs(residuals), 0.95, na.rm = TRUE)
                outlier_indices <- which(abs(residuals) > residual_threshold)

                html <- paste0("
                <div style='margin: 20px 0;'>
                <h4 style='color: #1976d2;'>Outlier and Influence Analysis</h4>

                <h5 style='color: #2c5aa0;'>Outlier Detection Summary:</h5>
                <ul>
                <li><strong>Total observations:</strong> ", nrow(data), "</li>
                <li><strong>Potential outliers identified:</strong> ", length(outlier_indices), " (",
                round(100 * length(outlier_indices) / nrow(data), 1), "%)</li>
                <li><strong>Detection threshold (95th percentile):</strong> ", round(residual_threshold, 3), "</li>
                </ul>")

                if (length(outlier_indices) > 0) {
                    html <- paste0(html, "
                    <h5 style='color: #2c5aa0;'>Influential Observations:</h5>
                    <table style='border-collapse: collapse; margin: 10px 0;'>
                    <tr style='background-color: #f5f5f5;'>
                    <th style='padding: 5px; border: 1px solid #ddd;'>Case ID</th>
                    <th style='padding: 5px; border: 1px solid #ddd;'>Residual</th>
                    <th style='padding: 5px; border: 1px solid #ddd;'>Actual</th>
                    <th style='padding: 5px; border: 1px solid #ddd;'>Predicted</th>
                    </tr>")

                    # Show top 10 outliers
                    top_outliers <- head(outlier_indices[order(abs(residuals[outlier_indices]), decreasing = TRUE)], 10)

                    for (idx in top_outliers) {
                        actual_val <- data[[target_col]][idx]
                        pred_val <- if (self$options$tree_mode == "regression") {
                            round(predictions[idx], 3)
                        } else {
                            predict(model, data[idx, ], type = "class")
                        }

                        html <- paste0(html, "
                        <tr>
                        <td style='padding: 5px; border: 1px solid #ddd;'>", idx, "</td>
                        <td style='padding: 5px; border: 1px solid #ddd;'>", round(residuals[idx], 3), "</td>
                        <td style='padding: 5px; border: 1px solid #ddd;'>", actual_val, "</td>
                        <td style='padding: 5px; border: 1px solid #ddd;'>", pred_val, "</td>
                        </tr>")
                    }

                    html <- paste0(html, "</table>")
                }

                html <- paste0(html, "
                <h5 style='color: #2c5aa0;'>Clinical Recommendations:</h5>
                <ul>
                <li><strong>Review outlier cases:</strong> Check for data entry errors or unusual clinical presentations</li>
                <li><strong>Consider exclusion criteria:</strong> Evaluate if outliers represent valid but rare cases</li>
                <li><strong>Robust modeling:</strong> Consider ensemble methods or robust tree algorithms if many outliers present</li>
                </ul>
                </div>")

                return(html)

            }, error = function(e) {
                return("Outlier analysis could not be completed due to model limitations.")
            })
        },

        # Prediction confidence intervals
        .prediction_intervals = function() {
            model_results <- self$results$model_results
            if (is.null(model_results)) {
                return(data.frame())
            }

            model <- model_results$model
            data <- self$data
            target_col <- self$options$target

            if (is.null(target_col) || target_col == "") {
                return(data.frame())
            }

            tryCatch({
                # For now, implement a basic bootstrap-based confidence interval approach
                n_bootstrap <- min(50, self$options$n_bootstrap)  # Limit for speed
                set.seed(self$options$seed_value)

                n_obs <- min(100, nrow(data))  # Limit to first 100 observations for demo
                predictions_matrix <- matrix(NA, nrow = n_obs, ncol = n_bootstrap)

                for (i in 1:n_bootstrap) {
                    private$.checkpoint()  # Add checkpoint before expensive bootstrap confidence interval
                    
                    # Bootstrap sample indices
                    boot_indices <- sample(nrow(data), replace = TRUE)
                    boot_data <- data[boot_indices, ]

                    # Train model on bootstrap sample
                    boot_model <- rpart::rpart(
                        formula = self$formula,
                        data = boot_data,
                        method = ifelse(self$options$tree_mode == "regression", "anova", "class"),
                        control = rpart::rpart.control(
                            maxdepth = self$options$max_depth,
                            minsplit = self$options$min_samples_split,
                            minbucket = self$options$min_samples_leaf,
                            cp = self$options$cost_complexity
                        )
                    )

                    # Get predictions on original data
                    if (self$options$tree_mode == "regression") {
                        predictions_matrix[, i] <- predict(boot_model, data[1:n_obs, ])
                    } else {
                        # For classification, get probability of positive class
                        probs <- predict(boot_model, data[1:n_obs, ], type = "prob")
                        if (ncol(probs) >= 2) {
                            predictions_matrix[, i] <- probs[, 2]  # Probability of positive class
                        } else {
                            predictions_matrix[, i] <- probs[, 1]
                        }
                    }
                }

                # Calculate confidence intervals
                result_df <- data.frame()
                for (i in 1:n_obs) {
                    pred_values <- predictions_matrix[i, ]
                    pred_values <- pred_values[!is.na(pred_values)]

                    if (length(pred_values) > 5) {  # Need sufficient predictions
                        mean_pred <- mean(pred_values)
                        ci_lower <- quantile(pred_values, 0.025, na.rm = TRUE)
                        ci_upper <- quantile(pred_values, 0.975, na.rm = TRUE)
                        interval_width <- ci_upper - ci_lower

                        prediction_label <- if (self$options$tree_mode == "regression") {
                            round(mean_pred, 3)
                        } else {
                            ifelse(mean_pred > 0.5, paste0(self$options$targetLevel, " (", round(mean_pred, 3), ")"),
                                   paste0("Not ", self$options$targetLevel, " (", round(1 - mean_pred, 3), ")"))
                        }

                        result_df <- rbind(result_df, data.frame(
                            observation = i,
                            prediction = prediction_label,
                            probability = round(mean_pred, 3),
                            ci_lower = round(ci_lower, 3),
                            ci_upper = round(ci_upper, 3),
                            interval_width = round(interval_width, 3)
                        ))
                    }
                }

                return(result_df)

            }, error = function(e) {
                return(data.frame())
            })
        },

        # Enhanced data splitting with multiple strategies
        .enhanced_data_split = function(data, target_var, target_level, test_split = 0.3) {
            split_method <- self$options$data_split_method
            stratified <- self$options$stratified_sampling

            tryCatch({
                set.seed(self$options$seed_value)
                n_total <- nrow(data)

                if (split_method == "random") {
                    # Simple random split
                    train_indices <- sample(n_total, round((1 - test_split) * n_total))
                    test_indices <- setdiff(1:n_total, train_indices)

                } else if (split_method == "stratified" || stratified) {
                    # Stratified split maintaining class proportions
                    if (requireNamespace("caret", quietly = TRUE)) {
                        train_indices <- caret::createDataPartition(
                            y = data[[target_var]],
                            p = 1 - test_split,
                            list = FALSE
                        )[, 1]
                        test_indices <- setdiff(1:n_total, train_indices)
                    } else {
                        # Fallback manual stratification
                        classes <- unique(data[[target_var]])
                        train_indices <- c()

                        for (class in classes) {
                            class_indices <- which(data[[target_var]] == class)
                            n_train_class <- round((1 - test_split) * length(class_indices))
                            class_train <- sample(class_indices, n_train_class)
                            train_indices <- c(train_indices, class_train)
                        }
                        test_indices <- setdiff(1:n_total, train_indices)
                    }

                } else if (split_method == "balanced") {
                    # Balanced split with equal class sizes in training
                    classes <- unique(data[[target_var]])
                    min_class_size <- min(table(data[[target_var]]))
                    train_size_per_class <- round(min_class_size * (1 - test_split))

                    train_indices <- c()
                    for (class in classes) {
                        class_indices <- which(data[[target_var]] == class)
                        class_train <- sample(class_indices, train_size_per_class)
                        train_indices <- c(train_indices, class_train)
                    }
                    test_indices <- setdiff(1:n_total, train_indices)

                } else if (split_method == "temporal") {
                    # Chronological split (assumes data is ordered by time)
                    split_point <- round((1 - test_split) * n_total)
                    train_indices <- 1:split_point
                    test_indices <- (split_point + 1):n_total

                } else {
                    # Default to stratified
                    if (requireNamespace("caret", quietly = TRUE)) {
                        train_indices <- caret::createDataPartition(
                            y = data[[target_var]],
                            p = 1 - test_split,
                            list = FALSE
                        )[, 1]
                        test_indices <- setdiff(1:n_total, train_indices)
                    } else {
                        train_indices <- sample(n_total, round((1 - test_split) * n_total))
                        test_indices <- setdiff(1:n_total, train_indices)
                    }
                }

                return(list(
                    train_data = data[train_indices, ],
                    test_data = data[test_indices, ],
                    train_indices = train_indices,
                    test_indices = test_indices
                ))

            }, error = function(e) {
                # Fallback to simple random split
                train_indices <- sample(n_total, round((1 - test_split) * n_total))
                test_indices <- setdiff(1:n_total, train_indices)

                return(list(
                    train_data = data[train_indices, ],
                    test_data = data[test_indices, ],
                    train_indices = train_indices,
                    test_indices = test_indices
                ))
            })
        },

        # Advanced cross-validation with trainControl integration
        .advanced_cross_validation = function(model_formula, data, algorithm) {
            validation_method <- self$options$validation
            cv_folds <- self$options$cv_folds
            cv_repeats <- self$options$cv_repeats
            target_var <- self$options$target

            tryCatch({
                set.seed(self$options$seed_value)

                # Set up trainControl based on validation method
                if (validation_method == "cv") {
                    if (requireNamespace("caret", quietly = TRUE)) {
                        train_control <- caret::trainControl(
                            method = "cv",
                            number = cv_folds,
                            savePredictions = TRUE,
                            classProbs = TRUE,
                            summaryFunction = caret::twoClassSummary,
                            verboseIter = FALSE
                        )
                    }
                } else if (validation_method == "stratified_cv") {
                    if (requireNamespace("caret", quietly = TRUE)) {
                        train_control <- caret::trainControl(
                            method = "cv",
                            number = cv_folds,
                            savePredictions = TRUE,
                            classProbs = TRUE,
                            summaryFunction = caret::twoClassSummary,
                            verboseIter = FALSE,
                            sampling = "up"  # Up-sampling for stratification
                        )
                    }
                } else if (validation_method == "repeated_cv") {
                    if (requireNamespace("caret", quietly = TRUE)) {
                        train_control <- caret::trainControl(
                            method = "repeatedcv",
                            number = cv_folds,
                            repeats = cv_repeats,
                            savePredictions = TRUE,
                            classProbs = TRUE,
                            summaryFunction = caret::twoClassSummary,
                            verboseIter = FALSE
                        )
                    }
                } else {
                    # Fallback to simple CV
                    if (requireNamespace("caret", quietly = TRUE)) {
                        train_control <- caret::trainControl(
                            method = "cv",
                            number = cv_folds,
                            savePredictions = TRUE,
                            classProbs = TRUE,
                            verboseIter = FALSE
                        )
                    }
                }

                # Perform advanced cross-validation with caret
                if (requireNamespace("caret", quietly = TRUE) && algorithm == "rpart") {
                    # For rpart, use caret's train function with CP tuning
                    cv_model <- caret::train(
                        model_formula,
                        data = data,
                        method = "rpart",
                        trControl = train_control,
                        tuneLength = 10,  # Try 10 different CP values
                        metric = "ROC"
                    )

                    # Extract detailed CV results
                    cv_results <- cv_model$resample

                    # Calculate summary statistics
                    summary_stats <- data.frame(
                        Metric = c("ROC", "Sensitivity", "Specificity"),
                        Mean = c(mean(cv_results$ROC, na.rm = TRUE),
                               mean(cv_results$Sens, na.rm = TRUE),
                               mean(cv_results$Spec, na.rm = TRUE)),
                        SD = c(sd(cv_results$ROC, na.rm = TRUE),
                              sd(cv_results$Sens, na.rm = TRUE),
                              sd(cv_results$Spec, na.rm = TRUE)),
                        Min = c(min(cv_results$ROC, na.rm = TRUE),
                               min(cv_results$Sens, na.rm = TRUE),
                               min(cv_results$Spec, na.rm = TRUE)),
                        Max = c(max(cv_results$ROC, na.rm = TRUE),
                               max(cv_results$Sens, na.rm = TRUE),
                               max(cv_results$Spec, na.rm = TRUE))
                    )

                    return(list(
                        cv_model = cv_model,
                        cv_results = cv_results,
                        summary_stats = summary_stats,
                        best_tune = cv_model$bestTune,
                        method = validation_method
                    ))
                } else {
                    # Manual cross-validation for other algorithms
                    return(private$.manual_cross_validation(model_formula, data, algorithm, cv_folds, cv_repeats))
                }

            }, error = function(e) {
                # Fallback to simple manual CV
                return(private$.manual_cross_validation(model_formula, data, algorithm, cv_folds, 1))
            })
        },

        # Manual cross-validation implementation
        .manual_cross_validation = function(model_formula, data, algorithm, k_folds = 5, repeats = 1) {
            set.seed(self$options$seed_value)
            target_var <- self$options$target
            target_level <- self$options$targetLevel

            all_results <- data.frame()

            for (rep in 1:repeats) {
                # Create folds
                folds <- caret::createFolds(data[[target_var]], k = k_folds, list = TRUE)

                fold_results <- data.frame()

                for (i in 1:k_folds) {
                    private$.checkpoint()  # Add checkpoint before expensive repeated CV fold
                    
                    test_indices <- folds[[i]]
                    train_indices <- setdiff(1:nrow(data), test_indices)

                    train_fold <- data[train_indices, ]
                    test_fold <- data[test_indices, ]

                    tryCatch({
                        # Train model on fold
                        if (algorithm == "rpart") {
                            fold_model <- rpart::rpart(model_formula, data = train_fold)
                        } else if (algorithm == "randomforest") {
                            fold_model <- randomForest::randomForest(model_formula, data = train_fold)
                        } else {
                            fold_model <- rpart::rpart(model_formula, data = train_fold)
                        }

                        # Make predictions
                        if (self$options$tree_mode == "classification") {
                            pred_prob <- predict(fold_model, test_fold, type = "prob")
                            pred_class <- predict(fold_model, test_fold, type = "class")

                            if (is.matrix(pred_prob) && ncol(pred_prob) >= 2) {
                                if (!is.null(target_level) && target_level %in% colnames(pred_prob)) {
                                    prob_positive <- pred_prob[, target_level]
                                } else {
                                    prob_positive <- pred_prob[, 2]
                                }
                            } else {
                                prob_positive <- as.numeric(pred_class == target_level)
                            }

                            # Calculate metrics
                            if (requireNamespace("pROC", quietly = TRUE)) {
                                roc_obj <- pROC::roc(test_fold[[target_var]], prob_positive, quiet = TRUE)
                                auc_val <- as.numeric(pROC::auc(roc_obj))
                            } else {
                                auc_val <- NA
                            }

                            if (requireNamespace("caret", quietly = TRUE)) {
                                cm <- caret::confusionMatrix(pred_class, test_fold[[target_var]], positive = target_level)
                                accuracy <- cm$overall["Accuracy"]
                                sensitivity <- cm$byClass["Sensitivity"]
                                specificity <- cm$byClass["Specificity"]
                            } else {
                                accuracy <- mean(pred_class == test_fold[[target_var]], na.rm = TRUE)
                                sensitivity <- NA
                                specificity <- NA
                            }

                            fold_results <- rbind(fold_results, data.frame(
                                Repeat = rep,
                                Fold = i,
                                Accuracy = accuracy,
                                Sensitivity = sensitivity,
                                Specificity = specificity,
                                AUC = auc_val
                            ))
                        }
                    }, error = function(e) {
                        # Skip this fold if error
                    })
                }

                all_results <- rbind(all_results, fold_results)
            }

            # Calculate summary statistics
            if (nrow(all_results) > 0) {
                summary_stats <- data.frame(
                    Metric = c("Accuracy", "Sensitivity", "Specificity", "AUC"),
                    Mean = c(mean(all_results$Accuracy, na.rm = TRUE),
                           mean(all_results$Sensitivity, na.rm = TRUE),
                           mean(all_results$Specificity, na.rm = TRUE),
                           mean(all_results$AUC, na.rm = TRUE)),
                    SD = c(sd(all_results$Accuracy, na.rm = TRUE),
                          sd(all_results$Sensitivity, na.rm = TRUE),
                          sd(all_results$Specificity, na.rm = TRUE),
                          sd(all_results$AUC, na.rm = TRUE)),
                    Min = c(min(all_results$Accuracy, na.rm = TRUE),
                           min(all_results$Sensitivity, na.rm = TRUE),
                           min(all_results$Specificity, na.rm = TRUE),
                           min(all_results$AUC, na.rm = TRUE)),
                    Max = c(max(all_results$Accuracy, na.rm = TRUE),
                           max(all_results$Sensitivity, na.rm = TRUE),
                           max(all_results$Specificity, na.rm = TRUE),
                           max(all_results$AUC, na.rm = TRUE))
                )

                return(list(
                    cv_results = all_results,
                    summary_stats = summary_stats,
                    method = "manual_cv"
                ))
            } else {
                return(NULL)
            }
        },

        # Xerror-based automatic tree pruning
        .xerror_based_pruning = function(model) {
            if (!inherits(model, "rpart") || is.null(model$cptable)) {
                return(model)  # Return original model if not rpart or no CP table
            }

            tryCatch({
                cp_table <- model$cptable
                xerror <- cp_table[, "xerror"]
                xerror_std <- cp_table[, "xstd"]

                prune_method <- self$options$prune_method

                if (prune_method == "min_xerror") {
                    # Select CP with minimum xerror
                    best_cp_idx <- which.min(xerror)
                    best_cp <- cp_table[best_cp_idx, "CP"]

                } else if (prune_method == "one_se") {
                    # 1-SE rule: most parsimonious tree within 1 SE of minimum xerror
                    min_xerror_idx <- which.min(xerror)
                    min_xerror <- xerror[min_xerror_idx]
                    min_se <- xerror_std[min_xerror_idx]

                    # Find first tree with xerror <= min_xerror + 1*SE
                    threshold <- min_xerror + min_se
                    best_cp_idx <- which(xerror <= threshold)[1]
                    best_cp <- cp_table[best_cp_idx, "CP"]

                } else {
                    # Custom CP or fallback to original model
                    best_cp <- self$options$cost_complexity
                }

                # Prune the tree
                pruned_model <- rpart::prune(model, cp = best_cp)

                # Add pruning information to model
                pruned_model$pruning_info <- list(
                    method = prune_method,
                    original_cp = model$control$cp,
                    pruned_cp = best_cp,
                    original_size = nrow(model$frame),
                    pruned_size = nrow(pruned_model$frame),
                    xerror_improvement = min(xerror) - xerror[min_xerror_idx]
                )

                return(pruned_model)

            }, error = function(e) {
                return(model)  # Return original if pruning fails
            })
        },

        # Enhanced confusion matrix with caret integration
        .enhanced_confusion_matrix = function(actual, predicted, target_level) {
            if (!requireNamespace("caret", quietly = TRUE)) {
                # Fallback to basic confusion matrix
                cm_table <- table(Predicted = predicted, Actual = actual)
                return(list(table = cm_table, enhanced = FALSE))
            }

            tryCatch({
                # Use caret's enhanced confusion matrix
                cm <- caret::confusionMatrix(
                    data = predicted,
                    reference = actual,
                    positive = target_level,
                    mode = "everything"  # Include all statistics
                )

                # Extract comprehensive statistics
                overall_stats <- cm$overall
                class_stats <- cm$byClass

                # Create clinical interpretation
                sensitivity <- class_stats["Sensitivity"]
                specificity <- class_stats["Specificity"]
                ppv <- class_stats["Pos Pred Value"]
                npv <- class_stats["Neg Pred Value"]

                # Calculate additional clinical metrics
                prevalence <- class_stats["Prevalence"]
                detection_rate <- class_stats["Detection Rate"]
                detection_prevalence <- class_stats["Detection Prevalence"]

                # Likelihood ratios
                lr_positive <- sensitivity / (1 - specificity)
                lr_negative <- (1 - sensitivity) / specificity

                clinical_metrics <- list(
                    sensitivity = sensitivity,
                    specificity = specificity,
                    ppv = ppv,
                    npv = npv,
                    prevalence = prevalence,
                    lr_positive = lr_positive,
                    lr_negative = lr_negative,
                    detection_rate = detection_rate,
                    detection_prevalence = detection_prevalence,
                    accuracy = overall_stats["Accuracy"],
                    kappa = overall_stats["Kappa"],
                    mcnemar_p = overall_stats["McnemarPValue"]
                )

                return(list(
                    cm = cm,
                    table = cm$table,
                    overall = overall_stats,
                    byClass = class_stats,
                    clinical = clinical_metrics,
                    enhanced = TRUE
                ))

            }, error = function(e) {
                # Fallback to basic confusion matrix
                cm_table <- table(Predicted = predicted, Actual = actual)
                return(list(table = cm_table, enhanced = FALSE, error = e$message))
            })
        },

        # Advanced pruning analysis
        .advanced_pruning_analysis = function() {
            model_results <- self$results$model_results
            if (is.null(model_results)) {
                return(NULL)
            }

            model <- model_results$model
            data <- self$data
            target_col <- self$options$target

            if (is.null(target_col) || target_col == "" || !inherits(model, "rpart")) {
                return("Advanced pruning analysis requires a trained rpart model.")
            }

            pruning_method <- self$options$advanced_pruning
            validation_split <- self$options$pruning_validation_split

            tryCatch({
                # Split data for pruning validation
                set.seed(self$options$seed_value)
                n_total <- nrow(data)
                val_indices <- sample(n_total, round(validation_split * n_total))
                train_data <- data[-val_indices, ]
                val_data <- data[val_indices, ]

                # Train full tree on training data
                full_tree <- rpart::rpart(
                    formula = self$formula,
                    data = train_data,
                    method = ifelse(self$options$tree_mode == "regression", "anova", "class"),
                    control = rpart::rpart.control(
                        maxdepth = self$options$max_depth,
                        minsplit = self$options$min_samples_split,
                        minbucket = self$options$min_samples_leaf,
                        cp = 0  # Grow full tree
                    )
                )

                if (pruning_method == "rep") {
                    return(private$.reduced_error_pruning(full_tree, train_data, val_data))
                } else if (pruning_method == "mep") {
                    return(private$.minimum_error_pruning(full_tree, train_data, val_data))
                }

                return("Unknown pruning method selected.")

            }, error = function(e) {
                return(paste("Advanced pruning analysis failed:", e$message))
            })
        },

        # Reduced Error Pruning implementation
        .reduced_error_pruning = function(full_tree, train_data, val_data) {
            # Get all possible subtrees by pruning
            cp_table <- full_tree$cptable

            best_cp <- NULL
            best_error <- Inf
            pruning_results <- data.frame()

            # Test different CP values
            for (i in 1:nrow(cp_table)) {
                cp_val <- cp_table[i, "CP"]

                # Prune tree with this CP
                pruned_tree <- rpart::prune(full_tree, cp = cp_val)

                # Evaluate on validation set
                if (self$options$tree_mode == "regression") {
                    val_pred <- predict(pruned_tree, val_data)
                    val_error <- mean((val_data[[self$options$target]] - val_pred)^2, na.rm = TRUE)
                } else {
                    val_pred <- predict(pruned_tree, val_data, type = "class")
                    val_error <- mean(val_pred != val_data[[self$options$target]], na.rm = TRUE)
                }

                # Store results
                pruning_results <- rbind(pruning_results, data.frame(
                    cp = cp_val,
                    n_nodes = length(unique(pruned_tree$where)),
                    validation_error = val_error,
                    tree_size = nrow(pruned_tree$frame)
                ))

                if (val_error < best_error) {
                    best_error <- val_error
                    best_cp <- cp_val
                }
            }

            html <- paste0("
            <div style='margin: 20px 0;'>
            <h4 style='color: #1976d2;'>Reduced Error Pruning (REP) Analysis</h4>

            <h5 style='color: #2c5aa0;'>Method Overview:</h5>
            <p>REP uses a separate validation set to evaluate different tree sizes and selects
            the tree with minimum validation error. This provides unbiased pruning decisions.</p>

            <h5 style='color: #2c5aa0;'>Optimal Pruning Results:</h5>
            <ul>
            <li><strong>Best CP value:</strong> ", round(best_cp, 6), "</li>
            <li><strong>Validation error:</strong> ", round(best_error, 4), "</li>
            <li><strong>Validation set size:</strong> ", nrow(val_data), " observations</li>
            </ul>

            <h5 style='color: #2c5aa0;'>Pruning Sequence:</h5>
            <table style='border-collapse: collapse; margin: 10px 0;'>
            <tr style='background-color: #f5f5f5;'>
            <th style='padding: 5px; border: 1px solid #ddd;'>CP Value</th>
            <th style='padding: 5px; border: 1px solid #ddd;'>Tree Size</th>
            <th style='padding: 5px; border: 1px solid #ddd;'>Validation Error</th>
            <th style='padding: 5px; border: 1px solid #ddd;'>Status</th>
            </tr>")

            for (i in 1:nrow(pruning_results)) {
                row <- pruning_results[i, ]
                status <- ifelse(row$cp == best_cp,
                               "<span style='color: #4caf50;'><strong>OPTIMAL</strong></span>",
                               "")

                html <- paste0(html, "
                <tr>
                <td style='padding: 5px; border: 1px solid #ddd;'>", round(row$cp, 6), "</td>
                <td style='padding: 5px; border: 1px solid #ddd;'>", row$tree_size, "</td>
                <td style='padding: 5px; border: 1px solid #ddd;'>", round(row$validation_error, 4), "</td>
                <td style='padding: 5px; border: 1px solid #ddd;'>", status, "</td>
                </tr>")
            }

            html <- paste0(html, "</table>

            <h5 style='color: #2c5aa0;'>Clinical Interpretation:</h5>
            <ul>
            <li><strong>Optimal tree:</strong> Balances complexity and generalization for clinical use</li>
            <li><strong>Validation-based:</strong> Unbiased estimate of performance on new patients</li>
            <li><strong>Interpretability:</strong> Simpler trees are preferred when validation errors are similar</li>
            </ul>
            </div>")

            return(html)
        },

        # Minimum Error Pruning implementation
        .minimum_error_pruning = function(full_tree, train_data, val_data) {
            # MEP considers both error and tree complexity
            cp_table <- full_tree$cptable

            best_cp <- NULL
            best_score <- Inf
            pruning_results <- data.frame()

            # Test different CP values with complexity penalty
            for (i in 1:nrow(cp_table)) {
                cp_val <- cp_table[i, "CP"]

                # Prune tree with this CP
                pruned_tree <- rpart::prune(full_tree, cp = cp_val)

                # Evaluate on validation set
                if (self$options$tree_mode == "regression") {
                    val_pred <- predict(pruned_tree, val_data)
                    val_error <- mean((val_data[[self$options$target]] - val_pred)^2, na.rm = TRUE)
                } else {
                    val_pred <- predict(pruned_tree, val_data, type = "class")
                    val_error <- mean(val_pred != val_data[[self$options$target]], na.rm = TRUE)
                }

                # Calculate MEP score: error + complexity penalty
                tree_size <- nrow(pruned_tree$frame)
                complexity_penalty <- 0.01 * tree_size / max(cp_table[, "nsplit"] + 1)  # Normalized penalty
                mep_score <- val_error + complexity_penalty

                # Store results
                pruning_results <- rbind(pruning_results, data.frame(
                    cp = cp_val,
                    n_nodes = length(unique(pruned_tree$where)),
                    validation_error = val_error,
                    tree_size = tree_size,
                    complexity_penalty = complexity_penalty,
                    mep_score = mep_score
                ))

                if (mep_score < best_score) {
                    best_score <- mep_score
                    best_cp <- cp_val
                }
            }

            optimal_result <- pruning_results[pruning_results$cp == best_cp, ]

            html <- paste0("
            <div style='margin: 20px 0;'>
            <h4 style='color: #1976d2;'>Minimum Error Pruning (MEP) Analysis</h4>

            <h5 style='color: #2c5aa0;'>Method Overview:</h5>
            <p>MEP balances prediction error and tree complexity by adding a penalty for larger trees.
            This encourages clinically interpretable models while maintaining good performance.</p>

            <h5 style='color: #2c5aa0;'>Optimal Pruning Results:</h5>
            <ul>
            <li><strong>Best CP value:</strong> ", round(best_cp, 6), "</li>
            <li><strong>Validation error:</strong> ", round(optimal_result$validation_error, 4), "</li>
            <li><strong>Complexity penalty:</strong> ", round(optimal_result$complexity_penalty, 4), "</li>
            <li><strong>MEP score:</strong> ", round(optimal_result$mep_score, 4), "</li>
            <li><strong>Final tree size:</strong> ", optimal_result$tree_size, " nodes</li>
            </ul>

            <h5 style='color: #2c5aa0;'>Pruning Sequence:</h5>
            <table style='border-collapse: collapse; margin: 10px 0;'>
            <tr style='background-color: #f5f5f5;'>
            <th style='padding: 5px; border: 1px solid #ddd;'>CP Value</th>
            <th style='padding: 5px; border: 1px solid #ddd;'>Tree Size</th>
            <th style='padding: 5px; border: 1px solid #ddd;'>Val Error</th>
            <th style='padding: 5px; border: 1px solid #ddd;'>Complexity</th>
            <th style='padding: 5px; border: 1px solid #ddd;'>MEP Score</th>
            <th style='padding: 5px; border: 1px solid #ddd;'>Status</th>
            </tr>")

            for (i in 1:nrow(pruning_results)) {
                row <- pruning_results[i, ]
                status <- ifelse(row$cp == best_cp,
                               "<span style='color: #4caf50;'><strong>OPTIMAL</strong></span>",
                               "")

                html <- paste0(html, "
                <tr>
                <td style='padding: 5px; border: 1px solid #ddd;'>", round(row$cp, 6), "</td>
                <td style='padding: 5px; border: 1px solid #ddd;'>", row$tree_size, "</td>
                <td style='padding: 5px; border: 1px solid #ddd;'>", round(row$validation_error, 4), "</td>
                <td style='padding: 5px; border: 1px solid #ddd;'>", round(row$complexity_penalty, 4), "</td>
                <td style='padding: 5px; border: 1px solid #ddd;'>", round(row$mep_score, 4), "</td>
                <td style='padding: 5px; border: 1px solid #ddd;'>", status, "</td>
                </tr>")
            }

            html <- paste0(html, "</table>

            <h5 style='color: #2c5aa0;'>Clinical Interpretation:</h5>
            <ul>
            <li><strong>Balanced approach:</strong> Considers both accuracy and interpretability for clinical use</li>
            <li><strong>Complexity control:</strong> Prevents overly complex trees that are hard to use clinically</li>
            <li><strong>Optimal trade-off:</strong> Achieves good performance with manageable complexity</li>
            </ul>
            </div>")

            return(html)
        },

        # Generate enhanced CV results
        .generate_enhanced_cv_results = function() {
            model_results <- self$results$model_results
            if (is.null(model_results) || is.null(model_results$cv_advanced)) {
                return("Enhanced cross-validation results not available.")
            }

            cv_results <- model_results$cv_advanced
            summary_stats <- cv_results$summary_stats
            method <- cv_results$method

            html <- paste0("
            <div style='margin: 20px 0;'>
            <h4 style='color: #1976d2;'>Enhanced Cross-Validation Analysis</h4>
            <p><strong>Method:</strong> ", toupper(gsub("_", " ", method)), "</p>

            <h5 style='color: #2c5aa0;'>Performance Summary:</h5>
            <table style='border-collapse: collapse; margin: 10px 0;'>
            <tr style='background-color: #f5f5f5;'>
            <th style='padding: 5px; border: 1px solid #ddd;'>Metric</th>
            <th style='padding: 5px; border: 1px solid #ddd;'>Mean ± SD</th>
            <th style='padding: 5px; border: 1px solid #ddd;'>Range</th>
            <th style='padding: 5px; border: 1px solid #ddd;'>Interpretation</th>
            </tr>")

            for (i in 1:nrow(summary_stats)) {
                metric <- summary_stats$Metric[i]
                mean_val <- summary_stats$Mean[i]
                sd_val <- summary_stats$SD[i]
                min_val <- summary_stats$Min[i]
                max_val <- summary_stats$Max[i]

                # Clinical interpretation
                interpretation <- switch(metric,
                    "Accuracy" = ifelse(mean_val >= 0.8, "Excellent", ifelse(mean_val >= 0.7, "Good", "Fair")),
                    "Sensitivity" = ifelse(mean_val >= 0.8, "High", ifelse(mean_val >= 0.7, "Moderate", "Low")),
                    "Specificity" = ifelse(mean_val >= 0.8, "High", ifelse(mean_val >= 0.7, "Moderate", "Low")),
                    "AUC" = ifelse(mean_val >= 0.8, "Excellent", ifelse(mean_val >= 0.7, "Good", "Fair")),
                    "Good"
                )

                color <- switch(interpretation,
                    "Excellent" = "#4caf50",
                    "High" = "#4caf50",
                    "Good" = "#ff9800",
                    "Moderate" = "#ff9800",
                    "Fair" = "#f44336",
                    "Low" = "#f44336",
                    "#666"
                )

                html <- paste0(html, "
                <tr>
                <td style='padding: 5px; border: 1px solid #ddd;'><strong>", metric, "</strong></td>
                <td style='padding: 5px; border: 1px solid #ddd;'>",
                sprintf("%.3f ± %.3f", mean_val, sd_val), "</td>
                <td style='padding: 5px; border: 1px solid #ddd;'>",
                sprintf("%.3f - %.3f", min_val, max_val), "</td>
                <td style='padding: 5px; border: 1px solid #ddd; color: ", color, ";'><strong>", interpretation, "</strong></td>
                </tr>")
            }

            html <- paste0(html, "</table>

            <h5 style='color: #2c5aa0;'>Clinical Interpretation:</h5>
            <ul>
            <li><strong>Stability:</strong> Low standard deviation indicates consistent performance across folds</li>
            <li><strong>Generalization:</strong> Small range suggests good generalization to new patients</li>
            <li><strong>Clinical Utility:</strong> High sensitivity/specificity balance needed for medical decisions</li>
            </ul>
            </div>")

            return(html)
        },

        # Generate enhanced CV table
        .generate_enhanced_cv_table = function() {
            model_results <- self$results$model_results
            if (is.null(model_results) || is.null(model_results$cv_advanced)) {
                return(data.frame())
            }

            summary_stats <- model_results$cv_advanced$summary_stats

            # Calculate 95% confidence intervals
            ci_data <- data.frame()
            for (i in 1:nrow(summary_stats)) {
                mean_val <- summary_stats$Mean[i]
                sd_val <- summary_stats$SD[i]
                se <- sd_val / sqrt(self$options$cv_folds * self$options$cv_repeats)  # Approx SE

                ci_lower <- mean_val - 1.96 * se
                ci_upper <- mean_val + 1.96 * se

                ci_data <- rbind(ci_data, data.frame(
                    metric = summary_stats$Metric[i],
                    mean = mean_val,
                    sd = sd_val,
                    min = summary_stats$Min[i],
                    max = summary_stats$Max[i],
                    ci_lower = ci_lower,
                    ci_upper = ci_upper
                ))
            }

            return(ci_data)
        },

        # Generate enhanced confusion matrix results
        .generate_enhanced_confusion_matrix_results = function() {
            model_results <- self$results$model_results
            if (is.null(model_results)) {
                return("Enhanced confusion matrix results not available.")
            }

            # Get predictions for enhanced confusion matrix
            actual <- model_results$actual
            predicted <- model_results$predictions_class
            target_level <- self$options$targetLevel

            if (is.null(actual) || is.null(predicted)) {
                return("Prediction data not available for enhanced confusion matrix.")
            }

            enhanced_cm <- private$.enhanced_confusion_matrix(actual, predicted, target_level)

            if (!enhanced_cm$enhanced) {
                return("Enhanced confusion matrix requires caret package.")
            }

            clinical <- enhanced_cm$clinical

            html <- paste0("
            <div style='margin: 20px 0;'>
            <h4 style='color: #1976d2;'>Clinical Confusion Matrix Analysis</h4>

            <h5 style='color: #2c5aa0;'>Clinical Performance Metrics:</h5>
            <table style='border-collapse: collapse; margin: 10px 0;'>
            <tr style='background-color: #f5f5f5;'>
            <th style='padding: 8px; border: 1px solid #ddd;'>Clinical Metric</th>
            <th style='padding: 8px; border: 1px solid #ddd;'>Value</th>
            <th style='padding: 8px; border: 1px solid #ddd;'>Clinical Interpretation</th>
            </tr>
            <tr>
            <td style='padding: 8px; border: 1px solid #ddd;'><strong>Sensitivity (True Positive Rate)</strong></td>
            <td style='padding: 8px; border: 1px solid #ddd;'>", sprintf("%.3f", clinical$sensitivity), "</td>
            <td style='padding: 8px; border: 1px solid #ddd;'>Ability to correctly identify positive cases</td>
            </tr>
            <tr>
            <td style='padding: 8px; border: 1px solid #ddd;'><strong>Specificity (True Negative Rate)</strong></td>
            <td style='padding: 8px; border: 1px solid #ddd;'>", sprintf("%.3f", clinical$specificity), "</td>
            <td style='padding: 8px; border: 1px solid #ddd;'>Ability to correctly identify negative cases</td>
            </tr>
            <tr>
            <td style='padding: 8px; border: 1px solid #ddd;'><strong>Positive Predictive Value (PPV)</strong></td>
            <td style='padding: 8px; border: 1px solid #ddd;'>", sprintf("%.3f", clinical$ppv), "</td>
            <td style='padding: 8px; border: 1px solid #ddd;'>Probability that positive prediction is correct</td>
            </tr>
            <tr>
            <td style='padding: 8px; border: 1px solid #ddd;'><strong>Negative Predictive Value (NPV)</strong></td>
            <td style='padding: 8px; border: 1px solid #ddd;'>", sprintf("%.3f", clinical$npv), "</td>
            <td style='padding: 8px; border: 1px solid #ddd;'>Probability that negative prediction is correct</td>
            </tr>")

            if (!is.na(clinical$lr_positive) && !is.na(clinical$lr_negative)) {
                html <- paste0(html, "
                <tr>
                <td style='padding: 8px; border: 1px solid #ddd;'><strong>Positive Likelihood Ratio</strong></td>
                <td style='padding: 8px; border: 1px solid #ddd;'>", sprintf("%.2f", clinical$lr_positive), "</td>
                <td style='padding: 8px; border: 1px solid #ddd;'>How much positive test increases disease probability</td>
                </tr>
                <tr>
                <td style='padding: 8px; border: 1px solid #ddd;'><strong>Negative Likelihood Ratio</strong></td>
                <td style='padding: 8px; border: 1px solid #ddd;'>", sprintf("%.2f", clinical$lr_negative), "</td>
                <td style='padding: 8px; border: 1px solid #ddd;'>How much negative test decreases disease probability</td>
                </tr>")
            }

            html <- paste0(html, "</table>

            <h5 style='color: #2c5aa0;'>Agreement Statistics:</h5>
            <ul>
            <li><strong>Cohen's Kappa:</strong> ", sprintf("%.3f", clinical$kappa), " - Inter-rater agreement measure</li>
            <li><strong>McNemar's p-value:</strong> ", sprintf("%.4f", clinical$mcnemar_p), " - Test of symmetry in errors</li>
            <li><strong>Prevalence in sample:</strong> ", sprintf("%.1f%%", clinical$prevalence * 100), "</li>
            </ul>

            <h5 style='color: #2c5aa0;'>Clinical Guidelines:</h5>
            <ul>
            <li><strong>High Sensitivity (>90%):</strong> Good for screening tests - few false negatives</li>
            <li><strong>High Specificity (>90%):</strong> Good for confirmatory tests - few false positives</li>
            <li><strong>High PPV:</strong> Most positive predictions are correct (important in low-prevalence settings)</li>
            <li><strong>High NPV:</strong> Most negative predictions are correct (important for ruling out disease)</li>
            </ul>
            </div>")

            return(html)
        },

        # Smooth progress update helper for long-running operations
        .update_progress_smooth = function(start_percent, end_percent, steps, base_message, operation_name) {
            step_size <- (end_percent - start_percent) / steps
            for (i in 1:steps) {
                current_percent <- start_percent + (i * step_size)
                step_message <- paste0(base_message, " (", i, "/", steps, ")")
                private$.update_progress(paste0(operation_name, "_step_", i), step_message, current_percent)
                # Small delay to show progression (minimal performance impact)
                Sys.sleep(0.05)  # 50ms delay per step
            }
        },

        # Progress feedback system
        .update_progress = function(stage, message, percent = NULL, details = NULL) {
            timestamp <- format(Sys.time(), "%H:%M:%S")

            # Define stage-specific styling and icons
            stage_info <- list(
                "initializing" = list(icon = "🔧", color = "#2196F3", bg = "#e3f2fd"),
                "data_prep" = list(icon = "📊", color = "#4CAF50", bg = "#e8f5e8"),
                "training" = list(icon = "🎯", color = "#FF9800", bg = "#fff3e0"),
                "hyperparameter_tuning" = list(icon = "⚙️", color = "#9C27B0", bg = "#f3e5f5"),
                "validation" = list(icon = "✅", color = "#4CAF50", bg = "#e8f5e8"),
                "model_comparison" = list(icon = "📈", color = "#607D8B", bg = "#eceff1"),
                "generating_results" = list(icon = "📋", color = "#795548", bg = "#efebe9"),
                "completed" = list(icon = "✨", color = "#4CAF50", bg = "#e8f5e8"),
                "error" = list(icon = "⚠️", color = "#f44336", bg = "#ffebee")
            )

            info <- stage_info[[stage]] %||% list(icon = "ℹ️", color = "#2196F3", bg = "#e3f2fd")

            # Build progress bar HTML if percent is provided
            progress_bar <- ""
            if (!is.null(percent)) {
                # Use enhanced SVG progress bar with consistent styling
                progress_bar <- treeProgressBar(
                    current = percent,
                    total = 100,
                    message = paste0("Progress: ", round(percent, 1), "%"),
                    width = 520,
                    height = 40
                )
            }

            # Build details section if provided
            details_html <- ""
            if (!is.null(details)) {
                details_html <- paste0("
                <div style='margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; font-size: 13px;'>
                <strong>Details:</strong> ", details, "
                </div>")
            }

            # Create the complete progress HTML
            progress_html <- paste0("
            <div style='background-color: ", info$bg, "; border-left: 4px solid ", info$color, "; padding: 15px; border-radius: 8px; margin: 10px 0;'>
                <div style='display: flex; align-items: center; margin-bottom: 8px;'>
                    <span style='font-size: 20px; margin-right: 10px;'>", info$icon, "</span>
                    <div>
                        <strong style='color: ", info$color, ";'>", message, "</strong>
                        <div style='font-size: 12px; color: #666; margin-top: 2px;'>", timestamp, "</div>
                    </div>
                </div>
                ", progress_bar, "
                ", details_html, "
            </div>")

            # Update the progress feedback result and trigger UI update
            self$results$progress_feedback$setContent(progress_html)
            private$.checkpoint()  # Ensure real-time UI updates for progress

            # For error messages, also update the main status
            if (stage == "error") {
                error_html <- paste0("
                <div style='color: #d32f2f; background-color: #ffebee; padding: 15px; border-radius: 8px; border-left: 4px solid #d32f2f;'>
                    <h4 style='margin-top: 0; color: #d32f2f;'>", info$icon, " Analysis Error</h4>
                    <p><strong>", message, "</strong></p>
                    ", if (!is.null(details)) paste0("<p style='font-size: 13px; margin-top: 10px;'>", details, "</p>") else "", "
                </div>")
                self$results$text1$setContent(error_html)
            }
        },

        # Update progress with algorithm-specific information
        .update_algorithm_progress = function(algorithm, stage, step_current = NULL, step_total = NULL) {
            algo_names <- list(
                "rpart" = "Enhanced CART",
                "fftrees" = "FFTrees",
                "randomforest" = "Random Forest",
                "c50" = "C5.0 Trees",
                "ctree" = "Conditional Inference Trees",
                "xgboost" = "XGBoost",
                "mob" = "Model-based Trees"
            )

            display_name <- algo_names[[algorithm]] %||% algorithm

            if (stage == "training") {
                message <- paste0("Training ", display_name, " model...")
                details <- paste0("Algorithm: ", display_name, " | Mode: ", self$options$tree_mode)
                
                # Use smooth progress updates for model training (40% to 60%)
                private$.update_progress_smooth(40, 60, 8, paste0("Training ", display_name), "training")
                return()  # Return early since smooth progress was handled
            } else if (stage == "hyperparameter_tuning") {
                percent <- if (!is.null(step_current) && !is.null(step_total)) {
                    round((step_current / step_total) * 100)
                } else NULL

                message <- paste0("Optimizing ", display_name, " hyperparameters...")
                details <- if (!is.null(step_current) && !is.null(step_total)) {
                    paste0("Testing parameter combination ", step_current, " of ", step_total)
                } else {
                    paste0("Running grid search for optimal parameters")
                }

                private$.update_progress("hyperparameter_tuning", message, percent, details)
                return()
            } else if (stage == "validation") {
                message <- paste0("Validating ", display_name, " performance...")
                details <- paste0("Method: ", toupper(gsub("_", " ", self$options$validation)))
                percent <- 75  # Validation is about 3/4 through the process
            } else {
                message <- paste0("Processing ", display_name, "...")
                details <- NULL
                percent <- NULL
            }

            private$.update_progress(stage, message, percent, details)
        },

        # Update progress for model comparison
        .update_comparison_progress = function(current_algo, total_algos, algorithm_name) {
            percent <- round((current_algo / total_algos) * 100)
            message <- "Comparing multiple algorithms..."
            details <- paste0("Currently evaluating: ", algorithm_name, " (", current_algo, " of ", total_algos, ")")

            private$.update_progress("model_comparison", message, percent, details)
        },

        # Generate competing splits analysis
        .generate_competing_splits_analysis = function(model_results) {
            if (is.null(model_results) || is.null(model_results$model)) {
                return(NULL)
            }

            model <- model_results$model
            if (!inherits(model, "rpart")) {
                return(NULL)
            }

            tryCatch({
                # Get competing splits information
                if (is.null(model$splits) || nrow(model$splits) == 0) {
                    return("<div style='padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px;'>
                    <p><strong>No competing splits information available.</strong></p>
                    <p>Increase 'maxcompete' parameter to save competing split information.</p>
                    </div>")
                }

                splits_df <- as.data.frame(model$splits)
                splits_df$node <- row.names(splits_df)

                # Create HTML table
                html <- paste0("
                <div style='padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px;'>
                <h4 style='color: #2c5aa0; margin-top: 0;'>Competing Splits Analysis</h4>
                <p>Shows alternative splitting criteria at each node and their relative quality.</p>

                <table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>
                <tr style='background-color: #e9ecef; border: 1px solid #ddd;'>
                <th style='padding: 8px; border: 1px solid #ddd;'>Node</th>
                <th style='padding: 8px; border: 1px solid #ddd;'>Variable</th>
                <th style='padding: 8px; border: 1px solid #ddd;'>Split Point</th>
                <th style='padding: 8px; border: 1px solid #ddd;'>Count</th>
                <th style='padding: 8px; border: 1px solid #ddd;'>Improvement</th>
                </tr>")

                for (i in 1:min(nrow(splits_df), 20)) {  # Limit to 20 rows for display
                    html <- paste0(html, "
                    <tr style='border: 1px solid #ddd;'>
                    <td style='padding: 8px; border: 1px solid #ddd;'>", splits_df$node[i], "</td>
                    <td style='padding: 8px; border: 1px solid #ddd;'>", row.names(splits_df)[i], "</td>
                    <td style='padding: 8px; border: 1px solid #ddd;'>", round(splits_df$index[i], 3), "</td>
                    <td style='padding: 8px; border: 1px solid #ddd;'>", splits_df$count[i], "</td>
                    <td style='padding: 8px; border: 1px solid #ddd;'>", round(splits_df$improve[i], 4), "</td>
                    </tr>")
                }

                html <- paste0(html, "
                </table>

                <h5 style='color: #2c5aa0;'>Interpretation:</h5>
                <ul>
                <li><strong>Improvement:</strong> Higher values indicate better splitting criteria</li>
                <li><strong>Count:</strong> Number of observations at the split</li>
                <li><strong>Split Point:</strong> Threshold value for the split</li>
                </ul>
                </div>")

                return(html)

            }, error = function(e) {
                return(paste0("<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px;'>
                <p><strong>Error generating competing splits analysis:</strong></p>
                <p>", e$message, "</p>
                </div>"))
            })
        },

        # Generate surrogate splits analysis
        .generate_surrogate_splits_analysis = function(model_results) {
            if (is.null(model_results) || is.null(model_results$model)) {
                return(NULL)
            }

            model <- model_results$model
            if (!inherits(model, "rpart")) {
                return(NULL)
            }

            tryCatch({
                # Get surrogate splits information
                if (is.null(model$splits) || nrow(model$splits) == 0) {
                    return("<div style='padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px;'>
                    <p><strong>No surrogate splits information available.</strong></p>
                    <p>Increase 'maxsurrogate' parameter to save surrogate split information.</p>
                    </div>")
                }

                # Extract surrogate information from model
                frame <- model$frame
                if (is.null(frame)) {
                    return(NULL)
                }

                # Create HTML content
                html <- paste0("
                <div style='padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px;'>
                <h4 style='color: #2c5aa0; margin-top: 0;'>Surrogate Splits Analysis</h4>
                <p>Shows backup splitting rules used when primary variables have missing values.</p>

                <h5 style='color: #2c5aa0;'>Model Configuration:</h5>
                <ul>
                <li><strong>Use surrogates:</strong> ", ifelse(model$control$usesurrogate > 0, "Yes", "No"), "</li>
                <li><strong>Max surrogates:</strong> ", model$control$maxsurrogate, "</li>
                <li><strong>Surrogate usage rule:</strong> ",
                switch(as.character(model$control$usesurrogate),
                       "0" = "Only if helpful",
                       "1" = "Always when available",
                       "2" = "If better than majority rule",
                       "Unknown"), "</li>
                </ul>")

                # Check for missing data handling
                if (any(is.na(model$model))) {
                    html <- paste0(html, "
                    <h5 style='color: #2c5aa0;'>Missing Data Handling:</h5>
                    <p>✓ Missing values detected - surrogate splits are being used for robust predictions.</p>")
                } else {
                    html <- paste0(html, "
                    <h5 style='color: #2c5aa0;'>Missing Data Handling:</h5>
                    <p>ℹ No missing values in training data - surrogates available for future predictions.</p>")
                }

                html <- paste0(html, "
                <h5 style='color: #2c5aa0;'>Clinical Benefits:</h5>
                <ul>
                <li><strong>Robust Predictions:</strong> Tree can still make predictions when key variables are missing</li>
                <li><strong>Clinical Flexibility:</strong> Alternative pathways when primary tests unavailable</li>
                <li><strong>Missing Data Tolerance:</strong> Maintains performance with incomplete records</li>
                </ul>
                </div>")

                return(html)

            }, error = function(e) {
                return(paste0("<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px;'>
                <p><strong>Error generating surrogate splits analysis:</strong></p>
                <p>", e$message, "</p>
                </div>"))
            })
        },

        # Generate CP sequence analysis
        .generate_cp_sequence_analysis = function(model_results) {
            if (is.null(model_results) || is.null(model_results$model)) {
                return(NULL)
            }

            model <- model_results$model
            if (!inherits(model, "rpart")) {
                return(NULL)
            }

            tryCatch({
                cp_table <- model$cptable
                if (is.null(cp_table) || nrow(cp_table) == 0) {
                    return(NULL)
                }

                # Apply automatic CP selection based on user preference
                cp_selection_method <- self$options$cp_auto_selection %||% "one_se_rule"

                if (cp_selection_method == "min_xerror") {
                    optimal_idx <- which.min(cp_table[, "xerror"])
                    selection_method_desc <- "Minimum cross-validation error"
                } else if (cp_selection_method == "one_se_rule") {
                    min_xerror <- min(cp_table[, "xerror"])
                    min_se <- cp_table[which.min(cp_table[, "xerror"]), "xstd"]
                    optimal_idx <- which(cp_table[, "xerror"] <= min_xerror + min_se)[1]
                    selection_method_desc <- "1-SE rule (most parsimonious)"
                } else {
                    # Manual selection
                    manual_cp <- self$options$cost_complexity %||% 0.01
                    optimal_idx <- which.min(abs(cp_table[, "CP"] - manual_cp))
                    selection_method_desc <- paste("Manual CP =", manual_cp)
                }

                optimal_cp <- cp_table[optimal_idx, "CP"]
                optimal_xerror <- cp_table[optimal_idx, "xerror"]
                optimal_size <- cp_table[optimal_idx, "nsplit"] + 1

                html <- paste0("
                <div style='padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px;'>
                <h4 style='color: #2c5aa0; margin-top: 0;'>Cost-Complexity Sequence Analysis</h4>
                <p>Shows the complete sequence of nested subtrees generated by cost-complexity pruning.</p>

                <h5 style='color: #2c5aa0;'>Optimal Tree Selection:</h5>
                <ul>
                <li><strong>Method:</strong> ", selection_method_desc, "</li>
                <li><strong>Selected CP:</strong> ", round(optimal_cp, 6), "</li>
                <li><strong>Cross-validation error:</strong> ", round(optimal_xerror, 4), "</li>
                <li><strong>Tree size:</strong> ", optimal_size, " terminal nodes</li>
                </ul>

                <h5 style='color: #2c5aa0;'>Complete CP Sequence:</h5>
                <table style='border-collapse: collapse; width: 100%; margin: 10px 0; font-size: 12px;'>
                <tr style='background-color: #e9ecef; border: 1px solid #ddd;'>
                <th style='padding: 6px; border: 1px solid #ddd;'>CP</th>
                <th style='padding: 6px; border: 1px solid #ddd;'>Splits</th>
                <th style='padding: 6px; border: 1px solid #ddd;'>Rel Error</th>
                <th style='padding: 6px; border: 1px solid #ddd;'>Xerror</th>
                <th style='padding: 6px; border: 1px solid #ddd;'>Xstd</th>
                <th style='padding: 6px; border: 1px solid #ddd;'>Selection</th>
                </tr>")

                for (i in 1:nrow(cp_table)) {
                    is_optimal <- (i == optimal_idx)
                    row_style <- if (is_optimal) {
                        "background-color: #e3f2fd; border: 1px solid #ddd; font-weight: bold;"
                    } else {
                        "border: 1px solid #ddd;"
                    }

                    selection_status <- if (is_optimal) {
                        "<span style='color: #4caf50;'>★ OPTIMAL</span>"
                    } else {
                        ""
                    }

                    html <- paste0(html, "
                    <tr style='", row_style, "'>
                    <td style='padding: 6px; border: 1px solid #ddd;'>", sprintf("%.6f", cp_table[i, "CP"]), "</td>
                    <td style='padding: 6px; border: 1px solid #ddd;'>", cp_table[i, "nsplit"], "</td>
                    <td style='padding: 6px; border: 1px solid #ddd;'>", sprintf("%.4f", cp_table[i, "rel error"]), "</td>
                    <td style='padding: 6px; border: 1px solid #ddd;'>", sprintf("%.4f", cp_table[i, "xerror"]), "</td>
                    <td style='padding: 6px; border: 1px solid #ddd;'>", sprintf("%.4f", cp_table[i, "xstd"]), "</td>
                    <td style='padding: 6px; border: 1px solid #ddd;'>", selection_status, "</td>
                    </tr>")
                }

                html <- paste0(html, "
                </table>

                <h5 style='color: #2c5aa0;'>Clinical Interpretation:</h5>
                <ul>
                <li><strong>CP (Complexity Parameter):</strong> Controls trade-off between model complexity and fit</li>
                <li><strong>Rel Error:</strong> Training error relative to root node</li>
                <li><strong>Xerror:</strong> Cross-validation error (key for pruning decisions)</li>
                <li><strong>1-SE Rule:</strong> Selects simplest model within 1 standard error of minimum xerror</li>
                </ul>
                </div>")

                return(html)

            }, error = function(e) {
                return(paste0("<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px;'>
                <p><strong>Error generating CP sequence analysis:</strong></p>
                <p>", e$message, "</p>
                </div>"))
            })
        },

        # Generate detailed xerror analysis
        .generate_xerror_detailed_analysis = function(model_results) {
            if (is.null(model_results) || is.null(model_results$model)) {
                return(NULL)
            }

            model <- model_results$model
            if (!inherits(model, "rpart")) {
                return(NULL)
            }

            tryCatch({
                cp_table <- model$cptable
                if (is.null(cp_table) || nrow(cp_table) == 0) {
                    return(NULL)
                }

                # Calculate confidence intervals for xerror
                xerror <- cp_table[, "xerror"]
                xstd <- cp_table[, "xstd"]
                lower_ci <- xerror - 1.96 * xstd  # 95% CI
                upper_ci <- xerror + 1.96 * xstd

                # Find optimal points
                min_xerror_idx <- which.min(xerror)
                min_xerror <- xerror[min_xerror_idx]
                se_threshold <- min_xerror + xstd[min_xerror_idx]
                one_se_idx <- which(xerror <= se_threshold)[1]

                html <- paste0("
                <div style='padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px;'>
                <h4 style='color: #2c5aa0; margin-top: 0;'>Detailed Cross-validation Error Analysis</h4>
                <p>Comprehensive analysis of cross-validation errors with confidence intervals and pruning recommendations.</p>

                <h5 style='color: #2c5aa0;'>Key Statistics:</h5>
                <ul>
                <li><strong>Minimum xerror:</strong> ", round(min_xerror, 4), " (Row ", min_xerror_idx, ")</li>
                <li><strong>1-SE threshold:</strong> ", round(se_threshold, 4), "</li>
                <li><strong>1-SE rule selection:</strong> Row ", one_se_idx, " (CP = ", round(cp_table[one_se_idx, "CP"], 6), ")</li>
                <li><strong>Cross-validation folds:</strong> ", model$control$xval, "</li>
                </ul>

                <h5 style='color: #2c5aa0;'>Xerror with 95% Confidence Intervals:</h5>
                <table style='border-collapse: collapse; width: 100%; margin: 10px 0; font-size: 12px;'>
                <tr style='background-color: #e9ecef; border: 1px solid #ddd;'>
                <th style='padding: 6px; border: 1px solid #ddd;'>Row</th>
                <th style='padding: 6px; border: 1px solid #ddd;'>CP</th>
                <th style='padding: 6px; border: 1px solid #ddd;'>Xerror</th>
                <th style='padding: 6px; border: 1px solid #ddd;'>Std Error</th>
                <th style='padding: 6px; border: 1px solid #ddd;'>95% CI Lower</th>
                <th style='padding: 6px; border: 1px solid #ddd;'>95% CI Upper</th>
                <th style='padding: 6px; border: 1px solid #ddd;'>Selection</th>
                </tr>")

                for (i in 1:nrow(cp_table)) {
                    is_min_xerror <- (i == min_xerror_idx)
                    is_one_se <- (i == one_se_idx)

                    row_style <- if (is_one_se) {
                        "background-color: #e3f2fd; border: 1px solid #ddd; font-weight: bold;"
                    } else if (is_min_xerror) {
                        "background-color: #fff3cd; border: 1px solid #ddd;"
                    } else {
                        "border: 1px solid #ddd;"
                    }

                    selection_status <- ""
                    if (is_one_se) selection_status <- "<span style='color: #4caf50;'>★ 1-SE</span>"
                    if (is_min_xerror) selection_status <- paste(selection_status, "<span style='color: #ff9800;'>Min</span>")

                    html <- paste0(html, "
                    <tr style='", row_style, "'>
                    <td style='padding: 6px; border: 1px solid #ddd;'>", i, "</td>
                    <td style='padding: 6px; border: 1px solid #ddd;'>", sprintf("%.6f", cp_table[i, "CP"]), "</td>
                    <td style='padding: 6px; border: 1px solid #ddd;'>", sprintf("%.4f", xerror[i]), "</td>
                    <td style='padding: 6px; border: 1px solid #ddd;'>", sprintf("%.4f", xstd[i]), "</td>
                    <td style='padding: 6px; border: 1px solid #ddd;'>", sprintf("%.4f", lower_ci[i]), "</td>
                    <td style='padding: 6px; border: 1px solid #ddd;'>", sprintf("%.4f", upper_ci[i]), "</td>
                    <td style='padding: 6px; border: 1px solid #ddd;'>", selection_status, "</td>
                    </tr>")
                }

                html <- paste0(html, "
                </table>

                <h5 style='color: #2c5aa0;'>Pruning Recommendations:</h5>
                <div style='background-color: #e8f5e8; padding: 10px; border-left: 4px solid #4caf50; margin: 10px 0;'>
                <p><strong>Recommended:</strong> Use 1-SE rule (Row ", one_se_idx, ") for clinical applications</p>
                <p><strong>Rationale:</strong> Provides good generalization with simpler, more interpretable tree</p>
                </div>

                <div style='background-color: #fff3e0; padding: 10px; border-left: 4px solid #ff9800; margin: 10px 0;'>
                <p><strong>Alternative:</strong> Minimum xerror (Row ", min_xerror_idx, ") for maximum accuracy</p>
                <p><strong>Trade-off:</strong> May overfit and be less robust to new data</p>
                </div>
                </div>")

                return(html)

            }, error = function(e) {
                return(paste0("<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px;'>
                <p><strong>Error generating xerror analysis:</strong></p>
                <p>", e$message, "</p>
                </div>"))
            })
        },

        # Generate impurity analysis
        .generate_impurity_analysis = function(model_results) {
            if (is.null(model_results) || is.null(model_results$model)) {
                return(NULL)
            }

            model <- model_results$model
            if (!inherits(model, "rpart")) {
                return(NULL)
            }

            tryCatch({
                # Get current splitting method
                current_split <- self$options$rpart_split %||% "gini"

                # Create comparison models with different splitting criteria
                train_data <- private$.training_data
                target_var <- self$options$target

                if (is.null(train_data) || is.null(target_var)) {
                    return(NULL)
                }

                # Get formula
                continuous_vars <- self$options$vars
                categorical_vars <- self$options$facs
                all_vars <- c(continuous_vars, categorical_vars)

                if (length(all_vars) == 0) {
                    return(NULL)
                }

                model_formula <- as.formula(paste(target_var, "~", paste(all_vars, collapse = "+")))

                # Train models with both splitting criteria
                gini_model <- rpart::rpart(
                    model_formula,
                    data = train_data,
                    method = ifelse(self$options$tree_mode == "regression", "anova", "class"),
                    parms = list(split = "gini"),
                    control = rpart::rpart.control(cp = 0.001, maxdepth = 5)  # Shallow for comparison
                )

                info_model <- rpart::rpart(
                    model_formula,
                    data = train_data,
                    method = ifelse(self$options$tree_mode == "regression", "anova", "class"),
                    parms = list(split = "information"),
                    control = rpart::rpart.control(cp = 0.001, maxdepth = 5)
                )

                # Compare models
                gini_nodes <- nrow(gini_model$frame)
                info_nodes <- nrow(info_model$frame)

                html <- paste0("
                <div style='padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px;'>
                <h4 style='color: #2c5aa0; margin-top: 0;'>Splitting Impurity Analysis</h4>
                <p>Comparison between Gini index and Information gain splitting criteria.</p>

                <h5 style='color: #2c5aa0;'>Current Model:</h5>
                <ul>
                <li><strong>Splitting method:</strong> ", tools::toTitleCase(current_split), "</li>
                <li><strong>Tree complexity:</strong> ", nrow(model$frame), " nodes</li>
                </ul>

                <h5 style='color: #2c5aa0;'>Splitting Criteria Comparison:</h5>
                <table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>
                <tr style='background-color: #e9ecef; border: 1px solid #ddd;'>
                <th style='padding: 8px; border: 1px solid #ddd;'>Criterion</th>
                <th style='padding: 8px; border: 1px solid #ddd;'>Tree Size</th>
                <th style='padding: 8px; border: 1px solid #ddd;'>Formula</th>
                <th style='padding: 8px; border: 1px solid #ddd;'>Best for</th>
                </tr>
                <tr style='", ifelse(current_split == "gini", "background-color: #e3f2fd; font-weight: bold;", ""), "border: 1px solid #ddd;'>
                <td style='padding: 8px; border: 1px solid #ddd;'>Gini Index", ifelse(current_split == "gini", " ★", ""), "</td>
                <td style='padding: 8px; border: 1px solid #ddd;'>", gini_nodes, " nodes</td>
                <td style='padding: 8px; border: 1px solid #ddd;'>1 - Σ(pi²)</td>
                <td style='padding: 8px; border: 1px solid #ddd;'>Binary classification, speed</td>
                </tr>
                <tr style='", ifelse(current_split == "information", "background-color: #e3f2fd; font-weight: bold;", ""), "border: 1px solid #ddd;'>
                <td style='padding: 8px; border: 1px solid #ddd;'>Information Gain", ifelse(current_split == "information", " ★", ""), "</td>
                <td style='padding: 8px; border: 1px solid #ddd;'>", info_nodes, " nodes</td>
                <td style='padding: 8px; border: 1px solid #ddd;'>-Σ(pi × log2(pi))</td>
                <td style='padding: 8px; border: 1px solid #ddd;'>Multi-class, theoretical optimality</td>
                </tr>
                </table>

                <h5 style='color: #2c5aa0;'>Impurity Measures Explained:</h5>
                <div style='background-color: #e8f4f8; padding: 10px; border-left: 4px solid #17a2b8; margin: 10px 0;'>
                <p><strong>Gini Index:</strong> Measures probability of misclassification. Faster to compute, tends to isolate most frequent class.</p>
                </div>

                <div style='background-color: #f0f8f0; padding: 10px; border-left: 4px solid #28a745; margin: 10px 0;'>
                <p><strong>Information Gain:</strong> Based on entropy reduction. Theoretically optimal, better for multi-class problems.</p>
                </div>

                <h5 style='color: #2c5aa0;'>Clinical Recommendations:</h5>
                <ul>
                <li><strong>Binary diagnosis:</strong> Gini often performs similarly with faster computation</li>
                <li><strong>Multi-class staging:</strong> Information gain may provide better discrimination</li>
                <li><strong>Large datasets:</strong> Gini recommended for computational efficiency</li>
                <li><strong>Interpretation:</strong> Both provide equivalent decision trees for clinical use</li>
                </ul>
                </div>")

                return(html)

            }, error = function(e) {
                return(paste0("<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px;'>
                <p><strong>Error generating impurity analysis:</strong></p>
                <p>", e$message, "</p>
                </div>"))
            })
        },

        # Generate recursive partitioning trace
        .generate_recursive_partitioning_trace = function(model_results) {
            if (is.null(model_results) || is.null(model_results$model)) {
                return(NULL)
            }

            model <- model_results$model
            if (!inherits(model, "rpart")) {
                return(NULL)
            }

            tryCatch({
                frame <- model$frame
                if (is.null(frame)) {
                    return(NULL)
                }

                # Get splitting information
                splits <- model$splits
                where <- model$where

                html <- paste0("
                <div style='padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px;'>
                <h4 style='color: #2c5aa0; margin-top: 0;'>Recursive Partitioning Trace</h4>
                <p>Step-by-step construction of the decision tree showing the recursive splitting process.</p>

                <h5 style='color: #2c5aa0;'>Tree Construction Summary:</h5>
                <ul>
                <li><strong>Total nodes:</strong> ", nrow(frame), "</li>
                <li><strong>Terminal nodes:</strong> ", sum(frame$var == '<leaf>'), "</li>
                <li><strong>Internal nodes:</strong> ", sum(frame$var != '<leaf>'), "</li>
                <li><strong>Maximum depth:</strong> ", max(as.numeric(row.names(frame)) %/% 2) + 1, "</li>
                </ul>

                <h5 style='color: #2c5aa0;'>Node-by-Node Construction:</h5>
                <div style='font-family: monospace; background-color: white; padding: 10px; border: 1px solid #ddd; max-height: 400px; overflow-y: auto;'>")

                # Process each node in the frame
                node_names <- as.numeric(row.names(frame))
                for (i in 1:nrow(frame)) {
                    node_num <- node_names[i]
                    depth <- floor(log2(node_num + 1))
                    indent <- paste(rep("  ", depth), collapse = "")

                    var_name <- frame$var[i]
                    n_obs <- frame$n[i]

                    if (var_name == "<leaf>") {
                        # Terminal node
                        if (self$options$tree_mode == "classification") {
                            pred_class <- levels(model$model[[1]])[frame$yval[i]]
                            html <- paste0(html, indent, "Node ", node_num, ": TERMINAL → Predict: ", pred_class, " (n=", n_obs, ")<br>")
                        } else {
                            pred_value <- round(frame$yval[i], 3)
                            html <- paste0(html, indent, "Node ", node_num, ": TERMINAL → Predict: ", pred_value, " (n=", n_obs, ")<br>")
                        }
                    } else {
                        # Internal node with split
                        split_point <- model$splits[var_name, "index"]
                        if (!is.na(split_point)) {
                            html <- paste0(html, indent, "Node ", node_num, ": Split on ", var_name, " < ", round(split_point, 3), " (n=", n_obs, ")<br>")
                        } else {
                            html <- paste0(html, indent, "Node ", node_num, ": Split on ", var_name, " (n=", n_obs, ")<br>")
                        }
                    }
                }

                html <- paste0(html, "
                </div>

                <h5 style='color: #2c5aa0;'>Recursive Partitioning Process:</h5>
                <ol>
                <li><strong>Start:</strong> Entire dataset at root node</li>
                <li><strong>Find best split:</strong> Evaluate all variables and thresholds</li>
                <li><strong>Split criterion:</strong> ", tools::toTitleCase(self$options$rpart_split %||% "gini"), " impurity reduction</li>
                <li><strong>Create children:</strong> Split dataset into left and right subsets</li>
                <li><strong>Recurse:</strong> Repeat process on each child node</li>
                <li><strong>Stopping criteria:</strong> Min observations (", model$control$minsplit, "), max depth (", model$control$maxdepth, "), or complexity (CP = ", model$control$cp, ")</li>
                </ol>

                <h5 style='color: #2c5aa0;'>Clinical Interpretation:</h5>
                <div style='background-color: #e8f5e8; padding: 10px; border-left: 4px solid #4caf50; margin: 10px 0;'>
                <p><strong>Tree structure:</strong> Each split represents a clinical decision point</p>
                <p><strong>Terminal nodes:</strong> Final diagnostic/prognostic categories</p>
                <p><strong>Path to prediction:</strong> Follow splits from root to terminal node for any patient</p>
                </div>
                </div>")

                return(html)

            }, error = function(e) {
                return(paste0("<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px;'>
                <p><strong>Error generating recursive partitioning trace:</strong></p>
                <p>", e$message, "</p>
                </div>"))
            })
        },

        # User-defined splitting functions (based on rpart vignette)
        .get_custom_splitting_functions = function(method_type) {
            # Based on usercode.R from rpart vignette
            switch(method_type,
                "custom_anova" = {
                    # Custom anova-like method (from vignette usercode.R)
                    itemp <- function(y, offset, parms, wt) {
                        if (is.matrix(y) && ncol(y) > 1)
                           stop("Matrix response not allowed")
                        if (!missing(parms) && length(parms) > 0)
                            warning("parameter argument ignored")
                        if (length(offset)) y <- y - offset
                        sfun <- function(yval, dev, wt, ylevel, digits ) {
                            paste("  mean=", format(signif(yval, digits)),
                                  ", MSE=" , format(signif(dev/wt, digits)),
                                  sep = '')
                        }
                        environment(sfun) <- .GlobalEnv
                        list(y = c(y), parms = NULL, numresp = 1, numy = 1, summary = sfun)
                    }

                    etemp <- function(y, wt, parms) {
                        wmean <- sum(y*wt)/sum(wt)
                        rss <- sum(wt*(y-wmean)^2)
                        list(label = wmean, deviance = rss)
                    }

                    stemp <- function(y, wt, x, parms, continuous) {
                        # Center y
                        n <- length(y)
                        y <- y- sum(y*wt)/sum(wt)

                        if (continuous) {
                            # continuous x variable
                            temp <- cumsum(y*wt)[-n]
                            left.wt  <- cumsum(wt)[-n]
                            right.wt <- sum(wt) - left.wt
                            lmean <- temp/left.wt
                            rmean <- -temp/right.wt
                            goodness <- (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2)
                            list(goodness = goodness, direction = sign(lmean))
                        } else {
                            # Categorical X variable
                            ux <- sort(unique(x))
                            wtsum <- tapply(wt, x, sum)
                            ysum  <- tapply(y*wt, x, sum)
                            means <- ysum/wtsum

                            # For anova splits, we can order the categories by their means
                            #  then use the same code as for a non-categorical
                            ord <- order(means)
                            n <- length(ord)
                            temp <- cumsum(ysum[ord])[-n]
                            left.wt  <- cumsum(wtsum[ord])[-n]
                            right.wt <- sum(wt) - left.wt
                            lmean <- temp/left.wt
                            rmean <- -temp/right.wt
                            list(goodness= (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2),
                                 direction = ux[ord])
                        }
                    }

                    return(list(eval = etemp, split = stemp, init = itemp))
                },
                "custom_logistic" = {
                    # Custom logistic regression method (from vignette usercode.R)
                    loginit <- function(y, offset, parms, wt) {
                        if (is.null(offset)) offset <- 0
                        if (any(y != 0 & y != 1)) stop ('response must be 0/1')

                        sfun <- function(yval, dev, wt, ylevel, digits ) {
                            paste("events=",  round(yval[,1]),
                                  ", coef= ", format(signif(yval[,2], digits)),
                                  ", deviance=" , format(signif(dev, digits)),
                                  sep = '')}
                        environment(sfun) <- .GlobalEnv
                        list(y = cbind(y, offset), parms = 0, numresp = 2, numy = 2,
                             summary = sfun)
                    }

                    logeval <- function(y, wt, parms) {
                        if (requireNamespace("stats", quietly = TRUE)) {
                            tfit <- glm(y[,1] ~ offset(y[,2]), binomial, weight = wt)
                            list(label= c(sum(y[,1]), tfit$coef), deviance = tfit$deviance)
                        } else {
                            # Fallback if glm not available
                            list(label = c(sum(y[,1]), 0), deviance = sum(wt))
                        }
                    }

                    logsplit <- function(y, wt, x, parms, continuous) {
                        if (continuous) {
                            # continuous x variable: do all the logistic regressions
                            n <- nrow(y)
                            goodness <- double(n-1)
                            direction <- goodness
                            temp <- rep(0, n)
                            for (i in 1:(n-1)) {
                                temp[i] <- 1
                                if (x[i] != x[i+1]) {
                                    if (requireNamespace("stats", quietly = TRUE)) {
                                        tfit <- glm(y[,1] ~ temp + offset(y[,2]), binomial, weight = wt)
                                        goodness[i] <- tfit$null.deviance - tfit$deviance
                                        direction[i] <- sign(tfit$coef[2])
                                    }
                                }
                            }
                        } else {
                            # Categorical X variable
                            if (requireNamespace("stats", quietly = TRUE)) {
                                tfit <- glm(y[,1] ~ factor(x) + offset(y[,2]) - 1, binomial, weight = wt)
                                ngrp <- length(tfit$coef)
                                direction <- rank(rank(tfit$coef) + runif(ngrp, 0, 0.1)) #break ties
                                xx <- direction[match(x, sort(unique(x)))] #relabel from small to large
                                goodness <- double(length(direction) - 1)
                                for (i in 1:length(goodness)) {
                                    tfit <- glm(y[,1] ~ I(xx > i) + offset(y[,2]), binomial, weight = wt)
                                    goodness[i] <- tfit$null.deviance - tfit$deviance
                                }
                            } else {
                                # Fallback
                                goodness <- rep(0, length(unique(x)) - 1)
                                direction <- 1:length(unique(x))
                            }
                        }
                        list(goodness=goodness, direction=direction)
                    }

                    return(list(eval = logeval, split = logsplit, init = loginit))
                },
                {
                    # Default: return NULL for unsupported methods
                    return(NULL)
                }
            )
        },

        # Method-specific parameter validation and guidance
        .validate_method_parameters = function() {
            tree_mode <- self$options$tree_mode %||% "classification"
            algorithm <- self$options$algorithm %||% "rpart"

            warnings <- c()
            recommendations <- c()

            # Algorithm-specific validations
            if (algorithm == "rpart") {
                # rpart-specific validations
                cp_value <- self$options$cost_complexity %||% 0.01
                if (cp_value < 0.001) {
                    warnings <- c(warnings, "Very low complexity parameter (CP < 0.001) may lead to overfitting")
                    recommendations <- c(recommendations, "Consider using cross-validation to select optimal CP value")
                }

                if (tree_mode == "classification") {
                    # Classification specific
                    max_depth <- self$options$tree_depth %||% 6
                    if (max_depth > 10) {
                        warnings <- c(warnings, "Deep trees (depth > 10) may overfit clinical data")
                        recommendations <- c(recommendations, "Consider pruning or reducing max_depth for better generalization")
                    }

                    # Loss matrix guidance
                    loss_preset <- self$options$clinical_loss_preset %||% "equal"
                    if (loss_preset == "equal") {
                        recommendations <- c(recommendations, "Consider using clinical loss matrices (screening/diagnosis presets) for medical applications")
                    }
                } else if (tree_mode == "regression") {
                    # Regression specific
                    min_split <- self$options$min_n %||% 20
                    if (min_split < 10) {
                        warnings <- c(warnings, "Low minimum split size may cause instability in regression trees")
                        recommendations <- c(recommendations, "Consider min_n >= 10 for stable regression splits")
                    }
                } else if (tree_mode == "survival") {
                    # Survival analysis specific
                    recommendations <- c(recommendations, "Ensure survival data has proper censoring indicators and time variables")
                } else if (tree_mode == "poisson") {
                    # Poisson specific
                    recommendations <- c(recommendations, "Poisson trees are designed for count data - ensure response variable contains non-negative integers")
                }

                # Cross-validation guidance
                xval_folds <- self$options$rpart_xval %||% 10
                if (xval_folds == 0) {
                    warnings <- c(warnings, "Cross-validation disabled (xval=0) - pruning may be suboptimal")
                    recommendations <- c(recommendations, "Enable cross-validation for better tree pruning")
                }

            } else if (algorithm == "fftrees") {
                # FFTrees-specific validations
                if (tree_mode != "classification") {
                    warnings <- c(warnings, "FFTrees algorithm is designed for binary classification only")
                    recommendations <- c(recommendations, "Use rpart or other algorithms for non-classification problems")
                }

                # FFTrees works best with binary outcomes
                target_var <- self$options$target
                if (!is.null(target_var) && !is.null(self$data)) {
                    target_levels <- unique(self$data[[target_var]])
                    if (length(target_levels) > 2) {
                        warnings <- c(warnings, "FFTrees works best with binary classification problems")
                        recommendations <- c(recommendations, "Consider binary encoding of target variable or use multi-class algorithms")
                    }
                }

            } else if (algorithm == "randomforest") {
                # Random Forest specific
                n_trees <- self$options$rf_ntree %||% 500
                if (n_trees < 100) {
                    warnings <- c(warnings, "Low number of trees may reduce Random Forest performance")
                    recommendations <- c(recommendations, "Consider using at least 100 trees for stable results")
                }

                mtry <- self$options$rf_mtry %||% 0
                if (mtry == 0) {
                    recommendations <- c(recommendations, "mtry=0 uses automatic selection - consider tuning for optimal performance")
                }

            } else if (algorithm == "xgboost") {
                # XGBoost specific
                learning_rate <- self$options$xgb_eta %||% 0.3
                n_rounds <- self$options$xgb_nrounds %||% 100

                if (learning_rate > 0.5) {
                    warnings <- c(warnings, "High learning rate may cause instability in XGBoost")
                    recommendations <- c(recommendations, "Consider eta <= 0.3 with more rounds for better convergence")
                }

                if (n_rounds < 50) {
                    warnings <- c(warnings, "Low number of rounds may underfit the data")
                    recommendations <- c(recommendations, "Consider increasing nrounds or using early stopping")
                }
            }

            # General data size guidance
            if (!is.null(self$data)) {
                n_samples <- nrow(self$data)
                n_vars <- length(c(self$options$vars, self$options$facs))

                if (n_samples < 100) {
                    warnings <- c(warnings, paste("Small sample size (n =", n_samples, ") may lead to unstable results"))
                    recommendations <- c(recommendations, "Consider bootstrap validation or cross-validation for reliability assessment")
                }

                if (n_vars > n_samples / 10) {
                    warnings <- c(warnings, "High-dimensional data (many variables relative to samples)")
                    recommendations <- c(recommendations, "Consider feature selection or regularization methods")
                }
            }

            return(list(
                warnings = warnings,
                recommendations = recommendations,
                algorithm = algorithm,
                tree_mode = tree_mode
            ))
        },

        # Enhanced cross-validation analysis with xpred.rpart
        .perform_xpred_analysis = function(model, train_data, target_var) {
            tryCatch({
                if (!requireNamespace("rpart", quietly = TRUE)) {
                    return(NULL)
                }

                # Get the number of cross-validation folds from the model
                xval_folds <- model$control$xval
                if (is.null(xval_folds) || xval_folds <= 1) {
                    return(NULL)
                }

                # Create cross-validation groups
                n_samples <- nrow(train_data)
                xgroup <- rep(1:xval_folds, length.out = n_samples)

                # Perform cross-validation predictions using xpred.rpart
                xpred_matrix <- rpart::xpred.rpart(model, xgroup)

                # Get actual values
                actual_values <- train_data[[target_var]]

                # Analysis depends on the type of model
                tree_mode <- self$options$tree_mode %||% "classification"

                if (tree_mode %in% c("classification", "class")) {
                    # Classification analysis
                    if (is.matrix(xpred_matrix)) {
                        # Convert probability matrix to class predictions
                        if (ncol(xpred_matrix) == 2) {
                            # Binary classification
                            predicted_probs <- xpred_matrix[, 2]
                            predicted_classes <- factor(ifelse(predicted_probs > 0.5,
                                                             levels(actual_values)[2],
                                                             levels(actual_values)[1]),
                                                       levels = levels(actual_values))

                            # Calculate cross-validation performance metrics
                            xval_accuracy <- mean(predicted_classes == actual_values, na.rm = TRUE)

                            # Calculate AUC if possible
                            xval_auc <- NULL
                            if (requireNamespace("pROC", quietly = TRUE)) {
                                xval_auc <- pROC::auc(actual_values, predicted_probs, quiet = TRUE)
                            }

                            return(list(
                                type = "classification",
                                predictions = predicted_classes,
                                probabilities = predicted_probs,
                                actual = actual_values,
                                accuracy = xval_accuracy,
                                auc = xval_auc,
                                xval_folds = xval_folds,
                                method = "xpred.rpart"
                            ))
                        }
                    }
                } else if (tree_mode %in% c("regression", "anova")) {
                    # Regression analysis
                    predicted_values <- as.numeric(xpred_matrix)

                    # Calculate cross-validation error metrics
                    mse <- mean((predicted_values - actual_values)^2, na.rm = TRUE)
                    rmse <- sqrt(mse)
                    mae <- mean(abs(predicted_values - actual_values), na.rm = TRUE)

                    # Calculate relative error as in the rpart vignette
                    baseline_error <- mean((actual_values - mean(actual_values, na.rm = TRUE))^2, na.rm = TRUE)
                    relative_error <- mse / baseline_error

                    # R-squared
                    ss_res <- sum((actual_values - predicted_values)^2, na.rm = TRUE)
                    ss_tot <- sum((actual_values - mean(actual_values, na.rm = TRUE))^2, na.rm = TRUE)
                    r_squared <- 1 - (ss_res / ss_tot)

                    return(list(
                        type = "regression",
                        predictions = predicted_values,
                        actual = actual_values,
                        mse = mse,
                        rmse = rmse,
                        mae = mae,
                        relative_error = relative_error,
                        r_squared = r_squared,
                        xval_folds = xval_folds,
                        method = "xpred.rpart"
                    ))
                }

                # Default return for unsupported modes
                return(list(
                    type = "unsupported",
                    method = "xpred.rpart",
                    xval_folds = xval_folds
                ))

            }, error = function(e) {
                warning("xpred.rpart analysis failed: ", e$message)
                return(NULL)
            })
        },

        # Comprehensive model training function
        .train_model = function(data) {
            algorithm <- self$options$algorithm
            target_var <- self$options$target

            message("[DEBUG] Algorithm selected: ", algorithm)
            message("[DEBUG] Target variable: ", target_var)
            message("[DEBUG] Data dimensions before split: ", nrow(data), " x ", ncol(data))

            # Prepare formula
            predictors <- setdiff(names(data), target_var)
            formula_str <- paste(target_var, "~", paste(predictors, collapse = " + "))
            model_formula <- as.formula(formula_str)

            message("[DEBUG] Predictors: ", paste(predictors, collapse = ", "))
            message("[DEBUG] Formula created: ", formula_str)

            # Split data for validation
            validation_method <- self$options$validation
            message("[DEBUG] Validation method: ", validation_method)

            if (validation_method == "holdout") {
                set.seed(123)
                message("[DEBUG] Creating holdout split with test_split = ", self$options$test_split)
                train_idx <- caret::createDataPartition(data[[target_var]],
                                                        p = 1 - self$options$test_split,
                                                        list = FALSE)
                train_data <- data[train_idx, ]
                test_data <- data[-train_idx, ]
                message("[DEBUG] Train set size: ", nrow(train_data))
                message("[DEBUG] Test set size: ", nrow(test_data))
            } else {
                message("[DEBUG] Using full data for both train and test")
                train_data <- data
                test_data <- data
            }

            private$.training_data <- train_data
            private$.test_data <- test_data

            # Define positive and negative classes
            positive_level <- self$options$targetLevel
            target_levels <- levels(as.factor(train_data[[target_var]]))
            negative_class <- setdiff(target_levels, positive_level)[1]
            target_level <- positive_level  # Alias for consistency

            return(tryCatch({
                # Train model based on algorithm
                if (algorithm == "fftrees") {
                    # FFTrees algorithm implementation
                    message("[DEBUG] Starting FFTrees algorithm")

                    # Check target variable levels for FFTrees (requires binary)
                    if (length(target_levels) != 2) {
                        message("[DEBUG] FFTrees requires binary classification. Converting to binary...")
                        train_data[[target_var]] <- factor(
                            ifelse(train_data[[target_var]] == positive_level, positive_level, "Other"),
                            levels = c("Other", positive_level)
                        )
                        test_data[[target_var]] <- factor(
                            ifelse(test_data[[target_var]] == positive_level, positive_level, "Other"),
                            levels = c("Other", positive_level)
                        )
                        negative_class <- "Other"
                    }

                    # FFTrees requires complete cases (no NA in target variable)
                    complete_train_rows <- complete.cases(train_data[[target_var]])
                    train_data <- train_data[complete_train_rows, ]
                    
                    complete_test_rows <- complete.cases(test_data[[target_var]])
                    test_data <- test_data[complete_test_rows, ]
                    
                    message(paste("[DEBUG] After removing NA values: train_data =", nrow(train_data), "rows, test_data =", nrow(test_data), "rows"))
                    
                    # Check if we have enough data after NA removal
                    if (nrow(train_data) == 0) {
                        stop("No complete cases found in training data for FFTrees analysis")
                    }
                    if (nrow(test_data) == 0) {
                        stop("No complete cases found in test data for FFTrees analysis")
                    }

                    # Create logical version of target variable
                    train_data[[paste0(target_var, "_logical")]] <- as.logical(train_data[[target_var]] == positive_level)
                    test_data[[paste0(target_var, "_logical")]] <- as.logical(test_data[[target_var]] == positive_level)

                    logical_target <- paste0(target_var, "_logical")
                    predictors <- setdiff(names(train_data), c(target_var, logical_target))
                    model_formula_logical <- as.formula(paste(logical_target, "~", paste(predictors, collapse = " + ")))

                    # Train FFTrees model
                    model <- FFTrees::FFTrees(
                        formula = model_formula_logical,
                        data = train_data,
                        data.test = test_data,
                        main = "Medical Decision Tree",
                        quiet = list(ini = TRUE, fin = FALSE, mis = FALSE, set = TRUE)
                    )

                    # Get predictions
                    test_pred <- predict(model, newdata = test_data)
                    predictions_logical <- as.logical(test_pred)
                    predictions_class <- factor(
                        ifelse(predictions_logical, positive_level, negative_class),
                        levels = c(negative_class, positive_level)
                    )
                    predictions_prob <- as.numeric(predictions_logical)

                } else if (algorithm == "rpart") {
                    message("[DEBUG] Training enhanced CART model")

                    # Determine if hyperparameter tuning is enabled
                    if (self$options$hyperparameter_tuning) {
                        model <- private$.tune_hyperparameters(model_formula, train_data, test_data, algorithm)
                        predictions_prob <- predict(model, test_data, type = "prob")[, 2]
                        predictions_class <- predict(model, test_data, type = "class")
                    } else {
                        # Enhanced rpart training with user-specified parameters
                        model <- private$.train_rpart_model(model_formula, train_data, test_data, target_var)
                        pred_results <- private$.make_rpart_predictions(model, test_data)
                        predictions_prob <- pred_results$prob
                        predictions_class <- pred_results$class
                    }

                } else if (algorithm == "c50") {
                    message("[DEBUG] Training C5.0 classification model")

                    if (!requireNamespace("C50", quietly = TRUE)) {
                        stop("C50 package required but not installed. Please install with: install.packages('C50')")
                    }

                    if (self$options$hyperparameter_tuning) {
                        model <- private$.tune_hyperparameters(model_formula, train_data, test_data, algorithm)
                        predictions_class <- predict(model, test_data, type = "class")
                        predictions_prob_matrix <- predict(model, test_data, type = "prob")
                        predictions_prob <- predictions_prob_matrix[, 2]
                    } else {
                        trials <- self$options$c50_trials %||% 1
                        winnow <- self$options$c50_winnow %||% FALSE

                        model <- C50::C5.0(
                            model_formula,
                            data = train_data,
                            trials = trials,
                            winnow = winnow,
                            control = C50::C5.0Control(
                                minCases = self$options$min_n %||% 20,
                                CF = 1 - (self$options$cost_complexity %||% 0.01)
                            )
                        )

                        predictions_class <- predict(model, test_data, type = "class")
                        predictions_prob_matrix <- predict(model, test_data, type = "prob")
                        predictions_prob <- predictions_prob_matrix[, 2]
                    }

                } else if (algorithm == "randomforest") {
                    message("[DEBUG] Training Random Forest model")

                    if (!requireNamespace("randomForest", quietly = TRUE)) {
                        stop("randomForest package required but not installed")
                    }

                    if (self$options$hyperparameter_tuning) {
                        model <- private$.tune_hyperparameters(model_formula, train_data, test_data, algorithm)
                    } else {
                        ntree <- self$options$rf_ntree %||% 500
                        mtry <- self$options$rf_mtry %||% 0

                        if (mtry == 0) {
                            n_predictors <- length(predictors)
                            mtry <- max(1, floor(sqrt(n_predictors)))
                        }

                        model <- randomForest::randomForest(
                            model_formula,
                            data = train_data,
                            ntree = ntree,
                            mtry = mtry,
                            na.action = na.roughfix,
                            importance = TRUE
                        )
                    }

                    predictions_prob_matrix <- predict(model, test_data, type = "prob")
                    if (ncol(predictions_prob_matrix) >= 2) {
                        predictions_prob <- predictions_prob_matrix[, 2]
                    } else {
                        predictions_prob <- predictions_prob_matrix[, 1]
                    }
                    predictions_class <- predict(model, test_data, type = "class")

                } else if (algorithm == "xgboost") {
                    message("[DEBUG] Training XGBoost model")

                    if (!requireNamespace("xgboost", quietly = TRUE)) {
                        stop("xgboost package required but not installed")
                    }

                    if (self$options$hyperparameter_tuning) {
                        model <- private$.tune_hyperparameters(model_formula, train_data, test_data, algorithm)

                        # XGBoost predictions for tuned model
                        test_prepared <- private$.prepare_data_for_xgboost(test_data, target_var)
                        test_matrix <- xgboost::xgb.DMatrix(test_prepared$X, label = test_prepared$y)
                        predictions_prob <- predict(model, test_matrix)
                        predictions_class <- factor(
                            ifelse(predictions_prob > 0.5, target_level,
                                   setdiff(target_levels, target_level)[1]),
                            levels = target_levels
                        )
                    } else {
                        # Manual XGBoost training - simplified version
                        pred_vars <- c(self$options$vars, self$options$facs)
                        train_x <- train_data[pred_vars]
                        train_y <- train_data[[target_var]]
                        test_x <- test_data[pred_vars]

                        # Convert categorical variables to numeric
                        for (var in self$options$facs) {
                            if (var %in% names(train_x)) {
                                if (is.factor(train_x[[var]])) {
                                    all_levels <- unique(c(levels(train_x[[var]]), levels(test_x[[var]])))
                                    train_x[[var]] <- factor(train_x[[var]], levels = all_levels)
                                    test_x[[var]] <- factor(test_x[[var]], levels = all_levels)
                                    train_x[[var]] <- as.numeric(train_x[[var]]) - 1
                                    test_x[[var]] <- as.numeric(test_x[[var]]) - 1
                                }
                            }
                        }

                        train_matrix <- as.matrix(train_x)
                        test_matrix <- as.matrix(test_x)
                        train_label <- as.numeric(train_y == target_level)

                        dtrain <- xgboost::xgb.DMatrix(data = train_matrix, label = train_label)
                        dtest <- xgboost::xgb.DMatrix(data = test_matrix)

                        model <- xgboost::xgboost(
                            data = dtrain,
                            nrounds = self$options$xgb_nrounds %||% 100,
                            eta = self$options$xgb_eta %||% 0.3,
                            max_depth = self$options$tree_depth %||% 6,
                            objective = "binary:logistic",
                            eval_metric = "auc",
                            verbose = 0,
                            nthread = 1
                        )

                        predictions_prob <- predict(model, dtest)
                        predictions_class <- factor(
                            ifelse(predictions_prob > 0.5, target_level,
                                   setdiff(target_levels, target_level)[1]),
                            levels = target_levels
                        )
                    }
                } else if (algorithm == "ensemble") {
                    message("[DEBUG] Training Ensemble model")
                    ensemble_results <- private$.train_ensemble_model(model_formula, train_data, test_data, target_var)
                    model <- ensemble_results$model
                    predictions_prob <- ensemble_results$predictions_prob
                    predictions_class <- ensemble_results$predictions_class
                } else if (algorithm == "consensus") {
                    message("[DEBUG] Training Consensus Trees model")
                    consensus_results <- private$.train_consensus_model(model_formula, train_data, test_data, target_var)
                    model <- consensus_results$model
                    predictions_prob <- consensus_results$predictions_prob
                    predictions_class <- consensus_results$predictions_class
                } else {
                    stop("Unsupported algorithm: ", algorithm)
                }

                # Calculate feature importance
                importance_data <- private$.calculate_importance(model, algorithm)

                # Perform cross-validation if requested
                validation_results <- NULL
                if (validation_method == "cv") {
                    validation_results <- private$.perform_cv(model_formula, data, algorithm)
                }

                # Return results
                list(
                    model = model,
                    predictions = predictions_prob,
                    predictions_class = predictions_class,
                    actual = test_data[[target_var]],
                    importance = importance_data,
                    validation_results = validation_results,
                    algorithm = algorithm,
                    formula = model_formula
                )

            }, error = function(e) {
                message("[DEBUG] Model training error: ", e$message)

                # Provide specific error guidance
                error_msg <- private$.generate_error_message(e, algorithm)
                self$results$model_summary$setContent(error_msg)
                return(NULL)
            }))
        },

        # Ensemble training function
        .train_ensemble_model = function(model_formula, train_data, test_data, target_var) {
            message("[DEBUG] Creating ensemble model")

            ensemble_size <- min(10, self$options$ensemble_size %||% 10)
            available_algorithms <- c("rpart")

            # Add available algorithms
            if (requireNamespace("randomForest", quietly = TRUE)) {
                available_algorithms <- c(available_algorithms, "randomforest")
            }
            if (requireNamespace("C50", quietly = TRUE)) {
                available_algorithms <- c(available_algorithms, "c50")
            }

            ensemble_models <- list()
            all_predictions_prob <- list()
            all_predictions_class <- list()

            target_level <- self$options$targetLevel
            target_levels <- levels(train_data[[target_var]])

            # Train multiple models
            for (i in 1:ensemble_size) {
                private$.checkpoint()  # Add checkpoint before expensive ensemble training
                
                algo <- sample(available_algorithms, 1)

                # Add diversity through bootstrap sampling if requested
                if (self$options$ensemble_diversity) {
                    boot_indices <- sample(nrow(train_data), replace = TRUE)
                    boot_data <- train_data[boot_indices, ]
                } else {
                    boot_data <- train_data
                }

                tryCatch({
                    if (algo == "rpart") {
                        model_i <- rpart::rpart(
                            model_formula, data = boot_data, method = "class",
                            control = rpart::rpart.control(
                                cp = runif(1, 0.001, 0.05),
                                maxdepth = sample(3:7, 1)
                            )
                        )
                    } else if (algo == "randomforest") {
                        model_i <- randomForest::randomForest(
                            model_formula, data = boot_data,
                            ntree = sample(c(100, 300, 500), 1)
                        )
                    } else if (algo == "c50") {
                        model_i <- C50::C5.0(model_formula, data = boot_data)
                    }

                    pred_prob_i <- predict(model_i, test_data, type = "prob")[, 2]
                    pred_class_i <- predict(model_i, test_data, type = "class")

                    ensemble_models[[i]] <- list(model = model_i, algorithm = algo)
                    all_predictions_prob[[i]] <- pred_prob_i
                    all_predictions_class[[i]] <- pred_class_i

                }, error = function(e) {
                    message("[DEBUG] Failed to train ensemble model ", i, ": ", e$message)
                })
            }

            # Combine predictions
            if (length(all_predictions_prob) > 0) {
                # Average probability predictions
                prob_matrix <- do.call(cbind, all_predictions_prob)
                ensemble_prob <- rowMeans(prob_matrix, na.rm = TRUE)

                # Majority voting for class predictions
                class_votes <- do.call(cbind, lapply(all_predictions_class, as.character))
                ensemble_class <- factor(
                    apply(class_votes, 1, function(x) {
                        tbl <- table(x)
                        names(tbl)[which.max(tbl)]
                    }),
                    levels = target_levels
                )

                ensemble_model <- list(
                    models = ensemble_models,
                    ensemble_size = length(ensemble_models)
                )
                class(ensemble_model) <- "ensemble_tree"

                return(list(
                    model = ensemble_model,
                    predictions_prob = ensemble_prob,
                    predictions_class = ensemble_class
                ))
            } else {
                stop("No ensemble models were successfully trained")
            }
        },

        # Consensus training function
        .train_consensus_model = function(model_formula, train_data, test_data, target_var) {
            message("[DEBUG] Creating consensus model using multiple validation methods")

            consensus_threshold <- self$options$consensus_threshold %||% 0.6
            target_level <- self$options$targetLevel
            target_levels <- levels(train_data[[target_var]])

            # Use CV for consensus
            cv_predictions <- private$.get_cv_consensus_predictions(model_formula, train_data, test_data)

            # Apply consensus threshold
            consensus_class <- factor(
                ifelse(cv_predictions > consensus_threshold, target_level,
                       setdiff(target_levels, target_level)[1]),
                levels = target_levels
            )

            consensus_model <- list(
                cv_predictions = cv_predictions,
                threshold = consensus_threshold
            )
            class(consensus_model) <- "consensus_tree"

            return(list(
                model = consensus_model,
                predictions_prob = cv_predictions,
                predictions_class = consensus_class
            ))
        },

        .get_cv_consensus_predictions = function(model_formula, train_data, test_data) {
            k <- 5
            folds <- sample(rep(1:k, length.out = nrow(train_data)))
            cv_predictions <- numeric(nrow(test_data))

            for (fold in 1:k) {
                private$.checkpoint()  # Add checkpoint before expensive CV consensus operation
                
                train_fold <- train_data[folds != fold, ]
                fold_model <- rpart::rpart(model_formula, data = train_fold, method = "class")
                fold_pred <- predict(fold_model, test_data, type = "prob")[, 2]
                cv_predictions <- cv_predictions + fold_pred / k
            }
            return(cv_predictions)
        },

        # Specialized rpart training function
        .train_rpart_model = function(model_formula, train_data, test_data, target_var) {
            # Handle missing data
            missing_method <- self$options$missing_data_method %||% "native"
            if (missing_method == "clinical_impute") {
                train_data <- private$.clinical_imputation(train_data, target_var)
                test_data <- private$.clinical_imputation(test_data, target_var)
            } else if (missing_method == "exclude") {
                complete_vars <- c(self$options$vars, self$options$facs, target_var)
                train_data <- train_data[complete.cases(train_data[complete_vars]), ]
                test_data <- test_data[complete.cases(test_data[complete_vars]), ]
            }

            # Enhanced rpart parameters
            n_classes <- length(levels(train_data[[target_var]]))

            # Prior probabilities
            prior_probs <- NULL
            prior_setting <- self$options$enhanced_prior_probs %||% "data"
            if (prior_setting == "population" && !is.null(self$options$population_prevalence)) {
                if (n_classes == 2) {
                    prev <- self$options$population_prevalence
                    prior_probs <- c(1 - prev, prev)
                }
            } else if (prior_setting == "balanced") {
                prior_probs <- rep(1/n_classes, n_classes)
            } else if (prior_setting == "custom" && !is.null(self$options$custom_prior_values) && self$options$custom_prior_values != "") {
                custom_priors <- as.numeric(strsplit(self$options$custom_prior_values, ",")[[1]])
                if (length(custom_priors) == n_classes && abs(sum(custom_priors) - 1) < 0.001) {
                    prior_probs <- custom_priors
                }
            }

            # Loss matrix
            loss_matrix <- NULL
            loss_preset <- self$options$clinical_loss_preset %||% "equal"
            if (loss_preset == "screening" && n_classes == 2) {
                loss_matrix <- matrix(c(0, 1, 5, 0), nrow = 2, ncol = 2)
            } else if (loss_preset == "diagnosis" && n_classes == 2) {
                loss_matrix <- matrix(c(0, 3, 1, 0), nrow = 2, ncol = 2)
            } else if (loss_preset == "custom" && !is.null(self$options$rpart_loss_matrix) && self$options$rpart_loss_matrix != "") {
                loss_vals <- as.numeric(strsplit(self$options$rpart_loss_matrix, ",")[[1]])
                if (length(loss_vals) == n_classes^2) {
                    loss_matrix <- matrix(loss_vals, nrow = n_classes, ncol = n_classes)
                }
            }

            # Method selection
            tree_mode <- self$options$tree_mode %||% "classification"
            use_custom <- self$options$use_custom_splitting %||% FALSE

            if (use_custom) {
                custom_method_type <- self$options$custom_splitting_method %||% "custom_anova"
                custom_method <- private$.get_custom_splitting_functions(custom_method_type)
                if (is.null(custom_method)) {
                    warning("Custom splitting method not found. Using standard rpart method.")
                    use_custom <- FALSE
                }
            }

            rpart_method <- if (use_custom) {
                custom_method
            } else {
                switch(tree_mode,
                       "classification" = "class",
                       "regression" = "anova",
                       "survival" = "exp",
                       "poisson" = "poisson",
                       "exponential" = "exp",
                       "class")
            }

            # Create rpart control
            rpart_control <- rpart::rpart.control(
                maxdepth = self$options$tree_depth %||% self$options$max_depth %||% 6,
                minsplit = self$options$min_n %||% self$options$min_samples_split %||% 20,
                minbucket = self$options$min_samples_leaf %||% 10,
                cp = self$options$cost_complexity %||% 0.01,
                surrogate = if (!is.null(self$options$rpart_surrogate)) self$options$rpart_surrogate else TRUE,
                usesurrogate = as.numeric(self$options$rpart_usesurrogate %||% "2"),
                maxsurrogate = self$options$rpart_maxsurrogate %||% 5,
                xval = self$options$rpart_xval %||% 10,
                maxcompete = self$options$rpart_maxcompete %||% 4
            )

            # Train model
            if (use_custom) {
                model <- rpart::rpart(model_formula, data = train_data, method = rpart_method, control = rpart_control)
            } else {
                model <- rpart::rpart(
                    model_formula,
                    data = train_data,
                    method = rpart_method,
                    parms = list(
                        split = self$options$rpart_split %||% "gini",
                        prior = prior_probs,
                        loss = loss_matrix
                    ),
                    control = rpart_control
                )
            }

            # Enhanced cross-validation analysis with xpred.rpart
            if (self$options$rpart_xval > 0) {
                xpred_analysis <- private$.perform_xpred_analysis(model, train_data, target_var)
                if (!is.null(xpred_analysis)) {
                    model$xpred_results <- xpred_analysis
                }
            }

            return(model)
        },

        # Make rpart predictions
        .make_rpart_predictions = function(model, test_data) {
            prediction_type <- self$options$rpart_prediction_type %||% "class"

            if (prediction_type == "prob") {
                pred_result <- predict(model, test_data, type = "prob")
                if (self$options$tree_mode == "classification" && ncol(pred_result) >= 2) {
                    predictions_prob <- pred_result[, 2]
                    predictions_class <- predict(model, test_data, type = "class")
                } else {
                    predictions_prob <- pred_result
                    predictions_class <- predict(model, test_data, type = "class")
                }
            } else if (prediction_type == "vector") {
                predictions_vector <- predict(model, test_data, type = "vector")
                predictions_prob <- predict(model, test_data, type = "prob")[, 2]
                predictions_class <- predict(model, test_data, type = "class")
                private$.vector_predictions <- predictions_vector
            } else if (prediction_type == "matrix") {
                predictions_matrix <- predict(model, test_data, type = "matrix")
                predictions_prob <- predict(model, test_data, type = "prob")[, 2]
                predictions_class <- predict(model, test_data, type = "class")
                private$.matrix_predictions <- predictions_matrix
            } else {
                predictions_prob <- predict(model, test_data, type = "prob")[, 2]
                predictions_class <- predict(model, test_data, type = "class")
            }

            return(list(prob = predictions_prob, class = predictions_class))
        },

        # Generate error messages
        .generate_error_message = function(e, algorithm) {
            error_type <- "Unknown"
            specific_help <- ""

            if (grepl("contrasts can be applied only to factors", e$message)) {
                error_type <- "Variable Type Error"
                specific_help <- "<p><strong>Solution:</strong> Some predictor variables appear to be coded as text but contain numeric values. Convert them to proper numeric or factor variables in your data preparation.</p>"
            } else if (grepl("sample size", e$message)) {
                error_type <- "Sample Size Error"
                specific_help <- "<p><strong>Solutions:</strong><ul><li>Collect more data</li><li>Reduce number of predictor variables</li><li>Use simpler statistical methods</li></ul></p>"
            } else if (grepl("singularities|rank-deficient", e$message)) {
                error_type <- "Multicollinearity Error"
                specific_help <- "<p><strong>Solutions:</strong><ul><li>Remove highly correlated predictor variables</li><li>Use feature selection</li><li>Check for duplicate or derived variables</li></ul></p>"
            } else if (grepl("levels|factor", e$message)) {
                error_type <- "Factor Level Error"
                specific_help <- "<p><strong>Solutions:</strong><ul><li>Check that categorical variables have valid levels</li><li>Ensure target level exists in the target variable</li><li>Remove unused factor levels</li></ul></p>"
            }

            return(paste0("
            <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
            <h4>", error_type, "</h4>
            <p><strong>Error Details:</strong> ", e$message, "</p>
            ", specific_help, "
            <p><strong>General Troubleshooting:</strong></p>
            <ul>
            <li>Verify data quality and variable types</li>
            <li>Check for sufficient sample size in each group</li>
            <li>Ensure predictor variables have variation</li>
            <li>Consider data preprocessing options</li>
            </ul>
            </div>"))
        },

        # Fancy plot generation using rattle-style visualization
        .generate_fancy_plot = function(model) {
            if (is.null(model)) return(NULL)

            tryCatch({
                message("[DEBUG] Generating fancy tree plot")

                # Create enhanced tree visualization
                plot_data <- list(
                    nodes = data.frame(
                        node_id = as.numeric(rownames(model$frame)),
                        splits = ifelse(model$frame$var == "<leaf>",
                                      paste("Node", rownames(model$frame)),
                                      as.character(model$frame$var)),
                        n = model$frame$n,
                        dev = model$frame$dev,
                        yval = model$frame$yval,
                        complexity = model$frame$complexity %||% 0
                    ),
                    edges = data.frame(
                        from = integer(0),
                        to = integer(0),
                        condition = character(0)
                    )
                )

                # Enhanced plot with colors and improved layout
                if (requireNamespace("rattle", quietly = TRUE)) {
                    # Use rattle for fancy plotting if available
                    rattle::fancyRpartPlot(model,
                                         main = "Enhanced Decision Tree",
                                         sub = paste("Complexity Parameter:", format(model$control$cp, digits = 4)))
                } else {
                    # Fallback to enhanced rpart plot
                    plot(model, uniform = TRUE, branch = 0.4, compress = TRUE, margin = 0.1)
                    text(model, all = TRUE, use.n = TRUE, fancy = TRUE, cex = 0.8)
                    title(main = "Enhanced Decision Tree Visualization")
                }

                return(plot_data)

            }, error = function(e) {
                message("[DEBUG] Fancy plot generation failed: ", e$message)
                # Fallback to standard plot
                plot(model)
                text(model)
                return(NULL)
            })
        },

        # F1 score analysis with detailed metrics
        .generate_f1_analysis = function(predictions, actual, model) {
            if (is.null(predictions) || is.null(actual)) return(NULL)

            tryCatch({
                message("[DEBUG] Generating F1 score analysis")

                # Ensure factors have same levels
                if (is.factor(predictions) && is.factor(actual)) {
                    all_levels <- union(levels(predictions), levels(actual))
                    predictions <- factor(predictions, levels = all_levels)
                    actual <- factor(actual, levels = all_levels)
                }

                # Calculate confusion matrix
                cm <- table(Predicted = predictions, Actual = actual)

                # Multi-class F1 calculation
                classes <- levels(actual)
                f1_scores <- numeric(length(classes))
                precision_scores <- numeric(length(classes))
                recall_scores <- numeric(length(classes))

                for (i in seq_along(classes)) {
                    class_name <- classes[i]

                    # True positives, false positives, false negatives
                    tp <- cm[class_name, class_name] %||% 0
                    fp <- sum(cm[class_name, ]) - tp
                    fn <- sum(cm[, class_name]) - tp

                    # Calculate precision and recall
                    precision <- if (tp + fp > 0) tp / (tp + fp) else 0
                    recall <- if (tp + fn > 0) tp / (tp + fn) else 0

                    # Calculate F1 score
                    f1 <- if (precision + recall > 0) 2 * (precision * recall) / (precision + recall) else 0

                    f1_scores[i] <- f1
                    precision_scores[i] <- precision
                    recall_scores[i] <- recall
                }

                # Overall metrics
                macro_f1 <- mean(f1_scores, na.rm = TRUE)
                weighted_f1 <- sum(f1_scores * table(actual)) / length(actual)

                # Create summary table
                f1_analysis <- data.frame(
                    Class = classes,
                    Precision = round(precision_scores, 4),
                    Recall = round(recall_scores, 4),
                    F1_Score = round(f1_scores, 4),
                    Support = as.numeric(table(actual)[classes])
                )

                # Add overall metrics
                overall_metrics <- data.frame(
                    Metric = c("Macro Avg F1", "Weighted Avg F1", "Accuracy"),
                    Value = round(c(macro_f1, weighted_f1, sum(diag(cm))/sum(cm)), 4)
                )

                return(list(
                    class_metrics = f1_analysis,
                    overall_metrics = overall_metrics,
                    confusion_matrix = cm
                ))

            }, error = function(e) {
                message("[DEBUG] F1 analysis failed: ", e$message)
                return(NULL)
            })
        },

        # Learning curve generation
        .generate_learning_curve = function(model_formula, data, target_var) {
            tryCatch({
                message("[DEBUG] Generating learning curve")

                # Define training set sizes
                n_total <- nrow(data)
                train_sizes <- unique(round(seq(0.1, 1.0, by = 0.1) * n_total))
                train_sizes <- train_sizes[train_sizes >= 10] # Minimum 10 samples

                learning_results <- data.frame(
                    TrainSize = integer(0),
                    TrainScore = numeric(0),
                    ValidationScore = numeric(0)
                )

                # Cross-validation folds
                n_folds <- min(5, nrow(data) %/% 10)
                if (n_folds < 2) n_folds <- 2

                # Create folds
                fold_indices <- sample(rep(1:n_folds, length.out = nrow(data)))

                for (size in train_sizes) {
                    fold_train_scores <- numeric(n_folds)
                    fold_val_scores <- numeric(n_folds)

                    for (fold in 1:n_folds) {
                        # Create train/validation split
                        val_indices <- which(fold_indices == fold)
                        train_indices <- which(fold_indices != fold)

                        # Subsample training data to desired size
                        if (length(train_indices) > size) {
                            train_indices <- sample(train_indices, size)
                        }

                        train_data <- data[train_indices, ]
                        val_data <- data[val_indices, ]

                        # Train model
                        fold_model <- tryCatch({
                            rpart::rpart(model_formula, data = train_data, method = "class")
                        }, error = function(e) NULL)

                        if (!is.null(fold_model)) {
                            # Calculate accuracies
                            train_pred <- predict(fold_model, train_data, type = "class")
                            val_pred <- predict(fold_model, val_data, type = "class")

                            fold_train_scores[fold] <- mean(train_pred == train_data[[target_var]], na.rm = TRUE)
                            fold_val_scores[fold] <- mean(val_pred == val_data[[target_var]], na.rm = TRUE)
                        } else {
                            fold_train_scores[fold] <- NA
                            fold_val_scores[fold] <- NA
                        }
                    }

                    # Average across folds
                    learning_results <- rbind(learning_results, data.frame(
                        TrainSize = size,
                        TrainScore = mean(fold_train_scores, na.rm = TRUE),
                        ValidationScore = mean(fold_val_scores, na.rm = TRUE)
                    ))
                }

                return(learning_results)

            }, error = function(e) {
                message("[DEBUG] Learning curve generation failed: ", e$message)
                return(NULL)
            })
        },

        # Grid search with cross-validation
        .perform_grid_search_cv = function(model_formula, data, target_var) {
            tryCatch({
                message("[DEBUG] Performing grid search CV")

                # Define parameter grid
                param_grid <- expand.grid(
                    cp = c(0.001, 0.01, 0.05, 0.1),
                    maxdepth = c(3, 5, 10, 15),
                    minsplit = c(2, 5, 10, 20),
                    minbucket = c(1, 3, 5, 10)
                )

                # Limit grid size if too large
                if (nrow(param_grid) > 50) {
                    param_grid <- param_grid[sample(nrow(param_grid), 50), ]
                }

                # Cross-validation setup
                n_folds <- min(5, nrow(data) %/% 10)
                if (n_folds < 2) n_folds <- 2
                fold_indices <- sample(rep(1:n_folds, length.out = nrow(data)))

                # Store results
                grid_results <- data.frame(
                    param_grid,
                    mean_cv_score = numeric(nrow(param_grid)),
                    std_cv_score = numeric(nrow(param_grid))
                )

                for (i in 1:nrow(param_grid)) {
                    params <- param_grid[i, ]
                    cv_scores <- numeric(n_folds)

                    for (fold in 1:n_folds) {
                        train_indices <- which(fold_indices != fold)
                        val_indices <- which(fold_indices == fold)

                        train_data <- data[train_indices, ]
                        val_data <- data[val_indices, ]

                        # Train model with current parameters
                        model <- tryCatch({
                            rpart::rpart(
                                model_formula,
                                data = train_data,
                                method = "class",
                                control = rpart::rpart.control(
                                    cp = params$cp,
                                    maxdepth = params$maxdepth,
                                    minsplit = params$minsplit,
                                    minbucket = params$minbucket
                                )
                            )
                        }, error = function(e) NULL)

                        if (!is.null(model)) {
                            val_pred <- predict(model, val_data, type = "class")
                            cv_scores[fold] <- mean(val_pred == val_data[[target_var]], na.rm = TRUE)
                        } else {
                            cv_scores[fold] <- 0
                        }
                    }

                    grid_results$mean_cv_score[i] <- mean(cv_scores, na.rm = TRUE)
                    grid_results$std_cv_score[i] <- sd(cv_scores, na.rm = TRUE)
                }

                # Find best parameters
                best_idx <- which.max(grid_results$mean_cv_score)
                best_params <- grid_results[best_idx, ]

                # Sort by performance
                grid_results <- grid_results[order(grid_results$mean_cv_score, decreasing = TRUE), ]

                return(list(
                    best_params = best_params,
                    all_results = grid_results,
                    best_score = grid_results$mean_cv_score[1]
                ))

            }, error = function(e) {
                message("[DEBUG] Grid search failed: ", e$message)
                return(NULL)
            })
        },

        # Surrogate split analysis
        .analyze_surrogate_splits = function(model) {
            if (is.null(model)) return(NULL)

            tryCatch({
                message("[DEBUG] Analyzing surrogate splits")

                # Extract surrogate information from model
                surrogate_info <- list()

                if (!is.null(model$splits)) {
                    splits_df <- as.data.frame(model$splits)

                    # Process each split
                    for (i in 1:nrow(splits_df)) {
                        split_info <- splits_df[i, ]

                        surrogate_info[[i]] <- list(
                            variable = rownames(splits_df)[i],
                            count = split_info$count %||% 0,
                            ncat = split_info$ncat %||% 0,
                            improve = split_info$improve %||% 0,
                            index = split_info$index %||% 0
                        )
                    }

                    # Create summary table
                    surrogate_summary <- data.frame(
                        Variable = sapply(surrogate_info, function(x) x$variable),
                        Count = sapply(surrogate_info, function(x) x$count),
                        Categories = sapply(surrogate_info, function(x) x$ncat),
                        Improvement = round(sapply(surrogate_info, function(x) x$improve), 4)
                    )

                    return(list(
                        surrogate_splits = surrogate_info,
                        summary_table = surrogate_summary
                    ))
                } else {
                    return(list(
                        surrogate_splits = list(),
                        summary_table = data.frame(
                            Variable = character(0),
                            Count = numeric(0),
                            Categories = numeric(0),
                            Improvement = numeric(0)
                        )
                    ))
                }

            }, error = function(e) {
                message("[DEBUG] Surrogate split analysis failed: ", e$message)
                return(NULL)
            })
        },

        # Missing pattern analysis
        .analyze_missing_patterns = function(data) {
            tryCatch({
                message("[DEBUG] Analyzing missing patterns")

                # Calculate missing data statistics
                missing_stats <- data.frame(
                    Variable = names(data),
                    Missing_Count = sapply(data, function(x) sum(is.na(x))),
                    Missing_Percent = round(sapply(data, function(x) mean(is.na(x)) * 100), 2),
                    Data_Type = sapply(data, class)
                )

                # Create missing pattern matrix
                if (any(missing_stats$Missing_Count > 0)) {
                    # Find combinations of missing patterns
                    missing_matrix <- is.na(data)
                    pattern_combinations <- unique(missing_matrix)

                    pattern_summary <- data.frame(
                        Pattern_ID = seq_len(nrow(pattern_combinations)),
                        Count = sapply(1:nrow(pattern_combinations), function(i) {
                            sum(apply(missing_matrix, 1, function(x) all(x == pattern_combinations[i, ])))
                        })
                    )

                    # Add pattern descriptions
                    pattern_summary$Description <- sapply(1:nrow(pattern_combinations), function(i) {
                        missing_vars <- names(data)[pattern_combinations[i, ]]
                        if (length(missing_vars) == 0) {
                            "Complete cases"
                        } else {
                            paste("Missing:", paste(missing_vars, collapse = ", "))
                        }
                    })

                    return(list(
                        variable_stats = missing_stats,
                        pattern_summary = pattern_summary,
                        total_missing = sum(missing_stats$Missing_Count),
                        complete_cases = sum(complete.cases(data))
                    ))
                } else {
                    return(list(
                        variable_stats = missing_stats,
                        pattern_summary = data.frame(
                            Pattern_ID = 1,
                            Count = nrow(data),
                            Description = "Complete cases"
                        ),
                        total_missing = 0,
                        complete_cases = nrow(data)
                    ))
                }

            }, error = function(e) {
                message("[DEBUG] Missing pattern analysis failed: ", e$message)
                return(NULL)
            })
        }
    )
)
