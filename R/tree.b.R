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

                # Initialize progress feedback
                private$.update_progress("initializing", "Initializing analysis...")

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
                private$.update_progress("data_prep", "Preparing and validating data...")
                message("[DEBUG] Preparing data...")
                data_prepared <- private$.prepare_data()
                if (is.null(data_prepared)) {
                    message("[DEBUG] Data preparation failed, exiting")
                    return()
                }
                message("[DEBUG] Data prepared successfully")

                # Train model
                private$.update_algorithm_progress(self$options$algorithm, "training")
                message("[DEBUG] Training model with algorithm: ", self$options$algorithm)
                message("[DEBUG] Analysis mode: ", tree_mode)
                model_results <- private$.train_model(data_prepared, tree_mode)
                private$.model_results <- model_results

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
                if (self$options$show_performance_metrics) {
                    summary_html <- private$.generate_model_summary(model_results)
                    self$results$model_summary$setContent(summary_html)

                    # Performance table
                    private$.generate_performance_table(model_results)

                    # Clinical metrics table
                    private$.generate_clinical_metrics_table(model_results)

                    # Performance table explanation
                    perf_explanation <- private$.generate_performance_explanation()
                    self$results$performance_table_explanation$setContent(perf_explanation)
                }

                # Confusion matrix
                if (self$options$show_confusion_matrix) {
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

                # Bootstrap confidence intervals
                if (self$options$bootstrap_confidence) {
                    bootstrap_html <- private$.generate_bootstrap_results(model_results)
                    self$results$bootstrap_intervals$setContent(bootstrap_html)
                }

                # Model comparison
                if (self$options$model_comparison) {
                    private$.update_progress("model_comparison", "Comparing multiple algorithms...")
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
                return()
            }

            algorithm <- self$options$algorithm

            tryCatch({
                if (algorithm == "fftrees") {
                    # Plot FFTrees with appropriate labels
                    model <- private$.model_results$model

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
                    # Plot rpart tree with enhanced styling (Appsilon-style)
                    model <- private$.model_results$model
                    plot_style <- self$options$tree_plot_style %||% "standard"
                    show_node_stats <- self$options$show_node_statistics %||% FALSE

                    # Configure plot parameters based on style
                    plot_params <- private$.get_tree_plot_params(plot_style, show_node_stats)

                    # Apply enhanced rpart.plot styling
                    do.call(rpart.plot::rpart.plot, c(
                        list(
                            x = model,
                            main = paste("Enhanced CART Decision Tree -", toupper(plot_style), "Style"),
                            under = TRUE,
                            clip.right.labs = FALSE,
                            box.palette = private$.get_color_palette(plot_style),
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

                # Handle missing data with medical-appropriate methods
                missing_strategy <- "complete"  # Default for now

                if (self$options$imputeMissing) {
                    missing_strategy <- "simple"
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
                error_msg <- paste0("
                <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
                <h4>Data Preparation Error</h4>
                <p>", e$message, "</p>
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

        .train_model = function(data, tree_mode = "classification") {
            tryCatch({
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

                # Train model based on algorithm
                if (algorithm == "fftrees") {
                    # DEBUG: Starting FFTrees processing
                    message("[DEBUG] Starting FFTrees algorithm")
                    message("[DEBUG] Train data dimensions: ", nrow(train_data), " x ", ncol(train_data))
                    message("[DEBUG] Test data dimensions: ", nrow(test_data), " x ", ncol(test_data))
                    message("[DEBUG] Formula: ", deparse(model_formula))

                    # Check target variable levels for FFTrees (requires binary)
                    message("[DEBUG] Original target variable levels: ", paste(target_levels, collapse = ", "))
                    message("[DEBUG] Target variable class: ", class(train_data[[target_var]]))
                    message("[DEBUG] Target variable sample values: ", paste(head(train_data[[target_var]], 10), collapse = ", "))
                    message("[DEBUG] Selected positive level (targetLevel): ", self$options$targetLevel)

                    # Check if targetLevel actually exists in the data
                    if (!self$options$targetLevel %in% train_data[[target_var]]) {
                        message("[DEBUG] WARNING: targetLevel '", self$options$targetLevel, "' not found in train data!")
                        message("[DEBUG] Available values: ", paste(unique(train_data[[target_var]]), collapse = ", "))
                    }

                    # FFTrees requires binary classification
                    if (length(target_levels) != 2) {
                        message("[DEBUG] FFTrees requires binary classification. Converting to binary...")

                        message("[DEBUG] Before conversion - train target unique: ",
                               paste(unique(train_data[[target_var]]), collapse = ", "))
                        message("[DEBUG] Before conversion - test target unique: ",
                               paste(unique(test_data[[target_var]]), collapse = ", "))

                        train_data[[target_var]] <- factor(
                            ifelse(train_data[[target_var]] == positive_level, positive_level, "Other"),
                            levels = c("Other", positive_level)
                        )
                        test_data[[target_var]] <- factor(
                            ifelse(test_data[[target_var]] == positive_level, positive_level, "Other"),
                            levels = c("Other", positive_level)
                        )

                        negative_class <- "Other"

                        message("[DEBUG] After conversion - train target unique: ",
                               paste(unique(train_data[[target_var]]), collapse = ", "))
                        message("[DEBUG] After conversion - test target unique: ",
                               paste(unique(test_data[[target_var]]), collapse = ", "))
                        message("[DEBUG] Converted to binary: Other vs ", positive_level)
                        message("[DEBUG] New train target distribution: ",
                               paste(names(table(train_data[[target_var]])), "=",
                                     table(train_data[[target_var]]), collapse = ", "))
                        message("[DEBUG] New test target distribution: ",
                               paste(names(table(test_data[[target_var]])), "=",
                                     table(test_data[[target_var]]), collapse = ", "))
                    }

                    # Get positive and negative class labels from data attributes
                    positive_class <- attr(data, "positive_class")
                    if (is.null(negative_class)) {
                        negative_class <- attr(data, "negative_class")
                    }

                    message("[DEBUG] Positive class: ", ifelse(is.null(positive_class), "NULL", positive_class))
                    message("[DEBUG] Negative class: ", ifelse(is.null(negative_class), "NULL", negative_class))

                    # Set decision labels based on our selected positive class
                    other_levels <- setdiff(levels(train_data[[target_var]]), positive_level)
                    negative_level <- if(length(other_levels) == 1) other_levels[1] else "Other"

                    decision_labels <- c(paste("Predict", negative_level), paste("Predict", positive_level))
                    message("[DEBUG] Corrected decision labels: ", paste(decision_labels, collapse = ", "))
                    message("[DEBUG] About to call FFTrees::FFTrees()")

                    # Debug data structure being passed to FFTrees
                    message("[DEBUG] Train data structure:")
                    message("[DEBUG]   Columns: ", paste(names(train_data), collapse = ", "))
                    message("[DEBUG]   Target column '", target_var, "' class: ", class(train_data[[target_var]]))
                    message("[DEBUG]   Target column '", target_var, "' levels: ",
                           paste(levels(train_data[[target_var]]), collapse = ", "))
                    message("[DEBUG]   Formula to FFTrees: ", deparse(model_formula))

                    # Fix FFTrees criterion issue by converting target to logical ourselves
                    # This ensures FFTrees uses our selected positive class correctly
                    message("[DEBUG] Converting target to logical: ", target_var, " == '", positive_level, "'")

                    # Create logical version of target variable where TRUE = positive class
                    train_data[[paste0(target_var, "_logical")]] <- as.logical(train_data[[target_var]] == positive_level)
                    test_data[[paste0(target_var, "_logical")]] <- as.logical(test_data[[target_var]] == positive_level)

                    # Create new formula using the logical target
                    logical_target <- paste0(target_var, "_logical")
                    predictors <- setdiff(names(train_data), c(target_var, logical_target))
                    model_formula_logical <- as.formula(paste(logical_target, "~", paste(predictors, collapse = " + ")))

                    message("[DEBUG] New logical formula: ", deparse(model_formula_logical))
                    message("[DEBUG] Logical target distribution - Train: ",
                           paste(names(table(train_data[[logical_target]])), "=",
                                 table(train_data[[logical_target]]), collapse = ", "))

                    # Wrap FFTrees call in tryCatch for better error handling
                    tryCatch({
                        # Train FFTrees model with logical target
                        model <- FFTrees::FFTrees(
                            formula = model_formula_logical,
                            data = train_data,
                            data.test = test_data,
                            main = "Medical Decision Tree",
                            decision.labels = decision_labels,
                            quiet = list(ini = TRUE, fin = FALSE, mis = FALSE, set = TRUE)
                        )
                        message("[DEBUG] FFTrees model created successfully")

                        # Inspect FFTrees model structure
                        message("[DEBUG] FFTrees model class: ", class(model))
                        message("[DEBUG] FFTrees model names: ", paste(names(model), collapse = ", "))

                        if ("trees" %in% names(model)) {
                            message("[DEBUG] model$trees names: ", paste(names(model$trees), collapse = ", "))
                            if ("decisions" %in% names(model$trees)) {
                                message("[DEBUG] model$trees$decisions names: ", paste(names(model$trees$decisions), collapse = ", "))
                                if ("test" %in% names(model$trees$decisions)) {
                                    message("[DEBUG] model$trees$decisions$test class: ", class(model$trees$decisions$test))
                                    message("[DEBUG] model$trees$decisions$test dim: ",
                                           ifelse(is.null(dim(model$trees$decisions$test)), "NULL",
                                                  paste(dim(model$trees$decisions$test), collapse = " x ")))
                                }
                            }
                        }

                    }, error = function(e) {
                        message("[DEBUG] FFTrees error: ", e$message)
                        stop("FFTrees failed: ", e$message)
                    })

                    message("[DEBUG] FFTrees model created successfully")

                    # Get predictions using the reliable predict() method
                    message("[DEBUG] Extracting predictions using predict() method")

                    tryCatch({
                        # Use predict() method - most reliable approach
                        test_pred <- predict(model, newdata = test_data)
                        message("[DEBUG] predict() result class: ", class(test_pred))
                        message("[DEBUG] predict() result length: ", length(test_pred))
                        message("[DEBUG] predict() result sample: ", paste(head(test_pred, 5), collapse = ", "))

                        # Convert to logical
                        predictions_logical <- as.logical(test_pred)

                        # Convert logical to factor matching original levels
                        predictions_class <- factor(
                            ifelse(predictions_logical, positive_level, negative_class),
                            levels = c(negative_class, positive_level)
                        )

                        # Get probabilities
                        predictions_prob <- as.numeric(predictions_logical)

                        message("[DEBUG] Final predictions_prob sample: ", paste(head(predictions_prob, 5), collapse = ", "))
                        message("[DEBUG] Final predictions_class sample: ", paste(head(predictions_class, 5), collapse = ", "))

                    }, error = function(e) {
                        message("[DEBUG] predict() method failed: ", e$message)
                        message("[DEBUG] Trying alternative prediction extraction...")

                        # Fallback to complex extraction - keep original logic as backup
                        # FFTrees stores predictions as a nested list structure
                        if (!is.null(model$trees$decisions$test)) {
                            # Find the best tree - typically stored in model$trees$best$test
                            if (!is.null(model$trees$best) && !is.null(model$trees$best$test)) {
                                best_tree <- model$trees$best$test
                                message("[DEBUG] Using best tree index: ", best_tree)
                            } else {
                                best_tree <- 1
                                message("[DEBUG] Using default tree index: ", best_tree)
                            }

                            # model$trees$decisions$test is a list where each element contains multiple trees
                            if (is.list(model$trees$decisions$test) && length(model$trees$decisions$test) >= best_tree) {
                                # The structure appears to be decisions$test[[tree_index]]
                                tree_decisions <- model$trees$decisions$test[[best_tree]]
                                message("[DEBUG] Tree decisions class: ", class(tree_decisions))
                                message("[DEBUG] Tree decisions length: ", length(tree_decisions))

                                # If it's still a list, it might be per test case
                                if (is.list(tree_decisions)) {
                                    # Check if we have the right number of elements for our test set
                                    message("[DEBUG] Expected test cases: ", nrow(test_data))

                                    # Try different extraction methods
                                    if (length(tree_decisions) == nrow(test_data)) {
                                        # Each element is one prediction
                                        predictions_logical <- as.logical(unlist(tree_decisions))
                                        message("[DEBUG] Method 1 - Direct unlist - length: ", length(predictions_logical))
                                    } else {
                                        # Maybe it's structured differently - try just the first element
                                        if (length(tree_decisions) > 0 && is.logical(tree_decisions[[1]])) {
                                            # Take only the first set of predictions that matches our test size
                                            for (i in 1:length(tree_decisions)) {
                                                if (length(tree_decisions[[i]]) == nrow(test_data)) {
                                                    predictions_logical <- tree_decisions[[i]]
                                                    message("[DEBUG] Method 2 - Found matching length at position ", i, " - length: ", length(predictions_logical))
                                                    break
                                                }
                                            }
                                            if (!exists("predictions_logical")) {
                                                # Fallback - take first nrow(test_data) predictions
                                                all_predictions <- unlist(tree_decisions)
                                                predictions_logical <- all_predictions[1:nrow(test_data)]
                                                message("[DEBUG] Method 3 - Truncated to test size - length: ", length(predictions_logical))
                                            }
                                        } else {
                                            # Final fallback
                                            predictions_logical <- as.logical(unlist(tree_decisions))[1:nrow(test_data)]
                                            message("[DEBUG] Method 4 - Fallback truncation - length: ", length(predictions_logical))
                                        }
                                    }

                                    message("[DEBUG] Final predictions class: ", class(predictions_logical))
                                    message("[DEBUG] Final predictions sample: ", paste(head(predictions_logical, 10), collapse = ", "))
                                } else if (is.logical(tree_decisions)) {
                                    predictions_logical <- tree_decisions
                                    message("[DEBUG] Direct logical vector - length: ", length(predictions_logical))
                                } else {
                                    # Fallback: try to coerce to logical
                                    predictions_logical <- as.logical(tree_decisions)
                                    message("[DEBUG] Coerced to logical - length: ", length(predictions_logical))
                                }

                                # Convert logical to factor matching original levels
                                predictions_class <- factor(
                                    ifelse(predictions_logical, positive_level, "Other"),
                                    levels = c("Other", positive_level)
                                )

                                # Get probabilities - use proportion of positive predictions as rough estimate
                                predictions_prob <- as.numeric(predictions_logical)

                            } else {
                                message("[DEBUG] ERROR: Cannot access tree ", best_tree, " from decisions list")
                                message("[DEBUG] decisions$test length: ", length(model$trees$decisions$test))
                                stop("Cannot extract predictions from FFTrees model structure")
                            }
                        } else {
                            message("[DEBUG] No test decisions found, trying predict()")
                            # Alternative: make predictions manually
                            test_pred <- predict(model, newdata = test_data)
                            message("[DEBUG] fallback predict() result length: ", length(test_pred))

                            predictions_class <- factor(
                                ifelse(test_pred, positive_level, negative_class),
                                levels = c(negative_class, positive_level)
                            )
                            predictions_prob <- as.numeric(test_pred)
                        }
                    }) # Close the tryCatch for predict() method

                    message("[DEBUG] Predictions extracted - prob length: ", length(predictions_prob),
                           ", class length: ", length(predictions_class))

                } else if (algorithm == "rpart") {
                    message("[DEBUG] Training enhanced CART model")

                    # Determine if hyperparameter tuning is enabled
                    if (self$options$hyperparameter_tuning) {
                        private$.update_algorithm_progress(algorithm, "hyperparameter_tuning")
                        message("[DEBUG] Performing hyperparameter tuning for ", algorithm)
                        model <- private$.tune_hyperparameters(model_formula, train_data, test_data, algorithm)
                    } else {
                        # Train with user-specified parameters
                        message("[DEBUG] Using user-specified parameters")

                        # Handle missing data based on user preference
                        missing_method <- self$options$missing_data_method %||% "native"
                        if (missing_method == "clinical_impute") {
                            train_data <- private$.clinical_imputation(train_data, target_var)
                            test_data <- private$.clinical_imputation(test_data, target_var)
                        } else if (missing_method == "exclude") {
                            complete_vars <- c(self$options$vars, self$options$facs, target_var)
                            train_data <- train_data[complete.cases(train_data[complete_vars]), ]
                            test_data <- test_data[complete.cases(test_data[complete_vars]), ]
                        }
                        # native and multiple_impute handled by rpart automatically

                        # Check for ensemble method
                        ensemble_method <- self$options$ensemble_method %||% "none"

                        if (ensemble_method == "random_forest") {
                            message("[DEBUG] Training Random Forest ensemble")
                            if (!requireNamespace("randomForest", quietly = TRUE)) {
                                message("[DEBUG] randomForest package not available, falling back to single tree")
                                ensemble_method <- "none"
                            } else {
                                model <- randomForest::randomForest(
                                    model_formula,
                                    data = train_data,
                                    ntree = self$options$n_trees %||% 100,
                                    na.action = na.roughfix
                                )
                            }
                        } else if (ensemble_method == "bagging") {
                            message("[DEBUG] Training Bagged trees ensemble")
                            if (!requireNamespace("randomForest", quietly = TRUE)) {
                                message("[DEBUG] randomForest package not available, falling back to single tree")
                                ensemble_method <- "none"
                            } else {
                                # Bagging is random forest with all variables considered at each split
                                n_vars <- length(c(self$options$vars, self$options$facs))
                                model <- randomForest::randomForest(
                                    model_formula,
                                    data = train_data,
                                    ntree = self$options$n_trees %||% 100,
                                    mtry = n_vars,  # Use all variables for bagging
                                    na.action = na.roughfix
                                )
                            }
                        }

                        if (ensemble_method == "none") {
                            # Single tree with tidymodels-style parameters
                            model <- rpart::rpart(
                                model_formula,
                                data = train_data,
                                control = rpart::rpart.control(
                                    maxdepth = self$options$tree_depth %||% self$options$max_depth %||% 6,
                                    minsplit = self$options$min_n %||% self$options$min_samples_split %||% 20,
                                    minbucket = self$options$min_samples_leaf %||% 10,
                                    cp = self$options$cost_complexity %||% self$options$complexity_parameter %||% 0.01
                                )
                            )
                        }
                    }

                    message("[DEBUG] Model training completed, making predictions")

                    # Make predictions based on model type
                    ensemble_method <- self$options$ensemble_method %||% "none"
                    if (ensemble_method %in% c("random_forest", "bagging")) {
                        # Ensemble predictions
                        predictions_prob <- predict(model, test_data, type = "prob")[, 2]
                        predictions_class <- predict(model, test_data, type = "class")
                    } else {
                        # Single tree predictions
                        predictions_prob <- predict(model, test_data, type = "prob")[, 2]
                        predictions_class <- predict(model, test_data, type = "class")
                    }

                    message("[DEBUG] Predictions completed")

                } else if (algorithm == "c50") {
                    message("[DEBUG] Training C5.0 classification model")

                    if (!requireNamespace("C50", quietly = TRUE)) {
                        stop("C50 package required but not installed. Please install with: install.packages('C50')")
                    }

                    # Determine if hyperparameter tuning is enabled
                    if (self$options$hyperparameter_tuning) {
                        message("[DEBUG] Performing hyperparameter tuning for C5.0")
                        model <- private$.tune_hyperparameters(model_formula, train_data, test_data, algorithm)

                        # Make predictions with tuned model
                        if (!is.null(model)) {
                            predictions_class <- predict(model, test_data, type = "class")
                            predictions_prob_matrix <- predict(model, test_data, type = "prob")
                            predictions_prob <- predictions_prob_matrix[, 2]  # Probability of positive class
                        }
                    } else {
                        # C5.0 parameters
                        trials <- self$options$c50_trials %||% 1
                        winnow <- self$options$c50_winnow %||% FALSE

                        # Train C5.0 model
                        model <- C50::C5.0(
                            model_formula,
                            data = train_data,
                            trials = trials,
                            winnow = winnow,
                            control = C50::C5.0Control(
                                minCases = self$options$min_n %||% 20,
                                CF = 1 - (self$options$cost_complexity %||% 0.01)  # Convert CP to confidence factor
                            )
                        )

                        # Make predictions
                        predictions_class <- predict(model, test_data, type = "class")
                        predictions_prob_matrix <- predict(model, test_data, type = "prob")
                        predictions_prob <- predictions_prob_matrix[, 2]  # Probability of positive class
                    }

                    message("[DEBUG] C5.0 model training completed")

                } else if (algorithm == "ctree") {
                    message("[DEBUG] Training conditional inference tree")

                    if (!requireNamespace("partykit", quietly = TRUE)) {
                        stop("partykit package required but not installed. Please install with: install.packages('partykit')")
                    }

                    # ctree parameters
                    mincriterion <- self$options$ctree_mincriterion %||% 0.95
                    minsplit <- self$options$min_n %||% 20
                    maxdepth <- self$options$tree_depth %||% 6

                    # Train ctree model
                    model <- partykit::ctree(
                        model_formula,
                        data = train_data,
                        control = partykit::ctree_control(
                            mincriterion = mincriterion,
                            minsplit = minsplit,
                            maxdepth = maxdepth
                        )
                    )

                    # Make predictions
                    predictions_class <- predict(model, test_data, type = "response")
                    predictions_prob <- predict(model, test_data, type = "prob")[, 2]

                    message("[DEBUG] ctree model training completed")

                } else if (algorithm == "mob") {
                    message("[DEBUG] Training model-based tree")

                    if (!requireNamespace("partykit", quietly = TRUE)) {
                        stop("partykit package required but not installed. Please install with: install.packages('partykit')")
                    }

                    # For mob, we need to specify a parametric model for the nodes
                    # Using logistic regression as default for classification
                    mob_formula <- as.formula(paste(target_var, "~ 1 |", paste(c(self$options$vars, self$options$facs), collapse = " + ")))

                    model <- partykit::mob(
                        mob_formula,
                        data = train_data,
                        fit = partykit::glmfit,
                        family = binomial(),
                        control = partykit::mob_control(
                            minsize = self$options$min_n %||% 20,
                            maxdepth = self$options$tree_depth %||% 6
                        )
                    )

                    # Make predictions
                    predictions_prob <- predict(model, test_data, type = "response")
                    predictions_class <- factor(
                        ifelse(predictions_prob > 0.5, positive_level, negative_class),
                        levels = c(negative_class, positive_level)
                    )

                    message("[DEBUG] MOB model training completed")

                } else if (algorithm == "randomforest") {
                    message("[DEBUG] Training Random Forest model")

                    if (!requireNamespace("randomForest", quietly = TRUE)) {
                        stop("randomForest package required but not installed. Please install with: install.packages('randomForest')")
                    }

                    # Determine if hyperparameter tuning is enabled
                    if (self$options$hyperparameter_tuning) {
                        message("[DEBUG] Performing hyperparameter tuning for Random Forest")
                        model <- private$.tune_hyperparameters(model_formula, train_data, test_data, algorithm)
                    } else {
                        # Random Forest parameters
                        ntree <- self$options$rf_ntree
                        mtry <- self$options$rf_mtry

                        # Auto-select mtry if not specified (0 = automatic)
                        if (mtry == 0) {
                            n_predictors <- length(c(self$options$vars, self$options$facs))
                            mtry <- if (tree_mode == "classification") {
                                max(1, floor(sqrt(n_predictors)))
                            } else {
                                max(1, floor(n_predictors / 3))
                            }
                        }

                        # Train Random Forest model
                        model <- randomForest::randomForest(
                            model_formula,
                            data = train_data,
                            ntree = ntree,
                            mtry = mtry,
                            na.action = na.roughfix,
                            importance = TRUE,  # For feature importance
                            keep.forest = TRUE
                        )
                    }

                    # Make predictions
                    if (tree_mode == "classification") {
                        predictions_prob <- predict(model, test_data, type = "prob")
                        if (ncol(predictions_prob) == 2) {
                            # Binary classification - get probability of positive class
                            target_level_idx <- which(colnames(predictions_prob) == target_level)
                            if (length(target_level_idx) > 0) {
                                predictions_prob <- predictions_prob[, target_level_idx]
                            } else {
                                predictions_prob <- predictions_prob[, 2]
                            }
                        } else {
                            # Multi-class - use first class probability as default
                            predictions_prob <- predictions_prob[, 1]
                        }
                        predictions_class <- predict(model, test_data, type = "class")
                    } else {
                        # Regression mode
                        predictions_prob <- predict(model, test_data)
                        predictions_class <- predictions_prob
                    }

                    message("[DEBUG] Random Forest model training completed")

                } else if (algorithm == "xgboost") {
                    message("[DEBUG] Training XGBoost model")

                    if (!requireNamespace("xgboost", quietly = TRUE)) {
                        stop("xgboost package required but not installed. Please install with: install.packages('xgboost')")
                    }

                    # Determine if hyperparameter tuning is enabled
                    if (self$options$hyperparameter_tuning) {
                        message("[DEBUG] Performing hyperparameter tuning for XGBoost")
                        model <- private$.tune_hyperparameters(model_formula, train_data, test_data, algorithm)

                        # For tuned XGBoost, predictions are handled differently
                        if (!is.null(model)) {
                            # Prepare test data for XGBoost predictions
                            test_prepared <- private$.prepare_data_for_xgboost(test_data, target_var)
                            test_matrix <- xgboost::xgb.DMatrix(test_prepared$X, label = test_prepared$y)

                            predictions_prob <- predict(model, test_matrix)
                            predictions_class <- factor(
                                ifelse(predictions_prob > 0.5, target_level,
                                       levels(test_data[[target_var]])[levels(test_data[[target_var]]) != target_level]),
                                levels = levels(test_data[[target_var]])
                            )
                        }
                    } else {
                        # XGBoost parameters
                        nrounds <- self$options$xgb_nrounds
                        eta <- self$options$xgb_eta
                        max_depth <- self$options$tree_depth

                        # Prepare data for XGBoost
                        tryCatch({
                            # Get predictor columns
                            pred_vars <- c(self$options$vars, self$options$facs)

                            # Prepare training data
                            train_x <- train_data[pred_vars]
                            train_y <- train_data[[target_var]]
                            test_x <- test_data[pred_vars]

                            # Convert categorical variables to numeric
                            for (var in self$options$facs) {
                                if (var %in% names(train_x)) {
                                    # Convert to numeric codes
                                    if (is.factor(train_x[[var]])) {
                                        # Ensure both train and test have same levels
                                        all_levels <- unique(c(levels(train_x[[var]]), levels(test_x[[var]])))
                                        train_x[[var]] <- factor(train_x[[var]], levels = all_levels)
                                        test_x[[var]] <- factor(test_x[[var]], levels = all_levels)

                                        train_x[[var]] <- as.numeric(train_x[[var]]) - 1
                                        test_x[[var]] <- as.numeric(test_x[[var]]) - 1
                                    } else {
                                        train_x[[var]] <- as.numeric(as.factor(train_x[[var]])) - 1
                                        test_x[[var]] <- as.numeric(as.factor(test_x[[var]])) - 1
                                    }
                                }
                            }

                            # Convert to matrix
                            train_matrix <- as.matrix(train_x)
                            test_matrix <- as.matrix(test_x)

                            if (tree_mode == "classification") {
                                # Binary classification
                                train_label <- as.numeric(train_y == target_level)

                                # Create DMatrix
                                dtrain <- xgboost::xgb.DMatrix(data = train_matrix, label = train_label)
                                dtest <- xgboost::xgb.DMatrix(data = test_matrix)

                                # Train model
                                model <- xgboost::xgboost(
                                    data = dtrain,
                                    nrounds = nrounds,
                                    eta = eta,
                                    max_depth = max_depth,
                                    objective = "binary:logistic",
                                    eval_metric = "auc",
                                    verbose = 0,
                                    nthread = 1  # For reproducibility
                                )

                                # Make predictions
                                predictions_prob <- predict(model, dtest)
                                predictions_class <- factor(
                                    ifelse(predictions_prob > 0.5, target_level,
                                           setdiff(levels(train_y), target_level)[1]),
                                    levels = levels(train_y)
                                )
                            } else {
                                # Regression mode
                                train_label <- as.numeric(train_y)

                                dtrain <- xgboost::xgb.DMatrix(data = train_matrix, label = train_label)
                                dtest <- xgboost::xgb.DMatrix(data = test_matrix)

                                model <- xgboost::xgboost(
                                    data = dtrain,
                                    nrounds = nrounds,
                                    eta = eta,
                                    max_depth = max_depth,
                                    objective = "reg:squarederror",
                                    eval_metric = "rmse",
                                    verbose = 0,
                                    nthread = 1
                                )

                                predictions_prob <- predict(model, dtest)
                                predictions_class <- predictions_prob
                            }

                            # Store additional information for XGBoost
                            model$feature_names <- colnames(train_matrix)
                            model$xgb_train_matrix <- train_matrix
                            model$target_levels <- levels(train_y)

                        }, error = function(e) {
                            stop(paste("XGBoost training error:", e$message))
                        })

                        message("[DEBUG] XGBoost model training completed")
                    }
                }

                # Handle different analysis modes
                if (tree_mode == "survival") {
                    message("[DEBUG] Survival analysis mode - adapting predictions")

                    # For survival analysis, we need time and status variables
                    # This would require additional UI elements for time/status specification
                    # For now, provide warning that this is experimental
                    warning("Survival analysis mode is experimental. Ensure proper time/status variables.")

                } else if (tree_mode == "regression") {
                    message("[DEBUG] Regression mode - continuous outcome prediction")

                    # For regression, predictions_prob should be the predicted values
                    # and predictions_class should be discretized for evaluation
                    if (algorithm %in% c("rpart", "ctree", "mob")) {
                        # Already handled by predict() methods above
                        if (exists("predictions_class") && is.factor(predictions_class)) {
                            # Convert back to numeric if needed
                            predictions_prob <- as.numeric(predictions_prob)
                        }
                    }
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
                error_type <- "Unknown"
                specific_help <- ""

                # Provide specific guidance based on error type
                if (grepl("contrasts can be applied only to factors", e$message)) {
                    error_type <- "Variable Type Error"
                    specific_help <- "<p><strong>Solution:</strong> Some predictor variables appear to be coded as text but contain numeric values. Convert them to proper numeric or factor variables in your data preparation.</p>"
                } else if (grepl("sample size", e$message)) {
                    error_type <- "Sample Size Error"
                    specific_help <- "<p><strong>Solutions:</strong><ul><li>Collect more data</li><li>Reduce number of predictor variables</li><li>Use simpler statistical methods</li></ul></p>"
                } else if (grepl("singularities", e$message) || grepl("rank-deficient", e$message)) {
                    error_type <- "Multicollinearity Error"
                    specific_help <- "<p><strong>Solutions:</strong><ul><li>Remove highly correlated predictor variables</li><li>Use feature selection</li><li>Check for duplicate or derived variables</li></ul></p>"
                } else if (grepl("levels", e$message) || grepl("factor", e$message)) {
                    error_type <- "Factor Level Error"
                    specific_help <- "<p><strong>Solutions:</strong><ul><li>Check that categorical variables have valid levels</li><li>Ensure target level exists in the target variable</li><li>Remove unused factor levels</li></ul></p>"
                } else if (grepl("memory", e$message) || grepl("cannot allocate", e$message)) {
                    error_type <- "Memory Error"
                    specific_help <- "<p><strong>Solutions:</strong><ul><li>Reduce dataset size</li><li>Use fewer predictor variables</li><li>Increase system memory</li></ul></p>"
                }

                message("[DEBUG] train_model ERROR: ", e$message)
                message("[DEBUG] Stack trace: ", deparse(sys.calls()))

                error_msg <- paste0("
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
                </div>")
                self$results$model_summary$setContent(error_msg)
                return(NULL)
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

                if (length(levels(actual)) == 2 && !is.null(self$options$targetLevel)) {
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
                }

                # Table is populated directly, no need to return

            }, error = function(e) {
                return(NULL)
            })
        },

        .generate_clinical_metrics_table = function(results) {

            tryCatch({
                actual <- results$actual
                predicted <- results$predictions_class

                if (length(levels(actual)) == 2 && !is.null(self$options$targetLevel)) {
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
                }

            }, error = function(e) {
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
                    show_welcome = FALSE,
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
                        show_welcome = FALSE,
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
                progress_bar <- paste0("
                <div style='width: 100%; background-color: #f5f5f5; border-radius: 10px; margin: 10px 0;'>
                    <div style='width: ", percent, "%; background-color: ", info$color, "; height: 20px; border-radius: 10px; transition: width 0.3s ease;'></div>
                </div>
                <div style='text-align: center; font-size: 12px; color: #666; margin-top: 5px;'>", percent, "% Complete</div>")
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

            # Update the progress feedback result
            self$results$progress_feedback$setContent(progress_html)

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
            } else {
                message <- paste0("Processing ", display_name, "...")
                details <- NULL
            }

            private$.update_progress(stage, message, details = details)
        },

        # Update progress for model comparison
        .update_comparison_progress = function(current_algo, total_algos, algorithm_name) {
            percent <- round((current_algo / total_algos) * 100)
            message <- "Comparing multiple algorithms..."
            details <- paste0("Currently evaluating: ", algorithm_name, " (", current_algo, " of ", total_algos, ")")

            private$.update_progress("model_comparison", message, percent, details)
        }
    )
)
