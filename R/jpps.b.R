
#' @title Predictive Power Score Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @export

jppsClass <- R6::R6Class(
    "jppsClass",
    inherit = jppsBase,
    private = list(
        .pps_results = NULL,
        .correlation_results = NULL,
        .prepared_data = NULL,
        
        .init = function() {
            analysis_type <- self$options$analysis_type
            
            # Show appropriate instructions based on analysis type
            if (analysis_type == "single" && (is.null(self$options$target_var) || is.null(self$options$predictor_var))) {
                private$.showInstructions("single")
            } else if (analysis_type == "predictors" && (is.null(self$options$target_var) || length(self$options$predictor_vars) == 0)) {
                private$.showInstructions("predictors")
            } else if (analysis_type == "matrix" && length(self$options$matrix_vars) < 2) {
                private$.showInstructions("matrix")
            } else if (analysis_type == "compare" && length(self$options$matrix_vars) < 2) {
                private$.showInstructions("compare")
            } else {
                private$.showInstructions("ready")
            }
        },
        
        .run = function() {
            analysis_type <- self$options$analysis_type
            
            # Check if we have required variables
            if (!private$.hasRequiredVariables()) {
                return()
            }
            
            # Prepare data
            data <- private$.prepareData()
            if (is.null(data)) return()
            
            # Perform PPS analysis
            private$.performPPSAnalysis(data)
            
            # Perform correlation comparison if requested
            if (self$options$show_correlation_comparison) {
                private$.performCorrelationComparison(data)
            }
            
            # Populate results
            private$.populatePPSScores()
            private$.populateCorrelationComparison()
            private$.populateSummaryStats()
            private$.populateInterpretation()
        },
        
        .hasRequiredVariables = function() {
            analysis_type <- self$options$analysis_type
            
            switch(analysis_type,
                "single" = !is.null(self$options$target_var) && !is.null(self$options$predictor_var),
                "predictors" = !is.null(self$options$target_var) && length(self$options$predictor_vars) > 0,
                "matrix" = length(self$options$matrix_vars) >= 2,
                "compare" = length(self$options$matrix_vars) >= 2,
                FALSE
            )
        },
        
        .showInstructions = function(type) {
            instructions <- switch(type,
                "single" = private$.createInstructionsHTML("single"),
                "predictors" = private$.createInstructionsHTML("predictors"),
                "matrix" = private$.createInstructionsHTML("matrix"),
                "compare" = private$.createInstructionsHTML("compare"),
                "ready" = private$.createInstructionsHTML("ready")
            )
            
            self$results$instructions$setContent(instructions)
        },
        
        .createInstructionsHTML = function(type) {
            base_style <- "
            <html>
            <head>
            <style>
                .main {
                    margin: 20px;
                    font-family: -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Arial,sans-serif;
                }
                .header {
                    color: #3f51b5;
                    font-size: 18px;
                    font-weight: bold;
                    margin-bottom: 10px;
                }
                .description {
                    margin-bottom: 20px;
                    line-height: 1.5;
                }
                .requirements {
                    background-color: #f5f5f5;
                    padding: 15px;
                    border-left: 4px solid #2196f3;
                    margin-bottom: 20px;
                }
                .steps {
                    counter-reset: step-counter;
                }
                .step {
                    counter-increment: step-counter;
                    margin-bottom: 10px;
                    padding-left: 30px;
                    position: relative;
                }
                .step::before {
                    content: counter(step-counter);
                    position: absolute;
                    left: 0;
                    background-color: #2196f3;
                    color: white;
                    width: 20px;
                    height: 20px;
                    border-radius: 50%;
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    font-size: 12px;
                    font-weight: bold;
                }
            </style>
            </head>
            <body>
            <div class='main'>
                <div class='header'>🎯 Predictive Power Score Analysis</div>"
            
            content <- switch(type,
                "single" = "
                <div class='description'>
                    Calculate PPS for a single predictor-target pair. PPS detects linear and 
                    non-linear relationships, providing an asymmetric score from 0 to 1.
                </div>
                <div class='requirements'>
                    <strong>Required:</strong><br>
                    • Target Variable: Variable to predict<br>
                    • Single Predictor: Variable used for prediction
                </div>",
                
                "predictors" = "
                <div class='description'>
                    Calculate PPS for multiple predictors against a target variable. 
                    Identifies which variables have the strongest predictive power.
                </div>
                <div class='requirements'>
                    <strong>Required:</strong><br>
                    • Target Variable: Variable to predict<br>
                    • Multiple Predictors: Variables to evaluate as predictors
                </div>",
                
                "matrix" = "
                <div class='description'>
                    Calculate PPS for all variable combinations in a matrix format. 
                    Shows bidirectional predictive relationships between all variables.
                </div>
                <div class='requirements'>
                    <strong>Required:</strong><br>
                    • Matrix Variables: At least 2 variables for PPS matrix
                </div>",
                
                "compare" = "
                <div class='description'>
                    Compare PPS with traditional correlation analysis. Shows where PPS 
                    detects relationships that correlation might miss.
                </div>
                <div class='requirements'>
                    <strong>Required:</strong><br>
                    • Matrix Variables: At least 2 variables for comparison
                </div>",
                
                "ready" = "
                <div class='description'>
                    PPS analysis ready! PPS uses machine learning to detect predictive 
                    relationships and can identify non-linear patterns that correlation analysis might miss.
                </div>
                <div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px; border-left: 4px solid #4caf50;'>
                    <strong>✓ Analysis configured correctly</strong><br>
                    Review your settings and examine the results below.
                </div>"
            )
            
            footer <- "
                <div style='margin-top: 20px; font-style: italic; color: #666;'>
                    Uses the ppsr package for Predictive Power Score calculations.
                </div>
            </div>
            </body>
            </html>"
            
            paste0(base_style, content, footer)
        },
        
        .prepareData = function() {
            data <- self$data
            analysis_type <- self$options$analysis_type
            
            # Get variables based on analysis type
            vars_to_use <- switch(analysis_type,
                "single" = c(self$options$target_var, self$options$predictor_var),
                "predictors" = c(self$options$target_var, self$options$predictor_vars),
                "matrix" = self$options$matrix_vars,
                "compare" = self$options$matrix_vars
            )
            
            if (length(vars_to_use) == 0) return(NULL)
            
            # Extract and clean data
            clean_data <- data[vars_to_use]
            
            # Remove missing values
            clean_data <- clean_data[complete.cases(clean_data), ]
            
            if (nrow(clean_data) == 0) {
                self$results$instructions$setContent(
                    "<div style='padding: 20px; color: #d32f2f;'>
                    <strong>❌ No complete cases</strong><br>
                    All selected variables have missing values in combination.
                    </div>"
                )
                return(NULL)
            }
            
            # Apply sample size limit if specified
            sample_size <- self$options$sample_size
            if (sample_size > 0 && nrow(clean_data) > sample_size) {
                set.seed(123)  # For reproducibility
                clean_data <- clean_data[sample(nrow(clean_data), sample_size), ]
            }
            
            private$.prepared_data <- clean_data
            return(clean_data)
        },
        
        .performPPSAnalysis = function(data) {
            if (!requireNamespace("ppsr", quietly = TRUE)) {
                self$results$instructions$setContent(
                    "<div style='padding: 20px; color: #d32f2f;'>
                    <strong>❌ Package Required</strong><br>
                    The 'ppsr' package is required for Predictive Power Score analysis.
                    </div>"
                )
                return()
            }
            
            tryCatch({
                analysis_type <- self$options$analysis_type
                algorithm <- self$options$algorithm
                cv_folds <- self$options$cv_folds
                
                # Set algorithm
                algo <- switch(algorithm,
                    "tree" = "tree",
                    "forest" = "forest", 
                    "auto" = "auto"
                )
                
                if (analysis_type == "single") {
                    # Single predictor vs target
                    target <- self$options$target_var
                    predictor <- self$options$predictor_var
                    
                    pps_result <- ppsr::score(
                        df = data,
                        x = predictor,
                        y = target,
                        algorithm = algo,
                        cv_folds = cv_folds
                    )
                    
                    private$.pps_results <- data.frame(
                        predictor = predictor,
                        target = target,
                        pps_score = pps_result$pps,
                        baseline_score = pps_result$baseline_score,
                        model_score = pps_result$model_score,
                        cross_validation = paste(cv_folds, "fold CV"),
                        stringsAsFactors = FALSE
                    )
                    
                } else if (analysis_type == "predictors") {
                    # Multiple predictors vs target
                    target <- self$options$target_var
                    predictors <- self$options$predictor_vars
                    
                    pps_result <- ppsr::score_predictors(
                        df = data,
                        y = target,
                        algorithm = algo,
                        cv_folds = cv_folds
                    )
                    
                    # Filter to selected predictors
                    pps_result <- pps_result[pps_result$x %in% predictors, ]
                    
                    private$.pps_results <- data.frame(
                        predictor = pps_result$x,
                        target = rep(target, nrow(pps_result)),
                        pps_score = pps_result$pps,
                        baseline_score = pps_result$baseline_score,
                        model_score = pps_result$model_score,
                        cross_validation = paste(cv_folds, "fold CV"),
                        stringsAsFactors = FALSE
                    )
                    
                } else if (analysis_type %in% c("matrix", "compare")) {
                    # Full matrix analysis
                    vars <- self$options$matrix_vars
                    
                    pps_result <- ppsr::score_matrix(
                        df = data[vars],
                        algorithm = algo,
                        cv_folds = cv_folds
                    )
                    
                    # Convert matrix to long format
                    pps_long <- expand.grid(
                        predictor = vars,
                        target = vars,
                        stringsAsFactors = FALSE
                    )
                    pps_long$pps_score <- as.vector(pps_result)
                    
                    # Remove self-predictions
                    pps_long <- pps_long[pps_long$predictor != pps_long$target, ]
                    
                    private$.pps_results <- data.frame(
                        predictor = pps_long$predictor,
                        target = pps_long$target,
                        pps_score = pps_long$pps_score,
                        baseline_score = NA,
                        model_score = NA,
                        cross_validation = paste(cv_folds, "fold CV"),
                        stringsAsFactors = FALSE
                    )
                }
                
            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<div style='padding: 20px; color: #d32f2f;'>
                    <strong>❌ PPS Analysis Error</strong><br>
                    ", e$message, "
                    </div>")
                )
                private$.pps_results <- NULL
            })
        },
        
        .performCorrelationComparison = function(data) {
            if (!self$options$show_correlation_comparison || is.null(private$.pps_results)) return()
            
            tryCatch({
                method <- self$options$correlation_method
                vars <- self$options$matrix_vars
                
                # Calculate correlation matrix
                cor_matrix <- cor(data[vars], method = method, use = "complete.obs")
                
                # Convert to long format
                cor_long <- expand.grid(
                    var1 = vars,
                    var2 = vars,
                    stringsAsFactors = FALSE
                )
                cor_long$correlation <- as.vector(cor_matrix)
                cor_long <- cor_long[cor_long$var1 != cor_long$var2, ]
                
                # Match with PPS results
                pps_subset <- private$.pps_results[private$.pps_results$predictor %in% vars & 
                                                 private$.pps_results$target %in% vars, ]
                
                # Create comparison dataframe
                comparison <- data.frame()
                for (i in 1:nrow(pps_subset)) {
                    pred <- pps_subset$predictor[i]
                    targ <- pps_subset$target[i]
                    
                    # Find corresponding correlation
                    cor_val <- cor_long$correlation[cor_long$var1 == pred & cor_long$var2 == targ]
                    if (length(cor_val) == 0) cor_val <- NA
                    
                    comparison <- rbind(comparison, data.frame(
                        variable_pair = paste(pred, "→", targ),
                        pps_score = pps_subset$pps_score[i],
                        correlation = abs(cor_val),  # Use absolute correlation
                        pps_advantage = pps_subset$pps_score[i] - abs(cor_val),
                        stringsAsFactors = FALSE
                    ))
                }
                
                private$.correlation_results <- comparison
                
            }, error = function(e) {
                private$.correlation_results <- NULL
            })
        },
        
        .populatePPSScores = function() {
            if (is.null(private$.pps_results)) return()
            
            table <- self$results$pps_scores
            
            # Apply threshold filter
            threshold <- self$options$min_pps_threshold
            results <- private$.pps_results[private$.pps_results$pps_score >= threshold, ]
            
            # Apply sorting
            sort_method <- self$options$sort_results
            if (sort_method == "pps_desc") {
                results <- results[order(-results$pps_score), ]
            } else if (sort_method == "pps_asc") {
                results <- results[order(results$pps_score), ]
            } else if (sort_method == "alphabetical") {
                results <- results[order(results$predictor, results$target), ]
            }
            
            # Populate table
            for (i in 1:nrow(results)) {
                table$addRow(rowKey = i, values = list(
                    predictor = results$predictor[i],
                    target = results$target[i],
                    pps_score = results$pps_score[i],
                    baseline_score = results$baseline_score[i],
                    model_score = results$model_score[i],
                    cross_validation = results$cross_validation[i]
                ))
            }
        },
        
        .populateCorrelationComparison = function() {
            if (!self$options$show_correlation_comparison || is.null(private$.correlation_results)) return()
            
            table <- self$results$correlation_comparison
            
            results <- private$.correlation_results
            
            for (i in 1:nrow(results)) {
                table$addRow(rowKey = i, values = list(
                    variable_pair = results$variable_pair[i],
                    pps_score = results$pps_score[i],
                    correlation = results$correlation[i],
                    pps_advantage = results$pps_advantage[i]
                ))
            }
        },
        
        .populateSummaryStats = function() {
            if (!self$options$show_summary || is.null(private$.pps_results)) return()
            
            table <- self$results$summary_stats
            
            results <- private$.pps_results
            
            # Calculate summary statistics
            n_relationships <- nrow(results)
            mean_pps <- mean(results$pps_score, na.rm = TRUE)
            max_pps <- max(results$pps_score, na.rm = TRUE)
            min_pps <- min(results$pps_score, na.rm = TRUE)
            threshold <- self$options$min_pps_threshold
            above_threshold <- sum(results$pps_score >= threshold)
            
            # Best predictor
            best_idx <- which.max(results$pps_score)
            best_predictor <- paste(results$predictor[best_idx], "→", results$target[best_idx])
            
            summary_data <- data.frame(
                statistic = c("Number of Relationships", "Mean PPS Score", "Maximum PPS Score", 
                            "Minimum PPS Score", "Above Threshold", "Best Predictor"),
                value = c(
                    as.character(n_relationships),
                    sprintf("%.4f", mean_pps),
                    sprintf("%.4f", max_pps),
                    sprintf("%.4f", min_pps),
                    sprintf("%d (%.1f%%)", above_threshold, 100*above_threshold/n_relationships),
                    paste0(best_predictor, " (", sprintf("%.4f", max_pps), ")")
                ),
                stringsAsFactors = FALSE
            )
            
            for (i in 1:nrow(summary_data)) {
                table$addRow(rowKey = i, values = summary_data[i, ])
            }
        },
        
        .populateInterpretation = function() {
            if (!self$options$show_interpretation) return()
            
            interpretation_html <- "
            <div style='font-family: -apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Arial,sans-serif; padding: 20px;'>
                <h3 style='color: #3f51b5; margin-bottom: 15px;'>🔍 PPS Interpretation Guide</h3>
                
                <div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px; border-left: 4px solid #4caf50; margin-bottom: 15px;'>
                    <strong>PPS Score Interpretation:</strong><br>
                    • <strong>0.0:</strong> No predictive power (random prediction)<br>
                    • <strong>0.0-0.2:</strong> Weak predictive relationship<br>
                    • <strong>0.2-0.5:</strong> Moderate predictive relationship<br>
                    • <strong>0.5-0.8:</strong> Strong predictive relationship<br>
                    • <strong>0.8-1.0:</strong> Very strong to perfect prediction
                </div>
                
                <div style='background-color: #f3e5f5; padding: 15px; border-radius: 8px; border-left: 4px solid #9c27b0; margin-bottom: 15px;'>
                    <strong>Key Advantages of PPS:</strong><br>
                    • Detects non-linear relationships that correlation might miss<br>
                    • Asymmetric: X→Y may differ from Y→X predictive power<br>
                    • Works with mixed data types (numeric, categorical)<br>
                    • Uses machine learning for robust relationship detection
                </div>
                
                <div style='background-color: #fff3e0; padding: 15px; border-radius: 8px; border-left: 4px solid #ff9800;'>
                    <strong>⚠️ Important Considerations:</strong><br>
                    • PPS is a \"quick and dirty\" exploration tool<br>
                    • High PPS doesn't necessarily imply causation<br>
                    • Results may vary with different algorithms and sample sizes<br>
                    • Consider domain knowledge when interpreting relationships<br>
                    • Use PPS to guide further, more detailed analysis
                </div>
            </div>"
            
            self$results$interpretation$setContent(interpretation_html)
        },
        
        .plot_heatmap = function(image, ggtheme, theme, ...) {
            if (is.null(private$.pps_results) || self$options$analysis_type != "matrix") return()
            
            # Check if ggplot2 is available
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                plot <- ggplot2::ggplot() + 
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "ggplot2 package required"), 
                                     size = 5) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::theme_void()
                print(plot)
                return(TRUE)
            }
            
            tryCatch({
                # Prepare data for heatmap
                results <- private$.pps_results
                
                # Create color scheme
                color_scheme <- self$options$color_scheme
                colors <- switch(color_scheme,
                    "viridis" = ggplot2::scale_fill_viridis_c(),
                    "blues" = ggplot2::scale_fill_gradient(low = "white", high = "blue"),
                    "reds" = ggplot2::scale_fill_gradient(low = "white", high = "red"),
                    "greens" = ggplot2::scale_fill_gradient(low = "white", high = "green"),
                    "custom" = ggplot2::scale_fill_gradient(
                        low = self$options$custom_color_low, 
                        high = self$options$custom_color_high
                    ),
                    ggplot2::scale_fill_viridis_c()
                )
                
                # Create plot
                plot <- ggplot2::ggplot(results, ggplot2::aes(x = predictor, y = target, fill = pps_score)) +
                    ggplot2::geom_tile(color = "white") +
                    colors +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                        plot.title = ggplot2::element_text(hjust = 0.5)
                    ) +
                    ggplot2::labs(
                        x = "Predictor",
                        y = "Target",
                        fill = "PPS Score",
                        title = ifelse(self$options$plot_title != "", 
                                     self$options$plot_title, 
                                     "Predictive Power Score Heatmap")
                    )
                
                # Add values if requested
                if (self$options$show_values_on_plot) {
                    plot <- plot + ggplot2::geom_text(
                        ggplot2::aes(label = sprintf("%.3f", pps_score)),
                        color = "black", size = 3
                    )
                }
                
                print(plot)
                TRUE
                
            }, error = function(e) {
                plot <- ggplot2::ggplot() + 
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = paste("Error:", e$message)), 
                                     size = 4) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::theme_void()
                print(plot)
                TRUE
            })
        },
        
        .plot_barplot = function(image, ggtheme, theme, ...) {
            if (is.null(private$.pps_results) || !self$options$analysis_type %in% c("single", "predictors")) return()
            
            tryCatch({
                results <- private$.pps_results
                
                # Apply sorting
                sort_method <- self$options$sort_results
                if (sort_method == "pps_desc") {
                    results <- results[order(-results$pps_score), ]
                } else if (sort_method == "pps_asc") {
                    results <- results[order(results$pps_score), ]
                }
                
                # Create predictor labels
                results$predictor_label <- factor(results$predictor, levels = results$predictor)
                
                # Create plot
                plot <- ggplot2::ggplot(results, ggplot2::aes(x = predictor_label, y = pps_score)) +
                    ggplot2::geom_col(fill = "steelblue", alpha = 0.7) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                        plot.title = ggplot2::element_text(hjust = 0.5)
                    ) +
                    ggplot2::labs(
                        x = "Predictor Variable",
                        y = "PPS Score",
                        title = ifelse(self$options$plot_title != "", 
                                     self$options$plot_title, 
                                     paste("PPS Scores for", results$target[1]))
                    ) +
                    ggplot2::ylim(0, 1)
                
                # Add values if requested
                if (self$options$show_values_on_plot) {
                    plot <- plot + ggplot2::geom_text(
                        ggplot2::aes(label = sprintf("%.3f", pps_score)),
                        vjust = -0.5, size = 3
                    )
                }
                
                print(plot)
                TRUE
                
            }, error = function(e) {
                plot <- ggplot2::ggplot() + 
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = paste("Error:", e$message)), 
                                     size = 4) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::theme_void()
                print(plot)
                TRUE
            })
        },
        
        .plot_comparison = function(image, ggtheme, theme, ...) {
            if (is.null(private$.correlation_results)) return()
            
            tryCatch({
                results <- private$.correlation_results
                
                # Create scatter plot comparing PPS vs Correlation
                plot <- ggplot2::ggplot(results, ggplot2::aes(x = correlation, y = pps_score)) +
                    ggplot2::geom_point(alpha = 0.7, size = 3) +
                    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
                    ggplot2::theme_minimal() +
                    ggplot2::labs(
                        x = paste(toupper(substring(self$options$correlation_method, 1, 1)), 
                                substring(self$options$correlation_method, 2), "Correlation (Absolute)"),
                        y = "PPS Score",
                        title = "PPS vs Correlation Comparison"
                    ) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
                
                print(plot)
                TRUE
                
            }, error = function(e) {
                plot <- ggplot2::ggplot() + 
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = paste("Error:", e$message)), 
                                     size = 4) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::theme_void()
                print(plot)
                TRUE
            })
        }
    )
)
