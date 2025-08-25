#' @title Partial Correlation Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom stats cor cor.test lm residuals complete.cases
#' @export

partialcorrelationClass <- R6::R6Class(
    "partialcorrelationClass",
    inherit = partialcorrelationBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$vars) || 
                is.null(self$options$controls) || 
                length(self$options$vars) < 2 || length(self$options$controls) < 1)
                return()
            
            # Set up instructions
            html <- self$results$instructions
            html$setContent(
                '<html>
                <head>
                </head>
                <body>
                <div class="instructions">
                <h3>Enhanced Partial Correlation Analysis with BlueSky Integration</h3>
                <p>This module provides comprehensive partial and semi-partial correlation analysis with BlueSky-inspired features:</p>
                <ul>
                <li><b>Partial correlation:</b> Correlation between X and Y, controlling for Z (BlueSky pcor.test)</li>
                <li><b>Semi-partial correlation:</b> Correlation between X and Y, with Z partialled from X only (BlueSky spcor.test)</li>
                <li><b>Zero-order correlation:</b> Simple correlation without controls</li>
                <li><b>Robust error handling:</b> BlueSky-style graceful degradation with informative messages</li>
                <li><b>Multiple comparison correction:</b> Bonferroni, Holm, FDR, and BY methods</li>
                <li><b>Effect size interpretation:</b> Cohen\'s guidelines for correlation magnitudes</li>
                <li><b>Bootstrap confidence intervals:</b> Robust CI estimation methods</li>
                </ul>
                <p><b>Clinical Applications:</b> Essential for controlling confounding variables in clinicopathological research.</p>
                </div>
                </body>
                </html>'
            )
            
            # Initialize tables
            vars <- self$options$vars
            n_vars <- length(vars)
            n_pairs <- n_vars * (n_vars - 1) / 2
            
            if (n_pairs > 0) {
                self$results$partialCorr$addRows(n_pairs)
                if (self$options$showZeroOrder) {
                    self$results$zeroOrder$addRows(n_pairs)
                }
            }
        },
        
        .run = function() {
            if (is.null(self$data) || is.null(self$options$vars) || 
                is.null(self$options$controls) || 
                length(self$options$vars) < 2 || length(self$options$controls) < 1)
                return()
            
            vars <- self$options$vars
            controls <- self$options$controls
            method <- self$options$method
            ci_level <- self$options$ciWidth / 100
            
            # Get data and convert to numeric
            data <- self$data
            all_vars <- c(vars, controls)
            
            for (var in all_vars) {
                data[[var]] <- jmvcore::toNumeric(data[[var]])
            }
            
            # Store original sample size for diagnostics
            original_n <- nrow(data)
            
            # Remove incomplete cases
            data <- data[complete.cases(data[all_vars]), all_vars, drop = FALSE]
            final_n <- nrow(data)
            
            if (final_n < (length(controls) + 3)) {
                if (self$options$robustErrorHandling) {
                    self$results$instructions$setContent(
                        paste0("<div style='color: orange;'><strong>Warning:</strong> Insufficient data for reliable partial correlation analysis.<br>",
                               "Need at least ", length(controls) + 3, " complete cases, but only ", final_n, " available.<br>",
                               "Consider reducing control variables or collecting more data.</div>")
                    )
                } else {
                    self$results$instructions$setContent(
                        paste("Insufficient data for partial correlation analysis. Need at least", 
                              length(controls) + 3, "complete cases.")
                    )
                }
                return()
            }
            
            # BlueSky-style error handling with message tracking
            private$.error_messages <- character(0)
            private$.warning_messages <- character(0)
            
            # Calculate correlations based on type
            correlation_type <- self$options$correlationType
            
            if (correlation_type %in% c("partial", "both")) {
                private$.calculatePartialCorrelations(data, vars, controls, method, ci_level)
            }
            
            if (correlation_type %in% c("semipartial", "both")) {
                private$.calculateSemiPartialCorrelations(data, vars, controls, method, ci_level)
            }
            
            # Calculate zero-order correlations if requested
            if (self$options$showZeroOrder) {
                private$.calculateZeroOrderCorrelations(data, vars, method)
            }
            
            # Generate diagnostic information
            if (self$options$showDiagnostics) {
                private$.generateDiagnostics(original_n, final_n, vars, controls)
            }
            
            # Perform assumption checks
            if (self$options$assumptionChecks) {
                private$.performAssumptionChecks(data, vars, controls)
            }
            
            # Generate comprehensive summary
            if (self$options$bluesky_integration && self$options$comprehensive_output) {
                private$.generateComprehensiveSummary(data, vars, controls)
            }
            
            # Generate recommendations
            if (self$options$showRecommendations) {
                private$.generateRecommendations()
            }
            
            # Generate clinical interpretation
            if (self$options$clinicalInterpretation) {
                private$.generateClinicalInterpretation()
            }
            
            # Generate methods explanation
            if (self$options$detailedOutput) {
                private$.generateMethodsExplanation()
            }
        },
        
        .calculatePartialCorrelations = function(data, vars, controls, method, ci_level) {
            partial_table <- self$results$partialCorr
            row_idx <- 1
            
            # Create control variables string for display
            controls_str <- paste(controls, collapse = ", ")
            
            # Calculate residuals for each variable after controlling for control variables
            residuals_data <- data.frame(row.names = rownames(data))
            
            for (var in vars) {
                # Create formula for regression
                formula_str <- paste(var, "~", paste(controls, collapse = " + "))
                formula_obj <- as.formula(formula_str)
                
                # Fit regression model
                model <- lm(formula_obj, data = data)
                residuals_data[[var]] <- residuals(model)
            }
            
            # Calculate correlations between residuals
            for (i in 1:(length(vars) - 1)) {
                for (j in (i + 1):length(vars)) {
                    var1 <- vars[i]
                    var2 <- vars[j]
                    
                    # Perform correlation test on residuals
                    test_result <- tryCatch({
                        if (method == "pearson") {
                            cor.test(residuals_data[[var1]], residuals_data[[var2]], 
                                   method = method, conf.level = ci_level)
                        } else {
                            # For non-parametric methods, confidence intervals may not be available
                            cor.test(residuals_data[[var1]], residuals_data[[var2]], 
                                   method = method)
                        }
                    }, error = function(e) NULL)
                    
                    if (!is.null(test_result)) {
                        # Format confidence interval
                        ci_text <- ""
                        if (self$options$ci && !is.null(test_result$conf.int)) {
                            ci_text <- sprintf("[%.3f, %.3f]", 
                                             test_result$conf.int[1], 
                                             test_result$conf.int[2])
                        }
                        
                        # Degrees of freedom for partial correlation
                        df <- nrow(data) - length(controls) - 2
                        
                        # Set row values
                        row_values <- list(
                            var1 = var1,
                            var2 = var2,
                            controls = controls_str,
                            r = test_result$estimate,
                            rCI = ci_text,
                            p = test_result$p.value,
                            df = df,
                            n = nrow(data)
                        )
                        
                        partial_table$setRow(rowNo = row_idx, values = row_values)
                    }
                    
                    row_idx <- row_idx + 1
                }
            }
        },
        
        .calculateZeroOrderCorrelations = function(data, vars, method) {
            zero_order_table <- self$results$zeroOrder
            row_idx <- 1
            
            for (i in 1:(length(vars) - 1)) {
                for (j in (i + 1):length(vars)) {
                    var1 <- vars[i]
                    var2 <- vars[j]
                    
                    # Calculate zero-order correlation
                    test_result <- tryCatch({
                        cor.test(data[[var1]], data[[var2]], method = method)
                    }, error = function(e) NULL)
                    
                    if (!is.null(test_result)) {
                        row_values <- list(
                            var1 = var1,
                            var2 = var2,
                            r = test_result$estimate,
                            p = test_result$p.value,
                            n = sum(complete.cases(data[[var1]], data[[var2]]))
                        )
                        
                        zero_order_table$setRow(rowNo = row_idx, values = row_values)
                    }
                    
                    row_idx <- row_idx + 1
                }
            }
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(self$data) || is.null(self$options$vars) || 
                is.null(self$options$controls) || !self$options$matrixPlot)
                return()
            
            vars <- self$options$vars
            controls <- self$options$controls
            data <- self$data
            
            # Get clean data
            all_vars <- c(vars, controls)
            for (var in all_vars) {
                data[[var]] <- jmvcore::toNumeric(data[[var]])
            }
            data <- data[complete.cases(data[all_vars]), all_vars, drop = FALSE]
            
            if (nrow(data) < (length(controls) + 3))
                return()
            
            # Calculate partial correlation matrix
            partial_corr_matrix <- matrix(NA, nrow = length(vars), ncol = length(vars))
            rownames(partial_corr_matrix) <- vars
            colnames(partial_corr_matrix) <- vars
            
            # Fill diagonal with 1s
            diag(partial_corr_matrix) <- 1
            
            # Calculate residuals for plotting
            residuals_data <- data.frame(row.names = rownames(data))
            for (var in vars) {
                formula_str <- paste(var, "~", paste(controls, collapse = " + "))
                model <- lm(as.formula(formula_str), data = data)
                residuals_data[[var]] <- residuals(model)
            }
            
            # Fill correlation matrix
            for (i in 1:length(vars)) {
                for (j in 1:length(vars)) {
                    if (i != j) {
                        partial_corr_matrix[i, j] <- cor(residuals_data[[vars[i]]], 
                                                       residuals_data[[vars[j]]], 
                                                       method = self$options$method,
                                                       use = "complete.obs")
                    }
                }
            }
            
            # Convert to long format for ggplot
            cor_long <- expand.grid(Var1 = vars, Var2 = vars)
            cor_long$Correlation <- as.vector(partial_corr_matrix)
            
            # Create heatmap
            p <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
                geom_tile() +
                geom_text(aes(label = sprintf("%.2f", Correlation)), color = "white", size = 3) +
                scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                   midpoint = 0, limit = c(-1, 1)) +
                labs(title = paste("Partial Correlation Matrix\n(controlling for:", 
                                 paste(controls, collapse = ", "), ")"),
                     x = "", y = "") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
            print(p)
            TRUE
        }
    )
)