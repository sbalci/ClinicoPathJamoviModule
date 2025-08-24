#' @title Enhanced Correlation Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom stats cor.test complete.cases
#' @export

enhancedcorrelationClass <- R6::R6Class(
    "enhancedcorrelationClass",
    inherit = enhancedcorrelationBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$vars) || length(self$options$vars) < 2)
                return()
            
            # Set up instructions
            html <- self$results$instructions
            html$setContent(
                '<html>
                <head>
                </head>
                <body>
                <div class="instructions">
                <h3>Enhanced Correlation Analysis</h3>
                <p>This analysis provides correlation coefficients with confidence intervals and significance tests.</p>
                <ul>
                <li><b>Pearson:</b> Linear correlation for continuous variables</li>
                <li><b>Spearman:</b> Non-parametric correlation based on ranks</li>
                <li><b>Kendall:</b> Non-parametric correlation based on concordance</li>
                </ul>
                <p><b>Note:</b> Spearman and Kendall correlations are robust to outliers and non-linear relationships.</p>
                </div>
                </body>
                </html>'
            )
            
            # Initialize details table
            details <- self$results$text
            n_vars <- length(vars)
            n_pairs <- n_vars * (n_vars - 1) / 2
            
            if (n_pairs > 0) {
                details$addRows(n_pairs)
            }
            
        },
        
        .run = function() {
            if (is.null(self$data) || is.null(self$options$vars) || length(self$options$vars) < 2)
                return()
            
            vars <- self$options$vars
            method <- self$options$method
            ci_level <- self$options$ciWidth / 100
            alpha_level <- self$options$sigLevel
            
            # Get data
            data <- self$data
            for (var in vars) {
                data[[var]] <- jmvcore::toNumeric(data[[var]])
            }
            
            # Remove incomplete cases
            data <- data[complete.cases(data[vars]), vars, drop = FALSE]
            
            if (nrow(data) < 3) {
                self$results$instructions$setContent("Insufficient data for correlation analysis (need at least 3 complete cases).")
                return()
            }
            
            # Calculate detailed correlations with tests
            details <- self$results$text
            row_idx <- 1
            
            for (i in 1:(length(vars) - 1)) {
                for (j in (i + 1):length(vars)) {
                    var1 <- vars[i]
                    var2 <- vars[j]
                    
                    # Perform correlation test
                    test_result <- tryCatch({
                        cor.test(data[[var1]], data[[var2]], 
                                method = method, 
                                conf.level = ci_level)
                    }, error = function(e) NULL)
                    
                    if (!is.null(test_result)) {
                        # Format confidence interval
                        ci_text <- ""
                        if (self$options$ci && !is.null(test_result$conf.int)) {
                            ci_text <- sprintf("[%.3f, %.3f]", 
                                             test_result$conf.int[1], 
                                             test_result$conf.int[2])
                        }
                        
                        # Set row values
                        row_values <- list(
                            var1 = var1,
                            var2 = var2,
                            r = test_result$estimate,
                            rCI = ci_text,
                            p = test_result$p.value,
                            n = sum(complete.cases(data[[var1]], data[[var2]]))
                        )
                        
                        details$setRow(rowNo = row_idx, values = row_values)
                    }
                    
                    row_idx <- row_idx + 1
                }
            }
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(self$data) || is.null(self$options$vars) || length(self$options$vars) < 2)
                return()
            
            vars <- self$options$vars
            data <- self$data
            
            # Convert to numeric
            for (var in vars) {
                data[[var]] <- jmvcore::toNumeric(data[[var]])
            }
            
            # Remove incomplete cases
            data <- data[complete.cases(data[vars]), vars, drop = FALSE]
            
            if (nrow(data) < 3)
                return()
            
            # Create correlation plot matrix
            if (self$options$plotMatrix) {
                # Simple correlation heatmap
                cor_matrix <- cor(data[vars], method = self$options$method, use = "complete.obs")
                
                # Convert to long format for ggplot
                cor_long <- expand.grid(Var1 = vars, Var2 = vars)
                cor_long$Correlation <- as.vector(cor_matrix)
                
                p <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
                    geom_tile() +
                    geom_text(aes(label = sprintf("%.2f", Correlation)), color = "white", size = 3) +
                    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                       midpoint = 0, limit = c(-1, 1)) +
                    labs(title = paste("Correlation Matrix (", toupper(self$options$method), ")", sep = ""),
                         x = "", y = "") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))
                
                print(p)
                TRUE
            } else if (self$options$plotScatter) {
                # Create scatterplot matrix (simplified version)
                if (length(vars) == 2) {
                    p <- ggplot(data, aes_string(x = vars[1], y = vars[2])) +
                        geom_point(alpha = 0.6) +
                        geom_smooth(method = "loess", se = FALSE, color = "red") +
                        labs(title = paste("Scatterplot:", vars[1], "vs", vars[2])) +
                        theme_minimal()
                    
                    print(p)
                    TRUE
                } else {
                    # For more than 2 variables, show first pair
                    p <- ggplot(data, aes_string(x = vars[1], y = vars[2])) +
                        geom_point(alpha = 0.6) +
                        geom_smooth(method = "loess", se = FALSE, color = "red") +
                        labs(title = paste("Scatterplot:", vars[1], "vs", vars[2], "\n(First pair of variables)")) +
                        theme_minimal()
                    
                    print(p)
                    TRUE
                }
            } else {
                FALSE
            }
        }
    )
)