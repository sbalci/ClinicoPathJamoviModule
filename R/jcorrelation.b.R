#' @title Correlation Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'

jcorrelationClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jcorrelationClass",
    inherit = jcorrelationBase,
    private = list(
        .init = function() {
            # Initialize tables with dynamic columns based on selected variables
            if (length(self$options$vars) > 1) {
                private$.initMatrix()
                private$.initTests()
                private$.initSummary()
            }
        },
        
        .run = function() {
            # Validate inputs
            if (length(self$options$vars) < 2) {
                return()
            }

            if (nrow(self$data) == 0) {
                stop('Data contains no (complete) rows')
            }

            # Clean and prepare data
            data <- private$.cleanData()
            
            if (nrow(data) < 3) {
                stop('At least 3 complete observations are required for correlation analysis')
            }

            # Perform correlation analysis
            private$.populateMatrix(data)
            private$.populateTests(data)
            private$.populateSummary(data)
            
            # Generate natural language report
            if (self$options$report) {
                private$.generateReport(data)
            }

            # Prepare plot data
            if (self$options$plots) {
                plotData <- data[self$options$vars]
                self$results$plot$setState(plotData)
                self$results$plotMatrix$setState(plotData)
                self$results$plotPairs$setState(plotData)
            }
        },

        .cleanData = function() {
            # Select variables and handle grouping
            vars <- self$options$vars
            group <- self$options$group
            
            if (!is.null(group)) {
                vars <- c(vars, group)
            }
            
            data <- self$data[vars]
            
            # Convert to numeric and remove missing values
            for (var in self$options$vars) {
                data[[var]] <- jmvcore::toNumeric(data[[var]])
            }
            
            # Handle grouping variable
            if (!is.null(group)) {
                data[[group]] <- as.factor(data[[group]])
            }
            
            # Remove incomplete cases
            data <- jmvcore::naOmit(data)
            
            return(data)
        },

        .initMatrix = function() {
            matrix <- self$results$matrix
            vars <- self$options$vars
            
            # Add row variable column
            matrix$addColumn(
                name = "var",
                title = "Variable",
                type = "text"
            )
            
            # Add columns for each variable
            for (var in vars) {
                matrix$addColumn(
                    name = var,
                    title = var,
                    type = "text"
                )
            }
        },

        .initTests = function() {
            tests <- self$results$tests
            
            tests$addColumn(name = "var1", title = "Variable 1", type = "text")
            tests$addColumn(name = "var2", title = "Variable 2", type = "text")
            tests$addColumn(name = "r", title = "r", type = "number", format = "zto")
            tests$addColumn(name = "p", title = "p", type = "number", format = "zto,pvalue")
            tests$addColumn(name = "t", title = "t", type = "number", format = "zto")
            tests$addColumn(name = "df", title = "df", type = "integer")
            
            if (self$options$ci) {
                ciTitle <- paste0(self$options$ciWidth, "% Confidence Interval")
                tests$addColumn(name = "lower", title = "Lower", superTitle = ciTitle, type = "number", format = "zto")
                tests$addColumn(name = "upper", title = "Upper", superTitle = ciTitle, type = "number", format = "zto")
            }
            
            tests$addColumn(name = "n", title = "N", type = "integer")
        },

        .initSummary = function() {
            summary <- self$results$summary
            
            # Will be populated dynamically based on analysis
        },

        .populateMatrix = function(data) {
            matrix <- self$results$matrix
            vars <- self$options$vars
            method <- self$options$method
            alpha <- self$options$flagAlpha
            
            # Calculate correlation matrix
            cor_data <- data[vars]
            cor_matrix <- cor(cor_data, method = method, use = "complete.obs")
            
            # Calculate p-values
            n <- nrow(cor_data)
            p_matrix <- matrix(NA, ncol = length(vars), nrow = length(vars))
            rownames(p_matrix) <- colnames(p_matrix) <- vars
            
            for (i in 1:length(vars)) {
                for (j in 1:length(vars)) {
                    if (i != j) {
                        test_result <- cor.test(cor_data[[vars[i]]], cor_data[[vars[j]]], 
                                               method = method, 
                                               alternative = self$options$alternative)
                        p_matrix[i, j] <- test_result$p.value
                    } else {
                        p_matrix[i, j] <- 1.0
                    }
                }
            }
            
            # Populate matrix table
            for (i in 1:length(vars)) {
                row_values <- list(var = vars[i])
                
                for (j in 1:length(vars)) {
                    if (i == j) {
                        row_values[[vars[j]]] <- "—"
                    } else {
                        cor_val <- cor_matrix[i, j]
                        p_val <- p_matrix[i, j]
                        
                        # Format correlation with significance flagging
                        if (self$options$flag && p_val < alpha) {
                            if (p_val < 0.001) {
                                cor_text <- paste0(format(round(cor_val, 3), nsmall = 3), "***")
                            } else if (p_val < 0.01) {
                                cor_text <- paste0(format(round(cor_val, 3), nsmall = 3), "**")
                            } else if (p_val < 0.05) {
                                cor_text <- paste0(format(round(cor_val, 3), nsmall = 3), "*")
                            } else {
                                cor_text <- format(round(cor_val, 3), nsmall = 3)
                            }
                        } else {
                            cor_text <- format(round(cor_val, 3), nsmall = 3)
                        }
                        
                        row_values[[vars[j]]] <- cor_text
                    }
                }
                
                matrix$addRow(rowKey = i, values = row_values)
            }
        },

        .populateTests = function(data) {
            tests <- self$results$tests
            vars <- self$options$vars
            method <- self$options$method
            alternative <- self$options$alternative
            conf_level <- self$options$ciWidth / 100
            
            # Generate all pairwise combinations
            pairs <- combn(vars, 2, simplify = FALSE)
            
            rowNo <- 1
            for (pair in pairs) {
                var1 <- pair[1]
                var2 <- pair[2]
                
                # Checkpoint before each correlation test
                private$.checkpoint()
                
                # Perform correlation test
                test_result <- cor.test(data[[var1]], data[[var2]], 
                                       method = method, 
                                       alternative = alternative,
                                       conf.level = conf_level)
                
                # Extract values
                r <- test_result$estimate
                p <- test_result$p.value
                t_val <- if (!is.null(test_result$statistic)) test_result$statistic else NA
                df <- if (!is.null(test_result$parameter)) test_result$parameter else NA
                n <- sum(complete.cases(data[[var1]], data[[var2]]))
                
                row_values <- list(
                    var1 = var1,
                    var2 = var2,
                    r = r,
                    p = p,
                    n = n
                )
                
                if (!is.na(t_val)) {
                    row_values$t <- t_val
                }
                if (!is.na(df)) {
                    row_values$df <- df
                }
                
                # Add confidence intervals if requested
                if (self$options$ci && !is.null(test_result$conf.int)) {
                    row_values$lower <- test_result$conf.int[1]
                    row_values$upper <- test_result$conf.int[2]
                }
                
                tests$addRow(rowKey = rowNo, values = row_values)
                rowNo <- rowNo + 1
            }
        },

        .populateSummary = function(data) {
            summary <- self$results$summary
            vars <- self$options$vars
            
            if (length(vars) <= 2) return()
            
            # Calculate summary statistics
            cor_data <- data[vars]
            cor_matrix <- cor(cor_data, method = self$options$method, use = "complete.obs")
            
            # Remove diagonal elements
            cor_values <- cor_matrix[upper.tri(cor_matrix)]
            
            stats <- list(
                list(stat = "Number of variables", value = length(vars)),
                list(stat = "Number of correlations", value = length(cor_values)),
                list(stat = "Mean correlation", value = mean(cor_values, na.rm = TRUE)),
                list(stat = "Median correlation", value = median(cor_values, na.rm = TRUE)),
                list(stat = "Min correlation", value = min(cor_values, na.rm = TRUE)),
                list(stat = "Max correlation", value = max(cor_values, na.rm = TRUE)),
                list(stat = "SD correlation", value = sd(cor_values, na.rm = TRUE))
            )
            
            for (i in seq_along(stats)) {
                summary$addRow(rowKey = i, values = stats[[i]])
            }
        },

        .generateReport = function(data) {
            report_html <- "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>"
            report_html <- paste0(report_html, "<h4 style='color: #495057; margin-top: 0;'>Correlation Analysis Summary</h4>")
            
            # Check if correlation package is available for advanced reporting
            if (requireNamespace("correlation", quietly = TRUE) && requireNamespace("report", quietly = TRUE)) {
                tryCatch({
                    cor_analysis <- correlation::correlation(data[self$options$vars], method = self$options$method)
                    cor_report <- report::report(cor_analysis)
                    
                    report_text <- as.character(cor_report)
                    report_html <- paste0(report_html, 
                                         "<p style='margin-bottom: 0; line-height: 1.6; color: #212529;'>",
                                         gsub("\n", "<br>", report_text),
                                         "</p>")
                }, error = function(e) {
                    # Fallback to basic reporting
                    report_html <<- paste0(report_html, private$.basicReport(data))
                })
            } else {
                # Basic reporting
                report_html <- paste0(report_html, private$.basicReport(data))
            }
            
            report_html <- paste0(report_html, "</div>")
            self$results$report$setContent(report_html)
        },

        .basicReport = function(data) {
            vars <- self$options$vars
            method <- self$options$method
            n <- nrow(data)
            
            # Count significant correlations
            sig_count <- 0
            total_count <- 0
            
            pairs <- combn(vars, 2, simplify = FALSE)
            for (pair in pairs) {
                test_result <- cor.test(data[[pair[1]]], data[[pair[2]]], method = method)
                if (test_result$p.value < self$options$flagAlpha) {
                    sig_count <- sig_count + 1
                }
                total_count <- total_count + 1
            }
            
            method_name <- switch(method,
                                 "pearson" = "Pearson's",
                                 "spearman" = "Spearman's",
                                 "kendall" = "Kendall's")
            
            report <- paste0(
                "<p>", method_name, " correlation analysis was performed on ", length(vars), 
                " variables with ", n, " complete observations. ",
                "Out of ", total_count, " pairwise correlations, ", sig_count, 
                " (", round(sig_count/total_count * 100, 1), "%) were statistically significant ",
                "at α = ", self$options$flagAlpha, ".</p>"
            )
            
            return(report)
        },

        .plot = function(image, ggtheme, theme, ...) {
            if (length(self$options$vars) < 2 || !self$options$plots)
                return()

            plotData <- image$state
            
            if (is.null(plotData) || nrow(plotData) == 0)
                return()

            # Create basic correlation plot
            plot <- GGally::ggpairs(plotData, 
                                   method = self$options$method,
                                   title = paste("Correlation Analysis:", 
                                               stringr::str_to_title(self$options$method)))
            
            plot <- plot + ggtheme
            
            print(plot)
            TRUE
        },

        .plotMatrix = function(image, ggtheme, theme, ...) {
            if (length(self$options$vars) < 2 || !self$options$plots)
                return()

            plotData <- image$state
            
            if (is.null(plotData) || nrow(plotData) == 0)
                return()

            # Calculate correlation matrix
            cor_matrix <- cor(plotData, method = self$options$method, use = "complete.obs")
            
            if (requireNamespace("corrplot", quietly = TRUE)) {
                # Use corrplot for correlation matrix visualization
                corrplot::corrplot(cor_matrix, method = "color", type = "upper", 
                                  order = "hclust", tl.cex = 0.8, tl.col = "black")
            } else {
                # Fallback to ggplot2
                cor_df <- as.data.frame(as.table(cor_matrix))
                names(cor_df) <- c("Var1", "Var2", "Correlation")
                
                plot <- ggplot2::ggplot(cor_df, ggplot2::aes(Var1, Var2, fill = Correlation)) +
                    ggplot2::geom_tile() +
                    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                                                 midpoint = 0, limits = c(-1, 1)) +
                    ggplot2::theme_minimal() +
                    ggplot2::labs(title = "Correlation Matrix") +
                    ggtheme
                
                print(plot)
            }
            
            TRUE
        },

        .plotPairs = function(image, ggtheme, theme, ...) {
            if (length(self$options$vars) < 2 || !self$options$plots)
                return()

            plotData <- image$state
            
            if (is.null(plotData) || nrow(plotData) == 0)
                return()

            # Create pairs plot with correlation information
            plot <- GGally::ggpairs(plotData,
                                   upper = list(continuous = GGally::wrap("cor", method = self$options$method)),
                                   lower = list(continuous = "smooth"),
                                   diag = list(continuous = "densityDiag"),
                                   title = paste("Pairs Plot with", stringr::str_to_title(self$options$method), "Correlations"))
            
            plot <- plot + ggtheme
            
            print(plot)
            TRUE
        }
    )
)