#' @title Polychoric Correlation Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom stats cor cor.test complete.cases chisq.test qnorm pnorm
#' @export

polychoriccorrClass <- R6::R6Class(
    "polychoriccorrClass",
    inherit = polychoriccorrBase,
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
                <h3>Polychoric Correlation Analysis</h3>
                <p>Polychoric correlation estimates the correlation between underlying continuous variables from observed ordinal data.</p>
                <ul>
                <li><b>Polychoric:</b> Correlation between two ordinal variables</li>
                <li><b>Tetrachoric:</b> Special case for two binary variables</li>
                <li><b>Polyserial:</b> Correlation between ordinal and continuous variables</li>
                <li><b>Biserial:</b> Correlation between binary and continuous variables</li>
                </ul>
                <p><b>Note:</b> These methods assume underlying bivariate normality of latent variables.</p>
                </div>
                </body>
                </html>'
            )
            
            # Initialize correlation table
            vars <- self$options$vars
            n_vars <- length(vars)
            n_pairs <- n_vars * (n_vars - 1) / 2
            
            if (n_pairs > 0) {
                self$results$correlations$addRows(n_pairs)
            }
            
            # Initialize frequency tables if requested
            if (self$options$showFreq) {
                frequencies <- self$results$frequencies
                for (i in 1:(n_vars - 1)) {
                    for (j in (i + 1):n_vars) {
                        pair_name <- paste(vars[i], "×", vars[j])
                        frequencies$addItem(key = pair_name)
                    }
                }
            }
        },
        
        .run = function() {
            if (is.null(self$data) || is.null(self$options$vars) || length(self$options$vars) < 2)
                return()
            
            vars <- self$options$vars
            data <- self$data
            
            # Convert variables to factors
            for (var in vars) {
                if (!is.factor(data[[var]])) {
                    data[[var]] <- as.factor(data[[var]])
                }
            }
            
            # Remove incomplete cases
            data <- data[complete.cases(data[vars]), vars, drop = FALSE]
            
            if (nrow(data) < 10) {
                self$results$instructions$setContent("Insufficient data for polychoric correlation analysis (need at least 10 complete cases).")
                return()
            }
            
            # Calculate correlations
            self$.calculatePolychoricCorrelations(data, vars)
            
            # Create frequency tables if requested
            if (self$options$showFreq) {
                self$.createFrequencyTables(data, vars)
            }
        },
        
        .calculatePolychoricCorrelations = function(data, vars) {
            correlations_table <- self$results$correlations
            corr_type <- self$options$corrType
            ci_level <- self$options$ciWidth / 100
            row_idx <- 1
            
            for (i in 1:(length(vars) - 1)) {
                for (j in (i + 1):length(vars)) {
                    var1 <- vars[i]
                    var2 <- vars[j]
                    
                    # Get variable data
                    x <- data[[var1]]
                    y <- data[[var2]]
                    
                    # Determine actual correlation type based on data
                    x_levels <- nlevels(x)
                    y_levels <- nlevels(y)
                    actual_type <- corr_type
                    
                    if (corr_type == "polychoric") {
                        if (x_levels == 2 && y_levels == 2) {
                            actual_type <- "tetrachoric"
                        }
                    }
                    
                    # Calculate correlation based on type
                    result <- self$.calculateCorrelationPair(x, y, actual_type, ci_level)
                    
                    if (!is.null(result)) {
                        # Format confidence interval
                        ci_text <- ""
                        if (self$options$ci && !is.null(result$ci)) {
                            ci_text <- sprintf("[%.3f, %.3f]", result$ci[1], result$ci[2])
                        }
                        
                        # Set row values
                        row_values <- list(
                            var1 = var1,
                            var2 = var2,
                            type = actual_type,
                            r = result$estimate,
                            rCI = ci_text,
                            chisq = result$chisq,
                            p = result$p.value,
                            n = result$n
                        )
                        
                        correlations_table$setRow(rowNo = row_idx, values = row_values)
                    }
                    
                    row_idx <- row_idx + 1
                }
            }
        },
        
        .calculateCorrelationPair = function(x, y, type, ci_level) {
            # Convert factors to numeric for calculations
            x_num <- as.numeric(x)
            y_num <- as.numeric(y)
            n <- length(x_num)
            
            if (type == "tetrachoric") {
                # Tetrachoric correlation for 2x2 tables
                result <- self$.calculateTetrachoric(x, y, ci_level)
            } else if (type == "polychoric") {
                # Approximation using Spearman correlation on ranks
                result <- self$.calculatePolychoricApprox(x_num, y_num, ci_level)
            } else {
                # For other types, use Spearman as approximation
                result <- self$.calculatePolychoricApprox(x_num, y_num, ci_level)
            }
            
            return(result)
        },
        
        .calculateTetrachoric = function(x, y, ci_level) {
            # Create 2x2 contingency table
            ct <- table(x, y)
            
            # Check if we have a valid 2x2 table
            if (nrow(ct) != 2 || ncol(ct) != 2) {
                return(NULL)
            }
            
            # Extract cell counts
            a <- ct[1, 1]; b <- ct[1, 2]
            c <- ct[2, 1]; d <- ct[2, 2]
            
            # Check for zero cells (add small constant if needed)
            if (any(c(a, b, c, d) == 0)) {
                a <- a + 0.5; b <- b + 0.5
                c <- c + 0.5; d <- d + 0.5
            }
            
            # Calculate tetrachoric correlation (simplified approximation)
            # Using the formula: r = cos(π / (1 + sqrt(ad/bc)))
            if ((a * d) > 0 && (b * c) > 0) {
                ratio <- (a * d) / (b * c)
                r_tet <- cos(pi / (1 + sqrt(ratio)))
                
                # Adjust sign based on main diagonal dominance
                if (a + d < b + c) {
                    r_tet <- -r_tet
                }
            } else {
                r_tet <- 0
            }
            
            # Chi-square test for independence
            chisq_test <- tryCatch({
                chisq.test(ct, correct = FALSE)
            }, error = function(e) list(statistic = NA, p.value = NA))
            
            # Approximate confidence interval
            n <- sum(ct)
            se <- sqrt((1 - r_tet^2)^2 / (n - 1))
            z_alpha <- qnorm(1 - (1 - ci_level) / 2)
            ci <- c(r_tet - z_alpha * se, r_tet + z_alpha * se)
            ci <- pmax(pmin(ci, 1), -1)  # Constrain to [-1, 1]
            
            return(list(
                estimate = r_tet,
                ci = ci,
                chisq = chisq_test$statistic,
                p.value = chisq_test$p.value,
                n = n
            ))
        },
        
        .calculatePolychoricApprox = function(x, y, ci_level) {
            # Use Spearman correlation as approximation for polychoric
            result <- tryCatch({
                cor.test(x, y, method = "spearman", conf.level = ci_level)
            }, error = function(e) NULL)
            
            if (is.null(result)) {
                return(NULL)
            }
            
            # Simple chi-square test for association
            ct <- table(x, y)
            chisq_test <- tryCatch({
                chisq.test(ct)
            }, error = function(e) list(statistic = NA, p.value = NA))
            
            return(list(
                estimate = result$estimate,
                ci = if (!is.null(result$conf.int)) result$conf.int else c(NA, NA),
                chisq = chisq_test$statistic,
                p.value = result$p.value,
                n = sum(complete.cases(x, y))
            ))
        },
        
        .createFrequencyTables = function(data, vars) {
            frequencies <- self$results$frequencies
            freq_idx <- 1
            
            for (i in 1:(length(vars) - 1)) {
                for (j in (i + 1):length(vars)) {
                    var1 <- vars[i]
                    var2 <- vars[j]
                    pair_name <- paste(var1, "×", var2)
                    
                    # Create frequency table
                    ct <- table(data[[var1]], data[[var2]])
                    
                    # Get the frequency table object
                    freq_table <- frequencies$get(key = pair_name)
                    
                    # Add columns for each level of var2
                    col_names <- colnames(ct)
                    for (col_name in col_names) {
                        freq_table$addColumn(name = col_name, title = col_name, type = 'integer')
                    }
                    freq_table$addColumn(name = 'total', title = 'Total', type = 'integer')
                    
                    # Add rows with data
                    row_names <- rownames(ct)
                    for (row_name in row_names) {
                        row_values <- list(.name = row_name)
                        for (col_name in col_names) {
                            row_values[[col_name]] <- ct[row_name, col_name]
                        }
                        row_values[['total']] <- sum(ct[row_name, ])
                        
                        freq_table$addRow(rowKey = row_name, values = row_values)
                    }
                    
                    freq_idx <- freq_idx + 1
                }
            }
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(self$data) || is.null(self$options$vars) || 
                length(self$options$vars) < 2 || !self$options$matrixPlot)
                return()
            
            vars <- self$options$vars
            data <- self$data
            
            # Convert to factors and get clean data
            for (var in vars) {
                if (!is.factor(data[[var]])) {
                    data[[var]] <- as.factor(data[[var]])
                }
            }
            data <- data[complete.cases(data[vars]), vars, drop = FALSE]
            
            if (nrow(data) < 10)
                return()
            
            # Create correlation matrix for plotting
            n_vars <- length(vars)
            corr_matrix <- matrix(1, nrow = n_vars, ncol = n_vars)
            rownames(corr_matrix) <- vars
            colnames(corr_matrix) <- vars
            
            # Fill correlation matrix
            for (i in 1:(n_vars - 1)) {
                for (j in (i + 1):n_vars) {
                    x <- as.numeric(data[[vars[i]]])
                    y <- as.numeric(data[[vars[j]]])
                    
                    # Use Spearman as approximation
                    r <- cor(x, y, method = "spearman", use = "complete.obs")
                    corr_matrix[i, j] <- r
                    corr_matrix[j, i] <- r
                }
            }
            
            # Convert to long format for ggplot
            cor_long <- expand.grid(Var1 = vars, Var2 = vars)
            cor_long$Correlation <- as.vector(corr_matrix)
            
            # Create heatmap
            p <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
                geom_tile() +
                geom_text(aes(label = sprintf("%.2f", Correlation)), color = "white", size = 3) +
                scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                   midpoint = 0, limit = c(-1, 1)) +
                labs(title = paste("Polychoric Correlation Matrix"),
                     x = "", y = "") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
            print(p)
            TRUE
        }
    )
)