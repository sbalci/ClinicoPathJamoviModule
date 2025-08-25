#' @title Enhanced Two-Way Frequency Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom stats chisq.test fisher.test
#' @importFrom vcd cramersV assocstats
#' @export

enhancedtwowayfrequencyClass <- R6::R6Class(
    "enhancedtwowayfrequencyClass",
    inherit = enhancedtwowayfrequencyBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$rowVar) || 
                is.null(self$options$colVar))
                return()
            
            # Set up instructions
            html <- self$results$instructions
            html$setContent(
                '<html>
                <head>
                </head>
                <body>
                <div class="instructions">
                <h3>Enhanced Two-Way Frequency Analysis with BlueSky Integration</h3>
                <p>This module provides comprehensive cross-tabulation analysis with BlueSky-inspired features:</p>
                <ul>
                <li><b>Multiple percentage types:</b> Cell, row, and column percentages (BlueSky BSkyTwoWayFrequency)</li>
                <li><b>Statistical tests:</b> Chi-square test of independence and Fisher\'s exact test</li>
                <li><b>Association measures:</b> Cramér\'s V, Phi coefficient, and contingency coefficient</li>
                <li><b>Residual analysis:</b> Standardized residuals for pattern detection</li>
                <li><b>Robust error handling:</b> BlueSky-style graceful degradation with informative messages</li>
                <li><b>Assumption validation:</b> Expected frequency checks and recommendations</li>
                <li><b>Clinical interpretation:</b> Context-aware guidance for medical research</li>
                </ul>
                <p><b>Clinical Applications:</b> Essential for exploring relationships between categorical variables in clinicopathological studies.</p>
                </div>
                </body>
                </html>'
            )
            
            # Initialize dynamic table structures
            if (!is.null(self$options$rowVar) && !is.null(self$options$colVar)) {
                private$.initializeTables()
            }
        },
        
        .run = function() {
            if (is.null(self$data) || is.null(self$options$rowVar) || 
                is.null(self$options$colVar))
                return()
            
            # Get variables
            rowVar <- self$options$rowVar
            colVar <- self$options$colVar
            
            # Get data and convert to factors
            data <- self$data
            data[[rowVar]] <- as.factor(data[[rowVar]])
            data[[colVar]] <- as.factor(data[[colVar]])
            
            # Remove missing values
            complete_data <- data[!is.na(data[[rowVar]]) & !is.na(data[[colVar]]), ]
            
            if (nrow(complete_data) == 0) {
                if (self$options$robustErrorHandling) {
                    self$results$instructions$setContent(
                        "<div style='color: orange;'><strong>Warning:</strong> No complete cases found for analysis.<br>Please check for missing values in selected variables.</div>"
                    )
                } else {
                    self$results$instructions$setContent("No complete cases available for analysis.")
                }
                return()
            }
            
            # BlueSky-style error handling
            private$.error_messages <- character(0)
            private$.warning_messages <- character(0)
            
            # Create cross-tabulation table (BlueSky BSkyTwoWayFrequency approach)
            mytable <- table(complete_data[[rowVar]], complete_data[[colVar]])
            
            # Calculate various percentage types
            if (self$options$cellPercent || self$options$showCounts) {
                private$.calculateFrequencyTables(mytable, complete_data, rowVar, colVar)
            }
            
            # Statistical tests
            if (self$options$chiSquareTest || self$options$fisherTest) {
                private$.performStatisticalTests(mytable)
            }
            
            # Association measures
            if (self$options$associationMeasures) {
                private$.calculateAssociationMeasures(mytable)
            }
            
            # Assumption checks
            if (self$options$showDiagnostics || self$options$expectedFrequencies) {
                private$.performAssumptionChecks(mytable)
            }
            
            # Generate diagnostic information
            if (self$options$showDiagnostics) {
                private$.generateDiagnostics(complete_data, mytable, rowVar, colVar)
            }
            
            # Generate comprehensive summary
            if (self$options$bluesky_integration && self$options$comprehensive_output) {
                private$.generateComprehensiveSummary(mytable, complete_data)
            }
            
            # Generate recommendations
            if (self$options$showRecommendations) {
                private$.generateRecommendations(mytable)
            }
            
            # Generate clinical interpretation
            if (self$options$clinicalInterpretation) {
                private$.generateClinicalInterpretation(mytable)
            }
            
            # Generate methods explanation
            if (self$options$detailedOutput) {
                private$.generateMethodsExplanation()
            }
        },
        
        .initializeTables = function() {
            # Initialize observed frequencies table
            if (self$options$showCounts) {
                # Will be populated during analysis
            }
        },
        
        .calculateFrequencyTables = function(mytable, complete_data, rowVar, colVar) {
            # BlueSky BSkyTwoWayFrequency implementation
            
            # Cell percentages (prop.table(mytable))
            if (self$options$cellPercent) {
                cell_percent_table <- prop.table(mytable)
                private$.populateMatrixTable(self$results$cellPercentMatrix, mytable, cell_percent_table, "percentage")
            }
            
            # Row percentages (prop.table(mytable, 1))  
            if (self$options$rowPercent) {
                row_percent_table <- prop.table(mytable, 1)
                private$.populateMatrixTable(self$results$rowPercentMatrix, mytable, row_percent_table, "percentage")
            }
            
            # Column percentages (prop.table(mytable, 2))
            if (self$options$colPercent) {
                col_percent_table <- prop.table(mytable, 2)
                private$.populateMatrixTable(self$results$colPercentMatrix, mytable, col_percent_table, "percentage")
            }
            
            # Main cross-tabulation matrix
            private$.populateMatrixTable(self$results$crossTabMatrix, mytable, mytable, "count")
            
            # Detailed observed frequencies table
            if (self$options$showCounts) {
                private$.populateDetailedFrequencyTable(mytable, complete_data, rowVar, colVar)
            }
        },
        
        .populateMatrixTable = function(table, count_table, value_table, format_type) {
            row_names <- rownames(count_table)
            col_names <- colnames(count_table)
            
            # Clear existing columns except the first one
            for (i in seq_along(col_names)) {
                col_name <- paste0("col_", i)
                table$addColumn(name = col_name, title = col_names[i], 
                               type = if (format_type == "count") "integer" else "number")
            }
            
            # Add total column if requested
            if (self$options$showTotals) {
                table$addColumn(name = "total", title = "Total", 
                               type = if (format_type == "count") "integer" else "number")
            }
            
            # Populate rows
            for (i in seq_along(row_names)) {
                row_data <- list(row_category = row_names[i])
                
                # Add data for each column
                for (j in seq_along(col_names)) {
                    col_name <- paste0("col_", j)
                    value <- value_table[i, j]
                    if (format_type == "percentage" && self$options$percentageDisplay == "percentage") {
                        value <- value * 100
                    }
                    row_data[[col_name]] <- value
                }
                
                # Add total if requested
                if (self$options$showTotals) {
                    if (format_type == "count") {
                        row_data$total <- sum(count_table[i, ])
                    } else {
                        row_data$total <- if (self$options$percentageDisplay == "percentage") 100 else 1.0
                    }
                }
                
                table$addRow(rowKey = i, values = row_data)
            }
            
            # Add totals row if requested
            if (self$options$showTotals) {
                total_row_data <- list(row_category = "Total")
                
                for (j in seq_along(col_names)) {
                    col_name <- paste0("col_", j)
                    if (format_type == "count") {
                        total_row_data[[col_name]] <- sum(count_table[, j])
                    } else {
                        total_row_data[[col_name]] <- if (self$options$percentageDisplay == "percentage") 100 else 1.0
                    }
                }
                
                if (self$options$showTotals) {
                    total_row_data$total <- sum(count_table)
                }
                
                table$addRow(rowKey = "total", values = total_row_data)
            }
        },
        
        .populateDetailedFrequencyTable = function(mytable, complete_data, rowVar, colVar) {
            observed_table <- self$results$observedFrequencies
            
            # Calculate expected frequencies if needed
            expected_freq <- NULL
            if (self$options$expectedFrequencies) {
                chi_result <- tryCatch({
                    chisq.test(mytable)
                }, error = function(e) NULL)
                
                if (!is.null(chi_result)) {
                    expected_freq <- chi_result$expected
                }
            }
            
            # Calculate standardized residuals if needed
            std_residuals <- NULL
            if (self$options$residualAnalysis) {
                chi_result <- tryCatch({
                    chisq.test(mytable)
                }, error = function(e) NULL)
                
                if (!is.null(chi_result)) {
                    std_residuals <- chi_result$stdres
                }
            }
            
            # Populate detailed table
            row_idx <- 1
            for (i in 1:nrow(mytable)) {
                for (j in 1:ncol(mytable)) {
                    row_values <- list(
                        rowVar_level = rownames(mytable)[i],
                        colVar_level = colnames(mytable)[j],
                        count = mytable[i, j]
                    )
                    
                    # Add percentages if requested
                    if (self$options$cellPercent) {
                        cell_pct <- prop.table(mytable)[i, j]
                        row_values$cell_percent <- if (self$options$percentageDisplay == "percentage") cell_pct * 100 else cell_pct
                    }
                    
                    if (self$options$rowPercent) {
                        row_pct <- prop.table(mytable, 1)[i, j]
                        row_values$row_percent <- if (self$options$percentageDisplay == "percentage") row_pct * 100 else row_pct
                    }
                    
                    if (self$options$colPercent) {
                        col_pct <- prop.table(mytable, 2)[i, j]
                        row_values$col_percent <- if (self$options$percentageDisplay == "percentage") col_pct * 100 else col_pct
                    }
                    
                    # Add expected frequency if available
                    if (!is.null(expected_freq)) {
                        row_values$expected <- expected_freq[i, j]
                    }
                    
                    # Add standardized residual if available
                    if (!is.null(std_residuals)) {
                        row_values$std_residual <- std_residuals[i, j]
                    }
                    
                    observed_table$setRow(rowNo = row_idx, values = row_values)
                    row_idx <- row_idx + 1
                }
            }
        },
        
        .performStatisticalTests = function(mytable) {
            test_table <- self$results$testResults
            row_idx <- 1
            
            # Chi-square test
            if (self$options$chiSquareTest) {
                chi_result <- tryCatch({
                    chisq.test(mytable, correct = self$options$continuityCorrection)
                }, error = function(e) {
                    if (self$options$robustErrorHandling) {
                        private$.warning_messages <- c(private$.warning_messages, 
                                                     paste("Chi-square test error:", e$message))
                        return(NULL)
                    } else {
                        stop(e)
                    }
                })
                
                if (!is.null(chi_result)) {
                    interpretation <- if (chi_result$p.value < 0.05) {
                        "Significant association detected"
                    } else {
                        "No significant association"
                    }
                    
                    effect_size <- ""
                    if (self$options$associationMeasures) {
                        cramers_v <- private$.calculateCramersV(mytable)
                        effect_size <- sprintf("Cramér's V = %.3f", cramers_v)
                    }
                    
                    test_table$setRow(rowNo = row_idx, values = list(
                        test_name = "Pearson Chi-Square",
                        statistic = chi_result$statistic,
                        df = chi_result$parameter,
                        p_value = chi_result$p.value,
                        interpretation = interpretation,
                        effect_size = effect_size
                    ))
                    row_idx <- row_idx + 1
                }
            }
            
            # Fisher's exact test
            if (self$options$fisherTest) {
                fisher_result <- tryCatch({
                    fisher.test(mytable)
                }, error = function(e) {
                    if (self$options$robustErrorHandling) {
                        private$.warning_messages <- c(private$.warning_messages, 
                                                     paste("Fisher's exact test error:", e$message))
                        return(NULL)
                    } else {
                        stop(e)
                    }
                })
                
                if (!is.null(fisher_result)) {
                    interpretation <- if (fisher_result$p.value < 0.05) {
                        "Significant association detected"
                    } else {
                        "No significant association"
                    }
                    
                    test_table$setRow(rowNo = row_idx, values = list(
                        test_name = "Fisher's Exact Test",
                        statistic = NA,  # Fisher's test doesn't have a test statistic
                        df = NA,
                        p_value = fisher_result$p.value,
                        interpretation = interpretation,
                        effect_size = ""
                    ))
                    row_idx <- row_idx + 1
                }
            }
        },
        
        .calculateAssociationMeasures = function(mytable) {
            assoc_table <- self$results$associationMeasuresTable
            row_idx <- 1
            
            # Cramér's V
            cramers_v <- private$.calculateCramersV(mytable)
            cramers_interp <- private$.interpretCramersV(cramers_v)
            
            assoc_table$setRow(rowNo = row_idx, values = list(
                measure = "Cramér's V",
                value = cramers_v,
                confidence_interval = "",  # Would need bootstrap for CI
                interpretation = cramers_interp$interpretation,
                clinical_significance = cramers_interp$clinical
            ))
            row_idx <- row_idx + 1
            
            # Phi coefficient (for 2x2 tables)
            if (nrow(mytable) == 2 && ncol(mytable) == 2) {
                phi <- private$.calculatePhi(mytable)
                phi_interp <- private$.interpretPhi(phi)
                
                assoc_table$setRow(rowNo = row_idx, values = list(
                    measure = "Phi Coefficient",
                    value = phi,
                    confidence_interval = "",
                    interpretation = phi_interp$interpretation,
                    clinical_significance = phi_interp$clinical
                ))
                row_idx <- row_idx + 1
            }
            
            # Contingency coefficient
            contingency_c <- private$.calculateContingencyCoefficient(mytable)
            
            assoc_table$setRow(rowNo = row_idx, values = list(
                measure = "Contingency Coefficient",
                value = contingency_c,
                confidence_interval = "",
                interpretation = private$.interpretContingencyC(contingency_c),
                clinical_significance = "General measure of association strength"
            ))
        },
        
        .calculateCramersV = function(mytable) {
            # Calculate Cramér's V
            chi_sq <- chisq.test(mytable)$statistic
            n <- sum(mytable)
            min_dim <- min(nrow(mytable), ncol(mytable))
            sqrt(chi_sq / (n * (min_dim - 1)))
        },
        
        .calculatePhi = function(mytable) {
            # Phi coefficient for 2x2 tables
            chi_sq <- chisq.test(mytable)$statistic
            n <- sum(mytable)
            sqrt(chi_sq / n)
        },
        
        .calculateContingencyCoefficient = function(mytable) {
            # Contingency coefficient
            chi_sq <- chisq.test(mytable)$statistic
            n <- sum(mytable)
            sqrt(chi_sq / (chi_sq + n))
        },
        
        .interpretCramersV = function(v) {
            if (v < 0.1) {
                list(interpretation = "Negligible association", clinical = "Clinically insignificant relationship")
            } else if (v < 0.3) {
                list(interpretation = "Small association", clinical = "Weak clinical relationship")
            } else if (v < 0.5) {
                list(interpretation = "Moderate association", clinical = "Moderate clinical significance")
            } else {
                list(interpretation = "Strong association", clinical = "Strong clinical relationship")
            }
        },
        
        .interpretPhi = function(phi) {
            abs_phi <- abs(phi)
            if (abs_phi < 0.1) {
                list(interpretation = "Negligible association", clinical = "Clinically insignificant relationship")
            } else if (abs_phi < 0.3) {
                list(interpretation = "Small association", clinical = "Weak clinical relationship")
            } else if (abs_phi < 0.5) {
                list(interpretation = "Moderate association", clinical = "Moderate clinical significance")
            } else {
                list(interpretation = "Strong association", clinical = "Strong clinical relationship")
            }
        },
        
        .interpretContingencyC = function(c) {
            if (c < 0.1) {
                "Negligible association"
            } else if (c < 0.3) {
                "Small association"
            } else if (c < 0.5) {
                "Moderate association"
            } else {
                "Strong association"
            }
        }
    )
)