# This file is a generated template, your changes will not be overwritten

#' @import jmvcore
#' @import R6
#' @importFrom vcd assocstats cramersV
#' @importFrom DescTools CramerV Phi CohenW Lambda GoodmanKruskalTau GoodmanKruskalGamma
#' @importFrom stats chisq.test fisher.test residuals xtabs
#' @importFrom utils capture.output
#' @export


categoricaladvancedClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "categoricaladvancedClass",
    inherit = categoricaladvancedBase,
    private = list(
        .init = function() {
            
            # Initialize results tables with proper structure
            private$.initContingencyTable()
            private$.initChiSquareTable()
            private$.initFisherTable()
            private$.initEffectSizesTable()
            private$.initResidualsTable()
            private$.initPostHocTable()
            private$.initAssociationTable()
            
            # Set up instructions
            private$.populateInstructionsTable()
            
        },
        
        .run = function() {
            
            # Early exit if no data
            if (is.null(self$data) || nrow(self$data) == 0)
                return()
            
            # Get variables
            row_var <- self$options$row_var
            col_var <- self$options$col_var
            strata_var <- self$options$strata_var
            
            # Validate inputs
            if (is.null(row_var) || length(row_var) == 0) {
                self$results$instructions$setContent("Please specify a row variable to begin categorical analysis.")
                return()
            }
            
            if (is.null(col_var) || length(col_var) == 0) {
                self$results$instructions$setContent("Please specify a column variable to begin categorical analysis.")
                return()
            }
            
            # Build the analysis
            private$.buildAnalysis()
            
        },
        
        .buildAnalysis = function() {
            
            # Prepare data for analysis
            analysisData <- private$.prepareData()
            
            if (is.null(analysisData)) {
                return()
            }
            
            # Create contingency table
            contingency_table <- private$.createContingencyTable(analysisData)
            
            if (is.null(contingency_table)) {
                return()
            }
            
            # Display contingency table if requested
            if (self$options$show_contingency) {
                private$.populateContingencyTable(contingency_table, analysisData)
            }
            
            # Perform chi-square test
            if (self$options$chi_square) {
                private$.performChiSquareTest(contingency_table, analysisData)
            }
            
            # Perform Fisher's exact test
            if (self$options$fisher_exact) {
                private$.performFisherTest(contingency_table, analysisData)
            }
            
            # Calculate effect sizes
            if (self$options$effect_sizes) {
                private$.calculateEffectSizes(contingency_table, analysisData)
            }
            
            # Calculate residuals
            if (self$options$residual_analysis) {
                private$.calculateResiduals(contingency_table, analysisData)
            }
            
            # Perform post hoc tests
            if (self$options$posthoc_tests) {
                private$.performPostHocTests(contingency_table, analysisData)
            }
            
            # Calculate association measures
            if (self$options$association_measures) {
                private$.calculateAssociationMeasures(contingency_table, analysisData)
            }
            
            # Generate interpretation
            private$.generateInterpretation()
            
        },
        
        .prepareData = function() {
            
            # Get variables
            row_var <- self$options$row_var
            col_var <- self$options$col_var
            strata_var <- self$options$strata_var
            
            # Get data
            data <- self$data
            
            # Create variable list
            vars_to_check <- c(row_var, col_var)
            if (!is.null(strata_var) && length(strata_var) > 0) {
                vars_to_check <- c(vars_to_check, strata_var)
            }
            
            # Create analysis dataset
            analysisData <- data[, vars_to_check, drop = FALSE]
            analysisData <- na.omit(analysisData)
            
            if (nrow(analysisData) < 5) {
                self$results$instructions$setContent("Insufficient data for categorical analysis. Need at least 5 complete observations.")
                return(NULL)
            }
            
            # Ensure variables are factors
            analysisData[[row_var]] <- as.factor(analysisData[[row_var]])
            analysisData[[col_var]] <- as.factor(analysisData[[col_var]])
            
            if (!is.null(strata_var) && length(strata_var) > 0) {
                analysisData[[strata_var]] <- as.factor(analysisData[[strata_var]])
            }
            
            return(analysisData)
        },
        
        .createContingencyTable = function(data) {
            
            row_var <- self$options$row_var
            col_var <- self$options$col_var
            strata_var <- self$options$strata_var
            
            tryCatch({
                
                if (!is.null(strata_var) && length(strata_var) > 0) {
                    # Create stratified table
                    cont_table <- xtabs(as.formula(paste("~", row_var, "+", col_var, "+", strata_var)), 
                                       data = data)
                } else {
                    # Create simple contingency table
                    cont_table <- xtabs(as.formula(paste("~", row_var, "+", col_var)), 
                                       data = data)
                }
                
                return(cont_table)
                
            }, error = function(e) {
                self$results$instructions$setContent(paste("Failed to create contingency table:", e$message))
                return(NULL)
            })
        },
        
        .populateContingencyTable = function(cont_table, data) {
            
            table <- self$results$contingency
            
            row_var <- self$options$row_var
            col_var <- self$options$col_var
            
            # Convert to 2-way table if stratified
            if (length(dim(cont_table)) > 2) {
                # Sum across strata for display
                cont_table <- apply(cont_table, c(1, 2), sum)
            }
            
            # Get row and column names
            row_names <- rownames(cont_table)
            col_names <- colnames(cont_table)
            
            # Add column headers
            for (j in 1:length(col_names)) {
                table$addColumn(
                    name = paste0("col_", j),
                    title = col_names[j],
                    type = 'integer'
                )
            }
            
            # Add total column
            table$addColumn(name = 'total', title = 'Total', type = 'integer')
            
            # Add rows
            for (i in 1:length(row_names)) {
                row_values <- list()
                row_values[['row_label']] <- row_names[i]
                
                for (j in 1:length(col_names)) {
                    row_values[[paste0("col_", j)]] <- cont_table[i, j]
                }
                
                row_values[['total']] <- sum(cont_table[i, ])
                
                table$addRow(rowKey = paste0("row_", i), values = row_values)
            }
            
            # Add total row
            total_row <- list()
            total_row[['row_label']] <- "Total"
            
            for (j in 1:length(col_names)) {
                total_row[[paste0("col_", j)]] <- sum(cont_table[, j])
            }
            
            total_row[['total']] <- sum(cont_table)
            
            table$addRow(rowKey = "total", values = total_row)
        },
        
        .performChiSquareTest = function(cont_table, data) {
            
            table <- self$results$chisquare
            
            tryCatch({
                
                # Convert to 2-way table if stratified
                if (length(dim(cont_table)) > 2) {
                    # Perform Cochran-Mantel-Haenszel test for stratified data
                    cmh_result <- mantelhaen.test(cont_table)
                    
                    row <- list(
                        test = "Cochran-Mantel-Haenszel",
                        statistic = round(cmh_result$statistic, 4),
                        df = cmh_result$parameter,
                        p_value = round(cmh_result$p.value, 4),
                        interpretation = private$.interpretPValue(cmh_result$p.value)
                    )
                    
                    table$addRow(rowKey = "cmh", values = row)
                    
                    # Also perform chi-square on collapsed table
                    cont_table <- apply(cont_table, c(1, 2), sum)
                }
                
                # Perform chi-square test
                chi_result <- chisq.test(cont_table, correct = self$options$yates_correction)
                
                # Calculate expected frequencies
                expected_min <- min(chi_result$expected)
                cells_below_5 <- sum(chi_result$expected < 5)
                total_cells <- length(chi_result$expected)
                
                row <- list(
                    test = if(self$options$yates_correction) "Chi-square with Yates" else "Pearson Chi-square",
                    statistic = round(chi_result$statistic, 4),
                    df = chi_result$parameter,
                    p_value = round(chi_result$p.value, 4),
                    interpretation = private$.interpretPValue(chi_result$p.value)
                )
                
                table$addRow(rowKey = "chi2", values = row)
                
                # Add warning if expected frequencies are low
                if (expected_min < 5) {
                    row <- list(
                        test = "Warning",
                        statistic = round(expected_min, 2),
                        df = cells_below_5,
                        p_value = NA,
                        interpretation = paste(cells_below_5, "of", total_cells, "cells have expected < 5")
                    )
                    
                    table$addRow(rowKey = "warning", values = row)
                }
                
                # Store chi-square result for later use
                private$.chi_result <- chi_result
                
            }, error = function(e) {
                self$results$instructions$setContent(paste("Chi-square test failed:", e$message))
            })
        },
        
        .performFisherTest = function(cont_table, data) {
            
            table <- self$results$fisher
            
            tryCatch({
                
                # Convert to 2-way table if stratified
                if (length(dim(cont_table)) > 2) {
                    cont_table <- apply(cont_table, c(1, 2), sum)
                }
                
                # Check table dimensions
                n_rows <- nrow(cont_table)
                n_cols <- ncol(cont_table)
                
                # Perform Fisher's exact test
                if (n_rows == 2 && n_cols == 2) {
                    # 2x2 table - can calculate odds ratio
                    fisher_result <- fisher.test(cont_table, alternative = self$options$alternative)
                    
                    row <- list(
                        test = "Fisher's Exact Test",
                        odds_ratio = round(fisher_result$estimate, 4),
                        ci_lower = round(fisher_result$conf.int[1], 4),
                        ci_upper = round(fisher_result$conf.int[2], 4),
                        p_value = round(fisher_result$p.value, 4),
                        interpretation = private$.interpretPValue(fisher_result$p.value)
                    )
                    
                    table$addRow(rowKey = "fisher", values = row)
                    
                } else {
                    # RxC table - Freeman-Halton extension
                    fisher_result <- fisher.test(cont_table, simulate.p.value = TRUE, B = 2000)
                    
                    row <- list(
                        test = "Freeman-Halton Test",
                        odds_ratio = NA,
                        ci_lower = NA,
                        ci_upper = NA,
                        p_value = round(fisher_result$p.value, 4),
                        interpretation = private$.interpretPValue(fisher_result$p.value)
                    )
                    
                    table$addRow(rowKey = "freeman", values = row)
                }
                
            }, error = function(e) {
                self$results$instructions$setContent(paste("Fisher's exact test failed:", e$message))
            })
        },
        
        .calculateEffectSizes = function(cont_table, data) {
            
            table <- self$results$effectsizes
            
            tryCatch({
                
                # Convert to 2-way table if stratified
                if (length(dim(cont_table)) > 2) {
                    cont_table <- apply(cont_table, c(1, 2), sum)
                }
                
                # Total sample size
                n <- sum(cont_table)
                
                # Chi-square statistic
                if (!is.null(private$.chi_result)) {
                    chi_sq <- private$.chi_result$statistic
                } else {
                    chi_result <- chisq.test(cont_table, correct = FALSE)
                    chi_sq <- chi_result$statistic
                }
                
                # Calculate Cramér's V
                k <- min(nrow(cont_table), ncol(cont_table))
                cramers_v <- sqrt(chi_sq / (n * (k - 1)))
                
                # Calculate Phi coefficient (for 2x2 tables)
                if (nrow(cont_table) == 2 && ncol(cont_table) == 2) {
                    phi <- sqrt(chi_sq / n)
                    
                    row <- list(
                        measure = "Phi coefficient",
                        value = round(phi, 4),
                        ci_lower = NA,
                        ci_upper = NA,
                        interpretation = private$.interpretPhi(phi)
                    )
                    
                    table$addRow(rowKey = "phi", values = row)
                }
                
                # Cramér's V
                row <- list(
                    measure = "Cramér's V",
                    value = round(cramers_v, 4),
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = private$.interpretCramersV(cramers_v)
                )
                
                table$addRow(rowKey = "cramers_v", values = row)
                
                # Cohen's w
                cohen_w <- sqrt(chi_sq / n)
                
                row <- list(
                    measure = "Cohen's w",
                    value = round(cohen_w, 4),
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = private$.interpretCohenW(cohen_w)
                )
                
                table$addRow(rowKey = "cohen_w", values = row)
                
                # Contingency coefficient
                cont_coef <- sqrt(chi_sq / (chi_sq + n))
                
                row <- list(
                    measure = "Contingency coefficient",
                    value = round(cont_coef, 4),
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = private$.interpretContingency(cont_coef)
                )
                
                table$addRow(rowKey = "cont_coef", values = row)
                
            }, error = function(e) {
                self$results$instructions$setContent(paste("Effect size calculation failed:", e$message))
            })
        },
        
        .calculateResiduals = function(cont_table, data) {
            
            table <- self$results$residuals
            
            tryCatch({
                
                # Convert to 2-way table if stratified
                if (length(dim(cont_table)) > 2) {
                    cont_table <- apply(cont_table, c(1, 2), sum)
                }
                
                # Get chi-square test results
                if (!is.null(private$.chi_result)) {
                    chi_result <- private$.chi_result
                } else {
                    chi_result <- chisq.test(cont_table, correct = FALSE)
                }
                
                # Calculate standardized residuals
                std_residuals <- chi_result$residuals
                
                # Get row and column names
                row_names <- rownames(cont_table)
                col_names <- colnames(cont_table)
                
                # Add residuals to table
                for (i in 1:length(row_names)) {
                    for (j in 1:length(col_names)) {
                        
                        observed <- cont_table[i, j]
                        expected <- chi_result$expected[i, j]
                        std_resid <- std_residuals[i, j]
                        
                        # Adjusted residual (more accurate for small samples)
                        adj_resid <- std_resid / sqrt(1 - sum(cont_table[i,])/sum(cont_table)) / 
                                    sqrt(1 - sum(cont_table[,j])/sum(cont_table))
                        
                        row <- list(
                            cell = paste(row_names[i], "-", col_names[j]),
                            observed = observed,
                            expected = round(expected, 2),
                            std_residual = round(std_resid, 3),
                            adj_residual = round(adj_resid, 3),
                            contribution = round(std_resid^2, 3),
                            interpretation = private$.interpretResidual(adj_resid)
                        )
                        
                        table$addRow(rowKey = paste0("cell_", i, "_", j), values = row)
                    }
                }
                
            }, error = function(e) {
                self$results$instructions$setContent(paste("Residual calculation failed:", e$message))
            })
        },
        
        .performPostHocTests = function(cont_table, data) {
            
            table <- self$results$posthoc
            
            row_var <- self$options$row_var
            col_var <- self$options$col_var
            
            tryCatch({
                
                # Convert to 2-way table if stratified
                if (length(dim(cont_table)) > 2) {
                    cont_table <- apply(cont_table, c(1, 2), sum)
                }
                
                # Check if post hoc is needed (more than 2x2)
                if (nrow(cont_table) <= 2 && ncol(cont_table) <= 2) {
                    self$results$posthoc$setNote("note", "Post hoc tests not needed for 2x2 tables")
                    return()
                }
                
                # Perform pairwise chi-square tests
                if (nrow(cont_table) > 2) {
                    # Pairwise comparisons for rows
                    row_levels <- rownames(cont_table)
                    
                    for (i in 1:(length(row_levels) - 1)) {
                        for (j in (i + 1):length(row_levels)) {
                            
                            # Create subset table
                            subset_table <- cont_table[c(i, j), ]
                            
                            # Perform chi-square test
                            chi_result <- chisq.test(subset_table, correct = self$options$yates_correction)
                            
                            # Apply multiple testing correction
                            n_comparisons <- choose(length(row_levels), 2)
                            
                            if (self$options$posthoc_correction == "bonferroni") {
                                adj_p <- min(chi_result$p.value * n_comparisons, 1)
                            } else if (self$options$posthoc_correction == "holm") {
                                # This is simplified - proper Holm requires sorting all p-values
                                adj_p <- chi_result$p.value
                            } else {
                                adj_p <- chi_result$p.value
                            }
                            
                            row <- list(
                                comparison = paste(row_levels[i], "vs", row_levels[j]),
                                chi_square = round(chi_result$statistic, 3),
                                df = chi_result$parameter,
                                p_value = round(chi_result$p.value, 4),
                                p_adjusted = round(adj_p, 4),
                                significant = if(adj_p < self$options$alpha_level) "Yes" else "No"
                            )
                            
                            table$addRow(rowKey = paste0("comp_", i, "_", j), values = row)
                        }
                    }
                }
                
            }, error = function(e) {
                self$results$instructions$setContent(paste("Post hoc tests failed:", e$message))
            })
        },
        
        .calculateAssociationMeasures = function(cont_table, data) {
            
            table <- self$results$association
            
            tryCatch({
                
                # Convert to 2-way table if stratified
                if (length(dim(cont_table)) > 2) {
                    cont_table <- apply(cont_table, c(1, 2), sum)
                }
                
                # Lambda (asymmetric measure of association)
                # Using DescTools if available, otherwise calculate manually
                if (requireNamespace("DescTools", quietly = TRUE)) {
                    lambda <- DescTools::Lambda(cont_table)
                    
                    row <- list(
                        measure = "Lambda (symmetric)",
                        value = round(lambda, 4),
                        interpretation = private$.interpretLambda(lambda)
                    )
                    
                    table$addRow(rowKey = "lambda", values = row)
                }
                
                # Goodman-Kruskal Tau
                if (requireNamespace("DescTools", quietly = TRUE)) {
                    tau <- DescTools::GoodmanKruskalTau(cont_table)
                    
                    row <- list(
                        measure = "Goodman-Kruskal Tau",
                        value = round(tau$estimate, 4),
                        interpretation = private$.interpretTau(tau$estimate)
                    )
                    
                    table$addRow(rowKey = "tau", values = row)
                }
                
                # Goodman-Kruskal Gamma (for ordinal data)
                if (self$options$ordinal_data) {
                    if (requireNamespace("DescTools", quietly = TRUE)) {
                        gamma <- DescTools::GoodmanKruskalGamma(cont_table)
                        
                        row <- list(
                            measure = "Goodman-Kruskal Gamma",
                            value = round(gamma$estimate, 4),
                            interpretation = private$.interpretGamma(gamma$estimate)
                        )
                        
                        table$addRow(rowKey = "gamma", values = row)
                    }
                }
                
                # Uncertainty coefficient
                # Simplified calculation
                n <- sum(cont_table)
                row_entropy <- -sum(rowSums(cont_table)/n * log(rowSums(cont_table)/n + 1e-10))
                col_entropy <- -sum(colSums(cont_table)/n * log(colSums(cont_table)/n + 1e-10))
                
                # Joint entropy
                joint_entropy <- -sum((cont_table/n) * log(cont_table/n + 1e-10))
                
                # Mutual information
                mutual_info <- row_entropy + col_entropy - joint_entropy
                
                # Uncertainty coefficient (symmetric)
                uc_symmetric <- 2 * mutual_info / (row_entropy + col_entropy)
                
                row <- list(
                    measure = "Uncertainty coefficient",
                    value = round(uc_symmetric, 4),
                    interpretation = private$.interpretUC(uc_symmetric)
                )
                
                table$addRow(rowKey = "uc", values = row)
                
            }, error = function(e) {
                self$results$instructions$setContent(paste("Association measure calculation failed:", e$message))
            })
        },
        
        .interpretPValue = function(p) {
            if (is.na(p)) return("Cannot calculate")
            if (p < 0.001) return("Highly significant")
            if (p < 0.01) return("Very significant")
            if (p < 0.05) return("Significant")
            if (p < 0.1) return("Marginally significant")
            return("Not significant")
        },
        
        .interpretPhi = function(phi) {
            if (is.na(phi)) return("Cannot calculate")
            phi <- abs(phi)
            if (phi < 0.1) return("Negligible association")
            if (phi < 0.3) return("Small association")
            if (phi < 0.5) return("Medium association")
            return("Large association")
        },
        
        .interpretCramersV = function(v) {
            if (is.na(v)) return("Cannot calculate")
            if (v < 0.1) return("Negligible association")
            if (v < 0.3) return("Weak association")
            if (v < 0.5) return("Moderate association")
            return("Strong association")
        },
        
        .interpretCohenW = function(w) {
            if (is.na(w)) return("Cannot calculate")
            if (w < 0.1) return("Small effect")
            if (w < 0.3) return("Medium effect")
            if (w < 0.5) return("Large effect")
            return("Very large effect")
        },
        
        .interpretContingency = function(c) {
            if (is.na(c)) return("Cannot calculate")
            if (c < 0.1) return("Very weak association")
            if (c < 0.3) return("Weak association")
            if (c < 0.5) return("Moderate association")
            return("Strong association")
        },
        
        .interpretResidual = function(r) {
            if (is.na(r)) return("Cannot calculate")
            r <- abs(r)
            if (r < 2) return("No significant deviation")
            if (r < 3) return("Moderate deviation")
            if (r < 4) return("Large deviation")
            return("Very large deviation")
        },
        
        .interpretLambda = function(lambda) {
            if (is.na(lambda)) return("Cannot calculate")
            if (lambda < 0.1) return("Very weak predictive power")
            if (lambda < 0.3) return("Weak predictive power")
            if (lambda < 0.5) return("Moderate predictive power")
            return("Strong predictive power")
        },
        
        .interpretTau = function(tau) {
            if (is.na(tau)) return("Cannot calculate")
            if (tau < 0.1) return("Very weak association")
            if (tau < 0.3) return("Weak association")
            if (tau < 0.5) return("Moderate association")
            return("Strong association")
        },
        
        .interpretGamma = function(gamma) {
            if (is.na(gamma)) return("Cannot calculate")
            gamma <- abs(gamma)
            if (gamma < 0.1) return("Very weak ordinal association")
            if (gamma < 0.3) return("Weak ordinal association")
            if (gamma < 0.5) return("Moderate ordinal association")
            if (gamma < 0.7) return("Strong ordinal association")
            return("Very strong ordinal association")
        },
        
        .interpretUC = function(uc) {
            if (is.na(uc)) return("Cannot calculate")
            if (uc < 0.1) return("Very low uncertainty reduction")
            if (uc < 0.3) return("Low uncertainty reduction")
            if (uc < 0.5) return("Moderate uncertainty reduction")
            return("High uncertainty reduction")
        },
        
        .generateInterpretation = function() {
            
            # Generate clinical interpretation
            row_var <- self$options$row_var
            col_var <- self$options$col_var
            
            html <- ""
            
            html <- paste0(html, "<h3>Categorical Analysis Interpretation</h3>")
            
            html <- paste0(html, "<h4>Analysis Overview</h4>")
            html <- paste0(html, "<p>This analysis examines the association between <strong>", row_var, 
                          "</strong> and <strong>", col_var, "</strong>.</p>")
            
            html <- paste0(html, "<h4>Test Selection Guide</h4>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><strong>Chi-square test:</strong> Use when all expected frequencies ≥ 5</li>")
            html <- paste0(html, "<li><strong>Fisher's exact test:</strong> Use for small samples or when expected frequencies < 5</li>")
            html <- paste0(html, "<li><strong>Yates correction:</strong> Apply for 2×2 tables with small samples</li>")
            html <- paste0(html, "</ul>")
            
            html <- paste0(html, "<h4>Effect Size Interpretation</h4>")
            html <- paste0(html, "<p><strong>Cramér's V:</strong></p>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li>< 0.1: Negligible association</li>")
            html <- paste0(html, "<li>0.1-0.3: Weak association</li>")
            html <- paste0(html, "<li>0.3-0.5: Moderate association</li>")
            html <- paste0(html, "<li>> 0.5: Strong association</li>")
            html <- paste0(html, "</ul>")
            
            html <- paste0(html, "<p><strong>Phi coefficient (2×2 tables):</strong></p>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li>< 0.1: Negligible</li>")
            html <- paste0(html, "<li>0.1-0.3: Small</li>")
            html <- paste0(html, "<li>0.3-0.5: Medium</li>")
            html <- paste0(html, "<li>> 0.5: Large</li>")
            html <- paste0(html, "</ul>")
            
            html <- paste0(html, "<h4>Residual Analysis</h4>")
            html <- paste0(html, "<p>Standardized residuals indicate which cells contribute most to the association:</p>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li>|Residual| < 2: No significant deviation</li>")
            html <- paste0(html, "<li>|Residual| 2-3: Moderate deviation</li>")
            html <- paste0(html, "<li>|Residual| > 3: Large deviation</li>")
            html <- paste0(html, "</ul>")
            
            html <- paste0(html, "<h4>Clinical Applications in Pathology</h4>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><strong>Biomarker validation:</strong> Test associations between marker positivity and outcomes</li>")
            html <- paste0(html, "<li><strong>Tumor grading:</strong> Analyze grade distributions across patient groups</li>")
            html <- paste0(html, "<li><strong>Diagnostic accuracy:</strong> Compare test results with gold standard diagnoses</li>")
            html <- paste0(html, "<li><strong>Risk stratification:</strong> Evaluate categorical risk factors</li>")
            html <- paste0(html, "</ul>")
            
            self$results$interpretation$setContent(html)
        },
        
        .initContingencyTable = function() {
            table <- self$results$contingency
            
            # Add row label column
            table$addColumn(name = 'row_label', title = '', type = 'text')
        },
        
        .initChiSquareTable = function() {
            table <- self$results$chisquare
            
            table$addColumn(name = 'test', title = 'Test', type = 'text')
            table$addColumn(name = 'statistic', title = 'Statistic', type = 'number')
            table$addColumn(name = 'df', title = 'df', type = 'integer')
            table$addColumn(name = 'p_value', title = 'p-value', type = 'number', format = 'zto,pvalue')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .initFisherTable = function() {
            table <- self$results$fisher
            
            table$addColumn(name = 'test', title = 'Test', type = 'text')
            table$addColumn(name = 'odds_ratio', title = 'Odds Ratio', type = 'number')
            table$addColumn(name = 'ci_lower', title = '95% CI Lower', type = 'number')
            table$addColumn(name = 'ci_upper', title = '95% CI Upper', type = 'number')
            table$addColumn(name = 'p_value', title = 'p-value', type = 'number', format = 'zto,pvalue')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .initEffectSizesTable = function() {
            table <- self$results$effectsizes
            
            table$addColumn(name = 'measure', title = 'Measure', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'number')
            table$addColumn(name = 'ci_lower', title = '95% CI Lower', type = 'number')
            table$addColumn(name = 'ci_upper', title = '95% CI Upper', type = 'number')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .initResidualsTable = function() {
            table <- self$results$residuals
            
            table$addColumn(name = 'cell', title = 'Cell', type = 'text')
            table$addColumn(name = 'observed', title = 'Observed', type = 'integer')
            table$addColumn(name = 'expected', title = 'Expected', type = 'number')
            table$addColumn(name = 'std_residual', title = 'Std Residual', type = 'number')
            table$addColumn(name = 'adj_residual', title = 'Adj Residual', type = 'number')
            table$addColumn(name = 'contribution', title = 'Chi² Contribution', type = 'number')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .initPostHocTable = function() {
            table <- self$results$posthoc
            
            table$addColumn(name = 'comparison', title = 'Comparison', type = 'text')
            table$addColumn(name = 'chi_square', title = 'Chi-square', type = 'number')
            table$addColumn(name = 'df', title = 'df', type = 'integer')
            table$addColumn(name = 'p_value', title = 'p-value', type = 'number', format = 'zto,pvalue')
            table$addColumn(name = 'p_adjusted', title = 'Adjusted p', type = 'number', format = 'zto,pvalue')
            table$addColumn(name = 'significant', title = 'Significant', type = 'text')
        },
        
        .initAssociationTable = function() {
            table <- self$results$association
            
            table$addColumn(name = 'measure', title = 'Measure', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'number')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .populateInstructionsTable = function() {
            html <- paste0(
                "<h2>Enhanced Chi-Square and Fisher's Tests</h2>",
                "<p>This module performs comprehensive categorical data analysis with effect sizes, residual analysis, and post hoc tests.</p>",
                "<h3>Getting Started</h3>",
                "<ol>",
                "<li><strong>Row Variable:</strong> Select the first categorical variable</li>",
                "<li><strong>Column Variable:</strong> Select the second categorical variable</li>",
                "<li><strong>Strata Variable (optional):</strong> Variable for stratified analysis</li>",
                "<li><strong>Test Options:</strong> Choose appropriate tests based on your data</li>",
                "</ol>",
                "<h3>Test Selection</h3>",
                "<ul>",
                "<li><strong>Chi-square test:</strong> Standard test for independence (requires expected frequencies ≥ 5)</li>",
                "<li><strong>Fisher's exact test:</strong> Use for small samples or when chi-square assumptions are violated</li>",
                "<li><strong>Cochran-Mantel-Haenszel:</strong> For stratified categorical data</li>",
                "</ul>",
                "<h3>Effect Sizes</h3>",
                "<ul>",
                "<li><strong>Phi coefficient:</strong> For 2×2 tables only</li>",
                "<li><strong>Cramér's V:</strong> For any size contingency table</li>",
                "<li><strong>Cohen's w:</strong> Effect size for chi-square test</li>",
                "<li><strong>Contingency coefficient:</strong> Alternative association measure</li>",
                "</ul>",
                "<h3>Advanced Features</h3>",
                "<p><strong>Residual Analysis:</strong> Identifies cells that contribute most to the association</p>",
                "<p><strong>Post Hoc Tests:</strong> Pairwise comparisons for tables larger than 2×2</p>",
                "<p><strong>Association Measures:</strong> Lambda, Tau, Gamma for predictive power and ordinal associations</p>"
            )
            
            self$results$instructions$setContent(html)
        },
        
        .chi_result = NULL
    )
)