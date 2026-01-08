#' @title Chi-Square Post-Hoc Tests - Enhanced Implementation
#' @description
#' Comprehensive post-hoc analysis for chi-square tests including residuals analysis,
#' pairwise comparisons, and multiple testing corrections. Integrates methods from
#' jamovi forum recommendations and best practices from statistical literature.
#' 
#' @details
#' This enhanced implementation provides three complementary approaches:
#' 1. Residuals Analysis - Identifies which cells contribute to significance (recommended first step)
#' 2. Pairwise Comparisons - Formal hypothesis testing between group pairs
#' 3. Comprehensive Analysis - Combines both approaches with educational guidance
#' 
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats chisq.test qchisq p.adjust fisher.test pairwise.table qnorm pnorm
#' @importFrom vcd mosaic
#' @importFrom grid gpar
#' @importFrom grDevices hcl.colors
#' @importFrom htmltools HTML div h3 h4 h5 p strong em br
#'

chisqposttestClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "chisqposttestClass",
    inherit = chisqposttestBase,
    private = list(

        .init = function() {
            # Prevent analysis from running without variables selected
            if (is.null(self$options$rows) || is.null(self$options$cols)) {
                return()
            }
        },
        
        # Enhanced helper functions for comprehensive analysis ----
        
        # Analyze residuals with proper statistical corrections and explicit methodology
        .analyzeResiduals = function(chi_result, contingency_table, alpha = 0.05, cutoff = NULL) {
            # Calculate basic components
            observed <- contingency_table
            expected <- chi_result$expected
            total_sample <- sum(observed)
            
            # Calculate multiple types of residuals for comprehensive analysis
            # 1. Raw residuals (simple difference)
            raw_residuals <- observed - expected
            
            # 2. Standardized residuals (Pearson residuals)
            std_residuals <- raw_residuals / sqrt(expected)
            
            # 3. Adjusted standardized residuals (accounting for variance structure)
            # This is the most appropriate for post-hoc analysis
            adjusted_residuals <- chi_result$stdres
            
            # 4. Calculate critical value using configurable approach
            critical_z <- private$.calculateResidualsCriticalValue(contingency_table, alpha, cutoff)
            
            # 5. Determine significance and effect size
            significant_cells <- abs(adjusted_residuals) > critical_z
            effect_magnitude <- private$.classifyResidualMagnitude(adjusted_residuals)
            
            # 6. Generate interpretations
            cell_interpretations <- private$.generateResidualInterpretations(
                adjusted_residuals, significant_cells, effect_magnitude
            )
            
            # Compile comprehensive results
            results <- list(
                # Data components
                observed = observed,
                expected = expected,
                total_sample = total_sample,
                
                # Residual types
                raw_residuals = raw_residuals,
                std_residuals = std_residuals,
                adjusted_residuals = adjusted_residuals,
                
                # Statistical decision components
                critical_value = critical_z,
                alpha_level = alpha,
                correction_method = if (is.null(cutoff)) "Bonferroni" else "Custom",
                significant_cells = significant_cells,
                
                # Interpretation components
                effect_magnitude = effect_magnitude,
                interpretation = cell_interpretations,
                
                # Summary statistics
                num_significant_cells = sum(significant_cells),
                proportion_significant = sum(significant_cells) / length(significant_cells),
                max_absolute_residual = max(abs(adjusted_residuals), na.rm = TRUE)
            )
            
            return(results)
        },
        
        # Calculate critical value for residuals significance testing
        .calculateResidualsCriticalValue = function(contingency_table, alpha, custom_cutoff) {
            if (!is.null(custom_cutoff)) {
                # Use user-specified cutoff
                return(custom_cutoff)
            }
            
            # Calculate Bonferroni-corrected critical value
            num_cells <- length(contingency_table)
            corrected_alpha <- alpha / num_cells
            critical_z <- qnorm(1 - (corrected_alpha / 2))
            
            return(critical_z)
        },
        
        # Classify residual magnitude for effect size interpretation
        .classifyResidualMagnitude = function(residuals) {
            abs_residuals <- abs(residuals)
            magnitude_matrix <- matrix(
                ifelse(abs_residuals < 1.0, "Negligible",
                       ifelse(abs_residuals < 2.0, "Small",
                              ifelse(abs_residuals < 3.0, "Medium",
                                     ifelse(abs_residuals < 4.0, "Large", "Very Large")))),
                nrow = nrow(residuals), 
                ncol = ncol(residuals),
                dimnames = dimnames(residuals)
            )
            return(magnitude_matrix)
        },
        
        # Generate comprehensive interpretations for residual analysis
        .generateResidualInterpretations = function(residuals, significant_cells, magnitude) {
            interpretation_matrix <- matrix(
                NA, 
                nrow = nrow(residuals), 
                ncol = ncol(residuals),
                dimnames = dimnames(residuals)
            )
            
            for (i in 1:nrow(residuals)) {
                for (j in 1:ncol(residuals)) {
                    residual_val <- residuals[i, j]
                    is_sig <- significant_cells[i, j]
                    mag <- magnitude[i, j]
                    
                    if (!is_sig) {
                        interpretation_matrix[i, j] <- "As expected"
                    } else {
                        direction <- if (residual_val > 0) "Over-represented" else "Under-represented"
                        interpretation_matrix[i, j] <- paste(direction, paste0("(", mag, ")"))
                    }
                }
            }
            
            return(interpretation_matrix)
        },
        
        # Robust pairwise chi-square testing with optimized resource management
        .robustPairwiseTests = function(contingency_table, method = "bonferroni", test_selection = "auto") {
            # Check if we need memory optimization for large tables
            row_names <- rownames(contingency_table)
            total_comparisons <- choose(length(row_names), 2)
            
            # Use chunked processing for moderate-sized tables to prevent resource limits
            if (total_comparisons > 25) {
                return(private$.robustPairwiseTestsChunked(contingency_table, method, test_selection))
            }
            
            # Standard processing for smaller datasets
            row_names <- rownames(contingency_table)
            col_names <- colnames(contingency_table)
            
            # Create all pairwise combinations for rows
            pairwise_results <- list()
            comparison_index <- 1
            
            # Row-wise pairwise comparisons
            if (length(row_names) >= 2) {
                for (i in 1:(length(row_names) - 1)) {
                    for (j in (i + 1):length(row_names)) {
                        # Extract 2x2 subtable
                        subtable <- contingency_table[c(i, j), , drop = FALSE]
                        
                        # Skip if insufficient data
                        if (any(dim(subtable) < 2) || sum(subtable) == 0) next
                        
                        # Perform statistical tests based on user selection
                        test_result <- try({
                            # Always compute chi-square for effect size
                            chi_test <- stats::chisq.test(subtable, correct = FALSE)
                            expected_counts <- chi_test$expected
                            
                            # Determine which tests to run based on test_selection
                            fisher_test <- NULL
                            use_fisher <- test_selection == "fisher" || 
                                          (test_selection == "auto" && any(expected_counts < 5))
                            if (use_fisher) {
                                fisher_test <- try(stats::fisher.test(subtable), silent = TRUE)
                                if (inherits(fisher_test, "try-error")) fisher_test <- NULL
                            }
                            
                            # Store both test results if available
                            result <- list(
                                comparison = paste(row_names[i], "vs", row_names[j]),
                                type = "row_comparison",
                                subtable = subtable,
                                expected = expected_counts,
                                chi_statistic = chi_test$statistic,
                                chi_df = chi_test$parameter,
                                chi_pvalue = chi_test$p.value,
                                sample_size = sum(subtable),
                                effect_size = sqrt(chi_test$statistic / sum(subtable)),
                                phi_ci = if (self$options$phiCI) private$.calculatePhiCI(subtable) else "",
                                test_selection = test_selection
                            )
                            
                            # Add Fisher's test results if computed
                            if (!is.null(fisher_test)) {
                                result$fisher_pvalue <- fisher_test$p.value
                                result$test_used <- "Fisher's exact"
                                result$actual_pvalue <- fisher_test$p.value
                            } else {
                                result$fisher_pvalue <- NA
                                result$test_used <- "Chi-square"
                                result$actual_pvalue <- chi_test$p.value
                            }
                            
                            result
                        }, silent = TRUE)
                        
                        if (!inherits(test_result, "try-error")) {
                            pairwise_results[[comparison_index]] <- test_result
                            comparison_index <- comparison_index + 1
                        }
                    }
                }
            }
            
            # Column-wise pairwise comparisons
            if (length(col_names) >= 2) {
                for (i in 1:(length(col_names) - 1)) {
                    for (j in (i + 1):length(col_names)) {
                        # Extract 2x2 subtable (transpose for column comparisons)
                        subtable <- contingency_table[, c(i, j), drop = FALSE]
                        
                        # Skip if insufficient data
                        if (any(dim(subtable) < 2) || sum(subtable) == 0) next
                        
                        # Perform statistical tests based on user selection
                        test_result <- try({
                            # Always compute chi-square for effect size
                            chi_test <- stats::chisq.test(subtable, correct = FALSE)
                            expected_counts <- chi_test$expected
                            
                            # Determine which tests to run based on test_selection
                            fisher_test <- NULL
                            use_fisher <- test_selection == "fisher" || 
                                          (test_selection == "auto" && any(expected_counts < 5))
                            if (use_fisher) {
                                fisher_test <- try(stats::fisher.test(subtable), silent = TRUE)
                                if (inherits(fisher_test, "try-error")) fisher_test <- NULL
                            }
                            
                            # Store both test results if available
                            result <- list(
                                comparison = paste(col_names[i], "vs", col_names[j]),
                                type = "column_comparison",
                                subtable = subtable,
                                expected = expected_counts,
                                chi_statistic = chi_test$statistic,
                                chi_df = chi_test$parameter,
                                chi_pvalue = chi_test$p.value,
                                sample_size = sum(subtable),
                                effect_size = sqrt(chi_test$statistic / sum(subtable)),
                                phi_ci = if (self$options$phiCI) private$.calculatePhiCI(subtable) else "",
                                test_selection = test_selection
                            )
                            
                            # Add Fisher's test results if computed
                            if (!is.null(fisher_test)) {
                                result$fisher_pvalue <- fisher_test$p.value
                                result$test_used <- "Fisher's exact"
                                result$actual_pvalue <- fisher_test$p.value
                            } else {
                                result$fisher_pvalue <- NA
                                result$test_used <- "Chi-square"
                                result$actual_pvalue <- chi_test$p.value
                            }
                            
                            result
                        }, silent = TRUE)
                        
                        if (!inherits(test_result, "try-error")) {
                            pairwise_results[[comparison_index]] <- test_result
                            comparison_index <- comparison_index + 1
                        }
                    }
                }
            }
            
            # Apply p-value adjustments
            if (length(pairwise_results) > 0) {
                chi_pvalues <- sapply(pairwise_results, function(x) x$chi_pvalue)
                fisher_pvalues <- sapply(pairwise_results, function(x) x$fisher_pvalue)
                actual_pvalues <- sapply(pairwise_results, function(x) x$actual_pvalue)
                
                # Apply correction to the actually used p-values
                actual_adjusted <- stats::p.adjust(actual_pvalues, method = method)
                
                # Apply correction to chi-square p-values (always available)
                chi_adjusted <- stats::p.adjust(chi_pvalues, method = method)
                
                # Apply correction to Fisher's p-values only if they exist
                fisher_adjusted <- rep(NA, length(fisher_pvalues))
                valid_fisher <- !is.na(fisher_pvalues)
                if (any(valid_fisher)) {
                    fisher_adjusted[valid_fisher] <- stats::p.adjust(fisher_pvalues[valid_fisher], method = method)
                }
                
                # Add adjusted p-values back to results
                for (i in seq_along(pairwise_results)) {
                    pairwise_results[[i]]$chi_pvalue_adjusted <- chi_adjusted[i]
                    pairwise_results[[i]]$fisher_pvalue_adjusted <- fisher_adjusted[i]
                    pairwise_results[[i]]$actual_pvalue_adjusted <- actual_adjusted[i]
                    pairwise_results[[i]]$adjustment_method <- method
                }
            }
            
            return(pairwise_results)
        },
        
        # Memory-optimized chunked processing for large contingency tables
        .robustPairwiseTestsChunked = function(contingency_table, method = "bonferroni", test_selection = "auto", chunk_size = 50) {
            row_names <- rownames(contingency_table)
            col_names <- colnames(contingency_table)
            total_comparisons <- choose(length(row_names), 2) + choose(length(col_names), 2)
            
            # Initialize variables
            all_pairwise_results <- list()
            completed_comparisons <- 0
            overall_index <- 1
            
            # Processing comparisons in chunks for memory efficiency
            
            # Create comparison pairs (both row and column comparisons)
            comparison_pairs <- list()
            pair_index <- 1
            
            # Row-wise comparison pairs
            for (i in 1:(length(row_names) - 1)) {
                for (j in (i + 1):length(row_names)) {
                    comparison_pairs[[pair_index]] <- list(indices = c(i, j), type = "row")
                    pair_index <- pair_index + 1
                }
            }
            
            # Column-wise comparison pairs
            for (i in 1:(length(col_names) - 1)) {
                for (j in (i + 1):length(col_names)) {
                    comparison_pairs[[pair_index]] <- list(indices = c(i, j), type = "column")
                    pair_index <- pair_index + 1
                }
            }
            
            # Process in chunks
            num_chunks <- ceiling(length(comparison_pairs) / chunk_size)
            
            for (chunk_idx in 1:num_chunks) {
                start_idx <- (chunk_idx - 1) * chunk_size + 1
                end_idx <- min(chunk_idx * chunk_size, length(comparison_pairs))
                chunk_pairs <- comparison_pairs[start_idx:end_idx]
                
                # Process current chunk
                chunk_results <- list()
                chunk_result_idx <- 1
                
                for (pair_idx in seq_along(chunk_pairs)) {
                    pair_info <- chunk_pairs[[pair_idx]]
                    i <- pair_info$indices[1]
                    j <- pair_info$indices[2]
                    comp_type <- pair_info$type
                    completed_comparisons <- completed_comparisons + 1
                    
                    # Extract 2x2 subtable based on comparison type
                    if (comp_type == "row") {
                        subtable <- contingency_table[c(i, j), , drop = FALSE]
                        comp_names <- row_names
                    } else { # column comparison
                        subtable <- contingency_table[, c(i, j), drop = FALSE]
                        comp_names <- col_names
                    }
                    
                    # Skip if insufficient data
                    if (any(dim(subtable) < 2) || sum(subtable) == 0) next
                    
                    # Perform statistical tests
                    test_result <- try({
                        # Always compute chi-square for effect size
                        chi_test <- stats::chisq.test(subtable, correct = FALSE)
                        expected_counts <- chi_test$expected
                        
                        # Determine which tests to run based on test_selection
                        fisher_test <- NULL
                        use_fisher <- test_selection == "fisher" || 
                                      (test_selection == "auto" && any(expected_counts < 5))
                        if (use_fisher) {
                            fisher_test <- try(stats::fisher.test(subtable), silent = TRUE)
                            if (inherits(fisher_test, "try-error")) fisher_test <- NULL
                        }
                        
                        # Store both test results if available
                        result <- list(
                            comparison = paste(comp_names[i], "vs", comp_names[j]),
                            type = paste0(comp_type, "_comparison"),
                            subtable = subtable,
                            expected = expected_counts,
                            chi_statistic = chi_test$statistic,
                            chi_df = chi_test$parameter,
                            chi_pvalue = chi_test$p.value,
                            sample_size = sum(subtable),
                            effect_size = sqrt(chi_test$statistic / sum(subtable)),
                            phi_ci = if (self$options$phiCI) private$.calculatePhiCI(subtable) else "",
                            test_selection = test_selection
                        )
                        
                        # Add Fisher's test results if computed
                        if (!is.null(fisher_test)) {
                            result$fisher_pvalue <- fisher_test$p.value
                            result$test_used <- "Fisher's exact"
                            result$actual_pvalue <- fisher_test$p.value
                        } else {
                            result$fisher_pvalue <- NA
                            result$test_used <- "Chi-square"
                            result$actual_pvalue <- chi_test$p.value
                        }
                        
                        result
                    }, silent = TRUE)
                    
                    if (!inherits(test_result, "try-error")) {
                        chunk_results[[chunk_result_idx]] <- test_result
                        chunk_result_idx <- chunk_result_idx + 1
                    }
                    
                    # Minimal garbage collection - only for very large chunks
                    if (completed_comparisons %% 100 == 0) {
                        gc(verbose = FALSE)
                    }
                }
                
                # Add chunk results to overall results
                if (length(chunk_results) > 0) {
                    for (result in chunk_results) {
                        all_pairwise_results[[overall_index]] <- result
                        overall_index <- overall_index + 1
                    }
                }
                
                # Clear chunk results to free memory
                chunk_results <- NULL
                chunk_pairs <- NULL
                gc(verbose = FALSE)
            }
            
            # Apply p-value adjustments to all results
            if (length(all_pairwise_results) > 0) {
                chi_pvalues <- sapply(all_pairwise_results, function(x) x$chi_pvalue)
                fisher_pvalues <- sapply(all_pairwise_results, function(x) x$fisher_pvalue)
                actual_pvalues <- sapply(all_pairwise_results, function(x) x$actual_pvalue)
                
                # Apply correction to the actually used p-values
                actual_adjusted <- stats::p.adjust(actual_pvalues, method = method)
                
                # Apply correction to chi-square p-values (always available)
                chi_adjusted <- stats::p.adjust(chi_pvalues, method = method)
                
                # Apply correction to Fisher's p-values only if they exist
                fisher_adjusted <- rep(NA, length(fisher_pvalues))
                valid_fisher <- !is.na(fisher_pvalues)
                if (any(valid_fisher)) {
                    fisher_adjusted[valid_fisher] <- stats::p.adjust(fisher_pvalues[valid_fisher], method = method)
                }
                
                # Add adjusted p-values back to results
                for (i in seq_along(all_pairwise_results)) {
                    all_pairwise_results[[i]]$chi_pvalue_adjusted <- chi_adjusted[i]
                    all_pairwise_results[[i]]$fisher_pvalue_adjusted <- fisher_adjusted[i]
                    all_pairwise_results[[i]]$actual_pvalue_adjusted <- actual_adjusted[i]
                    all_pairwise_results[[i]]$adjustment_method <- method
                }
            }
            
            return(all_pairwise_results)
        },
        
        # Create detailed HTML table for individual pairwise comparisons
        .createDetailedComparisonHTML = function(pairwise_results, row_var_name = NULL, col_var_name = NULL) {
            if (length(pairwise_results) == 0) return("")
            
            # Separate row and column comparisons
            row_comparisons <- pairwise_results[sapply(pairwise_results, function(x) x$type == "row_comparison")]
            col_comparisons <- pairwise_results[sapply(pairwise_results, function(x) x$type == "column_comparison")]
            
            html_parts <- list()
            
            # Row comparisons section
            if (length(row_comparisons) > 0) {
                row_title <- htmltools::h3(
                    style = "color: #1976d2; margin-top: 20px;", 
                    paste(.("Row-wise Pairwise Comparisons"), if (!is.null(row_var_name)) paste0(" (", row_var_name, ")") else "")
                )
                row_comparisons_html <- private$.createComparisonSection(row_comparisons, row_var_name, col_var_name)
                html_parts <- c(html_parts, list(row_title, row_comparisons_html))
            }
            
            # Column comparisons section
            if (length(col_comparisons) > 0) {
                col_title <- htmltools::h3(
                    style = "color: #1976d2; margin-top: 20px;", 
                    paste(.("Column-wise Pairwise Comparisons"), if (!is.null(col_var_name)) paste0(" (", col_var_name, ")") else "")
                )
                col_comparisons_html <- private$.createComparisonSection(col_comparisons, row_var_name, col_var_name)
                html_parts <- c(html_parts, list(col_title, col_comparisons_html))
            }
            
            main_content <- htmltools::div(
                style = "margin: 20px 0;",
                htmltools::h3(style = "color: #1976d2;", .("Detailed Pairwise Comparison Tables")),
                html_parts
            )
            
            return(as.character(main_content))
        },
        
        # Helper function to create a section of comparisons
        .createComparisonSection = function(comparisons, row_var_name = NULL, col_var_name = NULL) {
            comparison_divs <- lapply(seq_along(comparisons), function(i) {
                result <- comparisons[[i]]
                subtable <- result$subtable
                
                # Determine test method and significance
                use_fisher <- identical(result$test_used, "Fisher's exact")
                p_adj <- result$actual_pvalue_adjusted
                is_significant <- p_adj < self$options$sig
                
                # Create significance indicator
                sig_indicator <- if (is_significant) {
                    htmltools::span(style = "color: #d32f2f; font-weight: bold;", "✓ Significant")
                } else {
                    htmltools::span(style = "color: #666;", "Not significant")
                }
                
                # Test results summary
                test_method <- if (use_fisher) "Fisher's exact test" else "Chi-square test"
                p_value <- result$actual_pvalue
                
                test_results <- htmltools::div(
                    style = "margin-bottom: 15px; padding: 10px; background-color: #f5f5f5; border-radius: 3px;",
                    htmltools::strong("Test Results:"), htmltools::br(),
                    "Method: ", test_method, " (", 
                    if (use_fisher) "used due to low expected frequencies" else "standard approach", 
                    ")", htmltools::br(),
                    "Chi-square = ", round(result$chi_statistic, 3), ", ",
                    "p = ", format.pval(p_value, digits = 3), ", ",
                    "Adjusted p = ", format.pval(p_adj, digits = 3), htmltools::br(),
                    "Effect size (Phi) = ", round(result$effect_size, 3), htmltools::br(),
                    sig_indicator
                )
                
                # Create contingency table using htmltools
                contingency_table <- private$.createContingencyTableHTML(subtable, row_var_name, col_var_name)
                
                # Wrap in comparison div
                htmltools::div(
                    style = "margin: 20px 0; padding: 15px; border: 1px solid #ddd; border-radius: 5px;",
                    htmltools::h4(style = "margin-top: 0; color: #1976d2;", result$comparison),
                    test_results,
                    contingency_table
                )
            })
            
            return(comparison_divs)
        },
        
        # Helper function to create main contingency table HTML
        .createMainContingencyTableHTML = function(contTable, expValues = NULL, row_var_name = NULL, col_var_name = NULL) {
            row_names <- rownames(contTable)
            col_names <- colnames(contTable)
            
            # Create header row with variable names
            header_cells <- list(
                htmltools::tags$th(
                    style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center; background-color: #f8f9fa;", 
                    htmltools::div(
                        style = "font-weight: bold; color: #495057;",
                        if (!is.null(col_var_name)) paste0(col_var_name, " →") else "",
                        htmltools::br(),
                        if (!is.null(row_var_name)) paste0(row_var_name, " ↓") else ""
                    )
                )
            )
            
            # Add column headers with variable name context
            for (col in col_names) {
                header_cells[[length(header_cells) + 1]] <- htmltools::tags$th(
                    style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center; background-color: #e3f2fd;", 
                    htmltools::div(
                        style = "font-weight: bold;",
                        if (!is.null(col_var_name)) {
                            list(
                                htmltools::div(style = "font-size: 11px; color: #6c757d; margin-bottom: 2px;", col_var_name),
                                htmltools::div(style = "font-size: 13px; color: #212529;", col)
                            )
                        } else {
                            col
                        }
                    )
                )
            }
            header_row <- htmltools::tags$tr(style = "background-color: #e3f2fd;", header_cells)
            
            # Create data rows
            data_rows <- lapply(seq_along(row_names), function(i) {
                row_cells <- list(
                    htmltools::tags$th(
                        style = "border: 1px solid #e1e5e9; padding: 8px; background-color: #e3f2fd; font-weight: bold;",
                        if (!is.null(row_var_name)) {
                            htmltools::div(
                                htmltools::div(style = "font-size: 11px; color: #6c757d; margin-bottom: 2px;", row_var_name),
                                htmltools::div(style = "font-size: 13px; color: #212529;", row_names[i])
                            )
                        } else {
                            row_names[i]
                        }
                    )
                )
                
                # Add data cells
                for (j in seq_along(col_names)) {
                    obs <- contTable[i, j]
                    
                    if (!is.null(expValues)) {
                        exp <- round(expValues[i, j], 1)
                        cell_content <- list(obs, htmltools::br(), htmltools::tags$small(paste0("(", exp, ")")))
                    } else {
                        cell_content <- obs
                    }
                    
                    row_cells <- c(row_cells, list(
                        htmltools::tags$td(
                            style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center;",
                            cell_content
                        )
                    ))
                }
                
                htmltools::tags$tr(row_cells)
            })
            
            # Create complete table with jamovi-like styling
            table_content <- htmltools::tags$table(
                style = "border-collapse: collapse; width: 100%; margin: 15px 0; font-family: 'Segoe UI', system-ui, sans-serif; font-size: 13px; background-color: white; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
                htmltools::tags$thead(header_row),
                htmltools::tags$tbody(data_rows)
            )
            
            # Add explanation if expected values are shown
            if (!is.null(expValues)) {
                full_content <- htmltools::div(
                    htmltools::p("Values shown as: Observed", htmltools::br(), htmltools::tags$small("(Expected)")),
                    table_content
                )
                return(as.character(full_content))
            } else {
                return(as.character(table_content))
            }
        },
        
        # Helper function to create contingency table HTML using htmltools
        .createContingencyTableHTML = function(subtable, row_var_name = NULL, col_var_name = NULL) {
            row_names <- rownames(subtable)
            col_names <- colnames(subtable)
            row_totals <- rowSums(subtable)
            col_totals <- colSums(subtable)
            grand_total <- sum(subtable)
            
            # Create table header
            header_row <- htmltools::tags$tr(
                style = "background-color: #e3f2fd;",
                htmltools::tags$th(
                    style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center; background-color: #f8f9fa;", 
                    if (!is.null(col_var_name) || !is.null(row_var_name)) {
                        htmltools::div(
                            style = "font-weight: bold; color: #495057; font-size: 11px;",
                            if (!is.null(col_var_name)) paste0(col_var_name, " →") else "",
                            htmltools::br(),
                            if (!is.null(row_var_name)) paste0(row_var_name, " ↓") else ""
                        )
                    } else {
                        ""
                    }
                ),
                htmltools::tags$th(
                    style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center;", 
                    if (!is.null(col_var_name)) {
                        htmltools::div(
                            htmltools::div(style = "font-size: 11px; color: #6c757d; margin-bottom: 2px;", col_var_name),
                            htmltools::div(style = "font-size: 13px; color: #212529;", col_names[1])
                        )
                    } else {
                        col_names[1]
                    }
                ),
                htmltools::tags$th(
                    style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center;", 
                    if (!is.null(col_var_name)) {
                        htmltools::div(
                            htmltools::div(style = "font-size: 11px; color: #6c757d; margin-bottom: 2px;", col_var_name),
                            htmltools::div(style = "font-size: 13px; color: #212529;", col_names[2])
                        )
                    } else {
                        col_names[2]
                    }
                ),
                htmltools::tags$th(style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center;", htmltools::strong("Total"))
            )
            
            # Create data rows
            data_rows <- lapply(1:2, function(j) {
                row_pct1 <- round(subtable[j, 1] / row_totals[j] * 100, 1)
                row_pct2 <- round(subtable[j, 2] / row_totals[j] * 100, 1)
                
                htmltools::tags$tr(
                    htmltools::tags$td(
                        style = "border: 1px solid #e1e5e9; padding: 8px; background-color: #e3f2fd; font-weight: bold;", 
                        if (!is.null(row_var_name)) {
                            htmltools::div(
                                htmltools::div(style = "font-size: 11px; color: #6c757d; margin-bottom: 2px;", row_var_name),
                                htmltools::div(style = "font-size: 13px; color: #212529;", row_names[j])
                            )
                        } else {
                            row_names[j]
                        }
                    ),
                    htmltools::tags$td(
                        style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center;",
                        subtable[j, 1], htmltools::br(), htmltools::tags$small(paste0("(", row_pct1, "%)"))
                    ),
                    htmltools::tags$td(
                        style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center;",
                        subtable[j, 2], htmltools::br(), htmltools::tags$small(paste0("(", row_pct2, "%)"))
                    ),
                    htmltools::tags$td(
                        style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center; font-weight: bold;",
                        row_totals[j]
                    )
                )
            })
            
            # Create total row
            col_pct1 <- round(col_totals[1] / grand_total * 100, 1)
            col_pct2 <- round(col_totals[2] / grand_total * 100, 1)
            
            total_row <- htmltools::tags$tr(
                style = "background-color: #f5f5f5; font-weight: bold;",
                htmltools::tags$td(style = "border: 1px solid #e1e5e9; padding: 8px;", "Total"),
                htmltools::tags$td(
                    style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center;",
                    col_totals[1], htmltools::br(), htmltools::tags$small(paste0("(", col_pct1, "%)"))
                ),
                htmltools::tags$td(
                    style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center;",
                    col_totals[2], htmltools::br(), htmltools::tags$small(paste0("(", col_pct2, "%)"))
                ),
                htmltools::tags$td(
                    style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center;",
                    grand_total
                )
            )
            
            # Build complete table
            htmltools::tags$table(
                style = "border-collapse: collapse; width: 100%; margin: 10px 0;",
                htmltools::tags$thead(header_row),
                htmltools::tags$tbody(data_rows, total_row)
            )
        },
        
        # Validate chi-square test assumptions
        .validateAssumptions = function(contTable, chiSqTest) {
            expected <- chiSqTest$expected
            total_cells <- length(expected)
            low_expected_5 <- sum(expected < 5)
            low_expected_1 <- sum(expected < 1)
            
            # Calculate proportions
            prop_low_5 <- low_expected_5 / total_cells
            prop_low_1 <- low_expected_1 / total_cells
            
            # Determine warning level
            warning_level <- "none"
            warning_msg <- ""
            recommendation <- ""
            
            if (prop_low_1 > 0) {
                warning_level <- "severe"
                warning_msg <- sprintf(.("Critical: %.0f%% of cells have expected counts < 1"), prop_low_1 * 100)
                recommendation <- .("Chi-square test is not valid. Use Fisher's exact test instead.")
            } else if (prop_low_5 > 0.2) {
                warning_level <- "moderate"
                warning_msg <- sprintf(.("Warning: %.0f%% of cells have expected counts < 5"), prop_low_5 * 100)
                recommendation <- .("Consider using Fisher's exact test for more reliable results.")
            } else if (low_expected_5 > 0) {
                warning_level <- "mild"
                warning_msg <- sprintf(.("Note: %d cell(s) have expected counts < 5"), low_expected_5)
                recommendation <- .("Results should be interpreted with caution.")
            }
            
            # Check sample size
            total_n <- sum(contTable)
            if (total_n < 20) {
                warning_level <- "moderate"
                warning_msg <- c(warning_msg, sprintf(.("Small sample size (n=%d)"), total_n))
                recommendation <- c(recommendation, .("Results may be unreliable with small samples."))
            }
            
            
            # Check power
            power_results <- private$.checkPower(total_n, chiSqTest$parameter)
            if (power_results$underpowered) {
                # If we don't already have a severe warning, set to moderate
                if (warning_level == "none" || warning_level == "mild") {
                    warning_level <- "moderate"
                }
                warning_msg <- c(warning_msg, power_results$warning_msg)
                recommendation <- c(recommendation, power_results$recommendation)
            }
            
            return(list(
                warning_level = warning_level,
                warning_msg = warning_msg,
                recommendation = recommendation,
                low_expected_5 = low_expected_5,
                low_expected_1 = low_expected_1,
                prop_low_5 = prop_low_5,
                prop_low_1 = prop_low_1,
                total_n = total_n,
                power_n_required = power_results$n_required
            ))
        },
        
        # Check statistical power for chi-square test
        .checkPower = function(total_n, df) {
            # Define medium effect size (w = 0.3)
            medium_effect <- 0.3
            target_power <- 0.8
            alpha <- 0.05
            
            # Default values
            underpowered <- FALSE
            warning_msg <- NULL
            recommendation <- NULL
            n_required <- NULL
            
            # Attempt to use pwr package if available
            if (requireNamespace("pwr", quietly = TRUE)) {
                # Calculate power for actual n
                power_actual <- try(pwr::pwr.chisq.test(w = medium_effect, df = df, N = total_n, sig.level = alpha), silent = TRUE)
                
                # Calculate required n for target power
                power_required <- try(pwr::pwr.chisq.test(w = medium_effect, df = df, power = target_power, sig.level = alpha), silent = TRUE)
                
                if (!inherits(power_actual, "try-error") && !inherits(power_required, "try-error")) {
                    n_required <- ceiling(power_required$N)
                    
                    if (power_actual$power < target_power) {
                        underpowered <- TRUE
                        warning_msg <- sprintf(.("The study may be underpowered (Power = %.1f%%) to detect a medium effect size."), power_actual$power * 100)
                        recommendation <- sprintf(.("A total sample size of at least n=%d is recommended to achieve 80%% power."), n_required)
                    }
                }
            } else {
                # Fallback to heuristic if pwr not available
                # Cohen (1988) suggests n=88 for df=1, n=107 for df=2, etc. for medium effect
                # A safe general heuristic for clinical studies is total_n < 50 or < 100 depending on complexity
                heuristic_limit <- 50 + (df * 10)
                if (total_n < heuristic_limit) {
                    underpowered <- TRUE
                    warning_msg <- sprintf(.("Small sample size (n=%d) may limit statistical power."), total_n)
                    recommendation <- .("Consider if the study has sufficient power to detect clinically relevant differences.")
                }
            }
            
            return(list(
                underpowered = underpowered,
                warning_msg = warning_msg,
                recommendation = recommendation,
                n_required = n_required
            ))
        },
        
        # Generate clinical summary
        .generateClinicalSummary = function(chiSqTest, contTable, pairwise_results = NULL, rows = NULL, cols = NULL) {
            # Calculate effect size (Cramér's V)
            n <- sum(contTable)
            k <- min(nrow(contTable), ncol(contTable))
            cramers_v <- sqrt(chiSqTest$statistic / (n * (k - 1)))
            
            # Interpret effect size based on degrees of freedom
            df <- chiSqTest$parameter
            if (df == 1) {
                effect_interpretation <- if(cramers_v < 0.1) .("negligible")
                    else if(cramers_v < 0.3) .("small")
                    else if(cramers_v < 0.5) .("medium")
                    else .("large")
            } else if (df == 2) {
                effect_interpretation <- if(cramers_v < 0.07) .("negligible")
                    else if(cramers_v < 0.21) .("small")
                    else if(cramers_v < 0.35) .("medium")
                    else .("large")
            } else {
                effect_interpretation <- if(cramers_v < 0.06) .("negligible")
                    else if(cramers_v < 0.17) .("small")
                    else if(cramers_v < 0.29) .("medium")
                    else .("large")
            }
            
            # Count significant pairwise comparisons
            n_significant_pairs <- 0
            if (!is.null(pairwise_results) && length(pairwise_results) > 0) {
                n_significant_pairs <- sum(sapply(pairwise_results, function(x) {
                    x$actual_pvalue_adjusted < self$options$sig
                }))
            }
            
            # Generate natural language summary
            var_names <- if (!is.null(rows) && !is.null(cols)) {
                paste0(.("'"), rows, .("' and '"), cols, .("'"))
            } else {
                .("the row and column variables")
            }
            
            significance_text <- if (chiSqTest$p.value < 0.001) {
                .("highly significant")
            } else if (chiSqTest$p.value < 0.01) {
                .("very significant")
            } else if (chiSqTest$p.value < 0.05) {
                .("statistically significant")
            } else {
                .("not statistically significant")
            }
            
            summary_text <- sprintf(
                .("The association between %s was %s (χ²(%d) = %.2f, p %s, n = %d). The effect size was %s (Cramér's V = %.3f)."),
                var_names,
                significance_text,
                chiSqTest$parameter,
                chiSqTest$statistic,
                if (chiSqTest$p.value < 0.001) "< 0.001" else sprintf("= %.3f", chiSqTest$p.value),
                n,
                effect_interpretation,
                cramers_v
            )
            
            # Add post-hoc information if applicable
            if (chiSqTest$p.value < 0.05 && !is.null(pairwise_results) && length(pairwise_results) > 0) {
                posthoc_text <- sprintf(
                    .(" Post-hoc analysis revealed %d significant pairwise difference(s) out of %d comparisons after %s correction."),
                    n_significant_pairs,
                    length(pairwise_results),
                    self$options$posthoc
                )
                summary_text <- paste0(summary_text, posthoc_text)
            }
            
            return(list(
                summary_text = summary_text,
                effect_size = cramers_v,
                effect_interpretation = effect_interpretation,
                n_significant_pairs = n_significant_pairs,
                significant = chiSqTest$p.value < 0.05
            ))
        },

        # Generate copy-ready report sentences
        .generateReportSentences = function(chiSqTest, contTable, clinical_summary, assumptions) {
            sentences <- list()
            
            # Methods sentence
            sentences$methods <- sprintf(
                .("A chi-square test of independence was performed to examine the relationship between %s categorical variables. %s"),
                if (nrow(contTable) == 2 && ncol(contTable) == 2) "two binary" else "two",
                if (assumptions$warning_level != "none") assumptions$recommendation[1] else ""
            )
            
            # Results sentence
            sentences$results <- clinical_summary$summary_text
            
            # Table description
            sentences$table <- sprintf(
                .("The contingency table consisted of %d rows and %d columns with a total sample size of %d."),
                nrow(contTable), ncol(contTable), sum(contTable)
            )
            
            # Statistical conclusion
            sentences$conclusion <- if (chiSqTest$p.value < 0.05) {
                sprintf(
                    .("The null hypothesis of independence was rejected at the α = %.2f significance level."),
                    self$options$sig
                )
            } else {
                sprintf(
                    .("The null hypothesis of independence could not be rejected at the α = %.2f significance level."),
                    self$options$sig
                )
            }
            
            return(sentences)
        },
        
        # Create statistical glossary
        .createGlossaryPanel = function() {
            glossary <- htmltools::div(
                style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #6c757d; margin: 10px 0;",
                htmltools::h4(.("Statistical Terms Glossary"), style = "color: #495057; margin-top: 0;"),

                htmltools::tags$dl(
                    htmltools::tags$dt(htmltools::strong(.("Chi-square test (χ²)"))),
                    htmltools::tags$dd(.("Tests whether two categorical variables are independent. Used when you want to know if categories of one variable are associated with categories of another.")),

                    htmltools::tags$dt(htmltools::strong(.("p-value"))),
                    htmltools::tags$dd(.("The probability of observing results at least as extreme as what was found, assuming no real association exists. p < 0.05 typically indicates statistical significance.")),

                    htmltools::tags$dt(htmltools::strong(.("Cramér's V"))),
                    htmltools::tags$dd(.("Effect size measure for chi-square tests. Ranges from 0 (no association) to 1 (perfect association). Values: 0.1=small, 0.3=medium, 0.5=large effect.")),

                    htmltools::tags$dt(htmltools::strong(.("Standardized Residuals"))),
                    htmltools::tags$dd(.("Show which specific cell combinations contribute to significance. Values > 2 or < -2 indicate cells that differ significantly from expected.")),

                    htmltools::tags$dt(htmltools::strong(.("Expected Counts"))),
                    htmltools::tags$dd(.("The number of observations expected in each cell if the variables were independent. Chi-square test requires most cells to have expected counts ≥ 5.")),

                    htmltools::tags$dt(htmltools::strong(.("Fisher's Exact Test"))),
                    htmltools::tags$dd(.("Alternative to chi-square when expected counts are low. Provides exact p-values rather than approximations.")),

                    htmltools::tags$dt(htmltools::strong(.("Bonferroni Correction"))),
                    htmltools::tags$dd(.("Adjusts p-values when multiple comparisons are made to control false positive rate. Divides significance level by number of tests.")),

                    htmltools::tags$dt(htmltools::strong(.("Post-hoc Tests"))),
                    htmltools::tags$dd(.("Follow-up tests performed after a significant overall result to identify which specific group pairs differ significantly."))
                )
            )

            return(as.character(glossary))
        },
        
        # Create educational HTML panels
        .createEducationalPanel = function(type = "overview", num_comparisons = NULL, alpha = 0.05) {
            if (type == "overview") {
                return(htmltools::div(
                    style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #1976d2; margin: 10px 0;",
                    htmltools::h4(.("Chi-Square Post-Hoc Analysis Guide"), style = "color: #1976d2; margin-top: 0;"),
                    htmltools::p(htmltools::strong(.("Three-Step Comprehensive Analysis:"))),
                    htmltools::div(
                        "1. ", htmltools::strong(.("Overall Chi-Square Test:")), " ", .("Tests if there's any association between variables"), htmltools::br(),
                        "2. ", htmltools::strong(.("Residuals Analysis:")), " ", .("Identifies which specific cells contribute to significance"), htmltools::br(),
                        "3. ", htmltools::strong(.("Pairwise Comparisons:")), " ", .("Formal hypothesis testing between group pairs")
                    ),
                    htmltools::p(htmltools::em(.("Recommended approach: Start with residuals analysis for pattern identification, then use pairwise tests for formal hypothesis testing.")))
                ))
            } else if (type == "residuals") {
                return(htmltools::div(
                    style = "padding: 15px; background-color: #e8f5e8; border-left: 4px solid #4caf50; margin: 10px 0;",
                    htmltools::h4(.("Standardized Residuals Interpretation"), style = "color: #2e7d32; margin-top: 0;"),
                    htmltools::p(htmltools::strong(.("What are standardized residuals?"))),
                    htmltools::p(.("Standardized residuals measure how much each cell deviates from what we'd expect if there was no association. They're calculated as (Observed - Expected) / √Expected, then adjusted for multiple comparisons.")),
                    htmltools::div(
                        style = "background-color: #f1f8e9; padding: 10px; border-radius: 3px;",
                        htmltools::strong(.("Interpretation Guidelines:")), htmltools::br(),
                        .("• |Residual| > 2.0: Suggests meaningful deviation"), htmltools::br(),
                        .("• |Residual| > 3.0: Strong evidence of deviation"), htmltools::br(),
                        .("• Positive values: Over-represented (more than expected)"), htmltools::br(),
                        .("• Negative values: Under-represented (fewer than expected)")
                    )
                ))
            } else if (type == "multiple_testing" && !is.null(num_comparisons)) {
                adjusted_alpha <- alpha / num_comparisons
                return(htmltools::div(
                    style = "padding: 15px; background-color: #fff3e0; border-left: 4px solid #ff9800; margin: 10px 0;",
                    htmltools::h4(.("Multiple Testing Correction"), style = "color: #e65100; margin-top: 0;"),
                    htmltools::p(htmltools::strong(.("Why correction is needed:"))),
                    htmltools::p(.("When performing multiple tests, the probability of finding at least one false positive increases. With {num_comparisons} comparisons, there's a {false_positive_rate}% chance of a false positive without correction.")),
                    htmltools::div(
                        style = "background-color: #fce4ec; padding: 10px; border-radius: 3px;",
                        htmltools::strong(.("Correction Applied:")), htmltools::br(),
                        .("• Number of comparisons:"), " ", num_comparisons, htmltools::br(),
                        .("• Adjusted significance level:"), " α = ", round(adjusted_alpha, 4), htmltools::br(),
                        .("• Use adjusted p-values for significance decisions")
                    )
                ))
            }
        },
        
        # Create detailed guidance HTML for residuals interpretation
        .createResidualsGuidanceHTML = function(cutoff) {
            guidance <- htmltools::div(
                style = "padding: 15px; background-color: #f0f7ff; border-left: 4px solid #1976d2; margin: 10px 0; font-family: 'Segoe UI', system-ui, sans-serif;",
                htmltools::h4(.("Residuals Interpretation Guidance"), style = "color: #1976d2; margin-top: 0;"),
                htmltools::p(
                    htmltools::strong(.("How to read the Residuals Table:")),
                    htmltools::br(),
                    .("Standardized residuals identify which specific cells in the table drive the overall significant association. They represent the distance (in standard deviations) between the observed count and the expected count.")
                ),
                htmltools::div(
                    style = "background-color: #ffffff; padding: 12px; border: 1px border; border-color: #d1e3f8; border-radius: 5px; margin-bottom: 15px;",
                    htmltools::tags$ul(
                        style = "margin: 0; padding-left: 20px;",
                        htmltools::tags$li(
                            htmltools::strong(.("Positive values (+)")), ": ", 
                            .("Over-represented. There are MORE observations in this cell than expected.")
                        ),
                        htmltools::tags$li(
                            htmltools::strong(.("Negative values (-)")), ": ", 
                            .("Under-represented. There are FEWER observations in this cell than expected.")
                        ),
                        htmltools::tags$li(
                            htmltools::strong(.("Significance:")), " ",
                            sprintf(.("Values exceeding ±%.2f are considered statistically significant."), cutoff)
                        )
                    )
                ),
                htmltools::div(
                    style = "font-style: italic; background-color: #f8f9fa; padding: 10px; border-radius: 4px; border: 1px dashed #dee2e6;",
                    htmltools::strong(.("Example Interpretation:")), htmltools::br(),
                    sprintf(.("If the cell 'Grade 3 × Positive' has a residual of +3.2 (with cutoff ±%.2f):"), cutoff), htmltools::br(),
                    .("It indicates that Grade 3 cases are significantly over-represented among the Positive group compared to what would be expected by chance.")
                )
            )
            return(as.character(guidance))
        },
        
        # Calculate Bootstrap CI for Phi Coefficient
        .calculatePhiCI = function(subtable, conf_level = 0.95, n_boot = 999) {
            if (!requireNamespace("boot", quietly = TRUE)) {
                return(NA)
            }
            
            # Phi statistic function for boot
            phi_fun <- function(data, indices) {
                d <- data[indices, ]
                tab <- table(d[[1]], d[[2]])
                # Handle cases where bootstrap sample has only 1 dimension
                if (any(dim(tab) < 2)) return(0)
                
                # Chi-square with no correction for Phi calculation
                chi <- try(stats::chisq.test(tab, correct = FALSE), silent = TRUE)
                if (inherits(chi, "try-error")) return(0)
                
                return(sqrt(chi$statistic / sum(tab)))
            }
            
            # Prepare data for bootstrapping (reconstruct cases from table)
            row_names <- rownames(subtable)
            col_names <- colnames(subtable)
            cases <- expand.grid(R = row_names, C = col_names, stringsAsFactors = FALSE)
            counts <- as.vector(subtable)
            df <- cases[rep(seq_len(nrow(cases)), counts), ]
            
            # Run bootstrap
            boot_res <- try(boot::boot(data = df, statistic = phi_fun, R = n_boot), silent = TRUE)
            if (inherits(boot_res, "try-error")) return(NA)
            
            # Calculate BCa interval
            ci_res <- try(boot::boot.ci(boot_res, conf = conf_level, type = "bca"), silent = TRUE)
            
            # Fallback to percentile if BCa fails or is inappropriate
            if (inherits(ci_res, "try-error") || is.null(ci_res$bca)) {
                ci_res <- try(boot::boot.ci(boot_res, conf = conf_level, type = "perc"), silent = TRUE)
            }
            
            if (inherits(ci_res, "try-error")) return(NA)
            
            # Extract CI
            interval <- if (!is.null(ci_res$bca)) ci_res$bca[4:5] else ci_res$percent[4:5]
            return(sprintf("[%s, %s]", round(interval[1], 3), round(interval[2], 3)))
        },
        
        # Helper method to handle initial setup and validation
        .handleInitialSetup = function() {
            # ToDo Message ----
            if (is.null(self$options$rows) || is.null(self$options$cols)) {
                todo <- paste0("
                <br>", .("Welcome to ClinicoPath Chi-Square Post-Hoc Tests"), "
                <br><br>
                ", .("This tool performs a Chi-Square test followed by pairwise post-hoc tests for all combinations of category levels when the overall Chi-Square test is significant."), "
                <br><br>
                <strong>", .("Data Input Options:"), "</strong>
                <br>• <strong>", .("Individual observations:"), "</strong> ", .("Select row and column variables from raw data"), "
                <br>• <strong>", .("Frequency counts:"), "</strong> ", .("Select row and column variables plus a counts variable for aggregated data"), "
                <br><br>
                ", .("The post-hoc tests help identify which specific group combinations contribute to the significant overall effect."), "
                <hr><br>
                ")
                html <- self$results$todo
                html$setContent(todo)
                return(FALSE) # Indicates setup not complete
            } else {
                todo <- ""
                html <- self$results$todo
                html$setContent(todo)
                return(TRUE) # Setup complete
            }
        },
        
        # Helper method to prepare and validate data
        .prepareAndValidateData = function() {
            # Error Message ----
            if (nrow(self$data) == 0) {
                # Use HTML message in todo element instead of Notice object
                self$results$todo$setContent(
                    paste0("<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; color: #721c24;'>",
                          "<strong>Error:</strong> Data contains no (complete) rows. Please check your dataset and variable selections.",
                          "</div>"))
                return(NULL)
            }

            # Prepare Data ----
            data <- self$data
            rows <- self$options$rows
            cols <- self$options$cols
            counts <- self$options$counts

            # Check if counts variable exists when specified
            if (!is.null(counts) && !(counts %in% names(data))) {
                # Use HTML message in todo element instead of Notice object
                self$results$todo$setContent(
                    paste0("<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; color: #721c24;'>",
                          "<strong>Error:</strong> The counts variable '", counts, "' does not exist in the data. Please select a valid numeric variable for counts.",
                          "</div>"))
                return(NULL)
            }

            # Exclude NA ----
            excl <- self$options$excl
            if (excl) {
                data <- jmvcore::naOmit(data)
            }

            return(list(data = data, rows = rows, cols = cols, counts = counts))
        },
        
        # Shared helper method to create contingency table (used by both .run() and .plot())
        .buildContingencyTable = function(data, rows, cols, counts, excl, show_warnings = TRUE) {
            # Handle missing values
            if (excl) {
                data <- jmvcore::naOmit(data)
            }
            
            # Create contingency table
            contTable <- try({
                if (!is.null(counts)) {
                    # Data is already summarized with counts - create weighted contingency table
                    # Ensure counts variable is numeric (jamovi may convert it to factor)
                    data[[counts]] <- as.numeric(as.character(data[[counts]]))
                    
                    # Build formula with backticks to handle special variable names
                    formula_str <- paste0("`", counts, "` ~ `", rows, "` + `", cols, "`")
                    xtabs(as.formula(formula_str), data = data)
                } else {
                    # Regular individual observation data
                    table(data[[rows]], data[[cols]], useNA = if(excl) "no" else "ifany")
                }
            }, silent = TRUE)
            
            # Handle errors
            if (inherits(contTable, "try-error")) {
                # Use HTML message in todo element instead of Notice object
                if (show_warnings) {
                    self$results$todo$setContent(
                        paste0("<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; color: #721c24;'>",
                              "<strong>Error:</strong> Error creating contingency table. Please check your data format and variable selections. Ensure rows and columns variables are categorical.",
                              "</div>"))
                }
                return(NULL)
            }

            # Validate table dimensions
            if (any(dim(contTable) < 2)) {
                # Use HTML message in todo element instead of Notice object
                if (show_warnings) {
                    self$results$todo$setContent(
                        paste0("<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; color: #721c24;'>",
                              "<strong>Error:</strong> Contingency table must have at least 2 rows and 2 columns. Please check that your variables have at least 2 categories each.",
                              "</div>"))
                }
                return(NULL)
            }
            
            # Add warning message about weighted data (only in main analysis)
            if (!is.null(counts) && show_warnings && self$options$showEducational) {
                weight_warning <- htmltools::div(
                    style = "padding: 10px; background-color: #e1f5fe; border-left: 4px solid #0277bd; margin: 10px 0;",
                    htmltools::h5("📊 Weighted Data Analysis", style = "color: #01579b; margin-top: 0;"),
                    htmltools::p("The data is being treated as frequency counts. Each row represents a combination of categories with the specified count/weight.")
                )
                self$results$weightedDataInfo$setContent(as.character(weight_warning))
            }
            
            return(contTable)
        },
        
        # Helper method to create contingency table (wrapper for backwards compatibility)
        .createContingencyTable = function(data, rows, cols, counts) {
            return(private$.buildContingencyTable(data, rows, cols, counts, self$options$excl, show_warnings = TRUE))
        },
        
        # Helper method to perform chi-square test and populate results
        .performChiSquareTest = function(contTable, rows = NULL, cols = NULL) {
            # Perform Chi-Square Test ----
            chiSqTest <- stats::chisq.test(contTable, correct = FALSE)

            # Add chi-square results to the table ----
            self$results$chisqTable$setRow(
                rowNo = 1,
                values = list(
                    stat = "Chi-Square",
                    value = chiSqTest$statistic,
                    df = chiSqTest$parameter,
                    p = chiSqTest$p.value
                )
            )

            # Format contingency table with optional expected values ----
            expValues <- if (self$options$exp) chiSqTest$expected else NULL
            tableHtml <- private$.createMainContingencyTableHTML(contTable, expValues, rows, cols)
            self$results$contingencyTable$setContent(tableHtml)
            
            return(chiSqTest)
        },
        
        # Helper method to handle residuals analysis
        .handleResidualsAnalysis = function(chiSqTest, contTable, rows = NULL, cols = NULL) {
            if (!self$options$showResiduals) return()
            
            # Use custom cutoff if provided
            cutoff_value <- if (self$options$residualsCutoff != 2.0) self$options$residualsCutoff else NULL
            residuals_analysis <- private$.analyzeResiduals(chiSqTest, contTable, self$options$sig, cutoff_value)
            
            # Create residuals guidance panel and results using htmltools
            guidance_cutoff <- if (!is.null(cutoff_value)) cutoff_value else residuals_analysis$critical_value
            residuals_guidance <- private$.createResidualsGuidanceHTML(guidance_cutoff)
            
            # Create residuals interpretation panel and results using htmltools
            residuals_panel <- private$.createEducationalPanel("residuals")
            residuals_table <- private$.createResidualsTableHTML(residuals_analysis, contTable, rows, cols)
            
            # Combine panel and table
            full_content <- htmltools::div(
                style = "margin: 15px 0;",
                residuals_panel,
                residuals_table,
                htmltools::p(
                    style = "margin-top: 10px;",
                    htmltools::tags$small(
                        htmltools::strong("Critical value for significance: "),
                        "±", round(residuals_analysis$critical_value, 3)
                    )
                )
            )
            
            self$results$residualsGuidance$setContent(residuals_guidance)
            self$results$residualsAnalysis$setContent(as.character(full_content))
        },
        
        # Helper method to create residuals table using htmltools
        .createResidualsTableHTML = function(residuals_analysis, contTable, row_var_name = NULL, col_var_name = NULL) {
            row_names <- rownames(contTable)
            col_names <- colnames(contTable)
            
            # Create header row with variable names
            header_cells <- list(
                htmltools::tags$th(
                    style = "border: 1px solid #e1e5e9; padding: 8px; background-color: #f8f9fa;", 
                    htmltools::div(
                        style = "font-weight: bold; color: #495057;",
                        if (!is.null(col_var_name)) paste0(col_var_name, " →") else "",
                        htmltools::br(),
                        if (!is.null(row_var_name)) paste0(row_var_name, " ↓") else ""
                    )
                )
            )
            
            # Add column headers with variable name context
            for (col in col_names) {
                header_cells[[length(header_cells) + 1]] <- htmltools::tags$th(
                    style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center; background-color: #e8f5e8;", 
                    htmltools::div(
                        style = "font-weight: bold;",
                        if (!is.null(col_var_name)) {
                            list(
                                htmltools::div(style = "font-size: 11px; color: #6c757d; margin-bottom: 2px;", col_var_name),
                                htmltools::div(style = "font-size: 13px; color: #212529;", col)
                            )
                        } else {
                            col
                        }
                    )
                )
            }
            header_row <- htmltools::tags$tr(style = "background-color: #e8f5e8;", header_cells)
            
            # Create data rows
            data_rows <- lapply(seq_along(row_names), function(i) {
                # Row header
                row_cells <- list(
                    htmltools::tags$td(
                        style = "border: 1px solid #e1e5e9; padding: 8px; font-weight: bold; background-color: #f5f5f5;",
                        if (!is.null(row_var_name)) {
                            htmltools::div(
                                htmltools::div(style = "font-size: 11px; color: #6c757d; margin-bottom: 2px;", row_var_name),
                                htmltools::div(style = "font-size: 13px; color: #212529;", row_names[i])
                            )
                        } else {
                            row_names[i]
                        }
                    )
                )
                
                # Data cells
                for (j in seq_along(col_names)) {
                    residual_val <- round(residuals_analysis$adjusted_residuals[i, j], 3)
                    is_significant <- residuals_analysis$significant_cells[i, j]
                    interpretation <- residuals_analysis$interpretation[i, j]
                    
                    # Determine cell styling based on significance
                    cell_style <- if (is_significant) {
                        if (residual_val > 0) {
                            "border: 1px solid #e1e5e9; padding: 8px; text-align: center; background-color: #ffebee; color: #c62828; font-weight: bold;"
                        } else {
                            "border: 1px solid #e1e5e9; padding: 8px; text-align: center; background-color: #e3f2fd; color: #1565c0; font-weight: bold;"
                        }
                    } else {
                        "border: 1px solid #e1e5e9; padding: 8px; text-align: center; background-color: #f9f9f9;"
                    }
                    
                    row_cells <- c(row_cells, list(
                        htmltools::tags$td(
                            style = cell_style,
                            residual_val,
                            htmltools::br(),
                            htmltools::tags$small(interpretation)
                        )
                    ))
                }
                
                htmltools::tags$tr(row_cells)
            })
            
            # Create complete table with jamovi-like styling
            htmltools::tags$table(
                style = "border-collapse: collapse; width: 100%; margin: 15px 0; font-family: 'Segoe UI', system-ui, sans-serif; font-size: 13px; background-color: white; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
                htmltools::tags$thead(header_row),
                htmltools::tags$tbody(data_rows)
            )
        },
        
        # Handle detailed comparison tables display
        .handleDetailedComparisons = function(chiSqTest, contTable, rows = NULL, cols = NULL) {
            overall_significant <- chiSqTest$p.value < self$options$sig
            
            if (overall_significant) {
                adjustMethod <- self$options$posthoc
                pairwise_results <- private$.robustPairwiseTests(contTable, adjustMethod, self$options$testSelection)
                
                if (length(pairwise_results) > 0) {
                    detailed_content <- htmltools::div(
                        style = "margin: 15px 0;",
                        htmltools::h4(.("Detailed 2×2 Comparison Tables"), style = "color: #1976d2; margin-bottom: 15px;")
                    )
                    
                    tables_html <- lapply(pairwise_results, function(result) {
                        # Determine which test method was used (stored in result)
                        test_used <- result$test_used
                        p_value <- result$actual_pvalue
                        p_adj <- result$actual_pvalue_adjusted
                        
                        # Create the 2x2 table HTML
                        table_html <- private$.createContingencyTableHTML(result$subtable)
                        
                        # Create the complete comparison section
                        htmltools::div(
                            style = "margin: 20px 0; padding: 15px; border: 1px solid #e1e5e9; border-radius: 4px;",
                            htmltools::h5(result$comparison, style = "color: #495057; margin-top: 0;"),
                            table_html,
                            htmltools::div(
                                style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 3px;",
                                htmltools::div(
                                    style = "display: inline-block; margin-right: 20px;",
                                    htmltools::strong(.("Test used: ")), test_used
                                ),
                                htmltools::div(
                                    style = "display: inline-block; margin-right: 20px;",
                                    htmltools::strong(.("p-value: ")), format.pval(p_value, digits = 3)
                                ),
                                htmltools::div(
                                    style = "display: inline-block; margin-right: 20px;",
                                    htmltools::strong(.("Adjusted p: ")), format.pval(p_adj, digits = 3)
                                ),
                                htmltools::div(
                                    style = "display: inline-block;",
                                    htmltools::strong(.("Significant: ")), if (p_adj < self$options$sig) .("Yes") else .("No")
                                )
                            )
                        )
                    })
                    
                    full_content <- htmltools::div(
                        detailed_content,
                        tables_html
                    )
                    
                    self$results$detailedComparisons$setContent(as.character(full_content))
                }
            }
        },
        
        # Helper method to handle post-hoc testing
        .handlePostHocTesting = function(chiSqTest, contTable, rows = NULL, cols = NULL) {
            # CRITICAL FIX: Check if user wants to disable post-hoc tests entirely
            # When posthoc = "none", skip all pairwise testing
            if (self$options$posthoc == "none") {
                # Use HTML message only (no Notice object to avoid serialization errors)
                message_text <- paste0(
                    "You selected 'None' for post-hoc method. No pairwise comparisons will be performed. ",
                    "If you want pairwise comparisons with no p-value adjustment, this feature is not currently available. ",
                    "Please select Bonferroni, Holm, or FDR for pairwise testing with appropriate corrections."
                )
                self$results$multipleTestingInfo$setContent(
                    paste0("<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb;'>",
                          "<strong>", .("Post-hoc Testing Disabled:"), "</strong> ",
                          message_text,
                          "</div>"))
                # Hide the posthoc table when no post-hoc testing is performed
                self$results$posthocTable$setVisible(FALSE)
                return(invisible(NULL))
            }

            # CRITICAL FIX: Enforce omnibus significance prerequisite
            # Only run post-hoc if overall chi-square is significant
            overall_significant <- chiSqTest$p.value < self$options$sig

            if (!overall_significant) {
                # Use HTML message only (no Notice object to avoid serialization errors)
                message_text <- paste0(
                    "Overall chi-square test is not significant (p = ",
                    format.pval(chiSqTest$p.value, digits = 3),
                    " ≥ ", self$options$sig, "). ",
                    "Post-hoc pairwise comparisons are only valid when the overall test is significant. ",
                    "Running pairwise tests after a non-significant omnibus test increases Type I error (false positives) ",
                    "and constitutes data dredging."
                )
                self$results$multipleTestingInfo$setContent(
                    paste0("<div style='padding: 15px; background-color: #fff3cd; border: 1px solid #ffc107;'>",
                          "<strong>", .("Post-hoc Testing Not Performed:"), "</strong> ",
                          message_text,
                          "</div>"))
                # Hide the posthoc table when omnibus test is not significant
                self$results$posthocTable$setVisible(FALSE)
                return(invisible(NULL))
            }

            adjustMethod <- self$options$posthoc
            
            # Use robust pairwise testing approach with user-selected test method
            pairwise_results <- private$.robustPairwiseTests(contTable, adjustMethod, self$options$testSelection)
            
            if (length(pairwise_results) > 0) {
                # Show the posthoc table since we have results to display
                self$results$posthocTable$setVisible(TRUE)

                fisher_used <- any(sapply(pairwise_results, function(x) identical(x$test_used, "Fisher's exact")))

                # Use HTML notice for educational panel (no Notice object to avoid serialization errors)
                fisher_notice <- NULL
                if (fisher_used) {
                    fisher_notice <- htmltools::div(
                        style = "padding: 10px; background-color: #e3f2fd; border-left: 4px solid #1976d2; margin: 8px 0;",
                        htmltools::strong(.("Method notice: ")),
                        .("Pairwise comparisons with expected cell counts < 5 are automatically analysed with Fisher's exact test; the reported p-values use that exact method.")
                    )
                }

                # Create multiple testing correction panel (conditional)
                if (self$options$showEducational) {
                    correction_panel <- private$.createEducationalPanel("multiple_testing",
                                                                        length(pairwise_results),
                                                                        self$options$sig)
                    if (!is.null(fisher_notice)) {
                        combined <- htmltools::div(correction_panel, fisher_notice)
                        self$results$multipleTestingInfo$setContent(as.character(combined))
                    } else {
                        self$results$multipleTestingInfo$setContent(as.character(correction_panel))
                    }
                } else if (!is.null(fisher_notice)) {
                    self$results$multipleTestingInfo$setContent(as.character(fisher_notice))
                }

                # Populate pairwise results table
                for (i in seq_along(pairwise_results)) {
                    result <- pairwise_results[[i]]
                    test_used <- result$test_used
                    p_raw <- result$actual_pvalue
                    p_adj <- result$actual_pvalue_adjusted

                    sig_indicator <- if (p_adj < self$options$sig) .("Yes") else .("No")
                    self$results$posthocTable$addRow(
                        rowKey = i,
                        values = list(
                            comparison = result$comparison,
                            test_method = test_used,
                            chi = result$chi_statistic,
                            p = p_raw,
                            padj = p_adj,
                            effect_size = round(result$effect_size, 3),
                            phi_ci = if (is.null(result$phi_ci)) "" else result$phi_ci,
                            sig = sig_indicator
                        )
                    )
                }

                # Create detailed comparison tables (conditional)
                if (self$options$showDetailedTables) {
                    detailed_html <- private$.createDetailedComparisonHTML(pairwise_results, rows, cols)
                    self$results$detailedComparisons$setContent(detailed_html)
                }
            } else {
                # No valid pairwise comparisons possible
                # Hide the posthoc table when no valid pairwise comparisons
                self$results$posthocTable$setVisible(FALSE)
                self$results$multipleTestingInfo$setContent(
                    paste0("<div style='padding: 15px; background-color: #fff3cd; border: 1px solid #ffeaa7;'>", .("No valid pairwise comparisons could be performed. This may be due to insufficient data in subtables or table structure."), "</div>"))
            }
            
            # Generate export table if requested
            if (self$options$exportResults) {
                private$.generateExportTable(chiSqTest, contTable)
            }
        },
        
        
        # Generate comprehensive export table
        .generateExportTable = function(chiSqTest, contTable) {
            export_data <- list()
            row_index <- 1
            
            # Overall Chi-Square Results
            export_data[[row_index]] <- list(
                category = "Overall Test",
                measure = "Chi-Square Statistic",
                value = round(chiSqTest$statistic, 4),
                interpretation = paste("Chi-square =", round(chiSqTest$statistic, 4))
            )
            row_index <- row_index + 1
            
            export_data[[row_index]] <- list(
                category = "Overall Test",
                measure = "Degrees of Freedom",
                value = chiSqTest$parameter,
                interpretation = paste("df =", chiSqTest$parameter)
            )
            row_index <- row_index + 1
            
            export_data[[row_index]] <- list(
                category = "Overall Test",
                measure = "P-value",
                value = format.pval(chiSqTest$p.value, digits = 4),
                interpretation = if (chiSqTest$p.value < 0.05) "Significant association" else "No significant association"
            )
            row_index <- row_index + 1
            
            # Contingency Table Data
            row_names <- rownames(contTable)
            col_names <- colnames(contTable)
            for (i in seq_along(row_names)) {
                for (j in seq_along(col_names)) {
                    export_data[[row_index]] <- list(
                        category = "Contingency Table",
                        measure = paste(row_names[i], "×", col_names[j]),
                        value = contTable[i, j],
                        interpretation = paste("Observed count:", contTable[i, j])
                    )
                    row_index <- row_index + 1
                }
            }
            
            # Residuals Analysis (if enabled)
            if (self$options$showResiduals) {
                cutoff_value <- if (self$options$residualsCutoff != 2.0) self$options$residualsCutoff else NULL
                residuals_analysis <- private$.analyzeResiduals(chiSqTest, contTable, self$options$sig, cutoff_value)
                
                export_data[[row_index]] <- list(
                    category = "Residuals Analysis",
                    measure = "Critical Value",
                    value = round(residuals_analysis$critical_value, 3),
                    interpretation = paste("±", round(residuals_analysis$critical_value, 3), "for significance")
                )
                row_index <- row_index + 1
                
                export_data[[row_index]] <- list(
                    category = "Residuals Analysis",
                    measure = "Significant Cells",
                    value = residuals_analysis$num_significant_cells,
                    interpretation = paste(residuals_analysis$num_significant_cells, "of", length(residuals_analysis$significant_cells), "cells significant")
                )
                row_index <- row_index + 1
                
                # Individual residuals
                for (i in seq_along(row_names)) {
                    for (j in seq_along(col_names)) {
                        export_data[[row_index]] <- list(
                            category = "Cell Residuals",
                            measure = paste(row_names[i], "×", col_names[j], "Residual"),
                            value = round(residuals_analysis$adjusted_residuals[i, j], 3),
                            interpretation = residuals_analysis$interpretation[i, j]
                        )
                        row_index <- row_index + 1
                    }
                }
            }
            
            # Post-hoc Results (if significant)
            if (chiSqTest$p.value < self$options$sig) {
                pairwise_results <- private$.robustPairwiseTests(contTable, self$options$posthoc, self$options$testSelection)
                
                if (length(pairwise_results) > 0) {
                    for (i in seq_along(pairwise_results)) {
                        result <- pairwise_results[[i]]
                        
                        test_name <- result$test_used
                        p_value <- result$actual_pvalue
                        p_adj <- result$actual_pvalue_adjusted
                        
                        export_data[[row_index]] <- list(
                            category = "Pairwise Comparisons",
                            measure = paste(result$comparison, "- Test"),
                            value = test_name,
                            interpretation = paste("Test method:", test_name)
                        )
                        row_index <- row_index + 1
                        
                        export_data[[row_index]] <- list(
                            category = "Pairwise Comparisons",
                            measure = paste(result$comparison, "- Chi-square"),
                            value = round(result$chi_statistic, 4),
                            interpretation = paste("Chi-square =", round(result$chi_statistic, 4))
                        )
                        row_index <- row_index + 1
                        
                        export_data[[row_index]] <- list(
                            category = "Pairwise Comparisons",
                            measure = paste(result$comparison, "- Raw p-value"),
                            value = format.pval(p_value, digits = 4),
                            interpretation = paste("Raw p =", format.pval(p_value, digits = 4))
                        )
                        row_index <- row_index + 1
                        
                        export_data[[row_index]] <- list(
                            category = "Pairwise Comparisons",
                            measure = paste(result$comparison, "- Adjusted p-value"),
                            value = format.pval(p_adj, digits = 4),
                            interpretation = if (p_adj < self$options$sig) "Significant after correction" else "Not significant after correction"
                        )
                        row_index <- row_index + 1
                        
                        export_data[[row_index]] <- list(
                            category = "Pairwise Comparisons",
                            measure = paste(result$comparison, "- Effect Size"),
                            value = round(result$effect_size, 3),
                            interpretation = paste("Phi coefficient =", round(result$effect_size, 3))
                        )
                        row_index <- row_index + 1
                    }
                }
            }
            
            # Populate the export table
            for (i in seq_along(export_data)) {
                self$results$exportTable$addRow(
                    rowKey = i,
                    values = export_data[[i]]
                )
            }
        },
        
        # Main analysis method - Core implementation
        .run = function() {
            # Initial setup and validation
            if (!private$.handleInitialSetup()) return()

            # Prepare and validate data
            data_info <- private$.prepareAndValidateData()
            if (is.null(data_info)) return()  # Error notice already inserted

            data <- data_info$data
            rows <- data_info$rows
            cols <- data_info$cols
            counts <- data_info$counts

            # Create contingency table
            contTable <- private$.createContingencyTable(data, rows, cols, counts)
            if (is.null(contTable)) return()  # Error notice already inserted
            
            # Perform main chi-square test
            chiSqTest <- private$.performChiSquareTest(contTable, rows, cols)
            
            # Validate assumptions and show warnings
            assumptions <- private$.validateAssumptions(contTable, chiSqTest)
            if (self$options$showAssumptionsCheck) {
                private$.displayAssumptionsCheck(assumptions, chiSqTest)
            }

            # Add HTML warnings for critical assumption violations (no Notice objects to avoid serialization errors)
            warning_messages <- c()

            # Small sample size warning
            if (assumptions$total_n < 20) {
                warning_msg <- sprintf('Very small sample size (n=%d). Chi-square approximation may be unreliable. Consider using Fisher\'s exact test and interpreting results with caution.', assumptions$total_n)
                warning_messages <- c(warning_messages, warning_msg)
            }

            # Low expected counts warning
            if (assumptions$prop_low_5 > 0.2) {
                warning_msg <- sprintf('%.0f%% of cells have expected counts < 5. Chi-square test assumptions violated. Use Fisher\'s exact test for more reliable results.', assumptions$prop_low_5 * 100)
                warning_messages <- c(warning_messages, warning_msg)
            }

            # Display combined warnings in todo element if any exist
            if (length(warning_messages) > 0) {
                combined_warnings <- paste0(
                    "<div style='padding: 15px; background-color: #fff3cd; border: 1px solid #ffc107; color: #856404;'>",
                    "<strong>Warning:</strong><ul>",
                    paste0("<li>", warning_messages, "</li>", collapse = ""),
                    "</ul></div>"
                )
                self$results$todo$setContent(combined_warnings)
            }

            # Show educational overview if requested
            if (self$options$showEducational) {
                overview_panel <- private$.createEducationalPanel("overview")
                self$results$educationalOverview$setContent(as.character(overview_panel))
            }
            
            # Handle residuals analysis
            residuals_analysis <- NULL
            if (self$options$showResiduals) {
                cutoff_value <- if (self$options$residualsCutoff != 2.0) self$options$residualsCutoff else NULL
                residuals_analysis <- private$.analyzeResiduals(chiSqTest, contTable, self$options$sig, cutoff_value)
                private$.handleResidualsAnalysis(chiSqTest, contTable, rows, cols)
            }
            
            # Generate and display clinical summary
            if (self$options$showClinicalSummary) {
                pairwise_results <- NULL
                if (chiSqTest$p.value < self$options$sig) {
                    pairwise_results <- private$.robustPairwiseTests(contTable, self$options$posthoc, self$options$testSelection)
                }
                clinical_summary <- private$.generateClinicalSummary(chiSqTest, contTable, pairwise_results, rows, cols)
                private$.displayClinicalSummary(clinical_summary)
            }
            
            # Handle post-hoc testing
            private$.handlePostHocTesting(chiSqTest, contTable, rows, cols)
            
            # Detailed comparison tables are handled within post-hoc testing
            # No separate call needed as it's integrated into .handlePostHocTesting()
            
            # Generate report-ready sentences if requested
            if (self$options$copyReadySentences) {
                assumptions <- private$.validateAssumptions(contTable, chiSqTest)
                clinical_summary <- private$.generateClinicalSummary(chiSqTest, contTable, NULL, rows, cols)
                sentences <- private$.generateReportSentences(chiSqTest, contTable, clinical_summary, assumptions)
                private$.displayReportSentences(sentences)
            }
            
            # Show statistical glossary if requested
            if (self$options$showGlossary) {
                glossary <- private$.createGlossaryPanel()
                self$results$glossaryPanel$setContent(glossary)
            }
            
            # Export results if requested
            if (self$options$exportResults) {
                private$.generateExportTable(chiSqTest, contTable)
            }

            # Note: INFO notice at completion removed to avoid serialization errors
            # The analysis results are already visible in the results tables
        },
        
        # Display clinical summary
        .displayClinicalSummary = function(clinical_summary) {
            summary_style <- if (clinical_summary$significant) {
                "padding: 15px; background-color: #e8f5e8; border-left: 4px solid #4caf50; margin: 10px 0;"
            } else {
                "padding: 15px; background-color: #fff3e0; border-left: 4px solid #ff9800; margin: 10px 0;"
            }
            
            summary_content <- htmltools::div(
                style = summary_style,
                htmltools::h4(.("Clinical Summary"), style = "margin-top: 0; color: #2e7d32;"),
                htmltools::p(clinical_summary$summary_text),
                htmltools::div(
                    style = "margin-top: 10px; font-size: 12px; color: #666;",
                    htmltools::strong(.("Effect Size Interpretation: ")),
                    sprintf(.("The effect size is %s (Cramér's V = %.3f)"), 
                           clinical_summary$effect_interpretation, clinical_summary$effect_size)
                )
            )
            
            self$results$clinicalSummary$setContent(as.character(summary_content))
        },
        
        # Display assumptions check results
        .displayAssumptionsCheck = function(assumptions, chiSqTest) {
            warning_colors <- list(
                none = "#d4edda",
                mild = "#fff3cd", 
                moderate = "#ffeaa7",
                severe = "#f8d7da"
            )
            
            border_colors <- list(
                none = "#c3e6cb",
                mild = "#ffeaa7",
                moderate = "#fdcb6e",
                severe = "#dc3545"
            )
            
            style <- sprintf(
                "padding: 15px; background-color: %s; border-left: 4px solid %s; margin: 10px 0;",
                warning_colors[[assumptions$warning_level]],
                border_colors[[assumptions$warning_level]]
            )
            
            title_text <- if (assumptions$warning_level == "none") {
                .("✓ Assumptions Check: All conditions met")
            } else {
                .("⚠ Assumptions Check: Review required")
            }
            
            assumptions_content <- htmltools::div(
                style = style,
                htmltools::h4(title_text, style = "margin-top: 0;"),
                if (length(assumptions$warning_msg) > 0) {
                    htmltools::div(
                        htmltools::div(htmltools::strong(.("Issues identified:")), style = "margin-bottom: 5px;"),
                        htmltools::tags$ul(
                            style = "margin-top: 0;",
                            lapply(assumptions$warning_msg, function(msg) htmltools::tags$li(msg))
                        )
                    )
                },
                if (length(assumptions$recommendation) > 0) {
                    htmltools::div(
                        htmltools::div(htmltools::strong(.("Recommendations:")), style = "margin-bottom: 5px;"),
                        htmltools::tags$ul(
                            style = "margin-top: 0;",
                            lapply(assumptions$recommendation, function(rec) htmltools::tags$li(rec))
                        )
                    )
                },
                htmltools::div(
                    style = "margin-top: 10px; font-size: 12px;",
                    sprintf(.("Sample size: %d | Expected counts < 5: %d/%d cells | Expected counts < 1: %d cells"),
                           assumptions$total_n, assumptions$low_expected_5, length(chiSqTest$expected), assumptions$low_expected_1)
                )
            )
            
            self$results$assumptionsCheck$setContent(as.character(assumptions_content))
        },

        # Display report sentences
        .displayReportSentences = function(sentences) {
            sentences_content <- htmltools::div(
                style = "padding: 15px; background-color: #f3e5f5; border-left: 4px solid #9c27b0; margin: 10px 0;",
                htmltools::h4(.("Report-Ready Sentences"), style = "color: #7b1fa2; margin-top: 0;"),

                htmltools::div(
                    htmltools::h5(.("Methods:")),
                    htmltools::div(
                        style = "background: white; padding: 10px; border-radius: 4px; margin: 5px 0; border: 1px solid #ddd;",
                        htmltools::p(sentences$methods)
                    )
                ),

                htmltools::div(
                    htmltools::h5(.("Results:")),
                    htmltools::div(
                        style = "background: white; padding: 10px; border-radius: 4px; margin: 5px 0; border: 1px solid #ddd;",
                        htmltools::p(sentences$results)
                    )
                ),

                htmltools::div(
                    htmltools::h5(.("Conclusion:")),
                    htmltools::div(
                        style = "background: white; padding: 10px; border-radius: 4px; margin: 5px 0; border: 1px solid #ddd;",
                        htmltools::p(sentences$conclusion)
                    )
                )
            )

            self$results$reportSentences$setContent(as.character(sentences_content))
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Only draw if requested
            if (!self$options$plot)
                return()

            # Sanity checks
            rows   <- self$options$rows
            cols   <- self$options$cols
            counts <- self$options$counts
            if (is.null(rows) || is.null(cols))
                return(FALSE)

            # Build contingency table using shared logic (no warnings for plots)
            contTable <- try({
                private$.buildContingencyTable(self$data, rows, cols, counts, self$options$excl, show_warnings = FALSE)
            }, silent = TRUE)
            
            if (inherits(contTable, "try-error") || any(dim(contTable) < 2))
                return(FALSE)

            # Chi-square → residuals
            chiSqTest <- try(stats::chisq.test(contTable, correct = FALSE), silent = TRUE)
            if (inherits(chiSqTest, "try-error"))
                return(FALSE)
            resids <- chiSqTest$residuals

            # Melt into a data.frame
            df <- as.data.frame(as.table(resids), responseName = "residual")
            names(df) <- c("Row", "Col", "Residual")
            df$Row <- factor(df$Row, levels = rev(rownames(resids)))
            df$Col <- factor(df$Col, levels = colnames(resids))




            # Set color palette for heatmap
            low_color <- "blue"
            high_color <- "red"

            # Build ggplot
            p <- ggplot2::ggplot(df, ggplot2::aes(x = Col, y = Row, fill = Residual)) +
                ggplot2::geom_tile(color = "grey80") +
                ggplot2::geom_text(ggplot2::aes(label = round(Residual, 2)), size = 3, color = "black") +
                ggplot2::scale_fill_gradient2(
                    low    = low_color,
                    mid    = "white", 
                    high   = high_color,
                    midpoint = 0,
                    name     = "Std. Resid.",
                    guide    = ggplot2::guide_colorbar()
                ) +
                ggplot2::labs(
                    title = sprintf("Standardized Residuals: %s vs %s", rows, cols),
                    x     = cols,
                    y     = rows
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                    panel.grid  = ggplot2::element_blank(),
                    plot.title  = ggplot2::element_text(hjust = 0.5)
                )

            # Print to jamovi's graphics device
            print(p)
            TRUE
        }






    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for chisqposttest analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            rows <- self$options$rows
            cols <- self$options$cols

            if (is.null(rows) || is.null(cols))
                return('')

            # Escape rows variable
            rows_escaped <- if (!is.null(rows) && !identical(make.names(rows), rows)) {
                paste0('`', rows, '`')
            } else {
                rows
            }

            # Escape cols variable
            cols_escaped <- if (!is.null(cols) && !identical(make.names(cols), cols)) {
                paste0('`', cols, '`')
            } else {
                cols
            }

            # Build arguments
            rows_arg <- paste0('rows = "', rows_escaped, '"')
            cols_arg <- paste0('cols = "', cols_escaped, '"')

            # Get other arguments using base helper (if available)
            args <- ''
            if (!is.null(private$.asArgs)) {
                args <- private$.asArgs(incData = FALSE)
            }
            if (args != '')
                args <- paste0(',\n    ', args)

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, '::chisqposttest(\n    data = data,\n    ',
                   rows_arg, ',\n    ', cols_arg, args, ')')
        }
    ) # End of public list
)
