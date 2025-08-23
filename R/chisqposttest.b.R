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
        
        # Robust pairwise chi-square testing with progress indicators and memory optimization
        .robustPairwiseTests = function(contingency_table, method = "bonferroni", test_selection = "auto") {
            # Check if we need memory optimization for large tables
            row_names <- rownames(contingency_table)
            total_comparisons <- choose(length(row_names), 2)
            
            # Use chunked processing for very large numbers of comparisons
            if (total_comparisons > 100) {
                return(private$.robustPairwiseTestsChunked(contingency_table, method, test_selection))
            }
            
            # Standard processing for smaller datasets
            # Convert table to vector format for chisq.multcomp
            row_names <- rownames(contingency_table)
            col_names <- colnames(contingency_table)
            
            # Calculate total number of possible comparisons for progress tracking
            total_comparisons <- choose(length(row_names), 2)
            
            # Create all pairwise combinations for rows
            pairwise_results <- list()
            comparison_index <- 1
            completed_comparisons <- 0
            
            if (length(row_names) >= 2) {
                # Initialize progress for long-running analyses
                if (total_comparisons > 10) {
                    private$.checkpoint(paste("Starting", total_comparisons, "pairwise comparisons..."))
                }
                
                # Row-wise pairwise comparisons
                for (i in 1:(length(row_names) - 1)) {
                    for (j in (i + 1):length(row_names)) {
                        completed_comparisons <- completed_comparisons + 1
                        
                        # Update progress for long analyses
                        if (total_comparisons > 10 && completed_comparisons %% 5 == 0) {
                            progress_pct <- round((completed_comparisons / total_comparisons) * 100)
                            private$.checkpoint(paste("Progress:", progress_pct, "% -", completed_comparisons, "of", total_comparisons, "comparisons completed"))
                        }
                        
                        # Extract 2x2 subtable
                        subtable <- contingency_table[c(i, j), , drop = FALSE]
                        
                        # Skip if insufficient data
                        if (any(dim(subtable) < 2) || sum(subtable) < 5) next
                        
                        # Perform statistical tests based on user selection
                        test_result <- try({
                            # Always compute chi-square for effect size
                            chi_test <- stats::chisq.test(subtable, correct = FALSE)
                            
                            # Determine which tests to run based on test_selection
                            fisher_test <- NULL
                            if (test_selection == "fisher" || 
                                (test_selection == "auto" && any(subtable < 5))) {
                                fisher_test <- stats::fisher.test(subtable)
                            }
                            
                            # Store both test results if available
                            result <- list(
                                comparison = paste(row_names[i], "vs", row_names[j]),
                                type = "row_comparison",
                                subtable = subtable,
                                chi_statistic = chi_test$statistic,
                                chi_df = chi_test$parameter,
                                chi_pvalue = chi_test$p.value,
                                sample_size = sum(subtable),
                                effect_size = sqrt(chi_test$statistic / sum(subtable)),
                                test_selection = test_selection
                            )
                            
                            # Add Fisher's test results if computed
                            if (!is.null(fisher_test)) {
                                result$fisher_pvalue <- fisher_test$p.value
                            } else {
                                result$fisher_pvalue <- NA
                            }
                            
                            result
                        }, silent = TRUE)
                        
                        if (!inherits(test_result, "try-error")) {
                            pairwise_results[[comparison_index]] <- test_result
                            comparison_index <- comparison_index + 1
                        }
                    }
                }
                
                # Final progress update for long analyses
                if (total_comparisons > 10) {
                    private$.checkpoint(paste("Completed all", total_comparisons, "pairwise comparisons. Applying", method, "correction..."))
                }
            }
            
            # Apply p-value adjustments
            if (length(pairwise_results) > 0) {
                chi_pvalues <- sapply(pairwise_results, function(x) x$chi_pvalue)
                fisher_pvalues <- sapply(pairwise_results, function(x) x$fisher_pvalue)
                
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
                    pairwise_results[[i]]$adjustment_method <- method
                }
            }
            
            return(pairwise_results)
        },
        
        # Memory-optimized chunked processing for large contingency tables
        .robustPairwiseTestsChunked = function(contingency_table, method = "bonferroni", test_selection = "auto", chunk_size = 25) {
            row_names <- rownames(contingency_table)
            col_names <- colnames(contingency_table)
            total_comparisons <- choose(length(row_names), 2)
            
            # Initialize variables
            all_pairwise_results <- list()
            completed_comparisons <- 0
            overall_index <- 1
            
            private$.checkpoint(paste("Processing", total_comparisons, "comparisons in chunks of", chunk_size, "for memory efficiency..."))
            
            # Create comparison pairs
            comparison_pairs <- list()
            pair_index <- 1
            for (i in 1:(length(row_names) - 1)) {
                for (j in (i + 1):length(row_names)) {
                    comparison_pairs[[pair_index]] <- c(i, j)
                    pair_index <- pair_index + 1
                }
            }
            
            # Process in chunks
            num_chunks <- ceiling(length(comparison_pairs) / chunk_size)
            
            for (chunk_idx in 1:num_chunks) {
                start_idx <- (chunk_idx - 1) * chunk_size + 1
                end_idx <- min(chunk_idx * chunk_size, length(comparison_pairs))
                chunk_pairs <- comparison_pairs[start_idx:end_idx]
                
                private$.checkpoint(paste("Processing chunk", chunk_idx, "of", num_chunks, "- comparisons", start_idx, "to", end_idx))
                
                # Process current chunk
                chunk_results <- list()
                chunk_result_idx <- 1
                
                for (pair_idx in seq_along(chunk_pairs)) {
                    i <- chunk_pairs[[pair_idx]][1]
                    j <- chunk_pairs[[pair_idx]][2]
                    completed_comparisons <- completed_comparisons + 1
                    
                    # Extract 2x2 subtable
                    subtable <- contingency_table[c(i, j), , drop = FALSE]
                    
                    # Skip if insufficient data
                    if (any(dim(subtable) < 2) || sum(subtable) < 5) next
                    
                    # Perform statistical tests
                    test_result <- try({
                        # Always compute chi-square for effect size
                        chi_test <- stats::chisq.test(subtable, correct = FALSE)
                        
                        # Determine which tests to run based on test_selection
                        fisher_test <- NULL
                        if (test_selection == "fisher" || 
                            (test_selection == "auto" && any(subtable < 5))) {
                            fisher_test <- stats::fisher.test(subtable)
                        }
                        
                        # Store both test results if available
                        result <- list(
                            comparison = paste(row_names[i], "vs", row_names[j]),
                            type = "row_comparison",
                            subtable = subtable,
                            chi_statistic = chi_test$statistic,
                            chi_df = chi_test$parameter,
                            chi_pvalue = chi_test$p.value,
                            sample_size = sum(subtable),
                            effect_size = sqrt(chi_test$statistic / sum(subtable)),
                            test_selection = test_selection
                        )
                        
                        # Add Fisher's test results if computed
                        if (!is.null(fisher_test)) {
                            result$fisher_pvalue <- fisher_test$p.value
                        } else {
                            result$fisher_pvalue <- NA
                        }
                        
                        result
                    }, silent = TRUE)
                    
                    if (!inherits(test_result, "try-error")) {
                        chunk_results[[chunk_result_idx]] <- test_result
                        chunk_result_idx <- chunk_result_idx + 1
                    }
                    
                    # Periodic checkpoint within chunk
                    if (completed_comparisons %% 10 == 0) {
                        progress_pct <- round((completed_comparisons / total_comparisons) * 100)
                        private$.checkpoint(paste("Progress:", progress_pct, "% - processed", completed_comparisons, "comparisons"))
                        
                        # Force garbage collection for memory management
                        if (completed_comparisons %% 50 == 0) {
                            gc(verbose = FALSE)
                        }
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
                
                private$.checkpoint(paste("Completed chunk", chunk_idx, "- total valid comparisons so far:", length(all_pairwise_results)))
            }
            
            # Apply p-value adjustments to all results
            if (length(all_pairwise_results) > 0) {
                private$.checkpoint(paste("Applying", method, "correction to", length(all_pairwise_results), "valid comparisons..."))
                
                chi_pvalues <- sapply(all_pairwise_results, function(x) x$chi_pvalue)
                fisher_pvalues <- sapply(all_pairwise_results, function(x) x$fisher_pvalue)
                
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
                    all_pairwise_results[[i]]$adjustment_method <- method
                }
                
                private$.checkpoint(paste("Completed chunked processing:", length(all_pairwise_results), "comparisons with", method, "correction applied"))
            }
            
            return(all_pairwise_results)
        },
        
        # Create detailed HTML table for individual pairwise comparisons
        .createDetailedComparisonHTML = function(pairwise_results, row_var_name = NULL, col_var_name = NULL) {
            if (length(pairwise_results) == 0) return("")
            
            comparison_divs <- lapply(seq_along(pairwise_results), function(i) {
                result <- pairwise_results[[i]]
                subtable <- result$subtable
                
                # Determine test method and significance
                use_fisher <- any(subtable < 5)
                p_adj <- if (use_fisher) result$fisher_pvalue_adjusted else result$chi_pvalue_adjusted
                is_significant <- p_adj < 0.05
                
                # Create significance indicator
                sig_indicator <- if (is_significant) {
                    htmltools::span(style = "color: #d32f2f; font-weight: bold;", "✓ Significant")
                } else {
                    htmltools::span(style = "color: #666;", "Not significant")
                }
                
                # Test results summary
                test_method <- if (use_fisher) "Fisher's exact test" else "Chi-square test"
                p_value <- if (use_fisher) result$fisher_pvalue else result$chi_pvalue
                
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
            
            main_content <- htmltools::div(
                style = "margin: 20px 0;",
                htmltools::h3(style = "color: #1976d2;", "Detailed Pairwise Comparison Tables"),
                comparison_divs
            )
            
            return(as.character(main_content))
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
        
        # Create educational HTML panels
        .createEducationalPanel = function(type = "overview", num_comparisons = NULL, alpha = 0.05) {
            if (type == "overview") {
                return(htmltools::div(
                    style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #1976d2; margin: 10px 0;",
                    htmltools::h4("Chi-Square Post-Hoc Analysis Guide", style = "color: #1976d2; margin-top: 0;"),
                    htmltools::p(htmltools::strong("Three-Step Comprehensive Analysis:")),
                    htmltools::div(
                        "1. ", htmltools::strong("Overall Chi-Square Test:"), " Tests if there's any association between variables", htmltools::br(),
                        "2. ", htmltools::strong("Residuals Analysis:"), " Identifies which specific cells contribute to significance", htmltools::br(),
                        "3. ", htmltools::strong("Pairwise Comparisons:"), " Formal hypothesis testing between group pairs"
                    ),
                    htmltools::p(htmltools::em("Recommended approach: Start with residuals analysis for pattern identification, then use pairwise tests for formal hypothesis testing."))
                ))
            } else if (type == "residuals") {
                return(htmltools::div(
                    style = "padding: 15px; background-color: #e8f5e8; border-left: 4px solid #4caf50; margin: 10px 0;",
                    htmltools::h4("Standardized Residuals Interpretation", style = "color: #2e7d32; margin-top: 0;"),
                    htmltools::p(htmltools::strong("What are standardized residuals?")),
                    htmltools::p("Standardized residuals measure how much each cell deviates from what we'd expect if there was no association. They're calculated as (Observed - Expected) / √Expected, then adjusted for multiple comparisons."),
                    htmltools::div(
                        style = "background-color: #f1f8e9; padding: 10px; border-radius: 3px;",
                        htmltools::strong("Interpretation Guidelines:"), htmltools::br(),
                        "• |Residual| > 2.0: Suggests meaningful deviation", htmltools::br(),
                        "• |Residual| > 3.0: Strong evidence of deviation", htmltools::br(),
                        "• Positive values: Over-represented (more than expected)", htmltools::br(),
                        "• Negative values: Under-represented (fewer than expected)"
                    )
                ))
            } else if (type == "multiple_testing" && !is.null(num_comparisons)) {
                adjusted_alpha <- alpha / num_comparisons
                return(htmltools::div(
                    style = "padding: 15px; background-color: #fff3e0; border-left: 4px solid #ff9800; margin: 10px 0;",
                    htmltools::h4("Multiple Testing Correction", style = "color: #e65100; margin-top: 0;"),
                    htmltools::p(htmltools::strong("Why correction is needed:")),
                    htmltools::p("When performing multiple tests, the probability of finding at least one false positive increases. With ", num_comparisons, " comparisons, there's a ", round((1 - (1 - alpha)^num_comparisons) * 100, 1), "% chance of a false positive without correction."),
                    htmltools::div(
                        style = "background-color: #fce4ec; padding: 10px; border-radius: 3px;",
                        htmltools::strong("Correction Applied:"), htmltools::br(),
                        "• Number of comparisons: ", num_comparisons, htmltools::br(),
                        "• Adjusted significance level: α = ", round(adjusted_alpha, 4), htmltools::br(),
                        "• Use adjusted p-values for significance decisions"
                    )
                ))
            }
        },
        
        # Helper method to handle initial setup and validation
        .handleInitialSetup = function() {
            # ToDo Message ----
            if (is.null(self$options$rows) || is.null(self$options$cols)) {
                todo <- "
                <br>Welcome to ClinicoPath Chi-Square Post-Hoc Tests
                <br><br>
                This tool performs a Chi-Square test followed by pairwise post-hoc tests for all combinations of category levels when the overall Chi-Square test is significant.
                <br><br>
                <strong>Data Input Options:</strong>
                <br>• <strong>Individual observations:</strong> Select row and column variables from raw data
                <br>• <strong>Frequency counts:</strong> Select row and column variables plus a counts variable for aggregated data
                <br><br>
                The post-hoc tests help identify which specific group combinations contribute to the significant overall effect.
                <hr><br>
                "
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
            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")

            # Prepare Data ----
            data <- self$data
            rows <- self$options$rows
            cols <- self$options$cols
            counts <- self$options$counts

            # Check if counts variable exists when specified
            if (!is.null(counts) && !(counts %in% names(data))) {
                stop(paste("The counts variable '", counts, "' does not exist in the data. Please select a valid numeric variable for counts.", sep = ""))
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
                stop("Error creating contingency table. Please check your data and variable selections.")
            }
            
            # Validate table dimensions
            if (any(dim(contTable) < 2)) {
                stop("Contingency table must have at least 2 rows and 2 columns. Please check your data.")
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
        
        # Helper method to handle post-hoc testing
        .handlePostHocTesting = function(chiSqTest, contTable, rows = NULL, cols = NULL) {
            overall_significant <- chiSqTest$p.value < self$options$sig
            
            if (!overall_significant) {
                # Chi-square not significant - show note but continue with detailed tables if requested
                self$results$multipleTestingInfo$setContent(
                    "<div style='padding: 15px; background-color: #d4edda; border: 1px solid #c3e6cb;'><strong>Note:</strong> Overall chi-square test is not significant (p ≥ 0.05). Post-hoc pairwise comparisons are not recommended when the overall test is non-significant, but detailed tables are shown below for educational purposes.</div>")
            }
            
            adjustMethod <- self$options$posthoc
            
            # Add checkpoint before potentially long operation
            private$.checkpoint()
            
            # Use robust pairwise testing approach with user-selected test method
            pairwise_results <- private$.robustPairwiseTests(contTable, adjustMethod, self$options$testSelection)
            
            if (length(pairwise_results) > 0) {
                # Create multiple testing correction panel (conditional)
                if (self$options$showEducational) {
                    correction_panel <- private$.createEducationalPanel("multiple_testing", 
                                                                        length(pairwise_results), 
                                                                        self$options$sig)
                    self$results$multipleTestingInfo$setContent(as.character(correction_panel))
                }
                
                # Populate pairwise results table
                for (i in seq_along(pairwise_results)) {
                    result <- pairwise_results[[i]]
                    
                    # Determine which test method to use based on user selection and data
                    if (self$options$testSelection == "fisher") {
                        use_fisher <- TRUE
                        test_used <- "Fisher's exact"
                    } else if (self$options$testSelection == "chisquare") {
                        use_fisher <- FALSE
                        test_used <- "Chi-square"
                    } else { # auto selection
                        use_fisher <- any(result$subtable < 5) || is.na(result$fisher_pvalue) == FALSE
                        test_used <- if (use_fisher) "Fisher's exact" else "Chi-square"
                    }
                    
                    # Use appropriate p-values
                    primary_p_adj <- result$chi_pvalue_adjusted
                    fisher_p_adj <- result$fisher_pvalue_adjusted
                    final_p_adj <- if (use_fisher && !is.na(fisher_p_adj)) fisher_p_adj else primary_p_adj
                    
                    self$results$posthocTable$addRow(
                        rowKey = i,
                        values = list(
                            comparison = result$comparison,
                            test_method = test_used,
                            chi = result$chi_statistic,
                            p = if (use_fisher) result$fisher_pvalue else result$chi_pvalue,
                            padj = final_p_adj,
                            effect_size = round(result$effect_size, 3),
                            sig = ifelse(final_p_adj < self$options$sig, "Yes", "No")
                        )
                    )
                    
                    # Add checkpoint for each comparison
                    private$.checkpoint(flush = FALSE)
                }
                
                # Create detailed comparison tables (conditional)
                if (self$options$showDetailedTables) {
                    detailed_html <- private$.createDetailedComparisonHTML(pairwise_results, rows, cols)
                    self$results$detailedComparisons$setContent(detailed_html)
                }
            } else {
                # No valid pairwise comparisons possible
                self$results$multipleTestingInfo$setContent(
                    "<div style='padding: 15px; background-color: #fff3cd; border: 1px solid #ffeaa7;'>No valid pairwise comparisons could be performed. This may be due to insufficient data in subtables or table structure.</div>")
            }
        },
        
        .run = function() {
            # Handle initial setup and validation
            if (!private$.handleInitialSetup()) {
                return() # Exit early if setup incomplete
            }
            
            # Prepare and validate data
            data_info <- private$.prepareAndValidateData()
            
            # Create contingency table
            contTable <- private$.createContingencyTable(data_info$data, data_info$rows, data_info$cols, data_info$counts)
            
            # Perform chi-square test and populate results
            chiSqTest <- private$.performChiSquareTest(contTable, data_info$rows, data_info$cols)
            
            # Handle residuals analysis
            private$.handleResidualsAnalysis(chiSqTest, contTable, data_info$rows, data_info$cols)
            
            # Create educational overview panel (conditional)
            if (self$options$showEducational) {
                overview_panel <- private$.createEducationalPanel("overview")
                self$results$educationalOverview$setContent(as.character(overview_panel))
            }
            
            # Handle post-hoc testing
            private$.handlePostHocTesting(chiSqTest, contTable, data_info$rows, data_info$cols)
            
            # Handle export functionality
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
                        
                        # Determine which test to use
                        use_fisher <- self$options$testSelection == "fisher" || 
                                     (self$options$testSelection == "auto" && any(result$subtable < 5))
                        
                        test_name <- if (use_fisher) "Fisher's Exact" else "Chi-square"
                        p_value <- if (use_fisher) result$fisher_pvalue else result$chi_pvalue
                        p_adj <- if (use_fisher) result$fisher_pvalue_adjusted else result$chi_pvalue_adjusted
                        
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




            # Build ggplot
            p <- ggplot2::ggplot(df, ggplot2::aes(x = Col, y = Row, fill = Residual)) +
                ggplot2::geom_tile(color = "grey80") +
                ggplot2::geom_text(ggplot2::aes(label = round(Residual, 2)), size = 3, color = "black") +
                ggplot2::scale_fill_gradient2(
                    low    = "blue",
                    mid    = "white", 
                    high   = "red",
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






    )
)
