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
#' @importFrom stats chisq.test p.adjust fisher.test qnorm complete.cases xtabs quantile
#' @importFrom grid gpar
#' @importFrom grDevices hcl.colors
#' @importFrom htmltools HTML div h3 h4 h5 p strong em br
#'
#' @return An \code{R6} class generator object for the \code{chisqposttestClass} backend; used internally by the jamovi analysis wrapper and not called directly.

chisqposttestClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "chisqposttestClass",
    inherit = chisqposttestBase,
    private = list(

        # Notice collection helpers. A single Preformatted (plain-text) output item:
        # avoids BOTH the jmvcore::Notice serialization error from
        # self$results$insert(999, Notice) AND any HTML in notices (project convention:
        # notice content must be plain text). ====
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            duplicate <- vapply(private$.noticeList, function(notice) {
                identical(notice$type, type) &&
                    identical(notice$title, title) &&
                    identical(notice$content, content)
            }, logical(1))
            if (any(duplicate))
                return()

            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type,
                title = title,
                content = content
            )
            # Render immediately so early-return validation aborts still display the notice
            private$.renderNotices()
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                return()
            }

            blocks <- vapply(private$.noticeList, function(notice) {
                prefix <- switch(notice$type,
                    ERROR          = "ERROR: ",
                    STRONG_WARNING = "WARNING: ",
                    WARNING        = "WARNING: ",
                    "")
                paste0(prefix, notice$title, "\n", notice$content)
            }, character(1))

            self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
        },

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
            
            # Adjusted standardized residuals (accounting for variance structure).
            # This is the quantity used everywhere in this analysis - the table, the
            # plot and the export all read it - so the "> 2" rule of thumb quoted in
            # the guidance panel applies to the number the user actually sees.
            adjusted_residuals <- chi_result$stdres

            # Calculate critical value using configurable approach
            critical_z <- private$.calculateResidualsCriticalValue(contingency_table, alpha, cutoff)

            # Determine significance and the |z| band the cell falls in
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
                
                # Residual type actually reported
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
        
        # Band a cell by the size of its adjusted standardized residual.
        #
        # These are deliberately NOT effect-size labels ("Small"/"Large"). An
        # adjusted standardized residual is a z-type test statistic: its magnitude
        # grows with sqrt(n), so the same departure from independence gives |z| = 1.2
        # at n = 100 and |z| = 3.8 at n = 1000. Labelling it "Large" would tell the
        # reader a big study found a big effect when it found a well-powered small
        # one. The bands therefore name the statistic, not an effect size.
        .classifyResidualMagnitude = function(residuals) {
            abs_residuals <- abs(residuals)
            magnitude_matrix <- matrix(
                ifelse(abs_residuals < 1.0, "|z| < 1",
                       ifelse(abs_residuals < 2.0, "|z| 1-2",
                              ifelse(abs_residuals < 3.0, "|z| 2-3",
                                     ifelse(abs_residuals < 4.0, "|z| 3-4", "|z| >= 4")))),
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
            
            for (i in seq_len(nrow(residuals))) {
                for (j in seq_len(ncol(residuals))) {
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

        # chisq.test() emits "Chi-squared approximation may be incorrect" whenever
        # an expected count is small. The analysis already reports that condition
        # properly in its Assumptions panel - with the exact number of cells below
        # 5 and below 1, and the recommendation to use Fisher's exact test - so the
        # raw warning is redundant: a jamovi GUI user never sees it, and an R-API
        # caller just gets noise on stderr.
        #
        # Only THAT message is muffled; any other warning still propagates, so a
        # genuinely unexpected condition is not hidden.
        .chisqQuiet = function(...) {
            withCallingHandlers(
                stats::chisq.test(...),
                warning = function(w) {
                    if (grepl("Chi-squared approximation", conditionMessage(w), fixed = TRUE))
                        invokeRestart("muffleWarning")
                })
        },

        # Number of pairwise sub-comparisons the post-hoc family actually contains.
        #
        # A dimension with exactly TWO levels contributes nothing. Its single "pair"
        # is rows/columns 1 and 2 crossed with the whole of the other variable, i.e.
        # the entire table - so that comparison IS the omnibus chi-square already
        # printed above the post-hoc table, not a sub-comparison of it. Counting it
        # made the family one test too large and inflated every genuine adjusted
        # p-value (a 3x2 table divided by 4 instead of 3), and on a 2x2 table it
        # produced two copies of the omnibus test whose Bonferroni-doubled p-value
        # could read "Significant: No" for the very test reported as significant in
        # the pane above.
        # Fisher's exact test for one pairwise sub-table, with a disclosed fallback.
        #
        # fisher.test() on a 2xC sub-table fails for a RESOURCE reason (the FEXACT
        # workspace) as readily as for a data reason. Dropping straight back to
        # chi-square handed the user the very p-value the auto-selection had just
        # judged untrustworthy, with nothing on screen to say the exact test had been
        # attempted and abandoned. Try the Monte Carlo exact test first, and if that
        # fails too, say which comparison fell back.
        .fisherPvalue = function(subtable, comparison_label) {
            fisher_test <- try(stats::fisher.test(subtable), silent = TRUE)
            if (!inherits(fisher_test, "try-error"))
                return(list(p = fisher_test$p.value, method = "Fisher's exact"))

            private$.checkpoint()
            fisher_test <- try(
                stats::fisher.test(subtable, simulate.p.value = TRUE, B = 2000),
                silent = TRUE)
            if (!inherits(fisher_test, "try-error"))
                return(list(p = fisher_test$p.value, method = "Fisher's exact (Monte Carlo)"))

            private$.addNotice(
                "STRONG_WARNING",
                .("An exact test could not be computed for one comparison"),
                sprintf(
                    .("Fisher's exact test was selected for the comparison %s because at least one of its expected counts is below 5, but it could not be computed, and the Monte Carlo version of it failed as well. The chi-square p-value is reported for that comparison instead. It relies on a large-sample approximation that the low expected counts undermine, so read it with that in mind."),
                    comparison_label))
            return(NULL)
        },

        .pairwiseComparisonCount = function(contingency_table) {
            dimensions <- dim(contingency_table)
            sum(vapply(dimensions, function(n) {
                if (n < 3) 0 else choose(n, 2)
            }, numeric(1)))
        },
        
        # Robust pairwise chi-square testing with optimized resource management
        .robustPairwiseTests = function(contingency_table, method = "bonferroni", test_selection = "auto") {
            # Check if we need memory optimization for large tables
            total_comparisons <- private$.pairwiseComparisonCount(contingency_table)
            
            # Guard against pathological tables (e.g. continuous variables with thousands of combinations)
            if (total_comparisons > 500) {
                private$.addNotice(
                    "WARNING",
                    .("Pairwise comparisons disabled for high-cardinality table"),
                    sprintf(.("The contingency table generates %d pairwise comparisons, exceeding the safety threshold of 500. Pairwise comparisons were disabled to avoid computational freezing and extreme multiple-testing penalties. Please bin continuous variables or group factor levels."), total_comparisons)
                )
                return(list())
            }

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
            
            # Row-wise pairwise comparisons. >= 3, not >= 2: see
            # .pairwiseComparisonCount() for why a two-level dimension is skipped.
            if (length(row_names) >= 3) {
                for (i in 1:(length(row_names) - 1)) {
                    for (j in (i + 1):length(row_names)) {
                        # Yield to jamovi: with bootstrap CIs on, one comparison is
                        # 999 resampled tables, so a run must stay interruptible.
                        private$.checkpoint()

                        # Extract 2x2 subtable
                        subtable <- contingency_table[c(i, j), , drop = FALSE]
                        
                        # Skip if insufficient data
                        if (any(dim(subtable) < 2) || sum(subtable) == 0) next

                        # A pair can leave a category of the OTHER variable with no
                        # observations at all (comparing grades 1 and 2 when only
                        # grade 3 ever carries a given marker). That zero margin makes
                        # every expected count in the category zero, so chisq.test
                        # returns a NaN statistic and a NaN p-value; the NaN survived
                        # p.adjust() and then aborted the whole analysis at
                        # `if (p_adj < sig)` with "missing value where TRUE/FALSE
                        # needed". Drop the unobserved categories - the comparison is
                        # then exactly the test on the categories that were seen.
                        subtable <- subtable[rowSums(subtable) > 0, colSums(subtable) > 0, drop = FALSE]
                        if (any(dim(subtable) < 2)) next
                        
                        # Perform statistical tests based on user selection
                        test_result <- try({
                            # Always compute chi-square for effect size
                            chi_test <- private$.chisqQuiet(subtable, correct = FALSE)
                            expected_counts <- chi_test$expected
                            comparison_label <- paste(row_names[i], "vs", row_names[j])
                            
                            # Determine which tests to run based on test_selection
                            fisher_res <- NULL
                            use_fisher <- test_selection == "fisher" || 
                                          (test_selection == "auto" && any(expected_counts < 5))
                            if (use_fisher)
                                fisher_res <- private$.fisherPvalue(subtable, comparison_label)
                            
                            # Store both test results if available
                            result <- list(
                                comparison = comparison_label,
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
                            if (!is.null(fisher_res)) {
                                result$fisher_pvalue <- fisher_res$p
                                result$test_used <- fisher_res$method
                                result$actual_pvalue <- fisher_res$p
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
            
            # Column-wise pairwise comparisons (same >= 3 rule as the rows above)
            if (length(col_names) >= 3) {
                for (i in 1:(length(col_names) - 1)) {
                    for (j in (i + 1):length(col_names)) {
                        private$.checkpoint()

                        # Extract 2x2 subtable (transpose for column comparisons)
                        subtable <- contingency_table[, c(i, j), drop = FALSE]
                        
                        # Skip if insufficient data
                        if (any(dim(subtable) < 2) || sum(subtable) == 0) next

                        # A pair can leave a category of the OTHER variable with no
                        # observations at all (comparing grades 1 and 2 when only
                        # grade 3 ever carries a given marker). That zero margin makes
                        # every expected count in the category zero, so chisq.test
                        # returns a NaN statistic and a NaN p-value; the NaN survived
                        # p.adjust() and then aborted the whole analysis at
                        # `if (p_adj < sig)` with "missing value where TRUE/FALSE
                        # needed". Drop the unobserved categories - the comparison is
                        # then exactly the test on the categories that were seen.
                        subtable <- subtable[rowSums(subtable) > 0, colSums(subtable) > 0, drop = FALSE]
                        if (any(dim(subtable) < 2)) next
                        
                        # Perform statistical tests based on user selection
                        test_result <- try({
                            # Always compute chi-square for effect size
                            chi_test <- private$.chisqQuiet(subtable, correct = FALSE)
                            expected_counts <- chi_test$expected
                            comparison_label <- paste(col_names[i], "vs", col_names[j])
                            
                            # Determine which tests to run based on test_selection
                            fisher_res <- NULL
                            use_fisher <- test_selection == "fisher" || 
                                          (test_selection == "auto" && any(expected_counts < 5))
                            if (use_fisher)
                                fisher_res <- private$.fisherPvalue(subtable, comparison_label)
                            
                            # Store both test results if available
                            result <- list(
                                comparison = comparison_label,
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
                            if (!is.null(fisher_res)) {
                                result$fisher_pvalue <- fisher_res$p
                                result$test_used <- fisher_res$method
                                result$actual_pvalue <- fisher_res$p
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
            total_comparisons <- private$.pairwiseComparisonCount(contingency_table)
            
            # Initialize variables
            all_pairwise_results <- list()
            completed_comparisons <- 0
            overall_index <- 1
            
            # Processing comparisons in chunks for memory efficiency
            
            # Create comparison pairs (both row and column comparisons)
            comparison_pairs <- list()
            pair_index <- 1
            
            # Row-wise comparison pairs. The >= 3 guard must match the unchunked
            # path and .pairwiseComparisonCount() exactly, or the two routes would
            # adjust over different numbers of comparisons.
            if (length(row_names) >= 3) {
                for (i in 1:(length(row_names) - 1)) {
                    for (j in (i + 1):length(row_names)) {
                        comparison_pairs[[pair_index]] <- list(indices = c(i, j), type = "row")
                        pair_index <- pair_index + 1
                    }
                }
            }

            # Column-wise comparison pairs
            if (length(col_names) >= 3) {
                for (i in 1:(length(col_names) - 1)) {
                    for (j in (i + 1):length(col_names)) {
                        comparison_pairs[[pair_index]] <- list(indices = c(i, j), type = "column")
                        pair_index <- pair_index + 1
                    }
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
                    private$.checkpoint()

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

                    # Drop categories with no observations in this pair - see the
                    # note in .robustPairwiseTests() for why the NaN they produce is
                    # fatal rather than merely untidy.
                    subtable <- subtable[rowSums(subtable) > 0, colSums(subtable) > 0, drop = FALSE]
                    if (any(dim(subtable) < 2)) next
                    
                    # Perform statistical tests
                    test_result <- try({
                        # Always compute chi-square for effect size
                        chi_test <- private$.chisqQuiet(subtable, correct = FALSE)
                        expected_counts <- chi_test$expected
                        comparison_label <- paste(comp_names[i], "vs", comp_names[j])
                        
                        # Determine which tests to run based on test_selection
                        fisher_res <- NULL
                        use_fisher <- test_selection == "fisher" || 
                                      (test_selection == "auto" && any(expected_counts < 5))
                        if (use_fisher)
                            fisher_res <- private$.fisherPvalue(subtable, comparison_label)
                        
                        # Store both test results if available
                        result <- list(
                            comparison = comparison_label,
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
                        if (!is.null(fisher_res)) {
                            result$fisher_pvalue <- fisher_res$p
                            result$test_used <- fisher_res$method
                            result$actual_pvalue <- fisher_res$p
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
                # startsWith, not identical: the Monte Carlo fallback labels itself
                # "Fisher's exact (Monte Carlo)" and is still an exact-test p-value.
                use_fisher <- startsWith(result$test_used, "Fisher")
                p_adj <- result$actual_pvalue_adjusted
                is_significant <- p_adj < self$options$sig
                
                # Create significance indicator
                sig_indicator <- if (is_significant) {
                    htmltools::span(style = "color: #d32f2f; font-weight: bold;", " Significant")
                } else {
                    htmltools::span(style = "color: inherit; opacity: 0.75;", "Not significant")
                }
                
                # Test results summary
                test_method <- if (use_fisher) paste0(result$test_used, " test") else "Chi-square test"
                p_value <- result$actual_pvalue
                
                test_results <- htmltools::div(
                    style = "margin-bottom: 15px; padding: 10px; background-color: rgba(88, 88, 88, 0.06); border-radius: 3px; color: inherit;",
                    htmltools::strong("Test Results:"), htmltools::br(),
                    "Method: ", test_method, " (", 
                    if (use_fisher) "used due to low expected frequencies" else "standard approach", 
                    ")", htmltools::br(),
                    "Chi-square = ", round(result$chi_statistic, 3), ", ",
                    "p = ", format.pval(p_value, digits = 3), ", ",
                    "Adjusted p = ", format.pval(p_adj, digits = 3), htmltools::br(),
                    "Effect size (Phi/V) = ", round(result$effect_size, 3), htmltools::br(),
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
                    style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center; background-color: rgba(138, 155, 172, 0.06); color: inherit;", 
                    htmltools::div(
                        style = "font-weight: bold; color: inherit;",
                        if (!is.null(col_var_name)) paste0(col_var_name, " \u{2192}") else "",
                        htmltools::br(),
                        if (!is.null(row_var_name)) paste0(row_var_name, " \u{2193}") else ""
                    )
                )
            )
            
            # Add column headers with variable name context
            for (col in col_names) {
                header_cells[[length(header_cells) + 1]] <- htmltools::tags$th(
                    style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center; background-color: rgba(33, 152, 239, 0.13); color: inherit;", 
                    htmltools::div(
                        style = "font-weight: bold;",
                        if (!is.null(col_var_name)) {
                            list(
                                htmltools::div(style = "font-size: 11px; color: inherit; opacity: 0.75; margin-bottom: 2px;", col_var_name),
                                htmltools::div(style = "font-size: 13px; color: inherit;", col)
                            )
                        } else {
                            col
                        }
                    )
                )
            }
            header_row <- htmltools::tags$tr(style = "background-color: rgba(33, 152, 239, 0.13); color: inherit;", header_cells)
            
            # Create data rows
            data_rows <- lapply(seq_along(row_names), function(i) {
                row_cells <- list(
                    htmltools::tags$th(
                        style = "border: 1px solid #e1e5e9; padding: 8px; background-color: rgba(33, 152, 239, 0.13); font-weight: bold; color: inherit;",
                        if (!is.null(row_var_name)) {
                            htmltools::div(
                                htmltools::div(style = "font-size: 11px; color: inherit; opacity: 0.75; margin-bottom: 2px;", row_var_name),
                                htmltools::div(style = "font-size: 13px; color: inherit;", row_names[i])
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
                style = "border-collapse: collapse; width: 100%; margin: 15px 0; font-family: 'Segoe UI', system-ui, sans-serif; font-size: 13px; color: inherit; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
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

            # Create table header (iterate over every column, not just the first two,
            # so 2xC / Rx2 subtables render all categories)
            header_cells <- list(
                htmltools::tags$th(
                    style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center; background-color: rgba(138, 155, 172, 0.06); color: inherit;",
                    if (!is.null(col_var_name) || !is.null(row_var_name)) {
                        htmltools::div(
                            style = "font-weight: bold; color: inherit; font-size: 11px;",
                            if (!is.null(col_var_name)) paste0(col_var_name, " \u{2192}") else "",
                            htmltools::br(),
                            if (!is.null(row_var_name)) paste0(row_var_name, " \u{2193}") else ""
                        )
                    } else {
                        ""
                    }
                )
            )
            for (col in col_names) {
                header_cells[[length(header_cells) + 1]] <- htmltools::tags$th(
                    style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center;",
                    if (!is.null(col_var_name)) {
                        htmltools::div(
                            htmltools::div(style = "font-size: 11px; color: inherit; opacity: 0.75; margin-bottom: 2px;", col_var_name),
                            htmltools::div(style = "font-size: 13px; color: inherit;", col)
                        )
                    } else {
                        col
                    }
                )
            }
            header_cells[[length(header_cells) + 1]] <- htmltools::tags$th(
                style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center;", htmltools::strong("Total"))
            header_row <- htmltools::tags$tr(style = "background-color: rgba(33, 152, 239, 0.13); color: inherit;", header_cells)

            # Create data rows (iterate over every row and every column)
            data_rows <- lapply(seq_along(row_names), function(i) {
                cells <- list(
                    htmltools::tags$td(
                        style = "border: 1px solid #e1e5e9; padding: 8px; background-color: rgba(33, 152, 239, 0.13); font-weight: bold; color: inherit;",
                        if (!is.null(row_var_name)) {
                            htmltools::div(
                                htmltools::div(style = "font-size: 11px; color: inherit; opacity: 0.75; margin-bottom: 2px;", row_var_name),
                                htmltools::div(style = "font-size: 13px; color: inherit;", row_names[i])
                            )
                        } else {
                            row_names[i]
                        }
                    )
                )
                for (j in seq_along(col_names)) {
                    row_pct <- if (row_totals[i] > 0) round(subtable[i, j] / row_totals[i] * 100, 1) else 0
                    cells[[length(cells) + 1]] <- htmltools::tags$td(
                        style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center;",
                        subtable[i, j], htmltools::br(), htmltools::tags$small(paste0("(", row_pct, "%)"))
                    )
                }
                cells[[length(cells) + 1]] <- htmltools::tags$td(
                    style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center; font-weight: bold;",
                    row_totals[i]
                )
                htmltools::tags$tr(cells)
            })

            # Create total row (iterate over every column)
            total_cells <- list(
                htmltools::tags$td(style = "border: 1px solid #e1e5e9; padding: 8px;", "Total")
            )
            for (j in seq_along(col_names)) {
                col_pct <- if (grand_total > 0) round(col_totals[j] / grand_total * 100, 1) else 0
                total_cells[[length(total_cells) + 1]] <- htmltools::tags$td(
                    style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center;",
                    col_totals[j], htmltools::br(), htmltools::tags$small(paste0("(", col_pct, "%)"))
                )
            }
            total_cells[[length(total_cells) + 1]] <- htmltools::tags$td(
                style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center;",
                grand_total
            )
            total_row <- htmltools::tags$tr(style = "background-color: rgba(88, 88, 88, 0.06); font-weight: bold; color: inherit;", total_cells)

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
            # character(0), NOT "": these accumulate with c() below, and an empty-string
            # seed made `length(warning_msg) > 0` true even when nothing was wrong, so
            # the "All conditions met" panel still printed an "Issues identified:"
            # heading with one blank bullet.
            warning_level <- "none"
            warning_msg <- character(0)
            recommendation <- character(0)

            if (prop_low_1 > 0) {
                warning_level <- "severe"
                warning_msg <- c(warning_msg, sprintf(.("Critical: %.0f%% of cells have expected counts < 1"), prop_low_1 * 100))
                recommendation <- c(recommendation, .("Chi-square test is not valid. Use Fisher's exact test instead."))
            } else if (prop_low_5 > 0.2) {
                warning_level <- "moderate"
                warning_msg <- c(warning_msg, sprintf(.("Warning: %.0f%% of cells have expected counts < 5"), prop_low_5 * 100))
                recommendation <- c(recommendation, .("Consider using Fisher's exact test for more reliable results."))
            } else if (low_expected_5 > 0) {
                warning_level <- "mild"
                warning_msg <- c(warning_msg, sprintf(.("Note: %d cell(s) have expected counts < 5"), low_expected_5))
                recommendation <- c(recommendation, .("Results should be interpreted with caution."))
            }
            
            # Check sample size
            total_n <- sum(contTable)
            if (total_n < 20) {
                warning_level <- "moderate"
                warning_msg <- c(warning_msg, sprintf(.("Small sample size (n=%d)"), total_n))
                recommendation <- c(recommendation, .("Results may be unreliable with small samples."))
            }
            
            
            # Check power
            power_results <- private$.checkPower(total_n, chiSqTest$parameter, self$options$sig)
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
        .checkPower = function(total_n, df, alpha = 0.05) {
            # Define medium effect size (w = 0.3)
            medium_effect <- 0.3
            target_power <- 0.8

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
                        warning_msg <- sprintf(.("The study may be underpowered (Power = %.1f%% at \u{3B1} = %g) to detect a medium effect size (w = 0.3)."), power_actual$power * 100, alpha)
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
            # Calculate effect size (Cramer's V)
            n <- sum(contTable)
            k <- min(nrow(contTable), ncol(contTable))
            cramers_v <- sqrt(chiSqTest$statistic / (n * (k - 1)))
            
            # Interpret the effect size against Cohen's benchmarks.
            #
            # Cohen's w cut-points (0.1 / 0.3 / 0.5) translate to Cramer's V by
            # dividing by sqrt(df*), where df* = min(r, c) - 1 -- the SAME quantity V
            # is normalised by, and NOT the chi-square df = (r-1)(c-1). Keying the
            # bands on the chi-square df mis-grades every non-square table: a 3x2
            # table (three grades x a binary outcome) has df = 2 but df* = 1, so
            # V = 0.25 was being reported as a "medium" effect when Cohen's benchmark
            # makes it small.
            dfstar <- k - 1
            cuts <- c(0.1, 0.3, 0.5) / sqrt(dfstar)
            effect_interpretation <- if (cramers_v < cuts[1]) .("negligible")
                else if (cramers_v < cuts[2]) .("small")
                else if (cramers_v < cuts[3]) .("medium")
                else .("large")

            alpha <- self$options$sig

            # Count significant pairwise comparisons
            n_significant_pairs <- 0
            if (!is.null(pairwise_results) && length(pairwise_results) > 0) {
                n_significant_pairs <- sum(sapply(pairwise_results, function(x) {
                    x$actual_pvalue_adjusted < alpha
                }))
            }
            
            # Generate natural language summary
            var_names <- if (!is.null(rows) && !is.null(cols)) {
                paste0(.("'"), rows, .("' and '"), cols, .("'"))
            } else {
                .("the row and column variables")
            }
            
            # One rung, not three. A p-value is not a measure of how strong or how
            # true an effect is, so "highly significant" / "very significant" was
            # dropped: the magnitude claim belongs to Cramer's V, which is reported
            # in the same sentence.
            significance_text <- if (chiSqTest$p.value < alpha) {
                sprintf(.("statistically significant at \u{3B1} = %g"), alpha)
            } else {
                sprintf(.("not statistically significant at \u{3B1} = %g"), alpha)
            }
            
            summary_text <- sprintf(
                .("The association between %s was %s (\u{3C7}\u{B2}(%d) = %.2f, p %s, n = %d). The effect size was %s (Cram\u{E9}r's V = %.3f)."),
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
            if (self$options$posthoc == "none") {
                summary_text <- paste(summary_text, .("Pairwise post-hoc comparisons were not performed because the post-hoc method is set to 'None'."))
            } else if (chiSqTest$p.value < alpha && !is.null(pairwise_results) && length(pairwise_results) > 0) {
                posthoc_text <- sprintf(
                    .("Post-hoc analysis revealed %d significant pairwise difference(s) out of %d comparisons after %s correction."),
                    n_significant_pairs,
                    length(pairwise_results),
                    self$options$posthoc
                )
                # paste(), not paste0(): these are two complete sentences and this
                # panel is offered for copying into reports.
                summary_text <- paste(summary_text, posthoc_text)
            }

            return(list(
                summary_text = summary_text,
                effect_size = cramers_v,
                effect_interpretation = effect_interpretation,
                n_significant_pairs = n_significant_pairs,
                significant = chiSqTest$p.value < alpha
            ))
        },

        # Generate copy-ready report sentences
        .generateReportSentences = function(chiSqTest, contTable, clinical_summary, assumptions) {
            sentences <- list()
            
            # Methods sentence. This panel is offered for pasting into a manuscript,
            # so it reports only what was DONE. It used to splice
            # assumptions$recommendation[1] in here, which is advice addressed to the
            # analyst ("Consider using Fisher's exact test for more reliable
            # results.") and read as a non-sequitur in the middle of a Methods
            # paragraph. The recommendation still appears in the Assumptions panel.
            sentences$methods <- sprintf(
                .("A chi-square test of independence, without continuity correction, was performed to examine the relationship between %s categorical variables.%s"),
                if (nrow(contTable) == 2 && ncol(contTable) == 2) "two binary" else "two",
                if (assumptions$low_expected_5 > 0)
                    sprintf(.(" %d of the %d cells had an expected count below 5."),
                            assumptions$low_expected_5, length(chiSqTest$expected))
                else ""
            )
            
            # Results sentence
            sentences$results <- clinical_summary$summary_text
            
            # Table description
            sentences$table <- sprintf(
                .("The contingency table consisted of %d rows and %d columns with a total sample size of %d."),
                nrow(contTable), ncol(contTable), sum(contTable)
            )
            
            # Statistical conclusion.
            # The branch and the printed alpha must read the SAME value: with
            # sig = 0.01 and p = 0.03 the old code took the p < 0.05 branch and then
            # printed "rejected at the alpha = 0.01 level", a false statement in a
            # panel whose whole purpose is to be pasted into a manuscript.
            sentences$conclusion <- if (chiSqTest$p.value < self$options$sig) {
                sprintf(
                    .("The null hypothesis of independence was rejected at the \u{3B1} = %g significance level."),
                    self$options$sig
                )
            } else {
                sprintf(
                    .("The null hypothesis of independence could not be rejected at the \u{3B1} = %g significance level. Absence of evidence against independence is not evidence of independence."),
                    self$options$sig
                )
            }
            
            return(sentences)
        },
        
        # Create statistical glossary
        .createGlossaryPanel = function() {
            glossary <- htmltools::div(
                style = "padding: 15px; background-color: rgba(138, 155, 172, 0.06); border-left: 4px solid #6c757d; margin: 10px 0; color: inherit;",
                htmltools::h4(.("Statistical Terms Glossary"), style = "color: inherit; margin-top: 0;"),

                htmltools::tags$dl(
                    htmltools::tags$dt(htmltools::strong(.("Chi-square test (\u{3C7}\u{B2})"))),
                    htmltools::tags$dd(.("Tests whether two categorical variables are independent. Used when you want to know if categories of one variable are associated with categories of another.")),

                    htmltools::tags$dt(htmltools::strong(.("p-value"))),
                    htmltools::tags$dd(.("The probability of observing results at least as extreme as what was found, assuming no real association exists. A result is called statistically significant when the p-value falls below the significance level set in the options (0.05 by default). A p-value is not the probability that there is no association, and a large p-value is not evidence that the variables are independent.")),

                    htmltools::tags$dt(htmltools::strong(.("Cram\u{E9}r's V"))),
                    htmltools::tags$dd(.("Effect size measure for chi-square tests. Ranges from 0 (no association) to 1 (perfect association). Cohen's benchmarks depend on the table shape: they are 0.1 (small), 0.3 (medium) and 0.5 (large) divided by the square root of min(rows, columns) - 1. So they are 0.1 / 0.3 / 0.5 for a 2xC or Rx2 table, and 0.07 / 0.21 / 0.35 for a table with three categories on both sides.")),

                    htmltools::tags$dt(htmltools::strong(.("Adjusted Standardized Residuals"))),
                    htmltools::tags$dd(.("Show which specific cell combinations contribute to significance. Each is a z-type statistic, so a cell is flagged when it exceeds the critical value printed beneath the residuals table (2.0 is the usual rule of thumb; the Bonferroni criterion uses a larger value that accounts for the number of cells). Because they grow with sample size, they rank cells within one table but are not an effect size.")),

                    htmltools::tags$dt(htmltools::strong(.("Expected Counts"))),
                    htmltools::tags$dd(.("The number of observations expected in each cell if the variables were independent. Chi-square test requires most cells to have expected counts \u{2265} 5.")),

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
                    style = "padding: 15px; background-color: rgba(138, 155, 172, 0.06); border-left: 4px solid #1976d2; margin: 10px 0; color: inherit;",
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
                    style = "padding: 15px; background-color: rgba(33, 159, 33, 0.1); border-left: 4px solid #4caf50; margin: 10px 0; color: inherit;",
                    htmltools::h4(.("Standardized Residuals Interpretation"), style = "color: #2e7d32; margin-top: 0;"),
                    htmltools::p(htmltools::strong(.("What are standardized residuals?"))),
                    htmltools::p(.("Standardized residuals measure how much each cell deviates from what we would expect if there was no association. The Pearson residual is (Observed - Expected) / \u{221A}Expected; the adjusted standardized residual reported here divides that by \u{221A}((1 - row proportion)(1 - column proportion)), which gives it an approximately standard normal distribution so it can be read like a z score.")),
                    htmltools::div(
                        style = "background-color: rgba(114, 184, 33, 0.1); padding: 10px; border-radius: 3px; color: inherit;",
                        htmltools::strong(.("Interpretation Guidelines:")), htmltools::br(),
                        .("\u{2022} |Residual| > 2.0: Suggests meaningful deviation"), htmltools::br(),
                        .("\u{2022} |Residual| > 3.0: Strong evidence of deviation"), htmltools::br(),
                        .("\u{2022} Positive values: Over-represented (more than expected)"), htmltools::br(),
                        .("\u{2022} Negative values: Under-represented (fewer than expected)")
                    )
                ))
            } else if (type == "multiple_testing" && !is.null(num_comparisons)) {
                # jmvcore's .() is a lookup, not a template engine: it performs no
                # brace interpolation, so "{num_comparisons}" used to reach the user
                # literally. Build the numbers with sprintf instead.
                method_name <- switch(self$options$posthoc,
                    bonferroni = .("Bonferroni"),
                    holm       = .("Holm"),
                    fdr        = .("Benjamini-Hochberg (FDR)"),
                    self$options$posthoc)
                return(htmltools::div(
                    style = "padding: 15px; background-color: rgba(255, 169, 33, 0.14); border-left: 4px solid #ff9800; margin: 10px 0; color: inherit;",
                    htmltools::h4(.("Multiple Testing Correction"), style = "color: #e65100; margin-top: 0;"),
                    htmltools::p(htmltools::strong(.("Why correction is needed:"))),
                    htmltools::p(sprintf(
                        .("When several tests are performed, the chance of at least one false positive grows. With %d comparisons at \u{3B1} = %g, that chance would be about %.0f%% if no correction were applied."),
                        num_comparisons, alpha, 100 * (1 - (1 - alpha)^num_comparisons))),
                    htmltools::div(
                        style = "background-color: rgba(230, 33, 99, 0.12); padding: 10px; border-radius: 3px; color: inherit;",
                        htmltools::strong(.("Correction Applied:")), htmltools::br(),
                        .("\u{2022} Number of comparisons:"), " ", num_comparisons, htmltools::br(),
                        .("\u{2022} Method:"), " ", method_name, htmltools::br(),
                        if (identical(self$options$posthoc, "bonferroni"))
                            list(.("\u{2022} Equivalent per-comparison level:"), " \u{3B1} = ",
                                 round(alpha / num_comparisons, 4), htmltools::br()),
                        sprintf(.("\u{2022} Compare the adjusted p-values against \u{3B1} = %g, not the raw ones"), alpha)
                    )
                ))
            }
        },
        
        # Create detailed guidance HTML for residuals interpretation
        .createResidualsGuidanceHTML = function(cutoff, criterion = "bonferroni") {
            criterion_text <- if (identical(criterion, "fixed")) {
                sprintf(.("Criterion: fixed cutoff, taken from the 'Residual significance cutoff' box (\u{B1}%.2f)."), cutoff)
            } else {
                sprintf(.("Criterion: Bonferroni-corrected normal deviate, which accounts for the number of cells tested (\u{B1}%.2f here). Switch the 'Residual significance criterion' to 'Fixed cutoff' to use the value in the cutoff box instead."), cutoff)
            }
            guidance <- htmltools::div(
                style = "padding: 15px; background-color: rgba(33, 137, 255, 0.07); color: inherit; border-left: 4px solid #1976d2; margin: 10px 0; font-family: 'Segoe UI', system-ui, sans-serif;",
                htmltools::h4(.("Residuals Interpretation Guidance"), style = "color: #1976d2; margin-top: 0;"),
                htmltools::p(
                    htmltools::strong(.("How to read the Residuals Table:")),
                    htmltools::br(),
                    .("Standardized residuals identify which specific cells in the table drive the overall significant association. They represent the distance (in standard deviations) between the observed count and the expected count.")
                ),
                htmltools::div(
                    style = "background-color: rgba(255, 255, 255, 0.06); padding: 12px; border: 1px border; border-color: #d1e3f8; border-radius: 5px; margin-bottom: 15px; color: inherit;",
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
                            sprintf(.("Values exceeding \u{B1}%.2f are flagged."), cutoff),
                            " ", criterion_text
                        )
                    )
                ),
                htmltools::div(
                    style = "font-style: italic; background-color: rgba(138, 155, 172, 0.06); padding: 10px; border-radius: 4px; border: 1px dashed #dee2e6; color: inherit;",
                    htmltools::strong(.("Example Interpretation:")), htmltools::br(),
                    sprintf(.("If the cell 'Grade 3 \u{D7} Positive' has a residual of +3.2 (with cutoff \u{B1}%.2f):"), cutoff), htmltools::br(),
                    .("It indicates that Grade 3 cases are significantly over-represented among the Positive group compared to what would be expected by chance.")
                )
            )
            return(as.character(guidance))
        },
        
        # Bootstrap CI for the pairwise effect size.
        #
        # The statistic is sqrt(chi-square / n). Every subtable tested here is 2xC or
        # Rx2, so min(r, c) - 1 = 1 and this is exactly Cramer's V (and equals |phi|
        # when the subtable is 2x2). It is nonnegative by construction, so it carries
        # no direction of association - the results column is labelled accordingly.
        .calculatePhiCI = function(subtable, conf_level = 0.95, n_boot = 999) {
            if (!requireNamespace("boot", quietly = TRUE)) {
                return("")
            }

            # A resample that loses a whole row or column cannot yield V. Scoring
            # those as 0 (the previous behaviour) injects a point mass at zero and
            # drags the lower limit down for exactly the sparse tables where the
            # interval matters most. Return NA and exclude them instead.
            phi_fun <- function(data, indices) {
                d <- data[indices, ]
                tab <- table(d[[1]], d[[2]])
                if (any(dim(tab) < 2)) return(NA_real_)

                # Chi-square with no correction, matching the point estimate
                chi <- try(private$.chisqQuiet(tab, correct = FALSE), silent = TRUE)
                if (inherits(chi, "try-error")) return(NA_real_)

                unname(sqrt(chi$statistic / sum(tab)))
            }
            
            # Prepare data for bootstrapping (reconstruct cases from table)
            row_names <- rownames(subtable)
            col_names <- colnames(subtable)
            cases <- expand.grid(R = row_names, C = col_names, stringsAsFactors = FALSE)
            counts <- as.vector(subtable)
            df <- cases[rep(seq_len(nrow(cases)), counts), ]
            
            # Seed the bootstrap.
            #
            # Without this the interval moved on every run of the SAME analysis on
            # the SAME data - measured [0.251, 0.573], [0.261, 0.578],
            # [0.260, 0.580] across three consecutive runs. In jamovi any option
            # toggle re-runs the analysis, so a clinician could copy an interval
            # into a manuscript and find it had changed by the time they looked
            # again. A resampling-based interval has to be reproducible to be
            # reportable.
            #
            # The global RNG state is saved and restored so seeding here does not
            # perturb anything else in the user's session.
            if (exists(".Random.seed", envir = .GlobalEnv)) {
                .old_seed <- get(".Random.seed", envir = .GlobalEnv)
                on.exit(assign(".Random.seed", .old_seed, envir = .GlobalEnv), add = TRUE)
            } else {
                on.exit(
                    if (exists(".Random.seed", envir = .GlobalEnv))
                        rm(".Random.seed", envir = .GlobalEnv),
                    add = TRUE)
            }
            set.seed(42)

            # Run bootstrap. Every failure path returns "" rather than NA: this value
            # goes straight into a text column, where a literal "NA" reads as a number
            # the analysis computed.
            boot_res <- try(boot::boot(data = df, statistic = phi_fun, R = n_boot), silent = TRUE)
            if (inherits(boot_res, "try-error")) return("")

            finite_t <- boot_res$t[is.finite(boot_res$t)]
            if (length(finite_t) < 100) return("")

            if (length(finite_t) < n_boot) {
                # boot.ci cannot skip the degenerate replicates (BCa in particular
                # needs the full resample array), so compute a percentile interval
                # from the replicates that are defined.
                probs <- c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2)
                interval <- unname(stats::quantile(finite_t, probs))
            } else {
                # Calculate BCa interval, falling back to percentile if BCa fails
                ci_res <- try(boot::boot.ci(boot_res, conf = conf_level, type = "bca"), silent = TRUE)
                if (inherits(ci_res, "try-error") || is.null(ci_res$bca)) {
                    ci_res <- try(boot::boot.ci(boot_res, conf = conf_level, type = "perc"), silent = TRUE)
                }
                if (inherits(ci_res, "try-error") || is.null(ci_res)) return("")
                interval <- if (!is.null(ci_res$bca)) ci_res$bca[4:5] else ci_res$percent[4:5]
            }

            if (length(interval) != 2 || any(!is.finite(interval))) return("")
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
                <br>\u{2022} <strong>", .("Individual observations:"), "</strong> ", .("Select row and column variables from raw data"), "
                <br>\u{2022} <strong>", .("Frequency counts:"), "</strong> ", .("Select row and column variables plus a counts variable for aggregated data"), "
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
                    paste0("<div style='padding: 15px; background-color: rgba(216, 33, 50, 0.18); border: 1px solid #f5c6cb; color: inherit;'>",
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
                    paste0("<div style='padding: 15px; background-color: rgba(216, 33, 50, 0.18); border: 1px solid #f5c6cb; color: inherit;'>",
                          "<strong>Error:</strong> The counts variable '", htmltools::htmlEscape(counts), "' does not exist in the data. Please select a valid numeric variable for counts.",
                          "</div>"))
                return(NULL)
            }

            # Missing values are handled in ONE place, .buildContingencyTable(),
            # because .plot() calls that helper directly with the raw self$data and
            # must apply exactly the same policy as .run().
            return(list(data = data, rows = rows, cols = cols, counts = counts))
        },
        
        # Shared helper method to create contingency table (used by both .run() and .plot())
        .buildContingencyTable = function(data, rows, cols, counts, show_warnings = TRUE) {
            # Missing values are ALWAYS excluded from the cross-classification.
            #
            # Carrying them in as an <NA> category - which is what the raw-rows path
            # used to do out of the box, because the missing-value checkbox is off by
            # default - makes NA a genuine level of the table: the chi-square df grows
            # from (r-1)(c-1) to r*c, the statistic and p-value are wrong, and the
            # post-hoc table fills with comparisons labelled "<NA> vs Grade 1".
            # droplevels() cannot prevent this, because NA is not a level. The
            # weighted (counts) path always dropped them via xtabs, so this also makes
            # the raw-rows and frequency-count analyses of the same data agree.
            analysis_vars <- intersect(c(rows, cols, counts), names(data))
            n_before <- nrow(data)
            if (length(analysis_vars) > 0)
                data <- data[stats::complete.cases(data[, analysis_vars, drop = FALSE]), , drop = FALSE]
            n_dropped <- n_before - nrow(data)
            if (n_dropped > 0 && show_warnings)
                private$.addNotice(
                    "STRONG_WARNING",
                    .("Rows with missing values were excluded"),
                    sprintf(
                        .("%d of %d rows (%.1f%%) had a missing value on %s and were dropped before the contingency table was built, so every result below describes the remaining %d rows. Missing values are never counted as a category of their own: that would add a row or column and change the degrees of freedom of the test. If the dropped rows may differ systematically from the rest, say so when reporting the association, or analyse missingness explicitly."),
                        n_dropped, n_before, 100 * n_dropped / n_before,
                        paste(analysis_vars, collapse = ", "), nrow(data)))

            # Drop unused factor levels so empty observed categories do not create
            # zero-margin rows/columns that would make the chi-square test undefined.
            if (is.factor(data[[rows]])) data[[rows]] <- droplevels(data[[rows]])
            if (is.factor(data[[cols]])) data[[cols]] <- droplevels(data[[cols]])

            # Create contingency table
            contTable <- try({
                if (!is.null(counts)) {
                    # Data is already summarized with counts - create weighted contingency table
                    # Ensure counts variable is numeric (jamovi may convert it to factor)
                    data[[counts]] <- as.numeric(as.character(data[[counts]]))
                    
                    # Build formula with composeTerm-escaped variable names
                    formula_str <- jmvcore::constructFormula(counts, c(rows, cols))
                    xtabs(jmvcore::asFormula(formula_str), data = data)
                } else {
                    # Regular individual observation data. useNA = "no" is the whole
                    # point of the listwise deletion above - see the comment there.
                    table(data[[rows]], data[[cols]], useNA = "no")
                }
            }, silent = TRUE)
            
            # Handle errors
            if (inherits(contTable, "try-error")) {
                # Use HTML message in todo element instead of Notice object
                if (show_warnings) {
                    self$results$todo$setContent(
                        paste0("<div style='padding: 15px; background-color: rgba(216, 33, 50, 0.18); border: 1px solid #f5c6cb; color: inherit;'>",
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
                        paste0("<div style='padding: 15px; background-color: rgba(216, 33, 50, 0.18); border: 1px solid #f5c6cb; color: inherit;'>",
                              "<strong>Error:</strong> Contingency table must have at least 2 rows and 2 columns. Please check that your variables have at least 2 categories each.",
                              "</div>"))
                }
                return(NULL)
            }
            
            # Add warning message about weighted data (only in main analysis).
            # Written whenever counts is set so the visible:(counts) panel is never
            # rendered empty (does not require the Educational panels toggle).
            if (!is.null(counts) && show_warnings) {
                weight_warning <- htmltools::div(
                    style = "padding: 10px; background-color: rgba(33, 181, 248, 0.14); border-left: 4px solid #0277bd; margin: 10px 0; color: inherit;",
                    htmltools::h5(" Weighted Data Analysis", style = "color: #01579b; margin-top: 0;"),
                    htmltools::p("The data is being treated as frequency counts. Each row represents a combination of categories with the specified count/weight.")
                )
                self$results$weightedDataInfo$setContent(as.character(weight_warning))
            }
            
            return(contTable)
        },
        
        # Helper method to perform chi-square test and populate results
        .performChiSquareTest = function(contTable, rows = NULL, cols = NULL) {
            # Guard against degenerate tables that make the chi-square test undefined.
            # .buildContingencyTable already rejects tables with < 2 rows/columns, but a table
            # can still have a zero row/column margin (a category with no observations) or be
            # entirely empty - both give zero expected counts and NaN statistics, and some
            # inputs make chisq.test error outright. Reject with an actionable message instead
            # of emitting NaN results or a cryptic error.
            if (sum(contTable) == 0)
                jmvcore::reject("The contingency table is empty: no rows are complete on both selected variables. Check the selected variables, and the counts variable if one is used.")
            empty_rows <- rownames(contTable)[rowSums(contTable) == 0]
            empty_cols <- colnames(contTable)[colSums(contTable) == 0]
            if (length(empty_rows) > 0 || length(empty_cols) > 0) {
                parts <- c(
                    if (length(empty_rows) > 0) paste0("row category/categories with no observations: ", paste(empty_rows, collapse = ", ")),
                    if (length(empty_cols) > 0) paste0("column category/categories with no observations: ", paste(empty_cols, collapse = ", ")))
                jmvcore::reject(paste0("The chi-square test is undefined because of ", paste(parts, collapse = "; "), ". Drop or merge empty categories and re-run."))
            }

            # Perform Chi-Square Test (wrapped: pathological but non-empty tables can still error) ----
            chiSqTest <- tryCatch(
                private$.chisqQuiet(contTable, correct = FALSE),
                error = function(e) jmvcore::reject(paste0("The chi-square test could not be computed: ", conditionMessage(e), ". Check that both variables are categorical with valid, nonnegative counts.")))

            # Add chi-square results to the table ----
            self$results$chisqTable$setRow(
                rowNo = 1,
                values = list(
                    stat = .("Pearson chi-square (no continuity correction)"),
                    value = chiSqTest$statistic,
                    df = chiSqTest$parameter,
                    p = chiSqTest$p.value
                )
            )

            # Disclose the correction choice: jamovi's own Contingency Tables applies
            # Yates' continuity correction to a 2x2 by default, so the same 2x2 table
            # gives a different statistic there and the reader needs to know why.
            if (nrow(contTable) == 2 && ncol(contTable) == 2)
                self$results$chisqTable$setNote(
                    "continuity",
                    .("Computed without Yates' continuity correction."))
            else
                self$results$chisqTable$setNote("continuity", NULL)

            # Format contingency table with optional expected values ----
            expValues <- if (self$options$exp) chiSqTest$expected else NULL
            tableHtml <- private$.createMainContingencyTableHTML(contTable, expValues, rows, cols)
            self$results$contingencyTable$setContent(tableHtml)
            
            return(chiSqTest)
        },
        
        # Helper method to handle residuals analysis
        .handleResidualsAnalysis = function(chiSqTest, contTable, rows = NULL, cols = NULL) {
            if (!self$options$showResiduals) return()
            
            # The cutoff box is honoured only when the criterion is set to "fixed".
            # Previously the box's own default value (2.0) was used as a sentinel for
            # "unset", so the documented 2.0 cutoff could never actually be requested,
            # and dialling the box from 3.0 back to 2.0 silently switched the analysis
            # to a Bonferroni-corrected z (2.64 for a 2x3 table) with no way back.
            cutoff_value <- if (identical(self$options$residualsCriterion, "fixed"))
                self$options$residualsCutoff else NULL
            residuals_analysis <- private$.analyzeResiduals(chiSqTest, contTable, self$options$sig, cutoff_value)

            # An analysis saved before the criterion option existed carries a custom
            # cutoff but no criterion, so it reopens with the Bonferroni default and
            # the box is ignored. Say which value is in force rather than silently
            # flagging a different set of cells than the saved file did.
            if (!identical(self$options$residualsCriterion, "fixed") &&
                !isTRUE(all.equal(self$options$residualsCutoff, 2.0)))
                private$.addNotice(
                    "WARNING",
                    .("The residual cutoff box is not in use"),
                    sprintf(
                        .("'Residual significance criterion' is set to the Bonferroni-corrected z, so cells are flagged at +/-%.2f and the 'Residual significance cutoff' value of %.2f is ignored. Set the criterion to 'Fixed cutoff' to use the value in the box."),
                        residuals_analysis$critical_value, self$options$residualsCutoff))

            # Create residuals guidance panel and results using htmltools
            residuals_guidance <- private$.createResidualsGuidanceHTML(
                residuals_analysis$critical_value, self$options$residualsCriterion)
            
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
                        "\u{B1}", round(residuals_analysis$critical_value, 3)
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
                    style = "border: 1px solid #e1e5e9; padding: 8px; background-color: rgba(138, 155, 172, 0.06); color: inherit;", 
                    htmltools::div(
                        style = "font-weight: bold; color: inherit;",
                        if (!is.null(col_var_name)) paste0(col_var_name, " \u{2192}") else "",
                        htmltools::br(),
                        if (!is.null(row_var_name)) paste0(row_var_name, " \u{2193}") else ""
                    )
                )
            )
            
            # Add column headers with variable name context
            for (col in col_names) {
                header_cells[[length(header_cells) + 1]] <- htmltools::tags$th(
                    style = "border: 1px solid #e1e5e9; padding: 8px; text-align: center; background-color: rgba(33, 159, 33, 0.1); color: inherit;", 
                    htmltools::div(
                        style = "font-weight: bold;",
                        if (!is.null(col_var_name)) {
                            list(
                                htmltools::div(style = "font-size: 11px; color: inherit; opacity: 0.75; margin-bottom: 2px;", col_var_name),
                                htmltools::div(style = "font-size: 13px; color: inherit;", col)
                            )
                        } else {
                            col
                        }
                    )
                )
            }
            header_row <- htmltools::tags$tr(style = "background-color: rgba(33, 159, 33, 0.1); color: inherit;", header_cells)
            
            # Create data rows
            data_rows <- lapply(seq_along(row_names), function(i) {
                # Row header
                row_cells <- list(
                    htmltools::tags$td(
                        style = "border: 1px solid #e1e5e9; padding: 8px; font-weight: bold; background-color: rgba(88, 88, 88, 0.06); color: inherit;",
                        if (!is.null(row_var_name)) {
                            htmltools::div(
                                htmltools::div(style = "font-size: 11px; color: inherit; opacity: 0.75; margin-bottom: 2px;", row_var_name),
                                htmltools::div(style = "font-size: 13px; color: inherit;", row_names[i])
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
                            "border: 1px solid #e1e5e9; padding: 8px; text-align: center; background-color: rgba(255, 33, 67, 0.09); color: inherit; font-weight: bold;"
                        } else {
                            "border: 1px solid #e1e5e9; padding: 8px; text-align: center; background-color: rgba(33, 152, 239, 0.13); color: inherit; font-weight: bold;"
                        }
                    } else {
                        "border: 1px solid #e1e5e9; padding: 8px; text-align: center; background-color: rgba(155, 155, 155, 0.06); color: inherit;"
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
                style = "border-collapse: collapse; width: 100%; margin: 15px 0; font-family: 'Segoe UI', system-ui, sans-serif; font-size: 13px; color: inherit; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
                htmltools::tags$thead(header_row),
                htmltools::tags$tbody(data_rows)
            )
        },
        
        # Helper method to handle post-hoc testing
        .handlePostHocTesting = function(chiSqTest, contTable, rows = NULL, cols = NULL, pairwise_results = NULL) {
            # addRow() appends unconditionally - it does not replace a row with the
            # same rowKey - and the table's clearWith list does not name every option
            # that re-runs .run() (ticking the glossary, for instance). Without this
            # the pairwise rows were appended a second and third time, so a 3x3 table
            # grew from 6 rows to 12 to 18 as the user toggled unrelated checkboxes.
            self$results$posthocTable$deleteRows()

            # CRITICAL FIX: Check if user wants to disable post-hoc tests entirely
            # When posthoc = "none", skip all pairwise testing
            #
            # NOTE (release review): "None" here means "run no pairwise tests",
            # not "run them without adjustment". That is a deliberate, documented
            # choice - the message below states it - and it produces no wrong
            # numbers, so it was left as is. Enabling unadjusted comparisons would
            # be a one-line change (p.adjust(p, "none") returns p unchanged) but is
            # a design decision, not a defect fix.
            if (self$options$posthoc == "none") {
                # Use HTML message only (no Notice object to avoid serialization errors)
                message_text <- paste0(
                    "You selected 'None' for post-hoc method. No pairwise comparisons will be performed. ",
                    "If you want pairwise comparisons with no p-value adjustment, this feature is not currently available. ",
                    "Please select Bonferroni, Holm, or FDR for pairwise testing with appropriate corrections."
                )
                self$results$multipleTestingInfo$setContent(
                    paste0("<div style='padding: 15px; background-color: rgba(216, 33, 50, 0.18); border: 1px solid #f5c6cb; color: inherit;'>",
                          "<strong>", .("Post-hoc Testing Disabled:"), "</strong> ",
                          message_text,
                          "</div>"))
                # Visibility is declared in the .r.yaml
                # (visible: (posthoc:bonferroni || posthoc:holm || posthoc:fdr)),
                # so the table is gated by the option itself rather than by a
                # setVisible() call whose effect outlives the run that made it.
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
                    " \u{2265} ", self$options$sig, "). ",
                    "Post-hoc pairwise comparisons are only valid when the overall test is significant. ",
                    "Running pairwise tests after a non-significant omnibus test increases Type I error (false positives) ",
                    "and constitutes data dredging."
                )
                self$results$multipleTestingInfo$setContent(
                    paste0("<div style='padding: 15px; background-color: rgba(255, 202, 33, 0.23); border: 1px solid #ffc107; color: inherit;'>",
                          "<strong>", .("Post-hoc Testing Not Performed:"), "</strong> ",
                          message_text,
                          "</div>"))
                # The table stays visible and empty, with the explanation above it.
                # Hiding it here made visibility a function of the PREVIOUS run's
                # data: the pane collapsed and re-expanded as the user nudged the
                # significance level across the omnibus p-value.
                return(invisible(NULL))
            }

            adjustMethod <- self$options$posthoc

            # Use robust pairwise testing approach with user-selected test method.
            # Computed once in .run() and passed in; recompute only if absent.
            if (is.null(pairwise_results))
                pairwise_results <- private$.robustPairwiseTests(contTable, adjustMethod, self$options$testSelection)
            
            if (length(pairwise_results) > 0) {
                fisher_used <- any(sapply(pairwise_results, function(x) startsWith(x$test_used, "Fisher")))
                fisher_mc_used <- any(sapply(pairwise_results, function(x) identical(x$test_used, "Fisher's exact (Monte Carlo)")))

                # Use HTML notice for educational panel (no Notice object to avoid serialization errors)
                fisher_notice <- NULL
                if (fisher_used) {
                    fisher_notice <- htmltools::div(
                        style = "padding: 10px; background-color: rgba(33, 152, 239, 0.13); border-left: 4px solid #1976d2; margin: 8px 0; color: inherit;",
                        htmltools::strong(.("Method notice: Pairwise comparisons with expected cell counts below 5 are automatically analysed with Fisher's exact test; the reported p-values use that exact method.")),
                        if (fisher_mc_used) htmltools::div(.("For at least one comparison the exact computation could not be completed, and Fisher's test was evaluated by Monte Carlo simulation instead; the Test Method column names those comparisons.")) else NULL
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
                } else {
                    # Authoritatively rewrite the panel on EVERY run. Without this
                    # final else, a "Post-hoc Testing Not Performed" message written
                    # on an earlier run stayed on screen above a fully populated
                    # pairwise table once the user raised the significance level.
                    self$results$multipleTestingInfo$setContent("")
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

                # The bootstrap interval is a 95% interval for ONE comparison; the
                # p-value beside it has been adjusted for all of them, so the two
                # columns can disagree about the same row.
                if (self$options$phiCI)
                    self$results$posthocTable$setNote(
                        "phici",
                        .("The bootstrap interval is computed at 95% for each comparison separately and is NOT adjusted for the number of comparisons, unlike the adjusted p-value column. An interval that excludes 0 does not imply significance after correction."))
                else
                    self$results$posthocTable$setNote("phici", NULL)

                # A two-level variable is deliberately not split into pairs: its one
                # pair is the whole table, i.e. the omnibus test reported above.
                if (nrow(contTable) < 3 || ncol(contTable) < 3)
                    self$results$posthocTable$setNote(
                        "twolevel",
                        .("A variable with only two categories is not split into pairs. Its single pair is the whole table, which is the chi-square test reported above, so including it would repeat that test inside its own post-hoc family and enlarge the multiple-testing correction. Only variables with three or more categories contribute pairwise comparisons."))
                else
                    self$results$posthocTable$setNote("twolevel", NULL)

                # Create detailed comparison tables (conditional)
                if (self$options$showDetailedTables) {
                    detailed_html <- private$.createDetailedComparisonHTML(pairwise_results, rows, cols)
                    self$results$detailedComparisons$setContent(detailed_html)
                }
            } else {
                # No valid pairwise comparisons possible. The (empty) posthoc
                # table is left visible: this is a failure state, not an
                # option-driven one, and hiding it makes the results pane
                # collapse and re-expand while the user picks variables.
                empty_message <- if (nrow(contTable) < 3 && ncol(contTable) < 3)
                    .("A 2x2 table has no pairwise sub-comparisons. Splitting either variable into a pair of levels reproduces the whole table, so the chi-square test reported above is already the only comparison there is. The residuals analysis shows which cells the association comes from.")
                else
                    .("No valid pairwise comparisons could be performed. This may be due to insufficient data in subtables or table structure.")
                self$results$multipleTestingInfo$setContent(
                    paste0("<div style='padding: 15px; background-color: rgba(255, 202, 33, 0.23); border: 1px solid #ffeaa7; color: inherit;'>", empty_message, "</div>"))
            }

            # NOTE: The export table is generated once from .run() (not here) to
            # avoid populating exportTable twice per run (duplicate rowKeys).
        },


        # Generate comprehensive export table
        .generateExportTable = function(chiSqTest, contTable, pairwise_results = NULL) {
            # Same append-not-replace problem as posthocTable - see the note there.
            self$results$exportTable$deleteRows()

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
                # A non-significant p-value is absence of evidence, not evidence that
                # the variables are independent - "No significant association" as a
                # bare verdict invites exactly that misreading.
                interpretation = if (chiSqTest$p.value < self$options$sig)
                    sprintf("Statistically significant association at \u{3B1} = %g", self$options$sig)
                else
                    sprintf("No statistically significant association detected at \u{3B1} = %g; absence of evidence is not evidence of absence, so read the effect size and its precision alongside this", self$options$sig)
            )
            row_index <- row_index + 1
            
            # Contingency Table Data
            row_names <- rownames(contTable)
            col_names <- colnames(contTable)
            for (i in seq_along(row_names)) {
                for (j in seq_along(col_names)) {
                    export_data[[row_index]] <- list(
                        category = "Contingency Table",
                        measure = paste(row_names[i], "\u{D7}", col_names[j]),
                        value = contTable[i, j],
                        interpretation = paste("Observed count:", contTable[i, j])
                    )
                    row_index <- row_index + 1
                }
            }
            
            # Residuals Analysis (if enabled)
            if (self$options$showResiduals) {
                cutoff_value <- if (identical(self$options$residualsCriterion, "fixed"))
                    self$options$residualsCutoff else NULL
                residuals_analysis <- private$.analyzeResiduals(chiSqTest, contTable, self$options$sig, cutoff_value)
                
                export_data[[row_index]] <- list(
                    category = "Residuals Analysis",
                    measure = "Critical Value",
                    value = round(residuals_analysis$critical_value, 3),
                    interpretation = paste("\u{B1}", round(residuals_analysis$critical_value, 3), "for significance")
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
                            measure = paste(row_names[i], "\u{D7}", col_names[j], "Residual"),
                            value = round(residuals_analysis$adjusted_residuals[i, j], 3),
                            interpretation = residuals_analysis$interpretation[i, j]
                        )
                        row_index <- row_index + 1
                    }
                }
            }
            
            # Post-hoc Results (only when post-hoc testing is enabled AND the
            # omnibus test is significant - the same gate the results pane uses)
            if (self$options$posthoc == "none") {
                export_data[[row_index]] <- list(
                    category = "Pairwise Comparisons",
                    measure = "Post-hoc method",
                    value = "None",
                    interpretation = "No pairwise comparisons were performed. Select Bonferroni, Holm or FDR to obtain them."
                )
                row_index <- row_index + 1
            } else if (chiSqTest$p.value < self$options$sig) {
                # Reuse the pairwise results computed in .run(); recompute only if absent.
                if (is.null(pairwise_results))
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
                            interpretation = paste("Effect size (Phi/V) =", round(result$effect_size, 3))
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
            # Reset the notice buffer: without this the same notice is appended once
            # per run cycle and the panel grows a duplicate on every option change.
            private$.noticeList <- list()
            private$.renderNotices()

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
            contTable <- private$.buildContingencyTable(data, rows, cols, counts, show_warnings = TRUE)
            if (is.null(contTable)) return()  # Error notice already inserted
            
            # The "Exclude missing values" checkbox is inert: listwise deletion is
            # unconditional (see .buildContingencyTable). Only say so when the user
            # has actually cleared the box, so the notice never appears unprompted.
            if (!isTRUE(self$options$excl))
                private$.addNotice(
                    "WARNING",
                    .("The 'Exclude missing values' box has no effect"),
                    .("Rows with a missing value on the row, column or counts variable are always excluded before the contingency table is built, whether or not this box is ticked. Counting missing values as a category of their own would add a row or a column to the table and change the degrees of freedom of the chi-square test."))

            # Perform main chi-square test
            chiSqTest <- private$.performChiSquareTest(contTable, rows, cols)

            # Compute pairwise post-hoc comparisons once and reuse across the
            # post-hoc table, clinical summary, and export table. This avoids
            # re-running the (optionally bootstrapped) pairwise tests 3-4x per run.
            #
            # Post-hoc method "None" means "run no pairwise tests" (see the option
            # description). It therefore has to gate this too: computing them for the
            # clinical summary or the export table ran p.adjust(p, "none"), which
            # returns the raw p-values unchanged, and both panels then labelled those
            # unadjusted values "after none correction" / "Adjusted p-value" - exactly
            # the uncorrected multiple testing the option exists to prevent.
            pairwise_results <- NULL
            need_pairwise <- self$options$posthoc != "none"
            if (need_pairwise && chiSqTest$p.value < self$options$sig)
                pairwise_results <- private$.robustPairwiseTests(contTable, self$options$posthoc, self$options$testSelection)

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
                # Also as a notice: this condition invalidates the test that is
                # printed above it, and the HTML div shares the `todo` element with
                # the welcome text, so it reads as ordinary body copy.
                # What happened to the PAIRWISE comparisons is read off the results
                # that were actually produced, not assumed. Fisher is used only when
                # post-hoc testing is enabled, the omnibus test is significant, and
                # the test selection is Fisher or auto-with-low-expected-counts; the
                # old wording promised it unconditionally.
                fisher_pairwise <- length(pairwise_results) > 0 &&
                    any(vapply(pairwise_results,
                               function(x) startsWith(x$test_used, "Fisher"), logical(1)))
                pairwise_note <- if (fisher_pairwise)
                    .(" Fisher's exact test was used for the pairwise comparisons whose expected counts fell below 5.")
                else if (length(pairwise_results) > 0)
                    .(" The pairwise comparisons below were computed with the chi-square test and rest on the same approximation.")
                else
                    .(" No pairwise comparisons were performed for this table.")
                private$.addNotice(
                    "STRONG_WARNING",
                    .("Sample too small for the chi-square approximation"),
                    paste0(sprintf(
                        .("Only %d observations remain in the table. The chi-square statistic relies on a large-sample approximation, so the p-value reported above is unreliable at this size. For the overall table, an exact test or a larger sample is needed before the p-value can be read at face value."),
                        assumptions$total_n), pairwise_note))
            }

            # Low expected counts warning
            if (assumptions$prop_low_5 > 0.2) {
                warning_msg <- sprintf('%.0f%% of cells have expected counts < 5. Chi-square test assumptions violated. Use Fisher\'s exact test for more reliable results.', assumptions$prop_low_5 * 100)
                warning_messages <- c(warning_messages, warning_msg)
                private$.addNotice(
                    "STRONG_WARNING",
                    .("Expected counts are too low for the chi-square approximation"),
                    sprintf(
                        .("%d of %d cells (%.0f%%) have an expected count below 5, above the usual 20%% limit. The chi-square approximation breaks down in that situation, so the overall p-value above is not dependable. Fisher's exact test, or merging categories that are sparse for substantive reasons, gives a p-value that does not rely on the approximation. Turn on the Assumptions check panel for the per-cell detail."),
                        assumptions$low_expected_5, length(chiSqTest$expected), assumptions$prop_low_5 * 100))
            }

            # Display combined warnings in todo element if any exist
            if (length(warning_messages) > 0) {
                combined_warnings <- paste0(
                    "<div style='padding: 15px; background-color: rgba(255, 202, 33, 0.23); border: 1px solid #ffc107; color: inherit;'>",
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
            
            # Handle residuals analysis (residuals are computed inside the handler)
            if (self$options$showResiduals) {
                private$.handleResidualsAnalysis(chiSqTest, contTable, rows, cols)
            }
            
            # Generate and display clinical summary (reuses shared pairwise_results)
            if (self$options$showClinicalSummary) {
                clinical_summary <- private$.generateClinicalSummary(chiSqTest, contTable, pairwise_results, rows, cols)
                private$.displayClinicalSummary(clinical_summary)
            }
            
            # Handle post-hoc testing (reuses shared pairwise_results)
            private$.handlePostHocTesting(chiSqTest, contTable, rows, cols, pairwise_results)
            
            # Detailed comparison tables are handled within post-hoc testing
            # No separate call needed as it's integrated into .handlePostHocTesting()
            
            # Generate report-ready sentences if requested
            if (self$options$copyReadySentences) {
                # Reuse `assumptions` from above (re-running .validateAssumptions()
                # calls pwr::pwr.chisq.test twice more for a value already in scope)
                # and pass the SAME pairwise results the Clinical Summary panel used.
                # Passing NULL made the copy-ready "Results" sentence silently drop
                # the post-hoc clause that the Clinical Summary printed verbatim for
                # the same run, so the two panels disagreed.
                clinical_summary <- private$.generateClinicalSummary(chiSqTest, contTable, pairwise_results, rows, cols)
                sentences <- private$.generateReportSentences(chiSqTest, contTable, clinical_summary, assumptions)
                private$.displayReportSentences(sentences)
            }
            
            # Show statistical glossary if requested
            if (self$options$showGlossary) {
                glossary <- private$.createGlossaryPanel()
                self$results$glossaryPanel$setContent(glossary)
            }
            
            # Export results if requested (reuses shared pairwise_results)
            if (self$options$exportResults) {
                private$.generateExportTable(chiSqTest, contTable, pairwise_results)
            }

            # Note: INFO notice at completion removed to avoid serialization errors
            # The analysis results are already visible in the results tables
        },
        
        # Display clinical summary
        .displayClinicalSummary = function(clinical_summary) {
            summary_style <- if (clinical_summary$significant) {
                "padding: 15px; background-color: rgba(33, 159, 33, 0.1); color: inherit; border-left: 4px solid #4caf50; margin: 10px 0;"
            } else {
                "padding: 15px; background-color: rgba(255, 169, 33, 0.14); color: inherit; border-left: 4px solid #ff9800; margin: 10px 0;"
            }
            
            summary_content <- htmltools::div(
                style = summary_style,
                htmltools::h4(.("Clinical Summary"), style = "margin-top: 0; color: #2e7d32;"),
                htmltools::p(clinical_summary$summary_text),
                htmltools::div(
                    style = "margin-top: 10px; font-size: 12px; color: inherit; opacity: 0.75;",
                    htmltools::strong(sprintf(
                        .("Effect size interpretation: The effect size is %s (Cram\u{E9}r's V = %.3f)."),
                        clinical_summary$effect_interpretation,
                        clinical_summary$effect_size
                    ))
                )
            )
            
            self$results$clinicalSummary$setContent(as.character(summary_content))
        },
        
        # Display assumptions check results
        .displayAssumptionsCheck = function(assumptions, chiSqTest) {
            # Translucent tints, not opaque pastels: each composites over white to
            # exactly the old colour in the light theme, and tints instead of
            # replacing the background in the dark theme, where an opaque pastel with
            # no foreground colour left this whole panel light-on-light.
            warning_colors <- list(
                none = "rgba(33, 162, 64, 0.19)",
                mild = "rgba(255, 202, 33, 0.23)",
                moderate = "rgba(255, 202, 33, 0.4)",
                severe = "rgba(216, 33, 50, 0.18)"
            )

            border_colors <- list(
                none = "#c3e6cb",
                mild = "#ffeaa7",
                moderate = "#fdcb6e",
                severe = "#dc3545"
            )
            
            style <- sprintf(
                "padding: 15px; background-color: %s; color: inherit; border-left: 4px solid %s; margin: 10px 0;",
                warning_colors[[assumptions$warning_level]],
                border_colors[[assumptions$warning_level]]
            )
            
            title_text <- if (assumptions$warning_level == "none") {
                .("Assumptions check: All conditions met")
            } else {
                .("Assumptions check: Review required")
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
                style = "padding: 15px; background-color: rgba(153, 33, 170, 0.12); border-left: 4px solid #9c27b0; margin: 10px 0; color: inherit;",
                htmltools::h4(.("Report-Ready Sentences"), style = "color: #7b1fa2; margin-top: 0;"),

                htmltools::div(
                    htmltools::h5(.("Methods:")),
                    htmltools::div(
                        style = "background: rgba(255, 255, 255, 0.06); color: inherit; padding: 10px; border-radius: 4px; margin: 5px 0; border: 1px solid #ddd;",
                        htmltools::p(sentences$methods)
                    )
                ),

                htmltools::div(
                    htmltools::h5(.("Results:")),
                    htmltools::div(
                        style = "background: rgba(255, 255, 255, 0.06); color: inherit; padding: 10px; border-radius: 4px; margin: 5px 0; border: 1px solid #ddd;",
                        htmltools::p(sentences$results)
                    )
                ),

                htmltools::div(
                    htmltools::h5(.("Conclusion:")),
                    htmltools::div(
                        style = "background: rgba(255, 255, 255, 0.06); color: inherit; padding: 10px; border-radius: 4px; margin: 5px 0; border: 1px solid #ddd;",
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
                private$.buildContingencyTable(self$data, rows, cols, counts, show_warnings = FALSE)
            }, silent = TRUE)
            
            if (inherits(contTable, "try-error") || is.null(contTable) || any(dim(contTable) < 2))
                return(FALSE)

            # Chi-square -> residuals.
            #
            # $stdres (adjusted standardized residuals), NOT $residuals (Pearson).
            # The residuals table, the export table and the "|residual| > 2" rule of
            # thumb in the guidance panel all refer to the adjusted version, which is
            # always the larger of the two: plotting Pearson residuals printed a
            # different number in the same cell of the same table, and made cells the
            # table flagged as significant look unremarkable in the heatmap.
            chiSqTest <- try(private$.chisqQuiet(contTable, correct = FALSE), silent = TRUE)
            if (inherits(chiSqTest, "try-error"))
                return(FALSE)
            resids <- chiSqTest$stdres

            # Same critical value the table uses, so the two panels flag the same cells
            cutoff_value <- if (identical(self$options$residualsCriterion, "fixed"))
                self$options$residualsCutoff else NULL
            critical <- private$.calculateResidualsCriticalValue(contTable, self$options$sig, cutoff_value)

            # Melt into a data.frame
            df <- as.data.frame(as.table(resids), responseName = "residual")
            names(df) <- c("Row", "Col", "Residual")
            df$Row <- factor(df$Row, levels = rev(rownames(resids)))
            df$Col <- factor(df$Col, levels = colnames(resids))
            df$Label <- paste0(round(df$Residual, 2),
                               ifelse(!is.na(df$Residual) & abs(df$Residual) > critical, "*", ""))
            # scale_fill_gradient2() below is given no limits, so it rescales to the
            # data: rescale_mid() maps a residual r to 0.5 + r / (2 * max|residual|).
            # The label colour therefore has to key off that RELATIVE position. Keyed
            # to a constant 3, a table whose largest residual was 8 painted a cell of
            # 3.1 - which lands at 0.69, a pale pink - in white and made it
            # unreadable, and a table whose range never reached 3 printed dark text
            # on the darkest tiles.
            fill_extent <- suppressWarnings(max(abs(df$Residual), na.rm = TRUE))
            if (!is.finite(fill_extent) || fill_extent <= 0) fill_extent <- Inf
            df$OnDarkFill <- !is.na(df$Residual) & abs(df$Residual) > 0.8 * fill_extent

            # ColorBrewer RdBu endpoints rather than pure "blue"/"red": the same
            # diverging reading, but distinguishable under deuteranopia.
            low_color <- "#2166ac"
            high_color <- "#b2182b"

            # Build ggplot
            # ggtheme is added BEFORE the two scales, not after. jamovi's ggtheme is a
            # LIST that carries its own default DISCRETE colour and fill scales, so
            # appending it last replaced scale_fill_gradient2() with a discrete scale
            # and the render died with "Continuous value supplied to a discrete
            # scale" - the residual heatmap never drew at all.
            p <- ggplot2::ggplot(df, ggplot2::aes(x = Col, y = Row, fill = Residual)) +
                ggplot2::geom_tile(color = "grey80") +
                ggplot2::geom_text(ggplot2::aes(label = Label, colour = OnDarkFill), size = 3) +
                ggtheme +
                ggplot2::scale_colour_manual(
                    values = c(`FALSE` = "grey10", `TRUE` = "white"),
                    guide  = "none"
                ) +
                ggplot2::scale_fill_gradient2(
                    low    = low_color,
                    mid    = "#f7f7f7",
                    high   = high_color,
                    midpoint = 0,
                    name     = "Adj. std.\nresidual",
                    guide    = ggplot2::guide_colorbar()
                ) +
                ggplot2::labs(
                    title = sprintf("Adjusted Standardized Residuals: %s vs %s", rows, cols),
                    subtitle = sprintf("* marks cells with |residual| > %.2f", critical),
                    x     = cols,
                    y     = rows
                ) +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                    panel.grid  = ggplot2::element_blank(),
                    plot.title  = ggplot2::element_text(hjust = 0.5),
                    plot.subtitle = ggplot2::element_text(hjust = 0.5)
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

            rows_arg <- paste0('rows = ', paste(deparse(rows), collapse = ' '))
            cols_arg <- paste0('cols = ', paste(deparse(cols), collapse = ' '))

            # Get other arguments using base helper (if available)
            args <- ''
            if (!is.null(private$.asArgs)) {
                args <- private$.asArgs(incData = FALSE)
            }
            if (args != '') {
                args_lines <- strsplit(args, ",\\s*\\n\\s*")[[1]]
                args_lines <- args_lines[!grepl("^\\s*(rows|cols)\\s*=", args_lines)]
                args <- if (length(args_lines) > 0) {
                    paste(args_lines, collapse = ',\n    ')
                } else {
                    ''
                }
            }
            if (args != '')
                args <- paste0(',\n    ', args)

            # Resolve the namespace this class was DEFINED in, not the one that
            # happens to be calling. utils::packageName() defaults to parent.frame(),
            # which from inside an R6 method is the caller's frame - so it returned
            # the caller's package (or NULL, falling through to a hard-coded
            # "ClinicoPath" that a ClinicoPathDescriptives-only user cannot load).
            pkg_name <- environmentName(topenv(environment(self$asSource)))
            if (!is.character(pkg_name) || !nzchar(pkg_name) ||
                pkg_name %in% c("R_GlobalEnv", "base"))
                pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, '::chisqposttest(\n    data = data,\n    ',
                   rows_arg, ',\n    ', cols_arg, args, ')')
        }
    ) # End of public list
)
