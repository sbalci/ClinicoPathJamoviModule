#' @title Chi-Square Post-Hoc Analysis Addon
#' @description
#' Jamovi addon that extends the jmv::conttables function with comprehensive
#' post-hoc analysis capabilities for chi-square tests.
#'
#' @details
#' This addon follows the jamovi addon pattern (like moretests/ttestis) to extend
#' the base contingency tables analysis with advanced statistical methods:
#' - Standardized residuals analysis with multiple comparison corrections
#' - Pairwise comparisons between categories with automatic test selection
#' - Effect size calculations (Cramér's V, Phi coefficient)
#' - Educational guidance and clinical interpretations
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats chisq.test fisher.test p.adjust qnorm pnorm
#' @importFrom htmltools HTML div h3 h4 p strong br
#' @noRd

chisqposttestaddonClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "chisqposttestaddonClass",
    inherit = chisqposttestaddonBase,  # Generated from .a.yaml

    public = list(
        initialize = function() {
            super$initialize()
        }
    ),

    private = list(

        .init = function() {
            # DEBUG: Always create a visible table to test if addon is loading

            if (is.null(self$parent))
                return()

            # Create a simple debug table that should always appear
            debugTable <- jmvcore::Table$new(
                options = list(
                    name = "addonDebug",
                    title = "DEBUG: Addon is Loading!",
                    visible = TRUE  # Always visible for debugging
                )
            )

            # Add a simple column and row to verify it works
            debugTable$addColumn(name = "status", title = "Status", type = "text")
            debugTable$addRow(rowNo = 1, values = list(status = "ClinicoPath addon is working!"))

            # Try to insert the debug table
            tryCatch({
                self$parent$results$insert(debugTable)

                # Also try to add a note
                self$parent$results$setNote('debug', 'DEBUG: ClinicoPath addon loaded successfully')
            }, error = function(e) {
                # If insertion fails, at least try to add a note
                tryCatch({
                    self$parent$results$setNote('debug', paste('DEBUG: Addon error -', e$message))
                }, error = function(e2) {
                    # Silent fail
                })
            })
        },

        .run = function() {
            # DEBUG: Simple run method to test if addon is executing

            if (is.null(self$parent))
                return()

            # DEBUG: Update the debug table with run information
            if (!is.null(self$parent$results$addonDebug)) {
                # Add information about parent options
                debugInfo <- paste(
                    "Run executed! Parent options found:",
                    "resS =", if(!is.null(self$parent$options$resS)) self$parent$options$resS else "NULL",
                    "resP =", if(!is.null(self$parent$options$resP)) self$parent$options$resP else "NULL",
                    "resU =", if(!is.null(self$parent$options$resU)) self$parent$options$resU else "NULL",
                    "resA =", if(!is.null(self$parent$options$resA)) self$parent$options$resA else "NULL"
                )

                self$parent$results$addonDebug$setRow(rowNo = 1, values = list(status = debugInfo))
            }
        },

        # Extract contingency table data from parent conttables analysis
        .getContingencyData = function() {
            tryCatch({
                # Access parent conttables data through self$parent
                # Following the moretests pattern

                if (is.null(self$parent))
                    return(NULL)

                # Get the parent's options for row and column variables
                rows <- self$parent$options$rows
                cols <- self$parent$options$cols
                counts <- self$parent$options$counts

                if (is.null(rows) || is.null(cols))
                    return(NULL)

                # Get data from parent
                data <- self$data
                if (is.null(data) || nrow(data) == 0)
                    return(NULL)

                # Build contingency table same way as conttables does
                row_var <- data[[rows]]
                col_var <- data[[cols]]

                # Handle counts if provided
                if (!is.null(counts) && counts != "") {
                    counts_var <- data[[counts]]
                    contingency_table <- xtabs(counts_var ~ row_var + col_var)
                } else {
                    contingency_table <- table(row_var, col_var)
                }

                # Perform chi-square test to get residuals
                chi_result <- chisq.test(contingency_table)

                return(list(
                    table = contingency_table,
                    chi_result = chi_result,
                    rows = rows,
                    cols = cols
                ))

            }, error = function(e) {
                return(NULL)
            })
        },

        # Residuals analysis - extracted from chisqposttest
        .analyzeResiduals = function(chi_result, contingency_table, alpha = 0.05, cutoff = NULL) {
            # Calculate basic components
            observed <- contingency_table
            expected <- chi_result$expected
            total_sample <- sum(observed)

            # Calculate multiple types of residuals
            raw_residuals <- observed - expected
            std_residuals <- raw_residuals / sqrt(expected)
            adjusted_residuals <- chi_result$stdres  # Most appropriate for post-hoc

            # Calculate critical value with multiple comparison correction
            critical_z <- private$.calculateResidualsCriticalValue(contingency_table, alpha, cutoff)

            # Determine significance and effect magnitude
            significant_cells <- abs(adjusted_residuals) > critical_z
            effect_magnitude <- private$.classifyResidualMagnitude(adjusted_residuals)

            # Generate interpretations
            cell_interpretations <- private$.generateResidualInterpretations(
                adjusted_residuals, significant_cells, effect_magnitude
            )

            # Compile results
            results <- list(
                observed = observed,
                expected = expected,
                total_sample = total_sample,
                raw_residuals = raw_residuals,
                std_residuals = std_residuals,
                adjusted_residuals = adjusted_residuals,
                critical_value = critical_z,
                alpha_level = alpha,
                correction_method = if (is.null(cutoff)) "Bonferroni" else "Custom",
                significant_cells = significant_cells,
                effect_magnitude = effect_magnitude,
                interpretation = cell_interpretations,
                num_significant_cells = sum(significant_cells),
                proportion_significant = sum(significant_cells) / length(significant_cells),
                max_absolute_residual = max(abs(adjusted_residuals), na.rm = TRUE)
            )

            return(results)
        },

        # Calculate critical value for residuals significance testing
        .calculateResidualsCriticalValue = function(contingency_table, alpha, custom_cutoff) {
            if (!is.null(custom_cutoff)) {
                return(custom_cutoff)
            }

            # Bonferroni-corrected critical value
            num_cells <- length(contingency_table)
            corrected_alpha <- alpha / num_cells
            critical_z <- qnorm(1 - (corrected_alpha / 2))

            return(max(critical_z, 1.96))  # At least 1.96 for p < 0.05
        },

        # Classify residual magnitudes for effect size interpretation
        .classifyResidualMagnitude = function(residuals) {
            abs_residuals <- abs(residuals)
            magnitude <- array("Negligible", dim = dim(residuals))

            magnitude[abs_residuals >= 2.0 & abs_residuals < 2.5] <- "Small"
            magnitude[abs_residuals >= 2.5 & abs_residuals < 3.0] <- "Medium"
            magnitude[abs_residuals >= 3.0] <- "Large"

            return(magnitude)
        },

        # Generate interpretive text for residuals
        .generateResidualInterpretations = function(residuals, significant, magnitude) {
            interpretations <- array("", dim = dim(residuals))

            for (i in seq_len(nrow(residuals))) {
                for (j in seq_len(ncol(residuals))) {
                    residual_val <- residuals[i, j]
                    is_sig <- significant[i, j]
                    mag <- magnitude[i, j]

                    if (is_sig) {
                        direction <- if (residual_val > 0) "over-represented" else "under-represented"
                        interpretations[i, j] <- paste0(
                            mag, " ", direction, " (z = ", round(residual_val, 2), ")"
                        )
                    } else {
                        interpretations[i, j] <- "Not significant"
                    }
                }
            }

            return(interpretations)
        },

        # Pairwise comparisons between categories
        .performPairwiseComparisons = function(contingency_table, correction_method = "bonferroni") {
            row_names <- rownames(contingency_table)
            col_names <- colnames(contingency_table)

            results <- list()

            # Row-wise comparisons if more than 2 rows
            if (length(row_names) > 2) {
                row_pairs <- combn(row_names, 2, simplify = FALSE)
                results$row_comparisons <- private$.performRowComparisons(
                    contingency_table, row_pairs, correction_method
                )
            }

            # Column-wise comparisons if more than 2 columns
            if (length(col_names) > 2) {
                col_pairs <- combn(col_names, 2, simplify = FALSE)
                results$col_comparisons <- private$.performColumnComparisons(
                    contingency_table, col_pairs, correction_method
                )
            }

            return(results)
        },

        # Perform pairwise row comparisons
        .performRowComparisons = function(contingency_table, row_pairs, correction_method = "bonferroni") {
            comparisons <- list()

            for (pair in row_pairs) {
                row1 <- pair[1]
                row2 <- pair[2]

                # Extract 2x2 subtable
                subtable <- contingency_table[c(row1, row2), , drop = FALSE]

                # Perform appropriate test (automatic selection)
                test_result <- private$.performPairwiseTest(subtable)

                # Calculate effect size
                effect_size <- private$.calculatePairwiseEffectSize(subtable)

                comparisons[[paste(row1, "vs", row2)]] <- list(
                    pair = pair,
                    subtable = subtable,
                    test_result = test_result,
                    effect_size = effect_size
                )
            }

            # Apply multiple testing correction using user-specified method
            p_values <- sapply(comparisons, function(x) x$test_result$p.value)

            # Convert correction method name to R's p.adjust method name
            adjust_method <- switch(correction_method,
                "bonferroni" = "bonferroni",
                "holm" = "holm",
                "fdr" = "fdr",
                "none" = "none",
                "bonferroni"  # default fallback
            )

            adjusted_p <- if (adjust_method == "none") {
                p_values  # No adjustment
            } else {
                p.adjust(p_values, method = adjust_method)
            }

            for (i in seq_along(comparisons)) {
                comparisons[[i]]$adjusted_p <- adjusted_p[i]
                comparisons[[i]]$correction_method <- correction_method
            }

            return(comparisons)
        },

        # Perform pairwise column comparisons
        .performColumnComparisons = function(contingency_table, col_pairs, correction_method = "bonferroni") {
            comparisons <- list()

            for (pair in col_pairs) {
                col1 <- pair[1]
                col2 <- pair[2]

                # Extract 2x2 subtable
                subtable <- contingency_table[, c(col1, col2), drop = FALSE]

                # Perform appropriate test
                test_result <- private$.performPairwiseTest(subtable)

                # Calculate effect size
                effect_size <- private$.calculatePairwiseEffectSize(subtable)

                comparisons[[paste(col1, "vs", col2)]] <- list(
                    pair = pair,
                    subtable = subtable,
                    test_result = test_result,
                    effect_size = effect_size
                )
            }

            # Apply multiple testing correction using user-specified method
            p_values <- sapply(comparisons, function(x) x$test_result$p.value)

            # Convert correction method name to R's p.adjust method name
            adjust_method <- switch(correction_method,
                "bonferroni" = "bonferroni",
                "holm" = "holm",
                "fdr" = "fdr",
                "none" = "none",
                "bonferroni"  # default fallback
            )

            adjusted_p <- if (adjust_method == "none") {
                p_values  # No adjustment
            } else {
                p.adjust(p_values, method = adjust_method)
            }

            for (i in seq_along(comparisons)) {
                comparisons[[i]]$adjusted_p <- adjusted_p[i]
                comparisons[[i]]$correction_method <- correction_method
            }

            return(comparisons)
        },

        # Perform individual pairwise test with automatic method selection
        .performPairwiseTest = function(subtable) {
            # Automatic test selection based on expected frequencies
            chi_result <- chisq.test(subtable)
            if (any(chi_result$expected < 5)) {
                # Use Fisher's exact test for small expected frequencies
                return(fisher.test(subtable))
            } else {
                return(chi_result)
            }
        },

        # Calculate effect sizes for pairwise comparisons
        .calculatePairwiseEffectSize = function(subtable) {
            n <- sum(subtable)
            chi_stat <- chisq.test(subtable)$statistic

            # Cramér's V for 2x2 table (equivalent to Phi)
            phi <- sqrt(chi_stat / n)

            # Effect size interpretation
            interpretation <- if (phi >= 0.5) "Large"
                             else if (phi >= 0.3) "Medium"
                             else if (phi >= 0.1) "Small"
                             else "Negligible"

            return(list(
                phi = phi,
                cramers_v = phi,  # Same for 2x2 tables
                interpretation = interpretation
            ))
        },

        # Calculate comprehensive effect sizes for the full table
        .calculateEffectSizes = function(chi_result, contingency_table) {
            n <- sum(contingency_table)
            chi_stat <- chi_result$statistic
            min_dim <- min(dim(contingency_table)) - 1

            # Cramér's V
            cramers_v <- sqrt(chi_stat / (n * min_dim))

            # Phi coefficient (for 2x2 tables)
            phi <- if (all(dim(contingency_table) == c(2, 2))) {
                sqrt(chi_stat / n)
            } else {
                NA
            }

            return(list(
                cramers_v = cramers_v,
                phi = phi,
                chi_squared = chi_stat,
                sample_size = n
            ))
        },

        # Populate residuals table in parent results
        .populateResidualsTable = function(residuals_results, contingency_table) {
            table <- self$parent$results$enhancedPostHoc

            # Add columns if not already set
            if (length(table$columns) == 0) {
                table$addColumn(name = "cell", title = "Cell", type = "text")
                table$addColumn(name = "observed", title = "Observed", type = "integer")
                table$addColumn(name = "expected", title = "Expected", type = "number")
                table$addColumn(name = "stdResidual", title = "Std. Residual", type = "number")
                table$addColumn(name = "interpretation", title = "Interpretation", type = "text")
            }

            # Populate significant cells
            sig_cells <- which(residuals_results$significant_cells, arr.ind = TRUE)

            if (nrow(sig_cells) > 0) {
                for (i in seq_len(nrow(sig_cells))) {
                    row_idx <- sig_cells[i, 1]
                    col_idx <- sig_cells[i, 2]

                    cell_name <- paste(rownames(contingency_table)[row_idx],
                                     "×", colnames(contingency_table)[col_idx])

                    table$setRow(rowNo = i, values = list(
                        cell = cell_name,
                        observed = residuals_results$observed[row_idx, col_idx],
                        expected = round(residuals_results$expected[row_idx, col_idx], 2),
                        stdResidual = round(residuals_results$adjusted_residuals[row_idx, col_idx], 3),
                        interpretation = residuals_results$interpretation[row_idx, col_idx]
                    ))
                }
            } else {
                table$setRow(rowNo = 1, values = list(
                    cell = "No significant cells",
                    observed = NA,
                    expected = NA,
                    stdResidual = NA,
                    interpretation = "All cells consistent with independence"
                ))
            }
        },

        # Populate pairwise comparisons table in parent results
        .populatePairwiseTable = function(pairwise_results) {
            table <- self$parent$results$postHocPairwise

            # Add columns if not already set
            if (length(table$columns) == 0) {
                table$addColumn(name = "comparison", title = "Comparison", type = "text")
                table$addColumn(name = "statistic", title = "Test Statistic", type = "number")
                table$addColumn(name = "pValue", title = "p-value", type = "number", format = "zto,pvalue")
                table$addColumn(name = "adjPValue", title = "Adj. p-value", type = "number", format = "zto,pvalue")
                table$addColumn(name = "effectSize", title = "Cramér's V", type = "number")
            }

            rowNo <- 1

            # Add row comparisons
            if (!is.null(pairwise_results$row_comparisons)) {
                for (name in names(pairwise_results$row_comparisons)) {
                    comp <- pairwise_results$row_comparisons[[name]]
                    table$setRow(rowNo = rowNo, values = list(
                        comparison = paste("Rows:", name),
                        statistic = round(comp$test_result$statistic, 3),
                        pValue = comp$test_result$p.value,
                        adjPValue = comp$adjusted_p,
                        effectSize = round(comp$effect_size$phi, 3)
                    ))
                    rowNo <- rowNo + 1
                }
            }

            # Add column comparisons
            if (!is.null(pairwise_results$col_comparisons)) {
                for (name in names(pairwise_results$col_comparisons)) {
                    comp <- pairwise_results$col_comparisons[[name]]
                    table$setRow(rowNo = rowNo, values = list(
                        comparison = paste("Cols:", name),
                        statistic = round(comp$test_result$statistic, 3),
                        pValue = comp$test_result$p.value,
                        adjPValue = comp$adjusted_p,
                        effectSize = round(comp$effect_size$phi, 3)
                    ))
                    rowNo <- rowNo + 1
                }
            }
        },

        # Add educational note to parent results
        .addEducationalNote = function(residuals_results, effect_sizes) {
            # Add a note to parent results with interpretation guidance
            if (!is.null(self$parent$results)) {
                note_text <- paste0(
                    "Post-Hoc Analysis: ",
                    if (residuals_results$num_significant_cells > 0) {
                        paste0(residuals_results$num_significant_cells, " cells show significant departure from independence. ")
                    } else {
                        "No cells show significant departure from independence. "
                    },
                    "Effect size (Cramér's V): ", round(effect_sizes$cramers_v, 3),
                    " (", private$.interpretCramersV(effect_sizes$cramers_v), ")."
                )

                # Try to add note to parent (this depends on jamovi version)
                tryCatch({
                    self$parent$results$setNote("posthoc", note_text)
                }, error = function(e) {
                    # If setNote doesn't work, try alternate method
                })
            }
        },

        # Helper to interpret Cramér's V
        .interpretCramersV = function(v) {
            if (v >= 0.5) "large effect"
            else if (v >= 0.3) "medium effect"
            else if (v >= 0.1) "small effect"
            else "negligible effect"
        },

        # Generate educational content and interpretation guidance
        .generateEducationalContent = function(residuals_results, effect_sizes) {
            # Educational HTML content for users
            educational_html <- HTML(paste0(
                "<div style='padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; margin: 10px 0;'>",
                "<h4 style='color: #007bff; margin-top: 0;'>Chi-Square Post-Hoc Analysis Results</h4>",

                "<h5>Residuals Analysis</h5>",
                "<p>Standardized residuals identify which cells contribute most to chi-square significance:</p>",
                "<ul>",
                "<li><strong>|z| ≥ 2.0:</strong> Significant departure from independence (p < 0.05)</li>",
                "<li><strong>|z| ≥ 2.58:</strong> Highly significant departure (p < 0.01)</li>",
                "<li><strong>|z| ≥ 3.29:</strong> Very highly significant departure (p < 0.001)</li>",
                "</ul>",

                "<h5>Multiple Comparison Corrections</h5>",
                "<p>Bonferroni correction applied to control family-wise error rate in pairwise comparisons.</p>",

                "<h5>Effect Size Interpretation</h5>",
                "<p><strong>Cramér's V:</strong> 0.1 = small, 0.3 = medium, 0.5 = large association</p>",

                if (residuals_results$num_significant_cells > 0) {
                    paste0("<p><strong>Significant Cells Found:</strong> ",
                           residuals_results$num_significant_cells, " out of ",
                           length(residuals_results$significant_cells),
                           " cells show significant departure from independence.</p>")
                } else {
                    "<p><strong>No Significant Cells:</strong> All cells show patterns consistent with independence.</p>"
                },

                "</div>"
            ))

            # In a real implementation, this would populate results tables
            # For addon pattern, this would integrate with jamovi results system
            return(educational_html)
        }
    )
)
