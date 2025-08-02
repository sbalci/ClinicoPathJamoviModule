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
        
        # Analyze residuals with proper statistical corrections
        .analyzeResiduals = function(chi_result, contingency_table, alpha = 0.05, cutoff = NULL) {
            # Calculate standardized and deviance residuals
            observed <- contingency_table
            expected <- chi_result$expected
            
            # Standardized residuals (Pearson residuals)
            std_residuals <- (observed - expected) / sqrt(expected)
            
            # Adjusted standardized residuals (with degrees of freedom correction)
            adjusted_residuals <- chi_result$stdres
            
            # Use custom cutoff if provided, otherwise use Bonferroni correction
            if (!is.null(cutoff)) {
                critical_z <- cutoff
            } else {
                # Identify significant cells using Bonferroni correction
                num_cells <- length(observed)
                critical_z <- qnorm(1 - (alpha / (2 * num_cells)))
            }
            
            # Create results matrix with significance indicators
            results <- list(
                observed = observed,
                expected = expected,
                std_residuals = std_residuals,
                adjusted_residuals = adjusted_residuals,
                significant_cells = abs(adjusted_residuals) > critical_z,
                critical_value = critical_z,
                interpretation = ifelse(abs(adjusted_residuals) > critical_z,
                                      ifelse(adjusted_residuals > 0, "Over-represented", "Under-represented"),
                                      "As expected")
            )
            
            return(results)
        },
        
        # Robust pairwise chi-square testing using chisq.multcomp approach
        .robustPairwiseTests = function(contingency_table, method = "bonferroni", test_selection = "auto") {
            # Convert table to vector format for chisq.multcomp
            row_names <- rownames(contingency_table)
            col_names <- colnames(contingency_table)
            
            # Create all pairwise combinations for rows
            pairwise_results <- list()
            comparison_index <- 1
            
            if (length(row_names) >= 2) {
                # Row-wise pairwise comparisons
                for (i in 1:(length(row_names) - 1)) {
                    for (j in (i + 1):length(row_names)) {
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
        
        # Create detailed HTML table for individual pairwise comparisons
        .createDetailedComparisonHTML = function(pairwise_results) {
            if (length(pairwise_results) == 0) return("")
            
            html <- "<div style='margin: 20px 0;'>"
            html <- paste0(html, "<h3 style='color: #1976d2;'>Detailed Pairwise Comparison Tables</h3>")
            
            for (i in seq_along(pairwise_results)) {
                result <- pairwise_results[[i]]
                subtable <- result$subtable
                
                # Determine test method and significance
                use_fisher <- any(subtable < 5)
                p_adj <- if (use_fisher) result$fisher_pvalue_adjusted else result$chi_pvalue_adjusted
                is_significant <- p_adj < 0.05
                
                # Create comparison header
                sig_indicator <- if (is_significant) {
                    "<span style='color: #d32f2f; font-weight: bold;'>✓ Significant</span>"
                } else {
                    "<span style='color: #666;'>Not significant</span>"
                }
                
                html <- paste0(html, 
                    "<div style='margin: 20px 0; padding: 15px; border: 1px solid #ddd; border-radius: 5px;'>",
                    "<h4 style='margin-top: 0; color: #1976d2;'>", result$comparison, "</h4>")
                
                # Test results summary
                test_method <- if (use_fisher) "Fisher's exact test" else "Chi-square test"
                p_value <- if (use_fisher) result$fisher_pvalue else result$chi_pvalue
                
                html <- paste0(html,
                    "<div style='margin-bottom: 15px; padding: 10px; background-color: #f5f5f5; border-radius: 3px;'>",
                    "<strong>Test Results:</strong><br>",
                    "Method: ", test_method, " (", if (use_fisher) "used due to low expected frequencies" else "standard approach", ")<br>",
                    "Chi-square = ", round(result$chi_statistic, 3), ", ",
                    "p = ", format.pval(p_value, digits = 3), ", ",
                    "Adjusted p = ", format.pval(p_adj, digits = 3), "<br>",
                    "Effect size (Phi) = ", round(result$effect_size, 3), "<br>",
                    sig_indicator, "</div>")
                
                # 2x2 contingency table
                row_names <- rownames(subtable)
                col_names <- colnames(subtable)
                row_totals <- rowSums(subtable)
                col_totals <- colSums(subtable)
                grand_total <- sum(subtable)
                
                html <- paste0(html,
                    "<table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>",
                    "<thead><tr style='background-color: #e3f2fd;'>",
                    "<th style='border: 1px solid #666; padding: 8px; text-align: center;'></th>",
                    "<th style='border: 1px solid #666; padding: 8px; text-align: center;'>", col_names[1], "</th>",
                    "<th style='border: 1px solid #666; padding: 8px; text-align: center;'>", col_names[2], "</th>",
                    "<th style='border: 1px solid #666; padding: 8px; text-align: center;'><strong>Total</strong></th>",
                    "</tr></thead><tbody>")
                
                # Add data rows with percentages
                for (j in 1:2) {
                    row_pct1 <- round(subtable[j, 1] / row_totals[j] * 100, 1)
                    row_pct2 <- round(subtable[j, 2] / row_totals[j] * 100, 1)
                    
                    html <- paste0(html,
                        "<tr>",
                        "<td style='border: 1px solid #666; padding: 8px; background-color: #e3f2fd; font-weight: bold;'>", row_names[j], "</td>",
                        "<td style='border: 1px solid #666; padding: 8px; text-align: center;'>", 
                        subtable[j, 1], "<br><small>(", row_pct1, "%)</small></td>",
                        "<td style='border: 1px solid #666; padding: 8px; text-align: center;'>", 
                        subtable[j, 2], "<br><small>(", row_pct2, "%)</small></td>",
                        "<td style='border: 1px solid #666; padding: 8px; text-align: center; font-weight: bold;'>", 
                        row_totals[j], "</td></tr>")
                }
                
                # Total row
                col_pct1 <- round(col_totals[1] / grand_total * 100, 1)
                col_pct2 <- round(col_totals[2] / grand_total * 100, 1)
                
                html <- paste0(html,
                    "<tr style='background-color: #f5f5f5; font-weight: bold;'>",
                    "<td style='border: 1px solid #666; padding: 8px;'>Total</td>",
                    "<td style='border: 1px solid #666; padding: 8px; text-align: center;'>", 
                    col_totals[1], "<br><small>(", col_pct1, "%)</small></td>",
                    "<td style='border: 1px solid #666; padding: 8px; text-align: center;'>", 
                    col_totals[2], "<br><small>(", col_pct2, "%)</small></td>",
                    "<td style='border: 1px solid #666; padding: 8px; text-align: center;'>", 
                    grand_total, "</td></tr>",
                    "</tbody></table></div>")
            }
            
            html <- paste0(html, "</div>")
            return(html)
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
        
        .run = function() {

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
                return()
            } else {
                todo <- ""
                html <- self$results$todo
                html$setContent(todo)
            }

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

            # Create the contingency table
            if (!is.null(counts)) {
                # Data is already summarized with counts - create weighted contingency table
                # Ensure counts variable is numeric (jamovi may convert it to factor)
                data[[counts]] <- as.numeric(as.character(data[[counts]]))
                
                # Build formula with backticks to handle special variable names
                formula_str <- paste0("`", counts, "` ~ `", rows, "` + `", cols, "`")
                contTable <- xtabs(as.formula(formula_str), data = data)
                
                # Add warning message about weighted data
                if (self$options$showEducational) {
                    weight_warning <- htmltools::div(
                        style = "padding: 10px; background-color: #e1f5fe; border-left: 4px solid #0277bd; margin: 10px 0;",
                        htmltools::h5("📊 Weighted Data Analysis", style = "color: #01579b; margin-top: 0;"),
                        htmltools::p("The data is being treated as frequency counts. Each row represents a combination of categories with the specified count/weight.")
                    )
                    self$results$weightedDataInfo$setContent(as.character(weight_warning))
                }
            } else {
                # Regular individual observation data
                contTable <- table(data[[rows]], data[[cols]], useNA = if(excl) "no" else "ifany")
            }

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
            if (self$options$exp) {
                # Include expected values
                formattedTable <- vcd::structable(contTable)
                expValues <- chiSqTest$expected

                # Create HTML table with observed and expected values
                tableHtml <- "<table border='1' style='border-collapse: collapse;'>"
                tableHtml <- paste0(tableHtml, "<tr><th></th>")

                # Add column headers
                colNames <- colnames(contTable)
                for (col in colNames) {
                    tableHtml <- paste0(tableHtml, "<th>", col, "</th>")
                }
                tableHtml <- paste0(tableHtml, "</tr>")

                # Add rows with observed and expected values
                rowNames <- rownames(contTable)
                for (i in 1:length(rowNames)) {
                    tableHtml <- paste0(tableHtml, "<tr><th>", rowNames[i], "</th>")
                    for (j in 1:length(colNames)) {
                        obs <- contTable[i, j]
                        exp <- round(expValues[i, j], 1)
                        tableHtml <- paste0(tableHtml, "<td>", obs, "<br><small>(", exp, ")</small></td>")
                    }
                    tableHtml <- paste0(tableHtml, "</tr>")
                }

                tableHtml <- paste0(tableHtml, "</table>")
                tableHtml <- paste0("<p>Values shown as: Observed<br><small>(Expected)</small></p>", tableHtml)
            } else {
                # Show only observed values
                formattedTable <- vcd::structable(contTable)

                # Convert to HTML table
                tableHtml <- "<table border='1' style='border-collapse: collapse;'>"
                tableHtml <- paste0(tableHtml, "<tr><th></th>")

                # Add column headers
                colNames <- colnames(contTable)
                for (col in colNames) {
                    tableHtml <- paste0(tableHtml, "<th>", col, "</th>")
                }
                tableHtml <- paste0(tableHtml, "</tr>")

                # Add rows with observed values
                rowNames <- rownames(contTable)
                for (i in 1:length(rowNames)) {
                    tableHtml <- paste0(tableHtml, "<tr><th>", rowNames[i], "</th>")
                    for (j in 1:length(colNames)) {
                        obs <- contTable[i, j]
                        tableHtml <- paste0(tableHtml, "<td>", obs, "</td>")
                    }
                    tableHtml <- paste0(tableHtml, "</tr>")
                }

                tableHtml <- paste0(tableHtml, "</table>")
            }

            self$results$contingencyTable$setContent(tableHtml)

            # Enhanced Residuals Analysis ----
            if (self$options$showResiduals) {
                # Use custom cutoff if provided
                cutoff_value <- if (self$options$residualsCutoff != 2.0) self$options$residualsCutoff else NULL
                residuals_analysis <- private$.analyzeResiduals(chiSqTest, contTable, self$options$sig, cutoff_value)
                
                # Create residuals interpretation panel and results
                residuals_panel <- private$.createEducationalPanel("residuals")
                
                # Format residuals results into HTML table
                residuals_html <- "<div style='margin: 15px 0;'>"
                residuals_html <- paste0(residuals_html, as.character(residuals_panel))
                
                # Add residuals table
                residuals_html <- paste0(residuals_html, 
                    "<table style='border-collapse: collapse; width: 100%; margin: 15px 0;'>",
                    "<thead><tr style='background-color: #e8f5e8;'>",
                    "<th style='border: 1px solid #666; padding: 8px;'></th>")
                
                col_names <- colnames(contTable)
                for (col in col_names) {
                    residuals_html <- paste0(residuals_html, 
                        "<th style='border: 1px solid #666; padding: 8px; text-align: center;'>", col, "</th>")
                }
                residuals_html <- paste0(residuals_html, "</tr></thead><tbody>")
                
                row_names <- rownames(contTable)
                for (i in seq_along(row_names)) {
                    residuals_html <- paste0(residuals_html, "<tr>",
                        "<td style='border: 1px solid #666; padding: 8px; font-weight: bold; background-color: #f5f5f5;'>", 
                        row_names[i], "</td>")
                    
                    for (j in seq_along(col_names)) {
                        residual_val <- round(residuals_analysis$adjusted_residuals[i, j], 3)
                        is_significant <- residuals_analysis$significant_cells[i, j]
                        interpretation <- residuals_analysis$interpretation[i, j]
                        
                        cell_style <- if (is_significant) {
                            if (residual_val > 0) "background-color: #ffebee; color: #c62828; font-weight: bold;"
                            else "background-color: #e3f2fd; color: #1565c0; font-weight: bold;"
                        } else {
                            "background-color: #f9f9f9;"
                        }
                        
                        residuals_html <- paste0(residuals_html,
                            "<td style='border: 1px solid #666; padding: 8px; text-align: center; ", cell_style, "'>",
                            residual_val, "<br><small>", interpretation, "</small></td>")
                    }
                    residuals_html <- paste0(residuals_html, "</tr>")
                }
                
                residuals_html <- paste0(residuals_html, "</tbody></table>")
                residuals_html <- paste0(residuals_html, 
                    "<p><small><strong>Critical value for significance:</strong> ±", 
                    round(residuals_analysis$critical_value, 3), "</small></p></div>")
                
                self$results$residualsAnalysis$setContent(residuals_html)
            }
            
            # Create educational overview panel (conditional)
            if (self$options$showEducational) {
                overview_panel <- private$.createEducationalPanel("overview")
                self$results$educationalOverview$setContent(as.character(overview_panel))
            }
            
            # Enhanced Pairwise Testing ----
            if (chiSqTest$p.value < self$options$sig) {
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
                        detailed_html <- private$.createDetailedComparisonHTML(pairwise_results)
                        self$results$detailedComparisons$setContent(detailed_html)
                    }
                } else {
                    # No valid pairwise comparisons possible
                    self$results$multipleTestingInfo$setContent(
                        "<div style='padding: 15px; background-color: #fff3cd; border: 1px solid #ffeaa7;'>No valid pairwise comparisons could be performed. This may be due to insufficient data in subtables or table structure.</div>")
                }
            } else {
                # Chi-square not significant, no post-hoc testing needed
                self$results$multipleTestingInfo$setContent(
                    "<div style='padding: 15px; background-color: #d4edda; border: 1px solid #c3e6cb;'><strong>Note:</strong> Overall chi-square test is not significant (p ≥ 0.05). Post-hoc pairwise comparisons are not recommended when the overall test is non-significant.</div>")
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

            # Optionally omit NAs
            data <- if (self$options$excl) jmvcore::naOmit(self$data) else self$data

            # Build contingency table
            contTable <- try({
                if (!is.null(counts)) {
                    data[[counts]] <- as.numeric(as.character(data[[counts]]))
                    formula_str <- paste0("`", counts, "` ~ `", rows, "` + `", cols, "`")
                    xtabs(as.formula(formula_str), data = data)
                }
                else
                    table(data[[rows]], data[[cols]], useNA = if(self$options$excl) "no" else "ifany")
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


            # self$results$mydataview_plot$setContent(
            #     list(
            #         df = df, 
            #         rows = rows, 
            #         cols = cols, 
            #         counts = counts,
            #         contTable = contTable,
            #         chiSqTest = chiSqTest,
            #         df = df,
            #         residuals = resids, 
            #         ggtheme = ggtheme, 
            #         theme = theme
            #     )
            # )


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
