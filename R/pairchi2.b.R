#' @title Pairwise Chi-Square Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats chisq.test p.adjust
#' @importFrom vcd assocplot
#' @export

pairchi2Class <- if (requireNamespace('jmvcore')) R6::R6Class(
    "pairchi2Class",
    inherit = pairchi2Base,
    private = list(
        # Helper function for data cleaning ----
        .cleanData = function() {
            rowVarName <- self$options$row
            colVarName <- self$options$col
            
            data <- jmvcore::select(self$data, c(rowVarName, colVarName))
            data <- jmvcore::naOmit(data)

            if (!is.null(rowVarName))
                data[[rowVarName]] <- as.factor(data[[rowVarName]])
            if (!is.null(colVarName))
                data[[colVarName]] <- as.factor(data[[colVarName]])

            return(data)
        },

        # Initialize table structures ----
        .init = function() {
            if (is.null(self$options$row) || is.null(self$options$col))
                return()

            data <- private$.cleanData()
            rowVarName <- self$options$row
            colVarName <- self$options$col

            if (nrow(data) == 0)
                return()

            # Setup main contingency table
            freqs <- self$results$conftable
            freqs$addColumn(name="var1", title=rowVarName, type="text")
            
            if (!is.null(colVarName) && colVarName %in% names(data)) {
                for (level in levels(data[[colVarName]])) {
                    freqs$addColumn(
                        name=paste0("col_", level),
                        title=level,
                        type="integer"
                    )
                }
            }
            freqs$addColumn(name="total", title="Total", type="integer")

            # Setup expected frequencies table  
            exp <- self$results$expected
            exp$addColumn(name="var1", title=rowVarName, type="text")
            
            if (!is.null(colVarName) && colVarName %in% names(data)) {
                for (level in levels(data[[colVarName]])) {
                    exp$addColumn(
                        name=paste0("exp_", level),
                        title=level,
                        type="number"
                    )
                }
            }

            # Setup standardized residuals table
            res <- self$results$stdres
            res$addColumn(name="var1", title=rowVarName, type="text")
            
            if (!is.null(colVarName) && colVarName %in% names(data)) {
                for (level in levels(data[[colVarName]])) {
                    res$addColumn(
                        name=paste0("res_", level),
                        title=level,
                        type="number"
                    )
                }
            }
        },

        # Calculate effect sizes ----
        .calculateEffectSizes = function(conftable) {
            n <- sum(conftable)
            chi2 <- suppressWarnings(stats::chisq.test(conftable, correct = FALSE))
            chi2_stat <- chi2$statistic
            
            # Cramér's V
            cramers_v <- sqrt(chi2_stat / (n * (min(dim(conftable)) - 1)))
            
            # Phi coefficient (for 2x2 tables)
            phi <- if (all(dim(conftable) == c(2, 2))) {
                sqrt(chi2_stat / n)
            } else {
                NA
            }
            
            # Interpret Cramér's V
            v_interp <- if (cramers_v < 0.1) "Small" 
                       else if (cramers_v < 0.3) "Medium" 
                       else if (cramers_v < 0.5) "Large" 
                       else "Very Large"
            
            return(list(
                cramers_v = cramers_v,
                phi = phi,
                v_interpretation = v_interp
            ))
        },

        # Create HTML table for 2x2 contingency table ----
        .create2x2HTML = function(table2x2, comparison, chi_result, p_adj, is_significant) {
            # Get row and column names
            row_names <- rownames(table2x2)
            col_names <- colnames(table2x2)
            
            # Calculate percentages
            row_totals <- rowSums(table2x2)
            col_totals <- colSums(table2x2)
            grand_total <- sum(table2x2)
            
            # Create significance indicator
            sig_indicator <- if (is_significant) {
                "<span style='color: #d32f2f; font-weight: bold;'>✓ Significant</span>"
            } else {
                "<span style='color: #666;'>Not significant</span>"
            }
            
            # Build HTML table
            html <- paste0(
                "<div style='margin: 20px 0; padding: 15px; border: 1px solid #ddd; border-radius: 5px;'>",
                "<h4 style='margin-top: 0; color: #1976d2;'>", comparison, "</h4>",
                
                # Summary statistics
                "<div style='margin-bottom: 15px; padding: 10px; background-color: #f5f5f5; border-radius: 3px;'>",
                "<strong>Test Results:</strong><br>",
                "Chi-square = ", round(chi_result$statistic, 3), ", ",
                "df = ", chi_result$parameter, ", ",
                "p = ", format.pval(chi_result$p.value, digits = 3), ", ",
                "Adjusted p = ", format.pval(p_adj, digits = 3), "<br>",
                sig_indicator,
                "</div>",
                
                # 2x2 table with styling
                "<table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>",
                "<thead>",
                "<tr style='background-color: #e3f2fd;'>",
                "<th style='border: 1px solid #666; padding: 8px; text-align: center;'></th>",
                "<th style='border: 1px solid #666; padding: 8px; text-align: center;'>", col_names[1], "</th>",
                "<th style='border: 1px solid #666; padding: 8px; text-align: center;'>", col_names[2], "</th>",
                "<th style='border: 1px solid #666; padding: 8px; text-align: center;'><strong>Total</strong></th>",
                "</tr>",
                "</thead>",
                "<tbody>"
            )
            
            # Add data rows
            for (i in 1:2) {
                row_pct1 <- round(table2x2[i, 1] / row_totals[i] * 100, 1)
                row_pct2 <- round(table2x2[i, 2] / row_totals[i] * 100, 1)
                
                html <- paste0(html,
                    "<tr>",
                    "<td style='border: 1px solid #666; padding: 8px; background-color: #e3f2fd; font-weight: bold;'>", row_names[i], "</td>",
                    "<td style='border: 1px solid #666; padding: 8px; text-align: center;'>", 
                    table2x2[i, 1], "<br><small>(", row_pct1, "%)</small></td>",
                    "<td style='border: 1px solid #666; padding: 8px; text-align: center;'>", 
                    table2x2[i, 2], "<br><small>(", row_pct2, "%)</small></td>",
                    "<td style='border: 1px solid #666; padding: 8px; text-align: center; font-weight: bold;'>", 
                    row_totals[i], "</td>",
                    "</tr>"
                )
            }
            
            # Add total row
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
                grand_total, "</td>",
                "</tr>",
                "</tbody>",
                "</table>",
                "</div>"
            )
            
            return(html)
        },

        # Create Bonferroni explanation ----
        .createBonferroniExplanation = function(num_comparisons, alpha = 0.05) {
            adjusted_alpha <- alpha / num_comparisons
            
            html <- paste0(
                "<div style='padding: 15px; background-color: #f8f9fa; border-left: 4px solid #1976d2; margin: 10px 0;'>",
                "<h4 style='color: #1976d2; margin-top: 0;'>Bonferroni Multiple Testing Correction</h4>",
                
                "<p><strong>Why correction is needed:</strong><br>",
                "When performing multiple statistical tests, the chance of finding at least one false positive result increases. ",
                "This is known as the 'multiple comparisons problem.'</p>",
                
                "<p><strong>How Bonferroni correction works:</strong><br>",
                "The Bonferroni method controls the family-wise error rate by adjusting the significance threshold. ",
                "Instead of using α = 0.05 for each test, we use α = 0.05 ÷ number of comparisons.</p>",
                
                "<div style='background-color: #e3f2fd; padding: 10px; border-radius: 3px; margin: 10px 0;'>",
                "<strong>For your analysis:</strong><br>",
                "• Number of pairwise comparisons: <strong>", num_comparisons, "</strong><br>",
                "• Original significance level: <strong>α = 0.05</strong><br>",
                "• Adjusted significance level: <strong>α = ", round(adjusted_alpha, 4), "</strong><br>",
                "• Adjusted p-values = Original p-values × ", num_comparisons,
                "</div>",
                
                "<p><strong>Interpretation:</strong><br>",
                "• <strong>Original p-value:</strong> The probability of observing this result by chance for this specific comparison<br>",
                "• <strong>Adjusted p-value:</strong> The probability adjusted for multiple testing - use this for significance decisions<br>",
                "• A result is significant if the <strong>adjusted p-value < 0.05</strong></p>",
                "</div>"
            )
            
            return(html)
        },

        # Main run function ----
        .run = function() {
            if (is.null(self$options$row) || is.null(self$options$col)) {
                todo <- "
                    <br>Welcome to Enhanced Pairwise Chi-Square Analysis
                    <br><br>
                    This analysis performs comprehensive post-hoc testing by:
                    <ul>
                        <li><strong>Creating individual 2×2 tables</strong> for each pairwise comparison</li>
                        <li><strong>Calculating chi-square tests</strong> for each 2×2 table separately</li>
                        <li><strong>Applying Bonferroni correction</strong> to control family-wise error rate</li>
                        <li><strong>Presenting results</strong> in beautifully formatted HTML tables</li>
                        <li><strong>Providing detailed explanations</strong> of multiple testing correction</li>
                    </ul>
                    <br>
                    <b>What you'll get:</b>
                    <ul>
                        <li>Summary table with all pairwise comparisons and corrected p-values</li>
                        <li>Individual 2×2 contingency tables with percentages and test results</li>
                        <li>Clear significance indicators and explanations</li>
                        <li>Educational content about multiple testing correction</li>
                    </ul>
                    <br>
                    <b>To begin:</b> Select categorical row and column variables (need at least 3 levels in row variable for pairwise comparisons).
                    <hr>
                "
                html <- self$results$todo
                html$setContent(todo)
                return()
            } else {
                html <- self$results$todo
                html$setContent("")
            }

            # Prepare data
            data <- private$.cleanData()
            rowVar <- self$options$row
            colVar <- self$options$col

            # Basic error checking
            if (nrow(data) == 0)
                jmvcore::reject("Data contains no (complete) rows")
            if (nlevels(data[[rowVar]]) < 2)
                jmvcore::reject("Row variable '{}' contains fewer than 2 levels", code='', rowVar)
            if (nlevels(data[[colVar]]) < 2)
                jmvcore::reject("Column variable '{}' contains fewer than 2 levels", code='', colVar)

            # Create contingency table
            result <- table(data[[rowVar]], data[[colVar]])
            
            # Fill main contingency table
            rowLevels <- levels(data[[rowVar]])
            colLevels <- levels(data[[colVar]])
            
            for (i in seq_along(rowLevels)) {
                values <- as.list(result[i,])
                names(values) <- paste0("col_", colLevels)
                values$var1 <- rowLevels[i]
                values$total <- sum(result[i,])
                self$results$conftable$addRow(rowKey=i, values=values)
            }

            # Add total row
            totals <- colSums(result)
            values <- as.list(totals)
            names(values) <- paste0("col_", colLevels)
            values$var1 <- "Total"
            values$total <- sum(totals)
            self$results$conftable$addRow(rowKey=length(rowLevels)+1, values=values)

            # Overall chi-square test
            overall <- suppressWarnings(stats::chisq.test(result, correct = FALSE))
            
            # Add to chi-square results table
            self$results$chi2test$addRow(rowKey=1, values=list(
                statistic=overall$statistic,
                df=overall$parameter,
                p=overall$p.value
            ))

            # Assumptions check
            if (self$options$showAssumptions) {
                expected <- overall$expected
                min_expected <- min(expected)
                cells_less_than_5 <- sum(expected < 5)
                percent_less_than_5 <- cells_less_than_5 / length(expected) * 100

                self$results$assumptions$addRow(rowKey=1, values=list(
                    assumption="Minimum expected count ≥ 5",
                    result=ifelse(min_expected >= 5, "✓ Met", "✗ Not met")
                ))
                self$results$assumptions$addRow(rowKey=2, values=list(
                    assumption="Less than 20% of cells have expected count < 5",
                    result=ifelse(percent_less_than_5 <= 20, "✓ Met", "✗ Not met")
                ))
            }

            # Expected frequencies
            if (self$options$showExpected) {
                expected <- overall$expected
                for (i in seq_along(rowLevels)) {
                    values <- as.list(expected[i,])
                    names(values) <- paste0("exp_", colLevels)
                    values$var1 <- rowLevels[i]
                    self$results$expected$addRow(rowKey=i, values=values)
                }
            }

            # Effect sizes
            if (self$options$effectSize) {
                effect_sizes <- private$.calculateEffectSizes(result)
                
                self$results$effectsize$addRow(rowKey=1, values=list(
                    measure="Cramér's V",
                    value=effect_sizes$cramers_v,
                    interpretation=effect_sizes$v_interpretation
                ))
                
                if (!is.na(effect_sizes$phi)) {
                    self$results$effectsize$addRow(rowKey=2, values=list(
                        measure="Phi coefficient",
                        value=effect_sizes$phi,
                        interpretation=""
                    ))
                }
            }

            # Enhanced Pairwise comparisons with individual 2x2 tables
            if (nrow(result) > 2) {
                results <- list()
                html_tables <- ""
                rowNo <- 1

                for (i in 1:(nrow(result)-1)) {
                    for (j in (i+1):nrow(result)) {
                        subTable <- result[c(i,j),]
                        comparison_name <- paste(rowLevels[i], "vs", rowLevels[j])
                        
                        test <- try(suppressWarnings({
                            chi <- stats::chisq.test(subTable, correct = FALSE)
                            list(
                                comparison = comparison_name,
                                chi_square = chi$statistic,
                                df = chi$parameter,
                                p = chi$p.value,
                                n = sum(subTable),
                                chi_test = chi,
                                table2x2 = subTable
                            )
                        }), silent=TRUE)
                        
                        if (!inherits(test, "try-error"))
                            results[[rowNo]] <- test
                        rowNo <- rowNo + 1
                    }
                }

                # Apply Bonferroni correction and create output
                if (length(results) > 0) {
                    pvals <- sapply(results, function(x) x$p)
                    
                    # Force Bonferroni correction as requested
                    p_adj_bonferroni <- pvals * length(pvals)
                    p_adj_bonferroni[p_adj_bonferroni > 1] <- 1  # Cap at 1.0
                    
                    # Create summary table and individual HTML tables
                    for (i in seq_along(results)) {
                        is_significant <- p_adj_bonferroni[i] < 0.05
                        
                        # Add to summary table
                        significance_text <- if (is_significant) "Significant" else "Not significant"
                        
                        self$results$pairwise1$addRow(rowKey=i, values=list(
                            comparison = results[[i]]$comparison,
                            chi_square = results[[i]]$chi_square,
                            df = results[[i]]$df,
                            p = results[[i]]$p,
                            p_adj = p_adj_bonferroni[i],
                            n = results[[i]]$n,
                            significance = significance_text
                        ))
                        
                        # Create individual HTML table
                        html_tables <- paste0(html_tables, 
                            private$.create2x2HTML(
                                results[[i]]$table2x2,
                                results[[i]]$comparison,
                                results[[i]]$chi_test,
                                p_adj_bonferroni[i],
                                is_significant
                            )
                        )
                    }
                    
                    # Set HTML content for individual tables
                    self$results$pairwiseTables$setContent(html_tables)
                    
                    # Create and set Bonferroni explanation
                    bonferroni_explanation <- private$.createBonferroniExplanation(length(results))
                    self$results$bonferroniExplanation$setContent(bonferroni_explanation)
                }
            } else {
                # If only 2 levels, show a message
                no_pairs_msg <- paste0(
                    "<div style='padding: 15px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 5px;'>",
                    "<strong>Note:</strong> Only 2 levels found in the row variable. ",
                    "Pairwise comparisons require at least 3 levels. ",
                    "The overall chi-square test above shows the association between your variables.",
                    "</div>"
                )
                self$results$pairwiseTables$setContent(no_pairs_msg)
                self$results$bonferroniExplanation$setContent("")
            }

            # Standardized residuals
            if (self$options$showStdRes) {
                stdres <- overall$stdres
                for (i in seq_along(rowLevels)) {
                    values <- as.list(round(stdres[i,], 3))
                    names(values) <- paste0("res_", colLevels)
                    values$var1 <- rowLevels[i]
                    self$results$stdres$addRow(rowKey=i, values=values)
                }
            }

            # Save data for plot
            if (self$options$plots) {
                image <- self$results$plot
                image$setState(list(
                    "conftable" = result,
                    "chi2test" = overall
                ))
            }
        },

        .plot = function(image, ggtheme, theme, ...) {
            if (!self$options$plots)
                return()

            plotData <- image$state
            conftable <- plotData$conftable

            # Create association plot using vcd
            if (requireNamespace("vcd", quietly = TRUE)) {
                assoc <- vcd::assocplot(conftable, shade=TRUE)
                print(assoc)
                return(TRUE)
            } else {
                return(FALSE)
            }
        }
    )
)

