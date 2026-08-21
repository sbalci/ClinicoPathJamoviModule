#' @title Enhanced Two-Way Frequency Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats chisq.test fisher.test
#' @importFrom vcd assocstats
#' @export
#' @return An \code{R6} class generator object for the \code{enhancedtwowayfrequencyClass} backend; used internally by the jamovi analysis wrapper and not called directly.

enhancedtwowayfrequencyClass <- R6::R6Class(
    "enhancedtwowayfrequencyClass",
    inherit = enhancedtwowayfrequencyBase,
    private = list(
        # Declared so runtime assignment does not hit R6's locked-environment guard
        .error_messages = character(0),
        .warning_messages = character(0),

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
                <li><b>Association measures:</b> Cram\u{00E9}r\'s V, Phi coefficient, and contingency coefficient</li>
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

            # Store state for plot renderers
            private$.setPlotStates(mytable, rowVar, colVar)

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
            for (i in seq_len(nrow(mytable))) {
                for (j in seq_len(ncol(mytable))) {
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

                    observed_table$addRow(rowKey = row_idx, values = row_values)
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
                        effect_size <- sprintf("Cram\u{00E9}r's V = %.3f", cramers_v)
                    }

                    test_table$addRow(rowKey = row_idx, values = list(
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

                    test_table$addRow(rowKey = row_idx, values = list(
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

            assoc_table$addRow(rowKey = row_idx, values = list(
                measure = "Cram\u{00E9}r's V",
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

                assoc_table$addRow(rowKey = row_idx, values = list(
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

            assoc_table$addRow(rowKey = row_idx, values = list(
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
        },

        .escapeHtml = function(x) {
            x <- as.character(x)
            x <- gsub("&", "&amp;", x, fixed = TRUE)
            x <- gsub("<", "&lt;", x, fixed = TRUE)
            x <- gsub(">", "&gt;", x, fixed = TRUE)
            x <- gsub("\"", "&quot;", x, fixed = TRUE)
            x <- gsub("'", "&#39;", x, fixed = TRUE)
            x
        },

        .performAssumptionChecks = function(mytable) {
            assumption_table <- self$results$assumptionCheck
            min_threshold <- self$options$minimumExpected

            chi_result <- tryCatch(suppressWarnings(chisq.test(mytable)),
                                   error = function(e) NULL)

            if (!is.null(chi_result)) {
                expected <- chi_result$expected
                total_cells <- length(expected)
                min_expected <- min(expected)
                n_below <- sum(expected < min_threshold)
                pct_below5 <- 100 * sum(expected < 5) / total_cells

                assumption_table$addRow(rowKey = "min_expected", values = list(
                    assumption = paste0("Minimum expected frequency >= ", min_threshold),
                    status = if (min_expected >= min_threshold) "Met" else "Violated",
                    details = sprintf("Smallest expected count = %.2f; %d of %d cells below threshold",
                                      min_expected, n_below, total_cells),
                    recommendation = if (min_expected >= min_threshold)
                        "Chi-square approximation is appropriate."
                    else
                        "Consider Fisher's exact test or collapsing sparse categories."
                ))

                assumption_table$addRow(rowKey = "cochran", values = list(
                    assumption = "Cochran's rule (>= 80% of cells with expected >= 5)",
                    status = if (pct_below5 <= 20) "Met" else "Violated",
                    details = sprintf("%.1f%% of cells have expected count < 5", pct_below5),
                    recommendation = if (pct_below5 <= 20)
                        "Large-sample assumptions satisfied."
                    else
                        "Use an exact test; asymptotic p-values may be unreliable."
                ))
            }

            assumption_table$addRow(rowKey = "independence", values = list(
                assumption = "Independence of observations",
                status = "Assumed",
                details = sprintf("Total observations analysed: %d", sum(mytable)),
                recommendation = "Each observation must contribute to exactly one cell."
            ))
        },

        .generateDiagnostics = function(complete_data, mytable, rowVar, colVar) {
            diag_table <- self$results$diagnostics
            n_total <- nrow(self$data)
            n_complete <- nrow(complete_data)
            n_missing <- n_total - n_complete

            diag_table$addRow(rowKey = "sample", values = list(
                diagnostic_item = "Sample size",
                value = as.character(n_complete),
                interpretation = sprintf("%d complete cases of %d total", n_complete, n_total),
                quality_flag = if (n_complete >= 30) "Adequate" else "Small sample"
            ))

            diag_table$addRow(rowKey = "missing", values = list(
                diagnostic_item = "Missing cases",
                value = as.character(n_missing),
                interpretation = sprintf("%.1f%% of rows excluded",
                                         if (n_total > 0) 100 * n_missing / n_total else 0),
                quality_flag = if (n_total > 0 && n_missing / n_total > 0.1)
                    "High missingness" else "Acceptable"
            ))

            diag_table$addRow(rowKey = "dims", values = list(
                diagnostic_item = "Table dimensions",
                value = sprintf("%d x %d", nrow(mytable), ncol(mytable)),
                interpretation = sprintf("%d row levels, %d column levels",
                                         nrow(mytable), ncol(mytable)),
                quality_flag = if (any(dim(mytable) < 2)) "Degenerate" else "Valid"
            ))

            n_empty <- sum(mytable == 0)
            diag_table$addRow(rowKey = "empty", values = list(
                diagnostic_item = "Empty cells",
                value = as.character(n_empty),
                interpretation = sprintf("%d of %d cells have zero count",
                                         n_empty, length(mytable)),
                quality_flag = if (n_empty == 0) "None" else "Sparse cells present"
            ))
        },

        .generateComprehensiveSummary = function(mytable, complete_data) {
            summary_table <- self$results$comprehensiveAnalysisSummary
            n <- sum(mytable)
            cramers_v <- tryCatch(private$.calculateCramersV(mytable),
                                  error = function(e) NA_real_)
            chi <- tryCatch(suppressWarnings(chisq.test(mytable)),
                            error = function(e) NULL)

            summary_table$addRow(rowKey = "n", values = list(
                measure = "Total observations",
                value = as.character(n),
                interpretation = "Cases contributing to the cross-tabulation",
                clinical_significance = if (n >= 30)
                    "Sufficient for inference" else "Interpret with caution"
            ))

            if (!is.null(chi)) {
                summary_table$addRow(rowKey = "chi", values = list(
                    measure = "Pearson chi-square",
                    value = sprintf("%.3f (df = %d)", chi$statistic, chi$parameter),
                    interpretation = sprintf("p = %.4f", chi$p.value),
                    clinical_significance = if (chi$p.value < 0.05)
                        "Statistically significant association"
                    else
                        "No significant association"
                ))
            }

            if (!is.na(cramers_v)) {
                interp <- private$.interpretCramersV(cramers_v)
                summary_table$addRow(rowKey = "cramers", values = list(
                    measure = "Cram\u{00E9}r's V",
                    value = sprintf("%.3f", cramers_v),
                    interpretation = interp$interpretation,
                    clinical_significance = interp$clinical
                ))
            }
        },

        .generateRecommendations = function(mytable) {
            html <- self$results$recommendations
            recs <- character(0)

            chi <- tryCatch(suppressWarnings(chisq.test(mytable)),
                            error = function(e) NULL)
            if (!is.null(chi)) {
                min_exp <- min(chi$expected)
                if (min_exp < self$options$minimumExpected)
                    recs <- c(recs, sprintf(
                        "Smallest expected frequency (%.2f) is below %g; prefer Fisher's exact test.",
                        min_exp, self$options$minimumExpected))
                if (chi$p.value < 0.05)
                    recs <- c(recs, "The association is statistically significant; report an effect size (Cram\u{00E9}r's V / Phi) alongside the p-value.")
                else
                    recs <- c(recs, "No statistically significant association was detected; consider statistical power and sample size before concluding independence.")
            }
            if (nrow(mytable) == 2 && ncol(mytable) == 2 && !self$options$continuityCorrection)
                recs <- c(recs, "For a 2x2 table, Yates' continuity correction is commonly recommended.")
            if (length(recs) == 0)
                recs <- "No specific recommendations; assumptions appear to be met."

            items <- paste0("<li>", recs, "</li>", collapse = "")
            html$setContent(paste0(
                "<div style='padding:8px;'><h3>Statistical Recommendations</h3><ul>",
                items, "</ul></div>"))
        },

        .generateClinicalInterpretation = function(mytable) {
            html <- self$results$clinicalInterpretationGuide
            rowVar <- private$.escapeHtml(self$options$rowVar)
            colVar <- private$.escapeHtml(self$options$colVar)

            cramers_v <- tryCatch(private$.calculateCramersV(mytable),
                                  error = function(e) NA_real_)
            chi <- tryCatch(suppressWarnings(chisq.test(mytable)),
                            error = function(e) NULL)

            assoc_text <- if (!is.na(cramers_v)) {
                interp <- private$.interpretCramersV(cramers_v)
                sprintf("The strength of association between <b>%s</b> and <b>%s</b> is <b>%s</b> (Cram\u{00E9}r's V = %.3f).",
                        rowVar, colVar, tolower(interp$interpretation), cramers_v)
            } else {
                sprintf("Association between <b>%s</b> and <b>%s</b> could not be quantified.",
                        rowVar, colVar)
            }

            sig_text <- if (!is.null(chi)) {
                if (chi$p.value < 0.05)
                    "The chi-square test indicates a statistically significant relationship, suggesting the two variables are not independent."
                else
                    "The chi-square test does not provide evidence of a relationship between the variables."
            } else ""

            html$setContent(paste0(
                "<div style='padding:8px;'>",
                "<h3>Clinical Application Guidance</h3>",
                "<p>", assoc_text, "</p>",
                "<p>", sig_text, "</p>",
                "<p><i>Note:</i> Statistical association does not imply causation. Interpret findings in the context of study design, potential confounders, and clinical plausibility.</p>",
                "</div>"))
        },

        .generateMethodsExplanation = function() {
            html <- self$results$methodsExplanation
            html$setContent(
                "<div style='padding:8px;'>
                <h3>Statistical Methods and References</h3>
                <ul>
                <li><b>Pearson chi-square test:</b> Tests independence of two categorical variables using observed versus expected frequencies under independence.</li>
                <li><b>Fisher's exact test:</b> Exact test of association, recommended when expected cell counts are small.</li>
                <li><b>Yates' continuity correction:</b> Adjustment applied to 2x2 chi-square tests to reduce approximation error.</li>
                <li><b>Cram\u{00E9}r's V:</b> Effect size for association, ranging 0 to 1, derived from the chi-square statistic.</li>
                <li><b>Phi coefficient:</b> Association measure for 2x2 tables.</li>
                <li><b>Contingency coefficient:</b> General association measure derived from chi-square.</li>
                <li><b>Standardized residuals:</b> Identify the cells contributing most to a significant chi-square result.</li>
                </ul>
                <p><b>Reference:</b> Agresti A (2018) <i>An Introduction to Categorical Data Analysis</i>, 3rd ed., Wiley.</p>
                </div>")
        },

        .setPlotStates = function(mytable, rowVar, colVar) {
            state <- list(
                counts = matrix(as.integer(mytable),
                                nrow = nrow(mytable),
                                dimnames = dimnames(mytable)),
                rowVar = rowVar,
                colVar = colVar)
            if (self$options$heatmapPlot)
                self$results$heatmapPlot$setState(state)
            if (self$options$mosaicPlot)
                self$results$mosaicPlot$setState(state)
            if (self$options$residualAnalysis && self$options$detailedOutput)
                self$results$residualPlot$setState(state)
        },

        .plotHeatmap = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) return(FALSE)
            st <- image$state
            df <- as.data.frame(as.table(st$counts), stringsAsFactors = FALSE)
            names(df) <- c("Row", "Col", "Count")

            p <- ggplot2::ggplot(df, ggplot2::aes(x = Col, y = Row, fill = Count)) +
                ggplot2::geom_tile(color = "white") +
                ggplot2::geom_text(ggplot2::aes(label = Count)) +
                ggplot2::scale_fill_gradient(low = "#deebf7", high = "#08519c") +
                ggplot2::labs(x = st$colVar, y = st$rowVar, title = "Frequency Heatmap") +
                ggtheme
            print(p)
            TRUE
        },

        .plotMosaic = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) return(FALSE)
            st <- image$state
            tbl <- as.table(st$counts)
            names(dimnames(tbl)) <- c(st$rowVar, st$colVar)
            graphics::mosaicplot(tbl, main = "Mosaic Plot", color = TRUE,
                                 xlab = st$rowVar, ylab = st$colVar, las = 1)
            TRUE
        },

        .plotResiduals = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) return(FALSE)
            st <- image$state
            chi <- tryCatch(suppressWarnings(chisq.test(st$counts)),
                            error = function(e) NULL)
            if (is.null(chi)) return(FALSE)
            df <- as.data.frame(as.table(chi$stdres), stringsAsFactors = FALSE)
            names(df) <- c("Row", "Col", "Residual")

            p <- ggplot2::ggplot(df, ggplot2::aes(x = Col, y = Row, fill = Residual)) +
                ggplot2::geom_tile(color = "white") +
                ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Residual))) +
                ggplot2::scale_fill_gradient2(low = "#b2182b", mid = "white",
                                              high = "#2166ac", midpoint = 0) +
                ggplot2::labs(x = st$colVar, y = st$rowVar,
                              title = "Standardized Residuals") +
                ggtheme
            print(p)
            TRUE
        }
    )
)
