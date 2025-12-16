#' @title Dot Chart
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#' @import ggplot2
#' @importFrom rlang sym
#' @importFrom digest digest
#'


jjdotplotstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjdotplotstatsClass",
    inherit = jjdotplotstatsBase,
    private = list(
        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .processedOptions = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .messages = NULL,
        .currentPreset = "basic",

        # Variable name escaping for special characters
        .escapeVar = function(var) {
            if (is.null(var)) return(NULL)
            # Use backticks for variables with spaces/special chars
            if (grepl("[^A-Za-z0-9_\\.]", var)) {
                return(paste0("`", var, "`"))
            }
            return(var)
        },

        # init ----

        .init = function() {
            # Apply clinical presets if not custom
            private$.applyClinicalPreset()
            
            # Since dep is single variable, use fixed size
            # Use configurable plot dimensions
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 650
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450
            
            self$results$plot$setSize(plotwidth, plotheight)


            if (!is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                num_levels <- nlevels(
                    as.factor(mydata[[grvar]])
                )

                self$results$plot2$setSize(num_levels * plotwidth, plotheight)

            }

        }


,
        # Shared validation helper
        .validateInputs = function() {
            if (is.null(self$options$dep) || is.null(self$options$group))
                return(FALSE)

            if (nrow(self$data) == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'noCompleteRows',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Data contains no complete rows. Please check for missing values in your selected variables.')
                self$results$insert(1, notice)
                return(FALSE)
            }

            # Check variable existence with better context
            if (!(self$options$dep %in% names(self$data))) {
                available_vars <- paste(names(self$data), collapse=", ")
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'depVarNotFound',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(sprintf('Variable "%s" not found in data. Available variables: %s. Please select a valid continuous variable for the dependent variable.', self$options$dep, available_vars))
                self$results$insert(1, notice)
                return(FALSE)
            }

            if (!(self$options$group %in% names(self$data))) {
                available_vars <- paste(names(self$data), collapse=", ")
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'groupVarNotFound',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(sprintf('Variable "%s" not found in data. Available variables: %s. Please select a valid grouping variable.', self$options$group, available_vars))
                self$results$insert(1, notice)
                return(FALSE)
            }

            # Require at least two groups with complete data
            relevant_cols <- c(self$options$dep, self$options$group)
            if (!is.null(self$options$grvar))
                relevant_cols <- c(relevant_cols, self$options$grvar)
            complete_rows <- complete.cases(self$data[relevant_cols])
            group_levels <- nlevels(droplevels(as.factor(self$data[[self$options$group]][complete_rows])))
            if (group_levels < 2) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'insufficientGroups',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(sprintf('At least two groups with data are required for comparison. Found %d group(s) with complete data. Please check for missing values or select different variables.', group_levels))
                self$results$insert(1, notice)
                return(FALSE)
            }

            # Check total sample size
            n_total <- sum(complete_rows)
            if (n_total < 30) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'smallSampleSize',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                notice$setContent(sprintf('Small total sample size (N = %d). Statistical tests may be unreliable with N < 30. Consider interpreting results cautiously or collecting more data.', n_total))
                self$results$insert(1, notice)
            }

            # Check minimum group size
            group_data <- self$data[[self$options$group]][complete_rows]
            group_sizes <- table(droplevels(as.factor(group_data)))
            min_group_n <- min(group_sizes)
            if (min_group_n < 10) {
                min_group_name <- names(which.min(group_sizes))
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'verySmallGroups',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                notice$setContent(sprintf('Very small group sizes detected (minimum n = %d in group "%s"). Groups with n < 10 may produce unreliable test results. Consider combining groups or collecting more data.', min_group_n, min_group_name))
                self$results$insert(1, notice)
            }

            # Validate centrality parameter consistency
            private$.validateCentralityOptions()

            return(TRUE)
        },
        
        # Centrality parameter validation helper
        .validateCentralityOptions = function() {
            if (self$options$centralityparameter == "none" && self$options$centralityk != 2) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'centralityKUnused',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent('Centrality decimal places specified but centrality parameter is "none". The precision setting will have no effect.')
                self$results$insert(999, notice)
            }

            if (self$options$centralityplotting && self$options$centralityparameter == "none") {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'centralityPlotMismatch',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent('Centrality plotting enabled but centrality parameter is "none". No centrality lines will be displayed.')
                self$results$insert(1, notice)
            }

            if (!self$options$centralityplotting && self$options$centralitytype != "parametric") {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'centralityTypeUnused',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent('Centrality type specified but centrality plotting is disabled. The type setting will have no effect.')
                self$results$insert(999, notice)
            }
        },
        
        # Message accumulation helper
        .accumulateMessage = function(message) {
            if (is.null(private$.messages)) {
                private$.messages <- character()
            }
            private$.messages <- append(private$.messages, message)
            self$results$todo$setContent(paste(private$.messages, collapse = ""))
        },
        
        # Clinical interpretation helper
        .generateClinicalInterpretation = function(mydata) {
            dep_var <- self$options$dep
            group_var <- self$options$group
            test_type <- self$options$typestatistics
            preset <- private$.currentPreset
            
            tryCatch({
                # Calculate basic descriptive statistics
                group_means <- tapply(mydata[[dep_var]], mydata[[group_var]], mean, na.rm = TRUE)
                group_medians <- tapply(mydata[[dep_var]], mydata[[group_var]], median, na.rm = TRUE)
                group_ns <- table(mydata[[group_var]])

                groups <- names(group_means)
                n_groups <- length(groups)

                # CORRECTLY IDENTIFY ANALYSIS TYPE based on number of groups
                test_description <- if (n_groups == 2) {
                    switch(test_type,
                        "parametric" = "independent samples t-test for comparing means",
                        "nonparametric" = "Mann-Whitney U test for comparing distributions",
                        "robust" = "Yuen's test using trimmed means",
                        "bayes" = "Bayesian t-test for group comparison",
                        "two-group comparison"
                    )
                } else {
                    switch(test_type,
                        "parametric" = "one-way ANOVA for comparing means across groups",
                        "nonparametric" = "Kruskal-Wallis test for comparing distributions across groups",
                        "robust" = "robust ANOVA using trimmed means",
                        "bayes" = "Bayesian ANOVA for group comparison",
                        "multi-group comparison"
                    )
                }
                
                # Generate sample description based on number of groups
                sample_desc <- if (n_groups == 2) {
                    paste0("Group '", groups[1], "' (n=", group_ns[groups[1]], ") vs ",
                           "Group '", groups[2], "' (n=", group_ns[groups[2]], ")")
                } else {
                    paste0(n_groups, " groups: ",
                           paste(sapply(names(group_ns), function(g) paste0("'", g, "' (n=", group_ns[g], ")")),
                                collapse = ", "))
                }

                # Generate results description based on number of groups
                results_desc <- if (n_groups == 2) {
                    paste0("Group '", groups[1], "' shows a ",
                           switch(test_type,
                                 "parametric" = paste0("mean of ", round(group_means[groups[1]], 2)),
                                 "nonparametric" = paste0("median of ", round(group_medians[groups[1]], 2)),
                                 paste0("central value of ", round(group_means[groups[1]], 2))),
                           " vs Group '", groups[2], "' with a ",
                           switch(test_type,
                                 "parametric" = paste0("mean of ", round(group_means[groups[2]], 2)),
                                 "nonparametric" = paste0("median of ", round(group_medians[groups[2]], 2)),
                                 paste0("central value of ", round(group_means[groups[2]], 2))),
                           ".")
                } else {
                    central_measure <- if (test_type %in% c("parametric", "bayes")) "means" else "medians"
                    central_values <- if (test_type %in% c("parametric", "bayes")) group_means else group_medians
                    paste0("The ", n_groups, " groups show ", central_measure, " ranging from ",
                           round(min(central_values, na.rm = TRUE), 2), " to ",
                           round(max(central_values, na.rm = TRUE), 2), ". ",
                           "The plot visualizes the complete distribution across all groups.")
                }

                interpretation <- glue::glue(
                    "<div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 10px 0;'>",
                    "<h4 style='color: #007bff; margin-top: 0;'>📊 Clinical Interpretation</h4>",
                    "<p><strong>Analysis:</strong> This dot plot shows the distribution of {dep_var} across {n_groups} {group_var} ",
                    if (n_groups == 2) "groups" else "groups",
                    " using a {test_description}.</p>",
                    "<p><strong>Sample:</strong> {sample_desc}</p>",
                    "<p><strong>Results:</strong> {results_desc}</p>",
                    "<p><em>💡 Tip: The statistical significance and effect size will be displayed in the plot subtitle when the analysis completes.</em></p>",
                    "</div>"
                )
                
                self$results$interpretation$setContent(interpretation)
            }, error = function(e) {
                # Provide basic interpretation if detailed fails
                basic_interp <- glue::glue(
                    "<div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff;'>",
                    "<h4 style='color: #007bff; margin-top: 0;'>📊 Clinical Interpretation</h4>",
                    "<p>This analysis compares {dep_var} across {group_var} groups using dot plots.</p>",
                    "<p>The plot will show the distribution and statistical comparison when completed.</p>",
                    "</div>"
                )
                self$results$interpretation$setContent(basic_interp)
            })
        },
        
        # Assumption checking helper  
        .checkAssumptions = function(mydata) {
            dep_var <- self$options$dep
            group_var <- self$options$group
            
            tryCatch({
                # Basic sample size check
                group_counts <- table(mydata[[group_var]])
                min_n <- min(group_counts)
                
                # Check for sufficient sample size
                sample_warning <- if(min_n < 30) {
                    if(min_n < 10) {
                        "⚠️ <strong>Small sample sizes</strong> (n < 10 in some groups). Consider descriptive analysis only."
                    } else {
                        "ℹ️ <strong>Moderate sample sizes</strong> (n < 30). Non-parametric tests may be more appropriate."
                    }
                } else {
                    "✓ <strong>Adequate sample sizes</strong> for statistical testing."
                }
                
                # Check data distribution characteristics
                numeric_data <- jmvcore::toNumeric(mydata[[dep_var]])

                # Calculate proper skewness with fallback
                if (requireNamespace("e1071", quietly = TRUE)) {
                    # Use proper skewness (standardized third moment)
                    skewness_rough <- tryCatch({
                        e1071::skewness(numeric_data, na.rm = TRUE, type = 2)  # Type 2 is SAS/SPSS method
                    }, error = function(e) {
                        # Fallback to approximation if error
                        (mean(numeric_data, na.rm = TRUE) - median(numeric_data, na.rm = TRUE)) / sd(numeric_data, na.rm = TRUE)
                    })
                } else {
                    # Fallback to approximation if e1071 not available
                    skewness_rough <- (mean(numeric_data, na.rm = TRUE) - median(numeric_data, na.rm = TRUE)) / sd(numeric_data, na.rm = TRUE)
                }
                
                distribution_note <- if(abs(skewness_rough) > 1) {
                    "ℹ️ <strong>Skewed distribution</strong> detected. Non-parametric tests recommended."
                } else if(abs(skewness_rough) > 0.5) {
                    "ℹ️ <strong>Moderately skewed</strong> distribution. Consider robust or non-parametric methods."
                } else {
                    "✓ <strong>Approximately normal</strong> distribution suitable for parametric tests."
                }
                
                # Generate recommendations
                test_recommendation <- switch(self$options$typestatistics,
                    "parametric" = if(abs(skewness_rough) > 1 || min_n < 10) {
                        # Add Notice for clinical safety
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'testDataMismatch',
                            type = jmvcore::NoticeType$STRONG_WARNING
                        )
                        notice$setContent(sprintf('Parametric test selected but data shows high skewness (%.2f) and/or small sample sizes (minimum n = %d). Parametric tests assume normality. Consider switching to nonparametric test (Mann-Whitney/Kruskal-Wallis) for more reliable results.', skewness_rough, min_n))
                        self$results$insert(1, notice)
                        "💡 <strong>Recommendation:</strong> Consider switching to non-parametric test due to distribution or sample size."
                    } else {
                        "✓ <strong>Parametric test</strong> is appropriate for your data."
                    },
                    "nonparametric" = "✓ <strong>Non-parametric test</strong> is robust and suitable for most data types.",
                    "robust" = "✓ <strong>Robust test</strong> handles outliers and non-normal distributions well.",
                    "bayes" = "✓ <strong>Bayesian approach</strong> provides evidence strength regardless of distribution.",
                    "Current test selection is reasonable."
                )
                
                assumptions_content <- glue::glue(
                    "<div style='background-color: #fff3cd; padding: 15px; border-left: 4px solid #ffc107; margin: 10px 0;'>",
                    "<h4 style='color: #856404; margin-top: 0;'>🔍 Data Assessment & Recommendations</h4>",
                    "<p>{sample_warning}</p>",
                    "<p>{distribution_note}</p>",
                    "<p>{test_recommendation}</p>",
                    "<hr style='border-color: #ffeaa7;'>",
                    "<p><strong>Sample sizes by group:</strong><br>",
                    paste(names(group_counts), ": n =", group_counts, collapse = "<br>"),
                    "</p>",
                    "</div>"
                )
                
                self$results$assumptions$setContent(assumptions_content)
            }, error = function(e) {
                basic_assumptions <- glue::glue(
                    "<div style='background-color: #fff3cd; padding: 15px; border-left: 4px solid #ffc107;'>",
                    "<h4 style='color: #856404; margin-top: 0;'>🔍 Data Assessment</h4>",
                    "<p>Evaluating data characteristics for appropriate statistical testing...</p>",
                    "</div>"
                )
                self$results$assumptions$setContent(basic_assumptions)
            })
        },
        
        # Clinical preset application
        .applyClinicalPreset = function() {
            preset <- self$options$clinicalPreset
            if (is.null(preset) || preset == "custom") {
                private$.currentPreset <- "custom"
                private$.accumulateMessage("<br>📋 Using custom analysis settings.<br>")
                return()
            }

            private$.currentPreset <- preset
            preset_message <- ""

            # Apply settings based on preset
            if (preset == "basic") {
                preset_message <- "Using basic analysis settings optimized for straightforward comparisons."
            } else if (preset == "publication") {
                preset_message <- "Using publication-ready settings with comprehensive statistical reporting."
            } else if (preset == "clinical") {
                preset_message <- "Using clinical settings optimized for medical decision-making."
            }

            if (nchar(preset_message) > 0) {
                private$.accumulateMessage(glue::glue("<br>📋 {preset_message}<br>"))
            }
        },
        
        # Report sentence generator
        .generateReportSentence = function() {
            dep_var <- self$options$dep
            group_var <- self$options$group
            test_type <- self$options$typestatistics

            # Determine number of groups from data
            mydata <- private$.prepareData()
            if (is.null(mydata) || is.null(group_var) || !(group_var %in% names(mydata))) {
                return()  # Cannot generate report without data
            }

            n_groups <- length(unique(mydata[[group_var]]))

            # CORRECTLY IDENTIFY TEST NAME based on number of groups
            test_name <- if (n_groups == 2) {
                switch(test_type,
                    "parametric" = "independent samples t-test",
                    "nonparametric" = "Mann-Whitney U test",
                    "robust" = "Yuen's robust test for trimmed means",
                    "bayes" = "Bayesian t-test",
                    "two-group comparison test"
                )
            } else {
                switch(test_type,
                    "parametric" = "one-way analysis of variance (ANOVA)",
                    "nonparametric" = "Kruskal-Wallis H test",
                    "robust" = "robust one-way ANOVA",
                    "bayes" = "Bayesian ANOVA",
                    "multi-group comparison test"
                )
            }

            comparison_phrase <- if (n_groups == 2) {
                "between two groups"
            } else {
                paste0("across ", n_groups, " groups")
            }

            report_template <- glue::glue(
                "<div style='background-color: #e7f3ff; padding: 15px; border-left: 4px solid #0066cc; margin: 10px 0;'>",
                "<h4 style='color: #0066cc; margin-top: 0;'>📝 Copy-Ready Report Sentence</h4>",
                "<div style='background-color: white; padding: 10px; border: 1px dashed #0066cc; font-family: \"Times New Roman\", serif;'>",
                "<p>A <strong>{test_name}</strong> was performed to compare <em>{dep_var}</em> {comparison_phrase} of <em>{group_var}</em>. ",
                "The dot plot visualization shows the distribution and central tendencies across groups, ",
                "with statistical results displayed in the plot subtitle including effect size and significance testing.</p>",
                "</div>",
                "<p><em>💡 Click to select the text above and copy to your report. Statistical values will be automatically filled when the analysis completes.</em></p>",
                "</div>"
            )

            self$results$reportSentence$setContent(report_template)
        },
        
        # Guided steps generator
        .generateGuidedSteps = function() {
            if (!self$options$guidedMode) return()
            
            steps <- glue::glue(
                "<div style='background-color: #e8f5e8; padding: 15px; border-left: 4px solid #28a745; margin: 10px 0;'>",
                "<h4 style='color: #155724; margin-top: 0;'>🎯 Analysis Steps</h4>",
                "<ol style='margin: 10px 0; padding-left: 20px;'>",
                "<li><strong>Data Selection:</strong> Choose continuous variable and grouping variable</li>",
                "<li><strong>Test Selection:</strong> Review data assessment recommendations above</li>",
                "<li><strong>Options:</strong> Configure display and statistical options</li>",
                "<li><strong>Interpretation:</strong> Review clinical interpretation and assumptions</li>",
                "<li><strong>Report:</strong> Copy report template for documentation</li>",
                "</ol>",
                "<p><em>💡 Tip: Follow these steps in order for best results. Check the Data Assessment panel for recommendations.</em></p>",
                "</div>"
            )
            
            self$results$guidedSteps$setContent(steps)
        },
        
        # Next steps recommendations
        .generateRecommendations = function() {
            if (!self$options$guidedMode) return()
            
            preset <- private$.currentPreset
            
            recommendations <- switch(preset,
                "publication" = glue::glue(
                    "<div style='background-color: #fff8e1; padding: 15px; border-left: 4px solid #ffc107; margin: 10px 0;'>",
                    "<h4 style='color: #856404; margin-top: 0;'>📚 Publication Checklist</h4>",
                    "<ul style='margin: 10px 0; padding-left: 20px;'>",
                    "<li>✓ Report effect size with confidence intervals</li>",
                    "<li>✓ Include assumption checking results</li>",
                    "<li>✓ State statistical test used and why</li>",
                    "<li>✓ Report exact p-values (not just p < 0.05)</li>",
                    "<li>✓ Consider multiple testing corrections if applicable</li>",
                    "</ul>",
                    "</div>"
                ),
                "clinical" = glue::glue(
                    "<div style='background-color: #e3f2fd; padding: 15px; border-left: 4px solid #2196f3; margin: 10px 0;'>",
                    "<h4 style='color: #1976d2; margin-top: 0;'>🏥 Clinical Decision Points</h4>",
                    "<ul style='margin: 10px 0; padding-left: 20px;'>",
                    "<li>Consider clinical significance vs. statistical significance</li>",
                    "<li>Evaluate practical impact of observed differences</li>",
                    "<li>Review sample representativeness for your population</li>",
                    "<li>Consider confounding variables not in this analysis</li>",
                    "<li>Discuss findings with clinical colleagues</li>",
                    "</ul>",
                    "</div>"
                ),
                glue::glue(
                    "<div style='background-color: #f3e5f5; padding: 15px; border-left: 4px solid #9c27b0; margin: 10px 0;'>",
                    "<h4 style='color: #7b1fa2; margin-top: 0;'>🔍 Next Steps</h4>",
                    "<ul style='margin: 10px 0; padding-left: 20px;'>",
                    "<li>Review the statistical assumptions above</li>",
                    "<li>Consider additional analyses if needed</li>",
                    "<li>Document your methods and findings</li>",
                    "<li>Consider replication with independent data</li>",
                    "</ul>",
                    "</div>"
                )
            )
            
            self$results$recommendations$setContent(recommendations)
        },
        
        # Data quality validation helper
        .validateDataQuality = function(mydata, dep_var) {
            num_vals <- jmvcore::toNumeric(mydata[[dep_var]])
            num_vals <- num_vals[!is.na(num_vals)]
            
            if (length(num_vals) < 3) {
                private$.accumulateMessage(
                    glue::glue("<br>⚠️ Warning: {dep_var} has less than 3 valid observations<br>")
                )
            }
            if (length(unique(num_vals)) < 2) {
                private$.accumulateMessage(
                    glue::glue("<br>⚠️ Warning: {dep_var} has no variation (all values are the same)<br>")
                )
            }
        },
        
        # Outlier detection helper
        .detectOutliers = function(data, var) {
            vals <- jmvcore::toNumeric(data[[var]])
            vals <- vals[!is.na(vals)]
            if (length(vals) > 0) {
                # Checkpoint before expensive quantile calculations
                private$.checkpoint()
                Q1 <- quantile(vals, 0.25, na.rm = TRUE)
                Q3 <- quantile(vals, 0.75, na.rm = TRUE)
                IQR <- Q3 - Q1
                outliers <- which(data[[var]] < (Q1 - 1.5 * IQR) | data[[var]] > (Q3 + 1.5 * IQR))
                if (length(outliers) > 0) {
                    private$.accumulateMessage(
                        glue::glue("<br>ℹ️ {length(outliers)} potential outlier(s) detected in {var}<br>")
                    )
                }
            }
        },
        
        # Statistical summary helper
        .addDataSummary = function(data, dep_var, group_var) {
            if (!is.null(dep_var) && !is.null(group_var)) {
                tryCatch({
                    # Checkpoint before expensive tapply operation
                    private$.checkpoint()
                    summary_stats <- tapply(data[[dep_var]], data[[group_var]], 
                                           function(x) c(mean = mean(x, na.rm = TRUE), 
                                                        n = sum(!is.na(x))))
                    n_groups <- length(summary_stats)
                    total_n <- sum(sapply(summary_stats, function(x) x["n"]), na.rm = TRUE)
                    private$.accumulateMessage(
                        glue::glue("<br>📊 Analysis summary: {n_groups} groups, {total_n} total observations<br>")
                    )
                }, error = function(e) {
                    # Silently handle errors in summary calculation
                })
            }
        },

        # Optimized data preparation with robust caching
        .prepareData = function(force_refresh = FALSE) {
            # Create robust hash of current data to detect changes
            current_hash <- digest::digest(list(
                dep = self$options$dep,
                group = self$options$group,
                data_dim = dim(self$data),
                col_names = names(self$data),
                grvar = self$options$grvar
            ), algo = "md5")
            
            # Only reprocess if data has changed or forced refresh
            if (!is.null(private$.processedData) && 
                private$.data_hash == current_hash && 
                !force_refresh) {
                return(private$.processedData)
            }

            # Clear previous messages and add processing feedback
            private$.messages <- NULL
            private$.accumulateMessage(
                glue::glue("<br>Processing data for dot plot analysis...<br><hr>")
            )
            
            # Track processing time for large datasets
            start_time <- Sys.time()

            mydata <- self$data

            # Convert dependent variable to numeric (single variable)
            dep_var <- self$options$dep
            if (!is.null(dep_var)) {
                mydata[[dep_var]] <- jmvcore::toNumeric(mydata[[dep_var]])
            }

            # SELECTIVE NA OMISSION - only remove rows with NAs in analysis variables
            # This prevents dropping patients with NAs in unused columns
            if (!is.null(dep_var) && !is.null(self$options$group)) {
                relevant_cols <- c(dep_var, self$options$group)

                # Add grouping variable if present
                if (!is.null(self$options$grvar)) {
                    relevant_cols <- c(relevant_cols, self$options$grvar)
                }

                private$.checkpoint()

                # Count rows before and after NA removal
                n_before <- nrow(mydata)
                mydata <- mydata[complete.cases(mydata[relevant_cols]), ]
                n_after <- nrow(mydata)

                # Report NA removal if any occurred
                if (n_before > n_after) {
                    n_dropped <- n_before - n_after
                    private$.accumulateMessage(
                        glue::glue("<br>ℹ️ Info: {n_dropped} rows excluded due to missing values in analysis variables.<br>",
                                  "Rows with data: {n_after} of {n_before} ({round(100 * n_after / n_before, 1)}%)<br>")
                    )
                }
            }
            
            # Validate data quality
            if (!is.null(dep_var)) {
                private$.validateDataQuality(mydata, dep_var)
            }
            
            # Detect outliers for datasets with sufficient size
            if (nrow(mydata) > 10 && !is.null(dep_var)) {
                private$.detectOutliers(mydata, dep_var)
            }
            
            # Add statistical summary
            private$.addDataSummary(mydata, dep_var, self$options$group)
            
            # Add processing time feedback for large datasets
            elapsed <- difftime(Sys.time(), start_time, units = "secs")
            if (nrow(mydata) > 1000) {
                private$.accumulateMessage(
                    glue::glue("<br>✅ Large dataset processed in {round(elapsed, 2)} seconds<br>")
                )
            }

            # Cache the processed data with hash
            private$.processedData <- mydata
            private$.data_hash <- current_hash
            return(mydata)
        },

        # Optimized options preparation with robust caching
        .prepareOptions = function(force_refresh = FALSE) {
            # Create robust hash of current options to detect changes
            current_options_hash <- digest::digest(list(
                dep = self$options$dep,
                group = self$options$group,
                grvar = self$options$grvar,
                typestatistics = self$options$typestatistics,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                testvalue = self$options$testvalue,
                bfmessage = self$options$bfmessage,
                conflevel = self$options$conflevel,
                k = self$options$k,
                testvalueline = self$options$testvalueline,
                centralityparameter = self$options$centralityparameter,
                centralityk = self$options$centralityk,
                titles = list(self$options$mytitle, self$options$xtitle, self$options$ytitle),
                display = list(self$options$resultssubtitle, self$options$originaltheme)
            ), algo = "md5")
            
            # Only reprocess if options have changed or forced refresh
            if (!is.null(private$.processedOptions) && 
                private$.options_hash == current_options_hash && 
                !force_refresh) {
                return(private$.processedOptions)
            }

            # Add options preparation feedback if not already processing
            if (is.null(private$.messages)) {
                private$.accumulateMessage(
                    glue::glue("<br>Preparing dot plot analysis options...<br><hr>")
                )
            }

            # Process type of statistics
            typestatistics <- self$options$typestatistics

            # Process variables
            dep <- self$options$dep
            group <- self$options$group

            # Centrality settings mapped to ggstatsplot arguments
            centrality_plotting <- isTRUE(self$options$centralityplotting) && self$options$centralityparameter != "none"
            centrality_type <- self$options$centralitytype
            if (self$options$centralityparameter == "median")
                centrality_type <- "nonparametric"
            if (is.null(centrality_type) || centrality_type == "")
                centrality_type <- typestatistics

            # Compute axis labels respecting orientation flip (values on x-axis)
            xlab <- self$options$ytitle
            if (xlab == '') xlab <- group
            ylab <- self$options$xtitle
            if (ylab == '') ylab <- dep
            
            # Process titles
            mytitle <- self$options$mytitle
            if (mytitle == '') mytitle <- NULL
            
            xtitle <- self$options$xtitle
            if (xtitle == '') xtitle <- NULL
            
            ytitle <- self$options$ytitle
            if (ytitle == '') ytitle <- NULL
            
            # Cache the processed options with all parameters
            options_list <- list(
                typestatistics = typestatistics,
                dep = dep,
                group = group,
                mytitle = mytitle,
                xlab = xlab,
                ylab = ylab,
                xtitle = xtitle,
                ytitle = ytitle,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                testvalue = self$options$testvalue,
                bfmessage = self$options$bfmessage,
                conflevel = self$options$conflevel,
                digits = self$options$k,
                testvalueline = self$options$testvalueline,
                centralityparameter = self$options$centralityparameter,
                centralityk = self$options$centralityk,
                resultssubtitle = self$options$resultssubtitle,
                originaltheme = self$options$originaltheme
            )

            # Apply preset overrides without mutating options object
            if (private$.currentPreset == "basic") {
                options_list$resultssubtitle <- TRUE
            } else if (private$.currentPreset == "publication") {
                options_list$resultssubtitle <- TRUE
                options_list$originaltheme <- TRUE
            } else if (private$.currentPreset == "clinical") {
                centrality_plotting <- TRUE
                centrality_type <- "nonparametric"
                options_list$centralityplotting <- TRUE
                options_list$centralityparameter <- "median"
            }
            
            # Process centrality parameters if enabled
            options_list$centrality.plotting <- centrality_plotting
            options_list$centrality.type <- centrality_type
            options_list$ggplot.component <- list(ggplot2::coord_flip())
            if (isTRUE(self$options$testvalueline)) {
                options_list$ggplot.component <- c(
                    options_list$ggplot.component,
                    list(ggplot2::geom_hline(
                        yintercept = self$options$testvalue,
                        linetype = "dashed",
                        color = "red"
                    ))
                )
            }
            
            private$.processedOptions <- options_list
            private$.options_hash <- current_options_hash
            return(options_list)
        },

        # run ----
        .run = function() {
            # Clear messages at start of new run
            private$.messages <- NULL
            
            # Initial Message ----
            if ( is.null(self$options$dep) || is.null(self$options$group)) {

                # todo ----

                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Dot Charts.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations for <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.html' target='_blank'>ggbetweenstats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbetweenstats.html' target='_blank'>grouped_ggbetweenstats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                # todo ----
                todo <- glue::glue(
                    "<br>You have selected to use a Dot Plot to compare continuous variables by groups.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'noDataInRun',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent('Data contains no complete rows after filtering. Please check for missing values.')
                    self$results$insert(1, notice)
                    return()
                }

                # Pre-process data and options for performance with enhanced validation
                tryCatch({
                    mydata <- private$.prepareData()
                    private$.prepareOptions()

                    # Generate clinical interpretation and assumptions
                    private$.generateClinicalInterpretation(mydata)
                    private$.checkAssumptions(mydata)
                    private$.generateReportSentence()

                    # Generate guided mode content if enabled
                    private$.generateGuidedSteps()
                    private$.generateRecommendations()
                }, error = function(e) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'dataProcessingFailed',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(sprintf('Data processing failed: %s. Please check your variable selections and try again.', e$message))
                    self$results$insert(1, notice)
                    return()
                })

            }
        }


        ,
        .plot = function(image, ggtheme, theme, ...) {
            # Use shared validation helper ----
            if (!private$.validateInputs())
                return()

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()


            # ggbetweenstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.html

            # Checkpoint before expensive ggstatsplot computation
            private$.checkpoint()

            plot <- tryCatch({
                ggstatsplot::ggbetweenstats(
                    data = mydata,
                    x = !!rlang::sym(private$.escapeVar(options_data$group)),
                    y = !!rlang::sym(private$.escapeVar(options_data$dep)),
                    title = options_data$mytitle,
                    xlab = options_data$xlab,
                    ylab = options_data$ylab,
                    type = options_data$typestatistics,
                    effsize.type = options_data$effsizetype,
                    conf.level = options_data$conflevel,
                    digits = options_data$digits,
                    bf.message = options_data$bfmessage,
                    centrality.plotting = options_data$centrality.plotting,
                    centrality.type = options_data$centrality.type,
                    results.subtitle = options_data$resultssubtitle,
                    ggplot.component = options_data$ggplot.component,
                    ggtheme = if (options_data$originaltheme) ggstatsplot::theme_ggstatsplot() else ggtheme
                )
            }, error = function(e) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'plotGenerationFailed',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(sprintf('Plot generation failed: %s. Please check your data for issues (constant variables, insufficient variation, or extreme outliers) or try a different statistical test.', e$message))
                self$results$insert(1, notice)
                return(NULL)
            })

            if (is.null(plot)) return()

            # Add success notice
            n_obs <- nrow(mydata)
            n_groups <- length(unique(mydata[[options_data$group]]))
            test_name <- switch(options_data$typestatistics,
                "parametric" = "parametric (t-test/ANOVA)",
                "nonparametric" = "nonparametric (Mann-Whitney/Kruskal-Wallis)",
                "robust" = "robust (trimmed means)",
                "bayes" = "Bayesian",
                "selected"
            )

            notice <- jmvcore::Notice$new(
                options = self$options,
                name = 'analysisComplete',
                type = jmvcore::NoticeType$INFO
            )
            notice$setContent(sprintf('Analysis completed successfully using %s test. Compared %d groups with N = %d total observations.', test_name, n_groups, n_obs))
            self$results$insert(999, notice)

            # Print Plot ----

            print(plot)
            TRUE

        }


        ,

        .plot2 = function(image, ggtheme, theme, ...) {
            # Use shared validation helper with additional grouping check ----
            if (!private$.validateInputs() || is.null(self$options$grvar))
                return()

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()


            # grouped_ggbetweenstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbetweenstats.html



            if (!is.null(self$options$grvar)) {
                grvar <- self$options$grvar

                # Checkpoint before expensive grouped ggstatsplot computation
                private$.checkpoint()

                plot2 <- tryCatch({
                    ggstatsplot::grouped_ggbetweenstats(
                        data = mydata,
                        x = !!rlang::sym(private$.escapeVar(options_data$group)),
                        y = !!rlang::sym(private$.escapeVar(options_data$dep)),
                        grouping.var = !!rlang::sym(grvar),
                        type = options_data$typestatistics,
                        effsize.type = options_data$effsizetype,
                        conf.level = options_data$conflevel,
                        digits = options_data$digits,
                        bf.message = options_data$bfmessage,
                        results.subtitle = options_data$resultssubtitle,
                        centrality.plotting = options_data$centrality.plotting,
                        centrality.type = options_data$centrality.type,
                        ggplot.component = options_data$ggplot.component,
                        ggtheme = if (options_data$originaltheme) ggstatsplot::theme_ggstatsplot() else ggtheme,
                        xlab = options_data$xlab,
                        ylab = options_data$ylab,
                        title = options_data$mytitle
                    )
                }, error = function(e) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'groupedPlotFailed',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(sprintf('Grouped plot generation failed: %s. Please check your grouping variable and data.', e$message))
                    self$results$insert(1, notice)
                    return(NULL)
                })

                if (is.null(plot2)) return()
            }


            # Print Plot ----

            print(plot2)
            TRUE

        }





    )
)
