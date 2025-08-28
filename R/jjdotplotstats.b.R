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
            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')
            
            # Check variable existence with better context
            if (!(self$options$dep %in% names(self$data))) {
                available_vars <- paste(names(self$data), collapse=", ")
                stop(glue::glue(
                    'Variable "{self$options$dep}" not found in data.\n',
                    'Available variables: {available_vars}\n',
                    'Please select a valid continuous variable for the dependent variable.'
                ))
            }
            if (!(self$options$group %in% names(self$data))) {
                available_vars <- paste(names(self$data), collapse=", ")
                stop(glue::glue(
                    'Variable "{self$options$group}" not found in data.\n',
                    'Available variables: {available_vars}\n',
                    'Please select a valid grouping variable.'
                ))
            }
            
            # Validate centrality parameter consistency
            private$.validateCentralityOptions()
                
            return(TRUE)
        },
        
        # Centrality parameter validation helper
        .validateCentralityOptions = function() {
            if (self$options$centralityparameter == "none" && self$options$centralityk != 2) {
                private$.accumulateMessage(
                    "<br>ℹ️ Note: Centrality decimal places specified but centrality parameter is 'none'<br>"
                )
            }
            
            if (self$options$centralityplotting && self$options$centralityparameter == "none") {
                private$.accumulateMessage(
                    "<br>⚠️ Warning: Centrality plotting enabled but centrality parameter is 'none'<br>"
                )
            }
            
            if (!self$options$centralityplotting && self$options$centralitytype != "parametric") {
                private$.accumulateMessage(
                    "<br>ℹ️ Note: Centrality type specified but centrality plotting is disabled<br>"
                )
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
                if (length(groups) != 2) {
                    groups <- groups[1:min(2, length(groups))]
                }
                
                # Generate interpretation based on test type
                test_description <- switch(test_type,
                    "parametric" = "t-test for comparing means",
                    "nonparametric" = "Mann-Whitney U test for comparing distributions",
                    "robust" = "robust test using trimmed means",
                    "bayes" = "Bayesian comparison of groups",
                    "comparison of groups"
                )
                
                interpretation <- glue::glue(
                    "<div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 10px 0;'>",
                    "<h4 style='color: #007bff; margin-top: 0;'>📊 Clinical Interpretation</h4>",
                    "<p><strong>Analysis:</strong> This dot plot shows the distribution of {dep_var} across different {group_var} categories using a {test_description}.</p>",
                    "<p><strong>Sample:</strong> ",
                    if(length(groups) >= 2) {
                        paste0("Group '", groups[1], "' (n=", group_ns[groups[1]], "), ",
                               "Group '", groups[2], "' (n=", group_ns[groups[2]], ")")
                    } else {
                        paste0("Total n = ", sum(group_ns))
                    },
                    "</p>",
                    "<p><strong>Results:</strong> ",
                    if(length(groups) >= 2) {
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
                        paste0("The analysis shows the distribution pattern of ", dep_var, " across groups.")
                    },
                    "</p>",
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
                skewness_rough <- (mean(numeric_data, na.rm = TRUE) - median(numeric_data, na.rm = TRUE)) / sd(numeric_data, na.rm = TRUE)
                
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
            preset <- if (!is.null(self$options$clinicalPreset)) self$options$clinicalPreset else "basic"
            
            # Store preset for use in interpretation methods
            private$.currentPreset <- preset
            
            # Modify welcome message based on preset
            if (!is.null(self$options$dep) && !is.null(self$options$group)) {
                preset_message <- switch(preset,
                    "basic" = "Using basic analysis settings optimized for straightforward comparisons.",
                    "publication" = "Using publication-ready settings with comprehensive statistical reporting.",
                    "clinical" = "Using clinical settings optimized for medical decision-making.",
                    "custom" = "Using your custom analysis configuration.",
                    "Basic analysis settings applied."
                )
                
                private$.accumulateMessage(
                    glue::glue("<br>📋 {preset_message}<br>")
                )
            }
        },
        
        # Report sentence generator
        .generateReportSentence = function() {
            dep_var <- self$options$dep
            group_var <- self$options$group
            test_type <- self$options$typestatistics
            
            test_name <- switch(test_type,
                "parametric" = "independent samples t-test",
                "nonparametric" = "Mann-Whitney U test", 
                "robust" = "robust comparison test",
                "bayes" = "Bayesian group comparison",
                "statistical comparison"
            )
            
            report_template <- glue::glue(
                "<div style='background-color: #e7f3ff; padding: 15px; border-left: 4px solid #0066cc; margin: 10px 0;'>",
                "<h4 style='color: #0066cc; margin-top: 0;'>📝 Copy-Ready Report Sentence</h4>",
                "<div style='background-color: white; padding: 10px; border: 1px dashed #0066cc; font-family: \"Times New Roman\", serif;'>",
                "<p>A <strong>{test_name}</strong> was performed to compare <em>{dep_var}</em> between <em>{group_var}</em> groups. ",
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

            # Exclude NA with checkpoint
            private$.checkpoint()
            mydata <- jmvcore::naOmit(mydata)
            
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
                xtitle = xtitle,
                ytitle = ytitle,
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
                resultssubtitle = self$options$resultssubtitle,
                originaltheme = self$options$originaltheme
            )
            
            # Process centrality parameters if enabled
            if (options_list$centralityplotting) {
                options_list$centrality.plotting <- TRUE
                options_list$centrality.type <- options_list$centralitytype
            } else {
                options_list$centrality.plotting <- FALSE
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
                This function uses ggplot2 and ggstatsplot packages. See documentations for <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggdotplotstats.html' target='_blank'>ggdotplotstats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggdotplotstats.html' target='_blank'>grouped_ggdotplotstats</a>.
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

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

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
                    private$.accumulateMessage(
                        glue::glue("<br>❌ Error during processing: {e$message}<br>")
                    )
                    stop(paste("Data processing failed:", e$message, "\nPlease check your variable selections."))
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


            # ggdotplotstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggdotplotstats.html

            # Checkpoint before expensive ggstatsplot computation
            private$.checkpoint()

            plot <- ggstatsplot::ggdotplotstats(
                data = mydata,
                x = !!rlang::sym(options_data$dep),
                y = !!rlang::sym(options_data$group),
                title = options_data$mytitle,
                xlab = options_data$xtitle,
                ylab = options_data$ytitle,
                type = options_data$typestatistics,
                test.value = options_data$testvalue,
                effsize.type = options_data$effsizetype,
                conf.level = options_data$conflevel,
                k = options_data$k,
                bf.message = options_data$bfmessage,
                test.value.line = options_data$testvalueline,
                centrality.parameter = options_data$centralityparameter,
                centrality.k = options_data$centralityk,
                results.subtitle = options_data$resultssubtitle
            )


            if (!options_data$originaltheme) {
                plot <- plot + ggtheme
            } else {
                plot <- plot + ggstatsplot::theme_ggstatsplot()
            }

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


            # grouped_ggdotplotstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggdotplotstats.html



            if (!is.null(self$options$grvar)) {
                selected_theme <- if (!options_data$originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()
                grvar <- self$options$grvar

                # Checkpoint before expensive grouped ggstatsplot computation
                private$.checkpoint()
                
                plot2 <- ggstatsplot::grouped_ggdotplotstats(
                    data = mydata,
                    x = !!rlang::sym(options_data$dep),
                    y = !!rlang::sym(options_data$group),
                    grouping.var = !!rlang::sym(grvar),
                    type = options_data$typestatistics,
                    test.value = options_data$testvalue,
                    effsize.type = options_data$effsizetype,
                    conf.level = options_data$conflevel,
                    k = options_data$k,
                    bf.message = options_data$bfmessage,
                    test.value.line = options_data$testvalueline,
                    centrality.parameter = options_data$centralityparameter,
                    centrality.k = options_data$centralityk,
                    results.subtitle = options_data$resultssubtitle,
                    ggtheme = selected_theme
                )
            }


            # Print Plot ----

            print(plot2)
            TRUE

        }





    )
)








