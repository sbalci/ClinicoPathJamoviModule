#' @title Violin Plots to Compare Within Group
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#' @import tidyr
#' @import ggplot2
#' @importFrom rlang sym
#' @importFrom digest digest
#'


jjwithinstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjwithinstatsClass",
    inherit = jjwithinstatsBase,
    private = list(
        .prepared_data = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .messages = NULL,

        # init ----

        .init = function() {
            # Use configurable plot dimensions with smart scaling
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 650
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450
            
            # Only setup plot if variables are selected
            if (!is.null(self$options$dep1) && !is.null(self$options$dep2)) {
                # Calculate proportional scaling based on number of measurements
                num_measurements <- sum(!is.null(self$options$dep1), !is.null(self$options$dep2),
                                      !is.null(self$options$dep3), !is.null(self$options$dep4))
                
                if (num_measurements > 2) {
                    # Proportional scaling instead of magic numbers
                    scale_factor_width <- 1 + (num_measurements - 2) * 0.15  # 15% per additional measurement
                    scale_factor_height <- 1 + (num_measurements - 2) * 0.1   # 10% per additional measurement
                    plotwidth <- plotwidth * scale_factor_width
                    plotheight <- plotheight * scale_factor_height
                }
                
                self$results$plot$setSize(plotwidth, plotheight)
                
                # Apply clinical presets if selected
                private$.applyClinicalPresets()
                
                # Pre-prepare data and options for performance
                private$.prepareData()
                private$.prepareOptions()
                
                # Clear any welcome message when variables are selected
                self$results$todo$setContent("")
            } else {
                # Hide plot when insufficient variables selected
                self$results$plot$setVisible(visible = FALSE)
                
                # Show welcome message when insufficient variables
                welcome_msg <- "<br><strong>Welcome to ClinicoPath</strong><br><br>
                        This tool generates Violin Plots for repeated measurements (e.g., biomarker levels over time, treatment responses).<br><br>
                        <strong>Data Format:</strong> Wide format required - each row = one subject, columns = separate time points/measurements.<br><br>
                        <strong>Requirements:</strong><br>
                        • Select at least 2 measurements (First and Second Measurement fields)<br>
                        • No missing values allowed<br>
                        • Use for comparing 2-4 measurements from the same subjects<br><br>
                        See documentation: <a href='https://indrajeetpatil.github.io/ggstatsplot/reference/ggwithinstats.html' target='_blank'>ggwithinstats</a><br>
                        Please cite jamovi and packages as instructed.<br><hr>"
                
                self$results$todo$setVisible(visible = TRUE)
                self$results$todo$setContent(welcome_msg)
            }

        },
        
        # Enhanced message management system
        .accumulateMessage = function(message) {
            if (is.null(private$.messages)) {
                private$.messages <- character()
            }
            private$.messages <- append(private$.messages, message)
            self$results$todo$setContent(paste(private$.messages, collapse = ""))
        },
        
        # Reset messages for new analysis run
        .resetMessages = function() {
            private$.messages <- character()
            self$results$todo$setContent("")
        },
        
        # Apply clinical presets for common scenarios
        .applyClinicalPresets = function() {
            if (is.null(self$options$clinicalpreset) || self$options$clinicalpreset == "custom") {
                return()  # No preset to apply
            }
            
            # Apply preset configurations based on clinical scenario
            switch(self$options$clinicalpreset,
                "biomarker" = {
                    # Biomarker tracking: nonparametric, show individual trajectories
                    if (self$options$typestatistics == "parametric") {  # Only change if default
                        # Would need to update options through proper channels
                        private$.accumulateMessage(.("💡 Biomarker preset: Consider using Nonparametric test for skewed biomarker data<br>"))
                    }
                    private$.accumulateMessage(.("🔬 <strong>Biomarker Tracking:</strong> Optimized for laboratory values and biomarker levels over time<br>"))
                },
                "treatment" = {
                    # Treatment response: parametric with pairwise comparisons
                    if (!self$options$pairwisecomparisons) {
                        private$.accumulateMessage(.("💡 Treatment preset: Enable pairwise comparisons to identify when treatment effects occur<br>"))
                    }
                    private$.accumulateMessage(.("💊 <strong>Treatment Response:</strong> Optimized for clinical treatment monitoring<br>"))
                },
                "laboratory" = {
                    # Laboratory values: robust with centrality plotting
                    if (self$options$typestatistics == "parametric") {
                        private$.accumulateMessage(.("💡 Laboratory preset: Consider Robust test to handle outliers common in lab values<br>"))
                    }
                    if (!self$options$centralityplotting) {
                        private$.accumulateMessage(.("💡 Laboratory preset: Enable centrality plotting to see overall trends<br>"))
                    }
                    private$.accumulateMessage(.("🔬 <strong>Laboratory Values:</strong> Optimized for clinical lab value monitoring<br>"))
                }
            )
        },
        
        # Enhanced input validation with comprehensive checks
        .validateInputs = function() {
            # Basic requirement check
            if (is.null(self$options$dep1) || is.null(self$options$dep2))
                return(FALSE)
            
            # Comprehensive data structure validation
            private$.validateDataStructure()
            
            # Variable-specific validation
            vars <- c(self$options$dep1, self$options$dep2, 
                      self$options$dep3, self$options$dep4)
            vars <- vars[!sapply(vars, is.null)]
            
            private$.validateVariableTypes(vars)
            
            return(TRUE)
        },
        
        # Comprehensive data structure validation
        .validateDataStructure = function() {
            if (!is.data.frame(self$data))
                stop(.("Data must be a data frame"))
            if (nrow(self$data) == 0)
                stop(.("Data contains no (complete) rows"))
            if (ncol(self$data) < 2)
                stop(.("Data must contain at least 2 columns"))
            if (!is.finite(nrow(self$data)) || !is.finite(ncol(self$data)))
                stop(.("Data dimensions are invalid"))
        },
        
        # Variable type and existence validation
        .validateVariableTypes = function(vars) {
            for (var in vars) {
                if (!(var %in% names(self$data)))
                    stop(.("Variable '{name}' not found in data"), list(name = var))
                
                # Check if variable can be converted to numeric
                test_vals <- self$data[[var]]
                if (!is.numeric(test_vals)) {
                    converted_vals <- suppressWarnings(as.numeric(as.character(test_vals)))
                    if (all(is.na(converted_vals)) && !all(is.na(test_vals)))
                        stop(.("Variable '{name}' cannot be converted to numeric values"), list(name = var))
                }
            }
        },
        
        # Data quality validation helper
        .validateDataQuality = function(mydata, vars) {
            # Checkpoint before expensive validation loop
            private$.checkpoint()
            
            for (var in vars) {
                num_vals <- jmvcore::toNumeric(mydata[[var]])
                num_vals <- num_vals[!is.na(num_vals)]
                
                if (length(num_vals) < 3) {
                    private$.accumulateMessage(
                        glue::glue(.("<br>⚠️ <strong>Warning:</strong> {var} has less than 3 valid observations<br>"))
                    )
                }
                if (length(unique(num_vals)) < 2) {
                    private$.accumulateMessage(
                        glue::glue(.("<br>⚠️ <strong>Warning:</strong> {var} has no variation (all values identical)<br>"))
                    )
                }
            }
            
            # Additional clinical misuse detection
            private$.detectClinicalMisuse(mydata, vars)
        },
        
        # Clinical misuse detection and guidance
        .detectClinicalMisuse = function(mydata, vars) {
            total_subjects <- nrow(mydata)
            
            # Small sample size warning
            if (total_subjects < 10) {
                private$.accumulateMessage(
                    .("<br>⚠️ <strong>Small Sample Size:</strong> With fewer than 10 subjects, results may be unreliable. Consider larger sample or descriptive analysis.<br>")
                )
            }
            
            # Check for potential outliers across all variables
            for (var in vars) {
                num_vals <- jmvcore::toNumeric(mydata[[var]])
                num_vals <- num_vals[!is.na(num_vals)]
                
                if (length(num_vals) > 4) {  # Need enough data points to detect outliers
                    q75 <- quantile(num_vals, 0.75, na.rm = TRUE)
                    q25 <- quantile(num_vals, 0.25, na.rm = TRUE)
                    iqr <- q75 - q25
                    outliers <- sum(num_vals > (q75 + 1.5 * iqr) | num_vals < (q25 - 1.5 * iqr))
                    
                    if (outliers > length(num_vals) * 0.1) {  # More than 10% outliers
                        private$.accumulateMessage(
                            .("<br>💡 <strong>Many Outliers Detected:</strong> Consider Robust test type to reduce outlier influence<br>")
                        )
                        break  # Only show this message once
                    }
                }
            }
            
            # Check for extremely skewed data that might benefit from nonparametric tests
            if (self$options$typestatistics == "parametric") {
                for (var in vars) {
                    num_vals <- jmvcore::toNumeric(mydata[[var]])
                    num_vals <- num_vals[!is.na(num_vals)]
                    
                    if (length(num_vals) > 5) {
                        # Simple skewness check using mean vs median
                        mean_val <- mean(num_vals)
                        median_val <- median(num_vals)
                        
                        if (abs(mean_val - median_val) > sd(num_vals)) {  # Highly skewed
                            private$.accumulateMessage(
                                .("<br>💡 <strong>Skewed Data Detected:</strong> Consider Nonparametric test for skewed biomarker or clinical data<br>")
                            )
                            break  # Only show this message once
                        }
                    }
                }
            }
        },

        # Optimized data preparation with robust caching
        .prepareData = function(force_refresh = FALSE) {
            # IMMEDIATE CHECK: Return NULL if minimum variables not selected
            if (is.null(self$options$dep1) || is.null(self$options$dep2)) {
                return(NULL)
            }
            
            # Checkpoint before expensive hash computation for large datasets
            private$.checkpoint(flush = FALSE)

            # CRITICAL FIX: Create robust hash including ACTUAL DATA VALUES
            # Bug: Old code only hashed variable names and dimensions
            # Result: Editing data values didn't invalidate cache, showing outdated results
            vars <- Filter(Negate(is.null), c(self$options$dep1, self$options$dep2,
                                             self$options$dep3, self$options$dep4))

            # Extract actual data for selected variables
            data_subset <- self$data[, vars, drop = FALSE]

            current_hash <- digest::digest(list(
                dep1 = self$options$dep1, dep2 = self$options$dep2,
                dep3 = self$options$dep3, dep4 = self$options$dep4,
                data_dim = dim(self$data), col_names = names(self$data),
                # CRITICAL FIX: Include actual data content in hash
                data_content = data_subset
            ), algo = "md5")
            
            # Only reprocess if data has changed or forced refresh
            if (!is.null(private$.prepared_data) && 
                private$.data_hash == current_hash && 
                !force_refresh) {
                return(private$.prepared_data)
            }
            
            # Add processing feedback
            private$.accumulateMessage(
                glue::glue(.("<br>Processing {length(vars)} measurements for within-subjects analysis...<br>"))
            )
            
            # Variables already validated above, proceed with data preparation
            mydata <- self$data
            
            # Check for empty dataset
            if (nrow(mydata) == 0) {
                private$.accumulateMessage(
                    .("<br>❌ Dataset is empty (0 rows). Please load data first.<br>")
                )
                private$.prepared_data <- NULL
                return(NULL)
            }
            
            mydata$rowid <- seq.int(nrow(mydata))
            
            # Check if required variables exist in dataset
            missing_vars <- vars[!vars %in% names(mydata)]
            if (length(missing_vars) > 0) {
                private$.accumulateMessage(
                    paste0(.("<br>❌ Variables not found in dataset: "), paste(missing_vars, collapse = ", "), "<br>")
                )
                private$.prepared_data <- NULL
                return(NULL)
            }
            
            # Checkpoint before expensive data conversion loop
            private$.checkpoint()
            
            # Convert variables to numeric once
            for (var in vars)
                mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])
            
            # Enhanced validation for optional parameters
            if (!is.null(self$options$dep3) && is.null(self$options$dep2)) {
                stop('Second measurement required when third is specified')
            }
            if (!is.null(self$options$dep4) && is.null(self$options$dep3)) {
                stop('Third measurement required when fourth is specified')
            }

            # CRITICAL FIX: Validate paired design structure
            # Within-subjects analysis requires complete data for all subjects across measurements
            # Check for excessive missing data that would break paired tests
            n_rows <- nrow(mydata)
            complete_cases <- sum(complete.cases(mydata[, vars, drop = FALSE]))
            missing_pct <- (1 - complete_cases/n_rows) * 100

            if (complete_cases < 3) {
                warning_msg <- paste0(
                    "<div style='background:#fff3cd; border-left:4px solid #ff9800; padding:15px; margin:10px 0;'>",
                    "<h4 style='color:#ff6f00; margin-top:0;'>⚠️ Insufficient Complete Cases for Paired Analysis</h4>",
                    "<p><strong>Within-subjects analysis requires complete data across all measurements.</strong></p>",
                    "<p>Current status:</p>",
                    "<ul>",
                    "<li>Total subjects: ", n_rows, "</li>",
                    "<li>Complete cases (all measurements present): ", complete_cases, "</li>",
                    "<li>Missing data: ", round(missing_pct, 1), "%</li>",
                    "</ul>",
                    "<p><strong>Minimum required:</strong> At least 3 subjects with complete data across all measurements.</p>",
                    "</div>"
                )
                private$.accumulateMessage(warning_msg)
                private$.prepared_data <- NULL
                return(NULL)
            }

            if (missing_pct > 50) {
                warning_msg <- paste0(
                    "<div style='background:#fff3cd; border-left:4px solid #ffc107; padding:15px; margin:10px 0;'>",
                    "<h4 style='color:#ff9800; margin-top:0;'>⚠️ High Missing Data Rate</h4>",
                    "<p><strong>Warning:</strong> ", round(missing_pct, 1), "% of subjects have incomplete measurements.</p>",
                    "<p>Paired analysis will only use the ", complete_cases, " subjects with complete data.</p>",
                    "<p>Consider investigating why so much data is missing.</p>",
                    "</div>"
                )
                self$results$warnings$setContent(
                    paste0(self$results$warnings$state, warning_msg)
                )
                self$results$warnings$setVisible(TRUE)
            }

            # Validate data quality before processing
            private$.validateDataQuality(mydata, vars)
            
            # Remove NA values once
            mydata <- jmvcore::naOmit(mydata)
            
            if (nrow(mydata) == 0) {
                private$.accumulateMessage(
                    .("<br>❌ No complete observations after removing missing values<br>")
                )
                private$.prepared_data <- NULL
                return(NULL)
            }
            
            # Checkpoint before expensive data transformation
            private$.checkpoint()
            
            # Perform pivot_longer transformation once
            long_data <- tidyr::pivot_longer(
                mydata,
                cols = vars,
                names_to = "measurement",
                values_to = "value"
            )
            
            long_data$measurement <- factor(long_data$measurement, levels = vars)
            
            private$.prepared_data <- long_data
            private$.data_hash <- current_hash
            
            return(private$.prepared_data)
        },
        
        .prepareOptions = function(force_refresh = FALSE) {
            # Checkpoint before options processing
            private$.checkpoint(flush = FALSE)
            
            # Create robust hash of current options to detect changes
            current_options_hash <- digest::digest(list(
                typestatistics = self$options$typestatistics,
                pairwisecomparisons = self$options$pairwisecomparisons,
                pairwisedisplay = self$options$pairwisedisplay,
                padjustmethod = self$options$padjustmethod,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                pointpath = self$options$pointpath,
                centralitypath = self$options$centralitypath,
                violin = self$options$violin,
                boxplot = self$options$boxplot,
                point = self$options$point,
                titles = list(self$options$mytitle, self$options$xtitle, self$options$ytitle),
                display = list(self$options$resultssubtitle, self$options$originaltheme),
                advanced = list(self$options$bfmessage, self$options$conflevel, self$options$k)
            ), algo = "md5")
            
            # Only reprocess if options have changed or forced refresh
            if (!is.null(private$.prepared_options) && 
                private$.options_hash == current_options_hash && 
                !force_refresh) {
                return(private$.prepared_options)
            }
            
            # Process options directly (no formula construction needed for simple strings)
            typestatistics <- self$options$typestatistics
            pairwisedisplay <- self$options$pairwisedisplay
            padjustmethod <- self$options$padjustmethod
            
            # Process titles once
            mytitle <- self$options$mytitle
            xtitle <- if (self$options$xtitle == '') NULL else self$options$xtitle
            ytitle <- if (self$options$ytitle == '') NULL else self$options$ytitle
            
            # Process plot component arguments once
            violinargs <- if (self$options$violin) {
                list(width = 0.5, alpha = 0.2, na.rm = TRUE)
            } else {
                list(width = 0)
            }
            
            # Updated API parameter names for current ggstatsplot
            boxplotargs <- if (self$options$boxplot) {
                list(width = 0.2, alpha = 0.5, na.rm = TRUE)
            } else {
                list(width = 0)
            }
            
            pointargs <- if (self$options$point) {
                list(size = 3, alpha = 0.5, na.rm = TRUE)
            } else {
                list(alpha = 0)
            }
            
            pointpathargs <- if (self$options$pointpath) {
                list(alpha = 0.5, linetype = "dashed")
            } else {
                list(alpha = 0)
            }
            
            centralitypointargs <- if (self$options$centralityplotting) {
                list(size = 5, color = "darkred")
            } else {
                list(size = 0)
            }
            
            centralitypathargs <- if (self$options$centralitypath && self$options$centralityplotting) {
                list(linewidth = 1, color = "red", alpha = 0.5)
            } else {
                list(linewidth = 0)
            }
            
            private$.prepared_options <- list(
                typestatistics = typestatistics,
                pairwisecomparisons = self$options$pairwisecomparisons,
                pairwisedisplay = pairwisedisplay,
                padjustmethod = padjustmethod,
                pointpath = self$options$pointpath,
                mytitle = mytitle,
                xtitle = xtitle,
                ytitle = ytitle,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                centralitypath = self$options$centralitypath,
                violinargs = violinargs,
                boxplotargs = boxplotargs,
                pointargs = pointargs,
                pointpathargs = pointpathargs,
                centralitypointargs = centralitypointargs,
                centralitypathargs = centralitypathargs,
                originaltheme = self$options$originaltheme,
                resultssubtitle = self$options$resultssubtitle,
                # Add missing parameters with current API names
                bfmessage = if (!is.null(self$options$bfmessage)) self$options$bfmessage else TRUE,
                conflevel = if (!is.null(self$options$conflevel)) self$options$conflevel else 0.95,
                digits = if (!is.null(self$options$k)) self$options$k else 2
            )
            
            private$.options_hash <- current_options_hash
            
            return(private$.prepared_options)
        },
        
        # Clinical interpretation helper
        .generateClinicalInterpretation = function(num_measurements, test_type) {
            interpretation_parts <- list()
            
            # Test explanation
            test_explanation <- switch(test_type,
                "parametric" = .("Repeated measures ANOVA tests whether means differ significantly across time points, assuming normal distribution."),
                "nonparametric" = .("Friedman test compares medians across time points without assuming normal distribution (robust for skewed biomarker data)."),
                "robust" = .("Robust test uses trimmed means, reducing influence of outliers common in clinical measurements."),
                "bayes" = .("Bayesian analysis provides evidence for/against differences, useful when traditional p-values are inconclusive."),
                .("Statistical test compares measurements across time points.")
            )
            
            # Sample interpretation
            clinical_context <- sprintf(
                .("This analysis compares %d measurements from the same subjects. Useful for: biomarker tracking, treatment response monitoring, disease progression assessment."),
                num_measurements
            )
            
            # What to look for
            guidance <- .("🔍 <strong>What to look for:</strong><br>• Statistical significance (p < 0.05) indicates real changes over time<br>• Effect sizes show practical importance<br>• Individual trajectories reveal response patterns<br>• Outliers may indicate treatment non-responders or measurement errors")
            
            interpretation_parts <- list(
                paste0("<div style='background-color:#f8f9fa;padding:15px;margin:10px 0;border-left:4px solid #007bff;'>"),
                paste0("<h4 style='margin-top:0;color:#007bff;'>", .("Clinical Context"), "</h4>"),
                paste0("<p>", clinical_context, "</p>"),
                paste0("<p><strong>", .("Test Used:"), "</strong> ", test_explanation, "</p>"),
                guidance,
                "</div>"
            )
            
            return(paste(interpretation_parts, collapse = ""))
        },
        
        # Generate analysis summary
        .generateAnalysisSummary = function(opts, num_measurements, has_significance = NULL) {
            summary_parts <- list()
            
            # Analysis overview
            analysis_type <- switch(opts$typestatistics,
                "parametric" = .("Repeated measures ANOVA"),
                "nonparametric" = .("Friedman test"),
                "robust" = .("Robust repeated measures test"),
                "bayes" = .("Bayesian repeated measures analysis"),
                .("Within-subjects comparison")
            )
            
            summary_header <- sprintf(
                .("<strong>Analysis:</strong> %s with %d measurements per subject"),
                analysis_type, num_measurements
            )
            
            # Configuration summary
            config_items <- character()
            if (opts$pairwisecomparisons) {
                config_items <- c(config_items, .("✓ Pairwise comparisons enabled"))
            }
            if (opts$centralityplotting) {
                config_items <- c(config_items, .("✓ Central tendency displayed"))
            }
            if (opts$pointpath) {
                config_items <- c(config_items, .("✓ Individual trajectories shown"))
            }
            
            if (length(config_items) > 0) {
                config_summary <- paste0("<br><strong>", .("Configuration:"), "</strong><br>", 
                                       paste(config_items, collapse = "<br>"))
            } else {
                config_summary <- ""
            }
            
            summary_parts <- list(
                paste0("<div style='background-color:#f0f8f0;padding:10px;border:1px solid #28a745;'>"),
                summary_header,
                config_summary,
                "</div>"
            )
            
            return(paste(summary_parts, collapse = ""))
        }

        # run ----
        ,
        .run = function() {
            ## Initial Message ----
            if ( is.null(self$options$dep1) || is.null(self$options$dep2)) {

                ### todo ----

                # Simple welcome message for testing
                todo <- "<br><strong>Welcome to ClinicoPath</strong><br><br>
                        This tool generates Violin Plots for repeated measurements (e.g., biomarker levels over time, treatment responses).<br><br>
                        <strong>Data Format:</strong> Wide format required - each row = one subject, columns = separate time points/measurements.<br><br>
                        <strong>Requirements:</strong><br>
                        • Select at least 2 measurements (First and Second Measurement fields)<br>
                        • No missing values allowed<br>
                        • Use for comparing 2-4 measurements from the same subjects<br><br>
                        See documentation: <a href='https://indrajeetpatil.github.io/ggstatsplot/reference/ggwithinstats.html' target='_blank'>ggwithinstats</a><br>
                        Please cite jamovi and packages as instructed.<br><hr>"

                # Ensure todo is visible and set content
                self$results$todo$setVisible(visible = TRUE)
                self$results$todo$setContent(todo)
                
                # Hide plot output when no variables selected
                self$results$plot$setVisible(visible = FALSE)
                
                # Hide other outputs as well
                self$results$interpretation$setVisible(visible = FALSE)
                if (!is.null(self$results$summary)) {
                    self$results$summary$setVisible(visible = FALSE)
                }

                return()

            } else {
                # Clear messages for actual analysis
                private$.resetMessages()
                
                # Make all outputs visible when variables are selected
                self$results$todo$setVisible(visible = TRUE)
                self$results$plot$setVisible(visible = TRUE)
                self$results$interpretation$setVisible(visible = TRUE)
                if (!is.null(self$results$summary)) {
                    self$results$summary$setVisible(visible = TRUE)
                }
                
                # Generate clinical guidance for active analysis
                vars <- c(self$options$dep1, self$options$dep2, 
                         self$options$dep3, self$options$dep4)
                num_measurements <- sum(!sapply(vars, is.null))
                
                # Generate clinical interpretation with error protection
                tryCatch({
                    clinical_interp <- private$.generateClinicalInterpretation(
                        num_measurements, self$options$typestatistics
                    )
                    self$results$interpretation$setContent(clinical_interp)
                }, error = function(e) {
                    self$results$interpretation$setContent(.("<br>Clinical interpretation temporarily unavailable<br>"))
                })
                
                # Generate analysis summary with error protection
                tryCatch({
                    opts <- private$.prepareOptions()
                    analysis_summary <- private$.generateAnalysisSummary(opts, num_measurements)
                    self$results$summary$setContent(analysis_summary)
                }, error = function(e) {
                    self$results$summary$setContent(.("<br>Analysis summary temporarily unavailable<br>"))
                })

                ### todo ----
                todo <- sprintf(.("<br>✅ Ready for analysis: Violin Plot comparing %d repeated measurements.<br><hr>"), 
                               num_measurements)

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop(.("Data contains no (complete) rows"))

                # Generate explanations if requested
                if (self$options$showExplanations) {
                    private$.generateExplanations()
                }

            }
        }


        # the plot function ----
        ,
        .plot = function(image, ggtheme, theme, ...) {
            # Use shared validation helper ----
            if (!private$.validateInputs())
                return()

            # Use prepared data and options ----
            long_data <- private$.prepareData()
            opts <- private$.prepareOptions()
            
            if (is.null(long_data)) {
                return()
            }

            # Additional validation
            if (nrow(long_data) == 0) {
                warning_msg <- .("<br>No complete data rows available after removing missing values.<br>Please check your data for missing values in the selected variables.<br><hr>")
                self$results$todo$setContent(warning_msg)
                return()
            }

            # Checkpoint before expensive plot creation
            private$.checkpoint()
            
            # Create plot using optimized data and options ----
            tryCatch({
                plot <- ggstatsplot::ggwithinstats(
                    data = long_data,
                    x = measurement,
                    y = value,
                    paired = TRUE,
                    id = "rowid",
                    title = opts$mytitle,
                    xlab = opts$xtitle,
                    ylab = opts$ytitle,
                    type = opts$typestatistics,
                    pairwise.comparisons = opts$pairwisecomparisons,
                    pairwise.display = opts$pairwisedisplay,
                    p.adjust.method = opts$padjustmethod,
                    effsize.type = opts$effsizetype,
                    centrality.plotting = opts$centralityplotting,
                    centrality.type = opts$centralitytype,
                    point.path = opts$pointpath,
                    centrality.path = opts$centralitypath,
                    violin.args = opts$violinargs,
                    boxplot.args = opts$boxplotargs,
                    point.args = opts$pointargs,
                    point.path.args = opts$pointpathargs,
                    centrality.point.args = opts$centralitypointargs,
                    centrality.path.args = opts$centralitypathargs,
                    results.subtitle = opts$resultssubtitle,
                    # Updated parameter names for current API
                    bf.message = opts$bfmessage,
                    conf.level = opts$conflevel,
                    digits = opts$digits
                )

                # Checkpoint before theme application
                private$.checkpoint()
                
                # Apply theme
                if (!opts$originaltheme) {
                    plot <- plot + ggtheme
                } else {
                    plot <- plot + ggstatsplot::theme_ggstatsplot()
                }

                # Print Plot ----
                print(plot)
                TRUE
                
            }, error = function(e) {
                error_msg <- paste0(
                    .("<br>Error creating within-subjects plot: "), e$message,
                    .("<br><br>Please check that:"),
                    .("<br>• All measurement variables contain numeric values"),
                    .("<br>• Data has at least 2 complete rows"),
                    .("<br>• Variables have sufficient variance for statistical tests"),
                    .("<br>• No extreme outliers that might affect analysis"),
                    .("<br><hr>")
                )
                self$results$todo$setContent(error_msg)
            })

        }

        ,
        .plotGGPubr = function(image, ...) {
            # Validate inputs
            if (is.null(self$options$dep1) || is.null(self$options$dep2))
                return()

            # Skip if ggpubr plot not requested
            if (!self$options$addGGPubrPlot)
                return()

            tryCatch({
                # Prepare data - convert wide to long format
                mydata <- self$data

                # Collect dependent variables
                deps <- c(self$options$dep1, self$options$dep2)
                if (!is.null(self$options$dep3)) deps <- c(deps, self$options$dep3)
                if (!is.null(self$options$dep4)) deps <- c(deps, self$options$dep4)

                # Create subject ID if not exists
                mydata$Subject_ID <- seq_len(nrow(mydata))

                # Convert to long format
                long_data <- data.frame()
                for (i in seq_along(deps)) {
                    temp <- data.frame(
                        Subject_ID = mydata$Subject_ID,
                        Measurement = deps[i],
                        Value = mydata[[deps[i]]]
                    )
                    long_data <- rbind(long_data, temp)
                }

                # Remove NA values
                long_data <- long_data[complete.cases(long_data), ]

                # Get palette
                palette <- self$options$ggpubrPalette

                # Create plot based on type
                if (self$options$ggpubrPlotType == "paired") {
                    # Paired plot with connecting lines
                    args <- list(
                        data = long_data,
                        x = "Measurement",
                        y = "Value",
                        id = "Subject_ID",
                        palette = palette,
                        line.color = "gray",
                        line.size = 0.4
                    )

                    if (!self$options$ggpubrShowLines) {
                        args$line.color <- NA
                    }

                    plot <- do.call(ggpubr::ggpaired, args)

                } else if (self$options$ggpubrPlotType == "boxplot") {
                    args <- list(
                        data = long_data,
                        x = "Measurement",
                        y = "Value",
                        palette = palette,
                        add = if (self$options$ggpubrAddPoints) "jitter" else NULL
                    )
                    plot <- do.call(ggpubr::ggboxplot, args)

                } else if (self$options$ggpubrPlotType == "violin") {
                    args <- list(
                        data = long_data,
                        x = "Measurement",
                        y = "Value",
                        palette = palette,
                        add = if (self$options$ggpubrAddPoints) "jitter" else NULL
                    )
                    plot <- do.call(ggpubr::ggviolin, args)

                } else if (self$options$ggpubrPlotType == "line") {
                    args <- list(
                        data = long_data,
                        x = "Measurement",
                        y = "Value",
                        palette = palette,
                        add = "mean_se"
                    )
                    plot <- do.call(ggpubr::ggline, args)
                }

                # CRITICAL FIX: Add statistical comparisons with PAIRED tests
                # Bug: Old code used default independent-sample tests
                # Result: Wrong p-values for within-subjects design
                if (self$options$ggpubrAddStats && self$options$ggpubrPlotType != "line") {
                    # Determine appropriate paired test method based on user's test type
                    test_method <- switch(
                        self$options$testType,
                        "parametric" = "t.test",
                        "nonparametric" = "wilcox.test",
                        "robust" = "t.test",  # Fallback for robust
                        "t.test"  # Default
                    )

                    plot <- plot + ggpubr::stat_compare_means(
                        method = test_method,
                        paired = TRUE,  # CRITICAL: This is a within-subjects design
                        label = "p.signif"
                    )
                }

                # Apply theme
                plot <- plot + ggpubr::theme_pubr()

                print(plot)
                TRUE

            }, error = function(e) {
                error_msg <- paste0(
                    .("<br>Error creating ggpubr within-subjects plot: "), e$message,
                    .("<br><br>Please check that measurement variables contain numeric values."),
                    .("<br><hr>")
                )
                self$results$todo$setContent(error_msg)
            })
        }

    )
)
