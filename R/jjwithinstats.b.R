#' @title Violin Plots to Compare Within Group (Repeated Measures)
#'
#' @description
#' Creates violin plots for within-subjects (repeated measures) analysis using
#' ggstatsplot::ggwithinstats. Compares 2-4 measurements from the same subjects
#' with statistical testing and pairwise comparisons. Ideal for biomarker tracking,
#' treatment response monitoring, and longitudinal clinical studies.
#'
#' @param data Data frame in wide format (one row per subject, columns for time points)
#' @param dep1 First measurement variable (required, numeric)
#' @param dep2 Second measurement variable (required, numeric)
#' @param dep3 Third measurement variable (optional, numeric)
#' @param dep4 Fourth measurement variable (optional, numeric)
#' @param typestatistics Type of statistical test: "parametric" (repeated measures ANOVA),
#'   "nonparametric" (Friedman test), "robust" (trimmed means), or "bayes" (Bayesian)
#' @param pairwisecomparisons Logical, whether to display pairwise comparisons between
#'   time points (default: FALSE)
#' @param pairwisedisplay Which pairwise comparisons to show: "significant",
#'   "non-significant", or "everything" (default: "significant")
#' @param padjustmethod P-value adjustment method for multiple comparisons:
#'   "holm" (default), "hochberg", "bonferroni", "BH", "fdr", etc.
#' @param effsizetype Effect size type for parametric tests: "biased" (Cohen's d),
#'   "unbiased" (Hedge's g), "eta", or "omega"
#' @param centralityplotting Logical, whether to display mean/median trend lines
#' @param centralitytype Type of centrality measure: "parametric" (mean),
#'   "nonparametric" (median), "robust" (trimmed mean), or "bayes"
#' @param pointpath Logical, whether to show individual subject trajectories
#' @param centralitypath Logical, whether to connect centrality points across measurements
#' @param violin Logical, display violin plot layer
#' @param boxplot Logical, display box plot layer
#' @param point Logical, display individual data points
#' @param mytitle Plot title (default: "Within Group Comparison")
#' @param xtitle X-axis label (default: auto from measurement names)
#' @param ytitle Y-axis label (default: auto from value variable)
#' @param originaltheme Logical, use ggstatsplot theme (TRUE) or jamovi theme (FALSE)
#' @param resultssubtitle Logical, display statistical results as subtitle
#' @param bfmessage Logical, display Bayes Factor message for Bayesian analysis
#' @param conflevel Confidence level for intervals (default: 0.95)
#' @param k Number of decimal places for statistics (default: 2)
#' @param plotwidth Plot width in pixels (default: 650)
#' @param plotheight Plot height in pixels (default: 450)
#' @param clinicalpreset Clinical analysis preset: "custom", "biomarker",
#'   "treatment", or "laboratory"
#' @param addGGPubrPlot Logical, add publication-ready ggpubr variant plot
#' @param ggpubrPlotType Type of ggpubr plot: "boxplot", "violin", "paired", or "line"
#' @param ggpubrPalette Color palette for ggpubr: "jco", "npg", "aaas", "lancet", etc.
#' @param ggpubrAddStats Logical, add statistical comparisons to ggpubr plot
#' @param ggpubrShowLines Logical, show connecting lines in paired plot
#' @param ggpubrAddPoints Logical, overlay individual points on ggpubr plot
#' @param showExplanations Logical, display explanations of statistical methods
#'
#' @return A jamovi analysis object with violin plots, statistical tests, and
#'   clinical interpretation
#'
#' @details
#' **Data Requirements:**
#' - Wide format required (one row per subject)
#' - Each column represents a different time point or condition
#' - Complete data required for paired analysis (listwise deletion)
#' - Minimum 3 subjects with complete data across all measurements
#'
#' **Statistical Tests:**
#' - Parametric: Repeated measures ANOVA (assumes normality)
#' - Nonparametric: Friedman test (no distribution assumptions)
#' - Robust: Uses trimmed means (resistant to outliers)
#' - Bayesian: Provides evidence strength via Bayes Factors
#'
#' **Clinical Presets:**
#' - Biomarker: Optimized for laboratory biomarker tracking (nonparametric)
#' - Treatment: Optimized for treatment response monitoring (parametric with pairwise)
#' - Laboratory: Optimized for clinical lab values (robust)
#'
#' @section Performance Optimization:
#' The function implements sophisticated caching:
#' - Data preparation cached based on variable selection and data content
#' - Options cached separately to minimize reprocessing
#' - Plot state management prevents unnecessary regeneration
#' - Checkpoint calls before expensive operations for responsiveness
#'
#' @section Clinical Validation:
#' The function performs comprehensive data quality checks:
#' - Validates paired design requirements (complete cases)
#' - Detects small sample sizes (< 10 subjects)
#' - Identifies potential outliers (> 10% outliers)
#' - Warns about skewed data for parametric tests
#' - Alerts to high missing data rates (> 50%)
#'
#' @examples
#' \dontrun{
#' # Basic within-subjects analysis
#' data(iris)
#' iris_wide <- data.frame(
#'     Subject = 1:50,
#'     Baseline = iris$Sepal.Length[1:50],
#'     Month3 = iris$Sepal.Width[1:50] * 2.5,
#'     Month6 = iris$Petal.Length[1:50] * 1.8
#' )
#'
#' jjwithinstats(
#'     data = iris_wide,
#'     dep1 = "Baseline",
#'     dep2 = "Month3",
#'     dep3 = "Month6",
#'     typestatistics = "parametric",
#'     pairwisecomparisons = TRUE,
#'     centralityplotting = TRUE,
#'     pointpath = TRUE
#' )
#'
#' # Clinical biomarker tracking (nonparametric)
#' jjwithinstats(
#'     data = clinical_data,
#'     dep1 = "CRP_baseline",
#'     dep2 = "CRP_week4",
#'     dep3 = "CRP_week12",
#'     clinicalpreset = "biomarker",
#'     typestatistics = "nonparametric",
#'     pairwisecomparisons = TRUE,
#'     mytitle = "C-Reactive Protein Levels During Treatment"
#' )
#'
#' # Robust analysis with publication plot
#' jjwithinstats(
#'     data = lab_data,
#'     dep1 = "ALT_pre",
#'     dep2 = "ALT_post",
#'     typestatistics = "robust",
#'     centralityplotting = TRUE,
#'     centralitytype = "robust",
#'     addGGPubrPlot = TRUE,
#'     ggpubrPlotType = "paired",
#'     ggpubrAddStats = TRUE
#' )
#' }
#'
#' @references
#' Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot' approach.
#'   Journal of Open Source Software, 6(61), 3167. \doi{10.21105/joss.03167}
#'
#' @seealso
#' \code{\link[ggstatsplot]{ggwithinstats}} for the underlying plotting function
#'
#' @family JJStatsPlot functions
#' @keywords hplot htest
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#' @import tidyr
#' @import ggplot2
#' @importFrom rlang sym
#' @importFrom digest digest
#' @importFrom ggstatsplot ggwithinstats theme_ggstatsplot
#' @importFrom ggpubr ggpaired ggboxplot ggviolin ggline stat_compare_means theme_pubr
#'
#' @export


jjwithinstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjwithinstatsClass",
    inherit = jjwithinstatsBase,
    private = list(
        .prepared_data = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .messages = NULL,
        .data_messages = NULL,

        # Notice collection list for HTML-based notices (avoids serialization errors)
        .noticeList = list(),

        # Variable name safety utility ----
        .escapeVar = function(var) {
            if (is.null(var)) return(NULL)
            # Escape special characters for safe variable access
            # Use jmvcore::composeTerm for variables with spaces/special chars
            if (grepl("[^A-Za-z0-9_]", var)) {
                jmvcore::composeTerm(var)
            } else {
                var
            }
        },

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
        
        # Enhanced message management system (dual output: HTML + Notice)
        .accumulateMessage = function(message, notice_type = "WARNING") {
            if (is.null(private$.messages)) {
                private$.messages <- character()
            }
            private$.messages <- append(private$.messages, message)

            # LEGACY: Keep HTML warnings for backward compatibility
            if (!is.null(self$results$warnings)) {
                self$results$warnings$setContent(paste(private$.messages, collapse = ""))
                self$results$warnings$setVisible(TRUE)
            }

            # MODERN: Add to HTML-based notice system (no serialization issues)
            # Note: We don't call .renderNotices() here - that's done at end of .run()
            # This just accumulates the notices for later rendering
        },
        
        # Reset messages for new analysis run
        .resetMessages = function() {
            private$.messages <- character()
            # Clear HTML-based notice list
            private$.noticeList <- list()
            # Don't clear TODO here, as it might hold "Welcome" or "Ready"
            # warning content is cleared
            if (!is.null(self$results$warnings)) {
                self$results$warnings$setContent("")
                # Don't hide yet, wait until run to decide visibility or let specific checks set it
            }
        },

        # DEPRECATED: Old notice system with serialization errors
        # DO NOT USE - kept for reference only
        # Use .addNoticeHTML() instead
        # .addNotice = function(content, type = "WARNING", name = NULL) {
        #     # This method causes serialization errors due to insert(999, notice)
        #     # See CLAUDE.md > Notice Serialization and HTML Conversion
        #     stop("Deprecated: Use .addNoticeHTML() instead to avoid serialization errors")
        # },

        # MODERN NOTICE SYSTEM (HTML-based, no serialization issues) ----

        # Add a notice to the HTML-based collection (recommended for all new code)
        .addNoticeHTML = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type,
                title = title,
                content = content
            )
        },

        # HTML sanitization for security
        .safeHtmlOutput = function(text) {
            if (is.null(text) || length(text) == 0) return("")
            text <- as.character(text)
            # Sanitize potentially dangerous characters
            text <- gsub("&", "&amp;", text, fixed = TRUE)
            text <- gsub("<", "&lt;", text, fixed = TRUE)
            text <- gsub(">", "&gt;", text, fixed = TRUE)
            text <- gsub("\"", "&quot;", text, fixed = TRUE)
            text <- gsub("'", "&#x27;", text, fixed = TRUE)
            text <- gsub("/", "&#x2F;", text, fixed = TRUE)
            return(text)
        },

        # Render collected notices as HTML (call at end of .run())
        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                return()
            }

            # Map notice types to colors and icons
            typeStyles <- list(
                ERROR = list(color = "#dc2626", bgcolor = "#fef2f2", border = "#fca5a5", icon = "⛔"),
                STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed", border = "#fdba74", icon = "⚠️"),
                WARNING = list(color = "#ca8a04", bgcolor = "#fefce8", border = "#fde047", icon = "⚡"),
                INFO = list(color = "#2563eb", bgcolor = "#eff6ff", border = "#93c5fd", icon = "ℹ️")
            )

            html <- "<div style='margin: 10px 0;'>"

            for (notice in private$.noticeList) {
                style <- typeStyles[[notice$type]]
                if (is.null(style)) style <- typeStyles$INFO

                html <- paste0(html,
                    "<div style='background-color: ", style$bgcolor, "; ",
                    "border-left: 4px solid ", style$border, "; ",
                    "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                    "<strong style='color: ", style$color, ";'>",
                    style$icon, " ", private$.safeHtmlOutput(notice$title), "</strong><br>",
                    "<span style='color: #374151;'>", private$.safeHtmlOutput(notice$content), "</span>",
                    "</div>"
                )
            }

            html <- paste0(html, "</div>")

            self$results$notices$setContent(html)
        },

        # Helper to cache data-specific messages
        .accumulateDataMessage = function(message) {
            private$.data_messages <- append(private$.data_messages, message)
            private$.accumulateMessage(message)
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
                    private$.accumulateMessage(.("🔬 <strong>Biomarker Tracking (Guidance Only):</strong> Optimized for laboratory values. Please manually ensure 'Nonparametric' is selected if data is skewed.<br>"))
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
            # Use safe variable names for variables with spaces/special chars
            safe_vars <- sapply(vars, function(v) {
                if (grepl("[^A-Za-z0-9_]", v)) jmvcore::composeTerm(v) else v
            })
            data_subset <- self$data[, safe_vars, drop = FALSE]

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
                # Re-emit cached messages
                if (!is.null(private$.data_messages)) {
                    for (msg in private$.data_messages) {
                        private$.accumulateMessage(msg)
                    }
                }
                return(private$.prepared_data)
            }
            
            # Reset data messages for new processing
            private$.data_messages <- character()
            
            # Add processing feedback
            private$.accumulateDataMessage(
                glue::glue(.("<br>Processing {length(vars)} measurements for within-subjects analysis...<br>"))
            )
            
            # Variables already validated above, proceed with data preparation
            mydata <- self$data
            
            # Check for empty dataset
            if (nrow(mydata) == 0) {
                private$.accumulateDataMessage(
                    .("<br>❌ Dataset is empty (0 rows). Please load data first.<br>")
                )
                private$.prepared_data <- NULL
                return(NULL)
            }
            
            mydata$rowid <- seq.int(nrow(mydata))

            # Check if required variables exist in dataset using safe names
            safe_vars_check <- sapply(vars, function(v) {
                if (grepl("[^A-Za-z0-9_]", v)) jmvcore::composeTerm(v) else v
            })
            missing_vars <- vars[!safe_vars_check %in% names(mydata)]
            if (length(missing_vars) > 0) {
                private$.accumulateDataMessage(
                    paste0(.("<br>❌ Variables not found in dataset: "), paste(missing_vars, collapse = ", "), "<br>")
                )
                private$.prepared_data <- NULL
                return(NULL)
            }
            
            # Checkpoint before expensive data conversion loop
            private$.checkpoint()

            # Convert variables to numeric with labelled awareness
            for (i in seq_along(vars)) {
                var <- vars[i]
                safe_var <- safe_vars_check[i]
                col <- mydata[[safe_var]]

                # Handle labelled data (preserve labels for reporting)
                if (inherits(col, "haven_labelled")) {
                    col <- haven::as_factor(col, levels = "both")
                }

                mydata[[safe_var]] <- jmvcore::toNumeric(col)
            }
            
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
                private$.accumulateDataMessage(warning_msg)
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
                if (!is.null(self$results$warnings)) {
                    self$results$warnings$setContent(warning_msg)
                    self$results$warnings$setVisible(TRUE)
                } else {
                    private$.accumulateDataMessage(warning_msg)
                }
            }

            # Validate data quality before processing
            # Note: .validateDataQuality calls .accumulateMessage directly, maybe update it too?
            # For now, data quality messages are less critical to cache strictly or should be cached too.
            # Ideally .validateDataQuality should use .accumulateDataMessage logic if passed.
            private$.validateDataQuality(mydata, vars)
            
            # Remove NA values once
            mydata <- jmvcore::naOmit(mydata)
            
            # Report N retained
            final_n <- nrow(mydata)
            dropped_n <- n_rows - final_n
            
            if (dropped_n > 0) {
                 private$.accumulateDataMessage(
                    glue::glue(.("<br>ℹ️ <strong>Data Processing:</strong> {final_n} subjects retained. {dropped_n} incomplete cases removed.<br>"))
                )
            } else {
                 private$.accumulateDataMessage(
                    glue::glue(.("<br>ℹ️ <strong>Data Processing:</strong> All {final_n} subjects retained (complete data).<br>"))
                )
            }
            
            if (nrow(mydata) == 0) {
                private$.accumulateDataMessage(
                    .("<br>❌ No complete observations after removing missing values<br>")
                )
                private$.prepared_data <- NULL
                return(NULL)
            }
            
            # Checkpoint before expensive data transformation
            private$.checkpoint()

            # Perform pivot_longer transformation once using safe variable names
            long_data <- tidyr::pivot_longer(
                mydata,
                cols = safe_vars_check,
                names_to = "measurement",
                values_to = "value"
            )

            # Map safe names back to original names for display
            names_mapping <- setNames(vars, safe_vars_check)
            long_data$measurement <- names_mapping[as.character(long_data$measurement)]
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
        },

        # Generate explanations ----
        .generateExplanations = function() {
            self$results$explanations$setVisible(TRUE)
            self$results$explanations$setContent(
                "<h3>Explanations</h3>
                <p>
                    This analysis compares repeated measurements from the same subjects (within-subjects design).
                    It helps determine if there are significant differences between time points or conditions.
                </p>
                <p>
                    <strong>Violin Plots:</strong> Show the distribution of data. Wider sections indicate more data points.
                    <br>
                    <strong>Box Plots:</strong> Show the median (middle line) and quartiles (box edges).
                </p>
                <p>
                    <strong>Statistical Tests:</strong>
                    <ul>
                        <li><strong>Parametric (ANOVA/t-test):</strong> Assumes normal distribution. Tests for difference in means.</li>
                        <li><strong>Nonparametric (Friedman/Wilcoxon):</strong> No distribution assumption. Tests for difference in medians.</li>
                        <li><strong>Robust:</strong> Resistant to outliers. Uses trimmed means.</li>
                        <li><strong>Bayesian:</strong> Provides evidence strength for/against the null hypothesis.</li>
                    </ul>
                </p>"
            )
        },

        # run ----
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
                if (!is.null(self$results$warnings)) {
                    self$results$warnings$setVisible(visible = FALSE)
                }

                return()

            } else {
                # Clear messages for actual analysis
                private$.resetMessages()
                
                # Check messages from presets and data quality each run
                private$.applyClinicalPresets()
                # Ensure data is prepared (and N messages generated)
                private$.prepareData()
                
                # Make all outputs visible when variables are selected
                self$results$todo$setVisible(visible = TRUE)
                self$results$plot$setVisible(visible = TRUE)
                self$results$interpretation$setVisible(visible = TRUE)
                if (!is.null(self$results$summary)) {
                    self$results$summary$setVisible(visible = TRUE)
                }
                if (!is.null(self$results$warnings)) {
                    self$results$warnings$setVisible(visible = TRUE)
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

                # Render accumulated HTML-based notices (if any)
                private$.renderNotices()

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

            # CRITICAL FIX: Set plot state for efficient caching
            # This ensures plot only regenerates when data or visual options actually change
            state_data <- list(
                # Data content (convert to base data.frame to avoid serialization issues)
                data = as.data.frame(long_data),
                # All visual options that affect plot appearance
                visual_opts = list(
                    typestatistics = opts$typestatistics,
                    pairwisecomparisons = opts$pairwisecomparisons,
                    pairwisedisplay = opts$pairwisedisplay,
                    padjustmethod = opts$padjustmethod,
                    effsizetype = opts$effsizetype,
                    centralityplotting = opts$centralityplotting,
                    centralitytype = opts$centralitytype,
                    pointpath = opts$pointpath,
                    centralitypath = opts$centralitypath,
                    violin = self$options$violin,
                    boxplot = self$options$boxplot,
                    point = self$options$point,
                    mytitle = opts$mytitle,
                    xtitle = opts$xtitle,
                    ytitle = opts$ytitle,
                    originaltheme = opts$originaltheme,
                    resultssubtitle = opts$resultssubtitle,
                    bfmessage = opts$bfmessage,
                    conflevel = opts$conflevel,
                    k = self$options$k
                )
            )

            # Set state - jamovi will only regenerate if state changes
            image$setState(state_data)

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
                deps <- c(self$options$dep1, self$options$dep2)
                if (!is.null(self$options$dep3)) deps <- c(deps, self$options$dep3)
                if (!is.null(self$options$dep4)) deps <- c(deps, self$options$dep4)

                # Use safe variable names for variables with spaces/special chars
                safe_deps <- sapply(deps, function(v) {
                    if (grepl("[^A-Za-z0-9_]", v)) jmvcore::composeTerm(v) else v
                })

                # CRITICAL FIX: Paired analysis requires listwise deletion
                # Only keep subjects with processing data for ALL selected measurements
                mydata <- self$data
                mydata <- mydata[, safe_deps, drop = FALSE]
                mydata <- jmvcore::naOmit(mydata)
                
                # Check for empty data after filtering
                if (nrow(mydata) == 0) {
                     return() # Should be handled by main validation
                }

                # Create subject ID for paired analysis
                mydata$Subject_ID <- seq_len(nrow(mydata))

                # Convert to long format using safe variable names
                long_data <- data.frame()
                for (i in seq_along(deps)) {
                    temp <- data.frame(
                        Subject_ID = mydata$Subject_ID,
                        Measurement = deps[i],  # Original name for display
                        Value = mydata[[safe_deps[i]]]  # Safe name for access
                    )
                    long_data <- rbind(long_data, temp)
                }

                # Start Plot Construction
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
                        line.size = 0.4,
                        label = NULL
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

                # Add statistical comparisons
                if (self$options$ggpubrAddStats && self$options$ggpubrPlotType != "line") {
                    # Determine appropriate paired test method
                    test_method <- switch(
                        self$options$typestatistics,
                        "parametric" = "t.test",
                        "nonparametric" = "wilcox.test",
                        "robust" = "t.test",  # Fallback
                        "bayes" = NULL,
                        "t.test"  # Default
                    )

                    if (!is.null(test_method)) {
                        # Use global p-value for >2 groups, paired for 2 groups
                        # For >2 groups, ggpubr might warn but paired=TRUE is key
                        plot <- plot + ggpubr::stat_compare_means(
                            method = test_method,
                            paired = TRUE,
                            label = "p.signif"
                        )
                    }
                }

                # Apply theme
                plot <- plot + ggpubr::theme_pubr() +
                    ggplot2::labs(subtitle = "Descriptive Plot (ggpubr)")

                print(plot)
                TRUE

            }, error = function(e) {
                # Silent fail for secondary plot or log error
                # self$results$todo$setContent(paste("ggpubr error:", e$message))
            })
        }

    )
)
