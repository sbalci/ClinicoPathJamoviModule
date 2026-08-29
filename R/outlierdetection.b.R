#' @title Outlier Detection with easystats
#' 
#' @description
#' Advanced outlier detection using multiple statistical methods from the easystats performance package.
#' This function provides comprehensive outlier detection through univariate methods (Z-scores, IQR, confidence intervals),
#' multivariate methods (Mahalanobis distance, MCD, OPTICS, LOF), and composite scoring across multiple algorithms.
#' Complements existing data quality assessment modules with state-of-the-art outlier detection capabilities.
#' Perfect for clinical research data quality control and preprocessing.
#'
#' @details
#' The outlier detection module supports four main categories of methods:
#' 
#' **Univariate Methods:**
#' - **Robust Z-Score (MAD-based):** Uses median absolute deviation for robust standardization
#' - **Standard Z-Score:** Classical z-score based on mean and standard deviation
#' - **Interquartile Range (IQR):** Tukey's method using quartiles and IQR multiplier
#' - **Equal-Tailed Interval (ETI):** Symmetric confidence interval approach
#' - **Highest Density Interval (HDI):** Bayesian credible interval method
#' 
#' **Multivariate Methods:**
#' - **Mahalanobis Distance:** Classical multivariate distance accounting for covariance
#' - **Robust Mahalanobis Distance:** Robust version using minimum covariance determinant
#' - **Minimum Covariance Determinant (MCD):** Robust covariance estimation
#' - **OPTICS Clustering:** Density-based clustering approach
#' - **Local Outlier Factor (LOF):** Local density deviation method
#' 
#' \strong{Composite Methods:} Combine multiple algorithms for robust detection with adjustable thresholds
#' 
#' \strong{All Methods:} Comprehensive analysis using all available techniques
#'
#' @section Method Selection Guidelines:
#' - **Univariate:** When analyzing variables independently, simple interpretation needed
#' - **Multivariate:** When variable relationships matter, detecting complex outlier patterns
#' - **Composite:** When robust detection across different data patterns is needed
#' - **All:** For comprehensive analysis and method comparison
#'
#' @section Threshold Recommendations:
#' - **Z-Score:** 3.29 (99.9 percent confidence, ~0.1 percent outliers)
#' - **IQR Multiplier:** 1.7 (more conservative than Tukey's 1.5)
#' - **Confidence Level:** 0.999 (99.9 percent for interval methods)
#' - **Composite Threshold:** 0.5 (outliers detected by 50 percent or more of methods)
#'
#' @section Clinical Applications:
#' - **Laboratory Data:** CBC, chemistry panels, liver function tests
#' - **Anthropometric Data:** Height, weight, BMI measurements
#' - **Physiological Data:** Blood pressure, heart rate, temperature
#' - **Biomarker Data:** Protein levels, genetic markers, metabolites
#' - **Quality Control:** Data entry errors, instrument malfunctions
#'
#' @section Output Components:
#' - **Outlier Table:** Detailed results with outlier scores and classifications
#' - **Method Comparison:** Performance across different detection algorithms
#' - **Exclusion Summary:** Recommendations for data cleaning procedures
#' - **Visualization:** Plots showing outlier patterns and distributions
#' - **Interpretation:** Detailed guidance on results and methodology
#'
#' @section Statistical Considerations:
#' - **Sample Size:** Minimum 30 observations recommended for robust results
#' - **Distribution:** Robust methods handle non-normal distributions better
#' - **Missing Data:** Complete cases analysis performed automatically
#' - **Correlations:** Multivariate methods account for variable relationships
#' - **False Positives:** Conservative thresholds reduce over-detection
#'
#' @section References:
#' - Ludecke, D., Ben-Shachar, M., Patil, I., Waggoner, P., & Makowski, D. (2021). 
#'   performance: An R Package for Assessment, Comparison and Testing of Statistical Models. 
#'   Journal of Open Source Software, 6(60), 3139. https://doi.org/10.21105/joss.03139
#' - Rousseeuw, P. J., & Hubert, M. (2018). Anomaly detection by robust statistics. 
#'   Wiley Interdisciplinary Reviews: Data Mining and Knowledge Discovery, 8(2), e1236.
#' - Breunig, M. M., Kriegel, H. P., Ng, R. T., & Sander, J. (2000). LOF: identifying 
#'   density-based local outliers. ACM sigmod record, 29(2), 93-104.
#'
#' @examples
#' \dontrun{
#' # All examples use the 250-row `histopathology` dataset bundled with the module.
#' data(histopathology)
#'
#' # Example 1: univariate outlier detection on a single lab-style measurement
#' outlierdetection(
#'   data = histopathology,
#'   vars = c("Age", "MeasurementA", "MeasurementB"),
#'   method_category = "univariate",
#'   univariate_methods = "zscore_robust",
#'   zscore_threshold = 3.29,
#'   show_outlier_table = TRUE
#' )
#'
#' # Example 2: multivariate detection across correlated measurements.
#' # NOTE: multivariate_methods = "mahalanobis_robust" is routed through the
#' # bigutilsr package by performance; where bigutilsr is not available the
#' # analysis reports that and produces no result. "mcd" needs only robustbase,
#' # which ships with the module, so it is the portable robust alternative.
#' outlierdetection(
#'   data = histopathology,
#'   vars = c("MeasurementA", "MeasurementB", "OverallTime"),
#'   method_category = "multivariate",
#'   multivariate_methods = "mahalanobis",
#'   show_method_comparison = TRUE,
#'   show_exclusion_summary = TRUE
#' )
#'
#' # Example 3: composite detection (the default category).
#' # In this category the Z-score threshold and IQR multiplier below are passed
#' # through to the individual methods.
#' outlierdetection(
#'   data = histopathology,
#'   vars = c("Age", "OverallTime", "MeasurementA"),
#'   method_category = "composite",
#'   composite_threshold = 0.6,
#'   zscore_threshold = 3.0,
#'   iqr_multiplier = 1.5,
#'   show_outlier_table = TRUE,
#'   show_interpretation = TRUE
#' )
#'
#' # Example 4: every method that is available in this installation.
#' # Techniques whose optional packages are absent are reported in Analysis
#' # Messages and left out of the composite rather than failing the run.
#' outlierdetection(
#'   data = histopathology,
#'   vars = c("Age", "MeasurementA", "MeasurementB"),
#'   method_category = "all",
#'   show_method_comparison = TRUE,
#'   show_exclusion_summary = TRUE
#' )
#' }
#'
#' @seealso
#' \code{\link[performance]{check_outliers}} for the underlying outlier detection functions
#' 
#' @keywords outlier detection, data quality, clinical research, statistical analysis
#' @concept data preprocessing
#' @concept quality control
#' @concept robust statistics
#' @concept multivariate analysis
#' 
#' @return A jamovi analysis object containing outlier detection results with tables, 
#'         plots, and interpretation based on selected options
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes labs theme_minimal theme element_text
#' @importFrom ggplot2 geom_point geom_hline scale_color_manual
#' @importFrom performance check_outliers
#' @importFrom dplyr mutate row_number
#' @importFrom htmltools HTML
#' @importFrom stringr str_to_title
#' @importFrom dbscan optics lof
#' @importFrom robustbase covMcd
outlierdetectionClass <- if (requireNamespace("jmvcore")) R6::R6Class("outlierdetectionClass",
    inherit = outlierdetectionBase,
    private = list(
        
        .messages = NULL,
        .warningsBlocks = NULL,

        # Read an option that may not be present in the compiled .h.R yet.
        # jmvcore's `$` ERRORS on an undeclared option rather than returning NULL,
        # so a newly added option would crash every run until jmvtools::prepare()
        # regenerates the header. Fall back to the documented default instead.
        .optionOr = function(name, fallback) {
            val <- tryCatch(self$options[[name]], error = function(e) NULL)
            if (is.null(val) || (length(val) == 1 && is.na(val))) fallback else val
        },

        .accumulateMessage = function(msg) {
            private$.messages <- c(private$.messages, msg)
        },

        # Accumulate a full HTML block (validation summary, sampling notice, error)
        # to be shown in the always-visible `warnings` output.
        .addWarningsBlock = function(html) {
            private$.warningsBlocks <- c(private$.warningsBlocks, html)
        },

        .resetMessages = function() {
            private$.messages <- NULL
            private$.warningsBlocks <- NULL
            self$results$warnings$setContent("")
        },

        # Compose all accumulated warning blocks + list messages into the
        # always-visible `warnings` panel. Safe to call from early-return paths.
        .renderWarnings = function() {
            parts <- character(0)
            if (length(private$.warningsBlocks) > 0)
                parts <- c(parts, private$.warningsBlocks)
            if (length(private$.messages) > 0) {
                parts <- c(parts, paste0(
                    "<div class='alert alert-warning'>",
                    "<h6>Analysis Messages</h6>",
                    "<ul>",
                    paste(paste0("<li>", private$.messages, "</li>"), collapse = ""),
                    "</ul></div>"
                ))
            }
            if (length(parts) > 0)
                self$results$warnings$setContent(paste(parts, collapse = ""))
        },

        .createHTMLSection = function(title, content, style = "info", icon = NULL) {
            # Helper function to create consistent HTML sections
            styles <- list(
                info = "background-color: rgba(33, 152, 239, 0.13); color: inherit;",
                warning = "background-color: rgba(255, 169, 33, 0.14); color: inherit;",
                error = "background-color: rgba(255, 33, 67, 0.09); color: inherit;",
                success = "background-color: rgba(33, 159, 43, 0.1); color: inherit;",
                neutral = "background-color: rgba(88, 88, 88, 0.06); color: inherit;"
            )

            icon_html <- if (!is.null(icon)) paste0(icon, " ") else ""

            paste0(
                "<div style='", styles[[style]], " padding: 15px; border-radius: 8px; margin: 10px 0;'>",
                "<h4 style='margin-top: 0;'>", icon_html, title, "</h4>",
                content,
                "</div>"
            )
        },

        .run = function() {
            # Security note: user-derived variable names / CSV headers that reach
            # HTML output ARE escaped via htmltools::htmlEscape() (col at L367/L1018;
            # error/warning/info at L1480-L1502; condition messages at L458). When
            # adding new HTML that interpolates a dynamic (variable name, factor
            # label, CSV header), escape it the same way. (Escaping is present here
            # -- do not remove it.)
            # TODO (forward-looking): no `.()` wrapping in this file (~1.5k
            # LOC of educational and method-description text). Address in
            # a /prepare-translation pass.
            # Reset messages
            private$.resetMessages()

            # Check if required variables have been selected
            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                intro_msg <- "
                <div style='background-color: rgba(33, 152, 239, 0.13); padding: 20px; border-radius: 8px; margin: 20px 0; color: inherit;'>
                <h3 style='margin-top: 0;'> Welcome to Outlier Detection!</h3>
                <p><strong>Comprehensive outlier detection using easystats performance package</strong></p>
                <p>Complements existing ClinicoPath data quality modules with state-of-the-art detection methods</p>

                <h4>Required Variables:</h4>
                <ol>
                <li><strong>Variables for Analysis:</strong> Select continuous variables to analyze for outliers</li>
                </ol>

                <h4>Detection Methods Available:</h4>
                <ul>
                <li><strong>Univariate Methods:</strong> Z-scores (robust/standard), IQR, confidence intervals</li>
                <li><strong>Multivariate Methods:</strong> Mahalanobis distance, MCD, OPTICS, LOF</li>
                <li><strong>Composite Scoring:</strong> Combines multiple methods for robust detection</li>
                <li><strong>All Methods:</strong> Comprehensive analysis using all available techniques</li>
                </ul>

                <h4>Perfect For:</h4>
                <ul>
                <li><strong>Clinical Data Quality:</strong> Identify problematic observations in patient data</li>
                <li><strong>Research Preprocessing:</strong> Clean datasets before statistical analysis</li>
                <li><strong>Exploratory Analysis:</strong> Understand data distribution and extreme values</li>
                <li><strong>Method Comparison:</strong> Compare different outlier detection approaches</li>
                <li><strong>Publication Preparation:</strong> Document outlier handling procedures</li>
                </ul>

                <h4>Key Features:</h4>
                <ul>
                <li><strong>Multiple Algorithms:</strong> 10+ detection methods from easystats ecosystem</li>
                <li><strong>Robust Thresholds:</strong> Conservative defaults based on research literature</li>
                <li><strong>Comprehensive Output:</strong> Tables, plots, and exclusion recommendations</li>
                <li><strong>Method Documentation:</strong> Detailed interpretation and citation guidance</li>
                </ul>

                <p style='font-size: 12px; opacity: 0.8; margin-top: 20px;'>
                 <em>State-of-the-art outlier detection for clinical research and data quality control</em>
                </p>
                </div>"

                self$results$todo$setContent(intro_msg)
                self$results$todo$setVisible(TRUE)
                return()
            } else {
                self$results$todo$setVisible(FALSE)
            }

            # Validate dataset
            if (nrow(self$data) == 0) {
                error_msg <- private$.createHTMLSection(
                    "Empty Dataset",
                    "Dataset contains no rows. Please provide data with at least one observation.",
                    style = "error",
                    icon = ""
                )
                private$.addWarningsBlock(error_msg)
                private$.renderWarnings()
                return()
            }

            # Safely require performance package
            if (!requireNamespace("performance", quietly = TRUE)) {
                error_msg <- private$.createHTMLSection(
                    "Missing Package",
                    'The performance package, which this analysis depends on, is not available in this session. It ships with the module, so this usually means a broken or partial installation - reinstall the module. No outlier results can be produced until it loads.',
                    style = "error",
                    icon = ""
                )
                private$.addWarningsBlock(error_msg)
                private$.renderWarnings()
                return()
            }
            
            # Check for additional dependencies based on the methods that will
            # ACTUALLY run.
            #
            # This block used to key every check on self$options$multivariate_methods
            # for method_category in c("multivariate", "composite", "all"), but that
            # option is only read on the multivariate branch: composite hard-codes
            # c("zscore_robust", "iqr", "mahalanobis") and "all" builds its own set.
            # A user in the default composite category whose (disabled, invisible)
            # multivariate combobox still read "lof" from an earlier session was
            # therefore told about a method that was not running.
            method_category <- self$options$method_category
            if (method_category == "multivariate") {
                multivariate_method <- self$options$multivariate_methods

                # LOF's neighbourhood size is not a free parameter here: the
                # performance package hard-codes minPts = ncol(x), the number of
                # VARIABLES, which is far smaller than the 10-20 neighbours the LOF
                # literature normally assumes. Measured on 303 bivariate rows with
                # 3 planted joint outliers: minPts = 2 flags 11 points, minPts = 10
                # or 20 flag 4 - all settings catch the 3 real ones, so the extra
                # 7 are false positives. The user choosing LOF has no control over
                # this, so tell them what it means for their result.
                if (multivariate_method == "lof") {
                    private$.accumulateMessage(paste0(
                        "<strong>About Local Outlier Factor:</strong> the neighbourhood size is fixed ",
                        "at the number of selected variables by the underlying package, which is small ",
                        "by the standards of the LOF literature. LOF therefore tends to flag more points ",
                        "than the other multivariate methods on the same data, and the extra flags are ",
                        "usually borderline rather than erroneous. Cross-check anything it flags against ",
                        "Mahalanobis distance or MCD before excluding it."))
                }

                # Classical Mahalanobis measures each row against a covariance
                # matrix estimated from the same rows, so a group of observations
                # that are extreme in the same direction inflates the ellipsoid that
                # is meant to exclude them (Rousseeuw & Van Zomeren 1990 masking).
                # Verified with performance 0.17.1 on 200 rows, 3 variables with
                # rho = 0.6 and 20 rows shifted +12 SD on one variable: this method
                # flagged 1 of the 20 planted rows, MCD on byte-identical data
                # flagged 20 of 20. This is the DEFAULT multivariate choice, so the
                # low count it can return is worth stating up front.
                if (multivariate_method == "mahalanobis") {
                    private$.accumulateMessage(paste0(
                        "<strong>About Mahalanobis distance:</strong> the covariance matrix each ",
                        "observation is measured against is estimated from the same rows being ",
                        "screened. A group of observations that are extreme in the same direction ",
                        "therefore widens that covariance and can fall inside its own boundary, so ",
                        "this method can report few or no outliers on data that contain several. ",
                        "<em>Minimum covariance determinant (MCD)</em> estimates the covariance from ",
                        "the most tightly grouped subset of the rows instead, so it resists this up ",
                        "to roughly half the sample; run it as a cross-check, particularly when the ",
                        "count here is zero."))
                }

                # Check for dbscan package for clustering methods
                if (multivariate_method %in% c("optics", "lof") &&
                    !requireNamespace("dbscan", quietly = TRUE)) {
                    private$.accumulateMessage(sprintf(
                        '<strong>Missing Package:</strong> The %s method needs the dbscan package, which is not available in this session. It ships with the module, so this usually means a broken installation; meanwhile, choose Mahalanobis distance or MCD instead.',
                        multivariate_method
                    ))
                }

                # Check for robustbase package for robust methods
                if (multivariate_method == "mcd" &&
                    !requireNamespace("robustbase", quietly = TRUE)) {
                    private$.accumulateMessage(
                        '<strong>Missing Package:</strong> The MCD method needs the robustbase package, which is not available in this session. It ships with the module, so this usually means a broken installation; meanwhile, choose Mahalanobis distance or a univariate method instead.'
                    )
                }

                # Robust Mahalanobis is routed through bigutilsr by the performance
                # package. bigutilsr is a declared Import of ClinicoPath but not of
                # every submodule that ships this analysis, so the availability is
                # decided at runtime - say so BEFORE the run rather than letting it
                # surface as a raw package error. (Do not state unconditionally that
                # the package is missing: it is present in some builds.)
                if (multivariate_method == "mahalanobis_robust" &&
                    !requireNamespace("bigutilsr", quietly = TRUE)) {
                    private$.accumulateMessage(
                        '<strong>Method Not Available:</strong> Robust Mahalanobis distance is computed through the bigutilsr package, which is not available in this session, so this run cannot produce a result. Choose <em>Minimum covariance determinant (MCD)</em> for a robust multivariate distance, or <em>Mahalanobis distance</em> for the classical one.'
                    )
                }
            } else if (method_category == "all") {
                # "All methods" is assembled from whatever is loadable; tell the
                # user up front which techniques are not in the set, so a smaller
                # composite denominator is not mistaken for a complete comparison.
                all_set <- private$.allMethodSet()
                if (length(all_set$missing) > 0) {
                    private$.accumulateMessage(sprintf(
                        '<strong>Methods Excluded From "All":</strong> %d of the available techniques could not be included because their supporting packages are not installed with this module (%s). The analysis ran with %d method(s): %s. The composite proportion below is therefore computed over those methods only.',
                        length(all_set$missing),
                        htmltools::htmlEscape(paste(all_set$missing, collapse = "; ")),
                        length(all_set$methods),
                        htmltools::htmlEscape(paste(all_set$methods, collapse = ", "))
                    ))
                }
            }

            # Get data and variables
            dataset <- self$data
            selected_vars <- self$options$vars
            
            # Prepare analysis data
            if (length(selected_vars) == 0) {
                return()
            }
            
            # Perform comprehensive input validation
            validation_results <- private$.validateInputs(dataset, selected_vars)
            
            # Show validation summary if there are issues (routed to the always-visible
            # `warnings` output; `interpretation` is reserved for the interpretation guide).
            if (length(validation_results$warnings) > 0 || length(validation_results$info) > 0
                || length(validation_results$errors) > 0) {
                validation_html <- private$.generateValidationSummary(validation_results)
                private$.addWarningsBlock(validation_html)
            }

            # Stop if critical errors found
            if (validation_results$should_stop) {
                private$.renderWarnings()
                return()
            }
            
            analysis_data <- dataset[selected_vars]

            # Handle single variable case - ensure it's a data frame
            if (is.vector(analysis_data) || is.factor(analysis_data)) {
                analysis_data <- data.frame(var = analysis_data)
                names(analysis_data) <- selected_vars
            }

            # Listwise deletion. Every count reported downstream - the plain-language
            # summary, the copy-ready report sentence, the exclusion table - is taken
            # from `original_n` below, i.e. from the COMPLETE CASES, so the rows lost
            # here have to be stated explicitly or they are never mentioned anywhere.
            # The validation summary reports per-variable missing percentages, which
            # is a different and much smaller number when the missingness is spread
            # across several variables (8 variables at 10% missing each can cost well
            # over half the rows jointly).
            n_input <- nrow(analysis_data)
            analysis_data <- analysis_data[complete.cases(analysis_data), , drop = FALSE]

            # CRITICAL FIX: Preserve original dataset size before sampling
            original_n <- nrow(analysis_data)

            if (n_input > original_n) {
                private$.accumulateMessage(sprintf(
                    paste0('<strong>Rows Excluded For Missing Data:</strong> %d of %d rows (%.1f%%) ',
                           'were dropped because at least one selected variable was missing. ',
                           'The analysis, and every count reported below, refers to the %d ',
                           'complete cases.'),
                    n_input - original_n, n_input,
                    100 * (n_input - original_n) / n_input, original_n))
            }
            
            # Clinical Assumption Checklist
            # 1. Sample Size Check
            if (original_n < 30) {
                private$.accumulateMessage(sprintf(
                    '<strong>Small Sample:</strong> Sample size is small (N=%d). Outlier detection may be unreliable. Recommended N >= 30.',
                    original_n
                ))
            }
            
            # 2. Skewness Check for Classical Methods
            # Only relevant if using standard Z-score or Mahalanobis (but good to warn generally)
            if (self$options$method_category %in% c("univariate", "multivariate", "composite")) {
                 for (col in names(analysis_data)) {
                     if (is.numeric(analysis_data[[col]])) {
                         # Simple skewness check
                         x <- analysis_data[[col]]
                         n <- length(x)
                         m3 <- sum((x - mean(x))^3) / n
                         s3 <- sd(x)^3
                         skew <- m3 / s3

                         if (!is.na(skew) && abs(skew) > 2) {
                            private$.accumulateMessage(sprintf(
                                '<strong>High Skewness:</strong> Variable %s is highly skewed (%.2f). Standard methods may flag valid values. Consider Robust Z-score.',
                                htmltools::htmlEscape(col), skew
                            ))
                         }
                     }
                 }
            }

            # Performance optimization for large datasets.
            # Threshold and retained size are user-configurable; the defaults
            # (10000 / 5000) reproduce the previously hard-coded behaviour.
            sample_threshold <- private$.optionOr("sampleThreshold", 10000)
            sample_size_opt <- private$.optionOr("sampleSize", 5000)
            # Only a sane lower bound here. Do NOT clamp to sample_threshold: the two
            # options are independent - the threshold decides WHEN to subsample, the
            # size decides HOW MANY rows to keep. Clamping silently overrode an
            # explicit user choice (threshold 1000 + size 5000 analysed 1000 rows,
            # not 5000), contradicting the option's own help text "larger values
            # recover more of them at the cost of speed". The data-size cap that
            # actually matters is applied at the point of use below.
            sample_size_opt <- max(100, sample_size_opt)

            if (nrow(analysis_data) > min(5000, sample_threshold)) {
                performance_msg <- NULL

                # For very large datasets, subsample
                if (nrow(analysis_data) > sample_threshold) {
                    sample_size <- min(sample_size_opt, nrow(analysis_data))
                    # User-configurable seed for reproducible subsampling;
                    # falls back to 123 (previous fixed value) when unset.
                    seed_val <- self$options$seed
                    if (is.null(seed_val)) seed_val <- 123
                    # Save/restore the global RNG state so seeding the subsample
                    # does not perturb downstream reproducibility outside this run.
                    if (exists(".Random.seed", envir = .GlobalEnv)) {
                        .old_seed <- get(".Random.seed", envir = .GlobalEnv)
                        on.exit(assign(".Random.seed", .old_seed, envir = .GlobalEnv), add = TRUE)
                    } else {
                        on.exit(
                            if (exists(".Random.seed", envir = .GlobalEnv))
                                rm(".Random.seed", envir = .GlobalEnv),
                            add = TRUE
                        )
                    }
                    set.seed(seed_val)
                    sample_idx <- sample(nrow(analysis_data), sample_size)
                    # Keep original_n preserved from above
                    analysis_data <- analysis_data[sample_idx, , drop = FALSE]

                    performance_msg <- private$.createHTMLSection(
                        "Performance Optimization",
                        paste0(
                            "<p><strong>Large dataset detected:</strong> ",
                            "The selected variables give ", original_n, " complete cases, which is above the ",
                            "subsampling threshold of ", sample_threshold, ". ",
                            "For faster analysis, we've sampled ", sample_size, " observations.</p>",
                            "<p><strong>What this costs you:</strong> ",
                            "A random subsample shows systematic problems well, but it can only find the outliers it ",
                            "happens to contain - roughly ", round(100 * sample_size / original_n), "% of them here. ",
                            "If you are screening for individual erroneous values to verify against source records, ",
                            "analyse the full dataset instead.</p>",
                            "<p><em>To analyse every row, raise <strong>Subsample above (rows)</strong> under ",
                            "Threshold Settings past ", original_n, "; to keep more of them, raise ",
                            "<strong>Rows to analyse when subsampling</strong>. Both are slower.</em></p>"
                        ),
                        style = "info",
                        icon = ""
                    )
                } else {
                    # For moderately large datasets, just notify
                    performance_msg <- private$.createHTMLSection(
                        "Processing Large Dataset",
                        paste0(
                            "<p>Analyzing ", nrow(analysis_data), " observations. ",
                            "This may take a moment for complex multivariate methods.</p>",
                            "<p><em>Tip: For faster results, consider univariate methods or analyze fewer variables at once.</em></p>"
                        ),
                        style = "info",
                        icon = ""
                    )
                }

                # Route sampling / large-dataset notice to the always-visible warnings panel
                if (!is.null(performance_msg)) {
                    private$.addWarningsBlock(performance_msg)
                }
            }

            if (is.null(analysis_data) || nrow(analysis_data) == 0) {
                error_msg <- private$.createHTMLSection(
                    "No Complete Cases",
                    "No complete cases found. Selected variables contain only missing values. Choose variables with complete data.",
                    style = "error",
                    icon = ""
                )
                # Compose onto everything accumulated so far instead of overwriting it.
                # setContent() replaces the whole shared `warnings` item, so the direct
                # call discarded the validation summary (which carries the per-variable
                # missing-data percentages - exactly the diagnosis needed here), the
                # small-sample note and the skewness notes.
                private$.addWarningsBlock(error_msg)
                private$.renderWarnings()
                return()
            }

            # Convert to numeric with safe variable access.
            # A bare as.numeric() on a factor returns LEVEL INDICES (1, 2, 3 ...),
            # not the labels, so every downstream outlier statistic would be wrong
            # by an unpredictable offset. `vars` is permitted:[numeric] so the GUI
            # never sends a factor, but the R API can. Convert through the labels
            # instead, and reject rather than silently analysing nonsense.
            # (jmvcore::toNumeric() is not a substitute here -- it is a no-op on
            # factors and characters, it only unwraps a `values` attribute.)
            for (var in selected_vars) {
                column <- analysis_data[[var]]
                if (is.factor(column))
                    column <- as.character(column)
                converted <- suppressWarnings(as.numeric(column))
                failed <- !is.na(column) & is.na(converted)
                if (any(failed))
                    jmvcore::reject(
                        .fmt(
                            .("Variable '{var}' contains {n} non-missing value(s) that cannot be converted to numbers, so outlier detection cannot run on it. Correct those values or select a continuous variable."),
                            var = var,
                            n = sum(failed)),
                        code = "non_numeric_variable")
                analysis_data[[var]] <- converted
            }

            # Initialize outlier_results variable
            outlier_results <- NULL

            # Perform outlier detection with proper error handling
            outlier_results <- tryCatch({
                result <- private$.perform_outlier_detection(analysis_data)
                result  # Return the result from tryCatch

            }, error = function(e) {
                error_msg <- paste0("
                <div style='color: inherit; background-color: rgba(216, 33, 50, 0.18); padding: 20px; border-radius: 8px;'>
                <h4> Outlier Detection Error</h4>
                <p><strong>Error:</strong> ", htmltools::htmlEscape(conditionMessage(e)), "</p>
                <p><strong>Method:</strong> ", self$options$method_category, "</p>
                <p><strong>Variables:</strong> ", ncol(analysis_data), " variable(s)</p>
                <p><strong>Observations:</strong> ", nrow(analysis_data), "</p>
                <h5>Common Solutions:</h5>
                <ul>
                <li>Try a different detection method (Robust Z-Score is most reliable)</li>
                <li>Check for infinite or extremely large values</li>
                <li>Ensure sufficient sample size (n \u{2265} 30 recommended)</li>
                <li>For multivariate methods, try with fewer variables</li>
                </ul>
                </div>")
                # Route failure to the always-visible warnings panel and render now,
                # since .run() returns early when outlier_results is NULL.
                private$.addWarningsBlock(error_msg)
                private$.renderWarnings()
                NULL  # Return NULL on error
            })

            # Check if outlier detection was successful
            if (is.null(outlier_results)) {
                # If outlier detection failed, don't proceed with generating outputs
                return()
            }

            # Generate plain-language summary
            plain_summary <- private$.generate_plain_summary(outlier_results, analysis_data, original_n)

            # Generate outputs with original dataset size
            if (self$options$show_outlier_table) {
                table_html <- private$.generate_outlier_table(outlier_results, analysis_data, original_n)
                # Combine plain summary with technical table
                combined_html <- paste0(plain_summary, table_html)
                self$results$outlier_table$setContent(combined_html)
            }

            if (self$options$show_method_comparison) {
                if (self$options$method_category %in% c("composite", "all")) {
                    comparison_html <- private$.generate_method_comparison(outlier_results)
                } else {
                    # The panel is shown whenever the checkbox is ticked, so an
                    # unexplained empty box appeared for anyone who ticked it in the
                    # univariate or multivariate category. Say why there is nothing
                    # to compare instead of rendering nothing.
                    comparison_html <- private$.createHTMLSection(
                        "Nothing to Compare",
                        paste0(
                            "<p>The ", private$.get_method_description(), " run applies a single ",
                            "detection method, so there is only one set of flags and no second ",
                            "method to compare it against.</p>",
                            "<p>Set <strong>Detection method category</strong> to ",
                            "<em>Composite (multiple methods)</em> or <em>All methods</em> to get a ",
                            "per-method breakdown here.</p>"
                        ),
                        style = "neutral",
                        icon = ""
                    )
                }
                self$results$method_comparison$setContent(comparison_html)
            }

            if (self$options$show_exclusion_summary) {
                exclusion_html <- private$.generate_exclusion_summary(outlier_results, analysis_data, original_n)
                self$results$exclusion_summary$setContent(exclusion_html)
            }
            
            if (self$options$show_interpretation) {
                interpretation_html <- private$.generate_interpretation_guide()
                self$results$interpretation$setContent(interpretation_html)
            }

            # Store plot data for visualization
            if (self$options$show_visualization) {
                plotData <- list(
                    outlier_results = outlier_results,
                    analysis_data = analysis_data
                )
                self$results$plot$setState(plotData)
            }
            
            # Analysis-complete summary message: count outliers with the SAME composite
            # rule used by the tables/plot (proportion of per-method flags >= threshold),
            # so the message agrees with the displayed counts when threshold != 0.5.
            if (!is.null(outlier_results)) {
                n_analyzed <- nrow(analysis_data)
                proportion_outlier <- private$.compute_outlier_proportion(outlier_results)
                n_outliers <- sum(proportion_outlier >= self$options$composite_threshold, na.rm = TRUE)
                outlier_pct <- round(n_outliers / n_analyzed * 100, 1)

                private$.accumulateMessage(sprintf(
                    '<strong>Analysis Complete:</strong> %d observations analyzed, %d outliers detected (%.1f%%) using %s method.',
                    n_analyzed, n_outliers, outlier_pct, private$.get_method_description()
                ))
            }

            # Render accumulated warning blocks + list messages to the always-visible panel.
            private$.renderWarnings()

        },

        .plot = function(image, ggtheme, theme, ...) {
            # Simple, reliable plot function

            plotData <- image$state

            if (is.null(plotData) || is.null(plotData$outlier_results) || is.null(plotData$analysis_data)) {
                return(FALSE)
            }

            outlier_results <- plotData$outlier_results
            analysis_data <- plotData$analysis_data

            # CRITICAL FIX: Extract detailed data from result list
            if (is.list(outlier_results) && "outlier_data" %in% names(outlier_results)) {
                # Composite outlier score via shared helper (per-method proportion)
                outlier_score <- private$.compute_outlier_proportion(outlier_results)

                # Create plot data with composite scores
                plot_data <- data.frame(
                    row_id = seq_along(outlier_score),
                    outlier_score = outlier_score,
                    is_outlier = outlier_score >= self$options$composite_threshold
                )

                # Create scatter plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = row_id, y = outlier_score, color = is_outlier)) +
                    ggplot2::geom_point(size = 2, alpha = 0.7) +
                    ggplot2::geom_hline(yintercept = self$options$composite_threshold, linetype = "dashed", color = "red") +
                    ggplot2::scale_color_manual(
                        name = "Status",
                        values = c("FALSE" = "#3498db", "TRUE" = "#e74c3c"),
                        labels = c("FALSE" = "Normal", "TRUE" = "Outlier")
                    ) +
                    ggplot2::labs(
                        title = "Outlier Detection Results",
                        x = "Observation Index",
                        y = "Outlier Score"
                    ) +
                    ggplot2::theme_minimal()

            } else {
                # Binary results
                plot_data <- data.frame(
                    row_id = seq_len(nrow(analysis_data)),
                    is_outlier = as.logical(outlier_results)
                )

                # Create binary plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = row_id, y = as.numeric(is_outlier), color = is_outlier)) +
                    ggplot2::geom_jitter(size = 2, alpha = 0.7, width = 0.2, height = 0.05) +
                    ggplot2::scale_color_manual(
                        name = "Status",
                        values = c("FALSE" = "#3498db", "TRUE" = "#e74c3c"),
                        labels = c("FALSE" = "Normal", "TRUE" = "Outlier")
                    ) +
                    ggplot2::scale_y_continuous(
                        breaks = c(0, 1),
                        labels = c("Normal", "Outlier"),
                        limits = c(-0.1, 1.1)
                    ) +
                    ggplot2::labs(
                        title = "Outlier Detection Results",
                        x = "Observation Index",
                        y = "Classification"
                    ) +
                    ggplot2::theme_minimal()
            }

            # Apply jamovi theme
            p <- p + ggtheme

            print(p)
            return(TRUE)
        },

        .perform_outlier_detection = function(data) {
            
            method_category <- self$options$method_category
            
            # Check data validity before proceeding
            if (is.null(data) || nrow(data) == 0) {
                jmvcore::reject("No data available for outlier detection")
            }

            if (is.null(data) || ncol(data) == 0) {
                jmvcore::reject("No variables available for outlier detection")
            }
            
            # Check for completely missing variables
            all_na_vars <- sapply(data, function(x) all(is.na(x)))
            if (any(all_na_vars)) {
                # `code = NULL` is required: reject()'s second POSITIONAL argument is
                # `code`, so passing the value positionally left "{}" unsubstituted
                # and the user saw a literal "{}" instead of the variable names.
                jmvcore::reject("Variables with all missing values: {}", code = NULL,
                                paste(names(data)[all_na_vars], collapse = ", "))
            }
            
            # Check for constant variables  
            constant_vars <- sapply(data, function(x) {
                non_na_x <- x[!is.na(x)]
                length(unique(non_na_x)) <= 1
            })
            if (any(constant_vars)) {
                jmvcore::reject("Variables with constant values (no variation): {}", code = NULL,
                                paste(names(data)[constant_vars], collapse = ", "))
            }
            
            # Set up method and threshold based on category
            if (method_category == "univariate") {
                method <- self$options$univariate_methods
                threshold <- private$.get_univariate_threshold(method)

                # HDI is estimated by bayestestR, which cannot place the interval
                # bounds when the requested mass leaves fewer than one observation
                # in each tail. It does not fail loudly: it returns NA for every
                # observation, which used to flow through rowMeans(na.rm = TRUE) as
                # NaN and be reported to the user as a clean "0 outliers" result in
                # a green panel. Verified with performance 0.17.1: at the module
                # default confidence level of 0.999, every n below 1000 came back
                # entirely unclassified. Refuse the run instead of reporting a
                # method failure as a data-quality finding.
                if (method == "hdi") {
                    ci <- self$options$confidence_level
                    n_needed <- ceiling(1 / (1 - ci))
                    if (nrow(data) < n_needed) {
                        max_ci <- floor((1 - 1 / nrow(data)) * 1000) / 1000
                        jmvcore::reject(
                            .fmt(
                                .("The highest density interval cannot be estimated at a confidence level of {ci} from {n} complete cases: that level needs at least {needed} observations to place the interval bounds, so every observation would come back unclassified and the analysis would report zero outliers whether or not extreme values are present. Lower the confidence level to at most {maxci} for this sample size, or choose the Equal-tailed interval or Robust Z-score method, which work at this sample size."),
                                ci = base::format(ci),
                                n = nrow(data),
                                needed = n_needed,
                                maxci = base::format(max_ci)),
                            code = "hdi_ci_too_wide")
                    }
                }

                # Both interval methods place their bounds on the OBSERVED quantiles,
                # so the number of flags they can produce is a property of n and the
                # confidence level, not of how extreme the values are. At the module
                # default ci = 0.999 the equal-tailed bound sits between the two most
                # extreme order statistics until n is in the thousands. Verified with
                # performance 0.17.1 on 1 variable with 5 planted values at 20-40 SD:
                # n = 1000 -> ETI flagged 2 of 5 and HDI 0 of 5; n = 2000 -> ETI 2,
                # HDI 1; only at n = 5000 (where the cap below reaches 6) did ETI
                # flag all 5. The cap formula was checked against those runs: ETI's
                # total equalled 2 * ceiling((1 - ci) / 2 * (n - 1)) exactly.
                if (method %in% c("eti", "hdi")) {
                    ci <- self$options$confidence_level
                    cap <- 2 * ceiling((1 - ci) / 2 * (nrow(data) - 1))
                    method_label <- if (method == "eti")
                        "equal-tailed interval" else "highest density interval"
                    private$.accumulateMessage(sprintf(
                        paste0('<strong>How many values this method can flag:</strong> the %s ',
                               'places its bounds on the observed quantiles, so at a confidence ',
                               'level of %s with %d observations it can flag at most about %d ',
                               'value(s) per variable, however many extreme values the data ',
                               'contain. On data with no extreme values it will usually flag ',
                               'close to that many anyway, because the bounds fall between the ',
                               'outermost observations. Use Robust Z-score or IQR if you need a ',
                               'count that responds to how far out the values are.'),
                        method_label, base::format(ci), nrow(data), cap))
                }

            } else if (method_category == "multivariate") {
                # Check if multivariate methods are applicable
                if (ncol(data) == 1) {
                    jmvcore::reject("Multivariate methods require multiple variables. Please select more variables or use univariate methods.")
                }
                
                method <- self$options$multivariate_methods
                threshold <- NULL  # Use default thresholds
                
                # Special checks for specific multivariate methods
                if (method %in% c("optics", "lof")) {
                    if (!requireNamespace("dbscan", quietly = TRUE)) {
                        jmvcore::reject(
                            "The {} method needs the dbscan package, which is not available in this session. Choose Mahalanobis distance or MCD instead, or reinstall the module.",
                            code = NULL, method)
                    }
                }
                
                if (method == "mcd") {
                    if (!requireNamespace("robustbase", quietly = TRUE)) {
                        jmvcore::reject("The MCD method needs the robustbase package, which is not available in this session. Choose Mahalanobis distance or a univariate method instead, or reinstall the module.")
                    }
                }

                # Robust Mahalanobis is routed through bigutilsr, which is not
                # installed with this module, so without this guard the user gets
                # a raw R error they cannot act on inside jamovi.
                if (method == "mahalanobis_robust") {
                    if (!requireNamespace("bigutilsr", quietly = TRUE)) {
                        jmvcore::reject("Robust Mahalanobis distance is computed through the bigutilsr package, which is not installed with this module, so no result can be produced for this method. Choose Minimum covariance determinant (MCD) for a robust multivariate distance, or Mahalanobis distance for the classical one.")
                    }
                }

            } else if (method_category == "composite") {
                method <- c("zscore_robust", "iqr", "mahalanobis")
                threshold <- private$.compositeThresholds()
                
                # For composite methods, check if we have enough variables for multivariate component
                if (ncol(data) == 1) {
                    method <- c("zscore_robust", "iqr")  # Remove mahalanobis for single variable
                }
                
            } else if (method_category == "all") {
                # Do NOT pass method = "all": the performance package expands that
                # to a set including mahalanobis_robust and ics, which need the
                # optional packages bigutilsr and ICS/ICSOutlier. Neither ships with
                # this module, so "All methods" - one of four top-level user-visible
                # choices - failed outright on every standard install. Build the set
                # from what is actually loadable instead.
                method <- private$.allMethodSet()$methods
                threshold <- private$.compositeThresholds()

                # Same single-variable restriction the composite branch applies.
                if (ncol(data) == 1)
                    method <- setdiff(method, c("mahalanobis", "mahalanobis_robust",
                                                "mcd", "optics", "lof", "ics"))
            }
            
            # Perform outlier detection
            private$.checkpoint()
            outlier_result <- performance::check_outliers(
                data,
                method = method,
                threshold = threshold
            )
            private$.checkpoint()

            # Check if result is valid
            if (is.null(outlier_result)) {
                jmvcore::reject("Outlier detection returned no results")
            }

            # CRITICAL FIX: Extract detailed data from attributes
            # performance::check_outliers() returns a logical vector with
            # detailed information stored in attr(result, "data")
            outlier_data <- attr(outlier_result, "data")

            # A method can return without erroring and still classify nothing:
            # bayestestR's interval methods return NA for every observation when the
            # requested interval is too wide for the sample. rowMeans(na.rm = TRUE)
            # then yields NaN, every "proportion >= threshold" test yields FALSE, and
            # a total method failure is presented to the user as a green "no outliers
            # found" panel. Catch it here so no consumer downstream has to.
            if (!is.null(outlier_data) && is.data.frame(outlier_data)) {
                flag_cols <- grep("^Outlier_", names(outlier_data), value = TRUE)
                if (length(flag_cols) > 0 &&
                    all(vapply(outlier_data[flag_cols], function(x) all(is.na(x)), logical(1)))) {
                    jmvcore::reject(
                        .fmt(
                            .("{method} returned no classification for any of the {n} observations analysed, so no outlier result can be shown. This happens when the method cannot be estimated at the requested setting rather than because the data are clean - reporting it as zero outliers would be misleading. Lower the confidence level, or select Robust Z-score or the Interquartile range method, and run the analysis again."),
                            method = private$.get_method_description(),
                            n = nrow(data)),
                        code = "no_classification_returned")
                }
            }

            # If detailed data exists, augment the result
            if (!is.null(outlier_data) && is.data.frame(outlier_data)) {
                # Create a comprehensive result object with both the logical vector
                # and the detailed per-method scores/probabilities
                result_list <- list(
                    outlier_logical = as.logical(outlier_result),
                    outlier_data = outlier_data,
                    method = method,
                    threshold = threshold,
                    n_obs = nrow(data)
                )

                return(result_list)
            } else {
                # Fallback for older performance package versions
                result_list <- list(
                    outlier_logical = as.logical(outlier_result),
                    outlier_data = NULL,
                    method = method,
                    threshold = threshold,
                    n_obs = nrow(data)
                )

                return(result_list)
            }
        },

        # The set of methods used for "All methods".
        #
        # performance::check_outliers(method = "all") expands to a fixed list that
        # includes mahalanobis_robust (needs bigutilsr) and ics (needs ICS and
        # ICSOutlier). None of those are declared dependencies of this module, so
        # the literal "all" raised a hard package error on every standard install.
        # Assemble the set from what is actually loadable and report the rest.
        .allMethodSet = function() {
            methods <- c("zscore_robust", "iqr", "ci", "mahalanobis")
            missing <- character(0)

            if (requireNamespace("robustbase", quietly = TRUE))
                methods <- c(methods, "mcd")
            else
                missing <- c(missing, "minimum covariance determinant, which needs robustbase")

            if (requireNamespace("dbscan", quietly = TRUE))
                methods <- c(methods, "optics", "lof")
            else
                missing <- c(missing, "OPTICS clustering and local outlier factor, which need dbscan")

            if (requireNamespace("bigutilsr", quietly = TRUE))
                methods <- c(methods, "mahalanobis_robust")
            else
                missing <- c(missing, "robust Mahalanobis distance, which needs bigutilsr")

            if (requireNamespace("ICS", quietly = TRUE) &&
                requireNamespace("ICSOutlier", quietly = TRUE))
                methods <- c(methods, "ics")
            else
                missing <- c(missing, "invariant coordinate selection, which needs ICS and ICSOutlier")

            list(methods = methods, missing = missing)
        },

        # Per-method thresholds for the composite and "all" categories.
        #
        # These branches used to pass threshold = NULL, so the Z-score threshold,
        # IQR multiplier and confidence level boxes did nothing at all outside the
        # univariate category - and the default category is composite. The inertness
        # was invisible because the module defaults (3.29 / 1.7 / 0.999) are exactly
        # performance's own defaults, so only a user who CHANGED a value was affected,
        # and they got a byte-identical result with no explanation.
        # check_outliers() accepts a named list and falls back to its own default for
        # any method not named, so naming methods that are not in the current set is
        # harmless (verified against performance 0.17.1, no warning emitted, and a
        # list carrying the defaults reproduces threshold = NULL exactly).
        .compositeThresholds = function() {
            list(
                zscore        = self$options$zscore_threshold,
                zscore_robust = self$options$zscore_threshold,
                iqr           = self$options$iqr_multiplier,
                ci            = self$options$confidence_level,
                eti           = self$options$confidence_level
            )
        },

        .get_univariate_threshold = function(method) {
            if (method %in% c("zscore", "zscore_robust")) {
                return(self$options$zscore_threshold)
            } else if (method == "iqr") {
                return(self$options$iqr_multiplier)
            } else if (method %in% c("eti", "hdi")) {
                return(self$options$confidence_level)
            }
            return(NULL)
        },

        # Human-readable statement of the cut-off actually applied, for the
        # explanatory text. Kept in one place so the wording cannot drift from
        # the value that was passed to check_outliers().
        .threshold_in_force = function() {
            mcat <- self$options$method_category
            if (mcat == "univariate") {
                m <- self$options$univariate_methods
                if (m %in% c("zscore", "zscore_robust"))
                    return(paste0("Z = ", base::format(self$options$zscore_threshold)))
                if (m == "iqr")
                    return(paste0("IQR multiplier ", base::format(self$options$iqr_multiplier)))
                return(paste0("confidence level ", base::format(self$options$confidence_level)))
            }
            if (mcat == "multivariate")
                return("the package default cut-off for the selected multivariate method")
            paste0("flagged by at least ",
                   round(self$options$composite_threshold * 100), "% of the methods used")
        },

        # LARGE-SAMPLE rate of flags a normally distributed variable would produce
        # by chance at the cut-off actually in force, as a percentage, across n_vars
        # variables. This is the asymptotic tail probability; the finite-sample rate
        # differs (see the caller, which prints the small-sample caveat below n=500).
        # Returns NULL when there is no closed form - the composite, "all" and
        # multivariate categories combine cut-offs and are not summarised this way.
        #
        # The univariate flag is "extreme on at least one variable", so the
        # per-variable tail probability is combined across variables assuming
        # independence; correlated variables give a somewhat lower rate.
        .expected_flag_rate = function(n_vars) {
            if (self$options$method_category != "univariate")
                return(NULL)
            m <- self$options$univariate_methods
            per_var <- if (m %in% c("zscore", "zscore_robust")) {
                2 * stats::pnorm(-abs(self$options$zscore_threshold))
            } else if (m == "iqr") {
                # Tukey fences on a normal variable: Q1/Q3 sit at -/+0.6745 SD and
                # the IQR is 1.349 SD, so the fence is 0.6745 + 1.349 * k SD out.
                2 * stats::pnorm(-(0.6745 + 1.349 * self$options$iqr_multiplier))
            } else {
                # No number is reported for the interval methods. They place their
                # bounds on the OBSERVED quantiles, so the flag count is a property
                # of the sample size rather than a tail probability: at ci = 0.999
                # with n = 200 the interpolated quantile sits between the two most
                # extreme order statistics, so ETI flags the min and max of every
                # variable whatever the distribution. Measured: 2.97% of rows over
                # 30 clean normal replicates, against the 0.30% a (1 - ci) tail
                # argument would predict - a ten-fold error, so it is not stated.
                return(NULL)
            }
            if (!is.finite(per_var) || is.null(n_vars) || n_vars < 1)
                return(NULL)
            100 * (1 - (1 - per_var)^n_vars)
        },

        # Per-variable attribution for the univariate categories.
        #
        # Returns, for every row, the variable(s) whose own statistic crosses the
        # threshold, with the offending value - or NULL when the category/method has
        # no per-variable statistic to recompute. The reconstructions below were
        # checked against performance 0.17.1 on 200x3 data with planted outliers and
        # reproduce its Outlier_* flags EXACTLY (identical() TRUE) for zscore,
        # zscore_robust, iqr and eti, so the attribution cannot disagree with the
        # flag it explains.
        #
        # Degenerate scales are part of that agreement, not an exception. A heavily
        # tied variable (limit-of-detection reads, a mostly-constant score) can have
        # mad == 0, sd == 0 or IQR == 0. performance does not bail out there: it
        # divides by zero, so every row that differs from the centre gets Inf and is
        # flagged, and rows equal to the centre get NaN -> NA. Returning all-FALSE
        # here instead printed "-" in the Driven by column for every one of those
        # flagged rows - exactly the case where the flags are most numerous and the
        # attribution most needed. Verified with performance 0.17.1 on
        # c(rep(5, 160), rnorm(40, 5, 1)) (mad == 0, IQR span == 0): it flags the
        # same 40 rows that `x != median(x)` and `x < q[1] | x > q[2]` identify.
        .univariate_drivers = function(data) {
            if (self$options$method_category != "univariate")
                return(NULL)
            method <- self$options$univariate_methods
            if (!method %in% c("zscore", "zscore_robust", "iqr", "eti"))
                return(NULL)   # hdi has no cheap closed-form reconstruction
            if (is.null(data) || !is.data.frame(data) || nrow(data) == 0)
                return(NULL)

            flag_one <- function(x) {
                x <- suppressWarnings(as.numeric(x))
                if (all(is.na(x)))
                    return(rep(FALSE, length(x)))
                if (method == "zscore_robust") {
                    centre <- stats::median(x, na.rm = TRUE)
                    scale <- stats::mad(x, na.rm = TRUE)
                    if (!is.finite(scale)) return(rep(FALSE, length(x)))
                    if (scale == 0) return(x != centre)
                    abs((x - centre) / scale) > self$options$zscore_threshold
                } else if (method == "zscore") {
                    centre <- mean(x, na.rm = TRUE)
                    scale <- stats::sd(x, na.rm = TRUE)
                    if (!is.finite(scale)) return(rep(FALSE, length(x)))
                    if (scale == 0) return(x != centre)
                    abs((x - centre) / scale) > self$options$zscore_threshold
                } else if (method == "iqr") {
                    q <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
                    span <- q[2] - q[1]
                    if (!is.finite(span)) return(rep(FALSE, length(x)))
                    # span == 0 needs no special case: the fences collapse onto the
                    # quartiles, which is what performance computes there too.
                    k <- self$options$iqr_multiplier
                    (x < q[1] - k * span) | (x > q[2] + k * span)
                } else {
                    tail_p <- (1 - self$options$confidence_level) / 2
                    q <- stats::quantile(x, c(tail_p, 1 - tail_p), na.rm = TRUE, names = FALSE)
                    (x < q[1]) | (x > q[2])
                }
            }

            labels <- rep("", nrow(data))
            for (nm in names(data)) {
                hit <- flag_one(data[[nm]])
                hit[is.na(hit)] <- FALSE
                if (!any(hit))
                    next
                # NOT escaped here: .format_outlier_table() escapes every non-numeric
                # cell value, so escaping twice would render "&amp;lt;" to the user.
                txt <- paste0(nm, " (",
                              base::format(data[[nm]][hit], trim = TRUE, digits = 4), ")")
                labels[hit] <- ifelse(nzchar(labels[hit]),
                                      paste(labels[hit], txt, sep = ", "), txt)
            }
            labels[!nzchar(labels)] <- "-"
            labels
        },

        .get_method_description = function() {
            method_category <- self$options$method_category
            
            if (method_category == "univariate") {
                method_name <- switch(self$options$univariate_methods,
                    "zscore_robust" = "Robust Z-Score (MAD-based)",
                    "zscore" = "Standard Z-Score",
                    "iqr" = "Interquartile Range (IQR)",
                    "eti" = "Equal-Tailed Interval",
                    "hdi" = "Highest Density Interval",
                    "Univariate Method"
                )
            } else if (method_category == "multivariate") {
                method_name <- switch(self$options$multivariate_methods,
                    "mahalanobis" = "Mahalanobis Distance",
                    "mahalanobis_robust" = "Robust Mahalanobis Distance",
                    "mcd" = "Minimum Covariance Determinant",
                    "optics" = "OPTICS Clustering",
                    "lof" = "Local Outlier Factor",
                    "Multivariate Method"
                )
            } else if (method_category == "composite") {
                method_name = "Composite (Multiple Methods)"
            } else {
                method_name = "All Available Methods"
            }
            
            return(method_name)
        },

        # Shared composite score: proportion of per-method flags per observation.
        # Uses ONLY the per-method '^Outlier_' flag columns so the aggregate
        # 'Outlier' column that performance::check_outliers also returns is not
        # double-counted. Falls back to the logical vector when detailed data
        # is unavailable. Reused by the summary, tables, exclusion and plot so
        # every producer applies the same proportion >= composite_threshold rule.
        .compute_outlier_proportion = function(outlier_results) {
            if (is.list(outlier_results) && "outlier_logical" %in% names(outlier_results)) {
                outlier_logical <- outlier_results$outlier_logical
                outlier_data <- outlier_results$outlier_data
                if (!is.null(outlier_data) && is.data.frame(outlier_data)) {
                    outlier_cols <- grep("^Outlier_", names(outlier_data), value = TRUE)
                    if (length(outlier_cols) > 0) {
                        return(rowMeans(outlier_data[, outlier_cols, drop = FALSE], na.rm = TRUE))
                    }
                }
                return(as.numeric(outlier_logical))
            }
            return(as.numeric(as.logical(outlier_results)))
        },

        .generate_plain_summary = function(outlier_results, data, original_n = NULL) {
            # Generate plain-language summary for clinical users

            n_total <- nrow(data)
            n_vars <- ncol(data)

            # When the dataset was subsampled, this panel described the SUBSAMPLE
            # as "your dataset" ("In your dataset of 5000 observations" for a
            # 12000-row file) and the copy-ready sentence repeated the wrong N.
            # Sampling matters more here than for most statistics: a random subset
            # can only reveal the outliers it happens to contain, so a count from
            # the subsample is a lower bound on the outliers in the data. Verified
            # on 12000 rows with 4 planted gross outliers - only the 1 that was
            # sampled could be found.
            subsampled <- !is.null(original_n) && original_n != n_total
            scope_text <- if (subsampled) {
                sprintf("a random subsample of %d observations drawn from the %d-observation dataset",
                        n_total, original_n)
            } else {
                sprintf("your dataset of %d observations", n_total)
            }
            sampling_caveat <- if (subsampled) {
                sprintf(paste0(
                    " <strong>Because only %d of %d observations were analysed, this count is a lower ",
                    "bound:</strong> outliers among the %d rows that were not sampled cannot be ",
                    "detected. Analyse the full dataset, or fewer variables at a time, if you need a ",
                    "complete list."),
                    n_total, original_n, original_n - n_total)
            } else ""

            # Composite outlier score via shared helper (per-method proportion)
            proportion_outlier <- private$.compute_outlier_proportion(outlier_results)
            threshold <- self$options$composite_threshold
            n_outliers <- sum(proportion_outlier >= threshold, na.rm = TRUE)

            outlier_pct <- round(n_outliers / n_total * 100, 1)

            # Create clinical context interpretation
            clinical_context <- if (n_outliers == 0) {
                "No values exceeded the detection thresholds used here. This does not by itself establish data quality: outliers can mask one another, so these methods may miss them."
            } else if (outlier_pct < 1) {
                "A very small number of unusual values were found. The detection method cannot tell whether these are data entry errors or genuine extreme values; review them individually."
            } else if (outlier_pct < 5) {
                "A modest number of unusual values were identified. Review these cases to determine if they represent data entry errors or genuine clinical variation."
            } else if (outlier_pct < 10) {
                "Several unusual values were detected. This warrants careful review to distinguish between data quality issues and true biological variation."
            } else {
                "A substantial proportion of unusual values were found. Consider reviewing data collection procedures and checking for systematic issues."
            }

            # Create action recommendations
            action_text <- if (n_outliers == 0) {
                "No observations exceeded the detection thresholds, so none are listed below for review. This is not a confirmation that the data contain no errors."
            } else if (outlier_pct < 5) {
                paste0("Review the ", n_outliers, " flagged observation(s) below. ",
                       "In clinical data, outliers may represent: ",
                       "(1) data entry errors, (2) equipment malfunction, ",
                       "(3) genuine extreme cases, or (4) rare disease presentations.")
            } else {
                paste0("Carefully examine the ", n_outliers, " flagged observations. ",
                       "The high outlier rate suggests possible systematic issues. ",
                       "Consider: (1) reviewing data collection protocols, ",
                       "(2) checking measurement equipment calibration, ",
                       "(3) verifying data entry procedures.")
            }

            # Method description in plain language with specific details
            method_desc <- switch(self$options$method_category,
                "univariate" = {
                    specific_method <- switch(self$options$univariate_methods,
                        "zscore" = "Z-score analysis",
                        "zscore_robust" = "Robust Z-score analysis",
                        "iqr" = "Interquartile Range (IQR) method",
                        "eti" = "Equal-Tailed Interval method",
                        "hdi" = "Highest Density Interval method",
                        "univariate analysis"
                    )
                    paste0(specific_method, " (analyzing each variable independently)")
                },
                "multivariate" = {
                    specific_method <- switch(self$options$multivariate_methods,
                        "mahalanobis" = "Mahalanobis distance",
                        "mahalanobis_robust" = "Robust Mahalanobis distance",
                        "mcd" = "Minimum Covariance Determinant (MCD)",
                        "optics" = "OPTICS clustering method",
                        "lof" = "Local Outlier Factor (LOF)",
                        "multivariate analysis"
                    )
                    paste0(specific_method, " (considering relationships between variables)")
                },
                "composite" = "multiple detection methods combined for robust identification (Z-score, IQR, and Mahalanobis distance)",
                "all" = "comprehensive analysis using all available detection methods",
                "standard statistical methods"
            )

            # Create the summary HTML
            summary_html <- private$.createHTMLSection(
                "Plain Language Summary",
                paste0(
                    "<p><strong>What we found:</strong> ",
                    "In ", scope_text, " across ", n_vars, " variable(s), ",
                    "we identified ", n_outliers, " potential outlier(s) (", outlier_pct, "%) using ",
                    method_desc, ".", sampling_caveat, "</p>",
                    "<p><strong>Clinical interpretation:</strong> ", clinical_context, "</p>",
                    "<p><strong>", if (n_outliers == 0) "What this means" else "Recommended action", ":</strong> ", action_text, "</p>"
                ),
                # Zero detections is not a "success": several of the methods
                # offered here can return zero on contaminated data (classical
                # Mahalanobis masks, the interval methods are capped by n), and the
                # text below says so. Render it neutral rather than green.
                style = if(n_outliers == 0) "neutral" else if(outlier_pct < 5) "info" else "warning",
                icon = ""
            )

            # Add copy-ready report sentence
            report_sentence <- sprintf(
                "Outlier detection analysis was performed on %s across %d variable(s) using %s. A total of %d outlier(s) were identified (%.1f%% of the observations analysed). %s%s",
                if (subsampled)
                    sprintf("a random subsample of %d observations from a dataset of %d",
                            n_total, original_n)
                else sprintf("%d observations", n_total),
                n_vars, method_desc,
                n_outliers, outlier_pct,
                if (subsampled)
                    "Because a subsample was analysed, this count is a lower bound on the outliers present in the full dataset. "
                else "",
                if(n_outliers == 0) "No observations exceeded the detection thresholds applied."
                else if(outlier_pct < 5) "A small proportion of observations exceeded the detection thresholds."
                else "Further data review is recommended."
            )

            report_html <- private$.createHTMLSection(
                "Report Sentence",
                paste0(
                    "<div style='background-color: rgba(128, 128, 128, 0.09); color: inherit; padding: 10px; border: 1px solid rgba(128, 128, 128, 0.35); ",
                    "border-radius: 4px; font-family: monospace; font-size: 12px;'>",
                    report_sentence,
                    "</div>",
                    "<p style='margin-top: 10px; font-style: italic; opacity: 0.8; font-size: 11px;'>",
                    "Copy the text above for use in clinical reports or documentation.</p>"
                ),
                style = "neutral",
                icon = ""
            )

            return(paste0(summary_html, report_html))
        },

        .generate_outlier_table = function(outlier_results, data, original_n = NULL) {

            # CRITICAL FIX: Extract detailed data from result list
            if (is.list(outlier_results) && "outlier_data" %in% names(outlier_results)) {
                outlier_logical <- outlier_results$outlier_logical
                outlier_data <- outlier_results$outlier_data

                # If we have detailed per-method data, use it
                if (!is.null(outlier_data) && is.data.frame(outlier_data)) {
                    outlier_df <- outlier_data

                    # CRITICAL FIX: Calculate composite score as percentage of methods
                    # that flagged each observation (not just a binary yes/no)
                    if (ncol(outlier_df) > 0) {
                        # Count how many methods flagged each observation using only
                        # the per-method "Outlier_zscore_robust", "Outlier_iqr", ...
                        # flags (excludes the aggregate "Outlier" column to avoid double-counting)
                        outlier_cols <- grep("^Outlier_", names(outlier_df), value = TRUE)

                        if (length(outlier_cols) > 0) {
                            # Calculate proportion of methods flagging each case
                            outlier_df$Proportion_Outlier <- rowMeans(outlier_df[, outlier_cols, drop = FALSE], na.rm = TRUE)
                        } else {
                            # Fallback to logical vector
                            outlier_df$Proportion_Outlier <- as.numeric(outlier_logical)
                        }
                    } else {
                        outlier_df$Proportion_Outlier <- as.numeric(outlier_logical)
                    }
                } else {
                    # Fallback: use logical vector
                    outlier_df <- data.frame(
                        Outlier = outlier_logical,
                        Proportion_Outlier = as.numeric(outlier_logical)
                    )
                }
            } else {
                # Legacy fallback
                outlier_df <- data.frame(
                    Outlier = as.logical(outlier_results),
                    Proportion_Outlier = as.numeric(as.logical(outlier_results))
                )
            }

            # Name the variable(s) responsible, for the univariate categories.
            #
            # performance::check_outliers() aggregates a univariate run over the whole
            # data frame: it returns ONE Distance_* column (the row-wise maximum of the
            # per-variable statistic) and ONE flag. With eight lab variables selected the
            # user was told "row 68 is an outlier" with no way to know which of the eight
            # values to look up - and the panel immediately above tells them to verify
            # each flagged value against source records. Recompute the per-variable
            # statistic and say which variable crossed the line, and at what value.
            drivers <- private$.univariate_drivers(data)
            if (!is.null(drivers) && length(drivers) == nrow(outlier_df))
                outlier_df[["Driven by"]] <- drivers

            # Add row indices. Prefer the original observation identifiers (rownames of
            # the analyzed data) so that, when a large dataset was subsampled, flagged
            # rows still map back to the original observations rather than 1..n.
            data_rownames <- rownames(data)
            if (!is.null(data_rownames) && length(data_rownames) == nrow(outlier_df)) {
                outlier_df$Row <- data_rownames
            } else {
                outlier_df$Row <- seq_len(nrow(outlier_df))
            }

            # CRITICAL FIX: Apply composite threshold to proportion, not binary flag
            threshold <- self$options$composite_threshold
            n_outliers <- sum(outlier_df$Proportion_Outlier >= threshold, na.rm = TRUE)

            # CRITICAL FIX: Use original dataset size if provided
            total_n <- if (!is.null(original_n)) original_n else nrow(data)
            outlier_rate <- round(n_outliers / nrow(outlier_df) * 100, 2)

            # Add sampling notice if original_n differs from current data
            sampling_notice <- ""
            if (!is.null(original_n) && original_n != nrow(outlier_df)) {
                sampling_notice <- paste0(
                    "<p style='color: inherit; background-color: rgba(255, 202, 33, 0.23); padding: 10px; border-radius: 4px;'>",
                    "<strong> Sampling Applied:</strong> Analysis performed on ", nrow(outlier_df),
                    " randomly sampled observations from the original ", original_n, " observations. ",
                    "Outlier counts and rates shown below refer to the sampled subset.</p>"
                )
            }

            table_html <- paste0(
                "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin-bottom: 20px; color: inherit;'>",
                "<h3 style='margin-top: 0;'> Outlier Detection Results</h3>",
                "<p><strong>Method:</strong> ", private$.get_method_description(), "</p>",
                "<p><strong>Complete Cases:</strong> ", total_n,
                if (!is.null(original_n) && original_n != nrow(outlier_df))
                    paste0(" (analyzed ", nrow(outlier_df), " sampled)") else "", "</p>",
                "<p><strong>Outliers Detected:</strong> ", n_outliers, " (", outlier_rate,
                "% of ", if (!is.null(original_n) && original_n != nrow(outlier_df)) "sampled " else "", "observations)</p>",
                sampling_notice,
                "</div>"
            )

            # Per-observation listing.
            #
            # This used to be wrapped in `if (nrow(outlier_df) <= 100)`, so the
            # table vanished entirely for any dataset with more than 100 rows -
            # that is, essentially every clinical dataset - even though
            # `show_outlier_table` defaults to TRUE, its description promises
            # "classification for each observation", and the plain-language
            # summary directly above tells the user to "review the N flagged
            # observation(s) below". There was nothing below.
            # .format_outlier_table() already caps the display at 100 rows, so
            # the outer guard only suppressed output that was safe to render.
            #
            # Flagged rows are listed first: they are what the user has to check
            # against source records, and a listing of the first 100 rows in file
            # order is of no use for that.
            flagged <- which(!is.na(outlier_df$Proportion_Outlier) &
                             outlier_df$Proportion_Outlier >= threshold)
            n_flagged <- length(flagged)

            if (n_flagged > 0) {
                ordered <- flagged[order(outlier_df$Proportion_Outlier[flagged], decreasing = TRUE)]
                shown <- head(ordered, 100)
                heading <- if (n_flagged > length(shown)) {
                    sprintf("Flagged Observations (showing %d of %d, strongest first)",
                            length(shown), n_flagged)
                } else {
                    sprintf("Flagged Observations (all %d)", n_flagged)
                }
                table_html <- paste0(table_html,
                    "<div style='background-color: rgba(255, 255, 255, 0.06); padding: 15px; border-radius: 8px; margin-top: 20px; color: inherit;'>",
                    "<h4>", heading, "</h4>",
                    "<p style='font-size: 12px; opacity: 0.8; margin-top: 0;'>",
                    "Row numbers refer to the original dataset",
                    if (!is.null(original_n) && original_n != nrow(outlier_df))
                        ", including when a subsample was analysed" else "",
                    ". Verify each flagged value against source records before excluding it.</p>",
                    private$.format_outlier_table(outlier_df[shown, , drop = FALSE]),
                    "</div>"
                )
            } else {
                table_html <- paste0(table_html,
                    "<div style='background-color: rgba(255, 255, 255, 0.06); padding: 15px; border-radius: 8px; margin-top: 20px; color: inherit;'>",
                    "<p style='margin: 0;'>No observation reached the outlier threshold, ",
                    "so there is nothing to list here.</p></div>"
                )
            }

            return(table_html)
        },

        .format_outlier_table = function(outlier_df) {
            
            # Limit to first 100 rows for display
            display_df <- head(outlier_df, 100)
            
            table_html <- "<table style='width: 100%; border-collapse: collapse; font-family: Arial, sans-serif;'>"
            table_html <- paste0(table_html,
                "<thead><tr style='background-color: #6c757d; color: #ffffff;'>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>Row</th>"
            )
            
            # Add column headers
            for (col in names(display_df)[names(display_df) != "Row"]) {
                table_html <- paste0(table_html,
                    "<th style='padding: 8px; border: 1px solid #dee2e6;'>", htmltools::htmlEscape(col), "</th>"
                )
            }
            table_html <- paste0(table_html, "</tr></thead><tbody>")
            
            # Add data rows.
            # Backgrounds are translucent tints, not opaque hex: this is the listing
            # the user is told to check against source records, and an opaque light
            # fill with no foreground colour rendered it white-on-white in jamovi's
            # dark theme. The rgba values composite to the previous pastels over a
            # white page while staying legible over a dark one.
            # No per-row outlier tint: the only caller passes rows that already
            # cleared the composite threshold, so every row would carry it and it
            # would distinguish nothing. (It also never fired: the test was
            # isTRUE(display_df$Outlier[i]) and performance returns `Outlier` as
            # rowMeans() of the per-method flags, i.e. numeric 0/1, for which
            # isTRUE() is FALSE.)
            for (i in seq_len(nrow(display_df))) {
                row_bg <- if (i %% 2 == 0) "transparent" else "rgba(128, 128, 128, 0.07)"

                table_html <- paste0(table_html,
                    "<tr style='background-color: ", row_bg, "; color: inherit;'>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6;'>", htmltools::htmlEscape(as.character(display_df$Row[i])), "</td>"
                )
                
                for (col in names(display_df)[names(display_df) != "Row"]) {
                    value <- display_df[[col]][i]
                    if (is.logical(value)) {
                        value <- if (isTRUE(value)) "Yes" else "No"
                    } else if (is.numeric(value)) {
                        value <- round(value, 4)
                    } else {
                        # Cell values can now carry variable names (the "Driven by"
                        # column), which are user-supplied column headers. Anything
                        # non-numeric goes through the same escaping the headers use.
                        value <- htmltools::htmlEscape(as.character(value))
                    }
                    
                    table_html <- paste0(table_html,
                        "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", value, "</td>"
                    )
                }
                table_html <- paste0(table_html, "</tr>")
            }
            
            table_html <- paste0(table_html, "</tbody></table>")
            return(table_html)
        },

        .generate_method_comparison = function(outlier_results) {
            
            # Extract detailed method data
            detailed_data <- outlier_results$outlier_data
            
            comparison_html <- paste0(
                "<div style='background-color: rgba(33, 159, 33, 0.1); padding: 20px; border-radius: 8px; color: inherit;'>",
                "<h3 style='margin-top: 0;'> Method Comparison & Composite Breakdown</h3>"
            )

            if (!is.null(detailed_data) && is.data.frame(detailed_data)) {
                # Look for columns starting with "Z_Score", "Mahalanobis", "IQR", etc. 
                # or typically they are just the method names in performance package depending on version.
                # Actually performance::check_outliers data usually has columns like 'Z_Score_Robust', 'IQR', 'Mahalanobis', etc.
                
                # Filter for numeric/logical columns that strictly look like method flags or scores
                # Note: They are often probabilities or standardized scores. 
                # Let's try to summarize how many outliers each method found.
                
                # Select ONLY per-method flag columns (0/1). Using the raw Distance_*/
                # z-score columns and thresholding them at composite_threshold inflates
                # the per-method outlier counts, so restrict to the "Outlier_*" flags.
                valid_cols <- grep("^Outlier_", names(detailed_data), value = TRUE)
                
                if (length(valid_cols) > 0) {
                     # Create a summary table of agreement
                     comparison_html <- paste0(comparison_html,
                        "<p>The table below shows how many observations were flagged by each individual method included in the composite score.</p>",
                        "<table style='width: 100%; border-collapse: collapse; margin-top: 15px;'>",
                        "<tr style='background-color: #4caf50; color: #ffffff;'>",
                        "<th style='padding: 10px; border: 1px solid #ddd;'>Method</th>",
                        "<th style='padding: 10px; border: 1px solid #ddd;'>Outliers Detected</th>",
                        "<th style='padding: 10px; border: 1px solid #ddd;'>% of Observations Flagged</th>",
                        "</tr>"
                     )
                     
                     n_total <- nrow(detailed_data)
                     
                     for (col in valid_cols) {
                         # Assuming these are probabilities or 0/1 scores. 
                         # Usually standard in performance is probability or distance converted.
                         # Roughly, > threshold or some cut-off. 
                         # For logical columns acts as 0/1. For numeric, it's often a probability.
                         
                         vals <- detailed_data[[col]]
                         if (is.logical(vals)) {
                             n_flagged <- sum(vals, na.rm=TRUE)
                         } else {
                             # Assume numeric score > threshold is flag, but threshold varies.
                             # Actually check_outliers standardizes to roughly [0,1] or flags.
                             # If we can't be sure, we count non-zero? 
                             # Let's check if it's binary-like.
                             if (all(vals %in% c(0, 1, NA))) {
                                  n_flagged <- sum(vals == 1, na.rm=TRUE)
                             } else {
                                  # Continuous score - harder to count "flags" without specific threshold
                                  # Just report "Score Range" maybe?
                                  # Or use the global composite threshold as proxy?
                                  n_flagged <- sum(vals >= self$options$composite_threshold, na.rm=TRUE) 
                             }
                         }
                         
                         pct <- round(n_flagged / n_total * 100, 1)
                         
                         comparison_html <- paste0(comparison_html,
                             "<tr>",
                             "<td style='padding: 8px; border: 1px solid #ddd;'><strong>", htmltools::htmlEscape(col), "</strong></td>",
                             "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", n_flagged, "</td>",
                             "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", pct, "%</td>",
                             "</tr>"
                         )
                     }
                     comparison_html <- paste0(comparison_html, "</table>")

                     # The column above is a per-method DETECTION rate, not agreement:
                     # nothing in it compares one method with another. Report the
                     # actual concordance separately so the two are not confused.
                     flags <- as.matrix(detailed_data[, valid_cols, drop = FALSE])
                     mode(flags) <- "numeric"
                     n_meth <- length(valid_cols)
                     hits <- rowSums(flags, na.rm = TRUE)
                     n_none <- sum(hits == 0)
                     n_all <- sum(hits == n_meth)
                     n_some <- n_total - n_none - n_all
                     comparison_html <- paste0(comparison_html,
                        "<h4>Agreement Between Methods</h4>",
                        "<p>Of the ", n_total, " observations analysed, ", n_all,
                        " were flagged by all ", n_meth, " methods, ", n_some,
                        " by some but not all of them, and ", n_none, " by none. ",
                        "Observations in the middle group are the ones the methods disagree about; ",
                        "which of them count as outliers here is decided entirely by the composite ",
                        "threshold below.</p>"
                     )
                }
            }
            
            comparison_html <- paste0(comparison_html,
                "<p style='margin-top: 15px;'><strong>Composite Logic:</strong> An observation is classified as a composite outlier if it is flagged by at least ", 
                round(self$options$composite_threshold * 100), "% of the methods used.</p>",
                "</div>"
            )
            
            return(comparison_html)
        },

        .generate_exclusion_summary = function(outlier_results, data, original_n = NULL) {

            # Composite outlier score via shared helper (per-method proportion)
            proportion_outlier <- private$.compute_outlier_proportion(outlier_results)
            threshold <- self$options$composite_threshold
            n_outliers <- sum(proportion_outlier >= threshold, na.rm = TRUE)

            # CRITICAL FIX: Use original dataset size if provided
            n_total <- if (!is.null(original_n)) original_n else nrow(data)
            n_analyzed <- nrow(data)  # May be sampled
            n_remaining <- n_analyzed - n_outliers
            exclusion_rate <- round(n_outliers / n_analyzed * 100, 2)
            retention_rate <- round(n_remaining / n_analyzed * 100, 2)
            
            # Add sampling notice if applicable
            sampling_note <- ""
            if (!is.null(original_n) && original_n != n_analyzed) {
                sampling_note <- paste0(
                    "<p style='color: inherit; background-color: rgba(255, 202, 33, 0.23); padding: 10px; border-radius: 4px; margin-bottom: 10px;'>",
                    "<strong> Note:</strong> Statistics below refer to the ", n_analyzed,
                    " sampled observations from the original ", original_n, " observations.</p>"
                )
            }

            exclusion_html <- paste0(
                "<div style='background-color: rgba(255, 169, 33, 0.14); padding: 20px; border-radius: 8px; color: inherit;'>",
                "<h3 style='margin-top: 0;'> Exclusion Recommendations</h3>",
                sampling_note,
                "<table style='width: 100%; border-collapse: collapse;'>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Complete Cases:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_total,
                if (!is.null(original_n) && original_n != n_analyzed) paste0(" (analyzed ", n_analyzed, " sampled)") else "", "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Outliers Detected:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_outliers, " (", exclusion_rate,
                "% of ", if (!is.null(original_n) && original_n != n_analyzed) "sampled " else "", "observations)</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Observations Retained:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_remaining, " (", retention_rate,
                "% of ", if (!is.null(original_n) && original_n != n_analyzed) "sampled " else "", "observations)</td></tr>",
                "</table>",
                "<h4>Recommendations:</h4>",
                "<ul>"
            )
            
            # The old text here graded the observed rate against fixed 1 / 5 / 10 %
            # cut-offs and called anything under 5 % "acceptable for most analyses".
            # That is not a property of the data: at the module default of a robust
            # Z of 3.29, a well-behaved distribution produces about 0.1 % flags, so a
            # 4.9 % rate is a fifty-fold excess - the opposite of the verdict shown.
            # Conversely a permissive setting (Z = 2, or composite threshold 0.1)
            # produces double-digit rates on clean data. The rate is only readable
            # against the threshold in force, so state that comparison and let the
            # reader draw the conclusion.
            # The figure below is the LARGE-SAMPLE tail probability implied by the
            # cut-off (see .expected_flag_rate()), not the finite-sample rate. The
            # two differ materially in small samples, because the centre and scale
            # the cut-off is applied to are themselves estimated from the same rows.
            # Measured on clean N(0,1) data, 3 variables, 400 replicates: the robust
            # Z-score at 3.29 flagged 1.02% of rows at n = 50, 0.59% at n = 100 and
            # 0.47% at n = 200 against a large-sample value of 0.30%, converging by
            # n = 1000 (0.32%); the IQR rule at 1.7 flagged 2.20% at n = 50 and
            # 1.22% at n = 200 against 0.90%. Standard Z errs the other way (0.18%
            # at n = 50) because an extreme value inflates its own SD. So the number
            # is labelled as the large-sample value and the small-sample caveat is
            # printed below 500 rows rather than letting the reader treat a modest
            # excess as a finding.
            expected_pct <- private$.expected_flag_rate(ncol(data))
            if (!is.null(expected_pct)) {
                exclusion_html <- paste0(exclusion_html,
                    "<li><strong>Observed vs expected:</strong> ", exclusion_rate,
                    "% of the observations analysed were flagged. At the threshold in force ",
                    "(", private$.threshold_in_force(), "), a large sample from a normal ",
                    "distribution would produce about ", base::format(round(expected_pct, 2), nsmall = 2),
                    "% flags across ", ncol(data), " variable(s) by chance alone. The comparison ",
                    "between those two numbers, not the observed rate on its own, is what indicates ",
                    "whether the data carry more extreme values than the threshold anticipates.</li>",
                    if (n_analyzed < 500)
                        paste0("<li><strong>Small sample:</strong> that large-sample figure is a ",
                               "poor guide at the ", n_analyzed, " observations analysed here. The ",
                               "centre and spread the cut-off is applied to are estimated from the ",
                               "same rows, so on clean normal data the robust Z-score and IQR rules ",
                               "flag a higher percentage at this sample size than the large-sample ",
                               "value states, and the standard Z-score flags a lower one. Treat a ",
                               "modest difference between the two numbers as uninformative here.</li>")
                    else "",
                    "<li>An excess over the expected rate is consistent with heavier tails than a ",
                    "normal distribution, a mixture of distinct subgroups, or data errors. These ",
                    "methods cannot distinguish between those explanations.</li>"
                )
            } else {
                exclusion_html <- paste0(exclusion_html,
                    "<li><strong>Observed rate:</strong> ", exclusion_rate,
                    "% of the observations analysed were flagged by the ",
                    private$.get_method_description(), " run. No chance-expected rate is ",
                    "reported for it: the interval methods set their bounds from the observed ",
                    "quantiles, and the composite, all-methods and multivariate categories ",
                    "combine cut-offs that have no single closed form.</li>",
                    if (self$options$method_category %in% c("composite", "all"))
                        paste0("<li>The <em>Method comparison</em> output reports what each ",
                               "individual method flagged, which is the comparison available here.</li>")
                    else "",
                    "<li>Whether this rate is high depends on the threshold in force ",
                    "(", private$.threshold_in_force(), "); a more permissive setting raises it on ",
                    "any dataset, including a clean one.</li>"
                )
            }

            exclusion_html <- paste0(exclusion_html,
                "<li>Excluding a flagged observation is a judgement about that observation, not a ",
                "statistical conclusion: these methods identify values that are far from the rest, ",
                "not values that are wrong.</li>",
                "<li>Document outlier handling procedures for transparency</li>",
                "<li>Report analyses both with and without outliers when feasible</li>",
                "</ul></div>"
            )
            
            return(exclusion_html)
        },

        .generate_interpretation_guide = function() {
            
            method_category <- self$options$method_category
            
            interpretation_html <- paste0(
                "<div style='background-color: rgba(33, 152, 239, 0.13); padding: 20px; border-radius: 8px; color: inherit;'>",
                "<h3 style='margin-top: 0;'> Analysis Interpretation Guide</h3>",
                
                "<h4>Current Method: ", private$.get_method_description(), "</h4>"
            )
            
            if (method_category == "univariate") {
                interpretation_html <- paste0(interpretation_html,
                    "<p><strong>Univariate methods</strong> analyze each variable separately and detect observations that are extreme on at least one variable.</p>",
                    "<p><strong>Advantages:</strong> Simple to interpret, computationally efficient</p>",
                    "<p><strong>Limitations:</strong> May miss multivariate outliers, liberal with high-dimensional data</p>"
                )
            } else if (method_category == "multivariate") {
                interpretation_html <- paste0(interpretation_html,
                    "<p><strong>Multivariate methods</strong> consider relationships between variables and detect observations that are unusual in the multivariate space.</p>",
                    "<p><strong>Advantages:</strong> Detect complex outlier patterns, account for variable correlations</p>",
                    "<p><strong>Limitations:</strong> More complex to interpret, computationally intensive</p>"
                )
            } else {
                interpretation_html <- paste0(interpretation_html,
                    "<p><strong>Composite methods</strong> combine multiple algorithms to provide robust outlier detection with reduced false positive rates.</p>",
                    "<p><strong>Advantages:</strong> Robust across different data patterns, comprehensive coverage</p>",
                    "<p><strong>Threshold:</strong> ", self$options$composite_threshold, " means outliers detected by \u{2265} ", round(self$options$composite_threshold * 100), "% of methods</p>"
                )
            }
            
            interpretation_html <- paste0(interpretation_html,
                "<h4>Key Considerations:</h4>",
                "<ul>",
                "<li><strong>Clinical Context:</strong> Consider whether outliers represent data errors or genuine biological variation</li>",
                "<li><strong>Sample Size:</strong> Outlier impact is greater in smaller samples</li>",
                "<li><strong>Study Design:</strong> Repeated measures may require different outlier handling</li>",
                "<li><strong>Analysis Goals:</strong> Descriptive vs. inferential statistics may warrant different approaches</li>",
                "</ul>",
                
                "<h4>Reporting Guidelines:</h4>",
                "<ul>",
                "<li>Document outlier detection method and thresholds used</li>",
                "<li>Report number and percentage of outliers detected</li>",
                "<li>Justify outlier handling decisions with theoretical rationale</li>",
                "<li>Consider sensitivity analyses with/without outliers</li>",
                "</ul>",
                
                "<p style='font-size: 12px; opacity: 0.85; margin-top: 15px;'>",
                "<em> Citation: L\u{FC}decke et al. (2021). performance: An R package for assessment, comparison and testing of statistical models. Journal of Open Source Software, 6(60), 3139.</em>",
                "</p></div>"
            )
            
            return(interpretation_html)
        },

        .validateInputs = function(dataset, selected_vars) {
            validation_results <- list(
                errors = character(0),
                warnings = character(0),
                info = character(0),
                should_stop = FALSE
            )
            
            # Check dataset validity
            if (is.null(dataset) || !is.data.frame(dataset)) {
                validation_results$errors <- c(validation_results$errors, "Dataset is not a valid data frame")
                validation_results$should_stop <- TRUE
                return(validation_results)
            }
            
            if (nrow(dataset) == 0) {
                validation_results$errors <- c(validation_results$errors, "Dataset contains no rows")
                validation_results$should_stop <- TRUE
                return(validation_results)
            }
            
            # Check variable selection
            if (length(selected_vars) == 0) {
                validation_results$errors <- c(validation_results$errors, "No variables selected for analysis")
                validation_results$should_stop <- TRUE
                return(validation_results)
            }
            
            # Check if selected variables exist in dataset
            missing_vars <- setdiff(selected_vars, names(dataset))
            if (length(missing_vars) > 0) {
                validation_results$errors <- c(validation_results$errors, 
                    paste("Variables not found in dataset:", paste(missing_vars, collapse = ", ")))
                validation_results$should_stop <- TRUE
                return(validation_results)
            }
            
            # Check sample size
            if (nrow(dataset) < 30) {
                validation_results$warnings <- c(validation_results$warnings,
                    paste("Small sample size (n =", nrow(dataset), "). Results may be unreliable. Recommend n \u{2265} 30."))
            }
            
            # Check number of variables
            if (length(selected_vars) == 1) {
                validation_results$info <- c(validation_results$info,
                    "Single variable selected. Multivariate methods will not be applicable.")
            } else if (length(selected_vars) > 10) {
                validation_results$warnings <- c(validation_results$warnings,
                    paste("Many variables selected (", length(selected_vars), "). Consider reducing dimensionality for better performance."))
            }
            
            # Check variable types and data quality
            for (var in selected_vars) {
                var_data <- dataset[[var]]
                
                # Check if variable exists
                if (is.null(var_data)) {
                    validation_results$errors <- c(validation_results$errors, 
                        paste("Variable", var, "is NULL"))
                    validation_results$should_stop <- TRUE
                    next
                }
                
                # Check if variable is numeric or can be converted
                if (!is.numeric(var_data)) {
                    if (is.character(var_data) || is.factor(var_data)) {
                        # Try to convert to numeric
                        numeric_conversion <- suppressWarnings(as.numeric(as.character(var_data)))
                        failed <- !is.na(var_data) & is.na(numeric_conversion)
                        if (any(failed)) {
                            validation_results$errors <- c(validation_results$errors,
                                paste("Variable", var, "contains", sum(failed),
                                      "non-missing value(s) that cannot be converted to numeric"))
                            validation_results$should_stop <- TRUE
                            next
                        } else {
                            validation_results$warnings <- c(validation_results$warnings,
                                paste("Variable", var, "converted from character/factor to numeric"))
                        }
                    } else {
                        validation_results$warnings <- c(validation_results$warnings,
                            paste("Variable", var, "is not numeric. Attempting conversion."))
                    }
                }
                
                # Check missing data
                n_missing <- sum(is.na(var_data))
                if (n_missing > 0) {
                    missing_pct <- round(n_missing / length(var_data) * 100, 1)
                    if (missing_pct > 50) {
                        validation_results$warnings <- c(validation_results$warnings,
                            paste("Variable", var, "has", missing_pct, "% missing data. Consider exclusion."))
                    } else if (missing_pct > 20) {
                        validation_results$warnings <- c(validation_results$warnings,
                            paste("Variable", var, "has", missing_pct, "% missing data. Results may be affected."))
                    } else if (missing_pct > 0) {
                        validation_results$info <- c(validation_results$info,
                            paste("Variable", var, "has", missing_pct, "% missing data (within acceptable range)."))
                    }
                }
                
                # Check for constant values
                # Initialize per-variable so unique_values is always defined even
                # when the variable is entirely NA (avoids an uncaught error at the
                # extreme-value check below for all-NA numeric variables).
                unique_values <- numeric(0)
                numeric_var <- suppressWarnings(as.numeric(as.character(var_data)))
                if (!all(is.na(numeric_var))) {
                    unique_values <- unique(numeric_var[!is.na(numeric_var)])
                    if (length(unique_values) == 1) {
                        validation_results$warnings <- c(validation_results$warnings,
                            paste("Variable", var, "has only one unique value. Outlier detection not meaningful."))
                    } else if (length(unique_values) == 2) {
                        validation_results$warnings <- c(validation_results$warnings,
                            paste("Variable", var, "has only two unique values. Consider if outlier detection is appropriate."))
                    }
                }
                
                # Check for infinite values
                if (any(is.infinite(numeric_var), na.rm = TRUE)) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste("Variable", var, "contains infinite values. These will be excluded."))
                }
                
                # Check for extreme values that might indicate data entry errors
                if (length(unique_values) > 2) {
                    q75 <- quantile(numeric_var, 0.75, na.rm = TRUE)
                    q25 <- quantile(numeric_var, 0.25, na.rm = TRUE)
                    iqr <- q75 - q25
                    
                    extreme_low <- q25 - 10 * iqr
                    extreme_high <- q75 + 10 * iqr
                    
                    n_extreme <- sum(numeric_var < extreme_low | numeric_var > extreme_high, na.rm = TRUE)
                    if (n_extreme > 0) {
                        validation_results$info <- c(validation_results$info,
                            paste("Variable", var, "has", n_extreme, "potentially extreme values that may represent data entry errors."))
                    }
                }
            }
            
            # Check method compatibility
            method_category <- self$options$method_category
            if (method_category == "multivariate" && length(selected_vars) == 1) {
                validation_results$warnings <- c(validation_results$warnings,
                    "Multivariate methods selected but only one variable provided. Consider univariate methods.")
            }
            
            # Threshold settings.
            #
            # The jamovi GUI clamps these to the min/max in the .a.yaml, but
            # jmvcore::OptionNumber does NOT enforce them (verified: setting 9 on an
            # option declared max = 3 stores 9), so an R-API caller can reach every
            # branch below. They are kept, but they now say what the value DOES
            # rather than asserting a range the GUI already enforces.
            if (method_category %in% c("univariate", "composite", "all")) {
                zscore_threshold <- self$options$zscore_threshold
                if (zscore_threshold < 2) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste0("Z-score threshold ", zscore_threshold,
                               " is permissive: on a normally distributed variable roughly ",
                               base::format(round(100 * 2 * stats::pnorm(-abs(zscore_threshold)), 1), nsmall = 1),
                               "% of values fall beyond it by chance, so expect flags on clean data."))
                } else if (zscore_threshold > 5) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste0("Z-score threshold ", zscore_threshold,
                               " is far into the tail; on a normally distributed variable fewer than ",
                               "1 in a million values reach it, so most runs will flag nothing."))
                }

                iqr_multiplier <- self$options$iqr_multiplier
                if (iqr_multiplier < 1) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste0("IQR multiplier ", iqr_multiplier,
                               " places the fences inside Tukey's usual 1.5 and will flag a ",
                               "substantial share of ordinary values."))
                } else if (iqr_multiplier > 3) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste0("IQR multiplier ", iqr_multiplier,
                               " places the fences well beyond Tukey's usual 1.5; only very ",
                               "extreme values will be flagged."))
                }

                confidence_level <- self$options$confidence_level
                if (confidence_level < 0.9 || confidence_level >= 1) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste0("Confidence level ", confidence_level,
                               " is outside the 0.90-0.999 range the interval methods are set up for; ",
                               "a level at or above 1 cannot be estimated at all."))
                }
            }

            if (method_category %in% c("composite", "all")) {
                composite_threshold <- self$options$composite_threshold
                if (composite_threshold <= 0 || composite_threshold > 1.0) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste0("Composite threshold ", composite_threshold,
                               " is not a proportion between 0 and 1. It is compared against the ",
                               "share of methods that flagged each observation, so a value outside ",
                               "that range flags either everything or nothing."))
                }
            }
            
            # Add success message if no major issues
            if (length(validation_results$errors) == 0 && length(validation_results$warnings) == 0) {
                validation_results$info <- c(validation_results$info,
                    " Data validation passed. Analysis can proceed.")
            }
            
            return(validation_results)
        },

        .generateValidationSummary = function(validation_results) {
            html_content <- "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;'>"
            html_content <- paste0(html_content, "<h4 style='margin-top: 0;'> Data Validation Summary</h4>")
            
            # Add errors
            if (length(validation_results$errors) > 0) {
                html_content <- paste0(html_content, "<div style='background-color: rgba(216, 33, 50, 0.18); padding: 10px; border-radius: 4px; margin: 10px 0; color: inherit;'>")
                html_content <- paste0(html_content, "<h5 style='margin-top: 0;'> Errors (Analysis Stopped)</h5>")
                html_content <- paste0(html_content, "<ul>")
                for (error in validation_results$errors) {
                    html_content <- paste0(html_content, "<li>", htmltools::htmlEscape(error), "</li>")
                }
                html_content <- paste0(html_content, "</ul></div>")
            }
            
            # Add warnings
            if (length(validation_results$warnings) > 0) {
                html_content <- paste0(html_content, "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 10px; border-radius: 4px; margin: 10px 0; color: inherit;'>")
                html_content <- paste0(html_content, "<h5 style='margin-top: 0;'> Warnings</h5>")
                html_content <- paste0(html_content, "<ul>")
                for (warning in validation_results$warnings) {
                    html_content <- paste0(html_content, "<li>", htmltools::htmlEscape(warning), "</li>")
                }
                html_content <- paste0(html_content, "</ul></div>")
            }
            
            # Add info messages
            if (length(validation_results$info) > 0) {
                html_content <- paste0(html_content, "<div style='background-color: rgba(33, 163, 188, 0.21); padding: 10px; border-radius: 4px; margin: 10px 0; color: inherit;'>")
                html_content <- paste0(html_content, "<h5 style='margin-top: 0;'> Information</h5>")
                html_content <- paste0(html_content, "<ul>")
                for (info in validation_results$info) {
                    html_content <- paste0(html_content, "<li>", htmltools::htmlEscape(info), "</li>")
                }
                html_content <- paste0(html_content, "</ul></div>")
            }
            
            html_content <- paste0(html_content, "</div>")
            return(html_content)
        }

    )
)
