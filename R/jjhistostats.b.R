#' @title Histogram
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import rlang
#' @import purrr
#' @import glue
#' @import ggstatsplot
#' @importFrom digest digest


jjhistostatsClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "jjhistostatsClass",
        inherit = jjhistostatsBase,
        private = list(

            # Cache for processed data and options to avoid redundant computation
            .processedData = NULL,
            .processedOptions = NULL,
            .processedAesthetics = NULL,
            .optionsHash = NULL,

            # init ----
            .init = function() {
                private$.applyClinicalPreset()

                deplen <- length(self$options$dep)

                # Use configurable plot dimensions
                plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 600
                plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450

                # Improved height calculation to prevent compressed plots
                # Add extra spacing when combining multiple plots vertically
                if (deplen > 1) {
                    # Add 15% extra height per plot for better spacing
                    total_height <- deplen * plotheight * 1.15
                } else {
                    total_height <- plotheight
                }

                self$results$plot$setSize(plotwidth, total_height)


                if (!is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                num_levels <- nlevels(
                    as.factor(mydata[[grvar]])
                )

                # For grouped analysis, calculate width based on layout
                ncol_estimate <- ceiling(sqrt(num_levels))
                grouped_width <- ncol_estimate * plotwidth

                # Height calculation for grouped plots with multiple dependent variables
                if (deplen > 1) {
                    grouped_height <- deplen * plotheight * 1.15
                } else {
                    # For single dep var, height based on number of grouping levels
                    nrow_estimate <- ceiling(num_levels / ncol_estimate)
                    grouped_height <- nrow_estimate * plotheight
                }

                self$results$plot2$setSize(grouped_width, grouped_height)

                }



            },

            # Calculate hash of current options to detect changes
            .calculateOptionsHash = function() {
                options_to_hash <- list(
                    dep = self$options$dep,
                    grvar = self$options$grvar,
                    typestatistics = self$options$typestatistics,
                    changebinwidth = self$options$changebinwidth,
                    binwidth = self$options$binwidth,
                    centralityline = self$options$centralityline,
                    centralitytype = self$options$centralitytype,
                    resultssubtitle = self$options$resultssubtitle,
                    enableOneSampleTest = self$options$enableOneSampleTest,
                    test.value = self$options$test.value,
                    conf.level = self$options$conf.level,
                    bf.message = self$options$bf.message,
                    digits = self$options$digits,
                    binfill = self$options$binfill,
                    bincolor = self$options$bincolor,
                    binalpha = self$options$binalpha,
                    centralitylinecolor = self$options$centralitylinecolor,
                    centralitylinewidth = self$options$centralitylinewidth,
                    centralitylinetype = self$options$centralitylinetype,
                    xlab = self$options$xlab,
                    title = self$options$title,
                    subtitle = self$options$subtitle,
                    caption = self$options$caption,
                    plotwidth = self$options$plotwidth,
                    plotheight = self$options$plotheight
                )
                digest::digest(options_to_hash, algo = "md5")
            },

            # Check if cache should be invalidated
            .shouldRefreshCache = function() {
                current_hash <- private$.calculateOptionsHash()
                if (is.null(private$.optionsHash) || private$.optionsHash != current_hash) {
                    private$.optionsHash <- current_hash
                    return(TRUE)
                }
                return(FALSE)
            },

            # Optimized data preparation with caching
            .prepareData = function(force_refresh = FALSE) {
                if (!is.null(private$.processedData) && !force_refresh && !private$.shouldRefreshCache()) {
                    return(private$.processedData)
                }

                # Prepare data with progress feedback
                self$results$todo$setContent(
                    glue::glue("<br>Processing data for histogram analysis...<br><hr>")
                )

                # Checkpoint before expensive data processing
                private$.checkpoint()

                # Only copy data if transformations are needed
                vars <- self$options$dep
                needs_transformation <- FALSE
                
                if (!is.null(vars)) {
                    for (var in vars) {
                        if (!is.numeric(self$data[[var]])) {
                            needs_transformation <- TRUE
                            break
                        }
                    }
                }
                
                # VALIDATE NUMERIC VARIABLES - reject factors instead of blind conversion
                factor_warnings <- character()
                for (var in vars) {
                    if (is.factor(self$data[[var]])) {
                        factor_warnings <- c(factor_warnings, var)
                    }
                }

                # Stop if any factors detected
                if (length(factor_warnings) > 0) {
                    stop(paste0("Histogram analysis requires numeric variables. The following variables are categorical: ",
                               paste(factor_warnings, collapse = ", "),
                               ". Please select continuous numeric variables for histogram analysis."))
                }

                mydata <- self$data

                # SELECTIVE NA OMISSION - only remove rows with NAs in selected variables
                # This prevents dropping patients with NAs in unused columns
                relevant_cols <- vars

                # Add grouping variable if present
                if (!is.null(self$options$grvar)) {
                    relevant_cols <- c(relevant_cols, self$options$grvar)
                }

                # Checkpoint before NA exclusion
                private$.checkpoint(flush = FALSE)

                # Count rows before and after NA removal
                n_before <- nrow(mydata)
                mydata <- mydata[complete.cases(mydata[relevant_cols]), ]
                n_after <- nrow(mydata)

                # Report NA removal if any occurred
                if (n_before > n_after) {
                    n_dropped <- n_before - n_after
                    self$results$todo$setContent(
                        glue::glue("<br>ℹ️ Info: {n_dropped} rows excluded due to missing values in selected variables.<br>",
                                  "Rows with data: {n_after} of {n_before} ({round(100 * n_after / n_before, 1)}%)<br><hr>")
                    )
                }

                # Cache the processed data
                private$.processedData <- mydata
                return(mydata)
            },

            # Shared plot generation function to eliminate duplication
            .generateHistogram = function(data, x_var, options_data, aesthetics_data, grvar_sym = NULL, messages = TRUE) {
                # Checkpoint before expensive statistical plot generation
                private$.checkpoint(flush = FALSE)

                # Build base arguments common to all plots
                base_args <- list(
                    data = data,
                    x = rlang::sym(x_var),
                    messages = messages,
                    type = options_data$typestatistics,
                    results.subtitle = options_data$resultssubtitle,
                    centrality.plotting = options_data$centralityline,
                    binwidth = options_data$binwidth,
                    conf.level = options_data$conf.level,
                    bf.message = options_data$bf.message,
                    digits = options_data$digits,
                    xlab = aesthetics_data$xlab,
                    title = aesthetics_data$title,
                    subtitle = aesthetics_data$subtitle,
                    caption = aesthetics_data$caption,
                    bin.args = aesthetics_data$bin.args,
                    centrality.line.args = aesthetics_data$centrality.line.args
                )

                # Conditionally add test.value only if one-sample test is enabled
                if (options_data$enableOneSampleTest) {
                    base_args$test.value <- options_data$test.value
                }

                # Add grouping variable if provided
                if (!is.null(grvar_sym)) {
                    base_args$grouping.var <- grvar_sym
                }

                # Add centrality.type if specified
                if (!is.null(options_data$centrality.type)) {
                    base_args$centrality.type <- options_data$centrality.type
                }

                # Remove NULL arguments to prevent conflicts
                base_args <- base_args[!sapply(base_args, is.null)]

                # Checkpoint before calling expensive ggstatsplot functions
                private$.checkpoint(flush = FALSE)

                # Call appropriate function based on grouping
                if (is.null(grvar_sym)) {
                    do.call(ggstatsplot::gghistostats, base_args)
                } else {
                    do.call(ggstatsplot::grouped_gghistostats, base_args)
                }
            },
            
            # Consolidated input validation with clinical assumption checking
            .validateInputs = function() {
                # Check if dependent variables are selected
                if (is.null(self$options$dep) || length(self$options$dep) == 0) {
                    return(list(valid = FALSE, message = "Please select at least one variable for histogram analysis."))
                }
                
                # Check if data exists and has rows
                if (is.null(self$data) || nrow(self$data) == 0) {
                    return(list(valid = FALSE, message = "Data contains no (complete) rows"))
                }
                
                # Check if selected variables exist in data
                missing_vars <- self$options$dep[!self$options$dep %in% names(self$data)]
                if (length(missing_vars) > 0) {
                    return(list(valid = FALSE, message = paste("Selected variables not found in data:", paste(missing_vars, collapse = ", "))))
                }
                
                # Check if grouping variable exists if specified
                if (!is.null(self$options$grvar) && !self$options$grvar %in% names(self$data)) {
                    return(list(valid = FALSE, message = paste("Grouping variable '", self$options$grvar, "' not found in data", sep = "")))
                }
                
                # Check if binwidth is positive when manually specified
                if (self$options$changebinwidth && (!is.null(self$options$binwidth) && self$options$binwidth <= 0)) {
                    return(list(valid = FALSE, message = "Bin width must be a positive number"))
                }
                
                return(list(valid = TRUE, message = NULL))
            },
            
            # Clinical assumption checking and warnings
            .generateClinicalWarnings = function(data, variables) {
                warnings <- c()

                # WARN ABOUT TEST VALUE = 0 WHEN ONE-SAMPLE TEST IS ENABLED
                # Testing "is mean = 0?" is almost never clinically relevant
                if (self$options$enableOneSampleTest &&
                    !is.null(self$options$test.value) &&
                    self$options$test.value == 0) {
                    # Check if any variable has all positive or all negative values
                    has_irrelevant_test <- FALSE
                    for (var in variables) {
                        if (!var %in% names(data)) next
                        var_data <- data[[var]][!is.na(data[[var]])]
                        if (length(var_data) > 0) {
                            if (all(var_data > 0) || all(var_data < 0)) {
                                has_irrelevant_test <- TRUE
                                break
                            }
                        }
                    }

                    if (has_irrelevant_test) {
                        warnings <- c(warnings, paste0(
                            "🚨 <strong>CRITICAL: TEST VALUE = 0 WARNING</strong> 🚨<br>",
                            "Testing 'is the mean equal to 0?' is <strong>rarely clinically meaningful</strong> for biomedical data. ",
                            "Your data contains only positive or only negative values, making a test against 0 inappropriate.<br>",
                            "<strong>RECOMMENDED ACTION:</strong> Change the 'Test Value' to a clinically relevant threshold:<br>",
                            "• Normal reference range limit (e.g., upper limit for cholesterol)<br>",
                            "• Treatment decision threshold (e.g., cutoff for intervention)<br>",
                            "• Population norm or expected value (e.g., established biomarker level)<br>",
                            "• Or <strong>uncheck 'Enable One-Sample Test'</strong> if you only need descriptive statistics."
                        ))
                    }
                }

                for (var in variables) {
                    if (!var %in% names(data)) next

                    var_data <- data[[var]]
                    var_data <- var_data[!is.na(var_data)]

                    if (length(var_data) == 0) next
                    
                    # Sample size warnings
                    if (length(var_data) < 30) {
                        warnings <- c(warnings, paste0(
                            "⚠️ Small sample size (n=", length(var_data), ") for '", var, 
                            "'. Consider nonparametric analysis or interpret results cautiously."
                        ))
                    }
                    
                    # Extreme outlier detection (values beyond 3 MAD from median)
                    if (length(var_data) > 5) {
                        med <- median(var_data)
                        mad_val <- mad(var_data)
                        if (mad_val > 0) {
                            outliers <- sum(abs(var_data - med) > 3 * mad_val)
                            if (outliers > 0) {
                                warnings <- c(warnings, paste0(
                                    "⚠️ Detected ", outliers, " extreme outlier(s) in '", var, 
                                    "'. Consider reviewing data quality or using robust methods."
                                ))
                            }
                        }
                    }
                    
                    # Constant data warning
                    if (length(unique(var_data)) == 1) {
                        warnings <- c(warnings, paste0(
                            "⚠️ Variable '", var, "' has constant values. Histogram analysis may not be meaningful."
                        ))
                    }
                    
                    # Very few unique values warning
                    if (length(unique(var_data)) < 5 && length(var_data) > 10) {
                        warnings <- c(warnings, paste0(
                            "⚠️ Variable '", var, "' has only ", length(unique(var_data)), 
                            " unique values. Consider treating as categorical or ordinal data."
                        ))
                    }
                }
                
                # Grouped analysis warnings
                if (!is.null(self$options$grvar) && self$options$grvar %in% names(data)) {
                    group_var <- data[[self$options$grvar]]
                    group_sizes <- table(group_var, useNA = "no")
                    
                    if (any(group_sizes < 10)) {
                        small_groups <- names(group_sizes[group_sizes < 10])
                        warnings <- c(warnings, paste0(
                            "⚠️ Small group size(s) detected: ", paste(small_groups, collapse = ", "), 
                            " (n < 10). Results may be unreliable for these groups."
                        ))
                    }
                }
                
                return(warnings)
            },
            
            # Generate performance warnings
            .generatePerformanceWarnings = function(data, options) {
                warnings <- c()
                
                # Bayesian analysis performance warning
                if (!is.null(options$typestatistics) && options$typestatistics == "bayes") {
                    n_rows <- nrow(data)
                    
                    if (n_rows > 1000) {
                        warnings <- c(warnings, paste0(
                            "🐌 <strong>SLOW COMPUTATION WARNING:</strong> Bayesian analysis with ", n_rows, 
                            " rows may take several minutes. Consider using 'parametric' or 'nonparametric' for faster results."
                        ))
                    } else if (n_rows > 500) {
                        warnings <- c(warnings, paste0(
                            "⏱️ <strong>PERFORMANCE NOTE:</strong> Bayesian analysis with ", n_rows, 
                            " rows may take 30-60 seconds. Be patient or switch to faster methods."
                        ))
                    } else {
                        warnings <- c(warnings, 
                            "⏱️ <strong>BAYESIAN ANALYSIS:</strong> This method provides rich uncertainty quantification but requires 15-30 seconds to compute. Consider parametric/nonparametric for instant results."
                        )
                    }
                    
                    # Additional Bayesian guidance
                    warnings <- c(warnings, 
                        "💡 <strong>Speed Tips:</strong> For similar insights with instant results, try 'Parametric (t-test)' for normally distributed data or 'Nonparametric (Mann-Whitney)' for skewed data."
                    )
                }
                
                # Large dataset general warning
                if (nrow(data) > 5000) {
                    warnings <- c(warnings, paste0(
                        "📊 <strong>LARGE DATASET:</strong> Processing ", nrow(data), 
                        " rows may take extra time for plot generation and statistical calculations."
                    ))
                }
                
                return(warnings)
            },
            
            # Generate clinical interpretation
            .generateClinicalInterpretation = function(data, variables) {
                if (!self$options$showInterpretation) return("")
                
                interpretation_parts <- c()
                
                for (var in variables) {
                    if (!var %in% names(data)) next
                    
                    var_data <- data[[var]]
                    var_data <- var_data[!is.na(var_data)]
                    
                    if (length(var_data) == 0) next
                    
                    # Basic descriptive statistics
                    n <- length(var_data)
                    mean_val <- mean(var_data)
                    median_val <- median(var_data)
                    sd_val <- sd(var_data)
                    
                    # Distribution shape assessment
                    skewness_val <- if (sd_val > 0) {
                        sum((var_data - mean_val)^3) / (n * sd_val^3)
                    } else 0
                    
                    # Normality assessment (simple)
                    is_normal <- abs(skewness_val) < 0.5 && n >= 30
                    
                    # Generate interpretation
                    var_interpretation <- paste0(
                        "<h4>", var, "</h4>",
                        "<ul>",
                        "<li><strong>Sample size:</strong> ", n, " observations</li>",
                        "<li><strong>Central tendency:</strong> Mean = ", round(mean_val, 2), 
                        ", Median = ", round(median_val, 2), "</li>",
                        "<li><strong>Variability:</strong> SD = ", round(sd_val, 2), "</li>",
                        "<li><strong>Distribution shape:</strong> ",
                        if (abs(skewness_val) < 0.5) {
                            "Approximately symmetric (suitable for parametric tests)"
                        } else if (skewness_val > 0.5) {
                            "Right-skewed (consider nonparametric tests or log transformation)"
                        } else {
                            "Left-skewed (consider nonparametric tests)"
                        }, "</li>",
                        "<li><strong>Clinical implications:</strong> ",
                        if (is_normal) {
                            "Normal distribution allows use of parametric statistics (t-tests, ANOVA). Mean and SD are appropriate summary measures."
                        } else {
                            "Non-normal distribution suggests using nonparametric tests (Mann-Whitney, Kruskal-Wallis). Median and IQR may be more appropriate summary measures."
                        }, "</li>",
                        "</ul>"
                    )
                    
                    interpretation_parts <- c(interpretation_parts, var_interpretation)
                }
                
                if (length(interpretation_parts) > 0) {
                    full_interpretation <- paste0(
                        "<div style='background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 15px; margin: 10px 0;'>",
                        "<h3>Clinical Interpretation</h3>",
                        "<div style='background-color: #fff3cd; border-left: 3px solid #ffc107; padding: 10px; margin: 10px 0;'>",
                        "<strong>⚠️ Note:</strong> This interpretation uses simplified heuristics (skewness &lt; 0.5 and n ≥ 30) ",
                        "as initial screening guidance. These are <strong>rule-of-thumb approximations</strong>, not formal ",
                        "diagnostic criteria. Always supplement with formal normality tests (Shapiro-Wilk, Anderson-Darling) ",
                        "and expert clinical judgment before making analysis decisions.",
                        "</div>",
                        paste(interpretation_parts, collapse = ""),
                        "<hr>",
                        "<h4>Recommendations:</h4>",
                        "<ul>",
                        "<li>Review distribution shapes to select appropriate statistical tests</li>",
                        "<li>Consider outliers and their clinical significance</li>",
                        "<li>For biomarker data, evaluate reference ranges and clinical cutoffs</li>",
                        "<li>Use grouped analysis to compare distributions between clinical subgroups</li>",
                        "<li><strong>Verify normality assumptions</strong> with formal statistical tests before using parametric methods</li>",
                        "</ul>",
                        "</div>"
                    )
                    return(full_interpretation)
                } else {
                    return("")
                }
            },
            
            # Optimized aesthetic preparation with caching
            .prepareAesthetics = function(force_refresh = FALSE) {
                if (!is.null(private$.processedAesthetics) && !force_refresh && !private$.shouldRefreshCache()) {
                    return(private$.processedAesthetics)
                }

                # Process bin.args
                bin.args <- list(
                    fill = self$options$binfill,
                    color = self$options$bincolor,
                    alpha = self$options$binalpha
                )
                
                # Process centrality.line.args
                centrality.line.args <- list(
                    color = self$options$centralitylinecolor,
                    linewidth = self$options$centralitylinewidth,
                    linetype = self$options$centralitylinetype
                )
                
                # Process text parameters
                xlab <- if (self$options$xlab != '') self$options$xlab else NULL
                title <- if (self$options$title != '') self$options$title else NULL
                subtitle <- if (self$options$subtitle != '') self$options$subtitle else NULL
                caption <- if (self$options$caption != '') self$options$caption else NULL
                
                aesthetics_list <- list(
                    bin.args = bin.args,
                    centrality.line.args = centrality.line.args,
                    xlab = xlab,
                    title = title,
                    subtitle = subtitle,
                    caption = caption
                )
                
                private$.processedAesthetics <- aesthetics_list
                return(aesthetics_list)
            },


            # Optimized options preparation with caching
            .prepareOptions = function(force_refresh = FALSE) {
                if (!is.null(private$.processedOptions) && !force_refresh && !private$.shouldRefreshCache()) {
                    return(private$.processedOptions)
                }

                # Prepare options with progress feedback
                self$results$todo$setContent(
                    glue::glue("<br>Preparing histogram analysis options...<br><hr>")
                )

                # Process core analysis options
                typestatistics <- self$options$typestatistics
                dep <- self$options$dep
                
                # Process binwidth
                binwidth <- NULL
                if (self$options$changebinwidth) {
                    binwidth <- self$options$binwidth
                }
                
                # Process centrality.type
                centrality.type <- if (self$options$centralitytype != 'default') self$options$centralitytype else NULL
                
                # Cache the processed options
                options_list <- list(
                    typestatistics = typestatistics,
                    dep = dep,
                    binwidth = binwidth,
                    resultssubtitle = self$options$resultssubtitle,
                    centralityline = self$options$centralityline,
                    enableOneSampleTest = self$options$enableOneSampleTest,
                    test.value = self$options$test.value,
                    conf.level = self$options$conf.level,
                    bf.message = self$options$bf.message,
                    digits = self$options$digits,
                    centrality.type = centrality.type
                )
                private$.processedOptions <- options_list
                return(options_list)
            },

            # run ----
            .run = function() {
                ## Initial Message ----
                if (is.null(self$options$dep) || length(self$options$dep) == 0) {

                    ## todo ----

                    todo <- glue::glue(
                    "<div style='background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 20px; margin: 10px 0; border-radius: 5px;'>
                    <h2 style='color: #495057; margin-top: 0;'>📊 Histogram Analysis</h2>
                    <p style='font-size: 16px; color: #6c757d; margin: 15px 0;'>
                    <strong>Welcome to ClinicoPath Histogram Tool!</strong><br>
                    Create statistical histograms with clinical interpretation and advanced visualization options.
                    </p>
                    
                    <div style='background-color: #e3f2fd; border-left: 4px solid #2196f3; padding: 15px; margin: 15px 0;'>
                    <h4 style='color: #1976d2; margin-top: 0;'>🎯 Getting Started:</h4>
                    <ol style='margin: 10px 0; padding-left: 20px;'>
                    <li><strong>Select Variables:</strong> Choose one or more continuous variables from the left panel</li>
                    <li><strong>Optional Grouping:</strong> Add a grouping variable to compare distributions between groups</li>
                    <li><strong>Choose Analysis Type:</strong> Use clinical presets or customize statistical settings</li>
                    </ol>
                    </div>
                    
                    <div style='background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px; margin: 15px 0;'>
                    <h4 style='color: #856404; margin-top: 0;'>🩺 Clinical Examples:</h4>
                    <ul style='margin: 10px 0; padding-left: 20px;'>
                    <li><strong>Lab Values:</strong> Cholesterol levels, blood glucose, biomarker concentrations</li>
                    <li><strong>Patient Characteristics:</strong> Age distribution, BMI, vital signs</li>
                    <li><strong>Pathology Scores:</strong> Tumor grades, staging scores, severity ratings</li>
                    <li><strong>Treatment Outcomes:</strong> Response measurements, survival times, quality scores</li>
                    </ul>
                    </div>
                    
                    <p style='font-size: 14px; color: #868e96; margin: 20px 0 0 0;'>
                    📚 <strong>Documentation:</strong> 
                    <a href='https://indrajeetpatil.github.io/ggstatsplot/reference/gghistostats.html' target='_blank' style='color: #007bff;'>gghistostats</a> | 
                    <a href='https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_gghistostats.html' target='_blank' style='color: #007bff;'>grouped_gghistostats</a>
                    </p>
                    </div>"
                    )

                    self$results$todo$setContent(todo)

                    return()

                } else {

                    todo <- glue::glue("<br>You have selected to make a histogram.<br><hr>")

                    self$results$todo$setContent(todo)

                    # Use consistent validation approach (but only for meaningful validation issues)
                    validation_result <- private$.validateInputs()
                    if (!validation_result$valid) {
                        # Only show error for actual data problems, not missing variables
                        if (!grepl("Please select at least one variable", validation_result$message)) {
                            stop(validation_result$message)
                        }
                    }

                    # Pre-process data, options, and aesthetics for performance
                    mydata <- private$.prepareData()
                    private$.prepareOptions()
                    private$.prepareAesthetics()
                    
                    # Generate clinical warnings and performance warnings
                    warnings <- private$.generateClinicalWarnings(mydata, self$options$dep)
                    performance_warnings <- private$.generatePerformanceWarnings(mydata, self$options)
                    
                    all_warnings <- c(warnings, performance_warnings)
                    
                    if (length(all_warnings) > 0) {
                        warning_text <- paste(all_warnings, collapse = "<br>")
                        todo <- glue::glue(
                            "<br>You have selected to make a histogram.<br><hr>",
                            "<div style='background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 10px; margin: 5px 0;'>",
                            "<strong>Clinical & Performance Considerations:</strong><br>",
                            "{warning_text}",
                            "</div><hr>"
                        )
                        self$results$todo$setContent(todo)
                    }
                    
                    # Generate and populate clinical interpretation
                    if (self$options$showInterpretation) {
                        interpretation_content <- private$.generateClinicalInterpretation(mydata, self$options$dep)
                        if (interpretation_content != "") {
                            self$results$interpretation$setContent(interpretation_content)
                        }
                    }

                }
            }

            ,
            .plot = function(image, ggtheme, theme, ...) {
                # Main plot generation function
                
                # Early return if no variables selected (don't show error)
                if (is.null(self$options$dep) || length(self$options$dep) == 0) {
                    return()
                }
                
                # Validate inputs using shared helper
                validation_result <- private$.validateInputs()
                if (!validation_result$valid) {
                    if (!is.null(validation_result$message)) {
                        stop(validation_result$message)
                    }
                    return()
                }

                # Checkpoint before data preparation and plot generation
                private$.checkpoint()

                # Use cached data, options, and aesthetics for performance
                mydata <- private$.prepareData()
                options_data <- private$.prepareOptions()
                aesthetics_data <- private$.prepareAesthetics()
                
                dep <- options_data$dep

                # Single variable plot
                if (length(self$options$dep) == 1) {
                    plot <- private$.generateHistogram(
                        data = mydata,
                        x_var = dep,
                        options_data = options_data,
                        aesthetics_data = aesthetics_data
                    )
                }

                # Multiple variable plots
                if (length(self$options$dep) > 1) {
                    dep2 <- as.list(self$options$dep)

                    # Checkpoint before expensive loop over multiple variables
                    private$.checkpoint()

                    plotlist <- purrr::map(
                        dep2,
                        function(x_var) {
                            # Each iteration processes a different variable - checkpoint for responsiveness
                            private$.checkpoint(flush = FALSE)
                            private$.generateHistogram(
                                data = mydata,
                                x_var = x_var,
                                options_data = options_data,
                                aesthetics_data = aesthetics_data,
                                messages = FALSE
                            )
                        }
                    )

                    # Checkpoint before expensive plot combination
                    private$.checkpoint(flush = FALSE)

                    plot <- ggstatsplot::combine_plots(
                        plotlist = plotlist,
                        plotgrid.args = list(
                            ncol = 1,
                            heights = rep(1, length(plotlist))
                        ),
                        annotation.args = list(
                            tag_levels = "A"
                        )
                    )
                }

                # Print plot
                print(plot)
                TRUE
            }


            ,
            .plot2 = function(image, ggtheme, theme, ...) {
                # Grouped plot generation function
                
                # Early return if no variables selected (don't show error)
                if (is.null(self$options$dep) || length(self$options$dep) == 0) {
                    return()
                }
                
                # Check for required grouping variable
                if (is.null(self$options$grvar))
                    return()
                    
                # Validate inputs
                validation_result <- private$.validateInputs()
                if (!validation_result$valid) {
                    if (!is.null(validation_result$message)) {
                        stop(validation_result$message)
                    }
                    return()
                }

                # Checkpoint before data preparation and grouped plot generation
                private$.checkpoint()

                # Use cached data, options, and aesthetics for performance
                mydata <- private$.prepareData()
                options_data <- private$.prepareOptions()
                aesthetics_data <- private$.prepareAesthetics()
                
                dep <- options_data$dep
                grvar <- self$options$grvar

                # Single variable grouped plot
                if (length(self$options$dep) == 1) {
                    plot2 <- private$.generateHistogram(
                        data = mydata,
                        x_var = dep,
                        options_data = options_data,
                        aesthetics_data = aesthetics_data,
                        grvar_sym = rlang::sym(grvar)
                    )
                }

                # Multiple variable grouped plots
                if (length(self$options$dep) > 1) {
                    dep2 <- as.list(self$options$dep)

                    # Checkpoint before expensive loop over multiple grouped variables
                    private$.checkpoint()

                    plotlist <- purrr::map(
                        dep2,
                        function(x_var) {
                            # Each iteration processes a different grouped variable - checkpoint for responsiveness
                            private$.checkpoint(flush = FALSE)
                            private$.generateHistogram(
                                data = mydata,
                                x_var = x_var,
                                options_data = options_data,
                                aesthetics_data = aesthetics_data,
                                grvar_sym = rlang::sym(grvar),
                                messages = FALSE
                            )
                        }
                    )

                    # Checkpoint before expensive grouped plot combination
                    private$.checkpoint(flush = FALSE)

                    plot2 <- ggstatsplot::combine_plots(
                        plotlist = plotlist,
                        plotgrid.args = list(
                            ncol = 1,
                            heights = rep(1, length(plotlist))
                        ),
                        annotation.args = list(
                            tag_levels = "A"
                        )
                    )
                }

                # Print plot
                print(plot2)
                TRUE
            }

            ,
            .plotGGPubr = function(image, ...) {
                # Validate inputs
                if (is.null(self$options$dep))
                    return()

                # Skip if ggpubr plot not requested
                if (!self$options$addGGPubrPlot)
                    return()

                # Prepare data
                mydata <- self$data
                dep <- self$options$dep

                # Single variable
                if (length(dep) == 1) {
                    # Build histogram arguments
                    args <- list(
                        data = mydata,
                        x = dep,
                        fill = self$options$ggpubrPalette,
                        add_density = self$options$ggpubrAddDensity
                    )

                    # Create histogram
                    plot <- do.call(ggpubr::gghistogram, args)

                    # Add mean line if requested
                    if (self$options$ggpubrAddMean) {
                        mean_val <- mean(mydata[[dep]], na.rm = TRUE)
                        plot <- plot + ggplot2::geom_vline(xintercept = mean_val,
                                                          color = "red",
                                                          linetype = "dashed",
                                                          linewidth = 1)
                    }

                    # Apply theme
                    plot <- plot + ggpubr::theme_pubr()

                    print(plot)
                }

                # Multiple variables
                if (length(dep) > 1) {
                    dep_list <- as.list(dep)

                    plotlist <- lapply(dep_list, function(depvar) {
                        args <- list(
                            data = mydata,
                            x = depvar,
                            fill = self$options$ggpubrPalette,
                            add_density = self$options$ggpubrAddDensity,
                            title = depvar
                        )

                        p <- do.call(ggpubr::gghistogram, args)

                        if (self$options$ggpubrAddMean) {
                            mean_val <- mean(mydata[[depvar]], na.rm = TRUE)
                            p <- p + ggplot2::geom_vline(xintercept = mean_val,
                                                        color = "red",
                                                        linetype = "dashed",
                                                        linewidth = 1)
                        }

                        p <- p + ggpubr::theme_pubr()
                        return(p)
                    })

                    plot <- ggpubr::ggarrange(plotlist = plotlist, ncol = 1, nrow = length(dep))
                    print(plot)
                }

                TRUE
            }

            ,
            .plotGGPubr2 = function(image, ...) {
                # Validate inputs
                if (is.null(self$options$dep) || is.null(self$options$grvar))
                    return()

                # Skip if ggpubr plot not requested
                if (!self$options$addGGPubrPlot)
                    return()

                # Prepare data
                mydata <- self$data
                dep <- self$options$dep
                grvar <- self$options$grvar

                # Single variable with faceting
                if (length(dep) == 1) {
                    args <- list(
                        data = mydata,
                        x = dep,
                        fill = self$options$ggpubrPalette,
                        add_density = self$options$ggpubrAddDensity,
                        facet.by = grvar
                    )

                    plot <- do.call(ggpubr::gghistogram, args)

                    if (self$options$ggpubrAddMean) {
                        mean_val <- mean(mydata[[dep]], na.rm = TRUE)
                        plot <- plot + ggplot2::geom_vline(xintercept = mean_val,
                                                          color = "red",
                                                          linetype = "dashed",
                                                          linewidth = 1)
                    }

                    plot <- plot + ggpubr::theme_pubr()
                    print(plot)
                }

                # Multiple variables with faceting
                if (length(dep) > 1) {
                    dep_list <- as.list(dep)

                    plotlist <- lapply(dep_list, function(depvar) {
                        args <- list(
                            data = mydata,
                            x = depvar,
                            fill = self$options$ggpubrPalette,
                            add_density = self$options$ggpubrAddDensity,
                            facet.by = grvar,
                            title = depvar
                        )

                        p <- do.call(ggpubr::gghistogram, args)

                        if (self$options$ggpubrAddMean) {
                            mean_val <- mean(mydata[[depvar]], na.rm = TRUE)
                            p <- p + ggplot2::geom_vline(xintercept = mean_val,
                                                        color = "red",
                                                        linetype = "dashed",
                                                        linewidth = 1)
                        }

                        p <- p + ggpubr::theme_pubr()
                        return(p)
                    })

                    plot <- ggpubr::ggarrange(plotlist = plotlist, ncol = 1, nrow = length(dep))
                    print(plot)
                }

                TRUE
            }

            ,
            .plotDensity = function(image, ...) {
                if (is.null(self$options$dep) || !self$options$addDistributionDiagnostics)
                    return()

                mydata <- self$data
                dep <- self$options$dep

                if (length(dep) == 1) {
                    plot <- ggpubr::ggdensity(
                        mydata,
                        x = dep,
                        fill = self$options$ggpubrDensityColor,
                        add = "mean",
                        rug = TRUE
                    )
                    plot <- plot + ggpubr::theme_pubr()
                    print(plot)
                }

                TRUE
            }

            ,
            .plotQQ = function(image, ...) {
                if (is.null(self$options$dep) || !self$options$addDistributionDiagnostics || !self$options$ggpubrShowQQ)
                    return()

                mydata <- self$data
                dep <- self$options$dep

                if (length(dep) == 1) {
                    plot <- ggpubr::ggqqplot(
                        mydata,
                        x = dep,
                        color = self$options$ggpubrDensityColor
                    )
                    plot <- plot + ggpubr::theme_pubr()
                    print(plot)
                }

                TRUE
            }

            ,
            .plotECDF = function(image, ...) {
                if (is.null(self$options$dep) || !self$options$addDistributionDiagnostics || !self$options$ggpubrShowECDF)
                    return()

                mydata <- self$data
                dep <- self$options$dep

                if (length(dep) == 1) {
                    plot <- ggpubr::ggecdf(
                        mydata,
                        x = dep,
                        color = self$options$ggpubrDensityColor
                    )
                    plot <- plot + ggpubr::theme_pubr()
                    print(plot)
                }

                TRUE
            }
        )
    )