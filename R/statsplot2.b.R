#' @title Plots and Graphs Based on Variable Types
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr



statsplot2Class <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "statsplot2Class",
        inherit = statsplot2Base,
        private = list(
            # Cache for analysis results to avoid redundant calculations
            .cached_analysis = NULL,
            
            # Plot dimension constants
            .PLOT_DIMENSIONS = list(
                default = list(width = 800, height = 600),
                grouped_native = list(width_per_level = 400, height_per_level = 300, max_width = 1600, max_height = 1200),
                grouped_manual = list(width = 1200, height_per_row = 450, max_height = 1400)
            ),
            
            # Method to invalidate cache when options change
            .invalidateCache = function() {
                private$.cached_analysis <- NULL
            },
            
            # Standardized validation method for plot data
            .validatePlotData = function(prepared_data, plot_type) {
                data <- prepared_data$data
                x_var <- prepared_data$group
                y_var <- prepared_data$dep

                # Basic data validation
                if (nrow(data) < 2) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'insufficientData',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(glue::glue(
                        "Insufficient data for {plot_type}.\n",
                        "• Variables: {y_var} by {x_var}\n",
                        "• Found: {nrow(data)} observation(s)\n",
                        "• Required: ≥2 observations\n",
                        "• Check your data filtering."
                    ))
                    self$results$insert(1, notice)
                    return(FALSE)
                }

                # Variable-specific validation based on expected types
                y_data_clean <- data[[y_var]][!is.na(data[[y_var]])]
                x_data_clean <- data[[x_var]][!is.na(data[[x_var]])]

                # Check for sufficient non-missing values
                if (length(y_data_clean) < 2) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'insufficientDepValues',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(glue::glue(
                        "Dependent variable '{y_var}' has insufficient non-missing values for {plot_type}.\n",
                        "• Found: {length(y_data_clean)} valid value(s)\n",
                        "• Required: ≥2 valid values\n",
                        "• Check for missing data in '{y_var}'."
                    ))
                    self$results$insert(1, notice)
                    return(FALSE)
                }

                if (length(x_data_clean) < 2) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'insufficientGroupValues',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(glue::glue(
                        "Grouping variable '{x_var}' has insufficient non-missing values for {plot_type}.\n",
                        "• Found: {length(x_data_clean)} valid value(s)\n",
                        "• Required: ≥2 valid values\n",
                        "• Check for missing data in '{x_var}'."
                    ))
                    self$results$insert(1, notice)
                    return(FALSE)
                }

                # Factor-specific validation
                if (is.factor(data[[y_var]])) {
                    y_levels <- length(unique(y_data_clean))
                    if (y_levels < 1) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'noValidDepLevels',
                            type = jmvcore::NoticeType$ERROR
                        )
                        notice$setContent(glue::glue(
                            "Factor variable '{y_var}' has no valid levels for {plot_type}.\n",
                            "• All values are missing after data cleaning\n",
                            "• Check data for: {paste(unique(data[[y_var]]), collapse=', ')}"
                        ))
                        self$results$insert(1, notice)
                        return(FALSE)
                    }
                }

                if (is.factor(data[[x_var]])) {
                    x_levels <- length(unique(x_data_clean))
                    if (x_levels < 1) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'noValidGroupLevels',
                            type = jmvcore::NoticeType$ERROR
                        )
                        notice$setContent(glue::glue(
                            "Factor variable '{x_var}' has no valid levels for {plot_type}.\n",
                            "• All values are missing after data cleaning\n",
                            "• Check data for: {paste(unique(data[[x_var]]), collapse=', ')}"
                        ))
                        self$results$insert(1, notice)
                        return(FALSE)
                    }
                }

                return(TRUE)
            },
            
            # Private function to detect variable types and analysis parameters
            .detectAnalysisType = function(force_refresh = FALSE) {
                # Return cached result if available and no refresh requested
                if (!is.null(private$.cached_analysis) && !force_refresh) {
                    return(private$.cached_analysis)
                }
                # Return early if no variables selected
                if (is.null(self$options$dep) || is.null(self$options$group)) {
                    return(NULL)
                }
                
                # Get variable data
                mydep <- self$data[[self$options$dep]]
                mygroup <- self$data[[self$options$group]]
                
                # Define continuous types
                contin <- c("integer", "numeric", "double")
                
                # Determine variable types using inherits with contin array
                dep_type <- if (inherits(mydep, "factor")) {
                    "factor"
                } else if (inherits(mydep, contin)) {
                    "continuous"
                } else {
                    "unknown"
                }
                
                group_type <- if (inherits(mygroup, "factor")) {
                    "factor"
                } else if (inherits(mygroup, contin)) {
                    "continuous"
                } else {
                    "unknown"
                }
                
                # Get other options
                direction <- self$options$direction
                distribution <- self$options$distribution
                alluvsty <- self$options$alluvsty
                
                # Create analysis type identifier
                plot_type <- paste(direction, group_type, dep_type, sep = "_")
                
                # Create and cache analysis information
                analysis_info <- list(
                    dep_type = dep_type,
                    group_type = group_type,
                    direction = direction,
                    distribution = distribution,
                    alluvsty = alluvsty,
                    plot_type = plot_type,
                    dep_var = self$options$dep,
                    group_var = self$options$group,
                    grvar = self$options$grvar
                )
                
                # Add warnings for unexpected variable types
                if (dep_type == "unknown" || group_type == "unknown") {
                    warning(glue::glue("Unexpected variable types detected. {analysis_info$dep_var}: {class(mydep)[1]}, {analysis_info$group_var}: {class(mygroup)[1]}. Analysis may not work as expected."))
                }
                
                # Cache the result
                private$.cached_analysis <- analysis_info
                
                return(analysis_info)
            },

            .generateExplanationMessage = function(analysis_info) {
                # Format variable descriptions with their types
                dep_desc <- glue::glue("{analysis_info$dep_var} ({analysis_info$dep_type})")
                group_desc <- glue::glue("{analysis_info$group_var} ({analysis_info$group_type})")
                
                # Generate explanation message based on plot type
                base_message <- switch(analysis_info$plot_type,
                    "independent_factor_continuous" = glue::glue(
                        "You have selected to use a violin plot to compare {dep_desc} between independent groups defined by {group_desc}."
                    ),
                    "independent_continuous_continuous" = glue::glue(
                        "You have selected to use a scatter plot to examine the relationship between {group_desc} and {dep_desc}."
                    ),
                    "independent_factor_factor" = glue::glue(
                        "You have selected to use a bar chart to compare {dep_desc} across categories of {group_desc}."
                    ),
                    "independent_continuous_factor" = glue::glue(
                        "You have selected to compare {dep_desc} with {group_desc}. Note: Consider switching variables for a more appropriate visualization."
                    ),
                    "repeated_factor_continuous" = glue::glue(
                        "You have selected to use a violin plot to compare {dep_desc} between repeated measurements defined by {group_desc}. For more customization options, consider using jjstatsplot::jjwithinstats. To verify statistical results, check with jmv::ttestPS for paired samples t-test."
                    ),
                    "repeated_continuous_continuous" = glue::glue(
                        "Scatterplots for repeated measurements of {group_desc} and {dep_desc} aren't supported by specialized functions. Consider: 1) Using 'independent' design, 2) Creating difference scores, or 3) Using correlation analysis. A basic ggplot2 visualization will be generated instead."
                    ),
                    "repeated_factor_factor" = glue::glue(
                        "You have selected to compare repeated measurements of {dep_desc} and {group_desc} using an alluvial diagram."
                    ),
                    "repeated_continuous_factor" = glue::glue(
                        "This combination ({dep_desc} vs {group_desc}) in repeated measures has limited support. Consider: 1) Switching variables ({group_desc} as dependent), 2) Using 'independent' design, or 3) Creating summary scores. A basic ggplot2 visualization will be generated."
                    ),
                    # Default case for unknown combinations or types
                    glue::glue("This variable combination ({dep_desc} vs {group_desc} with {analysis_info$direction} design) will use a basic ggplot2 visualization since specialized statistical plots are not available.")
                )
                
                # Add notes about option applicability
                notes <- character(0)
                
                # Note about statistical approach
                if (analysis_info$dep_type == "factor" && analysis_info$group_type == "factor") {
                    notes <- c(notes, "Note: Statistical approach option does not apply to categorical comparisons.")
                }
                
                # Note about alluvial style
                if (analysis_info$plot_type == "repeated_factor_factor") {
                    notes <- c(notes, "Alluvial style option is now available for this repeated categorical comparison.")
                } else if (analysis_info$direction == "repeated") {
                    notes <- c(notes, "Alluvial style option only applies to repeated factor vs factor comparisons.")
                }
                
                # Combine messages
                if (length(notes) > 0) {
                    stat_exp <- glue::glue("{base_message}\n\n{paste(notes, collapse = '\n')}")
                } else {
                    stat_exp <- base_message
                }
                
                return(stat_exp)
            },
            
            # Generate clinical interpretation for results
            .generateClinicalInterpretation = function(analysis_info) {
                interpretation <- switch(analysis_info$plot_type,
                    "independent_factor_continuous" = glue::glue(
                        "Clinical Interpretation: This violin plot compares the distribution of {analysis_info$dep_var} between different {analysis_info$group_var} groups. Look for differences in medians (center lines) and spread (violin width). Wider violins indicate more variability. Statistical significance testing is included when applicable."
                    ),
                    "independent_continuous_continuous" = glue::glue(
                        "Clinical Interpretation: This scatter plot examines the linear relationship between {analysis_info$group_var} and {analysis_info$dep_var}. The trend line shows the association with confidence bands (gray area). Positive slopes indicate that higher {analysis_info$group_var} values are associated with higher {analysis_info$dep_var} values."
                    ),
                    "independent_factor_factor" = glue::glue(
                        "Clinical Interpretation: This bar chart compares the frequency distribution of {analysis_info$dep_var} categories across {analysis_info$group_var} groups. Height differences indicate varying proportions. Chi-square statistics test for independence between the variables."
                    ),
                    "repeated_factor_continuous" = glue::glue(
                        "Clinical Interpretation: This paired violin plot compares {analysis_info$dep_var} between two time points or conditions ({analysis_info$group_var}). Connected points show individual changes. The statistical test evaluates whether the mean change is significantly different from zero."
                    ),
                    "repeated_factor_factor" = glue::glue(
                        "Clinical Interpretation: This alluvial diagram shows how subjects transition between {analysis_info$dep_var} categories from {analysis_info$group_var}. Flow thickness represents the number of subjects. Useful for tracking changes in disease stages, treatment responses, or classifications over time."
                    ),
                    "independent_continuous_factor" = glue::glue(
                        "Clinical Interpretation: This dot plot shows the distribution of {analysis_info$group_var} values within each {analysis_info$dep_var} category. Each dot represents individual observations. Compare the central tendency and spread between categories."
                    ),
                    # Default for unsupported combinations
                    glue::glue("Clinical Interpretation: This basic plot shows the relationship between {analysis_info$dep_var} and {analysis_info$group_var}. While specialized statistical tests aren't available for this combination, the visualization can still provide valuable insights about patterns in your data.")
                )
                
                # Add assumption notes based on statistical approach
                assumption_notes <- ""
                if (analysis_info$distribution == "p") {
                    assumption_notes <- "\n\nParametric Approach: Assumes normally distributed data. Best for continuous variables with bell-shaped distributions."
                } else if (analysis_info$distribution == "np") {
                    assumption_notes <- "\n\nNonparametric Approach: Distribution-free method. Suitable for skewed data, ordinal scales, or when normality assumptions are violated."
                } else if (analysis_info$distribution == "r") {
                    assumption_notes <- "\n\nRobust Approach: Less sensitive to outliers. Good choice when your data contains extreme values that might affect standard statistical tests."
                } else if (analysis_info$distribution == "bf") {
                    assumption_notes <- "\n\nBayesian Approach: Provides evidence for or against the null hypothesis. Bayes factors > 3 suggest moderate evidence, > 10 suggest strong evidence."
                }
                
                return(paste0(interpretation, assumption_notes))
            },
            
            # Check statistical assumptions and provide warnings
            .checkAssumptions = function(analysis_info, data) {
                notices <- list()

                dep_data <- data[[analysis_info$dep_var]]
                group_data <- data[[analysis_info$group_var]]

                # Sample size checks
                total_n <- sum(!is.na(dep_data) & !is.na(group_data))
                if (total_n < 30) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'smallSample',
                        type = jmvcore::NoticeType$STRONG_WARNING
                    )
                    notice$setContent(glue::glue(
                        "Small sample size detected (n={total_n}).\n",
                        "• Nonparametric approaches recommended for n<30\n",
                        "• Consider robust statistical methods\n",
                        "• Results may have reduced statistical power"
                    ))
                    notices <- append(notices, list(notice))
                }

                # Parametric assumption checks
                if (analysis_info$distribution == "p" && analysis_info$dep_type == "continuous") {
                    # Check for extreme outliers (beyond 3.5 IQR)
                    if (length(dep_data) > 4) {
                        Q1 <- quantile(dep_data, 0.25, na.rm = TRUE)
                        Q3 <- quantile(dep_data, 0.75, na.rm = TRUE)
                        IQR_val <- Q3 - Q1
                        extreme_outliers <- sum(dep_data < (Q1 - 3.5 * IQR_val) | dep_data > (Q3 + 3.5 * IQR_val), na.rm = TRUE)
                        if (extreme_outliers > 0) {
                            notice <- jmvcore::Notice$new(
                                options = self$options,
                                name = 'extremeOutliers',
                                type = jmvcore::NoticeType$STRONG_WARNING
                            )
                            notice$setContent(glue::glue(
                                "Extreme outliers detected in {analysis_info$dep_var}.\n",
                                "• Found: {extreme_outliers} extreme outlier(s) (>3.5 IQR)\n",
                                "• Consider robust statistical approach (distribution='r')\n",
                                "• Outliers may unduly influence parametric results"
                            ))
                            notices <- append(notices, list(notice))
                        }
                    }

                    # Basic normality warning for small samples
                    if (total_n < 100) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'normalityCheck',
                            type = jmvcore::NoticeType$WARNING
                        )
                        notice$setContent(glue::glue(
                            "Consider checking distribution visually (n={total_n}).\n",
                            "• For samples <100, normality assumptions are critical\n",
                            "• Consider nonparametric approach if data appears skewed\n",
                            "• Inspect violin plot shape for distributional form"
                        ))
                        notices <- append(notices, list(notice))
                    }
                }

                # Group size balance check for between-subjects designs
                if (analysis_info$direction == "independent" && is.factor(group_data)) {
                    group_sizes <- table(group_data)
                    min_group <- min(group_sizes)
                    max_group <- max(group_sizes)
                    if (max_group / min_group > 4) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'unbalancedGroups',
                            type = jmvcore::NoticeType$WARNING
                        )
                        notice$setContent(glue::glue(
                            "Unbalanced group sizes detected.\n",
                            "• Smallest group: {min_group}\n",
                            "• Largest group: {max_group}\n",
                            "• Ratio: {round(max_group/min_group, 1)}:1\n",
                            "• Results may be less reliable with imbalanced designs"
                        ))
                        notices <- append(notices, list(notice))
                    }

                    # Very small group sizes
                    if (min_group < 5) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'verySmallGroups',
                            type = jmvcore::NoticeType$STRONG_WARNING
                        )
                        notice$setContent(glue::glue(
                            "Very small group size(s) detected.\n",
                            "• Minimum group size: {min_group}\n",
                            "• Consider combining groups if scientifically appropriate\n",
                            "• Consider exact statistical methods for small samples\n",
                            "• Statistical power may be severely limited"
                        ))
                        notices <- append(notices, list(notice))
                    }
                }

                # Repeated measures specific checks
                if (analysis_info$direction == "repeated") {
                    # Check for complete pairs
                    if (is.factor(group_data) && length(levels(group_data)) == 2) {
                        complete_pairs <- sum(complete.cases(dep_data, group_data))
                        if (complete_pairs < total_n) {
                            notice <- jmvcore::Notice$new(
                                options = self$options,
                                name = 'incompletePairs',
                                type = jmvcore::NoticeType$WARNING
                            )
                            notice$setContent(glue::glue(
                                "Missing paired observations detected.\n",
                                "• Complete pairs: {complete_pairs}\n",
                                "• Total observations: {total_n}\n",
                                "• Missing: {total_n - complete_pairs}\n",
                                "• Only complete pairs will be used in paired analysis"
                            ))
                            notices <- append(notices, list(notice))
                        }
                    }
                }

                return(notices)
            },

            .run = function() {

                StatStratum <- ggalluvial::StatStratum

                analysis_info <- NULL

                # Invalidate cache to ensure fresh analysis with current options
                private$.invalidateCache()
                
                # Get analysis type information
                analysis_info <- private$.detectAnalysisType()
                
                # If no variables selected, show initial message
                if (is.null(analysis_info)) {

                    todo <- glue::glue(
                "
                <br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate plots based on variable types.
                <br><br>
                This function uses ggstatsplot and ggalluvial packages. Please cite jamovi and the packages as given below.
                "
                    )

                    self$results$todo$setVisible(TRUE)
                    self$results$todo$setContent(todo)

                    return()

                }
                
                # Clear todo message
                self$results$todo$setVisible(FALSE)

                # Enhanced data validation with context
                if (nrow(self$data) == 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'emptyDataset',
                        type = jmvcore::NoticeType$ERROR
                    )
                    dep_name <- self$options$dep %||% "not selected"
                    group_name <- self$options$group %||% "not selected"
                    notice$setContent(glue::glue(
                        "No data available for analysis.\n",
                        "• Variables selected: dependent='{dep_name}', grouping='{group_name}'\n",
                        "• Check data loading and variable selection\n",
                        "• Verify dataset is not empty"
                    ))
                    self$results$insert(1, notice)
                    return()
                }

                # Check assumptions and insert notices
                assumption_notices <- private$.checkAssumptions(analysis_info, self$data)
                if (length(assumption_notices) > 0) {
                    # Insert assumption notices at top positions (after any ERROR notices)
                    # STRONG_WARNING notices first, then WARNING notices
                    strong_warnings <- Filter(function(n) identical(n$type, jmvcore::NoticeType$STRONG_WARNING), assumption_notices)
                    warnings <- Filter(function(n) identical(n$type, jmvcore::NoticeType$WARNING), assumption_notices)

                    position <- 1
                    for (notice in strong_warnings) {
                        self$results$insert(position, notice)
                        position <- position + 1
                    }
                    for (notice in warnings) {
                        self$results$insert(position, notice)
                        position <- position + 1
                    }
                }

                # Generate explanation message using the new function
                stat_exp <- private$.generateExplanationMessage(analysis_info)

                # Generate clinical interpretation
                clinical_interpretation <- private$.generateClinicalInterpretation(analysis_info)

                # Combine explanations (without warnings - those are now separate Notices)
                combined_explanation <- paste(stat_exp, "\n\n", clinical_interpretation, sep = "")

                # Set the explanation message in results
                self$results$ExplanationMessage$setContent(combined_explanation)

                # Add success summary at the end
                success <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'analysisComplete',
                    type = jmvcore::NoticeType$INFO
                )
                n_total <- nrow(self$data)
                n_used <- n_total  # Will be updated if sampling occurred

                success$setContent(glue::glue(
                    "Analysis completed successfully.\n",
                    "• Plot type: {analysis_info$plot_type}\n",
                    "• Observations: {n_used:,} of {n_total:,}\n",
                    "• Statistical approach: {analysis_info$distribution}\n",
                    "• Study design: {analysis_info$direction}"
                ))
                self$results$insert(999, success)

            },
            
            .init = function() {
                # Centralized package dependency checking
                required_packages <- c("ggstatsplot", "ggalluvial", "dplyr", "easyalluvial", "patchwork", "cowplot")
                missing_packages <- required_packages[!sapply(required_packages, function(pkg) requireNamespace(pkg, quietly = TRUE))]
                if (length(missing_packages) > 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'missingPackages',
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(glue::glue(
                        "Optional packages missing for full functionality.\n",
                        "• Missing: {paste(missing_packages, collapse = ', ')}\n",
                        "• Install with: install.packages(c({paste(shQuote(missing_packages), collapse = ', ')}))\n",
                        "• Basic functionality will still work\n",
                        "• Some plot types may fall back to simpler visualizations"
                    ))
                    self$results$insert(1, notice)
                }
                
                # Initialize and set option visibility based on selected variables
                
                # Get analysis type information
                analysis_info <- private$.detectAnalysisType()
                
                if (is.null(analysis_info)) {
                    # No variables selected - hide conditional options
                    # Keep all options visible initially
                    return()
                }
                
                # Determine which options should be enabled
                
                # 1. Study Design (direction) - Always relevant when variables are selected
                # Could be disabled for specific unsupported combinations
                
                # 2. Statistical Approach (distribution) - Only relevant for quantitative analyses
                # Enable for: continuous outcomes, disable for pure categorical comparisons
                enable_distribution <- (
                    analysis_info$dep_type == "continuous" || 
                    analysis_info$group_type == "continuous"
                )
                
                # 3. Alluvial Style - Only relevant for repeated factor vs factor
                enable_alluvial <- (
                    analysis_info$direction == "repeated" && 
                    analysis_info$dep_type == "factor" && 
                    analysis_info$group_type == "factor"
                )
                
                # Apply visibility rules (if UI supports it)
                # Note: Jamovi may not support dynamic enable/disable in all versions
                # This is primarily for documentation of when options are relevant
                
                if (!enable_distribution) {
                    # Statistical approach not relevant for pure categorical comparisons
                    # User should be aware this option doesn't affect factor vs factor plots
                }
                
                if (!enable_alluvial) {
                    # Alluvial style only matters for repeated factor vs factor
                    # Hide or disable this option for other combinations
                }
                
                # You could also add informative messages
                if (analysis_info$plot_type == "independent_factor_factor" && 
                    !is.null(self$options$distribution) && 
                    self$options$distribution != "p") {
                    # Note: Statistical approach doesn't affect bar charts for factor comparisons
                }
                
                if (analysis_info$plot_type != "repeated_factor_factor" && 
                    !is.null(self$options$alluvsty)) {
                    # Note: Alluvial style only applies to repeated factor comparisons
                }
            },
            
            # Fallback plot using basic ggplot2 when all else fails
            .plotFallback = function(prepared_data, analysis_info) {
                # Create a basic ggplot based on variable types
                data <- prepared_data$data
                x_var <- prepared_data$group
                y_var <- prepared_data$dep
                
                # Determine the most appropriate basic plot
                if (analysis_info$dep_type == "continuous" && analysis_info$group_type == "continuous") {
                    # Scatter plot for continuous vs continuous
                    plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var))) +
                        ggplot2::geom_point(alpha = 0.6) +
                        ggplot2::geom_smooth(method = "lm", se = TRUE) +
                        ggplot2::labs(
                            title = paste("Basic Scatter Plot:", y_var, "vs", x_var),
                            subtitle = "Generated using basic ggplot2 (variable type checks bypassed)",
                            x = x_var,
                            y = y_var
                        )
                } else if (analysis_info$dep_type == "continuous" && analysis_info$group_type == "factor") {
                    # Box plot for continuous vs factor
                    plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var))) +
                        ggplot2::geom_boxplot(alpha = 0.7) +
                        ggplot2::geom_jitter(width = 0.2, alpha = 0.4) +
                        ggplot2::labs(
                            title = paste("Basic Box Plot:", y_var, "by", x_var),
                            subtitle = "Generated using basic ggplot2 (variable type checks bypassed)",
                            x = x_var,
                            y = y_var
                        )
                } else if (analysis_info$dep_type == "factor" && analysis_info$group_type == "continuous") {
                    # Histogram with faceting for factor vs continuous
                    plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x_var))) +
                        ggplot2::geom_histogram(bins = 20, alpha = 0.7, fill = "steelblue") +
                        ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(y_var)), scales = "free") +
                        ggplot2::labs(
                            title = paste("Basic Histogram:", x_var, "split by", y_var),
                            subtitle = "Generated using basic ggplot2 (variable type checks bypassed)",
                            x = x_var,
                            y = "Count"
                        )
                } else if (analysis_info$dep_type == "factor" && analysis_info$group_type == "factor") {
                    # Bar plot for factor vs factor
                    plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x_var), fill = !!rlang::sym(y_var))) +
                        ggplot2::geom_bar(position = "dodge", alpha = 0.8) +
                        ggplot2::labs(
                            title = paste("Basic Bar Plot:", y_var, "by", x_var),
                            subtitle = "Generated using basic ggplot2 (variable type checks bypassed)",
                            x = x_var,
                            y = "Count",
                            fill = y_var
                        )
                } else {
                    # Generic scatter plot for unknown types
                    plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var))) +
                        ggplot2::geom_point(alpha = 0.6) +
                        ggplot2::labs(
                            title = paste("Basic Plot:", y_var, "vs", x_var),
                            subtitle = "Generated using basic ggplot2 (variable type checks bypassed)",
                            x = x_var,
                            y = y_var
                        )
                }
                
                # Add grouping if present
                if (!is.null(prepared_data$grvar)) {
                    plot <- plot + 
                        ggplot2::facet_wrap(
                            ggplot2::vars(!!rlang::sym(prepared_data$grvar)),
                            scales = "free"
                        )
                }
                
                # Add theme
                plot <- plot + 
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.subtitle = ggplot2::element_text(color = "orange", size = 10),
                        plot.title = ggplot2::element_text(size = 12, face = "bold")
                    )
                
                return(plot)
            },
            
            # Prepare data for plotting (handle NA exclusion, term composition)
            .prepareDataForPlot = function(analysis_info) {
                # Get base data
                mydata <- self$data
                
                # Handle NA exclusion if requested
                if (self$options$excl) {
                    mydata <- jmvcore::naOmit(mydata)
                }
                
                # Handle large dataset sampling if requested
                original_nrow <- nrow(mydata)
                if (self$options$sampleLarge && original_nrow > 10000) {
                    set.seed(42)  # For reproducible sampling
                    sample_size <- 5000
                    mydata <- mydata[sample(nrow(mydata), sample_size), ]
                    message(glue::glue("Large dataset detected ({original_nrow:,} rows). Sampled {sample_size:,} rows for visualization performance. Disable 'Sample Large Datasets' option to use full dataset."))
                }
                
                # Prepare composed terms for use with ggstatsplot
                # Note: We use simple strings instead of composed terms to avoid NSE issues
                dep_var <- analysis_info$dep_var
                group_var <- analysis_info$group_var
                grvar <- analysis_info$grvar
                
                # Return prepared data and variable names
                list(
                    data = mydata,
                    dep = dep_var,
                    group = group_var,
                    grvar = grvar,
                    distribution = analysis_info$distribution,
                    alluvsty = analysis_info$alluvsty
                )
            },
            
            # Main dispatcher for plot generation
            .generatePlot = function(analysis_info, prepared_data) {
                # Check if grouped plot is needed
                if (!is.null(prepared_data$grvar)) {
                    result <- private$.plotGrouped(analysis_info, prepared_data)
                    if (!is.null(result)) {
                        return(result)
                    }
                    # If grouped plot fails, continue to fallback
                }
                
                # Try specialized plot functions first
                plot <- tryCatch({
                    switch(analysis_info$plot_type,
                        "independent_factor_continuous" = private$.plotBetweenStats(prepared_data),
                        "independent_continuous_continuous" = private$.plotScatterStats(prepared_data),
                        "independent_factor_factor" = private$.plotBarStats(prepared_data),
                        "independent_continuous_factor" = private$.plotDotplotStats(prepared_data),
                        "repeated_factor_continuous" = private$.plotWithinStats(prepared_data),
                        "repeated_factor_factor" = private$.plotAlluvial(prepared_data),
                        "repeated_continuous_continuous" = NULL,  # Will trigger fallback
                        "repeated_continuous_factor" = NULL,      # Will trigger fallback
                        NULL  # Will trigger fallback
                    )
                }, error = function(e) {
                    # If specialized function fails, return NULL to trigger fallback
                    message("Specialized plot function failed: ", conditionMessage(e))
                    return(NULL)
                })
                
                # If specialized plot failed or returned NULL, use fallback
                if (is.null(plot)) {
                    message("Using fallback ggplot2 visualization")
                    plot <- private$.plotFallback(prepared_data, analysis_info)
                }
                
                return(plot)
            },
            
            # Plot function for between-subjects comparisons (factor vs continuous)
            .plotBetweenStats = function(prepared_data) {
                # Validate data and return NULL if validation fails
                if (!private$.validatePlotData(prepared_data, "violin plot")) {
                    return(NULL)
                }

                # Checkpoint before expensive plot generation
                private$.checkpoint()

                plot <- ggstatsplot::ggbetweenstats(
                    data = prepared_data$data,
                    x = !!rlang::sym(prepared_data$group),
                    y = !!rlang::sym(prepared_data$dep),
                    type = prepared_data$distribution
                )
                return(plot)
            },
            
            # Plot function for scatter plots (continuous vs continuous)
            .plotScatterStats = function(prepared_data) {
                # Validate data and return NULL if validation fails
                if (!private$.validatePlotData(prepared_data, "scatter plot")) {
                    return(NULL)
                }

                # Checkpoint before expensive plot generation
                private$.checkpoint()

                plot <- ggstatsplot::ggscatterstats(
                    data = prepared_data$data,
                    x = !!rlang::sym(prepared_data$group),
                    y = !!rlang::sym(prepared_data$dep),
                    type = prepared_data$distribution
                )
                return(plot)
            },

            # Plot function for bar charts (factor vs factor)
            .plotBarStats = function(prepared_data) {
                # Validate data and return NULL if validation fails
                if (!private$.validatePlotData(prepared_data, "bar chart")) {
                    return(NULL)
                }

                # Checkpoint before expensive plot generation
                private$.checkpoint()

                plot <- ggstatsplot::ggbarstats(
                    data = prepared_data$data,
                    x = !!rlang::sym(prepared_data$dep),
                    y = !!rlang::sym(prepared_data$group)
                )
                return(plot)
            },

            # Plot function for dot plots (continuous vs factor)
            .plotDotplotStats = function(prepared_data) {
                # For ggdotplotstats: x = continuous, y = factor
                # The combination is "independent_continuous_factor" meaning:
                # group is continuous, dep is factor

                # Validate data and return NULL if validation fails
                if (!private$.validatePlotData(prepared_data, "dot plot")) {
                    return(NULL)
                }

                # Checkpoint before expensive plot generation
                private$.checkpoint()

                x_var <- prepared_data$group  # continuous variable
                y_var <- prepared_data$dep    # factor variable

                plot <- ggstatsplot::ggdotplotstats(
                    data = prepared_data$data,
                    x = !!rlang::sym(x_var),  # continuous variable
                    y = !!rlang::sym(y_var)   # factor variable
                )
                return(plot)
            },

            # Plot function for within-subjects comparisons (repeated measures)
            .plotWithinStats = function(prepared_data) {
                # Validate data and return NULL if validation fails
                if (!private$.validatePlotData(prepared_data, "within-subjects violin plot")) {
                    return(NULL)
                }

                # Checkpoint before expensive plot generation
                private$.checkpoint()

                plot <- ggstatsplot::ggwithinstats(
                    data = prepared_data$data,
                    x = !!rlang::sym(prepared_data$group),
                    y = !!rlang::sym(prepared_data$dep),
                    type = prepared_data$distribution,
                    pairwise.comparisons = TRUE
                )
                return(plot)
            },

            # Plot function for alluvial diagrams (factor vs factor, repeated)
            .plotAlluvial = function(prepared_data) {
                # Validate data and return NULL if validation fails
                if (!private$.validatePlotData(prepared_data, "alluvial diagram")) {
                    return(NULL)
                }
                
                if (prepared_data$alluvsty == "t1") {
                    # Use ggalluvial
                    plot <- private$.plotAlluvialGG(prepared_data)
                } else {
                    # Use easyalluvial
                    plot <- private$.plotAlluvialEasy(prepared_data)
                }
                return(plot)
            },
            
            # ggalluvial implementation
            .plotAlluvialGG = function(prepared_data) {
                # Enhanced package validation
                if (!requireNamespace("ggalluvial", quietly = TRUE)) {
                    stop("Package 'ggalluvial' is required for alluvial plots but is not installed. Install with: install.packages('ggalluvial')")
                }
                if (!requireNamespace("dplyr", quietly = TRUE)) {
                    stop("Package 'dplyr' is required for data manipulation but is not installed. Install with: install.packages('dplyr')")
                }
                
                # Create plot data
                plotData <- data.frame(
                    gr = prepared_data$data[[prepared_data$group]],
                    dp = prepared_data$data[[prepared_data$dep]]
                )
                
                # Checkpoint before expensive data aggregation
                private$.checkpoint(flush = FALSE)
                
                # Tally the combinations
                mydata_changes <- plotData %>%
                    dplyr::group_by(gr, dp) %>%
                    dplyr::tally()
                
                # Create alluvial plot
                stratum <- ggalluvial::StatStratum
                
                plot <- ggplot2::ggplot(
                    data = mydata_changes,
                    ggplot2::aes(axis1 = gr, axis2 = dp, y = n)
                ) +
                    ggplot2::scale_x_discrete(
                        limits = c(prepared_data$group, prepared_data$dep),
                        expand = c(.1, .05)
                    ) +
                    ggplot2::xlab(prepared_data$group) +
                    ggalluvial::geom_alluvium(ggplot2::aes(fill = gr, colour = gr)) +
                    ggalluvial::geom_stratum() +
                    ggalluvial::stat_stratum(geom = "stratum") +
                    ggplot2::geom_label(stat = stratum, infer.label = TRUE) +
                    ggplot2::theme_minimal()
                
                return(plot)
            },
            
            # easyalluvial implementation
            .plotAlluvialEasy = function(prepared_data) {
                # Enhanced package validation
                if (!requireNamespace("easyalluvial", quietly = TRUE)) {
                    stop("Package 'easyalluvial' is required for simplified alluvial plots but is not installed. Install with: install.packages('easyalluvial')")
                }
                
                plot <- easyalluvial::alluvial_wide(
                    data = prepared_data$data,
                    max_variables = 5,
                    fill_by = 'first_variable'
                )
                return(plot)
            },
            
            # Grouped plots for when grvar is specified
            .plotGrouped = function(analysis_info, prepared_data) {
                # Check if native grouped function exists for this plot type
                grouped_func_available <- FALSE
                
                # Handle specific grouped plot types with native support
                if (analysis_info$plot_type == "independent_factor_continuous") {
                    # Use grouped_ggbetweenstats for factor vs continuous
                    plot <- ggstatsplot::grouped_ggbetweenstats(
                        data = prepared_data$data,
                        x = !!rlang::sym(prepared_data$group),
                        y = !!rlang::sym(prepared_data$dep),
                        grouping.var = !!rlang::sym(prepared_data$grvar),
                        pairwise.comparisons = TRUE,
                        p.adjust.method = "bonferroni"
                    )
                    grouped_func_available <- TRUE
                } else if (analysis_info$plot_type == "independent_continuous_continuous") {
                    # Try grouped_ggscatterstats if available
                    tryCatch({
                        plot <- ggstatsplot::grouped_ggscatterstats(
                            data = prepared_data$data,
                            x = !!rlang::sym(prepared_data$group),
                            y = !!rlang::sym(prepared_data$dep),
                            grouping.var = !!rlang::sym(prepared_data$grvar)
                        )
                        grouped_func_available <- TRUE
                    }, error = function(e) {
                        grouped_func_available <- FALSE
                    })
                }
                
                # If no native grouped function, create multiple plots
                if (!grouped_func_available) {
                    # Get unique levels of grouping variable with enhanced validation
                    grvar_col <- prepared_data$data[[prepared_data$grvar]]
                    grvar_levels <- unique(grvar_col)
                    
                    # Check for empty levels after filtering
                    empty_levels <- sapply(grvar_levels, function(level) {
                        sum(grvar_col == level, na.rm = TRUE) == 0
                    })
                    if (any(empty_levels)) {
                        warning(glue::glue("Some levels of '{prepared_data$grvar}' have no data: {paste(grvar_levels[empty_levels], collapse=', ')}. These levels will be skipped."))
                        grvar_levels <- grvar_levels[!empty_levels]
                    }
                    
                    # Create a list to store individual plots
                    plot_list <- list()
                    
                    # Generate plot for each level of grouping variable
                    for (i in seq_along(grvar_levels)) {
                        # Checkpoint before expensive grouped plot generation
                        private$.checkpoint(flush = FALSE)
                        level <- grvar_levels[i]
                        
                        # Filter data for this level
                        level_data <- prepared_data
                        level_data$data <- prepared_data$data[grvar_col == level, ]
                        level_data$grvar <- NULL  # Remove grvar to avoid recursion
                        
                        # Generate plot for this subset with error handling
                        subplot <- tryCatch({
                            switch(analysis_info$plot_type,
                                "independent_continuous_continuous" = private$.plotScatterStats(level_data),
                                "independent_factor_factor" = private$.plotBarStats(level_data),
                                "independent_continuous_factor" = private$.plotDotplotStats(level_data),
                                "repeated_factor_continuous" = private$.plotWithinStats(level_data),
                                "repeated_factor_factor" = private$.plotAlluvial(level_data),
                                # Default: factor vs continuous
                                private$.plotBetweenStats(level_data)
                            )
                        }, error = function(e) {
                            # If specialized function fails, use fallback for this subset
                            message("Specialized grouped plot failed for level ", level, ": ", conditionMessage(e))
                            private$.plotFallback(level_data, analysis_info)
                        })
                        
                        # Add title to identify the group
                        if (!is.null(subplot)) {
                            subplot <- subplot + 
                                ggplot2::ggtitle(paste(prepared_data$grvar, "=", level))
                            plot_list[[i]] <- subplot
                        }
                    }
                    
                    # Combine plots using patchwork if available, otherwise return first plot
                    if (length(plot_list) > 0) {
                        if (requireNamespace("patchwork", quietly = TRUE)) {
                            # Use patchwork to combine plots
                            plot <- patchwork::wrap_plots(plot_list, ncol = 2)
                        } else if (requireNamespace("cowplot", quietly = TRUE)) {
                            # Use cowplot as fallback
                            plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 2)
                        } else {
                            # Return just the first plot with a message
                            plot <- plot_list[[1]]
                            message("Install 'patchwork' or 'cowplot' package to see all grouped plots")
                        }
                    } else {
                        plot <- NULL
                    }
                }
                
                return(plot)
            },

            .plot = function(image, ggtheme, theme, ...) {
                # the plot function ----
                
                # Get analysis type information
                analysis_info <- private$.detectAnalysisType()
                
                # Return early if no variables selected
                if (is.null(analysis_info)) {
                    return()
                }

                # Enhanced data validation with context
                if (nrow(self$data) == 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'emptyDatasetPlot',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(glue::glue(
                        "No data available for plotting.\n",
                        "• Variables: '{analysis_info$dep_var}' vs '{analysis_info$group_var}'\n",
                        "• Verify data is loaded and variables exist\n",
                        "• Check that dataset contains observations"
                    ))
                    self$results$insert(1, notice)
                    return()
                }
                
                # Check if plot type is supported
                if (analysis_info$plot_type %in% c("repeated_continuous_continuous", "repeated_continuous_factor")) {
                    # These combinations are not supported
                    return()
                }
                
                # Checkpoint before data preparation
                private$.checkpoint()
                
                # Prepare data for plotting
                prepared_data <- private$.prepareDataForPlot(analysis_info)
                
                # Adjust plot size if grouping variable is used
                if (!is.null(prepared_data$grvar)) {
                    # Get number of levels in grouping variable
                    grvar_col <- prepared_data$data[[prepared_data$grvar]]
                    num_levels <- length(unique(grvar_col))
                    
                    # Check if this plot type uses native grouped functions
                    uses_native_grouped <- (analysis_info$plot_type == "independent_factor_continuous") ||
                                         (analysis_info$plot_type == "independent_continuous_continuous" && 
                                          exists("grouped_ggscatterstats", where = asNamespace("ggstatsplot")))
                    
                    # Set dynamic width based on number of groups using constants
                    dims <- private$.PLOT_DIMENSIONS
                    if (uses_native_grouped) {
                        # Native grouped functions typically arrange plots automatically
                        # Use moderate width increase
                        new_width <- max(dims$default$width, min(num_levels * dims$grouped_native$width_per_level, dims$grouped_native$max_width))
                        new_height <- max(dims$default$height, min(num_levels * dims$grouped_native$height_per_level, dims$grouped_native$max_height))
                    } else {
                        # Manual grouped plots (using patchwork/cowplot) need more space
                        # Calculate width for 2-column layout
                        rows <- ceiling(num_levels / 2)
                        new_width <- dims$grouped_manual$width  # Fixed wider width
                        new_height <- max(dims$default$height, min(rows * dims$grouped_manual$height_per_row, dims$grouped_manual$max_height))  # Height based on rows
                    }
                    
                    # Apply the new size
                    image$setSize(new_width, new_height)
                } else {
                    # Default size for non-grouped plots
                    dims <- private$.PLOT_DIMENSIONS
                    image$setSize(dims$default$width, dims$default$height)
                }
                
                # Checkpoint before main plot generation
                private$.checkpoint()
                
                # Generate the plot
                plot <- private$.generatePlot(analysis_info, prepared_data)
                
                # Return the plot
                if (!is.null(plot)) {
                    print(plot)
                    return(TRUE)
                } else {
                    return()
                }
            }

        )
    )
