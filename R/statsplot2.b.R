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
            
            # Method to invalidate cache when options change
            .invalidateCache = function() {
                private$.cached_analysis <- NULL
            },

            # Validate that design (independent/repeated) matches data structure
            .validateDesignDataMatch = function(analysis_info, data) {
                # Only validate for repeated measures
                if (analysis_info$direction != "repeated") {
                    return(list(valid = TRUE, warnings = character(0)))
                }

                warnings_html <- character(0)
                dep_var <- analysis_info$dep_var
                group_var <- analysis_info$group_var

                # For repeated measures with factor grouping variable
                if (analysis_info$group_type == "factor") {
                    group_levels <- levels(data[[group_var]])
                    if (is.null(group_levels)) {
                        group_levels <- unique(data[[group_var]])
                    }
                    n_levels <- length(group_levels)

                    # Check if number of observations is divisible by number of groups
                    n_obs <- nrow(data)
                    if (n_obs %% n_levels != 0) {
                        warning_msg <- glue::glue(
                            "<div style='background: #fff3e0; border-left: 4px solid #f57c00; padding: 12px; margin: 10px 0;'>",
                            "<strong>Design-Data Mismatch Warning</strong><br/>",
                            "You selected <strong>Repeated Measures</strong> design, but the data structure may not match:<br/>",
                            "• Total observations: {n_obs}<br/>",
                            "• Groups in '{group_var}': {n_levels} ({paste(group_levels, collapse=', ')})<br/>",
                            "• Expected for balanced repeated measures: {n_obs} should be divisible by {n_levels}<br/><br/>",
                            "<strong>Possible issues:</strong><br/>",
                            "1. This might be <em>independent groups</em> data, not repeated measures<br/>",
                            "2. Missing observations for some subjects<br/>",
                            "3. Data needs restructuring (wide format to long format)<br/><br/>",
                            "<strong>Action:</strong> Verify your study design matches the data structure.",
                            "</div>"
                        )
                        warnings_html <- c(warnings_html, warning_msg)
                    }

                    # Check for suspiciously even distribution (suggests independent groups)
                    group_counts <- table(data[[group_var]])
                    if (length(unique(group_counts)) == 1 && n_levels >= 2) {
                        # Perfectly balanced groups often indicate independent samples
                        obs_per_group <- unique(group_counts)[1]
                        warning_msg <- glue::glue(
                            "<div style='background: #e3f2fd; border-left: 4px solid #2196f3; padding: 12px; margin: 10px 0;'>",
                            "<strong>Design Check</strong><br/>",
                            "Perfectly balanced groups detected ({obs_per_group} observations per group).<br/>",
                            "• This pattern is common in <strong>independent groups</strong> designs<br/>",
                            "• For repeated measures, each <em>subject</em> should appear in multiple groups<br/>",
                            "• Verify: Does each row represent a different subject, or the same subject at different times?",
                            "</div>"
                        )
                        warnings_html <- c(warnings_html, warning_msg)
                    }
                }

                return(list(valid = TRUE, warnings = warnings_html))
            },

            # Standardized validation method for plot data
            .validatePlotData = function(prepared_data, plot_type) {
                data <- prepared_data$data
                x_var <- prepared_data$group
                y_var <- prepared_data$dep

                # Basic data validation
                if (nrow(data) < 2) {
                # notice <- jmvcore::Notice$new(
                #     options = self$options,
                #     name = 'insufficientData',
                #     type = jmvcore::NoticeType$ERROR
                # )
                # notice$setContent(glue::glue(
                #     "Insufficient data for {plot_type}.\n",
                #     "• Variables: {y_var} by {x_var}\n",
                #     "• Found: {nrow(data)} observation(s)\n",
                #     "• Required: ≥2 observations\n",
                #     "• Check your data filtering."
                # ))
                # self$results$insert(999, notice)
                    return(FALSE)
                }

                # Variable-specific validation based on expected types
                y_data_clean <- data[[y_var]][!is.na(data[[y_var]])]
                x_data_clean <- data[[x_var]][!is.na(data[[x_var]])]

                # Check for sufficient non-missing values
                if (length(y_data_clean) < 2) {
                    # notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = 'insufficientDepValues',
                    #     type = jmvcore::NoticeType$ERROR
                    # )
                    # notice$setContent(glue::glue(
                    #     "Dependent variable '{y_var}' has insufficient non-missing values for {plot_type}.\n",
                    #     "• Found: {length(y_data_clean)} valid value(s)\n",
                    #     "• Required: ≥2 valid values\n",
                    #     "• Check for missing data in '{y_var}'."
                    # ))
                    # self$results$insert(999, notice)
                    return(FALSE)
                }

                if (length(x_data_clean) < 2) {
                    # notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = 'insufficientGroupValues',
                    #     type = jmvcore::NoticeType$ERROR
                    # )
                    # notice$setContent(glue::glue(
                    #     "Grouping variable '{x_var}' has insufficient non-missing values for {plot_type}.\n",
                    #     "• Found: {length(x_data_clean)} valid value(s)\n",
                    #     "• Required: ≥2 valid values\n",
                    #     "• Check for missing data in '{x_var}'."
                    # ))
                    # self$results$insert(999, notice)
                    return(FALSE)
                }

                # Factor-specific validation
                if (is.factor(data[[y_var]])) {
                    y_levels <- length(unique(y_data_clean))
                    if (y_levels < 1) {
                        # notice <- jmvcore::Notice$new(
                        #     options = self$options,
                        #     name = 'noValidDepLevels',
                        #     type = jmvcore::NoticeType$ERROR
                        # )
                        # notice$setContent(glue::glue(
                        #     "Factor variable '{y_var}' has no valid levels for {plot_type}.\n",
                        #     "• All values are missing after data cleaning\n",
                        #     "• Check data for: {paste(unique(data[[y_var]]), collapse=', ')}"
                        # ))
                        # self$results$insert(999, notice)
                        return(FALSE)
                    }
                }

                if (is.factor(data[[x_var]])) {
                    x_levels <- length(unique(x_data_clean))
                    if (x_levels < 1) {
                        # notice <- jmvcore::Notice$new(
                        #     options = self$options,
                        #     name = 'noValidGroupLevels',
                        #     type = jmvcore::NoticeType$ERROR
                        # )
                        # notice$setContent(glue::glue(
                        #     "Factor variable '{x_var}' has no valid levels for {plot_type}.\n",
                        #     "• All values are missing after data cleaning\n",
                        #     "• Check data for: {paste(unique(data[[x_var]]), collapse=', ')}"
                        # ))
                        # self$results$insert(999, notice)
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

                # Helper: treat integer-coded small-cardinality variables as categorical
                .infer_type <- function(v) {
                    contin <- c("integer", "numeric", "double")

                    if (inherits(v, "factor") || inherits(v, "ordered")) return("factor")

                    # Handle character variables (common in clinical data)
                    if (is.character(v)) {
                        unique_vals <- length(unique(v[!is.na(v)]))
                        # Character columns with ≤20 unique values treated as categorical
                        if (unique_vals > 0 && unique_vals <= 20) return("factor")
                        return("unknown")
                    }

                    # Handle numeric variables with enhanced threshold
                    if (inherits(v, contin)) {
                        unique_vals <- length(unique(v[!is.na(v)]))
                        # Increased threshold to 15 for clinical scales (11-15 point scales)
                        if (unique_vals > 0 && unique_vals <= 15 &&
                            all(abs(v[!is.na(v)] - round(v[!is.na(v)])) < .Machine$double.eps^0.5)) {
                            # Warning for borderline cases
                            if (unique_vals > 10) {
                                warning(sprintf(
                                    "Variable has %d unique integer values (borderline categorical/continuous). Treating as categorical. To force continuous, convert to numeric with decimals.",
                                    unique_vals
                                ))
                            }
                            return("factor")
                        }
                        return("continuous")
                    }

                    return("unknown")
                }
                
                dep_type <- .infer_type(mydep)
                group_type <- .infer_type(mygroup)
                
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
                dep_var_safe <- private$.safeHtmlOutput(analysis_info$dep_var)
                group_var_safe <- private$.safeHtmlOutput(analysis_info$group_var)
                dep_type_safe <- private$.safeHtmlOutput(analysis_info$dep_type)
                group_type_safe <- private$.safeHtmlOutput(analysis_info$group_type)

                # Generate HTML explanation message based on plot type
                html <- "<div style='background: #f8f9fa; border-left: 4px solid #2196f3; padding: 15px; margin: 10px 0; border-radius: 4px;'>"
                html <- paste0(html, "<h4 style='margin-top: 0; color: #1976d2;'>Plot Selection Summary</h4>")

                # Main message based on plot type
                base_message <- switch(analysis_info$plot_type,
                    "independent_factor_continuous" = glue::glue(
                        "<p><strong>Violin plot</strong> will be used to compare <code>{dep_var_safe}</code> <em>({dep_type_safe})</em> between independent groups defined by <code>{group_var_safe}</code> <em>({group_type_safe})</em>.</p>"
                    ),
                    "independent_continuous_continuous" = glue::glue(
                        "<p><strong>Scatter plot</strong> will examine the relationship between <code>{group_var_safe}</code> <em>({group_type_safe})</em> and <code>{dep_var_safe}</code> <em>({dep_type_safe})</em>.</p>"
                    ),
                    "independent_factor_factor" = glue::glue(
                        "<p><strong>Bar chart</strong> will compare <code>{dep_var_safe}</code> <em>({dep_type_safe})</em> across categories of <code>{group_var_safe}</code> <em>({group_type_safe})</em>.</p>"
                    ),
                    "independent_continuous_factor" = glue::glue(
                        "<p><strong>Dot plot</strong> will compare <code>{dep_var_safe}</code> <em>({dep_type_safe})</em> with <code>{group_var_safe}</code> <em>({group_type_safe})</em>.</p>",
                        "<p style='background: #fff3e0; padding: 8px; border-radius: 4px; color: #f57c00;'>",
                        "<strong>Tip:</strong> Consider switching variables for a more conventional visualization.</p>"
                    ),
                    "repeated_factor_continuous" = glue::glue(
                        "<p><strong>Paired violin plot</strong> will compare <code>{dep_var_safe}</code> <em>({dep_type_safe})</em> between repeated measurements defined by <code>{group_var_safe}</code> <em>({group_type_safe})</em>.</p>",
                        "<p style='font-size: 0.9em;'><strong>Advanced options:</strong> ",
                        "For more customization, use <code>jjstatsplot::jjwithinstats</code>. ",
                        "To verify results, check with <code>jmv::ttestPS</code> for paired samples t-test.</p>"
                    ),
                    "repeated_continuous_continuous" = glue::glue(
                        "<p><strong>Basic scatter plot</strong> will be generated for repeated measurements of <code>{group_var_safe}</code> and <code>{dep_var_safe}</code>.</p>",
                        "<div style='background: #fff3e0; padding: 10px; margin: 10px 0; border-radius: 4px;'>",
                        "<strong>Limited Support:</strong> Specialized functions don't support this combination. Consider:<br>",
                        "<ul style='margin: 5px 0; padding-left: 20px;'>",
                        "<li>Using 'independent' design instead</li>",
                        "<li>Creating difference scores</li>",
                        "<li>Using correlation analysis</li>",
                        "</ul></div>"
                    ),
                    "repeated_factor_factor" = glue::glue(
                        "<p><strong>Alluvial diagram</strong> will show repeated measurements of <code>{dep_var_safe}</code> <em>({dep_type_safe})</em> and <code>{group_var_safe}</code> <em>({group_type_safe})</em>.</p>"
                    ),
                    "repeated_continuous_factor" = glue::glue(
                        "<p><strong>Basic visualization</strong> will be generated for <code>{dep_var_safe}</code> vs <code>{group_var_safe}</code> in repeated measures.</p>",
                        "<div style='background: #fff3e0; padding: 10px; margin: 10px 0; border-radius: 4px;'>",
                        "<strong>Limited Support:</strong> Consider alternatives:<br>",
                        "<ul style='margin: 5px 0; padding-left: 20px;'>",
                        "<li>Switching variables (<code>{group_var_safe}</code> as dependent)</li>",
                        "<li>Using 'independent' design</li>",
                        "<li>Creating summary scores</li>",
                        "</ul></div>"
                    ),
                    # Default case
                    glue::glue(
                        "<p><strong>Basic ggplot2 visualization</strong> will be used for <code>{dep_var_safe}</code> vs <code>{group_var_safe}</code> with {analysis_info$direction} design.</p>",
                        "<p style='color: #757575;'><em>Specialized statistical plots are not available for this combination.</em></p>"
                    )
                )

                html <- paste0(html, base_message)

                # Add notes about option applicability
                notes_html <- ""

                # Note about statistical approach
                if (analysis_info$dep_type == "factor" && analysis_info$group_type == "factor") {
                    notes_html <- paste0(notes_html,
                        "<p style='background: #e3f2fd; padding: 8px; border-radius: 4px; font-size: 0.9em;'>",
                        "<strong>Note:</strong> Statistical approach option does not apply to categorical comparisons.</p>"
                    )
                }

                # Note about alluvial style
                if (analysis_info$plot_type == "repeated_factor_factor") {
                    notes_html <- paste0(notes_html,
                        "<p style='background: #e8f5e9; padding: 8px; border-radius: 4px; font-size: 0.9em;'>",
                        "<strong>Alluvial style option is available</strong> for this repeated categorical comparison.</p>"
                    )
                } else if (analysis_info$direction == "repeated") {
                    notes_html <- paste0(notes_html,
                        "<p style='background: #e3f2fd; padding: 8px; border-radius: 4px; font-size: 0.9em;'>",
                        "<strong>Note:</strong> Alluvial style option only applies to repeated factor vs factor comparisons.</p>"
                    )
                }

                # Plot type information
                plot_type_safe <- private$.safeHtmlOutput(analysis_info$plot_type)
                notes_html <- paste0(notes_html,
                    "<p style='color: #616161; font-size: 0.85em; margin-top: 10px;'>",
                    "<strong>Technical details:</strong> Plot type: <code>", plot_type_safe, "</code> | ",
                    "Variables: <code>", dep_var_safe, "</code> (", dep_type_safe, ") vs ",
                    "<code>", group_var_safe, "</code> (", group_type_safe, ")</p>"
                )

                html <- paste0(html, notes_html, "</div>")

                return(html)
            },
            
            # Generate clinical interpretation for results
            .generateClinicalInterpretation = function(analysis_info) {
                dep_var_safe <- private$.safeHtmlOutput(analysis_info$dep_var)
                group_var_safe <- private$.safeHtmlOutput(analysis_info$group_var)

                # Start HTML container
                html <- "<div style='background: #f1f8e9; border-left: 4px solid #689f38; padding: 15px; margin: 10px 0; border-radius: 4px;'>"
                html <- paste0(html, "<h4 style='margin-top: 0; color: #558b2f;'>Clinical Interpretation</h4>")

                # Main interpretation based on plot type
                interpretation <- switch(analysis_info$plot_type,
                    "independent_factor_continuous" = glue::glue(
                        "<p>This <strong>violin plot</strong> compares the distribution of <code>{dep_var_safe}</code> between different <code>{group_var_safe}</code> groups.</p>",
                        "<ul style='margin: 10px 0; padding-left: 20px;'>",
                        "<li><strong>Medians:</strong> Center lines show typical values in each group</li>",
                        "<li><strong>Spread:</strong> Wider violins indicate more variability</li>",
                        "<li><strong>Distribution shape:</strong> Violin width reveals data density at different values</li>",
                        "<li><strong>Statistical testing:</strong> Included when applicable to assess group differences</li>",
                        "</ul>"
                    ),
                    "independent_continuous_continuous" = glue::glue(
                        "<p>This <strong>scatter plot</strong> examines the linear relationship between <code>{group_var_safe}</code> and <code>{dep_var_safe}</code>.</p>",
                        "<ul style='margin: 10px 0; padding-left: 20px;'>",
                        "<li><strong>Trend line:</strong> Shows the association direction and strength</li>",
                        "<li><strong>Confidence bands:</strong> Gray area indicates uncertainty in the trend</li>",
                        "<li><strong>Positive slope:</strong> Higher {group_var_safe} values → higher {dep_var_safe} values</li>",
                        "<li><strong>Correlation:</strong> Tighter scatter around line indicates stronger association</li>",
                        "</ul>"
                    ),
                    "independent_factor_factor" = glue::glue(
                        "<p>This <strong>bar chart</strong> compares the frequency distribution of <code>{dep_var_safe}</code> categories across <code>{group_var_safe}</code> groups.</p>",
                        "<ul style='margin: 10px 0; padding-left: 20px;'>",
                        "<li><strong>Bar height:</strong> Represents count or proportion in each category</li>",
                        "<li><strong>Height differences:</strong> Indicate varying proportions between groups</li>",
                        "<li><strong>Chi-square test:</strong> Tests for independence between variables</li>",
                        "<li><strong>Patterns:</strong> Look for systematic differences across groups</li>",
                        "</ul>"
                    ),
                    "repeated_factor_continuous" = glue::glue(
                        "<p>This <strong>paired violin plot</strong> compares <code>{dep_var_safe}</code> between two time points or conditions (<code>{group_var_safe}</code>).</p>",
                        "<ul style='margin: 10px 0; padding-left: 20px;'>",
                        "<li><strong>Connected points:</strong> Show individual subject changes over time</li>",
                        "<li><strong>Distribution shift:</strong> Reveals overall treatment/time effect</li>",
                        "<li><strong>Statistical test:</strong> Evaluates if mean change differs significantly from zero</li>",
                        "<li><strong>Individual variability:</strong> Lines show subject-specific responses</li>",
                        "</ul>"
                    ),
                    "repeated_factor_factor" = glue::glue(
                        "<p>This <strong>alluvial diagram</strong> shows how subjects transition between <code>{dep_var_safe}</code> categories from <code>{group_var_safe}</code>.</p>",
                        "<ul style='margin: 10px 0; padding-left: 20px;'>",
                        "<li><strong>Flow thickness:</strong> Represents the number of subjects in each transition</li>",
                        "<li><strong>Pathways:</strong> Show movement between categories over time</li>",
                        "<li><strong>Applications:</strong> Tracking disease stages, treatment responses, or classifications</li>",
                        "<li><strong>Stability:</strong> Straight flows = stable categories; crossed flows = changes</li>",
                        "</ul>"
                    ),
                    "independent_continuous_factor" = glue::glue(
                        "<p>This <strong>dot plot</strong> shows the distribution of <code>{group_var_safe}</code> values within each <code>{dep_var_safe}</code> category.</p>",
                        "<ul style='margin: 10px 0; padding-left: 20px;'>",
                        "<li><strong>Individual dots:</strong> Each represents a single observation</li>",
                        "<li><strong>Central tendency:</strong> Compare typical values between categories</li>",
                        "<li><strong>Spread:</strong> Horizontal spread shows variability within categories</li>",
                        "<li><strong>Outliers:</strong> Isolated dots may represent unusual cases</li>",
                        "</ul>"
                    ),
                    # Default for unsupported combinations
                    glue::glue(
                        "<p>This <strong>basic plot</strong> shows the relationship between <code>{dep_var_safe}</code> and <code>{group_var_safe}</code>.</p>",
                        "<p style='color: #616161;'><em>While specialized statistical tests aren't available for this combination, ",
                        "the visualization can still provide valuable insights about patterns in your data.</em></p>"
                    )
                )

                html <- paste0(html, interpretation)

                # Add assumption notes based on statistical approach
                if (analysis_info$dep_type == "continuous" || analysis_info$group_type == "continuous") {
                    assumption_html <- "<div style='background: #ffffff; padding: 10px; margin-top: 10px; border-radius: 4px; border: 1px solid #ddd;'>"
                    assumption_html <- paste0(assumption_html, "<h5 style='margin-top: 0; color: #424242;'>Statistical Approach</h5>")

                    if (analysis_info$distribution == "p") {
                        assumption_html <- paste0(assumption_html,
                            "<p style='margin: 5px 0;'><strong>Parametric:</strong> Assumes normally distributed data. ",
                            "Best for continuous variables with bell-shaped distributions.</p>",
                            "<p style='font-size: 0.85em; color: #757575;'>",
                            "<strong>When to use:</strong> Data appears symmetric, no extreme outliers, n≥30 per group</p>"
                        )
                    } else if (analysis_info$distribution == "np") {
                        assumption_html <- paste0(assumption_html,
                            "<p style='margin: 5px 0;'><strong>Nonparametric:</strong> Distribution-free method. ",
                            "Suitable for skewed data, ordinal scales, or when normality assumptions are violated.</p>",
                            "<p style='font-size: 0.85em; color: #757575;'>",
                            "<strong>When to use:</strong> Skewed data, outliers present, ordinal scales, small samples</p>"
                        )
                    } else if (analysis_info$distribution == "r") {
                        assumption_html <- paste0(assumption_html,
                            "<p style='margin: 5px 0;'><strong>Robust:</strong> Less sensitive to outliers. ",
                            "Good choice when data contains extreme values that might affect standard tests.</p>",
                            "<p style='font-size: 0.85em; color: #757575;'>",
                            "<strong>When to use:</strong> Outliers present but meaningful, heavy-tailed distributions</p>"
                        )
                    } else if (analysis_info$distribution == "bf") {
                        assumption_html <- paste0(assumption_html,
                            "<p style='margin: 5px 0;'><strong>Bayesian:</strong> Provides evidence for or against the null hypothesis.</p>",
                            "<ul style='margin: 5px 0; padding-left: 20px; font-size: 0.9em;'>",
                            "<li>BF > 3: Moderate evidence</li>",
                            "<li>BF > 10: Strong evidence</li>",
                            "<li>BF > 30: Very strong evidence</li>",
                            "</ul>",
                            "<p style='font-size: 0.85em; color: #757575;'>",
                            "<strong>When to use:</strong> Want to quantify evidence, need to support null hypothesis</p>"
                        )
                    }

                    assumption_html <- paste0(assumption_html, "</div>")
                    html <- paste0(html, assumption_html)
                }

                html <- paste0(html, "</div>")

                return(html)
            },
            
            # Check statistical assumptions and provide warnings
            .checkAssumptions = function(analysis_info, data) {
                notices <- list()

                dep_data <- data[[analysis_info$dep_var]]
                group_data <- data[[analysis_info$group_var]]

                # Sample size checks
                total_n <- sum(!is.na(dep_data) & !is.na(group_data))
                if (total_n < 30) {
                    # notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = 'smallSample',
                    #     type = jmvcore::NoticeType$STRONG_WARNING
                    # )
                    # notice$setContent(glue::glue(
                    #     "Small sample size detected (n={total_n}).\n",
                    #     "• Nonparametric approaches recommended for n<30\n",
                    #     "• Consider robust statistical methods\n",
                    #     "• Results may have reduced statistical power"
                    # ))
                    # notices <- append(notices, list(list(notice = notice, type = jmvcore::NoticeType$STRONG_WARNING)))
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
                            # notice <- jmvcore::Notice$new(
                            #     options = self$options,
                            #     name = 'extremeOutliers',
                            #     type = jmvcore::NoticeType$STRONG_WARNING
                            # )
                            # notice$setContent(glue::glue(
                            #     "Extreme outliers detected in {analysis_info$dep_var}.\n",
                            #     "• Found: {extreme_outliers} extreme outlier(s) (>3.5 IQR)\n",
                            #     "• Consider robust statistical approach (distribution='r')\n",
                            #     "• Outliers may unduly influence parametric results"
                            # ))
                            # notices <- append(notices, list(list(notice = notice, type = jmvcore::NoticeType$STRONG_WARNING)))
                        }
                    }

                    # Basic normality warning for small samples
                    if (total_n < 100) {
                        # notice <- jmvcore::Notice$new(
                        #     options = self$options,
                        #     name = 'normalityCheck',
                        #     type = jmvcore::NoticeType$WARNING
                        # )
                        # notice$setContent(glue::glue(
                        #     "Consider checking distribution visually (n={total_n}).\n",
                        #     "• For samples <100, normality assumptions are critical\n",
                        #     "• Consider nonparametric approach if data appears skewed\n",
                        #     "• Inspect violin plot shape for distributional form"
                        # ))
                        # notices <- append(notices, list(list(notice = notice, type = jmvcore::NoticeType$WARNING)))
                    }
                }

                # Group size balance check for between-subjects designs
                if (analysis_info$direction == "independent" && is.factor(group_data)) {
                    group_sizes <- table(group_data)
                    min_group <- min(group_sizes)
                    max_group <- max(group_sizes)
                    if (max_group / min_group > 4) {
                        # notice <- jmvcore::Notice$new(
                        #     options = self$options,
                        #     name = 'unbalancedGroups',
                        #     type = jmvcore::NoticeType$WARNING
                        # )
                        # notice$setContent(glue::glue(
                        #     "Unbalanced group sizes detected.\n",
                        #     "• Smallest group: {min_group}\n",
                        #     "• Largest group: {max_group}\n",
                        #     "• Ratio: {round(max_group/min_group, 1)}:1\n",
                        #     "• Results may be less reliable with imbalanced designs"
                        # ))
                        # notices <- append(notices, list(list(notice = notice, type = jmvcore::NoticeType$WARNING)))
                    }

                    # Very small group sizes
                    if (min_group < 5) {
                        # notice <- jmvcore::Notice$new(
                        #     options = self$options,
                        #     name = 'verySmallGroups',
                        #     type = jmvcore::NoticeType$STRONG_WARNING
                        # )
                        # notice$setContent(glue::glue(
                        #     "Very small group size(s) detected.\n",
                        #     "• Minimum group size: {min_group}\n",
                        #     "• Consider combining groups if scientifically appropriate\n",
                        #     "• Consider exact statistical methods for small samples\n",
                        #     "• Statistical power may be severely limited"
                        # ))
                        # notices <- append(notices, list(list(notice = notice, type = jmvcore::NoticeType$STRONG_WARNING)))
                    }
                }

                # Repeated measures specific checks
                if (analysis_info$direction == "repeated") {
                    # Check for complete pairs
                    if (is.factor(group_data) && length(levels(group_data)) == 2) {
                        complete_pairs <- sum(complete.cases(dep_data, group_data))
                        if (complete_pairs < total_n) {
                            # notice <- jmvcore::Notice$new(
                            #     options = self$options,
                            #     name = 'incompletePairs',
                            #     type = jmvcore::NoticeType$WARNING
                            # )
                            # notice$setContent(glue::glue(
                            #     "Missing paired observations detected.\n",
                            #     "• Complete pairs: {complete_pairs}\n",
                            #     "• Total observations: {total_n}\n",
                            #     "• Missing: {total_n - complete_pairs}\n",
                            #     "• Only complete pairs will be used in paired analysis"
                            # ))
                            # notices <- append(notices, list(list(notice = notice, type = jmvcore::NoticeType$WARNING)))
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
                "<div style='padding: 20px; background: #f5f5f5; border-radius: 8px;'>",
                "<h3 style='color: #1976d2; margin-top: 0;'>Welcome to Automatic Plot Selection</h3>",
                "<p style='font-size: 14px;'>This tool automatically selects the most appropriate statistical visualization based on your variable types.</p>",
                "<h4 style='color: #424242;'>Getting Started:</h4>",
                "<ol style='font-size: 13px;'>",
                "<li>Select a <strong>Dependent Variable</strong> (y-axis, outcome)</li>",
                "<li>Select a <strong>Grouping Variable</strong> (x-axis, comparison groups)</li>",
                "<li>Configure <strong>Study Design</strong> and <strong>Statistical Approach</strong></li>",
                "</ol>",
                "<p style='font-size: 12px; color: #757575;'><em>Powered by ggstatsplot and ggalluvial packages. Please cite jamovi and these packages.</em></p>",
                "</div>"
                    )

                    self$results$todo$setVisible(TRUE)
                    self$results$todo$setContent(todo)

                    return()

                }
                
                # Clear todo message
                self$results$todo$setVisible(FALSE)

                # Enhanced data validation with context
                if (nrow(self$data) == 0) {
                    dep_name <- self$options$dep %||% "not selected"
                    group_name <- self$options$group %||% "not selected"
                    error_html <- glue::glue(
                        "<div style='color: #d32f2f; padding: 15px; border-left: 4px solid #d32f2f; background: #ffebee;'>",
                        "<h4 style='margin-top: 0;'>No Data Available</h4>",
                        "<p><strong>Variables selected:</strong></p>",
                        "<ul style='margin: 5px 0;'>",
                        "<li>Dependent: <code>{dep_name}</code></li>",
                        "<li>Grouping: <code>{group_name}</code></li>",
                        "</ul>",
                        "<p style='margin-top: 10px;'><strong>Action:</strong> Check data loading and variable selection. Verify dataset is not empty.</p>",
                        "</div>"
                    )
                    self$results$ExplanationMessage$setContent(error_html)
                    return()
                }

                # Sample size validation with HTML warnings
                dep_data <- self$data[[analysis_info$dep_var]]
                group_data <- self$data[[analysis_info$group_var]]
                n_complete <- sum(complete.cases(dep_data, group_data))
                n_total <- nrow(self$data)

                validation_warnings <- ""

                # Critical: n < 10
                if (n_complete < 10) {
                    validation_warnings <- paste0(validation_warnings, glue::glue(
                        "<div style='background: #ffebee; border-left: 4px solid #d32f2f; padding: 12px; margin: 10px 0;'>",
                        "<strong>CRITICAL: Very Small Sample (n={n_complete})</strong><br/>",
                        "Your analysis has only <strong>{n_complete} complete observations</strong>.<br/><br/>",
                        "<strong>Statistical concerns:</strong><br/>",
                        "• Results are <em>highly unreliable</em> with n&lt;10<br/>",
                        "• Statistical tests may fail or produce meaningless p-values<br/>",
                        "• Effect sizes and confidence intervals will be very imprecise<br/>",
                        "• Normality assumptions cannot be verified<br/><br/>",
                        "<strong>Clinical recommendations:</strong><br/>",
                        "1. <strong>Collect more data</strong> before drawing conclusions<br/>",
                        "2. Use descriptive statistics only (no hypothesis testing)<br/>",
                        "3. Consider exact tests if n≥5 (Fisher's exact test)<br/>",
                        "4. Clearly report sample size limitations in any publication",
                        "</div>"
                    ))
                } else if (n_complete < 30) {
                    # Warning: 10 ≤ n < 30
                    validation_warnings <- paste0(validation_warnings, glue::glue(
                        "<div style='background: #fff3e0; border-left: 4px solid #f57c00; padding: 12px; margin: 10px 0;'>",
                        "<strong>Small Sample Warning (n={n_complete})</strong><br/>",
                        "You have <strong>{n_complete} complete observations</strong> (below the conventional n≥30 guideline).<br/><br/>",
                        "<strong>Recommendations:</strong><br/>",
                        "• Consider <strong>nonparametric</strong> statistical approach (less sensitive to small samples)<br/>",
                        "• Avoid parametric tests unless normality is well-established<br/>",
                        "• Use <strong>robust methods</strong> to reduce outlier influence<br/>",
                        "• Report confidence intervals (more informative than p-values alone)<br/>",
                        "• Consider exact tests when applicable<br/><br/>",
                        "<strong>Clinical note:</strong> Statistical power is limited. Negative findings may reflect insufficient sample size rather than true absence of effect.",
                        "</div>"
                    ))
                }

                # Design validation warnings
                design_validation <- private$.validateDesignDataMatch(analysis_info, self$data)
                if (length(design_validation$warnings) > 0) {
                    validation_warnings <- paste0(validation_warnings, paste(design_validation$warnings, collapse = "\n"))
                }

                # Check assumptions and insert notices
                assumption_notices <- private$.checkAssumptions(analysis_info, self$data)
                if (length(assumption_notices) > 0) {
                    # Insert assumption notices at top positions (after any ERROR notices)
                    # STRONG_WARNING notices first, then WARNING notices
                    strong_warnings <- Filter(function(n) identical(n$type, jmvcore::NoticeType$STRONG_WARNING), assumption_notices)
                    warnings <- Filter(function(n) identical(n$type, jmvcore::NoticeType$WARNING), assumption_notices)

                    position <- 1
                    for (item in strong_warnings) {
                        self$results$insert(position, item$notice)
                        position <- position + 1
                    }
                    for (item in warnings) {
                        self$results$insert(position, item$notice)
                        position <- position + 1
                    }
                }

                # Generate explanation message using the new function
                stat_exp <- private$.generateExplanationMessage(analysis_info)

                # Prepare data for plotting and counts
                prepared_data <- private$.prepareDataForPlot(analysis_info)

                # Generate clinical interpretation
                clinical_interpretation <- private$.generateClinicalInterpretation(analysis_info)

                # Combine HTML explanations
                combined_html <- "<div style='font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, sans-serif;'>"

                # Add validation warnings if any
                if (nchar(validation_warnings) > 0) {
                    combined_html <- paste0(combined_html, validation_warnings)
                }

                # Add main explanations
                combined_html <- paste0(combined_html, stat_exp, clinical_interpretation, "</div>")

                # Set the explanation message in results
                self$results$ExplanationMessage$setContent(combined_html)

                # Add success summary at the end
                # success <- jmvcore::Notice$new(
                #     options = self$options,
                #     name = 'analysisComplete',
                #     type = jmvcore::NoticeType$INFO
                # )
                # n_total <- nrow(self$data)
                # n_used <- nrow(prepared_data$data)
                
                # success$setContent(glue::glue(
                #     "Analysis completed successfully.\n",
                #     "• Plot type: {analysis_info$plot_type}\n",
                #     "• Observations used: {format(n_used, big.mark = ',')} of {format(n_total, big.mark = ',')}\n",
                #     "• Statistical approach: {analysis_info$distribution}\n",
                #     "• Study design: {analysis_info$direction}"
                # ))
                # self$results$insert(999, success)

            },
            
            .init = function() {
                # Centralized package dependency checking
                required_packages <- c("ggstatsplot", "ggalluvial", "dplyr", "easyalluvial", "patchwork", "cowplot")
                missing_packages <- required_packages[!sapply(required_packages, function(pkg) requireNamespace(pkg, quietly = TRUE))]
                if (length(missing_packages) > 0) {
                    install_cmd <- paste0("install.packages(c('", paste(missing_packages, collapse = "', '"), "'))")
                    warning_html <- glue::glue(
                        "<div style='color: #f57c00; padding: 15px; border-left: 4px solid #f57c00; background: #fff3e0;'>",
                        "<h4 style='margin-top: 0;'>Optional Packages Missing</h4>",
                        "<p><strong>Missing:</strong> {paste(missing_packages, collapse = ', ')}</p>",
                        "<p><strong>Install with:</strong></p>",
                        "<pre style='background: #f5f5f5; padding: 8px; border-radius: 4px; overflow-x: auto;'>{install_cmd}</pre>",
                        "<p style='margin-bottom: 0;'><em>Basic functionality will still work. Some plot types may use simpler visualizations.</em></p>",
                        "</div>"
                    )
                    self$results$ExplanationMessage$setContent(warning_html)
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
                    # Alluvial style not applicable
                    # notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = 'alluvialNotApplicable',
                    #     type = jmvcore::NoticeType$INFO
                    # )
                    # notice$setContent("Alluvial style option applies only to repeated factor vs factor data; it is ignored for this combination.")
                    # self$results$insert(999, notice)
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
                    before_n <- nrow(mydata)
                    mydata <- jmvcore::naOmit(mydata)
                    after_n <- nrow(mydata)
                } else {
                    before_n <- nrow(mydata)
                    after_n <- before_n
                }
                
                # Handle large dataset sampling if requested
                original_nrow <- nrow(mydata)
                if (self$options$sampleLarge && original_nrow > 10000) {
                    set.seed(42)  # For reproducible sampling
                    sample_size <- 5000
                    mydata <- mydata[sample(nrow(mydata), sample_size), ]
                    message(glue::glue("Large dataset detected ({format(original_nrow, big.mark = ',')} rows). Sampled {format(sample_size, big.mark = ',')} rows for visualization performance. Disable 'Sample Large Datasets' option to use full dataset."))
                    sampled_flag <- TRUE
                } else {
                    sampled_flag <- FALSE
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
                    alluvsty = analysis_info$alluvsty,
                    dropped = before_n - after_n,
                    sampled = sampled_flag
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
                    # notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = 'emptyDatasetPlot',
                    #     type = jmvcore::NoticeType$ERROR
                    # )
                    # notice$setContent(glue::glue(
                    #     "No data available for plotting.\n",
                    #     "• Variables: '{analysis_info$dep_var}' vs '{analysis_info$group_var}'\n",
                    #     "• Verify data is loaded and variables exist\n",
                    #     "• Check that dataset contains observations"
                    # ))
                    # self$results$insert(999, notice)
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
                n_total <- nrow(self$data)
                n_used <- nrow(prepared_data$data)
                # Notify if sampling applied
                if (!is.null(prepared_data$sampled) && prepared_data$sampled) {
                    # sample_notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = 'samplingApplied',
                    #     type = jmvcore::NoticeType$INFO
                    # )
                    # sample_notice$setContent(glue::glue(
                    #     "Large dataset detected; sampled 5,000 of {n_total} rows for plotting. Disable 'Sample Large Datasets' to use full data."
                    # ))
                    # self$results$insert(999, sample_notice)
                }
                # Notify if rows dropped due to NA exclusion
                if (!is.null(prepared_data$dropped) && prepared_data$dropped > 0) {
                    # drop_notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = 'naDropped',
                    #     type = jmvcore::NoticeType$INFO
                    # )
                    # drop_notice$setContent(glue::glue(
                    #     "{prepared_data$dropped} row(s) removed due to missing values."
                    # ))
                    # self$results$insert(999, drop_notice)
                }
                
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

        ), # End of private list
        public = list(
            #' @description
            #' Generate R source code for Statistical Plot analysis
            #' @return Character string with R syntax for reproducible analysis
            asSource = function() {
                dep <- self$options$dep

                if (is.null(dep))
                    return('')

                # Get arguments using base helper (if available)
                args <- ''
                if (!is.null(private$.asArgs)) {
                    args <- private$.asArgs(incData = FALSE)
                }
                if (args != '')
                    args <- paste0(',\n    ', args)

                # Get package name dynamically
                pkg_name <- utils::packageName()
                if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

                # Build complete function call
                paste0(pkg_name, '::statsplot2(\n    data = data', args, ')')
            }
        ) # End of public list
    )
