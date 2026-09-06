#' @title Automatic Plot Selection
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
#' @return An \code{R6} class generator object for the \code{statsplot2Class} backend; used internally by the jamovi analysis wrapper and not called directly.



statsplot2Class <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "statsplot2Class",
        inherit = statsplot2Base,
        private = list(
            # Cache for analysis results to avoid redundant calculations
            .cached_analysis = NULL,
            .cached_plot = NULL,

            # Plot dimension constants
            .PLOT_DIMENSIONS = list(
                default = list(width = 800, height = 600),
                grouped_native = list(width_per_level = 400, height_per_level = 300, max_width = 1600, max_height = 1200),
                grouped_manual = list(width = 1200, height_per_row = 450, max_height = 1400)
            ),

            # Notice collection helpers. A single Preformatted (plain-text) output item:
            # avoids BOTH the jmvcore::Notice serialization error from
            # self$results$insert(999, Notice) AND any HTML in notices (project convention:
            # notice content must be plain text). ====
            .noticeList = list(),

            .addNotice = function(type, title, content) {
                # De-duplicate: the same validation runs in both .run and the render path,
                # so skip appending an identical (type+title+content) notice to avoid showing
                # it twice within a single run cycle.
                content_chr <- as.character(content)
                for (existing in private$.noticeList) {
                    if (identical(existing$type, type) &&
                        identical(as.character(existing$title), as.character(title)) &&
                        identical(as.character(existing$content), content_chr)) {
                        return(invisible(NULL))
                    }
                }
                private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                    type = type,
                    title = title,
                    content = content
                )
                # Render immediately so early-return validation aborts still display the notice
                private$.renderNotices()
            },

            .renderNotices = function() {
                if (length(private$.noticeList) == 0) {
                    self$results$notices$setContent("")
                    return()
                }

                # Plain text only - notices avoid HTML by project convention; the Preformatted
                # output item renders this literally (no markup, no injection surface).
                blocks <- vapply(private$.noticeList, function(notice) {
                    prefix <- switch(notice$type,
                        ERROR          = "ERROR: ",
                        STRONG_WARNING = "WARNING: ",
                        WARNING        = "WARNING: ",
                        "")
                    paste0(prefix, notice$title, "\n", notice$content)
                }, character(1))

                self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
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
                return(text)
            },
            
            # Method to invalidate cache when options change
            .invalidateCache = function() {
                private$.cached_analysis <- NULL
                private$.cached_plot <- NULL
            },

            # Human-readable label for a detected plot type. Kept in sync with the labels
            # passed to .validatePlotData inside each .plot*Stats function so the ERROR
            # notice generated from .run matches the render path and de-duplicates cleanly.
            .plotTypeLabel = function(plot_type) {
                switch(plot_type,
                    "independent_factor_continuous"     = "violin plot",
                    "independent_continuous_continuous" = "scatter plot",
                    "independent_factor_factor"         = "bar chart",
                    "independent_continuous_factor"     = "horizontal group-comparison plot",
                    "repeated_factor_continuous"        = "within-subjects violin plot",
                    "repeated_factor_factor"            = "alluvial diagram",
                    plot_type
                )
            },

            # Optional packages that enable the full range of plot types. Returns the subset
            # that is not installed.
            .getMissingPackages = function() {
                required_packages <- c("ggstatsplot", "ggalluvial", "dplyr", "easyalluvial", "patchwork", "cowplot")
                required_packages[!vapply(required_packages, function(pkg) requireNamespace(pkg, quietly = TRUE), logical(1))]
            },

            # Standardized validation method for plot data
            .validatePlotData = function(prepared_data, plot_type) {
                data <- prepared_data$data
                x_var <- prepared_data$group
                y_var <- prepared_data$dep

                # Basic data validation
                if (nrow(data) < 2) {
                    private$.addNotice('ERROR', 'Insufficient Data', glue::glue(
                        "Insufficient data for {plot_type}.\n",
                        " - Variables: {y_var} by {x_var}\n",
                        " - Found: {nrow(data)} observation(s)\n",
                        " - Required: >=2 observations\n",
                        " - Check your data filtering."
                    ))
                    return(FALSE)
                }

                # Variable-specific validation based on expected types
                y_data_clean <- data[[y_var]][!is.na(data[[y_var]])]
                x_data_clean <- data[[x_var]][!is.na(data[[x_var]])]

                # Check for sufficient non-missing values
                if (length(y_data_clean) < 2) {
                    private$.addNotice('ERROR', 'Insufficient Dependent Values', glue::glue(
                        "Dependent variable '{y_var}' has insufficient non-missing values for {plot_type}.\n",
                        " - Found: {length(y_data_clean)} valid value(s)\n",
                        " - Required: >=2 valid values\n",
                        " - Check for missing data in '{y_var}'."
                    ))
                    return(FALSE)
                }

                if (length(x_data_clean) < 2) {
                    private$.addNotice('ERROR', 'Insufficient Grouping Values', glue::glue(
                        "Grouping variable '{x_var}' has insufficient non-missing values for {plot_type}.\n",
                        " - Found: {length(x_data_clean)} valid value(s)\n",
                        " - Required: >=2 valid values\n",
                        " - Check for missing data in '{x_var}'."
                    ))
                    return(FALSE)
                }

                # Factor-specific validation
                if (is.factor(data[[y_var]])) {
                    y_levels <- length(unique(y_data_clean))
                    if (y_levels < 1) {
                        private$.addNotice('ERROR', 'No Valid Dependent Levels', glue::glue(
                            "Factor variable '{y_var}' has no valid levels for {plot_type}.\n",
                            " - All values are missing after data cleaning\n",
                            " - Check data for: {paste(unique(data[[y_var]]), collapse=', ')}"
                        ))
                        return(FALSE)
                    }
                }

                if (is.factor(data[[x_var]])) {
                    x_levels <- length(unique(x_data_clean))
                    if (x_levels < 1) {
                        private$.addNotice('ERROR', 'No Valid Grouping Levels', glue::glue(
                            "Factor variable '{x_var}' has no valid levels for {plot_type}.\n",
                            " - All values are missing after data cleaning\n",
                            " - Check data for: {paste(unique(data[[x_var]]), collapse=', ')}"
                        ))
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
                .infer_type <- function(v, label) {
                    contin <- c("integer", "numeric", "double")

                    if (inherits(v, "factor") || inherits(v, "ordered")) return("factor")

                    # Handle character variables (common in clinical data)
                    if (is.character(v)) {
                        unique_vals <- length(unique(v[!is.na(v)]))
                        # Character columns with \u226420 unique values treated as categorical
                        if (unique_vals > 0 && unique_vals <= 20) return("factor")
                        return("unknown")
                    }

                    # Handle numeric variables
                    if (inherits(v, contin)) {
                        if (isTRUE(self$options$forceContinuous)) return("continuous")
                        unique_vals <- length(unique(v[!is.na(v)]))
                        # Up to 15 distinct whole numbers (scores, counts, grades) are read
                        # as categorical. That changes the plot AND the test, and a jamovi
                        # user has no way to add decimals, so every reclassification is
                        # reported and the option above is the escape hatch.
                        if (unique_vals > 0 && unique_vals <= 15 &&
                            all(abs(v[!is.na(v)] - round(v[!is.na(v)])) < .Machine$double.eps^0.5)) {
                            private$.addNotice('WARNING', 'Numeric variable analysed as categorical', glue::glue(
                                "'{label}' has {unique_vals} distinct whole-number values and is analysed as categorical, ",
                                "which changes the plot type and the test. ",
                                "Enable 'Treat all numeric variables as continuous' to compare it as a continuous measure."
                            ))
                            return("factor")
                        }
                        return("continuous")
                    }

                    return("unknown")
                }
                
                dep_type <- .infer_type(mydep, self$options$dep)
                group_type <- .infer_type(mygroup, self$options$group)
                
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
                
                # Add warnings for unexpected variable types - surfaced via the notices output
                # (base warning() is not reliably shown in the jamovi UI).
                if (dep_type == "unknown" || group_type == "unknown") {
                    private$.addNotice('WARNING', 'Unexpected Variable Types', glue::glue(
                        "Unexpected variable types detected.\n",
                        " - {analysis_info$dep_var}: {class(mydep)[1]}\n",
                        " - {analysis_info$group_var}: {class(mygroup)[1]}\n",
                        " - Analysis may not work as expected."
                    ))
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
                html <- "<div style='background-color: rgba(138, 155, 172, 0.06); border-left: 4px solid #2196f3; padding: 15px; margin: 10px 0; border-radius: 4px; color: inherit;'>"
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
                        "<p><strong>Horizontal group-comparison plot</strong> will compare <code>{dep_var_safe}</code> <em>({dep_type_safe})</em> with <code>{group_var_safe}</code> <em>({group_type_safe})</em>.</p>",
                        "<p style='background-color: rgba(255, 169, 33, 0.14); padding: 8px; border-radius: 4px; color: inherit;'>",
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
                        "<div style='background-color: rgba(255, 169, 33, 0.14); padding: 10px; margin: 10px 0; border-radius: 4px; color: inherit;'>",
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
                        "<div style='background-color: rgba(255, 169, 33, 0.14); padding: 10px; margin: 10px 0; border-radius: 4px; color: inherit;'>",
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
                        "<p style='color: inherit;'><em>Specialized statistical plots are not available for this combination.</em></p>"
                    )
                )

                html <- paste0(html, base_message)

                # Add notes about option applicability
                notes_html <- ""

                # Note about statistical approach
                if (analysis_info$dep_type == "factor" && analysis_info$group_type == "factor") {
                    notes_html <- paste0(notes_html,
                        "<p style='background-color: rgba(33, 152, 239, 0.13); padding: 8px; border-radius: 4px; font-size: 0.9em; color: inherit;'>",
                        "<strong>Note:</strong> Statistical approach option does not apply to categorical comparisons.</p>"
                    )
                }

                # Note about alluvial style
                if (analysis_info$plot_type == "repeated_factor_factor") {
                    notes_html <- paste0(notes_html,
                        "<p style='background-color: rgba(33, 159, 43, 0.1); padding: 8px; border-radius: 4px; font-size: 0.9em; color: inherit;'>",
                        "<strong>Alluvial style option is available</strong> for this repeated categorical comparison.</p>"
                    )
                } else if (analysis_info$direction == "repeated") {
                    notes_html <- paste0(notes_html,
                        "<p style='background-color: rgba(33, 152, 239, 0.13); padding: 8px; border-radius: 4px; font-size: 0.9em; color: inherit;'>",
                        "<strong>Note:</strong> Alluvial style option only applies to repeated factor vs factor comparisons.</p>"
                    )
                }

                # Plot type information
                plot_type_safe <- private$.safeHtmlOutput(analysis_info$plot_type)
                notes_html <- paste0(notes_html,
                    "<p style='color: inherit; font-size: 0.85em; margin-top: 10px;'>",
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
                html <- "<div style='background-color: rgba(114, 184, 33, 0.1); border-left: 4px solid #689f38; padding: 15px; margin: 10px 0; border-radius: 4px; color: inherit;'>"
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
                        "<li><strong>Positive slope:</strong> Higher {group_var_safe} values \u2192 higher {dep_var_safe} values</li>",
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
                        "<li><strong>Distribution shift:</strong> Shows the overall change between the two time points or conditions</li>",
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
                        "<p>This <strong>horizontal group-comparison plot</strong> shows the distribution of <code>{group_var_safe}</code> values within each <code>{dep_var_safe}</code> category.</p>",
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
                        "<p style='color: inherit;'><em>While specialized statistical tests aren't available for this combination, ",
                        "the visualization can still provide valuable insights about patterns in your data.</em></p>"
                    )
                )

                html <- paste0(html, interpretation)

                # Add assumption notes based on statistical approach
                if (analysis_info$dep_type == "continuous" || analysis_info$group_type == "continuous") {
                    assumption_html <- "<div style='background-color: rgba(255, 255, 255, 0.06); padding: 10px; margin-top: 10px; border-radius: 4px; border: 1px solid #ddd; color: inherit;'>"
                    assumption_html <- paste0(assumption_html, "<h5 style='margin-top: 0; color: inherit;'>Statistical Approach</h5>")

                    if (analysis_info$distribution == "p") {
                        assumption_html <- paste0(assumption_html,
                            "<p style='margin: 5px 0;'><strong>Parametric:</strong> Assumes normally distributed data. ",
                            "Best for continuous variables with bell-shaped distributions.</p>",
                            "<p style='font-size: 0.85em; color: inherit;'>",
                            "<strong>When to use:</strong> Data appears symmetric, no extreme outliers, n\u226530 per group</p>"
                        )
                    } else if (analysis_info$distribution == "np") {
                        assumption_html <- paste0(assumption_html,
                            "<p style='margin: 5px 0;'><strong>Nonparametric:</strong> Distribution-free method. ",
                            "Suitable for skewed data, ordinal scales, or when normality assumptions are violated.</p>",
                            "<p style='font-size: 0.85em; color: inherit;'>",
                            "<strong>When to use:</strong> Skewed data, outliers present, ordinal scales, small samples</p>"
                        )
                    } else if (analysis_info$distribution == "r") {
                        assumption_html <- paste0(assumption_html,
                            "<p style='margin: 5px 0;'><strong>Robust:</strong> Less sensitive to outliers. ",
                            "Good choice when data contains extreme values that might affect standard tests.</p>",
                            "<p style='font-size: 0.85em; color: inherit;'>",
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
                            "<p style='margin: 5px 0;'>Bayes factors depend on the prior. The ggstatsplot default prior is used ",
                            "(Cauchy, scale 0.707, for mean comparisons); the result sentence states the prior actually applied.</p>",
                            "<p style='font-size: 0.85em; color: inherit;'>",
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

                dep_data <- data[[analysis_info$dep_var]]
                group_data <- data[[analysis_info$group_var]]

                # Sample size checks
                total_n <- sum(!is.na(dep_data) & !is.na(group_data))
                if (total_n < 10) {
                    private$.addNotice('STRONG_WARNING', 'Very Small Sample', glue::glue(
                        "Only {total_n} complete observations.\n",
                        " - Hypothesis tests are unreliable at this size; report descriptive statistics only\n",
                        " - Effect sizes and confidence intervals will be very imprecise\n",
                        " - State the sample-size limitation in any report"
                    ))
                } else if (total_n < 30) {
                    private$.addNotice('STRONG_WARNING', 'Small Sample Size', glue::glue(
                        "Small sample size detected (n={total_n}).\n",
                        " - Sample size alone does not determine the appropriate test\n",
                        " - Consider robust statistical methods\n",
                        " - Results may have reduced statistical power"
                    ))
                }

                # Parametric assumption checks
                if (analysis_info$distribution == "p" && analysis_info$dep_type == "continuous") {
                    # Check for extreme outliers (beyond 3.5 IQR) WITHIN each group: a
                    # real group difference widens the pooled IQR and hides them.
                    grp <- if (analysis_info$group_type == "factor") group_data else rep(1L, length(dep_data))
                    extreme_outliers <- sum(unlist(tapply(dep_data, grp, function(v) {
                        v <- v[!is.na(v)]
                        if (length(v) < 5) return(0L)
                        q <- stats::quantile(v, c(0.25, 0.75))
                        iqr <- q[[2]] - q[[1]]
                        sum(v < q[[1]] - 3.5 * iqr | v > q[[2]] + 3.5 * iqr)
                    })), na.rm = TRUE)
                    if (extreme_outliers > 0) {
                        private$.addNotice('STRONG_WARNING', 'Extreme Outliers Detected', glue::glue(
                            "Extreme outliers detected in {analysis_info$dep_var}.\n",
                            " - Found: {extreme_outliers} extreme outlier(s) (>3.5 IQR within its group)\n",
                            " - Consider robust statistical approach (distribution='r')\n",
                            " - Outliers may unduly influence parametric results"
                        ))
                    }

                    # Reminder, not a finding: it depends on n alone, so INFO
                    if (total_n < 100) {
                        private$.addNotice('INFO', 'Normality Check', glue::glue(
                            "Consider checking distribution visually (n={total_n}).\n",
                            " - Inspect distributions and outliers within each group\n",
                            " - Consider nonparametric approach if data appears skewed\n",
                            " - Inspect violin plot shape for distributional form"
                        ))
                    }
                }

                # Group size balance check for between-subjects designs
                if (analysis_info$direction == "independent" && analysis_info$group_type == "factor") {
                    group_sizes <- table(group_data)
                    min_group <- min(group_sizes)
                    max_group <- max(group_sizes)
                    if (max_group / min_group > 4) {
                        private$.addNotice('WARNING', 'Unbalanced Group Sizes', glue::glue(
                            "Unbalanced group sizes detected.\n",
                            " - Smallest group: {min_group}\n",
                            " - Largest group: {max_group}\n",
                            " - Ratio: {round(max_group/min_group, 1)}:1\n",
                            " - Results may be less reliable with imbalanced designs"
                        ))
                    }

                    # Very small group sizes
                    if (min_group < 5) {
                        private$.addNotice('STRONG_WARNING', 'Very Small Group Sizes', glue::glue(
                            "Very small group size(s) detected.\n",
                            " - Minimum group size: {min_group}\n",
                            " - Consider combining groups if scientifically appropriate\n",
                            " - Consider exact statistical methods for small samples\n",
                            " - Statistical power may be severely limited"
                        ))
                    }
                }

                invisible(NULL)
            },

            # Per-panel one-row statistics from a ggstatsplot figure (or a
            # patchwork of them). Empty when the figure carries no test.
            .extractStats = function(plot) {
                if (is.null(plot)) return(list())
                st <- tryCatch(suppressWarnings(ggstatsplot::extract_stats(plot)), error = function(e) NULL)
                if (is.null(st)) return(list())
                if (!is.null(st$subtitle_data)) return(list(list(label = NULL, sd = as.data.frame(st$subtitle_data))))
                out <- list()
                for (i in seq_along(st)) {
                    sd <- if (is.list(st[[i]])) st[[i]]$subtitle_data else NULL
                    if (is.null(sd) || nrow(sd) == 0) next
                    title <- tryCatch(plot[[i]]$labels$title, error = function(e) NULL)
                    out[[length(out) + 1]] <- list(label = if (is.null(title)) NULL else as.character(title), sd = as.data.frame(sd))
                }
                out
            },

            # One copy-ready sentence from a one-row statsExpressions result.
            .sentence = function(sd, dep, group, label = NULL) {
                esc <- private$.safeHtmlOutput
                fmt <- function(x) {
                    if (is.null(x) || length(x) == 0 || is.na(x[1])) return("NA")
                    x <- x[1]
                    if (abs(x) >= 1e6 || (x != 0 && abs(x) < 1e-3)) return(formatC(x, digits = 3, format = "g"))
                    base::format(signif(x, 3), scientific = FALSE, trim = TRUE, big.mark = ",")
                }
                p_text <- function(p) {
                    if (is.null(p) || is.na(p[1])) return("p not available")
                    if (p[1] < 0.001) "p < 0.001" else paste0("p = ", formatC(p[1], format = "f", digits = 3))
                }
                n <- if (!is.null(sd$n.obs)) sd$n.obs[1] else NA
                effect <- ""
                if (!is.null(sd$estimate) && !is.na(sd$estimate[1])) {
                    ci <- ""
                    if (!is.null(sd$conf.low) && !is.na(sd$conf.low[1])) {
                        level <- if (!is.null(sd$conf.level)) round(100 * sd$conf.level[1]) else 95
                        ci <- sprintf(" (%d%% %s %s to %s)", as.integer(level),
                                      if (!is.null(sd$bf10)) "credible interval" else "CI",
                                      fmt(sd$conf.low), fmt(sd$conf.high))
                    }
                    es_name <- if (!is.null(sd$effectsize)) esc(sd$effectsize[1]) else "effect size"
                    effect <- sprintf("; %s = %s%s", es_name, fmt(sd$estimate), ci)
                }
                core <- if (!is.null(sd$bf10)) {
                    prior <- if (!is.null(sd$prior.distribution))
                        sprintf(" (prior: %s, scale %s)", esc(sd$prior.distribution[1]), fmt(sd$prior.scale)) else ""
                    sprintf("BF10 = %s%s", fmt(sd$bf10), prior)
                } else {
                    df <- c(if (!is.null(sd$df)) sd$df[1], if (!is.null(sd$df.error)) sd$df.error[1])
                    df <- df[!is.na(df)]
                    sprintf("test statistic = %s%s, %s", fmt(sd$statistic),
                            if (length(df)) paste0(" (df = ", paste(vapply(df, fmt, ""), collapse = ", "), ")") else "",
                            p_text(sd$p.value))
                }
                where <- if (is.null(label)) "" else sprintf(", %s", esc(label))
                sprintf("%s by %s%s: %s; %s%s; n = %s.", esc(dep), esc(group), where, esc(sd$method[1]), core, effect, n)
            },

            .buildSummary = function(stats, has_test, approach, analysis_info) {
                box <- function(body) {
                    paste0(
                        "<div style='padding: 12px; border-left: 4px solid #1976d2; background-color: rgba(33, 152, 239, 0.08); color: inherit;'>",
                        "<p style='margin-top: 0;'><strong>Result sentence</strong></p>", body, "</div>")
                }
                if (!has_test) {
                    return(box("<p>This figure is descriptive; no statistical test applies to this combination of variables.</p>"))
                }
                if (length(stats) == 0) {
                    return(box(sprintf("<p>The %s test could not be computed for this comparison; the figure is shown without a test result (see the warning above).</p>", approach)))
                }
                box(paste(vapply(stats, function(st) {
                    paste0("<p>", private$.sentence(st$sd, analysis_info$dep_var, analysis_info$group_var, st$label), "</p>")
                }, ""), collapse = ""))
            },

            .run = function() {

                private$.noticeList <- list()
                private$.renderNotices()

                analysis_info <- NULL

                # Invalidate cache to ensure fresh analysis with current options
                private$.invalidateCache()
                
                # Get analysis type information
                analysis_info <- private$.detectAnalysisType()
                
                # If no variables selected, show initial message
                if (is.null(analysis_info)) {

                    todo <- glue::glue(
                "<div style='padding: 20px; background-color: rgba(88, 88, 88, 0.06); border-radius: 8px; color: inherit;'>",
                "<h3 style='color: #1976d2; margin-top: 0;'>Welcome to Automatic Plot Selection</h3>",
                "<p style='font-size: 14px;'>This tool automatically selects the most appropriate statistical visualization based on your variable types.</p>",
                "<h4 style='color: inherit;'>Getting Started:</h4>",
                "<ol style='font-size: 13px;'>",
                "<li>Select a <strong>Dependent Variable</strong> (y-axis, outcome)</li>",
                "<li>Select a <strong>Grouping Variable</strong> (x-axis, comparison groups)</li>",
                "<li>Configure <strong>Study Design</strong> and <strong>Statistical Approach</strong></li>",
                "</ol>",
                "<p style='font-size: 12px; color: inherit;'><em>Powered by ggstatsplot and ggalluvial packages. Please cite jamovi and these packages.</em></p>",
                "</div>"
                    )

                    self$results$todo$setVisible(TRUE)
                    self$results$todo$setContent(todo)

                    return()

                }
                
                # Clear todo message
                self$results$todo$setVisible(FALSE)

                # Surface any missing optional packages through the notices output. .init writes
                # install guidance to ExplanationMessage for the no-variable case, but .run
                # overwrites ExplanationMessage once variables are selected, so route the guidance
                # to the (reset-at-top-of-run) notices output where it survives.
                missing_packages <- private$.getMissingPackages()
                if (length(missing_packages) > 0) {
                    install_cmd <- paste0("install.packages(c('", paste(missing_packages, collapse = "', '"), "'))")
                    private$.addNotice('WARNING', 'Optional Packages Missing', glue::glue(
                        "Optional packages are not installed: {paste(missing_packages, collapse = ', ')}.\n",
                        " - Install with: {install_cmd}\n",
                        " - Basic functionality still works; some plot types may fall back to simpler visualizations."
                    ))
                }

                # Enhanced data validation with context
                if (nrow(self$data) == 0) {
                    private$.addNotice('ERROR', 'No data available',
                        "The dataset has no rows. Check data loading, filters and variable selection.")
                    return()
                }

                # Check assumptions (adds STRONG_WARNING / WARNING notices to the
                # plain-text notices output item via private$.addNotice)
                private$.checkAssumptions(analysis_info, self$data)

                # Prepare data for plotting and counts
                prepared_data <- private$.prepareDataForPlot(analysis_info)

                # Explanation and interpretation are opt-in (jamovi convention:
                # educational panels render only when the user enables them).
                if (self$options$showExplanations) {
                    self$results$ExplanationMessage$setContent(paste0(
                        "<div style='font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, sans-serif;'>",
                        private$.generateExplanationMessage(analysis_info),
                        private$.generateClinicalInterpretation(analysis_info),
                        "</div>"))
                }

                # Validate the prepared data before reporting success. .validatePlotData adds
                # the appropriate ERROR notice (Insufficient Data / No Valid Levels) when the
                # data cannot support the selected plot; only report success when it passes.
                # The same validation runs again in the render path, but .addNotice
                # de-duplicates identical notices so any ERROR is shown once.
                validation_ok <- private$.validatePlotData(
                    prepared_data,
                    private$.plotTypeLabel(analysis_info$plot_type)
                )

                if (validation_ok) {
                    # Add success summary at the end.
                    #
                    # n_used must count rows the STATISTICS can use, not rows in the
                    # frame. With `Exclude missing values` off (the default) missing
                    # values are left in place and ggstatsplot drops them itself, so
                    # nrow() over-reported N: 180 rows with 155 usable outcomes was
                    # announced as "Observations used: 180 of 180".
                    n_total <- nrow(self$data)
                    .an_vars <- Filter(Negate(is.null), list(
                        analysis_info$dep_var, analysis_info$group_var, analysis_info$grvar))
                    .an_vars <- intersect(unlist(.an_vars), names(prepared_data$data))
                    n_used <- if (length(.an_vars))
                        sum(stats::complete.cases(prepared_data$data[, .an_vars, drop = FALSE]))
                    else nrow(prepared_data$data)
                    n_dropped_na <- nrow(prepared_data$data) - n_used

                    # Random subsampling changes the statistics, not just the
                    # picture. "Observations used: 5,000 of 30,000" reads like
                    # missing-data exclusion; the explanation that the reduction was
                    # a deliberate random draw went to message(), i.e. the R
                    # console, which a jamovi user never sees.
                    #
                    # It matters because every p-value below is computed on the
                    # subsample. Measured over 300 replicates at d = 0.05 with
                    # n = 30,000: full-data power 99.7% (median p ~ 0.0000) against
                    # 45.3% on the 5,000-row draw (median p = 0.0716). The same
                    # data goes from a near-certain detection to a coin flip.
                    # A comparison needs at least two groups. With one, every
                    # "difference" is vacuous, yet the run reported success.
                    blocking <- FALSE
                    .gv <- analysis_info$group_var
                    if (!is.null(.gv) && .gv %in% names(prepared_data$data)) {
                        .n_grp <- length(unique(stats::na.omit(prepared_data$data[[.gv]])))
                        if (.n_grp < 2) {
                            blocking <- TRUE
                            private$.addNotice('ERROR', 'Only one group to compare',
                                glue::glue(
                                    "'{.gv}' has a single level, so there is nothing to compare against. ",
                                    "Any test or effect size shown describes one group on its own. ",
                                    "Choose a grouping variable with at least two levels."))
                        }
                    }

                    # A grouping variable with (almost) one level per row is an
                    # identifier, not a grouping. ggstatsplot would still run its
                    # k(k-1)/2 pairwise tests (25 s for 200 levels here) and then die
                    # with "not enough observations", so stop before drawing.
                    if (!is.null(.gv) && analysis_info$group_type == "factor" && .gv %in% names(prepared_data$data)) {
                        .lv <- length(unique(stats::na.omit(prepared_data$data[[.gv]])))
                        .nn <- sum(!is.na(prepared_data$data[[.gv]]))
                        if (.lv > 20 && .lv > .nn / 2) {
                            jmvcore::reject(glue::glue(
                                "'{.gv}' has {.lv} levels for {.nn} observations - about one level per row - ",
                                "so it looks like an identifier, not a grouping variable. Choose a variable with a few groups."))
                        } else if (.lv > 20) {
                            private$.addNotice('STRONG_WARNING', 'Many groups', glue::glue(
                                "'{.gv}' has {.lv} levels. The comparison runs {.lv * (.lv - 1) / 2} pairwise tests, ",
                                "which is slow and crowds the figure; consider collapsing levels."))
                        }
                    }

                    # Plot selection infers the variable TYPE from the data, so a
                    # constant numeric outcome (one unique value) is read as a
                    # factor and the whole analysis silently switches from a
                    # continuous comparison to a categorical one.
                    .dv <- analysis_info$dep_var
                    if (!is.null(.dv) && .dv %in% names(prepared_data$data)) {
                        .vals <- stats::na.omit(prepared_data$data[[.dv]])
                        if (length(.vals) > 0 && length(unique(.vals)) < 2) {
                            blocking <- TRUE
                            private$.addNotice('ERROR', 'Outcome has no variation',
                                glue::glue(
                                    "'{.dv}' takes a single value, so it cannot be compared across groups. ",
                                    "Note that a constant numeric variable is also read as categorical by the ",
                                    "automatic plot selection, which changes the analysis type - the plot below ",
                                    "is '{analysis_info$plot_type}'. Choose an outcome that varies."))
                        }
                    }

                    # Split panels are drawn one level at a time; a level with fewer
                    # than two complete rows cannot be plotted and would otherwise
                    # vanish from the panel grid without a word (the render path
                    # cannot post notices).
                    .sv <- analysis_info$grvar
                    if (!is.null(.sv) && .sv %in% names(prepared_data$data)) {
                        .cc <- stats::complete.cases(prepared_data$data[, c(.dv, .gv), drop = FALSE])
                        .present <- unique(stats::na.omit(as.character(prepared_data$data[[.sv]])))
                        .per_level <- table(factor(as.character(prepared_data$data[[.sv]])[.cc], levels = .present))
                        .thin <- names(.per_level)[.per_level < 2]
                        if (length(.thin) > 0) {
                            private$.addNotice('STRONG_WARNING', 'Split panel(s) omitted', glue::glue(
                                "Level(s) {paste(sQuote(.thin, FALSE), collapse = ', ')} of '{.sv}' have fewer than 2 complete observations, ",
                                "so no panel is drawn for them. The remaining panels are shown."))
                        }
                    }

                    if (isTRUE(prepared_data$sampled)) {
                        private$.addNotice('STRONG_WARNING', 'Statistics computed on a random subsample',
                            glue::glue(
                                "Only {base::format(n_used, big.mark = ',')} of {base::format(n_total, big.mark = ',')} rows were analysed. ",
                                "The rows were drawn at RANDOM for plotting speed - they were not excluded for missing data.\n",
                                " - Every p-value, effect size and confidence interval below describes this subsample, not your full dataset.\n",
                                " - Discarding rows lowers power, so a real effect is more likely to be missed.\n",
                                " - Turn off 'Sample large datasets' to analyse all {base::format(n_total, big.mark = ',')} rows before reporting any result.\n",
                                " - Seed {self$options$seed} was used, so the same draw repeats until you change it."
                            ))
                    }

                    # Do not claim success alongside a blocking ERROR - a panel that
                    # says "completed successfully" under "there is nothing to
                    # compare against" is exactly the contradiction being removed.
                    # Build the figure here, once: its statistics are quoted in the
                    # result sentence and checked for silent failure; the renderer
                    # reuses it.
                    private$.checkpoint()
                    plot <- private$.generatePlot(analysis_info, prepared_data)
                    private$.cached_plot <- plot
                    stats <- private$.extractStats(plot)
                    has_test <- analysis_info$plot_type %in% c(
                        "independent_factor_continuous", "independent_continuous_continuous",
                        "independent_factor_factor", "independent_continuous_factor",
                        "repeated_factor_continuous")
                    approach <- switch(analysis_info$distribution,
                        p = "parametric", np = "nonparametric", r = "robust", bf = "Bayesian",
                        analysis_info$distribution)
                    # ggstatsplot swallows a failing test and draws the figure with
                    # no subtitle (the Bayesian one-way comparison errors inside
                    # performance:: on some data). Say so instead of staying silent.
                    if (has_test && length(stats) == 0 && !blocking) {
                        private$.addNotice('STRONG_WARNING', 'Statistics could not be computed', glue::glue(
                            "The {approach} test failed for this comparison, so the figure is shown without a test result. ",
                            "Choose another statistical approach or check the data."))
                    }
                    if (self$options$showSummary) {
                        self$results$summary$setContent(
                            private$.buildSummary(stats, has_test, approach, analysis_info))
                    }

                    # The alluvial diagrams DRAW missing values as an 'NA' stratum;
                    # every other plot type drops them.
                    na_fate <- if (identical(analysis_info$plot_type, "repeated_factor_factor"))
                        "appear as an 'NA' stratum in the diagram (enable 'Exclude missing values' to drop them)"
                    else "are omitted from the statistics"
                    # Categorical comparisons always use the chi-square / flow display;
                    # echoing the approach option there implied it had been applied.
                    approach_label <- if (analysis_info$dep_type == "factor" && analysis_info$group_type == "factor")
                        "not applicable (categorical comparison)" else analysis_info$distribution

                    private$.addNotice('INFO', 'Analysis Summary', glue::glue(
                        "{if (blocking) 'Analysis ran, but see the error(s) above before using these results.' else 'Analysis completed successfully.'}\n",
                        " - Plot type: {analysis_info$plot_type}\n",
                        " - Observations used: {base::format(n_used, big.mark = ',')} of {base::format(n_total, big.mark = ',')}",
                        "{if (isTRUE(prepared_data$sampled)) ' (RANDOM SUBSAMPLE - see warning above)' else ''}\n",
                        "{if (n_dropped_na > 0) paste0(' - ', base::format(n_dropped_na, big.mark = \',\'), ' row(s) carried a missing value in the analysed variables and ', na_fate, '.\\n') else ''}",
                        " - Statistical approach: {approach_label}\n",
                        " - Study design: {analysis_info$direction}"
                    ))
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
                            subtitle = "No statistical test is available for this combination",
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
                            subtitle = "No statistical test is available for this combination",
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
                            subtitle = "No statistical test is available for this combination",
                            x = x_var,
                            y = "Count"
                        )
                } else if (analysis_info$dep_type == "factor" && analysis_info$group_type == "factor") {
                    # Bar plot for factor vs factor
                    plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x_var), fill = !!rlang::sym(y_var))) +
                        ggplot2::geom_bar(position = "dodge", alpha = 0.8) +
                        ggplot2::labs(
                            title = paste("Basic Bar Plot:", y_var, "by", x_var),
                            subtitle = "No statistical test is available for this combination",
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
                            subtitle = "No statistical test is available for this combination",
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
                repeated_continuous <- identical(analysis_info$plot_type, "repeated_factor_continuous")
                subject_id <- if (repeated_continuous) self$options$subjectID else NULL
                if (repeated_continuous && (is.null(subject_id) || !nzchar(subject_id)))
                    jmvcore::reject("Select a Subject ID for repeated continuous outcomes, or use Repeated Measurements (jjwithinstats) for wide-format data.")
                selected <- unique(c(analysis_info$dep_var, analysis_info$group_var, analysis_info$grvar, subject_id))
                mydata <- self$data[, selected, drop = FALSE]
                
                # Handle NA exclusion if requested
                if (self$options$excl) {
                    before_n <- nrow(mydata)
                    mydata <- jmvcore::naOmit(mydata)
                    after_n <- nrow(mydata)
                } else {
                    before_n <- nrow(mydata)
                    after_n <- before_n
                }
                
                if (repeated_continuous) {
                    if (subject_id %in% c(analysis_info$dep_var, analysis_info$group_var, analysis_info$grvar))
                        jmvcore::reject("Subject ID must differ from the outcome, condition and split variables.")
                    mydata <- mydata[stats::complete.cases(mydata) &
                        is.finite(mydata[[analysis_info$dep_var]]), , drop = FALSE]
                    if (anyDuplicated(mydata[c(subject_id, analysis_info$group_var)]) > 0)
                        jmvcore::reject("Repeated analysis requires one observation per subject and condition. Resolve duplicate records.")
                    if (!is.null(analysis_info$grvar)) {
                        n_panels <- tapply(as.character(mydata[[analysis_info$grvar]]),
                            mydata[[subject_id]], function(x) length(unique(x)))
                        if (any(n_panels > 1))
                            jmvcore::reject("Each subject must belong to one split panel.")
                    }
                    conditions <- unique(mydata[[analysis_info$group_var]])
                    n_by_id <- table(mydata[[subject_id]])
                    complete_ids <- names(n_by_id)[n_by_id == length(conditions)]
                    mydata <- mydata[mydata[[subject_id]] %in% complete_ids, , drop = FALSE]
                    mydata <- droplevels(mydata)
                    if (length(conditions) < 2 || length(complete_ids) < 3)
                        jmvcore::reject("Repeated analysis requires at least three complete subjects and two conditions.")
                    after_n <- nrow(mydata)
                }

                # Handle large dataset sampling if requested
                original_nrow <- nrow(mydata)
                # Threshold and retained size are user-configurable; the defaults
                # (10,000 / 5,000) reproduce the previous hard-coded behaviour.
                thr <- self$options$sampleThreshold
                keep <- self$options$sampleSize

                if (self$options$sampleLarge && original_nrow > thr) {
                    # User-configurable seed for reproducible sampling (default 42).
                    seed_val <- self$options$seed
                    if (is.null(seed_val)) seed_val <- 42
                    withr::local_preserve_seed()
                    set.seed(seed_val)
                    # Never "sample" more rows than exist.
                    sample_size <- min(keep, original_nrow)
                    if (repeated_continuous) {
                        ids <- unique(mydata[[subject_id]])
                        n_conditions <- length(unique(mydata[[analysis_info$group_var]]))
                        n_keep <- min(length(ids), max(3L, floor(sample_size / n_conditions)))
                        chosen <- ids[sample.int(length(ids), n_keep)]
                        mydata <- mydata[mydata[[subject_id]] %in% chosen, , drop = FALSE]
                        sample_size <- nrow(mydata)
                    } else {
                        mydata <- mydata[sample.int(nrow(mydata), sample_size), , drop = FALSE]
                    }
                    message(glue::glue("Large dataset detected ({base::format(original_nrow, big.mark = ',')} rows). Sampled {base::format(sample_size, big.mark = ',')} rows for visualization performance. Disable 'Sample Large Datasets' option to use full dataset."))
                    sampled_flag <- sample_size < original_nrow
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
                    subject_id = subject_id,
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
                withr::local_seed(self$options$seed)
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

                # formula.tools (pulled in by logistf) overrides as.character.formula,
                # which breaks stats::oneway.test. ggstatsplot swallows that and returns
                # a plot with no subtitle at all, so a three-or-more-group comparison
                # rendered a figure with no statistics on it and no warning.
                plot <- withBaseFormulaChar(ggstatsplot::ggbetweenstats(
                    data = prepared_data$data,
                    x = !!rlang::sym(prepared_data$group),
                    y = !!rlang::sym(prepared_data$dep),
                    type = prepared_data$distribution
                ))
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
                    y = !!rlang::sym(prepared_data$group),
                    type = prepared_data$distribution
                )
                return(plot)
            },

            # Plot function for dot plots (continuous vs factor)
            .plotDotplotStats = function(prepared_data) {
                # The selector roles are reversed: group is continuous and dep
                # is categorical. Compare observations between categories.

                # Validate data and return NULL if validation fails
                if (!private$.validatePlotData(prepared_data, "horizontal group-comparison plot")) {
                    return(NULL)
                }

                # Checkpoint before expensive plot generation
                private$.checkpoint()

                x_var <- prepared_data$group  # continuous variable
                y_var <- prepared_data$dep    # factor variable

                # Compare observations between categories, preserving the estimand
                # when the two selected variables are exchanged.
                plot <- withBaseFormulaChar(ggstatsplot::ggbetweenstats(
                    data = prepared_data$data,
                    x = !!rlang::sym(y_var),
                    y = !!rlang::sym(x_var),
                    type = prepared_data$distribution
                )) + ggplot2::coord_flip()
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
                    subject.id = !!rlang::sym(prepared_data$subject_id)
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
                    jmvcore::reject("Package 'ggalluvial' is required for alluvial plots but is not installed. Install with: install.packages('ggalluvial')")
                }
                if (!requireNamespace("dplyr", quietly = TRUE)) {
                    jmvcore::reject("Package 'dplyr' is required for data manipulation but is not installed. Install with: install.packages('dplyr')")
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
                    ggplot2::geom_label(stat = ggalluvial::StatStratum,
                        ggplot2::aes(label = ggplot2::after_stat(stratum))) +
                    ggplot2::theme_minimal()
                
                return(plot)
            },
            
            # easyalluvial implementation
            .plotAlluvialEasy = function(prepared_data) {
                # Enhanced package validation
                if (!requireNamespace("easyalluvial", quietly = TRUE)) {
                    jmvcore::reject("Package 'easyalluvial' is required for simplified alluvial plots but is not installed. Install with: install.packages('easyalluvial')")
                }
                
                # Subset to the user-selected group/dep columns (group first, matching the
                # ggalluvial axis1=group, axis2=dep layout) so the diagram reflects the chosen
                # variables instead of every column in the data frame.
                alluvial_data <- prepared_data$data[, c(prepared_data$group, prepared_data$dep), drop = FALSE]

                plot <- .quietly(easyalluvial::alluvial_wide(
                    data = alluvial_data,
                    max_variables = 2,
                    fill_by = 'first_variable'
                ))
                return(plot)
            },
            
            # Grouped plots for when grvar is specified
            .plotGrouped = function(analysis_info, prepared_data) {
                # Check if native grouped function exists for this plot type
                grouped_func_available <- FALSE
                
                # Handle specific grouped plot types with native support
                if (analysis_info$plot_type == "independent_factor_continuous") {
                    # Use grouped_ggbetweenstats for factor vs continuous. Capture success via
                    # the tryCatch return value so a failure falls through to the manual
                    # multi-panel fallback instead of propagating uncaught through .generatePlot.
                    grouped_func_available <- tryCatch({
                        plot <- withBaseFormulaChar(ggstatsplot::grouped_ggbetweenstats(
                            data = prepared_data$data,
                            x = !!rlang::sym(prepared_data$group),
                            y = !!rlang::sym(prepared_data$dep),
                            grouping.var = !!rlang::sym(prepared_data$grvar),
                            type = prepared_data$distribution
                        ))
                        TRUE
                    }, error = function(e) {
                        message("grouped_ggbetweenstats failed: ", conditionMessage(e))
                        FALSE
                    })
                } else if (analysis_info$plot_type == "independent_continuous_continuous") {
                    # Try grouped_ggscatterstats if available. Capture the tryCatch return value
                    # so a failure correctly resets the flag and falls through to the manual
                    # multi-panel fallback (the previous handler assigned the flag in its own
                    # local scope with no <<-, discarding the reset).
                    grouped_func_available <- tryCatch({
                        plot <- ggstatsplot::grouped_ggscatterstats(
                            data = prepared_data$data,
                            x = !!rlang::sym(prepared_data$group),
                            y = !!rlang::sym(prepared_data$dep),
                            grouping.var = !!rlang::sym(prepared_data$grvar),
                            type = prepared_data$distribution
                        )
                        TRUE
                    }, error = function(e) {
                        message("grouped_ggscatterstats failed: ", conditionMessage(e))
                        FALSE
                    })
                }
                
                # If no native grouped function, create multiple plots
                if (!grouped_func_available) {
                    # Get unique levels of grouping variable with enhanced validation
                    grvar_col <- prepared_data$data[[prepared_data$grvar]]
                    # NA is not a level: `NA == level` selects phantom all-NA rows into
                    # every panel and drew an 'NA' stratum in split alluvial diagrams.
                    grvar_levels <- unique(stats::na.omit(grvar_col))
                    
                    # Create a list to store individual plots
                    plot_list <- list()
                    
                    # Generate plot for each level of grouping variable
                    for (i in seq_along(grvar_levels)) {
                        # Checkpoint before expensive grouped plot generation
                        private$.checkpoint(flush = FALSE)
                        level <- grvar_levels[i]
                        
                        # Filter data for this level
                        level_data <- prepared_data
                        level_data$data <- prepared_data$data[which(grvar_col == level), , drop = FALSE]
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
                    # No data available for plotting; the empty-dataset condition is
                    # reported to the user via the notices/explanation outputs from .run.
                    return()
                }
                
                # For repeated_continuous_continuous / repeated_continuous_factor there is no
                # specialized ggstatsplot function; .generatePlot returns NULL for these and
                # falls through to .plotFallback (basic ggplot2), matching the explanation
                # message which promises a basic visualization. Do NOT early-return here or the
                # promised plot would never render.

                # Checkpoint before data preparation
                private$.checkpoint()
                
                # Prepare data for plotting
                prepared_data <- private$.prepareDataForPlot(analysis_info)
                
                # Adjust plot size if grouping variable is used
                if (!is.null(prepared_data$grvar)) {
                    # Get number of levels in grouping variable
                    grvar_col <- prepared_data$data[[prepared_data$grvar]]
                    num_levels <- length(unique(stats::na.omit(grvar_col)))
                    
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
                
                # .run() already built the figure (and quoted its statistics);
                # rebuild only when this render has no run behind it (a resize
                # after reload, or a run that returned early).
                plot <- private$.cached_plot
                if (is.null(plot)) {
                    plot <- private$.generatePlot(analysis_info, prepared_data)
                }
                
                # Return the plot
                if (!is.null(plot)) {
                    # ggalluvial warns "Some strata appear at multiple axes" whenever the
                    # same categories exist at both time points - the normal
                    # repeated-measures case - and jamovi would show it in Analysis Notes.
                    .quietly(print(plot), deprecation_pattern = "strata appear at multiple axes")
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
