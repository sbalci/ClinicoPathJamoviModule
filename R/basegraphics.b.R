
#' @title Base Graphics Visualization
#' @description
#' Base R graphics visualization module providing fast, blazing fast, and extremely 
#' customizable data visualization solutions using pure base R graphics.
#' This module showcases the power and flexibility of base R plotting functions
#' without external dependencies.
#'
#' @details
#' This module implements the functionality requested in GitHub Issue #75,
#' providing comprehensive base R graphics visualization capabilities. Base R
#' graphics offer exceptional performance and unlimited customization potential
#' for clinical research and data visualization.
#'
#' Supported plot types:
#' - Scatter plots: Visualize relationships between continuous variables
#' - Line plots: Show trends and time series data
#' - Histograms: Display distribution of continuous variables
#' - Box plots: Compare distributions across groups
#' - Bar plots: Visualize categorical data frequencies
#' - Density plots: Smooth distribution visualization
#' - Pairs plots: Multiple variable relationships
#' - Matrix plots: Multiple series on same plot
#'
#' @param data A data frame containing the variables to plot.
#' @param plot_type Type of base R plot to generate.
#' @param x_var Variable for x-axis.
#' @param y_var Variable for y-axis (continuous plots).
#' @param group_var Optional grouping variable for stratified plots.
#' @param main_title Main title for the plot.
#' @param x_label Label for x-axis.
#' @param y_label Label for y-axis.
#'
#' @return Base R graphics plots with customizable styling and options.
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom dplyr select all_of
#' @importFrom janitor clean_names
#' @importFrom labelled set_variable_labels var_label
#' @importFrom haven as_factor
#' @import magrittr

basegraphicsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "basegraphicsClass",
    inherit = basegraphicsBase,
    private = list(

        # Internal data storage
        .processed_data = NULL,
        .plot_data = NULL,
        .warnings = list(),  # Collect warnings for HTML display (avoids Notice serialization errors)

        # Variable name escaping utility
        .escapeVariableName = function(x) {
            if (is.null(x)) return(NULL)
            # Use janitor for consistency, but add explicit safety
            cleaned <- janitor::make_clean_names(x)
            # Ensure valid R names
            make.names(cleaned)
        },

        .init = function() {
            # Initialize instructions
            instructions_html <- paste(
                "<div style='background-color: #fff3cd; padding: 15px; border-radius: 8px; margin: 10px 0; border-left: 4px solid #ff9800;'>",
                "<h4 style='color: #e65100; margin-top: 0;'>⚠️ IMPORTANT: Exploratory Visualization Only</h4>",
                "<p style='margin: 5px 0; color: #d84315;'><strong>This module provides DESCRIPTIVE and EXPLORATORY visualization only.</strong></p>",
                "<p style='margin: 5px 0;'><strong>NOT intended for:</strong></p>",
                "<ul style='margin: 5px 0; padding-left: 20px;'>",
                "<li>Formal statistical inference or hypothesis testing</li>",
                "<li>Clinical decision-making without proper statistical analysis</li>",
                "<li>Publication-quality statistical reporting</li>",
                "</ul>",
                "<p style='margin: 5px 0;'><strong>Statistical overlays (correlation, R²) are exploratory estimates only</strong> and should NOT be interpreted as rigorous statistical tests. For clinical research, always use appropriate statistical methods with proper validation.</p>",
                "</div>",
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>Base Graphics Visualization</h3>",
                "<div style='margin: 10px 0;'>",
                "<h4 style='color: #388e3c; margin: 10px 0 5px 0;'>Fast & Customizable Base R Plots:</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Scatter Plots:</strong> Visualize relationships between continuous variables</li>",
                "<li><strong>Line Plots:</strong> Show trends and time series data</li>",
                "<li><strong>Histograms:</strong> Display distribution of continuous variables</li>",
                "<li><strong>Box Plots:</strong> Compare distributions across groups</li>",
                "<li><strong>Bar Plots:</strong> Visualize categorical data frequencies</li>",
                "<li><strong>Density Plots:</strong> Smooth distribution visualization</li>",
                "<li><strong>Pairs Plots:</strong> Multiple variable relationships</li>",
                "<li><strong>Matrix Plots:</strong> Multiple series on same plot</li>",
                "</ul>",
                "</div>",
                "<div style='margin: 10px 0;'>",
                "<h4 style='color: #388e3c; margin: 10px 0 5px 0;'>Quick Start:</h4>",
                "<ol style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Choose Plot Type:</strong> Select from 8 base R plot types</li>",
                "<li><strong>Select Variables:</strong> Choose X and Y variables as appropriate</li>",
                "<li><strong>Optional Grouping:</strong> Add grouping variable for colored/stratified plots</li>",
                "<li><strong>Customize Appearance:</strong> Adjust titles, labels, colors, and styling</li>",
                "<li><strong>View Results:</strong> Generate fast, customizable base R graphics</li>",
                "</ol>",
                "</div>",
                "<div style='background-color: #fff8e1; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                "<p style='margin: 0; color: #f57c00;'><strong>Performance Note:</strong> Base R graphics are blazing fast and require no external dependencies, making them ideal for large datasets and rapid exploration.</p>",
                "</div>",
                "<p style='margin: 10px 0 0 0; color: #666; font-style: italic;'>💡 This module implements GitHub Issue #75 showcasing the power of base R graphics.</p>",
                "</div>"
            )
            
            self$results$instructions$setContent(instructions_html)
        },

        # Validate variable name cleaning
        .validateVariableNames = function(original_data) {
            original_names <- colnames(original_data)
            cleaned_names <- janitor::make_clean_names(original_names)

            # Check for duplicates in original names
            if (any(duplicated(original_names))) {
                dup_names <- original_names[duplicated(original_names)]
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'duplicateVariableNames',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(sprintf(
                    'Duplicate variable names detected: %s. This may cause incorrect variable mapping.',
                    paste(unique(dup_names), collapse = ", ")
                ))
                # REMOVED:                 self$results$insert(1, notice)  # Causes serialization error
            }

            # Check if cleaning creates duplicates
            if (any(duplicated(cleaned_names))) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'cleaningConflict',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent('Variable name cleaning created duplicate names. Some variables may be incorrectly mapped.')
                # REMOVED:                 self$results$insert(1, notice)  # Causes serialization error
            }

            # Check if cleaning changed names - add to warnings
            # NOTE: Removed Notice insertion to avoid serialization errors
            if (!all(cleaned_names == original_names)) {
                changed_count <- sum(cleaned_names != original_names)
                private$.warnings[[length(private$.warnings) + 1]] <- list(
                    type = "INFO",
                    message = sprintf('%d variable name(s) were cleaned to ensure compatibility. Original labels are preserved.', changed_count)
                )
            }
        },

        # Validate data types for selected plot type
        .validateDataTypes = function() {
            plot_type <- self$options$plot_type
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            group_var <- self$options$group_var

            if (is.null(x_var)) return()

            mydata <- private$.processed_data
            x_var_clean <- private$.escapeVariableName(x_var)

            # Check X variable type based on plot type
            if (plot_type %in% c("scatter", "line", "histogram", "density")) {
                # These require continuous X variable
                if (!is.numeric(mydata[[x_var_clean]])) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'inappropriateXType',
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(sprintf(
                        "Plot type '%s' typically requires a continuous X variable, but '%s' appears to be categorical. Results may be misleading.",
                        plot_type, x_var
                    ))
                # REMOVED:                     self$results$insert(1, notice)  # Causes serialization error
                }
            }

            if (plot_type == "barplot") {
                # Barplot works better with categorical data
                if (is.numeric(mydata[[x_var_clean]]) && length(unique(mydata[[x_var_clean]])) > 20) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'tooManyBars',
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(sprintf(
                        "Variable '%s' has many unique values (%d). Consider using a histogram instead of a bar plot for continuous data.",
                        x_var, length(unique(mydata[[x_var_clean]]))
                    ))
                # REMOVED:                     self$results$insert(1, notice)  # Causes serialization error
                }
            }

            # Check Y variable if specified
            if (!is.null(y_var)) {
                y_var_clean <- private$.escapeVariableName(y_var)
                if (!is.numeric(mydata[[y_var_clean]])) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'categoricalYVariable',
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(sprintf(
                        "Y variable '%s' should be continuous but appears to be categorical.",
                        y_var
                    ))
                # REMOVED:                     self$results$insert(1, notice)  # Causes serialization error
                }
            }

            # Check grouping variable
            if (!is.null(group_var)) {
                group_var_clean <- private$.escapeVariableName(group_var)
                group_data <- as.factor(mydata[[group_var_clean]])
                n_groups <- length(levels(group_data))

                if (n_groups > 10) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'tooManyGroups',
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(sprintf(
                        "Grouping variable '%s' has %d levels. Plots may be difficult to interpret with many groups.",
                        group_var, n_groups
                    ))
                # REMOVED:                     self$results$insert(1, notice)  # Causes serialization error
                }

                # Check for group imbalance
                group_counts <- table(group_data)
                min_count <- min(group_counts)
                max_count <- max(group_counts)

                if (max_count > 5 * min_count) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'groupImbalance',
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(sprintf(
                        'Severe group imbalance detected. Smallest group: n=%d, largest group: n=%d. Comparisons may be unreliable.',
                        min_count, max_count
                    ))
                # REMOVED:                     self$results$insert(1, notice)  # Causes serialization error
                }
            }
        },

        # Check sample size and report data loss
        .checkSampleSize = function(initial_data, final_data) {
            initial_n <- nrow(initial_data)
            final_n <- nrow(final_data)

            data_loss <- initial_n - final_n
            data_loss_pct <- round((data_loss / initial_n) * 100, 1)

            if (data_loss > 0) {
                # Data loss WARNING (>20%) or INFO (<20%)
                notice_type <- if (data_loss_pct > 20) jmvcore::NoticeType$STRONG_WARNING else jmvcore::NoticeType$INFO
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'dataLoss',
                    type = notice_type
                )
                notice$setContent(sprintf(
                    'Sample size: %d observations retained from %d total (%d removed due to missing values, %.1f%% data loss).',
                    final_n, initial_n, data_loss, data_loss_pct
                ))
                position <- if (data_loss_pct > 20) 1 else 999
                # REMOVED:                 self$results$insert(position, notice)  # Causes serialization error

                # Additional warning for substantial loss
                if (data_loss_pct > 20) {
                    notice2 <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'substantialDataLoss',
                        type = jmvcore::NoticeType$STRONG_WARNING
                    )
                    notice2$setContent('Substantial data loss (>20%) may indicate data quality issues or inappropriate variable selection.')
                # REMOVED:                     self$results$insert(2, notice2)  # Causes serialization error
                }
            } else {
                # No data loss - INFO
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'completeData',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent(sprintf('Sample size: %d complete observations (no missing data).', final_n))
                # REMOVED:                 self$results$insert(999, notice)  # Causes serialization error
            }

            # Small sample warning
            if (final_n < 30) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'smallSample',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(sprintf(
                    'Small sample size (n=%d). Results may be unstable and should be interpreted with caution.',
                    final_n
                ))
                # REMOVED:                 self$results$insert(1, notice)  # Causes serialization error
            }
        },

        # Validate statistics before adding overlays
        .validateStatistics = function(x, y = NULL) {
            n <- if (is.null(y)) length(x) else length(na.omit(cbind(x, y))[,1])

            # Check sample size for correlation/regression
            if (!is.null(y)) {
                if (n < 30) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'smallSampleCorrelation',
                        type = jmvcore::NoticeType$STRONG_WARNING
                    )
                    notice$setContent(sprintf(
                        'Sample size (n=%d) is below recommended minimum (n=30) for stable correlation estimates. Results should be interpreted with extreme caution.',
                        n
                    ))
                # REMOVED:                     self$results$insert(1, notice)  # Causes serialization error
                }

                # Check for outliers using ±3 SD threshold
                x_outliers <- sum(abs(scale(x, center = TRUE, scale = TRUE)) > 3, na.rm = TRUE)
                y_outliers <- sum(abs(scale(y, center = TRUE, scale = TRUE)) > 3, na.rm = TRUE)

                if (x_outliers > 0 || y_outliers > 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'outliersDetected',
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(sprintf(
                        'Potential outliers detected (%d observations >3 SD from mean). Correlation may be influenced by extreme values.',
                        x_outliers + y_outliers
                    ))
                # REMOVED:                     self$results$insert(1, notice)  # Causes serialization error
                }

                # Check for non-linearity using simple curvature test
                if (n >= 10) {
                    lm_linear <- lm(y ~ x)
                    lm_quadratic <- lm(y ~ x + I(x^2))
                    p_value <- tryCatch({
                        anova(lm_linear, lm_quadratic)$`Pr(>F)`[2]
                    }, error = function(e) 1)

                    if (!is.na(p_value) && p_value < 0.05) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'nonLinearRelationship',
                            type = jmvcore::NoticeType$WARNING
                        )
                        notice$setContent('Non-linear relationship detected. Linear correlation (r) and R² may not adequately describe this relationship.')
                # REMOVED:                         self$results$insert(1, notice)  # Causes serialization error
                    }
                }

                # Check normality assumption for Pearson correlation only
                # Spearman correlation is non-parametric and does not require normality
                if (n >= 10 && self$options$correlation_method == "pearson") {
                    shapiro_x <- tryCatch(shapiro.test(x)$p.value, error = function(e) 1)
                    shapiro_y <- tryCatch(shapiro.test(y)$p.value, error = function(e) 1)

                    if (shapiro_x < 0.05 || shapiro_y < 0.05) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'nonNormalDistribution',
                            type = jmvcore::NoticeType$WARNING
                        )
                        notice$setContent("Non-normal distribution detected. Pearson correlation assumes bivariate normality. Switch to Spearman's correlation (in Statistical Overlays options) for non-normal data.")
                # REMOVED:                         self$results$insert(1, notice)  # Causes serialization error
                    }
                } else if (self$options$correlation_method == "spearman") {
                    # Inform user that Spearman is appropriate for non-normal data
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'spearmanMethodInfo',
                        type = jmvcore::NoticeType$INFO
                    )
                    notice$setContent("Using Spearman's rank correlation (non-parametric). This method is robust to outliers and does not assume normality.")
                # REMOVED:                     self$results$insert(999, notice)  # Causes serialization error
                }

                # CRITICAL: Exploratory statistics disclaimer
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'exploratoryStatistics',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                notice$setContent('Correlation and R² displayed on plot are EXPLORATORY estimates only. These do NOT constitute formal hypothesis tests and should not be used for clinical decision-making without proper statistical validation.')
                # REMOVED:                 self$results$insert(1, notice)  # Causes serialization error
            }
        },

        .run = function() {
            # Initialize warnings list (avoid Notice serialization errors)
            private$.warnings <- list()

            # Clear instructions if analysis is ready
            if (!is.null(self$options$x_var) && self$options$plot_type != "") {
                self$results$instructions$setContent("")
            }

            # Early validation
            if (is.null(self$options$x_var) && !(self$options$plot_type %in% c("pairs", "matplot"))) {
                return()
            }

            # Data validation
            if (nrow(self$data) == 0) {
                stop("Data contains no (complete) rows")
            }

            # Store initial data for comparison
            initial_data <- self$data

            # Validate variable names before processing
            private$.validateVariableNames(initial_data)

            # Process data
            private$.processed_data <- private$.process_data()

            # Check sample size and report data loss
            private$.checkSampleSize(initial_data, private$.processed_data)

            # Validate data types for plot appropriateness
            private$.validateDataTypes()

            # Prepare plot data
            private$.prepare_plot_data()

            # Validate statistics if they will be displayed
            if (self$options$show_statistics) {
                if (self$options$plot_type %in% c("scatter", "line") && !is.null(private$.plot_data$y)) {
                    # Validate statistics and display notices (method inserts notices directly)
                    private$.validateStatistics(private$.plot_data$x, private$.plot_data$y)
                }
            }

            # Set plot state for rendering
            # Extract only plain option values to avoid serialization errors
            # DO NOT store self$options directly - it contains function references
            plot_options <- list(
                plot_type = self$options$plot_type,
                main_title = self$options$main_title,
                x_label = self$options$x_label,
                y_label = self$options$y_label,
                color_scheme = self$options$color_scheme,
                point_type = self$options$point_type,
                point_size = self$options$point_size,
                bins = self$options$bins,
                add_legend = self$options$add_legend,
                custom_limits = self$options$custom_limits,
                x_min = self$options$x_min,
                x_max = self$options$x_max,
                y_min = self$options$y_min,
                y_max = self$options$y_max,
                line_type = self$options$line_type,
                line_width = self$options$line_width,
                bar_width = self$options$bar_width,
                add_grid = self$options$add_grid,
                grid_style = self$options$grid_style,
                show_statistics = self$options$show_statistics,
                correlation_method = self$options$correlation_method,
                show_regression_line = self$options$show_regression_line,
                regression_line_color = self$options$regression_line_color,
                show_confidence_interval = self$options$show_confidence_interval
            )

            self$results$base_plot$setState(list(
                data = private$.plot_data,
                options = plot_options
            ))

            # Generate plot description
            private$.generate_description()

            # Generate statistical glossary if statistics are shown
            if (self$options$show_statistics) {
                private$.generate_glossary()
            }

            # Generate plain-language summary if requested
            if (self$options$show_statistics && self$options$show_summary) {
                private$.generate_summary()
            }
        },
        
        .process_data = function() {
            mydata <- self$data
            
            # Store original names and labels
            if (is.null(colnames(mydata))) {
                colnames(mydata) <- paste0("V", seq_len(ncol(mydata)))
            }
            original_names <- colnames(mydata)
            labels <- setNames(original_names, original_names)
            
            # Clean variable names
            mydata <- mydata %>% janitor::clean_names()
            
            # Restore labels to cleaned names
            if (length(original_names) == length(colnames(mydata))) {
                corrected_labels <- setNames(original_names, colnames(mydata))
                mydata <- labelled::set_variable_labels(.data = mydata, .labels = corrected_labels)
            }
            
            # Remove missing values for selected variables
            required_vars <- c()
            if (!is.null(self$options$x_var)) required_vars <- c(required_vars, self$options$x_var)
            if (!is.null(self$options$y_var)) required_vars <- c(required_vars, self$options$y_var)
            if (!is.null(self$options$group_var)) required_vars <- c(required_vars, self$options$group_var)
            
            # Convert to cleaned names
            if (length(required_vars) > 0) {
                required_vars_clean <- sapply(required_vars, private$.escapeVariableName)
                mydata <- mydata[complete.cases(mydata[required_vars_clean]), , drop = FALSE]
            } else {
                mydata <- mydata[complete.cases(mydata), , drop = FALSE]
            }
            
            return(mydata)
        },
        
        .prepare_plot_data = function() {
            mydata <- private$.processed_data
            
            plot_data <- list()

            if (!is.null(self$options$x_var)) {
                # Convert variable names to cleaned versions using utility
                x_var_clean <- private$.escapeVariableName(self$options$x_var)
                plot_data$x <- mydata[[x_var_clean]]
                plot_data$x_name <- self$options$x_var
            }

            # Add Y variable if specified
            if (!is.null(self$options$y_var)) {
                y_var_clean <- private$.escapeVariableName(self$options$y_var)
                plot_data$y <- mydata[[y_var_clean]]
                plot_data$y_name <- self$options$y_var
            }

            # Add grouping variable if specified
            if (!is.null(self$options$group_var)) {
                group_var_clean <- private$.escapeVariableName(self$options$group_var)
                group_col <- mydata[[group_var_clean]]

                # Handle labelled factors properly
                if (inherits(group_col, "haven_labelled")) {
                    # Convert haven_labelled to factor with labels
                    plot_data$group <- haven::as_factor(group_col)
                } else if (is.factor(group_col)) {
                    # Already a factor, use as-is
                    plot_data$group <- group_col
                } else {
                    # Convert to factor
                    plot_data$group <- as.factor(group_col)
                }

                plot_data$group_name <- self$options$group_var
            }
            
            private$.plot_data <- plot_data
        },
        
        .generate_description = function() {
            plot_type <- self$options$plot_type
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            group_var <- self$options$group_var
            
            # Create description based on plot type
            desc_html <- "<h4>Base R Graphics Plot Description</h4>"
            desc_html <- paste0(desc_html, "<div style='background-color: #f5f5f5; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
            
            if (plot_type == "scatter") {
                desc_html <- paste0(desc_html, "<p><strong>Scatter Plot:</strong> Shows the relationship between ", x_var)
                if (!is.null(y_var)) {
                    desc_html <- paste0(desc_html, " and ", y_var)
                }
                desc_html <- paste0(desc_html, ".</p>")
            } else if (plot_type == "histogram") {
                desc_html <- paste0(desc_html, "<p><strong>Histogram:</strong> Shows the distribution of ", x_var, ".</p>")
            } else if (plot_type == "boxplot") {
                desc_html <- paste0(desc_html, "<p><strong>Box Plot:</strong> Shows the distribution of ", x_var)
                if (!is.null(group_var)) {
                    desc_html <- paste0(desc_html, " grouped by ", group_var)
                }
                desc_html <- paste0(desc_html, ".</p>")
            } else if (plot_type == "barplot") {
                desc_html <- paste0(desc_html, "<p><strong>Bar Plot:</strong> Shows frequencies/counts for ", x_var, ".</p>")
            } else if (plot_type == "density") {
                desc_html <- paste0(desc_html, "<p><strong>Density Plot:</strong> Shows smooth density estimation for ", x_var, ".</p>")
            } else if (plot_type == "line") {
                desc_html <- paste0(desc_html, "<p><strong>Line Plot:</strong> Shows trend lines for the data.</p>")
            } else if (plot_type == "pairs") {
                desc_html <- paste0(desc_html, "<p><strong>Pairs Plot:</strong> Shows pairwise relationships between variables.</p>")
            } else if (plot_type == "matplot") {
                desc_html <- paste0(desc_html, "<p><strong>Matrix Plot:</strong> Shows multiple data series on the same plot.</p>")
            }
            
            if (!is.null(group_var)) {
                desc_html <- paste0(desc_html, "<p><strong>Grouping:</strong> Data is grouped by ", group_var, " with different colors/symbols.</p>")
            }
            
            desc_html <- paste0(desc_html, "<p><strong>Graphics Engine:</strong> Pure base R graphics (no external dependencies)</p>")
            desc_html <- paste0(desc_html, "<p><strong>Performance:</strong> Blazing fast rendering for large datasets</p>")
            desc_html <- paste0(desc_html, "</div>")
            
            self$results$plot_description$setContent(desc_html)
        },

        .generate_glossary = function() {
            # Generate statistical glossary for terms used in this module
            glossary_html <- "<h4>Statistical Glossary</h4>"
            glossary_html <- paste0(glossary_html, "<div style='background-color: #f9f9f9; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
            glossary_html <- paste0(glossary_html, "<dl style='margin: 0;'>")

            # Pearson correlation
            glossary_html <- paste0(glossary_html, "<dt><strong>Pearson Correlation (r)</strong></dt>")
            glossary_html <- paste0(glossary_html, "<dd style='margin-left: 20px; margin-bottom: 10px;'>")
            glossary_html <- paste0(glossary_html, "Measures linear relationship strength between two continuous variables. ")
            glossary_html <- paste0(glossary_html, "Range: -1 to +1. Values near 0 indicate weak relationship; values near ±1 indicate strong relationship. ")
            glossary_html <- paste0(glossary_html, "Assumes bivariate normality and is sensitive to outliers.")
            glossary_html <- paste0(glossary_html, "</dd>")

            # Spearman correlation
            glossary_html <- paste0(glossary_html, "<dt><strong>Spearman Correlation (ρ)</strong></dt>")
            glossary_html <- paste0(glossary_html, "<dd style='margin-left: 20px; margin-bottom: 10px;'>")
            glossary_html <- paste0(glossary_html, "Non-parametric measure of monotonic relationship strength based on ranked data. ")
            glossary_html <- paste0(glossary_html, "Range: -1 to +1. Does not assume normality and is robust to outliers. ")
            glossary_html <- paste0(glossary_html, "Appropriate for ordinal data or non-normal distributions.")
            glossary_html <- paste0(glossary_html, "</dd>")

            # R-squared
            glossary_html <- paste0(glossary_html, "<dt><strong>R² (Coefficient of Determination)</strong></dt>")
            glossary_html <- paste0(glossary_html, "<dd style='margin-left: 20px; margin-bottom: 10px;'>")
            glossary_html <- paste0(glossary_html, "Proportion of variance in Y explained by X in linear regression. ")
            glossary_html <- paste0(glossary_html, "Range: 0 to 1. Higher values indicate better model fit. ")
            glossary_html <- paste0(glossary_html, "R² = 0.70 means 70% of variance is explained by the model.")
            glossary_html <- paste0(glossary_html, "</dd>")

            # Mean
            glossary_html <- paste0(glossary_html, "<dt><strong>Mean</strong></dt>")
            glossary_html <- paste0(glossary_html, "<dd style='margin-left: 20px; margin-bottom: 10px;'>")
            glossary_html <- paste0(glossary_html, "Arithmetic average of all values. Sensitive to extreme values (outliers).")
            glossary_html <- paste0(glossary_html, "</dd>")

            # Median
            glossary_html <- paste0(glossary_html, "<dt><strong>Median</strong></dt>")
            glossary_html <- paste0(glossary_html, "<dd style='margin-left: 20px; margin-bottom: 10px;'>")
            glossary_html <- paste0(glossary_html, "Middle value when data is ordered. Robust to outliers; preferred for skewed distributions.")
            glossary_html <- paste0(glossary_html, "</dd>")

            # Standard deviation
            glossary_html <- paste0(glossary_html, "<dt><strong>Standard Deviation (SD)</strong></dt>")
            glossary_html <- paste0(glossary_html, "<dd style='margin-left: 20px; margin-bottom: 10px;'>")
            glossary_html <- paste0(glossary_html, "Measure of data spread around the mean. ")
            glossary_html <- paste0(glossary_html, "About 68% of values fall within ±1 SD, 95% within ±2 SD (for normal distributions).")
            glossary_html <- paste0(glossary_html, "</dd>")

            # Sample size
            glossary_html <- paste0(glossary_html, "<dt><strong>Sample Size (n)</strong></dt>")
            glossary_html <- paste0(glossary_html, "<dd style='margin-left: 20px; margin-bottom: 10px;'>")
            glossary_html <- paste0(glossary_html, "Number of observations in the analysis. ")
            glossary_html <- paste0(glossary_html, "Minimum n=30 recommended for stable correlation estimates.")
            glossary_html <- paste0(glossary_html, "</dd>")

            glossary_html <- paste0(glossary_html, "</dl></div>")

            self$results$glossary$setContent(glossary_html)
            self$results$glossary$setVisible(TRUE)
        },

        .generate_summary = function() {
            # Generate plain-language summary of statistical findings
            # Only applicable for bivariate plots with correlation
            if (!self$options$plot_type %in% c("scatter", "line") || is.null(private$.plot_data$y)) {
                return()
            }

            # Calculate correlation using selected method
            cor_method <- self$options$correlation_method
            cor_value <- cor(private$.plot_data$x, private$.plot_data$y, use = "complete.obs", method = cor_method)

            # Determine correlation strength
            abs_cor <- abs(cor_value)
            strength <- if (abs_cor < 0.3) {
                "weak"
            } else if (abs_cor < 0.7) {
                "moderate"
            } else {
                "strong"
            }

            # Determine direction
            direction <- if (cor_value > 0) {
                "positive"
            } else {
                "negative"
            }

            # Build summary
            summary_html <- "<h4>Plain-Language Summary</h4>"
            summary_html <- paste0(summary_html, "<div style='background-color: #e8f4f8; padding: 15px; border-radius: 5px; margin: 10px 0;'>")

            # Main interpretation
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            summary_html <- paste0(summary_html, "<p><strong>Relationship Interpretation:</strong></p>")
            summary_html <- paste0(summary_html, "<p>There is a <strong>", strength, " ", direction, " relationship</strong> between ", x_var, " and ", y_var, ". ")

            # Explain what this means
            if (direction == "positive") {
                summary_html <- paste0(summary_html, "This means that as ", x_var, " increases, ", y_var, " tends to increase as well. ")
            } else {
                summary_html <- paste0(summary_html, "This means that as ", x_var, " increases, ", y_var, " tends to decrease. ")
            }

            # Quantify the strength
            if (cor_method == "pearson") {
                r_squared <- cor_value^2
                pct_explained <- round(r_squared * 100, 1)
                summary_html <- paste0(summary_html, "About ", pct_explained, "% of the variation in ", y_var, " can be explained by ", x_var, ".")
            } else {
                summary_html <- paste0(summary_html, "The relationship is ", strength, " when considering the ranked values (not necessarily linear).")
            }
            summary_html <- paste0(summary_html, "</p>")

            # Provide context on strength
            summary_html <- paste0(summary_html, "<p><strong>Context:</strong></p><ul style='margin-top: 5px;'>")
            if (strength == "weak") {
                summary_html <- paste0(summary_html, "<li>", x_var, " and ", y_var, " show little association. Other factors likely play larger roles.</li>")
            } else if (strength == "moderate") {
                summary_html <- paste0(summary_html, "<li>", x_var, " and ", y_var, " show a meaningful association, but other factors also contribute.</li>")
            } else {
                summary_html <- paste0(summary_html, "<li>", x_var, " and ", y_var, " are closely related. Changes in one strongly predict changes in the other.</li>")
            }

            # Method-specific notes
            if (cor_method == "spearman") {
                summary_html <- paste0(summary_html, "<li>Using Spearman's method (rank-based), which is appropriate for non-normal data or ordinal variables.</li>")
            } else {
                summary_html <- paste0(summary_html, "<li>Using Pearson's method, which assumes a linear relationship and normally distributed data.</li>")
            }
            summary_html <- paste0(summary_html, "</ul>")

            # Important reminder
            summary_html <- paste0(summary_html, "<p style='margin-top: 10px; padding: 10px; background-color: #fff3cd; border-left: 4px solid #ffc107;'>")
            summary_html <- paste0(summary_html, "<strong>⚠️ Important:</strong> This is an <em>exploratory</em> analysis. ")
            summary_html <- paste0(summary_html, "Correlation does not prove causation. Formal hypothesis testing is needed before using these findings for clinical decisions.")
            summary_html <- paste0(summary_html, "</p>")

            summary_html <- paste0(summary_html, "</div>")

            self$results$summary$setContent(summary_html)
            self$results$summary$setVisible(TRUE)
        },

        .add_statistics_to_plot = function(data, options) {
            # Add statistical information overlay to plots
            # IMPORTANT: These are EXPLORATORY statistics only, not formal statistical tests
            # Validation warnings are added in .run() method before this is called

            if (options$plot_type %in% c("scatter", "line") && !is.null(data$y)) {
                # For bivariate plots, add correlation using selected method
                cor_method <- options$correlation_method
                cor_value <- cor(data$x, data$y, use = "complete.obs", method = cor_method)

                # Label depends on method (r for Pearson, ρ for Spearman)
                cor_label <- if (cor_method == "spearman") "\u03C1" else "r"
                stats_text <- paste0(cor_label, " = ", round(cor_value, 3), " (", cor_method, ")")

                # Add regression line for scatter plots
                if (options$plot_type == "scatter") {
                    abline(lm(data$y ~ data$x), col = "red", lty = 2)
                    lm_fit <- lm(data$y ~ data$x)
                    r_squared <- round(summary(lm_fit)$r.squared, 3)
                    stats_text <- paste0(stats_text, "\nR² = ", r_squared)
                }

                # Position text in upper-left corner
                mtext(stats_text, side = 3, line = -2, adj = 0.05, cex = 0.8, col = "blue")
                
            } else if (options$plot_type == "histogram") {
                # For histograms, add summary statistics
                mean_val <- round(mean(data$x, na.rm = TRUE), 2)
                median_val <- round(median(data$x, na.rm = TRUE), 2)
                sd_val <- round(sd(data$x, na.rm = TRUE), 2)
                
                stats_text <- paste0("Mean: ", mean_val, "\nMedian: ", median_val, "\nSD: ", sd_val)
                mtext(stats_text, side = 3, line = -4, adj = 0.95, cex = 0.8, col = "blue")
                
                # Add vertical lines for mean and median
                abline(v = mean_val, col = "red", lty = 2, lwd = 2)
                abline(v = median_val, col = "blue", lty = 3, lwd = 2)
                
            } else if (options$plot_type == "boxplot") {
                # For boxplots, add sample sizes
                if (!is.null(data$group)) {
                    group_counts <- table(data$group)
                    stats_text <- paste0("n = ", paste(group_counts, collapse = ", "))
                } else {
                    stats_text <- paste0("n = ", length(data$x))
                }
                mtext(stats_text, side = 3, line = -2, adj = 0.05, cex = 0.8, col = "blue")
                
            } else if (options$plot_type == "density") {
                # For density plots, add summary statistics
                mean_val <- round(mean(data$x, na.rm = TRUE), 2)
                median_val <- round(median(data$x, na.rm = TRUE), 2)
                
                stats_text <- paste0("Mean: ", mean_val, "\nMedian: ", median_val)
                mtext(stats_text, side = 3, line = -3, adj = 0.95, cex = 0.8, col = "blue")
                
                # Add vertical lines for mean and median
                abline(v = mean_val, col = "red", lty = 2)
                abline(v = median_val, col = "blue", lty = 3)
            }
        },
        
        # Plot rendering function
        .plot_base = function(image, ggtheme, theme, ...) {
            plot_state <- image$state
            if (is.null(plot_state)) return()
            
            data <- plot_state$data
            options <- plot_state$options
            
            # Set up plot parameters
            main_title <- if (options$main_title != "") options$main_title else paste(options$plot_type, "Plot")
            x_label <- if (options$x_label != "") options$x_label else data$x_name
            y_label <- if (options$y_label != "") options$y_label else if (!is.null(data$y_name)) data$y_name else "Values"
            
            # Set up colors
            if (!is.null(data$group)) {
                n_groups <- length(levels(data$group))
                if (options$color_scheme == "rainbow") {
                    colors <- rainbow(n_groups)
                } else if (options$color_scheme == "heat") {
                    colors <- heat.colors(n_groups)
                } else if (options$color_scheme == "terrain") {
                    colors <- terrain.colors(n_groups)
                } else if (options$color_scheme == "topo") {
                    colors <- topo.colors(n_groups)
                } else if (options$color_scheme == "cm") {
                    colors <- cm.colors(n_groups)
                } else {
                    colors <- 1:n_groups
                }
            } else {
                colors <- "black"
            }
            
            tryCatch({
                # Prepare custom axis limits
                xlim <- NULL
                if (options$custom_limits && options$x_min != "" && options$x_max != "") {
                    xlim <- c(as.numeric(options$x_min), as.numeric(options$x_max))
                }
                
                ylim <- NULL
                if (options$custom_limits && options$y_min != "" && options$y_max != "") {
                    ylim <- c(as.numeric(options$y_min), as.numeric(options$y_max))
                }
                
                # Generate plot based on type
                if (options$plot_type == "scatter") {
                    if (!is.null(data$y)) {
                        if (!is.null(data$group)) {
                            plot(data$x, data$y, 
                                col = colors[data$group], 
                                pch = as.numeric(options$point_type), 
                                cex = options$point_size,
                                main = main_title, 
                                xlab = x_label, 
                                ylab = y_label,
                                xlim = xlim, ylim = ylim)
                            if (options$add_legend) {
                                legend("topright", legend = levels(data$group), col = colors, pch = as.numeric(options$point_type))
                            }
                        } else {
                            plot(data$x, data$y, 
                                col = colors, 
                                pch = as.numeric(options$point_type), 
                                cex = options$point_size,
                                main = main_title, 
                                xlab = x_label, 
                                ylab = y_label,
                                xlim = xlim, ylim = ylim)
                        }
                    } else {
                        plot(data$x, 
                            col = colors, 
                            pch = as.numeric(options$point_type), 
                            cex = options$point_size,
                            main = main_title, 
                            xlab = x_label, 
                            ylab = "Index",
                            xlim = xlim, ylim = ylim)
                    }
                } else if (options$plot_type == "histogram") {
                    hist(data$x, 
                        breaks = options$bins,
                        main = main_title, 
                        xlab = x_label, 
                        ylab = "Frequency",
                        col = if (length(colors) == 1) colors else colors[1],
                        xlim = xlim, ylim = ylim)
                } else if (options$plot_type == "boxplot") {
                    if (!is.null(data$group)) {
                        boxplot(data$x ~ data$group, 
                               main = main_title, 
                               xlab = data$group_name, 
                               ylab = x_label,
                               col = colors,
                               ylim = ylim)  # xlim not applicable to boxplot with groups
                    } else {
                        boxplot(data$x, 
                               main = main_title, 
                               ylab = x_label,
                               col = if (length(colors) == 1) colors else colors[1],
                               ylim = ylim)
                    }
                } else if (options$plot_type == "barplot") {
                    if (is.factor(data$x) || is.character(data$x)) {
                        tab <- table(data$x)
                        barplot(tab, 
                               main = main_title, 
                               xlab = x_label, 
                               ylab = "Frequency",
                               col = colors,
                               ylim = ylim)  # xlim not meaningful for barplot
                    } else {
                        barplot(data$x, 
                               main = main_title, 
                               xlab = "Index", 
                               ylab = x_label,
                               col = colors,
                               ylim = ylim)
                    }
                } else if (options$plot_type == "density") {
                    if (!is.null(data$group)) {
                        groups <- levels(data$group)
                        first_group <- TRUE
                        for (i in seq_along(groups)) {
                            group_data <- data$x[data$group == groups[i]]
                            if (length(group_data) > 1) {
                                d <- density(group_data, na.rm = TRUE)
                                if (first_group) {
                                    plot(d, main = main_title, xlab = x_label, ylab = "Density", 
                                         col = colors[i], xlim = xlim, ylim = ylim)
                                    first_group <- FALSE
                                } else {
                                    lines(d, col = colors[i])
                                }
                            }
                        }
                        if (options$add_legend) {
                            legend("topright", legend = groups, col = colors, lty = 1)
                        }
                    } else {
                        d <- density(data$x, na.rm = TRUE)
                        plot(d, main = main_title, xlab = x_label, ylab = "Density", 
                             col = colors, xlim = xlim, ylim = ylim)
                    }
                } else if (options$plot_type == "line") {
                    if (!is.null(data$y)) {
                        plot(data$x, data$y, type = "l", 
                            main = main_title, 
                            xlab = x_label, 
                            ylab = y_label,
                            col = colors, xlim = xlim, ylim = ylim)
                    } else {
                        plot(data$x, type = "l", 
                            main = main_title, 
                            xlab = "Index", 
                            ylab = x_label,
                            col = colors, xlim = xlim, ylim = ylim)
                    }
                } else if (options$plot_type == "pairs") {
                    # Pairs plot for multiple variable relationships
                    mydata <- private$.processed_data
                    
                    # Select numeric variables for pairs plot
                    numeric_vars <- sapply(mydata, is.numeric)
                    if (sum(numeric_vars) < 2) {
                        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
                        text(1, 1, "Pairs plot requires at least 2 numeric variables\nPlease add more numeric variables to your dataset", 
                             cex = 1.2, col = "red")
                    } else {
                        numeric_data <- mydata[, numeric_vars, drop = FALSE]
                        
                        # Create pairs plot with customization
                        if (!is.null(data$group)) {
                            # Color by group
                            pairs(numeric_data, 
                                 main = main_title,
                                 col = colors[data$group],
                                 pch = as.numeric(options$point_type),
                                 cex = options$point_size)
                            
                            # Add legend in corner
                            if (options$add_legend) {
                                # Get plot coordinates for legend placement
                                par(xpd = TRUE)
                                legend(0.85, 0.95, legend = levels(data$group), 
                                      col = colors, pch = as.numeric(options$point_type),
                                      bty = "n", cex = 0.8)
                                par(xpd = FALSE)
                            }
                        } else {
                            pairs(numeric_data, 
                                 main = main_title,
                                 col = colors,
                                 pch = as.numeric(options$point_type),
                                 cex = options$point_size)
                        }
                    }
                } else if (options$plot_type == "matplot") {
                    # Matrix plot for multiple data series
                    mydata <- private$.processed_data
                    
                    # Select numeric variables for matrix plot
                    numeric_vars <- sapply(mydata, is.numeric)
                    if (sum(numeric_vars) < 2) {
                        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
                        text(1, 1, "Matrix plot requires at least 2 numeric variables\nPlease add more numeric variables to your dataset", 
                             cex = 1.2, col = "red")
                    } else {
                        numeric_data <- as.matrix(mydata[, numeric_vars, drop = FALSE])
                        
                        # Apply custom limits if specified
                        xlim <- if (options$custom_limits && !is.null(options$x_min) && !is.null(options$x_max)) {
                            c(options$x_min, options$x_max)
                        } else NULL
                        ylim <- if (options$custom_limits && !is.null(options$y_min) && !is.null(options$y_max)) {
                            c(options$y_min, options$y_max)
                        } else NULL
                        
                        # Create matrix plot
                        matplot(numeric_data, 
                               type = "l",
                               main = main_title,
                               xlab = x_label,
                               ylab = y_label,
                               col = colors,
                               lty = 1:ncol(numeric_data),
                               xlim = xlim, ylim = ylim)
                        
                        # Add legend
                        if (options$add_legend) {
                            legend("topright", legend = colnames(numeric_data), 
                                  col = colors, lty = 1:ncol(numeric_data), bty = "n")
                        }
                    }
                }
                
                # Add grid if requested
                if (options$add_grid) {
                    grid()
                }
                
                # Add statistics if requested
                if (options$show_statistics) {
                    private$.add_statistics_to_plot(data, options)
                }
                
                TRUE
            }, error = function(e) {
                plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
                text(1, 1, paste("Error generating plot:\n", e$message), cex = 1.2, col = "red")
                TRUE
            })
        }
    )
)
