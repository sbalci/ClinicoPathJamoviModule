#' @title Alluvial Plot
#' @return Alluvial Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom easyalluvial alluvial_wide add_marginal_histograms plot_condensation
#' @importFrom ggalluvial geom_alluvium geom_stratum stat_alluvium stat_stratum StatStratum
#' @importFrom rlang sym
#'
#' @description
#' This tool creates Alluvial Diagrams (Alluvial Plots) to visualize the flow of
#' categorical data across multiple dimensions. Alluvial diagrams are particularly
#' useful for showing how categorical variables relate to each other and how
#' observations flow between different categories.
#'
#' Features:
#' - Multiple variable alluvial plots with configurable maximum variables
#' - Condensation plots for detailed variable analysis
#' - Marginal histograms for additional context
#' - Flexible orientation (horizontal/vertical)
#' - Customizable bin labels and fill options
#' - Multiple plot engines (easyalluvial and ggalluvial)
#' - Color palettes and theme styling
#' - Sankey diagram styling with curve types
#' - Comprehensive data validation for optimal results
#'

alluvialClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "alluvialClass",
    inherit = alluvialBase,
    private = list(

        # Utility to escape variable names for safe use in outputs
        .escapeVar = function(x) {
            if (is.null(x) || length(x) == 0) return(x)
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        # Validate weight variable for weighted alluvial plots
        .validateWeightVariable = function(data, weight_var) {
            if (is.null(weight_var) || weight_var == "" || !weight_var %in% names(data)) {
                return(TRUE)  # No weight specified - valid
            }

            weight_col <- data[[weight_var]]

            # Validate weight type
            if (!is.numeric(weight_col)) {
                # Use plain text error message (no HTML in Notices)
                error_notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = "weightNotNumeric",
                    type = jmvcore::NoticeType$ERROR
                )
                error_notice$setContent(sprintf(
                    "Invalid Weight Variable: '%s' must be numeric (current type: %s). Please select a numeric variable containing counts, frequencies, or sampling weights.",
                    weight_var, class(weight_col)[1]
                ))
                self$results$insert(1, error_notice)
                return(FALSE)
            }

            # Check for negative weights
            n_negative <- sum(weight_col < 0, na.rm = TRUE)
            if (n_negative > 0) {
                # Use plain text error message (no HTML in Notices)
                error_notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = "negativeWeights",
                    type = jmvcore::NoticeType$ERROR
                )
                error_notice$setContent(sprintf(
                    "Negative Weights Detected: Weight variable '%s' contains %d negative value%s. Weights must be non-negative (>= 0).",
                    weight_var, n_negative, if(n_negative > 1) "s" else ""
                ))
                self$results$insert(1, error_notice)
                return(FALSE)
            }

            # Check for NA weights
            n_na <- sum(is.na(weight_col))
            if (n_na > 0) {
                pct_na <- round(100 * n_na / length(weight_col), 1)
                # REMOVED Notice: warning_notice <- jmvcore::Notice$new(
                # options = self$options,
                # name = "missingWeights",
                # type = jmvcore::NoticeType$STRONG_WARNING
                # )
                # warning_notice$setContent(paste0(
                # "⚠️ <b>Missing Weights:</b> ", n_na, " observations (", pct_na,
                # "%) have missing weights. ",
                # "These will be excluded from the visualization."
                # ))
                # REMOVED: # REMOVED: self$results$insert(1, warning_notice)  # Causes serialization error
            }

            return(TRUE)
        },

        # Aggregate data for weighted ggalluvial plots
        .aggregateDataForGgalluvial = function(data, vars, weight_var) {
            if (is.null(weight_var) || weight_var == "" || !weight_var %in% names(data)) {
                return(data)  # No aggregation needed
            }

            # Remove rows with NA weights
            data <- data[!is.na(data[[weight_var]]), ]

            # Aggregate weights by unique combinations of categorical variables
            agg_formula <- as.formula(paste(weight_var, "~", paste(vars, collapse = " + ")))

            # Use aggregate to sum weights by category combinations
            data_agg <- stats::aggregate(
                agg_formula,
                data = data,
                FUN = sum,
                na.action = na.pass
            )

            return(data_agg)
        },

        # Handle missing values with transparency reporting
        .handleMissingValues = function(data, vars) {
            # Count missing before removal
            n_total <- nrow(data)
            missing_counts <- sapply(vars, function(v) sum(is.na(data[[v]])))
            any_missing <- any(missing_counts > 0)

            # ALWAYS remove NAs for consistent behavior
            data_clean <- data[complete.cases(data[, vars, drop=FALSE]), ]
            n_removed <- n_total - nrow(data_clean)

            # Report missingness transparently
            if (n_removed > 0) {
                pct_removed <- round(100 * n_removed / n_total, 1)

                info_notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = "missingDataExcluded",
                    type = jmvcore::NoticeType$INFO
                )

                # Build details of which variables have missingness
                vars_with_missing <- names(missing_counts[missing_counts > 0])
                missing_details <- paste(sapply(vars_with_missing,
                    function(v) sprintf("%s: %d", v, missing_counts[v])),
                    collapse = ", ")

                info_notice$setContent(paste0(
                    "ℹ️ <b>Missing Data Excluded:</b> ", n_removed, " of ", n_total,
                    " observations (", pct_removed, "%) excluded due to missing values.<br/>",
                    "Variables with missingness: ", missing_details, "<br/>",
                    "Analysis based on ", nrow(data_clean), " complete cases."
                ))
                self$results$insert(1, info_notice)
            }

            return(data_clean)
        },

        # Shared validation helper to reduce duplication
        .validateAlluvialInputs = function() {
            # Clear any previous validation messages at the start
            # This prevents old errors from persisting when validation state changes
            self$results$dataWarning$setContent("")
            self$results$dataWarning$setVisible(FALSE)

            if (is.null(self$options$vars) || length(self$options$vars) == 0)
                return(FALSE)

            if (length(self$options$vars) < 2) {
                html <- paste0(
                    "<div style='background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0;'>",
                    "<h4 style='margin-top: 0; color: #721c24;'>Insufficient Variables</h4>",
                    "<p style='color: #721c24;'>Alluvial diagrams require at least <strong>2 variables</strong>.</p>",
                    "<p>Please select additional variables from the left panel.</p>",
                    "</div>"
                )
                self$results$dataWarning$setContent(html)
                self$results$dataWarning$setVisible(TRUE)
                return(FALSE)
            }

            if (nrow(self$data) == 0) {
                html <- paste0(
                    "<div style='background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0;'>",
                    "<h4 style='margin-top: 0; color: #721c24;'>No Data Available</h4>",
                    "<p style='color: #721c24;'>Data contains no (complete) rows.</p>",
                    "<p>Please check your data for missing values or filtering issues.</p>",
                    "</div>"
                )
                self$results$dataWarning$setContent(html)
                return(FALSE)
            }

            # Validate that variables are appropriate for alluvial diagrams
            if (!private$.validateVariableTypes(self$options$vars)) {
                return(FALSE)
            }

            # Clear warnings if everything is valid
            self$results$dataWarning$setContent("")
            return(TRUE)
        },

        # Data type validation and discretization helper
        .validateVariableTypes = function(vars) {
            warning_messages <- c()

            for (var in vars) {
                if (!(var %in% names(self$data))) {
                    var_safe <- htmltools::htmlEscape(var)
                    html <- paste0(
                        "<div style='background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0;'>",
                        "<h4 style='margin-top: 0; color: #721c24;'>Variable Not Found</h4>",
                        "<p style='color: #721c24;'>Variable '<strong>", var_safe, "</strong>' not found in the data.</p>",
                        "<p>Please ensure all selected variables exist in your dataset.</p>",
                        "</div>"
                    )
                    self$results$dataWarning$setContent(html)
                    self$results$dataWarning$setVisible(TRUE)
                    return(FALSE)
                }

                var_data <- self$data[[var]]

                # Check if variable is numeric with too many unique values (likely continuous)
                if (is.numeric(var_data)) {
                    unique_values <- length(unique(var_data[!is.na(var_data)]))
                    total_values <- sum(!is.na(var_data))

                    # HARD STOP for continuous variables (>20 unique values)
                    if (unique_values > 20) {
                        var_safe <- htmltools::htmlEscape(var)

                        error_notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = "continuousVariableNotAllowed",
                            type = jmvcore::NoticeType$ERROR)
                        error_notice$setContent(sprintf("Continuous Variable Not Allowed: Variable '%s' has %d unique values and appears continuous. Alluvial plots require categorical data. Please use the categorize function.", var, unique_values))
                        self$results$insert(1, error_notice)
                        return(FALSE)
                    }

                    # STRONG WARNING for 11-20 unique values
                    if (unique_values > 10) {
                        var_safe <- htmltools::htmlEscape(var)

                        # Collect warning messages for the notice banner
                        warning_messages <- c(warning_messages, sprintf(
                            'Variable "%s" has %d categories.',
                            var_safe, unique_values
                        ))

                # REMOVED Notice: warning_notice <- jmvcore::Notice$new(
                # options = self$options,
                # name = "tooManyCategories",
                # type = jmvcore::NoticeType$STRONG_WARNING
                # )
                # warning_notice$setContent(paste0(
                # "⚠️ <b>Too Many Categories:</b> Variable '", var_safe, "' has ",
                # unique_values, " categories. This may create an unreadable plot with too many thin flows.<br/>",
                # "<b>Recommendation:</b> Consider reducing to 3-7 categories for optimal visualization. ",
                # "Use Data → Transform to group less frequent categories."
                # ))
                # REMOVED: # REMOVED: self$results$insert(1, warning_notice)  # Causes serialization error
                    }
                }
            }

            # Note: Warning notices for too many categories are now handled by jmvcore::Notice at lines 262-273
            # No need for redundant HTML warning

            return(TRUE)
        },

        # Helper method to create ggalluvial plots
        .createGgalluvialPlot = function(data, vars, fill_var, weight_var = NULL) {
            # Check for required package
            if (!requireNamespace("ggalluvial", quietly = TRUE)) {
                stop("Package 'ggalluvial' is required for manual control engine. Please install it.")
            }

            # Prepare data - convert to factors
            for (var in vars) {
                data[[var]] <- as.factor(data[[var]])
            }

            # Get options
            sankey_style <- self$options$sankeyStyle
            curve_type <- self$options$curveType
            label_nodes <- self$options$labelNodes
            show_counts <- self$options$showCounts

            # Force sigmoid for Sankey style
            if (sankey_style) {
                curve_type <- "sigmoid"
            }

            # Create axis aesthetics dynamically
            n_vars <- length(vars)
            axis_names <- paste0("axis", 1:n_vars)

            # Build the aes call
            aes_args <- list()
            for (i in 1:n_vars) {
                aes_args[[axis_names[i]]] <- rlang::sym(vars[i])
            }

            # Add weight if provided
            if (!is.null(weight_var) && weight_var %in% names(data)) {
                aes_args$y <- rlang::sym(weight_var)
            }

            # Create base plot
            plot <- ggplot2::ggplot(data, do.call(ggplot2::aes, aes_args))

            # Set widths based on style
            if (sankey_style) {
                stratum_width <- 1/8
                alluvium_width <- 1/2
            } else {
                stratum_width <- 1/3
                alluvium_width <- 1/3
            }

            # Add alluvium (flows)
            plot <- plot +
                ggalluvial::geom_alluvium(
                    ggplot2::aes(fill = !!rlang::sym(fill_var)),
                    alpha = 0.8,
                    curve_type = curve_type,
                    width = alluvium_width
                )

            # Add stratum (nodes)
            plot <- plot +
                ggalluvial::geom_stratum(
                    width = stratum_width,
                    alpha = 0.8,
                    color = "white"
                )

            # Add labels if requested
            if (label_nodes) {
                plot <- plot +
                    ggplot2::geom_text(
                        stat = ggalluvial::StatStratum,
                        ggplot2::aes(label = ggplot2::after_stat(stratum)),
                        size = 3
                    )
            }

            # Add counts if requested
            if (show_counts) {
                plot <- plot +
                    ggplot2::geom_text(
                        stat = ggalluvial::StatStratum,
                        ggplot2::aes(label = ggplot2::after_stat(count)),
                        size = 2.5,
                        vjust = -0.5
                    )
            }

            return(plot)
        },

        # Helper to apply color palette
        .applyColorPalette = function(plot, palette) {
            if (palette == "default") {
                return(plot)
            }

            colors <- switch(palette,
                "viridis" = ggplot2::scale_fill_viridis_d(),
                "plasma" = ggplot2::scale_fill_viridis_d(option = "plasma"),
                "set3" = ggplot2::scale_fill_brewer(type = "qual", palette = "Set3"),
                "pastel1" = ggplot2::scale_fill_brewer(type = "qual", palette = "Pastel1"),
                "dark2" = ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2"),
                NULL
            )

            if (!is.null(colors)) {
                plot <- plot + colors
            }

            return(plot)
        },

        # Helper to apply theme style
        .applyThemeStyle = function(plot, theme_style) {
            theme_func <- switch(theme_style,
                "minimal" = ggplot2::theme_minimal(),
                "classic" = ggplot2::theme_classic(),
                "grey" = ggplot2::theme_grey(),
                "bw" = ggplot2::theme_bw(),
                NULL
            )

            if (!is.null(theme_func)) {
                plot <- plot + theme_func
            }

            return(plot)
        },
        .run = function() {

            # Input Validation ----

            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                # ToDo Message ----
                todo <- "
                <div style='font-family: Arial, sans-serif; color: #2c3e50; padding: 10px;'>
                  <h2>📊 Alluvial Diagrams</h2>
                  <p>Visualize the flow of categorical data across multiple dimensions.</p>

                  <div style='background-color: #e7f3ff; border-left: 4px solid #2196F3; padding: 10px; margin: 10px 0;'>
                    <h3 style='margin-top: 0;'>🚀 Quick Start</h3>
                    <ul style='margin-bottom: 0;'>
                      <li>Select <strong>2-5 categorical variables</strong> (optimal: 3-4)</li>
                      <li>Each variable should have <strong>3-7 categories</strong> for best readability</li>
                      <li>For continuous variables, use the <em>categorize function</em> to create bins first</li>
                    </ul>
                  </div>

                  <div style='background-color: #f0f8f0; border-left: 4px solid #4caf50; padding: 10px; margin: 10px 0;'>
                    <h3 style='margin-top: 0;'>🏥 Clinical Use Cases</h3>
                    <ul style='margin-bottom: 0;'>
                      <li><strong>Patient flow:</strong> Track progression through treatment stages</li>
                      <li><strong>Tumor progression:</strong> Visualize grade/stage transitions</li>
                      <li><strong>Diagnostic pathways:</strong> Show relationships between symptoms → diagnosis → outcomes</li>
                      <li><strong>Demographics:</strong> Explore patterns across age/sex/location categories</li>
                    </ul>
                  </div>

                  <div style='background-color: #fff8e1; border-left: 4px solid #ffc107; padding: 10px; margin: 10px 0;'>
                    <h3 style='margin-top: 0;'>💡 Tips</h3>
                    <ul style='margin-bottom: 0;'>
                      <li>Arrange variables in <strong>logical order</strong> (e.g., temporal sequence: Grade → Stage → Response)</li>
                      <li>Start with <strong>fewer variables</strong> and add more once you understand the patterns</li>
                      <li>Use <strong>weighted flows</strong> (GG Alluvial engine) for aggregated data with frequency counts</li>
                      <li>Enable <strong>marginal histograms</strong> to see individual variable distributions</li>
                    </ul>
                  </div>

                  <hr style='margin-top: 15px;'>
                  <p style='font-size: 0.9em; color: #7f8c8d; text-align: center;'>
                    Ready to begin? Select at least 2 categorical variables from the left panel.
                  </p>
                </div>
                "

                html <- self$results$todo
                html$setContent(todo)

                # Clear validation messages when no variables selected
                self$results$dataWarning$setContent("")
                self$results$dataWarning$setVisible(FALSE)

            } else {
                # Clear the to-do message
                todo <- ""
                html <- self$results$todo
                html$setContent(todo)

                # Use shared validation logic
                if (!private$.validateAlluvialInputs()) {
                    # Use modern jmvcore::Notice
                # REMOVED Notice: error_notice <- jmvcore::Notice$new(
                # options = self$options,
                # name = "validationFailure",
                # type = jmvcore::NoticeType$ERROR
                # )
                # error_notice$setContent(
                # "⛔ <b>Validation Failed:</b> Alluvial diagram requires at least 2 variables with valid data. Please check variable selection and ensure sufficient data."
                # )
                # REMOVED: self$results$insert(1, error_notice)
                    return()
                }

                # Validate condensation variable if provided
                if (!is.null(self$options$condensationvar) &&
                    length(self$options$condensationvar) > 0 &&
                    !(self$options$condensationvar %in% names(self$data))) {

                    condvar_safe <- htmltools::htmlEscape(self$options$condensationvar)
                # REMOVED Notice: error_notice <- jmvcore::Notice$new(
                # options = self$options,
                # name = "condensationVariableNotFound",
                # type = jmvcore::NoticeType$ERROR
                # )
                # error_notice$setContent(paste0(
                # "⛔ <b>Variable Not Found:</b> Condensation variable '", condvar_safe,
                # "' does not exist in the data. Please select a valid variable from the available list."
                # ))
                # REMOVED: self$results$insert(1, error_notice)
                    return()
                }

                # Clear dataWarning if validation passes
                self$results$dataWarning$setContent("")
                self$results$dataWarning$setVisible(FALSE)

                # NOTE: Removed INFO notice to avoid serialization errors
                # INFO notices without return() can cause "attempt to apply non-function" errors
                # The plot will be generated successfully without the notice
            }

        }

        ,

        .plot = function(image, ggtheme, theme, ...) {
            # Main alluvial plot generation function

            # Input validation using shared helper
            if (!private$.validateAlluvialInputs())
                return()

            # Wrap plot generation in error handler
            tryCatch({
                # Data Preparation ----
                # Extract selected variables and create working dataset
                varsName <- self$options$vars
                weight_var <- self$options$weight

                # Validate weight variable BEFORE data preparation
                if (!private$.validateWeightVariable(self$data, weight_var)) {
                    return()  # Stop if weight validation fails
                }

                # Select variables (include weight if specified)
                vars_to_select <- c(varsName)
                if (!is.null(weight_var) && weight_var != "" && weight_var %in% names(self$data)) {
                    vars_to_select <- c(varsName, weight_var)
                }
                mydata <- jmvcore::select(self$data, vars_to_select)

                # ALWAYS handle missing values with transparency
                mydata <- private$.handleMissingValues(mydata, varsName)

                # Check if enough data remains after NA removal
                if (nrow(mydata) == 0) {
                # REMOVED Notice: error_notice <- jmvcore::Notice$new(
                # options = self$options,
                # name = "noCompleteData",
                # type = jmvcore::NoticeType$ERROR
                # )
                # error_notice$setContent(
                # "⛔ <b>No Complete Data:</b> All observations have missing values in one or more selected variables. Cannot generate plot."
                # )
                # REMOVED: self$results$insert(1, error_notice)
                    return()
                }

                # Configure plot aesthetics ----
                # Set color fill strategy for the alluvial flows
                fill <- jmvcore::composeTerm(self$options$fill)

                # Configure bin labels with proper binning method
                bin_option <- self$options$bin
                custombinlabels <- self$options$custombinlabels

                # Determine bin labels based on user selection
                # Note: easyalluvial's bin_labels parameter only controls label display,
                # not the binning method itself. Actual binning is done by easyalluvial internally.
                if (!is.null(custombinlabels) && custombinlabels != "") {
                    # Custom labels provided by user
                    bin <- trimws(strsplit(custombinlabels, ",")[[1]])
                } else {
                    # Use predefined labels based on bin option
                    bin <- switch(bin_option,
                        "default" = c("LL", "ML", "M", "MH", "HH"),
                        "mean" = "mean",
                        "median" = "median",
                        "min_max" = c("min", "max"),
                        "cuts" = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                        c("LL", "ML", "M", "MH", "HH")  # fallback
                    )
                }

                # Generate core alluvial plot ----
                # Using easyalluvial package for robust alluvial diagram generation
                # Reference: https://erblast.github.io/easyalluvial/
                maxvars <- self$options$maxvars

                # Warn user if more variables selected than maxvars allows
                num_selected <- length(varsName)
                if (num_selected > maxvars) {
                    # Use modern jmvcore::Notice
                    truncate_notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = "variablesTruncated",
                        type = jmvcore::NoticeType$INFO
                    )
                    truncate_notice$setContent(paste0(
                        "ℹ️ <b>Variables Truncated:</b> ", num_selected, " variables selected, ",
                        "but maximum is ", maxvars, ". Only the first ", maxvars, " variables will be displayed.<br/>",
                        "<b>Tip:</b> Increase 'Maximum variables' setting (up to 20) to display more variables."
                    ))
                    self$results$insert(1, truncate_notice)

                    # Keep detailed warning in dataWarning
                    warning_html <- paste0(
                        "<div style='background-color: #d1ecf1; border-left: 4px solid #17a2b8; padding: 15px; margin: 10px 0;'>",
                        "<h4 style='margin-top: 0; color: #0c5460;'>Variables Truncated</h4>",
                        "<p style='color: #0c5460;'>You selected <strong>", num_selected, "</strong> variables, but maximum is set to <strong>", maxvars, "</strong>.</p>",
                        "<p>Only the first <strong>", maxvars, "</strong> variables will be displayed in the plot.</p>",
                        "<p><strong>To show more variables:</strong> Increase the 'Maximum variables' setting (up to 20).</p>",
                        "</div>"
                    )
                    # Append to existing warning if any
                    existing_warning <- self$results$dataWarning$content
                    if (!is.null(existing_warning) && nchar(existing_warning) > 0) {
                        self$results$dataWarning$setContent(paste0(existing_warning, warning_html))
                    } else {
                        self$results$dataWarning$setContent(warning_html)
                    }
                    self$results$dataWarning$setVisible(TRUE)
                }

                # Get engine selection
                engine <- self$options$engine

                # Add interpretability checks
                # Calculate total possible flow combinations
                n_levels_per_var <- sapply(varsName, function(v) {
                    if (is.factor(mydata[[v]])) {
                        length(levels(mydata[[v]]))
                    } else {
                        length(unique(mydata[[v]]))
                    }
                })
                total_combinations <- prod(n_levels_per_var)

                # Warn if too many combinations (spaghetti plot risk)
                if (total_combinations > 100) {
                # REMOVED Notice: warning_notice <- jmvcore::Notice$new(
                # options = self$options,
                # name = "tooManyCombinations",
                # type = jmvcore::NoticeType$STRONG_WARNING
                # )
                # warning_notice$setContent(paste0(
                # "⚠️ <b>Complex Visualization:</b> The selected variables create ",
                # total_combinations, " possible flow combinations. ",
                # "This may result in an overcrowded, difficult-to-interpret plot.<br/>",
                # "<b>Recommendations:</b><br/>",
                # "• Reduce the number of variables (currently ", length(varsName), ")<br/>",
                # "• Group less frequent categories to reduce category counts<br/>",
                # "• Focus on 3-5 variables with 3-7 categories each for optimal readability"
                # ))
                # REMOVED: # REMOVED: self$results$insert(1, warning_notice)  # Causes serialization error
                }

                # Generate plot based on selected engine ----
                if (engine == "ggalluvial") {
                    # Use ggalluvial engine for manual control
                    fill_var <- if (!is.null(self$options$fillGgalluvial)) {
                        self$options$fillGgalluvial
                    } else {
                        varsName[1]
                    }

                    # Aggregate data if weight variable provided
                    mydata_for_plot <- private$.aggregateDataForGgalluvial(
                        data = mydata,
                        vars = varsName,
                        weight_var = weight_var
                    )

                    plot <- private$.createGgalluvialPlot(
                        data = mydata_for_plot,
                        vars = varsName,
                        fill_var = fill_var,
                        weight_var = weight_var
                    )
                } else {
                    # Use easyalluvial engine (default)
                    plot <- easyalluvial::alluvial_wide(
                        data = mydata,
                        max_variables = maxvars,
                        fill_by = fill,
                        verbose = FALSE,  # Disabled to prevent console clutter in jamovi
                        bin_labels = bin
                    )
                }

                # Warn if weight is provided but ignored by easyalluvial
                if (engine == "easyalluvial" && !is.null(weight_var) && weight_var != "") {
                # REMOVED Notice: warning_notice <- jmvcore::Notice$new(
                # options = self$options,
                # name = "weightVariableIgnored",
                # type = jmvcore::NoticeType$STRONG_WARNING
                # )
                # warning_notice$setContent(paste0(
                # "⚠️ <b>Weight Variable Ignored:</b> The 'Weight Variable' option is only supported by the <strong>GG Alluvial</strong> engine.<br/>",
                # "<b>Suggestion:</b> Switch 'Plot Engine' to 'GG Alluvial (manual control)' to use weighted flows."
                # ))
                # REMOVED: # REMOVED: self$results$insert(1, warning_notice)  # Causes serialization error
                }

                # Add marginal histograms if requested (easyalluvial only) ----
                marg <- self$options$marg
                if (marg && engine == "easyalluvial") {
                    plot <- easyalluvial::add_marginal_histograms(
                        p = plot,
                        data_input = mydata,
                        keep_labels = TRUE,
                        top = TRUE,
                        plot = TRUE
                    )
                }

                # Apply color palette ----
                colorPalette <- self$options$colorPalette
                plot <- private$.applyColorPalette(plot, colorPalette)

                # Apply enhanced gradients if requested ----
                if (self$options$enhancedGradients && colorPalette == "default") {
                    plot <- plot +
                        ggplot2::scale_fill_viridis_d(option = "plasma", alpha = 0.8)
                }

                # Apply theme style ----
                themeStyle <- self$options$themeStyle
                plot <- private$.applyThemeStyle(plot, themeStyle)

                # Configure plot orientation / flow direction ----
                orient <- self$options$orient
                flowDirection <- self$options$flowDirection

                # Handle orientation (legacy) and flow direction (new)
                if (orient != "vert") {
                    plot <- plot + ggplot2::coord_flip()
                } else if (flowDirection == "top_bottom") {
                    plot <- plot + ggplot2::coord_flip()
                } else if (flowDirection == "right_left") {
                    plot <- plot + ggplot2::scale_x_reverse()
                } else if (flowDirection == "bottom_top") {
                    plot <- plot + ggplot2::coord_flip() + ggplot2::scale_y_reverse()
                }

                # Apply custom title and subtitle ----
                usetitle <- self$options$usetitle
                plotSubtitle <- self$options$plotSubtitle

                # HARD STOP: Cannot use both marginals and custom titles
                if (marg && usetitle) {
                # REMOVED Notice: error_notice <- jmvcore::Notice$new(
                # options = self$options,
                # name = "marginalsAndTitlesConflict",
                # type = jmvcore::NoticeType$ERROR
                # )
                # error_notice$setContent(paste0(
                # "⛔ <b>Incompatible Options:</b> Custom titles cannot be used with marginal plots. ",
                # "This combination would produce ambiguous plot labeling.<br/><br/>",
                # "<b>Required Action:</b> Choose one:<br/>",
                # "• Disable 'Use custom title' to keep marginal plots<br/>",
                # "• Disable 'Marginal plots' to use custom title"
                # ))
                # REMOVED: self$results$insert(1, error_notice)
                    return()  # Stop execution to prevent ambiguous output
                }

                if (!marg && usetitle) {
                    mytitle <- self$options$mytitle
                    if (!is.null(plotSubtitle) && plotSubtitle != "") {
                        plot <- plot + ggplot2::labs(title = mytitle, subtitle = plotSubtitle)
                    } else {
                        plot <- plot + ggplot2::ggtitle(mytitle)
                    }
                } else if (!is.null(plotSubtitle) && plotSubtitle != "") {
                    plot <- plot + ggplot2::labs(subtitle = plotSubtitle)
                }

                # Render the final plot
                print(plot)
                TRUE

            }, error = function(e) {
                # Handle plot generation errors gracefully
                # REMOVED Notice: error_notice <- jmvcore::Notice$new(
                # options = self$options,
                # name = "plotGenerationFailed",
                # type = jmvcore::NoticeType$ERROR
                # )
                # error_notice$setContent(paste0(
                # "⛔ <b>Plot Generation Failed:</b> ", htmltools::htmlEscape(e$message), "<br/><br/>",
                # "<b>Suggestions:</b><br/>",
                # "• Try using the Easy Alluvial engine instead of GG Alluvial<br/>",
                # "• Reduce the number of variables<br/>",
                # "• Check for variables with too many unique categories<br/>",
                # "• Ensure all selected variables exist in the data"
                # ))
                # REMOVED: self$results$insert(1, error_notice)
            })
        }

        ,

        .plot2 = function(image, ggtheme, theme, ...) {
            # Condensation plot generation function
            # Creates a detailed view of how one specific variable relates to others

            # Input validation - requires both variables and condensation variable
            if (is.null(self$options$condensationvar) || is.null(self$options$vars))
                return()

            if (nrow(self$data) == 0) {
                html <- paste0(
                    "<div style='background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0;'>",
                    "<h4 style='margin-top: 0; color: #721c24;'>No Data Available</h4>",
                    "<p style='color: #721c24;'>Data contains no (complete) rows.</p>",
                    "<p>Please check your data for missing values or filtering issues.</p>",
                    "</div>"
                )
                self$results$condensationWarning$setContent(html)
                return()
            }

            # Data preparation for condensation analysis ----
            # CRITICAL: Only use selected variables to prevent PHI leakage
            condvarName <- self$options$condensationvar
            condvarName <- jmvcore::composeTerm(components = condvarName)

            # Ensure condensation variable is included in selected variables
            varsName <- self$options$vars
            if (!(condvarName %in% varsName)) {
                varsName <- c(condvarName, varsName)
            }

            # Extract ONLY selected variables (prevent PHI leakage)
            mydata <- jmvcore::select(self$data, varsName)

            # Validate condensation variable is appropriate (not continuous)
            condvar_data <- mydata[[condvarName]]
            if (is.numeric(condvar_data)) {
                unique_vals <- length(unique(condvar_data[!is.na(condvar_data)]))
                if (unique_vals > 10) {
                    condvarName_safe <- htmltools::htmlEscape(condvarName)
                    html <- paste0(
                        "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px; margin: 10px 0;'>",
                        "<h4 style='margin-top: 0; color: #856404;'>Continuous Condensation Variable</h4>",
                        "<p style='color: #856404;'>Condensation variable '<strong>", condvarName_safe, "</strong>' has <strong>", unique_vals, "</strong> unique values.</p>",
                        "<p><strong>Security Warning:</strong> Continuous variables can expose patient-level data in condensation plots.</p>",
                        "<hr style='border-color: #ffc107;'>",
                        "<p><strong>Required Action:</strong> Condensation plots require categorical variables.</p>",
                        "<p style='margin: 10px 0;'><strong>Solutions:</strong></p>",
                        "<ol style='margin-left: 20px;'>",
                        "<li>Select a different <strong>categorical</strong> variable for condensation</li>",
                        "<li>Or bin '<strong>", condvarName_safe, "</strong>' first using Data -> Compute</li>",
                        "<li>Example: Create categories like 'Low', 'Medium', 'High'</li>",
                        "</ol>",
                        "</div>"
                    )
                    self$results$condensationWarning$setContent(html)
                    return()
                }
            }

            # Clear warnings if validation passes
            self$results$condensationWarning$setContent("")

            # Handle missing values based on user preference
            excl <- self$options$excl
            if (excl) {mydata <- jmvcore::naOmit(mydata)}

            # Generate condensation plot ----
            # Condensation plots show detailed relationships between the primary variable
            # and selected variables only (not the entire dataset)
            plot2 <- easyalluvial::plot_condensation(
                df = mydata,
                first = condvarName
            )

            # Render the condensation plot
            print(plot2)
            TRUE
        }
    )
)
