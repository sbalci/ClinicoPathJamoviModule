#' @title Alluvial Plot
#' @return Alluvial Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 aes after_stat coord_flip element_text geom_text ggplot
#' @importFrom ggplot2 ggtitle labs scale_fill_brewer scale_fill_viridis_d
#' @importFrom ggplot2 scale_x_reverse scale_y_reverse theme theme_bw theme_classic
#' @importFrom ggplot2 theme_grey theme_minimal
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

        # Notice collection helpers. A single Preformatted (plain-text) output item:
        # avoids BOTH the jmvcore::Notice serialization error from
        # self$results$insert(999, Notice) AND any HTML in notices (project convention:
        # notice content must be plain text). ====
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            duplicate <- vapply(private$.noticeList, function(notice) {
                identical(notice$type, type) &&
                    identical(notice$title, title) &&
                    identical(notice$content, content)
            }, logical(1))
            if (any(duplicate))
                return()

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

            # Plain text only notices avoid HTML by project convention; the Preformatted
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

        # Validate weight variable for weighted alluvial plots
        .validateWeightVariable = function(data, weight_var) {
            if (is.null(weight_var) || weight_var == "")
                return(TRUE)
            if (!weight_var %in% names(data)) {
                private$.addNotice(
                    "ERROR",
                    "Weight Variable Not Found",
                    paste0("Weight variable '", weight_var,
                        "' does not exist in the data.")
                )
                return(FALSE)
            }

            weight_col <- data[[weight_var]]

            # All weight-validation errors are routed through the single
            # `notices` (Preformatted) channel via .addNotice for a consistent
            # UX. Preformatted is plain text, so there is no HTML injection
            # surface even for user-supplied variable names.

            # Validate weight type
            if (!is.numeric(weight_col)) {
                private$.addNotice(
                    "ERROR",
                    "Invalid Weight Variable",
                    sprintf(
                        "'%s' must be numeric (current type: %s). Please select a numeric variable containing counts, frequencies, or sampling weights.",
                        weight_var, class(weight_col)[1]
                    )
                )
                return(FALSE)
            }

            non_missing <- !is.na(weight_col)
            if (!any(non_missing)) {
                private$.addNotice(
                    "ERROR",
                    "No Valid Weights",
                    "The weight variable contains only missing values."
                )
                return(FALSE)
            }

            if (any(!is.finite(weight_col[non_missing]))) {
                private$.addNotice(
                    "ERROR",
                    "Non-finite Weights",
                    "Weights must be finite numeric values or missing."
                )
                return(FALSE)
            }

            # Check for negative weights
            n_negative <- sum(weight_col < 0, na.rm = TRUE)
            if (n_negative > 0) {
                private$.addNotice(
                    "ERROR",
                    "Negative Weights Detected",
                    sprintf(
                        "Weight variable '%s' contains %d negative value%s. Weights must be non-negative (>= 0).",
                        weight_var, n_negative, if (n_negative > 1) "s" else ""
                    )
                )
                return(FALSE)
            }

            if (!any(weight_col > 0, na.rm = TRUE)) {
                private$.addNotice(
                    "ERROR",
                    "No Positive Weights",
                    "The weight variable must contain at least one positive value."
                )
                return(FALSE)
            }

            # Check for NA weights
            n_na <- sum(is.na(weight_col))
            if (n_na > 0) {
                pct_na <- round(100 * n_na / length(weight_col), 1)
                private$.addNotice('STRONG_WARNING', 'Missing Weights', paste0(
                    n_na, " observations (", pct_na,
                    "%) have missing weights. ",
                    "These will be excluded from the visualization."
                ))
            }

            return(TRUE)
        },

        # Aggregate data for weighted ggalluvial plots
        .aggregateDataForGgalluvial = function(data, vars, weight_var) {
            if (is.null(weight_var) || weight_var == "" || !weight_var %in% names(data)) {
                return(data)  # No aggregation needed
            }

            # Remove rows with NA weights
            data <- data[!is.na(data[[weight_var]]), , drop = FALSE]
            if (nrow(data) == 0)
                return(data)

            # Aggregate weights by unique combinations of categorical variables.
            # constructFormula backtick-quotes names with spaces; asFormula allowlist-validates.
            agg_formula <- jmvcore::asFormula(
                jmvcore::constructFormula(terms = vars, dep = weight_var)
            )

            # Use aggregate to sum weights by category combinations
            data_agg <- stats::aggregate(
                agg_formula,
                data = data,
                FUN = sum,
                na.action = na.pass
            )

            return(data_agg)
        },

        # Handle missing values according to the user-facing exclusion option.
        .handleMissingValues = function(data, vars, exclude, report = TRUE) {
            n_total <- nrow(data)
            missing_counts <- sapply(vars, function(v) sum(is.na(data[[v]])))

            if (!any(missing_counts > 0))
                return(data)

            if (!exclude) {
                for (var in vars) {
                    values <- data[[var]]
                    if (!anyNA(values))
                        next

                    if (is.factor(values)) {
                        ordered_values <- is.ordered(values)
                        old_levels <- levels(values)
                        values <- as.character(values)
                        values[is.na(values)] <- "(Missing)"
                        data[[var]] <- factor(
                            values,
                            levels = unique(c(old_levels, "(Missing)")),
                            ordered = ordered_values
                        )
                    } else {
                        values <- as.character(values)
                        values[is.na(values)] <- "(Missing)"
                        data[[var]] <- values
                    }
                }
                return(data)
            }

            data_clean <- data[stats::complete.cases(data[, vars, drop = FALSE]), ]
            n_removed <- n_total - nrow(data_clean)

            if (report && n_removed > 0) {
                pct_removed <- round(100 * n_removed / n_total, 1)

                vars_with_missing <- names(missing_counts[missing_counts > 0])
                missing_details <- paste(sapply(vars_with_missing,
                    function(v) sprintf("%s: %d", htmltools::htmlEscape(v), missing_counts[v])),
                    collapse = ", ")

                info_html <- paste0(
                    "<div style='padding: 15px; margin: 6px 0; background-color: #d1ecf1; border-left: 4px solid #17a2b8; color: #0c5460; border-radius: 5px;'>",
                    "<strong>Missing Data Excluded:</strong> ", n_removed, " of ", n_total,
                    " observations (", pct_removed, "%) excluded due to missing values.<br/>",
                    "Variables with missingness: ", missing_details, "<br/>",
                    "Analysis based on ", nrow(data_clean), " complete cases.",
                    "</div>"
                )
                self$results$dataWarning$setContent(info_html)
                self$results$dataWarning$setVisible(TRUE)
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
                self$results$dataWarning$setVisible(TRUE)
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

                        self$results$dataWarning$setContent(sprintf(
                            "<div style='padding: 15px; margin: 6px 0; background-color: #f8d7da; border-left: 4px solid #dc3545; color: #721c24; border-radius: 5px;'><strong>Error:</strong> Continuous Variable Not Allowed: Variable '%s' has %d unique values and appears continuous. Alluvial plots require categorical data. Please use the categorize function.</div>",
                            var_safe, unique_values))
                        self$results$dataWarning$setVisible(TRUE)
                        return(FALSE)
                    }

                    # STRONG WARNING for 11-20 unique values
                    if (unique_values > 10) {
                        private$.addNotice('STRONG_WARNING', 'Too Many Categories', paste0(
                            "Variable '", var, "' has ",
                            unique_values, " categories. This may create an unreadable plot with too many thin flows.\n",
                            "Recommendation: Consider reducing to 3-7 categories for optimal visualization. ",
                            "Use Data > Transform to group less frequent categories."
                        ))
                    }
                }
            }

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
            data[[fill_var]] <- as.factor(data[[fill_var]])

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

        .applyFlowDirection = function(plot, orient, flow_direction) {
            # Preserve the legacy orientation shortcut only when the newer,
            # explicit flow-direction option remains at its default.
            if (identical(flow_direction, "left_right") &&
                    identical(orient, "horr")) {
                flow_direction <- "top_bottom"
            }

            reverse_x <- if ("x" %in% names(plot$data) &&
                    (is.factor(plot$data$x) || is.character(plot$data$x))) {
                ggplot2::scale_x_discrete(limits = function(values) rev(values))
            } else {
                ggplot2::scale_x_reverse()
            }

            switch(flow_direction,
                left_right = plot,
                right_left = plot + reverse_x,
                top_bottom = plot + reverse_x + ggplot2::coord_flip(),
                bottom_top = plot + ggplot2::coord_flip(),
                plot
            )
        },

        .appendDataWarning = function(html) {
            existing <- self$results$dataWarning$content
            if (is.null(existing))
                existing <- ""
            self$results$dataWarning$setContent(paste0(existing, html))
            self$results$dataWarning$setVisible(TRUE)
        },

        .prepareMainPlotState = function() {
            vars_name <- self$options$vars
            max_vars <- self$options$maxvars
            plot_vars <- utils::head(vars_name, max_vars)
            engine <- self$options$engine
            weight_var <- self$options$weight

            # Pre-render validations that depend only on options are performed
            # here (during .run) rather than inside the .plot render callback.
            # Notices mutated from a render callback are not reliably transmitted
            # to the client, so option-only failures must surface before the
            # plot is drawn.
            custombinlabels <- self$options$custombinlabels
            if (!is.null(custombinlabels) && nzchar(custombinlabels)) {
                bin_labels <- trimws(strsplit(custombinlabels, ",")[[1]])
                bin_labels <- bin_labels[nzchar(bin_labels)]
                if (length(bin_labels) < 2) {
                    private$.addNotice(
                        "ERROR",
                        "Invalid Bin Labels",
                        "Provide at least two non-empty, comma-separated bin labels."
                    )
                    return(NULL)
                }
            }

            if (isTRUE(self$options$marg) && isTRUE(self$options$usetitle)) {
                private$.addNotice('ERROR', 'Incompatible Options', paste0(
                    "Custom titles cannot be used with marginal plots. ",
                    "This combination would produce ambiguous plot labeling.\n",
                    "Required Action: Choose one:\n",
                    " - Disable 'Use custom title' to keep marginal plots\n",
                    " - Disable 'Marginal plots' to use custom title"
                ))
                return(NULL)
            }

            has_weight <- !is.null(weight_var) && length(weight_var) > 0 &&
                nzchar(weight_var)
            if (engine == "ggalluvial" && has_weight &&
                    !private$.validateWeightVariable(self$data, weight_var)) {
                return(NULL)
            }

            fill_var <- plot_vars[1]
            requested_fill <- self$options$fillGgalluvial
            if (engine == "ggalluvial" && !is.null(requested_fill) &&
                    length(requested_fill) > 0 && nzchar(requested_fill)) {
                if (!requested_fill %in% names(self$data)) {
                    private$.addNotice(
                        "ERROR",
                        "Fill Variable Not Found",
                        paste0("Fill variable '", requested_fill,
                            "' does not exist in the data.")
                    )
                    return(NULL)
                }
                fill_var <- requested_fill
            }

            if (engine == "ggalluvial" && has_weight &&
                    weight_var %in% unique(c(plot_vars, fill_var))) {
                private$.addNotice(
                    "ERROR",
                    "Weight Variable Reused",
                    paste0(
                        "Weight variable '", weight_var,
                        "' must be different from the axis and fill variables."
                    )
                )
                return(NULL)
            }

            vars_to_select <- plot_vars
            if (engine == "ggalluvial") {
                vars_to_select <- unique(c(vars_to_select, fill_var))
                if (has_weight)
                    vars_to_select <- unique(c(vars_to_select, weight_var))
            } else if (has_weight) {
                private$.addNotice(
                    "STRONG_WARNING",
                    "Weight Variable Ignored",
                    paste0(
                        "The weight variable is only supported by the GG Alluvial engine.\n",
                        "Switch to the GG Alluvial engine to use weighted flows."
                    )
                )
            }

            mydata <- jmvcore::select(self$data, vars_to_select)
            missing_vars <- plot_vars
            if (engine == "ggalluvial")
                missing_vars <- unique(c(missing_vars, fill_var))
            mydata <- private$.handleMissingValues(
                mydata,
                missing_vars,
                exclude = self$options$excl
            )

            if (nrow(mydata) == 0) {
                private$.addNotice(
                    "ERROR",
                    "No Complete Data",
                    paste0(
                        "All observations have missing values in one or more selected ",
                        "variables. Cannot generate plot."
                    )
                )
                return(NULL)
            }

            if (length(vars_name) > max_vars) {
                warning_html <- paste0(
                    "<div style='background-color: #d1ecf1; border-left: 4px solid #17a2b8; padding: 15px; margin: 10px 0;'>",
                    "<h4 style='margin-top: 0; color: #0c5460;'>Variables Truncated</h4>",
                    "<p style='color: #0c5460;'>You selected <strong>",
                    length(vars_name), "</strong> variables, but the maximum is <strong>",
                    max_vars, "</strong>.</p>",
                    "<p>Only the first <strong>", max_vars,
                    "</strong> variables are displayed.</p>",
                    "</div>"
                )
                private$.appendDataWarning(warning_html)
            }

            n_levels <- vapply(plot_vars, function(var) {
                length(unique(mydata[[var]]))
            }, integer(1))
            total_combinations <- prod(n_levels)
            if (total_combinations > 100) {
                private$.addNotice(
                    "STRONG_WARNING",
                    "Complex Visualization",
                    paste0(
                        "The selected variables create ", total_combinations,
                        " possible flow combinations. This may produce an overcrowded plot.\n",
                        "Reduce the number of variables or group infrequent categories."
                    )
                )
            }

            if (engine == "ggalluvial" && has_weight) {
                private$.checkpoint()
                grouping_vars <- unique(c(plot_vars, fill_var))
                mydata <- private$.aggregateDataForGgalluvial(
                    data = mydata,
                    vars = grouping_vars,
                    weight_var = weight_var
                )
                if (nrow(mydata) == 0) {
                    private$.addNotice(
                        "ERROR",
                        "No Valid Weights",
                        "No observations with non-missing weights remain."
                    )
                    return(NULL)
                }
            }

            list(
                data = mydata,
                vars = plot_vars,
                engine = engine,
                fill_var = fill_var,
                weight_var = if (engine == "ggalluvial" && has_weight) {
                    weight_var
                } else {
                    NULL
                }
            )
        },

        .prepareCondensationState = function() {
            cond_var <- self$options$condensationvar
            if (is.null(cond_var) || length(cond_var) == 0 || !nzchar(cond_var))
                return(NULL)

            vars_name <- unique(c(cond_var, self$options$vars))
            mydata <- jmvcore::select(self$data, vars_name)
            cond_data <- mydata[[cond_var]]

            if (is.numeric(cond_data)) {
                unique_values <- length(unique(cond_data[!is.na(cond_data)]))
                if (unique_values > 10) {
                    cond_var_safe <- htmltools::htmlEscape(cond_var)
                    html <- paste0(
                        "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px; margin: 10px 0;'>",
                        "<h4 style='margin-top: 0; color: #856404;'>Continuous Condensation Variable</h4>",
                        "<p>Condensation variable '<strong>", cond_var_safe,
                        "</strong>' has <strong>", unique_values,
                        "</strong> unique values. Select a categorical variable.</p>",
                        "</div>"
                    )
                    self$results$condensationWarning$setContent(html)
                    return(NULL)
                }
            }

            mydata <- private$.handleMissingValues(
                mydata,
                vars_name,
                exclude = self$options$excl,
                report = FALSE
            )
            if (nrow(mydata) == 0) {
                self$results$condensationWarning$setContent(
                    "No complete observations remain for the condensation plot."
                )
                return(NULL)
            }

            self$results$condensationWarning$setContent("")
            list(data = mydata, condensation_var = cond_var)
        },

        .run = function() {

            private$.noticeList <- list()
            private$.renderNotices()
            self$results$plot$setState(NULL)
            self$results$plot2$setState(NULL)
            self$results$condensationWarning$setContent("")

            # TODO (forward-looking): no `.()` wrapping anywhere in this file:
            # welcome HTML, error/warning HTML, plot captions, and the data
            # summary are English-only. Internationalise in a
            # /prepare-translation pass before the next i18n release.
            # Plot callbacks do not expose a render-safe cancellation point;
            # data preparation is interrupted between its expensive phases.

            # Input Validation ----

            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                # ToDo Message ----
                todo <- "
                <div style='font-family: Arial, sans-serif; color: #2c3e50; padding: 10px;'>
                  <h2> Alluvial Diagrams</h2>
                  <p>Visualize the flow of categorical data across multiple dimensions.</p>

                  <div style='background-color: #e7f3ff; border-left: 4px solid #2196F3; padding: 10px; margin: 10px 0;'>
                    <h3 style='margin-top: 0;'> Quick Start</h3>
                    <ul style='margin-bottom: 0;'>
                      <li>Select <strong>2-5 categorical variables</strong> (optimal: 3-4)</li>
                      <li>Each variable should have <strong>3-7 categories</strong> for best readability</li>
                      <li>For continuous variables, use the <em>categorize function</em> to create bins first</li>
                    </ul>
                  </div>

                  <div style='background-color: #f0f8f0; border-left: 4px solid #4caf50; padding: 10px; margin: 10px 0;'>
                    <h3 style='margin-top: 0;'> Clinical Use Cases</h3>
                    <ul style='margin-bottom: 0;'>
                      <li><strong>Patient flow:</strong> Track progression through treatment stages</li>
                      <li><strong>Tumor progression:</strong> Visualize grade/stage transitions</li>
                      <li><strong>Diagnostic pathways:</strong> Show relationships between symptoms \u{2192} diagnosis \u{2192} outcomes</li>
                      <li><strong>Demographics:</strong> Explore patterns across age/sex/location categories</li>
                    </ul>
                  </div>

                  <div style='background-color: #fff8e1; border-left: 4px solid #ffc107; padding: 10px; margin: 10px 0;'>
                    <h3 style='margin-top: 0;'> Tips</h3>
                    <ul style='margin-bottom: 0;'>
                      <li>Arrange variables in <strong>logical order</strong> (e.g., temporal sequence: Grade \u{2192} Stage \u{2192} Response)</li>
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
                    private$.addNotice('ERROR', 'Validation Failed', 'Alluvial diagram requires at least 2 variables with valid data. Please check variable selection and ensure sufficient data.')
                    return()
                }

                # Validate condensation variable if provided
                if (!is.null(self$options$condensationvar) &&
                    length(self$options$condensationvar) > 0 &&
                    !(self$options$condensationvar %in% names(self$data))) {

                    private$.addNotice('ERROR', 'Variable Not Found', paste0(
                        "Condensation variable '", self$options$condensationvar,
                        "' does not exist in the data. Please select a valid variable from the available list."
                    ))
                    return()
                }

                # Clear dataWarning if validation passes
                self$results$dataWarning$setContent("")
                self$results$dataWarning$setVisible(FALSE)

                main_state <- private$.prepareMainPlotState()
                if (is.null(main_state))
                    return()
                self$results$plot$setState(main_state)

                condensation_state <- private$.prepareCondensationState()
                if (!is.null(condensation_state))
                    self$results$plot2$setState(condensation_state)
            }

        }

        ,

        .plot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$data))
                return(FALSE)

            tryCatch({
                varsName <- state$vars
                weight_var <- state$weight_var
                mydata <- state$data
                engine <- state$engine

                # Configure plot aesthetics 
                # Set color fill strategy for the alluvial flows.
                # `fill` is a List keyword (first_variable/last_variable/all_flows/
                # values) passed to alluvial_wide(fill_by=), NOT a formula term, so
                # it must be used verbatim do NOT run it through composeTerm().
                fill <- self$options$fill

                # Configure bin labels with proper binning method
                bin_option <- self$options$bin
                custombinlabels <- self$options$custombinlabels

                # Determine bin labels based on user selection
                # Note: easyalluvial's bin_labels parameter only controls label display,
                # not the binning method itself. Actual binning is done by easyalluvial internally.
                bins <- 5L
                if (!is.null(custombinlabels) && custombinlabels != "") {
                    # Custom labels provided by user
                    bin <- trimws(strsplit(custombinlabels, ",")[[1]])
                    bin <- bin[nzchar(bin)]
                    if (length(bin) < 2) {
                        private$.addNotice(
                            "ERROR",
                            "Invalid Bin Labels",
                            "Provide at least two non-empty, comma-separated bin labels."
                        )
                        return(FALSE)
                    }
                    bins <- length(bin)
                } else {
                    # Use predefined labels based on bin option
                    bin <- switch(bin_option,
                        "default" = c("LL", "ML", "M", "MH", "HH"),
                        "mean" = "mean",
                        "median" = "median",
                        "min_max" = "min_max",
                        "cuts" = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                        c("LL", "ML", "M", "MH", "HH")  # fallback
                    )
                }

                maxvars <- self$options$maxvars

                # Generate plot based on selected engine ----
                if (engine == "ggalluvial") {
                    plot <- private$.createGgalluvialPlot(
                        data = mydata,
                        vars = varsName,
                        fill_var = state$fill_var,
                        weight_var = weight_var
                    )
                } else {
                    # Use easyalluvial engine (default)
                    plot <- easyalluvial::alluvial_wide(
                        data = mydata,
                        max_variables = maxvars,
                        bins = bins,
                        fill_by = fill,
                        verbose = FALSE,  # Disabled to prevent console clutter in jamovi
                        bin_labels = bin
                    )
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

                # Post-processing below adds ggplot layers (scales, themes,
                # coord_flip, labs). easyalluvial::add_marginal_histograms()
                # returns a gtable rather than a ggplot, so adding layers to it
                # errors. Skip ALL such post-processing when marginal histograms
                # were drawn (easyalluvial engine only). The marg + custom-title
                # incompatibility is pre-checked in .prepareMainPlotState.
                is_marg_easy <- isTRUE(marg) && engine == "easyalluvial"

                if (!is_marg_easy) {
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

                    plot <- private$.applyFlowDirection(
                        plot,
                        orient = orient,
                        flow_direction = flowDirection
                    )

                    # Apply custom title and subtitle ----
                    usetitle <- self$options$usetitle
                    plotSubtitle <- self$options$plotSubtitle

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
                }

                # Render the final plot
                print(plot)
                TRUE

            }, error = function(e) {
                # Handle plot generation errors gracefully
                private$.addNotice('ERROR', 'Plot Generation Failed', paste0(
                    e$message, "\n",
                    "Suggestions:\n",
                    " - Try using the Easy Alluvial engine instead of GG Alluvial\n",
                    " - Reduce the number of variables\n",
                    " - Check for variables with too many unique categories\n",
                    " - Ensure all selected variables exist in the data"
                ))
            })
        }

        ,

        .plot2 = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$data))
                return(FALSE)

            tryCatch({
                plot <- rlang::inject(
                    easyalluvial::plot_condensation(
                        df = state$data,
                        first = !!rlang::sym(state$condensation_var)
                    )
                )
                plot <- private$.applyColorPalette(plot, self$options$colorPalette)
                plot <- private$.applyThemeStyle(plot, self$options$themeStyle)
                print(plot)
                TRUE
            }, error = function(e) {
                private$.addNotice(
                    "ERROR",
                    "Condensation Plot Failed",
                    paste0(e$message, "\nSelect a categorical condensation variable ",
                        "and check the selected variables for sparse categories.")
                )
                FALSE
            })
        }
    )
)
