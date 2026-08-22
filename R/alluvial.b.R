#' @title Alluvial Plot
#' @return Alluvial Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 aes after_stat coord_flip element_text geom_text ggplot
#' @importFrom ggplot2 discrete_scale ggtitle labs scale_fill_viridis_d
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

        # Label used to draw NA cells as their own stratum, and the number of rows
        # the main diagram was drawn from. Both are set during .run and read back
        # when composing the reading notice / the condensation panel notice.
        .naLabel = "(Missing)",
        .mainPlotRows = NULL,

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
                    INFO           = "NOTE: ",
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
                # A clinical export can legitimately hold a recorded category
                # literally named "(Missing)" (REDCap and registry files often do).
                # unique(c(old_levels, "(Missing)")) then collapses the two and the
                # NA rows are merged into that real group with no warning, inflating
                # it. Pick a label that collides with nothing observed.
                observed <- unique(unlist(lapply(vars, function(v) {
                    x <- data[[v]]
                    if (is.factor(x)) levels(x) else as.character(unique(x))
                })))
                na_label <- "(Missing)"
                k <- 1L
                while (na_label %in% observed) {
                    k <- k + 1L
                    na_label <- sprintf("(Missing %d)", k)
                }
                private$.naLabel <- na_label

                for (var in vars) {
                    values <- data[[var]]
                    if (!anyNA(values))
                        next

                    if (is.factor(values)) {
                        ordered_values <- is.ordered(values)
                        old_levels <- levels(values)
                        values <- as.character(values)
                        values[is.na(values)] <- na_label
                        data[[var]] <- factor(
                            values,
                            levels = unique(c(old_levels, na_label)),
                            ordered = ordered_values
                        )
                    } else {
                        # Mirror the factor branch above instead of dropping to a
                        # plain character vector. The drawing engine bins numeric
                        # columns but leaves character ones alone, so the old
                        # else-branch made the SAME variable render different
                        # strata depending only on whether it happened to contain
                        # a missing value. Keeping the original values as factor
                        # levels makes both branches produce identical nodes.
                        values <- factor(as.character(values))
                        levels(values) <- c(levels(values), na_label)
                        values[is.na(values)] <- na_label
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
                    "<div style='padding: 15px; margin: 6px 0; background-color: rgba(33, 163, 188, 0.21); border-left: 4px solid #17a2b8; color: inherit; border-radius: 5px;'>",
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
                    "<div style='background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0; color: #e05260;'>Insufficient Variables</h4>",
                    "<p>Alluvial diagrams require at least <strong>2 variables</strong>.</p>",
                    "<p>Please select additional variables from the left panel.</p>",
                    "</div>"
                )
                self$results$dataWarning$setContent(html)
                self$results$dataWarning$setVisible(TRUE)
                return(FALSE)
            }

            if (nrow(self$data) == 0) {
                html <- paste0(
                    "<div style='background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0; color: #e05260;'>No Data Available</h4>",
                    "<p>Data contains no (complete) rows.</p>",
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
                        "<div style='background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0; color: inherit;'>",
                        "<h4 style='margin-top: 0; color: #e05260;'>Variable Not Found</h4>",
                        "<p>Variable '<strong>", var_safe, "</strong>' not found in the data.</p>",
                        "<p>Please ensure all selected variables exist in your dataset.</p>",
                        "</div>"
                    )
                    self$results$dataWarning$setContent(html)
                    self$results$dataWarning$setVisible(TRUE)
                    return(FALSE)
                }

                var_data <- self$data[[var]]

                # HARD STOP for numeric variables that look continuous. This is a
                # TYPE test, not a readability test: >20 distinct numeric values
                # means the column is a measurement, and an alluvial diagram has
                # no meaningful stratum for a measurement.
                if (is.numeric(var_data) && private$.countCategories(var_data) > 20) {
                    var_safe <- htmltools::htmlEscape(var)
                    self$results$dataWarning$setContent(sprintf(
                        "<div style='padding: 15px; margin: 6px 0; background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; color: inherit; border-radius: 5px;'><strong>Error:</strong> Continuous Variable Not Allowed: Variable '%s' has %d unique values and appears continuous. Alluvial plots require categorical data. Please use the categorize function.</div>",
                        var_safe, private$.countCategories(var_data)))
                    self$results$dataWarning$setVisible(TRUE)
                    return(FALSE)
                }

                # READABILITY warning, for EVERY type. Previously only numeric
                # variables were counted, so a factor with hundreds of levels (a
                # patient ID, an accession number, a free-text site) reached the
                # plot with no warning at all.
                private$.warnHighCardinality(var, var_data)
            }

            return(TRUE)
        },

        # One rule for "how many strata will this variable draw", used for axis
        # variables and for the condensation variable alike.
        .countCategories = function(values) {
            length(unique(values[!is.na(values)]))
        },

        .warnHighCardinality = function(var, values) {
            n_categories <- private$.countCategories(values)
            if (n_categories <= 10)
                return(invisible(NULL))

            private$.addNotice('STRONG_WARNING', 'Too Many Categories', paste0(
                "Variable '", var, "' has ", n_categories,
                " distinct categories, so the diagram will be split into that many ",
                "strata and the flows between them will be very thin.\n",
                "Why this matters: with more than about 7 categories per variable the ",
                "ribbons overlap and individual paths can no longer be traced by eye.\n",
                "What to do next: group the less frequent categories with Data > Transform, ",
                "or plot fewer variables at a time."
            ))
            invisible(NULL)
        },

        # Draw the finished object, muffling third-party warnings that describe
        # the STRUCTURE of an alluvial diagram rather than a problem with the
        # data. Left bare they land in the undifferentiated "Analysis Notes"
        # panel with nothing to attach them to and no way to act on them:
        #  - ggalluvial "Some strata appear at multiple axes" fires whenever the
        #    same category set is measured at more than one time point, which is
        #    the commonest legitimate use of an alluvial diagram;
        #  - RColorBrewer "n too large..." should no longer be reachable now that
        #    .brewerPalette interpolates instead of overflowing, and the palette
        #    stretch is reported by a notice raised during .run; kept as a guard.
        .drawQuietly = function(plot) {
            withCallingHandlers(
                if (inherits(plot, "gtable")) {
                    # add_marginal_histograms() returns a gtable; print() would
                    # dispatch to print.gtable, which dumps the grob layout as
                    # text and draws nothing.
                    grid::grid.newpage()
                    grid::grid.draw(plot)
                } else {
                    .quietly(print(plot))
                },
                warning = function(w) {
                    if (grepl("appear at multiple axes|n too large, allowed maximum for palette",
                              conditionMessage(w)))
                        invokeRestart("muffleWarning")
                }
            )
        },

        # Draw an explanatory message INTO the image. Render callbacks cannot
        # reliably write to results elements, so a failure reported through a
        # notice can vanish and leave the user with a blank panel; the plot
        # itself is the only channel that is certain to reach them.
        .messagePlot = function(text) {
            ggplot2::ggplot() +
                ggplot2::geom_text(
                    ggplot2::aes(x = 0.5, y = 0.5, label = text),
                    size = 4
                ) +
                ggplot2::xlim(0, 1) +
                ggplot2::ylim(0, 1) +
                ggplot2::theme_void()
        },

        # Helper method to create ggalluvial plots
        .createGgalluvialPlot = function(data, vars, fill_var, weight_var = NULL) {
            # Check for required package. .prepareMainPlotState already raises a
            # notice for this during .run, so normally we never get here; this is
            # the render-phase fallback, where results elements cannot be set.
            if (!requireNamespace("ggalluvial", quietly = TRUE)) {
                return(private$.messagePlot(paste0(
                    "The GG Alluvial engine needs the R package 'ggalluvial',\n",
                    "which is not installed, so this plot cannot be drawn.\n\n",
                    "Switch the 'Plot engine' option to 'Easy Alluvial',\n",
                    "or run install.packages(\"ggalluvial\") in R and restart jamovi."
                )))
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

            # Add counts if requested. StatStratum's `count` is the sum of the y
            # aesthetic within the stratum, so once a weight variable is mapped to y
            # it is a weight TOTAL, not a case count, and it prints at full double
            # precision (node labels read 48.0079036487576). Round it in that case.
            # Unweighted, y defaults to 1 per row, so `count` is the whole number of
            # cases. (after_stat(n) is NOT a substitute: the weighted frame has
            # already been aggregated, so n counts aggregated rows, not cases.)
            if (show_counts) {
                count_label <- if (!is.null(weight_var) && weight_var %in% names(data)) {
                    ggplot2::aes(label = ggplot2::after_stat(round(count, 1)))
                } else {
                    ggplot2::aes(label = ggplot2::after_stat(count))
                }
                plot <- plot +
                    ggplot2::geom_text(
                        stat = ggalluvial::StatStratum,
                        count_label,
                        size = 2.5,
                        vjust = -0.5
                    )
            }

            return(plot)
        },

        # ColorBrewer qualitative palettes have a hard colour count.
        .paletteCapacity = list(set3 = 12L, pastel1 = 9L, dark2 = 8L),
        .paletteLabel = list(set3 = "Set3", pastel1 = "Pastel1", dark2 = "Dark2"),

        # scale_fill_brewer answers with NA once ggplot2 asks for more colours
        # than the palette holds, so those flows are drawn with no fill and
        # VANISH from the diagram - with only a bare RColorBrewer warning
        # ("n too large, allowed maximum for palette Dark2 is 8") to show for it.
        # ggplot2 asks for one colour per discrete value across EVERY layer that
        # maps fill, which in an alluvial diagram is the union of the flow groups
        # and the stratum values, so the cap is reached sooner than the number of
        # categories suggests. Hand ggplot2 a palette that interpolates past the
        # cap instead: the brewer hues are used exactly as published up to the
        # cap, and blended shades fill in beyond it, so nothing is ever dropped.
        .brewerPalette = function(name, cap) {
            force(name); force(cap)
            function(n) {
                base <- RColorBrewer::brewer.pal(cap, name)
                if (n <= cap)
                    base[seq_len(n)]
                else
                    grDevices::colorRampPalette(base)(n)
            }
        },

        # Helper to apply color palette
        .applyColorPalette = function(plot, palette) {
            if (palette == "default") {
                return(plot)
            }

            colors <- switch(palette,
                "viridis" = ggplot2::scale_fill_viridis_d(),
                "plasma" = ggplot2::scale_fill_viridis_d(option = "plasma"),
                "set3" = ggplot2::discrete_scale(
                    "fill", palette = private$.brewerPalette("Set3", 12L)),
                "pastel1" = ggplot2::discrete_scale(
                    "fill", palette = private$.brewerPalette("Pastel1", 9L)),
                "dark2" = ggplot2::discrete_scale(
                    "fill", palette = private$.brewerPalette("Dark2", 8L)),
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

            # head() silently discards everything past `maxvars`. A user who
            # selects twelve time points and sees a diagram of eight has no way to
            # tell which stages are missing - the only hint was the flow-count
            # notice, which quietly counts paths through eight. Name the dropped
            # variables and the setting that dropped them.
            if (length(vars_name) > length(plot_vars)) {
                dropped <- setdiff(vars_name, plot_vars)
                private$.addNotice(
                    "WARNING",
                    "Variables not shown",
                    paste0(
                        sprintf("Only the first %d of %d selected variables are plotted, because 'Maximum variables' is set to %d.",
                                length(plot_vars), length(vars_name), max_vars),
                        "\nNot shown: ", paste(dropped, collapse = ", "),
                        "\nRaise 'Maximum variables' to include them, or deselect the ones you do not need."))
            }
            engine <- self$options$engine
            weight_var <- self$options$weight

            # ggalluvial is an optional dependency. Detect it here, during .run,
            # so the user gets an actionable notice instead of a raw package
            # error raised from the render callback (which cannot populate
            # results elements).
            if (engine == "ggalluvial" && !requireNamespace("ggalluvial", quietly = TRUE)) {
                private$.addNotice(
                    "ERROR",
                    "GG Alluvial Engine Not Available",
                    paste0(
                        "The 'GG Alluvial' plot engine needs the R package 'ggalluvial', ",
                        "which is not installed on this computer, so no alluvial plot can be drawn. ",
                        "What to do next: switch the 'Plot engine' option to 'Easy Alluvial' - ",
                        "it is already installed and shows the same category flows ",
                        "(weighted flows and a separate fill variable are only available in GG Alluvial); ",
                        "or install the package by running install.packages(\"ggalluvial\") in R, ",
                        "then restart jamovi and run the analysis again."
                    )
                )
                return(NULL)
            }

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

            # Marginal histograms are drawn by assembling the diagram and the
            # histograms into a fixed grid (a gtable), which is no longer an
            # editable ggplot - so every styling layer this analysis would
            # otherwise add is silently dropped. Say so rather than suppressing
            # the whole diagram: it is a cosmetic limitation, not a data error.
            # (This previously raised an ERROR for marg + custom title and drew
            # nothing at all, even under the GG Alluvial engine, which never
            # draws marginal histograms in the first place.)
            marg_suppresses_styling <- isTRUE(self$options$marg) &&
                engine == "easyalluvial"
            if (marg_suppresses_styling) {
                private$.addNotice(
                    "WARNING",
                    "Styling options not applied",
                    paste0(
                        "Marginal plots are switched on, so the diagram and its histograms are ",
                        "assembled into a fixed grid that cannot take further styling. These ",
                        "settings were left out of this plot: colour palette, theme style, ",
                        "enhanced edge gradients, plot orientation, flow direction, plot ",
                        "subtitle and custom title.\n",
                        "Why this matters: changing any of them will not alter the picture on ",
                        "screen while marginal plots are on.\n",
                        "What to do next: switch 'Marginal plots' off if you need any of those settings."
                    )
                )
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

            # easyalluvial re-bins every NUMERIC column before drawing: it
            # centres, scales, Yeo-Johnson transforms and winsorises the values,
            # then cuts the result into equal-width intervals and replaces the
            # labels. A numeric Grade coded 1/2/3 therefore rendered as the
            # strata "LL", "M", "HH" (two of the five bins empty) - node labels
            # with no relationship to the recorded grades. Continuous columns are
            # already hard-stopped upstream (>20 distinct values), so every
            # numeric axis variable reaching this point is categorical; convert
            # it so its own values become the strata. This also makes the two
            # engines agree, since .createGgalluvialPlot factors everything.
            coerced_numeric <- character(0)
            for (v in missing_vars) {
                if (is.numeric(mydata[[v]])) {
                    coerced_numeric <- c(coerced_numeric, v)
                    mydata[[v]] <- factor(mydata[[v]])
                }
            }
            if (length(coerced_numeric) > 0) {
                private$.addNotice(
                    "INFO",
                    "Numeric variables plotted as categories",
                    paste0(
                        "These variables hold numbers and are drawn with their own recorded ",
                        "values as categories: ", paste(coerced_numeric, collapse = ", "), ".\n",
                        "Why this matters: the drawing engine would otherwise rescale each ",
                        "numeric variable and cut it into equal-width bins, so values coded ",
                        "1/2/3 would appear on the diagram as bin labels such as LL/M/HH.\n",
                        "What to do next: nothing is needed. To combine values into wider ",
                        "groups, recode the variable with Data > Transform before plotting."
                    )
                )
            }

            n_rows_before <- nrow(mydata)
            n_incomplete <- sum(!stats::complete.cases(
                mydata[, missing_vars, drop = FALSE]))
            mydata <- private$.handleMissingValues(
                mydata,
                missing_vars,
                exclude = self$options$excl
            )

            private$.mainPlotRows <- nrow(mydata)

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
                    "<div style='background-color: rgba(33, 163, 188, 0.21); border-left: 4px solid #17a2b8; padding: 15px; margin: 10px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0; color: #4db8cc;'>Variables Truncated</h4>",
                    "<p>You selected <strong>",
                    length(vars_name), "</strong> variables, but the maximum is <strong>",
                    max_vars, "</strong>.</p>",
                    "<p>Only the first <strong>", max_vars,
                    "</strong> variables are displayed.</p>",
                    "</div>"
                )
                private$.appendDataWarning(warning_html)
            }

            # Count the paths the data actually contain rather than the cartesian
            # product of the level counts. prod() described a figure that is never
            # drawn: three 5-level variables give 125 and tripped this warning even
            # when only 12 paths were observed and the diagram was perfectly legible.
            # prod() also returns a double, so the old paste0() rendered the number
            # as "1e+05" once it passed 100000; nrow() is an integer.
            n_distinct_paths <- nrow(unique(mydata[, plot_vars, drop = FALSE]))
            if (n_distinct_paths > 100) {
                private$.addNotice(
                    "STRONG_WARNING",
                    "Complex Visualization",
                    paste0(
                        "The data contain ", n_distinct_paths,
                        " distinct paths through the selected variables. This may produce an overcrowded plot.\n",
                        "Reduce the number of variables or group infrequent categories."
                    )
                )
            }

            # How many groups the fill scale has to colour. Counted here, from the
            # data, rather than guessed at render time.
            n_fill_groups <- if (engine == "ggalluvial") {
                private$.countCategories(mydata[[fill_var]])
            } else {
                switch(self$options$fill,
                    last_variable = private$.countCategories(
                        mydata[[plot_vars[length(plot_vars)]]]),
                    values = length(unique(unlist(lapply(plot_vars, function(v)
                        as.character(mydata[[v]]))))),
                    all_flows = nrow(unique(mydata[, plot_vars, drop = FALSE])),
                    private$.countCategories(mydata[[plot_vars[1]]]))
            }

            # n_fill_groups is a LOWER bound on what ggplot2 will ask the fill
            # scale for (it counts the flow groups; the scale's domain is the
            # union of those and the stratum values across all layers). So this
            # notice under-fires but never over-fires: if the lower bound already
            # exceeds the palette's capacity, the palette certainly had to be
            # stretched. .applyColorPalette blends the extra shades rather than
            # dropping the groups, so nothing disappears either way.
            colorPalette <- self$options$colorPalette
            palette_cap <- private$.paletteCapacity[[colorPalette]]
            if (!marg_suppresses_styling && !is.null(palette_cap) &&
                    n_fill_groups > palette_cap) {
                private$.addNotice(
                    "WARNING",
                    "Colour palette stretched",
                    paste0(
                        sprintf(
                            "The %s palette publishes %d distinguishable colours, and this diagram has at least %d groups to colour.",
                            private$.paletteLabel[[colorPalette]], palette_cap, n_fill_groups),
                        "\nWhy this matters: the extra groups are drawn in shades blended between ",
                        "the published colours, so neighbouring groups can look alike and are hard ",
                        "to tell apart in the legend.\n",
                        "What to do next: choose Viridis or Plasma, which stay distinguishable over ",
                        "many groups, or group categories with Data > Transform until the palette fits."
                    )
                )
            }

            # .aggregateDataForGgalluvial drops rows whose weight is NA, and it
            # runs before the reading notice is composed, so count them here or the
            # notice overstates the sample the diagram is drawn from.
            n_weight_na <- 0L
            if (engine == "ggalluvial" && has_weight) {
                private$.checkpoint()
                n_weight_na <- sum(is.na(mydata[[weight_var]]))
                private$.mainPlotRows <- private$.mainPlotRows - n_weight_na
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

            private$.addReadingNotice(
                engine = engine,
                has_weight = engine == "ggalluvial" && has_weight,
                weight_var = weight_var,
                excl = isTRUE(self$options$excl),
                n_rows_before = n_rows_before,
                n_incomplete = n_incomplete,
                n_weight_na = n_weight_na
            )

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

        # Everything a reader needs in order to know what the picture represents:
        # what a ribbon's width counts, what happened to missing values, and which
        # of the options they set the chosen engine never looks at.
        .addReadingNotice = function(engine, has_weight, weight_var, excl,
                                     n_rows_before, n_incomplete, n_weight_na = 0L) {
            # Rows kept: complete.cases() drops exactly the incomplete ones when
            # exclusion is on (and none when it is off, since those cells are
            # relabelled instead), and rows with no weight are dropped either way.
            # Derived rather than measured because weighted ggalluvial data has
            # already been aggregated by this point.
            n_dropped_incomplete <- if (excl) n_incomplete else 0L
            n_rows_after <- n_rows_before - n_dropped_incomplete - n_weight_na
            width_text <- if (has_weight) {
                sprintf("the total of the weight variable '%s' over the cases following that path.",
                        weight_var)
            } else {
                "the number of cases following that path (each case counts once)."
            }

            missing_text <- if (n_incomplete == 0) {
                "no row had a missing value in any plotted variable."
            } else if (excl) {
                sprintf(paste0("%d of %d rows had a missing value in a plotted variable and ",
                               "were removed."),
                        n_incomplete, n_rows_before)
            } else {
                sprintf(paste0("%d of %d rows had a missing value in a plotted variable. Those ",
                               "cells are shown as a '%s' category, which is drawn like ",
                               "any other category but is not an observed group. Switch on ",
                               "'Missing-value exclusion (NA)' to drop those rows instead."),
                        n_incomplete, n_rows_before, private$.naLabel)
            }

            if (n_weight_na > 0) {
                missing_text <- paste0(missing_text, sprintf(
                    paste0(" %s%d row(s) had no value for the weight variable '%s' ",
                           "and were removed."),
                    if (n_incomplete > 0) "A further " else "",
                    n_weight_na, weight_var))
            }
            if (n_dropped_incomplete + n_weight_na > 0) {
                missing_text <- paste0(missing_text, sprintf(
                    " The diagram is based on the remaining %d rows.", n_rows_after))
            }

            if (engine == "ggalluvial") {
                engine_label <- "GG Alluvial"
                ignored <- c("Fill by", "Bin labels", "Custom bin labels", "Marginal plots")
            } else {
                engine_label <- "Easy Alluvial"
                ignored <- c("Fill variable (ggalluvial)", "Weight variable", "Node labels",
                             "Counts on nodes", "Sankey styling", "Curve type")
            }

            lines <- c(
                paste0("Ribbon width: ", width_text),
                paste0("Missing values: ", missing_text),
                paste0("Ignored by the ", engine_label, " engine: ",
                       paste(ignored, collapse = ", "), ".")
            )

            # The numbers 'Counts on nodes' draws come from the same y aesthetic as
            # the ribbon widths, so under a weight variable they are weight totals.
            if (has_weight && isTRUE(self$options$showCounts)) {
                lines <- c(lines, paste0(
                    "Node numbers: the total of the weight variable in that group, rounded to ",
                    "one decimal place - not a case count."))
            }

            # Bin labels only rename intervals that the engine creates when it has
            # to split a continuous variable, and this analysis draws every axis
            # variable as a category, so they never take effect.
            bin_labels_set <- !identical(self$options$bin, "default") ||
                (!is.null(self$options$custombinlabels) &&
                     nzchar(self$options$custombinlabels))
            if (engine == "easyalluvial" && bin_labels_set) {
                lines <- c(lines, paste0(
                    "Bin labels: not used. They rename the intervals the engine creates when it ",
                    "has to split a continuous variable, and every variable here is drawn as a ",
                    "category, so nothing was binned."))
            }

            private$.addNotice("INFO", "How to read this diagram",
                               paste(lines, collapse = "\n"))
        },

        .prepareCondensationState = function() {
            cond_var <- self$options$condensationvar
            if (is.null(cond_var) || length(cond_var) == 0 || !nzchar(cond_var))
                return(NULL)

            # The axis variables the main diagram actually draws. Using the whole
            # selection here built the two panels of one output from different
            # variable sets whenever more variables were selected than 'Maximum
            # variables' allows, and ran complete-case filtering over more columns,
            # so the two figures silently used different sample sizes.
            plot_vars <- utils::head(self$options$vars, self$options$maxvars)
            vars_name <- unique(c(cond_var, plot_vars))
            mydata <- jmvcore::select(self$data, vars_name)
            cond_data <- mydata[[cond_var]]

            # Distinct non-missing values, counted the same way for every storage
            # type (.countCategories). The numeric case still hard-stops, because
            # >10 distinct numbers means the column is a measurement and has no
            # meaningful condensation panel; a high-cardinality factor is only
            # hard to read, so it is warned about and still drawn.
            unique_values <- private$.countCategories(cond_data)
            if (is.numeric(cond_data) && unique_values > 10) {
                cond_var_safe <- htmltools::htmlEscape(cond_var)
                html <- paste0(
                    "<div style='background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; padding: 15px; margin: 10px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0; color: #d4a017;'>Continuous Condensation Variable</h4>",
                    "<p>Condensation variable '<strong>", cond_var_safe,
                    "</strong>' has <strong>", unique_values,
                    "</strong> distinct numeric values and appears continuous, so no condensation ",
                    "plot was drawn.</p>",
                    "<p>Select a categorical variable, or group the values with Data &gt; Transform first.</p>",
                    "</div>"
                )
                self$results$condensationWarning$setContent(html)
                return(NULL)
            }
            if (!is.numeric(cond_data) && unique_values > 10) {
                private$.warnHighCardinality(cond_var, cond_data)
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

            # The condensation panel additionally requires the condensation
            # variable to be non-missing, so with exclusion on it can be built from
            # fewer rows than the diagram above. Say so rather than showing two
            # figures on different n without a word.
            if (!is.null(private$.mainPlotRows) &&
                    nrow(mydata) != private$.mainPlotRows) {
                private$.addNotice(
                    "INFO",
                    "Condensation panel sample",
                    sprintf(paste0("The condensation panel is based on %d rows; the diagram ",
                                   "above uses %d. The two panels require different variables ",
                                   "to be recorded, so they drop different rows."),
                            nrow(mydata), private$.mainPlotRows))
            }

            self$results$condensationWarning$setContent("")
            list(data = mydata, condensation_var = cond_var)
        },

        .run = function() {

            private$.noticeList <- list()
            private$.naLabel <- "(Missing)"
            private$.mainPlotRows <- NULL
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
                <div style='font-family: Arial, sans-serif; color: inherit; padding: 10px;'>
                  <h2> Alluvial Diagrams</h2>
                  <p>Visualize the flow of categorical data across multiple dimensions.</p>

                  <div style='background-color: rgba(33, 144, 255, 0.11); border-left: 4px solid #2196F3; padding: 10px; margin: 10px 0; color: inherit;'>
                    <h3 style='margin-top: 0;'> Quick Start</h3>
                    <ul style='margin-bottom: 0;'>
                      <li>Select <strong>2-5 categorical variables</strong> (optimal: 3-4)</li>
                      <li>Each variable should have <strong>3-7 categories</strong> for best readability</li>
                      <li>For continuous variables, use the <em>categorize function</em> to create bins first</li>
                    </ul>
                  </div>

                  <div style='background-color: rgba(33, 152, 33, 0.07); border-left: 4px solid #4caf50; padding: 10px; margin: 10px 0; color: inherit;'>
                    <h3 style='margin-top: 0;'> Clinical Use Cases</h3>
                    <ul style='margin-bottom: 0;'>
                      <li><strong>Patient flow:</strong> Track progression through treatment stages</li>
                      <li><strong>Tumor progression:</strong> Visualize grade/stage transitions</li>
                      <li><strong>Diagnostic pathways:</strong> Show relationships between symptoms \u{2192} diagnosis \u{2192} outcomes</li>
                      <li><strong>Demographics:</strong> Explore patterns across age/sex/location categories</li>
                    </ul>
                  </div>

                  <div style='background-color: rgba(255, 203, 33, 0.14); border-left: 4px solid #ffc107; padding: 10px; margin: 10px 0; color: inherit;'>
                    <h3 style='margin-top: 0;'> Tips</h3>
                    <ul style='margin-bottom: 0;'>
                      <li>Arrange variables in <strong>logical order</strong> (e.g., temporal sequence: Grade \u{2192} Stage \u{2192} Response)</li>
                      <li>Start with <strong>fewer variables</strong> and add more once you understand the patterns</li>
                      <li>Use <strong>weighted flows</strong> (GG Alluvial engine) for aggregated data with frequency counts</li>
                      <li>Enable <strong>marginal histograms</strong> to see individual variable distributions</li>
                    </ul>
                  </div>

                  <hr style='margin-top: 15px;'>
                  <p style='font-size: 0.9em; text-align: center;'>
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

                # Use shared validation logic. Every failure branch inside
                # .validateAlluvialInputs() writes its own specific, visible
                # message to dataWarning (too few variables / no rows / a
                # continuous variable / a variable not in the data). The generic
                # "requires at least 2 variables" notice that used to be added
                # here contradicted two of those three panels and sent users to
                # the wrong fix.
                if (!private$.validateAlluvialInputs())
                    return()

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

                # Bin labels rename the intervals easyalluvial creates when it has
                # to split a CONTINUOUS column. .prepareMainPlotState converts
                # every axis variable to a factor first, so in practice nothing is
                # binned and these labels are inert; they are kept so that data
                # reaching the engine by any other route is labelled sensibly.
                # The label count (>= 2) is already enforced during .run, which
                # returns before setState, so .plot cannot be reached with a bad
                # list - the duplicate guard that used to live here was dead code.
                bins <- 5L
                if (!is.null(custombinlabels) && custombinlabels != "") {
                    bin <- trimws(strsplit(custombinlabels, ",")[[1]])
                    bin <- bin[nzchar(bin)]
                    bins <- length(bin)
                } else {
                    # "cuts" is an easyalluvial keyword that prints the real
                    # interval boundaries. It used to be mapped to c("Q1".."Q5"),
                    # which labelled equal-WIDTH bins as if they were quintiles -
                    # a skewed variable could put 60% of cases in "Q1" and leave
                    # "Q2" and "Q4" empty.
                    bin <- switch(bin_option,
                        "default" = c("LL", "ML", "M", "MH", "HH"),
                        "mean" = "mean",
                        "median" = "median",
                        "min_max" = "min_max",
                        "cuts" = "cuts",
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
                    plot <- .quietly(easyalluvial::alluvial_wide(
                        data = mydata,
                        max_variables = maxvars,
                        bins = bins,
                        fill_by = fill,
                        verbose = FALSE,  # Disabled to prevent console clutter in jamovi
                        bin_labels = bin
                    ))
                }

                # Add marginal histograms if requested (easyalluvial only) ----
                marg <- self$options$marg
                if (marg && engine == "easyalluvial") {
                    # plot = FALSE: with plot = TRUE the histograms were drawn as
                    # a SIDE EFFECT of this call and the print() below then hit
                    # print.gtable, which writes a text dump of the grob layout to
                    # the console and draws nothing. Take the gtable and draw it
                    # explicitly instead (see the grid.draw branch below).
                    plot <- .quietly(easyalluvial::add_marginal_histograms(
                        p = plot,
                        data_input = mydata,
                        keep_labels = TRUE,
                        top = TRUE,
                        plot = FALSE
                    ))
                }

                # Post-processing below adds ggplot layers (scales, themes,
                # coord_flip, labs). easyalluvial::add_marginal_histograms()
                # returns a gtable rather than a ggplot, so adding layers to it
                # errors. Skip ALL such post-processing when marginal histograms
                # were drawn (easyalluvial engine only); .prepareMainPlotState
                # raises a notice naming everything this suppresses.
                is_marg_easy <- isTRUE(marg) && engine == "easyalluvial"

                if (!is_marg_easy) {
                    # Apply color palette ----
                    colorPalette <- self$options$colorPalette
                    # .quietly: easyalluvial installs its own fill scale first, so
                    # ggplot2 emits "Scale for fill is already present..." on every
                    # run with a palette chosen. That message reached the user's
                    # Analysis Notes with nothing to attach it to.
                    plot <- .quietly(private$.applyColorPalette(plot, colorPalette))

                    # Apply enhanced gradients if requested ----
                    if (self$options$enhancedGradients && colorPalette == "default") {
                        # .quietly for the same reason as the palette above: this
                        # replaces easyalluvial's own fill scale, and ggplot2 emits
                        # "Scale for fill is already present..." at + time.
                        plot <- .quietly(plot +
                            ggplot2::scale_fill_viridis_d(option = "plasma", alpha = 0.8))
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

                    # This whole block already runs only when marginal histograms
                    # were NOT drawn, so the old `!marg &&` guard was redundant
                    # under easyalluvial and actively wrong under GG Alluvial: a
                    # stale marg = TRUE (the checkbox is greyed but keeps its
                    # value when the engine changes) silently swallowed the
                    # custom title on a plot that has no marginal histograms.
                    if (usetitle) {
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

                # Render the final plot.
                private$.drawQuietly(plot)
                TRUE

            }, error = function(e) {
                # Draw the explanation INTO the image rather than into a notice.
                # A render callback cannot reliably write to results elements, so
                # the old .addNotice call could leave the user with a blank panel
                # and no idea why. It also fell off the end without an explicit
                # return value.
                print(private$.messagePlot(paste0(
                    "The alluvial diagram could not be drawn.\n\n",
                    "Reported by R: ", conditionMessage(e), "\n\n",
                    "What to try next:\n",
                    "  - switch 'Plot engine' to Easy Alluvial\n",
                    "  - plot fewer variables\n",
                    "  - group categories with few cases (Data > Transform)\n",
                    "  - check that every selected variable is still in the data"
                )))
                TRUE
            })
        }

        ,

        .plot2 = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$data))
                return(FALSE)

            tryCatch({
                plot <- .quietly(rlang::inject(
                    easyalluvial::plot_condensation(
                        df = state$data,
                        first = !!rlang::sym(state$condensation_var)
                    )
                ))
                plot <- .quietly(private$.applyColorPalette(
                    plot, self$options$colorPalette))
                plot <- private$.applyThemeStyle(plot, self$options$themeStyle)
                private$.drawQuietly(plot)
                TRUE
            }, error = function(e) {
                # Same reasoning as .plot: notices written from a render callback
                # are not reliably transmitted, so the explanation goes in the
                # image where the user is certain to see it.
                print(private$.messagePlot(paste0(
                    "The condensation plot could not be drawn.\n\n",
                    "Reported by R: ", conditionMessage(e), "\n\n",
                    "What to try next:\n",
                    "  - choose a categorical condensation variable\n",
                    "  - group categories with very few cases (Data > Transform)"
                )))
                TRUE
            })
        }
    )
)
