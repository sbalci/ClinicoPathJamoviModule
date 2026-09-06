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
#' - Fill options and an optional flow table (one row per path, commonest first)
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
        # A numeric column with more distinct values than this is a measurement
        # (continuous) and is refused. One rule for the axis variables and the
        # condensation variable alike; they used to differ (20 vs 10), so a
        # numeric with 11-20 values was accepted as an axis but refused as the
        # condensation variable, and neither message mentioned the other rule.
        .maxNumericCategories = 20L,
        # One-row data frame (path, n, pct, w) for the commonest path, set by
        # .prepareMainPlotState and quoted in the reading notice.
        .topFlow = NULL,

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
                # The severity label and the title are one template, so a
                # translator controls the punctuation between them.
                heading <- switch(notice$type,
                    ERROR          = .("ERROR: {title}"),
                    STRONG_WARNING = .("WARNING: {title}"),
                    WARNING        = .("WARNING: {title}"),
                    INFO           = .("NOTE: {title}"),
                    "{title}")
                paste0(.fmt(heading, title = notice$title), "\n", notice$content)
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
                    .("Weight Variable Not Found"),
                    .fmt(.("Weight variable '{weight}' does not exist in the data."),
                                    weight = weight_var)
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
                    .("Invalid Weight Variable"),
                    .fmt(
                        .("'{weight}' must be numeric (current type: {type}). Select a numeric variable containing counts, frequencies, or sampling weights."),
                        weight = weight_var, type = class(weight_col)[1])
                )
                return(FALSE)
            }

            non_missing <- !is.na(weight_col)
            if (!any(non_missing)) {
                private$.addNotice(
                    "ERROR",
                    .("No Valid Weights"),
                    .("The weight variable contains only missing values.")
                )
                return(FALSE)
            }

            if (any(!is.finite(weight_col[non_missing]))) {
                private$.addNotice(
                    "ERROR",
                    .("Non-finite Weights"),
                    .("Weights must be finite numeric values or missing.")
                )
                return(FALSE)
            }

            # Check for negative weights
            n_negative <- sum(weight_col < 0, na.rm = TRUE)
            if (n_negative > 0) {
                private$.addNotice(
                    "ERROR",
                    .("Negative Weights Detected"),
                    .fmt(
                        .("Weight variable '{weight}' contains {n} negative value(s). Weights must be non-negative (>= 0)."),
                        weight = weight_var, n = n_negative)
                )
                return(FALSE)
            }

            if (!any(weight_col > 0, na.rm = TRUE)) {
                private$.addNotice(
                    "ERROR",
                    .("No Positive Weights"),
                    .("The weight variable must contain at least one positive value.")
                )
                return(FALSE)
            }

            # Check for NA weights
            n_na <- sum(is.na(weight_col))
            if (n_na > 0) {
                pct_na <- round(100 * n_na / length(weight_col), 1)
                private$.addNotice("STRONG_WARNING", .("Missing Weights"), .fmt(
                    .("{n} observations ({pct}%) have missing weights. These will be excluded from the visualization."),
                    n = n_na, pct = pct_na))
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
        # What was dropped is reported once, by the "How to read this diagram"
        # notice (.addReadingNotice), not here.
        .handleMissingValues = function(data, vars, exclude) {
            if (!anyNA(data[, vars, drop = FALSE]))
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
                na_label <- .("(Missing)")
                k <- 1L
                while (na_label %in% observed) {
                    k <- k + 1L
                    na_label <- .fmt(.("(Missing {k})"), k = k)
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
            return(data_clean)
        },

        # Shared validation helper. Every failure is an ERROR notice in the one
        # notices panel; the analysis used to split these between an HTML
        # "Data Validation" panel and the notices, so a user had two places to
        # look for why no plot appeared.
        .validateAlluvialInputs = function() {
            if (is.null(self$options$vars) || length(self$options$vars) == 0)
                return(FALSE)

            if (length(self$options$vars) < 2) {
                private$.addNotice(
                    "ERROR",
                    .("Insufficient Variables"),
                    .("Alluvial diagrams require at least 2 variables. Select additional variables from the left panel.")
                )
                return(FALSE)
            }

            if (nrow(self$data) == 0) {
                private$.addNotice(
                    "ERROR",
                    .("No Data Available"),
                    .("Data contains no (complete) rows. Check your data for missing values or filtering.")
                )
                return(FALSE)
            }

            # Validate that variables are appropriate for alluvial diagrams
            private$.validateVariableTypes(self$options$vars)
        },

        # Data type validation and discretization helper
        .validateVariableTypes = function(vars) {
            for (var in vars) {
                if (!(var %in% names(self$data))) {
                    private$.addNotice(
                        "ERROR",
                        .("Variable Not Found"),
                        .fmt(
                            .("Variable '{variable}' was not found in the data. Make sure every selected variable still exists in the dataset."),
                            variable = var)
                    )
                    return(FALSE)
                }

                var_data <- self$data[[var]]

                # HARD STOP for numeric variables that look continuous. This is a
                # TYPE test, not a readability test: >20 distinct numeric values
                # means the column is a measurement, and an alluvial diagram has
                # no meaningful stratum for a measurement.
                if (is.numeric(var_data) &&
                        private$.countCategories(var_data) > private$.maxNumericCategories) {
                    private$.addNotice(
                        "ERROR",
                        .("Continuous Variable Not Allowed"),
                        .fmt(
                            .("Variable '{variable}' has {n} unique values and appears continuous, so it has no meaningful strata. Alluvial diagrams need categorical data: group the values first with the categorize analysis or Data > Transform."),
                            variable = var, n = private$.countCategories(var_data))
                    )
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

        # easyalluvial re-bins every NUMERIC column before drawing (see the
        # notice raised in .prepareMainPlotState), so every numeric column that
        # reaches either engine is converted to a factor of its own values.
        .factorNumeric = function(data, vars) {
            for (v in vars) {
                if (is.numeric(data[[v]]))
                    data[[v]] <- factor(data[[v]])
            }
            data
        },

        .warnHighCardinality = function(var, values) {
            n_categories <- private$.countCategories(values)
            if (n_categories <= 10)
                return(invisible(NULL))

            # One complete sentence per .() and the line breaks added here, so
            # each sentence translates on its own.
            private$.addNotice("STRONG_WARNING", .("Too Many Categories"), paste(
                .fmt(
                    .("Variable '{variable}' has {n} distinct categories, so the diagram will be split into that many strata and the flows between them will be very thin."),
                    variable = var, n = n_categories),
                .("Why this matters: with more than about 7 categories per variable the ribbons overlap and individual paths can no longer be traced by eye."),
                .("What to do next: group the less frequent categories with Data > Transform, or plot fewer variables at a time."),
                sep = "\n"))
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
        # Line-wrap a translated sentence for drawing inside an image.
        .wrapText = function(text, width = 62) {
            paste(strwrap(text, width = width), collapse = "\n")
        },

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
                return(private$.messagePlot(paste(
                    private$.wrapText(.("The GG Alluvial engine needs the R package 'ggalluvial', which is not installed, so this plot cannot be drawn.")),
                    "",
                    private$.wrapText(.("Switch the 'Plot engine' option to 'Easy Alluvial', or run install.packages(\"ggalluvial\") in R and restart jamovi.")),
                    sep = "\n")))
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
            axis_names <- paste0("axis", seq_len(n_vars))

            # Build the aes call
            aes_args <- list()
            for (i in seq_len(n_vars)) {
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

            # Name the axes. The stat places axis i at x = i on a continuous
            # position scale, so without this the ticks read 1.0, 1.5, 2.0, ...
            # and the reader cannot tell which axis is which variable.
            plot <- plot +
                ggplot2::scale_x_discrete(limits = vars, expand = c(0.05, 0.05))

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

        # Resolve the orientation and flow-direction options into whether the
        # axis order is reversed and whether the axes are flipped. right_left =
        # reversed x; top_bottom = reversed x + coord_flip; bottom_top = coord_flip.
        .resolveFlowDirection = function(orient, flow_direction) {
            # Preserve the legacy orientation shortcut only when the newer,
            # explicit flow-direction option remains at its default.
            if (identical(flow_direction, "left_right") &&
                    identical(orient, "horr")) {
                flow_direction <- "top_bottom"
            }
            list(
                reverse = flow_direction %in% c("right_left", "top_bottom"),
                flip = flow_direction %in% c("top_bottom", "bottom_top")
            )
        },

        .applyFlowDirection = function(plot, direction, engine) {
            # Easy Alluvial draws a long frame whose x is a factor of variable
            # names, so reversing its discrete limits reverses the axes. GG
            # Alluvial fixes axis i at position x = i, so no position scale can
            # reorder it: scale_x_reverse() only negated the tick labels and
            # scale_x_discrete(limits = rev(vars)) would relabel the axes without
            # moving them. Its axes are laid out in reversed order at creation
            # instead (.plot passes rev(vars) to .createGgalluvialPlot). The
            # engine comes from state rather than being sniffed from the plot's
            # column names, which broke on a variable literally named "x".
            if (direction$reverse && !identical(engine, "ggalluvial")) {
                plot <- plot +
                    ggplot2::scale_x_discrete(limits = function(values) rev(values))
            }
            if (direction$flip)
                plot <- plot + ggplot2::coord_flip()
            plot
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
                    .("Variables not shown"),
                    paste(
                        .fmt(
                            .("Only the first {shown} of {selected} selected variables are plotted, because 'Maximum variables' is set to {max}."),
                            shown = length(plot_vars), selected = length(vars_name), max = max_vars),
                        .fmt(.("Not shown: {variables}"), variables = paste(dropped, collapse = ", ")),
                        .("Raise 'Maximum variables' to include them, or deselect the ones you do not need."),
                        sep = "\n"))
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
                    .("GG Alluvial Engine Not Available"),
                    .("The 'GG Alluvial' plot engine needs the R package 'ggalluvial', which is not installed on this computer, so no alluvial plot can be drawn. What to do next: switch the 'Plot engine' option to 'Easy Alluvial' - it is already installed and shows the same category flows (weighted flows and a separate fill variable are only available in GG Alluvial); or install the package by running install.packages(\"ggalluvial\") in R, then restart jamovi and run the analysis again.")
                )
                return(NULL)
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
                    .("Styling options not applied"),
                    paste(
                        .("Marginal plots are switched on, so the diagram and its histograms are assembled into a fixed grid that cannot take further styling. These settings were left out of this plot: colour palette, theme style, enhanced edge gradients, plot orientation, flow direction, plot subtitle and custom title."),
                        .("Why this matters: changing any of them will not alter the picture on screen while marginal plots are on."),
                        .("What to do next: switch 'Marginal plots' off if you need any of those settings."),
                        sep = "\n")
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
                        .("Fill Variable Not Found"),
                        .fmt(.("Fill variable '{variable}' does not exist in the data."),
                                        variable = requested_fill)
                    )
                    return(NULL)
                }
                fill_var <- requested_fill
            }

            if (engine == "ggalluvial" && has_weight &&
                    weight_var %in% unique(c(plot_vars, fill_var))) {
                private$.addNotice(
                    "ERROR",
                    .("Weight Variable Reused"),
                    .fmt(.("Weight variable '{weight}' must be different from the axis and fill variables."),
                                    weight = weight_var)
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
                    .("Weight Variable Ignored"),
                    paste(
                        .("The weight variable is only supported by the GG Alluvial engine."),
                        .("Switch to the GG Alluvial engine to use weighted flows."),
                        sep = "\n")
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
            coerced_numeric <- missing_vars[vapply(missing_vars, function(v)
                is.numeric(mydata[[v]]), logical(1))]
            mydata <- private$.factorNumeric(mydata, missing_vars)
            if (length(coerced_numeric) > 0) {
                private$.addNotice(
                    "INFO",
                    .("Numeric variables plotted as categories"),
                    paste(
                        .fmt(
                            .("These variables hold numbers and are drawn with their own recorded values as categories: {variables}."),
                            variables = paste(coerced_numeric, collapse = ", ")),
                        .("Why this matters: the drawing engine would otherwise rescale each numeric variable and cut it into equal-width bins, so values coded 1/2/3 would appear on the diagram as bin labels such as LL/M/HH."),
                        .("What to do next: nothing is needed. To combine values into wider groups, recode the variable with Data > Transform before plotting."),
                        sep = "\n")
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
                    .("No Complete Data"),
                    .("All observations have missing values in one or more selected variables. Cannot generate plot.")
                )
                return(NULL)
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
                    .("Complex Visualization"),
                    paste(
                        .fmt(
                            .("The data contain {n} distinct paths through the selected variables. This may produce an overcrowded plot."),
                            n = n_distinct_paths),
                        .("Reduce the number of variables or group infrequent categories."),
                        sep = "\n")
                )
            }

            # Flow table and the commonest path. Built from the row-level data,
            # before any weight aggregation, so `n` is a case count. Under a
            # weight the table describes exactly the rows the ribbons are drawn
            # from: rows with no weight are left out of it, as they are of the
            # diagram, and the reading notice reports them separately.
            weighted <- engine == "ggalluvial" && has_weight
            flow_rows <- if (weighted) mydata[!is.na(mydata[[weight_var]]), , drop = FALSE] else mydata
            path_key <- do.call(paste, c(
                lapply(plot_vars, function(v) as.character(flow_rows[[v]])),
                sep = " \u{2192} "))
            flows <- as.data.frame(table(path = path_key), stringsAsFactors = FALSE)
            names(flows)[2] <- "n"
            flows$n <- as.integer(flows$n)
            if (weighted) {
                w_sum <- tapply(flow_rows[[weight_var]], path_key, sum)
                flows$w <- as.numeric(w_sum[flows$path])
                flows <- flows[order(-flows$w, -flows$n, flows$path), , drop = FALSE]
            } else {
                flows$w <- NA_real_
                flows <- flows[order(-flows$n, flows$path), , drop = FALSE]
            }
            flows$pct <- flows$n / sum(flows$n)
            private$.topFlow <- flows[1, , drop = FALSE]
            if (isTRUE(self$options$showFlowTable)) {
                tbl <- self$results$flowTable
                # The weight column belongs to the engine that uses the weight,
                # not to the mere presence of a weight selection.
                tbl$getColumn("w")$setVisible(weighted)
                # Data-shaped rows: not an .init() row set, so clear before adding
                # (addRow() never checks for an existing rowKey).
                tbl$deleteRows()
                for (i in seq_len(nrow(flows))) {
                    tbl$addRow(rowKey = i, values = list(
                        path = flows$path[i], n = flows$n[i],
                        pct = flows$pct[i], w = flows$w[i]))
                }
                tbl$setNote("base", if (weighted) {
                    .("One row per path through the plotted variables, largest weight first. Cases and percentages count the rows that carry a weight, the rows the diagram is drawn from; Weight total is the sum of the weight variable over those rows, which is what the ribbon width shows.")
                } else {
                    .("One row per path through the plotted variables, commonest first. Percentages are of all plotted cases.")
                })
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

            # A separate fill variable with many levels (a patient ID, a site)
            # colours the flows in that many shades and gives an unreadable
            # legend. The axis variables are already checked by
            # .validateVariableTypes; the fill variable was not.
            if (engine == "ggalluvial" && !fill_var %in% plot_vars &&
                    n_fill_groups > 10) {
                private$.addNotice("STRONG_WARNING", .("Too Many Fill Categories"), paste(
                    .fmt(
                        .("Fill variable '{variable}' has {n} distinct categories, so the flows are coloured in that many shades and the legend will be hard to read."),
                        variable = fill_var, n = n_fill_groups),
                    .("What to do next: choose a fill variable with fewer categories, or group the less frequent categories with Data > Transform."),
                    sep = "\n"))
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
                    .("Colour palette stretched"),
                    paste(
                        .fmt(
                            .("The {palette} palette publishes {cap} distinguishable colours, and this diagram has at least {groups} groups to colour."),
                            palette = private$.paletteLabel[[colorPalette]], cap = palette_cap, groups = n_fill_groups),
                        .("Why this matters: the extra groups are drawn in shades blended between the published colours, so neighbouring groups can look alike and are hard to tell apart in the legend."),
                        .("What to do next: choose Viridis or Plasma, which stay distinguishable over many groups, or group categories with Data > Transform until the palette fits."),
                        sep = "\n")
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
                        .("No Valid Weights"),
                        .("No observations with non-missing weights remain.")
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
            # Every line is one complete translatable sentence (or two joined by
            # a space); nothing is spliced mid-sentence.
            width_text <- if (has_weight) {
                .fmt(.("Ribbon width: the total of the weight variable '{weight}' over the cases following that path."),
                                weight = weight_var)
            } else {
                .("Ribbon width: the number of cases following that path (each case counts once).")
            }

            missing_text <- if (n_incomplete == 0) {
                .("Missing values: no row had a missing value in any plotted variable.")
            } else if (excl) {
                .fmt(.("Missing values: {n} of {total} rows had a missing value in a plotted variable and were removed."),
                                n = n_incomplete, total = n_rows_before)
            } else {
                .fmt(.("Missing values: {n} of {total} rows had a missing value in a plotted variable. Those cells are shown as a '{label}' category, which is drawn like any other category but is not an observed group. Switch on 'Missing-value exclusion (NA)' to drop those rows instead."),
                                n = n_incomplete, total = n_rows_before, label = private$.naLabel)
            }

            if (n_weight_na > 0) {
                missing_text <- paste(missing_text, if (n_incomplete > 0) {
                    .fmt(.("A further {n} row(s) had no value for the weight variable '{weight}' and were removed."),
                                    n = n_weight_na, weight = weight_var)
                } else {
                    .fmt(.("{n} row(s) had no value for the weight variable '{weight}' and were removed."),
                                    n = n_weight_na, weight = weight_var)
                })
            }
            if (n_dropped_incomplete + n_weight_na > 0) {
                missing_text <- paste(missing_text,
                    .fmt(.("The diagram is based on the remaining {n} rows."), n = n_rows_after))
            }

            # The option titles named here are the .a.yaml titles, which the
            # catalogs already carry, so the same msgids are reused.
            if (engine == "ggalluvial") {
                engine_text <- .fmt(
                    .("Ignored by the GG Alluvial engine: {options}."),
                    options = paste(c(.("Fill by"), .("Marginal plots")), collapse = ", "))
            } else {
                engine_text <- .fmt(
                    .("Ignored by the Easy Alluvial engine: {options}."),
                    options = paste(c(.("Fill by (ggalluvial)"), .("Weight variable"), .("Node labels"),
                                      .("Counts on nodes"), .("Sankey styling"), .("Curve type")),
                                    collapse = ", "))
            }

            lines <- c(width_text, missing_text, engine_text)

            # A quotable sentence: the commonest path and its share of cases (and
            # its weight total when that is what the ribbon shows).
            top <- private$.topFlow
            if (!is.null(top) && nrow(top) == 1) {
                # .mainPlotRows already excludes rows with no weight, which is
                # the base the flow table uses too.
                n_cases <- private$.mainPlotRows
                top_text <- if (has_weight && !is.na(top$w)) {
                    .fmt(
                        .("Commonest path: {path} ({n} of {total} cases, {pct}%; weight total {weight}). Switch on 'Flow table' to list every path."),
                        path = top$path, n = top$n, total = n_cases, pct = round(100 * top$pct, 1),
                        weight = round(top$w, 1))
                } else {
                    .fmt(
                        .("Commonest path: {path} ({n} of {total} cases, {pct}%). Switch on 'Flow table' to list every path."),
                        path = top$path, n = top$n, total = n_cases, pct = round(100 * top$pct, 1))
                }
                lines <- c(lines, top_text)
            }

            # The numbers 'Counts on nodes' draws come from the same y aesthetic as
            # the ribbon widths, so under a weight variable they are weight totals.
            if (has_weight && isTRUE(self$options$showCounts)) {
                lines <- c(lines, .("Node numbers: the total of the weight variable in that group, rounded to one decimal place - not a case count."))
            }

            private$.addNotice("INFO", .("How to read this diagram"),
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
            # type (.countCategories). The numeric case hard-stops at the same
            # cutoff as the axis variables, because that many distinct numbers
            # means the column is a measurement and has no meaningful
            # condensation panel; a high-cardinality variable of any type is
            # only hard to read, so it is warned about and still drawn.
            unique_values <- private$.countCategories(cond_data)
            if (is.numeric(cond_data) && unique_values > private$.maxNumericCategories) {
                esc <- htmltools::htmlEscape
                # color: inherit on the heading: a fixed dark amber read at
                # about 2.4:1 on the light pane; the border carries the accent.
                html <- paste0(
                    "<div style='background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; padding: 15px; margin: 10px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0; color: inherit;'>", esc(.("Continuous Condensation Variable")), "</h4>",
                    "<p>", esc(.fmt(
                        .("Condensation variable '{variable}' has {n} distinct numeric values and appears continuous, so no condensation plot was drawn."),
                        variable = cond_var, n = unique_values)), "</p>",
                    "<p>", esc(.("Select a categorical variable, or group the values with Data > Transform first.")), "</p>",
                    "</div>"
                )
                self$results$condensationWarning$setContent(html)
                return(NULL)
            }
            private$.warnHighCardinality(cond_var, cond_data)

            # plot_condensation() bins every numeric column into five equal-width
            # intervals (and warns "bins ... are empty" into Analysis Notes when
            # the recorded values do not fill them), so a Grade coded 1/2/3
            # under-counted its flows. Draw recorded values, as the main diagram
            # does.
            mydata <- private$.factorNumeric(mydata, vars_name)

            mydata <- private$.handleMissingValues(
                mydata,
                vars_name,
                exclude = self$options$excl
            )
            if (nrow(mydata) == 0) {
                self$results$condensationWarning$setContent(
                    htmltools::htmlEscape(.("No complete observations remain for the condensation plot."))
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
                    .("Condensation panel sample"),
                    .fmt(
                        .("The condensation panel is based on {panel} rows; the diagram above uses {diagram}. The two panels require different variables to be recorded, so they drop different rows."),
                        panel = nrow(mydata), diagram = private$.mainPlotRows))
            }

            self$results$condensationWarning$setContent("")
            list(data = mydata, condensation_var = cond_var)
        },

        .run = function() {

            private$.noticeList <- list()
            private$.naLabel <- .("(Missing)")
            private$.mainPlotRows <- NULL
            private$.topFlow <- NULL
            private$.renderNotices()
            self$results$plot$setState(NULL)
            self$results$plot2$setState(NULL)
            self$results$condensationWarning$setContent("")

            # Every user-visible string is wrapped in .(); HTML structure stays
            # outside the wrappers and translated text is escaped on the way in.
            # Plot callbacks do not expose a render-safe cancellation point;
            # data preparation is interrupted between its expensive phases.

            # Input Validation ----

            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                # ToDo Message ----
                esc <- htmltools::htmlEscape
                li <- function(head, body = NULL) {
                    if (is.null(body)) return(paste0("<li>", esc(head), "</li>"))
                    paste0("<li><strong>", esc(head), "</strong> ", esc(body), "</li>")
                }
                box <- function(style, heading, items) {
                    paste0("<div style='", style, "'><h3 style='margin-top: 0;'>", esc(heading), "</h3>",
                           "<ul style='margin-bottom: 0;'>", paste(items, collapse = ""), "</ul></div>")
                }
                todo <- paste0(
                    "<div style='font-family: Arial, sans-serif; color: inherit; padding: 10px;'>",
                    "<h2>", esc(.("Alluvial Diagrams")), "</h2>",
                    "<p>", esc(.("Visualize the flow of categorical data across multiple dimensions.")), "</p>",
                    box("background-color: rgba(33, 144, 255, 0.11); border-left: 4px solid #2196F3; padding: 10px; margin: 10px 0; color: inherit;",
                        .("Quick Start"), c(
                        li(.("Select 2-5 categorical variables (optimal: 3-4)")),
                        li(.("Each variable should have 3-7 categories for best readability")),
                        li(.("For continuous variables, use the categorize function to create groups first")))),
                    box("background-color: rgba(33, 152, 33, 0.07); border-left: 4px solid #4caf50; padding: 10px; margin: 10px 0; color: inherit;",
                        .("Clinical Use Cases"), c(
                        li(.("Patient flow:"), .("Track progression through treatment stages")),
                        li(.("Tumor progression:"), .("Visualize grade/stage transitions")),
                        li(.("Diagnostic pathways:"), .("Show relationships between symptoms, diagnosis and outcomes")),
                        li(.("Demographics:"), .("Explore patterns across age/sex/location categories")))),
                    box("background-color: rgba(255, 203, 33, 0.14); border-left: 4px solid #ffc107; padding: 10px; margin: 10px 0; color: inherit;",
                        .("Tips"), c(
                        li(.("Arrange variables in logical order (e.g., temporal sequence: Grade, Stage, Response)")),
                        li(.("Start with fewer variables and add more once you understand the patterns")),
                        li(.("Use weighted flows (GG Alluvial engine) for aggregated data with frequency counts")),
                        li(.("Switch on marginal histograms to see individual variable distributions")),
                        li(.("Switch on the flow table to list every path with its number of cases")))),
                    "<hr style='margin-top: 15px;'>",
                    "<p style='font-size: 0.9em; text-align: center;'>",
                    esc(.("Ready to begin? Select at least 2 categorical variables from the left panel.")),
                    "</p></div>")

                html <- self$results$todo
                html$setContent(todo)

            } else {
                # Clear the to-do message
                todo <- ""
                html <- self$results$todo
                html$setContent(todo)

                # Use shared validation logic. Every failure branch inside
                # .validateAlluvialInputs() raises its own specific ERROR notice
                # (too few variables / no rows / a continuous variable / a
                # variable not in the data), so nothing generic is added here.
                if (!private$.validateAlluvialInputs())
                    return()

                # Validate condensation variable if provided
                if (!is.null(self$options$condensationvar) &&
                    length(self$options$condensationvar) > 0 &&
                    !(self$options$condensationvar %in% names(self$data))) {

                    private$.addNotice("ERROR", .("Variable Not Found"), .fmt(
                        .("Condensation variable '{variable}' does not exist in the data. Select a valid variable from the available list."),
                        variable = self$options$condensationvar))
                    return()
                }

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

                maxvars <- self$options$maxvars

                direction <- private$.resolveFlowDirection(
                    orient = self$options$orient,
                    flow_direction = self$options$flowDirection
                )

                # Generate plot based on selected engine ----
                if (engine == "ggalluvial") {
                    # Axis order is fixed at creation (see .applyFlowDirection).
                    plot <- private$.createGgalluvialPlot(
                        data = mydata,
                        vars = if (direction$reverse) rev(varsName) else varsName,
                        fill_var = state$fill_var,
                        weight_var = weight_var
                    )
                } else {
                    # Use easyalluvial engine (default). No bins/bin_labels: every
                    # axis variable reaches the engine as a factor (see
                    # .prepareMainPlotState), so nothing is ever binned.
                    plot <- .quietly(easyalluvial::alluvial_wide(
                        data = mydata,
                        max_variables = maxvars,
                        fill_by = fill,
                        verbose = FALSE  # Disabled to prevent console clutter in jamovi
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
                    plot <- private$.applyFlowDirection(plot, direction, engine)

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
                print(private$.messagePlot(paste(
                    private$.wrapText(.("The alluvial diagram could not be drawn.")),
                    "",
                    private$.wrapText(.fmt(.("Reported by R: {message}"), message = conditionMessage(e))),
                    "",
                    private$.wrapText(.("What to try next: switch 'Plot engine' to Easy Alluvial; plot fewer variables; group categories with few cases (Data > Transform); check that every selected variable is still in the data.")),
                    sep = "\n")))
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
                print(private$.messagePlot(paste(
                    private$.wrapText(.("The condensation plot could not be drawn.")),
                    "",
                    private$.wrapText(.fmt(.("Reported by R: {message}"), message = conditionMessage(e))),
                    "",
                    private$.wrapText(.("What to try next: choose a categorical condensation variable; group categories with very few cases (Data > Transform).")),
                    sep = "\n")))
                TRUE
            })
        }
    )
)
