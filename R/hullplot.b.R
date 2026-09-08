#' @title Hull Plot for Group Visualization
#' @return Hull plot using ggforce for cluster and group visualization
#'
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual scale_fill_manual
#' @importFrom ggplot2 theme_minimal theme_classic theme_light theme_dark labs
#' @importFrom ggplot2 scale_color_viridis_d scale_fill_viridis_d theme_bw stat_ellipse
#' @importFrom ggforce geom_mark_hull
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales alpha hue_pal
#' @importFrom htmltools HTML
#' @importFrom dplyr group_by summarise n
#' @importFrom viridis viridis
#' @importFrom rlang .data
#' @importFrom grid unit
hullplotClass <- if (requireNamespace("jmvcore")) R6::R6Class("hullplotClass",
    inherit = hullplotBase,
    private = list(
        .prepared_data = NULL,
        .data_cache_key = NULL,

        # Collected user-facing notices for the current run. Rendered into the
        # `notices` result item, which is declared FIRST in hullplot.r.yaml so
        # data-quality warnings appear ABOVE the figure they qualify. Previously
        # they were buried in the interpretation guide, below the plot and below
        # two optional tables.
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <-
                list(type = type, title = title, content = content)
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                # Clear a panel left over from a previous run.
                self$results$notices$setContent("")
                return()
            }

            # Backgrounds are translucent rgba tints and body text is
            # `color: inherit`, so the panel composites over either jamovi
            # theme. Only the title takes a saturated colour, chosen to read
            # against both a light and a dark ground.
            typeStyles <- list(
                ERROR          = list(color = "#dc2626", bg = "rgba(220, 38, 38, 0.10)", border = "#fca5a5"),
                STRONG_WARNING = list(color = "#ea580c", bg = "rgba(234, 88, 12, 0.10)", border = "#fdba74"),
                WARNING        = list(color = "#ca8a04", bg = "rgba(202, 138, 4, 0.12)", border = "#fde047"),
                INFO           = list(color = "#2563eb", bg = "rgba(37, 99, 235, 0.08)", border = "#93c5fd")
            )

            # Most severe first, regardless of the order they were added in.
            priority <- c(ERROR = 1, STRONG_WARNING = 2, WARNING = 3, INFO = 4)
            types <- vapply(private$.noticeList, function(n) n$type, character(1))
            ordered <- private$.noticeList[order(priority[types])]

            html <- "<div style='margin: 10px 0;'>"
            for (notice in ordered) {
                style <- typeStyles[[notice$type]]
                if (is.null(style)) style <- typeStyles$INFO
                html <- paste0(html,
                    "<div style='background-color: ", style$bg, "; ",
                    "border-left: 4px solid ", style$border, "; ",
                    "padding: 12px; margin: 8px 0; border-radius: 4px; color: inherit;'>",
                    "<strong style='color: ", style$color, ";'>",
                    htmltools::htmlEscape(notice$title), "</strong><br>",
                    "<span style='color: inherit;'>",
                    htmltools::htmlEscape(notice$content), "</span>",
                    "</div>")
            }
            html <- paste0(html, "</div>")

            self$results$notices$setContent(html)
        },

        .prepare_data = function() {
            # CRITICAL FIX: Create cache key including data CONTENT hash
            # This prevents stale data after filtering/editing
            data_size <- if (is.null(self$data)) 0 else nrow(self$data)

            # Calculate hash of relevant data columns to detect content changes
            data_hash <- ""
            if (!is.null(self$data) && data_size > 0) {
                # Select only the columns that will be used in the plot
                relevant_cols <- c(
                    self$options$x_var,
                    self$options$y_var,
                    self$options$group_var,
                    self$options$color_var,
                    self$options$size_var
                )
                relevant_cols <- relevant_cols[!sapply(relevant_cols, is.null)]
                relevant_cols <- relevant_cols[relevant_cols != ""]
                relevant_cols <- relevant_cols[relevant_cols %in% names(self$data)]

                if (length(relevant_cols) > 0) {
                    relevant_data <- self$data[, relevant_cols, drop = FALSE]
                    # Use digest for fast, reliable hashing
                    if (requireNamespace("digest", quietly = TRUE)) {
                        data_hash <- digest::digest(relevant_data, algo = "xxhash64")
                    } else {
                        # Base-R fallback fingerprint (no digest dependency):
                        # dimensions plus a per-column checksum. Not cryptographic,
                        # but deterministic and sufficient as a cache-invalidation key.
                        col_sig <- vapply(relevant_data, function(col) {
                            if (is.numeric(col)) {
                                paste0(sum(as.numeric(col), na.rm = TRUE),
                                       "_", sum(!is.na(col)))
                            } else {
                                paste0(length(unique(col)),
                                       "_", sum(nchar(as.character(col)), na.rm = TRUE))
                            }
                        }, character(1))
                        data_hash <- paste0(
                            nrow(relevant_data), "x", ncol(relevant_data), ":",
                            paste(col_sig, collapse = "|")
                        )
                    }
                }
            }

            cache_key <- paste(
                self$options$x_var,
                self$options$y_var,
                self$options$group_var,
                self$options$color_var,
                self$options$size_var,
                data_size,
                data_hash,  #  Now includes actual data content
                sep = "|"
            )

            # Return cached data if key matches
            if (!is.null(private$.data_cache_key) && private$.data_cache_key == cache_key && !is.null(private$.prepared_data)) {
                return(private$.prepared_data)
            }

            # Check if required variables have been selected
            if (is.null(self$options$x_var) || is.null(self$options$y_var) || is.null(self$options$group_var) ||
                self$options$x_var == "" || self$options$y_var == "" || self$options$group_var == "") {
                return(NULL)
            }

            # Validate dataset exists and has data
            if (is.null(self$data)) {
                return(NULL)
            }

            if (nrow(self$data) == 0) {
                return(NULL)
            }

            # Get data and variables
            dataset <- self$data
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            group_var <- self$options$group_var
            color_var <- self$options$color_var
            size_var <- self$options$size_var

            # Validate variable names exist in dataset
            required_vars <- c(x_var, y_var, group_var)
            missing_vars <- required_vars[!required_vars %in% names(dataset)]
            if (length(missing_vars) > 0) {
                jmvcore::reject(.("Variables not found in dataset: {missing}"), missing = paste(missing_vars, collapse = ", "))
            }

            # Guard against the same variable appearing twice among x/y/group,
            # which creates duplicate column names in plot_data. `[[<-` then
            # writes to the FIRST matching column, so selecting the X variable
            # as the grouping variable silently converted the X axis to a
            # factor and plotted the wrong data with no error.
            if (x_var == y_var) {
                jmvcore::reject(.("X-Axis and Y-Axis variables must be different (both are set to '{var}')."), var = x_var)
            }
            if (group_var == x_var || group_var == y_var) {
                jmvcore::reject(
                    .("The Grouping Variable must differ from the X and Y variables ('{var}' is used for both). Grouping by an axis variable would place every distinct value in its own hull."),
                    var = group_var)
            }

            # Prepare data - create subset with required variables
            plot_data <- data.frame(
                x = dataset[[x_var]],
                y = dataset[[y_var]],
                group = dataset[[group_var]]
            )
            names(plot_data) <- c(x_var, y_var, group_var)

            # Add color variable if specified and different from group variable
            color_mapping <- group_var  # Default to group variable
            if (!is.null(color_var) && color_var != "" && color_var != group_var && color_var %in% names(dataset)) {
                plot_data[[paste0("color_", color_var)]] <- dataset[[color_var]]
                color_mapping <- paste0("color_", color_var)
            }

            # Add size variable if specified
            if (!is.null(size_var) && size_var != "" && size_var %in% names(dataset)) {
                plot_data[[paste0("size_", size_var)]] <- dataset[[size_var]]
            }

            # Remove rows with missing values in required variables. Also filter
            # NA in any active optional color/size column so points do not render
            # as grey (NA colour) or get silently dropped with a warning (NA size).
            required_cols <- c(x_var, y_var, group_var)
            if (color_mapping != group_var && color_mapping %in% names(plot_data)) {
                required_cols <- c(required_cols, color_mapping)
            }
            size_col <- if (!is.null(size_var) && size_var != "") paste0("size_", size_var) else NULL
            if (!is.null(size_col) && size_col %in% names(plot_data)) {
                required_cols <- c(required_cols, size_col)
            }

            # complete.cases() treats only NA as missing, so Inf/-Inf survived
            # into the hull, the axis scales and - worse - the group statistics
            # ("Inf +/- NaN") and the copy-ready summary, where an infinite
            # centroid distance made every comparison read "well-separated".
            # is.finite() rules out NA, NaN, Inf and -Inf for numeric columns in
            # one step; factors keep the plain NA test.
            n_before <- nrow(plot_data)
            keep <- rep(TRUE, n_before)
            n_nonfinite <- 0L
            for (cc in required_cols) {
                v <- plot_data[[cc]]
                if (is.numeric(v)) {
                    n_nonfinite <- n_nonfinite + sum(!is.na(v) & !is.finite(v))
                    keep <- keep & is.finite(v)
                } else {
                    keep <- keep & !is.na(v)
                }
            }
            plot_data <- plot_data[keep, , drop = FALSE]
            n_excluded <- n_before - nrow(plot_data)

            if (nrow(plot_data) == 0) {
                jmvcore::reject(.("No complete cases found for the selected variables."))
            }

            # Convert group variable to factor. droplevels() matters: a level
            # whose every row was excluded above survives as an empty level, and
            # downstream code counts levels() - producing "3 groups" for a
            # two-group plot, an unused palette colour, and an outlier-panel row
            # for a group that is not in the data.
            plot_data[[group_var]] <- droplevels(as.factor(plot_data[[group_var]]))

            # Convert color mapping variable to factor if it's not the group variable
            if (color_mapping != group_var && color_mapping %in% names(plot_data)) {
                plot_data[[color_mapping]] <- droplevels(as.factor(plot_data[[color_mapping]]))
            }

            # Validate group sizes and add warnings
            group_counts <- table(plot_data[[group_var]])
            min_group_size <- 3
            small_groups <- names(group_counts[group_counts < min_group_size])

            # Each entry carries its own severity so .run() can hand it straight
            # to .addNotice() without a name-to-severity lookup that would have to
            # be kept in step with this block.
            validation_warnings <- list()

            # Rows dropped above were previously invisible: the interpretation
            # panel reported the surviving N with no indication that anything
            # had been removed.
            if (n_excluded > 0) {
                validation_warnings$excluded <- list(
                    type = "STRONG_WARNING",
                    title = "Rows excluded from the plot",
                    content = sprintf(
                        "%d of %d rows (%s%%) were excluded because a selected variable was missing%s. %d rows were plotted.",
                        n_excluded, n_before,
                        base::format(round(100 * n_excluded / n_before, 1)),
                        if (n_nonfinite > 0)
                            sprintf(" or not a finite number (%d infinite/undefined value(s) found)", n_nonfinite)
                        else "",
                        nrow(plot_data)))
            }

            if (length(small_groups) > 0) {
                validation_warnings$small_groups <- list(
                    type = "WARNING",
                    title = "Groups too small for a meaningful hull",
                    content = sprintf(
                        "Groups with fewer than %d points: %s. Hull boundaries may not be meaningful for these groups.",
                        min_group_size, paste(small_groups, collapse = ", ")))
            }

            if (length(levels(plot_data[[group_var]])) > 10) {
                validation_warnings$many_groups <- list(
                    type = "WARNING",
                    title = "Many groups",
                    content = "More than 10 groups detected. Consider grouping similar categories for clearer visualization.")
            }

            # The colour variable is cast to a factor and drawn with a discrete
            # palette, so a continuous variable dropped here produces one legend
            # key per distinct value rather than a colour gradient.
            if (color_mapping != group_var && color_mapping %in% names(plot_data)) {
                n_color_levels <- length(levels(plot_data[[color_mapping]]))
                if (n_color_levels > 10) {
                    validation_warnings$many_colors <- list(
                        type = "WARNING",
                        title = "Colour variable has many distinct values",
                        content = sprintf(
                            "The colour variable '%s' has %d distinct values, each drawn as a separate legend entry. Colour is treated as categorical here; choose a categorical variable, or leave it empty to colour by the grouping variable.",
                            color_var, n_color_levels))
                }
            }

            # Cache the prepared data
            prepared_data <- list(
                data = plot_data,
                x_var = x_var,
                y_var = y_var,
                group_var = group_var,
                color_mapping = color_mapping,
                size_var = size_var,
                validation_warnings = validation_warnings
            )

            private$.prepared_data <- prepared_data
            private$.data_cache_key <- cache_key

            return(prepared_data)
        },

        .run = function() {

            # Reset per run: .addNotice() appends, so without this the same
            # notice would accumulate once per run cycle. on.exit() renders them
            # on EVERY exit path -- .run() has three early returns below, and a
            # trailing .renderNotices() would silently drop the notices on all
            # three.
            private$.noticeList <- list()
            on.exit(private$.renderNotices(), add = TRUE)

            # Check if required variables have been selected
            if (is.null(self$options$x_var) || is.null(self$options$y_var) || is.null(self$options$group_var) ||
                self$options$x_var == "" || self$options$y_var == "" || self$options$group_var == "") {
                intro_msg <- "
                <div style='background-color: rgba(33, 152, 239, 0.13); padding: 20px; border-radius: 8px; margin: 20px 0; color: inherit;'>
                <h3 style='color: inherit; margin-top: 0;'> Welcome to Hull Plot Visualization!</h3>
                <p><strong>Create stunning cluster visualizations</strong> using ggforce hull polygons</p>
                <p>Based on R-Bloggers tutorial: 'Make a Hull Plot to Visualize Clusters in ggplot2'</p>

                <h4 style='color: inherit;'>Quick Start:</h4>
                <ol>
                <li><strong>X-Axis Variable:</strong> Select a continuous variable for horizontal axis</li>
                <li><strong>Y-Axis Variable:</strong> Select a continuous variable for vertical axis</li>
                <li><strong>Grouping Variable:</strong> Choose categorical variable to define hull boundaries</li>
                <li><strong>Optional:</strong> Add color and size variables for enhanced visualization</li>
                <li><strong>Customize:</strong> Adjust hull appearance, colors, and themes</li>
                </ol>

                <h4 style='color: inherit;'>Perfect For:</h4>
                <ul>
                <li><strong>Customer Segmentation:</strong> Visualize customer groups and segments</li>
                <li><strong>Clinical Clusters:</strong> Show patient subgroups in clinical research</li>
                <li><strong>Data Exploration:</strong> Identify natural groupings in your data</li>
                <li><strong>Research Presentation:</strong> Professional publication-ready plots</li>
                </ul>

                <p style='font-size: 12px; color: inherit; opacity: 0.75; margin-top: 20px;'>
                 <em>Hull plots use ggforce::geom_mark_hull() to create polygonal boundaries around grouped data points</em>
                </p>
                </div>"

                self$results$todo$setContent(intro_msg)
                return()
            } else {
                self$results$todo$setContent("")
            }

            # Safely require ggforce and concaveman
            if (!requireNamespace("ggforce", quietly = TRUE)) {
                private$.addNotice(
                    "ERROR",
                    "The ggforce package is required",
                    "Hull plots are drawn with ggforce::geom_mark_hull(). Install it with install.packages('ggforce') and re-run the analysis.")
                return()
            }

            # Check for V8/concaveman availability and prepare note
            v8_available <- requireNamespace("V8", quietly = TRUE)
            concaveman_available <- requireNamespace("concaveman", quietly = TRUE)
            if (!(v8_available && concaveman_available)) {
                private$.addNotice(
                    "INFO",
                    "Concave hulls unavailable",
                    "V8 and concaveman are not both installed, so convex hulls are drawn instead and the Hull Shape setting has no effect. Install them with install.packages(c('V8', 'concaveman')) for concave hulls.")
            }

            # Prepare data using cached method
            prepared <- private$.prepare_data()
            if (is.null(prepared)) {
                # All three variables are selected (checked above), so reaching
                # here means the dataset is empty or absent. Previously every
                # panel was simply left blank with no explanation.
                private$.addNotice(
                    "ERROR",
                    "No data to plot",
                    "The dataset contains no rows. Check any row filters that are active, and confirm the data has been loaded.")
                return()
            }

            plot_data <- prepared$data
            x_var <- prepared$x_var
            y_var <- prepared$y_var
            group_var <- prepared$group_var

            # Surface data-quality findings above the plot rather than inside the
            # interpretation guide, which renders below it.
            for (w in prepared$validation_warnings)
                private$.addNotice(w$type, w$title, w$content)

            # stat_ellipse() fits a multivariate t and needs at least 4 points; for
            # smaller groups it silently draws nothing and emits "Too few points to
            # calculate an ellipse" plus a "Removed 1 row" warning into jamovi's
            # Analysis Notes, where it reads as unexplained package chatter. Say it
            # here instead, and muffle those two in .plot().
            if (self$options$confidence_ellipses) {
                ellipse_counts <- table(plot_data[[group_var]])
                too_small <- names(ellipse_counts[ellipse_counts < 4])
                if (length(too_small) > 0) {
                    private$.addNotice(
                        "WARNING",
                        "Some groups have no data ellipse",
                        sprintf(
                            "A 95 percent data ellipse needs at least 4 points, so no ellipse is drawn for: %s. The hull and the points for these groups are unaffected.",
                            paste(too_small, collapse = ", ")))
                }
            }

            # Generate group statistics if requested
            if (self$options$show_statistics) {
                stats_html <- private$.generate_group_statistics(plot_data, x_var, y_var, group_var)
                self$results$statistics$setContent(stats_html)
            }

            # Generate outlier analysis if requested
            if (self$options$outlier_detection) {
                outlier_html <- private$.generate_outlier_analysis(plot_data, x_var, y_var, group_var)
                self$results$outliers$setContent(outlier_html)
            }

            # Generate natural language summary if requested
            if (self$options$show_summary) {
                summary_html <- private$.generate_natural_summary(plot_data, x_var, y_var, group_var)
                self$results$summary$setContent(summary_html)
            }

            # Generate assumptions guide if requested
            if (self$options$show_assumptions) {
                assumptions_html <- private$.generate_assumptions_guide()
                self$results$assumptions$setContent(assumptions_html)
            }

            # Data-quality warnings and the convex-hull fallback are notices now
            # (rendered above the plot), so the guide carries explanation only.
            self$results$interpretation$setContent(
                private$.generate_interpretation_guide(plot_data, x_var, y_var, group_var))

            # Set state for plot function. Appearance options are NOT copied in:
            # .plot() reads self$options directly, and every appearance option is
            # already listed in the plot's clearWith in hullplot.r.yaml, which is
            # what triggers the re-render.
            self$results$plot$setState(prepared)

        },

        .plot = function(image, ggtheme, theme, ...) {

            # Get data from image state (set by .run() method)
            prepared <- image$state
            if (is.null(prepared)) {
                return(FALSE)  # No state data available
            }

            plot_data <- prepared$data
            x_var <- prepared$x_var
            y_var <- prepared$y_var
            group_var <- prepared$group_var
            color_mapping <- prepared$color_mapping
            size_var <- prepared$size_var

            # Check V8/concaveman availability once; concave hulls need both.
            # When either is missing we fall back to convex hulls (geom_polygon)
            # instead of ggforce::geom_mark_hull.
            v8_available <- requireNamespace("V8", quietly = TRUE)
            concaveman_available <- requireNamespace("concaveman", quietly = TRUE)
            use_fallback_hull <- !(v8_available && concaveman_available)

            # Concavity applies only to the ggforce geom_mark_hull path, which is
            # only taken when concaveman is available, so no fallback adjustment
            # is needed here (it is unused on the convex-fallback path).
            hull_concavity <- self$options$hull_concavity

            # Hull padding as a FRACTION OF THE PANEL ("npc"), not millimetres.
            # The option is declared 0-1 with a 0.05 default, and ggforce's own
            # default is unit(5, "mm"); handing 0-1 to "mm" made the control
            # inert - sweeping the entire range grew the inked hull area by 68
            # pixels out of 374,400 (0.018%) on an 800x600 render. In "npc" the
            # same range spans a visible 0-100% of the panel, which is what the
            # label "Higher values create larger hulls" promises.
            hull_expand <- grid::unit(self$options$hull_expand, "npc")

            # Create base plot
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(.data[[x_var]], .data[[y_var]]))

            if (use_fallback_hull) {
                # Build convex hull polygons per group using chull
                split_groups <- split(plot_data, plot_data[[group_var]])
                hull_list <- lapply(split_groups, function(df) {
                    if (nrow(df) < 3) return(df)
                    idx <- grDevices::chull(df[[x_var]], df[[y_var]])
                    df[idx, , drop = FALSE]
                })
                hull_df <- do.call(rbind, hull_list)

                p <- p + ggplot2::geom_polygon(
                    data = hull_df,
                    ggplot2::aes(.data[[x_var]], .data[[y_var]], fill = .data[[group_var]], group = .data[[group_var]]),
                    alpha = self$options$hull_alpha,
                    color = NA
                )

                if (self$options$show_labels) {
                    # Label at group centroids
                    centroids <- stats::aggregate(
                        hull_df[c(x_var, y_var)],
                        list(group = hull_df[[group_var]]),
                        mean
                    )
                    names(centroids)[names(centroids) == "group"] <- group_var
                    p <- p + ggplot2::geom_text(
                        data = centroids,
                        ggplot2::aes(.data[[x_var]], .data[[y_var]], label = .data[[group_var]]),
                        fontface = "bold",
                        color = "black"
                    )
                }
            } else {
                # Add hull polygons via ggforce with proper concavity handling
                if (self$options$show_labels) {
                    p <- p + ggforce::geom_mark_hull(
                        ggplot2::aes(fill = .data[[group_var]], label = .data[[group_var]]),
                        concavity = hull_concavity,
                        expand = hull_expand,
                        alpha = self$options$hull_alpha,
                        show.legend = TRUE
                    )
                } else {
                    p <- p + ggforce::geom_mark_hull(
                        ggplot2::aes(fill = .data[[group_var]]),
                        concavity = hull_concavity,
                        expand = hull_expand,
                        alpha = self$options$hull_alpha,
                        show.legend = TRUE
                    )
                }
            }
            
            # Add confidence ellipses if requested
            if (self$options$confidence_ellipses) {
                if (color_mapping == group_var) {
                    # Points and ellipses share the same colour scale (both driven
                    # by group_var), so mapping colour is safe.
                    p <- p + ggplot2::stat_ellipse(
                        ggplot2::aes(color = .data[[group_var]]),
                        level = 0.95,
                        linetype = "dashed",
                        linewidth = 0.8
                    )
                } else {
                    # A separate color_var drives the point colour scale. Mapping
                    # ellipse colour to group_var as well would force the single
                    # discrete colour scale to cover the UNION of both variables'
                    # levels, triggering "Insufficient values in manual scale".
                    # Instead group ellipses by group_var but draw a neutral colour.
                    p <- p + ggplot2::stat_ellipse(
                        ggplot2::aes(group = .data[[group_var]]),
                        level = 0.95,
                        linetype = "dashed",
                        linewidth = 0.8,
                        color = "grey30"
                    )
                }
            }
            
            # Add points - fix aes construction
            if (!is.null(size_var) && size_var != "" && paste0("size_", size_var) %in% names(plot_data)) {
                p <- p + ggplot2::geom_point(
                    ggplot2::aes(color = .data[[color_mapping]], size = .data[[paste0("size_", size_var)]]),
                    alpha = self$options$point_alpha
                )
            } else {
                p <- p + ggplot2::geom_point(
                    ggplot2::aes(color = .data[[color_mapping]]),
                    alpha = self$options$point_alpha,
                    size = self$options$point_size
                )
            }
            
            # CRITICAL FIX: Calculate correct number of levels for each aesthetic
            # - Hulls (fill) use group_var levels
            # - Points (color) use color_mapping levels (could be different variable)
            n_groups <- length(levels(plot_data[[group_var]]))
            n_colors <- length(levels(plot_data[[color_mapping]]))

            # Generate color palettes with correct sizes
            fill_palette <- private$.get_color_palette(n_groups)
            color_palette <- if (color_mapping == group_var) {
                fill_palette  # Same variable - reuse palette
            } else {
                private$.get_color_palette(n_colors)  # Different variable - separate palette
            }

            # Apply fill scale for hulls (always based on group_var)
            p <- p + ggplot2::scale_fill_manual(
                values = fill_palette,
                name = group_var
            )

            # Apply colour scale for points (based on color_mapping)
            p <- p + ggplot2::scale_colour_manual(
                values = color_palette,
                name = if (color_mapping == group_var) group_var else self$options$color_var
            )
            
            # Apply theme
            p <- p + private$.get_plot_theme()
            
            # Add labels
            x_label <- if (self$options$x_label != "") self$options$x_label else x_var
            y_label <- if (self$options$y_label != "") self$options$y_label else y_var
            plot_title <- if (self$options$plot_title != "") self$options$plot_title else "Hull Plot - Group Visualization"
            
            # Legend titles are set by scale_fill_manual()/scale_colour_manual()
            # above (they name the variable). Setting fill=/color= here would
            # override those with a generic "Groups".
            p <- p + ggplot2::labs(
                title = plot_title,
                x = x_label,
                y = y_label
            )

            # Add caption when falling back to convex hulls
            if (use_fallback_hull) {
                p <- p + ggplot2::labs(
                    caption = "Concave hulls unavailable (install V8 + concaveman); showing convex hulls"
                )
            }
            
            # Handle size legend (only when a size aesthetic was actually mapped,
            # matching the geom_point branch above)
            if (!is.null(size_var) && size_var != "" && paste0("size_", size_var) %in% names(plot_data)) {
                p <- p + ggplot2::labs(size = size_var)
            }
            
            # Muffle ONLY the two known ggplot2 notices that the notice panel now
            # explains (see .run()). Everything else is left to reach the user:
            # a blanket suppressWarnings() here would hide real render failures.
            withCallingHandlers(
                print(p),
                message = function(cond) {
                    if (grepl("Too few points to calculate an ellipse", conditionMessage(cond), fixed = TRUE))
                        invokeRestart("muffleMessage")
                },
                warning = function(cond) {
                    if (grepl("containing missing values or values outside the scale range",
                              conditionMessage(cond), fixed = TRUE))
                        invokeRestart("muffleWarning")
                })
            TRUE
        },

        .get_color_palette = function(n_colors) {
            palette_name <- self$options$color_palette
            
            if (palette_name == "viridis") {
                return(viridis::viridis(n_colors))
            } else if (palette_name == "clinical") {
                clinical_colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#593E2C", "#8E6C8A")
                return(rep(clinical_colors, length.out = n_colors))
            } else if (palette_name %in% c("set1", "set2", "dark2")) {
                palette_r_name <- switch(palette_name,
                    "set1" = "Set1",
                    "set2" = "Set2", 
                    "dark2" = "Dark2"
                )
                if (n_colors <= 8) {
                    return(RColorBrewer::brewer.pal(min(max(3, n_colors), 8), palette_r_name))
                } else {
                    base_colors <- RColorBrewer::brewer.pal(8, palette_r_name)
                    return(grDevices::colorRampPalette(base_colors)(n_colors))
                }
            } else {
                # Default ggplot2 colors
                return(scales::hue_pal()(n_colors))
            }
        },

        .get_plot_theme = function() {
            theme_name <- self$options$plot_theme
            
            switch(theme_name,
                "minimal" = ggplot2::theme_minimal(),
                "classic" = ggplot2::theme_classic(),
                "light" = ggplot2::theme_light(),
                "dark" = ggplot2::theme_dark(),
                "clinical" = ggplot2::theme_minimal() + 
                    ggplot2::theme(
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.border = ggplot2::element_rect(fill = NA, color = "grey20"),
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        legend.title = ggplot2::element_text(size = 11, face = "bold")
                    ),
                ggplot2::theme_minimal()  # fallback
            )
        },

        # sd() is NA for a single observation, which rendered as a bare
        # "5 \u00b1 NA". Report the absence rather than printing NA.
        .meanSd = function(mean_value, sd_value) {
            if (is.na(sd_value))
                return(paste0(mean_value, " (SD not estimable, n = 1)"))
            paste0(mean_value, " \u00b1 ", sd_value)
        },

        .generate_group_statistics = function(data, x_var, y_var, group_var) {
            # Calculate group statistics
            group_stats <- data %>%
                dplyr::group_by(!!rlang::sym(group_var)) %>%
                dplyr::summarise(
                    n = dplyr::n(),
                    x_mean = round(mean(!!rlang::sym(x_var), na.rm = TRUE), 2),
                    x_sd = round(sd(!!rlang::sym(x_var), na.rm = TRUE), 2),
                    y_mean = round(mean(!!rlang::sym(y_var), na.rm = TRUE), 2),
                    y_sd = round(sd(!!rlang::sym(y_var), na.rm = TRUE), 2),
                    .groups = 'drop'
                )
            
            # Create HTML table
            stats_html <- paste0(
                "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin-bottom: 20px; color: inherit;'>",
                "<h3 style='color: inherit; margin-top: 0;'> Group Statistics Summary</h3>",
                "<table style='width: 100%; border-collapse: collapse; font-family: Arial, sans-serif;'>",
                "<thead><tr style='background-color: inherit; opacity: 0.75; color: white;'>",
                "<th style='padding: 12px; border: 1px solid #dee2e6;'>Group</th>",
                "<th style='padding: 12px; border: 1px solid #dee2e6;'>N</th>",
                "<th style='padding: 12px; border: 1px solid #dee2e6;'>", htmltools::htmlEscape(x_var), " Mean \u00b1 SD</th>",
                "<th style='padding: 12px; border: 1px solid #dee2e6;'>", htmltools::htmlEscape(y_var), " Mean \u00b1 SD</th>",
                "</tr></thead><tbody>"
            )
            
            for (i in seq_len(nrow(group_stats))) {
                row_bg <- if (i %% 2 == 0) "rgba(127, 127, 127, 0.10)" else "transparent"
                stats_html <- paste0(stats_html,
                    "<tr style='background-color: ", row_bg, ";'>",
                    "<td style='padding: 10px; border: 1px solid #dee2e6;'><strong>", htmltools::htmlEscape(group_stats[[group_var]][i]), "</strong></td>",
                    "<td style='padding: 10px; border: 1px solid #dee2e6; text-align: center;'>", group_stats$n[i], "</td>",
                    "<td style='padding: 10px; border: 1px solid #dee2e6; text-align: center;'>", private$.meanSd(group_stats$x_mean[i], group_stats$x_sd[i]), "</td>",
                    "<td style='padding: 10px; border: 1px solid #dee2e6; text-align: center;'>", private$.meanSd(group_stats$y_mean[i], group_stats$y_sd[i]), "</td>",
                    "</tr>"
                )
            }
            
            stats_html <- paste0(stats_html, 
                "</tbody></table>",
                "<p style='font-size: 12px; color: inherit; opacity: 0.75; margin-top: 15px;'>",
                "<em>Statistics calculated for ", nrow(data), " complete observations across ", nrow(group_stats), " groups.</em>",
                "</p></div>"
            )
            
            return(stats_html)
        },

        .generate_outlier_analysis = function(data, x_var, y_var, group_var) {
            # Simple outlier detection using IQR method within groups
            outliers_list <- list()
            # Quartiles are essentially the data extremes below this n, so an IQR
            # outlier count would be statistically meaningless; report "n too small"
            # instead of a definite number for such groups.
            min_group_n <- 5

            for (group in levels(data[[group_var]])) {
                group_data <- data[data[[group_var]] == group, ]

                if (nrow(group_data) < min_group_n) {
                    outliers_list[[group]] <- NA_integer_
                    next
                }

                # X variable outliers
                x_q1 <- quantile(group_data[[x_var]], 0.25, na.rm = TRUE)
                x_q3 <- quantile(group_data[[x_var]], 0.75, na.rm = TRUE)
                x_iqr <- x_q3 - x_q1
                x_outliers <- which(group_data[[x_var]] < (x_q1 - 1.5 * x_iqr) | group_data[[x_var]] > (x_q3 + 1.5 * x_iqr))
                
                # Y variable outliers
                y_q1 <- quantile(group_data[[y_var]], 0.25, na.rm = TRUE)
                y_q3 <- quantile(group_data[[y_var]], 0.75, na.rm = TRUE)
                y_iqr <- y_q3 - y_q1
                y_outliers <- which(group_data[[y_var]] < (y_q1 - 1.5 * y_iqr) | group_data[[y_var]] > (y_q3 + 1.5 * y_iqr))
                
                # Combined outliers
                all_outliers <- unique(c(x_outliers, y_outliers))
                outliers_list[[group]] <- length(all_outliers)
            }
            
            outlier_html <- paste0(
                "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 20px; border-radius: 8px; margin-bottom: 20px; color: inherit;'>",
                "<h3 style='color: inherit; margin-top: 0;'> Outlier Detection (IQR Method)</h3>",
                "<ul>"
            )
            
            total_outliers <- 0
            for (group in names(outliers_list)) {
                count <- outliers_list[[group]]
                if (is.na(count)) {
                    outlier_html <- paste0(outlier_html,
                        "<li><strong>", htmltools::htmlEscape(group), ":</strong> n too small for reliable outlier detection (n &lt; ", min_group_n, ")</li>"
                    )
                } else {
                    total_outliers <- total_outliers + count
                    outlier_html <- paste0(outlier_html,
                        "<li><strong>", htmltools::htmlEscape(group), ":</strong> ", count, " potential outliers detected</li>"
                    )
                }
            }
            
            outlier_html <- paste0(outlier_html,
                "</ul>",
                "<p><strong>Total potential outliers in assessed groups:</strong> ", total_outliers, "</p>",
                "<p style='font-size: 12px; color: inherit; margin-top: 15px;'>",
                "<em>Outliers are points beyond 1.5 \u00d7 IQR from Q1/Q3. The rule is applied to ",
                "<strong>each axis separately</strong> and the two results are combined, so it finds points with an ",
                "extreme X value or an extreme Y value. It does <strong>not</strong> test the X-Y combination: a point ",
                "that sits far off the pattern the other points follow - visibly outside its hull - is not flagged if ",
                "both of its coordinates are individually unremarkable. Read the plot alongside these counts, and ",
                "consider investigating flagged points for data quality or interesting patterns.</em>",
                "</p></div>"
            )
            
            return(outlier_html)
        },

        .generate_interpretation_guide = function(data, x_var, y_var, group_var) {
            n_groups <- length(levels(data[[group_var]]))
            n_total <- nrow(data)

            interpretation_html <- paste0(
                "<div style='background-color: rgba(33, 163, 188, 0.21); padding: 20px; border-radius: 8px; color: inherit;'>",
                "<h3 style='color: inherit; margin-top: 0;'> Hull Plot Interpretation Guide</h3>",

                "<h4 style='color: inherit;'>Plot Summary:</h4>",
                "<ul>",
                "<li><strong>Variables:</strong> ", htmltools::htmlEscape(x_var), " (X-axis) vs ", htmltools::htmlEscape(y_var), " (Y-axis)</li>",
                "<li><strong>Groups:</strong> ", n_groups, if (n_groups == 1) " group" else " groups", " defined by ", htmltools::htmlEscape(group_var), "</li>",
                "<li><strong>Observations:</strong> ", n_total, " data points</li>",
                "</ul>",

                "<h4 style='color: inherit;'>How to Read Hull Plots:</h4>",
                "<ul>",
                "<li><strong>Data ellipses:</strong> Optional 95% ellipses describe model-based data dispersion, not confidence regions for the group means.</li>",
                "<li><strong>Hull Boundaries:</strong> Polygonal areas show the extent of each group</li>",
                "<li><strong>Overlap:</strong> Overlapping hulls indicate similar characteristics between groups</li>",
                "<li><strong>Separation:</strong> Distinct hulls suggest clear group differences</li>",
                "<li><strong>Point Density:</strong> Clustered points within hulls show group cohesion</li>",
                "</ul>",

                "<h4 style='color: inherit;'>Clinical/Research Applications:</h4>",
                "<ul>",
                "<li><strong>Patient Segmentation:</strong> Identify distinct patient subgroups</li>",
                "<li><strong>Treatment Response:</strong> Visualize how different treatments cluster</li>",
                "<li><strong>Biomarker Analysis:</strong> Show relationships between biomarkers and outcomes</li>",
                "<li><strong>Quality Control:</strong> Detect unusual patterns or outliers</li>",
                "</ul>",

                "<p style='font-size: 12px; color: inherit; margin-top: 15px;'>",
                "<em> Hull plots are excellent for presentations and publications as they clearly show group boundaries and relationships.</em>",
                "</p></div>"
            )

            return(interpretation_html)
        },

        .generate_natural_summary = function(data, x_var, y_var, group_var) {
            n_groups <- length(levels(data[[group_var]]))
            n_total <- nrow(data)

            # Calculate a descriptive separation measure in within-group SD units.
            # NOTE: This is a DESCRIPTIVE HEURISTIC, not a validated statistical test
            group_stats <- data %>%
                dplyr::group_by(!!rlang::sym(group_var)) %>%
                dplyr::summarise(
                    x_mean = mean(!!rlang::sym(x_var), na.rm = TRUE),
                    y_mean = mean(!!rlang::sym(y_var), na.rm = TRUE),
                    x_sd = sd(!!rlang::sym(x_var), na.rm = TRUE),
                    y_sd = sd(!!rlang::sym(y_var), na.rm = TRUE),
                    n = dplyr::n(),
                    .groups = 'drop'
                )

            if (n_groups < 2) {
                # Single group - no inter-group distances to calculate
                separation_quality <- "single cohort (no comparison available)"
            } else {
                # Standardize each axis separately; changing units on one axis must
                # not change the separation descriptor. Require estimable within-group SDs.
                axis_sd <- c(mean(group_stats$x_sd, na.rm = TRUE),
                             mean(group_stats$y_sd, na.rm = TRUE))
                discrim_index <- NA_real_
                if (all(is.finite(axis_sd) & axis_sd > 0)) {
                    centres <- as.matrix(group_stats[c("x_mean", "y_mean")])
                    centres <- sweep(centres, 2, axis_sd, "/")
                    # The CLOSEST pair decides whether the groups are actually
                    # distinguishable. mean() let one distant group mask a fully
                    # overlapping pair: with two identical groups plus a far
                    # third, the mean of all pairs was 20.5 while the closest
                    # pair was 0.10, and the summary - offered as copy-ready
                    # manuscript text - called that "well-separated". mean() also
                    # grows with the number of groups, so the same adjacent gap
                    # scored 4.5 with two groups and 9.7 with six.
                    discrim_index <- min(stats::dist(centres))
                }

                # Determine separation based on discriminability index
                # IMPORTANT: These thresholds are DESCRIPTIVE RULES OF THUMB, not validated cutoffs
                separation_quality <- if (is.na(discrim_index)) {
                    "unable to calculate"
                } else if (discrim_index > 3) {
                    "well-separated"
                } else if (discrim_index > 1.5) {
                    "moderately separated"
                } else {
                    "overlapping"
                }
            }

            # Generate copy-ready summary
            summary_html <- paste0(
                "<div style='background-color: rgba(33, 159, 33, 0.1); border-left: 4px solid #28a745; padding: 20px; margin-bottom: 20px; border-radius: 4px; color: inherit;'>",
                "<h3 style='color: inherit; margin-top: 0;'> Natural Language Summary</h3>",

                # Add disclaimer about descriptive nature
                if (n_groups >= 2) paste0(
                    "<div style='background-color: rgba(255, 202, 33, 0.23); border-left: 3px solid #ffc107; padding: 10px; margin: 10px 0; color: inherit;'>",
                    "<p style='margin: 0; font-size: 13px;'><strong> Note on 'Separation' Assessment:</strong> ",
                    "The descriptors '", separation_quality, "' are based on a descriptive heuristic (discriminability index = ",
                    "the smallest centroid distance between any two groups, after dividing each axis by its mean within-group SD). ",
                    "It therefore describes the worst-separated pair, not the average pair. ",
                    "<strong>This is NOT a formal statistical test.</strong> ",
                    "Thresholds are arbitrary descriptive categories, not validated clinical cutoffs. ",
                    "For formal inference about group differences, use appropriate statistical tests (MANOVA, discriminant analysis, etc.).</p>",
                    "</div>"
                ) else "",

                "<div style='background-color: rgba(255, 255, 255, 0.06); padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #c3e6cb; color: inherit;'>",
                "<h4 style='color: inherit; margin-top: 0;'>Copy-Ready Text:</h4>",
                "<p style='font-family: \"Times New Roman\", serif; line-height: 1.6; margin: 0;'>",
                # With a single group the comparative wording is not merely
                # ungrammatical ("revealed 1 distinct groups"), it asserts a
                # comparison that does not exist - and this text is offered for
                # use in manuscripts. Give that case its own sentence.
                if (n_groups < 2) paste0(
                    "<strong>Hull plot analysis described a single group defined by ",
                    htmltools::htmlEscape(group_var), ". ",
                    "The visualization shows the relationship between ", htmltools::htmlEscape(x_var),
                    " and ", htmltools::htmlEscape(y_var), " across ", n_total, " observations, ",
                    "with a hull boundary delineating the extent of that group's distribution. ",
                    "No between-group comparison is possible from a single group.</strong>"
                ) else paste0(
                    "<strong>Hull plot analysis revealed ", n_groups, " distinct groups based on ",
                    htmltools::htmlEscape(group_var), " classifications. ",
                    "The visualization shows the relationship between ", htmltools::htmlEscape(x_var),
                    " and ", htmltools::htmlEscape(y_var), " across ", n_total, " observations. ",
                    "The closest pair of group centroids, measured in within-group SD units, is described as ",
                    separation_quality, "; this describes centroid distance, not hull overlap, ",
                    "with hull boundaries clearly delineating the extent of each group's distribution.</strong>"
                ),
                "</p>",
                "</div>",

                "<h4 style='color: inherit;'>Key Findings:</h4>",
                "<ul style='color: inherit;'>"
            )

            # Add specific findings for each group
            for (i in seq_len(nrow(group_stats))) {
                group_name <- group_stats[[group_var]][i]
                n_points <- group_stats$n[i]
                summary_html <- paste0(summary_html,
                    "<li><strong>", htmltools::htmlEscape(group_name), ":</strong> ", n_points, " observations (",
                    round(100 * n_points / n_total, 1), "% of total)</li>"
                )
            }

            summary_html <- paste0(summary_html,
                "</ul>",

                "<h4 style='color: inherit;'>Clinical Interpretation:</h4>",
                "<p style='color: inherit;'>",
                "Hull plots are particularly valuable for identifying patient subgroups, treatment response patterns, ",
                "and biomarker relationships. ",
                # The trailing `else` used to catch the single-group case and
                # claim "substantial overlap between categories" when there was
                # only one category.
                if (n_groups < 2)
                    "With only one group present there are no categories to compare; add a grouping variable with at least two levels, or check whether missing data removed every observation from the other groups."
                else
                    "Separation of sample centroids does not establish biological differences, treatment response, or diagnostic discrimination. Interpret the axes and group definitions in context and validate any clinical claim independently.",
                "</p>",

                "<p style='font-size: 11px; color: inherit; opacity: 0.75; margin-top: 20px; font-style: italic;'>",
                "This summary is generated automatically based on the hull plot visualization. ",
                "Copy the text above for use in reports, presentations, or publications.",
                "</p>",
                "</div>"
            )

            return(summary_html)
        },

        .generate_assumptions_guide = function() {
            assumptions_html <- paste0(
                "<div style='background-color: rgba(255, 203, 33, 0.14); border-left: 4px solid #ff9800; padding: 20px; margin-bottom: 20px; border-radius: 4px; color: inherit;'>",
                "<h3 style='color: inherit; margin-top: 0;'> Data Requirements &amp; Assumptions</h3>",

                "<h4 style='color: inherit;'>Data Requirements:</h4>",
                "<ul style='color: inherit;'>",
                "<li><strong>X &amp; Y Variables:</strong> Continuous numeric variables (measurements, scores, biomarker levels)</li>",
                "<li><strong>Grouping Variable:</strong> Categorical variable (treatment groups, patient types, disease stages)</li>",
                "<li><strong>Minimum Sample Size:</strong> At least 3 observations per group for meaningful hull boundaries</li>",
                "<li><strong>Complete Cases:</strong> Missing values in key variables will be excluded from analysis</li>",
                "</ul>",

                "<h4 style='color: inherit;'>Key Assumptions:</h4>",
                "<ul style='color: inherit;'>",
                "<li><strong>Meaningful Grouping:</strong> The grouping variable represents biologically or clinically relevant categories</li>",
                "<li><strong>Scale Appropriateness:</strong> X and Y variables are on appropriate scales for comparison</li>",
                "<li><strong>Data Independence:</strong> Observations should be independent (not repeated measures without appropriate handling)</li>",
                "<li><strong>Outlier Consideration:</strong> Extreme outliers may distort hull boundaries and interpretation</li>",
                "</ul>",

                "<h4 style='color: inherit;'>Best Practices:</h4>",
                "<ul style='color: inherit;'>",
                "<li><strong>Sample Size:</strong> Larger groups (n > 10) provide more stable hull boundaries</li>",
                "<li><strong>Variable Selection:</strong> Choose variables that are expected to differentiate between groups</li>",
                "<li><strong>Outlier Management:</strong> Review and investigate outliers before final interpretation</li>",
                "<li><strong>Clinical Context:</strong> Always interpret results within the specific clinical or research context</li>",
                "<li><strong>Validation:</strong> Consider complementing with statistical tests for group differences</li>",
                "</ul>",

                "<h4 style='color: inherit;'>When Hull Plots Are Most Useful:</h4>",
                "<ul style='color: inherit;'>",
                "<li><strong>Exploratory Analysis:</strong> Initial investigation of group patterns and relationships</li>",
                "<li><strong>Presentation:</strong> Clear visual communication of group boundaries to clinical audiences</li>",
                "<li><strong>Hypothesis Generation:</strong> Identifying potential subgroups or response patterns</li>",
                "<li><strong>Quality Control:</strong> Detecting unusual patterns or data quality issues</li>",
                "</ul>",

                "<div style='background-color: rgba(255, 166, 33, 0.35); padding: 15px; border-radius: 6px; margin-top: 15px; border: 2px solid #ff9800; color: inherit;'>",
                "<h4 style='color: inherit; margin-top: 0;'> Critical Reminder: Exploratory vs. Inferential Analysis</h4>",
                "<p style='color: inherit; margin: 5px 0;'><strong>Hull plots are DESCRIPTIVE VISUALIZATIONS, not statistical tests.</strong></p>",
                "<ul style='color: inherit; margin: 10px 0;'>",
                "<li><strong>Visual separation \u2260 statistical significance:</strong> Groups may appear separated in a hull plot but not differ significantly when tested formally</li>",
                "<li><strong>Descriptive indices (e.g., 'well-separated') are heuristics:</strong> These use arbitrary thresholds, not validated statistical cutoffs</li>",
                "<li><strong>Required for inference:</strong> Complement hull plots with appropriate statistical tests:",
                "<ul>",
                "<li>MANOVA for multivariate group differences</li>",
                "<li>Discriminant analysis for classification</li>",
                "<li>Hotelling's T\u00b2 for two-group comparisons</li>",
                "<li>Permutation tests for complex designs</li>",
                "</ul></li>",
                "<li><strong>Best use case:</strong> Exploratory analysis, hypothesis generation, presentation of patterns</li>",
                "</ul>",
                "<p style='color: inherit; margin: 5px 0; font-style: italic;'>",
                "Never claim statistical significance based solely on hull plot appearance or separation descriptors.",
                "</p>",
                "</div>",
                "</div>"
            )

            return(assumptions_html)
        }

    )
)
