#' @title Segmented Total Bar Charts
#' @description
#' Create segmented total bar charts (100% stacked bars) that show proportional
#' breakdowns within categories. Perfect for displaying composition data where
#' each bar represents 100% and segments show relative proportions.
#'
#' @details
#' This module creates segmented total bar charts using ggplot2, where:
#' - Each bar represents 100% of the total for that category
#' - Segments within bars show relative proportions
#' - Colors distinguish different segments
#' - Labels can show percentages and/or raw counts
#'
#' The visualization approach is inspired by the ggsegmentedtotalbar package
#' (https://github.com/ozancanozdemir/ggsegmentedtotalbar) by Ozancan Ozdemir.
#' This implementation uses a clean ggplot2-based approach optimized for
#' clinical research workflows within the jamovi environment
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_col position_fill coord_flip geom_bar
#' @importFrom ggplot2 theme_minimal theme_classic theme_bw labs scale_fill_brewer
#' @importFrom ggplot2 scale_fill_viridis_d geom_text element_text theme element_rect
#' @importFrom ggplot2 element_line margin scale_y_continuous scale_fill_manual
#' @importFrom ggplot2 position_fill facet_wrap
#' @importFrom dplyr group_by summarise mutate arrange count ungroup filter slice pull select
#' @importFrom stats chisq.test xtabs
#' @importFrom tidyr pivot_wider
#' @importFrom scales percent
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stringr str_to_title
#' @importFrom rlang sym
#' @export

jjsegmentedtotalbarClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jjsegmentedtotalbarClass",
    inherit = jjsegmentedtotalbarBase,
    private = list(

        # Internal data storage (computed on-demand for better memory management)
        .processed_data = NULL,
        .composition_data = NULL,
        .preset_config = NULL,
        .preset_style_override = NULL,
        .preset_palette_override = NULL,

        # Variable name escaping utility for special characters
        .escapeVar = function(x) {
            # Convert variable names with special characters to safe R names
            # This mirrors the modelbuilder behavior for handling spaces and punctuation
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        # Initialize notice collection list
        .noticeList = list(),

        # Add a notice to the collection
        .addNotice = function(type, title, content) {
          private$.noticeList[[length(private$.noticeList) + 1]] <- list(
            type = type,
            title = title,
            content = content
          )
        },

        # Render collected notices as HTML
        .renderNotices = function() {
          if (length(private$.noticeList) == 0) {
            self$results$warnings$setVisible(FALSE)
            return()
          }

          # Map notice types to colors and icons
          typeStyles <- list(
            ERROR = list(color = "#dc2626", bgcolor = "#fef2f2", border = "#fca5a5", icon = "⛔"),
            STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed", border = "#fdba74", icon = "⚠️"),
            WARNING = list(color = "#ca8a04", bgcolor = "#fefce8", border = "#fde047", icon = "⚡"),
            INFO = list(color = "#2563eb", bgcolor = "#eff6ff", border = "#93c5fd", icon = "ℹ️")
          )

          html <- "<div style='margin: 10px 0;'>"

          for (notice in private$.noticeList) {
            style <- typeStyles[[notice$type]] %||% typeStyles$INFO

            html <- paste0(html,
              "<div style='background-color: ", style$bgcolor, "; ",
              "border-left: 4px solid ", style$border, "; ",
              "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
              "<strong style='color: ", style$color, ";'>",
              style$icon, " ", notice$title, "</strong><br>",
              "<span style='color: #374151;'>", notice$content, "</span>",
              "</div>"
            )
          }

          html <- paste0(html, "</div>")

          self$results$warnings$setContent(html)
          self$results$warnings$setVisible(TRUE)
        },

        .init = function() {
            # Initialize comprehensive instructions with clinical context

            html_content <- paste(
                "<div style='font-family: sans-serif; padding: 15px; background-color: #f5f5f5; border-radius: 5px;'>",
                "<h3>Segmented Total Bar Chart Instructions</h3>",
                "<p>This tool creates publication-quality segmented bar charts (also known as stacked bar charts), ideal for visualizing the relationship between two categorical variables.</p>",
                "<ul>",
                "<li><b>X-Axis Variable:</b> The main category (e.g., Treatment Group, Time Point)</li>",
                "<li><b>Fill Variable:</b> The segments within each bar (e.g., Response Type, Outcome)</li>",
                "<li><b>Value Variable:</b> The counts or values determining segment sizes (must be numeric)</li>",
                "</ul>",
                "<p><b>Clinical Presets:</b> Use the 'Analysis Template' dropdown to quickly apply settings for common clinical research scenarios.</p>",
                "</div>"
            )
            self$results$instructions$setContent(html_content)

            # Set plot size based on user options
            width_px <- self$options$plot_width * 72
            height_px <- self$options$plot_height * 72
            
            # Helper to safely set size/visibility
            image_set <- function(img, w, h, v) {
                if(!is.null(img)) {
                    if(is.function(img$setSize)) img$setSize(width=w, height=h)
                    if(is.function(img$setVisible)) img$setVisible(v)
                }
            }
            
            image_set(self$results$plot, width_px, height_px, self$options$show_plot)

            if (!is.null(self$results$statistical_tests) && is.function(self$results$statistical_tests$setVisible)) {
                self$results$statistical_tests$setVisible(self$options$show_statistical_tests)
            }

            # Apply clinical presets if selected
            private$.applyPresetConfiguration()
            
            # Show preset guidance if not custom
            if (self$options$analysis_preset != "custom") {
                if (!is.null(self$results$preset_guidance)) self$results$preset_guidance$setVisible(TRUE)
                if (!is.null(self$results$presetInfo)) self$results$presetInfo$setVisible(TRUE)
                private$.updatePresetGuidance()
            } else {
                if (!is.null(self$results$preset_guidance)) self$results$preset_guidance$setVisible(FALSE)
                if (!is.null(self$results$presetInfo)) self$results$presetInfo$setVisible(FALSE)
            }

            # Note: Notices are collected in private$.noticeList and rendered as HTML
            # at the end of .run() via private$.renderNotices()
        },

        .run = function() {
            # Check if required variables are specified
            if (is.null(self$options$x_var) ||
                is.null(self$options$y_var) ||
                is.null(self$options$fill_var)) {
                return()
            }

            # Clear existing tables to prevent accumulation
            self$results$composition_table$deleteRows()
            self$results$detailed_stats$deleteRows()

            if (self$options$show_statistical_tests) {
                 self$results$statistical_tests$deleteRows()
            }

            # Get data
            data <- self$data

            # Extract variable names
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            fill_var <- self$options$fill_var
            facet_var <- self$options$facet_var

            # Validate data
            if (nrow(data) == 0) {
                self$results$instructions$setContent(.("No data available for analysis."))
                return()
            }



            # Prepare data
            private$.processData(data, x_var, y_var, fill_var, facet_var)

            # Create the plot (will be handled by .plot method called by jamovi)
            # The actual plot creation is handled by the .plot method

            # Update summary table
            private$.updateSummary()

            # Checkpoint before composition table processing


            # Update composition table
            private$.updateComposition()

            # Checkpoint before detailed statistics calculation


            # Update detailed statistics
            private$.updateDetailedStats()

            # Create interpretation
            private$.createInterpretation()

            # Create clinical summary
            private$.createClinicalSummary()

            # Perform statistical tests if requested
            if (self$options$show_statistical_tests) {
                # Validate that data implies counts (integers)
                is_count_data <- all(private$.processed_data$count %% 1 == 0) && all(private$.processed_data$count >= 0)

                if (!is_count_data) {
                    private$.addNotice("STRONG_WARNING", "Non-Count Data Warning",
                        .("Statistical tests skipped. Value Variable contains non-integer or negative values suggesting continuous data rather than counts. Chi-square tests are only valid for count/frequency data."))
                } else {
                    private$.performStatisticalTests()
                }
            }

            # Generate explanations if requested
            if (self$options$showExplanations) {
                private$.generateExplanations()
            }

            # Add completion info notice
            if (!is.null(private$.processed_data) && nrow(private$.processed_data) > 0) {
                n_total <- sum(private$.processed_data$count, na.rm = TRUE)
                n_categories <- length(unique(private$.processed_data[[self$options$x_var]]))
                n_segments <- length(unique(private$.processed_data[[self$options$fill_var]]))

                private$.addNotice("INFO", "Analysis Info",
                    sprintf(.("Analysis completed successfully using %d observations across %d categories and %d segments."),
                        n_total, n_categories, n_segments))
            }
        },

        .generateExplanations = function() {
            self$results$explanations$setVisible(TRUE)
            self$results$explanations$setContent(
                "<h3>Explanations</h3>
                <p>
                    This segmented total bar chart shows the proportional breakdown of a continuous variable across different categories.
                    Each bar represents 100% of the total for that category, and the segments within each bar show the relative proportions of the different fill variable levels.
                </p>
                <p>
                    The chi-square test is used to determine if there is a significant association between the categorical variables.
                    A significant p-value suggests that the proportions of the fill variable levels are not the same across all categories of the x-variable.
                </p>
                <p>
                    Standardized residuals are used to identify which cells contribute most to a significant chi-square result.
                    A standardized residual greater than 2 or less than -2 is considered to be a major contributor to the chi-square value.
                </p>"
            )
        },

        .processData = function(data, x_var, y_var, fill_var, facet_var) {

            # Validate variables exist in data
            required_vars <- c(x_var, y_var, fill_var)
            missing_vars <- setdiff(required_vars, names(data))
            if (length(missing_vars) > 0) {
                private$.addNotice("ERROR", "Missing Variables",
                    paste(.("Missing variables in data:"), paste(missing_vars, collapse = ", ")))
                return()
            }

            # Check for NA values in key variables
            if (sum(is.na(data[[x_var]])) > 0) {
                na_count <- sum(is.na(data[[x_var]]))
                if (self$options$exclude_missing) {
                    private$.addNotice("WARNING", "Missing Data",
                        paste("Found", na_count, "NA values in", x_var, "- these will be removed"))
                    data <- data[!is.na(data[[x_var]]), ]
                } else {
                    private$.addNotice("INFO", "Missing Data Present",
                        paste("Found", na_count, "NA values in", x_var, "- these will be included in the analysis"))
                }
            }

            if (sum(is.na(data[[fill_var]])) > 0) {
                na_count <- sum(is.na(data[[fill_var]]))
                if (self$options$exclude_missing) {
                    private$.addNotice("WARNING", "Missing Data",
                        paste("Found", na_count, "NA values in", fill_var, "- these will be removed"))
                    data <- data[!is.na(data[[fill_var]]), ]
                } else {
                    private$.addNotice("INFO", "Missing Data Present",
                        paste("Found", na_count, "NA values in", fill_var, "- these will be included in the analysis"))
                }
            }

            # Check if y_var is numeric
            if (!is.numeric(data[[y_var]])) {
                private$.addNotice("ERROR", "Invalid Variable Type",
                    paste(y_var, .("must be a numeric variable")))
                return()
            }

            # Convert variables to factors if needed
            data[[x_var]] <- as.factor(data[[x_var]])
            data[[fill_var]] <- as.factor(data[[fill_var]])

            # Check for sufficient levels
            if (length(levels(data[[x_var]])) < 1) {
                private$.addNotice("ERROR", "Empty Variable",
                    paste(x_var, "must have at least one category"))
                return()
            }

            if (length(levels(data[[fill_var]])) < 1) {
                private$.addNotice("ERROR", "Empty Variable",
                    paste(fill_var, "must have at least one segment"))
                return()
            }

            if (!is.null(facet_var)) {
                if (!facet_var %in% names(data)) {
                    private$.addNotice("ERROR", "Missing Facet Variable",
                        paste("Facet variable", facet_var, "not found in data"))
                    return()
                }
                data[[facet_var]] <- as.factor(data[[facet_var]])
            }

            # CRITICAL FIX: Group and summarise data
            # For aggregated data (one row per category), n() returns 1, which is wrong
            # The y_var already contains the counts/values we need to sum
            if (!is.null(facet_var)) {
                processed_data <- data %>%
                    dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(fill_var), !!rlang::sym(facet_var)) %>%
                    dplyr::summarise(
                        value = sum(!!rlang::sym(y_var), na.rm = TRUE),
                        # CRITICAL FIX: count should equal value for count data
                        # This ensures correct N reporting for both raw and aggregated data
                        count = sum(!!rlang::sym(y_var), na.rm = TRUE),
                        .groups = 'drop'
                    )
            } else {
                processed_data <- data %>%
                    dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(fill_var)) %>%
                    dplyr::summarise(
                        value = sum(!!rlang::sym(y_var), na.rm = TRUE),
                        # CRITICAL FIX: count should equal value for count data
                        # This ensures correct N reporting for both raw and aggregated data
                        count = sum(!!rlang::sym(y_var), na.rm = TRUE),
                        .groups = 'drop'
                    )
            }

            # Calculate percentages within each category
            if (!is.null(facet_var)) {
                processed_data <- processed_data %>%
                    dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(facet_var)) %>%
                    dplyr::mutate(
                        percentage = value / sum(value) * 100,
                        total_in_category = sum(value)
                    ) %>%
                    dplyr::ungroup() %>%
                    dplyr::mutate(
                        overall_percentage = value / sum(value) * 100
                    )
            } else {
                processed_data <- processed_data %>%
                    dplyr::group_by(!!rlang::sym(x_var)) %>%
                    dplyr::mutate(
                        percentage = value / sum(value) * 100,
                        total_in_category = sum(value)
                    ) %>%
                    dplyr::ungroup() %>%
                    dplyr::mutate(
                        overall_percentage = value / sum(value) * 100
                    )
            }

            # Check for empty groups
            if (nrow(processed_data) == 0) {
                private$.addNotice("ERROR", "No Valid Data",
                    .("No valid data after processing. Please check your input variables."))
                return()
            }

            # Check for zero totals
            zero_totals <- processed_data %>%
                dplyr::group_by(!!rlang::sym(x_var)) %>%
                dplyr::summarise(total = sum(value), .groups = 'drop') %>%
                dplyr::filter(total == 0)

            if (nrow(zero_totals) > 0) {
                private$.addNotice("WARNING", "Zero Total Categories",
                    paste("Categories with zero totals found:",
                        paste(zero_totals[[x_var]], collapse = ", "),
                        "- these may appear empty in the plot"))
            }

            # Apply sorting if requested
            if (self$options$sort_categories != "none") {
                processed_data <- private$.applySorting(processed_data, x_var, fill_var, facet_var)
            }

            # Perform clinical data validation
            private$.validateClinicalData(processed_data, x_var, fill_var)

            private$.processed_data <- processed_data

            # Create composition data for table
            composition_data <- processed_data %>%
                dplyr::select(!!rlang::sym(x_var), !!rlang::sym(fill_var), count, percentage, overall_percentage, total_in_category)

            names(composition_data) <- c("category", "segment", "count", "percentage", "overall_percentage", "total_in_category")

            private$.composition_data <- composition_data
        },

        .applySorting = function(data, x_var, fill_var, facet_var) {

            sort_type <- self$options$sort_categories

            if (sort_type == "total") {
                # Sort by total value in each category
                if (!is.null(facet_var)) {
                    # When faceting, aggregate across all facets for overall category ordering
                    category_totals <- data %>%
                        dplyr::group_by(!!rlang::sym(x_var)) %>%
                        dplyr::summarise(total = sum(value), .groups = 'drop') %>%
                        dplyr::arrange(desc(total))
                } else {
                    category_totals <- data %>%
                        dplyr::group_by(!!rlang::sym(x_var)) %>%
                        dplyr::summarise(total = sum(value), .groups = 'drop') %>%
                        dplyr::arrange(desc(total))
                }

                # Use unique levels to avoid duplicates
                unique_levels <- unique(category_totals[[x_var]])
                data[[x_var]] <- factor(data[[x_var]], levels = unique_levels)

            } else if (sort_type == "largest_segment") {
                # Sort by the size of the largest segment in each category
                if (!is.null(facet_var)) {
                    # When faceting, find the maximum segment across all facets for each category
                    largest_segments <- data %>%
                        dplyr::group_by(!!rlang::sym(x_var)) %>%
                        dplyr::summarise(max_segment = max(value), .groups = 'drop') %>%
                        dplyr::arrange(desc(max_segment))
                } else {
                    largest_segments <- data %>%
                        dplyr::group_by(!!rlang::sym(x_var)) %>%
                        dplyr::summarise(max_segment = max(value), .groups = 'drop') %>%
                        dplyr::arrange(desc(max_segment))
                }

                # Use unique levels to avoid duplicates
                unique_levels <- unique(largest_segments[[x_var]])
                data[[x_var]] <- factor(data[[x_var]], levels = unique_levels)

            } else if (sort_type == "alpha") {
                # Alphabetical sorting - get unique levels first
                current_levels <- unique(as.character(data[[x_var]]))
                sorted_levels <- sort(current_levels)
                data[[x_var]] <- factor(data[[x_var]], levels = sorted_levels)
            }

            return(data)
        },

        .validateClinicalData = function(data, x_var, fill_var) {

            # Check minimum sample sizes for statistical reliability
            category_counts <- data %>%
                dplyr::group_by(!!rlang::sym(x_var)) %>%
                dplyr::summarise(total_count = sum(count), .groups = 'drop')

            small_categories <- category_counts %>%
                dplyr::filter(total_count < 5)

            if (nrow(small_categories) > 0) {
                private$.addNotice("STRONG_WARNING", "Small Sample Size",
                    paste(.("Some categories have fewer than 5 observations. Results may be unreliable for small groups:"),
                        paste(small_categories[[x_var]], collapse = ", ")))
            }

            # Check for extreme proportions that may indicate data quality issues
            extreme_segments <- data %>%
                dplyr::filter(percentage > 95)

            if (nrow(extreme_segments) > 0) {
                private$.addNotice("STRONG_WARNING", "Extreme Proportions",
                    .("Some segments represent >95% of their category. Consider combining rare categories or checking data quality."))
            }

            # Check for very small segments that may not be clinically meaningful
            tiny_segments <- data %>%
                dplyr::filter(percentage < 1 & count < 3)

            if (nrow(tiny_segments) > 0) {
                private$.addNotice("WARNING", "Tiny Segments",
                    .("Some segments represent <1% with very small counts. These may not be clinically meaningful."))
            }

            # Check for reasonable number of categories for visualization
            n_categories <- length(unique(data[[x_var]]))
            if (n_categories > 10) {
                private$.addNotice("WARNING", "Too Many Categories",
                    paste(.("Large number of categories ("), n_categories,
                        .(") may make the chart difficult to interpret. Consider grouping similar categories.")))
            }

            # Check for reasonable number of segments
            n_segments <- length(unique(data[[fill_var]]))
            if (n_segments > 8) {
                private$.addNotice("WARNING", "Too Many Segments",
                    paste(.("Large number of segments ("), n_segments,
                        .(") may make colors difficult to distinguish. Consider grouping similar segments.")))
            }

            # Check for missing combinations (empty cells)
            expected_combinations <- n_categories * n_segments
            actual_combinations <- nrow(data)
            if (actual_combinations < expected_combinations) {
                missing_combinations <- expected_combinations - actual_combinations
                private$.addNotice("WARNING", "Missing Data Combinations",
                    paste(.("Missing data combinations detected:"), missing_combinations,
                        .("empty category-segment pairs. This may indicate sparse data.")))
            }
        },

        # Centralized color palette definitions
        .getColorPalette = function(palette_name, n_colors) {
            
            palettes <- list(
                clinical = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#6A994E", "#7209B7", "#F72585", "#4361EE"),
                colorblind = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000"),
                bbc_multi = c("#1380A1", "#FAAB18", "#990000", "#588300", "#990099", "#FF6600", "#809900", "#bcc4c1"),
                prism_colorblind_safe = c("#0000FF", "#FF0000", "#00BF00", "#FF00FF", "#FFFF00", "#00FFFF", "#800080", "#FFA500"),
                nature = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4", "#91D1C2", "#DC0000"),
                science = c("#0C5BB0", "#FFA042", "#15983D", "#EC0000", "#8b7355", "#149EF3", "#FA6B09", "#A149FA")
            )
            
            if (palette_name %in% names(palettes)) {
                colors <- palettes[[palette_name]]
                if (n_colors <= length(colors)) {
                    return(colors[1:n_colors])
                }
            }
            
            return(NULL)  # Use default/viridis fallback
        },

        .applyColorPalette = function(p, data, fill_var) {

            palette <- self$options$color_palette
            n_colors <- length(unique(data[[fill_var]]))

            if (palette == "viridis") {
                p <- p + ggplot2::scale_fill_viridis_d()
            } else if (palette == "set1") {
                p <- p + ggplot2::scale_fill_brewer(type = "qual", palette = "Set1")
            } else if (palette == "dark2") {
                p <- p + ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2")
            } else if (palette == "paired") {
                p <- p + ggplot2::scale_fill_brewer(type = "qual", palette = "Paired")
            } else {
                # Try custom palettes
                custom_colors <- private$.getColorPalette(palette, n_colors)
                if (!is.null(custom_colors)) {
                    p <- p + ggplot2::scale_fill_manual(values = custom_colors)
                } else {
                    # Fallback to viridis for unknown palettes
                    p <- p + ggplot2::scale_fill_viridis_d()
                }
            }

            return(p)
        },

        .applyTheme = function(p) {

            style <- self$options$chart_style

            if (style == "clean") {
                p <- p + ggplot2::theme_minimal() +
                    ggplot2::theme(
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        text = ggplot2::element_text(size = 11),
                        axis.title = ggplot2::element_text(size = 12, face = "bold"),
                        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)
                    )
            } else if (style == "publication") {
                p <- p + ggplot2::theme_classic() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 10),
                        axis.title = ggplot2::element_text(size = 11, face = "bold"),
                        plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                        legend.text = ggplot2::element_text(size = 9),
                        axis.line = ggplot2::element_line(color = "black", size = 0.5)
                    )
            } else if (style == "presentation") {
                p <- p + ggplot2::theme_minimal() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 14),
                        axis.title = ggplot2::element_text(size = 16, face = "bold"),
                        plot.title = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
                        legend.text = ggplot2::element_text(size = 12),
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank()
                    )
            } else if (style == "clinical") {
                p <- p + ggplot2::theme_bw() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 11),
                        axis.title = ggplot2::element_text(size = 12, face = "bold"),
                        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        legend.position = "right",
                        legend.background = ggplot2::element_rect(fill = "white", color = "gray80"),
                        panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1)
                    )
            } else if (style == "bbc_style") {
                p <- p + ggplot2::theme_minimal() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 11),
                        plot.title = ggplot2::element_text(size = 18, face = "bold", hjust = 0),
                        plot.subtitle = ggplot2::element_text(size = 14, hjust = 0),
                        axis.title = ggplot2::element_blank(),
                        axis.text = ggplot2::element_text(size = 10),
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.background = ggplot2::element_blank(),
                        legend.position = "top",
                        legend.justification = "left",
                        legend.text = ggplot2::element_text(size = 10),
                        legend.title = ggplot2::element_blank()
                    )
            } else if (style == "prism_style") {
                p <- p + ggplot2::theme_classic() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 10),
                        axis.title = ggplot2::element_text(size = 11),
                        plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                        axis.line = ggplot2::element_line(color = "black", linewidth = 0.5),
                        axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.5),
                        legend.text = ggplot2::element_text(size = 9),
                        legend.key.size = ggplot2::unit(0.8, "cm"),
                        panel.background = ggplot2::element_rect(fill = "white")
                    )
            }

            return(p)
        },

        .addTitles = function(p) {

            # Set titles
            plot_title <- if (self$options$plot_title != "") self$options$plot_title else .("Segmented Total Bar Chart")
            x_title <- if (self$options$x_title != "") self$options$x_title else self$options$x_var
            y_title <- if (self$options$y_title != "") self$options$y_title else .("Percentage")
            legend_title <- if (self$options$legend_title != "") self$options$legend_title else self$options$fill_var

            p <- p + ggplot2::labs(
                title = plot_title,
                x = x_title,
                y = y_title,
                fill = legend_title
            )

            return(p)
        },

        .applyLegend = function(p) {

            position <- self$options$legend_position

            if (position == "none") {
                p <- p + ggplot2::theme(legend.position = "none")
            } else {
                p <- p + ggplot2::theme(legend.position = position)
            }

            return(p)
        },

        .updateSummary = function() {

            data <- private$.processed_data

            if (is.null(data)) {
                return()
            }

            n_categories <- length(unique(data[[self$options$x_var]]))
            n_segments <- length(unique(data[[self$options$fill_var]]))
            total_obs <- sum(data$count, na.rm = TRUE)

            summary_row <- list(
                categories = n_categories,
                segments = n_segments,
                total_observations = total_obs,
                chart_type = .("Segmented Total Bar (100% Stacked)")
            )

            self$results$summary$setRow(rowNo = 1, values = summary_row)
        },

        .updateComposition = function() {

            data <- private$.composition_data

            if (is.null(data)) {
                return()
            }

            for (i in 1:nrow(data)) {
                row <- list(
                    category = as.character(data$category[i]),
                    segment = as.character(data$segment[i]),
                    count = data$count[i],
                    percentage = data$percentage[i] / 100,  # Convert to proportion for percentage format
                    overall_percentage = data$overall_percentage[i] / 100,
                    total_in_category = data$total_in_category[i]
                )

                self$results$composition_table$addRow(rowKey = i, values = row)
            }
        },

        .createInterpretation = function() {

            data <- private$.processed_data
            composition_data <- private$.composition_data

            if (is.null(data) || is.null(composition_data)) {
                return()
            }

            n_categories <- length(unique(data[[self$options$x_var]]))
            n_segments <- length(unique(data[[self$options$fill_var]]))

            # Find largest segment overall
            largest_segment <- composition_data %>%
                dplyr::arrange(desc(percentage)) %>%
                dplyr::slice(1)

            # Find most balanced category
            balanced_category <- composition_data %>%
                dplyr::group_by(category) %>%
                dplyr::summarise(
                    cv = sd(percentage) / mean(percentage),
                    .groups = 'drop'
                ) %>%
                dplyr::arrange(cv) %>%
                dplyr::slice(1)

            interpretation_html <- paste(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 6px; margin: 10px 0;'>",
                "<h4 style='color: #495057; margin-top: 0;'>Chart Interpretation</h4>",
                "<p><strong>Data Overview:</strong></p>",
                "<ul>",
                paste0("<li>", n_categories, " categories with ", n_segments, " segments each</li>"),
                paste0("<li>Total observations: ", sum(composition_data$count), "</li>"),
                "</ul>",
                "<p><strong>Key Findings:</strong></p>",
                "<ul>",
                paste0("<li><strong>Largest segment:</strong> ", largest_segment$segment[1],
                       " in ", largest_segment$category[1],
                       " (", round(largest_segment$percentage[1], 1), "%)</li>"),
                paste0("<li><strong>Most balanced category:</strong> ", balanced_category$category[1],
                       " shows the most even distribution across segments</li>"),
                "</ul>",
                "</div>"
            )

            self$results$interpretation$setContent(interpretation_html)
        },

        .updateDetailedStats = function() {

            data <- private$.processed_data
            composition_data <- private$.composition_data

            if (is.null(data) || is.null(composition_data)) {
                return()
            }

            # Calculate detailed statistics
            n_categories <- length(unique(data[[self$options$x_var]]))
            n_segments <- length(unique(data[[self$options$fill_var]]))
            total_obs <- sum(data$count, na.rm = TRUE)

            # Calculate min and max percentages
            min_pct <- round(min(composition_data$percentage, na.rm = TRUE), 1)
            max_pct <- round(max(composition_data$percentage, na.rm = TRUE), 1)
            mean_pct <- round(mean(composition_data$percentage, na.rm = TRUE), 1)

            # Calculate segment with highest variation
            segment_variation <- composition_data %>%
                dplyr::group_by(segment) %>%
                dplyr::summarise(
                    cv = sd(percentage, na.rm = TRUE) / mean(percentage, na.rm = TRUE),
                    .groups = 'drop'
                ) %>%
                dplyr::arrange(desc(cv)) %>%
                dplyr::slice(1)

            # Create statistics rows
            stats_rows <- list(
                list(measure = "Total Categories", value = as.character(n_categories)),
                list(measure = "Total Segments", value = as.character(n_segments)),
                list(measure = "Total Observations", value = as.character(total_obs)),
                list(measure = "Min Percentage", value = paste0(min_pct, "%")),
                list(measure = "Max Percentage", value = paste0(max_pct, "%")),
                list(measure = "Mean Percentage", value = paste0(mean_pct, "%")),
                list(measure = "Most Variable Segment", value = as.character(segment_variation$segment[1]))
            )

            # Add rows to table
            for (i in seq_along(stats_rows)) {
                self$results$detailed_stats$addRow(rowKey = i, values = stats_rows[[i]])
            }

            # Make the table visible if we have data
            self$results$detailed_stats$setVisible(TRUE)
        },

        .plot = function(image, ggtheme, theme, ...) {

            # Plot builder supporting both traditional stacked bars and Flerlage-style plots

            # Check if this plot should be shown
            if (!self$options$show_plot) {
                return()
            }

            # Validate required options
            if (is.null(self$options$x_var) ||
                is.null(self$options$y_var) ||
                is.null(self$options$fill_var)) {
                return()
            }

            df <- private$.processed_data
            if (is.null(df) || nrow(df) == 0) return()

            # Route to appropriate plot type
            if (self$options$plot_type == "flerlage") {
                p <- private$.plotFlerlage(df, image, ggtheme, theme)
            } else {
                p <- private$.plotTraditional(df, image, ggtheme, theme)
            }

            # Print plot
            print(p)
            TRUE
        },

        .plotTraditional = function(df, image, ggtheme, theme, ...) {
            # Traditional 100% stacked bar chart implementation

            x_var <- self$options$x_var
            fill_var <- self$options$fill_var

            # Apply sorting if requested (inline)
            if (self$options$sort_categories != "none") {
                if (self$options$sort_categories == "total") {
                    category_order <- df %>%
                        dplyr::group_by(!!rlang::sym(x_var)) %>%
                        dplyr::summarise(total = sum(.data$value), .groups = 'drop') %>%
                        dplyr::arrange(desc(.data$total)) %>%
                        dplyr::pull(!!rlang::sym(x_var))
                    df[[x_var]] <- factor(df[[x_var]], levels = unique(category_order))
                } else if (self$options$sort_categories == "largest_segment") {
                    category_order <- df %>%
                        dplyr::group_by(!!rlang::sym(x_var)) %>%
                        dplyr::summarise(max_segment = max(.data$value), .groups = 'drop') %>%
                        dplyr::arrange(desc(.data$max_segment)) %>%
                        dplyr::pull(!!rlang::sym(x_var))
                    df[[x_var]] <- factor(df[[x_var]], levels = unique(category_order))
                } else if (self$options$sort_categories == "alpha") {
                    df[[x_var]] <- factor(df[[x_var]], levels = sort(unique(as.character(df[[x_var]]))))
                }
            }

            # Basic stacked-to-100% bar chart
            p <- ggplot2::ggplot(
                df,
                ggplot2::aes(
                    x = !!rlang::sym(x_var),
                    y = .data$value,
                    fill = !!rlang::sym(fill_var)
                )
            ) +
                ggplot2::geom_col(position = "fill", width = self$options$bar_width)

            # Orientation
            if (identical(self$options$orientation, "horizontal"))
                p <- p + ggplot2::coord_flip()

            # Faceting (simple)
            if (!is.null(self$options$facet_var) && self$options$facet_var != "") {
                p <- p + ggplot2::facet_wrap(rlang::sym(self$options$facet_var), scales = "free_x")
            }

            # Percentage labels (simple, inline)
            if (isTRUE(self$options$show_percentages)) {
                label_data <- df
                if (!"percentage" %in% names(label_data)) {
                    label_data <- label_data %>%
                        dplyr::group_by(!!rlang::sym(x_var)) %>%
                        dplyr::mutate(percentage = .data$value / sum(.data$value) * 100) %>%
                        dplyr::ungroup()
                }
                label_data <- label_data %>%
                    dplyr::filter(.data$percentage >= self$options$label_threshold)

                # Format
                fmt <- switch(
                    self$options$percentage_format,
                    "decimal1" = function(z) paste0(format(round(z, 1), nsmall = 1), "%"),
                    "decimal2" = function(z) paste0(format(round(z, 2), nsmall = 2), "%"),
                    function(z) paste0(round(z), "%")
                )
                label_data$..lab <- fmt(label_data$percentage)
                if (isTRUE(self$options$show_counts)) {
                    # Prefer value if present, else count
                    if ("value" %in% names(label_data))
                        label_data$..lab <- paste0(label_data$..lab, "\n(n=", label_data$value, ")")
                    else if ("count" %in% names(label_data))
                        label_data$..lab <- paste0(label_data$..lab, "\n(n=", label_data$count, ")")
                }

                if (nrow(label_data) > 0) {
                    p <- p + ggplot2::geom_text(
                        data = label_data,
                        ggplot2::aes(
                            x = !!rlang::sym(x_var),
                            y = .data$value,
                            label = .data$..lab
                        ),
                        position = ggplot2::position_fill(vjust = 0.5),
                        size = 3, color = "white", fontface = "bold",
                        inherit.aes = FALSE
                    )
                }
            }

            # Outlines
            if (isTRUE(self$options$add_outline) && !identical(self$options$outline_color, "none")) {
                outline_col <- switch(
                    self$options$outline_color,
                    black = "black",
                    gray = "gray60",
                    white = "white",
                    "white"
                )
                p <- p + ggplot2::geom_col(
                    position = "fill",
                    width = self$options$bar_width,
                    color = outline_col,
                    fill = NA,
                    linewidth = 0.2
                )
            }

            # Titles and legend
            plot_title <- if (nzchar(self$options$plot_title)) self$options$plot_title else .("Segmented Total Bar Chart")
            x_title <- if (nzchar(self$options$x_title)) self$options$x_title else x_var
            y_title <- if (nzchar(self$options$y_title)) self$options$y_title else .("Percentage")
            legend_title <- if (nzchar(self$options$legend_title)) self$options$legend_title else fill_var

            p <- p + ggplot2::labs(title = plot_title, x = x_title, y = y_title, fill = legend_title)

            # Apply color palette using centralized method
            p <- private$.applyColorPalette(p, df, fill_var)

            # Apply chart style            # Apply theme
            p <- private$.applyTheme(p)
            
            # Ensure title is centered (override if needed)
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(hjust = 0.5)
            )
            if (!identical(self$options$legend_position, "right")) {
                if (identical(self$options$legend_position, "none"))
                    p <- p + ggplot2::theme(legend.position = "none")
                else
                    p <- p + ggplot2::theme(legend.position = self$options$legend_position)
            }

            # Export-friendly tweaks
            if (isTRUE(self$options$export_ready)) {
                p <- p + ggplot2::theme(
                    # text = ggplot2::element_text(family = "Arial"),
                    plot.background = ggplot2::element_rect(fill = "white", color = NA),
                    panel.background = ggplot2::element_rect(fill = "white", color = NA),
                    legend.background = ggplot2::element_rect(fill = "white", color = NA)
                )
            }

            return(p)
        },

        .plotFlerlage = function(df, image, ggtheme, theme, ...) {
            # Flerlage-style segmented total bar chart using ggsegmentedtotalbar package

            # Check if package is available
            if (!requireNamespace("ggsegmentedtotalbar", quietly = TRUE)) {
                private$.addNotice("ERROR", "Missing Package",
                    "The ggsegmentedtotalbar package is required for Flerlage-style plots. Install it with: install.packages('ggsegmentedtotalbar')")
                return(ggplot2::ggplot() + ggplot2::theme_void() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "ggsegmentedtotalbar package not installed",
                        size = 6, color = "red"))
            }

            x_var <- self$options$x_var
            fill_var <- self$options$fill_var

            # Prepare data for ggsegmentedtotalbar
            # The package expects: group, segment, value, total columns
            plot_data <- df %>%
                dplyr::group_by(!!rlang::sym(x_var)) %>%
                dplyr::mutate(total = sum(.data$value)) %>%
                dplyr::ungroup() %>%
                dplyr::rename(
                    group = !!rlang::sym(x_var),
                    segment = !!rlang::sym(fill_var)
                )

            # Apply sorting if requested
            if (self$options$sort_categories != "none") {
                if (self$options$sort_categories == "total") {
                    category_order <- plot_data %>%
                        dplyr::group_by(.data$group) %>%
                        dplyr::summarise(total = sum(.data$value), .groups = 'drop') %>%
                        dplyr::arrange(desc(.data$total)) %>%
                        dplyr::pull(.data$group)
                    plot_data$group <- factor(plot_data$group, levels = unique(category_order))
                } else if (self$options$sort_categories == "alpha") {
                    plot_data$group <- factor(plot_data$group, levels = sort(unique(as.character(plot_data$group))))
                }
            }

            # Create base Flerlage plot
            p <- ggsegmentedtotalbar::ggsegmentedtotalbar(
                data = plot_data,
                group = "group",
                segment = "segment",
                value = "value",
                total = "total",
                label = self$options$flerlage_show_labels,
                label_size = self$options$flerlage_label_size,
                label_color = self$options$flerlage_label_color,
                alpha = self$options$flerlage_alpha,
                color = self$options$flerlage_box_color
            )

            # Add custom titles
            p <- p + ggplot2::labs(
                title = if (nchar(self$options$plot_title) > 0) self$options$plot_title else NULL,
                x = if (nchar(self$options$x_title) > 0) self$options$x_title else x_var,
                y = if (nchar(self$options$y_title) > 0) self$options$y_title else "Total",
                fill = if (nchar(self$options$legend_title) > 0) self$options$legend_title else fill_var
            )

            # Apply chart style
            chart_theme <- private$.getChartTheme(self$options$chart_style)
            if (!is.null(chart_theme)) {
                p <- p + chart_theme
            }

            # Apply color palette
            palette_colors <- private$.getColorPalette(
                self$options$color_palette,
                length(unique(plot_data$segment))
            )
            if (!is.null(palette_colors)) {
                p <- p + ggplot2::scale_fill_manual(values = palette_colors)
            }

            # Legend position
            if (!identical(self$options$legend_position, "none")) {
                p <- p + ggplot2::theme(legend.position = self$options$legend_position)
            } else {
                p <- p + ggplot2::theme(legend.position = "none")
            }

            # Orientation (if horizontal requested)
            if (identical(self$options$orientation, "horizontal")) {
                p <- p + ggplot2::coord_flip()
            }

            # Export-friendly tweaks
            if (isTRUE(self$options$export_ready)) {
                p <- p + ggplot2::theme(
                    plot.background = ggplot2::element_rect(fill = "white", color = NA),
                    panel.background = ggplot2::element_rect(fill = "white", color = NA),
                    legend.background = ggplot2::element_rect(fill = "white", color = NA)
                )
            }

            return(p)
        },

        .createClinicalSummary = function() {

            data <- private$.processed_data
            composition_data <- private$.composition_data

            if (is.null(data) || is.null(composition_data)) {
                return()
            }

            # Calculate key metrics
            total_obs <- sum(composition_data$count)
            n_categories <- length(unique(data[[self$options$x_var]]))
            n_segments <- length(unique(data[[self$options$fill_var]]))

            # Find dominant patterns
            largest_segment <- composition_data %>%
                dplyr::arrange(desc(percentage)) %>%
                dplyr::slice(1)

            # Find most balanced category
            category_balance <- composition_data %>%
                dplyr::group_by(category) %>%
                dplyr::summarise(
                    cv = sd(percentage) / mean(percentage),
                    .groups = 'drop'
                ) %>%
                dplyr::arrange(cv) %>%
                dplyr::slice(1)

            # Generate clinical summary text
            summary_text <- sprintf(
                .("This analysis examined %d observations across %d categories of %s, with data segmented by %s into %d distinct groups. The most prominent finding was '%s' in the '%s' category, representing %.1f%% of that group. The '%s' category showed the most balanced distribution across all segments, indicating relatively equal representation of different outcomes within that group."),
                as.integer(total_obs),
                as.integer(n_categories),
                self$options$x_var,
                self$options$fill_var,
                as.integer(n_segments),
                largest_segment$segment[1],
                largest_segment$category[1],
                largest_segment$percentage[1],
                category_balance$category[1]
            )

            # Create copy-ready report sentence
            report_sentence <- sprintf(
                .("Segmented total bar chart analysis of %s by %s (N=%d): %s was the predominant segment in %s (%.1f%%). Distribution patterns varied across categories, with %s showing the most balanced composition."),
                self$options$y_var,
                self$options$x_var,
                as.integer(total_obs),
                largest_segment$segment[1],
                largest_segment$category[1],
                largest_segment$percentage[1],
                category_balance$category[1]
            )

            # Create clinical summary HTML
            clinical_summary_html <- paste(
                "<div style='background-color: #f0f8ff; padding: 20px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #1e88e5;'>",
                paste0("<h4 style='color: #1565c0; margin-top: 0;'>", .("Clinical Summary"), "</h4>"),
                paste0("<p style='margin: 10px 0; line-height: 1.6;'>", summary_text, "</p>"),
                paste0("<div style='background-color: #e3f2fd; padding: 12px; border-radius: 4px; margin: 15px 0;'>"),
                paste0("<h5 style='color: #0d47a1; margin-top: 0;'>", .("Copy-Ready Report Sentence:"), "</h5>"),
                paste0("<p style='margin: 5px 0; font-style: italic; font-size: 14px;'>", report_sentence, "</p>"),
                paste0("</div>"),
                paste0("<div style='margin-top: 15px; font-size: 12px; color: #666;'>"),
                paste0("<strong>", .("Clinical Application:"), "</strong> ", .("Use this visualization to compare proportional outcomes across treatment groups, patient populations, or time periods. Ideal for showing response rates, demographic compositions, or biomarker distributions in clinical research.")),
                paste0("</div>"),
                "</div>"
            )

            self$results$clinical_summary$setContent(clinical_summary_html)
        },
        
        .applyPresetConfiguration = function() {
            
            preset <- self$options$analysis_preset
            
            if (preset == "custom") {
                return()  # No preset configuration
            }
            
            # Store preset configuration for reference
            preset_configs <- list(
                treatment_response = list(
                    suggested_x_label = "Treatment Group",
                    suggested_y_label = "Response Rate (%)",
                    suggested_fill_label = "Response Type",
                    color_palette = "clinical",
                    chart_style = "clinical",
                    show_percentages = TRUE,
                    percentage_format = "integer",
                    guidance = "Ideal for comparing treatment efficacy across different patient groups. Each bar shows the proportion of complete, partial, and no response within each treatment arm."
                ),
                demographics = list(
                    suggested_x_label = "Disease Stage",
                    suggested_y_label = "Patient Distribution (%)",
                    suggested_fill_label = "Demographic Category",
                    color_palette = "colorblind",
                    chart_style = "publication",
                    show_percentages = TRUE,
                    percentage_format = "decimal1",
                    guidance = "Perfect for showing patient characteristics across disease stages or time points. Helps identify demographic patterns and potential confounders."
                ),
                biomarker = list(
                    suggested_x_label = "Mutation Status",
                    suggested_y_label = "Expression Distribution (%)",
                    suggested_fill_label = "Expression Level",
                    color_palette = "viridis",
                    chart_style = "clinical",
                    show_percentages = TRUE,
                    percentage_format = "decimal1",
                    guidance = "Excellent for biomarker analysis showing expression patterns across genetic variants. Useful for stratified medicine and precision oncology."
                ),
                quality = list(
                    suggested_x_label = "Department/Unit",
                    suggested_y_label = "Quality Score Distribution (%)",
                    suggested_fill_label = "Performance Rating",
                    color_palette = "bbc_multi",
                    chart_style = "presentation",
                    show_percentages = TRUE,
                    percentage_format = "integer",
                    guidance = "Ideal for quality improvement initiatives. Shows performance metrics distribution across departments or time periods for benchmarking."
                ),
                temporal = list(
                    suggested_x_label = "Time Point",
                    suggested_y_label = "Patient Status (%)",
                    suggested_fill_label = "Outcome Status",
                    color_palette = "science",
                    chart_style = "clinical",
                    show_percentages = TRUE,
                    percentage_format = "decimal1",
                    guidance = "Perfect for longitudinal studies showing patient outcomes over time. Each time point shows proportions of different status categories."
                )
            )
            
            if (preset %in% names(preset_configs)) {
                private$.preset_config <- preset_configs[[preset]]
            }
        },
        
        .updatePresetGuidance = function() {
            
            if (is.null(private$.preset_config)) {
                return()
            }
            
            config <- private$.preset_config
            preset_name <- stringr::str_to_title(gsub("_", " ", self$options$analysis_preset))
            
            guidance_html <- paste(
                "<div style='background-color: #e8f4fd; padding: 15px; border-radius: 6px; margin: 10px 0; border-left: 4px solid #2196f3;'>",
                paste0("<h4 style='color: #1565c0; margin-top: 0;'>", preset_name, " Template</h4>"),
                paste0("<p style='margin: 8px 0; line-height: 1.5;'>", config$guidance, "</p>"),
                "<div style='background-color: #fff; padding: 10px; border-radius: 4px; margin: 10px 0;'>",
                "<h5 style='color: #333; margin-top: 0;'>Suggested Variable Labels:</h5>",
                "<ul style='margin: 5px 0; padding-left: 20px;'>",
                paste0("<li><strong>X-axis:</strong> ", config$suggested_x_label, "</li>"),
                paste0("<li><strong>Y-axis:</strong> ", config$suggested_y_label, "</li>"),
                paste0("<li><strong>Segments:</strong> ", config$suggested_fill_label, "</li>"),
                "</ul>",
                "</div>",
                "<div style='background-color: #f8f9fa; padding: 10px; border-radius: 4px; margin: 10px 0;'>",
                "<h5 style='color: #495057; margin-top: 0;'>Clinical Template Details:</h5>",
                "<p style='font-size: 13px; line-height: 1.4; margin: 5px 0;'>This template optimizes chart settings, color palettes, and statistical options for your specific analysis type. All settings can be further customized as needed.</p>",
                "</div>",
                "</div>"
            )
            
            self$results$preset_guidance$setContent(guidance_html)
        },
        
        .performStatisticalTests = function() {
            
            data <- private$.processed_data
            
            if (is.null(data)) {
                return()
            }
            
            # Clear existing results
            self$results$statistical_tests$deleteRows()
            
            tryCatch({
                # Create contingency table for chi-square test
                x_var <- self$options$x_var
                fill_var <- self$options$fill_var

                # Convert processed data back to contingency table format
                contingency_data <- data %>%
                    dplyr::select(!!rlang::sym(x_var), !!rlang::sym(fill_var), count)

                # Create matrix for chi-square test
                contingency_matrix <- xtabs(count ~ ., data = contingency_data)

                # Perform chi-square test
                chi_test <- chisq.test(contingency_matrix)

                # Determine significance
                alpha <- 1 - self$options$confidence_level
                is_significant <- chi_test$p.value < alpha

                interpretation <- if (is_significant) {
                    paste0("Significant association between ", x_var, " and ", fill_var, " (p < ", alpha, ")")
                } else {
                    paste0("No significant association between ", x_var, " and ", fill_var, " (p >= ", alpha, ")")
                }

                # Add test results to table
                test_row <- list(
                    test_name = "Pearson's Chi-square",
                    statistic = chi_test$statistic[[1]],
                    df = chi_test$parameter[[1]],
                    p_value = chi_test$p.value,
                    interpretation = interpretation
                )

                self$results$statistical_tests$addRow(rowKey = 1, values = test_row)
                
                # If significant, add post-hoc analysis
                if (is_significant && nrow(contingency_matrix) > 2 && ncol(contingency_matrix) > 2) {
                    
                    # Calculate standardized residuals for interpretation
                    residuals <- chi_test$stdres
                    max_residual_pos <- which(abs(residuals) == max(abs(residuals), na.rm = TRUE), arr.ind = TRUE)
                    
                    if (length(max_residual_pos) > 0 && nrow(max_residual_pos) > 0) {
                        max_row <- rownames(residuals)[max_residual_pos[1, 1]]
                        max_col <- colnames(residuals)[max_residual_pos[1, 2]]
                        residual_value <- residuals[max_residual_pos[1, 1], max_residual_pos[1, 2]]
                        
                        residual_interpretation <- if (residual_value > 0) {
                            paste0("Highest positive association: ", max_row, " → ", max_col)
                        } else {
                            paste0("Highest negative association: ", max_row, " → ", max_col)
                        }
                        
                        residual_row <- list(
                            test_name = "Standardized Residuals",
                            statistic = abs(residual_value),
                            df = NA,
                            p_value = NA,
                            interpretation = residual_interpretation
                        )
                        
                        self$results$statistical_tests$addRow(rowKey = 2, values = residual_row)
                    }
                }
                
            }, error = function(e) {
                # Add error row if test fails
                error_row <- list(
                    test_name = "Chi-square Test",
                    statistic = NA,
                    df = NA,
                    p_value = NA,
                    interpretation = paste("Test failed:", e$message)
                )
                
                self$results$statistical_tests$addRow(rowKey = 1, values = error_row)
            })

            # Render all collected notices as HTML
            private$.renderNotices()
        }
    )
)
