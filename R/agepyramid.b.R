#' @title Age Pyramid
#' @description Generates an age pyramid plot from the provided data.
#' The function allows customization of bin width (age group granularity) and plot title.
#' It creates a visually appealing plot showing the distribution of age by gender.
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import tibble
#'
#' @param age The name of the column containing age data.
#'

agepyramidClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "agepyramidClass",
    inherit = agepyramidBase,
    private = list(
        .run = function() {
            # Check if required options (age and gender) are provided
            if (is.null(self$options$age) || is.null(self$options$gender)) {
                self$results$welcome$setContent(
                    "<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; border-left: 4px solid #2196F3;'>
                    <h3 style='color: #1976d2; margin-top: 0;'>📊 Age Pyramid Analysis</h3>
                    <p style='font-size: 15px;'>Create demographic visualizations showing age distribution by gender.</p>
                    <h4 style='color: #1976d2; margin-bottom: 8px;'>Required Variables:</h4>
                    <ol style='font-size: 14px; line-height: 1.6;'>
                        <li><strong>Age:</strong> Continuous numeric variable (e.g., patient age in years)</li>
                        <li><strong>Gender:</strong> Categorical variable (typically binary: Male/Female)</li>
                    </ol>
                    <h4 style='color: #1976d2; margin-bottom: 8px;'>Features:</h4>
                    <ul style='font-size: 14px; line-height: 1.6;'>
                        <li><strong>Age group presets:</strong> Pediatric (<18), Reproductive (15-50), Geriatric (65+), Life Course, or Custom</li>
                        <li><strong>Custom age breaks:</strong> Define your own age boundaries (e.g., 0,18,25,50,65,100)</li>
                        <li><strong>Customizable bin width</strong> for automatic age grouping</li>
                        <li><strong>Color palettes:</strong> Standard, Colorblind-friendly, Grayscale, or Custom colors</li>
                        <li><strong>Readable age group labels</strong> (e.g., 1-5, 6-10, 86+)</li>
                        <li><strong>Table with counts and percentages</strong></li>
                        <li><strong>Gender level selection</strong> for flexible data structures</li>
                    </ul>
                    <p style='font-size: 13px; color: #666; margin-bottom: 0; font-style: italic;'>
                    Select your Age and Gender variables to begin.
                    </p>
                    </div>"
                )
                return()
            }

            if (nrow(self$data) == 0)
                stop("Data contains no (complete) rows")

            # Read and prepare data ----
            mydata <- self$data

            age <- self$options$age
            gender <- self$options$gender

            # Select and clean the required columns
            mydata <- jmvcore::select(mydata, c(age, gender))
            mydata <- jmvcore::naOmit(mydata)

            # Convert age to numeric and gender to factor
            # Use as.character before as.numeric to handle factors correctly
            mydata[["Age"]] <- as.numeric(as.character(mydata[[age]]))
            mydata[["Gender"]] <- as.factor(mydata[[gender]])

            # Determine gender levels with smart defaults ----
            n_initial <- nrow(self$data)  # Track for data summary
            female_level <- self$options$female
            male_level <- self$options$male
            gender_levels <- levels(mydata[["Gender"]])

            # Apply smart defaults if levels not selected
            if (is.null(female_level) && is.null(male_level)) {
                # Neither selected - use first two levels
                if (length(gender_levels) >= 2) {
                    female_level <- gender_levels[1]
                    male_level <- gender_levels[2]
                } else if (length(gender_levels) == 1) {
                    # Single level - treat as female
                    female_level <- gender_levels[1]
                    male_level <- NULL
                }
            } else if (is.null(female_level)) {
                # Only male selected - use first non-male level as female
                remaining <- gender_levels[gender_levels != male_level]
                female_level <- if(length(remaining) > 0) remaining[1] else NULL
            } else if (is.null(male_level)) {
                # Only female selected - use first non-female level as male
                remaining <- gender_levels[gender_levels != female_level]
                male_level <- if(length(remaining) > 0) remaining[1] else NULL
            }

            # Validate that female and male levels are different
            if (!is.null(female_level) && !is.null(male_level) && female_level == male_level) {
                error_html <- paste0(
                    "<div style='background-color: #ffebee; padding: 20px; border-radius: 8px; border-left: 4px solid #f44336;'>",
                    "<h3 style='color: #c62828; margin-top: 0;'>⚠️ Configuration Error</h3>",
                    "<p style='font-size: 15px;'><strong>Female and Male gender levels cannot be the same.</strong></p>",
                    "<p style='font-size: 14px;'>You have selected '<strong>", female_level, "</strong>' for both Female and Male.</p>",
                    "<h4 style='color: #c62828; margin-bottom: 8px;'>To fix this:</h4>",
                    "<ol style='font-size: 14px; line-height: 1.6;'>",
                    "    <li>Select different levels for Female and Male, OR</li>",
                    "    <li>Leave one or both unselected to use auto-detection</li>",
                    "</ol>",
                    "<p style='font-size: 13px; color: #666; margin-bottom: 0; font-style: italic;'>",
                    "The age pyramid requires two distinct gender categories for comparison.",
                    "</p>",
                    "</div>"
                )
                self$results$dataInfo$setContent(error_html)
                return()
            }

            # Detect single-gender cohort
            is_single_gender <- is.null(male_level) || is.null(female_level)
            single_gender_label <- if(!is.null(female_level)) female_level else male_level

            # Create standardized gender variable
            is_female <- if (!is.null(female_level)) {
                mydata[["Gender"]] == female_level
            } else {
                FALSE
            }
            is_male <- if (!is.null(male_level)) {
                mydata[["Gender"]] == male_level
            } else {
                FALSE
            }
            mydata <- mydata %>%
                dplyr::mutate(
                    Gender2 = dplyr::case_when(
                        is_female ~ "Female",
                        is_male ~ "Male",
                        TRUE ~ NA_character_  # Other values become NA
                    )
                ) %>%
                dplyr::filter(!is.na(Gender2))  # Remove unrecognized genders

            n_final <- nrow(mydata)  # Track for data summary

            # Determine age group breaks based on preset or custom bin width ----
            age_groups <- if (!is.null(self$options$age_groups)) self$options$age_groups else 'custom'
            max_age <- max(mydata[["Age"]], na.rm = TRUE)

            # Select breaks based on age_groups option
            if (age_groups == 'pediatric') {
                # Pediatric: Birth to 18 years with developmental milestones
                breaks_seq <- c(0, 1, 2, 5, 10, 15, 18, Inf)
            } else if (age_groups == 'reproductive') {
                # Reproductive age: 15-50 with 5-year intervals
                breaks_seq <- c(0, 15, 20, 25, 30, 35, 40, 45, 50, Inf)
            } else if (age_groups == 'geriatric') {
                # Geriatric: 65+ with 5-year intervals
                breaks_seq <- c(0, 65, 70, 75, 80, 85, 90, 95, Inf)
            } else if (age_groups == 'lifecourse') {
                # Life course: Key developmental stages
                breaks_seq <- c(0, 5, 15, 25, 45, 65, 75, 85, Inf)
            } else {
                # Custom: Check for custom_breaks first, then use bin_width
                custom_breaks <- self$options$custom_breaks
                if (!is.null(custom_breaks) && nchar(trimws(custom_breaks)) > 0) {
                    # Parse comma-separated values
                    breaks_seq <- tryCatch({
                        breaks_str <- trimws(strsplit(custom_breaks, ",")[[1]])
                        breaks_num <- as.numeric(breaks_str)
                        # Remove NA values and sort
                        breaks_num <- sort(unique(breaks_num[!is.na(breaks_num)]))
                        # Add Inf at the end if not present
                        if (tail(breaks_num, 1) != Inf) {
                            breaks_num <- c(breaks_num, Inf)
                        }
                        breaks_num
                    }, error = function(e) {
                        # Fall back to bin_width if parsing fails
                        bin_width <- if (!is.null(self$options$bin_width)) self$options$bin_width else 5
                        breaks <- seq(from = 0, to = max_age, by = bin_width)
                        if (max_age > tail(breaks, n = 1)) {
                            breaks <- c(breaks, max_age)
                        }
                        breaks
                    })
                } else {
                    # Use bin_width
                    bin_width <- if (!is.null(self$options$bin_width)) self$options$bin_width else 5
                    breaks_seq <- seq(from = 0, to = max_age, by = bin_width)
                    if (max_age > tail(breaks_seq, n = 1)) {
                        breaks_seq <- c(breaks_seq, max_age)
                    }
                }
            }

            # Safeguard: ensure we have at least two unique breaks for cut()
            if (length(unique(breaks_seq)) < 2) {
                # Fallback to a single bin [0, max_age+1] if data is restricted
                breaks_seq <- c(0, max_age + 1)
            }

            # Create readable age group labels
            labels <- private$.create_age_labels(breaks_seq)

            mydata[["Pop"]] <- cut(mydata[["Age"]],
                                   include.lowest = TRUE,
                                   right = TRUE,
                                   breaks = breaks_seq,
                                   labels = labels,
                                   ordered_result = FALSE)

            # Prepare data for plotting and table output ----
            plotData <- mydata %>%
                dplyr::select(Gender = Gender2, Pop) %>%
                dplyr::group_by(Gender, Pop) %>%
                dplyr::count() %>%
                dplyr::ungroup() %>%
                dplyr::arrange(Pop) %>%
                as.data.frame()

            # Save state for plot rendering; ensures plot gets updated when bin_width changes
            image <- self$results$plot
            image$setState(plotData)

            # Also save state for ggcharts plot (if enabled)
            if (self$options$enableGGCharts) {
                imageGGCharts <- self$results$plotGGCharts
                imageGGCharts$setState(plotData)
            }

            # Pivot data for table output ----
            plotData2 <- plotData %>%
                tidyr::pivot_wider(names_from = Gender,
                                   values_from = n,
                                   values_fill = list(n = 0)) %>%  # Fill missing counts with 0
                dplyr::arrange(dplyr::desc(Pop)) %>%
                dplyr::filter(!is.na(Pop)) %>%
                dplyr::mutate(Pop = as.character(Pop)) %>%
                as.data.frame()

            # Calculate totals and add percentages ----
            if (!("Female" %in% names(plotData2))) {
                plotData2$Female <- 0
            }
            if (!("Male" %in% names(plotData2))) {
                plotData2$Male <- 0
            }
            total_female <- sum(plotData2$Female, na.rm = TRUE)
            total_male <- sum(plotData2$Male, na.rm = TRUE)

            # Add percentage columns (safe division)
            plotData2$Female_Pct <- ifelse(total_female > 0,
                round(plotData2$Female / total_female * 100, 1), 0)
            plotData2$Male_Pct <- ifelse(total_male > 0,
                round(plotData2$Male / total_male * 100, 1), 0)

            # Add summary row
            summary_row <- data.frame(
                Pop = "Total",
                Female = total_female,
                Male = total_male,
                Female_Pct = ifelse(total_female > 0, 100.0, 0),
                Male_Pct = ifelse(total_male > 0, 100.0, 0),
                stringsAsFactors = FALSE
            )

            plotData2 <- rbind(plotData2, summary_row)

            # Populate the results table ----
            pyramidTable <- self$results$pyramidTable
            for(i in seq_len(nrow(plotData2))) {
                pyramidTable$addRow(rowKey = i, values = plotData2[i,])
            }

            # Build data summary HTML ----
            info_html <- private$.build_data_summary_html(
                n_initial = n_initial,
                n_final = n_final,
                is_single_gender = is_single_gender,
                female_level = female_level,
                male_level = male_level,
                single_gender_label = single_gender_label
            )
            self$results$dataInfo$setContent(info_html)
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Check if required options (age and gender) are provided
            if (is.null(self$options$age) || is.null(self$options$gender))
                return()

            if (nrow(self$data) == 0)
                stop("Data contains no (complete) rows")

            # Retrieve the prepared plot data
            plotData <- image$state

            # Return early if no plot data available (e.g., validation errors in .run())
            if (is.null(plotData))
                return(FALSE)

            # Ensure that the age bins (Pop) reflect the latest bin width:
            # Convert 'Pop' to character then back to factor with the order of appearance.
            plotData$Pop <- factor(as.character(plotData$Pop), levels = unique(as.character(plotData$Pop)))

            # Set plot title (using user option if provided)
            plot_title <- if (!is.null(self$options$plot_title)) self$options$plot_title else "Age Pyramid"

            # Determine color palette ----
            color_palette <- self$options$color_palette
            if (is.null(color_palette) || length(color_palette) == 0) {
                color_palette <- 'standard'
            }

            # Set colors based on palette selection
            if (color_palette == 'colorblind') {
                # Orange/Blue palette (colorblind-friendly)
                color_female <- "#E69F00"  # Orange
                color_male <- "#0072B2"    # Blue
            } else if (color_palette == 'grayscale') {
                # Grayscale palette
                color_female <- "#666666"  # Dark gray
                color_male <- "#CCCCCC"    # Light gray
            } else if (color_palette == 'custom') {
                # Custom colors from user
                color_female <- self$options$female_color
                color_male <- self$options$male_color
                # Fallback to defaults if colors are empty
                if (is.null(color_female) || nchar(trimws(color_female)) == 0) {
                    color_female <- "#E91E63"
                }
                if (is.null(color_male) || nchar(trimws(color_male)) == 0) {
                    color_male <- "#2196F3"
                }
            } else {
                # Standard palette (default pink/blue)
                color_female <- "#E91E63"  # Pink
                color_male <- "#2196F3"    # Blue
            }

            # Create a visually appealing age pyramid plot ----
            plot <- ggplot2::ggplot(data = plotData,
                                    mapping = ggplot2::aes(
                                        x = Pop,
                                        y = ifelse(Gender == "Female", -n, n),
                                        fill = Gender
                                    )) +
                ggplot2::geom_col(width = 0.7, color = "black", show.legend = TRUE) +  # Added border for clarity
                ggplot2::coord_flip() +
                ggplot2::scale_y_continuous(labels = abs,
                                            limits = c(-max(plotData$n, na.rm = TRUE), max(plotData$n, na.rm = TRUE))
                ) +
                ggplot2::scale_fill_manual(values = c("Female" = color_female, "Male" = color_male)) +
                ggplot2::labs(x = "Age Group",
                              y = "Population Count",
                              title = plot_title,
                              fill = "Gender")

            # Apply theme based on user preference
            if (!self$options$originaltheme) {
                # Use jamovi's theme
                plot <- plot + ggtheme
            } else {
                # Use original code's custom theme
                plot <- plot +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        axis.text = ggplot2::element_text(size = 10),
                        axis.title = ggplot2::element_text(size = 12),
                        legend.position = "bottom"
                    )
            }

            print(plot)
            return(TRUE)
        },

        .plotGGCharts = function(image, ggtheme, theme, ...) {
            # Check if ggcharts plot is enabled
            if (!self$options$enableGGCharts)
                return(FALSE)

            # Check if required options (age and gender) are provided
            if (is.null(self$options$age) || is.null(self$options$gender))
                return(FALSE)

            if (nrow(self$data) == 0)
                return(FALSE)

            # Retrieve the prepared plot data from .run()
            # We'll use the same data preparation that was done for the main plot
            plotData <- image$state

            # Return early if no plot data available
            if (is.null(plotData))
                return(FALSE)

            # ggcharts pyramid_chart requires long-format data with:
            # - x: age groups (categorical)
            # - y: population counts (numeric)
            # - group: gender categories (exactly 2 unique values)

            # The plotData from .run() is already in the correct format:
            # columns: Gender, Pop, n

            # Determine bar colors based on color palette selection
            color_scheme <- self$options$ggcharts_colors
            if (is.null(color_scheme) || length(color_scheme) == 0) {
                color_scheme <- 'default'
            }

            # Set colors based on palette selection
            if (color_scheme == 'default') {
                # ggcharts default: Blue and Orange
                bar_colors <- c("#1F77B4", "#FF7F0E")
            } else if (color_scheme == 'standard') {
                # Standard: Pink and Blue (matching main plot)
                bar_colors <- c("#E91E63", "#2196F3")
            } else if (color_scheme == 'colorblind') {
                # Colorblind-friendly: Orange and Blue
                bar_colors <- c("#E69F00", "#0072B2")
            } else if (color_scheme == 'grayscale') {
                # Grayscale
                bar_colors <- c("#666666", "#CCCCCC")
            } else if (color_scheme == 'custom') {
                # Custom colors from user
                color1 <- self$options$ggcharts_color1
                color2 <- self$options$ggcharts_color2
                # Fallback to defaults if colors are empty
                if (is.null(color1) || nchar(trimws(color1)) == 0) {
                    color1 <- "#1F77B4"
                }
                if (is.null(color2) || nchar(trimws(color2)) == 0) {
                    color2 <- "#FF7F0E"
                }
                bar_colors <- c(color1, color2)
            } else {
                # Default fallback
                bar_colors <- c("#1F77B4", "#FF7F0E")
            }

            # Get other options
            sort_option <- if (!is.null(self$options$ggcharts_sort)) {
                self$options$ggcharts_sort
            } else {
                "no"
            }

            plot_title <- if (!is.null(self$options$ggcharts_title)) {
                self$options$ggcharts_title
            } else {
                "Age Pyramid (ggcharts)"
            }

            xlab_text <- if (!is.null(self$options$ggcharts_xlab)) {
                self$options$ggcharts_xlab
            } else {
                "Population"
            }

            # Create the ggcharts pyramid
            tryCatch({
                plot <- ggcharts::pyramid_chart(
                    data = plotData,
                    x = Pop,
                    y = n,
                    group = Gender,
                    bar_colors = bar_colors,
                    sort = sort_option,
                    xlab = xlab_text,
                    title = plot_title
                )

                print(plot)
                return(TRUE)
            }, error = function(e) {
                # If ggcharts fails, show informative message
                warning("ggcharts pyramid failed: ", e$message)
                return(FALSE)
            })
        },

        .create_age_labels = function(breaks) {
            # Create readable labels from age breaks
            # With right=TRUE in cut(), intervals are (lower, upper]
            # So we label as "lower+1 to upper" for clinical interpretation
            if (length(breaks) < 2) return(c())

            labels <- c()
            for (i in seq_len(length(breaks) - 1)) {
                lower <- breaks[i]
                upper <- breaks[i + 1]
                if (is.infinite(upper)) {
                    # Open-ended final category: "86+" for (85, Inf]
                    labels[i] <- paste0(lower + 1, "+")
                } else {
                    # Closed intervals: "1-5" for (0,5], "6-10" for (5,10]
                    labels[i] <- paste(lower + 1, upper, sep = "-")
                }
            }
            return(labels)
        },

        .build_data_summary_html = function(n_initial, n_final, is_single_gender,
                                           female_level, male_level, single_gender_label) {
            # Build informative HTML showing data quality and gender level info
            n_excluded <- n_initial - n_final

            html <- "<div style='background-color: #e3f2fd; padding: 15px; border-radius: 8px; border-left: 4px solid #2196F3;'>"
            html <- paste0(html, "<h4 style='margin: 0 0 8px 0; color: #1976d2;'>📊 Data Summary</h4>")
            html <- paste0(html, "<table style='width: 100%; font-size: 14px;'>")
            html <- paste0(html, "<tr><td><strong>Initial observations:</strong></td><td>", n_initial, "</td></tr>")
            html <- paste0(html, "<tr><td><strong>Final observations:</strong></td><td>", n_final, "</td></tr>")

            if (n_excluded > 0) {
                pct_excluded <- round(n_excluded / n_initial * 100, 1)
                html <- paste0(html, "<tr><td><strong>Excluded:</strong></td><td style='color: #d32f2f;'>",
                    n_excluded, " (", pct_excluded, "%)</td></tr>")
            }

            if (is_single_gender) {
                html <- paste0(html, "<tr><td><strong>Cohort type:</strong></td><td style='color: #f57c00;'>",
                    "Single-gender (", single_gender_label, ")</td></tr>")
            } else {
                html <- paste0(html, "<tr><td><strong>Female level:</strong></td><td>", female_level, "</td></tr>")
                html <- paste0(html, "<tr><td><strong>Male level:</strong></td><td>", male_level, "</td></tr>")
            }

            html <- paste0(html, "</table></div>")
            return(html)
        }
    )
)
