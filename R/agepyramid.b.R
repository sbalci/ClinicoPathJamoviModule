#' @title Age Pyramid
#' @description Generates an age pyramid plot from the provided data.
#' The function allows customization of bin width (age group granularity), plot title,
#' and colors. It creates a visually appealing plot showing the
#' distribution of age by gender using ggplot2.
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#'
#' @param age The name of the column containing age data.
#'

agepyramidClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "agepyramidClass",
    inherit = agepyramidBase,
    private = list(
        .data_info_messages = list(),

        .run = function() {
            # Initialize
            private$.data_info_messages <- list()

            # Check required options
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
                    <p style='font-size: 13px; color: #666; margin-bottom: 0; font-style: italic;'>
                    Select your Age and Gender variables in the left panel to begin.
                    </p>
                    </div>"
                )
                return()
            }

            if (nrow(self$data) == 0)
                stop(.("Data contains no (complete) rows"))

            # Prepare data ----
            mydata <- self$data
            age_var <- self$options$age
            gender_var <- self$options$gender

            # Select columns and remove NAs
            mydata <- jmvcore::select(mydata, c(age_var, gender_var))
            mydata <- jmvcore::naOmit(mydata)

            # Convert types
            age_numeric <- as.numeric(as.character(mydata[[age_var]]))
            mydata$Age <- age_numeric
            mydata$Gender <- as.factor(mydata[[gender_var]])

            # Remove invalid ages
            n_before <- nrow(mydata)
            mydata <- mydata %>% dplyr::filter(!is.na(Age))
            n_after <- nrow(mydata)

            if (n_after == 0) {
                stop(.("No valid age data available"))
            }

            # Get gender levels ----
            female_level <- self$options$female
            male_level <- self$options$male
            gender_levels <- levels(mydata$Gender)

            # Apply smart defaults if needed
            if (is.null(female_level) && is.null(male_level)) {
                if (length(gender_levels) >= 2) {
                    female_level <- gender_levels[1]
                    male_level <- gender_levels[2]
                } else if (length(gender_levels) == 1) {
                    female_level <- gender_levels[1]
                    male_level <- NULL
                }
            } else if (is.null(female_level)) {
                remaining <- gender_levels[gender_levels != male_level]
                female_level <- if(length(remaining) > 0) remaining[1] else NULL
            } else if (is.null(male_level)) {
                remaining <- gender_levels[gender_levels != female_level]
                male_level <- if(length(remaining) > 0) remaining[1] else NULL
            }

            # Detect single-gender cohort
            is_single_gender <- is.null(male_level) || is.null(female_level)
            single_gender_label <- if(!is.null(female_level)) female_level else male_level

            # Create standardized Gender2 variable ----
            if (is_single_gender) {
                mydata <- mydata %>%
                    dplyr::mutate(
                        Gender2 = ifelse(Gender == single_gender_label, "Cohort", NA_character_)
                    )
            } else {
                mydata <- mydata %>%
                    dplyr::mutate(
                        Gender2 = dplyr::case_when(
                            Gender == female_level ~ "Female",
                            Gender == male_level ~ "Male",
                            TRUE ~ NA_character_
                        )
                    )
            }

            # Filter to valid genders
            mydata <- mydata %>% dplyr::filter(!is.na(Gender2))

            if (nrow(mydata) == 0) {
                stop(.("No valid data after gender filtering"))
            }

            # Create age bins ----
            age_groups_preset <- self$options$age_groups %||% "custom"
            bin_width <- self$options$bin_width %||% 5
            bin_width <- max(1, min(50, bin_width))

            if (age_groups_preset == "custom") {
                max_age <- max(mydata$Age, na.rm = TRUE)
                breaks_seq <- seq(from = 0, to = ceiling(max_age), by = bin_width)
                if (max_age > tail(breaks_seq, 1)) {
                    breaks_seq <- c(breaks_seq, ceiling(max_age))
                }
            } else {
                breaks_seq <- switch(age_groups_preset,
                    "pediatric" = c(0, 1, 2, 5, 10, 15, 18, Inf),
                    "reproductive" = c(0, 15, 20, 25, 30, 35, 40, 45, 50, Inf),
                    "geriatric" = c(0, 65, 70, 75, 80, 85, 90, 95, Inf),
                    "lifecourse" = c(0, 5, 15, 25, 45, 65, 75, 85, Inf),
                    seq(from = 0, to = ceiling(max(mydata$Age)), by = bin_width)
                )
            }

            # Create labels
            labels <- private$.create_age_labels(breaks_seq)

            # Bin the ages
            mydata$AgeGroup <- cut(mydata$Age,
                                   breaks = breaks_seq,
                                   labels = labels,
                                   include.lowest = TRUE,
                                   right = TRUE,
                                   ordered_result = FALSE)

            # Aggregate data for plotting ----
            plotData <- mydata %>%
                dplyr::group_by(Gender2, AgeGroup) %>%
                dplyr::count() %>%
                dplyr::ungroup() %>%
                as.data.frame()

            # Store state for plot ----
            # Include all visual options to trigger re-render on changes
            plotState <- list(
                data = plotData,
                is_single_gender = is_single_gender,
                single_gender_label = single_gender_label,
                # Visual options for reactive updates
                plot_title = self$options$plot_title,
                color_palette = self$options$color_palette,
                color1 = self$options$color1,
                color2 = self$options$color2
            )
            self$results$plot$setState(plotState)

            # Populate table ----
            tableData <- plotData %>%
                tidyr::pivot_wider(
                    names_from = Gender2,
                    values_from = n,
                    values_fill = 0
                ) %>%
                as.data.frame()

            # Handle single-gender vs binary-gender column naming
            if (is_single_gender) {
                # Single-gender: rename Cohort to appropriate column
                if ("Cohort" %in% names(tableData)) {
                    if (!is.null(female_level) && is.null(male_level)) {
                        tableData$Female <- tableData$Cohort
                        tableData$Male <- 0
                    } else {
                        tableData$Male <- tableData$Cohort
                        tableData$Female <- 0
                    }
                    tableData$Cohort <- NULL
                }
            } else {
                # Binary-gender: ensure both columns exist
                if (!"Female" %in% names(tableData)) tableData$Female <- 0
                if (!"Male" %in% names(tableData)) tableData$Male <- 0
            }

            # Remove any Cohort column if it still exists
            if ("Cohort" %in% names(tableData)) {
                tableData$Cohort <- NULL
            }

            # Calculate percentages
            total_female <- sum(tableData$Female, na.rm = TRUE)
            total_male <- sum(tableData$Male, na.rm = TRUE)

            tableData$Female_Pct <- ifelse(total_female > 0,
                round(tableData$Female / total_female * 100, 1), 0)
            tableData$Male_Pct <- ifelse(total_male > 0,
                round(tableData$Male / total_male * 100, 1), 0)

            # Add total row
            summary_row <- data.frame(
                AgeGroup = "Total",
                Female = total_female,
                Male = total_male,
                Female_Pct = ifelse(total_female > 0, 100, 0),
                Male_Pct = ifelse(total_male > 0, 100, 0),
                stringsAsFactors = FALSE
            )

            tableData <- rbind(tableData, summary_row)

            # Populate results table
            pyramidTable <- self$results$pyramidTable
            for (i in seq_len(nrow(tableData))) {
                pyramidTable$addRow(rowKey = i, values = tableData[i, ])
            }

            # Build data info HTML ----
            info_html <- private$.build_data_info_html(
                n_initial = nrow(self$data),
                n_final = nrow(mydata),
                is_single_gender = is_single_gender,
                female_level = female_level,
                male_level = male_level,
                single_gender_label = single_gender_label
            )
            self$results$dataInfo$setContent(info_html)
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Check if plot state exists
            plotState <- image$state
            if (is.null(plotState) || is.null(plotState$data)) {
                return(FALSE)
            }

            # Extract data and metadata
            plotData <- as.data.frame(plotState$data)
            is_single_gender <- plotState$is_single_gender %||% FALSE
            single_gender_label <- plotState$single_gender_label %||% "Unknown"

            # Get visual options from state (these trigger re-render on change)
            plot_title <- plotState$plot_title
            color_palette <- plotState$color_palette %||% "standard"
            color1 <- plotState$color1
            color2 <- plotState$color2

            # Validate data
            if (nrow(plotData) == 0) {
                return(FALSE)
            }

            # Ensure proper column types
            plotData$Gender2 <- as.character(plotData$Gender2)
            plotData$AgeGroup <- as.factor(plotData$AgeGroup)
            plotData$n <- as.numeric(plotData$n)

            # Determine colors ----
            if (color_palette == "standard") {
                female_color <- "#FF69B4"  # Hot pink
                male_color <- "#4169E1"    # Royal blue
            } else if (color_palette == "accessible") {
                female_color <- "#E69F00"  # Orange
                male_color <- "#56B4E9"    # Sky blue
            } else {
                female_color <- color1 %||% "#FF69B4"
                male_color <- color2 %||% "#4169E1"
            }

            # Create plot ----
            if (is_single_gender) {
                # Single-gender bar chart
                cohort_color <- if(!is.null(color1)) color1 else female_color

                if (is.null(plot_title) || plot_title == "") {
                    plot_title <- sprintf(.("Age Distribution - %s"), single_gender_label)
                }

                plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = AgeGroup, y = n))
                plot <- plot + ggplot2::geom_col(fill = cohort_color, color = "black", width = 0.7)
                plot <- plot + ggplot2::coord_flip()
                plot <- plot + ggplot2::labs(
                    x = .("Age Group"),
                    y = .("Count"),
                    title = plot_title
                )
                plot <- plot + ggplot2::theme_minimal()
                plot <- plot + ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                    axis.text = ggplot2::element_text(size = 10),
                    axis.title = ggplot2::element_text(size = 12)
                )

            } else {
                # Binary-gender pyramid
                if (is.null(plot_title) || plot_title == "") {
                    plot_title <- .("Age Pyramid")
                }

                bar_colors <- c("Female" = female_color, "Male" = male_color)

                plot <- ggplot2::ggplot(plotData,
                    ggplot2::aes(
                        x = AgeGroup,
                        y = ifelse(Gender2 == "Female", -n, n),
                        fill = Gender2
                    ))
                plot <- plot + ggplot2::geom_col(width = 0.7, color = "black")
                plot <- plot + ggplot2::coord_flip()
                plot <- plot + ggplot2::scale_y_continuous(
                    labels = abs,
                    limits = c(-max(plotData$n), max(plotData$n))
                )
                plot <- plot + ggplot2::scale_fill_manual(values = bar_colors, name = .("Gender"))
                plot <- plot + ggplot2::labs(
                    x = .("Age Group"),
                    y = .("Population Count"),
                    title = plot_title
                )
                plot <- plot + ggplot2::theme_minimal()
                plot <- plot + ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                    axis.text = ggplot2::element_text(size = 10),
                    axis.title = ggplot2::element_text(size = 12),
                    legend.position = "bottom"
                )
            }

            # Apply jamovi theme
            plot <- plot + ggtheme

            # Render
            print(plot)
            return(TRUE)
        },

        .create_age_labels = function(breaks) {
            if (length(breaks) < 2) return(c())
            labels <- c()
            for (i in seq_len(length(breaks) - 1)) {
                lower <- breaks[i]
                upper <- breaks[i + 1]
                if (is.infinite(upper)) {
                    labels[i] <- paste0(lower + 1, "+")
                } else {
                    labels[i] <- paste(lower + 1, upper, sep = "-")
                }
            }
            return(labels)
        },

        .build_data_info_html = function(n_initial, n_final, is_single_gender,
                                        female_level, male_level, single_gender_label) {
            n_excluded <- n_initial - n_final

            html <- "<div style='background-color: #e3f2fd; padding: 15px; border-radius: 8px; border-left: 4px solid #2196F3;'>"
            html <- paste0(html, "<h4 style='margin: 0 0 8px 0; color: #1976d2;'>📊 ", .("Data Summary"), "</h4>")
            html <- paste0(html, "<table style='width: 100%; font-size: 14px;'>")
            html <- paste0(html, "<tr><td><strong>", .("Initial observations:"), "</strong></td><td>", n_initial, "</td></tr>")
            html <- paste0(html, "<tr><td><strong>", .("Final observations:"), "</strong></td><td>", n_final, "</td></tr>")

            if (n_excluded > 0) {
                pct_excluded <- round(n_excluded / n_initial * 100, 1)
                html <- paste0(html, "<tr><td><strong>", .("Excluded:"), "</strong></td><td style='color: #d32f2f;'>",
                    n_excluded, " (", pct_excluded, "%)</td></tr>")
            }

            if (is_single_gender) {
                html <- paste0(html, "<tr><td><strong>", .("Cohort type:"), "</strong></td><td style='color: #f57c00;'>",
                    .("Single-gender"), " (", single_gender_label, ")</td></tr>")
            } else {
                html <- paste0(html, "<tr><td><strong>", .("Female level:"), "</strong></td><td>", female_level, "</td></tr>")
                html <- paste0(html, "<tr><td><strong>", .("Male level:"), "</strong></td><td>", male_level, "</td></tr>")
            }

            html <- paste0(html, "</table></div>")
            return(html)
        }
    )
)
