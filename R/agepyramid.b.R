#' @title Age Pyramid
#' @description Generates an age pyramid plot from the provided data.
#' The function allows customization of bin width (age group granularity), plot title, 
#' colors, and plotting engine. It creates a visually appealing plot showing the 
#' distribution of age by gender using either ggplot2 or ggcharts.
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import ggcharts
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
        # Store data quality messages for HTML output
        .data_info_messages = list(),

        .run = function() {
            # Initialize messages
            private$.data_info_messages <- list()

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
                        <li><strong>Standard age pyramids</strong> for binary gender data</li>
                        <li><strong>Single-gender cohort bar charts</strong> for gender-specific studies</li>
                        <li><strong>Customizable age bins</strong> and color palettes</li>
                        <li><strong>Clinical age group presets</strong> (pediatric, geriatric, reproductive, life course)</li>
                        <li><strong>Data quality reporting</strong> with exclusion counts and transparency</li>
                        <li><strong>Two plot engines</strong>: ggcharts (polished pyramids) or ggplot2 (customizable)</li>
                    </ul>
                    <p style='font-size: 13px; color: #666; margin-bottom: 0; font-style: italic;'>
                    Select your Age and Gender variables in the left panel to begin.
                    </p>
                    </div>"
                )
                return()
            }

            if (nrow(self$data) == 0)
                stop(.("Data contains no (complete) rows"))

            # Performance monitoring for large datasets ----
            # Expected processing times (approximate, hardware-dependent):
            # < 1K rows: < 1 second
            # 1K-10K rows: 1-5 seconds  
            # 10K-100K rows: 5-30 seconds
            # > 100K rows: 30+ seconds (checkpoint system activates)
            dataset_size <- nrow(self$data)
            if (dataset_size > 50000) {
                # Large dataset detected - inform user and enable frequent checkpoints
                message(sprintf(.("Processing large dataset (%d rows) - this may take a moment..."), dataset_size))
            }

            # Read and prepare data ----
            mydata <- self$data

            age <- self$options$age
            gender <- self$options$gender

            # Checkpoint before data preparation (can be expensive for large datasets)
            private$.checkpoint()

            # Select and clean the required columns
            mydata <- jmvcore::select(mydata, c(age, gender))
            mydata <- jmvcore::naOmit(mydata)

            # Convert age to numeric and gender to factor with validation
            age_numeric <- jmvcore::toNumeric(mydata[[age]])
            if (any(is.na(age_numeric)) && !all(is.na(mydata[[age]]))) {
                private$.data_info_messages$age_conversion <- list(
                    type = "warning",
                    message = .("Some age values could not be converted to numeric and will be excluded")
                )
            }
            mydata[["Age"]] <- age_numeric
            mydata[["Gender"]] <- as.factor(mydata[[gender]])

            # Create a standardized gender variable based on selected levels
            female_level <- self$options$female
            male_level <- self$options$male

            # Get all unique gender levels
            gender_levels <- levels(mydata[["Gender"]])
            n_levels <- length(gender_levels)

            # Smart defaults if user hasn't selected levels
            if (is.null(female_level) && is.null(male_level)) {
                # Neither selected - use first two levels
                if (n_levels >= 2) {
                    female_level <- gender_levels[1]
                    male_level <- gender_levels[2]
                } else if (n_levels == 1) {
                    # Single gender cohort detected
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
                stop(.("Female and Male levels cannot be the same. Please select different gender categories."))
            }

            # Check for single-gender cohort
            is_single_gender <- is.null(male_level) || is.null(female_level)
            single_gender_label <- if(!is.null(female_level)) female_level else male_level

            # Store info about multiple gender levels if more than 2
            if (n_levels > 2 && !is_single_gender) {
                other_levels <- gender_levels[!gender_levels %in% c(female_level, male_level)]
                if (length(other_levels) > 0) {
                    private$.data_info_messages$multi_gender <- list(
                        type = "info",
                        message = sprintf(
                            .("Gender variable has %d levels. Only '%s' (Female) and '%s' (Male) will be used. Other levels (%s) will be excluded."),
                            n_levels,
                            female_level,
                            male_level,
                            paste(other_levels, collapse = ", ")
                        )
                    )
                }
            }

            # Create standardized gender variable with explicit handling
            if (is_single_gender) {
                # Single gender cohort - all matching records get one label
                mydata <- mydata %>%
                    dplyr::mutate(
                        Gender2 = dplyr::case_when(
                            is.na(Gender) ~ NA_character_,
                            Gender == single_gender_label ~ "Single Gender Cohort",
                            TRUE ~ NA_character_  # Exclude other levels
                        )
                    )
            } else {
                # Standard binary gender pyramid
                mydata <- mydata %>%
                    dplyr::mutate(
                        Gender2 = dplyr::case_when(
                            is.na(Gender) ~ NA_character_,  # Explicitly handle missing values
                            Gender == female_level ~ "Female",
                            Gender == male_level ~ "Male",
                            TRUE ~ NA_character_  # All other values become NA (will be filtered out)
                        )
                    )
            }

            # Filter out rows with missing or unrecognized gender values
            n_before <- nrow(mydata)
            n_initial <- nrow(self$data)
            mydata <- mydata %>% dplyr::filter(!is.na(Gender2))
            n_after <- nrow(mydata)

            # Store exclusion info
            if (n_after < n_before) {
                n_excluded <- n_before - n_after
                private$.data_info_messages$excluded <- list(
                    type = "info",
                    message = sprintf(
                        .("Excluded %d observations with missing or unrecognized gender values (%d remaining)"),
                        n_excluded,
                        n_after
                    )
                )
            }

            # Store single-gender cohort info
            if (is_single_gender) {
                private$.data_info_messages$single_gender <- list(
                    type = "note",
                    message = sprintf(
                        .("Single-gender cohort detected: %s (N = %d). Displaying age distribution bar chart instead of pyramid."),
                        single_gender_label,
                        n_after
                    )
                )
            }

            # Store data summary
            private$.data_info_messages$summary <- list(
                type = "summary",
                n_initial = n_initial,
                n_final = n_after,
                n_excluded_total = n_initial - n_after,
                female_level = female_level,
                male_level = male_level,
                is_single_gender = is_single_gender
            )

            if (n_after == 0) {
                self$results$dataInfo$setContent(
                    "<div style='background-color: #ffebee; padding: 15px; border-radius: 8px; border-left: 4px solid #f44336;'>
                    <h4 style='margin: 0 0 10px 0; color: #c62828;'>❌ No Valid Data</h4>
                    <p style='margin: 0 0 10px 0; font-size: 14px;'>No valid observations remaining after data filtering.</p>
                    <h5 style='margin: 10px 0 5px 0; color: #c62828;'>Possible causes:</h5>
                    <ul style='font-size: 14px; line-height: 1.6; margin: 0 0 10px 0;'>
                        <li>All gender values are missing (NA)</li>
                        <li>Selected female/male levels don't exist in your data</li>
                        <li>All age values are non-numeric or missing</li>
                        <li>No overlap between selected levels and actual data</li>
                    </ul>
                    <h5 style='margin: 10px 0 5px 0; color: #c62828;'>Please check:</h5>
                    <ul style='font-size: 14px; line-height: 1.6; margin: 0;'>
                        <li>Variable selections match your dataset</li>
                        <li>Gender levels exist: <strong>" + paste(gender_levels, collapse = ", ") + "</strong></li>
                        <li>Selected Female level: <strong>" + (if(!is.null(female_level)) female_level else "None") + "</strong></li>
                        <li>Selected Male level: <strong>" + (if(!is.null(male_level)) male_level else "None") + "</strong></li>
                        <li>Data quality and missing value patterns</li>
                    </ul>
                    </div>"
                )
                return()
            }

            # Store single-gender status for plot rendering
            private$.is_single_gender <- is_single_gender
            private$.single_gender_label <- single_gender_label

            # Determine age binning strategy ----
            age_groups_preset <- self$options$age_groups %||% "custom"
            
            if (age_groups_preset == "custom") {
                # Use custom bin width (default to 5 years if not provided)
                bin_width <- self$options$bin_width %||% 5
                max_age <- max(mydata[["Age"]], na.rm = TRUE)
                breaks_seq <- seq(from = 0, to = max_age, by = bin_width)
                if (max_age > tail(breaks_seq, n = 1)) {
                    breaks_seq <- c(breaks_seq, max_age)
                }
            } else {
                # Use preset clinical age groups
                breaks_seq <- switch(age_groups_preset,
                    "pediatric" = c(0, 1, 2, 5, 10, 15, 18, Inf),
                    "reproductive" = c(0, 15, 20, 25, 30, 35, 40, 45, 50, Inf), 
                    "geriatric" = c(0, 65, 70, 75, 80, 85, 90, 95, Inf),
                    "lifecourse" = c(0, 5, 15, 25, 45, 65, 75, 85, Inf),
                    # Fallback to custom if unknown
                    {
                        bin_width <- self$options$bin_width %||% 5
                        max_age <- max(mydata[["Age"]], na.rm = TRUE)
                        c(seq(from = 0, to = max_age, by = bin_width), 
                          if (max_age > max(seq(from = 0, to = max_age, by = bin_width))) max_age)
                    }
                )
            }
            mydata[["Pop"]] <- cut(mydata[["Age"]],
                                   include.lowest = TRUE,
                                   right = TRUE,
                                   breaks = breaks_seq,
                                   ordered_result = TRUE)

            # Checkpoint before data aggregation (expensive operation)
            private$.checkpoint()

            # Prepare data for plotting and table output ----
            plotData <- mydata %>%
                dplyr::select(Gender = Gender2, Pop) %>%
                dplyr::group_by(Gender, Pop) %>%
                dplyr::count() %>%
                dplyr::ungroup()

            # Save state for plot rendering; ensures plot gets updated when bin_width changes
            image <- self$results$plot
            image$setState(plotData)

            # Generate and populate data quality HTML ----
            info_html <- private$.build_data_info_html()
            self$results$dataInfo$setContent(info_html)

            # Pivot data for table output ----
            plotData2 <- plotData %>%
                tidyr::pivot_wider(names_from = Gender,
                                   values_from = n,
                                   values_fill = list(n = 0)) %>%  # Fill missing counts with 0
                dplyr::arrange(dplyr::desc(Pop))

            plotData2 <- as.data.frame(plotData2) %>%
                dplyr::filter(!is.na(Pop)) %>%
                dplyr::mutate(Pop = as.character(Pop))

            # Handle single-gender cohort column schema ----
            # For single-gender cohorts, ensure table columns match expected Female/Male schema
            if (is_single_gender) {
                # Rename single-gender column for schema compatibility
                if ("Single Gender Cohort" %in% colnames(plotData2)) {
                    # Determine if this is female-only or male-only cohort
                    if (!is.null(female_level) && is.null(male_level)) {
                        # Female-only cohort
                        colnames(plotData2)[colnames(plotData2) == "Single Gender Cohort"] <- "Female"
                        plotData2$Male <- 0
                    } else if (!is.null(male_level) && is.null(female_level)) {
                        # Male-only cohort
                        colnames(plotData2)[colnames(plotData2) == "Single Gender Cohort"] <- "Male"
                        plotData2$Female <- 0
                    } else {
                        # Fallback: treat as female
                        colnames(plotData2)[colnames(plotData2) == "Single Gender Cohort"] <- "Female"
                        plotData2$Male <- 0
                    }
                }
                # Ensure both columns exist
                if (!"Female" %in% colnames(plotData2)) {
                    plotData2$Female <- 0
                }
                if (!"Male" %in% colnames(plotData2)) {
                    plotData2$Male <- 0
                }
            }

            # Calculate population statistics for enhanced table output ----
            total_female <- sum(plotData2$Female, na.rm = TRUE)
            total_male <- sum(plotData2$Male, na.rm = TRUE) 
            total_population <- total_female + total_male
            
            # Add percentage columns with safe division
            plotData2$Female_Pct <- ifelse(total_female > 0,
                round(plotData2$Female / total_female * 100, 1),
                0)
            plotData2$Male_Pct <- ifelse(total_male > 0,
                round(plotData2$Male / total_male * 100, 1),
                0)

            # Add summary statistics row with safe percentages
            summary_row <- data.frame(
                Pop = .(("Total")),
                Female = total_female,
                Male = total_male,
                Female_Pct = ifelse(total_female > 0, 100.0, 0),
                Male_Pct = ifelse(total_male > 0, 100.0, 0),
                stringsAsFactors = FALSE
            )
            
            plotData2 <- rbind(plotData2, summary_row)

            # Populate the results table ----
            pyramidTable <- self$results$pyramidTable
            
            # Checkpoint before table population (loop operation)
            private$.checkpoint()
            
            for(i in seq_len(nrow(plotData2))) {
                pyramidTable$addRow(rowKey = i, values = plotData2[i,])
                
                # For larger tables, checkpoint periodically to show incremental results
                if (nrow(plotData2) > 20 && i %% 10 == 0) {
                    private$.checkpoint(flush = FALSE)  # Poll for changes without flushing
                }
            }
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Check if required options (age and gender) are provided
            if (is.null(self$options$age) || is.null(self$options$gender))
                return()

            if (nrow(self$data) == 0)
                stop(.("Data contains no (complete) rows"))

            # Retrieve the prepared plot data
            plotData <- image$state

            # Ensure that the age bins (Pop) reflect the latest bin width:
            # Convert 'Pop' to character then back to factor with the order of appearance.
            plotData$Pop <- factor(as.character(plotData$Pop), levels = unique(as.character(plotData$Pop)))

            # Check if this is a single-gender cohort
            is_single_gender <- private$.is_single_gender %||% FALSE
            single_gender_label <- private$.single_gender_label %||% "Unknown"

            # Set plot title (using user option if provided)
            if (is_single_gender) {
                plot_title <- self$options$plot_title %||% sprintf(.("Age Distribution - Single Gender Cohort (%s)"), single_gender_label)
            } else {
                plot_title <- self$options$plot_title %||% .("Age Pyramid")
            }
            
            # Get plot engine choice
            plot_engine <- self$options$plot_engine %||% "ggcharts"
            
            # Get colors based on palette selection ----
            color_palette <- self$options$color_palette %||% "standard"
            
            if (color_palette == "standard") {
                if (plot_engine == "ggcharts") {
                    female_color <- "#FF69B4"  # Hot pink for Female
                    male_color <- "#4169E1"    # Royal blue for Male
                } else {
                    female_color <- "#F8766D"  # ggplot2 red/pink for Female
                    male_color <- "#00BFC4"    # ggplot2 teal for Male
                }
            } else if (color_palette == "accessible") {
                # Colorblind-friendly Okabe-Ito palette
                female_color <- "#E69F00"  # Orange for Female  
                male_color <- "#56B4E9"    # Sky blue for Male
            } else {
                # Custom colors from user options
                female_color <- self$options$color1 %||% "#FF69B4"
                male_color <- self$options$color2 %||% "#4169E1"
            }
            
            # Create named vector for consistent color mapping
            bar_colors <- c("Female" = female_color, "Male" = male_color)

            # Checkpoint before plot generation (potentially expensive rendering)
            private$.checkpoint()

            # Handle single-gender cohort with simple bar chart ----
            if (is_single_gender) {
                # Create a simple horizontal bar chart for single-gender cohorts
                cohort_color <- if(!is.null(self$options$color1)) {
                    self$options$color1
                } else if(color_palette == "accessible") {
                    "#E69F00"  # Orange
                } else {
                    "#4169E1"  # Royal blue
                }

                plot <- ggplot2::ggplot(data = plotData,
                                        mapping = ggplot2::aes(x = Pop, y = n)) +
                    ggplot2::geom_col(fill = cohort_color, color = "black", width = 0.7) +
                    ggplot2::coord_flip() +
                    ggplot2::labs(
                        x = .("Age Group"),
                        y = .("Count"),
                        title = plot_title,
                        subtitle = sprintf(.("Single gender cohort: %s (N = %d)"),
                                         single_gender_label,
                                         sum(plotData$n, na.rm = TRUE)),
                        caption = .("Note: Age pyramid visualization requires data from both genders.\nShowing simple age distribution for single-gender cohort.")
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "#666666"),
                        plot.caption = ggplot2::element_text(hjust = 0, face = "italic", color = "#888888", size = 9),
                        axis.text = ggplot2::element_text(size = 10),
                        axis.title = ggplot2::element_text(size = 12)
                    )

                # Apply any additional theme modifications
                plot <- plot + ggtheme
                print(plot)
                return(TRUE)
            }

            # Create age pyramid plot based on selected engine ----
            if (plot_engine == "ggcharts") {
                # Check if ggcharts is available
                if (!requireNamespace("ggcharts", quietly = TRUE)) {
                    # Fall back to ggplot2 if ggcharts not available
                    plot_engine <- "ggplot2"
                    warning(.("ggcharts package not available, falling back to ggplot2"))
                } else {
                    # Create age pyramid plot using ggcharts ----
                    # ggcharts expects colors in the order they appear in data
                    chart_colors <- c(female_color, male_color)
                    plot <- ggcharts::pyramid_chart(
                        data = plotData,
                        x = Pop,
                        y = n,
                        group = Gender,
                        bar_colors = chart_colors,
                        title = plot_title,
                        xlab = .("Population Count")
                    )
                }
            }
            
            if (plot_engine == "ggplot2") {
                # Create a visually appealing age pyramid plot using ggplot2 ----
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
                    ggplot2::scale_fill_manual(values = bar_colors, name = .("Gender")) +  # Use named colors
                    ggplot2::labs(x = .("Age Group"),
                                  y = .("Population Count"),
                                  title = plot_title,
                                  fill = .("Gender")) +
                    ggplot2::theme_minimal() +  # Clean minimal theme for improved visuals
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        axis.text = ggplot2::element_text(size = 10),
                        axis.title = ggplot2::element_text(size = 12),
                        legend.position = "bottom"
                    )
            }

            # Apply any additional theme modifications passed via ggtheme
            plot <- plot + ggtheme

            print(plot)
            return(TRUE)
        },

        .build_data_info_html = function() {
            msgs <- private$.data_info_messages

            if (length(msgs) == 0) {
                return("")
            }

            # Start HTML with main container
            html <- "<div style='font-family: Arial, sans-serif; padding: 15px; background-color: #f8f9fa; border-radius: 8px; margin-bottom: 15px;'>"

            # Data Summary Section (always show if available)
            if (!is.null(msgs$summary)) {
                s <- msgs$summary
                html <- paste0(html,
                    "<div style='background-color: #e3f2fd; padding: 12px; border-radius: 6px; margin-bottom: 12px; border-left: 4px solid #2196F3;'>",
                    "<h4 style='margin: 0 0 8px 0; color: #1976d2;'>📊 ", .("Data Summary"), "</h4>",
                    "<table style='width: 100%; font-size: 14px;'>",
                    "<tr><td style='padding: 4px;'><strong>", .("Initial observations:"), "</strong></td><td style='padding: 4px;'>", s$n_initial, "</td></tr>",
                    "<tr><td style='padding: 4px;'><strong>", .("Final observations:"), "</strong></td><td style='padding: 4px;'>", s$n_final, "</td></tr>"
                )

                if (s$n_excluded_total > 0) {
                    html <- paste0(html,
                        "<tr><td style='padding: 4px;'><strong>", .("Total excluded:"), "</strong></td><td style='padding: 4px; color: #d32f2f;'>",
                        s$n_excluded_total, " (", round(s$n_excluded_total / s$n_initial * 100, 1), "%)</td></tr>"
                    )
                }

                if (!s$is_single_gender) {
                    html <- paste0(html,
                        "<tr><td style='padding: 4px;'><strong>", .("Female level:"), "</strong></td><td style='padding: 4px;'>", s$female_level, "</td></tr>",
                        "<tr><td style='padding: 4px;'><strong>", .("Male level:"), "</strong></td><td style='padding: 4px;'>", s$male_level, "</td></tr>"
                    )
                } else {
                    html <- paste0(html,
                        "<tr><td style='padding: 4px;'><strong>", .("Cohort type:"), "</strong></td><td style='padding: 4px; color: #f57c00;'>",
                        .("Single-gender"), "</td></tr>"
                    )
                }

                html <- paste0(html, "</table></div>")
            }

            # Single-Gender Note (prominent)
            if (!is.null(msgs$single_gender)) {
                html <- paste0(html,
                    "<div style='background-color: #fff3e0; padding: 12px; border-radius: 6px; margin-bottom: 12px; border-left: 4px solid #FF9800;'>",
                    "<h4 style='margin: 0 0 6px 0; color: #f57c00;'>ℹ️ ", .("Single-Gender Cohort"), "</h4>",
                    "<p style='margin: 0; font-size: 14px;'>", msgs$single_gender$message, "</p>",
                    "</div>"
                )
            }

            # Data Quality Messages
            if (!is.null(msgs$multi_gender)) {
                html <- paste0(html,
                    "<div style='background-color: #e8f5e9; padding: 12px; border-radius: 6px; margin-bottom: 12px; border-left: 4px solid #4CAF50;'>",
                    "<h4 style='margin: 0 0 6px 0; color: #388e3c;'>ℹ️ ", .("Multiple Gender Levels"), "</h4>",
                    "<p style='margin: 0; font-size: 14px;'>", msgs$multi_gender$message, "</p>",
                    "</div>"
                )
            }

            if (!is.null(msgs$excluded)) {
                html <- paste0(html,
                    "<div style='background-color: #fff9c4; padding: 12px; border-radius: 6px; margin-bottom: 12px; border-left: 4px solid #FFC107;'>",
                    "<h4 style='margin: 0 0 6px 0; color: #f57f17;'>⚠️ ", .("Data Exclusions"), "</h4>",
                    "<p style='margin: 0; font-size: 14px;'>", msgs$excluded$message, "</p>",
                    "</div>"
                )
            }

            if (!is.null(msgs$age_conversion)) {
                html <- paste0(html,
                    "<div style='background-color: #ffebee; padding: 12px; border-radius: 6px; margin-bottom: 12px; border-left: 4px solid #f44336;'>",
                    "<h4 style='margin: 0 0 6px 0; color: #c62828;'>⚠️ ", .("Age Conversion"), "</h4>",
                    "<p style='margin: 0; font-size: 14px;'>", msgs$age_conversion$message, "</p>",
                    "</div>"
                )
            }

            # Close main container
            html <- paste0(html, "</div>")

            return(html)
        }
    )
)
