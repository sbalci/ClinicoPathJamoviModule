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
        .run = function() {
            # Check if required options (age and gender) are provided
            if (is.null(self$options$age) || is.null(self$options$gender))
                return()

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
                warning(.(("Some age values could not be converted to numeric and will be excluded")))
            }
            mydata[["Age"]] <- age_numeric
            mydata[["Gender"]] <- as.factor(mydata[[gender]])

            # Create a standardized gender variable based on the selected 'female' level
            female_level <- self$options$female
            if (is.null(female_level)) {
                # Use first level as female by default
                female_level <- levels(mydata[["Gender"]])[1]
            }
            
            mydata <- mydata %>%
                dplyr::mutate(
                    Gender2 = dplyr::case_when(
                        Gender == female_level ~ "Female",
                        TRUE ~ "Male"
                    )
                )

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

            # Pivot data for table output ----
            plotData2 <- plotData %>%
                tidyr::pivot_wider(names_from = Gender,
                                   values_from = n,
                                   values_fill = list(n = 0)) %>%  # Fill missing counts with 0
                dplyr::arrange(dplyr::desc(Pop))

            plotData2 <- as.data.frame(plotData2) %>%
                dplyr::filter(!is.na(Pop)) %>%
                dplyr::mutate(Pop = as.character(Pop))
            
            # Calculate population statistics for enhanced table output ----
            total_female <- sum(plotData2$Female, na.rm = TRUE)
            total_male <- sum(plotData2$Male, na.rm = TRUE) 
            total_population <- total_female + total_male
            
            # Add percentage columns
            plotData2$Female_Pct <- round(plotData2$Female / total_female * 100, 1)
            plotData2$Male_Pct <- round(plotData2$Male / total_male * 100, 1)
            
            # Add summary statistics row
            summary_row <- data.frame(
                Pop = .(("Total")),
                Female = total_female,
                Male = total_male,
                Female_Pct = 100.0,
                Male_Pct = 100.0,
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

            # Set plot title (using user option if provided)
            plot_title <- self$options$plot_title %||% .("Age Pyramid")
            
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
        }
    )
)
