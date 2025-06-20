#' @title Age Pyramid
#' @description Generates an age pyramid plot from the provided data using ggcharts::pyramid_chart.
#' The function allows customization of bin width (age group granularity), plot title, and bar colors.
#' It creates a visually appealing plot showing the distribution of age by gender.
#' @importFrom R6 R6Class
#' @import jmvcore
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
                stop("Data contains no (complete) rows")

            # Read and prepare data ----
            mydata <- self$data

            age <- self$options$age
            gender <- self$options$gender

            # Select and clean the required columns
            mydata <- jmvcore::select(mydata, c(age, gender))
            mydata <- jmvcore::naOmit(mydata)

            # Convert age to numeric and gender to factor
            mydata[["Age"]] <- jmvcore::toNumeric(mydata[[age]])
            mydata[["Gender"]] <- as.factor(mydata[[gender]])

            # Create a standardized gender variable based on the selected 'female' level
            mydata <- mydata %>%
                dplyr::mutate(
                    Gender2 = dplyr::case_when(
                        Gender == self$options$female ~ "Female",
                        TRUE ~ "Male"
                    )
                )

            # Determine bin width (default to 5 years if not provided) ----
            bin_width <- if (!is.null(self$options$bin_width)) self$options$bin_width else 5

            max_age <- max(mydata[["Age"]], na.rm = TRUE)
            # Create age bins using the custom bin width
            breaks_seq <- seq(from = 0, to = max_age, by = bin_width)
            if (max_age > tail(breaks_seq, n = 1)) {
                breaks_seq <- c(breaks_seq, max_age)
            }
            mydata[["Pop"]] <- cut(mydata[["Age"]],
                                   include.lowest = TRUE,
                                   right = TRUE,
                                   breaks = breaks_seq,
                                   ordered_result = TRUE)

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
                tibble::rownames_to_column(var = "row") %>%
                dplyr::filter(!is.na(Pop)) %>%
                dplyr::mutate(Pop = as.character(Pop))

            # Populate the results table ----
            pyramidTable <- self$results$pyramidTable
            for(i in seq_len(nrow(plotData2))) {
                pyramidTable$addRow(rowKey = i, values = plotData2[i,])
            }
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Check if required options (age and gender) are provided
            if (is.null(self$options$age) || is.null(self$options$gender))
                return()

            if (nrow(self$data) == 0)
                stop("Data contains no (complete) rows")

            # Retrieve the prepared plot data
            plotData <- image$state

            # Ensure that the age bins (Pop) reflect the latest bin width:
            # Convert 'Pop' to character then back to factor with the order of appearance.
            plotData$Pop <- factor(as.character(plotData$Pop), levels = unique(as.character(plotData$Pop)))

            # Set plot title (using user option if provided)
            plot_title <- if (!is.null(self$options$plot_title)) self$options$plot_title else "Age Pyramid"
            
            # Get bar colors from options
            bar_colors <- c(
                self$options$color1 %||% "#1F77B4",
                self$options$color2 %||% "#FF7F0E"
            )

            # Create age pyramid plot using ggcharts ----
            plot <- ggcharts::pyramid_chart(
                data = plotData,
                x = Pop,
                y = n,
                group = Gender,
                bar_colors = bar_colors,
                title = plot_title,
                xlab = "Population Count"
            )

            # Apply any additional theme modifications passed via ggtheme
            plot <- plot + ggtheme

            print(plot)
            return(TRUE)
        }
    )
)
