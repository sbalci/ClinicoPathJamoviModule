#' @title Group and Summarize
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import dplyr
#' @import ggplot2
#' @import lubridate
#'

groupsummaryClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "groupsummaryClass",
    inherit = groupsummaryBase,
    private = list(
        .run = function() {

            # Welcome message when no variables selected ----
            if (length(self$options$groupVars) == 0 || length(self$options$sumVars) == 0) {
                todo <- "
                <br>Welcome to ClinicoPath Group and Summarize
                <br><br>
                This tool helps you group your data by categorical or date variables and calculate statistics.
                <br><br>
                <b>Instructions:</b><br>
                1. Select one or more variables to group by (categorical or date)<br>
                2. Select numeric variables to summarize<br>
                3. Choose which statistics to calculate (sum, mean, median, count)<br>
                4. If grouping by date:<br>
                   - Select the date variable<br>
                   - Choose the date format<br>
                   - Select time aggregation level<br>
                <br>
                The tool will create a summary table and visualization of your grouped data.
                <hr><br>
                "
                html <- self$results$todo
                html$setContent(todo)
                return()
            } else {
                # Clear todo message when analysis runs
                todo <- ""
                html <- self$results$todo
                html$setContent(todo)
            }

            # Check for data ----
            if (nrow(self$data) == 0)
                stop("Data contains no (complete) rows")

            # Get data and variables ----
            mydata <- self$data
            groupVars <- self$options$groupVars
            sumVars <- self$options$sumVars
            statistics <- self$options$statistics

            # Ensure groupVars is a character vector
            if (!is.null(groupVars)) {
                if (is.character(groupVars)) {
                    # Already character, nothing to do
                } else if (is.list(groupVars)) {
                    # Convert list to character vector
                    groupVars <- unlist(groupVars)
                } else {
                    # Try to convert to character
                    groupVars <- as.character(groupVars)
                }
            }

            # Ensure sumVars is a character vector
            if (!is.null(sumVars)) {
                if (is.character(sumVars)) {
                    # Already character, nothing to do
                } else if (is.list(sumVars)) {
                    # Convert list to character vector
                    sumVars <- unlist(sumVars)
                } else {
                    # Try to convert to character
                    sumVars <- as.character(sumVars)
                }
            }

            # Ensure at least one statistic is selected
            if (is.null(statistics) || length(statistics) == 0) {
                statistics <- c("sum", "n")  # Default statistics
            }

            # Handle date variable if specified ----
            dateVar <- self$options$dateVar
            if (!is.null(dateVar) && dateVar %in% groupVars) {

                # Show date processing info
                dateInfo <- glue::glue("
                    <br><b>Date Processing:</b><br>
                    Variable: {dateVar}<br>
                    Format: {self$options$dateFormat}<br>
                    Aggregation: {self$options$timeAggregation}<br>
                ")
                self$results$dateInfo$setContent(dateInfo)

                # Parse date based on selected format
                date_parser <- switch(self$options$dateFormat,
                                      "ymd" = lubridate::ymd,
                                      "dmy" = lubridate::dmy,
                                      "mdy" = lubridate::mdy,
                                      "ymd_hms" = lubridate::ymd_hms,
                                      "dmy_hms" = lubridate::dmy_hms,
                                      "mdy_hms" = lubridate::mdy_hms,
                                      lubridate::ymd  # default
                )

                # Try to parse the date
                tryCatch({
                    mydata[[dateVar]] <- date_parser(as.character(mydata[[dateVar]]))

                    # Create time aggregation variable
                    time_agg_var <- paste0(dateVar, "_", self$options$timeAggregation)

                    mydata[[time_agg_var]] <- switch(self$options$timeAggregation,
                                                     "hour" = lubridate::floor_date(mydata[[dateVar]], "hour"),
                                                     "day" = lubridate::floor_date(mydata[[dateVar]], "day"),
                                                     "week" = lubridate::floor_date(mydata[[dateVar]], "week"),
                                                     "month" = lubridate::floor_date(mydata[[dateVar]], "month"),
                                                     "year" = lubridate::floor_date(mydata[[dateVar]], "year"),
                                                     mydata[[dateVar]]  # default to original
                    )

                    # Replace the original date variable with aggregated one in groupVars
                    groupVars[groupVars == dateVar] <- time_agg_var

                }, error = function(e) {
                    stop(paste("Error parsing date variable:", e$message))
                })
            }

            # Handle missing values based on option ----
            if (!self$options$showMissing) {
                # Remove rows with NA in grouping variables
                mydata <- mydata %>%
                    dplyr::filter(dplyr::if_all(dplyr::all_of(groupVars), ~ !is.na(.)))
            }

            # Create list of summary functions based on selected statistics ----
            summary_funs <- list()
            if ("sum" %in% statistics) {
                summary_funs$sum <- ~ sum(., na.rm = TRUE)
            }
            if ("mean" %in% statistics) {
                summary_funs$mean <- ~ round(mean(., na.rm = TRUE), 2)
            }
            if ("median" %in% statistics) {
                summary_funs$median <- ~ round(median(., na.rm = TRUE), 2)
            }
            if ("n" %in% statistics) {
                summary_funs$n <- ~ sum(!is.na(.))
            }

            # Perform group by and summarize ----
            summary_data <- mydata %>%
                dplyr::group_by(dplyr::across(dplyr::all_of(groupVars))) %>%
                dplyr::summarise(
                    dplyr::across(
                        dplyr::all_of(sumVars),
                        summary_funs,
                        .names = "{.col}_{.fn}"
                    ),
                    .groups = 'drop'
                )

            # Calculate percentages if requested and sum is included ----
            if (self$options$addPercentage && "sum" %in% statistics) {
                for (var in sumVars) {
                    if (paste0(var, "_sum") %in% names(summary_data)) {
                        total_sum <- sum(summary_data[[paste0(var, "_sum")]], na.rm = TRUE)
                        if (total_sum > 0) {
                            summary_data[[paste0(var, "_pct")]] <-
                                round(summary_data[[paste0(var, "_sum")]] / total_sum * 100, 2)
                            }
                        }
                    }
                }

            # Sort data based on option ----
            if (length(sumVars) > 0) {
                # Find first statistic column for first sumVar
                first_stat_col <- NULL
                for (stat in c("sum", "mean", "median", "n")) {
                    col_name <- paste0(sumVars[1], "_", stat)
                    if (col_name %in% names(summary_data)) {
                        first_stat_col <- col_name
                        break
                    }
                }

                if (!is.null(first_stat_col)) {
                    if (self$options$sortBy == "first_desc") {
                        summary_data <- summary_data %>%
                            dplyr::arrange(dplyr::desc(.[[first_stat_col]]))
                    } else if (self$options$sortBy == "first_asc") {
                        summary_data <- summary_data %>%
                            dplyr::arrange(.[[first_stat_col]])
                    }
                }
            }

            # Setup table ----
            table <- self$results$summaryTable

            # Clear existing rows instead of columns
            table$deleteRows()

            # Since we can't dynamically add columns in jamovi, we'll create a
            # formatted text output for the results
            # First, let's create column headers as a string
            headers <- c()

            # Add group variable headers
            for (var in groupVars) {
                col_title <- var
                if (!is.null(dateVar) && grepl(paste0("^", dateVar, "_"), var)) {
                    col_title <- gsub("_", " ", var)
                    col_title <- tools::toTitleCase(col_title)
                }
                headers <- c(headers, col_title)
            }

            # Add statistic columns for each variable
            for (var in sumVars) {
                if ("sum" %in% statistics) {
                    table$addColumn(
                        name = paste0(var, "_sum"),
                        title = paste0(var, " (Sum)"),
                        type = 'number',
                        format = 'zto'
                    )
                }

                if ("mean" %in% statistics) {
                    table$addColumn(
                        name = paste0(var, "_mean"),
                        title = paste0(var, " (Mean)"),
                        type = 'number'
                    )
                }

                if ("median" %in% statistics) {
                    table$addColumn(
                        name = paste0(var, "_median"),
                        title = paste0(var, " (Median)"),
                        type = 'number'
                    )
                }

                if ("n" %in% statistics) {
                    table$addColumn(
                        name = paste0(var, "_n"),
                        title = paste0(var, " (N)"),
                        type = 'integer'
                    )
                }

                if (self$options$addPercentage && "sum" %in% statistics &&
                    paste0(var, "_pct") %in% names(summary_data)) {
                    table$addColumn(
                        name = paste0(var, "_pct"),
                        title = paste0(var, " (%)"),
                        type = 'number',
                        format = 'pc'
                    )
                }
            }

            # Add rows to table ----
            for (i in seq_len(nrow(summary_data))) {
                # Format date values for display
                row_values <- as.list(summary_data[i, ])
                for (var in groupVars) {
                    if (!is.null(dateVar) && grepl(paste0("^", dateVar, "_"), var)) {
                        # Format date based on aggregation level
                        if (self$options$timeAggregation == "hour") {
                            row_values[[var]] <- format(row_values[[var]], "%Y-%m-%d %H:00")
                        } else if (self$options$timeAggregation == "day") {
                            row_values[[var]] <- format(row_values[[var]], "%Y-%m-%d")
                        } else if (self$options$timeAggregation == "week") {
                            row_values[[var]] <- format(row_values[[var]], "Week of %Y-%m-%d")
                        } else if (self$options$timeAggregation == "month") {
                            row_values[[var]] <- format(row_values[[var]], "%Y-%m")
                        } else if (self$options$timeAggregation == "year") {
                            row_values[[var]] <- format(row_values[[var]], "%Y")
                        }
                    }
                }
                table$addRow(rowKey = i, values = row_values)
            }

            # Add total row ----
            total_row <- list()
            total_row[[groupVars[1]]] <- "TOTAL"

            # Fill other group columns with empty strings
            if (length(groupVars) > 1) {
                for (j in 2:length(groupVars)) {
                    total_row[[groupVars[j]]] <- ""
                }
            }

            # Calculate totals for each statistic
            for (var in sumVars) {
                if ("sum" %in% statistics) {
                    total_row[[paste0(var, "_sum")]] <-
                        sum(summary_data[[paste0(var, "_sum")]], na.rm = TRUE)
                }
                if ("mean" %in% statistics) {
                    # Calculate overall mean from raw data
                    total_row[[paste0(var, "_mean")]] <-
                        round(mean(mydata[[var]], na.rm = TRUE), 2)
                }
                if ("median" %in% statistics) {
                    # Calculate overall median from raw data
                    total_row[[paste0(var, "_median")]] <-
                        round(median(mydata[[var]], na.rm = TRUE), 2)
                }
                if ("n" %in% statistics) {
                    total_row[[paste0(var, "_n")]] <-
                        sum(summary_data[[paste0(var, "_n")]], na.rm = TRUE)
                }
                if (self$options$addPercentage && "sum" %in% statistics) {
                    total_row[[paste0(var, "_pct")]] <- 100.00
                }
            }

            table$addRow(rowKey = nrow(summary_data) + 1, values = total_row)

            # Prepare data for plot ----
            plotData <- list(
                data = summary_data,
                groupVars = groupVars,
                sumVars = sumVars,
                statistics = statistics,
                dateVar = dateVar,
                timeAggregation = self$options$timeAggregation
            )

            image <- self$results$plot
            image$setState(plotData)
            },

        .plot = function(image, ggtheme, theme, ...) {

            # Get plot data ----
            plotData <- image$state

            if (is.null(plotData))
                return()

            summary_data <- plotData$data
            groupVars <- plotData$groupVars
            sumVars <- plotData$sumVars
            statistics <- plotData$statistics

            # Determine which statistic to plot (prefer sum, then mean, then median, then n)
            stat_to_plot <- NULL
            stat_label <- NULL

            for (stat in c("sum", "mean", "median", "n")) {
                if (stat %in% statistics) {
                    stat_to_plot <- stat
                    stat_label <- tools::toTitleCase(stat)
                    break
                }
            }

            # Check if we have valid data to plot
            if (is.null(stat_to_plot) || length(sumVars) == 0 || nrow(summary_data) == 0) {
                # Create an empty plot with message
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5,
                                      label = "No data to display",
                                      size = 6, hjust = 0.5, vjust = 0.5) +
                    ggplot2::theme_void()

                print(plot)
                return(TRUE)
            }

            # Create plot for first summary variable
            plot_var <- sumVars[1]
            plot_col <- paste0(plot_var, "_", stat_to_plot)

            # Create group labels
            if (length(groupVars) == 1) {
                summary_data$group_label <- as.character(summary_data[[groupVars[1]]])
            } else {
                summary_data$group_label <- apply(
                    summary_data[, groupVars],
                    1,
                    paste,
                    collapse = " - "
                )
            }

            # Check if we're plotting time series data
            is_time_series <- !is.null(plotData$dateVar) &&
                any(grepl(paste0("^", plotData$dateVar, "_"), groupVars))

            if (is_time_series && length(groupVars) == 1) {
                # Time series line plot
                plot <- ggplot2::ggplot(
                    summary_data,
                    ggplot2::aes(
                        x = .data[[groupVars[1]]],
                        y = .data[[plot_col]]
                    )
                ) +
                    ggplot2::geom_line(color = "#4373B6", size = 1.2) +
                    ggplot2::geom_point(color = "#4373B6", size = 3) +
                    ggplot2::labs(
                        x = tools::toTitleCase(gsub("_", " ", groupVars[1])),
                        y = paste(stat_label, "of", plot_var),
                        title = paste(stat_label, "of", plot_var, "over time")
                    ) +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

                # Add value labels
                plot <- plot +
                    ggplot2::geom_text(
                        ggplot2::aes(label = format(.data[[plot_col]],
                                                    big.mark = ",",
                                                    scientific = FALSE)),
                        vjust = -1,
                        size = 3
                    )

            } else {
                # Bar plot for categorical grouping
                plot <- ggplot2::ggplot(
                    summary_data,
                    ggplot2::aes(
                        x = stats::reorder(group_label, .data[[plot_col]]),
                        y = .data[[plot_col]]
                    )
                ) +
                    ggplot2::geom_bar(
                        stat = "identity",
                        fill = "#4373B6",
                        alpha = 0.8
                    ) +
                    ggplot2::coord_flip() +
                    ggplot2::labs(
                        x = paste(groupVars, collapse = " + "),
                        y = paste(stat_label, "of", plot_var),
                        title = paste(stat_label, "of", plot_var, "by", paste(groupVars, collapse = " and "))
                    )

                # Add value labels on bars
                plot <- plot +
                    ggplot2::geom_text(
                        ggplot2::aes(label = format(.data[[plot_col]],
                                                    big.mark = ",",
                                                    scientific = FALSE)),
                        hjust = -0.1,
                        size = 3
                    )

                # Adjust y-axis to accommodate labels (since we used coord_flip)
                max_val <- max(summary_data[[plot_col]], na.rm = TRUE)
                min_val <- min(0, min(summary_data[[plot_col]], na.rm = TRUE))
                plot <- plot +
                    ggplot2::ylim(min_val, max_val * 1.15)
            }

            plot <- plot + ggtheme

            print(plot)
            TRUE
        }
        )
    )
