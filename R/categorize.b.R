#' @title Categorize Continuous Variables
#' @return Categorized variable with frequency tables and distribution plots
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom stats quantile sd median
#'
#' @description
#' This tool converts continuous numeric variables into categorical variables
#' using various binning methods. Similar to questionr::icut but with multiple
#' automated binning options suitable for clinical research workflows.
#'
#' Features:
#' - Multiple binning methods (quantile, equal, manual, mean±SD, median split, Jenks)
#' - Customizable labels and category names
#' - Distribution visualization with break points
#' - R code generation for reproducibility
#'

categorizeClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "categorizeClass",
    inherit = categorizeBase,
    private = list(

        # Calculate break points based on method
        .calculateBreaks = function(x, method, nbins, manual_breaks, sdmult) {
            x <- x[!is.na(x)]

            if (length(x) == 0) {
                return(NULL)
            }

            breaks <- switch(method,
                "equal" = {
                    seq(min(x), max(x), length.out = nbins + 1)
                },
                "quantile" = {
                    probs <- seq(0, 1, length.out = nbins + 1)
                    unique(quantile(x, probs = probs, na.rm = TRUE))
                },
                "manual" = {
                    if (is.null(manual_breaks) || manual_breaks == "") {
                        return(NULL)
                    }
                    vals <- as.numeric(trimws(strsplit(manual_breaks, ",")[[1]]))
                    if (any(is.na(vals))) {
                        return(NULL)
                    }
                    sort(vals)
                },
                "meansd" = {
                    m <- mean(x, na.rm = TRUE)
                    s <- sd(x, na.rm = TRUE)
                    c(min(x), m - sdmult * s, m, m + sdmult * s, max(x))
                },
                "median" = {
                    med <- median(x, na.rm = TRUE)
                    c(min(x), med, max(x))
                },
                "jenks" = {
                    # Simple implementation of natural breaks
                    # For production, consider using classInt::classIntervals
                    if (requireNamespace("classInt", quietly = TRUE)) {
                        ci <- classInt::classIntervals(x, n = nbins, style = "jenks")
                        ci$brks
                    } else {
                        # Fallback to quantiles if classInt not available
                        probs <- seq(0, 1, length.out = nbins + 1)
                        unique(quantile(x, probs = probs, na.rm = TRUE))
                    }
                },
                # Default fallback
                {
                    probs <- seq(0, 1, length.out = nbins + 1)
                    unique(quantile(x, probs = probs, na.rm = TRUE))
                }
            )

            # Ensure breaks span the data range
            if (!is.null(breaks) && length(breaks) > 1) {
                if (min(breaks) > min(x)) breaks[1] <- min(x)
                if (max(breaks) < max(x)) breaks[length(breaks)] <- max(x)
            }

            return(breaks)
        },

        # Generate labels based on method and number of bins
        .generateLabels = function(breaks, label_type, custom_labels, nbins) {
            n_categories <- length(breaks) - 1

            if (n_categories <= 0) {
                return(NULL)
            }

            labels <- switch(label_type,
                "auto" = {
                    # Generate range labels like "[0-25)", "[25-50)", etc.
                    sapply(1:n_categories, function(i) {
                        sprintf("[%.1f, %.1f]", breaks[i], breaks[i + 1])
                    })
                },
                "semantic" = {
                    semantic_labels <- c("Very Low", "Low", "Medium-Low", "Medium",
                                        "Medium-High", "High", "Very High")
                    if (n_categories == 2) {
                        c("Low", "High")
                    } else if (n_categories == 3) {
                        c("Low", "Medium", "High")
                    } else if (n_categories == 4) {
                        c("Low", "Medium-Low", "Medium-High", "High")
                    } else if (n_categories == 5) {
                        c("Very Low", "Low", "Medium", "High", "Very High")
                    } else if (n_categories <= 7) {
                        semantic_labels[1:n_categories]
                    } else {
                        paste0("Level ", 1:n_categories)
                    }
                },
                "numbered" = {
                    as.character(1:n_categories)
                },
                "lettered" = {
                    LETTERS[1:n_categories]
                },
                "custom" = {
                    if (is.null(custom_labels) || custom_labels == "") {
                        paste0("Category ", 1:n_categories)
                    } else {
                        custom <- trimws(strsplit(custom_labels, ",")[[1]])
                        if (length(custom) != n_categories) {
                            # Warn and use numbered
                            paste0("Category ", 1:n_categories)
                        } else {
                            custom
                        }
                    }
                },
                # Default
                paste0("Category ", 1:n_categories)
            )

            return(labels)
        },

        # Generate R code for reproducibility
        .generateRCode = function(varname, method, nbins, breaks, sdmult,
                                  labels, customlabels, newvarname,
                                  includelowest, rightclosed, ordered) {

            if (newvarname == "") {
                newvarname <- paste0(varname, "_cat")
            }

            code <- "# Categorize continuous variable\n"

            if (method == "manual") {
                code <- paste0(code, "breaks <- c(", breaks, ")\n")
            } else if (method == "equal") {
                code <- paste0(code, "breaks <- seq(min(data$", varname, ", na.rm = TRUE), ",
                              "max(data$", varname, ", na.rm = TRUE), length.out = ", nbins + 1, ")\n")
            } else if (method == "quantile") {
                code <- paste0(code, "breaks <- quantile(data$", varname,
                              ", probs = seq(0, 1, length.out = ", nbins + 1, "), na.rm = TRUE)\n")
            } else if (method == "meansd") {
                code <- paste0(code, "m <- mean(data$", varname, ", na.rm = TRUE)\n",
                              "s <- sd(data$", varname, ", na.rm = TRUE)\n",
                              "breaks <- c(min(data$", varname, ", na.rm = TRUE), ",
                              "m - ", sdmult, " * s, m, m + ", sdmult, " * s, ",
                              "max(data$", varname, ", na.rm = TRUE))\n")
            } else if (method == "median") {
                code <- paste0(code, "breaks <- c(min(data$", varname, ", na.rm = TRUE), ",
                              "median(data$", varname, ", na.rm = TRUE), ",
                              "max(data$", varname, ", na.rm = TRUE))\n")
            } else if (method == "jenks") {
                code <- paste0(code, "# Requires: install.packages('classInt')\n",
                              "ci <- classInt::classIntervals(data$", varname,
                              ", n = ", nbins, ", style = 'jenks')\n",
                              "breaks <- ci$brks\n")
            }

            # Add label generation
            if (labels == "custom" && customlabels != "") {
                code <- paste0(code, "labels <- c('",
                              paste(trimws(strsplit(customlabels, ",")[[1]]), collapse = "', '"), "')\n")
            } else if (labels == "semantic") {
                code <- paste0(code, "# Generate semantic labels based on number of categories\n")
            } else if (labels == "numbered") {
                code <- paste0(code, "labels <- 1:(length(breaks) - 1)\n")
            } else if (labels == "lettered") {
                code <- paste0(code, "labels <- LETTERS[1:(length(breaks) - 1)]\n")
            }

            # Add cut command
            code <- paste0(code, "\ndata$", newvarname, " <- cut(\n",
                          "  data$", varname, ",\n",
                          "  breaks = breaks,\n",
                          "  labels = labels,\n",
                          "  include.lowest = ", ifelse(includelowest, "TRUE", "FALSE"), ",\n",
                          "  right = ", ifelse(rightclosed, "TRUE", "FALSE"), ",\n",
                          "  ordered_result = ", ifelse(ordered, "TRUE", "FALSE"), "\n",
                          ")\n")

            return(code)
        },

        .run = function() {

            # Input Validation ----
            if (is.null(self$options$var) || length(self$options$var) == 0) {
                todo <- "
                <div style='font-family: Arial, sans-serif; color: #2c3e50;'>
                  <h2>Categorize Continuous Variables</h2>
                  <p>This tool converts continuous numeric variables into categorical variables.</p>
                  <hr>
                  <h3>Instructions</h3>
                  <ol>
                    <li><strong>Select a variable</strong> - Choose a continuous numeric variable</li>
                    <li><strong>Choose binning method</strong>:
                      <ul>
                        <li><em>Equal Intervals</em>: Divide range into equal-width bins</li>
                        <li><em>Quantiles</em>: Create bins with equal number of observations</li>
                        <li><em>Manual Breaks</em>: Specify your own cut points</li>
                        <li><em>Mean +/- SD</em>: Use mean and standard deviation</li>
                        <li><em>Median Split</em>: Simple dichotomization at median</li>
                        <li><em>Natural Breaks (Jenks)</em>: Minimize within-class variance</li>
                      </ul>
                    </li>
                    <li><strong>Set number of categories</strong> and label style</li>
                    <li><strong>Review</strong> the frequency table and distribution plot</li>
                    <li><strong>Add to data</strong> - Check 'Add Categorized Variable to Data' in Output options to add directly to your dataset</li>
                  </ol>
                  <hr>
                  <p><strong>Tip:</strong> The new categorized variable will appear in your data view and can be used in other analyses like Alluvial Diagrams, Cross Tables, etc.</p>
                </div>
                "
                self$results$todo$setContent(todo)
                return()
            } else {
                self$results$todo$setContent("")
            }

            # Get data ----
            varname <- self$options$var

            if (!(varname %in% names(self$data))) {
                self$results$warnings$setContent(
                    paste0("<div style='color: #dc3545;'>Variable '",
                           htmltools::htmlEscape(varname),
                           "' not found in data.</div>")
                )
                return()
            }

            x <- self$data[[varname]]

            # Check if numeric
            if (!is.numeric(x)) {
                self$results$warnings$setContent(
                    paste0("<div style='color: #dc3545;'>Variable '",
                           htmltools::htmlEscape(varname),
                           "' is not numeric. Please select a continuous variable.</div>")
                )
                return()
            }

            # Handle missing values
            if (self$options$excl) {
                x_clean <- x[!is.na(x)]
            } else {
                x_clean <- x
            }

            n_total <- length(x)
            n_valid <- sum(!is.na(x))
            n_missing <- sum(is.na(x))

            # Variable summary ----
            summary_html <- paste0(
                "<div style='padding: 10px;'>",
                "<strong>Variable:</strong> ", htmltools::htmlEscape(varname), "<br>",
                "<strong>N (total):</strong> ", n_total, "<br>",
                "<strong>N (valid):</strong> ", n_valid, "<br>",
                "<strong>N (missing):</strong> ", n_missing, "<br>",
                "<strong>Range:</strong> ", round(min(x, na.rm = TRUE), 2), " - ",
                round(max(x, na.rm = TRUE), 2), "<br>",
                "<strong>Mean:</strong> ", round(mean(x, na.rm = TRUE), 2), "<br>",
                "<strong>Median:</strong> ", round(median(x, na.rm = TRUE), 2), "<br>",
                "<strong>SD:</strong> ", round(sd(x, na.rm = TRUE), 2),
                "</div>"
            )
            self$results$summaryText$setContent(summary_html)

            # Calculate breaks ----
            method <- self$options$method
            nbins <- self$options$nbins
            manual_breaks <- self$options$breaks
            sdmult <- self$options$sdmult

            breaks <- private$.calculateBreaks(x_clean, method, nbins, manual_breaks, sdmult)

            if (is.null(breaks) || length(breaks) < 2) {
                self$results$warnings$setContent(
                    paste0("<div style='color: #dc3545;'>Could not calculate break points. ",
                    "Please check your settings.</div>")
                )
                return()
            }

            # Check for valid breaks
            if (method == "manual") {
                custom <- as.numeric(trimws(strsplit(manual_breaks, ",")[[1]]))
                if (any(is.na(custom))) {
                    self$results$warnings$setContent(
                        "<div style='color: #dc3545;'>Invalid manual break points. ",
                        "Please enter comma-separated numbers.</div>"
                    )
                    return()
                }
            }

            # Check if custom labels match number of categories
            n_categories <- length(breaks) - 1
            warning_msg <- ""

            if (self$options$labels == "custom" && self$options$customlabels != "") {
                custom_labels <- trimws(strsplit(self$options$customlabels, ",")[[1]])
                if (length(custom_labels) != n_categories) {
                    warning_msg <- paste0(
                        "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; ",
                        "padding: 10px; margin: 10px 0;'>",
                        "<strong>Warning:</strong> You provided ", length(custom_labels),
                        " labels but have ", n_categories, " categories. ",
                        "Using numbered labels instead.</div>"
                    )
                }
            }

            # Generate labels ----
            labels_result <- private$.generateLabels(
                breaks,
                self$options$labels,
                self$options$customlabels,
                nbins
            )

            # Create categorized variable ----
            x_cat <- cut(
                x,
                breaks = breaks,
                labels = labels_result,
                include.lowest = self$options$includelowest,
                right = self$options$rightclosed,
                ordered_result = self$options$ordered
            )

            # Populate break points table ----
            breakTable <- self$results$breakpointsTable
            for (i in seq_along(breaks)) {
                breakTable$addRow(rowKey = i, values = list(
                    index = i,
                    value = round(breaks[i], 4)
                ))
            }

            # Populate frequency table ----
            freqTable <- self$results$freqTable
            freq <- table(x_cat, useNA = "ifany")

            total <- sum(freq)
            cumsum_freq <- cumsum(freq)

            for (i in seq_along(freq)) {
                cat_name <- names(freq)[i]
                if (is.na(cat_name)) cat_name <- "Missing"

                # Get range for this category
                if (!is.na(names(freq)[i]) && i <= n_categories) {
                    range_str <- sprintf("[%.2f, %.2f]", breaks[i], breaks[i + 1])
                } else {
                    range_str <- "NA"
                }

                freqTable$addRow(rowKey = i, values = list(
                    category = cat_name,
                    range = range_str,
                    n = as.integer(freq[i]),
                    percent = freq[i] / total,
                    cumPercent = cumsum_freq[i] / total
                ))
            }

            # Add categorized variable to data ----
            if (self$options$addtodata && self$results$addtodata$isNotFilled()) {
                # Convert factor to character for jamovi compatibility
                x_cat_values <- as.character(x_cat)

                # Set row numbers (1-based indices for all rows)
                self$results$addtodata$setRowNums(1:length(x_cat_values))
                self$results$addtodata$setValues(x_cat_values)
            }

            # Generate R code ----
            if (self$options$showcode) {
                newvarname <- self$options$newvarname
                if (newvarname == "") newvarname <- paste0(varname, "_cat")

                code <- private$.generateRCode(
                    varname, method, nbins, manual_breaks, sdmult,
                    self$options$labels, self$options$customlabels, newvarname,
                    self$options$includelowest, self$options$rightclosed, self$options$ordered
                )

                code_html <- paste0(
                    "<div style='background-color: #f8f9fa; padding: 15px; ",
                    "border-radius: 4px; font-family: monospace; white-space: pre-wrap;'>",
                    htmltools::htmlEscape(code),
                    "</div>"
                )
                self$results$rcode$setContent(code_html)
            }

            # Set warnings
            self$results$warnings$setContent(warning_msg)
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Input validation
            if (is.null(self$options$var) || !self$options$showplot) {
                return()
            }

            varname <- self$options$var
            if (!(varname %in% names(self$data))) {
                return()
            }

            x <- self$data[[varname]]
            if (!is.numeric(x)) {
                return()
            }

            # Get breaks
            if (self$options$excl) {
                x_clean <- x[!is.na(x)]
            } else {
                x_clean <- x
            }

            breaks <- private$.calculateBreaks(
                x_clean,
                self$options$method,
                self$options$nbins,
                self$options$breaks,
                self$options$sdmult
            )

            if (is.null(breaks) || length(breaks) < 2) {
                return()
            }

            # Create plot data
            plot_data <- data.frame(x = x[!is.na(x)])

            # Create histogram with break lines
            plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x)) +
                ggplot2::geom_histogram(
                    ggplot2::aes(y = ggplot2::after_stat(density)),
                    bins = 30,
                    fill = "#6c757d",
                    color = "white",
                    alpha = 0.7
                ) +
                ggplot2::geom_density(
                    color = "#007bff",
                    linewidth = 1
                ) +
                ggplot2::geom_vline(
                    xintercept = breaks,
                    color = "#dc3545",
                    linetype = "dashed",
                    linewidth = 0.8
                ) +
                ggplot2::labs(
                    title = paste("Distribution of", varname),
                    subtitle = paste("Red dashed lines show category boundaries (",
                                    self$options$method, " method)"),
                    x = varname,
                    y = "Density"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                    plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "#666666")
                )

            # Add break point labels
            break_labels <- data.frame(
                x = breaks,
                y = max(ggplot2::ggplot_build(plot)$data[[1]]$density, na.rm = TRUE) * 1.05,
                label = round(breaks, 1)
            )

            plot <- plot +
                ggplot2::geom_text(
                    data = break_labels,
                    ggplot2::aes(x = x, y = y, label = label),
                    color = "#dc3545",
                    size = 3,
                    angle = 45,
                    hjust = 0
                )

            print(plot)
            TRUE
        }
    )
)
