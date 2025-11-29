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

        # Validate breaks for strict monotonicity and uniqueness
        .validateBreaks = function(breaks, method) {
            if (is.null(breaks) || length(breaks) < 2) {
                return(list(valid = FALSE, message = "Insufficient break points generated."))
            }

            # Check for NaN or Inf
            if (any(is.na(breaks)) || any(is.infinite(breaks))) {
                return(list(valid = FALSE, message = "Break points contain invalid values (NA or Inf)."))
            }

            # Check for strict monotonicity (no duplicates, strictly increasing)
            if (any(diff(breaks) <= 0)) {
                return(list(valid = FALSE,
                    message = paste0("Break points are not strictly increasing. ",
                                   "This can occur with: (1) tied/constant values in quantile methods, ",
                                   "(2) duplicate manual breaks, or (3) zero variance in mean/median±SD methods. ",
                                   "Please check your data or adjust the binning method.")))
            }

            # Check minimum separation (relative to range)
            breaks_range <- max(breaks) - min(breaks)
            min_diff <- min(diff(breaks))
            if (breaks_range > 0 && min_diff / breaks_range < 1e-10) {
                return(list(valid = FALSE,
                    message = "Break points are too close together (possible numerical precision issue)."))
            }

            return(list(valid = TRUE, message = NULL))
        },

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
                    # Guard against zero variance
                    if (s == 0 || is.na(s)) {
                        return(NULL)  # Will trigger validation error
                    }
                    breaks_raw <- c(min(x), m - sdmult * s, m, m + sdmult * s, max(x))
                    # Remove duplicates and ensure strictly increasing
                    unique(sort(breaks_raw))
                },
                "median" = {
                    med <- median(x, na.rm = TRUE)
                    # Guard against median equals min or max (constant data)
                    if (med == min(x) || med == max(x)) {
                        return(NULL)  # Will trigger validation error
                    }
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
        .generateLabels = function(breaks, label_type, custom_labels, nbins, include_lowest, right_closed) {
            n_categories <- length(breaks) - 1

            if (n_categories <= 0) {
                return(NULL)
            }

            labels <- switch(label_type,
                "auto" = {
                    # Generate range labels with correct bracket notation
                    # right=TRUE (right_closed=TRUE): (a, b] except first is [a, b] when include_lowest=TRUE
                    # right=FALSE (right_closed=FALSE): [a, b) except last is [a, b] when include_lowest=TRUE
                    sapply(1:n_categories, function(i) {
                        if (right_closed) {
                            left_bracket <- "("
                            right_bracket <- "]"
                            if (i == 1 && include_lowest) left_bracket <- "["
                        } else {
                            left_bracket <- "["
                            right_bracket <- ")"
                            if (i == n_categories && include_lowest) right_bracket <- "]"
                        }
                        sprintf("%s%.1f, %.1f%s", left_bracket, breaks[i], breaks[i + 1], right_bracket)
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
                # Generate semantic labels based on number of categories
                code <- paste0(code,
                    "# Generate semantic labels based on number of categories\n",
                    "n_categories <- length(breaks) - 1\n",
                    "if (n_categories == 2) {\n",
                    "  labels <- c('Low', 'High')\n",
                    "} else if (n_categories == 3) {\n",
                    "  labels <- c('Low', 'Medium', 'High')\n",
                    "} else if (n_categories == 4) {\n",
                    "  labels <- c('Low', 'Medium-Low', 'Medium-High', 'High')\n",
                    "} else if (n_categories == 5) {\n",
                    "  labels <- c('Very Low', 'Low', 'Medium', 'High', 'Very High')\n",
                    "} else if (n_categories <= 7) {\n",
                    "  semantic_labels <- c('Very Low', 'Low', 'Medium-Low', 'Medium', 'Medium-High', 'High', 'Very High')\n",
                    "  labels <- semantic_labels[1:n_categories]\n",
                    "} else {\n",
                    "  labels <- paste0('Level ', 1:n_categories)\n",
                    "}\n")
            } else if (labels == "numbered") {
                code <- paste0(code, "labels <- as.character(1:(length(breaks) - 1))\n")
            } else if (labels == "lettered") {
                code <- paste0(code, "labels <- LETTERS[1:(length(breaks) - 1)]\n")
            } else if (labels == "auto") {
                # Generate range labels with correct bracket notation
                code <- paste0(code,
                    "# Generate range labels\n",
                    "n_categories <- length(breaks) - 1\n",
                    "labels <- sapply(1:n_categories, function(i) {\n",
                    "  left_bracket <- ifelse(", ifelse(rightclosed, "TRUE", "FALSE"), ", '(', '[')\n",
                    "  right_bracket <- ifelse(", ifelse(rightclosed, "TRUE", "FALSE"), ", ']', ')')\n",
                    "  if (i == 1 && ", ifelse(includelowest, "TRUE", "FALSE"), ") left_bracket <- '['\n",
                    "  sprintf('%s%.1f, %.1f%s', left_bracket, breaks[i], breaks[i + 1], right_bracket)\n",
                    "})\n")
            } else {
                # Fallback to numbered
                code <- paste0(code, "labels <- as.character(1:(length(breaks) - 1))\n")
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

            # Basic sanity check for variability
            if (sum(!is.na(x_clean)) < 2 || sd(x_clean, na.rm = TRUE) == 0) {
                self$results$warnings$setContent(
                    paste0("<div style='color: #dc3545;'>Variable '",
                           htmltools::htmlEscape(varname),
                           "' has no variability; cannot create categories.</div>")
                )
                return()
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

            # Enforce sorted unique breaks to avoid cut() failures
            if (!is.null(breaks)) {
                breaks <- sort(unique(breaks))
            }

            # Validate breaks with detailed error messages
            validation <- private$.validateBreaks(breaks, method)
            if (!validation$valid) {
                self$results$warnings$setContent(
                    paste0("<div style='color: #dc3545;'><strong>Error:</strong> ",
                           htmltools::htmlEscape(validation$message), "</div>")
                )
                return()
            }

            # Additional check for manual breaks
            if (method == "manual") {
                custom <- as.numeric(trimws(strsplit(manual_breaks, ",")[[1]]))
                if (any(is.na(custom))) {
                    self$results$warnings$setContent(
                        "<div style='color: #dc3545;'><strong>Error:</strong> Invalid manual break points. ",
                        "Please enter comma-separated numbers.</div>"
                    )
                    return()
                }
            }

            # Check if custom labels match number of categories
            n_categories <- length(breaks) - 1
            warning_msg <- ""

            # Alert if Jenks falls back to quantile
            if (method == "jenks" && !requireNamespace("classInt", quietly = TRUE)) {
                warning_msg <- paste0(
                    warning_msg,
                    "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; ",
                    "padding: 10px; margin: 10px 0;'>",
                    "<strong>Package Missing:</strong> Natural Breaks (Jenks) requires the 'classInt' package. ",
                    "<strong>Fallback:</strong> Using quantile-based binning instead. ",
                    "Install classInt with <code>install.packages('classInt')</code> to use true Jenks optimization.</div>"
                )
            }

            if (self$options$labels == "custom" && self$options$customlabels != "") {
                custom_labels <- trimws(strsplit(self$options$customlabels, ",")[[1]])
                if (length(custom_labels) != n_categories) {
                    warning_msg <- paste0(
                        warning_msg,
                        "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; ",
                        "padding: 10px; margin: 10px 0;'>",
                        "<strong>Warning:</strong> You provided ", length(custom_labels),
                        " labels but have ", n_categories, " categories. ",
                        "Using numbered labels instead.</div>"
                    )
                }
            }

            # Warn if bins collapsed due to ties/insufficient range
            if (method %in% c("equal", "quantile", "jenks") && n_categories != nbins) {
                warning_msg <- paste0(
                    warning_msg,
                    "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; ",
                    "padding: 10px; margin: 10px 0;'>",
                    "<strong>Bin Collapse Warning:</strong> Requested ", nbins, " categories but only ", n_categories,
                    " distinct bins could be created due to tied values or limited range. ",
                    "<strong>Clinical Impact:</strong> Interpretations based on \"", nbins, "-tiles\" ",
                    "(e.g., quartiles, tertiles) may be misleading. Verify bin boundaries before use.</div>"
                )
            }

            # Generate labels ----
            labels_result <- private$.generateLabels(
                breaks,
                self$options$labels,
                self$options$customlabels,
                nbins,
                self$options$includelowest,
                self$options$rightclosed
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

            # Clinical suitability checks ----
            # Check bin balance and minimum counts
            bin_counts <- table(x_cat, useNA = "no")
            n_valid_for_check <- sum(bin_counts)

            if (length(bin_counts) > 0 && n_valid_for_check > 0) {
                # Check for very small bins (< 5 observations)
                small_bins <- sum(bin_counts < 5)
                if (small_bins > 0) {
                    warning_msg <- paste0(
                        warning_msg,
                        "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; ",
                        "padding: 10px; margin: 10px 0;'>",
                        "<strong>Small Bin Warning:</strong> ", small_bins, " bin(s) have fewer than 5 observations. ",
                        "Statistical analyses may be unreliable with such small group sizes.</div>"
                    )
                }

                # Check for severe imbalance (one bin has >70% of observations)
                max_prop <- max(bin_counts) / n_valid_for_check
                if (max_prop > 0.70) {
                    warning_msg <- paste0(
                        warning_msg,
                        "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; ",
                        "padding: 10px; margin: 10px 0;'>",
                        "<strong>Bin Imbalance Warning:</strong> One bin contains ",
                        round(max_prop * 100, 1), "% of observations. ",
                        "Severe imbalance may reduce statistical power and affect clinical interpretations.</div>"
                    )
                }
            }

            # Outlier sensitivity warning for mean±SD method
            if (method == "meansd") {
                # Check if data has extreme outliers using IQR method
                x_clean_check <- x[!is.na(x)]
                q1 <- quantile(x_clean_check, 0.25)
                q3 <- quantile(x_clean_check, 0.75)
                iqr <- q3 - q1
                outliers <- sum(x_clean_check < (q1 - 3 * iqr) | x_clean_check > (q3 + 3 * iqr))

                if (outliers > 0) {
                    warning_msg <- paste0(
                        warning_msg,
                        "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; ",
                        "padding: 10px; margin: 10px 0;'>",
                        "<strong>Outlier Sensitivity Warning:</strong> Detected ", outliers,
                        " extreme outlier(s). Mean±SD binning is sensitive to outliers, ",
                        "which can create poorly distributed categories. Consider using quantile or natural breaks methods.</div>"
                    )
                }
            }

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

            # Calculate frequencies
            # When excl=TRUE, breaks are based on non-missing but x_cat includes NA where x was NA
            freq <- table(x_cat, useNA = "ifany")

            # Separate valid and missing counts for clarity
            n_total_obs <- length(x_cat)
            n_valid_obs <- sum(!is.na(x_cat))
            n_missing_obs <- sum(is.na(x_cat))

            # Calculate cumulative frequencies (only for valid categories)
            valid_indices <- which(!is.na(names(freq)))
            cumsum_freq <- cumsum(as.numeric(freq[valid_indices]))

            # Add valid categories
            row_idx <- 1
            for (i in valid_indices) {
                cat_name <- names(freq)[i]

                # Get range for this category with correct bracket notation
                cat_idx <- as.integer(cat_name)
                if (!is.na(cat_idx) && cat_idx <= n_categories) {
                    # Determine bracket notation based on cut() logic
                    # right=TRUE (rightclosed=TRUE): (a, b] except first is [a, b] when include.lowest=TRUE
                    # right=FALSE (rightclosed=FALSE): [a, b) except last is [a, b] when include.lowest=TRUE
                    if (self$options$rightclosed) {
                        # right=TRUE: (a, b]
                        left_bracket <- "("
                        right_bracket <- "]"
                        # First interval with include.lowest becomes [a, b]
                        if (cat_idx == 1 && self$options$includelowest) {
                            left_bracket <- "["
                        }
                    } else {
                        # right=FALSE: [a, b)
                        left_bracket <- "["
                        right_bracket <- ")"
                        # Last interval with include.lowest becomes [a, b]
                        if (cat_idx == n_categories && self$options$includelowest) {
                            right_bracket <- "]"
                        }
                    }
                    range_str <- sprintf("%s%.2f, %.2f%s", left_bracket, breaks[cat_idx], breaks[cat_idx + 1], right_bracket)
                } else {
                    range_str <- as.character(cat_name)
                }

                # Calculate percentages based on valid observations only
                pct_val <- freq[i] / n_valid_obs
                cum_pct_val <- cumsum_freq[row_idx] / n_valid_obs

                freqTable$addRow(rowKey = row_idx, values = list(
                    category = cat_name,
                    range = range_str,
                    n = as.integer(freq[i]),
                    percent = pct_val,
                    cumPercent = cum_pct_val
                ))
                row_idx <- row_idx + 1
            }

            # Add missing row if there are missing values
            if (n_missing_obs > 0) {
                freqTable$addRow(rowKey = row_idx, values = list(
                    category = "Missing",
                    range = "NA",
                    n = as.integer(n_missing_obs),
                    percent = NaN,  # Don't calculate percentage for missing
                    cumPercent = NaN
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

            # Add general caution about categorization information loss
            warning_msg <- paste0(
                warning_msg,
                "<div style='background-color: #e8f4fd; border-left: 4px solid #0d6efd; ",
                "padding: 10px; margin: 10px 0;'>",
                "<strong>Methodological Note:</strong> Dichotomization and categorization reduce statistical power, ",
                "obscure dose–response relationships, and can introduce arbitrary thresholds. ",
                "Continuous analyses are generally preferred unless there is strong clinical justification for categories. ",
                "See: Altman DG, Royston P. The cost of dichotomising continuous variables. BMJ 2006;332:1080.</div>"
            )

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

            if (!is.null(breaks)) {
                breaks <- sort(unique(breaks))
            }

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
