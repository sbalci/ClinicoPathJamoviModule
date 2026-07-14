#' @title Categorize Continuous Variables
#' @return Categorized variable with frequency tables and distribution plots
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats quantile sd median
#'
#' @description
#' This tool converts continuous numeric variables into categorical variables
#' using various binning methods. Similar to questionr::icut but with multiple
#' automated binning options suitable for clinical research workflows.
#'
#' Features:
#' - Multiple binning methods (quantile, equal, manual, mean+/-SD, median split, Jenks)
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
                                   "(2) duplicate manual breaks, or (3) zero variance in mean/median\u{00B1}SD methods. ",
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
        .generateLabels = function(breaks, label_type, custom_labels, include_lowest, right_closed) {
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
                        if (length(custom) != n_categories || anyDuplicated(custom) > 0) {
                            # Fall back to numbered when the count does not match
                            # or the labels are not unique (duplicate factor
                            # labels break cut()/merge categories).
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

        # Generate R code for reproducibility.
        # Variable names are backtick-quoted via jmvcore::composeTerm() inside
        # .generateRCode() (below), and custom labels are serialized with dput(),
        # so the generated copy-paste snippet stays valid R for names with spaces
        # or special characters.
        .generateRCode = function(varname, method, nbins, breaks, sdmult,
                                  labels, customlabels, newvarname,
                                  includelowest, rightclosed, ordered) {

            if (newvarname == "") {
                newvarname <- paste0(varname, "_cat")
            }

            # Backtick-quote names for the generated (display-only) R snippet so a variable
            # name containing spaces / special characters still produces syntactically valid
            # copy-paste R. Done after the default newvarname is derived from the raw name.
            varname <- jmvcore::composeTerm(varname)
            newvarname <- jmvcore::composeTerm(newvarname)

            code <- "# Categorize continuous variable\n"

            if (method == "manual") {
                code <- paste0(code, "breaks <- c(", breaks, ")\n")
            } else if (method == "equal") {
                code <- paste0(code, "breaks <- seq(min(data$", varname, ", na.rm = TRUE), ",
                              "max(data$", varname, ", na.rm = TRUE), length.out = ", nbins + 1, ")\n")
            } else if (method == "quantile") {
                # unique(sort(...)) mirrors the backend so the copy-paste snippet
                # does not throw "'breaks' are not unique" on tied data.
                code <- paste0(code, "breaks <- unique(sort(quantile(data$", varname,
                              ", probs = seq(0, 1, length.out = ", nbins + 1, "), na.rm = TRUE)))\n")
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
                              "breaks <- unique(sort(ci$brks))\n")
            }

            # Add label generation
            if (labels == "custom" && customlabels != "") {
                custom_parts <- trimws(strsplit(customlabels, ",")[[1]])
                custom_literals <- paste(
                    utils::capture.output(dput(custom_parts)),
                    collapse = "\n"
                )
                code <- paste0(
                    code,
                    "labels <- ",
                    custom_literals,
                    "\n"
                )
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

            # TODO (forward-looking): no `.()` wrapping anywhere in this file:
            # the welcome HTML, error notice bodies (already migrated to HTML
            # boxes), assumption text, and the .noticeBox helper messages are
            # all English-only. Address in a /prepare-translation pass.
            # TODO (forward-looking, perf): cache these breaks in plot state so
            # the render callback does not calculate them a second time.

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
                    <li><strong>Add to data</strong> - Enable the 'Categorized variable' output (below the binning options) to add it directly to your dataset</li>
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

            # Notices are rendered as HTML in `todo` to avoid the protobuf
            # serialization failure triggered by dynamic jmvcore::Notice objects
            # (see docs/NOTICE_TO_HTML_CONVERSION_GUIDE.md).
            .errBox <- function(msg)
                paste0("<div style='padding: 15px; background-color: #f8d7da; border-left: 4px solid #dc3545; color: #721c24; border-radius: 5px;'><strong>Error:</strong> ", msg, "</div>")

            if (!(varname %in% names(self$data))) {
                self$results$todo$setContent(.errBox(jmvcore::format(
                    "Variable '{}' not found in dataset. Please select a valid variable from the data.",
                    htmltools::htmlEscape(varname))))
                return()
            }

            x <- self$data[[varname]]

            # Check if numeric
            if (!is.numeric(x)) {
                self$results$todo$setContent(.errBox(jmvcore::format(
                    "Variable '{}' is not numeric. Categorization requires a continuous numeric variable.",
                    htmltools::htmlEscape(varname))))
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
                self$results$todo$setContent(.errBox(jmvcore::format(
                    "Variable '{}' has zero variability (constant value). Cannot create categories from a constant variable.",
                    htmltools::htmlEscape(varname))))
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

            private$.checkpoint()
            breaks <- private$.calculateBreaks(
                x_clean,
                method,
                nbins,
                manual_breaks,
                sdmult
            )

            # Enforce sorted unique breaks to avoid cut() failures
            if (!is.null(breaks)) {
                breaks <- sort(unique(breaks))
            }

            # Validate breaks with detailed error messages
            validation <- private$.validateBreaks(breaks, method)
            if (!validation$valid) {
                self$results$todo$setContent(.errBox(jmvcore::format(
                    "Break point validation failed: {}",
                    htmltools::htmlEscape(validation$message))))
                return()
            }

            # Cache the computed breaks in plot state so the render callback
            # (.plot) reuses them instead of recomputing from self$data /
            # self$options. Only simple, serializable values are stored.
            if (self$options$showplot) {
                self$results$plot$setState(list(
                    breaks  = breaks,
                    method  = method,
                    varname = varname
                ))
            }

            # Additional check for manual breaks
            if (method == "manual") {
                custom <- as.numeric(trimws(strsplit(manual_breaks, ",")[[1]]))
                if (any(is.na(custom))) {
                    self$results$todo$setContent(.errBox(
                        "Invalid manual break points. Please enter comma-separated numeric values (e.g., 0, 25, 50, 75, 100)."))
                    return()
                }
            }

            # Check if custom labels match number of categories
            n_categories <- length(breaks) - 1

            # Collect notice HTML chunks for prioritized rendering.
            # Notices are emitted as HTML to avoid the jamovi protobuf
            # serialization failure caused by dynamically inserted
            # jmvcore::Notice objects (see docs/NOTICE_TO_HTML_CONVERSION_GUIDE.md).
            notice_html <- list()
            .noticeBox <- function(level, msg) {
                cfg <- switch(level,
                    STRONG_WARNING = list(bg = "#fff3cd", border = "#ff9800", title = "Warning", color = "#856404"),
                    WARNING        = list(bg = "#fff3cd", border = "#ffc107", title = "Warning", color = "#856404"),
                    INFO           = list(bg = "#d1ecf1", border = "#17a2b8", title = "Note",    color = "#0c5460"))
                paste0(
                    "<div style='padding: 12px 15px; margin: 6px 0; background-color: ", cfg$bg,
                    "; border-left: 4px solid ", cfg$border, "; color: ", cfg$color,
                    "; border-radius: 4px;'><strong>", cfg$title, ":</strong> ", msg, "</div>")
            }

            # WARNING: Jenks falls back to quantile
            if (method == "jenks" && !requireNamespace("classInt", quietly = TRUE)) {
                notice_html$jenksFallback <- .noticeBox("WARNING",
                    "Natural Breaks (Jenks) requires the 'classInt' package. Using quantile-based binning instead. Install classInt with install.packages('classInt') to enable true Jenks optimization.")
            }

            # WARNING: Custom labels mismatch
            if (self$options$labels == "custom" && self$options$customlabels != "") {
                custom_labels <- trimws(strsplit(self$options$customlabels, ",")[[1]])
                if (length(custom_labels) != n_categories) {
                    notice_html$labelMismatch <- .noticeBox("WARNING", jmvcore::format(
                        "Custom labels mismatch: provided {} labels but have {} categories. Using numbered labels instead.",
                        length(custom_labels), n_categories))
                } else if (anyDuplicated(custom_labels) > 0) {
                    notice_html$labelMismatch <- .noticeBox("WARNING",
                        "Custom labels contain duplicate values. Category labels must be unique; using numbered labels instead.")
                }
            }

            # WARNING: Bin collapse
            if (method %in% c("equal", "quantile", "jenks") && n_categories != nbins) {
                notice_html$binCollapse <- .noticeBox("WARNING", jmvcore::format(
                    "Bin collapse: requested {} categories but only {} distinct bins could be created due to tied values or limited range. Interpretations based on '{}-tiles' (e.g., quartiles, tertiles) may be misleading; verify bin boundaries before use.",
                    nbins, n_categories, nbins))
            }

            # Generate labels ----
            labels_result <- private$.generateLabels(
                breaks,
                self$options$labels,
                self$options$customlabels,
                self$options$includelowest,
                self$options$rightclosed
            )

            # Create categorized variable ----
            private$.checkpoint()
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
                # STRONG_WARNING: Very small bins (< 5 observations)
                small_bins <- sum(bin_counts < 5)
                if (small_bins > 0) {
                    notice_html$smallBins <- .noticeBox("STRONG_WARNING", jmvcore::format(
                        "Small bins detected: {} bin(s) have fewer than 5 observations. Statistical analyses may be unreliable with such small group sizes; consider reducing the number of categories or using a different binning method.",
                        small_bins))
                }

                # STRONG_WARNING: Severe imbalance (one bin has >70% of observations)
                max_prop <- max(bin_counts) / n_valid_for_check
                if (max_prop > 0.70) {
                    # sprintf("%.1f%%", x) retained intentionally to preserve trailing-zero
                    # formatting (e.g. "50.0%"). Translation pass will revisit.
                    notice_html$binImbalance <- .noticeBox("STRONG_WARNING", sprintf(
                        "Severe bin imbalance: one bin contains %.1f%% of observations. This may reduce statistical power and affect clinical interpretations; consider using quantile-based binning for balanced groups.",
                        max_prop * 100))
                }
            }

            # WARNING: Outlier sensitivity for mean±SD method
            if (method == "meansd") {
                # Check if data has extreme outliers using IQR method
                x_clean_check <- x[!is.na(x)]
                q1 <- quantile(x_clean_check, 0.25)
                q3 <- quantile(x_clean_check, 0.75)
                iqr <- q3 - q1
                outliers <- sum(x_clean_check < (q1 - 3 * iqr) | x_clean_check > (q3 + 3 * iqr))

                if (outliers > 0) {
                    notice_html$outlierSensitivity <- .noticeBox("WARNING", jmvcore::format(
                        "Outlier sensitivity: detected {} extreme outlier(s). Mean\u{00B1}SD binning is sensitive to outliers, which can create poorly distributed categories. Consider using quantile or natural breaks methods.",
                        outliers))
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

                # Get range for this category with correct bracket notation.
                # table() returns levels in break order, so the interval index
                # is the loop's ordinal position (row_idx). Parsing the label
                # (as.integer(cat_name)) only works for the "numbered" label
                # style and returns NA (with a coercion warning) for
                # semantic/lettered/custom/auto labels.
                cat_idx <- row_idx
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

                # Calculate percentages based on valid observations only.
                # Guard the denominator: manual breaks can leave zero valid
                # categorized observations, which would give NaN/Inf.
                if (n_valid_obs > 0) {
                    pct_val <- freq[i] / n_valid_obs
                    cum_pct_val <- cumsum_freq[row_idx] / n_valid_obs
                } else {
                    pct_val <- NaN
                    cum_pct_val <- NaN
                }

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
                self$results$addtodata$setRowNums(seq_along(x_cat_values))
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

            # INFO: Analysis complete with methodological note
            notice_html$analysisComplete <- .noticeBox("INFO", jmvcore::format(
                "Categorization completed: {} valid observations divided into {} groups using {} method. Note: Categorization reduces statistical power and may obscure dose-response relationships (Altman & Royston, BMJ 2006;332:1080). Continuous analyses are generally preferred unless there is strong clinical justification.",
                n_valid, n_categories, self$options$method))

            # Render notices in priority order: STRONG_WARNING -> WARNING -> INFO
            priority_order <- c('smallBins', 'binImbalance', 'jenksFallback', 'labelMismatch', 'binCollapse', 'outlierSensitivity', 'analysisComplete')
            rendered <- character()
            for (name in priority_order) {
                if (!is.null(notice_html[[name]])) {
                    rendered <- c(rendered, notice_html[[name]])
                }
            }
            if (length(rendered) > 0) {
                self$results$todo$setContent(paste(rendered, collapse = ""))
            }
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

            # Prefer breaks cached in plot state (set in .run) to avoid
            # recomputing; fall back to recomputation if state is unavailable.
            state <- image$state
            if (!is.null(state) && !is.null(state$breaks)) {
                breaks <- state$breaks
            } else {
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
            }

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
