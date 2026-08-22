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

        # Above this n, classInt computes Fisher-Jenks breaks on the full data
        # in well under a second; above it the exact algorithm becomes too slow
        # for an interactive pane and a pinned-seed subsample is used instead.
        .jenksExactMaxN = 20000L,

        # Which mean+/-SD band, if any, collapsed against the data range
        # on the last .calculateBreaks() call. Set there, read in .run().
        .meansdDropped = character(),

        # Decimal places needed for every break point to print distinctly.
        #
        # A hard-coded single decimal silently produced DUPLICATE interval
        # labels on sub-unit scales (proportions, Ki-67 fractions, indices,
        # ratios). cut() does not error on duplicate labels - factor() merges
        # them - so two categories became one, the frequency table lost a row,
        # and the Range column then described a different interval from the one
        # the row counted. Every place that prints a break point uses this.
        .autoDigits = function(breaks) {
            d <- suppressWarnings(min(diff(breaks)))
            digits <- if (!is.finite(d) || d <= 0)
                1L
            else
                max(1L, as.integer(ceiling(-log10(d))) + 1L)
            while (digits < 12L &&
                   anyDuplicated(sprintf(paste0("%.", digits, "f"), breaks)) > 0)
                digits <- digits + 1L
            min(digits, 12L)
        },

        # Validate breaks for strict monotonicity and uniqueness
        # Single reading of the manual break-point string, shared by the
        # analysis and the generated R snippet.
        .parseManualBreaks = function(manual_breaks) {
            if (is.null(manual_breaks) || !nzchar(trimws(manual_breaks)))
                return(NULL)
            vals <- suppressWarnings(
                as.numeric(trimws(strsplit(manual_breaks, ",")[[1]])))
            if (anyNA(vals)) return(NULL)
            vals
        },

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
        .calculateBreaks = function(x, method, nbins, manual_breaks, sdmult,
                                    extend_to_data = TRUE) {
            x <- x[!is.na(x)]
            private$.meansdDropped <- character()

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
                    vals <- private$.parseManualBreaks(manual_breaks)
                    if (is.null(vals)) {
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
                    # On skewed data (CRP, ferritin, tumour burden) m - k*s can
                    # fall at or below min(x), and m + k*s above max(x). Folding
                    # that boundary against min/max used to leave a degenerate
                    # sliver bin - measured 6 of 300 cases in bin 1 on
                    # rexp(300, 1/12) - which the small-bin (< 5) and 70%
                    # imbalance checks both miss. Drop the unusable boundary
                    # instead and report it (private$.meansdDropped, read in
                    # .run()) so the user is told which band was lost.
                    lo <- m - sdmult * s
                    hi <- m + sdmult * s
                    dropped <- character()
                    if (lo <= min(x)) dropped <- c(dropped, "lower")
                    if (hi >= max(x)) dropped <- c(dropped, "upper")
                    private$.meansdDropped <- dropped
                    breaks_raw <- c(min(x), m, max(x))
                    if (!("lower" %in% dropped)) breaks_raw <- c(breaks_raw, lo)
                    if (!("upper" %in% dropped)) breaks_raw <- c(breaks_raw, hi)
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
                    if (requireNamespace("classInt", quietly = TRUE)) {
                        # style = "jenks" draws a RANDOM 10% subsample once n
                        # exceeds classInt's largeN (3000) and sets no seed, so
                        # the same registry gave different cut-points on every
                        # run - measured interior breaks 38.5/48.5/59.3 versus
                        # 40.2/49.5/58.7 on two consecutive calls with n = 6000.
                        # "fisher" is the exact Fisher-Jenks optimum (identical
                        # objective, no heuristic) and runs on the full data
                        # here; classInt's own message recommends it for large
                        # n. Above .jenksExactMaxN it still subsamples, so the
                        # seed is pinned and restored to keep runs reproducible
                        # without disturbing the user's RNG stream.
                        n_x <- length(x)
                        old_seed <- if (exists(".Random.seed", envir = globalenv()))
                            get(".Random.seed", envir = globalenv()) else NULL
                        on.exit({
                            if (is.null(old_seed))
                                suppressWarnings(rm(".Random.seed", envir = globalenv()))
                            else
                                assign(".Random.seed", old_seed, envir = globalenv())
                        }, add = TRUE)
                        set.seed(20240101L)
                        ci <- suppressWarnings(classInt::classIntervals(
                            x,
                            n = nbins,
                            style = "fisher",
                            largeN = if (n_x <= private$.jenksExactMaxN) Inf else 3000L))
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

            # Ensure breaks span the data range by EXTENDING it, not by moving the
            # outermost cut-points.
            #
            # This used to assign breaks[1] <- min(x) and breaks[length] <- max(x),
            # which overwrote the user's own outer thresholds. For every computed
            # method the endpoints already are min(x)/max(x), so it was a no-op
            # there - it only ever affected MANUAL breaks, and it deleted them.
            #
            # Measured: eGFR data spanning 12.3-109.2 with the standard CKD
            # cut-points "30,60,90" produced breaks 12.26, 60, 109.21. The 30 and
            # 90 thresholds were gone, four CKD stages collapsed into two bins, and
            # the 30 patients below 30 (stage 4/5) were merged with stage 3 -
            # silently. Prepending and appending keeps every threshold the user
            # asked for while still covering the data.
            # extend_to_data = FALSE keeps manual break points exactly as entered,
            # so values outside them fall outside every bin and are excluded. The
            # count is reported to the user in .run(); silently losing cases would
            # be the same class of defect as silently moving the break points.
            if (!is.null(breaks) && length(breaks) > 1 && isTRUE(extend_to_data)) {
                if (min(breaks) > min(x, na.rm = TRUE))
                    breaks <- c(min(x, na.rm = TRUE), breaks)
                if (max(breaks) < max(x, na.rm = TRUE))
                    breaks <- c(breaks, max(x, na.rm = TRUE))
                breaks <- unique(sort(breaks))
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
                    # Precision comes from the break spacing, never a fixed
                    # single decimal - see private$.autoDigits().
                    fmt <- paste0("%s%.", private$.autoDigits(breaks),
                                  "f, %.", private$.autoDigits(breaks), "f%s")
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
                        sprintf(fmt, left_bracket, breaks[i], breaks[i + 1], right_bracket)
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
                    # LETTERS runs out at 26; manual break points can ask for
                    # more, and LETTERS[27] is NA, which cut() turns into an
                    # unlabelled (dropped) category.
                    if (n_categories > length(LETTERS))
                        paste0("Category ", 1:n_categories)
                    else
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

            # Backstop: cut() accepts duplicate or NA labels without complaint
            # and factor() then MERGES the affected categories, so the analysis
            # would silently report fewer groups than break points. Never let a
            # non-unique label set reach cut().
            if (is.null(labels) || anyNA(labels) || anyDuplicated(labels) > 0)
                labels <- paste0("Category ", 1:n_categories)

            return(labels)
        },

        # Generate R code for reproducibility.
        #
        # The snippet re-derives the break points with the SAME calls the
        # backend makes, rather than printing them as decimal literals. Two
        # reasons. (1) The old snippet used a different derivation: for manual
        # breaks the backend prepends min(x) and appends max(x) unless
        # out-of-range exclusion is on, and the snippet did not, so "30, 60, 90"
        # on eGFR spanning 12.3-109.2 gave 4 categories on screen and 2
        # categories (35% of cases silently dropped to NA) in the copied code;
        # the mean+/-SD and median snippets could also fail outright. (2)
        # Printing the computed break points instead does not fix it: R's
        # sprintf("%.17g") and dput() are not round-trip exact for every double
        # on every platform (measured 1-ULP and 16-ULP errors here), and a
        # break point shifted by one ULP moves any observation lying exactly on
        # it into the neighbouring category - which is common for quantile
        # breaks, since those land on observed values.
        #
        # Variable names are backtick-quoted via jmvcore::composeTerm() and the
        # label and break literals are serialized with dput(), so names or
        # labels containing spaces, quotes or backslashes stay valid R.
        .generateRCode = function(varname, method, nbins, manual_breaks, sdmult,
                                  label_style, labels_used, newvarname,
                                  includelowest, rightclosed, ordered,
                                  exclude_oor, n_obs) {

            if (newvarname == "") {
                newvarname <- paste0(varname, "_cat")
            }

            varname <- jmvcore::composeTerm(varname)
            newvarname <- jmvcore::composeTerm(newvarname)

            lit <- function(v) paste(utils::capture.output(dput(v)), collapse = "\n")

            code <- paste0(
                "# Categorize continuous variable\n",
                "x <- data$", varname, "\n\n")

            has_classInt <- requireNamespace("classInt", quietly = TRUE)

            derivation <- switch(method,
                "equal" = paste0(
                    "# Equal-width intervals over the observed range\n",
                    "breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = ",
                    nbins + 1, ")\n"),
                "quantile" = paste0(
                    "# Sample quantiles at ", nbins, " equal probability steps\n",
                    "breaks <- unique(quantile(x, probs = seq(0, 1, length.out = ",
                    nbins + 1, "), na.rm = TRUE))\n"),
                "manual" = paste0(
                    "# Break points entered by hand\n",
                    "breaks <- sort(", lit(private$.parseManualBreaks(manual_breaks)), ")\n",
                    if (exclude_oor)
                        "# Kept exactly as entered: values outside them are not categorized\n"
                    else ""),
                "meansd" = paste0(
                    "# Mean and mean +/- ", sdmult, " SD, bounded by the data range.\n",
                    "# A boundary falling outside the observed range is dropped rather\n",
                    "# than folded against min/max, which would leave a sliver bin.\n",
                    "m <- mean(x, na.rm = TRUE)\n",
                    "s <- sd(x, na.rm = TRUE)\n",
                    "lo <- m - ", sdmult, " * s\n",
                    "hi <- m + ", sdmult, " * s\n",
                    "breaks <- c(min(x, na.rm = TRUE), m, max(x, na.rm = TRUE))\n",
                    "if (lo > min(x, na.rm = TRUE)) breaks <- c(breaks, lo)\n",
                    "if (hi < max(x, na.rm = TRUE)) breaks <- c(breaks, hi)\n"),
                "median" = paste0(
                    "# Median split, bounded by the data range\n",
                    "breaks <- c(min(x, na.rm = TRUE), median(x, na.rm = TRUE), max(x, na.rm = TRUE))\n"),
                "jenks" = if (has_classInt) paste0(
                    "# Fisher-Jenks natural breaks. style = 'fisher' is the exact\n",
                    "# optimum when it runs on the full data (largeN = Inf below);\n",
                    "# 'jenks' silently switches to a random subsample above\n",
                    "# 3000 observations and would give different breaks every run.\n",
                    "# Requires: install.packages('classInt')\n",
                    if (n_obs > private$.jenksExactMaxN) paste0(
                        "set.seed(20240101)  # ", n_obs,
                        " observations: classInt subsamples, so pin the seed\n",
                        "ci <- classInt::classIntervals(x[!is.na(x)], n = ", nbins,
                        ", style = 'fisher')\n")
                    else paste0(
                        "ci <- classInt::classIntervals(x[!is.na(x)], n = ", nbins,
                        ", style = 'fisher', largeN = Inf)\n"),
                    "breaks <- ci$brks\n")
                else paste0(
                    "# The 'classInt' package was not available, so the analysis fell\n",
                    "# back to quantile binning. install.packages('classInt') for\n",
                    "# true Fisher-Jenks natural breaks.\n",
                    "breaks <- unique(quantile(x, probs = seq(0, 1, length.out = ",
                    nbins + 1, "), na.rm = TRUE))\n"),
                paste0("breaks <- unique(quantile(x, probs = seq(0, 1, length.out = ",
                       nbins + 1, "), na.rm = TRUE))\n"))

            # The backend extends the break points to the data range for every
            # method unless out-of-range exclusion is on. It is a no-op wherever
            # the derivation already starts at min(x) and ends at max(x), but
            # emitting it unconditionally keeps the snippet identical to the
            # analysis instead of relying on that being true.
            extension <- if (exclude_oor) "" else paste0(
                "# Extend to cover the data, so no observation falls outside every bin\n",
                "if (min(breaks) > min(x, na.rm = TRUE)) breaks <- c(min(x, na.rm = TRUE), breaks)\n",
                "if (max(breaks) < max(x, na.rm = TRUE)) breaks <- c(breaks, max(x, na.rm = TRUE))\n")

            code <- paste0(code, derivation, extension, "breaks <- sort(unique(breaks))\n")

            labels_literal <- if (is.null(labels_used)) "NULL" else lit(as.character(labels_used))
            code <- paste0(
                code,
                "\n# Category labels (style: ", label_style, ")\n",
                "labels <- ", labels_literal, "\n")

            # Add cut command
            code <- paste0(code, "\ndata$", newvarname, " <- cut(\n",
                          "  x,\n",
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

            # Notices accumulate across run cycles unless the pane is reset
            # first; do it before anything can return early.
            self$results$notices$setContent("")

            # Input Validation ----
            if (is.null(self$options$var) || length(self$options$var) == 0) {
                todo <- "
                <div style='font-family: Arial, sans-serif; color: inherit;'>
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

            # Errors and notices are rendered as HTML in the `notices` item to
            # avoid the protobuf serialization failure triggered by dynamic
            # jmvcore::Notice objects (see
            # docs/NOTICE_TO_HTML_CONVERSION_GUIDE.md). `todo` keeps the welcome
            # text only, so a warning never appears under "Instructions".
            .errBox <- function(msg)
                paste0("<div style='padding: 15px; background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; color: inherit; border-radius: 5px;'><strong>Error:</strong> ", msg, "</div>")

            if (!(varname %in% names(self$data))) {
                self$results$notices$setContent(.errBox(jmvcore::format(
                    "Variable '{}' not found in dataset. Please select a valid variable from the data.",
                    htmltools::htmlEscape(varname))))
                return()
            }

            x <- self$data[[varname]]

            # Check if numeric
            if (!is.numeric(x)) {
                self$results$notices$setContent(.errBox(jmvcore::format(
                    "Variable '{}' is not numeric. Categorization requires a continuous numeric variable.",
                    htmltools::htmlEscape(varname))))
                return()
            }

            # Break points are always computed on the non-missing values -
            # .calculateBreaks() drops NA as its first statement either way.
            # The `excl` option controls the DENOMINATOR of the frequency table
            # instead (see below), which is the only place it can be observed.
            x_clean <- x[!is.na(x)]

            # Basic sanity check for variability
            if (sum(!is.na(x_clean)) < 2 || sd(x_clean, na.rm = TRUE) == 0) {
                self$results$notices$setContent(.errBox(jmvcore::format(
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
                # signif(), not round(x, 2): a fixed 2 decimals prints every
                # summary of a proportion, index or ratio as 0 or 0.01.
                "<strong>Range:</strong> ", signif(min(x, na.rm = TRUE), 4), " - ",
                signif(max(x, na.rm = TRUE), 4), "<br>",
                "<strong>Mean:</strong> ", signif(mean(x, na.rm = TRUE), 4), "<br>",
                "<strong>Median:</strong> ", signif(median(x, na.rm = TRUE), 4), "<br>",
                "<strong>SD:</strong> ", signif(sd(x, na.rm = TRUE), 4),
                "</div>"
            )
            self$results$summaryText$setContent(summary_html)

            # Calculate breaks ----
            method <- self$options$method
            nbins <- self$options$nbins
            manual_breaks <- self$options$breaks
            sdmult <- self$options$sdmult

            private$.checkpoint()
            # Only the manual method can have break points that do not span the
            # data; the computed methods build theirs from min(x)/max(x), so the
            # switch is deliberately ignored for them.
            exclude_oor <- isTRUE(self$options$excludeoutofrange) &&
                identical(method, "manual")

            breaks <- private$.calculateBreaks(
                x_clean,
                method,
                nbins,
                manual_breaks,
                sdmult,
                extend_to_data = !exclude_oor
            )

            # Enforce sorted unique breaks to avoid cut() failures
            if (!is.null(breaks)) {
                breaks <- sort(unique(breaks))
            }

            # Validate breaks with detailed error messages
            validation <- private$.validateBreaks(breaks, method)
            if (!validation$valid) {
                self$results$notices$setContent(.errBox(jmvcore::format(
                    "Break point validation failed: {}",
                    htmltools::htmlEscape(validation$message))))
                return()
            }

            # Cache the computed breaks in plot state so the render callback
            # (.plot) reuses them instead of recomputing from self$data /
            # self$options. Only simple, serializable values are stored.
            if (self$options$showplot) {
                self$results$plot$setState(list(
                    breaks      = breaks,
                    method      = method,
                    varname     = varname,
                    exclude_oor = exclude_oor
                ))
            }

            # Additional check for manual breaks
            if (method == "manual") {
                if (is.null(private$.parseManualBreaks(manual_breaks))) {
                    self$results$notices$setContent(.errBox(
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
                # Translucent tints composite to the previous pastel over a
                # white pane and stay readable on jamovi's dark theme, matching
                # .errBox above and the R-code box below. An opaque pastel with
                # pinned dark text renders as a bright island in dark mode.
                cfg <- switch(level,
                    STRONG_WARNING = list(bg = "rgba(255, 193, 7, 0.18)", border = "#ff9800", title = "Warning"),
                    WARNING        = list(bg = "rgba(255, 193, 7, 0.12)", border = "#ffc107", title = "Warning"),
                    INFO           = list(bg = "rgba(23, 162, 184, 0.12)", border = "#17a2b8", title = "Note"))
                paste0(
                    "<div style='padding: 12px 15px; margin: 6px 0; background-color: ", cfg$bg,
                    "; border-left: 4px solid ", cfg$border, "; color: inherit",
                    "; border-radius: 4px;'><strong>", cfg$title, ":</strong> ", msg, "</div>")
            }

            # WARNING: Jenks falls back to quantile
            if (method == "jenks" && !requireNamespace("classInt", quietly = TRUE)) {
                notice_html$jenksFallback <- .noticeBox("WARNING",
                    "Natural Breaks (Jenks) requires the 'classInt' package. Using quantile-based binning instead. Install classInt with install.packages('classInt') to enable true Jenks optimization.")
            }

            # WARNING: Jenks breaks computed on a subsample
            if (method == "jenks" && requireNamespace("classInt", quietly = TRUE) &&
                length(x_clean) > private$.jenksExactMaxN) {
                # classInt::classIntervals draws ceiling(samp_prop * nobs)
                # values but CAPS that at largeN, which is the 3000L passed in
                # .calculateBreaks(). So the sample is 10% only up to n = 30000
                # and shrinks in proportion after that (3% at n = 100000) -
                # report the count that is actually used, not a fixed share.
                n_subsample <- min(ceiling(0.1 * length(x_clean)), 3000L)
                notice_html$jenksSubsample <- .noticeBox("WARNING", jmvcore::format(
                    "Natural-breaks approximation: with {} observations the break points were computed on a random subsample of {} values (plus the minimum and maximum) rather than the full data, because the exact algorithm is too slow at this size. The subsample is drawn with a fixed seed, so repeated runs give the same break points, but they are an approximation to the optimum. Quantile binning uses every observation if exact boundaries matter.",
                    length(x_clean), n_subsample))
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

            # Invariant: one factor level per interval. cut() silently merges
            # categories whose labels collide, and the Range column below is
            # rebuilt from breaks[i]/breaks[i+1] on the assumption that level i
            # IS interval i - so a merge would print a range that does not match
            # the count beside it. .generateLabels() now guarantees unique
            # labels; this catches anything it cannot.
            if (nlevels(x_cat) != n_categories) {
                self$results$notices$setContent(.errBox(jmvcore::format(
                    "Category labels collided: {} break points define {} intervals but only {} distinct categories were produced, so two or more intervals would be merged into one row. The frequency table and the added variable would not match the break points, so no results are shown. Choose a different label style (Numbered or Lettered), or reduce the number of categories so the boundaries are further apart.",
                    length(breaks), n_categories, nlevels(x_cat))))
                return()
            }

            # Report every non-missing case that did NOT land in a category.
            # There are exactly two causes, and only the first used to be
            # reported:
            #  (1) "Out-of-range value exclusion" with manual breaks keeps the
            #      break points as entered, so values beyond them are dropped;
            #  (2) with "Lowest value in first bin" OFF, cut() leaves one outer
            #      break open - the lowest when intervals are right-closed, the
            #      highest when they are left-closed - so every observation
            #      exactly equal to that break is dropped. This happens for
            #      EVERY method, because the computed methods put breaks[1] at
            #      min(x) and breaks[length] at max(x), and it was silent.
            # Cause (2) is a boundary case, not an out-of-range case, so the
            # two are counted and worded separately.
            n_dropped <- sum(!is.na(x) & is.na(x_cat))
            if (n_dropped > 0) {
                # A value inside [min(breaks), max(breaks)] can only be dropped
                # by the open outer break; interior breaks always belong to one
                # side or the other.
                n_boundary <- sum(!is.na(x) & is.na(x_cat) &
                                  x >= min(breaks) & x <= max(breaks))
                n_outside <- n_dropped - n_boundary

                if (n_outside > 0) {
                    # Strict inequalities partition the strictly-outside cases
                    # exactly, so below + above == n_outside by construction.
                    below <- sum(!is.na(x) & is.na(x_cat) & x < min(breaks))
                    above <- n_outside - below
                    notice_html$outOfRange <- .noticeBox("WARNING", jmvcore::format(
                        "Excluded {} observation(s) ({}%) that fall outside the break points [{}, {}]: {} below and {} above. These are not counted in any category. Turn off 'Out-of-range value exclusion' to extend the outer breaks and keep every case.",
                        n_outside,
                        round(100 * n_outside / sum(!is.na(x)), 1),
                        base::format(min(breaks)), base::format(max(breaks)),
                        below, above))
                }

                if (n_boundary > 0) {
                    open_end <- if (isTRUE(self$options$rightclosed))
                        base::format(min(breaks)) else base::format(max(breaks))
                    notice_html$boundaryDropped <- .noticeBox("STRONG_WARNING", jmvcore::format(
                        "Boundary values not categorized: {} observation(s) ({}%) are exactly equal to {} and fall outside every interval, because 'Lowest value in first bin' is switched off. They are not counted in any category. Switch that option on to keep them.",
                        n_boundary,
                        round(100 * n_boundary / sum(!is.na(x)), 1),
                        open_end))
                }
            }

            # Clinical suitability checks ----
            # Check bin balance and minimum counts
            bin_counts <- table(x_cat, useNA = "no")
            n_valid_for_check <- sum(bin_counts)

            if (length(bin_counts) > 0 && n_valid_for_check > 0) {
                # STRONG_WARNING: Very small bins - absolute (< 5 cases) or
                # proportional (< 2% of the sample). The absolute test alone
                # misses slivers in large samples: a 6-of-300 artefact bin
                # created by a collapsed mean+/-SD boundary passed it.
                sliver_cut <- 0.02 * n_valid_for_check
                small_bins <- sum(bin_counts < 5 | bin_counts <= sliver_cut)
                if (small_bins > 0) {
                    notice_html$smallBins <- .noticeBox("STRONG_WARNING", jmvcore::format(
                        "Small bins detected: {} of {} bin(s) hold fewer than 5 observations or under 2% of the sample (smallest bin n = {}). Estimates within such groups are very imprecise and tests involving them have little power; consider reducing the number of categories or using quantile binning for more even group sizes.",
                        small_bins, length(bin_counts), min(bin_counts)))
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

            # STRONG_WARNING: a mean+/-SD boundary fell outside the data range
            if (method == "meansd" && length(private$.meansdDropped) > 0) {
                which_band <- paste(private$.meansdDropped, collapse = " and ")
                notice_html$meansdCollapse <- .noticeBox("STRONG_WARNING", jmvcore::format(
                    "Mean\u{00B1}SD boundary outside the data: the {} boundary (mean +/- {} SD) lies beyond the observed range, so that band cannot be formed and {} categories were created instead of the usual 4. This happens on skewed distributions (CRP, ferritin, tumour burden). Quantile or natural-breaks binning gives bands that always fall inside the data.",
                    which_band, sdmult, n_categories))
            }

            # WARNING: Outlier sensitivity for mean +/- SD method
            if (method == "meansd") {
                # Check if data has extreme outliers using IQR method
                q1 <- quantile(x_clean, 0.25)
                q3 <- quantile(x_clean, 0.75)
                iqr <- q3 - q1
                outliers <- sum(x_clean < (q1 - 3 * iqr) | x_clean > (q3 + 3 * iqr))

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
                    # No round() here: rounding to 4 DECIMAL places printed every
                    # break point of a sub-unit variable as 0. The column is
                    # type number, so jamovi formats it to significant digits.
                    value = breaks[i]
                ))
            }

            # Populate frequency table ----
            freqTable <- self$results$freqTable

            # Calculate frequencies. Break points come from the non-missing
            # values, but x_cat carries NA wherever x was NA (and wherever a
            # value fell outside the breaks), so those are counted separately.
            freq <- table(x_cat, useNA = "ifany")

            # Separate valid, genuinely missing, and out-of-range counts.
            # sum(is.na(x_cat)) alone cannot tell a true NA in the source
            # variable from a value dropped for falling outside the break
            # points, so excluded cases used to be reported under a row
            # labelled "Missing" while the Variable Summary said N (missing) = 0.
            n_total_obs <- length(x_cat)
            n_valid_obs <- sum(!is.na(x_cat))
            n_missing_obs <- sum(is.na(x))
            n_oor_obs <- sum(!is.na(x) & is.na(x_cat))

            # `excl` (Missing-value exclusion, default TRUE) chooses the
            # denominator: percentages over the categorised cases only, or over
            # every row so that the missing and out-of-range rows carry their
            # own share and the column no longer sums to 100% on its own.
            pct_denom <- if (isTRUE(self$options$excl)) n_valid_obs else n_total_obs

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
                    # Same precision as the auto category labels - a fixed
                    # 2 decimals contradicted them on both sub-unit and
                    # large-magnitude scales.
                    range_fmt <- paste0("%s%.", private$.autoDigits(breaks),
                                        "f, %.", private$.autoDigits(breaks), "f%s")
                    range_str <- sprintf(range_fmt, left_bracket, breaks[cat_idx], breaks[cat_idx + 1], right_bracket)
                } else {
                    range_str <- as.character(cat_name)
                }

                # Calculate percentages based on valid observations only.
                # Guard the denominator: manual breaks can leave zero valid
                # categorized observations, which would give NaN/Inf.
                if (pct_denom > 0) {
                    pct_val <- freq[i] / pct_denom
                    cum_pct_val <- cumsum_freq[row_idx] / pct_denom
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

            # Add a row for genuinely missing source values
            if (n_missing_obs > 0) {
                freqTable$addRow(rowKey = row_idx, values = list(
                    category = "Missing",
                    range = "NA",
                    n = as.integer(n_missing_obs),
                    percent = if (isTRUE(self$options$excl) || pct_denom <= 0)
                        NaN else n_missing_obs / pct_denom,
                    cumPercent = NaN
                ))
                row_idx <- row_idx + 1
            }

            # Add a separate row for values that were not categorised, so they
            # can never be confused with true missingness. Values sitting
            # exactly ON the open outer break are NOT outside the break points,
            # so the row is worded by the cause that actually applies (see the
            # notice block above for the same split).
            if (n_oor_obs > 0) {
                n_boundary_obs <- sum(!is.na(x) & is.na(x_cat) &
                                      x >= min(breaks) & x <= max(breaks))
                if (n_boundary_obs == 0) {
                    oor_category <- "Out of range"
                    oor_range <- "outside the break points"
                } else if (n_boundary_obs == n_oor_obs) {
                    oor_category <- "Not categorized"
                    oor_range <- "on the open outer break point"
                } else {
                    oor_category <- "Not categorized"
                    oor_range <- "outside or on the open outer break point"
                }
                freqTable$addRow(rowKey = row_idx, values = list(
                    category = oor_category,
                    range = oor_range,
                    n = as.integer(n_oor_obs),
                    percent = if (isTRUE(self$options$excl) || pct_denom <= 0)
                        NaN else n_oor_obs / pct_denom,
                    cumPercent = NaN
                ))
                row_idx <- row_idx + 1
            }

            # Name for the categorised variable, shared by the dataset column
            # and the generated R snippet so the two cannot disagree.
            new_name <- trimws(self$options$newvarname)
            if (!nzchar(new_name)) new_name <- paste0(varname, "_cat")

            # Add categorized variable to data ----
            if (self$options$addtodata && self$results$addtodata$isNotFilled()) {
                # Hand jamovi the FACTOR itself. as.character() used to be
                # applied here, but jmvcore's Output$asProtoBuf re-derives the
                # levels of a character column with as.factor(), which sorts
                # them ALPHABETICALLY: the default "auto" labels came back as
                # "(38.0, 49.0]" < "(49.0, 62.0]" < "(62.0, 73.0]" <
                # "[25.0, 38.0]" because "(" sorts before "[", semantic labels
                # as High < Low < Medium-High < Medium-Low, and numbered labels
                # as 1, 10, 11, 12, 2, ... A factor falls through to the level
                # branch untouched, so levels(x_cat) - the cut() interval order
                # - is what reaches the dataset, which is what the "Ordered
                # factor" option promises.

                # Apply the user's variable name and let the `ordered` option
                # decide the measure type of the delivered column. Without this
                # the column was always called <var>_cat at measure type
                # nominal, so both options made promises they did not keep.
                # set() re-initialises keys/titles/measure types exactly as
                # Output$initialize does, then setRowNums/setValues refill it;
                # guarded so a jmvcore change cannot take the analysis down.
                try(self$results$addtodata$set(
                        keys = 1L,
                        titles = new_name,
                        descriptions = paste0("Categorized version of ", varname,
                                              " using ", method, " method"),
                        measureTypes = if (isTRUE(self$options$ordered)) "ordinal" else "nominal"),
                    silent = TRUE)

                # Row numbers must be the ORIGINAL dataset rows. seq_along() of
                # the filtered frame shifted every value up by however many rows
                # a row filter removed above it, writing categories to the wrong
                # patients with no visible symptom.
                self$results$addtodata$setRowNums(rownames(self$data))
                self$results$addtodata$setValues(x_cat)
            }

            # Generate R code ----
            if (self$options$showcode) {
                code <- private$.generateRCode(
                    varname, method, nbins, manual_breaks, sdmult,
                    self$options$labels, labels_result, new_name,
                    self$options$includelowest, self$options$rightclosed,
                    self$options$ordered, exclude_oor, length(x_clean)
                )

                code_html <- paste0(
                    "<div style='background-color: rgba(138, 155, 172, 0.06); color: inherit; padding: 15px; ",
                    "border-radius: 4px; font-family: monospace; white-space: pre-wrap;'>",
                    htmltools::htmlEscape(code),
                    "</div>"
                )
                self$results$rcode$setContent(code_html)
            }

            # INFO: Analysis complete with methodological note
            notice_html$analysisComplete <- .noticeBox("INFO", jmvcore::format(
                "Categorization completed: {} observations placed into {} groups using {} method. Note: Categorization reduces statistical power and may obscure dose-response relationships (Altman & Royston, BMJ 2006;332:1080). Continuous analyses are generally preferred unless there is strong clinical justification.",
                n_valid_obs, n_categories, self$options$method))

            # Render notices in priority order: STRONG_WARNING -> WARNING -> INFO
            # 'outOfRange' is a data-loss warning and was missing here, which
            # silently discarded the only feedback the exclusion feature has.
            priority_order <- c('outOfRange', 'boundaryDropped', 'smallBins', 'binImbalance', 'meansdCollapse',
                                'jenksSubsample', 'jenksFallback', 'labelMismatch',
                                'binCollapse', 'outlierSensitivity', 'analysisComplete')
            rendered <- character()
            # Anything not listed above still gets rendered, at the end. A
            # notice that was built and then dropped because its name was
            # missing from the list is how the out-of-range warning - the only
            # feedback that feature has - went unseen.
            for (name in c(priority_order, setdiff(names(notice_html), priority_order))) {
                if (!is.null(notice_html[[name]])) {
                    rendered <- c(rendered, notice_html[[name]])
                }
            }
            if (length(rendered) > 0) {
                self$results$notices$setContent(paste(rendered, collapse = ""))
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
                # The fallback used to take the default extend_to_data = TRUE,
                # so with manual breaks and out-of-range exclusion on it drew
                # boundary lines at min(x)/max(x) that the analysis had
                # deliberately not used, contradicting both tables.
                exclude_oor <- isTRUE(self$options$excludeoutofrange) &&
                    identical(self$options$method, "manual")

                breaks <- private$.calculateBreaks(
                    x[!is.na(x)],
                    self$options$method,
                    self$options$nbins,
                    self$options$breaks,
                    self$options$sdmult,
                    extend_to_data = !exclude_oor
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
                # ggtheme is jamovi's theme-aware ggplot theme; theme_minimal()
                # drew black titles and grey axis text onto the transparent png,
                # which is unreadable on the dark theme.
                ggtheme +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                    plot.subtitle = ggplot2::element_text(hjust = 0.5)
                )

            # Add break point labels
            break_labels <- data.frame(
                x = breaks,
                y = max(ggplot2::ggplot_build(plot)$data[[1]]$density, na.rm = TRUE) * 1.05,
                label = sprintf(paste0("%.", private$.autoDigits(breaks), "f"), breaks)
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
