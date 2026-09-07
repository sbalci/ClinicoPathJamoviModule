#' @title Lollipop Charts for Categorical Data Visualization
#' @description
#' Creates comprehensive lollipop charts for categorical data visualization with 
#' emphasis on clinical applications. Lollipop charts are particularly effective 
#' for displaying categorical data with a focus on individual values, making them 
#' ideal for patient timelines, treatment outcomes, biomarker levels, and 
#' comparative clinical assessments.
#' 
#' @details
#' The lollipop chart function is designed specifically for clinical research 
#' applications where categorical data visualization with emphasis on individual 
#' values is crucial. Unlike bar charts, lollipop charts reduce ink-to-data ratio 
#' and provide cleaner visualization for sparse data or when highlighting specific 
#' categories.
#' 
#' Key features:
#' - Flexible orientation (vertical/horizontal)
#' - Advanced sorting options (by value, alphabetical)
#' - Clinical color schemes and themes
#' - Highlighting capabilities for specific categories
#' - Statistical summary integration
#' - Professional publication-ready appearance
#' 
#' Common clinical applications:
#' - Patient timeline visualization
#' - Biomarker level comparisons
#' - Treatment outcome rankings
#' - Survey response visualization
#' - Quality metric displays
#' - Diagnostic test results
#' 
#' @examples
#' \dontrun{
#' data("clinical_lab_data", package = "ClinicoPath")
#'
#' # Mean haemoglobin by treatment group
#' result <- lollipop(
#'   data = clinical_lab_data,
#'   dep = "hemoglobin",
#'   group = "treatment_group",
#'   highlight = NULL
#' )
#'
#' # Horizontal chart, largest value at the top, values labelled
#' result <- lollipop(
#'   data = clinical_lab_data,
#'   dep = "albumin",
#'   group = "disease_severity",
#'   sortBy = "value_desc",
#'   orientation = "horizontal",
#'   showValues = TRUE,
#'   highlight = NULL
#' )
#'
#' # Highlight one hospital, with a reference line at the mean
#' result <- lollipop(
#'   data = clinical_lab_data,
#'   dep = "platelet_count",
#'   group = "hospital",
#'   useHighlight = TRUE,
#'   highlight = "Hospital A",
#'   showMean = TRUE
#' )
#' }
#' 
#' @importFrom R6 R6Class
#' @importFrom jmvcore .
#' @return An \code{R6} class generator object for the \code{lollipopClass} backend; used internally by the jamovi analysis wrapper and not called directly.

lollipopClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lollipopClass",
    inherit = lollipopBase,
    private = list(

        # Notice collection helpers. A single Preformatted (plain-text) output item:
        # avoids BOTH the jmvcore::Notice serialization error from
        # self$results$insert(999, Notice) AND any HTML in notices (project convention:
        # notice content must be plain text). ====
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type,
                title = title,
                content = content
            )
            # Render immediately so early-return validation aborts still display the notice
            private$.renderNotices()
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                return()
            }

            # Plain text only - notices avoid HTML by project convention; the Preformatted
            # output item renders this literally (no markup, no injection surface).
            blocks <- vapply(private$.noticeList, function(notice) {
                prefix <- switch(notice$type,
                    ERROR          = "ERROR: ",
                    STRONG_WARNING = "IMPORTANT WARNING: ",
                    WARNING        = "WARNING: ",
                    "")
                paste0(prefix, notice$title, "\n", notice$content)
            }, character(1))

            self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
        },

        # Initialize results and validate dependencies
        .init = function() {
            # Reset notices for new analysis
            private$.noticeList <- list()

            # Wire user-controlled plot dimensions to the Image output
            if (!is.null(self$options$width) && !is.null(self$options$height)) {
                self$results$plot$setSize(self$options$width, self$options$height)
            }

            # The summary table has a fixed row set, so build the skeleton (rowKey
            # + statistic label) here; .populateSummary() then only fills `value`.
            # Placed before the missing-package early return below so every rowKey
            # exists on every path (setRow() on a missing rowKey aborts the run).
            summaryTable <- self$results$summary
            # Under aggregation each plotted point is a group summary, not an
            # observation, so "Number of Observations" would report 4 for a
            # 20-row dataset. Name the row for what the number actually counts.
            summaryTable$addRow(rowKey = "n_obs", values = list(
                statistic = if (identical(self$options$aggregation, "none"))
                    .("Number of Observations") else .("Number of Plotted Points")))
            summaryTable$addRow(rowKey = "n_groups", values = list(
                statistic = .("Number of Groups")))
            summaryTable$addRow(rowKey = "mean", values = list(
                statistic = .("Mean Value")))
            summaryTable$addRow(rowKey = "median", values = list(
                statistic = .("Median Value")))
            summaryTable$addRow(rowKey = "sd", values = list(
                statistic = .("Standard Deviation")))
            summaryTable$addRow(rowKey = "range", values = list(
                statistic = .("Value Range")))
            summaryTable$addRow(rowKey = "highest", values = list(
                statistic = .("Highest Value Group")))
            summaryTable$addRow(rowKey = "lowest", values = list(
                statistic = .("Lowest Value Group")))

            # Check for required packages
            missing_packages <- c()
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                missing_packages <- c(missing_packages, "ggplot2")
            }

            if (length(missing_packages) > 0) {
                error_msg <- jmvcore::format(
                    .("The lollipop chart needs the following R packages, which are not installed: {packages}. Install them with install.packages() and re-run the analysis."),
                    packages = paste(missing_packages, collapse = ", "))

                private$.addNotice('ERROR', .("Missing Required Packages"), error_msg)
                return()
            }
            
            # Initialize with welcome message if no variables selected
            if (is.null(self$options$dep) || is.null(self$options$group)) {
                welcome_msg <- paste0("
                <div class='alert alert-info'>
                <h4>", .("Welcome to Lollipop Chart Analysis"), "</h4>
                <p>", .("This function creates lollipop charts for categorical data visualization with clinical applications."), "</p>
                
                <h5>", .("Required inputs:"), "</h5>
                <ul>
                <li><strong>", .("Dependent Variable"), "</strong>: ", .("Numeric values (biomarker levels, scores, measurements)"), "</li>
                <li><strong>", .("Grouping Variable"), "</strong>: ", .("Categories (patient IDs, treatments, conditions)"), "</li>
                </ul>
                
                <h5>", .("Key features:"), "</h5>
                <ul>
                <li><strong>Flexible Layout</strong>: Vertical or horizontal orientation</li>
                <li><strong>Smart Sorting</strong>: Order by value, alphabetical, or original order</li>
                <li><strong>Highlighting</strong>: Emphasize specific categories or patients</li>
                <li><strong>Clinical Themes</strong>: Professional color schemes and layouts</li>
                <li><strong>Statistical Integration</strong>: Summary statistics and reference lines</li>
                </ul>
                
                <h5>", .("Data Handling Notes:"), "</h5>
                <ul>
                <li>Rows with missing values (NA) in the selected variables will be automatically removed.</li>
                <li>If your data has multiple rows per group, use the <strong>Data Aggregation</strong> option (Mean/Median/Sum) to avoid over-plotting.</li>
                </ul>

                <h5>Clinical applications:</h5>
                <ul>
                <li>Patient timeline visualization (days to event, treatment progression)</li>
                <li>Biomarker level comparisons across patients</li>
                <li>Treatment outcome rankings</li>
                <li>Survey response visualization</li>
                <li>Quality metric displays</li>
                <li>Diagnostic test result comparisons</li>
                </ul>
                
                <h5>Advantages over bar charts:</h5>
                <ul>
                <li>Reduced visual clutter (lower ink-to-data ratio)</li>
                <li>Better for sparse data or many categories</li>
                <li>Emphasizes individual data points</li>
                <li>", .("Professional appearance for publications"), "</li>
                </ul>
                </div>
                ")
                
                self$results$todo$setContent(welcome_msg)
                
                # Hide results until data is provided
                self$results$summary$setVisible(FALSE)
                self$results$plot$setVisible(FALSE)
            }
        },
        
        .run = function() {
            private$.noticeList <- list()

            # Early exits for missing data or variables
            if (is.null(self$data) || nrow(self$data) == 0) {
                return()
            }
            
            if (is.null(self$options$dep) || is.null(self$options$group)) {
                return()
            }
            
            # Hide welcome message and show results
            self$results$todo$setVisible(FALSE)
            self$results$summary$setVisible(TRUE)
            self$results$plot$setVisible(TRUE)

            # Main analysis pipeline with comprehensive error handling
            tryCatch({
                # Checkpoint before expensive data cleaning
                private$.checkpoint()
                
                # Prepare and validate data
                data <- private$.cleanData()
                if (is.null(data)) return()
                
                # Checkpoint before statistical calculations
                private$.checkpoint()
                
                # Calculate summary statistics
                summary_stats <- private$.calculateSummary(data)
                private$.populateSummary(summary_stats)

                # Check for potential issues and warnings
                private$.checkForMisuseAndWarnings(data, summary_stats)

                # Add note about conditional coloring if enabled
                if (self$options$conditionalColor) {
                    private$.addNotice(
                        'INFO',
                        .("Conditional Coloring"),
                        jmvcore::format(
                            .("Values above {thr} are drawn in orange and the rest in blue, so colour encodes the threshold rather than the group."),
                            thr = base::format(self$options$colorThreshold, digits = 3))
                    )

                    # Conditional coloring takes precedence over category highlighting
                    if (self$options$useHighlight) {
                        private$.addNotice(
                            'INFO',
                            .("Highlighting Ignored"),
                            .("Conditional colouring is active, so the highlighted category is not drawn differently; turn conditional colouring off to use highlighting.")
                        )
                    }
                }

                # Generate and display clinical summary
                clinical_summary <- private$.generateClinicalSummary(summary_stats, self$options$dep, self$options$group)
                self$results$todo$setContent(clinical_summary)
                self$results$todo$setVisible(TRUE)
                
                # Checkpoint before plot data preparation
                private$.checkpoint()
                
                # Save plot data for rendering
                private$.savePlotData(data)

                # Render notices at the end of the run. .renderNotices() sets the
                # content to "" for an empty queue, so a run that emits zero notices
                # clears any stale text left over from a previous run.
                private$.renderNotices()

            }, error = function(e) {
                msg_html <- htmltools::htmlEscape(e$message)
                error_msg <- paste0(
                    "<div class='alert alert-danger'>",
                    "<h4>Analysis Error</h4>",
                    "<p><strong>Error:</strong> ", msg_html, "</p>",
                    "<p>Please check your data and variable selections.</p>",
                    "</div>"
                )
                self$results$todo$setContent(error_msg)
                self$results$todo$setVisible(TRUE)

                # Also flush the notice queue on the error path so prior notices
                # do not linger under an error message.
                private$.renderNotices()
            })
        },
        
        # The level to highlight, or NULL when highlighting is off or unset.
        # .cleanData() and .plot() both need this and used to carry identical
        # copies of the expression; one definition keeps them from drifting.
        .activeHighlight = function() {
            if (isTRUE(self$options$useHighlight) &&
                !is.null(self$options$highlight) &&
                !identical(self$options$highlight, ""))
                self$options$highlight
            else
                NULL
        },

        # Comprehensive data cleaning and validation
        .cleanData = function() {
            # Extract variables
            dep_var <- self$options$dep
            group_var <- self$options$group
            
            # Check if variables exist in data
            missing_vars <- setdiff(c(dep_var, group_var), names(self$data))
            if (length(missing_vars) > 0) {
                jmvcore::reject(.("Variables not found in data: {missing}"), missing = paste(missing_vars, collapse = ", "))
            }
            
            if (identical(dep_var, group_var))
                jmvcore::reject(.("Select different dependent and grouping variables."))

            # Select and clean data
            data <- self$data[c(dep_var, group_var)]
            
            # Checkpoint before expensive data validation and conversion
            private$.checkpoint(flush = FALSE)  # Only poll for changes, don't push results yet
            
            # Validate dependent variable (must be numeric).
            #
            # jmvcore::toNumeric() is NOT a coercion function: it unwraps a jamovi
            # `values` attribute and otherwise returns its argument untouched. On a
            # plain character or factor column it is a no-op, so is.na() was all
            # FALSE and this guard could never fire - the text column simply flowed
            # on into aggregate()/mean() and failed somewhere far less obvious.
            # Coerce for real, then judge.
            dep_raw <- data[[dep_var]]
            dep_data <- jmvcore::toNumeric(dep_raw)
            if (!is.numeric(dep_data))
                dep_data <- suppressWarnings(as.numeric(as.character(dep_data)))
            if (all(is.na(dep_data))) {
                jmvcore::reject(.("Dependent variable must be numeric (continuous variable)."))
            }
            data[[dep_var]] <- dep_data

            # complete.cases() below follows is.na(), which is TRUE for NaN but
            # FALSE for Inf, so an infinite value would survive into the axis
            # range, the mean and the "highest group" claim.
            n_before_finite <- nrow(data)
            data <- data[is.finite(data[[dep_var]]) | is.na(data[[dep_var]]), , drop = FALSE]
            n_nonfinite <- n_before_finite - nrow(data)
            if (n_nonfinite > 0) {
                private$.addNotice(
                    'WARNING',
                    .("Non-finite Values Removed"),
                    jmvcore::format(
                        .("{n} row(s) held an infinite value for the dependent variable and were removed; an infinite value usually means a division by zero or an out-of-range entry."),
                        n = n_nonfinite)
                )
            }
            
            # Validate grouping variable
            group_data <- data[[group_var]]
            if (is.character(group_data)) {
                data[[group_var]] <- factor(group_data)
            } else if (!is.factor(group_data)) {
                data[[group_var]] <- factor(group_data)
            }
            
            # Check number of groups.
            #
            # Count only levels that actually carry data: unique() treats NA as a
            # value, so one real category plus any missing value scored 2 and
            # sailed past this guard, leaving a single-lollipop "comparison" after
            # the complete-case filter below.
            n_groups <- length(unique(data[[group_var]][!is.na(data[[group_var]])]))
            if (n_groups < 2) {
                jmvcore::reject(.("Grouping variable must have at least 2 different categories."))
            }

            if (n_groups > 50) {
                private$.addNotice(
                    'WARNING',
                    .("Many Group Levels"),
                    jmvcore::format(
                        .("The grouping variable has {n} levels, more than a single chart can label legibly; consider pooling categories or splitting the chart."),
                        n = n_groups)
                )
            }
            
            # Checkpoint before potentially expensive missing data removal
            private$.checkpoint(flush = FALSE)
            
            # Remove rows with missing values
            complete_before <- nrow(data)
            data <- data[complete.cases(data), ]
            data <- droplevels(data)
            complete_after <- nrow(data)
            
            if (complete_after == 0) {
                jmvcore::reject(.("No complete cases found. Please check for missing values in selected variables."))
            }
            
            if (complete_after < complete_before) {
                n_removed <- complete_before - complete_after
                pct_removed <- round(100 * n_removed / complete_before, 1)
                private$.addNotice(
                    'WARNING',
                    .("Missing Data Removed"),
                    jmvcore::format(
                        .("{n} rows ({pct}%) had a missing value in the selected variables and were removed before plotting."),
                        n = n_removed,
                        pct = pct_removed)
                )
            }
            
            # Check minimum data requirements
            if (nrow(data) < 2) {
                jmvcore::reject(.("At least 2 complete observations are required for lollipop chart analysis."))
            }
            
            if (nlevels(data[[group_var]]) < 2)
                jmvcore::reject(.("At least 2 groups with complete observations are required."))

            # Warn about a highlight level that is not in the data. The level
            # itself is NOT carried out of here: .plot() re-derives it from the
            # options and degrades correctly via %in%, so the local assignment
            # this block used to end with was dead.
            highlight_level <- private$.activeHighlight()
            if (!is.null(highlight_level) && !highlight_level %in% data[[group_var]]) {
                private$.addNotice(
                    'WARNING',
                    .("Highlight Level Not Found"),
                    jmvcore::format(
                        .("The level {level} does not occur in the grouping variable, so nothing is highlighted; pick a level that is present in the data."),
                        level = highlight_level)
                )
            }

            # CRITICAL FIX: Check for duplicates and aggregate if requested
            # Without aggregation, multiple rows per group will over-plot
            group_counts <- table(data[[group_var]])
            has_duplicates <- any(group_counts > 1)

            if (has_duplicates && self$options$aggregation == "none") {
                max_count <- max(group_counts)
                groups_with_dups <- names(group_counts[group_counts > 1])
                private$.addNotice(
                    'STRONG_WARNING',
                    .("Duplicate Groups Detected"),
                    jmvcore::format(
                        .("Some groups hold several observations (up to {n}, for example {groups}), and with no aggregation every one of them is drawn on the same stem, so the chart over-plots and hides how many points each lollipop stands for; choose Mean, Median or Sum under Data aggregation."),
                        n = max_count,
                        groups = paste(utils::head(groups_with_dups, 5), collapse = ", "))
                )
            }

            source_n <- nrow(data)
            source_group_counts <- group_counts

            # Within-group spread has to be measured on the RAW rows: once
            # .aggregateData() collapses each group to a single point the
            # dispersion it discarded is unrecoverable, and the chart shows no
            # error bars to hint at it. Carry the largest within-group SD out
            # with the data so .checkForMisuseAndWarnings() can disclose it.
            source_max_within_sd <- if (any(group_counts > 1)) {
                sds <- tapply(data[[dep_var]], data[[group_var]], stats::sd)
                sds <- sds[is.finite(sds)]
                if (length(sds)) max(sds) else NA_real_
            } else NA_real_

            # Apply aggregation if requested
            if (self$options$aggregation != "none") {
                data <- private$.aggregateData(data, dep_var, group_var, self$options$aggregation)
            }

            # Apply sorting if requested
            data <- private$.applySorting(data, dep_var, group_var)
            
            # Add column names for easier reference
            colnames(data) <- c("dependent", "group")
            attr(data, "source_n") <- source_n
            attr(data, "source_group_counts") <- source_group_counts
            attr(data, "source_max_within_sd") <- source_max_within_sd
            
            return(data)
        },
        
        # Apply sorting based on user selection
        .applySorting = function(data, dep_var, group_var) {
            sort_method <- self$options$sortBy

            # CRITICAL FIX: Must relevel factor, not just reorder rows
            # ggplot2 uses factor levels order, not data frame row order
            if (sort_method == "value_asc") {
                # Sort by ascending values
                data <- data[order(data[[dep_var]]), ]
                data[[group_var]] <- private$.relevelSorted(data[[group_var]])
            } else if (sort_method == "value_desc") {
                # Sort by descending values
                data <- data[order(-data[[dep_var]]), ]
                data[[group_var]] <- private$.relevelSorted(data[[group_var]])
            } else if (sort_method == "group_alpha") {
                # Sort alphabetically by group
                data <- data[order(as.character(data[[group_var]])), ]
                data[[group_var]] <- private$.relevelSorted(data[[group_var]])
            }
            # "original" keeps the original order (no releveling needed)

            return(data)
        },

        # Relevel a grouping factor so ggplot draws it in the sorted row order.
        #
        # ggplot lays a discrete scale out in level order: left-to-right on x
        # (vertical chart) but BOTTOM-to-top on y (horizontal chart). Using the
        # row order verbatim therefore rendered a horizontal "descending" chart
        # upside down - the largest value sat at the bottom and the chart read
        # ascending from the top. Reverse the levels for the horizontal layout so
        # the first sorted row is the TOP lollipop in both orientations.
        .relevelSorted = function(group_values) {
            lvls <- unique(as.character(group_values))
            if (identical(self$options$orientation, "horizontal"))
                lvls <- rev(lvls)
            factor(as.character(group_values), levels = lvls)
        },

        # Aggregate data by group to prevent over-plotting
        .aggregateData = function(data, dep_var, group_var, method) {
            # stats::aggregate, one summary per group
            agg_func <- switch(method,
                "mean" = function(x) mean(x, na.rm = TRUE),
                "median" = function(x) median(x, na.rm = TRUE),
                "sum" = function(x) sum(x, na.rm = TRUE),
                function(x) mean(x, na.rm = TRUE)  # Default to mean
            )

            # Aggregate by group
            agg_data <- aggregate(
                data[[dep_var]],
                by = list(group = data[[group_var]]),
                FUN = agg_func
            )

            # Rename columns to match original
            colnames(agg_data) <- c(group_var, dep_var)

            # Ensure group column is factor with same levels
            agg_data[[group_var]] <- factor(agg_data[[group_var]], levels = levels(data[[group_var]]))
            
            # Reorder to [dep, group] to match .cleanData expectation
            agg_data <- agg_data[, c(dep_var, group_var)]

            return(agg_data)
        },

        # Calculate summary statistics
        .calculateSummary = function(data) {
            summary_stats <- list()
            
            # Basic data information
            summary_stats$n_observations <- nrow(data)
            summary_stats$source_n <- attr(data, "source_n")
            if (is.null(summary_stats$source_n)) summary_stats$source_n <- nrow(data)
            summary_stats$n_groups <- length(unique(data$group))
            
            # Dependent variable statistics
            dep_data <- data$dependent
            summary_stats$dep_mean <- mean(dep_data, na.rm = TRUE)
            summary_stats$dep_median <- median(dep_data, na.rm = TRUE)
            summary_stats$dep_sd <- sd(dep_data, na.rm = TRUE)
            summary_stats$dep_min <- min(dep_data, na.rm = TRUE)
            summary_stats$dep_max <- max(dep_data, na.rm = TRUE)
            summary_stats$dep_range <- summary_stats$dep_max - summary_stats$dep_min
            
            # Checkpoint before the group scan, which can be expensive on large data
            private$.checkpoint(flush = FALSE)

            # Highest / lowest group.
            #
            # These used to be which.max/which.min over the GROUP MEANS while
            # "Value Range" two rows above reported the raw extremes - two bases
            # in one table, and with aggregation off they could disagree with the
            # chart itself. Group A holding {1, 100} and group B holding {60, 60}
            # draws its tallest lollipop in A, but the mean picked B. Reading the
            # extremes off the PLOTTED values instead makes the table agree with
            # the picture, and is identical under aggregation (one row per group,
            # so the plotted value IS the group summary).
            #
            # which.max also broke ties silently by position; name every tied
            # group instead, so equal values are visible rather than arbitrated.
            .tied_groups <- function(values, groups, target) {
                hits <- unique(as.character(groups[!is.na(values) & values == target]))
                paste(hits, collapse = ", ")
            }
            summary_stats$groups_with_highest <-
                .tied_groups(dep_data, data$group, summary_stats$dep_max)
            summary_stats$groups_with_lowest <-
                .tied_groups(dep_data, data$group, summary_stats$dep_min)

            return(summary_stats)
        },
        
        # Generate clinical summary for easier interpretation
        .generateClinicalSummary = function(summary_stats, dep_var, group_var) {
            highest_safe <- htmltools::htmlEscape(as.character(summary_stats$groups_with_highest))
            lowest_safe  <- htmltools::htmlEscape(as.character(summary_stats$groups_with_lowest))
            summary_html <- paste0(
                "<div class='alert alert-success'>",
                "<h5>", .("Clinical Summary"), "</h5>",
                "<p><strong>", .("Analysis Overview"), ":</strong> ",
                .("This analysis compared"), " <strong>", summary_stats$n_observations, "</strong> ",
                .("plotted values across"), " <strong>", summary_stats$n_groups, "</strong> ",
                .("groups"), ".</p>",

                "<p><strong>", .("Key Findings"), ":</strong></p>",
                "<ul>",
                "<li>", .("Mean value"), ": <strong>", round(summary_stats$dep_mean, 2), "</strong> ",
                "(", .("Standard Deviation"), " = ", round(summary_stats$dep_sd, 2), ")</li>",
                "<li>", .("Value range"), ": ", round(summary_stats$dep_min, 2), " - ", round(summary_stats$dep_max, 2), "</li>",
                "<li>", .("Highest values found in"), ": <strong>", highest_safe, "</strong></li>",
                "<li>", .("Lowest values found in"), ": <strong>", lowest_safe,  "</strong></li>",
                "</ul>",
                
                "<p><strong>", .("Clinical Interpretation"), ":</strong> ",
                .("These are descriptive summaries of the plotted values. When aggregation is selected, each point represents a group summary. Clinical importance requires a prespecified threshold and appropriate comparison; it cannot be inferred from the range or standard deviation."), "</p>",
                "</div>"
            )
            
            return(summary_html)
        },
        
        # Advanced misuse detection and contextual warnings
        .checkForMisuseAndWarnings = function(data, summary_stats) {
            # Thinly replicated groups.
            #
            # The bare test n_groups > source_n / 3 is satisfied by ANY
            # one-row-per-category dataset - which is the canonical lollipop
            # input, not a problem - so it told every correct chart to "use a
            # different visualization". Only complain when the data actually
            # carries replicate observations and still averages under three per
            # group, i.e. when the group means really are thinly estimated.
            # A two-group chart with thin data is already covered by the small
            # sample and unbalanced notices, and "The 2 groups hold 5
            # observations" reads oddly next to them.
            has_replicates <- summary_stats$source_n > summary_stats$n_groups
            if (has_replicates && summary_stats$n_groups >= 3 &&
                summary_stats$n_groups > summary_stats$source_n / 3) {
                private$.addNotice(
                    'WARNING',
                    .("Thinly Replicated Groups"),
                    jmvcore::format(
                        .("The {groups} groups hold {raw} observations between them, fewer than three per group on average, so each plotted point rests on very little data; consider pooling categories or reporting the individual observations."),
                        groups = summary_stats$n_groups,
                        raw = summary_stats$source_n)
                )
            }

            # Outliers.
            #
            # The previous test, range > 5 * SD, is not scale free: the expected
            # range/SD ratio of a normal sample GROWS with n and crosses 5 near
            # n = 130, so clean outlier-free data was told to "consider log
            # transformation" purely for being large. Tukey's far-out rule
            # (beyond 3 x IQR from the quartiles) is stable in n and flags the
            # points a reader would actually call outlying.
            dep_values <- data$dependent
            quartiles <- stats::quantile(dep_values, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
            iqr <- quartiles[2] - quartiles[1]
            if (is.finite(iqr) && iqr > 0) {
                n_far_out <- sum(dep_values < quartiles[1] - 3 * iqr |
                                 dep_values > quartiles[2] + 3 * iqr, na.rm = TRUE)
                if (n_far_out > 0) {
                    private$.addNotice(
                        'WARNING',
                        .("Extreme Values Present"),
                        jmvcore::format(
                            .("{n} plotted value(s) lie more than three interquartile ranges beyond the quartiles and will stretch the axis so the remaining lollipops look nearly equal; check these entries and consider a log scale."),
                            n = n_far_out)
                    )
                }
            }

            # Check for groups with very different sample sizes
            group_counts <- attr(data, "source_group_counts")
            if (is.null(group_counts)) group_counts <- table(droplevels(data$group))
            max_count <- max(group_counts)
            min_count <- min(group_counts)
            unbalanced <- max_count > 5 * min_count && length(group_counts) > 2
            if (unbalanced) {
                private$.addNotice(
                    'WARNING',
                    .("Unbalanced Group Sizes"),
                    jmvcore::format(
                        .("Group sizes range from {lo} to {hi} observations, so the lollipops are not estimated with equal precision; read the sparsely sampled groups with more caution."),
                        lo = min_count,
                        hi = max_count)
                )
            }

            aggregating <- !identical(self$options$aggregation, "none")

            # The mean reference line averages whatever is PLOTTED. Under
            # aggregation those are the group summaries, so the line is an
            # unweighted mean of means: with 30/3/3 observations at 10/50/52 it
            # is drawn at 37.3 while the grand mean of the raw data is 16.8. The
            # user reads "Mean =" off the chart and has no way to see which one
            # it is, so say so whenever the two can diverge materially.
            if (self$options$showMean && aggregating && unbalanced) {
                private$.addNotice(
                    'STRONG_WARNING',
                    .("Mean Line Averages the Group Summaries"),
                    jmvcore::format(
                        .("The mean reference line is the unweighted mean of the {groups} plotted group summaries, not the mean of the {raw} underlying observations; because the groups differ greatly in size these two means differ, and the line does not mark the overall average."),
                        groups = summary_stats$n_groups,
                        raw = summary_stats$source_n)
                )
            }

            # Aggregation draws one point per group and no error bar, so a group
            # whose observations are scattered looks exactly like a group whose
            # observations agree. Disclose it when the discarded spread rivals
            # the differences the chart is being used to show.
            max_within_sd <- attr(data, "source_max_within_sd")
            if (aggregating && !is.null(max_within_sd) && is.finite(max_within_sd) &&
                is.finite(summary_stats$dep_range) && summary_stats$dep_range > 0 &&
                max_within_sd > summary_stats$dep_range / 2) {
                private$.addNotice(
                    'WARNING',
                    .("Aggregation Hides Within-Group Spread"),
                    jmvcore::format(
                        .("Observations within a single group vary by up to {sd} (one standard deviation), which is more than half the {span} spread between the plotted group summaries; each lollipop is a single point with no error bar, so the chart shows less disagreement than the data contains."),
                        sd = base::format(max_within_sd, digits = 3),
                        span = base::format(summary_stats$dep_range, digits = 3))
                )
            }

            # Baseline misuse. A lollipop encodes its value as STEM LENGTH
            # measured from the baseline, so the baseline decides how much of
            # the chart carries information.
            baseline <- self$options$baseline
            if (is.numeric(baseline) && length(baseline) == 1 && is.finite(baseline) &&
                is.finite(summary_stats$dep_min) && is.finite(summary_stats$dep_max)) {

                # A baseline of zero splitting the data is the canonical
                # diverging lollipop - change scores, log ratios, z-scores - and
                # is exactly what the reader expects, so only a baseline the
                # user actually typed is worth flagging here.
                if (baseline != 0 &&
                    baseline > summary_stats$dep_min && baseline < summary_stats$dep_max) {
                    n_below <- sum(data$dependent < baseline, na.rm = TRUE)
                    n_above <- sum(data$dependent > baseline, na.rm = TRUE)
                    private$.addNotice(
                        'WARNING',
                        .("Baseline Falls Inside the Data Range"),
                        jmvcore::format(
                            .("The baseline of {base} sits inside the range of the data, so {above} lollipop(s) point one way and {below} point the other; stem length now shows distance from the baseline rather than magnitude, which is easy to misread."),
                            base = base::format(baseline, digits = 3),
                            above = n_above,
                            below = n_below)
                    )
                } else {
                    # Distance from the baseline to the far end of the data sets
                    # the drawn span; the visible differences between lollipops
                    # are only dep_range of that. A hemoglobin series of
                    # 13.1-13.6 drawn from baseline 0 puts 96% of every stem
                    # below the data, and all eight lollipops look identical.
                    span <- max(abs(summary_stats$dep_max - baseline),
                                abs(summary_stats$dep_min - baseline))
                    if (is.finite(span) && span > 0 && summary_stats$dep_range / span < 0.25) {
                        private$.addNotice(
                            'WARNING',
                            .("Baseline Far From the Data"),
                            jmvcore::format(
                                .("Measured from the baseline of {base}, the differences between groups occupy only {pct}% of each stem, so the lollipops look almost equal; set the baseline near the lower end of the data to make the comparison visible."),
                                base = base::format(baseline, digits = 3),
                                pct = round(100 * summary_stats$dep_range / span, 1))
                        )
                    }
                }
            }

            # Check for small overall sample size
            if (summary_stats$source_n < 10) {
                private$.addNotice(
                    'WARNING',
                    .("Small Sample Size"),
                    jmvcore::format(
                        .("The chart rests on {n} observations in total, too few for the differences between groups to be stable; treat the ordering as provisional."),
                        n = summary_stats$source_n)
                )
            }
        },
        
        # Populate summary table. Rows are created in .init(); only the `value`
        # cells are written here.
        .populateSummary = function(summary_stats) {
            table <- self$results$summary

            # When aggregation is active, the statistics below describe the plotted
            # per-group aggregated values (e.g. mean of sums when aggregation = sum),
            # not the raw observations. Make that explicit so labels like
            # "Mean Value" are not misread.
            agg_method <- self$options$aggregation
            if (!is.null(agg_method) && agg_method != "none") {
                table$setNote(
                    "aggregation",
                    jmvcore::format(
                        .("Statistics describe the {n} plotted per-group {method} values aggregated from {raw} observations, not the raw observations themselves."),
                        n = summary_stats$n_observations,
                        method = agg_method,
                        raw = summary_stats$source_n)
                )
            } else {
                table$setNote("aggregation", NULL)
            }

            # Data characteristics
            table$setRow(rowKey = "n_obs", values = list(
                value = as.character(summary_stats$n_observations)))

            table$setRow(rowKey = "n_groups", values = list(
                value = as.character(summary_stats$n_groups)))

            # Dependent variable statistics
            table$setRow(rowKey = "mean", values = list(
                value = base::format(summary_stats$dep_mean, digits = 3)))

            table$setRow(rowKey = "median", values = list(
                value = base::format(summary_stats$dep_median, digits = 3)))

            table$setRow(rowKey = "sd", values = list(
                value = base::format(summary_stats$dep_sd, digits = 3)))

            table$setRow(rowKey = "range", values = list(
                value = paste(base::format(summary_stats$dep_min, digits = 3), "-",
                              base::format(summary_stats$dep_max, digits = 3))))

            # Group information
            table$setRow(rowKey = "highest", values = list(
                value = as.character(summary_stats$groups_with_highest)))

            table$setRow(rowKey = "lowest", values = list(
                value = as.character(summary_stats$groups_with_lowest)))
        },
        
        # Get color scheme
        .getColorScheme = function(n_colors, highlight_level = NULL) {
            scheme_name <- self$options$colorScheme
            
            base_colors <- switch(scheme_name,
                "default" = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#7FB069", "#8E6C8A"),
                "clinical" = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b"),
                "viridis" = c("#440154", "#31688e", "#35b779", "#fde725", "#21908c", "#5dc863"),
                "colorblind" = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"),
                c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#7FB069", "#8E6C8A")  # fallback
            )
            
            # Extend colors if needed
            if (n_colors > length(base_colors)) {
                base_colors <- rep(base_colors, length.out = n_colors)
            } else {
                base_colors <- base_colors[1:n_colors]
            }
            
            # Handle highlighting
            if (!is.null(highlight_level)) {
                highlight_color <- "#FF0000"  # Red for highlight
                normal_color <- "#CCCCCC"     # Gray for non-highlighted
                
                return(list(
                    colors = base_colors,
                    highlight_color = highlight_color,
                    normal_color = normal_color,
                    has_highlight = TRUE
                ))
            } else {
                return(list(
                    colors = base_colors,
                    has_highlight = FALSE
                ))
            }
        },
        
        # Get plot theme
        .getPlotTheme = function() {
            theme_name <- self$options$theme
            
            base_theme <- switch(theme_name,
                "default" = ggplot2::theme_gray(),
                "minimal" = ggplot2::theme_minimal(),
                "classic" = ggplot2::theme_classic(),
                "publication" = ggplot2::theme_bw() + 
                    ggplot2::theme(
                        panel.grid.minor = ggplot2::element_blank(),
                        strip.background = ggplot2::element_blank(),
                        legend.position = "bottom",
                        text = ggplot2::element_text(size = 12),
                        axis.title = ggplot2::element_text(size = 14),
                        plot.title = ggplot2::element_text(size = 16, hjust = 0.5)
                    ),
                ggplot2::theme_gray()  # fallback
            )
            
            return(base_theme)
        },
        
        # Create base plot with proper orientation
        .createBasePlot = function(plot_data, orientation) {
            if (orientation == "horizontal") {
                return(ggplot2::ggplot(plot_data, ggplot2::aes(x = dependent, y = group)))
            } else {
                return(ggplot2::ggplot(plot_data, ggplot2::aes(x = group, y = dependent)))
            }
        },
        
        # Add lollipop elements (refactored to eliminate duplication)
        .addLollipopElements = function(p, plot_data, orientation, color_scheme, highlight_level, point_size, line_width, line_type = "solid", baseline = 0, conditional_color = FALSE, color_threshold = 0) {
            has_highlight <- color_scheme$has_highlight && 
                            !is.null(highlight_level) && 
                            highlight_level %in% plot_data$group
            
            # Always create color_category column, determine coloring strategy
            if (conditional_color) {
                # Conditional coloring based on threshold
                plot_data$color_category <- ifelse(plot_data$dependent > color_threshold, "above_threshold", "below_threshold")
                use_color_mapping <- TRUE
            } else if (has_highlight) {
                # Highlight-based coloring
                plot_data$color_category <- ifelse(plot_data$group == highlight_level, "highlighted", "normal")
                use_color_mapping <- TRUE
            } else {
                # No special coloring - all same category
                plot_data$color_category <- "normal"
                use_color_mapping <- FALSE
            }
            
            # Update the plot data in the ggplot object
            p$data <- plot_data
            
            # Get segment coordinates based on orientation
            if (orientation == "horizontal") {
                if (use_color_mapping) {
                    segment_aes <- ggplot2::aes(x = baseline, xend = dependent, y = group, yend = group, color = color_category)
                    point_aes <- ggplot2::aes(color = color_category)
                } else {
                    segment_aes <- ggplot2::aes(x = baseline, xend = dependent, y = group, yend = group)
                    point_aes <- ggplot2::aes()
                }
            } else {
                if (use_color_mapping) {
                    segment_aes <- ggplot2::aes(x = group, xend = group, y = baseline, yend = dependent, color = color_category)
                    point_aes <- ggplot2::aes(color = color_category)
                } else {
                    segment_aes <- ggplot2::aes(x = group, xend = group, y = baseline, yend = dependent)
                    point_aes <- ggplot2::aes()
                }
            }
            
            # Add segments and points
            p <- p + ggplot2::geom_segment(segment_aes, linewidth = line_width, linetype = line_type)
            
            if (use_color_mapping) {
                # Apply custom colors. use_color_mapping is TRUE only when
                # conditional_color or has_highlight is TRUE, so these two branches
                # are exhaustive (no dead trailing else).
                if (conditional_color) {
                    colors <- c("above_threshold" = "#E69F00", "below_threshold" = "#56B4E9")  # Orange/Blue
                } else {
                    colors <- c("highlighted" = color_scheme$highlight_color, "normal" = color_scheme$normal_color)
                }
                
                p <- p + 
                    ggplot2::geom_point(point_aes, size = point_size) +
                    ggplot2::scale_color_manual(values = colors, guide = "none")
            } else {
                # No special coloring - use default color
                p <- p + ggplot2::geom_point(color = color_scheme$colors[1], size = point_size)
            }
            
            return(p)
        },
        
        # Main plotting function
        .plot = function(image, ggtheme, theme, ...) {
            # Get plot state (contains data + visual options)
            plot_state <- image$state
            if (is.null(plot_state)) return(FALSE)

            # Extract data from state
            plot_data <- plot_state$data
            if (is.null(plot_data) || nrow(plot_data) == 0) return(FALSE)
            
            # Get options
            orientation <- self$options$orientation
            show_values <- self$options$showValues
            show_mean <- self$options$showMean
            highlight_level <- private$.activeHighlight()
            point_size <- self$options$pointSize
            line_width <- self$options$lineWidth
            line_type <- self$options$lineType
            baseline <- self$options$baseline
            conditional_color <- self$options$conditionalColor
            color_threshold <- self$options$colorThreshold
            
            # Set up colors
            n_groups <- length(unique(plot_data$group))
            color_scheme <- private$.getColorScheme(n_groups, highlight_level)
            
            # Checkpoint before expensive plot generation
            private$.checkpoint(flush = FALSE)
            
            # Create base plot
            p <- private$.createBasePlot(plot_data, orientation)
            
            # Add lollipop elements (refactored to eliminate duplication)
            p <- private$.addLollipopElements(p, plot_data, orientation, color_scheme, highlight_level, point_size, line_width, line_type, baseline, conditional_color, color_threshold)
            
            # Add value labels if requested
            if (show_values) {
                if (orientation == "horizontal") {
                    p <- p + ggplot2::geom_text(
                        ggplot2::aes(label = round(dependent, 2)),
                        hjust = -0.2,
                        size = 3
                    )
                } else {
                    p <- p + ggplot2::geom_text(
                        ggplot2::aes(label = round(dependent, 2)),
                        vjust = -0.5,
                        size = 3
                    )
                }
            }
            
            # Add mean line if requested
            if (show_mean) {
                mean_value <- mean(plot_data$dependent, na.rm = TRUE)
                # Under aggregation the plotted points ARE the group summaries,
                # so this is a mean of means and not the overall average. A bare
                # "Mean =" invites the reader to take it for the grand mean.
                mean_label <- if (identical(self$options$aggregation, "none")) {
                    jmvcore::format(.("Mean = {val}"), val = round(mean_value, 2))
                } else {
                    jmvcore::format(.("Mean of group values = {val}"), val = round(mean_value, 2))
                }
                if (orientation == "horizontal") {
                    p <- p + ggplot2::geom_vline(
                        xintercept = mean_value,
                        linetype = "dashed",
                        color = "red",
                        linewidth = 1
                    ) +
                    ggplot2::annotate(
                        "text",
                        x = mean_value,
                        y = Inf,
                        label = mean_label,
                        hjust = 1.1,
                        vjust = 1.5,
                        color = "red",
                        size = 3
                    )
                } else {
                    p <- p + ggplot2::geom_hline(
                        yintercept = mean_value,
                        linetype = "dashed",
                        color = "red",
                        linewidth = 1
                    ) +
                    ggplot2::annotate(
                        "text",
                        x = Inf,
                        y = mean_value,
                        label = mean_label,
                        hjust = 1.1,
                        vjust = -0.5,
                        color = "red",
                        size = 3
                    )
                }
            }
            
            # Add labels
            dep_var <- self$options$dep
            group_var <- self$options$group
            
            xlabel <- if (!is.null(self$options$xlabel) && nchar(self$options$xlabel) > 0) {
                self$options$xlabel
            } else {
                if (orientation == "horizontal") dep_var else group_var
            }
            
            ylabel <- if (!is.null(self$options$ylabel) && nchar(self$options$ylabel) > 0) {
                self$options$ylabel
            } else {
                if (orientation == "horizontal") group_var else dep_var
            }
            
            plot_title <- if (!is.null(self$options$title) && nchar(self$options$title) > 0) {
                self$options$title
            } else {
                paste(.("Lollipop Chart:"), dep_var, .("by"), group_var)
            }
            
            p <- p + ggplot2::labs(
                x = xlabel,
                y = ylabel,
                title = plot_title
            )
            
            # Handle axis rotation for vertical orientation with many groups
            if (orientation == "vertical" && n_groups > 10) {
                p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
            }
            
            # Apply theme
            p <- p + private$.getPlotTheme()
            
            # Print plot
            print(p)
            TRUE
        },
        
        # Save plot data
        .savePlotData = function(data) {
            # CRITICAL: Include ALL visual options in state to trigger updates
            # when user changes colors, themes, sizes, etc.
            plotState <- list(
                data = data,
                # Visual appearance options
                orientation = self$options$orientation,
                colorScheme = self$options$colorScheme,
                theme = self$options$theme,
                pointSize = self$options$pointSize,
                lineWidth = self$options$lineWidth,
                lineType = self$options$lineType,
                baseline = self$options$baseline,
                # Display options
                showValues = self$options$showValues,
                showMean = self$options$showMean,
                # Highlighting
                useHighlight = self$options$useHighlight,
                highlight = self$options$highlight,
                # Conditional coloring
                conditionalColor = self$options$conditionalColor,
                colorThreshold = self$options$colorThreshold,
                # Labels
                xlabel = self$options$xlabel,
                ylabel = self$options$ylabel,
                title = self$options$title
            )

            self$results$plot$setState(plotState)
        }
    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for Lollipop Plot analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            dep <- self$options$dep
            group <- self$options$group

            if (is.null(dep) || is.null(group))
                return('')

            # Build the argument list in option-declaration order.
            #
            # Every TEXT option is emitted as a deparse()'d string literal.
            # deparse() produces valid, fully-escaped R for any content - spaces,
            # embedded double quotes, backslashes - whereas jmvcore's default
            # sourcify wraps the raw value in quotes without escaping it, so a
            # chart titled  Hb ("g/dL")  produced  title = "Hb ("g/dL")" , which
            # does not parse. The test is on OptionString rather than
            # OptionVariable because OptionVariable inherits from it: one check
            # therefore covers variable names, factor Levels and the free-text
            # title/xlabel/ylabel boxes alike, and any text option added later is
            # escaped automatically. Values still equal to their declared default
            # are skipped so the generated call stays as short as jmvcore's.
            #
            # Variables are NOT re-emitted through private$.asArgs() - doing so
            # previously duplicated them in the generated syntax (the "double
            # variables" bug). All non-variable options keep jmvcore's per-option
            # sourcify so formatting stays consistent with jamovi.
            args <- character(0)
            for (option in private$.options$options) {
                if (option$name == 'data')
                    next
                if (inherits(option, 'OptionString') || inherits(option, 'OptionVariables')) {
                    val <- option$value
                    if (!is.null(val) && length(val) > 0 && !identical(val, option$default))
                        args <- c(args, paste0(option$name, ' = ',
                                               paste0(deparse(val), collapse = '')))
                } else {
                    as <- private$.sourcifyOption(option)
                    if (!identical(as, ''))
                        args <- c(args, as)
                }
            }

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, '::lollipop(\n    data = data,\n    ',
                   paste(args, collapse = ',\n    '), ')')
        }
    ) # End of public list
)
