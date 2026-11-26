#' @title Line Chart for Time Series and Trend Analysis
#' @description
#' Creates comprehensive line charts for time series analysis and trend visualization
#' in clinical and pathological research. This function supports multiple groups,
#' confidence intervals, trend lines, and statistical overlays, making it ideal for
#' analyzing longitudinal data, treatment responses, and biomarker trends over time.
#'
#' @details
#' The line chart function is designed specifically for clinical research applications
#' where visualization of trends and patterns over time or ordered categories is crucial.
#' It provides extensive customization options and statistical features to create
#' publication-ready plots for clinical studies.
#'
#' Key features:
#' - Multiple group support for comparative analysis
#' - Confidence intervals and trend lines
#' - Clinical color palettes and themes
#' - Reference lines for normal ranges/thresholds
#' - Statistical correlation analysis
#' - Professional publication-ready appearance
#'
#' Common clinical applications:
#' - Laboratory values over time
#' - Treatment response monitoring
#' - Biomarker evolution
#' - Dose-response relationships
#' - Survival probability trends
#' - Quality metrics tracking
#'
#' @examples
#' \dontrun{
#' # Basic time series analysis
#' result <- linechart(
#'   data = patient_data,
#'   xvar = "visit_month",
#'   yvar = "hemoglobin_level"
#' )
#'
#' # Grouped analysis with confidence intervals
#' result <- linechart(
#'   data = treatment_data,
#'   xvar = "time_point",
#'   yvar = "tumor_size",
#'   groupby = "treatment_arm",
#'   confidence = TRUE,
#'   trendline = TRUE
#' )
#'
#' # Clinical monitoring with reference line
#' result <- linechart(
#'   data = lab_data,
#'   xvar = "days_post_treatment",
#'   yvar = "white_blood_cell_count",
#'   showRefline = TRUE,
#'   refline = 4000,
#'   reflineLabel = "Normal Lower Limit"
#' )
#' }
#'
#' @importFrom R6 R6Class
#' @importFrom jmvcore .
#' @return An \code{R6} class generator object for the \code{linechartClass} backend; used internally by the jamovi analysis wrapper and not called directly.

linechartClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "linechartClass",
    inherit = linechartBase,
    private = list(

        # Safe R-syntax pane codegen: drop undeclared option names and non-scalar
        # values, and escape non-default character scalars so quotes/backslashes/
        # newlines cannot break out of string literals in regenerated R syntax.
        .sourcifyOption = function(option) {
            if (!option$name %in% names(formals(linechart)))
                return("")
            value <- option$value
            if (is.list(value))
                return("")
            if (is.character(value)) {
                if (length(value) != 1L || is.na(value) || identical(value, option$default))
                    return("")
                return(paste0(option$name, " = ", encodeString(value, quote = '"')))
            }
            super$.sourcifyOption(option)
        },

        # Use one explicit formatter for result text so numeric precision is
        # stable regardless of namespace imports or the caller's print options.
        .fmtNum = function(x, digits = 3) {
            if (length(x) != 1 || !is.finite(x)) return("NA")
            sub("\\.$", "", formatC(signif(x, digits), format = "fg", flag = "#", digits = digits))
        },

        # Rows dropped during preparation, recorded so .checkDataQuality() can
        # report them via jamovi Notices. warning() only reaches the R console.
        .n_excluded_missing   = 0L,
        .n_excluded_nonfinite = 0L,
        .n_rows_analysed      = 0L,
        .top_notice_pos       = 1L,

        # Insert a single-line plain-text jamovi Notice.
        # STRONG_WARNING and WARNING notices are inserted at the top in order;
        # INFO notices are appended at the bottom via results$add().
        .emitNotice = function(name, type, content, position = "top") {
            clean_text <- trimws(gsub("[\r\n]+", " ", gsub("<[^>]+>", "", as.character(content))))
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = name,
                type = type
            )
            notice$setContent(clean_text)
            if (identical(position, "bottom")) {
                self$results$add(notice)
            } else {
                self$results$insert(private$.top_notice_pos, notice)
                private$.top_notice_pos <- private$.top_notice_pos + 1L
            }
        },

        # Initialize results and validate dependencies
        .init = function() {
            # Apply user-specified plot dimensions so the Width/Height options
            # actually resize the rendered Image. Enforce bounds as a defense-in-depth
            # backstop in case an R caller invokes run(noThrow = TRUE) after an option error.
            w <- suppressWarnings(as.integer(self$options$width))
            h <- suppressWarnings(as.integer(self$options$height))
            if (length(w) != 1L || is.na(w) || w < 300L || w > 1200L) w <- 800L
            if (length(h) != 1L || is.na(h) || h < 300L || h > 1000L) h <- 600L
            self$results$plot$setSize(w, h)

            # The summary table has a fixed row set, so build the skeleton (rowKey
            # + statistic label) here; .populateSummary() then only fills `value`.
            # Group Names is gated on the groupby option - the SAME condition
            # .populateSummary() uses - so every rowKey it sets exists on every
            # path (setRow() on a missing rowKey aborts the analysis).
            summaryTable <- self$results$summary
            summaryTable$addRow(rowKey = "n_obs", values = list(
                statistic = .("Number of Observations")))
            summaryTable$addRow(rowKey = "n_x", values = list(
                statistic = .("Number of X-axis Points")))
            summaryTable$addRow(rowKey = "n_groups", values = list(
                statistic = .("Number of Groups")))
            if (!is.null(self$options$groupby))
                summaryTable$addRow(rowKey = "group_names", values = list(
                    statistic = .("Group Names")))
            summaryTable$addRow(rowKey = "y_mean", values = list(
                statistic = .("Y Mean")))
            summaryTable$addRow(rowKey = "y_median", values = list(
                statistic = .("Y Median")))
            summaryTable$addRow(rowKey = "y_sd", values = list(
                statistic = .("Y Standard Deviation")))
            summaryTable$addRow(rowKey = "y_range", values = list(
                statistic = .("Y Range")))

            # Show onboarding welcome message only when required variables are unset.
            # Visibility of analysis outputs is driven declaratively in linechart.r.yaml.
            if (is.null(self$options$xvar) || is.null(self$options$yvar)) {
                private$.showWelcomeMessage()
                self$results$todo$setVisible(TRUE)
            }
        },

        .run = function() {
            private$.top_notice_pos <- 1L

            # Early exits for missing variables or empty data.
            # Validation failures propagate directly via jmvcore::reject() to
            # jamovi's native error banner without catching or hiding panels.
            if (is.null(self$options$xvar) || is.null(self$options$yvar)) {
                return()
            }

            if (is.null(self$data) || nrow(self$data) == 0) {
                jmvcore::reject(.("The dataset has no rows, so there is nothing to chart. If a row filter is active, it may be excluding every case."))
            }

            # Hide onboarding welcome panel once both required variables are set
            self$results$todo$setVisible(FALSE)

            # Check runtime dependencies (propagates jmvcore::reject() if missing)
            private$.checkDependencies()

            # Prepare and validate data (propagates jmvcore::reject() on invalid input)
            private$.checkpoint()  # Checkpoint before expensive data cleaning
            data <- private$.cleanData()

            # Emit data-quality jamovi Notices (STRONG_WARNING / WARNING at top, INFO at bottom)
            private$.checkDataQuality(data)

            # Calculate summary statistics
            private$.checkpoint()  # Checkpoint before statistical calculations
            summary_stats <- private$.calculateSummary(data)
            private$.populateSummary(summary_stats)

            # Calculate correlation once if trend line requested, then thread
            # the result into the natural-language summary, the correlation
            # table, and the assumptions panel (avoids recomputing cor.test/lm).
            correlation_stats <- NULL
            if (isTRUE(self$options$trendline)) {
                private$.checkpoint()  # Checkpoint before correlation analysis
                correlation_stats <- private$.calculateCorrelation(data)
            }

            # Generate natural language summary (reuses correlation_stats)
            private$.generateNaturalSummary(summary_stats, data, correlation_stats)

            if (isTRUE(self$options$trendline)) {
                private$.populateCorrelation(correlation_stats)
                private$.populateAssumptions(data, correlation_stats)
                if (self$results$correlation$rowCount == 0L) {
                    self$results$correlation$setVisible(FALSE)
                }
            }

            # Save plot data for rendering
            private$.savePlotData(data)
        },

        # Comprehensive data cleaning and validation
        .cleanData = function() {
            # Extract variables
            xvar <- self$options$xvar
            yvar <- self$options$yvar
            groupby <- self$options$groupby

            # Select required columns
            required_vars <- c(xvar, yvar)
            if (!is.null(groupby)) {
                required_vars <- c(required_vars, groupby)
            }

            # Check if variables exist in data
            missing_vars <- setdiff(required_vars, names(self$data))
            if (length(missing_vars) > 0) {
                jmvcore::reject(.fmt(
                    .("Variables not found in data: {vars}"),
                    vars = paste(missing_vars, collapse = ", ")))
            }

            # Select and clean data
            data <- self$data[required_vars]

            # Convert explicit NA factor levels (e.g. from addNA() / exclude = NULL)
            # back to true NA so complete.cases() excludes them rather than creating
            # a phantom "NA" category.
            for (col_nm in required_vars) {
                col_v <- data[[col_nm]]
                if (is.factor(col_v) && anyNA(levels(col_v))) {
                    data[[col_nm]] <- factor(
                        as.character(col_v),
                        levels = levels(col_v)[!is.na(levels(col_v))],
                        ordered = is.ordered(col_v)
                    )
                }
            }

            # Validate X variable
            x_data <- data[[xvar]]
            if (inherits(x_data, "haven_labelled")) {
                x_data <- as.numeric(x_data)
                data[[xvar]] <- x_data
            }
            if (is.character(x_data)) {
                # Check non-missing values only so a single NA does not force a
                # numeric-looking character column into an alphabetical factor.
                unique_values <- unique(x_data[!is.na(x_data)])
                if (length(unique_values) > 0 &&
                    all(grepl("^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?$", unique_values, perl = TRUE))) {
                    # Looks numeric, convert
                    x_num <- suppressWarnings(as.numeric(x_data))
                    if (any(is.na(x_num) & !is.na(x_data))) {
                        jmvcore::reject(.("X-axis variable contains non-numeric values that cannot be converted."))
                    }
                    x_data <- x_num
                } else {
                    # Convert to factor
                    x_data <- factor(x_data)
                }
                data[[xvar]] <- x_data
            } else if (is.factor(x_data)) {
                # Check if factor levels are meaningful for ordering
                levels_numeric <- suppressWarnings(as.numeric(levels(x_data)))
                if (!any(is.na(levels_numeric))) {
                    # Reorder factor levels numerically while preserving ordered class
                    data[[xvar]] <- factor(
                        x_data,
                        levels = levels(x_data)[order(levels_numeric)],
                        ordered = is.ordered(x_data)
                    )
                }
            }

            # Validate Y variable (must be numeric)
            y_raw <- data[[yvar]]
            if (inherits(y_raw, "haven_labelled")) {
                y_raw <- as.numeric(y_raw)
            }
            y_data <- jmvcore::toNumeric(y_raw)
            if (!is.numeric(y_data) || all(is.na(y_data))) {
                jmvcore::reject(.("Y-axis variable must be numeric (continuous variable)."))
            }
            data[[yvar]] <- y_data

            # Validate grouping variable if provided
            if (!is.null(groupby)) {
                group_data <- data[[groupby]]
                if (inherits(group_data, "haven_labelled") &&
                    requireNamespace("haven", quietly = TRUE)) {
                    data[[groupby]] <- haven::as_factor(group_data)
                } else if (is.character(group_data)) {
                    data[[groupby]] <- factor(group_data)
                } else if (!is.factor(group_data)) {
                    data[[groupby]] <- factor(group_data)
                }

                # A high group count reaches the user through .checkDataQuality()
                # ("Many groups detected ..."), which writes into the results panel.
                # A warning() here only reached the R console, which jamovi never
                # shows, so it was a duplicate of an invisible message.
            }

            # Remove rows with missing values.
            #
            # complete.cases() follows is.na(), which is TRUE for NaN but FALSE for
            # Inf, so infinite values used to survive this filter. They then reached
            # `var(y) == 0` below, where var() returns NaN and `NaN == 0` is NA, so
            # `if (NA)` aborted the run with "missing value where TRUE/FALSE needed"
            # - a message that tells a clinician nothing, under a heading that then
            # advised removing MISSING values, which is not what went wrong.
            complete_before <- nrow(data)
            data <- data[stats::complete.cases(data), , drop = FALSE]

            n_nonfinite <- 0L
            num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
            if (length(num_cols) > 0 && nrow(data) > 0) {
                finite_rows <- Reduce(`&`, lapply(data[num_cols], is.finite))
                n_nonfinite <- sum(!finite_rows)
                if (n_nonfinite > 0)
                    data <- data[finite_rows, , drop = FALSE]
            }
            complete_after <- nrow(data)

            if (complete_after == 0) {
                jmvcore::reject(.("No usable rows remain after removing missing and infinite values. Please check the selected variables."))
            }

            # Drop unused factor levels on groupby after NA/Inf exclusions so
            # downstream group-size checks do not count 0-row levels.
            if (!is.null(groupby) && is.factor(data[[groupby]])) {
                data[[groupby]] <- droplevels(data[[groupby]])
            }

            # Record exclusions so they can be shown in the results panel.
            # warning() alone goes to the R console, which the jamovi user never
            # sees, so dropped rows were invisible in the GUI.
            private$.n_excluded_missing  <- complete_before - complete_after - n_nonfinite
            private$.n_excluded_nonfinite <- n_nonfinite
            private$.n_rows_analysed      <- complete_after

            # Enhanced minimum data requirements with suggestions
            if (nrow(data) < 3) {
                jmvcore::reject(.fmt(
                    .("At least 3 complete observations are required for line chart analysis. The current dataset has {n} observation(s). Consider checking for missing values or selecting different variables."),
                    n = nrow(data)))
            }

            # Enhanced variation check with suggestions
            if (stats::var(data[[yvar]], na.rm = TRUE) == 0) {
                jmvcore::reject(.("The Y-axis variable has no variation (all values are identical). Line charts require variation in the Y variable. Please select a different variable with varying values."))
            }

            # (refline is a numeric Number option: no non-numeric validation needed here)

            # CRITICAL FIX: Sort data by x-variable to prevent zig-zag lines
            # For longitudinal data, observations must be ordered by time/sequence
            # to correctly connect points chronologically
            if (!is.null(groupby)) {
                # Sort by grouping variable first, then by x-variable
                # This ensures each group's points are connected in correct order
                data <- data[order(data[[groupby]], data[[xvar]]), ]
            } else {
                # Sort by x-variable only for ungrouped data
                data <- data[order(data[[xvar]]), ]
            }

            return(data)
        },

        # Calculate summary statistics
        .calculateSummary = function(data) {
            xvar <- self$options$xvar
            yvar <- self$options$yvar
            groupby <- self$options$groupby

            summary_stats <- list()

            # Basic data information
            summary_stats$n_observations <- nrow(data)
            summary_stats$n_x_points <- length(unique(data[[xvar]]))

            # Y variable statistics
            y_data <- data[[yvar]]
            summary_stats$y_mean <- mean(y_data, na.rm = TRUE)
            summary_stats$y_median <- stats::median(y_data, na.rm = TRUE)
            summary_stats$y_sd <- stats::sd(y_data, na.rm = TRUE)
            summary_stats$y_min <- min(y_data, na.rm = TRUE)
            summary_stats$y_max <- max(y_data, na.rm = TRUE)
            summary_stats$y_range <- summary_stats$y_max - summary_stats$y_min

            # Group information if applicable
            if (!is.null(groupby)) {
                summary_stats$n_groups <- length(unique(data[[groupby]]))
                summary_stats$group_names <- paste(unique(data[[groupby]]), collapse = ", ")
            } else {
                summary_stats$n_groups <- 1
                summary_stats$group_names <- .("Single group")
            }

            return(summary_stats)
        },

        # Calculate correlation statistics
        .calculateCorrelation = function(data) {
            xvar <- self$options$xvar
            yvar <- self$options$yvar
            groupby <- self$options$groupby

            correlation_stats <- list()

            # Check for grouped data or repeated measures within (groupby, xvar) cells
            n_unique_x <- length(unique(data[[xvar]]))
            n_observations <- nrow(data)
            correlation_stats$n_obs <- n_observations
            if (!is.null(groupby)) {
                n_cells <- nrow(unique(data[, c(groupby, xvar), drop = FALSE]))
                avg_obs_per_x <- n_observations / n_cells
            } else {
                avg_obs_per_x <- n_observations / n_unique_x
            }

            # Flag if multiple observations occur per X-value (within group if grouped)
            has_repeated_measures <- avg_obs_per_x > 1.5

            if (!is.null(groupby)) {
                correlation_stats$has_grouping <- TRUE
            }

            if (has_repeated_measures) {
                correlation_stats$has_repeated_measures <- TRUE
            }

            # Apply independence / grouping limitation note to both numeric and categorical X
            if (has_repeated_measures || !is.null(groupby)) {
                correlation_stats$independence_note <-
                    .("IMPORTANT LIMITATION: these statistics assume independent observations and pool across any groups. For longitudinal data with repeated measures or grouped cohorts they may overstate significance or mask within-group patterns (Simpson's paradox). Consider patient-level summaries, group-specific models, or mixed-effects models in specialised software.")
            }

            # Overall correlation (if X is numeric)
            x_data <- data[[xvar]]
            y_data <- data[[yvar]]

            if (is.numeric(x_data) && length(unique(x_data)) >= 2) {
                # NAIVE STATISTICS (assume independence - for reference only)
                # Pearson correlation
                cor_result <- stats::cor.test(x_data, y_data, method = "pearson")
                correlation_stats$pearson_r_naive <- cor_result$estimate
                correlation_stats$pearson_p_naive <- cor_result$p.value
                correlation_stats$pearson_ci_lower_naive <- cor_result$conf.int[1]
                correlation_stats$pearson_ci_upper_naive <- cor_result$conf.int[2]

                # Spearman correlation (rank-based)
                # Tied ranks or N >= 1290 make stats::cor.test use the asymptotic
                # approximation rather than exact Algorithm AS 89; disclose both.
                has_spearman_ties <- anyDuplicated(x_data) > 0L ||
                    anyDuplicated(y_data) > 0L
                cor_spearman <- stats::cor.test(
                    x_data,
                    y_data,
                    method = "spearman",
                    exact = if (has_spearman_ties) FALSE else NULL)
                correlation_stats$spearman_r_naive <- cor_spearman$estimate
                correlation_stats$spearman_p_naive <- cor_spearman$p.value
                correlation_stats$spearman_has_ties <- has_spearman_ties
                correlation_stats$spearman_asymptotic_large_n <- !has_spearman_ties && n_observations >= 1290L

                # Linear regression statistics
                lm_result <- stats::lm(y_data ~ x_data)
                correlation_stats$slope_naive <- stats::coef(lm_result)[2]
                correlation_stats$intercept_naive <- stats::coef(lm_result)[1]
                correlation_stats$r_squared_naive <- summary(lm_result)$r.squared
                correlation_stats$regression_p_naive <- summary(lm_result)$coefficients[2, 4]
                slope_scale <- stats::sd(y_data) / stats::sd(x_data)
                correlation_stats$slope_is_zero <-
                    is.finite(slope_scale) &&
                    abs(correlation_stats$slope_naive) <=
                        sqrt(.Machine$double.eps) * slope_scale

                # Set "display" values - use naive with clear labeling
                correlation_stats$pearson_r <- correlation_stats$pearson_r_naive
                correlation_stats$pearson_p <- correlation_stats$pearson_p_naive
                correlation_stats$pearson_ci_lower <- correlation_stats$pearson_ci_lower_naive
                correlation_stats$pearson_ci_upper <- correlation_stats$pearson_ci_upper_naive
                correlation_stats$spearman_r <- correlation_stats$spearman_r_naive
                correlation_stats$spearman_p <- correlation_stats$spearman_p_naive
                correlation_stats$slope <- correlation_stats$slope_naive
                correlation_stats$intercept <- correlation_stats$intercept_naive
                correlation_stats$r_squared <- correlation_stats$r_squared_naive
                correlation_stats$regression_p <- correlation_stats$regression_p_naive

                # Per-group stratified trend analysis when multiple groups are present
                if (!is.null(groupby)) {
                    grp_vec <- droplevels(as.factor(data[[groupby]]))
                    grp_levels <- levels(grp_vec)
                    if (length(grp_levels) >= 2L) {
                        group_trends <- list()
                        for (lvl in grp_levels) {
                            idx <- grp_vec == lvl
                            xg <- x_data[idx]
                            yg <- y_data[idx]
                            ng <- length(xg)
                            x_has_var <- length(unique(xg)) >= 2L && is.finite(stats::var(xg)) && stats::var(xg) > 0
                            y_has_var <- length(unique(yg)) >= 2L && is.finite(stats::var(yg)) && stats::var(yg) > 0
                            if (ng >= 3L && x_has_var && y_has_var) {
                                cg <- tryCatch(stats::cor.test(xg, yg, method = "pearson"), error = function(e) NULL)
                                lg <- tryCatch(stats::lm(yg ~ xg), error = function(e) NULL)
                                if (!is.null(cg) && !is.null(lg)) {
                                    ci_low <- if (!is.null(cg$conf.int) && length(cg$conf.int) == 2L && is.finite(cg$conf.int[1])) cg$conf.int[1] else NULL
                                    ci_high <- if (!is.null(cg$conf.int) && length(cg$conf.int) == 2L && is.finite(cg$conf.int[2])) cg$conf.int[2] else NULL
                                    group_trends[[lvl]] <- list(
                                        group = lvl,
                                        n = ng,
                                        valid = TRUE,
                                        pearson_r = unname(cg$estimate),
                                        ci_lower = ci_low,
                                        ci_upper = ci_high,
                                        pearson_p = cg$p.value,
                                        slope = unname(stats::coef(lg)[2]),
                                        r_squared = unname(cg$estimate)^2
                                    )
                                } else {
                                    group_trends[[lvl]] <- list(group = lvl, n = ng, valid = FALSE, reason = "fit")
                                }
                            } else {
                                reason <- if (ng < 3L) "small_n" else "constant"
                                group_trends[[lvl]] <- list(group = lvl, n = ng, valid = FALSE, reason = reason)
                            }
                        }
                        correlation_stats$group_trends <- group_trends
                    }
                }

            } else {
                # For categorical X, calculate one-way ANOVA across X categories
                if ((is.ordered(x_data) || is.factor(x_data)) && length(unique(x_data)) >= 2) {
                    n_cat <- length(unique(x_data))
                    df2 <- length(y_data) - n_cat
                    if (df2 <= 0L) {
                        correlation_stats$anova_f <- NA_real_
                        correlation_stats$anova_p <- NA_real_
                        correlation_stats$anova_df1 <- n_cat - 1L
                        correlation_stats$anova_df2 <- 0L
                    } else {
                        anova_result <- stats::anova(stats::lm(y_data ~ x_data))
                        correlation_stats$anova_f <- anova_result$`F value`[1]
                        correlation_stats$anova_p <- anova_result$`Pr(>F)`[1]
                        # An F with no degrees of freedom cannot be interpreted or
                        # reported, so carry both df alongside it.
                        correlation_stats$anova_df1 <- anova_result$Df[1]
                        correlation_stats$anova_df2 <- anova_result$Df[2]
                    }
                }
            }

            return(correlation_stats)
        },

        # Populate summary table. Rows are created in .init(); only the `value`
        # cells are written here.
        .populateSummary = function(summary_stats) {
            table <- self$results$summary

            table$setRow(rowKey = "n_obs", values = list(
                value = as.character(summary_stats$n_observations)))

            table$setRow(rowKey = "n_x", values = list(
                value = as.character(summary_stats$n_x_points)))

            table$setRow(rowKey = "n_groups", values = list(
                value = as.character(summary_stats$n_groups)))

            if (!is.null(self$options$groupby))
                table$setRow(rowKey = "group_names", values = list(
                    value = summary_stats$group_names))

            table$setRow(rowKey = "y_mean", values = list(
                value = private$.fmtNum(summary_stats$y_mean)))

            table$setRow(rowKey = "y_median", values = list(
                value = private$.fmtNum(summary_stats$y_median)))

            table$setRow(rowKey = "y_sd", values = list(
                value = private$.fmtNum(summary_stats$y_sd)))

            table$setRow(rowKey = "y_range", values = list(
                value = paste(private$.fmtNum(summary_stats$y_min), "-",
                              private$.fmtNum(summary_stats$y_max))))
        },

        # Enhanced correlation table with copy-ready interpretations
        .populateCorrelation = function(correlation_stats) {
            table <- self$results$correlation
            .deleteTableRows(table)

            row_num <- 1
            
            # Extract flags for repeated measures and grouping
            has_repeated_measures <- isTRUE(correlation_stats$has_repeated_measures)
            has_grouping <- isTRUE(correlation_stats$has_grouping)

            # Pearson correlation with enhanced interpretation
            if (!is.null(correlation_stats$pearson_r)) {
                # Create copy-ready sentence
                copy_ready <- private$.generateCopyReadyCorrelation(correlation_stats)

                table$addRow(rowKey = row_num, values = list(
                    measure = .("Pearson Correlation"),
                    value = correlation_stats$pearson_r,
                    ci_lower = correlation_stats$pearson_ci_lower,
                    ci_upper = correlation_stats$pearson_ci_upper,
                    pvalue = correlation_stats$pearson_p,
                    interpretation = private$.interpretCorrelation(correlation_stats$pearson_r,
                                                                   correlation_stats$pearson_p,
                                                                   has_repeated_measures,
                                                                   has_grouping)
                ))
                row_num <- row_num + 1

                table$addRow(rowKey = row_num, values = list(
                    measure = .("R-squared (Effect Size)"),
                    value = correlation_stats$r_squared,
                    interpretation = .fmt(
                        .("{percent}% of variance explained. {interpretation}"),
                        percent = round(correlation_stats$r_squared * 100, 1),
                        interpretation = private$.interpretEffectSize(correlation_stats$r_squared))
                ))
                row_num <- row_num + 1

                # Enhanced slope interpretation
                slope_is_zero <- isTRUE(correlation_stats$slope_is_zero)
                slope_interpretation <- if (slope_is_zero) {
                    .("No linear trend: The estimated regression slope is effectively zero at numerical precision.")
                } else if (correlation_stats$slope > 0) {
                    .fmt(
                        .("Positive trend: Each unit increase in X corresponds to an average {slope} unit increase in Y."),
                        slope = private$.fmtNum(abs(correlation_stats$slope)))
                } else {
                    .fmt(
                        .("Negative trend: Each unit increase in X corresponds to an average {slope} unit decrease in Y."),
                        slope = private$.fmtNum(abs(correlation_stats$slope)))
                }

                table$addRow(rowKey = row_num, values = list(
                    measure = .("Regression Slope"),
                    value = correlation_stats$slope,
                    pvalue = correlation_stats$regression_p,
                    interpretation = slope_interpretation
                ))
                row_num <- row_num + 1

                # Add copy-ready summary as a special row
                table$addRow(rowKey = row_num, values = list(
                    measure = .("Copy-Ready Summary"),
                    value = "",
                    interpretation = copy_ready
                ))
                row_num <- row_num + 1
            }

            # Spearman correlation with enhanced interpretation
            if (!is.null(correlation_stats$spearman_r)) {
                spearman_note <- if (isTRUE(correlation_stats$spearman_has_ties)) {
                    paste0(
                        " ",
                        .("The p-value uses an asymptotic approximation because tied ranks prevent exact inference."))
                } else if (isTRUE(correlation_stats$spearman_asymptotic_large_n)) {
                    paste0(
                        " ",
                        .("The p-value uses an asymptotic approximation because N >= 1290 exceeds R's exact calculation limit."))
                } else {
                    ""
                }
                table$addRow(rowKey = row_num, values = list(
                    measure = .("Spearman Correlation (Rank-based)"),
                    value = correlation_stats$spearman_r,
                    pvalue = correlation_stats$spearman_p,
                    interpretation = paste0(private$.interpretCorrelation(correlation_stats$spearman_r,
                                                                         correlation_stats$spearman_p,
                                                                         has_repeated_measures,
                                                                         has_grouping),
                                          " ", .("This rank-based measure is less sensitive to extreme values than Pearson correlation."),
                                          spearman_note)
                ))
                row_num <- row_num + 1
            }

            # ANOVA for categorical X with enhanced interpretation
            if (!is.null(correlation_stats$anova_f)) {
                anova_valid <- is.finite(correlation_stats$anova_f) && is.finite(correlation_stats$anova_p)
                anova_label <- if (!is.null(correlation_stats$anova_df1)) {
                    .fmt(.("ANOVA F-statistic (df {df1}, {df2})"),
                         df1 = correlation_stats$anova_df1,
                         df2 = correlation_stats$anova_df2)
                } else {
                    .("ANOVA F-statistic")
                }

                if (!anova_valid) {
                    table$addRow(rowKey = row_num, values = list(
                        measure = anova_label,
                        value = NULL,
                        pvalue = NULL,
                        interpretation = .("One-way ANOVA cannot be computed because there are no within-category replicates (residual df = 0). At least one X-axis category must have multiple observations.")
                    ))
                } else {
                    anova_interpretation <- if (correlation_stats$anova_p < 0.05) {
                        .("Significant differences between groups were detected (p < 0.05). Post-hoc testing is recommended to identify specific group differences.")
                    } else {
                        .("No statistically significant differences between groups were detected (p \u2265 0.05). This result does not establish equivalence between the group means.")
                    }
                    if (has_grouping) {
                        anova_interpretation <- paste0(
                            anova_interpretation, " ",
                            .("Caution: This one-way ANOVA tests differences across X-axis categories pooled over grouping levels and assumes independent observations."))
                    }
                    table$addRow(rowKey = row_num, values = list(
                        measure = anova_label,
                        value = correlation_stats$anova_f,
                        pvalue = correlation_stats$anova_p,
                        interpretation = anova_interpretation
                    ))
                }
                row_num <- row_num + 1
            }

            # Per-group stratified trends when groupby is active (numeric X)
            if (!is.null(correlation_stats$group_trends) && length(correlation_stats$group_trends) > 0L) {
                for (gt in correlation_stats$group_trends) {
                    grp_label <- .fmt(.("Group Trend: {group} (Pearson r)"), group = gt$group)
                    if (isTRUE(gt$valid)) {
                        g_key <- paste(
                            private$.correlationBandKey(gt$pearson_r),
                            if (gt$pearson_r >= 0) "pos" else "neg",
                            if (is.finite(gt$pearson_p) && gt$pearson_p < 0.05) "sig" else "ns",
                            sep = "_"
                        )
                        g_tpl <- switch(g_key,
                            negligible_pos_sig  = .("Within-group negligible positive trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; statistically significant)."),
                            negligible_pos_ns   = .("Within-group negligible positive trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; not statistically significant)."),
                            negligible_neg_sig  = .("Within-group negligible negative trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; statistically significant)."),
                            negligible_neg_ns   = .("Within-group negligible negative trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; not statistically significant)."),
                            weak_pos_sig        = .("Within-group weak positive trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; statistically significant)."),
                            weak_pos_ns         = .("Within-group weak positive trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; not statistically significant)."),
                            weak_neg_sig        = .("Within-group weak negative trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; statistically significant)."),
                            weak_neg_ns         = .("Within-group weak negative trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; not statistically significant)."),
                            moderate_pos_sig    = .("Within-group moderate positive trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; statistically significant)."),
                            moderate_pos_ns     = .("Within-group moderate positive trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; not statistically significant)."),
                            moderate_neg_sig    = .("Within-group moderate negative trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; statistically significant)."),
                            moderate_neg_ns     = .("Within-group moderate negative trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; not statistically significant)."),
                            strong_pos_sig      = .("Within-group strong positive trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; statistically significant)."),
                            strong_pos_ns       = .("Within-group strong positive trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; not statistically significant)."),
                            strong_neg_sig      = .("Within-group strong negative trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; statistically significant)."),
                            strong_neg_ns       = .("Within-group strong negative trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; not statistically significant)."),
                            very_strong_pos_sig = .("Within-group very strong positive trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; statistically significant)."),
                            very_strong_pos_ns  = .("Within-group very strong positive trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; not statistically significant)."),
                            very_strong_neg_sig = .("Within-group very strong negative trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; statistically significant)."),
                            very_strong_neg_ns  = .("Within-group very strong negative trend (n = {n}, slope = {slope}, R\u00b2 = {r2}; not statistically significant)."),
                            .("Within-group trend estimated (n = {n}, slope = {slope}, R\u00b2 = {r2}).")
                        )
                        g_interp <- .fmt(
                            g_tpl,
                            n = gt$n,
                            slope = private$.fmtNum(gt$slope),
                            r2 = round(gt$r_squared, 3)
                        )
                        table$addRow(rowKey = row_num, values = list(
                            measure = grp_label,
                            value = gt$pearson_r,
                            ci_lower = gt$ci_lower,
                            ci_upper = gt$ci_upper,
                            pvalue = gt$pearson_p,
                            interpretation = g_interp
                        ))
                    } else {
                        g_reason <- if (identical(gt$reason, "small_n")) {
                            .fmt(.("Within-group trend could not be estimated (n = {n}; at least 3 observations required)."), n = gt$n)
                        } else {
                            .fmt(.("Within-group trend could not be estimated (n = {n}; constant X or Y values within group)."), n = gt$n)
                        }
                        table$addRow(rowKey = row_num, values = list(
                            measure = grp_label,
                            value = NULL,
                            ci_lower = NULL,
                            ci_upper = NULL,
                            pvalue = NULL,
                            interpretation = g_reason
                        ))
                    }
                    row_num <- row_num + 1
                }
            }

            # Add independence / grouping assumption warning for both numeric and categorical analyses
            if (!is.null(correlation_stats$independence_note) && row_num > 1) {
                table$addRow(rowKey = row_num, values = list(
                    measure = .("Statistical Validity"),
                    value = "",
                    interpretation = correlation_stats$independence_note
                ))
                row_num <- row_num + 1
            }
        },

        # Generate copy-ready correlation interpretation
        .generateCopyReadyCorrelation = function(correlation_stats) {
            r <- correlation_stats$pearson_r
            p <- correlation_stats$pearson_p
            r_squared <- correlation_stats$r_squared

            has_repeated_measures <- isTRUE(correlation_stats$has_repeated_measures)
            has_grouping <- isTRUE(correlation_stats$has_grouping)

            # Format exact p-value without coarsening p in [0.001, 0.01) or rounding
            # p = 0.0499 to "p = 0.05" alongside "statistically significant".
            sig_level <- if (is.na(p)) {
                "p = NA"
            } else if (p < 0.001) {
                "p < 0.001"
            } else if (p < 0.05 && round(p, 3) >= 0.05) {
                sprintf("p = %.4f", p)
            } else {
                sprintf("p = %.3f", p)
            }
            is_significant <- is.finite(p) && p < 0.05

            ci_part <- if (!is.null(correlation_stats$pearson_ci_lower) &&
                           !is.null(correlation_stats$pearson_ci_upper) &&
                           is.finite(correlation_stats$pearson_ci_lower) &&
                           is.finite(correlation_stats$pearson_ci_upper)) {
                sprintf(", 95%% CI [%.3f, %.3f]",
                        correlation_stats$pearson_ci_lower,
                        correlation_stats$pearson_ci_upper)
            } else {
                ""
            }
            n_part <- if (!is.null(correlation_stats$n_obs)) {
                sprintf(", N = %d", as.integer(correlation_stats$n_obs))
            } else {
                ""
            }
            stat_summary <- paste0("Pearson r = ", sprintf("%.3f", r), ci_part, ", ", sig_level, n_part)

            # Build copy-ready sentence using whole-sentence i18n templates (no word splicing)
            correlation_is_zero <- isTRUE(all.equal(
                unname(r), 0, tolerance = sqrt(.Machine$double.eps)))
            cr_tpl <- if (correlation_is_zero) {
                .("The analysis found a negligible correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is not statistically significant.")
            } else {
                cr_key <- paste(
                    private$.correlationBandKey(r),
                    if (r > 0) "pos" else "neg",
                    if (is_significant) "sig" else "ns",
                    sep = "_"
                )
                switch(cr_key,
                    negligible_pos_sig  = .("The analysis found a negligible positive correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is statistically significant."),
                    negligible_pos_ns   = .("The analysis found a negligible positive correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is not statistically significant."),
                    negligible_neg_sig  = .("The analysis found a negligible negative correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is statistically significant."),
                    negligible_neg_ns   = .("The analysis found a negligible negative correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is not statistically significant."),
                    weak_pos_sig        = .("The analysis found a weak positive correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is statistically significant."),
                    weak_pos_ns         = .("The analysis found a weak positive correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is not statistically significant."),
                    weak_neg_sig        = .("The analysis found a weak negative correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is statistically significant."),
                    weak_neg_ns         = .("The analysis found a weak negative correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is not statistically significant."),
                    moderate_pos_sig    = .("The analysis found a moderate positive correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is statistically significant."),
                    moderate_pos_ns     = .("The analysis found a moderate positive correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is not statistically significant."),
                    moderate_neg_sig    = .("The analysis found a moderate negative correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is statistically significant."),
                    moderate_neg_ns     = .("The analysis found a moderate negative correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is not statistically significant."),
                    strong_pos_sig      = .("The analysis found a strong positive correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is statistically significant."),
                    strong_pos_ns       = .("The analysis found a strong positive correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is not statistically significant."),
                    strong_neg_sig      = .("The analysis found a strong negative correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is statistically significant."),
                    strong_neg_ns       = .("The analysis found a strong negative correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is not statistically significant."),
                    very_strong_pos_sig = .("The analysis found a very strong positive correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is statistically significant."),
                    very_strong_pos_ns  = .("The analysis found a very strong positive correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is not statistically significant."),
                    very_strong_neg_sig = .("The analysis found a very strong negative correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is statistically significant."),
                    very_strong_neg_ns  = .("The analysis found a very strong negative correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance, and the relationship is not statistically significant."),
                    .("The analysis examined the correlation between {xvar} and {yvar} ({stats}). The correlation explains {variance}% of the variance.")
                )
            }
            copy_ready <- .fmt(
                cr_tpl,
                xvar = self$options$xvar, yvar = self$options$yvar,
                stats = stat_summary, variance = round(r_squared * 100, 1)
            )

            # Append repeated-measures and grouped-data cautions independently so
            # neither shadows the other.
            if (has_repeated_measures) {
                copy_ready <- paste0(copy_ready, " ", .("Note: Results should be interpreted with caution as repeated X values were observed. This does not identify repeated subjects; verify whether observations are independent."))
            }
            if (has_grouping) {
                copy_ready <- paste0(copy_ready, " ", .("Note: Results should be interpreted with caution as pooled correlation across grouped data may mask within-group patterns (Simpson's paradox)."))
            }
            if (!has_repeated_measures && !has_grouping && is_significant) {
                copy_ready <- paste0(copy_ready, " ", .("Statistical significance reflects sample size as well as the size of the association; the magnitude of r and the R-squared value reported above describe the strength of the relationship. Correlation does not imply causation."))
            }

            return(copy_ready)
        },

        # Untranslated band key for correlation strength (with FP boundary tolerance).
        #   |r| < 0.10  negligible
        #   |r| < 0.30  weak
        #   |r| < 0.50  moderate
        #   |r| < 0.70  strong
        #   |r| >= 0.70 very_strong
        .correlationBandKey = function(r) {
            if (is.na(r)) return("undetermined")
            abs_r <- abs(r)
            eps <- 1e-12
            if (abs_r < 0.1 - eps) {
                "negligible"
            } else if (abs_r < 0.3 - eps) {
                "weak"
            } else if (abs_r < 0.5 - eps) {
                "moderate"
            } else if (abs_r < 0.7 - eps) {
                "strong"
            } else {
                "very_strong"
            }
        },

        # Interpret effect size using complete translatable sentences.
        # NOTE: this is a variance-explained (R-squared) scale and is deliberately
        # distinct from .correlationBandKey() (which classifies |r|). The R-squared
        # thresholds below are intentionally conservative for clinical reporting.
        .interpretEffectSize = function(r_squared) {
            if (r_squared >= 0.5) {
                .("Large effect size. Whether an association of this magnitude matters in a given setting depends on the outcome and the context, and cannot be read off R-squared alone.")
            } else if (r_squared >= 0.25) {
                .("Medium effect size. Whether an association of this magnitude matters in a given setting depends on the outcome and the context, and cannot be read off R-squared alone.")
            } else if (r_squared >= 0.1) {
                .("Small effect size. Whether an association of this magnitude matters in a given setting depends on the outcome and the context, and cannot be read off R-squared alone.")
            } else {
                .("Very small effect size. Whether an association of this magnitude matters in a given setting depends on the outcome and the context, and cannot be read off R-squared alone.")
            }
        },

        # Enhanced correlation interpretation with clinical context
        .interpretCorrelation = function(r, p_value, has_repeated_measures = FALSE, has_grouping = FALSE) {
            if (is.na(r) || is.na(p_value)) return(.("Not available"))

            is_sig <- is.finite(p_value) && p_value < 0.05
            correlation_is_zero <- isTRUE(all.equal(
                unname(r), 0, tolerance = sqrt(.Machine$double.eps)))

            # Complete translatable phrases per (band, direction, significance)
            base_interpretation <- if (correlation_is_zero) {
                if (is_sig) {
                    .("negligible correlation, statistically significant")
                } else {
                    .("negligible correlation, not statistically significant")
                }
            } else {
                interp_key <- paste(
                    private$.correlationBandKey(r),
                    if (r > 0) "pos" else "neg",
                    if (is_sig) "sig" else "ns",
                    sep = "_"
                )
                switch(interp_key,
                    negligible_pos_sig  = .("negligible positive correlation, statistically significant"),
                    negligible_pos_ns   = .("negligible positive correlation, not statistically significant"),
                    negligible_neg_sig  = .("negligible negative correlation, statistically significant"),
                    negligible_neg_ns   = .("negligible negative correlation, not statistically significant"),
                    weak_pos_sig        = .("weak positive correlation, statistically significant"),
                    weak_pos_ns         = .("weak positive correlation, not statistically significant"),
                    weak_neg_sig        = .("weak negative correlation, statistically significant"),
                    weak_neg_ns         = .("weak negative correlation, not statistically significant"),
                    moderate_pos_sig    = .("moderate positive correlation, statistically significant"),
                    moderate_pos_ns     = .("moderate positive correlation, not statistically significant"),
                    moderate_neg_sig    = .("moderate negative correlation, statistically significant"),
                    moderate_neg_ns     = .("moderate negative correlation, not statistically significant"),
                    strong_pos_sig      = .("strong positive correlation, statistically significant"),
                    strong_pos_ns       = .("strong positive correlation, not statistically significant"),
                    strong_neg_sig      = .("strong negative correlation, statistically significant"),
                    strong_neg_ns       = .("strong negative correlation, not statistically significant"),
                    very_strong_pos_sig = .("very strong positive correlation, statistically significant"),
                    very_strong_pos_ns  = .("very strong positive correlation, not statistically significant"),
                    very_strong_neg_sig = .("very strong negative correlation, statistically significant"),
                    very_strong_neg_ns  = .("very strong negative correlation, not statistically significant"),
                    .("Not available")
                )
            }

            # Add clinical context (emit repeated-measures and grouping cautions independently)
            notes <- character()
            if (has_repeated_measures) {
                notes <- c(notes, .("Caution: Repeated measures detected. Standard correlation assumes independence and may overstate significance."))
            }
            if (has_grouping) {
                notes <- c(notes, .("Caution: Grouped data detected. Correlation across groups may mask within-group patterns (Simpson's paradox)."))
            }
            if (length(notes) == 0L) {
                notes <- .("Correlation does not imply causation.")
            }

            return(paste0(base_interpretation, " - ", paste(notes, collapse = " ")))
        },

        # Enhanced color palette with accessibility focus (honors jamovi theme$palette)
        .getColorPalette = function(n_colors, theme = NULL) {
            palette_name <- self$options$colorPalette

            if (identical(palette_name, "default") && !is.null(theme) && !is.null(theme$palette)) {
                jmv_pal <- tryCatch(
                    jmvcore::colorPalette(n = n_colors, pal = theme$palette),
                    error = function(e) NULL
                )
                if (!is.null(jmv_pal) && length(jmv_pal) >= n_colors) {
                    return(jmv_pal[1:n_colors])
                }
            }

            # Enhanced palettes with better contrast and colorblind accessibility
            palette_colors <- switch(palette_name,
                "default" = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#7FB069", "#8E6C8A"),
                "colorblind" = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000"),
                "viridis" = c("#440154", "#31688e", "#35b779", "#fde725", "#21908c", "#5dc863"),
                "clinical" = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b"),
                # Default fallback with high contrast
                c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#7FB069", "#8E6C8A")
            )

            # Interpolate additional distinct colors when more groups are
            # requested than the base palette provides.
            if (n_colors > length(palette_colors)) {
                palette_colors <- grDevices::colorRampPalette(palette_colors)(n_colors)
            }

            return(palette_colors[1:n_colors])
        },

        # Generate statistical assumptions panel
        .populateAssumptions = function(data, correlation_stats = NULL) {
            xvar <- self$options$xvar
            yvar <- self$options$yvar

            # Build assumptions content
            assumptions_html <- paste0(
                "<div class='alert alert-info'>",
                "<h5>", .("Statistical Assumptions & Guidelines"), "</h5>",
                "<p>", .("Understanding the assumptions behind your analysis helps ensure valid interpretation."), "</p>",
                "</div>"
            )

            # Core assumptions for line charts
            assumptions_html <- paste0(assumptions_html,
                "<div class='card'>",
                "<div class='card-header'><strong>", .("Line Chart Assumptions"), "</strong></div>",
                "<div class='card-body'>",
                "<ul>",
                "<li>", .("<strong>Continuous Y-variable:</strong> The outcome variable should be numeric and measurable."), "</li>",
                "<li>", .("<strong>Ordered X-variable:</strong> Time points or sequence should have meaningful order."), "</li>",
                "<li>", .("<strong>Independence:</strong> Observations should be independent unless using appropriate methods for correlated data."), "</li>",
                "</ul>",
                "</div>",
                "</div><br>"
            )

            # Trend analysis assumptions (if applicable)
            if (isTRUE(self$options$trendline) && !is.null(correlation_stats)) {
                x_data <- data[[xvar]]
                y_data <- data[[yvar]]

                # Pre-fit linear model once for numeric X
                lm_model <- if (is.numeric(x_data) && !is.null(correlation_stats$pearson_r)) {
                    stats::lm(y_data ~ x_data)
                } else {
                    NULL
                }
                residuals_vec <- if (!is.null(lm_model)) stats::residuals(lm_model) else NULL
                y_sd <- if (is.numeric(y_data)) stats::sd(y_data) else NA_real_
                is_exact_fit <- !is.null(residuals_vec) && is.finite(y_sd) && y_sd > 0 &&
                    max(abs(residuals_vec)) <= sqrt(.Machine$double.eps) * y_sd

                # Check linearity using nested polynomial partial F-test (cubic when >= 4
                # distinct X and n > 4, else quadratic when >= 3 distinct X and n > 3)
                # rather than an unadjusted delta-R^2 heuristic.
                linearity_check <- if (!is.null(lm_model)) {
                    if (is_exact_fit) {
                        .("The linear relationship assumption appears reasonable (exact linear fit).")
                    } else {
                        n_ux <- length(unique(x_data))
                        poly_deg <- if (n_ux >= 4L && length(x_data) > 4L) {
                            3L
                        } else if (n_ux >= 3L && length(x_data) > 3L) {
                            2L
                        } else {
                            0L
                        }
                        poly_model <- if (poly_deg >= 2L) {
                            tryCatch(stats::lm(y_data ~ stats::poly(x_data, poly_deg)), error = function(e) NULL)
                        } else {
                            NULL
                        }
                        if (!is.null(poly_model)) {
                            cmp <- tryCatch(stats::anova(lm_model, poly_model), error = function(e) NULL)
                            p_nl <- if (!is.null(cmp) && nrow(cmp) >= 2L) cmp$`Pr(>F)`[2] else NA_real_
                            if (is.finite(p_nl) && p_nl < 0.05) {
                                nl_msg <- .fmt(
                                    .("Potential non-linear relationship detected (polynomial partial F-test p = {p}). Consider polynomial or spline fitting."),
                                    p = private$.fmtNum(p_nl))
                                private$.emitNotice(
                                    name = "nonLinearTrend",
                                    type = jmvcore::NoticeType$STRONG_WARNING,
                                    content = nl_msg
                                )
                                nl_msg
                            } else if (is.finite(p_nl)) {
                                .fmt(.("The linear relationship assumption appears reasonable (polynomial partial F-test p = {p})."),
                                     p = private$.fmtNum(p_nl))
                            } else {
                                .("Unable to assess linearity automatically.")
                            }
                        } else {
                            .("Unable to assess linearity automatically.")
                        }
                    }
                } else if (is.numeric(x_data)) {
                    .("Unable to assess linearity automatically.")
                } else {
                    .("Linearity assessment not applicable for categorical X-variable.")
                }

                # Check normality and homoscedasticity of residuals
                homoscedasticity_check <- .("Homoscedasticity check not applicable for this analysis.")
                normality_check <- if (!is.null(lm_model)) {
                    if (is_exact_fit) {
                        homoscedasticity_check <- .("Exact linear fit (zero residuals within numerical precision); constant-variance test is not required.")
                        .("Exact linear fit (zero residuals within numerical precision); residual normality test is not required.")
                    } else {
                        # Use x_data when the fitted slope is numerically zero so constant
                        # fitted values do not blind the check to heteroscedasticity across X.
                        spread_ref <- if (isTRUE(correlation_stats$slope_is_zero)) {
                            x_data
                        } else {
                            stats::fitted(lm_model)
                        }
                        homoscedasticity_check <- if (length(residuals_vec) >= 4) {
                            bp <- tryCatch(
                                suppressWarnings(stats::cor.test(abs(residuals_vec), spread_ref,
                                                                 method = "spearman", exact = FALSE)),
                                error = function(e) NULL)
                            if (is.null(bp) || is.na(bp$p.value)) {
                                .("Unable to assess constant variance automatically.")
                            } else if (bp$p.value <= 0.05) {
                                het_msg <- .fmt(
                                    .("Residual spread changes with the fitted value (Spearman p = {p}), so the constant-variance assumption is doubtful. Confidence intervals and p-values from the linear fit may be too narrow."),
                                    p = private$.fmtNum(bp$p.value))
                                private$.emitNotice(
                                    name = "heteroscedasticity",
                                    type = jmvcore::NoticeType$WARNING,
                                    content = het_msg
                                )
                                het_msg
                            } else {
                                .fmt(.("Residual spread looks constant across fitted values (Spearman p = {p})."),
                                     p = private$.fmtNum(bp$p.value))
                            }
                        } else {
                            .("Insufficient data to assess constant variance.")
                        }

                        if (length(residuals_vec) >= 3 && length(residuals_vec) <= 5000) {
                            shapiro_result <- tryCatch({
                                stats::shapiro.test(residuals_vec)
                            }, error = function(e) NULL)

                            if (!is.null(shapiro_result)) {
                                if (shapiro_result$p.value > 0.05) {
                                    .("Residuals appear normally distributed (Shapiro-Wilk p > 0.05).")
                                } else {
                                    .("Residuals may not be normally distributed (Shapiro-Wilk p \u2264 0.05). Consider robust methods.")
                                }
                            } else {
                                .("Unable to test normality of residuals.")
                            }
                        } else if (length(residuals_vec) > 5000) {
                            .("Shapiro-Wilk normality test is limited to 5,000 observations; inspect the residual distribution visually.")
                        } else {
                            .("Insufficient data for residual normality testing.")
                        }
                    }
                } else {
                    .("Normality check not applicable for this analysis.")
                }

                if (isTRUE(correlation_stats$has_repeated_measures)) {
                    private$.emitNotice(
                        name = "independenceCaution",
                        type = jmvcore::NoticeType$STRONG_WARNING,
                        content = .("Multiple observations per X value were detected; standard correlation and regression p-values assume independent observations.")
                    )
                }

                assumptions_html <- paste0(assumptions_html,
                    "<div class='card'>",
                    "<div class='card-header'><strong>", .("Trend Analysis Assumptions"), "</strong></div>",
                    "<div class='card-body'>",
                    "<h6>", .("Key Assumptions:"), "</h6>",
                    "<ul>",
                    "<li>", .("<strong>Linearity:</strong> The relationship between variables is approximately linear."), "</li>",
                    "<li>", .("<strong>Independence:</strong> Data points are independent of each other."), "</li>",
                    "<li>", .("<strong>Normality:</strong> Residuals are approximately normally distributed."), "</li>",
                    "<li>", .("<strong>Homoscedasticity:</strong> Variance of residuals is constant across fitted values."), "</li>",
                    "</ul>",
                    "<h6>", .("Assumption Checks:"), "</h6>",
                    "<ul>",
                    "<li>", linearity_check, "</li>",
                    "<li>", normality_check, "</li>",
                    "<li>", homoscedasticity_check, "</li>",
                    "</ul>",
                    "</div>",
                    "</div><br>"
                )
            }

            # Clinical interpretation guidelines
            assumptions_html <- paste0(assumptions_html,
                "<div class='card'>",
                "<div class='card-header'><strong>", .("Clinical Interpretation Guidelines"), "</strong></div>",
                "<div class='card-body'>",
                "<h6>", .("Effect Size Interpretation (Illustrative Conventions):"), "</h6>",
                "<ul>",
                "<li>", .("<strong>R\u00b2 \u2265 0.50:</strong> Large effect - at least 50% of the variance accounted for"), "</li>",
                "<li>", .("<strong>R\u00b2 = 0.25 to &lt; 0.50:</strong> Medium effect - 25% to under 50% of the variance accounted for"), "</li>",
                "<li>", .("<strong>R\u00b2 = 0.10 to &lt; 0.25:</strong> Small effect - 10% to under 25% of the variance accounted for"), "</li>",
                "<li>", .("<strong>R\u00b2 &lt; 0.10:</strong> Very small effect - under 10% of the variance accounted for"), "</li>",
                "</ul>",
                "<h6>", .("Confidence Intervals:"), "</h6>",
                "<p>", .("When displayed, confidence intervals show the uncertainty around trend lines. Wider intervals indicate greater uncertainty."), "</p>",
                "<h6>", .("Sample Size Considerations:"), "</h6>",
                "<p>", .("Minimum recommendations: 10+ observations for basic trends, 30+ for reliable confidence intervals, 50+ for robust correlation analysis."), "</p>",
                "</div>",
                "</div>"
            )

            self$results$assumptions$setContent(assumptions_html)
        },

        # Enhanced plot theme with accessibility improvements
        .getPlotTheme = function(ggtheme = NULL) {
            theme_name <- self$options$theme
            default_theme <- if (!is.null(ggtheme)) ggtheme else ggplot2::theme_gray()

            base_theme <- switch(theme_name,
                "jamovi" = default_theme,
                "default" = default_theme,
                "minimal" = ggplot2::theme_minimal(),
                "classic" = ggplot2::theme_classic(),
                "publication" = ggplot2::theme_bw() +
                    ggplot2::theme(
                        panel.grid.minor = ggplot2::element_blank(),
                        strip.background = ggplot2::element_blank(),
                        legend.position = "bottom",
                        # Enhanced accessibility
                        text = ggplot2::element_text(size = 12, colour = "black"),
                        axis.title = ggplot2::element_text(size = 14, colour = "black"),
                        axis.text = ggplot2::element_text(size = 11, colour = "black"),
                        plot.title = ggplot2::element_text(size = 16, hjust = 0.5, colour = "black", face = "bold"),
                        legend.text = ggplot2::element_text(size = 11, colour = "black"),
                        legend.title = ggplot2::element_text(size = 12, colour = "black", face = "bold"),
                        # Improve contrast
                        panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.5),
                        axis.line = ggplot2::element_line(colour = "black", linewidth = 0.5)
                    ),
                default_theme  # fallback
            )

            # Add custom modifications for clinical publications with accessibility
            if (theme_name == "publication") {
                base_theme <- base_theme +
                    ggplot2::theme(
                        # High contrast for accessibility
                        panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
                        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                        # Better grid lines
                        panel.grid.major = ggplot2::element_line(colour = "grey80", linewidth = 0.3),
                        # Enhanced text readability
                        axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.5),
                        axis.ticks.length = ggplot2::unit(0.15, "cm")
                    )
            }

            return(base_theme)
        },

        # Main plotting function - now modular and testable
        .plot = function(image, ggtheme, theme, ...) {
            # Get plot data
            plot_data <- image$state
            if (is.null(plot_data)) return(FALSE)

            private$.checkpoint()  # Checkpoint before plot generation

            # Check dependencies before plotting
            private$.checkDependencies()

            # Build plot using modular approach
            p <- private$.buildBasePlot(plot_data)
            p <- private$.addGroupingAndLines(p, plot_data, theme = theme)
            p <- private$.addTrendlines(p, plot_data)
            p <- private$.addReferenceLines(p, plot_data)
            p <- private$.addLabelsAndTheme(p, plot_data, ggtheme = ggtheme)

            # Print plot
            print(p)
            TRUE
        },

        # Save plot data
        .savePlotData = function(data) {
            # Coerce to plain data.frame before setState() to avoid tibble serialization issues
            self$results$plot$setState(as.data.frame(data))
        },

        # === NEW HELPER FUNCTIONS FOR MODULAR DESIGN ===

        # Check dependencies at runtime
        .checkDependencies = function() {
            # ggplot2 only. dplyr used to be listed here but is never called
            # anywhere in this analysis, so a missing dplyr aborted the run for a
            # package it does not use.
            required_packages <- c("ggplot2")
            missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

            if (length(missing_packages) > 0) {
                pkg_list <- paste(missing_packages, collapse = ", ")
                jmvcore::reject(.fmt(
                    .("This analysis needs the {pkgs} package(s), which are not installed. Install them and run the analysis again."),
                    pkgs = pkg_list))
            }
        },

        # Enhanced welcome message
        .showWelcomeMessage = function() {
            welcome_msg <- paste0(
                "<div class='alert alert-info'>",
                "<h4>", .("Welcome to Line Chart Analysis"), "</h4>",
                "<p>", .("This function creates comprehensive line charts for time series and trend analysis in clinical research."), "</p>",

                "<div class='alert alert-success'>",
                "<h5>", .("Quick Start Guide:"), "</h5>",
                "<ol>",
                "<li>", .("Select your X-axis variable (time points, visits, or sequence)"), "</li>",
                "<li>", .("Select your Y-axis variable (measurements, lab values, or biomarkers)"), "</li>",
                "<li>", .("Optionally add a grouping variable (treatment arms, patient groups)"), "</li>",
                "<li>", .("Choose display options (confidence intervals, trend lines, reference lines)"), "</li>",
                "</ol>",
                "</div>",

                "<h5>", .("Clinical Applications:"), "</h5>",
                "<div class='row'>",
                "<div class='col-md-6'>",
                "<ul>",
                "<li>", .("Longitudinal biomarker tracking"), "</li>",
                "<li>", .("Treatment response monitoring"), "</li>",
                "<li>", .("Laboratory value trends"), "</li>",
                "</ul>",
                "</div>",
                "<div class='col-md-6'>",
                "<ul>",
                "<li>", .("Disease progression analysis"), "</li>",
                "<li>", .("Dose-response relationships"), "</li>",
                "<li>", .("Quality metrics over time"), "</li>",
                "</ul>",
                "</div>",
                "</div>",

                "<div class='alert alert-warning'>",
                "<h6>", .("Statistical Notes:"), "</h6>",
                "<ul>",
                "<li>", .("Minimum 3 time points recommended for trend analysis"), "</li>",
                "<li>", .("Consider using confidence intervals for small sample sizes"), "</li>",
                "<li>", .("Reference lines help identify clinically significant thresholds"), "</li>",
                "</ul>",
                "</div>",
                "</div>"
            )

            self$results$todo$setContent(welcome_msg)
        },

        # Data quality and misuse detection via native single-line jamovi Notices
        .checkDataQuality = function(data) {
            xvar <- self$options$xvar
            groupby <- self$options$groupby

            strong_notices <- list()
            warn_notices <- list()
            info_notices <- list()

            # A line needs at least two distinct x positions. With one, the chart is
            # a vertical strip of points, and trend/correlation statistics describe nothing.
            n_x <- length(unique(data[[xvar]]))
            if (n_x < 2) {
                strong_notices[["singleXValue"]] <- .fmt(
                    .("All observations share a single X value ({val}). A line chart needs at least two distinct X positions; no trend can be shown or estimated from one."),
                    val = base::format(unique(data[[xvar]])[1]))
            }

            # Minimum sample-size guards for overlays and inferential summaries
            if (isTRUE(self$options$trendline) && nrow(data) < 5) {
                strong_notices[["smallSampleTrend"]] <- .fmt(
                    .("Less than 5 data points available for trend analysis (n = {n}). Results may be unreliable."),
                    n = nrow(data))
            }

            if (isTRUE(self$options$confidence) && nrow(data) < 10) {
                strong_notices[["smallSampleCI"]] <- .fmt(
                    .("Small sample size (< 10 observations; n = {n}) may produce unreliable confidence intervals."),
                    n = nrow(data))
            }

            # Grouping checks (drop unused 0-count factor levels)
            if (!is.null(groupby)) {
                n_groups <- length(unique(data[[groupby]]))
                if (n_groups > 8) {
                    warn_notices[["manyGroups"]] <- .fmt(
                        .("Many groups detected ({n}). Consider reducing groups or using faceting for clarity."),
                        n = n_groups)
                }

                group_counts <- table(droplevels(data[[groupby]]))
                if (length(group_counts) > 0 && min(group_counts) < 3) {
                    strong_notices[["smallGroupSize"]] <- .fmt(
                        .("Some groups have very few observations (< 3; minimum group n = {minN}). Consider combining small groups."),
                        minN = as.integer(min(group_counts)))
                }
            }

            # Exclusions: every statistic is computed on surviving rows
            if (private$.n_excluded_missing > 0) {
                warn_notices[["excludedMissing"]] <- .fmt(
                    .("{excluded} row(s) were excluded because of missing values; {analyzed} rows were analyzed."),
                    excluded = private$.n_excluded_missing,
                    analyzed = private$.n_rows_analysed)
            }

            if (private$.n_excluded_nonfinite > 0) {
                warn_notices[["excludedNonFinite"]] <- .fmt(
                    .("{excluded} row(s) held infinite values (Inf or -Inf) and were excluded; {analyzed} rows were analyzed. Infinite values are usually a division by zero or a log of zero upstream - check the source variable."),
                    excluded = private$.n_excluded_nonfinite,
                    analyzed = private$.n_rows_analysed)
            }

            # X-axis spacing for time series (scale-invariant relative tolerance)
            if (is.numeric(data[[xvar]])) {
                x_diff <- diff(sort(unique(data[[xvar]])))
                max_diff <- if (length(x_diff) > 0) max(abs(x_diff)) else 0
                if (length(x_diff) > 1 && is.finite(max_diff) && max_diff > 0 &&
                    length(unique(round(x_diff / max_diff, 6))) > 1) {
                    warn_notices[["unequalIntervals"]] <- .("Unequal time intervals detected. Trend interpretation should account for irregular spacing.")
                }
            }

            # Disclose what the shaded confidence band actually represents (model fit vs observed line)
            if (isTRUE(self$options$confidence)) {
                info_notices[["confidenceBandInfo"]] <- if (isTRUE(self$options$smooth)) {
                    .("The shaded band is the 95% confidence interval of a LOESS fit, not the uncertainty of the plotted points themselves.")
                } else {
                    .("The shaded band is the 95% confidence interval of a straight-line (linear regression) fit, while the plotted line connects the observed values. The band therefore describes a linear trend, not the line you see; enable Smooth line if you want the band and the line to match.")
                }
            }

            # Emit in strict priority order: STRONG_WARNING -> WARNING at top, INFO at bottom
            for (nm in names(strong_notices)) {
                private$.emitNotice(nm, jmvcore::NoticeType$STRONG_WARNING, strong_notices[[nm]], position = "top")
            }
            for (nm in names(warn_notices)) {
                private$.emitNotice(nm, jmvcore::NoticeType$WARNING, warn_notices[[nm]], position = "top")
            }
            for (nm in names(info_notices)) {
                private$.emitNotice(nm, jmvcore::NoticeType$INFO, info_notices[[nm]], position = "bottom")
            }
        },

        # Generate natural language summary.
        # correlation_stats is threaded in from .run() (computed once) to avoid a
        # second cor.test/lm pass; falls back to computing it only if not supplied.
        .generateNaturalSummary = function(summary_stats, data, correlation_stats = NULL) {
            xvar <- self$options$xvar
            yvar <- self$options$yvar
            groupby <- self$options$groupby

            # Basic summary using whole-sentence i18n templates
            summary_text <- paste0(
                "<div class='alert alert-info'>",
                "<h5>", .("Analysis Summary"), "</h5>",
                "<p><strong>", .("Dataset:"), "</strong> ",
                .fmt(.("{nobs} observations across {nx} time points."),
                     nobs = summary_stats$n_observations,
                     nx = summary_stats$n_x_points),
                "</p>"
            )

            # Add group information (group_names is user data -> escape for HTML)
            if (!is.null(groupby) && summary_stats$n_groups > 1) {
                summary_text <- paste0(
                    summary_text,
                    "<p><strong>", .("Groups:"), "</strong> ",
                    .fmt(.("{ngroups} groups ({names})."),
                         ngroups = summary_stats$n_groups,
                         names = jmvcore::htmlEscape(summary_stats$group_names)),
                    "</p>"
                )
            }

            # Add Y variable summary (yvar is a user column name -> escape for HTML)
            summary_text <- paste0(
                summary_text,
                "<p>",
                .fmt(.("{yvar} ranges from {ymin} to {ymax} (mean: {mean}, SD: {sd})."),
                     yvar = paste0("<strong>", jmvcore::htmlEscape(yvar), "</strong>"),
                     ymin = private$.fmtNum(summary_stats$y_min),
                     ymax = private$.fmtNum(summary_stats$y_max),
                     mean = private$.fmtNum(summary_stats$y_mean),
                     sd = private$.fmtNum(summary_stats$y_sd)),
                "</p>"
            )

            # Add trend information if available
            if (isTRUE(self$options$trendline) && is.numeric(data[[xvar]])) {
                # Reuse the correlation computed once in .run(); compute here only
                # if it was not supplied (defensive fallback).
                if (is.null(correlation_stats)) {
                    correlation_stats <- private$.calculateCorrelation(data)
                }
                if (!is.null(correlation_stats$slope)) {
                    slope_is_zero <- isTRUE(correlation_stats$slope_is_zero)
                    p_val <- correlation_stats$pearson_p
                    is_sig <- is.finite(p_val) && p_val < 0.05
                    p_txt <- if (is.na(p_val)) {
                        "p = NA"
                    } else if (p_val < 0.001) {
                        "p < 0.001"
                    } else if (p_val < 0.05 && round(p_val, 3) >= 0.05) {
                        sprintf("p = %.4f", p_val)
                    } else {
                        sprintf("p = %.3f", p_val)
                    }
                    has_groups <- !is.null(groupby)
                    trend_sentence <- if (slope_is_zero) {
                        .fmt(
                            .("No clear linear trend was detected (R\u00b2 = {r2})."),
                            r2 = round(correlation_stats$r_squared, 3))
                    } else if (!is_sig) {
                        if (has_groups) {
                            .fmt(
                                .("Across all groups pooled (which may differ from within-group lines), no statistically significant linear trend was detected (r = {r}, {p}, R\u00b2 = {r2})."),
                                r = sprintf("%.3f", correlation_stats$pearson_r),
                                p = p_txt,
                                r2 = round(correlation_stats$r_squared, 3))
                        } else {
                            .fmt(
                                .("No statistically significant linear trend was detected (r = {r}, {p}, R\u00b2 = {r2})."),
                                r = sprintf("%.3f", correlation_stats$pearson_r),
                                p = p_txt,
                                r2 = round(correlation_stats$r_squared, 3))
                        }
                    } else {
                        ns_key <- paste(
                            private$.correlationBandKey(correlation_stats$pearson_r),
                            if (correlation_stats$slope > 0) "inc" else "dec",
                            if (has_groups) "grp" else "one",
                            sep = "_"
                        )
                        ns_tpl <- switch(ns_key,
                            negligible_inc_grp  = .("Across all groups pooled (which may differ from within-group lines), a negligible increasing linear trend was observed (r = {r}, {p}, R\u00b2 = {r2})."),
                            negligible_dec_grp  = .("Across all groups pooled (which may differ from within-group lines), a negligible decreasing linear trend was observed (r = {r}, {p}, R\u00b2 = {r2})."),
                            weak_inc_grp        = .("Across all groups pooled (which may differ from within-group lines), a weak increasing linear trend was observed (r = {r}, {p}, R\u00b2 = {r2})."),
                            weak_dec_grp        = .("Across all groups pooled (which may differ from within-group lines), a weak decreasing linear trend was observed (r = {r}, {p}, R\u00b2 = {r2})."),
                            moderate_inc_grp    = .("Across all groups pooled (which may differ from within-group lines), a moderate increasing linear trend was observed (r = {r}, {p}, R\u00b2 = {r2})."),
                            moderate_dec_grp    = .("Across all groups pooled (which may differ from within-group lines), a moderate decreasing linear trend was observed (r = {r}, {p}, R\u00b2 = {r2})."),
                            strong_inc_grp      = .("Across all groups pooled (which may differ from within-group lines), a strong increasing linear trend was observed (r = {r}, {p}, R\u00b2 = {r2})."),
                            strong_dec_grp      = .("Across all groups pooled (which may differ from within-group lines), a strong decreasing linear trend was observed (r = {r}, {p}, R\u00b2 = {r2})."),
                            very_strong_inc_grp = .("Across all groups pooled (which may differ from within-group lines), a very strong increasing linear trend was observed (r = {r}, {p}, R\u00b2 = {r2})."),
                            very_strong_dec_grp = .("Across all groups pooled (which may differ from within-group lines), a very strong decreasing linear trend was observed (r = {r}, {p}, R\u00b2 = {r2})."),
                            negligible_inc_one  = .("A negligible increasing trend was detected (r = {r}, {p}, R\u00b2 = {r2})."),
                            negligible_dec_one  = .("A negligible decreasing trend was detected (r = {r}, {p}, R\u00b2 = {r2})."),
                            weak_inc_one        = .("A weak increasing trend was detected (r = {r}, {p}, R\u00b2 = {r2})."),
                            weak_dec_one        = .("A weak decreasing trend was detected (r = {r}, {p}, R\u00b2 = {r2})."),
                            moderate_inc_one    = .("A moderate increasing trend was detected (r = {r}, {p}, R\u00b2 = {r2})."),
                            moderate_dec_one    = .("A moderate decreasing trend was detected (r = {r}, {p}, R\u00b2 = {r2})."),
                            strong_inc_one      = .("A strong increasing trend was detected (r = {r}, {p}, R\u00b2 = {r2})."),
                            strong_dec_one      = .("A strong decreasing trend was detected (r = {r}, {p}, R\u00b2 = {r2})."),
                            very_strong_inc_one = .("A very strong increasing trend was detected (r = {r}, {p}, R\u00b2 = {r2})."),
                            very_strong_dec_one = .("A very strong decreasing trend was detected (r = {r}, {p}, R\u00b2 = {r2})."),
                            .("A linear trend was detected (r = {r}, {p}, R\u00b2 = {r2}).")
                        )
                        .fmt(
                            ns_tpl,
                            r = sprintf("%.3f", correlation_stats$pearson_r),
                            p = p_txt,
                            r2 = round(correlation_stats$r_squared, 3)
                        )
                    }

                    summary_text <- paste0(
                        summary_text,
                        "<p><strong>", .("Trend:"), "</strong> ",
                        trend_sentence, "</p>")
                }
            }

            summary_text <- paste0(summary_text, "</div>")
            self$results$naturalSummary$setContent(summary_text)
        },

        # === MODULAR PLOT BUILDING FUNCTIONS ===

        # Build base plot with modern aes() syntax
        .buildBasePlot = function(plot_data) {
            xvar <- self$options$xvar
            yvar <- self$options$yvar

            # Use modern aes() with .data pronoun instead of deprecated aes_string()
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]]))
            return(p)
        },

        # Add grouping, lines, confidence intervals, and points
        .addGroupingAndLines = function(p, plot_data, theme = NULL) {
            groupby <- self$options$groupby

            if (!is.null(groupby)) {
                # Add grouping aesthetics
                p <- p + ggplot2::aes(color = .data[[groupby]], group = .data[[groupby]])

                # Get colors
                n_groups <- length(unique(plot_data[[groupby]]))
                colors <- private$.getColorPalette(n_groups, theme = theme)

                if (isTRUE(self$options$smooth)) {
                    if (isTRUE(self$options$confidence)) {
                        private$.checkpoint(flush = FALSE)
                        p <- p + ggplot2::geom_smooth(
                            ggplot2::aes(fill = .data[[groupby]]),
                            method = "loess", se = TRUE, alpha = 0.2, linewidth = 1.2
                        ) + ggplot2::scale_fill_manual(values = colors, guide = "none")
                    } else {
                        p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE, linewidth = 1.2)
                    }
                } else {
                    if (isTRUE(self$options$confidence)) {
                        private$.checkpoint(flush = FALSE)
                        # Draw shaded CI ribbon only (colour = NA suppresses an unrequested solid regression line)
                        p <- p + ggplot2::geom_smooth(
                            ggplot2::aes(fill = .data[[groupby]]),
                            method = "lm", se = TRUE, alpha = 0.2, colour = NA
                        ) + ggplot2::scale_fill_manual(values = colors, guide = "none")
                    }
                    p <- p + ggplot2::geom_line(linewidth = 1.2)
                }

                # Add points if requested
                if (isTRUE(self$options$points)) {
                    p <- p + ggplot2::geom_point(size = 2.5, alpha = 0.8)
                }

                # Set colors
                p <- p + ggplot2::scale_color_manual(values = colors)

            } else {
                # Single group: set group = 1 so discrete/factor X levels connect into one line
                p <- p + ggplot2::aes(group = 1)
                color <- private$.getColorPalette(1, theme = theme)

                if (isTRUE(self$options$smooth)) {
                    if (isTRUE(self$options$confidence)) {
                        private$.checkpoint(flush = FALSE)
                        p <- p + ggplot2::geom_smooth(
                            method = "loess", se = TRUE, alpha = 0.2,
                            color = color, fill = color, linewidth = 1.2
                        )
                    } else {
                        p <- p + ggplot2::geom_smooth(
                            method = "loess", se = FALSE, color = color, linewidth = 1.2
                        )
                    }
                } else {
                    if (isTRUE(self$options$confidence)) {
                        private$.checkpoint(flush = FALSE)
                        p <- p + ggplot2::geom_smooth(
                            method = "lm", se = TRUE, alpha = 0.2,
                            colour = NA, fill = color
                        )
                    }
                    p <- p + ggplot2::geom_line(color = color, linewidth = 1.2)
                }

                # Add points if requested
                if (isTRUE(self$options$points)) {
                    p <- p + ggplot2::geom_point(color = color, size = 2.5, alpha = 0.8)
                }
            }

            return(p)
        },

        # Add trend lines
        .addTrendlines = function(p, plot_data) {
            if (!isTRUE(self$options$trendline)) return(p)

            xvar <- self$options$xvar
            groupby <- self$options$groupby

            # Only add trend lines for numeric X variables
            if (is.numeric(plot_data[[xvar]])) {
                if (!is.null(groupby)) {
                    p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = "dashed", alpha = 0.7, linewidth = 0.8)
                } else {
                    p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
                                                 color = "black", alpha = 0.7, linewidth = 0.8)
                }
            }

            return(p)
        },

        # Add reference lines
        .addReferenceLines = function(p, plot_data) {
            # Gated by its own switch rather than by "value != 0". Treating 0 as
            # the off-sentinel made the most common clinical reference line -
            # change from baseline, a difference, a log fold-change - impossible
            # to draw.
            if (!isTRUE(self$options$showRefline)) return(p)

            if (is.null(self$options$refline) || is.na(self$options$refline)) return(p)
            refline_value <- as.numeric(self$options$refline)
            if (is.na(refline_value)) return(p)

            refline_label <- if (!is.null(self$options$reflineLabel) &&
                                nchar(self$options$reflineLabel) > 0) {
                self$options$reflineLabel
            } else {
                .("Reference")
            }

            p <- p +
                ggplot2::geom_hline(yintercept = refline_value,
                                   linetype = "dotted", color = "red", linewidth = 1) +
                ggplot2::annotate("text", x = Inf, y = refline_value,
                                 label = refline_label, hjust = 1.1, vjust = -0.5,
                                 color = "red", size = 3.5, fontface = "bold")

            return(p)
        },

        # Add labels and theme
        .addLabelsAndTheme = function(p, plot_data, ggtheme = NULL) {
            xvar <- self$options$xvar
            yvar <- self$options$yvar
            groupby <- self$options$groupby

            # Determine labels
            xlabel <- if (!is.null(self$options$xlabel) && nchar(self$options$xlabel) > 0) {
                self$options$xlabel
            } else {
                xvar
            }

            ylabel <- if (!is.null(self$options$ylabel) && nchar(self$options$ylabel) > 0) {
                self$options$ylabel
            } else {
                yvar
            }

            plot_title <- if (!is.null(self$options$title) && nchar(self$options$title) > 0) {
                self$options$title
            } else {
                if (!is.null(groupby)) {
                    .fmt(.("{y} by {x} and {group}"), y = ylabel, x = xlabel, group = groupby)
                } else {
                    .fmt(.("{y} by {x}"), y = ylabel, x = xlabel)
                }
            }

            # Add labels
            p <- p + ggplot2::labs(
                x = xlabel,
                y = ylabel,
                title = plot_title,
                color = if (!is.null(groupby)) groupby else NULL
            )

            # Apply theme
            p <- p + private$.getPlotTheme(ggtheme = ggtheme)

            return(p)
        }
    )
)
