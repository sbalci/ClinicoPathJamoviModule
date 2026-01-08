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
#'   refline = 4000,
#'   reflineLabel = "Normal Lower Limit"
#' )
#' }
#'
#' @importFrom R6 R6Class
#' @import jmvcore

linechartClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "linechartClass",
    inherit = linechartBase,
    private = list(

        # Initialize results and validate dependencies
        .init = function() {
            # Check for required packages and validate parameters at initialization
            private$.checkDependencies()
            private$.validateParameters()

            # Initialize with enhanced welcome message if no variables selected
            if (is.null(self$options$xvar) || is.null(self$options$yvar)) {
                private$.showWelcomeMessage()

                # Hide results until data is provided
                self$results$summary$setVisible(FALSE)
                self$results$correlation$setVisible(FALSE)
                self$results$assumptions$setVisible(FALSE)
                self$results$plot$setVisible(FALSE)
            }
        },

        .run = function() {
            # Early exits for missing data or variables
            if (is.null(self$data) || nrow(self$data) == 0) {
                return()
            }

            if (is.null(self$options$xvar) || is.null(self$options$yvar)) {
                return()
            }

            # Hide welcome message and show results
            self$results$todo$setVisible(FALSE)
            self$results$summary$setVisible(TRUE)
            self$results$plot$setVisible(TRUE)

            # Main analysis pipeline with comprehensive error handling
            tryCatch({
                # Check dependencies at runtime
                private$.checkDependencies()

                # Prepare and validate data
                private$.checkpoint()  # Checkpoint before expensive data cleaning
                data <- private$.cleanData()
                if (is.null(data)) return()

                # Perform misuse detection
                private$.checkDataQuality(data)

                # Calculate summary statistics
                private$.checkpoint()  # Checkpoint before statistical calculations
                summary_stats <- private$.calculateSummary(data)
                private$.populateSummary(summary_stats)

                # Generate natural language summary
                private$.generateNaturalSummary(summary_stats, data)

                # Calculate correlation if trend line requested
                if (self$options$trendline) {
                    private$.checkpoint()  # Checkpoint before correlation analysis
                    correlation_stats <- private$.calculateCorrelation(data)
                    private$.populateCorrelation(correlation_stats)
                    private$.populateAssumptions(data, correlation_stats)
                    self$results$correlation$setVisible(TRUE)
                    self$results$assumptions$setVisible(TRUE)
                } else {
                    self$results$correlation$setVisible(FALSE)
                    self$results$assumptions$setVisible(FALSE)
                }

                # Save plot data for rendering
                private$.savePlotData(data)

            }, error = function(e) {
                private$.handleError(e)
            })
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
                stop(paste(.("Variables not found in data:"), paste(missing_vars, collapse = ", ")))
            }

            # Select and clean data
            data <- self$data[required_vars]

            # Validate X variable
            x_data <- data[[xvar]]
            if (is.character(x_data)) {
                # Try to convert to factor with meaningful ordering
                unique_values <- unique(x_data)
                if (all(grepl("^[0-9.-]+$", unique_values, perl = TRUE))) {
                    # Looks numeric, convert
                    x_data <- as.numeric(x_data)
                    if (any(is.na(x_data))) {
                        stop(.("X-axis variable contains non-numeric values that cannot be converted."))
                    }
                } else {
                    # Convert to ordered factor
                    x_data <- factor(x_data)
                }
                data[[xvar]] <- x_data
            } else if (is.factor(x_data)) {
                # Check if factor levels are meaningful for ordering
                levels_numeric <- suppressWarnings(as.numeric(levels(x_data)))
                if (!any(is.na(levels_numeric))) {
                    # Reorder factor levels numerically
                    data[[xvar]] <- factor(x_data, levels = levels(x_data)[order(levels_numeric)])
                }
            }

            # Validate Y variable (must be numeric)
            y_data <- jmvcore::toNumeric(data[[yvar]])
            if (all(is.na(y_data))) {
                stop(.("Y-axis variable must be numeric (continuous variable)."))
            }
            data[[yvar]] <- y_data

            # Validate grouping variable if provided
            if (!is.null(groupby)) {
                group_data <- data[[groupby]]
                if (is.character(group_data)) {
                    data[[groupby]] <- factor(group_data)
                } else if (!is.factor(group_data)) {
                    data[[groupby]] <- factor(group_data)
                }

                # Check number of groups
                n_groups <- length(unique(data[[groupby]]))
                if (n_groups > 10) {
                    warning(.("Grouping variable has more than 10 levels. Consider reducing groups for clarity."))
                }
            }

            # Remove rows with missing values
            complete_before <- nrow(data)
            data <- data[complete.cases(data), ]
            complete_after <- nrow(data)

            if (complete_after == 0) {
                stop(.("No complete cases found. Please check for missing values in selected variables."))
            }

            if (complete_after < complete_before) {
                n_removed <- complete_before - complete_after
                warning(paste(n_removed, .("rows with missing values were removed from analysis.")))
            }

            # Enhanced minimum data requirements with suggestions
            if (nrow(data) < 3) {
                stop(paste0(.("At least 3 complete observations are required for line chart analysis. "),
                           .("Current dataset has "), nrow(data), .(" observations. "),
                           .("Consider checking for missing values or selecting different variables.")))
            }

            # Enhanced variation check with suggestions
            if (var(data[[yvar]], na.rm = TRUE) == 0) {
                stop(paste0(.("Y-axis variable has no variation (all values are identical). "),
                           .("Line charts require variation in the Y variable. "),
                           .("Please select a different variable with varying values.")))
            }

            # Validate reference line if provided
            if (!is.null(self$options$refline) && !is.na(self$options$refline)) {
                refline_value <- as.numeric(self$options$refline)
                if (is.na(refline_value)) {
                    warning(.("Reference line value is not numeric and will be ignored."))
                }
            }

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
            summary_stats$y_median <- median(y_data, na.rm = TRUE)
            summary_stats$y_sd <- sd(y_data, na.rm = TRUE)
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

            # CRITICAL FIX: Check for grouped data or repeated measures
            # and provide both naive and adjusted statistics when appropriate
            n_unique_x <- length(unique(data[[xvar]]))
            n_observations <- nrow(data)
            avg_obs_per_x <- n_observations / n_unique_x

            # Flag if likely repeated measures (multiple obs per x-value)
            has_repeated_measures <- avg_obs_per_x > 1.5  # More than 1.5 obs per x on average

            # Issue warnings for statistical validity
            if (!is.null(groupby)) {
                warning(paste0(
                    "Correlation statistics treat all observations as independent, ",
                    "which may not be appropriate for grouped data. ",
                    "For more rigorous analysis of grouped longitudinal data, ",
                    "consider mixed-effects models using additional software."
                ))
                correlation_stats$has_grouping <- TRUE
            }

            if (has_repeated_measures) {
                warning(paste0(
                    "Data appears to have repeated measures (multiple observations per time point). ",
                    "Both naive (assumes independence) and patient-level aggregate statistics are provided. ",
                    "Use patient-level results for longitudinal inference."
                ))
                correlation_stats$has_repeated_measures <- TRUE
            }

            # Overall correlation (if X is numeric)
            x_data <- data[[xvar]]
            y_data <- data[[yvar]]

            if (is.numeric(x_data)) {
                # NAIVE STATISTICS (assume independence - for reference only)
                # Pearson correlation
                cor_result <- cor.test(x_data, y_data, method = "pearson")
                correlation_stats$pearson_r_naive <- cor_result$estimate
                correlation_stats$pearson_p_naive <- cor_result$p.value
                correlation_stats$pearson_ci_lower_naive <- cor_result$conf.int[1]
                correlation_stats$pearson_ci_upper_naive <- cor_result$conf.int[2]

                # Spearman correlation (rank-based)
                cor_spearman <- cor.test(x_data, y_data, method = "spearman")
                correlation_stats$spearman_r_naive <- cor_spearman$estimate
                correlation_stats$spearman_p_naive <- cor_spearman$p.value

                # Linear regression statistics
                lm_result <- lm(y_data ~ x_data)
                correlation_stats$slope_naive <- coef(lm_result)[2]
                correlation_stats$intercept_naive <- coef(lm_result)[1]
                correlation_stats$r_squared_naive <- summary(lm_result)$r.squared
                correlation_stats$regression_p_naive <- summary(lm_result)$coefficients[2, 4]

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

                # Add methodology note for interpretation
                if (has_repeated_measures || !is.null(groupby)) {
                    correlation_stats$independence_note <- paste0(
                        "⚠️ IMPORTANT LIMITATION: Statistics assume independent observations. ",
                        "For longitudinal data with repeated measures, these statistics may ",
                        "overstate significance. ",
                        "Consider: (1) Aggregating to patient-level summaries (e.g., slope per patient), ",
                        "(2) Using mixed-effects models in specialized software, or ",
                        "(3) Interpreting p-values as exploratory descriptives only."
                    )
                }

            } else {
                # For categorical X, calculate trend test if ordered
                if (is.ordered(x_data) || is.factor(x_data)) {
                    # Jonckheere-Terpstra trend test would be ideal here
                    # For now, use basic ANOVA
                    anova_result <- anova(lm(y_data ~ x_data))
                    correlation_stats$anova_f <- anova_result$`F value`[1]
                    correlation_stats$anova_p <- anova_result$`Pr(>F)`[1]
                }
            }

            return(correlation_stats)
        },

        # Populate summary table
        .populateSummary = function(summary_stats) {
            table <- self$results$summary
            table$deleteRows()

            row_num <- 1

            # Data characteristics
            table$addRow(rowKey = row_num, values = list(
                statistic = .("Number of Observations"),
                value = as.character(summary_stats$n_observations)
            ))
            row_num <- row_num + 1

            table$addRow(rowKey = row_num, values = list(
                statistic = .("Number of X-axis Points"),
                value = as.character(summary_stats$n_x_points)
            ))
            row_num <- row_num + 1

            table$addRow(rowKey = row_num, values = list(
                statistic = .("Number of Groups"),
                value = as.character(summary_stats$n_groups)
            ))
            row_num <- row_num + 1

            if (summary_stats$n_groups > 1) {
                table$addRow(rowKey = row_num, values = list(
                    statistic = .("Group Names"),
                    value = summary_stats$group_names
                ))
                row_num <- row_num + 1
            }

            # Y variable statistics
            table$addRow(rowKey = row_num, values = list(
                statistic = .("Y Mean"),
                value = format(summary_stats$y_mean, digits = 3)
            ))
            row_num <- row_num + 1

            table$addRow(rowKey = row_num, values = list(
                statistic = .("Y Median"),
                value = format(summary_stats$y_median, digits = 3)
            ))
            row_num <- row_num + 1

            table$addRow(rowKey = row_num, values = list(
                statistic = .("Y Standard Deviation"),
                value = format(summary_stats$y_sd, digits = 3)
            ))
            row_num <- row_num + 1

            table$addRow(rowKey = row_num, values = list(
                statistic = .("Y Range"),
                value = paste(format(summary_stats$y_min, digits = 3), "-",
                             format(summary_stats$y_max, digits = 3))
            ))
        },

        # Enhanced correlation table with copy-ready interpretations
        .populateCorrelation = function(correlation_stats) {
            table <- self$results$correlation
            table$deleteRows()

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
                    interpretation = private$.interpretCorrelation(correlation_stats$pearson_r,
                                                                   correlation_stats$pearson_p,
                                                                   has_repeated_measures,
                                                                   has_grouping)
                ))
                row_num <- row_num + 1

                table$addRow(rowKey = row_num, values = list(
                    measure = .("R-squared (Effect Size)"),
                    value = correlation_stats$r_squared,
                    interpretation = paste0(round(correlation_stats$r_squared * 100, 1),
                                          .("% of variance explained. "),
                                          private$.interpretEffectSize(correlation_stats$r_squared))
                ))
                row_num <- row_num + 1

                # Enhanced slope interpretation
                slope_interpretation <- paste0(
                    if (correlation_stats$slope > 0) .("Positive trend: ") else .("Negative trend: "),
                    .("Each unit increase in X corresponds to "),
                    format(abs(correlation_stats$slope), digits = 3),
                    if (correlation_stats$slope > 0) .(" unit increase") else .(" unit decrease"),
                    .(" in Y on average.")
                )

                table$addRow(rowKey = row_num, values = list(
                    measure = .("Regression Slope"),
                    value = correlation_stats$slope,
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

                # Add independence assumption warning if applicable
                if (!is.null(correlation_stats$independence_note)) {
                    table$addRow(rowKey = row_num, values = list(
                        measure = .("⚠️ Statistical Validity"),
                        value = "",
                        interpretation = correlation_stats$independence_note
                    ))
                    row_num <- row_num + 1
                }
            }

            # Spearman correlation with enhanced interpretation
            if (!is.null(correlation_stats$spearman_r)) {
                table$addRow(rowKey = row_num, values = list(
                    measure = .("Spearman Correlation (Rank-based)"),
                    value = correlation_stats$spearman_r,
                    interpretation = paste0(private$.interpretCorrelation(correlation_stats$spearman_r,
                                                                         correlation_stats$spearman_p,
                                                                         has_repeated_measures,
                                                                         has_grouping),
                                          " ", .("This non-parametric measure is robust to outliers."))
                ))
                row_num <- row_num + 1
            }

            # ANOVA for categorical X with enhanced interpretation
            if (!is.null(correlation_stats$anova_f)) {
                anova_interpretation <- if (correlation_stats$anova_p < 0.05) {
                    paste0(.("Significant differences between groups detected (p < 0.05). "),
                          .("Post-hoc testing recommended to identify specific group differences."))
                } else {
                    paste0(.("No significant differences between groups detected (p ≥ 0.05). "),
                          .("Groups show similar mean values."))
                }

                table$addRow(rowKey = row_num, values = list(
                    measure = .("ANOVA F-statistic"),
                    value = correlation_stats$anova_f,
                    interpretation = anova_interpretation
                ))
            }
        },

        # Generate copy-ready correlation interpretation
        .generateCopyReadyCorrelation = function(correlation_stats) {
            r <- correlation_stats$pearson_r
            p <- correlation_stats$pearson_p
            r_squared <- correlation_stats$r_squared
            
            has_repeated_measures <- isTRUE(correlation_stats$has_repeated_measures)
            has_grouping <- isTRUE(correlation_stats$has_grouping)

            # Determine significance
            sig_level <- if (p < 0.001) "p < 0.001" else if (p < 0.01) "p < 0.01" else paste0("p = ", round(p, 3))
            is_significant <- p < 0.05

            # Build copy-ready sentence
            direction <- if (r > 0) .("positive") else .("negative")
            strength <- if (abs(r) < 0.3) .("weak") else if (abs(r) < 0.5) .("moderate") else .("strong")

            copy_ready <- paste0(
                .("Analysis revealed a "), strength, " ", direction, " ", .("correlation between the variables "),
                "(r = ", round(r, 3), ", ", sig_level, "). ",
                .("The correlation explains "), round(r_squared * 100, 1), .("% of the variance. "),
                if (is_significant) {
                    .("This relationship is statistically significant.")
                } else {
                    .("This relationship is not statistically significant.")
                }
            )
            
            # Add caution if needed
            if (has_repeated_measures) {
                copy_ready <- paste0(copy_ready, " ", .("Note: Results should be interpreted with caution as repeated measures were detected, violating the independence assumption."))
            } else if (has_grouping) {
                copy_ready <- paste0(copy_ready, " ", .("Note: Results should be interpreted with caution as grouped data may mask within-group patterns."))
            } else if (is_significant) {
                copy_ready <- paste0(copy_ready, " ", .("This finding may have clinical relevance."))
            }

            return(copy_ready)
        },

        # Interpret effect size
        .interpretEffectSize = function(r_squared) {
            if (r_squared >= 0.5) {
                return(.("Large effect size - clinically meaningful association."))
            } else if (r_squared >= 0.25) {
                return(.("Medium effect size - moderate practical significance."))
            } else if (r_squared >= 0.1) {
                return(.("Small effect size - limited practical significance."))
            } else {
                return(.("Very small effect size - minimal practical significance."))
            }
        },

        # Enhanced correlation interpretation with clinical context
        .interpretCorrelation = function(r, p_value, has_repeated_measures = FALSE, has_grouping = FALSE) {
            if (is.na(r) || is.na(p_value)) return(.("Not available"))

            # Significance levels with clinical interpretation
            sig_text <- if (p_value < 0.001) "***" else if (p_value < 0.01) "**" else if (p_value < 0.05) "*" else "ns"
            clinical_sig <- if (p_value < 0.05) .("statistically significant") else .("not statistically significant")

            # Strength interpretation with clinical relevance
            abs_r <- abs(r)
            strength <- if (abs_r < 0.1) .("negligible") else
                       if (abs_r < 0.3) .("weak") else
                       if (abs_r < 0.5) .("moderate") else
                       if (abs_r < 0.7) .("strong") else .("very strong")

            direction <- if (r > 0) .("positive") else .("negative")

            # Create copy-ready interpretation
            base_interpretation <- paste0(strength, " ", direction, " ", .("correlation"), " (", sig_text, ")")

            # Add clinical context
            if (has_repeated_measures) {
                clinical_note <- .("Caution: Repeated measures detected. Standard correlation assumes independence and may overstate significance.")
            } else if (has_grouping) {
                clinical_note <- .("Caution: Grouped data detected. Correlation across groups may mask within-group patterns (Simpson's paradox).")
            } else if (abs_r >= 0.5) {
                clinical_note <- .("This suggests a clinically meaningful relationship.")
            } else if (abs_r >= 0.3) {
                clinical_note <- .("This suggests a moderate association worth investigating.")
            } else {
                clinical_note <- .("This suggests a weak association with limited clinical significance.")
            }

            return(paste0(base_interpretation, " - ", clinical_note))
        },

        # Enhanced color palette with accessibility focus
        .getColorPalette = function(n_colors) {
            palette_name <- self$options$colorPalette

            if (n_colors == 1) {
                return("#2E86AB")  # High contrast single color
            }

            # Enhanced palettes with better contrast and colorblind accessibility
            palette_colors <- switch(palette_name,
                "default" = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#7FB069", "#8E6C8A"),
                "colorblind" = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"),
                "viridis" = c("#440154", "#31688e", "#35b779", "#fde725", "#21908c", "#5dc863"),
                "clinical" = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b"),
                # Default fallback with high contrast
                c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#7FB069", "#8E6C8A")
            )

            # Ensure we don't exceed available colors, cycle if needed
            if (n_colors > length(palette_colors)) {
                # Repeat palette if more colors needed
                palette_colors <- rep(palette_colors, ceiling(n_colors / length(palette_colors)))
            }

            return(palette_colors[1:n_colors])
        },

        # Generate statistical assumptions panel
        .populateAssumptions = function(data, correlation_stats = NULL) {
            xvar <- self$options$xvar
            yvar <- self$options$yvar
            groupby <- self$options$groupby

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
            if (self$options$trendline && !is.null(correlation_stats)) {
                x_data <- data[[xvar]]
                y_data <- data[[yvar]]

                # Check linearity (basic)
                linearity_check <- if (is.numeric(x_data)) {
                    # Simple linearity assessment using correlation vs polynomial fit
                    linear_r2 <- correlation_stats$r_squared
                    quadratic_model <- tryCatch({
                        lm(y_data ~ poly(x_data, 2))
                    }, error = function(e) NULL)

                    if (!is.null(quadratic_model)) {
                        quad_r2 <- summary(quadratic_model)$r.squared
                        improvement <- quad_r2 - linear_r2
                        if (improvement > 0.05) {
                            .("⚠️ Potential non-linear relationship detected. Consider polynomial or spline fitting.")
                        } else {
                            .("✅ Linear relationship assumption appears reasonable.")
                        }
                    } else {
                        .("Unable to assess linearity automatically.")
                    }
                } else {
                    .("Linearity assessment not applicable for categorical X-variable.")
                }

                # Check normality of residuals (basic)
                normality_check <- if (is.numeric(x_data) && !is.null(correlation_stats$pearson_r)) {
                    lm_model <- lm(y_data ~ x_data)
                    residuals <- residuals(lm_model)
                    if (length(residuals) >= 3) {
                        shapiro_result <- tryCatch({
                            shapiro.test(residuals)
                        }, error = function(e) NULL)

                        if (!is.null(shapiro_result)) {
                            if (shapiro_result$p.value > 0.05) {
                                .("✅ Residuals appear normally distributed (Shapiro-Wilk p > 0.05).")
                            } else {
                                .("⚠️ Residuals may not be normally distributed (Shapiro-Wilk p ≤ 0.05). Consider robust methods.")
                            }
                        } else {
                            .("Unable to test normality of residuals.")
                        }
                    } else {
                        .("Insufficient data for residual normality testing.")
                    }
                } else {
                    .("Normality check not applicable for this analysis.")
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
                "<h6>", .("Effect Size Interpretation:"), "</h6>",
                "<ul>",
                "<li>", .("<strong>R² ≥ 0.50:</strong> Large effect - clinically significant relationship"), "</li>",
                "<li>", .("<strong>R² = 0.25-0.49:</strong> Medium effect - moderate clinical relevance"), "</li>",
                "<li>", .("<strong>R² = 0.10-0.24:</strong> Small effect - limited clinical significance"), "</li>",
                "<li>", .("<strong>R² < 0.10:</strong> Very small effect - minimal clinical importance"), "</li>",
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
                ggplot2::theme_gray()  # fallback
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
            if (is.null(plot_data)) return()

            private$.checkpoint()  # Checkpoint before plot generation

            # Check dependencies before plotting
            private$.checkDependencies()

            # Build plot using modular approach
            p <- private$.buildBasePlot(plot_data)
            p <- private$.addGroupingAndLines(p, plot_data)
            p <- private$.addTrendlines(p, plot_data)
            p <- private$.addReferenceLines(p, plot_data)
            p <- private$.addLabelsAndTheme(p, plot_data)

            # Print plot
            print(p)
            TRUE
        },

        # Save plot data
        .savePlotData = function(data) {
            # Set plot state for rendering
            self$results$plot$setState(data)
        },

        # === NEW HELPER FUNCTIONS FOR MODULAR DESIGN ===

        # Check dependencies at runtime
        .checkDependencies = function() {
            required_packages <- c("ggplot2", "dplyr")
            missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

            if (length(missing_packages) > 0) {
                error_msg <- paste0(
                    "The following required packages are not installed: ",
                    paste(missing_packages, collapse = ", "),
                    "\n\nPlease install them using:\n",
                    "install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))",
                    "\n\nFor help with installation, see your system administrator or R documentation."
                )

                self$results$todo$setContent(paste0(
                    "<div class='alert alert-danger'>",
                    "<h4>", .("Missing Dependencies"), "</h4>",
                    "<p>", gsub("\n", "<br>", error_msg), "</p>",
                    "</div>"
                ))
                stop(paste(missing_packages, collapse = ", "))
            }
        },

        # Validate numeric parameters
        .validateParameters = function() {
            # Validate plot dimensions
            if (!is.null(self$options$width)) {
                if (self$options$width < 300 || self$options$width > 1200) {
                    jmvcore::reject(.("Plot width must be between 300 and 1200 pixels. Current value will be ignored."), code="")
                }
            }
            if (!is.null(self$options$height)) {
                if (self$options$height < 300 || self$options$height > 1000) {
                    jmvcore::reject(.("Plot height must be between 300 and 1000 pixels. Current value will be ignored."), code="")
                }
            }

            # Validate reference line if provided
            if (!is.null(self$options$refline) && !is.na(self$options$refline)) {
                refline_value <- suppressWarnings(as.numeric(self$options$refline))
                if (is.na(refline_value)) {
                    jmvcore::reject(.("Reference line value must be numeric. Please enter a valid number."), code="")
                }
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

        # Data quality and misuse detection
        .checkDataQuality = function(data) {
            xvar <- self$options$xvar
            yvar <- self$options$yvar
            groupby <- self$options$groupby

            warnings <- c()

            # Check minimum data requirements for different analyses
            if (self$options$trendline && nrow(data) < 5) {
                warnings <- c(warnings, .("Warning: Less than 5 data points available for trend analysis. Results may be unreliable."))
            }

            if (self$options$confidence && nrow(data) < 10) {
                warnings <- c(warnings, .("Warning: Small sample size (< 10 observations) may produce unreliable confidence intervals."))
            }

            # Check for too many groups
            if (!is.null(groupby)) {
                n_groups <- length(unique(data[[groupby]]))
                if (n_groups > 8) {
                    warnings <- c(warnings, paste(.("Warning: Many groups detected ("), n_groups, .("). Consider reducing groups or using faceting for clarity.")))
                }

                # Check group balance
                group_counts <- table(data[[groupby]])
                if (min(group_counts) < 3) {
                    warnings <- c(warnings, .("Warning: Some groups have very few observations (< 3). Consider combining small groups."))
                }
            }

            # Check X-axis spacing for time series
            if (is.numeric(data[[xvar]])) {
                x_diff <- diff(sort(unique(data[[xvar]])))
                if (length(unique(round(x_diff, 6))) > 1) {
                    warnings <- c(warnings, .("Note: Unequal time intervals detected. Trend interpretation should account for irregular spacing."))
                }
            }

            # Display warnings if any
            if (length(warnings) > 0) {
                warning_html <- paste0(
                    "<div class='alert alert-warning'>",
                    "<h6>", .("Data Quality Alerts:"), "</h6>",
                    "<ul>",
                    paste0("<li>", warnings, "</li>", collapse = ""),
                    "</ul>",
                    "</div>"
                )

                # Append to existing content if any
                existing_content <- self$results$todo$content
                if (!is.null(existing_content) && nchar(existing_content) > 0) {
                    self$results$todo$setContent(paste0(existing_content, warning_html))
                } else {
                    self$results$todo$setContent(warning_html)
                }
                self$results$todo$setVisible(TRUE)
            }
        },

        # Generate natural language summary
        .generateNaturalSummary = function(summary_stats, data) {
            xvar <- self$options$xvar
            yvar <- self$options$yvar
            groupby <- self$options$groupby

            # Basic summary
            summary_text <- paste0(
                "<div class='alert alert-info'>",
                "<h5>", .("Analysis Summary"), "</h5>",
                "<p><strong>", .("Dataset:"), "</strong> ", summary_stats$n_observations, " ", .("observations"),
                " ", .("across"), " ", summary_stats$n_x_points, " ", .("time points"), "</p>"
            )

            # Add group information
            if (!is.null(groupby) && summary_stats$n_groups > 1) {
                summary_text <- paste0(summary_text,
                    "<p><strong>", .("Groups:"), "</strong> ", summary_stats$n_groups, " ", .("groups"), " (", summary_stats$group_names, ")</p>"
                )
            }

            # Add Y variable summary
            summary_text <- paste0(summary_text,
                "<p><strong>", yvar, "</strong> ", .("ranges from"), " ",
                format(summary_stats$y_min, digits = 3), " ", .("to"), " ", format(summary_stats$y_max, digits = 3),
                " (mean: ", format(summary_stats$y_mean, digits = 3), ", SD: ", format(summary_stats$y_sd, digits = 3), ")</p>"
            )

            # Add trend information if available
            if (self$options$trendline && is.numeric(data[[xvar]])) {
                correlation_stats <- private$.calculateCorrelation(data)
                if (!is.null(correlation_stats$slope)) {
                    trend_direction <- if (correlation_stats$slope > 0) .("increasing") else .("decreasing")
                    trend_strength <- if (abs(correlation_stats$r_squared) > 0.5) .("strong") else
                                     if (abs(correlation_stats$r_squared) > 0.25) .("moderate") else .("weak")

                    summary_text <- paste0(summary_text,
                        "<p><strong>", .("Trend:"), "</strong> ", trend_strength, " ", trend_direction, " ", .("trend detected"),
                        " (R² = ", round(correlation_stats$r_squared, 3), ")</p>"
                    )
                }
            }

            summary_text <- paste0(summary_text, "</div>")

            # FIXED: Use dedicated naturalSummary output instead of appending to todo
            self$results$naturalSummary$setContent(summary_text)
            self$results$naturalSummary$setVisible(TRUE)
        },

        # Enhanced error handling
        .handleError = function(e) {
            error_suggestions <- list(
                "not found" = .("Check that all selected variables exist in your dataset."),
                "numeric" = .("Ensure the Y-axis variable contains numeric values. Convert text numbers if needed."),
                "missing" = .("Remove rows with missing values or select variables with complete data."),
                "groups" = .("Verify that the grouping variable has meaningful categories."),
                "packages" = .("Install required packages using the provided commands.")
            )

            # Find relevant suggestion
            suggestion <- .("Please verify your data and variable selections.")
            for (pattern in names(error_suggestions)) {
                if (grepl(pattern, e$message, ignore.case = TRUE)) {
                    suggestion <- error_suggestions[[pattern]]
                    break
                }
            }

            error_msg <- paste0(
                "<div class='alert alert-danger'>",
                "<h4>", .("Analysis Error"), "</h4>",
                "<p><strong>", .("Error:"), "</strong> ", e$message, "</p>",
                "<p><strong>", .("Suggestion:"), "</strong> ", suggestion, "</p>",
                "<details>",
                "<summary>", .("Common Solutions"), "</summary>",
                "<ul>",
                "<li>", .("Ensure variables contain appropriate data types (numeric for Y-axis)"), "</li>",
                "<li>", .("Check for missing values and remove incomplete cases"), "</li>",
                "<li>", .("Verify that grouping variables have multiple levels"), "</li>",
                "<li>", .("Ensure at least 3 observations are available for analysis"), "</li>",
                "</ul>",
                "</details>",
                "</div>"
            )
            self$results$todo$setContent(error_msg)
            self$results$todo$setVisible(TRUE)
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
        .addGroupingAndLines = function(p, plot_data) {
            groupby <- self$options$groupby

            if (!is.null(groupby)) {
                # Add grouping aesthetics
                p <- p + ggplot2::aes(color = .data[[groupby]], group = .data[[groupby]])

                # Get colors
                n_groups <- length(unique(plot_data[[groupby]]))
                colors <- private$.getColorPalette(n_groups)

                # Add confidence intervals if requested
                if (self$options$confidence) {
                    private$.checkpoint(flush = FALSE)
                    method <- if (self$options$smooth) "loess" else "lm"
                    p <- p + ggplot2::geom_smooth(method = method, alpha = 0.2)
                }

                # Add main lines
                if (self$options$smooth) {
                    p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE, linewidth = 1.2)
                } else {
                    p <- p + ggplot2::geom_line(linewidth = 1.2)
                }

                # Add points if requested
                if (self$options$points) {
                    p <- p + ggplot2::geom_point(size = 2.5, alpha = 0.8)
                }

                # Set colors
                p <- p + ggplot2::scale_color_manual(values = colors)

            } else {
                # Single group
                color <- private$.getColorPalette(1)

                # Add confidence interval if requested
                if (self$options$confidence) {
                    private$.checkpoint(flush = FALSE)
                    method <- if (self$options$smooth) "loess" else "lm"
                    p <- p + ggplot2::geom_smooth(method = method, alpha = 0.2, color = color, fill = color)
                }

                # Add main line
                if (self$options$smooth) {
                    p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE, color = color, linewidth = 1.2)
                } else {
                    p <- p + ggplot2::geom_line(color = color, linewidth = 1.2)
                }

                # Add points if requested
                if (self$options$points) {
                    p <- p + ggplot2::geom_point(color = color, size = 2.5, alpha = 0.8)
                }
            }

            return(p)
        },

        # Add trend lines
        .addTrendlines = function(p, plot_data) {
            if (!self$options$trendline) return(p)

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
            if (is.null(self$options$refline) || is.na(self$options$refline) || self$options$refline == 0) return(p)

            refline_value <- as.numeric(self$options$refline)
            if (is.na(refline_value) || refline_value == 0) return(p)

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
        .addLabelsAndTheme = function(p, plot_data) {
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
                    paste(ylabel, .("by"), xlabel, .("and"), groupby)
                } else {
                    paste(ylabel, .("by"), xlabel)
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
            p <- p + private$.getPlotTheme()

            return(p)
        }
    )
)
