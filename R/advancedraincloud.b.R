#' @title Advanced Raincloud Plot with Longitudinal Support
#' @return Advanced raincloud plots using ggrain package
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes labs theme_minimal theme element_text
#' @importFrom ggplot2 scale_fill_manual scale_color_manual
#' @importFrom ggrain geom_rain
#' @importFrom dplyr group_by summarise mutate n arrange transmute slice_tail inner_join ungroup
#' @importFrom stats t.test wilcox.test aov kruskal.test
#' @importFrom RColorBrewer brewer.pal
#' @importFrom viridis viridis
#' @importFrom htmltools HTML
advancedraincloudClass <- if (requireNamespace("jmvcore")) {
    R6::R6Class("advancedraincloudClass",
        inherit = advancedraincloudBase,
        private = list(
            # Rows dropped for Inf/-Inf in the dependent variable (see .run).
            .n_nonfinite = 0L,
            .analysis_data = NULL,
            .comparison_results = NULL,
            .change_summary = NULL,
            .analysis_notes = NULL,

            # Constants for maintainability
            .constants = list(
                OUTLIER_PERCENTILES = list(lower = 0.05, upper = 0.95),
                IQR_MULTIPLIER = 1.5,
                CONFIDENCE_LEVEL = 0.95,
                MAX_GROUPS_FOR_PAIRWISE = 10,
                MAX_GROUPS_FOR_DISPLAY = 20,
                PROGRESS_REPORT_INTERVAL = 20
            ),

            # Validation helper functions
            .validate_numeric_range = function(value, name, min, max) {
                if (!is.null(value)) {
                    if (!is.numeric(value) || value < min || value > max) {
                        jmvcore::reject(jmvcore::format(
                            .("{name} must be a numeric value between {min} and {max}."),
                            name = name, min = min, max = max), code = "")
                    }
                }
            },
            .validate_numeric_positive = function(value, name) {
                if (!is.null(value)) {
                    if (!is.numeric(value) || value <= 0) {
                        jmvcore::reject(jmvcore::format(
                            .("{name} must be a positive numeric value."), name = name),
                            code = "")
                    }
                }
            },
            .validate_numeric_type = function(value, name) {
                if (!is.null(value) && !is.numeric(value)) {
                    jmvcore::reject(jmvcore::format(
                        .("{name} must be a numeric value."), name = name), code = "")
                }
            },

            # Validate all option values. Called from .run() (NOT .init()) so that
            # jmvcore::reject() errors actually surface to the user instead of being
            # swallowed by an init-time error handler.
            .validate_options = function() {
                private$.validate_numeric_range(self$options$point_alpha, .("Point transparency"), 0, 1)
                private$.validate_numeric_range(self$options$violin_alpha, .("Violin transparency"), 0, 1)
                private$.validate_numeric_range(self$options$point_size, .("Point size"), 0.1, 5)
                private$.validate_numeric_range(self$options$boxplot_width, .("Boxplot width"), 0.1, 1)

                # Validate jitter seed is numeric
                private$.validate_numeric_type(self$options$jitter_seed, .("Jitter seed"))

                # Validate responder threshold only when change scores are enabled
                if (self$options$show_change_scores) {
                    private$.validate_numeric_range(self$options$responder_threshold, .("Responder threshold"), 0, 100)
                }

                # Validate CV bands only when enabled
                if (self$options$show_cv_bands) {
                    private$.validate_numeric_range(self$options$cv_band_1, .("CV Band 1 percentage"), 1, 50)
                    private$.validate_numeric_range(self$options$cv_band_2, .("CV Band 2 percentage"), 1, 50)
                }

                # Validate numeric types
                private$.validate_numeric_type(self$options$clinical_cutoff, .("Clinical cutoff"))
                private$.validate_numeric_type(self$options$reference_range_min, .("Reference range minimum"))
                private$.validate_numeric_type(self$options$reference_range_max, .("Reference range maximum"))

                # Validate positive values only when relevant options are enabled
                if (self$options$show_mcid) {
                    private$.validate_numeric_positive(self$options$mcid_value, .("MCID value"))
                }

                # Validate reference range logic only when both values are set and non-zero
                if (!is.null(self$options$reference_range_min) && !is.null(self$options$reference_range_max)) {
                    if (is.numeric(self$options$reference_range_min) && is.numeric(self$options$reference_range_max)) {
                        if (self$options$reference_range_min != 0 && self$options$reference_range_max != 0 &&
                            self$options$reference_range_min >= self$options$reference_range_max) {
                            jmvcore::reject(.("Reference range minimum must be less than maximum"), code = "")
                        }
                    }
                }
            },

            # Analysis-note accumulator: operationally important, data-driven conditions
            # detected during .run() are surfaced in an always-visible Html output so they
            # persist in saved results (base warning()/message() do not).
            .addAnalysisNote = function(msg) {
                private$.analysis_notes <- c(private$.analysis_notes, msg)
            },
            .appendPlotNote = function(plot, note) {
                current <- plot$labels$caption
                caption <- if (is.null(current) || !nzchar(current)) {
                    note
                } else {
                    paste(current, note, sep = "\n")
                }
                plot + ggplot2::labs(caption = caption)
            },
            .renderAnalysisNotes = function() {
                notes <- private$.analysis_notes
                if (is.null(notes) || length(notes) == 0) {
                    self$results$analysisNotes$setContent("")
                    return()
                }
                items <- paste0(
                    "<li>", vapply(notes, htmltools::htmlEscape, character(1)), "</li>",
                    collapse = ""
                )
                html <- paste0(
                    "<div style='background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; padding: 15px; border-radius: 5px; margin-bottom: 15px; color: inherit;'>",
                    "<h4 style='color: #856404; margin-top: 0;'>", .("Analysis Notes"), "</h4>",
                    "<ul style='margin-bottom: 0;'>", items, "</ul>",
                    "</div>"
                )
                self$results$analysisNotes$setContent(html)
            },

            # Optimized HTML generation helper
            .build_html_table = function(data_frame, title, headers, bg_color = "#f8f9fa", title_color = "#495057") {
                if (nrow(data_frame) == 0) {
                    return("")
                }

                # Vectorized HTML generation for better performance
                header_html <- paste0("<th style='padding: 8px; border: 1px solid #dee2e6;'>", headers, "</th>", collapse = "")

                # Generate rows using vectorized approach
                row_colors <- ifelse(seq_len(nrow(data_frame)) %% 2 == 0, "#ffffff", "#f8f9fa")

                rows_html <- vapply(seq_len(nrow(data_frame)), function(i) {
                    row_data <- paste0("<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>",
                        data_frame[i, ], "</td>",
                        collapse = ""
                    )
                    paste0("<tr style='background-color: ", row_colors[i], ";'>", row_data, "</tr>")
                }, character(1))

                # Combine all parts efficiently
                paste0(
                    "<div style='background-color: ", bg_color, "; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                    "<h3 style='color: ", title_color, "; margin-top: 0;'>", title, "</h3>",
                    "<table style='width: 100%; border-collapse: collapse; font-family: Arial, sans-serif;'>",
                    "<thead><tr style='background-color: #6c757d; color: #ffffff; color: white;'>", header_html, "</tr></thead>",
                    "<tbody>", paste(rows_html, collapse = ""), "</tbody></table></div>"
                )
            },

            # Enhanced checkpoint with progress reporting
            .checkpoint_with_progress = function(message = "", current = 0, total = 1, flush = TRUE) {
                if (total > 1 && current > 0) {
                    progress_pct <- round((current / total) * 100)
                    if (progress_pct %% private$.constants$PROGRESS_REPORT_INTERVAL == 0) {
                        if (message != "") {
                            cat(paste0(message, " (", progress_pct, "% complete)\n"))
                        }
                    }
                }
                private$.checkpoint(flush = flush)
            },
            .init = function() {
                # Option validation is performed in .run() via private$.validate_options()
                # so that jmvcore::reject() errors surface to the user. Performing it here
                # (inside a swallowing tryCatch) would silently discard those errors.
            },
            .run = function() {
                # Reset accumulated analysis notes at the top of every run so they do not
                # accumulate across runs and so stale notes clear on option-only changes.
                private$.analysis_notes <- character(0)
                self$results$analysisNotes$setContent("")

                # Check if required variables have been selected
                if (is.null(self$options$y_var) || is.null(self$options$x_var) ||
                    self$options$y_var == "" || self$options$x_var == "" ||
                    length(self$options$y_var) == 0 || length(self$options$x_var) == 0) {
                    intro_msg <- "
                <div style='background-color: rgba(33, 152, 239, 0.13); padding: 20px; border-radius: 8px; margin: 20px 0; color: inherit;'>
                <h3 style='color: #1976d2; margin-top: 0;'> Welcome to Advanced Raincloud Plots!</h3>
                <p><strong>Enhanced distribution visualization with longitudinal connections</strong> using ggrain</p>
                <p>Complements the existing Raincloud Plot module with advanced features and customization</p>

                <h4 style='color: #1976d2;'>Required Variables:</h4>
                <ol>
                <li><strong>Y-Axis Variable:</strong> Continuous variable for distribution analysis</li>
                <li><strong>X-Axis Variable:</strong> Grouping variable for categories</li>
                </ol>

                <h4 style='color: #1976d2;'>Advanced Features:</h4>
                <ul>
                <li><strong>Longitudinal Connections:</strong> Connect repeated observations with ID variable</li>
                <li><strong>Likert Scale Support:</strong> Specialized handling for ordinal survey data</li>
                <li><strong>Flexible Positioning:</strong> Left, right, or flanking raincloud placement</li>
                <li><strong>Covariate Mapping:</strong> Remap point colors based on additional variables</li>
                <li><strong>Enhanced Customization:</strong> Advanced styling and appearance options</li>
                </ul>

                <h4 style='color: #1976d2;'>Perfect For:</h4>
                <ul>
                <li><strong>Longitudinal Studies:</strong> Repeated measures with connected observations</li>
                <li><strong>Survey Analysis:</strong> Likert scales and ordinal response data</li>
                <li><strong>Clinical Trials:</strong> Before/after treatment comparisons with connections</li>
                <li><strong>Complex Distributions:</strong> Multi-group comparisons with covariates</li>
                <li><strong>Publication Graphics:</strong> Highly customizable professional plots</li>
                </ul>

                <h4 style='color: #1976d2;'>Comparison with Standard Raincloud Plot:</h4>
                <ul>
                <li><strong>Standard Raincloud (ggdist):</strong> Basic distribution visualization</li>
                <li><strong>Advanced Raincloud (ggrain):</strong> Longitudinal connections + Likert support</li>
                <li><strong>Use Advanced for:</strong> Repeated measures, survey data, complex connections</li>
                <li><strong>Use Standard for:</strong> Simple distribution comparisons</li>
                </ul>

                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                 <em>Advanced raincloud plots for complex research designs and longitudinal data analysis</em>
                </p>
                </div>"

                    self$results$todo$setContent(intro_msg)
                    return()
                } else {
                    self$results$todo$setContent("")
                }

                # Validate option values (rejects surface to the user here, not in .init())
                private$.validate_options()

                # Validate dataset
                if (nrow(self$data) == 0) {
                    jmvcore::reject(.("Error: The provided dataset contains no complete rows. Please check your data and try again."))
                }

                # Safely require ggrain. Write the hard dependency error to the always-visible
                # analysisNotes item (NOT the visibility-gated interpretation item) so it can
                # never be hidden by unchecking the 'Usage guide' option.
                if (!requireNamespace("ggrain", quietly = TRUE)) {
                    error_msg <- "
                <div style='color: red; background-color: rgba(255, 33, 67, 0.09); padding: 20px; border-radius: 8px;'>
                <h4>ggrain Package Required</h4>
                <p>The ggrain package is required for advanced raincloud plot functionality.</p>
                <p>Please install it using: <code>install.packages('ggrain')</code></p>
                </div>"
                    self$results$analysisNotes$setContent(error_msg)
                    return()
                }

                private$.comparison_results <- NULL
                private$.change_summary <- NULL

                # Get data and variables
                dataset <- self$data
                y_var <- self$options$y_var
                x_var <- self$options$x_var
                fill_var <- self$options$fill_var
                id_var <- self$options$id_var
                cov_var <- self$options$cov_var

                if (self$options$show_longitudinal && (is.null(id_var) || id_var == "")) {
                    private$.addAnalysisNote(.("Longitudinal connections were requested but no ID variable is set; connections were not drawn. Select a Longitudinal ID variable to enable them."))
                }

                # Prepare analysis data
                analysis_vars <- c(y_var, x_var)
                if (!is.null(fill_var) && fill_var != "") {
                    analysis_vars <- c(analysis_vars, fill_var)
                }
                id_needed <- !is.null(id_var) && id_var != "" &&
                    (isTRUE(self$options$show_longitudinal) || isTRUE(self$options$show_change_scores))
                if (id_needed) {
                    analysis_vars <- c(analysis_vars, id_var)
                }
                if (!is.null(cov_var) && cov_var != "") {
                    analysis_vars <- c(analysis_vars, cov_var)
                }
                analysis_vars <- unique(analysis_vars[!(is.na(analysis_vars) | analysis_vars == "")])

                analysis_data <- as.data.frame(dataset[analysis_vars], stringsAsFactors = FALSE)
                # TODO (jamovify): df[complete.cases(df), ] is functionally close to na.omit(df)
                # but doesn't go through jmvcore::naOmit, which is the project's helper for
                # explicitly preserving jamovi column attributes (measureType, values, labels).
                # If you want consistent attribute handling across the codebase, replace with:
                #   analysis_data <- jmvcore::naOmit(analysis_data)
                # Note: as.data.frame(stringsAsFactors = FALSE) above already strips some attrs,
                # so attribute preservation may not matter much here revisit if downstream
                # code starts depending on labelled-factor handling.
                analysis_data <- analysis_data[complete.cases(analysis_data), ]

                if (nrow(analysis_data) == 0) {
                    jmvcore::reject(.("Error: No complete cases found for the selected variables."))
                }

                # Convert variables to appropriate types.
                #
                # as.numeric() on a FACTOR yields the level INDICES (1, 2, 3 ...),
                # not the numbers the labels spell, so a factor of "10","20","30"
                # would silently become 1,2,3 and every statistic below - means,
                # SDs, effect sizes, change scores - would be computed on rank
                # codes. The GUI's `permitted` list keeps factors out of the
                # picker, but an R-API caller reaches here.
                y_raw <- analysis_data[[y_var]]
                analysis_data[[y_var]] <- if (is.factor(y_raw))
                    suppressWarnings(as.numeric(as.character(y_raw)))
                else suppressWarnings(as.numeric(y_raw))

                # complete.cases() above follows is.na(), which is TRUE for NaN
                # but FALSE for Inf, so an infinite value survives into every
                # mean, SD and axis range. Drop non-finite values and record how
                # many, so the count can be disclosed rather than vanish.
                n_before_finite <- nrow(analysis_data)
                analysis_data <- analysis_data[is.finite(analysis_data[[y_var]]), , drop = FALSE]
                private$.n_nonfinite <- n_before_finite - nrow(analysis_data)
                if (nrow(analysis_data) == 0)
                    jmvcore::reject(.("No usable numeric values remain in the dependent variable after removing missing and non-finite entries."))
                if (private$.n_nonfinite > 0)
                    private$.addAnalysisNote(sprintf(
                        .("%d row(s) were excluded because '%s' held a non-finite value (Inf or -Inf), which usually means a division by zero or an out-of-range entry."),
                        private$.n_nonfinite, y_var))

                analysis_data[[x_var]] <- droplevels(as.factor(analysis_data[[x_var]]))

                if (!is.null(fill_var) && fill_var != "" && fill_var %in% names(analysis_data)) {
                    analysis_data[[fill_var]] <- as.factor(analysis_data[[fill_var]])
                }
                if (!is.null(id_var) && id_var != "" && id_var %in% names(analysis_data)) {
                    analysis_data[[id_var]] <- as.factor(analysis_data[[id_var]])
                }
                if (!is.null(cov_var) && cov_var != "" && cov_var %in% names(analysis_data)) {
                    if (is.numeric(analysis_data[[cov_var]])) {
                        # Keep numeric for continuous covariate mapping
                    } else {
                        analysis_data[[cov_var]] <- as.factor(analysis_data[[cov_var]])
                    }
                }

                # Apply log transformation if requested
                if (self$options$log_transform) {
                    # A 0.01 offset is added to tolerate exact zeros, but values <= -0.01 make
                    # the log argument non-positive and would silently produce NaN (dropping
                    # points and biasing every downstream statistic). Reject with an actionable
                    # message instead.
                    if (any(analysis_data[[y_var]] <= -0.01, na.rm = TRUE)) {
                        jmvcore::reject(.("Log transformation cannot be applied: the Y-axis variable contains non-positive values. Remove the log transform or use a variable with strictly positive values."))
                    }
                    # Add small constant to avoid log(0) issues
                    analysis_data[[y_var]] <- log(analysis_data[[y_var]] + 0.01)
                }

                # Handle outliers if requested
                if (self$options$outlier_method != "none") {
                    private$.checkpoint() # Before outlier processing
                    analysis_data <- private$.handle_outliers(analysis_data, y_var, self$options$outlier_method)
                }

                # Generate summary statistics if requested
                if (self$options$show_statistics) {
                    private$.checkpoint() # Before statistics generation
                    stats_html <- private$.generate_statistics(analysis_data, y_var, x_var, fill_var)

                    # Add missing data information if requested
                    if (self$options$show_missing_info) {
                        missing_info_html <- private$.generate_missing_data_info(dataset, analysis_data, analysis_vars)
                        stats_html <- paste0(stats_html, missing_info_html)
                    }

                    self$results$statistics$setContent(stats_html)
                }

                # Generate group comparisons if requested
                if (self$options$show_comparisons) {
                    private$.checkpoint() # Before comparison tests
                    comparison_res <- private$.generate_comparisons(analysis_data, y_var, x_var, fill_var)
                    private$.comparison_results <- comparison_res$stats
                    self$results$comparisons$setContent(comparison_res$html)
                } else {
                    private$.comparison_results <- NULL
                }

                # Generate interpretation guide
                if (self$options$show_interpretation) {
                    interpretation_html <- private$.generate_interpretation_guide(analysis_data, y_var, x_var, id_var)
                    self$results$interpretation$setContent(interpretation_html)
                }

                # Generate effect size analysis if requested
                if (self$options$show_effect_size) {
                    private$.checkpoint() # Before effect size calculations
                    start_time <- Sys.time()
                    effect_size_html <- private$.generate_effect_sizes(analysis_data, y_var, x_var, self$options$effect_size_type)
                    elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 2)

                    # Add timing info for complex calculations
                    if (elapsed > 1) {
                        effect_size_html <- paste0(
                            effect_size_html,
                            "<p style='font-size:10px;color:#666;margin-top:10px;'> Computed in ", elapsed, "s</p>"
                        )
                    }

                    self$results$effect_sizes$setContent(effect_size_html)
                }

                # Generate change analysis if requested
                if (self$options$show_change_scores) {
                    private$.checkpoint() # Before change score analysis
                    if (is.null(id_var) || id_var == "") {
                        # No warning() here: jamovi never surfaces one, and the same
                        # message is written to change_analysis just below, where the
                        # user actually sees it.
                        warning_html <- paste0(
                            "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 20px; border-radius: 8px; color: inherit;'>",
                            "<p>", .("Please provide an ID variable to compute change scores."), "</p>",
                            "</div>"
                        )
                        self$results$change_analysis$setContent(warning_html)
                    } else {
                        change_data <- as.data.frame(dataset[c(y_var, x_var, id_var)], stringsAsFactors = FALSE)
                        change_data[[y_var]] <- as.numeric(change_data[[y_var]])
                        change_data[[x_var]] <- as.factor(change_data[[x_var]])
                        change_data[[id_var]] <- as.factor(change_data[[id_var]])
                        change_res <- private$.generate_change_analysis(
                            change_data, y_var, x_var, id_var,
                            self$options$baseline_group, self$options$responder_threshold
                        )
                        private$.change_summary <- change_res$summary
                        self$results$change_analysis$setContent(change_res$html)
                    }
                } else {
                    private$.change_summary <- NULL
                }

                # Generate clinical report if requested
                if (self$options$generate_report) {
                    private$.checkpoint() # Before clinical report generation
                    report_html <- private$.generate_clinical_report(
                        analysis_data, y_var, x_var,
                        self$options$population_type
                    )
                    self$results$clinical_report$setContent(report_html)
                }

                # Generate methods text if requested
                if (self$options$include_methods) {
                    methods_html <- private$.generate_methods_text(self$options)
                    self$results$methods_text$setContent(methods_html)
                }

                # Collect operationally important, data-driven notes for the always-visible
                # analysisNotes output. These mirror conditions detected during plotting that
                # base warning()/message() would not persist in saved results.
                x_levels <- levels(analysis_data[[x_var]])

                if (self$options$show_longitudinal && !is.null(id_var) && id_var != "" &&
                    id_var %in% names(analysis_data)) {
                    id_counts <- table(analysis_data[[id_var]])
                    repeated_ids <- sum(id_counts > 1)
                    if (repeated_ids == 0) {
                        private$.addAnalysisNote(.("Longitudinal connections were requested but no subject has repeated observations, so no connections were drawn."))
                    } else if (repeated_ids < 3) {
                        private$.addAnalysisNote(sprintf(
                            .("Only %d subject(s) have repeated observations, so the longitudinal connections shown may not be meaningful."),
                            repeated_ids))
                    }
                }

                if (!is.null(self$options$trial_arms) && self$options$trial_arms != "") {
                    arm_labels <- trimws(strsplit(self$options$trial_arms, ",")[[1]])
                    if (length(arm_labels) != length(x_levels)) {
                        private$.addAnalysisNote(jmvcore::format(
                            .("Treatment arm labels were ignored: {n_labels} label(s) were provided for {n_groups} group(s). Provide one comma-separated label per group."),
                            n_labels = length(arm_labels),
                            n_groups = length(x_levels)))
                    }
                }

                if (!is.null(self$options$time_labels) && self$options$time_labels != "" &&
                    self$options$show_longitudinal && !is.null(id_var) && id_var != "") {
                    time_labels <- trimws(strsplit(self$options$time_labels, ",")[[1]])
                    if (length(time_labels) != length(x_levels)) {
                        private$.addAnalysisNote(jmvcore::format(
                            .("Time point labels were ignored: {n_labels} label(s) were provided for {n_points} time point(s). Provide one comma-separated label per time point."),
                            n_labels = length(time_labels),
                            n_points = length(x_levels)))
                    }
                }

                # Plot-time fallbacks that make the figure differ from what was requested.
                # .plot() cannot write to a results element, so the cheap, data-only
                # conditions are re-detected here and disclosed as notes; the matching
                # warning() calls in .plot() remain as console-only diagnostics.
                n_groups <- length(x_levels)
                if (n_groups > private$.constants$MAX_GROUPS_FOR_DISPLAY) {
                    private$.addAnalysisNote(sprintf(
                        .("'%s' has %d groups, which may crowd the plot and make it hard to read. Consider collapsing categories."),
                        x_var, n_groups))
                }

                n_fill_levels <- if (!is.null(fill_var) && fill_var != "" &&
                                     fill_var %in% names(analysis_data))
                    length(unique(analysis_data[[fill_var]])) else 0L
                if (n_groups > 15 || n_fill_levels > 5) {
                    private$.addAnalysisNote(sprintf(
                        .("This analysis has %d group(s) and %d fill level(s). The raincloud (ggrain) layer is skipped above 15 groups or 5 fill levels to keep the plot readable, in which case standard violin, boxplot and point layers are drawn instead."),
                        n_groups, n_fill_levels))
                }

                if (!is.null(cov_var) && cov_var != "" && cov_var %in% names(dataset)) {
                    # Only rows that would otherwise have been kept: a row missing y or x
                    # is dropped regardless, and blaming the covariate for it overstates
                    # the covariate's cost.
                    other_vars <- Filter(function(v) !is.null(v) && v != "" && v %in% names(dataset),
                                         list(y_var, x_var))
                    kept_otherwise <- if (length(other_vars) > 0)
                        stats::complete.cases(dataset[, unlist(other_vars), drop = FALSE])
                    else rep(TRUE, nrow(dataset))
                    n_cov_missing <- sum(is.na(dataset[[cov_var]]) & kept_otherwise)
                    if (n_cov_missing > 0) {
                        private$.addAnalysisNote(sprintf(
                            .("%d row(s) were excluded because the covariate '%s' was missing; those observations do not appear in the plot or in the statistics."),
                            n_cov_missing, cov_var))
                    }
                }

                # Render accumulated notes (clears the output when there are none).
                private$.renderAnalysisNotes()

                # Store data for plotting and expose it as plot state (canonical jamovi
                # pattern). analysis_data is already a base data.frame; guard setState so a
                # serialization failure can never break the run - the private field remains
                # the fallback source in .plot().
                private$.analysis_data <- analysis_data
                tryCatch(
                    self$results$plot$setState(analysis_data),
                    error = function(e) NULL
                )
            },
            .plot = function(image, ggtheme, theme, ...) {
                # Prefer the serialized plot state; fall back to the private field (populated
                # at the end of .run()) so state-only re-renders and file re-opens both work.
                if (is.null(image$state) && is.null(private$.analysis_data)) {
                    return()
                }

                analysis_data <- if (!is.null(image$state)) image$state else private$.analysis_data
                y_var <- self$options$y_var
                x_var <- self$options$x_var
                fill_var <- self$options$fill_var
                id_var <- self$options$id_var
                cov_var <- self$options$cov_var

                # Validate that we have groups
                n_groups <- length(unique(analysis_data[[x_var]]))
                if (n_groups == 0) {
                    warning(.("No groups found in the data"))
                    return()
                }

                # Validate data structure
                if (nrow(analysis_data) == 0) {
                    warning(.("No data available for plotting"))
                    return()
                }

                # Determine fill mapping
                if (!is.null(fill_var) && fill_var != "") {
                    fill_mapping <- fill_var
                } else {
                    fill_mapping <- x_var
                }

                # Create base plot with error handling
                private$.checkpoint(flush = FALSE) # Before plot creation
                tryCatch(
                    {
                        p <- ggplot2::ggplot(analysis_data, ggplot2::aes(
                            x = .data[[x_var]],
                            y = .data[[y_var]],
                            fill = .data[[fill_mapping]]
                        ))
                    },
                    error = function(e) {
                        stop(jmvcore::format(
                            .("Failed to create the base plot: {error}. Please check your variable selections."),
                            error = conditionMessage(e)))
                    }
                )

                # Add covariate mapping if specified
                if (!is.null(cov_var) && cov_var != "") {
                    p$mapping$colour <- rlang::sym(cov_var)
                }

                # Validate data structure for ggrain compatibility
                n_groups <- length(unique(analysis_data[[x_var]]))
                if (n_groups > private$.constants$MAX_GROUPS_FOR_DISPLAY) {
                    # Console-only; the user-facing note is added in .run().
                    warning(jmvcore::format(
                        .("A large number of groups ({n}) may cause display issues. Consider grouping your data."),
                        n = n_groups))
                }

                # Ensure no NA values in grouping variable
                analysis_data <- analysis_data[!is.na(analysis_data[[x_var]]) & !is.na(analysis_data[[y_var]]), ]

                # Create geom_rain with enhanced error handling and validation
                rain_params <- list(
                    rain.side = self$options$rain_side,
                    seed = self$options$jitter_seed, # For consistent jittering
                    point.args = list(
                        size = self$options$point_size,
                        alpha = self$options$point_alpha
                    ),
                    violin.args = list(
                        alpha = self$options$violin_alpha
                    ),
                    boxplot.args = list(
                        width = self$options$boxplot_width
                    )
                )

                # Add longitudinal connections if requested and valid
                if (self$options$show_longitudinal && !is.null(id_var) && id_var != "" && id_var %in% names(analysis_data)) {
                    # Check for NA values and handle appropriately
                    original_n <- nrow(analysis_data)

                    if (any(is.na(analysis_data[[id_var]]))) {
                        # Use complete.cases to remove rows with NA in ID variable
                        complete_rows <- complete.cases(analysis_data[[id_var]])
                        analysis_data <- analysis_data[complete_rows, ]
                        excluded_n <- original_n - nrow(analysis_data)

                        # Inform user about exclusions
                        if (excluded_n > 0) {
                            message(jmvcore::format(
                                .("Longitudinal analysis excluded {n} observation(s) with missing ID values ({percent}%)."),
                                n = excluded_n,
                                percent = round((excluded_n / original_n) * 100, 1)))
                        }
                    }

                    # Validate ID structure for meaningful longitudinal connections
                    if (nrow(analysis_data) > 0) {
                        id_counts <- table(analysis_data[[id_var]])
                        repeated_ids <- sum(id_counts > 1)
                        total_ids <- length(id_counts)

                        if (repeated_ids == 0) {
                            # Console-only; the user-facing note is added in .run().
                            warning(.("No repeated IDs found after data cleaning. Longitudinal connections require subjects with multiple observations."))
                        } else if (repeated_ids < 3) {
                            # Console-only; the user-facing note is added in .run().
                            warning(jmvcore::format(
                                .("Very few subjects with repeated measures ({n}) remain after data cleaning. Longitudinal connections may not be meaningful."),
                                n = repeated_ids))
                            rain_params$id.long.var <- id_var
                        } else {
                            # Proper longitudinal structure detected
                            rain_params$id.long.var <- id_var
                            message(jmvcore::format(
                                .("Longitudinal connections were enabled for {repeated}/{total} subjects with repeated measures."),
                                repeated = repeated_ids, total = total_ids))
                        }
                    } else {
                        warning(.("All observations excluded due to missing ID values. Longitudinal connections disabled."))
                    }

                    # Update stored analysis data after cleaning
                    private$.analysis_data <- analysis_data
                }

                # Add Likert mode if requested
                if (self$options$likert_mode) {
                    rain_params$likert <- TRUE
                }

                # Add covariate mapping if specified and valid
                if (!is.null(cov_var) && cov_var != "" && cov_var %in% names(analysis_data)) {
                    # Ensure covariate variable is properly formatted
                    if (!any(is.na(analysis_data[[cov_var]]))) {
                        rain_params$cov <- cov_var
                    } else {
                        # Console-only; .run() notes the rows dropped for a missing covariate.
                        warning(.("Covariate variable contains NA values. Covariate mapping disabled."))
                    }
                }

                # Check for problematic ggrain combinations and use fallback
                use_fallback <- FALSE

                # Known problematic combinations that cause ggrain NA issues
                # Relaxed constraints based on testing - ggrain is more robust than initially thought
                if (n_groups > 15 ||
                    (!is.null(fill_var) && fill_var != "" && length(unique(analysis_data[[fill_var]])) > 5)) {
                    use_fallback <- TRUE
                    # Console-only; the user-facing note is added in .run().
                    warning(.("Using standard geom fallback due to high data complexity."))
                }

                # Try ggrain first, with immediate fallback on any error
                if (!use_fallback) {
                    private$.checkpoint(flush = FALSE) # Before expensive ggrain operation
                    # Capture the error instead of setting the flag via `<<-`
                    # inside the handler; set flag/warning after the tryCatch.
                    rain_err <- tryCatch(
                        {
                            # Validate rain.side parameter with all official options
                            valid_sides <- c("l", "r", "f", "f1x1", "f2x2")
                            if (!self$options$rain_side %in% valid_sides) {
                                rain_params$rain.side <- "l" # Default fallback
                                # Console-only; the user-facing note is added in .run().
                                warning(.("Invalid rain.side value. Using default 'l' (left)."))
                            }

                            p <- p + do.call(ggrain::geom_rain, rain_params)
                            NULL
                        },
                        error = function(e) e
                    )
                    if (!is.null(rain_err)) {
                        # Set fallback flag for any ggrain error
                        use_fallback <- TRUE
                        warning(jmvcore::format(
                            .("ggrain failed with error: {error}. Using the standard-geometry fallback."),
                            error = conditionMessage(rain_err)))
                    }
                }

                # Use fallback geoms if ggrain failed or was skipped
                if (use_fallback) {
                    tryCatch(
                        {
                            # Create sophisticated fallback that mimics raincloud appearance
                            # Set consistent jittering with seed for reproducible results
                            set.seed(self$options$jitter_seed)
                            dodge_width <- 0.8
                            jitter_width <- 0.15

                            # Build plot layers based on raincloud position
                            if (self$options$rain_side == "l") {
                                # Left-side raincloud: violin on left, points on right
                                p <- p +
                                    ggplot2::geom_violin(
                                        alpha = self$options$violin_alpha,
                                        position = ggplot2::position_nudge(x = -0.2),
                                        trim = FALSE
                                    ) +
                                    ggplot2::geom_boxplot(
                                        width = self$options$boxplot_width,
                                        alpha = 0.7,
                                        position = ggplot2::position_nudge(x = -0.1),
                                        outlier.shape = NA
                                    ) +
                                    ggplot2::geom_point(
                                        size = self$options$point_size,
                                        alpha = self$options$point_alpha,
                                        position = ggplot2::position_jitter(width = jitter_width, height = 0)
                                    )
                            } else if (self$options$rain_side == "r") {
                                # Right-side raincloud: points on left, violin on right
                                p <- p +
                                    ggplot2::geom_point(
                                        size = self$options$point_size,
                                        alpha = self$options$point_alpha,
                                        position = ggplot2::position_jitter(width = jitter_width, height = 0)
                                    ) +
                                    ggplot2::geom_boxplot(
                                        width = self$options$boxplot_width,
                                        alpha = 0.7,
                                        position = ggplot2::position_nudge(x = 0.1),
                                        outlier.shape = NA
                                    ) +
                                    ggplot2::geom_violin(
                                        alpha = self$options$violin_alpha,
                                        position = ggplot2::position_nudge(x = 0.2),
                                        trim = FALSE
                                    )
                            } else if (self$options$rain_side %in% c("f", "f1x1", "f2x2")) {
                                # Flanking: violin split or centered - handle different flanking types
                                p <- p +
                                    ggplot2::geom_violin(
                                        alpha = self$options$violin_alpha,
                                        position = ggplot2::position_dodge(dodge_width),
                                        trim = FALSE
                                    ) +
                                    ggplot2::geom_boxplot(
                                        width = self$options$boxplot_width,
                                        alpha = 0.7,
                                        position = ggplot2::position_dodge(dodge_width),
                                        outlier.shape = NA
                                    ) +
                                    ggplot2::geom_point(
                                        size = self$options$point_size,
                                        alpha = self$options$point_alpha,
                                        position = ggplot2::position_jitterdodge(
                                            dodge.width = dodge_width,
                                            jitter.width = jitter_width
                                        )
                                    )
                            }

                            # Add a note that fallback was used
                            p <- p + ggplot2::labs(
                                caption = paste0(
                                    ifelse(is.null(p$labels$caption) || p$labels$caption == "", "",
                                        paste0(p$labels$caption, "\n")
                                    ),
                                    .("Note: Standard-geometry fallback used because the requested raincloud layer was incompatible with the data.")
                                )
                            )
                        },
                        error = function(e2) {
                            # Enhanced error context for better debugging
                            data_info <- paste0(
                                "Data: ", nrow(analysis_data), " rows, ",
                                ncol(analysis_data), " cols, ",
                                length(unique(analysis_data[[x_var]])), " groups"
                            )
                            stop(
                                "Both ggrain and fallback plotting failed. ",
                                "Fallback error: ", htmltools::htmlEscape(e2$message), ". ",
                                data_info, ". ",
                                "Please check your data structure and variable types."
                            )
                        }
                    )
                }

                # Apply color palette with error handling
                p <- tryCatch(
                    {
                        colors <- private$.get_color_palette(fill_mapping, analysis_data)
                        p + ggplot2::scale_fill_manual(values = colors)
                    },
                    error = function(e) {
                        # Fallback to default ggplot2 colors if palette generation fails
                        warning(jmvcore::format(
                            .("Color palette generation failed, so default colors were used: {error}"),
                            error = conditionMessage(e)))
                        private$.appendPlotNote(
                            p,
                            .("Note: The requested fill palette could not be generated, so default colors were used."))
                    }
                )

                # Apply covariate color scale if needed with error handling
                if (!is.null(cov_var) && cov_var != "") {
                    p <- tryCatch(
                        {
                            if (is.numeric(analysis_data[[cov_var]])) {
                                p + ggplot2::scale_color_viridis_c()
                            } else {
                                cov_colors <- private$.get_color_palette(cov_var, analysis_data)
                                p + ggplot2::scale_color_manual(values = cov_colors)
                            }
                        },
                        error = function(e) {
                            warning(jmvcore::format(
                                .("Covariate color-scale generation failed, so defaults were used: {error}"),
                                error = conditionMessage(e)))
                            private$.appendPlotNote(
                                p,
                                .("Note: The requested covariate color scale could not be generated, so default colors were used."))
                        }
                    )
                }

                # Add clinical cutoff line only when a non-zero cutoff is actually set
                # (0 is the "unset" default; drawing a threshold at y=0 by default is wrong).
                if (!is.null(self$options$clinical_cutoff) && is.numeric(self$options$clinical_cutoff) &&
                    !is.na(self$options$clinical_cutoff) && self$options$clinical_cutoff != 0) {
                    p <- p + ggplot2::geom_hline(
                        yintercept = self$options$clinical_cutoff,
                        linetype = "dashed",
                        color = "red",
                        size = 1
                    ) +
                        ggplot2::annotate(
                            "text",
                            x = Inf,
                            y = self$options$clinical_cutoff,
                            label = "Clinical Threshold",
                            hjust = 1.1,
                            vjust = -0.5,
                            color = "red",
                            size = 3
                        )
                }

                # Add reference range shading if specified
                if (!is.null(self$options$reference_range_min) && !is.null(self$options$reference_range_max) &&
                    is.numeric(self$options$reference_range_min) && is.numeric(self$options$reference_range_max) &&
                    !is.na(self$options$reference_range_min) && !is.na(self$options$reference_range_max) &&
                    self$options$reference_range_min < self$options$reference_range_max) {
                    p <- p + ggplot2::annotate(
                        "rect",
                        xmin = -Inf, xmax = Inf,
                        ymin = self$options$reference_range_min,
                        ymax = self$options$reference_range_max,
                        alpha = 0.2,
                        fill = "green"
                    ) +
                        ggplot2::annotate(
                            "text",
                            x = -Inf,
                            y = self$options$reference_range_max,
                            label = "Normal Range",
                            hjust = -0.1,
                            vjust = -0.5,
                            color = "darkgreen",
                            size = 3
                        )
                }

                # Add MCID band if requested
                if (self$options$show_mcid && !is.null(self$options$mcid_value) && is.numeric(self$options$mcid_value) && self$options$mcid_value != 0) {
                    # Calculate group means for MCID display
                    group_means <- aggregate(analysis_data[[y_var]],
                        by = list(analysis_data[[x_var]]),
                        FUN = mean, na.rm = TRUE
                    )

                    for (i in seq_len(nrow(group_means))) {
                        p <- p + ggplot2::annotate(
                            "rect",
                            xmin = i - 0.4, xmax = i + 0.4,
                            ymin = group_means$x[i] - self$options$mcid_value / 2,
                            ymax = group_means$x[i] + self$options$mcid_value / 2,
                            alpha = 0.15,
                            fill = "blue"
                        )
                    }
                }

                # Add sample size annotations if requested
                if (self$options$show_sample_size) {
                    # Calculate sample sizes per group
                    sample_sizes <- table(analysis_data[[x_var]])

                    # Get y-axis range for positioning
                    y_range <- range(analysis_data[[y_var]], na.rm = TRUE)
                    y_pos <- y_range[1] - diff(y_range) * 0.05

                    for (i in seq_along(sample_sizes)) {
                        p <- p + ggplot2::annotate(
                            "text",
                            x = i,
                            y = y_pos,
                            label = paste0("n=", sample_sizes[i]),
                            size = 3,
                            color = "black"
                        )
                    }
                }

                # Add CV bands if requested for biomarker analysis
                if (self$options$show_cv_bands) {
                    # Get configurable CV percentages
                    cv_1_pct <- self$options$cv_band_1 / 100 # Convert percentage to decimal
                    cv_2_pct <- self$options$cv_band_2 / 100 # Convert percentage to decimal

                    # Calculate CV bands using configurable percentages
                    group_stats <- aggregate(analysis_data[[y_var]],
                        by = list(analysis_data[[x_var]]),
                        FUN = function(x) c(mean = mean(x), sd = sd(x))
                    )

                    for (i in seq_len(nrow(group_stats))) {
                        mean_val <- group_stats$x[i, "mean"]
                        cv_1 <- mean_val * cv_1_pct
                        cv_2 <- mean_val * cv_2_pct

                        # First CV band (typically analytical variability)
                        p <- p + ggplot2::annotate(
                            "rect",
                            xmin = i - 0.3, xmax = i + 0.3,
                            ymin = mean_val - cv_1,
                            ymax = mean_val + cv_1,
                            alpha = 0.1,
                            fill = "orange"
                        )

                        # Second CV band (typically biological variability)
                        p <- p + ggplot2::annotate(
                            "rect",
                            xmin = i - 0.3, xmax = i + 0.3,
                            ymin = mean_val - cv_2,
                            ymax = mean_val + cv_2,
                            alpha = 0.05,
                            fill = "red"
                        )

                        # Add CV band labels
                        p <- p + ggplot2::annotate(
                            "text",
                            x = i + 0.35,
                            y = mean_val + cv_1,
                            label = paste0("CV ", self$options$cv_band_1, "%"),
                            size = 2.5,
                            color = "orange",
                            hjust = 0
                        )

                        p <- p + ggplot2::annotate(
                            "text",
                            x = i + 0.35,
                            y = mean_val + cv_2,
                            label = paste0("CV ", self$options$cv_band_2, "%"),
                            size = 2.5,
                            color = "red",
                            hjust = 0
                        )
                    }
                }

                # Apply custom trial arm labels if provided
                if (!is.null(self$options$trial_arms) && self$options$trial_arms != "") {
                    arm_labels <- trimws(strsplit(self$options$trial_arms, ",")[[1]])
                    x_levels <- levels(analysis_data[[x_var]])

                    # Check if number of labels matches number of groups
                    if (length(arm_labels) == length(x_levels)) {
                        # Create a named vector for scale_x_discrete
                        names(arm_labels) <- x_levels
                        p <- p + ggplot2::scale_x_discrete(labels = arm_labels)
                    } else {
                        warning(jmvcore::format(
                            .("The number of trial-arm labels ({n_labels}) does not match the number of groups ({n_groups}), so default labels were used."),
                            n_labels = length(arm_labels),
                            n_groups = length(x_levels)))
                    }
                }

                # Apply custom time point labels if provided (for longitudinal data)
                if (!is.null(self$options$time_labels) && self$options$time_labels != "") {
                    time_labels <- trimws(strsplit(self$options$time_labels, ",")[[1]])
                    x_levels <- levels(analysis_data[[x_var]])

                    # For longitudinal data, time labels override regular group labels
                    if (self$options$show_longitudinal && !is.null(id_var) && id_var != "") {
                        if (length(time_labels) == length(x_levels)) {
                            # Create a named vector for scale_x_discrete
                            names(time_labels) <- x_levels
                            p <- p + ggplot2::scale_x_discrete(labels = time_labels)
                        } else {
                            warning(jmvcore::format(
                                .("The number of time-point labels ({n_labels}) does not match the number of time points ({n_points}), so default labels were used."),
                                n_labels = length(time_labels),
                                n_points = length(x_levels)))
                        }
                    }
                }

                # Apply journal-specific theme
                p <- private$.apply_journal_theme(p, self$options$journal_style)

                # Add p-values if requested and comparisons were made
                if (self$options$p_value_position != "none" && self$options$show_comparisons) {
                    p <- private$.add_p_values(p, analysis_data, y_var, x_var, self$options$p_value_position)
                }

                # Add labels
                x_label <- if (self$options$x_label != "") self$options$x_label else x_var
                y_label <- if (self$options$y_label != "") self$options$y_label else y_var

                # Add log scale label if transformed
                if (self$options$log_transform) {
                    # log(y + 0.01), not log(y): a 0.01 offset is added in .run()
                    # so exact zeros survive. Naming the offset matters when the
                    # measurement itself is small - for a proportion or a
                    # biomarker near 0.05 the shift is not negligible.
                    y_label <- paste0("log(", y_label, " + 0.01)")
                }

                plot_title <- self$options$plot_title

                # Add population type to title if specified
                if (self$options$population_type != "itt") {
                    pop_label <- switch(self$options$population_type,
                        "pp" = "Per-Protocol",
                        "mitt" = "Modified ITT",
                        "at" = "As-Treated"
                    )
                    plot_title <- paste0(plot_title, " (", pop_label, " Population)")
                }

                p <- p + ggplot2::labs(
                    title = plot_title,
                    x = x_label,
                    y = y_label
                )


                print(p)
                TRUE
            },
            .get_color_palette = function(var_name, data) {
                palette_name <- self$options$color_palette
                n_colors <- length(levels(as.factor(data[[var_name]])))

                # Handle case when no groups are present
                if (n_colors == 0) {
                    return("#2E86AB") # Return single default color
                }

                if (palette_name == "clinical") {
                    base_colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#593E2C", "#8E6C8A")
                    if (n_colors <= length(base_colors)) {
                        return(base_colors[1:n_colors])
                    } else {
                        # Use colorRampPalette to generate more colors
                        return(grDevices::colorRampPalette(base_colors)(n_colors))
                    }
                } else if (palette_name == "viridis") {
                    # viridisLite::viridis() has no `discrete` argument; passing it errors and
                    # silently falls back to default colors, so the Viridis option never works.
                    return(viridis::viridis(n_colors))
                } else if (palette_name == "pastel") {
                    base_colors <- c("#FFB3BA", "#BAFFC9", "#BAE1FF", "#FFFFBA", "#FFDFBA", "#E0BBE4")
                    if (n_colors <= length(base_colors)) {
                        return(base_colors[1:n_colors])
                    } else {
                        # Use colorRampPalette to generate more colors
                        return(grDevices::colorRampPalette(base_colors)(n_colors))
                    }
                } else if (palette_name %in% c("set1", "set2", "dark2")) {
                    palette_r_name <- switch(palette_name,
                        "set1" = "Set1",
                        "set2" = "Set2",
                        "dark2" = "Dark2"
                    )
                    if (n_colors <= 8) {
                        return(RColorBrewer::brewer.pal(max(3, n_colors), palette_r_name)[1:n_colors])
                    } else {
                        base_colors <- RColorBrewer::brewer.pal(8, palette_r_name)
                        return(grDevices::colorRampPalette(base_colors)(n_colors))
                    }
                } else {
                    # Default clinical colors with fallback for large numbers
                    base_colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#593E2C", "#8E6C8A")
                    if (n_colors <= length(base_colors)) {
                        return(base_colors[1:n_colors])
                    } else {
                        # Use colorRampPalette to generate more colors
                        return(grDevices::colorRampPalette(base_colors)(n_colors))
                    }
                }
            },
            .generate_statistics = function(data, y_var, x_var, fill_var) {
                group_var <- if (!is.null(fill_var) && fill_var != "") fill_var else x_var

                stats_summary <- dplyr::summarise(
                    dplyr::group_by(data, .data[[group_var]]),
                    n = dplyr::n(),
                    mean = round(mean(.data[[y_var]], na.rm = TRUE), 3),
                    median = round(median(.data[[y_var]], na.rm = TRUE), 3),
                    sd = round(sd(.data[[y_var]], na.rm = TRUE), 3),
                    min_val = round(min(.data[[y_var]], na.rm = TRUE), 3),
                    max_val = round(max(.data[[y_var]], na.rm = TRUE), 3),
                    .groups = "drop"
                )

                # The log transform is applied in .run() BEFORE any statistic is
                # computed, so every number below is on the log scale. The plot's
                # y-axis says so but this table did not, and the Methods panel
                # that mentions it is OFF by default - leaving a clinician reading
                # "Mean 2.31" for a biomarker that is actually about 10 ng/mL.
                # Put the scale in the column headings, where it cannot be missed.
                # Punctuation and spacing OUTSIDE the translation call: a key
                # with a leading space (" [log scale]") makes jmvcore's translator
                # throw 'key must be not be "" or NA', which crashed the whole
                # analysis for anyone with the log transform on.
                unit_suffix <- if (isTRUE(self$options$log_transform))
                    paste0(" [", .("log scale"), "]") else ""
                headers <- c(.("Group"), .("N"),
                             paste0(.("Mean"), unit_suffix),
                             paste0(.("Median"), unit_suffix),
                             paste0(.("SD"), unit_suffix),
                             paste0(.("Range"), unit_suffix))

                # Prepare data for table generation
                table_data <- data.frame(
                    Group = paste0(
                        "<strong>",
                        htmltools::htmlEscape(stats_summary[[group_var]]),
                        "</strong>"
                    ),
                    N = stats_summary$n,
                    Mean = stats_summary$mean,
                    Median = stats_summary$median,
                    SD = stats_summary$sd,
                    Range = paste0(stats_summary$min_val, " - ", stats_summary$max_val),
                    stringsAsFactors = FALSE
                )

                stats_html <- private$.build_html_table(
                    table_data,
                    .("Advanced Raincloud Statistics"),
                    headers
                )

                # Add summary note
                stats_html <- paste0(
                    stats_html,
                    "<p style='font-size: 12px; color: #6c757d; margin-top: 15px;'>",
                    "<em>", .("Summary statistics for advanced raincloud plot groups."), "</em>",
                    "</p>"
                )

                return(stats_html)
            },
            .generate_comparisons = function(data, y_var, x_var, fill_var) {
                group_var <- if (!is.null(fill_var) && fill_var != "") fill_var else x_var
                groups <- levels(as.factor(data[[group_var]]))
                n_groups <- length(groups)

                if (n_groups < 2) {
                    return(list(
                        html = "<div style='padding: 20px;'><p>Group comparisons require at least 2 groups.</p></div>",
                        stats = NULL
                    ))
                }

                # Perform appropriate statistical test
                df_value <- NA
                if (n_groups == 2) {
                    group1_data <- data[data[[group_var]] == groups[1], y_var]
                    group2_data <- data[data[[group_var]] == groups[2], y_var]

                    # Use Wilcoxon test for robustness
                    test_result <- wilcox.test(group1_data, group2_data)
                    test_name <- "Wilcoxon rank-sum test"
                    test_stat <- round(test_result$statistic, 4)
                    raw_p <- test_result$p.value
                    p_value <- round(raw_p, 4)
                    test_details <- paste0("W = ", test_stat)
                } else {
                    # Multiple groups - use Kruskal-Wallis
                    formula_str <- jmvcore::constructFormula(terms = group_var, dep = y_var)
                    kw_result <- kruskal.test(jmvcore::asFormula(formula_str), data = data)

                    test_name <- "Kruskal-Wallis test (omnibus only)"
                    test_stat <- round(kw_result$statistic, 4)
                    raw_p <- kw_result$p.value
                    p_value <- round(raw_p, 4)
                    test_details <- paste0("\u{03C7}\u{00B2} = ", test_stat, ", df = ", kw_result$parameter)
                    df_value <- kw_result$parameter
                }

                # Format results
                significance <- if (p_value < 0.001) "Highly significant (***)" else if (p_value < 0.01) "Very significant (**)" else if (p_value < 0.05) "Significant (*)" else "Not significant"

                # Add warning for Kruskal-Wallis about missing post-hoc tests
                post_hoc_warning <- ""
                if (n_groups > 2) {
                    post_hoc_warning <- paste0(
                        "<div style='background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; padding: 10px; margin-top: 10px; color: inherit;'>",
                        "<p style='margin: 0;'><strong> IMPORTANT:</strong> This is an <strong>omnibus test only</strong>. ",
                        "A significant result indicates <em>at least one</em> group differs, but does NOT identify which specific groups differ. ",
                        "For pairwise comparisons, use post-hoc tests with multiplicity adjustment (e.g., Dunn's test with Holm or Bonferroni correction) ",
                        "in dedicated statistical software.</p>",
                        "</div>"
                    )
                }

                comparison_html <- paste0(
                    "<div style='background-color: rgba(153, 33, 170, 0.12); padding: 20px; border-radius: 8px; margin-bottom: 20px; color: inherit;'>",
                    "<h3 style='color: #7b1fa2; margin-top: 0;'> Group Comparison Results</h3>",
                    "<table style='width: 100%; border-collapse: collapse;'>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Test Method:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", test_name, "</td></tr>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Test Statistic:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", test_details, "</td></tr>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>P-value:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", p_value, "</td></tr>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Result:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", significance, "</td></tr>",
                    "</table>",
                    post_hoc_warning,
                    "<p style='font-size: 12px; color: #7b1fa2; margin-top: 15px;'>",
                    "<em>* p < 0.05, ** p < 0.01, *** p < 0.001. Non-parametric tests used for robustness.</em>",
                    "</p></div>"
                )

                stats <- list(
                    method = test_name,
                    statistic = test_stat,
                    df = df_value,
                    p_value = raw_p,
                    label = private$.format_p_value(raw_p)
                )

                return(list(html = comparison_html, stats = stats))
            },
            .generate_interpretation_guide = function(data, y_var, x_var, id_var) {
                n_total <- nrow(data)
                n_groups <- length(levels(as.factor(data[[x_var]])))
                rain_side <- self$options$rain_side
                has_longitudinal <- self$options$show_longitudinal && !is.null(id_var) && id_var != ""
                has_likert <- self$options$likert_mode

                interpretation_html <- paste0(
                    "<div style='background-color: rgba(33, 159, 33, 0.1); padding: 20px; border-radius: 8px; color: inherit;'>",
                    "<h3 style='color: #2e7d32; margin-top: 0;'> Advanced Raincloud Plot Guide</h3>",
                    "<h4 style='color: #2e7d32;'>Current Configuration:</h4>",
                    "<ul>",
                    "<li><strong>Variable:</strong> ", htmltools::htmlEscape(y_var), " distributed across ", n_groups, " groups</li>",
                    "<li><strong>Observations:</strong> ", n_total, " data points</li>",
                    "<li><strong>Raincloud Position:</strong> ", switch(rain_side,
                        "l" = "Left side",
                        "r" = "Right side",
                        "f" = "Flanking (both sides)",
                        "f1x1" = "Flanking 1x1 (paired)",
                        "f2x2" = "Flanking 2x2 (grouped)",
                        rain_side
                    ), "</li>",
                    if (has_longitudinal) paste0("<li><strong>Longitudinal Connections:</strong> Connected by ", htmltools::htmlEscape(id_var), "</li>") else "",
                    if (has_likert) "<li><strong>Likert Mode:</strong> Y-axis jittering enabled for ordinal data</li>" else "",
                    "</ul>",
                    "<h4 style='color: #2e7d32;'>Advanced Features in Use:</h4>",
                    "<ul>",
                    if (has_longitudinal) {
                        "<li><strong>Longitudinal Connections:</strong> Lines connect repeated observations from the same subjects across groups - perfect for before/after or repeated measures designs.</li>"
                    } else {
                        "<li><strong>Longitudinal Connections:</strong> Not enabled. Add an ID variable to connect repeated observations.</li>"
                    },
                    if (has_likert) {
                        "<li><strong>Likert Scale Mode:</strong> Y-axis jittering applied to better visualize discrete ordinal responses.</li>"
                    } else {
                        "<li><strong>Likert Scale Mode:</strong> Not enabled. Enable for survey or ordinal data visualization.</li>"
                    },
                    "</ul>",
                    "<h4 style='color: #2e7d32;'>ggrain vs. Standard Raincloud:</h4>",
                    "<ul>",
                    "<li><strong>Standard Raincloud (ggdist):</strong> Basic three-layer visualization (violin + box + points)</li>",
                    "<li><strong>Advanced Raincloud (ggrain):</strong> Enhanced with connections, positioning, and specialized data support</li>",
                    "<li><strong>Key Advantages:</strong> Longitudinal connections, Likert support, flexible positioning, covariate mapping</li>",
                    "</ul>",
                    "<h4 style='color: #2e7d32;'>Clinical Research Applications:</h4>",
                    "<ul>",
                    "<li><strong>Repeated Measures:</strong> Connect pre/post treatment measurements for same patients</li>",
                    "<li><strong>Survey Analysis:</strong> Handle Likert scale responses with appropriate jittering</li>",
                    "<li><strong>Longitudinal Studies:</strong> Track individual participants across multiple time points</li>",
                    "<li><strong>Complex Comparisons:</strong> Multi-group analysis with covariate mapping</li>",
                    "</ul>",
                    "<h4 style='color: #2e7d32;'>Raincloud Position Options:</h4>",
                    "<ul>",
                    "<li><strong>Left ('l'):</strong> Traditional raincloud with density on left, data on right</li>",
                    "<li><strong>Right ('r'):</strong> Mirrored version with density on right, data on left</li>",
                    "<li><strong>Flanking ('f'):</strong> Density distributions on both sides for comparison</li>",
                    "</ul>",
                    "<p style='font-size: 12px; color: #2e7d32; margin-top: 15px;'>",
                    "<em> Advanced raincloud plots provide enhanced visualization capabilities for complex research designs</em>",
                    "</p></div>"
                )

                return(interpretation_html)
            },

            # New helper functions for clinical research features

            .handle_outliers = function(data, y_var, method) {
                y_values <- data[[y_var]]

                if (method == "winsorize") {
                    # Winsorize using constants
                    lower <- quantile(y_values, private$.constants$OUTLIER_PERCENTILES$lower, na.rm = TRUE)
                    upper <- quantile(y_values, private$.constants$OUTLIER_PERCENTILES$upper, na.rm = TRUE)
                    data[[y_var]] <- pmax(pmin(y_values, upper), lower)
                } else if (method == "trim") {
                    # Trim using constants
                    lower <- quantile(y_values, private$.constants$OUTLIER_PERCENTILES$lower, na.rm = TRUE)
                    upper <- quantile(y_values, private$.constants$OUTLIER_PERCENTILES$upper, na.rm = TRUE)
                    data <- data[y_values >= lower & y_values <= upper, ]
                } else if (method == "iqr") {
                    # IQR method using constants
                    Q1 <- quantile(y_values, 0.25, na.rm = TRUE)
                    Q3 <- quantile(y_values, 0.75, na.rm = TRUE)
                    IQR <- Q3 - Q1
                    lower <- Q1 - private$.constants$IQR_MULTIPLIER * IQR
                    upper <- Q3 + private$.constants$IQR_MULTIPLIER * IQR
                    data <- data[y_values >= lower & y_values <= upper, ]
                }

                return(data)
            },
            .generate_effect_sizes = function(data, y_var, x_var, effect_type) {
                groups <- unique(data[[x_var]])
                n_groups <- length(groups)

                if (n_groups < 2) {
                    return("<p>Effect size calculation requires at least 2 groups.</p>")
                }

                # Implement performance optimization: limit pairwise comparisons
                if (n_groups > private$.constants$MAX_GROUPS_FOR_PAIRWISE) {
                    # Called from .run(), so the truncation can be disclosed to the user.
                    private$.addAnalysisNote(sprintf(
                        .("Effect sizes were computed for the first %d of %d groups only; pairwise comparisons are limited to keep the analysis responsive."),
                        private$.constants$MAX_GROUPS_FOR_PAIRWISE, n_groups))
                    groups <- groups[1:private$.constants$MAX_GROUPS_FOR_PAIRWISE]
                    n_groups <- length(groups)
                }

                html <- paste0(
                    "<div style='background-color: rgba(33, 159, 43, 0.1); padding: 20px; border-radius: 8px; margin-bottom: 20px; color: inherit;'>",
                    "<h3 style='color: #2e7d32; margin-top: 0;'> Effect Size Analysis</h3>",
                    "<p>Effect size type: <strong>", effect_type, "</strong></p>",
                    "<table style='width: 100%; border-collapse: collapse;'>",
                    "<thead><tr style='background-color: #4caf50; color: #ffffff; color: white;'>",
                    "<th style='padding: 8px; border: 1px solid #ddd;'>Comparison</th>",
                    "<th style='padding: 8px; border: 1px solid #ddd;'>Effect Size</th>",
                    "<th style='padding: 8px; border: 1px solid #ddd;'>95% CI</th>",
                    "<th style='padding: 8px; border: 1px solid #ddd;'>Interpretation</th>",
                    "</tr></thead><tbody>"
                )

                # Calculate pairwise effect sizes with progress tracking
                total_comparisons <- (n_groups * (n_groups - 1)) / 2
                comparison_count <- 0

                for (i in 1:(n_groups - 1)) {
                    for (j in (i + 1):n_groups) {
                        comparison_count <- comparison_count + 1
                        private$.checkpoint_with_progress(
                            .("Computing effect sizes"),
                            comparison_count,
                            total_comparisons,
                            flush = FALSE
                        )

                        group1_data <- data[data[[x_var]] == groups[i], y_var]
                        group2_data <- data[data[[x_var]] == groups[j], y_var]

                        # Calculate effect size based on type
                        effect_result <- private$.calculate_effect_size(
                            group1_data, group2_data, effect_type
                        )

                        # Handle errors in effect size calculation
                        if (is.na(effect_result$effect_size)) {
                            error_msg <- if (!is.null(effect_result$error)) effect_result$error else "Unable to calculate"
                            html <- paste0(
                                html,
                                "<tr style='background-color: rgba(255, 202, 33, 0.23); color: inherit;'>",
                                "<td style='padding: 8px; border: 1px solid #ddd;'>",
                                htmltools::htmlEscape(groups[i]), " vs ", htmltools::htmlEscape(groups[j]), "</td>",
                                "<td colspan='3' style='padding: 8px; border: 1px solid #ddd; text-align: center;'>",
                                " ", error_msg, "</td>",
                                "</tr>"
                            )
                        } else {
                            # Interpret effect size
                            interpretation <- private$.interpret_effect_size(abs(effect_result$effect_size))

                            # Format CI values safely
                            ci_text <- if (is.na(effect_result$ci_lower) || is.na(effect_result$ci_upper)) {
                                "N/A"
                            } else {
                                paste0("[", round(effect_result$ci_lower, 3), ", ", round(effect_result$ci_upper, 3), "]")
                            }

                            html <- paste0(
                                html,
                                "<tr>",
                                "<td style='padding: 8px; border: 1px solid #ddd;'>",
                                htmltools::htmlEscape(groups[i]), " vs ", htmltools::htmlEscape(groups[j]), "</td>",
                                "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>",
                                round(effect_result$effect_size, 3), "</td>",
                                "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>",
                                ci_text, "</td>",
                                "<td style='padding: 8px; border: 1px solid #ddd;'>",
                                interpretation, "</td>",
                                "</tr>"
                            )
                        }
                    }
                }

                html <- paste0(
                    html, "</tbody></table>",
                    "<p style='font-size: 12px; color: #2e7d32; margin-top: 10px;'>",
                    "<em>", .("Confidence intervals use a large-sample normal (z) approximation and may be too narrow for small samples; interpret with caution when group sizes are small."), "</em>",
                    "</p></div>"
                )
                return(html)
            },
            .calculate_effect_size = function(group1, group2, type) {
                n1 <- length(group1)
                n2 <- length(group2)
                mean1 <- mean(group1, na.rm = TRUE)
                mean2 <- mean(group2, na.rm = TRUE)
                sd1 <- sd(group1, na.rm = TRUE)
                sd2 <- sd(group2, na.rm = TRUE)

                # Initialize effect_size and se
                effect_size <- NA
                se <- NA

                if (type == "cohens_d") {
                    # Pooled standard deviation with zero guard
                    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))

                    if (pooled_sd == 0 || is.na(pooled_sd) || is.infinite(pooled_sd)) {
                        # Return NA if pooled SD is invalid
                        return(list(
                            effect_size = NA,
                            ci_lower = NA,
                            ci_upper = NA,
                            error = "Zero or invalid pooled standard deviation - groups may have no variation"
                        ))
                    }

                    effect_size <- (mean1 - mean2) / pooled_sd
                    # Standard error for Cohen's d
                    se <- sqrt((n1 + n2) / (n1 * n2) + effect_size^2 / (2 * (n1 + n2)))
                } else if (type == "hedges_g") {
                    # Hedges' g includes small sample correction
                    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))

                    if (pooled_sd == 0 || is.na(pooled_sd) || is.infinite(pooled_sd)) {
                        return(list(
                            effect_size = NA,
                            ci_lower = NA,
                            ci_upper = NA,
                            error = "Zero or invalid pooled standard deviation - groups may have no variation"
                        ))
                    }

                    d <- (mean1 - mean2) / pooled_sd
                    # Small sample correction factor
                    J <- 1 - (3 / (4 * (n1 + n2 - 2) - 1))
                    effect_size <- d * J
                    # Standard error for Hedges' g (same as Cohen's d)
                    se <- sqrt((n1 + n2) / (n1 * n2) + effect_size^2 / (2 * (n1 + n2)))
                } else if (type == "glass_delta") {
                    # Glass's delta uses ONLY control group (group2) SD

                    if (sd2 == 0 || is.na(sd2) || is.infinite(sd2)) {
                        return(list(
                            effect_size = NA,
                            ci_lower = NA,
                            ci_upper = NA,
                            error = "Zero or invalid control group standard deviation - control group may have no variation"
                        ))
                    }

                    effect_size <- (mean1 - mean2) / sd2
                    # Glass's delta standardises by the CONTROL SD only, but both
                    # group means still carry sampling error, so the first term is
                    # (n1+n2)/(n1*n2) = 1/n1 + 1/n2 - not 1/n2 alone. The previous
                    # `n1/(n1*n2)` simplifies to 1/n2 and dropped the treatment
                    # group's contribution entirely, and the second denominator
                    # used 2*n2 rather than 2*(n2-1). Measured at n1=n2=20,
                    # delta=0.8: SE 0.2569 against the correct 0.3418, i.e. a
                    # confidence interval only 75% as wide as it should be, so the
                    # estimate looked far more precise than it is.
                    # Hedges & Olkin (1985), Statistical Methods for Meta-Analysis.
                    se <- if (n2 > 1)
                        sqrt((n1 + n2) / (n1 * n2) + effect_size^2 / (2 * (n2 - 1)))
                    else NA_real_
                }

                # Calculate confidence intervals using constants
                z_value <- qnorm(1 - (1 - private$.constants$CONFIDENCE_LEVEL) / 2) # 1.96 for 95% CI

                if (is.na(se) || is.infinite(se)) {
                    return(list(
                        effect_size = effect_size,
                        ci_lower = NA,
                        ci_upper = NA,
                        error = "Invalid standard error calculation"
                    ))
                }

                ci_lower <- effect_size - z_value * se
                ci_upper <- effect_size + z_value * se

                return(list(
                    effect_size = effect_size,
                    ci_lower = ci_lower,
                    ci_upper = ci_upper
                ))
            },
            .interpret_effect_size = function(es) {
                # Cohen's bands describe MAGNITUDE, so take the absolute value
                # here rather than relying on every caller to do it. Comparing a
                # SIGNED effect size against 0.2 / 0.5 / 0.8 classifies d = -1.5
                # and d = -0.9 as "Negligible" while +0.9 is "Large" - the sign is
                # only which group was named first. Today the single caller
                # already passes abs(), so this was latent rather than live; the
                # guard is here so a second caller cannot reintroduce it.
                if (is.null(es) || length(es) == 0 || is.na(es) || !is.finite(es))
                    return("Not estimable")
                a <- abs(es)
                if (a < 0.2) {
                    return("Negligible")
                } else if (a < 0.5) {
                    return("Small")
                } else if (a < 0.8) {
                    return("Medium")
                } else {
                    return("Large")
                }
            },
            .generate_change_analysis = function(data, y_var, x_var, id_var, baseline_group, threshold) {
                if (is.null(id_var) || id_var == "" || is.null(baseline_group) || baseline_group == "") {
                    return(list(
                        html = paste0(
                            "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 20px; border-radius: 8px; margin-bottom: 20px; color: inherit;'>",
                            "<h3 style='color: #856404; margin-top: 0;'> Change Score Analysis</h3>",
                            "<p>", .("Change analysis requires both a longitudinal ID variable and a baseline group specification."), "</p>",
                            "</div>"
                        ),
                        summary = NULL
                    ))
                }

                id_sym <- rlang::sym(id_var)

                original_n <- nrow(data)
                required_vars <- c(id_var, x_var, y_var)
                complete_data <- data[complete.cases(data[required_vars]), ]
                excluded_n <- original_n - nrow(complete_data)

                cleaning_report <- ""
                if (excluded_n > 0) {
                    cleaning_report <- paste0(
                        "<div style='background-color: rgba(33, 152, 239, 0.13); padding: 15px; border-radius: 5px; margin-bottom: 15px; color: inherit;'>",
                        "<h4 style='color: #1976d2; margin-top: 0;'> ", .("Data Cleaning Summary"), "</h4>",
                        # The translation keys used to be ") observations", which
                        # rendered as "900) observations" - an unopened bracket in
                        # every change-score report. Punctuation stays outside the
                        # key; only the word is translated.
                        "<p><strong>", .("Original dataset:"), "</strong> ", original_n, " ", .("observations"), "</p>",
                        "<p><strong>", .("Complete cases:"), "</strong> ", nrow(complete_data), " ", .("observations"), "</p>",
                        "<p><strong>", .("Excluded (missing data):"), "</strong> ", excluded_n, " ", .("observations"), " (",
                        round((excluded_n / original_n) * 100, 1), "%)</p>",
                        "<p style='font-size: 12px; color: #1976d2;'><em>",
                        .("Complete case analysis performed - observations with missing ID, group, or outcome values were excluded."),
                        "</em></p></div>"
                    )
                }

                if (!baseline_group %in% unique(complete_data[[x_var]])) {
                    available_groups <- paste(htmltools::htmlEscape(unique(complete_data[[x_var]])), collapse = ", ")
                    return(list(
                        html = paste0(
                            cleaning_report,
                            "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 20px; border-radius: 8px; margin-bottom: 20px; color: inherit;'>",
                            "<h3 style='color: #856404; margin-top: 0;'> Change Score Analysis</h3>",
                            "<p>", jmvcore::format(
                                .("Baseline group '{group}' was not found in the complete data. Available groups: {available}."),
                                group = htmltools::htmlEscape(baseline_group),
                                available = available_groups), "</p>",
                            "</div>"
                        ),
                        summary = NULL
                    ))
                }

                id_counts <- table(complete_data[[id_var]])
                if (all(id_counts == 1)) {
                    return(list(
                        html = paste0(
                            cleaning_report,
                            "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 20px; border-radius: 8px; margin-bottom: 20px; color: inherit;'>",
                            "<h3 style='color: #856404; margin-top: 0;'> Change Score Analysis</h3>",
                            "<p>", .("No repeated observations found in complete data. Change analysis requires subjects with multiple measurements."), "</p>",
                            "</div>"
                        ),
                        summary = NULL
                    ))
                }

                level_index <- setNames(seq_along(levels(complete_data[[x_var]])), levels(complete_data[[x_var]]))
                baseline_data <- complete_data[complete_data[[x_var]] == baseline_group, ]
                followup_data <- complete_data[complete_data[[x_var]] != baseline_group, ]

                if (nrow(baseline_data) == 0 || nrow(followup_data) == 0) {
                    return(list(
                        html = paste0(
                            cleaning_report,
                            "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 20px; border-radius: 8px; margin-bottom: 20px; color: inherit;'>",
                            "<h3 style='color: #856404; margin-top: 0;'> Change Score Analysis</h3>",
                            "<p>", .("Insufficient baseline or follow-up observations to compute change scores."), "</p>",
                            "</div>"
                        ),
                        summary = NULL
                    ))
                }

                baseline_by_id <- baseline_data %>%
                    dplyr::group_by(.data[[id_var]]) %>%
                    dplyr::summarise(baseline_value = dplyr::first(.data[[y_var]]), .groups = "drop")

                followup_by_id <- followup_data %>%
                    dplyr::mutate(.time_index = level_index[as.character(.data[[x_var]])]) %>%
                    dplyr::group_by(.data[[id_var]]) %>%
                    dplyr::arrange(.data$.time_index) %>%
                    dplyr::slice_tail(n = 1) %>%
                    dplyr::ungroup() %>%
                    dplyr::transmute(
                        !!id_sym := .data[[id_var]],
                        followup_group = as.character(.data[[x_var]]),
                        followup_value = .data[[y_var]]
                    )

                paired_data <- dplyr::inner_join(baseline_by_id, followup_by_id, by = id_var)
                if (nrow(paired_data) == 0) {
                    return(list(
                        html = paste0(
                            cleaning_report,
                            "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 20px; border-radius: 8px; margin-bottom: 20px; color: inherit;'>",
                            "<h3 style='color: #856404; margin-top: 0;'> Change Score Analysis</h3>",
                            "<p>", .("No paired observations found for change analysis in complete data."), "</p>",
                            "</div>"
                        ),
                        summary = NULL
                    ))
                }

                paired_data <- paired_data %>%
                    dplyr::mutate(
                        change_score = followup_value - baseline_value,
                        percent_change = ifelse(abs(baseline_value) > .Machine$double.eps,
                            (change_score / abs(baseline_value)) * 100,
                            NA_real_
                        )
                    )

                direction_label <- if (threshold >= 0) "increase" else "decrease"
                threshold_value <- abs(threshold)

                responders <- paired_data %>%
                    dplyr::mutate(
                        responder = dplyr::case_when(
                            is.na(.data$percent_change) ~ FALSE,
                            threshold >= 0 ~ .data$percent_change >= threshold_value,
                            TRUE ~ .data$percent_change <= -threshold_value
                        ),
                        # Named by DIRECTION, not by benefit. These were
                        # "improver"/"decliner", which assumes higher = better -
                        # false for tumour size, pain scores, LDL, turnaround
                        # time and most biomarkers, where an increase is the
                        # patient getting worse. The counts are identical; only
                        # the claim attached to them changes. The Responders row
                        # above IS direction-aware, because its threshold is
                        # signed.
                        increased = .data$change_score > 0,
                        decreased = .data$change_score < 0,
                        stable = .data$change_score == 0
                    )

                n_total <- nrow(responders)
                n_responders <- sum(responders$responder, na.rm = TRUE)
                n_improvers <- sum(responders$increased, na.rm = TRUE)
                n_decliners <- sum(responders$decreased, na.rm = TRUE)
                n_stable <- sum(responders$stable, na.rm = TRUE)
                pct_fmt <- function(count) {
                    if (n_total > 0) round((count / n_total) * 100, 1) else 0
                }

                mean_change <- round(mean(responders$change_score, na.rm = TRUE), 3)
                median_change <- round(median(responders$change_score, na.rm = TRUE), 3)
                sd_change <- round(sd(responders$change_score, na.rm = TRUE), 3)

                threshold_label <- paste0(ifelse(threshold >= 0, ">=", "<="), threshold_value)

                html <- paste0(
                    cleaning_report,
                    "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 20px; border-radius: 8px; margin-bottom: 20px; color: inherit;'>",
                    "<h3 style='color: #856404; margin-top: 0;'> Change Score Analysis</h3>",
                    "<h4>", .("Analysis Parameters"), "</h4>",
                    "<table style='width: 100%; border-collapse: collapse;'>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>", .("Baseline group:"), "</strong></td>",
                    "<td style='padding: 8px; border: 1px solid #ddd;'>", htmltools::htmlEscape(baseline_group), "</td></tr>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>", .("Response threshold:"), "</strong></td>",
                    "<td style='padding: 8px; border: 1px solid #ddd;'>", threshold_label, "%</td></tr>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>", .("Paired subjects:"), "</strong></td>",
                    "<td style='padding: 8px; border: 1px solid #ddd;'>", n_total, "</td></tr>",
                    "</table>",
                    "<h4>", .("Change Score Summary"), "</h4>",
                    "<table style='width: 100%; border-collapse: collapse;'>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>", .("Mean change:"), "</strong></td>",
                    "<td style='padding: 8px; border: 1px solid #ddd;'>", mean_change, " \u{00B1} ", sd_change, "</td></tr>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>", .("Median change:"), "</strong></td>",
                    "<td style='padding: 8px; border: 1px solid #ddd;'>", median_change, "</td></tr>",
                    "</table>",
                    "<h4>", .("Response Categories"), "</h4>",
                    "<table style='width: 100%; border-collapse: collapse;'>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>", .("Responders ("), threshold_label, "% change):</strong></td>",
                    "<td style='padding: 8px; border: 1px solid #ddd;'>", n_responders, " (", pct_fmt(n_responders), "%)</td></tr>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>", .("Increased from baseline:"), "</strong></td>",
                    "<td style='padding: 8px; border: 1px solid #ddd;'>", n_improvers, " (", pct_fmt(n_improvers), "%)</td></tr>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>", .("Decreased from baseline:"), "</strong></td>",
                    "<td style='padding: 8px; border: 1px solid #ddd;'>", n_decliners, " (", pct_fmt(n_decliners), "%)</td></tr>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>", .("Stable:"), "</strong></td>",
                    "<td style='padding: 8px; border: 1px solid #ddd;'>", n_stable, " (", pct_fmt(n_stable), "%)</td></tr>",
                    "</table>",
                    "<p style='font-size: 12px; color: #856404; margin-top: 15px;'>",
                    "<em>", .("Analysis based on complete paired observations. Change scores calculated as (Follow-up - Baseline) values."), "</em>",
                    "</p></div>"
                )

                summary <- list(
                    baseline_group = baseline_group,
                    n_total = n_total,
                    n_responders = n_responders,
                    n_improvers = n_improvers,
                    n_decliners = n_decliners,
                    n_stable = n_stable,
                    mean_change = mean_change,
                    median_change = median_change,
                    sd_change = sd_change,
                    direction = direction_label,
                    threshold = threshold
                )

                return(list(html = html, summary = summary))
            },
            .generate_clinical_report = function(data, y_var, x_var, pop_type) {
                pop_label <- switch(pop_type,
                    "itt" = "Intention-to-Treat",
                    "pp" = "Per-Protocol",
                    "mitt" = "Modified ITT",
                    "at" = "As-Treated"
                )

                # The Analysis Population option selects a LABEL. It performs no
                # filtering and no reclassification - the analysis is whatever
                # rows were supplied, minus every row dropped for missing values
                # (and minus outliers if that option is on).
                #
                # That matters most for the ITT label, because a complete-case
                # analysis is by definition NOT intention-to-treat: ITT keeps all
                # randomised participants, including those with missing outcomes.
                # Stamping "Intention-to-Treat" on a figure whose incomplete rows
                # were silently discarded is a claim the analysis cannot support,
                # so state what was actually analysed and flag the contradiction.
                n_supplied <- tryCatch(nrow(self$data), error = function(e) NA_integer_)
                n_analysed <- nrow(data)
                n_dropped <- if (is.na(n_supplied)) NA_integer_ else n_supplied - n_analysed

                pop_caveat <- paste0(
                    "<p style='font-size:12px;color:#1565c0;margin-top:4px;'><em>",
                    .("This label is your declaration about the dataset you supplied; the analysis does not construct or verify the population. No rows are added, removed or reassigned by this setting."),
                    "</em></p>")

                itt_warning <- if (identical(pop_type, "itt") && !is.na(n_dropped) && n_dropped > 0)
                    paste0(
                        "<div style='background-color: rgba(216, 33, 50, 0.18);color: inherit;padding:12px;border-radius:6px;margin:10px 0;'>",
                        sprintf(.("%d of %d supplied rows (%.1f%%) were excluded before analysis because of missing values in the selected variables. An intention-to-treat analysis retains all randomised participants, so these results are a COMPLETE-CASE analysis and should not be reported as ITT without imputation or another accounting for the missing outcomes."),
                                n_dropped, n_supplied, 100 * n_dropped / n_supplied),
                        "</div>")
                else ""

                html <- paste0(
                    "<div style='background-color: rgba(33, 152, 239, 0.13); padding: 20px; border-radius: 8px; margin-bottom: 20px; color: inherit;'>",
                    "<h3 style='color: #1565c0; margin-top: 0;'> Clinical Analysis Report</h3>",
                    "<h4>Study Population</h4>",
                    "<p>Analysis population as declared: <strong>", pop_label, "</strong>",
                    if (!is.na(n_dropped)) paste0(
                        " - ",
                        sprintf(.("%d of %d supplied rows analysed"), n_analysed, n_supplied)) else "",
                    "</p>",
                    pop_caveat,
                    itt_warning,
                    "<p>Total N: <strong>", nrow(data), "</strong></p>",
                    "<h4>Primary Outcome</h4>",
                    "<p>Variable: <strong>", htmltools::htmlEscape(y_var), "</strong></p>",
                    "<p>Measurement type: Continuous</p>",
                    "<h4>Statistical Summary</h4>",
                    "<p>Number of groups: <strong>", length(unique(data[[x_var]])), "</strong></p>",
                    "<p>Overall mean (SD): <strong>",
                    round(mean(data[[y_var]], na.rm = TRUE), 2), " (",
                    round(sd(data[[y_var]], na.rm = TRUE), 2), ")</strong></p>",
                    "</div>"
                )

                return(html)
            },
            .generate_methods_text = function(options) {
                html <- paste0(
                    "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin-bottom: 20px; color: inherit;'>",
                    "<h3 style='color: #495057; margin-top: 0;'> Methods Section</h3>",
                    "<p style='text-align: justify;'>",
                    "Data were visualized using advanced raincloud plots, which combine ",
                    "violin plots, boxplots, and individual data points to provide a comprehensive ",
                    "view of data distributions. "
                )

                # Add specific methods based on options
                if (options$log_transform) {
                    html <- paste0(html, "Values were log-transformed prior to analysis. ")
                }

                if (options$outlier_method != "none") {
                    html <- paste0(
                        html, "Outliers were handled using the ",
                        options$outlier_method, " method. "
                    )
                }

                if (options$show_effect_size) {
                    html <- paste0(
                        html, "Effect sizes were calculated using ",
                        options$effect_size_type, ". "
                    )
                }

                html <- paste0(
                    html,
                    "Statistical analyses were performed using R version ",
                    R.version$major, ".", R.version$minor,
                    " with the jamovi ClinicoPath module.",
                    "</p></div>"
                )

                return(html)
            },
            .apply_journal_theme = function(p, journal_style) {
                if (journal_style == "nature") {
                    p <- p + ggplot2::theme_minimal() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(size = 10, face = "bold"),
                            axis.title = ggplot2::element_text(size = 9),
                            axis.text = ggplot2::element_text(size = 8),
                            legend.title = ggplot2::element_text(size = 9),
                            legend.text = ggplot2::element_text(size = 8),
                            legend.position = "bottom"
                        )
                } else if (journal_style == "nejm") {
                    p <- p + ggplot2::theme_classic() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(size = 12, face = "bold"),
                            axis.title = ggplot2::element_text(size = 10),
                            axis.text = ggplot2::element_text(size = 9),
                            legend.position = "right"
                        )
                } else if (journal_style == "lancet") {
                    p <- p + ggplot2::theme_bw() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(size = 11, face = "bold"),
                            axis.title = ggplot2::element_text(size = 10),
                            panel.grid.minor = ggplot2::element_blank()
                        )
                } else if (journal_style == "jama") {
                    p <- p + ggplot2::theme_light() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(size = 11),
                            axis.title = ggplot2::element_text(size = 10, face = "bold"),
                            panel.border = ggplot2::element_rect(color = "black", size = 1)
                        )
                } else {
                    # Default theme
                    p <- p + ggplot2::theme_minimal() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
                            axis.title = ggplot2::element_text(size = 12),
                            legend.position = "bottom"
                        )
                }

                return(p)
            },
            .generate_missing_data_info = function(original_data, analysis_data, required_vars) {
                n_original <- nrow(original_data)
                n_analysis <- nrow(analysis_data)
                n_excluded <- n_original - n_analysis

                # Calculate missing data by variable
                missing_info <- list()
                for (var in required_vars) {
                    if (var %in% names(original_data)) {
                        n_missing <- sum(is.na(original_data[[var]]))
                        pct_missing <- round((n_missing / n_original) * 100, 1)
                        missing_info[[var]] <- list(n_missing = n_missing, pct_missing = pct_missing)
                    }
                }

                html <- paste0(
                    "<div style='background-color: rgba(255, 169, 33, 0.14); padding: 20px; border-radius: 8px; margin-top: 20px; color: inherit;'>",
                    "<h3 style='color: #e65100; margin-top: 0;'> ", .("Missing Data Information"), "</h3>",
                    "<h4>", .("Data Exclusions"), "</h4>",
                    "<p><strong>", .("Original dataset:"), "</strong> ", n_original, " ", .("observations"), "</p>",
                    "<p><strong>", .("Analysis dataset:"), "</strong> ", n_analysis, " ", .("observations"), "</p>",
                    "<p><strong>", .("Excluded (incomplete):"), "</strong> ", n_excluded, " ", .("observations"), " (",
                    round((n_excluded / n_original) * 100, 1), "%)</p>"
                )

                if (length(missing_info) > 0) {
                    html <- paste0(
                        html, "<h4>", .("Missing Data by Variable"), "</h4>",
                        "<table style='width: 100%; border-collapse: collapse;'>",
                        "<thead><tr style='background-color: #ff9800; color: #111111; color: white;'>",
                        "<th style='padding: 8px; border: 1px solid #ddd;'>", .("Variable"), "</th>",
                        "<th style='padding: 8px; border: 1px solid #ddd;'>", .("Missing (n)"), "</th>",
                        "<th style='padding: 8px; border: 1px solid #ddd;'>", .("Missing (%)"), "</th>",
                        "</tr></thead><tbody>"
                    )

                    for (var_name in names(missing_info)) {
                        info <- missing_info[[var_name]]
                        html <- paste0(
                            html,
                            "<tr>",
                            "<td style='padding: 8px; border: 1px solid #ddd;'>", htmltools::htmlEscape(var_name), "</td>",
                            "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", info$n_missing, "</td>",
                            "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", info$pct_missing, "%</td>",
                            "</tr>"
                        )
                    }

                    html <- paste0(html, "</tbody></table>")
                }

                html <- paste0(
                    html,
                    "<p style='font-size: 12px; color: #e65100; margin-top: 15px;'>",
                    "<em>", .("Complete case analysis performed. Observations with missing values in any required variable were excluded."), "</em>",
                    "</p></div>"
                )

                return(html)
            },
            .add_p_values = function(p, data, y_var, x_var, position) {
                stats <- private$.comparison_results
                if (is.null(stats) || is.null(position) || position == "none") {
                    return(p)
                }

                label <- paste0("p = ", stats$label)
                if (position == "above") {
                    y_max <- max(data[[y_var]], na.rm = TRUE)
                    y_pos <- y_max + diff(range(data[[y_var]], na.rm = TRUE)) * 0.1
                    p <- p + ggplot2::annotate(
                        "text",
                        x = mean(seq_along(unique(data[[x_var]]))),
                        y = y_pos,
                        label = label,
                        size = 3,
                        fontface = "italic"
                    )
                } else if (position == "legend") {
                    current_caption <- if (!is.null(p$labels$caption)) p$labels$caption else ""
                    caption_text <- trimws(paste(current_caption, label))
                    p <- p + ggplot2::labs(caption = caption_text)
                } else if (position == "table") {
                    # Already displayed via comparison table, nothing to add to plot
                    return(p)
                }

                return(p)
            },
            .format_p_value = function(p_value) {
                if (is.na(p_value)) {
                    return("NA")
                }
                if (p_value < 0.001) {
                    return("< 0.001")
                }
                round(p_value, 3)
            }
        )
    )
}
