#' @title Violin Plots to Compare Between Groups
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#' @import ggplot2
#' @importFrom digest digest
#'
#' @return An \code{R6} class generator object for the \code{jjbetweenstatsClass} backend; used internally by the jamovi analysis wrapper and not called directly.


jjbetweenstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjbetweenstatsClass",
    inherit = jjbetweenstatsBase,
    private = list(
        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .processedOptions = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .accumulated_messages = NULL,  # For accumulating diagnostic messages
        .diagnosticsHtml = NULL,       # Persisted diagnostics HTML (survives .prepareData cache hits)
        .assumptionWarnings = NULL,    # Cached Levene/Shapiro warnings (computed once in .prepareData)

        # Helper to accumulate messages instead of overwriting
        .appendMessage = function(message) {
            if (is.null(private$.accumulated_messages)) {
                private$.accumulated_messages <- character()
            }
            private$.accumulated_messages <- c(private$.accumulated_messages, message)
        },

        # Combine accumulated interim-diagnostic messages into one HTML block and
        # PERSIST it in a private field. Deliberately writes to NO results element:
        # this runs inside .prepareData(), which is called from the plot render
        # functions, and results content mutated during a render is discarded by
        # jamovi (that made the whole panel vanish once plots were visible). The
        # `diagnostics` element is populated only from .run() using this field.
        .storeDiagnostics = function() {
            if (!is.null(private$.accumulated_messages) && length(private$.accumulated_messages) > 0) {
                body <- paste(private$.accumulated_messages, collapse = "")
                private$.diagnosticsHtml <- paste0(
                    "<div style='padding: 12px 15px; background-color: #eef2f7; ",
                    "border-left: 4px solid #6c757d; margin: 10px 0;'>",
                    "<strong>Data Diagnostics &amp; Assumptions</strong>", body, "</div>"
                )
                private$.accumulated_messages <- NULL
            } else {
                private$.diagnosticsHtml <- ""
            }
        },

        # init ----
        .init = function() {
            deplen <- length(self$options$dep)

            # Use configurable plot dimensions
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 650
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450

            # Improved height calculation to prevent compressed plots
            # Add extra spacing when combining multiple plots vertically
            if (deplen > 1) {
                # Add 15% extra height per plot for better spacing
                total_height <- deplen * plotheight * 1.15
            } else {
                total_height <- plotheight
            }

            self$results$plot$setSize(plotwidth, total_height)

            if (!is.null(self$options$grvar)) {
                mydata <- self$data
                grvar <- self$options$grvar

                if (!is.null(mydata[[grvar]])) {
                    num_levels <- nlevels(as.factor(mydata[[grvar]]))
                    # For grouped analysis, calculate width based on layout
                    # grouped_ggbetweenstats arranges plots in a grid
                    # Estimate grid dimensions: use ceiling(sqrt(num_levels)) for balanced layout
                    ncol_estimate <- ceiling(sqrt(num_levels))
                    grouped_width <- ncol_estimate * plotwidth

                    # Height calculation for grouped plots with multiple dependent variables
                    if (deplen > 1) {
                        grouped_height <- deplen * plotheight * 1.15
                    } else {
                        # For single dep var, height based on number of grouping levels
                        nrow_estimate <- ceiling(num_levels / ncol_estimate)
                        grouped_height <- nrow_estimate * plotheight
                    }

                    self$results$plot2$setSize(grouped_width, grouped_height)
                }
            }
        },

        # Shared validation helper
        .validateInputs = function() {
            if (is.null(self$options$dep) || is.null(self$options$group))
                return(FALSE)
            if (nrow(self$data) == 0)
                jmvcore::reject(.('Data contains no (complete) rows'))

            # Get available variables for helpful error messages
            available_vars <- paste(names(self$data), collapse = '", "')

            # Check variable existence with helpful error messages
            for (var in self$options$dep) {
                if (!(var %in% names(self$data)))
                    jmvcore::reject(.('Variable "{var}" not found in data. Available variables: "{available_vars}"'),
                                    var = var, available_vars = available_vars)
            }
            if (!(self$options$group %in% names(self$data)))
                jmvcore::reject(.('Grouping variable "{group}" not found in data. Available variables: "{available_vars}"'),
                                group = self$options$group, available_vars = available_vars)
                
            return(TRUE)
        },
        
        # Data quality validation helper
        .validateDataQuality = function(mydata, vars) {
            for (var in vars) {
                num_vals <- jmvcore::toNumeric(mydata[[var]])
                num_vals <- num_vals[!is.na(num_vals)]

                if (length(num_vals) < 3) {
                    # ACCUMULATE instead of overwrite
                    private$.appendMessage(
                        glue::glue(.("<br> Warning: {var} has less than 3 valid observations<br>"),
                                   var = htmltools::htmlEscape(var))
                    )
                }
                if (length(unique(num_vals)) < 2) {
                    # ACCUMULATE instead of overwrite
                    private$.appendMessage(
                        glue::glue(.("<br> Warning: {var} has no variation (all values are the same)<br>"),
                                   var = htmltools::htmlEscape(var))
                    )
                }
            }
        },
        
        # Outlier detection helper (IQR rule)
        .detectOutliers = function(data, vars, method = "IQR") {
            outliers <- list()

            # For very large datasets, report only the outlier COUNT (not the full
            # index vector) to keep diagnostic messages compact. Thresholds are
            # ALWAYS derived from exact quantiles (cheap even at 1e5+ rows) so the
            # reported count is reproducible run-to-run; the previous sample()-based
            # estimate was unseeded and made counts change between identical runs.
            data_size <- nrow(data)
            large_dataset <- data_size > 5000

            for (var in vars) {
                vals <- jmvcore::toNumeric(data[[var]])
                vals <- vals[!is.na(vals)]

                if (length(vals) > 0) {
                    Q1 <- quantile(vals, 0.25, na.rm = TRUE)
                    Q3 <- quantile(vals, 0.75, na.rm = TRUE)
                    IQR <- Q3 - Q1

                    # Only calculate outlier indices if IQR is meaningful
                    if (IQR > 0) {
                        outlier_indices <- which(
                            data[[var]] < (Q1 - 1.5 * IQR) |
                            data[[var]] > (Q3 + 1.5 * IQR)
                        )

                        if (length(outlier_indices) > 0) {
                            # For large datasets, only report count not indices
                            if (large_dataset) {
                                outliers[[var]] <- length(outlier_indices)
                            } else {
                                outliers[[var]] <- outlier_indices
                            }
                        }
                    }
                }
            }
            return(outliers)
        },
        
        # Theme application helper
        .applyTheme = function(plot, opts, ggtheme) {
            if (!opts$originaltheme) {
                plot <- plot + ggtheme
            } else {
                plot <- plot + ggstatsplot::theme_ggstatsplot()
            }
            
            # Apply colorblind-safe palette if requested
            if (opts$colorblindSafe) {
                # Use viridis color palette which is colorblind-safe
                plot <- plot + ggplot2::scale_fill_viridis_d(option = "D") +
                              ggplot2::scale_color_viridis_d(option = "D")
            }
            
            return(plot)
        },
        
        # Clinical results summary. Rendered from .run() (NOT from a plot render
        # function): jamovi discards results content mutated during a render, which
        # is why the previous render-path version appeared then vanished on
        # option-only re-renders (the same anti-pattern already fixed for
        # diagnostics/mecGuidance). The test is named from the number of OBSERVED
        # group levels so a >2-group comparison is not mislabelled as a two-sample
        # test, and we no longer call nchar()/htmlEscape() on ggstatsplot's
        # plotmath language subtitle (which threw and forced generic fallback text).
        .generateClinicalSummary = function() {
            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            mydata <- private$.prepareData()
            n_total <- nrow(mydata)
            grp <- mydata[[self$options$group]]
            grp_levels <- unique(grp[!is.na(grp)])
            n_groups <- length(grp_levels)

            type <- self$options$typestatistics
            if (n_groups <= 2) {
                test_name <- switch(type,
                    "parametric"    = if (self$options$varequal) .("Student's t-test") else .("Welch's t-test"),
                    "nonparametric" = .("Mann-Whitney U test"),
                    "robust"        = .("robust test (Yuen's trimmed means)"),
                    "bayes"         = .("Bayesian t-test"),
                    .("two-group comparison")
                )
            } else {
                test_name <- switch(type,
                    "parametric"    = if (self$options$varequal) .("one-way ANOVA") else .("Welch's ANOVA"),
                    "nonparametric" = .("Kruskal-Wallis test"),
                    "robust"        = .("robust ANOVA"),
                    "bayes"         = .("Bayesian ANOVA"),
                    .("ANOVA")
                )
            }

            vars <- htmltools::htmlEscape(paste(self$options$dep, collapse = ", "))
            clinical_text <- sprintf(
                .("<div style='padding: 12px 15px; background-color: #eef7ee; border-left: 4px solid #28a745; margin: 10px 0;'><h4 style='margin-top:0;'>Results Summary</h4><p>%s comparing %s across %d group(s) (n = %d). See the plot subtitle for the test statistic, p-value, and effect size.</p></div>"),
                test_name,
                vars,
                n_groups,
                n_total
            )
            self$results$clinicalSummary$setContent(clinical_text)
        },

        # Wrap a plot-building/printing expression so degenerate inputs (a group
        # with <2 observations, an all-NA cell, an internal ggstatsplot/ggpubr
        # failure) surface as an actionable message instead of a raw jamovi engine
        # error. `expr` is a promise, evaluated lazily inside the tryCatch.
        .tryPlot = function(expr) {
            tryCatch(
                expr,
                error = function(e) {
                    jmvcore::reject(
                        .("Unable to generate the plot: {err}. This often happens when a group has fewer than two observations or contains only missing values; verify that each group has sufficient non-missing data."),
                        err = conditionMessage(e)
                    )
                }
            )
        },
        
        # Build the multiple-endpoint correction guidance HTML.
        # Rendered directly in .run() into the always-visible `mecGuidance` results
        # element (NOT via the data-memoized .prepareData()/todo channel), so it
        # reliably appears and updates whenever multiEndpointCorrection changes.
        # Returns "" when guidance does not apply (< 2 dependent variables), which
        # renders as an empty, effectively hidden element.
        .endpointGuidanceHtml = function() {
            dep <- self$options$dep
            if (is.null(dep) || length(dep) <= 1)
                return("")

            num_endpoints <- length(dep)
            correction <- self$options$multiEndpointCorrection

            body <- if (correction == "none") {
                actual_alpha <- 1 - (1 - 0.05)^num_endpoints
                sprintf(
                    .(" <strong>CRITICAL: MULTIPLE ENDPOINT TESTING WITHOUT CORRECTION</strong><br>You are testing %d dependent variables simultaneously without adjustment. This inflates your family-wise error rate from 5%% to approximately %.1f%%.<br><strong>RECOMMENDATION:</strong> Select a correction method to see guidance, or interpret all p-values cautiously acknowledging this inflated error rate."),
                    num_endpoints, actual_alpha * 100
                )
            } else if (correction == "bonferroni") {
                adjusted_alpha <- 0.05 / num_endpoints
                sprintf(
                    .(" <strong>Bonferroni Correction Guidance (Manual Application Required):</strong><br>You are testing %d endpoints. To control family-wise error rate at 5%%:<br>\u2022 <strong>Adjusted significance threshold: \u03b1 = %.4f</strong><br>\u2022 Compare each p-value from the plots below to %.4f (NOT 0.05)<br>\u2022 Only results with p < %.4f should be considered statistically significant<br>\u2022 Example: If cholesterol shows p = 0.03, it is NOT significant (0.03 > %.4f)<br> <strong>IMPORTANT:</strong> This correction is not applied automatically. You must manually compare reported p-values to the adjusted threshold."),
                    num_endpoints, adjusted_alpha, adjusted_alpha, adjusted_alpha, adjusted_alpha
                )
            } else if (correction == "holm") {
                sprintf(
                    .(" <strong>Holm Correction Guidance (Manual Application Required):</strong><br>You are testing %d endpoints. To apply Holm's step-down procedure:<br>1. Rank all p-values from smallest to largest<br>2. For the smallest p-value, use threshold: \u03b1 = 0.05/%d = %.4f<br>3. For the second smallest, use: \u03b1 = 0.05/%d = %.4f<br>4. Continue until a p-value fails to meet its threshold<br>5. All subsequent tests are considered non-significant<br> <strong>IMPORTANT:</strong> This correction requires manual application. Collect p-values from plots below and apply the step-down procedure."),
                    num_endpoints, num_endpoints, 0.05/num_endpoints,
                    num_endpoints - 1, 0.05/(num_endpoints - 1)
                )
            } else if (correction == "fdr") {
                sprintf(
                    .(" <strong>FDR Correction Guidance (Manual Application Required):</strong><br>You are testing %d endpoints. To control false discovery rate at 5%% using Benjamini-Hochberg:<br>1. Rank all p-values from smallest to largest (p\u2081 \u2264 p\u2082 \u2264 ... \u2264 p%d)<br>2. Find the largest i where: p\u1d62 \u2264 (i/%d) \u00d7 0.05<br>3. Reject hypotheses 1 through i<br>4. Collect p-values from plots below and apply this procedure in external software (e.g., R's p.adjust() function)<br> <strong>IMPORTANT:</strong> FDR correction requires manual calculation. This analysis does not automatically adjust p-values."),
                    num_endpoints, num_endpoints, num_endpoints
                )
            } else {
                return("")
            }

            # Colour the box by severity: red when uncorrected, amber otherwise.
            border <- if (correction == "none") "#dc3545" else "#ffc107"
            bg     <- if (correction == "none") "#ffe5e5" else "#fff3cd"
            paste0(
                "<div style='padding: 15px; background-color: ", bg,
                "; border-left: 4px solid ", border, "; margin: 10px 0;'>",
                "<p style='margin: 0;'>", body, "</p></div>"
            )
        },

        # Statistical assumption checker
        .checkAssumptions = function(data, variables, group_var, test_type) {
            warnings <- c()

            # NOTE: Multiple-endpoint correction guidance is rendered separately in
            # .run() via private$.endpointGuidanceHtml() into the always-visible
            # `mecGuidance` element. It used to be emitted here, but this path only
            # reaches the user through the data-memoized .prepareData()/todo channel,
            # so it silently vanished whenever multiEndpointCorrection changed
            # without the data changing ("appears then disappears").

            for (var in variables) {
                var_data <- data[[var]]
                group_data <- data[[group_var]]

                # Check sample sizes
                group_counts <- table(group_data, useNA = "no")
                min_group_size <- min(group_counts)

                if (min_group_size < 3) {
                    warnings <- c(warnings, sprintf(.(" %s: Minimum group size is %d (recommend \u22653)"),
                                                    htmltools::htmlEscape(var), min_group_size))
                }

                if (test_type == "parametric" && min_group_size >= 3) {
                    # HOMOGENEITY OF VARIANCE TEST (Levene's test)
                    tryCatch({
                        # Use car::leveneTest for homogeneity of variance
                        if (requireNamespace("car", quietly = TRUE)) {
                            levene_result <- car::leveneTest(var_data ~ group_data, center = median)
                            levene_p <- levene_result$`Pr(>F)`[1]

                            if (levene_p < 0.05 && !self$options$varequal) {
                                warnings <- c(warnings, sprintf(
                                    .(" %s: Variances differ significantly between groups (Levene's test p = %.3f). Consider enabling 'Equal Variances = FALSE' or using non-parametric test."),
                                    htmltools::htmlEscape(var), levene_p
                                ))
                            }
                        }
                    }, error = function(e) {
                        # Silently continue if car package not available or test fails
                    })

                    # Basic normality check using Shapiro-Wilk for small-medium samples
                    for (level in names(group_counts)) {
                        group_subset <- var_data[group_data == level & !is.na(var_data)]
                        n_subset <- length(group_subset)

                        if (n_subset >= 3 && n_subset <= 200) {
                            # Use Shapiro-Wilk for small-medium samples (n \u2264 200)
                            p_val <- tryCatch(shapiro.test(group_subset)$p.value, error = function(e) 1)
                            if (p_val < 0.05) {
                                warnings <- c(warnings, sprintf(
                                    .(" %s: Data may not be normally distributed in group '%s' (Shapiro-Wilk p = %.3f, consider non-parametric)"),
                                    htmltools::htmlEscape(var), htmltools::htmlEscape(level), p_val
                                ))
                            }
                        } else if (n_subset > 200) {
                            # For large samples (n > 200), Shapiro-Wilk is too sensitive
                            # Instead, check skewness and kurtosis
                            skewness <- tryCatch({
                                mean_val <- mean(group_subset)
                                sd_val <- sd(group_subset)
                                if (sd_val > 0) {
                                    sum((group_subset - mean_val)^3) / (n_subset * sd_val^3)
                                } else 0
                            }, error = function(e) 0)

                            if (abs(skewness) > 1) {
                                warnings <- c(warnings, sprintf(
                                    .(" %s: Large sample (n = %d) in group '%s' shows substantial skewness (%.2f). Visual inspection recommended."),
                                    htmltools::htmlEscape(var), n_subset, htmltools::htmlEscape(level), skewness
                                ))
                            }
                        }
                    }
                }
            }

            return(warnings)
        },

        # Optimized data preparation with robust caching
        .prepareData = function(force_refresh = FALSE) {
            # Create robust hash of current data to detect changes
            current_hash <- digest::digest(list(
                dep = self$options$dep,
                group = self$options$group,
                data_dim = dim(self$data),
                col_names = names(self$data),
                grvar = self$options$grvar,
                # Assumption diagnostics depend on the test type and the
                # equal-variance flag, so include them: changing the test must
                # recompute the interim calculations rather than reuse stale ones.
                typestatistics = self$options$typestatistics,
                varequal = self$options$varequal
            ), algo = "md5")
            
            # Only reprocess if data has changed or forced refresh
            if (!is.null(private$.processedData) && 
                private$.data_hash == current_hash && 
                !force_refresh) {
                return(private$.processedData)
            }
            
            # Checkpoint before expensive data processing
            private$.checkpoint()

            mydata <- self$data
            vars <- self$options$dep

            # Convert numeric variables efficiently - checkpoint before loop
            private$.checkpoint(flush = FALSE)
            for (var in vars) {
                mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])
            }

            # SELECTIVE NA OMISSION: Only drop rows with NAs in variables actually used
            # Build list of columns relevant to this analysis
            relevant_cols <- c(vars, self$options$group)
            if (!is.null(self$options$grvar)) {
                relevant_cols <- c(relevant_cols, self$options$grvar)
            }

            # Count rows before NA removal for reporting
            n_before <- nrow(mydata)

            # Filter to complete cases ONLY for relevant columns
            mydata <- mydata[complete.cases(mydata[relevant_cols]), ]

            n_after <- nrow(mydata)

            # Report NA removal for auditability
            if (n_before > n_after) {
                n_dropped <- n_before - n_after
                na_message <- glue::glue(.("<br> Info: {n_dropped} rows with missing values in analysis variables were excluded.<br>"))
                # ACCUMULATE instead of overwrite
                private$.appendMessage(na_message)
            }

            # Guard: the grouping variable must retain >= 2 non-empty levels after
            # NA removal. A constant column or excessive missingness collapses it to
            # a single level, which otherwise reaches ggstatsplot and produces a
            # cryptic downstream error instead of an actionable message.
            n_group_levels <- nlevels(droplevels(as.factor(mydata[[self$options$group]])))
            if (n_group_levels < 2) {
                jmvcore::reject(
                    .("The grouping variable '{group}' must have at least 2 groups with data after removing missing values (found {n}). Check for a constant column or excessive missing data."),
                    group = self$options$group, n = n_group_levels
                )
            }

            # Validate data quality
            private$.validateDataQuality(mydata, vars)

            # Check statistical assumptions (cached for reuse by the explanations panel)
            assumption_warnings <- private$.checkAssumptions(mydata, vars, self$options$group, self$options$typestatistics)
            private$.assumptionWarnings <- assumption_warnings
            if (length(assumption_warnings) > 0) {
                warning_text <- paste(assumption_warnings, collapse = "<br>")
                # ACCUMULATE instead of overwrite
                private$.appendMessage(glue::glue("<br>{warning_text}<br>"))
            }
            
            # Detect outliers if large dataset - checkpoint before expensive outlier detection
            if (nrow(mydata) > 30) {
                private$.checkpoint(flush = FALSE)
                outliers <- private$.detectOutliers(mydata, vars)
                if (length(outliers) > 0) {
                    for (var in names(outliers)) {
                        # Handle both count (for large datasets) and indices (for smaller datasets)
                        if (is.numeric(outliers[[var]]) && length(outliers[[var]]) == 1) {
                            # For large datasets, we only have the count
                            n_outliers <- outliers[[var]]
                        } else {
                            # For smaller datasets, we have the actual indices
                            n_outliers <- length(outliers[[var]])
                        }
                        # ACCUMULATE instead of overwrite
                        private$.appendMessage(
                            glue::glue(.("<br> {var} has {n_outliers} potential outlier(s) detected<br>"),
                                       var = htmltools::htmlEscape(var))
                        )
                    }
                }
            }

            # Persist accumulated diagnostics to a field (rendered later from .run()).
            private$.storeDiagnostics()

            # Cache the processed data with hash
            private$.processedData <- mydata
            private$.data_hash <- current_hash
            return(mydata)
        },

        # Helper function for title processing
        .processTitle = function(title) {
            if (is.null(title) || title == '') NULL else title
        },

        # Optimized options processing with robust caching
        .prepareOptions = function(force_refresh = FALSE) {
            # Create robust hash of current options to detect changes
            current_options_hash <- digest::digest(list(
                typestatistics = self$options$typestatistics,
                pairwisecomparisons = self$options$pairwisecomparisons,
                pairwisedisplay = self$options$pairwisedisplay,
                padjustmethod = self$options$padjustmethod,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                bfmessage = self$options$bfmessage,
                k = self$options$k,
                conflevel = self$options$conflevel,
                varequal = self$options$varequal,
                multiEndpointCorrection = self$options$multiEndpointCorrection,
                titles = list(self$options$mytitle, self$options$xtitle, self$options$ytitle),
                display = list(self$options$resultssubtitle, self$options$originaltheme),
                dimensions = list(self$options$plotwidth, self$options$plotheight),
                colorblindSafe = self$options$colorblindSafe
            ), algo = "md5")
            
            # Only reprocess if options have changed or forced refresh
            if (!is.null(private$.processedOptions) && 
                private$.options_hash == current_options_hash && 
                !force_refresh) {
                return(private$.processedOptions)
            }

            options <- list(
                typestatistics = self$options$typestatistics,
                pairwisecomparisons = self$options$pairwisecomparisons,
                pairwisedisplay = self$options$pairwisedisplay,
                padjustmethod = self$options$padjustmethod,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                resultssubtitle = self$options$resultssubtitle,
                originaltheme = self$options$originaltheme,
                mytitle = private$.processTitle(self$options$mytitle),
                xtitle = private$.processTitle(self$options$xtitle),
                ytitle = private$.processTitle(self$options$ytitle),
                bfmessage = self$options$bfmessage,
                k = self$options$k,
                conflevel = self$options$conflevel,
                varequal = self$options$varequal,
                colorblindSafe = self$options$colorblindSafe
            )

            # Set default violin and box args for ggstatsplot
            options$violinargs <- list(width = 0.5, alpha = 0.2, na.rm = TRUE)
            options$boxplotargs <- list(width = 0.3, alpha = 0.5, na.rm = TRUE)
            
            # Point args are always used for data points in ggstatsplot
            options$pointargs <- list(
                position = ggplot2::position_jitterdodge(dodge.width = 0.6),
                alpha = 0.4,
                size = 3,
                stroke = 0
            )
            
            # Process centrality parameters if enabled - properly map to ggstatsplot API
            # Note: ggbetweenstats uses centrality.plotting and centrality.type parameters
            if (options$centralityplotting) {
                options$centrality.plotting <- TRUE
                options$centrality.type <- options$centralitytype
                options$centrality.point.args <- list(size = 5, color = "darkred")
                options$centrality.label.args <- list(size = 3, nudge_x = 0.4, segment.linetype = 4)
            } else {
                options$centrality.plotting <- FALSE
            }

            # Cache the processed options with hash
            private$.processedOptions <- options
            private$.options_hash <- current_options_hash
            return(options)
        }

        # run ----
        ,

.run = function() {
    # Preserve and restore the RNG state so any internal randomness used by the
    # statistical backends (e.g. ggstatsplot's Bayesian/robust tests) does not leak
    # global RNG entropy. withr::local_preserve_seed() saves/restores without
    # touching .GlobalEnv. (Outlier detection now uses exact quantiles, not sampling.)
    withr::local_preserve_seed()

    # Generate explanatory content only when requested. Every one of these panels
    # (about/summary/assumptions/interpretation/report) is hidden in the results
    # unless showexplanations is TRUE, so computing them otherwise is wasted work.
    if (self$options$showexplanations) {
        private$.generateAboutContent()
        private$.generateSummary()
        private$.generateAssumptionsContent()
        private$.generateInterpretationGuide()
        private$.generateCopyReadyReport()
    }

    # Initial Message ----
    if (is.null(self$options$dep) || is.null(self$options$group)) {
        todo <- .(
            "<br>Welcome to ClinicoPath
        <br><br>
        This tool creates optimized Box-Violin Plots for comparing continuous variables between groups.
        <br><br>
        This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://www.indrapatil.com/ggstatsplot/reference/ggbetweenstats.html' target='_blank'>here</a> and <a href = 'https://www.indrapatil.com/ggstatsplot/reference/grouped_ggbetweenstats.html' target='_blank'>here</a>.
        <br>
        Please cite jamovi and the packages as given below.
        <br><hr>"
        )

        self$results$todo$setContent(todo)
        # No analysis yet -> clear the separate guidance / diagnostics panels.
        self$results$mecGuidance$setContent("")
        self$results$diagnostics$setContent("")
        self$results$clinicalSummary$setContent("")
        return()

    } else {
        todo <- glue::glue(
            .("<br>Violin plot analysis comparing {vars} by {group}{grouped}.<br><hr>"),
            vars = htmltools::htmlEscape(paste(self$options$dep, collapse=', ')),
            group = htmltools::htmlEscape(self$options$group),
            grouped = if(!is.null(self$options$grvar)) paste0(', grouped by ', htmltools::htmlEscape(self$options$grvar)) else ''
        )

        # The To-Do panel holds ONLY the analysis header.
        self$results$todo$setContent(todo)

        # Data validation
        if (nrow(self$data) == 0)
            jmvcore::reject(.('Data contains no (complete) rows'))

        # Add checkpoint for user feedback
        private$.checkpoint()

        # Compute data + interim diagnostics HERE, in the run context, so their
        # content persists. Setting results content inside the plot render
        # functions (where .prepareData() is also called) is discarded by jamovi
        # and made these panels vanish once plots were visible. .prepareData() is
        # memoized, so the plot render functions reuse this without recomputing.
        private$.prepareData()

        # Separate, always-set panels (never written to during render):
        #   mecGuidance -> multiple-endpoint correction recommendation ("" hides it)
        #   diagnostics -> interim data-quality / assumption calculations ("" hides it)
        self$results$mecGuidance$setContent(private$.endpointGuidanceHtml())
        self$results$diagnostics$setContent(
            if (is.null(private$.diagnosticsHtml)) "" else private$.diagnosticsHtml)

        # Clinical results summary, computed in the run context so it survives
        # option-only re-renders (setting it during .plot render is discarded).
        private$.generateClinicalSummary()
    }
},
.generateAboutContent = function() {
    about_content <- paste0(
        "<div style='padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; margin: 10px 0;'>",
        "<h4 style='color: #007bff; margin-top: 0;'> About Between-Group Comparison</h4>",
        "<p><strong>Purpose:</strong> Compare a continuous variable across different groups to identify significant differences.</p>",
        "<p><strong>When to Use:</strong></p>",
        "<ul>",
        "<li><strong>Clinical Trials:</strong> Compare treatment outcomes (e.g., blood pressure) between a drug and a placebo group.</li>",
        "<li><strong>Biomarker Analysis:</strong> Assess if a biomarker level differs between healthy and diseased patients.</li>",
        "<li><strong>Pathology:</strong> Compare tumor sizes across different cancer grades.</li>",
        "</ul>",
        "<p><strong>Output Includes:</strong></p>",
        "<ul>",
        "<li>Box-violin plots for visualizing data distribution.</li>",
        "<li>Statistical test results (e.g., t-test, ANOVA, or non-parametric alternatives).</li>",
        "<li>Effect size measures (e.g., Cohen's d, eta-squared) to quantify the magnitude of differences.</li>",
        "<li>Post-hoc pairwise comparisons to identify which specific groups differ.</li>",
        "</ul>",
        "</div>"
    )
    
    self$results$about$setContent(about_content)
},
.generateSummary = function() {
    if (is.null(self$options$dep) || is.null(self$options$group)) {
        return()
    }
    
    mydata <- private$.prepareData()
    n_total <- nrow(mydata)
    n_groups <- length(unique(mydata[[self$options$group]]))
    dep_vars <- htmltools::htmlEscape(paste(self$options$dep, collapse = ", "))

    test_method <- switch(self$options$typestatistics,
        "parametric" = if (self$options$varequal) "ANOVA" else "Welch's ANOVA",
        "nonparametric" = "Kruskal-Wallis test",
        "robust" = "Robust ANOVA",
        "bayes" = "Bayesian ANOVA",
        "ANOVA"
    )
    
    # MULTI-ENDPOINT CLARITY: Distinguish single vs multiple variables
    if (length(self$options$dep) == 1) {
        multi_var_note <- ""
    } else {
        multi_var_note <- paste0(
            "<p><strong> Note:</strong> ", length(self$options$dep),
            " dependent variables analyzed. Each variable is tested separately. ",
            "Results in plots show statistics for each individual variable.</p>"
        )
    }

    summary_content <- paste0(
        "<div style='padding: 15px; background-color: #e8f5e8; border-left: 4px solid #28a745; margin: 10px 0;'>",
        "<h4 style='color: #28a745; margin-top: 0;'> Analysis Summary</h4>",
        "<p><strong>Variables Analyzed:</strong> ", dep_vars, " by ", htmltools::htmlEscape(self$options$group), "</p>",
        multi_var_note,  # Add multi-variable clarification
        "<p><strong>Sample Size:</strong> ", n_total, " observations across ", n_groups, " groups</p>",
        "<p><strong>Statistical Method:</strong> ", test_method, "</p>",
        if (self$options$pairwisecomparisons && n_groups > 2) paste0(
            "<p><strong>Post-hoc Analysis:</strong> Pairwise comparisons with ",
            self$options$padjustmethod, " correction</p>"
        ) else "",
        if (!is.null(self$options$grvar)) paste0(
            "<p><strong>Subgroup Analysis:</strong> Results stratified by ", htmltools::htmlEscape(self$options$grvar), "</p>"
        ) else "",
        "<p><strong>Confidence Level:</strong> ", (self$options$conflevel * 100), "%</p>",
        "</div>"
    )
    
    self$results$summary$setContent(summary_content)
},
.generateAssumptionsContent = function() {
    if (is.null(self$options$dep) || is.null(self$options$group)) {
        return()
    }

    mydata <- private$.prepareData()
    # Reuse the assumption warnings computed during .prepareData() instead of
    # re-running Levene's/Shapiro-Wilk per variable/group. Fall back to a fresh
    # computation only if the cache is somehow empty.
    warnings <- private$.assumptionWarnings
    if (is.null(warnings))
        warnings <- private$.checkAssumptions(mydata, self$options$dep, self$options$group, self$options$typestatistics)

    # Add note about multiple endpoints if applicable
    multi_endpoint_note <- ""
    if (length(self$options$dep) > 1) {
        multi_endpoint_note <- paste0(
            "<div style='background-color: #ffe5e5; border-left: 4px solid #dc3545; padding: 10px; margin: 10px 0;'>",
            "<p><strong> Multiple Endpoint Testing:</strong> You are analyzing ",
            length(self$options$dep), " dependent variables. Each test uses the standard \u03b1 = 0.05 threshold. ",
            "The 'Multiple Endpoint Correction Guidance' option above provides instructions for manual p-value adjustment. ",
            "<strong>Important:</strong> Corrections are NOT applied automatically by this analysis.</p>",
            "</div>"
        )
    }

    assumptions_content <- paste0(
        "<div style='padding: 15px; background-color: #fff3cd; border-left: 4px solid #ffc107; margin: 10px 0;'>",
        "<h4 style='color: #856404; margin-top: 0;'> Statistical Assumptions & Warnings</h4>",

        multi_endpoint_note,

        "<p><strong>General Assumptions:</strong></p>",
        "<ul>",
        "<li>Dependent variable is continuous.</li>",
        "<li>Observations are independent.</li>",
        "<li>Groups are independent.</li>",
        if (self$options$typestatistics == "parametric") "<li>Residuals are normally distributed.</li>",
        if (self$options$typestatistics == "parametric" && self$options$varequal) "<li>Homogeneity of variances (equal variances).</li>",
        "</ul>",

        if (length(warnings) > 0) paste0(
            "<p><strong>Detected Issues & Guidance:</strong></p>",
            "<ul><li>", paste(warnings, collapse = "</li><li>"), "</li></ul>"
        ) else "<p>No major issues detected with statistical assumptions.</p>",

        "</div>"
    )

    self$results$assumptions$setContent(assumptions_content)
},
.generateInterpretationGuide = function() {
    interpretation_content <- paste0(
        "<div style='padding: 15px; background-color: #d1ecf1; border-left: 4px solid #17a2b8; margin: 10px 0;'>",
        "<h4 style='color: #0c5460; margin-top: 0;'> How to Interpret Results</h4>",
        
        "<p><strong>Statistical Significance:</strong></p>",
        "<ul>",
        "<li><strong>p < 0.05:</strong> Significant difference between groups.</li>",
        "<li><strong>p \u2265 0.05:</strong> No significant difference detected.</li>",
        "</ul>",
        
        "<p><strong>Effect Size Interpretation:</strong></p>",
        "<ul>",
        "<li><strong>Cohen's d:</strong> 0.2 (small), 0.5 (medium), 0.8 (large) effect.</li>",
        "<li><strong>Eta-squared (\u03b7\u00b2):</strong> 0.01 (small), 0.06 (medium), 0.14 (large) effect.</li>",
        "</ul>",
        
        "<p><strong>Clinical Context:</strong></p>",
        "<ul>",
        "<li>Consider if the observed difference is clinically meaningful, not just statistically significant.</li>",
        "<li>Look at the confidence intervals to understand the precision of the effect size estimate.</li>",
        "<li>Examine the plots to understand the distribution and overlap between groups.</li>",
        "</ul>",
        "</div>"
    )
    
    self$results$interpretation$setContent(interpretation_content)
},
.generateCopyReadyReport = function() {
    if (is.null(self$options$dep) || is.null(self$options$group)) {
        return()
    }

    mydata <- private$.prepareData()
    n_total <- nrow(mydata)
    n_groups <- length(unique(mydata[[self$options$group]]))

    # MULTI-ENDPOINT CLARITY: Distinguish single vs multiple variables
    if (length(self$options$dep) == 1) {
        dep_vars <- htmltools::htmlEscape(self$options$dep[1])
        multi_var_note <- ""
    } else {
        dep_vars <- htmltools::htmlEscape(paste(self$options$dep, collapse = ", "))
        multi_var_note <- paste0(
            "<p><strong> Note:</strong> ", length(self$options$dep),
            " dependent variables analyzed. Each variable is tested separately. ",
            "Results in plots show statistics for each individual variable.</p>"
        )
    }
    
    test_method <- switch(self$options$typestatistics,
        "parametric" = if (self$options$varequal) "ANOVA" else "Welch's ANOVA",
        "nonparametric" = "Kruskal-Wallis test", 
        "robust" = "Robust ANOVA",
        "bayes" = "Bayesian ANOVA",
        "ANOVA"
    )
    
    report_template <- paste0(
        "<div style='padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; margin: 10px 0;'>",
        "<h4 style='color: #495057; margin-top: 0;'> Copy-Ready Report Template</h4>",
        
        "<div style='background-color: #ffffff; padding: 15px; border: 1px dashed #6c757d; margin: 10px 0;'>",
        "<h5>Methods:</h5>",
        "<p>A between-groups analysis was conducted to compare the levels of ", dep_vars,
        " across ", n_groups, " groups of ", htmltools::htmlEscape(self$options$group), ". A ", test_method,
        " was used to test for significant differences. ",
        
        if (self$options$pairwisecomparisons && n_groups > 2) {
            paste0("Post-hoc pairwise comparisons were conducted with ", 
                  self$options$padjustmethod, " correction for multiple testing. ")
        } else "",
        
        "Statistical significance was assessed at the \u03b1 = ", (1 - self$options$conflevel), " level.",
        "</p>",
        
        "<h5>Results:</h5>",
        "<p>[Insert specific results here: test statistic, p-value, effect size with 95% CI]</p>",
        "<p>Example: \"The analysis revealed a statistically significant difference in [dependent variable] between the groups (",
        "F(", n_groups - 1, ", ", n_total - n_groups, ") = [value], p = [value], \u03b7\u00b2 = [value], 95% CI [lower, upper]). ",
        "Post-hoc tests showed that Group A had significantly higher levels than Group B (p = [value]).\"</p>",
        
        "<h5>Conclusion:</h5>",
        "<p>[Interpret findings in clinical context, considering both statistical significance and clinical relevance]</p>",
        "</div>",
        
        "<button onclick='navigator.clipboard.writeText(this.parentElement.querySelector(\"div\").innerText)' ",
        "style='background-color: #007bff; color: white; border: none; padding: 8px 16px; border-radius: 4px; cursor: pointer;'>",
        " Copy Template to Clipboard</button>",
        "</div>"
    )
    
    self$results$report$setContent(report_template)
},
.plot = function(image, ggtheme, theme, ...) {
            # Use shared validation helper ----
            if (!private$.validateInputs())
                return()

            # Add checkpoint for user feedback
            private$.checkpoint()

            # Use optimized data and options preparation
            mydata <- private$.prepareData()
            opts <- private$.prepareOptions()
            
            dep <- self$options$dep
            group <- self$options$group

            # Single dependent variable analysis ----
            if (length(dep) == 1) {
                private$.checkpoint()
                
                # Build argument list
                args_list <- list(
                    data = mydata,
                    x = rlang::sym(group),
                    y = rlang::sym(dep),
                    title = opts$mytitle,
                    xlab = opts$xtitle,
                    ylab = opts$ytitle,
                    type = opts$typestatistics,
                    pairwise.comparisons = opts$pairwisecomparisons,
                    pairwise.display = opts$pairwisedisplay,
                    p.adjust.method = opts$padjustmethod,
                    effsize.type = opts$effsizetype,
                    bf.message = opts$bfmessage,
                    k = opts$k,
                    conf.level = opts$conflevel,
                    var.equal = opts$varequal,
                    point.args = opts$pointargs,
                    results.subtitle = opts$resultssubtitle,
                    centrality.plotting = if (!is.null(opts$centrality.plotting)) opts$centrality.plotting else FALSE,
                    centrality.type = if (!is.null(opts$centrality.type)) opts$centrality.type else NULL
                )
                
                # Add violin.args and boxplot.args only if they exist
                if (!is.null(opts$violinargs)) {
                    args_list$violin.args <- opts$violinargs
                }
                if (!is.null(opts$boxplotargs)) {
                    args_list$boxplot.args <- opts$boxplotargs
                }
                
                plot <- private$.tryPlot(do.call(ggstatsplot::ggbetweenstats, args_list))

                # Apply theme using helper
                plot <- private$.applyTheme(plot, opts, ggtheme)
            }

            # Multiple dependent variables analysis ----
            if (length(dep) > 1) {
                private$.checkpoint()
                
                dep2 <- as.list(dep)
                dep2_symbols <- purrr::map(dep2, rlang::sym)

                # Checkpoint before expensive multiple plot generation
                private$.checkpoint(flush = FALSE)
                plotlist <- purrr::pmap(
                    .l = list(
                        y = dep2_symbols,
                        messages = FALSE
                    ),
                    .f = function(y, messages) {
                        plot_args <- list(
                            data = mydata,
                            x = rlang::sym(group),
                            y = y,
                            messages = messages,
                            title = opts$mytitle,
                            xlab = opts$xtitle,
                            ylab = opts$ytitle,
                            type = opts$typestatistics,
                            pairwise.comparisons = opts$pairwisecomparisons,
                            pairwise.display = opts$pairwisedisplay,
                            p.adjust.method = opts$padjustmethod,
                            effsize.type = opts$effsizetype,
                            bf.message = opts$bfmessage,
                            k = opts$k,
                            conf.level = opts$conflevel,
                            var.equal = opts$varequal,
                            point.args = opts$pointargs,
                            results.subtitle = opts$resultssubtitle,
                            centrality.plotting = if (!is.null(opts$centrality.plotting)) opts$centrality.plotting else FALSE,
                            centrality.type = if (!is.null(opts$centrality.type)) opts$centrality.type else NULL
                        )
                        
                        # Add violin.args and boxplot.args if they exist
                        if (!is.null(opts$violinargs)) {
                            plot_args$violin.args <- opts$violinargs
                        }
                        if (!is.null(opts$boxplotargs)) {
                            plot_args$boxplot.args <- opts$boxplotargs
                        }
                        
                        private$.tryPlot(do.call(ggstatsplot::ggbetweenstats, plot_args))
                    }
                )

                # Apply theme to all plots using helper - checkpoint before theme application
                private$.checkpoint(flush = FALSE)
                plotlist <- lapply(plotlist, function(p) private$.applyTheme(p, opts, ggtheme))

                # Checkpoint before combining plots
                private$.checkpoint(flush = FALSE)
                plot <- ggstatsplot::combine_plots(
                    plotlist = plotlist,
                    plotgrid.args = list(
                        ncol = 1,
                        heights = rep(1, length(plotlist))
                    ),
                    annotation.args = list(
                        tag_levels = "A"
                    )
                )
            }

            # Print Plot ----

                print(plot)
                TRUE
        },
        .plot2 = function(image, ggtheme, theme, ...) {
            # Use shared validation helper with additional grouping check ----
            if (!private$.validateInputs() || is.null(self$options$grvar))
                return()

            # Add checkpoint for user feedback
            private$.checkpoint()

            # Use optimized data and options preparation (cached)
            mydata <- private$.prepareData()
            opts <- private$.prepareOptions()
            
            dep <- self$options$dep
            group <- self$options$group
            grvar <- self$options$grvar

            # Single dependent variable grouped analysis ----
            if (length(dep) == 1) {
                private$.checkpoint()
                
                selected_theme <- if (!opts$originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()

                # Calculate optimal grid layout for grouped plots
                if (!is.null(mydata[[grvar]])) {
                    num_levels <- nlevels(as.factor(mydata[[grvar]]))
                    ncol_layout <- ceiling(sqrt(num_levels))
                } else {
                    ncol_layout <- 2  # default fallback
                }

                # Build argument list for grouped analysis
                grouped_args <- list(
                    data = mydata,
                    x = rlang::sym(group),
                    y = rlang::sym(dep),
                    grouping.var = rlang::sym(grvar),
                    type = opts$typestatistics,
                    pairwise.comparisons = opts$pairwisecomparisons,
                    pairwise.display = opts$pairwisedisplay,
                    p.adjust.method = opts$padjustmethod,
                    effsize.type = opts$effsizetype,
                    bf.message = opts$bfmessage,
                    k = opts$k,
                    conf.level = opts$conflevel,
                    var.equal = opts$varequal,
                    point.args = opts$pointargs,
                    results.subtitle = opts$resultssubtitle,
                    centrality.plotting = if (!is.null(opts$centrality.plotting)) opts$centrality.plotting else FALSE,
                    centrality.type = if (!is.null(opts$centrality.type)) opts$centrality.type else NULL,
                    ggtheme = selected_theme,
                    plotgrid.args = list(
                        ncol = ncol_layout
                    ),
                    annotation.args = list(
                        tag_levels = "A"
                    )
                )
                
                # Add violin.args and boxplot.args if they exist
                if (!is.null(opts$violinargs)) {
                    grouped_args$violin.args <- opts$violinargs
                }
                if (!is.null(opts$boxplotargs)) {
                    grouped_args$boxplot.args <- opts$boxplotargs
                }
                
                plot2 <- private$.tryPlot(do.call(ggstatsplot::grouped_ggbetweenstats, grouped_args))
            }

            # Multiple dependent variables grouped analysis ----
            if (length(dep) > 1) {
                private$.checkpoint()

                selected_theme <- if (!opts$originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()

                # Calculate optimal grid layout for grouped plots
                if (!is.null(mydata[[grvar]])) {
                    num_levels <- nlevels(as.factor(mydata[[grvar]]))
                    ncol_layout <- ceiling(sqrt(num_levels))
                } else {
                    ncol_layout <- 2  # default fallback
                }

                dep2 <- as.list(dep)
                dep2_symbols <- purrr::map(dep2, rlang::sym)

                # Checkpoint before expensive multiple grouped plot generation
                private$.checkpoint(flush = FALSE)
                plotlist <- purrr::pmap(
                    .l = list(
                        y = dep2_symbols,
                        messages = FALSE
                    ),
                    .f = function(y, messages) {
                        # Build argument list for multiple variable grouped analysis
                        grouped_multi_args <- list(
                            data = mydata,
                            x = rlang::sym(group),
                            y = y,
                            grouping.var = rlang::sym(grvar),
                            messages = messages,
                                    type = opts$typestatistics,
                            pairwise.comparisons = opts$pairwisecomparisons,
                            pairwise.display = opts$pairwisedisplay,
                            p.adjust.method = opts$padjustmethod,
                            effsize.type = opts$effsizetype,
                            bf.message = opts$bfmessage,
                            k = opts$k,
                            conf.level = opts$conflevel,
                            var.equal = opts$varequal,
                            point.args = opts$pointargs,
                            results.subtitle = opts$resultssubtitle,
                            centrality.plotting = if (!is.null(opts$centrality.plotting)) opts$centrality.plotting else FALSE,
                            centrality.type = if (!is.null(opts$centrality.type)) opts$centrality.type else NULL,
                            ggtheme = selected_theme,
                            plotgrid.args = list(
                                ncol = ncol_layout
                            ),
                            annotation.args = list(
                                tag_levels = "A"
                            )
                        )
                        
                        # Add violin.args and boxplot.args if they exist
                        if (!is.null(opts$violinargs)) {
                            grouped_multi_args$violin.args <- opts$violinargs
                        }
                        if (!is.null(opts$boxplotargs)) {
                            grouped_multi_args$boxplot.args <- opts$boxplotargs
                        }
                        
                        private$.tryPlot(do.call(ggstatsplot::grouped_ggbetweenstats, grouped_multi_args))
                    }
                )

                # Checkpoint before combining grouped plots
                private$.checkpoint(flush = FALSE)
                plot2 <- ggstatsplot::combine_plots(
                    plotlist = plotlist,
                    plotgrid.args = list(
                        ncol = 1,
                        heights = rep(1, length(plotlist))
                    ),
                    annotation.args = list(
                        tag_levels = "A"
                    )
                )
            }

            # Print Plot ----
            print(plot2)
            TRUE
        },
        .plotGGPubr = function(image, ggtheme, theme, ...) {
            # Validate inputs
            if (!private$.validateInputs())
                return()

            # Skip if ggpubr plot not requested
            if (!self$options$addGGPubrPlot)
                return()

            private$.checkpoint()

            # Prepare data
            mydata <- private$.prepareData()
            dep <- self$options$dep
            group <- self$options$group

            # Get palette
            palette <- if (self$options$ggpubrPalette == "default") NULL else self$options$ggpubrPalette

            # Single dependent variable
            if (length(dep) == 1) {
                # Build arguments conditionally
                args <- list(
                    data = mydata,
                    x = group,
                    y = dep,
                    color = group,
                    title = if (nchar(self$options$mytitle) > 0) self$options$mytitle else NULL,
                    xlab = if (nchar(self$options$xtitle) > 0) self$options$xtitle else group,
                    ylab = if (nchar(self$options$ytitle) > 0) self$options$ytitle else dep,
                    add = if (self$options$ggpubrAddPoints) "jitter" else NULL,
                    palette = palette
                )

                # Create plot based on type
                if (self$options$ggpubrPlotType == "boxplot") {
                    plot <- do.call(ggpubr::ggboxplot, args)
                } else if (self$options$ggpubrPlotType == "violin") {
                    plot <- do.call(ggpubr::ggviolin, args)
                } else if (self$options$ggpubrPlotType == "boxviolin") {
                    # Create violin plot and add boxplot
                    plot <- do.call(ggpubr::ggviolin, args)
                    plot <- plot + ggplot2::geom_boxplot(width = 0.1)
                }

                # Add statistical comparisons
                if (self$options$ggpubrAddStats) {
                    plot <- plot + ggpubr::stat_compare_means()
                }

                # Apply theme
                plot <- plot + ggpubr::theme_pubr()

                private$.tryPlot(print(plot))
            }

            # Multiple dependent variables
            if (length(dep) > 1) {
                dep_list <- as.list(dep)

                plotlist <- lapply(dep_list, function(depvar) {
                    args <- list(
                        data = mydata,
                        x = group,
                        y = depvar,
                        color = group,
                        title = depvar,
                        xlab = group,
                        ylab = depvar,
                        add = if (self$options$ggpubrAddPoints) "jitter" else NULL,
                        palette = palette
                    )

                    if (self$options$ggpubrPlotType == "boxplot") {
                        p <- do.call(ggpubr::ggboxplot, args)
                    } else if (self$options$ggpubrPlotType == "violin") {
                        p <- do.call(ggpubr::ggviolin, args)
                    } else if (self$options$ggpubrPlotType == "boxviolin") {
                        p <- do.call(ggpubr::ggviolin, args)
                        p <- p + ggplot2::geom_boxplot(width = 0.1)
                    }

                    if (self$options$ggpubrAddStats) {
                        p <- p + ggpubr::stat_compare_means()
                    }

                    p <- p + ggpubr::theme_pubr()
                    return(p)
                })

                plot <- ggpubr::ggarrange(plotlist = plotlist, ncol = 1, nrow = length(dep))
                private$.tryPlot(print(plot))
            }

            TRUE
        },
        .plotGGPubr2 = function(image, ggtheme, theme, ...) {
            # Validate inputs
            if (!private$.validateInputs())
                return()

            # Skip if ggpubr plot not requested or no grouping variable
            if (!self$options$addGGPubrPlot || is.null(self$options$grvar))
                return()

            private$.checkpoint()

            # Prepare data
            mydata <- private$.prepareData()
            dep <- self$options$dep
            group <- self$options$group
            grvar <- self$options$grvar

            # Get palette
            palette <- if (self$options$ggpubrPalette == "default") NULL else self$options$ggpubrPalette

            # Single dependent variable with faceting
            if (length(dep) == 1) {
                args <- list(
                    data = mydata,
                    x = group,
                    y = dep,
                    color = group,
                    title = if (nchar(self$options$mytitle) > 0) self$options$mytitle else NULL,
                    xlab = if (nchar(self$options$xtitle) > 0) self$options$xtitle else group,
                    ylab = if (nchar(self$options$ytitle) > 0) self$options$ytitle else dep,
                    add = if (self$options$ggpubrAddPoints) "jitter" else NULL,
                    palette = palette,
                    facet.by = grvar
                )

                if (self$options$ggpubrPlotType == "boxplot") {
                    plot <- do.call(ggpubr::ggboxplot, args)
                } else if (self$options$ggpubrPlotType == "violin") {
                    plot <- do.call(ggpubr::ggviolin, args)
                } else if (self$options$ggpubrPlotType == "boxviolin") {
                    plot <- do.call(ggpubr::ggviolin, args)
                    plot <- plot + ggplot2::geom_boxplot(width = 0.1)
                }

                if (self$options$ggpubrAddStats) {
                    plot <- plot + ggpubr::stat_compare_means()
                }

                plot <- plot + ggpubr::theme_pubr()
                private$.tryPlot(print(plot))
            }

            # Multiple dependent variables with faceting
            if (length(dep) > 1) {
                dep_list <- as.list(dep)

                plotlist <- lapply(dep_list, function(depvar) {
                    args <- list(
                        data = mydata,
                        x = group,
                        y = depvar,
                        color = group,
                        title = depvar,
                        xlab = group,
                        ylab = depvar,
                        add = if (self$options$ggpubrAddPoints) "jitter" else NULL,
                        palette = palette,
                        facet.by = grvar
                    )

                    if (self$options$ggpubrPlotType == "boxplot") {
                        p <- do.call(ggpubr::ggboxplot, args)
                    } else if (self$options$ggpubrPlotType == "violin") {
                        p <- do.call(ggpubr::ggviolin, args)
                    } else if (self$options$ggpubrPlotType == "boxviolin") {
                        p <- do.call(ggpubr::ggviolin, args)
                        p <- p + ggplot2::geom_boxplot(width = 0.1)
                    }

                    if (self$options$ggpubrAddStats) {
                        p <- p + ggpubr::stat_compare_means()
                    }

                    p <- p + ggpubr::theme_pubr()
                    return(p)
                })

                plot <- ggpubr::ggarrange(plotlist = plotlist, ncol = 1, nrow = length(dep))
                private$.tryPlot(print(plot))
            }

            TRUE
        }

    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for Between-Groups Statistics analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            # Get arguments
            args <- private$.asArgs(incData = FALSE)
            if (args != '')
                args <- paste0(',\n    ', args)

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, '::jjbetweenstats(\n    data = data', args, ')')
        }
    ) # End of public list

)
