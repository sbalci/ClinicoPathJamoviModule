#' @title Bar Charts
#'
#' @importFrom R6 R6Class
#' @importFrom jmvcore .
#' @importFrom digest digest
#' @importFrom purrr map imap
#' @importFrom rlang sym
#' @importFrom glue glue
#'
#' @return An \code{R6} class generator object for the \code{jjbarstatsClass} backend; used internally by the jamovi analysis wrapper and not called directly.

jjbarstatsClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "jjbarstatsClass",
        inherit = jjbarstatsBase,
        private = list(
        # Single read path for options. Presets used to override some of them here;
        # they now set the controls in the GUI (jamovi/js/jjbarstats.events.js).
        .option = function(option) self$options$option(option)$value,

            # Cache variables for performance
            .cached_data = NULL,
            .data_hash = NULL,
            .validation_passed = FALSE,

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

                # De-duplicate: the same notice can be added by multiple render passes
                # (.plot + .plot2 when grvar is set, plot resizes that re-render without a
                # fresh .run(), or repeated dependent variables). Key on type/title/content.
                keys <- vapply(private$.noticeList, function(n) {
                    paste(n$type, n$title, n$content, sep = "\n")
                }, character(1))
                private$.noticeList <- private$.noticeList[!duplicated(keys)]

                # Plain text only - notices avoid HTML by project convention; the Preformatted
                # output item renders this literally (no markup, no injection surface).
                blocks <- vapply(private$.noticeList, function(notice) {
                    prefix <- switch(notice$type,
                        ERROR          = "ERROR: ",
                        STRONG_WARNING = "STRONG WARNING: ",
                        WARNING        = "WARNING: ",
                        "")
                    paste0(prefix, notice$title, "\n", notice$content)
                }, character(1))

                self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
            },

            # init ----

            .init = function() {

                deplen <- length(self$options$dep)

                # Improved height calculation to prevent compressed plots
                # Add extra spacing when combining multiple plots vertically
                if (deplen > 1) {
                    # Add 15% extra height per plot for better spacing
                    total_height <- deplen * 450 * 1.15
                } else {
                    total_height <- 450
                }

                self$results$plot$setSize(650, total_height)

                if (!is.null(self$options$grvar)) {

                    mydata <- self$data

                    grvar <-  self$options$grvar

                    num_levels <- nlevels(
                        as.factor(mydata[[grvar]])
                    )

                    # For grouped analysis, calculate width based on layout
                    ncol_estimate <- ceiling(sqrt(num_levels))
                    grouped_width <- ncol_estimate * 650

                    # Height calculation for grouped plots with multiple dependent variables
                    if (deplen > 1) {
                        grouped_height <- deplen * 450 * 1.15
                    } else {
                        # For single dep var, height based on number of grouping levels
                        nrow_estimate <- ceiling(num_levels / ncol_estimate)
                        grouped_height <- nrow_estimate * 450
                    }

                    self$results$plot2$setSize(grouped_width, grouped_height)

                }

            },

            # Helper Methods ----

            .validateVariables = function() {
                dep_vars <- self$options$dep
                group_var <- self$options$group
                
                # Check if variables exist
                all_vars <- c(dep_vars, group_var)
                if (!is.null(self$options$grvar)) {
                    all_vars <- c(all_vars, self$options$grvar)
                }
                if (!is.null(self$options$counts)) {
                    all_vars <- c(all_vars, self$options$counts)
                }
                
                missing_vars <- all_vars[!all_vars %in% names(self$data)]
                if (length(missing_vars) > 0) {
                    jmvcore::reject(.('Variables not found in data: {vars}'),
                                    vars = paste(missing_vars, collapse = ", "))
                }
                
                # Check that variables are appropriate for bar charts (categorical)
                # Exclude counts variable from categorical check
                vars_to_check <- setdiff(all_vars, self$options$counts)
                for (var in vars_to_check) {
                    if (!is.null(var)) {
                        var_class <- class(self$data[[var]])
                        if (!any(c("factor", "character", "logical") %in% var_class)) {
                            # Try to convert numeric to factor if it has few unique values
                            if (is.numeric(self$data[[var]])) {
                                unique_vals <- length(unique(self$data[[var]][!is.na(self$data[[var]])]))
                                if (unique_vals > 10) {
                                    jmvcore::reject(.("Variable '{var}' appears to be continuous ({n} unique values). Bar charts are for categorical data. Consider converting to groups first."),
                                                    var = var, n = unique_vals)
                                }
                            }
                        }
                    }
                }
                
                # Validate counts variable if provided
                if (!is.null(self$options$counts)) {
                    # Checkpoint before expensive validation
                    private$.checkpoint(flush = FALSE)
                    counts_var <- self$options$counts
                    if (!is.numeric(self$data[[counts_var]])) {
                        jmvcore::reject(.("Counts variable '{var}' must be numeric."),
                                        var = counts_var)
                    }
                    if (any(self$data[[counts_var]] < 0, na.rm = TRUE)) {
                        jmvcore::reject(.("Counts variable '{var}' contains negative values."),
                                        var = counts_var)
                    }
                    count_values <- self$data[[counts_var]]
                    count_values <- count_values[!is.na(count_values)]
                    if (any(!is.finite(count_values)) || any(count_values != floor(count_values)))
                        jmvcore::reject(.("Frequency counts must be finite whole numbers."))
                    # A zero total is not a small sample, it is no sample. Without
                    # this the summary panel announced "Sample Size: 0 observations"
                    # next to "Statistical Method: Chi-square test of independence"
                    # and still drew a chart.
                    if (sum(self$data[[counts_var]], na.rm = TRUE) <= 0) {
                        jmvcore::reject(.("Counts variable '{var}' sums to zero - there are no observations to analyse."),
                                        var = counts_var)
                    }
                }
                
                # Enhanced validation for statistical tests
                private$.validateStatisticalRequirements(dep_vars, group_var)
                
                return(TRUE)
            },

            .validateStatisticalRequirements = function(dep_vars, group_var) {
                # WEIGHTED GROUP SIZES: Use weighted counts, not row counts
                if (!is.null(group_var) && group_var %in% names(self$data)) {
                    # Checkpoint before table calculation
                    private$.checkpoint(flush = FALSE)

                    # Get weighted group sizes
                    group_sizes <- private$.getWeightedGroupCounts(self$data, group_var)

                    # NOTE: User-facing data-quality warnings (small groups, zero cells,
                    # low expected counts) are emitted via private$.emitDataQualityNotices()
                    # from .run() so they render in the jamovi GUI and are not hidden behind
                    # the validation cache. Here we keep only the hard-stop validity check.
                    if (length(group_sizes) < 2) {
                        jmvcore::reject(.("Grouping variable must have at least 2 categories for comparison."))
                    }
                }

                # Check dependent variables have sufficient variation
                for (dep_var in dep_vars) {
                    if (!is.null(dep_var) && dep_var %in% names(self$data)) {
                        # Checkpoint before each table calculation in loop
                        private$.checkpoint(flush = FALSE)

                        # Non-missing levels; the same count for weighted and unweighted data
                        dep_levels_count <- length(table(self$data[[dep_var]], useNA = "no"))

                        if (dep_levels_count < 2) {
                            jmvcore::reject(.("Variable '{var}' has insufficient variation (only {n} level). Need at least 2 categories."),
                                            var = dep_var, n = dep_levels_count)
                        }
                    }
                }
            },

            .getCachedData = function() {
                # Create hash of current data AND ACTUAL DATA VALUES + options state
                # CRITICAL FIX: Include data content hash to prevent stale cache returns
                relevant_cols <- unique(c(
                    self$options$dep,
                    self$options$group,
                    self$options$grvar,
                    self$options$counts
                ))
                relevant_cols <- relevant_cols[!sapply(relevant_cols, is.null)]
                relevant_cols <- relevant_cols[relevant_cols %in% names(self$data)]

                # Hash ACTUAL DATA VALUES in relevant columns
                data_content_hash <- if (length(relevant_cols) > 0) {
                    digest::digest(self$data[, relevant_cols, drop = FALSE], algo = "md5")
                } else {
                    NULL
                }

                current_hash <- digest::digest(list(
                    data_dim = dim(self$data),
                    data_names = names(self$data),
                    data_content = data_content_hash,  #  CRITICAL: Include actual data
                    options = list(
                        dep = self$options$dep,
                        group = self$options$group,
                        grvar = self$options$grvar,
                        counts = self$options$counts,
                        paired = private$.option("paired"),
                        label = self$options$label
                    )
                ), algo = "md5")
                
                # Return cached data if hash matches and validation passed
                if (!is.null(private$.cached_data) && 
                    !is.null(private$.data_hash) &&
                    private$.data_hash == current_hash && 
                    private$.validation_passed) {
                    return(private$.cached_data)
                }
                
                # Checkpoint before expensive validation and data preparation
                private$.checkpoint(flush = FALSE)
                # Validate and prepare fresh data
                private$.validateVariables()
                private$.cached_data <- private$.prepareData()
                private$.data_hash <- current_hash
                private$.validation_passed <- TRUE
                
                return(private$.cached_data)
            },

            .selectTheme = function(ggtheme) {
                if (self$options$originaltheme) {
                    return(ggstatsplot::theme_ggstatsplot())
                } else {
                    return(ggtheme)
                }
            },

            .methodDescription = function(data) {
                # statsExpressions calls stats::mcnemar.test(correct = FALSE): say so, because
                # base R's default is the continuity-corrected statistic and the two differ.
                if (isTRUE(private$.option("paired"))) return("McNemar's test (chi-squared without continuity correction)")
                if (identical(private$.option("typestatistics"), "bayes"))
                    return("Bayesian contingency table analysis")
                methods <- vapply(self$options$dep, function(dv) {
                    if (isTRUE(private$.option("resultssubtitle")) &&
                        !is.null(private$.exactSubtitle(data, dv)))
                        "Fisher's exact test for sparse 2-by-2 tables"
                    else "Pearson's chi-squared test"
                }, character(1))
                desc <- paste(unique(methods), collapse = "; ")
                if (!is.null(self$options$grvar))
                    desc <- paste0(desc, "; split panels use Pearson's chi-squared test")
                desc
            },

            .generateAboutContent = function() {
                about_content <- paste0(
                    "<div style='padding: 15px; background-color: rgba(138, 155, 172, 0.06); border-left: 4px solid #007bff; margin: 10px 0; color: inherit;'>",
                    "<h4 style='color: #007bff; margin-top: 0;'> About Bar Chart Analysis</h4>",
                    "<p><strong>Purpose:</strong> Compare the distribution of categorical variables across groups using statistical testing.</p>",
                    "<p><strong>When to Use:</strong></p>",
                    "<ul>",
                    "<li><strong>Diagnostic Tests:</strong> Compare test results (positive/negative) across patient groups</li>",
                    "<li><strong>Treatment Response:</strong> Analyze response rates across different treatments</li>",
                    "<li><strong>Biomarker Expression:</strong> Compare expression levels (low/medium/high) by clinical factors</li>",
                    "<li><strong>Risk Factor Analysis:</strong> Examine how risk factors relate to outcomes</li>",
                    "</ul>",
                    "<p><strong>Output Includes:</strong></p>",
                    "<ul>",
                    "<li>Visual bar chart with statistical annotations</li>",
                    "<li>Chi-square or appropriate statistical test results</li>",
                    "<li>Effect size measures and confidence intervals</li>",
                    "<li>Optional proportion tests within each group (unadjusted p-values above the bars)</li>",
                    "</ul>",
                    "</div>"
                )
                
                self$results$about$setContent(about_content)
            },

            .generateSummary = function(analysis_data) {
                if (is.null(self$options$dep) || is.null(self$options$group)) {
                    return()
                }

                # WEIGHTED COUNTS: Use effective sample size, not row count
                n_total <- private$.getEffectiveSampleSize(analysis_data)

                # Get weighted group counts
                group_counts <- private$.getWeightedGroupCounts(analysis_data, self$options$group)
                n_groups <- length(group_counts)

                dep_vars <- htmltools::htmlEscape(paste(self$options$dep, collapse = ", "))

                test_method <- private$.methodDescription(analysis_data)

                summary_content <- paste0(
                    "<div style='padding: 15px; background-color: rgba(33, 159, 33, 0.1); border-left: 4px solid #28a745; margin: 10px 0; color: inherit;'>",
                    "<h4 style='color: #28a745; margin-top: 0;'> Analysis Summary</h4>",
                    "<p><strong>Variables Analyzed:</strong> ", dep_vars, " by ", htmltools::htmlEscape(self$options$group), "</p>",
                    "<p><strong>Sample Size:</strong> ", n_total, " observations across ", n_groups, " groups</p>",
                    "<p><strong>Statistical Method:</strong> ", test_method, "</p>",
                    if (isTRUE(private$.option("proportiontest")))
                        "<p><strong>Proportion Tests:</strong> one goodness-of-fit test per group; the p-values above the bars are not adjusted for multiple testing</p>"
                    else "",
                    if (!is.null(self$options$grvar)) paste0(
                        "<p><strong>Subgroup Analysis:</strong> Results stratified by ", htmltools::htmlEscape(self$options$grvar), "</p>"
                    ) else "",
                    "<p><strong>Confidence Level:</strong> ", (self$options$conflevel * 100), "%</p>",
                    "</div>"
                )
                
                self$results$summary$setContent(summary_content)
            },

            # Pure decision helper for the "Expected proportions" box. Returns the
            # vector actually handed to the plot plus the notice explaining any
            # departure from what the user typed, so .run() can render it.
            #
            # A malformed entry used to be swallowed: the notice was raised inside
            # .createBarPlot (i.e. during .plot(), where notices are discarded), and
            # a length mismatch was never checked at all - "0.5,0.5" against a
            # three-level outcome ran a proportion test on proportions the user
            # never specified, with nothing on screen to say so.
            .computeRatioDecision = function(data, dep_var) {
                none <- list(ratio = NULL, notice = NULL)
                raw <- self$options$ratio
                if (is.null(raw) || !nzchar(trimws(raw)))
                    return(none)

                warn <- function(msg)
                    list(ratio = NULL,
                         notice = list('WARNING', 'Expected proportions ignored',
                                       paste0(msg, " Equal proportions were used instead.")))

                vals <- suppressWarnings(as.numeric(trimws(strsplit(raw, ",")[[1]])))

                if (anyNA(vals))
                    return(warn(sprintf(
                        "'%s' is not a list of numbers; expected comma-separated values such as '0.5,0.5'.",
                        raw)))
                if (any(vals <= 0))
                    return(warn(sprintf("'%s' contains a zero or negative proportion.", raw)))

                # Must line up with the outcome's categories, not the groups.
                n_lvl <- if (!is.null(dep_var) && dep_var %in% names(data))
                    length(unique(stats::na.omit(data[[dep_var]]))) else NA_integer_
                if (!is.na(n_lvl) && length(vals) != n_lvl)
                    return(warn(sprintf(
                        "%d proportion%s given for '%s', which has %d categories.",
                        length(vals), if (length(vals) == 1) "" else "s", dep_var, n_lvl)))

                if (abs(sum(vals) - 1) > 0.001) {
                    norm <- vals / sum(vals)
                    return(list(ratio = norm,
                                notice = list('INFO', 'Expected proportions rescaled',
                                    sprintf("'%s' sums to %.3f, so it was rescaled to sum to 1: %s.",
                                            raw, sum(vals), paste(round(norm, 3), collapse = ", ")))))
                }
                list(ratio = vals, notice = NULL)
            },

            # "p = < 0.001" reads badly - the operator belongs to the number.
            .fmtP = function(p, html = TRUE) {
                if (!is.finite(p)) return("p = NA")
                if (p < 0.001) paste0("p ", if (html) "&lt;" else "<", " 0.001")
                else paste0("p = ", formatC(p, format = "f", digits = 3))
            },

            # Build a replacement subtitle carrying Fisher's exact test.
            #
            # ggbarstats has no exact-test option, so on a sparse 2x2 the subtitle
            # showed an uncorrected Pearson chi-squared that is not valid at those
            # expected counts - and the assumptions panel had to tell the reader to
            # disregard the number printed on the figure. A chart that contradicts
            # its own caption travels badly: it gets pasted into a slide deck
            # without the panel. So compute the exact test and put it ON the plot.
            # (Ported from R/jjpiestats.b.R, where this was first done.)
            #
            # Returns a plotmath expression in ggstatsplot's own idiom, or NULL when
            # the exact test does not apply - not 2x2, adequate expected counts,
            # paired data (McNemar is already correct) or a Bayesian analysis.
            .exactSubtitle = function(data, dep_var) {
                group_var <- self$options$group
                if (is.null(group_var) || !nzchar(group_var) || is.null(dep_var))
                    return(NULL)
                if (isTRUE(private$.option("paired")) ||
                    identical(private$.option("typestatistics"), "bayes"))
                    return(NULL)

                tryCatch({
                    tb <- private$.getWeightedTable(data, dep_var, group_var)
                    if (!identical(dim(tb), c(2L, 2L)))
                        return(NULL)
                    if (!any(suppressWarnings(chisq.test(tb)$expected) < 5))
                        return(NULL)

                    conf_level <- self$options$conflevel %||% 0.95
                    ft <- stats::fisher.test(tb, conf.level = conf_level)
                    d  <- max(0L, as.integer(self$options$digits %||% 2L))
                    fmt <- function(x) formatC(x, format = "f", digits = d)
                    p_txt <- if (ft$p.value < 0.001) "< 0.001"
                             else formatC(ft$p.value, format = "f", digits = max(3L, d))

                    # A zero cell sends the odds ratio to 0 or Inf; omit it rather
                    # than print an uninterpretable bound.
                    or <- unname(ft$estimate)
                    has_or <- is.finite(or) && all(is.finite(ft$conf.int))

                    txt <- if (has_or)
                        sprintf('list(italic("p")["Fisher"] == "%s", widehat(italic("OR")) == "%s", CI["%g%%"] ~ "[" * "%s", "%s" * "]", italic("n")["obs"] == "%d")',
                                p_txt, fmt(or), conf_level * 100,
                                fmt(ft$conf.int[1]), fmt(ft$conf.int[2]), sum(tb))
                    else
                        sprintf('list(italic("p")["Fisher"] == "%s", italic("n")["obs"] == "%d")',
                                p_txt, sum(tb))

                    parse(text = txt)[[1]]
                }, error = function(e) NULL)
            },

            # Pure decision helper: determines whether the Fisher's exact correction
            # applies for a single dependent variable's 2\u00d72 table. NO rendering
            # side-effects, so it can be called per-variable from .createBarPlot without
            # clobbering the all-variables assumptions panel.
            .computeFisherDecision = function(data, dep_var) {
                result <- list(
                    use_fisher = FALSE, fisher_reason = NULL,
                    assumption_violated = FALSE, is_2x2 = FALSE,
                    low_count_cells = 0L, total_cells = 0L, pct_low = 0
                )
                if (is.null(self$options$group) || is.null(dep_var))
                    return(result)
                if (!(self$options$group %in% names(data)) || !(dep_var %in% names(data)))
                    return(result)

                cross_table <- private$.getWeightedTable(data, dep_var, self$options$group)
                expected_counts <- tryCatch({
                    suppressWarnings(chisq.test(cross_table)$expected)
                }, error = function(e) {
                    # Default to safe values if chi-square test fails
                    default_counts <- cross_table
                    default_counts[] <- 5
                    default_counts
                })

                if (any(expected_counts < 5)) {
                    result$assumption_violated <- TRUE
                    result$low_count_cells <- sum(expected_counts < 5)
                    result$total_cells <- length(expected_counts)
                    result$pct_low <- round(100 * result$low_count_cells / result$total_cells, 1)
                    result$is_2x2 <- (result$total_cells == 4 && all(dim(cross_table) == c(2, 2)))
                    if (result$is_2x2) {
                        # Flags a 2x2 whose chi-squared is unreliable. It does NOT
                        # switch the test - ggbarstats has no exact-test option, so
                        # the caller reports the Fisher p-value alongside the plot
                        # instead of claiming a substitution that never happened.
                        result$use_fisher <- TRUE
                        result$fisher_reason <- sprintf(
                            "Chi-squared is unreliable on this 2\u00d72 table: %d of 4 cells (%.1f%%) have expected counts < 5.",
                            result$low_count_cells, result$pct_low
                        )
                    }
                }
                result
            },

            # Render the statistical-assumptions panel for ALL dependent variables.
            # Rendering-only: the Fisher decision consumed by the plot lives in
            # .computeFisherDecision(), so multi-dep analyses no longer overwrite the
            # panel with the last processed variable.
            .checkStatisticalAssumptions = function(analysis_data) {
                if (is.null(self$options$dep) || is.null(self$options$group)) {
                    self$results$assumptions$setContent("")
                    return(invisible(NULL))
                }

                warnings <- character()
                recommendations <- character()

                # Check group sizes for chi-square validity across every dependent variable
                if (self$options$group %in% names(analysis_data)) {
                    for (dep_var_check in self$options$dep) {
                        if (!(dep_var_check %in% names(analysis_data)))
                            next
                        fc <- private$.computeFisherDecision(analysis_data, dep_var_check)
                        if (isTRUE(fc$assumption_violated)) {
                            warnings <- c(warnings, paste0(
                                " <strong>Chi-square Assumption Violated (",
                                htmltools::htmlEscape(dep_var_check), "):</strong> ",
                                fc$low_count_cells, " of ", fc$total_cells, " cells (", fc$pct_low,
                                "%) have expected counts < 5."
                            ))
                            if (isTRUE(fc$is_2x2)) {
                                # NOT "using Fisher's exact test". ggbarstats has no
                                # exact-test option: measured on a 2x2 with minimum
                                # expected count 1.5, type="parametric" and
                                # type="nonparametric" both return the SAME uncorrected
                                # Pearson chi2(1)=3.20, p=0.07, while fisher.test() on
                                # that table gives p=0.118. The old text told the
                                # clinician an exact test had been substituted when the
                                # subtitle still showed an invalid chi-squared - and one
                                # that falls on the other side of 0.05. Give them the
                                # real number instead of a false reassurance.
                                fisher_p <- tryCatch(
                                    stats::fisher.test(
                                        private$.getWeightedTable(analysis_data,
                                                                  dep_var_check,
                                                                  self$options$group)
                                    )$p.value,
                                    error = function(e) NA_real_)
                                recommendations <- c(recommendations, paste0(
                                    # Keep this in step with the figure. When the
                                    # subtitle was replaced with the exact test,
                                    # telling the reader to disregard it would
                                    # recreate the contradiction being fixed.
                                    if (!is.null(private$.exactSubtitle(analysis_data, dep_var_check)))
                                        paste0(
                                            " <strong>Exact test used:</strong> ",
                                            "a chi-squared is unreliable at these expected counts, ",
                                            "so the chart subtitle reports Fisher's exact test instead.")
                                    else paste0(
                                        " <strong>Use the exact test, not the plot subtitle:</strong> ",
                                        "the chart reports an uncorrected Pearson chi-squared, which is ",
                                        "unreliable at these expected counts, and the plotting package ",
                                        "offers no exact-test option for this chart. ",
                                        if (is.finite(fisher_p))
                                            sprintf("Fisher's exact test on this table gives <strong>%s</strong>; quote that value.",
                                                    private$.fmtP(fisher_p))
                                        else
                                            "Fisher's exact test could not be computed for this table.")))
                            } else {
                                # For non-2\u00d72 tables, warn but don't auto-switch
                                recommendations <- c(recommendations,
                                    " <strong>Recommendation:</strong> Consider combining categories or using non-parametric methods. Fisher's exact test is only available for 2\u00d72 tables."
                                )
                            }
                        }
                    }
                }

                # Check for paired data appropriateness
                if (private$.option("paired")) {
                    warnings <- c(warnings,
                        " <strong>Paired Analysis:</strong> McNemar's test assumes matched pairs (e.g., before/after, case/control matching). It is computed without continuity correction, so with few discordant pairs it is anti-conservative; the notice panel flags that case."
                    )
                }

                # Generate assumptions content
                assumptions_content <- paste0(
                    "<div style='padding: 15px; background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; margin: 10px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0;'> Statistical Assumptions & Warnings</h4>",

                    "<p><strong>General Assumptions:</strong></p>",
                    "<ul>",
                    "<li>Variables are categorical or ordinal</li>",
                    "<li>Observations are independent</li>",
                    "<li>Expected cell counts \u2265 5 for chi-square validity</li>",
                    if (private$.option("paired")) "<li>Paired observations (matched subjects)</li>" else "",
                    "</ul>",

                    if (length(warnings) > 0) paste0(
                        "<p><strong>Detected Issues:</strong></p>",
                        "<ul><li>", paste(warnings, collapse = "</li><li>"), "</li></ul>"
                    ) else "",

                    if (length(recommendations) > 0) paste0(
                        "<p><strong>Recommendations:</strong></p>",
                        "<ul><li>", paste(recommendations, collapse = "</li><li>"), "</li></ul>"
                    ) else "",

                    "</div>"
                )

                self$results$assumptions$setContent(assumptions_content)
                invisible(NULL)
            },

            .generateInterpretationGuide = function(analysis_data) {
                # Cohen's cut-offs (0.1 / 0.3 / 0.5) are for df* = 1. For an r x c table
                # they divide by sqrt(min(r, c) - 1), and the subtitle's V is the
                # bias-corrected one. Compute per outcome so a 3x3 is not read on a 2x2 scale.
                dfstar <- vapply(self$options$dep, function(dv) {
                    tb <- tryCatch(private$.getWeightedTable(analysis_data, dv, self$options$group),
                                   error = function(e) NULL)
                    if (is.null(tb)) return(NA_real_)
                    max(1, min(dim(tb)) - 1)
                }, numeric(1))
                dfstar <- dfstar[!is.na(dfstar)]
                v_line <- if (length(dfstar) == 0 || all(dfstar == 1))
                    "<li><strong>Cram\u00e9r's V (bias-corrected):</strong> 0.1 (small), 0.3 (medium), 0.5 (large)</li>"
                else paste0(vapply(sort(unique(dfstar)), function(k) sprintf(
                    "<li><strong>Cram\u00e9r's V (bias-corrected), %s:</strong> %.2f (small), %.2f (medium), %.2f (large); Cohen's cut-offs divided by the square root of min(rows, columns) - 1 = %d</li>",
                    htmltools::htmlEscape(paste(names(dfstar)[dfstar == k], collapse = ", ")),
                    0.1 / sqrt(k), 0.3 / sqrt(k), 0.5 / sqrt(k), as.integer(k)), character(1)), collapse = "")

                # There is no results table in jjbarstats.r.yaml: Cramer's V and its
                # confidence interval exist only inside the ggstatsplot subtitle, and
                # `resultssubtitle` is FALSE by default - so the pointer must be
                # conditional, read through the same .option() the plot uses.
                effect_pointer <- if (isTRUE(private$.option("resultssubtitle")))
                    "Cram\u00e9r's V, reported with the test in the plot subtitle, measures how strong the association is on a scale from 0 (none) to 1 (perfect), and its confidence interval shows how loosely this sample pins that value down."
                else
                    "Cram\u00e9r's V measures how strong the association is on a scale from 0 (none) to 1 (perfect), and its confidence interval shows how loosely this sample pins that value down; switch on 'Statistical results in subtitle' to display them."

                interpretation_content <- paste0(
                    "<div style='padding: 15px; background-color: rgba(33, 163, 188, 0.21); border-left: 4px solid #17a2b8; margin: 10px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0;'> How to Interpret Results</h4>",
                    
                    "<p><strong>Statistical Significance:</strong></p>",
                    "<ul>",
                    "<li><strong>p < 0.05:</strong> Significant association between variables</li>",
                    paste0("<li><strong>p \u2265 0.05:</strong> No significant association was detected. This is an absence of evidence for an association, not evidence that the variables are independent - an association of small or moderate size may simply be undetectable at this sample size. ", effect_pointer, "</li>"),
                    "<li>When several tables are tested, use adjusted p-values: the chance of at least one false positive rises with the number of comparisons.</li>",
                    "</ul>",
                    
                    "<p><strong>Effect Size Interpretation:</strong></p>",
                    "<ul>",
                    v_line,
                    "<li><strong>Odds Ratio:</strong> >1 (positive association), <1 (negative association)</li>",
                    "</ul>",
                    
                    "<p><strong>Clinical Context:</strong></p>",
                    switch(self$options$clinicalpreset %||% "custom",
                        "diagnostic" = paste0(
                            "<ul>",
                            "<li><strong>Sensitivity:</strong> Proportion of true positives correctly identified</li>",
                            "<li><strong>Specificity:</strong> Proportion of true negatives correctly identified</li>",
                            "<li><strong>Clinical Significance:</strong> Consider both statistical significance and clinical utility</li>",
                            "</ul>"
                        ),
                        "treatment" = paste0(
                            "<ul>",
                            "<li><strong>Response Rates:</strong> Compare proportions of responders across treatments</li>",
                            "<li><strong>Proportion Tests:</strong> Within each treatment, test whether the outcome split departs from the expected proportions</li>",
                            "<li><strong>Clinical Impact:</strong> Consider magnitude of difference and number needed to treat</li>",
                            "</ul>"
                        ),
                        "biomarker" = paste0(
                            "<ul>",
                            "<li><strong>Expression Patterns:</strong> Compare distribution across clinical groups</li>",
                            "<li><strong>Prognostic Value:</strong> Association with outcomes is a starting point only; prognostic or predictive value has to be established in an independent cohort</li>",
                            "<li><strong>Validation:</strong> Consider external validation and clinical correlation</li>",
                            "</ul>"
                        ),
                        paste0(
                            "<ul>",
                            "<li><strong>Association Strength:</strong> Look at both statistical significance and effect size</li>",
                            "<li><strong>Clinical Relevance:</strong> Consider biological plausibility and clinical impact</li>",
                            "<li><strong>Further Analysis:</strong> May guide stratification or subgroup analyses</li>",
                            "</ul>"
                        )
                    ),
                    "</div>"
                )
                
                self$results$interpretation$setContent(interpretation_content)
            },

            .generateCopyReadyReport = function(analysis_data) {
                if (is.null(self$options$dep) || is.null(self$options$group)) {
                    return()
                }

                # WEIGHTED COUNTS: Use effective sample size
                n_total <- private$.getEffectiveSampleSize(analysis_data)

                # Get weighted group counts
                group_counts <- private$.getWeightedGroupCounts(analysis_data, self$options$group)
                n_groups <- length(group_counts)

                dep_vars <- htmltools::htmlEscape(paste(self$options$dep, collapse = " and "))

                # Generate template report
                report_template <- paste0(
                    "<div style='padding: 15px; background-color: rgba(138, 155, 172, 0.06); border: 1px solid #dee2e6; margin: 10px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0;'> Copy-Ready Report Template</h4>",

                    "<div style='background-color: rgba(255, 255, 255, 0.06); padding: 15px; border: 1px dashed #6c757d; margin: 10px 0; color: inherit;'>",
                    "<h5>Methods:</h5>",
                    "<p>Bar chart analysis was performed to examine the association between ", dep_vars,
                    " and ", htmltools::htmlEscape(self$options$group), " using ",
                    private$.methodDescription(analysis_data), ". ",
                    
                    if (isTRUE(private$.option("proportiontest")))
                        "Proportion tests were carried out within each group; their p-values are not adjusted for multiple testing. "
                    else "",
                    
                    "The confidence level was ", 100 * self$options$conflevel, "%. ",
                    "Analysis included ", n_total, " observations across ", n_groups, " groups.",
                    "</p>",
                    
                    "<h5>Results:</h5>",
                    "<p>[Insert specific results here: test statistic, p-value, effect size with 95% CI]</p>",
                    # The example used to assert a significant association
                    # unconditionally - wrong whenever the result was null.
                    "<p>Template (state the direction only if the test was significant): ",
                    "\"There was [a / no] statistically significant association between [variable 1] and [variable 2] ",
                    "(\u03c7\u00b2 = [value], p = [value], Cram\u00e9r's V = [value], 95% CI [lower, upper]).",
                    "\"</p>",
                    
                    "<h5>Conclusion:</h5>",
                    "<p>[Interpret findings in clinical context, considering both statistical significance and clinical relevance]</p>",
                    "</div>",
                    
                    "</div>"
                )
                
                self$results$report$setContent(report_template)
            },

            # Helper function to get effective sample size (weighted or unweighted)
            .getEffectiveSampleSize = function(data) {
                if (!is.null(self$options$counts) && self$options$counts %in% names(data)) {
                    # Weighted data: sum of counts
                    return(sum(data[[self$options$counts]], na.rm = TRUE))
                } else {
                    # Unweighted data: number of rows
                    return(nrow(data))
                }
            },

            # Helper function to create weighted contingency table
            .getWeightedTable = function(data, var1, var2) {
                if (!is.null(self$options$counts) && self$options$counts %in% names(data)) {
                    # Weighted contingency table using xtabs
                    formula_str <- paste0(jmvcore::composeTerm(self$options$counts),
                                          " ~ ", jmvcore::composeTerm(var1),
                                          " + ", jmvcore::composeTerm(var2))
                    weighted_table <- xtabs(jmvcore::asFormula(formula_str), data = data)
                    return(weighted_table)
                } else {
                    # Unweighted: regular table
                    return(table(data[[var1]], data[[var2]]))
                }
            },

            # Helper function to get weighted group counts
            .getWeightedGroupCounts = function(data, group_var) {
                if (!is.null(self$options$counts) && self$options$counts %in% names(data)) {
                    # Weighted: aggregate by group
                    counts_var <- self$options$counts
                    agg_data <- aggregate(data[[counts_var]],
                                         by = list(group = data[[group_var]]),
                                         FUN = sum, na.rm = TRUE)
                    group_counts <- setNames(agg_data$x, agg_data$group)
                    return(group_counts)
                } else {
                    # Unweighted: simple table
                    return(table(data[[group_var]], useNA = "no"))
                }
            },

            # Emit user-facing data-quality notices (small groups, zero cells, low
            # expected counts). Called unconditionally from .run() so they render in the
            # jamovi GUI and are recomputed every run rather than hidden behind the
            # validation cache.
            .emitDataQualityNotices = function(data) {
                group_var <- self$options$group
                if (is.null(group_var) || !(group_var %in% names(data)))
                    return(invisible(NULL))

                for (dep_var in self$options$dep) {
                    if (is.null(dep_var) || !(dep_var %in% names(data)))
                        next
                    cross_table <- private$.getWeightedTable(data, dep_var, group_var)
                    if (any(cross_table == 0)) {
                        private$.addNotice('WARNING', 'Zero-Count Cells', sprintf(
                            "Variable '%s' vs '%s': some cells have zero counts. Consider collapsing categories.",
                            dep_var, group_var
                        ))
                    }
                    # Expected counts matter for the chi-squared only: McNemar works on
                    # discordant pairs and the Bayesian analysis has no such assumption.
                    if (isTRUE(private$.option("paired")) ||
                        identical(private$.option("typestatistics"), "bayes"))
                        next
                    fc <- private$.computeFisherDecision(data, dep_var)
                    if (!isTRUE(fc$assumption_violated))
                        next
                    if (isTRUE(fc$is_2x2)) {
                        # One notice for a sparse 2x2, in step with the figure: the main
                        # chart's subtitle is swapped to Fisher's exact test when a subtitle
                        # is requested; split-by panels are a patchwork and keep the chi-squared.
                        fisher_p <- tryCatch(stats::fisher.test(cross_table)$p.value,
                                             error = function(e) NA_real_)
                        swapped <- isTRUE(private$.option("resultssubtitle")) &&
                            !is.null(private$.exactSubtitle(data, dep_var))
                        private$.addNotice('STRONG_WARNING', 'Chi-squared assumption violated', paste0(
                            fc$fisher_reason, " ",
                            if (swapped)
                                "The chart subtitle therefore reports Fisher's exact test instead of the chi-squared."
                            else if (is.finite(fisher_p))
                                sprintf("Fisher's exact test on this table gives %s; quote that value, not a chi-squared.",
                                        private$.fmtP(fisher_p, html = FALSE))
                            else
                                "Fisher's exact test could not be computed for this table.",
                            if (swapped && !is.null(self$options$grvar))
                                " The split-by panels keep the uncorrected chi-squared." else ""))
                    } else {
                        private$.addNotice('WARNING', 'Low Expected Counts', sprintf(
                            "Variable '%s' vs '%s': %d of %d cells (%.1f%%) have expected counts below 5, so the chi-squared p-value is unreliable. Consider collapsing categories.",
                            dep_var, group_var, fc$low_count_cells, fc$total_cells, fc$pct_low))
                    }
                }
                invisible(NULL)
            },

            # Resolve whether a paired (McNemar) analysis is valid BEFORE the summary
            # narrative is generated. An invalid paired design stops analysis.
            .resolvePairedOverride = function(data) {
                if (!isTRUE(private$.option("paired")))
                    return(invisible(NULL))

                for (dep_var in self$options$dep) {
                    if (is.null(dep_var))
                        next
                    paired_valid <- private$.validatePairedData(data, dep_var)
                    if (!paired_valid$valid) {
                        private$.addNotice('ERROR', 'Invalid Paired Data', paired_valid$message)
                        jmvcore::reject(paired_valid$message)
                    } else {
                        private$.addNotice('STRONG_WARNING', 'Paired Data Assumption', paired_valid$message)
                        # The chi-squared approximation needs enough discordant pairs; below
                        # about 25 the exact binomial test on those pairs is the valid test.
                        if (paired_valid$discordant < 25)
                            private$.addNotice('STRONG_WARNING', 'Few discordant pairs', sprintf(
                                "Only %d discordant pairs (%d vs %d) drive McNemar's test for '%s'; below 25 its chi-squared approximation (computed without continuity correction) is unreliable and anti-conservative. Use an exact binomial test on the discordant pairs.",
                                paired_valid$discordant, paired_valid$b, paired_valid$c, dep_var))
                    }
                }
                invisible(NULL)
            },

            .prepareData = function() {
                mydata <- self$data

                # ggstatsplot drops rows with a missing value in any analysed variable
                # itself; doing it here keeps every reported count equal to what is
                # tested. The number of dropped rows is reported as a notice from .run().
                private$.checkpoint(flush = FALSE)
                relevant_vars <- c(self$options$dep, self$options$group,
                                   self$options$grvar, self$options$counts)
                mydata <- mydata[complete.cases(mydata[relevant_vars]), ]

                if (nrow(mydata) == 0) {
                    jmvcore::reject(.('No complete data rows available: every row has a missing value in at least one selected variable.'))
                }

                return(mydata)
            },

            # CRITICAL SAFETY METHOD: Validate data is appropriate for paired/McNemar test
            .validatePairedData = function(data, dep_var) {
                # McNemar's test requires:
                # 1. A 2\u00d72 contingency table
                # 2. Paired/matched observations

                if (is.null(self$options$group) || is.null(dep_var)) {
                    return(list(
                        valid = FALSE,
                        message = "Paired analysis requires both dependent variable and grouping variable."
                    ))
                }

                # Check if it's a 2\u00d72 table
                cross_table <- private$.getWeightedTable(data, dep_var, self$options$group)

                if (!all(dim(cross_table) == c(2, 2))) {
                    return(list(
                        valid = FALSE,
                        message = sprintf(
                            "McNemar test requires a 2\u00d72 table. Your data has %d\u00d7%d levels. Use an analysis for paired categorical outcomes with these categories.",
                            nrow(cross_table), ncol(cross_table)
                        )
                    ))
                }

                # McNemar's statistic uses only the discordant pairs (off-diagonal cells).
                discordant <- cross_table[1, 2] + cross_table[2, 1]
                if (discordant == 0) {
                    return(list(
                        valid = FALSE,
                        message = "No discordant pairs: every pair has the same outcome under both conditions, so McNemar's test cannot be computed. Report the agreement instead."
                    ))
                }

                # Check for adequate sample size (total count)
                total_n <- sum(cross_table)
                if (total_n < 10) {
                    return(list(
                        valid = FALSE,
                        message = sprintf(
                            "Only %d paired observations are available. Use an exact binomial test on discordant pairs; do not replace a paired analysis with an independent-samples Fisher test.",
                            total_n
                        )
                    ))
                }

                # WARNING: We cannot validate actual pairing structure without a subject ID
                # This is a limitation - user must ensure data is properly paired
                return(list(
                    valid = TRUE,
                    discordant = discordant,
                    b = cross_table[1, 2], c = cross_table[2, 1],
                    message = "Data structure compatible with McNemar test. Ensure observations are actually paired (e.g., before/after, matched cases/controls). If data is independent, disable paired option."
                ))
            },

            .createBarPlot = function(data, dep_var, ggtheme, grouped = FALSE, progress_label = NULL) {
                # Progress indicator
                if (!is.null(progress_label)) {
                    private$.checkpoint()
                }

                # Data-quality notices (sparse 2x2, low expected counts, zero cells) are
                # raised once per run in .emitDataQualityNotices(), not here at render time.

                # WEIGHTED DATA HANDLING:
                # ggstatsplot::ggbarstats supports a 'counts' parameter for aggregated data.
                # When counts is specified, ggstatsplot will properly weight the statistical tests.
                # We pass the counts column directly to ggbarstats (see base_args below).
                #
                # IMPORTANT: Our summary statistics (.generateSummary, .checkStatisticalAssumptions,
                # .generateCopyReadyReport) now use weighted counts via helper functions to ensure
                # reported sample sizes match what ggstatsplot analyzes.

                # Expected proportions. The decision is computed by a pure helper so
                # .run() can render the accompanying notice - a notice raised here
                # would be written during .plot() and silently discarded.
                ratio_vec <- private$.computeRatioDecision(data, dep_var)$ratio

                # Swap in Fisher's exact test when the chi-squared on the subtitle
                # would be invalid. Only for the ungrouped chart: grouped_ggbarstats
                # returns a combined patchwork whose per-panel subtitles cannot be
                # replaced this way, so that path keeps the notice instead.
                want_subtitle <- if (!is.null(private$.option("resultssubtitle")))
                    private$.option("resultssubtitle") else TRUE
                exact_sub <- if (!grouped && isTRUE(want_subtitle))
                    private$.exactSubtitle(data, dep_var) else NULL

                # Base arguments for ggstatsplot functions with performance optimizations
                base_args <- list(
                    data = data,
                    x = rlang::sym(dep_var),
                    y = rlang::sym(self$options$group),
                    counts = if (!is.null(self$options$counts)) rlang::sym(self$options$counts) else NULL,
                    type = private$.option("typestatistics"),
                    paired = if (!is.null(private$.option("paired"))) private$.option("paired") else FALSE,
                    results.subtitle = want_subtitle && is.null(exact_sub),
                    label = if (!is.null(self$options$label)) self$options$label else "percentage",
                    digits = if (!is.null(self$options$digits)) self$options$digits else 2L,
                    digits.perc = if (!is.null(self$options$digitsperc)) self$options$digitsperc else 0L,
                    proportion.test = if (!is.null(private$.option("proportiontest"))) private$.option("proportiontest") else TRUE,
                    bf.message = if (!is.null(self$options$bfmessage)) self$options$bfmessage else FALSE,
                    conf.level = if (!is.null(self$options$conflevel)) self$options$conflevel else 0.95,
                    ratio = ratio_vec,
                    # ggstatsplot >= 0.13 wants "package::palette"; a bare name is ignored
                    # with a warning and silently falls back to ggthemes::gdoc.
                    palette = if (identical(self$options$palette, "gdoc")) "ggthemes::gdoc"
                              else paste0("RColorBrewer::", self$options$palette)
                    # NOTE: 'messages' was removed from ggbarstats/grouped_ggbarstats in
                    # recent ggstatsplot; it is no longer forwarded (was previously absorbed
                    # by ... and had no effect).
                )
                
                # Enhanced error handling with context preservation
                tryCatch({
                    if (grouped) {
                        # Add grouping variable for grouped analysis
                        base_args$grouping.var <- rlang::sym(self$options$grvar)
                        base_args$ggtheme <- private$.selectTheme(ggtheme)

                        # Checkpoint before expensive grouped_ggbarstats call
                        private$.checkpoint()
                        return(do.call(ggstatsplot::grouped_ggbarstats, base_args))
                    } else {
                        # Standard bar chart
                        # Checkpoint before expensive ggbarstats call
                        private$.checkpoint()
                        plot <- do.call(ggstatsplot::ggbarstats, base_args)
                        if (!is.null(exact_sub))
                            plot <- plot + ggplot2::labs(subtitle = exact_sub)
                        return(plot + private$.selectTheme(ggtheme))
                    }
                }, error = function(e) {
                    # Preserve original error context with enhanced information
                    original_error <- conditionMessage(e)
                    context_info <- paste0(
                        "Variable: ", dep_var, 
                        ", Groups: ", length(unique(data[[self$options$group]])),
                        ", N: ", nrow(data),
                        if (grouped) paste0(", Split by: ", self$options$grvar) else ""
                    )
                    
                    stop(paste0("Bar chart creation failed for ", progress_label %||% dep_var, 
                               ". Context: ", context_info, 
                               ". Original error: ", original_error),
                         call. = FALSE)
                })
            },

            .createMultiplePlots = function(data, dep_vars, ggtheme, grouped = FALSE) {
                # Progress indicator for multiple plots
                private$.checkpoint()
                
                # More memory-efficient symbol creation
                dep_symbols <- purrr::map(dep_vars, ~ rlang::sym(.x))
                
                # Create plots with progress tracking
                plotlist <- purrr::imap(dep_symbols, ~ {
                    progress_label <- paste0("plot ", .y, "/", length(dep_symbols), " (", dep_vars[.y], ")")
                    private$.createBarPlot(
                        data = data, 
                        dep_var = dep_vars[.y], 
                        ggtheme = ggtheme, 
                        grouped = grouped,
                        progress_label = progress_label
                    )
                })
                
                # Checkpoint before expensive plot combination
                private$.checkpoint()
                # Combine plots with improved spacing
                return(ggstatsplot::combine_plots(
                    plotlist = plotlist,
                    plotgrid.args = list(
                        ncol = 1,
                        heights = rep(1, length(plotlist))
                    ),
                    annotation.args = list(
                        tag_levels = "A"
                    )
                ))
            }

            # run ----
            ,
            .run = function() {
                # Reset per-run state so notices do not accumulate across successive
                # runs / plot resizes.
                private$.noticeList <- list()
                private$.renderNotices()

                # Always generate About content
                private$.generateAboutContent()

                # Initial Message ----
                if (is.null(self$options$dep) ||
                    is.null(self$options$group)) {
                    
                    todo <- glue::glue(
                        "<div style='padding: 15px; background-color: rgba(33, 144, 255, 0.11); border-left: 4px solid #0066cc; margin: 10px 0; color: inherit;'>",
                        "<h4 style='color: #0066cc; margin-top: 0;'> Getting Started</h4>",
                        "<p><strong>Step 1:</strong> Select your <strong>Outcome Variable</strong> (what you want to analyze)</p>",
                        "<p><strong>Step 2:</strong> Choose a <strong>Group Variable</strong> (what you want to compare)</p>",
                        "<p><strong>Step 3:</strong> Pick a <strong>Clinical Analysis Preset</strong> for scenario-specific interpretation:</p>",
                        "<ul style='margin-left: 20px;'>",
                        "<li> <strong>Diagnostic Test:</strong> 2\u00d72 tables with sensitivity/specificity</li>",
                        "<li> <strong>Treatment Response:</strong> Compare response rates across treatments</li>",
                        "<li> <strong>Biomarker Expression:</strong> Analyze expression patterns</li>",
                        "<li> <strong>Risk Factor Analysis:</strong> Examine risk factor relationships</li>",
                        "</ul>",
                        "<p><strong>Step 4:</strong> Review results and clinical interpretations</p>",
                        "<hr>",
                        "<p><small> <strong>Documentation:</strong> <a href='https://www.indrapatil.com/ggstatsplot/reference/ggbarstats.html' target='_blank'>ggbarstats</a> | ",
                        "<a href='https://www.indrapatil.com/ggstatsplot/reference/grouped_ggbarstats.html' target='_blank'>grouped_ggbarstats</a></small></p>",
                        "</div>"
                    )

                    self$results$todo$setContent(todo)
                    return()

                } else {
                    # Enhanced data validation with better error messages and caching
                    tryCatch({
                        # Basic data check
                        if (nrow(self$data) == 0) {
                            jmvcore::reject(.('Dataset is empty. Please ensure your data contains observations.'))
                        }
                        
                        # Checkpoint before data validation and preparation
                        private$.checkpoint(flush = FALSE)
                        # Use cached data validation and preparation
                        start_time <- Sys.time()
                        prepared_data <- private$.getCachedData()
                        prep_time <- round(difftime(Sys.time(), start_time, units = "secs"), 2)
                        
                        n_dropped <- nrow(self$data) - nrow(prepared_data)

                        # Checkpoint before performance calculations
                        private$.checkpoint(flush = FALSE)
                        # Performance warnings
                        perf_warning <- ""
                        if (private$.option("typestatistics") == "bayes") {
                            perf_warning <- paste0(perf_warning, 
                                                 "<br> <b>Performance Note:</b> Bayesian analysis is computationally intensive.<br>")
                        }
                        
                        # Additional info about analysis settings
                        analysis_info <- ""
                        if (private$.option("paired")) {
                            analysis_info <- paste0(analysis_info, "<br>\u2022 Using paired/repeated measures design (McNemar's test)")
                        }
                        if (!is.null(self$options$counts)) {
                            analysis_info <- paste0(analysis_info, "<br>\u2022 Using counts variable: ", htmltools::htmlEscape(self$options$counts))
                        }
                        if (!is.null(self$options$ratio) && nchar(trimws(self$options$ratio)) > 0) {
                            analysis_info <- paste0(analysis_info, "<br>\u2022 Expected proportions: ", htmltools::htmlEscape(self$options$ratio))
                        }
                        if (self$options$label != "percentage") {
                            analysis_info <- paste0(analysis_info, "<br>\u2022 Label display: ", self$options$label)
                        }

                        todo <- glue::glue(
                            "<br>Bar chart analysis comparing {htmltools::htmlEscape(paste(self$options$dep, collapse=', '))} by {htmltools::htmlEscape(self$options$group)}{if(!is.null(self$options$grvar)) paste0(', grouped by ', htmltools::htmlEscape(self$options$grvar)) else ''}.<br>
                            <br>Data prepared: {nrow(prepared_data)} observations{if (n_dropped > 0) paste0(' (', n_dropped, ' rows with missing values excluded)') else ''}.<br>
                            {analysis_info}
                            {perf_warning}
                            {if(prep_time > 0.1) paste0('<br>Preparation time: ', prep_time, ' seconds.<br>') else ''}
                            <hr>"
                        )
                        
                        self$results$todo$setContent(todo)

                        # Emit data-quality notices (visible in the GUI) and resolve the
                        # paired override BEFORE the summary narrative is generated so the
                        # reported test matches what the plot will actually run.
                        private$.emitDataQualityNotices(prepared_data)
                        private$.resolvePairedOverride(prepared_data)

                        # Rows lost to missing values were only ever message()d, which
                        # jamovi does not show; say it where the reader is.
                        if (n_dropped > 0)
                            private$.addNotice('INFO', 'Rows with missing values excluded', sprintf(
                                "%d of %d rows had a missing value in a selected variable and were excluded; %d rows were analysed.",
                                n_dropped, nrow(self$data), nrow(prepared_data)))

                        # Multiplicity. ggbarstats' per-group proportion tests are never
                        # adjusted (p.adjust.method is forwarded into contingency_table(),
                        # whose `...` ignores it), and several outcomes are several tests.
                        n_grp <- length(private$.getWeightedGroupCounts(prepared_data, self$options$group))
                        n_dep <- length(self$options$dep)
                        if (isTRUE(private$.option("proportiontest")) && n_grp * n_dep > 1)
                            private$.addNotice('INFO', 'Proportion tests are unadjusted', sprintf(
                                "The p-values above the bars come from %d separate goodness-of-fit tests (one per group%s) with no correction for multiple testing; adjust them before reporting.",
                                n_grp * n_dep, if (n_dep > 1) " and outcome" else ""))
                        if (n_dep > 1)
                            private$.addNotice('INFO', 'Several outcomes tested', sprintf(
                                "%d outcome variables were each tested against '%s'; the %d test results are not adjusted for multiple testing.",
                                n_dep, self$options$group, n_dep))

                        # Say so when the typed proportions were rescaled or dropped.
                        # Deduplicated: the same entry is checked once per dependent
                        # variable but the message is usually identical.
                        seen_ratio_msgs <- character()
                        for (dv in self$options$dep) {
                            n <- private$.computeRatioDecision(prepared_data, dv)$notice
                            if (!is.null(n) && !(n[[3]] %in% seen_ratio_msgs)) {
                                seen_ratio_msgs <- c(seen_ratio_msgs, n[[3]])
                                private$.addNotice(n[[1]], n[[2]], n[[3]])
                            }
                        }

                        # Generate clinical interpretation panels.
                        # Each panel's visibility is governed by its own toggle in .r.yaml
                        # (showSummary / showAssumptions / showInterpretation); generate the
                        # content whenever that toggle is on so a visible panel is never left
                        # blank. showexplanations acts as a master switch enabling all panels.
                        if (isTRUE(self$options$showexplanations) || isTRUE(self$options$showSummary))
                            private$.generateSummary(prepared_data)
                        if (isTRUE(self$options$showexplanations) || isTRUE(self$options$showAssumptions))
                            private$.checkStatisticalAssumptions(prepared_data)
                        if (isTRUE(self$options$showexplanations) || isTRUE(self$options$showInterpretation))
                            private$.generateInterpretationGuide(prepared_data)
                        if (isTRUE(self$options$showexplanations))
                            private$.generateCopyReadyReport(prepared_data)

                        # ENHANCEMENT 3: Clinical prevalence warning for diagnostic preset
                        if (self$options$clinicalpreset == "diagnostic" &&
                            !is.null(self$options$dep) &&
                            length(self$options$dep) > 0) {

                            # Check first dependent variable for 2-level disease status
                            tryCatch({
                                dep_table <- table(prepared_data[[self$options$dep[1]]])

                                if (length(dep_table) == 2) {
                                    # Calculate prevalence (proportion of less common outcome)
                                    prevalence <- min(dep_table) / sum(dep_table)

                                    # Warn about extreme prevalence (< 5% or > 95%)
                                    if (prevalence < 0.05 || prevalence > 0.95) {
                                        private$.addNotice('STRONG_WARNING', 'Extreme Disease Prevalence', sprintf(
                                            "Extreme disease prevalence detected (%.1f%%). Positive/negative predictive values are highly prevalence-dependent and may not generalize to populations with different baseline risk. Consider reporting likelihood ratios or conducting sensitivity analysis across prevalence ranges.",
                                            prevalence * 100
                                        ))
                                    }
                                }
                            }, error = function(e) {
                                # Silently fail if prevalence check encounters issues
                                # (e.g., non-standard data structure)
                            })
                        }

                    }, error = function(e) {
                        # Reset cache on error
                        private$.cached_data <- NULL
                        private$.validation_passed <- FALSE
                        
                        # Enhanced error reporting with more context
                        error_context <- ""
                        if (grepl("continuous", e$message, ignore.case = TRUE)) {
                            error_context <- "<br> <b>Tip:</b> Use Data > Transform to create categorical groups from continuous variables.<br>"
                        } else if (grepl("minimum|group size", e$message, ignore.case = TRUE)) {
                            error_context <- "<br> <b>Tip:</b> Consider combining small categories or collecting more data.<br>"
                        } else if (grepl("variation|level", e$message, ignore.case = TRUE)) {
                            error_context <- "<br> <b>Tip:</b> Ensure your variables have multiple categories for comparison.<br>"
                        }
                        
                        error_msg <- glue::glue(
                            "<br> <b>Error in Bar Chart Analysis:</b><br>
                            <br>{htmltools::htmlEscape(e$message)}<br>
                            {error_context}
                            <br><b>General Troubleshooting:</b><br>
                            \u2022 Ensure dependent and grouping variables are categorical<br>
                            \u2022 Check that selected variables exist in your dataset<br>
                            \u2022 Verify sufficient sample sizes in each group (\u22655 recommended)<br>
                            \u2022 Confirm variables have adequate variation (\u22652 categories)<br><hr>"
                        )
                        self$results$todo$setContent(error_msg)
                        stop(e)
                    })
                    
                    # Add checkpoint for user feedback
                    private$.checkpoint()
                }
            }

            ,
            .plot = function(image, ggtheme, theme, ...) {
                # Validation ----
                if (is.null(self$options$dep) || is.null(self$options$group))
                    return()

                # Checkpoint before data retrieval
                private$.checkpoint(flush = FALSE)
                # Use cached data for performance
                tryCatch({
                    mydata <- private$.getCachedData()
                }, error = function(e) {
                    stop(paste("Plot preparation failed:", e$message))
                })

                dep <- self$options$dep

                # Single vs Multiple dependent variables using shared logic
                if (length(dep) == 1) {
                    plot <- private$.createBarPlot(
                        data = mydata, 
                        dep_var = dep, 
                        ggtheme = ggtheme, 
                        grouped = FALSE,
                        progress_label = "main plot"
                    )
                } else {
                    plot <- private$.createMultiplePlots(
                        data = mydata, 
                        dep_vars = dep, 
                        ggtheme = ggtheme, 
                        grouped = FALSE
                    )
                }

                # Print Plot ----
                print(plot)
                TRUE
            }


            ,

            .plot2 = function(image, ggtheme, theme, ...) {
                # Validation ----
                if (is.null(self$options$dep) || is.null(self$options$group) || is.null(self$options$grvar))
                    return()

                # Checkpoint before data retrieval
                private$.checkpoint(flush = FALSE)
                # Use cached data for performance  
                tryCatch({
                    mydata <- private$.getCachedData()
                }, error = function(e) {
                    stop(paste("Grouped plot preparation failed:", e$message))
                })

                dep <- self$options$dep

                # Single vs Multiple dependent variables using shared logic (grouped)
                if (length(dep) == 1) {
                    plot2 <- private$.createBarPlot(
                        data = mydata, 
                        dep_var = dep, 
                        ggtheme = ggtheme, 
                        grouped = TRUE,
                        progress_label = "grouped plot"
                    )
                } else {
                    plot2 <- private$.createMultiplePlots(
                        data = mydata, 
                        dep_vars = dep, 
                        ggtheme = ggtheme, 
                        grouped = TRUE
                    )
                }

                # Print Plot ----
                print(plot2)
                TRUE
            }

            ,
            .plotBalloon = function(image, ...) {
                if (!self$options$addGGPubrBalloon)
                    return()

                if (is.null(self$options$dep) || is.null(self$options$group))
                    return()

                # Use validated/cached data so the balloon plot respects the same
                # excl / NA-handling and validation applied to the main plots.
                tryCatch({
                    mydata <- private$.getCachedData()
                }, error = function(e) {
                    stop(paste("Balloon plot preparation failed:", e$message))
                })
                dep <- self$options$dep
                group <- self$options$group

                # BUG FIX: Handle multiple dependent variables
                # Balloon plot only works with single dependent variable
                if (length(dep) > 1) {
                    # Use only the first dependent variable and notify user
                    dep_first <- dep[1]

                    private$.addNotice('INFO', 'Balloon Plot Single Variable', sprintf(
                        "Balloon plot created for first variable only (%s). Multiple dependent variables not supported for balloon plots.",
                        dep_first
                    ))

                    dep <- dep_first
                }

                # Create contingency table
                if (!is.null(self$options$counts) && self$options$counts %in% names(mydata)) {
                    formula_str <- paste0(jmvcore::composeTerm(self$options$counts),
                                          " ~ ", jmvcore::composeTerm(dep),
                                          " + ", jmvcore::composeTerm(group))
                    cont_table <- xtabs(jmvcore::asFormula(formula_str), data = mydata)
                } else {
                    cont_table <- table(mydata[[dep]], mydata[[group]])
                }

                # Convert to data frame for ggballoonplot
                cont_df <- as.data.frame(cont_table)
                names(cont_df) <- c("Row", "Column", "Freq")

                # Create balloon plot
                plot <- ggpubr::ggballoonplot(
                    cont_df,
                    x = "Column",
                    y = "Row",
                    size = "Freq",
                    fill = "Freq",
                    ggtheme = ggpubr::theme_pubr()
                )

                # Add gradient color based on palette
                if (self$options$ggpubrBalloonPalette == "jco") {
                    plot <- plot + ggplot2::scale_fill_gradient(low = "#FFFFFF", high = "#0073C2FF")
                } else if (self$options$ggpubrBalloonPalette == "lancet") {
                    plot <- plot + ggplot2::scale_fill_gradient(low = "#FFFFFF", high = "#00468BFF")
                } else {
                    plot <- plot + ggplot2::scale_fill_gradient(low = "#FFFFFF", high = "#999999")
                }

                print(plot)
                TRUE
            }

        ), # End of private list
        public = list(
            #' @description
            #' Generate R source code for Bar Chart Statistics analysis
            #' @return Character string with R syntax for reproducible analysis
            asSource = function() {
                dep <- self$options$dep
                group <- self$options$group

                if (is.null(dep) || is.null(group))
                    return('')

                # Build the argument list in option-declaration order.
                #
                # Every variable-name option (single OptionVariable or multi-variable
                # OptionVariables) is emitted as a deparse()'d string literal. deparse()
                # produces valid, fully-escaped R for names containing spaces, quotes or
                # backslashes (e.g. `Tumor Grade`); jmvcore's default sourcify would emit
                # some of these as bare, unquoted symbols and yield invalid syntax.
                # Detecting the option by CLASS (not by name) means any variable option
                # added later is escaped automatically.
                #
                # Variables are NOT re-emitted through private$.asArgs() - doing so
                # previously duplicated them in the generated syntax (the "double
                # variables" bug). All non-variable options keep jmvcore's per-option
                # sourcify so formatting stays consistent with jamovi.
                args <- character(0)
                for (option in private$.options$options) {
                    if (option$name == 'data')
                        next
                    if (inherits(option, 'OptionVariable') || inherits(option, 'OptionVariables')) {
                        val <- option$value
                        if (!is.null(val) && length(val) > 0)
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
                paste0(pkg_name, '::jjbarstats(\n    data = data,\n    ',
                       paste(args, collapse = ',\n    '), ')')
            }
        ) # End of public list
    )
