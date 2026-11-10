#' @title Pie Charts
#' @importFrom R6 R6Class
#' @importFrom jmvcore .
#' @importFrom digest digest
#' @importFrom rlang sym
#' @importFrom glue glue
#'
#' @return An \code{R6} class generator object for the \code{jjpiestatsClass} backend; used internally by the jamovi analysis wrapper and not called directly.

jjpiestatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjpiestatsClass",
    inherit = jjpiestatsBase,
    private = list(

        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .data_hash = NULL,
        .validation_passed = FALSE,
        .plotTheme = NULL,
        .droppedInfo = NULL,

        # Notice collection helpers. A single Preformatted (plain-text) output item
        # keeps all notices in plain text with no HTML markup (project convention). ====
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            # De-duplicate: render functions may re-run without .run() (e.g. on a
            # plot resize), which would otherwise append the same notice again.
            already <- vapply(private$.noticeList, function(n) {
                identical(n$type, type) &&
                    identical(n$title, title) &&
                    identical(n$content, content)
            }, logical(1))
            if (!any(already)) {
                private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                    type = type,
                    title = title,
                    content = content
                )
            }
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
                    STRONG_WARNING = "WARNING: ",
                    WARNING        = "WARNING: ",
                    "")
                paste0(prefix, notice$title, "\n", notice$content)
            }, character(1))

            self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
        },

        # init ----

        .init = function() {

            self$results$plot1$setSize(650, 450)

            self$results$plot2$setSize(650, 450)


            if (!is.null(self$options$grvar) && nzchar(self$options$grvar)) {
                if (is.null(self$options$group) || !nzchar(self$options$group)) {
                    if (!is.null(self$options$dep) && nzchar(self$options$dep)) {
                        self$results$plot4$setTitle(
                            paste0(self$options$dep, " by ", self$options$grvar)
                        )
                    }
                }
            }

            if (!is.null(self$options$grvar) && nzchar(self$options$grvar)) {

                mydata <- self$data
                grvar <- self$options$grvar

                if (!is.null(mydata) && grvar %in% names(mydata)) {
                    num_levels <- max(1L, nlevels(as.factor(mydata[[grvar]])))
                    group <- self$options$group

                    if (!is.null(group) && nzchar(group) && group %in% names(mydata)) {
                        num_levels_group <- max(1L, nlevels(as.factor(mydata[[group]])))
                        self$results$plot4$setSize((num_levels + num_levels_group) * 600, 450)
                    } else {
                        self$results$plot4$setSize(num_levels * 600, 450)
                    }
                }

            }

        },

        # Clinical Enhancement Methods ----
        
        .effectiveOptionsList = function() {
            # Build a derived options list without mutating self$options
            valid_types <- c("parametric", "nonparametric", "robust", "bayes")
            typestatistics <- if (isTRUE(self$options$typestatistics %in% valid_types)) {
                self$options$typestatistics
            } else {
                "parametric"
            }

            valid_labels <- c("percentage", "counts", "both")
            label <- if (isTRUE(self$options$label %in% valid_labels)) {
                self$options$label
            } else {
                "percentage"
            }

            valid_presets <- c("custom", "diagnostic", "treatment", "biomarker")
            clinicalpreset <- if (isTRUE(self$options$clinicalpreset %in% valid_presets)) {
                self$options$clinicalpreset
            } else {
                "custom"
            }

            valid_palettes <- c("jco", "npg", "aaas", "lancet", "jama", "nejm")
            ggpubrDonutPalette <- if (isTRUE(self$options$ggpubrDonutPalette %in% valid_palettes)) {
                self$options$ggpubrDonutPalette
            } else {
                "jco"
            }

            digits <- suppressWarnings(as.integer(self$options$digits))
            if (length(digits) != 1L || is.na(digits) || digits < 0L || digits > 5L) {
                digits <- 2L
            }

            conflevel <- suppressWarnings(as.numeric(self$options$conflevel))
            if (length(conflevel) != 1L || is.na(conflevel) || conflevel < 0.50 || conflevel > 0.99) {
                conflevel <- 0.95
            }

            opts <- list(
                dep = self$options$dep,
                group = self$options$group,
                grvar = self$options$grvar,
                typestatistics = typestatistics,
                counts = self$options$counts,
                raw_ratio = self$options$ratio,
                paired = isTRUE(self$options$paired),
                label = label,
                digits = digits,
                conflevel = conflevel,
                proportiontest = isTRUE(self$options$proportiontest),
                bfmessage = isTRUE(self$options$bfmessage),
                messages = isTRUE(self$options$messages),
                resultssubtitle = isTRUE(self$options$resultssubtitle),
                originaltheme = isTRUE(self$options$originaltheme),
                clinicalpreset = clinicalpreset,
                ggpubrDonutPalette = ggpubrDonutPalette
            )

            preset <- opts$clinicalpreset
            if (!is.null(preset) && preset != "custom") {
                if (preset == "diagnostic") {
                    opts$resultssubtitle <- TRUE
                    opts$proportiontest <- TRUE
                } else if (preset == "treatment") {
                    opts$typestatistics <- "parametric"
                    opts$resultssubtitle <- TRUE
                    opts$proportiontest <- TRUE
                } else if (preset == "biomarker") {
                    opts$typestatistics <- "robust"
                    opts$resultssubtitle <- TRUE
                    opts$label <- "both"
                }
            }

            # Parse the ratio once. .parseRatio() coerces with as.numeric(), which
            # warns on non-numeric input; jamovi shows raw R warnings in the
            # undifferentiated Analysis Notes panel, so muffle them here. The
            # user-facing complaint is raised properly from .emitRunNotices(),
            # which re-derives the reason and states it in a notice.
            #
            # Assign through `[` rather than `$`: .parseRatio() returns NULL for an
            # empty or invalid ratio, and `opts$ratio <- NULL` would DELETE the
            # element, after which `opts$ratio` partial-matches the sibling
            # `raw_ratio` and yields "" instead of NULL.
            opts["ratio"] <- list(withCallingHandlers(
                private$.parseRatio(opts$raw_ratio),
                warning = function(w) invokeRestart("muffleWarning")
            ))

            opts
        },
        
        .getPlotTheme = function(ggtheme = NULL) {
            # Hash-based invalidation - without this, toggling `originaltheme`
            # between .run() invocations on the same R6 instance returns a stale theme.
            current_originaltheme <- isTRUE(self$options$originaltheme)
            if (!current_originaltheme && !is.null(ggtheme)) {
                return(ggtheme)
            }
            if (is.null(private$.plotTheme) ||
                !identical(attr(private$.plotTheme, "originaltheme"), current_originaltheme)) {
                private$.plotTheme <- if (!current_originaltheme) {
                    ggplot2::theme_bw()
                } else {
                    ggstatsplot::theme_ggstatsplot()
                }
                attr(private$.plotTheme, "originaltheme") <- current_originaltheme
            }
            private$.plotTheme
        },
        
        .generateClinicalPanels = function(prepared_data = NULL) {
            # 'about' is always visible (r.yaml visible: true), so populate it
            # every run rather than only when 'showexplanations' is on. The old
            # early return here left the always-visible 'about' and 'report'
            # (visible: (dep)) panels empty by default, and also made the granular
            # showSummary/showAssumptions/showInterpretation toggles render empty
            # panels whenever 'showexplanations' was off. 'showexplanations' now
            # acts as a master switch that also reveals the granular panels.
            show_expl <- isTRUE(self$options$showexplanations)
            preset_active <- !identical(self$options$clinicalpreset %||% "custom", "custom")
            panel_data <- prepared_data %||% self$data

            # Generate About content
            about_content <- private$.generateAboutContent()
            self$results$about$setContent(about_content)

            has_dep <- !is.null(self$options$dep)

            # Placeholder shown when a toggle is on but the outcome variable is
            # not yet selected, so an enabled panel never renders blank-but-titled.
            placeholder <- glue::glue(
                "<p><em>{msg}</em></p><hr>",
                msg = .('Select a dependent (outcome) variable to populate this panel.')
            )

            # Summary (needs the outcome variable)
            if (show_expl || isTRUE(self$options$showSummary)) {
                self$results$summary$setContent(
                    if (has_dep) private$.generateSummaryContent() else placeholder
                )
            }

            # Interpretation guidance
            if (show_expl || isTRUE(self$options$showInterpretation) || preset_active) {
                self$results$interpretation$setContent(
                    if (has_dep) private$.generateInterpretationContent() else placeholder
                )
            }

            # Assumptions & warnings (works with or without a grouping variable;
            # the contingency-table checks are simply skipped when no group is set)
            if (show_expl || isTRUE(self$options$showAssumptions) || preset_active) {
                self$results$assumptions$setContent(
                    if (has_dep) private$.generateAssumptionsContent(panel_data) else placeholder
                )
            }

            # Copy-ready report (r.yaml gates visibility on dep, so only populate
            # when the outcome variable is present)
            if (has_dep) {
                self$results$report$setContent(private$.generateReportContent(panel_data))
            }
        },
        
        .generateAboutContent = function() {
            preset_info <- switch(self$options$clinicalpreset %||% "custom",
                "diagnostic" = .('This analysis is configured for diagnostic test evaluation using 2\u00d72 contingency table association testing and within-group proportion tests (place the reference standard in Grouping Variable and the index test in Dependent Variable so within-group slice proportions represent sensitivity and specificity).'),
                "treatment" = .('This analysis is optimized for treatment response comparison across multiple treatment groups.'), 
                "biomarker" = .('This analysis is configured for biomarker distribution analysis across groups, displaying both counts and percentages with contingency table association testing.'),
                .('This analysis uses custom settings for pie chart generation with statistical testing.')
            )
            
            glue::glue(
                "<h3>{title}</h3>
                <p><strong>{what}:</strong> {description}</p>
                <p><strong>{when}:</strong> {usage}</p>
                <p><strong>{preset_label}:</strong> {preset_info}</p>
                <p><strong>{output_label}:</strong> {outputs}</p>
                <hr>",
                title = .('Pie Chart Analysis'),
                what = .('What this analysis does'),
                description = .('Generates pie charts with statistical analysis to compare categorical variables across groups. Performs chi-square tests, Fisher\'s exact tests, or other appropriate statistical tests based on your data.'),
                when = .('When to use'),
                usage = .('Use when you want to visualize proportions of categorical outcomes and test for significant differences between groups. Ideal for diagnostic test results, treatment responses, or biomarker categories.'),
                preset_label = .('Current configuration'),
                output_label = .('What you\'ll get'),
                outputs = .('Interactive pie charts with statistical test results, confidence intervals, and effect sizes. Optional grouped analysis for complex study designs.')
            )
        },
        
        .generateSummaryContent = function() {
            # Basic summary based on current selections
            dep_info <- if (!is.null(self$options$dep)) {
                paste(.('Analyzing variable:'), htmltools::htmlEscape(self$options$dep))
            } else { .('No outcome variable selected') }

            group_info <- if (!is.null(self$options$group) && self$options$group != "") {
                paste(.('Comparing across groups:'), htmltools::htmlEscape(self$options$group))
            } else { .('No grouping variable - single pie chart') }
            
            method_info <- paste(.('Statistical method:'), if (private$.isMcNemar()) 'McNemar' else tools::toTitleCase(private$.effectiveOptionsList()$typestatistics))
            
            glue::glue(
                "<h4>{summary_title}</h4>
                <p>\u2022 {dep_info}</p>
                <p>\u2022 {group_info}</p>
                <p>\u2022 {method_info}</p>
                <p>\u2022 {sample_info}</p>
                <hr>",
                summary_title = .('Analysis Configuration'),
                sample_info = paste(.('Sample size:'), nrow(self$data), .('input rows (before exclusions; frequencies may differ)'))
            )
        },
        
        .generateAssumptionsContent = function(data = NULL) {
            df <- data %||% self$data
            opts <- private$.effectiveOptionsList()
            if (private$.isMcNemar()) {
                return(paste0("<p>",
                    .('McNemar analysis requires paired categorical observations with matching category coding. Pairs must be independent of one another. An independent-samples Fisher test is not a replacement.'),
                    " ",
                    .('The McNemar chi-square reported by the plotting package is computed without a continuity correction, so it will not match the base R default.'),
                    " ",
                    .('This asymptotic test uses only the discordant pairs. Fagerland et al. (2013) recommend it when a type I error rate slightly above the nominal level (at most 5.37% at 5%) is acceptable, and advise against the exact binomial and continuity-corrected versions in any situation, as both are overly conservative. Few discordant pairs limit its power.'),
                    "</p>"))
            }
            if (identical(opts$typestatistics, "bayes"))
                return("<p>Bayesian contingency analysis assumes independent observations and valid case frequencies. Interpret the Bayes factor with its prior; Pearson expected-count diagnostics do not determine Bayesian validity.</p>")
            warnings_list <- c()

            # Check for small sample sizes
            if (!is.null(self$options$group) && self$options$group %in% names(df)) {
                tryCatch({
                    # CRITICAL FIX #4: Use weighted contingency table if counts variable present
                    counts_var <- self$options$counts
                    if (!is.null(counts_var) && counts_var != "" && counts_var %in% names(df)) {
                        sub_df <- df[!is.na(df[[counts_var]]) & df[[counts_var]] > 0, , drop = FALSE]
                        sub_df[[self$options$dep]] <- droplevels(as.factor(sub_df[[self$options$dep]]))
                        sub_df[[self$options$group]] <- droplevels(as.factor(sub_df[[self$options$group]]))
                        # Weighted contingency table using xtabs
                        formula_str <- paste0(jmvcore::composeTerm(counts_var),
                                              " ~ ", jmvcore::composeTerm(self$options$dep),
                                              " + ", jmvcore::composeTerm(self$options$group))
                        contingency_table <- xtabs(jmvcore::asFormula(formula_str), data = sub_df)
                    } else {
                        # Unweighted contingency table
                        contingency_table <- table(
                            droplevels(as.factor(df[[self$options$dep]])),
                            droplevels(as.factor(df[[self$options$group]]))
                        )
                    }
                    expected_counts <- suppressWarnings(chisq.test(contingency_table)$expected)
                    
                    if (any(expected_counts < 5)) {
                        # Do NOT tell the user to pick "nonparametric" to obtain a
                        # Fisher test - that option returns the identical Pearson
                        # chi-square (measured: chi2(1) = 4.47, p = 0.03 for
                        # parametric, nonparametric and robust alike, while
                        # fisher.test() on that table gives p = 0.0511, i.e. the
                        # other side of 0.05). Compute the exact test here and hand
                        # over the real number.
                        n_low <- sum(expected_counts < 5)

                        # Whether the chart subtitle was replaced with the exact
                        # test depends on the same conditions .exactSubtitle()
                        # checks. Keep this text in step with what is actually on
                        # the figure - a panel that contradicts the chart is the
                        # defect this whole fix is about.
                        swapped <- isTRUE(opts$resultssubtitle) && !is.null(private$.exactSubtitle(
                            df, self$options$dep, self$options$group,
                            self$options$counts, opts$conflevel, opts$digits))

                        # fisher.test() enumerates tables and can hang on a large
                        # sparse one; cap it and say so rather than freeze jamovi.
                        fisher_p <- if (prod(dim(contingency_table)) <= 30)
                            tryCatch(stats::fisher.test(contingency_table)$p.value,
                                     error = function(e) NA_real_)
                        else NA_real_

                        warnings_list <- c(warnings_list, paste0(
                            "",
                            sprintf(.('Chi-square is unreliable here: %d of %d expected cell counts are below 5.'),
                                    n_low, length(expected_counts)),
                            " ",
                            if (swapped)
                                .('The chart subtitle therefore reports Fisher\'s exact test instead of the chi-square.')
                            else if (!isTRUE(opts$resultssubtitle))
                                paste0("Statistical subtitles are disabled. Fisher's exact test on this table: ",
                                    if (is.finite(fisher_p)) private$.fmtP(fisher_p) else "not computed", ".")
                            else if (is.finite(fisher_p))
                                sprintf(.('The chart subtitle still reports an uncorrected Pearson chi-square - no statistic option changes that, as the plotting package offers no exact test. Fisher\'s exact test on this table gives <b>%s</b>; quote that value, not the subtitle.'),
                                        private$.fmtP(fisher_p))
                            else
                                .('The chart subtitle still reports an uncorrected Pearson chi-square. An exact test was not computed for a table of this size; consider collapsing sparse categories.')))
                    }
                    
                    if (any(contingency_table < 2)) {
                        warnings_list <- c(warnings_list,
                            paste("", .('Some categories have very few observations. Consider combining categories or collecting more data.'))
                        )
                    }

                    grvar <- self$options$grvar
                    if (!is.null(grvar) && nzchar(grvar) && grvar %in% names(df)) {
                        sparse_strata <- character()
                        for (lv in levels(droplevels(as.factor(df[[grvar]])))) {
                            sub_g <- df[!is.na(df[[grvar]]) & df[[grvar]] == lv, , drop = FALSE]
                            if (nrow(sub_g) == 0) next
                            tb_g <- if (!is.null(counts_var) && counts_var != "" && counts_var %in% names(sub_g)) {
                                sub_g <- sub_g[!is.na(sub_g[[counts_var]]) & sub_g[[counts_var]] > 0, , drop = FALSE]
                                sub_g[[self$options$dep]] <- droplevels(as.factor(sub_g[[self$options$dep]]))
                                sub_g[[self$options$group]] <- droplevels(as.factor(sub_g[[self$options$group]]))
                                f_g <- paste0(jmvcore::composeTerm(counts_var), " ~ ",
                                              jmvcore::composeTerm(self$options$dep), " + ",
                                              jmvcore::composeTerm(self$options$group))
                                stats::xtabs(jmvcore::asFormula(f_g), data = sub_g)
                            } else {
                                table(droplevels(as.factor(sub_g[[self$options$dep]])),
                                      droplevels(as.factor(sub_g[[self$options$group]])))
                            }
                            if (nrow(tb_g) >= 2L && ncol(tb_g) >= 2L) {
                                exp_g <- suppressWarnings(stats::chisq.test(tb_g)$expected)
                                if (any(exp_g < 5)) {
                                    sparse_strata <- c(sparse_strata, lv)
                                }
                            }
                        }
                        if (length(sparse_strata) > 0) {
                            warnings_list <- c(warnings_list,
                                sprintf(.('In Split By (%s), expected cell counts are below 5 in: %s. Check the notices panel for exact p-values per stratum.'),
                                        htmltools::htmlEscape(grvar),
                                        htmltools::htmlEscape(paste(sparse_strata, collapse = ", ")))
                            )
                        }
                    }
                }, error = function(e) {
                    # Silently continue if table creation fails
                })
            }
            
            # General assumptions
            assumptions <- c(
                paste("", .('Data should be categorical (factors or characters)')),
                paste("", .('Observations should be independent')),
                paste("", .('For statistical tests: adequate sample size in each category'))
            )
            
            warnings_section <- if (length(warnings_list) > 0) {
                paste0("<h5>", .('Warnings'), "</h5>\n<ul>\n",
                      paste0("<li>", warnings_list, "</li>", collapse = "\n"),
                      "\n</ul>\n")
            } else {
                paste0("<p> ", .('No issue was detected by these limited checks; study design and independence still require verification.'), "</p>\n")
            }
            
            glue::glue(
                "<h4>{assumptions_title}</h4>
                <h5>{general_title}</h5>
                <ul>
                {paste0('<li>', assumptions, '</li>', collapse = '\n')}
                </ul>
                {warnings_section}
                <hr>",
                assumptions_title = .('Statistical Assumptions & Warnings'),
                general_title = .('General Requirements')
            )
        },
        
        .generateInterpretationContent = function() {
            # Paired runs show McNemar's test, whatever the frequentist type: the
            # chi-square, expected-count and Fisher guidance below does not apply.
            method_guidance <- if (private$.isMcNemar())
                .('McNemar\'s test compares the pairs that changed in one direction with those that changed in the other. p < 0.05 indicates that the proportion differs between the paired measurements. The expected-count rule does not apply, and no exact p-value replaces the result on the chart.')
            else switch(private$.effectiveOptionsList()$typestatistics,
                "parametric" = .('Chi-square test results show whether group differences are statistically significant. Look for p-values < 0.05 for significant associations. Example: \u03c7\u00b2(1) = 5.2, p = 0.023 indicates significant difference between groups.'),
                # For a contingency table, ggstatsplot computes the SAME Pearson
                # chi-square for parametric, nonparametric and robust - it offers no
                # exact test. Telling the user these buttons change the method sent
                # them to a "Fisher's exact test" that never ran.
                "nonparametric" = .('For contingency tables this option computes the same Pearson chi-square as Parametric - the plotting package provides no exact test. If expected cell counts are below 5, use the exact p-value reported in the Assumptions panel instead of the chart subtitle.'),
                "robust" = .('For contingency tables this option computes the same Pearson chi-square as Parametric. Robust estimation applies to continuous outcomes, not to counts.'),
                "bayes" = .('The chart subtitle reports log_e(BF01), the natural logarithm of the Bayes factor in favor of the null hypothesis (BF01 = 1 / BF10). Negative values favor the alternative hypothesis of a difference (e.g., log_e(BF01) < -1.10 corresponds to BF10 > 3, moderate evidence for a difference; < -2.30 corresponds to BF10 > 10, strong evidence), whereas positive values > 1.10 (BF01 > 3) favor the null hypothesis of no difference.'),
                .('Statistical analysis will be performed based on your data characteristics.')
            )

            clinical_context <- switch(self$options$clinicalpreset %||% "custom",
                "diagnostic" = .('For diagnostic tests: Focus on sensitivity (true positive rate) and specificity (true negative rate). Example: Sensitivity = 85% means test correctly identifies 85 out of 100 diseased patients. Specificity = 92% means test correctly identifies 92 out of 100 healthy individuals. PPV and NPV depend on disease prevalence in your population.'),
                "treatment" = .('For treatment response: Look for significant differences between treatment arms. Example: 45% response in Treatment A vs 25% in Treatment B (p = 0.012, Cram\u00e9r\'s V = 0.31) indicates a moderate effect size. Whether a difference of this magnitude matters for patients is a judgement about the outcome, not a property of the statistic; on the count scale the same contrast is an absolute risk difference of 20 percentage points, i.e. a number needed to treat of 1/0.20 = 5.'),
                "biomarker" = .('For biomarker analysis: Examine distribution patterns across patient groups. Example: High biomarker expression in 65% of responders vs 30% in non-responders (p < 0.001, OR = 4.3, 95% CI 2.1 to 8.7) describes an association between expression and response in this sample. A cross-sectional association does not establish predictive value; that requires evaluation in an independent cohort.'),
                .('Interpret results in the context of your specific research question and clinical setting. Focus on effect sizes (Cram\u00e9r\'s V, odds ratios) alongside p-values for clinical significance assessment.')
            )

            # Effect size interpretation guide.
            #
            # The chart subtitle reports the BIAS-CORRECTED Cramer's V (Bergsma
            # 2013) for two-way tables, and Pearson's contingency coefficient (C)
            # for one-way goodness-of-fit tests.
            has_group <- !is.null(self$options$group) && nzchar(self$options$group)
            one_way_guide <- if (!has_group) {
                paste0(
                    "<p><strong>", .('Effect Size Interpretation (One-Way Goodness-of-Fit - Pearson\'s C):'), "</strong> ",
                    .('For a single-variable pie chart, the subtitle reports Pearson\'s contingency coefficient (Pearson\'s C), which measures departure from expected proportions on a scale from 0 (exact match to expected proportions) toward 1 (larger departure).'),
                    "</p>"
                )
            } else ""

            effect_size_guide <- paste0(
                one_way_guide,
                "<p><strong>", .('Effect Size Interpretation (Cram\u00e9r\'s V):'), "</strong></p>",
                "<ul>",
                "<li>", .('Small effect: V = 0.10 (subtle association, may require large samples to detect)'), "</li>",
                "<li>", .('Medium effect: V = 0.30 (noticeable association)'), "</li>",
                "<li>", .('Large effect: V = 0.50 (strong association)'), "</li>",
                "</ul>",
                "<p><em>", .('Note: the chart subtitle reports the bias-corrected Cram\u00e9r\'s V (Bergsma 2013), which is smaller than the classical statistic these landmarks describe - especially in small samples and larger tables. Treat the thresholds as approximate, and read the confidence interval next to V rather than the point estimate alone.'), "</em></p>"
            )

            glue::glue(
                "<h4>{interpretation_title}</h4>
                <p><strong>{method_title}:</strong> {method_guidance}</p>
                <p><strong>{clinical_title}:</strong> {clinical_context}</p>
                {effect_size_guide}
                <p><strong>{general_title}:</strong> {general_guidance}</p>
                <hr>",
                interpretation_title = .('How to Interpret Your Results'),
                method_title = .('Statistical Method'),
                clinical_title = .('Clinical Context'),
                general_title = .('General Guidance'),
                general_guidance = .('Pie charts show proportions visually - larger slices represent higher frequencies. A significance test reports how compatible the observed data are with the hypothesis of no group difference: a small p-value means data this extreme would be unusual if there were no difference. It is not the probability that the difference is due to chance, nor the probability that the null hypothesis is true, and a large p-value does not establish that the groups are the same. Report both the p-value and the effect size (Cram\u00e9r\'s V, odds ratios) for complete interpretation.')
            )
        },
        
        .generateReportContent = function(data = NULL) {
            df <- data %||% self$data
            # This sentence is offered as Methods text to paste into a manuscript,
            # so it must name the test that actually ran. It did not: measured on a
            # 2x2 with a low expected count, ggpiestats returns the SAME
            # uncorrected Pearson chi2(1) = 4.47, p = 0.03 for type = "parametric",
            # "nonparametric" AND "robust" - only "bayes" differs. Naming
            # "Fisher's exact test" therefore published a test that was never run,
            # and on that table Fisher gives p = 0.0511 - the other side of 0.05
            # from the number shown. statsExpressions::contingency_table() routes
            # every frequentist type to chi-square; there is no exact-test option.
            opts <- private$.effectiveOptionsList()
            # Bayes first: statsExpressions ignores `paired` for type = "bayes".
            method_name <- if (identical(opts$typestatistics, "bayes"))
                .('a Bayesian contingency table analysis')
            else if (isTRUE(opts$paired))
                # statsExpressions runs mcnemar.test(correct = FALSE) while base R
                # defaults to correct = TRUE, so an unqualified "McNemar's test" does
                # not reproduce in base R. Name the correction.
                .("McNemar's test without continuity correction")
            else if (isTRUE(opts$resultssubtitle) &&
                     !is.null(private$.exactSubtitle(df, opts$dep, opts$group,
                                                    opts$counts, opts$conflevel, opts$digits)))
                .("Fisher's exact test for the sparse two-way chart")
            else .("Pearson's chi-squared test")
            if (!is.null(opts$grvar) && !isTRUE(opts$paired) &&
                !identical(opts$typestatistics, "bayes"))
                method_name <- paste0(method_name, "; ", .("split panels use Pearson's chi-squared test"))

            
            # Interpolate the {outcome}/{groups}/{method} placeholders with glue.
            # (paste() only honors sep/collapse, so named args were previously
            # treated as positional values and the placeholders never filled.)
            sample_description <- if (!is.null(self$options$group) && self$options$group != "") {
                glue::glue(.('We compared {outcome} distributions across {groups} using {method}.'),
                     outcome = htmltools::htmlEscape(self$options$dep),
                     groups = htmltools::htmlEscape(self$options$group),
                     method = method_name)
            } else if (isTRUE(opts$resultssubtitle)) {
                one_way_method <- if (identical(opts$typestatistics, "bayes"))
                    .('a Bayesian goodness-of-fit proportion analysis')
                else
                    .("Pearson's chi-squared goodness-of-fit test")
                glue::glue(.('We analyzed the distribution of {outcome} using {method}.'),
                     outcome = htmltools::htmlEscape(self$options$dep),
                     method = one_way_method)
            } else {
                glue::glue(.('We analyzed the distribution of {outcome} using descriptive statistics.'),
                     outcome = htmltools::htmlEscape(self$options$dep))
            }
            
            glue::glue(
                "<h4>{report_title}</h4>
                <div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border: 1px solid #dee2e6; border-radius: 5px; color: inherit;'>
                <p><strong>{methods_title}:</strong></p>
                <p>{sample_description} {additional_details}</p>
                <p><strong>{results_title}:</strong></p>
                <p>{results_placeholder}</p>
                </div>
                <p><small>{note}</small></p>
                <hr>",
                report_title = .('Copy-Ready Report Template'),
                methods_title = .('Methods'),
                results_title = .('Results'),
                additional_details = .('All analyses were performed using jamovi statistical software. Specify your prespecified significance threshold when reporting frequentist tests.'),
                results_placeholder = .('[Enter the test statistic, effect estimate, confidence interval and p-value or Bayes factor from the relevant chart.]'),
                note = .('Copy the text above and modify as needed for your manuscript or report.')
            )
        },
        
        # Helper Methods for Validation ----

        .normalizeFactorCol = function(col) {
            if (inherits(col, "haven_labelled") && requireNamespace("haven", quietly = TRUE)) {
                col <- haven::as_factor(col)
            }
            if (is.factor(col)) {
                lv <- levels(col)
                bad <- is.na(lv) | lv == "<NA>"
                if (any(bad)) {
                    col <- factor(as.character(col), levels = lv[!bad])
                }
            } else if (is.character(col)) {
                col[col == "<NA>"] <- NA_character_
            }
            col
        },
        
        .validateVariables = function() {
            dep <- self$options$dep
            group <- self$options$group
            grvar <- self$options$grvar
            
            # Check if required variables exist
            all_vars <- c(dep, group, grvar)
            all_vars <- all_vars[!is.null(all_vars) & all_vars != ""]
            
            missing_vars <- all_vars[!all_vars %in% names(self$data)]
            if (length(missing_vars) > 0) {
                jmvcore::reject(.('Variables not found in data: {vars}'),
                                vars = paste(missing_vars, collapse = ', '))
            }
            
            # Validate that variables are appropriate for pie charts (categorical)
            for (var in all_vars) {
                if (!is.null(var) && var != "") {
                    var_class <- class(self$data[[var]])
                    if (!any(c("factor", "character", "logical") %in% var_class)) {
                        # Try to convert numeric to factor if it has few unique values
                        if (is.numeric(self$data[[var]])) {
                            unique_vals <- length(unique(self$data[[var]][!is.na(self$data[[var]])]))
                            if (unique_vals > 10) {
                                jmvcore::reject(
                                    .('Variable "{var}" appears to be continuous ({count} unique values). Pie charts are for categorical data. Consider converting to groups first.'),
                                    var = var, count = unique_vals
                                )
                            }
                        }
                    }
                }
            }
            
            # CRITICAL FIX #2: Validate counts variable before statistical requirements
            counts_var <- self$options$counts
            if (!is.null(counts_var) && counts_var != "") {
                counts_validation <- private$.validateCounts(self$data, counts_var)
                if (!counts_validation$valid) {
                    jmvcore::reject(counts_validation$message)
                }
            }

            # Enhanced validation for statistical tests
            private$.validateStatisticalRequirements(dep, group)

            return(TRUE)
        },
        
        .validateStatisticalRequirements = function(dep, group) {
            df <- self$data
            if (!is.null(dep) && dep != "" && dep %in% names(df)) {
                df[[dep]] <- private$.normalizeFactorCol(df[[dep]])
            }
            if (!is.null(group) && group != "" && group %in% names(df)) {
                df[[group]] <- private$.normalizeFactorCol(df[[group]])
            }
            counts_var <- self$options$counts
            has_valid_counts <- !is.null(counts_var) && counts_var != "" &&
                counts_var %in% names(df) && is.numeric(df[[counts_var]])
            if (has_valid_counts && !isTRUE(self$options$paired)) {
                df <- df[!is.na(df[[counts_var]]) & df[[counts_var]] > 0, , drop = FALSE]
            }

            total_obs <- if (has_valid_counts) {
                sum(df[[counts_var]], na.rm = TRUE)
            } else if (!is.null(dep) && dep != "" && dep %in% names(df)) {
                sum(!is.na(df[[dep]]))
            } else {
                nrow(df)
            }
            if (total_obs < 3) {
                return()
            }

            # Check minimum group sizes for statistical tests when group variable is present
            if (!is.null(group) && group != "" && group %in% names(df)) {
                group_sizes <- table(droplevels(as.factor(df[[group]])), useNA = "no")

                if (length(group_sizes) < 2) {
                    jmvcore::reject(.('Grouping variable must have at least 2 categories for comparison.'))
                }
            }

            # Check dependent variable has sufficient variation
            if (!is.null(dep) && dep != "" && dep %in% names(df)) {
                dep_levels <- table(droplevels(as.factor(df[[dep]])), useNA = "no")
                if (length(dep_levels) < 2) {
                    jmvcore::reject(
                        .('Variable "{var}" has insufficient variation (only {count} level). Need at least 2 categories for meaningful pie chart.'),
                        var = dep, count = length(dep_levels)
                    )
                }
            }
        },

        .validateCounts = function(data, counts_var) {
            # CRITICAL FIX #2: Validate counts variable for aggregated data

            # Check if variable exists
            if (!counts_var %in% names(data)) {
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('Counts variable "%s" not found in data. Available variables: %s'),
                        counts_var,
                        paste(names(data), collapse = ", ")
                    )
                ))
            }

            # Check if variable is numeric
            if (!is.numeric(data[[counts_var]])) {
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('Counts variable "%s" must be numeric. Found type: %s. The counts variable should contain frequency counts for aggregated/tabulated data.'),
                        counts_var,
                        class(data[[counts_var]])[1]
                    )
                ))
            }

            # Check for negative values
            if (any(data[[counts_var]] < 0, na.rm = TRUE)) {
                neg_count <- sum(data[[counts_var]] < 0, na.rm = TRUE)
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('Counts variable "%s" contains %d negative values. Frequency counts must be non-negative (>= 0).'),
                        counts_var,
                        neg_count
                    )
                ))
            }

            # Check if all values are NA
            if (all(is.na(data[[counts_var]]))) {
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('Counts variable "%s" contains only missing values (NA). Cannot compute statistics with no valid counts.'),
                        counts_var
                    )
                ))
            }

            non_na_counts <- data[[counts_var]][!is.na(data[[counts_var]])]
            if (any(!is.finite(non_na_counts))) {
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('Counts variable "%s" contains non-finite values (Inf/NaN). Frequency counts must be finite whole numbers.'),
                        counts_var
                    )
                ))
            }

            if (any(abs(non_na_counts - round(non_na_counts)) > 1e-7)) {
                frac_count <- sum(abs(non_na_counts - round(non_na_counts)) > 1e-7)
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('Counts variable "%s" contains %d non-integer (fractional) value(s). Frequency counts must be whole numbers.'),
                        counts_var,
                        frac_count
                    )
                ))
            }

            # Check if total count is zero after removing NAs
            total_count <- sum(data[[counts_var]], na.rm = TRUE)
            if (total_count == 0) {
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('Counts variable "%s" sums to zero. Need at least one observation with positive count.'),
                        counts_var
                    )
                ))
            }

            # All validations passed
            return(list(valid = TRUE, message = .('Counts variable validated successfully.')))
        },

        # The chart shows McNemar only for a paired, non-Bayesian run:
        # statsExpressions ignores `paired` for type = "bayes".
        .isMcNemar = function() {
            opts <- private$.effectiveOptionsList()
            isTRUE(opts$paired) && !identical(opts$typestatistics, "bayes")
        },

        # McNemar's test uses only the DISCORDANT pairs b and c, so what it can show
        # depends on b + c, not on the table total. The chart keeps the plotting
        # package's asymptotic test without continuity correction
        # (mcnemar.test(correct = FALSE)). Fagerland, Lydersen & Laake (BMC Med Res
        # Methodol 2013;13:91) recommend it where a type I error rate slightly above
        # the nominal level is acceptable (at most 5.37% at 5% over 10 to 100 pairs),
        # and "do not recommend use of the McNemar exact conditional test nor the
        # asymptotic test with CC in any situation". So there is no b + c cut-off;
        # the run notices say how much information b + c carries.
        #
        # Returns the number of discordant pairs b + c of a 2x2 table, or NULL (no
        # grouping variable, or not 2x2).
        .pairedDiscordant = function(data, dep_var, group_var, counts_var = NULL) {
            if (is.null(group_var) || !nzchar(group_var))
                return(NULL)
            tryCatch({
                has_cv <- !is.null(counts_var) && nzchar(counts_var) && counts_var %in% names(data) &&
                    is.numeric(data[[counts_var]])
                if (has_cv) {
                    wt <- ifelse(is.na(data[[counts_var]]), 0, data[[counts_var]])
                    active_dep <- names(which(tapply(wt, as.character(data[[dep_var]]), sum) > 0))
                    active_grp <- names(which(tapply(wt, as.character(data[[group_var]]), sum) > 0))
                    active_all <- union(active_dep, active_grp)
                    data <- data[as.character(data[[dep_var]]) %in% active_all &
                                 as.character(data[[group_var]]) %in% active_all, , drop = FALSE]
                }
                lev_dep <- levels(droplevels(as.factor(data[[dep_var]])))
                lev_grp <- levels(droplevels(as.factor(data[[group_var]])))
                if (!setequal(lev_dep, lev_grp))
                    return(NULL)
                data[[dep_var]] <- factor(data[[dep_var]], levels = lev_dep)
                data[[group_var]] <- factor(data[[group_var]], levels = lev_dep)
                if (has_cv) {
                    formula_str <- paste0(jmvcore::composeTerm(counts_var), " ~ ",
                                          jmvcore::composeTerm(dep_var), " + ",
                                          jmvcore::composeTerm(group_var))
                    tb <- xtabs(jmvcore::asFormula(formula_str), data = data)
                } else {
                    tb <- table(data[[dep_var]], data[[group_var]])
                }
                if (!identical(dim(tb), c(2L, 2L)))
                    return(NULL)

                tb[1L, 2L] + tb[2L, 1L]
            }, error = function(e) NULL)
        },

        # "p = < 0.001" reads badly; the operator belongs to the number. Returns a
        # ready-to-embed string, e.g. "p = 0.051" or "p < 0.001". `html` picks the
        # entity form for the HTML panels (a bare "<" is fine in HTML5 text when
        # followed by a space, but the entity is unambiguous).
        .fmtP = function(p, html = TRUE) {
            if (!is.finite(p)) return("p = NA")
            if (p < 0.001) paste0("p ", if (html) "&lt;" else "<", " 0.001")
            else paste0("p = ", formatC(p, format = "f", digits = 3))
        },

        # Build a replacement subtitle carrying Fisher's exact test.
        #
        # ggstatsplot has no exact-test option, so on a sparse 2x2 the chart
        # subtitle showed an uncorrected Pearson chi-square that is not valid at
        # those expected counts - and the Assumptions panel had to tell the reader
        # to ignore the number printed on the figure. A figure that contradicts its
        # own caption travels badly: the chart gets pasted into a slide deck without
        # the panel. So compute the exact test here and put it ON the plot.
        #
        # Returns a plotmath expression in ggstatsplot's own idiom, or NULL when
        # the exact test does not apply (not 2x2, adequate expected counts, paired
        # data or a Bayesian analysis). Paired data keep the plotting package's
        # McNemar test (see .pairedDiscordant); Fisher assumes independent samples.
        .exactSubtitle = function(data, dep_var, group_var, counts_var = NULL,
                                  conf_level = 0.95, digits = 2L) {
            if (is.null(group_var) || !nzchar(group_var))
                return(NULL)
            opts <- private$.effectiveOptionsList()
            if (identical(opts$typestatistics, "bayes") || isTRUE(opts$paired))
                return(NULL)

            tryCatch({
                if (!is.null(counts_var) && nzchar(counts_var) && counts_var %in% names(data)) {
                    sub_df <- data[!is.na(data[[counts_var]]) & data[[counts_var]] > 0, , drop = FALSE]
                    sub_df[[dep_var]] <- droplevels(as.factor(sub_df[[dep_var]]))
                    sub_df[[group_var]] <- droplevels(as.factor(sub_df[[group_var]]))
                    formula_str <- paste0(jmvcore::composeTerm(counts_var), " ~ ",
                                          jmvcore::composeTerm(dep_var), " + ",
                                          jmvcore::composeTerm(group_var))
                    tb <- xtabs(jmvcore::asFormula(formula_str), data = sub_df)
                } else {
                    tb <- table(
                        droplevels(as.factor(data[[dep_var]])),
                        droplevels(as.factor(data[[group_var]]))
                    )
                }
                if (!identical(dim(tb), c(2L, 2L)))
                    return(NULL)

                expected <- suppressWarnings(chisq.test(tb)$expected)
                if (!any(expected < 5))
                    return(NULL)

                ft <- stats::fisher.test(tb, conf.level = conf_level)
                d  <- max(0L, as.integer(digits))
                fmt <- function(x) formatC(x, format = "f", digits = d)
                p_txt <- if (ft$p.value < 0.001) "< 0.001"
                         else formatC(ft$p.value, format = "f", digits = max(3L, d))

                # Odds ratio can be 0 or Inf on a zero cell; omit it rather than
                # print an uninterpretable bound.
                or <- unname(ft$estimate)
                has_or <- is.finite(or) && or > 0 && all(is.finite(ft$conf.int))

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

        .validatePairedData = function(data, dep_var, group_var) {
            # CRITICAL FIX #3: Validate paired data for McNemar test
            # McNemar test requires exactly 2x2 contingency table

            if (is.null(group_var) || group_var == "") {
                return(list(
                    valid = FALSE,
                    message = .('McNemar test requires a grouping variable (e.g., Pre vs Post, Treatment A vs B). No grouping variable specified.')
                ))
            }

            counts_var <- self$options$counts
            has_cv <- !is.null(counts_var) && counts_var != "" && counts_var %in% names(data) &&
                is.numeric(data[[counts_var]])
            if (has_cv) {
                wt <- ifelse(is.na(data[[counts_var]]), 0, data[[counts_var]])
                active_dep <- names(which(tapply(wt, as.character(data[[dep_var]]), sum) > 0))
                active_grp <- names(which(tapply(wt, as.character(data[[group_var]]), sum) > 0))
                active_all <- union(active_dep, active_grp)
                data <- data[as.character(data[[dep_var]]) %in% active_all &
                             as.character(data[[group_var]]) %in% active_all, , drop = FALSE]
            }

            lev_dep <- levels(droplevels(as.factor(data[[dep_var]])))
            lev_grp <- levels(droplevels(as.factor(data[[group_var]])))
            if (length(lev_dep) == 2L && length(lev_grp) == 2L && !setequal(lev_dep, lev_grp)) {
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('McNemar\'s test requires wide-format paired data where both variables share the same two category labels (e.g., both coded Pos/Neg). Found %s (%s) vs %s (%s).'),
                        dep_var, paste(lev_dep, collapse = "/"),
                        group_var, paste(lev_grp, collapse = "/")
                    )
                ))
            }
            if (setequal(lev_dep, lev_grp)) {
                data[[dep_var]] <- factor(data[[dep_var]], levels = lev_dep)
                data[[group_var]] <- factor(data[[group_var]], levels = lev_dep)
            } else {
                data[[dep_var]] <- droplevels(as.factor(data[[dep_var]]))
                data[[group_var]] <- droplevels(as.factor(data[[group_var]]))
            }

            # Create contingency table (use weighted if counts variable present)
            if (has_cv) {
                # Weighted contingency table
                formula_str <- paste0(jmvcore::composeTerm(counts_var),
                                      " ~ ", jmvcore::composeTerm(dep_var),
                                      " + ", jmvcore::composeTerm(group_var))
                cross_table <- xtabs(jmvcore::asFormula(formula_str), data = data)
            } else {
                # Unweighted contingency table
                cross_table <- table(data[[dep_var]], data[[group_var]])
            }

            # Check if table is exactly 2x2
            if (!all(dim(cross_table) == c(2, 2))) {
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('McNemar test requires a 2\u00d72 contingency table. Your data has %d\u00d7%d levels (%s: %d levels, %s: %d levels). For paired outcomes with more than two categories, use an appropriate test of marginal homogeneity. Do not substitute an independent-samples test or dichotomize solely to obtain this test.'),
                        nrow(cross_table), ncol(cross_table),
                        dep_var, nrow(cross_table),
                        group_var, ncol(cross_table)
                    )
                ))
            }

            # Fagerland et al. (2013) evaluated McNemar's tests on 10 to 100 pairs, so
            # below 10 there is no evidence the test keeps its error rate. Do not
            # suggest the exact binomial test (they advise against it in any
            # situation) or Fisher's exact test (it assumes independent samples).
            # A zero row or column is no obstacle: the test uses only b and c.
            total_n <- sum(cross_table)
            if (total_n < 10) {
                return(list(
                    valid = FALSE,
                    message = sprintf(
                        .('McNemar\'s test is not shown for fewer than 10 paired observations; your data has %d. The evidence that the test keeps its error rate (Fagerland et al. 2013) starts at 10 pairs, and with fewer it has almost no power. Collect more data. Fisher\'s exact test is not an alternative: it assumes independent samples, not paired ones.'),
                        total_n
                    )
                ))
            }

            # All validations passed
            return(list(
                valid = TRUE,
                message = sprintf(
                    .('Data structure validated for McNemar test: 2\u00d72 table with %d paired observations.'),
                    total_n
                )
            ))
        },

        .getCachedData = function() {
            # CRITICAL FIX #1: Include actual data content in hash (not just dimensions/names)
            # CRITICAL FIX: Defensive check for dep variable
            if (is.null(self$options$dep) || length(self$options$dep) == 0 ||
                (length(self$options$dep) == 1 && self$options$dep == "")) {
                jmvcore::reject(.('Dependent variable is required but not properly set'))
            }

            opts <- private$.effectiveOptionsList()

            # Build list of relevant columns for hash
            relevant_cols <- c(opts$dep)
            if (!is.null(opts$group) && length(opts$group) > 0 && opts$group != "") {
                relevant_cols <- c(relevant_cols, opts$group)
            }
            if (!is.null(opts$grvar) && length(opts$grvar) > 0 && opts$grvar != "") {
                relevant_cols <- c(relevant_cols, opts$grvar)
            }
            if (!is.null(opts$counts) && length(opts$counts) > 0 && opts$counts != "") {
                relevant_cols <- c(relevant_cols, opts$counts)
            }

            # Filter to columns that actually exist
            relevant_cols <- relevant_cols[relevant_cols %in% names(self$data)]

            # Additional safety check
            if (length(relevant_cols) == 0) {
                jmvcore::reject(.('No valid analysis variables found in dataset. Please check variable names.'))
            }

            # Hash actual data content (not just structure)
            data_content_hash <- if (length(relevant_cols) > 0) {
                digest::digest(
                    self$data[, relevant_cols, drop = FALSE],
                    algo = "md5"
                )
            } else {
                digest::digest(dim(self$data), algo = "md5")
            }

            # Create hash of current data and options state
            current_hash <- digest::digest(list(
                data_dim = dim(self$data),
                data_names = names(self$data),
                data_content = data_content_hash,  #  NOW INCLUDES ACTUAL DATA VALUES
                options = opts
            ), algo = "md5")

            # Return cached data if hash matches and validation passed
            if (!is.null(private$.processedData) &&
                !is.null(private$.data_hash) &&
                private$.data_hash == current_hash &&
                private$.validation_passed) {
                return(private$.processedData)
            }

            # Validate and prepare fresh data
            # Checkpoint before validation (statistical requirements checking)
            private$.checkpoint(flush = FALSE)
            private$.validateVariables()
            private$.processedData <- private$.prepareData()
            private$.data_hash <- current_hash
            private$.validation_passed <- TRUE

            return(private$.processedData)
        },

        # Optimized data preparation with caching
        .prepareData = function(force_refresh = FALSE) {
            # CRITICAL FIX: Defensive check for dep variable
            if (is.null(self$options$dep) || length(self$options$dep) == 0 ||
                (length(self$options$dep) == 1 && self$options$dep == "")) {
                jmvcore::reject(.('Dependent variable is required but not properly set'))
            }

            # No progress message in `todo` here: this also runs from the plot
            # renderers (cache miss), where it overwrote the status set by .run().

            mydata <- self$data

            # Normalize explicit <NA> factor levels (from addNA / factor(exclude = NULL))
            # and haven_labelled vectors before complete.cases() so missing values are
            # excluded rather than treated as an active category level.
            for (v in c(self$options$dep, self$options$group, self$options$grvar)) {
                if (!is.null(v) && length(v) == 1L && nzchar(v) && v %in% names(mydata)) {
                    mydata[[v]] <- private$.normalizeFactorCol(mydata[[v]])
                }
            }

            # Selective NA omission - only remove rows with NAs in unsplit analysis variables
            # (grvar NAs are omitted only when rendering split-by panels in .plot4 / .emitRunNotices)
            relevant_cols <- c(self$options$dep)
            if (!is.null(self$options$group) && length(self$options$group) > 0 && self$options$group != "") {
                relevant_cols <- c(relevant_cols, self$options$group)
            }
            if (!is.null(self$options$counts) && length(self$options$counts) > 0 && self$options$counts != "") {
                relevant_cols <- c(relevant_cols, self$options$counts)
            }

            # Safety check
            if (length(relevant_cols) == 0) {
                jmvcore::reject(.('No valid analysis variables specified'))
            }

            # Remove rows with NAs only in relevant columns
            n_before <- nrow(mydata)
            private$.checkpoint()
            # Keep data as data.frame even when only one relevant column is present
            mydata <- mydata[complete.cases(mydata[relevant_cols]), , drop = FALSE]
            n_after <- nrow(mydata)

            # Record dropped rows so .emitRunNotices() can surface them without
            # being overwritten when .run() writes the ready status into todo.
            private$.droppedInfo <- if (n_before > n_after) {
                list(
                    n_dropped = n_before - n_after,
                    n_before  = n_before,
                    vars      = relevant_cols
                )
            } else {
                NULL
            }

            counts_var <- self$options$counts
            has_counts <- !is.null(counts_var) && length(counts_var) > 0 &&
                counts_var != "" && counts_var %in% names(mydata) &&
                is.numeric(mydata[[counts_var]])

            if (has_counts) {
                mydata[[counts_var]] <- as.integer(round(mydata[[counts_var]]))
                if (!isTRUE(self$options$paired)) {
                    mydata <- mydata[mydata[[counts_var]] > 0L, , drop = FALSE]
                } else if (!is.null(self$options$group) && nzchar(self$options$group) &&
                           self$options$group %in% names(mydata)) {
                    wt <- mydata[[counts_var]]
                    active_dep <- names(which(tapply(wt, as.character(mydata[[self$options$dep]]), sum) > 0))
                    active_grp <- names(which(tapply(wt, as.character(mydata[[self$options$group]]), sum) > 0))
                    active_all <- union(active_dep, active_grp)
                    mydata <- mydata[as.character(mydata[[self$options$dep]]) %in% active_all &
                                     as.character(mydata[[self$options$group]]) %in% active_all, , drop = FALSE]
                }
            }

            total_obs <- if (has_counts) sum(mydata[[counts_var]], na.rm = TRUE) else nrow(mydata)
            if (nrow(mydata) == 0 || total_obs < 3) {
                jmvcore::reject(.('No complete data rows available after handling missing values. Please check your data for the selected variables.'))
            }

            dep_col <- self$options$dep
            grp_col <- self$options$group
            if (isTRUE(self$options$paired) && !is.null(grp_col) && length(grp_col) > 0 &&
                grp_col != "" && grp_col %in% names(mydata)) {
                lev_dep <- levels(droplevels(as.factor(mydata[[dep_col]])))
                lev_grp <- levels(droplevels(as.factor(mydata[[grp_col]])))
                if (setequal(lev_dep, lev_grp)) {
                    mydata[[dep_col]] <- factor(mydata[[dep_col]], levels = lev_dep)
                    mydata[[grp_col]] <- factor(mydata[[grp_col]], levels = lev_dep)
                }
            } else {
                mydata[[dep_col]] <- droplevels(as.factor(mydata[[dep_col]]))
                if (!is.null(grp_col) && length(grp_col) > 0 && grp_col != "" && grp_col %in% names(mydata)) {
                    mydata[[grp_col]] <- droplevels(as.factor(mydata[[grp_col]]))
                }
            }

            return(mydata)
        },

        # Helper method to parse ratio string
        .parseRatio = function(ratio_string) {
            if (is.null(ratio_string) || ratio_string == "") return(NULL)

            tryCatch({
                ratios <- as.numeric(strsplit(ratio_string, ",")[[1]])

                # ERROR: Non-numeric values
                if (any(is.na(ratios))) {
                    return(NULL)
                }

                # ERROR: Ratios don't sum to 1
                if (abs(sum(ratios) - 1) > 0.001) {
                    return(NULL)
                }

                # ERROR: Non-positive ratios
                if (any(ratios <= 0)) {
                    return(NULL)
                }

                return(ratios)
            }, error = function(e) {
                return(NULL)
            })
        },

        # Guard: expected proportions (ratio) must have one value per outcome
        # level. .parseRatio only validates numeric / sums-to-1 / positivity, not
        # length, so a mismatched vector (e.g. "0.5,0.5" for a 3-level outcome)
        # would otherwise be passed straight to ggpiestats(ratio=...) and error
        # inside the statsExpressions goodness-of-fit test. On mismatch, fall back
        # to NULL (equal theoretical proportions) and tell the user why.
        .applyRatioLengthGuard = function(ratio, data, dep) {
            if (is.null(ratio) || is.null(dep) || is.null(data[[dep]])) return(ratio)
            n_lev <- nlevels(droplevels(as.factor(data[[dep]])))
            if (length(ratio) != n_lev) {
                private$.addNotice(
                    "WARNING",
                    .('Expected proportions ignored'),
                    sprintf(
                        .('You supplied %d expected proportion(s), but the outcome variable has %d level(s). The number of expected proportions must match the number of outcome categories. Falling back to equal theoretical proportions.'),
                        length(ratio), n_lev
                    )
                )
                return(NULL)
            }
            ratio
        },

        # Optimized options preparation with caching
        .prepareOptions = function() {
            # Deliberately NOT cached. The previous `.processedOptions` cache had no
            # invalidation key (unlike .getCachedData(), which hashes data+options),
            # so once populated it returned the FIRST run's options for the life of
            # the R6 instance. jamovi reuses one instance across option changes - see
            # the note in .getPlotTheme() - so every renderer reading through here
            # silently ignored every later option change. Measured: after switching
            # to 5 digits and a Bayesian test, this still reported 2 and "parametric".
            # .effectiveOptionsList() is cheap and always current, so the cache bought
            # nothing and cost correctness.

            # No progress message in `todo`: every plot renderer calls this, and the
            # write replaced the status set by .run() (seen after Bayesian renders).

            # Defensive validation: ensure dep is not zero-length
            if (is.null(self$options$dep) || length(self$options$dep) == 0 || (length(self$options$dep) == 1 && self$options$dep == "")) {
                jmvcore::reject(.('Dependent variable is not properly set'))
            }
            
            # Build effective options (includes preset and parsed ratio)
            private$.effectiveOptionsList()
        },

        .selectPalette = function(data, dep) {
            n_slices <- if (!is.null(dep) && dep %in% names(data)) {
                nlevels(droplevels(as.factor(data[[dep]])))
            } else 0L
            if (n_slices <= 8L) "RColorBrewer::Dark2"
            else if (n_slices <= 20L) "ggthemes::Tableau_20"
            else if (n_slices <= 36L) "Polychrome::palette36"
            else "palettesForR::Named"
        }



        # run ----
        ,
        # Every user-facing notice that depends on the data is raised from here,
        # called near the end of .run(). The plot functions cannot do it: notices
        # written during .plot() are discarded because jamovi has already composed
        # the results panel by then. That is why malformed "Expected proportions"
        # produced no message at all - the guard ran, in the wrong lifecycle phase.
        .emitRunNotices = function(prepared_data) {
            opts  <- private$.effectiveOptionsList()
            dep   <- opts$dep
            group <- opts$group

            # Expected proportions. .parseRatio silently returns NULL on bad
            # input (its four error messages were commented out during an
            # abandoned Notice-to-HTML migration), so re-derive the reason here.
            raw <- self$options$ratio
            if (!is.null(raw) && nzchar(trimws(raw))) {
                vals <- suppressWarnings(as.numeric(trimws(strsplit(raw, ",")[[1]])))
                ignored <- function(why)
                    private$.addNotice("WARNING", .('Expected proportions ignored'),
                        paste0(why, " ", .('Equal theoretical proportions were used instead.')))

                if (anyNA(vals))
                    ignored(sprintf(.('\'%s\' is not a list of numbers; expected comma-separated values such as "0.5,0.5".'), raw))
                else if (any(vals <= 0))
                    ignored(sprintf(.('\'%s\' contains a zero or negative proportion.'), raw))
                else if (abs(sum(vals) - 1) > 0.001)
                    ignored(sprintf(.('\'%s\' sums to %s, but expected proportions must sum to 1.'),
                                    raw, formatC(sum(vals), format = "f", digits = 3)))
                else
                    # Valid numbers: the remaining failure is a length mismatch,
                    # which .applyRatioLengthGuard reports.
                    invisible(private$.applyRatioLengthGuard(vals, prepared_data, dep))
            }

            # A pie chart encodes proportion as angle, which readers compare poorly
            # once the slices get thin; the existing guard only rejects a SINGLE
            # level, so a 90-level variable rendered 90 unreadable slivers without
            # comment. Warn rather than block - the numbers are still correct, and
            # the composition table remains usable.
            if (!is.null(dep) && dep %in% names(prepared_data)) {
                n_slices <- nlevels(droplevels(as.factor(prepared_data[[dep]])))
                if (n_slices > 7)
                    private$.addNotice("WARNING", .('Too many slices to read'),
                        sprintf(.('\'%s\' has %d categories, so the chart has %d slices. Pie charts become hard to read beyond about seven; consider collapsing rare categories, or use a bar chart, which compares lengths rather than angles.'),
                                dep, n_slices, n_slices))
            }

            # Sparse r x c (!= 2x2) two-way tables cannot be auto-swapped by .exactSubtitle()
            # (which only replaces 2x2 tables where OR is defined). Emit a visible warning
            # notice when expected counts < 5 in a larger contingency table.
            if (!is.null(group) && nzchar(group) && group %in% names(prepared_data) &&
                !identical(dep, group) && !isTRUE(opts$paired) &&
                !identical(opts$typestatistics, "bayes")) {
                tryCatch({
                    cv <- opts$counts
                    tb <- if (!is.null(cv) && nzchar(cv) && cv %in% names(prepared_data)) {
                        f <- paste(jmvcore::composeTerm(cv), "~", jmvcore::composeTerm(dep),
                                   "+", jmvcore::composeTerm(group))
                        stats::xtabs(jmvcore::asFormula(f), data = prepared_data)
                    } else {
                        table(droplevels(as.factor(prepared_data[[dep]])),
                              droplevels(as.factor(prepared_data[[group]])))
                    }
                    if (nrow(tb) >= 2L && ncol(tb) >= 2L && !identical(dim(tb), c(2L, 2L))) {
                        exp_counts <- suppressWarnings(stats::chisq.test(tb)$expected)
                        if (any(exp_counts < 5)) {
                            n_low <- sum(exp_counts < 5)
                            n_tot <- length(exp_counts)
                            fp <- if (prod(dim(tb)) <= 30) {
                                tryCatch(stats::fisher.test(tb)$p.value, error = function(e) NA_real_)
                            } else NA_real_
                            private$.addNotice("WARNING", .('Low expected cell counts in contingency table'),
                                sprintf(
                                    .('%d of %d expected cell counts are below 5 in the %d\u00d7%d contingency table, so the Pearson chi-square shown on the chart subtitle may be unreliable.%s'),
                                    n_low, n_tot, nrow(tb), ncol(tb),
                                    if (is.finite(fp)) paste0(" ", sprintf(.('Fisher\'s exact test on this table gives %s.'), private$.fmtP(fp, html = FALSE)))
                                    else paste0(" ", .('Consider collapsing sparse categories.'))
                                ))
                        }
                    }
                }, error = function(e) NULL)
            }

            # Split By panels with a sparse table: give the exact p-value for each
            # one here. .plot4 builds each panel itself and, when the subtitle is
            # on, already prints Fisher's exact test on a sparse 2x2 panel (same
            # table, same p); a larger sparse panel still shows a chi-square, and
            # with the subtitle off no test is shown. The wording must hold in all
            # three cases, so it must not claim a chi-square is on the panel.
            grvar <- opts$grvar
            if (!is.null(grvar) && !is.null(group) && nzchar(grvar) && nzchar(group) &&
                grvar %in% names(prepared_data) && !isTRUE(opts$paired) &&
                !identical(opts$typestatistics, "bayes")) {

                lines <- character()
                for (lv in levels(droplevels(as.factor(prepared_data[[grvar]])))) {
                    sub_df <- prepared_data[!is.na(prepared_data[[grvar]]) &
                                            prepared_data[[grvar]] == lv, , drop = FALSE]
                    r <- tryCatch({
                        cv <- opts$counts
                        tb <- if (!is.null(cv) && nzchar(cv)) {
                            sub_df[[dep]] <- droplevels(as.factor(sub_df[[dep]]))
                            sub_df[[group]] <- droplevels(as.factor(sub_df[[group]]))
                            f <- paste(jmvcore::composeTerm(cv), "~", jmvcore::composeTerm(dep),
                                       "+", jmvcore::composeTerm(group))
                            stats::xtabs(jmvcore::asFormula(f), data = sub_df)
                        } else table(droplevels(as.factor(sub_df[[dep]])),
                                     droplevels(as.factor(sub_df[[group]])))
                        if (nrow(tb) < 2L || ncol(tb) < 2L) NULL
                        else if (!any(suppressWarnings(chisq.test(tb)$expected) < 5)) NULL
                        else {
                            fp <- if (prod(dim(tb)) <= 30) {
                                tryCatch(stats::fisher.test(tb)$p.value, error = function(e) NA_real_)
                            } else NA_real_
                            if (is.finite(fp))
                                sprintf("%s: %s", lv, private$.fmtP(fp, html = FALSE))
                            else
                                sprintf(.("%s: sparse contingency table (expected cell counts < 5)"), lv)
                        }
                    }, error = function(e) NULL)
                    if (!is.null(r)) lines <- c(lines, r)
                }

                if (length(lines))
                    private$.addNotice("WARNING", .('Split By panels: use these exact p-values'),
                        paste0(sprintf(.('In %d of the Split By panels the expected cell counts are below 5, so a chi-square test is unreliable there. Fisher\'s exact test gives:'),
                                       length(lines)),
                               " ", paste(lines, collapse = "; "), "."))
            }

            # paired disables the single-variable chart; say so where it is seen.
            if (isTRUE(self$options$paired))
                private$.addNotice("WARNING", .('Single-variable pie chart not shown'),
                    .('The paired/repeated-measures option requires a two-way (grouped) comparison, so the single-variable pie chart cannot be drawn. Disable the "Paired" option to display this chart, or read the paired result from the grouped charts.'))

            # .validatePairedData ran only inside .plot2/.plot4, i.e. during
            # .plot(), so a paired request on a table that is not 2x2 drew
            # nothing and said nothing. Raise its verdict here as well.
            if (isTRUE(self$options$paired)) {
                v <- private$.validatePairedData(prepared_data, dep, group)
                m <- private$.pairedDiscordant(prepared_data, dep, group, self$options$counts)
                if (!v$valid) {
                    private$.addNotice("WARNING", .('Paired analysis not shown'), v$message)
                } else if (!private$.isMcNemar()) {
                    private$.addNotice("WARNING", .('Pairing not used by the Bayesian analysis'),
                        .('The Bayesian contingency analysis has no paired version: the chart shows a Bayes factor for independence of the two variables, not a test of change between the paired measurements. Choose a type of statistic other than Bayes for McNemar\'s test.'))
                } else if (!is.null(m)) {
                    # How much information the discordant pairs carry, not a validity
                    # cut-off. With m pairs the asymptotic statistic is at most m, so
                    # its smallest p is P(chi2(1) > m): m <= 3 cannot reach 0.05.
                    if (m == 0)
                        private$.addNotice("WARNING", .('No discordant pairs'),
                            .('Every pair falls in the same category on both measurements, so McNemar\'s test has no information: p is 1 by convention and no test result is shown on the chart.'))
                    else if (stats::pchisq(m, df = 1, lower.tail = FALSE) >= 0.05)
                        private$.addNotice("WARNING", .('Too few discordant pairs to reach p < 0.05'),
                            if (m == 1)
                                .('Only 1 discordant pair: McNemar\'s test cannot reach p < 0.05, so its non-significant result says nothing about whether the proportions changed.')
                            else
                                sprintf(.('Only %d discordant pairs: with so few, McNemar\'s test cannot reach p < 0.05 whatever their split, so its non-significant result says nothing about whether the proportions changed.'), m))
                    else if (m < 10)
                        private$.addNotice("INFO", .('Few discordant pairs'),
                            sprintf(.('Only %d discordant pairs, so McNemar\'s test has little power here; a non-significant result does not show that the proportions are the same.'), m))
                }

                if (!is.null(grvar) && nzchar(grvar) && grvar %in% names(prepared_data) &&
                    !is.null(group) && nzchar(group) && private$.isMcNemar()) {
                    split_pair_notes <- character()
                    for (lv in levels(droplevels(as.factor(prepared_data[[grvar]])))) {
                        sub_df <- prepared_data[!is.na(prepared_data[[grvar]]) &
                                                prepared_data[[grvar]] == lv, , drop = FALSE]
                        if (nrow(sub_df) == 0) next
                        v_sub <- private$.validatePairedData(sub_df, dep, group)
                        m_sub <- private$.pairedDiscordant(sub_df, dep, group, self$options$counts)
                        if (!v_sub$valid) {
                            split_pair_notes <- c(split_pair_notes, sprintf("%s: %s", lv, v_sub$message))
                        } else if (!is.null(m_sub)) {
                            if (m_sub == 0) {
                                split_pair_notes <- c(split_pair_notes,
                                    sprintf(.("%s: No discordant pairs (no test result shown)."), lv))
                            } else if (stats::pchisq(m_sub, df = 1, lower.tail = FALSE) >= 0.05) {
                                split_pair_notes <- c(split_pair_notes,
                                    sprintf(.("%s: Only %d discordant pair(s) (cannot reach p < 0.05)."), lv, m_sub))
                            } else if (m_sub < 10) {
                                split_pair_notes <- c(split_pair_notes,
                                    sprintf(.("%s: Only %d discordant pairs (low power)."), lv, m_sub))
                            }
                        }
                    }
                    if (length(split_pair_notes) > 0) {
                        private$.addNotice("WARNING", .('Split By paired panels'),
                            paste(split_pair_notes, collapse = " "))
                    }
                }
            }

            # A variable crossed with itself yields a perfectly diagonal table, so
            # the association test is a tautology: every off-diagonal cell is 0 and
            # the p-value is ~0 by construction, not by any finding in the data.
            if (!is.null(group) && !is.null(dep) && identical(dep, group))
                private$.addNotice("STRONG_WARNING", .('Variable compared with itself'),
                    sprintf(.('\'%s\' is used as both the outcome and the grouping variable. The contingency table is then diagonal by construction, so the association test reports a near-zero p-value that reflects the setup rather than the data. Choose a different grouping variable.'),
                            dep))

            # Rows excluded for missing values. Previously written into `todo`
            # by .prepareData() and immediately overwritten by .run()'s status
            # message, so the exclusion was never seen.
            di <- private$.droppedInfo
            if (!is.null(di) && isTRUE(di$n_dropped > 0)) {
                private$.addNotice("INFO", .('Missing values excluded'),
                    sprintf(.('%d of %d rows (%.1f%%) were excluded because of missing values in the analysis variables: %s.'),
                            as.integer(di$n_dropped), as.integer(di$n_before),
                            100 * di$n_dropped / di$n_before,
                            paste(di$vars, collapse = ", ")))
            }

            if (isTRUE(self$options$messages)) {
                opts <- private$.effectiveOptionsList()
                private$.addNotice("INFO", .("Analysis settings"),
                    sprintf(.("Effective test type: %s. Paired: %s. Confidence level: %.1f%%."),
                            opts$typestatistics, opts$paired, 100 * opts$conflevel))
            }
            private$.renderNotices()
        },

        .run = function() {

            # Reset notices at the top of every run so stale messages from a
            # previous state (e.g. a paired-validation failure the user has since
            # fixed) are cleared; .emitRunNotices() re-populates and re-renders
            # near the end of the run.
            private$.noticeList <- list()
            private$.renderNotices()

            # Generate clinical interpretation panels
            private$.generateClinicalPanels()

            # Initial Message ----
            # CRITICAL FIX: Check for zero-length character vectors
            if ( is.null(self$options$dep) ||
                 length(self$options$dep) == 0 ||
                 (length(self$options$dep) == 1 && self$options$dep == "") ) {

                # TODO ----
                todo <- glue::glue(
                "<br>{welcome}
                <br><br>
                {description}
                <br><br>
                {documentation}
                <br>
                {citation}
                <br><hr>",
                welcome = .('Welcome to ClinicoPath'),
                description = .('This tool will help you generate Pie Charts with statistical analysis.'),
                documentation = .('This function uses ggplot2 and ggstatsplot packages. See documentations <a href = \'https://www.indrapatil.com/ggstatsplot/reference/ggpiestats.html\' target=\'_blank\'>ggpiestats</a> and <a href = \'https://www.indrapatil.com/ggstatsplot/reference/grouped_ggpiestats.html\' target=\'_blank\'>grouped_ggpiestats</a>.'),
                citation = .('Please cite jamovi and the packages as given below.')
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
                    
            # Use cached data validation and preparation
            start_time <- Sys.time()
            # Checkpoint before data validation and preparation
            private$.checkpoint()
            prepared_data <- private$.getCachedData()
            prep_time <- round(difftime(Sys.time(), start_time, units = "secs"), 2)

            # Refresh clinical panels with the validated, prepared dataset so
            # Assumptions and Copy-Ready Report agree with the plotted data.
            private$.generateClinicalPanels(prepared_data)
                    
                    # Enhanced success message with timing and caching info
                    cache_status <- if (private$.validation_passed && !is.null(private$.processedData)) {
                        " (cached)"
                    } else {
                        " (fresh validation)"
                    }
                    
                    # Performance warnings
                    perf_warning <- ""
                    if (self$options$typestatistics == "bayes") {
                        perf_warning <- paste0("<br> <b>", .('Performance Note:'), "</b> ", .('Bayesian analysis is computationally intensive.'), "<br>")
                    }
                    
                    # Build status message
                    dep_info <- paste(.('Variable:'), htmltools::htmlEscape(self$options$dep))
                    group_info <- if (!is.null(self$options$group) && self$options$group != "") {
                        paste(",", .('grouped by'), htmltools::htmlEscape(self$options$group))
                    } else { "" }
                    split_info <- if (!is.null(self$options$grvar) && self$options$grvar != "") {
                        paste(",", .('split by'), htmltools::htmlEscape(self$options$grvar))
                    } else { "" }
                    
                    obs_count <- if (!is.null(self$options$counts) && nzchar(self$options$counts) &&
                                     self$options$counts %in% names(prepared_data) &&
                                     is.numeric(prepared_data[[self$options$counts]])) {
                        sum(prepared_data[[self$options$counts]], na.rm = TRUE)
                    } else {
                        nrow(prepared_data)
                    }

                    todo <- glue::glue(
                        "<br>{ready_msg} {dep_info}{group_info}{split_info}.<br>
                        <br>{data_msg}: {obs_count} {obs_msg}{cache_status}.<br>
                        <br>{method_msg}: {if (private$.isMcNemar()) 'McNemar' else tools::toTitleCase(private$.effectiveOptionsList()$typestatistics)} {analysis_msg}.<br>
                        {perf_warning}
                        {if(prep_time > 0.1) paste0('<br>', prep_time_msg, ': ', prep_time, ' ', seconds_msg, '.<br>') else ''}
                        <hr>",
                        ready_msg = .('Pie chart analysis ready'),
                        data_msg = .('Data prepared'),
                        obs_msg = .('observations'),
                        method_msg = .('Statistical method'),
                        analysis_msg = .('analysis'),
                        prep_time_msg = .('Preparation time'),
                        seconds_msg = .('seconds')
                    )
                    
                    self$results$todo$setContent(todo)

                    # Emit the data-dependent notices HERE, in .run(). They were
                    # previously raised from .plot1/.plot2/.plot4, i.e. during
                    # .plot(), where jamovi has already composed the results panel
                    # and discards them - so every ratio complaint was invisible.
                    private$.emitRunNotices(prepared_data)

                }, error = function(e) {
                    # Reset cache on error, then let jamovi present the
                    # jmvcore::reject() message once. Writing a second HTML
                    # error card into `todo` duplicated it (notices guide 13.6).
                    private$.processedData <- NULL
                    private$.validation_passed <- FALSE
                    private$.droppedInfo <- NULL
                    stop(e)
                })
                
                # Add checkpoint for user feedback
                private$.checkpoint()
            }
        }


        # the plot1 function ----


        ,
        .plot1 = function(image, ggtheme, theme, ...) {

            # The Bayesian contingency test samples its posterior, and a frequentist run with
            # the Bayes factor message draws one too: seed them and name the seed.
            withr::local_seed(self$options$seed)

            # Validation ----
            # CRITICAL FIX: Check for zero-length character vectors
            if ( is.null(self$options$dep) ||
                 length(self$options$dep) == 0 ||
                 (length(self$options$dep) == 1 && self$options$dep == "") )
                return()

            # Use cached data for performance with error handling
            tryCatch({
                mydata <- private$.getCachedData()
            }, error = function(e) {
                stop(paste0('Data preparation failed in plot1: ', conditionMessage(e)), call. = FALSE)
            })

            tryCatch({
                options_data <- private$.prepareOptions()
            }, error = function(e) {
                stop(paste0('Options preparation failed in plot1: ', conditionMessage(e)), call. = FALSE)
            })

            dep <- options_data$dep


            # ggpiestats ----
            # https://www.indrapatil.com/ggstatsplot/reference/ggpiestats.html



            # Get counts variable if specified
            counts_var <- if (!is.null(options_data$counts) && options_data$counts != "") {
                options_data$counts
            } else {
                NULL
            }
            # ggpiestats tidy-selects `counts`; passing a character variable by
            # name triggers tidyselect's "external vector" deprecation warning.
            counts_sym <- if (!is.null(counts_var)) rlang::sym(counts_var) else NULL

            # plot1 has no grouping variable (y = NULL), so paired does not apply.
            # r.yaml hides plot1 when paired is on, and .emitRunNotices() explains
            # why; a notice raised here (during .plot()) would be discarded.
            if (options_data$paired)
                return()

            # Guard: expected proportions must match the number of outcome levels.
            # Keep the result in a LOCAL (as .plot2/.plot4 do). Assigning NULL back
            # into options_data would delete the element, and a later `$ratio` read
            # would then partial-match a sibling key instead of returning NULL.
            ratio_vec <- private$.applyRatioLengthGuard(options_data[["ratio"]], mydata, dep)

            # Checkpoint before expensive statistical computation
            private$.checkpoint()

            plot1 <-
                ggstatsplot::ggpiestats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = NULL,
                    counts = !!counts_sym,
                    ratio = ratio_vec,
                    paired = FALSE,  # Always FALSE for single variable
                    type = options_data$typestatistics,
                    label = options_data$label,
                    label.args = list(alpha = 1, fill = "white"),
                    bf.message = options_data$bfmessage,
                    title = NULL,
                    subtitle = NULL,
                    caption = NULL,
                    conf.level = options_data$conflevel,
                    legend.title = NULL,
                    digits = options_data$digits,
                    digits.perc = options_data$digits,
                    proportion.test = options_data$proportiontest,
                    palette = private$.selectPalette(mydata, dep),
                    ggplot.component = NULL,
                    results.subtitle = options_data$resultssubtitle
                    )



            # Apply the theme: jamovi's ggtheme unless the ggstatsplot theme is requested
            plot1 <- plot1 + private$.getPlotTheme(ggtheme)

            if (isTRUE(options_data$resultssubtitle) &&
                (statsSeedMatters(options_data$typestatistics, "contingency") ||
                 captionSeedMatters(options_data$typestatistics, "contingency", 2L, options_data$bfmessage)))
                plot1 <- addSeedCaption(plot1, self, self$options$seed)

            # Print Plot1 ----

            print(plot1)
            TRUE

        },


        # the plot2 function ----


        .plot2 = function(image, ggtheme, theme, ...) {

            # The Bayesian contingency test samples its posterior, and a frequentist run with
            # the Bayes factor message draws one too: seed them and name the seed.
            withr::local_seed(self$options$seed)

            # Validation ----
            # CRITICAL FIX: Check for zero-length character vectors
            if ( is.null(self$options$dep) || length(self$options$dep) == 0 ||
                 (length(self$options$dep) == 1 && self$options$dep == "") ||
                 is.null(self$options$group) || length(self$options$group) == 0 ||
                 (length(self$options$group) == 1 && self$options$group == "") )
                return()

            # Use cached data for performance with error handling
            tryCatch({
                mydata <- private$.getCachedData()
                options_data <- private$.prepareOptions()
            }, error = function(e) {
                stop(paste0('Plot preparation failed: ', conditionMessage(e)), call. = FALSE)
            })
            
            dep <- options_data$dep
            group <- options_data$group

            if (identical(dep, group))
                return()



            # ggpiestats ----
            # https://www.indrapatil.com/ggstatsplot/reference/ggpiestats.html

            # Get counts variable if specified
            counts_var <- if (!is.null(options_data$counts) && options_data$counts != "") {
                options_data$counts
            } else {
                NULL
            }
            counts_sym <- if (!is.null(counts_var)) rlang::sym(counts_var) else NULL

            # Guard: expected proportions must match the number of outcome levels
            ratio_vec <- private$.applyRatioLengthGuard(options_data$ratio, mydata, dep)

            # Invalid paired data: draw nothing. The reason is reported by
            # .emitRunNotices() during .run(); a notice raised here is discarded.
            if (options_data$paired) {
                validation <- private$.validatePairedData(mydata, dep, group)
                if (!validation$valid)
                    return()
            }

            # The old "auto-switch to Fisher's exact" rewrote the user's chosen type
            # to "nonparametric" on a sparse 2x2. It bought nothing: measured,
            # ggpiestats returns the identical uncorrected Pearson chi2(1) = 4.47,
            # p = 0.03 for parametric, nonparametric and robust alike, because
            # statsExpressions routes every frequentist type to chi-square. So the
            # switch silently changed what the user asked for, returned the same
            # number, and led the report to label it "Fisher's exact test" - a test
            # that never ran. The exact p-value is now given in the Assumptions panel.
            override_type <- options_data$typestatistics

            # Checkpoint before expensive statistical computation
            private$.checkpoint()

            # Flush before the slow Bayesian computation. Do not write to `todo`
            # here: this runs during .plot(), where jamovi discards the update, and
            # under the R wrapper it overwrote the status written by .run(), which
            # already carries the Bayesian performance note.
            if (override_type == "bayes") {
                private$.checkpoint(flush = TRUE)
            }

            # If the chi-square on the subtitle would be invalid, swap in Fisher's
            # exact test rather than printing a number the Assumptions panel then
            # has to disown.
            exact_sub <- if (isTRUE(options_data$resultssubtitle))
                private$.exactSubtitle(mydata, dep, group, counts_var,
                                       options_data$conflevel, options_data$digits) else NULL

            plot2 <-
                ggstatsplot::ggpiestats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = !!rlang::sym(group),
                    counts = !!counts_sym,
                    ratio = ratio_vec,  # Use enhanced parsed ratio
                    paired = options_data$paired,
                    type = override_type,  # Use potentially overridden type
                    label = options_data$label,
                    label.args = list(alpha = 1, fill = "white"),
                    bf.message = options_data$bfmessage,
                    title = NULL,
                    subtitle = NULL,
                    caption = NULL,
                    conf.level = options_data$conflevel,
                    legend.title = NULL,
                    digits = options_data$digits,
                    digits.perc = options_data$digits,
                    proportion.test = options_data$proportiontest,
                    palette = private$.selectPalette(mydata, dep),
                    ggplot.component = NULL,
                    results.subtitle = options_data$resultssubtitle && (is.null(exact_sub) || isTRUE(options_data$bfmessage))
                )

            if (!is.null(exact_sub))
                plot2 <- plot2 + ggplot2::labs(subtitle = exact_sub)

            plot2 <- plot2 + private$.getPlotTheme(ggtheme)


            if (isTRUE(options_data$resultssubtitle) &&
                (statsSeedMatters(options_data$typestatistics, "contingency") ||
                 (!isTRUE(options_data$paired) &&
                  captionSeedMatters(options_data$typestatistics, "contingency", 2L, options_data$bfmessage))))
                plot2 <- addSeedCaption(plot2, self, self$options$seed)

            # Print Plot2 ----
            print(plot2)
            TRUE
        },


        # the plot4 function ----




        .plot4 = function(image, ggtheme, theme, ...) {

            # The Bayesian contingency test samples its posterior, and a frequentist run with
            # the Bayes factor message draws one too: seed them and name the seed.
            withr::local_seed(self$options$seed)

            # Validation ----
            # CRITICAL FIX: Check for zero-length character vectors
            if ( is.null(self$options$dep) || length(self$options$dep) == 0 ||
                 (length(self$options$dep) == 1 && self$options$dep == "") ||
                 is.null(self$options$grvar) || length(self$options$grvar) == 0 ||
                 (length(self$options$grvar) == 1 && self$options$grvar == "") )
                return()

            # Use cached data for performance with error handling
            tryCatch({
                mydata <- private$.getCachedData()
                options_data <- private$.prepareOptions()
            }, error = function(e) {
                stop(paste0('Grouped plot preparation failed: ', conditionMessage(e)), call. = FALSE)
            })
            
            dep <- options_data$dep
            group <- options_data$group
            grvar <- options_data$grvar
            has_group <- !is.null(group) && length(group) > 0 && nzchar(group)

            if ((has_group && identical(dep, group)) || grvar %in% c(dep, if (has_group) group))
                return()

            if (!grvar %in% names(mydata))
                return()
            mydata <- mydata[!is.na(mydata[[grvar]]), , drop = FALSE]
            if (nrow(mydata) == 0)
                return()

            if ( !is.null(grvar) ) {

                selected_theme <- private$.getPlotTheme(ggtheme)

                counts_var <- if (!is.null(options_data$counts) && options_data$counts != "") {
                    options_data$counts
                } else {
                    NULL
                }
                counts_sym <- if (!is.null(counts_var)) rlang::sym(counts_var) else NULL

                # Invalid paired data: draw nothing. .emitRunNotices() reports why.
                if (options_data$paired) {
                    if (!has_group) return()
                    validation <- private$.validatePairedData(mydata, dep, group)
                    if (!validation$valid)
                        return()
                }

                override_type <- options_data$typestatistics

                # Checkpoint before expensive grouped statistical computation
                private$.checkpoint()

                # Flush before the slow Bayesian computation (no `todo` write
                # during .plot(); see .plot2).
                if (override_type == "bayes") {
                    private$.checkpoint(flush = TRUE)
                }

                gr_levels <- levels(droplevels(as.factor(mydata[[grvar]])))
                plist <- list()
                for (lv in gr_levels) {
                    sub_df <- mydata[mydata[[grvar]] == lv, , drop = FALSE]
                    if (nrow(sub_df) == 0) next
                    if (isTRUE(options_data$paired) && has_group) {
                        v_sub <- private$.validatePairedData(sub_df, dep, group)
                        if (!v_sub$valid) next
                        lev_d <- levels(droplevels(as.factor(sub_df[[dep]])))
                        lev_g <- levels(droplevels(as.factor(sub_df[[group]])))
                        shared_lev <- union(lev_d, lev_g)
                        sub_df[[dep]] <- factor(sub_df[[dep]], levels = shared_lev)
                        sub_df[[group]] <- factor(sub_df[[group]], levels = shared_lev)
                    } else {
                        sub_df[[dep]] <- droplevels(as.factor(sub_df[[dep]]))
                        if (has_group) {
                            sub_df[[group]] <- droplevels(as.factor(sub_df[[group]]))
                        }
                    }
                    if (nlevels(sub_df[[dep]]) < 1L) next
                    ratio_sub <- private$.applyRatioLengthGuard(options_data$ratio, sub_df, dep)
                    exact_sub <- if (has_group && isTRUE(options_data$resultssubtitle))
                        private$.exactSubtitle(sub_df, dep, group, counts_var,
                                               options_data$conflevel, options_data$digits) else NULL
                    p_sub <- if (has_group) {
                        p_tmp <- ggstatsplot::ggpiestats(
                            data = sub_df,
                            x = !!rlang::sym(dep),
                            y = !!rlang::sym(group),
                            counts = !!counts_sym,
                            type = override_type,
                            ratio = ratio_sub,
                            paired = options_data$paired,
                            label = options_data$label,
                            digits = options_data$digits,
                            digits.perc = options_data$digits,
                            conf.level = options_data$conflevel,
                            proportion.test = options_data$proportiontest,
                            bf.message = options_data$bfmessage,
                            results.subtitle = options_data$resultssubtitle && (is.null(exact_sub) || isTRUE(options_data$bfmessage)),
                            title = paste0(grvar, ": ", lv),
                            ggtheme = selected_theme,
                            palette = private$.selectPalette(sub_df, dep)
                        )
                        if (!is.null(exact_sub)) p_tmp <- p_tmp + ggplot2::labs(subtitle = exact_sub)
                        p_tmp
                    } else {
                        ggstatsplot::ggpiestats(
                            data = sub_df,
                            x = !!rlang::sym(dep),
                            y = NULL,
                            counts = !!counts_sym,
                            type = override_type,
                            ratio = ratio_sub,
                            paired = FALSE,
                            label = options_data$label,
                            digits = options_data$digits,
                            digits.perc = options_data$digits,
                            conf.level = options_data$conflevel,
                            proportion.test = options_data$proportiontest,
                            bf.message = options_data$bfmessage,
                            results.subtitle = options_data$resultssubtitle,
                            title = paste0(grvar, ": ", lv),
                            ggtheme = selected_theme,
                            palette = private$.selectPalette(sub_df, dep)
                        )
                    }
                    plist[[length(plist) + 1L]] <- p_sub
                }
                if (length(plist) == 0L) return()
                plot4 <- ggstatsplot::combine_plots(plotlist = plist)
            }


            if (isTRUE(options_data$resultssubtitle) &&
                (statsSeedMatters(options_data$typestatistics, "contingency") ||
                 (!isTRUE(options_data$paired) &&
                  captionSeedMatters(options_data$typestatistics, "contingency", 2L, options_data$bfmessage))))
                plot4 <- addSeedCaption(plot4, self, self$options$seed)

            # Print Plot4 ----
            print(plot4)
            TRUE
        },

        .plotDonut = function(image, ggtheme, theme, ...) {

            if (!self$options$addGGPubrDonut) return()
            # CRITICAL FIX: Check for zero-length character vectors
            if (is.null(self$options$dep) || length(self$options$dep) == 0 ||
                (length(self$options$dep) == 1 && self$options$dep == "")) return()

            # Use cached data
            tryCatch({
                mydata <- private$.getCachedData()
                options_data <- private$.prepareOptions()
            }, error = function(e) {
                stop(paste0('Donut plot preparation failed: ', conditionMessage(e)), call. = FALSE)
            })
            
            dep <- options_data$dep
            group <- options_data$group
            if (!is.null(group) && group != "" && identical(dep, group)) {
                return()
            }
            
            # Prepare data for ggdonutchart (needs summary table)
            # Handle weighted counts if present
            if (!is.null(options_data$counts) && options_data$counts != "") {
                # Weighted aggregation
                formula_str <- paste0(jmvcore::composeTerm(options_data$counts),
                                      " ~ ", jmvcore::composeTerm(dep))
                if (!is.null(group) && group != "") {
                    formula_str <- paste0(formula_str, " + ", jmvcore::composeTerm(group))
                }

                # Use xtabs to sum counts
                agg_table <- xtabs(jmvcore::asFormula(formula_str), data = mydata)
                plot_data <- as.data.frame(agg_table, responseName = "count")
                # as.data.frame.table() runs make.names() on the dimension names,
                # so "Tumor Grade" came back as "Tumor.Grade" and ggdonutchart(label =
                # dep) could not find the column. Restore the raw names; xtabs keeps
                # the formula order (dep, then group).
                names(plot_data) <- c(dep, if (!is.null(group) && group != "") group, "count")
                
            } else {
                # Unweighted aggregation
                if (!is.null(group) && group != "") {
                    plot_data <- as.data.frame(table(mydata[[dep]], mydata[[group]]))
                    names(plot_data) <- c(dep, group, "count")
                } else {
                    plot_data <- as.data.frame(table(mydata[[dep]]))
                    names(plot_data) <- c(dep, "count")
                }
            }
            
            # Filter out zero counts to avoid plotting issues
            plot_data <- plot_data[plot_data$count > 0, , drop = FALSE]
            
            # Create plot
            plot <- ggpubr::ggdonutchart(
                plot_data, 
                x = "count", 
                label = dep,
                fill = dep,
                color = "white",
                palette = options_data$ggpubrDonutPalette,
                lab.pos = "in", 
                lab.font = "white"
            )
            
            # Facet if grouped: recompute per-group cumulative label positions and
            # normalize to within-group percentages when group totals differ so
            # coord_polar(theta = "y") closes a full circle in every facet.
            if (!is.null(group) && group != "") {
                df <- plot$data
                df[[dep]] <- factor(df[[dep]], levels = levels(as.factor(plot_data[[dep]])))
                ord <- order(df[[group]], -as.integer(df[[dep]]))
                df <- df[ord, , drop = FALSE]
                grp_totals <- tapply(df$count, df[[group]], sum)
                if (length(unique(grp_totals[!is.na(grp_totals)])) > 1L) {
                    df$count <- 100 * df$count / as.numeric(grp_totals[as.character(df[[group]])])
                }
                df$.lab.ypos. <- stats::ave(df$count, df[[group]], FUN = function(v) cumsum(v) - 0.5 * v)
                plot$data <- df
                plot$layers[[1]]$data <- df
                plot <- plot + ggplot2::facet_wrap(jmvcore::asFormula(paste("~", jmvcore::composeTerm(group))))
            }
            
            print(plot)
            TRUE
        },

        .sourcifyOption = function(option) {
            if (!option$name %in% setdiff(names(formals(jjpiestats)), "data"))
                return("")
            val <- option$value
            if (is.null(val) || is.list(val) || length(val) == 0)
                return("")
            if (identical(val, option$default))
                return("")
            if (is.character(val)) {
                if (all(!nzchar(val)))
                    return("")
                escaped <- encodeString(val, quote = '"')
                val_str <- if (length(escaped) == 1L) {
                    escaped
                } else {
                    paste0("c(", paste(escaped, collapse = ", "), ")")
                }
                return(paste0(option$name, " = ", val_str))
            }
            super$.sourcifyOption(option)
        }

    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for Pie Chart Statistics analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            # Build the argument list in option-declaration order via
            # private$.sourcifyOption(), which filters against declared formals,
            # suppresses list values, and escapes all character values (including
            # variable names) with encodeString(..., quote = '"').
            args <- character(0)
            for (option in private$.options$options) {
                if (option$name == 'data')
                    next
                as <- private$.sourcifyOption(option)
                if (!identical(as, ''))
                    args <- c(args, as)
            }

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, '::jjpiestats(\n    data = data',
                   if (length(args)) paste0(',\n    ', paste(args, collapse = ',\n    ')) else '',
                   ')')
        }
    ) # End of public list
)
