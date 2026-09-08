#' @title Histogram
#' @importFrom R6 R6Class
#' @importFrom jmvcore .
#' @return An \code{R6} class generator object for the \code{jjhistostatsClass} backend; used internally by the jamovi analysis wrapper and not called directly.


jjhistostatsClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "jjhistostatsClass",
        inherit = jjhistostatsBase,
        private = list(
        # Fixed seed for the sampling-based paths. Bayesian output uses
        # BayesFactor's MCMC and robust/effect-size CIs use bootstrapping, so
        # without this the SAME analysis reported different numbers on every
        # re-render - a credible interval that moves when nothing changed.
        .STOCHASTIC_SEED = 20250101L,


        # Option overrides for clinical presets (jamovi options are read-only at runtime;
        # presets record overrides here and reads go through private$.option()).
        overrides = list(),
        .option = function(option) {
            if (option %in% names(private$overrides)) return(private$overrides[[option]])
            opt_obj <- self$options$option(option)
            if (!is.null(opt_obj)) return(opt_obj$value)
            return(NULL)
        },

            # Clinical presets record their settings in private$overrides instead of writing
            # back into the read-only jamovi option objects; reads go through private$.option().
            # (2026-07-13 audit fix.)

            # Apply clinical preset configurations
            .applyClinicalPreset = function() {
                preset <- self$options$clinicalPreset

                # Clear first. jamovi reuses the analysis object across option changes, and
                # `overrides` is instance state, so a preset's settings used to survive after
                # the user switched back to Custom: the panel showed the user's choices while
                # the analysis went on computing the preset's. Measured with
                # clinicalPreset='biomarkers' then 'custom': the options panel read
                # parametric / subtitle off, the analysis still ran nonparametric / subtitle on.
                private$overrides <- list()

                # Only apply preset if not using custom settings
                if (preset == 'custom') return()

                # Apply preset-specific configurations
                if (preset == 'lab_values') {
                    # Lab Values: parametric with centrality line
                    private$overrides[["typestatistics"]] <- 'parametric'
                    private$overrides[["centralityline"]] <- TRUE
                    private$overrides[["resultssubtitle"]] <- TRUE
                } else if (preset == 'biomarkers') {
                    # Biomarker Distribution: nonparametric, robust
                    private$overrides[["typestatistics"]] <- 'nonparametric'
                    private$overrides[["centralityline"]] <- TRUE
                    private$overrides[["resultssubtitle"]] <- TRUE
                } else if (preset == 'patient_chars') {
                    # Patient Characteristics: parametric for age, BMI
                    private$overrides[["typestatistics"]] <- 'parametric'
                    private$overrides[["centralityline"]] <- TRUE
                    private$overrides[["resultssubtitle"]] <- TRUE
                } else if (preset == 'pathology_scores') {
                    # Pathology Scores: nonparametric for ordinal data
                    private$overrides[["typestatistics"]] <- 'nonparametric'
                    private$overrides[["centralityline"]] <- TRUE
                    private$overrides[["resultssubtitle"]] <- TRUE
                }
            },

            # init ----
            .init = function() {
                private$.applyClinicalPreset()

                deplen <- length(self$options$dep)

                # Use configurable plot dimensions
                plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 600
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

                # ggpubr histogram panels are stacked vertically (ggarrange nrow = length(dep));
                # scale their height by the number of variables so multi-variable output is
                # not compressed into the fixed r.yaml height.
                if (isTRUE(self$options$addGGPubrPlot)) {
                    ggpubr_height <- if (deplen > 1) deplen * plotheight * 1.15 else plotheight
                    self$results$ggpubrPlot$setSize(plotwidth, ggpubr_height)
                    self$results$ggpubrPlot2$setSize(1200, ggpubr_height)
                }


                if (!is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                num_levels <- nlevels(
                    as.factor(mydata[[grvar]])
                )

                # For grouped analysis, calculate width based on layout
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



            },

            # Data preparation. Deliberately NOT cached: the previous digest-keyed cache
            # was side-effecting (.shouldRefreshCache() updated the stored hash on the
            # first caller of each run), so .prepareOptions()/.prepareAesthetics()
            # always saw "unchanged" and returned the FIRST run's values forever on a
            # reused R6 instance - every plot option was dead after the first render.
            # These helpers are a toNumeric loop and two list() constructions; the cost
            # of recomputing them is nil next to ggstatsplot.
            .prepareData = function() {
                # NOTE: no progress text is written to results$todo here. This helper is
                # called from .run() AFTER the To Do panel has been composed, and from the
                # render paths, so any setContent() here overwrites the user-facing content
                # with an internal progress string that then never gets rewritten.
                # Checkpoint before expensive data processing
                private$.checkpoint()

                vars <- self$options$dep

                # VALIDATE NUMERIC VARIABLES - reject factors instead of blind conversion
                factor_warnings <- character()
                for (var in vars) {
                    if (is.factor(self$data[[var]])) {
                        factor_warnings <- c(factor_warnings, var)
                    }
                }

                # Stop if any factors detected
                if (length(factor_warnings) > 0) {
                    jmvcore::reject(
                        .("Histogram analysis requires numeric variables. The following variables are categorical: {vars}. Please select continuous numeric variables for histogram analysis."),
                        vars = paste(factor_warnings, collapse = ", ")
                    )
                }

                # Get the data - ggstatsplot handles NAs internally
                mydata <- self$data

                for (v in vars) {
                    x <- jmvcore::toNumeric(mydata[[v]])
                    if (!is.numeric(x)) jmvcore::reject(.("Histogram variables must be numeric."))
                    x[!is.finite(x)] <- NA_real_
                    mydata[[v]] <- x
                }

                return(mydata)
            },

            # Shared plot generation function to eliminate duplication
            .generateHistogram = function(data, x_var, options_data, aesthetics_data, grvar_sym = NULL, ggtheme = NULL) {
                # Checkpoint before expensive statistical plot generation
                private$.checkpoint(flush = FALSE)

                # Build base arguments common to all plots
                # Note: the deprecated `messages` argument was removed - ggstatsplot >= 1.0.0
                # no longer accepts it and forwards it to geom_histogram, triggering an
                # "Ignoring unknown parameters" warning.
                base_args <- list(
                    data = data,
                    x = rlang::sym(x_var),
                    type = options_data$typestatistics,
                    results.subtitle = options_data$resultssubtitle,
                    centrality.plotting = options_data$centralityline,
                    binwidth = options_data$binwidth,
                    conf.level = options_data$conf.level,
                    alternative = options_data$alternative,
                    # `tr` (trim) and `bf.prior` were previously frozen at ggstatsplot's
                    # defaults (0.2 and 0.707) with no way to see or change them. A Bayes
                    # factor is not interpretable without its prior, and "robust" is not
                    # meaningful without the trim level, so both are now options and both
                    # are disclosed in .generateClinicalWarnings().
                    tr = options_data$trimlevel,
                    bf.prior = options_data$bfprior,
                    bf.message = options_data$bf.message,
                    digits = options_data$digits,
                    xlab = aesthetics_data$xlab,
                    title = aesthetics_data$title,
                    subtitle = aesthetics_data$subtitle,
                    caption = aesthetics_data$caption,
                    bin.args = aesthetics_data$bin.args,
                    centrality.line.args = aesthetics_data$centrality.line.args
                )

                # Conditionally add test.value only if one-sample test is enabled
                if (options_data$enableOneSampleTest) {
                    base_args$test.value <- options_data$test.value
                }

                # Add grouping variable if provided
                if (!is.null(grvar_sym)) {
                    base_args$grouping.var <- grvar_sym

                    # grouped_gghistostats() supplies `title` itself, once per group, via its
                    # internal pmap. Passing the user's title as well makes do.call() raise
                    # `formal argument "title" matched by multiple actual arguments` and the
                    # whole grouped plot fails to render. Only an empty title escaped, because
                    # the NULL-strip below removed it.
                    base_args$title <- NULL
                }

                # With several variables selected, every panel is generated by this same call
                # with a different x_var, so a single user-supplied title or x-axis label would
                # be stamped onto all of them -- three analytes all captioned with the first
                # one's name. A per-variable label is only unambiguous when there is one
                # variable; otherwise fall back to the variable's own name.
                if (length(options_data$dep) > 1) {
                    base_args$title <- NULL
                    base_args$xlab <- x_var
                }

                # Add centrality.type if specified
                if (!is.null(options_data$centrality.type)) {
                    base_args$centrality.type <- options_data$centrality.type
                }

                # Follow the host theme jamovi hands the renderer, unless the
                # user asked for ggstatsplot's own. Set after the NULL-strip
                # below would drop it, so it goes in first.
                base_args$ggtheme <- if (isTRUE(self$options$originaltheme))
                    ggstatsplot::theme_ggstatsplot() else ggtheme

                # Remove NULL arguments to prevent conflicts
                base_args <- base_args[!sapply(base_args, is.null)]

                # Checkpoint before calling expensive ggstatsplot functions
                private$.checkpoint(flush = FALSE)

                # Call appropriate function based on grouping
                if (is.null(grvar_sym)) {
                    do.call(ggstatsplot::gghistostats, base_args)
                } else {
                    do.call(ggstatsplot::grouped_gghistostats, base_args)
                }
            },
            
            # Consolidated input validation with clinical assumption checking
            .validateInputs = function() {
                # Check if dependent variables are selected
                if (is.null(self$options$dep) || length(self$options$dep) == 0) {
                    return(list(valid = FALSE, message = .("Please select at least one variable for histogram analysis.")))
                }
                
                # Check if data exists and has rows
                if (is.null(self$data) || nrow(self$data) == 0) {
                    return(list(valid = FALSE, message = .("The dataset contains no complete rows.")))
                }
                
                # Check if selected variables exist in data
                missing_vars <- self$options$dep[!self$options$dep %in% names(self$data)]
                if (length(missing_vars) > 0) {
                    return(list(valid = FALSE, message = jmvcore::format(
                        .("These selected variables are not present in the data: {vars}."),
                        vars = paste(missing_vars, collapse = ", "))))
                }
                
                # A variable with fewer than three non-missing values cannot be binned, and with
                # centrality plotting on it reaches ggstatsplot as an empty vector and dies with
                # "You must provide a model-object. Argument 'model' cannot be missing or NULL"
                # -- an upstream message that names neither the variable nor the real problem.
                # Reachable in one click, because every clinical preset forces centralityline on.
                for (v in self$options$dep) {
                    if (!v %in% names(self$data)) next
                    n_ok <- sum(is.finite(jmvcore::toNumeric(self$data[[v]])))
                    if (n_ok < 3)
                        return(list(valid = FALSE, message = if (n_ok == 1)
                            jmvcore::format(
                                .("Variable '{var}' has 1 finite numeric value. A histogram needs at least 3. Check the variable selection and any active row filters."),
                                var = v)
                        else
                            jmvcore::format(
                                .("Variable '{var}' has {n} finite numeric values. A histogram needs at least 3. Check the variable selection and any active row filters."),
                                var = v, n = n_ok)))
                }

                # Check if grouping variable exists if specified
                if (!is.null(self$options$grvar) && !self$options$grvar %in% names(self$data)) {
                    return(list(valid = FALSE, message = jmvcore::format(
                        .("The grouping variable '{var}' is not present in the data."),
                        var = self$options$grvar)))
                }

                # The same column in both boxes: jmvcore coerces grvar to a factor (its
                # `permitted` is factor), so the histogram's own x aesthetic becomes discrete
                # and stat_bin() dies with "requires a continuous x aesthetic".
                if (!is.null(self$options$grvar) && self$options$grvar %in% self$options$dep) {
                    return(list(valid = FALSE, message = jmvcore::format(
                        .("'{var}' is used both as a histogram variable and as the Split by variable. Choose a different grouping variable."),
                        var = self$options$grvar)))
                }

                # Check if binwidth is positive when manually specified
                if (self$options$changebinwidth && (!is.null(self$options$binwidth) && self$options$binwidth <= 0)) {
                    return(list(valid = FALSE, message = .("Bin width must be a positive number.")))
                }

                # A positive but tiny bin width is fully enterable in the GUI (the JS handler
                # only clamps values <= 0) and produces an EMPTY panel: ggplot2 refuses more
                # than 1,000,000 bins, and that error goes to stderr where jamovi never shows
                # it. Reject well below that ceiling with a message that names the data range
                # and a workable width.
                if (self$options$changebinwidth && !is.null(self$options$binwidth) &&
                    self$options$binwidth > 0) {
                    max_bins <- 5000
                    for (v in self$options$dep) {
                        if (!v %in% names(self$data)) next
                        x <- self$data[[v]]
                        if (!is.numeric(x)) x <- suppressWarnings(as.numeric(as.character(x)))
                        x <- x[is.finite(x)]
                        if (length(x) < 2) next
                        rng <- diff(range(x))
                        if (rng <= 0) next
                        n_bins <- rng / self$options$binwidth
                        if (n_bins > max_bins)
                            return(list(valid = FALSE, message = jmvcore::format(
                                .("A bin width of {width} would split '{var}' into {bins} bins, so the histogram would be unreadable or blank. '{var}' ranges over {range}; a bin width of about {suggested} gives roughly 30 bins."),
                                width = base::format(self$options$binwidth), var = v,
                                bins = base::format(round(n_bins), big.mark = ","),
                                range = base::format(signif(rng, 4)),
                                suggested = base::format(signif(rng / 30, 3)))))
                    }
                }

                return(list(valid = TRUE, message = NULL))
            },
            
            # Clinical assumption checking and warnings
            .generateClinicalWarnings = function(data, variables) {
                warnings <- c()

                # WARN WHENEVER THE SUBTITLE WILL REPORT A TEST AGAINST A NULL OF 0.
                #
                # The subtitle is produced by ggstatsplot whenever results.subtitle is TRUE, and
                # it is ALWAYS a one-sample location test. `test.value` is only forwarded when
                # enableOneSampleTest is on (see .generateHistogram), so with that option OFF the
                # test still runs -- against ggstatsplot's own default null of 0. Measured on
                # cholesterol ~ N(5.2, 0.9), n = 80: the subtitle reads t(79) = 58.42,
                # p = 9.14e-67, Hedges' g = 6.47, which is a true statement about a meaningless
                # null (the same data against 5.2 gives p = 0.295). The old guard fired only when
                # enableOneSampleTest was TRUE -- i.e. only in the case where the user had
                # deliberately chosen the value -- and its closing advice ("uncheck Enable
                # One-Sample Test") steered the user into exactly the silent-null state.
                show_subtitle <- isTRUE(private$.option("resultssubtitle"))
                effective_null <- if (isTRUE(self$options$enableOneSampleTest)) {
                    self$options$test.value
                } else {
                    0   # ggstatsplot's default when test.value is not forwarded
                }

                if (show_subtitle && !is.null(effective_null) && isTRUE(effective_null == 0)) {
                    # Only nag when 0 lies outside the observed data, which is the case where the
                    # test is guaranteed significant and therefore uninformative.
                    has_irrelevant_test <- FALSE
                    for (var in variables) {
                        if (!var %in% names(data)) next
                        var_data <- data[[var]][!is.na(data[[var]])]
                        if (length(var_data) > 0) {
                            if (all(var_data > 0) || all(var_data < 0)) {
                                has_irrelevant_test <- TRUE
                                break
                            }
                        }
                    }

                    if (has_irrelevant_test) {
                        chose_it <- isTRUE(self$options$enableOneSampleTest)
                        test_name <- switch(as.character(private$.option("typestatistics")),
                               parametric    = .("one-sample t-test"),
                               nonparametric = .("one-sample Wilcoxon signed-rank test"),
                               robust        = .("one-sample bootstrap-t test"),
                               bayes         = .("one-sample Bayesian t-test"),
                               .("one-sample test"))
                        warnings <- c(warnings, paste0(
                            jmvcore::format(
                                .("<strong>The statistical subtitle is testing against zero.</strong><br>The subtitle on the histogram is a <strong>{test}</strong> asking whether the centre of your data differs from <strong>0</strong>."),
                                test = test_name),
                            if (chose_it) "" else
                                " ", .("This is the default null, because 'Custom null value' is switched off so no test value is supplied."),
                            " ", .("Every value in your data lies on one side of 0, so this test is guaranteed to be significant and tells you nothing.<br><strong>What to do:</strong><br>"),
                            .("\u2022 Switch on <strong>Custom null value</strong> and set <strong>Test value</strong> to a clinically meaningful comparison - a reference-range limit, a treatment threshold, or a published population norm; or<br>"),
                            .("\u2022 Switch off <strong>Statistical results</strong> for a purely descriptive histogram. Turning off 'Custom null value' alone does <em>not</em> remove the test.")
                        ))
                    }
                }

                # METHOD DISCLOSURE. Both of these are analysis choices that change the
                # number reported in the subtitle, and both used to be invisible:
                #  - a Bayes factor cannot be interpreted or reproduced without the prior
                #    scale it was computed under (BF depends on it directly), and
                #  - "robust" says nothing about HOW robust; at the default 0.2 the top and
                #    bottom 20% of observations are discarded, which can remove a genuine
                #    high-expressing subgroup without the user ever knowing.
                if (isTRUE(private$.option("resultssubtitle"))) {
                    stat_type <- private$.option("typestatistics")
                    if (identical(stat_type, "bayes") || isTRUE(self$options$bf.message)) {
                        warnings <- c(warnings, jmvcore::format(
                            .("<strong>Bayes factor prior:</strong> the Bayes factor is computed under a Cauchy prior on effect size with scale r = {r}. A Bayes factor is only interpretable against the prior that produced it, so report this value alongside it, and vary the Bayes prior scale to check how sensitive your conclusion is to the choice."),
                            r = base::format(self$options$bfprior)))
                    }
                    if (identical(stat_type, "robust")) {
                        warnings <- c(warnings, jmvcore::format(
                            .("<strong>Robust test trim level:</strong> the robust test uses a bootstrap-t on a trimmed mean, discarding the highest and lowest {pct}% of observations ({n} of {total} rows at each tail). If a genuine high- or low-value subgroup matters clinically, that subgroup is being trimmed away - lower the trim proportion or use the nonparametric test instead."),
                            pct = base::format(100 * self$options$trimlevel),
                            n = round(self$options$trimlevel * nrow(data)), total = nrow(data)))
                    }
                    if (!identical(self$options$alternative, "two.sided")) {
                        warnings <- c(warnings, jmvcore::format(
                            .("<strong>One-sided test in use.</strong> The reported p-value tests only whether the centre is {direction} than the test value, not whether it differs in either direction. A one-sided test is only valid if the direction was chosen before seeing these data."),
                            direction = if (identical(self$options$alternative, "greater"))
                                .("greater") else .("less")))
                    }
                }

                for (var in variables) {
                    if (!var %in% names(data)) next

                    var_data <- data[[var]]
                    # is.na() is TRUE for NaN, so NaN is dropped here exactly like NA.
                    var_data <- var_data[!is.na(var_data)]

                    # A variable with nothing left to plot used to be skipped silently. The
                    # histogram still renders, but ggplot2's stat_bin() fails on an empty or
                    # zero-range vector ("'from' must be a finite number") and the panel comes
                    # out blank -- and that warning goes to stderr, which jamovi does not show.
                    # So the user is left looking at an empty plot with no explanation.
                    if (length(var_data) == 0) {
                        warnings <- c(warnings, jmvcore::format(
                            .("<strong>No data to plot for '{var}'.</strong> Every value in this variable is missing, so the histogram is empty. Check the variable selection, and any row filters that may be active."),
                            var = htmltools::htmlEscape(var)))
                        next
                    }

                    if (length(var_data) < 3) {
                        warnings <- c(warnings, if (length(var_data) == 1)
                            jmvcore::format(
                                .("<strong>Only 1 finite numeric value for '{var}'.</strong> A histogram needs a range of values to bin; the panel will be empty or near-empty, and any test statistic based on it is not interpretable."),
                                var = htmltools::htmlEscape(var))
                        else
                            jmvcore::format(
                                .("<strong>Only {n} finite numeric values for '{var}'.</strong> A histogram needs a range of values to bin; the panel will be empty or near-empty, and any test statistic based on it is not interpretable."),
                                n = length(var_data), var = htmltools::htmlEscape(var)))
                    }

                    # NORMALITY vs THE TEST THAT IS ACTUALLY RUNNING.
                    # Shapiro-Wilk was previously computed ONLY inside
                    # .generateClinicalInterpretation(), which returns early unless
                    # showInterpretation is on - and that defaults to FALSE. So the default
                    # configuration ran a one-sample t-test and reported it in the subtitle
                    # with no assumption check surfaced anywhere. Measured on a lognormal
                    # variable (n = 120, Shapiro-Wilk p = 3.4e-06): the subtitle reported the
                    # t-test and the panel said nothing at all.
                    # Only warn when a parametric test is actually being REPORTED, and only
                    # where Shapiro-Wilk is valid (3 <= n <= 5000).
                    if (isTRUE(private$.option("resultssubtitle")) &&
                        identical(private$.option("typestatistics"), "parametric") &&
                        length(var_data) >= 3 && length(var_data) <= 5000 &&
                        stats::sd(var_data) > 0) {
                        sw_p <- tryCatch(stats::shapiro.test(var_data)$p.value,
                                         error = function(e) NA_real_)
                        if (!is.na(sw_p) && sw_p <= 0.05) {
                            warnings <- c(warnings, paste0(
                                jmvcore::format(
                                    .("<strong>'{var}' is not normally distributed</strong> (Shapiro-Wilk p = {p}), but the subtitle is reporting a <strong>parametric one-sample t-test</strong>, which assumes approximate normality."),
                                    var = htmltools::htmlEscape(var),
                                    p = base::format.pval(sw_p, digits = 3, eps = 1e-16)),
                                " ",
                                if (length(var_data) >= 30)
                                    jmvcore::format(
                                        .("At n = {n} the central limit theorem makes the t-test fairly robust to this, so treat it as a caution rather than an error."),
                                        n = length(var_data))
                                else
                                    jmvcore::format(
                                        .("At n = {n} the central limit theorem does not rescue the test, so the p-value and confidence interval may be misleading."),
                                        n = length(var_data)),
                                " ", .("Switch Type of statistic to Nonparametric (Wilcoxon signed-rank) if you want a test that makes no normality assumption.")
                            ))
                        }
                    }

                    # Sample size warnings
                    if (length(var_data) < 30) {
                        warnings <- c(warnings, jmvcore::format(
                            .("Small sample size (n = {n}) for '{var}'. Consider a nonparametric analysis, or interpret the results cautiously."),
                            n = length(var_data), var = htmltools::htmlEscape(var)))
                    }
                    
                    # Extreme outlier detection (values beyond 3 MAD from median)
                    if (length(var_data) > 5) {
                        med <- median(var_data)
                        mad_val <- mad(var_data)
                        if (mad_val > 0) {
                            outliers <- sum(abs(var_data - med) > 3 * mad_val)
                            if (outliers > 0) {
                                warnings <- c(warnings, if (outliers == 1)
                                    jmvcore::format(
                                        .("Detected 1 extreme outlier in '{var}' (more than 3 MAD from the median). Consider reviewing data quality, or using the robust method."),
                                        var = htmltools::htmlEscape(var))
                                else
                                    jmvcore::format(
                                        .("Detected {n} extreme outliers in '{var}' (more than 3 MAD from the median). Consider reviewing data quality, or using the robust method."),
                                        n = outliers, var = htmltools::htmlEscape(var)))
                            }
                        }
                    }
                    
                    # Constant data warning. The histogram panel comes out BLANK in this case
                    # (stat_bin() cannot build breaks over a zero range and its error only
                    # reaches stderr), so say so rather than leaving the user to guess.
                    if (length(unique(var_data)) == 1) {
                        warnings <- c(warnings, jmvcore::format(
                            .("<strong>Variable '{var}' has constant values</strong> (every row is {value}). There is no range to bin, so the histogram panel for this variable will be empty. Check the variable selection and any active row filters."),
                            var = htmltools::htmlEscape(var),
                            value = htmltools::htmlEscape(base::format(var_data[1]))))
                    }

                    # Very few unique values warning. Excludes the constant case, which
                    # has its own dedicated message above - a constant column used to raise
                    # BOTH, the second reading "has only 1 unique values".
                    if (length(unique(var_data)) > 1 &&
                        length(unique(var_data)) < 5 && length(var_data) > 10) {
                        warnings <- c(warnings, jmvcore::format(
                            .("Variable '{var}' has only {n} unique values. Consider treating it as categorical or ordinal data."),
                            var = htmltools::htmlEscape(var), n = length(unique(var_data))))
                    }
                }
                
                # Grouped analysis warnings
                if (!is.null(self$options$grvar) && self$options$grvar %in% names(data)) {
                    group_var <- data[[self$options$grvar]]
                    group_sizes <- table(group_var, useNA = "no")
                    
                    if (any(group_sizes < 10)) {
                        small_groups <- names(group_sizes[group_sizes < 10])
                        warnings <- c(warnings, if (length(small_groups) == 1)
                            jmvcore::format(
                                .("Small group size detected: {groups} (n < 10). Results may be unreliable for this group."),
                                groups = htmltools::htmlEscape(small_groups))
                        else
                            jmvcore::format(
                                .("Small group sizes detected: {groups} (n < 10). Results may be unreliable for these groups."),
                                groups = htmltools::htmlEscape(paste(small_groups, collapse = ", "))))
                    }
                }

                # LABELS THAT ARE SILENTLY DISCARDED.
                # Three text boxes are conditionally inert (verified byte-identical renders):
                # ggstatsplot overwrites `subtitle` with the statistics expression whenever
                # Statistical Results is on, and .generateHistogram() drops `title` for the
                # grouped/multi-variable panels and replaces `xlab` with the variable name when
                # more than one variable is selected. Say so instead of dropping them silently.
                if (nzchar(self$options$subtitle) && isTRUE(private$.option("resultssubtitle"))) {
                    warnings <- c(warnings, .("<strong>Your subtitle was not used.</strong> With Statistical results switched on, the subtitle area is written by the statistical test. Switch off Statistical results to show your own subtitle."))
                }
                if (nzchar(self$options$title) &&
                    (length(self$options$dep) > 1 || !is.null(self$options$grvar))) {
                    warnings <- c(warnings, paste0(
                        if (length(self$options$dep) > 1)
                            .("<strong>Your title was not used.</strong> With more than one variable selected, each panel is titled by its own variable,")
                        else
                            .("<strong>Your title was not used.</strong> With a Split by variable, each panel is titled by its own group,"),
                        " ", .("so a single title would be stamped on all of them. Select one variable with no Split by variable to use your own title.")))
                }
                if (nzchar(self$options$xlab) && length(self$options$dep) > 1) {
                    warnings <- c(warnings, .("<strong>Your X-axis label was not used.</strong> With more than one variable selected, each panel is labelled with its own variable name."))
                }
                # The fourth member of this set. ggstatsplot writes the Bayes factor into
                # the CAPTION slot (not the subtitle) for every non-Bayesian test, so a
                # user caption is overwritten with no warning. Verified over all six
                # combinations: the caption survives unless the subtitle is on AND
                # bf.message is on AND the test is not already Bayesian - in Bayesian mode
                # the factor goes into the subtitle and the caption is left alone.
                if (nzchar(self$options$caption) &&
                    isTRUE(private$.option("resultssubtitle")) &&
                    isTRUE(self$options$bf.message) &&
                    !identical(private$.option("typestatistics"), "bayes")) {
                    warnings <- c(warnings, .("<strong>Your caption was not used.</strong> With Bayes factor switched on, the caption area is written by the Bayes factor. Switch off Bayes factor to show your own caption."))
                }

                # ANALYTIC N. ggstatsplot drops missing values silently and reports a
                # statistic computed on whatever is left, so the n behind the subtitle was
                # never stated anywhere: with 37 of 120 rows missing, nothing in the output
                # said the test ran on 83. The "Small sample size" line above only fires
                # below n = 30, so the usual clinical case - a few hundred rows with some
                # missingness - reported nothing at all.
                dropped <- character()
                for (var in variables) {
                    if (!var %in% names(data)) next
                    n_total <- length(data[[var]])
                    n_used  <- sum(!is.na(data[[var]]))
                    if (n_used < n_total)
                        dropped <- c(dropped, jmvcore::format(
                            .("{var}: n = {used} of {total} ({pct}% missing)"),
                            var = htmltools::htmlEscape(var), used = n_used, total = n_total,
                            pct = base::format(round(100 * (n_total - n_used) / n_total, 1))))
                }
                if (length(dropped) > 0)
                    warnings <- c(warnings, jmvcore::format(
                        .("<strong>Rows with missing values were excluded.</strong> Every statistic and histogram below is computed on the remaining observations - {detail}."),
                        detail = paste(dropped, collapse = "; ")))

                return(warnings)
            },

            # Generate performance warnings
            .generatePerformanceWarnings = function(data, options) {
                warnings <- c()
                
                # Bayesian analysis performance warning
                if (!is.null(options$typestatistics) && options$typestatistics == "bayes") {
                    n_rows <- nrow(data)
                    
                    if (n_rows > 1000) {
                        warnings <- c(warnings, jmvcore::format(
                            .("<strong>Slow computation:</strong> a Bayesian analysis of {n} rows may take several minutes. Consider the parametric or nonparametric method for faster results."),
                            n = n_rows))
                    } else if (n_rows > 500) {
                        warnings <- c(warnings, jmvcore::format(
                            .("<strong>Performance note:</strong> a Bayesian analysis of {n} rows may take 30 to 60 seconds. Wait for it, or switch to a faster method."),
                            n = n_rows))
                    } else {
                        warnings <- c(warnings,
                            .("<strong>Bayesian analysis:</strong> this method gives richer uncertainty quantification but takes 15 to 30 seconds to compute. The parametric or nonparametric method returns instant results."))
                    }
                    
                    # Additional Bayesian guidance
                    warnings <- c(warnings,
                        .("<strong>Faster alternatives:</strong> for similar insight with instant results, use Parametric (one-sample t-test) for roughly symmetric data, or Nonparametric (Wilcoxon signed-rank) for skewed data."))
                }
                
                # Large dataset general warning
                if (nrow(data) > 5000) {
                    warnings <- c(warnings, jmvcore::format(
                        .("<strong>Large dataset:</strong> processing {n} rows may take extra time for plot generation and statistical calculation."),
                        n = nrow(data)))
                }
                
                return(warnings)
            },
            
            # Generate clinical interpretation
            .generateClinicalInterpretation = function(data, variables) {
                if (!self$options$showInterpretation) return("")
                
                interpretation_parts <- c()
                
                for (var in variables) {
                    if (!var %in% names(data)) next
                    
                    var_data <- data[[var]]
                    var_data <- var_data[!is.na(var_data)]
                    
                    # sd() of a single value is NA, and the `if (sd_val > 0)` below would then
                    # evaluate `if (NA)` and abort the whole analysis with the bare R message
                    # "missing value where TRUE/FALSE needed". Reachable from a rare-entity
                    # cohort or an aggressive row filter, and only when this panel is enabled.
                    if (length(var_data) < 2) {
                        interpretation_parts <- c(interpretation_parts, paste0(
                            "<h4>", htmltools::htmlEscape(var), "</h4><ul><li>",
                            if (length(var_data) == 0)
                                "No non-missing values, so there is no distribution to describe."
                            else
                                "Only one non-missing value, so spread and distribution shape are undefined.",
                            "</li></ul>"))
                        next
                    }

                    # Basic descriptive statistics
                    n <- length(var_data)
                    mean_val <- mean(var_data)
                    median_val <- median(var_data)
                    sd_val <- sd(var_data)
                    
                    # Distribution shape assessment
                    # Population-moment (g1) skewness: m3 / m2^1.5. The previous
                    # sum((x-mean)^3)/(n*sd^3) form mixed an n-denominator third moment
                    # with an (n-1)-denominator sample SD, biasing the estimate.
                    skewness_val <- if (sd_val > 0) {
                        m2 <- sum((var_data - mean_val)^2) / n
                        m3 <- sum((var_data - mean_val)^3) / n
                        m3 / m2^1.5
                    } else 0
                    
                    # Normality assessment.
                    # Skewness alone cannot detect non-normality that is symmetric: a bimodal
                    # mixture (Shapiro-Wilk p = 1.8e-18) and a uniform sample (p = 2.9e-05) both
                    # have |g1| < 0.05 and were being certified as "Normal distribution allows
                    # use of parametric statistics". Shapiro-Wilk is the standard test and is
                    # valid for 3 <= n <= 5000; outside that range fall back to the skewness
                    # rule of thumb and say which was used.
                    shapiro_p <- NA_real_
                    if (n >= 3 && n <= 5000 && sd_val > 0)
                        shapiro_p <- tryCatch(stats::shapiro.test(var_data)$p.value,
                                              error = function(e) NA_real_)
                    is_normal <- if (!is.na(shapiro_p)) {
                        shapiro_p > 0.05 && abs(skewness_val) < 1
                    } else {
                        abs(skewness_val) < 0.5 && n >= 30
                    }
                    
                    # Generate interpretation
                    shape_text <- if (sd_val == 0) {
                        .("Constant - every observation has the same value, so there is no shape to describe.")
                    } else if (abs(skewness_val) >= 0.5) {
                        # Drive this bullet from the same evidence as `is_normal` below, so the
                        # three bullets cannot contradict each other. Skewness alone called a
                        # bimodal mixture (Shapiro-Wilk p = 7e-14) and a constant column
                        # "Approximately symmetric (suitable for parametric tests)" while the
                        # next two bullets said the opposite.
                        paste0(
                            if (skewness_val > 0) .("Right-skewed.") else .("Left-skewed."),
                            " ",
                            if (is_normal)
                                .("Mild - the normality check below is not rejected, so parametric tests remain reasonable.")
                            else if (skewness_val > 0)
                                .("Consider nonparametric tests, or a log transformation.")
                            else
                                .("Consider nonparametric tests."))
                    } else if (!is.na(shapiro_p) && shapiro_p <= 0.05) {
                        .("Symmetric but not normal - for example bimodal, or with heavier or lighter tails than a normal curve. Inspect the histogram before using parametric tests.")
                    } else {
                        # Must agree with `is_normal`: at n = 2 the skewness is exactly 0
                        # and Shapiro-Wilk cannot run, so the old unconditional
                        # "parametric tests are reasonable" contradicted the
                        # "Non-normal distribution" bullet three lines below.
                        paste0(.("Approximately symmetric."),
                               " ",
                               if (is_normal)
                                   .("Parametric tests are reasonable; symmetry alone does not rule out bimodality or heavy tails, so check the histogram.")
                               else
                                   jmvcore::format(
                                       .("But with n = {n} the normality check below could not be run, so symmetry here is not evidence of a normal distribution."),
                                       n = n))
                    }

                    normality_text <- if (!is.na(shapiro_p)) {
                        paste0(jmvcore::format(
                                   .("<li><strong>Normality (Shapiro-Wilk):</strong> p = {p}"),
                                   p = base::format.pval(shapiro_p, digits = 3, eps = 1e-16)),
                               " ",
                               if (shapiro_p > 0.05) .("(no evidence against normality)")
                               else .("(evidence against normality)"), "</li>")
                    } else if (sd_val == 0) {
                        paste0("<li>", .("<strong>Normality:</strong> not assessable - every value is identical."), "</li>")
                    } else {
                        paste0("<li>", jmvcore::format(
                            .("<strong>Normality:</strong> Shapiro-Wilk is not available at n = {n}; the skewness rule-of-thumb was used instead."),
                            n = n), "</li>")
                    }

                    implications_text <- if (sd_val == 0) {
                        .("This variable is constant - every observation has the same value. There is no distribution to summarise and no test to run on it.")
                    } else if (is_normal) {
                        .("No evidence of a departure from normality was found, so parametric statistics (t-tests, ANOVA) are reasonable here. Absence of evidence against normality does not establish that the distribution is normal, particularly at small n or when the Shapiro-Wilk test could not be run. Mean and SD are appropriate summary measures.")
                    } else {
                        .("Non-normal distribution: prefer rank-based methods. Within this analysis that is the Wilcoxon signed-rank option; if you go on to compare groups, the rank-based equivalents are Mann-Whitney (two groups) and Kruskal-Wallis (three or more). Median and IQR are the more appropriate summary measures here.")
                    }

                    var_interpretation <- paste0(
                        "<h4>", htmltools::htmlEscape(var), "</h4><ul>",
                        "<li>", jmvcore::format(.("<strong>Sample size:</strong> {n} observations"), n = n), "</li>",
                        "<li>", jmvcore::format(
                            .("<strong>Central tendency:</strong> mean = {mean}, median = {median}"),
                            mean = round(mean_val, 2), median = round(median_val, 2)), "</li>",
                        "<li>", jmvcore::format(.("<strong>Variability:</strong> SD = {sd}"),
                                                sd = round(sd_val, 2)), "</li>",
                        "<li><strong>", .("Distribution shape:"), "</strong> ", shape_text, "</li>",
                        normality_text,
                        "<li><strong>", .("Clinical implications:"), "</strong> ", implications_text, "</li>",
                        "</ul>"
                    )

                    interpretation_parts <- c(interpretation_parts, var_interpretation)
                }
                
                if (length(interpretation_parts) > 0) {
                    full_interpretation <- paste0(
                        "<div style='background-color: rgba(138, 155, 172, 0.06); border: 1px solid #dee2e6; padding: 15px; margin: 10px 0; color: inherit;'>",
                        "<h3>", .("Clinical Interpretation"), "</h3>",
                        "<div style='background-color: rgba(255, 202, 33, 0.23); border-left: 3px solid #ffc107; padding: 10px; margin: 10px 0; color: inherit;'>",
                        "<strong>", .("Note"), ":</strong> ",
                        .("Normality is judged by the Shapiro-Wilk test (p &gt; 0.05 and |skewness| &lt; 1) whenever it is applicable (3 to 5000 observations); outside that range the skewness <strong>rule-of-thumb</strong> (|skewness| &lt; 0.5 and n at least 30) is used instead and the bullet says so. A significant Shapiro-Wilk test on a large sample can flag departures too small to matter clinically, so read it alongside the histogram and your expert judgment rather than as a formal decision rule."),
                        "</div>",
                        paste(interpretation_parts, collapse = ""),
                        "<hr><h4>", .("Recommendations"), "</h4><ul>",
                        "<li>", .("Review distribution shapes to select appropriate statistical tests."), "</li>",
                        "<li>", .("Consider outliers and their clinical significance."), "</li>",
                        "<li>", .("For biomarker data, evaluate reference ranges and clinical cut-offs."), "</li>",
                        "<li>", .("Use a Split by variable to compare distributions between clinical subgroups."), "</li>",
                        "<li>", .("Verify normality assumptions with formal statistical tests before using parametric methods."), "</li>",
                        "</ul></div>"
                    )
                    return(full_interpretation)
                } else {
                    return("")
                }
            },
            
            .prepareAesthetics = function() {
                # Process bin.args
                bin.args <- list(
                    fill = self$options$binfill,
                    color = self$options$bincolor,
                    alpha = self$options$binalpha
                )
                
                # Process centrality.line.args
                centrality.line.args <- list(
                    color = self$options$centralitylinecolor,
                    linewidth = self$options$centralitylinewidth,
                    linetype = self$options$centralitylinetype
                )
                
                # Process text parameters
                xlab <- if (self$options$xlab != '') self$options$xlab else NULL
                title <- if (self$options$title != '') self$options$title else NULL
                subtitle <- if (self$options$subtitle != '') self$options$subtitle else NULL
                caption <- if (self$options$caption != '') self$options$caption else NULL
                
                aesthetics_list <- list(
                    bin.args = bin.args,
                    centrality.line.args = centrality.line.args,
                    xlab = xlab,
                    title = title,
                    subtitle = subtitle,
                    caption = caption
                )
                
                return(aesthetics_list)
            },


            .prepareOptions = function() {
                # NOTE: no progress text is written to results$todo here (see .prepareData).
                # Process core analysis options
                typestatistics <- private$.option("typestatistics")
                dep <- self$options$dep
                
                # Process binwidth
                binwidth <- NULL
                if (self$options$changebinwidth) {
                    binwidth <- self$options$binwidth
                }
                
                # Process centrality.type
                centrality.type <- if (self$options$centralitytype != 'default') self$options$centralitytype else NULL
                
                options_list <- list(
                    typestatistics = typestatistics,
                    dep = dep,
                    binwidth = binwidth,
                    resultssubtitle = private$.option("resultssubtitle"),
                    centralityline = private$.option("centralityline"),
                    enableOneSampleTest = self$options$enableOneSampleTest,
                    test.value = self$options$test.value,
                    conf.level = self$options$conf.level,
                    alternative = self$options$alternative,
                    trimlevel = self$options$trimlevel,
                    bfprior = self$options$bfprior,
                    bf.message = self$options$bf.message,
                    digits = self$options$digits,
                    centrality.type = centrality.type
                )
                return(options_list)
            },

            # run ----
            .run = function() {
                # Defensive: ensure clinical-preset overrides are populated even if a
                # render/run path executes before .init(). private$overrides is instance
                # state read by .prepareOptions() via private$.option();
                # .applyClinicalPreset() is idempotent.
                private$.applyClinicalPreset()

                ## Initial Message ----
                if (is.null(self$options$dep) || length(self$options$dep) == 0) {

                    ## todo ----

                    # Text is split from the HTML scaffolding so each .() holds a complete
                    # sentence a translator can work with, rather than a slab of markup.
                    todo <- paste0(
                    "<div style='background-color: rgba(138, 155, 172, 0.06); border: 1px solid #dee2e6; padding: 20px; margin: 10px 0; border-radius: 5px; color: inherit;'>",
                    "<h2 style='margin-top: 0;'>", .("Histogram Analysis"), "</h2>",
                    "<p style='font-size: 16px; margin: 15px 0;'><strong>",
                    .("Welcome to the ClinicoPath histogram tool."), "</strong><br>",
                    .("Create statistical histograms with clinical interpretation and advanced visualization options."),
                    "</p>",
                    "<div style='background-color: rgba(33, 152, 239, 0.13); border-left: 4px solid #2196f3; padding: 15px; margin: 15px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0;'>", .("Getting started"), "</h4>",
                    "<ol style='margin: 10px 0; padding-left: 20px;'>",
                    "<li>", .("<strong>Select variables:</strong> choose one or more continuous variables from the left panel."), "</li>",
                    "<li>", .("<strong>Optional grouping:</strong> add a Split by variable to compare distributions between groups."), "</li>",
                    "<li>", .("<strong>Choose an analysis type:</strong> use a clinical preset, or set the statistical options yourself."), "</li>",
                    "</ol></div>",
                    "<div style='background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; padding: 15px; margin: 15px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0;'>", .("Clinical examples"), "</h4>",
                    "<ul style='margin: 10px 0; padding-left: 20px;'>",
                    "<li>", .("<strong>Lab values:</strong> cholesterol levels, blood glucose, biomarker concentrations."), "</li>",
                    "<li>", .("<strong>Patient characteristics:</strong> age distribution, BMI, vital signs."), "</li>",
                    "<li>", .("<strong>Pathology scores:</strong> tumour grades, staging scores, severity ratings."), "</li>",
                    "<li>", .("<strong>Treatment outcomes:</strong> response measurements, survival times, quality scores."), "</li>",
                    "</ul></div>",
                    "<p style='font-size: 14px; margin: 20px 0 0 0;'><strong>", .("Documentation"), ":</strong> ",
                    "<a href='https://www.indrapatil.com/ggstatsplot/reference/gghistostats.html' target='_blank' style='color: #007bff;'>gghistostats</a> | ",
                    "<a href='https://www.indrapatil.com/ggstatsplot/reference/grouped_gghistostats.html' target='_blank' style='color: #007bff;'>grouped_gghistostats</a>",
                    "</p></div>"
                    )

                    self$results$todo$setContent(todo)

                    return()

                } else {

                    todo <- paste0("<br>", .("You have selected to make a histogram."), "<br><hr>")

                    self$results$todo$setContent(todo)

                    # Use consistent validation approach (but only for meaningful validation issues)
                    validation_result <- private$.validateInputs()
                    if (!validation_result$valid) {
                        # Only show error for actual data problems, not missing variables
                        if (!grepl("Please select at least one variable", validation_result$message)) {
                            jmvcore::reject(validation_result$message)
                        }
                    }

                    # Pre-process data, options, and aesthetics for performance
                    mydata <- private$.prepareData()
                    private$.prepareOptions()
                    private$.prepareAesthetics()
                    
                    # Generate clinical warnings and performance warnings
                    warnings <- private$.generateClinicalWarnings(mydata, self$options$dep)
                    # Use processed options so clinical-preset overrides to typestatistics
                    # are honored here (consistent with all other reads via private$.option()).
                    performance_warnings <- private$.generatePerformanceWarnings(mydata, private$.prepareOptions())
                    
                    all_warnings <- c(warnings, performance_warnings)

                    # SEVERITY ORDER. These were rendered in the order the generators happened
                    # to append them, so "The statistical subtitle is testing against zero" -
                    # which invalidates the headline statistic - could sit below "Detected 3
                    # extreme outlier(s)". Rank by how much the message undermines the result,
                    # keeping the original order within each band (a stable sort).
                    severity <- function(w) {
                        if (grepl(paste0("testing against zero|is not normally distributed|",
                                         "No data to plot|constant values|One-sided test in use"),
                                  w, fixed = FALSE)) 1L
                        else if (grepl("Bayes factor prior|Robust test trim level", w)) 2L
                        else if (grepl("Only [0-9]+ finite|Small sample size|Small group size|were excluded",
                                       w)) 2L
                        else if (grepl("extreme outlier|unique values|was not used", w)) 3L
                        else 4L
                    }
                    all_warnings <- all_warnings[order(vapply(all_warnings, severity, integer(1)),
                                                       seq_along(all_warnings))]

                    if (length(all_warnings) > 0) {
                        warning_text <- paste(all_warnings, collapse = "<br>")
                        todo <- paste0(
                            "<br>", .("You have selected to make a histogram."), "<br><hr>",
                            "<div style='background-color: rgba(255, 202, 33, 0.23); border: 1px solid #ffeaa7; padding: 10px; margin: 5px 0; color: inherit;'>",
                            "<strong>", .("Clinical and performance considerations"), "</strong><br>",
                            warning_text,
                            "</div><hr>"
                        )
                        self$results$todo$setContent(todo)
                    }
                    
                    # Generate and populate clinical interpretation
                    if (self$options$showInterpretation) {
                        interpretation_content <- private$.generateClinicalInterpretation(mydata, self$options$dep)
                        if (interpretation_content != "") {
                            self$results$interpretation$setContent(interpretation_content)
                        }
                    }

                }
            }

            ,
            .plot = function(image, ggtheme, theme, ...) {
                # Seed the sampling-based paths (Bayesian MCMC, bootstrap CIs) so a
                # re-render of an unchanged analysis reports the same numbers.
                withr::local_seed(private$.STOCHASTIC_SEED)

                # Main plot generation function
                # Defensive: repopulate clinical-preset overrides in case this render
                # path runs without a prior .init() (idempotent).
                private$.applyClinicalPreset()

                # Early return if no variables selected (don't show error)
                if (is.null(self$options$dep) || length(self$options$dep) == 0) {
                    return()
                }
                
                # Validate inputs using shared helper
                validation_result <- private$.validateInputs()
                if (!validation_result$valid) {
                    if (!is.null(validation_result$message)) {
                        jmvcore::reject(validation_result$message)
                    }
                    return()
                }

                # Checkpoint before data preparation and plot generation
                private$.checkpoint()

                # Use cached data, options, and aesthetics for performance
                mydata <- private$.prepareData()
                options_data <- private$.prepareOptions()
                aesthetics_data <- private$.prepareAesthetics()

                dep <- options_data$dep

                # Single variable plot
                if (length(self$options$dep) == 1) {
                    plot <- private$.generateHistogram(
                        data = mydata,
                        x_var = dep,
                        options_data = options_data,
                        aesthetics_data = aesthetics_data,
                        ggtheme = ggtheme
                    )
                }

                # Multiple variable plots
                if (length(self$options$dep) > 1) {
                    dep2 <- as.list(self$options$dep)

                    # Checkpoint before expensive loop over multiple variables
                    private$.checkpoint()

                    plotlist <- purrr::map(
                        dep2,
                        function(x_var) {
                            # Each iteration processes a different variable - checkpoint for responsiveness
                            private$.checkpoint(flush = FALSE)
                            private$.generateHistogram(
                                data = mydata,
                                x_var = x_var,
                                options_data = options_data,
                                aesthetics_data = aesthetics_data,
                                ggtheme = ggtheme
                            )
                        }
                    )

                    # Checkpoint before expensive plot combination
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

                # Print plot
                print(plot)
                TRUE
            }


            ,
            .plot2 = function(image, ggtheme, theme, ...) {
                # Seed the sampling-based paths (Bayesian MCMC, bootstrap CIs) so a
                # re-render of an unchanged analysis reports the same numbers.
                withr::local_seed(private$.STOCHASTIC_SEED)

                # Grouped plot generation function
                # Defensive: repopulate clinical-preset overrides in case this render
                # path runs without a prior .init() (idempotent).
                private$.applyClinicalPreset()

                # Early return if no variables selected (don't show error)
                if (is.null(self$options$dep) || length(self$options$dep) == 0) {
                    return()
                }
                
                # Check for required grouping variable
                if (is.null(self$options$grvar))
                    return()
                    
                # Validate inputs
                validation_result <- private$.validateInputs()
                if (!validation_result$valid) {
                    if (!is.null(validation_result$message)) {
                        jmvcore::reject(validation_result$message)
                    }
                    return()
                }

                # Checkpoint before data preparation and grouped plot generation
                private$.checkpoint()

                # Use cached data, options, and aesthetics for performance
                mydata <- private$.prepareData()
                options_data <- private$.prepareOptions()
                aesthetics_data <- private$.prepareAesthetics()
                
                dep <- options_data$dep
                grvar <- self$options$grvar

                # Single variable grouped plot
                if (length(self$options$dep) == 1) {
                    plot2 <- private$.generateHistogram(
                        data = mydata,
                        x_var = dep,
                        options_data = options_data,
                        aesthetics_data = aesthetics_data,
                        grvar_sym = rlang::sym(grvar),
                        ggtheme = ggtheme
                    )
                }

                # Multiple variable grouped plots
                if (length(self$options$dep) > 1) {
                    dep2 <- as.list(self$options$dep)

                    # Checkpoint before expensive loop over multiple grouped variables
                    private$.checkpoint()

                    plotlist <- purrr::map(
                        dep2,
                        function(x_var) {
                            # Each iteration processes a different grouped variable - checkpoint for responsiveness
                            private$.checkpoint(flush = FALSE)
                            private$.generateHistogram(
                                data = mydata,
                                x_var = x_var,
                                options_data = options_data,
                                aesthetics_data = aesthetics_data,
                                grvar_sym = rlang::sym(grvar),
                                ggtheme = ggtheme
                            )
                        }
                    )

                    # Checkpoint before expensive grouped plot combination
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

                # Print plot
                print(plot2)
                TRUE
            }

            ,
            .plotGGPubr = function(image, ...) {
                # Validate inputs
                if (is.null(self$options$dep))
                    return()

                # Skip if ggpubr plot not requested
                if (!self$options$addGGPubrPlot)
                    return()

                # Renderers can run without a fresh .run() (window resize, reopening a
                # saved .omv), so they cannot rely on .run() having rejected bad input first.
                if (!private$.validateInputs()$valid)
                    return()

                # Prepare data
                mydata <- self$data
                dep <- self$options$dep

                # Single variable
                if (length(dep) == 1) {
                    # Build histogram arguments
                    args <- list(
                        data = mydata,
                        x = dep,
                        fill = self$options$ggpubrPalette,
                        add_density = self$options$ggpubrAddDensity
                    )

                    # Create histogram
                    plot <- do.call(ggpubr::gghistogram, args)

                    # Add mean line if requested
                    if (self$options$ggpubrAddMean) {
                        mean_val <- mean(mydata[[dep]], na.rm = TRUE)
                        plot <- plot + ggplot2::geom_vline(xintercept = mean_val,
                                                          color = "red",
                                                          linetype = "dashed",
                                                          linewidth = 1)
                    }

                    # Apply theme
                    plot <- plot + ggpubr::theme_pubr()

                    print(plot)
                }

                # Multiple variables
                if (length(dep) > 1) {
                    dep_list <- as.list(dep)

                    plotlist <- lapply(dep_list, function(depvar) {
                        args <- list(
                            data = mydata,
                            x = depvar,
                            fill = self$options$ggpubrPalette,
                            add_density = self$options$ggpubrAddDensity,
                            title = depvar
                        )

                        p <- do.call(ggpubr::gghistogram, args)

                        if (self$options$ggpubrAddMean) {
                            mean_val <- mean(mydata[[depvar]], na.rm = TRUE)
                            p <- p + ggplot2::geom_vline(xintercept = mean_val,
                                                        color = "red",
                                                        linetype = "dashed",
                                                        linewidth = 1)
                        }

                        p <- p + ggpubr::theme_pubr()
                        return(p)
                    })

                    plot <- ggpubr::ggarrange(plotlist = plotlist, ncol = 1, nrow = length(dep))
                    print(plot)
                }

                TRUE
            }

            ,
            .plotGGPubr2 = function(image, ...) {
                # Validate inputs
                if (is.null(self$options$dep) || is.null(self$options$grvar))
                    return()

                # Skip if ggpubr plot not requested
                if (!self$options$addGGPubrPlot)
                    return()

                # Renderers can run without a fresh .run() (window resize, reopening a
                # saved .omv), so they cannot rely on .run() having rejected bad input first.
                if (!private$.validateInputs()$valid)
                    return()

                # Prepare data
                mydata <- self$data
                dep <- self$options$dep
                grvar <- self$options$grvar

                # Single variable with faceting
                if (length(dep) == 1) {
                    args <- list(
                        data = mydata,
                        x = dep,
                        fill = self$options$ggpubrPalette,
                        add_density = self$options$ggpubrAddDensity,
                        facet.by = grvar
                    )

                    plot <- do.call(ggpubr::gghistogram, args)

                    if (self$options$ggpubrAddMean) {
                        mean_val <- mean(mydata[[dep]], na.rm = TRUE)
                        plot <- plot + ggplot2::geom_vline(xintercept = mean_val,
                                                          color = "red",
                                                          linetype = "dashed",
                                                          linewidth = 1)
                    }

                    plot <- plot + ggpubr::theme_pubr()
                    print(plot)
                }

                # Multiple variables with faceting
                if (length(dep) > 1) {
                    dep_list <- as.list(dep)

                    plotlist <- lapply(dep_list, function(depvar) {
                        args <- list(
                            data = mydata,
                            x = depvar,
                            fill = self$options$ggpubrPalette,
                            add_density = self$options$ggpubrAddDensity,
                            facet.by = grvar,
                            title = depvar
                        )

                        p <- do.call(ggpubr::gghistogram, args)

                        if (self$options$ggpubrAddMean) {
                            mean_val <- mean(mydata[[depvar]], na.rm = TRUE)
                            p <- p + ggplot2::geom_vline(xintercept = mean_val,
                                                        color = "red",
                                                        linetype = "dashed",
                                                        linewidth = 1)
                        }

                        p <- p + ggpubr::theme_pubr()
                        return(p)
                    })

                    plot <- ggpubr::ggarrange(plotlist = plotlist, ncol = 1, nrow = length(dep))
                    print(plot)
                }

                TRUE
            }

            ,
            .plotDensity = function(image, ...) {
                if (is.null(self$options$dep) || !self$options$addDistributionDiagnostics)
                    return()

                # Renderers can run without a fresh .run() (window resize, reopening a
                # saved .omv), so they cannot rely on .run() having rejected bad input first.
                if (!private$.validateInputs()$valid)
                    return()

                mydata <- self$data
                dep <- self$options$dep

                # These panels are the analysis's only normality diagnostics, and the
                # single-variable gate below used to have no `else`: with two or more variables
                # selected the function returned TRUE having drawn nothing, so jamovi showed an
                # empty box titled "QQ Plot - Normality Assessment". A blank normality panel
                # reads as "checked, nothing to report", which is the opposite of the truth.
                # Panels are stacked with ggarrange, as the ggpubr histogram path above does.
                build <- function(v) ggpubr::ggdensity(
                    mydata, x = v, fill = self$options$ggpubrDensityColor,
                    add = "mean", rug = TRUE, title = if (length(dep) > 1) v else NULL
                ) + ggpubr::theme_pubr()

                if (length(dep) == 1) {
                    print(build(dep))
                } else {
                    print(ggpubr::ggarrange(plotlist = lapply(dep, build),
                                            ncol = 1, nrow = length(dep)))
                }

                TRUE
            }

            ,
            .plotQQ = function(image, ...) {
                if (is.null(self$options$dep) || !self$options$addDistributionDiagnostics || !self$options$ggpubrShowQQ)
                    return()

                # Renderers can run without a fresh .run() (window resize, reopening a
                # saved .omv), so they cannot rely on .run() having rejected bad input first.
                if (!private$.validateInputs()$valid)
                    return()

                mydata <- self$data
                dep <- self$options$dep

                # These panels are the analysis's only normality diagnostics, and the
                # single-variable gate below used to have no `else`: with two or more variables
                # selected the function returned TRUE having drawn nothing, so jamovi showed an
                # empty box titled "QQ Plot - Normality Assessment". A blank normality panel
                # reads as "checked, nothing to report", which is the opposite of the truth.
                # Panels are stacked with ggarrange, as the ggpubr histogram path above does.
                build <- function(v) ggpubr::ggqqplot(
                    mydata, x = v, color = self$options$ggpubrDensityColor,
                    title = if (length(dep) > 1) v else NULL
                ) + ggpubr::theme_pubr()

                if (length(dep) == 1) {
                    print(build(dep))
                } else {
                    print(ggpubr::ggarrange(plotlist = lapply(dep, build),
                                            ncol = 1, nrow = length(dep)))
                }

                TRUE
            }

            ,
            .plotECDF = function(image, ...) {
                if (is.null(self$options$dep) || !self$options$addDistributionDiagnostics || !self$options$ggpubrShowECDF)
                    return()

                # Renderers can run without a fresh .run() (window resize, reopening a
                # saved .omv), so they cannot rely on .run() having rejected bad input first.
                if (!private$.validateInputs()$valid)
                    return()

                mydata <- self$data
                dep <- self$options$dep

                # These panels are the analysis's only normality diagnostics, and the
                # single-variable gate below used to have no `else`: with two or more variables
                # selected the function returned TRUE having drawn nothing, so jamovi showed an
                # empty box titled "QQ Plot - Normality Assessment". A blank normality panel
                # reads as "checked, nothing to report", which is the opposite of the truth.
                # Panels are stacked with ggarrange, as the ggpubr histogram path above does.
                build <- function(v) ggpubr::ggecdf(
                    mydata, x = v, color = self$options$ggpubrDensityColor,
                    title = if (length(dep) > 1) v else NULL
                ) + ggpubr::theme_pubr()

                if (length(dep) == 1) {
                    print(build(dep))
                } else {
                    print(ggpubr::ggarrange(plotlist = lapply(dep, build),
                                            ncol = 1, nrow = length(dep)))
                }

                TRUE
            }
        ), # End of private list
        public = list(
            #' @description
            #' Generate R source code for Histogram Statistics analysis
            #' @return Character string with R syntax for reproducible analysis
            asSource = function() {
                dep <- self$options$dep

                if (is.null(dep) || length(dep) == 0)
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
                paste0(pkg_name, '::jjhistostats(\n    data = data,\n    ',
                       paste(args, collapse = ',\n    '), ')')
            }
        ) # End of public list
    )
