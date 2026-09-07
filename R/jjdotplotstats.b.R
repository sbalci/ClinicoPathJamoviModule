#' @title Horizontal Box-Violin Comparison
#' @importFrom R6 R6Class
#' @importFrom rlang sym
#' @importFrom digest digest
#'
#' @return An \code{R6} class generator object for the \code{jjdotplotstatsClass} backend; used internally by the jamovi analysis wrapper and not called directly.


jjdotplotstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjdotplotstatsClass",
    inherit = jjdotplotstatsBase,
    private = list(
        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .processedOptions = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .messages = NULL,
        .noticesList = NULL,
        # Cached result of .validateInputs(); computed once per .run() so the
        # render paths (.plot / .plot2) gate silently instead of re-validating.
        .inputsValid = FALSE,
        # Set by .subtitleExpr() when the statsExpressions takeover could not be
        # used, so .run() can say that the effect-size selection was ignored.
        .subtitleFallback = NULL,
        # Subtitle expression computed in .run() (see .subtitleExpr) and read by
        # .plot(). Computing it during rendering was useless for reporting: any
        # notice raised there is discarded, because jamovi has already composed
        # the results panel by the time a figure is drawn.
        .subtitleCache = NULL,
        # Bayes-factor caption expression computed in .run() (see .captionExpr)
        # and read by .plot(), for the same reason as .subtitleCache.
        .captionCache = NULL,
        # Why bfmessage could not be honoured, so .run() can say so.
        .captionUnavailable = NULL,
        # Fallback for the user-facing "Random seed" option.
        #
        # statsExpressions bootstraps the effect-size CI for the nonparametric
        # and robust families and never seeds it, and ggstatsplot jitters the
        # points from the same stream. Measured on identical data, type =
        # "robust", five runs: the reported 95% CI moved across [0.04, 0.30] to
        # [0.06, 0.36]. A figure re-rendered for a report has to come back the
        # same, so pin the stream (withr restores the caller's on exit).
        #
        # It is an option rather than a constant because it moves a number the
        # user reports: someone looking at a borderline interval has to be able
        # to re-run on a different resample and see whether it holds.
        .defaultSeed = 42L,

        .seed = function() {
            s <- self$options$seed
            if (is.null(s) || !is.finite(s)) private$.defaultSeed else as.integer(s)
        },
        # Rows dropped by .prepareData() for holding Inf/-Inf, reported separately
        # from ordinary missingness because a non-finite measurement signals a
        # data-entry or divide-by-zero problem rather than an absent observation.
        .nonFiniteDropped = 0L,
        # Messages produced by .prepareData(), kept so a cache hit can re-emit
        # them (see .prepareData).
        .data_messages = NULL,

        # Notice accumulation system (HTML-based, avoids serialization issues)
        .addNotice = function(message, type = "INFO") {
            if (is.null(private$.noticesList)) {
                private$.noticesList <- list()
            }

            # Determine styling based on type
            style_info <- switch(type,
                "ERROR" = list(
                    color = "#721c24",
                    bg = "#f8d7da",
                    border = "#f5c6cb",
                    icon = ""
                ),
                "STRONG_WARNING" = list(
                    color = "#856404",
                    bg = "#fff3cd",
                    border = "#ffeaa7",
                    icon = ""
                ),
                "WARNING" = list(
                    color = "#856404",
                    bg = "#fff3cd",
                    border = "#ffeaa7",
                    icon = ""
                ),
                "INFO" = list(
                    color = "#004085",
                    bg = "#cce5ff",
                    border = "#b8daff",
                    icon = ""
                ),
                # Default
                list(
                    color = "#004085",
                    bg = "#cce5ff",
                    border = "#b8daff",
                    icon = ""
                )
            )

            # The severity word is user-visible, so it is translated here
            # rather than leaking the internal enum name into the panel.
            type_label <- switch(type,
                ERROR          = .("Error"),
                STRONG_WARNING = .("Important"),
                WARNING        = .("Warning"),
                INFO           = .("Note"),
                type)

            # data-severity carries the untranslated enum so tests and any
            # downstream tooling can identify severity without scraping a word
            # that changes with the interface language.
            notice_html <- paste0(
                "<div data-severity='", type, "' style='background-color: ", style_info$bg,
                "; border-left: 4px solid ", style_info$border,
                "; padding: 12px; margin: 8px 0; color: ", style_info$color, ";'>",
                "<strong>", type_label, ":</strong> ", message,
                "</div>")

            private$.noticesList <- c(private$.noticesList,
                                     list(list(type = type, html = notice_html)))
            private$.renderNotices()
        },

        # Render in severity order, not call order.
        #
        # The list is appended to as validation walks its checks, and those
        # checks are not ordered by severity: .validateInputs() raises the two
        # sample-size STRONG_WARNINGs before the constant-variable ERROR, so a
        # constant dependent variable in a small sample used to publish
        # "STRONG_WARNING, STRONG_WARNING, ERROR" and bury the one message that
        # says the analysis did not run. Rank here instead of reordering the
        # checks, so any notice added later lands in the right band for free.
        .renderNotices = function() {
            if (is.null(private$.noticesList) || length(private$.noticesList) == 0) {
                return()
            }

            rank <- c(ERROR = 1L, STRONG_WARNING = 2L, WARNING = 3L, INFO = 4L)
            ord  <- vapply(private$.noticesList,
                           function(n) unname(rank[n$type]), integer(1))
            ord[is.na(ord)] <- 4L
            notices_html <- paste(
                vapply(private$.noticesList[order(ord)], function(n) n$html, ""),
                collapse = "\n")
            self$results$notices$setContent(notices_html)
        },

        .clearNotices = function() {
            private$.noticesList <- NULL
            self$results$notices$setContent("")
        },

        # init ----

        .init = function() {
            # Since dep is single variable, use fixed size
            # Use configurable plot dimensions
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 650
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450
            
            self$results$plot$setSize(plotwidth, plotheight)


            if (!is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                num_levels <- nlevels(
                    droplevels(as.factor(mydata[[grvar]]))
                )

                # num_levels * plotwidth is unbounded: a Split By variable with
                # 10 levels asked for a 6500-pixel canvas. Cap the total and let
                # the panels narrow instead, and use droplevels() so empty
                # levels of a filtered factor do not reserve space for panels
                # that are never drawn.
                self$results$plot2$setSize(
                    min(max(num_levels, 1L) * plotwidth, 3000L), plotheight)

            }

        }


,
        # Shared validation helper
        .validateInputs = function() {
            if (is.null(self$options$dep) || is.null(self$options$group))
                return(FALSE)

            if (nrow(self$data) == 0) {
                private$.addNotice(.("Data contains no complete rows. Please check for missing values in your selected variables."), "ERROR")
                return(FALSE)
            }

            # Check variable existence with better context
            if (!(self$options$dep %in% names(self$data))) {
                available_vars <- htmltools::htmlEscape(paste(names(self$data), collapse=", "))
                private$.addNotice(jmvcore::format(
                    .("Variable '{name}' was not found in the data. Available variables are: {available}. Please select a valid continuous variable for the dependent variable."),
                    name = htmltools::htmlEscape(self$options$dep), available = available_vars), "ERROR")
                return(FALSE)
            }

            if (!(self$options$group %in% names(self$data))) {
                available_vars <- htmltools::htmlEscape(paste(names(self$data), collapse=", "))
                private$.addNotice(jmvcore::format(
                    .("Variable '{name}' was not found in the data. Available variables are: {available}. Please select a valid grouping variable."),
                    name = htmltools::htmlEscape(self$options$group), available = available_vars), "ERROR")
                return(FALSE)
            }

            # Require at least two groups with complete data
            relevant_cols <- c(self$options$dep, self$options$group)
            if (!is.null(self$options$grvar))
                relevant_cols <- c(relevant_cols, self$options$grvar)
            complete_rows <- complete.cases(self$data[relevant_cols])
            group_levels <- nlevels(droplevels(as.factor(self$data[[self$options$group]][complete_rows])))
            if (group_levels < 2) {
                private$.addNotice(jmvcore::format(
                    .("At least two groups with data are required for a comparison. Groups found with complete data: {found}. Please check for missing values or select different variables."),
                    found = group_levels), "ERROR")
                return(FALSE)
            }

            # No confidence-level guard here. The .a.yaml constrains conflevel
            # to [0.5, 0.999] and jmvcore rejects anything outside that before
            # .run() is ever entered ("conflevel must be between 0.5 and 0.999
            # (is 1)"), from the GUI and from the R wrapper alike, so the check
            # that used to sit here could not fire and only read as protection
            # that was not there.

            # Check total sample size
            n_total <- sum(complete_rows)
            if (n_total < 30) {
                private$.addNotice(jmvcore::format(
                    .("Small total sample size (N = {n}). Statistical tests may be unreliable below 30 observations. Interpret the result cautiously or collect more data."),
                    n = n_total), "STRONG_WARNING")
            }

            # Check minimum group size
            group_data <- self$data[[self$options$group]][complete_rows]
            group_sizes <- table(droplevels(as.factor(group_data)))
            min_group_n <- min(group_sizes)
            if (min_group_n < 10) {
                min_group_name <- names(which.min(group_sizes))
                private$.addNotice(jmvcore::format(
                    .("Very small group sizes detected: the smallest is n = {n} in group '{group}'. Groups below 10 observations may produce unreliable test results. Consider combining groups or collecting more data."),
                    n = min_group_n, group = htmltools::htmlEscape(min_group_name)), "STRONG_WARNING")
            }

            # A dependent variable with no spread makes ggstatsplot die inside its
            # own layout code ("arguments imply differing number of rows: 0, 1")
            # and hand back an EMPTY plot box. Catching it here, in .run(), is the
            # difference between an actionable message and a blank figure - the
            # error raised at render time cannot reach the results panel, which
            # jamovi has already composed by then.
            dep_vals <- jmvcore::toNumeric(self$data[[self$options$dep]][complete_rows])
            dep_vals <- dep_vals[is.finite(dep_vals)]
            if (length(dep_vals) > 0 && length(unique(dep_vals)) < 2) {
                private$.addNotice(jmvcore::format(
                    .("'{name}' takes the same value ({value}) in every row, so there is no variation to compare between groups. Check that the correct variable is selected."),
                    name = htmltools::htmlEscape(self$options$dep),
                    value = base::format(dep_vals[1])), "ERROR")
                return(FALSE)
            }

            # Validate centrality parameter consistency
            private$.validateCentralityOptions()

            return(TRUE)
        },
        
        # Centrality parameter validation helper
        .validateCentralityOptions = function() {
            # centralityk was a ggdotplotstats argument (centrality.k) that no
            # longer exists anywhere in ggstatsplot 1.0.0. Verified by rendering:
            # centralityk = 0 and centralityk = 5 both label the means 9.92 /
            # 13.16 / 11.10, while the "Statistical Precision" box (k) does move
            # them (k = 0 gives 10 / 13 / 11). The control is removed from the
            # UI; R-API callers who still pass it get told where the real knob is.
            if (self$options$centralityk != 2) {
                private$.addNotice(.("'Central Tendency Precision' no longer has any effect - the statistics package dropped that setting. Use 'Statistical Precision (Decimal Places)' instead; it controls the centrality labels too."), "INFO")
            }

            if (self$options$centralityplotting && self$options$centralityparameter == "none") {
                private$.addNotice(.("Centrality plotting is enabled but the centrality parameter is set to none, so no centrality lines will be displayed."), "WARNING")
            }

            if (!self$options$centralityplotting && self$options$centralitytype != "parametric") {
                private$.addNotice(.("A centrality type is selected but centrality plotting is switched off, so the type setting has no effect."), "INFO")
            }
        },
        
        # Restore base R's as.character() for formulas while `expr` runs.
        #
        # This is not defensive programming, it is a live bug. `logistf` - a
        # runtime dependency of the odds-ratio analysis, and therefore in this
        # package's Imports - pulls in `formula.tools`, whose
        # as.character.formula returns ONE deparsed string ("v ~ g") where base R
        # returns c("~", "v", "g"). stats::oneway.test does
        #     dp <- as.character(formula)
        # and rejects anything of length != 3 with "a two-sided formula is
        # required", so merely loading ClinicoPath breaks Welch's ANOVA for the
        # whole R session.
        #
        # Measured consequence: with ClinicoPath loaded,
        # ggstatsplot::ggbetweenstats() on three groups returns subtitle = NULL.
        # The user ticks "Statistical results in plot" and gets a figure with no
        # statistics on it and no warning that anything failed.
        #
        # The S3 methods table is an ordinary unlocked environment, so swap the
        # method for the duration of the call and put it back on exit.
        # Single implementation lives in R/ggstatsplot_utils.R.
        .withBaseFormulaChar = function(expr) {
            withBaseFormulaChar(expr)
        },

        # How many group levels actually carry data.
        .nGroupLevels = function(data, group_var) {
            if (is.null(group_var) || !group_var %in% names(data)) return(NA_integer_)
            nlevels(droplevels(as.factor(data[[group_var]])))
        },

        # Build the plot subtitle ourselves.
        #
        # ggstatsplot 1.0.0 dropped `effsize.type` from ggbetweenstats: it is
        # swallowed by `...`, so the four-way "Effect Size Measure" selector was
        # completely inert. Measured on three groups of 40 - Cohen's d, Hedge's
        # g, eta-squared and omega-squared all returned the identical subtitle
        # reporting omega-squared (0.37). statsExpressions, which ggstatsplot
        # calls internally, still honours the argument, so compute the
        # expression here and hand it to the plot with results.subtitle = FALSE.
        #
        # Returns NULL when the takeover cannot be used, in which case
        # ggstatsplot produces its own subtitle:
        #   - subtitles switched off
        #   - Bayesian type (statsExpressions errors on this combination)
        #   - fewer than two groups with data
        .subtitleExpr = function(data, group_var, dep_var, opts) {
            if (!isTRUE(opts$resultssubtitle)) return(NULL)
            if (identical(opts$typestatistics, "bayes")) return(NULL)

            n_lev <- private$.nGroupLevels(data, group_var)
            if (is.na(n_lev) || n_lev < 2) return(NULL)

            # two_sample_test rejects "eta"/"omega" (they are ANOVA-only); map
            # them onto the equivalent two-group family so the selector still
            # means something when there are exactly two groups.
            eff <- opts$effsizetype
            if (n_lev == 2 && identical(eff, "eta"))   eff <- "biased"
            if (n_lev == 2 && identical(eff, "omega")) eff <- "unbiased"

            fn <- if (n_lev == 2) statsExpressions::two_sample_test
                  else            statsExpressions::oneway_anova

            res <- tryCatch(
                withr::with_seed(private$.seed(),
                    private$.withBaseFormulaChar(rlang::inject(fn(
                        data         = data,
                        x            = !!rlang::sym(group_var),
                        y            = !!rlang::sym(dep_var),
                        type         = opts$typestatistics,
                        effsize.type = eff,
                        digits       = opts$digits,
                        conf.level   = opts$conflevel)))),
                error = function(e) e)

            # Not hypothetical: `formula.tools` registers an
            # `as.character.formula` returning one deparsed string where base R
            # returns c("~", "y", "g"), which makes stats::oneway.test reject
            # every valid formula with "a two-sided formula is required". It
            # arrives transitively via logistf, so any session that has run a
            # Firth regression loses the three-or-more-group takeover. Falling
            # back to ggstatsplot's subtitle is safe but silently ignores the
            # effect-size choice, so record it and say so in .run().
            if (inherits(res, "condition")) {
                private$.subtitleFallback <- conditionMessage(res)
                return(NULL)
            }
            if (is.null(res$expression) || length(res$expression) == 0) {
                private$.subtitleFallback <- "the statistics engine returned no expression"
                return(NULL)
            }
            res$expression[[1]]
        },

        # Build the Bayes-factor caption ourselves, for the same reason as the
        # subtitle.
        #
        # ggstatsplot reads bf.message ONLY inside `if (results.subtitle)`, and
        # the subtitle takeover above has to switch that off to make the effect
        # size selector work. So ticking "Bayes factor interpretation" produced
        # nothing at all on this figure, in every configuration - measured.
        # Upstream builds the caption by re-running the same test with
        # type = "bayes", so do exactly that.
        #
        # Returns NULL, and records why, whenever the caption cannot exist:
        #   - the box is unticked, or subtitles are off (nothing to caption)
        #   - the test is already Bayesian (the subtitle IS the Bayes factor)
        #   - three or more groups: ggstatsplot gates the caption on
        #     type == "parametric", and statsExpressions::oneway_anova(type =
        #     "bayes") errors out anyway ("you seem to have stumbled on some
        #     weird edge case"), which is why upstream also renders nothing here
        .captionExpr = function(data, group_var, dep_var, opts) {
            private$.captionUnavailable <- NULL
            if (!isTRUE(opts$bfmessage)) return(NULL)
            if (!isTRUE(opts$resultssubtitle)) {
                private$.captionUnavailable <- "the plot is not showing statistical results at all"
                return(NULL)
            }
            if (identical(opts$typestatistics, "bayes")) {
                private$.captionUnavailable <- "the test is already Bayesian, so the subtitle itself reports the Bayes factor"
                return(NULL)
            }
            if (!identical(opts$typestatistics, "parametric")) {
                private$.captionUnavailable <- "a Bayes factor is only defined here for the parametric test"
                return(NULL)
            }

            n_lev <- private$.nGroupLevels(data, group_var)
            if (is.na(n_lev) || n_lev != 2) {
                private$.captionUnavailable <- "the statistics package computes one only for a two-group comparison"
                return(NULL)
            }

            res <- tryCatch(
                withr::with_seed(private$.seed(),
                    private$.withBaseFormulaChar(rlang::inject(
                        statsExpressions::two_sample_test(
                            data       = data,
                            x          = !!rlang::sym(group_var),
                            y          = !!rlang::sym(dep_var),
                            type       = "bayes",
                            digits     = opts$digits,
                            conf.level = opts$conflevel)))),
                error = function(e) e)

            if (inherits(res, "condition")) {
                private$.captionUnavailable <- conditionMessage(res)
                return(NULL)
            }
            if (is.null(res$expression) || length(res$expression) == 0) {
                private$.captionUnavailable <- "the statistics engine returned no Bayes factor"
                return(NULL)
            }
            res$expression[[1]]
        },

        # The "Assumptions and interpretation" panel.
        #
        # Everything the analysis publishes today says what was RUN; nothing says
        # what was FOUND, what the test assumes, or which of the four test
        # families was appropriate. That is the gap a clinician has to close from
        # memory, so close it here: the assumptions of the test actually
        # selected, the result as a sentence, and a methods line that can be
        # pasted into a report.
        #
        # Translucent rgba() fills and inherited colour, never an opaque hex
        # background: jamovi renders results in a dark theme too.
        .renderExplanation = function(data, opts, n_groups) {
            if (!isTRUE(self$options$showexplanation)) return()

            dep_lab   <- htmltools::htmlEscape(opts$dep)
            group_lab <- htmltools::htmlEscape(opts$group)
            omnibus   <- if (n_groups == 2) .("two-group comparison") else .("omnibus comparison across all groups")

            assumptions <- switch(opts$typestatistics,
                parametric = c(
                    .("The values are roughly symmetric within each group. The test tolerates mild skew at these sample sizes but not a long tail or a floor/ceiling effect - read the violins."),
                    .("Groups do NOT have to share the same spread. Welch's correction is used throughout, which is why the degrees of freedom are fractional."),
                    .("Observations are independent. Repeated measurements on the same patient, or several blocks from one specimen, break this and need a paired or mixed model instead.")),
                nonparametric = c(
                    .("No distributional assumption. The test compares ranks, so it answers whether one group tends to give higher values, not whether the means differ."),
                    .("Groups should have a broadly similar distribution SHAPE if you want to read the result as a difference in medians; otherwise read it as stochastic dominance."),
                    .("Observations are independent.")),
                robust = c(
                    .("The comparison uses 20 percent trimmed means, so the most extreme 20 percent at each end of every group is discarded before testing. This is the right choice when outliers are real measurements you do not want to delete but do not want to dominate the result either."),
                    .("Groups do NOT have to share the same spread."),
                    .("Observations are independent. With small groups, trimming 20 percent from each tail can leave very few values - check the per-group sizes.")),
                bayes = c(
                    .("The Bayes factor is computed against a default (Cauchy) prior on the effect size, not one elicited for your setting. Treat it as evidence strength under a conventional prior, not as a posterior probability that the groups differ."),
                    .("Observations are independent."),
                    .("A Bayes factor near 1 means the data do not discriminate between the hypotheses - which is different from evidence of no difference, and is worth stating explicitly when you report it.")),
                .("Observations are independent."))

            test_line <- switch(opts$typestatistics,
                parametric    = if (n_groups == 2) .("Welch's t-test") else .("Welch's one-way analysis of variance"),
                nonparametric = if (n_groups == 2) .("the Mann-Whitney U test") else .("the Kruskal-Wallis test"),
                robust        = if (n_groups == 2) .("Yuen's trimmed-mean test") else .("a heteroscedastic one-way comparison of trimmed means"),
                bayes         = if (n_groups == 2) .("a Bayesian t-test") else .("a Bayesian analysis of variance"),
                .("the selected test"))

            # Only the parametric family honours "Effect Size Measure"; the
            # other three each report one fixed statistic of their own.
            # Measured, both group counts, all four selector values:
            #   parametric    3+ eta^2p / omega^2p     2 Cohen's d / Hedges' g
            #   nonparametric 3+ epsilon^2 (ordinal)   2 rank-biserial correlation
            #   robust        3+ xi                    2 robust (AKP) delta
            #   bayes            Bayes factor + posterior delta
            eff_line <- switch(opts$typestatistics,
                parametric = if (n_groups == 2)
                        switch(opts$effsizetype, unbiased = , omega = .("Hedges' g"), .("Cohen's d"))
                    else
                        switch(opts$effsizetype, unbiased = , omega = .("partial omega-squared"),
                               .("partial eta-squared")),
                nonparametric = if (n_groups == 2) .("the rank-biserial correlation")
                                else .("ordinal epsilon-squared"),
                robust        = if (n_groups == 2) .("the robust (AKP) standardised difference")
                                else .("the explanatory measure of effect size (xi)"),
                bayes         = .("the posterior median difference"),
                .("the selected effect size"))

            interval_word <- if (identical(opts$typestatistics, "bayes"))
                .("credible") else .("confidence")

            pw_line <- if (identical(self$options$pairwisedisplay, "none")) {
                .("No pairwise comparisons were drawn.")
            } else {
                adj <- switch(self$options$padjustmethod,
                              holm = .("Holm"), fdr = .("Benjamini-Hochberg"),
                              bonferroni = .("Bonferroni"), none = .("no"),
                              self$options$padjustmethod)
                jmvcore::format(
                    .("Pairwise comparisons between {which} are shown as brackets, with {adjustment} adjustment for multiple testing."),
                    which = if (identical(self$options$pairwisedisplay, "all"))
                                .("every pair of groups") else .("the pairs that reached significance"),
                    adjustment = adj)
            }

            # The effect size is taken from the same model as the test. For the
            # parametric family that model is Welch's, so this number does NOT
            # equal the one a classical equal-variance ANOVA reports - measured
            # on one dataset, 0.52 here against 0.38 from stats::aov. Say so,
            # because someone will cross-check it in another package.
            model_note <- if (identical(opts$typestatistics, "parametric") && n_groups > 2)
                paste0("<p>", .("The effect size is estimated from the same Welch model as the test, so it will not match the value a classical equal-variance ANOVA reports for these data. That is expected, not an error."), "</p>") else ""

            seed_note <- if (opts$typestatistics %in% c("nonparametric", "robust"))
                paste0("<p>", jmvcore::format(
                    .("The confidence interval on the effect size is obtained by resampling. It is computed with a fixed random seed ({seed}) so this figure reproduces exactly; change the seed under Plot Configuration to check that a borderline interval is not an artefact of one resample."),
                    seed = private$.seed()), "</p>") else ""

            methods <- jmvcore::format(
                .("A comparison of {outcome} across {groups} groups of {factor} was performed using {test}, with {effect} reported as the effect size and a {level} per cent {interval} interval. {pairwise}"),
                outcome = dep_lab, groups = n_groups, factor = group_lab,
                test = test_line, effect = eff_line,
                level = base::format(100 * opts$conflevel),
                interval = interval_word, pairwise = pw_line)

            html <- paste0(
                "<div style=\'background-color: rgba(127,127,127,0.08); border-left: 4px solid rgba(66,133,244,0.7); padding: 12px 16px; margin: 8px 0; color: inherit;\'>",
                "<h3 style=\'margin-top:0; color: inherit;\'>", .("What this analysis assumes"), "</h3><ul>",
                paste0("<li>", vapply(assumptions, htmltools::htmlEscape, ""), "</li>", collapse = ""),
                "</ul>",
                model_note, seed_note,
                "<h3 style=\'color: inherit;\'>", .("Reading the figure"), "</h3>",
                "<p>", .("Each group is drawn as a violin (the shape of its distribution), a boxplot (median and quartiles) and its individual observations."),
                " ", .("The comparison is horizontal, so values run along the bottom axis and group labels down the side."), " ",
                htmltools::htmlEscape(pw_line), "</p>",
                "<h3 style=\'color: inherit;\'>", .("Methods sentence you can copy"), "</h3>",
                "<p style=\'font-style: italic;\'>", htmltools::htmlEscape(methods), "</p>",
                "<p><strong>", .("Before you report this"), ":</strong> ",
                jmvcore::format(
                    .("The {comparison} tells you that the groups differ, not by how much any one pair differs or whether that difference matters clinically. Read the effect size and its interval, not the p value alone."),
                    comparison = omnibus), "</p>",
                "</div>")

            self$results$explanation$setContent(html)
        },

        # Draw a failure message INTO the plot panel.
        #
        # private$.addNotice() cannot help here: jamovi composes and sends the
        # results panel when .run() returns, so anything a render callback writes
        # to an Html item is discarded. Painting the reason where the figure
        # should have been is the only way the user learns why the box is empty.
        .plotFailure = function(msg) {
            print(
                ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0, y = 0, hjust = 0.5, vjust = 0.5,
                                      size = 4, colour = "#721c24",
                                      label = paste(strwrap(msg, width = 60), collapse = "\n")) +
                    ggplot2::theme_void()
            )
            TRUE
        },

        # Message accumulation helper
        .accumulateMessage = function(message) {
            if (is.null(private$.messages)) {
                private$.messages <- character()
            }
            private$.messages <- append(private$.messages, message)
            self$results$todo$setContent(paste(private$.messages, collapse = ""))
        },

        # Same, but also records the message so .prepareData() can replay it on a
        # cache hit. Without this the exclusion disclosures vanish: .run() clears
        # private$.messages on every run, while .prepareData() is keyed on the
        # variables and data dimensions only - so changing any OPTION (test type,
        # confidence level, a title) is a cache hit that skips re-emission, and
        # "N rows excluded due to missing values" silently disappears from a
        # panel whose analysis still excludes them.
        .accumulateDataMessage = function(message) {
            private$.data_messages <- c(private$.data_messages, message)
            private$.accumulateMessage(message)
        },
        
        
        
        
        # Data quality validation helper
        .validateDataQuality = function(mydata, dep_var) {
            num_vals <- jmvcore::toNumeric(mydata[[dep_var]])
            num_vals <- num_vals[!is.na(num_vals)]
            
            if (length(num_vals) < 3) {
                private$.accumulateDataMessage(
                    paste0("<br> ", jmvcore::format(
                        .("Warning: {name} has fewer than 3 valid observations."),
                        name = htmltools::htmlEscape(dep_var)), "<br>")
                )
            }
            if (length(unique(num_vals)) < 2) {
                private$.accumulateDataMessage(
                    paste0("<br> ", jmvcore::format(
                        .("Warning: {name} has no variation - every value is the same."),
                        name = htmltools::htmlEscape(dep_var)), "<br>")
                )
            }
        },
        
        # Outlier detection helper
        .detectOutliers = function(data, var) {
            vals <- jmvcore::toNumeric(data[[var]])
            vals <- vals[!is.na(vals)]
            if (length(vals) > 0) {
                # Checkpoint before expensive quantile calculations
                private$.checkpoint()
                Q1 <- quantile(vals, 0.25, na.rm = TRUE)
                Q3 <- quantile(vals, 0.75, na.rm = TRUE)
                IQR <- Q3 - Q1
                outliers <- which(data[[var]] < (Q1 - 1.5 * IQR) | data[[var]] > (Q3 + 1.5 * IQR))
                if (length(outliers) > 0) {
                    private$.accumulateDataMessage(
                        paste0("<br> ", jmvcore::format(
                            .("Potential outliers detected in {name}: {count}."),
                            name = htmltools::htmlEscape(var),
                            count = length(outliers)), "<br>")
                    )
                }
            }
        },
        
        # Statistical summary helper.
        #
        # Counted straight off the already-filtered frame. The previous tapply()
        # route returned NULL for any factor level left with no rows - exactly
        # what happens after a whole group is lost to missingness - which made
        # sum(sapply(...)) error out and the summary line vanish into the
        # tryCatch that used to sit here.
        .addDataSummary = function(data, dep_var, group_var) {
            if (is.null(dep_var) || is.null(group_var)) return()
            n_groups <- nlevels(droplevels(as.factor(data[[group_var]])))
            total_n <- sum(!is.na(data[[dep_var]]))
            private$.accumulateDataMessage(
                paste0("<br> ", jmvcore::format(
                    .("Analysis summary: {groups} groups, {total} total observations."),
                    groups = n_groups, total = total_n), "<br>")
            )
        },

        # Optimized data preparation with robust caching
        .prepareData = function(force_refresh = FALSE) {
            # Create a hash of the current data to detect changes. This keys on
            # the analysis variables plus the data's dimensions and column names
            # rather than the individual cell values. jamovi re-instantiates the
            # analysis object whenever the underlying data is edited, so within a
            # single object lifecycle identical dims + names imply identical
            # values; add a value digest here if this helper is ever reused
            # outside jamovi's lifecycle.
            current_hash <- digest::digest(list(
                dep = self$options$dep,
                group = self$options$group,
                data_dim = dim(self$data),
                col_names = names(self$data),
                grvar = self$options$grvar
            ), algo = "md5")
            
            # Only reprocess if data has changed or forced refresh
            if (!is.null(private$.processedData) && 
                private$.data_hash == current_hash && 
                !force_refresh) {
                for (msg in private$.data_messages)
                    private$.accumulateMessage(msg)
                return(private$.processedData)
            }

            # Clear previous messages and add processing feedback
            private$.messages <- NULL
            private$.data_messages <- NULL
            private$.accumulateDataMessage(
                paste0("<br>", .("Processing data for the comparison..."), "<br><hr>")
            )
            
            # Track processing time for large datasets
            start_time <- Sys.time()

            mydata <- self$data

            # Convert dependent variable to numeric (single variable)
            dep_var <- self$options$dep
            if (!is.null(dep_var)) {
                mydata[[dep_var]] <- jmvcore::toNumeric(mydata[[dep_var]])
                if (!is.numeric(mydata[[dep_var]]))
                    jmvcore::reject("The dependent variable must be numeric.")
            }

            # SELECTIVE NA OMISSION - only remove rows with NAs in analysis variables
            # This prevents dropping patients with NAs in unused columns
            if (!is.null(dep_var) && !is.null(self$options$group)) {
                relevant_cols <- c(dep_var, self$options$group)

                # Add grouping variable if present
                if (!is.null(self$options$grvar)) {
                    relevant_cols <- c(relevant_cols, self$options$grvar)
                }

                private$.checkpoint()

                # Count rows before and after NA removal
                n_before <- nrow(mydata)
                mydata <- mydata[complete.cases(mydata[relevant_cols]), ]
                n_after <- nrow(mydata)

                # complete.cases() follows is.na(), which is TRUE for NaN but
                # FALSE for Inf, so an infinite measurement survived into
                # ggstatsplot and killed the whole figure with "'from' must be a
                # finite number" - an EMPTY plot box sitting under this module's
                # own "Analysis completed successfully" notice. Measured on 120
                # rows with a single Inf: zero text elements in the rendered SVG.
                private$.nonFiniteDropped <- 0L
                finite_rows <- is.finite(jmvcore::toNumeric(mydata[[dep_var]]))
                if (any(!finite_rows)) {
                    private$.nonFiniteDropped <- sum(!finite_rows)
                    mydata <- mydata[finite_rows, , drop = FALSE]
                    private$.accumulateDataMessage(
                        paste0("<br> ", jmvcore::format(
                            .("Rows excluded because {name} held an infinite value: {count}. Infinite values usually indicate a division by zero or an out-of-range entry - check the source data."),
                            count = private$.nonFiniteDropped,
                            name = htmltools::htmlEscape(dep_var)), "<br>")
                    )
                    n_after <- nrow(mydata)
                }

                # A group whose measurements are ALL missing disappears from the
                # comparison entirely. The row count alone does not reveal that:
                # a clinician who selected Control/DrugA/DrugB and lost Control
                # to missingness would otherwise read a two-group result as
                # though that is what they asked for. Name what went.
                before_lv <- levels(droplevels(as.factor(self$data[[self$options$group]])))
                after_lv  <- levels(droplevels(as.factor(mydata[[self$options$group]])))
                gone <- setdiff(before_lv, after_lv)
                if (length(gone) > 0) {
                    private$.accumulateDataMessage(
                        paste0("<br> <strong>", .("Groups dropped"), ":</strong> ",
                               jmvcore::format(
                                   .("{names} had no usable measurements and were excluded from the comparison entirely."),
                                   names = htmltools::htmlEscape(paste(gone, collapse = ", "))), "<br>")
                    )
                }

                # Report NA removal if any occurred
                if (n_before > n_after) {
                    n_dropped <- n_before - n_after
                    private$.accumulateDataMessage(
                        paste0("<br> ", jmvcore::format(
                            .("Rows excluded for missing values in the analysis variables: {dropped}."),
                            dropped = n_dropped), "<br>",
                            jmvcore::format(
                                .("Rows with data: {kept} of {total} ({percent} percent)."),
                                kept = n_after, total = n_before,
                                percent = round(100 * n_after / n_before, 1)), "<br>")
                    )
                }
            }
            
            # Validate data quality
            if (!is.null(dep_var)) {
                private$.validateDataQuality(mydata, dep_var)
            }
            
            # Detect outliers for datasets with sufficient size
            if (nrow(mydata) > 10 && !is.null(dep_var)) {
                private$.detectOutliers(mydata, dep_var)
            }
            
            # Add statistical summary
            private$.addDataSummary(mydata, dep_var, self$options$group)
            
            # Add processing time feedback for large datasets
            elapsed <- difftime(Sys.time(), start_time, units = "secs")
            if (nrow(mydata) > 1000) {
                private$.accumulateDataMessage(
                    paste0("<br> ", jmvcore::format(
                        .("Large dataset processed in {seconds} seconds."),
                        seconds = round(elapsed, 2)), "<br>")
                )
            }

            # Cache the processed data with hash
            private$.processedData <- mydata
            private$.data_hash <- current_hash
            return(mydata)
        },

        # Optimized options preparation with robust caching
        .prepareOptions = function(force_refresh = FALSE) {
            # Create robust hash of current options to detect changes
            current_options_hash <- digest::digest(list(
                dep = self$options$dep,
                group = self$options$group,
                grvar = self$options$grvar,
                typestatistics = self$options$typestatistics,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                testvalue = self$options$testvalue,
                bfmessage = self$options$bfmessage,
                conflevel = self$options$conflevel,
                k = self$options$k,
                testvalueline = self$options$testvalueline,
                centralityparameter = self$options$centralityparameter,
                centralityk = self$options$centralityk,
                pairwisedisplay = self$options$pairwisedisplay,
                padjustmethod = self$options$padjustmethod,
                seed = self$options$seed,
                titles = list(self$options$mytitle, self$options$xtitle, self$options$ytitle),
                display = list(self$options$resultssubtitle, self$options$originaltheme)
            ), algo = "md5")
            
            # Only reprocess if options have changed or forced refresh
            if (!is.null(private$.processedOptions) && 
                private$.options_hash == current_options_hash && 
                !force_refresh) {
                return(private$.processedOptions)
            }

            # Add options preparation feedback if not already processing
            if (is.null(private$.messages)) {
                private$.accumulateMessage(
                    paste0("<br>", .("Preparing analysis options..."), "<br><hr>")
                )
            }

            # Process type of statistics
            typestatistics <- self$options$typestatistics

            # Process variables
            dep <- self$options$dep
            group <- self$options$group

            # Centrality settings mapped to ggstatsplot arguments.
            #
            # There are two controls for one thing, in two different collapse
            # boxes: "Central Tendency Display" (centralityparameter:
            # mean/median/none) and "Central Tendency Measure" (centralitytype:
            # mean/median/trimmed/Bayesian). They used to contradict each other
            # silently - measured with centralityparameter = "mean" and
            # centralitytype = "nonparametric", the plot drew and labelled the
            # MEDIANS (9.80, 13.56, 11.05) while the user's Display control read
            # "Mean". centralitytype is the richer control and is the one the UI
            # enables alongside the plotting checkbox, so it decides; the
            # Display control keeps only its unique power, which is switching
            # centrality off. A disagreement is now stated rather than resolved
            # in silence.
            centrality_plotting <- isTRUE(self$options$centralityplotting) && self$options$centralityparameter != "none"
            centrality_type <- self$options$centralitytype
            if (is.null(centrality_type) || centrality_type == "")
                centrality_type <- typestatistics

            if (centrality_plotting) {
                implied <- switch(self$options$centralityparameter,
                                  mean = "parametric", median = "nonparametric", NULL)
                if (!is.null(implied) && !identical(implied, centrality_type)) {
                    shown <- switch(centrality_type,
                                    parametric = "mean", nonparametric = "median",
                                    robust = "trimmed mean", bayes = "Bayesian (MAP) estimate",
                                    centrality_type)
                    private$.addNotice(jmvcore::format(
                        .("Your two central-tendency settings disagree: 'Central Tendency Display' still carries the legacy value '{legacy}' while 'Central Tendency Measure' selects {selected}. The plot shows the {drawn}, because 'Central Tendency Display' only decides whether a line is drawn at all."),
                        legacy = self$options$centralityparameter,
                        selected = switch(centrality_type, parametric = "Mean",
                                          nonparametric = "Median", robust = "Trimmed Mean",
                                          bayes = "Bayesian Estimate", centrality_type),
                        drawn = shown), "WARNING")
                }
            }

            # Compute axis labels respecting orientation flip (values on x-axis)
            xlab <- self$options$ytitle
            if (xlab == '') xlab <- group
            ylab <- self$options$xtitle
            if (ylab == '') ylab <- dep
            
            # Process titles
            mytitle <- self$options$mytitle
            if (mytitle == '') mytitle <- NULL
            
            # Cache the processed options with all parameters
            options_list <- list(
                typestatistics = typestatistics,
                dep = dep,
                group = group,
                mytitle = mytitle,
                xlab = xlab,
                ylab = ylab,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                testvalue = self$options$testvalue,
                bfmessage = self$options$bfmessage,
                conflevel = self$options$conflevel,
                digits = self$options$k,
                testvalueline = self$options$testvalueline,
                centralityparameter = self$options$centralityparameter,
                centralityk = self$options$centralityk,
                resultssubtitle = self$options$resultssubtitle,
                originaltheme = self$options$originaltheme
            )

            # Process centrality parameters if enabled
            options_list$centrality.plotting <- centrality_plotting
            options_list$centrality.type <- centrality_type

            # Post-hoc pairwise comparisons.
            #
            # ggstatsplot runs these on EVERY figure by default
            # (pairwise.display = "significant", p.adjust.method = "holm") and
            # captions the plot with the method it used, so the analysis was
            # already publishing multiplicity-corrected brackets between named
            # group pairs that the user could neither see in the interface nor
            # change. The defaults are the right ones; they just have to be a
            # stated choice, because which correction was applied belongs in a
            # methods paragraph.
            options_list$pairwise.display <- self$options$pairwisedisplay
            options_list$p.adjust.method <- switch(self$options$padjustmethod,
                                                   fdr = "BH", self$options$padjustmethod)
            options_list$ggplot.component <- list(ggplot2::coord_flip())
            if (isTRUE(self$options$testvalueline)) {
                options_list$ggplot.component <- c(
                    options_list$ggplot.component,
                    list(ggplot2::geom_hline(
                        yintercept = self$options$testvalue,
                        linetype = "dashed",
                        color = "red"
                    ))
                )
            }
            
            private$.processedOptions <- options_list
            private$.options_hash <- current_options_hash
            return(options_list)
        },

        # run ----
        .run = function() {
            # Clear messages, notices, and cached validity at start of new run
            private$.messages <- NULL
            private$.clearNotices()
            private$.inputsValid <- FALSE
            private$.subtitleFallback <- NULL
            private$.subtitleCache <- NULL
            private$.captionCache <- NULL
            private$.captionUnavailable <- NULL

            # Initial Message ----
            if ( is.null(self$options$dep) || is.null(self$options$group)) {

                # todo ----

                todo <- paste0(
                    "<br>", .("Welcome to ClinicoPath"), "<br><br>",
                    .("This analysis compares a continuous variable across groups and draws the comparison horizontally, as a box-violin figure with the individual observations shown."),
                    "<br><br>",
                    .("Looking for a Cleveland dot chart - one summary point per group, tested against a reference value? Use 'Dot Chart (Summary vs Reference Value)' instead. This analysis uses every observation and compares the groups with each other."),
                    "<br><br>",
                    .("Select a continuous dependent variable and a categorical grouping variable to begin."),
                    "<br><br>",
                    jmvcore::format(
                        .("This analysis is built on the {pkg1} and {pkg2} packages; please cite jamovi and the packages listed below."),
                        pkg1 = "ggplot2", pkg2 = "ggstatsplot"),
                    "<br><hr>")

                self$results$todo$setContent(todo)

                return()

            } else {

                # No intro line here: .prepareData() writes the same Html item
                # through .accumulateMessage() a few lines below, so anything set
                # now is overwritten before the panel is ever sent. The data
                # messages are the useful content and they own this item.

                if (nrow(self$data) == 0) {
                    private$.addNotice(.("Data contains no complete rows after filtering. Please check for missing values."), "ERROR")
                    return()
                }

                # Pre-process data and options for performance with enhanced validation
                private$.inputsValid <- FALSE
                tryCatch({
                    mydata <- private$.prepareData()
                    options_data <- private$.prepareOptions()

                    # Validate inputs once per run. All validation notices are
                    # emitted here (not in the render paths) so they neither
                    # duplicate when a Split-By variable makes both plots render,
                    # nor accumulate on plot-only re-renders such as resizing.
                    private$.inputsValid <- private$.validateInputs()

                    # Describe the analysis; do NOT claim it succeeded. .run()
                    # finishes before a single pixel is drawn, so the old
                    # "Analysis completed successfully" notice was published
                    # while the figure could still fail - and it regularly did,
                    # leaving a confident success message above an empty plot
                    # box (measured with one Inf value, and with a constant
                    # dependent variable).
                    if (isTRUE(private$.inputsValid)) {
                        n_obs <- nrow(mydata)
                        n_groups <- length(unique(mydata[[options_data$group]]))
                        test_name <- switch(options_data$typestatistics,
                            "parametric" = "parametric (t-test/ANOVA)",
                            "nonparametric" = "nonparametric (Mann-Whitney/Kruskal-Wallis)",
                            "robust" = "robust (trimmed means)",
                            "bayes" = "Bayesian",
                            "selected"
                        )
                        private$.addNotice(jmvcore::format(
                            .("Comparing {groups} groups with N = {n} observations using a {test} test."),
                            groups = n_groups, n = n_obs, test = test_name), "INFO")

                        # Computed HERE, not in .plot(): a notice raised during
                        # rendering is thrown away, so this is the only place the
                        # user can be told the effect-size choice was dropped.
                        # The robust family bootstraps 100 resamples in here:
                        # 12.7 s at N = 50,000, during which jamovi has no way
                        # to interrupt or report progress without a checkpoint.
                        private$.checkpoint()
                        private$.subtitleCache <- private$.subtitleExpr(
                            mydata, options_data$group, options_data$dep, options_data)
                        private$.captionCache <- private$.captionExpr(
                            mydata, options_data$group, options_data$dep, options_data)

                        # The statsExpressions takeover is what makes the effect
                        # size selector work; say so when it could not be used.
                        if (!is.null(private$.subtitleFallback) && isTRUE(self$options$resultssubtitle))
                            private$.addNotice(jmvcore::format(
                                .("The effect size measure you selected could not be applied ({reason}), so the plot shows the statistics package default instead."),
                                reason = htmltools::htmlEscape(private$.subtitleFallback)), "WARNING")

                        # The takeover covers the single figure only.
                        # grouped_ggbetweenstats builds one subtitle per panel
                        # inside itself and takes no list of expressions, so
                        # effsize.type stays inert on the split figure. Measured
                        # on 3 groups x 2 split levels: "Cohen's d" and
                        # "Omega-squared" both rendered omega-squared, with no
                        # message anywhere - the comment in .plot2() claimed
                        # .run() disclosed this, and it did not.
                        # The selector has no counterpart outside the parametric
                        # family: each of the other three reports one fixed
                        # statistic of its own, so a changed selection is dropped.
                        if (!identical(self$options$effsizetype, "biased") &&
                            !identical(self$options$typestatistics, "parametric") &&
                            isTRUE(self$options$resultssubtitle))
                            private$.addNotice(jmvcore::format(
                                .("'Effect Size Measure' applies to the parametric test only. The {test} test reports its own effect size instead, so your selection was not used."),
                                test = switch(self$options$typestatistics,
                                              nonparametric = "nonparametric",
                                              robust = "robust", bayes = "Bayesian",
                                              self$options$typestatistics)), "INFO")

                        if (!is.null(self$options$grvar) && isTRUE(self$options$resultssubtitle))
                            private$.addNotice(.("The 'Effect Size Measure' setting does not reach the Split By panels: the statistics package computes one subtitle per panel internally and always uses its own default there. The single unsplit figure below honours your selection."), "INFO")

                        # Say why the box did nothing rather than leaving the
                        # user to notice the caption is missing.
                        # Uncorrected pairwise p values overstate significance,
                        # and the figure does not say how many comparisons were
                        # made, so the reader cannot discount them for themselves.
                        if (!identical(self$options$pairwisedisplay, "none") &&
                            identical(self$options$padjustmethod, "none"))
                            private$.addNotice(jmvcore::format(
                                .("Pairwise comparisons are shown without any adjustment for multiple testing. With {groups} groups that is {pairs} comparisons, so even if no groups truly differ there is about a {risk} percent chance that at least one of them reaches significance. Use Holm unless you have a specific reason not to."),
                                groups = n_groups, pairs = choose(n_groups, 2),
                                risk = round(100 * (1 - 0.95 ^ choose(n_groups, 2)))), "STRONG_WARNING")

                        private$.renderExplanation(mydata, options_data, n_groups)

                        if (isTRUE(self$options$bfmessage) && !is.null(private$.captionUnavailable))
                            private$.addNotice(jmvcore::format(
                                .("No Bayes factor caption is shown because {reason}."),
                                reason = htmltools::htmlEscape(private$.captionUnavailable)), "INFO")
                    }
                }, error = function(e) {
                    private$.addNotice(jmvcore::format(
                        .("Data processing failed: {reason}. Please check your variable selections and try again."),
                        reason = htmltools::htmlEscape(e$message)), "ERROR")
                    return()
                })

            }
        }


        ,
        .plot = function(image, ggtheme, theme, ...) {
            # Inputs are validated once in .run(); the render path only reads the
            # cached result so validation notices are not re-emitted per render. ----
            if (!isTRUE(private$.inputsValid))
                return()

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()


            # ggbetweenstats ----
            # https://www.indrapatil.com/ggstatsplot/reference/ggbetweenstats.html

            # Checkpoint before expensive ggstatsplot computation
            private$.checkpoint()

            # effsize.type is inert in ggstatsplot 1.0.0, so the subtitle is
            # computed through statsExpressions in .run() and switched off here
            # when the takeover succeeded.
            sub_expr <- private$.subtitleCache
            cap_expr <- private$.captionCache

            plot <- tryCatch({
                p <- withr::with_seed(private$.seed(),
                     private$.withBaseFormulaChar(ggstatsplot::ggbetweenstats(
                    data = mydata,
                    x = !!rlang::sym(options_data$group),
                    y = !!rlang::sym(options_data$dep),
                    title = options_data$mytitle,
                    xlab = options_data$xlab,
                    ylab = options_data$ylab,
                    type = options_data$typestatistics,
                    conf.level = options_data$conflevel,
                    digits = options_data$digits,
                    bf.message = options_data$bfmessage,
                    centrality.plotting = options_data$centrality.plotting,
                    centrality.type = options_data$centrality.type,
                    results.subtitle = if (is.null(sub_expr)) options_data$resultssubtitle else FALSE,
                    pairwise.display = options_data$pairwise.display,
                    p.adjust.method = options_data$p.adjust.method,
                    ggplot.component = options_data$ggplot.component,
                    ggtheme = if (options_data$originaltheme) ggstatsplot::theme_ggstatsplot() else ggtheme
                )))
                # Attach after construction: do.call()/quote-based routes either
                # evaluate the plotmath language object ("could not find function
                # 'italic'") or break the rlang::sym() arguments.
                if (!is.null(sub_expr)) p <- p + ggplot2::labs(subtitle = sub_expr)
                # Same channel ggstatsplot uses for bf.message, restored here
                # because results.subtitle = FALSE closed its own.
                if (!is.null(cap_expr)) p <- p + ggplot2::labs(caption = cap_expr)
                p
            }, error = function(e) e)

            if (inherits(plot, "condition"))
                return(private$.plotFailure(jmvcore::format(
                    .("The plot could not be drawn: {reason}. Check the dependent variable for constant values, extreme outliers or too few observations per group, or try a different statistical test."),
                    reason = conditionMessage(plot))))
            if (is.null(plot)) return()

            # Print Plot ----

            print(plot)
            TRUE

        }


        ,

        .plot2 = function(image, ggtheme, theme, ...) {
            # Inputs are validated once in .run(); the render path only reads the
            # cached result. The Split-By variable must also be present. ----
            if (!isTRUE(private$.inputsValid) || is.null(self$options$grvar))
                return()

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()


            # grouped_ggbetweenstats ----
            # https://www.indrapatil.com/ggstatsplot/reference/grouped_ggbetweenstats.html



            if (!is.null(self$options$grvar)) {
                grvar <- self$options$grvar

                # Checkpoint before expensive grouped ggstatsplot computation
                private$.checkpoint()

                plot2 <- tryCatch({
                    # No takeover here: grouped_ggbetweenstats computes one
                    # subtitle per panel internally and there is no supported way
                    # to hand it a list of expressions. effsize.type is therefore
                    # still inert on this figure - .run() says so.
                    withr::with_seed(private$.seed(),
                    private$.withBaseFormulaChar(ggstatsplot::grouped_ggbetweenstats(
                        data = mydata,
                        x = !!rlang::sym(options_data$group),
                        y = !!rlang::sym(options_data$dep),
                        grouping.var = !!rlang::sym(grvar),
                        type = options_data$typestatistics,
                        conf.level = options_data$conflevel,
                        digits = options_data$digits,
                        bf.message = options_data$bfmessage,
                        results.subtitle = options_data$resultssubtitle,
                        pairwise.display = options_data$pairwise.display,
                        p.adjust.method = options_data$p.adjust.method,
                        centrality.plotting = options_data$centrality.plotting,
                        centrality.type = options_data$centrality.type,
                        ggplot.component = options_data$ggplot.component,
                        ggtheme = if (options_data$originaltheme) ggstatsplot::theme_ggstatsplot() else ggtheme,
                        xlab = options_data$xlab,
                        ylab = options_data$ylab,
                        # NOT `title =`. grouped_ggbetweenstats sets the title of
                        # each panel to that panel's level name, so passing one
                        # through `...` collided with its own argument and threw
                        # "formal argument \"title\" matched by multiple actual
                        # arguments" for EVERY Split By analysis - the whole
                        # feature had never produced a figure. The error went to a
                        # notice raised at render time, which jamovi discards, so
                        # the user saw an empty panel under a success message.
                        # The overall title belongs to the patchwork annotation.
                        annotation.args = list(title = options_data$mytitle)
                    )))
                }, error = function(e) e)

                if (inherits(plot2, "condition"))
                    return(private$.plotFailure(jmvcore::format(
                        .("The split figure could not be drawn: {reason}. Check that every level of the Split By variable has enough data in at least two groups."),
                        reason = conditionMessage(plot2))))
                if (is.null(plot2)) return()
            }


            # Print Plot ----

            print(plot2)
            TRUE

        }





    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for jjdotplotstats analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            dep <- self$options$dep
            group <- self$options$group

            if (is.null(dep) || is.null(group))
                return('')

            # Build the argument list in option-declaration order.
            #
            # Every variable-name option (dep, group, grvar) is emitted as a
            # deparse()'d string literal. deparse() produces valid, fully-escaped
            # R for names containing spaces, quotes or backslashes (e.g.
            # `Tumor Grade`); jmvcore's default sourcify would emit these as bare,
            # unquoted symbols and yield invalid syntax. Detecting OptionVariable
            # by class (rather than by name) means any variable option added later
            # is escaped automatically.
            #
            # data/dep/group are NOT re-emitted through private$.asArgs() - doing
            # so previously duplicated dep and group in the generated syntax (the
            # "double variables" bug). All non-variable options keep jmvcore's
            # per-option sourcify so formatting stays consistent with jamovi.
            args <- character(0)
            for (option in private$.options$options) {
                if (option$name == 'data')
                    next
                if (inherits(option, 'OptionVariable') || inherits(option, 'OptionVariables')) {
                    val <- option$value
                    if (!is.null(val))
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
            paste0(pkg_name, '::jjdotplotstats(\n    data = data,\n    ',
                   paste(args, collapse = ',\n    '), ')')
        }
    ) # End of public list
)
