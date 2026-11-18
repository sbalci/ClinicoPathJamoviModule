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
        # Cached result of .validateInputs() and .validateSplitInputs(); computed
        # once per .run() so the render paths (.plot / .plot2) gate silently.
        .inputsValid = FALSE,
        .splitValid = TRUE,
        # Set by .subtitleExpr() when the statsExpressions takeover could not be
        # used even though ggbetweenstats can still draw its default subtitle.
        .subtitleFallback = NULL,
        # Set by .subtitleExpr() when no statistical subtitle can be computed at
        # all (e.g. singleton group n < 2 or zero post-trim spread).
        .subtitleUncomputable = NULL,
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
        # Exact numeric p-value from the most recent omnibus evaluation in .run().
        .lastExactP = NA_real_,

        # Private fields die with the analysis instance. jmvcore restores results from disk
        # and renders without calling .run() (right-click > Export...), so on that path
        # .inputsValid holds its FALSE initial value and both renderers return before drawing
        # - a blank exported image - while the two expression caches come back NULL and the
        # figure silently loses its subtitle and Bayes-factor caption. image$state is part of
        # the serialised results, so publish all state fields there. They are small: a
        # statsExpressions subtitle is an ordinary `call`, ~0.9 KB serialised (measured).
        .publishRenderState = function() {
            st_main <- list(
                valid = isTRUE(private$.inputsValid),
                uncomputable = !is.null(private$.subtitleUncomputable),
                subtitle = private$.subtitleCache,
                caption = private$.captionCache
            )
            st_split <- list(
                valid = isTRUE(private$.inputsValid),
                splitValid = isTRUE(private$.splitValid)
            )
            if (!is.null(self$results$plot))
                self$results$plot$setState(st_main)
            if (!is.null(self$results$plot2))
                self$results$plot2$setState(st_split)
        },

        # image$state is the ONLY source, deliberately. .run() publishes it before any
        # render can happen in-session, and on the export path it is all that exists - so a
        # private$ fallback would add a code path that never runs and would hide the very
        # dependency this is here to remove. A NULL state means .run() did not get far
        # enough to validate, and not drawing is then correct.
        .renderState = function(image) {
            st <- image$state
            if (is.null(st)) list(valid = FALSE) else st
        },
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

        # Severity-ordered theme-safe HTML notice panel helper
        .addNotice = function(message, type = "INFO") {
            if (is.null(private$.noticesList)) {
                private$.noticesList <- list()
            }

            # Translucent rgba() fills composite cleanly over both light and dark jamovi themes
            style_info <- switch(type,
                "ERROR" = list(
                    bg = "rgba(216, 33, 50, 0.18)",
                    border = "#dc3545"
                ),
                "STRONG_WARNING" = list(
                    bg = "rgba(255, 202, 33, 0.23)",
                    border = "#fd7e14"
                ),
                "WARNING" = list(
                    bg = "rgba(255, 202, 33, 0.23)",
                    border = "#ffc107"
                ),
                "INFO" = list(
                    bg = "rgba(0, 123, 255, 0.14)",
                    border = "#007bff"
                ),
                list(
                    bg = "rgba(0, 123, 255, 0.14)",
                    border = "#007bff"
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
                "; color: inherit; border-left: 4px solid ", style_info$border,
                "; padding: 12px; margin: 8px 0;'>",
                "<strong style='color: inherit;'>", type_label, ":</strong> ", message,
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

        # Clean categorical columns: preserve haven_labelled value labels in declared
        # order and convert explicit <NA> factor levels (from addNA()) back to true NA
        # so missing group memberships are never analyzed as an extra biological cohort.
        .cleanFactor = function(x) {
            if (is.null(x)) return(NULL)
            if (inherits(x, "haven_labelled")) {
                lbls <- attr(x, "labels", exact = TRUE)
                if (!is.null(lbls) && length(lbls) > 0L) {
                    raw_vals <- unclass(x)
                    m <- match(raw_vals, unname(lbls))
                    out_chr <- ifelse(is.na(raw_vals), NA_character_,
                                      ifelse(!is.na(m), names(lbls)[m], as.character(raw_vals)))
                    lvl_names <- unique(c(names(lbls), stats::na.omit(unique(out_chr))))
                    return(factor(out_chr, levels = lvl_names))
                }
            }
            if (is.factor(x)) {
                raw_lvls <- levels(x)
                if (anyNA(raw_lvls)) {
                    keep_lvls <- raw_lvls[!is.na(raw_lvls)]
                    chr <- raw_lvls[as.integer(x)]
                    return(factor(chr, levels = keep_lvls, ordered = is.ordered(x)))
                }
                return(x)
            }
            as.factor(x)
        },

        .fmtStatVal = function(val, digits = 2L) {
            digits <- as.integer(digits)
            if (!is.finite(val)) return(NA_character_)
            if (val != 0 && abs(val) < 1e-4 && abs(val) < 10^(-digits)) {
                formatC(val, digits = digits, format = "e")
            } else {
                sprintf(paste0("%.", digits, "f"), val)
            }
        },

        # init ----

        .init = function() {
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 650
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450

            self$results$plot$setSize(plotwidth, plotheight)

            if (!is.null(self$options$grvar)) {
                mydata <- self$data
                grvar <- self$options$grvar
                num_levels <- if (!is.null(mydata) && grvar %in% names(mydata)) {
                    nlevels(droplevels(private$.cleanFactor(mydata[[grvar]])))
                } else {
                    1L
                }
                self$results$plot2$setSize(
                    min(max(num_levels, 1L) * plotwidth, 3000L), plotheight)
            }
        }


,
        # Shared validation helper operating on the cleaned finite analysis data
        .validateInputs = function(mydata = NULL) {
            if (is.null(self$options$dep) || is.null(self$options$group))
                return(FALSE)

            if (nrow(self$data) == 0) {
                private$.addNotice(.("Data contains no complete rows. Please check for missing values in your selected variables."), "ERROR")
                return(FALSE)
            }

            if (!(self$options$dep %in% names(self$data))) {
                available_vars <- htmltools::htmlEscape(paste(names(self$data), collapse = ", "))
                private$.addNotice(.fmt(
                    .("Variable '{name}' was not found in the data. Available variables are: {available}. Please select a valid continuous variable for the dependent variable."),
                    name = htmltools::htmlEscape(self$options$dep), available = available_vars), "ERROR")
                return(FALSE)
            }

            if (!(self$options$group %in% names(self$data))) {
                available_vars <- htmltools::htmlEscape(paste(names(self$data), collapse = ", "))
                private$.addNotice(.fmt(
                    .("Variable '{name}' was not found in the data. Available variables are: {available}. Please select a valid grouping variable."),
                    name = htmltools::htmlEscape(self$options$group), available = available_vars), "ERROR")
                return(FALSE)
            }

            if (!is.null(self$options$grvar) && !(self$options$grvar %in% names(self$data))) {
                available_vars <- htmltools::htmlEscape(paste(names(self$data), collapse = ", "))
                private$.addNotice(.fmt(
                    .("Variable '{name}' was not found in the data. Available variables are: {available}. Please select a valid Split By variable."),
                    name = htmltools::htmlEscape(self$options$grvar), available = available_vars), "ERROR")
                return(FALSE)
            }

            if (identical(self$options$dep, self$options$group)) {
                private$.addNotice(.("Dependent Variable and Grouping Variable must be distinct columns."), "ERROR")
                return(FALSE)
            }
            if (!is.null(self$options$grvar) &&
                (identical(self$options$grvar, self$options$dep) ||
                 identical(self$options$grvar, self$options$group))) {
                private$.addNotice(.("The Split By variable must be distinct from Dependent Variable and Grouping Variable."), "ERROR")
                return(FALSE)
            }

            raw_dep_num <- suppressWarnings(jmvcore::toNumeric(self$data[[self$options$dep]]))
            if (sum(is.finite(raw_dep_num)) == 0L) {
                private$.addNotice(.fmt(
                    .("Variable '{name}' contains no finite numeric measurements. Please select a valid continuous numeric variable."),
                    name = htmltools::htmlEscape(self$options$dep)), "ERROR")
                return(FALSE)
            }

            if (is.null(mydata))
                mydata <- private$.prepareData(emit_messages = FALSE)

            # Require at least two groups with complete finite data on (dep, group)
            clean_grp <- droplevels(private$.cleanFactor(mydata[[self$options$group]]))
            group_levels <- nlevels(clean_grp)
            if (group_levels < 2) {
                private$.addNotice(.fmt(
                    .("At least two groups with data are required for a comparison. Groups found with complete data: {found}. Please check for missing values or select different variables."),
                    found = group_levels), "ERROR")
                return(FALSE)
            }

            # Check total finite sample size
            n_total <- nrow(mydata)
            if (n_total < 30) {
                private$.addNotice(.fmt(
                    .("Small total sample size (N = {n}). Statistical tests may be unreliable below 30 observations. Interpret the result cautiously or collect more data."),
                    n = n_total), "STRONG_WARNING")
            }

            # Check minimum finite group size
            group_sizes <- table(clean_grp)
            min_group_n <- min(group_sizes)
            if (min_group_n < 10) {
                min_group_name <- names(which.min(group_sizes))
                private$.addNotice(.fmt(
                    .("Very small group sizes detected: the smallest is n = {n} in group '{group}'. Groups below 10 observations may produce unreliable test results. Consider combining groups or collecting more data."),
                    n = min_group_n, group = htmltools::htmlEscape(min_group_name)), "STRONG_WARNING")
            }

            # Guard against constant dependent variable before plotting
            dep_vals <- jmvcore::toNumeric(mydata[[self$options$dep]])
            dep_vals <- dep_vals[is.finite(dep_vals)]
            if (length(dep_vals) > 0 && length(unique(dep_vals)) < 2) {
                private$.addNotice(.fmt(
                    .("'{name}' takes the same value ({value}) in every row, so there is no variation to compare between groups. Check that the correct variable is selected."),
                    name = htmltools::htmlEscape(self$options$dep),
                    value = base::format(dep_vals[1])), "ERROR")
                return(FALSE)
            }

            # Validate centrality parameter consistency
            private$.validateCentralityOptions()

            return(TRUE)
        },

        # Validate per-stratum sample sizes and group counts for Split By (plot2)
        .validateSplitInputs = function(mydata) {
            grvar <- self$options$grvar
            if (is.null(grvar) || !(grvar %in% names(mydata)))
                return(TRUE)

            sp_df <- private$.prepareSplitData(mydata)
            n_grvar_na <- nrow(mydata) - nrow(sp_df)
            if (n_grvar_na > 0L) {
                private$.addNotice(.fmt(
                    .("The Split By variable '{grvar}' is missing for {count} observation(s); those rows are excluded from the Split By panels only and remain in the main unsplit comparison."),
                    grvar = htmltools::htmlEscape(grvar),
                    count = n_grvar_na), "INFO")
            }

            if (nrow(sp_df) == 0L) {
                private$.addNotice(.fmt(
                    .("No complete observations remain for Split By variable '{grvar}', so the Split By figure cannot be drawn."),
                    grvar = htmltools::htmlEscape(grvar)), "WARNING")
                return(FALSE)
            }

            st_fac <- droplevels(private$.cleanFactor(sp_df[[grvar]]))
            deg_levels <- character()
            valid_levels <- character()
            singleton_levels <- character()
            small_levels <- character()

            for (lv in levels(st_fac)) {
                sub_lv <- sp_df[st_fac == lv, , drop = FALSE]
                grp_lv <- droplevels(private$.cleanFactor(sub_lv[[self$options$group]]))
                y_lv <- sub_lv[[self$options$dep]]
                y_lv <- y_lv[is.finite(y_lv)]
                if (nlevels(grp_lv) < 2L || length(unique(y_lv)) < 2L) {
                    deg_levels <- c(deg_levels, lv)
                } else {
                    valid_levels <- c(valid_levels, lv)
                    g_tab <- table(grp_lv)
                    if (any(g_tab < 2L)) {
                        singleton_levels <- c(singleton_levels, lv)
                    } else if (any(g_tab < 10L)) {
                        small_levels <- c(small_levels, lv)
                    }
                }
            }

            if (length(valid_levels) == 0L) {
                private$.addNotice(.fmt(
                    .("Every level of Split By variable '{grvar}' ({levels}) has fewer than two groups or no variation in '{dep}', so no stratified comparison could be computed."),
                    grvar = htmltools::htmlEscape(grvar),
                    levels = htmltools::htmlEscape(paste(deg_levels, collapse = ", ")),
                    dep = htmltools::htmlEscape(self$options$dep)), "WARNING")
                return(FALSE)
            }

            if (length(deg_levels) > 0L) {
                private$.addNotice(.fmt(
                    .("Split By level(s) '{levels}' had fewer than two groups with data or no variation in '{dep}', so statistical test results could not be computed for those panel(s)."),
                    levels = htmltools::htmlEscape(paste(deg_levels, collapse = ", ")),
                    dep = htmltools::htmlEscape(self$options$dep)), "WARNING")
            }

            if (length(singleton_levels) > 0L) {
                private$.addNotice(.fmt(
                    .("Split By level(s) '{levels}' contain at least one group with fewer than 2 observations, so statistical test subtitles could not be computed for those panel(s)."),
                    levels = htmltools::htmlEscape(paste(singleton_levels, collapse = ", "))), "WARNING")
            } else if (length(small_levels) > 0L) {
                private$.addNotice(.fmt(
                    .("Small subgroup sample sizes (fewer than 10 observations in at least one group) occur within Split By level(s) '{levels}'; interpret per-panel tests cautiously."),
                    levels = htmltools::htmlEscape(paste(small_levels, collapse = ", "))), "WARNING")
            }

            TRUE
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

            if (isTRUE(self$options$centralityplotting) && self$options$centralityparameter != "none") {
                centrality_type <- self$options$centralitytype
                if (is.null(centrality_type) || centrality_type == "")
                    centrality_type <- self$options$typestatistics
                implied <- switch(self$options$centralityparameter,
                                  mean = "parametric", median = "nonparametric", NULL)
                if (!is.null(implied) && !identical(implied, centrality_type)) {
                    shown <- switch(centrality_type,
                                    parametric = "mean", nonparametric = "median",
                                    robust = "trimmed mean", bayes = "Bayesian (MAP) estimate",
                                    htmltools::htmlEscape(as.character(centrality_type)))
                    private$.addNotice(.fmt(
                        .("Your two central-tendency settings disagree: 'Central Tendency Display' still carries the legacy value '{legacy}' while 'Central Tendency Measure' selects {selected}. The plot shows the {drawn}, because 'Central Tendency Display' only decides whether a line is drawn at all."),
                        legacy = htmltools::htmlEscape(as.character(self$options$centralityparameter)),
                        selected = switch(centrality_type, parametric = "Mean",
                                          nonparametric = "Median", robust = "Trimmed Mean",
                                          bayes = "Bayesian Estimate",
                                          htmltools::htmlEscape(as.character(centrality_type))),
                        drawn = shown), "WARNING")
                }
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
        # Single implementation lives in R/utils-ggstatsplot.R.
        .withBaseFormulaChar = function(expr) {
            withBaseFormulaChar(expr)
        },

        # How many group levels actually carry data (after stripping <NA> levels).
        .nGroupLevels = function(data, group_var) {
            if (is.null(group_var) || !group_var %in% names(data)) return(NA_integer_)
            nlevels(droplevels(private$.cleanFactor(data[[group_var]])))
        },

        # Clamp Wilcox & Tian (2011) explanatory measure xi and its bootstrap CI
        # to [0, 1] (WRS2::t1way does not clamp bootstrap bounds above 1.0).
        .clampXiExpr = function(expr, digits = 2L) {
            if (is.null(expr)) return(NULL)
            ex <- if (is.expression(expr)) expr[[1]] else expr
            if (!is.call(ex) || length(ex) < 6L) return(expr)
            s <- paste(deparse(ex), collapse = " ")
            if (!grepl("widehat(xi)", s, fixed = TRUE)) return(expr)
            tryCatch({
                est <- suppressWarnings(as.numeric(ex[[4]][[3]]))
                lo  <- suppressWarnings(as.numeric(ex[[5]][[3]][[3]]))
                hi  <- suppressWarnings(as.numeric(ex[[6]][[2]]))
                if (is.finite(est) && (est < 0 || est > 1))
                    ex[[4]][[3]] <- private$.fmtStatVal(min(max(est, 0), 1), digits)
                if (is.finite(lo) && (lo < 0 || lo > 1))
                    ex[[5]][[3]][[3]] <- private$.fmtStatVal(min(max(lo, 0), 1), digits)
                if (is.finite(hi) && (hi < 0 || hi > 1))
                    ex[[6]][[2]] <- private$.fmtStatVal(min(max(hi, 0), 1), digits)
                if (is.expression(expr)) as.expression(ex) else ex
            }, error = function(e) expr)
        },

        # Pure helper for 3+-group Bayesian one-way ANOVA subtitle expression.
        # statsExpressions 2.1.1 errors inside performance::r2_bayes on K >= 3
        # even when BayesFactor::anovaBF succeeds. Try statsExpressions first; on
        # failure, compute exact Rouder et al. (2012) JZS log_e(BF_01) and Gelman
        # et al. (2019) posterior Bayesian R-squared credible interval via Gibbs draws.
        .bayesAnovaSubtitle = function(data, group_var, dep_var, opts, seed_val) {
            df_bf <- data.frame(
                .jj_y = as.numeric(data[[dep_var]]),
                .jj_g = droplevels(private$.cleanFactor(data[[group_var]]))
            )
            df_bf <- df_bf[is.finite(df_bf$.jj_y) & !is.na(df_bf$.jj_g), , drop = FALSE]
            df_bf$.jj_g <- droplevels(df_bf$.jj_g)
            if (nlevels(df_bf$.jj_g) < 2L || nrow(df_bf) <= nlevels(df_bf$.jj_g) ||
                !is.finite(stats::sd(df_bf$.jj_y)) || stats::sd(df_bf$.jj_y) == 0) {
                return(structure(list(), error_msg = .("insufficient within-group variation for Bayesian ANOVA")))
            }

            se_res <- tryCatch(
                withr::with_seed(seed_val,
                    private$.withBaseFormulaChar(
                        statsExpressions::oneway_anova(
                            data       = df_bf,
                            x          = .jj_g,
                            y          = .jj_y,
                            type       = "bayes",
                            digits     = opts$digits,
                            conf.level = opts$conflevel))),
                error = function(e) e)

            if (!inherits(se_res, "condition") &&
                !is.null(se_res$expression) && length(se_res$expression) > 0L &&
                !is.null(se_res$expression[[1]])) {
                return(se_res$expression[[1]])
            }

            if (!requireNamespace("BayesFactor", quietly = TRUE)) {
                err_msg <- if (inherits(se_res, "condition")) conditionMessage(se_res) else .("BayesFactor package unavailable")
                return(structure(list(), error_msg = err_msg))
            }

            bf_calc <- tryCatch(
                withr::with_seed(seed_val,
                    private$.withBaseFormulaChar({
                        bf_obj <- BayesFactor::anovaBF(
                            .jj_y ~ .jj_g, data = df_bf,
                            rscaleFixed = "medium", progress = FALSE
                        )
                        bf_tbl <- BayesFactor::extractBF(bf_obj)
                        bf10 <- as.numeric(bf_tbl$bf[1])
                        log_bf01 <- -log(bf10)
                        post <- BayesFactor::posterior(bf_obj, iterations = 4000L, progress = FALSE)
                        g_cols <- paste0(".jj_g-", levels(df_bf$.jj_g))
                        mu_draws <- as.numeric(post[, "mu"])
                        eff_mat <- as.matrix(post[, g_cols, drop = FALSE])
                        fitted_draws <- mu_draws + eff_mat[, as.integer(df_bf$.jj_g), drop = FALSE]
                        n_obs_bf <- nrow(df_bf)
                        centered <- fitted_draws - rowMeans(fitted_draws)
                        var_fit <- rowSums(centered * centered) / (n_obs_bf - 1L)
                        sig2_draws <- as.numeric(post[, "sig2"])
                        r2_draws <- pmin(pmax(var_fit / (var_fit + sig2_draws), 0), 1)
                        alpha_tail <- (1 - opts$conflevel) / 2
                        r2_med <- stats::median(r2_draws, na.rm = TRUE)
                        r2_ci <- stats::quantile(
                            r2_draws, probs = c(alpha_tail, 1 - alpha_tail),
                            na.rm = TRUE, names = FALSE
                        )
                        ci_lbl <- paste0(round(100 * opts$conflevel), "%")
                        bquote(list(
                            log[e] * (BF["01"]) == .(private$.fmtStatVal(log_bf01, opts$digits)),
                            widehat(italic(R)^2)["Bayesian"] == .(private$.fmtStatVal(r2_med, opts$digits)),
                            CI[.(ci_lbl)]^ETI ~ "[" * .(private$.fmtStatVal(r2_ci[1], opts$digits)),
                            .(private$.fmtStatVal(r2_ci[2], opts$digits)) * "]",
                            italic(r)["Cauchy"]^"JZS" == .(private$.fmtStatVal(0.5, opts$digits)),
                            italic("n")["obs"] == .(as.character(n_obs_bf))
                        ))
                    })
                ),
                error = function(e) e
            )

            if (inherits(bf_calc, "condition")) {
                return(structure(list(), error_msg = conditionMessage(bf_calc)))
            }
            bf_calc
        },

        # Probe whether the base statistical test itself can run on `data` (used
        # only when statsExpressions errors, to distinguish a true uncomputable
        # test from an effsizetype-only fallback).
        .canComputeDefaultTest = function(data, group_var, dep_var, typestatistics) {
            df_t <- data.frame(
                .jj_y = as.numeric(data[[dep_var]]),
                .jj_g = droplevels(private$.cleanFactor(data[[group_var]]))
            )
            df_t <- df_t[is.finite(df_t$.jj_y) & !is.na(df_t$.jj_g), , drop = FALSE]
            df_t$.jj_g <- droplevels(df_t$.jj_g)
            k_g <- nlevels(df_t$.jj_g)
            if (k_g < 2L) return(FALSE)
            ok <- tryCatch(
                private$.withBaseFormulaChar({
                    if (identical(typestatistics, "parametric")) {
                        if (k_g == 2L) stats::t.test(.jj_y ~ .jj_g, data = df_t)
                        else           stats::oneway.test(.jj_y ~ .jj_g, data = df_t)
                    } else if (identical(typestatistics, "nonparametric")) {
                        if (k_g == 2L) stats::wilcox.test(.jj_y ~ .jj_g, data = df_t)
                        else           stats::kruskal.test(.jj_y ~ .jj_g, data = df_t)
                    } else if (identical(typestatistics, "robust") && requireNamespace("WRS2", quietly = TRUE)) {
                        if (k_g == 2L) WRS2::yuen(.jj_y ~ .jj_g, data = df_t)
                        else           WRS2::t1way(.jj_y ~ .jj_g, data = df_t)
                    } else {
                        NULL
                    }
                }),
                error = function(e) NULL
            )
            if (is.null(ok)) return(FALSE)
            stat_v <- if (!is.null(ok$statistic)) as.numeric(ok$statistic[1])
                      else if (!is.null(ok$test)) as.numeric(ok$test[1])
                      else NA_real_
            is.finite(stat_v)
        },

        # Pure helper for 3+-group Kruskal-Wallis H and ordinal epsilon-squared
        # with a 1,000-resample i.i.d. bias-corrected and accelerated (BCa)
        # bootstrap confidence interval clamped to [0, 1] (fixes upward tie bias
        # in statsExpressions/effectsize's 100-draw stratified percentile CI).
        .kruskalBcaSubtitle = function(data, group_var, dep_var, opts, seed_val) {
            df_kw <- data.frame(
                .jj_y = as.numeric(data[[dep_var]]),
                .jj_g = droplevels(private$.cleanFactor(data[[group_var]]))
            )
            df_kw <- df_kw[is.finite(df_kw$.jj_y) & !is.na(df_kw$.jj_g), , drop = FALSE]
            df_kw$.jj_g <- droplevels(df_kw$.jj_g)
            n_obs <- nrow(df_kw)
            k_g <- nlevels(df_kw$.jj_g)
            if (k_g < 3L || n_obs <= k_g || length(unique(df_kw$.jj_y)) < 2L) {
                return(structure(list(), error_msg = .("insufficient variation across groups for Kruskal-Wallis test")))
            }

            calc_kw <- function(yv, gv) {
                N <- length(yv)
                ug <- unique(gv)
                if (length(ug) < 2L || N <= 2L) return(c(H = NA_real_, eps2 = NA_real_))
                r <- rank(yv, ties.method = "average")
                gi <- match(gv, ug)
                ni <- tabulate(gi)
                rs <- as.numeric(rowsum(r, gi, reorder = FALSE))
                H_raw <- (12 / (N * (N + 1))) * sum((rs * rs) / ni) - 3 * (N + 1)
                t_tab <- tabulate(match(yv, unique(yv)))
                denom <- 1 - sum(t_tab^3 - t_tab) / (N^3 - N)
                if (!is.finite(denom) || denom <= 0) return(c(H = NA_real_, eps2 = NA_real_))
                H_val <- max(0, H_raw / denom)
                c(H = H_val, eps2 = min(max(H_val / (N - 1L), 0), 1))
            }

            yv <- df_kw$.jj_y
            gv <- as.integer(df_kw$.jj_g)
            obs_kw <- calc_kw(yv, gv)
            if (!is.finite(obs_kw[["H"]])) {
                return(structure(list(), error_msg = .("the test statistic could not be evaluated (zero within-group spread)")))
            }

            H_stat <- obs_kw[["H"]]
            df_kw_val <- as.integer(k_g - 1L)
            p_val <- stats::pchisq(H_stat, df = df_kw_val, lower.tail = FALSE)
            est <- obs_kw[["eps2"]]

            ci_bounds <- tryCatch(
                withr::with_seed(seed_val, {
                    B <- 1000L
                    boot <- numeric(B)
                    for (b in seq_len(B)) {
                        idb <- sample.int(n_obs, n_obs, replace = TRUE)
                        boot[b] <- calc_kw(yv[idb], gv[idb])[["eps2"]]
                    }
                    boot <- boot[is.finite(boot)]
                    if (length(boot) < 20L) {
                        c(est, est)
                    } else {
                        B_eff <- length(boot)
                        prop_less <- (sum(boot < est) + 0.5 * sum(boot == est)) / B_eff
                        prop_less <- min(max(prop_less, 0.5 / B_eff), 1 - 0.5 / B_eff)
                        z0 <- stats::qnorm(prop_less)
                        if (n_obs <= 300L) {
                            jack <- numeric(n_obs)
                            for (i in seq_len(n_obs)) {
                                jack[i] <- calc_kw(yv[-i], gv[-i])[["eps2"]]
                            }
                        } else {
                            n_blocks <- 200L
                            blk_id <- ((seq_len(n_obs) - 1L) %% n_blocks) + 1L
                            jack <- vapply(seq_len(n_blocks), function(b) {
                                calc_kw(yv[blk_id != b], gv[blk_id != b])[["eps2"]]
                            }, numeric(1))
                        }
                        jack <- jack[is.finite(jack)]
                        acc <- 0
                        if (length(jack) > 2L) {
                            j_diff <- mean(jack) - jack
                            denom_a <- 6 * (sum(j_diff * j_diff))^(1.5)
                            if (is.finite(denom_a) && denom_a > 0) {
                                acc <- sum(j_diff^3) / denom_a
                            }
                        }
                        alpha_tail <- (1 - opts$conflevel) / 2
                        za <- stats::qnorm(c(alpha_tail, 1 - alpha_tail))
                        denom_bca <- 1 - acc * (z0 + za)
                        adj_p <- ifelse(denom_bca <= 1e-6, c(0, 1),
                                        stats::pnorm(z0 + (z0 + za) / denom_bca))
                        adj_p <- pmin(pmax(adj_p, 0), 1)
                        raw_ci <- stats::quantile(boot, probs = adj_p, names = FALSE, type = 7)
                        c(max(0, min(raw_ci[1], est)), min(1, max(raw_ci[2], est)))
                    }
                }),
                error = function(e) c(est, est)
            )

            private$.lastExactP <- p_val
            ci_lbl <- paste0(round(100 * opts$conflevel), "%")
            bquote(list(
                chi["Kruskal-Wallis"]^2 * "(" * .(df_kw_val) * ")" == .(private$.fmtStatVal(H_stat, opts$digits)),
                italic(p) == .(private$.fmtStatVal(p_val, opts$digits)),
                widehat(epsilon)["ordinal"]^2 == .(private$.fmtStatVal(est, opts$digits)),
                CI[.(ci_lbl)] ~ "[" * .(private$.fmtStatVal(ci_bounds[1], opts$digits)),
                .(private$.fmtStatVal(ci_bounds[2], opts$digits)) * "]",
                italic("n")["obs"] == .(as.character(n_obs))
            ))
        },

        # Build the plot subtitle ourselves (also invoked with force_compute = TRUE
        # when showexplanation or post-hoc brackets need omnibus computability/values).
        .subtitleExpr = function(data, group_var, dep_var, opts, force_compute = FALSE) {
            private$.subtitleFallback <- NULL
            private$.subtitleUncomputable <- NULL
            private$.lastExactP <- NA_real_
            if (!isTRUE(opts$resultssubtitle) && !isTRUE(force_compute)) return(NULL)

            grp_clean <- droplevels(private$.cleanFactor(data[[group_var]]))
            grp_counts <- table(grp_clean)
            n_lev <- length(grp_counts)
            if (n_lev < 2L) return(NULL)

            if (any(grp_counts < 2L)) {
                small_grp <- names(grp_counts)[which(grp_counts < 2L)[1]]
                private$.subtitleUncomputable <- .fmt(
                    .("group '{group}' has fewer than 2 observations (n = {n})"),
                    group = small_grp, n = as.integer(grp_counts[[small_grp]]))
                return(NULL)
            }

            if (identical(opts$typestatistics, "robust")) {
                h_counts <- grp_counts - 2L * floor(0.2 * grp_counts)
                if (any(h_counts < 2L)) {
                    small_grp <- names(h_counts)[which(h_counts < 2L)[1]]
                    private$.subtitleUncomputable <- .fmt(
                        .("group '{group}' retains fewer than 2 observations after 20 percent trimming"),
                        group = small_grp)
                    return(NULL)
                }
            }

            df_se <- data.frame(
                .jj_y = as.numeric(data[[dep_var]]),
                .jj_g = grp_clean
            )

            if (identical(opts$typestatistics, "bayes")) {
                if (n_lev == 2L) {
                    res_b2 <- tryCatch(
                        withr::with_seed(private$.seed(),
                            private$.withBaseFormulaChar(
                                statsExpressions::two_sample_test(
                                    data       = df_se,
                                    x          = .jj_g,
                                    y          = .jj_y,
                                    type       = "bayes",
                                    digits     = opts$digits,
                                    conf.level = opts$conflevel))),
                        error = function(e) e)
                    if (inherits(res_b2, "condition") || is.null(res_b2$expression) || length(res_b2$expression) == 0L) {
                        private$.subtitleUncomputable <- if (inherits(res_b2, "condition")) conditionMessage(res_b2) else .("Bayesian t-test returned no expression")
                        return(NULL)
                    }
                    return(res_b2$expression[[1]])
                }

                sub_b3 <- private$.bayesAnovaSubtitle(data, group_var, dep_var, opts, private$.seed())
                err_b3 <- attr(sub_b3, "error_msg", exact = TRUE)
                if (!is.null(err_b3)) {
                    private$.subtitleUncomputable <- err_b3
                    return(NULL)
                }
                return(sub_b3)
            }

            if (identical(opts$typestatistics, "nonparametric") && n_lev >= 3L) {
                sub_kw <- private$.kruskalBcaSubtitle(data, group_var, dep_var, opts, private$.seed())
                err_kw <- attr(sub_kw, "error_msg", exact = TRUE)
                if (!is.null(err_kw)) {
                    private$.subtitleUncomputable <- err_kw
                    return(NULL)
                }
                return(sub_kw)
            }

            # two_sample_test rejects "eta"/"omega" (they are ANOVA-only); map
            # them onto the equivalent two-group family so the selector still
            # means something when there are exactly two groups.
            eff <- opts$effsizetype
            if (n_lev == 2L && identical(eff, "eta"))   eff <- "biased"
            if (n_lev == 2L && identical(eff, "omega")) eff <- "unbiased"

            fn <- if (n_lev == 2L) statsExpressions::two_sample_test
                  else             statsExpressions::oneway_anova

            res <- tryCatch(
                withr::with_seed(private$.seed(),
                    private$.withBaseFormulaChar(fn(
                        data         = df_se,
                        x            = .jj_g,
                        y            = .jj_y,
                        type         = opts$typestatistics,
                        effsize.type = eff,
                        digits       = opts$digits,
                        conf.level   = opts$conflevel))),
                error = function(e) e)

            if (inherits(res, "condition")) {
                if (isTRUE(private$.canComputeDefaultTest(data, group_var, dep_var, opts$typestatistics))) {
                    private$.subtitleFallback <- conditionMessage(res)
                } else {
                    private$.subtitleUncomputable <- conditionMessage(res)
                }
                return(NULL)
            }
            if (is.null(res$expression) || length(res$expression) == 0L) {
                private$.subtitleUncomputable <- .("the statistics engine returned no expression")
                return(NULL)
            }
            if (("statistic" %in% names(res) && !is.finite(res$statistic[1])) ||
                ("p.value" %in% names(res) && !is.finite(res$p.value[1]))) {
                private$.subtitleUncomputable <- .("the test statistic could not be evaluated (zero within-group spread)")
                return(NULL)
            }
            if ("p.value" %in% names(res)) {
                private$.lastExactP <- as.numeric(res$p.value[1])
            }

            private$.clampXiExpr(res$expression[[1]], opts$digits)
        },

        # Build the Bayes-factor caption ourselves, for the same reason as the
        # subtitle.
        .captionExpr = function(data, group_var, dep_var, opts) {
            private$.captionUnavailable <- NULL
            if (!isTRUE(opts$bfmessage)) return(NULL)
            if (!isTRUE(opts$resultssubtitle)) {
                private$.captionUnavailable <- .("the plot is not showing statistical results at all")
                return(NULL)
            }
            if (!is.null(private$.subtitleUncomputable)) {
                return(NULL)
            }
            if (identical(opts$typestatistics, "bayes")) {
                if (!is.null(private$.subtitleCache)) {
                    private$.captionUnavailable <- .("the test is already Bayesian, so the subtitle itself reports the Bayes factor")
                }
                return(NULL)
            }
            if (!identical(opts$typestatistics, "parametric")) {
                private$.captionUnavailable <- .("a Bayes factor is only defined here for the parametric test")
                return(NULL)
            }

            n_lev <- private$.nGroupLevels(data, group_var)
            if (is.na(n_lev) || n_lev != 2) {
                private$.captionUnavailable <- .("the statistics package computes one only for a two-group comparison")
                return(NULL)
            }

            df_cap <- data.frame(
                .jj_y = as.numeric(data[[dep_var]]),
                .jj_g = droplevels(private$.cleanFactor(data[[group_var]]))
            )

            res <- tryCatch(
                withr::with_seed(private$.seed(),
                    private$.withBaseFormulaChar(
                        statsExpressions::two_sample_test(
                            data       = df_cap,
                            x          = .jj_g,
                            y          = .jj_y,
                            type       = "bayes",
                            digits     = opts$digits,
                            conf.level = opts$conflevel))),
                error = function(e) e)

            if (inherits(res, "condition")) {
                private$.captionUnavailable <- conditionMessage(res)
                return(NULL)
            }
            if (is.null(res$expression) || length(res$expression) == 0) {
                private$.captionUnavailable <- .("the statistics engine returned no Bayes factor")
                return(NULL)
            }
            res$expression[[1]]
        },

        # Build a plain-language SAMPL-compliant result summary sentence from
        # cleaned data and the computed omnibus subtitle expression.
        .plainResultSentence = function(data, opts, n_groups, sub_expr, test_line, eff_line) {
            grp <- droplevels(private$.cleanFactor(data[[opts$group]]))
            yv  <- as.numeric(data[[opts$dep]])
            grp_parts <- vapply(levels(grp), function(lv) {
                vg <- yv[grp == lv]
                vg <- vg[is.finite(vg)]
                ng <- length(vg)
                if (opts$typestatistics %in% c("parametric", "bayes")) {
                    m_s  <- if (ng > 0L) private$.fmtStatVal(mean(vg), opts$digits) else "NA"
                    sd_s <- if (ng >= 2L) private$.fmtStatVal(stats::sd(vg), opts$digits) else "NA"
                    sprintf("%s (n = %d, mean = %s, SD = %s)", lv, ng, m_s, sd_s)
                } else if (identical(opts$typestatistics, "nonparametric")) {
                    med_s <- if (ng > 0L) private$.fmtStatVal(stats::median(vg), opts$digits) else "NA"
                    iqr_s <- if (ng > 0L) private$.fmtStatVal(stats::IQR(vg), opts$digits) else "NA"
                    sprintf("%s (n = %d, median = %s, IQR = %s)", lv, ng, med_s, iqr_s)
                } else {
                    tm_s  <- if (ng > 0L) private$.fmtStatVal(mean(vg, trim = 0.2), opts$digits) else "NA"
                    iqr_s <- if (ng > 0L) private$.fmtStatVal(stats::IQR(vg), opts$digits) else "NA"
                    sprintf("%s (n = %d, 20%% trimmed mean = %s, IQR = %s)", lv, ng, tm_s, iqr_s)
                }
            }, character(1))
            grp_summary_txt <- paste(grp_parts, collapse = "; ")
            n_tot <- sum(is.finite(yv) & !is.na(grp))

            if (!is.null(private$.subtitleUncomputable) || is.null(sub_expr)) {
                reason_txt <- if (!is.null(private$.subtitleUncomputable)) {
                    private$.subtitleUncomputable
                } else {
                    .("insufficient within-group variation")
                }
                return(.fmt(
                    .("Across N = {total} complete observations ({groupsummary}), {test} could not be evaluated ({reason}), so no omnibus test statistic, interval, or post-hoc pairwise brackets were produced."),
                    total = n_tot, groupsummary = grp_summary_txt,
                    test = test_line, reason = reason_txt
                ))
            }

            ex <- if (is.expression(sub_expr)) sub_expr[[1]] else sub_expr
            parsed <- tryCatch({
                if (identical(opts$typestatistics, "bayes")) {
                    logbf_s <- as.character(ex[[2]][[3]])
                    eff_s   <- as.character(ex[[3]][[3]])
                    lo_s    <- as.character(ex[[4]][[3]][[3]])
                    hi_s    <- as.character(ex[[5]][[2]])
                    lbf_num <- suppressWarnings(as.numeric(logbf_s))
                    ev_phrase <- if (!is.finite(lbf_num)) {
                        .("relative evidence evaluated under the JZS prior")
                    } else if (lbf_num < -2.30) {
                        .("strong relative evidence for a group difference")
                    } else if (lbf_num < -1.10) {
                        .("moderate relative evidence for a group difference")
                    } else if (lbf_num < 0) {
                        .("weak relative evidence toward a group difference")
                    } else if (lbf_num == 0) {
                        .("equal relative support for the null and alternative models")
                    } else if (lbf_num <= 1.10) {
                        .("weak relative evidence toward the null hypothesis of no difference")
                    } else if (lbf_num <= 2.30) {
                        .("moderate relative evidence favoring the null hypothesis of no difference")
                    } else {
                        .("strong relative evidence favoring the null hypothesis of no difference")
                    }
                    .fmt(
                        .("Across N = {total} complete observations ({groupsummary}), {test} yielded log_e(BF_01) = {logbf} ({evidence}), with {effname} = {effval} ({level} per cent credible interval {cilow} to {cihigh})."),
                        total = n_tot, groupsummary = grp_summary_txt, test = test_line,
                        logbf = logbf_s, evidence = ev_phrase, effname = eff_line,
                        effval = eff_s, level = base::format(100 * opts$conflevel),
                        cilow = lo_s, cihigh = hi_s
                    )
                } else {
                    stat_s <- as.character(ex[[2]][[3]])
                    p_s    <- as.character(ex[[3]][[3]])
                    eff_s  <- as.character(ex[[4]][[3]])
                    lo_s   <- as.character(ex[[5]][[3]][[3]])
                    hi_s   <- as.character(ex[[6]][[2]])
                    p_num  <- if (is.finite(private$.lastExactP)) private$.lastExactP else suppressWarnings(as.numeric(p_s))
                    stat_sym <- switch(opts$typestatistics,
                        parametric    = if (n_groups == 2) "t" else "F",
                        nonparametric = if (n_groups == 2) "W" else "chi-squared",
                        robust        = if (n_groups == 2) "|t|" else "F",
                        "statistic"
                    )
                    verdict_txt <- if (is.finite(p_num) && p_num < 0.05) {
                        .("statistically significant at the 0.05 threshold")
                    } else {
                        .("not statistically significant at the 0.05 threshold; absence of significance does not establish equivalence")
                    }
                    .fmt(
                        .("Across N = {total} complete observations ({groupsummary}), {test} gave {statname} = {statval}, p = {pval} ({verdict}), with {effname} = {effval} ({level} per cent confidence interval {cilow} to {cihigh})."),
                        total = n_tot, groupsummary = grp_summary_txt, test = test_line,
                        statname = stat_sym, statval = stat_s, pval = p_s, verdict = verdict_txt,
                        effname = eff_line, effval = eff_s, level = base::format(100 * opts$conflevel),
                        cilow = lo_s, cihigh = hi_s
                    )
                }
            }, error = function(e) NULL)

            if (is.null(parsed)) {
                return(.fmt(
                    .("Group summary across N = {total} complete observations: {groupsummary}."),
                    total = n_tot, groupsummary = grp_summary_txt
                ))
            }
            parsed
        },

        # The "Assumptions and interpretation" panel.
        # Translucent rgba() fills and inherited colour, never an opaque hex
        # background: jamovi renders results in a dark theme too.
        .renderExplanation = function(data, opts, n_groups, sub_expr = NULL) {
            if (!isTRUE(self$options$showexplanation)) return()

            omnibus <- if (n_groups == 2) .("two-group comparison") else .("omnibus comparison across all groups")
            uncomputable <- !is.null(private$.subtitleUncomputable)

            assumptions <- switch(opts$typestatistics,
                parametric = c(
                    .("The values are roughly symmetric within each group. The test tolerates mild skew at moderate sample sizes but not a heavy tail or a floor/ceiling effect - inspect the violins."),
                    .("Groups do NOT have to share the same spread. Welch's unequal-variance correction is used throughout, which is why the degrees of freedom are fractional."),
                    .("Observations are independent. Repeated measurements on the same patient, or multiple cores/blocks from one specimen, violate independence and need a paired or mixed-effects model instead.")),
                nonparametric = c(
                    .("No normality assumption. The test compares ranks, so it evaluates whether one group tends to yield larger values rather than whether arithmetic means differ."),
                    .("Groups should share a broadly similar distribution shape if you interpret the result as a median shift; otherwise interpret it as stochastic dominance."),
                    .("Observations are independent across patients or specimens; clustered or repeated measurements require a paired or mixed-effects rank procedure.")),
                robust = c(
                    .("The comparison uses 20 percent trimmed means, discarding the most extreme 20 percent in each tail of every group before testing. Use this when real extreme observations should neither be deleted nor dominate the estimate."),
                    .("Groups do NOT have to share the same spread."),
                    .("Observations are independent across patients or specimens. In small groups, trimming 20 percent from each tail leaves few effective observations - check per-group sizes.")),
                bayes = c(
                    .("The Bayes factor is computed under a default Jeffreys-Zellner-Siow Cauchy prior on standardized effects (r = 0.707 for two groups; r = 0.5 for three or more groups) assuming a shared residual variance across groups. Treat it as relative evidence under a conventional prior, not as a posterior probability that a hypothesis is true."),
                    .("Observations are independent across patients or specimens; repeated or multi-core measurements from one patient violate independence."),
                    .("The figure displays log_e(BF_01), the natural logarithm of the Bayes factor favoring the null over the alternative: 0 indicates equal support (BF_01 = 1), negative values favor a group difference (below -1.10 moderate evidence, BF_10 > 3; below -2.30 strong evidence, BF_10 > 10), and positive values (above +1.10 or +2.30) favor the null hypothesis of no difference.")),
                .("Observations are independent."))

            test_line <- switch(opts$typestatistics,
                parametric    = if (n_groups == 2) .("Welch's t-test") else .("Welch's one-way analysis of variance"),
                nonparametric = if (n_groups == 2) .("the Mann-Whitney U test") else .("the Kruskal-Wallis test"),
                robust        = if (n_groups == 2) .("Yuen's trimmed-mean test") else .("a heteroscedastic one-way comparison of trimmed means"),
                bayes         = if (n_groups == 2) .("a Bayesian t-test") else .("a Bayesian one-way analysis of variance"),
                .("the selected test"))

            eff_line <- switch(opts$typestatistics,
                parametric = if (n_groups == 2)
                        switch(opts$effsizetype, unbiased = , omega = .("Hedges' g"), .("Cohen's d"))
                    else
                        switch(opts$effsizetype, unbiased = , omega = .("partial omega-squared"),
                               .("partial eta-squared")),
                nonparametric = if (n_groups == 2) .("the rank-biserial correlation")
                                else .("ordinal epsilon-squared"),
                robust        = if (n_groups == 2) .("the robust (AKP) standardized difference")
                                else .("the explanatory measure of effect size (xi)"),
                bayes         = if (n_groups == 2) .("the posterior median difference")
                                else .("posterior Bayesian R-squared"),
                .("the selected effect size"))

            interval_word <- if (identical(opts$typestatistics, "bayes"))
                .("credible") else .("confidence")

            if (n_groups <= 2) {
                pw_reading <- ""
                pw_methods <- ""
            } else if (uncomputable) {
                pw_suppressed <- .("Post-hoc pairwise brackets were suppressed because the omnibus test could not be evaluated.")
                pw_reading <- paste0(" ", htmltools::htmlEscape(pw_suppressed))
                pw_methods <- ""
            } else if (identical(self$options$pairwisedisplay, "none")) {
                pw_reading <- paste0(" ", htmltools::htmlEscape(.("No pairwise comparisons were drawn.")))
                pw_methods <- paste0(" ", .("No post-hoc pairwise comparisons were drawn."))
            } else if (identical(opts$typestatistics, "bayes")) {
                pw_msg <- .("Pairwise comparisons between every pair of groups are shown as brackets reporting unadjusted natural-log JZS Bayes factors (log_e(BF_01)).")
                pw_reading <- paste0(" ", htmltools::htmlEscape(pw_msg))
                pw_methods <- paste0(" ", pw_msg)
            } else if (identical(opts$typestatistics, "parametric") &&
                       identical(self$options$padjustmethod, "holm")) {
                pw_msg <- .fmt(
                    .("Pairwise comparisons between {which} are shown as brackets using the Games-Howell single-step studentized-range procedure for family-wise error control."),
                    which = if (identical(self$options$pairwisedisplay, "all"))
                                .("every pair of groups") else .("the pairs that reached significance"))
                pw_reading <- paste0(" ", htmltools::htmlEscape(pw_msg))
                pw_methods <- paste0(" ", pw_msg)
            } else if (identical(opts$typestatistics, "parametric")) {
                adj <- switch(self$options$padjustmethod,
                              fdr = .("Benjamini-Hochberg"),
                              bonferroni = .("Bonferroni"),
                              none = .("no"),
                              self$options$padjustmethod)
                pw_msg <- .fmt(
                    .("Pairwise comparisons between {which} are shown as brackets using pairwise Welch t-tests with {adjustment} adjustment for multiple testing."),
                    which = if (identical(self$options$pairwisedisplay, "all"))
                                .("every pair of groups") else .("the pairs that reached significance"),
                    adjustment = adj)
                pw_reading <- paste0(" ", htmltools::htmlEscape(pw_msg))
                pw_methods <- paste0(" ", pw_msg)
            } else {
                adj <- switch(self$options$padjustmethod,
                              holm = .("Holm"), fdr = .("Benjamini-Hochberg"),
                              bonferroni = .("Bonferroni"), none = .("no"),
                              self$options$padjustmethod)
                pw_msg <- .fmt(
                    .("Pairwise comparisons between {which} are shown as brackets, with {adjustment} adjustment for multiple testing."),
                    which = if (identical(self$options$pairwisedisplay, "all"))
                                .("every pair of groups") else .("the pairs that reached significance"),
                    adjustment = adj)
                pw_reading <- paste0(" ", htmltools::htmlEscape(pw_msg))
                pw_methods <- paste0(" ", pw_msg)
            }

            model_note <- if (identical(opts$typestatistics, "parametric") && n_groups > 2) {
                paste0("<p>", .("The effect size is estimated from the Welch model rather than an equal-variance ANOVA, and its interval is reported as a one-sided lower confidence bound up to 1.00 (Steiger, 2004)."), "</p>")
            } else if (identical(opts$typestatistics, "nonparametric") && n_groups == 2) {
                paste0("<p>", .("The Mann-Whitney p value uses the tie-adjusted normal approximation with continuity correction."), "</p>")
            } else if (identical(opts$typestatistics, "nonparametric") && n_groups > 2) {
                paste0("<p>", .("Ordinal epsilon-squared is accompanied by a two-sided bias-corrected and accelerated (BCa) bootstrap confidence interval across 1,000 resamples."), "</p>")
            } else if (identical(opts$typestatistics, "robust") && n_groups == 2) {
                paste0("<p>", .("Yuen's test reports an unsigned |t| statistic, while the robust AKP effect size standardizes the trimmed-mean difference by the first group's Winsorized spread."), "</p>")
            } else {
                ""
            }

            seed_note <- if (statsSeedMatters(opts$typestatistics, "between", n_groups) &&
                             !identical(opts$typestatistics, "bayes")) {
                paste0("<p>", .fmt(
                    .("The confidence interval on the effect size is obtained by bootstrap resampling with a fixed random seed ({seed}) so this figure reproduces exactly; change the seed under Plot Configuration to verify that a borderline interval is stable across resamples."),
                    seed = private$.seed()), "</p>")
            } else if (identical(opts$typestatistics, "bayes") ||
                       captionSeedMatters(opts$typestatistics, "between", n_groups,
                                          isTRUE(opts$bfmessage) && isTRUE(opts$resultssubtitle))) {
                paste0("<p>", .fmt(
                    .("Bayesian posterior quantities and credible intervals use Markov chain Monte Carlo sampling with fixed random seed {seed} for exact reproducibility."),
                    seed = private$.seed()), "</p>")
            } else {
                ""
            }

            result_txt <- private$.plainResultSentence(
                data, opts, n_groups, sub_expr, test_line, eff_line
            )

            methods <- if (uncomputable) {
                .fmt(
                    .("Distributions of {outcome} across {groups} groups of {factor} (N = {total}) were displayed using horizontal box-violin plots; formal hypothesis testing via {test} was not estimable ({reason})."),
                    outcome = opts$dep, groups = n_groups, factor = opts$group,
                    total = nrow(data), test = test_line,
                    reason = private$.subtitleUncomputable %||% .("zero within-group variation")
                )
            } else {
                paste0(
                    .fmt(
                        .("A comparison of {outcome} across {groups} groups of {factor} was performed using {test}, with {effect} reported as the effect size and a {level} per cent {interval} interval."),
                        outcome = opts$dep, groups = n_groups, factor = opts$group,
                        test = test_line, effect = eff_line,
                        level = base::format(100 * opts$conflevel),
                        interval = interval_word),
                    pw_methods
                )
            }

            html <- paste0(
                "<div style='background-color: rgba(127,127,127,0.08); color: inherit; border-left: 4px solid rgba(66,133,244,0.7); padding: 12px 16px; margin: 8px 0;'>",
                "<h3 style='margin-top:0; color: inherit;'>", .("Result summary"), "</h3>",
                "<p>", htmltools::htmlEscape(result_txt), "</p>",
                "<h3 style='color: inherit;'>", .("What this analysis assumes"), "</h3><ul>",
                paste0("<li>", vapply(assumptions, htmltools::htmlEscape, ""), "</li>", collapse = ""),
                "</ul>",
                model_note, seed_note,
                "<h3 style='color: inherit;'>", .("Reading the figure"), "</h3>",
                "<p>", .("Each group is drawn as a violin (the shape of its distribution), a boxplot (median and quartiles) and its individual observations."),
                " ", .("The comparison is horizontal, so values run along the bottom axis and group labels down the side."),
                pw_reading, "</p>",
                "<h3 style='color: inherit;'>", .("Methods sentence you can copy"), "</h3>",
                "<p style='font-style: italic;'>", htmltools::htmlEscape(methods), "</p>",
                "<p><strong>", .("Before you report this"), ":</strong> ",
                .fmt(
                    .("The {comparison} tests whether the groups differ overall, not by how much any one pair differs or whether a difference matters clinically. Read the effect size and its interval alongside the test statistic."),
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
        },
        
        
        
        
        # Data quality validation helper
        .validateDataQuality = function(mydata, dep_var) {
            num_vals <- jmvcore::toNumeric(mydata[[dep_var]])
            num_vals <- num_vals[!is.na(num_vals)]
            
            if (length(num_vals) < 3) {
                private$.accumulateDataMessage(
                    paste0("<br> ", .fmt(
                        .("Warning: {name} has fewer than 3 valid observations."),
                        name = htmltools::htmlEscape(dep_var)), "<br>")
                )
            }
            if (length(unique(num_vals)) < 2) {
                private$.accumulateDataMessage(
                    paste0("<br> ", .fmt(
                        .("Warning: {name} has no variation - every value is the same."),
                        name = htmltools::htmlEscape(dep_var)), "<br>")
                )
            }
        },
        
        # Within-group IQR outlier detection helper (conditions on group so well-separated
        # clinical cohorts are not falsely flagged as marginal pooled outliers).
        .detectOutliers = function(data, var, group_var = self$options$group) {
            vals <- jmvcore::toNumeric(data[[var]])
            if (is.null(group_var) || !(group_var %in% names(data))) return()
            grps <- droplevels(private$.cleanFactor(data[[group_var]]))
            if (sum(is.finite(vals)) == 0L || nlevels(grps) == 0L) return()

            private$.checkpoint()
            flag <- rep(FALSE, length(vals))
            for (lvl in levels(grps)) {
                idx <- which(grps == lvl & is.finite(vals))
                if (length(idx) < 4L) next
                g_vals <- vals[idx]
                q1 <- stats::quantile(g_vals, 0.25, na.rm = TRUE)
                q3 <- stats::quantile(g_vals, 0.75, na.rm = TRUE)
                iqr_val <- q3 - q1
                if (is.finite(iqr_val) && iqr_val > 0) {
                    flag[idx] <- g_vals < (q1 - 1.5 * iqr_val) | g_vals > (q3 + 1.5 * iqr_val)
                }
            }
            n_out <- sum(flag)
            if (n_out > 0L) {
                private$.accumulateDataMessage(
                    paste0("<br> ", .fmt(
                        .("Potential outliers detected within groups in {name}: {count}."),
                        name = htmltools::htmlEscape(var),
                        count = n_out), "<br>")
                )
            }
        },

        # Statistical summary helper.
        .addDataSummary = function(data, dep_var, group_var) {
            if (is.null(dep_var) || is.null(group_var)) return()
            n_groups <- nlevels(droplevels(private$.cleanFactor(data[[group_var]])))
            total_n <- sum(is.finite(data[[dep_var]]))
            private$.accumulateDataMessage(
                paste0("<br> ", .fmt(
                    .("Analysis summary: {groups} groups, {total} total observations."),
                    groups = n_groups, total = total_n), "<br>")
            )
        },

        # Subset cleaned unsplit data to non-missing Split By observations for plot2
        .prepareSplitData = function(mydata) {
            grvar <- self$options$grvar
            grp <- self$options$group
            if (is.null(grvar) || !(grvar %in% names(mydata)))
                return(mydata[0, , drop = FALSE])
            keep <- !is.na(mydata[[grvar]])
            sp_df <- mydata[keep, , drop = FALSE]
            if (nrow(sp_df) > 0L) {
                sp_df[[grvar]] <- droplevels(private$.cleanFactor(sp_df[[grvar]]))
                sp_df[[grp]] <- droplevels(private$.cleanFactor(sp_df[[grp]]))
            }
            sp_df
        },

        # Optimized data preparation with robust caching (keyed on selected data columns)
        .prepareData = function(force_refresh = FALSE, emit_messages = TRUE) {
            dep_col <- if (!is.null(self$options$dep) && self$options$dep %in% names(self$data))
                self$data[[self$options$dep]] else NULL
            group_col <- if (!is.null(self$options$group) && self$options$group %in% names(self$data))
                self$data[[self$options$group]] else NULL
            grvar_col <- if (!is.null(self$options$grvar) && self$options$grvar %in% names(self$data))
                self$data[[self$options$grvar]] else NULL

            current_hash <- digest::digest(list(
                dep = self$options$dep,
                group = self$options$group,
                grvar = self$options$grvar,
                data_dim = dim(self$data),
                col_names = names(self$data),
                dep_vals = dep_col,
                group_vals = group_col,
                grvar_vals = grvar_col
            ), algo = "md5")

            if (!is.null(private$.processedData) &&
                private$.data_hash == current_hash &&
                !force_refresh) {
                if (isTRUE(emit_messages)) {
                    for (msg in private$.data_messages)
                        private$.accumulateMessage(msg)
                }
                return(private$.processedData)
            }

            if (isTRUE(emit_messages)) {
                private$.messages <- NULL
            }
            private$.data_messages <- NULL
            private$.accumulateDataMessage(
                paste0("<br>", .("Processing data for the comparison..."), "<br><hr>")
            )

            start_time <- Sys.time()
            mydata <- self$data

            dep_var <- self$options$dep
            if (!is.null(dep_var) && dep_var %in% names(mydata)) {
                mydata[[dep_var]] <- suppressWarnings(jmvcore::toNumeric(mydata[[dep_var]]))
            }

            group_var <- self$options$group
            if (!is.null(group_var) && group_var %in% names(mydata)) {
                mydata[[group_var]] <- private$.cleanFactor(mydata[[group_var]])
            }
            if (!is.null(self$options$grvar) && self$options$grvar %in% names(mydata)) {
                mydata[[self$options$grvar]] <- private$.cleanFactor(mydata[[self$options$grvar]])
            }

            # Selective omission on (dep, group) ONLY so missingness in optional
            # Split By (grvar) never drops complete patients from unsplit outputs.
            if (!is.null(dep_var) && dep_var %in% names(mydata) &&
                !is.null(group_var) && group_var %in% names(mydata)) {
                private$.checkpoint()

                before_lv <- levels(droplevels(private$.cleanFactor(self$data[[group_var]])))
                n_before <- nrow(mydata)

                na_keep <- !is.na(mydata[[dep_var]]) & !is.na(mydata[[group_var]])
                mydata <- mydata[na_keep, , drop = FALSE]
                n_after_na <- nrow(mydata)
                n_missing_dropped <- n_before - n_after_na

                private$.nonFiniteDropped <- 0L
                finite_rows <- is.finite(mydata[[dep_var]])
                if (any(!finite_rows)) {
                    private$.nonFiniteDropped <- sum(!finite_rows)
                    mydata <- mydata[finite_rows, , drop = FALSE]
                    private$.accumulateDataMessage(
                        paste0("<br> ", .fmt(
                            .("Rows excluded because {name} held an infinite value: {count}. Infinite values usually indicate a division by zero or an out-of-range entry - check the source data."),
                            count = private$.nonFiniteDropped,
                            name = htmltools::htmlEscape(dep_var)), "<br>")
                    )
                }

                mydata[[group_var]] <- droplevels(mydata[[group_var]])
                after_lv <- levels(mydata[[group_var]])
                gone <- setdiff(before_lv, after_lv)
                if (length(gone) > 0) {
                    private$.accumulateDataMessage(
                        paste0("<br> <strong>", .("Groups dropped"), ":</strong> ",
                               .fmt(
                                   .("{names} had no usable measurements and were excluded from the comparison entirely."),
                                   names = htmltools::htmlEscape(paste(gone, collapse = ", "))), "<br>")
                    )
                }

                if (n_missing_dropped > 0L) {
                    n_kept <- nrow(mydata)
                    private$.accumulateDataMessage(
                        paste0("<br> ", .fmt(
                            .("Rows excluded for missing values in the analysis variables: {dropped}."),
                            dropped = n_missing_dropped), "<br>",
                            .fmt(
                                .("Rows with data: {kept} of {total} ({percent} percent)."),
                                kept = n_kept, total = n_before,
                                percent = round(100 * n_kept / n_before, 1)), "<br>")
                    )
                }

                if (!is.null(self$options$grvar) && self$options$grvar %in% names(mydata)) {
                    n_grvar_na <- sum(is.na(mydata[[self$options$grvar]]))
                    if (n_grvar_na > 0L) {
                        private$.accumulateDataMessage(
                            paste0("<br> ", .fmt(
                                .("Split By note: {count} observation(s) with missing {grvar} are excluded from the Split By panels only."),
                                count = n_grvar_na,
                                grvar = htmltools::htmlEscape(self$options$grvar)), "<br>")
                        )
                    }
                }
            }

            if (!is.null(dep_var) && dep_var %in% names(mydata)) {
                private$.validateDataQuality(mydata, dep_var)
            }

            if (nrow(mydata) > 10 && !is.null(dep_var) && dep_var %in% names(mydata)) {
                private$.detectOutliers(mydata, dep_var, group_var)
            }

            private$.addDataSummary(mydata, dep_var, group_var)

            elapsed <- difftime(Sys.time(), start_time, units = "secs")
            if (nrow(mydata) > 1000) {
                private$.accumulateDataMessage(
                    paste0("<br> ", .fmt(
                        .("Large dataset processed in {seconds} seconds."),
                        seconds = round(elapsed, 2)), "<br>")
                )
            }

            private$.processedData <- mydata
            private$.data_hash <- current_hash
            if (isTRUE(emit_messages)) {
                for (msg in private$.data_messages)
                    private$.accumulateMessage(msg)
            }
            return(mydata)
        },

        # Optimized options preparation with robust caching
        .prepareOptions = function(force_refresh = FALSE, emit_messages = FALSE) {
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
                display = list(
                    self$options$resultssubtitle,
                    self$options$originaltheme,
                    self$options$colorpalette
                )
            ), algo = "md5")
            
            # Only reprocess if options have changed or forced refresh
            if (!is.null(private$.processedOptions) && 
                private$.options_hash == current_options_hash && 
                !force_refresh) {
                return(private$.processedOptions)
            }

            # Add options preparation feedback if not already processing
            if (isTRUE(emit_messages) && is.null(private$.messages)) {
                private$.accumulateMessage(
                    paste0("<br>", .("Preparing analysis options..."), "<br><hr>")
                )
            }

            # Process type of statistics
            typestatistics <- self$options$typestatistics

            # Process variables
            dep <- self$options$dep
            group <- self$options$group

            # Centrality settings mapped to ggstatsplot arguments (disagreement notice
            # is emitted unconditionally on every .run() in .validateCentralityOptions()).
            centrality_plotting <- isTRUE(self$options$centralityplotting) && self$options$centralityparameter != "none"
            centrality_type <- self$options$centralitytype
            if (is.null(centrality_type) || centrality_type == "")
                centrality_type <- typestatistics

            # Compute axis labels respecting orientation flip (values on x-axis)
            xlab <- self$options$ytitle
            if (xlab == "") xlab <- group
            ylab <- self$options$xtitle
            if (ylab == "") ylab <- dep

            # Process titles
            mytitle <- self$options$mytitle
            if (mytitle == "") mytitle <- NULL

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
                conflevel = if (is.numeric(self$options$conflevel) &&
                                length(self$options$conflevel) == 1L &&
                                is.finite(self$options$conflevel)) {
                    min(max(self$options$conflevel, 0.5), 0.999)
                } else {
                    0.95
                },
                digits = if (is.numeric(self$options$k) &&
                             length(self$options$k) == 1L &&
                             is.finite(self$options$k)) {
                    min(max(as.integer(self$options$k), 0L), 5L)
                } else {
                    2L
                },
                testvalueline = self$options$testvalueline,
                centralityparameter = self$options$centralityparameter,
                centralityk = self$options$centralityk,
                resultssubtitle = self$options$resultssubtitle,
                originaltheme = self$options$originaltheme,
                colorpalette = if (!is.null(self$options$colorpalette) &&
                                   nzchar(self$options$colorpalette)) {
                    self$options$colorpalette
                } else {
                    "jamovi"
                }
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
            if (isTRUE(self$options$testvalueline) && is.finite(self$options$testvalue)) {
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

        # Prevent statsExpressions::pairwise_comparisons from double-adjusting
        # PMCMRplus::gamesHowellTest studentized-range FWER p-values on K >= 3
        # parametric comparisons under Holm (default), and compute genuine
        # BH-FDR, Bonferroni, or unadjusted pairwise Welch t-test p-values when
        # those multiplicity options are explicitly chosen.
        .fixParametricPairwise = function(plot_obj, sub_data, group_var, dep_var, opts) {
            if (is.null(plot_obj) || !inherits(plot_obj, "gg")) return(plot_obj)
            if (identical(opts$pairwise.display, "none") ||
                !identical(opts$typestatistics, "parametric"))
                return(plot_obj)
            n_lev <- private$.nGroupLevels(sub_data, group_var)
            if (is.na(n_lev) || n_lev < 3L) return(plot_obj)

            sig_idx <- which(vapply(plot_obj$layers,
                                    function(l) inherits(l$geom, "GeomSignif"), logical(1)))
            if (length(sig_idx) == 0L) return(plot_obj)
            idx <- sig_idx[1]
            sp <- plot_obj$layers[[idx]]$stat_params
            comps <- sp$comparisons
            if (is.null(comps) || length(comps) == 0L) return(plot_obj)

            if (identical(opts$p.adjust.method, "holm")) {
                sp$annotations <- gsub("italic(p)[unadj.]", 'italic(p)["GH-adj."]',
                                       sp$annotations, fixed = TRUE)
                plot_obj$layers[[idx]]$stat_params <- sp
                return(plot_obj)
            }

            # padjustmethod %in% c("fdr" -> "BH", "bonferroni", "none"):
            # compute pairwise unequal-variance Welch t-test p-values across all K*(K-1)/2 pairs
            y_vec <- as.numeric(sub_data[[dep_var]])
            g_vec <- droplevels(private$.cleanFactor(sub_data[[group_var]]))
            lvls  <- levels(g_vec)
            all_pairs <- utils::combn(lvls, 2L, simplify = FALSE)
            raw_all <- vapply(all_pairs, function(pair) {
                tt <- tryCatch(
                    stats::t.test(
                        x = y_vec[g_vec == pair[1]],
                        y = y_vec[g_vec == pair[2]],
                        var.equal = FALSE
                    ),
                    error = function(e) NULL
                )
                if (is.null(tt) || !is.finite(tt$p.value)) NA_real_ else unname(tt$p.value)
            }, numeric(1))

            adj_all <- if (identical(opts$p.adjust.method, "none")) {
                raw_all
            } else {
                stats::p.adjust(raw_all, method = opts$p.adjust.method)
            }

            all_keys <- vapply(all_pairs, function(pr) paste(pr[1], pr[2], sep = "\r"), character(1))
            comp_keys <- vapply(comps, function(pr) {
                k1 <- paste(pr[1], pr[2], sep = "\r")
                if (k1 %in% all_keys) k1 else paste(pr[2], pr[1], sep = "\r")
            }, character(1))
            pvals <- adj_all[match(comp_keys, all_keys)]

            keep_mask <- if (identical(opts$pairwise.display, "significant")) {
                !is.na(pvals) & pvals < 0.05
            } else {
                !is.na(pvals)
            }

            for (sc in plot_obj$scales$scales) {
                if (!is.null(sc$secondary.axis) && !inherits(sc$secondary.axis, "waiver")) {
                    if (identical(opts$p.adjust.method, "none")) {
                        sc$secondary.axis$name <- as.expression(bquote(list(
                            "Pairwise test:" ~ bold("Welch's t"),
                            "Bars shown:" ~ bold(.(opts$pairwise.display)),
                            alpha == 0.05
                        )))
                    } else {
                        adj_lbl <- if (identical(opts$p.adjust.method, "BH")) "BH" else "Bonferroni"
                        sc$secondary.axis$name <- as.expression(bquote(list(
                            "Pairwise test:" ~ bold("Welch's t"),
                            "Bars shown:" ~ bold(.(opts$pairwise.display)),
                            "p-value adjustment:" ~ bold(.(adj_lbl))
                        )))
                    }
                }
            }

            if (!any(keep_mask)) {
                plot_obj$layers[[idx]] <- NULL
                return(plot_obj)
            }

            sub_tag <- switch(opts$p.adjust.method,
                BH         = '"FDR-corrected"',
                bonferroni = '"Bonferroni-corrected"',
                "unadj."
            )
            sp$comparisons <- comps[keep_mask]
            sp$annotations <- vapply(pvals[keep_mask], function(pv) {
                paste0("list(italic(p)[", sub_tag, '] == "', private$.fmtStatVal(pv, opts$digits), '")')
            }, character(1))
            plot_obj$layers[[idx]]$stat_params <- sp
            plot_obj
        },

        # Replace ggstatsplot's misleading Bayesian secondary-axis pairwise label
        # ("Pairwise test: Student's t, Bars shown: all, alpha == 0.05") with an
        # accurate Bayesian JZS label free of frequentist alpha claims.
        .fixBayesPairwise = function(plot_obj, sub_data, group_var, opts) {
            if (is.null(plot_obj) || !inherits(plot_obj, "gg")) return(plot_obj)
            if (identical(opts$pairwise.display, "none") ||
                !identical(opts$typestatistics, "bayes"))
                return(plot_obj)
            n_lev <- private$.nGroupLevels(sub_data, group_var)
            if (is.na(n_lev) || n_lev < 3L) return(plot_obj)

            for (sc in plot_obj$scales$scales) {
                if (!is.null(sc$secondary.axis) && !inherits(sc$secondary.axis, "waiver")) {
                    sc$secondary.axis$name <- expression(list(
                        "Pairwise test:" ~ bold("Bayesian JZS t-test"),
                        "Pairs shown:" ~ bold("all")
                    ))
                }
            }
            plot_obj
        },

        # Harmonize Split-By (plot2) sub-panels with main-plot post-hoc fixes,
        # Wilcox-Tian xi [0, 1] clamping, Kruskal-Wallis BCa CIs, and 3+-group
        # Bayesian ANOVA subtitles.
        .enhanceSplitPanels = function(grouped_plot, group_var, dep_var, opts, seed_val) {
            if (is.null(grouped_plot)) return(grouped_plot)

            enhance_one <- function(panel_i) {
                if (!inherits(panel_i, "gg") || is.null(panel_i$data) ||
                    !all(c(group_var, dep_var) %in% names(panel_i$data))) {
                    return(panel_i)
                }
                panel_i <- private$.fixParametricPairwise(
                    panel_i, panel_i$data, group_var, dep_var, opts
                )
                panel_i <- private$.fixBayesPairwise(
                    panel_i, panel_i$data, group_var, opts
                )
                if (isTRUE(opts$resultssubtitle)) {
                    k_pan <- private$.nGroupLevels(panel_i$data, group_var)
                    if (identical(opts$typestatistics, "nonparametric") && !is.na(k_pan) && k_pan >= 3L) {
                        sub_kw_i <- private$.kruskalBcaSubtitle(
                            panel_i$data, group_var, dep_var, opts, seed_val
                        )
                        if (is.null(attr(sub_kw_i, "error_msg", exact = TRUE))) {
                            panel_i <- panel_i + ggplot2::labs(subtitle = sub_kw_i)
                        }
                    } else if (!is.null(panel_i$labels$subtitle)) {
                        panel_i$labels$subtitle <- private$.clampXiExpr(
                            panel_i$labels$subtitle, opts$digits
                        )
                    } else if (identical(opts$typestatistics, "bayes") &&
                               !is.na(k_pan) && k_pan >= 3L) {
                        sub_i <- private$.bayesAnovaSubtitle(
                            panel_i$data, group_var, dep_var, opts, seed_val
                        )
                        if (is.null(attr(sub_i, "error_msg", exact = TRUE))) {
                            panel_i <- panel_i + ggplot2::labs(subtitle = sub_i)
                        }
                    }
                }
                panel_i
            }

            if (inherits(grouped_plot, "patchwork")) {
                for (i in seq_along(grouped_plot)) {
                    grouped_plot[[i]] <- enhance_one(grouped_plot[[i]])
                }
                return(grouped_plot)
            }
            enhance_one(grouped_plot)
        },

        .paletteColors = function(n_colors, pal_name, theme) {
            n <- max(as.integer(n_colors), 1L)
            switch(
                pal_name,
                okabeito = {
                    base_cols <- c(
                        "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                        "#0072B2", "#D55E00", "#CC79A7", "#000000"
                    )
                    if (n <= length(base_cols)) {
                        base_cols[seq_len(n)]
                    } else {
                        grDevices::colorRampPalette(base_cols)(n)
                    }
                },
                viridis = viridisLite::viridis(n, begin = 0.12, end = 0.88),
                dark2 = {
                    base_cols <- c(
                        "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
                        "#66A61E", "#E6AB02", "#A6761D", "#666666"
                    )
                    if (n <= length(base_cols)) {
                        base_cols[seq_len(n)]
                    } else {
                        grDevices::colorRampPalette(base_cols)(n)
                    }
                },
                jmvcore::colorPalette(
                    n = n,
                    pal = if (!is.null(theme$palette)) theme$palette else "jmv"
                )
            )
        },

        .applyPalette = function(plot_obj, group_levels, theme, opts) {
            pal_name <- opts$colorpalette
            if (is.null(pal_name) || identical(pal_name, "jamovi")) {
                return(plot_obj)
            }
            n_col <- max(length(group_levels), 1L)
            cols <- private$.paletteColors(n_col, pal_name, theme)
            if (length(group_levels) == length(cols) && length(group_levels) > 0L) {
                names(cols) <- group_levels
            }
            add_scales <- function(p_one) {
                if (!inherits(p_one, "gg")) {
                    return(p_one)
                }
                suppressMessages(
                    p_one +
                        ggplot2::scale_color_manual(values = cols) +
                        ggplot2::scale_fill_manual(values = cols)
                )
            }
            if (inherits(plot_obj, "patchwork")) {
                for (i in seq_along(plot_obj)) {
                    plot_obj[[i]] <- add_scales(plot_obj[[i]])
                }
                return(plot_obj)
            }
            add_scales(plot_obj)
        },

        .sourcifyOption = function(option) {
            if (!option$name %in% names(formals(jjdotplotstats)))
                return("")
            value <- option$value
            if (is.list(value) || is.language(value))
                return("")
            if (is.character(value)) {
                if (length(value) != 1L || is.na(value) || identical(value, option$default))
                    return("")
                return(paste0(option$name, " = ", encodeString(value, quote = '"')))
            }
            if (!is.logical(value) && !is.numeric(value) && !is.null(value))
                return("")
            if (length(value) > 1L)
                return("")
            super$.sourcifyOption(option)
        },

        # run ----
        .run = function() {
            # Clear messages, notices, explanation, and cached validity at start of new run
            private$.messages <- NULL
            private$.clearNotices()
            if (!is.null(self$results$explanation))
                self$results$explanation$setContent("")
            private$.inputsValid <- FALSE
            private$.splitValid <- TRUE
            private$.subtitleFallback <- NULL
            private$.subtitleUncomputable <- NULL
            private$.subtitleCache <- NULL
            private$.captionCache <- NULL
            private$.captionUnavailable <- NULL
            private$.lastExactP <- NA_real_

            # Initial Message ----
            if (is.null(self$options$dep) || is.null(self$options$group)) {
                todo <- paste0(
                    "<div style='background-color: rgba(0, 123, 255, 0.08); color: inherit; border-left: 4px solid #007bff; padding: 12px 16px; margin: 8px 0;'>",
                    "<h3 style='margin-top: 0; color: inherit;'>", .("Welcome to ClinicoPath"), "</h3>",
                    "<p>", .("This analysis compares a continuous variable across groups and draws the comparison horizontally, as a box-violin figure with the individual observations shown."), "</p>",
                    "<p>", .("Looking for a Cleveland dot chart - one summary point per group, tested against a reference value? Use 'Dot Chart (Summary vs Reference Value)' instead. This analysis uses every observation and compares the groups with each other."), "</p>",
                    "<p><strong>", .("Select a continuous dependent variable and a categorical grouping variable to begin."), "</strong></p>",
                    "<p style='margin-bottom: 0;'>",
                    .fmt(
                        .("This analysis is built on the {pkg1} and {pkg2} packages; please cite jamovi and the packages listed below."),
                        pkg1 = "ggplot2", pkg2 = "ggstatsplot"),
                    "</p></div>")

                self$results$todo$setContent(todo)
                private$.publishRenderState()
                return()
            } else {
                if (nrow(self$data) == 0) {
                    self$results$todo$setContent("")
                    private$.publishRenderState()
                    private$.addNotice(.("Data contains no complete rows after filtering. Please check for missing values."), "ERROR")
                    return()
                }

                private$.inputsValid <- FALSE
                private$.splitValid <- TRUE
                private$.publishRenderState()
                tryCatch({
                    mydata <- private$.prepareData(emit_messages = TRUE)
                    options_data <- private$.prepareOptions(emit_messages = TRUE)

                    private$.inputsValid <- private$.validateInputs(mydata)
                    if (isTRUE(private$.inputsValid) && !is.null(self$options$grvar)) {
                        private$.splitValid <- private$.validateSplitInputs(mydata)
                    }
                    private$.publishRenderState()

                    if (isTRUE(private$.inputsValid)) {
                        n_obs <- nrow(mydata)
                        n_groups <- private$.nGroupLevels(mydata, options_data$group)
                        test_name <- switch(options_data$typestatistics,
                            "parametric" = "parametric (t-test/ANOVA)",
                            "nonparametric" = "nonparametric (Mann-Whitney/Kruskal-Wallis)",
                            "robust" = "robust (trimmed means)",
                            "bayes" = "Bayesian",
                            "selected"
                        )
                        private$.addNotice(.fmt(
                            .("Comparing {groups} groups with N = {n} observations using a {test} test."),
                            groups = n_groups, n = n_obs, test = test_name), "INFO")

                        private$.checkpoint()
                        need_omnibus <- isTRUE(options_data$resultssubtitle) ||
                            isTRUE(self$options$showexplanation)
                        computed_sub <- private$.subtitleExpr(
                            mydata, options_data$group, options_data$dep, options_data,
                            force_compute = need_omnibus)
                        private$.subtitleCache <- if (isTRUE(options_data$resultssubtitle)) computed_sub else NULL
                        private$.captionCache <- private$.captionExpr(
                            mydata, options_data$group, options_data$dep, options_data)
                        private$.publishRenderState()

                        if (!is.null(private$.subtitleUncomputable) &&
                            (isTRUE(self$options$resultssubtitle) || isTRUE(self$options$showexplanation))) {
                            private$.addNotice(.fmt(
                                .("Statistical test results could not be computed ({reason}), so no test subtitle is shown on the plot."),
                                reason = htmltools::htmlEscape(private$.subtitleUncomputable)), "WARNING")
                        } else if (!is.null(private$.subtitleFallback) && isTRUE(self$options$resultssubtitle)) {
                            private$.addNotice(.fmt(
                                .("The effect size measure you selected could not be applied ({reason}), so the plot shows the statistics package default instead."),
                                reason = htmltools::htmlEscape(private$.subtitleFallback)), "WARNING")
                        }

                        if (!isTRUE(self$options$resultssubtitle) &&
                            !isTRUE(self$options$showexplanation) &&
                            (!identical(self$options$effsizetype, "biased") ||
                             !isTRUE(all.equal(self$options$conflevel, 0.95))))
                            private$.addNotice(
                                .("Enable 'Statistical results in plot' or 'Assumptions and interpretation' to display the selected effect size or confidence level."),
                                "INFO")

                        if (!identical(self$options$effsizetype, "biased") &&
                            !identical(self$options$typestatistics, "parametric") &&
                            isTRUE(self$options$resultssubtitle))
                            private$.addNotice(.fmt(
                                .("'Effect Size Measure' applies to the parametric test only. The {test} test reports its own effect size instead, so your selection was not used."),
                                test = switch(self$options$typestatistics,
                                              nonparametric = "nonparametric",
                                              robust = "robust", bayes = "Bayesian",
                                              htmltools::htmlEscape(as.character(self$options$typestatistics)))), "INFO")

                        if (!is.null(self$options$grvar) &&
                            identical(self$options$typestatistics, "parametric") &&
                            isTRUE(self$options$resultssubtitle))
                            private$.addNotice(.("The 'Effect Size Measure' setting does not reach the Split By panels: the statistics package computes one subtitle per panel internally and always uses its own default there. The single unsplit figure below honours your selection."), "INFO")

                        if (n_groups > 2 && is.null(private$.subtitleUncomputable) &&
                            identical(self$options$typestatistics, "parametric") &&
                            !identical(self$options$pairwisedisplay, "none") &&
                            identical(self$options$padjustmethod, "holm"))
                            private$.addNotice(.("For three or more groups, parametric pairwise brackets use the Games-Howell single-step studentized-range procedure for family-wise error control (p_GH-adj); a second adjustment pass is not applied on top of it."), "INFO")

                        if (n_groups > 2 && is.null(private$.subtitleUncomputable) &&
                            identical(self$options$typestatistics, "parametric") &&
                            !identical(self$options$pairwisedisplay, "none") &&
                            self$options$padjustmethod %in% c("fdr", "bonferroni"))
                            private$.addNotice(.fmt(
                                .("For three or more groups with {adjustment} selected, parametric pairwise brackets use pairwise Welch unequal-variance t-tests with {adjustment} adjustment."),
                                adjustment = if (identical(self$options$padjustmethod, "fdr")) .("Benjamini-Hochberg") else .("Bonferroni")), "INFO")

                        if (n_groups > 2 && is.null(private$.subtitleUncomputable) &&
                            identical(self$options$typestatistics, "bayes") &&
                            identical(self$options$pairwisedisplay, "significant"))
                            private$.addNotice(.("Bayesian pairwise comparisons report natural-log JZS Bayes factors rather than frequentist p values, so all group pairs are shown on the plot."), "INFO")

                        if (n_groups > 8L && is.null(private$.subtitleUncomputable) &&
                            !identical(self$options$pairwisedisplay, "none")) {
                            if (identical(self$options$typestatistics, "bayes")) {
                                private$.addNotice(.fmt(
                                    .("Displaying all {pairs} Bayesian pairwise brackets across {groups} groups can crowd the figure; consider setting Pairwise Comparisons to 'None'."),
                                    pairs = choose(n_groups, 2L), groups = n_groups), "WARNING")
                            } else if (identical(self$options$pairwisedisplay, "all")) {
                                private$.addNotice(.fmt(
                                    .("Displaying all {pairs} pairwise brackets across {groups} groups can crowd the figure; consider 'Significant pairs only' or 'None'."),
                                    pairs = choose(n_groups, 2L), groups = n_groups), "WARNING")
                            }
                        }

                        if (n_groups > 2 && is.null(private$.subtitleUncomputable) &&
                            !identical(self$options$typestatistics, "bayes") &&
                            !identical(self$options$pairwisedisplay, "none") &&
                            identical(self$options$padjustmethod, "none"))
                            private$.addNotice(.fmt(
                                .("Pairwise comparisons are shown without any adjustment for multiple testing. With {groups} groups that is {pairs} comparisons, so even if no groups truly differ there is about a {risk} percent chance that at least one of them reaches significance. Use Holm unless you have a specific reason not to."),
                                groups = n_groups, pairs = choose(n_groups, 2),
                                risk = round(100 * (1 - 0.95 ^ choose(n_groups, 2)))), "STRONG_WARNING")

                        private$.renderExplanation(mydata, options_data, n_groups, computed_sub)

                        if (isTRUE(self$options$bfmessage) && !is.null(private$.captionUnavailable)) {
                            split_has_2grp <- FALSE
                            if (!is.null(self$options$grvar) && isTRUE(private$.splitValid) &&
                                isTRUE(self$options$resultssubtitle) &&
                                identical(self$options$typestatistics, "parametric") &&
                                n_groups != 2L) {
                                sp_chk <- private$.prepareSplitData(mydata)
                                if (nrow(sp_chk) > 0L) {
                                    st_f <- droplevels(private$.cleanFactor(sp_chk[[self$options$grvar]]))
                                    k_sp <- vapply(levels(st_f), function(lv) {
                                        private$.nGroupLevels(sp_chk[st_f == lv, , drop = FALSE], options_data$group)
                                    }, integer(1))
                                    split_has_2grp <- any(!is.na(k_sp) & k_sp == 2L)
                                }
                            }
                            if (split_has_2grp) {
                                private$.addNotice(.fmt(
                                    .("The Bayes factor caption is omitted on the unsplit figure ({reason}), but is shown on two-group Split By panels."),
                                    reason = htmltools::htmlEscape(private$.captionUnavailable)), "INFO")
                            } else {
                                private$.addNotice(.fmt(
                                    .("No Bayes factor caption is shown because {reason}."),
                                    reason = htmltools::htmlEscape(private$.captionUnavailable)), "INFO")
                            }
                        }
                    }
                }, error = function(e) {
                    private$.addNotice(.fmt(
                        .("Data processing failed: {reason}. Please check your variable selections and try again."),
                        reason = htmltools::htmlEscape(e$message)), "ERROR")
                    return()
                })
            }
        }


        ,
        .plot = function(image, ggtheme, theme, ...) {
            rs <- private$.renderState(image)
            if (!isTRUE(rs$valid))
                return()

            mydata <- private$.prepareData(emit_messages = FALSE)
            options_data <- private$.prepareOptions(emit_messages = FALSE)

            private$.checkpoint()

            uncomputable <- isTRUE(rs$uncomputable)
            sub_expr <- if (uncomputable) NULL else rs$subtitle
            cap_expr <- if (uncomputable) NULL else rs$caption
            n_lev <- private$.nGroupLevels(mydata, options_data$group)

            pw_disp <- if (uncomputable) "none" else options_data$pairwise.display
            p_adj_arg <- options_data$p.adjust.method
            if (!identical(pw_disp, "none") &&
                identical(options_data$typestatistics, "parametric") &&
                !is.na(n_lev) && n_lev >= 3L) {
                p_adj_arg <- "none"
                if (!identical(options_data$p.adjust.method, "holm")) {
                    pw_disp <- "all"
                }
            }

            use_alias <- !identical(make.names(options_data$dep), options_data$dep) ||
                !identical(make.names(options_data$group), options_data$group)
            if (use_alias) {
                mydata$.jj_dep <- as.numeric(mydata[[options_data$dep]])
                mydata$.jj_group <- droplevels(private$.cleanFactor(mydata[[options_data$group]]))
            }
            x_col <- if (use_alias) ".jj_group" else options_data$group
            y_col <- if (use_alias) ".jj_dep" else options_data$dep
            bf_effective <- isTRUE(options_data$bfmessage) && isTRUE(options_data$resultssubtitle)

            plot <- tryCatch({
                p <- withr::with_seed(private$.seed(),
                     private$.withBaseFormulaChar(ggstatsplot::ggbetweenstats(
                    data = mydata,
                    x = !!rlang::sym(x_col),
                    y = !!rlang::sym(y_col),
                    title = options_data$mytitle,
                    xlab = options_data$xlab,
                    ylab = options_data$ylab,
                    type = options_data$typestatistics,
                    conf.level = options_data$conflevel,
                    digits = options_data$digits,
                    bf.message = if (uncomputable || !bf_effective || !is.null(cap_expr)) FALSE else TRUE,
                    centrality.plotting = options_data$centrality.plotting,
                    centrality.type = options_data$centrality.type,
                    results.subtitle = if (uncomputable || !is.null(sub_expr)) FALSE else options_data$resultssubtitle,
                    pairwise.display = pw_disp,
                    p.adjust.method = p_adj_arg,
                    ggplot.component = options_data$ggplot.component,
                    ggtheme = if (options_data$originaltheme) ggstatsplot::theme_ggstatsplot() else ggtheme
                )))
                p <- private$.fixParametricPairwise(
                    p, mydata, x_col, y_col, options_data
                )
                p <- private$.fixBayesPairwise(
                    p, mydata, x_col, options_data
                )
                grp_lv <- levels(droplevels(private$.cleanFactor(mydata[[options_data$group]])))
                p <- private$.applyPalette(p, grp_lv, theme, options_data)
                if (!is.null(sub_expr)) p <- p + ggplot2::labs(subtitle = sub_expr)
                if (!is.null(cap_expr)) p <- p + ggplot2::labs(caption = cap_expr)
                p
            }, error = function(e) e)

            if (inherits(plot, "condition"))
                return(private$.plotFailure(.fmt(
                    .("The plot could not be drawn: {reason}. Check the dependent variable for constant values, extreme outliers or too few observations per group, or try a different statistical test."),
                    reason = conditionMessage(plot))))

            .k <- if (is.na(n_lev)) 0L else n_lev
            if ((statsSeedMatters(options_data$typestatistics, "between", .k) && !uncomputable) ||
                (!is.null(cap_expr) && !uncomputable))
                plot <- addSeedCaption(plot, self, private$.seed())

            if (is.null(plot)) return()

            tryCatch(
                withr::with_seed(private$.seed(), print(plot)),
                error = function(e) {
                    private$.plotFailure(.fmt(
                        .("The plot could not be drawn: {reason}. Check the dependent variable for constant values, extreme outliers or too few observations per group, or try a different statistical test."),
                        reason = conditionMessage(e)))
                }
            )
            TRUE
        }


        ,

        .plot2 = function(image, ggtheme, theme, ...) {
            rs <- private$.renderState(image)
            if (!isTRUE(rs$valid) || isFALSE(rs$splitValid) || is.null(self$options$grvar))
                return()

            base_data <- private$.prepareData(emit_messages = FALSE)
            mydata <- private$.prepareSplitData(base_data)
            if (nrow(mydata) == 0L)
                return()
            options_data <- private$.prepareOptions(emit_messages = FALSE)

            grvar <- self$options$grvar
            private$.checkpoint()

            n_lev <- private$.nGroupLevels(mydata, options_data$group)
            pw_disp <- options_data$pairwise.display
            p_adj_arg <- options_data$p.adjust.method
            if (!identical(pw_disp, "none") &&
                identical(options_data$typestatistics, "parametric") &&
                !is.na(n_lev) && n_lev >= 3L) {
                p_adj_arg <- "none"
                if (!identical(options_data$p.adjust.method, "holm")) {
                    pw_disp <- "all"
                }
            }

            use_alias <- !identical(make.names(options_data$dep), options_data$dep) ||
                !identical(make.names(options_data$group), options_data$group)
            if (use_alias) {
                mydata$.jj_dep <- as.numeric(mydata[[options_data$dep]])
                mydata$.jj_group <- droplevels(private$.cleanFactor(mydata[[options_data$group]]))
            }
            x_col <- if (use_alias) ".jj_group" else options_data$group
            y_col <- if (use_alias) ".jj_dep" else options_data$dep
            bf_effective <- isTRUE(options_data$bfmessage) && isTRUE(options_data$resultssubtitle)

            run_grouped <- function(df_in, pw_arg) {
                withr::with_seed(private$.seed(),
                    private$.withBaseFormulaChar(ggstatsplot::grouped_ggbetweenstats(
                        data = df_in,
                        x = !!rlang::sym(x_col),
                        y = !!rlang::sym(y_col),
                        grouping.var = !!rlang::sym(grvar),
                        type = options_data$typestatistics,
                        conf.level = options_data$conflevel,
                        digits = options_data$digits,
                        bf.message = bf_effective,
                        results.subtitle = options_data$resultssubtitle,
                        pairwise.display = pw_arg,
                        p.adjust.method = p_adj_arg,
                        centrality.plotting = options_data$centrality.plotting,
                        centrality.type = options_data$centrality.type,
                        ggplot.component = options_data$ggplot.component,
                        ggtheme = if (options_data$originaltheme) ggstatsplot::theme_ggstatsplot() else ggtheme,
                        xlab = options_data$xlab,
                        ylab = options_data$ylab,
                        annotation.args = list(title = options_data$mytitle)
                    ))
                )
            }

            plot2 <- tryCatch(run_grouped(mydata, pw_disp), error = function(e) e)

            # Fallback if a degenerate stratum crashes pairwise comparisons (e.g. WRS2::lincon on singleton)
            if (inherits(plot2, "condition") && !identical(pw_disp, "none")) {
                plot2 <- tryCatch(run_grouped(mydata, "none"), error = function(e) e)
            }

            if (inherits(plot2, "condition"))
                return(private$.plotFailure(.fmt(
                    .("The split figure could not be drawn: {reason}. Check that every level of the Split By variable has enough data in at least two groups."),
                    reason = conditionMessage(plot2))))
            if (is.null(plot2)) return()

            plot2 <- private$.enhanceSplitPanels(
                plot2, x_col, y_col, options_data, private$.seed()
            )
            grp_lv_all <- levels(droplevels(private$.cleanFactor(base_data[[options_data$group]])))
            plot2 <- private$.applyPalette(plot2, grp_lv_all, theme, options_data)

            st_fac <- droplevels(private$.cleanFactor(mydata[[grvar]]))
            k_vec <- vapply(levels(st_fac), function(lv) {
                private$.nGroupLevels(mydata[st_fac == lv, , drop = FALSE], options_data$group)
            }, integer(1))
            k_vec <- unique(c(if (is.na(n_lev)) integer(0) else n_lev, k_vec[!is.na(k_vec)]))
            seed_matters_split <- any(vapply(k_vec, function(ki) {
                statsSeedMatters(options_data$typestatistics, "between", ki) ||
                    captionSeedMatters(options_data$typestatistics, "between", ki, bf_effective)
            }, logical(1)))
            if (seed_matters_split)
                plot2 <- addSeedCaption(plot2, self, private$.seed())

            tryCatch(
                withr::with_seed(private$.seed(), print(plot2)),
                error = function(e) {
                    private$.plotFailure(.fmt(
                        .("The split figure could not be drawn: {reason}. Check that every level of the Split By variable has enough data in at least two groups."),
                        reason = conditionMessage(e)))
                }
            )
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

            if (!is.character(dep) || length(dep) != 1L || is.na(dep) || !nzchar(dep) ||
                !is.character(group) || length(group) != 1L || is.na(group) || !nzchar(group))
                return("")

            # Build the argument list in option-declaration order, filtering out
            # undeclared .omv options, dropping list/language values, and escaping
            # all character scalars via encodeString(..., quote = '"').
            valid_names <- names(formals(jjdotplotstats))
            args <- character(0)
            for (option in private$.options$options) {
                if (option$name == "data" || !option$name %in% valid_names)
                    next
                if (inherits(option, "OptionVariable")) {
                    val <- option$value
                    if (is.character(val) && length(val) == 1L && !is.na(val) && nzchar(val))
                        args <- c(args, paste0(option$name, " = ",
                                               encodeString(val, quote = '"')))
                } else if (inherits(option, "OptionVariables")) {
                    val <- option$value
                    if (is.character(val) && length(val) >= 1L && !anyNA(val)) {
                        enc <- encodeString(val, quote = '"')
                        rhs <- if (length(enc) == 1L) enc else paste0("c(", paste(enc, collapse = ", "), ")")
                        args <- c(args, paste0(option$name, " = ", rhs))
                    }
                } else {
                    as <- private$.sourcifyOption(option)
                    if (!identical(as, ""))
                        args <- c(args, as)
                }
            }

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, "::jjdotplotstats(\n    data = data,\n    ",
                   paste(args, collapse = ",\n    "), ")")
        }
    ) # End of public list
)
