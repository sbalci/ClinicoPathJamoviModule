# File-level Stat ggproto for Welch's one-way ANOVA on companion ggpubr panels
# when K >= 3 groups and varequal = FALSE. ggpubr::stat_compare_means(method = "anova")
# hardcodes equal-variance Fisher aov() and ignores var.equal = FALSE; computing
# stats::oneway.test(..., var.equal = FALSE) per panel keeps both ungrouped and
# faceted companion plots statistically consistent with the main plot.
.jjbetweenStatWelchOneway <- ggplot2::ggproto(
    "StatWelchOneway",
    ggplot2::Stat,
    required_aes = c("x", "y"),
    default_aes = ggplot2::aes(label = ggplot2::after_stat(label)),
    compute_panel = function(data, scales) {
        df <- data[is.finite(data$y) & !is.na(data$x), , drop = FALSE]
        if (nrow(df) < 3L || length(unique(df$x)) < 2L)
            return(data.frame())
        grp_tab <- table(df$x)
        if (any(grp_tab < 2L))
            return(data.frame())
        wt <- tryCatch(
            withBaseFormulaChar(stats::oneway.test(y ~ factor(x), data = df, var.equal = FALSE)),
            error = function(e) NULL
        )
        if (is.null(wt) || is.na(wt$p.value))
            return(data.frame())
        yr <- range(df$y, na.rm = TRUE)
        span <- diff(yr)
        if (!is.finite(span) || span <= 0)
            span <- 1
        pval <- unname(wt$p.value)
        p_txt <- if (pval < 2.2e-16) "< 2.2e-16" else sub("^<\\s*", "< ", base::format.pval(pval, digits = 2, eps = 1e-4))
        lbl_txt <- if (grepl("^<", p_txt)) {
            paste0("Welch ANOVA, p ", p_txt)
        } else {
            paste0("Welch ANOVA, p = ", p_txt)
        }
        data.frame(
            x = min(as.numeric(df$x), na.rm = TRUE),
            y = yr[2] + 0.06 * span,
            p = pval,
            method = "Welch ANOVA",
            label = lbl_txt,
            stringsAsFactors = FALSE
        )
    }
)

#' @title Box-Violin Plots to Compare Between Groups
#' @importFrom R6 R6Class
#' @importFrom jmvcore .
#'
#' @return An \code{R6} class generator object for the \code{jjbetweenstatsClass} backend; used internally by the jamovi analysis wrapper and not called directly.
jjbetweenstatsClass <- if (requireNamespace("jmvcore")) R6::R6Class(
    "jjbetweenstatsClass",
    inherit = jjbetweenstatsBase,
    private = list(
        # Set by .subtitleExpr when the statsExpressions takeover failed while
        # ggstatsplot could still render a fallback default subtitle; read only in .run().
        .subtitleFallback = NULL,

        # TRUE when the Bayesian test cannot be computed for the selected data;
        # read only in .run().
        .bayesNoStatistic = FALSE,

        # Dependent variables for which neither statsExpressions nor ggstatsplot
        # can compute any subtitle (e.g., singleton group n_g < 2 or zero variance);
        # read only in .run().
        .noStatisticVars = character(),

        # Cache for processed data, options, and computed statsExpressions tibbles
        .processedData = NULL,
        .processedOptions = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .statsCache = NULL,
        .accumulated_info = NULL,
        .accumulated_warnings = NULL,
        .accumulated_strong = NULL,
        .strongNoticeMessages = NULL,
        .diagnosticsHtml = NULL,
        .assumptionWarnings = NULL,

        # Safe integer seed accessor guarding against NA, NULL, non-numeric, non-finite, or < 1 inputs
        .safeSeed = function() {
            raw_seed <- self$options$seed
            if (is.null(raw_seed) || !is.numeric(raw_seed) || length(raw_seed) != 1L ||
                is.na(raw_seed) || !is.finite(raw_seed)) {
                return(20250101L)
            }
            s <- suppressWarnings(as.integer(raw_seed))
            if (is.na(s) || s < 1L) 20250101L else s
        },

        # Safe plot dimension accessor clamping tampered .omv inputs to declared pixel bounds
        .safeDim = function(x, default, min_val, max_val) {
            if (is.null(x) || !is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
                return(as.integer(default))
            }
            val <- suppressWarnings(as.integer(round(x)))
            if (is.na(val)) return(as.integer(default))
            max(as.integer(min_val), min(as.integer(max_val), val))
        },

        # Deduplicated dependent-variable vector so repeated names in R calls never
        # inflate multiple-endpoint FWER thresholds or duplicate panels/summaries.
        .depVars = function() {
            deps <- self$options$dep
            if (is.null(deps) || length(deps) == 0L) return(character())
            unique(as.character(deps))
        },

        # Serialize unnamed atomic R values into valid parseable literals for asSource()
        .R_literal = function(x) {
            if (is.null(x) || !is.atomic(x)) return("NULL")
            attributes(x) <- NULL
            paste(
                deparse(x, backtick = TRUE,
                        control = c("keepNA", "keepInteger")),
                collapse = " "
            )
        },

        # Format numeric values matching statsExpressions fixed/scientific style
        .fmtStatVal = function(x, digits = 2L) {
            digits <- as.integer(digits)
            if (!is.finite(x)) return(NA_character_)
            if (x != 0 && abs(x) < 10^(-digits)) {
                sprintf(paste0("%.", digits, "e"), x)
            } else {
                sprintf(paste0("%.", digits, "f"), x)
            }
        },

        # Format assumption-check p-values without ever emitting 'p = 0.000'
        .fmtPClause = function(p, html = TRUE) {
            if (!is.finite(p)) return("p = NA")
            if (p < 0.001) {
                if (isTRUE(html)) "p &lt; 0.001" else "p < 0.001"
            } else {
                sprintf("p = %.3f", p)
            }
        },

        # Clean categorical columns: preserve haven_labelled value labels in declared
        # order and convert explicit <NA> factor levels (from addNA()) back to true NA
        # so missing group memberships are never analyzed as an extra arm.
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

        # Per-variable complete-case subsetting so missing values in dep[2] or grvar
        # never delete valid observations of dep[1] on unstratified outputs.
        .subsetVar = function(mydata, dep_var, include_grvar = FALSE) {
            grp_col <- self$options$group
            keep <- !is.na(mydata[[grp_col]]) & is.finite(mydata[[dep_var]])
            if (isTRUE(include_grvar) && !is.null(self$options$grvar) &&
                self$options$grvar %in% names(mydata)) {
                keep <- keep & !is.na(mydata[[self$options$grvar]])
            }
            sub_df <- mydata[keep, , drop = FALSE]
            sub_df[[grp_col]] <- droplevels(private$.cleanFactor(sub_df[[grp_col]]))
            if (isTRUE(include_grvar) && !is.null(self$options$grvar) &&
                self$options$grvar %in% names(sub_df)) {
                sub_df[[self$options$grvar]] <- droplevels(private$.cleanFactor(sub_df[[self$options$grvar]]))
            }
            sub_df
        },

        # Stratified subsetting helper for Split-By plots (.plot2 and .plotGGPubr2):
        # drops strata of grvar that retain fewer than 2 observed groups and rejects
        # uniformly when no valid stratum remains.
        .subsetStratifiedVar = function(mydata, dep_var, reject_if_empty = TRUE) {
            grp_col <- self$options$group
            grvar <- self$options$grvar
            sub_df <- private$.subsetVar(mydata, dep_var, include_grvar = TRUE)
            dropped_strata <- character()

            if (!is.null(grvar) && grvar %in% names(sub_df) && nrow(sub_df) > 0L) {
                st_fac <- sub_df[[grvar]]
                valid_strata <- character()
                for (st_lvl in levels(st_fac)) {
                    st_rows <- sub_df[st_fac == st_lvl, , drop = FALSE]
                    if (nrow(st_rows) > 0L) {
                        if (private$.nGroupLevels(st_rows, grp_col) >= 2L) {
                            valid_strata <- c(valid_strata, st_lvl)
                        } else {
                            dropped_strata <- c(dropped_strata, st_lvl)
                        }
                    }
                }
                sub_df <- sub_df[st_fac %in% valid_strata, , drop = FALSE]
                if (nrow(sub_df) > 0L) {
                    sub_df[[grp_col]] <- droplevels(private$.cleanFactor(sub_df[[grp_col]]))
                    sub_df[[grvar]] <- droplevels(private$.cleanFactor(sub_df[[grvar]]))
                }
            }

            if (isTRUE(reject_if_empty) &&
                (nrow(sub_df) == 0L || private$.nGroupLevels(sub_df, grp_col) < 2L)) {
                jmvcore::reject(
                    .("Insufficient complete observations across '{group}' and '{grvar}' to build the stratified plot."),
                    group = grp_col, grvar = grvar
                )
            }

            attr(sub_df, "dropped_strata") <- dropped_strata
            sub_df
        },

        # Helper to accumulate diagnostic messages by severity tier
        .appendMessage = function(message, level = "info", plain_notice = NULL) {
            if (identical(level, "strong")) {
                private$.accumulated_strong <- c(private$.accumulated_strong, message)
                if (!is.null(plain_notice) && nzchar(plain_notice)) {
                    private$.strongNoticeMessages <- c(private$.strongNoticeMessages, plain_notice)
                }
            } else if (identical(level, "warn")) {
                private$.accumulated_warnings <- c(private$.accumulated_warnings, message)
            } else {
                private$.accumulated_info <- c(private$.accumulated_info, message)
            }
        },

        # Memoized statsExpressions computation shared across .run() diagnostics,
        # copy-ready report generation, and .plot() subtitle rendering so bootstrap
        # and Bayesian MCMC estimators never run twice for the same inputs.
        .computeStatsResult = function(data, group_var, dep_var, opts, type_override = NULL) {
            stat_type <- if (!is.null(type_override)) type_override else opts$typestatistics
            n_lev <- private$.nGroupLevels(data, group_var)
            y_fin <- data[[dep_var]]
            y_fin <- y_fin[is.finite(y_fin)]
            if (is.na(n_lev) || n_lev < 2L || length(unique(y_fin)) < 2L) {
                return(list(ok = FALSE, res = NULL, expr = NULL, fallback = NULL))
            }

            seed_val <- private$.safeSeed()
            cache_key <- digest::digest(list(
                g = data[[group_var]],
                y = data[[dep_var]],
                type = stat_type,
                varequal = isTRUE(opts$varequal),
                effsizetype = opts$effsizetype,
                k = opts$k,
                conflevel = opts$conflevel,
                seed = seed_val
            ), algo = "md5")

            if (is.null(private$.statsCache)) {
                private$.statsCache <- list()
            }
            if (!is.null(private$.statsCache[[cache_key]])) {
                cached <- private$.statsCache[[cache_key]]
                if (!isTRUE(cached$ok) && is.null(type_override) && !identical(stat_type, "bayes")) {
                    private$.subtitleFallback <- cached$fallback
                }
                return(cached)
            }

            withr::local_preserve_seed()
            if (identical(stat_type, "bayes") || statsSeedMatters(stat_type, "between", n_lev)) {
                withr::local_seed(seed_val)
            }

            eff <- opts$effsizetype
            if (n_lev == 2L && identical(eff, "eta"))   eff <- "biased"
            if (n_lev == 2L && identical(eff, "omega")) eff <- "unbiased"

            fn <- if (n_lev == 2L) statsExpressions::two_sample_test
                  else             statsExpressions::oneway_anova

            res <- if (identical(stat_type, "bayes")) {
                tryCatch(
                    withBaseFormulaChar(rlang::inject(fn(
                        data       = data,
                        x          = !!rlang::sym(group_var),
                        y          = !!rlang::sym(dep_var),
                        type       = "bayes",
                        digits     = opts$k,
                        conf.level = opts$conflevel))),
                    error = function(e) e)
            } else {
                tryCatch(
                    withBaseFormulaChar(rlang::inject(fn(
                        data         = data,
                        x            = !!rlang::sym(group_var),
                        y            = !!rlang::sym(dep_var),
                        type         = stat_type,
                        var.equal    = isTRUE(opts$varequal),
                        effsize.type = eff,
                        digits       = opts$k,
                        conf.level   = opts$conflevel))),
                    error = function(e) e)
            }

            if (inherits(res, "condition")) {
                fb <- conditionMessage(res)
                if (is.null(type_override) && !identical(stat_type, "bayes")) {
                    private$.subtitleFallback <- fb
                }
                out_err <- list(ok = FALSE, res = NULL, expr = NULL, fallback = fb)
                private$.statsCache[[cache_key]] <- out_err
                return(out_err)
            }
            if (is.null(res$expression) || length(res$expression) == 0L) {
                fb <- .("the statistics engine returned no expression")
                if (is.null(type_override) && !identical(stat_type, "bayes")) {
                    private$.subtitleFallback <- fb
                }
                out_empty <- list(ok = FALSE, res = NULL, expr = NULL, fallback = fb)
                private$.statsCache[[cache_key]] <- out_empty
                return(out_empty)
            }

            bad_row <- if (identical(stat_type, "bayes")) {
                !("bf10" %in% names(res) && is.finite(res$bf10[1])) ||
                    ("estimate" %in% names(res) && !is.finite(res$estimate[1]))
            } else {
                !("statistic" %in% names(res) && is.finite(res$statistic[1])) ||
                    !("p.value" %in% names(res) && is.finite(res$p.value[1]))
            }
            if (isTRUE(bad_row)) {
                out_deg <- list(ok = FALSE, res = NULL, expr = NULL, fallback = NULL)
                private$.statsCache[[cache_key]] <- out_deg
                return(out_deg)
            }

            expr <- res$expression[[1]]

            # statsExpressions omits paired = FALSE when calling effectsize::cohens_d /
            # hedges_g, causing Student's t-test (var.equal = TRUE) to display an
            # unpooled effect size whose CI can contradict the Student p-value.
            # Recompute with pooled_sd = TRUE so both expression and cached tibble agree.
            if (n_lev == 2L && identical(stat_type, "parametric") && isTRUE(opts$varequal)) {
                pooled_es <- tryCatch({
                    fml <- jmvcore::asFormula(jmvcore::constructFormula(dep_var, group_var))
                    if (identical(eff, "unbiased")) {
                        withBaseFormulaChar(effectsize::hedges_g(
                            fml, data = data, pooled_sd = TRUE, ci = opts$conflevel
                        ))
                    } else {
                        withBaseFormulaChar(effectsize::cohens_d(
                            fml, data = data, pooled_sd = TRUE, ci = opts$conflevel
                        ))
                    }
                }, error = function(e) NULL)

                if (!is.null(pooled_es) && nrow(pooled_es) >= 1L &&
                    is.call(expr) && length(expr) >= 6L) {
                    est_col <- if ("Hedges_g" %in% names(pooled_es)) "Hedges_g" else "Cohens_d"
                    est_val <- as.numeric(pooled_es[[est_col]][1])
                    ci_lo   <- as.numeric(pooled_es$CI_low[1])
                    ci_hi   <- as.numeric(pooled_es$CI_high[1])
                    if (all(is.finite(c(est_val, ci_lo, ci_hi)))) {
                        expr[[4]][[3]]       <- private$.fmtStatVal(est_val, opts$k)
                        expr[[5]][[3]][[3]]  <- private$.fmtStatVal(ci_lo, opts$k)
                        expr[[6]][[2]]       <- private$.fmtStatVal(ci_hi, opts$k)
                        res$estimate[1]      <- est_val
                        res$conf.low[1]      <- ci_lo
                        res$conf.high[1]     <- ci_hi
                        res$effectsize[1]    <- if (identical(eff, "unbiased")) "Hedges' g" else "Cohen's d"
                    }
                }
            }

            out_ok <- list(ok = TRUE, res = res, expr = expr, fallback = NULL)
            private$.statsCache[[cache_key]] <- out_ok
            out_ok
        },

        # Option-dependent subtitle diagnostics across ALL selected dependent variables.
        .subtitleDiagnosticsHtml = function(mydata) {
            private$.subtitleFallback <- NULL
            private$.bayesNoStatistic <- FALSE
            private$.noStatisticVars <- character()
            out <- character()

            deps <- private$.depVars()
            grp_col <- self$options$group
            opts <- private$.prepareOptions()

            if (isTRUE(self$options$resultssubtitle) && length(deps) > 0L) {
                if (identical(self$options$typestatistics, "bayes")) {
                    bayes_failed <- character()
                    for (v in deps) {
                        sub_v <- private$.subsetVar(mydata, v, include_grvar = FALSE)
                        if (isTRUE(private$.bayesProbeFails(sub_v, grp_col, v))) {
                            bayes_failed <- c(bayes_failed, v)
                        }
                    }
                    private$.noStatisticVars <- bayes_failed
                    private$.bayesNoStatistic <- length(bayes_failed) == length(deps) && length(deps) > 0L
                    if (length(bayes_failed) > 0L) {
                        if (length(deps) == 1L) {
                            out <- c(out, paste0(
                                "<br>",
                                .("Note: the Bayesian test could not be computed for this data, so the plot carries no subtitle and no test statistic, Bayes factor or effect size is shown. Choose a different statistical approach (parametric, nonparametric or robust) to obtain a result."),
                                "<br>"
                            ))
                        } else {
                            out <- c(out, paste0(
                                "<br>",
                                .fmt(
                                    .("Note: the Bayesian test could not be computed for {vars}, so no test statistic, Bayes factor or effect size is shown on the affected plot(s). Choose a different statistical approach (parametric, nonparametric or robust) to obtain a result."),
                                    vars = htmltools::htmlEscape(paste(bayes_failed, collapse = ", "))
                                ),
                                "<br>"
                            ))
                        }
                    }
                } else {
                    empty_sub_vars <- character()
                    fallback_msgs <- character()
                    for (v in deps) {
                        sub_v <- private$.subsetVar(mydata, v, include_grvar = FALSE)
                        grp_counts <- table(droplevels(private$.cleanFactor(sub_v[[grp_col]])))
                        uncomputable <- length(grp_counts) < 2L ||
                            any(grp_counts < 2L) ||
                            length(unique(sub_v[[v]])) < 2L
                        private$.subtitleFallback <- NULL
                        sub_expr <- private$.subtitleExpr(sub_v, grp_col, v, opts)
                        if (is.null(sub_expr)) {
                            if (uncomputable) {
                                empty_sub_vars <- c(empty_sub_vars, v)
                            } else if (!is.null(private$.subtitleFallback)) {
                                fallback_msgs <- c(fallback_msgs, private$.subtitleFallback)
                            }
                        }
                    }
                    private$.noStatisticVars <- empty_sub_vars
                    private$.subtitleFallback <- if (length(fallback_msgs) > 0L) fallback_msgs[1] else NULL

                    if (length(empty_sub_vars) > 0L) {
                        out <- c(out, paste0(
                            "<br>",
                            .fmt(
                                .("Note: a statistical subtitle could not be computed for {vars} because at least one group has fewer than 2 observations or the variable is constant; the affected plot carries no test statistic."),
                                vars = htmltools::htmlEscape(paste(empty_sub_vars, collapse = ", "))
                            ),
                            "<br>"
                        ))
                    }
                }
            }

            if (!is.null(private$.subtitleFallback) && isTRUE(self$options$resultssubtitle)) {
                out <- c(out, paste0(
                    "<br>",
                    .fmt(
                        .("Note: the plot subtitle fell back to the package default ({reason}), so 'Equal variances', 'Parametric effect size' and 'Decimal places' did not affect the reported statistic. Welch's variant and the default effect size were used."),
                        reason = htmltools::htmlEscape(private$.subtitleFallback)
                    ),
                    "<br>"
                ))
            }

            if (isTRUE(self$options$varequal) &&
                isTRUE(self$options$pairwisecomparisons) &&
                identical(self$options$typestatistics, "parametric") &&
                !identical(self$options$pairwisedisplay, "none")) {
                out <- c(out, paste0(
                    "<br>",
                    .("Note: 'Equal variances' applies to the overall test only. Parametric pairwise brackets use the Games-Howell comparison (or Welch pairwise t-tests when adjustment is 'none'), which does not assume equal variances, so brackets may differ from a pooled-variance post-hoc test."),
                    "<br>"
                ))
            }

            if (isTRUE(self$options$pairwisecomparisons) &&
                identical(self$options$typestatistics, "parametric") &&
                !identical(self$options$padjustmethod, "none")) {
                any_3plus <- any(vapply(deps, function(v) {
                    private$.nGroupLevels(private$.subsetVar(mydata, v, FALSE), grp_col) >= 3L
                }, logical(1)))
                if (isTRUE(any_3plus)) {
                    out <- c(out, paste0(
                        "<br>",
                        .("Note: for three or more groups, parametric pairwise brackets report single-step Games-Howell studentized-range family-wise error rate adjusted p-values (p_GH-adj) rather than applying a second adjustment pass."),
                        "<br>"
                    ))
                }
            }

            if (identical(self$options$typestatistics, "nonparametric") &&
                isTRUE(self$options$resultssubtitle)) {
                out <- c(out, paste0(
                    "<br>",
                    .("Note: the nonparametric p-value is computed from the normal approximation with a continuity correction, not the exact test. For small samples without ties, stats::wilcox.test() in R defaults to the exact test and may report a slightly different p-value."),
                    "<br>"
                ))
            }

            if (isTRUE(private$.bfCaptionUnavailable(mydata))) {
                out <- c(out, paste0(
                    "<br>",
                    .("Note: 'Bayes factor message' has no effect here. A Bayes factor caption is available only for a parametric comparison of exactly two groups; for other statistical approaches, or three or more groups, no Bayes factor is reported."),
                    "<br>"
                ))
            }

            # Disclose Split-By differences whenever Equal variances or a non-default
            # effect size is chosen, OR when parametric subtitles are active with the
            # biased default (main plot shows Cohen's d / partial eta-squared whereas
            # grouped_ggbetweenstats computes Hedges' g / partial omega-squared).
            split_disclose <- !is.null(self$options$grvar) && (
                isTRUE(self$options$varequal) ||
                !identical(self$options$effsizetype, "biased") ||
                (isTRUE(self$options$resultssubtitle) &&
                 identical(self$options$typestatistics, "parametric") &&
                 !identical(self$options$effsizetype, "unbiased"))
            )
            if (isTRUE(split_disclose)) {
                out <- c(out, paste0(
                    "<br>",
                    .("Note: Grouped ('Split By') plots are computed per panel by ggstatsplot, which always uses the Welch variant and unbiased effect sizes (Hedges' g or partial omega-squared). 'Equal variances' and 'Parametric effect size' apply to the main figure only."),
                    "<br>"
                ))
            }

            # Disclose any Split-By strata dropped from stratified plots due to < 2 groups with data
            if (!is.null(self$options$grvar) && self$options$grvar %in% names(mydata)) {
                dropped_all <- unique(unlist(lapply(deps, function(v) {
                    st_df <- private$.subsetStratifiedVar(mydata, v, reject_if_empty = FALSE)
                    attr(st_df, "dropped_strata")
                })))
                if (length(dropped_all) > 0L) {
                    out <- c(out, paste0(
                        "<br>",
                        .fmt(
                            .("Note: Split-By subgroup(s) '{strata}' had fewer than 2 groups with complete observations and were excluded from stratified plots."),
                            strata = htmltools::htmlEscape(paste(dropped_all, collapse = "', '"))
                        ),
                        "<br>"
                    ))
                }
            }

            paste(out, collapse = "")
        },

        # Compose severity-tiered theme-safe HTML blocks for self$results$diagnostics
        # without using base::sub() backreference replacement on user column names.
        .storeDiagnostics = function() {
            blocks <- character()

            if (!is.null(private$.accumulated_strong) && length(private$.accumulated_strong) > 0L) {
                blocks <- c(blocks, paste0(
                    "<div style='padding: 12px 15px; background-color: rgba(216, 33, 50, 0.18); color: inherit; ",
                    "border-left: 4px solid #dc3545; margin: 10px 0;'>",
                    "<strong style='color: inherit;'>", .("Critical Statistical Validity Warnings"), "</strong>",
                    paste(private$.accumulated_strong, collapse = ""),
                    "</div>"
                ))
            }

            if (!is.null(private$.accumulated_warnings) && length(private$.accumulated_warnings) > 0L) {
                blocks <- c(blocks, paste0(
                    "<div style='padding: 12px 15px; background-color: rgba(255, 202, 33, 0.23); color: inherit; ",
                    "border-left: 4px solid #ffc107; margin: 10px 0;'>",
                    "<strong style='color: inherit;'>", .("Assumption & Data Quality Warnings"), "</strong>",
                    paste(private$.accumulated_warnings, collapse = ""),
                    "</div>"
                ))
            }

            if (!is.null(private$.accumulated_info) && length(private$.accumulated_info) > 0L) {
                blocks <- c(blocks, paste0(
                    "<div style='padding: 12px 15px; background-color: rgba(138, 155, 172, 0.06); color: inherit; ",
                    "border-left: 4px solid #6c757d; margin: 10px 0;'>",
                    "<strong style='color: inherit;'>", .("Data Diagnostics & Notes"), "</strong>",
                    paste(private$.accumulated_info, collapse = ""),
                    "</div>"
                ))
            }

            private$.accumulated_strong <- NULL
            private$.accumulated_warnings <- NULL
            private$.accumulated_info <- NULL
            private$.diagnosticsHtml <- paste(blocks, collapse = "")
        },

        # Append option-dependent subtitle notes safely using substr() slicing rather than
        # base::sub() replacement so backslashes in variable names are never stripped.
        .mergeDiagnosticsHtml = function(diag_html, sub_html) {
            if (!nzchar(sub_html)) return(diag_html)
            notes_title <- paste0("<strong style='color: inherit;'>", .("Data Diagnostics & Notes"), "</strong>")
            if (nzchar(diag_html) && grepl(notes_title, diag_html, fixed = TRUE) && grepl("</div>$", diag_html)) {
                paste0(substr(diag_html, 1L, nchar(diag_html) - 6L), sub_html, "</div>")
            } else {
                note_box <- paste0(
                    "<div style='padding: 12px 15px; background-color: rgba(138, 155, 172, 0.06); color: inherit; ",
                    "border-left: 4px solid #6c757d; margin: 10px 0;'>",
                    notes_title, sub_html, "</div>"
                )
                paste0(diag_html, note_box)
            }
        },

        # Idempotent jmvcore::Notice banner for severe statistical validity hazards
        .syncValidityNotice = function(messages) {
            if (!inherits(self$results, "Group") || length(self$results$items) == 0L)
                return(invisible(NULL))

            item_names <- vapply(self$results$items, function(it) it$name, character(1))
            idx <- match("validityWarning", item_names)
            msgs <- unique(Filter(nzchar, messages))

            if (length(msgs) > 0L) {
                msg_plain <- paste(msgs, collapse = " ")
                if (is.na(idx)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = "validityWarning",
                        type = jmvcore::NoticeType$STRONG_WARNING
                    )
                    notice$setContent(msg_plain)
                    self$results$insert(1, notice)
                } else {
                    self$results$items[[idx]]$setContent(msg_plain)
                    self$results$items[[idx]]$setVisible(TRUE)
                }
            } else if (!is.na(idx)) {
                self$results$items[[idx]]$setVisible(FALSE)
            }
            invisible(NULL)
        },

        # init ----
        .init = function() {
            deplen <- length(private$.depVars())

            plotwidth <- private$.safeDim(self$options$plotwidth, default = 650L, min_val = 300L, max_val = 1200L)
            plotheight <- private$.safeDim(self$options$plotheight, default = 450L, min_val = 300L, max_val = 800L)

            if (deplen > 1) {
                total_height <- min(4096L, as.integer(round(deplen * plotheight * 1.15)))
            } else {
                total_height <- plotheight
            }

            self$results$plot$setSize(plotwidth, total_height)
            self$results$ggpubrPlot$setSize(plotwidth, total_height)

            if (is.character(self$options$grvar) && length(self$options$grvar) == 1L && nzchar(self$options$grvar)) {
                mydata <- self$data
                grvar <- self$options$grvar

                if (!is.null(mydata) && grvar %in% names(mydata) && !is.null(mydata[[grvar]])) {
                    clean_gr <- droplevels(private$.cleanFactor(mydata[[grvar]]))
                    num_levels <- max(1L, nlevels(clean_gr))
                    ncol_estimate <- ceiling(sqrt(num_levels))
                    nrow_estimate <- ceiling(num_levels / ncol_estimate)
                    grouped_width <- min(4096L, as.integer(ncol_estimate * plotwidth))

                    if (deplen > 1) {
                        grouped_height <- min(4096L, as.integer(round(deplen * nrow_estimate * plotheight * 1.15)))
                    } else {
                        grouped_height <- min(4096L, as.integer(nrow_estimate * plotheight))
                    }

                    self$results$plot2$setSize(grouped_width, grouped_height)
                    self$results$ggpubrPlot2$setSize(grouped_width, grouped_height)
                }
            }
        },

        # Shared validation helper enforced by both .run() and all plot renderers
        .validateInputs = function() {
            deps <- private$.depVars()
            grp <- self$options$group
            grv <- self$options$grvar
            if (length(deps) == 0L || is.null(grp) || !nzchar(grp))
                return(FALSE)
            if (nrow(self$data) == 0)
                jmvcore::reject(.("Data contains no (complete) rows"))

            available_vars <- paste(names(self$data), collapse = '", "')

            for (var in deps) {
                if (!(var %in% names(self$data)))
                    jmvcore::reject(
                        .('Variable "{var}" not found in data. Available variables: "{availableVars}"'),
                        var = var, availableVars = available_vars
                    )
            }
            if (!(grp %in% names(self$data)))
                jmvcore::reject(
                    .('Grouping variable "{group}" not found in data. Available variables: "{availableVars}"'),
                    group = grp, availableVars = available_vars
                )
            if (!is.null(grv) && nzchar(grv) && !(grv %in% names(self$data)))
                jmvcore::reject(
                    .('Split-by variable "{grvar}" not found in data. Available variables: "{availableVars}"'),
                    grvar = grv, availableVars = available_vars
                )

            if (grp %in% deps)
                jmvcore::reject(
                    .("The grouping variable '{group}' cannot also be selected in 'Dependent Variables'. Choose distinct outcome and grouping columns."),
                    group = grp
                )
            if (!is.null(grv) && nzchar(grv)) {
                if (identical(grv, grp))
                    jmvcore::reject(
                        .("The 'Grouping Variable' and 'Split By' variable must be different columns (both are set to '{var}')."),
                        var = grv
                    )
                if (grv %in% deps)
                    jmvcore::reject(
                        .("The 'Split By' variable '{grvar}' cannot also be selected in 'Dependent Variables'. Choose distinct outcome and subgroup columns."),
                        grvar = grv
                    )
            }

            return(TRUE)
        },

        # Data quality validation helper (per variable)
        .validateDataQuality = function(mydata, vars) {
            for (var in vars) {
                sub_v <- private$.subsetVar(mydata, var, include_grvar = FALSE)
                num_vals <- sub_v[[var]]

                if (length(num_vals) < 3) {
                    private$.appendMessage(
                        paste0(
                            "<br>",
                            .fmt(.("Warning: {var} has fewer than 3 valid observations."),
                                 var = htmltools::htmlEscape(var)),
                            "<br>"
                        ),
                        level = "strong",
                        plain_notice = .fmt(.("Warning: {var} has fewer than 3 valid observations."), var = var)
                    )
                }
                if (length(unique(num_vals)) < 2) {
                    private$.appendMessage(
                        paste0(
                            "<br>",
                            .fmt(.("Warning: {var} has no variation (all values are identical)."),
                                 var = htmltools::htmlEscape(var)),
                            "<br>"
                        ),
                        level = "strong",
                        plain_notice = .fmt(.("Warning: {var} has no variation (all values are identical)."), var = var)
                    )
                }
            }
        },

        # Within-group IQR outlier detection helper (conditions on group so well-separated
        # clinical arms are never falsely flagged as marginal pooled outliers).
        .detectOutliers = function(data, vars, group_var = self$options$group, method = "IQR") {
            outliers <- list()
            large_dataset <- nrow(data) > 5000

            for (var in vars) {
                sub_df <- private$.subsetVar(data, var, include_grvar = FALSE)
                if (nrow(sub_df) == 0L) next
                vals <- sub_df[[var]]
                grps <- sub_df[[group_var]]

                flag <- rep(FALSE, nrow(sub_df))
                for (lvl in levels(grps)) {
                    idx <- which(grps == lvl)
                    if (length(idx) < 4L) next
                    g_vals <- vals[idx]
                    q1 <- stats::quantile(g_vals, 0.25, na.rm = TRUE)
                    q3 <- stats::quantile(g_vals, 0.75, na.rm = TRUE)
                    iqr_val <- q3 - q1
                    if (is.finite(iqr_val) && iqr_val > 0) {
                        flag[idx] <- g_vals < (q1 - 1.5 * iqr_val) | g_vals > (q3 + 1.5 * iqr_val)
                    }
                }

                outlier_indices <- which(flag)
                if (length(outlier_indices) > 0L) {
                    outliers[[var]] <- if (large_dataset) length(outlier_indices) else outlier_indices
                }
            }
            return(outliers)
        },

        # Number of grouping levels that actually carry data (ignoring <NA> factor levels).
        .nGroupLevels = function(data, group_var) {
            if (is.null(group_var) || !group_var %in% names(data)) return(NA_integer_)
            nlevels(droplevels(private$.cleanFactor(data[[group_var]])))
        },

        # Companion ggpubr statistical layer: respects Welch oneway.test when
        # K >= 3 and varequal = FALSE instead of silently running Fisher's aov().
        .ggpubrStatLayer = function(n_groups) {
            ts <- self$options$typestatistics
            two <- !is.na(n_groups) && n_groups <= 2

            if (identical(ts, "parametric") && !two && !isTRUE(self$options$varequal)) {
                welch_layer <- ggplot2::layer(
                    stat = .jjbetweenStatWelchOneway,
                    geom = "text",
                    position = "identity",
                    show.legend = FALSE,
                    inherit.aes = TRUE,
                    params = list(hjust = 0, vjust = 0, na.rm = TRUE)
                )
                return(list(
                    layer = welch_layer,
                    note  = .("Companion panel test: Welch's one-way ANOVA (oneway.test)")
                ))
            }

            method <- switch(ts,
                "parametric"    = if (two) "t.test" else "anova",
                "nonparametric" = if (two) "wilcox.test" else "kruskal.test",
                if (two) "wilcox.test" else "kruskal.test")
            args <- list(method = method)
            if (identical(ts, "parametric") && two)
                args$method.args <- list(var.equal = isTRUE(self$options$varequal))
            list(
                layer = do.call(ggpubr::stat_compare_means, args),
                note  = if (ts %in% c("robust", "bayes"))
                            .fmt(
                                .("Companion panel test: {method} (ggpubr provides no {ts} equivalent; the main figure carries the {ts} result)"),
                                method = method, ts = ts
                            )
                        else
                            .fmt(.("Companion panel test: {method}"), method = method)
            )
        },

        # Shared single-panel builder for .plotGGPubr() and .plotGGPubr2()
        .buildOneGGPubrPanel = function(sub_data, dep_var, theme = NULL, multi = FALSE, facet_var = NULL) {
            group <- self$options$group
            palette <- private$.ggpubrPaletteFor(sub_data, theme)

            panel_title <- if (isTRUE(multi)) {
                if (nchar(self$options$mytitle) > 0) paste0(self$options$mytitle, ": ", dep_var) else dep_var
            } else {
                if (nchar(self$options$mytitle) > 0) self$options$mytitle else NULL
            }

            y_lab <- if (isTRUE(multi)) {
                if (nchar(self$options$ytitle) > 0) paste0(self$options$ytitle, " (", dep_var, ")") else dep_var
            } else {
                if (nchar(self$options$ytitle) > 0) self$options$ytitle else dep_var
            }

            plot_df <- data.frame(
                .jj_x = sub_data[[group]],
                .jj_y = sub_data[[dep_var]],
                stringsAsFactors = FALSE
            )
            args <- list(
                data = plot_df,
                x = ".jj_x",
                y = ".jj_y",
                color = ".jj_x",
                title = panel_title,
                xlab = if (nchar(self$options$xtitle) > 0) self$options$xtitle else group,
                ylab = y_lab,
                add = if (self$options$ggpubrAddPoints) "jitter" else NULL,
                palette = palette
            )
            if (!is.null(facet_var)) {
                args$data$.jj_facet <- sub_data[[facet_var]]
                args$facet.by <- ".jj_facet"
            }

            p <- if (self$options$ggpubrPlotType == "boxplot") {
                do.call(ggpubr::ggboxplot, args)
            } else if (self$options$ggpubrPlotType == "violin") {
                do.call(ggpubr::ggviolin, args)
            } else {
                do.call(ggpubr::ggviolin, args) + ggplot2::geom_boxplot(width = 0.1)
            }

            if (self$options$ggpubrAddStats) {
                n_grp <- private$.nGroupLevels(sub_data, group)
                y_fin <- sub_data[[dep_var]]
                y_fin <- y_fin[is.finite(y_fin)]
                if (!is.na(n_grp) && n_grp >= 2L && length(unique(y_fin)) >= 2L) {
                    sl <- private$.ggpubrStatLayer(n_grp)
                    p <- p + sl$layer + ggplot2::labs(caption = sl$note)
                }
            }

            p + ggplot2::labs(color = group) + ggpubr::theme_pubr()
        },

        .testLabel = function(n_groups, honour_varequal = TRUE) {
            type <- self$options$typestatistics
            student <- honour_varequal && isTRUE(self$options$varequal)
            if (!is.na(n_groups) && n_groups <= 2) {
                switch(type,
                    "parametric"    = if (student) .("Student's t-test") else .("Welch's t-test"),
                    "nonparametric" = .("Mann-Whitney U test"),
                    "robust"        = .("robust test (Yuen's trimmed means)"),
                    "bayes"         = .("Bayesian t-test"),
                    .("two-group comparison"))
            } else {
                switch(type,
                    "parametric"    = if (student) .("one-way ANOVA (Fisher)") else .("Welch's ANOVA"),
                    "nonparametric" = .("Kruskal-Wallis test"),
                    "robust"        = .("robust ANOVA (trimmed means)"),
                    "bayes"         = .("Bayesian ANOVA"),
                    .("one-way ANOVA"))
            }
        },

        # Build the plot subtitle with statsExpressions (memoized via .computeStatsResult).
        .subtitleExpr = function(data, group_var, dep_var, opts) {
            if (!isTRUE(opts$resultssubtitle)) return(NULL)
            if (identical(opts$typestatistics, "bayes")) return(NULL)

            st <- private$.computeStatsResult(data, group_var, dep_var, opts)
            if (!isTRUE(st$ok)) return(NULL)
            st$expr
        },

        .bfCaptionExpr = function(data, group_var, dep_var, opts) {
            if (!isTRUE(opts$bfmessage) || !isTRUE(opts$resultssubtitle)) return(NULL)
            if (!identical(opts$typestatistics, "parametric")) return(NULL)

            n_lev <- private$.nGroupLevels(data, group_var)
            if (is.na(n_lev) || n_lev != 2L) return(NULL)

            st <- private$.computeStatsResult(data, group_var, dep_var, opts, type_override = "bayes")
            if (!isTRUE(st$ok)) return(NULL)
            st$expr
        },

        .bfCaptionUnavailable = function(data) {
            if (!isTRUE(self$options$bfmessage) || !isTRUE(self$options$resultssubtitle))
                return(FALSE)
            if (!identical(self$options$typestatistics, "parametric")) return(TRUE)
            deps <- private$.depVars()
            if (length(deps) == 0L) return(TRUE)
            all(vapply(deps, function(v) {
                n_lev <- private$.nGroupLevels(private$.subsetVar(data, v, FALSE), self$options$group)
                is.na(n_lev) || n_lev != 2L
            }, logical(1)))
        },

        .bayesProbeFails = function(data, group_var, dep_var) {
            n_lev <- private$.nGroupLevels(data, group_var)
            if (is.na(n_lev) || n_lev < 2L) return(FALSE)
            opts <- private$.prepareOptions()
            st <- private$.computeStatsResult(data, group_var, dep_var, opts, type_override = "bayes")
            !isTRUE(st$ok)
        },

        .pairwiseDisplay = function(opts) {
            if (isTRUE(opts$pairwisecomparisons)) opts$pairwisedisplay else "none"
        },

        # Check whether post-hoc pairwise comparisons can run safely on a panel without
        # crashing inside statsExpressions::pairwise_comparisons / WRS2::lincon.
        .panelSupportsPairwise = function(df_panel, group_var, dep_var, opts) {
            if (is.null(df_panel) || nrow(df_panel) < 2L) return(FALSE)
            g_clean <- droplevels(private$.cleanFactor(df_panel[[group_var]]))
            grp_counts <- table(g_clean)
            if (length(grp_counts) < 2L) return(FALSE)
            y_fin <- df_panel[[dep_var]]
            y_fin <- y_fin[is.finite(y_fin)]
            if (length(unique(y_fin)) < 2L) return(FALSE)
            if (identical(opts$typestatistics, "robust") && any(grp_counts < 2L)) return(FALSE)
            TRUE
        },

        # Sanitize ggbetweenstats / grouped_ggbetweenstats arguments on degenerate panels
        # (constant outcome or singleton group) so distribution plots render cleanly
        # without 'NA' subtitle fragments or pairwise crashes.
        .sanitizePlotArgs = function(args_list, sub_data, group_var, dep_var, opts,
                                     sub_expr = NULL, grvar = NULL) {
            y_fin <- sub_data[[dep_var]]
            y_fin <- y_fin[is.finite(y_fin)]
            is_const <- length(unique(y_fin)) < 2L

            if (is.null(grvar)) {
                if (!private$.panelSupportsPairwise(sub_data, group_var, dep_var, opts)) {
                    args_list$pairwise.display <- "none"
                }
                if (identical(opts$typestatistics, "bayes")) {
                    if (is_const || isTRUE(private$.bayesProbeFails(sub_data, group_var, dep_var))) {
                        args_list$results.subtitle <- FALSE
                        args_list$bf.message <- FALSE
                    }
                } else {
                    grp_counts <- table(droplevels(private$.cleanFactor(sub_data[[group_var]])))
                    uncomputable <- is_const || length(grp_counts) < 2L || any(grp_counts < 2L)
                    if (!is.null(sub_expr) || uncomputable) {
                        args_list$results.subtitle <- FALSE
                    }
                    if (uncomputable) {
                        args_list$bf.message <- FALSE
                    }
                }
            } else {
                st_fac <- droplevels(private$.cleanFactor(sub_data[[grvar]]))
                pw_ok <- length(levels(st_fac)) > 0L && all(vapply(levels(st_fac), function(lv) {
                    private$.panelSupportsPairwise(
                        sub_data[st_fac == lv, , drop = FALSE], group_var, dep_var, opts
                    )
                }, logical(1)))
                if (!isTRUE(pw_ok)) {
                    args_list$pairwise.display <- "none"
                }
                if (is_const) {
                    args_list$results.subtitle <- FALSE
                    args_list$bf.message <- FALSE
                }
            }
            args_list
        },

        # Prevent statsExpressions::pairwise_comparisons from double-adjusting
        # PMCMRplus::gamesHowellTest studentized-range FWER p-values on K >= 3
        # parametric comparisons, and compute true unadjusted pairwise Welch/Student
        # p-values when padjustmethod == "none".
        .fixParametricPairwise = function(plot_obj, sub_data, group_var, dep_var, opts) {
            if (is.null(plot_obj) || !inherits(plot_obj, "gg")) return(plot_obj)
            if (!isTRUE(opts$pairwisecomparisons) || !identical(opts$typestatistics, "parametric"))
                return(plot_obj)
            n_lev <- private$.nGroupLevels(sub_data, group_var)
            if (is.na(n_lev) || n_lev < 3L) return(plot_obj)

            sig_idx <- which(vapply(plot_obj$layers, function(l) inherits(l$geom, "GeomSignif"), logical(1)))
            if (length(sig_idx) == 0L) return(plot_obj)
            idx <- sig_idx[1]
            sp <- plot_obj$layers[[idx]]$stat_params
            comps <- sp$comparisons
            if (is.null(comps) || length(comps) == 0L) return(plot_obj)

            if (!identical(opts$padjustmethod, "none")) {
                # Games-Howell p-values were already single-step studentized-range
                # FWER adjusted when p.adjust.method = "none" was forwarded; relabel
                # [unadj.] -> [GH-adj.] so the annotation accurately names the adjustment.
                sp$annotations <- gsub("italic(p)[unadj.]", "italic(p)[GH-adj.]",
                                       sp$annotations, fixed = TRUE)
                plot_obj$layers[[idx]]$stat_params <- sp
                return(plot_obj)
            }

            # padjustmethod == "none": compute genuine unadjusted pairwise t-test p-values
            y_vec <- sub_data[[dep_var]]
            g_vec <- droplevels(private$.cleanFactor(sub_data[[group_var]]))
            pvals <- vapply(comps, function(pair) {
                tt <- tryCatch(
                    stats::t.test(
                        x = y_vec[g_vec == pair[1]],
                        y = y_vec[g_vec == pair[2]],
                        var.equal = isTRUE(opts$varequal)
                    ),
                    error = function(e) NULL
                )
                if (is.null(tt)) NA_real_ else unname(tt$p.value)
            }, numeric(1))

            keep_mask <- switch(opts$pairwisedisplay,
                "significant"     = !is.na(pvals) & pvals < 0.05,
                "non-significant" = !is.na(pvals) & pvals >= 0.05,
                !is.na(pvals)
            )
            if (!any(keep_mask)) {
                plot_obj$layers[[idx]] <- NULL
                return(plot_obj)
            }
            sp$comparisons <- comps[keep_mask]
            sp$annotations <- vapply(pvals[keep_mask], function(pv) {
                paste0('list(italic(p)[unadj.] == "', private$.fmtStatVal(pv, opts$k), '")')
            }, character(1))
            plot_obj$layers[[idx]]$stat_params <- sp
            plot_obj
        },

        # Apply .fixParametricPairwise() across every sub-panel of a grouped_ggbetweenstats
        # patchwork object so stratified K >= 3 parametric brackets match the main plot.
        .fixGroupedParametricPairwise = function(grouped_plot, group_var, dep_var, opts) {
            if (is.null(grouped_plot)) return(grouped_plot)
            if (!isTRUE(opts$pairwisecomparisons) || !identical(opts$typestatistics, "parametric"))
                return(grouped_plot)

            if (inherits(grouped_plot, "patchwork")) {
                n_panels <- length(grouped_plot)
                for (i in seq_len(n_panels)) {
                    panel_i <- grouped_plot[[i]]
                    if (inherits(panel_i, "gg") && !is.null(panel_i$data) &&
                        all(c(group_var, dep_var) %in% names(panel_i$data))) {
                        grouped_plot[[i]] <- private$.fixParametricPairwise(
                            panel_i, panel_i$data, group_var, dep_var, opts
                        )
                    }
                }
                return(grouped_plot)
            }

            if (inherits(grouped_plot, "gg") && !is.null(grouped_plot$data) &&
                all(c(group_var, dep_var) %in% names(grouped_plot$data))) {
                return(private$.fixParametricPairwise(
                    grouped_plot, grouped_plot$data, group_var, dep_var, opts
                ))
            }

            grouped_plot
        },

        # Palette for the ggpubr companion panels (including jamovi global palette).
        .ggpubrPaletteFor = function(data, theme = NULL) {
            n <- private$.nGroupLevels(data, self$options$group)
            if (is.na(n) || n < 1) n <- 1L
            if (isTRUE(self$options$colorblindSafe)) {
                return(grDevices::hcl.colors(n, palette = "viridis"))
            }
            if (identical(self$options$ggpubrPalette, "jamovi")) {
                pal_name <- if (!is.null(theme) && !is.null(theme$palette)) theme$palette else "jmv"
                return(jmvcore::colorPalette(n = n, pal = pal_name))
            }
            if (identical(self$options$ggpubrPalette, "default")) NULL
            else self$options$ggpubrPalette
        },

        # Theme application helper
        .applyTheme = function(plot, opts, ggtheme) {
            if (!opts$originaltheme) {
                plot <- plot + ggtheme
            } else {
                plot <- plot + ggstatsplot::theme_ggstatsplot()
            }

            if (opts$colorblindSafe) {
                plot <- plot + ggplot2::scale_fill_viridis_d(option = "D") +
                              ggplot2::scale_color_viridis_d(option = "D")
            }

            return(plot)
        },

        # Clinical results summary rendered from .run()
        .generateClinicalSummary = function() {
            deps <- private$.depVars()
            if (length(deps) == 0L || is.null(self$options$group))
                return()

            mydata <- private$.prepareData()
            n_per_dep <- vapply(deps, function(v) nrow(private$.subsetVar(mydata, v, FALSE)), integer(1))
            n_groups_vec <- vapply(deps, function(v) {
                private$.nGroupLevels(private$.subsetVar(mydata, v, FALSE), self$options$group)
            }, integer(1))
            n_groups <- if (length(n_groups_vec) > 0L && any(!is.na(n_groups_vec))) max(n_groups_vec, na.rm = TRUE) else 0L
            n_desc <- if (length(unique(n_per_dep)) <= 1L) {
                as.character(n_per_dep[1])
            } else {
                paste0(min(n_per_dep), "\u2013", max(n_per_dep))
            }

            test_name <- private$.testLabel(n_groups)
            vars <- htmltools::htmlEscape(paste(deps, collapse = ", "))
            all_no_stat <- length(deps) > 0L && length(private$.noStatisticVars) == length(deps)

            where <- if (!isTRUE(self$options$resultssubtitle)) {
                .("No test statistic is displayed: switch on 'Statistical results' to show it in the plot subtitle.")
            } else if (isTRUE(private$.bayesNoStatistic)) {
                .("No test statistic is displayed: the Bayesian test could not be computed for this data, so the plot subtitle is empty (see Data Diagnostics).")
            } else if (isTRUE(all_no_stat)) {
                .("No test statistic is displayed: the plot subtitle could not be computed for this data (see Data Diagnostics).")
            } else if (identical(self$options$typestatistics, "bayes")) {
                .("See the plot subtitle for the test statistic (Bayes factor) and posterior credible interval.")
            } else {
                .("See the plot subtitle for the test statistic, p-value, and effect size.")
            }

            summary_sentence <- .fmt(
                .("{testName} comparing {vars} across {nGroups} group(s) (n = {nDesc}). {where}"),
                testName = test_name,
                vars = vars,
                nGroups = n_groups,
                nDesc = n_desc,
                where = where
            )
            clinical_text <- paste0(
                "<div style='padding: 12px 15px; background-color: rgba(33, 151, 33, 0.08); border-left: 4px solid #28a745; margin: 10px 0; color: inherit;'>",
                "<h4 style='margin-top: 0;'>", .("Results Summary"), "</h4>",
                "<p style='margin-bottom: 0;'>", summary_sentence, "</p></div>"
            )
            self$results$clinicalSummary$setContent(clinical_text)
        },

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

        # Multiple-endpoint guidance HTML (distinguishing Bayesian vs frequentist modes)
        .endpointGuidanceHtml = function() {
            dep <- private$.depVars()
            if (length(dep) <= 1L)
                return("")

            num_endpoints <- length(dep)

            if (identical(self$options$typestatistics, "bayes")) {
                body <- .fmt(
                    .("<strong>Multiple Endpoint Bayesian Analysis:</strong><br>You are evaluating {n} dependent variables simultaneously. Each plot reports its own Bayes factor (log_e BF_01) and posterior credible interval under an independent prior; consider prior calibration on model probabilities when interpreting evidence across multiple endpoints."),
                    n = num_endpoints
                )
                return(paste0(
                    "<div style='padding: 15px; background-color: rgba(33, 85, 151, 0.08); border-left: 4px solid #007bff; margin: 10px 0; color: inherit;'>",
                    "<p style='margin: 0;'>", body, "</p></div>"
                ))
            }

            correction <- self$options$multiEndpointCorrection

            body <- if (correction == "none") {
                actual_alpha <- 1 - (1 - 0.05)^num_endpoints
                .fmt(
                    .("<strong>Multiple Endpoint Testing Without Correction:</strong><br>You are testing {n} dependent variables simultaneously without adjustment. Under independent null hypotheses, this inflates the family-wise error rate from 5% to approximately {fwer}%.<br><strong>Recommendation:</strong> Select a correction method for manual threshold guidance, or interpret unadjusted p-values cautiously as exploratory."),
                    n = num_endpoints,
                    fwer = sprintf("%.1f", actual_alpha * 100)
                )
            } else if (correction == "bonferroni") {
                adj_str <- sprintf("%.4f", 0.05 / num_endpoints)
                .fmt(
                    .("<strong>Bonferroni Correction Guidance (Manual Application Required):</strong><br>You are testing {n} endpoints. To control family-wise error rate at 5%:<br>\u2022 <strong>Adjusted significance threshold: \u03b1 = {adj}</strong><br>\u2022 Compare each p-value from the plots below to {adj} (not 0.05)<br>\u2022 Only results with p &lt; {adj} should be considered statistically significant<br><strong>Important:</strong> This correction is not applied automatically to the plot subtitles."),
                    n = num_endpoints, adj = adj_str
                )
            } else if (correction == "holm") {
                first_str <- sprintf("%.4f", 0.05 / num_endpoints)
                second_n  <- num_endpoints - 1L
                second_str <- sprintf("%.4f", 0.05 / second_n)
                .fmt(
                    .("<strong>Holm Step-Down Guidance (Manual Application Required):</strong><br>You are testing {n} endpoints. To apply Holm's step-down procedure:<br>1. Rank all endpoint p-values from smallest to largest<br>2. Test the smallest p-value against \u03b1 = 0.05/{n} = {first}<br>3. If significant, test the second smallest against \u03b1 = 0.05/{n2} = {second}<br>4. Continue until a p-value exceeds its threshold; retain all remaining hypotheses.<br><strong>Important:</strong> Collect p-values from the plots below and apply this step-down check manually."),
                    n = num_endpoints, first = first_str, n2 = second_n, second = second_str
                )
            } else if (correction == "fdr") {
                .fmt(
                    .("<strong>Benjamini-Hochberg FDR Guidance (Manual Application Required):</strong><br>You are testing {n} endpoints. To control false discovery rate at 5%:<br>1. Rank all endpoint p-values from smallest to largest (i = 1, ..., {n})<br>2. Find the largest rank i where p_i \u2264 (i/{n}) \u00d7 0.05<br>3. Treat endpoints 1 through i as discoveries (or pass the vector of p-values to stats::p.adjust(..., method = 'BH') in R).<br><strong>Important:</strong> FDR adjustment is not applied automatically to the plot subtitles."),
                    n = num_endpoints
                )
            } else {
                return("")
            }

            border <- if (correction == "none") "#dc3545" else "#ffc107"
            bg     <- if (correction == "none") "rgba(255, 33, 33, 0.12)" else "rgba(255, 202, 33, 0.23)"
            paste0(
                "<div style='padding: 15px; background-color: ", bg,
                "; border-left: 4px solid ", border, "; margin: 10px 0; color: inherit;'>",
                "<p style='margin: 0;'>", body, "</p></div>"
            )
        },

        # Statistical assumption checker (evaluates each variable and each group
        # independently so one small n = 2 arm never suppresses checks on larger arms,
        # flags zero-variance arms explicitly, and checks both skewness and kurtosis for n > 200).
        .checkAssumptions = function(data, variables, group_var, test_type, emit_diagnostics = FALSE) {
            warnings <- character()

            for (var in variables) {
                sub_df <- private$.subsetVar(data, var, include_grvar = FALSE)
                var_data <- sub_df[[var]]
                group_data <- sub_df[[group_var]]

                group_counts <- table(droplevels(private$.cleanFactor(group_data)), useNA = "no")
                min_group_size <- if (length(group_counts)) min(group_counts) else 0L

                if (min_group_size < 3) {
                    msg_html <- .fmt(
                        .("{var}: Minimum group size is {n} (recommend \u22653)"),
                        var = htmltools::htmlEscape(var), n = min_group_size
                    )
                    warnings <- c(warnings, msg_html)
                    if (isTRUE(emit_diagnostics)) {
                        private$.appendMessage(
                            paste0("<br>", msg_html),
                            level = "strong",
                            plain_notice = .fmt(
                                .("{var}: Minimum group size is {n} (recommend >= 3)."),
                                var = var, n = min_group_size
                            )
                        )
                    }
                }

                if (test_type == "parametric") {
                    # Levene's test runs whenever at least 2 groups have >= 2 observations
                    if (sum(group_counts >= 2L) >= 2L && requireNamespace("car", quietly = TRUE)) {
                        tryCatch({
                            levene_result <- suppressWarnings(withBaseFormulaChar(
                                car::leveneTest(var_data ~ group_data, center = median)
                            ))
                            levene_p <- levene_result$`Pr(>F)`[1]
                            if (!is.na(levene_p) && levene_p < 0.05) {
                                msg_lev <- .fmt(
                                    .("{var}: Variances differ significantly between groups (Unequal variances: Levene's test {pClause})."),
                                    var = htmltools::htmlEscape(var),
                                    pClause = private$.fmtPClause(levene_p, html = TRUE)
                                )
                                warnings <- c(warnings, msg_lev)
                                if (isTRUE(emit_diagnostics)) {
                                    private$.appendMessage(paste0("<br>", msg_lev), level = "warn")
                                }
                                if (isTRUE(self$options$varequal)) {
                                    msg_eq <- .fmt(
                                        .("{var}: 'Equal variances' is ticked, so Student's test is being reported despite unequal variances. Untick it to use Welch's test, which does not assume equal variances."),
                                        var = htmltools::htmlEscape(var)
                                    )
                                    warnings <- c(warnings, msg_eq)
                                    if (isTRUE(emit_diagnostics)) {
                                        private$.appendMessage(
                                            paste0("<br>", msg_eq),
                                            level = "strong",
                                            plain_notice = .fmt(
                                                .("{var}: 'Equal variances' is enabled despite significant variance heterogeneity (Levene's {pClause}); untick 'Equal variances' to use Welch's test."),
                                                var = var,
                                                pClause = private$.fmtPClause(levene_p, html = FALSE)
                                            )
                                        )
                                    }
                                }
                            }
                        }, error = function(e) NULL)
                    }

                    # Per-group normality & zero-variance checks
                    for (level in names(group_counts)) {
                        group_subset <- var_data[group_data == level & !is.na(var_data)]
                        n_subset <- length(group_subset)

                        if (n_subset >= 2L && length(unique(group_subset)) < 2L) {
                            msg_zv <- .fmt(
                                .("{var}: Group '{level}' has zero variance (all values are identical); parametric normality and variance assumptions are violated."),
                                var = htmltools::htmlEscape(var),
                                level = htmltools::htmlEscape(level)
                            )
                            warnings <- c(warnings, msg_zv)
                            if (isTRUE(emit_diagnostics)) {
                                private$.appendMessage(
                                    paste0("<br>", msg_zv),
                                    level = "strong",
                                    plain_notice = .fmt(
                                        .("{var}: Group '{level}' has zero variance; parametric assumptions are violated."),
                                        var = var, level = level
                                    )
                                )
                            }
                        } else if (n_subset >= 3L && n_subset <= 200L) {
                            p_val <- tryCatch(stats::shapiro.test(group_subset)$p.value, error = function(e) NA_real_)
                            if (!is.na(p_val) && p_val < 0.05) {
                                msg_sw <- .fmt(
                                    .("{var}: Non-normal distribution detected in group '{level}' (Shapiro-Wilk {pClause}, consider nonparametric or robust approach)."),
                                    var = htmltools::htmlEscape(var),
                                    level = htmltools::htmlEscape(level),
                                    pClause = private$.fmtPClause(p_val, html = TRUE)
                                )
                                warnings <- c(warnings, msg_sw)
                                if (isTRUE(emit_diagnostics)) {
                                    private$.appendMessage(paste0("<br>", msg_sw), level = "warn")
                                }
                            }
                        } else if (n_subset > 200L) {
                            moments <- tryCatch({
                                mean_val <- mean(group_subset)
                                sd_val <- stats::sd(group_subset)
                                if (is.finite(sd_val) && sd_val > 0) {
                                    z <- (group_subset - mean_val) / sd_val
                                    c(skew = mean(z^3), kurt = mean(z^4) - 3)
                                } else {
                                    c(skew = 0, kurt = 0)
                                }
                            }, error = function(e) c(skew = 0, kurt = 0))

                            if (abs(moments[["skew"]]) > 1 || abs(moments[["kurt"]]) > 2) {
                                msg_lg <- .fmt(
                                    .("{var}: Large sample (n = {n}) in group '{level}' shows substantial non-normality (skewness = {skew}, excess kurtosis = {kurt}). Visual inspection or a robust/nonparametric approach is recommended."),
                                    var = htmltools::htmlEscape(var),
                                    n = n_subset,
                                    level = htmltools::htmlEscape(level),
                                    skew = sprintf("%.2f", moments[["skew"]]),
                                    kurt = sprintf("%.2f", moments[["kurt"]])
                                )
                                warnings <- c(warnings, msg_lg)
                                if (isTRUE(emit_diagnostics)) {
                                    private$.appendMessage(paste0("<br>", msg_lg), level = "warn")
                                }
                            }
                        }
                    }
                }
            }

            return(warnings)
        },

        # Data preparation with per-variable NA isolation and column-scoped hashing
        .prepareData = function(force_refresh = FALSE) {
            deps <- private$.depVars()
            used_cols <- intersect(
                unique(c(deps, self$options$group, self$options$grvar)),
                names(self$data)
            )
            scoped_data <- self$data[, used_cols, drop = FALSE]

            current_hash <- digest::digest(list(
                dep = deps,
                group = self$options$group,
                data = scoped_data,
                grvar = self$options$grvar,
                typestatistics = self$options$typestatistics,
                varequal = self$options$varequal
            ), algo = "md5")

            if (!is.null(private$.processedData) &&
                identical(private$.data_hash, current_hash) &&
                !force_refresh) {
                return(private$.processedData)
            }

            private$.checkpoint()

            private$.accumulated_info <- character()
            private$.accumulated_warnings <- character()
            private$.accumulated_strong <- character()
            private$.strongNoticeMessages <- character()
            private$.statsCache <- list()

            mydata <- scoped_data
            vars <- deps
            grp_col <- self$options$group

            private$.checkpoint(flush = FALSE)
            for (var in vars) {
                num_v <- jmvcore::toNumeric(mydata[[var]])
                num_v[!is.finite(num_v)] <- NA_real_
                mydata[[var]] <- num_v
            }

            mydata[[grp_col]] <- private$.cleanFactor(mydata[[grp_col]])
            if (!is.null(self$options$grvar) && self$options$grvar %in% names(mydata)) {
                mydata[[self$options$grvar]] <- private$.cleanFactor(mydata[[self$options$grvar]])
            }

            n_before <- nrow(mydata)

            if (length(vars) == 1L) {
                keep_rows <- !is.na(mydata[[grp_col]]) & is.finite(mydata[[vars[1]]])
                mydata <- mydata[keep_rows, , drop = FALSE]
                n_after <- nrow(mydata)
                if (n_before > n_after) {
                    n_dropped <- n_before - n_after
                    private$.appendMessage(
                        paste0(
                            "<br>",
                            .fmt(.("Info: {nDropped} rows with missing or non-finite values in analysis variables were excluded."),
                                 nDropped = n_dropped),
                            "<br>"
                        ),
                        level = "info"
                    )
                }
            } else {
                # Multiple dependent variables: keep every row that has a valid group and
                # at least one finite dependent variable so missingness in dep[2] never
                # deletes valid observations of dep[1]. Each downstream computation
                # subsets per variable via private$.subsetVar().
                any_dep_ok <- Reduce(`|`, lapply(mydata[vars], is.finite))
                keep_rows <- !is.na(mydata[[grp_col]]) & any_dep_ok
                per_var_dropped <- vapply(vars, function(v) {
                    sum(is.na(mydata[[grp_col]]) | !is.finite(mydata[[v]]))
                }, integer(1))
                mydata <- mydata[keep_rows, , drop = FALSE]
                if (any(per_var_dropped > 0L)) {
                    counts_str <- paste(
                        vapply(seq_along(vars), function(i) {
                            paste0(htmltools::htmlEscape(vars[i]), ": ", per_var_dropped[i])
                        }, character(1)),
                        collapse = "; "
                    )
                    private$.appendMessage(
                        paste0(
                            "<br>",
                            .fmt(.("Info: Rows with missing or non-finite values excluded per variable ({details})."),
                                 details = counts_str),
                            "<br>"
                        ),
                        level = "info"
                    )
                }
            }

            # Verify every selected variable retains >= 2 non-empty groups
            for (v in vars) {
                sub_v <- private$.subsetVar(mydata, v, include_grvar = FALSE)
                n_group_levels <- private$.nGroupLevels(sub_v, grp_col)
                if (is.na(n_group_levels) || n_group_levels < 2L) {
                    jmvcore::reject(
                        .("The grouping variable '{group}' must have at least 2 groups with data after removing missing values (found {n}). Check for a constant column or excessive missing data."),
                        group = grp_col, n = if (is.na(n_group_levels)) 0L else n_group_levels
                    )
                }
            }

            private$.validateDataQuality(mydata, vars)

            assumption_warnings <- private$.checkAssumptions(
                mydata, vars, grp_col, self$options$typestatistics, emit_diagnostics = TRUE
            )
            private$.assumptionWarnings <- assumption_warnings

            private$.checkpoint(flush = FALSE)
            outliers <- private$.detectOutliers(mydata, vars, group_var = grp_col)
            if (length(outliers) > 0) {
                for (var in names(outliers)) {
                    n_outliers <- if (nrow(mydata) > 5000)
                        as.integer(outliers[[var]]) else length(outliers[[var]])
                    private$.appendMessage(
                        paste0(
                            "<br>",
                            .fmt(.("{var} has {nOutliers} potential outlier(s) detected within groups"),
                                 var = htmltools::htmlEscape(var), nOutliers = n_outliers),
                            "<br>"
                        ),
                        level = "warn"
                    )
                }
            }

            private$.storeDiagnostics()

            private$.processedData <- mydata
            private$.data_hash <- current_hash
            return(mydata)
        },

        .processTitle = function(title) {
            if (is.null(title) || title == "") NULL else title
        },

        .prepareOptions = function(force_refresh = FALSE) {
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
                colorblindSafe = self$options$colorblindSafe,
                seed = private$.safeSeed()
            ), algo = "md5")

            if (!is.null(private$.processedOptions) &&
                identical(private$.options_hash, current_options_hash) &&
                !force_refresh) {
                return(private$.processedOptions)
            }

            private$.statsCache <- list()

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

            options$violinargs <- list(width = 0.5, alpha = 0.2, na.rm = TRUE)
            options$boxplotargs <- list(width = 0.3, alpha = 0.5, na.rm = TRUE)
            options$pointargs <- list(
                position = ggplot2::position_jitterdodge(dodge.width = 0.6),
                alpha = 0.4,
                size = 3,
                stroke = 0
            )

            if (options$centralityplotting) {
                options$centrality.plotting <- TRUE
                options$centrality.type <- options$centralitytype
                options$centrality.point.args <- list(size = 5, color = "darkred")
                options$centrality.label.args <- list(size = 3, nudge_x = 0.4, segment.linetype = 4)
            } else {
                options$centrality.plotting <- FALSE
            }

            private$.processedOptions <- options
            private$.options_hash <- current_options_hash
            return(options)
        },

        # run ----
        .run = function() {
            withr::local_preserve_seed()

            deps <- private$.depVars()
            if (length(deps) == 0L || is.null(self$options$group)) {
                partial <- (length(deps) > 0L) || !is.null(self$options$group)
                missing_hint <- if (partial) {
                    still_needed <- c(
                        if (length(deps) == 0L)
                            .("Select at least one continuous variable in 'Dependent Variables'."),
                        if (is.null(self$options$group))
                            .("Select a categorical variable in 'Grouping Variable'.")
                    )
                    paste0("<p style='margin: 8px 0 0 0;'><strong>", .("Next step:"), "</strong> ",
                           paste(still_needed, collapse = " "), "</p>")
                } else {
                    ""
                }

                todo <- paste0(
                    "<div class='jmv-welcome' style='padding: 15px; background-color: rgba(33, 85, 151, 0.08); border-left: 4px solid #007bff; margin: 10px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0;'>", .("Box-Violin Plots to Compare Between Groups"), "</h4>",
                    "<p>", .("Select one or more continuous dependent variables and a categorical grouping variable in the left panel to compare independent groups."), "</p>",
                    missing_hint,
                    "<p style='margin-bottom: 0;'>",
                    .("Documentation:"), " ",
                    "<a href='https://www.indrapatil.com/ggstatsplot/reference/ggbetweenstats.html' target='_blank'>ggbetweenstats</a>",
                    " \u2022 ",
                    "<a href='https://www.indrapatil.com/ggstatsplot/reference/grouped_ggbetweenstats.html' target='_blank'>grouped_ggbetweenstats</a>",
                    "</p></div>"
                )

                private$.syncValidityNotice(character())
                self$results$todo$setContent(todo)
                self$results$mecGuidance$setContent("")
                self$results$diagnostics$setContent("")
                self$results$clinicalSummary$setContent("")
                self$results$about$setContent("")
                self$results$summary$setContent("")
                self$results$assumptions$setContent("")
                self$results$interpretation$setContent("")
                self$results$report$setContent("")
                return()
            }

            self$results$todo$setContent("")

            if (!private$.validateInputs())
                return()

            private$.checkpoint()

            mydata <- private$.prepareData()
            private$.syncValidityNotice(private$.strongNoticeMessages)

            self$results$mecGuidance$setContent(private$.endpointGuidanceHtml())

            diag_html <- if (is.null(private$.diagnosticsHtml)) "" else private$.diagnosticsHtml
            sub_html <- if (is.null(mydata)) "" else private$.subtitleDiagnosticsHtml(mydata)
            diag_html <- private$.mergeDiagnosticsHtml(diag_html, sub_html)
            self$results$diagnostics$setContent(diag_html)

            private$.generateClinicalSummary()

            if (isTRUE(self$options$showexplanations)) {
                private$.generateAboutContent()
                private$.generateSummary()
                private$.generateAssumptionsContent()
                private$.generateInterpretationGuide()
                private$.generateCopyReadyReport()
            } else {
                self$results$about$setContent("")
                self$results$summary$setContent("")
                self$results$assumptions$setContent("")
                self$results$interpretation$setContent("")
                self$results$report$setContent("")
            }
        },

        .generateAboutContent = function() {
            about_content <- paste0(
                "<div style='padding: 15px; background-color: rgba(138, 155, 172, 0.06); border-left: 4px solid #007bff; margin: 10px 0; color: inherit;'>",
                "<h4 style='margin-top: 0;'>", .("About Between-Group Comparison"), "</h4>",
                "<p><strong>", .("Purpose:"), "</strong> ",
                .("Compare continuous measurements across independent groups using combined box-violin-jitter plots and hypothesis testing."), "</p>",
                "<p><strong>", .("Typical Clinical Applications:"), "</strong></p>",
                "<ul>",
                "<li><strong>", .("Clinical trials:"), "</strong> ", .("Compare continuous endpoints across randomized treatment arms."), "</li>",
                "<li><strong>", .("Biomarker studies:"), "</strong> ", .("Evaluate biomarker distributions across diagnostic or molecular subtypes."), "</li>",
                "<li><strong>", .("Pathology:"), "</strong> ", .("Compare patient-level morphometric or immunohistochemical scores across histologic grades (one observation per independent patient)."), "</li>",
                "</ul>",
                "</div>"
            )
            self$results$about$setContent(about_content)
        },

        .generateSummary = function() {
            deps <- private$.depVars()
            if (length(deps) == 0L || is.null(self$options$group))
                return()

            mydata <- private$.prepareData()
            n_per_dep <- vapply(deps, function(v) nrow(private$.subsetVar(mydata, v, FALSE)), integer(1))
            n_desc <- if (length(unique(n_per_dep)) <= 1L) {
                as.character(n_per_dep[1])
            } else {
                .fmt(.("{minN}\u2013{maxN} (per variable)"), minN = min(n_per_dep), maxN = max(n_per_dep))
            }
            n_groups <- max(vapply(deps, function(v) {
                private$.nGroupLevels(private$.subsetVar(mydata, v, FALSE), self$options$group)
            }, integer(1)), na.rm = TRUE)
            dep_vars <- htmltools::htmlEscape(paste(deps, collapse = ", "))
            test_method <- private$.testLabel(n_groups)

            multi_var_note <- if (length(deps) == 1L) {
                ""
            } else {
                paste0(
                    "<p><strong>", .("Note:"), "</strong> ",
                    .fmt(.("{k} dependent variables analyzed. Each variable is tested separately using its own non-missing observations."),
                         k = length(deps)),
                    "</p>"
                )
            }

            method_line <- if (isTRUE(private$.bayesNoStatistic)) {
                .fmt(.("{method} (could not be computed for this data; see Data Diagnostics)"),
                     method = test_method)
            } else {
                test_method
            }

            interval_label <- if (identical(self$options$typestatistics, "bayes")) {
                .("Credible Level:")
            } else {
                .("Confidence Level:")
            }

            summary_content <- paste0(
                "<div style='padding: 15px; background-color: rgba(33, 159, 33, 0.1); border-left: 4px solid #28a745; margin: 10px 0; color: inherit;'>",
                "<h4 style='margin-top: 0;'>", .("Analysis Summary"), "</h4>",
                "<p><strong>", .("Variables Analyzed:"), "</strong> ",
                .fmt(.("{deps} by {group}"), deps = dep_vars, group = htmltools::htmlEscape(self$options$group)),
                "</p>",
                multi_var_note,
                "<p><strong>", .("Sample Size:"), "</strong> ",
                .fmt(.("{nDesc} observations across {nGroups} groups"), nDesc = n_desc, nGroups = n_groups),
                "</p>",
                "<p><strong>", .("Statistical Method:"), "</strong> ", method_line, "</p>",
                if (self$options$pairwisecomparisons && n_groups > 2) paste0(
                    "<p><strong>", .("Post-hoc Analysis:"), "</strong> ",
                    if (identical(self$options$typestatistics, "parametric") && !identical(self$options$padjustmethod, "none"))
                        .("Pairwise Games-Howell comparisons (studentized-range FWER adjustment)")
                    else
                        .fmt(.("Pairwise comparisons with {adj} adjustment"), adj = htmltools::htmlEscape(self$options$padjustmethod)),
                    "</p>"
                ) else "",
                if (!is.null(self$options$grvar)) paste0(
                    "<p><strong>", .("Subgroup Analysis:"), "</strong> ",
                    .fmt(.("Results stratified by {grvar}"), grvar = htmltools::htmlEscape(self$options$grvar)),
                    "</p>"
                ) else "",
                "<p style='margin-bottom: 0;'><strong>", interval_label, "</strong> ", (self$options$conflevel * 100), "%</p>",
                "</div>"
            )

            self$results$summary$setContent(summary_content)
        },

        .generateAssumptionsContent = function() {
            deps <- private$.depVars()
            if (length(deps) == 0L || is.null(self$options$group))
                return()

            mydata <- private$.prepareData()
            warnings <- private$.assumptionWarnings
            if (is.null(warnings)) {
                warnings <- private$.checkAssumptions(mydata, deps, self$options$group, self$options$typestatistics)
            }

            multi_endpoint_note <- ""
            if (length(deps) > 1L && !identical(self$options$typestatistics, "bayes")) {
                multi_endpoint_note <- paste0(
                    "<div style='background-color: rgba(255, 33, 33, 0.12); border-left: 4px solid #dc3545; padding: 10px; margin: 10px 0; color: inherit;'>",
                    "<p style='margin: 0;'><strong>", .("Multiple Endpoint Testing:"), "</strong> ",
                    .fmt(.("You are analyzing {k} dependent variables. Each frequentist test uses an unadjusted \u03b1 = 0.05 threshold unless you manually apply the guidance shown above."),
                         k = length(deps)),
                    "</p></div>"
                )
            }

            assumptions_content <- paste0(
                "<div style='padding: 15px; background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; margin: 10px 0; color: inherit;'>",
                "<h4 style='margin-top: 0;'>", .("Statistical Assumptions & Warnings"), "</h4>",
                multi_endpoint_note,
                "<p><strong>", .("Core Design Assumptions:"), "</strong></p>",
                "<ul>",
                "<li>", .("Dependent variable is continuous."), "</li>",
                "<li>", .("Observations are independent (one measurement per patient/subject; clustered or multi-core TMA data require mixed-effects or patient-level aggregation)."), "</li>",
                "<li>", .("Groups are mutually exclusive and independent."), "</li>",
                if (self$options$typestatistics == "parametric") paste0("<li>", .("Within-group distributions are approximately normal."), "</li>") else "",
                if (self$options$typestatistics == "parametric" && self$options$varequal) paste0("<li>", .("Homogeneity of variances across groups."), "</li>") else "",
                if (self$options$typestatistics == "bayes") paste0("<li>", .("Default Jeffreys-Zellner-Siow Cauchy prior scale (r = 0.707) is appropriate for standardized group differences."), "</li>") else "",
                "</ul>",
                if (length(warnings) > 0) paste0(
                    "<p><strong>", .("Detected Issues & Guidance:"), "</strong></p>",
                    "<ul><li>", paste(warnings, collapse = "</li><li>"), "</li></ul>"
                ) else paste0("<p style='margin-bottom: 0;'>", .("No major sample-size, variance, or normality warnings were triggered."), "</p>"),
                "</div>"
            )

            self$results$assumptions$setContent(assumptions_content)
        },

        .generateInterpretationGuide = function() {
            subtitle_on <- isTRUE(self$options$resultssubtitle)
            is_bayes <- identical(self$options$typestatistics, "bayes")

            ev_section <- if (is_bayes) {
                paste0(
                    "<p><strong>", .("Bayesian Evidence (log_e BF_01):"), "</strong></p>",
                    "<ul>",
                    "<li>", .("ggstatsplot reports the natural logarithm of BF_01 (null over alternative): negative values favor a group difference (H1), whereas positive values favor the null hypothesis of no difference (H0)."), "</li>",
                    "<li>", .("Conventional Jeffreys thresholds: |log_e(BF_01)| between 1.10 and 2.30 indicates moderate evidence (BF between 3 and 10), and |log_e(BF_01)| > 2.30 indicates strong evidence (BF > 10)."), "</li>",
                    "</ul>"
                )
            } else {
                non_sig_note <- if (subtitle_on) {
                    .("Absence of statistically significant difference is not proof of equivalence; inspect the effect size and its confidence interval in the plot subtitle to judge whether clinically meaningful effects remain compatible with the data.")
                } else {
                    .("Absence of statistically significant difference is not proof of equivalence; enable 'Statistical results' to inspect the effect size and confidence interval in the plot subtitle.")
                }
                paste0(
                    "<p><strong>", .("Frequentist Hypothesis Testing:"), "</strong></p>",
                    "<ul>",
                    "<li><strong>p &lt; 0.05:</strong> ", .("Statistically significant difference between groups at the conventional 5% level."), "</li>",
                    "<li><strong>p \u2265 0.05:</strong> ", non_sig_note, "</li>",
                    "<li>", .("When the Bayes factor caption is enabled for two-group parametric tests, negative log_e(BF_01) favors H1 (difference) and positive log_e(BF_01) favors H0 (null)."), "</li>",
                    "</ul>"
                )
            }

            interpretation_content <- paste0(
                "<div style='padding: 15px; background-color: rgba(33, 163, 188, 0.21); border-left: 4px solid #17a2b8; margin: 10px 0; color: inherit;'>",
                "<h4 style='margin-top: 0;'>", .("How to Interpret Results"), "</h4>",
                ev_section,
                "<p><strong>", .("Reference Effect Size Benchmarks (Cohen, 1988):"), "</strong></p>",
                "<ul>",
                "<li><strong>Cohen's d / Hedges' g:</strong> ", .("0.2 (small), 0.5 (medium), 0.8 (large) standardized difference."), "</li>",
                "<li><strong>Eta-squared / Omega-squared (\u03b7\u00b2 / \u03c9\u00b2):</strong> ", .("0.01 (small), 0.06 (medium), 0.14 (large) proportion of variance explained."), "</li>",
                "<li><strong>Rank-biserial r / Epsilon-squared / Wilcox \u03be:</strong> ", .("Interpret alongside raw clinical units (medians/IQRs or trimmed means), as generic benchmarks from Cohen (1988) do not replace disease-specific minimal clinically important differences."), "</li>",
                "</ul>",
                "</div>"
            )

            self$results$interpretation$setContent(interpretation_content)
        },

        # Format a single dependent variable's computed statistics into a copy-ready sentence
        .formatAutoFilledReportLine = function(var_name, sub_v, opts) {
            grp_col <- self$options$group
            n_lev <- private$.nGroupLevels(sub_v, grp_col)
            if (is.na(n_lev) || n_lev < 2L) return(NULL)
            is_bayes <- identical(opts$typestatistics, "bayes")

            st <- private$.computeStatsResult(
                sub_v, grp_col, var_name, opts,
                type_override = if (is_bayes) "bayes" else NULL
            )
            if (!isTRUE(st$ok) || is.null(st$res) || nrow(st$res) < 1L) return(NULL)

            row1 <- st$res[1, , drop = FALSE]
            ci_pct <- round(100 * opts$conflevel)
            n_obs <- nrow(sub_v)
            var_esc <- htmltools::htmlEscape(var_name)
            method_lbl <- htmltools::htmlEscape(private$.testLabel(n_lev))

            if (is_bayes) {
                log_bf01 <- if ("log_e_bf10" %in% names(row1)) -as.numeric(row1$log_e_bf10[1]) else NA_real_
                est_val <- if ("estimate" %in% names(row1)) as.numeric(row1$estimate[1]) else NA_real_
                ci_lo   <- if ("conf.low" %in% names(row1)) as.numeric(row1$conf.low[1]) else NA_real_
                ci_hi   <- if ("conf.high" %in% names(row1)) as.numeric(row1$conf.high[1]) else NA_real_
                if (!all(is.finite(c(log_bf01, est_val, ci_lo, ci_hi)))) return(NULL)
                ci_method <- if ("conf.method" %in% names(row1) && !is.na(row1$conf.method[1]) &&
                                 nzchar(as.character(row1$conf.method[1]))) {
                    htmltools::htmlEscape(as.character(row1$conf.method[1]))
                } else if (n_lev == 2L) {
                    "ETI"
                } else {
                    "HDI"
                }
                if (n_lev == 2L) {
                    return(.fmt(
                        .("{method} comparing {var} across {nGroups} groups (n = {nObs}) yielded log_e(BF_01) = {logBf01}, posterior difference = {est} ({ci}% {ciMethod}: {ciLo} to {ciHi})."),
                        method = method_lbl,
                        var = var_esc,
                        nGroups = n_lev,
                        nObs = n_obs,
                        logBf01 = private$.fmtStatVal(log_bf01, opts$k),
                        est = private$.fmtStatVal(est_val, opts$k),
                        ci = ci_pct,
                        ciMethod = ci_method,
                        ciLo = private$.fmtStatVal(ci_lo, opts$k),
                        ciHi = private$.fmtStatVal(ci_hi, opts$k)
                    ))
                }
                return(.fmt(
                    .("{method} comparing {var} across {nGroups} groups (n = {nObs}) yielded log_e(BF_01) = {logBf01}, posterior Bayesian R\u00b2 = {est} ({ci}% {ciMethod}: {ciLo} to {ciHi})."),
                    method = method_lbl,
                    var = var_esc,
                    nGroups = n_lev,
                    nObs = n_obs,
                    logBf01 = private$.fmtStatVal(log_bf01, opts$k),
                    est = private$.fmtStatVal(est_val, opts$k),
                    ci = ci_pct,
                    ciMethod = ci_method,
                    ciLo = private$.fmtStatVal(ci_lo, opts$k),
                    ciHi = private$.fmtStatVal(ci_hi, opts$k)
                ))
            }

            stat_val <- if ("statistic" %in% names(row1)) as.numeric(row1$statistic[1]) else NA_real_
            pval     <- if ("p.value" %in% names(row1)) as.numeric(row1$p.value[1]) else NA_real_
            est_val  <- if ("estimate" %in% names(row1)) as.numeric(row1$estimate[1]) else NA_real_
            ci_lo    <- if ("conf.low" %in% names(row1)) as.numeric(row1$conf.low[1]) else NA_real_
            ci_hi    <- if ("conf.high" %in% names(row1)) as.numeric(row1$conf.high[1]) else NA_real_
            if (!is.finite(stat_val) || !is.finite(pval) || !is.finite(est_val)) return(NULL)

            df1 <- if ("df" %in% names(row1) && is.finite(row1$df[1])) private$.fmtStatVal(as.numeric(row1$df[1]), opts$k) else NULL
            df2 <- if ("df.error" %in% names(row1) && is.finite(row1$df.error[1])) {
                d2 <- as.numeric(row1$df.error[1])
                if (abs(d2 - round(d2)) < 1e-8) as.character(as.integer(round(d2))) else private$.fmtStatVal(d2, opts$k)
            } else {
                NULL
            }
            if (!is.null(df1)) {
                d1_num <- as.numeric(row1$df[1])
                if (abs(d1_num - round(d1_num)) < 1e-8) df1 <- as.character(as.integer(round(d1_num)))
            }

            stat_label <- switch(opts$typestatistics,
                "parametric" = if (n_lev == 2L) paste0("t(", df2, ")") else paste0("F(", df1, ", ", df2, ")"),
                "nonparametric" = if (n_lev == 2L) "W" else paste0("\u03c7\u00b2(", df2, ")"),
                "robust" = if (n_lev == 2L) paste0("t_Yuen(", df2, ")") else paste0("F_trimmed(", df1, ", ", df2, ")"),
                "stat"
            )

            eff_label <- switch(opts$typestatistics,
                "parametric" = if (n_lev == 2L) {
                    if (opts$effsizetype %in% c("unbiased", "omega")) "Hedges' g" else "Cohen's d"
                } else {
                    if (opts$effsizetype %in% c("unbiased", "omega")) "partial \u03c9\u00b2" else "partial \u03b7\u00b2"
                },
                "nonparametric" = if (n_lev == 2L) "rank-biserial r" else "\u03b5\u00b2",
                "robust" = if (n_lev == 2L) "robust d_AKP" else "explanatory \u03be",
                "effect size"
            )

            ci_lo_str <- if (is.finite(ci_lo)) private$.fmtStatVal(ci_lo, opts$k) else as.character(ci_lo)
            ci_hi_str <- if (is.finite(ci_hi)) private$.fmtStatVal(ci_hi, opts$k) else as.character(ci_hi)
            p_str <- private$.fmtStatVal(pval, opts$k)

            if (pval < 0.05) {
                .fmt(
                    .("{method} showed a statistically significant difference in {var} across the {nGroups} groups ({statLabel} = {statVal}, p = {pVal}, {effLabel} = {effVal}, {ci}% CI: {ciLo} to {ciHi}, n = {nObs})."),
                    method = method_lbl, var = var_esc, nGroups = n_lev,
                    statLabel = stat_label, statVal = private$.fmtStatVal(stat_val, opts$k),
                    pVal = p_str, effLabel = eff_label, effVal = private$.fmtStatVal(est_val, opts$k),
                    ci = ci_pct, ciLo = ci_lo_str, ciHi = ci_hi_str, nObs = n_obs
                )
            } else {
                .fmt(
                    .("{method} showed no statistically significant difference in {var} across the {nGroups} groups ({statLabel} = {statVal}, p = {pVal}, {effLabel} = {effVal}, {ci}% CI: {ciLo} to {ciHi}, n = {nObs})."),
                    method = method_lbl, var = var_esc, nGroups = n_lev,
                    statLabel = stat_label, statVal = private$.fmtStatVal(stat_val, opts$k),
                    pVal = p_str, effLabel = eff_label, effVal = private$.fmtStatVal(est_val, opts$k),
                    ci = ci_pct, ciLo = ci_lo_str, ciHi = ci_hi_str, nObs = n_obs
                )
            }
        },

        .generateCopyReadyReport = function() {
            deps <- private$.depVars()
            if (length(deps) == 0L || is.null(self$options$group))
                return()

            mydata <- private$.prepareData()
            opts <- private$.prepareOptions()
            n_per_dep <- vapply(deps, function(v) nrow(private$.subsetVar(mydata, v, FALSE)), integer(1))
            n_total_str <- if (length(unique(n_per_dep)) <= 1L) {
                as.character(n_per_dep[1])
            } else {
                paste0(min(n_per_dep), "\u2013", max(n_per_dep))
            }
            n_groups <- max(vapply(deps, function(v) {
                private$.nGroupLevels(private$.subsetVar(mydata, v, FALSE), self$options$group)
            }, integer(1)), na.rm = TRUE)

            dep_vars <- htmltools::htmlEscape(paste(deps, collapse = ", "))
            multi_var_note <- if (length(deps) == 1L) {
                ""
            } else {
                paste0(
                    "<p><strong>", .("Note:"), "</strong> ",
                    .fmt(.("{k} dependent variables were analyzed separately using variable-specific complete observations."),
                         k = length(deps)),
                    "</p>"
                )
            }

            test_method <- private$.testLabel(n_groups)
            stats_source <- if (isTRUE(self$options$resultssubtitle)) {
                .("the plot subtitle")
            } else {
                .("the plot subtitle (enable 'Statistical results' to display it)")
            }

            is_bayes <- identical(self$options$typestatistics, "bayes")

            methods_tail <- if (is_bayes && n_groups <= 2L) {
                .fmt(
                    .("Posterior difference estimates are reported with {ci}% equal-tailed credible intervals (ETI) and natural-log Bayes factors (log_e BF_01)."),
                    ci = round(100 * self$options$conflevel)
                )
            } else if (is_bayes) {
                .fmt(
                    .("Posterior Bayesian R\u00b2 estimates are reported with {ci}% highest density credible intervals (HDI) and natural-log Bayes factors (log_e BF_01)."),
                    ci = round(100 * self$options$conflevel)
                )
            } else {
                .fmt(
                    .("Effect sizes are reported with {ci}% confidence intervals; two-sided statistical significance was evaluated at \u03b1 = 0.05."),
                    ci = round(100 * self$options$conflevel)
                )
            }

            auto_lines <- character()
            if (isTRUE(self$options$resultssubtitle)) {
                for (v in deps) {
                    sub_v <- private$.subsetVar(mydata, v, include_grvar = FALSE)
                    line_v <- private$.formatAutoFilledReportLine(v, sub_v, opts)
                    if (!is.null(line_v) && nzchar(line_v)) {
                        auto_lines <- c(auto_lines, paste0("<p>", line_v, "</p>"))
                    }
                }
            }

            results_fill <- if (length(auto_lines) > 0L) {
                paste(auto_lines, collapse = "")
            } else if (is_bayes && n_groups <= 2L) {
                paste0(
                    "<p>", .fmt(.("Insert Bayesian summary from {src}: log_e(BF_01), posterior difference, and credible interval."), src = stats_source), "</p>",
                    "<p>", .fmt(
                        .("Template: \"{method} comparing (dependent variable) across {nGroups} groups yielded log_e(BF_01) = (value), posterior difference = (value) ({ci}% ETI: lower to upper).\""),
                        method = htmltools::htmlEscape(test_method),
                        nGroups = n_groups,
                        ci = round(100 * self$options$conflevel)
                    ), "</p>"
                )
            } else if (is_bayes) {
                paste0(
                    "<p>", .fmt(.("Insert Bayesian summary from {src}: log_e(BF_01), posterior Bayesian R\u00b2, and credible interval."), src = stats_source), "</p>",
                    "<p>", .fmt(
                        .("Template: \"{method} comparing (dependent variable) across {nGroups} groups yielded log_e(BF_01) = (value), posterior Bayesian R\u00b2 = (value) ({ci}% HDI: lower to upper).\""),
                        method = htmltools::htmlEscape(test_method),
                        nGroups = n_groups,
                        ci = round(100 * self$options$conflevel)
                    ), "</p>"
                )
            } else {
                paste0(
                    "<p>", .fmt(.("Insert test statistics from {src}: statistic, degrees of freedom, p-value, and effect size with CI."), src = stats_source), "</p>",
                    "<p>", .fmt(
                        .("Template: \"{method} showed (a / no) statistically significant difference in (dependent variable) across the {nGroups} groups (statistic = value, p = value, effect size = value, {ci}% CI: lower to upper).\""),
                        method = htmltools::htmlEscape(test_method),
                        nGroups = n_groups,
                        ci = round(100 * self$options$conflevel)
                    ), "</p>"
                )
            }

            posthoc_clause <- if (self$options$pairwisecomparisons && n_groups > 2) {
                if (identical(self$options$typestatistics, "parametric") && !identical(self$options$padjustmethod, "none")) {
                    paste0(.("Post-hoc pairwise comparisons used the Games-Howell studentized-range procedure."), " ")
                } else {
                    paste0(
                        .fmt(
                            .("Post-hoc pairwise comparisons used {adj} adjustment."),
                            adj = htmltools::htmlEscape(self$options$padjustmethod)
                        ),
                        " "
                    )
                }
            } else {
                ""
            }

            methods_lead <- .fmt(
                .("A between-groups analysis was conducted to compare {deps} across {nGroups} independent groups of {group} (n = {nTotal}) using {method}."),
                deps = dep_vars,
                nGroups = n_groups,
                group = htmltools::htmlEscape(self$options$group),
                nTotal = n_total_str,
                method = htmltools::htmlEscape(test_method)
            )

            report_template <- paste0(
                "<div style='padding: 15px; background-color: rgba(138, 155, 172, 0.06); border: 1px solid #dee2e6; margin: 10px 0; color: inherit;'>",
                "<h4 style='margin-top: 0;'>", .("Copy-Ready Report Template"), "</h4>",
                "<div style='background-color: rgba(255, 255, 255, 0.06); padding: 15px; border: 1px dashed #6c757d; margin: 10px 0 0 0; color: inherit; user-select: all;'>",
                "<h5>", .("Methods:"), "</h5>",
                "<p>", methods_lead, " ",
                if (isTRUE(private$.bayesNoStatistic)) {
                    paste0(.("NOTE: this test could not be computed for the present data - choose a different statistical approach before reporting."), " ")
                } else {
                    ""
                },
                posthoc_clause,
                methods_tail,
                "</p>",
                multi_var_note,
                "<h5>", .("Results:"), "</h5>",
                results_fill,
                "<h5>", .("Conclusion:"), "</h5>",
                "<p style='margin-bottom: 0;'>", .("[Interpret findings in clinical context, weighing both effect magnitude and precision.]"), "</p>",
                "</div>",
                "</div>"
            )

            self$results$report$setContent(report_template)
        },

        .plot = function(image, ggtheme, theme, ...) {
            seed_val <- private$.safeSeed()
            withr::local_seed(seed_val)

            if (!private$.validateInputs())
                return(FALSE)

            private$.checkpoint()

            mydata <- private$.prepareData()
            opts <- private$.prepareOptions()

            dep <- private$.depVars()
            group <- self$options$group

            # Single dependent variable analysis ----
            if (length(dep) == 1) {
                private$.checkpoint()
                sub_data <- private$.subsetVar(mydata, dep[1], include_grvar = FALSE)
                n_lev <- private$.nGroupLevels(sub_data, group)

                args_list <- list(
                    data = sub_data,
                    x = rlang::sym(group),
                    y = rlang::sym(dep[1]),
                    title = opts$mytitle,
                    xlab = opts$xtitle,
                    ylab = opts$ytitle,
                    type = opts$typestatistics,
                    pairwise.display = private$.pairwiseDisplay(opts),
                    p.adjust.method = opts$padjustmethod,
                    effsize.type = opts$effsizetype,
                    bf.message = opts$bfmessage,
                    digits = opts$k,
                    conf.level = opts$conflevel,
                    point.args = opts$pointargs,
                    results.subtitle = opts$resultssubtitle,
                    centrality.plotting = if (!is.null(opts$centrality.plotting)) opts$centrality.plotting else FALSE,
                    centrality.type = if (!is.null(opts$centrality.type)) opts$centrality.type else NULL
                )

                if (isTRUE(opts$centrality.plotting)) {
                    args_list$centrality.point.args <- opts$centrality.point.args
                    args_list$centrality.label.args <- opts$centrality.label.args
                }
                if (!is.null(opts$violinargs))  args_list$violin.args  <- opts$violinargs
                if (!is.null(opts$boxplotargs)) args_list$boxplot.args <- opts$boxplotargs

                # Prevent Games-Howell double p.adjust on K >= 3 parametric comparisons
                if (isTRUE(opts$pairwisecomparisons) && identical(opts$typestatistics, "parametric") &&
                    !is.na(n_lev) && n_lev >= 3L) {
                    args_list$p.adjust.method <- "none"
                    if (identical(opts$padjustmethod, "none")) {
                        args_list$pairwise.display <- "everything"
                    }
                }

                sub_expr <- private$.subtitleExpr(sub_data, group, dep[1], opts)
                args_list <- private$.sanitizePlotArgs(
                    args_list, sub_data, group, dep[1], opts, sub_expr = sub_expr
                )

                withr::local_seed(seed_val)
                plot <- private$.tryPlot(withBaseFormulaChar(do.call(ggstatsplot::ggbetweenstats, args_list)))
                plot <- private$.fixParametricPairwise(plot, sub_data, group, dep[1], opts)

                if (!is.null(sub_expr) && !is.null(plot))
                    plot <- plot + ggplot2::labs(subtitle = sub_expr)

                bf_expr <- private$.bfCaptionExpr(sub_data, group, dep[1], opts)
                if (!is.null(bf_expr) && !is.null(plot))
                    plot <- plot + ggplot2::labs(caption = bf_expr)

                plot <- private$.applyTheme(plot, opts, ggtheme)
                .k <- n_lev
            }

            # Multiple dependent variables analysis ----
            if (length(dep) > 1) {
                private$.checkpoint()

                dep2_symbols <- purrr::map(as.list(dep), rlang::sym)

                private$.checkpoint(flush = FALSE)
                plotlist <- purrr::pmap(
                    .l = list(
                        y = dep2_symbols,
                        messages = FALSE
                    ),
                    .f = function(y, messages) {
                        # Independent per-variable RNG stream so dep[2] never depends on dep[1]
                        withr::local_seed(seed_val)
                        y_name <- rlang::as_string(y)
                        sub_data <- private$.subsetVar(mydata, y_name, include_grvar = FALSE)
                        n_lev_v <- private$.nGroupLevels(sub_data, group)

                        plot_args <- list(
                            data = sub_data,
                            x = rlang::sym(group),
                            y = y,
                            messages = messages,
                            title = if (!is.null(opts$mytitle)) paste0(opts$mytitle, ": ", y_name) else y_name,
                            xlab = opts$xtitle,
                            ylab = if (!is.null(opts$ytitle)) opts$ytitle else y_name,
                            type = opts$typestatistics,
                            pairwise.display = private$.pairwiseDisplay(opts),
                            p.adjust.method = opts$padjustmethod,
                            effsize.type = opts$effsizetype,
                            bf.message = opts$bfmessage,
                            digits = opts$k,
                            conf.level = opts$conflevel,
                            point.args = opts$pointargs,
                            results.subtitle = opts$resultssubtitle,
                            centrality.plotting = if (!is.null(opts$centrality.plotting)) opts$centrality.plotting else FALSE,
                            centrality.type = if (!is.null(opts$centrality.type)) opts$centrality.type else NULL
                        )

                        if (isTRUE(opts$centrality.plotting)) {
                            plot_args$centrality.point.args <- opts$centrality.point.args
                            plot_args$centrality.label.args <- opts$centrality.label.args
                        }
                        if (!is.null(opts$violinargs))  plot_args$violin.args  <- opts$violinargs
                        if (!is.null(opts$boxplotargs)) plot_args$boxplot.args <- opts$boxplotargs

                        if (isTRUE(opts$pairwisecomparisons) && identical(opts$typestatistics, "parametric") &&
                            !is.na(n_lev_v) && n_lev_v >= 3L) {
                            plot_args$p.adjust.method <- "none"
                            if (identical(opts$padjustmethod, "none")) {
                                plot_args$pairwise.display <- "everything"
                            }
                        }

                        sub_expr <- private$.subtitleExpr(sub_data, group, y_name, opts)
                        plot_args <- private$.sanitizePlotArgs(
                            plot_args, sub_data, group, y_name, opts, sub_expr = sub_expr
                        )

                        withr::local_seed(seed_val)
                        pp <- private$.tryPlot(withBaseFormulaChar(do.call(ggstatsplot::ggbetweenstats, plot_args)))
                        pp <- private$.fixParametricPairwise(pp, sub_data, group, y_name, opts)

                        if (!is.null(sub_expr) && !is.null(pp))
                            pp <- pp + ggplot2::labs(subtitle = sub_expr)
                        bf_expr <- private$.bfCaptionExpr(sub_data, group, y_name, opts)
                        if (!is.null(bf_expr) && !is.null(pp))
                            pp <- pp + ggplot2::labs(caption = bf_expr)
                        pp
                    }
                )

                private$.checkpoint(flush = FALSE)
                plotlist <- lapply(plotlist, function(p) private$.applyTheme(p, opts, ggtheme))

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
                .k <- max(vapply(dep, function(v) {
                    private$.nGroupLevels(private$.subsetVar(mydata, v, FALSE), group)
                }, integer(1)), na.rm = TRUE)
            }

            if ((isTRUE(opts$resultssubtitle) && statsSeedMatters(opts$typestatistics, "between", .k)) ||
                captionSeedMatters(opts$typestatistics, "between", .k, opts$bfmessage))
                plot <- addSeedCaption(plot, self, seed_val)

            print(plot)
            TRUE
        },

        .plot2 = function(image, ggtheme, theme, ...) {
            seed_val <- private$.safeSeed()
            withr::local_seed(seed_val)

            if (!private$.validateInputs() || is.null(self$options$grvar))
                return(FALSE)

            private$.checkpoint()

            mydata <- private$.prepareData()
            opts <- private$.prepareOptions()

            dep <- private$.depVars()
            group <- self$options$group
            grvar <- self$options$grvar

            cb_component <- if (isTRUE(opts$colorblindSafe)) {
                list(
                    ggplot2::scale_fill_viridis_d(option = "D"),
                    ggplot2::scale_color_viridis_d(option = "D")
                )
            } else {
                NULL
            }

            # Single dependent variable grouped analysis ----
            if (length(dep) == 1) {
                private$.checkpoint()
                sub_data <- private$.subsetStratifiedVar(mydata, dep[1], reject_if_empty = TRUE)

                selected_theme <- if (!opts$originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()
                num_levels <- max(1L, nlevels(droplevels(private$.cleanFactor(sub_data[[grvar]]))))
                ncol_layout <- ceiling(sqrt(num_levels))
                n_lev <- private$.nGroupLevels(sub_data, group)

                ann_args <- list(tag_levels = "A")
                if (!is.null(opts$mytitle)) ann_args$title <- opts$mytitle

                grouped_args <- list(
                    data = sub_data,
                    x = rlang::sym(group),
                    y = rlang::sym(dep[1]),
                    grouping.var = rlang::sym(grvar),
                    xlab = opts$xtitle,
                    ylab = opts$ytitle,
                    type = opts$typestatistics,
                    pairwise.display = private$.pairwiseDisplay(opts),
                    p.adjust.method = opts$padjustmethod,
                    effsize.type = opts$effsizetype,
                    bf.message = opts$bfmessage,
                    digits = opts$k,
                    conf.level = opts$conflevel,
                    point.args = opts$pointargs,
                    results.subtitle = opts$resultssubtitle,
                    centrality.plotting = if (!is.null(opts$centrality.plotting)) opts$centrality.plotting else FALSE,
                    centrality.type = if (!is.null(opts$centrality.type)) opts$centrality.type else NULL,
                    ggtheme = selected_theme,
                    ggplot.component = cb_component,
                    plotgrid.args = list(ncol = ncol_layout),
                    annotation.args = ann_args
                )

                if (isTRUE(opts$centrality.plotting)) {
                    grouped_args$centrality.point.args <- opts$centrality.point.args
                    grouped_args$centrality.label.args <- opts$centrality.label.args
                }
                if (!is.null(opts$violinargs))  grouped_args$violin.args  <- opts$violinargs
                if (!is.null(opts$boxplotargs)) grouped_args$boxplot.args <- opts$boxplotargs

                if (isTRUE(opts$pairwisecomparisons) && identical(opts$typestatistics, "parametric") &&
                    !is.na(n_lev) && n_lev >= 3L) {
                    grouped_args$p.adjust.method <- "none"
                    if (identical(opts$padjustmethod, "none")) {
                        grouped_args$pairwise.display <- "everything"
                    }
                }

                grouped_args <- private$.sanitizePlotArgs(
                    grouped_args, sub_data, group, dep[1], opts, grvar = grvar
                )

                withr::local_seed(seed_val)
                plot2 <- private$.tryPlot(withBaseFormulaChar(do.call(ggstatsplot::grouped_ggbetweenstats, grouped_args)))
                plot2 <- private$.fixGroupedParametricPairwise(plot2, group, dep[1], opts)
                .k <- n_lev
            }

            # Multiple dependent variables grouped analysis ----
            if (length(dep) > 1) {
                private$.checkpoint()

                selected_theme <- if (!opts$originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()
                dep2_symbols <- purrr::map(as.list(dep), rlang::sym)

                private$.checkpoint(flush = FALSE)
                plotlist <- purrr::pmap(
                    .l = list(
                        y = dep2_symbols,
                        messages = FALSE
                    ),
                    .f = function(y, messages) {
                        withr::local_seed(seed_val)
                        y_name <- rlang::as_string(y)
                        sub_data <- private$.subsetStratifiedVar(mydata, y_name, reject_if_empty = TRUE)
                        num_levels <- max(1L, nlevels(droplevels(private$.cleanFactor(sub_data[[grvar]]))))
                        ncol_layout <- ceiling(sqrt(num_levels))
                        n_lev_v <- private$.nGroupLevels(sub_data, group)

                        ann_args <- list(
                            tag_levels = "A",
                            title = if (!is.null(opts$mytitle)) paste0(opts$mytitle, ": ", y_name) else y_name
                        )

                        grouped_multi_args <- list(
                            data = sub_data,
                            x = rlang::sym(group),
                            y = y,
                            grouping.var = rlang::sym(grvar),
                            messages = messages,
                            xlab = opts$xtitle,
                            ylab = if (!is.null(opts$ytitle)) opts$ytitle else y_name,
                            type = opts$typestatistics,
                            pairwise.display = private$.pairwiseDisplay(opts),
                            p.adjust.method = opts$padjustmethod,
                            effsize.type = opts$effsizetype,
                            bf.message = opts$bfmessage,
                            digits = opts$k,
                            conf.level = opts$conflevel,
                            point.args = opts$pointargs,
                            results.subtitle = opts$resultssubtitle,
                            centrality.plotting = if (!is.null(opts$centrality.plotting)) opts$centrality.plotting else FALSE,
                            centrality.type = if (!is.null(opts$centrality.type)) opts$centrality.type else NULL,
                            ggtheme = selected_theme,
                            ggplot.component = cb_component,
                            plotgrid.args = list(ncol = ncol_layout),
                            annotation.args = ann_args
                        )

                        if (isTRUE(opts$centrality.plotting)) {
                            grouped_multi_args$centrality.point.args <- opts$centrality.point.args
                            grouped_multi_args$centrality.label.args <- opts$centrality.label.args
                        }
                        if (!is.null(opts$violinargs))  grouped_multi_args$violin.args  <- opts$violinargs
                        if (!is.null(opts$boxplotargs)) grouped_multi_args$boxplot.args <- opts$boxplotargs

                        if (isTRUE(opts$pairwisecomparisons) && identical(opts$typestatistics, "parametric") &&
                            !is.na(n_lev_v) && n_lev_v >= 3L) {
                            grouped_multi_args$p.adjust.method <- "none"
                            if (identical(opts$padjustmethod, "none")) {
                                grouped_multi_args$pairwise.display <- "everything"
                            }
                        }

                        grouped_multi_args <- private$.sanitizePlotArgs(
                            grouped_multi_args, sub_data, group, y_name, opts, grvar = grvar
                        )

                        gp <- private$.tryPlot(withBaseFormulaChar(do.call(ggstatsplot::grouped_ggbetweenstats, grouped_multi_args)))
                        private$.fixGroupedParametricPairwise(gp, group, y_name, opts)
                    }
                )

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
                .k <- max(vapply(dep, function(v) {
                    private$.nGroupLevels(private$.subsetStratifiedVar(mydata, v, reject_if_empty = FALSE), group)
                }, integer(1)), na.rm = TRUE)
            }

            if ((isTRUE(opts$resultssubtitle) && statsSeedMatters(opts$typestatistics, "between", .k)) ||
                captionSeedMatters(opts$typestatistics, "between", .k, opts$bfmessage))
                plot2 <- addSeedCaption(plot2, self, seed_val)
            print(plot2)
            TRUE
        },

        .plotGGPubr = function(image, ggtheme, theme, ...) {
            seed_val <- private$.safeSeed()
            withr::local_seed(seed_val)

            if (!private$.validateInputs() || !self$options$addGGPubrPlot)
                return(FALSE)

            private$.checkpoint()

            mydata <- private$.prepareData()
            dep <- private$.depVars()

            if (length(dep) == 1) {
                sub_data <- private$.subsetVar(mydata, dep[1], include_grvar = FALSE)
                plot <- private$.buildOneGGPubrPanel(sub_data, dep[1], theme = theme, multi = FALSE, facet_var = NULL)
                private$.tryPlot(print(plot))
            } else {
                plotlist <- lapply(as.list(dep), function(depvar) {
                    withr::local_seed(seed_val)
                    sub_data <- private$.subsetVar(mydata, depvar, include_grvar = FALSE)
                    private$.buildOneGGPubrPanel(sub_data, depvar, theme = theme, multi = TRUE, facet_var = NULL)
                })
                plot <- ggpubr::ggarrange(plotlist = plotlist, ncol = 1, nrow = length(dep))
                private$.tryPlot(print(plot))
            }

            TRUE
        },

        .plotGGPubr2 = function(image, ggtheme, theme, ...) {
            seed_val <- private$.safeSeed()
            withr::local_seed(seed_val)

            if (!private$.validateInputs() || !self$options$addGGPubrPlot || is.null(self$options$grvar))
                return(FALSE)

            private$.checkpoint()

            mydata <- private$.prepareData()
            dep <- private$.depVars()
            grvar <- self$options$grvar

            if (length(dep) == 1) {
                sub_data <- private$.subsetStratifiedVar(mydata, dep[1], reject_if_empty = TRUE)
                plot <- private$.buildOneGGPubrPanel(sub_data, dep[1], theme = theme, multi = FALSE, facet_var = grvar)
                private$.tryPlot(print(plot))
            } else {
                plotlist <- lapply(as.list(dep), function(depvar) {
                    withr::local_seed(seed_val)
                    sub_data <- private$.subsetStratifiedVar(mydata, depvar, reject_if_empty = TRUE)
                    private$.buildOneGGPubrPanel(sub_data, depvar, theme = theme, multi = TRUE, facet_var = grvar)
                })
                plot <- ggpubr::ggarrange(plotlist = plotlist, ncol = 1, nrow = length(dep))
                private$.tryPlot(print(plot))
            }

            TRUE
        }
    ),
    public = list(
        #' @description
        #' Generate R source code for Between-Groups Statistics analysis with safe
        #' quoting for non-syntactic, spaced, or Unicode variable names.
        #' @return Character string with parseable R syntax for reproducible analysis
        asSource = function() {
            defaults <- list(
                grvar = NULL,
                centralityplotting = FALSE,
                centralitytype = "parametric",
                typestatistics = "parametric",
                seed = 20250101L,
                pairwisecomparisons = FALSE,
                pairwisedisplay = "significant",
                padjustmethod = "holm",
                effsizetype = "biased",
                mytitle = "Between Group Comparison",
                xtitle = "",
                ytitle = "",
                originaltheme = FALSE,
                resultssubtitle = FALSE,
                bfmessage = FALSE,
                k = 2L,
                conflevel = 0.95,
                varequal = FALSE,
                multiEndpointCorrection = "none",
                plotwidth = 650L,
                plotheight = 450L,
                colorblindSafe = FALSE,
                showexplanations = FALSE,
                addGGPubrPlot = FALSE,
                ggpubrPlotType = "boxplot",
                ggpubrPalette = "jco",
                ggpubrAddStats = FALSE,
                ggpubrAddPoints = FALSE
            )

            arg_lines <- c("data = data")
            deps <- private$.depVars()
            if (is.character(self$options$dep) && length(deps) > 0L) {
                arg_lines <- c(arg_lines, paste0("dep = ", private$.R_literal(deps)))
            }
            if (is.character(self$options$group) && length(self$options$group) == 1L) {
                arg_lines <- c(arg_lines, paste0("group = ", private$.R_literal(self$options$group)))
            }

            for (nm in names(defaults)) {
                val <- self$options[[nm]]
                def <- defaults[[nm]]
                if (is.null(def)) {
                    if (is.character(val) && length(val) == 1L && !is.na(val) && nzchar(val)) {
                        arg_lines <- c(arg_lines, paste0(nm, " = ", private$.R_literal(val)))
                    }
                    next
                }
                type_ok <- is.atomic(val) && length(val) == 1L && !is.na(val) && (
                    (is.logical(def) && is.logical(val)) ||
                    (is.numeric(def) && is.numeric(val)) ||
                    (is.character(def) && is.character(val))
                )
                if (type_ok && !isTRUE(all.equal(unname(val), def))) {
                    arg_lines <- c(arg_lines, paste0(nm, " = ", private$.R_literal(val)))
                }
            }

            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"

            paste0(pkg_name, "::jjbetweenstats(\n    ", paste(arg_lines, collapse = ",\n    "), "\n)")
        }
    )
)
