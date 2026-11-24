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


        # Option overrides for clinical presets (jamovi options are read-only at runtime;
        # presets record overrides here and reads go through private$.option()).
        overrides = list(),
        .statsComputed = FALSE,
        .option = function(option) {
            if (option %in% names(private$overrides)) return(private$overrides[[option]])
            opt_obj <- self$options$option(option)
            if (!is.null(opt_obj)) return(opt_obj$value)
            return(NULL)
        },

            # Clinical presets record their settings in private$overrides instead of writing
            # back into the read-only jamovi option objects; reads go through private$.option().
            # Only options still at their schema default are populated by a preset so explicit
            # user choices are never silently clobbered.
            .applyClinicalPreset = function() {
                preset <- self$options$clinicalPreset

                # Clear first so returning to Custom never leaks stale preset state.
                private$overrides <- list()

                # Only apply preset if not using custom settings
                if (preset == 'custom') return()

                # Schema defaults from jamovi/jjhistostats.a.yaml
                defaults <- list(
                    typestatistics = 'parametric',
                    centralityline = FALSE,
                    centralitytype = 'default',
                    resultssubtitle = FALSE,
                    showInterpretation = FALSE,
                    bf.message = FALSE,
                    changebinwidth = FALSE,
                    binwidth = 1.1
                )
                set_if_default <- function(opt_name, preset_val) {
                    opt_obj <- self$options$option(opt_name)
                    if (!is.null(opt_obj) && identical(opt_obj$value, defaults[[opt_name]])) {
                        private$overrides[[opt_name]] <- preset_val
                    }
                }

                # Apply preset-specific configurations (synchronized with jjhistostats.events.js)
                if (preset == 'lab_values') {
                    set_if_default("typestatistics", 'parametric')
                    set_if_default("centralityline", TRUE)
                    set_if_default("centralitytype", 'parametric')
                    set_if_default("resultssubtitle", TRUE)
                    set_if_default("showInterpretation", TRUE)
                    set_if_default("bf.message", TRUE)
                } else if (preset == 'biomarkers') {
                    set_if_default("typestatistics", 'nonparametric')
                    set_if_default("centralityline", TRUE)
                    set_if_default("centralitytype", 'nonparametric')
                    set_if_default("resultssubtitle", TRUE)
                    set_if_default("showInterpretation", TRUE)
                    set_if_default("bf.message", FALSE)
                } else if (preset == 'patient_chars') {
                    set_if_default("typestatistics", 'parametric')
                    set_if_default("centralityline", TRUE)
                    set_if_default("centralitytype", 'parametric')
                    set_if_default("resultssubtitle", TRUE)
                    set_if_default("showInterpretation", FALSE)
                    set_if_default("bf.message", FALSE)
                } else if (preset == 'pathology_scores') {
                    set_if_default("typestatistics", 'nonparametric')
                    set_if_default("centralityline", TRUE)
                    set_if_default("centralitytype", 'nonparametric')
                    set_if_default("resultssubtitle", TRUE)
                    set_if_default("showInterpretation", TRUE)
                    set_if_default("bf.message", FALSE)
                    set_if_default("changebinwidth", TRUE)
                    set_if_default("binwidth", 1.0)
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

                # Scale diagnostic plots vertically when multiple variables are stacked
                if (isTRUE(self$options$addDistributionDiagnostics) && deplen > 1) {
                    diag_height <- deplen * 400 * 1.15
                    self$results$densityPlot$setSize(600, diag_height)
                    self$results$qqPlot$setSize(600, diag_height)
                    self$results$ecdfPlot$setSize(600, diag_height)
                }


                if (!is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                gr_chr <- as.character(mydata[[grvar]])
                num_levels <- max(1L, length(unique(gr_chr[!is.na(gr_chr)])))

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

            # Strip discrete scales (jmvcore::ggPalette) from jamovi's ggtheme list.
            # ggstatsplot::gghistostats() maps fill = after_stat(count) (continuous),
            # so adding a discrete fill scale aborts ggplot_build() on every default render.
            .cleanGgtheme = function(ggtheme) {
                if (isTRUE(self$options$originaltheme)) {
                    return(ggstatsplot::theme_ggstatsplot())
                }
                if (is.null(ggtheme)) return(NULL)
                if (inherits(ggtheme, "theme")) return(ggtheme)
                if (is.list(ggtheme)) {
                    theme_els <- Filter(function(el) inherits(el, "theme"), ggtheme)
                    if (length(theme_els) > 0) return(theme_els[[1]])
                    return(ggplot2::theme_minimal())
                }
                ggtheme
            },

            .formatStatNum = function(val, digits) {
                if (length(val) == 0 || is.na(val)) return("NA")
                if (is.infinite(val)) return(if (val > 0) "Inf" else "-Inf")
                sprintf(paste0("%.", digits, "f"), val)
            },

            .formatStatP = function(p, digits) {
                if (length(p) == 0 || is.na(p)) return("NA")
                if (p < 10^(-digits)) {
                    formatC(p, digits = digits, format = "e")
                } else {
                    sprintf(paste0("%.", digits, "f"), p)
                }
            },

            .formatDescNum = function(val) {
                if (length(val) == 0 || is.na(val) || !is.finite(val)) return("NA")
                if (val == 0) return("0")
                if (abs(val) < 0.01 || abs(val) >= 1e6) {
                    return(formatC(val, digits = 3, format = "g"))
                }
                as.character(round(val, 2))
            },

            .formatConfPct = function(conf_level) {
                paste0(sub("\\.?0+$", "", sprintf("%.4f", 100 * conf_level)), "%")
            },

            .formatPWithOp = function(p) {
                p_str <- base::format.pval(p, digits = 3, eps = 1e-16)
                if (startsWith(p_str, "<")) {
                    paste0("p &lt; ", trimws(sub("^<", "", p_str)))
                } else {
                    paste0("p = ", p_str)
                }
            },

            # Genuine one-sided bootstrap-t on trimmed mean (Phipson-Smyth (b+1)/(B+1), B=2000)
            .buildOneSidedRobustExpr = function(x_vec, test_value, tr, conf_level, alternative, digits) {
                n_obs <- length(x_vec)
                mu_tr <- mean(x_vec, trim = tr)
                se_tr <- WRS2::trimse(x_vec, tr)
                if (!is.finite(se_tr) || se_tr <= 0) return(NULL)
                t_obs <- (mu_tr - test_value) / se_tr
                nboot <- 2000L
                boot_mat <- matrix(sample(x_vec, size = n_obs * nboot, replace = TRUE), nrow = nboot) - mu_tr
                top <- apply(boot_mat, 1, mean, trim = tr)
                bot <- apply(boot_mat, 1, WRS2::trimse, tr = tr)
                t_boot <- top / bot
                t_boot <- t_boot[is.finite(t_boot)]
                B_eff <- length(t_boot)
                if (B_eff < 10L) return(NULL)
                if (identical(alternative, "greater")) {
                    p_val <- (sum(t_boot >= t_obs) + 1) / (B_eff + 1)
                    ci_lo <- mu_tr - as.numeric(stats::quantile(t_boot, probs = conf_level, names = FALSE)) * se_tr
                    ci_hi <- Inf
                } else {
                    p_val <- (sum(t_boot <= t_obs) + 1) / (B_eff + 1)
                    ci_lo <- -Inf
                    ci_hi <- mu_tr - as.numeric(stats::quantile(t_boot, probs = 1 - conf_level, names = FALSE)) * se_tr
                }
                ci_lbl <- private$.formatConfPct(conf_level)
                bquote(list(
                    italic("t")["bootstrapped"] == .(private$.formatStatNum(t_obs, digits)),
                    italic(p) == .(private$.formatStatP(p_val, digits)),
                    widehat(mu)["trimmed"] == .(private$.formatStatNum(mu_tr, digits)),
                    CI[.(ci_lbl)] ~ "[" * .(private$.formatStatNum(ci_lo, digits)),
                    .(private$.formatStatNum(ci_hi, digits)) * "]",
                    italic("n")["obs"] == .(as.character(n_obs))
                ))
            },

            # Genuine directional JZS Bayes factor via BayesFactor::ttestBF(nullInterval = ...).
            # Reads log(BF10) directly from @bayesFactor$bf[1] (no exp() overflow above 709.78),
            # and summarizes posterior difference from the unconstrained JZS model so opposite-sign
            # data never collapse to NA/Inf rejection draws or display truncated positive estimates.
            .buildDirectionalBayesExpr = function(x_vec, test_value, bf_prior, conf_level, alternative, digits) {
                null_int <- if (identical(alternative, "greater")) c(0, Inf) else c(-Inf, 0)
                bf_dir <- tryCatch(
                    BayesFactor::ttestBF(x = x_vec, mu = test_value, rscale = bf_prior, nullInterval = null_int),
                    error = function(e) NULL
                )
                if (is.null(bf_dir)) return(NULL)
                log_bf10 <- tryCatch(as.numeric(bf_dir@bayesFactor$bf[1]), error = function(e) NA_real_)
                if (!is.finite(log_bf10)) return(NULL)
                log_e_bf01 <- -log_bf10

                bf_unc <- tryCatch(
                    BayesFactor::ttestBF(x = x_vec, mu = test_value, rscale = bf_prior),
                    error = function(e) NULL
                )
                if (is.null(bf_unc)) return(NULL)
                post <- tryCatch(
                    BayesFactor::posterior(bf_unc, iterations = 4000, progress = FALSE),
                    error = function(e) NULL
                )
                if (is.null(post) || !"mu" %in% colnames(post)) return(NULL)
                diff_draws <- as.numeric(post[, "mu"]) - test_value
                diff_draws <- diff_draws[is.finite(diff_draws)]
                if (length(diff_draws) < 10L) return(NULL)
                est <- stats::median(diff_draws)
                alpha <- 1 - conf_level
                qs <- stats::quantile(diff_draws, probs = c(alpha / 2, 1 - alpha / 2), names = FALSE)
                ci_lbl <- private$.formatConfPct(conf_level)
                bquote(list(
                    log[e] * (BF["01"]) == .(private$.formatStatNum(log_e_bf01, digits)),
                    widehat(delta)["difference"]^"posterior" == .(private$.formatStatNum(est, digits)),
                    CI[.(ci_lbl)]^ETI ~ "[" * .(private$.formatStatNum(qs[1], digits)),
                    .(private$.formatStatNum(qs[2], digits)) * "]",
                    italic("r")["Cauchy"]^"JZS" == .(private$.formatStatNum(bf_prior, digits))
                ))
            },

            # Replace WRS2::trimcibt zero-exceedance .Machine$double.xmin (~2.225e-308 at any digits)
            # with the Phipson-Smyth (2010) bound p < 0.005 (1 / (200 + 1)).
            .patchRobustZeroP = function(expr) {
                if (is.null(expr) || !is.call(expr)) return(expr)
                for (i in seq_along(expr)) {
                    el <- expr[[i]]
                    if (is.call(el) && identical(el[[1]], as.name("==")) &&
                        identical(deparse(el[[2]]), "italic(p)") &&
                        is.character(el[[3]])) {
                        p_num <- suppressWarnings(as.numeric(el[[3]]))
                        if (!is.na(p_num) && p_num <= 1e-300) {
                            expr[[i]] <- call("<", el[[2]], "0.005")
                        }
                    } else if (is.call(el)) {
                        expr[[i]] <- private$.patchRobustZeroP(el)
                    }
                }
                expr
            },

            # Replace the two-sided CI bounds inside a parametric/nonparametric statsExpressions
            # subtitle with the genuine one-sided CI matching `alternative`.
            .patchOneSidedCI = function(expr, ci_lo_str, ci_hi_str) {
                if (is.null(expr) || !is.call(expr)) return(expr)
                # In statsExpressions, list(..., CI["95%"] ~ "[" * "lo", "hi" * "]", ...)
                for (i in seq_along(expr)) {
                    el <- expr[[i]]
                    if (is.call(el) && identical(el[[1]], as.name("~")) &&
                        grepl("^CI\\[", paste(deparse(el[[2]]), collapse = ""))) {
                        el[[3]] <- call("*", "[", ci_lo_str)
                        expr[[i]] <- el
                        if (i + 1L <= length(expr)) {
                            expr[[i + 1L]] <- call("*", ci_hi_str, "]")
                        }
                        break
                    }
                }
                expr
            },

            # Single-panel histogram generator shared by .plot and grouped .plot2
            .generateSingleHistogram = function(data, x_var, options_data, aesthetics_data, ggtheme = NULL, panel_title = NULL, forced_binwidth = NULL) {
                private$.checkpoint(flush = FALSE)

                x_fin <- data[[x_var]]
                x_fin <- x_fin[is.finite(x_fin)]
                test_val <- if (isTRUE(options_data$enableOneSampleTest)) options_data$test.value else 0
                stat_type <- options_data$typestatistics
                alt <- options_data$alternative
                tr <- options_data$trimlevel
                digits <- options_data$digits
                conf_level <- options_data$conf.level

                # Guard degenerate statistical paths (n < 3 subgroups, constant data,
                # insufficient untrimmed n, or zero Winsorized SE from heavy ties)
                too_few_obs <- length(x_fin) < 3L
                is_constant <- !too_few_obs && length(unique(x_fin)) <= 1L
                robust_too_small <- !too_few_obs && !is_constant && identical(stat_type, "robust") &&
                    (length(x_fin) - 2L * floor(tr * length(x_fin)) < 3L)
                robust_zero_se <- !too_few_obs && !is_constant && !robust_too_small &&
                    identical(stat_type, "robust") && {
                        se_chk <- tryCatch(WRS2::trimse(x_fin, tr), error = function(e) NA_real_)
                        !is.finite(se_chk) || se_chk <= 0
                    }

                effective_subtitle <- isTRUE(options_data$resultssubtitle) &&
                    !too_few_obs && !is_constant && !robust_too_small && !robust_zero_se
                custom_sub <- aesthetics_data$subtitle
                if (isTRUE(options_data$resultssubtitle) && too_few_obs) {
                    custom_sub <- if (length(x_fin) == 1L) {
                        .("No statistical test: subgroup has only 1 observation (need at least 3).")
                    } else {
                        .fmt(.("No statistical test: subgroup has only {n} observations (need at least 3)."),
                             n = length(x_fin))
                    }
                } else if (isTRUE(options_data$resultssubtitle) && is_constant) {
                    custom_sub <- .("No statistical test: variable is constant.")
                } else if (isTRUE(options_data$resultssubtitle) && robust_too_small) {
                    custom_sub <- .("Robust test unavailable: trimming leaves fewer than 3 observations.")
                } else if (isTRUE(options_data$resultssubtitle) && robust_zero_se) {
                    custom_sub <- .("Robust test unavailable: zero Winsorized variance in trimmed core (heavy ties).")
                }

                # For one-sided robust or bayes, compute directional stats ourselves after
                # building the histogram, because statsExpressions ignores `alternative` there.
                manual_one_sided <- effective_subtitle &&
                    !identical(alt, "two.sided") &&
                    stat_type %in% c("robust", "bayes")

                base_args <- list(
                    data = data,
                    x = rlang::sym(x_var),
                    type = stat_type,
                    results.subtitle = if (manual_one_sided) FALSE else effective_subtitle,
                    centrality.plotting = isTRUE(options_data$centralityline) && length(x_fin) >= 1,
                    binwidth = if (!is.null(forced_binwidth)) forced_binwidth else options_data$binwidth,
                    conf.level = conf_level,
                    alternative = alt,
                    tr = tr,
                    bf.prior = options_data$bfprior,
                    bf.message = isTRUE(options_data$bf.message) && effective_subtitle,
                    digits = digits,
                    xlab = if (length(options_data$dep) > 1) x_var else aesthetics_data$xlab,
                    title = if (!is.null(panel_title)) panel_title else if (length(options_data$dep) > 1) NULL else aesthetics_data$title,
                    subtitle = if (effective_subtitle && !manual_one_sided) NULL else custom_sub,
                    caption = aesthetics_data$caption,
                    bin.args = aesthetics_data$bin.args,
                    centrality.line.args = aesthetics_data$centrality.line.args
                )

                if (isTRUE(options_data$enableOneSampleTest)) {
                    base_args$test.value <- test_val
                }
                if (!is.null(options_data$centrality.type)) {
                    base_args$centrality.type <- options_data$centrality.type
                }
                base_args$ggtheme <- private$.cleanGgtheme(ggtheme)

                # Remove NULL arguments to prevent conflicts
                base_args <- base_args[!sapply(base_args, is.null)]

                private$.checkpoint(flush = FALSE)
                p <- do.call(ggstatsplot::gghistostats, base_args)

                # Post-process subtitles/captions for edge cases & one-sided accuracy
                if (effective_subtitle) {
                    stat_ran <- TRUE
                    if (manual_one_sided && identical(stat_type, "robust")) {
                        rob_expr <- private$.buildOneSidedRobustExpr(
                            x_fin, test_val, tr, conf_level, alt, digits
                        )
                        if (!is.null(rob_expr)) {
                            p <- p + ggplot2::labs(subtitle = rob_expr)
                        } else {
                            stat_ran <- FALSE
                            p <- p + ggplot2::labs(subtitle = .("Robust test unavailable: insufficient finite bootstrap resamples."))
                        }
                    } else if (manual_one_sided && identical(stat_type, "bayes")) {
                        bay_expr <- private$.buildDirectionalBayesExpr(
                            x_fin, test_val, options_data$bfprior, conf_level, alt, digits
                        )
                        if (!is.null(bay_expr)) {
                            p <- p + ggplot2::labs(subtitle = bay_expr)
                        } else {
                            stat_ran <- FALSE
                            p <- p + ggplot2::labs(subtitle = .("Bayesian directional test unavailable for these data."))
                        }
                    } else if (identical(stat_type, "robust")) {
                        # Two-sided robust: replace impossible B=200 p~2.225e-308 with p < "0.005"
                        p$labels$subtitle <- private$.patchRobustZeroP(p$labels$subtitle)
                    } else if (!identical(alt, "two.sided") && stat_type %in% c("parametric", "nonparametric")) {
                        # One-sided parametric/nonparametric: match effect-size CI to `alternative`
                        es_df <- tryCatch({
                            if (identical(stat_type, "parametric")) {
                                effectsize::hedges_g(x_fin, mu = test_val, ci = conf_level,
                                                     alternative = alt, verbose = FALSE)
                            } else {
                                effectsize::rank_biserial(x_fin, mu = test_val, ci = conf_level,
                                                          alternative = alt, verbose = FALSE)
                            }
                        }, error = function(e) NULL)
                        if (!is.null(es_df) && nrow(es_df) >= 1 && !is.null(p$labels$subtitle)) {
                            p$labels$subtitle <- private$.patchOneSidedCI(
                                p$labels$subtitle,
                                private$.formatStatNum(es_df$CI_low[1], digits),
                                private$.formatStatNum(es_df$CI_high[1], digits)
                            )
                        }
                        # Also match parametric bf.message caption to directional hypothesis
                        if (identical(stat_type, "parametric") && isTRUE(options_data$bf.message)) {
                            bf_cap <- private$.buildDirectionalBayesExpr(
                                x_fin, test_val, options_data$bfprior, conf_level, alt, digits
                            )
                            if (!is.null(bf_cap)) p <- p + ggplot2::labs(caption = bf_cap)
                        }
                    }
                    if (stat_ran) private$.statsComputed <- TRUE
                }

                p
            },

            # Shared plot generation function to eliminate duplication
            .generateHistogram = function(data, x_var, options_data, aesthetics_data, grvar_sym = NULL, ggtheme = NULL) {
                private$.checkpoint(flush = FALSE)

                if (is.null(grvar_sym)) {
                    return(private$.generateSingleHistogram(
                        data = data, x_var = x_var,
                        options_data = options_data, aesthetics_data = aesthetics_data,
                        ggtheme = ggtheme
                    ))
                }

                # Grouped histogram: mirror grouped_gghistostats() shared automatic binwidth
                # while safely handling explicit-NA factor levels (addNA) and all-NA subgroups.
                grvar_name <- rlang::as_name(grvar_sym)
                raw_g <- data[[grvar_name]]
                g_chr <- as.character(raw_g)
                valid_grp <- !is.na(g_chr)
                gdata <- data[valid_grp, , drop = FALSE]
                g_chr_valid <- g_chr[valid_grp]
                raw_lvls <- if (is.factor(raw_g)) levels(raw_g) else unique(g_chr_valid)
                lvls <- raw_lvls[!is.na(raw_lvls)]

                x_all <- gdata[[x_var]]
                x_all <- x_all[is.finite(x_all)]
                shared_bw <- options_data$binwidth
                if (is.null(shared_bw) && length(x_all) >= 2 && diff(range(x_all)) > 0) {
                    shared_bw <- (max(x_all) - min(x_all)) / sqrt(length(x_all))
                }

                plotlist <- list()
                for (lvl in lvls) {
                    sub_df <- gdata[g_chr_valid == lvl, , drop = FALSE]
                    if (nrow(sub_df) == 0) next
                    x_sub <- sub_df[[x_var]]
                    if (sum(is.finite(x_sub)) == 0) next
                    private$.checkpoint(flush = FALSE)
                    plotlist[[length(plotlist) + 1L]] <- private$.generateSingleHistogram(
                        data = sub_df, x_var = x_var,
                        options_data = options_data, aesthetics_data = aesthetics_data,
                        ggtheme = ggtheme, panel_title = lvl, forced_binwidth = shared_bw
                    )
                }

                if (length(plotlist) == 0) {
                    jmvcore::reject(.("No grouping level contains finite numeric values to plot."))
                }
                ggstatsplot::combine_plots(plotlist = plotlist)
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
                    return(list(valid = FALSE, message = .fmt(
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
                            .fmt(
                                .("Variable '{var}' has 1 finite numeric value. A histogram needs at least 3. Check the variable selection and any active row filters."),
                                var = v)
                        else
                            .fmt(
                                .("Variable '{var}' has {n} finite numeric values. A histogram needs at least 3. Check the variable selection and any active row filters."),
                                var = v, n = n_ok)))
                }

                # Check if grouping variable exists if specified
                if (!is.null(self$options$grvar) && !self$options$grvar %in% names(self$data)) {
                    return(list(valid = FALSE, message = .fmt(
                        .("The grouping variable '{var}' is not present in the data."),
                        var = self$options$grvar)))
                }

                # The same column in both boxes: jmvcore coerces grvar to a factor (its
                # `permitted` is factor), so the histogram's own x aesthetic becomes discrete
                # and stat_bin() dies with "requires a continuous x aesthetic".
                if (!is.null(self$options$grvar) && self$options$grvar %in% self$options$dep) {
                    return(list(valid = FALSE, message = .fmt(
                        .("'{var}' is used both as a histogram variable and as the Split by variable. Choose a different grouping variable."),
                        var = self$options$grvar)))
                }

                # Check effective binwidth (including clinicalPreset overrides such as pathology_scores)
                use_bw <- isTRUE(private$.option("changebinwidth"))
                bw_val <- private$.option("binwidth")

                # Check if binwidth is positive when manually or preset specified
                if (use_bw && (!is.null(bw_val) && bw_val <= 0)) {
                    return(list(valid = FALSE, message = .("Bin width must be a positive number.")))
                }

                # A positive but tiny bin width is fully enterable in the GUI (or via a preset
                # like pathology_scores on a wide-range variable) and produces an EMPTY panel:
                # ggplot2 refuses more than 1,000,000 bins, and that error goes to stderr.
                if (use_bw && !is.null(bw_val) && bw_val > 0) {
                    max_bins <- 5000
                    for (v in self$options$dep) {
                        if (!v %in% names(self$data)) next
                        x <- self$data[[v]]
                        if (!is.numeric(x)) x <- suppressWarnings(as.numeric(as.character(x)))
                        x <- x[is.finite(x)]
                        if (length(x) < 2) next
                        rng <- diff(range(x))
                        if (rng <= 0) next
                        n_bins <- rng / bw_val
                        if (n_bins > max_bins)
                            return(list(valid = FALSE, message = .fmt(
                                .("A bin width of {width} would split '{var}' into {bins} bins, so the histogram would be unreadable or blank. '{var}' ranges over {range}; a bin width of about {suggested} gives roughly 30 bins."),
                                width = base::format(bw_val), var = v,
                                bins = base::format(round(n_bins), big.mark = ","),
                                range = base::format(signif(rng, 4)),
                                suggested = base::format(signif(rng / 30, 3)))))
                    }
                }

                # Defense-in-depth option backstops (guards R callers using run(noThrow = TRUE))
                cl <- self$options$conf.level
                if (!is.numeric(cl) || length(cl) != 1L || !is.finite(cl) || cl < 0.5 || cl > 0.999) {
                    return(list(valid = FALSE, message = .("Confidence level must be between 0.5 and 0.999.")))
                }
                tr <- self$options$trimlevel
                if (!is.numeric(tr) || length(tr) != 1L || !is.finite(tr) || tr < 0 || tr > 0.4) {
                    return(list(valid = FALSE, message = .("Trim proportion must be between 0 and 0.4.")))
                }
                bfp <- self$options$bfprior
                if (!is.numeric(bfp) || length(bfp) != 1L || !is.finite(bfp) || bfp < 0.1 || bfp > 2) {
                    return(list(valid = FALSE, message = .("Bayes prior scale must be between 0.1 and 2.")))
                }
                dig <- self$options$digits
                if (!is.numeric(dig) || length(dig) != 1L || !is.finite(dig) || dig < 0 || dig > 5 || dig != floor(dig)) {
                    return(list(valid = FALSE, message = .("Decimal places must be an integer between 0 and 5.")))
                }
                st <- private$.option("typestatistics")
                if (!is.character(st) || length(st) != 1L || !st %in% c("parametric", "nonparametric", "robust", "bayes")) {
                    return(list(valid = FALSE, message = .("Type of statistic must be parametric, nonparametric, robust, or bayes.")))
                }
                alt <- self$options$alternative
                if (!is.character(alt) || length(alt) != 1L || !alt %in% c("two.sided", "greater", "less")) {
                    return(list(valid = FALSE, message = .("Test direction must be two.sided, greater, or less.")))
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
                            .fmt(
                                .("<strong>The statistical subtitle is testing against zero.</strong><br>The subtitle on the histogram is a <strong>{test}</strong> asking whether the center of your data differs from <strong>0</strong>."),
                                test = test_name),
                            if (chose_it) "" else
                                paste0(" ", .("This is the default null, because 'Custom null value' is switched off so no test value is supplied.")),
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
                    bf_active <- identical(stat_type, "bayes") ||
                        (identical(stat_type, "parametric") && isTRUE(private$.option("bf.message")))
                    if (bf_active) {
                        warnings <- c(warnings, .fmt(
                            .("<strong>Bayes factor prior:</strong> the Bayes factor is computed under a Cauchy prior on standardized effect size with scale r = {r} (the displayed posterior difference is on the original measurement scale). A Bayes factor is only interpretable against the prior that produced it, so report this value alongside it, and vary the Bayes prior scale to check how sensitive your conclusion is to the choice."),
                            r = base::format(self$options$bfprior)))
                    }
                    if (identical(stat_type, "robust")) {
                        tr <- self$options$trimlevel
                        trim_details <- character()
                        for (var in variables) {
                            if (!var %in% names(data)) next
                            n_used <- sum(is.finite(data[[var]]))
                            k_trim <- floor(tr * n_used)
                            if (length(variables) == 1) {
                                trim_details <- c(trim_details, .fmt(
                                    .("{k} of {total} observations at each tail"),
                                    k = k_trim, total = n_used))
                            } else {
                                trim_details <- c(trim_details, .fmt(
                                    .("{var}: {k} of {total} at each tail"),
                                    var = htmltools::htmlEscape(var), k = k_trim, total = n_used))
                            }
                            if (n_used - 2L * k_trim < 3L) {
                                warnings <- c(warnings, .fmt(
                                    .("<strong>Insufficient observations after trimming '{var}':</strong> trimming {pct}% from each tail of n = {total} leaves fewer than 3 observations, so the robust bootstrap-t test cannot be computed. Lower Trim proportion or switch to Nonparametric."),
                                    var = htmltools::htmlEscape(var),
                                    pct = base::format(100 * tr), total = n_used))
                            } else {
                                x_u <- data[[var]][is.finite(data[[var]])]
                                se_chk <- tryCatch(WRS2::trimse(x_u, tr), error = function(e) NA_real_)
                                if (length(unique(x_u)) > 1L && (!is.finite(se_chk) || se_chk <= 0)) {
                                    warnings <- c(warnings, .fmt(
                                        .("<strong>Zero Winsorized variance in '{var}':</strong> heavy ties in the trimmed core make the robust standard error 0, so the robust test cannot be computed. Lower Trim proportion or switch to Nonparametric."),
                                        var = htmltools::htmlEscape(var)))
                                }
                            }
                        }
                        nboot_label <- if (identical(self$options$alternative, "two.sided")) 200L else 2000L
                        warnings <- c(warnings, .fmt(
                            .("<strong>Robust test trim level:</strong> the robust test uses a bootstrap-t on a trimmed mean (B = {nboot} resamples), discarding the highest and lowest {pct}% of observations ({detail}). If a genuine high- or low-value subgroup matters clinically, that subgroup is being trimmed away - lower the trim proportion or use the nonparametric test instead."),
                            nboot = nboot_label,
                            pct = base::format(100 * tr),
                            detail = paste(trim_details, collapse = "; ")))
                    }
                    if (identical(stat_type, "nonparametric")) {
                        small_np <- vapply(variables, function(v) {
                            if (!v %in% names(data)) return(FALSE)
                            n_u <- sum(is.finite(data[[v]]))
                            n_u >= 3 && n_u < 50
                        }, logical(1))
                        if (any(small_np)) {
                            warnings <- c(warnings, .("<strong>Wilcoxon normal approximation:</strong> at small sample sizes (n &lt; 50), the one-sample Wilcoxon signed-rank p-value uses a continuity-corrected normal approximation (exact = FALSE), which can be slightly conservative."))
                        }
                        warnings <- c(warnings, .("<strong>Wilcoxon signed-rank assumption:</strong> this test evaluates whether the pseudomedian differs from the test value (which equals the median only when the distribution is roughly symmetric around the test value)."))
                        for (var in variables) {
                            if (!var %in% names(data)) next
                            x_u <- data[[var]][is.finite(data[[var]])]
                            n_zero <- sum(x_u == effective_null)
                            if (n_zero > 0L && n_zero < length(x_u)) {
                                warnings <- c(warnings, .fmt(
                                    .("<strong>Exact ties with test value in '{var}':</strong> {nz} of {nt} observations equal {mu} and are excluded from the Wilcoxon signed-rank statistic and rank-biserial correlation (computed on {nnz} non-zero differences)."),
                                    var = htmltools::htmlEscape(var),
                                    nz = n_zero, nt = length(x_u),
                                    mu = base::format(effective_null),
                                    nnz = length(x_u) - n_zero))
                            }
                        }
                    }
                    if (!identical(self$options$alternative, "two.sided")) {
                        dir_word <- if (identical(self$options$alternative, "greater"))
                            .("greater") else .("less")
                        if (identical(stat_type, "bayes")) {
                            warnings <- c(warnings, .fmt(
                                .("<strong>One-sided test in use.</strong> The reported Bayes factor tests the directional hypothesis that the center is {direction} than the test value (while the posterior difference and ETI summarize the unconstrained posterior). A one-sided test is only valid if the direction was chosen before seeing these data."),
                                direction = dir_word))
                        } else {
                            warnings <- c(warnings, .fmt(
                                .("<strong>One-sided test in use.</strong> The reported p-value and confidence interval test only whether the center is {direction} than the test value, not whether it differs in either direction. A one-sided test is only valid if the direction was chosen before seeing these data."),
                                direction = dir_word))
                        }
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
                        warnings <- c(warnings, .fmt(
                            .("<strong>No data to plot for '{var}'.</strong> Every value in this variable is missing, so the histogram is empty. Check the variable selection, and any row filters that may be active."),
                            var = htmltools::htmlEscape(var)))
                        next
                    }

                    if (length(var_data) < 3) {
                        warnings <- c(warnings, if (length(var_data) == 1)
                            .fmt(
                                .("<strong>Only 1 finite numeric value for '{var}'.</strong> A histogram needs a range of values to bin; the panel will be empty or near-empty, and any test statistic based on it is not interpretable."),
                                var = htmltools::htmlEscape(var))
                        else
                            .fmt(
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
                                .fmt(
                                    .("<strong>'{var}' is not normally distributed</strong> (Shapiro-Wilk {p}), but the subtitle is reporting a <strong>parametric one-sample t-test</strong>, which assumes approximate normality."),
                                    var = htmltools::htmlEscape(var),
                                    p = private$.formatPWithOp(sw_p)),
                                " ",
                                if (length(var_data) >= 30)
                                    .fmt(
                                        .("At n = {n} the central limit theorem makes the t-test fairly robust to this, so treat it as a caution rather than an error."),
                                        n = length(var_data))
                                else
                                    .fmt(
                                        .("At n = {n} the central limit theorem does not rescue the test, so the p-value and confidence interval may be misleading."),
                                        n = length(var_data)),
                                " ", .("Switch Type of statistic to Nonparametric (Wilcoxon signed-rank) if you want a test that makes no normality assumption.")
                            ))
                        }
                    }

                    # Sample size warnings
                    if (length(var_data) < 30) {
                        warnings <- c(warnings, .fmt(
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
                                    .fmt(
                                        .("Detected 1 extreme outlier in '{var}' (more than 3 scaled MAD from the median). Consider reviewing data quality, or using the robust method."),
                                        var = htmltools::htmlEscape(var))
                                else
                                    .fmt(
                                        .("Detected {n} extreme outliers in '{var}' (more than 3 scaled MAD from the median). Consider reviewing data quality, or using the robust method."),
                                        n = outliers, var = htmltools::htmlEscape(var)))
                            }
                        }
                    }
                    
                    # Constant data warning. The histogram panel comes out BLANK in this case
                    # (stat_bin() cannot build breaks over a zero range and its error only
                    # reaches stderr), so say so rather than leaving the user to guess.
                    if (length(unique(var_data)) == 1) {
                        warnings <- c(warnings, .fmt(
                            .("<strong>Variable '{var}' has constant values</strong> (every row is {value}). There is no range to bin, so the histogram panel for this variable will be empty. Check the variable selection and any active row filters."),
                            var = htmltools::htmlEscape(var),
                            value = htmltools::htmlEscape(base::format(var_data[1]))))
                    }

                    # Very few unique values warning. Excludes the constant case, which
                    # has its own dedicated message above - a constant column used to raise
                    # BOTH, the second reading "has only 1 unique values".
                    if (length(unique(var_data)) > 1 &&
                        length(unique(var_data)) < 5 && length(var_data) > 10) {
                        warnings <- c(warnings, .fmt(
                            .("Variable '{var}' has only {n} unique values. Consider treating it as categorical or ordinal data."),
                            var = htmltools::htmlEscape(var), n = length(unique(var_data))))
                    }
                }
                
                # Grouped analysis warnings: count finite dep values per populated level
                # (ignores unused 0-row factor levels and safely handles addNA() factors).
                if (!is.null(self$options$grvar) && self$options$grvar %in% names(data)) {
                    group_var <- data[[self$options$grvar]]
                    g_chr <- as.character(group_var)
                    raw_lvls <- if (is.factor(group_var)) levels(group_var) else unique(g_chr)
                    raw_lvls <- raw_lvls[!is.na(raw_lvls)]

                    small_groups <- character()
                    for (var in variables) {
                        if (!var %in% names(data)) next
                        empty_lvls <- character()
                        tiny_lvls <- character()
                        for (lvl in raw_lvls) {
                            idx_l <- !is.na(g_chr) & g_chr == lvl
                            if (!any(idx_l)) next  # unused 0-row factor level
                            n_fin_l <- sum(is.finite(data[[var]][idx_l]))
                            if (n_fin_l == 0L) {
                                empty_lvls <- c(empty_lvls, lvl)
                            } else {
                                if (n_fin_l < 10L) small_groups <- c(small_groups, lvl)
                                if (n_fin_l < 3L) tiny_lvls <- c(tiny_lvls, lvl)
                            }
                        }
                        if (length(empty_lvls) > 0) {
                            warnings <- c(warnings, .fmt(
                                .("<strong>Subgroup(s) omitted from grouped histogram:</strong> '{var}' has no finite values in {groups}, so those subgroup panel(s) were skipped."),
                                var = htmltools::htmlEscape(var),
                                groups = htmltools::htmlEscape(paste(empty_lvls, collapse = ", "))))
                        }
                        if (length(tiny_lvls) > 0 && isTRUE(private$.option("resultssubtitle"))) {
                            warnings <- c(warnings, .fmt(
                                .("<strong>Subgroup(s) too small for statistical testing:</strong> '{var}' has fewer than 3 finite values in {groups}, so their statistical subtitles were omitted."),
                                var = htmltools::htmlEscape(var),
                                groups = htmltools::htmlEscape(paste(tiny_lvls, collapse = ", "))))
                        }
                    }

                    small_groups <- unique(small_groups)
                    if (length(small_groups) > 0) {
                        warnings <- c(warnings, if (length(small_groups) == 1)
                            .fmt(
                                .("Small group size detected: {groups} (n < 10). Results may be unreliable for this group."),
                                groups = htmltools::htmlEscape(small_groups))
                        else
                            .fmt(
                                .("Small group sizes detected: {groups} (n < 10). Results may be unreliable for these groups."),
                                groups = htmltools::htmlEscape(paste(small_groups, collapse = ", "))))
                    }

                    warnings <- c(warnings, .("<strong>Pooled diagnostics note:</strong> normality, skewness, and outlier considerations above are computed on the pooled sample, whereas the grouped histogram tests each subgroup separately."))
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
                    warnings <- c(warnings, .("<strong>Your X-axis label was not used.</strong> With more than one variable selected, each panel is labeled with its own variable name."))
                }
                # ggstatsplot writes the Bayes factor into the CAPTION slot ONLY for a
                # PARAMETRIC test (bf.condition = type == "parametric"). Nonparametric,
                # robust, and bayes modes leave the user caption intact.
                if (nzchar(self$options$caption) &&
                    isTRUE(private$.option("resultssubtitle")) &&
                    isTRUE(private$.option("bf.message")) &&
                    identical(private$.option("typestatistics"), "parametric")) {
                    warnings <- c(warnings, .("<strong>Your caption was not used.</strong> With Bayes factor switched on, the caption area is written by the Bayes factor. Switch off Bayes factor to show your own caption."))
                }

                # ANALYTIC N. ggstatsplot drops missing values silently and reports a
                # statistic computed on whatever is left, so the n behind the subtitle was
                # never stated anywhere: with 37 of 120 rows missing, nothing in the output
                # said the test ran on 83. Check both dep variables and grvar.
                dropped <- character()
                for (var in variables) {
                    if (!var %in% names(data)) next
                    n_total <- length(data[[var]])
                    n_used  <- sum(!is.na(data[[var]]))
                    if (n_used < n_total)
                        dropped <- c(dropped, .fmt(
                            .("{var}: n = {used} of {total} ({pct}% missing)"),
                            var = htmltools::htmlEscape(var), used = n_used, total = n_total,
                            pct = base::format(round(100 * (n_total - n_used) / n_total, 1))))
                }
                if (!is.null(self$options$grvar) && self$options$grvar %in% names(data)) {
                    gv <- self$options$grvar
                    n_g_total <- length(data[[gv]])
                    n_g_used  <- sum(!is.na(data[[gv]]))
                    if (n_g_used < n_g_total)
                        dropped <- c(dropped, .fmt(
                            .("{var} (grouped plot): n = {used} of {total} ({pct}% missing)"),
                            var = htmltools::htmlEscape(gv), used = n_g_used, total = n_g_total,
                            pct = base::format(round(100 * (n_g_total - n_g_used) / n_g_total, 1))))
                }
                if (length(dropped) > 0)
                    warnings <- c(warnings, .fmt(
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
                        warnings <- c(warnings, .fmt(
                            .("<strong>Slow computation:</strong> a Bayesian analysis of {n} rows may take several minutes. Consider the parametric or nonparametric method for faster results."),
                            n = n_rows))
                    } else if (n_rows > 500) {
                        warnings <- c(warnings, .fmt(
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
                    warnings <- c(warnings, .fmt(
                        .("<strong>Large dataset:</strong> processing {n} rows may take extra time for plot generation and statistical calculation."),
                        n = nrow(data)))
                }
                
                return(warnings)
            },
            
            # Generate clinical interpretation
            .generateClinicalInterpretation = function(data, variables) {
                if (!isTRUE(private$.option("showInterpretation"))) return("")
                
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
                    } else {
                        0
                    }
                    
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
                                   .fmt(
                                       .("But with n = {n} the normality check below could not be run, so symmetry here is not evidence of a normal distribution."),
                                       n = n))
                    }

                    normality_text <- if (!is.na(shapiro_p)) {
                        paste0(.fmt(
                                   .("<li><strong>Normality (Shapiro-Wilk):</strong> {p}"),
                                   p = private$.formatPWithOp(shapiro_p)),
                               " ",
                               if (shapiro_p > 0.05) .("(no evidence against normality)")
                               else .("(evidence against normality)"), "</li>")
                    } else if (sd_val == 0) {
                        paste0("<li>", .("<strong>Normality:</strong> not assessable - every value is identical."), "</li>")
                    } else {
                        paste0("<li>", .fmt(
                            .("<strong>Normality:</strong> Shapiro-Wilk is not available at n = {n}; the skewness rule-of-thumb was used instead."),
                            n = n), "</li>")
                    }

                    implications_text <- if (sd_val == 0) {
                        .("This variable is constant - every observation has the same value. There is no distribution to summarize and no test to run on it.")
                    } else if (is_normal) {
                        .("No evidence of a departure from normality was found, so parametric statistics (t-tests, ANOVA) are reasonable here. Absence of evidence against normality does not establish that the distribution is normal, particularly at small n or when the Shapiro-Wilk test could not be run. Mean and SD are appropriate summary measures.")
                    } else if (!is.na(shapiro_p) && shapiro_p > 0.05 && abs(skewness_val) >= 1) {
                        .("Substantial skewness (|g1| >= 1) was observed even though the Shapiro-Wilk test did not reach significance (which has limited power at small sample sizes). Prefer rank-based methods (Wilcoxon signed-rank here; Mann-Whitney or Kruskal-Wallis for group comparisons) and report median and IQR.")
                    } else if (n >= 30 && abs(skewness_val) < 1) {
                        .fmt(
                            .("Non-normal distribution: median and IQR summarize the shape better than mean and SD, and rank-based methods (Wilcoxon signed-rank here; Mann-Whitney or Kruskal-Wallis for group comparisons) make no normality assumption. At n = {n} with moderate skewness, the central limit theorem still makes the one-sample t-test fairly robust for inference on the mean."),
                            n = n)
                    } else {
                        .("Non-normal distribution: prefer rank-based methods. Within this analysis that is the Wilcoxon signed-rank option; if you go on to compare groups, the rank-based equivalents are Mann-Whitney (two groups) and Kruskal-Wallis (three or more). Median and IQR are the more appropriate summary measures here.")
                    }

                    var_interpretation <- paste0(
                        "<h4>", htmltools::htmlEscape(var), "</h4><ul>",
                        "<li>", .fmt(.("<strong>Sample size:</strong> {n} observations"), n = n), "</li>",
                        "<li>", .fmt(
                            .("<strong>Central tendency:</strong> mean = {mean}, median = {median}"),
                            mean = private$.formatDescNum(mean_val),
                            median = private$.formatDescNum(median_val)), "</li>",
                        "<li>", .fmt(.("<strong>Variability:</strong> SD = {sd}"),
                                                sd = private$.formatDescNum(sd_val)), "</li>",
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
                
                # Process binwidth (honour clinical-preset overrides via private$.option)
                binwidth <- NULL
                if (isTRUE(private$.option("changebinwidth"))) {
                    binwidth <- private$.option("binwidth")
                }

                # Process centrality.type
                ct_opt <- private$.option("centralitytype")
                centrality.type <- if (!is.null(ct_opt) && ct_opt != 'default') ct_opt else NULL

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
                    bf.message = private$.option("bf.message"),
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
                    "<li>", .("<strong>Pathology scores:</strong> tumor grades, staging scores, severity ratings."), "</li>",
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

                    # Validate inputs BEFORE touching any result panel so jamovi preserves its
                    # greyed-out prior result presentation on reject() (guide §5 Round 8).
                    validation_result <- private$.validateInputs()
                    if (!validation_result$valid) {
                        # Only show error for actual data problems, not missing variables
                        if (!grepl("Please select at least one variable", validation_result$message)) {
                            jmvcore::reject(validation_result$message)
                        }
                    }

                    todo <- paste0("<br>", .("You have selected to make a histogram."), "<br><hr>")

                    self$results$todo$setContent(todo)

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
                    
                    # Generate and populate clinical interpretation (honouring clinicalPreset)
                    show_interp <- isTRUE(private$.option("showInterpretation"))
                    self$results$interpretation$setVisible(show_interp)
                    if (show_interp) {
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
                withr::local_seed(self$options$seed)

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
                private$.statsComputed <- FALSE

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

                if (isTRUE(private$.statsComputed) &&
                    (statsSeedMatters(options_data$typestatistics, "one_sample") ||
                     captionSeedMatters(options_data$typestatistics, "one_sample", 2L, options_data$bf.message)))
                    plot <- addSeedCaption(plot, self, self$options$seed)

                # Print plot
                print(plot)
                TRUE
            }


            ,
            .plot2 = function(image, ggtheme, theme, ...) {
                # Seed the sampling-based paths (Bayesian MCMC, bootstrap CIs) so a
                # re-render of an unchanged analysis reports the same numbers.
                withr::local_seed(self$options$seed)

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
                private$.statsComputed <- FALSE
                
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

                if (isTRUE(private$.statsComputed) &&
                    (statsSeedMatters(options_data$typestatistics, "one_sample") ||
                     captionSeedMatters(options_data$typestatistics, "one_sample", 2L, options_data$bf.message)))
                    plot2 <- addSeedCaption(plot2, self, self$options$seed)

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

                # Prepare sanitized data (handles non-finite values and factor rejection)
                mydata <- private$.prepareData()
                dep <- self$options$dep

                # Build one ggpubr histogram using a safe syntactic column name (`.x`) so
                # column names with parentheses, slashes, or operators (e.g. "Age (years)",
                # "Grade 2/3", "A+B") are never parsed as R expressions by ggpubr.
                build_one <- function(depvar, set_title = FALSE) {
                    x_vec <- mydata[[depvar]]
                    plot_df <- data.frame(.x = x_vec)
                    args <- list(
                        data = plot_df,
                        x = ".x",
                        fill = self$options$ggpubrPalette,
                        add_density = self$options$ggpubrAddDensity,
                        xlab = depvar
                    )
                    if (isTRUE(self$options$ggpubrAddDensity)) {
                        args$y <- "density"
                        args$ylab <- .("Density")
                    }
                    if (set_title) {
                        args$title <- depvar
                    }
                    p <- do.call(ggpubr::gghistogram, args)
                    if (self$options$ggpubrAddMean) {
                        mean_val <- mean(x_vec, na.rm = TRUE)
                        if (is.finite(mean_val)) {
                            p <- p + ggplot2::geom_vline(xintercept = mean_val,
                                                         color = "red",
                                                         linetype = "dashed",
                                                         linewidth = 1)
                        }
                    }
                    p + ggpubr::theme_pubr()
                }

                if (length(dep) == 1) {
                    print(build_one(dep, set_title = FALSE))
                } else {
                    plotlist <- lapply(as.list(dep), build_one, set_title = TRUE)
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

                # Prepare sanitized data
                mydata <- private$.prepareData()
                dep <- self$options$dep
                grvar <- self$options$grvar

                # Build one faceted ggpubr histogram using safe `.x` / `.g` column names,
                # dropping rows with NA in grvar (including explicit addNA() levels), and
                # computing per-group means for each facet when ggpubrAddMean is enabled.
                build_one <- function(depvar, set_title = FALSE) {
                    g_vec <- mydata[[grvar]]
                    g_chr <- as.character(g_vec)
                    keep <- !is.na(g_chr)
                    raw_lvls <- if (is.factor(g_vec)) levels(g_vec) else unique(g_chr[keep])
                    lvls <- raw_lvls[!is.na(raw_lvls)]
                    plot_df <- data.frame(
                        .x = mydata[[depvar]][keep],
                        .g = droplevels(factor(g_chr[keep], levels = lvls))
                    )
                    args <- list(
                        data = plot_df,
                        x = ".x",
                        fill = self$options$ggpubrPalette,
                        add_density = self$options$ggpubrAddDensity,
                        facet.by = ".g",
                        xlab = depvar
                    )
                    if (isTRUE(self$options$ggpubrAddDensity)) {
                        args$y <- "density"
                        args$ylab <- .("Density")
                    }
                    if (set_title) {
                        args$title <- depvar
                    }
                    p <- do.call(ggpubr::gghistogram, args)
                    if (self$options$ggpubrAddMean && nrow(plot_df) > 0) {
                        grp_means <- stats::aggregate(
                            .x ~ .g,
                            data = plot_df[is.finite(plot_df$.x), , drop = FALSE],
                            FUN = mean
                        )
                        if (nrow(grp_means) > 0) {
                            p <- p + ggplot2::geom_vline(
                                data = grp_means,
                                mapping = ggplot2::aes(xintercept = !!rlang::sym(".x")),
                                color = "red",
                                linetype = "dashed",
                                linewidth = 1
                            )
                        }
                    }
                    p + ggpubr::theme_pubr()
                }

                if (length(dep) == 1) {
                    print(build_one(dep, set_title = FALSE))
                } else {
                    plotlist <- lapply(as.list(dep), build_one, set_title = TRUE)
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

                mydata <- private$.prepareData()
                dep <- self$options$dep

                # Use safe internal column `.x` so non-syntactic names (e.g. "Age (years)",
                # "A+B") are never parsed as R expressions by ggpubr.
                build <- function(v) {
                    plot_df <- data.frame(.x = mydata[[v]])
                    ggpubr::ggdensity(
                        plot_df, x = ".x", fill = self$options$ggpubrDensityColor,
                        add = "mean", rug = TRUE, xlab = v,
                        title = if (length(dep) > 1) v else NULL
                    ) + ggpubr::theme_pubr()
                }

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

                mydata <- private$.prepareData()
                dep <- self$options$dep

                build <- function(v) {
                    plot_df <- data.frame(.x = mydata[[v]])
                    ggpubr::ggqqplot(
                        plot_df, x = ".x", color = self$options$ggpubrDensityColor,
                        ylab = v,
                        title = if (length(dep) > 1) v else NULL
                    ) + ggpubr::theme_pubr()
                }

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

                mydata <- private$.prepareData()
                dep <- self$options$dep

                build <- function(v) {
                    plot_df <- data.frame(.x = mydata[[v]])
                    ggpubr::ggecdf(
                        plot_df, x = ".x", color = self$options$ggpubrDensityColor,
                        xlab = v,
                        title = if (length(dep) > 1) v else NULL
                    ) + ggpubr::theme_pubr()
                }

                if (length(dep) == 1) {
                    print(build(dep))
                } else {
                    print(ggpubr::ggarrange(plotlist = lapply(dep, build),
                                            ncol = 1, nrow = length(dep)))
                }

                TRUE
            }
            ,
            # Hardened syntax-pane serialization (Category G1): drop undeclared option names,
            # drop list values, and escape character scalars with encodeString(quote = '"')
            # so quotes, backslashes, or newlines cannot break out of string literals.
            .sourcifyOption = function(option) {
                declared <- setdiff(names(formals(jjhistostats)), "data")
                if (!option$name %in% declared)
                    return("")
                value <- option$value
                if (is.list(value))
                    return("")
                if (is.character(value) && length(value) == 1L && !is.na(value)) {
                    if (identical(value, option$default))
                        return("")
                    return(paste0(option$name, " = ", encodeString(value, quote = '"')))
                }
                super$.sourcifyOption(option)
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
                # variables" bug). All non-variable options go through our hardened
                # private$.sourcifyOption(), and undeclared option names are dropped.
                args <- character(0)
                declared <- setdiff(names(formals(jjhistostats)), "data")
                for (option in private$.options$options) {
                    if (!option$name %in% declared)
                        next
                    if (inherits(option, 'OptionVariable') || inherits(option, 'OptionVariables')) {
                        val <- option$value
                        if (is.character(val) && length(val) > 0)
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
