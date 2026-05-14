#' @title Latent Biomarker Construct + Cox Regression
#' @return Results object
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
#' @import jmvcore

latentbiomarkerClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "latentbiomarkerClass",
    inherit = latentbiomarkerBase,
    private = list(

        # ---- Notice collection (pattern from R/waterfall.b.R:95-142) ----
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type, title = title, content = content
            )
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                return()
            }
            typeStyles <- list(
                ERROR          = list(color = "#dc2626", bgcolor = "#fef2f2", border = "#fca5a5"),
                STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed", border = "#fdba74"),
                WARNING        = list(color = "#ca8a04", bgcolor = "#fefce8", border = "#fde047"),
                INFO           = list(color = "#2563eb", bgcolor = "#eff6ff", border = "#93c5fd")
            )
            html <- "<div style='margin: 10px 0;'>"
            for (n in private$.noticeList) {
                style <- typeStyles[[n$type]]
                if (is.null(style)) style <- typeStyles$INFO
                html <- paste0(html,
                    "<div style='background-color: ", style$bgcolor,
                    "; border-left: 4px solid ", style$border,
                    "; padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                    "<strong style='color: ", style$color, ";'>",
                    htmltools::htmlEscape(n$title), "</strong><br>",
                    "<span style='color: #374151;'>",
                    htmltools::htmlEscape(n$content), "</span>",
                    "</div>")
            }
            html <- paste0(html, "</div>")
            self$results$notices$setContent(html)
        },

        # ---- Translatable refusal/warning messages ----
        .messages = list(
            G1_refuse = function(n) paste0(
                "Insufficient sample size for SEM. With n = ", n, " (< 100), factor ",
                "loadings and fit indices are unreliable. Consider (1) a simpler ",
                "z-score composite with conventional Cox regression, or (2) waiting ",
                "for a larger cohort."),
            G1_warn = function(n) paste0(
                "Sample size is modest for SEM (n = ", n, "; 100-199). Results may be ",
                "unstable; interpret loadings and confidence intervals with caution."),
            G2_refuse = function(k, n, cpp) paste0(
                "Insufficient cases per CFA parameter. Your measurement model has K = ",
                k, " parameters (loadings + residual variances + factor variance), ",
                "requiring n >= ", 5L * k, " (and ideally n >= ", 10L * k, "). You have n = ",
                n, ", giving CPP = ", round(cpp, 2), ". Options: (1) reduce indicators, ",
                "(2) use a summary score with conventional Cox, or (3) collect more data."),
            G2_warn = function(cpp) paste0(
                "Cases-per-parameter ratio is ", round(cpp, 2),
                " (recommended >= 10). Standard errors may be optimistic."),
            G3_refuse = function(k) paste0(
                "Too few indicators (", k, "). A reflective factor requires at least 3 ",
                "indicators to be identified. With 2 indicators the model is ",
                "under-identified and cannot be fit."),
            G3_warn = "With exactly 3 indicators and 1 factor, the model is just-identified (df = 0). CFI, RMSEA, and SRMR are not meaningful - they will be reported as NA or trivial values.",
            G4_warn = function(epv) paste0(
                "Cox model has ", round(epv, 2), " events per covariate (recommended >= 10; ",
                "Peduzzi/Concato). Consider reducing adjusters."),
            G5_warn = function(rmax) paste0(
                "Indicators are weakly intercorrelated (max |r| = ", round(rmax, 2),
                "). They may not reflect a common construct."),
            G6_refuse = paste0(
                "CFA assumes indicators reflect an underlying latent construct ",
                "(e.g., CD8/PD-L1/TIL all reflect 'immune activation'). If indicators ",
                "constitute a composite where each adds independent information ",
                "(e.g., a histologic grade summing nuclear grade + tubules + mitoses), ",
                "CFA gives misleading results. Use cSEM or seminr for formative models, ",
                "or compute the composite directly. Tick the confirmation box if your ",
                "model is genuinely reflective."),
            FIT_poor = function(cfi, rmsea) paste0(
                "Single-factor model fits poorly (CFI = ", round(cfi, 3),
                ", RMSEA = ", round(rmsea, 3),
                "). Consider splitting into multiple constructs - SEMLj supports multi-factor SEM."),
            PH_violated = function(p) paste0(
                "Proportional-hazards assumption violated (global p = ", signif(p, 3),
                "). HR is an average effect; consider stratification or time-dependent terms."),
            UNCERTAINTY = paste0(
                "HR confidence intervals do not account for measurement uncertainty in ",
                "the latent factor (two-stage Murphy-Topel issue). True CIs are wider ",
                "than reported. Interpret conservatively.")
        ),

        # ---- Helper: count CFA parameters for a single-factor std.lv model ----
        .countCFAParams = function(n_indicators) {
            2L * as.integer(n_indicators)
        },

        # ---- Detect indicator types ----
        # Returns list(continuous = chr, ordinal = chr, all_continuous = lgl)
        .detectIndicatorTypes = function(df, indicators, override) {
            if (identical(override, "continuous")) {
                return(list(continuous = indicators, ordinal = character(0), all_continuous = TRUE))
            }
            if (identical(override, "ordinal")) {
                return(list(continuous = character(0), ordinal = indicators, all_continuous = FALSE))
            }
            # auto: ordinal if factor, ordered, logical, or numeric with <= 5 unique values
            is_ord <- vapply(indicators, function(v) {
                x <- df[[v]]
                is.factor(x) || is.ordered(x) || is.logical(x) ||
                    (is.numeric(x) && length(unique(stats::na.omit(x))) <= 5L)
            }, logical(1))
            list(
                continuous = indicators[!is_ord],
                ordinal    = indicators[is_ord],
                all_continuous = !any(is_ord)
            )
        },

        # ---- Choose estimator based on indicator types ----
        .chooseEstimator = function(itypes) {
            if (itypes$all_continuous) {
                list(estimator = "MLR", missing = "fiml", ordered = NULL)
            } else {
                list(estimator = "WLSMV", missing = "pairwise", ordered = itypes$ordinal)
            }
        },

        # ---- Build lavaan model syntax with escaped indicator names ----
        .buildModelSyntax = function(indicators) {
            rhs <- paste(vapply(indicators, jmvcore::composeTerm, character(1)),
                         collapse = " + ")
            paste0("Factor =~ ", rhs)
        },

        # ---- Fit CFA ----
        .fitCFA = function(df, indicators, est_spec) {
            model <- private$.buildModelSyntax(indicators)
            args <- list(
                model = model,
                data = df,
                std.lv = TRUE,
                estimator = est_spec$estimator
            )
            if (!is.null(est_spec$ordered) && length(est_spec$ordered) > 0L) {
                args$ordered <- est_spec$ordered
            }
            if (!is.null(est_spec$missing)) {
                args$missing <- est_spec$missing
            }
            do.call(lavaan::cfa, args)
        },

        # ---- Interpret fit indices ----
        .interpretFit = function(cfi, tli, rmsea, srmr) {
            if (is.na(cfi) || is.na(rmsea)) return("Just-identified - fit untestable")
            good <- isTRUE(cfi >= 0.95) && isTRUE(tli >= 0.95) &&
                    isTRUE(rmsea <= 0.06) && isTRUE(srmr <= 0.08)
            acceptable <- isTRUE(cfi >= 0.90) && isTRUE(rmsea <= 0.10) && isTRUE(srmr <= 0.10)
            if (good) "Good fit"
            else if (acceptable) "Acceptable fit"
            else "Poor fit"
        },

        # ---- McDonald's omega and AVE from standardized loadings ----
        # omega = (sum lambda)^2 / ( (sum lambda)^2 + sum theta )
        # AVE   = mean(lambda^2)
        .computeReliability = function(std_loadings) {
            lam <- std_loadings
            theta <- 1 - lam^2
            omega <- (sum(lam))^2 / ((sum(lam))^2 + sum(theta))
            ave   <- mean(lam^2)
            list(omega = omega, ave = ave)
        },

        # ---- Extract factor scores ----
        .extractScores = function(fit, method, standardize) {
            scores <- lavaan::lavPredict(fit, method = method)
            v <- as.numeric(scores[, 1])
            if (isTRUE(standardize)) {
                v <- as.numeric(scale(v))
            }
            v
        },

        # ---- KM render: receives setState payload ----
        .kmPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)
            df <- data.frame(time = state$time, event = state$event,
                             strata = state$strata)
            df <- df[stats::complete.cases(df), , drop = FALSE]
            if (nrow(df) < 5L) return(FALSE)
            fit <- survival::survfit(survival::Surv(time, event) ~ strata, data = df)
            p <- survminer::ggsurvplot(
                fit, data = df,
                pval = TRUE, risk.table = FALSE,
                xlab = "Time", ylab = "Survival probability",
                legend.title = "Factor-score stratum",
                ggtheme = ggtheme
            )$plot
            print(p)
            TRUE
        },

        # ---- Holds CFA/Cox state across helper calls within one .run() ----
        .fitState = NULL,

        # ---- Pipeline ----
        .run = function() {
            private$.noticeList <- list()
            opt <- self$options

            # Silent on incomplete input
            if (is.null(opt$dep_time) || is.null(opt$dep_event) ||
                is.null(opt$indicators) || length(opt$indicators) == 0) {
                return()
            }

            # ---- Gate G6: reflective confirmation ----
            if (!isTRUE(opt$reflective_confirmed)) {
                private$.addNotice("ERROR",
                    "Reflective-measurement confirmation required",
                    private$.messages$G6_refuse)
                private$.renderNotices()
                return()
            }

            df <- self$data
            df <- jmvcore::naOmit(df[, c(opt$dep_time, opt$dep_event,
                                         opt$indicators, opt$adjusters), drop = FALSE])

            n <- nrow(df)
            k <- length(opt$indicators)

            # ---- Gate G3: indicators < 3 (hard refusal) ----
            if (k < 3L) {
                private$.addNotice("ERROR",
                    "Too few indicators",
                    private$.messages$G3_refuse(k))
                private$.renderNotices()
                return()
            }

            # ---- Gate G1: n < 100 (hard refusal); 100 <= n < 200 soft warning ----
            if (n < 100L) {
                private$.addNotice("ERROR",
                    "Insufficient sample size",
                    private$.messages$G1_refuse(n))
                private$.renderNotices()
                return()
            }
            if (n < 200L) {
                private$.addNotice("WARNING",
                    "Modest sample size",
                    private$.messages$G1_warn(n))
            }

            # ---- Indicator-type detection + estimator selection ----
            indicator_types_opt <- if (!is.null(opt$indicator_types)) opt$indicator_types else "auto"
            itypes <- private$.detectIndicatorTypes(df, opt$indicators, indicator_types_opt)
            est_spec <- private$.chooseEstimator(itypes)

            if (!itypes$all_continuous) {
                private$.addNotice("STRONG_WARNING",
                    "WLSMV estimator selected (ordinal/binary indicators detected)",
                    paste0(
                        "Using WLSMV with polychoric/tetrachoric correlations for ",
                        length(itypes$ordinal), " ordinal/binary indicator(s): ",
                        paste(itypes$ordinal, collapse = ", "),
                        ". WLSMV requires larger samples than MLR; recommended n >= 500."))
            }

            # ---- Gate G2: cases per CFA parameter ----
            n_params <- private$.countCFAParams(k)
            cpp <- n / n_params
            if (cpp < 5) {
                private$.addNotice("ERROR",
                    "Insufficient cases per CFA parameter",
                    private$.messages$G2_refuse(n_params, n, cpp))
                private$.renderNotices()
                return()
            }
            if (cpp < 10) {
                private$.addNotice("WARNING",
                    "Low cases-per-parameter ratio",
                    private$.messages$G2_warn(cpp))
            }

            # ---- Gate G3-soft: just-identified model ----
            if (k == 3L) {
                private$.addNotice("INFO",
                    "Just-identified model",
                    private$.messages$G3_warn)
            }

            # ---- Gate G5: indicator correlations ----
            cor_mat <- tryCatch(
                stats::cor(
                    data.frame(lapply(df[, opt$indicators, drop = FALSE], function(x)
                        as.numeric(if (is.factor(x)) as.integer(x) else x))),
                    use = "pairwise.complete.obs"),
                error = function(e) NULL)
            if (!is.null(cor_mat)) {
                off_diag <- abs(cor_mat[upper.tri(cor_mat)])
                off_diag <- off_diag[is.finite(off_diag)]
                if (length(off_diag) > 0L) {
                    rmax <- max(off_diag, na.rm = TRUE)
                    if (rmax < 0.3) {
                        private$.addNotice("WARNING",
                            "Weak inter-indicator correlations",
                            private$.messages$G5_warn(rmax))
                    }
                }
            }

            # Persist pipeline state
            private$.fitState <- list(
                df = df, n = n, k = k, n_params = n_params, cpp = cpp,
                itypes = itypes, est_spec = est_spec
            )

            # ---- Fit CFA ----
            fit <- tryCatch(
                private$.fitCFA(df, opt$indicators, est_spec),
                error = function(e) {
                    private$.addNotice("ERROR",
                        "CFA estimation failed",
                        paste0("lavaan::cfa() returned an error: ", conditionMessage(e),
                               ". This often means the indicator covariance matrix is ",
                               "non-positive-definite, or an indicator has zero variance."))
                    NULL
                })
            if (is.null(fit) || !lavaan::lavInspect(fit, "converged")) {
                if (!is.null(fit)) {
                    private$.addNotice("ERROR",
                        "CFA did not converge",
                        "The lavaan model failed to converge. Check for near-zero variance indicators or extreme collinearity.")
                }
                private$.renderNotices()
                return()
            }
            private$.fitState$fit <- fit

            # ---- Fit indices table ----
            fm <- lavaan::fitMeasures(fit,
                c("chisq", "df", "pvalue", "cfi", "tli",
                  "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
            interp <- private$.interpretFit(fm["cfi"], fm["tli"], fm["rmsea"], fm["srmr"])
            self$results$fitTable$setRow(rowNo = 1, list(
                chisq          = unname(fm["chisq"]),
                df             = unname(fm["df"]),
                chisq_p        = unname(fm["pvalue"]),
                cfi            = unname(fm["cfi"]),
                tli            = unname(fm["tli"]),
                rmsea          = unname(fm["rmsea"]),
                rmsea_lo       = unname(fm["rmsea.ci.lower"]),
                rmsea_hi       = unname(fm["rmsea.ci.upper"]),
                srmr           = unname(fm["srmr"]),
                interpretation = interp
            ))

            if (!is.na(fm["cfi"]) && (fm["cfi"] < 0.95 || fm["rmsea"] > 0.10)) {
                private$.addNotice("WARNING",
                    "Poor model fit",
                    private$.messages$FIT_poor(fm["cfi"], fm["rmsea"]))
            }

            # ---- Loadings table ----
            pe_std <- lavaan::parameterEstimates(fit, standardized = TRUE)
            load_rows <- pe_std[pe_std$op == "=~" & pe_std$lhs == "Factor", , drop = FALSE]
            loadings_table <- self$results$loadingsTable
            for (i in seq_len(nrow(load_rows))) {
                lam <- load_rows$std.all[i]
                loadings_table$addRow(rowKey = i, values = list(
                    indicator = load_rows$rhs[i],
                    est_std   = lam,
                    est       = load_rows$est[i],
                    se        = load_rows$se[i],
                    z         = load_rows$z[i],
                    pvalue    = load_rows$pvalue[i],
                    r2        = lam^2
                ))
            }

            # ---- Reliability table ----
            rel <- private$.computeReliability(load_rows$std.all)
            self$results$reliabilityTable$setRow(rowNo = 1, list(
                omega = rel$omega,
                ave   = rel$ave
            ))

            # ---- Summary table ----
            time_col  <- df[[opt$dep_time]]
            event_col <- df[[opt$dep_event]]
            n_events <- sum(as.character(event_col) == as.character(opt$event_level),
                            na.rm = TRUE)
            self$results$summaryTable$setRow(rowNo = 1, list(
                n            = n,
                n_events     = n_events,
                n_indicators = k,
                n_params     = n_params,
                cpp          = cpp,
                estimator    = est_spec$estimator,
                missing      = ifelse(is.null(est_spec$missing), "listwise",
                                      est_spec$missing)
            ))

            # ---- Extract factor scores ----
            factor_scores <- tryCatch(
                private$.extractScores(fit, opt$factor_score_method,
                                       opt$standardize_scores),
                error = function(e) NULL)
            if (is.null(factor_scores) || length(factor_scores) != n) {
                private$.addNotice("ERROR",
                    "Factor-score extraction failed",
                    "lavPredict() did not return scores of length n. This is rare; check for indicator missingness.")
                private$.renderNotices()
                return()
            }
            private$.fitState$factor_scores <- factor_scores

            # ---- Cox setup ----
            time_num  <- jmvcore::toNumeric(df[[opt$dep_time]])
            event_lvl <- as.character(opt$event_level)
            event_num <- as.integer(as.character(df[[opt$dep_event]]) == event_lvl)

            # ---- G4: events per Cox variable (soft warning) ----
            n_cox_vars <- 1L + (if (is.null(opt$adjusters)) 0L else length(opt$adjusters))
            epv <- sum(event_num, na.rm = TRUE) / n_cox_vars
            if (epv < 10) {
                private$.addNotice("WARNING",
                    "Low events-per-variable in Cox model",
                    private$.messages$G4_warn(epv))
            }

            # ---- Cox model ----
            cox_df <- data.frame(.time = time_num, .event = event_num,
                                 biomarker_factor = factor_scores)
            if (!is.null(opt$adjusters)) {
                for (av in opt$adjusters) cox_df[[av]] <- df[[av]]
            }

            rhs_terms <- c("biomarker_factor",
                           if (!is.null(opt$adjusters))
                               vapply(opt$adjusters, jmvcore::composeTerm,
                                      character(1))
                           else character(0))
            cox_formula <- stats::as.formula(
                paste0("survival::Surv(.time, .event) ~ ",
                       paste(rhs_terms, collapse = " + ")))

            cox_fit <- tryCatch(
                survival::coxph(cox_formula, data = cox_df),
                error = function(e) {
                    private$.addNotice("ERROR",
                        "Cox model failed",
                        paste0("survival::coxph() returned: ", conditionMessage(e)))
                    NULL
                })
            if (is.null(cox_fit)) {
                private$.renderNotices()
                return()
            }
            private$.fitState$cox_fit <- cox_fit
            private$.fitState$cox_df  <- cox_df

            # ---- Cox table ----
            cs <- summary(cox_fit)
            coef_mat <- cs$coefficients
            cox_table <- self$results$coxTable
            for (i in seq_len(nrow(coef_mat))) {
                cox_table$addRow(rowKey = rownames(coef_mat)[i], values = list(
                    term  = rownames(coef_mat)[i],
                    hr    = coef_mat[i, "exp(coef)"],
                    ci_lo = cs$conf.int[i, "lower .95"],
                    ci_hi = cs$conf.int[i, "upper .95"],
                    z     = coef_mat[i, "z"],
                    pvalue= coef_mat[i, "Pr(>|z|)"]
                ))
            }

            # ---- Always-on uncertainty notice ----
            private$.addNotice("INFO",
                "Measurement uncertainty not propagated",
                private$.messages$UNCERTAINTY)

            # ---- PH test (Task 9) ----
            phz <- tryCatch(survival::cox.zph(cox_fit), error = function(e) NULL)
            if (!is.null(phz)) {
                ph_table <- self$results$phTable
                for (i in seq_len(nrow(phz$table))) {
                    ph_table$addRow(rowKey = rownames(phz$table)[i], values = list(
                        term   = rownames(phz$table)[i],
                        chisq  = phz$table[i, "chisq"],
                        df     = phz$table[i, "df"],
                        pvalue = phz$table[i, "p"]
                    ))
                }
                global_p <- phz$table[nrow(phz$table), "p"]
                if (!is.na(global_p) && global_p < 0.05) {
                    private$.addNotice("WARNING",
                        "Proportional-hazards assumption violated",
                        private$.messages$PH_violated(global_p))
                }
            }

            # ---- KM state (Task 9) ----
            if (isTRUE(opt$show_plot_km)) {
                strata_n <- switch(opt$km_strata,
                                   median = 2L, tertile = 3L, quartile = 4L)
                strata <- cut(factor_scores,
                              breaks = stats::quantile(factor_scores,
                                                      probs = seq(0, 1, length.out = strata_n + 1L),
                                                      na.rm = TRUE),
                              include.lowest = TRUE,
                              labels = paste0("Q", seq_len(strata_n)))
                self$results$kmPlot$setState(list(
                    time   = time_num,
                    event  = event_num,
                    strata = strata
                ))
            }

            # Success-path render — gate failures render and return earlier
            private$.renderNotices()
        }
    )
)
