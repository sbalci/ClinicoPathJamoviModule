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

        # ---- Pipeline ----
        .run = function() {
            private$.noticeList <- list()
            opt <- self$options

            # Silent on incomplete input
            if (is.null(opt$dep_time) || is.null(opt$dep_event) ||
                is.null(opt$indicators) || length(opt$indicators) == 0) {
                return()
            }

            # ---- Gate G6: reflective confirmation (hard refusal, runs first) ----
            if (!isTRUE(opt$reflective_confirmed)) {
                private$.addNotice("ERROR",
                    "Reflective-measurement confirmation required",
                    private$.messages$G6_refuse)
                private$.renderNotices()
                return()
            }

            # Pull data using jmvcore conventions
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

            # Subsequent gates and computation in later tasks
            # Success-path render — gate failures render and return earlier
            private$.renderNotices()
        }
    )
)
