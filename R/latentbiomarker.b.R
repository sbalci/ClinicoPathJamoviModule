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

        # ---- Pipeline ----
        .run = function() {
            private$.noticeList <- list()
            opt <- self$options

            # Silent on incomplete input
            if (is.null(opt$dep_time) || is.null(opt$dep_event) ||
                is.null(opt$indicators) || length(opt$indicators) == 0) {
                return()
            }

            # Gate G6 — reflective confirmation
            if (!isTRUE(opt$reflective_confirmed)) {
                private$.addNotice("ERROR",
                    "Reflective-measurement confirmation required",
                    paste0(
                        "CFA assumes indicators reflect an underlying latent construct ",
                        "(e.g., CD8/PD-L1/TIL all reflect 'immune activation'). If indicators ",
                        "constitute a composite where each adds independent information ",
                        "(e.g., a histologic grade summing nuclear grade + tubules + mitoses), ",
                        "CFA gives misleading results. Use cSEM or seminr for formative models, ",
                        "or compute the composite directly. Tick the confirmation box if your ",
                        "model is genuinely reflective."))
                private$.renderNotices()
                return()
            }

            # Subsequent gates and computation added in later tasks
            # Success-path render — gate failures render and return earlier
            private$.renderNotices()
        }
    )
)
