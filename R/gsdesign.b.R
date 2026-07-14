#' Group-Sequential Design & Sample Size — backend
#'
#' Inspired by the Jamovi-TrialPlots module by highwind
#' (https://github.com/highwindmx/Jamovi-TrialPlots), released under LGPL, which
#' uses gsDesign2 for a survival design. This is an independent re-implementation
#' for ClinicoPath (GPL-2) built on the CRAN gold-standard gsDesign package,
#' extended to survival, binary, and continuous endpoints.
#'
#' @importFrom R6 R6Class
gsdesignClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "gsdesignClass",
    inherit = gsdesignBase,
    private = list(

        .run = function() {
            x <- tryCatch(private$.buildDesign(), error = function(e) {
                self$results$summary$setContent(private$.errHtml(conditionMessage(e)))
                NULL
            })
            if (is.null(x)) return()

            private$.fillBoundaryTable(x)
            self$results$summary$setContent(private$.summaryHtml(x))
            # The gsDesign object holds spending-function closures and is not
            # cleanly serializable; the plot recomputes it from options instead.
            self$results$boundaryPlot$setState(list(ready = TRUE))
        },

        .buildDesign = function() {
            opt <- self$options
            if (!(opt$alpha > 0 && opt$alpha < 1)) stop("alpha must be between 0 and 1")
            if (!(opt$power > 0 && opt$power < 1)) stop("power must be between 0 and 1")
            if (opt$kMax < 1) stop("number of analyses must be at least 1")

            alpha1 <- if (identical(opt$sided, "2")) opt$alpha / 2 else opt$alpha
            beta <- 1 - opt$power
            test.type <- if (identical(opt$testType, "efffut")) 4 else 1

            sfu <- switch(opt$sfu,
                OF     = gsDesign::sfLDOF,
                Pocock = gsDesign::sfLDPocock,
                HSD    = gsDesign::sfHSD,
                WT     = "WT")
            timing <- private$.parseTiming(opt$timing, opt$kMax)

            common <- list(k = opt$kMax, test.type = test.type,
                           alpha = alpha1, beta = beta, sfu = sfu, timing = timing)
            if (opt$sfu %in% c("HSD", "WT")) common$sfupar <- opt$sfupar

            if (opt$endpoint == "survival") {
                args <- c(common, list(
                    lambdaC = log(2) / opt$medianControl,
                    hr = opt$hr,
                    eta = -log(1 - opt$dropoutRate) / 12,
                    T = opt$accrualDuration + opt$followupDuration,
                    minfup = opt$followupDuration,
                    ratio = opt$ratio))
                x <- do.call(gsDesign::gsSurv, args)
                x$.effectScale <- "HR"
            } else if (opt$endpoint == "binary") {
                nfix <- gsDesign::nBinomial(p1 = opt$p1, p2 = opt$p2,
                          alpha = alpha1, beta = beta, ratio = opt$ratio)
                args <- c(common, list(n.fix = nfix))
                x <- do.call(gsDesign::gsDesign, args)
                x$.effectScale <- "risk difference"
            } else {
                d <- opt$deltaMean / opt$stdDev
                za <- stats::qnorm(1 - alpha1); zb <- stats::qnorm(opt$power)
                nfix_per_group <- (za + zb)^2 / d^2
                nfix <- ceiling(nfix_per_group * (1 + opt$ratio))  # total, allocation-adjusted
                args <- c(common, list(n.fix = nfix))
                x <- do.call(gsDesign::gsDesign, args)
                x$.effectScale <- "std. effect size"
            }
            x$.endpoint <- opt$endpoint
            x$.ratio <- opt$ratio
            x
        },

        .parseTiming = function(s, k) {
            s <- trimws(s)
            if (nchar(s) == 0) return(1)  # gsDesign: 1 = equal spacing
            v <- suppressWarnings(as.numeric(strsplit(s, ",")[[1]]))
            v <- v[!is.na(v)]
            if (length(v) == 0) return(1)
            v
        },

        # effect-scale efficacy boundary (survival only; NA for other endpoints)
        .effBoundary = function(x) {
            if (!identical(x$.endpoint, "survival")) return(rep(NA_real_, x$k))
            r <- x$.ratio
            exp(-x$upper$bound * (1 + r) / sqrt(r * x$n.I))
        },

        .fillBoundaryTable = function(x) {
            tbl <- self$results$boundaryTable
            k <- x$k
            zeff <- x$upper$bound
            pnom <- stats::pnorm(-zeff)           # one-sided nominal p at each look
            cumAlpha <- cumsum(x$upper$spend)
            effbound <- private$.effBoundary(x)
            for (i in seq_len(k)) {
                tbl$addRow(rowKey = i, values = list(
                    analysis = i,
                    infoFrac = x$timing[i],
                    n        = x$n.I[i],
                    zBound   = zeff[i],
                    pNominal = pnom[i],
                    effBound = effbound[i],
                    cumAlpha = cumAlpha[i]
                ))
            }
        },

        .plot = function(image, ...) {
            if (is.null(image$state)) return(FALSE)
            # recompute the design from options (state cannot hold the object)
            x <- tryCatch(private$.buildDesign(), error = function(e) NULL)
            if (is.null(x)) return(FALSE)
            p <- tryCatch(plot(x), error = function(e) NULL)
            if (is.null(p) || !inherits(p, "ggplot")) {
                # fallback: manual Z-boundary plot
                df <- data.frame(info = x$timing, z = x$upper$bound)
                p <- ggplot2::ggplot(df, ggplot2::aes(x = info, y = z)) +
                    ggplot2::geom_line() + ggplot2::geom_point(size = 2) +
                    ggplot2::labs(x = "Information fraction", y = "Efficacy Z boundary") +
                    ggplot2::theme_classic()
            }
            print(p)
            TRUE
        },

        .summaryHtml = function(x) {
            opt <- self$options
            bs <- tryCatch(
                paste(utils::capture.output(print(gsDesign::gsBoundSummary(x))), collapse = "<br>"),
                error = function(e) "")
            max_events <- ceiling(max(x$n.I))
            if (identical(opt$endpoint, "survival")) {
                max_n <- tryCatch(ceiling(max(x$eNC + x$eNE)), error = function(e) NA_real_)
                size_line <- paste0(.("Max sample size"), ": ", max_n, " | ",
                                    .("max events"), ": ", max_events)
            } else {
                size_line <- paste0(.("Max sample size"), ": ", max_events)
            }
            alpha1 <- if (identical(opt$sided, "2")) opt$alpha / 2 else opt$alpha
            paste0(
                "<div style='padding:8px;font-family:monospace;'>",
                "<b>", .("Group-Sequential Design"), "</b><br>",
                .("Endpoint"), ": ", opt$endpoint, " | ", .("effect scale"), ": ", x$.effectScale, "<br>",
                .("Analyses"), ": ", x$k, " | ", .("spending"), ": ", opt$sfu, " | ",
                .("boundaries"), ": ", opt$testType, "<br>",
                .("Alpha"), ": ", signif(opt$alpha, 3), " (", opt$sided, "-sided; 1-sided = ",
                signif(alpha1, 3), ") | ", .("Power"), ": ", opt$power, "<br>",
                size_line, "<br><br>",
                bs,
                "<br><i>", .("Inspired by the Jamovi-TrialPlots module by highwind (github.com/highwindmx/Jamovi-TrialPlots)."), "</i>",
                "</div>"
            )
        },

        .errHtml = function(msg) {
            paste0("<div style='padding:8px;color:#8a1f11;'><b>",
                   .("Design could not be computed"), ":</b> ",
                   jmvcore::htmlEscape(msg), "</div>")
        }
    )
)
