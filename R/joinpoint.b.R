#' @title Joinpoint Trend Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats lm coef pt
#' @export

joinpointClass <- R6::R6Class(
    "joinpointClass",
    inherit = joinpointBase,
    private = list(

        .init = function() {
            todo <- glue::glue(
                "<h3>Joinpoint Trend Analysis</h3>
                <p>Joinpoint regression fits connected log-linear segments to a series of
                rates over time and identifies the points where the trend changes. Each
                segment is summarized by its <b>Annual Percent Change (APC)</b>; the whole
                period by the <b>Average Annual Percent Change (AAPC)</b>.</p>
                <p><b>Provide:</b> a <b>time variable</b> (e.g. year) and a positive
                <b>rate</b> variable. The number of joinpoints up to the chosen maximum is
                selected automatically using the Bayesian Information Criterion.</p>"
            )
            self$results$todo$setContent(todo)
        },

        .run = function() {
            opt <- self$options
            if (is.null(opt$time) || is.null(opt$rate)) return()

            d <- private$.prepareData()
            if (is.null(d)) return()

            fit <- private$.fitJoinpoint(d)
            if (is.null(fit)) return()

            private$.populateJoinpoints(fit)
            if (opt$showSegments)
                private$.populateSegments(fit, d)
            if (opt$showAAPC)
                private$.populateAAPC(fit, d)
            if (opt$showPlot)
                self$results$plot$setState(list(d = d, fit = fit))
            if (opt$showSummary)
                private$.populateSummary(fit, d)
            if (opt$showExplanation)
                private$.populateExplanation()
        },

        .prepareData = function() {
            opt <- self$options
            time <- jmvcore::toNumeric(self$data[[opt$time]])
            rate <- jmvcore::toNumeric(self$data[[opt$rate]])
            ok <- !is.na(time) & !is.na(rate)
            time <- time[ok]; rate <- rate[ok]
            o <- order(time); time <- time[o]; rate <- rate[o]
            if (any(rate <= 0)) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>All rate values must be positive (the model is
                    log-linear).</p>")
                return(NULL)
            }
            if (length(rate) < 7) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>At least 7 time points are recommended for
                    joinpoint analysis.</p>")
                return(NULL)
            }
            list(time = time, rate = rate, conf = opt$conf_level)
        },

        .fitJoinpoint = function(d) {
            if (!requireNamespace("segmented", quietly = TRUE)) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>The 'segmented' package is required.</p>")
                return(NULL)
            }
            df <- data.frame(time = d$time, y = log(d$rate))
            lm0 <- stats::lm(y ~ time, data = df)
            kmax <- self$options$maxJoinpoints
            fit <- NULL
            if (kmax > 0) {
                fit <- tryCatch(
                    suppressWarnings(segmented::selgmented(
                        lm0, seg.Z = ~time, Kmax = kmax, type = "bic", msg = FALSE)),
                    error = function(e) NULL)
            }
            is_seg <- !is.null(fit) && inherits(fit, "segmented") &&
                      !is.null(fit$psi) && all(is.finite(fit$psi[, "Est."]))
            if (!is_seg) {
                # no joinpoints: single log-linear slope
                cf <- summary(lm0)$coefficients["time", ]
                z <- stats::qt(1 - (1 - d$conf) / 2, df = lm0$df.residual)
                return(list(
                    model = lm0, njp = 0L, joinpoints = numeric(0),
                    slopes = matrix(c(cf[1], cf[2], cf[1] - z * cf[2],
                                      cf[1] + z * cf[2], cf[4]), nrow = 1,
                                    dimnames = list("slope1",
                                        c("Est", "SE", "lo", "hi", "p"))),
                    conf = d$conf))
            }
            sl <- segmented::slope(fit, conf.level = d$conf)$time
            # column names in segmented's slope() output: "Est.", "St.Err.",
            # "t value", "CI(..).l", "CI(..).u" (match by position/pattern to be robust)
            estCol <- 1L; seCol <- 2L
            loCol <- grep("\\.l$", colnames(sl))[1]
            hiCol <- grep("\\.u$", colnames(sl))[1]
            pvals <- 2 * stats::pt(-abs(sl[, estCol] / sl[, seCol]),
                                   df = fit$df.residual)
            slopes <- cbind(Est = sl[, estCol], SE = sl[, seCol],
                            lo = sl[, loCol], hi = sl[, hiCol], p = pvals)
            list(model = fit, njp = nrow(fit$psi),
                 joinpoints = fit$psi[, "Est."], slopes = slopes, conf = d$conf)
        },

        .apc = function(x) 100 * (exp(x) - 1),

        .populateJoinpoints = function(fit) {
            tab <- self$results$joinpointTable
            if (fit$njp == 0) {
                tab$addRow(rowKey = 0, values = list(idx = 0, location = NA))
                tab$setNote("none", "No joinpoint selected; a single trend fits the series.")
                return()
            }
            for (i in seq_len(fit$njp))
                tab$addRow(rowKey = i, values = list(
                    idx = i, location = fit$joinpoints[i]))
        },

        .populateSegments = function(fit, d) {
            tab <- self$results$segmentTable
            sl <- fit$slopes
            bnds <- c(min(d$time), fit$joinpoints, max(d$time))
            for (i in seq_len(nrow(sl))) {
                seglab <- sprintf("%.0f - %.0f", bnds[i], bnds[i + 1])
                tab$addRow(rowKey = i, values = list(
                    segment = seglab,
                    apc = private$.apc(sl[i, "Est"]),
                    ci_lower = private$.apc(sl[i, "lo"]),
                    ci_upper = private$.apc(sl[i, "hi"]),
                    p = sl[i, "p"]))
            }
        },

        .populateAAPC = function(fit, d) {
            sl <- fit$slopes
            bnds <- c(min(d$time), fit$joinpoints, max(d$time))
            w <- diff(bnds)                       # segment durations
            w <- w / sum(w)
            aapc_slope <- sum(w * sl[, "Est"])
            se <- sqrt(sum((w^2) * (sl[, "SE"]^2)))  # segments treated independently
            z <- stats::qnorm(1 - (1 - d$conf) / 2)
            self$results$aapcTable$addRow(rowKey = "aapc", values = list(
                label = sprintf("AAPC (%.0f - %.0f)", min(d$time), max(d$time)),
                aapc = private$.apc(aapc_slope),
                ci_lower = private$.apc(aapc_slope - z * se),
                ci_upper = private$.apc(aapc_slope + z * se)))
        },

        .populateSummary = function(fit, d) {
            if (fit$njp == 0) {
                apc <- private$.apc(fit$slopes[1, "Est"])
                dir <- if (apc > 0) "increased" else "decreased"
                self$results$summary$setContent(glue::glue(
                    "<p>No change in trend was detected. Over {min(d$time)}&ndash;{max(d$time)}
                    the rate {dir} by an average of <b>{sprintf('%.2f', abs(apc))}% per
                    period</b>.</p>"))
                return()
            }
            jps <- paste(sprintf("%.0f", fit$joinpoints), collapse = ", ")
            sl <- fit$slopes
            last_apc <- private$.apc(sl[nrow(sl), "Est"])
            dir <- if (last_apc > 0) "rising" else "falling"
            self$results$summary$setContent(glue::glue(
                "<p><b>{fit$njp}</b> joinpoint(s) were detected (at {jps}). The most recent
                segment shows a <b>{dir}</b> trend with an annual percent change of
                <b>{sprintf('%.2f', last_apc)}%</b>. See the segment table for the APC of
                each period.</p>"))
        },

        .populateExplanation = function() {
            self$results$explanation$setContent(
                "<h4>Joinpoint regression</h4>
                <p>The (natural) logarithm of the rate is modelled as a continuous
                piecewise-linear function of time. Each linear segment corresponds to a
                constant <b>annual percent change</b>: APC = 100 x (exp(slope) - 1). Points
                where the slope changes are <b>joinpoints</b> (Kim et al., 2000).</p>
                <p>The number of joinpoints up to the chosen maximum is selected using the
                Bayesian Information Criterion. The <b>Average Annual Percent Change
                (AAPC)</b> is a duration-weighted average of the segment slopes across the
                whole period, back-transformed to a percent change.</p>
                <p>Note: this implementation uses BIC-based selection (via the
                <i>segmented</i> package) rather than the permutation-test selection of the
                NCI Joinpoint software, so the chosen number of joinpoints may differ
                slightly for borderline cases.</p>")
        },

        .plot = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            d <- st$d; fit <- st$fit
            df <- data.frame(time = d$time, rate = d$rate)
            df$fitted <- exp(stats::predict(fit$model))
            p <- ggplot2::ggplot(df, ggplot2::aes(x = time)) +
                ggplot2::geom_point(ggplot2::aes(y = rate), colour = "#666666",
                                    size = 1.8) +
                ggplot2::geom_line(ggplot2::aes(y = fitted), colour = "#2c7fb8",
                                   linewidth = 1)
            if (fit$njp > 0)
                p <- p + ggplot2::geom_vline(xintercept = fit$joinpoints,
                                             linetype = "dashed", colour = "#de2d26")
            if (self$options$logScale)
                p <- p + ggplot2::scale_y_log10()
            p <- p + ggplot2::labs(x = "Time", y = "Rate",
                                   title = "Joinpoint trend (log-linear segments)") +
                ggtheme
            print(p)
            TRUE
        }
    )
)
