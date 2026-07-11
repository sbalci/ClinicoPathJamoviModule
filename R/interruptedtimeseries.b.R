#' @title Interrupted Time Series Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats lm predict as.formula
#' @export

interruptedtimeseriesClass <- R6::R6Class(
    "interruptedtimeseriesClass",
    inherit = interruptedtimeseriesBase,
    private = list(

        .init = function() {
            todo <- glue::glue(
                "<h3>Interrupted Time Series Analysis</h3>
                <p>Segmented regression estimates how an intervention changed an outcome
                measured repeatedly over time. Four quantities are reported:</p>
                <ul>
                  <li><b>Baseline level</b> (intercept) and <b>baseline trend</b> (pre-intervention slope);</li>
                  <li><b>Level change</b> &mdash; the immediate jump at the intervention;</li>
                  <li><b>Trend change</b> &mdash; the change in slope afterwards.</li>
                </ul>
                <p><b>To run:</b> select a sequential <b>time variable</b>, a continuous
                <b>outcome</b>, and the <b>intervention time point</b>. Enable Newey-West
                standard errors to account for autocorrelation.</p>"
            )
            self$results$todo$setContent(todo)
        },

        .run = function() {
            opt <- self$options
            if (is.null(opt$time) || is.null(opt$outcome)) return()

            d <- private$.prepareData()
            if (is.null(d)) return()

            fit <- private$.fitITS(d)
            if (is.null(fit)) return()

            private$.populateCoef(fit)
            if (opt$showDiagnostics)
                private$.populateDiagnostics(fit)
            if (opt$predictAt != 0)
                private$.populateEffect(fit)
            if (opt$showPlot)
                self$results$plot$setState(fit)
            if (opt$showSummary)
                private$.populateSummary(fit)
            if (opt$showExplanation)
                private$.populateExplanation()
        },

        .prepareData = function() {
            opt <- self$options
            time <- jmvcore::toNumeric(self$data[[opt$time]])
            y    <- jmvcore::toNumeric(self$data[[opt$outcome]])
            ok <- !is.na(time) & !is.na(y)
            time <- time[ok]; y <- y[ok]
            o <- order(time); time <- time[o]; y <- y[o]
            if (length(y) < 8) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>At least 8 time points are recommended for
                    interrupted time series analysis.</p>")
                return(NULL)
            }
            itime <- opt$interventionTime
            if (itime <= min(time) || itime > max(time)) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>The intervention time point must fall within the
                    range of the time variable, with observations on both sides.</p>")
                return(NULL)
            }
            post <- as.integer(time >= itime)
            time_after <- pmax(0, time - itime + 1) * post
            data.frame(y = y, time = time, post = post, time_after = time_after,
                       itime = itime)
        },

        .fitITS = function(d) {
            m <- stats::lm(y ~ time + post + time_after, data = d)
            hac <- self$options$hac
            if (hac) {
                if (!requireNamespace("sandwich", quietly = TRUE) ||
                    !requireNamespace("lmtest", quietly = TRUE)) {
                    ct <- summary(m)$coefficients
                    hacUsed <- FALSE
                } else {
                    lag <- self$options$lag
                    V <- if (lag > 0)
                        sandwich::NeweyWest(m, lag = lag, prewhite = FALSE, adjust = TRUE)
                    else
                        sandwich::NeweyWest(m, prewhite = FALSE, adjust = TRUE)
                    ct <- lmtest::coeftest(m, vcov. = V)
                    hacUsed <- TRUE
                }
            } else {
                ct <- summary(m)$coefficients
                hacUsed <- FALSE
            }
            list(model = m, coef = ct, data = d, hacUsed = hacUsed,
                 itime = d$itime[1])
        },

        .termLabels = function() c(
            "(Intercept)" = "Baseline level",
            "time"        = "Baseline trend (per time unit)",
            "post"        = "Level change at intervention",
            "time_after"  = "Trend change after intervention"),

        .populateCoef = function(fit) {
            tab <- self$results$coefTable
            ct <- fit$coef
            labs <- private$.termLabels()
            for (rn in rownames(ct)) {
                tab$addRow(rowKey = rn, values = list(
                    term = if (rn %in% names(labs)) labs[[rn]] else rn,
                    estimate = ct[rn, 1], se = ct[rn, 2],
                    statistic = ct[rn, 3], p = ct[rn, 4]))
            }
            tab$setNote("note", if (fit$hacUsed)
                "Standard errors adjusted for autocorrelation (Newey-West)."
            else "Ordinary least squares standard errors (no autocorrelation adjustment).")
        },

        .populateDiagnostics = function(fit) {
            tab <- self$results$diagnostics
            if (requireNamespace("lmtest", quietly = TRUE)) {
                dw <- lmtest::dwtest(fit$model)
                tab$addRow(rowKey = "dw", values = list(
                    test = "Durbin-Watson (residual autocorrelation)",
                    statistic = unname(dw$statistic), p = dw$p.value))
            } else {
                res <- stats::residuals(fit$model)
                n <- length(res)
                dwstat <- sum(diff(res)^2) / sum(res^2)
                tab$addRow(rowKey = "dw", values = list(
                    test = "Durbin-Watson (residual autocorrelation)",
                    statistic = dwstat, p = NA))
            }
        },

        .counterfactual = function(fit, newtime) {
            # pre-intervention model prediction extrapolated (post=0, time_after=0)
            b <- stats::coef(fit$model)
            b[["(Intercept)"]] + b[["time"]] * newtime
        },

        .populateEffect = function(fit) {
            opt <- self$options
            at <- opt$predictAt
            if (at < fit$itime) return()
            b <- stats::coef(fit$model)
            time_after <- at - fit$itime + 1
            observed <- b[["(Intercept)"]] + b[["time"]] * at +
                        b[["post"]] + b[["time_after"]] * time_after
            cf <- private$.counterfactual(fit, at)
            absEffect <- observed - cf
            tab <- self$results$effectTable
            tab$addRow(rowKey = 1, values = list(
                measure = sprintf("Predicted outcome at time %g", at), value = observed))
            tab$addRow(rowKey = 2, values = list(
                measure = "Counterfactual (no intervention)", value = cf))
            tab$addRow(rowKey = 3, values = list(
                measure = "Absolute effect", value = absEffect))
            tab$addRow(rowKey = 4, values = list(
                measure = "Relative effect (%)",
                value = if (cf != 0) 100 * absEffect / cf else NA_real_))
        },

        .populateSummary = function(fit) {
            ct <- fit$coef
            lvl <- ct["post", 1]; lvlp <- ct["post", 4]
            trd <- ct["time_after", 1]; trdp <- ct["time_after", 4]
            sig <- function(p) if (is.na(p)) "" else if (p < 0.05) "statistically significant" else "not statistically significant"
            html <- glue::glue(
                "<p>At the intervention, the outcome changed in level by
                <b>{sprintf('%.3g', lvl)}</b> ({sig(lvlp)}, p = {sprintf('%.4f', lvlp)}).
                The post-intervention trend changed by <b>{sprintf('%.3g', trd)}</b> per
                time unit ({sig(trdp)}, p = {sprintf('%.4f', trdp)}) relative to the
                pre-intervention slope. {if (fit$hacUsed) 'Standard errors are
                autocorrelation-adjusted (Newey-West).' else 'Standard errors are OLS-based.'}</p>")
            self$results$summary$setContent(html)
        },

        .populateExplanation = function() {
            self$results$explanation$setContent(
                "<h4>Segmented regression for interrupted time series</h4>
                <p>The model fitted is
                <i>Y<sub>t</sub> = &beta;<sub>0</sub> + &beta;<sub>1</sub>&middot;time +
                &beta;<sub>2</sub>&middot;post + &beta;<sub>3</sub>&middot;time&nbsp;after&nbsp;intervention</i>,
                where <i>post</i> indicates the post-intervention period and
                <i>time after intervention</i> counts time points since the intervention
                (Wagner et al., 2002; Bernal et al., 2017).</p>
                <ul>
                  <li>&beta;<sub>0</sub> &mdash; baseline level;</li>
                  <li>&beta;<sub>1</sub> &mdash; baseline trend;</li>
                  <li>&beta;<sub>2</sub> &mdash; immediate change in level at the intervention;</li>
                  <li>&beta;<sub>3</sub> &mdash; change in trend after the intervention.</li>
                </ul>
                <p>Because sequential observations are usually correlated, Newey-West
                heteroscedasticity-and-autocorrelation-consistent (HAC) standard errors are
                offered, and the Durbin-Watson statistic tests for residual autocorrelation
                (values near 2 indicate little autocorrelation).</p>")
        },

        .plot = function(image, ggtheme, theme, ...) {
            fit <- image$state
            if (is.null(fit)) return(FALSE)
            d <- fit$data
            d$fitted <- stats::predict(fit$model)
            b <- stats::coef(fit$model)
            itime <- fit$itime

            p <- ggplot2::ggplot(d, ggplot2::aes(x = time, y = y)) +
                ggplot2::geom_point(colour = "#666666", size = 1.6) +
                ggplot2::geom_vline(xintercept = itime, linetype = "dashed",
                                    colour = "#de2d26")

            # fitted segments (pre and post drawn separately to show the discontinuity)
            pre  <- d[d$post == 0, ]
            post <- d[d$post == 1, ]
            p <- p +
                ggplot2::geom_line(data = pre,  ggplot2::aes(y = fitted),
                                   colour = "#2c7fb8", linewidth = 1) +
                ggplot2::geom_line(data = post, ggplot2::aes(y = fitted),
                                   colour = "#2c7fb8", linewidth = 1)

            if (self$options$counterfactual) {
                cf <- data.frame(time = post$time,
                                 y = b[["(Intercept)"]] + b[["time"]] * post$time)
                p <- p + ggplot2::geom_line(data = cf, ggplot2::aes(x = time, y = y),
                                            linetype = "dotted", colour = "#31a354",
                                            linewidth = 0.9)
            }
            p <- p +
                ggplot2::annotate("text", x = itime, y = max(d$y),
                                  label = "intervention", angle = 90, vjust = -0.4,
                                  hjust = 1, size = 3, colour = "#de2d26") +
                ggplot2::labs(x = "Time", y = "Outcome",
                              title = "Interrupted time series (segmented regression)") +
                ggtheme
            print(p)
            TRUE
        }
    )
)
