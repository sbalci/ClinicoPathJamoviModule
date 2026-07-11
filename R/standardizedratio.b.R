#' @title Standardized Incidence / Mortality Ratio (SIR / SMR)
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats poisson.test qgamma
#' @export

standardizedratioClass <- R6::R6Class(
    "standardizedratioClass",
    inherit = standardizedratioBase,
    private = list(

        .init = function() {
            todo <- glue::glue(
                "<h3>Standardized Incidence / Mortality Ratio (SIR / SMR)</h3>
                <p>Indirect standardization compares the number of events <b>observed</b>
                in a study population with the number <b>expected</b> if the population
                had experienced reference-population rates. The ratio observed / expected
                is the SIR (incidence) or SMR (mortality).</p>
                <p><b>Provide one row per stratum with:</b></p>
                <ul>
                  <li><b>Observed events</b>, and either</li>
                  <li>a <b>reference rate</b> plus <b>person-time</b> (expected = rate x person-time), or</li>
                  <li>a column of <b>expected events</b> directly.</li>
                </ul>
                <p>Exact Poisson confidence intervals and a test against SIR/SMR = 1 are reported.</p>"
            )
            self$results$todo$setContent(todo)
        },

        .run = function() {
            opt <- self$options
            if (is.null(opt$observed)) return()

            d <- private$.prepareData()
            if (is.null(d)) return()

            private$.populateOverall(d)
            if (opt$perStratum && !is.null(d$stratum))
                private$.populateStrata(d)
            if (opt$showPlot)
                self$results$plot$setState(d)
            if (opt$showSummary)
                private$.populateSummary(d)
            if (opt$showExplanation)
                private$.populateExplanation()
        },

        .prepareData = function() {
            opt <- self$options
            O <- jmvcore::toNumeric(self$data[[opt$observed]])

            if (opt$inputMode == "rate") {
                if (is.null(opt$personTime) || is.null(opt$refRate)) {
                    self$results$todo$setContent(
                        "<p style='color:#a33'>Provide both person-time and a reference
                        rate, or switch to supplying expected events directly.</p>")
                    return(NULL)
                }
                pt <- jmvcore::toNumeric(self$data[[opt$personTime]])
                rr <- jmvcore::toNumeric(self$data[[opt$refRate]])
                E <- rr * pt
            } else {
                if (is.null(opt$expected)) {
                    self$results$todo$setContent(
                        "<p style='color:#a33'>Provide a column of expected events.</p>")
                    return(NULL)
                }
                E <- jmvcore::toNumeric(self$data[[opt$expected]])
            }

            strat <- NULL
            if (!is.null(opt$stratum))
                strat <- as.character(self$data[[opt$stratum]])

            ok <- !is.na(O) & !is.na(E)
            O <- O[ok]; E <- E[ok]
            if (!is.null(strat)) strat <- strat[ok]
            if (length(O) == 0 || sum(E) <= 0) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>No valid strata, or total expected events is zero.</p>")
                return(NULL)
            }
            list(O = O, E = E, stratum = strat, conf = opt$conf_level)
        },

        .ratioCI = function(O, E, conf) {
            # exact Poisson CI for the ratio O/E (matches stats::poisson.test)
            pt <- stats::poisson.test(round(O), T = E, conf.level = conf)
            # round(O) keeps poisson.test happy for integer counts; keep raw ratio
            list(ratio = O / E,
                 low = pt$conf.int[1], high = pt$conf.int[2], p = pt$p.value)
        },

        .populateOverall = function(d) {
            O <- sum(d$O); E <- sum(d$E)
            r <- private$.ratioCI(O, E, d$conf)
            lbl <- if (self$options$ratioType == "smr") "SMR (overall)" else "SIR (overall)"
            self$results$overallTable$addRow(rowKey = "all", values = list(
                label = lbl, observed = O, expected = E,
                ratio = r$ratio, ci_lower = r$low, ci_upper = r$high, p = r$p))
        },

        .populateStrata = function(d) {
            tab <- self$results$stratumTable
            labs <- if (is.null(d$stratum)) as.character(seq_along(d$O)) else d$stratum
            for (i in seq_along(d$O)) {
                r <- private$.ratioCI(d$O[i], d$E[i], d$conf)
                tab$addRow(rowKey = i, values = list(
                    stratum = labs[i], observed = d$O[i], expected = d$E[i],
                    ratio = r$ratio, ci_lower = r$low, ci_upper = r$high))
            }
        },

        .populateSummary = function(d) {
            O <- sum(d$O); E <- sum(d$E)
            r <- private$.ratioCI(O, E, d$conf)
            type <- if (self$options$ratioType == "smr") "SMR" else "SIR"
            dir <- if (r$low > 1) "significantly higher than expected"
                   else if (r$high < 1) "significantly lower than expected"
                   else "not significantly different from expectation"
            html <- glue::glue(
                "<p>{O} events were observed against {sprintf('%.1f', E)} expected, giving
                a <b>{type} of {sprintf('%.2f', r$ratio)}</b>
                ({sprintf('%.0f', 100*d$conf)}% CI {sprintf('%.2f', r$low)}&ndash;{sprintf('%.2f', r$high)}).
                The observed event count is <b>{dir}</b> (p = {sprintf('%.4f', r$p)}).</p>")
            self$results$summary$setContent(html)
        },

        .populateExplanation = function() {
            self$results$explanation$setContent(
                "<h4>Indirect standardization</h4>
                <p>Expected events in each stratum are the reference-population rate
                multiplied by the study population's person-time. Summing over strata gives
                the total expected count E; the standardized ratio is O / E, where O is the
                total observed count.</p>
                <p>Because event counts follow a Poisson distribution, exact Poisson
                confidence limits are used: the interval for O / E is obtained from the
                gamma-distribution relationship to the Poisson (equivalent to
                <i>stats::poisson.test</i>). An SIR/SMR above 1 indicates more events than
                expected from the reference population; below 1, fewer.</p>")
        },

        .plot = function(image, ggtheme, theme, ...) {
            d <- image$state
            if (is.null(d)) return(FALSE)
            conf <- d$conf
            rows <- list()
            labs <- if (is.null(d$stratum)) as.character(seq_along(d$O)) else d$stratum
            if (self$options$perStratum && !is.null(d$stratum)) {
                for (i in seq_along(d$O)) {
                    r <- private$.ratioCI(d$O[i], d$E[i], conf)
                    rows[[i]] <- data.frame(label = labs[i], ratio = r$ratio,
                                            low = r$low, high = r$high, overall = FALSE)
                }
            }
            O <- sum(d$O); E <- sum(d$E); ro <- private$.ratioCI(O, E, conf)
            rows[[length(rows) + 1]] <- data.frame(label = "Overall", ratio = ro$ratio,
                                                    low = ro$low, high = ro$high, overall = TRUE)
            df <- do.call(rbind, rows)
            df$label <- factor(df$label, levels = rev(df$label))
            typ <- if (self$options$ratioType == "smr") "SMR" else "SIR"
            p <- ggplot2::ggplot(df, ggplot2::aes(x = ratio, y = label,
                                                  colour = overall)) +
                ggplot2::geom_vline(xintercept = 1, linetype = "dashed",
                                    colour = "#999999") +
                ggplot2::geom_point(size = 2.6) +
                ggplot2::geom_errorbarh(ggplot2::aes(xmin = low, xmax = high),
                                        height = 0.25) +
                ggplot2::scale_colour_manual(values = c(`FALSE` = "#2c7fb8",
                                                        `TRUE` = "#d95f02"),
                                             guide = "none") +
                ggplot2::labs(x = sprintf("%s (observed / expected)", typ), y = NULL,
                              title = sprintf("Standardized %s ratio",
                                  if (typ == "SMR") "mortality" else "incidence")) +
                ggtheme
            print(p)
            TRUE
        }
    )
)
