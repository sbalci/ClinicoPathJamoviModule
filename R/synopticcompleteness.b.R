#' @title Synoptic Report Completeness
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats lm coef
#' @export

synopticcompletenessClass <- R6::R6Class(
    "synopticcompletenessClass",
    inherit = synopticcompletenessBase,
    private = list(

        .init = function() {
            self$results$todo$setContent(glue::glue(
                "<h3>Synoptic Report Completeness</h3>
                <p>Audits how completely structured / synoptic pathology reports capture the
                required data elements &mdash; a laboratory-quality metric increasingly required
                for accreditation.</p>
                <p><b>Provide</b> the required data elements as variables (one per element). Each
                is counted as <i>present</i> for a report when it is non-missing (or equals a
                specified present value). Reports per-report, per-element and by-group
                completeness, and an optional trend over time.</p>"))
        },

        .run = function() {
            opt <- self$options
            if (is.null(opt$items) || length(opt$items) == 0) return()
            prep <- private$.prepare()
            if (is.null(prep)) return()

            if (opt$showOverall) private$.overall(prep)
            if (opt$showPerItem) private$.perItem(prep)
            if (opt$showByGroup && !is.null(opt$group)) private$.byGroup(prep)
            if (opt$showTrend && !is.null(opt$timeVar)) private$.trend(prep)
            if (opt$showPlot) self$results$plot$setState(prep)
            private$.writeCompleteness(prep)
            if (opt$showSummary) private$.summary(prep)
            if (opt$showExplanation) private$.explanation()
        },

        .isPresent = function(col) {
            # returns logical vector: TRUE where the element is present
            if (self$options$presenceRule == "value") {
                pv <- self$options$presentValue
                if (is.null(pv) || pv == "")
                    return(!is.na(col))
                return(!is.na(col) & as.character(col) == pv)
            }
            # non-missing rule: not NA and, for character, not blank / explicit absence
            present <- !is.na(col)
            if (is.character(col) || is.factor(col)) {
                cc <- trimws(as.character(col))
                present <- present & !(cc %in% c("", "NA", "N/A", "absent", "Absent",
                                                 "not reported", "Not reported", "NR"))
            }
            present
        },

        .prepare = function() {
            opt <- self$options; data <- self$data
            items <- opt$items
            pres <- sapply(items, function(v) private$.isPresent(data[[v]]))
            if (is.null(dim(pres))) pres <- matrix(pres, ncol = length(items))
            colnames(pres) <- items
            ni <- length(items)
            report_comp <- rowMeans(pres)            # fraction present per report
            thr <- opt$completeThreshold / 100
            complete <- report_comp >= thr - 1e-9
            grp <- if (!is.null(opt$group)) as.factor(data[[opt$group]]) else NULL
            tv  <- if (!is.null(opt$timeVar)) jmvcore::toNumeric(data[[opt$timeVar]]) else NULL
            list(pres = pres, ni = ni, nr = nrow(pres), report_comp = report_comp,
                 complete = complete, thr = thr, grp = grp, tv = tv, items = items)
        },

        .overall = function(p) {
            tab <- self$results$overallTable
            tab$addRow(rowKey = "nr", values = list(metric = "Reports audited",
                value = as.character(p$nr)))
            tab$addRow(rowKey = "ni", values = list(metric = "Required elements",
                value = as.character(p$ni)))
            tab$addRow(rowKey = "mean", values = list(metric = "Mean completeness",
                value = sprintf("%.1f%%", 100 * mean(p$report_comp))))
            tab$addRow(rowKey = "full", values = list(
                metric = sprintf("Reports \u2265 %.0f%% complete", 100 * p$thr),
                value = sprintf("%d (%.0f%%)", sum(p$complete), 100 * mean(p$complete))))
        },

        .perItem = function(p) {
            tab <- self$results$perItemTable
            ic <- colMeans(p$pres)
            ord <- order(ic)                        # worst first
            for (j in ord) {
                tab$addRow(rowKey = p$items[j], values = list(
                    element = p$items[j],
                    present = sum(p$pres[, j]),
                    pct = ic[j]))
            }
            tab$setNote("order", "Elements ordered from least to most complete (worst first).")
        },

        .byGroup = function(p) {
            tab <- self$results$byGroupTable
            g <- droplevels(p$grp)
            for (lev in levels(g)) {
                idx <- which(g == lev)
                tab$addRow(rowKey = lev, values = list(
                    grp = lev, n = length(idx),
                    meanComp = mean(p$report_comp[idx]),
                    fullyComplete = mean(p$complete[idx])))
            }
        },

        .trend = function(p) {
            tab <- self$results$trendTable
            ok <- !is.na(p$tv)
            if (sum(ok) < 3 || length(unique(p$tv[ok])) < 3) {
                tab$setNote("na", "Need at least 3 distinct time points for a trend.")
                return()
            }
            df <- data.frame(t = p$tv[ok], comp = p$report_comp[ok])
            m <- stats::lm(comp ~ t, data = df)
            slope <- stats::coef(m)[2]
            p_slope <- summary(m)$coefficients[2, 4]
            tab$addRow(rowKey = "slope", values = list(
                statistic = "Change in completeness per unit time",
                value = sprintf("%+.2f%% (p = %s)", 100 * slope,
                                format.pval(p_slope, digits = 3, eps = 1e-4))))
            tab$addRow(rowKey = "dir", values = list(
                statistic = "Direction",
                value = if (p_slope >= 0.05) "No significant trend"
                        else if (slope > 0) "Improving" else "Declining"))
            tab$setNote("its", "For a formal pre/post change analysis, pair with the Interrupted Time Series analysis.")
        },

        .writeCompleteness = function(p) {
            if (!self$options$addCompletenessToData$isNotFilled) {
                self$results$addCompletenessToData$setRowNums(rownames(self$data))
                self$results$addCompletenessToData$setValues(round(100 * p$report_comp, 1))
            }
        },

        .summary = function(p) {
            worst <- p$items[which.min(colMeans(p$pres))]
            html <- glue::glue(
                "<p>Across <b>{p$nr}</b> reports audited against <b>{p$ni}</b> required elements,
                mean completeness was <b>{sprintf('%.1f%%', 100*mean(p$report_comp))}</b>, and
                <b>{sum(p$complete)} ({round(100*mean(p$complete))}%)</b> reports met the
                \u2265{round(100*p$thr)}% completeness threshold. The least-completed element was
                <b>{worst}</b> ({sprintf('%.0f%%', 100*min(colMeans(p$pres)))}).</p>")
            self$results$summary$setContent(html)
        },

        .explanation = function() {
            self$results$explanation$setContent(
                "<h4>Synoptic-report completeness auditing</h4>
                <p>Structured (synoptic) reporting standards such as CAP and ICCR specify a set of
                required data elements. Completeness auditing measures the proportion of those
                elements actually populated &mdash; per report, per element (to find the weakest
                fields), and by subspecialty or reporting pathologist &mdash; and tracks it over
                time. Each element is judged present when it is non-missing (or equals a
                specified present value).</p>
                <p>The completeness trend estimates the change per unit time by linear regression;
                for a formal evaluation of a protocol change (before vs after an intervention),
                pair this with the Interrupted Time Series analysis.</p>")
        },

        .plot = function(image, ggtheme, theme, ...) {
            p <- image$state
            if (is.null(p)) return(FALSE)
            # If a time variable is present, show the completeness trend; else per-element bars.
            if (!is.null(p$tv) && sum(!is.na(p$tv)) >= 3 &&
                length(unique(p$tv[!is.na(p$tv)])) >= 3) {
                ok <- !is.na(p$tv)
                agg <- tapply(p$report_comp[ok], p$tv[ok], mean)
                df <- data.frame(t = as.numeric(names(agg)), comp = 100 * as.numeric(agg))
                gg <- ggplot2::ggplot(df, ggplot2::aes(x = t, y = comp)) +
                    ggplot2::geom_line(colour = "#2c7fb8", linewidth = 0.9) +
                    ggplot2::geom_point(colour = "#2c7fb8", size = 2) +
                    ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
                                         colour = "grey40", formula = y ~ x) +
                    ggplot2::scale_y_continuous(limits = c(0, 100)) +
                    ggplot2::labs(x = "Time / period", y = "Mean completeness (%)",
                        title = "Synoptic completeness over time") + ggtheme
            } else {
                ic <- sort(colMeans(p$pres))
                df <- data.frame(element = factor(names(ic), levels = names(ic)),
                                 pct = 100 * as.numeric(ic))
                gg <- ggplot2::ggplot(df, ggplot2::aes(x = element, y = pct)) +
                    ggplot2::geom_col(fill = "#2c7fb8", width = 0.7) +
                    ggplot2::geom_hline(yintercept = 100, linetype = "dashed", colour = "grey60") +
                    ggplot2::coord_flip() +
                    ggplot2::scale_y_continuous(limits = c(0, 100)) +
                    ggplot2::labs(x = NULL, y = "Completeness (%)",
                        title = "Per-element completeness") + ggtheme
            }
            print(gg)
            TRUE
        }
    )
)
