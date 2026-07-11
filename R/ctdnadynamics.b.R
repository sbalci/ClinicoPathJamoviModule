#' @title ctDNA / MRD Dynamics
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats median coef confint pnorm qnorm
#' @export

ctdnadynamicsClass <- R6::R6Class(
    "ctdnadynamicsClass",
    inherit = ctdnadynamicsBase,
    private = list(

        .init = function() {
            self$results$todo$setContent(glue::glue(
                "<h3>ctDNA / MRD Dynamics</h3>
                <p>Analyses circulating-tumour-DNA (ctDNA) / minimal-residual-disease (MRD)
                kinetics from paired <b>baseline</b> and <b>follow-up</b> variant allele
                fractions (VAF).</p>
                <p><b>Provide:</b> a baseline VAF and a follow-up VAF per patient. Samples at
                or below the <b>detection threshold</b> at follow-up are classed as
                <i>cleared / MRD-negative</i>. Optionally provide the time between draws (for a
                clearance rate) and a survival outcome (for a landmark analysis by MRD status).</p>"))
        },

        .run = function() {
            opt <- self$options
            if (is.null(opt$baselineVaf) || is.null(opt$followupVaf))
                return()

            prep <- private$.prepare()
            if (is.null(prep)) return()

            if (opt$showClassification) private$.classification(prep)
            if (opt$showDynamics)       private$.dynamics(prep)
            if (opt$showSurvival && !is.null(opt$survivalTime) &&
                !is.null(opt$survivalStatus)) private$.survival(prep)
            if (opt$showPlot)           self$results$plot$setState(prep)
            private$.writeStatus(prep)
            if (opt$showSummary)        private$.summary(prep)
            if (opt$showExplanation)    private$.explanation()
        },

        .prepare = function() {
            opt <- self$options; data <- self$data
            b <- jmvcore::toNumeric(data[[opt$baselineVaf]])
            f <- jmvcore::toNumeric(data[[opt$followupVaf]])
            thr <- opt$detectionThreshold
            keep <- !is.na(b) & !is.na(f)
            grp <- if (!is.null(opt$group)) as.factor(data[[opt$group]]) else NULL
            tb  <- if (!is.null(opt$timeBetween)) jmvcore::toNumeric(data[[opt$timeBetween]]) else NULL
            if (sum(keep) < 3) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>Need at least 3 patients with both baseline and follow-up VAF.</p>")
                return(NULL)
            }
            b <- b[keep]; f <- f[keep]
            if (!is.null(grp)) grp <- droplevels(grp[keep])
            if (!is.null(tb))  tb  <- tb[keep]
            cleared <- f <= thr                 # TRUE = cleared / MRD-negative
            eps <- 0.01
            lfc <- log2((f + eps) / (b + eps))  # log2 fold-change
            rate <- if (!is.null(tb)) (log(f + eps) - log(b + eps)) / tb else NULL
            list(b = b, f = f, thr = thr, cleared = cleared, lfc = lfc,
                 rate = rate, grp = grp, n = length(b), keep = keep)
        },

        .classification = function(p) {
            tab <- self$results$classificationTable
            nC <- sum(p$cleared); nP <- sum(!p$cleared); n <- p$n
            tab$addRow(rowKey = "cleared", values = list(
                category = "Cleared / MRD-negative", n = nC, pct = nC / n))
            tab$addRow(rowKey = "persistent", values = list(
                category = "Persistent / MRD-positive", n = nP, pct = nP / n))
            tab$addRow(rowKey = "total", values = list(
                category = "Total", n = n, pct = 1))
            tab$setNote("thr", sprintf(
                "Follow-up VAF \u2264 %g classed as cleared / MRD-negative.", p$thr))
        },

        .dynamics = function(p) {
            tab <- self$results$dynamicsTable
            fmt <- function(x) if (length(x) == 0 || all(is.na(x))) "\u2014" else sprintf("%.3f", stats::median(x, na.rm = TRUE))
            cl <- p$cleared
            tab$addRow(rowKey = "vaf_b", values = list(metric = "Median baseline VAF",
                cleared = fmt(p$b[cl]), persistent = fmt(p$b[!cl])))
            tab$addRow(rowKey = "vaf_f", values = list(metric = "Median follow-up VAF",
                cleared = fmt(p$f[cl]), persistent = fmt(p$f[!cl])))
            tab$addRow(rowKey = "lfc", values = list(metric = "Median log2 fold-change",
                cleared = fmt(p$lfc[cl]), persistent = fmt(p$lfc[!cl])))
            if (!is.null(p$rate))
                tab$addRow(rowKey = "rate", values = list(
                    metric = "Median clearance rate (log VAF / unit time)",
                    cleared = fmt(p$rate[cl]), persistent = fmt(p$rate[!cl])))
        },

        .survival = function(p) {
            opt <- self$options
            if (!requireNamespace("survival", quietly = TRUE)) {
                self$results$survivalTable$setNote("dep", "Package 'survival' is required.")
                return()
            }
            data <- self$data
            time <- jmvcore::toNumeric(data[[opt$survivalTime]])[p$keep]
            sv <- data[[opt$survivalStatus]][p$keep]
            ev <- opt$eventLevel
            if (is.factor(sv) || is.character(sv)) {
                if (is.null(ev) || ev == "") ev <- levels(as.factor(sv))[nlevels(as.factor(sv))]
                status <- as.integer(as.character(sv) == ev)
            } else {
                svn <- jmvcore::toNumeric(sv)
                status <- as.integer(svn == if (!is.null(ev) && ev != "") suppressWarnings(as.numeric(ev)) else max(svn, na.rm = TRUE))
            }
            mrd <- as.integer(!p$cleared)      # 1 = MRD-positive
            ok <- !is.na(time) & !is.na(status)
            if (sum(ok) < 5 || length(unique(mrd[ok])) < 2) {
                self$results$survivalTable$setNote("na",
                    "Not enough complete survival data with both MRD groups for a landmark analysis.")
                return()
            }
            df <- data.frame(time = time[ok], status = status[ok], mrd = mrd[ok])
            sd <- survival::survdiff(survival::Surv(time, status) ~ mrd, data = df)
            lr_p <- 1 - stats::pchisq(sd$chisq, length(sd$n) - 1)
            cox <- survival::coxph(survival::Surv(time, status) ~ mrd, data = df)
            z <- stats::qnorm(1 - (1 - opt$conf_level) / 2)
            b <- stats::coef(cox)[1]; se <- sqrt(stats::vcov(cox)[1, 1])
            hr <- exp(b); lo <- exp(b - z * se); hi <- exp(b + z * se)
            p_cox <- summary(cox)$coefficients[1, "Pr(>|z|)"]
            tab <- self$results$survivalTable
            tab$addRow(rowKey = "lr", values = list(statistic = "Log-rank test (MRD+ vs MRD-)",
                value = sprintf("chi-sq %.2f, p = %s", sd$chisq, format.pval(lr_p, digits = 3, eps = 1e-4))))
            tab$addRow(rowKey = "hr", values = list(statistic = "Hazard ratio (MRD+ vs MRD-)",
                value = sprintf("%.2f (%.0f%% CI %.2f\u2013%.2f)", hr, 100 * opt$conf_level, lo, hi)))
            tab$addRow(rowKey = "p", values = list(statistic = "Cox p-value",
                value = format.pval(p_cox, digits = 3, eps = 1e-4)))
            tab$setNote("landmark",
                "Landmark analysis: survival is compared from the MRD assessment timepoint onward by MRD status.")
        },

        .writeStatus = function(p) {
            if (!self$options$addStatusToData$isNotFilled) {
                status <- rep(NA_character_, length(p$keep))
                status[p$keep] <- ifelse(p$cleared, "Cleared", "Persistent")
                self$results$addStatusToData$setRowNums(rownames(self$data))
                self$results$addStatusToData$setValues(status)
            }
        },

        .summary = function(p) {
            nC <- sum(p$cleared); n <- p$n
            html <- glue::glue(
                "<p>Of {n} patients with paired ctDNA measurements,
                <b>{nC} ({round(100*nC/n)}%) cleared</b> to at or below the detection
                threshold (VAF \u2264 {p$thr}); {n - nC} had persistent / MRD-positive disease.
                The median log2 fold-change was {sprintf('%.2f', stats::median(p$lfc[p$cleared]))}
                in cleared patients versus {sprintf('%.2f', stats::median(p$lfc[!p$cleared]))} in
                persistent patients.</p>")
            self$results$summary$setContent(html)
        },

        .explanation = function() {
            self$results$explanation$setContent(
                "<h4>ctDNA / MRD dynamics</h4>
                <p>Circulating-tumour-DNA clearance is quantified from paired variant allele
                fractions (VAF). A follow-up sample at or below the detection threshold is
                classified as <b>cleared / MRD-negative</b>; otherwise it is
                <b>persistent / MRD-positive</b>. The log<sub>2</sub> fold-change and the
                per-unit-time clearance rate describe the depth and speed of the response.</p>
                <p>When a survival outcome is supplied, a <b>landmark analysis</b> compares
                survival from the MRD-assessment timepoint by MRD status, reported as a
                log-rank test and a Cox hazard ratio. Because MRD status is defined at a fixed
                landmark, this avoids immortal-time bias that would arise from treating clearance
                as a time-zero property.</p>")
        },

        .plot = function(image, ggtheme, theme, ...) {
            p <- image$state
            if (is.null(p)) return(FALSE)
            df <- data.frame(
                id = rep(seq_len(p$n), 2),
                timepoint = factor(rep(c("Baseline", "Follow-up"), each = p$n),
                                   levels = c("Baseline", "Follow-up")),
                vaf = c(p$b, p$f),
                status = rep(ifelse(p$cleared, "Cleared", "Persistent"), 2))
            gg <- ggplot2::ggplot(df, ggplot2::aes(x = timepoint, y = vaf,
                    group = id, colour = status)) +
                ggplot2::geom_line(alpha = 0.5) +
                ggplot2::geom_point(size = 1.6, alpha = 0.7) +
                ggplot2::geom_hline(yintercept = p$thr, linetype = "dashed", colour = "grey40") +
                ggplot2::scale_colour_manual(values = c(Cleared = "#2c7fb8", Persistent = "#de2d26")) +
                ggplot2::labs(x = NULL, y = "VAF / ctDNA level", colour = NULL,
                    title = "ctDNA trajectory (baseline \u2192 follow-up)",
                    caption = sprintf("Dashed line = detection threshold (%g)", p$thr)) +
                ggtheme + ggplot2::theme(legend.position = "bottom")
            # log scale helps when VAFs span orders of magnitude
            if (all(df$vaf >= 0) && max(df$vaf, na.rm = TRUE) / max(min(df$vaf[df$vaf > 0], na.rm = TRUE), 1e-3) > 50)
                gg <- gg + ggplot2::scale_y_log10()
            print(gg)
            TRUE
        }
    )
)
