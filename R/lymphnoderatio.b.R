#' @title Lymph Node Ratio
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats complete.cases pchisq quantile median
#' @export

lymphnoderatioClass <- R6::R6Class(
    "lymphnoderatioClass",
    inherit = lymphnoderatioBase,
    private = list(

        .init = function() {
            self$results$todo$setContent(glue::glue(
                "<h3>Lymph Node Ratio (LNR)</h3>
                <p>Computes LNR = positive nodes / examined nodes per patient, checks nodal
                yield against a minimum, and relates LNR to survival.</p>
                <p><b>Provide</b> the positive-node and examined-node counts. Optionally add a
                survival time and status to compare LNR strata (Kaplan-Meier / log-rank).
                Strata can use <b>established thresholds</b> (e.g. 0.2 and 0.5) or a
                <b>data-driven optimal cutpoint</b> that maximizes log-rank separation.</p>"))
        },

        .run = function() {
            opt <- self$options
            if (is.null(opt$positiveNodes) || is.null(opt$examinedNodes)) return()

            pos <- jmvcore::toNumeric(self$data[[opt$positiveNodes]])
            exm <- jmvcore::toNumeric(self$data[[opt$examinedNodes]])
            ok <- !is.na(pos) & !is.na(exm) & exm > 0 & pos >= 0 & pos <= exm
            if (sum(ok) < 2) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>Need at least two rows with valid node counts
                    (examined &gt; 0 and 0 \u{2264} positive \u{2264} examined).</p>")
                return()
            }
            lnr <- ifelse(ok, pos / exm, NA_real_)

            if (opt$showRatioSummary)
                private$.populateRatio(lnr[ok], exm[ok])

            # stratification
            strat <- private$.stratify(lnr)
            self$results$strataTable$setVisible(opt$showStrata && !is.null(strat))

            surv <- private$.survObjects()
            if (opt$showStrata && !is.null(strat))
                private$.populateStrata(lnr, strat, surv)

            if (opt$showPlot)
                self$results$plot$setState(list(
                    lnr = lnr[ok], strat = if (!is.null(strat)) strat[ok] else NULL,
                    surv = surv, stratAll = strat))

            # write-backs
            if (self$results$addRatioToData$isNotFilled()) {
                self$results$addRatioToData$setRowNums(rownames(self$data))
                self$results$addRatioToData$setValues(lnr)
            }
            if (!is.null(strat) && self$results$addStratumToData$isNotFilled()) {
                self$results$addStratumToData$setRowNums(rownames(self$data))
                self$results$addStratumToData$setValues(as.character(strat))
            }

            if (opt$showSummary)
                private$.summary(lnr[ok], exm[ok], strat[ok])
            if (opt$showExplanation)
                private$.explain()
        },

        .parseThresholds = function() {
            s <- strsplit(self$options$thresholds, ",")[[1]]
            v <- suppressWarnings(as.numeric(trimws(s)))
            v <- v[!is.na(v) & v > 0 & v < 1]
            sort(unique(v))
        },

        .stratify = function(lnr) {
            m <- self$options$stratMethod
            if (m == "none") return(NULL)
            if (m == "fixed") {
                cuts <- private$.parseThresholds()
                if (length(cuts) == 0) return(NULL)
                brks <- c(-Inf, cuts, Inf)
                labs <- private$.rangeLabels(cuts)
                return(cut(lnr, breaks = brks, labels = labs, right = TRUE))
            }
            # optimal cutpoint (needs survival)
            surv <- private$.survObjects()
            if (is.null(surv)) return(NULL)
            cp <- private$.optimalCut(lnr, surv$time, surv$status)
            if (is.na(cp)) return(NULL)
            cut(lnr, breaks = c(-Inf, cp, Inf),
                labels = c(sprintf("LNR \u2264 %.3f", cp), sprintf("LNR > %.3f", cp)),
                right = TRUE)
        },

        .rangeLabels = function(cuts) {
            labs <- character(length(cuts) + 1)
            labs[1] <- sprintf("\u2264 %.2g", cuts[1])
            if (length(cuts) > 1)
                for (i in 2:length(cuts))
                    labs[i] <- sprintf("%.2g\u2013%.2g", cuts[i - 1], cuts[i])
            labs[length(labs)] <- sprintf("> %.2g", cuts[length(cuts)])
            labs
        },

        .optimalCut = function(lnr, time, status) {
            v <- lnr[!is.na(lnr) & lnr > 0]
            if (length(v) < 20) return(NA_real_)
            cands <- unique(round(stats::quantile(v, seq(0.15, 0.85, 0.05), names = FALSE), 4))
            best_cp <- NA_real_; best_chi <- -Inf
            for (cp in cands) {
                g <- factor(lnr > cp)
                if (nlevels(g) < 2) next
                tb <- table(g)
                if (min(tb) < 10) next
                sd <- tryCatch(survival::survdiff(survival::Surv(time, status) ~ g),
                               error = function(e) NULL)
                if (is.null(sd)) next
                if (sd$chisq > best_chi) { best_chi <- sd$chisq; best_cp <- cp }
            }
            best_cp
        },

        .survObjects = function() {
            opt <- self$options
            if (is.null(opt$survivalTime) || is.null(opt$survivalStatus)) return(NULL)
            time <- jmvcore::toNumeric(self$data[[opt$survivalTime]])
            sraw <- self$data[[opt$survivalStatus]]
            ev <- if (!is.null(opt$eventLevel) && opt$eventLevel != "") opt$eventLevel else NULL
            if (is.factor(sraw) || is.character(sraw)) {
                if (is.null(ev)) ev <- levels(as.factor(sraw))[nlevels(as.factor(sraw))]
                status <- as.integer(as.character(sraw) == ev)
            } else {
                sn <- jmvcore::toNumeric(sraw)
                status <- as.integer(sn == if (!is.null(ev)) suppressWarnings(as.numeric(ev)) else max(sn, na.rm = TRUE))
            }
            list(time = time, status = status)
        },

        .populateRatio = function(lnr, exm) {
            tab <- self$results$ratioTable
            minY <- self$options$minYield
            tab$addRow(rowKey = "n", values = list(quantity = "Patients", value = length(lnr)))
            tab$addRow(rowKey = "mean", values = list(quantity = "Mean LNR", value = mean(lnr)))
            tab$addRow(rowKey = "median", values = list(quantity = "Median LNR", value = stats::median(lnr)))
            tab$addRow(rowKey = "nodeneg", values = list(quantity = "Node-negative (LNR = 0)", value = mean(lnr == 0)))
            tab$addRow(rowKey = "medexm", values = list(quantity = "Median nodes examined", value = stats::median(exm)))
            tab$addRow(rowKey = "adeq", values = list(
                quantity = sprintf("Adequate yield (\u2265 %d)", minY), value = mean(exm >= minY)))
        },

        .populateStrata = function(lnr, strat, surv) {
            tab <- self$results$strataTable
            conf <- self$options$conf_level
            df <- data.frame(lnr = lnr, strat = strat)
            haveSurv <- !is.null(surv)
            if (haveSurv) { df$time <- surv$time; df$status <- surv$status }
            df <- df[!is.na(df$strat) & !is.na(df$lnr), , drop = FALSE]
            if (haveSurv) df <- df[stats::complete.cases(df[, c("time", "status")]), , drop = FALSE]
            df$strat <- droplevels(df$strat)

            # cox for HR vs first stratum
            hrs <- NULL
            if (haveSurv && nlevels(df$strat) >= 2) {
                cox <- tryCatch(survival::coxph(survival::Surv(time, status) ~ strat, data = df),
                                error = function(e) NULL)
                if (!is.null(cox)) {
                    sm <- summary(cox, conf.int = conf)
                    hrs <- sm$conf.int
                }
                fit <- tryCatch(survival::survfit(survival::Surv(time, status) ~ strat, data = df),
                                error = function(e) NULL)
                med <- if (!is.null(fit)) summary(fit)$table else NULL
            } else med <- NULL

            levs <- levels(df$strat)
            for (i in seq_along(levs)) {
                lv <- levs[i]; sel <- df$strat == lv
                mt <- NA_real_
                if (!is.null(med)) {
                    rn <- paste0("strat=", lv)
                    if (is.matrix(med) && rn %in% rownames(med)) mt <- med[rn, "median"]
                    else if (!is.matrix(med) && length(levs) == 1) mt <- med["median"]
                }
                hr <- NA_real_; lo <- NA_real_; hi <- NA_real_
                if (!is.null(hrs) && i > 1) {
                    rn <- paste0("strat", lv)
                    if (rn %in% rownames(hrs)) {
                        hr <- hrs[rn, "exp(coef)"]; lo <- hrs[rn, 3]; hi <- hrs[rn, 4]
                    }
                } else if (i == 1 && !is.null(hrs)) { hr <- 1 }
                tab$addRow(rowKey = lv, values = list(
                    stratum = lv, n = sum(sel), meanLNR = mean(df$lnr[sel]),
                    events = if (haveSurv) sum(df$status[sel]) else NA_integer_,
                    medianTime = mt, hr = hr, hr_lower = lo, hr_upper = hi))
            }
            if (haveSurv && nlevels(df$strat) >= 2) {
                lr <- tryCatch(survival::survdiff(survival::Surv(time, status) ~ strat, data = df),
                               error = function(e) NULL)
                if (!is.null(lr)) {
                    p <- 1 - stats::pchisq(lr$chisq, length(lr$n) - 1)
                    tab$setNote("lr", sprintf(
                        "Log-rank across strata: \u03c7\u00b2 = %.2f, df = %d, p = %s. HR relative to first stratum.",
                        lr$chisq, length(lr$n) - 1, format.pval(p, digits = 3, eps = 1e-4)))
                }
            }
        },

        .summary = function(lnr, exm, strat) {
            minY <- self$options$minYield
            self$results$summary$setContent(glue::glue(
                "<p>Median LNR was <b>{sprintf('%.3f', stats::median(lnr))}</b> across
                {length(lnr)} patients; {sprintf('%.1f%%', 100 * mean(exm >= minY))} had an
                adequate nodal yield (\u2265 {minY} examined). {sprintf('%.1f%%', 100 * mean(lnr == 0))}
                were node-negative.</p>"))
        },

        .explain = function() {
            self$results$explanation$setContent(
                "<h4>Lymph node ratio</h4>
                <p>LNR is the number of metastatic (positive) lymph nodes divided by the total
                number examined. Because it is a proportion, it is less sensitive than absolute
                positive-node counts to variation in surgical harvest and pathological workup,
                and it has repeatedly been shown to be independently prognostic across
                colorectal, gastric, breast, and head-and-neck cancers.</p>
                <p>Nodal-yield adequacy compares the examined-node count to an accepted minimum
                (e.g. 12 nodes in colorectal resection). For prognosis, LNR is stratified either
                by established category thresholds or by a data-driven optimal cutpoint chosen to
                maximize the log-rank statistic; strata are then compared by Kaplan-Meier /
                log-rank testing, with Cox hazard ratios relative to the lowest stratum. A
                data-driven cutpoint is optimistic and should be validated externally.</p>")
        },

        .plot = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            # survival curves if strata + survival available, else LNR distribution
            if (!is.null(st$surv) && !is.null(st$stratAll)) {
                df <- data.frame(time = st$surv$time, status = st$surv$status, strat = st$stratAll)
                df <- df[stats::complete.cases(df) & !is.na(df$strat), , drop = FALSE]
                df$strat <- droplevels(df$strat)
                fit <- survival::survfit(survival::Surv(time, status) ~ strat, data = df)
                sf <- data.frame(time = fit$time, surv = fit$surv,
                                 strat = rep(names(fit$strata), fit$strata))
                sf$strat <- sub("strat=", "", sf$strat)
                p <- ggplot2::ggplot(sf, ggplot2::aes(x = time, y = surv, colour = strat)) +
                    ggplot2::geom_step(linewidth = 0.9) +
                    ggplot2::scale_y_continuous(limits = c(0, 1)) +
                    ggplot2::labs(x = "Time", y = "Survival probability", colour = "LNR stratum",
                                  title = "Survival by LNR stratum") +
                    ggtheme + ggplot2::theme(legend.position = "bottom")
            } else {
                df <- data.frame(lnr = st$lnr)
                p <- ggplot2::ggplot(df, ggplot2::aes(x = lnr)) +
                    ggplot2::geom_histogram(bins = 30, fill = "#2c7fb8", colour = "white") +
                    ggplot2::labs(x = "Lymph node ratio", y = "Patients",
                                  title = "Distribution of LNR") +
                    ggtheme
            }
            print(p)
            TRUE
        }
    )
)
