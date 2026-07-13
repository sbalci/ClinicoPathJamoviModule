#' @title Residual Cancer Burden (RCB)
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats complete.cases
#' @export

residualcancerburdenClass <- R6::R6Class(
    "residualcancerburdenClass",
    inherit = residualcancerburdenBase,
    private = list(

        .init = function() {
            todo <- glue::glue(
                "<h3>Residual Cancer Burden (RCB)</h3>
                <p>Computes the RCB index and class (Symmans et al., 2007) from the standard
                post-neoadjuvant pathology variables.</p>
                <p><b>Cohort mode</b> \u2014 map the six variables: primary tumour-bed
                dimensions (d1, d2 in mm), overall cancer cellularity (%), the in-situ
                fraction (%, optional), the number of positive nodes, and the largest nodal
                metastasis (mm, optional). <b>Single-case mode</b> \u2014 enter the values
                directly.</p>
                <p>The continuous index is classified as <b>RCB-0</b> (pCR),
                <b>RCB-I</b> (minimal, \u{2264}1.36), <b>RCB-II</b> (moderate, 1.36\u{2013}3.28),
                or <b>RCB-III</b> (extensive, &gt;3.28).</p>"
            )
            self$results$todo$setContent(todo)
        },

        # ---- RCB formula (Symmans 2007) -------------------------------------
        .rcbIndex = function(d1, d2, ca_pct, cis_pct, LN, dmet) {
            d1 <- ifelse(is.na(d1), 0, d1); d2 <- ifelse(is.na(d2), 0, d2)
            ca_pct <- ifelse(is.na(ca_pct), 0, ca_pct)
            cis_pct <- ifelse(is.na(cis_pct), 0, cis_pct)
            LN <- ifelse(is.na(LN), 0, LN); dmet <- ifelse(is.na(dmet), 0, dmet)

            dprim <- sqrt(d1 * d2)
            finv  <- (1 - cis_pct / 100) * (ca_pct / 100)
            finv  <- pmax(finv, 0)
            prim_base <- finv * dprim
            prim_term <- ifelse(prim_base <= 0, 0, 1.4 * prim_base^0.17)
            met_inner <- 4 * (1 - 0.75^LN) * dmet
            met_term  <- ifelse(met_inner <= 0, 0, met_inner^0.17)
            prim_term + met_term
        },

        .rcbClass = function(rcb) {
            cls <- character(length(rcb))
            cls[rcb == 0]                 <- "RCB-0 (pCR)"
            cls[rcb > 0 & rcb <= 1.36]    <- "RCB-I"
            cls[rcb > 1.36 & rcb <= 3.28] <- "RCB-II"
            cls[rcb > 3.28]               <- "RCB-III"
            factor(cls, levels = c("RCB-0 (pCR)", "RCB-I", "RCB-II", "RCB-III"))
        },

        .run = function() {
            if (self$options$inputMode == "manual") {
                private$.runManual()
            } else {
                private$.runCohort()
            }
        },

        # ---- single case ----------------------------------------------------
        .runManual = function() {
            opt <- self$options
            idx <- private$.rcbIndex(opt$mD1, opt$mD2, opt$mCellularity,
                                     opt$mCis, opt$mNodes, opt$mMetSize)
            cls <- as.character(private$.rcbClass(idx))
            tab <- self$results$caseTable
            tab$addRow(rowKey = "dprim", values = list(
                quantity = "Primary bed geometric mean (mm)", value = sqrt(opt$mD1 * opt$mD2)))
            tab$addRow(rowKey = "finv", values = list(
                quantity = "Invasive fraction (finv)",
                value = pmax((1 - opt$mCis / 100) * (opt$mCellularity / 100), 0)))
            tab$addRow(rowKey = "idx", values = list(
                quantity = "RCB index", value = idx))
            tab$setNote("idx", sprintf("RCB class: %s", cls))
            if (self$options$showPlot)
                self$results$plot$setState(list(mode = "manual", idx = idx, cls = cls))
            if (self$options$showSummary)
                private$.summaryManual(idx, cls)
            if (self$options$showExplanation)
                private$.explain()
        },

        # ---- cohort ---------------------------------------------------------
        .runCohort = function() {
            opt <- self$options
            if (is.null(opt$d1) || is.null(opt$d2) || is.null(opt$cellularity) ||
                is.null(opt$positiveNodes))
                return()

            data <- self$data
            d1 <- jmvcore::toNumeric(data[[opt$d1]])
            d2 <- jmvcore::toNumeric(data[[opt$d2]])
            ca <- jmvcore::toNumeric(data[[opt$cellularity]])
            cis <- if (!is.null(opt$cis)) jmvcore::toNumeric(data[[opt$cis]]) else rep(0, nrow(data))
            ln <- jmvcore::toNumeric(data[[opt$positiveNodes]])
            dmet <- if (!is.null(opt$metSize)) jmvcore::toNumeric(data[[opt$metSize]]) else rep(0, nrow(data))

            idx <- private$.rcbIndex(d1, d2, ca, cis, ln, dmet)
            cls <- private$.rcbClass(idx)

            ok <- !is.na(idx)
            if (sum(ok) == 0) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>No rows with sufficient data to compute RCB.</p>")
                return()
            }

            # case table = cohort summary of the index
            tab <- self$results$caseTable
            tab$addRow(rowKey = "n", values = list(quantity = "Patients with RCB", value = sum(ok)))
            tab$addRow(rowKey = "mean", values = list(quantity = "Mean RCB index", value = mean(idx[ok])))
            tab$addRow(rowKey = "median", values = list(quantity = "Median RCB index", value = stats::median(idx[ok])))
            tab$addRow(rowKey = "pcr", values = list(
                quantity = "pCR rate (RCB-0)", value = mean(idx[ok] == 0)))

            if (opt$showDistribution)
                private$.populateDistribution(idx[ok], cls[ok])

            if (opt$survivalLink && !is.null(opt$survivalTime) && !is.null(opt$survivalStatus))
                private$.populateSurvival(idx, cls)

            if (opt$showPlot)
                self$results$plot$setState(list(mode = "cohort", idx = idx[ok], cls = cls[ok]))

            # write-back outputs
            if (self$results$addIndexToData$isNotFilled()) {
                self$results$addIndexToData$setRowNums(rownames(data))
                self$results$addIndexToData$setValues(idx)
            }
            if (self$results$addClassToData$isNotFilled()) {
                self$results$addClassToData$setRowNums(rownames(data))
                self$results$addClassToData$setValues(as.character(cls))
            }

            if (opt$showSummary)
                private$.summaryCohort(idx[ok], cls[ok])
            if (opt$showExplanation)
                private$.explain()
        },

        .populateDistribution = function(idx, cls) {
            tab <- self$results$distributionTable
            n <- length(idx)
            for (lv in levels(cls)) {
                sel <- cls == lv
                tab$addRow(rowKey = lv, values = list(
                    rcbclass = lv, n = sum(sel), pct = sum(sel) / n,
                    meanIndex = if (any(sel)) mean(idx[sel]) else NA_real_))
            }
        },

        .populateSurvival = function(idx, cls) {
            opt <- self$options
            time <- jmvcore::toNumeric(self$data[[opt$survivalTime]])
            statusRaw <- self$data[[opt$survivalStatus]]
            ev <- if (!is.null(opt$eventLevel) && opt$eventLevel != "")
                      opt$eventLevel else NULL
            if (is.factor(statusRaw) || is.character(statusRaw)) {
                if (is.null(ev)) ev <- levels(as.factor(statusRaw))[nlevels(as.factor(statusRaw))]
                status <- as.integer(as.character(statusRaw) == ev)
            } else {
                sn <- jmvcore::toNumeric(statusRaw)
                status <- as.integer(sn == if (!is.null(ev)) suppressWarnings(as.numeric(ev)) else max(sn, na.rm = TRUE))
            }
            df <- data.frame(time = time, status = status, cls = cls)
            df <- df[stats::complete.cases(df) & !is.na(df$cls), , drop = FALSE]
            df$cls <- droplevels(df$cls)
            if (nrow(df) < 2 || nlevels(df$cls) < 2) return()

            tab <- self$results$survivalTable
            fit <- tryCatch(survival::survfit(survival::Surv(time, status) ~ cls, data = df),
                            error = function(e) NULL)
            med <- if (!is.null(fit)) summary(fit)$table else NULL
            for (lv in levels(df$cls)) {
                sel <- df$cls == lv
                mt <- NA_real_
                if (!is.null(med)) {
                    rn <- paste0("cls=", lv)
                    if (is.matrix(med) && rn %in% rownames(med)) mt <- med[rn, "median"]
                }
                tab$addRow(rowKey = lv, values = list(
                    rcbclass = lv, n = sum(sel), events = sum(df$status[sel]),
                    medianTime = mt))
            }
            lr <- tryCatch(survival::survdiff(survival::Surv(time, status) ~ cls, data = df),
                           error = function(e) NULL)
            if (!is.null(lr)) {
                p <- 1 - stats::pchisq(lr$chisq, length(lr$n) - 1)
                tab$setNote("logrank", sprintf(
                    "Log-rank test across RCB classes: \u03c7\u00b2 = %.2f, df = %d, p = %s.",
                    lr$chisq, length(lr$n) - 1, format.pval(p, digits = 3, eps = 1e-4)))
            }
        },

        .summaryManual = function(idx, cls) {
            self$results$summary$setContent(glue::glue(
                "<p>The computed <b>RCB index is {sprintf('%.2f', idx)}</b>, corresponding to
                <b>{cls}</b>.</p>"))
        },

        .summaryCohort = function(idx, cls) {
            n <- length(idx); pcr <- mean(idx == 0)
            iii <- mean(cls == "RCB-III")
            self$results$summary$setContent(glue::glue(
                "<p>Across {n} patients, the pCR rate (RCB-0) was
                <b>{sprintf('%.1f%%', 100 * pcr)}</b> and
                <b>{sprintf('%.1f%%', 100 * iii)}</b> had extensive residual disease
                (RCB-III). The median RCB index was
                {sprintf('%.2f', stats::median(idx))}.</p>"))
        },

        .explain = function() {
            self$results$explanation$setContent(
                "<h4>Residual Cancer Burden</h4>
                <p>RCB (Symmans et al., 2007) quantifies residual disease after neoadjuvant
                therapy from six routine pathology variables. The invasive fraction is
                f<sub>inv</sub> = (1 \u{2212} %CIS/100) \u{00D7} (%CA/100); the primary bed size is
                d<sub>prim</sub> = \u{221A}(d<sub>1</sub> \u{00D7} d<sub>2</sub>). The index is:</p>
                <p style='text-align:center'><i>RCB = 1.4 (f<sub>inv</sub> d<sub>prim</sub>)<sup>0.17</sup>
                + [4 (1 \u{2212} 0.75<sup>LN</sup>) d<sub>met</sub>]<sup>0.17</sup></i></p>
                <p>where LN is the number of positive nodes and d<sub>met</sub> the largest
                nodal metastasis (mm). Cut points of 1.36 and 3.28 define four classes:
                RCB-0 (pCR), RCB-I (minimal), RCB-II (moderate), and RCB-III (extensive)
                residual disease. RCB is a continuous, reproducible, independently
                prognostic measure of response.</p>")
        },

        .plot = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            if (st$mode == "manual") {
                # position of the single case on the RCB scale
                df <- data.frame(x = st$idx, y = 1)
                p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
                    ggplot2::annotate("rect", xmin = 0, xmax = 1.36, ymin = 0.5, ymax = 1.5,
                                      fill = "#2c7fb8", alpha = 0.25) +
                    ggplot2::annotate("rect", xmin = 1.36, xmax = 3.28, ymin = 0.5, ymax = 1.5,
                                      fill = "#fdae61", alpha = 0.3) +
                    ggplot2::annotate("rect", xmin = 3.28, xmax = max(4.5, st$idx + 0.5),
                                      ymin = 0.5, ymax = 1.5, fill = "#de2d26", alpha = 0.3) +
                    ggplot2::geom_vline(xintercept = st$idx, linewidth = 1.2) +
                    ggplot2::geom_point(size = 4) +
                    ggplot2::scale_y_continuous(limits = c(0.5, 1.5), breaks = NULL) +
                    ggplot2::labs(x = "RCB index", y = NULL,
                                  title = sprintf("RCB = %.2f  (%s)", st$idx, st$cls)) +
                    ggtheme
            } else {
                df <- data.frame(cls = st$cls)
                p <- ggplot2::ggplot(df, ggplot2::aes(x = cls, fill = cls)) +
                    ggplot2::geom_bar() +
                    ggplot2::scale_fill_manual(values = c(
                        "RCB-0 (pCR)" = "#1a9850", "RCB-I" = "#2c7fb8",
                        "RCB-II" = "#fdae61", "RCB-III" = "#de2d26"), guide = "none") +
                    ggplot2::labs(x = NULL, y = "Patients", title = "RCB class distribution") +
                    ggtheme
            }
            print(p)
            TRUE
        }
    )
)
