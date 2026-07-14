#' @title Tumor Budding (ITBCC)
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats pchisq
#' @export

tumorbuddingClass <- R6::R6Class(
    "tumorbuddingClass",
    inherit = tumorbuddingBase,
    private = list(

        .init = function() {
            self$results$todo$setContent(glue::glue(
                "<h3>Tumor Budding (ITBCC 2016)</h3>
                <p>Assigns an International Tumor Budding Consensus Conference grade from the
                number of tumor buds in a hotspot.</p>
                <p><b>Provide</b> a bud count per field. If several fields are recorded per case
                (share a <b>Case ID</b>), the densest field is used as the hotspot. Counts are
                normalized to the standard <b>0.785 mm\u{00B2}</b> field before grading:
                <b>Bd1</b> (low) 0\u{2013}4, <b>Bd2</b> (intermediate) 5\u{2013}9,
                <b>Bd3</b> (high) \u{2265}10 buds.</p>"))
        },

        .gradeOf = function(buds) {
            ifelse(buds <= 4, "Bd1 (low)",
                ifelse(buds <= 9, "Bd2 (intermediate)", "Bd3 (high)"))
        },

        .run = function() {
            opt <- self$options
            if (is.null(opt$budCount)) return()
            prep <- private$.prepare()
            if (is.null(prep)) return()

            if (opt$showGrading) private$.grading(prep)
            if (opt$showPerCase) private$.perCase(prep)
            if (opt$showSurvival && !is.null(opt$survivalTime) &&
                !is.null(opt$survivalStatus)) private$.survival(prep)
            if (opt$showPlot) self$results$plot$setState(prep)
            private$.writeGrade(prep)
            if (opt$showSummary) private$.summary(prep)
            if (opt$showExplanation) private$.explanation()
        },

        .prepare = function() {
            opt <- self$options; data <- self$data
            raw <- jmvcore::toNumeric(data[[opt$budCount]])
            area <- opt$fieldArea
            keep <- !is.na(raw) & raw >= 0
            if (sum(keep) < 1) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>No valid bud counts found.</p>")
                return(NULL)
            }

            # normalize each field to per-0.785 mm2
            norm <- raw * (0.785 / area)

            if (!is.null(opt$caseId)) {
                cid <- as.character(data[[opt$caseId]])
                dff <- data.frame(cid = cid[keep], norm = norm[keep], raw = raw[keep],
                                  stringsAsFactors = FALSE)
                # hotspot = densest field per case
                agg <- do.call(rbind, lapply(split(dff, dff$cid), function(s) {
                    i <- which.max(s$norm)
                    data.frame(case = s$cid[i], rawMax = s$raw[i], normalized = s$norm[i],
                               stringsAsFactors = FALSE)
                }))
                agg$grade <- private$.gradeOf(round(agg$normalized))
                perRowGrade <- NULL     # write-back handled at case level below
            } else {
                agg <- data.frame(case = as.character(seq_len(sum(keep))),
                                  rawMax = raw[keep], normalized = norm[keep],
                                  stringsAsFactors = FALSE)
                agg$grade <- private$.gradeOf(round(agg$normalized))
            }

            list(agg = agg, keep = keep, area = area, raw = raw, norm = norm,
                 hasCase = !is.null(opt$caseId))
        },

        .grading = function(p) {
            tab <- self$results$gradingTable
            levs <- c("Bd1 (low)", "Bd2 (intermediate)", "Bd3 (high)")
            defs <- c("0-4", "5-9", ">= 10")
            n <- nrow(p$agg)
            for (k in seq_along(levs)) {
                cnt <- sum(p$agg$grade == levs[k])
                tab$addRow(rowKey = levs[k], values = list(
                    grade = levs[k], definition = defs[k], n = cnt, pct = cnt / n))
            }
            tab$setNote("area", sprintf(
                "Counts normalized from a %.3f mm\u00b2 field to the ITBCC standard 0.785 mm\u00b2.",
                p$area))
        },

        .perCase = function(p) {
            tab <- self$results$perCaseTable
            for (i in seq_len(nrow(p$agg))) {
                tab$addRow(rowKey = i, values = list(
                    case = p$agg$case[i],
                    rawMax = p$agg$rawMax[i],
                    normalized = round(p$agg$normalized[i], 1),
                    grade = p$agg$grade[i]))
            }
        },

        .survival = function(p) {
            if (!p$hasCase) {
                # survival needs one row per case; without a case id, each row is a case
            }
            opt <- self$options
            if (!requireNamespace("survival", quietly = TRUE)) {
                self$results$survivalTable$setNote("dep", "Package 'survival' is required.")
                return()
            }
            data <- self$data
            # map survival to the per-case aggregation only when no case id (row = case)
            if (p$hasCase) {
                self$results$survivalTable$setNote("case",
                    "Survival linkage uses one row per case; with multiple fields per case, supply case-level survival on the hotspot row or omit the Case ID.")
            }
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
            gr <- factor(private$.gradeOf(round(p$norm[p$keep])),
                         levels = c("Bd1 (low)", "Bd2 (intermediate)", "Bd3 (high)"))
            ok <- !is.na(time) & !is.na(status)
            if (sum(ok) < 5 || length(unique(gr[ok])) < 2) {
                self$results$survivalTable$setNote("na",
                    "Not enough complete survival data across \u22652 budding grades.")
                return()
            }
            df <- data.frame(time = time[ok], status = status[ok], gr = droplevels(gr[ok]))
            sd <- survival::survdiff(survival::Surv(time, status) ~ gr, data = df)
            lr_p <- 1 - stats::pchisq(sd$chisq, length(sd$n) - 1)
            tab <- self$results$survivalTable
            tab$addRow(rowKey = "lr", values = list(
                statistic = "Log-rank across grades",
                value = sprintf("chi-sq %.2f (df %d), p = %s", sd$chisq, length(sd$n) - 1,
                                format.pval(lr_p, digits = 3, eps = 1e-4))))
            # Cox trend using grade as ordinal score
            cox <- tryCatch(survival::coxph(
                survival::Surv(time, status) ~ as.integer(gr), data = df),
                error = function(e) NULL)
            if (!is.null(cox)) {
                hr <- exp(stats::coef(cox)[1])
                p_t <- summary(cox)$coefficients[1, "Pr(>|z|)"]
                tab$addRow(rowKey = "trend", values = list(
                    statistic = "HR per grade increase (trend)",
                    value = sprintf("%.2f, p = %s", hr, format.pval(p_t, digits = 3, eps = 1e-4))))
            }
        },

        .writeGrade = function(p) {
            if (self$results$addGradeToData$isNotFilled() && !p$hasCase) {
                grade <- rep(NA_character_, length(p$keep))
                grade[p$keep] <- private$.gradeOf(round(p$norm[p$keep]))
                self$results$addGradeToData$setRowNums(rownames(self$data))
                self$results$addGradeToData$setValues(grade)
            }
        },

        .summary = function(p) {
            n <- nrow(p$agg)
            bd3 <- sum(p$agg$grade == "Bd3 (high)")
            html <- glue::glue(
                "<p>Across {n} case{ifelse(n==1,'','s')}, the ITBCC budding grade was
                <b>Bd1 (low)</b> in {sum(p$agg$grade=='Bd1 (low)')},
                <b>Bd2 (intermediate)</b> in {sum(p$agg$grade=='Bd2 (intermediate)')}, and
                <b>Bd3 (high)</b> in {bd3} ({round(100*bd3/n)}%).
                High-grade budding (Bd3) is associated with adverse outcome in several
                carcinomas.</p>")
            self$results$summary$setContent(html)
        },

        .explanation = function() {
            self$results$explanation$setContent(
                "<h4>ITBCC tumor budding</h4>
                <p>Tumor buds are single cells or clusters of up to four cells at the invasive
                front. Following the International Tumor Budding Consensus Conference (ITBCC
                2016), buds are counted in the single densest <b>hotspot</b> field, standardized
                to an area of <b>0.785 mm\u{00B2}</b> (a 20x objective with a 0.55 mm field
                diameter). When the microscope field differs, counts are scaled to that
                standard area before grading.</p>
                <p>The grade is <b>Bd1</b> (low, 0\u{2013}4 buds), <b>Bd2</b> (intermediate,
                5\u{2013}9), or <b>Bd3</b> (high, \u{2265}10). Higher grades carry worse prognosis;
                when a survival outcome is supplied, a log-rank test across grades and a Cox
                per-grade trend hazard ratio are reported.</p>")
        },

        .plot = function(image, ggtheme, theme, ...) {
            p <- image$state
            if (is.null(p)) return(FALSE)
            df <- as.data.frame(table(factor(p$agg$grade,
                levels = c("Bd1 (low)", "Bd2 (intermediate)", "Bd3 (high)"))))
            names(df) <- c("grade", "n")
            gg <- ggplot2::ggplot(df, ggplot2::aes(x = grade, y = n, fill = grade)) +
                ggplot2::geom_col(width = 0.65) +
                ggplot2::geom_text(ggplot2::aes(label = n), vjust = -0.3, size = 3.6) +
                ggplot2::scale_fill_manual(values = c(
                    "Bd1 (low)" = "#2c7fb8", "Bd2 (intermediate)" = "#fdae61",
                    "Bd3 (high)" = "#de2d26"), guide = "none") +
                ggplot2::labs(x = NULL, y = "Number of cases",
                    title = "ITBCC tumor budding grade distribution") +
                ggtheme
            print(gg)
            TRUE
        }
    )
)
