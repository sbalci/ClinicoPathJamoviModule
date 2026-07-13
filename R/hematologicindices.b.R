#' @title Hematologic Prognostic Indices
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats complete.cases median quantile pchisq
#' @export

hematologicindicesClass <- R6::R6Class(
    "hematologicindicesClass",
    inherit = hematologicindicesBase,
    private = list(

        .init = function() {
            self$results$todo$setContent(glue::glue(
                "<h3>Hematologic Prognostic Indices</h3>
                <p>Derives blood-count and inflammation-based prognostic indices from routine
                laboratory variables.</p>
                <ul>
                  <li><b>NLR</b> = neutrophils / lymphocytes; <b>PLR</b> = platelets /
                      lymphocytes; <b>LMR</b> = lymphocytes / monocytes;
                      <b>SII</b> = platelets \u{00D7} neutrophils / lymphocytes.</li>
                  <li><b>PNI</b> (Onodera) = 10 \u{00D7} albumin (g/dL) + 0.005 \u{00D7} lymphocytes (/\u{00B5}L).</li>
                  <li><b>CAR</b> = CRP / albumin; <b>GPS / mGPS</b> from CRP and albumin thresholds.</li>
                </ul>
                <p>Provide the counts you have (10<sup>9</sup>/L); indices whose inputs are
                missing are skipped. Optionally split one index at its median or an optimal
                cutpoint and compare survival.</p>"))
        },

        .run = function() {
            opt <- self$options
            if (is.null(opt$neutrophils) && is.null(opt$lymphocytes) &&
                is.null(opt$platelets) && is.null(opt$albumin))
                return()

            vals <- private$.compute()
            if (is.null(vals)) return()

            if (opt$showIndicesTable)
                private$.populateIndices(vals)

            if (("gps" %in% opt$indices) && !is.null(vals$gps))
                private$.populateGPS(vals$gps)
            else
                self$results$gpsTable$setVisible(FALSE)

            if (opt$showSurvival)
                private$.populateSurvival(vals)

            if (opt$showPlot)
                self$results$plot$setState(vals)

            # write-back the survival-split index
            si <- opt$survivalIndex
            if (!is.null(vals[[si]]) && self$results$addIndexToData$isNotFilled()) {
                self$results$addIndexToData$setRowNums(rownames(self$data))
                self$results$addIndexToData$setValues(vals[[si]])
            }

            if (opt$showSummary)
                private$.summary(vals)
            if (opt$showExplanation)
                private$.explain()
        },

        .getv = function(name) {
            if (is.null(self$options[[name]])) return(NULL)
            jmvcore::toNumeric(self$data[[self$options[[name]]]])
        },

        .compute = function() {
            opt <- self$options
            neut <- private$.getv("neutrophils")
            lymph <- private$.getv("lymphocytes")
            plt <- private$.getv("platelets")
            mono <- private$.getv("monocytes")
            alb <- private$.getv("albumin")
            crp <- private$.getv("crp")

            nrows <- nrow(self$data)
            out <- list()
            want <- opt$indices

            if ("nlr" %in% want && !is.null(neut) && !is.null(lymph))
                out$nlr <- ifelse(lymph > 0, neut / lymph, NA_real_)
            if ("plr" %in% want && !is.null(plt) && !is.null(lymph))
                out$plr <- ifelse(lymph > 0, plt / lymph, NA_real_)
            if ("lmr" %in% want && !is.null(lymph) && !is.null(mono))
                out$lmr <- ifelse(mono > 0, lymph / mono, NA_real_)
            if ("sii" %in% want && !is.null(plt) && !is.null(neut) && !is.null(lymph))
                out$sii <- ifelse(lymph > 0, plt * neut / lymph, NA_real_)
            if ("pni" %in% want && !is.null(alb) && !is.null(lymph)) {
                alb_gdl <- if (opt$albuminUnit == "gl") alb / 10 else alb
                tlc_uL <- lymph * 1000            # 10^9/L -> /uL
                out$pni <- 10 * alb_gdl + 0.005 * tlc_uL
            }
            if ("car" %in% want && !is.null(crp) && !is.null(alb)) {
                alb_gl <- if (opt$albuminUnit == "gdl") alb * 10 else alb
                out$car <- ifelse(alb_gl > 0, crp / alb_gl, NA_real_)
            }
            if ("gps" %in% want && !is.null(crp) && !is.null(alb)) {
                alb_gl <- if (opt$albuminUnit == "gdl") alb * 10 else alb
                out$gps <- private$.gpsScore(crp, alb_gl, opt$gpsType == "modified")
            }
            if (length(out) == 0) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>None of the selected indices could be computed
                    from the supplied variables. Check that the required inputs are
                    provided (e.g. NLR needs neutrophils and lymphocytes).</p>")
                return(NULL)
            }
            out
        },

        .gpsScore = function(crp_mgL, alb_gL, modified) {
            hiCRP <- crp_mgL > 10
            loAlb <- alb_gL < 35
            score <- integer(length(crp_mgL))
            if (modified) {
                score[!hiCRP] <- 0L
                score[hiCRP & !loAlb] <- 1L
                score[hiCRP & loAlb] <- 2L
            } else {
                score <- as.integer(hiCRP) + as.integer(loAlb)
            }
            score[is.na(crp_mgL) | is.na(alb_gL)] <- NA_integer_
            score
        },

        .populateIndices = function(vals) {
            tab <- self$results$indicesTable
            labels <- c(nlr = "NLR", plr = "PLR", lmr = "LMR", sii = "SII",
                        pni = "PNI", car = "CAR")
            for (k in names(labels)) {
                if (is.null(vals[[k]])) next
                v <- vals[[k]][!is.na(vals[[k]])]
                if (length(v) == 0) next
                qs <- stats::quantile(v, c(0.25, 0.75), names = FALSE)
                tab$addRow(rowKey = k, values = list(
                    index = labels[[k]], n = length(v), mean = mean(v),
                    median = stats::median(v), q1 = qs[1], q3 = qs[2]))
            }
        },

        .populateGPS = function(gps) {
            tab <- self$results$gpsTable
            g <- gps[!is.na(gps)]
            n <- length(g)
            for (s in 0:2)
                tab$addRow(rowKey = s, values = list(
                    score = sprintf("%s = %d", if (self$options$gpsType == "modified") "mGPS" else "GPS", s),
                    n = sum(g == s), pct = if (n > 0) mean(g == s) else 0))
        },

        .populateSurvival = function(vals) {
            opt <- self$options
            si <- opt$survivalIndex
            if (is.null(vals[[si]]) || is.null(opt$survivalTime) || is.null(opt$survivalStatus))
                return()
            idx <- vals[[si]]
            time <- private$.getv("survivalTime")
            sraw <- self$data[[opt$survivalStatus]]
            ev <- if (!is.null(opt$eventLevel) && opt$eventLevel != "") opt$eventLevel else NULL
            if (is.factor(sraw) || is.character(sraw)) {
                if (is.null(ev)) ev <- levels(as.factor(sraw))[nlevels(as.factor(sraw))]
                status <- as.integer(as.character(sraw) == ev)
            } else {
                sn <- jmvcore::toNumeric(sraw)
                status <- as.integer(sn == if (!is.null(ev)) suppressWarnings(as.numeric(ev)) else max(sn, na.rm = TRUE))
            }
            df <- data.frame(idx = idx, time = time, status = status)
            df <- df[stats::complete.cases(df), , drop = FALSE]
            if (nrow(df) < 4) return()

            cut <- if (opt$splitMethod == "optimal")
                private$.optimalCut(df$idx, df$time, df$status) else stats::median(df$idx)
            if (is.na(cut)) cut <- stats::median(df$idx)
            df$grp <- factor(ifelse(df$idx > cut, "High", "Low"), levels = c("Low", "High"))
            if (nlevels(droplevels(df$grp)) < 2) return()

            tab <- self$results$survivalTable
            cox <- tryCatch(survival::coxph(survival::Surv(time, status) ~ grp, data = df),
                            error = function(e) NULL)
            hrs <- if (!is.null(cox)) summary(cox, conf.int = 0.95)$conf.int else NULL
            fit <- tryCatch(survival::survfit(survival::Surv(time, status) ~ grp, data = df),
                            error = function(e) NULL)
            med <- if (!is.null(fit)) summary(fit)$table else NULL
            for (lv in c("Low", "High")) {
                sel <- df$grp == lv
                mt <- NA_real_
                if (!is.null(med) && is.matrix(med)) {
                    rn <- paste0("grp=", lv); if (rn %in% rownames(med)) mt <- med[rn, "median"]
                }
                hr <- NA_real_; lo <- NA_real_; hi <- NA_real_
                if (lv == "Low") hr <- 1
                else if (!is.null(hrs) && "grpHigh" %in% rownames(hrs)) {
                    hr <- hrs["grpHigh", "exp(coef)"]; lo <- hrs["grpHigh", 3]; hi <- hrs["grpHigh", 4]
                }
                tab$addRow(rowKey = lv, values = list(
                    group = sprintf("%s %s (%s cut %.3g)", toupper(si), lv,
                                    if (opt$splitMethod == "optimal") "optimal" else "median", cut),
                    n = sum(sel), events = sum(df$status[sel]), medianTime = mt,
                    hr = hr, hr_lower = lo, hr_upper = hi))
            }
            lr <- tryCatch(survival::survdiff(survival::Surv(time, status) ~ grp, data = df),
                           error = function(e) NULL)
            if (!is.null(lr)) {
                p <- 1 - stats::pchisq(lr$chisq, length(lr$n) - 1)
                tab$setNote("lr", sprintf(
                    "Log-rank p = %s; HR is High vs Low.",
                    format.pval(p, digits = 3, eps = 1e-4)))
            }
        },

        .optimalCut = function(idx, time, status) {
            v <- idx[!is.na(idx)]
            if (length(v) < 20) return(NA_real_)
            cands <- unique(stats::quantile(v, seq(0.2, 0.8, 0.05), names = FALSE))
            best_cp <- NA_real_; best_chi <- -Inf
            for (cp in cands) {
                g <- factor(idx > cp)
                if (nlevels(g) < 2 || min(table(g)) < 10) next
                sd <- tryCatch(survival::survdiff(survival::Surv(time, status) ~ g),
                               error = function(e) NULL)
                if (!is.null(sd) && sd$chisq > best_chi) { best_chi <- sd$chisq; best_cp <- cp }
            }
            best_cp
        },

        .summary = function(vals) {
            parts <- c()
            for (k in c("nlr", "plr", "lmr", "sii", "pni", "car")) {
                if (!is.null(vals[[k]])) {
                    v <- vals[[k]][!is.na(vals[[k]])]
                    if (length(v)) parts <- c(parts, sprintf("%s median %.2f", toupper(k), stats::median(v)))
                }
            }
            txt <- if (length(parts)) paste(parts, collapse = "; ") else "no indices computed"
            self$results$summary$setContent(glue::glue("<p>Cohort index medians: {txt}.</p>"))
        },

        .explain = function() {
            self$results$explanation$setContent(
                "<h4>Inflammation-based prognostic indices</h4>
                <p><b>NLR</b> = neutrophils / lymphocytes, <b>PLR</b> = platelets /
                lymphocytes, <b>LMR</b> = lymphocytes / monocytes, and
                <b>SII</b> = platelets \u{00D7} neutrophils / lymphocytes capture the balance
                between systemic inflammation and adaptive immunity.</p>
                <p><b>PNI</b> (Onodera, 1984) = 10 \u{00D7} albumin (g/dL) + 0.005 \u{00D7}
                total lymphocyte count (/\u{00B5}L) combines nutrition and immunity.
                <b>CAR</b> = CRP / albumin. The <b>Glasgow Prognostic Score</b> (McMillan)
                scores elevated CRP (&gt; 10 mg/L) and low albumin (&lt; 35 g/L): the
                <i>modified</i> GPS requires elevated CRP for any positive score, whereas the
                original GPS also scores isolated hypoalbuminaemia.</p>
                <p>Cut points for these indices are cancer- and assay-specific; a median or
                data-driven optimal split is provided for illustration and should be
                validated externally.</p>")
        },

        .plot = function(image, ggtheme, theme, ...) {
            vals <- image$state
            if (is.null(vals)) return(FALSE)
            opt <- self$options
            if (opt$showSurvival && !is.null(vals[[opt$survivalIndex]]) &&
                !is.null(opt$survivalTime) && !is.null(opt$survivalStatus)) {
                # KM by split of the chosen index
                si <- opt$survivalIndex
                idx <- vals[[si]]
                time <- private$.getv("survivalTime")
                sraw <- self$data[[opt$survivalStatus]]
                ev <- if (!is.null(opt$eventLevel) && opt$eventLevel != "") opt$eventLevel else NULL
                if (is.factor(sraw) || is.character(sraw)) {
                    if (is.null(ev)) ev <- levels(as.factor(sraw))[nlevels(as.factor(sraw))]
                    status <- as.integer(as.character(sraw) == ev)
                } else { sn <- jmvcore::toNumeric(sraw); status <- as.integer(sn == max(sn, na.rm = TRUE)) }
                df <- data.frame(idx, time, status)
                df <- df[stats::complete.cases(df), , drop = FALSE]
                cut <- if (opt$splitMethod == "optimal") private$.optimalCut(df$idx, df$time, df$status) else stats::median(df$idx)
                if (is.na(cut)) cut <- stats::median(df$idx)
                df$grp <- factor(ifelse(df$idx > cut, "High", "Low"), levels = c("Low", "High"))
                fit <- survival::survfit(survival::Surv(time, status) ~ grp, data = df)
                sf <- data.frame(time = fit$time, surv = fit$surv,
                                 grp = rep(names(fit$strata), fit$strata))
                sf$grp <- sub("grp=", "", sf$grp)
                p <- ggplot2::ggplot(sf, ggplot2::aes(x = time, y = surv, colour = grp)) +
                    ggplot2::geom_step(linewidth = 0.9) +
                    ggplot2::scale_y_continuous(limits = c(0, 1)) +
                    ggplot2::labs(x = "Time", y = "Survival probability",
                                  colour = toupper(si),
                                  title = sprintf("Survival by %s split", toupper(si))) +
                    ggtheme + ggplot2::theme(legend.position = "bottom")
            } else {
                # distributions of continuous indices (faceted)
                cont <- c("nlr", "plr", "lmr", "sii", "pni", "car")
                dfl <- do.call(rbind, lapply(cont, function(k) {
                    if (is.null(vals[[k]])) return(NULL)
                    data.frame(index = toupper(k), value = vals[[k]][!is.na(vals[[k]])])
                }))
                if (is.null(dfl) || nrow(dfl) == 0) return(FALSE)
                p <- ggplot2::ggplot(dfl, ggplot2::aes(x = value, fill = index)) +
                    ggplot2::geom_histogram(bins = 25, colour = "white") +
                    ggplot2::facet_wrap(~ index, scales = "free") +
                    ggplot2::labs(x = NULL, y = "Patients", title = "Index distributions") +
                    ggtheme + ggplot2::theme(legend.position = "none")
            }
            print(p)
            TRUE
        }
    )
)
