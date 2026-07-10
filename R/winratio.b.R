#' @title Win Ratio for Hierarchical Composite Endpoints
#' @importFrom R6 R6Class
#' @import jmvcore
#' @export

winratioClass <- R6::R6Class(
    "winratioClass",
    inherit = winratioBase,
    private = list(

        # ---- init: instructions --------------------------------------------
        .init = function() {
            todo <- glue::glue(
                "<h3>Win Ratio for Hierarchical Composite Endpoints</h3>
                <p>Compares every subject in the index group with every subject in the
                reference group, classifying each pair as a <b>win</b>, <b>loss</b>, or
                <b>tie</b> by examining endpoints in order of clinical priority. The first
                endpoint that distinguishes a pair decides it.</p>
                <p><b>To run the analysis, provide:</b></p>
                <ul>
                  <li>A two-level <b>group / treatment variable</b> and its reference level.</li>
                  <li>A <b>primary time-to-event endpoint</b> (time + event) &mdash; highest priority.</li>
                  <li><i>Optionally</i> a secondary time-to-event endpoint and a continuous
                      tiebreaker, examined only when higher-priority endpoints tie.</li>
                </ul>
                <p>Reports the <b>win ratio</b> (wins / losses), <b>win odds</b>,
                <b>net benefit</b>, and a confidence interval (analytic or bootstrap).</p>"
            )
            self$results$todo$setContent(todo)
        },

        # ---- run -------------------------------------------------------------
        .run = function() {

            opt <- self$options
            if (is.null(opt$group) || is.null(opt$time1) || is.null(opt$status1))
                return()
            if (is.null(opt$refLevel) || opt$refLevel == "")
                return()

            prep <- private$.prepareData()
            if (is.null(prep)) return()

            res <- private$.winRatio(prep$df, prep$eps, prep$idxLbl, prep$refLbl)
            if (is.null(res)) return()

            private$.populateMain(res)
            private$.populateCounts(res)
            if (opt$showComponents)
                private$.populateComponents(res)
            if (opt$showPlot)
                self$results$plot$setState(res)
            if (opt$showSummary)
                private$.populateSummary(res)
            if (opt$showExplanation)
                private$.populateExplanation()
        },

        # ---- data preparation ------------------------------------------------
        .prepareData = function() {
            opt  <- self$options
            data <- self$data

            g <- data[[opt$group]]
            if (!is.factor(g)) g <- as.factor(g)
            levs <- levels(droplevels(g))
            if (length(levs) != 2) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>The group variable must have exactly two
                    levels present in the data.</p>")
                return(NULL)
            }
            refLbl <- opt$refLevel
            if (!(refLbl %in% levs)) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>The chosen reference level is not present in
                    the data.</p>")
                return(NULL)
            }
            idxLbl <- setdiff(levs, refLbl)

            df <- data.frame(.grp = as.character(g), stringsAsFactors = FALSE)

            # endpoint hierarchy
            eps <- list()

            ev1 <- if (!is.null(opt$eventLevel1) && opt$eventLevel1 != "")
                        opt$eventLevel1 else NULL
            df$.t1 <- jmvcore::toNumeric(data[[opt$time1]])
            df$.e1 <- private$.eventNumeric(data[[opt$status1]], ev1)
            eps[[length(eps) + 1]] <- list(type = "tte", time = ".t1", event = ".e1",
                                           label = opt$time1)

            if (!is.null(opt$time2) && !is.null(opt$status2)) {
                ev2 <- if (!is.null(opt$eventLevel2) && opt$eventLevel2 != "")
                            opt$eventLevel2 else NULL
                df$.t2 <- jmvcore::toNumeric(data[[opt$time2]])
                df$.e2 <- private$.eventNumeric(data[[opt$status2]], ev2)
                eps[[length(eps) + 1]] <- list(type = "tte", time = ".t2", event = ".e2",
                                               label = opt$time2)
            }

            if (!is.null(opt$contEndpoint)) {
                df$.c1 <- jmvcore::toNumeric(data[[opt$contEndpoint]])
                eps[[length(eps) + 1]] <- list(type = "cont", value = ".c1",
                                               direction = opt$contDirection,
                                               tol = opt$contTol,
                                               label = opt$contEndpoint)
            }

            # drop rows with missing group or missing primary endpoint
            ok <- !is.na(df$.grp) & !is.na(df$.t1) & !is.na(df$.e1)
            df <- df[ok, , drop = FALSE]
            if (nrow(df) < 2 || length(unique(df$.grp)) != 2) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>Not enough complete observations in both
                    groups to compute the win ratio.</p>")
                return(NULL)
            }

            list(df = df, eps = eps, idxLbl = idxLbl, refLbl = refLbl)
        },

        .eventNumeric = function(x, eventLevel) {
            # returns 1 = event, 0 = censored/no-event
            if (is.factor(x) || is.character(x)) {
                if (is.null(eventLevel) || eventLevel == "") {
                    # fall back to the highest level
                    xf <- as.factor(x)
                    eventLevel <- levels(xf)[length(levels(xf))]
                }
                return(as.integer(as.character(x) == eventLevel))
            }
            xn <- jmvcore::toNumeric(x)
            if (!is.null(eventLevel) && eventLevel != "" &&
                !is.na(suppressWarnings(as.numeric(eventLevel))))
                return(as.integer(xn == as.numeric(eventLevel)))
            as.integer(xn == max(xn, na.rm = TRUE) | xn == 1)
        },

        # ---- pairwise hierarchical comparison --------------------------------
        # +1 : subject a wins ; -1 : a loses ; 0 : tie ; also returns deciding endpoint idx
        .comparePair = function(a, b, eps) {
            for (k in seq_along(eps)) {
                e <- eps[[k]]
                if (e$type == "tte") {
                    ta <- a[[e$time]]; ea <- a[[e$event]]
                    tb <- b[[e$time]]; eb <- b[[e$event]]
                    if (is.na(ta) || is.na(tb)) next
                    # a wins if b has the event strictly earlier while a outlives that time
                    if (!is.na(eb) && eb == 1 && ta > tb) return(c(1L, k))
                    if (!is.na(ea) && ea == 1 && tb > ta) return(c(-1L, k))
                } else {
                    va <- a[[e$value]]; vb <- b[[e$value]]
                    if (is.na(va) || is.na(vb)) next
                    d <- if (e$direction == "higher") va - vb else vb - va
                    tol <- if (is.null(e$tol)) 0 else e$tol
                    if (d >  tol) return(c(1L, k))
                    if (d < -tol) return(c(-1L, k))
                }
            }
            c(0L, NA_integer_)
        },

        # ---- win ratio engine (Dong et al. 2016 variance) -------------------
        .winRatio = function(df, eps, idxLbl, refLbl) {
            i1 <- which(df$.grp == idxLbl)   # index group
            i0 <- which(df$.grp == refLbl)   # reference
            n1 <- length(i1); n0 <- length(i0)

            win_i  <- integer(nrow(df))
            loss_i <- integer(nrow(df))
            nep    <- length(eps)
            comp_w <- integer(nep); comp_l <- integer(nep)
            W <- 0L; L <- 0L

            rows <- lapply(seq_len(nrow(df)), function(r) as.list(df[r, , drop = FALSE]))

            for (i in i1) {
                ai <- rows[[i]]
                for (j in i0) {
                    r <- private$.comparePair(ai, rows[[j]], eps)
                    if (r[1] == 1L) {
                        W <- W + 1L; win_i[i] <- win_i[i] + 1L; loss_i[j] <- loss_i[j] + 1L
                        comp_w[r[2]] <- comp_w[r[2]] + 1L
                    } else if (r[1] == -1L) {
                        L <- L + 1L; loss_i[i] <- loss_i[i] + 1L; win_i[j] <- win_i[j] + 1L
                        comp_l[r[2]] <- comp_l[r[2]] + 1L
                    }
                }
            }

            Ttot <- n1 * n0
            Ttie <- Ttot - W - L
            wr <- if (L == 0) Inf else W / L

            conf <- self$options$conf_level
            z <- stats::qnorm(1 - (1 - conf) / 2)

            if (self$options$ciMethod == "bootstrap") {
                ci <- private$.bootstrapCI(df, eps, idxLbl, refLbl, conf)
                se_logwr <- NA_real_
                ci_low <- ci[1]; ci_high <- ci[2]
                zstat <- ci[3]; pval <- ci[4]
            } else {
                p_w <- W / Ttot; p_l <- L / Ttot
                mw1 <- win_i[i1] / n0; ml1 <- loss_i[i1] / n0
                mw0 <- win_i[i0] / n1; ml0 <- loss_i[i0] / n1
                var_pw   <- stats::var(mw1) / n1 + stats::var(mw0) / n0
                var_pl   <- stats::var(ml1) / n1 + stats::var(ml0) / n0
                cov_pwpl <- stats::cov(mw1, ml1) / n1 + stats::cov(mw0, ml0) / n0
                if (W == 0 || L == 0) {
                    se_logwr <- NA_real_; ci_low <- NA_real_; ci_high <- NA_real_
                    zstat <- NA_real_; pval <- NA_real_
                } else {
                    var_logwr <- var_pw / p_w^2 + var_pl / p_l^2 -
                                 2 * cov_pwpl / (p_w * p_l)
                    se_logwr <- sqrt(max(var_logwr, 0))
                    ci_low  <- exp(log(wr) - z * se_logwr)
                    ci_high <- exp(log(wr) + z * se_logwr)
                    zstat <- log(wr) / se_logwr
                    pval  <- 2 * stats::pnorm(-abs(zstat))
                }
            }

            win_odds <- (W + 0.5 * Ttie) / (L + 0.5 * Ttie)
            net_benefit <- (W - L) / Ttot

            list(W = W, L = L, Ttie = Ttie, Ttot = Ttot, n1 = n1, n0 = n0,
                 idxLbl = idxLbl, refLbl = refLbl,
                 win_ratio = wr, win_odds = win_odds, net_benefit = net_benefit,
                 se_logwr = se_logwr, ci_low = ci_low, ci_high = ci_high,
                 z = zstat, p = pval, conf = conf,
                 comp_w = comp_w, comp_l = comp_l, eps = eps)
        },

        .bootstrapCI = function(df, eps, idxLbl, refLbl, conf) {
            B <- self$options$bootstrap_n
            i1 <- which(df$.grp == idxLbl); i0 <- which(df$.grp == refLbl)
            wrb <- numeric(0)
            rows <- lapply(seq_len(nrow(df)), function(r) as.list(df[r, , drop = FALSE]))
            for (b in seq_len(B)) {
                s1 <- sample(i1, replace = TRUE); s0 <- sample(i0, replace = TRUE)
                W <- 0L; L <- 0L
                for (i in s1) { ai <- rows[[i]]
                    for (j in s0) {
                        r <- private$.comparePair(ai, rows[[j]], eps)
                        if (r[1] == 1L) W <- W + 1L else if (r[1] == -1L) L <- L + 1L
                    }
                }
                if (L > 0 && W > 0) wrb <- c(wrb, W / L)
            }
            if (length(wrb) < 10) return(c(NA_real_, NA_real_, NA_real_, NA_real_))
            a <- (1 - conf) / 2
            ci <- stats::quantile(wrb, c(a, 1 - a), names = FALSE)
            # bootstrap p-value from log-scale SE
            se <- stats::sd(log(wrb))
            zstat <- mean(log(wrb)) / se
            pval <- 2 * stats::pnorm(-abs(zstat))
            c(ci[1], ci[2], zstat, pval)
        },

        # ---- output populators ----------------------------------------------
        .populateMain = function(r) {
            tab <- self$results$mainTable
            tab$addRow(rowKey = "wr", values = list(
                statistic = "Win ratio",
                estimate = r$win_ratio, ci_lower = r$ci_low,
                ci_upper = r$ci_high, p = r$p))
            if (self$options$showWinOdds)
                tab$addRow(rowKey = "wo", values = list(
                    statistic = "Win odds",
                    estimate = r$win_odds, ci_lower = NA, ci_upper = NA, p = NA))
            if (self$options$showNetBenefit)
                tab$addRow(rowKey = "nb", values = list(
                    statistic = "Net benefit",
                    estimate = r$net_benefit, ci_lower = NA, ci_upper = NA, p = NA))
            note <- sprintf(
                "Index group '%s' vs reference '%s'. %s CI method; confidence level %.0f%%.",
                r$idxLbl, r$refLbl,
                if (self$options$ciMethod == "bootstrap")
                    sprintf("Bootstrap (%d reps)", self$options$bootstrap_n)
                else "Analytic (Dong et al. 2016)",
                100 * r$conf)
            tab$setNote("wr", note)
        },

        .populateCounts = function(r) {
            tab <- self$results$countsTable
            tab$addRow(rowKey = "w", values = list(
                label = sprintf("Wins (%s better)", r$idxLbl),
                count = r$W, pct = r$W / r$Ttot))
            tab$addRow(rowKey = "l", values = list(
                label = sprintf("Losses (%s better)", r$refLbl),
                count = r$L, pct = r$L / r$Ttot))
            tab$addRow(rowKey = "t", values = list(
                label = "Ties", count = r$Ttie, pct = r$Ttie / r$Ttot))
            tab$addRow(rowKey = "tot", values = list(
                label = "Total pairs", count = r$Ttot, pct = 1))
        },

        .populateComponents = function(r) {
            tab <- self$results$componentsTable
            decidedAll <- r$W + r$L
            for (k in seq_along(r$eps)) {
                dk <- r$comp_w[k] + r$comp_l[k]
                tab$addRow(rowKey = k, values = list(
                    endpoint = sprintf("%d. %s", k, r$eps[[k]]$label),
                    wins = r$comp_w[k], losses = r$comp_l[k],
                    decided = dk,
                    decided_pct = if (decidedAll > 0) dk / decidedAll else 0))
            }
        },

        .populateSummary = function(r) {
            wrTxt <- if (is.infinite(r$win_ratio)) "infinite (no losses)"
                     else sprintf("%.2f", r$win_ratio)
            ciTxt <- if (is.na(r$ci_low)) ""
                     else sprintf(" (%.0f%% CI %.2f&ndash;%.2f)",
                                  100 * r$conf, r$ci_low, r$ci_high)
            dir <- if (is.infinite(r$win_ratio) || r$win_ratio > 1) r$idxLbl else r$refLbl
            html <- glue::glue(
                "<p>Across {format(r$Ttot, big.mark=',')} index&ndash;reference pairs,
                the <b>{r$idxLbl}</b> group won {format(r$W, big.mark=',')} pairs and
                lost {format(r$L, big.mark=',')} ({format(r$Ttie, big.mark=',')} ties).
                The <b>win ratio was {wrTxt}</b>{ciTxt}, favouring <b>{dir}</b>. The net
                benefit was {sprintf('%.3f', r$net_benefit)}.</p>")
            self$results$summary$setContent(html)
        },

        .populateExplanation = function() {
            html <- "
                <h4>Win ratio methodology</h4>
                <p>The win ratio (Pocock et al., 2012) analyses a composite of outcomes
                ranked by clinical importance. Each subject in the index group is paired
                with each subject in the reference group. For each pair, endpoints are
                assessed in priority order; the first endpoint able to declare a winner
                does so. Time-to-event endpoints declare a winner only when one subject's
                event time is exceeded by the other subject's follow-up (so censored
                pairs that cannot be ordered remain ties on that endpoint).</p>
                <p>The <b>win ratio</b> is the number of wins divided by the number of
                losses. The <b>win odds</b> splits ties evenly and remains defined when
                there are no losses. The <b>net benefit</b> is the proportion of wins
                minus the proportion of losses.</p>
                <p>The analytic confidence interval uses the variance of the log win
                ratio derived by Dong et al. (2016) from the per-subject win and loss
                proportions; the bootstrap option resamples subjects within each group.</p>"
            self$results$explanation$setContent(html)
        },

        # ---- plot ------------------------------------------------------------
        .plot = function(image, ggtheme, theme, ...) {
            r <- image$state
            if (is.null(r)) return(FALSE)
            df <- data.frame(
                outcome = factor(c("Loss", "Tie", "Win"),
                                 levels = c("Loss", "Tie", "Win")),
                prop = c(r$L, r$Ttie, r$W) / r$Ttot)
            p <- ggplot2::ggplot(df,
                    ggplot2::aes(x = "", y = prop, fill = outcome)) +
                ggplot2::geom_col(width = 0.6) +
                ggplot2::coord_flip() +
                ggplot2::scale_fill_manual(values = c(
                    Win = "#2c7fb8", Tie = "#bdbdbd", Loss = "#de2d26")) +
                ggplot2::scale_y_continuous(labels = scales::percent) +
                ggplot2::labs(
                    x = NULL, y = "Proportion of pairs", fill = NULL,
                    title = sprintf("Win ratio = %s  (%s vs %s)",
                        if (is.infinite(r$win_ratio)) "Inf" else sprintf("%.2f", r$win_ratio),
                        r$idxLbl, r$refLbl)) +
                ggtheme +
                ggplot2::theme(legend.position = "bottom")
            print(p)
            TRUE
        }
    )
)
