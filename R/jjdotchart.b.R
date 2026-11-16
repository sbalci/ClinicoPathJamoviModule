#' @title Dot Chart (Summary vs Reference Value)
#' @importFrom R6 R6Class
#' @importFrom rlang sym .data
#' @importFrom jmvcore .
#'
#' @return An \code{R6} class generator object for the \code{jjdotchartClass}
#'   backend; used internally by the jamovi analysis wrapper and not called
#'   directly.


jjdotchartClass <- if (requireNamespace("jmvcore")) R6::R6Class(
    "jjdotchartClass",
    inherit = jjdotchartBase,
    public = list(
        #' @description Initialize analysis options and guard non-finite reference values.
        #' @param noThrow Logical; whether to catch initialization exceptions.
        init = function(noThrow = FALSE) {
            opt_tv <- self$options$option("testvalue")
            if (!is.null(opt_tv)) {
                tv <- opt_tv$value
                if (!is.numeric(tv) || length(tv) != 1L || is.na(tv))
                    opt_tv$value <- Inf
            }
            super$init(noThrow = noThrow)
        }
    ),
    private = list(

        .noticeList = list(),
        .inputsValid = FALSE,
        .prepared = NULL,
        .tab = NULL,
        .ostMain = NULL,
        .splitCache = NULL,

        .sourcifyOption = function(option) {
            if (!option$name %in% names(formals(jjdotchart))) return("")
            value <- option$value
            if (identical(value, option$default)) return("")
            if (is.list(value) || length(value) != 1L) return("")
            if (!(is.character(value) || is.numeric(value) || is.logical(value))) return("")
            if (is.na(value) || (is.numeric(value) && !is.finite(value))) return("")
            if (is.character(value))
                return(paste0(option$name, " = ", encodeString(value, quote = '"')))
            super$.sourcifyOption(option)
        },

        # Mirror of .inputsValid into every Image's state so .omv reopen and
        # right-click image export can render without re-running .run().
        .validHere = function(image) isTRUE(image$state),

        .publishValidity = function(valid) {
            for (nm in c("plot", "plot2")) {
                img <- self$results[[nm]]
                if (!is.null(img)) img$setState(isTRUE(valid))
            }
        },

        .clearTableRows = function(table) {
            if (is.null(table)) return(invisible(NULL))
            table$deleteRows()
            if (length(table$names) > 0)
                table$.__enclos_env__$private$.rowNames <- character()
            invisible(table)
        },

        .initWeightedTable = function() {
            wt <- self$results$weightsensitivity
            if (is.null(wt)) return(invisible(NULL))
            if (wt$rowCount == 0L) {
                wt$addRow(rowKey = "unweighted", values = list(
                    scheme = .("Unweighted group summaries (1/k each)")
                ))
                wt$addRow(rowKey = "obs_weighted", values = list(
                    scheme = .("Caseload-weighted summaries (n_g / N)")
                ))
                wt$addRow(rowKey = "re_meta", values = list(
                    scheme = .("Precision-weighted random-effects (DerSimonian-Laird)")
                ))
            }
            invisible(wt)
        },

        .resetWeightedTable = function() {
            wt <- self$results$weightsensitivity
            if (is.null(wt)) return(invisible(NULL))
            private$.initWeightedTable()
            empty_vals <- list(
                units    = "",
                estimate = NULL,
                se       = NULL,
                ci_low   = NULL,
                ci_high  = NULL,
                vsref    = NULL,
                pvalue   = NULL
            )
            wt$setRow(rowKey = "unweighted", values = c(
                list(scheme = .("Unweighted group summaries (1/k each)")),
                empty_vals
            ))
            wt$setRow(rowKey = "obs_weighted", values = c(
                list(scheme = .("Caseload-weighted summaries (n_g / N)")),
                empty_vals
            ))
            wt$setRow(rowKey = "re_meta", values = c(
                list(scheme = .("Precision-weighted random-effects (DerSimonian-Laird)")),
                empty_vals
            ))
            invisible(wt)
        },

        # ---- option sanitizers ------------------------------------------------

        .trimVal = function() {
            tr <- suppressWarnings(as.double(self$options$trimprop %||% 0.20)[1])
            if (length(tr) != 1L || !is.finite(tr)) return(0.20)
            min(max(tr, 0.05), 0.40)
        },

        .trimPctLabel = function() {
            base::format(round(100 * private$.trimVal(), 2), trim = TRUE)
        },

        .bfPriorVal = function() {
            r <- suppressWarnings(as.double(self$options$bfprior %||% 0.707)[1])
            if (length(r) != 1L || !is.finite(r)) return(0.707)
            min(max(r, 0.10), 2.00)
        },

        .nbootRobust = function() {
            b <- suppressWarnings(as.integer(self$options$nboot %||% 200L)[1])
            if (length(b) != 1L || is.na(b)) return(200L)
            min(max(b, 100L), 5000L)
        },

        .nbootMedian = function() {
            b <- suppressWarnings(as.integer(self$options$ciresamples %||% 2000L)[1])
            if (length(b) != 1L || is.na(b)) return(2000L)
            min(max(b, 200L), 10000L)
        },

        .nbootMap = function() {
            b <- suppressWarnings(as.integer(self$options$mapresamples %||% 500L)[1])
            if (length(b) != 1L || is.na(b)) return(500L)
            min(max(b, 100L), 5000L)
        },

        .confVal = function() {
            cf <- suppressWarnings(as.double(self$options$conflevel %||% 0.95)[1])
            if (length(cf) != 1L || !is.finite(cf)) return(0.95)
            min(max(cf, 0.50), 0.999)
        },

        .digitsVal = function() {
            dg <- suppressWarnings(as.integer(self$options$k %||% 2L)[1])
            if (length(dg) != 1L || is.na(dg)) return(2L)
            min(max(dg, 0L), 5L)
        },

        .plotWidthVal = function() {
            pw <- suppressWarnings(as.integer(self$options$plotwidth %||% 650L)[1])
            if (length(pw) != 1L || is.na(pw)) return(650L)
            min(max(pw, 300L), 1200L)
        },

        .plotHeightVal = function() {
            ph <- suppressWarnings(as.integer(self$options$plotheight %||% 450L)[1])
            if (length(ph) != 1L || is.na(ph)) return(450L)
            min(max(ph, 300L), 800L)
        },

        # ---- notices ----------------------------------------------------------

        # Notices are stored with their severity and sorted on every render so
        # ERROR always appears above WARNING and INFO regardless of call order.
        .addNotice = function(message, type = "INFO") {
            private$.noticeList[[length(private$.noticeList) + 1L]] <-
                list(type = type, message = message)
            private$.renderNotices()
        },

        .severityLabel = function(type) {
            switch(type,
                "ERROR"   = .("ERROR"),
                "WARNING" = .("WARNING"),
                .("INFO")
            )
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                return(invisible(NULL))
            }
            tiers <- c("ERROR", "WARNING", "INFO")
            rank <- vapply(private$.noticeList, function(n) {
                i <- match(n$type, tiers)
                if (is.na(i)) length(tiers) else i
            }, integer(1))

            html <- vapply(private$.noticeList[order(rank)], function(n) {
                style <- switch(n$type,
                    "ERROR"   = list(bg = "rgba(220, 53, 69, 0.12)",  br = "#dc3545"),
                    "WARNING" = list(bg = "rgba(255, 193, 7, 0.16)",  br = "#d39e00"),
                                list(bg = "rgba(13, 110, 253, 0.10)", br = "#0d6efd"))
                paste0(
                    "<div style='background-color:", style$bg,
                    "; border-left:4px solid ", style$br,
                    "; padding:12px; margin:8px 0; color:inherit;'>",
                    "<strong style='color:inherit;'>", private$.severityLabel(n$type), ":</strong> ",
                    n$message, "</div>")
            }, character(1))

            self$results$notices$setContent(paste(html, collapse = "\n"))
        },

        .clearNotices = function() {
            private$.noticeList <- list()
            self$results$notices$setContent("")
        },

        # ---- vocabulary -------------------------------------------------------

        .summaryLabel = function() {
            switch(as.character(self$options$typestatistics),
                   parametric    = .("Mean"),
                   nonparametric = .("Median"),
                   robust        = .fmt(.("Trimmed mean ({pct}%)"), pct = private$.trimPctLabel()),
                   bayes         = .("MAP estimate"),
                   .("Summary"))
        },

        .summaryLabelLower = function() {
            switch(as.character(self$options$typestatistics),
                   parametric    = .("mean"),
                   nonparametric = .("median"),
                   robust        = .fmt(.("trimmed mean ({pct}%)"), pct = private$.trimPctLabel()),
                   bayes         = .("MAP estimate"),
                   .("summary"))
        },

        .summaryLabelPluralLower = function() {
            switch(as.character(self$options$typestatistics),
                   parametric    = .("means"),
                   nonparametric = .("medians"),
                   robust        = .fmt(.("{pct}% trimmed means"), pct = private$.trimPctLabel()),
                   bayes         = .("MAP estimates"),
                   .("summaries"))
        },

        .ciMethodLabel = function() {
            switch(as.character(self$options$typestatistics),
                   parametric    = .("Student-t confidence interval"),
                   nonparametric = .fmt(.("percentile bootstrap interval (B = {b})"), b = private$.nbootMedian()),
                   robust        = .fmt(.("Yuen Winsorized-t interval ({pct}% trim)"), pct = private$.trimPctLabel()),
                   bayes         = .fmt(.("percentile bootstrap interval around the 1024-grid Sheather-Jones kernel mode (B = {b})"), b = private$.nbootMap()),
                   .("confidence interval"))
        },

        .testLabel = function() {
            switch(as.character(self$options$typestatistics),
                   parametric    = .("one-sample t-test"),
                   nonparametric = .("Wilcoxon signed-rank test"),
                   robust        = .("bootstrapped trimmed-mean test"),
                   bayes         = .("Bayesian one-sample test"),
                   .("one-sample test"))
        },

        .syncTableTitles = function() {
            t <- self$results$summary
            if (!is.null(t)) {
                col <- tryCatch(t$getColumn("value"), error = function(e) NULL)
                if (!is.null(col))
                    col$setTitle(private$.summaryLabel())
                if (is.null(self$options$grvar)) {
                    t$setTitle(.("Group Summaries (one row per plotted point)"))
                } else {
                    t$setTitle(.("Overall Group Summaries (unstratified across Split By)"))
                }
            }
            wt <- self$results$weightsensitivity
            if (!is.null(wt)) {
                wt$setTitle(.("Benchmarking Sensitivity Check (Weighting Comparison)"))
            }
            p1 <- self$results$plot
            if (!is.null(p1)) {
                if (!is.null(self$options$dep) && !is.null(self$options$group)) {
                    p1$setTitle(.fmt(
                        .("{dep} by {group}"),
                        dep   = self$options$dep,
                        group = self$options$group
                    ))
                } else {
                    p1$setTitle(.("Dot Chart"))
                }
            }
            p2 <- self$results$plot2
            if (!is.null(p2)) {
                if (!is.null(self$options$dep) && !is.null(self$options$group) &&
                    !is.null(self$options$grvar)) {
                    p2$setTitle(.fmt(
                        .("{dep} by {group}, split by {grvar}"),
                        dep   = self$options$dep,
                        group = self$options$group,
                        grvar = self$options$grvar
                    ))
                } else {
                    p2$setTitle(.("Dot Chart (Split By)"))
                }
            }
            invisible(NULL)
        },

        # ---- data preparation -------------------------------------------------

        # Convert numeric / haven_labelled inputs while honoring SPSS/Stata
        # user-defined missing codes (na_values / na_range) before coercion.
        .cleanNumeric = function(x) {
            if (inherits(x, "haven_labelled") || !is.null(attr(x, "na_values", exact = TRUE)) ||
                !is.null(attr(x, "na_range", exact = TRUE))) {
                na_vals <- attr(x, "na_values", exact = TRUE)
                na_rng  <- attr(x, "na_range", exact = TRUE)
                raw <- unclass(x)
                if (!is.null(na_vals))
                    raw[raw %in% na_vals] <- NA
                if (is.numeric(raw) && is.numeric(na_rng) && length(na_rng) == 2L)
                    raw[!is.na(raw) & raw >= na_rng[1] & raw <= na_rng[2]] <- NA
                x <- raw
            }
            num <- jmvcore::toNumeric(x)
            if (!is.numeric(num))
                num <- suppressWarnings(as.numeric(as.character(num)))
            num
        },

        # Convert factor/character/labelled inputs while preserving SPSS/Stata
        # value labels, honoring na_values/na_range, stripping explicit <NA>
        # levels created by addNA(), and dropping unused levels.
        .cleanFactor = function(x) {
            if (inherits(x, "haven_labelled") || !is.null(attr(x, "labels", exact = TRUE))) {
                lbls    <- attr(x, "labels", exact = TRUE)
                na_vals <- attr(x, "na_values", exact = TRUE)
                na_rng  <- attr(x, "na_range", exact = TRUE)
                raw <- unclass(x)
                if (!is.null(na_vals))
                    raw[raw %in% na_vals] <- NA
                if (is.numeric(raw) && is.numeric(na_rng) && length(na_rng) == 2L)
                    raw[!is.na(raw) & raw >= na_rng[1] & raw <= na_rng[2]] <- NA
                if (!is.null(lbls) && length(lbls) > 0L && !is.null(names(lbls))) {
                    m <- match(raw, unname(lbls))
                    ch <- ifelse(
                        is.na(raw),
                        NA_character_,
                        ifelse(!is.na(m), names(lbls)[m], as.character(raw))
                    )
                    decl <- names(lbls)[!is.na(names(lbls)) & nzchar(trimws(names(lbls)))]
                    extra <- ch[!is.na(ch) & nzchar(trimws(ch)) & !(ch %in% decl)]
                    kept_lvls <- unique(c(decl, extra))
                    ch[is.na(raw) | !nzchar(trimws(ch))] <- NA_character_
                    return(droplevels(factor(ch, levels = kept_lvls)))
                }
                x <- raw
            }
            ch <- as.character(x)
            if (is.factor(x)) {
                lv <- levels(x)
                idx <- as.integer(x)
                lvl_na <- is.na(lv)
                if (any(lvl_na))
                    ch[!is.na(idx) & lvl_na[idx]] <- NA_character_
                kept_lvls <- unique(lv[!lvl_na & nzchar(trimws(lv))])
            } else {
                kept_lvls <- unique(ch[!is.na(ch) & nzchar(trimws(ch))])
            }
            ch[is.na(x) | !nzchar(trimws(ch))] <- NA_character_
            droplevels(factor(ch, levels = kept_lvls))
        },

        .prepareData = function() {
            if (!is.null(private$.prepared)) return(private$.prepared)

            dep <- self$options$dep
            grp <- self$options$group
            grv <- self$options$grvar
            if (is.null(dep) || is.null(grp)) return(NULL)

            mydata <- self$data
            req_cols <- c(dep, grp)
            if (!is.null(grv)) req_cols <- c(req_cols, grv)
            if (!all(req_cols %in% names(mydata))) return(NULL)

            num <- private$.cleanNumeric(mydata[[dep]])
            grp_clean <- private$.cleanFactor(mydata[[grp]])

            # Filter unstratified data strictly on (dep, group) so optional Split By
            # missingness never deletes complete observations from main outputs.
            miss_base <- is.na(num) | is.nan(num) | is.na(grp_clean)
            n_dropped <- sum(miss_base)

            keep_nonmiss <- !miss_base
            num_nm <- num[keep_nonmiss]
            fin <- is.finite(num_nm)
            n_inf <- sum(!fin)

            base_df <- mydata[keep_nonmiss, , drop = FALSE][fin, , drop = FALSE]
            base_df[[dep]] <- num_nm[fin]
            base_df[[grp]] <- droplevels(grp_clean[keep_nonmiss][fin])

            split_df <- NULL
            n_grvar_na <- 0L
            if (!is.null(grv) && nrow(base_df) > 0L) {
                gr_clean <- private$.cleanFactor(base_df[[grv]])
                n_grvar_na <- sum(is.na(gr_clean))
                ok_gr <- !is.na(gr_clean)
                split_df <- base_df[ok_gr, , drop = FALSE]
                split_df[[grv]] <- droplevels(gr_clean[ok_gr])
            }

            private$.prepared <- list(
                data       = base_df,
                split_data = split_df,
                n_dropped  = n_dropped,
                n_inf      = n_inf,
                n_grvar_na = n_grvar_na
            )
            private$.prepared
        },

        # ---- statistical helpers ----------------------------------------------

        .safeSeedDouble = function(base_seed) {
            s <- suppressWarnings(as.double(base_seed)[1])
            if (length(s) != 1L || !is.finite(s)) 20250101 else floor(abs(s))
        },

        # Deterministic per-group seed derived from (seed, group_label) so
        # bootstrap intervals are invariant to factor-level declaration order.
        .groupSeed = function(base_seed, grp_label) {
            bytes <- as.integer(charToRaw(enc2utf8(as.character(grp_label))))
            h <- private$.safeSeedDouble(base_seed) %% 1000000007
            for (b in bytes) {
                h <- (h * 131 + b + 1) %% 2147483629
            }
            as.integer(h) + 1L
        },

        # 1024-grid Sheather-Jones kernel mode averaging tied grid maxima so
        # symmetric samples are free of lower-index tie-break bias and location shifts
        # remain equivariant.
        .kdeMode = function(x) {
            x <- x[is.finite(x)]
            if (length(x) == 0L) return(NA_real_)
            if (length(unique(x)) < 2L) return(mean(x))
            den <- tryCatch(
                stats::density(x, n = 1024L, bw = "SJ", from = min(x), to = max(x)),
                error = function(e) stats::density(x, n = 1024L, bw = "nrd0", from = min(x), to = max(x))
            )
            ymax <- max(den$y)
            mean(den$x[den$y >= ymax * (1 - 1e-12)])
        },

        .groupSummaryOne = function(x_raw, grp_label) {
            # Sort canonically within each group so resampling is invariant to row order.
            xg <- sort(as.numeric(x_raw[is.finite(x_raw)]))
            ng <- length(xg)
            if (ng == 0L) return(NULL)

            sd_g <- if (ng > 1L) stats::sd(xg) else NA_real_
            se_est <- NA_real_
            conf <- private$.confVal()
            alpha <- 1 - conf
            ty <- as.character(self$options$typestatistics)

            if (identical(ty, "parametric")) {
                val <- mean(xg)
                if (ng >= 2L) {
                    se_est <- sd_g / sqrt(ng)
                    hw <- stats::qt(1 - alpha / 2, df = ng - 1L) * se_est
                    ci_lo <- val - hw
                    ci_hi <- val + hw
                } else {
                    ci_lo <- val
                    ci_hi <- val
                }
            } else if (identical(ty, "nonparametric")) {
                val <- stats::median(xg)
                B_med <- private$.nbootMedian()
                if (ng >= 2L && stats::sd(xg) > 0) {
                    gseed <- private$.groupSeed(self$options$seed, grp_label)
                    boots <- withr::with_seed(gseed, {
                        if (ng <= 5000L && as.double(ng) * as.double(B_med) <= 1e7) {
                            idx <- matrix(sample.int(ng, size = ng * B_med, replace = TRUE),
                                          nrow = ng, ncol = B_med)
                            vapply(seq_len(B_med), function(b) stats::median(xg[idx[, b]]), numeric(1))
                        } else {
                            vapply(seq_len(B_med), function(b) {
                                stats::median(xg[sample.int(ng, size = ng, replace = TRUE)])
                            }, numeric(1))
                        }
                    })
                    se_est <- stats::sd(boots)
                    qs <- stats::quantile(boots, probs = c(alpha / 2, 1 - alpha / 2),
                                          names = FALSE, type = 7)
                    ci_lo <- min(qs[1], val)
                    ci_hi <- max(qs[2], val)
                } else {
                    ci_lo <- val
                    ci_hi <- val
                }
            } else if (identical(ty, "robust")) {
                tr <- private$.trimVal()
                val <- mean(xg, trim = tr)
                if (ng >= 2L) {
                    g_tr <- floor(tr * ng)
                    h_tr <- ng - 2L * g_tr
                    xw <- xg
                    if (g_tr > 0L) {
                        xw[seq_len(g_tr)] <- xg[g_tr + 1L]
                        xw[(ng - g_tr + 1L):ng] <- xg[ng - g_tr]
                    }
                    se_est <- sqrt(stats::var(xw)) / ((1 - 2 * tr) * sqrt(ng))
                    hw <- stats::qt(1 - alpha / 2, df = max(h_tr - 1L, 1L)) * se_est
                    ci_lo <- val - hw
                    ci_hi <- val + hw
                } else {
                    ci_lo <- val
                    ci_hi <- val
                }
            } else {
                val <- private$.kdeMode(xg)
                B_map <- private$.nbootMap()
                if (ng >= 2L && stats::sd(xg) > 0) {
                    gseed <- private$.groupSeed(self$options$seed, grp_label)
                    boots <- withr::with_seed(gseed, {
                        if (ng <= 5000L && as.double(ng) * as.double(B_map) <= 1e7) {
                            idx <- matrix(sample.int(ng, size = ng * B_map, replace = TRUE),
                                          nrow = ng, ncol = B_map)
                            vapply(seq_len(B_map), function(b) private$.kdeMode(xg[idx[, b]]), numeric(1))
                        } else {
                            vapply(seq_len(B_map), function(b) {
                                private$.kdeMode(xg[sample.int(ng, size = ng, replace = TRUE)])
                            }, numeric(1))
                        }
                    })
                    se_est <- stats::sd(boots)
                    qs <- stats::quantile(boots, probs = c(alpha / 2, 1 - alpha / 2),
                                          names = FALSE, type = 7)
                    ci_lo <- min(qs[1], val)
                    ci_hi <- max(qs[2], val)
                } else {
                    ci_lo <- val
                    ci_hi <- val
                }
            }

            data.frame(
                grp     = as.character(grp_label),
                n       = as.integer(ng),
                value   = as.numeric(val),
                sd      = as.numeric(sd_g),
                se_est  = as.numeric(se_est),
                ci_low  = as.numeric(ci_lo),
                ci_high = as.numeric(ci_hi),
                stringsAsFactors = FALSE,
                row.names = NULL
            )
        },

        .groupTableFor = function(mydata) {
            dep <- self$options$dep
            grp <- self$options$group
            if (is.null(mydata) || nrow(mydata) == 0L) return(NULL)

            f_grp <- droplevels(as.factor(mydata[[grp]]))
            lvls <- levels(f_grp)
            if (length(lvls) == 0L) return(NULL)

            rows <- lapply(lvls, function(lv) {
                private$.groupSummaryOne(mydata[[dep]][f_grp == lv], lv)
            })
            rows <- Filter(Negate(is.null), rows)
            if (length(rows) == 0L) return(NULL)
            out <- do.call(rbind, rows)
            rownames(out) <- NULL
            out
        },

        .groupTable = function(mydata) {
            if (!is.null(private$.tab)) return(private$.tab)
            private$.tab <- private$.groupTableFor(mydata)
            private$.tab
        },

        .mainOst = function(values) {
            if (!is.null(private$.ostMain)) return(private$.ostMain)
            private$.ostMain <- private$.oneSampleStats(values, seed_offset = 0L)
            private$.ostMain
        },

        .splitPanelsData = function(prep) {
            if (!is.null(private$.splitCache)) return(private$.splitCache)
            grv <- self$options$grvar
            grp <- self$options$group
            sdf <- prep$split_data
            if (is.null(grv) || is.null(sdf) || nrow(sdf) == 0L) return(NULL)
            lvls <- levels(sdf[[grv]])
            if (length(lvls) == 0L) return(NULL)
            need_ost <- isTRUE(self$options$resultssubtitle)
            private$.splitCache <- lapply(seq_along(lvls), function(idx) {
                slv <- lvls[idx]
                sub_df <- sdf[sdf[[grv]] == slv, , drop = FALSE]
                sub_df[[grp]] <- droplevels(sub_df[[grp]])
                tab_s <- private$.groupTableFor(sub_df)
                ks <- if (is.null(tab_s)) 0L else nrow(tab_s)
                ost_s <- if (ks >= 2L && need_ost) {
                    private$.oneSampleStats(tab_s$value, seed_offset = idx * 1009L)
                } else {
                    NULL
                }
                list(level = slv, tab = tab_s, ks = ks, ost = ost_s)
            })
            private$.splitCache
        },

        # Compute and validate the one-sample test across the k group summaries.
        # Runs under the exact same deterministic seed in both .run() preflight and
        # .plot()/.plot2() so preflight and rendered plot never diverge.
        .oneSampleStats = function(values, seed_offset = 0L) {
            values <- as.numeric(values)
            if (length(values) < 2L || any(!is.finite(values))) {
                return(list(ok = FALSE, reason = .("fewer than two finite group summaries are available")))
            }

            tv <- self$options$testvalue
            conf <- private$.confVal()
            dig <- private$.digitsVal()
            ty <- as.character(self$options$typestatistics)
            tol0 <- 1e-14 * max(1, abs(tv), max(abs(values)))

            if (all(abs(values - tv) <= tol0)) {
                return(list(
                    ok = FALSE,
                    reason = .("all group summaries equal the Reference Value (zero discrepancy and zero between-group variance)")
                ))
            }
            if (stats::sd(values) <= tol0) {
                return(list(
                    ok = FALSE,
                    reason = .("all group summaries are identical (zero between-group variance)")
                ))
            }

            ost_seed <- as.integer((private$.safeSeedDouble(self$options$seed) + as.double(seed_offset)) %% 2147483629) + 1L
            fmt <- function(v) sprintf(paste0("%.", dig, "f"), v)
            ci_pct <- paste0(base::format(100 * conf, trim = TRUE), "%")

            if (identical(ty, "parametric")) {
                res <- tryCatch(
                    withr::with_seed(ost_seed, suppressWarnings(
                        statsExpressions::one_sample_test(
                            data       = data.frame(.v = values),
                            x          = !!rlang::sym(".v"),
                            type       = "parametric",
                            test.value = tv,
                            conf.level = conf,
                            digits     = dig
                        )
                    )),
                    error = function(e) e
                )
                if (inherits(res, "condition"))
                    return(list(ok = FALSE, reason = conditionMessage(res)))
                if (is.null(res$expression) || length(res$expression) == 0L)
                    return(list(ok = FALSE, reason = .("the statistics engine returned no expression")))
                nums <- c(res$statistic[1], res$p.value[1], res$estimate[1], res$conf.low[1], res$conf.high[1])
                if (any(!is.finite(nums)))
                    return(list(ok = FALSE, reason = .("non-finite test statistic or effect-size interval")))
                return(list(
                    ok         = TRUE,
                    expression = res$expression[[1]],
                    p.value    = res$p.value[1],
                    warnings   = character(0)
                ))
            }

            if (identical(ty, "nonparametric")) {
                d <- values - tv
                d_nz <- d[abs(d) > tol0]
                ke <- length(d_nz)
                if (ke < 2L) {
                    return(list(
                        ok = FALSE,
                        reason = .("fewer than two non-zero differences from the Reference Value remain after removing exact ties")
                    ))
                }
                rk <- rank(abs(d_nz))
                V_stat <- sum(rk[d_nz > 0])
                M <- ke * (ke + 1) / 2
                r_rb <- 2 * (V_stat / M) - 1
                ties <- as.numeric(table(rk))
                var_V <- ke * (ke + 1) * (2 * ke + 1) / 24 - sum(ties^3 - ties) / 48
                if (!is.finite(var_V) || var_V <= 0) {
                    return(list(ok = FALSE, reason = .("signed-rank variance across group summaries is zero")))
                }
                wt <- suppressWarnings(stats::wilcox.test(values, mu = tv, exact = FALSE, correct = TRUE))
                p_val <- unname(wt$p.value[1])
                if (!is.finite(p_val)) {
                    return(list(ok = FALSE, reason = .("Wilcoxon signed-rank p-value is undefined")))
                }

                # Delta-method Fisher-z interval including the (1 - r^2)^-1 Jacobian.
                se_r0 <- 2 * sqrt(var_V) / M
                r_clamp <- max(min(r_rb, 1 - 1 / (2 * M + 2)), -1 + 1 / (2 * M + 2))
                se_z <- se_r0 / (1 - r_clamp^2)
                z_crit <- stats::qnorm(1 - (1 - conf) / 2)
                ci <- tanh(atanh(r_clamp) + c(-1, 1) * z_crit * se_z)

                expr <- bquote(list(
                    italic("V")["Wilcoxon"] == .(fmt(V_stat)),
                    italic(p) == .(fmt(p_val)),
                    widehat(italic("r"))["biserial"]^"rank" == .(fmt(r_rb)),
                    CI[.(ci_pct)] ~ "[" * .(fmt(ci[1])), .(fmt(ci[2])) * "]",
                    italic("n")["obs"] == .(as.character(length(values)))
                ))
                return(list(
                    ok         = TRUE,
                    expression = expr,
                    p.value    = p_val,
                    ci_low     = ci[1],
                    ci_high    = ci[2],
                    warnings   = character(0)
                ))
            }

            if (identical(ty, "robust")) {
                tr <- private$.trimVal()
                k_v <- length(values)
                g_v <- floor(tr * k_v)
                h_v <- k_v - 2L * g_v
                xs_v <- sort(values)
                xw_v <- xs_v
                if (g_v > 0L) {
                    xw_v[seq_len(g_v)] <- xs_v[g_v + 1L]
                    xw_v[(k_v - g_v + 1L):k_v] <- xs_v[k_v - g_v]
                }
                se_v <- sqrt(stats::var(xw_v)) / ((1 - 2 * tr) * sqrt(k_v))
                if (!is.finite(se_v) || se_v <= tol0) {
                    return(list(
                        ok = FALSE,
                        reason = .fmt(.("the {pct}% Winsorized standard error across group summaries is zero"), pct = private$.trimPctLabel())
                    ))
                }

                nboot <- private$.nbootRobust()
                tc <- tryCatch(
                    withr::with_seed(ost_seed, suppressWarnings(
                        WRS2::trimcibt(values, nv = tv, tr = tr, alpha = 1 - conf, nboot = nboot)
                    )),
                    error = function(e) e
                )
                if (inherits(tc, "condition"))
                    return(list(ok = FALSE, reason = conditionMessage(tc)))

                t_stat <- as.numeric(tc$test.stat[1])
                est <- as.numeric(tc$estimate[1])
                raw_p <- as.numeric(tc$p.value[1])
                if (!is.finite(t_stat) || !is.finite(est) || !is.finite(raw_p)) {
                    return(list(ok = FALSE, reason = .("bootstrapped trimmed-mean test returned non-finite estimates")))
                }

                warns <- character(0)
                min_p <- 1 / (nboot + 1L)
                floored_p <- FALSE
                if (raw_p < min_p) {
                    p_val <- min_p
                    floored_p <- TRUE
                    warns <- c(warns, .fmt(
                        .("Zero bootstrap replicates exceeded the observed trimmed-t statistic across B = {b} resamples; the Monte Carlo p-value is bounded at 1/(B + 1) = {minp} (Phipson & Smyth, 2010) rather than reporting machine underflow."),
                        b    = nboot,
                        minp = sprintf("%.4f", min_p)
                    ))
                } else {
                    p_val <- raw_p
                }

                ci <- as.numeric(tc$ci)
                if (length(ci) < 2L || any(!is.finite(ci))) {
                    df_w <- max(h_v - 1L, 1L)
                    hw_v <- stats::qt(1 - (1 - conf) / 2, df = df_w) * se_v
                    ci <- c(est - hw_v, est + hw_v)
                    warns <- c(warns, .fmt(
                        .("With k = {kv} groups, percentile-t bootstrap resamples produced zero Winsorized variance in tail draws (infinite bootstrap CI); analytic Winsorized-t {cp}% confidence limits (df = {dfw}) are substituted, and the bootstrap p-value should be interpreted with caution."),
                        kv  = k_v,
                        cp  = base::format(100 * conf, trim = TRUE),
                        dfw = df_w
                    ))
                }

                fmt_p <- fmt(p_val)
                if (floored_p && suppressWarnings(as.numeric(fmt_p)) < min_p)
                    fmt_p <- sprintf("%.4f", ceiling(min_p * 1e4) / 1e4)

                expr <- bquote(list(
                    italic("t")["bootstrapped"] == .(fmt(t_stat)),
                    italic(p) == .(fmt_p),
                    widehat(mu)["trimmed"] == .(fmt(est)),
                    CI[.(ci_pct)] ~ "[" * .(fmt(ci[1])), .(fmt(ci[2])) * "]",
                    italic("n")["obs"] == .(as.character(k_v))
                ))
                return(list(
                    ok         = TRUE,
                    expression = expr,
                    p.value    = p_val,
                    ci_low     = ci[1],
                    ci_high    = ci[2],
                    warnings   = warns
                ))
            }

            # Bayesian one-sample Cauchy-JZS test
            bf_r <- private$.bfPriorVal()
            res <- tryCatch(
                withr::with_seed(ost_seed, suppressWarnings(
                    statsExpressions::one_sample_test(
                        data       = data.frame(.v = values),
                        x          = !!rlang::sym(".v"),
                        type       = "bayes",
                        test.value = tv,
                        conf.level = conf,
                        digits     = dig,
                        bf.prior   = bf_r
                    )
                )),
                error = function(e) e
            )
            if (inherits(res, "condition"))
                return(list(ok = FALSE, reason = conditionMessage(res)))
            if (is.null(res$expression) || length(res$expression) == 0L)
                return(list(ok = FALSE, reason = .("the Bayesian engine returned no expression")))
            nums <- c(res$bf10[1], res$log_e_bf10[1], res$estimate[1], res$conf.low[1], res$conf.high[1])
            if (any(!is.finite(nums)))
                return(list(ok = FALSE, reason = .("non-finite Bayes factor or posterior interval")))

            list(
                ok         = TRUE,
                expression = res$expression[[1]],
                p.value    = NA_real_,
                warnings   = character(0)
            )
        },

        .centralityStat = function(values, ctype, digits) {
            ty <- as.character(ctype)
            val <- switch(ty,
                parametric    = mean(values),
                nonparametric = stats::median(values),
                robust        = mean(values, trim = private$.trimVal()),
                bayes         = private$.kdeMode(values),
                mean(values)
            )
            sub_lbl <- switch(ty,
                parametric    = "mean",
                nonparametric = "median",
                robust        = "trimmed",
                bayes         = "MAP",
                "mean"
            )
            fmt_val <- sprintf(paste0("%.", digits, "f"), val)
            expr <- as.expression(bquote(widehat(mu)[.(sub_lbl)] == .(fmt_val)))
            list(val = val, expr = expr)
        },

        # ---- validation -------------------------------------------------------

        .validate = function(prep) {
            mydata <- prep$data

            if (nrow(mydata) == 0L) {
                private$.addNotice(
                    .("No rows remain after removing missing values. Check the selected variables."),
                    "ERROR"
                )
                return(FALSE)
            }

            tv <- self$options$testvalue
            if (!is.numeric(tv) || length(tv) != 1L || !is.finite(tv)) {
                private$.addNotice(
                    .("Reference value must be a finite number."),
                    "ERROR"
                )
                return(FALSE)
            }
            if (prep$n_inf > 0L) {
                private$.addNotice(.fmt(
                    .("{ninf} row(s) had an infinite value for '{dep}' and were excluded. Infinite values usually mean a division by zero or an out-of-range entry - check the source data."),
                    ninf = prep$n_inf,
                    dep  = htmltools::htmlEscape(self$options$dep)
                ), "WARNING")
            }

            tab <- private$.groupTable(mydata)
            if (is.null(tab)) {
                private$.addNotice(
                    .("The group summaries could not be computed for this data. Check that the Measurement variable is numeric and that each group has usable observations."),
                    "ERROR"
                )
                return(FALSE)
            }
            k <- nrow(tab)

            if (k < 2L) {
                private$.addNotice(.fmt(
                    .("Only {k} group has data. A dot chart needs at least two groups, and the statistical test needs at least three to be meaningful."),
                    k = k
                ), "ERROR")
                return(FALSE)
            }

            private$.addNotice(.fmt(
                .("Each of the {k} groups is reduced to one point (its {stat}), so the {test} runs on {k} group summaries, NOT on the {nobs} observations. This tests the unweighted group summaries against the Reference Value; it does not compare the groups with each other."),
                k    = k,
                stat = private$.summaryLabelLower(),
                test = private$.testLabel(),
                nobs = nrow(mydata)
            ), "INFO")

            ty <- as.character(self$options$typestatistics)
            tr <- private$.trimVal()
            h_eff <- k - 2L * floor(tr * k)

            if (k < 4L) {
                private$.addNotice(.fmt(
                    .("With only {k} groups, inference from their summaries is imprecise; treat the p-value and effect estimate with caution."),
                    k = k
                ), "WARNING")
            } else if (identical(ty, "robust") && (k < 8L || h_eff < 5L)) {
                private$.addNotice(.fmt(
                    .("With k = {k} groups, {pct}% symmetric trimming retains only {heff} central group summaries; trimmed-mean inference has low precision when fewer than 5 untrimmed summaries remain."),
                    k    = k,
                    pct  = private$.trimPctLabel(),
                    heff = h_eff
                ), "WARNING")
            }

            if (identical(ty, "nonparametric") && k < 50L) {
                private$.addNotice(.fmt(
                    .("The Wilcoxon signed-rank p-value is computed by normal approximation with a continuity correction, not the exact distribution, so it can differ from R's wilcox.test() default at this size (k = {k}). Near a 0.05 threshold the two can disagree; treat a borderline p-value as borderline."),
                    k = k
                ), "WARNING")
            }

            if (identical(ty, "bayes")) {
                private$.addNotice(.fmt(
                    .("Bayesian mode plots each group's 1024-grid Sheather-Jones kernel mode (MAP estimate) with a percentile bootstrap interval (B = {bmap}), and tests those k group modes with a Gaussian Cauchy-JZS Bayes factor (r = {bfr}). Positive log_e(BF_01) values favor the null hypothesis (group-summary mean equals the Reference Value); negative values favor a difference."),
                    bmap = private$.nbootMap(),
                    bfr  = base::format(private$.bfPriorVal(), trim = TRUE)
                ), "INFO")
            }

            n_small <- sum(tab$n < 3L)
            if (n_small > 0L) {
                private$.addNotice(.fmt(
                    .("{nsmall} group(s) have fewer than 3 observations. Their {stat} is unstable, a single-observation group is drawn with a zero-width interval, and each group still counts equally in the {test}."),
                    nsmall = n_small,
                    stat   = private$.summaryLabelLower(),
                    test   = private$.testLabel()
                ), "WARNING")
            }

            if (min(tab$n) > 0L && max(tab$n) >= 3L * min(tab$n)) {
                private$.addNotice(.fmt(
                    .("Group sizes are highly unequal ({nmin} to {nmax} observations), but every group contributes one equally weighted point (1/{k}), so smaller groups influence the test as much as high-volume groups. Enable 'Weighted sensitivity table' under Output to compare unweighted, caseload-weighted, and random-effects precision-weighted estimates."),
                    nmin = min(tab$n),
                    nmax = max(tab$n),
                    k    = k
                ), "WARNING")
            }

            unw_center <- mean(tab$value)
            w_center <- stats::weighted.mean(tab$value, tab$n)
            if (is.finite(unw_center) && is.finite(w_center) &&
                (unw_center - tv) * (w_center - tv) < 0) {
                private$.addNotice(.fmt(
                    .("The unweighted mean of the {k} group summaries ({unw}) and the observation-weighted mean across all {ntot} observations ({wt}) lie on OPPOSITE sides of the Reference Value ({tv}). Equal-weight cluster testing answers a different question than patient-weighted compliance; inspect 'Weighted sensitivity table' under Output."),
                    k    = k,
                    unw  = base::format(signif(unw_center, 4)),
                    ntot = sum(tab$n),
                    wt   = base::format(signif(w_center, 4)),
                    tv   = base::format(tv)
                ), "WARNING")
            }

            valid_se <- tab$n >= 2L & is.finite(tab$se_est) & tab$se_est > 0
            if (sum(valid_se) >= 2L) {
                grp_se <- tab$se_est[valid_se]
                se_ratio <- max(grp_se) / min(grp_se)
                if (is.finite(se_ratio) && se_ratio >= 5) {
                    private$.addNotice(.fmt(
                        .("Within-group standard errors vary widely across groups ({se1} to {se2}, {ratio}x ratio), whereas the {test} weights all {k} group summaries equally without inverse-variance weighting; enable 'Weighted sensitivity table' under Output to inspect the DerSimonian-Laird random-effects precision-weighted synthesis."),
                        se1   = base::format(signif(min(grp_se), 3)),
                        se2   = base::format(signif(max(grp_se), 3)),
                        ratio = sprintf("%.1f", se_ratio),
                        test  = private$.testLabel(),
                        k     = k
                    ), "WARNING")
                }
            }

            rng <- range(tab$value)
            if (tv < rng[1] || tv > rng[2]) {
                span <- diff(rng)
                far <- span == 0 || min(abs(tv - rng)) > 3 * span
                if (far) {
                    private$.addNotice(.fmt(
                        .("The Reference Value ({tv}) lies outside the range of the group {stats} ({r1} to {r2}). That is far outside, which can make the standardized effect size impossible to bound and leave the plot with no statistics at all - retain a scientifically prespecified reference and consider a different inferential method."),
                        tv    = base::format(tv),
                        stats = private$.summaryLabelPluralLower(),
                        r1    = base::format(signif(rng[1], 4)),
                        r2    = base::format(signif(rng[2], 4))
                    ), "WARNING")
                } else {
                    private$.addNotice(.fmt(
                        .("The Reference Value ({tv}) lies outside the range of the group {stats} ({r1} to {r2})."),
                        tv    = base::format(tv),
                        stats = private$.summaryLabelPluralLower(),
                        r1    = base::format(signif(rng[1], 4)),
                        r2    = base::format(signif(rng[2], 4))
                    ), "INFO")
                }
            }

            TRUE
        },

        .validateSplitBy = function(prep) {
            grv <- self$options$grvar
            if (is.null(grv)) return(invisible(NULL))

            if (prep$n_grvar_na > 0L) {
                private$.addNotice(.fmt(
                    .("{nna} row(s) had missing values on Split By ('{grv}') and were excluded from the split chart only; the unstratified table and chart retain all {nkeep} complete observations."),
                    nna   = prep$n_grvar_na,
                    grv   = htmltools::htmlEscape(grv),
                    nkeep = nrow(prep$data)
                ), "INFO")
            }

            sdf <- prep$split_data
            if (is.null(sdf) || nrow(sdf) == 0L) {
                if (!is.null(self$results$plot2))
                    self$results$plot2$setState(FALSE)
                private$.addNotice(.fmt(
                    .("No complete rows remain for Split By ('{grv}'); the split chart cannot be drawn."),
                    grv = htmltools::htmlEscape(grv)
                ), "ERROR")
                return(invisible(NULL))
            }

            p_list <- private$.splitPanelsData(prep)
            one_grp_lvls <- character(0)
            low_k_info <- character(0)
            failed_lvls <- character(0)

            for (item in p_list) {
                slv <- item$level
                ks <- item$ks

                if (ks < 2L) {
                    one_grp_lvls <- c(one_grp_lvls, sprintf("'%s' (k = %d)", htmltools::htmlEscape(slv), ks))
                } else {
                    if (ks < 4L) {
                        low_k_info <- c(low_k_info, .fmt(
                            .("'{slv}' ({ks} groups, df = {df})"),
                            slv = htmltools::htmlEscape(slv),
                            ks  = ks,
                            df  = ks - 1L
                        ))
                    }
                    if (!is.null(item$ost)) {
                        ost_s <- item$ost
                        if (!isTRUE(ost_s$ok)) {
                            failed_lvls <- c(failed_lvls, sprintf(
                                "'%s' (%s)",
                                htmltools::htmlEscape(slv),
                                htmltools::htmlEscape(ost_s$reason)
                            ))
                        } else if (length(ost_s$warnings) > 0L) {
                            for (wmsg in ost_s$warnings) {
                                private$.addNotice(.fmt(
                                    .("Split By '{slv}': {wmsg}"),
                                    slv  = htmltools::htmlEscape(slv),
                                    wmsg = htmltools::htmlEscape(wmsg)
                                ), "WARNING")
                            }
                        }
                    }
                }
            }

            if (length(one_grp_lvls) > 0L) {
                private$.addNotice(.fmt(
                    .("Split By stratum {lvls} has fewer than 2 groups with data; a dot chart and one-sample test require at least 2 groups per panel."),
                    lvls = paste(one_grp_lvls, collapse = ", ")
                ), "ERROR")
            }
            if (length(low_k_info) > 0L) {
                private$.addNotice(.fmt(
                    .("Split By runs a separate test inside each panel with fewer than 4 groups in {lvls}; per-panel p-values and effect sizes have very low degrees of freedom and should be treated with caution."),
                    lvls = paste(low_k_info, collapse = ", ")
                ), "WARNING")
            }
            if (length(failed_lvls) > 0L) {
                private$.addNotice(.fmt(
                    .("Split By panel statistics could not be computed for {lvls}, so the corresponding panel subtitle is omitted."),
                    lvls = paste(failed_lvls, collapse = "; ")
                ), "ERROR")
            }
            invisible(NULL)
        },

        # ---- lifecycle --------------------------------------------------------

        .syncPlotSizes = function() {
            w <- private$.plotWidthVal()
            h <- private$.plotHeightVal()
            self$results$plot$setSize(w, h)
            if (!is.null(self$options$grvar) && self$options$grvar %in% names(self$data)) {
                n <- nlevels(private$.cleanFactor(self$data[[self$options$grvar]]))
                self$results$plot2$setSize(min(max(n, 1L) * w, 3000L), h)
            }
        },

        .init = function() {
            private$.syncTableTitles()
            private$.initWeightedTable()
            private$.syncPlotSizes()
        },

        .run = function() {
            private$.clearNotices()
            private$.clearTableRows(self$results$summary)
            private$.resetWeightedTable()
            private$.syncTableTitles()
            private$.syncPlotSizes()
            private$.inputsValid <- FALSE
            private$.publishValidity(FALSE)
            private$.prepared <- NULL
            private$.tab <- NULL
            private$.ostMain <- NULL
            private$.splitCache <- NULL

            if (is.null(self$options$dep) || is.null(self$options$group)) {
                missing_slots <- c(
                    if (is.null(self$options$dep)) .("Measurement"),
                    if (is.null(self$options$group)) .("Groups (One Point Each)")
                )
                self$results$todo$setContent(paste0(
                    "<div class='jmv-welcome' style='padding: 15px; background-color: rgba(33, 85, 151, 0.08); border-left: 4px solid #007bff; margin: 10px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0; color: inherit;'>", .("Dot Chart (Summary vs Reference Value)"), "</h4>",
                    "<p>",
                    .("This tool draws a <strong>Cleveland dot chart</strong>: each group becomes ONE point, and those points are tested against a Reference Value you choose."),
                    "</p>",
                    "<p>",
                    .("<strong>The test's sample size is the number of groups, not the number of patients.</strong> To compare groups with each other using every observation, use <strong>Box-Violin Plots to Compare Between Groups</strong> instead."),
                    "</p>",
                    "<p><strong>", .("Required to start:"), "</strong> ",
                    paste(missing_slots, collapse = " + "), ".</p>",
                    "<p style='margin-bottom: 0;'>",
                    .("Documentation:"), " ",
                    "<a href='https://www.indrapatil.com/ggstatsplot/reference/ggdotplotstats.html' target='_blank'>ggdotplotstats</a>",
                    " \u2022 ",
                    "<a href='https://www.indrapatil.com/ggstatsplot/reference/grouped_ggdotplotstats.html' target='_blank'>grouped_ggdotplotstats</a>",
                    "</p></div>"))
                return()
            }
            self$results$todo$setContent("")

            sel_vars <- c(self$options$dep, self$options$group, self$options$grvar)
            if (anyDuplicated(sel_vars) > 0L) {
                private$.addNotice(
                    .("Measurement, Groups, and Split By must be distinct variables."),
                    "ERROR"
                )
                return()
            }

            if (nrow(self$data) == 0L) {
                private$.addNotice(.("Data contains no rows."), "ERROR")
                return()
            }

            prep <- private$.prepareData()
            if (is.null(prep)) return()
            if (prep$n_dropped > 0L) {
                private$.addNotice(.fmt(
                    .("{ndrop} row(s) with missing values were excluded."),
                    ndrop = prep$n_dropped
                ), "INFO")
            }

            if (!private$.validate(prep)) return()
            private$.inputsValid <- TRUE
            private$.publishValidity(TRUE)

            tab <- private$.groupTable(prep$data)
            private$.fillTable(tab)
            private$.fillWeightedTable(tab)

            if (isTRUE(self$options$resultssubtitle)) {
                ost <- private$.mainOst(tab$value)
                if (!isTRUE(ost$ok)) {
                    private$.addNotice(.fmt(
                        .("The statistics could not be computed for this data ({reason}), so the chart carries NO test result - only the points. This usually means the Reference Value is far from the group {stats} or the group summaries have zero variance; retain the scientifically prespecified reference value and consider a different inferential method."),
                        reason = htmltools::htmlEscape(ost$reason),
                        stats  = private$.summaryLabelPluralLower()
                    ), "ERROR")
                } else if (length(ost$warnings) > 0L) {
                    for (wmsg in ost$warnings) {
                        private$.addNotice(htmltools::htmlEscape(wmsg), "WARNING")
                    }
                }
            }

            private$.validateSplitBy(prep)
        },

        .fillTable = function(tab) {
            t <- self$results$summary
            if (is.null(tab) || is.null(t)) return()
            private$.syncTableTitles()

            t$setNote("seed", .fmt(.("Random seed: {seed}"), seed = self$options$seed))
            scope_prefix <- if (is.null(self$options$grvar)) {
                .("Each row is one plotted point on the main chart.")
            } else {
                .("Each row summarizes one overall group across the full complete-case cohort (unstratified by Split By).")
            }
            t$setNote("agg", .fmt(
                .("{scope} '{stat}' is the {statlow} of that group's observations, and Lower/Upper are the {cp}% {cimethod} drawn as that point's horizontal error bar. 'SD' is the standard deviation of the raw observations. 'vs Reference' is {stat} minus the Reference Value ({tv})."),
                scope    = scope_prefix,
                stat     = private$.summaryLabel(),
                statlow  = private$.summaryLabelLower(),
                cp       = base::format(100 * private$.confVal(), trim = TRUE),
                cimethod = private$.ciMethodLabel(),
                tv       = base::format(self$options$testvalue)
            ))

            private$.clearTableRows(t)
            for (i in seq_len(nrow(tab))) {
                t$addRow(rowKey = i, values = list(
                    grp     = tab$grp[i],
                    n       = tab$n[i],
                    value   = tab$value[i],
                    sd      = tab$sd[i],
                    ci_low  = tab$ci_low[i],
                    ci_high = tab$ci_high[i],
                    vsref   = tab$value[i] - self$options$testvalue
                ))
            }
        },

        .fillWeightedTable = function(tab) {
            wt <- self$results$weightsensitivity
            if (is.null(tab) || is.null(wt)) return(invisible(NULL))
            private$.initWeightedTable()

            k <- nrow(tab)
            tv <- self$options$testvalue
            conf <- private$.confVal()
            alpha <- 1 - conf
            tol0 <- 1e-14 * max(1, abs(tv), max(abs(tab$value)))
            t_crit <- stats::qt(1 - alpha / 2, df = k - 1L)
            z_crit <- stats::qnorm(1 - alpha / 2)

            # 1. Equal-weight (1/k) group summary benchmark
            unw_est <- mean(tab$value)
            s_between <- stats::sd(tab$value)
            if (is.finite(s_between) && s_between > tol0) {
                unw_err <- s_between / sqrt(k)
                unw_lo <- unw_est - t_crit * unw_err
                unw_hi <- unw_est + t_crit * unw_err
                unw_p <- 2 * stats::pt(-abs((unw_est - tv) / unw_err), df = k - 1L)
            } else {
                unw_err <- NULL
                unw_lo <- NULL
                unw_hi <- NULL
                unw_p <- NULL
            }
            wt$setRow(rowKey = "unweighted", values = list(
                scheme   = .("Unweighted group summaries (1/k each)"),
                units    = .fmt(.("{k} groups"), k = k),
                estimate = unw_est,
                se       = unw_err,
                ci_low   = unw_lo,
                ci_high  = unw_hi,
                vsref    = unw_est - tv,
                pvalue   = unw_p
            ))

            # 2. Caseload-weighted (n_g / N) cluster-robust CR1 synthesis
            n_tot <- sum(tab$n)
            w_obs <- tab$n / n_tot
            obs_est <- sum(w_obs * tab$value)
            cr1_sq <- (k / (k - 1L)) * sum((w_obs^2) * ((tab$value - obs_est)^2))
            if (is.finite(cr1_sq) && sqrt(cr1_sq) > tol0) {
                obs_err <- sqrt(cr1_sq)
                obs_lo <- obs_est - t_crit * obs_err
                obs_hi <- obs_est + t_crit * obs_err
                obs_p <- 2 * stats::pt(-abs((obs_est - tv) / obs_err), df = k - 1L)
            } else {
                obs_err <- NULL
                obs_lo <- NULL
                obs_hi <- NULL
                obs_p <- NULL
            }
            wt$setRow(rowKey = "obs_weighted", values = list(
                scheme   = .("Caseload-weighted summaries (n_g / N)"),
                units    = .fmt(.("{ntot} obs in {k} groups"), ntot = n_tot, k = k),
                estimate = obs_est,
                se       = obs_err,
                ci_low   = obs_lo,
                ci_high  = obs_hi,
                vsref    = obs_est - tv,
                pvalue   = obs_p
            ))

            # 3. DerSimonian-Laird random-effects precision-weighted synthesis
            ok_re <- tab$n >= 2L & is.finite(tab$se_est) & tab$se_est > tol0
            m_re <- sum(ok_re)
            if (m_re >= 2L) {
                yi <- tab$value[ok_re]
                vi <- tab$se_est[ok_re]^2
                wi_fe <- 1 / vi
                sum_w <- sum(wi_fe)
                mu_fe <- sum(wi_fe * yi) / sum_w
                Q_stat <- sum(wi_fe * ((yi - mu_fe)^2))
                c_dl <- sum_w - sum(wi_fe^2) / sum_w
                tau2_dl <- if (is.finite(c_dl) && c_dl > 0) max(0, (Q_stat - (m_re - 1L)) / c_dl) else 0
                i2_pct <- if (is.finite(Q_stat) && Q_stat > 0) max(0, 100 * (Q_stat - (m_re - 1L)) / Q_stat) else 0

                wi_re <- 1 / (vi + tau2_dl)
                re_est <- sum(wi_re * yi) / sum(wi_re)
                re_err <- sqrt(1 / sum(wi_re))
                re_lo <- re_est - z_crit * re_err
                re_hi <- re_est + z_crit * re_err
                re_p <- 2 * stats::pnorm(-abs((re_est - tv) / re_err))

                wt$setRow(rowKey = "re_meta", values = list(
                    scheme   = .("Precision-weighted random-effects (DerSimonian-Laird)"),
                    units    = .fmt(.("{m} of {k} groups"), m = m_re, k = k),
                    estimate = re_est,
                    se       = re_err,
                    ci_low   = re_lo,
                    ci_high  = re_hi,
                    vsref    = re_est - tv,
                    pvalue   = re_p
                ))

                excl_note <- if (m_re < k) {
                    paste0(" ", .fmt(
                        .("({nexcl} group(s) with n < 2 or zero within-group dispersion were excluded from inverse-variance weighting.)"),
                        nexcl = k - m_re
                    ))
                } else {
                    ""
                }
                wt$setNote("method", paste0(
                    .fmt(
                        .("Compares how the {k} group {stats} benchmark against the Reference Value ({tv}) under three weighting assumptions ({cp}% intervals, two-sided p-values). Caseload weighting uses a cluster-robust CR1 sandwich SE (df = {dfk}); DerSimonian-Laird random-effects synthesis uses within-group SEs with estimated between-group heterogeneity tau = {tau}, Cochran's Q({dfq}) = {qval}, I-squared = {i2}%."),
                        k     = k,
                        stats = private$.summaryLabelPluralLower(),
                        tv    = base::format(tv),
                        cp    = base::format(100 * conf, trim = TRUE),
                        dfk   = k - 1L,
                        tau   = sprintf("%.3f", sqrt(tau2_dl)),
                        dfq   = m_re - 1L,
                        qval  = sprintf("%.2f", Q_stat),
                        i2    = sprintf("%.1f", i2_pct)
                    ),
                    excl_note
                ))
            } else {
                wt$setRow(rowKey = "re_meta", values = list(
                    scheme   = .("Precision-weighted random-effects (DerSimonian-Laird)"),
                    units    = .fmt(.("{m} of {k} groups"), m = m_re, k = k),
                    estimate = NULL,
                    se       = NULL,
                    ci_low   = NULL,
                    ci_high  = NULL,
                    vsref    = NULL,
                    pvalue   = NULL
                ))
                wt$setNote("method", .fmt(
                    .("Compares unweighted (1/k) and caseload-weighted (n_g / N, cluster-robust CR1 SE, df = {dfk}) syntheses against the Reference Value ({tv}). Precision-weighted random-effects synthesis requires at least 2 groups with n >= 2 and positive within-group dispersion."),
                    dfk = k - 1L,
                    tv  = base::format(tv)
                ))
            }
            invisible(NULL)
        },

        # ---- plotting ---------------------------------------------------------

        .captionText = function() {
            o <- self$options
            cap <- .fmt(.("Dashed red line: Reference Value = {tv}"), tv = base::format(o$testvalue))
            if (isTRUE(o$centralityplotting)) {
                cap <- paste0(cap, ". ", .("Solid blue line: center of the plotted points (not the reference)."))
            }
            cap
        },

        .buildDotPanel = function(tab, ost, title = NULL, caption = NULL, ggtheme = NULL) {
            o <- self$options
            xlab <- if (nzchar(o$xtitle)) o$xtitle else o$dep
            ylab <- if (nzchar(o$ytitle)) o$ytitle else o$group

            ord <- order(tab$value, seq_len(nrow(tab)))
            plot_df <- tab[ord, , drop = FALSE]
            plot_df$rank <- seq_len(nrow(plot_df))
            denom <- max(nrow(plot_df) - 1L, 1L)
            plot_df$percent_rank <- (rank(plot_df$value, ties.method = "min") - 1) / denom
            pct_labels <- round(100 * plot_df$percent_rank)

            sub_expr <- if (isTRUE(o$resultssubtitle) && !is.null(ost) && isTRUE(ost$ok)) {
                ost$expression
            } else {
                NULL
            }

            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$value, y = .data$rank)) +
                ggplot2::geom_errorbar(
                    ggplot2::aes(xmin = .data$ci_low, xmax = .data$ci_high),
                    orientation = "y", width = 0, na.rm = TRUE
                ) +
                ggplot2::geom_point(colour = "black", size = 3, shape = 16) +
                ggplot2::scale_y_continuous(
                    name   = ylab,
                    labels = plot_df$grp,
                    breaks = plot_df$rank,
                    sec.axis = ggplot2::dup_axis(
                        name   = .("percentile"),
                        breaks = plot_df$rank,
                        labels = pct_labels
                    )
                ) +
                ggplot2::geom_vline(
                    xintercept = o$testvalue,
                    linetype   = "dashed",
                    colour     = "#b22222",
                    linewidth  = 0.7
                )

            if (isTRUE(o$centralityplotting)) {
                cent <- private$.centralityStat(plot_df$value, o$centralitytype, private$.digitsVal())
                p <- p +
                    ggplot2::geom_vline(
                        xintercept = cent$val,
                        linetype   = "solid",
                        colour     = "#0000FF",
                        linewidth  = 0.8
                    ) +
                    ggplot2::scale_x_continuous(
                        sec.axis = ggplot2::sec_axis(
                            transform = ~.,
                            name      = NULL,
                            labels    = cent$expr,
                            breaks    = cent$val
                        )
                    )
            }

            th <- if (isTRUE(o$originaltheme)) ggstatsplot::theme_ggstatsplot() else ggtheme
            p + ggplot2::labs(
                x        = xlab,
                y        = ylab,
                title    = title,
                subtitle = sub_expr,
                caption  = caption
            ) + th
        },

        .plotFailure = function(msg) {
            print(ggplot2::ggplot() +
                  ggplot2::annotate("text", x = 0, y = 0, size = 4, colour = "#721c24",
                                    label = paste(strwrap(msg, width = 60), collapse = "\n")) +
                  ggplot2::theme_void())
            TRUE
        },

        .buildMainPlot = function(prep, ggtheme) {
            tab <- private$.groupTable(prep$data)
            if (is.null(tab) || nrow(tab) < 2L)
                stop(.("At least two groups with finite observations are required."))
            ost <- if (isTRUE(self$options$resultssubtitle)) {
                private$.mainOst(tab$value)
            } else {
                NULL
            }
            ttl <- if (nzchar(self$options$mytitle)) self$options$mytitle else NULL
            p <- private$.buildDotPanel(
                tab     = tab,
                ost     = ost,
                title   = ttl,
                caption = private$.captionText(),
                ggtheme = ggtheme
            )
            addSeedCaption(p, self, self$options$seed)
        },

        .buildSplitPlot = function(prep, ggtheme) {
            grv <- self$options$grvar
            sdf <- prep$split_data
            if (is.null(sdf) || nrow(sdf) == 0L)
                stop(.("No complete observations remain after filtering missing Split By values."))

            p_list <- private$.splitPanelsData(prep)
            if (is.null(p_list) || length(p_list) == 0L)
                stop(.("Split By variable has no observed levels."))

            cap <- private$.captionText()
            panels <- lapply(p_list, function(item) {
                slv <- item$level
                tab_s <- item$tab
                ks <- item$ks
                panel_title <- paste0(grv, ": ", slv)

                if (ks < 2L) {
                    msg <- .fmt(
                        .("Cannot draw panel '{slv}': only {ks} group has data (at least 2 groups required)."),
                        slv = slv,
                        ks  = ks
                    )
                    return(
                        ggplot2::ggplot() +
                            ggplot2::annotate(
                                "text", x = 0, y = 0, size = 3.6, colour = "#721c24",
                                label = paste(strwrap(msg, width = 45), collapse = "\n")
                            ) +
                            ggplot2::labs(title = panel_title, caption = cap) +
                            ggplot2::theme_void()
                    )
                }

                private$.buildDotPanel(
                    tab     = tab_s,
                    ost     = item$ost,
                    title   = panel_title,
                    caption = cap,
                    ggtheme = ggtheme
                )
            })

            ttl <- if (nzchar(self$options$mytitle)) self$options$mytitle else NULL
            p <- patchwork::wrap_plots(panels) + patchwork::plot_annotation(title = ttl)
            addSeedCaption(p, self, self$options$seed)
        },

        .plot = function(image, ggtheme, theme, ...) {
            if (!private$.validHere(image)) return(FALSE)
            prep <- private$.prepareData()
            if (is.null(prep)) return(FALSE)

            private$.checkpoint()
            err <- tryCatch({
                p <- private$.buildMainPlot(prep, ggtheme)
                print(p)
                NULL
            }, error = function(e) e)

            if (!is.null(err)) {
                return(private$.plotFailure(.fmt(
                    .("The chart could not be drawn: {msg}. Check that each group has at least one usable observation."),
                    msg = conditionMessage(err)
                )))
            }
            TRUE
        },

        .plot2 = function(image, ggtheme, theme, ...) {
            if (!private$.validHere(image) || is.null(self$options$grvar)) return(FALSE)
            prep <- private$.prepareData()
            if (is.null(prep)) return(FALSE)

            private$.checkpoint()
            err <- tryCatch({
                p <- private$.buildSplitPlot(prep, ggtheme)
                print(p)
                NULL
            }, error = function(e) e)

            if (!is.null(err)) {
                return(private$.plotFailure(.fmt(
                    .("The split chart could not be drawn: {msg}. Check that every level of the Split By variable has at least two groups with data."),
                    msg = conditionMessage(err)
                )))
            }
            TRUE
        }
    )
)
