# This file is a generated template, your changes will not be overwritten

# The analysis calls jmvcore's .() on nearly every line. A submodule that ships
# only analyses without this tag (JamoviTest with waterfall + ihcheterogeneity)
# otherwise gets no jmvcore import and every .() fails at run time.
#' @import jmvcore
#' @noRd
ihcheterogeneityClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "ihcheterogeneityClass",
    inherit = ihcheterogeneityBase,
    private = list(
        # Minimum sizes, the materiality margin and the CV floor, in one place.
        .CLINICAL_CONSTANTS = list(
            # Cases for the analysis, pairs for a correlation, complete cases for
            # the ICC and the variance components.
            MIN_CASES_ANALYSIS = 5,
            # Cases per spatial compartment - the same minimum in every compartment
            # table and in the spatial plot, so they always cover the same compartments.
            MIN_CASES_COMPARTMENT = 3,
            # Paired values for a row of the sampling-bias table.
            MIN_PAIRS_BIAS = 3,
            # CV = SD / mean is unstable near zero: a Ki67 case scored (0, 1, 0) has
            # CV 173% for a one-point disagreement. Cases whose mean is below
            # CV_FLOOR_FRACTION x the CV_FLOOR_QUANTILE of all absolute values are
            # left out of CV averages - a quantile, not the maximum, so one mistyped
            # value cannot move the floor and drop a whole low-expression stratum.
            CV_FLOOR_FRACTION = 0.02,
            CV_FLOOR_QUANTILE = 0.95
        ),

        # Per-run caches, reset at the top of .run().
        .repro_stats = NULL,
        .bias_stats = NULL,
        .icc_info = NULL,
        .cv_floor = 0,
        .vc_done = FALSE,
        .vc_shares = NULL,
        .ss_done = FALSE,
        .noticeList = list(),

        # library-audit 2026-09-16 OncoPath [INFO] REJECTED: no native notice element - type: Notice fails the
        #   .r.yaml schema, type: Notification builds no results object (guide section 13)
        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type, title = title, content = content)
            # Render immediately so a notice added before an early return survives.
            private$.renderNotices()
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                return()
            }
            # library-audit 2026-09-16 OncoPath [INFO] DONE: titles inherit the pane colour - fixed hues fell to
            #   2.7-2.9:1 on the dark theme; the translucent tint and the border carry the severity
            typeStyles <- list(
                ERROR = list(bgcolor = "rgba(220, 38, 38, 0.10)", border = "#fca5a5"),
                STRONG_WARNING = list(bgcolor = "rgba(234, 88, 12, 0.10)", border = "#fdba74"),
                WARNING = list(bgcolor = "rgba(202, 138, 4, 0.12)", border = "#fde047"),
                INFO = list(bgcolor = "rgba(37, 99, 235, 0.08)", border = "#93c5fd")
            )
            html <- "<div style='margin: 10px 0;'>"
            severity <- match(vapply(private$.noticeList, function(n) n$type, ""), names(typeStyles), nomatch = 4L)
            for (notice in private$.noticeList[order(severity)]) {
                style <- typeStyles[[notice$type]]
                if (is.null(style)) style <- typeStyles$INFO
                html <- paste0(html,
                    "<div style='background-color: ", style$bgcolor, "; border-left: 4px solid ", style$border,
                    "; padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                    "<strong style='color: inherit;'>", htmltools::htmlEscape(notice$title), "</strong><br>",
                    "<span style='color: inherit;'>", htmltools::htmlEscape(notice$content), "</span></div>")
            }
            self$results$notices$setContent(paste0(html, "</div>"))
        },

        # Sampling-strategy note, merged into the final interpretation (Html
        # $state is always NULL and cannot be read back).
        .strategy_notes = NULL,

        # One paired comparison: region (x) minus reference (y).
        #
        # Never errors. A zero-variance difference vector - every case offset by
        # the same amount, routine when scores are binned to whole percentages -
        # has no sampling variance, so it gets no p-value and no effect size, but
        # its offset is known exactly and still counts towards the bias verdict.
        # (It used to fall out of the veto entirely: an exact constant offset got a
        # more favourable verdict than the same offset with a little noise.)
        #
        # `level` (optional, same length as x; NA where unknown) is the level a
        # difference may change with (proportional bias): see .levelFit().
        .biasRow = function(x, y, level = NULL) {
            d <- x - y
            n <- length(d)
            md <- mean(d)
            sdd <- if (n >= 2) stats::sd(d) else NA_real_
            constant <- n >= 2 && sdd <= sqrt(.Machine$double.eps) * max(1, abs(md))
            p <- NA_real_
            ci <- c(NA_real_, NA_real_)
            g <- NA_real_
            if (constant) {
                ci <- c(md, md)
            } else if (n >= 2) {
                tt <- tryCatch(stats::t.test(d), error = function(e) NULL)
                if (!is.null(tt)) {
                    p <- tt$p.value
                    ci <- as.numeric(tt$conf.int)
                }
                # Hedges' g for paired data: d_z times J = 1 - 3 / (4 df - 1),
                # df = n - 1, applied at EVERY n (it used to switch off at n = 50,
                # a ~1.6% jump in the reported value).
                g <- md / sdd * (1 - 3 / (4 * n - 5))
            }
            loa <- if (is.na(sdd)) c(NA_real_, NA_real_) else md + c(-1, 1) * 1.96 * sdd
            # 95% CI of each limit, approximate SE of Bland & Altman (1999):
            # SD * sqrt(1/n + 1.96^2 / (2 (n - 1))). Rows: lower limit, upper limit.
            loa_ci <- if (is.na(sdd)) matrix(NA_real_, 2, 2) else {
                half <- stats::qt(0.975, n - 1) * sdd * sqrt(1 / n + 1.96^2 / (2 * (n - 1)))
                rbind(loa[1] + c(-1, 1) * half, loa[2] + c(-1, 1) * half)
            }
            ref_mean <- mean(y)
            rel <- if (abs(ref_mean) < 1e-6) NA_real_ else md / abs(ref_mean) * 100
            rel_ci <- if (is.na(rel)) c(NA_real_, NA_real_) else ci / abs(ref_mean) * 100
            prop <- if (!constant && !is.null(level)) {
                ok <- is.finite(level)
                private$.levelFit(d[ok], level[ok])
            }
            list(n = n, mean_diff = md, sd = sdd, ci = ci, loa = loa, loa_ci = loa_ci, p = p, g = g,
                 constant = constant, rel = rel, rel_ci = rel_ci, ref_mean = ref_mean,
                 prop = prop)
        },

        # Proportional bias: does the difference d change with the level L?
        # Bland & Altman (1999) regress d on a level. With one reading per method
        # every available level shares an error or a deviation with one of the
        # readings, and builds in a slope of its own (regression to the mean): the
        # reference, a reference SD s, -s^2 / var(reference); the other regions, a
        # site effect the regions share. Such a slope cannot be told from a real one
        # (Dunn 2004): as the other regions' level it made unbiased regions MATERIAL
        # in up to 32% of simulated studies (release review 2026-09-19). So a slope
        # only ever stops a difference from being ruled out (see .analyzeSamplingBias).
        # Standard errors are HC3 (heteroscedasticity-consistent, MacKinnon & White
        # 1985): IHC error grows with the level, and plain OLS errors flagged a slope
        # in 33-48% of simulated studies with none (review 2026-09-19).
        # NULL when the slope cannot be estimated: fewer than 5 cases or 5 distinct
        # levels (an ordinal score such as HER2 0-3+ has no meaningful slope),
        # counted to 10 significant digits - averages that differ only in rounding
        # residue passed as 5 levels, left a leverage of 1 and aborted the analysis.
        .levelFit = function(d, L) {
            n <- length(d)
            if (n < private$.CLINICAL_CONSTANTS$MIN_CASES_ANALYSIS || length(unique(signif(L, 10))) < 5) return(NULL)
            # The cases with a known level can share one exact offset although the
            # row does not: the residue of sum(Lc) then passes for a slope.
            if (private$.isConstant(d)) return(NULL)
            lbar <- mean(L)
            Lc <- L - lbar
            sxx <- sum(Lc^2)
            a <- mean(d)
            b <- sum(Lc * d) / sxx
            e <- d - a - b * Lc
            h <- 1 / n + Lc^2 / sxx
            if (max(h) >= 1 - 1e-8) return(NULL)
            e3 <- e / (1 - h)
            var_b <- sum(Lc^2 * e3^2) / sxx^2
            var_a <- sum(e3^2) / n^2
            cov_ab <- sum(Lc * e3^2) / (n * sxx)
            se_b <- sqrt(var_b)
            if (!is.finite(se_b)) return(NULL)
            p <- if (se_b > 0) 2 * stats::pt(-abs(b / se_b), n - 2) else if (b != 0) 0 else 1
            # Judged at the 5th and 95th percentiles of the level: a straight line
            # with a CI band that widens away from the mean is inside a margin on
            # the whole interval once it is inside at both ends.
            ends <- stats::quantile(L, c(0.05, 0.95), names = FALSE)
            dl <- ends - lbar
            list(n = n, slope = b, se = se_b, p = p, df = n - 2, ends = ends,
                 fit = a + b * dl, se_fit = sqrt(pmax(var_a + dl^2 * var_b + 2 * dl * cov_ab, 0)))
        },

        # 2 x 2 matrix of CIs of the fitted difference at the two ends (rows: low
        # end, high end), in raw units.
        .endCI = function(prop, level) {
            half <- stats::qt((1 + level) / 2, prop$df) * prop$se_fit
            cbind(prop$fit - half, prop$fit + half)
        },

        # jmvcore passes a table note through the translator again, which reads a
        # " [..]" as a translation context and drops everything from it: a region
        # called "Ki67 [core 3]" cut its note short. A no-break space before the
        # bracket keeps the text identical on screen.
        .noteSafe = function(x) gsub(" [", "\u00a0[", x, fixed = TRUE),

        # Fisher-z 95% CI for a Spearman correlation with the variance of Bonett &
        # Wright (2000), (1 + r^2 / 2) / (n - 3). The 1.06 / (n - 3) of Fieller,
        # Hartley & Pearson covered only 85-92% at r = 0.90-0.95, the range of
        # agreement studies. No interval for |r| of 1 (up to rounding).
        .spearmanCI = function(r, n) {
            if (is.na(r) || n <= 3 || abs(r) >= 1 - 1e-8) return(c(NA_real_, NA_real_))
            tanh(atanh(r) + c(-1, 1) * stats::qnorm(0.975) * sqrt((1 + r^2 / 2) / (n - 3)))
        },

        # CI of a bias row at any confidence level, as a percentage of the
        # comparison mean. An exact constant offset has a zero-width interval.
        .relCI = function(r, level) {
            if (is.na(r$rel)) return(c(NA_real_, NA_real_))
            if (isTRUE(r$constant)) return(c(r$rel, r$rel))
            if (is.na(r$sd) || r$n < 2) return(c(NA_real_, NA_real_))
            half <- stats::qt((1 + level) / 2, r$n - 1) * r$sd / sqrt(r$n)
            (r$mean_diff + c(-1, 1) * half) / abs(r$ref_mean) * 100
        },

        # Relative error of ONE regional measurement, for the regions-per-case
        # advice. The per-case CV is the spread of the reference and all regions
        # together - not the error of a single region (it read 16% where a single
        # region deviated from the reference by 29%). Reference design: root-mean-
        # square relative difference region - reference; inter-regional: root-mean-
        # square within-case CV (the sample variance is unbiased, mean(s / m) is not).
        .singleMeasurementCV = function(whole_section, biopsy_data, has_reference) {
            X <- as.matrix(biopsy_data)
            storage.mode(X) <- "double"
            floor_value <- max(private$.cv_floor, 1e-6)
            if (has_reference) {
                ok <- !is.na(whole_section) & abs(whole_section) >= floor_value
                if (!any(ok)) return(NA_real_)
                rel <- (X[ok, , drop = FALSE] - whole_section[ok]) / abs(whole_section[ok])
                rel <- rel[!is.na(rel)]
                if (length(rel) < 2) return(NA_real_)
                sqrt(mean(rel^2)) * 100
            } else {
                cv2 <- apply(X, 1, function(v) {
                    v <- v[!is.na(v)]
                    if (length(v) < 2 || abs(mean(v)) < floor_value) NA_real_ else (stats::sd(v) / abs(mean(v)))^2
                })
                cv2 <- cv2[!is.na(cv2)]
                if (length(cv2) < 2) return(NA_real_)
                sqrt(mean(cv2)) * 100
            }
        },

        # Where a material difference sits: region names (variable names only, so
        # nothing translated is spliced into a sentence), whether the mean of all
        # regions is affected, and whether regional offsets point both ways.
        .materialInfo = function(rows) {
            material <- Filter(function(r) isTRUE(r$material), rows)
            regions <- Filter(function(r) !r$pooled, material)
            signs <- sign(vapply(regions, function(r) r$rel, numeric(1)))
            list(any = length(material) > 0,
                 rows = material,
                 regions = if (length(regions) > 0)
                     paste(sprintf("'%s'", vapply(regions, function(r) r$name, "")), collapse = ", ") else "",
                 pooled = any(vapply(material, function(r) isTRUE(r$pooled), logical(1))),
                 opposite = length(unique(signs[signs != 0])) > 1)
        },

        # What the level check found, for texts that report agreement. NULL when a
        # difference that changes with the level was judged at both ends (the
        # 90% CI sentence then covers it).
        .levelCheckSentence = function(rows) {
            rows <- Filter(function(r) !is.na(r$rel), rows)
            # A difference that is the same in every case cannot change with the level.
            checked <- Filter(function(r) !is.null(r$prop) || isTRUE(r$constant), rows)
            if (length(checked) == 0)
                return(.("Whether a difference changes with the level was not assessed (too few cases or distinct values)."))
            # Comparisons left out are named, so "not detected" is never read as
            # covering them.
            # (The mean of all regions uses the regions' level, so it is unchecked
            # only when they are.)
            unchecked <- Filter(function(r) is.null(r$prop) && !isTRUE(r$constant) && !r$pooled, rows)
            skipped <- if (length(unchecked) > 0)
                sprintf(.("Not checked for a difference that changes with the level (too few cases or distinct values): %s."),
                        paste(sprintf("'%s'", vapply(unchecked, function(r) r$name, "")), collapse = ", "))
            if (all(vapply(checked, function(r) isTRUE(r$constant), logical(1))) ||
                any(vapply(checked, function(r) isTRUE(r$prop_shown), logical(1)))) return(skipped)
            c(.("No difference that changes with the level was detected; this does not show that there is none."), skipped)
        },

        # How the level check works in this design, for the table note and the
        # methodology panel (present tense; the report has its own past tense).
        .levelMethodText = function(has_reference) {
            if (has_reference)
                .("Slope of the difference on the reference value, with heteroscedasticity-robust (HC3) standard errors; p is unadjusted. Measurement error in the reference alone makes the difference appear to fall as the reference rises (regression to the mean), so a slope can stop a difference from being ruled out but never shows it to be material. When p is below 0.05 divided by the number of comparisons, the difference is also judged at the 5th and 95th percentiles of the reference (Bland & Altman 1999).")
            else
                .("Slope of the difference on the case mean of all regions, with heteroscedasticity-robust (HC3) standard errors; p is unadjusted. Unequal measurement error between regions can produce a slope, so here a slope can stop a difference from being ruled out but never shows it to be material. When p is below 0.05 divided by the number of comparisons, the difference is also judged at the 5th and 95th percentiles of that level (Bland & Altman 1999).")
        },

        .materialWhereSentence = function(info) {
            if (nzchar(info$regions) && info$pooled)
                sprintf(.("It affects region(s) %s and the mean of all regions."), info$regions)
            else if (nzchar(info$regions))
                sprintf(.("It affects region(s) %s."), info$regions)
            else
                .("It affects the mean of all regions, although no single region is materially offset on its own.")
        },

        # Names of regions not assessed (correlation or bias), for the texts.
        .unassessedText = function(metrics) {
            if (length(metrics$unassessed) == 0) return(NULL)
            sprintf(.("Not assessed (too few paired values or the same value in every case): %s."),
                    paste(sprintf("'%s'", metrics$unassessed), collapse = ", "))
        },

        # The region whose limits of agreement reach furthest from zero.
        .widestLoA = function(rows) {
            regions <- Filter(function(r) !r$pooled && all(is.finite(r$loa)), rows)
            if (length(regions) == 0) return(NULL)
            regions[[which.max(vapply(regions, function(r) max(abs(r$loa)), numeric(1)))]]
        },

        # Names what the region was compared with: in the inter-regional design
        # the sentence used to say "differ from the reference", in copy-ready text
        # too, where no reference exists.
        .loaSentence = function(rows) {
            w <- private$.widestLoA(rows)
            if (is.null(w)) return(NULL)
            comparator <- if (is.null(w$comparator)) "reference" else w$comparator
            lo <- sprintf("%.2f", w$loa[1])
            hi <- sprintf("%.2f", w$loa[2])
            if (identical(comparator, "reference"))
                sprintf(.("The widest 95%% limits of agreement are %s to %s (region '%s'): an individual case can differ from the reference by about this much."), lo, hi, w$name)
            else if (identical(comparator, "others"))
                sprintf(.("The widest 95%% limits of agreement are %s to %s (region '%s'): in an individual case this region can differ from the mean of the other regions by about this much."), lo, hi, w$name)
            else
                sprintf(.("The widest 95%% limits of agreement are %s to %s (region '%s'): in an individual case this region can differ from region '%s' by about this much."), lo, hi, w$name, comparator)
        },

        # Thresholds in text: 15 / 2 is 7.5, not "8" (sprintf "%.0f" rounded the
        # band edges the analysis actually uses).
        .fmtNum = function(x) as.character(round(x, 2)),

        # The fitted difference at the two ends of the level (signed), its 90% CIs
        # (rows: low end, high end), the levels and the margin, in the marker's
        # units with one precision per row: two significant digits of the margin
        # (a 3.28-point margin gives 1 decimal, a 0.65-point one 2).
        .endValues = function(r) {
            digits <- min(6, max(0, 1 - floor(log10(r$margin_abs))))
            f <- function(x, signed = FALSE) sprintf(paste0(if (signed) "%+." else "%.", digits, "f"), x)
            ci <- if (is.null(r$end_ci90)) matrix(NA_real_, 2, 2) else r$end_ci90
            list(fit = f(r$prop$fit, signed = TRUE), at = f(r$prop$ends), margin = f(r$margin_abs),
                 ci = matrix(f(ci), 2))
        },

        # Any regional slot (1-4) or the Additional list starts the analysis: a
        # variable in slot 2 with slot 1 empty used to be ignored without a word.
        .hasRegionalMeasurement = function() {
            slots <- list(self$options$biopsy1, self$options$biopsy2,
                          self$options$biopsy3, self$options$biopsy4)
            any(!vapply(slots, is.null, logical(1))) || length(self$options$biopsies) > 0
        },

        # Same value in every non-missing position (up to rounding).
        .isConstant = function(v) {
            v <- v[!is.na(v)]
            length(v) >= 2 && stats::sd(v) <= sqrt(.Machine$double.eps) * max(1, abs(mean(v)))
        },

        # Per-case coefficient of variation - ONE definition, used by every table,
        # sentence and plot. When a reference exists it is included: the question
        # is whether a region reproduces the reference, so excluding it would make
        # a systematic under-read invisible. `floor` is required: the plot
        # renderers pass the value saved in the image state (private fields are
        # empty when a saved .omv is redrawn).
        .perCaseCV = function(whole_section, biopsy_data, has_reference, floor, with_status = FALSE) {
            m <- as.matrix(biopsy_data)
            storage.mode(m) <- "double"
            if (has_reference && !is.null(whole_section)) m <- cbind(as.numeric(whole_section), m)
            if (nrow(m) == 0) return(numeric(0))
            out <- vapply(seq_len(nrow(m)),
                          function(i) private$.calculateRobustCV(m[i, ], floor), numeric(1))
            if (with_status) {
                n_values <- rowSums(!is.na(m))
                identical_values <- vapply(seq_len(nrow(m)), function(i) {
                    v <- m[i, !is.na(m[i, ])]
                    length(v) >= 2 && all(v == v[1])
                }, logical(1))
                attr(out, "status") <- ifelse(n_values < 2, "short",
                                        ifelse(!is.na(out), "ok",
                                        ifelse(identical_values, "floor_identical", "floor")))
            }
            out
        },

        # Within-case CV over cases: the root mean square of the per-case CVs
        # (Bland). The plain average of SD / mean is biased low by the c4 factor
        # of each case's SD - a true 15% read 11.8% with 2 values per case, about
        # 6% low with 5 - so a design with FEWER regions passed the threshold more
        # easily. The sample variance is unbiased, so the root mean square is free of that bias.
        .rmsCV = function(cv) {
            if (any(!is.na(cv))) sqrt(mean(cv^2, na.rm = TRUE)) else NA_real_
        },

        .calculateRobustCV = function(values, floor = 0) {
            values <- values[!is.na(values)]
            if (length(values) < 2) return(NA_real_)
            mean_val <- mean(values)
            # Near-zero cases are left out WHATEVER their spread. Keeping only the
            # ones that happen to agree exactly (an all-zero ER-negative case) and
            # dropping those with a 0.1-point disagreement selected on the outcome
            # and pulled the mean CV down (and flipped the verdict).
            if (abs(mean_val) < max(floor, 1e-6)) return(NA_real_)
            sd_val <- stats::sd(values)
            if (sd_val <= sqrt(.Machine$double.eps) * abs(mean_val)) return(0)
            # A finite CV is always reported. The former ">500% = NA" cap dropped
            # the MOST heterogeneous cases, biasing the mean in exactly the
            # direction this analysis exists to detect.
            cv <- sd_val / abs(mean_val) * 100
            if (!is.finite(cv)) NA_real_ else cv
        },

        # Mean squares of a complete n x k matrix (cases x methods), in closed
        # form. The variance components and both ICCs used to come from two
        # dense aov() fits whose case factor had n levels - an nk x (n+k) model
        # matrix and O(n^3 k) work: 10 s at n = 1000, 73 s at n = 2000.
        .meanSquares = function(Y) {
            n <- nrow(Y)
            k <- ncol(Y)
            grand <- mean(Y)
            ss_rows <- k * sum((rowMeans(Y) - grand)^2)
            ss_cols <- n * sum((colMeans(Y) - grand)^2)
            ss_total <- sum((Y - grand)^2)
            ss_error <- ss_total - ss_rows - ss_cols
            # Rounding residue (an exact constant offset leaves ~1e-13, which was
            # reported as "within-case variance") is zero.
            if (ss_error <= 1e-12 * ss_total) ss_error <- 0
            list(n = n, k = k,
                 msr = ss_rows / (n - 1),
                 msc = ss_cols / (k - 1),
                 mse = ss_error / ((n - 1) * (k - 1)),
                 sst = ss_total)
        },

        # ICC(A,1) (absolute agreement, two-way random) and ICC(C,1)
        # (consistency) with the F-based 95% CIs of McGraw & Wong (1996) - the
        # same formulas psych::ICC() uses for its ICC2 and ICC3 rows. Returns NULL
        # when the matrix has no variability (the ICC is 0/0).
        .iccFromMatrix = function(Y, alpha = 0.05) {
            n <- nrow(Y)
            k <- ncol(Y)
            if (n < 2 || k < 2) return(NULL)
            ms <- private$.meanSquares(Y)
            if (ms$sst <= 1e-12 * max(1, sum(Y^2))) return(NULL)
            msr <- ms$msr
            msc <- ms$msc
            mse <- ms$mse
            den_a <- msr + (k - 1) * mse + k * (msc - mse) / n
            den_c <- msr + (k - 1) * mse
            if (!isTRUE(den_a > 0) || !isTRUE(den_c > 0)) return(NULL)
            icc_a <- (msr - mse) / den_a
            icc_c <- (msr - mse) / den_c

            q <- 1 - alpha / 2
            ci_a <- c(NA_real_, NA_real_)
            ci_c <- c(NA_real_, NA_real_)
            v <- NA_real_
            if (mse > 0) {
                f <- msr / mse
                df1 <- n - 1
                df2 <- (n - 1) * (k - 1)
                fl <- f / stats::qf(q, df1, df2)
                fu <- f * stats::qf(q, df2, df1)
                ci_c <- c((fl - 1) / (fl + k - 1), (fu - 1) / (fu + k - 1))
                fj <- msc / mse
                vn <- (k - 1) * (n - 1) * (k * icc_a * fj + n * (1 + (k - 1) * icc_a) - k * icc_a)^2
                vd <- (n - 1) * k^2 * icc_a^2 * fj^2 + (n * (1 + (k - 1) * icc_a) - k * icc_a)^2
                v <- vn / vd
            } else if (msr > 0) {
                # No residual variance (an exact constant offset): the F ratio is
                # infinite, so the consistency interval is 1 to 1, and the
                # Satterthwaite df of the agreement interval tends to k - 1, which
                # is finite - psych::ICC reports the same interval. It used to be
                # left blank beside "Judge the band from the 95% CI".
                ci_c <- c(1, 1)
                v <- k - 1
            }
            if (is.finite(v) && v > 0) {
                f3u <- stats::qf(q, n - 1, v)
                f3l <- stats::qf(q, v, n - 1)
                ci_a <- c(n * (msr - f3u * mse) / (f3u * (k * msc + (k * n - k - n) * mse) + n * msr),
                          n * (f3l * msr - mse) / (k * msc + (k * n - k - n) * mse + n * f3l * msr))
            }
            list(n = n, k = k, ms = ms,
                 agreement = list(value = icc_a, lower = ci_a[1], upper = ci_a[2]),
                 consistency = list(value = icc_c, lower = ci_c[1], upper = ci_c[2]))
        },

        # The complete-case matrix shared by the ICC and the variance components:
        # the reference (if any) plus every region measured in at least min_n
        # cases. A sparse optional region used to cut the complete-case set below
        # 3, which silently replaced the ICC with a mean correlation; such regions
        # are left out and named, with their counts. Nothing else is dropped: if
        # the remaining regions share too few complete cases, the ICC is reported
        # as not estimable rather than computed on a subset of regions chosen by
        # value counts (which once dropped a biased region with 6 values and
        # reported the remaining pair as "Excellent reliability").
        .iccMatrix = function(whole_section, biopsy_data, min_n) {
            X <- as.matrix(biopsy_data)
            storage.mode(X) <- "double"
            n_values <- colSums(!is.na(X))
            sparse <- n_values < min_n
            dropped <- if (any(sparse)) sprintf("%s (n = %d)", colnames(X)[sparse], n_values[sparse]) else character(0)
            X <- X[, !sparse, drop = FALSE]
            Y <- if (!is.null(whole_section)) cbind(Reference = as.numeric(whole_section), X) else X
            cc <- stats::complete.cases(Y)
            list(Y = Y[cc, , drop = FALSE], dropped = dropped, n_incomplete = sum(!cc))
        },

        .calculateICC = function(whole_section, biopsy_data,
                                 min_n = private$.CLINICAL_CONSTANTS$MIN_CASES_ANALYSIS) {
            mat <- private$.iccMatrix(whole_section, biopsy_data, min_n)
            Y <- mat$Y
            result <- NULL
            reason <- NULL
            if (ncol(Y) < 2) {
                reason <- .("An ICC needs at least two measurements per case, each available in enough cases.")
            } else if (nrow(Y) < min_n) {
                reason <- sprintf(.("Only %d cases have every measurement; the ICC needs at least %d. Regions measured in different subsets of cases reduce this number."), nrow(Y), min_n)
            } else {
                result <- private$.iccFromMatrix(Y)
                if (is.null(result))
                    reason <- .("The measurements show no variability, so the ICC is not defined.")
            }
            list(result = result, reason = reason, dropped = mat$dropped,
                 n = nrow(Y), k = ncol(Y), Y = Y, n_incomplete = mat$n_incomplete)
        },

        # Koo & Li (2016) reliability bands, used by the table, the summary and
        # the glossary alike (the table used to stop at "Good" while the summary
        # said "Excellent" for the same value).
        .iccBand = function(icc) {
            if (icc > 0.90) "excellent" else if (icc >= 0.75) "good" else if (icc >= 0.50) "moderate" else "poor"
        },

        .init = function() {
            # Welcome screen: whole translatable sentences, no literal HTML text.
            self$results$welcome$setContent(paste0(
                "<div class='jmv-welcome' style='padding: 20px; background-color: rgba(138, 155, 172, 0.06); border-left: 4px solid #007bff; color: inherit;'>",
                "<h3 style='margin-top: 0;'>", .("IHC Heterogeneity Analysis"), "</h3>",
                "<p><strong>", .("Get started:"), "</strong></p><ol>",
                "<li>", .("Select at least one regional measurement (Regional Measurement 1 to 4, or Additional Regional Measurements)."), "</li>",
                "<li>", .("Optionally add a reference measurement (whole slide, hotspot or overall score); without one, select at least two regional measurements."), "</li>",
                "<li>", .("Optionally add a Spatial Region ID: the compartment each case was sampled from, with one row per case."), "</li>",
                "</ol><p style='margin-bottom: 0;'><em>", .("Set the CV and correlation thresholds and the systematic difference margin before looking at the results."), "</em></p></div>"))

            # Hide the welcome screen whenever any regional measurement is
            # supplied. The plural 'biopsies' list cannot be referenced from the
            # r.yaml 'visible' expression, so drive visibility here instead.
            has_regional <- private$.hasRegionalMeasurement()
            self$results$welcome$setVisible(!has_regional)

            # Fixed row structure for the variance component table: three
            # components plus a total, every run. The method label depends on
            # whether a reference measurement was supplied, which is an option,
            # not a result - so the whole structure belongs here. The case row is
            # the two-way case component in both designs (it is not the variance
            # of the case-wise regional means, which also carries error / k).
            has_reference <- !is.null(self$options$wholesection)
            if (!has_reference)
                self$results$samplingbiastable$getColumn("mean_diff")$setTitle(
                    .("Mean Difference (Region - Comparator)"))
            variance_components <- c(
                .("Between-Case Variance"),
                .("Within-Case Variance (Sampling)"),
                if (has_reference) .("Method Variance") else .("Regional Method Variance"),
                .("Total Variance")
            )
            for (r in 1:4)
                self$results$variancetable$addRow(
                    rowKey = r, values = list(component = variance_components[r]))

            # Set conditional visibility based on options and analysis type.
            # Until a regional measurement is selected only the welcome panel
            # shows (empty tables and plot frames used to sit beneath it).
            analysis_type <- self$options$analysis_type
            self$results$reproducibilitytable$setVisible(has_regional)
            self$results$samplingbiastable$setVisible(has_regional)
            show_plots <- has_regional && (self$options$show_variability_plots ||
                         analysis_type == "variability" ||
                         analysis_type == "comprehensive")

            self$results$biopsyplot$setVisible(show_plots)
            self$results$variabilityplot$setVisible(show_plots)
            self$results$spatialplot$setVisible(
                show_plots && !is.null(self$options$spatial_id)
            )

            # Sample-size planning (ICC precision) works in both designs.
            show_sample_size <- has_regional && (self$options$sample_size_planning || analysis_type == "comprehensive")
            show_variance <- has_regional && (self$options$variance_components ||
                           analysis_type == "variability" ||
                           analysis_type == "comprehensive")
            self$results$samplesizetable$setVisible(show_sample_size)
            self$results$variancetable$setVisible(show_variance)
            self$results$spatialanalysistable$setVisible(has_regional && !is.null(self$options$spatial_id))
            self$results$compartmentComparison$setVisible(
                has_regional && self$options$compareCompartments && !is.null(self$options$spatial_id))
            self$results$compartmentTests$setVisible(
                has_regional && self$options$compartmentTests && !is.null(self$options$spatial_id))
            # Filled in .run(): empty until a regional measurement is selected.
            self$results$report_sentences$setVisible(has_regional && self$options$showReportSentences)
            self$results$assumptions$setVisible(has_regional && self$options$showAssumptions)

            self$results$summary$setVisible(has_regional && self$options$showSummary)
            self$results$glossary$setVisible(self$options$showGlossary)
            if (self$options$showGlossary) {
                private$.populateGlossary()
            }
        },

        .run = function() {
            private$.noticeList <- list()
            on.exit(private$.renderNotices(), add = TRUE)
            # Unset Variable options cannot be tested reliably with a leading `!`
            # in r.yaml (the expression is silently treated as always visible).
            # Keep the welcome panel synchronized here on every option change.
            has_regional_measurement <- private$.hasRegionalMeasurement()
            self$results$welcome$setVisible(!has_regional_measurement)

            # Reset per-run caches so a run that takes another path never shows
            # values from the previous run.
            private$.repro_stats <- NULL
            private$.bias_stats <- NULL
            private$.icc_info <- NULL
            private$.vc_done <- FALSE
            private$.vc_shares <- NULL
            private$.ss_done <- FALSE

            if (!has_regional_measurement) {
                return()
            }

            regional_count <- sum(!sapply(list(self$options$biopsy1, self$options$biopsy2,
                                             self$options$biopsy3, self$options$biopsy4), is.null))
            if (!is.null(self$options$biopsies)) {
                regional_count <- regional_count + length(self$options$biopsies)
            }

            # An inter-regional study needs >= 2 regions (there is nothing to
            # compare otherwise). A reference-based study is meaningful with a
            # single region: one biopsy against the whole section is the classic
            # agreement/bias design.
            if (is.null(self$options$wholesection) && regional_count < 2) {
                jmvcore::reject(.("At least 2 regional measurements are required for inter-regional heterogeneity analysis. With a reference (whole section) measurement selected, a single regional measurement is sufficient."))
            }

            data <- self$data
            if (nrow(data) == 0) {
                jmvcore::reject(.("The dataset contains no rows. Add data (or clear the active filter) and re-run."))
            }

            whole_section <- if (!is.null(self$options$wholesection)) {
                private$.toNumeric(data[[self$options$wholesection]])
            } else {
                NULL
            }
            biopsy_data <- private$.extractRegionalData(data)
            has_reference <- !is.null(whole_section)
            # A variable selected twice is analysed once, which can leave an
            # inter-regional design with a single region.
            if (!has_reference && ncol(biopsy_data) < 2) {
                jmvcore::reject(.("At least 2 regional measurements are required for inter-regional heterogeneity analysis. With a reference (whole section) measurement selected, a single regional measurement is sufficient."))
            }
            min_cases <- private$.CLINICAL_CONSTANTS$MIN_CASES_ANALYSIS

            # The unit of analysis is a case that can contribute to the design: a
            # reference value and at least one region (reference-based), or at
            # least two regions (inter-regional). The gate, the small-sample
            # notice, the Study Design line and every table use this one count.
            # The gate used to count ROWS - including rows whose reference was
            # missing - so a reference scored in 3 of 30 cases produced a full
            # verdict, with no small-sample notice, from 3 pairs.
            n_regions_present <- rowSums(!is.na(biopsy_data))
            usable <- if (has_reference) {
                !is.na(whole_section) & n_regions_present >= 1
            } else {
                n_regions_present >= 2
            }
            n_usable <- sum(usable)

            if (has_reference && n_usable == 0 && any(n_regions_present >= 1)) {
                # "{}" + argument: reject() formats its message, which turned a name
                # such as "Ki67 {core}" into "Ki67 ...".
                jmvcore::reject("{}", msg = sprintf(
                    .("The reference variable '%s' has no values in the cases that have regional measurements. Remove it to run an inter-regional analysis, or check the data."),
                    self$options$wholesection))
            }
            if (n_usable < min_cases) {
                jmvcore::reject(sprintf(
                    if (has_reference)
                        .("Insufficient data for reference-based heterogeneity analysis. At least 5 complete cases (a reference value and at least one regional measurement) are required; %d case(s) qualify.")
                    else
                        .("Insufficient data for inter-regional heterogeneity analysis. At least 5 complete cases (at least 2 regional measurements each) are required; %d case(s) qualify."),
                    n_usable))
            }

            spatial_regions <- NULL
            if (!is.null(self$options$spatial_id)) {
                spatial_id_var <- self$options$spatial_id
                if (spatial_id_var %in% names(data)) {
                    spatial_regions <- droplevels(as.factor(data[[spatial_id_var]][usable]))
                } else {
                    # reject() content renders as plain text (jamovi escapes it), so no htmlEscape needed.
                    jmvcore::reject("{}", msg = sprintf(.("Spatial ID variable '%s' not found in data."), self$options$spatial_id))
                }
            }

            n_rows <- nrow(data)
            if (has_reference) whole_section <- whole_section[usable]
            biopsy_data <- biopsy_data[usable, , drop = FALSE]
            if (n_rows > n_usable) {
                private$.addNotice("INFO", .("Cases not analysed"), sprintf(
                    if (has_reference)
                        .("%d of %d rows were not analysed because they lack a reference value or any regional measurement.")
                    else
                        .("%d of %d rows were not analysed because they have fewer than 2 regional measurements."),
                    n_rows - n_usable, n_rows))
            }

            all_values <- abs(c(whole_section, as.matrix(biopsy_data)))
            all_values <- all_values[!is.na(all_values)]
            private$.cv_floor <- private$.CLINICAL_CONSTANTS$CV_FLOOR_FRACTION *
                as.numeric(stats::quantile(all_values, private$.CLINICAL_CONSTANTS$CV_FLOOR_QUANTILE, names = FALSE))

            # Data-quality checks go to the notices panel with every other warning
            # (they used to sit in a separate box at the top of the interpretation).
            for (w in private$.detectMisuse(whole_section, biopsy_data))
                private$.addNotice("WARNING", .("Data quality"), w)

            study_design <- if (has_reference) "reference_based" else "inter_regional"

            private$.performHeterogeneityAnalysis(
                whole_section = whole_section,
                biopsy_data = biopsy_data,
                spatial_regions = spatial_regions,
                study_design = study_design
            )

            # No case has a CV (e.g. every value is 0): the variability plot would
            # stay visible and empty.
            if (!any(!is.na(private$.repro_stats$case_cv))) {
                self$results$variabilityplot$setVisible(FALSE)
                if (self$options$show_variability_plots || self$options$analysis_type %in% c("variability", "comprehensive"))
                    private$.addNotice("INFO", .("Variability plot not drawn"),
                        .("No case has a computable coefficient of variation (every case mean is zero or near zero), so the variability plot is not shown."))
            }

            # The spatial plot covers the compartments the tables cover; with
            # fewer than two of them there is nothing to draw.
            if (!is.null(spatial_regions)) {
                groups <- private$.compartments(spatial_regions)
                if (length(groups$keep) < 2) self$results$spatialplot$setVisible(FALSE)
            }

            private$.generateHeterogeneityPlots(whole_section, biopsy_data, spatial_regions, study_design)
            private$.generateHeterogeneityInterpretation(whole_section, biopsy_data, study_design)
        },

        # jamovi hands a nominal variable with an integer/decimal data type
        # back as a factor carrying a 'values' attribute. permitted: numeric
        # admits it, and without this unwrap the arithmetic aborted with a raw
        # "non-numeric argument to binary operator".
        .toNumeric = function(x) {
            x <- jmvcore::toNumeric(x)
            if (is.factor(x)) x <- suppressWarnings(as.numeric(as.character(x)))
            x
        },

        # Regional measurement columns, all rows, named by their variables so
        # every table and plot can say WHICH region (they used to read "Region 2"
        # for the variable in slot 3 once empty slots were compacted).
        .extractRegionalData = function(data) {
            individual_biopsies <- list(self$options$biopsy1, self$options$biopsy2,
                                       self$options$biopsy3, self$options$biopsy4)
            individual_biopsies <- individual_biopsies[!sapply(individual_biopsies, is.null)]
            additional_cols <- if (!is.null(self$options$biopsies)) self$options$biopsies else c()
            biopsy_columns <- c(unlist(individual_biopsies), additional_cols)

            # A variable selected twice agrees perfectly with itself and would
            # inflate every agreement statistic (and broke the biopsy plot).
            # The reference itself among the regions would compare with itself.
            reference <- self$options$wholesection
            if (!is.null(reference) && reference %in% biopsy_columns) {
                private$.addNotice("WARNING", .("Variable selected twice"), sprintf(
                    .("%s is the reference measurement and was also selected as a regional measurement; it is analysed as the reference only."),
                    reference))
                biopsy_columns <- setdiff(biopsy_columns, reference)
                if (length(biopsy_columns) == 0)
                    jmvcore::reject(.("Select at least one regional measurement other than the reference."))
            }
            repeated <- unique(biopsy_columns[duplicated(biopsy_columns)])
            if (length(repeated) > 0) {
                private$.addNotice("WARNING", .("Variable selected twice"), sprintf(
                    .("%s was selected more than once as a regional measurement and is analysed once."),
                    paste(repeated, collapse = ", ")))
                biopsy_columns <- unique(biopsy_columns)
            }

            biopsy_data <- data[, biopsy_columns, drop = FALSE]
            biopsy_data[] <- lapply(biopsy_data, private$.toNumeric)
            names(biopsy_data) <- biopsy_columns
            biopsy_data
        },

        .performHeterogeneityAnalysis = function(whole_section, biopsy_data, spatial_regions = NULL, study_design = "reference_based") {
            cv_threshold <- self$options$cv_threshold
            correlation_threshold <- self$options$correlation_threshold
            analysis_type <- self$options$analysis_type

            private$.repro_stats <- private$.analyzeReproducibility(
                whole_section, biopsy_data, correlation_threshold, cv_threshold, study_design)

            private$.analyzeSamplingBias(whole_section, biopsy_data)

            if (self$options$variance_components ||
                analysis_type == "variability" ||
                analysis_type == "comprehensive") {
                private$.analyzeVarianceComponents(whole_section, biopsy_data)
                private$.vc_done <- TRUE
            }

            if (self$options$sample_size_planning || analysis_type == "comprehensive") {
                private$.performSampleSizePlanning()
            }

            if (!is.null(spatial_regions)) {
                private$.analyzeSpatialHeterogeneity(whole_section, biopsy_data, spatial_regions)
            }
            if (self$options$compareCompartments && !is.null(spatial_regions)) {
                private$.compareCompartments(whole_section, biopsy_data, spatial_regions)
            }
            if (self$options$compartmentTests && !is.null(spatial_regions)) {
                private$.performCompartmentTests(whole_section, biopsy_data, spatial_regions)
            }

            # The sampling strategy changes no computation - only this note. The
            # default ("Not specified") prints nothing: a warning on every default
            # run trained users to ignore warnings. The analysis-type notes are
            # gone too; the one for "Comprehensive" listed modules (bias, power)
            # that did not run in inter-regional designs.
            sampling_strategy <- self$options$sampling_strategy
            private$.strategy_notes <- if (sampling_strategy == "systematic") {
                .("Sampling strategy recorded as systematic. The analysis treats the regions as exchangeable measurements of each case and applies no design-based adjustment; if the sampling pattern follows tissue architecture, the CV and ICC describe that pattern rather than the tumour as a whole.")
            } else if (sampling_strategy == "stratified") {
                .("Note: Stratified sampling was reported. This analysis applies NO design-based adjustment: all estimates treat the measurements as a simple sample, so stratification effects (if any) remain in the results.")
            } else {
                NULL
            }
        },

        .analyzeReproducibility = function(whole_section, biopsy_data, correlation_threshold = 0.80, cv_threshold = 20.0, study_design = "reference_based") {
            # Clear first: jamovi re-runs .run() on data-cell edits WITHOUT
            # firing clearWith, so uncleared addRow() calls duplicate rows.
            repro_table <- self$results$reproducibilitytable
            repro_table$deleteRows()

            min_pairs <- private$.CLINICAL_CONSTANTS$MIN_CASES_ANALYSIS
            X <- as.matrix(biopsy_data)
            storage.mode(X) <- "double"
            region_names <- colnames(X)
            n_biopsies <- ncol(X)
            has_reference <- !is.null(whole_section) && study_design == "reference_based"
            is_constant <- private$.isConstant
            sparse <- character(0)
            constant <- character(0)

            # Correlations need at least 5 pairs. With pairwise-complete data and
            # no minimum, a region measured in 2 cases gave r = +/-1 exactly, and
            # that value entered the headline mean correlation.
            inter_corr <- numeric(0)
            if (n_biopsies >= 2) {
                for (a in 1:(n_biopsies - 1)) for (b in (a + 1):n_biopsies) {
                    ok <- !is.na(X[, a]) & !is.na(X[, b])
                    if (sum(ok) < min_pairs) {
                        sparse <- c(sparse, sprintf("%s / %s (n = %d)", region_names[a], region_names[b], sum(ok)))
                        next
                    }
                    if (is_constant(X[ok, a]) || is_constant(X[ok, b])) next
                    inter_corr <- c(inter_corr, stats::cor(X[ok, a], X[ok, b], method = "spearman"))
                }
            }
            mean_inter_biopsy <- if (length(inter_corr) > 0) mean(inter_corr) else NA_real_

            ref_corr <- stats::setNames(rep(NA_real_, n_biopsies), region_names)
            ref_n <- stats::setNames(integer(n_biopsies), region_names)
            if (has_reference) {
                if (is_constant(whole_section)) constant <- c(constant, self$options$wholesection)
                for (j in seq_len(n_biopsies)) {
                    ok <- !is.na(whole_section) & !is.na(X[, j])
                    ref_n[j] <- sum(ok)
                    if (ref_n[j] < min_pairs) {
                        sparse <- c(sparse, sprintf("%s (n = %d)", region_names[j], ref_n[j]))
                        next
                    }
                    if (is_constant(whole_section[ok]) || is_constant(X[ok, j])) next
                    ref_corr[j] <- stats::cor(whole_section[ok], X[ok, j], method = "spearman")
                }
            }
            constant <- c(constant, region_names[vapply(seq_len(n_biopsies), function(j) is_constant(X[, j]), logical(1))])
            valid <- !is.na(ref_corr)
            correlations <- if (has_reference) ref_corr[valid] else inter_corr
            ref_ci <- lapply(seq_len(n_biopsies), function(j)
                if (valid[j]) private$.spearmanCI(ref_corr[[j]], ref_n[[j]]) else c(NA_real_, NA_real_))
            names(ref_ci) <- region_names
            # A region counts against the correlation criterion only when it is SHOWN
            # to be below the threshold (upper 95% limit below it). The lowest point
            # estimate of k equally good regions falls as k grows, so grading the
            # minimum penalised studies for sampling more cores.
            shown_below <- region_names[valid & vapply(ref_ci, function(ci) !is.na(ci[2]) && ci[2] < correlation_threshold, logical(1))]
            # Regions whose correlation could not be estimated are never silently
            # counted as agreeing.
            unassessed <- if (has_reference) region_names[!valid] else
                region_names[vapply(seq_len(n_biopsies), function(j)
                    is_constant(X[, j]) || sum(!is.na(X[, j])) < min_pairs, logical(1))]

            if (length(sparse) > 0) {
                repro_table$setNote("sparse_pairs", private$.noteSafe(sprintf(
                    .("Correlations from fewer than %d pairs are not reported: %s."),
                    min_pairs, htmltools::htmlEscape(paste(sparse, collapse = ", ")))))
            }
            if (length(constant) > 0) {
                repro_table$setNote("constant", private$.noteSafe(sprintf(
                    .("Correlations involving a measurement with the same value in every case are not defined: %s."),
                    htmltools::htmlEscape(paste(unique(constant), collapse = ", ")))))
            }

            # Row 1: mean regional-reference correlation, graded against the
            # user's threshold. With several regions each region also gets a row:
            # the verdict uses the LOWEST one, since any single region must be able
            # to stand in for the reference (an average of 0.99, 0.99, 0.99 and
            # 0.72 used to pass a 0.80 threshold).
            mean_ref_ci <- c(NA_real_, NA_real_)
            if (has_reference) {
                if (any(valid)) {
                    r <- mean(ref_corr[valid])
                    mean_ref_ci <- private$.spearmanCI(r, min(ref_n[valid]))
                    repro_table$addRow(rowKey = 1, values = list(
                        metric = .("Mean Regional-Reference Correlation"),
                        value = r, ci_lower = mean_ref_ci[1], ci_upper = mean_ref_ci[2],
                        interpretation = private$.corrGrade(r, correlation_threshold)))
                    if (sum(valid) > 1) {
                        for (j in which(valid)) {
                            cij <- ref_ci[[j]]
                            repro_table$addRow(rowKey = 100 + j, values = list(
                                metric = sprintf(.("Spearman correlation: %s vs reference"), region_names[j]),
                                value = ref_corr[[j]], ci_lower = cij[1], ci_upper = cij[2],
                                interpretation = private$.corrGrade(ref_corr[[j]], correlation_threshold)))
                        }
                    }
                    repro_table$setNote("corr_ci", if (sum(valid) > 1)
                        .("Correlation CIs use the Fisher z transformation with the Spearman variance of Bonett & Wright (2000); the CI of the mean correlation uses the smallest number of pairs and is conservative. Coverage falls below 95% when r is above about 0.97.")
                      else
                        .("Correlation CIs use the Fisher z transformation with the Spearman variance of Bonett & Wright (2000). Coverage falls below 95% when r is above about 0.97."))
                } else {
                    repro_table$addRow(rowKey = 1, values = list(
                        metric = .("Mean Regional-Reference Correlation"),
                        value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
                        interpretation = .("Not estimable - see note")))
                }
            }

            # Row 2: ICC(2,1) absolute agreement - always written. It used to be
            # replaced by a mean Spearman correlation on five fallback paths, or
            # to vanish without a word when not even that was defined.
            icc <- private$.calculateICC(if (has_reference) whole_section else NULL, biopsy_data)
            private$.icc_info <- icc
            if (!is.null(icc$result)) {
                a <- icc$result$agreement
                repro_table$addRow(rowKey = 2, values = list(
                    metric = .("ICC(2,1) - absolute agreement"),
                    value = a$value, ci_lower = a$lower, ci_upper = a$upper,
                    interpretation = switch(private$.iccBand(a$value),
                        excellent = .("Excellent reliability"), good = .("Good reliability"),
                        moderate = .("Moderate reliability"), poor = .("Poor reliability"))))
                # The consistency form beside it: the two differ exactly when there
                # is a systematic offset between measurements.
                cons <- icc$result$consistency
                repro_table$addRow(rowKey = 21, values = list(
                    metric = .("ICC(3,1) - consistency (bias-blind)"),
                    value = cons$value, ci_lower = cons$lower, ci_upper = cons$upper,
                    interpretation = .("Ignores systematic offset; compare with absolute agreement above")))
                repro_table$setNote("icc_bands", .("Reliability bands follow Koo & Li (2016): below 0.50 poor, 0.50 to 0.75 moderate, 0.75 to 0.90 good, above 0.90 excellent. Judge the band from the 95% CI as well as from the point estimate."))
            } else {
                repro_table$addRow(rowKey = 2, values = list(
                    metric = .("ICC(2,1) - absolute agreement"),
                    value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
                    interpretation = .("Not estimable - see note")))
                repro_table$setNote("icc_not_estimable", icc$reason)
                private$.addNotice("WARNING", .("ICC not estimable"), icc$reason)
            }
            if (length(icc$dropped) > 0) {
                repro_table$setNote("icc_dropped", private$.noteSafe(sprintf(
                    .("Regional measurements with fewer than %d values are left out of the ICC and the variance components: %s."),
                    min_pairs, htmltools::htmlEscape(paste(icc$dropped, collapse = ", ")))))
            }

            # Row 3: mean inter-regional correlation, graded against the SAME
            # user threshold as row 1 (fixed 0.80/0.60 used to keep the
            # correlation_threshold option out of the inter-regional table).
            if (!is.na(mean_inter_biopsy)) {
                repro_table$addRow(rowKey = 3, values = list(
                    metric = .("Mean Inter-Regional Correlation"),
                    value = mean_inter_biopsy, ci_lower = NA_real_, ci_upper = NA_real_,
                    interpretation = private$.corrGrade(mean_inter_biopsy, correlation_threshold)))
            }

            # Rows 4/5: mean and median per-case CV (shared helper, so the table
            # and the narrative can never report different CVs).
            cv_values <- private$.perCaseCV(whole_section, biopsy_data, has_reference, private$.cv_floor, with_status = TRUE)
            status <- attr(cv_values, "status")
            within_cv <- private$.rmsCV(cv_values)
            median_cv <- if (any(!is.na(cv_values))) stats::median(cv_values, na.rm = TRUE) else NA_real_
            cv_grade <- function(x) if (is.na(x)) .("Not estimable") else
                if (x <= cv_threshold / 2) .("Low variability") else
                if (x <= cv_threshold) .("Moderate variability") else .("High variability")
            repro_table$addRow(rowKey = 4, values = list(
                metric = if (has_reference) .("Within-case CV (%) - region vs reference")
                         else .("Within-case CV (%) - between regions"),
                value = within_cv, ci_lower = NA_real_, ci_upper = NA_real_,
                interpretation = cv_grade(within_cv)))
            repro_table$addRow(rowKey = 5, values = list(
                metric = .("Median per-case CV (%)"),
                value = median_cv, ci_lower = NA_real_, ci_upper = NA_real_,
                interpretation = if (is.na(median_cv)) .("Not estimable") else .("Descriptive, robust to a few extreme cases; not graded")))

            n_floor <- sum(status %in% c("floor", "floor_identical"))
            cv_note <- .("Per-case CV = SD / mean of that case's measurements. The within-case CV is their root mean square, which unlike their plain average does not fall when a case has fewer measurements; the median is descriptive and not graded.")
            if (n_floor > 0 && private$.cv_floor < 1e-6)
                cv_note <- paste(cv_note, sprintf(
                    .("%d case(s) whose mean is 0 were left out, because their CV is undefined."), n_floor))
            else if (n_floor > 0)
                cv_note <- paste(cv_note, sprintf(
                    .("%d case(s) whose mean is below %s (2%% of the 95th percentile of all values) were left out, because the CV is unstable near zero; %d of them had identical measurements."),
                    n_floor, sprintf("%.3g", private$.cv_floor), sum(status == "floor_identical")))
            repro_table$setNote("cv", cv_note)

            min_j <- if (any(valid)) which(valid)[which.min(ref_corr[valid])] else NA_integer_
            list(icc_value = if (!is.null(icc$result)) icc$result$agreement$value else NA_real_,
                 icc_lower = if (!is.null(icc$result)) icc$result$agreement$lower else NA_real_,
                 icc_upper = if (!is.null(icc$result)) icc$result$agreement$upper else NA_real_,
                 icc_method = if (!is.null(icc$result)) "icc" else "none",
                 icc_n = icc$n,
                 icc_dropped = icc$dropped,
                 correlations = correlations,
                 ref_corr = ref_corr[valid],
                 mean_ref_corr = if (has_reference && any(valid)) mean(ref_corr[valid]) else NA_real_,
                 mean_ref_ci = mean_ref_ci,
                 min_ref_corr = if (!is.na(min_j)) ref_corr[[min_j]] else NA_real_,
                 min_ref_name = if (!is.na(min_j)) region_names[min_j] else NA_character_,
                 ref_ci = ref_ci[valid],
                 shown_below = shown_below,
                 unassessed = unassessed,
                 mean_inter_biopsy = mean_inter_biopsy,
                 within_cv = within_cv, median_cv = median_cv,
                 case_cv = as.numeric(cv_values))
        },

        # Threshold attainment is reported as such. The same number used to be
        # graded four ways on one screen (fixed 0.80/0.60 in the table, the
        # user threshold in Key Findings, "excellent" at the threshold in the
        # copy-ready text, fixed 0.7/0.5 in the summary).
        .corrGrade = function(r, threshold) {
            if (r >= threshold) sprintf(.("Meets your threshold (\u2265 %s)"), threshold)
            else sprintf(.("Below your threshold (< %s)"), threshold)
        },

        .analyzeSamplingBias = function(whole_section, biopsy_data) {
            # Clear first: jamovi re-runs .run() on data-cell edits WITHOUT
            # firing clearWith, so uncleared addRow() calls duplicate rows.
            bias_table <- self$results$samplingbiastable
            bias_table$deleteRows()

            X <- as.matrix(biopsy_data)
            storage.mode(X) <- "double"
            region_names <- colnames(X)
            min_pairs <- private$.CLINICAL_CONSTANTS$MIN_PAIRS_BIAS
            margin <- self$options$bias_margin
            has_reference <- !is.null(whole_section)
            rows <- list()
            skipped <- character(0)
            add <- function(x, y, name, pooled, comparator, level = NULL) {
                r <- private$.biasRow(x, y, level)
                r$name <- name
                r$pooled <- pooled
                r$comparator <- comparator
                rows[[length(rows) + 1]] <<- r
            }

            # The level each difference is regressed on for proportional bias: the
            # reference value (known for every case of the row, and on the scale
            # pathologists read), or without a reference the case mean of the regions.
            # The mean of the OTHER regions was tried and dropped (release review
            # 2026-09-19): a site effect the regions share made unbiased regions
            # MATERIAL, and where the other regions were sparse the slope silently came
            # from a few cases. Every level shares an error with a reading, so a slope
            # only stops a difference from being ruled out.
            if (has_reference) {
                for (j in seq_len(ncol(X))) {
                    ok <- !is.na(whole_section) & !is.na(X[, j])
                    if (sum(ok) < min_pairs) {
                        skipped <- c(skipped, region_names[j])
                        next
                    }
                    add(X[ok, j], whole_section[ok], region_names[j], FALSE, "reference", level = whole_section[ok])
                }
                # The mean of all regions matters when the protocol averages them;
                # opposite regional offsets cancel in it.
                if (ncol(X) >= 2) {
                    region_mean <- rowMeans(X, na.rm = TRUE)
                    ok <- !is.na(whole_section) & is.finite(region_mean)
                    if (sum(ok) >= min_pairs) add(region_mean[ok], whole_section[ok], NA_character_, TRUE, "reference",
                                                  level = whole_section[ok])
                }
            } else if (ncol(X) == 2) {
                # Without a reference the regions are compared with one another: two
                # regions reading 27% apart used to be reported as agreeing, because
                # correlation and CV cannot see a proportional offset.
                ok <- !is.na(X[, 1]) & !is.na(X[, 2])
                if (sum(ok) >= min_pairs) add(X[ok, 2], X[ok, 1], region_names[2], FALSE, region_names[1],
                                              level = rowMeans(X[ok, , drop = FALSE]))
                else skipped <- c(skipped, region_names[2])
            } else {
                case_mean <- rowMeans(X, na.rm = TRUE)
                for (j in seq_len(ncol(X))) {
                    others <- rowMeans(X[, -j, drop = FALSE], na.rm = TRUE)
                    ok <- !is.na(X[, j]) & is.finite(others)
                    if (sum(ok) < min_pairs) {
                        skipped <- c(skipped, region_names[j])
                        next
                    }
                    add(X[ok, j], others[ok], region_names[j], FALSE, "others", level = case_mean[ok])
                }
            }

            # Three zones on the difference as a percentage of the comparison mean:
            # RULED OUT when the 90% CI lies inside the margin (two one-sided tests,
            # Schuirmann 1987; each row must pass, which needs no multiplicity
            # adjustment); MATERIAL when a CI adjusted for the m comparisons (regions
            # and the mean of all regions, Bonferroni) lies entirely beyond it;
            # otherwise INCONCLUSIVE. The earlier rule (point estimate > 5% and p < 0.05
            # against ZERO) called a truly 3% offset material in up to a third of
            # studies, and demanding the 95% CI inside the margin made the green
            # verdict almost unreachable at usual study sizes.
            # A constant reference is one fixed number, not a reference.
            #
            # A difference that changes with the level (slope shown at p < 0.05 / m,
            # HC3) is also judged at the 5th and 95th percentiles of the level: ruled
            # out only if both 90% CIs lie inside the margin (intersection-union). It
            # is never made MATERIAL by the slope, which with one reading per method
            # cannot be told from a regression-to-the-mean artefact (.levelFit). The
            # gate is Bonferroni: at p < 0.05 each row flagged by chance cost
            # equivalent data the green verdict in 10-17% of simulated studies.
            # Without the gate the ends' CIs, about twice as wide as the mean's, made
            # "met" unreachable at usual sizes.
            reference_constant <- has_reference && private$.isConstant(whole_section)
            m <- length(rows)
            level_material <- 1 - 0.10 / max(m, 1)
            slope_gate <- 0.05 / max(m, 1)
            for (i in seq_along(rows)) {
                r <- rows[[i]]
                ci90 <- private$.relCI(r, 0.90)
                ci_adj <- private$.relCI(r, level_material)
                rows[[i]]$rel_ci90 <- ci90
                rows[[i]]$material <- !reference_constant && !any(is.na(ci_adj)) &&
                    (min(ci_adj) > margin || max(ci_adj) < -margin)
                rows[[i]]$equivalent <- !any(is.na(ci90)) && all(abs(ci90) <= margin)
                margin_abs <- margin / 100 * abs(r$ref_mean)
                rows[[i]]$margin_abs <- margin_abs
                rows[[i]]$prop_shown <- !is.null(r$prop) && !is.na(r$rel) && r$prop$p < slope_gate
                if (rows[[i]]$prop_shown) {
                    e90 <- private$.endCI(r$prop, 0.90)
                    rows[[i]]$end_ci90 <- e90
                    rows[[i]]$equivalent <- rows[[i]]$equivalent && all(abs(e90) <= margin_abs)
                }
            }

            for (i in seq_along(rows)) {
                r <- rows[[i]]
                bias_table$addRow(rowKey = i, values = list(
                    comparison = if (r$pooled) .("Mean of all regions vs reference")
                                 else if (identical(r$comparator, "reference")) sprintf(.("%s vs reference"), r$name)
                                 else if (identical(r$comparator, "others")) sprintf(.("%s vs mean of the other regions"), r$name)
                                 else sprintf(.("%s vs %s"), r$name, r$comparator),
                    n = as.integer(r$n),
                    mean_diff = r$mean_diff,
                    ci_lower = r$ci[1], ci_upper = r$ci[2],
                    loa_lower = r$loa[1], loa_upper = r$loa[2],
                    loa_lower_lcl = r$loa_ci[1, 1], loa_lower_ucl = r$loa_ci[1, 2],
                    loa_upper_lcl = r$loa_ci[2, 1], loa_upper_ucl = r$loa_ci[2, 2],
                    p_value = r$p,
                    effect_size = r$g,
                    slope = if (is.null(r$prop)) NA_real_ else r$prop$slope,
                    slope_p = if (is.null(r$prop)) NA_real_ else r$prop$p,
                    clinical_impact = private$.impactText(r, judged = !reference_constant)))
            }

            if (m > 1) {
                bias_table$setNote("multiplicity", sprintf(
                    .("%d paired comparisons are reported; the p-values test against zero and are unadjusted. With a margin of %s%% of the comparison mean, a difference is ruled out when its 90%% CI lies within the margin (two one-sided tests) and shown to be material when its Bonferroni-adjusted %s%% CI lies entirely beyond the margin; otherwise it is inconclusive."),
                    m, margin, sprintf("%.1f", 100 * level_material)))
            } else if (m == 1) {
                bias_table$setNote("multiplicity", sprintf(
                    .("With a margin of %s%% of the comparison mean, a difference is ruled out when its 90%% CI lies within the margin (two one-sided tests) and shown to be material when that CI lies entirely beyond the margin; otherwise it is inconclusive. The p-value tests against zero."),
                    margin))
            }
            if (m > 0) {
                bias_table$setNote("loa_ci", .("The CIs of the limits of agreement use the approximate standard error of Bland & Altman (1999), SD x sqrt(1/n + 1.96^2 / (2(n - 1))); they are wide in small samples."))
                bias_table$setNote("loa", if (has_reference)
                    .("Mean difference = region minus reference. Limits of agreement (Bland & Altman 1986): 95% of individual differences are expected between these bounds if the differences are roughly normal.")
                  else
                    .("Mean difference = region minus the other region (or the mean of the other regions). Limits of agreement (Bland & Altman 1986): 95% of individual differences are expected between these bounds if the differences are roughly normal."))
            } else if (!has_reference) {
                bias_table$setNote("loa", .("Too few cases have paired regional measurements to compare the regions."))
            }
            if (any(vapply(rows, function(r) isTRUE(r$constant), logical(1)))) {
                bias_table$setNote("constant", .("Where every case differs by exactly the same amount there is no sampling variance, so no p-value or effect size is reported; the offset itself is exact."))
            }
            if (m > 0) {
                bias_table$setNote("slope", paste(.("Proportional bias:"), private$.levelMethodText(has_reference)))
                if (any(vapply(rows, function(r) is.null(r$prop), logical(1))))
                    bias_table$setNote("slope_blank", .("A blank slope means that no level check was possible: fewer than 5 cases or 5 distinct levels (an ordinal score has no meaningful slope), or the same difference in every case."))
                if (any(vapply(rows, function(r) isTRUE(r$prop_shown), logical(1))))
                    bias_table$setNote("loa_level", .("Where the slope was shown, the limits of agreement, which assume a difference that does not change with the level, are too wide in the middle of the range and too narrow at its ends."))
            }
            if (reference_constant) {
                bias_table$setNote("reference_constant", .("The reference has the same value in every case, so these differences compare the regions with one fixed number; none is judged material."))
            }
            if (length(skipped) > 0) {
                bias_table$setNote("skipped", private$.noteSafe(sprintf(
                    .("Regions with fewer than %d paired values are not tested: %s."),
                    min_pairs, htmltools::htmlEscape(paste(skipped, collapse = ", ")))))
            }

            private$.bias_stats <- list(
                rows = rows,
                skipped = skipped,
                any_material = any(vapply(rows, function(r) isTRUE(r$material), logical(1))),
                all_equivalent = length(rows) > 0 && all(vapply(rows, function(r) isTRUE(r$equivalent), logical(1))),
                reference_constant = reference_constant)
        },

        # Clinical impact = the zone of the decision rule at bias_margin, with the
        # 90% CI the rule reads. Fixed 5% / 15% bands contradicted the verdict
        # whenever bias_margin was not 5, and a 95% CI beside the zone could cross
        # the margin of a row the 90% CI rules out. The Bonferroni CI of the
        # "material" zone contains the 90% CI, so the interval shown agrees with
        # every zone. A constant reference is not judged (the verdict is
        # "insufficient").
        .impactText = function(r, judged = TRUE) {
            if (is.na(r$rel)) return(.("Not assessable (comparison mean near zero)"))
            margin <- self$options$bias_margin
            rel <- sprintf("%+.1f", r$rel)
            zone <- if (!judged) "none" else if (isTRUE(r$material)) "material"
                    else if (isTRUE(r$equivalent)) "within" else "open"
            if (isTRUE(r$constant)) {
                return(switch(zone,
                    material = sprintf(.("%s%%, identical in every case: beyond the %s%% margin"), rel, margin),
                    within = sprintf(.("%s%%, identical in every case: within the %s%% margin"), rel, margin),
                    sprintf(.("%s%%, identical in every case"), rel)))
            }
            # A difference that changes with the level: both ends in the marker's own
            # units (points of Ki67 %, H-score units) at the level where they apply,
            # beside the margin in the same units - a percentage of the cohort mean
            # hides where on the scale it fails.
            # The slope explains why a row is not ruled out; a row material by its
            # average keeps the average-difference text.
            if (isTRUE(r$prop_shown) && zone != "material") {
                end <- private$.endValues(r)
                if (zone == "open")
                    return(sprintf(.("Changes with the level: %s at %s (90%% CI %s to %s), %s at %s (90%% CI %s to %s); inconclusive at the \u00b1%s margin"),
                                   end$fit[1], end$at[1], end$ci[1, 1], end$ci[1, 2],
                                   end$fit[2], end$at[2], end$ci[2, 1], end$ci[2, 2], end$margin))
                return(switch(zone,
                    within = sprintf(.("Changes with the level: %s at %s, %s at %s (margin \u00b1%s); ruled out, both 90%% CIs within the margin"),
                                     end$fit[1], end$at[1], end$fit[2], end$at[2], end$margin),
                    sprintf(.("Changes with the level: %s at %s, %s at %s"), end$fit[1], end$at[1], end$fit[2], end$at[2])))
            }
            ci <- r$rel_ci90
            if (is.null(ci) || any(is.na(ci))) return(sprintf(.("%s%% of the comparison mean"), rel))
            lo <- sprintf("%.1f", ci[1])
            hi <- sprintf("%.1f", ci[2])
            switch(zone,
                material = sprintf(.("%s%% (90%% CI %s%% to %s%%): shown to exceed the %s%% margin"), rel, lo, hi, margin),
                within = sprintf(.("%s%% (90%% CI %s%% to %s%%): ruled out, within the %s%% margin"), rel, lo, hi, margin),
                open = sprintf(.("%s%% (90%% CI %s%% to %s%%): inconclusive at the %s%% margin"), rel, lo, hi, margin),
                sprintf(.("%s%% (90%% CI %s%% to %s%%)"), rel, lo, hi))
        },

        .analyzeVarianceComponents = function(whole_section, biopsy_data) {
            # Variance components from a TWO-WAY random-effects decomposition,
            #
            #     value_ij = mu + case_i + method_j + e_ij
            #
            # on the same complete-case matrix as the ICC. Expected mean squares
            # for the balanced two-way random model give
            #     sigma^2_case   = (MS_case   - MS_error) / k
            #     sigma^2_method = (MS_method - MS_error) / n
            #     sigma^2_error  =  MS_error
            # which sum to the total variance, so the percentages sum to 100.
            has_reference <- !is.null(whole_section)
            variance_table <- self$results$variancetable

            info <- private$.icc_info
            if (is.null(info)) info <- private$.calculateICC(whole_section, biopsy_data)
            Y <- info$Y
            n <- nrow(Y)
            k <- ncol(Y)

            insufficient <- function(msg) {
                variance_table$setNote("vc", msg)
                for (r in 1:4) {
                    variance_table$setRow(rowKey = r, values = list(
                        variance = NA_real_, percentage = NA_real_,
                        contribution = .("Not estimable")
                    ))
                }
            }

            # The same minimum as the ICC from the same mean squares (a full
            # decomposition used to be shown from 2-4 cases beside "ICC not
            # estimable: needs at least 5").
            min_n <- private$.CLINICAL_CONSTANTS$MIN_CASES_ANALYSIS
            if (n < min_n || k < 2) {
                insufficient(sprintf(.("Variance components need at least %d cases with every measurement, measured by at least 2 methods; %d case(s) qualify."), min_n, n))
                return()
            }
            ms <- private$.meanSquares(Y)
            # Identical values everywhere: floating-point residue used to be
            # reported as "100% High sampling variability".
            if (ms$sst <= 1e-12 * max(1, sum(Y^2))) {
                insufficient(.("No variability in the data: every measurement has the same value, so there is nothing to decompose."))
                return()
            }

            var_case   <- (ms$msr - ms$mse) / k
            var_method <- (ms$msc - ms$mse) / n
            var_error  <- ms$mse

            trunc_case <- var_case < 0
            trunc_method <- var_method < 0
            var_case   <- max(var_case, 0)
            var_method <- max(var_method, 0)
            total_variance <- var_case + var_method + var_error

            notes <- sprintf(
                .("Two-way random-effects decomposition (value = case + method + error) on %d cases measured by %d methods; components sum to the total variance."),
                n, k)
            if (info$n_incomplete > 0)
                notes <- paste(notes, sprintf(
                    .("%d case(s) with an incomplete set of measurements were excluded."), info$n_incomplete))
            if (length(info$dropped) > 0)
                notes <- paste(notes, sprintf(
                    .("Regional measurements with fewer than %d values were left out: %s."),
                    min_n, htmltools::htmlEscape(paste(info$dropped, collapse = ", "))))
            if (trunc_case && trunc_method) {
                notes <- paste(notes,
                    .("The case and method variance estimates were negative and have been truncated to zero."))
            } else if (trunc_case) {
                notes <- paste(notes,
                    .("The case variance estimate was negative and has been truncated to zero."))
            } else if (trunc_method) {
                notes <- paste(notes,
                    .("The method variance estimate was negative and has been truncated to zero."))
            }
            if (has_reference)
                notes <- paste(notes, .("Shares depend on how much the cases differ from one another: a small method share can still be a clinically material offset (see the Sampling Bias Analysis table)."))
            variance_table$setNote("vc", private$.noteSafe(notes))

            pct <- function(x) if (total_variance > 0) x / total_variance * 100 else NA_real_
            case_pct   <- pct(var_case)
            error_pct  <- pct(var_error)
            method_pct <- pct(var_method)
            private$.vc_shares <- c(case = case_pct, within = error_pct, method = method_pct)

            variance_table$setRow(rowKey = 1L, values = list(
                component = .("Between-Case Variance"),
                variance = var_case,
                percentage = case_pct,
                contribution = ifelse(!is.na(case_pct) && case_pct >= 60, .("Major contributor"),
                                     ifelse(!is.na(case_pct) && case_pct >= 30, .("Moderate contributor"), .("Minor contributor")))
            ))
            variance_table$setRow(rowKey = 2L, values = list(
                component = .("Within-Case Variance (Sampling)"),
                variance = var_error,
                percentage = error_pct,
                contribution = ifelse(!is.na(error_pct) && error_pct >= 30, .("High sampling variability"),
                                     ifelse(!is.na(error_pct) && error_pct >= 15, .("Moderate sampling variability"), .("Low sampling variability")))
            ))
            variance_table$setRow(rowKey = 3L, values = list(
                component = if (has_reference) .("Method Variance") else .("Regional Method Variance"),
                variance = var_method,
                percentage = method_pct,
                # A share of the total variance, not a clinical size: with widely
                # spread cases a 15% offset is a small share ("Negligible method
                # differences" sat beside a NOT ADEQUATE verdict).
                contribution = ifelse(!is.na(method_pct) && method_pct >= 20, .("Large share of total variance"),
                                     ifelse(!is.na(method_pct) && method_pct >= 10, .("Moderate share of total variance"), .("Small share of total variance")))
            ))
            variance_table$setRow(rowKey = 4L, values = list(
                component = .("Total Variance"),
                variance = total_variance,
                percentage = 100,
                contribution = .("Sum of all variance components")
            ))
        },

        # Cases needed to estimate the ICC with a chosen precision (Bonett 2002):
        #     n = 8 z^2 (1 - rho)^2 (1 + (k - 1) rho)^2 / (k (k - 1) w^2) + 1
        # for a 95% CI of full width w with k measurements per case.
        #
        # This replaced a power table for H0: correlation = 0 at Cohen's r = 0.1,
        # 0.3 and 0.5. No agreement study needs to show that a biopsy correlates
        # with its own whole section better than chance, and on data with a
        # tightly estimated ICC the old table printed "Substantial sample
        # increase recommended" with a required n of 830.
        .performSampleSizePlanning = function() {
            table <- self$results$samplesizetable
            table$deleteRows()
            private$.ss_done <- TRUE
            info <- private$.icc_info
            k <- if (is.null(info)) 0 else info$k
            n <- if (is.null(info)) 0 else info$n
            if (k < 2) {
                table$setNote("bonett", .("Sample-size planning needs at least two measurements per case."))
                return()
            }
            z <- stats::qnorm(0.975)
            plans <- list(
                list(rho = 0.75, label = .("Planning ICC 0.75 (lower limit of good reliability)")),
                list(rho = 0.90, label = .("Planning ICC 0.90 (lower limit of excellent reliability)")))
            # Bonett's formula gives the expected width of the EXACT F-based
            # interval, which exists for the consistency ICC. The observed row
            # therefore uses ICC(3,1): fed the absolute-agreement ICC it promised a
            # width of 0.04 beside an actual ICC(2,1) interval of 0.15 when a
            # systematic offset was present.
            observed <- if (!is.null(info$result)) info$result$consistency$value else NA_real_
            if (is.finite(observed) && observed > 0 && observed < 1)
                plans[[3]] <- list(rho = observed, label = .("Observed ICC(3,1), consistency"))
            # Never below the minimum this analysis itself requires: at high ICCs
            # the approximation returns 2 or 3 cases.
            min_cases <- private$.CLINICAL_CONSTANTS$MIN_CASES_ANALYSIS
            for (i in seq_along(plans)) {
                rho <- plans[[i]]$rho
                q <- 8 * z^2 * (1 - rho)^2 * (1 + (k - 1) * rho)^2 / (k * (k - 1))
                table$addRow(rowKey = i, values = list(
                    scenario = plans[[i]]$label,
                    planning_icc = rho,
                    width_current = if (n > 1) sqrt(q / (n - 1)) else NA_real_,
                    n_w20 = as.integer(max(ceiling(q / 0.20^2 + 1), min_cases)),
                    n_w10 = as.integer(max(ceiling(q / 0.10^2 + 1), min_cases))))
            }
            note <- sprintf(
                .("Bonett (2002) planning for the consistency ICC, ICC(3,1), with k = %d measurements per case and a 95%% confidence interval; this analysis has %d complete cases. The widths are expected values, so about half of the studies of that size obtain a wider interval. A systematic offset between the measurements widens the interval of the absolute-agreement ICC(2,1), which then needs more cases. The number of cases is never shown below the 5 this analysis requires. This is precision planning, not a hypothesis test."),
                k, n)
            if (n < 2)
                note <- paste(note, .("The expected width at the current size is blank because fewer than 2 cases have every measurement."))
            a <- if (!is.null(info$result)) info$result$agreement else NULL
            if (!is.null(a) && !is.na(a$lower) && !is.na(a$upper))
                note <- paste(note, sprintf(.("In these data the 95%% CI of ICC(2,1) has width %s."), sprintf("%.3f", a$upper - a$lower)))
            table$setNote("bonett", note)
        },

        # Compartments of the Spatial Region ID with enough cases, and the ones
        # skipped - one rule for the spatial table, the comparison table, the
        # tests and the plot (they used to use minimums of 2, 3 and 2).
        .compartments = function(spatial_regions) {
            min_n <- private$.CLINICAL_CONSTANTS$MIN_CASES_COMPARTMENT
            sr <- as.character(spatial_regions)
            # A blank label is a missing compartment: as a list name it cannot be
            # looked up, and the comparison table aborted on it.
            sr[!is.na(sr) & !nzchar(trimws(sr))] <- NA
            # Level order, the order set in jamovi (tables and plot used to follow
            # first appearance and the alphabet); a character state saved by an
            # earlier version keeps first appearance.
            levels_seen <- if (is.factor(spatial_regions)) intersect(levels(spatial_regions), sr[!is.na(sr)])
                           else unique(sr[!is.na(sr)])
            counts <- vapply(levels_seen, function(r) sum(sr == r, na.rm = TRUE), integer(1))
            small <- counts < min_n
            list(keep = levels_seen[!small], min_n = min_n, labels = sr, n_missing = sum(is.na(sr)),
                 skipped = if (any(small))
                     private$.noteSafe(htmltools::htmlEscape(paste(sprintf("%s (n = %d)", levels_seen[small], counts[small]), collapse = ", ")))
                 else NULL)
        },

        # Cases without a Spatial Region ID are left out of every compartment
        # output; the count is stated where it happens.
        .missingCompartmentNote = function(table, groups) {
            if (groups$n_missing > 0)
                table$setNote("missing_id", sprintf(
                    .("%d cases without a Spatial Region ID are left out of the compartment analysis."), groups$n_missing))
        },

        .analyzeSpatialHeterogeneity = function(whole_section, biopsy_data, spatial_regions) {
            # Clear first: jamovi re-runs .run() on data-cell edits WITHOUT
            # firing clearWith, so uncleared addRow() calls duplicate rows.
            spatial_table <- self$results$spatialanalysistable
            spatial_table$deleteRows()

            groups <- private$.compartments(spatial_regions)
            private$.missingCompartmentNote(spatial_table, groups)
            if (!is.null(groups$skipped)) {
                spatial_table$setNote("skipped_regions", sprintf(
                    .("Compartments with fewer than %d cases are not analysed: %s."),
                    groups$min_n, groups$skipped))
            }
            if (length(groups$keep) < 2) {
                spatial_table$setNote("too_few_regions", sprintf(
                    .("Spatial analysis needs at least 2 compartments with %d or more cases each; the Spatial Region ID variable supplies %d."),
                    groups$min_n, length(groups$keep)))
                return()
            }

            has_reference <- !is.null(whole_section)
            cv_thr <- self$options$cv_threshold
            for (i in seq_along(groups$keep)) {
                region <- groups$keep[i]
                mask <- !is.na(groups$labels) & groups$labels == region
                region_ws <- if (has_reference) whole_section[mask] else NULL
                region_bd <- biopsy_data[mask, , drop = FALSE]

                # CV per case, then averaged - pooling every measurement of every
                # case would measure between-patient spread, not heterogeneity
                # within a case, and invert the ranking of compartments.
                region_mean <- mean(c(region_ws, as.matrix(region_bd)), na.rm = TRUE)
                case_cvs <- private$.perCaseCV(region_ws, region_bd, has_reference, private$.cv_floor)
                region_cv <- private$.rmsCV(case_cvs)
                heterogeneity_level <- if (is.na(region_cv)) .("Not estimable") else
                    if (region_cv <= cv_thr / 2) .("Low") else
                    if (region_cv <= cv_thr) .("Moderate") else .("High")

                spatial_table$addRow(rowKey = i, values = list(
                    region = region,
                    n_cases = as.integer(sum(mask)),
                    mean_value = region_mean,
                    cv_percent = region_cv,
                    heterogeneity_level = heterogeneity_level
                ))
            }
            spatial_table$setNote("cv_bands", sprintf(
                .("Heterogeneity level grades the within-case CV (root mean square over the compartment's cases) against your %1$s%% threshold: Low at or below %2$s%%, Moderate up to %1$s%%, High above it."),
                private$.fmtNum(cv_thr), private$.fmtNum(cv_thr / 2)))
        },

        .generateHeterogeneityPlots = function(whole_section, biopsy_data, spatial_regions = NULL, study_design = "reference_based") {
            # Plot-ready state only: the analysed values, the labels and the CV
            # floor, so a redraw from a saved .omv needs nothing from .run().
            # Hidden plots get no state: it would only be saved in the .omv, and
            # ticking the plots re-runs the analysis anyway.
            analysis_type <- self$options$analysis_type
            if (!(self$options$show_variability_plots || analysis_type %in% c("variability", "comprehensive")))
                return()
            plot_data <- list(
                whole_section = whole_section,
                biopsy_data = as.data.frame(biopsy_data),
                ref_label = if (!is.null(whole_section)) sprintf(.("%s (reference)"), self$options$wholesection) else NULL,
                spatial_regions = if (is.null(spatial_regions)) NULL else droplevels(factor(spatial_regions)),
                cv_floor = private$.cv_floor
            )
            self$results$biopsyplot$setState(plot_data)
            self$results$variabilityplot$setState(plot_data)
            if (!is.null(spatial_regions)) {
                self$results$spatialplot$setState(plot_data)
            }
        },

        .biopsyplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            if (!requireNamespace('ggplot2', quietly = TRUE))
                return(FALSE)

            data <- image$state
            X <- as.matrix(data$biopsy_data)
            has_reference <- length(data$whole_section) > 0
            # Regions are labelled with their variable names. A state saved by an
            # earlier version has no ref_label; make.unique() keeps the factor
            # valid if a name repeats.
            ref_label <- if (is.null(data$ref_label)) .("Reference") else data$ref_label
            methods <- make.unique(c(if (has_reference) ref_label, colnames(X)))
            values <- c(if (has_reference) data$whole_section, as.vector(X))
            plot_df <- data.frame(
                Method = factor(rep(methods, each = nrow(X)), levels = methods),
                Value = values
            )
            plot_df <- plot_df[!is.na(plot_df$Value), ]

            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Method, y = Value, color = Method)) +
                # Outliers are among the jittered points already (drawn twice
                # before), and height = 0 keeps every point at its measured value.
                ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
                ggplot2::geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
                ggplot2::labs(
                    title = if (has_reference) .("Regional vs Reference Measurements") else .("Inter-Regional Measurements"),
                    subtitle = .("Distribution of IHC biomarker values across tissue regions"),
                    x = .("Measurement Location"),
                    y = .("Biomarker Value"),
                    color = .("Method")
                ) +
                ggtheme +
                # After ggtheme: jamovi's ggtheme is a complete theme that resets an
                # earlier theme() and drops vjust from axis.text.x. The x axis
                # already names each box, so the colour legend is not needed.
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
                               legend.position = "none")
            print(p)
            TRUE
        },

        .variabilityplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            if (!requireNamespace('ggplot2', quietly = TRUE))
                return(FALSE)

            data <- image$state
            cv_thr <- self$options$cv_threshold
            m <- as.matrix(data$biopsy_data)
            has_reference <- length(data$whole_section) > 0
            if (has_reference) m <- cbind(data$whole_section, m)
            if (nrow(m) == 0) return(FALSE)

            floor_value <- if (is.null(data$cv_floor)) 0 else data$cv_floor
            cv <- vapply(seq_len(nrow(m)), function(i) private$.calculateRobustCV(m[i, ], floor_value), numeric(1))
            case_mean <- rowMeans(m, na.rm = TRUE)
            ok <- !is.na(cv)
            # No case has two values to compare: nothing to plot.
            if (!any(ok)) return(FALSE)
            n_hidden <- sum(!ok & rowSums(!is.na(m)) >= 2)

            # CV against the case mean, not against row order: a trend line across
            # arbitrary row numbers implied a pattern with no meaning, while this
            # view shows where the CV becomes unstable (near-zero means).
            variability_df <- data.frame(Mean = case_mean[ok], CV_Percent = cv[ok])
            p <- ggplot2::ggplot(variability_df, ggplot2::aes(x = Mean, y = CV_Percent)) +
                ggplot2::geom_point(color = "steelblue", alpha = 0.7) +
                ggplot2::geom_hline(yintercept = c(cv_thr / 2, cv_thr), linetype = "dashed", alpha = 0.7) +
                ggplot2::annotate("text", x = Inf, y = cv_thr / 2, hjust = 1.05,
                                 label = sprintf(.("%s%% CV (threshold/2)"), private$.fmtNum(cv_thr / 2)), vjust = -0.5) +
                ggplot2::annotate("text", x = Inf, y = cv_thr, hjust = 1.05,
                                 label = sprintf(.("%s%% CV threshold"), private$.fmtNum(cv_thr)), vjust = -0.5) +
                ggplot2::labs(
                    title = .("Sampling Variability Analysis"),
                    subtitle = .("Per-case coefficient of variation against the case mean"),
                    x = .("Case mean (biomarker value)"),
                    y = .("Coefficient of Variation (%)"),
                    caption = if (n_hidden > 0) sprintf(.("%d case(s) with a mean near zero are not shown: their CV is unstable."), n_hidden) else NULL
                ) +
                ggtheme
            print(p)
            TRUE
        },

        .spatialplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state) || is.null(image$state$spatial_regions))
                return(FALSE)
            if (!requireNamespace('ggplot2', quietly = TRUE))
                return(FALSE)

            data <- image$state
            groups <- private$.compartments(data$spatial_regions)
            if (length(groups$keep) < 2) return(FALSE)

            has_reference <- length(data$whole_section) > 0
            floor_value <- if (is.null(data$cv_floor)) 0 else data$cv_floor
            region_stats <- do.call(rbind, lapply(groups$keep, function(region) {
                mask <- !is.na(groups$labels) & groups$labels == region
                ws <- if (has_reference) data$whole_section[mask] else NULL
                bd <- data$biopsy_data[mask, , drop = FALSE]
                cvs <- private$.perCaseCV(ws, bd, has_reference, floor = floor_value)
                data.frame(Region = region,
                           Mean_WS = mean(c(ws, as.matrix(bd)), na.rm = TRUE),
                           CV = private$.rmsCV(cvs),
                           stringsAsFactors = FALSE)
            }))

            # CV bands follow the user's cv_threshold (thr/2, thr), the same
            # grading as the spatial table beside this plot.
            cv_thr <- self$options$cv_threshold
            lvl_labels <- c(sprintf(.("Low (\u2264%s%%)"), private$.fmtNum(cv_thr / 2)),
                            sprintf(.("Moderate (%s-%s%%)"), private$.fmtNum(cv_thr / 2), private$.fmtNum(cv_thr)),
                            sprintf(.("High (>%s%%)"), private$.fmtNum(cv_thr)))
            region_stats$CV_Level <- cut(region_stats$CV,
                                       breaks = c(-Inf, cv_thr / 2, cv_thr, Inf),
                                       labels = lvl_labels)
            region_stats$Label <- ifelse(is.na(region_stats$CV), .("CV not estimable"),
                                         sprintf(.("CV: %s%%"), round(region_stats$CV, 1)))
            # The order of the tables (level order), not the alphabet.
            region_stats$Region <- factor(region_stats$Region, levels = groups$keep)

            # Sequential ColorBrewer YlOrRd: ordered, and distinguishable with
            # colour-vision deficiency (green/yellow/red is not).
            p <- ggplot2::ggplot(region_stats, ggplot2::aes(x = Region, y = Mean_WS)) +
                # show.legend = TRUE: with drop = FALSE, ggplot2 >= 3.5 draws an
                # empty key for a band no compartment reaches unless asked to.
                ggplot2::geom_col(ggplot2::aes(fill = CV_Level), alpha = 0.85, show.legend = TRUE) +
                ggplot2::geom_text(ggplot2::aes(label = Label), vjust = -0.5, size = 3) +
                ggplot2::labs(
                    title = .("Spatial Heterogeneity Analysis"),
                    subtitle = .("Mean biomarker values and variability by spatial region"),
                    x = .("Spatial Region"),
                    y = .("Mean Biomarker Value")
                ) +
                ggtheme +
                # After ggtheme: its palette scale replaced this fill scale (jamovi
                # colours, legend titled "CV_Level", unused band dropped) and its
                # complete theme reset the label rotation.
                ggplot2::scale_fill_manual(values = stats::setNames(c("#ffeda0", "#feb24c", "#f03b20"), lvl_labels),
                                         name = .("Variability Level"), drop = FALSE) +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
            print(p)
            TRUE
        },

        .calculateInterpretationMetrics = function(whole_section, biopsy_data, repro_stats = NULL, study_design = "reference_based") {
            has_reference <- !is.null(whole_section) && study_design == "reference_based"
            rs <- repro_stats
            bias <- private$.bias_stats
            all_measures <- if (has_reference) cbind(whole_section, as.matrix(biopsy_data)) else as.matrix(biopsy_data)
            shown_below <- if (is.null(rs$shown_below)) character(0) else rs$shown_below
            list(
                n_cases = nrow(biopsy_data),
                n_biopsies = ncol(biopsy_data),
                n_complete = sum(stats::complete.cases(all_measures)),
                overall_corr = if (has_reference) rs$mean_ref_corr else rs$mean_inter_biopsy,
                overall_ci = if (has_reference) rs$mean_ref_ci else c(NA_real_, NA_real_),
                # The correlation the verdict grades: the mean, unless a region is
                # SHOWN to fall below the threshold - then the lowest such region.
                verdict_corr = if (has_reference && length(shown_below) > 0) min(rs$ref_corr[shown_below])
                               else if (has_reference) rs$mean_ref_corr else rs$mean_inter_biopsy,
                ref_corr = rs$ref_corr,
                ref_ci = rs$ref_ci,
                shown_below = shown_below,
                min_ref_name = rs$min_ref_name,
                unassessed = unique(c(rs$unassessed, if (is.null(bias)) character(0) else bias$skipped)),
                within_cv = rs$within_cv,
                median_cv = rs$median_cv,
                case_cv = rs$case_cv,
                single_cv = private$.singleMeasurementCV(whole_section, biopsy_data, has_reference),
                icc = rs$icc_value,
                icc_lower = rs$icc_lower,
                icc_upper = rs$icc_upper,
                icc_method = rs$icc_method,
                icc_n = rs$icc_n,
                icc_dropped = rs$icc_dropped,
                correlations = rs$correlations,
                mean_inter_biopsy = rs$mean_inter_biopsy,
                has_reference = has_reference,
                reference_name = self$options$wholesection,
                bias_rows = if (is.null(bias)) list() else bias$rows,
                bias_material = isTRUE(bias$any_material),
                bias_equivalent = isTRUE(bias$all_equivalent),
                reference_constant = isTRUE(bias$reference_constant)
            )
        },

        # ONE verdict for every panel (the bias veto used to reach only the
        # Clinical Assessment box). "met" needs the correlation and CV thresholds,
        # every systematic difference ruled out within the margin and every
        # selected region assessed; with the thresholds met but agreement not
        # confirmed the verdict is "met_uncertain". A constant reference cannot be
        # judged at all.
        .verdict = function(metrics, cv_threshold, correlation_threshold) {
            if (isTRUE(metrics$reference_constant)) return("insufficient")
            if (isTRUE(metrics$bias_material)) return("bias")
            corr <- if (!is.null(metrics$verdict_corr)) metrics$verdict_corr else metrics$overall_corr
            cv <- metrics$within_cv
            if (is.null(corr) || is.null(cv) || is.na(corr) || is.na(cv)) return("insufficient")
            if (corr >= correlation_threshold && cv <= cv_threshold) {
                confirmed <- isTRUE(metrics$bias_equivalent) && length(metrics$unassessed) == 0
                return(if (confirmed) "met" else "met_uncertain")
            }
            if (corr >= correlation_threshold - 0.2 && cv <= cv_threshold * 1.5) return("moderate")
            "inadequate"
        },

        # One sentence per MATERIAL bias row (its Bonferroni-adjusted CI lies beyond
        # the margin), each a whole translatable template: only numbers and the
        # variable name are inserted.
        .biasSentence = function(r) {
            margin <- self$options$bias_margin
            if (is.null(r$comparator)) r$comparator <- "reference"
            md <- sprintf("%.2f", r$mean_diff)
            rel <- sprintf("%.1f", r$rel)
            lo <- sprintf("%.2f", r$ci[1])
            hi <- sprintf("%.2f", r$ci[2])
            if (isTRUE(r$constant)) {
                if (r$pooled)
                    return(sprintf(.("The mean of all regions differs from the reference by exactly %s in every case (%s%% of the reference mean)."), md, rel))
                if (identical(r$comparator, "reference"))
                    return(sprintf(.("Region '%s' differs from the reference by exactly %s in every case (%s%% of the reference mean)."), r$name, md, rel))
                if (identical(r$comparator, "others"))
                    return(sprintf(.("Region '%s' differs from the mean of the other regions by exactly %s in every case (%s%% of their mean)."), r$name, md, rel))
                return(sprintf(.("Region '%s' differs from region '%s' by exactly %s in every case (%s%% of its mean)."), r$name, r$comparator, md, rel))
            }
            if (r$pooled)
                sprintf(.("The mean of all regions differs from the reference by %s on average (%s%% of the reference mean, 95%% CI %s to %s); the difference is shown to exceed the %s%% margin."), md, rel, lo, hi, margin)
            else if (identical(r$comparator, "reference"))
                sprintf(.("Region '%s' differs from the reference by %s on average (%s%% of the reference mean, 95%% CI %s to %s); the difference is shown to exceed the %s%% margin."), r$name, md, rel, lo, hi, margin)
            else if (identical(r$comparator, "others"))
                sprintf(.("Region '%s' differs from the mean of the other regions by %s on average (%s%% of their mean, 95%% CI %s to %s); the difference is shown to exceed the %s%% margin."), r$name, md, rel, lo, hi, margin)
            else
                sprintf(.("Region '%s' differs from region '%s' by %s on average (%s%% of its mean, 95%% CI %s to %s); the difference is shown to exceed the %s%% margin."), r$name, r$comparator, md, rel, lo, hi, margin)
        },

        # Largest point estimate and widest CI bound, relative to the reference
        # mean, over the bias rows (NA when none is assessable). A difference that
        # changes with the level counts with its two ends.
        .biasReach = function(rows) {
            ends <- function(r, x) if (isTRUE(r$prop_shown)) x / abs(r$ref_mean) * 100
            rel <- vapply(rows, function(r) max(abs(c(r$rel, ends(r, r$prop$fit)))), numeric(1))
            reach <- vapply(rows, function(r) max(abs(c(r$rel, r$rel_ci, ends(r, r$end_ci90)))), numeric(1))
            list(rel = if (any(is.finite(rel))) max(rel[is.finite(rel)]) else NA_real_,
                 reach = if (any(is.finite(reach))) max(reach[is.finite(reach)]) else NA_real_)
        },

        # Sentence for bias rows none of which is material: equivalence shown, exact
        # agreement, or a difference that cannot be excluded.
        .noMaterialBiasSentence = function(rows) {
            margin <- self$options$bias_margin
            rr <- private$.biasReach(rows)
            if (is.na(rr$rel))
                return(.("Systematic differences could not be expressed relative to the comparison because its mean is near zero; see the Sampling Bias Analysis table."))
            if (!is.na(rr$reach) && rr$reach == 0)
                return(.("Every regional measurement equalled its comparison in every case."))
            if (all(vapply(rows, function(r) isTRUE(r$equivalent), logical(1))))
                return(paste(c(sprintf(.("No material systematic difference: every 90%% CI lies within the %s%% margin (largest estimated difference %s%%)."),
                                       margin, sprintf("%.1f", rr$rel)),
                               private$.levelCheckSentence(rows)), collapse = " "))
            private$.inconclusiveSentence(rows)
        },

        # The non-equivalent row with the largest estimated difference, with its
        # estimate and 90% CI (a 10.8% point estimate used to disappear behind
        # "No material systematic difference was shown").
        .inconclusiveSentence = function(rows) {
            margin <- self$options$bias_margin
            open_rows <- Filter(function(r) !isTRUE(r$equivalent) && !is.na(r$rel), rows)
            if (length(open_rows) == 0)
                return(.("Systematic differences could not be expressed relative to the comparison because its mean is near zero; see the Sampling Bias Analysis table."))
            reach <- function(x) max(abs(c(x$rel, if (isTRUE(x$prop_shown)) x$prop$fit / abs(x$ref_mean) * 100)))
            r <- open_rows[[which.max(vapply(open_rows, reach, numeric(1)))]]
            if (isTRUE(r$prop_shown)) {
                end <- private$.endValues(r)
                ci <- end$ci
                if (r$pooled)
                    return(sprintf(.("A systematic difference was neither ruled out nor shown: the difference between the mean of all regions and the reference changes with the level, an estimated %s at a level of %s (90%% CI %s to %s) and %s at %s (90%% CI %s to %s); agreement within the \u00b1%s margin needs both CIs inside it."),
                                   end$fit[1], end$at[1], ci[1, 1], ci[1, 2], end$fit[2], end$at[2], ci[2, 1], ci[2, 2], end$margin))
                return(sprintf(.("A systematic difference was neither ruled out nor shown: the difference between region '%s' and its comparison changes with the level, an estimated %s at a level of %s (90%% CI %s to %s) and %s at %s (90%% CI %s to %s); agreement within the \u00b1%s margin needs both CIs inside it."),
                               r$name, end$fit[1], end$at[1], ci[1, 1], ci[1, 2], end$fit[2], end$at[2], ci[2, 1], ci[2, 2], end$margin))
            }
            ci <- if (is.null(r$rel_ci90)) c(NA_real_, NA_real_) else r$rel_ci90
            rel <- sprintf("%.1f", r$rel)
            lo <- sprintf("%.1f", ci[1])
            hi <- sprintf("%.1f", ci[2])
            if (r$pooled)
                sprintf(.("A systematic difference was neither ruled out nor shown: the mean of all regions differs by an estimated %s%% of the reference mean (90%% CI %s%% to %s%%); agreement within the %s%% margin needs the whole 90%% CI inside it."), rel, lo, hi, margin)
            else
                sprintf(.("A systematic difference was neither ruled out nor shown: region '%s' differs by an estimated %s%% of the comparison mean (90%% CI %s%% to %s%%); agreement within the %s%% margin needs the whole 90%% CI inside it."), r$name, rel, lo, hi, margin)
        },

        .formatClinicalAssessment = function(metrics, cv_threshold, correlation_threshold) {
            verdict <- private$.verdict(metrics, cv_threshold, correlation_threshold)
            margin <- self$options$bias_margin
            rows <- if (is.null(metrics$bias_rows)) list() else metrics$bias_rows
            info <- private$.materialInfo(rows)
            f3 <- function(x) sprintf("%.3f", x)
            li <- function(label, text) paste0("<li><strong>", label, "</strong> ", text, "</li>")
            ref_corr <- if (is.null(metrics$ref_corr)) numeric(0) else metrics$ref_corr
            unassessed_text <- private$.unassessedText(metrics)

            correlation_item <- if (is.na(metrics$overall_corr)) {
                li(.("Representativeness:"), .("Correlation could not be estimated with the available data."))
            } else if (metrics$has_reference && length(ref_corr) > 1) {
                below <- ref_corr[ref_corr < correlation_threshold]
                with_ci <- function(nm) sprintf("'%s' (r = %s, 95%% CI %s to %s)", nm, f3(ref_corr[[nm]]),
                                               f3(metrics$ref_ci[[nm]][1]), f3(metrics$ref_ci[[nm]][2]))
                shown <- metrics$shown_below
                point_only <- setdiff(names(below), shown)
                li(.("Representativeness:"), htmltools::htmlEscape(paste(c(
                    sprintf(if (metrics$overall_corr >= correlation_threshold)
                                .("Mean Spearman correlation with the reference = %s across %d regions, which meets your %s threshold.")
                            else
                                .("Mean Spearman correlation with the reference = %s across %d regions, which is below your %s threshold."),
                            f3(metrics$overall_corr), length(ref_corr), correlation_threshold),
                    if (length(shown) > 0)
                        sprintf(.("Shown to be below the threshold on their own (upper 95%% limit below it): %s."),
                                paste(vapply(shown, with_ci, ""), collapse = ", ")),
                    if (length(point_only) > 0)
                        sprintf(.("Below the threshold as a point estimate only: %s."),
                                paste(vapply(point_only, with_ci, ""), collapse = ", ")),
                    unassessed_text), collapse = " ")))
            } else {
                meets <- metrics$overall_corr >= correlation_threshold
                # Without a reference the value is the mean over pairs of regions.
                text <- sprintf(if (metrics$has_reference) {
                                    if (meets) .("Spearman correlation = %s, which meets your %s threshold.")
                                    else .("Spearman correlation = %s, which is below your %s threshold.")
                                } else {
                                    if (meets) .("Mean Spearman correlation between regions = %s, which meets your %s threshold.")
                                    else .("Mean Spearman correlation between regions = %s, which is below your %s threshold.")
                                },
                                f3(metrics$overall_corr), correlation_threshold)
                ci <- metrics$overall_ci
                if (meets && length(ci) == 2 && !is.na(ci[1]) && ci[1] < correlation_threshold)
                    text <- paste(text, sprintf(.("Its 95%% CI extends down to %s, below the threshold."), f3(ci[1])))
                if (!is.null(unassessed_text)) text <- paste(text, htmltools::htmlEscape(unassessed_text))
                li(.("Representativeness:"), text)
            }

            variability_item <- if (!is.na(metrics$within_cv)) {
                # Same (thr/2, thr) bands as the reproducibility table.
                # No "the median is within your threshold" rider: the median per-case
                # CV sits below the root mean square by construction (about 0.67x
                # with 2 values per case), so it fired on homogeneous data.
                text <- sprintf(
                    if (metrics$within_cv <= cv_threshold / 2) .("Within-case CV = %s%% (Low variability)") else
                    if (metrics$within_cv <= cv_threshold) .("Within-case CV = %s%% (Moderate variability)") else
                    .("Within-case CV = %s%% (High variability)"),
                    sprintf("%.1f", metrics$within_cv))
                li(.("Sampling Variability:"), text)
            } else {
                li(.("Sampling Variability:"), .("Not available."))
            }

            bias_label <- if (metrics$has_reference) .("Sampling Bias:") else .("Differences Between Regions:")
            bias_item <- if (isTRUE(metrics$reference_constant)) {
                li(bias_label, .("Not assessable: the reference has the same value in every case."))
            } else if (length(rows) == 0) {
                li(bias_label, .("Not enough paired observations to test for systematic differences."))
            } else if (info$any) {
                li(bias_label, htmltools::htmlEscape(paste(vapply(info$rows, private$.biasSentence, ""), collapse = " ")))
            } else {
                li(bias_label, htmltools::htmlEscape(private$.noMaterialBiasSentence(rows)))
            }
            loa_sentence <- if (!isTRUE(metrics$reference_constant)) private$.loaSentence(rows) else NULL
            loa_item <- if (is.null(loa_sentence)) "" else li(.("Individual Agreement:"), htmltools::htmlEscape(loa_sentence))

            thresholds_met <- !is.null(metrics$verdict_corr) && !is.na(metrics$verdict_corr) && !is.na(metrics$within_cv) &&
                metrics$verdict_corr >= correlation_threshold && metrics$within_cv <= cv_threshold
            caveat <- .("These are summary statistics from this dataset alone; they are not an external validation and they do not describe agreement at the score thresholds used to classify cases.")
            not_confirmed <- c(
                if (!isTRUE(metrics$bias_equivalent)) {
                    if (length(rows) == 0) .("Systematic differences could not be assessed.")
                    else private$.inconclusiveSentence(rows)
                },
                unassessed_text)
            rr <- private$.biasReach(rows)
            status_text <- switch(verdict,
                bias = paste0(
                    "<p><strong>", .("NOT ADEQUATE FOR SUBSTITUTION:"), "</strong> ",
                    if (!metrics$has_reference)
                        .("A clinically material systematic difference between regions is present.")
                    else if (thresholds_met)
                        sprintf(.("The agreement thresholds (correlation \u2265 %s, CV \u2264 %s%%) are met, but a clinically material systematic difference from the reference is present."),
                                correlation_threshold, cv_threshold)
                    else
                        .("A clinically material systematic difference from the reference measurement is present."),
                    " ", htmltools::htmlEscape(private$.materialWhereSentence(info)),
                    if (thresholds_met) paste0(" ", .("Correlation is blind to a constant offset, and the CV mixes an offset with random variation, so neither shows that the measurements are interchangeable.")) else "",
                    " ", if (!metrics$has_reference) .("The regions cannot be used interchangeably without calibration.")
                         else if (info$opposite) .("The regions are offset in opposite directions, so each needs its own calibration.")
                         else .("Calibrate the affected measurement against the reference before using it in its place."),
                    "</p>"),
                insufficient = paste0(
                    "<p><strong>", .("INSUFFICIENT DATA:"), "</strong> ",
                    if (isTRUE(metrics$reference_constant))
                        htmltools::htmlEscape(sprintf(.("The reference variable '%s' has the same value in every case, so agreement with it cannot be judged. Check the data."),
                                                      metrics$reference_name))
                    else
                        .("Unable to evaluate sampling quality because the correlation or the variability could not be estimated; the notes under the Reproducibility Assessment table give the reason."),
                    "</p>"),
                met = paste0(
                    "<p><strong>", .("AGREEMENT THRESHOLDS MET:"), "</strong> ",
                    if (!metrics$has_reference)
                        sprintf(.("Regional measurements agree with one another in this dataset (correlation \u2265 %s, CV \u2264 %s%%), and the average difference of every comparison between regions was shown to lie within the %s%% margin (90%% CI)."), correlation_threshold, cv_threshold, margin)
                    else
                        sprintf(.("Regional measurements agree with the reference measurement in this dataset (correlation \u2265 %s, CV \u2264 %s%%), and the average difference of every comparison with the reference was shown to lie within the %s%% margin (90%% CI)."), correlation_threshold, cv_threshold, margin),
                    " ", htmltools::htmlEscape(paste(c(private$.levelCheckSentence(rows), caveat), collapse = " ")), "</p>"),
                met_uncertain = paste0(
                    "<p><strong>", .("AGREEMENT THRESHOLDS MET, NOT CONFIRMED:"), "</strong> ",
                    sprintf(.("Regional measurements meet the correlation and CV thresholds (correlation \u2265 %s, CV \u2264 %s%%), but agreement is not confirmed."), correlation_threshold, cv_threshold),
                    " ", htmltools::htmlEscape(paste(not_confirmed, collapse = " ")),
                    if (!isTRUE(metrics$bias_equivalent) && !is.na(rr$rel) && rr$rel <= margin)
                        paste0(" ", .("More cases would narrow the interval.")) else "",
                    " ", caveat, "</p>"),
                moderate = paste0(
                    "<p><strong>", .("MODERATE SAMPLING:"), "</strong> ",
                    sprintf(.("The thresholds are met only after relaxing them to correlation \u2265 %s and CV \u2264 %s%%."),
                            signif(correlation_threshold - 0.2, 6), signif(cv_threshold * 1.5, 6)),
                    " ", .("This relaxed band (correlation threshold minus 0.2, CV threshold times 1.5) is a heuristic of this analysis, not a published criterion."),
                    " ", .("Consider averaging more than one region per case or revising the sampling protocol."),
                    "</p>"),
                inadequate = paste0(
                    "<p><strong>", .("INADEQUATE SAMPLING:"), "</strong> ",
                    sprintf(.("Sampling does not meet quality thresholds (correlation \u2265 %s, CV \u2264 %s%%)."), correlation_threshold, cv_threshold),
                    " ", .("Review the sampling strategy; averaging several regions per case may be needed."),
                    "</p>")
            )

            # Severity is carried by the box border; the text inherits the theme
            # colour (orange text was 2.0:1 on the light theme, and the scope
            # caveat used to be coloured green like reassurance).
            border <- switch(verdict, bias = "#dc2626", inadequate = "#dc2626",
                             moderate = "#d97706", met_uncertain = "#d97706", met = "#16a34a", "#6b7280")
            paste0(
                "<h4>", .("Key Findings:"), "</h4>",
                "<ul>", correlation_item, bias_item, loa_item, variability_item, "</ul>",
                "<h4>", .("Clinical Assessment:"), "</h4>",
                "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 10px; border-left: 4px solid ",
                border, "; color: inherit;'>",
                status_text,
                "</div>"
            )
        },

        .generateRecommendations = function(metrics, cv_threshold) {
            verdict <- private$.verdict(metrics, self$options$cv_threshold, self$options$correlation_threshold)
            info <- private$.materialInfo(metrics$bias_rows)
            single <- metrics$single_cv
            f1 <- function(x) sprintf("%.1f", x)

            # The advice uses the error of ONE regional measurement. Averaging k
            # regions lowers it to about 1/sqrt(k); the old fixed "consider 2-3
            # additional samples" was not computed from anything, and the per-case
            # CV it quoted (reference and all regions together) understated a
            # single region's error.
            # Descriptive only: this single-region error is on a different scale from
            # the per-case CV the verdict grades (graded against the same threshold,
            # it told a study that met the CV threshold to take more regions).
            sampling_item <- if (verdict == "bias") {
                .("A systematic difference is present, and averaging more regions removes neither an offset nor a difference that changes with the level. Address the systematic difference first.")
            } else if (verdict == "insufficient" || is.null(single) || is.na(single)) {
                .("Insufficient data for a sampling recommendation.")
            } else {
                paste(c(
                    if (metrics$has_reference)
                        sprintf(.("A single region differs from the reference by about %s%% (root-mean-square relative difference, which includes any systematic offset). Averaging several regions per case reduces the random part of this difference by about the square root of their number."), f1(single))
                    else
                        sprintf(.("Single regions of a case differ by about %s%% (root-mean-square within-case CV). Averaging several regions per case reduces this by about the square root of their number."), f1(single)),
                    if (!is.na(metrics$within_cv) && metrics$within_cv > cv_threshold)
                        .("The within-case CV exceeds your threshold, so averaging more than one region per case is worth considering.")),
                    collapse = " ")
            }

            case_cv <- metrics$case_cv
            n_cv <- sum(!is.na(case_cv))
            n_high <- sum(case_cv > cv_threshold, na.rm = TRUE)

            paste0(
                "<h4>", .("Recommendations:"), "</h4>",
                "<ul>",
                "<li><strong>", .("Regions per case:"), "</strong> ", sampling_item, "</li>",
                # The same materiality rule as the verdict - it used to fire on
                # p < 0.05 alone, contradicting the verdict for a trivial but
                # significant offset in a large cohort.
                if (verdict == "bias") paste0(
                    "<li><strong>", .("Bias Correction:"), "</strong> ",
                    htmltools::htmlEscape(if (!metrics$has_reference)
                        sprintf(.("Calibrate region(s) %s against the other regions before using them interchangeably."), info$regions)
                    else if (nzchar(info$regions))
                        sprintf(.("Calibrate region(s) %s against the reference before substituting them."), info$regions)
                    else
                        .("Calibrate the mean of all regions against the reference before using it in its place.")),
                    if (info$opposite) paste0(" ", .("The regions are offset in opposite directions, so each needs its own calibration.")) else "",
                    "</li>") else "",
                if (n_cv > 0) paste0(
                    "<li><strong>", .("Quality Control:"), "</strong> ",
                    if (n_high > 0)
                        sprintf(.("Review the %d of %d cases whose CV exceeds your %s%% threshold."), n_high, n_cv, cv_threshold)
                    else
                        sprintf(.("No case has a CV above your %s%% threshold."), cv_threshold),
                    "</li>") else "",
                "<li><strong>", .("Validation:"), "</strong> ", .("Confirm findings in independent dataset"), "</li>",
                "</ul>",
                # This run's shares (four fixed bullets used to be printed whatever
                # the data, also when every variance row was "Not estimable").
                if (!is.null(private$.vc_shares) && !anyNA(private$.vc_shares)) paste0(
                    "<h4>", .("Statistical Interpretation:"), "</h4>",
                    "<ul><li>",
                    sprintf(.("Of the total variance, %1$s%% lies between cases, %2$s%% within cases (sampling and scoring error) and %3$s%% between measurement methods (see the Variance Component Analysis table)."),
                            sprintf("%.0f", private$.vc_shares[["case"]]), sprintf("%.0f", private$.vc_shares[["within"]]),
                            sprintf("%.0f", private$.vc_shares[["method"]])),
                    "</li></ul>") else ""
            )
        },

        .generateReportSentences = function(metrics, cv_threshold, correlation_threshold) {
            # Copy-ready sentences. Every sentence is one whole translatable
            # template; only numbers and variable names are inserted, never a
            # translated fragment. Works from a partial metrics list too.
            verdict <- private$.verdict(metrics, cv_threshold, correlation_threshold)
            margin <- self$options$bias_margin
            has_reference <- isTRUE(metrics$has_reference)
            rows <- if (is.null(metrics$bias_rows)) list() else metrics$bias_rows
            info <- private$.materialInfo(rows)
            is_icc <- identical(metrics$icc_method, "icc") && !is.null(metrics$icc) && !is.na(metrics$icc)
            ref_corr <- if (is.null(metrics$ref_corr)) numeric(0) else metrics$ref_corr
            f3 <- function(x) sprintf("%.3f", x)
            n <- as.integer(metrics$n_cases)
            k <- as.integer(metrics$n_biopsies)

            design_sentence <- if (has_reference && k == 1) {
                sprintf(.("IHC heterogeneity analysis was performed on %d cases, each with a reference and a regional measurement."), n)
            } else if (has_reference) {
                sprintf(.("IHC heterogeneity analysis was performed on %d cases, each with a reference measurement and at least one of %d regional measurements."), n, k)
            } else if (k == 2) {
                sprintf(.("IHC heterogeneity analysis was performed on %d cases, each with both regional measurements."), n)
            } else {
                sprintf(.("IHC heterogeneity analysis was performed on %d cases, each with at least two of %d regional measurements."), n, k)
            }
            methods <- c(
                design_sentence,
                if (is_icc)
                    .("Agreement was assessed with the intraclass correlation coefficient (ICC(2,1), absolute agreement, two-way random effects) and Spearman rank correlation.")
                else
                    .("Agreement was assessed using Spearman rank correlation; an intraclass correlation coefficient could not be estimated from these data."),
                # Only for tests that ran: not for a constant reference, not when no
                # pair could be formed, and the reference version only with one.
                if (isTRUE(metrics$reference_constant) || length(rows) == 0) NULL
                else if (has_reference)
                    sprintf(.("Systematic differences from the reference measurement were assessed for each region, and for the mean of all regions, with paired t-tests (mean difference with 95%% CI) and Bland-Altman 95%% limits of agreement. With a margin of %s%% of the reference mean, a difference was ruled out when its 90%% CI lay within the margin (two one-sided tests) and considered material when a CI adjusted for the number of comparisons (Bonferroni) lay entirely beyond it."), margin)
                else
                    sprintf(.("Systematic differences between regions were assessed with paired t-tests of each region against the other region(s) (mean difference with 95%% CI). With a margin of %s%% of the comparison mean, a difference was ruled out when its 90%% CI lay within the margin (two one-sided tests) and considered material when a CI adjusted for the number of comparisons (Bonferroni) lay entirely beyond it."), margin),
                if (isTRUE(metrics$reference_constant) || !any(vapply(rows, function(r) !is.null(r$prop), logical(1)))) NULL
                else if (has_reference)
                    .("Whether a difference changed with the level (proportional bias) was assessed by regressing it on the reference value, with heteroscedasticity-robust standard errors (Bland & Altman 1999); where the slope differed from zero (p below 0.05 divided by the number of comparisons), the difference was also judged at the 5th and 95th percentiles of the reference, and a difference that changed with the level was not considered ruled out.")
                else
                    .("Whether a difference changed with the level (proportional bias) was assessed by regressing it on the case mean of all regions, with heteroscedasticity-robust standard errors (Bland & Altman 1999); where the slope differed from zero (p below 0.05 divided by the number of comparisons), the difference was also judged at the 5th and 95th percentiles of that level, and a difference that changed with the level was not considered ruled out."),
                if (has_reference && length(ref_corr) > 0)
                    .("Confidence intervals for Spearman correlations used the Fisher z transformation with the variance of Bonett and Wright (2000)."),
                .("Sampling variability was quantified as the within-case coefficient of variation (CV), the root mean square of the per-case CVs; cases whose mean was near zero were left out."),
                sprintf(.("Quality thresholds were set at correlation \u2265%s and CV \u2264%s%%."), correlation_threshold, cv_threshold)
            )

            correlation_sentence <- if (is.null(metrics$overall_corr) || is.na(metrics$overall_corr)) {
                .("Correlation metrics were not estimable with the available data.")
            } else if (has_reference && length(ref_corr) > 1) {
                shown <- if (is.null(metrics$shown_below)) character(0) else metrics$shown_below
                paste(c(
                    sprintf(if (metrics$overall_corr >= correlation_threshold)
                                .("The mean Spearman correlation between regional and reference measurements was %s (range %s to %s across regions), which meets the %s threshold set for this analysis.")
                            else
                                .("The mean Spearman correlation between regional and reference measurements was %s (range %s to %s across regions), below the %s threshold set for this analysis."),
                            f3(metrics$overall_corr), f3(min(ref_corr)), f3(max(ref_corr)), correlation_threshold),
                    if (length(shown) > 0)
                        sprintf(.("The correlation of region(s) %s was shown to be below the threshold (upper 95%% confidence limit below it)."),
                                paste(sprintf("'%s'", shown), collapse = ", "))),
                    collapse = " ")
            } else if (has_reference) {
                r <- metrics$overall_corr
                meets <- r >= correlation_threshold
                ci <- metrics$overall_ci
                if (length(ci) == 2 && !any(is.na(ci))) {
                    if (meets && ci[1] < correlation_threshold)
                        sprintf(.("The Spearman correlation between the regional and reference measurements was %s (95%% CI %s to %s); the point estimate meets the %s threshold set for this analysis, but the lower confidence limit does not."),
                                f3(r), f3(ci[1]), f3(ci[2]), correlation_threshold)
                    else
                        sprintf(if (meets) .("The Spearman correlation between the regional and reference measurements was %s (95%% CI %s to %s), which meets the %s threshold set for this analysis.")
                                else .("The Spearman correlation between the regional and reference measurements was %s (95%% CI %s to %s), below the %s threshold set for this analysis."),
                                f3(r), f3(ci[1]), f3(ci[2]), correlation_threshold)
                } else {
                    sprintf(if (meets) .("The Spearman correlation between the regional and reference measurements was %s, which meets the %s threshold set for this analysis.")
                            else .("The Spearman correlation between the regional and reference measurements was %s, below the %s threshold set for this analysis."),
                            f3(r), correlation_threshold)
                }
            } else {
                sprintf(if (metrics$overall_corr >= correlation_threshold)
                            .("The mean Spearman correlation between regional measurements was %s, which meets the %s threshold set for this analysis.")
                        else
                            .("The mean Spearman correlation between regional measurements was %s, below the %s threshold set for this analysis."),
                        f3(metrics$overall_corr), correlation_threshold)
            }

            dropped <- if (is.null(metrics$icc_dropped)) character(0) else metrics$icc_dropped
            has_ci <- is_icc && !is.null(metrics$icc_lower) && !is.na(metrics$icc_lower) && !is.na(metrics$icc_upper)
            icc_sentence <- if (!is_icc) NULL else if (length(dropped) > 0 && has_ci) {
                sprintf(.("The ICC(2,1), computed without the regions measured in too few cases (%s), was %s (95%% CI %s to %s)."),
                        paste(dropped, collapse = ", "), f3(metrics$icc), f3(metrics$icc_lower), f3(metrics$icc_upper))
            } else if (length(dropped) > 0) {
                sprintf(.("The ICC(2,1), computed without the regions measured in too few cases (%s), was %s."),
                        paste(dropped, collapse = ", "), f3(metrics$icc))
            } else if (has_ci) {
                sprintf(.("The ICC(2,1) was %s (95%% CI %s to %s)."), f3(metrics$icc), f3(metrics$icc_lower), f3(metrics$icc_upper))
            } else {
                sprintf(.("The ICC(2,1) was %s."), f3(metrics$icc))
            }

            variability_sentence <- if (!is.null(metrics$within_cv) && !is.na(metrics$within_cv)) {
                sprintf(
                    if (metrics$within_cv <= cv_threshold / 2)
                        .("Sampling variability was low (within-case CV = %s%%).")
                    else if (metrics$within_cv <= cv_threshold)
                        .("Sampling variability was moderate (within-case CV = %s%%).")
                    else
                        .("Sampling variability was high (within-case CV = %s%%)."),
                    sprintf("%.1f", metrics$within_cv))
            } else {
                .("Sampling variability could not be estimated.")
            }

            bias_sentence <- if (isTRUE(metrics$reference_constant)) {
                .("Systematic differences were not assessed because the reference had the same value in every case.")
            } else if (length(rows) == 0) {
                .("Bias testing could not be performed because too few cases had paired measurements.")
            } else if (info$any) {
                paste(vapply(info$rows, private$.biasSentence, ""), collapse = " ")
            } else {
                private$.noMaterialBiasSentence(rows)
            }
            loa_sentence <- if (length(rows) > 0 && !isTRUE(metrics$reference_constant)) private$.loaSentence(rows) else NULL

            quality_sentence <- switch(verdict,
                bias = if (has_reference)
                        .("A material systematic difference from the reference measurement was present, so the affected regional measurement should not substitute for the reference without calibration, whatever the correlation and CV.")
                      else
                        .("A material systematic difference between regions was present, so the regions should not be used interchangeably without calibration, whatever the correlation and CV."),
                met = if (has_reference)
                        sprintf(.("The sampling approach met the predefined correlation and CV criteria, and the average difference of every comparison with the reference was shown to lie within the %s%% margin."), margin)
                      else
                        sprintf(.("The sampling approach met the predefined correlation and CV criteria, and the average difference of every comparison between regions was shown to lie within the %s%% margin."), margin),
                met_uncertain = .("The sampling approach met the predefined correlation and CV criteria, but agreement was not confirmed: a systematic difference was not ruled out, or a region could not be assessed."),
                moderate = sprintf(.("The sampling approach met the correlation and CV criteria only after relaxing them to correlation \u2265 %s and CV \u2264 %s%%."),
                                   signif(correlation_threshold - 0.2, 6), signif(cv_threshold * 1.5, 6)),
                inadequate = .("The sampling approach did not meet the predefined quality criteria."),
                .("Data were insufficient to evaluate overall sampling quality against predefined criteria."))

            clinical_sentence <- if (verdict == "bias" && !has_reference) {
                sprintf(.("In this dataset region(s) %s differ systematically from the other regions, so the regions should not be used interchangeably without calibration."), info$regions)
            } else if (verdict == "bias") {
                paste(c(
                    if (nzchar(info$regions))
                        sprintf(.("In this dataset region(s) %s are systematically offset from the reference, so they should be calibrated before being used in its place."), info$regions)
                    else
                        .("In this dataset the mean of all regions is systematically offset from the reference, although no single region is materially offset on its own."),
                    if (info$opposite) .("The regions are offset in opposite directions, so each needs its own calibration.")),
                    collapse = " ")
            } else if (verdict == "met_uncertain") {
                .("In this dataset the regional measurements met the correlation and variability thresholds, but agreement within the chosen margin was not demonstrated.")
            } else if (!is.null(metrics$within_cv) && !is.na(metrics$within_cv)) {
                if (metrics$within_cv <= cv_threshold / 2) {
                    .("In this dataset, measurement variability was well within the CV threshold set for this analysis.")
                } else if (metrics$within_cv <= cv_threshold) {
                    .("In this dataset, measurement variability was within, but close to, the CV threshold set for this analysis.")
                } else {
                    .("In this dataset, measurement variability exceeded the CV threshold set for this analysis.")
                }
            } else {
                .("In this dataset, the available data were insufficient to quantify measurement variability.")
            }

            paste0(
                "<h3>", .("Copy-Ready Report Sentences"), "</h3>",
                "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border: 1px solid #dee2e6; border-radius: 5px; color: inherit;'>",
                "<h4>", .("Methods Section:"), "</h4>",
                "<p style='font-family: monospace; background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 10px; border-left: 4px solid #007bff;'>",
                htmltools::htmlEscape(paste(methods, collapse = " ")),
                "</p>",
                "<h4>", .("Results Section:"), "</h4>",
                "<p style='font-family: monospace; background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 10px; border-left: 4px solid #28a745;'>",
                htmltools::htmlEscape(paste(c(correlation_sentence, icc_sentence, variability_sentence, bias_sentence, loa_sentence, quality_sentence), collapse = " ")),
                "</p>",
                "<h4>", .("Clinical Interpretation:"), "</h4>",
                "<p style='font-family: monospace; background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 10px; border-left: 4px solid #ffc107;'>",
                htmltools::htmlEscape(clinical_sentence),
                "</p>",
                "</div>",
                "<p><strong>", .("Usage:"), "</strong> ", .("Click and drag to select text, then copy (Ctrl+C/Cmd+C) for use in reports."), "</p>"
            )
        },

        .generateHeterogeneityInterpretation = function(whole_section, biopsy_data, study_design = "reference_based") {
            cv_threshold <- self$options$cv_threshold
            correlation_threshold <- self$options$correlation_threshold
            metrics <- private$.calculateInterpretationMetrics(whole_section, biopsy_data, private$.repro_stats, study_design)

            assessment <- private$.formatClinicalAssessment(metrics, cv_threshold, correlation_threshold)
            # The substitution verdict is the analysis's main clinical finding; readers
            # who look only at the notices must see it too.
            if (identical(private$.verdict(metrics, cv_threshold, correlation_threshold), "bias")) {
                info <- private$.materialInfo(metrics$bias_rows)
                margin <- self$options$bias_margin
                private$.addNotice("STRONG_WARNING", .("Material systematic difference"),
                    if (!metrics$has_reference)
                        sprintf(.("Region(s) %s differ systematically from the other regions by more than the %s%% margin, so the regions cannot be used interchangeably without calibration; see the Clinical Assessment."), info$regions, margin)
                    else if (nzchar(info$regions))
                        sprintf(.("Region(s) %s are offset from the reference by more than the %s%% margin and should not replace it without calibration; see the Clinical Assessment."), info$regions, margin)
                    else
                        sprintf(.("The mean of all regions is offset from the reference by more than the %s%% margin, although no single region is materially offset on its own; see the Clinical Assessment."), margin))
            }
            recommendations <- if (self$options$generate_recommendations) {
                private$.generateRecommendations(metrics, cv_threshold)
            } else ""
            report_sentences <- if (isTRUE(self$options$showReportSentences)) {
                private$.generateReportSentences(metrics, cv_threshold, correlation_threshold)
            } else ""

            # The Study Design line counts the cases actually analysed (it used to
            # count rows, and claimed k measurements for every case).
            design <- if (metrics$has_reference && metrics$n_biopsies == 1) {
                sprintf(.("%d cases with a reference measurement and one regional measurement."), as.integer(metrics$n_cases))
            } else if (metrics$has_reference) {
                sprintf(.("%d cases with a reference measurement and at least one of %d regional measurements."),
                        as.integer(metrics$n_cases), as.integer(metrics$n_biopsies))
            } else if (metrics$n_biopsies == 2) {
                sprintf(.("%d cases with both regional measurements."), as.integer(metrics$n_cases))
            } else {
                sprintf(.("%d cases with at least two of %d regional measurements."),
                        as.integer(metrics$n_cases), as.integer(metrics$n_biopsies))
            }
            if (metrics$n_complete < metrics$n_cases)
                design <- paste(design, sprintf(.("%d of these cases have every measurement."), as.integer(metrics$n_complete)))

            interpretation <- paste0(
                "<h3>", .("IHC Heterogeneity Analysis Report"), "</h3>",
                "<p><strong>", .("Study Design:"), "</strong> ", design, "</p>",
                assessment,
                recommendations
            )
            if (!is.null(private$.strategy_notes)) {
                interpretation <- paste0(interpretation,
                    "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 10px; border-left: 4px solid #6c757d; margin: 10px 0; color: inherit;'>",
                    private$.strategy_notes, "</div>")
            }

            self$results$interpretation$setContent(interpretation)
            self$results$report_sentences$setContent(report_sentences)
            self$results$assumptions$setContent(
                if (isTRUE(self$options$showAssumptions)) private$.generateAssumptionsContent(metrics) else "")
            private$.generatePlainLanguageSummary(metrics)
        },

        # Methodology panel: what THIS run did, as whole translatable sentences.
        # It used to call every analysis a "computational simulation" following
        # Zilenaite-Petrulaitiene et al. (2025) - a method (Haralick entropy from
        # hexagonal subsampling of DIA output) that is not implemented here - and
        # to call the reference a "gold standard" even when none was supplied.
        .generateAssumptionsContent = function(metrics) {
            li <- function(label, text) paste0("<li><strong>", label, "</strong> ", text, "</li>")
            section <- function(title, tint, items) paste0(
                "<div style='margin: 15px 0;'><h4>", title, "</h4>",
                "<div style='background-color: ", tint, "; padding: 12px; border-radius: 5px; color: inherit;'><ul>",
                paste(items, collapse = ""), "</ul></div></div>")
            is_icc <- identical(metrics$icc_method, "icc")

            methods <- c(
                li(.("Correlation:"), if (metrics$has_reference)
                    .("Spearman rank correlation between each region and the reference; each region is graded on its own and the mean is reported. CIs use the Fisher z transformation with the variance of Bonett & Wright (2000).")
                  else .("Spearman rank correlation between each pair of regions, averaged over pairs.")),
                li(.("Reliability:"), if (is_icc)
                    .("Intraclass correlation ICC(2,1), absolute agreement, two-way random effects, with the F-based 95% CI of McGraw & Wong (1996); the consistency form ICC(3,1) is shown beside it.")
                  else .("An intraclass correlation could not be estimated from these data.")),
                li(.("Variability:"), .("Coefficient of variation per case (SD / mean of the case's measurements), combined over cases as a root mean square (the within-case CV); cases whose mean is near zero are left out.")),
                li(.("Systematic difference:"), sprintf(if (metrics$has_reference)
                    .("Per-region paired t-test of region minus reference (and of the mean of all regions), with the 95%% CI of the mean difference and Bland-Altman 95%% limits of agreement. With a margin of %s%% of the reference mean, a difference is ruled out when its 90%% CI lies within the margin (two one-sided tests) and material when a Bonferroni-adjusted CI lies entirely beyond it; otherwise it is inconclusive.")
                  else
                    .("Paired t-test of each region against the other region(s), with the 95%% CI of the mean difference and Bland-Altman 95%% limits of agreement. With a margin of %s%% of the comparison mean, a difference is ruled out when its 90%% CI lies within the margin (two one-sided tests) and material when a Bonferroni-adjusted CI lies entirely beyond it; otherwise it is inconclusive."),
                    self$options$bias_margin)),
                if (!isTRUE(metrics$reference_constant) &&
                    any(vapply(metrics$bias_rows, function(r) !is.null(r$prop), logical(1))))
                    li(.("Proportional bias:"), private$.levelMethodText(metrics$has_reference)),
                if (metrics$has_reference)
                    li(.("Reference measurement:"), .("The reference (whole section, hotspot or overall score) is treated as the comparison standard; its own measurement error is not modelled.")),
                if (isTRUE(private$.vc_done))
                    li(.("Variance components:"), .("Two-way random-effects decomposition into case, method and residual variance on cases with every measurement."))
            )
            requirements <- c(
                li(.("Sample size:"), sprintf(.("At least 5 cases are required; this run analysed %d cases."), as.integer(metrics$n_cases))),
                li(.("Measurement scale:"), .("Continuous biomarker values (percentages, scores or quantitative units).")),
                li(.("Independence:"), .("Regional measurements of a case are treated as exchangeable, independently sampled measurements of that case.")),
                li(.("Normality:"), .("The t-test CIs and the limits of agreement assume roughly normal differences.")),
                li(.("Monotonicity:"), .("Spearman correlation measures monotonic association and is blind to systematic offsets."))
            )
            limitations <- c(
                li(.("Biomarker specificity:"), .("Findings may not generalise to other biomarkers or tissue types.")),
                li(.("Pre-analytical factors:"), .("Fixation, processing and staining variation are not separated from sampling variation.")),
                li(.("Observers:"), .("Inter- and intra-observer variation is not separated from regional variation.")),
                li(.("Scope:"), sprintf(.("The quality thresholds (correlation %s, CV %s%%) are set in the analysis options; results describe the cases analysed and are not an external validation."),
                                        self$options$correlation_threshold, self$options$cv_threshold))
            )
            references <- c(
                "<li>Koo TK, Li MY. J Chiropr Med 2016;15(2):155-163.</li>",
                "<li>McGraw KO, Wong SP. Psychol Methods 1996;1(1):30-46.</li>",
                "<li>Bland JM, Altman DG. Lancet 1986;1(8476):307-310.</li>",
                "<li>Bland JM, Altman DG. Stat Methods Med Res 1999;8(2):135-160.</li>",
                if (metrics$has_reference) "<li>Bonett DG, Wright TA. Psychometrika 2000;65(1):23-28.</li>",
                "<li>Schuirmann DJ. J Pharmacokinet Biopharm 1987;15(6):657-680.</li>",
                if (any(vapply(metrics$bias_rows, function(r) !is.null(r$prop), logical(1))))
                    "<li>MacKinnon JG, White H. J Econometrics 1985;29(3):305-325.</li>",
                if (isTRUE(private$.ss_done)) "<li>Bonett DG. Stat Med 2002;21(9):1331-1335.</li>"
            )
            paste0(
                "<h3>", .("Methodology & Assumptions"), "</h3>",
                section(.("Methods used in this run"), "rgba(33, 152, 239, 0.13)", methods),
                section(.("Data requirements and assumptions"), "rgba(255, 169, 33, 0.14)", requirements),
                section(.("Limitations"), "rgba(255, 33, 67, 0.09)", limitations),
                section(.("References"), "rgba(33, 159, 33, 0.1)", references)
            )
        },

        .populateGlossary = function() {
            entry <- function(term, text) paste0("<li><strong>", term, "</strong> ", text, "</li>")
            block <- function(title, border, items) paste0(
                "<div style='margin: 15px 0; padding: 15px; background-color: rgba(138, 155, 172, 0.06); border-left: 4px solid ",
                border, "; border-radius: 4px; color: inherit;'>",
                "<h4 style='color: inherit; margin-top: 0;'>", title, "</h4>",
                "<ul style='margin: 10px 0; padding-left: 20px;'>", paste(items, collapse = ""), "</ul></div>")
            glossary_content <- paste0(
                "<div style='max-width: 800px; margin: 0 auto; font-family: Arial, sans-serif;'>",
                "<h3 style='color: inherit; border-bottom: 2px solid #4a90e2; padding-bottom: 8px;'>", .("Statistical Terms Glossary"), "</h3>",
                block(.("Correlation"), "#4a90e2", c(
                    entry(.("Spearman correlation:"), .("Rank-order agreement between two sets of measurements, from -1 to +1. It is not affected by outliers or skewed distributions, and it cannot see a constant offset. This analysis compares it with the correlation threshold you set.")))),
                block(.("Reliability"), "#ff8c42", c(
                    entry(.("ICC(2,1), absolute agreement:"), .("The share of the total variation in scores that comes from genuine differences between cases; a systematic offset between measurements counts against it. Bands (Koo & Li 2016): above 0.90 excellent, 0.75 to 0.90 good, 0.50 to 0.75 moderate, below 0.50 poor.")),
                    entry(.("ICC(3,1), consistency:"), .("The same ratio with systematic offsets removed; it is shown beside the absolute-agreement form so an offset can be seen.")))),
                block(.("Variability"), "#48bb78", c(
                    entry(.("CV (coefficient of variation):"), .("SD divided by the mean, times 100, for one case's measurements. This analysis grades it against the CV threshold you set: low at or below half the threshold, moderate within it, high above it. The within-case CV combines the cases as a root mean square. It is unstable when the mean is near zero, so such cases are left out.")),
                    entry(.("Proportional bias:"), .("A difference between two measurements that changes with the level of the marker - for example a biopsy that reads higher than the whole section in low cases and lower in high cases. An average difference can hide it, and a fixed correction cannot remove it.")),
                    entry(.("Limits of agreement:"), .("Mean difference plus or minus 1.96 SD of the differences: the range expected to hold 95% of individual region-minus-reference differences.")),
                    entry(.("Variance components:"), .("The total variance split into between-case, between-method and residual (within-case) parts.")))),
                block(.("Verdicts"), "#6b7280", c(
                    entry(.("Agreement thresholds met:"), .("The correlation and CV thresholds you set are met and the average difference of every comparison was shown to lie within your margin (90% CI inside it); a difference that changes with the level must also lie within it at both ends of the range. If the thresholds are met but a difference was not ruled out, the verdict is 'met, not confirmed'.")),
                    entry(.("Moderate sampling:"), .("The thresholds are met only after relaxing them to the correlation threshold minus 0.2 and 1.5 times the CV threshold. This band is a heuristic of this analysis, not a published criterion.")),
                    entry(.("Not adequate for substitution:"), .("A systematic difference was shown to exceed your margin (Bonferroni-adjusted CI entirely beyond it); correlation and CV cannot rescue it.")))),
                block(.("IHC terms"), "#805ad5", c(
                    entry(.("Spatial heterogeneity:"), .("Variation in biomarker expression across regions of the same tumour.")),
                    entry(.("H-score:"), .("(1 \u00d7 % weak) + (2 \u00d7 % moderate) + (3 \u00d7 % strong) staining; range 0 to 300.")),
                    entry(.("Proliferation index:"), .("Percentage of tumour cells staining positive (for example Ki67); range 0 to 100."))))
            , "</div>")
            self$results$glossary$setContent(glossary_content)
        },

        .generatePlainLanguageSummary = function(metrics) {
            if (!self$options$showSummary) return()

            cv_thr <- self$options$cv_threshold
            cor_thr <- self$options$correlation_threshold
            verdict <- private$.verdict(metrics, cv_thr, cor_thr)
            info <- private$.materialInfo(metrics$bias_rows)
            icc_value <- metrics$icc
            is_icc <- identical(metrics$icc_method, "icc") && !is.na(icc_value)
            within_cv <- metrics$within_cv

            # The summary opens with the SAME verdict as the Clinical Assessment. It
            # used to grade only the ICC, so "regional measurements agree closely"
            # and "the measurements gave similar values" sat under MODERATE,
            # INADEQUATE and even INSUFFICIENT DATA verdicts.
            headline <- switch(verdict,
                met = .("Overall: the regional measurements met your agreement thresholds in this dataset."),
                met_uncertain = .("Overall: the regional measurements met your correlation and variability thresholds, but agreement was not confirmed (see the Clinical Assessment)."),
                moderate = .("Overall: the regional measurements met your thresholds only after relaxing them, so a single region may not represent the case well."),
                inadequate = .("Overall: the regional measurements did not meet your agreement thresholds."),
                bias = if (!metrics$has_reference)
                        sprintf(.("Overall: region(s) %s differ systematically from the other regions, so the regions cannot be used interchangeably without calibration."), info$regions)
                    else if (nzchar(info$regions))
                        sprintf(.("Overall: region(s) %s differ systematically from the reference and cannot replace it without calibration."), info$regions)
                    else
                        .("Overall: the mean of all regions is systematically offset from the reference, so the regions cannot replace it without calibration."),
                .("Overall: the data were not sufficient to judge agreement."))

            # One whole sentence per band (no translated label spliced in), and
            # wording about where the variation comes from rather than a verdict.
            agreement_sentence <- if (is_icc && !(verdict %in% c("met", "met_uncertain"))) {
                # No claim about where the variation comes from beside a failing
                # verdict ("almost all ... not from which measurement was used" sat
                # under a bias headline).
                template <- switch(private$.iccBand(icc_value),
                    excellent = .("Excellent reliability band (ICC = %.2f)."),
                    good = .("Good reliability band (ICC = %.2f)."),
                    moderate = .("Moderate reliability band (ICC = %.2f)."),
                    poor = .("Poor reliability band (ICC = %.2f)."))
                paste0("<li><strong>", .("Agreement Level:"), "</strong> ", sprintf(template, icc_value), "</li>")
            } else if (is_icc) {
                template <- switch(private$.iccBand(icc_value),
                    excellent = .("Excellent (ICC = %.2f): almost all of the variation in scores comes from differences between cases, not from which measurement was used."),
                    good = .("Good (ICC = %.2f): most of the variation in scores comes from differences between cases rather than from which measurement was used."),
                    moderate = .("Moderate (ICC = %.2f): a substantial part of the variation in scores comes from which measurement was used."),
                    poor = .("Poor (ICC = %.2f): much of the variation in scores comes from which measurement was used rather than from differences between cases."))
                paste0("<li><strong>", .("Agreement Level:"), "</strong> ", sprintf(template, icc_value), "</li>")
            } else {
                paste0("<li><strong>", .("Agreement Level:"), "</strong> ",
                       .("Not available - an ICC could not be estimated with the provided data."), "</li>")
            }

            variability_sentence <- if (!is.na(within_cv)) {
                template <- if (within_cv <= cv_thr / 2) {
                    .("Low (CV = %.1f%%, graded against your %s%% threshold) - measurements of the same case were consistent.")
                } else if (within_cv <= cv_thr) {
                    .("Moderate (CV = %.1f%%, graded against your %s%% threshold) - measurements of the same case varied moderately.")
                } else {
                    .("High (CV = %.1f%%, graded against your %s%% threshold) - measurements of the same case varied substantially.")
                }
                paste0("<li><strong>", .("Variability:"), "</strong> ", sprintf(template, within_cv, private$.fmtNum(cv_thr)), "</li>")
            } else {
                paste0("<li><strong>", .("Variability:"), "</strong> ",
                       .("Not available - insufficient data to estimate variability."), "</li>")
            }

            shown <- if (is.null(metrics$shown_below)) character(0) else metrics$shown_below
            correlation_sentence <- if (length(shown) > 0) {
                # The statistic the verdict grades, not only the mean.
                weakest <- shown[which.min(metrics$ref_corr[shown])]
                paste0("<li><strong>", .("Correlation:"), "</strong> ", htmltools::htmlEscape(sprintf(
                    .("The weakest region, '%s', correlated %.2f with the reference, shown to be below your %.2f threshold; the average was %.2f."),
                    weakest, metrics$ref_corr[[weakest]], cor_thr, metrics$overall_corr)), "</li>")
            } else if (!is.na(metrics$overall_corr)) {
                paste0("<li><strong>", .("Correlation:"), "</strong> ", sprintf(
                    if (metrics$overall_corr >= cor_thr) .("Average correlation of %.2f, which meets your %.2f threshold.")
                    else .("Average correlation of %.2f, below your %.2f threshold."),
                    metrics$overall_corr, cor_thr), "</li>")
            } else {
                paste0("<li><strong>", .("Correlation:"), "</strong> ",
                       .("Not available - correlation metrics were not estimable."), "</li>")
            }

            icc_n <- if (!is.null(metrics$icc_n)) metrics$icc_n else metrics$n_cases
            interpretation <- if (is_icc) {
                sprintf(.("The ICC reported here is the absolute-agreement form: it is the share of the total variation in scores that comes from genuine differences between cases rather than from which region was measured, and a consistent offset between regions counts against it. It is not the proportion of cases whose scores matched, and it depends on how spread out your cohort is - the same measurement error yields a lower ICC when the cases have a narrow range of values. The 95%% CI columns of the Reproducibility Assessment table show how precisely these %d cases pin the figure down."),
                        as.integer(icc_n))
            } else if (!is.na(within_cv)) {
                if (within_cv <= cv_thr / 2) {
                    sprintf(.("Variability between measurements was low in this dataset (within-case CV at or below %s%%, half your configured threshold)."), private$.fmtNum(cv_thr / 2))
                } else if (within_cv <= cv_thr) {
                    sprintf(.("Variability between measurements was moderate in this dataset (within-case CV within your configured %s%% threshold)."), private$.fmtNum(cv_thr))
                } else {
                    sprintf(.("Variability between measurements was high in this dataset (within-case CV above your configured %s%% threshold)."), private$.fmtNum(cv_thr))
                }
            } else {
                .("Data were insufficient to characterize sampling reliability.")
            }

            opening <- if (metrics$n_biopsies == 1) {
                sprintf(.("We analysed %d cases, each measured in one region and by a reference measurement, to see how well the regional measurement represents the biomarker expression of a case."),
                        as.integer(metrics$n_cases))
            } else {
                sprintf(.("We analysed %d cases, each measured in up to %d regions, to see how well regional measurements represent the biomarker expression of a case."),
                        as.integer(metrics$n_cases), as.integer(metrics$n_biopsies))
            }

            summary_content <- paste0(
                "<div style='max-width: 700px; margin: 0 auto; padding: 20px; background-color: rgba(138, 155, 172, 0.06); border-radius: 8px; font-family: Arial, sans-serif; color: inherit;'>",
                "<h3 style='color: inherit; margin-bottom: 15px; text-align: center;'> ", .("Analysis Summary in Plain Language"), "</h3>",
                "<div style='background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 6px; margin: 15px 0; border-left: 4px solid #007bff;'>",
                "<p style='margin: 0; font-size: 16px; line-height: 1.6;'>", htmltools::htmlEscape(opening), "</p>",
                "<p style='margin: 8px 0 0 0; font-size: 16px; line-height: 1.6;'><strong>", htmltools::htmlEscape(headline), "</strong></p>",
                "</div>",
                "<div style='background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 6px; margin: 15px 0; border-left: 4px solid #28a745;'>",
                "<h4 style='color: inherit; margin-top: 0;'> ", .("Key Findings:"), "</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                agreement_sentence, variability_sentence, correlation_sentence,
                "</ul></div>",
                "<div style='background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 6px; margin: 15px 0; border-left: 4px solid #ffc107;'>",
                "<h4 style='color: inherit; margin-top: 0;'> ", .("Interpretation:"), "</h4>",
                "<p style='margin: 0; line-height: 1.6;'>", interpretation, "</p></div>",
                "<div style='text-align: center; margin-top: 15px; font-size: 14px; color: inherit;'>",
                "<p style='margin: 0;'>", .("This summary provides a simplified interpretation of the statistical results for clinical understanding."), "</p>",
                "</div></div>"
            )
            self$results$summary$setContent(summary_content)
        },

        .detectMisuse = function(whole_section, biopsy_data) {
            warnings <- character(0)
            has_reference <- !is.null(whole_section)

            # n = the cases actually analysed (usable cases, not rows)
            n_cases <- nrow(biopsy_data)
            if (n_cases < 10) {
                # Once, as a notice (it was repeated in the Data Quality box).
                private$.addNotice("STRONG_WARNING", .("Small sample"), sprintf(
                    .("Only %d cases are available. Reliability and variance estimates from fewer than 10 cases are imprecise; treat every coefficient below as provisional."), n_cases))
            }

            combined_data <- c(whole_section, as.matrix(biopsy_data))
            combined_data <- combined_data[!is.na(combined_data)]
            if (length(combined_data) > 0) {
                q1 <- stats::quantile(combined_data, 0.25)
                q3 <- stats::quantile(combined_data, 0.75)
                iqr <- q3 - q1
                outliers <- combined_data < (q1 - 1.5 * iqr) | combined_data > (q3 + 1.5 * iqr)
                if (sum(outliers) > length(combined_data) * 0.1) {
                    warnings <- c(warnings, sprintf(.("High number of outliers detected (%s%%). Consider checking for measurement errors or data entry issues."), round(sum(outliers)/length(combined_data)*100, 1)))
                }
            }

            cv_values <- private$.perCaseCV(whole_section, biopsy_data, has_reference, private$.cv_floor)
            if (any(!is.na(cv_values))) {
                high_cv_cases <- sum(cv_values > 50, na.rm = TRUE)
                if (high_cv_cases > sum(!is.na(cv_values)) * 0.2) {
                    warnings <- c(warnings, sprintf(.("Very high variability (CV > 50%%) detected in %d cases. This may indicate measurement inconsistencies or high biological heterogeneity."), as.integer(high_cv_cases)))
                }
            }

            # Constant values: the reference is checked too - a constant reference
            # used to leave "INSUFFICIENT DATA" with no actionable reason.
            is_constant <- private$.isConstant
            if (has_reference && is_constant(whole_section)) {
                private$.addNotice("WARNING", .("Constant reference measurement"), sprintf(
                    .("The reference variable '%s' is constant (the same value in every analysed case), so no correlation with it can be estimated. Check the data."),
                    self$options$wholesection))
            }
            constant_regions <- names(biopsy_data)[vapply(biopsy_data, is_constant, logical(1))]
            if (length(constant_regions) > 0) {
                warnings <- c(warnings, sprintf(.("Regional measurements with the same value in every case: %s. Check for data entry errors."),
                                                paste(constant_regions, collapse = ", ")))
            }

            missing_percent <- sum(is.na(biopsy_data)) / (nrow(biopsy_data) * ncol(biopsy_data)) * 100
            if (missing_percent > 20) {
                warnings <- c(warnings, sprintf(.("High percentage of missing regional measurements (%s%%). This may affect reliability of heterogeneity assessment."), round(missing_percent, 1)))
            }

            if (any(c(whole_section, as.matrix(biopsy_data)) < 0, na.rm = TRUE)) {
                warnings <- c(warnings, .("Negative values detected. Most IHC biomarkers should have non-negative values (e.g., Ki67 %, H-scores). Verify data coding."))
            }

            max_value <- max(c(whole_section, as.matrix(biopsy_data)), na.rm = TRUE)
            if (is.finite(max_value) && max_value > 300) {
                warnings <- c(warnings, sprintf(.("Very high biomarker values detected (max: %s). Verify if these are appropriate for your biomarker scale (e.g., percentages should be \u2264100%%, H-scores \u2264300)."), round(max_value, 1)))
            }

            warnings
        },

        .compareCompartments = function(whole_section, biopsy_data, spatial_regions) {
            # Clear first: jamovi re-runs .run() on data-cell edits WITHOUT
            # firing clearWith, so uncleared addRow() calls duplicate rows.
            comp_table <- self$results$compartmentComparison
            comp_table$deleteRows()

            groups <- private$.compartments(spatial_regions)
            private$.missingCompartmentNote(comp_table, groups)
            if (!is.null(groups$skipped)) {
                comp_table$setNote("skipped", sprintf(
                    .("Compartments with fewer than %d cases are not compared: %s."), groups$min_n, groups$skipped))
            }
            if (length(groups$keep) < 2) {
                comp_table$setNote("too_few", sprintf(
                    .("A comparison needs at least 2 compartments with %d or more cases each."), groups$min_n))
                return()
            }

            has_reference <- !is.null(whole_section)
            if (!has_reference) {
                comp_table$setNote("no_reference", .("No reference measurement was supplied: the ICC is the agreement between regions, and the bias rows are not shown."))
            }
            cv_thr <- self$options$cv_threshold
            min_pairs <- private$.CLINICAL_CONSTANTS$MIN_PAIRS_BIAS

            stats_by <- list()
            for (region in groups$keep) {
                mask <- !is.na(groups$labels) & groups$labels == region
                ws <- if (has_reference) whole_section[mask] else NULL
                bd <- biopsy_data[mask, , drop = FALSE]
                icc <- private$.calculateICC(ws, bd, min_n = groups$min_n)
                cvs <- private$.perCaseCV(ws, bd, has_reference, private$.cv_floor)
                bias <- NULL
                if (has_reference) {
                    region_mean <- rowMeans(as.matrix(bd), na.rm = TRUE)
                    ok <- !is.na(ws) & is.finite(region_mean)
                    if (sum(ok) >= min_pairs) bias <- private$.biasRow(region_mean[ok], ws[ok])
                }
                stats_by[[region]] <- list(
                    icc = if (is.null(icc$result)) NULL else icc$result$agreement,
                    icc_reason = icc$reason,
                    icc_dropped = icc$dropped,
                    cv = private$.rmsCV(cvs),
                    bias = bias)
            }
            # A compartment without an ICC, or whose ICC leaves out a region
            # measured in too few of its cases, is named (the main table does the
            # same); the row used to vanish, or be ranked against ICCs of more
            # measurements.
            no_icc <- Filter(function(g) is.null(stats_by[[g]]$icc), names(stats_by))
            if (length(no_icc) > 0)
                comp_table$setNote("icc_missing", private$.noteSafe(htmltools::htmlEscape(paste(vapply(no_icc, function(g)
                    paste(sprintf(.("No ICC for compartment '%s'."), g), stats_by[[g]]$icc_reason), ""), collapse = " "))))
            partial <- Filter(function(g) !is.null(stats_by[[g]]$icc) && length(stats_by[[g]]$icc_dropped) > 0, names(stats_by))
            if (length(partial) > 0)
                comp_table$setNote("icc_dropped", private$.noteSafe(htmltools::htmlEscape(sprintf(
                    .("Regions with fewer than %d values in a compartment are left out of that compartment's ICC, which then rests on fewer measurements than the others: %s."),
                    groups$min_n, paste(vapply(partial, function(g)
                        sprintf("%s: %s", g, paste(stats_by[[g]]$icc_dropped, collapse = ", ")), ""), collapse = "; ")))))

            band <- function(x) if (x <= cv_thr / 2) 1L else if (x <= cv_thr) 2L else 3L
            row_key <- 1
            for (region in names(stats_by)) {
                st <- stats_by[[region]]
                others <- stats_by[names(stats_by) != region]

                # ICC: "higher/lower" only when the 95% CIs do not overlap - the
                # point estimates of 7-8 cases per compartment used to be ranked.
                if (!is.null(st$icc)) {
                    a <- st$icc
                    comparison <- if (is.na(a$lower) || is.na(a$upper)) {
                        .("No CI available for comparison")
                    } else {
                        higher <- character(0)
                        lower <- character(0)
                        for (o in names(others)) {
                            oi <- others[[o]]$icc
                            if (is.null(oi) || is.na(oi$lower) || is.na(oi$upper)) next
                            if (a$lower > oi$upper) higher <- c(higher, o)
                            if (a$upper < oi$lower) lower <- c(lower, o)
                        }
                        if (length(higher) + length(lower) == 0) {
                            .("95% CI overlaps every other compartment: no clear difference")
                        } else {
                            paste(c(
                                if (length(higher)) sprintf(.("Higher than %s (95%% CIs do not overlap)"), paste(higher, collapse = ", ")),
                                if (length(lower)) sprintf(.("Lower than %s (95%% CIs do not overlap)"), paste(lower, collapse = ", "))),
                                collapse = "; ")
                        }
                    }
                    comp_table$addRow(rowKey = row_key, values = list(
                        metric = .("ICC(2,1) - absolute agreement"), compartment = region,
                        value = a$value, ci_lower = a$lower, ci_upper = a$upper,
                        comparison = comparison))
                    row_key <- row_key + 1
                } else {
                    comp_table$addRow(rowKey = row_key, values = list(
                        metric = .("ICC(2,1) - absolute agreement"), compartment = region,
                        value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
                        comparison = .("Not estimable - see note")))
                    row_key <- row_key + 1
                }

                # CV: compared by the user's CV bands with EACH other compartment,
                # not a fixed 5-point window (which called 7.7% vs 12.5% "Similar
                # variability") and not their pooled mean (which called two equally
                # homogeneous compartments each "lower" than the rest).
                if (!is.na(st$cv)) {
                    higher <- character(0)
                    lower <- character(0)
                    compared <- 0
                    for (o in names(others)) {
                        oc <- others[[o]]$cv
                        if (is.na(oc)) next
                        compared <- compared + 1
                        if (band(st$cv) > band(oc)) higher <- c(higher, o)
                        if (band(st$cv) < band(oc)) lower <- c(lower, o)
                    }
                    comparison <- if (compared == 0) {
                        .("No comparison available")
                    } else if (length(higher) + length(lower) == 0) {
                        .("Same CV band as every other compartment")
                    } else {
                        paste(c(
                            if (length(higher)) sprintf(.("Higher CV band than %s"), paste(higher, collapse = ", ")),
                            if (length(lower)) sprintf(.("Lower CV band than %s"), paste(lower, collapse = ", "))),
                            collapse = "; ")
                    }
                    comp_table$addRow(rowKey = row_key, values = list(
                        metric = .("Within-case CV (%)"), compartment = region,
                        value = st$cv, ci_lower = NA_real_, ci_upper = NA_real_,
                        comparison = comparison))
                    row_key <- row_key + 1
                }

                # Bias: sign and size relative to the reference mean. The label
                # used to come from the sign of (this bias - mean of the others)
                # with a 0.05 raw-unit window, so a smaller under-read was called
                # "Higher positive bias", and on an H-score scale every difference
                # was labelled.
                if (!is.null(st$bias)) {
                    b <- st$bias
                    other_rel <- unlist(lapply(others, function(x) if (is.null(x$bias)) NA_real_ else x$bias$rel))
                    other_rel <- other_rel[!is.na(other_rel)]
                    comparison <- if (is.na(b$rel)) {
                        .("Not assessable (reference mean near zero)")
                    } else if (length(other_rel) == 0) {
                        sprintf(.("%+.1f%% of the reference mean"), b$rel)
                    } else {
                        sprintf(.("%+.1f%% of the reference mean (other compartments: %+.1f%%)"), b$rel, mean(other_rel))
                    }
                    comp_table$addRow(rowKey = row_key, values = list(
                        metric = .("Mean Bias (mean of regions - reference)"), compartment = region,
                        value = b$mean_diff, ci_lower = b$ci[1], ci_upper = b$ci[2],
                        comparison = comparison))
                    row_key <- row_key + 1
                }
            }
            comp_table$setNote("cv_bands", .("CV bands follow your CV threshold (low up to half of it, moderate up to it, high above it). For a test of whether heterogeneity differs between compartments, use the Kruskal-Wallis test on per-case CV (Compartment comparison tests)."))
        },

        .performCompartmentTests = function(whole_section, biopsy_data, spatial_regions) {
            # Clear first: jamovi re-runs .run() on data-cell edits WITHOUT
            # firing clearWith, so uncleared addRow() calls duplicate rows.
            test_table <- self$results$compartmentTests
            test_table$deleteRows()

            groups <- private$.compartments(spatial_regions)
            private$.missingCompartmentNote(test_table, groups)
            if (!is.null(groups$skipped)) {
                test_table$setNote("skipped", sprintf(
                    .("Compartments with fewer than %d cases are not tested: %s."), groups$min_n, groups$skipped))
            }
            if (length(groups$keep) < 2) {
                test_table$setNote("too_few", sprintf(
                    .("Tests need at least 2 compartments with %d or more cases each."), groups$min_n))
                return()
            }

            has_reference <- !is.null(whole_section)
            cv_values <- numeric(0)
            cv_groups <- character(0)
            mean_values <- numeric(0)
            mean_groups <- character(0)
            n_no_cv <- 0
            for (region in groups$keep) {
                mask <- !is.na(groups$labels) & groups$labels == region
                ws <- if (has_reference) whole_section[mask] else NULL
                bd <- biopsy_data[mask, , drop = FALSE]
                cvs <- private$.perCaseCV(ws, bd, has_reference, private$.cv_floor)
                n_no_cv <- n_no_cv + sum(is.na(cvs))
                cv_values <- c(cv_values, cvs[!is.na(cvs)])
                cv_groups <- c(cv_groups, rep(region, sum(!is.na(cvs))))
                # ONE summary value per case for the level test.
                m <- if (has_reference) cbind(ws, as.matrix(bd)) else as.matrix(bd)
                case_means <- rowMeans(m, na.rm = TRUE)
                case_means <- case_means[is.finite(case_means)]
                mean_values <- c(mean_values, case_means)
                mean_groups <- c(mean_groups, rep(region, length(case_means)))
            }

            row_key <- 1
            add_failed <- function(label, e) {
                test_table$addRow(rowKey = row_key, values = list(
                    test_type = label, statistic = NA_real_, df1 = NA_integer_, df2 = NA_integer_, p_value = NA_real_,
                    interpretation = sprintf(.("Could not compute: %s"), conditionMessage(e))))
            }
            add_tied <- function(label, text) {
                test_table$addRow(rowKey = row_key, values = list(
                    test_type = label, statistic = NA_real_, df1 = NA_integer_, df2 = NA_integer_, p_value = NA_real_,
                    interpretation = text))
            }

            # 1. Is one compartment more heterogeneous than another? Kruskal-Wallis
            #    on per-case CV - the primary heterogeneity test. The two tests
            #    shown before compared the SPREAD of CVs and the mean LEVEL of the
            #    biomarker, neither of which answers that question.
            if (length(unique(cv_groups)) >= 2) {
                label <- .("Kruskal-Wallis test (per-case CV)")
                res <- tryCatch(stats::kruskal.test(cv_values, factor(cv_groups)), error = function(e) e)
                # All values tied (every case agrees exactly, or an all-zero marker):
                # kruskal.test returns NaN without an error, and the NaN used to
                # reach if() below and abort the whole analysis.
                if (inherits(res, "error")) add_failed(label, res) else if (!is.finite(res$p.value))
                    add_tied(label, .("All per-case CVs are identical, so no test is possible.")) else
                    test_table$addRow(rowKey = row_key, values = list(
                        test_type = label, statistic = unname(res$statistic),
                        df1 = as.integer(res$parameter), df2 = NA_integer_, p_value = res$p.value,
                        interpretation = if (res$p.value < 0.05)
                            .("Per-case CV differs between compartments: at least one compartment is more heterogeneous than another")
                        else
                            .("No difference in per-case CV between compartments was detected; this does not establish that they are equally heterogeneous")))
                row_key <- row_key + 1
            }

            # 2. Brown-Forsythe (median-centred Levene) on per-case CV: whether
            #    the SPREAD of the CVs differs. aov(), not oneway.test(): once
            #    formula.tools is loaded (via logistf) its as.character.formula
            #    method makes oneway.test reject every formula for the session.
            if (length(unique(cv_groups)) >= 2 && length(cv_values) > length(unique(cv_groups))) {
                label <- .("Brown-Forsythe test (spread of per-case CV)")
                res <- tryCatch({
                    med <- tapply(cv_values, cv_groups, stats::median)
                    abs_dev <- abs(cv_values - med[cv_groups])
                    # rounding residue (8e-15) is no spread
                    abs_dev[abs_dev <= 1e-8 * max(1, abs(cv_values))] <- 0
                    if (all(abs_dev == 0)) NULL else {
                        frame <- data.frame(abs_dev = abs_dev, grp = factor(cv_groups))
                        summary(stats::aov(abs_dev ~ grp, data = frame))[[1]]
                    }
                }, error = function(e) e)
                if (is.null(res) || (!inherits(res, "error") && !is.finite(res[["F value"]][1])))
                    add_tied(label, .("All per-case CVs are equally spread within compartments, so no test is possible.")) else
                if (inherits(res, "error")) add_failed(label, res) else {
                    p <- res[["Pr(>F)"]][1]
                    test_table$addRow(rowKey = row_key, values = list(
                        test_type = label, statistic = res[["F value"]][1],
                        df1 = as.integer(res[["Df"]][1]), df2 = as.integer(res[["Df"]][2]), p_value = p,
                        interpretation = if (is.finite(p) && p < 0.05)
                            .("The spread of per-case CVs differs between compartments (unequal variance of heterogeneity); this does not by itself mean one compartment is more heterogeneous on average")
                        else
                            .("No difference in the spread of per-case CVs between compartments was detected; this does not establish that they are the same")))
                }
                row_key <- row_key + 1
            }

            # 3. Biomarker LEVEL by compartment: Kruskal-Wallis on per-case means.
            if (length(unique(mean_groups)) >= 2) {
                label <- .("Kruskal-Wallis test (per-case means)")
                res <- tryCatch(stats::kruskal.test(mean_values, factor(mean_groups)), error = function(e) e)
                if (inherits(res, "error")) add_failed(label, res) else if (!is.finite(res$p.value))
                    add_tied(label, .("All per-case means are identical, so no test is possible.")) else
                    test_table$addRow(rowKey = row_key, values = list(
                        test_type = label, statistic = unname(res$statistic),
                        df1 = as.integer(res$parameter), df2 = NA_integer_, p_value = res$p.value,
                        interpretation = if (res$p.value < 0.05)
                            .("Significant difference in per-case mean biomarker levels across compartments")
                        else
                            .("No significant difference in per-case mean biomarker levels detected across compartments; this does not establish that the distributions are the same")))
                row_key <- row_key + 1
            }

            test_table$setNote("kw_unit", if (has_reference)
                .("Each case contributes one value per test - its CV, or the mean of its reference and regional measurements - so the observations are independent.")
              else
                .("Each case contributes one value per test - its CV, or the mean of its regional measurements - so the observations are independent."))
            if (n_no_cv > 0) {
                test_table$setNote("no_cv", sprintf(
                    .("%d case(s) with a mean near zero have no CV and are not in the CV tests."), n_no_cv))
            }
        }

    )
)
