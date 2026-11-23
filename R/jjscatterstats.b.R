#' @title Scatter Plot
#' @importFrom R6 R6Class
#' @importFrom jmvcore .
#'
#' @return An \code{R6} class generator object for the \code{jjscatterstatsClass} backend; used internally by the jamovi analysis wrapper and not called directly.

jjscatterstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjscatterstatsClass",
    inherit = jjscatterstatsBase,
    private = list(
        # Fixed seed for the sampling-based paths. Bayesian output uses
        # BayesFactor's MCMC and robust/effect-size CIs use bootstrapping, so
        # without this the SAME analysis reported different numbers on every
        # re-render - a credible interval that moves when nothing changed.


        # Pure option overrides for clinical presets (jamovi options are read-only at
        # runtime; computing overrides directly from self$options$clinicalPreset ensures
        # they persist across jamovi redraws / resize / .omv reopen when .run() is not
        # re-executed on a fresh R6 instance).
        .presetOverrides = function() {
            preset <- self$options$clinicalPreset
            if (is.null(preset) || identical(preset, "custom"))
                return(list())
            if (identical(preset, "biomarker_correlation")) {
                return(list(typestatistics = "nonparametric"))
            } else if (identical(preset, "treatment_response_analysis")) {
                return(list(typestatistics = "robust", marginal = TRUE))
            } else if (identical(preset, "publication_ready")) {
                return(list(originaltheme = TRUE, resultssubtitle = TRUE))
            }
            list()
        },
        .option = function(option) {
            ov <- private$.presetOverrides()
            if (option %in% names(ov)) return(ov[[option]])
            opt_obj <- self$options$option(option)
            if (!is.null(opt_obj)) return(opt_obj$value)
            return(NULL)
        },

        # init ----

        # The grouped plot2 canvas grows with the number of `grvar` levels. A
        # high-cardinality split variable would otherwise produce an enormous
        # (potentially unrenderable) canvas, so the effective level count used
        # for sizing is capped at a sane upper bound. Same concern as jjradarplot.
        .init = function() {

            pw_raw <- suppressWarnings(as.numeric(self$options$plotwidth))
            ph_raw <- suppressWarnings(as.numeric(self$options$plotheight))
            plotwidth  <- if (length(pw_raw) == 1L && is.finite(pw_raw)) max(300L, min(1200L, as.integer(round(pw_raw)))) else 600L
            plotheight <- if (length(ph_raw) == 1L && is.finite(ph_raw)) max(300L, min(800L,  as.integer(round(ph_raw)))) else 450L

            self$results$plot$setSize(plotwidth, plotheight)
            self$results$ggpubrPlot$setSize(plotwidth, plotheight)

            if (!is.null(self$options$grvar)) {
                mydata <- self$data
                grvar <-  self$options$grvar
                num_levels <- nlevels(droplevels(as.factor(mydata[[grvar]])))
                # Cap the sizing multiplier so many-level factors do not blow up
                # the canvas; the panels themselves still all render.
                sizing_levels <- max(1, min(num_levels, 16))
                self$results$plot2$setSize(sizing_levels * plotwidth, plotheight)
                self$results$ggpubrPlot2$setSize(sizing_levels * plotwidth, plotheight)
            }

            # Set size for plot3 (enhanced scatter)
            self$results$plot3$setSize(plotwidth, plotheight)

            # Control visibility of plot3 based on enhanced plot variables
            # This is also handled in .r.yaml visible expression
            # Keeping this here as backup and for R function usage
            hasEnhancedVars <- !is.null(self$options$colorvar) ||
                               !is.null(self$options$sizevar) ||
                               !is.null(self$options$shapevar) ||
                               !is.null(self$options$alphavar) ||
                               !is.null(self$options$labelvar)
            self$results$plot3$setVisible(hasEnhancedVars)
        },

        # run ----

        .run = function() {

            # Reset the (append-style) warnings output at the top of every run so
            # stale method-substitution / degenerate-data notices from a previous
            # run cycle do not linger when the current run no longer triggers them.
            if ("warnings" %in% self$results$itemNames) {
                self$results$warnings$setContent("")
                self$results$warnings$setVisible(FALSE)
            }

            # Initial Message ----
            if (is.null(self$options$dep) || is.null(self$options$group)) {

                # todo ----
                self$results$todo$setContent(private$.welcomeMessage())
                return()

            } else {

                # todo ----
                todo <- paste0(
                    "<br>", .("You have selected to use a scatter plot with correlation analysis."), "<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    jmvcore::reject(.('Data contains no (complete) rows'))
            }

            private$.applyClinicalPreset()
            private$.warnCorrMethodMismatch()
            private$.generateExplanations()

            # grouped_ggscatterstats() and ggpubrPlot2 have no p-adjustment argument:
            # each facet's correlation is tested at the nominal level and the raw p is
            # printed on that panel. Fire whenever grouped statistics are shown on
            # either plot2 (resultssubtitle) or ggpubrPlot2 (addGGPubrPlot & ggpubrAddCorr).
            show_grouped_stats <- isTRUE(private$.option("resultssubtitle")) ||
                (isTRUE(private$.option("addGGPubrPlot")) && isTRUE(self$options$ggpubrAddCorr))
            if (!is.null(self$options$grvar) && show_grouped_stats) {
                n_groups <- length(unique(stats::na.omit(self$data[[self$options$grvar]])))
                if (n_groups > 1)
                    private$.appendWarning(.fmt(.("<b>One test per group, unadjusted.</b> Each of the {groups} panels shows its own correlation tested at the nominal level, with no correction for the fact that {groups} tests are being read together. If you are screening groups rather than testing one pre-specified comparison, adjust the p-values yourself (Holm or FDR over the {groups} values) before drawing conclusions."),
                        groups = n_groups))
            }

            # Guard identical x and y variables before correlation calculation.
            same_var <- identical(self$options$dep, self$options$group)
            if (same_var) {
                private$.appendWarning(.fmt(.("<b>Same variable on both axes:</b> '{var}' is selected for both x-axis and y-axis. Correlating a variable with itself is trivially 1 and uninformative; choose two distinct variables."),
                    var = htmltools::htmlEscape(self$options$dep)))
            }

            # Degenerate-data check. The same test already existed in .plot3, but .plot3 only
            # renders when an aesthetic mapping is set (its visible: expression), so the
            # MAIN plot -- the default output everyone sees -- had no guard at all. Verified:
            # a constant dep variable rendered a scatter plot with no warning, while the same
            # data through .plot3 correctly reported "Correlation not computed". Running the
            # check in .run() means it fires whichever plots are on screen.
            checkData <- private$.prepData()
            private$.warnShapeLevels(checkData)
            private$.warnGGPubrPalette()
            x_vals <- checkData[[self$options$dep]]
            y_vals <- checkData[[self$options$group]]
            degenerate <- FALSE
            n_complete <- 0L
            if (!is.null(x_vals) && !is.null(y_vals)) {
                # is.finite(), not complete.cases(): complete.cases() counts an Inf as a
                # present value, so an all-Inf column looked like perfectly good data.
                complete <- is.finite(x_vals) & is.finite(y_vals)
                n_complete <- sum(complete)
                degenerate <- n_complete < 3 ||
                    length(unique(x_vals[complete])) < 2 ||
                    length(unique(y_vals[complete])) < 2
                if (degenerate) {
                    # Each branch is a complete translatable clause rather than a glued
                    # fragment, so a translator can reorder it inside the sentence below.
                    reason <- if (n_complete == 1)
                        .("there is only one complete pair of values")
                    else if (n_complete < 3)
                        .fmt(.("there are only {n} complete pairs of values"), n = n_complete)
                    else if (length(unique(x_vals[complete])) < 2)
                        .fmt(.("'{var}' takes the same value in every row"),
                             var = htmltools::htmlEscape(self$options$dep))
                    else
                        .fmt(.("'{var}' takes the same value in every row"),
                             var = htmltools::htmlEscape(self$options$group))
                    private$.appendWarning(.fmt(.("<b>Correlation not computed:</b> {reason}. A correlation needs at least 3 complete pairs and variation in both variables; the plot below shows the points but no coefficient is meaningful."),
                        reason = reason))
                }
            }

            # Per-subgroup degenerate and tiny-sample guard for grouped plots.
            has_subgroup_strong <- FALSE
            n_active_groups <- 0L
            if (!degenerate && !is.null(self$options$grvar) && !is.null(checkData[[self$options$grvar]])) {
                g_col <- checkData[[self$options$grvar]]
                g_non_na <- !is.na(g_col)
                g_lvls <- unique(as.character(g_col[g_non_na]))
                n_active_groups <- length(g_lvls)
                degen_grps <- character(0)
                tiny_grps <- character(0)
                for (lv in g_lvls) {
                    idx <- g_non_na & (as.character(g_col) == lv)
                    xg <- x_vals[idx]; yg <- y_vals[idx]
                    ok_g <- is.finite(xg) & is.finite(yg)
                    ng <- sum(ok_g)
                    if (ng < 3L || length(unique(xg[ok_g])) < 2L || length(unique(yg[ok_g])) < 2L) {
                        degen_grps <- c(degen_grps, paste0(htmltools::htmlEscape(lv), " (n = ", ng, ")"))
                    } else if (ng < 10L) {
                        tiny_grps <- c(tiny_grps, paste0(htmltools::htmlEscape(lv), " (n = ", ng, ")"))
                    }
                }
                if (length(degen_grps) > 0L) {
                    has_subgroup_strong <- TRUE
                    private$.appendWarning(.fmt(.("<b>Correlation not computed in subgroup(s):</b> {groups}. A correlation needs at least 3 complete pairs and variation in both variables within each group."),
                        groups = paste(degen_grps, collapse = ", ")))
                }
                if (length(tiny_grps) > 0L) {
                    has_subgroup_strong <- TRUE
                    private$.appendWarning(.fmt(.("<b>Very small sample in subgroup(s):</b> {groups}. Correlations from fewer than 10 pairs have very wide confidence intervals and should be treated as descriptive only."),
                        groups = paste(tiny_grps, collapse = ", ")))
                }
            }

            has_caveat_strong <- isTRUE(private$.warnStatisticalCaveats(checkData))

            # Native single-line jamovi Notice banners (STRONG_WARNING / WARNING at top,
            # INFO completion summary appended at bottom via $add()). Fatal input errors
            # remain exclusively jmvcore::reject() so each error is reported once.
            has_warnings <- isTRUE(self$results$warnings$visible)
            if (degenerate || same_var || has_subgroup_strong || has_caveat_strong) {
                private$.setNativeNotice(
                    name = "statisticalCaveat",
                    msg = .("Important data or statistical assumption cautions were detected for this correlation; review the Warnings section below before interpreting the coefficient."),
                    type = jmvcore::NoticeType$STRONG_WARNING,
                    position = 1L
                )
            } else if (has_warnings) {
                private$.setNativeNotice(
                    name = "methodologicalNote",
                    msg = .("Methodological notes apply to this scatter plot; review the Warnings section below for details."),
                    type = jmvcore::NoticeType$WARNING,
                    position = 1L
                )
            }

            if (isTRUE(self$options$showSummary) && (degenerate || same_var || n_complete < 3L)) {
                self$results$summary$setContent(paste0(
                    "<div style='background-color: rgba(155, 155, 155, 0.06); border-left: 4px solid rgba(128, 128, 128, 0.6); padding: 14px; margin: 8px 0; color: inherit;'>",
                    "<p style='margin: 0;'>", .("Summary unavailable: correlation could not be computed for the current selection."), "</p></div>"
                ))
            }

            if (!degenerate && !same_var && n_complete >= 3L) {
                private$.generateSummary(checkData)
                ty <- as.character(private$.option("typestatistics"))
                method_lbl <- switch(
                    ty,
                    parametric = .("Pearson correlation"),
                    nonparametric = .("Spearman correlation"),
                    robust = .("Winsorized Pearson correlation"),
                    bayes = .("Bayesian Pearson correlation"),
                    .("Correlation")
                )
                info_msg <- if (n_active_groups > 1L) {
                    .fmt(.("Completed {method} on {n} complete pairs across {groups} groups."),
                         method = method_lbl, n = n_complete, groups = n_active_groups)
                } else {
                    .fmt(.("Completed {method} on {n} complete pairs."),
                         method = method_lbl, n = n_complete)
                }
                private$.setNativeNotice(
                    name = "analysisComplete",
                    msg = info_msg,
                    type = jmvcore::NoticeType$INFO,
                    position = NULL
                )
            }
        },

        # Insert a single-line plain-text jmvcore::Notice at top (position = 1L) or
        # append at bottom (position = NULL via $add(), avoiding sentinel indices).
        .setNativeNotice = function(name, msg, type = jmvcore::NoticeType$INFO, position = NULL) {
            clean_msg <- trimws(gsub("[\r\n]+", " ", as.character(msg)))
            existing <- self$results$items[[name]]
            if (!is.null(existing) && inherits(existing, "Notice")) {
                existing$set(type, clean_msg)
                existing$setVisible(TRUE)
                return(invisible(NULL))
            }
            notice <- jmvcore::Notice$new(options = self$options, name = name, type = type)
            notice$setContent(clean_msg)
            if (is.null(position)) {
                self$results$add(notice)
            } else {
                self$results$insert(position, notice)
            }
            invisible(NULL)
        },

        # Statistical guardrails for the reported coefficient. Every one of these was
        # SILENT before (verified 2026-09-08 on purpose-built data), which matters because
        # the figure and its p-value are what a clinician quotes.
        #
        # All five run from .run(), never from a render callback: jamovi has already
        # composed and sent the results panel by the time an image is drawn.
        .warnStatisticalCaveats = function(d) {
            dep <- self$options$dep
            grp <- self$options$group
            if (is.null(dep) || is.null(grp)) return(invisible(NULL))
            x <- d[[dep]]
            y <- d[[grp]]
            if (is.null(x) || is.null(y) || !is.numeric(x) || !is.numeric(y))
                return(invisible(NULL))

            ok <- is.finite(x) & is.finite(y)
            n  <- sum(ok)
            x <- x[ok]
            y <- y[ok]
            # Below 3 pairs, or with a constant axis, .run() has already said something
            # strictly better; adding more here would just be noise.
            if (n < 3 || length(unique(x)) < 2 || length(unique(y)) < 2)
                return(FALSE)

            strong_triggered <- FALSE
            ty <- as.character(private$.option("typestatistics"))
            rank_based <- identical(ty, "nonparametric")

            # --- 1. Complete-case loss ---------------------------------------------
            # n is disclosed only inside the plot subtitle, which is OFF by default, so
            # a cohort could lose half its rows with nothing on screen saying so.
            n_total <- nrow(self$data)
            n_lost  <- n_total - n
            if (n_total > 0 && n_lost > 0 && (n_lost / n_total) >= 0.20) {
                private$.appendWarning(.fmt(.("<b>{pct}% of rows could not be used:</b> the correlation below is computed on {used} of {total} rows. The remaining {lost} have a missing or non-numeric value in '{xvar}' or '{yvar}'. Complete-case analysis assumes what is missing is unrelated to the relationship being measured; if the missingness is informative (sicker patients missing a lab value, say) this coefficient is biased."),
                    pct = round(100 * n_lost / n_total), used = n, total = n_total,
                    lost = n_lost, xvar = htmltools::htmlEscape(dep),
                    yvar = htmltools::htmlEscape(grp)))
            }

            # --- 2. Small sample ---------------------------------------------------
            # A correlation on a handful of points has a confidence interval so wide it
            # is compatible with almost any conclusion.
            if (n < 10) {
                strong_triggered <- TRUE
                private$.appendWarning(.fmt(.("<b>Very small sample (n = {n}):</b> a correlation estimated from fewer than 10 pairs is extremely unstable - its confidence interval typically spans most of the range from -1 to 1, so a large coefficient here is not evidence of a large association. Treat this as descriptive only."),
                    n = n))
            } else if (n < 30) {
                private$.appendWarning(.fmt(.("<b>Small sample (n = {n}):</b> the confidence interval on this coefficient is wide, and a single unusual observation can move the estimate substantially. Report the interval, not just the point estimate."),
                    n = n))
            }

            # --- 3. Influence of a single observation ------------------------------
            # Leave-one-out Pearson r from the running sums: subtracting one point's
            # contribution is O(n) in total, so this stays cheap on large data rather
            # than refitting n times. Only meaningful for the moment-based coefficients;
            # rank methods are resistant by construction, which is the point of offering
            # them, so the notice names that escape route. Winsorized Pearson counts as
            # resistant too -- telling a user who already picked "Robust" to consider
            # switching to Robust would be nonsense.
            resistant <- ty %in% c("nonparametric", "robust")
            if (n >= 10 && !resistant) {
                # Center both variables before running sums to prevent catastrophic
                # floating-point cancellation on large-offset variables (e.g. 1e8 + noise).
                xc <- x - mean(x)
                yc <- y - mean(y)
                Sx <- sum(xc)
                Sy <- sum(yc)
                Sxx <- sum(xc * xc)
                Syy <- sum(yc * yc)
                Sxy <- sum(xc * yc)
                m <- n - 1L
                sx <- Sx - xc
                sy <- Sy - yc
                sxx <- Sxx - xc * xc
                syy <- Syy - yc * yc
                sxy <- Sxy - xc * yc
                den <- sqrt(pmax(0, m * sxx - sx^2) * pmax(0, m * syy - sy^2))
                r_full <- stats::cor(x, y)
                # If dropping a single point collapses variance to zero in either variable,
                # that single observation created the entire correlation.
                zero_var_drop <- den <= (.Machine$double.eps * sqrt(max(1e-30, m * Sxx * m * Syy)))
                if (any(zero_var_drop)) {
                    strong_triggered <- TRUE
                    private$.appendWarning(.fmt(.("<b>One observation is driving this result:</b> dropping a single point removes all variation in one of the variables, making the correlation (currently {full}) undefined. A coefficient that depends this heavily on one patient should not be reported on its own - inspect that point on the plot, and consider the 'Robust (Winsorized Pearson)' or 'Nonparametric (Spearman)' test type, which are far less sensitive to it."),
                        full = sprintf("%.2f", r_full)))
                } else {
                    r_loo <- pmax(-1, pmin(1, (m * sxy - sx * sy) / den))
                    delta <- suppressWarnings(max(abs(r_full - r_loo), na.rm = TRUE))
                    if (is.finite(delta) && delta > 0.2) {
                        strong_triggered <- TRUE
                        worst <- which.max(abs(r_full - r_loo))
                        private$.appendWarning(.fmt(.("<b>One observation is driving this result:</b> dropping a single point changes the correlation from {full} to {dropped} (a shift of {delta}). A coefficient that depends this heavily on one patient should not be reported on its own - inspect that point on the plot, and consider the 'Robust (Winsorized Pearson)' or 'Nonparametric (Spearman)' test type, which are far less sensitive to it."),
                            full = sprintf("%.2f", r_full),
                            dropped = sprintf("%.2f", r_loo[worst]),
                            delta = sprintf("%.2f", delta)))
                    }
                }
            }

            # --- 4. Relationship is not the shape the test assumes ------------------
            # Compared on RANKS when Spearman is selected: a monotone curve is linear in
            # ranks and correctly stays quiet, while a non-monotone (U-shaped) one still
            # fires - which is right, because rho is near zero there too. A quadratic
            # term is the cheap, deterministic curvature probe; it will not detect every
            # exotic shape, so the message points the user back at the plot.
            if (n >= 20) {
                xx <- if (rank_based) rank(x) else x
                yy <- if (rank_based) rank(y) else y
                r2_lin <- stats::cor(xx, yy)^2
                # Fit the curve BOTH ways round and keep the stronger. Standardize `a`
                # before squaring so large-offset variables (e.g. 1e9 + x) do not make
                # `a` and `I(a^2)` numerically collinear.
                r2_of <- function(a, b) {
                    tryCatch({
                        sa <- stats::sd(a)
                        if (!is.finite(sa) || sa <= 0) return(NA_real_)
                        ac <- (a - mean(a)) / sa
                        summary(stats::lm(b ~ ac + I(ac^2)))$r.squared
                    }, error = function(e) NA_real_)
                }
                r2_quad <- suppressWarnings(max(r2_of(xx, yy), r2_of(yy, xx), na.rm = TRUE))
                if (is.finite(r2_quad) && (r2_quad - r2_lin) > 0.15) {
                    strong_triggered <- TRUE
                    # Two whole alternative sentences rather than a glued clause: the
                    # rank and moment cases differ in more than one word once translated.
                    private$.appendWarning(if (rank_based)
                        .fmt(.("<b>The relationship is not a straight line:</b> a curved fit explains {quad}% of the variation where a straight one explains {lin}%. Spearman's rho measures only the monotonic part, so it understates a real association here - a coefficient near zero does NOT mean the two variables are unrelated. Read the scatter plot itself before concluding anything."),
                             quad = round(100 * r2_quad), lin = round(100 * r2_lin))
                    else
                        .fmt(.("<b>The relationship is not a straight line:</b> a curved fit explains {quad}% of the variation where a straight one explains {lin}%. This correlation measures only the linear part, so it understates a real association here - a coefficient near zero does NOT mean the two variables are unrelated. Read the scatter plot itself before concluding anything."),
                             quad = round(100 * r2_quad), lin = round(100 * r2_lin)))
                }
            }

            # --- 5. Ties under Spearman --------------------------------------------
            # cor.test() emits "Cannot compute exact p-value with ties" as an R warning,
            # which jamovi never shows the user.
            if (rank_based && n >= 10) {
                d_x <- length(unique(x)) / n
                d_y <- length(unique(y)) / n
                if (min(d_x, d_y) < 0.5) {
                    tied_var <- if (d_x <= d_y) dep else grp
                    k_distinct <- if (d_x <= d_y) length(unique(x)) else length(unique(y))
                    private$.appendWarning(.fmt(.("<b>Heavy ties in '{var}':</b> only {levels} distinct values across {n} observations. Spearman's p-value then uses a Student-t approximation rather than an exact permutation test, and coarse discretization attenuates the correlation. For a variable with this few levels, a test designed for ordered categories is usually the better choice."),
                        var = htmltools::htmlEscape(tied_var),
                        levels = k_distinct, n = n))
                }
            }

            invisible(strong_triggered)
        },

        # decisionpanel-style intro shown until both axis variables are chosen. Every
        # panel is a translucent rgba() tint with `color: inherit` so it stays legible
        # in jamovi's dark theme; no hardcoded foreground colours (see the module-wide
        # theme-safety rule in vignettes/jamovi_library_review_guide.md).
        .welcomeMessage = function() {
            has_x <- !is.null(self$options$dep)
            has_y <- !is.null(self$options$group)
            tick <- function(ok) if (ok) "[x]" else "[ ]"

            paste0(
                "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>",

                "<div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid rgba(128, 128, 128, 0.5); padding: 20px; margin-bottom: 20px; color: inherit;'>",
                "<h2 style='margin: 0 0 10px 0; font-size: 20px;'>", .("Scatter Plot"), "</h2>",
                "<p style='margin: 0; font-size: 14px;'>",
                .("Correlation between two continuous variables, with optional grouping, marginal distributions and publication-ready output."),
                "</p></div>",

                "<div style='background-color: rgba(155, 155, 155, 0.06); border-left: 4px solid rgba(128, 128, 128, 0.6); padding: 15px; margin-bottom: 20px; color: inherit;'>",
                "<h3 style='margin: 0 0 10px 0; font-size: 16px;'>", .("Setup progress"), "</h3>",
                "<div style='margin-bottom: 6px;'>", tick(has_x), " ", .("x-axis (first variable)"), "</div>",
                "<div style='margin-bottom: 6px;'>", tick(has_y), " ", .("y-axis (second variable)"), "</div>",
                "<p style='margin: 10px 0 0 0;'>",
                if (has_x && has_y) .("Both variables selected - the analysis runs automatically.")
                else .("Select both variables to run the correlation."),
                "</p></div>",

                "<table style='width: 100%; border-collapse: collapse; margin-bottom: 20px;'>",
                "<tr><td style='width: 50%; border: 1px solid rgba(128, 128, 128, 0.4); padding: 15px; vertical-align: top;'>",
                "<h4 style='margin: 0 0 10px 0; font-size: 15px;'>", .("Quick start"), "</h4>",
                "<ol style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                "<li>", .("Drop a continuous variable into <strong>x-axis</strong> (e.g. biomarker level)."), "</li>",
                "<li>", .("Drop a second continuous variable into <strong>y-axis</strong>."), "</li>",
                "<li>", .("Optionally set <strong>Split By</strong> to get one panel per group."), "</li>",
                "<li>", .("Pick a <strong>Statistical Test Type</strong> that suits the distribution."), "</li>",
                "<li>", .("<strong>Statistical results</strong> are shown on the plot by default (untick to show points and trend line only)."), "</li>",
                "</ol></td>",
                "<td style='width: 50%; border: 1px solid rgba(128, 128, 128, 0.4); padding: 15px; vertical-align: top;'>",
                "<h4 style='margin: 0 0 10px 0; font-size: 15px;'>", .("What you will get"), "</h4>",
                "<ul style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                "<li>", .("<strong>Scatter plot</strong> with a fitted trend line and confidence band"), "</li>",
                "<li>", .("<strong>Correlation coefficient</strong> with p-value and confidence interval"), "</li>",
                "<li>", .("<strong>Per-group panels</strong> when a Split By variable is set"), "</li>",
                "<li>", .("<strong>Marginal distributions</strong> on both axes"), "</li>",
                "<li>", .("<strong>Publication-ready output</strong> in journal color palettes"), "</li>",
                "</ul></td></tr></table>",

                "<div style='background-color: rgba(155, 155, 155, 0.06); border: 1px solid rgba(128, 128, 128, 0.4); padding: 15px; color: inherit;'>",
                "<h4 style='margin: 0 0 10px 0; font-size: 15px;'>", .("Choosing a test"), "</h4>",
                "<ul style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                "<li>", .("<strong>Parametric (Pearson):</strong> linear relationship, roughly normal data."), "</li>",
                "<li>", .("<strong>Nonparametric (Spearman):</strong> any monotonic relationship; ordinal scores, skewed lab values."), "</li>",
                "<li>", .("<strong>Robust (Winsorized Pearson):</strong> pulls extreme values in to the 20th/80th percentiles before computing r (Wilcox df and bootstrap CI); use when outliers dominate."), "</li>",
                "<li>", .("<strong>Bayesian:</strong> reports a Bayes factor (log_e BF01) and highest-density credible interval quantifying evidence strength."), "</li>",
                "<li>", .("Correlation assumes independent, uncensored rows (one row per patient) and does not imply causation - use survival analysis for right-censored time-to-event data and repeated-measures methods for clustered rows."), "</li>",
                "</ul></div>",

                "<p style='margin-top: 15px; font-size: 13px;'>",
                .("Please cite jamovi and the packages listed in the references below."),
                "</p></div>"
            )
        },

        # Number of distinct non-missing levels of the shape variable, or 0 when none
        # is selected. ggplot2's discrete shape palette carries only 6 values; beyond
        # that it emits a console warning jamovi never shows and DROPS every point in
        # the surplus levels (measured: 7 levels drew 103 of 120 points, 12 levels drew
        # 60 of 120, while the correlation printed below used all 120).
        .shapeLevels = function(d) {
            v <- self$options$shapevar
            if (is.null(v) || identical(v, "") || is.null(d[[v]])) return(0L)
            length(unique(stats::na.omit(d[[v]])))
        },

        .warnShapeLevels = function(d) {
            n <- private$.shapeLevels(d)
            if (n <= 6) return(invisible(NULL))
            private$.appendWarning(.fmt(.("<b>Shape mapping skipped:</b> '{var}' has {levels} levels and ggplot2 provides only 6 distinct shapes. Mapping it would have drawn no point at all for the surplus levels while the correlation still used every case. Use color for a variable with this many levels, or group the rare categories."),
                var = htmltools::htmlEscape(self$options$shapevar), levels = n))
        },

        # A journal palette needs something to colour. The ungrouped ggpubr panel plots
        # one cloud, so there is no discrete scale: jco, npg and lancet rendered
        # byte-identical output. Say so rather than leave a control that does nothing.
        .warnGGPubrPalette = function() {
            if (!isTRUE(private$.option("addGGPubrPlot"))) return(invisible(NULL))
            if (!is.null(self$options$grvar)) return(invisible(NULL))
            if (identical(private$.option("ggpubrPalette"), "jco")) return(invisible(NULL))
            private$.appendWarning(.("<b>Color palette not applied:</b> the publication-ready panel draws a single ungrouped set of points, so there is no grouping for a journal palette to color. Set a 'Split By' variable to see the palette take effect."))
        },

        # Assemble the near-identical preset-notification HTML from a single
        # template. `title` and `items` are hardcoded literals (no user input),
        # so no HTML escaping is required.
        .presetMessage = function(title, items) {
            li_items <- paste0("<li>", items, "</li>", collapse = "")
            paste0(
                "<div style='background-color: rgba(33, 152, 239, 0.13); border-left:4px solid #2196F3; padding:15px; margin:10px 0; color: inherit;'>",
                # No explicit colour: #1976D2 is a mid blue that loses contrast against the dark
                # theme's panel. The 4px blue left border already carries the "info" cue.
                "<h4 style='margin-top:0;'>", .fmt(.("Clinical Preset Applied: {title}"), title = title), "</h4>",
                "<p><strong>", .("The following settings have been automatically configured:"), "</strong></p>",
                "<ul>", li_items, "</ul>",
                "<p style='margin-bottom:0;'><em>", .("You can modify these settings manually or select 'Custom' preset."), "</em></p>",
                "</div>"
            )
        },

        # Clinical presets are resolved lazily via private$.presetOverrides() so they
        # survive jamovi redraws without .run(). This method displays the banner.
        .applyClinicalPreset = function() {
            preset <- self$options$clinicalPreset
            if (is.null(preset) || identical(preset, "custom")) {
                self$results$presetInfo$setVisible(FALSE)
                return()
            }

            preset_message <- NULL

            if (preset == "biomarker_correlation") {
                preset_message <- private$.presetMessage(
                    .("Biomarker Correlation"),
                    c(
                        .("Statistical test: <strong>Nonparametric (Spearman correlation)</strong>")
                    )
                )
            } else if (preset == "treatment_response_analysis") {
                preset_message <- private$.presetMessage(
                    .("Treatment Response Analysis"),
                    c(
                        .("Statistical test: <strong>Robust (20% Winsorized Pearson correlation)</strong>"),
                        .("Marginal distributions: <strong>Enabled</strong>")
                    )
                )
            } else if (preset == "publication_ready") {
                preset_message <- private$.presetMessage(
                    .("Publication Ready"),
                    c(
                        .("Theme: <strong>Original ggstatsplot theme</strong>"),
                        .("Results subtitle: <strong>Enabled (shows statistics on plot)</strong>")
                    )
                )
            }

            if (!is.null(preset_message)) {
                self$results$presetInfo$setContent(preset_message)
                self$results$presetInfo$setVisible(TRUE)
            }
        },

        .generateExplanations = function() {
            if (self$options$showExplanations) {
                test_type <- private$.option("typestatistics")

                method_text <- switch(
                    test_type,
                    "parametric" = .("The Pearson correlation coefficient (r) measures the strength and direction of the <strong>linear</strong> relationship between the two variables, with a Student-t test (df = n - 2) and Fisher-z confidence interval."),
                    "nonparametric" = .("Spearman's rho measures the strength and direction of the <strong>monotonic (rank-based)</strong> relationship between the two variables without assuming linearity or normality, using an exact permutation p-value when untied (n <= 1290) and a Bonett-Wright confidence interval."),
                    "robust" = .("The robust (20% two-sided Winsorized Pearson) correlation coefficient measures the association between the two variables while <strong>down-weighting the influence of outliers</strong>, using Wilcox's adjusted degrees of freedom (df = n - 2g - 2) and a seeded percentile-bootstrap confidence interval."),
                    "bayes" = .("The Bayesian analysis uses a shifted Beta(1.41, 1.41) prior (rscale = 0.707) and reports log<sub>e</sub>(BF<sub>01</sub>) quantifying the <strong>strength of evidence</strong> alongside the posterior median correlation and highest-density credible interval at the selected confidence level."),
                    .("The correlation coefficient measures the strength and direction of the relationship between the two variables.")
                )

                self$results$explanations$setVisible(TRUE)
                self$results$explanations$setContent(
                    paste0(
                        "<h3>", .("Explanations"), "</h3><p>",
                        .fmt(.("This scatter plot shows the relationship between two continuous variables assuming independent, uncensored observations (one row per patient). {method} For right-censored survival times use survival analysis, and for repeated observations per patient use a repeated-measures model."),
                             method = method_text),
                        "</p>"
                    )
                )
            }
        },

        # Opt-in copy-ready natural-language summary computed only when showSummary
        # is enabled, reusing .computeCorrStats() and self$options$seed so numbers
        # match the plot subtitle bit-for-bit. Magnitude/direction interpretation
        # uses one complete .() sentence per band (no spliced adjectives) so
        # translations remain grammatical across languages.
        .summaryBandSentence = function(est) {
            if (!is.finite(est)) return("")
            mag <- abs(est)
            band_key <- if (mag < 0.10) {
                "negligible"
            } else if (est > 0) {
                if (mag < 0.30) "pos_weak"
                else if (mag < 0.50) "pos_moderate"
                else if (mag < 0.70) "pos_strong"
                else "pos_very_strong"
            } else {
                if (mag < 0.30) "neg_weak"
                else if (mag < 0.50) "neg_moderate"
                else if (mag < 0.70) "neg_strong"
                else "neg_very_strong"
            }
            switch(
                band_key,
                negligible      = .("The observed association is negligible in magnitude."),
                pos_weak        = .("This indicates a weak positive association."),
                pos_moderate    = .("This indicates a moderate positive association."),
                pos_strong      = .("This indicates a strong positive association."),
                pos_very_strong = .("This indicates a very strong positive association."),
                neg_weak        = .("This indicates a weak negative association."),
                neg_moderate    = .("This indicates a moderate negative association."),
                neg_strong      = .("This indicates a strong negative association."),
                neg_very_strong = .("This indicates a very strong negative association."),
                ""
            )
        },

        .formatSummarySentence = function(st, x_label, y_label, dp) {
            est_s <- private$.fmtStatVal(st$estimate, dp)
            ci_pct <- round(100 * st$conf_level, 1)
            has_ci <- is.finite(st$conf.low) && is.finite(st$conf.high)
            # Pass bracketed interval via {ci} placeholder: jmvcore::.() strips
            # literal " [...]" suffixes as gettext context tags.
            ci_str <- if (has_ci) {
                paste0("[", private$.fmtStatVal(st$conf.low, dp), ", ", private$.fmtStatVal(st$conf.high, dp), "]")
            } else {
                ""
            }
            band_sentence <- private$.summaryBandSentence(st$estimate)

            if (identical(st$type, "bayes")) {
                lbf_s <- private$.fmtStatVal(st$log_bf01, dp)
                stat_sentence <- if (has_ci) {
                    .fmt(
                        .("A {method} between '{xvar}' and '{yvar}' (n = {n}) yielded a posterior median r = {est} ({pct}% HDI {ci}, log_e BF01 = {lbf})."),
                        method = st$method, xvar = x_label, yvar = y_label, n = st$n,
                        est = est_s, pct = ci_pct, ci = ci_str, lbf = lbf_s
                    )
                } else {
                    .fmt(
                        .("A {method} between '{xvar}' and '{yvar}' (n = {n}) yielded a posterior median r = {est} (log_e BF01 = {lbf})."),
                        method = st$method, xvar = x_label, yvar = y_label, n = st$n,
                        est = est_s, lbf = lbf_s
                    )
                }
                return(if (nzchar(band_sentence)) paste(stat_sentence, band_sentence) else stat_sentence)
            }

            p_dp <- max(dp, 3L)
            p_cut <- 10^(-p_dp)
            # Use structural entity &lt; because summary is rendered inside HTML <p>.
            p_txt <- if (!is.finite(st$p.value)) {
                "p = NA"
            } else if (st$p.value < p_cut) {
                sprintf(paste0("p &lt; %.", p_dp, "f"), p_cut)
            } else {
                sprintf(paste0("p = %.", p_dp, "f"), st$p.value)
            }
            sym <- if (identical(st$type, "nonparametric")) "rho" else "r"
            stat_sentence <- if (has_ci) {
                .fmt(
                    .("A {method} between '{xvar}' and '{yvar}' (n = {n}) yielded {sym} = {est} ({pct}% CI {ci}, {ptxt})."),
                    method = st$method, xvar = x_label, yvar = y_label, n = st$n,
                    sym = sym, est = est_s, pct = ci_pct, ci = ci_str, ptxt = p_txt
                )
            } else {
                .fmt(
                    .("A {method} between '{xvar}' and '{yvar}' (n = {n}) yielded {sym} = {est} ({ptxt}; confidence interval not defined at the boundary)."),
                    method = st$method, xvar = x_label, yvar = y_label, n = st$n,
                    sym = sym, est = est_s, ptxt = p_txt
                )
            }
            if (nzchar(band_sentence)) paste(stat_sentence, band_sentence) else stat_sentence
        },

        .generateSummary = function(d) {
            if (!isTRUE(self$options$showSummary)) return(invisible(NULL))

            withr::local_seed(self$options$seed)
            dep <- self$options$dep
            grp <- self$options$group
            ty <- as.character(private$.option("typestatistics"))
            dp <- max(0L, min(5L, as.integer(self$options$k)))
            x_lbl <- htmltools::htmlEscape(dep)
            y_lbl <- htmltools::htmlEscape(grp)

            st_all <- private$.computeCorrStats(
                d[[dep]], d[[grp]],
                type = ty, conf_level = self$options$conflevel, digits = dp
            )
            if (!isTRUE(st_all$valid)) return(invisible(NULL))

            main_sentence <- private$.formatSummarySentence(st_all, x_lbl, y_lbl, dp)
            grp_html <- ""

            if (!is.null(self$options$grvar) && !is.null(d[[self$options$grvar]])) {
                g_col <- d[[self$options$grvar]]
                g_ok <- !is.na(g_col)
                g_lvls <- unique(as.character(g_col[g_ok]))
                if (length(g_lvls) > 1L) {
                    li_parts <- character(0)
                    for (lv in g_lvls) {
                        private$.checkpoint()
                        idx <- g_ok & (as.character(g_col) == lv)
                        st_g <- private$.computeCorrStats(
                            d[[dep]][idx], d[[grp]][idx],
                            type = ty, conf_level = self$options$conflevel, digits = dp
                        )
                        lv_esc <- htmltools::htmlEscape(lv)
                        if (isTRUE(st_g$valid)) {
                            li_parts <- c(li_parts, paste0(
                                "<li><strong>", lv_esc, ":</strong> ",
                                private$.formatSummarySentence(st_g, x_lbl, y_lbl, dp),
                                "</li>"
                            ))
                        } else {
                            li_parts <- c(li_parts, paste0(
                                "<li><strong>", lv_esc, ":</strong> ",
                                .("Correlation not computed (insufficient data)."),
                                "</li>"
                            ))
                        }
                    }
                    grp_html <- paste0(
                        "<p style='margin: 10px 0 6px 0;'><strong>",
                        .fmt(.("Subgroup breakdown by '{grvar}':"), grvar = htmltools::htmlEscape(self$options$grvar)),
                        "</strong></p><ul style='margin: 0; padding-left: 20px;'>",
                        paste0(li_parts, collapse = ""),
                        "</ul>"
                    )
                }
            }

            html <- paste0(
                "<div style='background-color: rgba(155, 155, 155, 0.06); border-left: 4px solid rgba(128, 128, 128, 0.6); padding: 14px; margin: 8px 0; color: inherit;'>",
                "<h4 style='margin: 0 0 8px 0;'>", .("Summary"), "</h4>",
                "<p style='margin: 0;'>", main_sentence, "</p>",
                grp_html,
                "</div>"
            )
            self$results$summary$setContent(html)
            self$results$summary$setVisible(TRUE)
            invisible(NULL)
        },


        # Resolve the plot title / axis labels from the user-supplied options,
        # falling back to sensible variable-name defaults. Shared by plot, plot2
        # and plot3 (set includeGrvar = TRUE for the grouped-plot default title).
        .resolveLabels = function(includeGrvar = FALSE) {
            if (!is.null(self$options$mytitle) && self$options$mytitle != "") {
                title <- self$options$mytitle
            } else if (includeGrvar) {
                title <- paste(self$options$dep, "vs", self$options$group, "by", self$options$grvar)
            } else {
                title <- paste(self$options$dep, "vs", self$options$group)
            }

            if (!is.null(self$options$xtitle) && self$options$xtitle != "") {
                xtitle <- self$options$xtitle
            } else {
                xtitle <- self$options$dep
            }

            if (!is.null(self$options$ytitle) && self$options$ytitle != "") {
                ytitle <- self$options$ytitle
            } else {
                ytitle <- self$options$group
            }

            list(title = title, xtitle = xtitle, ytitle = ytitle)
        },

        # Shared data preparation for EVERY render path and for the .run() guards.
        #
        # (1) jamovi's `permitted: [numeric]` also accepts ORDINAL columns, which arrive
        #     as factors carrying an integer `values` attribute; jmvcore::toNumeric is
        #     what turns them back into numbers. .plot/.plot2/.plot3 did this but the two
        #     ggpubr panels did not, so an ordinal x rendered with a discrete axis, a
        #     regression line drawn across categories and a correlation label of "NA".
        # (2) Inf/-Inf pass BOTH is.na() and stats::complete.cases(), so a single Inf
        #     poisoned every downstream statistic while every guard reported healthy data:
        #     the main panel printed r = NA, p = NA next to "n = 79", the enhanced panel
        #     printed the literal "r = NaN", and type = "bayes" hard-errored. Non-finite
        #     values are dropped here, once, and reported.
        .prepData = function() {
            d <- self$data
            dep <- self$options$dep
            group <- self$options$group
            if (is.null(dep) || is.null(group)) return(d)

            d[[dep]] <- jmvcore::toNumeric(d[[dep]])
            d[[group]] <- jmvcore::toNumeric(d[[group]])

            if (!is.numeric(d[[dep]]) || !is.numeric(d[[group]]))
                jmvcore::reject(.('Both scatterplot variables must be numeric.'))

            nonfinite <- (!is.na(d[[dep]]) & !is.finite(d[[dep]])) |
                         (!is.na(d[[group]]) & !is.finite(d[[group]]))
            n_dropped <- sum(nonfinite)
            if (n_dropped > 0) {
                d <- d[!nonfinite, , drop = FALSE]
                # Singular and plural are two whole alternative sentences, not a glued
                # "row" + "s": jmvcore's .() has no ngettext, and plural rules differ by
                # language. Without this the message read "1 rows".
                private$.appendWarning(if (n_dropped == 1)
                    .fmt(.("<b>Non-finite values removed:</b> one row contained an infinite value (Inf or -Inf) in '{xvar}' or '{yvar}'. Infinite values are not missing values, so they pass the usual completeness checks while making every correlation undefined; it has been excluded and all statistics below are computed on the {kept} remaining rows."),
                         xvar = htmltools::htmlEscape(dep),
                         yvar = htmltools::htmlEscape(group), kept = nrow(d))
                else
                    .fmt(.("<b>Non-finite values removed:</b> {dropped} rows contained an infinite value (Inf or -Inf) in '{xvar}' or '{yvar}'. Infinite values are not missing values, so they pass the usual completeness checks while making every correlation undefined; they have been excluded and all statistics below are computed on the {kept} remaining rows."),
                         dropped = n_dropped, xvar = htmltools::htmlEscape(dep),
                         yvar = htmltools::htmlEscape(group), kept = nrow(d)))
            }
            d
        },

        # Statistically validated correlation engine shared across .plot, .plot2 and .plot3.
        # Resolves upstream correlation 0.8.8 defects:
        #   (1) Bayesian HDI now honours conf_level directly via BayesFactor::posterior()
        #       + exact shortest-interval HDI (upstream dropped ci= and mislabelled 95% HDI).
        #   (2) Spearman uses exact AS89 permutation p-value when untied (n <= 1290) and
        #       Bonett-Wright (2000) SE sqrt((1 + rho^2/2)/(n - 3)), avoiding p = 2.23e-308
        #       on n = 3/4 perfect ranks and aligning with ggpubrPlot.
        #   (3) Robust 20% Winsorized Pearson uses Wilcox (2012, Sec. 9.3.4) adjusted
        #       df = h - 2 = n - 2g - 2 and seeded percentile-bootstrap CI (nboot = 599),
        #       matching WRS2::wincor instead of unadjusted df = n - 2 / naive Fisher-z CI.
        .hdiShortest = function(draws, cred = 0.95) {
            draws <- draws[is.finite(draws)]
            m <- length(draws)
            if (m < 10L) return(c(NA_real_, NA_real_))
            s <- sort(draws)
            w <- max(1L, min(m - 1L, as.integer(floor(cred * m))))
            widths <- s[(w + 1L):m] - s[1L:(m - w)]
            idx <- which.min(widths)
            c(s[idx], s[idx + w])
        },

        .fmtStatVal = function(val, dp = 2L) {
            if (is.null(val) || length(val) != 1L || !is.finite(val))
                return("NA")
            dp <- max(0L, min(5L, as.integer(dp)))
            if (val != 0 && abs(val) < 10^(-dp) && dp > 0L) {
                sprintf(paste0("%.", dp, "e"), val)
            } else {
                sprintf(paste0("%.", dp, "f"), val)
            }
        },

        .computeCorrStats = function(x, y, type = "parametric", conf_level = 0.95, digits = 2L) {
            cl_raw <- suppressWarnings(as.numeric(conf_level))
            conf_level <- if (length(cl_raw) == 1L && is.finite(cl_raw) && cl_raw >= 0.50 && cl_raw <= 0.999) cl_raw else 0.95
            ok <- is.finite(x) & is.finite(y)
            x <- x[ok]; y <- y[ok]; n <- length(x)
            if (n < 3L || length(unique(x)) < 2L || length(unique(y)) < 2L) {
                return(list(valid = FALSE, type = type, n = n, conf_level = conf_level))
            }

            if (identical(type, "parametric")) {
                ct <- stats::cor.test(x, y, method = "pearson", conf.level = conf_level)
                ci <- if (!is.null(ct$conf.int) && length(ct$conf.int) == 2L) {
                    as.numeric(ct$conf.int)
                } else {
                    c(NA_real_, NA_real_)
                }
                return(list(
                    valid = TRUE, type = "parametric", method = .("Pearson correlation"),
                    estimate = as.numeric(ct$estimate),
                    statistic = as.numeric(ct$statistic),
                    df = as.numeric(ct$parameter),
                    p.value = as.numeric(ct$p.value),
                    conf.low = ci[1L], conf.high = ci[2L],
                    conf_level = conf_level, n = n
                ))
            }

            if (identical(type, "nonparametric")) {
                rx <- rank(x, ties.method = "average")
                ry <- rank(y, ties.method = "average")
                rho <- stats::cor(rx, ry)
                S_stat <- (n^3 - n) * (1 - rho) / 6
                has_ties <- (length(unique(x)) < n) || (length(unique(y)) < n)
                if (!has_ties && n <= 1290L) {
                    pv <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = TRUE)$p.value)
                } else {
                    # With ties and |rho| == 1 on very small n, the t-approx gives Inf -> 0;
                    # fall back to the exact permutation bound 2/n! if needed instead of 2.23e-308.
                    if (abs(rho) >= 1 - 1e-12 && n <= 10L) {
                        pv <- min(1, 2 / factorial(n))
                    } else {
                        pv <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE)$p.value)
                    }
                }
                # Bonett & Wright (2000) SE = sqrt((1 + rho^2 / 2) / (n - 3))
                if (n > 3L && is.finite(rho) && abs(rho) < 1) {
                    z <- stats::qnorm(1 - (1 - conf_level) / 2)
                    se_bw <- sqrt((1 + (rho^2) / 2) / (n - 3))
                    ci <- tanh(atanh(rho) + c(-1, 1) * z * se_bw)
                } else {
                    ci <- c(NA_real_, NA_real_)
                }
                return(list(
                    valid = TRUE, type = "nonparametric", method = .("Spearman correlation"),
                    estimate = as.numeric(rho),
                    statistic = as.numeric(S_stat),
                    df = NULL,
                    p.value = as.numeric(pv),
                    conf.low = ci[1L], conf.high = ci[2L],
                    conf_level = conf_level, n = n
                ))
            }

            if (identical(type, "robust")) {
                tr <- 0.2
                g <- as.integer(floor(tr * n))
                win_vec <- function(v) {
                    nv <- length(v)
                    gv <- as.integer(floor(tr * nv))
                    s <- sort(v)
                    lo <- s[gv + 1L]; hi <- s[nv - gv]
                    v[v < lo] <- lo; v[v > hi] <- hi; v
                }
                xw <- win_vec(x); yw <- win_vec(y)
                if (length(unique(xw)) < 2L || length(unique(yw)) < 2L) {
                    return(list(valid = FALSE, type = "robust", n = n, conf_level = conf_level))
                }
                rw <- stats::cor(xw, yw)
                df_w <- n - 2L * g - 2L
                tw <- if (abs(rw) >= 1) sign(rw) * Inf else rw * sqrt((n - 2) / (1 - rw^2))
                pw <- if (df_w > 0L) 2 * stats::pt(-abs(tw), df = df_w) else NA_real_

                # Wilcox (2012, Sec. 9.3.4) percentile bootstrap CI (matches WRS2::wincor)
                nboot <- 599L
                bvec <- numeric(nboot)
                for (b in seq_len(nboot)) {
                    idx <- sample.int(n, n, replace = TRUE)
                    xb <- win_vec(x[idx]); yb <- win_vec(y[idx])
                    bvec[b] <- if (length(unique(xb)) < 2L || length(unique(yb)) < 2L) NA_real_ else stats::cor(xb, yb)
                }
                bvec <- sort(bvec[is.finite(bvec)])
                if (length(bvec) >= 50L) {
                    alpha <- 1 - conf_level
                    nb <- length(bvec)
                    ilow <- max(1L, min(nb, as.integer(floor((alpha / 2) * nb + 0.5))))
                    ihi  <- max(1L, min(nb, nb - ilow + 1L))
                    ci <- c(bvec[ilow], bvec[ihi])
                } else {
                    ci <- c(NA_real_, NA_real_)
                }
                return(list(
                    valid = TRUE, type = "robust", method = .("Winsorized Pearson correlation"),
                    estimate = as.numeric(rw),
                    statistic = as.numeric(tw),
                    df = as.numeric(df_w),
                    p.value = as.numeric(pw),
                    conf.low = ci[1L], conf.high = ci[2L],
                    conf_level = conf_level, n = n
                ))
            }

            if (identical(type, "bayes")) {
                bf_prior <- 0.707
                bf_obj <- suppressWarnings(BayesFactor::correlationBF(y = y, x = x, rscale = bf_prior))
                log_bf10 <- as.numeric(bf_obj@bayesFactor$bf)
                bf10 <- exp(log_bf10)
                log_bf01 <- -log_bf10
                draws <- suppressWarnings(suppressMessages(
                    as.numeric(BayesFactor::posterior(bf_obj, iterations = 10000, progress = FALSE)[, "rho"])
                ))
                est <- stats::median(draws, na.rm = TRUE)
                hdi <- private$.hdiShortest(draws, cred = conf_level)
                return(list(
                    valid = TRUE, type = "bayes", method = .("Bayesian Pearson correlation"),
                    estimate = as.numeric(est),
                    bf10 = bf10,
                    log_bf01 = log_bf01,
                    prior_scale = 1 / bf_prior,
                    conf.low = hdi[1L], conf.high = hdi[2L],
                    conf_level = conf_level, n = n
                ))
            }

            list(valid = FALSE, type = type, n = n, conf_level = conf_level)
        },

        .buildPlotmathExpr = function(st, digits = 2L) {
            if (!isTRUE(st$valid)) return(NULL)
            dp <- max(0L, min(5L, as.integer(digits)))
            ci_pct <- paste0(round(100 * st$conf_level, 1), "%")
            ci_lo  <- private$.fmtStatVal(st$conf.low, dp)
            ci_hi  <- private$.fmtStatVal(st$conf.high, dp)
            est_s  <- private$.fmtStatVal(st$estimate, dp)
            n_s    <- base::format(st$n, big.mark = ",", trim = TRUE)

            if (identical(st$type, "bayes")) {
                lbf_s   <- private$.fmtStatVal(st$log_bf01, dp)
                prior_s <- private$.fmtStatVal(st$prior_scale, dp)
                return(bquote(list(
                    log[e] * (BF["01"]) == .(lbf_s),
                    widehat(rho)["Pearson"]^"posterior" == .(est_s),
                    CI[.(ci_pct)]^HDI ~ "[" * .(ci_lo),
                    .(ci_hi) * "]",
                    italic("r")["beta"]^"JZS" == .(prior_s)
                )))
            }

            p_s <- private$.fmtStatVal(st$p.value, dp)
            if (identical(st$type, "nonparametric")) {
                stat_s <- private$.fmtStatVal(st$statistic, dp)
                return(bquote(list(
                    italic("S") == .(stat_s),
                    italic(p) == .(p_s),
                    widehat(rho)["Spearman"] == .(est_s),
                    CI[.(ci_pct)] ~ "[" * .(ci_lo),
                    .(ci_hi) * "]",
                    italic("n")["pairs"] == .(n_s)
                )))
            }

            stat_s <- private$.fmtStatVal(st$statistic, dp)
            df_val <- if (is.finite(st$df)) as.numeric(round(st$df)) else "NA"
            est_sub <- if (identical(st$type, "robust")) "Winsorized" else "Pearson"
            bquote(list(
                italic("t")["Student"] * "(" * .(df_val) * ")" == .(stat_s),
                italic(p) == .(p_s),
                widehat(italic("r"))[.(est_sub)] == .(est_s),
                CI[.(ci_pct)] ~ "[" * .(ci_lo),
                .(ci_hi) * "]",
                italic("n")["pairs"] == .(n_s)
            ))
        },

        # plot ----

        .plot = function(image, ggtheme, theme, ...) {
            # Seed the sampling-based paths (Bayesian MCMC, Winsorized bootstrap CIs)
            # so a re-render of an unchanged analysis reports the same numbers.
            withr::local_seed(self$options$seed)


            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            plotData <- private$.prepData()

            # Prepare arguments for ggscatterstats
            labels <- private$.resolveLabels(includeGrvar = FALSE)
            title <- labels$title
            xtitle <- labels$xtitle
            ytitle <- labels$ytitle

            # Smoothing formula: only "gam" needs an explicit spline formula;
            # otherwise the default y ~ x linear form is used. Without this a
            # method="gam" fit collapses to a straight line (identical to lm).
            smooth_formula <- if (identical(self$options$smoothMethod, "gam")) {
                y ~ s(x, bs = "cs")
            } else {
                y ~ x
            }

            ty <- as.character(private$.option("typestatistics"))
            show_sub <- isTRUE(private$.option("resultssubtitle"))
            same_var <- identical(self$options$dep, self$options$group)

            # Function arguments (statistics subtitle/caption are attached via our
            # validated .computeCorrStats() helper below so Bayes conflevel, exact
            # small-n Spearman, and Wilcox Winsorized df/bootstrap CI are exact).
            .args <- list(
                data = plotData,
                x = self$options$dep,
                y = self$options$group,
                type = ty,
                title = title,
                xlab = xtitle,
                ylab = ytitle,
                results.subtitle = FALSE,
                conf.level = self$options$conflevel,
                bf.message = FALSE,
                digits = self$options$k,
                marginal = private$.option("marginal"),
                point.args = list(
                    size = self$options$pointsize,
                    alpha = self$options$pointalpha
                ),
                smooth.line.args = list(
                    linewidth = self$options$smoothlinesize,
                    color = self$options$smoothlinecolor,
                    method = self$options$smoothMethod,
                    formula = smooth_formula
                )
            )

            if (private$.option("marginal")) {
                .args$xsidehistogram.args <- list(fill = self$options$xsidefill, color = "black", na.rm = TRUE)
                .args$ysidehistogram.args <- list(fill = self$options$ysidefill, color = "black", na.rm = TRUE)
            }

            plot <- do.call(ggstatsplot::ggscatterstats, .args)

            stats_drawn <- FALSE
            if (show_sub && !same_var) {
                st <- private$.computeCorrStats(
                    plotData[[self$options$dep]], plotData[[self$options$group]],
                    type = ty, conf_level = self$options$conflevel, digits = self$options$k
                )
                if (isTRUE(st$valid)) {
                    stats_drawn <- TRUE
                    sub_expr <- private$.buildPlotmathExpr(st, digits = self$options$k)
                    cap_expr <- NULL
                    if (identical(ty, "parametric") && isTRUE(self$options$bfmessage)) {
                        st_bf <- private$.computeCorrStats(
                            plotData[[self$options$dep]], plotData[[self$options$group]],
                            type = "bayes", conf_level = self$options$conflevel, digits = self$options$k
                        )
                        if (isTRUE(st_bf$valid))
                            cap_expr <- private$.buildPlotmathExpr(st_bf, digits = self$options$k)
                    }
                    plot <- plot + ggplot2::labs(subtitle = sub_expr, caption = cap_expr)
                }
            }

            if (self$options$showRugPlot) {
                plot <- plot + ggplot2::geom_rug(alpha = 0.5)
            }

            if (!private$.option("originaltheme")) {
                plot <- plot + ggtheme
            } else {
                plot <- plot + ggstatsplot::theme_ggstatsplot()
            }

            seed_needed <- isTRUE(stats_drawn) && (
                statsSeedMatters(ty, "correlation") ||
                identical(ty, "robust") ||
                captionSeedMatters(ty, "correlation", 2L, self$options$bfmessage)
            )
            if (seed_needed)
                plot <- addSeedCaption(plot, self, self$options$seed)

            print(plot)
            TRUE
        },

        # plot2 ----

        .plot2 = function(image, ggtheme, theme, ...) {
            # Seed the sampling-based paths (Bayesian MCMC, Winsorized bootstrap CIs)
            # so a re-render of an unchanged analysis reports the same numbers.
            withr::local_seed(self$options$seed)


            if (is.null(self$options$dep) || is.null(self$options$group) || is.null(self$options$grvar))
                return()

            plotData <- private$.prepData()

            # Prepare arguments for grouped_ggscatterstats
            labels <- private$.resolveLabels(includeGrvar = TRUE)
            title <- labels$title
            xtitle <- labels$xtitle
            ytitle <- labels$ytitle

            # Smoothing formula: only "gam" needs an explicit spline formula.
            smooth_formula <- if (identical(self$options$smoothMethod, "gam")) {
                y ~ s(x, bs = "cs")
            } else {
                y ~ x
            }

            ty <- as.character(private$.option("typestatistics"))
            show_sub <- isTRUE(private$.option("resultssubtitle"))
            same_var <- identical(self$options$dep, self$options$group)

            # Call grouped_ggscatterstats with proper NSE handling via rlang::inject(),
            # splicing marginal histogram args cleanly without duplicating the call.
            extra_marg <- if (private$.option("marginal")) {
                list(
                    xsidehistogram.args = list(fill = self$options$xsidefill, color = "black", na.rm = TRUE),
                    ysidehistogram.args = list(fill = self$options$ysidefill, color = "black", na.rm = TRUE)
                )
            } else {
                list()
            }

            plot <- rlang::inject(
                ggstatsplot::grouped_ggscatterstats(
                    data = plotData,
                    x = !!rlang::sym(self$options$dep),
                    y = !!rlang::sym(self$options$group),
                    grouping.var = !!rlang::sym(self$options$grvar),
                    type = !!ty,
                    xlab = !!xtitle,
                    ylab = !!ytitle,
                    results.subtitle = FALSE,
                    conf.level = !!self$options$conflevel,
                    bf.message = FALSE,
                    digits = !!self$options$k,
                    marginal = !!private$.option("marginal"),
                    point.args = !!list(
                        size = self$options$pointsize,
                        alpha = self$options$pointalpha
                    ),
                    # See the note in .plot: method/formula belong INSIDE smooth.line.args,
                    # and overwriting that list without them silently reverts to loess.
                    smooth.line.args = !!list(
                        linewidth = self$options$smoothlinesize,
                        color = self$options$smoothlinecolor,
                        method = self$options$smoothMethod,
                        formula = smooth_formula
                    ),
                    !!!extra_marg
                )
            )

            # Attach validated per-panel subtitles/captions directly from each panel's
            # subset data frame, preventing broken `NULL == "NA"` plotmath on sparse strata.
            any_stats_drawn <- FALSE
            if (show_sub && !same_var) {
                n_panels <- length(plot$patches$plots) + 1L
                for (i in seq_len(n_panels)) {
                    private$.checkpoint()
                    pan <- plot[[i]]
                    pd <- pan$data
                    st <- private$.computeCorrStats(
                        pd[[self$options$dep]], pd[[self$options$group]],
                        type = ty, conf_level = self$options$conflevel, digits = self$options$k
                    )
                    if (isTRUE(st$valid)) {
                        any_stats_drawn <- TRUE
                        sub_expr <- private$.buildPlotmathExpr(st, digits = self$options$k)
                        cap_expr <- NULL
                        if (identical(ty, "parametric") && isTRUE(self$options$bfmessage)) {
                            st_bf <- private$.computeCorrStats(
                                pd[[self$options$dep]], pd[[self$options$group]],
                                type = "bayes", conf_level = self$options$conflevel, digits = self$options$k
                            )
                            if (isTRUE(st_bf$valid))
                                cap_expr <- private$.buildPlotmathExpr(st_bf, digits = self$options$k)
                        }
                        plot[[i]] <- pan + ggplot2::labs(subtitle = sub_expr, caption = cap_expr)
                    } else {
                        plot[[i]] <- pan + ggplot2::labs(subtitle = .("Correlation not computed (insufficient data)"))
                    }
                }
            }

            plot <- plot + patchwork::plot_annotation(title = title)

            if (self$options$showRugPlot) {
                plot <- plot & ggplot2::geom_rug(alpha = 0.5)
            }

            if (!private$.option("originaltheme")) {
                plot <- plot & ggtheme
            } else {
                plot <- plot & ggstatsplot::theme_ggstatsplot()
            }

            seed_needed <- isTRUE(any_stats_drawn) && (
                statsSeedMatters(ty, "correlation") ||
                identical(ty, "robust") ||
                captionSeedMatters(ty, "correlation", 2L, self$options$bfmessage)
            )
            if (seed_needed)
                plot <- addSeedCaption(plot, self, self$options$seed)

            print(plot)
            TRUE
        },

        # plot3 - Enhanced scatter with multiple aesthetics ----

        .plot3 = function(image, ggtheme, theme, ...) {
            # Seed the sampling-based paths (Bayesian MCMC, bootstrap CIs) so a
            # re-render of an unchanged analysis reports the same numbers.
            withr::local_seed(self$options$seed)


            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            # Only show if any enhanced variables are selected
            hasEnhanced <- !is.null(self$options$colorvar) ||
                          !is.null(self$options$sizevar) ||
                          !is.null(self$options$shapevar) ||
                          !is.null(self$options$alphavar) ||
                          !is.null(self$options$labelvar)

            if (!hasEnhanced)
                return()

            plotData <- private$.prepData()

            # Prepare title and labels
            labels <- private$.resolveLabels(includeGrvar = FALSE)
            title <- labels$title
            xtitle <- labels$xtitle
            ytitle <- labels$ytitle

            # Build base aesthetic mapping
            aes_mapping <- ggplot2::aes(
                x = .data[[self$options$dep]],
                y = .data[[self$options$group]]
            )

            # Start building plot
            p <- ggplot2::ggplot(plotData, aes_mapping)

            # Build point aesthetics mapping
            point_aes <- list()

            if (!is.null(self$options$colorvar) && self$options$colorvar != "") {
                point_aes$colour <- rlang::sym(self$options$colorvar)
            }

            if (!is.null(self$options$sizevar) && self$options$sizevar != "") {
                point_aes$size <- rlang::sym(self$options$sizevar)
            }

            if (!is.null(self$options$shapevar) && self$options$shapevar != "") {
                # ggplot2's discrete shape palette carries only 6 values. Beyond that it
                # emits a console warning -- which jamovi never shows -- and DROPS every
                # point in the surplus levels. Measured: 7 levels drew 103 of 120 points,
                # 12 levels drew 60 of 120, while the correlation printed below the plot
                # was computed on all 120. A figure that silently omits half the cohort is
                # worse than one without shapes, so map shape only when it can be honoured.
                # The notice for this lives in .run() (.warnShapeLevels): an Html item
                # written from a render callback is never sent to the results panel.
                if (private$.shapeLevels(plotData) <= 6)
                    point_aes$shape <- rlang::sym(self$options$shapevar)
            }

            if (!is.null(self$options$alphavar) && self$options$alphavar != "") {
                point_aes$alpha <- rlang::sym(self$options$alphavar)
            }

            # Add points with aesthetics, preserving pointsize/pointalpha when not mapped
            pt_fixed <- list()
            if (!("size" %in% names(point_aes)))  pt_fixed$size  <- self$options$pointsize
            if (!("alpha" %in% names(point_aes))) pt_fixed$alpha <- self$options$pointalpha
            if (length(point_aes) > 0L) {
                pt_fixed$mapping <- do.call(ggplot2::aes, point_aes)
            }
            p <- p + do.call(ggplot2::geom_point, pt_fixed)

            # Add smooth line with selected method and conflevel
            smooth_method <- switch(
                self$options$smoothMethod,
                "lm" = "lm",
                "loess" = "loess",
                "gam" = "gam",
                "lm"
            )

            smooth_formula <- if (identical(smooth_method, "gam")) {
                y ~ s(x, bs = "cs")
            } else {
                y ~ x
            }

            p <- p + ggplot2::geom_smooth(
                method = smooth_method,
                formula = smooth_formula,
                se = TRUE,
                level = self$options$conflevel,
                linewidth = self$options$smoothlinesize,
                color = self$options$smoothlinecolor
            )

            # Add rug plot if requested
            if (self$options$showRugPlot) {
                p <- p + ggplot2::geom_rug(alpha = 0.3, length = ggplot2::unit(0.05, "npc"))
            }

            # Add point labels using ggrepel if requested
            if (!is.null(self$options$labelvar) && self$options$labelvar != "") {
                if (requireNamespace("ggrepel", quietly = TRUE)) {
                    label_aes <- ggplot2::aes(label = .data[[self$options$labelvar]])
                    p <- p + ggrepel::geom_text_repel(
                        mapping = label_aes,
                        size = 3,
                        max.overlaps = 10
                    )
                }
            }

            # Correlation annotation gated on resultssubtitle and routed through our
            # unified .computeCorrStats() helper so plot3 agrees with plot for all 4 types.
            .corr_done <- FALSE
            test_type <- as.character(private$.option("typestatistics"))
            if (isTRUE(private$.option("resultssubtitle")) && !identical(self$options$dep, self$options$group)) {
                corr_res <- tryCatch({
                    st <- private$.computeCorrStats(
                        plotData[[self$options$dep]], plotData[[self$options$group]],
                        type = test_type, conf_level = self$options$conflevel, digits = self$options$k
                    )
                    if (!isTRUE(st$valid)) {
                        list(sub = .("Correlation not computed (insufficient data)"), done = FALSE)
                    } else {
                        dp <- max(0L, min(5L, as.integer(self$options$k)))
                        cor_symbol <- switch(test_type, nonparametric = "rho", "r")
                        est <- sprintf(paste0("%s = %.", dp, "f"), cor_symbol, st$estimate)
                        ci_txt <- if (is.finite(st$conf.low) && is.finite(st$conf.high)) {
                            sprintf(paste0(", %s%% CI [%.", dp, "f, %.", dp, "f]"),
                                    round(100 * st$conf_level, 1), st$conf.low, st$conf.high)
                        } else {
                            ""
                        }

                        tail_txt <- if (identical(test_type, "bayes") && is.finite(st$bf10)) {
                            sprintf(paste0("BF10 = %.", max(dp, 2L), "g"), st$bf10)
                        } else if (is.finite(st$p.value)) {
                            p_dp <- max(dp, 3L)
                            p_cut <- 10^(-p_dp)
                            if (st$p.value < p_cut) {
                                sprintf(paste0("p < %.", p_dp, "f"), p_cut)
                            } else {
                                sprintf(paste0("p = %.", p_dp, "f"), st$p.value)
                            }
                        } else {
                            "p = NA"
                        }

                        list(
                            sub = paste0(st$method, ": ", est, ci_txt, ", ", tail_txt, ", n = ", st$n),
                            done = TRUE
                        )
                    }
                }, error = function(e) {
                    list(sub = .("Correlation not computed"), done = FALSE)
                })
                .corr_done <- isTRUE(corr_res$done)
                p <- p + ggplot2::labs(subtitle = corr_res$sub)
            }

            # Add labels
            p <- p + ggplot2::labs(
                title = title,
                x = xtitle,
                y = ytitle
            )

            # Apply theme. jamovi's `ggtheme`, matching .plot/.plot2 -- a hardcoded
            # theme_bw() here made this panel ignore the user's jamovi theme setting.
            if (!private$.option("originaltheme")) {
                p <- p + ggtheme
            } else {
                p <- p + ggstatsplot::theme_ggstatsplot()
            }

            # only when the correlation was actually computed - a failed or skipped one
            # leaves the "not computed" subtitle, which no seed produced
            if (isTRUE(.corr_done) && (statsSeedMatters(test_type, "correlation") || identical(test_type, "robust")))
                p <- addSeedCaption(p, self, self$options$seed)

            # Add marginal plots if requested
            if (self$options$marginalType != "none") {
                if (requireNamespace("ggExtra", quietly = TRUE)) {
                    p <- ggExtra::ggMarginal(
                        p,
                        type = self$options$marginalType,
                        size = 5
                    )
                }
            }

            print(p)
            TRUE
        }

        ,
        # The ggpubr panels take their correlation from `ggpubrCorrMethod`, which is a
        # SEPARATE option from `typestatistics` that drives the main ggstatsplot panels. A
        # user who switches the analysis to Spearman but leaves ggpubrCorrMethod at its
        # default therefore gets two different coefficients for the same two variables in the
        # same output, with nothing saying why. Measured on 80 points: main panel rho =
        # 0.6275, ggpubr panel r = 0.6683. Both are correct; the silence is the problem.
        # Append to the Messages output. Kept in one place because the previous inline
        # copies read $state while writing setContent(), which are different slots on an
        # Html item, so each warning silently replaced the one before it.
        .appendWarning = function(html) {
            if (!("warnings" %in% self$results$itemNames)) return(invisible(NULL))
            current <- self$results$warnings$content
            if (is.null(current)) current <- ""
            if (grepl(substr(html, 1, 40), current, fixed = TRUE)) return(invisible(NULL))
            self$results$warnings$setContent(
                paste0(current, "<p style='background-color: rgba(255, 193, 7, 0.14); ",
                       "border-left: 4px solid #ffc107; padding: 8px 12px; margin: 6px 0; ",
                       "color: inherit;'>", html, "</p>"))
            self$results$warnings$setVisible(TRUE)
            invisible(NULL)
        },

        # Called from .run(), NOT from .plot*(). An Html item written during a plot
        # callback is never rendered: jamovi has already composed and sent the results
        # panel by the time the image is drawn, so the notice silently went nowhere.
        .warnCorrMethodMismatch = function() {
            if (!isTRUE(private$.option("addGGPubrPlot"))) return(invisible(NULL))
            if (!isTRUE(self$options$ggpubrAddCorr)) return(invisible(NULL))
            ty <- as.character(private$.option("typestatistics"))
            implied <- switch(ty, parametric = "pearson", nonparametric = "spearman", NULL)
            chosen <- self$options$ggpubrCorrMethod

            # robust/bayes have no ggpubr analogue, so the panel quietly fell back to an
            # ordinary Pearson/Spearman coefficient that appears nowhere in the analysis
            # the user asked for. Measured: main plot Winsorized r = 0.06 (p = 0.31) beside
            # a ggpubr panel reporting r = 0.0739 (p = 0.2475), with nothing said.
            if (is.null(implied)) {
                ty_label <- switch(ty,
                                   robust = .("robust (Winsorized Pearson)"),
                                   bayes = .("Bayesian"), ty)
                private$.appendWarning(.fmt(.("<b>The panel below is not showing your analysis type.</b> The main plot runs a {method} correlation, which the publication-ready panel cannot draw; that panel instead reports an ordinary <b>{fallback}</b> coefficient computed on the same two variables. Quote the main plot's subtitle for the {method} result."),
                    method = ty_label, fallback = chosen))
                return(invisible(NULL))
            }
            if (identical(implied, chosen)) return(invisible(NULL))

            private$.appendWarning(.fmt(.("The publication-ready panel is reporting a <b>{panel}</b> correlation while the main plot reports <b>{main}</b>, because 'Correlation method (ggpubr)' is set independently of 'Statistical Test Type'. Both coefficients are correct for this data, but they are different statistics - set the two to match unless you intend to show both."),
                panel = chosen, main = implied))
            invisible(NULL)
        },

        # ggpubr resolves x / y / color / facet.by by parse()ing the column NAME as R
        # code (ggpubr:::create_aes -> parse(text = .)), so a perfectly ordinary jamovi
        # column called "Tumor Size (mm)" is a syntax error, not a lookup miss:
        #   Caused by error in `parse()`: <text>:1:7: unexpected symbol  1: Tumor Size
        # Both publication panels died outright on any name that is not a syntactic R
        # name -- spaces, parentheses, slashes, a leading digit. ggstatsplot handles
        # these fine, so only this path needs the workaround.
        #
        # Returns the data with syntactic names plus the mapping, so the caller can
        # restore the real names as axis/legend labels.
        .ggpubrSafeNames = function(d) {
            orig <- names(d)
            safe <- make.names(orig, unique = TRUE)
            names(d) <- safe
            list(data = d, safe = stats::setNames(safe, orig))
        },

        .plotGGPubr = function(image, ...) {
            # Validate inputs
            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            # Skip if ggpubr plot not requested
            if (!private$.option("addGGPubrPlot"))
                return()

            # Prepare data. Same conversion/finite-filter as every other panel: this path
            # used to pass self$data straight through, so an ordinal column reached
            # ggscatter as a factor and the correlation label evaluated to NA.
            mydata <- private$.prepData()
            dep <- self$options$dep
            group <- self$options$group

            # Guard against degenerate x or y (ggpubr:::StatCor checks only unique(x) < 2,
            # leaving constant y to render literal "R = NA, NA").
            x_v <- mydata[[dep]]; y_v <- mydata[[group]]
            ok_v <- is.finite(x_v) & is.finite(y_v)
            can_corr <- !identical(dep, group) && sum(ok_v) >= 3L &&
                length(unique(x_v[ok_v])) >= 2L && length(unique(y_v[ok_v])) >= 2L

            # See .ggpubrSafeNames: ggpubr parses the column name as R code.
            sn <- private$.ggpubrSafeNames(mydata)
            mydata <- sn$data

            dp <- max(0L, min(5L, as.integer(self$options$k)))
            p_dp <- max(dp, 3L)

            # Build scatter plot arguments
            args <- list(
                data = mydata,
                x = sn$safe[[dep]],
                y = sn$safe[[group]],
                palette = private$.option("ggpubrPalette")
            )

            add_element <- NULL

            if (self$options$ggpubrAddCorr && can_corr) {
                add_element <- "reg.line"
                args$conf.int <- TRUE
                args$conf.int.level <- self$options$conflevel
                args$cor.coef <- TRUE
                args$cor.method <- self$options$ggpubrCorrMethod
                args$cor.coeff.args <- list(
                    cor.coef.name = switch(self$options$ggpubrCorrMethod,
                                           pearson = "R", spearman = "rho", "R"),
                    r.accuracy = 10^(-dp),
                    p.accuracy = 10^(-p_dp)
                )
            } else if (self$options$ggpubrAddSmooth && can_corr) {
                add_element <- "loess"
                args$conf.int <- TRUE
                args$conf.int.level <- self$options$conflevel
            }

            if (!is.null(add_element)) {
                args$add <- add_element
            }

            plot <- do.call(ggpubr::ggscatter, args)

            if (self$options$ggpubrAddCorr && self$options$ggpubrAddSmooth && can_corr) {
                plot <- plot + ggplot2::geom_smooth(method = "loess", se = TRUE, level = self$options$conflevel)
            }

            lbl <- private$.resolveLabels(includeGrvar = FALSE)
            plot <- plot + ggplot2::labs(title = lbl$title, x = lbl$xtitle, y = lbl$ytitle)

            plot <- plot + ggpubr::theme_pubr()

            print(plot)
            TRUE
        }

        ,
        .plotGGPubr2 = function(image, ...) {
            # Validate inputs
            if (is.null(self$options$dep) || is.null(self$options$group) || is.null(self$options$grvar))
                return()

            # Skip if ggpubr plot not requested
            if (!private$.option("addGGPubrPlot"))
                return()

            mydata <- private$.prepData()
            dep <- self$options$dep
            group <- self$options$group
            grvar <- self$options$grvar

            # Exclude rows with missing grouping variable so ggpubrPlot2 matches
            # plot2 (grouped_ggscatterstats) and does not create a spurious "NA" facet.
            mydata <- mydata[!is.na(mydata[[grvar]]), , drop = FALSE]
            if (nrow(mydata) == 0L)
                return()

            # Determine which strata have enough variation to compute a valid correlation
            # (prevents ggpubr:::StatCor from printing "R = NA, NA" on constant-y facets).
            g_chr <- as.character(mydata[[grvar]])
            valid_strata <- vapply(unique(g_chr), function(lv) {
                idx <- g_chr == lv
                xv <- mydata[[dep]][idx]; yv <- mydata[[group]][idx]
                okv <- is.finite(xv) & is.finite(yv)
                !identical(dep, group) && sum(okv) >= 3L &&
                    length(unique(xv[okv])) >= 2L && length(unique(yv[okv])) >= 2L
            }, logical(1))
            valid_lvls <- names(valid_strata)[valid_strata]
            any_valid <- length(valid_lvls) > 0L

            sn <- private$.ggpubrSafeNames(mydata)
            mydata <- sn$data

            dp <- max(0L, min(5L, as.integer(self$options$k)))
            p_dp <- max(dp, 3L)

            args <- list(
                data = mydata,
                x = sn$safe[[dep]],
                y = sn$safe[[group]],
                color = sn$safe[[grvar]],
                palette = private$.option("ggpubrPalette"),
                facet.by = sn$safe[[grvar]]
            )

            add_element <- NULL
            all_valid <- all(valid_strata)

            if (self$options$ggpubrAddCorr && all_valid) {
                add_element <- "reg.line"
                args$conf.int <- TRUE
                args$conf.int.level <- self$options$conflevel
                args$cor.coef <- TRUE
                args$cor.method <- self$options$ggpubrCorrMethod
                args$cor.coeff.args <- list(
                    cor.coef.name = switch(self$options$ggpubrCorrMethod,
                                           pearson = "R", spearman = "rho", "R"),
                    r.accuracy = 10^(-dp),
                    p.accuracy = 10^(-p_dp)
                )
            } else if (self$options$ggpubrAddCorr && any_valid) {
                # Mixed valid/degenerate strata: attach reg.line and stat_cor only on valid strata
                args$cor.coef <- FALSE
            } else if (self$options$ggpubrAddSmooth && any_valid) {
                add_element <- "loess"
                args$conf.int <- TRUE
                args$conf.int.level <- self$options$conflevel
            }

            if (!is.null(add_element)) {
                args$add <- add_element
            }

            plot <- do.call(ggpubr::ggscatter, args)

            if (self$options$ggpubrAddCorr && any_valid && !all_valid) {
                valid_df <- mydata[as.character(mydata[[sn$safe[[grvar]]]]) %in% valid_lvls, , drop = FALSE]
                plot <- plot +
                    ggplot2::geom_smooth(data = valid_df, method = "lm", formula = y ~ x,
                                         se = TRUE, level = self$options$conflevel) +
                    ggpubr::stat_cor(
                        data = valid_df,
                        method = self$options$ggpubrCorrMethod,
                        cor.coef.name = switch(self$options$ggpubrCorrMethod,
                                               pearson = "R", spearman = "rho", "R"),
                        r.accuracy = 10^(-dp),
                        p.accuracy = 10^(-p_dp)
                    )
            }

            if (self$options$ggpubrAddCorr && self$options$ggpubrAddSmooth && any_valid) {
                plot <- plot + ggplot2::geom_smooth(method = "loess", se = TRUE, level = self$options$conflevel)
            }

            lbl <- private$.resolveLabels(includeGrvar = TRUE)
            plot <- plot + ggplot2::labs(title = lbl$title, x = lbl$xtitle,
                                         y = lbl$ytitle, color = grvar)

            plot <- plot + ggpubr::theme_pubr()

            print(plot)
            TRUE
        },

        # Hardened per-option syntax-pane serializer (Category G1):
        # drops undeclared option names and list values from crafted .omv/.omt files,
        # and escapes non-default character scalars via encodeString(quote = '"').
        .sourcifyOption = function(option) {
            if (!option$name %in% names(formals(jjscatterstats)))
                return("")
            value <- option$value
            if (is.list(value))
                return("")
            if (is.character(value) && length(value) == 1L && !is.na(value) &&
                !identical(value, option$default)) {
                return(paste0(option$name, " = ", encodeString(value, quote = '"')))
            }
            super$.sourcifyOption(option)
        }
    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for Scatter Plot Statistics analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            dep <- self$options$dep
            group <- self$options$group

            if (is.null(dep) || is.null(group))
                return('')

            # Build the argument list in option-declaration order, accepting only
            # declared wrapper arguments. Variable options are escaped via
            # encodeString(quote = '"') so spaces, quotes, backslashes, or
            # newlines produce valid R string literals; non-variable options go
            # through our hardened private$.sourcifyOption().
            valid_args <- names(formals(jjscatterstats))
            args <- character(0)
            for (option in private$.options$options) {
                if (option$name == 'data' || !(option$name %in% valid_args))
                    next
                if (inherits(option, 'OptionVariable')) {
                    val <- option$value
                    if (is.character(val) && length(val) == 1L && !is.na(val))
                        args <- c(args, paste0(option$name, ' = ',
                                               encodeString(val, quote = '"')))
                } else {
                    as <- private$.sourcifyOption(option)
                    if (!identical(as, ''))
                        args <- c(args, as)
                }
            }

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, '::jjscatterstats(\n    data = data,\n    ',
                   paste(args, collapse = ',\n    '), ')')
        }
    ) # End of public list
)
