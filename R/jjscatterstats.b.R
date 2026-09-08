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
        .STOCHASTIC_SEED = 20250101L,


        # Option overrides for clinical presets (jamovi options are read-only at runtime;
        # presets record overrides here and reads go through private$.option()).
        overrides = list(),
        .option = function(option) {
            if (option %in% names(private$overrides)) return(private$overrides[[option]])
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

            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 600
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450

            self$results$plot$setSize(plotwidth, plotheight)

            if (!is.null(self$options$grvar)) {
                mydata <- self$data
                grvar <-  self$options$grvar
                num_levels <- nlevels(as.factor(mydata[[grvar]]))
                # Cap the sizing multiplier so many-level factors do not blow up
                # the canvas; the panels themselves still all render.
                sizing_levels <- max(1, min(num_levels, 16))
                self$results$plot2$setSize(sizing_levels * plotwidth, plotheight)
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
            if ( is.null(self$options$dep) || is.null(self$options$group)) {

                # todo ----
                self$results$todo$setContent(private$.welcomeMessage())
                return()

            } else {

                # todo ----
                todo <- glue::glue(
                    "<br>You have selected to use a scatter plot with correlation analysis.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    jmvcore::reject(.('Data contains no (complete) rows'))
            }

            private$.applyClinicalPreset()
            private$.warnCorrMethodMismatch()
            private$.generateExplanations()

            # grouped_ggscatterstats() has no p-adjustment argument: each facet's correlation
            # is tested at the nominal level and the raw p is printed on that panel. With k
            # groups that is k tests. Reproduced on histopathology (Age x OverallTime by
            # Grade): raw 0.3015 / 0.0281 / 0.2069, Holm-adjusted 0.4138 / 0.0842 / 0.4138.
            # No displayed number is wrong; the omission is the family it belongs to.
            if (!is.null(self$options$grvar) && isTRUE(private$.option("resultssubtitle"))) {
                n_groups <- length(unique(stats::na.omit(self$data[[self$options$grvar]])))
                if (n_groups > 1)
                    private$.appendWarning(.fmt(.("<b>One test per group, unadjusted.</b> Each of the {groups} panels shows its own correlation tested at the nominal level, with no correction for the fact that {groups} tests are being read together. If you are screening groups rather than testing one pre-specified comparison, adjust the p-values yourself (Holm or FDR over the {groups} values) before drawing conclusions."),
                        groups = n_groups))
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

            private$.warnStatisticalCaveats(checkData)
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
                return(invisible(NULL))

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
                Sx <- sum(x)
                Sy <- sum(y)
                Sxx <- sum(x * x)
                Syy <- sum(y * y)
                Sxy <- sum(x * y)
                m <- n - 1L
                sx <- Sx - x
                sy <- Sy - y
                sxx <- Sxx - x * x
                syy <- Syy - y * y
                sxy <- Sxy - x * y
                den <- sqrt(pmax(0, m * sxx - sx^2) * pmax(0, m * syy - sy^2))
                r_loo <- ifelse(den > 0, (m * sxy - sx * sy) / den, NA_real_)
                r_full <- stats::cor(x, y)
                delta <- suppressWarnings(max(abs(r_full - r_loo), na.rm = TRUE))
                if (is.finite(delta) && delta > 0.2) {
                    worst <- which.max(abs(r_full - r_loo))
                    private$.appendWarning(.fmt(.("<b>One observation is driving this result:</b> dropping a single point changes the correlation from {full} to {dropped} (a shift of {delta}). A coefficient that depends this heavily on one patient should not be reported on its own - inspect that point on the plot, and consider the 'Robust (Winsorized Pearson)' or 'Nonparametric (Spearman)' test type, which are far less sensitive to it."),
                        full = sprintf("%.2f", r_full),
                        dropped = sprintf("%.2f", r_loo[worst]),
                        delta = sprintf("%.2f", delta)))
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
                # Fit the curve BOTH ways round and keep the stronger. A quadratic in
                # xx only sees curvature along one axis: for y = x^2 it fits perfectly,
                # but put the parabolic variable on the x-axis instead and the same
                # cloud needs x = +/-sqrt(y), which no quadratic in y can express -- so a
                # one-directional probe missed the identical data with the axes swapped.
                # Which variable a user drops on which axis is arbitrary; the warning
                # must not be.
                r2_of <- function(a, b) {
                    tryCatch(summary(stats::lm(b ~ a + I(a^2)))$r.squared,
                             error = function(e) NA_real_)
                }
                r2_quad <- suppressWarnings(max(r2_of(xx, yy), r2_of(yy, xx), na.rm = TRUE))
                if (is.finite(r2_quad) && (r2_quad - r2_lin) > 0.15) {
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
                    private$.appendWarning(.fmt(.("<b>Heavy ties in '{var}':</b> only {levels} distinct values across {n} observations. Spearman's p-value then falls back on a normal approximation rather than an exact test, and rho itself is attenuated by the ties. For a variable with this few levels, a test designed for ordered categories is usually the better choice."),
                        var = htmltools::htmlEscape(tied_var),
                        levels = k_distinct, n = n))
                }
            }

            invisible(NULL)
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
                "<li>", .("Tick <strong>Statistical results</strong> to print the coefficient on the plot."), "</li>",
                "</ol></td>",
                "<td style='width: 50%; border: 1px solid rgba(128, 128, 128, 0.4); padding: 15px; vertical-align: top;'>",
                "<h4 style='margin: 0 0 10px 0; font-size: 15px;'>", .("What you will get"), "</h4>",
                "<ul style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                "<li>", .("<strong>Scatter plot</strong> with a fitted trend line and confidence band"), "</li>",
                "<li>", .("<strong>Correlation coefficient</strong> with p-value and confidence interval"), "</li>",
                "<li>", .("<strong>Per-group panels</strong> when a Split By variable is set"), "</li>",
                "<li>", .("<strong>Marginal distributions</strong> on both axes"), "</li>",
                "<li>", .("<strong>Publication-ready output</strong> in journal colour palettes"), "</li>",
                "</ul></td></tr></table>",

                "<div style='background-color: rgba(155, 155, 155, 0.06); border: 1px solid rgba(128, 128, 128, 0.4); padding: 15px; color: inherit;'>",
                "<h4 style='margin: 0 0 10px 0; font-size: 15px;'>", .("Choosing a test"), "</h4>",
                "<ul style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                "<li>", .("<strong>Parametric (Pearson):</strong> linear relationship, roughly normal data."), "</li>",
                "<li>", .("<strong>Nonparametric (Spearman):</strong> any monotonic relationship; ordinal scores, skewed lab values."), "</li>",
                "<li>", .("<strong>Robust (Winsorized Pearson):</strong> pulls extreme values in before computing r; use when outliers dominate."), "</li>",
                "<li>", .("<strong>Bayesian:</strong> reports a Bayes factor quantifying evidence strength."), "</li>",
                "<li>", .("Correlation is not causation, and a coefficient assumes the relationship has the shape the test looks for - always read the scatter itself."), "</li>",
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
            private$.appendWarning(.fmt(.("<b>Shape mapping skipped:</b> '{var}' has {levels} levels and ggplot2 provides only 6 distinct shapes. Mapping it would have drawn no point at all for the surplus levels while the correlation still used every case. Use colour for a variable with this many levels, or group the rare categories."),
                var = htmltools::htmlEscape(self$options$shapevar), levels = n))
        },

        # A journal palette needs something to colour. The ungrouped ggpubr panel plots
        # one cloud, so there is no discrete scale: jco, npg and lancet rendered
        # byte-identical output. Say so rather than leave a control that does nothing.
        .warnGGPubrPalette = function() {
            if (!isTRUE(private$.option("addGGPubrPlot"))) return(invisible(NULL))
            if (!is.null(self$options$grvar)) return(invisible(NULL))
            if (identical(private$.option("ggpubrPalette"), "jco")) return(invisible(NULL))
            private$.appendWarning(.("<b>Colour palette not applied:</b> the publication-ready panel draws a single ungrouped set of points, so there is no grouping for a journal palette to colour. Set a 'Split By' variable to see the palette take effect."))
        },

        # Assemble the near-identical preset-notification HTML from a single
        # template. `title` and `items` are hardcoded literals (no user input),
        # so no HTML escaping is required.
        .presetMessage = function(title, items) {
            paste0(
                "<div style='background-color: rgba(33, 152, 239, 0.13); border-left:4px solid #2196F3; padding:15px; margin:10px 0; color: inherit;'>",
                # No explicit colour: #1976D2 is a mid blue that loses contrast against the dark
                # theme's panel. The 4px blue left border already carries the "info" cue.
                "<h4 style='margin-top:0;'> Clinical Preset Applied: ", title, "</h4>",
                "<p><strong>The following settings have been automatically configured:</strong></p>",
                "<ul>", paste0(items, collapse = ""), "</ul>",
                "<p style='margin-bottom:0;'><em>You can modify these settings manually or select 'Custom' preset.</em></p>",
                "</div>"
            )
        },

        # Clinical presets record their settings in private$overrides (below) instead of
        # writing back into the read-only jamovi option objects; downstream code reads the
        # effective value via private$.option(name). (2026-07-13 audit fix.)
        .applyClinicalPreset = function() {
            # Reset any overrides from a previous run so a stale preset cannot
            # leak into a subsequent "custom" run if the R6 instance is reused.
            private$overrides <- list()

            preset <- self$options$clinicalPreset
            if (preset == "custom") {
                return()
            }

            # CRITICAL FIX: Make preset mutations transparent with warnings
            preset_message <- NULL

            if (preset == "biomarker_correlation") {
                preset_message <- private$.presetMessage(
                    "Biomarker Correlation",
                    c(
                        "<li>Statistical test: <strong>Nonparametric (Spearman correlation)</strong></li>"
                    )
                )
                private$overrides[["typestatistics"]] <- "nonparametric"
                # The ggpubr panel and its palette are NOT set here, and the banner no longer
                # claims them. `overrides` is runtime R6 state, whereas the panel's
                # visibility -- `visible: (addGGPubrPlot)` in the .r.yaml -- is evaluated by
                # jamovi against the OPTIONS object, which the override never touches.
                # Verified: with this preset selected, results$ggpubrPlot$visible was FALSE
                # while the banner announced "ggpubr scatter plot enabled". The palette
                # override was independently inert (see .plotGGPubr). Tick 'Publication-ready
                # plot (ggpubr)' in the UI to get that panel.

            } else if (preset == "treatment_response_analysis") {
                preset_message <- private$.presetMessage(
                    "Treatment Response Analysis",
                    c(
                        "<li>Statistical test: <strong>Robust (Winsorized Pearson correlation)</strong></li>",
                        "<li>Marginal distributions: <strong>Enabled</strong></li>"
                    )
                )
                private$overrides[["typestatistics"]] <- "robust"
                private$overrides[["marginal"]] <- TRUE

            } else if (preset == "publication_ready") {
                preset_message <- private$.presetMessage(
                    "Publication Ready",
                    c(
                        "<li>Theme: <strong>Original ggstatsplot theme</strong></li>",
                        "<li>Results subtitle: <strong>Enabled (shows statistics on plot)</strong></li>"
                    )
                )
                private$overrides[["originaltheme"]] <- TRUE
                private$overrides[["resultssubtitle"]] <- TRUE
            }

            # Display preset notification
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
                    "parametric" = "The Pearson correlation coefficient (r) measures the strength and direction of the <strong>linear</strong> relationship between the two variables. The p-value indicates the statistical significance of the correlation.",
                    "nonparametric" = "Spearman's rho measures the strength and direction of the <strong>monotonic (rank-based)</strong> relationship between the two variables, and does not assume a linear relationship or normally distributed data. The p-value indicates the statistical significance of the association.",
                    "robust" = "The robust (Winsorized Pearson) correlation coefficient measures the association between the two variables while <strong>down-weighting the influence of outliers</strong>. The p-value indicates the statistical significance of the association.",
                    "bayes" = "The Bayesian analysis reports a Bayes factor quantifying the <strong>strength of evidence</strong> for or against an association between the two variables, alongside the estimated correlation.",
                    "The correlation coefficient measures the strength and direction of the relationship between the two variables. The p-value indicates the statistical significance of the correlation."
                )

                self$results$explanations$setVisible(TRUE)
                self$results$explanations$setContent(
                    paste0(
                        "<h3>Explanations</h3>
                    <p>
                        This scatter plot shows the relationship between two continuous variables. ",
                        method_text,
                        "
                    </p>"
                    )
                )
            }
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

        # plot ----

        .plot = function(image, ggtheme, theme, ...) {
            # Seed the sampling-based paths (Bayesian MCMC, bootstrap CIs) so a
            # re-render of an unchanged analysis reports the same numbers.
            withr::local_seed(private$.STOCHASTIC_SEED)


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

            # Function arguments
            .args <- list(
                data = plotData,
                x = self$options$dep,
                y = self$options$group,
                type = private$.option("typestatistics"),
                title = title,
                xlab = xtitle,
                ylab = ytitle,
                results.subtitle = private$.option("resultssubtitle"),
                conf.level = self$options$conflevel,
                bf.message = self$options$bfmessage,
                # ggscatterstats 1.0.0 has NO `method`, `formula`, `k`, `marginal.type`,
                # `xfill` or `yfill` formals -- those names land in `...` and are silently
                # discarded. Worse, overwriting `smooth.line.args` with a two-element list
                # DELETED the package default, which is
                #   list(linewidth = 1.5, color = "blue", method = "lm", formula = y ~ x)
                # so geom_smooth() was left with method = NULL and fell back to LOESS. Net
                # effect: the default "Linear Model (lm)" drew a loess curve, and all three
                # smoothMethod values behaved identically. Verified against the installed
                # ggstatsplot. The correct homes are smooth.line.args / digits /
                # xsidehistogram.args / ysidehistogram.args.
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
                .args$xsidehistogram.args <- list(fill = self$options$xsidefill)
                .args$ysidehistogram.args <- list(fill = self$options$ysidefill)
            }

            plot <- do.call(ggstatsplot::ggscatterstats, .args)

            if (self$options$showRugPlot) {
                plot <- plot + ggplot2::geom_rug(alpha = 0.5)
            }

            if (!private$.option("originaltheme")) {
                plot <- plot + ggtheme
            } else {
                plot <- plot + ggstatsplot::theme_ggstatsplot()
            }

            print(plot)
            TRUE
        },

        # plot2 ----

        .plot2 = function(image, ggtheme, theme, ...) {
            # Seed the sampling-based paths (Bayesian MCMC, bootstrap CIs) so a
            # re-render of an unchanged analysis reports the same numbers.
            withr::local_seed(private$.STOCHASTIC_SEED)


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

            # Call grouped_ggscatterstats with proper NSE handling
            # Use rlang::expr to create the call with symbols
            plot_call <- rlang::expr(
                ggstatsplot::grouped_ggscatterstats(
                    data = plotData,
                    x = !!rlang::sym(self$options$dep),
                    y = !!rlang::sym(self$options$group),
                    grouping.var = !!rlang::sym(self$options$grvar),
                    type = !!private$.option("typestatistics"),
                    xlab = !!xtitle,
                    ylab = !!ytitle,
                    results.subtitle = !!private$.option("resultssubtitle"),
                    conf.level = !!self$options$conflevel,
                    bf.message = !!self$options$bfmessage,
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
                    )
                )
            )

            # Add marginal options if needed
            if (private$.option("marginal")) {
                plot_call <- rlang::expr(
                    ggstatsplot::grouped_ggscatterstats(
                        data = plotData,
                        x = !!rlang::sym(self$options$dep),
                        y = !!rlang::sym(self$options$group),
                        grouping.var = !!rlang::sym(self$options$grvar),
                        type = !!private$.option("typestatistics"),
                        xlab = !!xtitle,
                        ylab = !!ytitle,
                        results.subtitle = !!private$.option("resultssubtitle"),
                        conf.level = !!self$options$conflevel,
                        bf.message = !!self$options$bfmessage,
                        digits = !!self$options$k,
                        marginal = !!private$.option("marginal"),
                        xsidehistogram.args = !!list(fill = self$options$xsidefill),
                        ysidehistogram.args = !!list(fill = self$options$ysidefill),
                        point.args = !!list(
                            size = self$options$pointsize,
                            alpha = self$options$pointalpha
                        ),
                        smooth.line.args = !!list(
                            method = self$options$smoothMethod,
                            formula = smooth_formula,
                            linewidth = self$options$smoothlinesize,
                            color = self$options$smoothlinecolor
                        )
                    )
                )
            }

            # Safety note: `plot_call` is built with `rlang::expr()` and
            # `!!` quasiquotation. The function name
            # (`ggstatsplot::grouped_ggscatterstats`) is hardcoded. User
            # inputs flow only into argument-VALUE positions: column-name
            # strings via `rlang::sym()` (which become symbols bound at
            # evaluation time to columns of `plotData`, not function
            # calls), numeric/bool options, and free-text titles passed
            # as character literals. No user string lands in a
            # function-name position.
            plot <- eval(plot_call)

            # `title.prefix` is NOT a formal of grouped_ggscatterstats (nor of
            # ggscatterstats): names(formals()) is data/.../grouping.var/plotgrid.args/
            # annotation.args, so the title landed in `...` and was discarded -- the Title
            # option was completely inert on this plot (byte-identical png with and without
            # it) and the grouped figure carried no title at all. patchwork titles it.
            plot <- plot + patchwork::plot_annotation(title = title)

            if (self$options$showRugPlot) {
                # `&`, not `+`: grouped_ggscatterstats returns a patchwork, and `+` adds the
                # layer to the LAST panel only. Measured on a 4-level grouping variable:
                # `+` gave layer counts 4,4,4,5 while `&` gave 5,5,5,5.
                plot <- plot & ggplot2::geom_rug(alpha = 0.5)
            }

            if (!private$.option("originaltheme")) {
                plot <- plot & ggtheme
            } else {
                plot <- plot & ggstatsplot::theme_ggstatsplot()
            }

            print(plot)
            TRUE
        },

        # plot3 - Enhanced scatter with multiple aesthetics ----

        .plot3 = function(image, ggtheme, theme, ...) {
            # Seed the sampling-based paths (Bayesian MCMC, bootstrap CIs) so a
            # re-render of an unchanged analysis reports the same numbers.
            withr::local_seed(private$.STOCHASTIC_SEED)


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

            # Add points with aesthetics
            if (length(point_aes) > 0) {
                p <- p + ggplot2::geom_point(
                    mapping = do.call(ggplot2::aes, point_aes)
                )
            } else {
                p <- p + ggplot2::geom_point(
                    size = self$options$pointsize,
                    alpha = self$options$pointalpha
                )
            }

            # Add smooth line with selected method
            smooth_method <- switch(
                self$options$smoothMethod,
                "lm" = "lm",
                "loess" = "loess",
                "gam" = "gam",
                "lm"
            )

            # Only "gam" needs an explicit spline formula; otherwise the default
            # y ~ x linear form is used. Without this a method="gam" fit collapses
            # to a straight line (identical to lm).
            smooth_formula <- if (identical(smooth_method, "gam")) {
                y ~ s(x, bs = "cs")
            } else {
                y ~ x
            }

            p <- p + ggplot2::geom_smooth(
                method = smooth_method,
                formula = smooth_formula,
                se = TRUE,
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

            # Correlation annotation. This used to run stats::cor.test() behind a
            # switch that silently DOWNGRADED two of the four analysis types: "robust"
            # fell back to Spearman ("robust unavailable" -- while WRS2 has been in
            # Imports all along) and "bayes" fell back to Pearson. The panel therefore
            # reported a different statistic from the main plot directly above it,
            # under a label that admitted it only in parentheses.
            #
            # statsExpressions::corr_test is the engine ggscatterstats itself calls, so
            # routing through it makes this subtitle agree with the main panel BY
            # CONSTRUCTION for all four types. Verified on 80 points (seed 42):
            # parametric r = 0.6682502, nonparametric rho = 0.6274965, robust
            # (Winsorized Pearson) 0.5893926, bayes r = 0.6503592 / BF10 = 8.08e+08.
            tryCatch({
                test_type <- private$.option("typestatistics")

                # Guard against degenerate input: correlation is undefined with
                # fewer than 3 complete pairs or a constant (zero-variance) axis.
                # is.finite(), not complete.cases(), which counts Inf as present.
                x_vals <- plotData[[self$options$dep]]
                y_vals <- plotData[[self$options$group]]
                complete <- is.finite(x_vals) & is.finite(y_vals)
                n_complete <- sum(complete)
                degenerate <- n_complete < 3 ||
                    length(unique(x_vals[complete])) < 2 ||
                    length(unique(y_vals[complete])) < 2

                if (degenerate) {
                    # No panel message here: .run() already emits a strictly better one for
                    # exactly this condition (it names the offending variable). This branch
                    # used to setContent() the whole Warnings output, DELETING the
                    # multiplicity note and the run-level degenerate note and replacing them
                    # with a vaguer sentence.
                    p <- p + ggplot2::labs(subtitle = .("Correlation not computed (insufficient data)"))
                } else {
                    # rlang::inject + sym(): corr_test is tidy-eval, so a column name held
                    # in a variable must be spliced as a symbol. Works for names with
                    # spaces and punctuation (verified on "Tumor Size" / "Overall Time").
                    res <- as.data.frame(rlang::inject(statsExpressions::corr_test(
                        data       = plotData,
                        x          = !!rlang::sym(self$options$dep),
                        y          = !!rlang::sym(self$options$group),
                        type       = test_type,
                        conf.level = self$options$conflevel
                    )))

                    dp <- max(0L, min(5L, as.integer(self$options$k)))
                    # Symbol per method: the coefficient was printed as "r" whatever ran,
                    # so a Spearman result was labelled with Pearson's symbol.
                    cor_symbol <- switch(test_type,
                                         nonparametric = "rho",
                                         "r")
                    est <- sprintf(paste0("%s = %.", dp, "f"), cor_symbol,
                                   as.numeric(res$estimate))

                    # Bayesian output has no p-value; it reports a Bayes factor instead.
                    tail_txt <- if (identical(test_type, "bayes") && !is.null(res$bf10)) {
                        sprintf(paste0("BF10 = %.", dp, "g"), as.numeric(res$bf10))
                    } else {
                        pv <- as.numeric(res$p.value)
                        sprintf(paste0("p %s %.", max(dp, 3L), "f"),
                                if (pv < 0.001) "<" else "=",
                                if (pv < 0.001) 0.001 else pv)
                    }

                    n_used <- if (!is.null(res$n.obs)) as.integer(res$n.obs) else n_complete
                    p <- p + ggplot2::labs(subtitle = paste0(
                        as.character(res$method)[1], ": ", est, ", ", tail_txt,
                        ", n = ", n_used))
                }
            }, error = function(e) {
                # If correlation fails, continue without it. Appended, not setContent():
                # a bare setContent() here wiped every other notice off the panel.
                # htmlEscape e$message since cor.test errors may include column-name fragments
                private$.appendWarning(.fmt(.("Correlation calculation failed: {reason}"),
                    reason = htmltools::htmlEscape(e$message)))
            })

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

            # See .ggpubrSafeNames: ggpubr parses the column name as R code.
            sn <- private$.ggpubrSafeNames(mydata)
            mydata <- sn$data

            # Build scatter plot arguments
            args <- list(
                data = mydata,
                x = sn$safe[[dep]],
                y = sn$safe[[group]],
                palette = private$.option("ggpubrPalette")
            )

            # A journal palette needs something to colour. This panel plots one ungrouped
            # cloud, so there is no discrete scale and the palette cannot show: jco, npg and
            # lancet rendered byte-identical output. Say so rather than leave a control that
            # visibly does nothing. The grouped panel below (which colours by the Split By
            # variable) does honour it.
            # Notice emitted from .run(); see .warnGGPubrPalette.

            # CRITICAL FIX: Implement ggpubrAddSmooth option
            # Build the 'add' parameter based on user selections
            # ggpubr::ggscatter 'add' argument only accepts a single string in some versions
            # So we handle multiple elements by adding them manually
            add_element <- NULL

            if (self$options$ggpubrAddCorr) {
                add_element <- "reg.line"
                args$conf.int <- TRUE
                args$cor.coef <- TRUE
                args$cor.method <- self$options$ggpubrCorrMethod
                # ggpubr's stat_cor prints the FIRST element of its cor.coef.name default
                # ("R") whatever method ran, so a Spearman panel was labelled with Pearson's
                # symbol. The override reaches stat_cor through `cor.coeff.args`, NOT as a
                # top-level ggscatter argument: passing cor.coef.name = "rho" directly still
                # rendered italic(R) (measured), because ggscatter's `...` never reaches
                # stat_cor.
                args$cor.coeff.args <- list(
                    cor.coef.name = switch(self$options$ggpubrCorrMethod,
                                           pearson = "R", spearman = "rho", "R"))
            } else if (self$options$ggpubrAddSmooth) {
                # Only set loess here if reg.line is NOT set
                add_element <- "loess"
                args$conf.int <- TRUE # Add CI for loess too if it's the only one
            }

            # Set the add argument if we have one
            if (!is.null(add_element)) {
                args$add <- add_element
            }

            # Create scatter plot
            plot <- do.call(ggpubr::ggscatter, args)

            # If BOTH are selected, we need to add loess manually since we used reg.line for 'add'
            if (self$options$ggpubrAddCorr && self$options$ggpubrAddSmooth) {
                plot <- plot + ggplot2::geom_smooth(method = "loess", se = TRUE)
            }

            # Restore the labels: the plot was built against sanitised column names, and
            # these panels previously ignored the Title / X-Title / Y-Title options
            # entirely, so a custom axis label set for the main plot silently did not
            # apply here. .resolveLabels() falls back to the variable names when blank.
            lbl <- private$.resolveLabels(includeGrvar = FALSE)
            plot <- plot + ggplot2::labs(title = lbl$title, x = lbl$xtitle, y = lbl$ytitle)

            # Apply theme
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

            # Prepare data (see .plotGGPubr: ordinal columns need toNumeric here too)
            mydata <- private$.prepData()
            dep <- self$options$dep
            group <- self$options$group
            grvar <- self$options$grvar

            # See .ggpubrSafeNames: ggpubr parses the column name as R code. This panel
            # feeds the name into `color` and `facet.by` as well, so all three go through.
            sn <- private$.ggpubrSafeNames(mydata)
            mydata <- sn$data

            # Build scatter plot arguments with faceting
            args <- list(
                data = mydata,
                x = sn$safe[[dep]],
                y = sn$safe[[group]],
                # `color = grvar` is what makes `palette` mean anything: ggpubr applies a
                # discrete palette to a colour/fill scale, and without a mapping there is no
                # scale to apply it to. Verified before this change: the jco, npg and lancet
                # palettes produced BYTE-IDENTICAL png output.
                color = sn$safe[[grvar]],
                palette = private$.option("ggpubrPalette"),
                facet.by = sn$safe[[grvar]]
            )

            # CRITICAL FIX: Implement ggpubrAddSmooth option
            # Build the 'add' parameter based on user selections
            # ggpubr::ggscatter 'add' argument only accepts a single string in some versions
            # So we handle multiple elements by adding them manually
            add_element <- NULL

            if (self$options$ggpubrAddCorr) {
                add_element <- "reg.line"
                args$conf.int <- TRUE
                args$cor.coef <- TRUE
                args$cor.method <- self$options$ggpubrCorrMethod
                # ggpubr's stat_cor prints the FIRST element of its cor.coef.name default
                # ("R") whatever method ran, so a Spearman panel was labelled with Pearson's
                # symbol. The override reaches stat_cor through `cor.coeff.args`, NOT as a
                # top-level ggscatter argument: passing cor.coef.name = "rho" directly still
                # rendered italic(R) (measured), because ggscatter's `...` never reaches
                # stat_cor.
                args$cor.coeff.args <- list(
                    cor.coef.name = switch(self$options$ggpubrCorrMethod,
                                           pearson = "R", spearman = "rho", "R"))
            } else if (self$options$ggpubrAddSmooth) {
                # Only set loess here if reg.line is NOT set
                add_element <- "loess"
                args$conf.int <- TRUE # Add CI for loess too if it's the only one
            }

            # Set the add argument if we have one
            if (!is.null(add_element)) {
                args$add <- add_element
            }

            # Create scatter plot
            plot <- do.call(ggpubr::ggscatter, args)

            # If BOTH are selected, we need to add loess manually since we used reg.line for 'add'
            if (self$options$ggpubrAddCorr && self$options$ggpubrAddSmooth) {
                plot <- plot + ggplot2::geom_smooth(method = "loess", se = TRUE)
            }

            # Restore the labels (see .plotGGPubr); the legend keeps the real grvar name.
            lbl <- private$.resolveLabels(includeGrvar = TRUE)
            plot <- plot + ggplot2::labs(title = lbl$title, x = lbl$xtitle,
                                         y = lbl$ytitle, color = grvar)

            # Apply theme
            plot <- plot + ggpubr::theme_pubr()

            print(plot)
            TRUE
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

            # Build the argument list in option-declaration order.
            #
            # Every variable-name option (dep, group, grvar, colorvar, sizevar,
            # shapevar, alphavar, labelvar) is emitted as a deparse()'d string
            # literal. deparse() produces valid, fully-escaped R for names
            # containing spaces, quotes or backslashes (e.g. `Tumor Grade`);
            # jmvcore's default sourcify would emit these as bare, unquoted
            # symbols and yield invalid syntax. Detecting OptionVariable by class
            # (rather than by name) means any variable option added later is
            # escaped automatically.
            #
            # data/dep/group are NOT re-emitted through private$.asArgs() - doing
            # so previously duplicated dep and group in the generated syntax (the
            # "double variables" bug). All non-variable options keep jmvcore's
            # per-option sourcify so formatting stays consistent with jamovi.
            args <- character(0)
            for (option in private$.options$options) {
                if (option$name == 'data')
                    next
                if (inherits(option, 'OptionVariable')) {
                    val <- option$value
                    if (!is.null(val))
                        args <- c(args, paste0(option$name, ' = ',
                                               paste0(deparse(val), collapse = '')))
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
