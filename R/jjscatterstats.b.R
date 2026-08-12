#' @title Scatter Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
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
                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Scatter Plot with correlation analysis.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://www.indrapatil.com/ggstatsplot/reference/ggscatterstats.html' target='_blank'>ggscatterstats</a> and <a href = 'https://www.indrapatil.com/ggstatsplot/reference/grouped_ggscatterstats.html' target='_blank'>grouped_ggscatterstats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)
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
            private$.generateExplanations()

            # grouped_ggscatterstats() has no p-adjustment argument: each facet's correlation
            # is tested at the nominal level and the raw p is printed on that panel. With k
            # groups that is k tests. Reproduced on histopathology (Age x OverallTime by
            # Grade): raw 0.3015 / 0.0281 / 0.2069, Holm-adjusted 0.4138 / 0.0842 / 0.4138.
            # No displayed number is wrong; the omission is the family it belongs to.
            if (!is.null(self$options$grvar) && isTRUE(private$.option("resultssubtitle"))) {
                n_groups <- length(unique(stats::na.omit(self$data[[self$options$grvar]])))
                if (n_groups > 1)
                    private$.appendWarning(paste0(
                        " <b>One test per group, unadjusted.</b> Each of the ", n_groups,
                        " panels shows its own correlation tested at the nominal level, with ",
                        "no correction for the fact that ", n_groups, " tests are being read ",
                        "together. If you are screening groups rather than testing one ",
                        "pre-specified comparison, adjust the p-values yourself (Holm or FDR ",
                        "over the ", n_groups, " values) before drawing conclusions."))
            }

            # Degenerate-data check. The same test already existed in .plot3, but .plot3 only
            # renders when an aesthetic mapping is set (its visible: expression), so the
            # MAIN plot -- the default output everyone sees -- had no guard at all. Verified:
            # a constant dep variable rendered a scatter plot with no warning, while the same
            # data through .plot3 correctly reported "Correlation not computed". Running the
            # check in .run() means it fires whichever plots are on screen.
            x_vals <- self$data[[self$options$dep]]
            y_vals <- self$data[[self$options$group]]
            if (!is.null(x_vals) && !is.null(y_vals)) {
                complete <- stats::complete.cases(x_vals, y_vals)
                n_complete <- sum(complete)
                degenerate <- n_complete < 3 ||
                    length(unique(x_vals[complete])) < 2 ||
                    length(unique(y_vals[complete])) < 2
                if (degenerate && ("warnings" %in% self$results$itemNames)) {
                    reason <- if (n_complete < 3)
                        paste0("only ", n_complete, " complete pair",
                               if (n_complete == 1) "" else "s", " of values")
                    else if (length(unique(x_vals[complete])) < 2)
                        paste0("'", self$options$dep, "' takes the same value in every row")
                    else
                        paste0("'", self$options$group, "' takes the same value in every row")
                    current <- self$results$warnings$content
                    if (is.null(current)) current <- ""
                    self$results$warnings$setContent(paste0(current,
                        "<p style='color:#856404;'> <b>Correlation not computed:</b> ", reason,
                        ". A correlation needs at least 3 complete pairs and variation in both ",
                        "variables; the plot below shows the points but no coefficient is ",
                        "meaningful.</p>"))
                    self$results$warnings$setVisible(TRUE)
                }
            }
        },

        # Assemble the near-identical preset-notification HTML from a single
        # template. `title` and `items` are hardcoded literals (no user input),
        # so no HTML escaping is required.
        .presetMessage = function(title, items) {
            paste0(
                "<div style='background:#e3f2fd; border-left:4px solid #2196F3; padding:15px; margin:10px 0;'>",
                "<h4 style='color:#1976D2; margin-top:0;'> Clinical Preset Applied: ", title, "</h4>",
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
                title <- jmvcore::format(self$options$mytitle)
            } else if (includeGrvar) {
                title <- paste(self$options$dep, "vs", self$options$group, "by", self$options$grvar)
            } else {
                title <- paste(self$options$dep, "vs", self$options$group)
            }

            if (!is.null(self$options$xtitle) && self$options$xtitle != "") {
                xtitle <- jmvcore::format(self$options$xtitle)
            } else {
                xtitle <- self$options$dep
            }

            if (!is.null(self$options$ytitle) && self$options$ytitle != "") {
                ytitle <- jmvcore::format(self$options$ytitle)
            } else {
                ytitle <- self$options$group
            }

            list(title = title, xtitle = xtitle, ytitle = ytitle)
        },

        # plot ----

        .plot = function(image, ggtheme, theme, ...) {
            # Seed the sampling-based paths (Bayesian MCMC, bootstrap CIs) so a
            # re-render of an unchanged analysis reports the same numbers.
            withr::local_seed(private$.STOCHASTIC_SEED)


            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            plotData <- self$data

            plotData[[self$options$dep]] <- jmvcore::toNumeric(plotData[[self$options$dep]])
            plotData[[self$options$group]] <- jmvcore::toNumeric(plotData[[self$options$group]])

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
                plot <- plot + ggplot2::theme_bw()
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

            plotData <- self$data

            plotData[[self$options$dep]] <- jmvcore::toNumeric(plotData[[self$options$dep]])
            plotData[[self$options$group]] <- jmvcore::toNumeric(plotData[[self$options$group]])

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
                    title.prefix = !!title,
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
                        title.prefix = !!title,
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

            if (self$options$showRugPlot) {
                # `&`, not `+`: grouped_ggscatterstats returns a patchwork, and `+` adds the
                # layer to the LAST panel only. Measured on a 4-level grouping variable:
                # `+` gave layer counts 4,4,4,5 while `&` gave 5,5,5,5.
                plot <- plot & ggplot2::geom_rug(alpha = 0.5)
            }

            if (!private$.option("originaltheme")) {
                plot <- plot & ggplot2::theme_bw()
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

            plotData <- self$data

            # Convert variables to numeric
            plotData[[self$options$dep]] <- jmvcore::toNumeric(plotData[[self$options$dep]])
            plotData[[self$options$group]] <- jmvcore::toNumeric(plotData[[self$options$group]])

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
                n_shape_levels <- length(unique(stats::na.omit(
                    plotData[[self$options$shapevar]])))
                if (n_shape_levels > 6) {
                    private$.appendWarning(paste0(
                        " <b>Shape mapping skipped:</b> '",
                        htmltools::htmlEscape(self$options$shapevar), "' has ",
                        n_shape_levels, " levels and ggplot2 provides only 6 distinct ",
                        "shapes. Mapping it would have drawn no point at all for the ",
                        "surplus levels while the correlation still used every case. ",
                        "Use colour for a variable with this many levels, or group the ",
                        "rare categories."))
                } else {
                    point_aes$shape <- rlang::sym(self$options$shapevar)
                }
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

            # CRITICAL FIX: Add correlation annotation with proper method handling
            tryCatch({
                test_type <- private$.option("typestatistics")
                cor_method <- "pearson"  # Default
                method_label <- "Pearson"
                warning_msg <- NULL

                if (test_type == "parametric") {
                    cor_method <- "pearson"
                    method_label <- "Pearson"
                } else if (test_type == "nonparametric") {
                    cor_method <- "spearman"
                    method_label <- "Spearman"
                } else if (test_type == "robust") {
                    # Robust correlation requires special packages
                    if (requireNamespace("WRS2", quietly = TRUE)) {
                        # Could use WRS2::pbcor for robust correlation
                        # For now, fall back to Spearman with warning
                        cor_method <- "spearman"
                        method_label <- "Spearman (robust unavailable)"
                        warning_msg <- paste0(
                            " Robust correlation not fully implemented for enhanced plot. ",
                            "Falling back to Spearman correlation. ",
                            "For robust analysis, use the main ggstatsplot plot (plot 1)."
                        )
                    } else {
                        cor_method <- "pearson"
                        method_label <- "Pearson (robust unavailable)"
                        warning_msg <- paste0(
                            " Robust correlation requires WRS2 package which is not available. ",
                            "Falling back to Pearson correlation."
                        )
                    }
                } else if (test_type == "bayes" || test_type == "bayesian") {
                    # Bayesian correlation requires BayesFactor package
                    cor_method <- "pearson"
                    method_label <- "Pearson (Bayesian unavailable)"
                    warning_msg <- paste0(
                        " Bayesian correlation not implemented for enhanced plot. ",
                        "Falling back to Pearson correlation. ",
                        "For Bayesian analysis, use the main ggstatsplot plot (plot 1)."
                    )
                } else {
                    # Unknown method, default to Pearson with warning
                    cor_method <- "pearson"
                    method_label <- "Pearson (default)"
                    warning_msg <- paste0(
                        " Unknown correlation method '", test_type, "'. ",
                        "Falling back to Pearson correlation."
                    )
                }

                # Show warning if method was changed.
                # The `warnings` Html output is declared in .r.yaml; the itemNames
                # probe is retained as a defensive guard (jmvcore throws rather than
                # returning NULL on access to an undefined results item).
                if (!is.null(warning_msg) && ("warnings" %in% self$results$itemNames)) {
                    # $content, not $state: setContent() writes $content and never
                    # populates $state, so reading $state always yielded "" and each
                    # warning silently replaced the one before it.
                    current_warnings <- self$results$warnings$content
                    if (is.null(current_warnings)) {
                        current_warnings <- ""
                    }
                    new_warning <- paste0(
                        current_warnings,
                        "<p style='color:#856404;'>", warning_msg, "</p>"
                    )
                    self$results$warnings$setContent(new_warning)
                    self$results$warnings$setVisible(TRUE)
                }

                # Guard against degenerate input: correlation is undefined with
                # fewer than 3 complete pairs or a constant (zero-variance) axis.
                x_vals <- plotData[[self$options$dep]]
                y_vals <- plotData[[self$options$group]]
                complete <- stats::complete.cases(x_vals, y_vals)
                n_complete <- sum(complete)
                degenerate <- n_complete < 3 ||
                    length(unique(x_vals[complete])) < 2 ||
                    length(unique(y_vals[complete])) < 2

                if (degenerate) {
                    if ("warnings" %in% self$results$itemNames) {
                        insufficient_msg <- paste0(
                            "<p style='color:#856404;'>Correlation not computed: ",
                            "insufficient or degenerate data (need at least 3 complete ",
                            "pairs and non-constant x and y).</p>"
                        )
                        self$results$warnings$setContent(insufficient_msg)
                        self$results$warnings$setVisible(TRUE)
                    }
                    p <- p + ggplot2::labs(subtitle = "Correlation not computed (insufficient data)")
                } else {
                    cor_result <- stats::cor.test(
                        x_vals,
                        y_vals,
                        method = cor_method
                    )

                    # Symbol per method: the coefficient was printed as "r" whatever ran,
                    # so a Spearman result was labelled with Pearson's symbol. Honour the
                    # user's decimal-places option here too (it was hard-coded to 3), and
                    # report n -- the panel gave a coefficient and a p-value with no
                    # denominator anywhere in the analysis.
                    cor_symbol <- switch(cor_method,
                                         pearson  = "r",
                                         spearman = "rho",
                                         kendall  = "tau",
                                         "r")
                    dp <- max(0L, min(5L, as.integer(self$options$k)))
                    cor_text <- sprintf(
                        paste0("%s: %s = %.", dp, "f, p %s %.", max(dp, 3L), "f, n = %d"),
                        method_label,
                        cor_symbol,
                        cor_result$estimate,
                        ifelse(cor_result$p.value < 0.001, "<", "="),
                        ifelse(cor_result$p.value < 0.001, 0.001, cor_result$p.value),
                        n_complete
                    )

                    p <- p + ggplot2::labs(subtitle = cor_text)
                }
            }, error = function(e) {
                # If correlation fails, continue without it
                # htmlEscape e$message since cor.test errors may include column-name fragments
                if ("warnings" %in% self$results$itemNames) {
                    warning_msg <- paste0(" Correlation calculation failed: ", htmltools::htmlEscape(e$message))
                    self$results$warnings$setContent(warning_msg)
                    self$results$warnings$setVisible(TRUE)
                }
            })

            # Add labels
            p <- p + ggplot2::labs(
                title = title,
                x = xtitle,
                y = ytitle
            )

            # Apply theme
            if (!private$.option("originaltheme")) {
                p <- p + ggplot2::theme_bw()
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
                paste0(current, "<p style='color:#856404;'>", html, "</p>"))
            self$results$warnings$setVisible(TRUE)
            invisible(NULL)
        },

        .warnCorrMethodMismatch = function() {
            if (!isTRUE(self$options$ggpubrAddCorr)) return(invisible(NULL))
            implied <- switch(as.character(private$.option("typestatistics")),
                              parametric = "pearson", nonparametric = "spearman", NULL)
            if (is.null(implied)) return(invisible(NULL))          # robust/bayes: no analogue
            chosen <- self$options$ggpubrCorrMethod
            if (identical(implied, chosen)) return(invisible(NULL))

            msg <- paste0(
                " The publication-ready panel is reporting a <b>", chosen,
                "</b> correlation while the main plot reports <b>", implied,
                "</b>, because 'Correlation method (ggpubr)' is set independently of ",
                "'Statistical Test Type'. Both coefficients are correct for this data, but ",
                "they are different statistics - set the two to match unless you intend to ",
                "show both."
            )
            if ("warnings" %in% self$results$itemNames) {
                current <- self$results$warnings$content
                if (is.null(current)) current <- ""
                if (!grepl("Correlation method (ggpubr)", current, fixed = TRUE)) {
                    self$results$warnings$setContent(
                        paste0(current, "<p style='color:#856404;'>", msg, "</p>"))
                    self$results$warnings$setVisible(TRUE)
                }
            }
            invisible(NULL)
        },

        .plotGGPubr = function(image, ...) {
            # Validate inputs
            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            # Skip if ggpubr plot not requested
            if (!private$.option("addGGPubrPlot"))
                return()

            # Prepare data
            mydata <- self$data
            dep <- self$options$dep
            group <- self$options$group

            # Build scatter plot arguments
            args <- list(
                data = mydata,
                x = dep,
                y = group,
                palette = private$.option("ggpubrPalette")
            )

            # A journal palette needs something to colour. This panel plots one ungrouped
            # cloud, so there is no discrete scale and the palette cannot show: jco, npg and
            # lancet rendered byte-identical output. Say so rather than leave a control that
            # visibly does nothing. The grouped panel below (which colours by the Split By
            # variable) does honour it.
            if (is.null(self$options$grvar) &&
                !identical(private$.option("ggpubrPalette"), "jco")) {
                private$.appendWarning(paste0(
                    " <b>Colour palette not applied:</b> the publication-ready panel draws a ",
                    "single ungrouped set of points, so there is no grouping for a journal ",
                    "palette to colour. Set a 'Split By' variable to see the palette take ",
                    "effect."))
            }

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
                private$.warnCorrMethodMismatch()
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

            # Prepare data
            mydata <- self$data
            dep <- self$options$dep
            group <- self$options$group
            grvar <- self$options$grvar

            # Build scatter plot arguments with faceting
            args <- list(
                data = mydata,
                x = dep,
                y = group,
                # `color = grvar` is what makes `palette` mean anything: ggpubr applies a
                # discrete palette to a colour/fill scale, and without a mapping there is no
                # scale to apply it to. Verified before this change: the jco, npg and lancet
                # palettes produced BYTE-IDENTICAL png output.
                color = grvar,
                palette = private$.option("ggpubrPalette"),
                facet.by = grvar
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
                private$.warnCorrMethodMismatch()
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
