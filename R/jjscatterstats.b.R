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
                        "<li>Statistical test: <strong>Nonparametric (Spearman correlation)</strong></li>",
                        "<li>Additional plot: <strong>ggpubr scatter plot enabled</strong></li>",
                        "<li>Color palette: <strong>JCO (Journal of Clinical Oncology)</strong></li>"
                    )
                )
                private$overrides[["typestatistics"]] <- "nonparametric"
                private$overrides[["addGGPubrPlot"]] <- TRUE
                private$overrides[["ggpubrPalette"]] <- "jco"

            } else if (preset == "treatment_response_analysis") {
                preset_message <- private$.presetMessage(
                    "Treatment Response Analysis",
                    c(
                        "<li>Statistical test: <strong>Robust (trimmed mean correlation)</strong></li>",
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
                    "robust" = "The robust (percentage-bend) correlation coefficient measures the association between the two variables while <strong>down-weighting the influence of outliers</strong>. The p-value indicates the statistical significance of the association.",
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
                k = self$options$k,
                marginal = private$.option("marginal"),
                marginal.type = self$options$marginalType,
                point.args = list(
                    size = self$options$pointsize,
                    alpha = self$options$pointalpha
                ),
                method = self$options$smoothMethod,  # Wire smoothMethod
                formula = smooth_formula,            # GAM needs an explicit spline formula
                smooth.line.args = list(
                    linewidth = self$options$smoothlinesize,
                    color = self$options$smoothlinecolor
                )
            )

            if (private$.option("marginal")) {
                .args$xfill <- self$options$xsidefill
                .args$yfill <- self$options$ysidefill
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
                    k = !!self$options$k,
                    marginal = !!private$.option("marginal"),
                    marginal.type = !!self$options$marginalType,  # CRITICAL FIX: Use actual option value
                    point.args = !!list(
                        size = self$options$pointsize,
                        alpha = self$options$pointalpha
                    ),
                    method = !!self$options$smoothMethod, # Wire smoothMethod
                    formula = !!smooth_formula,           # GAM needs an explicit spline formula
                    smooth.line.args = !!list(
                        linewidth = self$options$smoothlinesize,
                        color = self$options$smoothlinecolor
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
                        k = !!self$options$k,
                        marginal = !!private$.option("marginal"),
                        marginal.type = !!self$options$marginalType, # Correctly use option
                        xfill = !!self$options$xsidefill,
                        yfill = !!self$options$ysidefill,
                        point.args = !!list(
                            size = self$options$pointsize,
                            alpha = self$options$pointalpha
                        ),
                        method = !!self$options$smoothMethod,
                        formula = !!smooth_formula,       # GAM needs an explicit spline formula
                        smooth.line.args = !!list(
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

        # plot3 - Enhanced scatter with multiple aesthetics ----

        .plot3 = function(image, ggtheme, theme, ...) {

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
                    current_warnings <- self$results$warnings$state
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

                    cor_text <- sprintf(
                        "%s: r = %.3f, p %s %.3f",
                        method_label,
                        cor_result$estimate,
                        ifelse(cor_result$p.value < 0.001, "<", "="),
                        ifelse(cor_result$p.value < 0.001, 0.001, cor_result$p.value)
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
