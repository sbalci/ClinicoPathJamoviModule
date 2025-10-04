#' @title Scatter Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#'

jjscatterstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjscatterstatsClass",
    inherit = jjscatterstatsBase,
    private = list(

        # init ----

        .init = function() {

            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 600
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450

            self$results$plot$setSize(plotwidth, plotheight)

            if (!is.null(self$options$grvar)) {
                mydata <- self$data
                grvar <-  self$options$grvar
                num_levels <- nlevels(as.factor(mydata[[grvar]]))
                self$results$plot2$setSize(num_levels * plotwidth, plotheight)
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

            # Initial Message ----
            if ( is.null(self$options$dep) || is.null(self$options$group)) {

                # todo ----
                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Scatter Plot with correlation analysis.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggscatterstats.html' target='_blank'>ggscatterstats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggscatterstats.html' target='_blank'>grouped_ggscatterstats</a>.
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
                    stop('Data contains no (complete) rows')
            }
        },

        # plot ----

        .plot = function(image, ggtheme, theme, ...) {

            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            plotData <- self$data

            plotData[[self$options$dep]] <- jmvcore::toNumeric(plotData[[self$options$dep]])
            plotData[[self$options$group]] <- jmvcore::toNumeric(plotData[[self$options$group]])

            # Prepare arguments for ggscatterstats
            if (!is.null(self$options$mytitle) && self$options$mytitle != "") {
                title <- jmvcore::format(self$options$mytitle)
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

            # Function arguments
            .args <- list(
                data = plotData,
                x = self$options$dep,
                y = self$options$group,
                type = self$options$typestatistics,
                title = title,
                xlab = xtitle,
                ylab = ytitle,
                results.subtitle = self$options$resultssubtitle,
                conf.level = self$options$conflevel,
                bf.message = self$options$bfmessage,
                k = self$options$k,
                marginal = self$options$marginal,
                marginal.type = "histogram",
                point.size = self$options$pointsize,
                point.alpha = self$options$pointalpha,
                smooth.line.args = list(
                    size = self$options$smoothlinesize,
                    color = self$options$smoothlinecolor
                )
            )

            if (self$options$marginal) {
                .args$xfill <- self$options$xsidefill
                .args$yfill <- self$options$ysidefill
            }

            plot <- do.call(ggstatsplot::ggscatterstats, .args)

            if (!self$options$originaltheme) {
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
            if (!is.null(self$options$mytitle) && self$options$mytitle != "") {
                title <- jmvcore::format(self$options$mytitle)
            } else {
                title <- paste(self$options$dep, "vs", self$options$group, "by", self$options$grvar)
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

            # Call grouped_ggscatterstats with proper NSE handling
            # Use rlang::expr to create the call with symbols
            plot_call <- rlang::expr(
                ggstatsplot::grouped_ggscatterstats(
                    data = plotData,
                    x = !!rlang::sym(self$options$dep),
                    y = !!rlang::sym(self$options$group),
                    grouping.var = !!rlang::sym(self$options$grvar),
                    type = !!self$options$typestatistics,
                    title.prefix = !!title,
                    xlab = !!xtitle,
                    ylab = !!ytitle,
                    results.subtitle = !!self$options$resultssubtitle,
                    conf.level = !!self$options$conflevel,
                    bf.message = !!self$options$bfmessage,
                    k = !!self$options$k,
                    marginal = !!self$options$marginal,
                    marginal.type = "histogram",
                    point.size = !!self$options$pointsize,
                    point.alpha = !!self$options$pointalpha,
                    smooth.line.args = !!list(
                        size = self$options$smoothlinesize,
                        color = self$options$smoothlinecolor
                    )
                )
            )

            # Add marginal options if needed
            if (self$options$marginal) {
                plot_call <- rlang::expr(
                    ggstatsplot::grouped_ggscatterstats(
                        data = plotData,
                        x = !!rlang::sym(self$options$dep),
                        y = !!rlang::sym(self$options$group),
                        grouping.var = !!rlang::sym(self$options$grvar),
                        type = !!self$options$typestatistics,
                        title.prefix = !!title,
                        xlab = !!xtitle,
                        ylab = !!ytitle,
                        results.subtitle = !!self$options$resultssubtitle,
                        conf.level = !!self$options$conflevel,
                        bf.message = !!self$options$bfmessage,
                        k = !!self$options$k,
                        marginal = !!self$options$marginal,
                        marginal.type = "histogram",
                        xfill = !!self$options$xsidefill,
                        yfill = !!self$options$ysidefill,
                        point.size = !!self$options$pointsize,
                        point.alpha = !!self$options$pointalpha,
                        smooth.line.args = !!list(
                            size = self$options$smoothlinesize,
                            color = self$options$smoothlinecolor
                        )
                    )
                )
            }

            # Evaluate the call
            plot <- eval(plot_call)

            if (!self$options$originaltheme) {
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
            if (!is.null(self$options$mytitle) && self$options$mytitle != "") {
                title <- jmvcore::format(self$options$mytitle)
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

            # Build base aesthetic mapping
            aes_mapping <- ggplot2::aes_string(
                x = self$options$dep,
                y = self$options$group
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

            p <- p + ggplot2::geom_smooth(
                method = smooth_method,
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
                    label_aes <- ggplot2::aes_string(label = self$options$labelvar)
                    p <- p + ggrepel::geom_text_repel(
                        mapping = label_aes,
                        size = 3,
                        max.overlaps = 10
                    )
                }
            }

            # Add correlation annotation
            tryCatch({
                cor_result <- stats::cor.test(
                    plotData[[self$options$dep]],
                    plotData[[self$options$group]],
                    method = switch(
                        self$options$typestatistics,
                        "parametric" = "pearson",
                        "nonparametric" = "spearman",
                        "pearson"
                    )
                )

                cor_text <- sprintf(
                    "r = %.3f, p %s %.3f",
                    cor_result$estimate,
                    ifelse(cor_result$p.value < 0.001, "<", "="),
                    ifelse(cor_result$p.value < 0.001, 0.001, cor_result$p.value)
                )

                p <- p + ggplot2::labs(subtitle = cor_text)
            }, error = function(e) {
                # If correlation fails, continue without it
            })

            # Add labels
            p <- p + ggplot2::labs(
                title = title,
                x = xtitle,
                y = ytitle
            )

            # Apply theme
            if (!self$options$originaltheme) {
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
    )
)