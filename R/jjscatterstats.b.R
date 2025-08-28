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

            # Function arguments
            .args <- list(
                data = plotData,
                x = self$options$dep,
                y = self$options$group,
                grouping.var = self$options$grvar,
                type = self$options$typestatistics,
                title.prefix = title,
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

            plot <- do.call(ggstatsplot::grouped_ggscatterstats, .args)

            if (!self$options$originaltheme) {
                plot <- plot + ggplot2::theme_bw()
            } else {
                plot <- plot + ggstatsplot::theme_ggstatsplot()
            }

            print(plot)
            TRUE
        }
    )
)