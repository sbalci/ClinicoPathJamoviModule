#' @title Publication-Ready Plots using ggpubr
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @export

jjpubrClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjpubrClass",
    inherit = jjpubrBase,
    private = list(

        # === Initialization ===
        .init = function() {
            if (is.null(self$options$xvar)) {
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent(private$.generateWelcomeMessage())
                return()
            }

            self$results$todo$setVisible(FALSE)

            # Set plot dimensions
            image <- self$results$plot
            image$setSize(self$options$plotWidth, self$options$plotHeight)
        },

        # === Main Run Function ===
        .run = function() {
            if (!private$.validateInputs()) return()

            if (!requireNamespace("ggpubr", quietly = TRUE)) {
                stop("The 'ggpubr' package is required. Install with: install.packages('ggpubr')")
            }

            private$.generatePlotInfo()
            private$.populateDescriptives()
            private$.populateCorrelation()
            private$.populateStatistics()
        },

        # === Validation ===
        .validateInputs = function() {
            if (is.null(self$options$xvar)) return(FALSE)

            requires_y <- c("boxplot", "violin", "scatter", "line", "errorplot", "dotplot")
            if (self$options$plotType %in% requires_y && is.null(self$options$yvar)) {
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent(paste0(
                    "<div style='padding: 20px;'><h3>Y Variable Required</h3>",
                    "<p>The ", self$options$plotType, " plot requires a Y variable.</p></div>"))
                return(FALSE)
            }

            return(TRUE)
        },

        # === Plot Generation ===
        .plot = function(image, ggtheme, theme, ...) {
            if (!private$.validateInputs()) return(FALSE)

            p <- tryCatch({
                switch(self$options$plotType,
                    boxplot = private$.createBoxPlot(),
                    violin = private$.createViolinPlot(),
                    scatter = private$.createScatterPlot(),
                    histogram = private$.createHistogram(),
                    density = private$.createDensityPlot(),
                    barplot = private$.createBarPlot(),
                    dotplot = private$.createDotPlot(),
                    line = private$.createLinePlot(),
                    errorplot = private$.createErrorPlot(),
                    stop("Unknown plot type"))
            }, error = function(e) {
                stop("Error creating plot: ", e$message)
            })

            if (is.null(p)) return(FALSE)
            p <- private$.applyTheme(p)
            print(p)
            return(TRUE)
        },

        # === BOX PLOT ===
        .createBoxPlot = function() {
            # Build arguments list conditionally
            args <- list(
                data = self$data,
                x = self$options$xvar,
                y = self$options$yvar,
                add = if (self$options$addPoints) "jitter" else NULL,
                add.params = list(alpha = self$options$pointAlpha),
                title = if (nchar(self$options$title) > 0) self$options$title else NULL,
                xlab = if (nchar(self$options$xlab) > 0) self$options$xlab else self$options$xvar,
                ylab = if (nchar(self$options$ylab) > 0) self$options$ylab else self$options$yvar,
                legend = self$options$legendPosition,
                facet.by = self$options$facetvar
            )

            # Add color/fill only if groupvar is specified
            if (!is.null(self$options$groupvar)) {
                args$color <- self$options$groupvar
                args$fill <- self$options$groupvar
                args$palette <- private$.getPalette()
            } else {
                args$fill <- self$options$fillColor
                args$color <- "black"
            }

            p <- do.call(ggpubr::ggboxplot, args)

            if (self$options$addMeanSD) {
                p <- p + stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white")
            }

            if (self$options$addStats) {
                p <- private$.addStatisticalComparisons(p)
            }

            return(p)
        },

        # === VIOLIN PLOT ===
        .createViolinPlot = function() {
            args <- list(
                data = self$data,
                x = self$options$xvar,
                y = self$options$yvar,
                add = if (self$options$addPoints) "jitter" else "boxplot",
                add.params = list(alpha = self$options$pointAlpha),
                title = if (nchar(self$options$title) > 0) self$options$title else NULL,
                xlab = if (nchar(self$options$xlab) > 0) self$options$xlab else self$options$xvar,
                ylab = if (nchar(self$options$ylab) > 0) self$options$ylab else self$options$yvar,
                legend = self$options$legendPosition,
                facet.by = self$options$facetvar
            )

            if (!is.null(self$options$groupvar)) {
                args$color <- self$options$groupvar
                args$fill <- self$options$groupvar
                args$palette <- private$.getPalette()
            } else {
                args$fill <- self$options$fillColor
                args$color <- "black"
            }

            p <- do.call(ggpubr::ggviolin, args)

            if (self$options$addStats) {
                p <- private$.addStatisticalComparisons(p)
            }

            return(p)
        },

        # === SCATTER PLOT ===
        .createScatterPlot = function() {
            p <- ggpubr::ggscatter(
                self$data,
                x = self$options$xvar,
                y = self$options$yvar,
                color = if (!is.null(self$options$groupvar)) self$options$groupvar else "black",
                palette = private$.getPalette(),
                size = 3,
                alpha = self$options$pointAlpha,
                add = if (self$options$addSmoothLine) "reg.line" else NULL,
                add.params = list(color = "blue", fill = "lightgray"),
                conf.int = self$options$addSmoothLine,
                title = if (nchar(self$options$title) > 0) self$options$title else NULL,
                xlab = if (nchar(self$options$xlab) > 0) self$options$xlab else self$options$xvar,
                ylab = if (nchar(self$options$ylab) > 0) self$options$ylab else self$options$yvar,
                legend = self$options$legendPosition,
                facet.by = self$options$facetvar
            )

            if (self$options$addCorr) {
                p <- p + ggpubr::stat_cor(method = self$options$corrMethod)
            }

            return(p)
        },

        # === HISTOGRAM ===
        .createHistogram = function() {
            p <- ggpubr::gghistogram(
                self$data,
                x = self$options$xvar,
                fill = if (!is.null(self$options$groupvar)) self$options$groupvar else self$options$fillColor,
                color = if (!is.null(self$options$groupvar)) self$options$groupvar else "black",
                palette = private$.getPalette(),
                bins = self$options$bins,
                add_density = self$options$addDensity,
                title = if (nchar(self$options$title) > 0) self$options$title else NULL,
                xlab = if (nchar(self$options$xlab) > 0) self$options$xlab else self$options$xvar,
                ylab = if (nchar(self$options$ylab) > 0) self$options$ylab else "Count",
                legend = self$options$legendPosition,
                facet.by = self$options$facetvar
            )

            if (self$options$addMean) {
                mean_val <- mean(self$data[[self$options$xvar]], na.rm = TRUE)
                p <- p + geom_vline(xintercept = mean_val, linetype = "dashed", color = "red", size = 1)
            }

            if (self$options$addMedian) {
                median_val <- median(self$data[[self$options$xvar]], na.rm = TRUE)
                p <- p + geom_vline(xintercept = median_val, linetype = "dashed", color = "blue", size = 1)
            }

            return(p)
        },

        # === DENSITY PLOT ===
        .createDensityPlot = function() {
            p <- ggpubr::ggdensity(
                self$data,
                x = self$options$xvar,
                fill = if (!is.null(self$options$groupvar)) self$options$groupvar else self$options$fillColor,
                color = if (!is.null(self$options$groupvar)) self$options$groupvar else "black",
                palette = private$.getPalette(),
                title = if (nchar(self$options$title) > 0) self$options$title else NULL,
                xlab = if (nchar(self$options$xlab) > 0) self$options$xlab else self$options$xvar,
                ylab = if (nchar(self$options$ylab) > 0) self$options$ylab else "Density",
                legend = self$options$legendPosition,
                facet.by = self$options$facetvar
            )

            if (self$options$addMean) {
                mean_val <- mean(self$data[[self$options$xvar]], na.rm = TRUE)
                p <- p + geom_vline(xintercept = mean_val, linetype = "dashed", color = "red", size = 1)
            }

            if (self$options$addMedian) {
                median_val <- median(self$data[[self$options$xvar]], na.rm = TRUE)
                p <- p + geom_vline(xintercept = median_val, linetype = "dashed", color = "blue", size = 1)
            }

            return(p)
        },

        # === BAR PLOT ===
        .createBarPlot = function() {
            p <- ggpubr::ggbarplot(
                self$data,
                x = self$options$xvar,
                y = if (!is.null(self$options$yvar)) self$options$yvar else NULL,
                fill = if (!is.null(self$options$groupvar)) self$options$groupvar else self$options$fillColor,
                color = if (!is.null(self$options$groupvar)) self$options$groupvar else "black",
                palette = private$.getPalette(),
                title = if (nchar(self$options$title) > 0) self$options$title else NULL,
                xlab = if (nchar(self$options$xlab) > 0) self$options$xlab else self$options$xvar,
                ylab = if (nchar(self$options$ylab) > 0) self$options$ylab else "Count",
                legend = self$options$legendPosition,
                facet.by = self$options$facetvar
            )
            return(p)
        },

        # === DOT PLOT ===
        .createDotPlot = function() {
            p <- ggpubr::ggdotplot(
                self$data,
                x = self$options$xvar,
                y = self$options$yvar,
                fill = if (!is.null(self$options$groupvar)) self$options$groupvar else self$options$fillColor,
                color = if (!is.null(self$options$groupvar)) self$options$groupvar else "black",
                palette = private$.getPalette(),
                title = if (nchar(self$options$title) > 0) self$options$title else NULL,
                xlab = if (nchar(self$options$xlab) > 0) self$options$xlab else self$options$xvar,
                ylab = if (nchar(self$options$ylab) > 0) self$options$ylab else self$options$yvar,
                legend = self$options$legendPosition
            )
            return(p)
        },

        # === LINE PLOT ===
        .createLinePlot = function() {
            p <- ggpubr::ggline(
                self$data,
                x = self$options$xvar,
                y = self$options$yvar,
                color = if (!is.null(self$options$groupvar)) self$options$groupvar else "black",
                palette = private$.getPalette(),
                title = if (nchar(self$options$title) > 0) self$options$title else NULL,
                xlab = if (nchar(self$options$xlab) > 0) self$options$xlab else self$options$xvar,
                ylab = if (nchar(self$options$ylab) > 0) self$options$ylab else self$options$yvar,
                legend = self$options$legendPosition,
                facet.by = self$options$facetvar
            )
            return(p)
        },

        # === ERROR PLOT ===
        .createErrorPlot = function() {
            p <- ggpubr::ggerrorplot(
                self$data,
                x = self$options$xvar,
                y = self$options$yvar,
                color = if (!is.null(self$options$groupvar)) self$options$groupvar else "black",
                palette = private$.getPalette(),
                title = if (nchar(self$options$title) > 0) self$options$title else NULL,
                xlab = if (nchar(self$options$xlab) > 0) self$options$xlab else self$options$xvar,
                ylab = if (nchar(self$options$ylab) > 0) self$options$ylab else self$options$yvar,
                legend = self$options$legendPosition
            )
            return(p)
        },

        # === HELPER FUNCTIONS ===
        .getPalette = function() {
            if (self$options$palette == "default") return(NULL)
            return(self$options$palette)
        },

        .applyTheme = function(p) {
            switch(self$options$theme,
                pubr = p + ggpubr::theme_pubr(),
                classic = p + theme_classic(),
                minimal = p + theme_minimal(),
                light = p + theme_light(),
                dark = p + theme_dark(),
                p
            )
        },

        .addStatisticalComparisons = function(p) {
            method <- if (self$options$statMethod == "auto") NULL else self$options$statMethod

            if (self$options$pairwiseComparisons) {
                comps <- private$.getPairwiseComparisons()
                p + ggpubr::stat_compare_means(method = method, label = "p.signif", comparisons = comps)
            } else {
                p + ggpubr::stat_compare_means(method = method)
            }
        },

        .getPairwiseComparisons = function() {
            levels <- unique(self$data[[self$options$xvar]])
            levels <- levels[!is.na(levels)]
            combn(levels, 2, simplify = FALSE)
        },

        # === TABLE POPULATION ===
        .populateDescriptives = function() {
            plot_types <- c("boxplot", "violin", "histogram", "density")
            if (!(self$options$plotType %in% plot_types)) return()

            y <- if (!is.null(self$options$yvar)) self$options$yvar else self$options$xvar
            group <- self$options$groupvar
            table <- self$results$descriptives

            if (is.null(group)) {
                stats <- private$.calculateDescriptives(self$data[[y]])
                table$addRow(rowKey = 1, values = list(
                    group = "Overall", n = stats$n, mean = stats$mean,
                    sd = stats$sd, median = stats$median, min = stats$min, max = stats$max))
            } else {
                groups <- unique(self$data[[group]])
                groups <- groups[!is.na(groups)]
                for (i in seq_along(groups)) {
                    subset_data <- self$data[[y]][self$data[[group]] == groups[i]]
                    stats <- private$.calculateDescriptives(subset_data)
                    table$addRow(rowKey = i, values = list(
                        group = as.character(groups[i]), n = stats$n, mean = stats$mean,
                        sd = stats$sd, median = stats$median, min = stats$min, max = stats$max))
                }
            }
        },

        .calculateDescriptives = function(x) {
            x <- x[!is.na(x)]
            list(n = length(x), mean = mean(x), sd = sd(x),
                 median = median(x), min = min(x), max = max(x))
        },

        .populateCorrelation = function() {
            if (self$options$plotType != "scatter" || !self$options$addCorr) return()

            cor_result <- cor.test(
                self$data[[self$options$xvar]],
                self$data[[self$options$yvar]],
                method = self$options$corrMethod
            )

            self$results$correlation$addRow(rowKey = 1, values = list(
                method = tools::toTitleCase(self$options$corrMethod),
                coefficient = cor_result$estimate,
                pvalue = cor_result$p.value,
                ci_lower = if (!is.null(cor_result$conf.int)) cor_result$conf.int[1] else NA,
                ci_upper = if (!is.null(cor_result$conf.int)) cor_result$conf.int[2] else NA
            ))
        },

        .populateStatistics = function() {
            if (!self$options$addStats) return()
            if (!(self$options$plotType %in% c("boxplot", "violin"))) return()

            groups <- unique(self$data[[self$options$xvar]])
            groups <- groups[!is.na(groups)]
            if (length(groups) < 2) return()

            table <- self$results$statistics
            method <- self$options$statMethod
            if (method == "auto") method <- if (length(groups) == 2) "t.test" else "anova"

            if (self$options$pairwiseComparisons && length(groups) > 2) {
                comps <- combn(groups, 2, simplify = FALSE)
                for (i in seq_along(comps)) {
                    g1 <- comps[[i]][1]
                    g2 <- comps[[i]][2]
                    data1 <- self$data[[self$options$yvar]][self$data[[self$options$xvar]] == g1]
                    data2 <- self$data[[self$options$yvar]][self$data[[self$options$xvar]] == g2]

                    test_result <- if (method %in% c("t.test", "auto")) {
                        t.test(data1, data2)
                    } else {
                        wilcox.test(data1, data2)
                    }

                    sig <- if (test_result$p.value < 0.001) "***" else
                           if (test_result$p.value < 0.01) "**" else
                           if (test_result$p.value < 0.05) "*" else "ns"

                    table$addRow(rowKey = i, values = list(
                        comparison = paste(g1, "vs", g2),
                        statistic = test_result$statistic,
                        pvalue = test_result$p.value,
                        significance = sig
                    ))
                }
            }
        },

        .generatePlotInfo = function() {
            plot_name <- switch(self$options$plotType,
                boxplot = "Box Plot", violin = "Violin Plot", scatter = "Scatter Plot",
                histogram = "Histogram", density = "Density Plot", barplot = "Bar Plot",
                dotplot = "Dot Plot", line = "Line Plot", errorplot = "Error Plot", "Plot")

            html <- paste0(
                "<div style='padding: 10px; background: #f9f9f9; border: 1px solid #ddd;'>",
                "<h4>", plot_name, "</h4>",
                "<p><strong>X:</strong> ", self$options$xvar, "</p>")

            if (!is.null(self$options$yvar))
                html <- paste0(html, "<p><strong>Y:</strong> ", self$options$yvar, "</p>")
            if (!is.null(self$options$groupvar))
                html <- paste0(html, "<p><strong>Group:</strong> ", self$options$groupvar, "</p>")

            html <- paste0(html, "<p><strong>Palette:</strong> ", self$options$palette,
                          " | <strong>Theme:</strong> ", self$options$theme, "</p></div>")

            self$results$plotInfo$setContent(html)
        },

        .generateWelcomeMessage = function() {
            paste0(
                "<div style='padding: 20px; background: #f9f9f9; border: 1px solid #ddd;'>",
                "<h3>📊 Welcome to ggpubr Plots!</h3>",
                "<p>Create publication-ready visualizations using the ggpubr package.</p>",
                "<h4>Getting Started:</h4>",
                "<ol>",
                "<li><strong>Select a plot type</strong> from the dropdown menu</li>",
                "<li><strong>Choose your X variable</strong> (and Y variable if needed)</li>",
                "<li><strong>Customize</strong> with colors, statistics, and appearance options</li>",
                "</ol>",
                "<h4>Available Plot Types:</h4>",
                "<ul>",
                "<li><strong>Box Plot:</strong> Compare distributions across groups</li>",
                "<li><strong>Violin Plot:</strong> Show full distribution shape</li>",
                "<li><strong>Scatter Plot:</strong> Visualize relationships</li>",
                "<li><strong>Histogram:</strong> Show distribution of a variable</li>",
                "<li><strong>Density Plot:</strong> Smooth distribution</li>",
                "</ul>",
                "<p style='margin-top: 15px; color: #666;'>",
                "<em>💡 Tip: Enable 'Add statistical comparisons' for automatic p-values!</em>",
                "</p></div>")
        }
    )
)
