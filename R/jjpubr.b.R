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
            private$.applyClinicalPreset()
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

            if (self$options$showExplanations) {
                private$.generateExplanations()
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

            # Comprehensive data type validation
            tryCatch({
                # Validate X variable type based on plot type
                x_data <- self$data[[self$options$xvar]]

                # Plot types requiring categorical X
                categorical_x_plots <- c("boxplot", "violin", "barplot", "dotplot")
                # Plot types requiring numeric X
                numeric_x_plots <- c("scatter", "histogram", "density", "line", "errorplot")

                if (self$options$plotType %in% categorical_x_plots) {
                    if (!is.factor(x_data) && !is.character(x_data)) {
                        # Check if it's numeric with few unique values (possible patient IDs, etc.)
                        if (is.numeric(x_data)) {
                            n_unique <- length(unique(x_data[!is.na(x_data)]))
                            stop(paste0(
                                "X variable '", self$options$xvar, "' is numeric (", n_unique, " unique values) but ",
                                self$options$plotType, " requires a categorical variable.\n\n",
                                "⚠️ WARNING: Using numeric IDs (e.g., patient IDs) as categorical groups produces ",
                                "meaningless statistical comparisons.\n\n",
                                "💡 Solution: Convert numeric codes to factors or select a proper grouping variable ",
                                "(e.g., treatment group, disease stage)."
                            ))
                        }
                    }
                } else if (self$options$plotType %in% numeric_x_plots) {
                    if (!is.numeric(x_data)) {
                        # Allow conversion for histogram/density if it's a factor with numeric levels
                        if (self$options$plotType %in% c("histogram", "density")) {
                            stop(paste0(
                                "X variable '", self$options$xvar, "' must be numeric for ",
                                self$options$plotType, " plots. Current type: ", class(x_data)[1]
                            ))
                        }
                    }
                }

                # Validate Y variable type (must be numeric for all plots that use it)
                if (!is.null(self$options$yvar)) {
                    y_data <- self$data[[self$options$yvar]]
                    if (!is.numeric(y_data)) {
                        stop(paste0(
                            "Y variable '", self$options$yvar, "' must be numeric for ",
                            self$options$plotType, " plots. Current type: ", class(y_data)[1], "\n\n",
                            "💡 Solution: Select a continuous numeric variable (e.g., biomarker level, ",
                            "measurement value, score)."
                        ))
                    }
                }

                # Validate scatter plot specifically (both X and Y must be numeric)
                if (self$options$plotType == "scatter") {
                    if (!is.numeric(x_data)) {
                        stop(paste0(
                            "Scatter plots require both X and Y to be numeric.\n",
                            "X variable '", self$options$xvar, "' is ", class(x_data)[1], " (must be numeric).\n\n",
                            "💡 Solution: Select continuous numeric variables for both axes."
                        ))
                    }
                    if (!is.numeric(self$data[[self$options$yvar]])) {
                        stop(paste0(
                            "Scatter plots require both X and Y to be numeric.\n",
                            "Y variable '", self$options$yvar, "' is ", class(self$data[[self$options$yvar]])[1],
                            " (must be numeric).\n\n",
                            "💡 Solution: Select continuous numeric variables for both axes."
                        ))
                    }
                }

                # Validate grouping variable (must be categorical if specified)
                if (!is.null(self$options$groupvar)) {
                    group_data <- self$data[[self$options$groupvar]]
                    if (!is.factor(group_data) && !is.character(group_data)) {
                        warning(paste0(
                            "Grouping variable '", self$options$groupvar, "' is numeric. ",
                            "Converting to factor for visualization. Ensure this is appropriate."
                        ))
                    }
                }

                return(TRUE)

            }, error = function(e) {
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent(paste0(
                    "<div style='padding: 20px; background: #fff3cd; border: 2px solid #ff6b6b;'>",
                    "<h3>❌ Data Validation Error</h3>",
                    "<p><strong>", e$message, "</strong></p>",
                    "</div>"))
                return(FALSE)
            })
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

            tryCatch({
                x_col <- self$options$xvar
                y_col <- self$options$yvar
                x_data <- self$data[[x_col]]
                y_data <- self$data[[y_col]]

                # Validate that both variables are numeric
                if (!is.numeric(x_data)) {
                    stop(paste0(
                        "Correlation requires numeric variables. ",
                        "X variable '", x_col, "' is ", class(x_data)[1], " (must be numeric)."
                    ))
                }

                if (!is.numeric(y_data)) {
                    stop(paste0(
                        "Correlation requires numeric variables. ",
                        "Y variable '", y_col, "' is ", class(y_data)[1], " (must be numeric)."
                    ))
                }

                # Get complete cases for correlation analysis
                complete_idx <- complete.cases(x_data, y_data)
                n_total <- length(x_data)
                n_complete <- sum(complete_idx)

                if (n_total > n_complete) {
                    warning(paste0(
                        "Correlation: ", n_total - n_complete, " observations (",
                        round(100 * (n_total - n_complete) / n_total, 1),
                        "%) excluded due to missing values. ",
                        "Correlation is based on n=", n_complete, " complete pairs."
                    ))
                }

                if (n_complete < 3) {
                    stop("Insufficient data for correlation analysis (n < 3 after removing missing values)")
                }

                # Compute correlation on complete cases only
                x_complete <- x_data[complete_idx]
                y_complete <- y_data[complete_idx]

                # IMPORTANT: Use complete data, not self$data with use="complete.obs"
                # This ensures the correlation matches the displayed scatter plot
                cor_result <- cor.test(
                    x_complete,
                    y_complete,
                    method = self$options$corrMethod
                )

                # Check if faceting is used (correlation may not match plot)
                if (!is.null(self$options$facetvar)) {
                    warning(paste0(
                        "Note: Correlation is computed on the entire dataset (n=", n_complete, "). ",
                        "When using faceting, correlations may differ within each facet. ",
                        "Consider running separate analyses for each facet level."
                    ))
                }

                self$results$correlation$addRow(rowKey = 1, values = list(
                    method = paste0(tools::toTitleCase(self$options$corrMethod), " (n=", n_complete, ")"),
                    coefficient = cor_result$estimate,
                    pvalue = cor_result$p.value,
                    ci_lower = if (!is.null(cor_result$conf.int)) cor_result$conf.int[1] else NA,
                    ci_upper = if (!is.null(cor_result$conf.int)) cor_result$conf.int[2] else NA
                ))

            }, error = function(e) {
                warning(paste("Correlation analysis failed:", e$message))
            })
        },

        .populateStatistics = function() {
            if (!self$options$addStats) return()
            if (!(self$options$plotType %in% c("boxplot", "violin"))) return()

            tryCatch({
                # Get complete data for statistical analysis
                x_col <- self$options$xvar
                y_col <- self$options$yvar

                # Remove NAs explicitly for transparent sample size reporting
                complete_data <- self$data[complete.cases(self$data[[x_col]], self$data[[y_col]]), ]
                n_total <- nrow(self$data)
                n_complete <- nrow(complete_data)

                if (n_total > n_complete) {
                    warning(paste0(
                        "Statistical tests: ", n_total - n_complete, " rows (",
                        round(100 * (n_total - n_complete) / n_total, 1),
                        "%) excluded due to missing values. ",
                        "Tests are based on n=", n_complete, " complete observations."
                    ))
                }

                if (n_complete < 3) {
                    stop("Insufficient data for statistical testing (n < 3 after removing missing values)")
                }

                groups <- unique(complete_data[[x_col]])
                groups <- groups[!is.na(groups)]
                if (length(groups) < 2) return()

                table <- self$results$statistics
                method <- self$options$statMethod
                if (method == "auto") method <- if (length(groups) == 2) "t.test" else "anova"

                # Validate sample sizes by group
                group_ns <- sapply(groups, function(g) {
                    sum(complete_data[[x_col]] == g)
                })

                min_n <- min(group_ns)
                if (min_n < 3) {
                    warning(paste0(
                        "Small sample size detected: minimum group has n=", min_n, ". ",
                        "Statistical tests may be unreliable. Consider using nonparametric tests."
                    ))
                }

                # Pairwise comparisons with multiple testing correction
                if (self$options$pairwiseComparisons && length(groups) > 2) {
                    comps <- combn(groups, 2, simplify = FALSE)
                    n_comparisons <- length(comps)

                    if (n_comparisons > 1) {
                        warning(paste0(
                            "Multiple testing: ", n_comparisons, " pairwise comparisons. ",
                            "Consider Bonferroni correction (significance threshold: ",
                            round(0.05 / n_comparisons, 4), ") or use p.adjust() for ",
                            "multiple testing correction."
                        ))
                    }

                    p_values <- numeric(n_comparisons)

                    for (i in seq_along(comps)) {
                        g1 <- comps[[i]][1]
                        g2 <- comps[[i]][2]
                        data1 <- complete_data[[y_col]][complete_data[[x_col]] == g1]
                        data2 <- complete_data[[y_col]][complete_data[[x_col]] == g2]

                        # Check group-specific sample sizes
                        n1 <- length(data1)
                        n2 <- length(data2)

                        if (n1 < 2 || n2 < 2) {
                            stop(paste0("Insufficient sample size for comparison ", g1, " vs ", g2))
                        }

                        # Perform test based on method
                        test_result <- if (method %in% c("t.test", "auto")) {
                            # Check variance homogeneity for t-test
                            if (n1 >= 3 && n2 >= 3) {
                                var_test <- var.test(data1, data2)
                                if (var_test$p.value < 0.05) {
                                    warning(paste0(
                                        "Comparison ", g1, " vs ", g2, ": Unequal variances detected (p=",
                                        round(var_test$p.value, 4), "). Using Welch t-test."
                                    ))
                                }
                            }
                            t.test(data1, data2, na.action = na.omit)
                        } else if (method == "wilcox.test") {
                            wilcox.test(data1, data2, na.action = na.omit, exact = FALSE)
                        } else if (method == "anova") {
                            # For pairwise ANOVA, use t-test
                            t.test(data1, data2, na.action = na.omit)
                        } else if (method == "kruskal.test") {
                            # For pairwise Kruskal, use Wilcoxon
                            wilcox.test(data1, data2, na.action = na.omit, exact = FALSE)
                        } else {
                            t.test(data1, data2, na.action = na.omit)
                        }

                        p_values[i] <- test_result$p.value

                        # Apply Bonferroni correction for display
                        p_adj <- p_values[i] * n_comparisons
                        p_adj <- min(p_adj, 1.0)  # Cap at 1.0

                        sig <- if (p_values[i] < 0.001) "***" else
                               if (p_values[i] < 0.01) "**" else
                               if (p_values[i] < 0.05) "*" else "ns"

                        sig_adj <- if (p_adj < 0.001) "***" else
                                   if (p_adj < 0.01) "**" else
                                   if (p_adj < 0.05) "*" else "ns"

                        table$addRow(rowKey = i, values = list(
                            comparison = paste(g1, "(n=", n1, ") vs", g2, "(n=", n2, ")"),
                            statistic = test_result$statistic,
                            pvalue = p_values[i],
                            significance = paste0(sig, " (adj: ", sig_adj, ")")
                        ))
                    }
                } else if (!self$options$pairwiseComparisons && length(groups) == 2) {
                    # Single comparison between two groups
                    g1 <- groups[1]
                    g2 <- groups[2]
                    data1 <- complete_data[[y_col]][complete_data[[x_col]] == g1]
                    data2 <- complete_data[[y_col]][complete_data[[x_col]] == g2]

                    n1 <- length(data1)
                    n2 <- length(data2)

                    test_result <- if (method %in% c("t.test", "auto")) {
                        # Check variance homogeneity
                        if (n1 >= 3 && n2 >= 3) {
                            var_test <- var.test(data1, data2)
                            if (var_test$p.value < 0.05) {
                                warning(paste0(
                                    "Unequal variances detected (p=", round(var_test$p.value, 4),
                                    "). Using Welch t-test."
                                ))
                            }
                        }
                        t.test(data1, data2, na.action = na.omit)
                    } else {
                        wilcox.test(data1, data2, na.action = na.omit, exact = FALSE)
                    }

                    sig <- if (test_result$p.value < 0.001) "***" else
                           if (test_result$p.value < 0.01) "**" else
                           if (test_result$p.value < 0.05) "*" else "ns"

                    table$addRow(rowKey = 1, values = list(
                        comparison = paste(g1, "(n=", n1, ") vs", g2, "(n=", n2, ")"),
                        statistic = test_result$statistic,
                        pvalue = test_result$p.value,
                        significance = sig
                    ))
                }
            }, error = function(e) {
                warning(paste("Statistical testing failed:", e$message))
            })
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
        },

        .applyClinicalPreset = function() {
            preset <- self$options$clinicalPreset
            if (preset == "custom") return()

            # Track which settings will be overridden
            overrides <- list()

            switch(preset,
                "prognostic_biomarker" = {
                    if (self$options$plotType != "boxplot") {
                        overrides <- c(overrides, paste0("Plot type changed from '", self$options$plotType, "' to 'boxplot'"))
                    }
                    if (!self$options$addStats) {
                        overrides <- c(overrides, "Statistical comparisons enabled")
                    }
                    if (self$options$statMethod != "auto") {
                        overrides <- c(overrides, paste0("Statistical method changed from '", self$options$statMethod, "' to 'auto'"))
                    }

                    self$options$plotType <- "boxplot"
                    self$options$addStats <- TRUE
                    self$options$statMethod <- "auto"
                },
                "diagnostic_test" = {
                    if (self$options$plotType != "boxplot") {
                        overrides <- c(overrides, paste0("Plot type changed from '", self$options$plotType, "' to 'boxplot'"))
                    }
                    if (!self$options$addStats) {
                        overrides <- c(overrides, "Statistical comparisons enabled")
                    }
                    if (self$options$statMethod != "t.test") {
                        overrides <- c(overrides, paste0("Statistical method changed from '", self$options$statMethod, "' to 't.test'"))
                    }

                    self$options$plotType <- "boxplot"
                    self$options$addStats <- TRUE
                    self$options$statMethod <- "t.test"
                },
                "correlation_analysis" = {
                    if (self$options$plotType != "scatter") {
                        overrides <- c(overrides, paste0("Plot type changed from '", self$options$plotType, "' to 'scatter'"))
                    }
                    if (!self$options$addCorr) {
                        overrides <- c(overrides, "Correlation statistics enabled")
                    }
                    if (self$options$corrMethod != "pearson") {
                        overrides <- c(overrides, paste0("Correlation method changed from '", self$options$corrMethod, "' to 'pearson'"))
                    }

                    self$options$plotType <- "scatter"
                    self$options$addCorr <- TRUE
                    self$options$corrMethod <- "pearson"
                }
            )

            # Display warning if settings were overridden
            if (length(overrides) > 0) {
                preset_name <- tools::toTitleCase(gsub("_", " ", preset))
                warning(paste0(
                    "\n⚠️ CLINICAL PRESET OVERRIDE WARNING ⚠️\n\n",
                    "The '", preset_name, "' preset has automatically changed the following settings:\n",
                    paste0("  • ", overrides, collapse = "\n"), "\n\n",
                    "These changes override your manual selections. In regulated clinical settings, ",
                    "ensure these preset configurations are appropriate for your analysis and documented ",
                    "in your protocol.\n\n",
                    "To use custom settings instead, select 'Custom' from the Clinical Preset dropdown.\n"
                ))
            }
        },

        .generateExplanations = function() {
            plot_type <- self$options$plotType
            preset <- self$options$clinicalPreset

            explanation <- "<h3>Clinical Explanations</h3>"

            # General explanation based on plot type
            explanation <- paste0(explanation, "<h4>", tools::toTitleCase(plot_type), "</h4>")
            explanation <- paste0(explanation, "<p>", switch(plot_type,
                "boxplot" = "Box plots are useful for comparing the distribution of a continuous variable between groups. The box represents the interquartile range (IQR), the line inside the box is the median, and the whiskers extend to 1.5 times the IQR.",
                "violin" = "Violin plots are similar to box plots, but they also show the probability density of the data at different values. This can be useful for visualizing the distribution of the data.",
                "scatter" = "Scatter plots are used to visualize the relationship between two continuous variables. A correlation coefficient can be added to quantify the strength and direction of the relationship.",
                "histogram" = "Histograms are used to visualize the distribution of a single continuous variable. The height of each bar represents the number of observations that fall within that bin.",
                "density" = "Density plots are a smoothed version of a histogram. They are useful for visualizing the underlying distribution of the data.",
                "barplot" = "Bar plots are used to visualize the distribution of a categorical variable. The height of each bar represents the number of observations in that category.",
                "dotplot" = "Dot plots are similar to bar plots, but they show the individual data points. This can be useful for visualizing the distribution of the data.",
                "line" = "Line plots are used to visualize the trend of a continuous variable over time or another continuous variable.",
                "errorplot" = "Error plots are used to visualize the mean and confidence interval of a continuous variable."
            ), "</p>")

            # Explanation based on clinical preset
            if (preset != "custom") {
                explanation <- paste0(explanation, "<h4>Clinical Preset: ", tools::toTitleCase(gsub("_", " ", preset)), "</h4>")
                explanation <- paste0(explanation, "<p>", switch(preset,
                    "prognostic_biomarker" = "This preset is designed for analyzing the relationship between a prognostic biomarker and a categorical outcome. The box plot allows for a quick comparison of the biomarker levels between the different outcome groups. The statistical test will help to determine if there is a significant difference in the biomarker levels between the groups.",
                    "diagnostic_test" = "This preset is designed for evaluating the performance of a diagnostic test. The box plot allows for a quick comparison of the test results between the diseased and non-diseased groups. The t-test will help to determine if there is a significant difference in the test results between the groups.",
                    "correlation_analysis" = "This preset is designed for analyzing the correlation between two continuous variables. The scatter plot allows for a quick visualization of the relationship between the two variables. The Pearson correlation coefficient will quantify the strength and direction of the linear relationship."
                ), "</p>")
            }

            self$results$plotInfo$setContent(explanation)
        }
    )
)
