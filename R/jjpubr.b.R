#' @title Publication-Ready Plots using ggpubr
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @export

jjpubrClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjpubrClass",
    inherit = jjpubrBase,
    private = list(
        ..stats_df = NULL,
        ..inferredMethod = NULL,
        ..assumptionReason = NULL,

        # === Helper: Escape Variable Names ===
        .escapeVarName = function(var) {
            if (is.null(var) || var == "") return(var)
            # Wrap in backticks if contains spaces or special chars
            if (grepl("[^A-Za-z0-9_.]", var)) {
                return(paste0("`", var, "`"))
            }
            return(var)
        },

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

            # Calculate statistics once for both plot and table (includes assumption checks/inference)
            private$.calculateStatistics()
            private$.checkAssumptions()  # CRITICAL: Check parametric test assumptions
            private$.populateStatistics()
            private$.generateSummary()
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
            # For x-only plot types, yvar can be NULL; ensure validation doesn't block them
            if (is.null(self$options$yvar) && !(self$options$plotType %in% c("histogram", "density", "barplot"))) {
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
                        if (self$options$plotType == "scatter") {
                            stop(paste0(
                                "Scatter plots require both X and Y to be numeric.
",
                                "Y variable '", self$options$yvar, "' is ", class(y_data)[1],
                                " (must be numeric).

",
                                "💡 Solution: Select continuous numeric variables for both axes."
                            ))
                        }
                        stop(paste0(
                            "Y variable '", self$options$yvar, "' must be numeric for ",
                            self$options$plotType, " plots. Current type: ", class(y_data)[1], "

",
                            "💡 Solution: Select a continuous numeric variable (e.g., biomarker level, ",
                            "measurement value, score)."
                        ))
                    }
                }

                # Validate scatter plot specifically (both X and Y must be numeric)
                if (self$options$plotType == "scatter") {
                    if (!is.numeric(x_data)) {
                        stop(paste0(
                            "Scatter plots require both X and Y to be numeric.
",
                            "X variable '", self$options$xvar, "' is ", class(x_data)[1], " (must be numeric).

",
                            "💡 Solution: Select continuous numeric variables for both axes."
                        ))
                    }
                }

                # Validate grouping variable (must be categorical if specified)
                if (!is.null(self$options$groupvar)) {
                    group_data <- self$data[[self$options$groupvar]]
                    if (!is.factor(group_data) && !is.character(group_data)) {
                        # notice <- jmvcore::Notice$new(
                        #     options = self$options,
                        #     name = 'numericGroupingVariable',
                        #     type = jmvcore::NoticeType$WARNING
                        # )
                        # notice$setContent(paste0(
                        #     "Grouping variable '", self$options$groupvar, "' is numeric and will be converted to factor for visualization. ",
                        #     "Ensure this is appropriate (e.g., not a continuous measurement)."
                        # ))
                        # self$results$insert(100, notice)
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
                stop(e)
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

            # Add marginal density/histogram plots if requested
            if (self$options$addMarginal) {
                if (requireNamespace("ggExtra", quietly = TRUE)) {
                    p <- ggExtra::ggMarginal(p, type = "density", fill = "lightgray", alpha = 0.5)
                } else {
                    # notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = 'ggExtraNotAvailable',
                    #     type = jmvcore::NoticeType$WARNING
                    # )
                    # notice$setContent(
                    #     "Marginal plots require the 'ggExtra' package. Install with: install.packages('ggExtra')"
                    # )
                    # self$results$insert(100, notice)
                }
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
            # Use inferred method if available
            inferred <- private$..inferredMethod %||% list(pairwise = NULL, omnibus = NULL)
            method <- inferred$pairwise
            groups <- unique(self$data[[self$options$xvar]])
            groups <- groups[!is.na(groups)]

            # Global test (ANOVA/Kruskal) if requested or default
            # We can keep the global test using stat_compare_means if desired, 
            # but for pairwise, we use the manual data.
            
            if (self$options$pairwiseComparisons) {
                if (!is.null(private$..stats_df) && nrow(private$..stats_df) > 0) {
                    # Use the pre-calculated adjusted p-values
                    p <- p + ggpubr::stat_pvalue_manual(
                        private$..stats_df, 
                        label = "p.adj.signif",
                        tip.length = 0.01
                    )
                }
            } else {
                # Default behavior for global test or simple 2-group comparison without explicit pairwise
                method_geom <- if (length(groups) > 2) inferred$omnibus else inferred$pairwise
                p <- p + ggpubr::stat_compare_means(method = method_geom)
            }
            return(p)
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
            table$setNote("na", "N counts are calculated after removing missing values in X/Y (and grouping when applicable).")
        },

        .calculateDescriptives = function(x) {
            x <- x[!is.na(x)]
            list(n = length(x), mean = mean(x), sd = sd(x),
                 median = median(x), min = min(x), max = max(x))
        },

        .inferStatMethod = function(data, x_col, y_col, n_groups) {
            base_method <- self$options$statMethod
            # Default choices
            pairwise_method <- if (n_groups == 2) "t.test" else "t.test"
            omnibus_method <- if (n_groups > 2) "anova" else NULL
            reason <- NULL

            if (base_method == "wilcox.test") {
                pairwise_method <- "wilcox.test"
                omnibus_method <- if (n_groups > 2) "kruskal.test" else NULL
                return(list(pairwise = pairwise_method, omnibus = omnibus_method, omnibus_label = ifelse(is.null(omnibus_method), "None", omnibus_method), reason = NULL))
            }

            # auto or t.test/parametric choice
            use_nonparam <- FALSE

            # Normality/variance checks for auto
            if (base_method == "auto") {
                groups <- unique(data[[x_col]])
                groups <- groups[!is.na(groups)]
                normality_failed <- FALSE
                for (g in groups) {
                    subset_data <- data[[y_col]][data[[x_col]] == g]
                    subset_data <- subset_data[!is.na(subset_data)]
                    if (length(subset_data) >= 3 && length(subset_data) <= 200) {
                        sw <- tryCatch(shapiro.test(subset_data), error = function(e) NULL)
                        if (!is.null(sw) && sw$p.value < 0.05) normality_failed <- TRUE
                    }
                }

                var_failed <- FALSE
                if (n_groups >= 2 && requireNamespace("car", quietly = TRUE)) {
                    formula_str <- paste(private$.escapeVarName(y_col), "~", private$.escapeVarName(x_col))
                    lv <- tryCatch(car::leveneTest(as.formula(formula_str), data = data, center = median), error = function(e) NULL)
                    if (!is.null(lv) && !is.na(lv$`Pr(>F)`[1]) && lv$`Pr(>F)`[1] < 0.05) var_failed <- TRUE
                }

                if (normality_failed || var_failed) {
                    use_nonparam <- TRUE
                    reason <- paste(
                        "Auto switched to nonparametric tests due to",
                        paste(c(if (normality_failed) "non-normality" else NULL, if (var_failed) "variance heterogeneity" else NULL), collapse = " and "),
                        "."
                    )
                }
            }

            if (use_nonparam) {
                pairwise_method <- "wilcox.test"
                omnibus_method <- if (n_groups > 2) "kruskal.test" else NULL
            } else {
                pairwise_method <- "t.test"
                omnibus_method <- if (n_groups > 2) "anova" else NULL
            }

            list(
                pairwise = pairwise_method,
                omnibus = omnibus_method,
                omnibus_label = ifelse(is.null(omnibus_method), "None", omnibus_method),
                reason = reason
            )
        },

        .checkAssumptions = function() {
            if (!self$options$addStats) return()
            if (!(self$options$plotType %in% c("boxplot", "violin"))) return()

            x_col <- self$options$xvar
            y_col <- self$options$yvar
            complete_data <- self$data[complete.cases(self$data[[x_col]], self$data[[y_col]]), ]
            groups <- unique(complete_data[[x_col]])
            groups <- groups[!is.na(groups)]

            # Only check if using t-test
            method <- self$options$statMethod
            if (method == "auto") method <- if (length(groups) == 2) "t.test" else "t.test"
            if (!(method %in% c("t.test", "auto"))) return()  # Skip for Wilcoxon

            # Check normality for each group
            for (g in groups) {
                subset_data <- complete_data[[y_col]][complete_data[[x_col]] == g]
                n <- length(subset_data)

                if (n >= 3 && n <= 200) {
                    # Shapiro-Wilk for small-medium samples
                    sw_test <- tryCatch({
                        shapiro.test(subset_data)
                    }, error = function(e) NULL)

                    if (!is.null(sw_test) && sw_test$p.value < 0.05) {
                        # notice <- jmvcore::Notice$new(
                        #     options = self$options,
                        #     name = paste0('normalityViolation_', make.names(as.character(g))),
                        #     type = jmvcore::NoticeType$STRONG_WARNING
                        # )
                        # notice$setContent(paste0(
                        #     "Normality assumption violated for group '", g, "' (Shapiro-Wilk p = ",
                        #     round(sw_test$p.value, 3), "). t-test results may be unreliable. ",
                        #     "Consider using 'Pairwise Wilcoxon tests' for non-normal data."
                        # ))
                        # self$results$insert(999, notice)
                    }
                } else if (n > 200) {
                    # Skewness check for large samples
                    skew <- (mean(subset_data) - median(subset_data)) / sd(subset_data)
                    if (abs(skew) > 1) {
                        # notice <- jmvcore::Notice$new(
                        #     options = self$options,
                        #     name = paste0('skewnessWarning_', make.names(as.character(g))),
                        #     type = jmvcore::NoticeType$WARNING
                        # )
                        # notice$setContent(paste0(
                        #     "Data for group '", g, "' appears skewed (skewness coefficient = ",
                        #     round(skew, 2), "). Consider using 'Pairwise Wilcoxon tests'."
                        # ))
                        # self$results$insert(100, notice)
                    }
                }
            }

            # Check variance homogeneity (Levene's test)
            if (length(groups) >= 2 && requireNamespace("car", quietly = TRUE)) {
                formula_str <- paste(private$.escapeVarName(y_col), "~", private$.escapeVarName(x_col))
                formula_obj <- as.formula(formula_str)

                levene_result <- tryCatch({
                    car::leveneTest(formula_obj, data = complete_data, center = median)
                }, error = function(e) NULL)

                if (!is.null(levene_result) && !is.na(levene_result$`Pr(>F)`[1]) && levene_result$`Pr(>F)`[1] < 0.05) {
                    # notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = 'varianceHeterogeneity',
                    #     type = jmvcore::NoticeType$WARNING
                    # )
                    # notice$setContent(paste0(
                    #     "Unequal variances detected across groups (Levene's test p = ",
                    #     round(levene_result$`Pr(>F)`[1], 3), "). ",
                    #     "Standard t-test assumes equal variances; results may be less reliable. ",
                    #     "Consider Welch correction or non-parametric alternatives."
                    # ))
                    # self$results$insert(100, notice)
                }
            }
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

                # Kendall CI notice
                if (self$options$corrMethod == "kendall") {
                    # notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = 'kendallNoCI',
                    #     type = jmvcore::NoticeType$INFO
                    # )
                    # notice$setContent(paste0(
                    #     "Kendall's tau correlation does not provide confidence intervals in the standard cor.test() implementation. ",
                    #     "The correlation coefficient and p-value are still valid. ",
                    #     "Consider using Spearman correlation if confidence intervals are needed for robust correlation analysis."
                    # ))
                    # self$results$insert(999, notice)
                }

                # Helper to compute correlation for a subset
                compute_cor <- function(idx, label) {
                    complete_idx <- complete.cases(x_data[idx], y_data[idx])
                    n_total <- length(idx)
                    n_complete <- sum(complete_idx)
                    if (n_complete < 3) {
                        return(list(values = NULL, msg = paste0(label, ": insufficient data for correlation (n<3 complete).")))
                    }
                    x_complete <- x_data[idx][complete_idx]
                    y_complete <- y_data[idx][complete_idx]
                    cor_result <- cor.test(
                        x_complete,
                        y_complete,
                        method = self$options$corrMethod
                    )
                    values <- list(
                        method = paste0(label, " - ", tools::toTitleCase(self$options$corrMethod), " (n=", n_complete, ")"),
                        coefficient = cor_result$estimate,
                        pvalue = cor_result$p.value,
                        ci_lower = if (!is.null(cor_result$conf.int)) cor_result$conf.int[1] else NA,
                        ci_upper = if (!is.null(cor_result$conf.int)) cor_result$conf.int[2] else NA
                    )
                    msg <- if (n_total > n_complete) paste0(
                        label, ": ", n_total - n_complete, " observations (",
                        round(100 * (n_total - n_complete) / n_total, 1),
                        "%) excluded due to missing values. Correlation based on n=", n_complete, " complete pairs."
                    ) else NULL
                    list(values = values, msg = msg)
                }

                # Clear previous rows
                # (tables recreate per run; nothing to clear explicitly)

                if (!is.null(self$options$groupvar)) {
                    groups <- unique(self$data[[self$options$groupvar]])
                    groups <- groups[!is.na(groups)]
                    if (length(groups) > 0) {
                        for (g in groups) {
                            idx <- which(self$data[[self$options$groupvar]] == g)
                            res <- compute_cor(idx, paste0("Group: ", g))
                            if (!is.null(res$values)) {
                                self$results$correlation$addRow(rowKey = paste0("group_", g), values = res$values)
                            }
                            if (!is.null(res$msg)) {
                                # notice <- jmvcore::Notice$new(
                                #     options = self$options,
                                #     name = paste0('correlationMissingData_', make.names(g)),
                                #     type = jmvcore::NoticeType$WARNING
                                # )
                                # notice$setContent(res$msg)
                                # self$results$insert(100, notice)
                            }
                        }
                    }
                    # Also provide pooled result for reference
                    pooled_res <- compute_cor(seq_along(x_data), "Pooled")
                    if (!is.null(pooled_res$values)) {
                        self$results$correlation$addRow(rowKey = "pooled", values = pooled_res$values)
                    }
                    if (!is.null(pooled_res$msg)) {
                        # notice <- jmvcore::Notice$new(
                        #     options = self$options,
                        #     name = 'correlationMissingDataPooled',
                        #     type = jmvcore::NoticeType$WARNING
                        # )
                        # notice$setContent(pooled_res$msg)
                        # self$results$insert(100, notice)
                    }
                    # notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = 'correlationGrouping',
                    #     type = jmvcore::NoticeType$INFO
                    # )
                    # notice$setContent("Correlations are computed per group and pooled. Within-group effects may differ from the pooled estimate.")
                    # self$results$insert(999, notice)
                    if (!is.null(self$options$facetvar)) {
                        # notice_facet <- jmvcore::Notice$new(
                        #     options = self$options,
                        #     name = 'correlationFacetingWarning',
                        #     type = jmvcore::NoticeType$INFO
                        # )
                        # notice_facet$setContent("Correlation with faceting: pooled and per-group correlations may differ across facets.")
                        # self$results$insert(999, notice_facet)
                    }
                } else {
                    pooled_res <- compute_cor(seq_along(x_data), "Overall")
                    if (!is.null(pooled_res$values)) {
                        self$results$correlation$addRow(rowKey = 1, values = pooled_res$values)
                    }
                    if (!is.null(pooled_res$msg)) {
                        # notice <- jmvcore::Notice$new(
                        #     options = self$options,
                        #     name = 'correlationMissingData',
                        #     type = jmvcore::NoticeType$WARNING
                        # )
                        # notice$setContent(pooled_res$msg)
                        # self$results$insert(100, notice)
                    }
                    if (!is.null(self$options$facetvar)) {
                        # notice_facet <- jmvcore::Notice$new(
                        #     options = self$options,
                        #     name = 'correlationFacetingWarningOverall',
                        #     type = jmvcore::NoticeType$INFO
                        # )
                        # notice_facet$setContent("Correlation with faceting: pooled correlation may differ across facets.")
                        # self$results$insert(999, notice_facet)
                    }
                }

            }, error = function(e) {
                # notice <- jmvcore::Notice$new(
                #     options = self$options,
                #     name = 'correlationFailed',
                #     type = jmvcore::NoticeType$STRONG_WARNING
                # )
                # notice$setContent(paste("Correlation analysis failed:", e$message))
                # self$results$insert(999, notice)
            })
        },

        .calculateStatistics = function() {
            if (!self$options$addStats) return()
            if (!(self$options$plotType %in% c("boxplot", "violin"))) return()

            tryCatch({
                x_col <- self$options$xvar
                y_col <- self$options$yvar

                # Get complete data
                complete_data <- self$data[complete.cases(self$data[[x_col]], self$data[[y_col]]), ]

                groups <- unique(complete_data[[x_col]])
                groups <- groups[!is.na(groups)]
                if (length(groups) < 2) return()

                # Small-sample warning
                group_sizes <- table(complete_data[[x_col]])
                if (any(group_sizes < 5)) {
                    small_groups <- names(group_sizes[group_sizes < 5])
                    small_sizes <- group_sizes[group_sizes < 5]
                    group_details <- paste(paste0(small_groups, " (n=", small_sizes, ")"), collapse = ", ")
                    # notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = 'smallSampleSize',
                    #     type = jmvcore::NoticeType$WARNING
                    # )
                    # notice$setContent(paste0(
                    #     "Small sample size detected: ", group_details, ". ",
                    #     "Groups with fewer than 5 observations may not meet assumptions for statistical tests. ",
                    #     "Results should be interpreted with caution."
                    # ))
                    # self$results$insert(999, notice)
                }

                # Infer parametric vs nonparametric based on assumptions when auto
                infer <- private$.inferStatMethod(complete_data, x_col, y_col, length(groups))
                private$..inferredMethod <- infer
                private$..assumptionReason <- infer$reason
                if (!is.null(infer$reason)) {
                    # notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = 'autoMethodSwitch',
                    #     type = jmvcore::NoticeType$INFO
                    # )
                    # notice$setContent(infer$reason)
                    # self$results$insert(999, notice)
                }

                # HIERARCHICAL TESTING: For >2 groups, perform omnibus test first
                # Only proceed to pairwise comparisons if omnibus test is significant
                if (length(groups) > 2) {
                    # Perform omnibus test (ANOVA or Kruskal-Wallis)
                    omnibus_result <- if (infer$omnibus == "anova") {
                        # One-way ANOVA
                        formula_str <- paste(private$.escapeVarName(y_col), "~", private$.escapeVarName(x_col))
                        anova_result <- aov(as.formula(formula_str), data = complete_data)
                        summary_result <- summary(anova_result)
                        list(
                            statistic = summary_result[[1]]$`F value`[1],
                            p.value = summary_result[[1]]$`Pr(>F)`[1],
                            method = "One-way ANOVA",
                            df1 = summary_result[[1]]$Df[1],
                            df2 = summary_result[[1]]$Df[2]
                        )
                    } else { # kruskal.test
                        # Kruskal-Wallis test
                        kw_result <- kruskal.test(as.formula(paste(private$.escapeVarName(y_col), "~", private$.escapeVarName(x_col))), data = complete_data)
                        list(
                            statistic = kw_result$statistic,
                            p.value = kw_result$p.value,
                            method = "Kruskal-Wallis",
                            df = kw_result$parameter
                        )
                    }

                    # Calculate effect size for omnibus test
                    omnibus_effect_size <- if (infer$omnibus == "anova") {
                        # Eta-squared for ANOVA: SS_between / SS_total
                        formula_str <- paste(private$.escapeVarName(y_col), "~", private$.escapeVarName(x_col))
                        anova_result <- aov(as.formula(formula_str), data = complete_data)
                        summary_result <- summary(anova_result)
                        ss_between <- summary_result[[1]]$`Sum Sq`[1]
                        ss_total <- sum(summary_result[[1]]$`Sum Sq`)
                        eta_sq <- ss_between / ss_total
                        effect_interp <- if (eta_sq < 0.01) "negligible"
                                        else if (eta_sq < 0.06) "small"
                                        else if (eta_sq < 0.14) "medium"
                                        else "large"
                        paste0("η²=", round(eta_sq, 3), " (", effect_interp, ")")
                    } else {
                        # Epsilon-squared for Kruskal-Wallis: (H - k + 1) / (n - k)
                        H <- omnibus_result$statistic
                        n <- nrow(complete_data)
                        k <- length(groups)
                        epsilon_sq <- (H - k + 1) / (n - k)
                        effect_interp <- if (epsilon_sq < 0.01) "negligible"
                                        else if (epsilon_sq < 0.06) "small"
                                        else if (epsilon_sq < 0.14) "medium"
                                        else "large"
                        paste0("ε²=", round(epsilon_sq, 3), " (", effect_interp, ")")
                    }

                    # Add omnibus test result to statistics table
                    omnibus_comparison <- if (infer$omnibus == "anova") {
                        paste0("Overall F(", omnibus_result$df1, ",", omnibus_result$df2, ")")
                    } else {
                        paste0("Overall H(", omnibus_result$df, ")")
                    }

                    self$results$statistics$addRow(rowKey = "omnibus", values = list(
                        comparison = omnibus_comparison,
                        method = omnibus_result$method,
                        statistic = omnibus_result$statistic,
                        pvalue = omnibus_result$p.value,
                        effectSize = omnibus_effect_size,
                        significance = if (omnibus_result$p.value < 0.001) "***" else if (omnibus_result$p.value < 0.01) "**" else if (omnibus_result$p.value < 0.05) "*" else "ns"
                    ))

                    # Check if omnibus test is significant at alpha = 0.05
                    if (omnibus_result$p.value >= 0.05) {
                        # Omnibus not significant - inform user
                        # notice <- jmvcore::Notice$new(
                        #     options = self$options,
                        #     name = 'omnibusNotSignificant',
                        #     type = jmvcore::NoticeType$INFO
                        # )
                        # notice$setContent(paste0(
                        #     "The omnibus test (", omnibus_result$method, ") was not significant (p = ",
                        #     round(omnibus_result$p.value, 4), "). ",
                        #     "Post-hoc pairwise comparisons are not recommended when the overall test is non-significant, ",
                        #     "as this increases Type I error risk. ",
                        #     if (self$options$pairwiseComparisons) "Pairwise results shown below should be interpreted with caution." else ""
                        # ))
                        # self$results$insert(999, notice)
                    
                        # If pairwise not requested, stop here
                        if (!self$options$pairwiseComparisons) {
                            return()
                        }
                    }

                    # If pairwise comparisons not enabled, stop after omnibus test
                    if (!self$options$pairwiseComparisons) {
                        # notice <- jmvcore::Notice$new(
                        #     options = self$options,
                        #     name = 'pairwiseNotEnabled',
                        #     type = jmvcore::NoticeType$INFO
                        # )
                        # notice$setContent(paste0(
                        #     "Omnibus test is significant (p = ", round(omnibus_result$p.value, 4), "). ",
                        #     "Enable 'Show Pairwise Comparisons' to see which specific groups differ."
                        # ))
                        # self$results$insert(999, notice)
                        return()
                    }
                } else {
                    # For 2 groups, pairwise IS the omnibus test - no hierarchical issue
                    # Continue to pairwise comparison below
                }
                
                # Generate pairwise comparisons (post-hoc tests)
                comps <- combn(groups, 2, simplify = FALSE)
                n_comparisons <- length(comps)

                # Determine actual test method used for pairwise comparisons
                actual_method_name <- if (infer$pairwise == "t.test") {
                    if (length(groups) > 2) {
                        "Pairwise t-tests (post-hoc)"
                    } else {
                        "Independent t-test"
                    }
                } else {
                    if (length(groups) > 2) {
                        "Pairwise Wilcoxon (post-hoc)"
                    } else {
                        "Mann-Whitney U test"
                    }
                }

                # Initialize results dataframe
                stats_df <- data.frame(
                    group1 = character(n_comparisons),
                    group2 = character(n_comparisons),
                    p = numeric(n_comparisons),
                    statistic = numeric(n_comparisons),
                    effect_size = numeric(n_comparisons),
                    effect_label = character(n_comparisons),
                    method = character(n_comparisons),  # Track which test was used
                    stringsAsFactors = FALSE
                )

                # Calculate max Y for positioning
                max_y <- max(complete_data[[y_col]], na.rm = TRUE)
                y_range <- max_y - min(complete_data[[y_col]], na.rm = TRUE)
                step_size <- y_range * 0.1

                for (i in seq_along(comps)) {
                    g1 <- comps[[i]][1]
                    g2 <- comps[[i]][2]
                    data1 <- complete_data[[y_col]][complete_data[[x_col]] == g1]
                    data2 <- complete_data[[y_col]][complete_data[[x_col]] == g2]

                    # Check sample sizes
                    if (length(data1) < 2 || length(data2) < 2) {
                        # Create descriptive notice for user
                        # notice <- jmvcore::Notice$new(
                        #     options = self$options,
                        #     name = paste0('insufficientSampleSize_', i),
                        #     type = jmvcore::NoticeType$STRONG_WARNING
                        # )
                        # notice$setContent(paste0(
                        #     "Insufficient data for statistical test between '", g1, "' (n=", length(data1),
                        #     ") and '", g2, "' (n=", length(data2), "). ",
                        #     "At least 2 observations per group required for comparison."
                        # ))
                        # self$results$insert(999, notice)
                        stop("Insufficient data for statistical comparison.")
                    }

                    # Perform pairwise test (NOT omnibus ANOVA/Kruskal-Wallis)
                    # Even if method is "anova", we perform t-test for each pair
                    test_result <- if (infer$pairwise == "t.test") {
                        t.test(data1, data2, na.action = na.omit)
                    } else {
                        wilcox.test(data1, data2, na.action = na.omit, exact = FALSE)
                    }

                    # Calculate effect size
                    effect_size <- if (infer$pairwise == "t.test") {
                        # Cohen's d for t-test
                        n1 <- length(data1)
                        n2 <- length(data2)
                        mean1 <- mean(data1, na.rm = TRUE)
                        mean2 <- mean(data2, na.rm = TRUE)
                        sd1 <- sd(data1, na.rm = TRUE)
                        sd2 <- sd(data2, na.rm = TRUE)
                        pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
                        cohens_d <- (mean1 - mean2) / pooled_sd
                        cohens_d
                    } else {
                        # Rank-biserial correlation for Wilcoxon/Mann-Whitney
                        n1 <- length(data1)
                        n2 <- length(data2)
                        U <- test_result$statistic
                        r <- 1 - (2 * U) / (n1 * n2)
                        r
                    }

                    # Effect size interpretation
                    effect_interp <- if (infer$pairwise == "t.test") {
                        # Cohen's d interpretation
                        if (abs(effect_size) < 0.2) "negligible"
                        else if (abs(effect_size) < 0.5) "small"
                        else if (abs(effect_size) < 0.8) "medium"
                        else "large"
                    } else {
                        # Rank-biserial r interpretation
                        if (abs(effect_size) < 0.1) "negligible"
                        else if (abs(effect_size) < 0.3) "small"
                        else if (abs(effect_size) < 0.5) "medium"
                        else "large"
                    }

                    stats_df$group1[i] <- as.character(g1)
                    stats_df$group2[i] <- as.character(g2)
                    stats_df$p[i] <- test_result$p.value
                    stats_df$statistic[i] <- test_result$statistic
                    stats_df$effect_size[i] <- effect_size
                    stats_df$effect_label[i] <- paste0(
                        if (infer$pairwise == "t.test") "d=" else "r=",
                        round(effect_size, 2), " (", effect_interp, ")"
                    )
                    stats_df$method[i] <- actual_method_name
                }
                
                # Adjust p-values
                stats_df$p.adj <- p.adjust(stats_df$p, method = "bonferroni")
                
                # Add significance stars
                stats_df$p.adj.signif <- symnum(stats_df$p.adj, corr = FALSE, na = FALSE, 
                                              cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                              symbols = c("***", "**", "*", "ns"))
                
                # Add y positions for plotting
                # Simple stacking: start above max_y and increment
                stats_df$y.position <- max_y + (seq_len(n_comparisons) * step_size)
                
                private$..stats_df <- stats_df
                
            }, error = function(e) {
                # notice <- jmvcore::Notice$new(
                #     options = self$options,
                #     name = 'statisticsCalculationFailed',
                #     type = jmvcore::NoticeType$STRONG_WARNING
                # )
                # notice$setContent(paste("Statistics calculation failed:", e$message))
                # self$results$insert(999, notice)
                stop(e)
            })
        },

        .populateStatistics = function() {
            if (!self$options$addStats) return()
            if (is.null(private$..stats_df)) return()
            
            table <- self$results$statistics
            df <- private$..stats_df
            infer <- private$..inferredMethod %||% list(omnibus_label = "None")
            
            for (i in 1:nrow(df)) {
                if (is.na(df$p[i])) next

                row_vals <- list(
                    comparison = paste(df$group1[i], "vs", df$group2[i]),
                    method = df$method[i],  # Show which test was actually performed
                    statistic = df$statistic[i],
                    pvalue = df$p[i],
                    effectSize = df$effect_label[i],
                    significance = paste0(
                        symnum(df$p[i], corr = FALSE, na = FALSE,
                               cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                               symbols = c("***", "**", "*", "ns")),
                        " (adj: ", df$p.adj.signif[i], ")"
                    )
                )
                table$addRow(rowKey = i, values = row_vals)
            }
            
                # Add notes about adjustment and pairwise-only nature
                if (nrow(df) > 1) {
                    table$setNote("adj", "P-values adjusted for multiple comparisons (Bonferroni).")
                    table$setNote("pairwise", paste0(
                        "Pairwise tests shown; omnibus method used: ", infer$omnibus_label, "."
                    ))
                }
                if (!is.null(private$..assumptionReason)) {
                    table$setNote("assumption", private$..assumptionReason)
                }
        },

        .generateSummary = function() {
            if (!self$options$addStats) return()
            if (is.null(private$..stats_df)) return()

            df <- private$..stats_df
            infer <- private$..inferredMethod
            n_groups <- length(unique(self$data[[self$options$xvar]]))
            x_var <- self$options$xvar
            y_var <- self$options$yvar

            # Omnibus result summary (if >2 groups)
            omnibus_text <- ""
            if (n_groups > 2 && !is.null(infer$omnibus)) {
                omnibus_method <- switch(infer$omnibus,
                    "anova" = "one-way ANOVA",
                    "kruskal.test" = "Kruskal-Wallis test",
                    infer$omnibus
                )
                omnibus_text <- paste0(
                    "A <strong>", omnibus_method, "</strong> comparing ", n_groups, " groups ",
                    "was performed as the omnibus test. "
                )
            }

            # Pairwise summary
            sig_pairs <- sum(df$p.adj < 0.05, na.rm = TRUE)
            total_pairs <- nrow(df)
            pairwise_text <- paste0(
                "<strong>", sig_pairs, " of ", total_pairs, " pairwise comparison",
                if (total_pairs > 1) "s were" else " was",
                " significant</strong> after Bonferroni correction (adjusted p < 0.05). "
            )

            # Effect size summary
            if ("effect_size" %in% names(df) && any(!is.na(df$effect_size))) {
                max_effect_idx <- which.max(abs(df$effect_size))
                max_effect <- df$effect_size[max_effect_idx]
                max_effect_comp <- paste(df$group1[max_effect_idx], "vs", df$group2[max_effect_idx])
                effect_type <- if (infer$pairwise == "t.test") "Cohen's d" else "rank-biserial r"

                effect_interp <- if (infer$pairwise == "t.test") {
                    if (abs(max_effect) >= 0.8) "large"
                    else if (abs(max_effect) >= 0.5) "medium"
                    else if (abs(max_effect) >= 0.2) "small"
                    else "negligible"
                } else {
                    if (abs(max_effect) >= 0.5) "large"
                    else if (abs(max_effect) >= 0.3) "medium"
                    else if (abs(max_effect) >= 0.1) "small"
                    else "negligible"
                }

                effect_text <- paste0(
                    "The largest effect size was <strong>", effect_interp, "</strong> (",
                    effect_type, " = ", round(max_effect, 2), ") for the comparison between ",
                    max_effect_comp, ". "
                )
            } else {
                effect_text <- ""
            }

            # Clinical interpretation
            clinical_text <- paste0(
                "These results suggest ",
                if (sig_pairs > 0) {
                    paste0("statistically significant differences in <strong>", y_var,
                           "</strong> across levels of <strong>", x_var, "</strong>. ")
                } else {
                    paste0("no statistically significant differences in <strong>", y_var,
                           "</strong> across levels of <strong>", x_var, "</strong> after multiple comparison adjustment. ")
                },
                "Interpret these results in the context of your research question and consider clinical significance alongside statistical significance."
            )

            summary_html <- paste0(
                "<div style='padding: 15px; background: linear-gradient(to right, #f0f8ff, #e6f3ff); border-left: 4px solid #4CAF50; border-radius: 4px;'>",
                "<h4 style='margin-top: 0; color: #2c3e50;'>📊 Analysis Summary</h4>",
                "<p>", omnibus_text, pairwise_text, effect_text, "</p>",
                "<p style='margin-bottom: 0;'><em style='color: #555;'>", clinical_text, "</em></p>",
                "</div>"
            )

            self$results$summary$setContent(summary_html)
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

            overrides <- list()

            switch(preset,
                "prognostic_biomarker" = {
                    if (self$options$plotType != "boxplot") overrides <- c(overrides, "Plot type set to boxplot")
                    if (!self$options$addStats) overrides <- c(overrides, "Statistical comparisons enabled")
                    plot_opt <- self$options$option("plotType")
                    stats_opt <- self$options$option("addStats")
                    method_opt <- self$options$option("statMethod")
                    plot_opt$value <- "boxplot"
                    stats_opt$value <- TRUE
                    method_opt$value <- "auto"
                },
                "diagnostic_test" = {
                    if (self$options$plotType != "boxplot") overrides <- c(overrides, "Plot type set to boxplot")
                    if (!self$options$addStats) overrides <- c(overrides, "Statistical comparisons enabled")
                    plot_opt <- self$options$option("plotType")
                    stats_opt <- self$options$option("addStats")
                    method_opt <- self$options$option("statMethod")
                    plot_opt$value <- "boxplot"
                    stats_opt$value <- TRUE
                    method_opt$value <- "wilcox.test"
                },
                "correlation_analysis" = {
                    if (self$options$plotType != "scatter") overrides <- c(overrides, "Plot type set to scatter")
                    if (!self$options$addCorr) overrides <- c(overrides, "Correlation statistics enabled")
                    plot_opt <- self$options$option("plotType")
                    corr_opt <- self$options$option("addCorr")
                    corr_method_opt <- self$options$option("corrMethod")
                    plot_opt$value <- "scatter"
                    corr_opt$value <- TRUE
                    corr_method_opt$value <- "pearson"
                }
            )

            if (length(overrides) > 0) {
                preset_name <- tools::toTitleCase(gsub("_", " ", preset))
                # notice <- jmvcore::Notice$new(
                #     options = self$options,
                #     name = 'clinicalPresetOverride',
                #     type = jmvcore::NoticeType$INFO
                # )
                # notice$setContent(paste0(
                #     "Clinical Preset Applied: ", preset_name, ". ",
                #     "The following options have been configured: ", paste(overrides, collapse = "; "), "."
                # ))
                # self$results$insert(999, notice)
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
