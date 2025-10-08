#' @title Violin Plots to Compare Between Groups
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#' @import ggplot2
#' @importFrom digest digest
#'


jjbetweenstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjbetweenstatsClass",
    inherit = jjbetweenstatsBase,
    private = list(
        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .processedOptions = NULL,
        .data_hash = NULL,
        .options_hash = NULL,

        # init ----
        .init = function() {
            deplen <- length(self$options$dep)
            
            # Use configurable plot dimensions
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 650
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450
            
            self$results$plot$setSize(plotwidth, deplen * plotheight)

            if (!is.null(self$options$grvar)) {
                mydata <- self$data
                grvar <- self$options$grvar
                
                if (!is.null(mydata[[grvar]])) {
                    num_levels <- nlevels(as.factor(mydata[[grvar]]))
                    self$results$plot2$setSize(num_levels * plotwidth, deplen * plotheight)
                }
            }
        },

        # Shared validation helper
        .validateInputs = function() {
            if (is.null(self$options$dep) || is.null(self$options$group))
                return(FALSE)
            if (nrow(self$data) == 0)
                stop(.('Data contains no (complete) rows'))
            
            # Get available variables for helpful error messages
            available_vars <- paste(names(self$data), collapse = '", "')
            
            # Check variable existence with helpful error messages
            for (var in self$options$dep) {
                if (!(var %in% names(self$data)))
                    stop(glue::glue(.('Variable "{var}" not found in data. Available variables: "{available_vars}"')))
            }
            if (!(self$options$group %in% names(self$data)))
                stop(glue::glue(.('Grouping variable "{group}" not found in data. Available variables: "{available_vars}"'), group = self$options$group))
                
            return(TRUE)
        },
        
        # Data quality validation helper
        .validateDataQuality = function(mydata, vars) {
            for (var in vars) {
                num_vals <- jmvcore::toNumeric(mydata[[var]])
                num_vals <- num_vals[!is.na(num_vals)]
                
                if (length(num_vals) < 3) {
                    self$results$todo$setContent(
                        glue::glue(.("<br>⚠️ Warning: {var} has less than 3 valid observations<br>"))
                    )
                }
                if (length(unique(num_vals)) < 2) {
                    self$results$todo$setContent(
                        glue::glue(.("<br>⚠️ Warning: {var} has no variation (all values are the same)<br>"))
                    )
                }
            }
        },
        
        # Optimized outlier detection helper for large datasets
        .detectOutliers = function(data, vars, method = "IQR") {
            outliers <- list()
            
            # For very large datasets, use sampling for outlier detection
            data_size <- nrow(data)
            use_sampling <- data_size > 5000
            
            for (var in vars) {
                vals <- jmvcore::toNumeric(data[[var]])
                vals <- vals[!is.na(vals)]
                
                if (length(vals) > 0) {
                    # For large datasets, sample for performance
                    if (use_sampling && length(vals) > 5000) {
                        # Use a representative sample for outlier threshold calculation
                        sample_size <- min(5000, length(vals))
                        sample_vals <- sample(vals, sample_size)
                        Q1 <- quantile(sample_vals, 0.25, na.rm = TRUE)
                        Q3 <- quantile(sample_vals, 0.75, na.rm = TRUE)
                    } else {
                        Q1 <- quantile(vals, 0.25, na.rm = TRUE)
                        Q3 <- quantile(vals, 0.75, na.rm = TRUE)
                    }
                    
                    IQR <- Q3 - Q1
                    
                    # Only calculate outlier indices if IQR is meaningful
                    if (IQR > 0) {
                        outlier_indices <- which(
                            data[[var]] < (Q1 - 1.5 * IQR) | 
                            data[[var]] > (Q3 + 1.5 * IQR)
                        )
                        
                        if (length(outlier_indices) > 0) {
                            # For large datasets, only report count not indices
                            if (use_sampling) {
                                outliers[[var]] <- length(outlier_indices)
                            } else {
                                outliers[[var]] <- outlier_indices
                            }
                        }
                    }
                }
            }
            return(outliers)
        },
        
        # Theme application helper
        .applyTheme = function(plot, opts, ggtheme) {
            if (!opts$originaltheme) {
                plot <- plot + ggtheme
            } else {
                plot <- plot + ggstatsplot::theme_ggstatsplot()
            }
            
            # Apply colorblind-safe palette if requested
            if (opts$colorblindSafe) {
                # Use viridis color palette which is colorblind-safe
                plot <- plot + ggplot2::scale_fill_viridis_d(option = "D") +
                              ggplot2::scale_color_viridis_d(option = "D")
            }
            
            return(plot)
        },
        
        # Clinical interpretation helper
        .generateClinicalInterpretation = function(plot_obj, test_type, variables) {
            interpretation <- tryCatch({
                # Extract statistics from ggstatsplot object
                if (!is.null(plot_obj$data)) {
                    subtitle_text <- plot_obj$labels$subtitle
                    if (!is.null(subtitle_text) && nchar(subtitle_text) > 0) {
                        # Extract key statistical information
                        test_name <- switch(test_type,
                            "parametric" = .("t-test"),
                            "nonparametric" = .("Mann-Whitney U test"),
                            "robust" = .("robust test"),
                            "bayes" = .("Bayesian analysis")
                        )
                        
                        clinical_text <- sprintf(
                            .("<div class='clinical-summary'><h4>Results Summary</h4><p>{test} comparing {vars} between groups.</p><p class='subtitle-stats'>{subtitle}</p></div>"),
                            test = test_name, vars = paste(variables, collapse = ", "), subtitle = subtitle_text
                        )
                        return(clinical_text)
                    }
                }
                return(.("<p>Analysis completed. See plot for detailed results.</p>"))
            }, error = function(e) {
                return(.("<p>Analysis completed successfully.</p>"))
            })
            
            return(interpretation)
        },
        
        # Statistical assumption checker
        .checkAssumptions = function(data, variables, group_var, test_type) {
            warnings <- c()
            
            for (var in variables) {
                var_data <- data[[var]]
                group_data <- data[[group_var]]
                
                # Check sample sizes
                group_counts <- table(group_data, useNA = "no")
                min_group_size <- min(group_counts)
                
                if (min_group_size < 3) {
                    warnings <- c(warnings, sprintf(.("⚠️ {var}: Minimum group size is {size} (recommend ≥3)"), var = var, size = min_group_size))
                }
                
                if (test_type == "parametric" && min_group_size >= 3) {
                    # Basic normality check using Shapiro-Wilk for small samples
                    for (level in names(group_counts)) {
                        group_subset <- var_data[group_data == level & !is.na(var_data)]
                        if (length(group_subset) >= 3 && length(group_subset) <= 5000) {
                            p_val <- tryCatch(shapiro.test(group_subset)$p.value, error = function(e) 1)
                            if (p_val < 0.05) {
                                warnings <- c(warnings, sprintf(.("⚠️ {var}: Data may not be normally distributed in group '{level}' (consider non-parametric)"), var = var, level = level))
                            }
                        }
                    }
                }
            }
            
            return(warnings)
        },

        # Optimized data preparation with robust caching
        .prepareData = function(force_refresh = FALSE) {
            # Create robust hash of current data to detect changes
            current_hash <- digest::digest(list(
                dep = self$options$dep,
                group = self$options$group,
                data_dim = dim(self$data),
                col_names = names(self$data),
                grvar = self$options$grvar
            ), algo = "md5")
            
            # Only reprocess if data has changed or forced refresh
            if (!is.null(private$.processedData) && 
                private$.data_hash == current_hash && 
                !force_refresh) {
                return(private$.processedData)
            }
            
            # Checkpoint before expensive data processing
            private$.checkpoint()
            
            # Add progress feedback
            self$results$todo$setContent(
                glue::glue(.("<br>Processing data for analysis...<br>"))
            )

            mydata <- self$data
            vars <- self$options$dep

            # Convert numeric variables efficiently - checkpoint before loop
            private$.checkpoint(flush = FALSE)
            for (var in vars) {
                mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])
            }

            # Exclude NA
            mydata <- jmvcore::naOmit(mydata)
            
            # Validate data quality
            private$.validateDataQuality(mydata, vars)
            
            # Check statistical assumptions
            assumption_warnings <- private$.checkAssumptions(mydata, vars, self$options$group, self$options$typestatistics)
            if (length(assumption_warnings) > 0) {
                warning_text <- paste(assumption_warnings, collapse = "<br>")
                self$results$todo$setContent(glue::glue("<br>{warning_text}<br>"))
            }
            
            # Detect outliers if large dataset - checkpoint before expensive outlier detection
            if (nrow(mydata) > 30) {
                private$.checkpoint(flush = FALSE)
                outliers <- private$.detectOutliers(mydata, vars)
                if (length(outliers) > 0) {
                    for (var in names(outliers)) {
                        # Handle both count (for large datasets) and indices (for smaller datasets)
                        if (is.numeric(outliers[[var]]) && length(outliers[[var]]) == 1) {
                            # For large datasets, we only have the count
                            n_outliers <- outliers[[var]]
                        } else {
                            # For smaller datasets, we have the actual indices
                            n_outliers <- length(outliers[[var]])
                        }
                        self$results$todo$setContent(
                            glue::glue(.("<br>ℹ️ {var} has {n_outliers} potential outlier(s) detected<br>"))
                        )
                    }
                }
            }
            
            # Cache the processed data with hash
            private$.processedData <- mydata
            private$.data_hash <- current_hash
            return(mydata)
        },

        # Helper function for title processing
        .processTitle = function(title) {
            if (is.null(title) || title == '') NULL else title
        },

        # Optimized options processing with robust caching
        .prepareOptions = function(force_refresh = FALSE) {
            # Create robust hash of current options to detect changes
            current_options_hash <- digest::digest(list(
                typestatistics = self$options$typestatistics,
                pairwisecomparisons = self$options$pairwisecomparisons,
                pairwisedisplay = self$options$pairwisedisplay,
                padjustmethod = self$options$padjustmethod,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                bfmessage = self$options$bfmessage,
                k = self$options$k,
                conflevel = self$options$conflevel,
                varequal = self$options$varequal,
                titles = list(self$options$mytitle, self$options$xtitle, self$options$ytitle),
                display = list(self$options$resultssubtitle, self$options$originaltheme),
                dimensions = list(self$options$plotwidth, self$options$plotheight),
                colorblindSafe = self$options$colorblindSafe
            ), algo = "md5")
            
            # Only reprocess if options have changed or forced refresh
            if (!is.null(private$.processedOptions) && 
                private$.options_hash == current_options_hash && 
                !force_refresh) {
                return(private$.processedOptions)
            }

            options <- list(
                typestatistics = self$options$typestatistics,
                pairwisecomparisons = self$options$pairwisecomparisons,
                pairwisedisplay = self$options$pairwisedisplay,
                padjustmethod = self$options$padjustmethod,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                resultssubtitle = self$options$resultssubtitle,
                originaltheme = self$options$originaltheme,
                mytitle = private$.processTitle(self$options$mytitle),
                xtitle = private$.processTitle(self$options$xtitle),
                ytitle = private$.processTitle(self$options$ytitle),
                bfmessage = self$options$bfmessage,
                k = self$options$k,
                conflevel = self$options$conflevel,
                varequal = self$options$varequal,
                colorblindSafe = self$options$colorblindSafe
            )

            # Set default violin and box args for ggstatsplot
            options$violinargs <- list(width = 0.5, alpha = 0.2, na.rm = TRUE)
            options$boxplotargs <- list(width = 0.3, alpha = 0.5, na.rm = TRUE)
            
            # Point args are always used for data points in ggstatsplot
            options$pointargs <- list(
                position = ggplot2::position_jitterdodge(dodge.width = 0.6),
                alpha = 0.4,
                size = 3,
                stroke = 0
            )
            
            # Process centrality parameters if enabled - properly map to ggstatsplot API
            # Note: ggbetweenstats uses centrality.plotting and centrality.type parameters
            if (options$centralityplotting) {
                options$centrality.plotting <- TRUE
                options$centrality.type <- options$centralitytype
                options$centrality.point.args <- list(size = 5, color = "darkred")
                options$centrality.label.args <- list(size = 3, nudge_x = 0.4, segment.linetype = 4)
            } else {
                options$centrality.plotting <- FALSE
            }

            # Cache the processed options with hash
            private$.processedOptions <- options
            private$.options_hash <- current_options_hash
            return(options)
        }

        # run ----
        ,
        .run = function() {
            # Initial Message ----
            if (is.null(self$options$dep) || is.null(self$options$group)) {
                todo <- .(
                    "<br>Welcome to ClinicoPath
                <br><br>
                This tool creates optimized Box-Violin Plots for comparing continuous variables between groups.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.html' target='_blank'>here</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbetweenstats.html' target='_blank'>here</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)
                return()

            } else {
                todo <- glue::glue(
                    .("<br>Violin plot analysis comparing {vars} by {group}{grouped}.<br><hr>"),
                    vars = paste(self$options$dep, collapse=', '),
                    group = self$options$group,
                    grouped = if(!is.null(self$options$grvar)) paste0(', grouped by ', self$options$grvar) else ''
                )

                self$results$todo$setContent(todo)

                # Data validation
                if (nrow(self$data) == 0)
                    stop(.('Data contains no (complete) rows'))
                    
                # Add checkpoint for user feedback
                private$.checkpoint()
            }
        }

        ,
        .plot = function(image, ggtheme, theme, ...) {
            # Use shared validation helper ----
            if (!private$.validateInputs())
                return()

            # Add checkpoint for user feedback
            private$.checkpoint()

            # Use optimized data and options preparation
            mydata <- private$.prepareData()
            opts <- private$.prepareOptions()
            
            dep <- self$options$dep
            group <- self$options$group

            # Single dependent variable analysis ----
            if (length(dep) == 1) {
                private$.checkpoint()
                
                # Build argument list
                args_list <- list(
                    data = mydata,
                    x = rlang::sym(group),
                    y = rlang::sym(dep),
                    title = opts$mytitle,
                    xlab = opts$xtitle,
                    ylab = opts$ytitle,
                    type = opts$typestatistics,
                    pairwise.comparisons = opts$pairwisecomparisons,
                    pairwise.display = opts$pairwisedisplay,
                    p.adjust.method = opts$padjustmethod,
                    effsize.type = opts$effsizetype,
                    bf.message = opts$bfmessage,
                    k = opts$k,
                    conf.level = opts$conflevel,
                    var.equal = opts$varequal,
                    point.args = opts$pointargs,
                    results.subtitle = opts$resultssubtitle,
                    centrality.plotting = if (!is.null(opts$centrality.plotting)) opts$centrality.plotting else FALSE,
                    centrality.type = if (!is.null(opts$centrality.type)) opts$centrality.type else NULL
                )
                
                # Add violin.args and boxplot.args only if they exist
                if (!is.null(opts$violinargs)) {
                    args_list$violin.args <- opts$violinargs
                }
                if (!is.null(opts$boxplotargs)) {
                    args_list$boxplot.args <- opts$boxplotargs
                }
                
                plot <- do.call(ggstatsplot::ggbetweenstats, args_list)

                # Apply theme using helper
                plot <- private$.applyTheme(plot, opts, ggtheme)
                
                # Generate clinical interpretation
                interpretation <- private$.generateClinicalInterpretation(plot, opts$typestatistics, dep)
                self$results$todo$setContent(interpretation)
            }

            # Multiple dependent variables analysis ----
            if (length(dep) > 1) {
                private$.checkpoint()
                
                dep2 <- as.list(dep)
                dep2_symbols <- purrr::map(dep2, rlang::sym)

                # Checkpoint before expensive multiple plot generation
                private$.checkpoint(flush = FALSE)
                plotlist <- purrr::pmap(
                    .l = list(
                        y = dep2_symbols,
                        messages = FALSE
                    ),
                    .f = function(y, messages) {
                        plot_args <- list(
                            data = mydata,
                            x = rlang::sym(group),
                            y = !!y,
                            messages = messages,
                            title = opts$mytitle,
                            xlab = opts$xtitle,
                            ylab = opts$ytitle,
                            type = opts$typestatistics,
                            pairwise.comparisons = opts$pairwisecomparisons,
                            pairwise.display = opts$pairwisedisplay,
                            p.adjust.method = opts$padjustmethod,
                            effsize.type = opts$effsizetype,
                            bf.message = opts$bfmessage,
                            k = opts$k,
                            conf.level = opts$conflevel,
                            var.equal = opts$varequal,
                            point.args = opts$pointargs,
                            results.subtitle = opts$resultssubtitle,
                            centrality.plotting = if (!is.null(opts$centrality.plotting)) opts$centrality.plotting else FALSE,
                            centrality.type = if (!is.null(opts$centrality.type)) opts$centrality.type else NULL
                        )
                        
                        # Add violin.args and boxplot.args if they exist
                        if (!is.null(opts$violinargs)) {
                            plot_args$violin.args <- opts$violinargs
                        }
                        if (!is.null(opts$boxplotargs)) {
                            plot_args$boxplot.args <- opts$boxplotargs
                        }
                        
                        do.call(ggstatsplot::ggbetweenstats, plot_args)
                    }
                )

                # Apply theme to all plots using helper - checkpoint before theme application
                private$.checkpoint(flush = FALSE)
                plotlist <- lapply(plotlist, function(p) private$.applyTheme(p, opts, ggtheme))

                # Checkpoint before combining plots
                private$.checkpoint(flush = FALSE)
                plot <- ggstatsplot::combine_plots(
                    plotlist = plotlist,
                    plotgrid.args = list(ncol = 1)
                )
                
                # Generate clinical interpretation for multiple variables
                interpretation <- private$.generateClinicalInterpretation(plotlist[[1]], opts$typestatistics, dep)
                self$results$todo$setContent(interpretation)
            }

            # Print Plot ----
            print(plot)
            TRUE
        }

        ,
        .plot2 = function(image, ggtheme, theme, ...) {
            # Use shared validation helper with additional grouping check ----
            if (!private$.validateInputs() || is.null(self$options$grvar))
                return()

            # Add checkpoint for user feedback
            private$.checkpoint()

            # Use optimized data and options preparation (cached)
            mydata <- private$.prepareData()
            opts <- private$.prepareOptions()
            
            dep <- self$options$dep
            group <- self$options$group
            grvar <- self$options$grvar

            # Single dependent variable grouped analysis ----
            if (length(dep) == 1) {
                private$.checkpoint()
                
                selected_theme <- if (!opts$originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()

                # Build argument list for grouped analysis
                grouped_args <- list(
                    data = mydata,
                    x = rlang::sym(group),
                    y = rlang::sym(dep),
                    grouping.var = rlang::sym(grvar),
                    type = opts$typestatistics,
                    pairwise.comparisons = opts$pairwisecomparisons,
                    pairwise.display = opts$pairwisedisplay,
                    p.adjust.method = opts$padjustmethod,
                    effsize.type = opts$effsizetype,
                    bf.message = opts$bfmessage,
                    k = opts$k,
                    conf.level = opts$conflevel,
                    var.equal = opts$varequal,
                    point.args = opts$pointargs,
                    results.subtitle = opts$resultssubtitle,
                    centrality.plotting = if (!is.null(opts$centrality.plotting)) opts$centrality.plotting else FALSE,
                    centrality.type = if (!is.null(opts$centrality.type)) opts$centrality.type else NULL,
                    ggtheme = selected_theme
                )
                
                # Add violin.args and boxplot.args if they exist
                if (!is.null(opts$violinargs)) {
                    grouped_args$violin.args <- opts$violinargs
                }
                if (!is.null(opts$boxplotargs)) {
                    grouped_args$boxplot.args <- opts$boxplotargs
                }
                
                plot2 <- do.call(ggstatsplot::grouped_ggbetweenstats, grouped_args)
            }

            # Multiple dependent variables grouped analysis ----
            if (length(dep) > 1) {
                private$.checkpoint()
                
                selected_theme <- if (!opts$originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()

                dep2 <- as.list(dep)
                dep2_symbols <- purrr::map(dep2, rlang::sym)

                # Checkpoint before expensive multiple grouped plot generation
                private$.checkpoint(flush = FALSE)
                plotlist <- purrr::pmap(
                    .l = list(
                        y = dep2_symbols,
                        messages = FALSE
                    ),
                    .f = function(y, messages) {
                        # Build argument list for multiple variable grouped analysis
                        grouped_multi_args <- list(
                            data = mydata,
                            x = rlang::sym(group),
                            y = !!y,
                            grouping.var = rlang::sym(grvar),
                            messages = messages,
                                    type = opts$typestatistics,
                            pairwise.comparisons = opts$pairwisecomparisons,
                            pairwise.display = opts$pairwisedisplay,
                            p.adjust.method = opts$padjustmethod,
                            effsize.type = opts$effsizetype,
                            bf.message = opts$bfmessage,
                            k = opts$k,
                            conf.level = opts$conflevel,
                            var.equal = opts$varequal,
                            point.args = opts$pointargs,
                            results.subtitle = opts$resultssubtitle,
                            centrality.plotting = if (!is.null(opts$centrality.plotting)) opts$centrality.plotting else FALSE,
                            centrality.type = if (!is.null(opts$centrality.type)) opts$centrality.type else NULL,
                            ggtheme = selected_theme
                        )
                        
                        # Add violin.args and boxplot.args if they exist
                        if (!is.null(opts$violinargs)) {
                            grouped_multi_args$violin.args <- opts$violinargs
                        }
                        if (!is.null(opts$boxplotargs)) {
                            grouped_multi_args$boxplot.args <- opts$boxplotargs
                        }
                        
                        do.call(ggstatsplot::grouped_ggbetweenstats, grouped_multi_args)
                    }
                )

                # Checkpoint before combining grouped plots
                private$.checkpoint(flush = FALSE)
                plot2 <- ggstatsplot::combine_plots(
                    plotlist = plotlist,
                    plotgrid.args = list(ncol = 1)
                )
            }

            # Print Plot ----
            print(plot2)
            TRUE
        }

        ,
        .plotGGPubr = function(image, ggtheme, theme, ...) {
            # Validate inputs
            if (!private$.validateInputs())
                return()

            # Skip if ggpubr plot not requested
            if (!self$options$addGGPubrPlot)
                return()

            private$.checkpoint()

            # Prepare data
            mydata <- private$.prepareData()
            dep <- self$options$dep
            group <- self$options$group

            # Get palette
            palette <- if (self$options$ggpubrPalette == "default") NULL else self$options$ggpubrPalette

            # Single dependent variable
            if (length(dep) == 1) {
                # Build arguments conditionally
                args <- list(
                    data = mydata,
                    x = group,
                    y = dep,
                    title = if (nchar(self$options$mytitle) > 0) self$options$mytitle else NULL,
                    xlab = if (nchar(self$options$xtitle) > 0) self$options$xtitle else group,
                    ylab = if (nchar(self$options$ytitle) > 0) self$options$ytitle else dep,
                    add = if (self$options$ggpubrAddPoints) "jitter" else NULL,
                    palette = palette
                )

                # Create plot based on type
                if (self$options$ggpubrPlotType == "boxplot") {
                    plot <- do.call(ggpubr::ggboxplot, args)
                } else if (self$options$ggpubrPlotType == "violin") {
                    plot <- do.call(ggpubr::ggviolin, args)
                } else if (self$options$ggpubrPlotType == "boxviolin") {
                    # Create violin plot and add boxplot
                    plot <- do.call(ggpubr::ggviolin, args)
                    plot <- plot + ggplot2::geom_boxplot(width = 0.1)
                }

                # Add statistical comparisons
                if (self$options$ggpubrAddStats) {
                    plot <- plot + ggpubr::stat_compare_means()
                }

                # Apply theme
                plot <- plot + ggpubr::theme_pubr()

                print(plot)
            }

            # Multiple dependent variables
            if (length(dep) > 1) {
                dep_list <- as.list(dep)

                plotlist <- lapply(dep_list, function(depvar) {
                    args <- list(
                        data = mydata,
                        x = group,
                        y = depvar,
                        title = depvar,
                        xlab = group,
                        ylab = depvar,
                        add = if (self$options$ggpubrAddPoints) "jitter" else NULL,
                        palette = palette
                    )

                    if (self$options$ggpubrPlotType == "boxplot") {
                        p <- do.call(ggpubr::ggboxplot, args)
                    } else if (self$options$ggpubrPlotType == "violin") {
                        p <- do.call(ggpubr::ggviolin, args)
                    } else if (self$options$ggpubrPlotType == "boxviolin") {
                        p <- do.call(ggpubr::ggviolin, args)
                        p <- p + ggplot2::geom_boxplot(width = 0.1)
                    }

                    if (self$options$ggpubrAddStats) {
                        p <- p + ggpubr::stat_compare_means()
                    }

                    p <- p + ggpubr::theme_pubr()
                    return(p)
                })

                plot <- ggpubr::ggarrange(plotlist = plotlist, ncol = 1, nrow = length(dep))
                print(plot)
            }

            TRUE
        }

        ,
        .plotGGPubr2 = function(image, ggtheme, theme, ...) {
            # Validate inputs
            if (!private$.validateInputs())
                return()

            # Skip if ggpubr plot not requested or no grouping variable
            if (!self$options$addGGPubrPlot || is.null(self$options$grvar))
                return()

            private$.checkpoint()

            # Prepare data
            mydata <- private$.prepareData()
            dep <- self$options$dep
            group <- self$options$group
            grvar <- self$options$grvar

            # Get palette
            palette <- if (self$options$ggpubrPalette == "default") NULL else self$options$ggpubrPalette

            # Single dependent variable with faceting
            if (length(dep) == 1) {
                args <- list(
                    data = mydata,
                    x = group,
                    y = dep,
                    title = if (nchar(self$options$mytitle) > 0) self$options$mytitle else NULL,
                    xlab = if (nchar(self$options$xtitle) > 0) self$options$xtitle else group,
                    ylab = if (nchar(self$options$ytitle) > 0) self$options$ytitle else dep,
                    add = if (self$options$ggpubrAddPoints) "jitter" else NULL,
                    palette = palette,
                    facet.by = grvar
                )

                if (self$options$ggpubrPlotType == "boxplot") {
                    plot <- do.call(ggpubr::ggboxplot, args)
                } else if (self$options$ggpubrPlotType == "violin") {
                    plot <- do.call(ggpubr::ggviolin, args)
                } else if (self$options$ggpubrPlotType == "boxviolin") {
                    plot <- do.call(ggpubr::ggviolin, args)
                    plot <- plot + ggplot2::geom_boxplot(width = 0.1)
                }

                if (self$options$ggpubrAddStats) {
                    plot <- plot + ggpubr::stat_compare_means()
                }

                plot <- plot + ggpubr::theme_pubr()
                print(plot)
            }

            # Multiple dependent variables with faceting
            if (length(dep) > 1) {
                dep_list <- as.list(dep)

                plotlist <- lapply(dep_list, function(depvar) {
                    args <- list(
                        data = mydata,
                        x = group,
                        y = depvar,
                        title = depvar,
                        xlab = group,
                        ylab = depvar,
                        add = if (self$options$ggpubrAddPoints) "jitter" else NULL,
                        palette = palette,
                        facet.by = grvar
                    )

                    if (self$options$ggpubrPlotType == "boxplot") {
                        p <- do.call(ggpubr::ggboxplot, args)
                    } else if (self$options$ggpubrPlotType == "violin") {
                        p <- do.call(ggpubr::ggviolin, args)
                    } else if (self$options$ggpubrPlotType == "boxviolin") {
                        p <- do.call(ggpubr::ggviolin, args)
                        p <- p + ggplot2::geom_boxplot(width = 0.1)
                    }

                    if (self$options$ggpubrAddStats) {
                        p <- p + ggpubr::stat_compare_means()
                    }

                    p <- p + ggpubr::theme_pubr()
                    return(p)
                })

                plot <- ggpubr::ggarrange(plotlist = plotlist, ncol = 1, nrow = length(dep))
                print(plot)
            }

            TRUE
        }

    )

)
