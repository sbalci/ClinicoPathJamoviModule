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
                stop('Data contains no (complete) rows')
            
            # Check variable existence
            for (var in self$options$dep) {
                if (!(var %in% names(self$data)))
                    stop(paste('Variable "', var, '" not found in data'))
            }
            if (!(self$options$group %in% names(self$data)))
                stop(paste('Variable "', self$options$group, '" not found in data'))
                
            return(TRUE)
        },
        
        # Data quality validation helper
        .validateDataQuality = function(mydata, vars) {
            for (var in vars) {
                num_vals <- jmvcore::toNumeric(mydata[[var]])
                num_vals <- num_vals[!is.na(num_vals)]
                
                if (length(num_vals) < 3) {
                    self$results$todo$setContent(
                        glue::glue("<br>⚠️ Warning: {var} has less than 3 valid observations<br>")
                    )
                }
                if (length(unique(num_vals)) < 2) {
                    self$results$todo$setContent(
                        glue::glue("<br>⚠️ Warning: {var} has no variation (all values are the same)<br>")
                    )
                }
            }
        },
        
        # Outlier detection helper
        .detectOutliers = function(data, vars, method = "IQR") {
            outliers <- list()
            for (var in vars) {
                vals <- jmvcore::toNumeric(data[[var]])
                vals <- vals[!is.na(vals)]
                if (length(vals) > 0) {
                    Q1 <- quantile(vals, 0.25, na.rm = TRUE)
                    Q3 <- quantile(vals, 0.75, na.rm = TRUE)
                    IQR <- Q3 - Q1
                    outlier_indices <- which(
                        data[[var]] < (Q1 - 1.5 * IQR) | 
                        data[[var]] > (Q3 + 1.5 * IQR)
                    )
                    if (length(outlier_indices) > 0) {
                        outliers[[var]] <- outlier_indices
                    }
                }
            }
            return(outliers)
        },
        
        # Theme application helper
        .applyTheme = function(plot, opts, ggtheme) {
            if (!opts$originaltheme) {
                plot + ggtheme
            } else {
                plot + ggstatsplot::theme_ggstatsplot()
            }
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
            
            # Add progress feedback
            self$results$todo$setContent(
                glue::glue("<br>Processing data for analysis...<br>")
            )

            mydata <- self$data
            vars <- self$options$dep

            # Convert numeric variables efficiently
            for (var in vars) {
                mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])
            }

            # Exclude NA
            mydata <- jmvcore::naOmit(mydata)
            
            # Validate data quality
            private$.validateDataQuality(mydata, vars)
            
            # Detect outliers if large dataset
            if (nrow(mydata) > 30) {
                outliers <- private$.detectOutliers(mydata, vars)
                if (length(outliers) > 0) {
                    for (var in names(outliers)) {
                        n_outliers <- length(outliers[[var]])
                        self$results$todo$setContent(
                            glue::glue("<br>ℹ️ {var} has {n_outliers} potential outlier(s) detected<br>")
                        )
                    }
                }
            }
            
            # Cache the processed data with hash
            private$.processedData <- mydata
            private$.data_hash <- current_hash
            return(mydata)
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
                violin = self$options$violin,
                boxplot = self$options$boxplot,
                point = self$options$point,
                plottype = self$options$plottype,
                bfmessage = self$options$bfmessage,
                k = self$options$k,
                conflevel = self$options$conflevel,
                varequal = self$options$varequal,
                meanplotting = self$options$meanplotting,
                meanci = self$options$meanci,
                notch = self$options$notch,
                samplesizeLabel = self$options$samplesizeLabel,
                titles = list(self$options$mytitle, self$options$xtitle, self$options$ytitle),
                display = list(self$options$resultssubtitle, self$options$originaltheme),
                dimensions = list(self$options$plotwidth, self$options$plotheight)
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
                violin = self$options$violin,
                boxplot = self$options$boxplot,
                point = self$options$point,
                resultssubtitle = self$options$resultssubtitle,
                originaltheme = self$options$originaltheme,
                mytitle = if (self$options$mytitle == '') NULL else self$options$mytitle,
                xtitle = if (self$options$xtitle == '') NULL else self$options$xtitle,
                ytitle = if (self$options$ytitle == '') NULL else self$options$ytitle,
                plottype = self$options$plottype,
                bfmessage = self$options$bfmessage,
                k = self$options$k,
                conflevel = self$options$conflevel,
                varequal = self$options$varequal,
                meanplotting = self$options$meanplotting,
                meanci = self$options$meanci,
                notch = self$options$notch,
                samplesizeLabel = self$options$samplesizeLabel
            )

            # Prepare visualization arguments using actual options
            options$violinargs <- if (options$violin) {
                list(width = 0.5, alpha = 0.2, na.rm = TRUE)
            } else {
                list(width = 0)
            }

            options$boxargs <- if (options$boxplot) {
                list(width = 0.2, alpha = 0.5, na.rm = TRUE)
            } else {
                list(width = 0)
            }

            options$pointargs <- if (options$point) {
                list(alpha = 0.5, linetype = "dashed")
            } else {
                list(alpha = 0)
            }
            
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
                todo <- glue::glue(
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
                    "<br>Violin plot analysis comparing {paste(self$options$dep, collapse=', ')} by {self$options$group}{if(!is.null(self$options$grvar)) paste0(', grouped by ', self$options$grvar) else ''}.<br><hr>"
                )

                self$results$todo$setContent(todo)

                # Data validation
                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')
                    
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
                
                plot <- ggstatsplot::ggbetweenstats(
                    data = mydata,
                    x = !!rlang::sym(group),
                    y = !!rlang::sym(dep),
                    title = opts$mytitle,
                    xlab = opts$xtitle,
                    ylab = opts$ytitle,
                    plot.type = opts$plottype,
                    type = opts$typestatistics,
                    pairwise.comparisons = opts$pairwisecomparisons,
                    pairwise.display = opts$pairwisedisplay,
                    p.adjust.method = opts$padjustmethod,
                    effsize.type = opts$effsizetype,
                    bf.message = opts$bfmessage,
                    k = opts$k,
                    conf.level = opts$conflevel,
                    var.equal = opts$varequal,
                    mean.plotting = opts$meanplotting,
                    mean.ci = opts$meanci,
                    notch = opts$notch,
                    sample.size.label = opts$samplesizeLabel,
                    violin.args = opts$violinargs,
                    box.args = opts$boxargs,
                    point.args = opts$pointargs,
                    results.subtitle = opts$resultssubtitle,
                    centrality.plotting = if (!is.null(opts$centrality.plotting)) opts$centrality.plotting else FALSE,
                    centrality.type = if (!is.null(opts$centrality.type)) opts$centrality.type else NULL
                )

                # Apply theme using helper
                plot <- private$.applyTheme(plot, opts, ggtheme)
            }

            # Multiple dependent variables analysis ----
            if (length(dep) > 1) {
                private$.checkpoint()
                
                dep2 <- as.list(dep)
                dep2_symbols <- purrr::map(dep2, rlang::sym)

                plotlist <- purrr::pmap(
                    .l = list(
                        y = dep2_symbols,
                        messages = FALSE
                    ),
                    .f = function(y, messages) {
                        ggstatsplot::ggbetweenstats(
                            data = mydata,
                            x = !!rlang::sym(group),
                            y = !!y,
                            messages = messages,
                            title = opts$mytitle,
                            xlab = opts$xtitle,
                            ylab = opts$ytitle,
                            plot.type = opts$plottype,
                            type = opts$typestatistics,
                            pairwise.comparisons = opts$pairwisecomparisons,
                            pairwise.display = opts$pairwisedisplay,
                            p.adjust.method = opts$padjustmethod,
                            effsize.type = opts$effsizetype,
                            bf.message = opts$bfmessage,
                            k = opts$k,
                            conf.level = opts$conflevel,
                            var.equal = opts$varequal,
                            mean.plotting = opts$meanplotting,
                            mean.ci = opts$meanci,
                            notch = opts$notch,
                            sample.size.label = opts$samplesizeLabel,
                            violin.args = opts$violinargs,
                            box.args = opts$boxargs,
                            point.args = opts$pointargs,
                            results.subtitle = opts$resultssubtitle,
                            centrality.plotting = if (!is.null(opts$centrality.plotting)) opts$centrality.plotting else FALSE,
                            centrality.type = if (!is.null(opts$centrality.type)) opts$centrality.type else NULL
                        )
                    }
                )

                # Apply theme to all plots using helper
                for (i in seq_along(plotlist)) {
                    plotlist[[i]] <- private$.applyTheme(plotlist[[i]], opts, ggtheme)
                }

                plot <- ggstatsplot::combine_plots(
                    plotlist = plotlist,
                    plotgrid.args = list(ncol = 1)
                )
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

                plot2 <- ggstatsplot::grouped_ggbetweenstats(
                    data = mydata,
                    x = !!rlang::sym(group),
                    y = !!rlang::sym(dep),
                    grouping.var = !!rlang::sym(grvar),
                    plot.type = opts$plottype,
                    type = opts$typestatistics,
                    pairwise.comparisons = opts$pairwisecomparisons,
                    pairwise.display = opts$pairwisedisplay,
                    p.adjust.method = opts$padjustmethod,
                    effsize.type = opts$effsizetype,
                    bf.message = opts$bfmessage,
                    k = opts$k,
                    conf.level = opts$conflevel,
                    var.equal = opts$varequal,
                    mean.plotting = opts$meanplotting,
                    mean.ci = opts$meanci,
                    notch = opts$notch,
                    sample.size.label = opts$samplesizeLabel,
                    violin.args = opts$violinargs,
                    box.args = opts$boxargs,
                    point.args = opts$pointargs,
                    results.subtitle = opts$resultssubtitle,
                    centrality.plotting = if (!is.null(opts$centrality.plotting)) opts$centrality.plotting else FALSE,
                    centrality.type = if (!is.null(opts$centrality.type)) opts$centrality.type else NULL,
                    ggtheme = selected_theme
                )
            }

            # Multiple dependent variables grouped analysis ----
            if (length(dep) > 1) {
                private$.checkpoint()
                
                selected_theme <- if (!opts$originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()

                dep2 <- as.list(dep)
                dep2_symbols <- purrr::map(dep2, rlang::sym)

                plotlist <- purrr::pmap(
                    .l = list(
                        y = dep2_symbols,
                        messages = FALSE
                    ),
                    .f = function(y, messages) {
                        ggstatsplot::grouped_ggbetweenstats(
                            data = mydata,
                            x = !!rlang::sym(group),
                            y = !!y,
                            grouping.var = !!rlang::sym(grvar),
                            messages = messages,
                            plot.type = opts$plottype,
                            type = opts$typestatistics,
                            pairwise.comparisons = opts$pairwisecomparisons,
                            pairwise.display = opts$pairwisedisplay,
                            p.adjust.method = opts$padjustmethod,
                            effsize.type = opts$effsizetype,
                            bf.message = opts$bfmessage,
                            k = opts$k,
                            conf.level = opts$conflevel,
                            var.equal = opts$varequal,
                            mean.plotting = opts$meanplotting,
                            mean.ci = opts$meanci,
                            notch = opts$notch,
                            sample.size.label = opts$samplesizeLabel,
                            violin.args = opts$violinargs,
                            box.args = opts$boxargs,
                            point.args = opts$pointargs,
                            results.subtitle = opts$resultssubtitle,
                            centrality.plotting = if (!is.null(opts$centrality.plotting)) opts$centrality.plotting else FALSE,
                            centrality.type = if (!is.null(opts$centrality.type)) opts$centrality.type else NULL,
                            ggtheme = selected_theme
                        )
                    }
                )

                plot2 <- ggstatsplot::combine_plots(
                    plotlist = plotlist,
                    plotgrid.args = list(ncol = 1)
                )
            }

            # Print Plot ----
            print(plot2)
            TRUE
        }

    )

)
