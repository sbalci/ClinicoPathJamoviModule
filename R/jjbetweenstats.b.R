#' @title Violin Plots to Compare Between Groups
#' @importFrom R6 R6Class
#' @import jmvcore
#'


jjbetweenstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjbetweenstatsClass",
    inherit = jjbetweenstatsBase,
    private = list(
        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .processedOptions = NULL,

        # init ----
        .init = function() {
            deplen <- length(self$options$dep)
            self$results$plot$setSize(650, deplen * 450)

            if (!is.null(self$options$grvar)) {
                mydata <- self$data
                grvar <- self$options$grvar
                
                if (!is.null(mydata[[grvar]])) {
                    num_levels <- nlevels(as.factor(mydata[[grvar]]))
                    self$results$plot2$setSize(num_levels * 650, deplen * 450)
                }
            }
        },

        # Optimized data preparation with caching
        .prepareData = function(force_refresh = FALSE) {
            if (!is.null(private$.processedData) && !force_refresh) {
                return(private$.processedData)
            }

            mydata <- self$data
            vars <- self$options$dep

            # Convert numeric variables efficiently
            for (var in vars) {
                mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])
            }

            # Exclude NA
            mydata <- jmvcore::naOmit(mydata)
            
            # Cache the processed data
            private$.processedData <- mydata
            return(mydata)
        },

        # Optimized options processing with caching
        .prepareOptions = function(force_refresh = FALSE) {
            if (!is.null(private$.processedOptions) && !force_refresh) {
                return(private$.processedOptions)
            }

            options <- list(
                typestatistics = jmvcore::constructFormula(terms = self$options$typestatistics),
                pairwisecomparisons = self$options$pairwisecomparisons,
                pairwisedisplay = jmvcore::constructFormula(terms = self$options$pairwisedisplay),
                padjustmethod = jmvcore::constructFormula(terms = self$options$padjustmethod),
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                resultssubtitle = self$options$resultssubtitle,
                originaltheme = self$options$originaltheme,
                mytitle = self$options$mytitle,
                xtitle = if (self$options$xtitle == '') NULL else self$options$xtitle,
                ytitle = if (self$options$ytitle == '') NULL else self$options$ytitle
            )

            # Prepare visualization arguments
            options$violinargs <- if (self$options$violin) {
                list(width = 0.5, alpha = 0.2, na.rm = TRUE)
            } else {
                list(width = 0)
            }

            options$boxplotargs <- if (self$options$boxplot) {
                list(width = 0.2, alpha = 0.5, na.rm = TRUE)
            } else {
                list(width = 0)
            }

            options$pointargs <- if (self$options$point) {
                list(alpha = 0.5, linetype = "dashed")
            } else {
                list(alpha = 0)
            }

            # Cache the processed options
            private$.processedOptions <- options
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
            # Validation ----
            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

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
                    type = opts$typestatistics,
                    pairwise.comparisons = opts$pairwisecomparisons,
                    pairwise.display = opts$pairwisedisplay,
                    p.adjust.method = opts$padjustmethod,
                    effsize.type = opts$effsizetype,
                    centrality.plotting = opts$centralityplotting,
                    centrality.type = opts$centralitytype,
                    violin.args = opts$violinargs,
                    boxplot.args = opts$boxplotargs,
                    point.args = opts$pointargs,
                    results.subtitle = opts$resultssubtitle
                )

                # Apply theme
                if (!opts$originaltheme) {
                    plot <- plot + ggtheme
                } else {
                    plot <- plot + ggstatsplot::theme_ggstatsplot()
                }
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
                            type = opts$typestatistics,
                            pairwise.comparisons = opts$pairwisecomparisons,
                            pairwise.display = opts$pairwisedisplay,
                            p.adjust.method = opts$padjustmethod,
                            effsize.type = opts$effsizetype,
                            centrality.plotting = opts$centralityplotting,
                            centrality.type = opts$centralitytype,
                            violin.args = opts$violinargs,
                            boxplot.args = opts$boxplotargs,
                            point.args = opts$pointargs,
                            results.subtitle = opts$resultssubtitle
                        )
                    }
                )

                # Apply theme to all plots
                for (i in seq_along(plotlist)) {
                    if (!opts$originaltheme) {
                        plotlist[[i]] <- plotlist[[i]] + ggtheme
                    } else {
                        plotlist[[i]] <- plotlist[[i]] + ggstatsplot::theme_ggstatsplot()
                    }
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
            # Validation ----
            if (is.null(self$options$dep) || is.null(self$options$group) || is.null(self$options$grvar))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

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
                    type = opts$typestatistics,
                    pairwise.comparisons = opts$pairwisecomparisons,
                    pairwise.display = opts$pairwisedisplay,
                    p.adjust.method = opts$padjustmethod,
                    effsize.type = opts$effsizetype,
                    centrality.plotting = opts$centralityplotting,
                    centrality.type = opts$centralitytype,
                    violin.args = opts$violinargs,
                    boxplot.args = opts$boxplotargs,
                    point.args = opts$pointargs,
                    results.subtitle = opts$resultssubtitle,
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
                            type = opts$typestatistics,
                            pairwise.comparisons = opts$pairwisecomparisons,
                            pairwise.display = opts$pairwisedisplay,
                            p.adjust.method = opts$padjustmethod,
                            effsize.type = opts$effsizetype,
                            centrality.plotting = opts$centralityplotting,
                            centrality.type = opts$centralitytype,
                            violin.args = opts$violinargs,
                            boxplot.args = opts$boxplotargs,
                            point.args = opts$pointargs,
                            results.subtitle = opts$resultssubtitle,
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
