#' @title Bar Charts
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'

jjbarstatsClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "jjbarstatsClass",
        inherit = jjbarstatsBase,
        private = list(
            # init ----

            .init = function() {

                deplen <- length(self$options$dep)
                self$results$plot$setSize(650, deplen * 450)

                if (!is.null(self$options$grvar)) {

                    mydata <- self$data

                    grvar <-  self$options$grvar

                    num_levels <- nlevels(
                        as.factor(mydata[[grvar]])
                    )

                    self$results$plot2$setSize(num_levels * 650, deplen * 450)

                }

            }

            # run ----
            ,
            .run = function() {
                # Initial Message ----
                if (is.null(self$options$dep) ||
                    is.null(self$options$group)) {
                    
                    todo <- glue::glue(
                        "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Bar Charts with statistical testing.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggbarstats.html' target='_blank'>ggbarstats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbarstats.html' target='_blank'>grouped_ggbarstats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                    )

                    self$results$todo$setContent(todo)
                    return()

                } else {
                    todo <- glue::glue(
                        "<br>Bar chart analysis comparing {paste(self$options$dep, collapse=', ')} by {self$options$group}{if(!is.null(self$options$grvar)) paste0(', grouped by ', self$options$grvar) else ''}.<br><hr>"
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

                # Prepare Data ----
                mydata <- self$data
                mydata <- jmvcore::naOmit(mydata)
                
                dep <- self$options$dep
                group <- self$options$group

                # Single dependent variable analysis ----
                if (length(self$options$dep) == 1) {
                    private$.checkpoint()
                    
                    plot <- ggstatsplot::ggbarstats(
                        data = mydata,
                        x = !!rlang::sym(dep),
                        y = !!rlang::sym(group),
                        type = self$options$typestatistics,
                        pairwise.comparisons = self$options$pairwisecomparisons,
                        pairwise.display = self$options$pairwisedisplay,
                        p.adjust.method = self$options$padjustmethod
                    )

                    # Apply theme
                    originaltheme <- self$options$originaltheme
                    if (!originaltheme) {
                        plot <- plot + ggtheme
                    } else {
                        plot <- plot + ggstatsplot::theme_ggstatsplot()
                    }
                }


                # Multiple dependent variables analysis ----
                if (length(self$options$dep) > 1) {
                    private$.checkpoint()
                    
                    dep2 <- as.list(self$options$dep)
                    dep2_symbols <- purrr::map(dep2, rlang::sym)

                    plotlist <- purrr::pmap(
                        .l = list(
                            x = dep2_symbols,
                            messages = FALSE
                        ),
                        .f = function(x, messages) {
                            ggstatsplot::ggbarstats(
                                data = mydata,
                                x = !!x,
                                y = !!rlang::sym(group),
                                messages = messages,
                                type = self$options$typestatistics,
                                pairwise.comparisons = self$options$pairwisecomparisons,
                                pairwise.display = self$options$pairwisedisplay,
                                p.adjust.method = self$options$padjustmethod
                            )
                        }
                    )

                    # Apply theme to all plots
                    originaltheme <- self$options$originaltheme
                    for (i in seq_along(plotlist)) {
                        if (!originaltheme) {
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

                # Prepare Data ----
                mydata <- self$data
                mydata <- jmvcore::naOmit(mydata)
                
                dep <- self$options$dep
                group <- self$options$group
                grvar <- self$options$grvar


                # Single dependent variable grouped analysis ----
                if (length(self$options$dep) == 1) {
                    private$.checkpoint()
                    
                    originaltheme <- self$options$originaltheme
                    selected_theme <- if (!originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()

                    plot2 <- ggstatsplot::grouped_ggbarstats(
                        data = mydata,
                        x = !!rlang::sym(dep),
                        y = !!rlang::sym(group),
                        grouping.var = !!rlang::sym(grvar),
                        type = self$options$typestatistics,
                        pairwise.comparisons = self$options$pairwisecomparisons,
                        pairwise.display = self$options$pairwisedisplay,
                        p.adjust.method = self$options$padjustmethod,
                        ggtheme = selected_theme
                    )
                }


                # Multiple dependent variables grouped analysis ----
                if (length(self$options$dep) > 1) {
                    private$.checkpoint()
                    
                    originaltheme <- self$options$originaltheme
                    selected_theme <- if (!originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()

                    dep2 <- as.list(self$options$dep)
                    dep2_symbols <- purrr::map(dep2, rlang::sym)

                    plotlist <- purrr::pmap(
                        .l = list(
                            x = dep2_symbols,
                            messages = FALSE
                        ),
                        .f = function(x, messages) {
                            ggstatsplot::grouped_ggbarstats(
                                data = mydata,
                                x = !!x,
                                y = !!rlang::sym(group),
                                grouping.var = !!rlang::sym(grvar),
                                messages = messages,
                                type = self$options$typestatistics,
                                pairwise.comparisons = self$options$pairwisecomparisons,
                                pairwise.display = self$options$pairwisedisplay,
                                p.adjust.method = self$options$padjustmethod,
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
