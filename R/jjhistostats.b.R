#' @title Histogram
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import rlang
#' @import purrr
#' @import glue
#' @import ggstatsplot


jjhistostatsClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "jjhistostatsClass",
        inherit = jjhistostatsBase,
        private = list(

            # Cache for processed data and options to avoid redundant computation
            .processedData = NULL,
            .processedOptions = NULL,
            .processedAesthetics = NULL,

            # init ----
            .init = function() {

                deplen <- length(self$options$dep)
                
                # Use configurable plot dimensions
                plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 600
                plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450

                self$results$plot$setSize(plotwidth, deplen * plotheight)


                if (!is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                num_levels <- nlevels(
                    as.factor(mydata[[grvar]])
                )

                self$results$plot2$setSize(num_levels * plotwidth, deplen * plotheight)

                }



            },

            # Optimized data preparation with caching
            .prepareData = function(force_refresh = FALSE) {
                if (!is.null(private$.processedData) && !force_refresh) {
                    return(private$.processedData)
                }

                # Prepare data with progress feedback
                self$results$todo$setContent(
                    glue::glue("<br>Processing data for histogram analysis...<br><hr>")
                )

                mydata <- self$data
                
                # Convert variables to numeric
                vars <- self$options$dep
                if (!is.null(vars)) {
                    for (var in vars) {
                        mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])
                    }
                }

                # Exclude NA
                mydata <- jmvcore::naOmit(mydata)

                # Cache the processed data
                private$.processedData <- mydata
                return(mydata)
            },

            # Shared plot generation function to eliminate duplication
            .generateHistogram = function(data, x_var, options_data, aesthetics_data, grvar_sym = NULL, messages = TRUE) {
                # Build base arguments common to all plots
                base_args <- list(
                    data = data,
                    x = rlang::sym(x_var),
                    messages = messages,
                    type = options_data$typestatistics,
                    results.subtitle = options_data$resultssubtitle,
                    centrality.plotting = options_data$centralityline,
                    binwidth = options_data$binwidth,
                    test.value = options_data$test.value,
                    conf.level = options_data$conf.level,
                    bf.message = options_data$bf.message,
                    digits = options_data$digits,
                    xlab = aesthetics_data$xlab,
                    title = aesthetics_data$title,
                    subtitle = aesthetics_data$subtitle,
                    caption = aesthetics_data$caption,
                    bin.args = aesthetics_data$bin.args,
                    centrality.line.args = aesthetics_data$centrality.line.args
                )
                
                # Add grouping variable if provided
                if (!is.null(grvar_sym)) {
                    base_args$grouping.var <- grvar_sym
                }
                
                # Add centrality.type if specified
                if (!is.null(options_data$centrality.type)) {
                    base_args$centrality.type <- options_data$centrality.type
                }
                
                # Remove NULL arguments to prevent conflicts
                base_args <- base_args[!sapply(base_args, is.null)]
                
                # Call appropriate function based on grouping
                if (is.null(grvar_sym)) {
                    do.call(ggstatsplot::gghistostats, base_args)
                } else {
                    do.call(ggstatsplot::grouped_gghistostats, base_args)
                }
            },
            
            # Consolidated input validation
            .validateInputs = function() {
                if (is.null(self$options$dep))
                    return(FALSE)
                    
                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')
                    
                return(TRUE)
            },
            
            # Optimized aesthetic preparation with caching
            .prepareAesthetics = function(force_refresh = FALSE) {
                if (!is.null(private$.processedAesthetics) && !force_refresh) {
                    return(private$.processedAesthetics)
                }

                # Process bin.args
                bin.args <- list(
                    fill = self$options$binfill,
                    color = self$options$bincolor,
                    alpha = self$options$binalpha
                )
                
                # Process centrality.line.args
                centrality.line.args <- list(
                    color = self$options$centralitylinecolor,
                    linewidth = self$options$centralitylinewidth,
                    linetype = self$options$centralitylinetype
                )
                
                # Process text parameters
                xlab <- if (self$options$xlab != '') self$options$xlab else NULL
                title <- if (self$options$title != '') self$options$title else NULL
                subtitle <- if (self$options$subtitle != '') self$options$subtitle else NULL
                caption <- if (self$options$caption != '') self$options$caption else NULL
                
                aesthetics_list <- list(
                    bin.args = bin.args,
                    centrality.line.args = centrality.line.args,
                    xlab = xlab,
                    title = title,
                    subtitle = subtitle,
                    caption = caption
                )
                
                private$.processedAesthetics <- aesthetics_list
                return(aesthetics_list)
            },

            # Optimized options preparation with caching
            .prepareOptions = function(force_refresh = FALSE) {
                if (!is.null(private$.processedOptions) && !force_refresh) {
                    return(private$.processedOptions)
                }

                # Prepare options with progress feedback
                self$results$todo$setContent(
                    glue::glue("<br>Preparing histogram analysis options...<br><hr>")
                )

                # Process core analysis options
                typestatistics <- self$options$typestatistics
                dep <- self$options$dep
                
                # Process binwidth
                binwidth <- NULL
                if (self$options$changebinwidth) {
                    binwidth <- self$options$binwidth
                }
                
                # Process centrality.type
                centrality.type <- if (self$options$centralitytype != 'default') self$options$centralitytype else NULL
                
                # Cache the processed options
                options_list <- list(
                    typestatistics = typestatistics,
                    dep = dep,
                    binwidth = binwidth,
                    resultssubtitle = self$options$resultssubtitle,
                    centralityline = self$options$centralityline,
                    test.value = self$options$test.value,
                    conf.level = self$options$conf.level,
                    bf.message = self$options$bf.message,
                    digits = self$options$digits,
                    centrality.type = centrality.type
                )
                private$.processedOptions <- options_list
                return(options_list)
            },

            # run ----
            .run = function() {
                ## Initial Message ----
                if (is.null(self$options$dep)) {

                    ## todo ----

                    todo <- glue::glue(
                    "<br>
                    Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Histograms.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/gghistostats.html' target='_blank'>gghistostats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_gghistostats.html' target='_blank'>grouped_gghistostats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                    )

                    self$results$todo$setContent(todo)

                    return()

                } else {

                    todo <- glue::glue("<br>You have selected to make a histogram.<br><hr>")

                    self$results$todo$setContent(todo)

                    if (nrow(self$data) == 0)
                        stop('Data contains no (complete) rows')

                    # Pre-process data, options, and aesthetics for performance
                    private$.prepareData()
                    private$.prepareOptions()
                    private$.prepareAesthetics()

                }
            }

            ,
            .plot = function(image, ggtheme, theme, ...) {
                # Main plot generation function
                
                # Validate inputs using shared helper
                if (!private$.validateInputs())
                    return()

                # Use cached data, options, and aesthetics for performance
                mydata <- private$.prepareData()
                options_data <- private$.prepareOptions()
                aesthetics_data <- private$.prepareAesthetics()
                
                dep <- options_data$dep

                # Single variable plot
                if (length(self$options$dep) == 1) {
                    plot <- private$.generateHistogram(
                        data = mydata,
                        x_var = dep,
                        options_data = options_data,
                        aesthetics_data = aesthetics_data
                    )
                }

                # Multiple variable plots
                if (length(self$options$dep) > 1) {
                    dep2 <- as.list(self$options$dep)

                    plotlist <- purrr::map(
                        dep2,
                        function(x_var) {
                            private$.generateHistogram(
                                data = mydata,
                                x_var = x_var,
                                options_data = options_data,
                                aesthetics_data = aesthetics_data,
                                messages = FALSE
                            )
                        }
                    )

                    plot <- ggstatsplot::combine_plots(
                        plotlist = plotlist,
                        plotgrid.args = list(ncol = 1)
                    )
                }

                # Print plot
                print(plot)
                TRUE
            }


            ,
            .plot2 = function(image, ggtheme, theme, ...) {
                # Grouped plot generation function
                
                # Validate inputs
                if (is.null(self$options$dep) || is.null(self$options$grvar))
                    return()
                    
                if (!private$.validateInputs())
                    return()

                # Use cached data, options, and aesthetics for performance
                mydata <- private$.prepareData()
                options_data <- private$.prepareOptions()
                aesthetics_data <- private$.prepareAesthetics()
                
                dep <- options_data$dep
                grvar <- self$options$grvar

                # Single variable grouped plot
                if (length(self$options$dep) == 1) {
                    plot2 <- private$.generateHistogram(
                        data = mydata,
                        x_var = dep,
                        options_data = options_data,
                        aesthetics_data = aesthetics_data,
                        grvar_sym = rlang::sym(grvar)
                    )
                }

                # Multiple variable grouped plots
                if (length(self$options$dep) > 1) {
                    dep2 <- as.list(self$options$dep)

                    plotlist <- purrr::map(
                        dep2,
                        function(x_var) {
                            private$.generateHistogram(
                                data = mydata,
                                x_var = x_var,
                                options_data = options_data,
                                aesthetics_data = aesthetics_data,
                                grvar_sym = rlang::sym(grvar),
                                messages = FALSE
                            )
                        }
                    )

                    plot2 <- ggstatsplot::combine_plots(
                        plotlist = plotlist,
                        plotgrid.args = list(ncol = 1)
                    )
                }

                # Print plot
                print(plot2)
                TRUE
            }
        )
    )