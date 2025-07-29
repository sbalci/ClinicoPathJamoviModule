#' @title Scatter Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#'

jjscatterstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjscatterstatsClass",
    inherit = jjscatterstatsBase,
    private = list(
        .prepared_data = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,

        # init ----

        .init = function() {

            self$results$plot$setSize(600, 450)


            if (!is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                num_levels <- nlevels(
                    as.factor(mydata[[grvar]])
                )

                self$results$plot2$setSize(num_levels * 600, 450)

            }

            # Pre-prepare data and options for performance
            private$.prepareData()
            private$.prepareOptions()

        },

        # Performance optimization methods ----
        
        .prepareData = function() {
            # Create a hash of current data to detect changes
            current_hash <- paste(self$options$dep, self$options$group, nrow(self$data), collapse = "_")
            
            # Only reprocess if data has changed
            if (is.null(private$.data_hash) || private$.data_hash != current_hash) {
                
                if (!is.null(self$options$dep) && !is.null(self$options$group)) {
                    mydata <- self$data
                    
                    # Convert variables to numeric
                    vars <- self$options$dep
                    for (var in vars) {
                        mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])
                    }
                    mydata[[self$options$group]] <- jmvcore::toNumeric(mydata[[self$options$group]])
                    
                    # Exclude NA values once
                    mydata <- jmvcore::naOmit(mydata)
                    
                    private$.prepared_data <- mydata
                    private$.data_hash <- current_hash
                } else {
                    private$.prepared_data <- NULL
                }
            }
            
            return(private$.prepared_data)
        },
        
        .prepareOptions = function() {
            # Cache processed options
            if (is.null(private$.prepared_options)) {
                
                # Process type statistics
                typestatistics <- self$options$typestatistics
                
                # Process titles
                mytitle <- if (self$options$mytitle == '') NULL else self$options$mytitle
                xtitle <- if (self$options$xtitle == '') NULL else self$options$xtitle
                ytitle <- if (self$options$ytitle == '') NULL else self$options$ytitle
                
                # Process point.args
                point.args <- list(
                    size = self$options$pointsize,
                    alpha = self$options$pointalpha
                )
                
                # Process smooth.line.args
                smooth.line.args <- list(
                    linewidth = self$options$smoothlinesize,
                    color = self$options$smoothlinecolor
                )
                
                # Process marginal histogram args
                xsidehistogram.args <- list(
                    fill = self$options$xsidefill,
                    color = "black",
                    na.rm = TRUE
                )
                
                ysidehistogram.args <- list(
                    fill = self$options$ysidefill,
                    color = "black", 
                    na.rm = TRUE
                )
                
                private$.prepared_options <- list(
                    typestatistics = typestatistics,
                    mytitle = mytitle,
                    xtitle = xtitle,
                    ytitle = ytitle,
                    dep = self$options$dep,
                    group = self$options$group,
                    grvar = self$options$grvar,
                    resultssubtitle = self$options$resultssubtitle,
                    originaltheme = self$options$originaltheme,
                    conflevel = self$options$conflevel,
                    bfmessage = self$options$bfmessage,
                    k = self$options$k,
                    marginal = self$options$marginal,
                    xsidehistogram.args = xsidehistogram.args,
                    ysidehistogram.args = ysidehistogram.args,
                    point.args = point.args,
                    smooth.line.args = smooth.line.args
                )
            }
            
            return(private$.prepared_options)
        }

        # run ----
        ,
        .run = function() {

            # Initial Message ----
            if ( is.null(self$options$dep) || is.null(self$options$group)) {

                # todo ----

                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Scatter Plot.
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
                    "<br>You have selected to use a scatter plot.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

            }
        }



        ,
        .plot = function(image, ggtheme, theme, ...) {
            # the plot function ----
            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$group))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Use prepared data and options ----
            mydata <- private$.prepareData()
            opts <- private$.prepareOptions()
            
            if (is.null(mydata)) {
                return()
            }

            # ggscatterstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggscatterstats.html

            plot <-
                ggstatsplot::ggscatterstats(
                    data = mydata,
                    x = !!rlang::sym(opts$dep),
                    y = !!rlang::sym(opts$group)

                    , type = opts$typestatistics
                    , conf.level = opts$conflevel
                    , bf.message = opts$bfmessage
                    , k = opts$k

                    , title = opts$mytitle
                    , xlab = opts$xtitle
                    , ylab = opts$ytitle

                    , results.subtitle = opts$resultssubtitle
                    
                    , marginal = opts$marginal
                    , xsidehistogram.args = opts$xsidehistogram.args
                    , ysidehistogram.args = opts$ysidehistogram.args
                    
                    , point.args = opts$point.args
                    , smooth.line.args = opts$smooth.line.args

                )

            # Apply theme ----
            if (!opts$originaltheme) {
                plot <- plot + ggtheme
            } else {
                plot <- plot + ggstatsplot::theme_ggstatsplot()
                # ggplot2::theme_bw()
            }

            # Print Plot ----
            print(plot)
            TRUE

        }


        ,
        .plot2 = function(image, ggtheme, theme, ...) {
            # the plot function ----
            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$group) || is.null(self$options$grvar))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Use prepared data and options ----
            mydata <- private$.prepareData()
            opts <- private$.prepareOptions()
            
            if (is.null(mydata) || is.null(opts$grvar)) {
                return()
            }

            # grouped_ggscatterstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggscatterstats.html

            plot2 <- ggstatsplot::grouped_ggscatterstats(
                data = mydata,
                x = !!rlang::sym(opts$dep),
                y = !!rlang::sym(opts$group),
                grouping.var = !!rlang::sym(opts$grvar),

                , type = opts$typestatistics
                , conf.level = opts$conflevel
                , bf.message = opts$bfmessage
                , k = opts$k
                
                # Note: title is omitted for grouped plots as they auto-generate titles
                , xlab = opts$xtitle
                , ylab = opts$ytitle

                , results.subtitle = opts$resultssubtitle
                
                , marginal = opts$marginal
                , xsidehistogram.args = opts$xsidehistogram.args
                , ysidehistogram.args = opts$ysidehistogram.args
                
                , point.args = opts$point.args
                , smooth.line.args = opts$smooth.line.args

            )

            # Apply theme ----
            if (!opts$originaltheme) {
                plot2 <- plot2 + ggtheme
            } else {
                plot2 <- plot2 + ggstatsplot::theme_ggstatsplot()
                # ggplot2::theme_bw()
            }

            # Print Plot ----
            print(plot2)
            TRUE
        }

    )
)
