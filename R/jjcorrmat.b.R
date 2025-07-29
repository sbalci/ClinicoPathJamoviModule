#' @title Correlation Matrix
#' @importFrom R6 R6Class
#' @import jmvcore
#'


jjcorrmatClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjcorrmatClass",
    inherit = jjcorrmatBase,
    private = list(

        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .processedOptions = NULL,

        # init ----
        .init = function() {

            deplen <- length(self$options$dep)

            self$results$plot$setSize(600, 450)


            if (!is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                num_levels <- nlevels(
                    as.factor(mydata[[grvar]])
                )

                self$results$plot2$setSize(num_levels * 600, 450)

            }



        },

        # Optimized data preparation with caching
        .prepareData = function(force_refresh = FALSE) {
            if (!is.null(private$.processedData) && !force_refresh) {
                return(private$.processedData)
            }

            # Prepare data with progress feedback
            self$results$todo$setContent(
                glue::glue("<br>Processing data for correlation analysis...<br><hr>")
            )

            mydata <- self$data

            # Exclude NA with checkpoint
            private$.checkpoint()
            mydata <- jmvcore::naOmit(mydata)

            # Cache the processed data
            private$.processedData <- mydata
            return(mydata)
        },

        # Optimized options preparation with caching
        .prepareOptions = function(force_refresh = FALSE) {
            if (!is.null(private$.processedOptions) && !force_refresh) {
                return(private$.processedOptions)
            }

            # Prepare options with progress feedback
            self$results$todo$setContent(
                glue::glue("<br>Preparing correlation analysis options...<br><hr>")
            )

            # Process type of statistics
            typestatistics <- self$options$typestatistics

            # Process variables - dep is already a list of variables
            myvars <- self$options$dep
            
            # Process text parameters
            title <- if (self$options$title != '') self$options$title else NULL
            subtitle <- if (self$options$subtitle != '') self$options$subtitle else NULL
            caption <- if (self$options$caption != '') self$options$caption else NULL
            
            # Process colors
            colors <- c(self$options$lowcolor, self$options$midcolor, self$options$highcolor)
            
            # Process ggcorrplot.args
            ggcorrplot.args <- list(
                method = self$options$matrixmethod,
                outline.color = "black"
            )

            # Cache the processed options
            options_list <- list(
                typestatistics = typestatistics,
                myvars = myvars,
                matrixtype = self$options$matrixtype,
                ggcorrplot.args = ggcorrplot.args,
                siglevel = self$options$siglevel,
                conflevel = self$options$conflevel,
                padjustmethod = self$options$padjustmethod,
                k = self$options$k,
                colors = colors,
                title = title,
                subtitle = subtitle,
                caption = caption
            )
            private$.processedOptions <- options_list
            return(options_list)
        }

            # run ----
            ,
            .run = function() {

            # Initial Message ----
            if ( length(self$options$dep) <= 1 ) {

                # TODO ----

                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Correlation Matrix Charts.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.html' target='_blank'>ggcorrmat</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggcorrmat.html' target='_blank'>grouped_ggcorrmat</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                # TODO ----
                todo <- glue::glue(
                    "<br>You have selected to use a correlation matrix to compare continuous variables.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                # Pre-process data and options for performance
                private$.prepareData()
                private$.prepareOptions()

            }
        }



        ,
        .plot = function(image, ggtheme, theme, ...) {
            # the plot function ----
            # Error messages ----

            if ( length(self$options$dep) <= 1 )
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()
            
            typestatistics <- options_data$typestatistics
            myvars <- options_data$myvars


            # ggcorrmat ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggcorrmat.html



            plot <- ggstatsplot::ggcorrmat(
                data = mydata,
                cor.vars = myvars,
                cor.vars.names = NULL,
                output = "plot",
                matrix.type = options_data$matrixtype,

                type = options_data$typestatistics,

                beta = 0.1,
                k = options_data$k,
                sig.level = options_data$siglevel,
                conf.level = options_data$conflevel,
                bf.prior = 0.707,
                p.adjust.method = options_data$padjustmethod,
                pch = "cross",
                ggcorrplot.args = options_data$ggcorrplot.args,
                package = "RColorBrewer",
                palette = "Dark2",
                colors = options_data$colors,

                ggplot.component = NULL,
                title = options_data$title,
                subtitle = options_data$subtitle,
                caption = options_data$caption,
                messages = TRUE

            )


            # originaltheme <- self$options$originaltheme
            #
            # if (!originaltheme) {
            #     plot <- plot + ggtheme
            # } else {
            #     plot <- plot + ggstatsplot::theme_ggstatsplot()
            #     # ggplot2::theme_bw()
            # }



            # Print Plot ----

            print(plot)
            TRUE

        }


        ,

        .plot2 = function(image, ggtheme, theme, ...) {
            # the plot function ----
            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$grvar))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()
            
            typestatistics <- options_data$typestatistics
            myvars <- options_data$myvars


            # grouped_ggcorrmat ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggcorrmat.html



            if ( !is.null(self$options$grvar) ) {

                grvar <- self$options$grvar

                plot2 <- ggstatsplot::grouped_ggcorrmat(
                    data = mydata,
                    cor.vars = myvars,
                    cor.vars.names = NULL,
                    grouping.var = !!rlang::sym(grvar),
                    title.prefix = NULL,
                    output = "plot",
                    plotgrid.args = list(),
                    title.text = options_data$title,
                    title.args = list(size = 16, fontface = "bold"),
                    caption.text = options_data$caption,
                    caption.args = list(size = 10),
                    sub.text = options_data$subtitle,
                    sub.args = list(size = 12)
                    , ggtheme = ggtheme
                    , ggstatsplot.layer = TRUE
                    , type = options_data$typestatistics
                    , matrix.type = options_data$matrixtype
                    , ggcorrplot.args = options_data$ggcorrplot.args
                    , k = options_data$k
                    , sig.level = options_data$siglevel
                    , conf.level = options_data$conflevel
                    , p.adjust.method = options_data$padjustmethod
                    , colors = options_data$colors



                )


            }


            # originaltheme <- self$options$originaltheme
            #
            # if (!originaltheme) {
            #     plot <- plot + ggtheme
            # } else {
            #     plot <- plot + ggstatsplot::theme_ggstatsplot()
            #     # ggplot2::theme_bw()
            # }

            # Print Plot ----

            print(plot2)
            TRUE

        }

    )
)







