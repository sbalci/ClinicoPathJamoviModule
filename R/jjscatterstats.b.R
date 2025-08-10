#' @title Scatter Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#' @import ggplot2
#' @importFrom digest digest
#'

jjscatterstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjscatterstatsClass",
    inherit = jjscatterstatsBase,
    private = list(
        .prepared_data = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,
        .options_hash = NULL,

        # init ----

        .init = function() {

            # Use configurable plot dimensions
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 600
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450

            self$results$plot$setSize(plotwidth, plotheight)

            if (!is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                num_levels <- nlevels(
                    as.factor(mydata[[grvar]])
                )

                self$results$plot2$setSize(num_levels * plotwidth, plotheight)

            }

            # Pre-prepare data and options for performance
            private$.prepareData()
            private$.prepareOptions()

        },

        # Performance optimization methods ----
        
        .prepareData = function() {
            # Create a robust hash of current data to detect changes
            current_hash <- digest::digest(list(
                dep = self$options$dep,
                group = self$options$group,
                data_dim = dim(self$data),
                col_names = names(self$data),
                grvar = self$options$grvar
            ), algo = "md5")
            
            # Only reprocess if data has changed
            if (is.null(private$.data_hash) || private$.data_hash != current_hash) {
                
                if (!is.null(self$options$dep) && !is.null(self$options$group)) {
                    mydata <- self$data
                    
                    # Convert variables to numeric (fix: process both dep and group consistently)
                    mydata[[self$options$dep]] <- jmvcore::toNumeric(mydata[[self$options$dep]])
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
        
        .prepareOptions = function(force_refresh = FALSE) {
            # Create robust hash of current options to detect changes
            current_options_hash <- digest::digest(list(
                typestatistics = self$options$typestatistics,
                conflevel = self$options$conflevel,
                bfmessage = self$options$bfmessage,
                k = self$options$k,
                marginal = self$options$marginal,
                titles = list(self$options$mytitle, self$options$xtitle, self$options$ytitle),
                display = list(self$options$resultssubtitle, self$options$originaltheme),
                aesthetics = list(
                    self$options$pointsize, self$options$pointalpha,
                    self$options$smoothlinesize, self$options$smoothlinecolor,
                    self$options$xsidefill, self$options$ysidefill
                ),
                dimensions = list(self$options$plotwidth, self$options$plotheight)
            ), algo = "md5")
            
            # Only reprocess if options have changed or forced refresh
            if (is.null(private$.prepared_options) || private$.options_hash != current_options_hash || force_refresh) {
                
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
                
                # Process marginal histogram args (using current ggstatsplot API)
                xparams <- list(
                    fill = self$options$xsidefill,
                    color = "black",
                    na.rm = TRUE
                )
                
                yparams <- list(
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
                    xparams = xparams,
                    yparams = yparams,
                    point.args = point.args,
                    smooth.line.args = smooth.line.args
                )
                
                private$.options_hash <- current_options_hash
            }
            
            return(private$.prepared_options)
        },

        # Shared validation helper ----
        .validateInputs = function() {
            if (is.null(self$options$dep) || is.null(self$options$group))
                return(FALSE)
            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')
                
            # Enhanced validation for correlation analysis
            mydata <- self$data
            
            # Check if variables exist in data
            if (!(self$options$dep %in% names(mydata)))
                stop(paste('X-axis variable "', self$options$dep, '" not found in data'))
            if (!(self$options$group %in% names(mydata)))
                stop(paste('Y-axis variable "', self$options$group, '" not found in data'))
            
            # Convert to numeric for validation
            x_vals <- jmvcore::toNumeric(mydata[[self$options$dep]])
            y_vals <- jmvcore::toNumeric(mydata[[self$options$group]])
            
            # Remove NA values for validation
            x_vals <- x_vals[!is.na(x_vals)]
            y_vals <- y_vals[!is.na(y_vals)]
            
            # Check for minimum sample size
            if (length(x_vals) < 3 || length(y_vals) < 3)
                stop('Correlation analysis requires at least 3 complete observations')
                
            # Check for constant variables (no variation)
            if (length(unique(x_vals)) < 2)
                stop('X-axis variable must have at least 2 unique values for correlation analysis')
            if (length(unique(y_vals)) < 2)
                stop('Y-axis variable must have at least 2 unique values for correlation analysis')
                
            return(TRUE)
        },

        # Shared theme application helper ----
        .applyTheme = function(plot, opts) {
            if (!opts$originaltheme) {
                plot + ggplot2::theme_bw()
            } else {
                plot + ggstatsplot::theme_ggstatsplot()
            }
        },

        # run ----
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
            # Use shared validation ----
            if (!private$.validateInputs())
                return()

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
                    , xparams = opts$xparams
                    , yparams = opts$yparams
                    
                    , point.args = opts$point.args
                    , smooth.line.args = opts$smooth.line.args

                )

            # Apply theme using shared helper ----
            plot <- private$.applyTheme(plot, opts)

            # Print Plot ----
            print(plot)
            TRUE

        }


        ,
        .plot2 = function(image, ggtheme, theme, ...) {
            # Use shared validation with additional grouping variable check ----
            if (!private$.validateInputs() || is.null(self$options$grvar))
                return()

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
                , xparams = opts$xparams
                , yparams = opts$yparams
                
                , point.args = opts$point.args
                , smooth.line.args = opts$smooth.line.args

            )

            # Apply theme using shared helper ----
            plot2 <- private$.applyTheme(plot2, opts)

            # Print Plot ----
            print(plot2)
            TRUE
        }

    )
)
