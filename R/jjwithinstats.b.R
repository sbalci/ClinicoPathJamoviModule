#' @title Violin Plots to Compare Within Group
#' @importFrom R6 R6Class
#' @import jmvcore
#'


jjwithinstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjwithinstatsClass",
    inherit = jjwithinstatsBase,
    private = list(
        .prepared_data = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,

        # init ----

        .init = function() {

            self$results$plot$setSize(650, 450)

            if (!is.null(self$options$dep3) || !is.null(self$options$dep4))
                self$results$plot$setSize(900, 600)

            # if (!is.null(self$options$dep3) && !is.null(self$options$dep4))
            #     self$results$plot$setSize(800, 600)
            
            # Pre-prepare data and options for performance
            private$.prepareData()
            private$.prepareOptions()

        },
        
        # Performance optimization methods ----
        
        .prepareData = function() {
            # Create a simple hash of current data to detect changes
            vars <- c(self$options$dep1, self$options$dep2, self$options$dep3, self$options$dep4)
            vars <- vars[!sapply(vars, is.null)]
            current_hash <- paste(vars, nrow(self$data), collapse = "_")
            
            # Only reprocess if data has changed
            if (is.null(private$.data_hash) || private$.data_hash != current_hash) {
                
                if (!is.null(self$options$dep1) && !is.null(self$options$dep2)) {
                    mydata <- self$data
                    mydata$rowid <- seq.int(nrow(mydata))
                    
                    # Convert variables to numeric once
                    for (var in vars)
                        mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])
                    
                    # Remove NA values once
                    mydata <- jmvcore::naOmit(mydata)
                    
                    # Perform pivot_longer transformation once
                    long_data <- tidyr::pivot_longer(
                        mydata,
                        cols = vars,
                        names_to = "measurement",
                        values_to = "value"
                    )
                    
                    long_data$measurement <- factor(long_data$measurement, levels = vars)
                    
                    private$.prepared_data <- long_data
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
                
                # Process formulas once
                typestatistics <- jmvcore::constructFormula(terms = self$options$typestatistics)
                pairwisedisplay <- jmvcore::constructFormula(terms = self$options$pairwisedisplay)
                padjustmethod <- jmvcore::constructFormula(terms = self$options$padjustmethod)
                
                # Process titles once
                mytitle <- self$options$mytitle
                xtitle <- if (self$options$xtitle == '') NULL else self$options$xtitle
                ytitle <- if (self$options$ytitle == '') NULL else self$options$ytitle
                
                # Process plot component arguments once
                violinargs <- if (self$options$violin) {
                    list(width = 0.5, alpha = 0.2, na.rm = TRUE)
                } else {
                    list(width = 0)
                }
                
                boxplotargs <- if (self$options$boxplot) {
                    list(width = 0.2, alpha = 0.5, na.rm = TRUE)
                } else {
                    list(width = 0)
                }
                
                pointargs <- if (self$options$point) {
                    list(alpha = 0.5, linetype = "dashed")
                } else {
                    list(alpha = 0)
                }
                
                private$.prepared_options <- list(
                    typestatistics = typestatistics,
                    pairwisecomparisons = self$options$pairwisecomparisons,
                    pairwisedisplay = pairwisedisplay,
                    padjustmethod = padjustmethod,
                    pointpath = self$options$pointpath,
                    mytitle = mytitle,
                    xtitle = xtitle,
                    ytitle = ytitle,
                    effsizetype = self$options$effsizetype,
                    centralityplotting = self$options$centralityplotting,
                    centralitytype = self$options$centralitytype,
                    centralitypath = self$options$centralitypath,
                    violinargs = violinargs,
                    boxplotargs = boxplotargs,
                    pointargs = pointargs,
                    originaltheme = self$options$originaltheme,
                    resultssubtitle = self$options$resultssubtitle
                )
            }
            
            return(private$.prepared_options)
        }

        # run ----
        ,
        .run = function() {

            ## Initial Message ----
            if ( is.null(self$options$dep1) || is.null(self$options$dep2)) {

                ### todo ----

                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Violin Plots for repeated measurements.
                <br><br>
                The data should be in wide format: Each row should have a unique case. Columns should have separate measurements. This function does not allow missing values.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggwithinstats.html' target='_blank'>ggwithinstats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggwithinstats.html' target='_blank'>grouped_ggwithinstats</a>.
                Please see above links for further information.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                ### todo ----
                todo <- glue::glue(
                "<br>You have selected to use a Violin Plot to Compare repeated measurements.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

            }
        }


        # the plot function ----
        ,
        .plot = function(image, ggtheme, theme, ...) {

            ## Error messages ----

            if (is.null(self$options$dep1) ||
                is.null(self$options$dep2))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Use prepared data and options ----
            long_data <- private$.prepareData()
            opts <- private$.prepareOptions()
            
            if (is.null(long_data)) {
                return()
            }

            # Additional validation
            if (nrow(long_data) == 0) {
                warning_msg <- "<br>No complete data rows available after removing missing values.<br>Please check your data for missing values in the selected variables.<br><hr>"
                self$results$todo$setContent(warning_msg)
                return()
            }

            # Create plot using optimized data and options ----
            tryCatch({
                plot <- ggstatsplot::ggwithinstats(
                    data = long_data,
                    x = measurement,
                    y = value,
                    paired = TRUE,
                    id = "rowid",
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
                    point.path = opts$pointpath,
                    centrality.path = opts$centralitypath,
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

                # Print Plot ----
                print(plot)
                TRUE
                
            }, error = function(e) {
                error_msg <- paste0(
                    "<br>Error creating within-subjects plot: ", e$message,
                    "<br><br>Please check that:",
                    "<br>• All measurement variables contain numeric values",
                    "<br>• Data has at least 2 complete rows",
                    "<br>• Variables have sufficient variance for statistical tests",
                    "<br>• No extreme outliers that might affect analysis",
                    "<br><hr>"
                )
                self$results$todo$setContent(error_msg)
            })

        }

    )
)
