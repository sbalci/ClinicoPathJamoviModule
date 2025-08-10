#' @title Violin Plots to Compare Within Group
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#' @import tidyr
#' @import ggplot2
#' @importFrom rlang sym
#' @importFrom digest digest
#'


jjwithinstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjwithinstatsClass",
    inherit = jjwithinstatsBase,
    private = list(
        .prepared_data = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .messages = NULL,

        # init ----

        .init = function() {
            # Use configurable plot dimensions
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 650
            plotheight <- if (!is.null(self$options$plotheight)) self$options$plotheight else 450
            
            self$results$plot$setSize(plotwidth, plotheight)

            if (!is.null(self$options$dep3) || !is.null(self$options$dep4)) {
                # Larger size for 3-4 measurements
                self$results$plot$setSize(plotwidth + 250, plotheight + 150)
            }

            
            # Pre-prepare data and options for performance
            private$.prepareData()
            private$.prepareOptions()

        },
        
        # Message accumulation helper
        .accumulateMessage = function(message) {
            if (is.null(private$.messages)) {
                private$.messages <- character()
            }
            private$.messages <- append(private$.messages, message)
            self$results$todo$setContent(paste(private$.messages, collapse = ""))
        },
        
        # Shared validation helper
        .validateInputs = function() {
            if (is.null(self$options$dep1) || is.null(self$options$dep2))
                return(FALSE)
            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')
            
            # Check variable existence
            vars <- c(self$options$dep1, self$options$dep2, 
                      self$options$dep3, self$options$dep4)
            vars <- vars[!sapply(vars, is.null)]
            
            for (var in vars) {
                if (!(var %in% names(self$data)))
                    stop(paste('Variable "', var, '" not found in data'))
            }
            return(TRUE)
        },
        
        # Data quality validation helper
        .validateDataQuality = function(mydata, vars) {
            for (var in vars) {
                num_vals <- jmvcore::toNumeric(mydata[[var]])
                num_vals <- num_vals[!is.na(num_vals)]
                
                if (length(num_vals) < 3) {
                    private$.accumulateMessage(
                        glue::glue("<br>⚠️ Warning: {var} has less than 3 valid observations<br>")
                    )
                }
                if (length(unique(num_vals)) < 2) {
                    private$.accumulateMessage(
                        glue::glue("<br>⚠️ Warning: {var} has no variation (all values are the same)<br>")
                    )
                }
            }
        },

        # Optimized data preparation with robust caching
        .prepareData = function(force_refresh = FALSE) {
            # Create robust hash of current data to detect changes
            vars <- Filter(Negate(is.null), c(self$options$dep1, self$options$dep2, 
                                             self$options$dep3, self$options$dep4))
            current_hash <- digest::digest(list(
                dep1 = self$options$dep1, dep2 = self$options$dep2,
                dep3 = self$options$dep3, dep4 = self$options$dep4,
                data_dim = dim(self$data), col_names = names(self$data)
            ), algo = "md5")
            
            # Only reprocess if data has changed or forced refresh
            if (!is.null(private$.prepared_data) && 
                private$.data_hash == current_hash && 
                !force_refresh) {
                return(private$.prepared_data)
            }
            
            # Add processing feedback
            private$.accumulateMessage(
                glue::glue("<br>Processing {length(vars)} measurements for within-subjects analysis...<br>")
            )
            
            if (!is.null(self$options$dep1) && !is.null(self$options$dep2)) {
                mydata <- self$data
                mydata$rowid <- seq.int(nrow(mydata))
                
                # Convert variables to numeric once
                for (var in vars)
                    mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])
                
                # Validate data quality before processing
                private$.validateDataQuality(mydata, vars)
                
                # Remove NA values once
                mydata <- jmvcore::naOmit(mydata)
                
                if (nrow(mydata) == 0) {
                    private$.accumulateMessage(
                        "<br>❌ No complete observations after removing missing values<br>"
                    )
                    private$.prepared_data <- NULL
                    return(NULL)
                }
                
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
            
            return(private$.prepared_data)
        },
        
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
                pointpath = self$options$pointpath,
                centralitypath = self$options$centralitypath,
                violin = self$options$violin,
                boxplot = self$options$boxplot,
                point = self$options$point,
                titles = list(self$options$mytitle, self$options$xtitle, self$options$ytitle),
                display = list(self$options$resultssubtitle, self$options$originaltheme),
                advanced = list(self$options$bfmessage, self$options$conflevel, self$options$k)
            ), algo = "md5")
            
            # Only reprocess if options have changed or forced refresh
            if (!is.null(private$.prepared_options) && 
                private$.options_hash == current_options_hash && 
                !force_refresh) {
                return(private$.prepared_options)
            }
            
            # Process options directly (no formula construction needed for simple strings)
            typestatistics <- self$options$typestatistics
            pairwisedisplay <- self$options$pairwisedisplay
            padjustmethod <- self$options$padjustmethod
            
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
            
            # Fix API parameter name for ggwithinstats
            boxargs <- if (self$options$boxplot) {
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
                boxargs = boxargs,  # Fixed parameter name
                pointargs = pointargs,
                originaltheme = self$options$originaltheme,
                resultssubtitle = self$options$resultssubtitle,
                # Add missing parameters
                bfmessage = if (!is.null(self$options$bfmessage)) self$options$bfmessage else TRUE,
                conflevel = if (!is.null(self$options$conflevel)) self$options$conflevel else 0.95,
                k = if (!is.null(self$options$k)) self$options$k else 2
            )
            
            private$.options_hash <- current_options_hash
            
            return(private$.prepared_options)
        }

        # run ----
        ,
        .run = function() {
            # Clear messages at start of new run
            private$.messages <- NULL
            
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
            # Use shared validation helper ----
            if (!private$.validateInputs())
                return()

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
                    box.args = opts$boxargs,  # Fixed API parameter name
                    point.args = opts$pointargs,
                    results.subtitle = opts$resultssubtitle,
                    # Add missing parameters
                    bf.message = opts$bfmessage,
                    conf.level = opts$conflevel,
                    k = opts$k
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
