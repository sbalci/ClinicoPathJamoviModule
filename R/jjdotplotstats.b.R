#' @title Dot Chart
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#' @import ggplot2
#' @importFrom rlang sym
#' @importFrom digest digest
#'


jjdotplotstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjdotplotstatsClass",
    inherit = jjdotplotstatsBase,
    private = list(
        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .processedOptions = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .messages = NULL,

        # init ----

        .init = function() {
            # Since dep is single variable, use fixed size
            # Use configurable plot dimensions
            plotwidth <- if (!is.null(self$options$plotwidth)) self$options$plotwidth else 650
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

        }


,
        # Shared validation helper
        .validateInputs = function() {
            if (is.null(self$options$dep) || is.null(self$options$group))
                return(FALSE)
            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')
            
            # Check variable existence
            if (!(self$options$dep %in% names(self$data)))
                stop(paste('Variable "', self$options$dep, '" not found in data'))
            if (!(self$options$group %in% names(self$data)))
                stop(paste('Variable "', self$options$group, '" not found in data'))
                
            return(TRUE)
        },
        
        # Message accumulation helper
        .accumulateMessage = function(message) {
            if (is.null(private$.messages)) {
                private$.messages <- character()
            }
            private$.messages <- append(private$.messages, message)
            self$results$todo$setContent(paste(private$.messages, collapse = ""))
        },
        
        # Data quality validation helper
        .validateDataQuality = function(mydata, dep_var) {
            num_vals <- jmvcore::toNumeric(mydata[[dep_var]])
            num_vals <- num_vals[!is.na(num_vals)]
            
            if (length(num_vals) < 3) {
                private$.accumulateMessage(
                    glue::glue("<br>⚠️ Warning: {dep_var} has less than 3 valid observations<br>")
                )
            }
            if (length(unique(num_vals)) < 2) {
                private$.accumulateMessage(
                    glue::glue("<br>⚠️ Warning: {dep_var} has no variation (all values are the same)<br>")
                )
            }
        },
        
        # Outlier detection helper
        .detectOutliers = function(data, var) {
            vals <- jmvcore::toNumeric(data[[var]])
            vals <- vals[!is.na(vals)]
            if (length(vals) > 0) {
                Q1 <- quantile(vals, 0.25, na.rm = TRUE)
                Q3 <- quantile(vals, 0.75, na.rm = TRUE)
                IQR <- Q3 - Q1
                outliers <- which(data[[var]] < (Q1 - 1.5 * IQR) | data[[var]] > (Q3 + 1.5 * IQR))
                if (length(outliers) > 0) {
                    private$.accumulateMessage(
                        glue::glue("<br>ℹ️ {length(outliers)} potential outlier(s) detected in {var}<br>")
                    )
                }
            }
        },
        
        # Statistical summary helper
        .addDataSummary = function(data, dep_var, group_var) {
            if (!is.null(dep_var) && !is.null(group_var)) {
                tryCatch({
                    summary_stats <- tapply(data[[dep_var]], data[[group_var]], 
                                           function(x) c(mean = mean(x, na.rm = TRUE), 
                                                        n = sum(!is.na(x))))
                    n_groups <- length(summary_stats)
                    total_n <- sum(sapply(summary_stats, function(x) x["n"]), na.rm = TRUE)
                    private$.accumulateMessage(
                        glue::glue("<br>📊 Analysis summary: {n_groups} groups, {total_n} total observations<br>")
                    )
                }, error = function(e) {
                    # Silently handle errors in summary calculation
                })
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

            # Clear previous messages and add processing feedback
            private$.messages <- NULL
            private$.accumulateMessage(
                glue::glue("<br>Processing data for dot plot analysis...<br><hr>")
            )
            
            # Track processing time for large datasets
            start_time <- Sys.time()

            mydata <- self$data
            
            # Convert dependent variable to numeric (single variable)
            dep_var <- self$options$dep
            if (!is.null(dep_var)) {
                mydata[[dep_var]] <- jmvcore::toNumeric(mydata[[dep_var]])
            }

            # Exclude NA with checkpoint
            private$.checkpoint()
            mydata <- jmvcore::naOmit(mydata)
            
            # Validate data quality
            if (!is.null(dep_var)) {
                private$.validateDataQuality(mydata, dep_var)
            }
            
            # Detect outliers for datasets with sufficient size
            if (nrow(mydata) > 10 && !is.null(dep_var)) {
                private$.detectOutliers(mydata, dep_var)
            }
            
            # Add statistical summary
            private$.addDataSummary(mydata, dep_var, self$options$group)
            
            # Add processing time feedback for large datasets
            elapsed <- difftime(Sys.time(), start_time, units = "secs")
            if (nrow(mydata) > 1000) {
                private$.accumulateMessage(
                    glue::glue("<br>✅ Large dataset processed in {round(elapsed, 2)} seconds<br>")
                )
            }

            # Cache the processed data with hash
            private$.processedData <- mydata
            private$.data_hash <- current_hash
            return(mydata)
        },

        # Optimized options preparation with robust caching
        .prepareOptions = function(force_refresh = FALSE) {
            # Create robust hash of current options to detect changes
            current_options_hash <- digest::digest(list(
                typestatistics = self$options$typestatistics,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                testvalue = self$options$testvalue,
                bfmessage = self$options$bfmessage,
                conflevel = self$options$conflevel,
                k = self$options$k,
                testvalueline = self$options$testvalueline,
                centralityparameter = self$options$centralityparameter,
                centralityk = self$options$centralityk,
                titles = list(self$options$mytitle, self$options$xtitle, self$options$ytitle),
                display = list(self$options$resultssubtitle, self$options$originaltheme)
            ), algo = "md5")
            
            # Only reprocess if options have changed or forced refresh
            if (!is.null(private$.processedOptions) && 
                private$.options_hash == current_options_hash && 
                !force_refresh) {
                return(private$.processedOptions)
            }

            # Add options preparation feedback if not already processing
            if (is.null(private$.messages)) {
                private$.accumulateMessage(
                    glue::glue("<br>Preparing dot plot analysis options...<br><hr>")
                )
            }

            # Process type of statistics
            typestatistics <- self$options$typestatistics

            # Process variables
            dep <- self$options$dep
            group <- self$options$group
            
            # Process titles
            mytitle <- self$options$mytitle
            if (mytitle == '') mytitle <- NULL
            
            xtitle <- self$options$xtitle
            if (xtitle == '') xtitle <- NULL
            
            ytitle <- self$options$ytitle
            if (ytitle == '') ytitle <- NULL
            
            # Cache the processed options with all parameters
            options_list <- list(
                typestatistics = typestatistics,
                dep = dep,
                group = group,
                mytitle = mytitle,
                xtitle = xtitle,
                ytitle = ytitle,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                testvalue = self$options$testvalue,
                bfmessage = self$options$bfmessage,
                conflevel = self$options$conflevel,
                k = self$options$k,
                testvalueline = self$options$testvalueline,
                centralityparameter = self$options$centralityparameter,
                centralityk = self$options$centralityk,
                resultssubtitle = self$options$resultssubtitle,
                originaltheme = self$options$originaltheme
            )
            
            # Process centrality parameters if enabled
            if (options_list$centralityplotting) {
                options_list$centrality.plotting <- TRUE
                options_list$centrality.type <- options_list$centralitytype
            } else {
                options_list$centrality.plotting <- FALSE
            }
            
            private$.processedOptions <- options_list
            private$.options_hash <- current_options_hash
            return(options_list)
        },

        # run ----
        .run = function() {
            # Clear messages at start of new run
            private$.messages <- NULL
            
            # Initial Message ----
            if ( is.null(self$options$dep) || is.null(self$options$group)) {

                # todo ----

                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Dot Charts.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations for <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggdotplotstats.html' target='_blank'>ggdotplotstats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggdotplotstats.html' target='_blank'>grouped_ggdotplotstats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                # todo ----
                todo <- glue::glue(
                    "<br>You have selected to use a Dot Plot to compare continuous variables by groups.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                # Pre-process data and options for performance with enhanced validation
                tryCatch({
                    private$.prepareData()
                    private$.prepareOptions()
                }, error = function(e) {
                    private$.accumulateMessage(
                        glue::glue("<br>❌ Error during processing: {e$message}<br>")
                    )
                    stop(paste("Data processing failed:", e$message, "\nPlease check your variable selections."))
                })

            }
        }


        ,
        .plot = function(image, ggtheme, theme, ...) {
            # Use shared validation helper ----
            if (!private$.validateInputs())
                return()

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()


            # ggdotplotstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggdotplotstats.html



            plot <- ggstatsplot::ggdotplotstats(
                data = mydata,
                x = !!rlang::sym(options_data$dep),
                y = !!rlang::sym(options_data$group),
                title = options_data$mytitle,
                xlab = options_data$xtitle,
                ylab = options_data$ytitle,
                type = options_data$typestatistics,
                test.value = options_data$testvalue,
                effsize.type = options_data$effsizetype,
                conf.level = options_data$conflevel,
                k = options_data$k,
                bf.message = options_data$bfmessage,
                test.value.line = options_data$testvalueline,
                centrality.parameter = options_data$centralityparameter,
                centrality.k = options_data$centralityk,
                results.subtitle = options_data$resultssubtitle
            )


            if (!options_data$originaltheme) {
                plot <- plot + ggtheme
            } else {
                plot <- plot + ggstatsplot::theme_ggstatsplot()
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

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()


            # grouped_ggdotplotstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggdotplotstats.html



            if (!is.null(self$options$grvar)) {
                selected_theme <- if (!options_data$originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()
                grvar <- self$options$grvar

                plot2 <- ggstatsplot::grouped_ggdotplotstats(
                    data = mydata,
                    x = !!rlang::sym(options_data$dep),
                    y = !!rlang::sym(options_data$group),
                    grouping.var = !!rlang::sym(grvar),
                    type = options_data$typestatistics,
                    test.value = options_data$testvalue,
                    effsize.type = options_data$effsizetype,
                    conf.level = options_data$conflevel,
                    k = options_data$k,
                    bf.message = options_data$bfmessage,
                    test.value.line = options_data$testvalueline,
                    centrality.parameter = options_data$centralityparameter,
                    centrality.k = options_data$centralityk,
                    results.subtitle = options_data$resultssubtitle,
                    ggtheme = selected_theme
                )
            }


            # Print Plot ----

            print(plot2)
            TRUE

        }





    )
)








