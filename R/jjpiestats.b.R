#' @title Pie Charts
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom digest digest
#' @importFrom rlang sym
#' @importFrom glue glue
#'

jjpiestatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjpiestatsClass",
    inherit = jjpiestatsBase,
    private = list(

        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .processedOptions = NULL,
        .data_hash = NULL,
        .validation_passed = FALSE,

        # init ----

        .init = function() {

            self$results$plot1$setSize(650, 450)

            self$results$plot2$setSize(650, 450)

            # if (!is.null(self$options$dep) && !is.null(self$options$grvar)) {
            #
            #     mydata <- self$data
            #
            #     grvar <-  self$options$grvar
            #
            #     num_levels <- nlevels(
            #         as.factor(mydata[[grvar]])
            #     )
            #
            #     self$results$plot3$setSize(num_levels * 600, 450)
            #
            # }

            if (!is.null(self$options$grvar) && !is.null(self$options$group)) {

                mydata <- self$data

                group <-  self$options$group

                num_levels_group <- nlevels(
                    as.factor(mydata[[group]])
                )

                self$results$plot4$setSize(num_levels_group * 600, 450)

            }




            if (!is.null(self$options$group) && !is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                num_levels <- nlevels(
                    as.factor(mydata[[grvar]])
                )

                group <-  self$options$group

                num_levels_group <- nlevels(
                    as.factor(mydata[[group]])
                )

                # self$results$plot3$setSize((num_levels + num_levels_group) * 600, 450)

                self$results$plot4$setSize((num_levels + num_levels_group) * 600, 450)

            }

        },

        # Helper Methods for Validation ----
        
        .validateVariables = function() {
            dep <- self$options$dep
            group <- self$options$group
            grvar <- self$options$grvar
            
            # Check if required variables exist
            all_vars <- c(dep, group, grvar)
            all_vars <- all_vars[!is.null(all_vars) & all_vars != ""]
            
            missing_vars <- all_vars[!all_vars %in% names(self$data)]
            if (length(missing_vars) > 0) {
                stop(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
            }
            
            # Validate that variables are appropriate for pie charts (categorical)
            for (var in all_vars) {
                if (!is.null(var) && var != "") {
                    var_class <- class(self$data[[var]])
                    if (!any(c("factor", "character", "logical") %in% var_class)) {
                        # Try to convert numeric to factor if it has few unique values
                        if (is.numeric(self$data[[var]])) {
                            unique_vals <- length(unique(self$data[[var]], na.rm = TRUE))
                            if (unique_vals > 10) {
                                stop(paste("Variable '", var, "' appears to be continuous (", unique_vals, 
                                         " unique values). Pie charts are for categorical data. Consider converting to groups first."))
                            }
                        }
                    }
                }
            }
            
            # Enhanced validation for statistical tests
            private$.validateStatisticalRequirements(dep, group)
            
            return(TRUE)
        },
        
        .validateStatisticalRequirements = function(dep, group) {
            # Check minimum group sizes for statistical tests when group variable is present
            if (!is.null(group) && group != "" && group %in% names(self$data)) {
                group_sizes <- table(self$data[[group]], useNA = "no")
                
                if (any(group_sizes < 5)) {
                    small_groups <- names(group_sizes[group_sizes < 5])
                    warning(paste("Small group sizes detected (", paste(paste(small_groups, ":", group_sizes[small_groups]), collapse = ", "),
                                "). Chi-square tests require minimum 5 observations per group for reliable results."))
                }
                
                if (length(group_sizes) < 2) {
                    stop("Grouping variable must have at least 2 categories for comparison.")
                }
            }
            
            # Check dependent variable has sufficient variation
            if (!is.null(dep) && dep != "" && dep %in% names(self$data)) {
                dep_levels <- table(self$data[[dep]], useNA = "no")
                if (length(dep_levels) < 2) {
                    stop(paste("Variable '", dep, "' has insufficient variation (only", length(dep_levels), "level). Need at least 2 categories for meaningful pie chart."))
                }
            }
        },
        
        .getCachedData = function() {
            # Create hash of current data and options state
            current_hash <- digest::digest(list(
                data_dim = dim(self$data),
                data_names = names(self$data),
                options = list(
                    dep = self$options$dep,
                    group = self$options$group,
                    grvar = self$options$grvar
                )
            ), algo = "md5")
            
            # Return cached data if hash matches and validation passed
            if (!is.null(private$.processedData) && 
                !is.null(private$.data_hash) &&
                private$.data_hash == current_hash && 
                private$.validation_passed) {
                return(private$.processedData)
            }
            
            # Validate and prepare fresh data
            private$.validateVariables()
            private$.processedData <- private$.prepareData()
            private$.data_hash <- current_hash
            private$.validation_passed <- TRUE
            
            return(private$.processedData)
        },

        # Optimized data preparation with caching
        .prepareData = function(force_refresh = FALSE) {
            # Prepare data with progress feedback
            self$results$todo$setContent(
                glue::glue("<br>Processing data for pie chart analysis...<br><hr>")
            )

            mydata <- self$data

            # Exclude NA with checkpoint
            private$.checkpoint()
            mydata <- jmvcore::naOmit(mydata)
            
            if (nrow(mydata) == 0) {
                stop('No complete data rows available after handling missing values. Please check your data.')
            }

            return(mydata)
        },

        # Optimized options preparation with caching
        .prepareOptions = function(force_refresh = FALSE) {
            if (!is.null(private$.processedOptions) && !force_refresh) {
                return(private$.processedOptions)
            }

            # Prepare options with progress feedback
            self$results$todo$setContent(
                glue::glue("<br>Preparing pie chart analysis options...<br><hr>")
            )

            # Process options
            dep <- self$options$dep
            group <- self$options$group
            grvar <- self$options$grvar
            typestatistics <- self$options$typestatistics
            
            # Cache the processed options
            options_list <- list(
                dep = dep,
                group = group,
                grvar = grvar,
                typestatistics = typestatistics,
                resultssubtitle = self$options$resultssubtitle,
                originaltheme = self$options$originaltheme
            )
            private$.processedOptions <- options_list
            return(options_list)
        }



        # run ----
        ,
        .run = function() {

            # Initial Message ----
            if ( is.null(self$options$dep) ) {

                # TODO ----
                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Pie Charts with statistical analysis.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.html' target='_blank'>ggpiestats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggpiestats.html' target='_blank'>grouped_ggpiestats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)
                return()

            } else {
                
                # Enhanced data validation with better error messages and caching
                tryCatch({
                    # Basic data check
                    if (nrow(self$data) == 0) {
                        stop('Dataset is empty. Please ensure your data contains observations.')
                    }
                    
                    # Use cached data validation and preparation
                    start_time <- Sys.time()
                    prepared_data <- private$.getCachedData()
                    prep_time <- round(difftime(Sys.time(), start_time, units = "secs"), 2)
                    
                    # Enhanced success message with timing and caching info
                    cache_status <- if (private$.validation_passed && !is.null(private$.processedData)) {
                        " (cached)"
                    } else {
                        " (fresh validation)"
                    }
                    
                    # Performance warnings
                    perf_warning <- ""
                    if (self$options$typestatistics == "bayes") {
                        perf_warning <- "<br>⚠️ <b>Performance Note:</b> Bayesian analysis is computationally intensive.<br>"
                    }
                    
                    # Build status message
                    dep_info <- paste("Variable:", self$options$dep)
                    group_info <- if (!is.null(self$options$group) && self$options$group != "") {
                        paste(", grouped by", self$options$group)
                    } else { "" }
                    split_info <- if (!is.null(self$options$grvar) && self$options$grvar != "") {
                        paste(", split by", self$options$grvar)
                    } else { "" }
                    
                    todo <- glue::glue(
                        "<br>Pie chart analysis ready. {dep_info}{group_info}{split_info}.<br>
                        <br>Data prepared: {nrow(prepared_data)} observations{cache_status}.<br>
                        <br>Statistical method: {tools::toTitleCase(self$options$typestatistics)} analysis.<br>
                        {perf_warning}
                        {if(prep_time > 0.1) paste0('<br>Preparation time: ', prep_time, ' seconds.<br>') else ''}
                        <hr>"
                    )
                    
                    self$results$todo$setContent(todo)
                    
                }, error = function(e) {
                    # Reset cache on error
                    private$.processedData <- NULL
                    private$.validation_passed <- FALSE
                    
                    # Enhanced error reporting with more context
                    error_context <- ""
                    if (grepl("continuous", e$message, ignore.case = TRUE)) {
                        error_context <- "<br>💡 <b>Tip:</b> Use Data > Transform to create categorical groups from continuous variables.<br>"
                    } else if (grepl("minimum|group size", e$message, ignore.case = TRUE)) {
                        error_context <- "<br>💡 <b>Tip:</b> Consider combining small categories or collecting more data.<br>"
                    } else if (grepl("variation|level", e$message, ignore.case = TRUE)) {
                        error_context <- "<br>💡 <b>Tip:</b> Ensure your variables have multiple categories for meaningful pie charts.<br>"
                    }
                    
                    error_msg <- glue::glue(
                        "<br>❌ <b>Error in Pie Chart Analysis:</b><br>
                        <br>{e$message}<br>
                        {error_context}
                        <br><b>General Troubleshooting:</b><br>
                        • Ensure dependent variable is categorical<br>
                        • Check that selected variables exist in your dataset<br>
                        • Verify sufficient sample sizes in each category (≥5 recommended)<br>
                        • Confirm variables have adequate variation (≥2 categories)<br><hr>"
                    )
                    self$results$todo$setContent(error_msg)
                    return()
                })
                
                # Add checkpoint for user feedback
                private$.checkpoint()
            }
        }


        # the plot1 function ----


        ,
        .plot1 = function(image, ggtheme, theme, ...) {

            # Validation ----
            if ( is.null(self$options$dep) )
                return()

            # Use cached data for performance with error handling
            tryCatch({
                mydata <- private$.getCachedData()
                options_data <- private$.prepareOptions()
            }, error = function(e) {
                stop(paste("Plot preparation failed:", e$message))
            })
            
            dep <- options_data$dep


            # ggpiestats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.html



            plot1 <-
                ggstatsplot::ggpiestats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = NULL,
                    counts = NULL,
                    ratio = NULL,
                    paired = FALSE,
                    type = options_data$typestatistics,
                    label = "percentage",
                    perc.k = 0,
                    label.args = list(alpha = 1, fill = "white"),
                    bf.message = TRUE,
                    sampling.plan = "indepMulti",
                    fixed.margin = "rows",
                    prior.concentration = 1,
                    title = NULL,
                    subtitle = NULL,
                    caption = NULL,
                    conf.level = 0.95,
                    nboot = 100,
                    legend.title = NULL,
                    k = 2,
                    proportion.test = TRUE,
                    package = "RColorBrewer",
                    palette = "Dark2",
                    ggplot.component = NULL,
                    output = "plot",
                    messages = TRUE,
                    results.subtitle = options_data$resultssubtitle
                    )



            originaltheme <- options_data$originaltheme

            if (!originaltheme) {
                plot1 <- plot1 + ggtheme
            } else {
                plot1 <- plot1 + ggstatsplot::theme_ggstatsplot()
                # ggplot2::theme_bw()
            }

            # Print Plot1 ----

            print(plot1)
            TRUE

        }


        # the plot2 function ----


        , .plot2 = function(image, ggtheme, theme, ...) {

            # Validation ----
            if ( is.null(self$options$dep) || is.null(self$options$group) )
                return()

            # Use cached data for performance with error handling
            tryCatch({
                mydata <- private$.getCachedData()
                options_data <- private$.prepareOptions()
            }, error = function(e) {
                stop(paste("Plot preparation failed:", e$message))
            })
            
            dep <- options_data$dep
            group <- options_data$group



            # ggpiestats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.html

            plot2 <-
                ggstatsplot::ggpiestats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = !!rlang::sym(group),
                    counts = NULL,
                    ratio = NULL,
                    paired = FALSE,
                    type = options_data$typestatistics,
                    label = "percentage",
                    perc.k = 0,
                    label.args = list(alpha = 1, fill = "white"),
                    bf.message = TRUE,
                    sampling.plan = "indepMulti",
                    fixed.margin = "rows",
                    prior.concentration = 1,
                    title = NULL,
                    subtitle = NULL,
                    caption = NULL,
                    conf.level = 0.95,
                    nboot = 100,
                    legend.title = NULL,
                    k = 2,
                    proportion.test = TRUE,
                    package = "RColorBrewer",
                    palette = "Dark2",
                    ggplot.component = NULL,
                    output = "plot",
                    messages = TRUE,
                    results.subtitle = options_data$resultssubtitle
                )


            originaltheme <- options_data$originaltheme

            if (!originaltheme) {
                plot2 <- plot2 + ggtheme
            } else {
                plot2 <- plot2 + ggstatsplot::theme_ggstatsplot()
                # ggplot2::theme_bw()
            }


            # Print Plot2 ----
            print(plot2)
            TRUE
        }


        # the plot4 function ----




        , .plot4 = function(image, ggtheme, theme, ...) {

            # Validation ----
            if ( is.null(self$options$dep) || is.null(self$options$group) || is.null(self$options$grvar) )
                return()

            # Use cached data for performance with error handling
            tryCatch({
                mydata <- private$.getCachedData()
                options_data <- private$.prepareOptions()
            }, error = function(e) {
                stop(paste("Grouped plot preparation failed:", e$message))
            })
            
            dep <- options_data$dep
            group <- options_data$group
            grvar <- options_data$grvar



            # grouped_ggpiestats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggpiestats.html

            if ( !is.null(grvar) ) {

                originaltheme <- options_data$originaltheme

                selected_theme <- if (!originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()


                plot4 <- ggstatsplot::grouped_ggpiestats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = !!rlang::sym(group),
                    counts = NULL,
                    grouping.var = !!rlang::sym(grvar),
                    type = options_data$typestatistics,
                    results.subtitle = options_data$resultssubtitle,
                    ggtheme = selected_theme,
                    ggstatsplot.layer = originaltheme
                )
            }


            # Print Plot4 ----
            print(plot4)
            TRUE
        }

    )
)
