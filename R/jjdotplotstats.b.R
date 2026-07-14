#' @title Dot Chart
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#' @import ggplot2
#' @importFrom rlang sym
#' @importFrom digest digest
#'
#' @return An \code{R6} class generator object for the \code{jjdotplotstatsClass} backend; used internally by the jamovi analysis wrapper and not called directly.


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
        .noticesList = NULL,
        # .currentPreset = "basic",

        # Notice accumulation system (HTML-based, avoids serialization issues)
        .addNotice = function(message, type = "INFO") {
            if (is.null(private$.noticesList)) {
                private$.noticesList <- list()
            }

            # Determine styling based on type
            style_info <- switch(type,
                "ERROR" = list(
                    color = "#721c24",
                    bg = "#f8d7da",
                    border = "#f5c6cb",
                    icon = ""
                ),
                "STRONG_WARNING" = list(
                    color = "#856404",
                    bg = "#fff3cd",
                    border = "#ffeaa7",
                    icon = ""
                ),
                "WARNING" = list(
                    color = "#856404",
                    bg = "#fff3cd",
                    border = "#ffeaa7",
                    icon = ""
                ),
                "INFO" = list(
                    color = "#004085",
                    bg = "#cce5ff",
                    border = "#b8daff",
                    icon = ""
                ),
                # Default
                list(
                    color = "#004085",
                    bg = "#cce5ff",
                    border = "#b8daff",
                    icon = ""
                )
            )

            notice_html <- glue::glue(
                "<div style='background-color: {style_info$bg}; ",
                "border-left: 4px solid {style_info$border}; ",
                "padding: 12px; margin: 8px 0; color: {style_info$color};'>",
                "<strong>{style_info$icon} {type}:</strong> {message}",
                "</div>"
            )

            private$.noticesList <- append(private$.noticesList, notice_html)
            private$.renderNotices()
        },

        .renderNotices = function() {
            if (is.null(private$.noticesList) || length(private$.noticesList) == 0) {
                return()
            }

            notices_html <- paste(private$.noticesList, collapse = "\n")
            self$results$notices$setContent(notices_html)
        },

        .clearNotices = function() {
            private$.noticesList <- NULL
            self$results$notices$setContent("")
        },

        # init ----

        .init = function() {
            # Apply clinical presets if not custom
            # private$.applyClinicalPreset()

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

            if (nrow(self$data) == 0) {
                private$.addNotice('Data contains no complete rows. Please check for missing values in your selected variables.', "ERROR")
                return(FALSE)
            }

            # Check variable existence with better context
            if (!(self$options$dep %in% names(self$data))) {
                available_vars <- htmltools::htmlEscape(paste(names(self$data), collapse=", "))
                private$.addNotice(sprintf('Variable "%s" not found in data. Available variables: %s. Please select a valid continuous variable for the dependent variable.', htmltools::htmlEscape(self$options$dep), available_vars), "ERROR")
                return(FALSE)
            }

            if (!(self$options$group %in% names(self$data))) {
                available_vars <- htmltools::htmlEscape(paste(names(self$data), collapse=", "))
                private$.addNotice(sprintf('Variable "%s" not found in data. Available variables: %s. Please select a valid grouping variable.', htmltools::htmlEscape(self$options$group), available_vars), "ERROR")
                return(FALSE)
            }

            # Require at least two groups with complete data
            relevant_cols <- c(self$options$dep, self$options$group)
            if (!is.null(self$options$grvar))
                relevant_cols <- c(relevant_cols, self$options$grvar)
            complete_rows <- complete.cases(self$data[relevant_cols])
            group_levels <- nlevels(droplevels(as.factor(self$data[[self$options$group]][complete_rows])))
            if (group_levels < 2) {
                private$.addNotice(sprintf('At least two groups with data are required for comparison. Found %d group(s) with complete data. Please check for missing values or select different variables.', group_levels), "ERROR")
                return(FALSE)
            }

            # Check total sample size
            n_total <- sum(complete_rows)
            if (n_total < 30) {
                private$.addNotice(sprintf('Small total sample size (N = %d). Statistical tests may be unreliable with N < 30. Consider interpreting results cautiously or collecting more data.', n_total), "STRONG_WARNING")
            }

            # Check minimum group size
            group_data <- self$data[[self$options$group]][complete_rows]
            group_sizes <- table(droplevels(as.factor(group_data)))
            min_group_n <- min(group_sizes)
            if (min_group_n < 10) {
                min_group_name <- names(which.min(group_sizes))
                private$.addNotice(sprintf('Very small group sizes detected (minimum n = %d in group "%s"). Groups with n < 10 may produce unreliable test results. Consider combining groups or collecting more data.', min_group_n, htmltools::htmlEscape(min_group_name)), "STRONG_WARNING")
            }

            # Validate centrality parameter consistency
            private$.validateCentralityOptions()

            return(TRUE)
        },
        
        # Centrality parameter validation helper
        .validateCentralityOptions = function() {
            if (self$options$centralityparameter == "none" && self$options$centralityk != 2) {
                private$.addNotice('Centrality decimal places specified but centrality parameter is "none". The precision setting will have no effect.', "INFO")
            }

            if (self$options$centralityplotting && self$options$centralityparameter == "none") {
                private$.addNotice('Centrality plotting enabled but centrality parameter is "none". No centrality lines will be displayed.', "WARNING")
            }

            if (!self$options$centralityplotting && self$options$centralitytype != "parametric") {
                private$.addNotice('Centrality type specified but centrality plotting is disabled. The type setting will have no effect.', "INFO")
            }
        },
        
        # Message accumulation helper
        .accumulateMessage = function(message) {
            if (is.null(private$.messages)) {
                private$.messages <- character()
            }
            private$.messages <- append(private$.messages, message)
            self$results$todo$setContent(paste(private$.messages, collapse = ""))
        },
        
        
        
        
        # Guided steps generator
        # .generateGuidedSteps = function() {
        #     if (!self$options$guidedMode) return()
        #
        #     steps <- glue::glue(
        #         "<div style='background-color: #e8f5e8; padding: 15px; border-left: 4px solid #28a745; margin: 10px 0;'>",
        #         "<h4 style='color: #155724; margin-top: 0;'> Analysis Steps</h4>",
        #         "<ol style='margin: 10px 0; padding-left: 20px;'>",
        #         "<li><strong>Data Selection:</strong> Choose continuous variable and grouping variable</li>",
        #         "<li><strong>Test Selection:</strong> Review data assessment recommendations above</li>",
        #         "<li><strong>Options:</strong> Configure display and statistical options</li>",
        #         "<li><strong>Interpretation:</strong> Review clinical interpretation and assumptions</li>",
        #         "<li><strong>Report:</strong> Copy report template for documentation</li>",
        #         "</ol>",
        #         "<p><em> Tip: Follow these steps in order for best results. Check the Data Assessment panel for recommendations.</em></p>",
        #         "</div>"
        #     )
        #
        #     self$results$guidedSteps$setContent(steps)
        # },
        
        # Next steps recommendations
        # .generateRecommendations = function() {
        #     if (!self$options$guidedMode) return()
        #
        #     preset <- private$.currentPreset
        #
        #     recommendations <- switch(preset,
        #         "publication" = glue::glue(
        #             "<div style='background-color: #fff8e1; padding: 15px; border-left: 4px solid #ffc107; margin: 10px 0;'>",
        #             "<h4 style='color: #856404; margin-top: 0;'> Publication Checklist</h4>",
        #             "<ul style='margin: 10px 0; padding-left: 20px;'>",
        #             "<li> Report effect size with confidence intervals</li>",
        #             "<li> Include assumption checking results</li>",
        #             "<li> State statistical test used and why</li>",
        #             "<li> Report exact p-values (not just p < 0.05)</li>",
        #             "<li> Consider multiple testing corrections if applicable</li>",
        #             "</ul>",
        #             "</div>"
        #         ),
        #         "clinical" = glue::glue(
        #             "<div style='background-color: #e3f2fd; padding: 15px; border-left: 4px solid #2196f3; margin: 10px 0;'>",
        #             "<h4 style='color: #1976d2; margin-top: 0;'> Clinical Decision Points</h4>",
        #             "<ul style='margin: 10px 0; padding-left: 20px;'>",
        #             "<li>Consider clinical significance vs. statistical significance</li>",
        #             "<li>Evaluate practical impact of observed differences</li>",
        #             "<li>Review sample representativeness for your population</li>",
        #             "<li>Consider confounding variables not in this analysis</li>",
        #             "<li>Discuss findings with clinical colleagues</li>",
        #             "</ul>",
        #             "</div>"
        #         ),
        #         glue::glue(
        #             "<div style='background-color: #f3e5f5; padding: 15px; border-left: 4px solid #9c27b0; margin: 10px 0;'>",
        #             "<h4 style='color: #7b1fa2; margin-top: 0;'> Next Steps</h4>",
        #             "<ul style='margin: 10px 0; padding-left: 20px;'>",
        #             "<li>Review the statistical assumptions above</li>",
        #             "<li>Consider additional analyses if needed</li>",
        #             "<li>Document your methods and findings</li>",
        #             "<li>Consider replication with independent data</li>",
        #             "</ul>",
        #             "</div>"
        #         )
        #     )
        #
        #     self$results$recommendations$setContent(recommendations)
        # },
        
        # Data quality validation helper
        .validateDataQuality = function(mydata, dep_var) {
            num_vals <- jmvcore::toNumeric(mydata[[dep_var]])
            num_vals <- num_vals[!is.na(num_vals)]
            
            if (length(num_vals) < 3) {
                private$.accumulateMessage(
                    glue::glue("<br> Warning: {dep_var} has less than 3 valid observations<br>",
                               dep_var = htmltools::htmlEscape(dep_var))
                )
            }
            if (length(unique(num_vals)) < 2) {
                private$.accumulateMessage(
                    glue::glue("<br> Warning: {dep_var} has no variation (all values are the same)<br>",
                               dep_var = htmltools::htmlEscape(dep_var))
                )
            }
        },
        
        # Outlier detection helper
        .detectOutliers = function(data, var) {
            vals <- jmvcore::toNumeric(data[[var]])
            vals <- vals[!is.na(vals)]
            if (length(vals) > 0) {
                # Checkpoint before expensive quantile calculations
                private$.checkpoint()
                Q1 <- quantile(vals, 0.25, na.rm = TRUE)
                Q3 <- quantile(vals, 0.75, na.rm = TRUE)
                IQR <- Q3 - Q1
                outliers <- which(data[[var]] < (Q1 - 1.5 * IQR) | data[[var]] > (Q3 + 1.5 * IQR))
                if (length(outliers) > 0) {
                    private$.accumulateMessage(
                        glue::glue("<br> {length(outliers)} potential outlier(s) detected in {var}<br>",
                                   var = htmltools::htmlEscape(var))
                    )
                }
            }
        },
        
        # Statistical summary helper
        .addDataSummary = function(data, dep_var, group_var) {
            if (!is.null(dep_var) && !is.null(group_var)) {
                tryCatch({
                    # Checkpoint before expensive tapply operation
                    private$.checkpoint()
                    summary_stats <- tapply(data[[dep_var]], data[[group_var]], 
                                           function(x) c(mean = mean(x, na.rm = TRUE), 
                                                        n = sum(!is.na(x))))
                    n_groups <- length(summary_stats)
                    total_n <- sum(sapply(summary_stats, function(x) x["n"]), na.rm = TRUE)
                    private$.accumulateMessage(
                        glue::glue("<br> Analysis summary: {n_groups} groups, {total_n} total observations<br>")
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

            # SELECTIVE NA OMISSION - only remove rows with NAs in analysis variables
            # This prevents dropping patients with NAs in unused columns
            if (!is.null(dep_var) && !is.null(self$options$group)) {
                relevant_cols <- c(dep_var, self$options$group)

                # Add grouping variable if present
                if (!is.null(self$options$grvar)) {
                    relevant_cols <- c(relevant_cols, self$options$grvar)
                }

                private$.checkpoint()

                # Count rows before and after NA removal
                n_before <- nrow(mydata)
                mydata <- mydata[complete.cases(mydata[relevant_cols]), ]
                n_after <- nrow(mydata)

                # Report NA removal if any occurred
                if (n_before > n_after) {
                    n_dropped <- n_before - n_after
                    private$.accumulateMessage(
                        glue::glue("<br> Info: {n_dropped} rows excluded due to missing values in analysis variables.<br>",
                                  "Rows with data: {n_after} of {n_before} ({round(100 * n_after / n_before, 1)}%)<br>")
                    )
                }
            }
            
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
                    glue::glue("<br> Large dataset processed in {round(elapsed, 2)} seconds<br>")
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
                dep = self$options$dep,
                group = self$options$group,
                grvar = self$options$grvar,
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

            # Centrality settings mapped to ggstatsplot arguments
            centrality_plotting <- isTRUE(self$options$centralityplotting) && self$options$centralityparameter != "none"
            centrality_type <- self$options$centralitytype
            if (self$options$centralityparameter == "median")
                centrality_type <- "nonparametric"
            if (is.null(centrality_type) || centrality_type == "")
                centrality_type <- typestatistics

            # Compute axis labels respecting orientation flip (values on x-axis)
            xlab <- self$options$ytitle
            if (xlab == '') xlab <- group
            ylab <- self$options$xtitle
            if (ylab == '') ylab <- dep
            
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
                xlab = xlab,
                ylab = ylab,
                xtitle = xtitle,
                ytitle = ytitle,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype,
                testvalue = self$options$testvalue,
                bfmessage = self$options$bfmessage,
                conflevel = self$options$conflevel,
                digits = self$options$k,
                testvalueline = self$options$testvalueline,
                centralityparameter = self$options$centralityparameter,
                centralityk = self$options$centralityk,
                resultssubtitle = self$options$resultssubtitle,
                originaltheme = self$options$originaltheme
            )

            # Apply preset overrides without mutating options object
            # if (private$.currentPreset == "basic") {
            #     options_list$resultssubtitle <- TRUE
            # } else if (private$.currentPreset == "publication") {
            #     options_list$resultssubtitle <- TRUE
            #     options_list$originaltheme <- TRUE
            # } else if (private$.currentPreset == "clinical") {
            #     centrality_plotting <- TRUE
            #     centrality_type <- "nonparametric"
            #     options_list$centralityplotting <- TRUE
            #     options_list$centralityparameter <- "median"
            # }
            
            # Process centrality parameters if enabled
            options_list$centrality.plotting <- centrality_plotting
            options_list$centrality.type <- centrality_type
            options_list$ggplot.component <- list(ggplot2::coord_flip())
            if (isTRUE(self$options$testvalueline)) {
                options_list$ggplot.component <- c(
                    options_list$ggplot.component,
                    list(ggplot2::geom_hline(
                        yintercept = self$options$testvalue,
                        linetype = "dashed",
                        color = "red"
                    ))
                )
            }
            
            private$.processedOptions <- options_list
            private$.options_hash <- current_options_hash
            return(options_list)
        },

        # run ----
        .run = function() {
            # Clear messages and notices at start of new run
            private$.messages <- NULL
            private$.clearNotices()

            # Initial Message ----
            if ( is.null(self$options$dep) || is.null(self$options$group)) {

                # todo ----

                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Dot Charts.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations for <a href = 'https://www.indrapatil.com/ggstatsplot/reference/ggbetweenstats.html' target='_blank'>ggbetweenstats</a> and <a href = 'https://www.indrapatil.com/ggstatsplot/reference/grouped_ggbetweenstats.html' target='_blank'>grouped_ggbetweenstats</a>.
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

                if (nrow(self$data) == 0) {
                    private$.addNotice('Data contains no complete rows after filtering. Please check for missing values.', "ERROR")
                    return()
                }

                # Pre-process data and options for performance with enhanced validation
                tryCatch({
                    mydata <- private$.prepareData()
                    private$.prepareOptions()

                    # Generate clinical interpretation and assumptions

                    # Generate guided mode content if enabled
                    # private$.generateGuidedSteps()
                    # private$.generateRecommendations()
                }, error = function(e) {
                    private$.addNotice(sprintf('Data processing failed: %s. Please check your variable selections and try again.', htmltools::htmlEscape(e$message)), "ERROR")
                    return()
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


            # ggbetweenstats ----
            # https://www.indrapatil.com/ggstatsplot/reference/ggbetweenstats.html

            # Checkpoint before expensive ggstatsplot computation
            private$.checkpoint()

            plot <- tryCatch({
                ggstatsplot::ggbetweenstats(
                    data = mydata,
                    x = !!rlang::sym(options_data$group),
                    y = !!rlang::sym(options_data$dep),
                    title = options_data$mytitle,
                    xlab = options_data$xlab,
                    ylab = options_data$ylab,
                    type = options_data$typestatistics,
                    effsize.type = options_data$effsizetype,
                    conf.level = options_data$conflevel,
                    digits = options_data$digits,
                    bf.message = options_data$bfmessage,
                    centrality.plotting = options_data$centrality.plotting,
                    centrality.type = options_data$centrality.type,
                    results.subtitle = options_data$resultssubtitle,
                    ggplot.component = options_data$ggplot.component,
                    ggtheme = if (options_data$originaltheme) ggstatsplot::theme_ggstatsplot() else ggtheme
                )
            }, error = function(e) {
                private$.addNotice(sprintf('Plot generation failed: %s. Please check your data for issues (constant variables, insufficient variation, or extreme outliers) or try a different statistical test.', htmltools::htmlEscape(e$message)), "ERROR")
                return(NULL)
            })

            if (is.null(plot)) return()

            # Add success notice
            n_obs <- nrow(mydata)
            n_groups <- length(unique(mydata[[options_data$group]]))
            test_name <- switch(options_data$typestatistics,
                "parametric" = "parametric (t-test/ANOVA)",
                "nonparametric" = "nonparametric (Mann-Whitney/Kruskal-Wallis)",
                "robust" = "robust (trimmed means)",
                "bayes" = "Bayesian",
                "selected"
            )

            private$.addNotice(sprintf('Analysis completed successfully using %s test. Compared %d groups with N = %d total observations.', test_name, n_groups, n_obs), "INFO")

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


            # grouped_ggbetweenstats ----
            # https://www.indrapatil.com/ggstatsplot/reference/grouped_ggbetweenstats.html



            if (!is.null(self$options$grvar)) {
                grvar <- self$options$grvar

                # Checkpoint before expensive grouped ggstatsplot computation
                private$.checkpoint()

                plot2 <- tryCatch({
                    ggstatsplot::grouped_ggbetweenstats(
                        data = mydata,
                        x = !!rlang::sym(options_data$group),
                        y = !!rlang::sym(options_data$dep),
                        grouping.var = !!rlang::sym(grvar),
                        type = options_data$typestatistics,
                        effsize.type = options_data$effsizetype,
                        conf.level = options_data$conflevel,
                        digits = options_data$digits,
                        bf.message = options_data$bfmessage,
                        results.subtitle = options_data$resultssubtitle,
                        centrality.plotting = options_data$centrality.plotting,
                        centrality.type = options_data$centrality.type,
                        ggplot.component = options_data$ggplot.component,
                        ggtheme = if (options_data$originaltheme) ggstatsplot::theme_ggstatsplot() else ggtheme,
                        xlab = options_data$xlab,
                        ylab = options_data$ylab,
                        title = options_data$mytitle
                    )
                }, error = function(e) {
                    private$.addNotice(sprintf('Grouped plot generation failed: %s. Please check your grouping variable and data.', htmltools::htmlEscape(e$message)), "ERROR")
                    return(NULL)
                })

                if (is.null(plot2)) return()
            }


            # Print Plot ----

            print(plot2)
            TRUE

        }





    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for jjdotplotstats analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            dep <- self$options$dep
            group <- self$options$group

            if (is.null(dep) || is.null(group))
                return('')

            # Build the argument list in option-declaration order.
            #
            # Every variable-name option (dep, group, grvar) is emitted as a
            # deparse()'d string literal. deparse() produces valid, fully-escaped
            # R for names containing spaces, quotes or backslashes (e.g.
            # `Tumor Grade`); jmvcore's default sourcify would emit these as bare,
            # unquoted symbols and yield invalid syntax. Detecting OptionVariable
            # by class (rather than by name) means any variable option added later
            # is escaped automatically.
            #
            # data/dep/group are NOT re-emitted through private$.asArgs() - doing
            # so previously duplicated dep and group in the generated syntax (the
            # "double variables" bug). All non-variable options keep jmvcore's
            # per-option sourcify so formatting stays consistent with jamovi.
            args <- character(0)
            for (option in private$.options$options) {
                if (option$name == 'data')
                    next
                if (inherits(option, 'OptionVariable') || inherits(option, 'OptionVariables')) {
                    val <- option$value
                    if (!is.null(val))
                        args <- c(args, paste0(option$name, ' = ',
                                               paste0(deparse(val), collapse = '')))
                } else {
                    as <- private$.sourcifyOption(option)
                    if (!identical(as, ''))
                        args <- c(args, as)
                }
            }

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, '::jjdotplotstats(\n    data = data,\n    ',
                   paste(args, collapse = ',\n    '), ')')
        }
    ) # End of public list
)
