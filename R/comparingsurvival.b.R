#' @title Comparing Survival Outcomes
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr
#' @importFrom survival survdiff survfit Surv
#' @importFrom ggplot2 ggplot aes geom_step geom_ribbon labs theme_minimal
#' @importFrom survminer ggsurvplot
#' @import ggfortify

comparingSurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "comparingSurvivalClass",
    inherit = comparingSurvivalBase,
    private = list(

        # init ----
        .init = function() {
            
            # Check if required packages are available
            if (!requireNamespace("survival", quietly = TRUE)) {
                self$results$text$setContent(
                    "<div style='color: red; font-weight: bold;'>
                    Error: The 'survival' package is required but not installed.
                    <br><br>
                    Please install it using: install.packages('survival')
                    </div>"
                )
                return()
            }
            
            # Initialize instructions
            private$.initInstructions()
            
            # Initialize tables only if we have variables
            if (!is.null(self$options$groups)) {
                groups <- private$.groups()
                
                summary1 <- self$results$compsurvTable1
                for (group in groups)
                    summary1$addRow(rowKey=group, list(group=group))
                
                summary3 <- self$results$compsurvTable3
                for (group in groups)
                    summary3$addRow(rowKey=group, list(group=group))
            }
        },
        
        # Initialize instructions
        .initInstructions = function() {
            # Show instructions if variables not selected
            if (is.null(self$options$times) || is.null(self$options$status) || is.null(self$options$groups)) {
                instructions_html <- "
                <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 10px 0;'>
                    <h3>Comparing Survival Outcomes Between Groups</h3>
                    <p><strong>Compare survival curves and test for significant differences between groups using the log-rank test.</strong></p>

                    <h4>Features:</h4>
                    <ul>
                        <li><strong>Log-Rank Test:</strong> Statistical test for comparing survival distributions</li>
                        <li><strong>Kaplan-Meier Curves:</strong> Visual comparison of survival probabilities</li>
                        <li><strong>Median Survival:</strong> Estimates with confidence intervals for each group</li>
                        <li><strong>Event Summary:</strong> Observed vs expected events for each group</li>
                        <li><strong>Cumulative Hazard:</strong> Optional log-scale cumulative hazard plots</li>
                    </ul>

                    <h4>To Get Started:</h4>
                    <ol>
                        <li><strong>Select Time Variable:</strong> Time to event or censoring</li>
                        <li><strong>Select Event Indicator:</strong> 1/TRUE = event occurred, 0/FALSE = censored</li>
                        <li><strong>Select Grouping Variable:</strong> Factor variable defining comparison groups</li>
                        <li><strong>Customize Options:</strong> Add confidence intervals, time units, log-scale plots</li>
                    </ol>

                    <h4>Requirements:</h4>
                    <ul>
                        <li><strong>Time Variable:</strong> Numeric, non-negative values</li>
                        <li><strong>Event Indicator:</strong> Binary (0/1, FALSE/TRUE, or factor with 2 levels)</li>
                        <li><strong>Grouping Variable:</strong> Factor with 2 or more levels for comparison</li>
                        <li><strong>Sample Size:</strong> At least 10 observations per group recommended</li>
                    </ul>

                    <h4>Interpretation:</h4>
                    <ul>
                        <li><strong>Log-rank p < 0.05:</strong> Significant difference between survival curves</li>
                        <li><strong>Median Survival:</strong> Time when 50% of subjects have experienced the event</li>
                        <li><strong>Hazard Ratio:</strong> Use Cox regression for quantified group comparisons</li>
                    </ul>

                    <p><em>Note: This function performs univariate survival comparison. For adjusted comparisons, use Cox regression with covariates.</em></p>
                </div>"

                self$results$text$setContent(instructions_html)
            } else {
                self$results$text$setContent("")
            }
        },
        
        .groups = function() {
            if (is.null(self$options$groups))
                return(character(0))
            
            group <- self$data[[self$options$groups]]
            
            if (is.factor(group)) {
                groups <- levels(group)
            } else {
                groups <- unique(group[!is.na(group)])
                groups <- sort(groups)
            }
            
            if (length(groups) == 0)
                groups <- ''

            return(groups)
        },
        
        .run = function() {

            times <- self$options$times
            status <- self$options$status
            groups <- self$options$groups
            
            # Early return with instructions if variables not selected
            if (is.null(times) || is.null(status) || is.null(groups)) {
                private$.initInstructions()
                return()
            } else {
                self$results$text$setContent("")
            }

            # Get data
            data <- self$data
            if (nrow(data) == 0) {
                stop("Data contains no rows")
            }

            # Perform survival comparison analysis
            tryCatch({

                # Add checkpoint before analysis
                private$.checkpoint()

                # Validate and prepare data
                survData <- private$.prepareSurvivalData(data, times, status, groups)
                
                # Add checkpoint after data preparation
                private$.checkpoint()

                # Perform log-rank test
                logrank_results <- private$.performLogRankTest(survData, times, status, groups)
                
                # Fit survival curves
                survfit_results <- private$.fitSurvivalCurves(survData, times, status, groups)
                
                # Add checkpoint after analysis
                private$.checkpoint()

                # Generate tables
                private$.populateEventsSummary(logrank_results)
                private$.populateLogRankTest(logrank_results)
                private$.populateMedianEstimates(survfit_results)
                
                # Generate plots
                private$.generateSurvivalPlots(survfit_results)

            }, error = function(e) {
                error_msg <- paste0(
                    "<div style='color: red; font-weight: bold;'>",
                    "Error in survival comparison: ", e$message,
                    "<br><br>",
                    "Please check your variable selections and data format:",
                    "<br>• Time variable should be numeric and non-negative",
                    "<br>• Event indicator should be binary (0/1 or logical)",  
                    "<br>• Grouping variable should be a factor with 2+ levels",
                    "<br>• Ensure adequate sample size in each group",
                    "</div>"
                )
                self$results$text$setContent(error_msg)
            })
        },
        
        # Prepare survival data
        .prepareSurvivalData = function(data, times, status, groups) {
            
            # Validate time variable
            time_data <- data[[times]]
            if (!is.numeric(time_data)) {
                stop("Time variable must be numeric")
            }
            if (any(time_data < 0, na.rm = TRUE)) {
                stop("Time variable cannot contain negative values")
            }
            
            # Validate and convert status variable
            status_data <- data[[status]]
            if (is.logical(status_data)) {
                status_data <- as.numeric(status_data)
            } else if (is.factor(status_data)) {
                if (nlevels(status_data) != 2) {
                    stop("Event indicator must have exactly 2 levels")
                }
                status_data <- as.numeric(status_data) - 1
            } else {
                status_data <- as.numeric(status_data)
                unique_vals <- unique(status_data[!is.na(status_data)])
                if (!all(unique_vals %in% c(0, 1))) {
                    stop("Event indicator must be binary (0/1)")
                }
            }
            
            # Validate grouping variable
            group_data <- data[[groups]]
            if (!is.factor(group_data)) {
                group_data <- as.factor(group_data)
            }
            if (nlevels(group_data) < 2) {
                stop("Grouping variable must have at least 2 levels")
            }
            
            # Create survival data
            survData <- data.frame(
                time = time_data,
                status = status_data,
                group = group_data
            )
            
            # Remove missing values
            complete_cases <- complete.cases(survData)
            if (sum(complete_cases) < 10) {
                stop("Insufficient complete observations for survival analysis")
            }
            
            survData <- survData[complete_cases, ]
            
            # Check group sizes
            group_sizes <- table(survData$group)
            if (any(group_sizes < 3)) {
                warning("Some groups have very few observations (< 3). Results may be unreliable.")
            }
            
            return(survData)
        },
        
        # Perform log-rank test
        .performLogRankTest = function(survData, times, status, groups) {
            
            # Create survival formula
            surv_formula <- survival::Surv(time, status) ~ group
            
            # Perform log-rank test
            logrank_fit <- survival::survdiff(surv_formula, data = survData)
            
            return(logrank_fit)
        },
        
        # Fit survival curves
        .fitSurvivalCurves = function(survData, times, status, groups) {
            
            # Create survival formula
            surv_formula <- survival::Surv(time, status) ~ group
            
            # Fit Kaplan-Meier curves
            survfit_result <- survival::survfit(surv_formula, data = survData)
            
            return(survfit_result)
        },
        
        # Populate events summary table
        .populateEventsSummary = function(logrank_results) {
            
            table1 <- self$results$compsurvTable1
            
            for (i in 1:length(logrank_results$n)) {
                table1$setRow(rowNo=i, values=list(
                    n = logrank_results$n[i],
                    obs = logrank_results$obs[i],
                    exp = round(logrank_results$exp[i], 2),
                    ovse = round((logrank_results$obs[i] - logrank_results$exp[i])^2 / logrank_results$exp[i], 4),
                    ovsev = round((logrank_results$obs[i] - logrank_results$exp[i])^2 / logrank_results$var[i,i], 4)
                ))
            }
        },
        
        # Populate log-rank test table
        .populateLogRankTest = function(logrank_results) {
            
            table2 <- self$results$compsurvTable2
            table2$setRow(rowNo=1, values=list(
                var = "Log-Rank",
                chisqr = round(logrank_results$chisq, 4),
                df = length(logrank_results$n) - 1,
                p = pchisq(logrank_results$chisq, length(logrank_results$n) - 1, lower.tail = FALSE)
            ))
        },
        
        # Populate median estimates table
        .populateMedianEstimates = function(survfit_results) {
            
            # Get summary table
            summary_table <- summary(survfit_results)$table
            
            # Handle case where summary returns matrix vs data frame
            if (is.matrix(summary_table)) {
                temp_df <- as.data.frame(summary_table)
            } else {
                temp_df <- summary_table
            }
            
            table3 <- self$results$compsurvTable3
            
            for (i in 1:nrow(temp_df)) {
                median_val <- if ("median" %in% colnames(temp_df)) temp_df[i, "median"] else NA
                cilb_val <- if ("0.95LCL" %in% colnames(temp_df)) temp_df[i, "0.95LCL"] else NA
                ciub_val <- if ("0.95UCL" %in% colnames(temp_df)) temp_df[i, "0.95UCL"] else NA
                
                table3$setRow(rowNo=i, values=list(
                    median = median_val,
                    cilb = cilb_val,
                    ciub = ciub_val
                ))
            }
        },
        
        # Generate survival plots
        .generateSurvivalPlots = function(survfit_results) {
            
            # Set state for main survival plot
            image <- self$results$plot
            image$setState(survfit_results)

            # Set state for log-log plot if requested
            image2 <- self$results$plot2
            if (self$options$loglogyn) {
                image2$setState(survfit_results)
                image2$setVisible(visible = TRUE)
            } else {
                image2$setVisible(visible = FALSE)
            }
        },
        
        # Main survival plot
        .plot = function(image, ...) {

            times <- self$options$times
            status <- self$options$status
            groups <- self$options$groups

            if (is.null(times) || is.null(status) || is.null(groups))
                return(FALSE)

            plotData <- image$state
            if (is.null(plotData))
                return(FALSE)

            conf.int <- self$options$ciyn
            time_units <- self$options$timeunits
            xlab <- if (time_units == "None") "Time" else paste("Time (", time_units, ")", sep="")
            
            # Create improved plot using ggfortify
            tryCatch({
                if (requireNamespace("ggfortify", quietly = TRUE)) {
                    plot <- ggfortify::autoplot(plotData,
                                               xlab = xlab,
                                               ylab = "Survival Probability",
                                               conf.int = conf.int,
                                               yScale = "frac") +
                           ggplot2::theme_minimal() +
                           ggplot2::theme(
                               legend.title = ggplot2::element_text(size = 12),
                               legend.text = ggplot2::element_text(size = 10),
                               axis.title = ggplot2::element_text(size = 12),
                               axis.text = ggplot2::element_text(size = 10)
                           )
                } else {
                    # Fallback basic plot
                    plot(plotData, xlab = xlab, ylab = "Survival Probability")
                    return(TRUE)
                }
                
                print(plot)
                return(TRUE)
                
            }, error = function(e) {
                # Fallback to base R plotting
                plot(plotData, xlab = xlab, ylab = "Survival Probability")
                return(TRUE)
            })
        },
        
        # Log-log cumulative hazard plot
        .plot2 = function(image2, ...) {

            times <- self$options$times
            status <- self$options$status
            groups <- self$options$groups

            if (is.null(times) || is.null(status) || is.null(groups))
                return(FALSE)

            plotData <- image2$state
            if (is.null(plotData))
                return(FALSE)

            conf.int <- self$options$ciyn
            time_units <- self$options$timeunits
            xlab <- if (time_units == "None") "Time" else paste("Time (", time_units, ")", sep="")
            
            # Create log-log cumulative hazard plot
            tryCatch({
                if (requireNamespace("ggfortify", quietly = TRUE)) {
                    plot <- ggfortify::autoplot(plotData,
                                               fun = function(x) log(-log(x)),
                                               xlab = xlab,
                                               ylab = "log(-log(Survival))",
                                               conf.int = conf.int) +
                           ggplot2::theme_minimal() +
                           ggplot2::labs(title = "Log-Log Survival Plot (Proportional Hazards Check)") +
                           ggplot2::theme(
                               legend.title = ggplot2::element_text(size = 12),
                               legend.text = ggplot2::element_text(size = 10),
                               axis.title = ggplot2::element_text(size = 12),
                               axis.text = ggplot2::element_text(size = 10),
                               plot.title = ggplot2::element_text(size = 14, hjust = 0.5)
                           )
                } else {
                    # Fallback basic plot
                    plot(plotData, fun = function(x) log(-log(x)), 
                         xlab = xlab, ylab = "log(-log(Survival))")
                    return(TRUE)
                }
                
                print(plot)
                return(TRUE)
                
            }, error = function(e) {
                # Fallback to base R plotting
                plot(plotData, fun = function(x) log(-log(x)), 
                     xlab = xlab, ylab = "log(-log(Survival))")
                return(TRUE)
            })
        }
    )
)
