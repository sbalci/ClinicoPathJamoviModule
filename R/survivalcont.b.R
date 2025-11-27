#' @title Survival Analysis for Continuous Explanatory Variable
#' 
#' @description
#' Comprehensive survival analysis for continuous explanatory variables with optimal 
#' cut-off determination, multiple cut-offs analysis, RMST analysis, residual diagnostics,
#' and advanced visualization options.
#' 
#' @details
#' This function provides advanced survival analysis specifically designed for continuous
#' explanatory variables. It includes:
#' 
#' **Core Features:**
#' - Optimal cut-off determination using maximally selected rank statistics
#' - Multiple cut-offs analysis with 4 different methods (quantile, recursive, tree-based, minimum p-value)
#' - Person-time analysis with interval stratification
#' - Date-based time calculation with multiple format support
#' - Multiple event level support (overall, cause-specific, competing risks)
#' - Landmark analysis for time-dependent effects
#' 
#' **Advanced Analytics:**
#' - Restricted Mean Survival Time (RMST) analysis
#' - Cox model residual diagnostics (Martingale, Deviance, Score, Schoenfeld)
#' - Log-log plots for proportional hazards assessment
#' - Enhanced error handling and data validation
#' 
#' **Visualization Options:**
#' - Kaplan-Meier survival curves with optimal cut-offs
#' - Multiple cut-offs histogram with cut-point annotations
#' - Cumulative events and hazard plots
#' - KMunicate-style plots for publication
#' - Residual diagnostic plots (4-panel layout)
#' - Log-log plots for assumption checking
#' 
#' @examples
#' \dontrun{
#' # Basic survival analysis with optimal cut-off
#' data("lung", package = "survival")
#' lung$status_binary <- ifelse(lung$status == 2, 1, 0)
#' 
#' result1 <- survivalcont(
#'   data = lung,
#'   elapsedtime = "time",
#'   outcome = "status_binary", 
#'   contexpl = "age",
#'   findcut = TRUE,
#'   sc = TRUE
#' )
#' 
#' # Multiple cut-offs analysis with different methods
#' result2 <- survivalcont(
#'   data = lung,
#'   elapsedtime = "time",
#'   outcome = "status_binary",
#'   contexpl = "ph.karno",
#'   multiple_cutoffs = TRUE,
#'   num_cutoffs = "three",
#'   cutoff_method = "recursive",
#'   min_group_size = 15,
#'   sc = TRUE
#' )
#' 
#' # RMST analysis with residual diagnostics
#' result3 <- survivalcont(
#'   data = lung,
#'   elapsedtime = "time",
#'   outcome = "status_binary",
#'   contexpl = "wt.loss",
#'   findcut = TRUE,
#'   rmst_analysis = TRUE,
#'   rmst_tau = 500,
#'   residual_diagnostics = TRUE,
#'   loglog = TRUE
#' )
#' 
#' # Person-time analysis with date calculation
#' # Create sample data with dates
#' set.seed(123)
#' n <- 200
#' sample_data <- data.frame(
#'   biomarker = rnorm(n, 100, 25),
#'   event = rbinom(n, 1, 0.6),
#'   dx_date = as.Date("2020-01-01") + sample(0:365, n, replace = TRUE),
#'   fu_date = as.Date("2020-01-01") + sample(366:1095, n, replace = TRUE)
#' )
#' 
#' result4 <- survivalcont(
#'   data = sample_data,
#'   tint = TRUE,
#'   dxdate = "dx_date",
#'   fudate = "fu_date",
#'   timetypedata = "ymd",
#'   timetypeoutput = "months",
#'   outcome = "event",
#'   contexpl = "biomarker",
#'   person_time = TRUE,
#'   time_intervals = "6, 12, 24",
#'   rate_multiplier = 1000,
#'   calculatedtime = TRUE
#' )
#' 
#' # Comprehensive analysis with all features
#' result5 <- survivalcont(
#'   data = lung,
#'   elapsedtime = "time",
#'   outcome = "status_binary",
#'   contexpl = "meal.cal",
#'   findcut = TRUE,
#'   multiple_cutoffs = TRUE,
#'   num_cutoffs = "two",
#'   cutoff_method = "quantile",
#'   rmst_analysis = TRUE,
#'   rmst_tau = 400,
#'   residual_diagnostics = TRUE,
#'   person_time = TRUE,
#'   time_intervals = "100, 300, 500",
#'   sc = TRUE,
#'   ce = TRUE,
#'   ch = TRUE,
#'   kmunicate = TRUE,
#'   loglog = TRUE,
#'   ci95 = TRUE,
#'   risktable = TRUE,
#'   calculatedcutoff = TRUE,
#'   calculatedmulticut = TRUE
#' )
#' }
#' 
#' @references
#' Hothorn, T., & Zeileis, A. (2008). Generalized maximally selected statistics. 
#' Biometrics, 64(4), 1263-1269.
#' 
#' Royston, P., & Parmar, M. K. (2013). Restricted mean survival time: an alternative 
#' to the hazard ratio for the design and analysis of randomized trials with a 
#' time-to-event outcome. BMC Medical Research Methodology, 13(1), 152.
#' 
#' Morris, T. P., et al. (2019). Proposals on Kaplan–Meier plots in medical research 
#' and a survey of stakeholder views: KMunicate. BMJ Open, 9(9), e030874.
#' 
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr

survivalcontClass <- if (requireNamespace("jmvcore")) {
    R6::R6Class(
        "survivalcontClass",
        inherit = survivalcontBase,
        private = list(
            # Private fields for storing analysis results
            residuals_data = NULL,

            # Competing Risk Helper Functions ----
            .isCompetingRisk = function() {
                # Check if competing risk analysis is active
                return(self$options$multievent && self$options$analysistype == "compete")
            },

            .competingRiskCumInc = function(mydata, mytime, myoutcome) {
                # Proper competing risk analysis using cmprsk::cuminc()
                # Returns cumulative incidence estimates for the event of interest

                # Ensure time is numeric
                mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])

                # For competing risk: outcome is 0=censored, 1=event of interest, 2=competing event
                tryCatch({
                    # Run cumulative incidence analysis
                    cuminc_fit <- cmprsk::cuminc(
                        ftime = mydata[[mytime]],
                        fstatus = mydata[[myoutcome]],
                        cencode = 0  # 0 is censored
                    )

                    return(cuminc_fit)

                }, error = function(e) {
                    stop(glue::glue(
                        "Competing risk analysis failed: {e$message}\n",
                        "Please ensure your data has events coded as: 0=censored, 1=event of interest, 2=competing event"
                    ))
                })
            },

            .getDefaultCutpoints = function() {
                # Return time-unit aware default cutpoints (like singlearm)
                time_unit <- self$options$timetypeoutput
                switch(time_unit,
                    "days" = c(365, 1095, 1825),      # 1, 3, 5 years in days
                    "weeks" = c(52, 156, 260),        # 1, 3, 5 years in weeks
                    "months" = c(12, 36, 60),         # 1, 3, 5 years in months
                    "years" = c(1, 3, 5),             # 1, 3, 5 years
                    c(12, 36, 60)  # default to months if unknown
                )
            },

            # Helper function to escape variable names with special characters for formulas
            .escapeVariableNames = function(var_names) {
                # Check if variable names contain special characters that need escaping
                need_escaping <- grepl("[^a-zA-Z0-9._]", var_names)
                var_names[need_escaping] <- paste0("`", var_names[need_escaping], "`")
                return(var_names)
            },

            # Helper function to create clinical tooltips and explanations
            .createClinicalTooltip = function(term, definition, example = NULL) {
                tooltip_html <- glue::glue(
                    '<div class="clinical-tooltip">
                    <strong>{term}</strong><br>
                    <span class="definition">{definition}</span>
                    {if (!is.null(example)) paste0("<br><em>Example: ", example, "</em>") else ""}
                    </div>'
                )
                return(tooltip_html)
            },

            # Helper function to create clinical interpretation boxes
            .createInterpretationBox = function(title, content, warning = NULL) {
                warning_html <- if (!is.null(warning)) {
                    glue::glue('<div class="warning-box">⚠️ {warning}</div>')
                } else ""
                
                interpretation_html <- glue::glue(
                    '<div class="interpretation-box">
                    <h4 class="interpretation-title">📊 {title}</h4>
                    <div class="interpretation-content">{content}</div>
                    {warning_html}
                    </div>'
                )
                return(interpretation_html)
            },

            # Helper function to generate copy-ready clinical sentences
            .generateClinicalSentence = function(analysis_type, variable_name, result_values) {
                switch(analysis_type,
                    "cox_regression" = {
                        # Safe access to Cox regression results with fallbacks
                        val <- result_values$hr
                        if (length(val) == 0 || is.na(val) || val == "-") {
                            hr_val <- "N/A"
                        } else {
                            hr_val <- round(as.numeric(val), 2)
                        }
                        
                        if (identical(hr_val, "N/A")) {
                            glue::glue(.('Analysis of {variable} could not determine hazard ratio.'), variable = variable_name)
                        } else {
                            glue::glue(.('When {variable} increases by 1 unit, the hazard (risk) of the event changes by a factor of {hr}.'), 
                                     variable = variable_name, hr = hr_val)
                        }
                    },
                    
                    "median_survival" = {
                        median_val <- ifelse(is.null(result_values$median), "N/A", round(result_values$median, 1))
                        time_unit <- ifelse(is.null(result_values$time_unit), "time units", result_values$time_unit)
                        group_name <- ifelse(is.null(result_values$group), "this group", result_values$group)
                        
                        if (median_val == "N/A") {
                            glue::glue(.('Median survival time for {group} could not be determined.'), group = group_name)
                        } else {
                            glue::glue(.('The median survival time for patients with {group} was {median} {time_unit}.'),
                                     group = group_name, median = median_val, time_unit = time_unit)
                        }
                    },
                    
                    "cutoff_analysis" = {
                        cutoff_val <- ifelse(is.null(result_values$cutoff), "N/A", round(as.numeric(result_values$cutoff), 2))
                        if (cutoff_val == "N/A") {
                            glue::glue(.('Optimal cut-off point for {variable} could not be determined.'), variable = variable_name)
                        } else {
                            glue::glue(.('The optimal cut-off point for {variable} is {cutoff}, which best separates patients into high-risk and low-risk groups.'),
                                     variable = variable_name, cutoff = cutoff_val)
                        }
                    },
                    
                    # Default case
                    glue::glue(.('Analysis completed for {variable}.'), variable = variable_name)
                )
            },

            # Error recovery wrapper for safe analysis execution
            .safeAnalysis = function(analysis_function, context = "", fallback_value = NULL) {
                tryCatch({
                    analysis_function()
                }, error = function(e) {
                    warning(glue::glue(.('Analysis failed in {context}: {error}. Using fallback method.'), 
                            context = context, error = e$message))
                    return(fallback_value)
                }, warning = function(w) {
                    message(glue::glue(.('Warning in {context}: {warning}'), 
                            context = context, warning = w$message))
                    suppressWarnings(analysis_function())
                })
            },

            # Clinical assumption checking
            .checkClinicalAssumptions = function(data, time_var, outcome_var, contexpl_var = NULL) {
                warnings <- list()
                
                # Sample size checks
                n <- nrow(data)
                if (n < 30) {
                    warnings <- append(warnings, .('Very small sample size (n = {n}). Results may be unreliable. Recommend n ≥ 50 for stable cut-off analysis.'))
                } else if (n < 50) {
                    warnings <- append(warnings, .('Small sample size (n = {n}). Consider larger sample for more reliable cut-off analysis.'))
                }
                
                # Event rate checks
                events <- sum(data[[outcome_var]], na.rm = TRUE)
                event_rate <- events / n
                if (event_rate < 0.1) {
                    warnings <- append(warnings, .('Low event rate ({rate}%). May need larger sample or longer follow-up for reliable survival analysis.'))
                } else if (event_rate > 0.9) {
                    warnings <- append(warnings, .('Very high event rate ({rate}%). Consider competing risks or cause-specific analysis.'))
                }
                
                # Follow-up time checks
                median_time <- median(data[[time_var]], na.rm = TRUE)
                if (median_time < 6) {
                    warnings <- append(warnings, .('Short median follow-up ({time} {units}). May be insufficient for meaningful survival analysis.'))
                }
                
                # Continuous variable distribution checks
                if (!is.null(contexpl_var) && contexpl_var %in% names(data)) {
                    cont_var <- data[[contexpl_var]]
                    if (length(unique(cont_var[!is.na(cont_var)])) < 10) {
                        warnings <- append(warnings, .('Limited variability in continuous explanatory variable. Consider treating as categorical.'))
                    }
                }
                
                return(warnings)
            },

            # Helper function to restore original variable names in output tables
            .restoreOriginalNamesInTable = function(table_data, all_labels) {
                if (is.null(table_data) || nrow(table_data) == 0) return(table_data)
                
                # Create a mapping from cleaned names to original names
                name_mapping <- setNames(unlist(all_labels), names(all_labels))
                
                # Restore names in the first column (which typically contains variable names)
                if (ncol(table_data) > 0) {
                    first_col <- table_data[[1]]
                    
                    # Process each row in the first column
                    for (i in seq_along(first_col)) {
                        current_name <- first_col[i]
                        
                        # Skip if it's not a string or is empty
                        if (is.na(current_name) || current_name == "" || !is.character(current_name)) next
                        
                        # Check if this name exists in our mapping and replace it
                        if (current_name %in% names(name_mapping)) {
                            table_data[i, 1] <- name_mapping[current_name]
                        }
                        
                        # Also handle factor level names (e.g., "variable_name=level")
                        for (clean_name in names(name_mapping)) {
                            original_name <- name_mapping[clean_name]
                            pattern <- paste0("^", clean_name, "([=:].*)?$")
                            if (grepl(pattern, current_name)) {
                                table_data[i, 1] <- gsub(clean_name, original_name, current_name)
                                break
                            }
                        }
                    }
                }
                
                return(table_data)
            },
            
            .detectCommonMisuses = function() {
                warnings <- c()
                
                # Detect multiple testing without correction
                plot_count <- sum(c(
                    self$options$sc, self$options$kmunicate, self$options$ce, 
                    self$options$ch, self$options$loglog
                ))
                if (plot_count > 3) {
                    warnings <- c(warnings, .(
                        "Multiple analysis outputs selected. Consider adjusting for multiple comparisons if making statistical inferences from multiple tests."
                    ))
                }
                
                # Detect inappropriate cut-off hunting
                if (self$options$multiple_cutoffs && !is.null(self$options$num_cutoffs)) {
                    # Map string options to numeric values
                    num_cuts_map <- c("two" = 2, "three" = 3, "four" = 4)
                    num_cuts <- num_cuts_map[self$options$num_cutoffs]
                    
                    # Fallback for safety
                    if (is.na(num_cuts)) num_cuts <- 0
                    
                    if (num_cuts > 5) {
                        warnings <- c(warnings, .(
                            "Testing many cut-offs ({n}) increases risk of false discoveries. Consider pre-specified cut-offs or correction for multiple testing."
                        ) %>% glue::glue(n = num_cuts))
                    }
                }
                
                # Detect potential data snooping
                if (self$options$findcut && self$options$multiple_cutoffs) {
                    warnings <- c(warnings, .(
                        "Using both optimal cut-off and multiple cut-offs may lead to overfitting. Consider validation in independent dataset or cross-validation."
                    ))
                }
                
                # Detect inappropriate landmark analysis
                if (self$options$uselandmark && !is.null(self$options$landmark)) {
                    landmark_time <- as.numeric(self$options$landmark)
                    if (landmark_time > 0 && nrow(self$data) > 0) {
                        # Check if landmark time removes too many patients
                        time_var <- self$options$elapsedtime
                        if (!is.null(time_var) && time_var %in% names(self$data)) {
                            time_data <- self$data[[time_var]]
                            patients_excluded <- sum(time_data < landmark_time, na.rm = TRUE)
                            exclusion_rate <- patients_excluded / length(time_data)
                            if (exclusion_rate > 0.3) {
                                warnings <- c(warnings, .(
                                    "Landmark analysis excludes {rate}% of patients. High exclusion rates may introduce bias."
                                ) %>% glue::glue(rate = round(exclusion_rate * 100, 1)))
                            }
                        }
                    }
                }
                
                return(warnings)
            },

            # init ----
            .init = function() {
                # Initialize all outputs to FALSE first (following singlearm pattern)
                # Core Cox Regression outputs
                self$results$coxRegressionHeading$setVisible(FALSE)
                self$results$coxSummary$setVisible(FALSE)
                self$results$coxTable$setVisible(FALSE)
                self$results$tCoxtext2$setVisible(FALSE)
                self$results$coxRegressionHeading3$setVisible(FALSE)
                self$results$coxRegressionExplanation$setVisible(FALSE)
                
                # Person-time analysis outputs
                self$results$personTimeHeading$setVisible(FALSE)
                self$results$personTimeTable$setVisible(FALSE)
                self$results$personTimeSummary$setVisible(FALSE)
                self$results$personTimeExplanation$setVisible(FALSE)
                
                # RMST analysis outputs
                self$results$rmstHeading$setVisible(FALSE)
                self$results$rmstTable$setVisible(FALSE)
                self$results$rmstSummary$setVisible(FALSE)
                self$results$rmstExplanation$setVisible(FALSE)
                
                # Residuals analysis outputs
                self$results$residualsTable$setVisible(FALSE)
                self$results$residualsPlot$setVisible(FALSE)
                self$results$residualDiagnosticsExplanation$setVisible(FALSE)
                
                # Cut-off analysis outputs
                self$results$cutoffAnalysisHeading$setVisible(FALSE)
                self$results$rescutTable$setVisible(FALSE)
                self$results$cutoffAnalysisHeading3$setVisible(FALSE)
                self$results$cutoffAnalysisExplanation$setVisible(FALSE)
                self$results$plot4$setVisible(FALSE)
                self$results$plot5$setVisible(FALSE)
                self$results$medianSummary$setVisible(FALSE)
                self$results$medianTable$setVisible(FALSE)
                self$results$survTableSummary$setVisible(FALSE)
                self$results$survTable$setVisible(FALSE)
                
                # Multiple cut-offs outputs
                self$results$multipleCutTable$setVisible(FALSE)
                self$results$multipleMedianTable$setVisible(FALSE)
                self$results$multipleCutoffsExplanation$setVisible(FALSE)
                self$results$multipleSurvTable$setVisible(FALSE)
                self$results$plotMultipleCutoffs$setVisible(FALSE)
                self$results$plotMultipleSurvival$setVisible(FALSE)
                
                # Survival plots outputs
                self$results$plot2$setVisible(FALSE)
                self$results$plot3$setVisible(FALSE)
                self$results$plot6$setVisible(FALSE)
                self$results$plot7$setVisible(FALSE)
                self$results$survivalPlotsHeading3$setVisible(FALSE)
                self$results$survivalPlotsExplanation$setVisible(FALSE)
                self$results$loglogPlotExplanation$setVisible(FALSE)
                
                # Always show Cox Regression heading and table when data is present
                self$results$coxRegressionHeading$setVisible(TRUE)
                self$results$coxTable$setVisible(TRUE)
                self$results$tCoxtext2$setVisible(TRUE)
                
                # Handle showSummaries visibility
                if (self$options$showSummaries) {
                    self$results$coxSummary$setVisible(TRUE)
                    
                    # Conditional summaries - require both showSummaries AND their specific option
                    if (self$options$person_time) {
                        self$results$personTimeSummary$setVisible(TRUE)
                    }
                    if (self$options$rmst_analysis) {
                        self$results$rmstSummary$setVisible(TRUE)
                    }
                    if (self$options$findcut) {
                        self$results$medianSummary$setVisible(TRUE)
                        self$results$survTableSummary$setVisible(TRUE)
                    }
                }
                
                # Handle showExplanations visibility
                if (self$options$showExplanations) {
                    # Cox regression explanation is always shown with explanations
                    self$results$coxRegressionHeading3$setVisible(TRUE)
                    self$results$coxRegressionExplanation$setVisible(TRUE)
                    
                    # Conditional explanations - require both showExplanations AND their specific option
                    if (self$options$findcut) {
                        self$results$cutoffAnalysisHeading3$setVisible(TRUE)
                        self$results$cutoffAnalysisExplanation$setVisible(TRUE)
                    }
                    if (self$options$multiple_cutoffs) {
                        self$results$multipleCutoffsExplanation$setVisible(TRUE)
                    }
                    if (self$options$person_time) {
                        self$results$personTimeExplanation$setVisible(TRUE)
                    }
                    if (self$options$rmst_analysis) {
                        self$results$rmstExplanation$setVisible(TRUE)
                    }
                    if (self$options$residual_diagnostics) {
                        self$results$residualDiagnosticsExplanation$setVisible(TRUE)
                    }
                    if (self$options$loglog) {
                        self$results$loglogPlotExplanation$setVisible(TRUE)
                    }
                    
                    # Survival plots explanation requires showExplanations AND at least one plot
                    if (self$options$sc || self$options$ce || self$options$ch || self$options$kmunicate) {
                        self$results$survivalPlotsHeading3$setVisible(TRUE)
                        self$results$survivalPlotsExplanation$setVisible(TRUE)
                    }
                }
                
                # Handle person_time visibility
                if (self$options$person_time) {
                    self$results$personTimeHeading$setVisible(TRUE)
                    self$results$personTimeTable$setVisible(TRUE)
                }
                
                # Handle RMST analysis visibility
                if (self$options$rmst_analysis) {
                    self$results$rmstHeading$setVisible(TRUE)
                    self$results$rmstTable$setVisible(TRUE)
                }
                
                # Handle residual diagnostics visibility
                if (self$options$residual_diagnostics) {
                    self$results$residualsTable$setVisible(TRUE)
                    self$results$residualsPlot$setVisible(TRUE)
                }
                
                # Handle findcut visibility
                if (self$options$findcut) {
                    self$results$cutoffAnalysisHeading$setVisible(TRUE)
                    self$results$rescutTable$setVisible(TRUE)
                    self$results$plot4$setVisible(TRUE)
                    self$results$medianTable$setVisible(TRUE)
                    self$results$survTable$setVisible(TRUE)
                    
                    # Show survival plot if requested
                    if (self$options$sc) {
                        self$results$plot5$setVisible(TRUE)
                    }
                }
                
                # Handle multiple cutoffs visibility
                if (self$options$multiple_cutoffs) {
                    self$results$multipleCutTable$setVisible(TRUE)
                    self$results$multipleMedianTable$setVisible(TRUE)
                    self$results$multipleSurvTable$setVisible(TRUE)
                    self$results$plotMultipleCutoffs$setVisible(TRUE)
                    
                    if (self$options$sc) {
                        self$results$plotMultipleSurvival$setVisible(TRUE)
                    }
                }
                
                # Handle plot visibility based on their options
                if (self$options$findcut) {
                    if (self$options$ce) {
                        self$results$plot2$setVisible(TRUE)
                    }
                    if (self$options$ch) {
                        self$results$plot3$setVisible(TRUE)
                    }
                    if (self$options$kmunicate) {
                        self$results$plot6$setVisible(TRUE)
                    }
                    if (self$options$loglog) {
                        self$results$plot7$setVisible(TRUE)
                    }
                }
            }

            # getData ----
            ,
            .getData = function() {
                mydata <- self$data

                mydata$row_names <- rownames(mydata)

                original_names <- names(mydata)

                labels <- setNames(original_names, original_names)

                mydata <- mydata %>% janitor::clean_names()

                corrected_labels <-
                    setNames(original_names, names(mydata))

                mydata <- labelled::set_variable_labels(
                    .data = mydata,
                    .labels = corrected_labels
                )

                all_labels <- labelled::var_label(mydata)


                mytime <-
                    names(all_labels)[all_labels == self$options$elapsedtime]

                myoutcome <-
                    names(all_labels)[all_labels == self$options$outcome]

                mydxdate <-
                    names(all_labels)[all_labels == self$options$dxdate]

                myfudate <-
                    names(all_labels)[all_labels == self$options$fudate]

                mycontexpl <-
                    names(all_labels)[all_labels == self$options$contexpl]

                return(list(
                    "mydata_labelled" = mydata,
                    "mytime_labelled" = mytime,
                    "myoutcome_labelled" = myoutcome,
                    "mydxdate_labelled" = mydxdate,
                    "myfudate_labelled" = myfudate,
                    "mycontexpl_labelled" = mycontexpl,
                    "all_labels" = all_labels
                ))
            }


            # todo ----
            ,
            .todo = function() {
                todo <- glue::glue(.(
                    'Welcome to ClinicoPath
                    
                    This tool will help you calculate a cut-off for a continuous variable based on survival outcome.
                    
                    After the cut-off is determined median survivals and 1,3,5-yr survivals are calculated.
                    
                    Explanatory variable is continuous.
                    
                    Select outcome level from Outcome variable.
                    
                    Outcome Level: if patient is dead or event (recurrence) occured. You may also use advanced outcome options depending on your analysis type.
                    
                    Survival time should be numeric and continuous. You may also use dates to calculate survival time in advanced elapsed time options.
                    
                    This function uses survival, survminer, and finalfit packages. Please cite jamovi and the packages as given below.
                    
                    See details for survival [here](https://cran.r-project.org/web/packages/survival/vignettes/survival.pdf).'))

                html <- self$results$todo
                html$setContent(todo)
            }




            # Define Survival Time ----
            ,
            .definemytime = function() {

            ## Read Labelled Data ----

            labelled_data <- private$.getData()

            mydata <- labelled_data$mydata_labelled
            mytime_labelled <- labelled_data$mytime_labelled
            mydxdate_labelled <- labelled_data$mydxdate_labelled
            myfudate_labelled <- labelled_data$myfudate_labelled

            tint <- self$options$tint


                if (!tint) {
                    ### Precalculated Time ----

                    mydata[["mytime"]] <-
                        jmvcore::toNumeric(mydata[[mytime_labelled]])


                } else if (tint) {
                    ### Time Interval ----

                    dxdate <- mydxdate_labelled
                    fudate <- myfudate_labelled
                    timetypedata <- self$options$timetypedata


                    # Check if input is numeric (Unix epoch) or text (requires parsing)
                    is_numeric_dx <- is.numeric(mydata[[dxdate]])
                    is_numeric_fu <- is.numeric(mydata[[fudate]])

                    if (is_numeric_dx && is_numeric_fu) {
                        # Handle numeric Unix epoch input (from DateTime Converter)
                        mydata[["start"]] <- as.POSIXct(mydata[[dxdate]], origin="1970-01-01", tz="UTC")
                        mydata[["end"]] <- as.POSIXct(mydata[[fudate]], origin="1970-01-01", tz="UTC")
                    } else if (!is_numeric_dx && !is_numeric_fu) {
                        # Handle text datetime input via lubridate
                        lubridate_functions <- list(
                            ymdhms = lubridate::ymd_hms,
                            ymd = lubridate::ymd,
                            ydm = lubridate::ydm,
                            mdy = lubridate::mdy,
                            myd = lubridate::myd,
                            dmy = lubridate::dmy,
                            dym = lubridate::dym
                        )

                        if (timetypedata %in% names(lubridate_functions)) {
                            func <- lubridate_functions[[timetypedata]]
                            tryCatch({
                                mydata[["start"]] <- func(mydata[[dxdate]])
                                mydata[["end"]] <- func(mydata[[fudate]])
                            }, error = function(e) {
                                stop(.('Date parsing error: {error}. Please check that your dates match the selected format: {format}',
                                     error = e$message, format = timetypedata))
                            })
                        } else {
                            stop(.('Unsupported time type: {type}. Supported formats: {formats}',
                                 type = timetypedata, formats = paste(names(lubridate_functions), collapse = ", ")))
                        }
                    } else {
                        # Mixed types error
                        stop(.("Diagnosis date and follow-up date must be in the same format (both numeric or both text)"))
                    }


                    if ( sum(!is.na(mydata[["start"]])) == 0 || sum(!is.na(mydata[["end"]])) == 0)  {
                        stop(.('Time difference cannot be calculated. Make sure that time type in variables are correct. Currently it is: {type}', 
                             type = self$options$timetypedata))
                    }

                    timetypeoutput <-
                        jmvcore::constructFormula(terms = self$options$timetypeoutput)


                    mydata <- mydata %>%
                        dplyr::mutate(interval = lubridate::interval(start, end))


                    mydata <- mydata %>%
                        dplyr::mutate(mytime = lubridate::time_length(interval,
                                                                      timetypeoutput))

                }


                df_time <- mydata %>% jmvcore::select(c("row_names", "mytime"))

                return(df_time)


            }

            # Define Outcome ----
            ,
            .definemyoutcome = function() {


            labelled_data <- private$.getData()

            mydata <- labelled_data$mydata_labelled
            myoutcome_labelled <- labelled_data$myoutcome_labelled


                contin <- c("integer", "numeric", "double")

                outcomeLevel <- self$options$outcomeLevel
                multievent <- self$options$multievent

                outcome1 <- mydata[[myoutcome_labelled]]

                if (!multievent) {
                    if (inherits(outcome1, contin)) {
                        if (!((length(unique(
                            outcome1[!is.na(outcome1)]
                        )) == 2) && (sum(unique(
                            outcome1[!is.na(outcome1)]
                        )) == 1))) {
                            stop(.('When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.'))

                        }

                        mydata[["myoutcome"]] <- mydata[[myoutcome_labelled]]
                            # mydata[[self$options$outcome]]

                    } else if (inherits(outcome1, "factor")) {
                        mydata[["myoutcome"]] <-
                            ifelse(
                                test = outcome1 == outcomeLevel,
                                yes = 1,
                                no = 0
                            )

                    } else {
                        stop(.('When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0. If you are using a factor as an outcome, please check the levels and content.'))

                    }

                } else if (multievent) {
                    analysistype <- self$options$analysistype

                    dod <- self$options$dod
                    dooc <- self$options$dooc
                    awd <- self$options$awd
                    awod <- self$options$awod

                    if (analysistype == 'overall') {
                        # Overall ----
                        # (Alive) <=> (Dead of Disease & Dead of Other Causes)


                        mydata[["myoutcome"]] <- NA_integer_

                        mydata[["myoutcome"]][outcome1 == awd] <- 0
                        mydata[["myoutcome"]][outcome1 == awod] <- 0
                        mydata[["myoutcome"]][outcome1 == dod] <- 1
                        mydata[["myoutcome"]][outcome1 == dooc] <- 1



                    } else if (analysistype == 'cause') {
                        # Cause Specific ----
                        # (Alive & Dead of Other Causes) <=> (Dead of Disease)


                        mydata[["myoutcome"]] <- NA_integer_

                        mydata[["myoutcome"]][outcome1 == awd] <- 0
                        mydata[["myoutcome"]][outcome1 == awod] <- 0
                        mydata[["myoutcome"]][outcome1 == dod] <- 1
                        mydata[["myoutcome"]][outcome1 == dooc] <- 0

                    } else if (analysistype == 'compete') {
                        # Competing Risks ----
                        # Alive <=> Dead of Disease accounting for Dead of Other Causes

                        # https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#part_3:_competing_risks


                        mydata[["myoutcome"]] <- NA_integer_

                        mydata[["myoutcome"]][outcome1 == awd] <- 0
                        mydata[["myoutcome"]][outcome1 == awod] <- 0
                        mydata[["myoutcome"]][outcome1 == dod] <- 1
                        mydata[["myoutcome"]][outcome1 == dooc] <- 2

                    }

                }

                df_outcome <- mydata %>% jmvcore::select(c("row_names", "myoutcome"))

                return(df_outcome)

            }


            # Define Factor ----
            ,

            .definemyfactor = function() {


            labelled_data <- private$.getData()

            mydata_labelled <- labelled_data$mydata_labelled
            mycontexpl_labelled <- labelled_data$mycontexpl_labelled

            mydata <- mydata_labelled

            mydata[["myfactor"]] <- mydata[[mycontexpl_labelled]]


            df_factor <- mydata %>% jmvcore::select(c("row_names","myfactor"))

            return(df_factor)

            }

            # Validation Methods ----
            ,
            .validateAnalysisRequirements = function() {
                # Basic variable checks
                has_outcome <- !is.null(self$options$outcome)
                
                # If multi-event analysis is enabled, check for required event levels
                if (self$options$multievent) {
                    has_required_events <- !is.null(self$options$dod) || !is.null(self$options$dooc)
                    outcome_valid <- has_outcome && has_required_events
                } else {
                    outcome_valid <- has_outcome
                }
                
                # Check time variable requirements
                if (self$options$tint) {
                    # Time calculation from dates
                    time_valid <- !is.null(self$options$dxdate) && !is.null(self$options$fudate)
                } else {
                    # Direct elapsed time
                    time_valid <- !is.null(self$options$elapsedtime)
                }
                
                # Check continuous explanatory variable
                contexpl_valid <- !is.null(self$options$contexpl)
                
                basic_requirements <- outcome_valid && time_valid && contexpl_valid
                
                # Misuse Detection Guards
                if (basic_requirements && nrow(self$data) > 0) {
                    misuse_warnings <- private$.detectCommonMisuses()
                    if (length(misuse_warnings) > 0) {
                        warning_content <- private$.createInterpretationBox(
                            .("Statistical Analysis Warning"),
                            paste(misuse_warnings, collapse = "<br><br>"),
                            warning = TRUE
                        )
                        self$results$todo$setContent(warning_content)
                        self$results$todo$setVisible(TRUE)
                    }
                }
                
                return(basic_requirements)
            }
            ,
            .handleIncompleteAnalysis = function() {
                private$.todo()
                # Hide all analysis results
                self$results$coxSummary$setVisible(FALSE)
                self$results$coxTable$setVisible(FALSE)
                self$results$tCoxtext2$setVisible(FALSE)
                self$results$rescutTable$setVisible(FALSE)
                self$results$medianSummary$setVisible(FALSE)
                self$results$medianTable$setVisible(FALSE)
                self$results$survTableSummary$setVisible(FALSE)
                self$results$survTable$setVisible(FALSE)
                self$results$plot4$setVisible(FALSE)
                self$results$plot5$setVisible(FALSE)
                self$results$plot2$setVisible(FALSE)
                self$results$plot3$setVisible(FALSE)
                self$results$plot6$setVisible(FALSE)
                self$results$todo$setVisible(TRUE)
            }

            # Clean Data For Analysis ----
            ,
            .cleandata = function() {

            labelled_data <- private$.getData()

            mydata_labelled        <- labelled_data$mydata_labelled
            mytime_labelled        <- labelled_data$mytime_labelled
            myoutcome_labelled     <- labelled_data$myoutcome_labelled
            mydxdate_labelled      <- labelled_data$mydxdate_labelled
            myfudate_labelled      <- labelled_data$myfudate_labelled
            mycontexpl_labelled <- labelled_data$mycontexpl_labelled

                time <- private$.definemytime()
                outcome <- private$.definemyoutcome()
                factor <- private$.definemyfactor()

                private$.checkpoint()

                cleanData <- dplyr::left_join(time, outcome, by = "row_names") %>%
                    dplyr::left_join(factor, by = "row_names")

                # Landmark ----
                # https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#landmark_method
                if (self$options$uselandmark) {

                  landmark <- jmvcore::toNumeric(self$options$landmark)

                    cleanData <- cleanData %>%
                        dplyr::filter(mytime >= landmark) %>%
                        dplyr::mutate(mytime = mytime - landmark)
                }

                # Names cleanData ----

                if (self$options$tint) {
                    name1time <- "CalculatedTime"
                }

                if (!self$options$tint &&
                    !is.null(self$options$elapsedtime)) {
                    name1time <- mytime_labelled
                }

                name2outcome <- myoutcome_labelled

                if (self$options$multievent) {
                    name2outcome <- "CalculatedOutcome"
                }

                if (!is.null(self$options$contexpl)
                    ) {
                    name3contexpl <- mycontexpl_labelled
                    }

                    cleanData <- cleanData %>%
                        dplyr::rename(
                            !!name1time := mytime,
                            !!name2outcome := myoutcome,
                            !!name3contexpl := myfactor
                        )

                # naOmit ----

                cleanData <- jmvcore::naOmit(cleanData)

                # Return Data ----

                return(
                    list(
                        "name1time" = name1time,
                        "name2outcome" = name2outcome,
                        "name3contexpl" = name3contexpl,
                        "cleanData" = cleanData,
                        "mytime_labelled" = mytime_labelled,
                        "myoutcome_labelled" = myoutcome_labelled,
                        "mydxdate_labelled" = mydxdate_labelled,
                        "myfudate_labelled" = myfudate_labelled,
                        "mycontexpl_labelled" = mycontexpl_labelled
                    )
                )

            }



            # Run Analysis ----
            ,
            .run = function() {
                

                # Errors, Warnings ----

                ## No variable todo ----

                ## Validate Analysis Requirements ----
                not_continue_analysis <- !private$.validateAnalysisRequirements()


                if (not_continue_analysis) {
                    private$.handleIncompleteAnalysis()
                    return()
                } else {
                    self$results$todo$setVisible(FALSE)
                }


                ## Input Validation and Data Checks ----

                # Enhanced input validation using helper method
                if (nrow(self$data) == 0)
                    stop(.('Data contains no (complete) rows'))

                private$.checkpoint()

                # Get Clean Data ----
                results <- private$.cleandata()
                
                # Clinical Assumption Checking ----
                if (!is.null(results) && !is.null(results$cleanData)) {
                    clinical_warnings <- private$.checkClinicalAssumptions(
                        results$cleanData,
                        results$name1time,
                        results$name2outcome,
                        results$name3contexpl
                    )
                    
                    if (length(clinical_warnings) > 0) {
                        warning_content <- private$.createInterpretationBox(
                            .("Clinical Assumptions Warning"),
                            paste(clinical_warnings, collapse = "<br><br>"),
                            warning = TRUE
                        )
                        
                        # Store warnings for display
                        self$results$clinicalWarnings$setContent(warning_content)
                        self$results$clinicalWarnings$setVisible(TRUE)
                    }
                }
                
                # Additional validation after data cleaning
                if (!is.null(results$cleanData)) {
                    private$.validateInputs(
                        data = results$cleanData,
                        time_var = results$name1time,
                        outcome_var = results$name2outcome,
                        contexpl_var = results$name3contexpl
                    )
                    
                    # Memory usage monitoring for large datasets
                    private$.checkMemoryUsage(results$cleanData)
                }
                

                # Run Analysis ----

                ## Run Continious Cox Regression ----
                private$.cox(results)



                ## Add the person-time analysis ----
                private$.checkpoint()  # Add checkpoint here

                # Run person-time analysis if enabled
                if (self$options$person_time) {
                    private$.personTimeAnalysis(results)
                }



                ## Run RMST analysis before cutoff (if enabled) ----
                if (self$options$rmst_analysis) {
                    private$.calculateRMST(results)
                }

                ## Run Residual diagnostics before cutoff (if enabled) ----
                if (self$options$residual_diagnostics) {
                    private$.calculateResiduals(results)
                }

                ## Run Multiple Cut-offs Analysis (INDEPENDENT) ----
                multicut_results <- NULL
                
                
                
                if (self$options$multiple_cutoffs) {
                    # Use the original clean data, before any single cutoff processing
                    multicut_results <- private$.multipleCutoffs(results)
                    if (!is.null(multicut_results)) {
                        private$.multipleCutoffTables(multicut_results)
                        
                        # Store data for plots
                        self$results$plotMultipleCutoffs$setState(list(
                            multicut_results = multicut_results,
                            results = results
                        ))
                        
                        # Add multiple cutoff groups to data
                        if (self$options$calculatedmulticut &&
                            self$results$calculatedmulticut$isNotFilled()) {
                            self$results$calculatedmulticut$setRowNums(rownames(results$cleanData))
                            self$results$calculatedmulticut$setValues(multicut_results$risk_groups)
                        }
                    }
                }

                ## Run Cut-off calculation and further analysis ----
                if (!self$options$findcut) {
                    return()
                }


                ## Run Cut-off calculation ----
                res.cut <- private$.cutoff(results)

                ## Run Cut-off Table ----
                private$.cutoffTable(res.cut)

                ## Run Categorise Data ----
                cutoffdata <- private$.cutoff2(res.cut)

                ## Run RMST analysis with cutoff data (if enabled) ----
                if (self$options$rmst_analysis) {
                    private$.calculateRMST(results, cutoffdata)
                }

                ## Run Residual diagnostics with cutoff data (if enabled) ----
                if (self$options$residual_diagnostics) {
                    private$.calculateResiduals(results, cutoffdata)
                }





                # self$results$mydataview$setContent(
                #     list(
                #         res.cut = res.cut,
                #         cutoffdata = cutoffdata,
                #         not_continue_analysis = not_continue_analysis
                #     )
                # )



                ## Run median cutoff ----

                private$.mediancutoff(cutoffdata)

                ## Run life table cutoff ----

                private$.lifetablecutoff(cutoffdata)







                # Prepare Data For Plots ----

                plotData1 <- list(res.cut = res.cut,
                                  name3contexpl = results$name3contexpl
                                  # ,
                                  # not_continue_analysis = not_continue_analysis
                )

                # self$results$mydataview2$setContent(plotData1)


                image4 <- self$results$plot4
                image4$setState(plotData1)

                plotData2 <- list(
                    cutoffdata = cutoffdata,
                    results = results,
                    multicut_results = multicut_results
                    # ,
                    # not_continue_analysis = not_continue_analysis
                    )

                image5 <- self$results$plot5
                image5$setState(plotData2)

                image2 <- self$results$plot2
                image2$setState(plotData2)

                image3 <- self$results$plot3
                image3$setState(plotData2)

                image6 <- self$results$plot6
                image6$setState(plotData2)

                # Set state for new plots
                image7 <- self$results$plot7
                image7$setState(plotData2)

                image9 <- self$results$residualsPlot
                image9$setState(plotData2)



                # Add Calculated Time to Data ----


                if (self$options$tint && self$options$calculatedtime && self$results$calculatedtime$isNotFilled()) {
                    self$results$calculatedtime$setRowNums(results$cleanData$row_names)
                    self$results$calculatedtime$setValues(results$cleanData$CalculatedTime)
                }


                # Add Redefined Outcome to Data ----

                if (self$options$multievent  && self$options$outcomeredefined && self$results$outcomeredefined$isNotFilled()) {
                    self$results$outcomeredefined$setRowNums(results$cleanData$row_names)
                    self$results$outcomeredefined$setValues(results$cleanData$CalculatedOutcome)
                }


                # Add calculatedcutoff to Data ----

                cutoffgr <- cutoffdata[[results$name3contexpl]]

                if (self$options$calculatedcutoff &&
                        self$results$calculatedcutoff$isNotFilled()) {
                        self$results$calculatedcutoff$setValues(cutoffgr)
                }

            # Educational Explanations ----
            # Always add explanation content - visibility is controlled by YAML conditions
            private$.addExplanations()

            }





            # Continious Cox Regression ----
            ,
            .cox = function(results) {

                private$.checkpoint()

                ## Cox Regression ----
                
                # Wrap Cox regression analysis in error recovery
                cox_result <- private$.safeAnalysis(function() {
                    
                    mytime <- results$name1time
                    mytime <- jmvcore::constructFormula(terms = mytime)

                    myoutcome <- results$name2outcome
                    myoutcome <-
                        jmvcore::constructFormula(terms = myoutcome)

                    myfactor <- results$name3contexpl
                    myfactor <-
                        jmvcore::constructFormula(terms = myfactor)

                    mydata <- results$cleanData

                    mydata[[mytime]] <-
                        jmvcore::toNumeric(mydata[[mytime]])

                    myformula <-
                        paste("Surv(", mytime, ",", myoutcome, ")")

                    finalfit::finalfit(
                        .data = mydata,
                        dependent = myformula,
                        explanatory = myfactor,
                        metrics = TRUE
                    )
                }, context = .("Cox regression analysis"), fallback_value = list(NULL, NULL))
                
                tCox <- cox_result

                # Check if Cox analysis was successful
                if (is.null(tCox) || is.null(tCox[[1]]) || is.null(tCox[[2]])) {
                    error_msg <- .(
                        "Cox regression analysis failed. This may be due to insufficient data, convergence issues, or inappropriate data structure. Please check your variables and try again."
                    )
                    self$results$tCoxtext2$setContent(private$.createInterpretationBox(
                        .("Analysis Error"), error_msg, warning = TRUE
                    ))
                    return()
                }
                
                # Restore original variable names in finalfit output table
                if (!is.null(tCox[[1]]) && nrow(tCox[[1]]) > 0) {
                    labelled_data <- private$.getData()
                    tCox[[1]] <- private$.restoreOriginalNamesInTable(tCox[[1]], labelled_data$all_labels)
                }

                # Create enhanced Cox results with clinical context
                cox_tooltip <- private$.createClinicalTooltip(
                    .("Cox Regression Analysis"),
                    .("Cox regression estimates the hazard ratio (HR) which represents the risk of the event occurring at any time for one group relative to another. HR > 1 indicates increased risk, HR < 1 indicates decreased risk."),
                    .("HR = 2.5 means the group has 2.5 times higher risk of the event")
                )
                
                tCoxtext2 <- glue::glue(.(
                    '{tooltip}
                    **Model Metrics:**
                    {metrics}'),
                    tooltip = cox_tooltip,
                    metrics = unlist(tCox[[2]]))

                if (self$options$uselandmark) {
                    landmark <- jmvcore::toNumeric(self$options$landmark)

                    tCoxtext2 <- glue::glue(.(
                        '{previous_text} 
                        
                        **Landmark Analysis:** Analysis restricted to patients who survived beyond {time} {units}. This approach reduces lead time bias in prognostic studies.'),
                        previous_text = tCoxtext2,
                        time = landmark, 
                        units = self$options$timetypeoutput)
                }

                self$results$tCoxtext2$setContent(tCoxtext2)


                tCox_df <-
                    tibble::as_tibble(tCox[[1]], .name_repair = "minimal") %>%
                    janitor::clean_names(dat = ., case = "snake")


                # Continious Cox-Regression Table ----

                coxTable <- self$results$coxTable

                data_frame <- tCox_df

                names(data_frame) <- c(
                    "contexpl",
                    "Levels",
                    "all",
                    "HR_univariable",
                    "HR_multivariable"
                )

                for (i in seq_along(data_frame[, 1, drop = T])) {
                    coxTable$addRow(rowKey = i, values = c(data_frame[i, ]))
                }


                # Continious coxTable explanation ----


                tCox_df <-
                    tibble::as_tibble(tCox[[1]], .name_repair = "minimal") %>%
                    janitor::clean_names(dat = ., case = "snake")

                names(tCox_df) <- names(data_frame) <- c(
                    "Explanatory",
                    "Levels",
                    "all",
                    "HR_univariable",
                    "HR_multivariable"
                )


                # https://stackoverflow.com/questions/38470355/r-fill-empty-cell-with-value-of-last-non-empty-cell

                while (length(ind <-
                    which(tCox_df$Explanatory == "")) > 0) {
                    tCox_df$Explanatory[ind] <- tCox_df$Explanatory[ind - 1]
                }

                # https://stackoverflow.com/questions/51180290/mutate-by-group-in-r

                # Enhanced clinical language summary
                tCox_df %>%
                    dplyr::group_by(Explanatory) %>%
                    dplyr::mutate(firstlevel = dplyr::first(Levels)) %>%
                    dplyr::mutate(
                        coxdescription = private$.generateClinicalSentence(
                            "cox_regression", 
                            Explanatory, 
                            list(hr = HR_multivariable, hr_univariate = HR_univariable)
                        )
                    ) %>%
                    dplyr::filter(HR_univariable != "-") %>%
                    dplyr::pull(coxdescription) -> coxSummary

                # Add clinical interpretation box
                if (length(coxSummary) > 0) {
                    clinical_summary <- private$.createInterpretationBox(
                        .("Clinical Interpretation"),
                        paste(coxSummary, collapse = "<br><br>")
                    )
                    
                    enhanced_summary <- paste(
                        clinical_summary,
                        "<br><hr><br>",
                        .("Copy-ready summary for clinical reports:"),
                        "<br><em>", paste(coxSummary, collapse = " "), "</em>",
                        sep = ""
                    )
                    
                    self$results$coxSummary$setContent(enhanced_summary)
                } else {
                    self$results$coxSummary$setContent(.("No significant associations found in Cox regression analysis."))
                }
            }



                # Continuous Optimal Cut-off ----
            ,
            .cutoff = function(results) {

                # Wrap cutoff analysis in error recovery
                cutoff_result <- private$.safeAnalysis(function() {
                    
                    mytime <- results$name1time
                    mytime <- jmvcore::constructFormula(terms = mytime)

                    myoutcome <- results$name2outcome
                    myoutcome <-
                        jmvcore::constructFormula(terms = myoutcome)

                    myfactor <- results$name3contexpl
                    myfactor <-
                        jmvcore::constructFormula(terms = myfactor)

                    mydata <- results$cleanData

                    mydata[[mytime]] <-
                        jmvcore::toNumeric(mydata[[mytime]])

                    private$.checkpoint()

                    # https://rpkgs.datanovia.com/survminer/reference/surv_cutpoint.html

                    res.cut <- survminer::surv_cutpoint(
                        mydata,
                        time = mytime,
                        event = myoutcome,
                        variables = myfactor,
                        minprop = 0.1
                        # ,
                        # progressbar = TRUE
                    )
                    
                    return(res.cut)
                    
                }, context = .("Optimal cutoff analysis"), fallback_value = NULL)

                return(cutoff_result)

            }

            # Cut-off Table ----
            ,
            .cutoffTable = function(res.cut) {
                rescut_summary <- summary(res.cut)

                rescutTable <- self$results$rescutTable

                # Enhanced clinical title with tooltip
                clinical_tooltip <- private$.createClinicalTooltip(
                    .("Optimal Cut-off Analysis"),
                    .("This analysis identifies the cut-off value that best separates patients into high and low risk groups. The optimal cut-off maximizes the statistical difference in survival between groups while ensuring adequate sample sizes for reliable analysis."),
                    .("Cut-off = 3.5 means patients with values ≥3.5 are in the high-risk group")
                )

                rescutTable$setTitle(paste(
                    clinical_tooltip,
                    .('{var} Optimal Cut-off Analysis'),
                    sep = "<br>"
                ) %>% glue::glue(var = self$options$contexpl))

                # Add clinical interpretation if cutoff was found
                if (!is.null(rescut_summary) && nrow(rescut_summary) > 0) {
                    cutoff_value <- rescut_summary[1, "cutpoint"]
                    variable_name <- self$options$contexpl
                    
                    clinical_sentence <- private$.generateClinicalSentence(
                        "cutoff_analysis",
                        variable_name,
                        list(cutoff = cutoff_value)
                    )
                    
                    interpretation_box <- private$.createInterpretationBox(
                        .("Clinical Application"),
                        clinical_sentence
                    )
                    
                    # Set table note with clinical context
                    rescutTable$setNote("clinical", interpretation_box)
                }

                data_frame <- rescut_summary
                for (i in seq_along(data_frame[, 1, drop = T])) {
                    rescutTable$addRow(rowKey = i, values = c(data_frame[i, ]))
                }
            }

            # Categorise Data ----
            ,
            .cutoff2 = function(res.cut) {
                res.cat <- survminer::surv_categorize(res.cut)
                return(res.cat)
            }


            # Median ----
            ,
            .mediancutoff = function(cutoffdata) {

                private$.checkpoint()

                results <- private$.cleandata()

                mydata <- cutoffdata

                ## Median Survival Table ----

                mytime <- results$name1time
                myoutcome <- results$name2outcome
                mycontexpl <- results$name3contexpl


                mytime <-
                    jmvcore::constructFormula(terms = mytime)

                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)

                mycontexpl <-
                    jmvcore::constructFormula(terms = mycontexpl)


                mydata[[mytime]] <-
                    jmvcore::toNumeric(mydata[[mytime]])


                # Escape variable names for safe formula construction
                escaped_time <- private$.escapeVariableNames(mytime)
                escaped_outcome <- private$.escapeVariableNames(myoutcome)
                escaped_contexpl <- private$.escapeVariableNames(mycontexpl)
                
                formula <-
                    paste('survival::Surv(',
                          escaped_time,
                          ',',
                          escaped_outcome,
                          ') ~ ',
                          escaped_contexpl)

                formula <- as.formula(formula)

                km_fit <- survival::survfit(formula, data = mydata)


                km_fit_median_df <- summary(km_fit)

                results1html <-
                    as.data.frame(km_fit_median_df$table) %>%
                    janitor::clean_names(dat = ., case = "snake") %>%
                    tibble::rownames_to_column(.data = .)


                results1html[, 1] <- gsub(
                    pattern = ", ",
                    replacement = " and ",
                    x = results1html[, 1]
                )

                results1table <- results1html

                results1table <- results1html

                names(results1table)[1] <- "factor"


                results2table <- results1table

                # Apply name restoration for better display 
                labelled_data <- private$.getData()
                results2table$factor <- gsub(pattern = paste0(mycontexpl,"="),
                                             replacement = paste0(self$options$contexpl, " = "),
                                             x = results1table$factor)

                # self$results$mydataview$setContent(
                #     list(
                #         results2table = results2table
                #     )
                # )


                medianTable <- self$results$medianTable
                data_frame <- results2table
                for (i in seq_along(data_frame[, 1, drop = T])) {
                    medianTable$addRow(rowKey = i, values = c(data_frame[i,]))
                }


                ## Median Survival Summary ----

                results1table %>%
                    dplyr::mutate(
                        description =
                            glue::glue(.(
                                'When {factor_val}, median survival is {median_val} [{lower} - {upper}, 95% CI] {units}.'),
                                factor_val = factor,
                                median_val = round(median, digits = 1),
                                lower = round(x0_95lcl, digits = 1),
                                upper = round(x0_95ucl, digits = 1),
                                units = self$options$timetypeoutput)
                    ) %>%
                    dplyr::mutate(
                        description = dplyr::case_when(
                            is.na(median) ~ paste0(
                                glue::glue(.(
                                    '{desc}. Note that when {factor_val}, the survival curve does not drop below 1/2 during the observation period, thus the median survival is undefined.'),
                                    desc = description, factor_val = factor)),
                            TRUE ~ paste0(description)
                        )
                    ) %>%
                    dplyr::mutate(description = gsub(
                        pattern = "=",
                        replacement = " is ",
                        x = description
                    )) %>%
                    dplyr::mutate(description = gsub(
                        pattern = mycontexpl,
                        replacement = self$options$contexpl,
                        x = description
                    )) %>%
                    dplyr::select(description) %>%
                    dplyr::pull(.) -> km_fit_median_definition

                medianSummary <- c(km_fit_median_definition,
                                   "The median survival time is when 50% of subjects have experienced the event.",
                                   "This means that 50% of subjects in this group survived longer than this time period."
                )


                self$results$medianSummary$setContent(medianSummary)

            }


            # Life Table ----
            ,
            .lifetablecutoff = function(cutoffdata) {

                private$.checkpoint()


                # survival table with flexible time points (preserving 1,3,5-yr default) ----

                # Use enhanced parsing method with 1,3,5 year defaults
                utimes <- private$.parseSurvivalTimePoints(self$options$cutp)

                results <- private$.cleandata()

                mydata <- cutoffdata

                mytime <- results$name1time
                myoutcome <- results$name2outcome
                mycontexpl <- results$name3contexpl


                mytime <-
                    jmvcore::constructFormula(terms = mytime)

                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)

                mycontexpl <-
                    jmvcore::constructFormula(terms = mycontexpl)


                mydata[[mytime]] <-
                    jmvcore::toNumeric(mydata[[mytime]])


                # Escape variable names for safe formula construction
                escaped_time <- private$.escapeVariableNames(mytime)
                escaped_outcome <- private$.escapeVariableNames(myoutcome)
                escaped_contexpl <- private$.escapeVariableNames(mycontexpl)
                
                formula <-
                    paste('survival::Surv(',
                          escaped_time,
                          ',',
                          escaped_outcome,
                          ') ~ ',
                          escaped_contexpl)

                formula <- as.formula(formula)

                km_fit <- survival::survfit(formula, data = mydata)

                km_fit_summary <- summary(km_fit, times = utimes, extend = TRUE)

                km_fit_df <-
                    as.data.frame(km_fit_summary[c(
                        "strata",
                        "time",
                        "n.risk",
                        "n.event",
                        "surv",
                        "std.err",
                        "lower",
                        "upper"
                    )])

                km_fit_df[, 1] <- gsub(
                    pattern = "thefactor=",
                    replacement = paste0(self$options$contexpl, " "),
                    x = km_fit_df[, 1]
                )


                km_fit_df2 <- km_fit_df

                km_fit_df2$strata <- gsub(pattern = paste0(mycontexpl,"="),
                                             replacement = paste0(self$options$contexpl, " = "),
                                             x =km_fit_df2$strata)

                data_frame <- km_fit_df2

                survTable <- self$results$survTable


                for (i in seq_along(data_frame[, 1, drop = T])) {
                    survTable$addRow(rowKey = i, values = c(data_frame[i, ]))
                }




                # survTableSummary 1,3,5-yr survival summary ----

                km_fit_df[, 1] <- gsub(
                    pattern = paste0(mycontexpl,"="),
                    replacement = paste0(self$options$contexpl, " is "),
                    x = km_fit_df[, 1]
                )


                km_fit_df %>%
                    dplyr::mutate(
                        description =
                            glue::glue(.(
                                'When {strata_val}, {time_val} month survival is {survival} [{ci_low}-{ci_high}, 95% CI].'),
                                strata_val = strata, time_val = time,
                                survival = scales::percent(surv),
                                ci_low = scales::percent(lower),
                                ci_high = scales::percent(upper))
                    ) %>%
                    dplyr::select(description) %>%
                    dplyr::pull(.) -> survTableSummary

                self$results$survTableSummary$setContent(survTableSummary)
            }


            ,
            # Person-Time Analysis Function ----
            .personTimeAnalysis = function(results) {
                # Check if person_time option is enabled
                if (!self$options$person_time) {
                    return()
                }

                # Extract data
                mytime <- results$name1time
                myoutcome <- results$name2outcome
                mydata <- results$cleanData

                # self$results$mydataview_personTimeAnalysis$setContent(
                #   list(
                #     mytime = mytime,
                #     myoutcome = myoutcome,
                #     mydata = mydata
                #   )
                # )

                # Ensure time is numeric
                mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])

                # Get total observed time
                total_time <- sum(mydata[[mytime]])

                # FIX: Count events properly - any non-zero value is an event (handles competing risk with code 2)
                total_events <- sum(mydata[[myoutcome]] >= 1, na.rm = TRUE)

                # Get time unit
                time_unit <- self$options$timetypeoutput

                # Get rate multiplier
                rate_multiplier <- self$options$rate_multiplier

                # Calculate overall incidence rate
                overall_rate <- (total_events / total_time) * rate_multiplier

                # Calculate confidence intervals using Poisson exact method
                ci_lower <- (stats::qchisq(0.025, 2*total_events) / 2) / total_time * rate_multiplier
                ci_upper <- (stats::qchisq(0.975, 2*(total_events + 1)) / 2) / total_time * rate_multiplier

                # Add to personTimeTable - first the overall row
                self$results$personTimeTable$addRow(rowKey=1, values=list(
                    interval=paste0("Overall (0-max)"),
                    events=total_events,
                    person_time=round(total_time, 2),
                    rate=round(overall_rate, 2),
                    rate_ci_lower=round(ci_lower, 2),
                    rate_ci_upper=round(ci_upper, 2)
                ))

                # Parse time intervals for stratified analysis
                time_intervals <- as.numeric(unlist(strsplit(self$options$time_intervals, ",")))
                time_intervals <- sort(unique(time_intervals))

                if (length(time_intervals) > 0) {
                    # Create time intervals with configurable multiplier
                    max_time_extended <- private$.calculateTimeIntervals(mydata[[mytime]])
                    breaks <- c(0, time_intervals, max_time_extended)

                    # Loop through intervals
                    for (i in 1:(length(breaks)-1)) {
                        start_time <- breaks[i]
                        end_time <- breaks[i+1]

                        # Add checkpoint for responsiveness using configurable frequency
                        private$.performCheckpoint(i)

                        # Filter data for this interval
                        if (i == 1) {
                            # For first interval, include patients from the beginning
                            interval_data <- mydata
                            # But truncate follow-up time to the interval end
                            follow_up_times <- pmin(mydata[[mytime]], end_time)
                            # FIX: Count events consistently with overall count
                            # For competing risk, this counts all events (both event of interest and competing)
                            # Consider using == 1 if you want event-of-interest-specific rates
                            events_in_interval <- sum(mydata[[myoutcome]] >= 1 & mydata[[mytime]] <= end_time, na.rm = TRUE)
                        } else {
                            # For later intervals, include only patients who survived past the previous cutpoint
                            survivors <- mydata[[mytime]] > start_time
                            interval_data <- mydata[survivors, ]

                            if (nrow(interval_data) == 0) {
                                # Skip if no patients in this interval
                                next
                            }

                            # Adjust entry time and follow-up time
                            adjusted_entry_time <- rep(start_time, nrow(interval_data))
                            adjusted_exit_time <- pmin(interval_data[[mytime]], end_time)
                            follow_up_times <- adjusted_exit_time - adjusted_entry_time

                            # FIX: Count events consistently with overall count
                            events_in_interval <- sum(interval_data[[myoutcome]] >= 1 &
                                                          interval_data[[mytime]] <= end_time &
                                                          interval_data[[mytime]] > start_time, na.rm = TRUE)
                        }

                        # Sum person-time in this interval
                        person_time_in_interval <- sum(follow_up_times)

                        # Calculate interval incidence rate
                        if (person_time_in_interval > 0) {
                            interval_rate <- (events_in_interval / person_time_in_interval) * rate_multiplier

                            # Calculate confidence intervals
                            if (events_in_interval > 0) {
                                interval_ci_lower <- (stats::qchisq(0.025, 2*events_in_interval) / 2) / person_time_in_interval * rate_multiplier
                                interval_ci_upper <- (stats::qchisq(0.975, 2*(events_in_interval + 1)) / 2) / person_time_in_interval * rate_multiplier
                            } else {
                                interval_ci_lower <- 0
                                interval_ci_upper <- (stats::qchisq(0.975, 2) / 2) / person_time_in_interval * rate_multiplier
                            }

                            # Add to personTimeTable
                            self$results$personTimeTable$addRow(rowKey=i+1, values=list(
                                interval=paste0(start_time, "-", end_time),
                                events=events_in_interval,
                                person_time=round(person_time_in_interval, 2),
                                rate=round(interval_rate, 2),
                                rate_ci_lower=round(interval_ci_lower, 2),
                                rate_ci_upper=round(interval_ci_upper, 2)
                            ))
                        }
                    }
                }

                # Create summary text with interpretation
                summary_html <- glue::glue(.(
                    '<h4>Person-Time Analysis Summary</h4>
                    <p>Total follow-up time: <b>{total_time_val} {units}</b></p>
                    <p>Number of events: <b>{events}</b></p>
                    <p>Overall incidence rate: <b>{rate}</b> per {multiplier} {units} [95% CI: {lower}-{upper}]</p>
                    <p>This represents the rate at which events occurred in your study population. The incidence rate is calculated as the number of events divided by the total person-time at risk.</p>'),
                    total_time_val = round(total_time, 1), units = time_unit,
                    events = total_events, rate = round(overall_rate, 2),
                    multiplier = rate_multiplier,
                    lower = round(ci_lower, 2), upper = round(ci_upper, 2))

                self$results$personTimeSummary$setContent(summary_html)
            }







            # Cut-off Plot ----
            ,
            .plot4 = function(image4, ggtheme, theme, ...) {

                if (!self$options$findcut) {
                    return()
                }

                plotData <- image4$state

                if (is.null(plotData)) {
                    return()
                }

                # if (plotData$not_continue_analysis) {
                #     return()
                # }

                private$.checkpoint()

                res.cut <- plotData$res.cut

                name3contexpl <- plotData$name3contexpl

                plot4 <-
                    plot(res.cut, name3contexpl, palette = "npg")

                print(plot4)
                TRUE
            }


            # Survival Curve with new cut-off ----
            ,
            .plot5 = function(image5, ggtheme, theme, ...) {



                if (!self$options$findcut) {
                    return()
                }

                plotData <- image5$state

                if (is.null(plotData)) {
                    return()
                }

                # if (plotData$not_continue_analysis) {
                #     return()
                # }

                private$.checkpoint()

                res.cat <- plotData$cutoffdata

                results <- plotData$results

                mytime <- results$name1time
                myoutcome <- results$name2outcome
                mycontexpl <- results$name3contexpl


                mytime <-
                    jmvcore::constructFormula(terms = mytime)

                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)

                mycontexpl <-
                    jmvcore::constructFormula(terms = mycontexpl)


                # Escape variable names for safe formula construction
                escaped_time <- private$.escapeVariableNames(mytime)
                escaped_outcome <- private$.escapeVariableNames(myoutcome)
                escaped_contexpl <- private$.escapeVariableNames(mycontexpl)
                
                formula <-
                    paste('survival::Surv(',
                          escaped_time,
                          ',',
                          escaped_outcome,
                          ') ~ ',
                          escaped_contexpl)

                myformula <- as.formula(formula)


                fit <- survminer::surv_fit(
                    formula = myformula,
                    data = res.cat
                )

                plot5 <- survminer::ggsurvplot(
                    fit,
                    data = res.cat,
                    risk.table = self$options$risktable,
                    conf.int = self$options$ci95

                )
                print(plot5)
                TRUE
            }


            # Cumulative Events with new cut-off ----
            # https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf
            ,
            .plot2 = function(image2, ggtheme, theme, ...) {

                if (!self$options$findcut) {
                    return()
                }

                if (!self$options$ce) {
                    return()
                }

                plotData <- image2$state


                if (is.null(plotData)) {
                    return()
                }

                # if (plotData$not_continue_analysis) {
                #     return()
                # }

                res.cat <- plotData$cutoffdata

                results <- plotData$results

                mytime <- results$name1time
                myoutcome <- results$name2outcome
                mycontexpl <- results$name3contexpl

                mytime <-
                    jmvcore::constructFormula(terms = mytime)

                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)

                mycontexpl <-
                    jmvcore::constructFormula(terms = mycontexpl)


                myformula <-
                    paste0('survival::Surv(',
                          mytime,
                          ',',
                          myoutcome,
                          ')')

                title2 <- as.character(mycontexpl)

                plot2 <- res.cat %>%
                    finalfit::surv_plot(
                        .data = .,
                        dependent = myformula,
                        explanatory = mycontexpl,
                        xlab = paste0("Time (", self$options$timetypeoutput, ")"),
                        # pval = TRUE,
                        legend = "none",
                        break.time.by = self$options$byplot,
                        xlim = c(0, self$options$endplot),
                        ylim = c(
                            self$options$ybegin_plot,
                            self$options$yend_plot),
                        title = paste0("Cumulative Events ", title2),
                        fun = "event",
                        risk.table = self$options$risktable,
                        conf.int = self$options$ci95,
                        censor = self$options$censored,
                        surv.median.line = self$options$medianline

                    )


                print(plot2)
                TRUE
            }



            # Cumulative Hazard with new cut-off ----
            ,
            .plot3 = function(image3, ggtheme, theme, ...) {

                if (!self$options$findcut) {
                    return()
                }

                if (!self$options$ch) {
                    return()
                }

                plotData <- image3$state

                if (is.null(plotData)) {
                    return()
                }

                # if (plotData$not_continue_analysis) {
                #     return()
                # }

                res.cat <- plotData$cutoffdata

                results <- plotData$results

                mytime <- results$name1time
                myoutcome <- results$name2outcome
                mycontexpl <- results$name3contexpl


                mytime <-
                    jmvcore::constructFormula(terms = mytime)

                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)

                mycontexpl <-
                    jmvcore::constructFormula(terms = mycontexpl)


                myformula <-
                    paste('survival::Surv(',
                          mytime,
                          ',',
                          myoutcome,
                          ')')

                title2 <- as.character(mycontexpl)

                plot3 <- res.cat %>%
                    finalfit::surv_plot(
                        .data = .,
                        dependent = myformula,
                        explanatory = mycontexpl,
                        xlab = paste0("Time (", self$options$timetypeoutput, ")"),
                        # pval = TRUE,
                        legend = "none",
                        break.time.by = self$options$byplot,
                        xlim = c(0, self$options$endplot),
                        ylim = c(
                            self$options$ybegin_plot,
                            self$options$yend_plot),
                        title = paste0("Cumulative Hazard ", title2),
                        fun = "cumhaz",
                        risk.table = self$options$risktable,
                        conf.int = self$options$ci95,
                        censor = self$options$censored,
                        surv.median.line = self$options$medianline
                    )


                print(plot3)
                TRUE
            }


            # KMunicate Style with new cut-off ----
            ,
            .plot6 = function(image6, ggtheme, theme, ...) {

                if (!self$options$findcut) {
                    return()
                }

                if (!self$options$kmunicate) {
                    return()
                }

                plotData <- image6$state

                if (is.null(plotData)) {
                    return()
                }

                # if (plotData$not_continue_analysis) {
                #     return()
                # }

                res.cat <- plotData$cutoffdata

                results <- plotData$results

                mytime <- results$name1time
                myoutcome <- results$name2outcome
                mycontexpl <- results$name3contexpl


                mytime <-
                    jmvcore::constructFormula(terms = mytime)

                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)

                mycontexpl <-
                    jmvcore::constructFormula(terms = mycontexpl)


                myformula <-
                    paste('survival::Surv(',
                          mytime,
                          ',',
                          myoutcome,
                          ') ~ ',
                          mycontexpl)

                myformula <- as.formula(myformula)

                # myformula <-
                #     paste0("survival::Surv(mytime, myoutcome) ~ ", contfactor)

                km_fit <- survival::survfit(myformula, data = res.cat)

                time_scale <-
                    seq(0, self$options$endplot, by = self$options$byplot)


                plot6 <-
                    KMunicate::KMunicate(
                        fit = km_fit,
                        time_scale = time_scale,
                        .xlab = paste0("Time in ", self$options$timetypeoutput)
                    )


                print(plot6)
                TRUE
            }

            # Multiple Cut-offs Analysis ----
            ,
            .multipleCutoffs = function(results) {
                tryCatch({
                    mytime <- results$name1time
                    myoutcome <- results$name2outcome
                    mycontexpl <- results$name3contexpl
                    mydata <- results$cleanData
                    
                    # Convert to numeric
                    mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])
                    
                    # Extract continuous variable values
                    if (!mycontexpl %in% names(mydata)) {
                        warning(.('Variable {var} not found in data. Available columns: {cols}'), 
                                var = mycontexpl, cols = paste(names(mydata), collapse = ", "))
                        return(NULL)
                    }
                    
                    cont_var <- mydata[[mycontexpl]]
                    if (is.null(cont_var)) {
                        warning(.('Variable {var} is NULL'), var = mycontexpl)
                        return(NULL)
                    }
                    
                    cont_var <- cont_var[!is.na(cont_var)]
                    
                    # Check if we have enough data
                    if (length(cont_var) < 10) {
                        warning(.('Insufficient data for multiple cutoffs analysis'))
                        return(NULL)
                    }
                    


                # self$results$mydataview_multipleCutoffs2$setContent(
                #                     list(
                #                         name_of_debug = c('inside multipleCutoffs'),
                #                         mytime = mytime,
                #                         myoutcome = myoutcome,
                #                         mycontexpl = mycontexpl,
                #                         mydata = mydata,
                #                         cont_var = cont_var
                #                         )
                #                 )



                    # Debug information
                    message(glue::glue(.('Data columns: {cols}'), cols = paste(names(mydata),collapse = ", ")))
                    message(glue::glue(.('Trying to access variable: {var}'), var = mycontexpl))
                    message(glue::glue(.('Variable exists: {exists}'), exists = mycontexpl %in% names(mydata)))
                    if (mycontexpl %in% names(mydata)) {
                        message(glue::glue(.('Multiple cutoffs analysis: n = {n}, method = {method}, num_cuts = {cuts}'), 
                        n = nrow(results$data), method = self$options$cutoff_method, cuts = self$options$num_cutoffs))
                    }
                    
                    # Determine number of cutoffs
                    num_cuts <- switch(self$options$num_cutoffs,
                                       "two" = 2,
                                       "three" = 3,
                                       "four" = 4)
                    
                    # Calculate cutoffs based on method
                    cutoff_values <- switch(self$options$cutoff_method,
                        "quantile" = private$.quantileCutoffs(cont_var, num_cuts),
                        "recursive" = private$.recursiveCutoffs(mydata, mytime, myoutcome, mycontexpl, num_cuts),
                        "tree" = private$.treeCutoffs(mydata, mytime, myoutcome, mycontexpl, num_cuts),
                        "minpval" = private$.minPvalueCutoffs(mydata, mytime, myoutcome, mycontexpl, num_cuts)
                    )
                    
                    # Check if cutoffs were successfully calculated
                    if (is.null(cutoff_values) || length(cutoff_values) == 0) {
                        warning(.('Failed to calculate cutoff values'))
                        return(NULL)
                    }
                    
                    # Create risk groups
                    risk_groups <- private$.createRiskGroups(mydata[[mycontexpl]], cutoff_values)
                    
                    
                    # Calculate survival statistics for each group
                    group_stats <- private$.calculateGroupStats(mydata, mytime, myoutcome, risk_groups)
                    
                    return(list(
                        cutoff_values = cutoff_values,
                        risk_groups = risk_groups,
                        group_stats = group_stats,
                        method = self$options$cutoff_method,
                        num_cuts = num_cuts,
                        original_data = mydata,
                        mytime = mytime,
                        myoutcome = myoutcome
                    ))
                }, error = function(e) {
                    warning(.('Error in multiple cutoffs analysis: {error}'), error = e$message)
                    return(NULL)
                })
            }

            # Quantile-based cutoffs ----
            ,
            .quantileCutoffs = function(cont_var, num_cuts) {
                if (num_cuts == 2) {
                    quantiles <- c(1/3, 2/3)
                } else if (num_cuts == 3) {
                    quantiles <- c(0.25, 0.5, 0.75)
                } else if (num_cuts == 4) {
                    quantiles <- c(0.2, 0.4, 0.6, 0.8)
                }
                
                cutoffs <- quantile(cont_var, probs = quantiles, na.rm = TRUE)
                return(as.numeric(cutoffs))
            }

            # Recursive optimal cutoffs ----
            ,
            .recursiveCutoffs = function(mydata, mytime, myoutcome, mycontexpl, num_cuts) {
                if (!requireNamespace("survminer", quietly = TRUE)) {
                    return(private$.quantileCutoffs(mydata[[mycontexpl]], num_cuts))
                }
                
                cutoffs <- numeric(num_cuts)
                current_data <- mydata
                
                for (i in 1:num_cuts) {
                    tryCatch({
                        res.cut <- survminer::surv_cutpoint(
                            current_data,
                            time = mytime,
                            event = myoutcome,
                            variables = mycontexpl,
                            minprop = self$options$min_group_size / 100
                        )
                        
                        cutoffs[i] <- summary(res.cut)$cutpoint
                        
                        # Remove data around cutpoint for next iteration
                        if (i < num_cuts) {
                            cutoff_val <- cutoffs[i]
                            margin <- 0.1 * sd(current_data[[mycontexpl]], na.rm = TRUE)
                            current_data <- current_data[
                                abs(current_data[[mycontexpl]] - cutoff_val) > margin, 
                            ]
                        }
                    }, error = function(e) {
                        # Fallback to quantile method
                        remaining_cuts <- num_cuts - i + 1
                        fallback_cuts <- private$.quantileCutoffs(current_data[[mycontexpl]], remaining_cuts)
                        cutoffs[i:num_cuts] <<- fallback_cuts
                    })
                }
                
                return(sort(cutoffs))
            }

            # Tree-based partitioning ----
            ,
            .treeCutoffs = function(mydata, mytime, myoutcome, mycontexpl, num_cuts) {
                if (!requireNamespace("rpart", quietly = TRUE)) {
                    return(private$.quantileCutoffs(mydata[[mycontexpl]], num_cuts))
                }
                
                tryCatch({
                    # Prepare survival formula with escaped variable names
                    escaped_time <- private$.escapeVariableNames(mytime)
                    escaped_outcome <- private$.escapeVariableNames(myoutcome)
                    escaped_contexpl <- private$.escapeVariableNames(mycontexpl)
                    
                    formula_str <- paste0("survival::Surv(", escaped_time, ", ", escaped_outcome, ") ~ ", escaped_contexpl)
                    formula <- as.formula(formula_str)
                    
                    # Fit survival tree with specified depth
                    tree_fit <- rpart::rpart(
                        formula, 
                        data = mydata,
                        method = "exp",
                        control = rpart::rpart.control(
                            maxdepth = num_cuts + 1,
                            minsplit = max(10, nrow(mydata) * self$options$min_group_size / 100),
                            cp = 0.01
                        )
                    )
                    
                    # Extract split points
                    splits <- tree_fit$splits
                    if (is.null(splits) || nrow(splits) == 0) {
                        return(private$.quantileCutoffs(mydata[[mycontexpl]], num_cuts))
                    }
                    
                    cutoffs <- unique(splits[splits[,1] == mycontexpl, "index"])
                    cutoffs <- sort(cutoffs)
                    
                    if (length(cutoffs) > num_cuts) {
                        cutoffs <- cutoffs[1:num_cuts]
                    } else if (length(cutoffs) < num_cuts) {
                        # Supplement with quantile cutoffs
                        additional_cuts <- private$.quantileCutoffs(mydata[[mycontexpl]], num_cuts - length(cutoffs))
                        cutoffs <- sort(c(cutoffs, additional_cuts))
                        cutoffs <- unique(cutoffs)[1:num_cuts]
                    }
                    
                    return(cutoffs)
                }, error = function(e) {
                    return(private$.quantileCutoffs(mydata[[mycontexpl]], num_cuts))
                })
            }

            # Minimum p-value cutoffs ----
            ,
            .minPvalueCutoffs = function(mydata, mytime, myoutcome, mycontexpl, num_cuts) {
                cont_var <- mydata[[mycontexpl]]
                sorted_vals <- sort(unique(cont_var))
                
                # Ensure minimum group size
                min_n <- ceiling(nrow(mydata) * self$options$min_group_size / 100)
                valid_cuts <- sorted_vals[(min_n + 1):(length(sorted_vals) - min_n)]
                
                if (length(valid_cuts) < num_cuts) {
                    return(private$.quantileCutoffs(cont_var, num_cuts))
                }
                
                # Test multiple combinations of cutoffs
                best_pval <- 1
                best_cuts <- numeric(num_cuts)
                
                # Sample cutoff combinations to avoid computational explosion
                n_samples <- min(1000, choose(length(valid_cuts), num_cuts))
                
                tryCatch({
                    for (i in 1:n_samples) {
                        test_cuts <- sort(sample(valid_cuts, num_cuts))
                        test_groups <- private$.createRiskGroups(cont_var, test_cuts)
                        
                        # Calculate log-rank test p-value
                        escaped_time <- private$.escapeVariableNames(mytime)
                        escaped_outcome <- private$.escapeVariableNames(myoutcome)
                        formula_str <- paste0("survival::Surv(", escaped_time, ", ", escaped_outcome, ") ~ test_groups")
                        formula <- as.formula(formula_str)
                        
                        test_data <- mydata
                        test_data$test_groups <- test_groups
                        
                        logrank_test <- survival::survdiff(formula, data = test_data)
                        pval <- 1 - pchisq(logrank_test$chisq, df = length(logrank_test$n) - 1)
                        
                        if (pval < best_pval) {
                            best_pval <- pval
                            best_cuts <- test_cuts
                        }
                    }
                    
                    return(best_cuts)
                }, error = function(e) {
                    return(private$.quantileCutoffs(cont_var, num_cuts))
                })
            }

            # Create risk groups from cutoffs ----
            ,
            .createRiskGroups = function(cont_var, cutoffs) {
                if (length(cutoffs) == 2) {
                    groups <- ifelse(cont_var <= cutoffs[1], "Low Risk",
                                   ifelse(cont_var <= cutoffs[2], "Medium Risk", "High Risk"))
                    level_order <- c("Low Risk", "Medium Risk", "High Risk")
                } else if (length(cutoffs) == 3) {
                    groups <- ifelse(cont_var <= cutoffs[1], "Low Risk",
                                   ifelse(cont_var <= cutoffs[2], "Medium-Low Risk",
                                         ifelse(cont_var <= cutoffs[3], "Medium-High Risk", "High Risk")))
                    level_order <- c("Low Risk", "Medium-Low Risk", "Medium-High Risk", "High Risk")
                } else if (length(cutoffs) == 4) {
                    groups <- ifelse(cont_var <= cutoffs[1], "Very Low Risk",
                                   ifelse(cont_var <= cutoffs[2], "Low Risk",
                                         ifelse(cont_var <= cutoffs[3], "Medium Risk",
                                               ifelse(cont_var <= cutoffs[4], "High Risk", "Very High Risk"))))
                    level_order <- c("Very Low Risk", "Low Risk", "Medium Risk", "High Risk", "Very High Risk")
                } else {
                    # Fallback for other numbers of cutoffs
                    groups <- cut(cont_var, breaks = c(-Inf, cutoffs, Inf), 
                                labels = paste("Group", 1:(length(cutoffs) + 1)))
                    level_order <- paste("Group", 1:(length(cutoffs) + 1))
                }
                
                # Filter level_order to only include levels that actually exist in the data
                existing_levels <- intersect(level_order, unique(groups))
                return(factor(groups, levels = existing_levels))
            }

            # Calculate survival statistics by group ----
            ,
            .calculateGroupStats = function(mydata, mytime, myoutcome, risk_groups) {
                stats_list <- list()
                
                for (group in levels(risk_groups)) {
                    group_data <- mydata[risk_groups == group, ]
                    
                    if (nrow(group_data) > 0) {
                        # Calculate median survival
                        escaped_time <- private$.escapeVariableNames(mytime)
                        escaped_outcome <- private$.escapeVariableNames(myoutcome)
                        formula_str <- paste0("survival::Surv(", escaped_time, ", ", escaped_outcome, ") ~ 1")
                        formula <- as.formula(formula_str)
                        
                        km_fit <- survival::survfit(formula, data = group_data)
                        
                        # Extract median survival statistics safely
                        surv_summary <- summary(km_fit)
                        median_val <- if (!is.null(surv_summary$table)) {
                            surv_summary$table["median"]
                        } else {
                            NA
                        }
                        
                        lower_val <- if (!is.null(surv_summary$table)) {
                            surv_summary$table["0.95LCL"]
                        } else {
                            NA
                        }
                        
                        upper_val <- if (!is.null(surv_summary$table)) {
                            surv_summary$table["0.95UCL"]
                        } else {
                            NA
                        }
                        
                        stats_list[[group]] <- list(
                            group = group,
                            n = nrow(group_data),
                            events = sum(group_data[[myoutcome]], na.rm = TRUE),
                            median_surv = as.numeric(median_val),
                            median_lower = as.numeric(lower_val),
                            median_upper = as.numeric(upper_val),
                            surv_fit = km_fit  # Store the survival fit object for time-specific survival calculations
                        )
                    }
                }
                
                return(stats_list)
            }

            # Populate multiple cutoffs tables ----
            ,
            .multipleCutoffTables = function(multicut_results) {
                message(glue::glue(.('multipleCutoffTables called')))
                
                # Check if results are valid
                if (is.null(multicut_results) || 
                    is.null(multicut_results$cutoff_values) ||
                    is.null(multicut_results$group_stats)) {
                    message(glue::glue(.('multicut_results is null or invalid')))
                    return()
                }
                
                message(glue::glue(.('Populating tables with {n} cutoffs'), n = length(multicut_results$cutoff_values)))
                
                # Populate cut-off points table (without statistical columns)
                cutoff_table <- self$results$multipleCutTable
                cutoff_table$deleteRows()  # Clear existing rows
                
                for (i in seq_along(multicut_results$cutoff_values)) {
                    cutoff_table$addRow(rowKey = i, values = list(
                        cutpoint_number = i,
                        cutpoint_value = round(multicut_results$cutoff_values[i], 2),
                        group_created = paste("Group", i + 1)
                    ))
                }
                
                # Calculate and display overall log-rank test as separate text
                if (!is.null(multicut_results$risk_groups) && length(unique(multicut_results$risk_groups)) > 1) {
                    # Get the original data
                    mydata <- multicut_results$original_data
                    mytime <- multicut_results$mytime  
                    myoutcome <- multicut_results$myoutcome
                    
                    # Perform log-rank test comparing all groups
                    escaped_time <- private$.escapeVariableNames(mytime)
                    escaped_outcome <- private$.escapeVariableNames(myoutcome)
                    formula_str <- paste0("survival::Surv(", escaped_time, ", ", escaped_outcome, ") ~ multicut_results$risk_groups")
                    tryCatch({
                        logrank_test <- survival::survdiff(as.formula(formula_str), data = mydata)
                        overall_chisq <- logrank_test$chisq
                        overall_pval <- 1 - pchisq(logrank_test$chisq, df = length(logrank_test$n) - 1)
                        
                        # Set the log-rank test results as text
                        logrank_text <- paste0("Overall Log-rank Test: χ² = ", round(overall_chisq, 3), 
                                             " (df = ", length(logrank_test$n) - 1, "), p = ", 
                                             ifelse(overall_pval < 0.001, "< 0.001", round(overall_pval, 3)))
                        
                        # Add interpretation 
                        interpretation <- if (overall_pval < 0.05) {
                            "The multiple cutoffs significantly differentiate survival between risk groups."
                        } else {
                            "The multiple cutoffs do not significantly differentiate survival between risk groups."
                        }
                        
                        full_text <- paste(logrank_text, interpretation, sep = "\n\n")
                        
                        # Store in a text result (we'll need to check what text output is available)
                        # For now, let's add it as a note
                        if (!is.null(self$results$multipleCutTable)) {
                            self$results$multipleCutTable$setNote("logrank", full_text)
                        }
                        
                    }, error = function(e) {
                        message(glue::glue(.('Error calculating log-rank test: {error}'), error = e$message))
                    })
                }
                
                # Populate median survival table
                median_table <- self$results$multipleMedianTable
                median_table$deleteRows()  # Clear existing rows
                
                for (group_name in names(multicut_results$group_stats)) {
                    stats <- multicut_results$group_stats[[group_name]]
                    if (!is.null(stats)) {
                        median_table$addRow(rowKey = group_name, values = list(
                            risk_group = stats$group,
                            n_patients = stats$n,
                            events = stats$events,
                            median_survival = if(is.na(stats$median_surv)) "NR" else round(stats$median_surv, 1),
                            median_lower = if(is.na(stats$median_lower)) NA else round(stats$median_lower, 1),
                            median_upper = if(is.na(stats$median_upper)) NA else round(stats$median_upper, 1)
                        ))
                    }
                }
                
                # Populate survival estimates table
                survtable <- self$results$multipleSurvTable
                survtable$deleteRows()  # Clear existing rows
                
                # Calculate survival at flexible time points (defaults to 1, 3, 5 years)
                time_points <- private$.parseSurvivalTimePoints(self$options$cutp)
                
                for (group_name in names(multicut_results$group_stats)) {
                    stats <- multicut_results$group_stats[[group_name]]
                    if (!is.null(stats) && !is.null(stats$surv_fit)) {
                        for (time_point in time_points) {
                            tryCatch({
                                # Extract survival probability at specific time point
                                surv_summary <- summary(stats$surv_fit, times = time_point)
                                
                                if (length(surv_summary$surv) > 0) {
                                    # Debug the raw values
                                    message(glue::glue(.('Debug survival for {group} at time {time}: raw surv = {surv}, n.risk = {risk}'), 
                                            group = group_name, time = time_point, surv = surv_prob, risk = n_risk))
                                    
                                    # Don't multiply by 100 - the YAML format: pc does this automatically
                                    surv_prob <- surv_summary$surv[1]  # Keep as proportion (0-1)
                                    lower_ci <- surv_summary$lower[1]
                                    upper_ci <- surv_summary$upper[1]
                                    n_at_risk <- surv_summary$n.risk[1]
                                    
                                    survtable$addRow(rowKey = paste(group_name, time_point, sep = "_"), 
                                                   values = list(
                                        risk_group = stats$group,
                                        time_point = time_point,
                                        n_risk = n_at_risk,  # Match YAML column name
                                        survival_prob = round(surv_prob, 3),  # Keep as proportion
                                        surv_lower = round(lower_ci, 3),  # Match YAML column name
                                        surv_upper = round(upper_ci, 3)   # Match YAML column name
                                    ))
                                } else {
                                    # No survival data available at this time point
                                    survtable$addRow(rowKey = paste(group_name, time_point, sep = "_"), 
                                                   values = list(
                                        risk_group = stats$group,
                                        time_point = time_point,
                                        n_risk = 0,  # Match YAML column name
                                        survival_prob = NA,
                                        surv_lower = NA,  # Match YAML column name
                                        surv_upper = NA   # Match YAML column name
                                    ))
                                }
                            }, error = function(e) {
                                message(glue::glue(.('Error calculating survival at time {time} for group {group}: {error}'), 
                                        time = time_point, group = group_name, error = e$message))
                            })
                        }
                    }
                }
            }

            # Multiple cutoffs visualization ----
            ,
            .plotMultipleCutoffs = function(image, ggtheme, theme, ...) {
                if (!self$options$multiple_cutoffs) {
                    return()
                }
                
                # Get the stored multiple cutoffs results
                plotData <- image$state
                if (is.null(plotData) || is.null(plotData$multicut_results)) {
                    # Create fallback visualization
                    plot <- ggplot2::ggplot() + 
                        ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, 
                                                      label = "Multiple Cutoffs Analysis\nRun analysis to see visualization"),
                                          size = 6) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void()
                    print(plot)
                    return(TRUE)
                }
                
                tryCatch({
                    multicut_results <- plotData$multicut_results
                    results <- plotData$results
                    
                    # Get the continuous variable data
                    cont_var <- results$cleanData[[results$name3contexpl]]
                    cutoff_values <- multicut_results$cutoff_values
                    
                    # Create histogram with cutoff lines
                    hist_data <- data.frame(values = cont_var)
                    
                    plot <- ggplot2::ggplot(hist_data, ggplot2::aes(x = values)) +
                        ggplot2::geom_histogram(bins = 30, alpha = 0.7, fill = "lightblue", color = "black") +
                        ggplot2::geom_vline(xintercept = cutoff_values, color = "red", linetype = "dashed", size = 1) +
                        ggplot2::labs(
                            title = paste0("Multiple Cut-offs for ", self$options$contexpl),
                            subtitle = paste0("Method: ", multicut_results$method, 
                                            " | Number of cut-offs: ", length(cutoff_values)),
                            x = self$options$contexpl,
                            y = "Frequency"
                        ) +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                                      plot.subtitle = ggplot2::element_text(hjust = 0.5))
                    
                    # Add cutoff value annotations
                    for (i in seq_along(cutoff_values)) {
                        plot <- plot + ggplot2::annotate("text", 
                                                        x = cutoff_values[i], 
                                                        y = Inf, 
                                                        label = paste0("Cut ", i, ": ", round(cutoff_values[i], 2)),
                                                        vjust = 1.2, 
                                                        color = "red", 
                                                        size = 3,
                                                        angle = 90)
                    }
                    
                    print(plot)
                }, error = function(e) {
                    # Fallback plot in case of error
                    plot <- ggplot2::ggplot() + 
                        ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, 
                                                      label = "Multiple Cutoffs Visualization\nError in plot generation"),
                                          size = 6) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void()
                    print(plot)
                })
                
                TRUE
            }

            # Multiple cutoffs survival plot ----
            ,
            .plotMultipleSurvival = function(image, ggtheme, theme, ...) {
                if (!self$options$multiple_cutoffs || !self$options$sc) {
                    return()
                }
                
                # Get the stored multiple cutoffs results
                plotData <- image$state
                if (is.null(plotData) || is.null(plotData$multicut_results)) {
                    plot <- ggplot2::ggplot() + 
                        ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, 
                                                      label = "Multiple Cutoffs Survival Plot\nRun analysis to see visualization"),
                                          size = 6) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void()
                    print(plot)
                    return(TRUE)
                }
                
                tryCatch({
                    multicut_results <- plotData$multicut_results
                    results <- plotData$results
                    
                    # Create data with risk groups
                    plot_data <- results$cleanData
                    plot_data$risk_groups <- multicut_results$risk_groups
                    
                    mytime <- results$name1time
                    myoutcome <- results$name2outcome
                    
                    # Create survival formula
                    escaped_time <- private$.escapeVariableNames(mytime)
                    escaped_outcome <- private$.escapeVariableNames(myoutcome)
                    formula_str <- paste0('survival::Surv(', escaped_time, ',', escaped_outcome, ') ~ risk_groups')
                    surv_formula <- as.formula(formula_str)
                    
                    # Fit survival model
                    fit <- survival::survfit(surv_formula, data = plot_data)
                    
                    # Create survival plot
                    surv_plot <- survminer::ggsurvplot(
                        fit,
                        data = plot_data,
                        title = paste0("Survival Curves - Multiple Cut-offs for ", self$options$contexpl),
                        subtitle = paste0("Method: ", multicut_results$method, " | Groups: ", length(levels(multicut_results$risk_groups))),
                        xlab = paste0("Time (", self$options$timetypeoutput, ")"),
                        ylab = "Survival Probability",
                        legend.title = "Risk Groups",
                        risk.table = self$options$risktable,
                        conf.int = self$options$ci95,
                        pval = TRUE,
                        pval.coord = c(0.1, 0.1),
                        break.time.by = self$options$byplot,
                        xlim = c(0, self$options$endplot),
                        ylim = c(self$options$ybegin_plot, self$options$yend_plot),
                        palette = "jco",
                        ggtheme = ggplot2::theme_minimal()
                    )
                    
                    print(surv_plot)
                }, error = function(e) {
                    # Fallback plot
                    plot <- ggplot2::ggplot() + 
                        ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, 
                                                      label = "Multiple Cutoffs Survival Plot\nError in plot generation"),
                                          size = 6) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void()
                    print(plot)
                })
                
                TRUE
            }

            # RMST Analysis ----
            ,
            .calculateRMST = function(results, cutoffdata = NULL) {
                if (!self$options$rmst_analysis) {
                    return()
                }
                
                # Use cutoffdata if provided (for cutoff analysis), otherwise use original data
                data_to_use <- if (!is.null(cutoffdata)) cutoffdata else results$cleanData
                
                mytime <- results$name1time
                myoutcome <- results$name2outcome
                
                # For cutoff analysis, use the cutoff groups; otherwise use original explanatory variable
                if (!is.null(cutoffdata) && results$name3contexpl %in% names(cutoffdata)) {
                    mygroup <- results$name3contexpl
                } else {
                    # For non-cutoff analysis, create binary groups based on median
                    median_val <- median(results$cleanData[[results$name3contexpl]], na.rm = TRUE)
                    data_to_use$rmst_groups <- ifelse(
                        results$cleanData[[results$name3contexpl]] <= median_val,
                        "Below Median", "Above Median"
                    )
                    mygroup <- "rmst_groups"
                }
                
                # Get tau (time horizon) from options or default to 75th percentile of observed times
                if (!is.null(self$options$rmst_tau) && self$options$rmst_tau > 0) {
                    tau <- self$options$rmst_tau
                } else {
                    tau <- quantile(data_to_use[[mytime]], 0.75, na.rm = TRUE)
                }
                
                # Calculate RMST for each group
                rmst_results <- list()
                groups <- unique(data_to_use[[mygroup]])
                
                for (group in groups) {
                    group_data <- data_to_use[data_to_use[[mygroup]] == group, ]
                    
                    if (nrow(group_data) > 0) {
                        # Create survival object
                        surv_obj <- survival::Surv(
                            time = group_data[[mytime]], 
                            event = group_data[[myoutcome]]
                        )
                        
                        # Fit Kaplan-Meier
                        km_fit <- survival::survfit(surv_obj ~ 1, data = group_data)
                        
                        # Calculate RMST manually using trapezoidal rule
                        times <- km_fit$time
                        surv_probs <- km_fit$surv
                        
                        # Add time 0 and tau if not present
                        if (!0 %in% times) {
                            times <- c(0, times)
                            surv_probs <- c(1, surv_probs)
                        }
                        
                        # Truncate at tau
                        valid_indices <- times <= tau
                        times <- times[valid_indices]
                        surv_probs <- surv_probs[valid_indices]
                        
                        # Add tau point if needed
                        if (max(times) < tau) {
                            # Interpolate survival at tau
                            surv_at_tau <- approx(km_fit$time, km_fit$surv, xout = tau, 
                                                rule = 2, method = "constant", f = 0)$y
                            times <- c(times, tau)
                            surv_probs <- c(surv_probs, surv_at_tau)
                        }
                        
                        # Calculate RMST using trapezoidal rule
                        if (length(times) > 1) {
                            time_diffs <- diff(times)
                            surv_avg <- (surv_probs[-length(surv_probs)] + surv_probs[-1]) / 2
                            rmst <- sum(time_diffs * surv_avg)

                            # FIX WARNING: Simplified standard error approach
                            # This is an approximate variance estimate, not the Greenwood-based
                            # variance used in survRM2::rmst2(). For rigorous RMST inference,
                            # consider using the survRM2 package directly.
                            # The fallback of 10% of RMST is arbitrary.
                            se_rmst <- sqrt(sum(km_fit$std.err^2, na.rm = TRUE)) * tau / max(km_fit$time, na.rm = TRUE)
                            se_rmst <- ifelse(is.na(se_rmst) || se_rmst == 0, rmst * 0.1, se_rmst)  # 10% fallback is ad-hoc
                            
                            # Calculate confidence intervals
                            ci_lower <- max(0, rmst - 1.96 * se_rmst)
                            ci_upper <- rmst + 1.96 * se_rmst
                            
                            rmst_results[[as.character(group)]] <- list(
                                group = as.character(group),
                                rmst = rmst,
                                se = se_rmst,
                                ci_lower = ci_lower,
                                ci_upper = ci_upper,
                                tau = tau,
                                n = nrow(group_data)
                            )
                        }
                    }
                }
                
                # Populate RMST table
                rmst_table <- self$results$rmstTable
                for (result in rmst_results) {
                    rmst_table$addRow(rowKey = result$group, values = result)
                }
                
                # Create RMST summary
                if (length(rmst_results) > 0) {
                    summary_text <- paste0(
                        "Restricted Mean Survival Time (RMST) Analysis\n",
                        "Time horizon (τ): ", round(tau, 1), " ", self$options$timetypeoutput, "\n\n",
                        "The RMST represents the average time a patient can expect to survive up to the specified time horizon.\n",
                        "This metric is particularly useful when the survival curves do not reach 50% (median undefined).\n\n"
                    )
                    
                    # Add group comparisons if multiple groups
                    if (length(rmst_results) == 2) {
                        group_names <- names(rmst_results)
                        diff_rmst <- rmst_results[[group_names[2]]]$rmst - rmst_results[[group_names[1]]]$rmst
                        summary_text <- paste0(
                            summary_text,
                            "Difference in RMST (", group_names[2], " vs ", group_names[1], "): ",
                            round(diff_rmst, 2), " ", self$options$timetypeoutput, "\n",
                            "Interpretation: Patients in '", group_names[2], "' group have on average ",
                            abs(round(diff_rmst, 2)), " ", 
                            if (diff_rmst > 0) "more" else "fewer",
                            " ", self$options$timetypeoutput, 
                            " of survival up to ", round(tau, 1), " ", self$options$timetypeoutput, "."
                        )
                    }
                    
                    self$results$rmstSummary$setContent(summary_text)
                }
            }

            # Residual Diagnostics ----
            ,
            .calculateResiduals = function(results, cutoffdata = NULL) {
                if (!self$options$residual_diagnostics) {
                    return()
                }
                
                # Use cutoffdata if provided, otherwise use original data
                data_to_use <- if (!is.null(cutoffdata)) cutoffdata else results$cleanData
                
                mytime <- results$name1time
                myoutcome <- results$name2outcome
                
                # For cutoff analysis, use the cutoff groups; otherwise use original explanatory variable
                if (!is.null(cutoffdata) && results$name3contexpl %in% names(cutoffdata)) {
                    myexplanatory <- results$name3contexpl
                } else {
                    myexplanatory <- results$name3contexpl
                }
                
                tryCatch({
                    # Create Cox model formula
                    escaped_time <- private$.escapeVariableNames(mytime)
                    escaped_outcome <- private$.escapeVariableNames(myoutcome)
                    escaped_explanatory <- private$.escapeVariableNames(myexplanatory)
                    formula_str <- paste0("survival::Surv(", escaped_time, ", ", escaped_outcome, ") ~ ", escaped_explanatory)
                    cox_formula <- as.formula(formula_str)
                    
                    # Fit Cox model
                    cox_model <- survival::coxph(cox_formula, data = data_to_use)
                    
                    # Calculate residuals
                    martingale_resid <- residuals(cox_model, type = "martingale")
                    deviance_resid <- residuals(cox_model, type = "deviance")
                    score_resid <- residuals(cox_model, type = "score")
                    schoenfeld_resid <- residuals(cox_model, type = "schoenfeld")
                    
                    # Handle missing values
                    n_obs <- length(martingale_resid)
                    if (length(schoenfeld_resid) < n_obs) {
                        schoenfeld_resid <- c(schoenfeld_resid, rep(NA, n_obs - length(schoenfeld_resid)))
                    }
                    
                    # Populate residuals table
                    residuals_table <- self$results$residualsTable
                    for (i in 1:min(100, n_obs)) {  # Limit to first 100 observations for display
                        residuals_table$addRow(rowKey = i, values = list(
                            observation = i,
                            martingale = round(martingale_resid[i], 4),
                            deviance = round(deviance_resid[i], 4),
                            score = if (is.matrix(score_resid)) round(score_resid[i, 1], 4) else round(score_resid[i], 4),
                            schoenfeld = round(schoenfeld_resid[i], 4)
                        ))
                    }
                    
                    # Store residuals for plotting
                    private$residuals_data <- list(
                        martingale = martingale_resid,
                        deviance = deviance_resid,
                        fitted = cox_model$linear.predictors,
                        data = data_to_use
                    )
                    
                }, error = function(e) {
                    # If residual calculation fails, show error message
                    residuals_table <- self$results$residualsTable
                    residuals_table$addRow(rowKey = 1, values = list(
                        observation = 1,
                        martingale = NA,
                        deviance = NA,
                        score = NA,
                        schoenfeld = NA
                    ))
                    residuals_table$setNote("error", paste("Residual calculation failed:", e$message))
                })
            }

            # Stratified Cox Regression ----
            ,
            .stratifiedCox = function(results, cutoffdata = NULL) {
                if (!self$options$stratified_cox || is.null(self$options$strata_variable)) {
                    return()
                }
                
                # This would require additional implementation
                # For now, add note that this feature is available in the main survival function
                self$results$coxSummary$setContent(c(
                    self$results$coxSummary$content,
                    "\nNote: Stratified Cox regression is available in the main Survival Analysis function."
                ))
            }

            # Log-Log Plot Function ----
            ,
            .plot7 = function(image, ggtheme, theme, ...) {
                if (!self$options$loglog || !self$options$findcut) {
                    return()
                }
                
                # Get the plot data from previous analysis
                plotData <- image$state
                if (is.null(plotData)) {
                    return()
                }
                
                res.cat <- plotData$cutoffdata
                results <- plotData$results
                
                if (is.null(res.cat) || is.null(results)) {
                    return()
                }
                
                mytime <- results$name1time
                myoutcome <- results$name2outcome
                mycontexpl <- results$name3contexpl
                
                # Create formula with escaped variable names
                escaped_time <- private$.escapeVariableNames(mytime)
                escaped_outcome <- private$.escapeVariableNames(myoutcome)
                escaped_contexpl <- private$.escapeVariableNames(mycontexpl)
                
                formula_str <- paste0('survival::Surv(', escaped_time, ',', escaped_outcome, ') ~ ', escaped_contexpl)
                myformula <- as.formula(formula_str)
                
                # Fit survival model
                fit <- survival::survfit(myformula, data = res.cat)
                
                tryCatch({
                    # Create log-log plot using survminer
                    loglog_plot <- survminer::ggsurvplot(
                        fit,
                        data = res.cat,
                        fun = "cloglog",
                        xlab = paste0("log(Time in ", self$options$timetypeoutput, ")"),
                        ylab = "log(-log(Survival))",
                        title = paste0("Log-Log Plot for ", self$options$contexpl),
                        legend.title = self$options$contexpl,
                        risk.table = FALSE,
                        conf.int = FALSE
                    )
                    
                    print(loglog_plot)
                }, error = function(e) {
                    # Fallback: create simple log-log plot with ggplot2
                    surv_data <- data.frame(
                        time = fit$time,
                        surv = fit$surv,
                        strata = rep(names(fit$strata), fit$strata)
                    )
                    
                    # Remove zero survival values for log transformation
                    surv_data <- surv_data[surv_data$surv > 0 & surv_data$time > 0, ]
                    
                    if (nrow(surv_data) > 0) {
                        surv_data$log_time <- log(surv_data$time)
                        surv_data$cloglog <- log(-log(surv_data$surv))
                        
                        loglog_plot <- ggplot2::ggplot(surv_data, ggplot2::aes(x = log_time, y = cloglog, color = strata)) +
                            ggplot2::geom_line() +
                            ggplot2::labs(
                                x = paste0("log(Time in ", self$options$timetypeoutput, ")"),
                                y = "log(-log(Survival))",
                                title = paste0("Log-Log Plot for ", self$options$contexpl),
                                color = self$options$contexpl
                            ) +
                            ggplot2::theme_minimal()
                        
                        print(loglog_plot)
                    }
                })
                
                TRUE
            }

            # Residuals Plot Function ----
            ,
            .plot9 = function(image, ggtheme, theme, ...) {
                if (!self$options$residual_diagnostics || is.null(private$residuals_data)) {
                    return()
                }
                
                tryCatch({
                    residuals_data <- private$residuals_data
                    
                    # Create a 2x2 plot layout
                    plot_data <- data.frame(
                        fitted = residuals_data$fitted,
                        martingale = residuals_data$martingale,
                        deviance = residuals_data$deviance
                    )
                    
                    # Martingale residuals vs fitted values
                    p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = fitted, y = martingale)) +
                        ggplot2::geom_point(alpha = 0.6) +
                        ggplot2::geom_smooth(method = "loess", se = FALSE, color = "red") +
                        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
                        ggplot2::labs(
                            x = "Linear Predictors",
                            y = "Martingale Residuals",
                            title = "Martingale Residuals vs Fitted"
                        ) +
                        ggplot2::theme_minimal()
                    
                    # Deviance residuals vs fitted values
                    p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = fitted, y = deviance)) +
                        ggplot2::geom_point(alpha = 0.6) +
                        ggplot2::geom_smooth(method = "loess", se = FALSE, color = "red") +
                        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
                        ggplot2::labs(
                            x = "Linear Predictors",
                            y = "Deviance Residuals",
                            title = "Deviance Residuals vs Fitted"
                        ) +
                        ggplot2::theme_minimal()
                    
                    # QQ plot of deviance residuals
                    p3 <- ggplot2::ggplot(plot_data, ggplot2::aes(sample = deviance)) +
                        ggplot2::stat_qq() +
                        ggplot2::stat_qq_line() +
                        ggplot2::labs(
                            x = "Theoretical Quantiles",
                            y = "Sample Quantiles",
                            title = "Q-Q Plot of Deviance Residuals"
                        ) +
                        ggplot2::theme_minimal()
                    
                    # Histogram of deviance residuals
                    p4 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = deviance)) +
                        ggplot2::geom_histogram(bins = 30, alpha = 0.7, fill = "skyblue") +
                        ggplot2::geom_density(alpha = 0.3, fill = "red") +
                        ggplot2::labs(
                            x = "Deviance Residuals",
                            y = "Frequency",
                            title = "Distribution of Deviance Residuals"
                        ) +
                        ggplot2::theme_minimal()
                    
                    # Combine plots using patchwork if available, otherwise show first plot
                    if (requireNamespace("patchwork", quietly = TRUE)) {
                        combined_plot <- (p1 + p2) / (p3 + p4)
                        print(combined_plot)
                    } else {
                        print(p1)
                    }
                    
                }, error = function(e) {
                    # Fallback plot
                    fallback_plot <- ggplot2::ggplot() +
                        ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, 
                                                      label = "Residuals plot unavailable"),
                                          size = 6) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void()
                    print(fallback_plot)
                })
                
                TRUE
            }

            # Educational Explanations ----
            ,
            .addExplanations = function() {
                # Cox Regression Explanation
                private$.setExplanationContent("coxRegressionExplanation", '
                <div class="explanation-box" style="background-color: #f0f8ff; padding: 15px; border-radius: 8px; margin: 10px 0;">
                    <h3 style="color: #2c5282; margin-top: 0;">📊 Understanding Cox Regression for Continuous Variables</h3>
                    
                    <div style="background-color: white; padding: 12px; border-radius: 5px; margin: 10px 0;">
                        <h4 style="color: #2d3748; margin-top: 0;">What is Cox Regression with Continuous Variables?</h4>
                        <p style="margin: 8px 0;">Cox regression with continuous variables analyzes how <strong>each unit increase</strong> in a continuous predictor (e.g., age, biomarker level) affects survival risk.</p>
                        
                        <div style="background-color: #e6f7ff; padding: 10px; border-radius: 5px; margin: 10px 0;">
                            <strong>💡 Key Concept:</strong> Unlike categorical variables, continuous variables provide a <strong>dose-response relationship</strong> - showing how risk changes smoothly across the entire range of values.
                        </div>
                    </div>
                    
                    <div style="background-color: #fef5e7; padding: 12px; border-radius: 5px; margin: 10px 0;">
                        <h4 style="color: #d68910; margin-top: 0;">🎯 Interpreting Hazard Ratios (HR)</h4>
                        <table style="width: 100%; border-collapse: collapse; margin: 10px 0;">
                            <tr style="background-color: #fff3cd;">
                                <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">HR Value</th>
                                <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Meaning</th>
                                <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Clinical Example</th>
                            </tr>
                            <tr>
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong>HR = 1.0</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">No effect</td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Variable does not affect survival</td>
                            </tr>
                            <tr style="background-color: #fffbf0;">
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong>HR > 1.0</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Increased risk</td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">HR = 1.05: 5% higher risk per unit</td>
                            </tr>
                            <tr>
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong>HR < 1.0</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Decreased risk (protective)</td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">HR = 0.95: 5% lower risk per unit</td>
                            </tr>
                        </table>
                    </div>
                    
                    <div style="background-color: #e8f5e9; padding: 12px; border-radius: 5px; margin: 10px 0;">
                        <h4 style="color: #2e7d32; margin-top: 0;">📈 Clinical Examples</h4>
                        
                        <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                            <strong>Example 1: Age and Cancer Survival</strong>
                            <p style="margin: 5px 0;">Age HR = 1.03 (95% CI: 1.01-1.05, p=0.001)</p>
                            <ul style="margin: 5px 0; padding-left: 20px;">
                                <li><strong>Interpretation:</strong> Each additional year of age increases death risk by 3%</li>
                                <li><strong>10-year difference:</strong> 1.03^10 = 34% higher risk for 70 vs 60 years old</li>
                                <li><strong>Significance:</strong> p<0.05 means this effect is statistically significant</li>
                            </ul>
                        </div>
                        
                        <div style="background-color: #f3e5f5; padding: 10px; border-radius: 5px; margin: 10px 0;">
                            <strong>Example 2: Biomarker Level</strong>
                            <p style="margin: 5px 0;">Protein X HR = 0.98 (95% CI: 0.96-0.99, p=0.02)</p>
                            <ul style="margin: 5px 0; padding-left: 20px;">
                                <li><strong>Protective factor:</strong> Higher protein X levels reduce death risk by 2% per unit</li>
                                <li><strong>Range effect:</strong> 20-point increase → 0.98^20 = 33% risk reduction</li>
                            </ul>
                        </div>
                    </div>
                    
                    <div style="background-color: #fff3e0; padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #ff9800;">
                        <strong>⚠️ Important Assumptions:</strong>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li><strong>Linear relationship:</strong> Effect is constant across all values (may not always be true)</li>
                            <li><strong>Proportional hazards:</strong> Relative effect stays constant over time</li>
                            <li><strong>Consider cut-offs:</strong> If relationship is non-linear, consider categorizing the variable</li>
                        </ul>
                    </div>
                </div>
                ')
                
                # Cut-off Point Analysis Explanation
                private$.setExplanationContent("cutoffAnalysisExplanation", '
                <div class="explanation-box" style="background-color: #f0f8ff; padding: 15px; border-radius: 8px; margin: 10px 0;">
                    <h3 style="color: #2c5282; margin-top: 0;">✂️ Understanding Cut-off Point Analysis</h3>
                    
                    <div style="background-color: white; padding: 12px; border-radius: 5px; margin: 10px 0;">
                        <h4 style="color: #2d3748; margin-top: 0;">What is Cut-off Point Analysis?</h4>
                        <p style="margin: 8px 0;">Cut-off analysis transforms a <strong>continuous variable into binary groups</strong> (high vs low) by finding the optimal threshold that best separates patients with different survival outcomes.</p>
                        
                        <div style="background-color: #e6f7ff; padding: 10px; border-radius: 5px; margin: 10px 0;">
                            <strong>🎯 Goal:</strong> Find the value that creates two groups with the <strong>maximum survival difference</strong>
                        </div>
                    </div>
                    
                    <div style="background-color: #fef5e7; padding: 12px; border-radius: 5px; margin: 10px 0;">
                        <h4 style="color: #d68910; margin-top: 0;">📊 How It Works</h4>
                        
                        <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                            <strong>1. Maximally Selected Rank Statistics Method:</strong>
                            <ul style="margin: 5px 0; padding-left: 20px;">
                                <li>Tests every possible cut-off value</li>
                                <li>For each cut-off, performs survival comparison</li>
                                <li>Selects cut-off with smallest p-value (biggest difference)</li>
                                <li>Adjusts p-value for multiple testing</li>
                            </ul>
                        </div>
                        
                        <table style="width: 100%; border-collapse: collapse; margin: 10px 0;">
                            <tr style="background-color: #fff3cd;">
                                <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Step</th>
                                <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Process</th>
                                <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Output</th>
                            </tr>
                            <tr>
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong>1. Testing</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Try multiple cut-offs</td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Range of p-values</td>
                            </tr>
                            <tr style="background-color: #fffbf0;">
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong>2. Selection</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Find minimum p-value</td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Optimal cut-off value</td>
                            </tr>
                            <tr>
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong>3. Validation</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Multiple testing correction</td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Adjusted p-value</td>
                            </tr>
                        </table>
                    </div>
                    
                    <div style="background-color: #e8f5e9; padding: 12px; border-radius: 5px; margin: 10px 0;">
                        <h4 style="color: #2e7d32; margin-top: 0;">🏥 Clinical Benefits</h4>
                        
                        <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                            <strong>✅ Advantages:</strong>
                            <ul style="margin: 5px 0; padding-left: 20px;">
                                <li><strong>Simple decision-making:</strong> "High" vs "Low" is easier than continuous values</li>
                                <li><strong>Risk stratification:</strong> Clear groups for treatment planning</li>
                                <li><strong>Communication:</strong> Easy to explain to patients and colleagues</li>
                                <li><strong>Guidelines:</strong> Can establish clinical thresholds</li>
                            </ul>
                        </div>
                        
                        <div style="background-color: #f3e5f5; padding: 10px; border-radius: 5px; margin: 10px 0;">
                            <strong>💡 Clinical Example:</strong>
                            <p style="margin: 5px 0;">Biomarker X optimal cut-off = 25.3 ng/mL</p>
                            <ul style="margin: 5px 0; padding-left: 20px;">
                                <li>High group (≥25.3): 40% 5-year survival</li>
                                <li>Low group (<25.3): 75% 5-year survival</li>
                                <li>Clinical use: "Patients with Biomarker X ≥25.3 need intensive monitoring"</li>
                            </ul>
                        </div>
                    </div>
                    
                    <div style="background-color: #ffebee; padding: 12px; border-radius: 5px; margin: 10px 0;">
                        <h4 style="color: #c62828; margin-top: 0;">⚠️ Important Limitations</h4>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li><strong>Data-dependent:</strong> Optimal cut-off may vary between studies</li>
                            <li><strong>Information loss:</strong> Converting continuous to binary loses precision</li>
                            <li><strong>Validation needed:</strong> Cut-off should be confirmed in independent datasets</li>
                            <li><strong>Population-specific:</strong> May not apply to different populations</li>
                        </ul>
                    </div>
                    
                    <div style="background-color: #fff3e0; padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #ff9800;">
                        <strong>💡 Best Practices:</strong>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li>Validate cut-off in independent cohort when possible</li>
                            <li>Consider clinical relevance, not just statistical significance</li>
                            <li>Report both continuous and cut-off analyses</li>
                            <li>Ensure adequate sample size in both groups</li>
                        </ul>
                    </div>
                </div>
                ')
                
                # Multiple Cutoffs Explanation
                private$.setExplanationContent("multipleCutoffsExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: #fff3cd; border-left: 4px solid #ffc107;">
                    <h4 style="margin-top: 0; color: #2c3e50;">Understanding Multiple Cut-offs Analysis</h4>
                    <p><strong>Risk Stratification:</strong> Creates multiple risk groups (low, intermediate, high) from continuous variable.</p>
                    <ul>
                        <li><strong>Multiple Cut-offs:</strong> Identifies 2-4 optimal cut-points for risk stratification</li>
                        <li><strong>Risk Groups:</strong> Creates ordered categories with distinct survival profiles</li>
                        <li><strong>Method Options:</strong> Quantile-based, tree-based, or minimum p-value approaches</li>
                        <li><strong>Group Validation:</strong> Ensures adequate sample size in each risk group</li>
                    </ul>
                    <p><em>Advantage:</em> Captures non-linear relationships better than single cut-off approaches.</p>
                </div>
                ')
                
                # Person-Time Analysis Explanation
                private$.setExplanationContent("personTimeExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: #d4edda; border-left: 4px solid #28a745;">
                    <h4 style="margin-top: 0; color: #2c3e50;">Understanding Person-Time Analysis</h4>
                    <p><strong>Person-Time:</strong> Accounts for both number of participants and their observation duration.</p>
                    <ul>
                        <li><strong>Incidence Rate:</strong> Events per person-time unit (e.g., per 100 person-years)</li>
                        <li><strong>Time Intervals:</strong> Analyzes rates across different follow-up periods</li>
                        <li><strong>Rate Comparison:</strong> Compares incidence rates between risk groups</li>
                        <li><strong>Confidence Intervals:</strong> Provide precision estimates for rates</li>
                    </ul>
                    <p><em>Clinical use:</em> Essential for comparing event rates when follow-up times vary between groups.</p>
                </div>
                ')
                
                # RMST Analysis Explanation
                private$.setExplanationContent("rmstExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: #e2e3e5; border-left: 4px solid #6c757d;">
                    <h4 style="margin-top: 0; color: #2c3e50;">Understanding Restricted Mean Survival Time (RMST)</h4>
                    <p><strong>RMST:</strong> Average survival time up to a specified time horizon (τ).</p>
                    <ul>
                        <li><strong>Time-Limited Analysis:</strong> Mean survival within a defined observation period</li>
                        <li><strong>Group Comparison:</strong> Difference in RMST between risk groups</li>
                        <li><strong>Robust Measure:</strong> Less sensitive to tail behavior than median survival</li>
                        <li><strong>Clinical Interpretation:</strong> Direct measure of expected survival time</li>
                    </ul>
                    <p><em>When to use:</em> Particularly valuable when median survival cannot be estimated or for time-limited analyses.</p>
                </div>
                ')
                
                # Residual Diagnostics Explanation
                private$.setExplanationContent("residualDiagnosticsExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: #ffeaa7; border-left: 4px solid #fdcb6e;">
                    <h4 style="margin-top: 0; color: #2c3e50;">Understanding Cox Model Residual Diagnostics</h4>
                    <p><strong>Model Residuals:</strong> Assess Cox model fit and identify potential issues.</p>
                    <ul>
                        <li><strong>Martingale Residuals:</strong> Detect functional form problems (should scatter around 0)</li>
                        <li><strong>Deviance Residuals:</strong> Standardized residuals for outlier detection</li>
                        <li><strong>Score Residuals:</strong> Assess influence of observations on coefficients</li>
                        <li><strong>Schoenfeld Residuals:</strong> Test proportional hazards assumption</li>
                    </ul>
                    <p><em>Clinical interpretation:</em> Large residuals may indicate patients with unusual survival patterns requiring further investigation.</p>
                </div>
                ')
                
                # Survival Plots Explanation
                private$.setExplanationContent("survivalPlotsExplanation", '
                <div class="explanation-box" style="background-color: #f0f8ff; padding: 15px; border-radius: 8px; margin: 10px 0;">
                    <h3 style="color: #2c5282; margin-top: 0;">📊 Understanding Survival Curves for Continuous Variables</h3>
                    
                    <div style="background-color: white; padding: 12px; border-radius: 5px; margin: 10px 0;">
                        <h4 style="color: #2d3748; margin-top: 0;">📈 Survival Curves with Cut-offs</h4>
                        <p style="margin: 8px 0;">When analyzing continuous variables, survival plots show <strong>separate curves for high vs low groups</strong> based on the optimal cut-off value.</p>
                        
                        <div style="background-color: #e6f7ff; padding: 10px; border-radius: 5px; margin: 10px 0;">
                            <strong>📖 How to Read the Plot:</strong>
                            <ul style="margin: 5px 0; padding-left: 20px;">
                                <li><strong>Two curves:</strong> High group (above cut-off) vs Low group (below cut-off)</li>
                                <li><strong>Separation:</strong> Wider gap between curves = stronger prognostic effect</li>
                                <li><strong>P-value:</strong> Tests whether group survival differs significantly</li>
                                <li><strong>Risk tables:</strong> Show number of patients at each time point</li>
                            </ul>
                        </div>
                    </div>
                    
                    <div style="background-color: #fef5e7; padding: 12px; border-radius: 5px; margin: 10px 0;">
                        <h4 style="color: #d68910; margin-top: 0;">🔍 Curve Interpretation Patterns</h4>
                        <table style="width: 100%; border-collapse: collapse; margin: 10px 0;">
                            <tr style="background-color: #fff3cd;">
                                <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Pattern</th>
                                <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Clinical Meaning</th>
                            </tr>
                            <tr>
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong>📏 Wide separation early</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Strong early prognostic effect</td>
                            </tr>
                            <tr style="background-color: #fffbf0;">
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong>📈 Curves converge later</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Effect diminishes over time</td>
                            </tr>
                            <tr>
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong>↔ Parallel curves</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Consistent proportional hazards</td>
                            </tr>
                            <tr style="background-color: #fffbf0;">
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong>✖ Crossing curves</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Time-dependent effects (complex interpretation)</td>
                            </tr>
                        </table>
                    </div>
                    
                    <div style="background-color: #e8f5e9; padding: 12px; border-radius: 5px; margin: 10px 0;">
                        <h4 style="color: #2e7d32; margin-top: 0;">💡 Clinical Application Tips</h4>
                        
                        <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                            <strong>🎯 Risk Stratification:</strong>
                            <ul style="margin: 5px 0; padding-left: 20px;">
                                <li>High group: May need more intensive monitoring/treatment</li>
                                <li>Low group: Could be suitable for less intensive approaches</li>
                                <li>Consider clinical context, not just statistical significance</li>
                                <li>Consider clinical context, not just statistical significance</li>
                            </ul>
                        </div>
                        
                        <div style="background-color: #f3e5f5; padding: 10px; border-radius: 5px; margin: 10px 0;">
                            <strong>📊 Biomarker Validation:</strong>
                            <p style="margin: 5px 0;">Strong separation with p<0.001 suggests the biomarker could be clinically useful for:</p>
                            <ul style="margin: 5px 0; padding-left: 20px;">
                                <li>Patient counseling about prognosis</li>
                                <li>Treatment selection decisions</li>
                                <li>Clinical trial stratification</li>
                            </ul>
                        </div>
                    </div>
                    
                    <div style="background-color: #fff3e0; padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #ff9800;">
                        <strong>⚠️ Important Considerations:</strong>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li><strong>Cut-off validation:</strong> Confirm cut-off value in independent dataset</li>
                            <li><strong>Clinical relevance:</strong> Ensure the difference is clinically meaningful</li>
                            <li><strong>Sample size:</strong> Both groups should have adequate numbers for reliable estimates</li>
                            <li><strong>Follow-up:</strong> Longer follow-up provides more reliable late-time estimates</li>
                        </ul>
                    </div>
                </div>
                ')
            },
            
            # === EXTRACTED HELPER METHODS FOR MAINTAINABILITY ===
            
            # Helper function to safely set explanation content
            .setExplanationContent = function(result_name, content) {
                tryCatch({
                    self$results[[result_name]]$setContent(content)
                }, error = function(e) {
                    # Silently ignore if result does not exist
                })
            },
            
            # Input validation helper - prevents common data issues
            .validateInputs = function(data, time_var, outcome_var, contexpl_var) {
                if (is.null(data) || nrow(data) == 0) {
                    stop(.('Data contains no (complete) rows'))
                }
                
                if (any(is.na(data[[time_var]]))) {
                    stop(.('Time variable contains missing values. Please remove or impute missing data before analysis.'))
                }
                
                if (any(data[[time_var]] <= 0, na.rm = TRUE)) {
                    stop(.('Time variable contains zero or negative values. Survival times must be positive.'))
                }
                
                if (any(is.na(data[[outcome_var]]))) {
                    stop(.('Outcome variable contains missing values. Please remove or impute missing data before analysis.'))
                }
                
                if (!is.null(contexpl_var) && any(is.na(data[[contexpl_var]]))) {
                    warning(.('Continuous explanatory variable contains missing values. These observations will be excluded from analysis.'))
                }
                
                # Check for sufficient sample size
                if (nrow(data) < 20) {
                    warning(.('Small sample size (n < 20). Results may be unreliable.'))
                }
                
                return(TRUE)
            },
            
            # Memory monitoring for large datasets
            .checkMemoryUsage = function(data, warn_threshold = 50000) {
                n_rows <- nrow(data)
                
                if (n_rows > warn_threshold) {
                    memory_usage <- format(object.size(data), units = "MB")
                    message(glue::glue(.('Processing large dataset: {rows} rows, ~{memory} memory usage'), 
                            rows = n_rows, memory = format(object.size(self$data), units = "auto")))                 
                    if (n_rows > 100000) {
                        message(.('Consider using data sampling or chunking for very large datasets'))
                    }
                }
                
                return(n_rows)
            },
            
            # Configurable checkpoint frequency for responsiveness
            .performCheckpoint = function(iteration, frequency = 5) {
                if (iteration %% frequency == 0) {
                    private$.checkpoint(FALSE)
                }
            },
            
            # Enhanced survival time points parsing with flexible options
            .parseSurvivalTimePoints = function(cutp_string, default_points = c(12, 36, 60)) {
                if (is.null(cutp_string) || cutp_string == "" || cutp_string == "default") {
                    return(default_points)
                }
                
                # Parse comma-separated values
                time_points <- tryCatch({
                    as.numeric(unlist(strsplit(cutp_string, "[,\\\\s]+")))
                }, error = function(e) {
                    warning(.('Could not parse survival time points: {error}. Using default values (1, 3, 5 years)'), 
                            error = e$message)
                    return(default_points)
                })
                
                # Remove invalid values
                time_points <- time_points[!is.na(time_points) & time_points > 0]
                
                if (length(time_points) == 0) {
                    warning(.('No valid time points specified. Using default values (1, 3, 5 years)'))
                    return(default_points)
                }
                
                # Sort and return unique values
                return(sort(unique(time_points)))
            },
            
            # Flexible interval calculation with configurable multiplier
            .calculateTimeIntervals = function(time_var, max_multiplier = 1.1) {
                max_time <- max(time_var, na.rm = TRUE)
                return(max_time * max_multiplier)
            }
        )
    )
}
