#' @title Comprehensive Survival Analysis
#' 
#' @description
#' Performs comprehensive univariate and stratified survival analysis comparing survival between groups.
#' This analysis calculates person-time follow-up for each group and uses this to derive accurate
#' survival estimates and incidence rates that account for varying follow-up durations across groups.
#' The Cox proportional hazards model incorporates person-time by modeling the hazard function,
#' which represents the instantaneous event rate per unit of person-time.
#' 
#' **Key Features:**
#' - Kaplan-Meier survival curves with multiple plot types
#' - Cox proportional hazards regression (univariate and stratified)
#' - Median survival time with confidence intervals
#' - Restricted Mean Survival Time (RMST) analysis
#' - Person-time analysis with incidence rates
#' - Competing risks and cause-specific survival
#' - Landmark analysis for conditional survival
#' - Proportional hazards assumption testing
#' - Model residual diagnostics
#' - Pairwise group comparisons with multiple testing correction
#' 
#' **Statistical Methods:**
#' - Kaplan-Meier estimator for survival probabilities
#' - Log-rank test for group comparisons
#' - Cox proportional hazards model for risk assessment
#' - Competing risks analysis using cumulative incidence functions
#' - RMST for robust survival comparisons
#' 
#' **Visualization Options:**
#' - Standard survival curves
#' - Cumulative events and hazard plots
#' - KMunicate-style plots for publication
#' - Log-log plots for proportional hazards assessment
#' - Residual diagnostic plots
#' 
#' @param data The data as a data frame
#' @param elapsedtime Numeric variable containing survival time (time to event or censoring)
#' @param tint Logical. Use dates to calculate survival time from diagnosis and follow-up dates
#' @param dxdate Date variable for diagnosis date (when tint = TRUE)
#' @param fudate Date variable for follow-up/event date (when tint = TRUE)
#' @param explanatory Factor variable for group comparisons (e.g., treatment groups, risk categories)
#' @param outcome Event indicator variable (binary: 0=censored, 1=event) or factor for multi-state outcomes
#' @param outcomeLevel Event level when using factor outcome variable
#' @param dod Dead of disease level (for competing risks analysis)
#' @param dooc Dead of other causes level (for competing risks analysis)
#' @param awd Alive with disease level (for competing risks analysis)
#' @param awod Alive without disease level (for competing risks analysis)
#' @param analysistype Type of survival analysis: "overall", "cause", or "compete"
#' @param cutp Time points for survival probability estimation (comma-separated)
#' @param timetypedata Date format in data: "ymd", "dmy", "mdy", etc.
#' @param timetypeoutput Time unit for output: "days", "weeks", "months", "years"
#' @param uselandmark Logical. Perform landmark analysis
#' @param landmark Landmark time point for conditional survival analysis
#' @param pw Logical. Perform pairwise group comparisons
#' @param padjustmethod Multiple testing correction method for pairwise comparisons
#' @param ph_cox Logical. Test proportional hazards assumption
#' @param stratified_cox Logical. Use stratified Cox regression
#' @param strata_variable Variable for Cox model stratification
#' @param rmst_analysis Logical. Calculate Restricted Mean Survival Time
#' @param rmst_tau Time horizon for RMST calculation (uses 75th percentile if NULL)
#' @param residual_diagnostics Logical. Calculate and display model residuals
#' @param export_survival_data Logical. Export survival estimates for external analysis
#' @param person_time Logical. Calculate person-time metrics and incidence rates
#' @param time_intervals Time intervals for stratified person-time analysis
#' @param rate_multiplier Multiplier for incidence rates (e.g., 100 for per 100 person-years)
#' @param sc Logical. Display survival curve plot
#' @param ce Logical. Display cumulative events plot
#' @param ch Logical. Display cumulative hazard plot
#' @param kmunicate Logical. Display KMunicate-style plot
#' @param loglog Logical. Display log-log plot for proportional hazards assessment
#' @param endplot Maximum time for plot x-axis
#' @param ybegin_plot Minimum value for plot y-axis
#' @param yend_plot Maximum value for plot y-axis
#' @param byplot Time interval for plot axis breaks
#' @param multievent Logical. Use multiple event levels for competing risks
#' @param ci95 Logical. Display 95% confidence intervals on plots
#' @param risktable Logical. Display risk table below survival curves
#' @param censored Logical. Display censoring marks on survival curves
#' @param pplot Logical. Display p-value on plots
#' @param medianline Type of median survival line: "none", "h", "v", "hv"
#' 
#' @return A comprehensive results object containing survival analysis outputs
#' 
#' @details
#' **Analysis Types:**
#' - **Overall Survival**: Time from study entry to death from any cause
#' - **Cause-Specific Survival**: Time to death from specific cause (censoring other deaths)
#' - **Competing Risks**: Accounts for multiple types of events that prevent observation of the primary outcome
#' 
#' **Person-Time Analysis:**
#' Calculates incidence rates accounting for varying follow-up times. Particularly useful for:
#' - Studies with differential loss to follow-up
#' - Comparison of event rates across populations
#' - Assessment of time-varying risk
#' 
#' **Restricted Mean Survival Time (RMST):**
#' Alternative to median survival when survival curves don't reach 50% or for comparing
#' survival over a specific time horizon. Represents the area under the survival curve
#' up to a specified time point.
#' 
#' **Model Diagnostics:**
#' - Proportional hazards assumption testing using Schoenfeld residuals
#' - Martingale and deviance residuals for outlier detection
#' - Log-log plots for visual assessment of proportional hazards
#' 
#' @examples
#' \donttest{
#' # Basic survival analysis
#' data("histopathologySurvival", package = "ClinicoPathJamoviModule")
#' 
#' # Standard survival analysis with median and survival probabilities
#' survival_result <- survival(
#'   data = histopathologySurvival,
#'   elapsedtime = "OverallSurvival_indays",
#'   outcome = "Outcome",
#'   outcomeLevel = "Dead",
#'   explanatory = "Grade",
#'   timetypeoutput = "months",
#'   cutp = "12, 36, 60",
#'   sc = TRUE,
#'   pw = TRUE
#' )
#' 
#' # Survival analysis with person-time metrics
#' survival_with_pt <- survival(
#'   data = histopathologySurvival,
#'   elapsedtime = "OverallSurvival_indays", 
#'   outcome = "Outcome",
#'   outcomeLevel = "Dead",
#'   explanatory = "Stage",
#'   person_time = TRUE,
#'   time_intervals = "365, 1095, 1825",
#'   rate_multiplier = 1000
#' )
#' 
#' # RMST analysis for non-proportional hazards
#' rmst_analysis <- survival(
#'   data = histopathologySurvival,
#'   elapsedtime = "OverallSurvival_indays",
#'   outcome = "Outcome", 
#'   outcomeLevel = "Dead",
#'   explanatory = "Treatment",
#'   rmst_analysis = TRUE,
#'   rmst_tau = 1095  # 3 years
#' )
#' 
#' # Competing risks analysis
#' competing_risks <- survival(
#'   data = cancer_data,
#'   elapsedtime = "survival_days",
#'   outcome = "death_cause",
#'   multievent = TRUE,
#'   dod = "Cancer",
#'   dooc = "Other",
#'   awd = "Alive_Disease",
#'   awod = "Alive_Free",
#'   analysistype = "compete",
#'   explanatory = "risk_group"
#' )
#' 
#' # Landmark analysis for conditional survival
#' landmark_survival <- survival(
#'   data = histopathologySurvival,
#'   elapsedtime = "OverallSurvival_indays",
#'   outcome = "Outcome",
#'   outcomeLevel = "Dead", 
#'   explanatory = "Grade",
#'   uselandmark = TRUE,
#'   landmark = 365  # 1-year conditional survival
#' )
#' 
#' # Date-based survival calculation
#' date_survival <- survival(
#'   data = clinical_data,
#'   tint = TRUE,
#'   dxdate = "diagnosis_date",
#'   fudate = "last_contact_date",
#'   timetypedata = "ymd",
#'   timetypeoutput = "months",
#'   outcome = "vital_status",
#'   outcomeLevel = "Dead",
#'   explanatory = "treatment_arm"
#' )
#' }
#' 
#' @references
#' Klein JP, Moeschberger ML (2003). Survival Analysis: Techniques for Censored and Truncated Data. Springer.
#' 
#' Therneau TM, Grambsch PM (2000). Modeling Survival Data: Extending the Cox Model. Springer.
#' 
#' Royston P, Parmar MK (2013). Restricted mean survival time: an alternative to the hazard ratio for the design and analysis of randomized trials with a time-to-event outcome. BMC Medical Research Methodology 13:152.
#' 
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr
#' @import survival
#' @import survminer
#' @import finalfit
#' @importFrom dplyr mutate filter
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme
#'

# Helper function to escape variable names with special characters for formulas
.escapeVariableNames <- function(var_names) {
    # Check if variable names contain special characters that need escaping
    need_escaping <- grepl("[^a-zA-Z0-9._]", var_names)
    var_names[need_escaping] <- paste0("`", var_names[need_escaping], "`")
    return(var_names)
}

# Helper function to restore original variable names in finalfit output tables
.restoreOriginalNamesInSurvivalTable <- function(table_data, name_mapping) {
    if (is.null(table_data) || nrow(table_data) == 0 || is.null(name_mapping)) {
        return(table_data)
    }
    
    # Restore names in the first column (which typically contains variable names)
    if (ncol(table_data) > 0) {
        first_col <- table_data[[1]]
        
        # For each cell in the first column, try to restore original name
        for (j in seq_along(first_col)) {
            cell_value <- first_col[j]
            if (!is.na(cell_value) && cell_value %in% names(name_mapping)) {
                table_data[j, 1] <- name_mapping[cell_value]
            }
        }
    }
    
    return(table_data)
}

# Helper function to get display name for variables (original name or fallback)
.getDisplayName <- function(var_name, name_mapping) {
    if (is.null(name_mapping) || is.null(var_name) || length(var_name) == 0) {
        return(as.character(var_name))
    }
    return(name_mapping[var_name] %||% as.character(var_name))
}

survivalClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "survivalClass",
        inherit = survivalBase,
        private = list(
            .parametric_model = NULL,
            .parametric_model_name = NULL,
            .parametric_results = NULL,
            .init = function() {
                # Hide all outputs first - this ensures they're hidden even if we return early
                # Hide all heading/explanation outputs
                self$results$medianSurvivalHeading$setVisible(FALSE)
                self$results$medianSurvivalExplanation$setVisible(FALSE)
                self$results$medianSurvivalHeading3$setVisible(FALSE)
                self$results$coxRegressionHeading$setVisible(FALSE)
                self$results$coxRegressionExplanation$setVisible(FALSE)
                self$results$coxRegressionHeading3$setVisible(FALSE)
                self$results$survivalTablesHeading$setVisible(FALSE)
                self$results$survivalTablesExplanation$setVisible(FALSE)
                self$results$survivalTablesHeading3$setVisible(FALSE)
                self$results$survivalPlotsHeading3$setVisible(FALSE)
                self$results$survivalPlotsExplanation$setVisible(FALSE)
                
                # Early validation check - if essential variables are missing, show todo and return
                if (is.null(self$options$outcome) || 
                    is.null(self$options$explanatory) || 
                    (is.null(self$options$elapsedtime) && 
                     (!self$options$tint || is.null(self$options$dxdate) || is.null(self$options$fudate)))) {
                    
                    # Show todo message (other outputs already hidden above)
                    self$results$todo$setVisible(TRUE)
                    private$.todo()
                    return()
                }
                
                # Initialize all outputs to FALSE first (following singlearm pattern)
                # Core survival analysis outputs
                self$results$medianSurvivalHeading$setVisible(FALSE)
                self$results$medianSurvivalExplanation$setVisible(FALSE)
                self$results$medianSurvivalHeading3$setVisible(FALSE)
                self$results$medianSummary$setVisible(FALSE)
                self$results$medianTable$setVisible(FALSE)
                
                # Cox regression outputs
                self$results$coxRegressionHeading$setVisible(FALSE)
                self$results$coxRegressionExplanation$setVisible(FALSE)
                self$results$coxRegressionHeading3$setVisible(FALSE)
                self$results$coxSummary$setVisible(FALSE)
                self$results$coxTable$setVisible(FALSE)
                self$results$tCoxtext2$setVisible(FALSE)
                self$results$cox_ph$setVisible(FALSE)
                self$results$plot8$setVisible(FALSE)
                
                # Survival tables outputs
                self$results$survivalTablesHeading$setVisible(FALSE)
                self$results$survivalTablesExplanation$setVisible(FALSE)
                self$results$survivalTablesHeading3$setVisible(FALSE)
                self$results$survTableSummary$setVisible(FALSE)
                self$results$survTable$setVisible(FALSE)
                
                # Survival plots outputs (only reference existing plots)
                self$results$plot$setVisible(FALSE)
                self$results$plot2$setVisible(FALSE)
                self$results$plot3$setVisible(FALSE)
                self$results$plot6$setVisible(FALSE)
                self$results$plot7$setVisible(FALSE)
                self$results$plot8$setVisible(FALSE)
                self$results$survivalPlotsHeading3$setVisible(FALSE)
                self$results$survivalPlotsExplanation$setVisible(FALSE)
                
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
                
                # Pairwise comparison outputs
                self$results$pairwiseComparisonHeading$setVisible(FALSE)
                self$results$pairwiseSummary$setVisible(FALSE)
                self$results$pairwiseTable$setVisible(FALSE)
                
                # Parametric models outputs
                self$results$parametricModelComparison$setVisible(FALSE)
                self$results$parametricModelSummary$setVisible(FALSE)
                self$results$parametricDiagnostics$setVisible(FALSE)
                self$results$parametricSurvivalPlot$setVisible(FALSE)
                self$results$hazardFunctionPlot$setVisible(FALSE)
                self$results$extrapolationPlot$setVisible(FALSE)
                self$results$extrapolationTable$setVisible(FALSE)
                self$results$parametricModelsExplanation$setVisible(FALSE)
                
                # Always show core survival analysis elements when data is present
                self$results$medianSurvivalHeading$setVisible(TRUE)
                self$results$medianTable$setVisible(TRUE)
                self$results$coxRegressionHeading$setVisible(TRUE)
                self$results$coxTable$setVisible(TRUE)
                self$results$tCoxtext2$setVisible(TRUE)
                self$results$survivalTablesHeading$setVisible(TRUE)
                self$results$survTable$setVisible(TRUE)
                
                # Handle showSummaries visibility
                if (self$options$showSummaries) {
                    self$results$medianSummary$setVisible(TRUE)
                    self$results$coxSummary$setVisible(TRUE)
                    self$results$survTableSummary$setVisible(TRUE)
                    
                    # Conditional summaries - require both showSummaries AND their specific option
                    if (self$options$person_time) {
                        self$results$personTimeSummary$setVisible(TRUE)
                    }
                    if (self$options$rmst_analysis) {
                        self$results$rmstSummary$setVisible(TRUE)
                    }
                    if (self$options$pw) {
                        self$results$pairwiseComparisonHeading$setVisible(TRUE)
                        self$results$pairwiseSummary$setVisible(TRUE)
                    }
                }

                # Handle showExplanations visibility  
                if (self$options$showExplanations) {
                    # Core explanations and headings
                    self$results$medianSurvivalHeading3$setVisible(TRUE)
                    self$results$medianSurvivalExplanation$setVisible(TRUE)
                    self$results$coxRegressionHeading3$setVisible(TRUE)
                    self$results$coxRegressionExplanation$setVisible(TRUE)
                    self$results$survivalTablesHeading3$setVisible(TRUE)
                    self$results$survivalTablesExplanation$setVisible(TRUE)
                    
                    # Conditional explanations - require both showExplanations AND their specific option
                    if (self$options$person_time) {
                        self$results$personTimeExplanation$setVisible(TRUE)
                    }
                    if (self$options$rmst_analysis) {
                        self$results$rmstExplanation$setVisible(TRUE)
                    }
                    if (self$options$residual_diagnostics) {
                        self$results$residualDiagnosticsExplanation$setVisible(TRUE)
                    }
                    if (self$options$use_parametric) {
                        self$results$parametricModelsExplanation$setVisible(TRUE)
                    }
                    
                    # Survival plots explanation requires showExplanations AND at least one plot
                    if (self$options$sc || self$options$ce || self$options$ch || 
                        self$options$kmunicate || self$options$loglog) {
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
                
                # Handle pairwise comparison visibility
                if (self$options$pw) {
                    self$results$pairwiseComparisonHeading$setVisible(TRUE)
                    self$results$pairwiseSummary$setVisible(TRUE)
                    self$results$pairwiseTable$setVisible(TRUE)
                }
                
                # Handle parametric models visibility
                if (self$options$use_parametric) {
                    self$results$parametricModelSummary$setVisible(TRUE)
                    if (self$options$compare_distributions) {
                        self$results$parametricModelComparison$setVisible(TRUE)
                    }
                    if (self$options$parametric_diagnostics) {
                        self$results$parametricDiagnostics$setVisible(TRUE)
                    }
                    if (self$options$parametric_survival_plots) {
                        self$results$parametricSurvivalPlot$setVisible(TRUE)
                    }
                    if (self$options$hazard_plots) {
                        self$results$hazardFunctionPlot$setVisible(TRUE)
                    }
                    if (self$options$parametric_extrapolation) {
                        self$results$extrapolationPlot$setVisible(TRUE)
                        self$results$extrapolationTable$setVisible(TRUE)
                    }
                    if (self$options$showExplanations) {
                        self$results$parametricModelsExplanation$setVisible(TRUE)
                    }
                }
                
                # Handle Cox PH visibility
                if (self$options$ph_cox) {
                    self$results$cox_ph$setVisible(TRUE)
                    self$results$plot8$setVisible(TRUE)
                }
                
                # Handle plot visibility based on their options
                if (self$options$sc) {
                    self$results$plot$setVisible(TRUE)
                }
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
                if (self$options$residual_diagnostics) {
                    self$results$residualsPlot$setVisible(TRUE)
                }
                if (self$options$use_parametric) {
                    # Parametric plots handled in parametric section
                    self$results$parametricSurvivalPlot$setVisible(TRUE)
                    if (self$options$hazard_plots) {
                        self$results$hazardFunctionPlot$setVisible(TRUE)
                    }
                }
            }
            ,


            .getData = function() {

            mydata <- self$data

            mydata$row_names <- rownames(mydata)

            original_names <- names(mydata)

            labels <- setNames(original_names, original_names)

            mydata <- mydata %>% janitor::clean_names()

            corrected_labels <-
                setNames(original_names, names(mydata))

            mydata <- labelled::set_variable_labels(.data = mydata,
                                                    .labels = corrected_labels)

            all_labels <- labelled::var_label(mydata)


            mytime <-
                names(all_labels)[all_labels == self$options$elapsedtime]

            myoutcome <-
                names(all_labels)[all_labels == self$options$outcome]

            mydxdate <-
                names(all_labels)[all_labels == self$options$dxdate]

            myfudate <-
                names(all_labels)[all_labels == self$options$fudate]

            myexplanatory <-
                names(all_labels)[all_labels == self$options$explanatory]

            return(list(
                "mydata_labelled" = mydata
                , "mytime_labelled" = mytime
                , "myoutcome_labelled" = myoutcome
                , "mydxdate_labelled" = mydxdate
                , "myfudate_labelled" = myfudate
                , "myexplanatory_labelled" = myexplanatory
                , "all_labels" = all_labels
                , "original_names_mapping" = corrected_labels
            ))


            }





            ,
            .todo = function() {

                 todo <- glue::glue(
                        "
                <br>Welcome to ClinicoPath
                <br><br>
                This tool will help you calculate median survivals and 1,3,5-yr survivals for a given fisk factor.
                <br><br>
                Explanatory variable should be categorical (ordinal or nominal).
                <br><br>
                Select outcome level from Outcome variable.
                <br><br>
                Outcome Level: if patient is dead or event (recurrence) occured. You may also use advanced outcome options depending on your analysis type.
                <br><br>
                Survival time should be numeric and continuous. You may also use dates to calculate survival time in advanced elapsed time options.
                <br><br>
                This function uses survival, survminer, and finalfit packages. Please cite jamovi and the packages as given below.
                <br><hr>
                <br>
                See details for survival <a href = 'https://cran.r-project.org/web/packages/survival/vignettes/survival.pdf'>here</a>."
                    )

                    html <- self$results$todo
                    html$setContent(todo)

            }

            # Input Validation ----
            ,
            .validateInputs = function() {
                # Check required variables
                if (is.null(self$options$outcome)) {
                    jmvcore::reject(.("Outcome variable is required. Please select a variable that indicates whether an event occurred (e.g., death, recurrence)."), code="missing_outcome")
                }
                
                if (is.null(self$options$explanatory)) {
                    jmvcore::reject(.("Explanatory variable is required. Please select a categorical variable to compare survival between groups."), code="missing_explanatory")
                }
                
                # Check time variables
                time_provided <- !is.null(self$options$elapsedtime)
                dates_provided <- self$options$tint && !is.null(self$options$dxdate) && !is.null(self$options$fudate)
                
                if (!time_provided && !dates_provided) {
                    jmvcore::reject(.("Time information is required. Either provide a survival time variable or enable date calculation with diagnosis and follow-up dates."), code="missing_time")
                }
                
                if (self$options$tint && (is.null(self$options$dxdate) || is.null(self$options$fudate))) {
                    jmvcore::reject(.("When using dates to calculate survival time, both diagnosis date and follow-up date are required."), code="incomplete_dates")
                }
                
                # Check multievent configuration
                if (self$options$multievent) {
                    if (is.null(self$options$dod) && is.null(self$options$dooc)) {
                        jmvcore::reject(.("When using multiple event levels, at least one event type (Dead of Disease or Dead of Other) must be specified."), code="missing_multievent")
                    }
                }
                
                # Check data availability
                if (nrow(self$data) == 0) {
                    jmvcore::reject(.("No data available for analysis. Please check your dataset."), code="empty_data")
                }
                
                return(TRUE)
            }

            # Optimized Table Population Helper ----
            ,
            .populateTableSafely = function(table_result, data_frame, column_mapping) {
                # Generic helper to reduce code duplication in table population
                # Uses explicit column mapping for type safety and better performance
                tryCatch({
                    if (is.null(data_frame) || nrow(data_frame) == 0) {
                        return(invisible(NULL))
                    }
                    
                    for (i in seq_len(nrow(data_frame))) {
                        row_values <- list()
                        
                        # Map columns using provided mapping
                        for (col_name in names(column_mapping)) {
                            source_col <- column_mapping[[col_name]]
                            if (source_col %in% names(data_frame)) {
                                row_values[[col_name]] <- data_frame[[source_col]][i]
                            } else {
                                row_values[[col_name]] <- NA
                            }
                        }
                        
                        table_result$addRow(rowKey = i, values = row_values)
                        
                        # Add checkpoint for large tables (every 100 rows)
                        if (i %% 100 == 0) {
                            private$.checkpoint()
                        }
                    }
                }, error = function(e) {
                    # Log error but don't break the analysis
                    warning(paste(.("Table population failed:"), e$message))
                })
            }
            
            # Safe Analysis Wrapper ----
            ,
            .safeAnalysis = function(analysis_func, error_message = "Analysis step failed") {
                # Wrapper for safer analysis execution with better error recovery
                tryCatch({
                    analysis_func()
                    TRUE
                }, error = function(e) {
                    # Cleanup memory and reset state on error
                    gc(verbose = FALSE)
                    private$.resetErrorState()
                    warning(paste(error_message, ":", e$message))
                    FALSE
                })
            },
            
            .resetErrorState = function() {
                # Hide conditional outputs on error to prevent inconsistent state
                self$results$plot$setVisible(FALSE)
                self$results$plot2$setVisible(FALSE)
                self$results$plot3$setVisible(FALSE)
                self$results$plot6$setVisible(FALSE)
                self$results$plot7$setVisible(FALSE)
                self$results$plot8$setVisible(FALSE)
                self$results$pairwiseTable$setVisible(FALSE)
                self$results$personTimeTable$setVisible(FALSE)
                self$results$rmstTable$setVisible(FALSE)
                # Clear any large objects from memory
                if (exists('.large_objects', envir = private)) {
                    rm(.large_objects, envir = private)
                }
            },

            # FIX: Helper functions for competing risk analysis ----
            # These functions provide proper cumulative incidence function (CIF)
            # support for competing risk scenarios (analysistype = "compete")

            .isCompetingRisk = function() {
                # Check if current analysis is competing risk mode
                return(self$options$multievent && self$options$analysistype == "compete")
            },

            .competingRiskCumInc = function(mydata, mytime, myoutcome, myfactor = NULL) {
                # Calculate cumulative incidence function for competing risks
                # Uses cmprsk package for proper handling of competing events
                #
                # Args:
                #   mydata: cleaned data frame
                #   mytime: time variable name
                #   myoutcome: outcome variable name (0=censored, 1=event, 2=competing)
                #   myfactor: grouping variable name (optional)
                # Returns:
                #   cuminc object from cmprsk package

                mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])
                
                # Get grouping variable if provided
                group_var <- if (!is.null(myfactor) && myfactor %in% names(mydata)) mydata[[myfactor]] else NULL

                cuminc_fit <- cmprsk::cuminc(
                    ftime = mydata[[mytime]],
                    fstatus = mydata[[myoutcome]],
                    group = group_var,
                    cencode = 0
                )
                return(cuminc_fit)
            },

            .getDefaultCutpoints = function() {
                # Get default time cutpoints based on selected time unit
                # This ensures cutpoints are appropriate for the time scale
                #
                # Returns:
                #   Numeric vector of default cutpoints (1, 3, 5 year equivalents)

                time_unit <- self$options$timetypeoutput
                switch(time_unit,
                    "days" = c(365, 1095, 1825),
                    "weeks" = c(52, 156, 260),
                    "months" = c(12, 36, 60),
                    "years" = c(1, 3, 5),
                    c(12, 36, 60)  # default to months
                )
            }

            # Define Survival Time ----
            ,
            .definemytime = function() {

            # Read Labelled Data ----

            labelled_data <- private$.getData()

            mydata <- labelled_data$mydata_labelled
            mytime_labelled <- labelled_data$mytime_labelled
            mydxdate_labelled <- labelled_data$mydxdate_labelled
            myfudate_labelled <- labelled_data$myfudate_labelled

            tint <- self$options$tint


                if (!tint) {
                    # Precalculated Time ----

                    mydata[["mytime"]] <-
                        jmvcore::toNumeric(mydata[[mytime_labelled]])


                } else if (tint) {
                    # Time Interval ----

                    dxdate <- mydxdate_labelled # self$options$dxdate
                    fudate <- myfudate_labelled #self$options$fudate
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
                            date_parser <- lubridate_functions[[timetypedata]]
                            mydata[["start"]] <- date_parser(mydata[[dxdate]])
                            mydata[["end"]] <- date_parser(mydata[[fudate]])
                        } else {
                            stop(sprintf(.("Unknown date format: %s. Supported formats are: %s"),
                                       timetypedata,
                                       paste(names(lubridate_functions), collapse = ", ")))
                        }
                    } else {
                        # Mixed types error
                        stop(.("Diagnosis date and follow-up date must be in the same format (both numeric or both text)"))
                    }


                    if ( sum(!is.na(mydata[["start"]])) == 0 || sum(!is.na(mydata[["end"]])) == 0)  {
                        stop(sprintf(.("Time difference cannot be calculated. Make sure that time type in variables are correct. Currently it is: %s"), self$options$timetypedata))
                    }

                    timetypeoutput <-
                        jmvcore::constructFormula(terms = self$options$timetypeoutput)


                    mydata <- mydata %>%
                        dplyr::mutate(interval = lubridate::interval(start, end))



                    mydata <- mydata %>%
                        dplyr::mutate(mytime = lubridate::time_length(interval, timetypeoutput))


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
                        unique_values <- unique(outcome1[!is.na(outcome1)])
                        if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
                            stop(sprintf(
                                .('Outcome variable must be binary (0/1) for survival analysis.\n- Use 0 for censored observations (alive/disease-free)\n- Use 1 for events (death/recurrence)\nCurrent values found: %s\n\nFor multi-state outcomes, enable "Multiple Event Levels" option.'),
                                paste(unique_values, collapse = ", ")
                            ))

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
                        stop(sprintf(
                            .('Invalid outcome variable format.\nFor survival analysis, the outcome variable must be:\n- Binary numeric (0/1): 0=censored, 1=event\n- Factor variable: Select appropriate event level\n\nCurrent variable type: %s\nFor complex outcomes with multiple states, enable "Multiple Event Levels" option.'),
                            class(outcome1)[1]
                        ))

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
            myexplanatory_labelled <- labelled_data$myexplanatory_labelled

            mydata <- mydata_labelled

            mydata[["myfactor"]] <- mydata[[myexplanatory_labelled]]


            df_factor <- mydata %>% jmvcore::select(c("row_names","myfactor"))

            return(df_factor)

            }


            # Clean Data For Analysis ----
            ,
            .cleandata = function() {
                # Memory management for large datasets
                if (nrow(self$data) > 10000) {
                    # Force garbage collection before processing large datasets
                    gc(verbose = FALSE)
                }

            labelled_data <- private$.getData()

            mydata_labelled        <- labelled_data$mydata_labelled
            mytime_labelled        <- labelled_data$mytime_labelled
            myoutcome_labelled     <- labelled_data$myoutcome_labelled
            mydxdate_labelled      <- labelled_data$mydxdate_labelled
            myfudate_labelled      <- labelled_data$myfudate_labelled
            myexplanatory_labelled <- labelled_data$myexplanatory_labelled

                time <- private$.definemytime()
                outcome <- private$.definemyoutcome()
                factor <- private$.definemyfactor()

                if (is.null(time) || is.null(outcome) || is.null(factor)) {
                    stop(.("Error: Data could not be cleaned for analysis."))
                }

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

                # Time Dependent Covariate ----
                # https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#time-dependent_covariate


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

                if (!is.null(self$options$explanatory)
                    ) {
                    name3explanatory <- myexplanatory_labelled
                    }

                    cleanData <- cleanData %>%
                        dplyr::rename(
                            !!name1time := mytime,
                            !!name2outcome := myoutcome,
                            !!name3explanatory := myfactor
                        )

                # naOmit ----

                cleanData <- jmvcore::naOmit(cleanData)


                # Prepare Data For Plots ----

                plotData <- list(
                    "name1time" = name1time,
                    "name2outcome" = name2outcome,
                    "name3explanatory" = name3explanatory,
                    "cleanData" = cleanData
                )

                image <- self$results$plot
                image$setState(plotData)

                image2 <- self$results$plot2
                image2$setState(plotData)

                image3 <- self$results$plot3
                image3$setState(plotData)

                image6 <- self$results$plot6
                image6$setState(plotData)
                
                image7 <- self$results$plot7
                image7$setState(plotData)

                # Return Data ----

                return(
                    list(
                        "name1time" = name1time,
                        "name2outcome" = name2outcome,
                        "name3explanatory" = name3explanatory,
                        "cleanData" = cleanData,
                        "mytime_labelled" = mytime_labelled,
                        "myoutcome_labelled" = myoutcome_labelled,
                        "mydxdate_labelled" = mydxdate_labelled,
                        "myfudate_labelled" = myfudate_labelled,
                        "myexplanatory_labelled" = myexplanatory_labelled
                    )
                )

            }


            # Core Analysis Components ----
            ,
            .runCoreAnalysis = function(results) {
                # Run core survival analysis components
                private$.safeAnalysis(function() {
                    private$.medianSurv(results)
                }, .("Median survival analysis failed"))
                private$.checkpoint()
                
                private$.safeAnalysis(function() {
                    private$.cox(results)
                }, .("Cox regression analysis failed"))
                private$.checkpoint()
                
                private$.safeAnalysis(function() {
                    private$.survTable(results)
                }, .("Survival table generation failed"))
                private$.checkpoint()
            }
            
            # Optional Analysis Components ----
            ,
            .runOptionalAnalyses = function(results) {
                # RMST Analysis
                if (self$options$rmst_analysis) {
                    private$.safeAnalysis(function() {
                        rmst_tau <- if (is.null(self$options$rmst_tau) || self$options$rmst_tau <= 0) {
                            NULL  # Use default (75th percentile)
                        } else {
                            self$options$rmst_tau
                        }
                        
                        rmst_results <- private$.calculateRMST(results, tau = rmst_tau)
                        
                        if (!is.null(rmst_results$table)) {
                            # Use helper function for table population
                            column_mapping <- list(
                                group = "Group",
                                rmst = "RMST", 
                                se = "SE",
                                ci_lower = "CI_Lower",
                                ci_upper = "CI_Upper",
                                tau = "Tau"
                            )
                            private$.populateTableSafely(
                                self$results$rmstTable, 
                                rmst_results$table, 
                                column_mapping
                            )
                            
                            # Add interpretation
                            self$results$rmstSummary$setContent(rmst_results$interpretation)
                        }
                    }, .("RMST analysis failed"))
                    private$.checkpoint()
                }
                
                # Parametric Survival Models
                if (self$options$use_parametric) {
                    private$.safeAnalysis(function() {
                        private$.parametricSurvival(results)
                    }, .("Parametric survival analysis failed"))
                    private$.checkpoint()
                }
                
                # Pairwise Comparisons
                if (self$options$pw) {
                    private$.safeAnalysis(function() {
                        private$.pairwise(results)
                    }, .("Pairwise comparison analysis failed"))
                    private$.checkpoint()
                }
                
                # Person-Time Analysis
                if (self$options$person_time) {
                    private$.safeAnalysis(function() {
                        private$.personTimeAnalysis(results)
                    }, .("Person-time analysis failed"))
                    private$.checkpoint()
                }
            }
            
            # Data Export and Finalization ----
            ,
            .finalizeResults = function(results) {
                # Handle data exports and final result population
                private$.safeAnalysis(function() {
                    private$.exportSurvivalData(results)
                }, .("Survival data export failed"))
                
                # Add Calculated Time to Data
                if (self$options$tint && self$options$calculatedtime && 
                    self$results$calculatedtime$isNotFilled()) {
                    self$results$calculatedtime$setRowNums(results$cleanData$row_names)
                    self$results$calculatedtime$setValues(results$cleanData$CalculatedTime)
                }
                
                # Add Redefined Outcome to Data
                if (self$options$multievent && self$options$outcomeredefined && 
                    self$results$outcomeredefined$isNotFilled()) {
                    self$results$outcomeredefined$setRowNums(results$cleanData$row_names)
                    self$results$outcomeredefined$setValues(results$cleanData$CalculatedOutcome)
                }
                
                # Populate explanations if enabled
                private$.populateExplanations()
                
                # Populate enhanced clinical content
                private$.populateEnhancedClinicalContent()
            }

            # Main Run Function (Refactored) ----
            ,
            .run = function() {

                # Input Validation ----
                validation_result <- tryCatch({
                    private$.validateInputs()
                    self$results$todo$setVisible(FALSE)
                    TRUE
                }, error = function(e) {
                    # If validation fails, show todo and hide results
                    private$.todo()
                    self$results$medianSummary$setVisible(FALSE)
                    self$results$medianTable$setVisible(FALSE)
                    self$results$coxSummary$setVisible(FALSE)
                    self$results$coxTable$setVisible(FALSE)
                    self$results$tCoxtext2$setVisible(FALSE)
                    self$results$cox_ph$setVisible(FALSE)
                    self$results$plot8$setVisible(FALSE)
                    self$results$survTableSummary$setVisible(FALSE)
                    self$results$survTable$setVisible(FALSE)
                    self$results$pairwiseSummary$setVisible(FALSE)
                    self$results$pairwiseTable$setVisible(FALSE)
                    self$results$plot$setVisible(FALSE)
                    self$results$plot2$setVisible(FALSE)
                    self$results$plot3$setVisible(FALSE)
                    self$results$plot6$setVisible(FALSE)
                    self$results$todo$setVisible(TRUE)
                    FALSE
                })
                
                # Return early if validation failed
                if (validation_result != TRUE) {
                    return()
                }

                # Get Clean Data ----
                results <- private$.cleandata()

                private$.checkpoint()  # Add checkpoint here


                if (is.null(results)) {
                    return()
                }

                # Run Analysis ----
                ## Median Survival ----
                    private$.medianSurv(results)
                private$.checkpoint()  # Add checkpoint here
                
                ## RMST Analysis ----
                if (self$options$rmst_analysis) {
                    rmst_tau <- if (is.null(self$options$rmst_tau) || self$options$rmst_tau <= 0) {
                        NULL  # Use default (75th percentile)
                    } else {
                        self$options$rmst_tau
                    }
                    
                    rmst_results <- private$.calculateRMST(results, tau = rmst_tau)
                    
                    if (!is.null(rmst_results$table)) {
                        # Populate RMST table
                        rmst_table <- self$results$rmstTable
                        for (i in 1:nrow(rmst_results$table)) {
                            rmst_table$addRow(rowKey = i, values = list(
                                group = rmst_results$table$Group[i],
                                rmst = rmst_results$table$RMST[i],
                                se = rmst_results$table$SE[i],
                                ci_lower = rmst_results$table$CI_Lower[i],
                                ci_upper = rmst_results$table$CI_Upper[i],
                                tau = rmst_results$table$Tau[i]
                            ))
                        }
                        
                        # Add interpretation
                        self$results$rmstSummary$setContent(rmst_results$interpretation)
                    }
                }
                private$.checkpoint()  # Add checkpoint here

                ## Cox ----
                    private$.cox(results)
                private$.checkpoint()  # Add checkpoint here

                ## Survival Table ----
                    private$.survTable(results)
                private$.checkpoint()  # Add checkpoint here
                
                ## Export Survival Data ----
                private$.exportSurvivalData(results)
                private$.checkpoint()  # Add checkpoint here

                ## Parametric Survival Models ----
                if (self$options$use_parametric) {
                    private$.parametricSurvival(results)
                }
                private$.checkpoint()  # Add checkpoint here

                ## Pairwise ----
                if (self$options$pw
                    # && !self$options$sas
                    ) {
                    private$.pairwise(results)
                }


                ## Add the person-time analysis ----
                private$.checkpoint()  # Add checkpoint here

                # Run person-time analysis if enabled
                if (self$options$person_time) {
                    private$.personTimeAnalysis(results)
                }
                
                ## Additional Model Diagnostics ----
                # Note: residual diagnostics are handled within .cox() function
                # when self$options$residual_diagnostics is TRUE



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
                
                # Populate explanations if enabled
                private$.populateExplanations()
                
                # Populate enhanced clinical content
                private$.populateEnhancedClinicalContent()
            }

            # RMST Analysis Function ----
            ,
            .calculateRMST = function(results, tau = NULL) {
                # Restricted Mean Survival Time calculation
                tryCatch({
                    mytime <- results$name1time
                    myoutcome <- results$name2outcome
                    myfactor <- results$name3explanatory
                    mydata <- results$cleanData
                    
                    mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])
                    
                    # Set default tau to 75th percentile of follow-up time if not specified
                    if (is.null(tau)) {
                        tau <- quantile(mydata[[mytime]], 0.75, na.rm = TRUE)
                    }
                    
                    # Escape variable names for safe formula construction
                    escaped_mytime <- .escapeVariableNames(mytime)
                    escaped_myoutcome <- .escapeVariableNames(myoutcome)
                    escaped_myfactor <- .escapeVariableNames(myfactor)
                    
                    formula <- paste('survival::Surv(', escaped_mytime, ',', escaped_myoutcome, ') ~ ', escaped_myfactor)
                    formula <- as.formula(formula)
                    
                    private$.checkpoint()
                    
                    km_fit <- survival::survfit(formula, data = mydata)
                    
                    # Calculate RMST for each group
                    rmst_summary <- summary(km_fit, rmean = tau, extend = TRUE)
                    
                    rmst_table <- data.frame(
                        Group = gsub(paste0(myfactor, "="), "", names(km_fit$strata)),
                        RMST = round(rmst_summary$table[, "rmean"], 2),
                        SE = round(rmst_summary$table[, "se(rmean)"], 2),
                        Tau = rep(round(tau, 1), length(km_fit$strata)),
                        stringsAsFactors = FALSE
                    )
                    
                    # Add confidence intervals (approximate)
                    rmst_table$CI_Lower <- round(rmst_table$RMST - 1.96 * rmst_table$SE, 2)
                    rmst_table$CI_Upper <- round(rmst_table$RMST + 1.96 * rmst_table$SE, 2)
                    
                    return(list(
                        table = rmst_table,
                        tau = tau,
                        interpretation = paste0(
                            "Restricted Mean Survival Time (RMST) represents the average survival time ",
                            "up to ", round(tau, 1), " ", self$options$timetypeoutput, ". ",
                            "This metric is useful when median survival cannot be estimated (curves don't reach 50%) ",
                            "or when comparing survival over a specific time horizon."
                        )
                    ))
                }, error = function(e) {
                    return(list(
                        table = NULL,
                        tau = NULL,
                        interpretation = paste("Error calculating RMST:", e$message)
                    ))
                })
            }
            
            # Median Survival Function ----
            ,
            .medianSurv = function(results) {

                mytime <- results$name1time
                myoutcome <- results$name2outcome
                myfactor <- results$name3explanatory
                myexplanatory_labelled <- results$myexplanatory_labelled

                mydata <- results$cleanData

                mydata[[mytime]] <-
                    jmvcore::toNumeric(mydata[[mytime]])

                ## Median Survival Table ----

                # FIX: Branch logic for competing risk vs standard survival
                if (private$.isCompetingRisk()) {
                    # COMPETING RISK MODE: Use cumulative incidence function
                    # This provides proper handling of competing events

                    cuminc_fit <- private$.competingRiskCumInc(mydata, mytime, myoutcome, myfactor)

                    # Process cumulative incidence by group if factor exists
                    if (!is.null(myfactor) && myfactor %in% names(mydata)) {
                        # Get unique groups
                        groups <- unique(mydata[[myfactor]])
                        groups <- groups[!is.na(groups)]

                        # Create results table manually from CIF
                        results2table <- data.frame(
                            factor = character(0),
                            records = numeric(0),
                            events = numeric(0),
                            median = numeric(0),
                            x0_95lcl = numeric(0),
                            x0_95ucl = numeric(0),
                            stringsAsFactors = FALSE
                        )

                        for (group in groups) {
                            # Get CIF for this group and event type 1
                            cif_name <- paste(group, "1", sep = " ")

                            if (cif_name %in% names(cuminc_fit)) {
                                cif_est <- cuminc_fit[[cif_name]]$est
                                cif_times <- cuminc_fit[[cif_name]]$time

                                # Calculate median: time when CIF reaches 0.5
                                median_time <- NA
                                ci_lower <- NA
                                ci_upper <- NA

                                if (max(cif_est, na.rm = TRUE) >= 0.5) {
                                    median_idx <- which(cif_est >= 0.5)[1]
                                    median_time <- cif_times[median_idx]

                                    # Approximate CI using variance
                                    if (!is.null(cuminc_fit[[cif_name]]$var)) {
                                        median_var <- cuminc_fit[[cif_name]]$var[median_idx]
                                        median_se <- sqrt(median_var)
                                        ci_lower <- median_time - 1.96 * median_se
                                        ci_upper <- median_time + 1.96 * median_se
                                    }
                                }

                                # Count events and records for this group
                                group_data <- mydata[mydata[[myfactor]] == group, ]
                                n_records <- nrow(group_data)
                                n_events <- sum(group_data[[myoutcome]] == 1, na.rm = TRUE)  # Event of interest only

                                results2table <- rbind(results2table, data.frame(
                                    factor = as.character(group),
                                    records = n_records,
                                    events = n_events,
                                    median = median_time,
                                    x0_95lcl = ci_lower,
                                    x0_95ucl = ci_upper,
                                    stringsAsFactors = FALSE
                                ))
                            }
                        }
                    } else {
                        # No grouping variable - overall CIF
                        cif_est <- cuminc_fit[["1"]]$est  # Event type 1
                        cif_times <- cuminc_fit[["1"]]$time

                        median_time <- NA
                        if (max(cif_est, na.rm = TRUE) >= 0.5) {
                            median_idx <- which(cif_est >= 0.5)[1]
                            median_time <- cif_times[median_idx]
                        }

                        results2table <- data.frame(
                            factor = "Overall",
                            records = nrow(mydata),
                            events = sum(mydata[[myoutcome]] == 1, na.rm = TRUE),
                            median = median_time,
                            x0_95lcl = NA,
                            x0_95ucl = NA,
                            stringsAsFactors = FALSE
                        )
                    }

                } else {
                    # STANDARD SURVIVAL MODE: Use Kaplan-Meier

                    formula <-
                        paste('survival::Surv(',
                              mytime,
                              ',',
                              myoutcome,
                              ') ~ ',
                              myfactor)

                    formula <- as.formula(formula)

                    private$.checkpoint()

                    km_fit <- survival::survfit(formula, data = mydata)

                    km_fit_median_df <- summary(km_fit)





                results1html <-
                    as.data.frame(km_fit_median_df$table) %>%

                    janitor::clean_names(dat = ., case = "snake") %>%
                    tibble::rownames_to_column(.data = .)


                results1html[, 1] <- gsub(pattern = ", ",
                                          replacement = " and ",
                                          x = results1html[, 1])

                results1table <- results1html

                names(results1table)[1] <- "factor"


                results2table <- results1table

                results2table$factor <- gsub(pattern = paste0(myexplanatory_labelled,"="),
                                             replacement = "",
                                             x = results1table$factor)
                }

                # At this point, results2table exists for both competing risk and standard survival

                medianTable <- self$results$medianTable
                data_frame <- results2table
                for (i in seq_along(data_frame[, 1, drop = T])) {
                    # Map columns explicitly to match schema
                    row_values <- list(
                        factor = data_frame$factor[i],
                        records = if ("records" %in% names(data_frame)) data_frame$records[i] else NA,
                        events = if ("events" %in% names(data_frame)) data_frame$events[i] else NA,
                        rmean = if ("rmean" %in% names(data_frame)) data_frame$rmean[i] else NA,
                        se_rmean = if ("se_rmean" %in% names(data_frame)) data_frame$se_rmean[i] else NA,
                        median = if ("median" %in% names(data_frame)) data_frame$median[i] else NA,
                        x0_95lcl = if ("x0_95lcl" %in% names(data_frame)) data_frame$x0_95lcl[i] else NA,
                        x0_95ucl = if ("x0_95ucl" %in% names(data_frame)) data_frame$x0_95ucl[i] else NA
                    )
                    medianTable$addRow(rowKey = i, values = row_values)
                }


                ## Median Survival Summary ----

                results2table %>%
                    dplyr::mutate(
                        description =
                            glue::glue(
                                "When {factor}, median survival is {round(median, digits = 1)} [{round(x0_95lcl, digits = 1)} - {round(x0_95ucl, digits = 1)}, 95% CI] ",
                                self$options$timetypeoutput,
                                "."
                            )
                    ) %>%
                  dplyr::mutate(
                    description = dplyr::case_when(
                      is.na(median) ~ paste0(
                        glue::glue("{description} \n Note that when {factor}, the survival curve does not drop below 1/2 during \n the observation period, thus the median survival is undefined.")),
                      TRUE ~ paste0(description)
                    )
                  ) %>%
                  dplyr::mutate(description = gsub(
                    pattern = "=",
                    replacement = " is ",
                    x = description
                  )) %>%
                  dplyr::mutate(description = gsub(
                    pattern = myexplanatory_labelled,
                    replacement = self$options$explanatory,
                    x = description
                  )) %>%
                    dplyr::select(description) %>%
                    dplyr::pull(.) -> km_fit_median_definition

                medianSummary <- c(km_fit_median_definition,
                                   "The median survival time is when 50% of subjects have experienced the event.",
                                   "This means that 50% of subjects in this group survived longer than this time period.",
                                   "Note: Confidence intervals are calculated using the log-log transformation method for improved accuracy with censored data (not plain Greenwood formula)."
                                   )

                self$results$medianSummary$setContent(medianSummary)

                # Add explanatory output for median survival
                if (self$options$showExplanations) {
                    median_explanation_html <- '
                    <div style="margin-bottom: 20px; padding: 15px; background-color: #f0f8ff; border-left: 4px solid #4169e1;">
                        <h4 style="margin-top: 0; color: #2c3e50;">Understanding Median Survival by Groups</h4>
                        <p style="margin-bottom: 10px;">This analysis compares median survival times between different groups in your data:</p>
                        <ul style="margin-left: 20px;">
                            <li><strong>Median Survival:</strong> Time when 50% of patients in each group experienced the event</li>
                            <li><strong>Group Comparison:</strong> Allows identification of prognostic factors</li>
                            <li><strong>95% CI:</strong> Confidence intervals for each group\'s median survival</li>
                            <li><strong>Log-rank Test:</strong> Statistical test comparing survival curves between groups</li>
                        </ul>
                        <p style="margin-bottom: 5px;"><strong>Clinical interpretation:</strong></p>
                        <ul style="margin-left: 20px;">
                            <li>Groups with longer median survival have better prognosis</li>
                            <li>Non-overlapping confidence intervals suggest significant differences</li>
                            <li>Use for patient stratification and treatment decisions</li>
                            <li>Consider clinical significance alongside statistical significance</li>
                        </ul>
                    </div>
                    '
                    self$results$medianSurvivalExplanation$setContent(median_explanation_html)
                }


            }

            ## Cox Regression Function ----
            ,
            .cox = function(results) {
                ### Cox Regression ----


                mytime <- results$name1time
                mytime <- jmvcore::constructFormula(terms = mytime)

                myoutcome <- results$name2outcome
                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)


                myfactor <- results$name3explanatory
                myfactor <-
                    jmvcore::constructFormula(terms = myfactor)

                mydata <- results$cleanData

                mydata[[mytime]] <-
                    jmvcore::toNumeric(mydata[[mytime]])

                myformula <-
                    paste("Surv(", mytime, ",", myoutcome, ")")
                
                # Add stratified Cox regression if enabled
                strata_var <- NULL
                if (self$options$stratified_cox && !is.null(self$options$strata_variable) && self$options$strata_variable != "") {
                    strata_var <- self$options$strata_variable
                    # Check if stratification variable exists
                    if (strata_var %in% names(mydata)) {
                        # Modify explanatory variable to include stratification
                        myfactor_with_strata <- paste0(myfactor, " + strata(", strata_var, ")")
                    } else {
                        warning(paste(.("Stratification variable"), strata_var, .("not found. Using standard Cox regression.")))
                        myfactor_with_strata <- myfactor
                    }
                } else {
                    myfactor_with_strata <- myfactor
                }

                private$.checkpoint()

                # Use appropriate explanatory formula (with or without stratification)
                explanatory_formula <- if (!is.null(strata_var) && strata_var %in% names(mydata)) {
                    myfactor_with_strata
                } else {
                    myfactor
                }
                
                finalfit::finalfit(
                    .data = mydata,
                    dependent = myformula,
                    explanatory = explanatory_formula,
                    metrics = TRUE
                ) -> tCox
                
                # Restore original variable names in Cox regression table
                if (!is.null(tCox[[1]]) && nrow(tCox[[1]]) > 0) {
                    labelled_data <- private$.getData()
                    original_names_mapping <- labelled_data$original_names_mapping
                    tCox[[1]] <- .restoreOriginalNamesInSurvivalTable(tCox[[1]], original_names_mapping)
                }

                tCoxtext2 <- glue::glue("
                                <br>
                                <b>Model Metrics:</b>
                                  ",
                                        unlist(tCox[[2]]),
                                        "
                                <br>
                                ")

                if (self$options$uselandmark) {

                  landmark <- jmvcore::toNumeric(self$options$landmark)

                  tCoxtext2 <- glue::glue(tCoxtext2,
                                          "Landmark time used as: ",
                                          landmark, " ",
                                          self$options$timetypeoutput, "."
                  )
                }


                self$results$tCoxtext2$setContent(tCoxtext2)


                tCox_df <-
                    tibble::as_tibble(tCox[[1]], .name_repair = "minimal") %>%
                    janitor::clean_names(dat = ., case = "snake")


                ## Cox-Regression Table ----

                coxTable <- self$results$coxTable

                data_frame <- tCox_df

                names(data_frame) <- c("Explanatory",
                                       "Levels",
                                       "all",
                                       "HR_univariable",
                                       "HR_multivariable")

                for (i in seq_along(data_frame[, 1, drop = T])) {
                    # Map columns explicitly to match schema
                    row_values <- list(
                        Explanatory = if ("Explanatory" %in% names(data_frame)) data_frame$Explanatory[i] else NA,
                        Levels = if ("Levels" %in% names(data_frame)) data_frame$Levels[i] else NA,
                        all = if ("all" %in% names(data_frame)) data_frame$all[i] else NA,
                        HR_univariable = if ("HR_univariable" %in% names(data_frame)) data_frame$HR_univariable[i] else NA
                    )
                    coxTable$addRow(rowKey = i, values = row_values)
                }


                ## Cox-Table Explanation ----

                tCox_df <-
                    tibble::as_tibble(tCox[[1]], .name_repair = "minimal") %>%
                    janitor::clean_names(dat = ., case = "snake")

                names(tCox_df) <-
                    names(data_frame) <- c("Explanatory",
                                           "Levels",
                                           "all",
                                           "HR_univariable",
                                           "HR_multivariable")


                # https://stackoverflow.com/questions/38470355/r-fill-empty-cell-with-value-of-last-non-empty-cell

                while (length(ind <-
                              which(tCox_df$Explanatory == "")) > 0) {
                    tCox_df$Explanatory[ind] <- tCox_df$Explanatory[ind - 1]
                }

                # https://stackoverflow.com/questions/51180290/mutate-by-group-in-r

                tCox_df %>%
                    dplyr::group_by(Explanatory) %>%
                    dplyr::mutate(firstlevel = dplyr::first(Levels)) %>%
                    dplyr::mutate(
                        coxdescription = glue::glue(
                            "When {Explanatory} is {Levels}, there is {HR_univariable} times risk than when {Explanatory} is {firstlevel}. \n For {Explanatory}, compared to the reference group ({firstlevel}), subjects in the {Levels} group had {HR_univariable} times the risk of experiencing the event at any given time point."

                        )
                    ) %>%
                    dplyr::filter(HR_univariable != '-') %>%
                    dplyr::pull(coxdescription) -> coxSummary




                coxSummary <- unlist(coxSummary)

                coxSummary <- c(coxSummary,
                                "A hazard ratio greater than 1 indicates increased risk, while less than 1 indicates decreased risk compared to the reference group."
                )

                self$results$coxSummary$setContent(coxSummary)

                # Add explanatory output for Cox regression
                if (self$options$showExplanations) {
                    cox_explanation_html <- '
                    <div style="margin-bottom: 20px; padding: 15px; background-color: #f0f8ff; border-left: 4px solid #4169e1;">
                        <h4 style="margin-top: 0; color: #2c3e50;">Understanding Cox Proportional Hazards Regression</h4>
                        <p style="margin-bottom: 10px;">Cox regression models the relationship between explanatory variables and the hazard (risk) of experiencing the event:</p>
                        <ul style="margin-left: 20px;">
                            <li><strong>Hazard Ratio (HR):</strong> Risk multiplier compared to reference group</li>
                            <li><strong>HR > 1:</strong> Increased risk of event occurrence</li>
                            <li><strong>HR < 1:</strong> Decreased risk of event occurrence</li>
                            <li><strong>P-value:</strong> Statistical significance of the association</li>
                            <li><strong>95% CI:</strong> Range of plausible values for the true HR</li>
                        </ul>
                        <p style="margin-bottom: 5px;"><strong>Clinical interpretation:</strong></p>
                        <ul style="margin-left: 20px;">
                            <li>HR = 2.0 means double the risk compared to reference</li>
                            <li>HR = 0.5 means half the risk compared to reference</li>
                            <li>Use for identifying prognostic factors and risk stratification</li>
                            <li>Assumes proportional hazards over time</li>
                            <li>Consider both statistical and clinical significance</li>
                        </ul>
                    </div>
                    '
                    self$results$coxRegressionExplanation$setContent(cox_explanation_html)
                }





                ## Proportional Hazards Assumption ----

                if (self$options$ph_cox) {

                    mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])

                    formula <-
                    paste('survival::Surv(',
                          mytime,
                          ',',
                          myoutcome,
                          ') ~ ',
                          myfactor)

                formula <- as.formula(formula)

                private$.checkpoint()  # Add checkpoint here


                    cox_model <- survival::coxph(formula, data = mydata)
                                                 # , na.action = na.exclude)

                    zph <- survival::cox.zph(cox_model)

                    self$results$cox_ph$setContent(print(zph))

                    # Generate enhanced PH interpretation
                    ph_interpretation <- private$.generatePHInterpretation(zph, myfactor)
                    self$results$phInterpretation$setContent(ph_interpretation)

                    image8 <- self$results$plot8
                    image8$setState(zph)
                    
                    # Add residual diagnostics if enabled
                    if (self$options$residual_diagnostics) {
                        residuals_data <- private$.calculateResiduals(cox_model, mydata)
                        
                        # Populate residuals table
                        if (!is.null(residuals_data)) {
                            residuals_table <- self$results$residualsTable
                            for (i in 1:nrow(residuals_data)) {
                                residuals_table$addRow(rowKey = i, values = list(
                                    observation = residuals_data$observation[i],
                                    martingale = residuals_data$martingale[i],
                                    deviance = residuals_data$deviance[i],
                                    score = residuals_data$score[i],
                                    schoenfeld = residuals_data$schoenfeld[i]
                                ))
                            }
                            
                            # Add residuals plot data
                            image9 <- self$results$residualsPlot
                            image9$setState(list(
                                residuals_data = residuals_data,
                                cox_model = cox_model
                            ))
                        }
                    }

                    }


            }
            
            # Calculate Residuals Function ----
            ,
            .calculateResiduals = function(cox_model, mydata) {
                tryCatch({
                    # Calculate different types of residuals
                    martingale_res <- residuals(cox_model, type = "martingale")
                    deviance_res <- residuals(cox_model, type = "deviance")
                    score_res <- residuals(cox_model, type = "score")
                    schoenfeld_res <- residuals(cox_model, type = "schoenfeld")
                    
                    # Create data frame with residuals
                    residuals_df <- data.frame(
                        observation = 1:length(martingale_res),
                        martingale = round(martingale_res, 4),
                        deviance = round(deviance_res, 4),
                        score = if (is.matrix(score_res)) round(score_res[,1], 4) else round(score_res, 4),
                        schoenfeld = if (length(schoenfeld_res) == length(martingale_res)) {
                            round(schoenfeld_res, 4)
                        } else {
                            rep(NA, length(martingale_res))
                        }
                    )
                    
                    return(residuals_df)
                    
                }, error = function(e) {
                    warning(paste(.("Error calculating residuals:"), e$message))
                    return(NULL)
                })
            }

            # Generate PH Interpretation Function ----
            ,
            .generatePHInterpretation = function(zph, covariate_name) {
                tryCatch({
                    # Extract test results
                    zph_table <- zph$table
                    p_value <- zph_table[nrow(zph_table), "p"]  # GLOBAL p-value

                    # Determine if PH assumption is violated
                    ph_violated <- p_value < 0.05

                    # Build HTML interpretation
                    html <- paste0(
                        "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                        "<h4 style='margin-top: 0; color: #2c3e50;'>Proportional Hazards Assessment</h4>"
                    )

                    # Status indicator
                    if (ph_violated) {
                        html <- paste0(html,
                            "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px; margin: 15px 0; border-radius: 4px;'>",
                            "<strong style='color: #856404;'>⚠️ WARNING: Proportional Hazards Assumption May Be Violated</strong><br/>",
                            sprintf("<p style='margin: 10px 0 0 0;'>Global test p-value = %.4f (p < 0.05 suggests violation)</p>", p_value),
                            "</div>"
                        )
                    } else {
                        html <- paste0(html,
                            "<div style='background-color: #d4edda; border-left: 4px solid #28a745; padding: 15px; margin: 15px 0; border-radius: 4px;'>",
                            "<strong style='color: #155724;'>✓ Proportional Hazards Assumption Appears Satisfied</strong><br/>",
                            sprintf("<p style='margin: 10px 0 0 0;'>Global test p-value = %.4f (p ≥ 0.05)</p>", p_value),
                            "</div>"
                        )
                    }

                    # Educational content
                    html <- paste0(html,
                        "<div style='margin: 20px 0;'>",
                        "<h5 style='color: #2c3e50; margin-bottom: 10px;'>Understanding the Test:</h5>",
                        "<ul style='line-height: 1.8;'>",
                        "<li><strong>Null Hypothesis:</strong> The hazard ratio remains constant over time (PH assumption holds)</li>",
                        "<li><strong>Test Method:</strong> Schoenfeld residuals correlation with time</li>",
                        "<li><strong>Interpretation:</strong> p < 0.05 suggests the effect of '", covariate_name, "' changes over time</li>",
                        "</ul>",
                        "</div>"
                    )

                    # Recommendations if violated
                    if (ph_violated) {
                        html <- paste0(html,
                            "<div style='background-color: #e7f3ff; padding: 15px; border-radius: 4px; margin: 15px 0;'>",
                            "<h5 style='color: #0056b3; margin-top: 0;'>📋 Recommended Solutions:</h5>",
                            "<ol style='line-height: 1.8; margin: 10px 0;'>",
                            "<li><strong>Stratified Cox Model:</strong> Stratify by '", covariate_name, "' if it has few categories",
                            "<pre style='background: #fff; padding: 10px; margin: 5px 0; border-left: 3px solid #0056b3;'>",
                            "survival::coxph(Surv(time, status) ~ other_vars + strata(", covariate_name, "))",
                            "</pre></li>",
                            "<li><strong>Time-Dependent Coefficients:</strong> Add interaction with time",
                            "<pre style='background: #fff; padding: 10px; margin: 5px 0; border-left: 3px solid #0056b3;'>",
                            "survival::coxph(Surv(time, status) ~ ", covariate_name, " + tt(", covariate_name, "), tt = function(x, t, ...) x * t)",
                            "</pre></li>",
                            "<li><strong>Alternative Approaches:</strong>",
                            "<ul style='margin: 5px 0;'>",
                            "<li>Restricted Mean Survival Time (RMST) analysis - does not assume proportional hazards</li>",
                            "<li>Accelerated failure time (AFT) models - different parametric assumption</li>",
                            "<li>Landmark analysis - analyze survival from specific time points</li>",
                            "</ul></li>",
                            "</ol>",
                            "</div>"
                        )
                    } else {
                        html <- paste0(html,
                            "<div style='background-color: #e8f5e9; padding: 15px; border-radius: 4px; margin: 15px 0;'>",
                            "<h5 style='color: #2e7d32; margin-top: 0;'>✓ Next Steps:</h5>",
                            "<p style='margin: 5px 0;'>The Cox proportional hazards model appears appropriate for your data. ",
                            "You can proceed with confidence in interpreting the hazard ratios as constant effects over time.</p>",
                            "<p style='margin: 10px 0 0 0;'><em>Note: Also examine the Schoenfeld residual plot above for visual confirmation.</em></p>",
                            "</div>"
                        )
                    }

                    # Close main div
                    html <- paste0(html, "</div>")

                    return(html)

                }, error = function(e) {
                    return(paste0("<p>Error generating PH interpretation: ", e$message, "</p>"))
                })
            }

            # Export Survival Data Function ----
            ,
            .exportSurvivalData = function(results) {
                # Export Kaplan-Meier estimates for external analysis
                if (!self$options$export_survival_data || !self$results$survivalExport$isNotFilled()) {
                    return()
                }
                
                tryCatch({
                    mytime <- results$name1time
                    myoutcome <- results$name2outcome
                    myfactor <- results$name3explanatory
                    mydata <- results$cleanData
                    
                    mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])
                    
                    # Escape variable names for safe formula construction
                    escaped_mytime <- .escapeVariableNames(mytime)
                    escaped_myoutcome <- .escapeVariableNames(myoutcome)
                    escaped_myfactor <- .escapeVariableNames(myfactor)
                    
                    formula <- paste('survival::Surv(', escaped_mytime, ',', escaped_myoutcome, ') ~ ', escaped_myfactor)
                    formula <- as.formula(formula)
                    
                    private$.checkpoint()
                    
                    km_fit <- survival::survfit(formula, data = mydata)
                    
                    # Generate time points for export (every unit from 0 to max time)
                    max_time <- max(mydata[[mytime]], na.rm = TRUE)
                    export_times <- seq(0, max_time, by = max(1, floor(max_time/100)))
                    
                    # Get survival estimates at specified times
                    km_export <- summary(km_fit, times = export_times, extend = TRUE)
                    
                    # Create export data frame
                    export_df <- data.frame(
                        time = km_export$time,
                        strata = km_export$strata,
                        n_risk = km_export$n.risk,
                        n_event = km_export$n.event,
                        survival = round(km_export$surv, 4),
                        std_error = round(km_export$std.err, 4),
                        lower_ci = round(km_export$lower, 4),
                        upper_ci = round(km_export$upper, 4)
                    )
                    
                    # Clean up strata names
                    export_df$strata <- gsub(paste0(myfactor, "="), "", export_df$strata)
                    
                    # Add to results as exportable data
                    self$results$survivalExport$setRowNums(rownames(export_df))
                    self$results$survivalExport$setValues(export_df)
                    
                    # Create summary for user
                    export_summary <- paste0(
                        "<h4>Survival Data Export Summary</h4>",
                        "<p>Exported ", nrow(export_df), " time points covering ", length(unique(export_df$strata)), " groups.</p>",
                        "<p>Time range: 0 to ", round(max_time, 1), " ", self$options$timetypeoutput, "</p>",
                        "<p>Data includes: time, group, number at risk, events, survival probability, standard error, and 95% confidence intervals.</p>"
                    )
                    
                    self$results$survivalExportSummary$setContent(export_summary)
                    
                }, error = function(e) {
                    warning(paste(.("Error exporting survival data:"), e$message))
                })
            }


            # Survival Table Function ----
            ,
            .survTable = function(results) {
                mytime <- results$name1time
                myoutcome <- results$name2outcome
                myfactor <- results$name3explanatory
                myexplanatory_labelled <- results$myexplanatory_labelled

                mydata <- results$cleanData

                mydata[[mytime]] <-
                    jmvcore::toNumeric(mydata[[mytime]])

                ## Median Survival Table ----

                formula <-
                    paste('survival::Surv(',
                          mytime,
                          ',',
                          myoutcome,
                          ') ~ ',
                          myfactor)

                formula <- as.formula(formula)

                private$.checkpoint()  # Add checkpoint here

                km_fit <- survival::survfit(formula, data = mydata)

                utimes <- self$options$cutp

                utimes <- strsplit(utimes, ",")
                utimes <- purrr::reduce(utimes, as.vector)
                utimes <- as.numeric(utimes)

                if (length(utimes) == 0) {
                    utimes <- c(12, 36, 60)
                }

                private$.checkpoint()  # Add checkpoint here

                km_fit_summary <- summary(km_fit, times = utimes, extend = TRUE)

                km_fit_df <-
                    as.data.frame(km_fit_summary[c("strata",
                                                   "time",
                                                   "n.risk",
                                                   "n.event",
                                                   "surv",
                                                   "std.err",
                                                   "lower",
                                                   "upper")])

                # self$results$tableview$setContent(km_fit_df)


                km_fit_df[, 1] <- gsub(
                    pattern = "thefactor=",
                    replacement = paste0(self$options$explanatory, " "),
                    x = km_fit_df[, 1]
                )


                km_fit_df2 <- km_fit_df

                km_fit_df[, 1] <- gsub(
                    pattern = paste0(myexplanatory_labelled,"="),
                    replacement = paste0(
                        # self$options$explanatory,
                        ""),
                    x = km_fit_df[, 1]
                )

                survTable <- self$results$survTable

                data_frame <- km_fit_df
                for (i in seq_along(data_frame[, 1, drop = T])) {
                    # Map columns explicitly to match schema
                    row_values <- list(
                        strata = if ("strata" %in% names(data_frame)) data_frame$strata[i] else NA,
                        time = if ("time" %in% names(data_frame)) data_frame$time[i] else NA,
                        n.risk = if ("n.risk" %in% names(data_frame)) data_frame$`n.risk`[i] else NA,
                        n.event = if ("n.event" %in% names(data_frame)) data_frame$`n.event`[i] else NA,
                        surv = if ("surv" %in% names(data_frame)) data_frame$surv[i] else NA,
                        lower = if ("lower" %in% names(data_frame)) data_frame$lower[i] else NA,
                        upper = if ("upper" %in% names(data_frame)) data_frame$upper[i] else NA
                    )
                    survTable$addRow(rowKey = i, values = row_values)
                }


                ## survTableSummary 1,3,5-yr survival summary ----

                km_fit_df2[, 1] <- gsub(
                    pattern = paste0(myexplanatory_labelled,"="),
                    replacement = paste0(self$options$explanatory, " is "),
                    x = km_fit_df2[, 1]
                )

                km_fit_df2 %>%
                    dplyr::mutate(
                        description =
                            glue::glue(
                                "When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]. \n For the {strata} group, the estimated probability of surviving beyond {time} months was {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]. \n At this time point, there were {n.risk} subjects still at risk and {n.event} events had occurred in this group."

                            )
                    ) %>%
                    dplyr::select(description) %>%
                    dplyr::pull(.) -> survTableSummary

                survTableSummaryWithNote <- c(survTableSummary,
                                              "Note: Confidence intervals for survival probabilities are calculated using the log-log transformation method rather than the plain Greenwood formula for better performance with censored survival data.")
                
                self$results$survTableSummary$setContent(survTableSummaryWithNote)

            }


            # Pairwise Function ----
            ,
            .pairwise = function(results) {
                ##  pairwise comparison ----

                mytime <- results$name1time
                myoutcome <- results$name2outcome
                myfactor <- results$name3explanatory

                mydata <- results$cleanData

                mydata[[mytime]] <-
                    jmvcore::toNumeric(mydata[[mytime]])

                ## Median Survival Table ----

                formula <-
                    paste('survival::Surv(',
                          mytime,
                          ',',
                          myoutcome,
                          ') ~ ',
                          myfactor)

                formula_p <- as.formula(formula)

                padjustmethod <-
                    jmvcore::constructFormula(terms = self$options$padjustmethod)

                private$.checkpoint()

                results_pairwise <-
                    survminer::pairwise_survdiff(formula = formula_p,
                                                 data = mydata,
                                                 p.adjust.method = padjustmethod)


                mypairwise2 <-
                    as.data.frame(results_pairwise[["p.value"]]) %>%
                    tibble::rownames_to_column(.data = .) %>%
                    tidyr::pivot_longer(data = ., cols = -rowname) %>%
                    dplyr::filter(complete.cases(.))



                ## Pairwise Table ----

                pairwiseTable <- self$results$pairwiseTable

                data_frame <- mypairwise2
                for (i in seq_along(data_frame[, 1, drop = T])) {

                    if (i %% 10 == 0) {  # Optional: add checkpoints periodically for larger tables
                        private$.checkpoint()  # Add checkpoint here
                    }

                    # Map columns explicitly to match schema
                    row_values <- list(
                        rowname = if ("rowname" %in% names(data_frame)) data_frame$rowname[i] else NA,
                        name = if ("name" %in% names(data_frame)) data_frame$name[i] else NA,
                        value = if ("value" %in% names(data_frame)) data_frame$value[i] else NA
                    )
                    pairwiseTable$addRow(rowKey = i, values = row_values)
                }

                thefactor <-
                    jmvcore::constructFormula(terms = self$options$explanatory)

                # Get original display name for table title
                labelled_data <- private$.getData()
                original_names_mapping <- labelled_data$original_names_mapping
                title2 <- .getDisplayName(self$options$explanatory, original_names_mapping)

                pairwiseTable$setTitle(paste0(.('Pairwise Comparisons '), title2))

                pairwiseTable$setNote(
                    key = padjustmethod,
                    note = paste0("p-value adjustement method: ",
                           padjustmethod)
                )

                mypairwise2 %>%
                    dplyr::mutate(
                        description =
                            glue::glue(
                                "The difference of ",
                                title2,
                                " between {rowname} and {name}",
                                " has a p-value of {format.pval(value, digits = 3, eps = 0.001)}.",

                                "The survival difference between {rowname} and {name} groups was tested using a log-rank test. The p-value of {format.pval(value, digits = 3, eps = 0.001)} {ifelse(value < 0.05, 'indicates a statistically significant difference', 'suggests no statistically significant difference')} in survival between these groups (using {padjustmethod} adjustment for multiple comparisons)."



                            )
                    ) %>%
                    dplyr::pull(description) -> pairwiseSummary

                pairwiseSummary <- unlist(pairwiseSummary)


                self$results$pairwiseSummary$setContent(pairwiseSummary)


                if (dim(mypairwise2)[1] == 1) {
                    self$results$pairwiseTable$setVisible(FALSE)

                    pairwiseSummary <-
                        "No pairwise comparison when explanatory variable has < 3 levels."
                    self$results$pairwiseSummary$setContent(pairwiseSummary)

                }


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

                # Ensure time is numeric
                mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])

                # Get total observed time
                total_time <- sum(mydata[[mytime]])

                # Get total events
                # FIX: Count events properly - any non-zero value is an event
                # In competing risk (0/1/2), this counts both event of interest and competing events
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

                # FIX: Add group-stratified person-time analysis
                # If explanatory variable exists, calculate person-time for each group
                myexplanatory <- results$name3explanatory
                if (!is.null(myexplanatory) && myexplanatory %in% names(mydata)) {
                    # Get unique groups
                    groups <- unique(mydata[[myexplanatory]])
                    groups <- groups[!is.na(groups)]  # Remove NA groups

                    rowKey_counter <- 2  # Start after overall row

                    for (group in groups) {
                        # Filter data for this group
                        group_data <- mydata[mydata[[myexplanatory]] == group, ]

                        if (nrow(group_data) > 0) {
                            # Calculate group-specific metrics
                            group_time <- sum(group_data[[mytime]], na.rm = TRUE)
                            group_events <- sum(group_data[[myoutcome]] >= 1, na.rm = TRUE)

                            # Calculate group incidence rate
                            if (group_time > 0) {
                                group_rate <- (group_events / group_time) * rate_multiplier

                                # Calculate confidence intervals using Poisson exact method
                                if (group_events > 0) {
                                    group_ci_lower <- (stats::qchisq(0.025, 2*group_events) / 2) / group_time * rate_multiplier
                                    group_ci_upper <- (stats::qchisq(0.975, 2*(group_events + 1)) / 2) / group_time * rate_multiplier
                                } else {
                                    group_ci_lower <- 0
                                    group_ci_upper <- (stats::qchisq(0.975, 2) / 2) / group_time * rate_multiplier
                                }

                                # Add to personTimeTable with group label
                                self$results$personTimeTable$addRow(rowKey=rowKey_counter, values=list(
                                    interval=paste0("Group: ", as.character(group)),
                                    events=group_events,
                                    person_time=round(group_time, 2),
                                    rate=round(group_rate, 2),
                                    rate_ci_lower=round(group_ci_lower, 2),
                                    rate_ci_upper=round(group_ci_upper, 2)
                                ))

                                rowKey_counter <- rowKey_counter + 1
                            }
                        }
                    }
                }

                # Parse time intervals for stratified analysis
                time_intervals <- as.numeric(unlist(strsplit(self$options$time_intervals, ",")))
                time_intervals <- sort(unique(time_intervals))

                if (length(time_intervals) > 0) {
                    # Create time intervals
                    breaks <- c(0, time_intervals, max(mydata[[mytime]]) * 1.1)

                    # Loop through intervals
                    for (i in 1:(length(breaks)-1)) {
                        start_time <- breaks[i]
                        end_time <- breaks[i+1]

                        # Add checkpoint for responsiveness
                        if (i %% 5 == 0) {
                            private$.checkpoint()
                        }

                        # Filter data for this interval
                        if (i == 1) {
                            # For first interval, include patients from the beginning
                            interval_data <- mydata
                            # But truncate follow-up time to the interval end
                            follow_up_times <- pmin(mydata[[mytime]], end_time)
                            # Count only events that occurred within this interval
                            # FIX: Count events consistently with overall count
                            # For competing risk, this counts all events (both event of interest and competing)
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

                            # Count only events that occurred within this interval
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
                summary_html <- glue::glue("
<h4>Person-Time Analysis Summary</h4>
<p>Total follow-up time: <b>{round(total_time, 1)} {time_unit}</b></p>
<p>Number of events: <b>{total_events}</b></p>
<p>Overall incidence rate: <b>{round(overall_rate, 2)}</b> per {rate_multiplier} {time_unit} [95% CI: {round(ci_lower, 2)}-{round(ci_upper, 2)}]</p>
<p>This represents the rate at which events occurred in your study population. The incidence rate is calculated as the number of events divided by the total person-time at risk.</p>
")

                self$results$personTimeSummary$setContent(summary_html)

                # Add explanatory output for person-time analysis
                if (self$options$showExplanations) {
                    person_time_explanation_html <- '
                    <div style="margin-bottom: 20px; padding: 15px; background-color: #f0f8ff; border-left: 4px solid #4169e1;">
                        <h4 style="margin-top: 0; color: #2c3e50;">Understanding Person-Time Analysis</h4>
                        <p style="margin-bottom: 10px;">Person-time analysis calculates incidence rates by accounting for the total time each patient was at risk:</p>
                        <ul style="margin-left: 20px;">
                            <li><strong>Person-Time:</strong> Sum of individual follow-up periods for all patients in each group</li>
                            <li><strong>Incidence Rate:</strong> Events per unit time (e.g., per 100 person-years)</li>
                            <li><strong>Rate Ratio:</strong> Comparison of incidence rates between groups</li>
                            <li><strong>95% CI:</strong> Confidence interval calculated using exact Poisson method</li>
                        </ul>
                        <p style="margin-bottom: 5px;"><strong>Clinical interpretation:</strong></p>
                        <ul style="margin-left: 20px;">
                            <li>Person-time analysis handles varying follow-up durations effectively</li>
                            <li>Higher rates indicate more frequent event occurrence in that group</li>
                            <li>Rate ratios >1 suggest increased risk compared to reference group</li>
                            <li>Use for comparing event rates between different patient populations</li>
                            <li>Particularly useful when follow-up times vary significantly between groups</li>
                        </ul>
                    </div>
                    '
                    self$results$personTimeExplanation$setContent(person_time_explanation_html)
                }
            }







            # Survival Curve ----
            ,
            .plot = function(image, ggtheme, theme, ...) {
                sc <- self$options$sc

                if (!sc)
                    return()

                results <- image$state

                if (is.null(results)) {
                    return()
                }

                mytime <- results$name1time
                mytime <- jmvcore::constructFormula(terms = mytime)

                myoutcome <- results$name2outcome
                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)


                myfactor <- results$name3explanatory
                myfactor <-
                    jmvcore::constructFormula(terms = myfactor)

                plotData <- results$cleanData

                plotData[[mytime]] <-
                    jmvcore::toNumeric(plotData[[mytime]])

                # Escape variable names for safe formula construction
                escaped_mytime <- .escapeVariableNames(mytime)
                escaped_myoutcome <- .escapeVariableNames(myoutcome)
                
                myformula <-
                    paste("survival::Surv(", escaped_mytime, ",", escaped_myoutcome, ")")

                # Get original display name for plot title
                labelled_data <- private$.getData()
                original_names_mapping <- labelled_data$original_names_mapping
                title2 <- .getDisplayName(myfactor, original_names_mapping)

                plot <- plotData %>%
                    finalfit::surv_plot(
                        .data = .,
                        dependent = myformula,
                        explanatory = myfactor,
                        xlab = paste0('Time (', self$options$timetypeoutput, ')'),
                        pval = self$options$pplot,
                        pval.method	= self$options$pplot,
                        legend = 'none',
                        break.time.by = self$options$byplot,
                        xlim = c(0, self$options$endplot),
                        ylim = c(
                            self$options$ybegin_plot,
                            self$options$yend_plot),
                        title = paste0(.("Survival curves for "), title2),
                        subtitle = .("Based on Kaplan-Meier estimates"),
                        risk.table = self$options$risktable,
                        conf.int = self$options$ci95,
                        censor = self$options$censored,
                        surv.median.line = self$options$medianline
                    )

                # plot <- plot + ggtheme

                print(plot)
                TRUE

            }



            # Cumulative Events ----
            # https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf
            ,
            .plot2 = function(image2, ggtheme, theme, ...) {
                ce <- self$options$ce

                if (!ce)
                    return()

                results <- image2$state

                if (is.null(results)) {
                    return()
                }

                mytime <- results$name1time
                mytime <- jmvcore::constructFormula(terms = mytime)

                myoutcome <- results$name2outcome
                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)


                myfactor <- results$name3explanatory
                myfactor <-
                    jmvcore::constructFormula(terms = myfactor)

                plotData <- results$cleanData

                plotData[[mytime]] <-
                    jmvcore::toNumeric(plotData[[mytime]])

                # Escape variable names for safe formula construction
                escaped_mytime <- .escapeVariableNames(mytime)
                escaped_myoutcome <- .escapeVariableNames(myoutcome)
                
                myformula <-
                    paste("survival::Surv(", escaped_mytime, ",", escaped_myoutcome, ")")

                # Get original display name for plot title
                labelled_data <- private$.getData()
                original_names_mapping <- labelled_data$original_names_mapping
                title2 <- .getDisplayName(myfactor, original_names_mapping)

                plot2 <- plotData %>%
                    finalfit::surv_plot(
                        .data = .,
                        dependent = myformula,
                        explanatory = myfactor,
                        xlab = paste0('Time (', self$options$timetypeoutput, ')'),
                        pval = self$options$pplot,
                        pval.method	= self$options$pplot,
                        legend = 'none',
                        break.time.by = self$options$byplot,
                        xlim = c(0, self$options$endplot),
                        ylim = c(
                            self$options$ybegin_plot,
                            self$options$yend_plot),
                        title = paste0(.("Cumulative Events "), title2),
                        fun = "event",
                        risk.table = self$options$risktable,
                        conf.int = self$options$ci95,
                        censor = self$options$censored,
                        surv.median.line = self$options$medianline

                    )

                print(plot2)
                TRUE

            }



            # Cumulative Hazard ----
            ,
            .plot3 = function(image3, ggtheme, theme, ...) {
                ch <- self$options$ch

                if (!ch)
                    return()

                results <- image3$state

                if (is.null(results)) {
                    return()
                }

                mytime <- results$name1time
                mytime <- jmvcore::constructFormula(terms = mytime)

                myoutcome <- results$name2outcome
                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)


                myfactor <- results$name3explanatory
                myfactor <-
                    jmvcore::constructFormula(terms = myfactor)

                plotData <- results$cleanData

                plotData[[mytime]] <-
                    jmvcore::toNumeric(plotData[[mytime]])

                # Escape variable names for safe formula construction
                escaped_mytime <- .escapeVariableNames(mytime)
                escaped_myoutcome <- .escapeVariableNames(myoutcome)
                
                myformula <-
                    paste("survival::Surv(", escaped_mytime, ",", escaped_myoutcome, ")")

                # Get original display name for plot title
                labelled_data <- private$.getData()
                original_names_mapping <- labelled_data$original_names_mapping
                title2 <- .getDisplayName(myfactor, original_names_mapping)


                plot3 <- plotData %>%
                    finalfit::surv_plot(
                        .data = .,
                        dependent = myformula,
                        explanatory = myfactor,
                        xlab = paste0('Time (', self$options$timetypeoutput, ')'),
                        pval = self$options$pplot,
                        pval.method	= self$options$pplot,
                        legend = 'none',
                        break.time.by = self$options$byplot,
                        xlim = c(0, self$options$endplot),
                        ylim = c(
                            self$options$ybegin_plot,
                            self$options$yend_plot),
                        title = paste0(.("Cumulative Hazard "), title2),
                        fun = "cumhaz",
                        risk.table = self$options$risktable,
                        conf.int = self$options$ci95,
                        censor = self$options$censored,
                        surv.median.line = self$options$medianline

                    )


                print(plot3)
                TRUE
            }
            
            # Log-Log Survival Plot (for PH assumption) ----
            ,
            .plot7 = function(image7, ggtheme, theme, ...) {
                loglog <- self$options$loglog

                if (!loglog)
                    return()

                results <- image7$state

                if (is.null(results)) {
                    return()
                }

                mytime <- results$name1time
                mytime <- jmvcore::constructFormula(terms = mytime)

                myoutcome <- results$name2outcome
                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)

                myfactor <- results$name3explanatory
                myfactor <-
                    jmvcore::constructFormula(terms = myfactor)

                plotData <- results$cleanData

                plotData[[mytime]] <-
                    jmvcore::toNumeric(plotData[[mytime]])

                myformula <-
                    paste('survival::Surv(',
                          mytime,
                          ',',
                          myoutcome,
                          ') ~ ',
                          myfactor)

                myformula <- as.formula(myformula)

                km_fit <-
                    survival::survfit(myformula, data = plotData)

                # Get original display name for plot title
                labelled_data <- private$.getData()
                original_names_mapping <- labelled_data$original_names_mapping
                title2 <- .getDisplayName(myfactor, original_names_mapping)

                # Create log-log plot
                tryCatch({
                    plot7 <- plotData %>%
                        finalfit::surv_plot(
                            .data = .,
                            dependent = paste("Surv(", escaped_mytime, ",", escaped_myoutcome, ")"),
                            explanatory = myfactor,
                            xlab = paste0('log(Time) (', self$options$timetypeoutput, ')'),
                            ylab = 'log(-log(Survival))',
                            title = paste0(.("Log-Log Plot for "), title2),
                            subtitle = .("Assessment of Proportional Hazards Assumption"),
                            fun = function(x) log(-log(x)),
                            legend = 'right'
                        ) +
                        ggplot2::scale_x_log10()

                    print(plot7)
                    TRUE
                    
                }, error = function(e) {
                    # Fallback simple log-log plot
                    survival_times <- seq(min(plotData[[mytime]], na.rm = TRUE), 
                                        max(plotData[[mytime]], na.rm = TRUE), 
                                        length.out = 100)
                    
                    km_summary <- summary(km_fit, times = survival_times, extend = TRUE)
                    
                    plot_data <- data.frame(
                        time = km_summary$time,
                        surv = km_summary$surv,
                        strata = km_summary$strata
                    )
                    
                    # Remove invalid values
                    plot_data <- plot_data[plot_data$surv > 0 & plot_data$surv < 1, ]
                    plot_data$log_time <- log(plot_data$time)
                    plot_data$log_log_surv <- log(-log(plot_data$surv))
                    
                    plot7 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = log_time, y = log_log_surv, color = strata)) +
                        ggplot2::geom_line(size = 1) +
                        ggplot2::labs(
                            x = paste0('log(Time) (', self$options$timetypeoutput, ')'),
                            y = 'log(-log(Survival))',
                            title = paste0(.("Log-Log Plot for "), title2),
                            subtitle = .("Parallel lines suggest proportional hazards")
                        ) +
                        ggtheme
                    
                    print(plot7)
                    TRUE
                })
            }


            # KMunicate Style ----
            ,
            .plot6 = function(image6, ggtheme, theme, ...) {
                kmunicate <- self$options$kmunicate

                if (!kmunicate)
                    return()

                results <- image6$state

                if (is.null(results)) {
                    return()
                }

                mytime <- results$name1time
                mytime <- jmvcore::constructFormula(terms = mytime)

                myoutcome <- results$name2outcome
                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)


                myfactor <- results$name3explanatory
                myfactor <-
                    jmvcore::constructFormula(terms = myfactor)

                plotData <- results$cleanData

                plotData[[mytime]] <-
                    jmvcore::toNumeric(plotData[[mytime]])


                # Get original display name for plot title
                labelled_data <- private$.getData()
                original_names_mapping <- labelled_data$original_names_mapping
                title2 <- .getDisplayName(myfactor, original_names_mapping)


                myformula <-
                    paste('survival::Surv(',
                          mytime,
                          ',',
                          myoutcome,
                          ') ~ ',
                          myfactor)

                myformula <- as.formula(myformula)

                km_fit <-
                    survival::survfit(myformula, data = plotData)

                time_scale <-
                    seq(0, self$options$endplot, by = self$options$byplot)


                plot6 <-
                    KMunicate::KMunicate(
                        fit = km_fit,
                        time_scale = time_scale,
                        .xlab = paste0('Time in ', self$options$timetypeoutput)
                    )


                print(plot6)
                TRUE

            }


            # Residuals Plot ----
            ,
            .plot9 = function(image9, ggtheme, theme, ...) {
                residual_diagnostics <- self$options$residual_diagnostics

                if (!residual_diagnostics)
                    return()

                plot_state <- image9$state

                if (is.null(plot_state) || is.null(plot_state$residuals_data)) {
                    return()
                }

                residuals_data <- plot_state$residuals_data
                
                tryCatch({
                    # Create residuals plot
                    plot9 <- ggplot2::ggplot(residuals_data, ggplot2::aes(x = observation)) +
                        ggplot2::geom_point(ggplot2::aes(y = martingale), color = "blue", alpha = 0.6) +
                        ggplot2::geom_smooth(ggplot2::aes(y = martingale), method = "loess", se = TRUE, color = "red") +
                        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
                        ggplot2::labs(
                            x = "Observation Index",
                            y = "Martingale Residuals",
                            title = .("Cox Model Residual Diagnostics"),
                            subtitle = .("Martingale residuals should be randomly scattered around zero")
                        ) +
                        ggtheme
                    
                    print(plot9)
                    TRUE
                    
                }, error = function(e) {
                    warning(paste(.("Error creating residuals plot:"), e$message))
                    FALSE
                })
            }
            
            # cox.zph ----
            ,
            .plot8 = function(image8, ggtheme, theme, ...) {

                ph_cox <- self$options$ph_cox

                if (!ph_cox)
                    return()

                zph <- image8$state

                if (is.null(zph)) {
                    return()
                }

                plot8 <- plot(zph)

                print(plot8)
                TRUE

            }
            
            # Set Explanation Content Helper ----
            ,
            .setExplanationContent = function(outputName, htmlContent) {
                if (self$options$showExplanations && !is.null(self$results[[outputName]])) {
                    self$results[[outputName]]$setContent(htmlContent)
                }
            }
            
            # Populate Explanations ----
            ,
            .populateExplanations = function() {
                if (!self$options$showExplanations) return()
                
                # Median Survival Explanation
                private$.setExplanationContent("medianSurvivalExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: #e8f4f8; border-left: 4px solid #17a2b8;">
                    <h4>📊 Understanding Your Median Survival Results</h4>
                    <p><strong>What is Median Survival?</strong> The median survival time tells you when half of your study population experienced the event you are studying.</p>
                    
                    <h5>🔍 How to Read Your Results:</h5>
                    <table style="width:100%; margin: 10px 0;">
                        <tr style="background-color: #f8f9fa;">
                            <td style="padding: 5px;"><strong>Records</strong></td>
                            <td style="padding: 5px;">Total number of patients in your study group</td>
                        </tr>
                        <tr>
                            <td style="padding: 5px;"><strong>Events</strong></td>
                            <td style="padding: 5px;">Number of patients who experienced the outcome (e.g., death, recurrence)</td>
                        </tr>
                        <tr style="background-color: #f8f9fa;">
                            <td style="padding: 5px;"><strong>Median</strong></td>
                            <td style="padding: 5px;">The time when 50% of patients had the event</td>
                        </tr>
                        <tr>
                            <td style="padding: 5px;"><strong>95% CI</strong></td>
                            <td style="padding: 5px;">Range where we are 95% confident the true median lies</td>
                        </tr>
                        <tr style="background-color: #f8f9fa;">
                            <td style="padding: 5px;"><strong>Not Reached (NR)</strong></td>
                            <td style="padding: 5px;">Good news! Less than half the patients had the event</td>
                        </tr>
                    </table>
                    
                    <h5>💡 Practical Example:</h5>
                    <p>If median survival = 24 months with 95% CI (18-30):</p>
                    <ul>
                        <li>Half your patients survived longer than 24 months</li>
                        <li>You can be 95% confident the true median is between 18 and 30 months</li>
                        <li>This helps predict typical patient outcomes for treatment planning</li>
                    </ul>
                    
                    <h5>⚠️ Important Notes:</h5>
                    <ul>
                        <li>Median is better than mean for survival data as it is not affected by extreme values</li>
                        <li>Wide confidence intervals suggest more uncertainty (often due to small sample size)</li>
                        <li>"Not Reached" is often a positive finding indicating good survival</li>
                    </ul>
                </div>
                ')
                
                # Cox Regression Explanation
                private$.setExplanationContent("coxRegressionExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: #fff3cd; border-left: 4px solid #ffc107;">
                    <h4>⚖️ Understanding Your Cox Regression Results</h4>
                    <p><strong>What is Cox Regression?</strong> This analysis compares the risk of experiencing an event between different groups, adjusting for time.</p>
                    
                    <h5>🎯 Key Metric: Hazard Ratio (HR)</h5>
                    <p>The hazard ratio tells you how much more (or less) likely one group is to experience the event compared to another.</p>
                    
                    <table style="width:100%; margin: 10px 0; border-collapse: collapse;">
                        <tr style="background-color: #f8f9fa;">
                            <th style="padding: 8px; text-align: left;">HR Value</th>
                            <th style="padding: 8px; text-align: left;">Meaning</th>
                            <th style="padding: 8px; text-align: left;">Plain English</th>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;"><strong>HR = 1.0</strong></td>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;">No difference</td>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;">Groups have equal risk</td>
                        </tr>
                        <tr style="background-color: #ffebee;">
                            <td style="padding: 8px;"><strong>HR = 2.0</strong></td>
                            <td style="padding: 8px;">Doubled risk</td>
                            <td style="padding: 8px;">Group has 2× higher risk of event</td>
                        </tr>
                        <tr style="background-color: #e8f5e9;">
                            <td style="padding: 8px;"><strong>HR = 0.5</strong></td>
                            <td style="padding: 8px;">Halved risk</td>
                            <td style="padding: 8px;">Group has 50% lower risk of event</td>
                        </tr>
                        <tr style="background-color: #ffebee;">
                            <td style="padding: 8px;"><strong>HR = 3.0</strong></td>
                            <td style="padding: 8px;">Tripled risk</td>
                            <td style="padding: 8px;">Group has 3× higher risk of event</td>
                        </tr>
                        <tr style="background-color: #e8f5e9;">
                            <td style="padding: 8px;"><strong>HR = 0.25</strong></td>
                            <td style="padding: 8px;">Quarter risk</td>
                            <td style="padding: 8px;">Group has 75% lower risk of event</td>
                        </tr>
                    </table>
                    
                    <h5>📈 Statistical Significance:</h5>
                    <ul>
                        <li><strong>P-value < 0.05:</strong> The difference is statistically significant (likely real, not due to chance)</li>
                        <li><strong>95% CI includes 1.0:</strong> The difference is NOT statistically significant</li>
                        <li><strong>95% CI excludes 1.0:</strong> The difference IS statistically significant</li>
                    </ul>
                    
                    <h5>💊 Clinical Example:</h5>
                    <p>If Treatment A vs Treatment B shows HR = 0.60 (95% CI: 0.40-0.85, p=0.004):</p>
                    <ul style="background-color: #e8f5e9; padding: 10px; border-radius: 5px;">
                        <li>Treatment A reduces risk by 40% compared to Treatment B</li>
                        <li>The result is statistically significant (p < 0.05 and CI excludes 1.0)</li>
                        <li>We can be 95% confident the true risk reduction is between 15% and 60%</li>
                    </ul>
                    
                    <h5>⚠️ Important Considerations:</h5>
                    <ul>
                        <li>HR assumes proportional hazards (risk ratio stays constant over time)</li>
                        <li>Wide confidence intervals suggest uncertainty (often from small sample size)</li>
                        <li>Statistical significance does not always mean clinical importance</li>
                    </ul>
                </div>
                ')
                
                # Survival Tables Explanation
                private$.setExplanationContent("survivalTablesExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: #d4edda; border-left: 4px solid #28a745;">
                    <h4>📋 Understanding Your Survival Probability Tables</h4>
                    <p><strong>What are Survival Tables?</strong> These tables show the percentage of patients surviving at key time points, which are standard benchmarks in medical research.</p>
                    
                    <h5>📊 How to Read the Table Columns:</h5>
                    <table style="width:100%; margin: 10px 0; border-collapse: collapse;">
                        <tr style="background-color: #f8f9fa;">
                            <th style="padding: 8px; text-align: left;">Column</th>
                            <th style="padding: 8px; text-align: left;">What It Means</th>
                            <th style="padding: 8px; text-align: left;">Example</th>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;"><strong>Time</strong></td>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;">Time point of measurement</td>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;">12 months (1 year)</td>
                        </tr>
                        <tr style="background-color: #f8f9fa;">
                            <td style="padding: 8px;"><strong>n.risk</strong></td>
                            <td style="padding: 8px;">Patients still being followed</td>
                            <td style="padding: 8px;">85 of 100 still in study</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px;"><strong>n.event</strong></td>
                            <td style="padding: 8px;">Cumulative events occurred</td>
                            <td style="padding: 8px;">15 events by this time</td>
                        </tr>
                        <tr style="background-color: #f8f9fa;">
                            <td style="padding: 8px;"><strong>survival</strong></td>
                            <td style="padding: 8px;">% surviving past this time</td>
                            <td style="padding: 8px;">85% still event-free</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px;"><strong>95% CI</strong></td>
                            <td style="padding: 8px;">Confidence range</td>
                            <td style="padding: 8px;">True rate likely 78-92%</td>
                        </tr>
                    </table>
                    
                    <h5>🏥 Common Medical Benchmarks:</h5>
                    <div style="background-color: #e3f2fd; padding: 10px; border-radius: 5px; margin: 10px 0;">
                        <ul style="margin: 5px 0;">
                            <li><strong>1-year survival:</strong> Short-term prognosis indicator</li>
                            <li><strong>3-year survival:</strong> Medium-term outcome measure</li>
                            <li><strong>5-year survival:</strong> Standard long-term benchmark (especially in cancer)</li>
                            <li><strong>10-year survival:</strong> Very long-term outcomes (chronic diseases)</li>
                        </ul>
                    </div>
                    
                    <h5>💡 Practical Example:</h5>
                    <p>If your table shows:</p>
                    <table style="background-color: #f8f9fa; padding: 5px; margin: 10px 0;">
                        <tr>
                            <td>5-year survival = 72% (95% CI: 65-79%)</td>
                        </tr>
                    </table>
                    <p>This means:</p>
                    <ul>
                        <li>72% of patients are expected to survive beyond 5 years</li>
                        <li>You can be 95% confident the true rate is between 65% and 79%</li>
                        <li>This can be compared to published rates for similar populations</li>
                    </ul>
                    
                    <h5>⚠️ Key Points to Remember:</h5>
                    <ul>
                        <li>Decreasing "n.risk" over time is normal (patients complete follow-up)</li>
                        <li>Wider confidence intervals at later times reflect fewer patients at risk</li>
                        <li>Compare your rates to established benchmarks in your field</li>
                        <li>Consider both statistical and clinical significance</li>
                    </ul>
                </div>
                ')
                
                # Person-Time Analysis Explanation
                private$.setExplanationContent("personTimeExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: #f8d7da; border-left: 4px solid #dc3545;">
                    <h4>Understanding Person-Time Analysis</h4>
                    <p><strong>Person-Time:</strong> Accounts for varying follow-up durations by calculating total observation time.</p>
                    <ul>
                        <li><strong>Person-Years:</strong> Sum of follow-up time for all patients in the group</li>
                        <li><strong>Incidence Rate:</strong> Events per unit of person-time (e.g., deaths per 100 person-years)</li>
                        <li><strong>Rate Ratio:</strong> Compares incidence rates between groups</li>
                        <li><strong>Confidence Intervals:</strong> Provide precision estimates for incidence rates</li>
                    </ul>
                    <p><em>Advantage:</em> More precise than simple event proportions when follow-up times vary significantly between patients.</p>
                </div>
                ')
                
                # RMST Explanation
                private$.setExplanationContent("rmstExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: #e2e3e5; border-left: 4px solid #6c757d;">
                    <h4>Understanding Restricted Mean Survival Time (RMST)</h4>
                    <p><strong>RMST:</strong> Average survival time up to a specified time horizon (τ).</p>
                    <ul>
                        <li><strong>Interpretation:</strong> Mean survival time within the observation period</li>
                        <li><strong>Time Horizon (τ):</strong> Maximum follow-up time considered</li>
                        <li><strong>Robust Measure:</strong> Less affected by tail behavior than median survival</li>
                        <li><strong>Difference in RMST:</strong> Directly interpretable as difference in mean survival time</li>
                    </ul>
                    <p><em>When to use:</em> Particularly useful when median survival cannot be estimated (too few events) or when comparing survival over a specific time period.</p>
                </div>
                ')
                
                # Residual Diagnostics Explanation
                private$.setExplanationContent("residualDiagnosticsExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: #ffeaa7; border-left: 4px solid #fdcb6e;">
                    <h4>Understanding Cox Model Residual Diagnostics</h4>
                    <p><strong>Model Residuals:</strong> Help assess model fit and identify potential problems.</p>
                    <ul>
                        <li><strong>Martingale Residuals:</strong> Detect outliers and functional form issues (should scatter around 0)</li>
                        <li><strong>Deviance Residuals:</strong> Standardized residuals for outlier detection (approximately normal if model fits)</li>
                        <li><strong>Score Residuals:</strong> Assess influence of observations on regression coefficients</li>
                        <li><strong>Schoenfeld Residuals:</strong> Test proportional hazards assumption (should be independent of time)</li>
                    </ul>
                    <p><em>Clinical interpretation:</em> Large residuals may indicate patients with unusual survival patterns or data quality issues.</p>
                </div>
                ')
                
                # Survival Plots Explanation
                private$.setExplanationContent("survivalPlotsExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: #d1ecf1; border-left: 4px solid #bee5eb;">
                    <h4>📈 Understanding Your Survival Curves and Plots</h4>
                    <p><strong>What are Survival Curves?</strong> These graphs show how the probability of survival changes over time for different groups.</p>
                    
                    <h5>🎨 Types of Plots Available:</h5>
                    <table style="width:100%; margin: 10px 0; border-collapse: collapse;">
                        <tr style="background-color: #f8f9fa;">
                            <th style="padding: 8px; text-align: left;">Plot Type</th>
                            <th style="padding: 8px; text-align: left;">What It Shows</th>
                            <th style="padding: 8px; text-align: left;">When to Use</th>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;"><strong>Survival Curve</strong></td>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;">% surviving over time</td>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;">Standard presentation</td>
                        </tr>
                        <tr style="background-color: #f8f9fa;">
                            <td style="padding: 8px;"><strong>Cumulative Events</strong></td>
                            <td style="padding: 8px;">% experiencing event</td>
                            <td style="padding: 8px;">Focus on event occurrence</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px;"><strong>Cumulative Hazard</strong></td>
                            <td style="padding: 8px;">Accumulated risk</td>
                            <td style="padding: 8px;">Technical assessment</td>
                        </tr>
                        <tr style="background-color: #f8f9fa;">
                            <td style="padding: 8px;"><strong>Log-Log Plot</strong></td>
                            <td style="padding: 8px;">Model assumptions</td>
                            <td style="padding: 8px;">Check proportional hazards</td>
                        </tr>
                    </table>
                    
                    <h5>📖 How to Read Survival Curves:</h5>
                    <div style="background-color: #fff3cd; padding: 10px; border-radius: 5px; margin: 10px 0;">
                        <ul style="margin: 5px 0;">
                            <li><strong>Y-axis (0-1 or 0-100%):</strong> Probability of survival</li>
                            <li><strong>X-axis:</strong> Time since study start</li>
                            <li><strong>Steps down:</strong> Each step represents an event</li>
                            <li><strong>Tick marks (+):</strong> Censored patients (lost to follow-up)</li>
                            <li><strong>Shaded areas:</strong> 95% confidence intervals</li>
                        </ul>
                    </div>
                    
                    <h5>🔍 What to Look For:</h5>
                    <table style="width:100%; margin: 10px 0;">
                        <tr>
                            <td style="padding: 8px; background-color: #e8f5e9;"><strong>✓ Curves separate early</strong></td>
                            <td style="padding: 8px;">Groups differ significantly</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px; background-color: #ffebee;"><strong>✗ Curves overlap</strong></td>
                            <td style="padding: 8px;">No significant difference</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px; background-color: #e3f2fd;"><strong>↕ Curves cross</strong></td>
                            <td style="padding: 8px;">Effect changes over time</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px; background-color: #fff3cd;"><strong>⬇ Steep drop</strong></td>
                            <td style="padding: 8px;">High early event rate</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px; background-color: #f3e5f5;"><strong>→ Flat plateau</strong></td>
                            <td style="padding: 8px;">Stable period (few events)</td>
                        </tr>
                    </table>
                    
                    <h5>📊 Risk Table (Below Plot):</h5>
                    <p>Shows how many patients are still being followed at each time point:</p>
                    <ul>
                        <li>Helps assess reliability of estimates</li>
                        <li>Fewer patients = wider confidence intervals</li>
                        <li>Important for interpreting late time points</li>
                    </ul>
                    
                    <h5>💡 Clinical Interpretation Tips:</h5>
                    <ul>
                        <li><strong>Median survival:</strong> Where curve crosses 50% line</li>
                        <li><strong>1-year survival:</strong> Height of curve at 12 months</li>
                        <li><strong>Statistical significance:</strong> Check if confidence bands overlap</li>
                        <li><strong>Clinical significance:</strong> Consider if differences are meaningful for patients</li>
                    </ul>
                </div>
                ')
                
                # Parametric Models Explanation
                private$.setExplanationContent("parametricModelsExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: #f8d7da; border-left: 4px solid #dc3545;">
                    <h4>Understanding Parametric Survival Models</h4>
                    <p><strong>Parametric Models:</strong> Alternative to Cox regression with explicit hazard function shapes.</p>
                    <ul>
                        <li><strong>Exponential:</strong> Constant hazard rate (memoryless property)</li>
                        <li><strong>Weibull:</strong> Monotonic hazard (increasing, decreasing, or constant)</li>
                        <li><strong>Log-Normal:</strong> Hazard increases then decreases</li>
                        <li><strong>Log-Logistic:</strong> Similar to log-normal but with heavier tails</li>
                        <li><strong>Gamma:</strong> Flexible hazard shape with gamma distribution</li>
                        <li><strong>Splines:</strong> Flexible non-parametric hazard estimation</li>
                    </ul>
                    <p><em>Advantages:</em> Allow extrapolation, provide explicit survival functions, enable economic modeling.</p>
                    <p><em>Model Selection:</em> Compare AIC/BIC values - lower is better. Visual fit assessment with Kaplan-Meier overlay.</p>
                </div>
                ')
            }
            
            # Clinical Interpretation Helper Functions ----
            ,
            .generateClinicalInterpretation = function(results, analysis_type = "overall") {
                if (!self$options$showSummaries) return("")
                
                interpretations <- list()
                
                # Median Survival Clinical Context
                if (!is.null(results$medianData) && nrow(results$medianData) > 0) {
                    for (i in seq_len(nrow(results$medianData))) {
                        # Safe column access with fallbacks
                        group_name <- tryCatch({
                            results$medianData[[i, 1]]
                        }, error = function(e) paste("Group", i))
                        
                        median_val <- tryCatch({
                            col_names <- names(results$medianData)
                            if ("median" %in% col_names) {
                                results$medianData[[i, "median"]]
                            } else if ("time" %in% col_names) {
                                results$medianData[[i, "time"]]
                            } else if (ncol(results$medianData) >= 2) {
                                results$medianData[[i, 2]]  # Second column as fallback
                            } else {
                                NA
                            }
                        }, error = function(e) NA)
                        
                        ci_lower <- tryCatch({
                            col_names <- names(results$medianData)
                            if ("lcl" %in% col_names) {
                                results$medianData[[i, "lcl"]]
                            } else if ("lower" %in% col_names) {
                                results$medianData[[i, "lower"]]
                            } else if ("conf.low" %in% col_names) {
                                results$medianData[[i, "conf.low"]]
                            } else {
                                NA
                            }
                        }, error = function(e) NA)
                        
                        ci_upper <- tryCatch({
                            col_names <- names(results$medianData)
                            if ("ucl" %in% col_names) {
                                results$medianData[[i, "ucl"]]
                            } else if ("upper" %in% col_names) {
                                results$medianData[[i, "upper"]]
                            } else if ("conf.high" %in% col_names) {
                                results$medianData[[i, "conf.high"]]
                            } else {
                                NA
                            }
                        }, error = function(e) NA)
                        
                        if (is.na(median_val) || median_val == "NR") {
                            clinical_meaning <- paste(.("In the"), group_name, .("group, fewer than half of the patients experienced the event during the follow-up period. This is typically considered a favorable prognostic finding."))
                        } else {
                            # Generate time-specific clinical context
                            time_context <- ""
                            if (median_val <= 6) {
                                time_context <- .("indicating a rapidly progressing condition requiring urgent intervention")
                            } else if (median_val <= 12) {
                                time_context <- .("suggesting an acute condition with significant short-term impact")
                            } else if (median_val <= 36) {
                                time_context <- .("indicating a condition with moderate progression timeline")
                            } else if (median_val <= 60) {
                                time_context <- .("suggesting a chronic condition with extended survival")
                            } else {
                                time_context <- .("indicating excellent long-term prognosis")
                            }
                            
                            clinical_meaning <- paste(.("In the"), group_name, .("group, half of the patients experienced the event by"), median_val, .("months,"), time_context, .(". The 95% confidence interval ("), ci_lower, .("to"), ci_upper, .(") provides the range of uncertainty around this estimate."))
                        }
                        
                        interpretations[[paste("median", group_name)]] <- clinical_meaning
                    }
                }
                
                # Cox Regression Clinical Context
                if (!is.null(results$coxData) && nrow(results$coxData) > 0) {
                    for (i in seq_len(nrow(results$coxData))) {
                        # Safer column access with dynamic column detection
                        cox_cols <- names(results$coxData)
                        
                        comparison <- tryCatch({
                            if (any(c("comparison", "term", "group", "variable") %in% cox_cols)) {
                                idx <- which(c("comparison", "term", "group", "variable") %in% cox_cols)[1]
                                col_name <- c("comparison", "term", "group", "variable")[idx]
                                results$coxData[[i, col_name]]
                            } else if (ncol(results$coxData) >= 1) {
                                results$coxData[[i, 1]]  # First column as fallback
                            } else {
                                paste("Group", i)
                            }
                        }, error = function(e) paste("Group", i))
                        
                        hr <- tryCatch({
                            hr_candidates <- c("hr", "hazard.ratio", "exp.coef", "HR", "exp_coef")
                            hr_col <- intersect(hr_candidates, cox_cols)[1]
                            if (!is.na(hr_col)) {
                                as.numeric(results$coxData[[i, hr_col]])
                            } else {
                                NA
                            }
                        }, error = function(e) NA)
                        
                        ci_lower <- tryCatch({
                            lower_candidates <- c("hrLower", "conf.low", "lower", "hr.lower", "ci_lower")
                            lower_col <- intersect(lower_candidates, cox_cols)[1]
                            if (!is.na(lower_col)) {
                                as.numeric(results$coxData[[i, lower_col]])
                            } else {
                                NA
                            }
                        }, error = function(e) NA)
                        
                        ci_upper <- tryCatch({
                            upper_candidates <- c("hrUpper", "conf.high", "upper", "hr.upper", "ci_upper")
                            upper_col <- intersect(upper_candidates, cox_cols)[1]
                            if (!is.na(upper_col)) {
                                as.numeric(results$coxData[[i, upper_col]])
                            } else {
                                NA
                            }
                        }, error = function(e) NA)
                        
                        p_val <- tryCatch({
                            p_candidates <- c("hrp", "p.value", "pvalue", "p", "Pr...z..")
                            p_col <- intersect(p_candidates, cox_cols)[1]
                            if (!is.na(p_col)) {
                                as.numeric(results$coxData[[i, p_col]])
                            } else {
                                NA
                            }
                        }, error = function(e) NA)
                        
                        if (!is.na(hr)) {
                            # Clinical interpretation based on HR magnitude
                            risk_interpretation <- ""
                            clinical_significance <- ""
                            
                            if (hr < 0.5) {
                                risk_interpretation <- paste(.("strongly protective effect ("), round((1-hr)*100, 0), .("% risk reduction)"))
                                clinical_significance <- .("This represents a clinically meaningful protective effect")
                            } else if (hr < 0.8) {
                                risk_interpretation <- paste(.("moderate protective effect ("), round((1-hr)*100, 0), .("% risk reduction)"))
                                clinical_significance <- .("This suggests a moderate clinical benefit")
                            } else if (hr < 1.2) {
                                risk_interpretation <- .("minimal difference in risk")
                                clinical_significance <- .("This difference may not be clinically meaningful")
                            } else if (hr < 2.0) {
                                risk_interpretation <- paste(.("moderately increased risk ("), round((hr-1)*100, 0), .("% higher risk)"))
                                clinical_significance <- .("This suggests a moderate increase in clinical risk")
                            } else {
                                risk_interpretation <- paste(.("substantially increased risk ("), round((hr-1)*100, 0), .("% higher risk)"))
                                clinical_significance <- .("This represents a clinically significant increase in risk")
                            }
                            
                            # Statistical vs Clinical Significance
                            statistical_context <- ""
                            if (p_val < 0.001) {
                                statistical_context <- .("with very strong statistical evidence")
                            } else if (p_val < 0.01) {
                                statistical_context <- .("with strong statistical evidence")
                            } else if (p_val < 0.05) {
                                statistical_context <- .("with statistical significance")
                            } else {
                                statistical_context <- .("but without statistical significance")
                            }
                            
                            clinical_meaning <- paste(.("Comparing"), comparison, .(": shows a"), risk_interpretation, statistical_context, .(". "), clinical_significance, .(". The hazard ratio of"), round(hr, 2), .("(95% CI:"), round(ci_lower, 2), .("to"), round(ci_upper, 2), .(") indicates the relative risk between groups."))
                            
                            interpretations[[paste("cox", gsub(" ", "_", comparison))]] <- clinical_meaning
                        }
                    }
                }
                
                return(interpretations)
            }
            
            ,
            .generateClinicalGlossary = function() {
                glossary_html <- '
                <div style="margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-left: 4px solid #6c757d;">
                    <h4>📖 Clinical Terminology Glossary</h4>
                    
                    <div style="display: grid; gap: 10px; margin-top: 15px;">
                        <div style="background-color: white; padding: 10px; border-radius: 5px;">
                            <strong>Event:</strong> The outcome of interest being studied (e.g., death, disease recurrence, treatment failure)
                        </div>
                        
                        <div style="background-color: white; padding: 10px; border-radius: 5px;">
                            <strong>Censoring:</strong> When a patient is lost to follow-up or the study ends before the event occurs
                        </div>
                        
                        <div style="background-color: white; padding: 10px; border-radius: 5px;">
                            <strong>Median Survival:</strong> Time point when 50% of patients have experienced the event
                        </div>
                        
                        <div style="background-color: white; padding: 10px; border-radius: 5px;">
                            <strong>Hazard Ratio (HR):</strong> Risk multiplier comparing two groups (HR=2 means twice the risk)
                        </div>
                        
                        <div style="background-color: white; padding: 10px; border-radius: 5px;">
                            <strong>95% Confidence Interval:</strong> Range where we are 95% confident the true value lies
                        </div>
                        
                        <div style="background-color: white; padding: 10px; border-radius: 5px;">
                            <strong>P-value:</strong> Probability that observed differences occurred by chance (p&lt;0.05 = statistically significant)
                        </div>
                        
                        <div style="background-color: white; padding: 10px; border-radius: 5px;">
                            <strong>Person-Time:</strong> Total follow-up time accounting for varying observation periods
                        </div>
                        
                        <div style="background-color: white; padding: 10px; border-radius: 5px;">
                            <strong>Kaplan-Meier:</strong> Non-parametric method to estimate survival probability over time
                        </div>
                        
                        <div style="background-color: white; padding: 10px; border-radius: 5px;">
                            <strong>Cox Regression:</strong> Statistical model comparing hazard rates between groups adjusting for time
                        </div>
                        
                        <div style="background-color: white; padding: 10px; border-radius: 5px;">
                            <strong>RMST:</strong> Restricted Mean Survival Time - average survival within a specific time window
                        </div>
                    </div>
                </div>
                ';
                
                return(glossary_html)
            }
            
            ,
            .generateCopyReadySentences = function(results) {
                if (!self$options$showSummaries) return("")
                
                sentences <- list()
                
                # Generate copy-ready sentences for median survival
                if (!is.null(results$medianData) && nrow(results$medianData) > 0) {
                    median_sentences <- c()
                    
                    for (i in seq_len(nrow(results$medianData))) {
                        # Safe column access for median survival data
                        group_name <- tryCatch({
                            results$medianData[[i, 1]]
                        }, error = function(e) paste("Group", i))
                        
                        median_val <- tryCatch({
                            col_names <- names(results$medianData)
                            if ("median" %in% col_names) {
                                results$medianData[[i, "median"]]
                            } else if ("time" %in% col_names) {
                                results$medianData[[i, "time"]]
                            } else {
                                NA
                            }
                        }, error = function(e) NA)
                        
                        ci_lower <- tryCatch({
                            col_names <- names(results$medianData)
                            if ("lcl" %in% col_names) {
                                results$medianData[[i, "lcl"]]
                            } else if ("lower" %in% col_names) {
                                results$medianData[[i, "lower"]]
                            } else {
                                NA
                            }
                        }, error = function(e) NA)
                        
                        ci_upper <- tryCatch({
                            col_names <- names(results$medianData)
                            if ("ucl" %in% col_names) {
                                results$medianData[[i, "ucl"]]
                            } else if ("upper" %in% col_names) {
                                results$medianData[[i, "upper"]]
                            } else {
                                NA
                            }
                        }, error = function(e) NA)
                        
                        if (is.na(median_val) || median_val == "NR") {
                            sentence <- paste(.("Median survival was not reached for the"), group_name, .("group, indicating that fewer than half of patients experienced the event during follow-up."))
                        } else {
                            sentence <- paste(.("Median survival for the"), group_name, .("group was"), median_val, .("months (95% CI:"), ci_lower, .("to"), ci_upper, .("months)."))
                        }
                        median_sentences <- c(median_sentences, sentence)
                    }
                    
                    sentences[["median_results"]] <- median_sentences
                }
                
                # Generate copy-ready sentences for Cox regression
                if (!is.null(results$coxData) && nrow(results$coxData) > 0) {
                    cox_sentences <- c()
                    
                    for (i in seq_len(nrow(results$coxData))) {
                        # Safe column access for Cox regression data
                        cox_cols <- names(results$coxData)
                        
                        comparison <- tryCatch({
                            if (any(c("comparison", "term", "group", "variable") %in% cox_cols)) {
                                idx <- which(c("comparison", "term", "group", "variable") %in% cox_cols)[1]
                                col_name <- c("comparison", "term", "group", "variable")[idx]
                                results$coxData[[i, col_name]]
                            } else {
                                paste("Group", i)
                            }
                        }, error = function(e) paste("Group", i))
                        
                        hr <- tryCatch({
                            hr_candidates <- c("hr", "hazard.ratio", "exp.coef", "HR")
                            hr_col <- intersect(hr_candidates, cox_cols)[1]
                            if (!is.na(hr_col)) {
                                as.numeric(results$coxData[[i, hr_col]])
                            } else {
                                NA
                            }
                        }, error = function(e) NA)
                        
                        ci_lower <- tryCatch({
                            lower_candidates <- c("hrLower", "conf.low", "lower", "hr.lower")
                            lower_col <- intersect(lower_candidates, cox_cols)[1]
                            if (!is.na(lower_col)) {
                                as.numeric(results$coxData[[i, lower_col]])
                            } else {
                                NA
                            }
                        }, error = function(e) NA)
                        
                        ci_upper <- tryCatch({
                            upper_candidates <- c("hrUpper", "conf.high", "upper", "hr.upper")
                            upper_col <- intersect(upper_candidates, cox_cols)[1]
                            if (!is.na(upper_col)) {
                                as.numeric(results$coxData[[i, upper_col]])
                            } else {
                                NA
                            }
                        }, error = function(e) NA)
                        
                        p_val <- tryCatch({
                            p_candidates <- c("hrp", "p.value", "pvalue", "p", "Pr...z..")
                            p_col <- intersect(p_candidates, cox_cols)[1]
                            if (!is.na(p_col)) {
                                as.numeric(results$coxData[[i, p_col]])
                            } else {
                                NA
                            }
                        }, error = function(e) NA)
                        
                        if (!is.na(hr)) {
                            significance <- ifelse(p_val < 0.05, .("statistically significant"), .("not statistically significant"))
                            direction <- ifelse(hr < 1, .("reduced"), .("increased"))
                            
                            sentence <- paste(.("Cox regression analysis showed"), direction, .("risk for"), comparison, .(", with a hazard ratio of"), round(hr, 2), .("(95% CI:"), round(ci_lower, 2), .("to"), round(ci_upper, 2), .(", p ="), round(p_val, 3), .("), which was"), significance, .("."))
                            cox_sentences <- c(cox_sentences, sentence)
                        }
                    }
                    
                    sentences[["cox_results"]] <- cox_sentences
                }
                
                # Generate copy-ready sentences for survival probabilities
                if (!is.null(results$survTable) && nrow(results$survTable) > 0) {
                    surv_sentences <- c()
                    
                    # Common time points for reporting
                    common_times <- c(12, 36, 60)
                    
                    for (time_point in common_times) {
                        surv_at_time <- results$survTable[results$survTable$time == time_point, ]
                        
                        if (nrow(surv_at_time) > 0) {
                            for (j in seq_len(nrow(surv_at_time))) {
                                group <- surv_at_time[[j, 1]]
                                survival <- round(surv_at_time[[j, "survival"]] * 100, 1)
                                ci_lower <- round(surv_at_time[[j, "lower"]] * 100, 1)
                                ci_upper <- round(surv_at_time[[j, "upper"]] * 100, 1)
                                
                                years <- time_point / 12
                                sentence <- paste(.("The"), years, .("-year survival rate for the"), group, .("group was"), survival, .("% (95% CI:"), ci_lower, .("% to"), ci_upper, .("%)."))
                                surv_sentences <- c(surv_sentences, sentence)
                            }
                        }
                    }
                    
                    sentences[["survival_probabilities"]] <- surv_sentences
                }
                
                return(sentences)
            }
            
            ,
            .populateEnhancedClinicalContent = function() {
                if (!self$options$showExplanations && !self$options$showSummaries) return()
                
                # Collect all analysis results from result objects with enhanced validation
                results <- list()
                
                # More robust data collection with validation
                tryCatch({
                    if (!is.null(self$results$medianSurvivalTable) && 
                        self$results$medianSurvivalTable$rowCount > 0) {
                        df <- self$results$medianSurvivalTable$asDF()
                        if (nrow(df) > 0 && ncol(df) > 0) {
                            results$medianData <- df
                        }
                    }
                }, error = function(e) {
                    # Log warning but continue
                    warning(paste(.("Could not access median survival data:"), e$message))
                })
                
                tryCatch({
                    if (!is.null(self$results$coxTable) && 
                        self$results$coxTable$rowCount > 0) {
                        df <- self$results$coxTable$asDF()
                        if (nrow(df) > 0 && ncol(df) > 0) {
                            results$coxData <- df
                        }
                    }
                }, error = function(e) {
                    # Log warning but continue
                    warning(paste(.("Could not access Cox regression data:"), e$message))
                })
                
                tryCatch({
                    if (!is.null(self$results$survTable) && 
                        self$results$survTable$rowCount > 0) {
                        df <- self$results$survTable$asDF()
                        if (nrow(df) > 0 && ncol(df) > 0) {
                            results$survTable <- df
                        }
                    }
                }, error = function(e) {
                    # Log warning but continue
                    warning(paste(.("Could not access survival probability data:"), e$message))
                })
                
                # Generate clinical interpretations
                clinical_interpretations <- private$.generateClinicalInterpretation(results)
                
                # Generate copy-ready sentences
                copy_sentences <- private$.generateCopyReadySentences(results)
                
                # Add Clinical Glossary
                if (self$options$showExplanations) {
                    private$.setExplanationContent("clinicalGlossaryExplanation", private$.generateClinicalGlossary())
                }
                
                # Add Enhanced Clinical Interpretation
                if (self$options$showSummaries && length(clinical_interpretations) > 0) {
                    interpretation_html <- paste(
                        '<div style="margin-bottom: 20px; padding: 15px; background-color: #e8f4fd; border-left: 4px solid #007bff;">',
                        '<h4>🏥 Clinical Interpretation</h4>',
                        paste(lapply(clinical_interpretations, function(x) paste('<p>', x, '</p>')), collapse = ""),
                        '</div>'
                    )
                    
                    private$.setExplanationContent("clinicalInterpretationExplanation", interpretation_html)
                }
                
                # Add Copy-Ready Clinical Report Sentences
                if (self$options$showSummaries && length(copy_sentences) > 0) {
                    copy_html <- '<div style="margin-bottom: 20px; padding: 15px; background-color: #fff3cd; border-left: 4px solid #ffc107;"><h4>📝 Copy-Ready Clinical Report Sentences</h4>'
                    
                    for (section_name in names(copy_sentences)) {
                        copy_html <- paste0(copy_html, '<h5>', tools::toTitleCase(gsub("_", " ", section_name)), ':</h5><div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">')
                        
                        for (sentence in copy_sentences[[section_name]]) {
                            copy_html <- paste0(copy_html, '<p style="margin: 5px 0; padding: 5px; background-color: #f8f9fa; border-radius: 3px;">', sentence, '</p>')
                        }
                        
                        copy_html <- paste0(copy_html, '</div>')
                    }
                    
                    copy_html <- paste0(copy_html, '</div>')
                    
                    private$.setExplanationContent("copyReadySentencesExplanation", copy_html)
                }
            }
            
            # Parametric Survival Analysis Methods ----
            
            ,
            .parametricSurvival = function(results) {
                
                # Check if flexsurv package is available
                if (!requireNamespace("flexsurv", quietly = TRUE)) {
                    warning(.("flexsurv package is required for parametric survival models. Please install it: install.packages('flexsurv')"))
                    return()
                }
                
                library(flexsurv)
                
                cleanData <- results$cleanData
                
                # Prepare formula for parametric models
                time_var <- .escapeVariableNames(results$name1time)
                outcome_var <- .escapeVariableNames(results$name2outcome)
                
                if (self$options$parametric_covariates && !is.null(self$options$explanatory)) {
                    explanatory_names <- self$options$explanatory
                    # Escape variable names for safe formula construction
                    escaped_explanatory_names <- .escapeVariableNames(explanatory_names)
                    # Build covariate formula
                    covariate_formula <- paste(escaped_explanatory_names, collapse = " + ")
                    formula_str <- paste0("Surv(", time_var, ", ", outcome_var, ") ~ ", covariate_formula)
                } else {
                    # Intercept-only model
                    formula_str <- paste0("Surv(", time_var, ", ", outcome_var, ") ~ 1")
                }
                
                survival_formula <- as.formula(formula_str)
                
                # List of distributions to compare if enabled
                if (self$options$compare_distributions) {
                    distributions <- c("exp", "weibull", "lnorm", "llogis", "gamma", "gengamma")
                } else {
                    distributions <- self$options$parametric_distribution
                }
                
                # Fit parametric models
                model_results <- list()
                comparison_data <- data.frame()
                
                for (dist in distributions) {
                    tryCatch({
                        if (dist == "survspline") {
                            # Fit spline model
                            model <- flexsurvspline(
                                formula = survival_formula,
                                data = cleanData,
                                k = self$options$spline_knots,
                                scale = self$options$spline_scale
                            )
                        } else {
                            # Fit standard parametric model
                            model <- flexsurvreg(
                                formula = survival_formula,
                                data = cleanData,
                                dist = dist
                            )
                        }
                        
                        model_results[[dist]] <- model
                        
                        # Extract model comparison metrics
                        comparison_data <- rbind(comparison_data, data.frame(
                            distribution = dist,
                            aic = model$AIC,
                            bic = model$AIC + (log(nrow(cleanData)) - 2) * model$npars,
                            loglik = model$loglik,
                            df = model$npars
                        ))
                        
                    }, error = function(e) {
                        message(paste(.("Failed to fit"), dist, .("distribution:"), e$message))
                    })
                }
                
                # Select best model if comparing multiple distributions
                if (self$options$compare_distributions && nrow(comparison_data) > 1) {
                    best_model_name <- comparison_data$distribution[which.min(comparison_data$aic)]
                    best_model <- model_results[[best_model_name]]
                    
                    # Populate comparison table
                    comparison_table <- self$results$parametricModelComparison
                    for (i in 1:nrow(comparison_data)) {
                        comparison_table$addRow(rowKey = i, values = list(
                            distribution = comparison_data$distribution[i],
                            aic = comparison_data$aic[i],
                            bic = comparison_data$bic[i],
                            loglik = comparison_data$loglik[i],
                            df = comparison_data$df[i]
                        ))
                    }
                } else {
                    # Use single specified model
                    best_model_name <- distributions[1]
                    best_model <- model_results[[best_model_name]]
                }
                
                # Populate model summary table
                if (!is.null(best_model)) {
                    # summary_data <- summary(best_model)
                    coef_table <- best_model$res
                    
                    model_table <- self$results$parametricModelSummary
                    for (i in 1:nrow(coef_table)) {
                        # Calculate p-value if missing
                        p_val <- if ("p" %in% colnames(coef_table)) {
                            coef_table[i, "p"]
                        } else {
                            est <- coef_table[i, "est"]
                            se <- coef_table[i, "se"]
                            if (!is.na(se) && se > 0) 2 * (1 - pnorm(abs(est / se))) else NA
                        }
                        
                        model_table$addRow(rowKey = i, values = list(
                            parameter = rownames(coef_table)[i],
                            estimate = coef_table[i, "est"],
                            se = coef_table[i, "se"],
                            ci_lower = coef_table[i, "L95%"],
                            ci_upper = coef_table[i, "U95%"],
                            pvalue = p_val
                        ))
                    }
                    
                    # Generate diagnostics
                    if (self$options$parametric_diagnostics) {
                        diagnostics_html <- private$.generateParametricDiagnostics(best_model, best_model_name)
                        self$results$parametricDiagnostics$setContent(diagnostics_html)
                    }
                    
                    # Store model for plotting functions
                    private$.parametric_model <- best_model
                    private$.parametric_model_name <- best_model_name
                    private$.parametric_results <- results
                }
            }
            
            ,
            .generateParametricDiagnostics = function(model, model_name) {
                
                diagnostics <- ""
                
                tryCatch({
                    # Model fit statistics
                    aic_val <- model$AIC
                    loglik_val <- model$loglik
                    npars <- model$npars
                    n_obs <- model$N
                    
                    diagnostics <- paste0(
                        '<div style="margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6;">',
                        '<h4>Parametric Model Diagnostics: ', toupper(model_name), '</h4>',
                        '<table class="table table-striped" style="margin-bottom: 15px;">',
                        '<tr><td><strong>Distribution:</strong></td><td>', model_name, '</td></tr>',
                        '<tr><td><strong>Sample Size:</strong></td><td>', n_obs, '</td></tr>',
                        '<tr><td><strong>Parameters:</strong></td><td>', npars, '</td></tr>',
                        '<tr><td><strong>Log-likelihood:</strong></td><td>', round(loglik_val, 3), '</td></tr>',
                        '<tr><td><strong>AIC:</strong></td><td>', round(aic_val, 3), '</td></tr>',
                        '</table>'
                    )
                    
                    # Add interpretation
                    if (model_name == "weibull") {
                        shape_param <- model$res[["shape", "est"]]
                        if (shape_param < 1) {
                            hazard_trend <- "decreasing over time (shape < 1)"
                        } else if (shape_param > 1) {
                            hazard_trend <- "increasing over time (shape > 1)"  
                        } else {
                            hazard_trend <- "constant over time (shape = 1, equivalent to exponential)"
                        }
                        diagnostics <- paste0(diagnostics,
                            '<p><strong>Hazard Pattern:</strong> ', hazard_trend, '</p>'
                        )
                    }
                    
                    diagnostics <- paste0(diagnostics, '</div>')
                    
                }, error = function(e) {
                    diagnostics <- paste0('<p>Error generating diagnostics: ', e$message, '</p>')
                })
                
                return(diagnostics)
            }
            
            # Parametric survival plot functions ----
            
            ,
            .plotParametricSurvival = function(image, ...) {
                
                if (is.null(private$.parametric_model)) {
                    return()
                }
                
                model <- private$.parametric_model
                results <- private$.parametric_results
                cleanData <- results$cleanData
                
                library(ggplot2)
                library(survival)
                
                # Generate survival curves from parametric model
                max_time <- max(cleanData$CalculatedTime, na.rm = TRUE)
                time_points <- seq(0, max_time, length.out = 100)
                
                # Get survival predictions
                if (!is.null(self$options$explanatory) && self$options$parametric_covariates) {
                    # Create prediction data for each group
                    groups <- levels(cleanData[[self$options$explanatory]])
                    plot_data <- data.frame()
                    
                    for (group in groups) {
                        pred_data <- data.frame(
                            time = time_points
                        )
                        pred_data[[self$options$explanatory]] <- group
                        
                        surv_pred <- summary(model, newdata = pred_data, t = time_points, ci = TRUE)
                        
                        group_data <- data.frame(
                            time = time_points,
                            survival = surv_pred$surv,
                            lower = surv_pred$lcl,
                            upper = surv_pred$ucl,
                            group = group,
                            type = "Parametric"
                        )
                        plot_data <- rbind(plot_data, group_data)
                    }
                    
                    # Add Kaplan-Meier for comparison
                    km_fit <- survfit(Surv(CalculatedTime, CalculatedOutcome) ~ get(self$options$explanatory), 
                                     data = cleanData)
                    km_summary <- summary(km_fit, times = time_points)
                    
                    if (length(groups) > 1) {
                        for (i in 1:length(groups)) {
                            group_indices <- km_summary$strata == paste0("get(self$options$explanatory)=", groups[i])
                            if (any(group_indices)) {
                                km_data <- data.frame(
                                    time = km_summary$time[group_indices],
                                    survival = km_summary$surv[group_indices],
                                    lower = km_summary$lower[group_indices],
                                    upper = km_summary$upper[group_indices],
                                    group = groups[i],
                                    type = "Kaplan-Meier"
                                )
                                plot_data <- rbind(plot_data, km_data)
                            }
                        }
                    }
                    
                    # Create plot
                    p <- ggplot(plot_data, aes(x = time, y = survival, color = group, linetype = type)) +
                        geom_line(size = 1) +
                        geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha = 0.2, color = NA) +
                        labs(
                            title = paste(.("Parametric Survival Curves:"), private$.parametric_model_name),
                            x = .("Time"),
                            y = .("Survival Probability"),
                            color = self$options$explanatory,
                            linetype = .("Method")
                        ) +
                        theme_minimal() +
                        scale_y_continuous(limits = c(0, 1))
                        
                } else {
                    # Single survival curve
                    surv_pred <- summary(model, t = time_points, ci = TRUE)
                    
                    plot_data <- data.frame(
                        time = time_points,
                        survival = surv_pred$surv,
                        lower = surv_pred$lcl,
                        upper = surv_pred$ucl,
                        type = "Parametric"
                    )
                    
                    # Add Kaplan-Meier
                    km_fit <- survfit(Surv(CalculatedTime, CalculatedOutcome) ~ 1, data = cleanData)
                    km_summary <- summary(km_fit, times = time_points)
                    
                    km_data <- data.frame(
                        time = km_summary$time,
                        survival = km_summary$surv,
                        lower = km_summary$lower,
                        upper = km_summary$upper,
                        type = "Kaplan-Meier"
                    )
                    
                    plot_data <- rbind(plot_data, km_data)
                    
                    p <- ggplot(plot_data, aes(x = time, y = survival, linetype = type)) +
                        geom_line(size = 1, color = "blue") +
                        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, fill = "blue") +
                        labs(
                            title = paste(.("Parametric vs Kaplan-Meier Survival:"), private$.parametric_model_name),
                            x = .("Time"), 
                            y = .("Survival Probability"),
                            linetype = .("Method")
                        ) +
                        theme_minimal() +
                        scale_y_continuous(limits = c(0, 1))
                }
                
                print(p)
                TRUE
            }
            
            ,
            .plotHazardFunction = function(image, ...) {
                
                if (is.null(private$.parametric_model)) {
                    return()
                }
                
                model <- private$.parametric_model
                results <- private$.parametric_results
                cleanData <- results$cleanData
                
                library(ggplot2)
                
                max_time <- max(cleanData$CalculatedTime, na.rm = TRUE)
                time_points <- seq(0.1, max_time, length.out = 100)  # Start from 0.1 to avoid issues at t=0
                
                tryCatch({
                    # Get hazard predictions
                    if (!is.null(self$options$explanatory) && self$options$parametric_covariates) {
                        groups <- levels(cleanData[[self$options$explanatory]])
                        plot_data <- data.frame()
                        
                        for (group in groups) {
                            pred_data <- data.frame(
                                time = time_points
                            )
                            pred_data[[self$options$explanatory]] <- group
                            
                            hazard_pred <- summary(model, newdata = pred_data, t = time_points, 
                                                  type = "hazard", ci = TRUE)
                            
                            group_data <- data.frame(
                                time = time_points,
                                hazard = hazard_pred$est,
                                lower = hazard_pred$lcl,
                                upper = hazard_pred$ucl,
                                group = group
                            )
                            plot_data <- rbind(plot_data, group_data)
                        }
                        
                        p <- ggplot(plot_data, aes(x = time, y = hazard, color = group)) +
                            geom_line(size = 1) +
                            geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha = 0.2, color = NA) +
                            labs(
                                title = paste(.("Hazard Function:"), private$.parametric_model_name),
                                x = .("Time"),
                                y = .("Hazard Rate"),
                                color = self$options$explanatory
                            ) +
                            theme_minimal()
                            
                    } else {
                        hazard_pred <- summary(model, t = time_points, type = "hazard", ci = TRUE)
                        
                        plot_data <- data.frame(
                            time = time_points,
                            hazard = hazard_pred$est,
                            lower = hazard_pred$lcl,
                            upper = hazard_pred$ucl
                        )
                        
                        p <- ggplot(plot_data, aes(x = time, y = hazard)) +
                            geom_line(size = 1, color = "red") +
                            geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, fill = "red") +
                            labs(
                                title = paste(.("Hazard Function:"), private$.parametric_model_name),
                                x = .("Time"),
                                y = .("Hazard Rate")
                            ) +
                            theme_minimal()
                    }
                    
                    print(p)
                    
                }, error = function(e) {
                    # Fallback simple plot if hazard estimation fails
                    p <- ggplot() +
                        annotate("text", x = 0.5, y = 0.5, 
                                label = paste(.("Hazard plot unavailable for"), private$.parametric_model_name),
                                size = 4) +
                        theme_void()
                    print(p)
                })
                
                TRUE
            }
            
            ,
            .plotExtrapolation = function(image, ...) {
                
                if (is.null(private$.parametric_model)) {
                    return()
                }
                
                model <- private$.parametric_model
                results <- private$.parametric_results
                cleanData <- results$cleanData
                
                library(ggplot2)
                library(survival)
                
                max_observed_time <- max(cleanData$CalculatedTime, na.rm = TRUE)
                
                # Set extrapolation time horizon
                if (self$options$extrapolation_time > 0) {
                    max_extrap_time <- self$options$extrapolation_time
                } else {
                    max_extrap_time <- 2 * max_observed_time  # Default: 2x observed time
                }
                
                # Time points for observed period
                observed_times <- seq(0, max_observed_time, length.out = 50)
                # Time points for extrapolation period
                extrap_times <- seq(max_observed_time, max_extrap_time, length.out = 50)
                all_times <- c(observed_times, extrap_times)
                
                if (!is.null(self$options$explanatory) && self$options$parametric_covariates) {
                    groups <- levels(cleanData[[self$options$explanatory]])
                    plot_data <- data.frame()
                    extrap_data <- data.frame()
                    
                    for (group in groups) {
                        # Observed period predictions
                        pred_data_obs <- data.frame(time = observed_times)
                        pred_data_obs[[self$options$explanatory]] <- group
                        surv_pred_obs <- summary(model, newdata = pred_data_obs, t = observed_times, ci = TRUE)
                        
                        obs_data <- data.frame(
                            time = observed_times,
                            survival = surv_pred_obs$surv,
                            lower = surv_pred_obs$lcl,
                            upper = surv_pred_obs$ucl,
                            group = group,
                            period = "Observed"
                        )
                        
                        # Extrapolation period predictions
                        pred_data_ext <- data.frame(time = extrap_times)
                        pred_data_ext[[self$options$explanatory]] <- group
                        surv_pred_ext <- summary(model, newdata = pred_data_ext, t = extrap_times, ci = TRUE)
                        
                        ext_data <- data.frame(
                            time = extrap_times,
                            survival = surv_pred_ext$surv,
                            lower = surv_pred_ext$lcl,
                            upper = surv_pred_ext$ucl,
                            group = group,
                            period = "Extrapolated"
                        )
                        
                        plot_data <- rbind(plot_data, obs_data, ext_data)
                        
                        # Store extrapolation table data
                        extrap_table_times <- seq(max_observed_time, max_extrap_time, length.out = 10)
                        pred_data_table <- data.frame(time = extrap_table_times)
                        pred_data_table[[self$options$explanatory]] <- group
                        surv_pred_table <- summary(model, newdata = pred_data_table, t = extrap_table_times, ci = TRUE)
                        
                        group_extrap <- data.frame(
                            time = extrap_table_times,
                            survival = surv_pred_table$surv,
                            ci_lower = surv_pred_table$lcl,
                            ci_upper = surv_pred_table$ucl,
                            group = group
                        )
                        extrap_data <- rbind(extrap_data, group_extrap)
                    }
                    
                    p <- ggplot(plot_data, aes(x = time, y = survival, color = group)) +
                        geom_line(aes(linetype = period), size = 1) +
                        geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha = 0.2, color = NA) +
                        geom_vline(xintercept = max_observed_time, linetype = "dashed", color = "gray") +
                        annotate("text", x = max_observed_time, y = 0.9, 
                                label = .("End of observed data"), angle = 90, vjust = -0.5) +
                        labs(
                            title = paste(.("Survival Extrapolation:"), private$.parametric_model_name),
                            x = .("Time"),
                            y = .("Survival Probability"),
                            color = self$options$explanatory,
                            linetype = .("Period")
                        ) +
                        theme_minimal() +
                        scale_y_continuous(limits = c(0, 1))
                        
                } else {
                    # Single curve extrapolation
                    surv_pred_obs <- summary(model, t = observed_times, ci = TRUE)
                    surv_pred_ext <- summary(model, t = extrap_times, ci = TRUE)
                    
                    obs_data <- data.frame(
                        time = observed_times,
                        survival = surv_pred_obs$surv,
                        lower = surv_pred_obs$lcl,
                        upper = surv_pred_obs$ucl,
                        period = "Observed"
                    )
                    
                    ext_data <- data.frame(
                        time = extrap_times,
                        survival = surv_pred_ext$surv,
                        lower = surv_pred_ext$lcl,
                        upper = surv_pred_ext$ucl,
                        period = "Extrapolated"
                    )
                    
                    plot_data <- rbind(obs_data, ext_data)
                    
                    p <- ggplot(plot_data, aes(x = time, y = survival, linetype = period)) +
                        geom_line(size = 1, color = "blue") +
                        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, fill = "blue") +
                        geom_vline(xintercept = max_observed_time, linetype = "dashed", color = "gray") +
                        annotate("text", x = max_observed_time, y = 0.9, 
                                label = .("End of observed data"), angle = 90, vjust = -0.5) +
                        labs(
                            title = paste(.("Survival Extrapolation:"), private$.parametric_model_name),
                            x = .("Time"),
                            y = .("Survival Probability"),
                            linetype = .("Period")
                        ) +
                        theme_minimal() +
                        scale_y_continuous(limits = c(0, 1))
                        
                    # Store extrapolation table data for single curve
                    extrap_table_times <- seq(max_observed_time, max_extrap_time, length.out = 10)
                    surv_pred_table <- summary(model, t = extrap_table_times, ci = TRUE)
                    
                    extrap_data <- data.frame(
                        time = extrap_table_times,
                        survival = surv_pred_table$surv,
                        ci_lower = surv_pred_table$lcl,
                        ci_upper = surv_pred_table$ucl
                    )
                }
                
                # Populate extrapolation table
                extrap_table <- self$results$extrapolationTable
                for (i in 1:nrow(extrap_data)) {
                    row_data <- list(
                        time = extrap_data$time[i],
                        survival = extrap_data$survival[i],
                        ci_lower = extrap_data$ci_lower[i],
                        ci_upper = extrap_data$ci_upper[i]
                    )
                    extrap_table$addRow(rowKey = i, values = row_data)
                }
                
                print(p)
                TRUE
            }

        )
    )
