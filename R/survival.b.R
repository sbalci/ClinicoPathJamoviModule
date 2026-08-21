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
#' @param analysistype Type of survival analysis: "overall", "cause", "dfs"
#'   (disease-free: Alive with Disease counts as an event), or "compete"
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
#' data("histopathologySurvival", package = "ClinicoPath")
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
#' @importFrom survival Surv
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme
#' @importFrom flexsurv flexsurvreg flexsurvspline
#' @noRd
NULL

# Note: `.escapeVariableNames` lives in R/utils.R as the canonical definition.

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
.survivalDisplayName <- function(var_name, name_mapping) {
    if (is.null(name_mapping) || is.null(var_name) || length(var_name) == 0) {
        return(as.character(var_name))
    }
    return(unname(name_mapping[var_name] %||% as.character(var_name)))
}

survivalClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "survivalClass",
        inherit = survivalBase,
        private = list(
            .parametric_model = NULL,
            .parametric_model_name = NULL,
            .parametric_results = NULL,
            .cachedGetData = NULL,
            # Result of .defineEventIndicator(), kept so .run() can render the
            # recode disclosure without redoing the work.
            .eventRecode = NULL,
            # TRUE below 10 events: descriptive output runs, models suppressed.
            .lowEventCount = FALSE,

            # HTML notice helper (avoids the protobuf serialization error caused by
            # passing jmvcore::Notice objects to self$results$insert / $add).
            # See R/survivalcont.b.R:700-743 for the reference implementation.
            .addHtmlMessage = function(type, title, message) {
                output_name <- switch(type,
                    "error" = "errors",
                    "strongWarning" = "strongWarnings",
                    "warning" = "warnings",
                    "info" = "infoMessages",
                    "warnings"
                )
                border_color <- switch(type,
                    "error" = "#d9534f",
                    "strongWarning" = "#e67e22",
                    "warning" = "#f0ad4e",
                    "info" = "#5bc0de",
                    "#f0ad4e"
                )
                current_content <- self$results[[output_name]]$content
                if (is.null(current_content)) current_content <- ""
                new_message <- sprintf(
                    '<div style="margin: 10px 0; padding: 10px; border-left: 4px solid %s; background-color: rgba(138, 155, 172, 0.06); color: inherit;"><strong>%s:</strong> %s</div>',
                    border_color,
                    htmltools::htmlEscape(title),
                    htmltools::htmlEscape(message)
                )
                self$results[[output_name]]$setContent(paste0(current_content, new_message))
                self$results[[output_name]]$setVisible(TRUE)
            },

            # Reset all notice outputs to empty/hidden at the start of each run.
            # jamovi persists Html result content across .run() invocations, so
            # without this reset .addHtmlMessage would append a fresh copy of each
            # notice on every option/interaction cycle (e.g. the "Missing outcome
            # values excluded" warning appearing many times). See survivalcont.b.R.
            .initializeMessageOutputs = function() {
                self$results$errors$setContent("")
                self$results$errors$setVisible(FALSE)

                self$results$strongWarnings$setContent("")
                self$results$strongWarnings$setVisible(FALSE)

                self$results$warnings$setContent("")
                self$results$warnings$setVisible(FALSE)

                self$results$infoMessages$setContent("")
                self$results$infoMessages$setVisible(FALSE)
            },

            # Unified survival formula builder - always escapes variable names
            # Original row indices for a cleaned frame.
            #
            # Consumers pull extra covariates out of self$data by POSITION. They
            # used as.integer(rownames(mydata)), which is correct only for as
            # long as every upstream step preserves rownames -- it does today
            # (cleanData is a data.frame), but a tibble anywhere in the chain
            # would silently renumber them to 1:n and join each patient to
            # somebody else's covariate with no error. cleanData is assembled by
            # left_join on a row_names column, so that column is the reliable
            # key; rownames stay as the fallback.
            .originalRowIndex = function(mydata) {
                if (!is.null(mydata[["row_names"]]))
                    return(suppressWarnings(as.integer(as.character(mydata[["row_names"]]))))
                suppressWarnings(as.integer(rownames(mydata)))
            }
            ,
            .buildSurvFormula = function(time_var, outcome_var, group_var = NULL, ns_prefix = TRUE) {
                esc_time <- .escapeVariableNames(time_var)
                esc_outcome <- .escapeVariableNames(outcome_var)
                surv_fn <- if (ns_prefix) "survival::Surv" else "Surv"
                lhs <- paste0(surv_fn, "(", esc_time, ", ", esc_outcome, ")")
                if (is.null(group_var)) {
                    return(lhs)
                }
                esc_group <- .escapeVariableNames(group_var)
                paste(lhs, "~", esc_group)
            },

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
                
                # Parametric models (core: comparison, summary, fitted-vs-KM plot,
                # explanation) are controlled by their .r.yaml `visible:` gates on
                # use_parametric. The extended outputs below are not implemented in
                # this release, so keep them force-hidden regardless of their options.
                
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
                    # Parametric features temporarily disabled for next release
                    # if (self$options$use_parametric) {
                    #     self$results$parametricModelsExplanation$setVisible(TRUE)
                    # }
                    
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
                
                # Parametric models visibility - DISABLED for this release
                # if (self$options$use_parametric) {
                #     self$results$parametricModelSummary$setVisible(TRUE)
                #     if (self$options$compare_distributions) {
                #         self$results$parametricModelComparison$setVisible(TRUE)
                #     }
                #     if (self$options$parametric_diagnostics) {
                #         self$results$parametricDiagnostics$setVisible(TRUE)
                #     }
                #     if (self$options$parametric_survival_plots) {
                #         self$results$parametricSurvivalPlot$setVisible(TRUE)
                #     }
                #     if (self$options$hazard_plots) {
                #         self$results$hazardFunctionPlot$setVisible(TRUE)
                #     }
                #     if (self$options$parametric_extrapolation) {
                #         self$results$extrapolationPlot$setVisible(TRUE)
                #         self$results$extrapolationTable$setVisible(TRUE)
                #     }
                #     if (self$options$showExplanations) {
                #         self$results$parametricModelsExplanation$setVisible(TRUE)
                #     }
                # }
                
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
                # Parametric plots - DISABLED for this release
                # if (self$options$use_parametric) {
                #     # Parametric plots handled in parametric section
                #     self$results$parametricSurvivalPlot$setVisible(TRUE)
                #     if (self$options$hazard_plots) {
                #         self$results$hazardFunctionPlot$setVisible(TRUE)
                #     }
                # }
            }
            ,


            .getData = function() {

            if (!is.null(private$.cachedGetData)) {
                return(private$.cachedGetData)
            }

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

            result <- list(
                "mydata_labelled" = mydata
                , "mytime_labelled" = mytime
                , "myoutcome_labelled" = myoutcome
                , "mydxdate_labelled" = mydxdate
                , "myfudate_labelled" = myfudate
                , "myexplanatory_labelled" = myexplanatory
                , "all_labels" = all_labels
                , "original_names_mapping" = corrected_labels
            )
            private$.cachedGetData <- result
            return(result)


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
                    warning(jmvcore::format(
                        .("Table population failed: {message}"),
                        message = conditionMessage(e)
                    ))
                })
            },
            
            # FIX: Helper functions for competing risk analysis ----
            # These functions provide proper cumulative incidence function (CIF)
            # support for competing risk scenarios (analysistype = "compete")

            .isCompetingRisk = function(state = NULL) {
                # Check if competing risk analysis is active.
                #
                # The STATUS VECTOR decides this, not the options. This used to
                # read `self$options$multievent && analysistype == "compete"`
                # alone, which is blind to the outcomeorganizer hand-off: a
                # recoded Censored/Event/Competing column arrives already 0/1/2
                # with multievent = FALSE -- the user never fills
                # dod/dooc/awd/awod, that is the whole point of the recoded
                # column. The guard was therefore FALSE and the 0/1/2 vector
                # went into an ordinary survival::Surv(), which for a max status
                # of 2 subtracts 1 and NAs anything outside 0/1: Censored became
                # NA (row silently DELETED), Event became censored, and
                # Competing became the event. If this ever reverts to testing
                # the options alone, competing-risk data is analysed backwards
                # again with no warning.
                #
                # `state` is a plot's image$state. jmvcore's .load() restores
                # results from disk without calling .run(), so a renderer can
                # execute where private$.eventRecode is still NULL; the flag
                # then has to come off the serialised state.
                isTRUE(state$has_competing) ||
                    isTRUE(private$.eventRecode$has_competing) ||
                    (isTRUE(self$options$multievent) &&
                         identical(self$options$analysistype, "compete"))
            },

            .competingRiskPlotRefusal = function(feature, ggtheme = NULL) {
                # A renderer that just return()s under competing risks leaves an
                # empty panel, which reads as a rendering glitch rather than a
                # deliberate refusal -- and an empty panel is exactly what the
                # user sees when the plot IS broken. Draw the reason instead.
                p <- ggplot2::ggplot() +
                    ggplot2::annotate(
                        "text", x = 0, y = 0, hjust = 0.5, vjust = 0.5, size = 4,
                        lineheight = 1.2,
                        label = paste(
                            strwrap(.competingRiskUnavailable(feature), width = 60),
                            collapse = "\n")
                    ) +
                    ggplot2::theme_void()
                print(p)
                TRUE
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

            # Define Survival Time ----
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

                    # Validate: no negative survival times
                    n_negative <- sum(mydata[["mytime"]] < 0, na.rm = TRUE)
                    if (n_negative > 0) {
                        jmvcore::reject(sprintf(
                            .("Invalid data: %d observation(s) have negative survival times. Check that follow-up dates are after diagnosis dates and that time values are correct."),
                            n_negative
                        ))
                    }

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
                            # ERROR for invalid date format
                            jmvcore::reject(sprintf(
                                .('Unknown date format: %s\nSupported formats: %s\nPlease select correct format in Date Type options'),
                                self$options$timetypedata,
                                paste(names(lubridate_functions), collapse = ", ")
                            ))
                        }
                    } else {
                        # ERROR for mixed date types
                        jmvcore::reject(.('Diagnosis date and follow-up date must be in the same format (both numeric or both text)\nPlease check your date variables and ensure consistent formatting'))
                    }


                    if ( sum(!is.na(mydata[["start"]])) == 0 || sum(!is.na(mydata[["end"]])) == 0)  {
                        # ERROR for time calculation failure
                        jmvcore::reject(sprintf(
                            .('Time difference cannot be calculated\nDate parsing produced no valid dates\nCurrent date type setting: %s\nPlease verify date format matches your data'),
                            self$options$timetypedata
                        ))
                    }

                    timetypeoutput <-
                        jmvcore::constructFormula(terms = self$options$timetypeoutput)


                    mydata <- mydata %>%
                        dplyr::mutate(interval = lubridate::interval(start, end))



                    mydata <- mydata %>%
                        dplyr::mutate(mytime = lubridate::time_length(interval, timetypeoutput))

                    # Validate: no negative survival times from date calculation
                    n_negative <- sum(mydata[["mytime"]] < 0, na.rm = TRUE)
                    if (n_negative > 0) {
                        jmvcore::reject(sprintf(
                            .("Invalid data: %d observation(s) have negative survival times (follow-up date before diagnosis date). Please check your date variables."),
                            n_negative
                        ))
                    }

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


                # Delegated to the shared coder in survival_utils.R so that all
                # five analyses that build an event indicator agree on validation
                # and on what happens to unselected levels and to NA.
                res <- .defineEventIndicator(
                    outcome      = mydata[[myoutcome_labelled]],
                    outcomeLevel = self$options$outcomeLevel,
                    multievent   = self$options$multievent,
                    analysistype = self$options$analysistype,
                    dod          = self$options$dod,
                    dooc         = self$options$dooc,
                    awd          = self$options$awd,
                    awod         = self$options$awod,
                    outcome_name = self$options$outcome
                )

                if (!is.null(res$error))
                    jmvcore::reject(res$error, code = "outcome_recode")

                private$.eventRecode <- res
                mydata[["myoutcome"]] <- res$status

                if (res$n_missing > 0) {
                    private$.addHtmlMessage(
                        "warning",
                        "Missing outcome values excluded",
                        sprintf("%d row(s) with missing outcome were excluded by jmvcore::naOmit() before model fitting.", res$n_missing)
                    )
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
                    jmvcore::reject(.("Error: Data could not be cleaned for analysis."))
                }

                cleanData <- dplyr::left_join(time, outcome, by = "row_names") %>%
                    dplyr::left_join(factor, by = "row_names")

                # Landmark ----
                # https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#landmark_method
                if (self$options$uselandmark) {

                  landmark <- jmvcore::toNumeric(self$options$landmark)

                  n_before_landmark <- nrow(cleanData)

                  # Apply landmark filtering
                  cleanData <- cleanData %>%
                    dplyr::filter(mytime >= landmark) %>%
                    dplyr::mutate(mytime = mytime - landmark)

                  if (nrow(cleanData) == 0) {
                      jmvcore::reject(sprintf(
                          .("Landmark time %.2f is beyond all observed follow-up times; no patients remain at risk."),
                          landmark
                      ))
                  }

                  # Add landmark exclusion info as table note (safe from serialization)
                  n_excluded_landmark <- n_before_landmark - nrow(cleanData)
                  if (n_excluded_landmark > 0) {
                      self$results$medianTable$setNote("landmark",
                          paste0("Landmark analysis at ", self$options$landmark, " ",
                                 self$options$timetypeoutput,
                                 ": ", n_excluded_landmark,
                                 " patients excluded (events/censoring before landmark). ",
                                 .("All times are measured FROM the landmark, not from study entry, and all estimates are conditional on being event-free at the landmark; they are not comparable with unlandmarked survival times.")))
                  }
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
                    "cleanData" = cleanData,
                    # The competing-risk flag has to TRAVEL WITH THE STATE.
                    # jmvcore's Analysis$.load() restores results from disk and
                    # can render a plot without ever calling .run(), so
                    # private$.eventRecode is NULL in that instance and the
                    # renderer's guard would silently fall through to FALSE.
                    "has_competing" = private$.isCompetingRisk()
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

            # Main Run Function (Refactored) ----
            ,
            .run = function() {

                # Reset cached data for this analysis cycle
                private$.cachedGetData <- NULL

                # Reset notice outputs so notices don't accumulate across runs
                private$.initializeMessageOutputs()

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

                # Populate subtitle with explanatory variable
                if (!is.null(self$options$explanatory)) {
                    subtitle_text <- paste0("Survival Analysis - ", htmltools::htmlEscape(self$options$explanatory))
                    self$results$subtitle$setContent(subtitle_text)
                }

                # Get Clean Data ----
                results <- private$.cleandata()

                # Always disclose how the outcome was recoded. A silent recode is a
                # clinical-safety hazard: the reader of a survival curve cannot otherwise
                # see which levels were collapsed into "censored", nor which estimand
                # the probability-scale outputs actually correspond to.
                if (!is.null(private$.eventRecode))
                    self$results$eventRecodeInfo$setContent(
                        .describeEventIndicator(private$.eventRecode, self$options$outcome))

                private$.checkpoint()  # Add checkpoint here


                if (is.null(results)) {
                    return()
                }

                # Clinical Safety: Event Count Checking ----
                mydata <- results$cleanData
                outcome_col <- results$name2outcome
                n_events <- sum(mydata[[outcome_col]] == 1, na.rm = TRUE)
                n_total <- nrow(mydata)

                # Very few events: warn, but still produce descriptive output.
                # This used to jmvcore::reject(), so the user got nothing at all.
                # There is no universal safe event count independent of model
                # complexity, and a small series still has a legitimate KM curve,
                # median and event count. Model-based output is gated separately
                # below, where a low event count genuinely does invalidate it.
                private$.lowEventCount <- n_events < 10
                if (private$.lowEventCount) {
                    private$.addHtmlMessage(
                        "warning",
                        sprintf("Only %d event(s) observed", n_events),
                        paste0("Descriptive results (Kaplan-Meier, median, counts) are shown, but with ",
                               n_events, " event(s) they are unstable and confidence intervals will be ",
                               "very wide. Hazard ratios and other model-based output are suppressed."))
                }

                # Low event count warnings via table notes (safe from serialization issues)
                if (n_events >= 10 && n_events < 20) {
                    self$results$medianTable$setNote("lowevents",
                        paste0("Caution: Only ", n_events, " events detected. Results may be unreliable. Consider increasing sample size or simplifying the model."))
                } else if (n_events >= 20 && n_events < 50) {
                    self$results$medianTable$setNote("moderateevents",
                        paste0("Note: ", n_events, " events detected. Adequate for basic KM/Cox but limited for complex models (calibration, RCS, bootstrap)."))
                }

                # Run Analysis ----
                ## Median Survival ----
                    private$.medianSurv(results)
                private$.checkpoint()  # Add checkpoint here
                
                ## RMST Analysis ----
                # The competing-risk refusal now lives INSIDE .calculateRMST()
                # (single gate, single note). The gate used to be here and tested
                # `multievent && analysistype == "compete"`, which is FALSE for an
                # outcomeorganizer hand-off -- so a 0/1/2 status walked straight
                # into survfit() and the table was populated with inverted RMSTs
                # (39.01 / 39.54 instead of 42.27 / 42.66) computed on 20 of 30
                # records, under a comment saying that must not happen.
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
                        for (i in seq_len(nrow(rmst_results$table))) {
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

                        # Add tau selection note
                        if (is.null(self$options$rmst_tau) || self$options$rmst_tau <= 0) {
                            actual_tau <- rmst_results$table$Tau[1]
                            self$results$rmstTable$setNote("tau_default",
                                sprintf("Time horizon (tau) was automatically set to %.1f: the smaller of the overall 75th percentile of follow-up and the maximum follow-up supported in every group. Specify a supported custom tau for a clinically meaningful horizon.", actual_tau))
                        }
                    }
                }
                private$.checkpoint()  # Add checkpoint here

                ## Cox ----
                # Every competing-risk gate below asks private$.isCompetingRisk(),
                # never `multievent && analysistype == "compete"`. The option pair
                # is blind to the outcomeorganizer hand-off, which delivers an
                # already-recoded 0/1/2 status with multievent = FALSE -- so every
                # one of these blocks used to run on a status vector that
                # survival::Surv() silently inverts (1 -> censored, 2 -> event,
                # 0 -> NA). And each refusal now says so on the output it blocks:
                # an empty table with no note reads as a glitch, not a decision.
                if (private$.isCompetingRisk()) {
                    # Every producer below is skipped, so its setState() never
                    # runs -- and a state left over from the PREVIOUS (valid)
                    # outcome variable would still render: a correct-looking
                    # curve sitting next to a refusal note. Drop them. The KM
                    # family (plot/plot2/plot3/plot6/plot7) does not need this;
                    # its state is written every run and carries has_competing,
                    # so those renderers refuse on their own.
                    for (p in c("plot8", "residualsPlot", "calibrationPlot",
                                "rcsPlot", "parametricSurvivalPlot"))
                        self$results[[p]]$setState(NULL)
                    self$results$coxTable$setNote("cr", .competingRiskUnavailable("Cox regression"))
                    private$.addHtmlMessage(
                        "info",
                        "Cox regression skipped (competing risks)",
                        "Competing-risk mode is selected; standard Cox regression is skipped because cause-specific hazards require a different model (e.g., Fine-Gray subdistribution)."
                    )
                } else if (private$.lowEventCount) {
                    self$results$coxTable$setNote("lowevents",
                        "Cox regression suppressed: fewer than 10 events.")
                } else {
                    private$.cox(results)
                }
                private$.checkpoint()  # Add checkpoint here

                ## Age-Adjusted Cox ----
                if (self$options$age_adjustment && !is.null(self$options$age_variable)) {
                    if (private$.isCompetingRisk()) {
                        self$results$ageAdjustedCoxTable$setNote(
                            "cr", .competingRiskUnavailable("Age-adjusted Cox regression"))
                    } else {
                        private$.ageAdjustedCox(results)
                    }
                }
                private$.checkpoint()

                ## Age as Time Scale ----
                if (self$options$age_adjustment && self$options$age_time_scale && !is.null(self$options$age_variable)) {
                    if (private$.isCompetingRisk()) {
                        self$results$ageTimeScaleTable$setNote(
                            "cr", .competingRiskUnavailable("Age as time scale"))
                    } else {
                        private$.ageTimeScaleCox(results)
                    }
                }
                private$.checkpoint()

                ## Age Standardization (SMR) ----
                if (self$options$age_adjustment && self$options$age_standardization && !is.null(self$options$age_variable)) {
                    if (private$.isCompetingRisk()) {
                        self$results$ageStandardizationTable$setNote(
                            "cr", .competingRiskUnavailable("Age standardization (SMR)"))
                    } else {
                        private$.ageStandardization(results)
                    }
                }
                private$.checkpoint()

                ## Survival Table ----
                    private$.survTable(results)
                private$.checkpoint()  # Add checkpoint here

                ## Export Survival Data ----
                # Same reason as RMST above: the exported survival estimates come
                # from a survfit built on the 0/1/2 status, which Surv() silently
                # inverts.
                if (private$.isCompetingRisk()) {
                    private$.addHtmlMessage(
                        "warning",
                        "Survival data export skipped (competing risks)",
                        .competingRiskUnavailable("Exported survival estimates")
                    )
                } else {
                    private$.exportSurvivalData(results)
                }
                private$.checkpoint()  # Add checkpoint here

                ## Parametric Survival Models (flexsurv) ----
                # .parametricSurvival() carries its own .isCompetingRisk() gate.
                if (self$options$use_parametric) {
                    private$.parametricSurvival(results)
                }
                private$.checkpoint()  # Add checkpoint here

                ## Calibration Curves ----
                if (self$options$calibration_curves) {
                    if (private$.isCompetingRisk()) {
                        self$results$calibrationTable$setNote(
                            "cr", .competingRiskUnavailable("Calibration curves"))
                    } else if (!private$.lowEventCount) {
                        private$.calculateCalibration(results)
                    }
                }
                private$.checkpoint()

                ## Non-Linearity Assessment (RCS) ----
                if (self$options$rcs_analysis && !is.null(self$options$rcs_variable)) {
                    if (private$.isCompetingRisk()) {
                        self$results$rcsTestTable$setNote(
                            "cr", .competingRiskUnavailable("Restricted cubic spline assessment"))
                    } else {
                        private$.calculateRCS(results)
                    }
                }
                private$.checkpoint()

                ## Pairwise ----
                if (self$options$pw) {
                    if (private$.isCompetingRisk()) {
                        self$results$pairwiseTable$setNote(
                            "cr", .competingRiskUnavailable("Pairwise log-rank comparisons"))
                    } else {
                        private$.pairwise(results)
                    }
                }

                ## Weighted Log-Rank Tests ----
                if (self$options$weightedLogRank) {
                    if (private$.isCompetingRisk()) {
                        self$results$weightedLogRankTable$setNote(
                            "cr", .competingRiskUnavailable("Weighted log-rank tests"))
                    } else {
                        private$.calculateWeightedLogRank(results)
                    }
                }
                private$.checkpoint()

                ## Bootstrap Internal Validation ----
                if (self$options$bootstrapValidation) {
                    if (private$.isCompetingRisk()) {
                        self$results$bootstrapValidationTable$setNote(
                            "cr", .competingRiskUnavailable("Bootstrap internal validation"))
                    } else if (!private$.lowEventCount) {
                        private$.calculateBootstrapValidation(results)
                    }
                }
                private$.checkpoint()

                ## REMARK Checklist ----
                if (self$options$remark_checklist) {
                    private$.generateRemarkChecklist(results)
                }
                private$.checkpoint()

                ## Add the person-time analysis ----
                private$.checkpoint()  # Add checkpoint here

                # Run person-time analysis if enabled
                if (self$options$person_time) {
                    if (private$.isCompetingRisk()) {
                        self$results$personTimeTable$setNote(
                            "cr", .competingRiskUnavailable("Person-time incidence rates"))
                    } else {
                        private$.personTimeAnalysis(results)
                    }
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

                # Analysis completion summary
                tryCatch({
                    n_obs <- nrow(results$cleanData)
                    # Count only primary events (status == 1), matching the event-count
                    # check above. .eventIndicator() returns status >= 1, which would also
                    # count competing events (status 2) in competing-risk mode.
                    n_events <- sum(results$cleanData[[results$name2outcome]] == 1, na.rm = TRUE)
                    event_rate <- if (n_obs > 0) (n_events / n_obs) * 100 else NA_real_
                    private$.addHtmlMessage(
                        "info",
                        "Analysis complete",
                        sprintf(
                            "Survival analysis completed on %d observations with %d events (%.1f%% event rate).",
                            n_obs, n_events, event_rate
                        )
                    )
                }, error = function(e) invisible(NULL))
            }

            # RMST Analysis Function ----
            ,
            .calculateRMST = function(results, tau = NULL) {
                # Restricted Mean Survival Time calculation
                #
                # RMST is the area under a Kaplan-Meier curve, so it assumes a
                # single event type. Under the competing-risk 0/1/2 coding
                # survival::Surv() does not error -- it warns "Invalid status
                # value, converted to NA" and remaps 1 to censored, 2 to event and
                # 0 to NA. The table stays populated and looks authoritative while
                # being computed backwards on a shrunken denominator. Refuse here
                # rather than at the call site, so every caller is covered.
                if (private$.isCompetingRisk()) {
                    self$results$rmstTable$setNote(
                        "cr", .competingRiskUnavailable("Restricted mean survival time"))
                    return(NULL)
                }
                tryCatch({
                    mytime <- results$name1time
                    myoutcome <- results$name2outcome
                    myfactor <- results$name3explanatory
                    mydata <- results$cleanData
                    
                    mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])
                    
                    # RMST comparisons require a common horizon supported by every
                    # group. survival::summary(..., extend = TRUE) otherwise carries
                    # the last KM estimate beyond a group's final observation, which
                    # is extrapolation rather than observed restricted mean survival.
                    group_max <- tapply(mydata[[mytime]], mydata[[myfactor]], max,
                                        na.rm = TRUE)
                    max_supported_tau <- min(group_max[is.finite(group_max)])
                    if (!is.finite(max_supported_tau) || max_supported_tau <= 0) {
                        self$results$rmstTable$setNote(
                            "support",
                            "RMST could not be calculated because no positive follow-up horizon is supported in every group.")
                        return(NULL)
                    }

                    if (is.null(tau)) {
                        tau <- min(
                            as.numeric(stats::quantile(mydata[[mytime]], 0.75,
                                                       na.rm = TRUE, names = FALSE)),
                            max_supported_tau
                        )
                    } else if (!is.finite(tau) || tau <= 0 || tau > max_supported_tau) {
                        self$results$rmstTable$setNote(
                            "support",
                            sprintf(
                                "RMST time horizon must be greater than 0 and no larger than %.2f, the maximum follow-up supported in every group.",
                                max_supported_tau
                            ))
                        return(NULL)
                    }

                    formula <- .asSurvivalFormula(private$.buildSurvFormula(mytime, myoutcome, myfactor))

                    private$.checkpoint()

                    km_fit <- survival::survfit(formula, data = mydata)

                    # Calculate RMST for each group
                    rmst_summary <- summary(km_fit, rmean = tau, extend = TRUE)
                    
                    rmst_values <- rmst_summary$table[, "rmean"]
                    rmst_se <- rmst_summary$table[, "se(rmean)"]
                    rmst_table <- data.frame(
                        Group = gsub(paste0(myfactor, "="), "", names(km_fit$strata)),
                        RMST = round(rmst_values, 2),
                        SE = round(rmst_se, 2),
                        Tau = rep(round(tau, 1), length(km_fit$strata)),
                        stringsAsFactors = FALSE
                    )

                    # Wald intervals supplied on the unrounded estimate scale.
                    rmst_table$CI_Lower <- round(rmst_values - 1.96 * rmst_se, 2)
                    rmst_table$CI_Upper <- round(rmst_values + 1.96 * rmst_se, 2)
                    
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
                        interpretation = paste("Error calculating RMST:", htmltools::htmlEscape(e$message))
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

                                    # CIF quantile CIs require specialized methods
                                    # (e.g., bootstrap or Fieller-type intervals);
                                    # the Wald approximation median +/- 1.96*SE is not
                                    # valid for cumulative incidence quantiles.
                                    ci_lower <- NA
                                    ci_upper <- NA
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

                    formula <- .asSurvivalFormula(private$.buildSurvFormula(mytime, myoutcome, myfactor))

                    private$.checkpoint()

                    km_fit <- survival::survfit(formula, data = mydata)

                    km_fit_median_df <- summary(km_fit)





                    tbl_raw <- km_fit_median_df$table
                    if (!is.matrix(tbl_raw)) {
                        tbl_df <- as.data.frame(t(tbl_raw))
                        grp_name <- if (!is.null(myfactor) && myfactor %in% names(mydata)) as.character(mydata[[myfactor]][1]) else "Overall"
                        rownames(tbl_df) <- paste0(myexplanatory_labelled, "=", grp_name)
                    } else {
                        tbl_df <- as.data.frame(tbl_raw)
                    }

                    results1html <- tbl_df %>%
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

                # Add note for competing risk median CI limitation
                if (private$.isCompetingRisk()) {
                    medianTable$setNote("crci",
                        "Confidence intervals for competing risk median times are not available (CIF quantile CIs require specialized methods such as bootstrap).")
                }

                ## Median Survival Summary ----

                # survfit may return character "NA" values for undefined
                # medians/CIs in very small or single-group datasets. Coerce
                # explicitly before formatting; round("NA") raises and used to
                # abort an otherwise valid descriptive analysis.
                median_num <- suppressWarnings(as.numeric(results2table$median))
                lower_num <- suppressWarnings(as.numeric(results2table$x0_95lcl))
                upper_num <- suppressWarnings(as.numeric(results2table$x0_95ucl))

                km_fit_median_definition <- vapply(
                    seq_len(nrow(results2table)),
                    function(i) {
                        factor_label <- as.character(results2table$factor[i])
                        factor_label <- gsub("=", " is ", factor_label)
                        factor_label <- gsub(
                            myexplanatory_labelled,
                            self$options$explanatory,
                            factor_label,
                            fixed = TRUE
                        )

                        if (!is.finite(median_num[i])) {
                            return(paste0(
                                "When ", factor_label,
                                ", median survival is undefined because the survival curve ",
                                "does not fall to 0.5 during observed follow-up."
                            ))
                        }

                        ci_text <- if (is.finite(lower_num[i]) && is.finite(upper_num[i])) {
                            sprintf(" [%.1f - %.1f, 95%% CI]", lower_num[i], upper_num[i])
                        } else {
                            " [95% CI not estimable]"
                        }
                        sprintf(
                            "When %s, median survival is %.1f%s %s.",
                            factor_label,
                            median_num[i],
                            ci_text,
                            self$options$timetypeoutput
                        )
                    },
                    character(1)
                )

                medianSummary <- c(km_fit_median_definition,
                                   "The median survival time is when 50% of subjects have experienced the event.",
                                   "This means that 50% of subjects in this group survived longer than this time period.",
                                   "Note: Confidence intervals are calculated on the log scale (survfit default, conf.type = 'log') using Greenwood's variance estimate, which improves accuracy with censored data."
                                   )

                self$results$medianSummary$setContent(medianSummary)

                # Add explanatory output for median survival
                if (self$options$showExplanations) {
                    median_explanation_html <- '
                    <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 152, 255, 0.07); border-left: 4px solid #4169e1; color: inherit;">
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
                            <li>Groups with longer median survival have better prognosis in this sample</li>
                            <li>Do not judge significance by eye from the two confidence intervals: non-overlapping
                                intervals are conservative evidence of a difference, but overlapping intervals
                                do <strong>not</strong> mean the groups are similar. Use the log-rank test.</li>
                            <li>The median is undefined whenever the curve does not fall to 50% during follow-up</li>
                            <li>These are observational comparisons; consider clinical significance alongside
                                statistical significance, and confounding before attributing the difference
                                to the grouping variable</li>
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
                
                # Skip if competing risk analysis
                if (private$.isCompetingRisk()) {
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

                mydata <- results$cleanData

                mydata[[mytime]] <-
                    jmvcore::toNumeric(mydata[[mytime]])

                myformula <- private$.buildSurvFormula(mytime, myoutcome, ns_prefix = FALSE)

                # Add stratified Cox regression if enabled
                strata_var <- NULL
                if (isTRUE(self$options$stratified_cox) &&
                    (is.null(self$options$strata_variable) ||
                     identical(self$options$strata_variable, ""))) {
                    self$results$coxTable$setNote(
                        "strata_required",
                        "Stratified Cox regression was requested, but no stratification variable was selected. The Cox model was not fitted."
                    )
                    return()
                }
                if (self$options$stratified_cox && !is.null(self$options$strata_variable) && self$options$strata_variable != "") {
                    strata_var <- self$options$strata_variable
                    # Pull strata variable from self$data (not cleanData which only has time/outcome/factor)
                    if (strata_var %in% names(self$data)) {
                        strata_col <- self$data[[strata_var]]
                        mydata[[strata_var]] <- strata_col[private$.originalRowIndex(mydata)]
                        mydata <- mydata[!is.na(mydata[[strata_var]]), , drop = FALSE]
                        if (length(unique(mydata[[strata_var]])) < 2) {
                            self$results$coxTable$setNote(
                                "constant_strata",
                                "The selected stratification variable has fewer than two observed levels. The Cox model was not fitted."
                            )
                            return()
                        }
                    } else {
                        warning(jmvcore::format(
                            .("Stratification variable {variable} not found. Using standard Cox regression."),
                            variable = strata_var
                        ))
                        strata_var <- NULL
                    }
                }

                private$.checkpoint()

                if (length(unique(mydata[[myfactor]])) < 2) {
                    self$results$coxTable$setNote(
                        "single_group",
                        "Cox regression requires at least two observed groups. Descriptive survival results remain available."
                    )
                    return()
                }

                # EPV (events per variable) check
                n_events_cox <- sum(mydata[[myoutcome]] == 1, na.rm = TRUE)
                n_levels <- length(unique(mydata[[myfactor]])) - 1  # df for factor
                if (n_levels > 0) {
                    epv <- n_events_cox / n_levels
                    if (epv < 5) {
                        self$results$coxTable$setNote("epv_critical",
                            sprintf("Events per variable = %.1f (< 5). Cox model is likely unreliable; consider reducing categories or increasing sample size.", epv))
                    } else if (epv < 10) {
                        self$results$coxTable$setNote("epv_warning",
                            sprintf("Events per variable = %.1f (< 10). Cox model may be unstable; interpret hazard ratios with caution.", epv))
                    }
                }

                # Always use unstratified factors for finalfit to prevent 'not found' or C-stack errors
                explanatory_formula <- myfactor

                finalfit::finalfit(
                    .data = mydata,
                    dependent = myformula,
                    explanatory = explanatory_formula,
                    metrics = TRUE
                ) -> tCox

                # Cox model convergence check
                tryCatch({
                    cox_check_formula <- .asSurvivalFormula(paste(myformula, "~", explanatory_formula))
                    cox_check_model <- survival::coxph(cox_check_formula, data = mydata)
                    if (!is.null(cox_check_model$iter) && cox_check_model$iter >= 20) {
                        self$results$coxTable$setNote("convergence",
                            sprintf("Cox model used %d iterations (maximum reached). Model may not have converged; results should be interpreted with caution.", cox_check_model$iter))
                    }
                }, error = function(e) {
                    self$results$coxTable$setNote("convergence_skipped",
                        "Cox convergence check could not be performed. Review model metrics carefully.")
                })

                # Manually calculate and override stratified HRs if strata requested
                if (!is.null(strata_var) && strata_var %in% names(mydata)) {
                    strat_applied <- FALSE
                    factors_list <- trimws(unlist(strsplit(explanatory_formula, "\\+")))
                    for (var in factors_list) {
                        strat_form <- .asSurvivalFormula(paste(myformula, "~", var, "+ strata(", jmvcore::composeTerm(strata_var), ")"))
                        strat_mod <- try(survival::coxph(strat_form, data = mydata), silent = TRUE)
                        if (!inherits(strat_mod, "try-error")) {
                            mod_sum <- summary(strat_mod)
                            coefs <- mod_sum$coefficients
                            cis <- mod_sum$conf.int
                            for (i in seq_len(nrow(coefs))) {
                                term_name <- rownames(coefs)[i]
                                hr <- sprintf("%.2f", cis[i, 1])
                                lower <- sprintf("%.2f", cis[i, 3])
                                upper <- sprintf("%.2f", cis[i, 4])
                                pval <- coefs[i, 5]
                                pval_str <- if (pval < 0.001) "<0.001" else sprintf("%.3f", pval)
                                formatted_hr <- sprintf("%s (%s-%s, p=%s)", hr, lower, upper, pval_str)
                                level_name <- sub(paste0("^", var), "", term_name)
                                # Overwrite univariable HR column.
                                #
                                # finalfit prints the variable name only on the
                                # FIRST row of each block and leaves continuation
                                # rows blank/NA, so matching column 1 against
                                # `var` never found the row carrying the level
                                # (for a two-level factor that is always the
                                # non-reference row). The overwrite therefore
                                # never fired: the table kept the UNSTRATIFIED
                                # hazard ratios while the metrics line below was
                                # unconditionally relabelled "Stratified by ...".
                                # Forward-fill the variable column before matching.
                                var_col <- as.character(tCox[[1]][, 1])
                                var_col[var_col == ""] <- NA_character_
                                for (k in seq_along(var_col))
                                    if (is.na(var_col[k]) && k > 1) var_col[k] <- var_col[k - 1]

                                idx <- which(var_col == var &
                                             as.character(tCox[[1]][, 2]) == level_name)
                                if (length(idx) >= 1) {
                                    tCox[[1]][idx[1], 4] <- formatted_hr
                                    tCox[[1]][idx[1], 5] <- formatted_hr
                                    strat_applied <- TRUE
                                }
                            }
                        }
                    }
                    
                    # Update model metrics for multivariable stratified model
                    multi_form <- .asSurvivalFormula(paste(myformula, "~", explanatory_formula, "+ strata(", jmvcore::composeTerm(strata_var), ")"))
                    multi_mod <- try(survival::coxph(multi_form, data = mydata), silent = TRUE)
                    if (!inherits(multi_mod, "try-error") && isTRUE(strat_applied)) {
                        tCox[[2]][[1]] <- paste("Stratified by", strata_var, "-", tCox[[2]][[1]])
                    } else if (!isTRUE(strat_applied)) {
                        # Never claim stratification that was not applied.
                        tCox[[2]][[1]] <- paste(
                            "NOT stratified (the stratified estimates could not be matched to the table) -",
                            tCox[[2]][[1]])
                    }
                }
                
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

                  tCoxtext2 <- paste0(tCoxtext2,
                                      sprintf(.("<br><b>Landmark analysis at %s %s.</b>"),
                                              format(landmark),
                                              self$options$timetypeoutput),
                                      .(" All times reported here are measured FROM the landmark, not from study entry, and all estimates are conditional on being event-free at the landmark. They are not comparable with unlandmarked survival times.")
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

                # Extreme HR detection
                tryCatch({
                    hr_strings <- data_frame$HR_univariable[data_frame$HR_univariable != "-"]
                    hr_numeric <- as.numeric(gsub("^([0-9.]+).*", "\\1", hr_strings))
                    hr_numeric <- hr_numeric[!is.na(hr_numeric)]
                    if (length(hr_numeric) > 0) {
                        if (any(hr_numeric > 10 | hr_numeric < 0.1)) {
                            coxTable$setNote("extreme_hr",
                                "Some hazard ratios are extreme (HR > 10 or < 0.1), which may indicate sparse data, complete separation, or data quality issues. Verify group sizes and event counts.")
                        }
                    }
                }, error = function(e) {
                    # Silently skip HR check on parse failure
                })

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
                            "For {Explanatory}, the {Levels} group had an estimated hazard ratio of {HR_univariable} relative to the reference group ({firstlevel}). \n A hazard ratio is a ratio of instantaneous event rates among those still at risk, not a ratio of cumulative risks, and it assumes the ratio stays constant over follow-up. If the confidence interval includes 1, the data are compatible with no difference in hazard."

                        )
                    ) %>%
                    dplyr::filter(HR_univariable != '-') %>%
                    dplyr::pull(coxdescription) -> coxSummary




                coxSummary <- unlist(coxSummary)

                coxSummary <- c(coxSummary,
                                "A hazard ratio greater than 1 indicates a higher event rate than the reference group, and less than 1 a lower event rate. These are unadjusted, univariable associations estimated from observational data: they are not adjusted for confounding, they are not causal effects, and they assume proportional hazards over the follow-up period."
                )

                self$results$coxSummary$setContent(coxSummary)

                # Add explanatory output for Cox regression
                if (self$options$showExplanations) {
                    cox_explanation_html <- '
                    <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 152, 255, 0.07); border-left: 4px solid #4169e1; color: inherit;">
                        <h4 style="margin-top: 0; color: #2c3e50;">Understanding Cox Proportional Hazards Regression</h4>
                        <p style="margin-bottom: 10px;">Cox regression models the relationship between explanatory variables and the hazard (risk) of experiencing the event:</p>
                        <ul style="margin-left: 20px;">
                            <li><strong>Hazard Ratio (HR):</strong> Ratio of instantaneous event rates versus the reference group (a relative rate, not a cumulative risk)</li>
                            <li><strong>HR > 1:</strong> Increased risk of event occurrence</li>
                            <li><strong>HR < 1:</strong> Decreased risk of event occurrence</li>
                            <li><strong>P-value:</strong> Statistical significance of the association</li>
                            <li><strong>95% CI:</strong> Range of plausible values for the true HR</li>
                        </ul>
                        <p style="margin-bottom: 5px;"><strong>Clinical interpretation:</strong></p>
                        <ul style="margin-left: 20px;">
                            <li>HR = 2.0 means twice the hazard compared to reference</li>
                            <li>HR = 0.5 means half the hazard compared to reference</li>
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

                    formula <- .asSurvivalFormula(private$.buildSurvFormula(mytime, myoutcome, myfactor))

                private$.checkpoint()  # Add checkpoint here


                    cox_model <- survival::coxph(formula, data = mydata)
                                                 # , na.action = na.exclude)

                    zph <- survival::cox.zph(cox_model)

                    # capture.output() rather than print(): print() returns the raw
                    # matrix, so the panel showed p = 0.855053 where the console
                    # shows 0.86 -- misleading precision on a diagnostic p-value --
                    # and the print() itself leaked to stdout on every run cycle.
                    self$results$cox_ph$setContent(
                        paste(utils::capture.output(print(zph)), collapse = "\n"))

                    # PH assumption violation notice - rendered as HTML to avoid Notice protobuf issues
                    tryCatch({
                        ph_p <- zph$table[, "p"]
                        ph_p <- ph_p[!is.na(ph_p)]
                        if (length(ph_p) > 0 && any(ph_p < 0.05)) {
                            private$.addHtmlMessage(
                                "warning",
                                "Proportional hazards assumption may be violated",
                                "cox.zph p-values below 0.05 indicate potential violation of the proportional hazards assumption for one or more terms. Consider time-varying effects, stratification, or splitting follow-up; see the PH interpretation section for details."
                            )
                        }
                    }, error = function(e) invisible(NULL))

                    # Generate enhanced PH interpretation
                    ph_interpretation <- private$.generatePHInterpretation(zph, myfactor)
                    self$results$phInterpretation$setContent(ph_interpretation)

                    image8 <- self$results$plot8
                    # Store serializable components of cox.zph for plotting
                    # (the full cox.zph object contains call/function refs that
                    #  can cause protobuf serialization issues)
                    zph_state <- list(
                        table = as.data.frame(zph$table),
                        time = zph$time,
                        n_vars = ncol(zph$y),
                        var_names = colnames(zph$y)
                    )
                    # Store each variable's residuals as a data.frame column
                    zph_state$y <- as.data.frame(zph$y)
                    if (!is.null(zph$x)) {
                        zph_state$x <- as.data.frame(zph$x)
                    }
                    image8$setState(zph_state)
                    
                    # Add residual diagnostics if enabled
                    if (self$options$residual_diagnostics) {
                        residuals_data <- private$.calculateResiduals(cox_model, mydata)
                        
                        # Populate residuals table
                        if (!is.null(residuals_data)) {
                            residuals_table <- self$results$residualsTable
                            for (i in seq_len(nrow(residuals_data))) {
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
                                residuals_data = residuals_data
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
                        observation = seq_along(martingale_res),
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
                    warning(jmvcore::format(
                        .("Error calculating residuals: {message}"),
                        message = conditionMessage(e)
                    ))
                    return(NULL)
                })
            }

            # Generate PH Interpretation Function ----
            ,
            .generatePHInterpretation = function(zph, covariate_name) {
                covariate_name <- htmltools::htmlEscape(covariate_name)
                tryCatch({
                    # Extract test results
                    zph_table <- zph$table
                    p_value <- zph_table[nrow(zph_table), "p"]  # GLOBAL p-value

                    # Determine if PH assumption is violated
                    ph_violated <- p_value < 0.05

                    # Build HTML interpretation
                    html <- paste0(
                        "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>",
                        "<h4 style='margin-top: 0; color: #2c3e50;'>Proportional Hazards Assessment</h4>"
                    )

                    # Status indicator
                    if (ph_violated) {
                        html <- paste0(html,
                            "<div style='background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; padding: 15px; margin: 15px 0; border-radius: 4px; color: inherit;'>",
                            "<strong style='color: #856404;'> WARNING: Proportional Hazards Assumption May Be Violated</strong><br/>",
                            sprintf("<p style='margin: 10px 0 0 0;'>Global test p-value = %.4f (p < 0.05 suggests violation)</p>", p_value),
                            "</div>"
                        )
                    } else {
                        html <- paste0(html,
                            "<div style='background-color: rgba(33, 162, 64, 0.19); border-left: 4px solid #28a745; padding: 15px; margin: 15px 0; border-radius: 4px; color: inherit;'>",
                            "<strong style='color: #155724;'> No Evidence Against the Proportional Hazards Assumption</strong><br/>",
                            sprintf("<p style='margin: 10px 0 0 0;'>Global test p-value = %.4f (p \u2265 0.05)</p>", p_value),
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
                            "<div style='background-color: rgba(33, 144, 255, 0.11); padding: 15px; border-radius: 4px; margin: 15px 0; color: inherit;'>",
                            "<h5 style='color: #0056b3; margin-top: 0;'> Recommended Solutions:</h5>",
                            "<ol style='line-height: 1.8; margin: 10px 0;'>",
                            "<li><strong>Stratified Cox Model:</strong> Stratify by '", covariate_name, "' if it has few categories",
                            "<pre style='background-color: rgba(255, 255, 255, 0.06); padding: 10px; margin: 5px 0; border-left: 3px solid #0056b3; color: inherit;'>",
                            "survival::coxph(Surv(time, status) ~ other_vars + strata(", covariate_name, "))",
                            "</pre></li>",
                            "<li><strong>Time-Dependent Coefficients:</strong> Add interaction with time",
                            "<pre style='background-color: rgba(255, 255, 255, 0.06); padding: 10px; margin: 5px 0; border-left: 3px solid #0056b3; color: inherit;'>",
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
                            "<div style='background-color: rgba(33, 159, 43, 0.1); padding: 15px; border-radius: 4px; margin: 15px 0; color: inherit;'>",
                            "<h5 style='color: #2e7d32; margin-top: 0;'> Next Steps:</h5>",
                            "<p style='margin: 5px 0;'>The test did not detect a departure from proportional hazards. ",
                            "That is absence of evidence, not evidence of absence: a non-significant test is also ",
                            "what you see with few events or short follow-up. Inspect the Schoenfeld residual plot ",
                            "before relying on a constant hazard ratio.</p>",
                            "<p style='margin: 10px 0 0 0;'><em>Note: Also examine the Schoenfeld residual plot above for visual confirmation.</em></p>",
                            "</div>"
                        )
                    }

                    # Close main div
                    html <- paste0(html, "</div>")

                    return(html)

                }, error = function(e) {
                    return(paste0("<p>Error generating PH interpretation: ", htmltools::htmlEscape(e$message), "</p>"))
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

                    formula <- .asSurvivalFormula(private$.buildSurvFormula(mytime, myoutcome, myfactor))

                    private$.checkpoint()

                    km_fit <- survival::survfit(formula, data = mydata)

                    # Generate time points for export (every unit from 0 to max time)
                    max_time <- max(mydata[[mytime]], na.rm = TRUE)
                    export_times <- seq(0, max_time, by = max(1, floor(max_time/100)))
                    
                    # Get survival estimates at specified times
                    km_export <- summary(km_fit, times = export_times, extend = TRUE)
                    # For each patient in cleanData, extract estimated KM survival probability at their observation time
                    times <- mydata[[mytime]]
                    surv_vals <- numeric(nrow(mydata))
                    for (i in seq_len(nrow(mydata))) {
                        s_sum <- tryCatch(summary(km_fit, times = times[i], extend = TRUE), error = function(e) NULL)
                        if (is.null(s_sum) || is.null(s_sum$surv) || length(s_sum$surv) == 0) {
                            surv_vals[i] <- NA_real_
                        } else {
                            surv_vals[i] <- round(s_sum$surv[1], 4)
                        }
                    }

                    # Add to data sheet as exportable Output column
                    self$results$survivalExport$setRowNums(results$cleanData$row_names)
                    self$results$survivalExport$setValues(surv_vals)

                    # Create summary for user
                    export_summary <- paste0(
                        "<h4>Survival Data Export Summary</h4>",
                        "<p>Exported estimated Kaplan-Meier survival probabilities for ", nrow(mydata), " observations to the data sheet.</p>",
                        "<p>Time range: ", round(min(times, na.rm=TRUE), 1), " to ", round(max(times, na.rm=TRUE), 1), " ", self$options$timetypeoutput, "</p>"
                    )
                    
                    self$results$survivalExportSummary$setContent(export_summary)
                    
                }, error = function(e) {
                    warning(jmvcore::format(
                        .("Error exporting survival data: {message}"),
                        message = conditionMessage(e)
                    ))
                })
            }


            # Survival Table Function ----
            ,
            .survTable = function(results) {
                # Skip if competing risk analysis
                if (private$.isCompetingRisk()) {
                    return()
                }
                
                mytime <- results$name1time
                myoutcome <- results$name2outcome
                myfactor <- results$name3explanatory
                myexplanatory_labelled <- results$myexplanatory_labelled

                mydata <- results$cleanData

                mydata[[mytime]] <-
                    jmvcore::toNumeric(mydata[[mytime]])

                ## Median Survival Table ----

                formula <- .asSurvivalFormula(private$.buildSurvFormula(mytime, myoutcome, myfactor))

                private$.checkpoint()

                km_fit <- survival::survfit(formula, data = mydata)

                # Cutpoint parsing. `as.numeric()` on a non-numeric token yields NA,
                # and summary.survfit() then aborts with "times contains missing
                # values" -- which killed the WHOLE analysis, not just this table,
                # because a single typo in a free-text option propagated out of
                # .run(). Parse defensively and report what was ignored.
                raw_tokens <- trimws(unlist(strsplit(self$options$cutp, ",")))
                raw_tokens <- raw_tokens[nzchar(raw_tokens)]
                utimes <- suppressWarnings(as.numeric(raw_tokens))

                bad_tokens <- raw_tokens[is.na(utimes) | utimes < 0]
                utimes <- sort(unique(utimes[!is.na(utimes) & utimes >= 0]))

                if (length(bad_tokens) > 0) {
                    survTableNotes <- paste0(
                        "Ignored cutpoint(s) that are not non-negative numbers: ",
                        paste(bad_tokens, collapse = ", "), ".")
                } else {
                    survTableNotes <- NULL
                }

                if (length(utimes) == 0) {
                    utimes <- c(12, 36, 60)
                    survTableNotes <- paste(c(survTableNotes,
                        "No usable cutpoints were supplied, so the default 12, 36, 60 was used."),
                        collapse = " ")
                }

                private$.checkpoint()  # Add checkpoint here

                km_fit_summary <- summary(km_fit, times = utimes, extend = TRUE)

                summary_strata <- km_fit_summary$strata
                if (is.null(summary_strata))
                    summary_strata <- rep("Overall", length(km_fit_summary$time))

                km_fit_df <- data.frame(
                    strata = as.character(summary_strata),
                    time = km_fit_summary$time,
                    n.risk = km_fit_summary$n.risk,
                    n.event = km_fit_summary$n.event,
                    surv = km_fit_summary$surv,
                    std.err = km_fit_summary$std.err,
                    lower = km_fit_summary$lower,
                    upper = km_fit_summary$upper,
                    stringsAsFactors = FALSE
                )

                # self$results$tableview$setContent(km_fit_df)

                # Do not report survival BEYOND a group's observed follow-up.
                #
                # `extend = TRUE` is required so that every group gets a row at
                # every requested cutpoint, but it also carries the last
                # Kaplan-Meier estimate forward indefinitely. With the default
                # cutpoints (12, 36, 60) and a cohort followed for two years, the
                # table used to report a "60-month survival" -- complete with a
                # confidence interval -- computed from ZERO patients at risk. For a
                # group whose last record happens to be an event the carried-forward
                # value is 0%, i.e. the table asserted that nobody survives five
                # years on the strength of two years of data.
                #
                # This is the same extrapolation that .calculateRMST() already
                # refuses; apply the rule here too. Drop the unsupported rows and
                # say which ones went, so a missing row never reads as a glitch.
                if (!is.null(km_fit$strata)) {
                    group_values <- as.character(mydata[[myfactor]])
                    group_max <- tapply(mydata[[mytime]], group_values, max, na.rm = TRUE)
                    strata_max <- stats::setNames(as.numeric(group_max),
                                                  paste0(myfactor, "=", names(group_max)))
                } else {
                    strata_max <- stats::setNames(
                        max(mydata[[mytime]], na.rm = TRUE), "Overall")
                }

                row_max <- strata_max[km_fit_df$strata]
                beyond_support <- !is.na(row_max) & km_fit_df$time > row_max

                if (any(beyond_support)) {
                    dropped <- unique(km_fit_df$time[beyond_support])
                    survTableNotes <- paste(c(survTableNotes, sprintf(
                        paste0("Cutpoint(s) %s exceed the observed follow-up in at least one group ",
                               "(longest follow-up per group: %s %s) and were omitted. ",
                               "Kaplan-Meier estimates beyond the last observation are extrapolation, ",
                               "not estimates, and would be shown with no patients still at risk."),
                        paste(dropped, collapse = ", "),
                        paste(sprintf("%s = %.1f", gsub(paste0("^", myfactor, "="), "",
                                                        names(strata_max)),
                                      as.numeric(strata_max)), collapse = "; "),
                        self$options$timetypeoutput)), collapse = " ")

                    km_fit_df <- km_fit_df[!beyond_support, , drop = FALSE]
                }

                if (!is.null(survTableNotes)) {
                    self$results$survTable$setNote("cutpoints", survTableNotes)
                }

                if (nrow(km_fit_df) == 0) {
                    self$results$survTableSummary$setContent(
                        "No requested time point falls within the observed follow-up of every group.")
                    return()
                }

                km_fit_df2 <- km_fit_df

                km_fit_df[, 1] <- gsub(
                    pattern = paste0(myexplanatory_labelled,"="),
                    replacement = paste0(
                        # self$options$explanatory,
                        ""),
                    x = km_fit_df[, 1],
                    fixed = TRUE
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
                    x = km_fit_df2[, 1],
                    fixed = TRUE
                )

                # Determine time unit label from user option
                time_unit_label <- self$options$timetypeoutput
                if (is.null(time_unit_label)) time_unit_label <- "month"
                # Singularize for label (e.g., "months" -> "month")
                time_unit_label <- sub("s$", "", time_unit_label)

                km_fit_df2 %>%
                    dplyr::mutate(
                        description =
                            glue::glue(
                                "When {strata}, {time} {time_unit_label} survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]. \n For the {strata} group, the estimated probability of surviving beyond {time} {time_unit_label}s was {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]. \n At this time point, there were {n.risk} subjects still at risk and {n.event} events had occurred in this group."

                            )
                    ) %>%
                    dplyr::select(description) %>%
                    dplyr::pull(.) -> survTableSummary

                survTableSummary <- htmltools::htmlEscape(survTableSummary)

                survTableSummaryWithNote <- c(survTableSummary,
                                              "Note: Confidence intervals for survival probabilities are calculated on the log scale (survfit default, conf.type = 'log') using Greenwood's variance estimate, for better performance with censored survival data.")
                
                self$results$survTableSummary$setContent(survTableSummaryWithNote)

            }


            # Pairwise Function ----
            ,
            .pairwise = function(results) {
                ##  pairwise comparison ----
                
                # Skip if competing risk analysis
                if (private$.isCompetingRisk()) {
                    return()
                }

                mytime <- results$name1time
                myoutcome <- results$name2outcome
                myfactor <- results$name3explanatory

                mydata <- results$cleanData

                mydata[[mytime]] <-
                    jmvcore::toNumeric(mydata[[mytime]])

                ## Median Survival Table ----

                formula_p <- .asSurvivalFormula(private$.buildSurvFormula(mytime, myoutcome, myfactor))

                padjustmethod <-
                    jmvcore::constructFormula(terms = self$options$padjustmethod)

                private$.checkpoint()

                # Determine rho for weighted log-rank (0 = standard log-rank)
                pw_rho <- 0
                if (self$options$weightedLogRank) {
                    test_type <- self$options$survivalTestType
                    # Keys encode the Fleming-Harrington rho passed to survdiff;
                    # rho=1 is exactly the Peto-Peto test. "logrank" leaves pw_rho = 0.
                    if (test_type == "fh_rho0_5") {
                        pw_rho <- 0.5
                    } else if (test_type == "fh_rho1") {
                        pw_rho <- 1
                    }
                }

                results_pairwise <-
                    survminer::pairwise_survdiff(formula = formula_p,
                                                 data = mydata,
                                                 p.adjust.method = padjustmethod,
                                                 rho = pw_rho)


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
                title2 <- .survivalDisplayName(self$options$explanatory, original_names_mapping)

                pairwiseTable$setTitle(jmvcore::format(
                    .("Pairwise comparisons: {factor}"),
                    factor = title2
                ))

                pairwiseTable$setNote(
                    key = padjustmethod,
                    note = paste0("p-value adjustment method: ",
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

                                "The survival difference between {rowname} and {name} groups was tested using a {ifelse(pw_rho == 0, 'log-rank', ifelse(pw_rho == 1, 'Peto-Peto (Fleming-Harrington rho=1)', paste0('Fleming-Harrington weighted log-rank (rho=', pw_rho, ')')))} test. The p-value of {format.pval(value, digits = 3, eps = 0.001)} {ifelse(value < 0.05, 'indicates a statistically significant difference', 'suggests no statistically significant difference')} in survival between these groups (using {padjustmethod} adjustment for multiple comparisons)."



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
            # Weighted Log-Rank Tests Function ----
            .calculateWeightedLogRank = function(results) {
                if (!self$options$weightedLogRank) {
                    return()
                }

                # Skip if competing risk analysis
                if (private$.isCompetingRisk()) {
                    return()
                }

                mytime <- results$name1time
                myoutcome <- results$name2outcome
                myfactor <- results$name3explanatory
                mydata <- results$cleanData

                mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])

                # Check that there is an explanatory variable with >1 level
                if (is.null(self$options$explanatory) || length(unique(mydata[[myfactor]])) < 2) {
                    return()
                }

                formula_obj <- .asSurvivalFormula(private$.buildSurvFormula(mytime, myoutcome, myfactor))

                # Define the tests to run.
                # NOTE: survival::survdiff(rho=) implements the Fleming-Harrington
                # G-rho family with weights S(t)^rho. It CANNOT produce Gehan-Breslow
                # (weights = number at risk) or Tarone-Ware (weights = sqrt of number at
                # risk), so those tests are not offered here. rho=1 is exactly the
                # Peto-Peto modification of the Gehan-Wilcoxon test (weights = KM S(t)).
                tests <- list(
                    list(name = "Log-Rank (standard)", rho = 0,
                         weighting = "Equal weighting across all time points"),
                    list(name = "Fleming-Harrington (rho=0.5)", rho = 0.5,
                         weighting = "Weights S(t)^0.5; moderate emphasis on early differences"),
                    list(name = "Peto-Peto (Fleming-Harrington rho=1)", rho = 1,
                         weighting = "Weights S(t); emphasizes early events (Peto-Peto modification of Gehan-Wilcoxon)")
                )

                table <- self$results$weightedLogRankTable

                for (i in seq_along(tests)) {
                    test_info <- tests[[i]]

                    tryCatch({
                        sd_result <- survival::survdiff(formula_obj,
                                                        data = mydata,
                                                        rho = test_info$rho)

                        chisq_val <- sd_result$chisq
                        df_val <- length(sd_result$n) - 1
                        p_val <- stats::pchisq(chisq_val, df = df_val, lower.tail = FALSE)

                        table$addRow(rowKey = i, values = list(
                            test = test_info$name,
                            rho = test_info$rho,
                            chisq = chisq_val,
                            df = df_val,
                            pvalue = p_val,
                            weighting = test_info$weighting
                        ))
                    }, error = function(e) {
                        table$addRow(rowKey = i, values = list(
                            test = test_info$name,
                            rho = test_info$rho,
                            weighting = paste("Error:", e$message)
                        ))
                    })
                }

                # Get display name for title
                labelled_data <- private$.getData()
                original_names_mapping <- labelled_data$original_names_mapping
                title2 <- .survivalDisplayName(self$options$explanatory, original_names_mapping)
                table$setTitle(paste0("Weighted Log-Rank Tests - ", title2))

                table$setNote("tests", paste0(
                    "All tests compare survival distributions across levels of ",
                    title2, ". These are members of the Fleming-Harrington G-rho ",
                    "family (survival::survdiff): rho=0 is the standard log-rank test ",
                    "(equal weight at all time points); larger rho weights early events ",
                    "more heavily (weights = S(t)^rho). When survival curves cross or the ",
                    "proportional hazards assumption is violated, different rho values may ",
                    "yield different conclusions."
                ))
                table$setNote("petopeto",
                    "survival::survdiff(rho=1) is exactly the Peto-Peto modification of the Gehan-Wilcoxon test (weights = Kaplan-Meier S(t)). The Gehan-Breslow (weights = number at risk) and Tarone-Ware (weights = sqrt of number at risk) tests use different weights and cannot be produced by survdiff()."
                )

                # Populate explanation if summaries enabled
                if (self$options$showSummaries) {
                    private$.populateWeightedLogRankExplanation()
                }
            }


            ,
            .populateWeightedLogRankExplanation = function() {
                html <- paste0(
                    "<h3>Weighted Log-Rank Tests</h3>",
                    "<p>The Fleming-Harrington family of weighted log-rank tests ",
                    "generalizes the standard log-rank test by applying different ",
                    "weights to events at different time points. The weighting is ",
                    "controlled by the parameter <b>rho (&#961;)</b>:</p>",
                    "<table border='1' cellpadding='5' style='border-collapse:collapse;'>",
                    "<tr><th>Test</th><th>&#961;</th><th>Weighting</th><th>Best For</th></tr>",
                    "<tr><td>Log-Rank (standard)</td><td>0</td>",
                    "<td>Equal weight at all time points</td>",
                    "<td>General use; optimal under proportional hazards</td></tr>",
                    "<tr><td>Fleming-Harrington (&#961;=0.5)</td><td>0.5</td>",
                    "<td>Weights = S(t)<sup>0.5</sup> (moderate early emphasis)</td>",
                    "<td>Compromise between log-rank and Peto-Peto</td></tr>",
                    "<tr><td>Peto-Peto (Fleming-Harrington &#961;=1)</td><td>1</td>",
                    "<td>Weights = S(t), the Kaplan-Meier estimate (early emphasis)</td>",
                    "<td>Detecting early differences; robust to late censoring</td></tr>",
                    "</table>",
                    "<p><i>Note: survival::survdiff() implements only the ",
                    "Fleming-Harrington G-&#961; family. The Gehan-Breslow (weights = ",
                    "number at risk) and Tarone-Ware (weights = sqrt of number at risk) ",
                    "tests use different weights and are not available via survdiff().</i></p>",
                    "<h4>When to Use Weighted Tests</h4>",
                    "<ul>",
                    "<li><b>If curves cross:</b> The standard log-rank test may miss ",
                    "differences when survival curves cross. Weighted tests can detect ",
                    "early or late divergence that the standard test misses.</li>",
                    "<li><b>If early events matter more:</b> Use Peto-Peto ",
                    "(&#961;=1) when early mortality is of primary clinical interest ",
                    "(e.g., perioperative outcomes, acute treatment toxicity).</li>",
                    "<li><b>Sensitivity analysis:</b> Running multiple weighted tests ",
                    "can reveal whether conclusions depend on the time horizon of interest.</li>",
                    "</ul>",
                    "<h4>Interpreting Discordant Results</h4>",
                    "<p>If the standard log-rank test is significant but ",
                    "the Peto-Peto (&#961;=1) test is not (or vice versa), this suggests ",
                    "the survival difference is concentrated in a specific part of the ",
                    "curve. Reporting both tests provides a more complete picture of ",
                    "the survival comparison.</p>",
                    "<p><i>Reference: Harrington DP, Fleming TR (1982). ",
                    "A class of rank test procedures for censored survival data. ",
                    "Biometrika, 69(3), 553-566.</i></p>"
                )
                self$results$weightedLogRankExplanation$setContent(html)
            }


            ,
            # Bootstrap Internal Validation Function ----
            .calculateBootstrapValidation = function(results) {
                if (!self$options$bootstrapValidation) {
                    return()
                }

                # Skip if competing risk analysis
                if (private$.isCompetingRisk()) {
                    return()
                }

                mytime <- results$name1time
                myoutcome <- results$name2outcome
                mydata <- results$cleanData

                mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])

                # Need at least an explanatory variable for a meaningful model
                if (is.null(self$options$explanatory)) {
                    self$results$bootstrapValidationTable$setNote(
                        "info",
                        "Bootstrap validation requires an explanatory variable in the Cox model."
                    )
                    return()
                }

                myfactor <- results$name3explanatory
                n_boot <- self$options$bootstrapValN

                # Need sufficient events for validation
                n_events <- sum(mydata[[myoutcome]] == 1, na.rm = TRUE)
                if (n_events < 10) {
                    self$results$bootstrapValidationTable$setNote(
                        "warning",
                        paste0("Only ", n_events, " events observed. Bootstrap validation ",
                               "requires adequate events (recommended: >= 10) for stable estimates.")
                    )
                    return()
                }

                table <- self$results$bootstrapValidationTable

                tryCatch({
                    # Build formula for Cox model
                    formula_str <- paste0(
                        'survival::Surv(', jmvcore::composeTerm(mytime), ', ', jmvcore::composeTerm(myoutcome), ') ~ ', jmvcore::composeTerm(myfactor)
                    )
                    formula_obj <- .asSurvivalFormula(formula_str)

                    # Fit standard Cox model and get apparent C-index
                    cox_fit <- survival::coxph(formula_obj, data = mydata)
                    apparent_concordance <- survival::concordance(cox_fit)
                    c_apparent <- apparent_concordance$concordance

                    # Bootstrap internal validation (Harrell's optimism approach)
                    # User-configurable seed for reproducibility (defaults to 42).
                    seed_val <- self$options$seed
                    if (is.null(seed_val)) seed_val <- 42
                    set.seed(seed_val)
                    # Pre-initialise to NA so failed bootstrap draws stay NA
                    # without needing `<<-` from the error handler below.
                    optimism_values <- rep(NA_real_, n_boot)
                    slope_values <- rep(NA_real_, n_boot)
                    successful_boots <- 0

                    for (b in seq_len(n_boot)) {
                        tryCatch({
                            # Draw bootstrap sample (with replacement)
                            boot_idx <- sample(nrow(mydata), replace = TRUE)
                            boot_data <- mydata[boot_idx, , drop = FALSE]

                            # Fit model on bootstrap sample
                            boot_fit <- survival::coxph(formula_obj, data = boot_data)

                            # Apparent performance on bootstrap sample
                            boot_concordance <- survival::concordance(boot_fit)
                            c_boot <- boot_concordance$concordance

                            # Performance of bootstrap model on ORIGINAL data
                            # Use linear predictor from boot model applied to original data
                            lp_orig <- tryCatch(
                                predict(boot_fit, newdata = mydata, type = "lp"),
                                error = function(e) NULL
                            )

                            if (!is.null(lp_orig)) {
                                # Compute concordance of bootstrap LP on original data
                                # Note: concordance(Surv ~ x) treats higher x as better prognosis
                                # but Cox LP has higher = worse prognosis, so use reverse=TRUE
                                surv_obj <- survival::Surv(mydata[[mytime]], mydata[[myoutcome]])
                                test_concordance <- survival::concordance(
                                    surv_obj ~ lp_orig, reverse = TRUE
                                )
                                c_test <- test_concordance$concordance

                                optimism_values[b] <- c_boot - c_test
                                successful_boots <- successful_boots + 1

                                # Calibration slope: regress original outcome on bootstrap LP
                                slope_fit <- tryCatch(
                                    survival::coxph(surv_obj ~ lp_orig),
                                    error = function(e) NULL
                                )
                                if (!is.null(slope_fit)) {
                                    slope_values[b] <- coef(slope_fit)[1]
                                } else {
                                    slope_values[b] <- NA
                                }
                            } else {
                                optimism_values[b] <- NA
                                slope_values[b] <- NA
                            }
                        }, error = function(e) {
                            # optimism_values[b] / slope_values[b] remain NA
                            # (arrays pre-initialised to NA_real_); no <<- needed.
                            NULL
                        })
                    }

                    # Compute corrected metrics
                    valid_optimism <- optimism_values[!is.na(optimism_values)]
                    valid_slopes <- slope_values[!is.na(slope_values)]

                    if (length(valid_optimism) < 10) {
                        self$results$bootstrapValidationTable$setNote(
                            "warning",
                            paste0("Only ", length(valid_optimism),
                                   " successful bootstrap resamples. Results may be unreliable.")
                        )
                        return()
                    }

                    mean_optimism <- mean(valid_optimism)
                    c_corrected <- c_apparent - mean_optimism
                    mean_slope <- mean(valid_slopes, na.rm = TRUE)

                    # Somers' Dxy = 2 * (C - 0.5)
                    dxy_apparent <- 2 * (c_apparent - 0.5)
                    dxy_corrected <- 2 * (c_corrected - 0.5)
                    dxy_optimism <- dxy_apparent - dxy_corrected

                    # Populate table
                    table$addRow(rowKey = 1, values = list(
                        metric = "C-index (Harrell's concordance)",
                        apparent = round(c_apparent, 4),
                        optimism = round(mean_optimism, 4),
                        corrected = round(c_corrected, 4),
                        n_bootstrap = as.integer(length(valid_optimism))
                    ))

                    table$addRow(rowKey = 2, values = list(
                        metric = "Somers' Dxy",
                        apparent = round(dxy_apparent, 4),
                        optimism = round(dxy_optimism, 4),
                        corrected = round(dxy_corrected, 4),
                        n_bootstrap = as.integer(length(valid_optimism))
                    ))

                    table$addRow(rowKey = 3, values = list(
                        metric = "Calibration slope",
                        apparent = 1.0000,
                        optimism = round(1 - mean_slope, 4),
                        corrected = round(mean_slope, 4),
                        n_bootstrap = as.integer(length(valid_slopes))
                    ))

                    # Get display name
                    labelled_data <- private$.getData()
                    original_names_mapping <- labelled_data$original_names_mapping
                    title2 <- .survivalDisplayName(self$options$explanatory, original_names_mapping)

                    table$setTitle(paste0("Bootstrap Internal Validation - ", title2))

                    # Interpretation note
                    interp <- ifelse(
                        mean_optimism > 0.05,
                        "Substantial optimism detected; the apparent C-index overestimates the model's true discriminative ability.",
                        ifelse(mean_optimism > 0.02,
                               "Moderate optimism detected; the corrected C-index provides a more realistic estimate.",
                               "Minimal optimism; the apparent performance is close to the internally validated estimate."
                        )
                    )

                    slope_interp <- ifelse(
                        mean_slope < 0.8,
                        " Calibration slope < 0.8 suggests the model predictions are too extreme (overfitting).",
                        ifelse(mean_slope < 0.9,
                               " Calibration slope slightly below 1 indicates mild overfitting.",
                               " Calibration slope near 1 indicates good calibration."
                        )
                    )

                    table$setNote("interpretation", paste0(
                        interp, slope_interp,
                        " Based on ", length(valid_optimism), " successful bootstrap resamples."
                    ))

                    # Populate explanation if summaries enabled
                    if (self$options$showSummaries) {
                        private$.populateBootstrapValidationExplanation(
                            c_apparent, c_corrected, mean_optimism, mean_slope,
                            length(valid_optimism), n_events
                        )
                    }

                }, error = function(e) {
                    table$setNote("error", paste0(
                        "Bootstrap validation failed: ", htmltools::htmlEscape(e$message)
                    ))
                })
            }


            ,
            .populateBootstrapValidationExplanation = function(
                c_apparent, c_corrected, optimism, cal_slope,
                n_successful, n_events
            ) {

                # C-index quality rating
                c_rating <- if (c_corrected >= 0.8) "excellent"
                            else if (c_corrected >= 0.7) "good"
                            else if (c_corrected >= 0.6) "moderate"
                            else "poor"

                html <- paste0(
                    "<h3>Bootstrap Internal Validation</h3>",
                    "<p>Internal validation assesses whether a Cox model's apparent ",
                    "performance is optimistic due to overfitting. The bootstrap method ",
                    "(Harrell 1996) estimates the <b>optimism</b>: the amount by which ",
                    "the apparent C-index overestimates the model's true discriminative ",
                    "ability on new data.</p>",

                    "<h4>Results Summary</h4>",
                    "<ul>",
                    "<li><b>Apparent C-index:</b> ", round(c_apparent, 3),
                    " (performance on the data used to build the model)</li>",
                    "<li><b>Optimism:</b> ", round(optimism, 3),
                    " (average over-estimation from ", n_successful, " bootstrap resamples)</li>",
                    "<li><b>Corrected C-index:</b> ", round(c_corrected, 3),
                    " (realistic estimate of model performance; rated as <b>", c_rating, "</b>)</li>",
                    "<li><b>Calibration slope:</b> ", round(cal_slope, 3),
                    " (ideal = 1.0; values &lt; 1 indicate overfitting)</li>",
                    "</ul>",

                    "<h4>Interpretation Guide</h4>",
                    "<table border='1' cellpadding='5' style='border-collapse:collapse;'>",
                    "<tr><th>Metric</th><th>Ideal Value</th><th>Concern Threshold</th></tr>",
                    "<tr><td>C-index</td><td>\u{2265} 0.7</td><td>&lt; 0.6 (poor discrimination)</td></tr>",
                    "<tr><td>Optimism</td><td>&lt; 0.02</td><td>&gt; 0.05 (overfitting)</td></tr>",
                    "<tr><td>Calibration slope</td><td>1.0</td><td>&lt; 0.8 (predictions too extreme)</td></tr>",
                    "</table>",

                    "<h4>How It Works</h4>",
                    "<p>For each of ", n_successful, " bootstrap resamples:</p>",
                    "<ol>",
                    "<li>A bootstrap sample (same size, with replacement) is drawn</li>",
                    "<li>The Cox model is fit on the bootstrap sample (apparent performance)</li>",
                    "<li>The bootstrap model is tested on the ORIGINAL data (test performance)</li>",
                    "<li>Optimism = apparent - test performance</li>",
                    "</ol>",
                    "<p>The corrected C-index = apparent C-index minus average optimism.</p>",

                    "<h4>Clinical Context</h4>",
                    "<p>This model was built on ", n_events, " events. ",
                    if (n_events < 20) {
                        "With fewer than 20 events, the model is at high risk of overfitting. "
                    } else if (n_events < 50) {
                        "With 20-50 events, moderate caution is warranted. "
                    } else {
                        "With 50+ events, the model has reasonable stability. "
                    },
                    "External validation on independent data is always recommended before ",
                    "clinical use.</p>",

                    "<p><i>Reference: Harrell FE Jr, Lee KL, Mark DB (1996). ",
                    "Multivariable prognostic models: issues in developing models, ",
                    "evaluating assumptions and adequacy, and measuring and reducing errors. ",
                    "Statistics in Medicine, 15(4), 361-387.</i></p>"
                )
                self$results$bootstrapValidationExplanation$setContent(html)
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
                # Count only primary events (event type 1), not competing events (type 2)
                total_events <- sum(mydata[[myoutcome]] == 1, na.rm = TRUE)

                # Get time unit
                time_unit <- self$options$timetypeoutput

                # Get rate multiplier
                rate_multiplier <- self$options$rate_multiplier

                # Calculate overall incidence rate
                overall_rate <- (total_events / total_time) * rate_multiplier

                # Calculate confidence intervals using Poisson exact method
                ci_lower <- (stats::qchisq(0.025, 2*total_events) / 2) / total_time * rate_multiplier
                ci_upper <- (stats::qchisq(0.975, 2*(total_events + 1)) / 2) / total_time * rate_multiplier

                # Initialize global row counter
                rowKey_counter <- 1

                # Exact (Garwood) bounds: a row with almost no accrued person-time
                # genuinely cannot rule out a very high rate. Correct, but it looks
                # like a bug without a footnote saying so. Do not cap it.
                self$results$personTimeTable$setNote(
                    "ci",
                    .("Exact (Garwood) Poisson 95% CI. Rows with 0 events give a one-sided 97.5% upper bound; intervals with very little accrued person-time yield correspondingly wide bounds."))

                # Add to personTimeTable - first the overall row
                self$results$personTimeTable$addRow(rowKey=rowKey_counter, values=list(
                    interval=paste0("Overall (0-max)"),
                    events=total_events,
                    person_time=round(total_time, 2),
                    rate=round(overall_rate, 2),
                    rate_ci_lower=round(ci_lower, 2),
                    rate_ci_upper=round(ci_upper, 2)
                ))
                rowKey_counter <- rowKey_counter + 1

                # FIX: Add group-stratified person-time analysis
                # If explanatory variable exists, calculate person-time for each group
                myexplanatory <- results$name3explanatory
                if (!is.null(myexplanatory) && myexplanatory %in% names(mydata)) {
                    # Get unique groups
                    groups <- unique(mydata[[myexplanatory]])
                    groups <- groups[!is.na(groups)]  # Remove NA groups

                    for (group in groups) {
                        # Filter data for this group
                        group_data <- mydata[mydata[[myexplanatory]] == group, ]

                        if (nrow(group_data) > 0) {
                            # Calculate group-specific metrics
                            group_time <- sum(group_data[[mytime]], na.rm = TRUE)
                            group_events <- sum(group_data[[myoutcome]] == 1, na.rm = TRUE)

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
                time_intervals <- suppressWarnings(as.numeric(unlist(strsplit(self$options$time_intervals, ","))))
                time_intervals <- sort(unique(time_intervals[!is.na(time_intervals) & time_intervals > 0]))

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
                            # Count only primary events (type 1) within this interval
                            events_in_interval <- sum(mydata[[myoutcome]] == 1 & mydata[[mytime]] <= end_time, na.rm = TRUE)
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

                            # Count only primary events (type 1) within this interval
                            events_in_interval <- sum(interval_data[[myoutcome]] == 1 &
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
                            self$results$personTimeTable$addRow(rowKey=rowKey_counter, values=list(
                                interval=paste0(start_time, "-", end_time),
                                events=events_in_interval,
                                person_time=round(person_time_in_interval, 2),
                                rate=round(interval_rate, 2),
                                rate_ci_lower=round(interval_ci_lower, 2),
                                rate_ci_upper=round(interval_ci_upper, 2)
                            ))
                            rowKey_counter <- rowKey_counter + 1
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
                    <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 152, 255, 0.07); border-left: 4px solid #4169e1; color: inherit;">
                        <h4 style="margin-top: 0; color: #2c3e50;">Understanding Person-Time Analysis</h4>
                        <p style="margin-bottom: 10px;">Person-time analysis calculates incidence rates by accounting for the total time each patient was at risk:</p>
                        <ul style="margin-left: 20px;">
                            <li><strong>Person-Time:</strong> Sum of individual follow-up periods for all patients in each group</li>
                            <li><strong>Incidence Rate:</strong> Events per unit time (e.g., per 100 person-years)</li>
                            <li><strong>95% CI:</strong> Confidence interval calculated using the exact (Garwood) Poisson method</li>
                        </ul>
                        <p style="margin-bottom: 5px;"><strong>Clinical interpretation:</strong></p>
                        <ul style="margin-left: 20px;">
                            <li>Person-time analysis handles varying follow-up durations effectively</li>
                            <li>Higher rates indicate more frequent event occurrence in that group</li>
                            <li>Particularly useful when follow-up times vary significantly between groups</li>
                            <li>These are <strong>crude</strong> rates: each assumes a constant hazard over the
                                interval it summarises, and none is adjusted for any other variable. Where the
                                hazard changes with time, prefer the Kaplan-Meier and Cox output above</li>
                            <li>No rate ratio or between-group test is computed here; use the log-rank test and
                                the hazard ratio for group comparisons</li>
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

                # Ask .isCompetingRisk(state), not the options. The option pair
                # `multievent && analysistype == "compete"` is FALSE for an
                # outcomeorganizer hand-off (0/1/2 status, multievent unset), so
                # this guard never fired and the curve rendered fully INVERTED:
                # the group holding every real event drew a flat line at 1.000
                # while the group with zero real events fell to 0.225. The flag is
                # read off image$state because .load() can render without .run().
                if (private$.isCompetingRisk(image$state))
                    return(private$.competingRiskPlotRefusal(
                        "The Kaplan-Meier survival curve", ggtheme))

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

                myformula <- private$.buildSurvFormula(mytime, myoutcome)

                # Get original display name for plot title
                labelled_data <- private$.getData()
                original_names_mapping <- labelled_data$original_names_mapping
                title2 <- .survivalDisplayName(myfactor, original_names_mapping)

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
                        title = jmvcore::format(
                            .("Survival curves for {group}"),
                            group = title2
                        ),
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

                # See .plot(): the option pair misses the outcomeorganizer
                # hand-off and this curve renders inverted.
                if (private$.isCompetingRisk(image2$state))
                    return(private$.competingRiskPlotRefusal(
                        "The cumulative-events plot", ggtheme))

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

                myformula <- private$.buildSurvFormula(mytime, myoutcome)

                # Get original display name for plot title
                labelled_data <- private$.getData()
                original_names_mapping <- labelled_data$original_names_mapping
                title2 <- .survivalDisplayName(myfactor, original_names_mapping)

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
                        title = jmvcore::format(
                            .("Cumulative events for {group}"),
                            group = title2
                        ),
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

                # See .plot(): the option pair misses the outcomeorganizer
                # hand-off and this curve renders inverted.
                if (private$.isCompetingRisk(image3$state))
                    return(private$.competingRiskPlotRefusal(
                        "The cumulative-hazard plot", ggtheme))

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

                myformula <- private$.buildSurvFormula(mytime, myoutcome)

                # Get original display name for plot title
                labelled_data <- private$.getData()
                original_names_mapping <- labelled_data$original_names_mapping
                title2 <- .survivalDisplayName(myfactor, original_names_mapping)

                plot3 <- plotData %>%
                    finalfit::surv_plot(
                        .data = .,
                        dependent = myformula,
                        explanatory = myfactor,
                        xlab = paste0('Time (', self$options$timetypeoutput, ')'),
                        ylab = "Cumulative Hazard",
                        pval = self$options$pplot,
                        pval.method	= self$options$pplot,
                        legend = 'none',
                        break.time.by = self$options$byplot,
                        xlim = c(0, self$options$endplot),
                        # For cumulative hazard, use NULL to allow auto-scaling beyond 1.0
                        ylim = NULL,
                        title = jmvcore::format(
                            .("Cumulative hazard for {group}"),
                            group = title2
                        ),
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

                # See .plot(): the option pair misses the outcomeorganizer
                # hand-off and this curve renders inverted.
                if (private$.isCompetingRisk(image7$state))
                    return(private$.competingRiskPlotRefusal(
                        "The log-log survival plot", ggtheme))

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

                myformula <- .asSurvivalFormula(private$.buildSurvFormula(mytime, myoutcome, myfactor))

                km_fit <- survival::survfit(myformula, data = plotData)

                # Get original display name for plot title
                labelled_data <- private$.getData()
                original_names_mapping <- labelled_data$original_names_mapping
                title2 <- .survivalDisplayName(myfactor, original_names_mapping)

                # Create log-log plot
                tryCatch({
                    plot7 <- plotData %>%
                        finalfit::surv_plot(
                            .data = .,
                            dependent = private$.buildSurvFormula(mytime, myoutcome, ns_prefix = FALSE),
                            explanatory = myfactor,
                            xlab = paste0('log(Time) (', self$options$timetypeoutput, ')'),
                            ylab = 'log(-log(Survival))',
                            title = jmvcore::format(
                                .("Log-log plot for {group}"),
                                group = title2
                            ),
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
                            title = jmvcore::format(
                                .("Log-log plot for {group}"),
                                group = title2
                            ),
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

                # See .plot(): the option pair misses the outcomeorganizer
                # hand-off and this curve renders inverted.
                if (private$.isCompetingRisk(image6$state))
                    return(private$.competingRiskPlotRefusal(
                        "The KMunicate-style plot", ggtheme))

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
                title2 <- .survivalDisplayName(myfactor, original_names_mapping)


                myformula <- .asSurvivalFormula(private$.buildSurvFormula(mytime, myoutcome, myfactor))

                km_fit <- survival::survfit(myformula, data = plotData)

                time_scale <- seq(0, self$options$endplot, by = self$options$byplot)


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

                # Residuals come from the Cox fit, which is refused under
                # competing risks -- see .plot() for why the option pair alone
                # let the hand-off through.
                if (private$.isCompetingRisk(image9$state))
                    return(private$.competingRiskPlotRefusal(
                        "Cox residual diagnostics", ggtheme))

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
                    warning(jmvcore::format(
                        .("Error creating residuals plot: {message}"),
                        message = conditionMessage(e)
                    ))
                    FALSE
                })
            }
            
            # cox.zph ----
            ,
            .plot8 = function(image8, ggtheme, theme, ...) {
                ph_cox <- self$options$ph_cox

                if (!ph_cox)
                    return()

                # Schoenfeld residuals come from the Cox fit, which is refused
                # under competing risks -- see .plot() for why the option pair
                # alone let the hand-off through.
                if (private$.isCompetingRisk(image8$state))
                    return(private$.competingRiskPlotRefusal(
                        "The proportional-hazards (Schoenfeld) plot", ggtheme))

                zph_state <- image8$state

                if (is.null(zph_state)) {
                    return()
                }

                tryCatch({
                    # Reconstruct Schoenfeld residual plots from serializable state
                    time_vals <- zph_state$time
                    y_df <- zph_state$y
                    var_names <- zph_state$var_names
                    n_vars <- zph_state$n_vars

                    if (is.null(n_vars) || n_vars == 0) return()

                    # Create multi-panel plot for each variable
                    plots <- list()
                    for (j in seq_len(n_vars)) {
                        var_name <- if (!is.null(var_names) && j <= length(var_names)) var_names[j] else paste("Var", j)
                        resid_vals <- y_df[[j]]
                        p_val <- if (!is.null(zph_state$table) && j <= nrow(zph_state$table)) {
                            zph_state$table[j, ncol(zph_state$table)]
                        } else NA

                        plot_df <- data.frame(time = time_vals, residual = resid_vals)
                        p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = time, y = residual)) +
                            ggplot2::geom_point(alpha = 0.5, size = 1) +
                            ggplot2::geom_smooth(method = "loess", se = TRUE, color = "red") +
                            ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
                            ggplot2::labs(
                                x = "Time",
                                y = "Scaled Schoenfeld Residuals",
                                title = paste0("PH Test: ", var_name),
                                subtitle = if (!is.na(p_val)) paste0("p = ", format.pval(p_val, digits = 3)) else NULL
                            ) +
                            ggtheme
                        plots[[j]] <- p
                    }

                    if (length(plots) == 1) {
                        print(plots[[1]])
                    } else {
                        # Use patchwork or gridExtra if available, otherwise just print first
                        if (requireNamespace("patchwork", quietly = TRUE)) {
                            combined <- Reduce("+", plots)
                            print(combined)
                        } else {
                            # Fallback: arrange with gridExtra
                            tryCatch({
                                do.call(gridExtra::grid.arrange, c(plots, ncol = min(2, length(plots))))
                            }, error = function(e2) {
                                print(plots[[1]])
                            })
                        }
                    }
                    TRUE
                }, error = function(e) {
                    warning(paste("Error creating PH test plot:", e$message))
                    FALSE
                })

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
                <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 149, 188, 0.1); border-left: 4px solid #17a2b8; color: inherit;">
                    <h4> Understanding Your Median Survival Results</h4>
                    <p><strong>What is Median Survival?</strong> The median survival time tells you when half of your study population experienced the event you are studying.</p>
                    
                    <h5> How to Read Your Results:</h5>
                    <table style="width:100%; margin: 10px 0;">
                        <tr style="background-color: rgba(138, 155, 172, 0.06); color: inherit;">
                            <td style="padding: 5px;"><strong>Records</strong></td>
                            <td style="padding: 5px;">Total number of patients in your study group</td>
                        </tr>
                        <tr>
                            <td style="padding: 5px;"><strong>Events</strong></td>
                            <td style="padding: 5px;">Number of patients who experienced the outcome (e.g., death, recurrence)</td>
                        </tr>
                        <tr style="background-color: rgba(138, 155, 172, 0.06); color: inherit;">
                            <td style="padding: 5px;"><strong>Median</strong></td>
                            <td style="padding: 5px;">The time when 50% of patients had the event</td>
                        </tr>
                        <tr>
                            <td style="padding: 5px;"><strong>95% CI</strong></td>
                            <td style="padding: 5px;">Range of median values compatible with the data (over repeated studies, 95% of such intervals contain the true median)</td>
                        </tr>
                        <tr style="background-color: rgba(138, 155, 172, 0.06); color: inherit;">
                            <td style="padding: 5px;"><strong>Not Reached (NR)</strong></td>
                            <td style="padding: 5px;">Fewer than half the patients had the event during the observed follow-up. This may mean long survival, but short follow-up or heavy censoring gives the same result.</td>
                        </tr>
                    </table>
                    
                    <h5> Practical Example:</h5>
                    <p>If median survival = 24 months with 95% CI (18-30):</p>
                    <ul>
                        <li>Half your patients survived longer than 24 months</li>
                        <li>Medians from 18 to 30 months are compatible with these data; over repeated studies, 95% of such intervals would contain the true median</li>
                        <li>This helps predict typical patient outcomes for treatment planning</li>
                    </ul>
                    
                    <h5> Important Notes:</h5>
                    <ul>
                        <li>Median is better than mean for survival data as it is not affected by extreme values</li>
                        <li>Wide confidence intervals suggest more uncertainty (often due to small sample size)</li>
                        <li>"Not Reached" is often a positive finding indicating good survival</li>
                    </ul>
                </div>
                ')
                
                # Cox Regression Explanation
                private$.setExplanationContent("coxRegressionExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; color: inherit;">
                    <h4> Understanding Your Cox Regression Results</h4>
                    <p><strong>What is Cox Regression?</strong> This analysis compares the risk of experiencing an event between different groups, adjusting for time.</p>
                    
                    <h5> Key Metric: Hazard Ratio (HR)</h5>
                    <p>The hazard ratio tells you how much more (or less) likely one group is to experience the event compared to another.</p>
                    
                    <table style="width:100%; margin: 10px 0; border-collapse: collapse;">
                        <tr style="background-color: rgba(138, 155, 172, 0.06); color: inherit;">
                            <th style="padding: 8px; text-align: left;">HR Value</th>
                            <th style="padding: 8px; text-align: left;">Meaning</th>
                            <th style="padding: 8px; text-align: left;">Plain English</th>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;"><strong>HR = 1.0</strong></td>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;">No difference</td>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;">Event rate is the same in both groups at any given moment</td>
                        </tr>
                        <tr style="background-color: rgba(255, 33, 67, 0.09); color: inherit;">
                            <td style="padding: 8px;"><strong>HR = 2.0</strong></td>
                            <td style="padding: 8px;">Twice the hazard</td>
                            <td style="padding: 8px;">Event rate is 2\u00d7 as high at any given moment</td>
                        </tr>
                        <tr style="background-color: rgba(33, 159, 43, 0.1); color: inherit;">
                            <td style="padding: 8px;"><strong>HR = 0.5</strong></td>
                            <td style="padding: 8px;">Half the hazard</td>
                            <td style="padding: 8px;">Event rate is half as high at any given moment</td>
                        </tr>
                        <tr style="background-color: rgba(255, 33, 67, 0.09); color: inherit;">
                            <td style="padding: 8px;"><strong>HR = 3.0</strong></td>
                            <td style="padding: 8px;">Three times the hazard</td>
                            <td style="padding: 8px;">Event rate is 3\u00d7 as high at any given moment</td>
                        </tr>
                        <tr style="background-color: rgba(33, 159, 43, 0.1); color: inherit;">
                            <td style="padding: 8px;"><strong>HR = 0.25</strong></td>
                            <td style="padding: 8px;">Quarter the hazard</td>
                            <td style="padding: 8px;">Event rate is one quarter as high at any given moment</td>
                        </tr>
                    </table>
                    
                    <h5> Statistical Significance:</h5>
                    <ul>
                        <li><strong>P-value < 0.05:</strong> The data are unlikely under the hypothesis of no difference. This is not the probability that the finding is due to chance.</li>
                        <li><strong>95% CI includes 1.0:</strong> The difference is NOT statistically significant</li>
                        <li><strong>95% CI excludes 1.0:</strong> The difference IS statistically significant</li>
                    </ul>
                    
                    <h5> Clinical Example:</h5>
                    <p>If Treatment A vs Treatment B shows HR = 0.60 (95% CI: 0.40-0.85, p=0.004):</p>
                    <ul style="background-color: rgba(33, 159, 43, 0.1); padding: 10px; border-radius: 5px; color: inherit;">
                        <li>Treatment A has 0.60 times the hazard of Treatment B at any moment - a relative rate, not a 40% cut in cumulative risk</li>
                        <li>The result is statistically significant (p < 0.05 and CI excludes 1.0)</li>
                        <li>The interval 0.40 to 0.85 is the range of hazard ratios compatible with the data; it does not translate directly into a cumulative risk difference</li>
                    </ul>
                    
                    <h5> Important Considerations:</h5>
                    <ul>
                        <li>HR assumes proportional hazards (the hazard ratio stays constant over time); it is not a cumulative risk ratio</li>
                        <li>Wide confidence intervals suggest uncertainty (often from small sample size)</li>
                        <li>Statistical significance does not always mean clinical importance</li>
                    </ul>
                </div>
                ')
                
                # Survival Tables Explanation
                private$.setExplanationContent("survivalTablesExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 162, 64, 0.19); border-left: 4px solid #28a745; color: inherit;">
                    <h4> Understanding Your Survival Probability Tables</h4>
                    <p><strong>What are Survival Tables?</strong> These tables show the percentage of patients surviving at key time points, which are standard benchmarks in medical research.</p>
                    
                    <h5> How to Read the Table Columns:</h5>
                    <table style="width:100%; margin: 10px 0; border-collapse: collapse;">
                        <tr style="background-color: rgba(138, 155, 172, 0.06); color: inherit;">
                            <th style="padding: 8px; text-align: left;">Column</th>
                            <th style="padding: 8px; text-align: left;">What It Means</th>
                            <th style="padding: 8px; text-align: left;">Example</th>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;"><strong>Time</strong></td>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;">Time point of measurement</td>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;">12 months (1 year)</td>
                        </tr>
                        <tr style="background-color: rgba(138, 155, 172, 0.06); color: inherit;">
                            <td style="padding: 8px;"><strong>n.risk</strong></td>
                            <td style="padding: 8px;">Patients still being followed</td>
                            <td style="padding: 8px;">85 of 100 still in study</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px;"><strong>n.event</strong></td>
                            <td style="padding: 8px;">Cumulative events occurred</td>
                            <td style="padding: 8px;">15 events by this time</td>
                        </tr>
                        <tr style="background-color: rgba(138, 155, 172, 0.06); color: inherit;">
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
                    
                    <h5> Common Medical Benchmarks:</h5>
                    <div style="background-color: rgba(33, 152, 239, 0.13); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <ul style="margin: 5px 0;">
                            <li><strong>1-year survival:</strong> Short-term prognosis indicator</li>
                            <li><strong>3-year survival:</strong> Medium-term outcome measure</li>
                            <li><strong>5-year survival:</strong> Standard long-term benchmark (especially in cancer)</li>
                            <li><strong>10-year survival:</strong> Very long-term outcomes (chronic diseases)</li>
                        </ul>
                    </div>
                    
                    <h5> Practical Example:</h5>
                    <p>If your table shows:</p>
                    <table style="background-color: rgba(138, 155, 172, 0.06); padding: 5px; margin: 10px 0; color: inherit;">
                        <tr>
                            <td>5-year survival = 72% (95% CI: 65-79%)</td>
                        </tr>
                    </table>
                    <p>This means:</p>
                    <ul>
                        <li>an estimated 72% remained event-free at 5 years</li>
                        <li>Rates from 65% to 79% are compatible with these data; over repeated studies, 95% of such intervals would contain the true rate</li>
                        <li>This can be compared to published rates for similar populations</li>
                    </ul>
                    
                    <h5> Key Points to Remember:</h5>
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
                <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; color: inherit;">
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
                <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 41, 56, 0.13); border-left: 4px solid #6c757d; color: inherit;">
                    <h4>Understanding Restricted Mean Survival Time (RMST)</h4>
                    <p><strong>RMST:</strong> Average survival time up to a specified time horizon (\u03c4).</p>
                    <ul>
                        <li><strong>Interpretation:</strong> Mean survival time within the observation period</li>
                        <li><strong>Time Horizon (\u03c4):</strong> Maximum follow-up time considered</li>
                        <li><strong>Robust Measure:</strong> Less affected by tail behavior than median survival</li>
                        <li><strong>Difference in RMST:</strong> Directly interpretable as difference in mean survival time</li>
                    </ul>
                    <p><em>When to use:</em> Particularly useful when median survival cannot be estimated (too few events) or when comparing survival over a specific time period.</p>
                </div>
                ')
                
                # Residual Diagnostics Explanation
                private$.setExplanationContent("residualDiagnosticsExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(255, 202, 33, 0.4); border-left: 4px solid #fdcb6e; color: inherit;">
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
                <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 163, 188, 0.21); border-left: 4px solid #bee5eb; color: inherit;">
                    <h4> Understanding Your Survival Curves and Plots</h4>
                    <p><strong>What are Survival Curves?</strong> These graphs show how the probability of survival changes over time for different groups.</p>
                    
                    <h5> Types of Plots Available:</h5>
                    <table style="width:100%; margin: 10px 0; border-collapse: collapse;">
                        <tr style="background-color: rgba(138, 155, 172, 0.06); color: inherit;">
                            <th style="padding: 8px; text-align: left;">Plot Type</th>
                            <th style="padding: 8px; text-align: left;">What It Shows</th>
                            <th style="padding: 8px; text-align: left;">When to Use</th>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;"><strong>Survival Curve</strong></td>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;">% surviving over time</td>
                            <td style="padding: 8px; border-top: 1px solid #dee2e6;">Standard presentation</td>
                        </tr>
                        <tr style="background-color: rgba(138, 155, 172, 0.06); color: inherit;">
                            <td style="padding: 8px;"><strong>Cumulative Events</strong></td>
                            <td style="padding: 8px;">% experiencing event</td>
                            <td style="padding: 8px;">Focus on event occurrence</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px;"><strong>Cumulative Hazard</strong></td>
                            <td style="padding: 8px;">Accumulated risk</td>
                            <td style="padding: 8px;">Technical assessment</td>
                        </tr>
                        <tr style="background-color: rgba(138, 155, 172, 0.06); color: inherit;">
                            <td style="padding: 8px;"><strong>Log-Log Plot</strong></td>
                            <td style="padding: 8px;">Model assumptions</td>
                            <td style="padding: 8px;">Check proportional hazards</td>
                        </tr>
                    </table>
                    
                    <h5> How to Read Survival Curves:</h5>
                    <div style="background-color: rgba(255, 202, 33, 0.23); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <ul style="margin: 5px 0;">
                            <li><strong>Y-axis (0-1 or 0-100%):</strong> Probability of survival</li>
                            <li><strong>X-axis:</strong> Time since study start</li>
                            <li><strong>Steps down:</strong> Each step represents an event</li>
                            <li><strong>Tick marks (+):</strong> Censored patients (lost to follow-up)</li>
                            <li><strong>Shaded areas:</strong> 95% confidence intervals</li>
                        </ul>
                    </div>
                    
                    <h5> What to Look For:</h5>
                    <table style="width:100%; margin: 10px 0;">
                        <tr>
                            <td style="padding: 8px; background-color: rgba(33, 159, 43, 0.1); color: inherit;"><strong> Curves separate early</strong></td>
                            <td style="padding: 8px;">Visible separation; check the log-rank test for statistical evidence</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px; background-color: rgba(255, 33, 67, 0.09); color: inherit;"><strong> Curves overlap</strong></td>
                            <td style="padding: 8px;">No clear separation by eye; overlap is not evidence of no difference - check the log-rank test, especially with few events</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px; background-color: rgba(33, 152, 239, 0.13); color: inherit;"><strong> Curves cross</strong></td>
                            <td style="padding: 8px;">Effect changes over time</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px; background-color: rgba(255, 202, 33, 0.23); color: inherit;"><strong> Steep drop</strong></td>
                            <td style="padding: 8px;">High early event rate</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px; background-color: rgba(153, 33, 170, 0.12); color: inherit;"><strong>\u2192 Flat plateau</strong></td>
                            <td style="padding: 8px;">Stable period (few events)</td>
                        </tr>
                    </table>
                    
                    <h5> Risk Table (Below Plot):</h5>
                    <p>Shows how many patients are still being followed at each time point:</p>
                    <ul>
                        <li>Helps assess reliability of estimates</li>
                        <li>Fewer patients = wider confidence intervals</li>
                        <li>Important for interpreting late time points</li>
                    </ul>
                    
                    <h5> Clinical Interpretation Tips:</h5>
                    <ul>
                        <li><strong>Median survival:</strong> Where curve crosses 50% line</li>
                        <li><strong>1-year survival:</strong> Height of curve at 12 months</li>
                        <li><strong>Statistical significance:</strong> Read it from the log-rank test or the Cox p-value; overlapping confidence bands do not by themselves mean p &gt;= 0.05</li>
                        <li><strong>Clinical significance:</strong> Consider if differences are meaningful for patients</li>
                    </ul>
                </div>
                ')
                
                # Parametric Models Explanation
                private$.setExplanationContent("parametricModelsExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; color: inherit;">
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
                    <p><em>Advantages:</em> Provide explicit survival functions and permit direct comparison of distributional assumptions. Extrapolation is not provided by this analysis.</p>
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
                            } else if ("x0_95lcl" %in% col_names) {
                                results$medianData[[i, "x0_95lcl"]]
                            } else if ("x0_95lcl" %in% col_names) {
                                results$medianData[[i, "x0_95lcl"]]
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
                            } else if ("x0_95ucl" %in% col_names) {
                                results$medianData[[i, "x0_95ucl"]]
                            } else if ("x0_95ucl" %in% col_names) {
                                results$medianData[[i, "x0_95ucl"]]
                            } else if ("upper" %in% col_names) {
                                results$medianData[[i, "upper"]]
                            } else if ("conf.high" %in% col_names) {
                                results$medianData[[i, "conf.high"]]
                            } else {
                                NA
                            }
                        }, error = function(e) NA)
                        
                        # Report the estimate and its uncertainty; do not grade it.
                        #
                        # "Median not reached" was previously called a favorable
                        # prognostic finding. It is not, on its own: short
                        # follow-up and heavy censoring produce exactly the same
                        # result as genuinely good survival. Likewise, thresholds
                        # at 6 / 12 / 36 / 60 declared "urgent intervention",
                        # "chronic condition" or "excellent long-term prognosis"
                        # without knowing the disease, the endpoint, the treatment
                        # setting -- or even the time unit, since the sentence was
                        # hard-coded to "months" while the unit is user-selectable.
                        time_unit <- self$options$timetypeoutput

                        if (is.na(median_val) || median_val == "NR") {
                            clinical_meaning <- jmvcore::format(
                                .("In the {group} group the median was not reached: fewer than half of the patients had the event during the observed follow-up. This may reflect genuinely long survival, but short follow-up or heavy censoring produces the same result, so it should be read together with the number at risk over time."),
                                group = group_name
                            )
                        } else {
                            clinical_meaning <- jmvcore::format(
                                .("In the {group} group, half of the patients had the event by {median} {unit} (95% CI {lower} to {upper})."),
                                group = group_name,
                                median = median_val,
                                unit = time_unit,
                                lower = ci_lower,
                                upper = ci_upper
                            )
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
                            # Describe the hazard ratio; do not convert it into a
                            # cumulative "% risk reduction" and do not grade its
                            # clinical importance.
                            #
                            # A hazard ratio is a ratio of instantaneous hazards,
                            # averaged over follow-up under proportional hazards.
                            # "HR 0.62 = 38% risk reduction" reads as an absolute
                            # cumulative-risk statement, which it is not: the
                            # absolute benefit depends on the baseline risk, the
                            # endpoint, and the length of follow-up. Nor can a
                            # fixed HR cut-off decide clinical meaningfulness
                            # without knowing any of those. (Hernan, "The Hazards
                            # of Hazard Ratios", Epidemiology 2010.)
                            direction <- if (hr < 1)
                                .("a lower hazard") else if (hr > 1)
                                .("a higher hazard") else .("the same hazard")

                            statistical_context <- if (is.na(p_val)) {
                                .("p-value not available")
                            } else if (p_val < 0.001) {
                                .("p < 0.001")
                            } else {
                                jmvcore::format(.("p = {p}"), p = base::format(round(p_val, 3), nsmall = 3))
                            }

                            crosses_one <- !is.na(ci_lower) && !is.na(ci_upper) &&
                                           ci_lower < 1 && ci_upper > 1

                            clinical_meaning <- jmvcore::format(
                                .("{comparison}: hazard ratio {hr} (95% CI {lower} to {upper}, {stats}) - {direction} in the compared group relative to the reference. The hazard ratio is a relative rate, not a cumulative risk difference; the absolute benefit or harm depends on the baseline risk, the endpoint and the length of follow-up.{cinote}"),
                                comparison = comparison,
                                hr = round(hr, 2),
                                lower = round(ci_lower, 2),
                                upper = round(ci_upper, 2),
                                stats = statistical_context,
                                direction = direction,
                                cinote = if (crosses_one)
                                    paste0(" ", .("The confidence interval includes 1, so the data are compatible with no difference.")) else ""
                            )

                            interpretations[[paste("cox", gsub(" ", "_", comparison))]] <- clinical_meaning
                        }
                    }
                }
                
                return(interpretations)
            }
            
            ,
            .generateClinicalGlossary = function() {
                glossary_html <- '
                <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(138, 155, 172, 0.06); border-left: 4px solid #6c757d; color: inherit;">
                    <h4> Clinical Terminology Glossary</h4>
                    
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
                            <strong>Hazard Ratio (HR):</strong> Ratio of instantaneous event rates between two groups (HR=2 means the event rate is twice as high at any given moment). It is a relative rate, not a cumulative risk ratio - the absolute difference depends on baseline risk and follow-up length.
                        </div>
                        
                        <div style="background-color: white; padding: 10px; border-radius: 5px;">
                            <strong>95% Confidence Interval:</strong> Range of values compatible with the data; over repeated studies, 95% of such intervals contain the true value
                        </div>
                        
                        <div style="background-color: white; padding: 10px; border-radius: 5px;">
                            <strong>P-value:</strong> How compatible the data are with no difference - the probability of a result at least this extreme if there were truly no difference. It is not the probability that the finding occurred by chance, nor that the null hypothesis is true.
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
                            # "Not reached" is a statement about the observed
                            # follow-up, not a favourable prognosis -- short
                            # follow-up produces the same result.
                            sentence <- jmvcore::format(
                                .("Median survival was not reached for the {group} group: fewer than half of the patients had the event during the observed follow-up."),
                                group = group_name
                            )
                        } else {
                            # The unit is user-selectable; this sentence used to
                            # say "months" whatever the user had chosen.
                            sentence <- jmvcore::format(
                                .("Median survival for the {group} group was {median} {unit} (95% CI: {lower} to {upper} {unit})."),
                                group = group_name,
                                median = median_val,
                                unit = self$options$timetypeoutput,
                                lower = ci_lower,
                                upper = ci_upper
                            )
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
                            # "reduced/increased risk" reads as a cumulative-risk
                            # claim. A hazard ratio is a relative rate.
                            direction <- ifelse(hr < 1, .("a lower hazard"), .("a higher hazard"))

                            # Do not lead with a direction the interval does not
                            # support: an HR of 0.85 with a CI of 0.60 to 1.20 is
                            # compatible with both a lower and a higher hazard.
                            crosses_one <- !is.na(ci_lower) && !is.na(ci_upper) &&
                                           ci_lower < 1 && ci_upper > 1

                            sentence <- if (crosses_one) {
                                jmvcore::format(
                                    .("Cox regression estimated a hazard ratio of {hr} for {comparison} (95% CI: {lower} to {upper}, p = {p}); the interval includes 1, so these data are compatible with both a lower and a higher hazard."),
                                    comparison = comparison,
                                    hr = round(hr, 2),
                                    lower = round(ci_lower, 2),
                                    upper = round(ci_upper, 2),
                                    p = round(p_val, 3)
                                )
                            } else {
                                jmvcore::format(
                                    .("Cox regression estimated {direction} for {comparison}, with a hazard ratio of {hr} (95% CI: {lower} to {upper}, p = {p}), which was {significance}."),
                                    direction = direction,
                                    comparison = comparison,
                                    hr = round(hr, 2),
                                    lower = round(ci_lower, 2),
                                    upper = round(ci_upper, 2),
                                    p = round(p_val, 3),
                                    significance = significance
                                )
                            }
                            cox_sentences <- c(cox_sentences, sentence)
                        }
                    }
                    
                    sentences[["cox_results"]] <- cox_sentences
                }
                
                # Generate copy-ready sentences for survival probabilities
                if (!is.null(results$survTable) && nrow(results$survTable) > 0) {
                    surv_sentences <- c()
                    
                    # Report at the cutpoints the user actually asked for, in the
                    # unit they chose. This used to be hard-coded to 12/36/60 and
                    # then divided by 12 to print "1-year", "3-year", "5-year" --
                    # so a cohort measured in days was reported as 1/3/5 years
                    # when the cutpoints happened to be 12, 36 and 60 days.
                    common_times <- suppressWarnings(as.numeric(trimws(
                        unlist(strsplit(self$options$cutp, ",")))))
                    common_times <- common_times[!is.na(common_times)]
                    if (length(common_times) == 0) common_times <- c(12, 36, 60)
                    time_unit <- self$options$timetypeoutput

                    for (time_point in common_times) {
                        surv_at_time <- results$survTable[results$survTable$time == time_point, ]
                        
                        if (nrow(surv_at_time) > 0) {
                            for (j in seq_len(nrow(surv_at_time))) {
                                group <- surv_at_time[[j, 1]]
                                survival <- round(surv_at_time[[j, "surv"]] * 100, 1)
                                ci_lower <- round(surv_at_time[[j, "lower"]] * 100, 1)
                                ci_upper <- round(surv_at_time[[j, "upper"]] * 100, 1)
                                
                                sentence <- jmvcore::format(
                                    .("Survival at {time} {unit} for the {group} group was {survival}% (95% CI: {lower}% to {upper}%)."),
                                    time = time_point,
                                    unit = time_unit,
                                    group = group,
                                    survival = survival,
                                    lower = ci_lower,
                                    upper = ci_upper
                                )
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
                    if (!is.null(self$results$medianTable) && 
                        self$results$medianTable$rowCount > 0) {
                        df <- self$results$medianTable$asDF
                        if (nrow(df) > 0 && ncol(df) > 0) {
                            results$medianData <- df
                        }
                    }
                }, error = function(e) {
                    # Log warning but continue
                    warning(jmvcore::format(
                        .("Could not access median survival data: {message}"),
                        message = conditionMessage(e)
                    ))
                })
                
                tryCatch({
                    if (!is.null(self$results$coxTable) && 
                        self$results$coxTable$rowCount > 0) {
                        df <- self$results$coxTable$asDF
                        if (nrow(df) > 0 && ncol(df) > 0) {
                            results$coxData <- df
                        }
                    }
                }, error = function(e) {
                    # Log warning but continue
                    warning(jmvcore::format(
                        .("Could not access Cox regression data: {message}"),
                        message = conditionMessage(e)
                    ))
                })
                
                tryCatch({
                    if (!is.null(self$results$survTable) && 
                        self$results$survTable$rowCount > 0) {
                        df <- self$results$survTable$asDF
                        if (nrow(df) > 0 && ncol(df) > 0) {
                            results$survTable <- df
                        }
                    }
                }, error = function(e) {
                    # Log warning but continue
                    warning(jmvcore::format(
                        .("Could not access survival probability data: {message}"),
                        message = conditionMessage(e)
                    ))
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
                        '<div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 149, 236, 0.1); border-left: 4px solid #007bff; color: inherit;">',
                        '<h4> Clinical Interpretation</h4>',
                        paste(lapply(clinical_interpretations, function(x) paste('<p>', htmltools::htmlEscape(x), '</p>')), collapse = ""),
                        '</div>'
                    )
                    
                    if (self$options$showSummaries && !is.null(self$results[["clinicalInterpretationExplanation"]])) {
                        self$results$clinicalInterpretationExplanation$setContent(interpretation_html)
                    }
                }
                
                # Add Copy-Ready Clinical Report Sentences
                if (self$options$showSummaries && length(copy_sentences) > 0) {
                    copy_html <- '<div style="margin-bottom: 20px; padding: 15px; background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; color: inherit;"><h4> Copy-Ready Clinical Report Sentences</h4>'
                    
                    for (section_name in names(copy_sentences)) {
                        copy_html <- paste0(copy_html, '<h5>', tools::toTitleCase(gsub("_", " ", section_name)), ':</h5><div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">')
                        
                        for (sentence in copy_sentences[[section_name]]) {
                            copy_html <- paste0(copy_html, '<p style="margin: 5px 0; padding: 5px; background-color: rgba(138, 155, 172, 0.06); border-radius: 3px; color: inherit;">', htmltools::htmlEscape(sentence), '</p>')
                        }
                        
                        copy_html <- paste0(copy_html, '</div>')
                    }
                    
                    copy_html <- paste0(copy_html, '</div>')
                    
                    if (self$options$showSummaries && !is.null(self$results[["copyReadySentencesExplanation"]])) {
                        self$results$copyReadySentencesExplanation$setContent(copy_html)
                    }
                }
            }

            # ================================================================
            # Calibration Curves for Survival Models (Gap 5)
            # ================================================================
            ,
            .calculateCalibration = function(results) {
                tryCatch({
                    mytime <- results$name1time
                    myoutcome <- results$name2outcome
                    myfactor <- results$name3explanatory
                    mydata <- results$cleanData

                    # Optionally include RCS variable for richer calibration
                    rcs_var <- self$options$rcs_variable
                    extra_term <- NULL
                    if (!is.null(rcs_var) && rcs_var %in% names(self$data)) {
                        rcs_col <- jmvcore::toNumeric(self$data[[rcs_var]])
                        mydata[[rcs_var]] <- rcs_col[private$.originalRowIndex(mydata)]
                        mydata <- mydata[!is.na(mydata[[rcs_var]]), , drop = FALSE]
                        extra_term <- jmvcore::composeTerm(rcs_var)
                    }

                    # Need Cox model - fit it.
                    #
                    # Escape each term separately and join afterwards. Escaping
                    # the assembled string instead back-quoted the whole thing as
                    # a single name -- `grp + age` -- which is not a column, so
                    # the fit died with "object 'grp + age' not found" whenever a
                    # second covariate was added.
                    rhs_terms <- c(myfactor, extra_term)
                    esc_rhs <- paste(.escapeVariableNames(rhs_terms), collapse = " + ")
                    formula <- .asSurvivalFormula(paste(private$.buildSurvFormula(mytime, myoutcome), "~", esc_rhs))
                    # Point the formula at this frame and keep the model frame on
                    # the fit. Without it, survival's methods try to re-evaluate
                    # `data = mydata` in the formula's environment, which does not
                    # hold it -- the whole output failed with
                    # "Calibration error: object 'mydata' not found".
                    environment(formula) <- environment()
                    cox_model <- survival::coxph(formula, data = mydata,
                                                 x = TRUE, y = TRUE, model = TRUE)

                    # Determine calibration time point
                    time_col <- mydata[[mytime]]
                    event_col <- mydata[[myoutcome]]
                    cal_time <- self$options$calibration_timepoint
                    if (is.null(cal_time) || cal_time <= 0) {
                        cal_time <- median(time_col, na.rm = TRUE)
                    }

                    n_groups <- self$options$calibration_ngroups

                    # Predicted survival at calibration time point
                    surv_fit <- survival::survfit(cox_model)
                    # Get linear predictor for risk scoring
                    lp <- predict(cox_model, type = "lp")

                    # Baseline cumulative hazard at the MEAN covariate (centered = TRUE)
                    # to match predict(type = "lp"), which returns the mean-centered linear
                    # predictor (x - xbar) * beta. Combining a centered lp with an
                    # uncentered baseline (centered = FALSE) biases predicted survival by a
                    # factor exp(-xbar * beta) and distorts the calibration slope/intercept.
                    basehaz_df <- survival::basehaz(cox_model, centered = TRUE)
                    if (nrow(basehaz_df) == 0 || max(basehaz_df$time) < cal_time) {
                        # cal_time beyond observed data
                        self$results$calibrationTable$setNote("error",
                            paste0("Calibration time point (", round(cal_time, 1),
                                   ") exceeds maximum observed time. Choose a smaller value."))
                        return()
                    }
                    # Find baseline cumulative hazard at calibration time
                    bh_at_time <- basehaz_df$hazard[which.min(abs(basehaz_df$time - cal_time))]
                    # Predicted survival for each patient using true baseline hazard
                    pred_surv <- exp(-bh_at_time * exp(lp))

                    # Create risk groups by quantiles of predicted survival
                    n_unique_pred <- length(unique(round(pred_surv, 8)))
                    # Reduce groups if too few unique predicted values
                    effective_groups <- min(n_groups, n_unique_pred)
                    if (effective_groups < 2) {
                        self$results$calibrationTable$setNote("error",
                            "Cannot create risk groups - too little variation in predicted survival. Add continuous covariates for meaningful calibration.")
                        return()
                    }
                    breaks <- quantile(pred_surv, probs = seq(0, 1, length.out = effective_groups + 1),
                                       na.rm = TRUE)
                    # Handle duplicated breaks
                    breaks <- unique(breaks)
                    if (length(breaks) < 3) {
                        self$results$calibrationTable$setNote("error",
                            "Cannot create risk groups - too little variation in predicted survival. Add continuous covariates for meaningful calibration.")
                        return()
                    }
                    breaks[1] <- breaks[1] - 0.001
                    breaks[length(breaks)] <- breaks[length(breaks)] + 0.001
                    risk_group <- cut(pred_surv, breaks = breaks, include.lowest = TRUE,
                                      labels = paste0("Q", seq_len(length(breaks) - 1)))

                    # Observed survival per group via KM
                    group_table <- self$results$calibrationGroupTable
                    group_pred <- c()
                    group_obs <- c()

                    surv_obj <- survival::Surv(time_col, event_col)

                    for (g in levels(risk_group)) {
                        idx <- which(risk_group == g)
                        if (length(idx) < 5) next

                        n_g <- length(idx)
                        events_g <- sum(event_col[idx], na.rm = TRUE)
                        mean_pred <- mean(pred_surv[idx], na.rm = TRUE)

                        # KM estimate at cal_time
                        km_g <- survival::survfit(surv_obj[idx] ~ 1)
                        km_summary <- summary(km_g, times = cal_time)

                        obs_surv <- if (length(km_summary$surv) > 0) km_summary$surv else NA
                        obs_lower <- if (length(km_summary$lower) > 0) km_summary$lower else NA
                        obs_upper <- if (length(km_summary$upper) > 0) km_summary$upper else NA

                        group_table$addRow(rowKey = g, values = list(
                            group = g,
                            n = as.integer(n_g),
                            events = as.integer(events_g),
                            predicted = mean_pred,
                            observed = obs_surv,
                            observed_lower = obs_lower,
                            observed_upper = obs_upper
                        ))

                        if (!is.na(obs_surv)) {
                            group_pred <- c(group_pred, mean_pred)
                            group_obs <- c(group_obs, obs_surv)
                        }
                    }

                    # Calibration metrics.
                    #
                    # These used to be an ordinary least-squares regression of
                    # grouped observed survival on grouped predicted survival,
                    # with its slope labelled "calibration slope" and its
                    # intercept "calibration-in-the-large". Neither is the
                    # survival estimand of that name: the OLS fit ignores
                    # censoring, uses only the handful of group means, and its
                    # intercept is not a measure of mean calibration. R-squared
                    # and mean-absolute-error thresholds were arbitrary on top of
                    # that.
                    #
                    # Replaced with the standard pair:
                    #   * calibration slope  - the coefficient of the model's own
                    #     linear predictor when refitted as the sole covariate
                    #     (Van Houwelingen). 1.0 means the predicted spread of
                    #     risk matches the observed spread; below 1 indicates
                    #     over-fitted, too-extreme predictions.
                    #   * mean calibration   - observed Kaplan-Meier survival at
                    #     the calibration time minus mean predicted survival.
                    #     Kaplan-Meier handles censoring, which the OLS did not.
                    cal_table <- self$results$calibrationTable

                    # --- calibration slope --------------------------------------
                    slope_fit <- tryCatch(
                        survival::coxph(survival::Surv(time_col, event_col) ~ lp),
                        error = function(e) NULL)

                    if (!is.null(slope_fit)) {
                        cal_slope <- unname(stats::coef(slope_fit)[1])
                        slope_ci  <- tryCatch(stats::confint(slope_fit),
                                              error = function(e) matrix(NA_real_, 1, 2))
                        cal_table$addRow(rowKey = "slope", values = list(
                            metric = "Calibration slope",
                            value = cal_slope,
                            ci_lower = slope_ci[1, 1],
                            ci_upper = slope_ci[1, 2],
                            ideal = "1.0",
                            interpretation = if (is.na(cal_slope)) "Not estimable"
                                else "Apparent slope - 1 by construction on the development data"
                        ))

                        # Refitting a model's own linear predictor on the data it
                        # was fitted to returns a slope of exactly 1 every time.
                        # The number only carries information when the model is
                        # applied to data it has not seen, or after bootstrap /
                        # cross-validation. Saying so prevents it being read as
                        # evidence of good calibration.
                        cal_table$setNote("slope_apparent",
                            paste0("The calibration slope is 1 by construction here, because the ",
                                   "model is evaluated on the same data it was fitted to. It becomes ",
                                   "informative only on external data or after internal validation ",
                                   "(bootstrap or cross-validation). The same optimism affects the ",
                                   "other rows in this table."))
                    }

                    # --- mean calibration at the calibration time ----------------
                    km_overall <- tryCatch(
                        summary(survival::survfit(survival::Surv(time_col, event_col) ~ 1),
                                times = cal_time, extend = TRUE),
                        error = function(e) NULL)

                    if (!is.null(km_overall) && length(km_overall$surv) == 1) {
                        obs_overall  <- km_overall$surv
                        pred_overall <- mean(pred_surv, na.rm = TRUE)
                        cal_table$addRow(rowKey = "meancal", values = list(
                            metric = paste0("Mean calibration (observed - predicted at t = ",
                                            round(cal_time, 1), ")"),
                            value = obs_overall - pred_overall,
                            ci_lower = km_overall$lower - pred_overall,
                            ci_upper = km_overall$upper - pred_overall,
                            ideal = "0.0",
                            interpretation = sprintf("Observed %.3f vs predicted %.3f",
                                                     obs_overall, pred_overall)
                        ))
                    }

                    # --- group-level agreement, reported without a grade ---------
                    if (length(group_pred) >= 3) {
                        mae <- mean(abs(group_obs - group_pred))
                        cal_table$addRow(rowKey = "mae", values = list(
                            metric = "Mean absolute difference across risk groups",
                            value = mae,
                            ci_lower = NA,
                            ci_upper = NA,
                            ideal = "0.0",
                            interpretation = sprintf("Averaged over %d risk groups; descriptive only",
                                                     length(group_pred))
                        ))
                    }

                    if (TRUE) {
                        # C-index (discrimination)
                        c_index <- tryCatch({
                            survival::concordance(cox_model)$concordance
                        }, error = function(e) NA)

                        if (!is.na(c_index)) {
                            c_se <- tryCatch({
                                sqrt(survival::concordance(cox_model)$var)
                            }, error = function(e) NA)

                            cal_table$addRow(rowKey = "cindex", values = list(
                                metric = "C-index (Discrimination)",
                                value = c_index,
                                # A concordance index lives in [0, 1]; the Wald
                                # limits were previously printed unconstrained.
                                ci_lower = if (!is.na(c_se)) max(0, c_index - 1.96 * c_se) else NA,
                                ci_upper = if (!is.na(c_se)) min(1, c_index + 1.96 * c_se) else NA,
                                ideal = "1.0",
                                interpretation = if (c_index >= 0.8) "Excellent" else if (c_index >= 0.7) "Good"
                                                 else if (c_index >= 0.6) "Moderate" else "Poor"
                            ))
                        }
                    }

                    cal_table$setNote("time",
                        paste0("Calibration assessed at t = ", round(cal_time, 1),
                               ". N = ", sum(!is.na(pred_surv)), " patients in ", length(group_pred), " risk groups."))

                    # Store data for plot
                    self$results$calibrationPlot$setState(list(
                        group_pred = group_pred,
                        group_obs = group_obs,
                        cal_time = cal_time
                    ))

                    # Calibration interpretation
                    if (self$options$showExplanations && length(group_pred) >= 3) {
                        html <- paste0(
                            "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.5;'>",
                            "<h3>Calibration Assessment</h3>",
                            "<p>Calibration measures how well the model's predicted survival probabilities ",
                            "match observed outcomes. Points near the 45-degree line indicate good calibration.</p>",
                            "<ul>",
                            "<li><strong>Calibration slope</strong>: Values near 1.0 indicate well-calibrated predictions. ",
                            "Slope < 1 suggests overfitting (predictions too extreme); slope > 1 suggests underfitting.</li>",
                            "<li><strong>Calibration-in-the-large</strong>: Intercept near 0 means no systematic over/under-prediction.</li>",
                            "<li><strong>C-index</strong>: Discrimination ability (0.5 = random, 1.0 = perfect). ",
                            "Note: good discrimination does not guarantee good calibration.</li>",
                            "</ul>",
                            "<div style='background-color: rgba(33, 159, 43, 0.1); padding: 12px; border-left: 4px solid #4CAF50; margin-top: 15px; color: inherit;'>",
                            "<strong>Clinical note:</strong> For survival models used to guide treatment decisions, ",
                            "calibration is as important as discrimination. A well-discriminating model that is ",
                            "poorly calibrated may systematically over- or under-estimate risk, leading to inappropriate ",
                            "treatment allocation.</div></div>")
                        self$results$calibrationInterpretation$setContent(html)
                    }

                }, error = function(e) {
                    self$results$calibrationTable$setNote("error",
                        paste("Calibration error:", htmltools::htmlEscape(e$message)))
                })
            }

            ,
            .plotCalibration = function(image, ggtheme, theme, ...) {
                state <- image$state
                if (is.null(state) || length(state$group_pred) < 3) return(FALSE)

                plot_df <- data.frame(
                    predicted = state$group_pred,
                    observed = state$group_obs
                )

                p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = predicted, y = observed)) +
                    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                                         color = "gray50", linewidth = 0.8) +
                    ggplot2::geom_point(size = 4, color = "#2196F3") +
                    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#F44336",
                                         fill = "#FFCDD2", linewidth = 0.8) +
                    ggplot2::labs(
                        title = paste0("Calibration Plot (t = ", round(state$cal_time, 1), ")"),
                        x = "Predicted Survival Probability",
                        y = "Observed Survival Probability (KM)"
                    ) +
                    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
                    ggtheme

                print(p)
                TRUE
            }

            # ================================================================
            # Restricted Cubic Splines - Non-Linearity Assessment (Gap 8)
            # ================================================================
            ,
            .calculateRCS = function(results) {
                tryCatch({
                    mytime <- results$name1time
                    myoutcome <- results$name2outcome
                    myfactor <- results$name3explanatory
                    mydata <- results$cleanData

                    rcs_var <- self$options$rcs_variable
                    n_knots <- self$options$rcs_knots

                    # The rcs_variable is not in cleanData (which only has time/outcome/explanatory)
                    # Pull it from the original data and add it to mydata
                    if (is.null(rcs_var) || !(rcs_var %in% names(self$data))) {
                        self$results$rcsTestTable$setNote("error",
                            "Selected variable not found in the data.")
                        return()
                    }

                    rcs_col <- jmvcore::toNumeric(self$data[[rcs_var]])
                    # Align with cleanData rows (cleanData has row_names or is naOmit'd)
                    # Add the variable to mydata using original row indices
                    mydata[[rcs_var]] <- rcs_col[private$.originalRowIndex(mydata)]
                    # Remove rows where RCS variable is NA
                    mydata <- mydata[!is.na(mydata[[rcs_var]]), , drop = FALSE]

                    rcs_values <- mydata[[rcs_var]]
                    if (all(is.na(rcs_values))) {
                        self$results$rcsTestTable$setNote("error",
                            "Selected variable contains no numeric values.")
                        return()
                    }

                    n_unique <- length(unique(na.omit(rcs_values)))
                    if (n_unique < n_knots + 2) {
                        self$results$rcsTestTable$setNote("error",
                            paste0("Too few unique values (", n_unique,
                                   ") for ", n_knots, " knots. Need at least ", n_knots + 2, "."))
                        return()
                    }

                    # Build formulas
                    surv_part <- private$.buildSurvFormula(mytime, myoutcome)

                    # Linear model
                    rcs_var_safe <- jmvcore::composeTerm(rcs_var)
                    if (!is.null(myfactor) && nchar(myfactor) > 0) {
                        formula_linear <- .asSurvivalFormula(paste0(surv_part, ' ~ ', rcs_var_safe, ' + ', myfactor))
                        formula_spline <- .asSurvivalFormula(paste0(surv_part, ' ~ splines::ns(', rcs_var_safe,
                                                            ', df = ', n_knots - 1, ') + ', myfactor))
                    } else {
                        formula_linear <- .asSurvivalFormula(paste0(surv_part, ' ~ ', rcs_var_safe))
                        formula_spline <- .asSurvivalFormula(paste0(surv_part, ' ~ splines::ns(', rcs_var_safe,
                                                            ', df = ', n_knots - 1, ')'))
                    }

                    # Fit both models
                    cox_linear <- survival::coxph(formula_linear, data = mydata)
                    cox_spline <- survival::coxph(formula_spline, data = mydata)

                    # Likelihood ratio test for non-linearity
                    lr_test <- anova(cox_linear, cox_spline)

                    # Extract test results
                    loglik_linear <- cox_linear$loglik[2]
                    loglik_spline <- cox_spline$loglik[2]
                    df_linear <- length(coef(cox_linear))
                    df_spline <- length(coef(cox_spline))
                    lr_chisq <- 2 * (loglik_spline - loglik_linear)
                    lr_df <- df_spline - df_linear
                    lr_p <- pchisq(lr_chisq, df = lr_df, lower.tail = FALSE)

                    # Populate test table
                    table <- self$results$rcsTestTable

                    table$addRow(rowKey = "linear", values = list(
                        model = paste0("Linear (", rcs_var, ")"),
                        df = as.integer(df_linear),
                        loglik = loglik_linear,
                        aic = AIC(cox_linear),
                        lr_chisq = NA,
                        lr_df = NA,
                        p_value = NA,
                        conclusion = ""
                    ))

                    conclusion <- if (lr_p < 0.05) {
                        paste0("Significant non-linearity (p = ", format.pval(lr_p, digits = 3),
                               "). Use spline model.")
                    } else {
                        paste0("No significant non-linearity (p = ", format.pval(lr_p, digits = 3),
                               "). Linear term adequate.")
                    }

                    table$addRow(rowKey = "spline", values = list(
                        model = paste0("Spline (", rcs_var, ", ", n_knots, " knots)"),
                        df = as.integer(df_spline),
                        loglik = loglik_spline,
                        aic = AIC(cox_spline),
                        lr_chisq = lr_chisq,
                        lr_df = as.integer(lr_df),
                        p_value = lr_p,
                        conclusion = conclusion
                    ))

                    table$setNote("info",
                        paste0("Null hypothesis: linear effect of ", rcs_var,
                               ". Natural splines with ", n_knots - 1, " degrees of freedom."))

                    # Prepare HR curve data for plot
                    var_range <- range(rcs_values, na.rm = TRUE)
                    var_seq <- seq(var_range[1], var_range[2], length.out = 100)
                    var_median <- median(rcs_values, na.rm = TRUE)

                    # Build prediction data
                    # For plotting, we need to extract the spline HR relative to median
                    spline_basis <- splines::ns(var_seq, df = n_knots - 1,
                                                knots = attr(splines::ns(rcs_values, df = n_knots - 1), "knots"),
                                                Boundary.knots = attr(splines::ns(rcs_values, df = n_knots - 1), "Boundary.knots"))
                    ref_basis <- splines::ns(var_median, df = n_knots - 1,
                                             knots = attr(splines::ns(rcs_values, df = n_knots - 1), "knots"),
                                             Boundary.knots = attr(splines::ns(rcs_values, df = n_knots - 1), "Boundary.knots"))

                    # Get spline coefficients from Cox model
                    spline_coef_names <- grep("splines::ns", names(coef(cox_spline)), value = TRUE)
                    spline_coefs <- coef(cox_spline)[spline_coef_names]

                    if (length(spline_coefs) > 0 && ncol(spline_basis) == length(spline_coefs)) {
                        lp_curve <- as.numeric(spline_basis %*% spline_coefs)
                        lp_ref <- as.numeric(ref_basis %*% spline_coefs)
                        hr_curve <- exp(lp_curve - lp_ref)

                        # Approximate CIs using variance-covariance matrix
                        vcov_spline <- vcov(cox_spline)
                        spline_vcov <- vcov_spline[spline_coef_names, spline_coef_names]
                        se_curve <- sqrt(rowSums((spline_basis - matrix(ref_basis, nrow = nrow(spline_basis),
                                                                        ncol = ncol(ref_basis), byrow = TRUE)) %*%
                                                  spline_vcov *
                                                  (spline_basis - matrix(ref_basis, nrow = nrow(spline_basis),
                                                                         ncol = ncol(ref_basis), byrow = TRUE))))
                        hr_lower <- exp(lp_curve - lp_ref - 1.96 * se_curve)
                        hr_upper <- exp(lp_curve - lp_ref + 1.96 * se_curve)

                        self$results$rcsPlot$setState(list(
                            var_seq = var_seq,
                            hr_curve = hr_curve,
                            hr_lower = hr_lower,
                            hr_upper = hr_upper,
                            var_name = rcs_var,
                            var_median = var_median,
                            n_knots = n_knots,
                            p_value = lr_p
                        ))
                    }

                    # Interpretation
                    if (self$options$showExplanations) {
                        html <- paste0(
                            "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.5;'>",
                            "<h3>Non-Linearity Assessment (Restricted Cubic Splines)</h3>",
                            "<p>This analysis tests whether the effect of <strong>", rcs_var,
                            "</strong> on survival follows a linear pattern or a more complex non-linear curve.</p>",
                            "<h4>Likelihood Ratio Test</h4>",
                            "<p>The LR test compares a Cox model with a linear term for ", rcs_var,
                            " against a model with natural splines (", n_knots, " knots, ", n_knots - 1,
                            " df). A significant p-value (< 0.05) indicates non-linearity.</p>",
                            "<h4>HR Curve Plot</h4>",
                            "<p>The hazard ratio curve shows how the relative risk changes across values of ", rcs_var,
                            ", with the reference point at the median value (", round(var_median, 1),
                            "). A flat line would indicate a constant HR (linear on log-HR scale).</p>",
                            "<div style='background-color: rgba(255, 169, 33, 0.14); padding: 12px; border-left: 4px solid #FF9800; margin-top: 15px; color: inherit;'>",
                            "<strong>Clinical note:</strong> Non-linear effects are common for continuous variables ",
                            "like age (J-shaped mortality), tumor size (threshold effects), and biomarker levels. ",
                            "If non-linearity is significant, reporting only a single HR per unit increase is misleading. ",
                            "Instead, report the HR curve or use meaningful cut-points.</div></div>")
                        self$results$rcsInterpretation$setContent(html)
                    }

                }, error = function(e) {
                    self$results$rcsTestTable$setNote("error",
                        paste("RCS analysis error:", htmltools::htmlEscape(e$message)))
                })
            }

            ,
            .plotRCS = function(image, ggtheme, theme, ...) {
                state <- image$state
                if (is.null(state) || is.null(state$hr_curve)) return(FALSE)

                plot_df <- data.frame(
                    x = state$var_seq,
                    hr = state$hr_curve,
                    lower = state$hr_lower,
                    upper = state$hr_upper
                )

                p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = hr)) +
                    ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                                         color = "gray50", linewidth = 0.5) +
                    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                                         fill = "#BBDEFB", alpha = 0.5) +
                    ggplot2::geom_line(color = "#1565C0", linewidth = 1.2) +
                    ggplot2::geom_vline(xintercept = state$var_median, linetype = "dotted",
                                         color = "#F44336", linewidth = 0.5) +
                    ggplot2::labs(
                        title = paste0("Hazard Ratio Curve: ", state$var_name,
                                       " (", state$n_knots, " knots)"),
                        subtitle = paste0("Reference: median = ", round(state$var_median, 1),
                                          " | Non-linearity p = ",
                                          format.pval(state$p_value, digits = 3)),
                        x = state$var_name,
                        y = "Hazard Ratio"
                    ) +
                    ggplot2::scale_y_continuous(trans = "log2",
                        breaks = c(0.25, 0.5, 1, 2, 4, 8)) +
                    ggtheme

                print(p)
                TRUE
            }

            # ================================================================
            # Parametric Plot Stubs (features disabled for this release)
            # These prevent crashes if use_parametric is accidentally enabled
            # ================================================================
            ,
            # How to read a covariate coefficient, per distribution.
            #
            # This used to state, for every distribution, that coefficients were
            # on the log-time (AFT) scale and that a negative value meant shorter
            # survival. That is wrong for the rate-parameterised families: in
            # flexsurv, exp, gompertz and gamma place covariates on the log-RATE
            # parameter, where the sign is inverted -- a positive coefficient
            # means shorter survival. For exponential and Gompertz that
            # parameterisation is proportional hazards, so exp(coef) is a hazard
            # ratio rather than a time ratio.
            #
            # The scale is read from flexsurv itself rather than hard-coded, so a
            # new distribution added to the option list cannot silently inherit
            # the wrong wording.
            .covariateScaleNote = function(dist) {

                if (identical(dist, "survspline")) {
                    return(switch(self$options$spline_scale,
                        hazard = paste0("Covariate coefficients are log hazard ratios ",
                            "(proportional hazards); a positive value indicates a higher ",
                            "hazard, i.e. shorter survival."),
                        # All three spline links are increasing in the covariate
                        # effect, and increasing each one lowers S(t). Verified by
                        # inverting the links at S = 0.6: hazard 0.536, odds 0.551,
                        # normal 0.521 -- every one shorter. The odds and normal
                        # cases previously said "longer survival", which is
                        # backwards: for scale="odds" the model is on the log odds
                        # of FAILURE, not of surviving.
                        odds = paste0("Covariate coefficients are log odds ratios for ",
                            "failure (proportional odds); a <b>positive</b> value indicates ",
                            "higher odds of the event, i.e. shorter survival."),
                        normal = paste0("Covariate coefficients are on the probit scale, ",
                            "where the link is -qnorm(S(t)); a <b>positive</b> value lowers ",
                            "the survival probability, i.e. shorter survival."),
                        paste0("The covariate coefficient scale depends on the spline scale ",
                            "selected.")))
                }

                loc <- tryCatch(flexsurv::flexsurv.dists[[dist]]$location,
                                error = function(e) NULL)

                if (identical(loc, "rate")) {
                    ph <- dist %in% c("exp", "gompertz")
                    paste0("Covariate coefficients are on the log-rate scale; a ",
                           "<b>positive</b> value indicates shorter survival",
                           if (ph) paste0(". This is a proportional-hazards parameterisation, ",
                                          "so exp(coefficient) is a hazard ratio.") else ".")
                } else if (!is.null(loc)) {
                    paste0("Covariate coefficients are on the log-time (accelerated failure ",
                           "time) scale; a <b>negative</b> value indicates shorter survival, ",
                           "and exp(coefficient) is a time ratio.")
                } else {
                    paste0("The covariate coefficient scale depends on the selected ",
                           "distribution; see the flexsurv documentation.")
                }
            }
            ,
            .distLabel = function(dist) {
                labs <- c(exp = "Exponential", weibull = "Weibull", lnorm = "Log-Normal",
                          llogis = "Log-Logistic", gamma = "Gamma", gengamma = "Generalized Gamma",
                          gompertz = "Gompertz", survspline = "Royston-Parmar spline")
                if (!is.null(dist) && dist %in% names(labs)) unname(labs[dist]) else dist
            }
            ,
            # Parametric survival models (flexsurv): distribution fit, AIC/BIC
            # comparison, model summary, and a fitted-vs-Kaplan-Meier plot.
            .parametricSurvival = function(results) {
                if (!isTRUE(self$options$use_parametric)) return()
                if (!requireNamespace("flexsurv", quietly = TRUE)) {
                    self$results$parametricModelSummary$setNote("flexsurv",
                        "The flexsurv package is required for parametric survival models.")
                    return()
                }
                if (private$.isCompetingRisk()) {
                    self$results$parametricModelSummary$setNote("compete",
                        "Parametric survival models are shown for standard (single-event) survival and are not available for competing-risks analyses.")
                    return()
                }

                mydata <- results$cleanData
                df <- data.frame(
                    .time   = jmvcore::toNumeric(mydata[[results$name1time]]),
                    .status = jmvcore::toNumeric(mydata[[results$name2outcome]])
                )
                keep <- stats::complete.cases(df) & df$.time > 0
                df   <- df[keep, , drop = FALSE]
                if (nrow(df) < 5 || sum(df$.status, na.rm = TRUE) < 2) {
                    self$results$parametricModelSummary$setNote("data",
                        "Not enough events / observations to fit a parametric survival model.")
                    return()
                }

                factorName <- results$name3explanatory
                useCov <- isTRUE(self$options$parametric_covariates) &&
                          !is.null(factorName) && factorName %in% names(mydata)
                if (useCov) {
                    df$.grp <- factor(mydata[[factorName]][keep])
                    if (nlevels(df$.grp) < 2) useCov <- FALSE
                }
                rhs  <- if (useCov) ".grp" else "1"
                form <- stats::as.formula(paste0("survival::Surv(.time, .status) ~ ", rhs))
                dist <- self$options$parametric_distribution

                fitOne <- function(dd) {
                    if (dd == "survspline") {
                        flexsurv::flexsurvspline(form, data = df,
                            k = as.integer(self$options$spline_knots),
                            scale = self$options$spline_scale)
                    } else {
                        flexsurv::flexsurvreg(form, data = df, dist = dd)
                    }
                }

                fit <- tryCatch(fitOne(dist), error = function(e) e)
                if (inherits(fit, "error")) {
                    self$results$parametricModelSummary$setNote("fit",
                        paste0("Parametric fit failed: ", conditionMessage(fit)))
                    return()
                }

                # ---- Model summary (parameters + Wald p for covariate terms) ----
                res     <- fit$res
                covrows <- if (useCov) grep("^\\.grp", rownames(res)) else integer(0)
                smy     <- self$results$parametricModelSummary
                smy$deleteRows()
                for (i in seq_len(nrow(res))) {
                    est <- res[i, "est"]; se <- res[i, "se"]
                    pv  <- if (i %in% covrows && is.finite(se) && se > 0)
                               2 * stats::pnorm(-abs(est / se)) else NA_real_
                    prm <- rownames(res)[i]
                    if (useCov)
                        prm <- sub("^\\.grp", paste0(results$myexplanatory_labelled, ": "), prm)
                    smy$addRow(rowKey = i, values = list(
                        parameter = prm, estimate = est, se = se,
                        ci_lower = res[i, "L95%"], ci_upper = res[i, "U95%"], pvalue = pv
                    ))
                }
                smy$setNote("modelfit", paste0(
                    private$.distLabel(dist), " model - AIC ", round(AIC(fit), 1),
                    ", BIC ", round(BIC(fit), 1),
                    ", log-likelihood ", round(as.numeric(stats::logLik(fit)), 1),
                    if (useCov) paste0(". ", private$.covariateScaleNote(dist)) else "."
                ))

                # ---- Distribution comparison (AIC / BIC / logLik) ----
                if (isTRUE(self$options$compare_distributions)) {
                    cmp <- self$results$parametricModelComparison
                    cmp$deleteRows()
                    for (dd in c("exp","weibull","lnorm","llogis","gamma","gengamma","gompertz")) {
                        f <- tryCatch(flexsurv::flexsurvreg(form, data = df, dist = dd),
                                      error = function(e) NULL)
                        if (is.null(f)) next
                        cmp$addRow(rowKey = dd, values = list(
                            distribution = private$.distLabel(dd),
                            aic = AIC(f), bic = BIC(f),
                            loglik = as.numeric(stats::logLik(f)), df = f$npars
                        ))
                    }
                }

                # ---- Fitted-vs-Kaplan-Meier plot state (serializable coords) ----
                if (isTRUE(self$options$parametric_survival_plots)) {
                    grid <- seq(min(df$.time), max(df$.time), length.out = 100)
                    fitted_df <- tryCatch({
                        ps <- summary(fit, type = "survival", t = grid, tidy = TRUE)
                        names(ps)[names(ps) == "est"] <- "surv"
                        ps$grp <- if (useCov) as.character(ps$.grp) else "Overall"
                        ps[, c("time", "surv", "grp")]
                    }, error = function(e) NULL)
                    km_df <- tryCatch({
                        km  <- survival::survfit(form, data = df)
                        grp <- if (useCov) sub("^\\.grp=", "", rep(names(km$strata), km$strata))
                               else rep("Overall", length(km$time))
                        data.frame(time = km$time, surv = km$surv, grp = grp)
                    }, error = function(e) NULL)
                    self$results$parametricSurvivalPlot$setState(list(
                        fitted = fitted_df, km = km_df,
                        dist = private$.distLabel(dist),
                        timeLabel = self$options$timetypeoutput
                    ))
                }
            }
            ,
            .plotParametricSurvival = function(image, ggtheme, theme, ...) {
                state <- image$state
                if (is.null(state) || is.null(state$fitted) || nrow(state$fitted) == 0)
                    return(FALSE)
                if (!requireNamespace("ggplot2", quietly = TRUE)) return(FALSE)

                p <- ggplot2::ggplot() +
                    ggplot2::geom_line(
                        data = state$fitted,
                        ggplot2::aes(x = time, y = surv, color = grp),
                        linewidth = 1
                    )
                if (!is.null(state$km) && nrow(state$km) > 0) {
                    p <- p + ggplot2::geom_step(
                        data = state$km,
                        ggplot2::aes(x = time, y = surv, color = grp),
                        linetype = "dashed", alpha = 0.6
                    )
                }
                p <- p +
                    ggplot2::scale_y_continuous(limits = c(0, 1), name = "Survival probability") +
                    ggplot2::labs(
                        x = paste0("Time (", state$timeLabel, ")"),
                        title = paste0("Parametric survival (", state$dist, ") vs Kaplan-Meier"),
                        color = "Group",
                        caption = "Solid = fitted parametric model; dashed = Kaplan-Meier"
                    ) +
                    ggtheme
                print(p)
                TRUE
            }

            # ================================================================
            # Age-Adjusted Cox Regression (Part A)
            # ================================================================
            ,
            .addAgeVariable = function(mydata) {
                # Pull age variable from self$data into cleanData
                # Follows same pattern as RCS variable (line ~4434)
                age_var <- self$options$age_variable
                if (is.null(age_var) || !(age_var %in% names(self$data))) {
                    return(list(mydata = mydata, age_col_name = NULL))
                }
                age_col <- jmvcore::toNumeric(self$data[[age_var]])
                mydata[[age_var]] <- age_col[private$.originalRowIndex(mydata)]
                mydata <- mydata[!is.na(mydata[[age_var]]), , drop = FALSE]
                return(list(mydata = mydata, age_col_name = age_var))
            }
            ,
            .ageAdjustedCox = function(results) {
                # Skip if competing risk analysis
                if (private$.isCompetingRisk()) {
                    return()
                }

                mytime <- results$name1time
                mytime <- jmvcore::constructFormula(terms = mytime)
                myoutcome <- results$name2outcome
                myoutcome <- jmvcore::constructFormula(terms = myoutcome)
                myfactor <- results$name3explanatory
                myfactor <- jmvcore::constructFormula(terms = myfactor)
                mydata <- results$cleanData
                mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])

                # Pull age variable into data
                age_result <- private$.addAgeVariable(mydata)
                mydata <- age_result$mydata
                age_col_name <- age_result$age_col_name

                if (is.null(age_col_name)) {
                    self$results$ageAdjustedCoxTable$setNote("error",
                        .("Age variable not found in data."))
                    return()
                }

                if (nrow(mydata) < 10) {
                    self$results$ageAdjustedCoxTable$setNote("error",
                        .("Insufficient observations for age-adjusted analysis after removing missing values."))
                    return()
                }

                myformula <- private$.buildSurvFormula(mytime, myoutcome, ns_prefix = FALSE)

                # Determine age adjustment mode
                use_age_strata <- self$options$age_stratified_cox
                use_age_covariate <- !use_age_strata

                # Also check if existing stratified_cox is enabled
                existing_strata_var <- NULL
                if (self$options$stratified_cox && !is.null(self$options$strata_variable) && self$options$strata_variable != "") {
                    existing_strata_var <- self$options$strata_variable
                    if (existing_strata_var %in% names(self$data)) {
                        strata_col <- self$data[[existing_strata_var]]
                        mydata[[existing_strata_var]] <- strata_col[private$.originalRowIndex(mydata)]
                        mydata <- mydata[!is.na(mydata[[existing_strata_var]]), , drop = FALSE]
                    } else {
                        existing_strata_var <- NULL
                    }
                }

                # Shared across tryCatch blocks for building the interpretation text
                interpretation_parts <- list()

                # ---- 1. Age-Adjusted Cox (covariate or stratified) ----
                tryCatch({
                    if (use_age_strata) {
                        # Create age groups from cutpoints
                        cutpoints_str <- self$options$age_group_cutpoints
                        cutpoints <- as.numeric(trimws(strsplit(cutpoints_str, ",")[[1]]))
                        cutpoints <- cutpoints[!is.na(cutpoints)]
                        if (length(cutpoints) == 0) cutpoints <- c(50, 65, 75)
                        cutpoints <- sort(unique(cutpoints))

                        mydata$age_group <- cut(mydata[[age_col_name]],
                            breaks = c(-Inf, cutpoints, Inf),
                            include.lowest = TRUE)

                        # Build formula with age strata
                        rhs <- paste0(myfactor, " + strata(age_group)")
                        if (!is.null(existing_strata_var)) {
                            rhs <- paste0(rhs, " + strata(", existing_strata_var, ")")
                            # Warn about cross-stratification
                            n_cross <- length(unique(interaction(mydata$age_group, mydata[[existing_strata_var]])))
                            n_events <- sum(mydata[[myoutcome]] == 1, na.rm = TRUE)
                            if (n_events / n_cross < 5) {
                                self$results$ageAdjustedCoxTable$setNote("sparse",
                                    .("Warning: Cross-stratification creates sparse strata (< 5 events per stratum). Consider using fewer age groups or removing one stratification variable."))
                            }
                        }

                        cox_adjusted <- survival::coxph(.asSurvivalFormula(paste(private$.buildSurvFormula(mytime, myoutcome), "~", rhs)), data = mydata)

                        heading_text <- paste0(
                            "Age-Stratified Cox Model\n",
                            "Age groups: ", paste(levels(mydata$age_group), collapse = ", "), "\n",
                            "N = ", nrow(mydata))
                        self$results$ageAdjustedCoxHeading$setContent(heading_text)

                    } else {
                        # Age as covariate - use finalfit for side-by-side table
                        safe_age_name <- jmvcore::composeTerm(age_col_name)
                        rhs_adjusted <- c(myfactor, safe_age_name)
                        if (!is.null(existing_strata_var)) {
                            rhs_adjusted <- c(rhs_adjusted, paste0("strata(", existing_strata_var, ")"))
                        }

                        heading_text <- paste0(
                            "Age-Adjusted Cox Regression\n",
                            "Adjusting for: ", age_col_name, "\n",
                            "N = ", nrow(mydata))
                        self$results$ageAdjustedCoxHeading$setContent(heading_text)
                    }

                    # Fit both unadjusted and adjusted models for comparison table
                    surv_lhs <- private$.buildSurvFormula(mytime, myoutcome)
                    cox_unadj <- survival::coxph(.asSurvivalFormula(paste(surv_lhs, "~", myfactor)), data = mydata)

                    if (use_age_strata) {
                        formula_adj_str <- paste(surv_lhs, "~", rhs)
                    } else {
                        rhs_adj <- paste0(myfactor, " + ", jmvcore::composeTerm(age_col_name))
                        if (!is.null(existing_strata_var)) {
                            rhs_adj <- paste0(rhs_adj, " + strata(", existing_strata_var, ")")
                        }
                        formula_adj_str <- paste(surv_lhs, "~", rhs_adj)
                    }
                    cox_adjusted <- survival::coxph(.asSurvivalFormula(formula_adj_str), data = mydata)

                    # Extract HRs for the group variable (not age)
                    unadj_summary <- summary(cox_unadj)
                    adj_summary <- summary(cox_adjusted)

                    unadj_coefs <- unadj_summary$conf.int
                    adj_coefs <- adj_summary$conf.int

                    # Populate age-adjusted Cox table
                    ageTable <- self$results$ageAdjustedCoxTable

                    # Find group-related rows in adjusted model
                    adj_names <- rownames(adj_coefs)
                    unadj_names <- rownames(unadj_coefs)

                    for (i in seq_along(unadj_names)) {
                        term_name <- unadj_names[i]

                        # Format unadjusted HR
                        hr_unadj <- sprintf("%.2f (%.2f-%.2f)",
                            unadj_coefs[i, 1],
                            unadj_coefs[i, 3],
                            unadj_coefs[i, 4])

                        # Find matching term in adjusted model
                        adj_idx <- match(term_name, adj_names)
                        if (!is.na(adj_idx)) {
                            hr_adj <- sprintf("%.2f (%.2f-%.2f)",
                                adj_coefs[adj_idx, 1],
                                adj_coefs[adj_idx, 3],
                                adj_coefs[adj_idx, 4])
                        } else {
                            hr_adj <- "-"
                        }

                        # Parse variable and level from coefficient name
                        var_name <- myfactor
                        level_name <- sub(paste0("^", myfactor), "", term_name)

                        ageTable$addRow(rowKey = i, values = list(
                            variable = var_name,
                            levels = level_name,
                            n = as.character(nrow(mydata)),
                            hr_unadjusted = hr_unadj,
                            hr_age_adjusted = hr_adj
                        ))
                    }

                    # Add age row for covariate model
                    if (use_age_covariate) {
                        age_idx <- match(age_col_name, adj_names)
                        if (!is.na(age_idx)) {
                            hr_age <- sprintf("%.2f (%.2f-%.2f)",
                                adj_coefs[age_idx, 1],
                                adj_coefs[age_idx, 3],
                                adj_coefs[age_idx, 4])
                            p_age <- adj_summary$coefficients[age_idx, 5]

                            ageTable$addRow(rowKey = length(unadj_names) + 1, values = list(
                                variable = age_col_name,
                                levels = sprintf("per unit (p=%s)", format.pval(p_age, digits = 3)),
                                n = "",
                                hr_unadjusted = "-",
                                hr_age_adjusted = hr_age
                            ))
                        }
                    }

                    # Generate interpretation
                    interpretation_parts <- list()

                    # Compare unadjusted vs adjusted HRs
                    if (nrow(unadj_coefs) > 0 && nrow(adj_coefs) > 0) {
                        # Use log-HR scale for confounding detection (Rothman & Greenland convention)
                        log_hr_unadj <- log(unadj_coefs[1, 1])
                        log_hr_adj <- log(adj_coefs[1, 1])
                        hr_change <- abs(log_hr_adj - log_hr_unadj) / abs(log_hr_unadj) * 100

                        if (hr_change > 10) {
                            interpretation_parts <- c(interpretation_parts,
                                sprintf("<p><b>The log hazard ratio (the model coefficient) changed by %.1f%% after age adjustment.</b> A change of this size is the conventional change-in-estimate signal for confounding by age, but it does not establish it: the Cox model is non-collapsible, so adjusted and unadjusted hazard ratios differ in magnitude even when the added covariate is unrelated to the exposure. If age lies on the causal pathway between %s and survival, adjustment removes part of the association rather than removing bias. This comparison covers the first coefficient of the model only.</p>",
                                    hr_change, myfactor))
                        } else {
                            interpretation_parts <- c(interpretation_parts,
                                sprintf("<p><b>The log hazard ratio (the model coefficient) changed by %.1f%% after age adjustment</b>, below the conventional 10%% change-in-estimate threshold. A small change does not rule out confounding by age in the relationship between %s and survival: opposing biases can cancel, the change-in-estimate rule is a heuristic with no inferential guarantee, and this comparison covers the first coefficient of the model only.</p>",
                                    hr_change, myfactor))
                        }
                    }

                    if (use_age_strata) {
                        interpretation_parts <- c(interpretation_parts,
                            sprintf("<p>The model stratifies by age groups (%s), allowing different baseline hazards for each age group.</p>",
                                paste(levels(mydata$age_group), collapse = ", ")))
                    }

                    self$results$ageAdjustedInterpretation$setContent(
                        paste(interpretation_parts, collapse = "\n"))

                }, error = function(e) {
                    self$results$ageAdjustedCoxTable$setNote("error",
                        jmvcore::format(
                            .("Age-adjusted Cox regression failed: {message}"),
                            message = conditionMessage(e)
                        ))
                })

                # ---- 2. Age Interaction Test ----
                if (self$options$age_interaction) {
                    tryCatch({
                        formula_interaction_str <- paste0(
                            "survival::Surv(", mytime, ", ", myoutcome, ") ~ ",
                            myfactor, " * ", jmvcore::composeTerm(age_col_name))
                        cox_interaction <- survival::coxph(
                            .asSurvivalFormula(formula_interaction_str), data = mydata)

                        int_summary <- summary(cox_interaction)
                        int_coefs <- int_summary$coefficients

                        intTable <- self$results$ageInteractionTable
                        for (i in seq_len(nrow(int_coefs))) {
                            intTable$addRow(rowKey = i, values = list(
                                term = rownames(int_coefs)[i],
                                coef = int_coefs[i, 1],
                                hr = exp(int_coefs[i, 1]),
                                se = int_coefs[i, 3],
                                z = int_coefs[i, 4],
                                pvalue = int_coefs[i, 5]
                            ))
                        }

                        # Check if any interaction terms are significant
                        int_terms <- grep(":", rownames(int_coefs), value = TRUE)
                        if (length(int_terms) > 0) {
                            int_pvals <- int_coefs[int_terms, 5, drop = FALSE]
                            any_sig <- any(int_pvals < 0.05)

                            if (any_sig) {
                                int_msg <- "<p><b>Significant age-group interaction detected.</b> The estimated effect of the explanatory variable on survival differed by age group in this sample. Age-stratified results may describe the data better than a single adjusted hazard ratio.</p>"
                            } else {
                                int_msg <- "<p><b>No significant age-group interaction was detected.</b> Interaction tests have low power, so this does not establish that the effect is constant across age groups. Inspect the interaction hazard ratios and their confidence intervals: if they are wide, effect modification of clinically important size has not been excluded. The choice between age adjustment and stratification rests on the proportional hazards check and on subject-matter grounds, not on this p-value.</p>"
                            }
                            # Append interaction message to existing interpretation
                            # Note: jamovi result objects don't expose a .content property,
                            # so we rebuild from interpretation_parts captured in outer scope.
                            interpretation_parts <- c(interpretation_parts, int_msg)
                            self$results$ageAdjustedInterpretation$setContent(
                                paste(interpretation_parts, collapse = "\n"))
                        }

                    }, error = function(e) {
                        self$results$ageInteractionTable$setNote("error",
                            jmvcore::format(
                                .("Age interaction test failed: {message}"),
                                message = conditionMessage(e)
                            ))
                    })
                }

                # ---- 3. Explanation ----
                if (self$options$showExplanations) {
                    explanation <- paste0(
                        "<h4>Age-Adjusted Survival Analysis</h4>",
                        "<p>Age adjustment accounts for differences in age distribution between ",
                        "comparison groups. This is critical because cancer incidence and mortality ",
                        "are strongly age-dependent.</p>",
                        "<h5>Methods Available:</h5>",
                        "<ul>",
                        "<li><b>Age as Covariate:</b> Includes age in the Cox model (Surv ~ group + age). ",
                        "Assumes a log-linear relationship between age and hazard. Reports the HR for ",
                        "the group variable adjusted for age.</li>",
                        "<li><b>Age-Stratified Cox:</b> Creates age groups and allows separate baseline ",
                        "hazards (Surv ~ group + strata(age_group)). No assumption about the age-hazard ",
                        "relationship. Preferred when the proportional hazards assumption for age is violated.</li>",
                        "<li><b>Age Interaction Test:</b> Tests whether the group effect varies by age ",
                        "(Surv ~ group * age). A significant interaction suggests the treatment/exposure ",
                        "effect differs across age groups.</li>",
                        "</ul>",
                        "<h5>Interpretation Guide:</h5>",
                        "<p>A change of more than 10% in the log hazard ratio (the model coefficient, which is ",
                        "the quantity reported above as the change) after age adjustment is the conventional ",
                        "change-in-estimate signal for confounding by age. It does not establish confounding: ",
                        "the Cox model is non-collapsible, so adjusted and unadjusted hazard ratios differ in ",
                        "magnitude even when the added covariate is unrelated to the exposure, and a small ",
                        "change does not rule confounding out either. If the interaction test is significant, ",
                        "age-stratified results may be more informative than a single adjusted HR.</p>",
                        "<h5>References:</h5>",
                        "<p>Rothman KJ, Greenland S, Lash TL. Modern Epidemiology. 3rd ed. ",
                        "Lippincott Williams & Wilkins; 2008.</p>")
                    self$results$ageAdjustedExplanation$setContent(explanation)
                }
            }

            # ================================================================
            # Age as Time Scale Cox Model
            # ================================================================
            # Uses Surv(age_at_entry, age_at_event, event) instead of
            # Surv(followup_time, event). Most rigorous for cancer epidemiology.
            # ================================================================
            ,
            .ageTimeScaleCox = function(results) {
                tryCatch({
                    mytime <- results$name1time
                    mytime <- jmvcore::constructFormula(terms = mytime)
                    myoutcome <- results$name2outcome
                    myoutcome <- jmvcore::constructFormula(terms = myoutcome)
                    myfactor <- results$name3explanatory
                    myfactor <- jmvcore::constructFormula(terms = myfactor)
                    mydata <- results$cleanData
                    mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])

                    # Pull age variable
                    age_result <- private$.addAgeVariable(mydata)
                    mydata <- age_result$mydata
                    age_col_name <- age_result$age_col_name

                    if (is.null(age_col_name)) {
                        self$results$ageTimeScaleTable$setNote("error",
                            .("Age variable not found in data."))
                        return()
                    }

                    # Calculate age at entry and age at event/censoring
                    # age_at_entry = age (at diagnosis/enrollment)
                    # age_at_event = age + follow-up time
                    # Both must be in same units (typically years)
                    age_entry <- mydata[[age_col_name]]
                    followup_time <- mydata[[mytime]]

                    # Determine time unit from option
                    time_unit <- self$options$timetypeoutput
                    if (is.null(time_unit)) time_unit <- "months"

                    # Convert follow-up to years for age scale
                    followup_years <- switch(time_unit,
                        "days"   = followup_time / 365.25,
                        "weeks"  = followup_time / 52.18,
                        "months" = followup_time / 12,
                        "years"  = followup_time,
                        followup_time / 12  # default assume months
                    )

                    age_exit <- age_entry + followup_years

                    # Validate
                    valid <- !is.na(age_entry) & !is.na(age_exit) & age_exit > age_entry & age_entry >= 0
                    if (sum(valid) < 10) {
                        self$results$ageTimeScaleTable$setNote("error",
                            .("Insufficient valid observations for age-as-time-scale analysis. Check that age and follow-up time are in compatible units."))
                        return()
                    }

                    mydata <- mydata[valid, , drop = FALSE]
                    age_entry <- age_entry[valid]
                    age_exit <- age_exit[valid]
                    mydata$age_entry <- age_entry
                    mydata$age_exit <- age_exit

                    # Fit Cox model with age as time scale
                    formula_str <- paste0(
                        "survival::Surv(age_entry, age_exit, ", myoutcome, ") ~ ", myfactor)
                    cox_age <- survival::coxph(.asSurvivalFormula(formula_str), data = mydata)

                    # Refuse to publish a non-converged fit.
                    #
                    # With few events on the age time scale coxph can diverge and
                    # still return an object. It was written straight to the
                    # table, producing hazard ratios like 1,615,474,749 with a CI
                    # of (0, Inf) and an interpretation panel underneath
                    # narrating it as though it had estimated something. A
                    # coefficient whose |value| is enormous, or whose standard
                    # error is not finite, is a divergence, not a finding.
                    cf <- stats::coef(cox_age)
                    se <- suppressWarnings(sqrt(diag(stats::vcov(cox_age))))
                    diverged <- length(cf) == 0 || any(!is.finite(cf)) ||
                                any(abs(cf) > 20) || any(!is.finite(se)) || any(se > 20)

                    if (isTRUE(diverged)) {
                        self$results$ageTimeScaleTable$setNote("noconverge", paste0(
                            "Age-as-time-scale model did not converge, so no hazard ratio is ",
                            "reported. This normally means one group has no events on the age ",
                            "scale, or the groups are perfectly separated. Use the standard ",
                            "time scale, or combine categories, before interpreting."))
                        return()
                    }

                    cox_summary <- summary(cox_age)
                    coefs <- cox_summary$conf.int

                    # Populate results table
                    ageTable <- self$results$ageTimeScaleTable
                    for (i in seq_len(nrow(coefs))) {
                        term_name <- rownames(coefs)[i]
                        var_name <- myfactor
                        level_name <- sub(paste0("^", myfactor), "", term_name)

                        ageTable$addRow(rowKey = i, values = list(
                            variable = var_name,
                            levels = level_name,
                            hr = coefs[i, 1],
                            ci_lower = coefs[i, 3],
                            ci_upper = coefs[i, 4],
                            pvalue = cox_summary$coefficients[i, 5]
                        ))
                    }

                    # Interpretation
                    interpretation <- paste0(
                        "<h4>Cox Model with Age as Time Scale</h4>",
                        "<p>This model uses biological age as the time axis instead of ",
                        "follow-up time from enrollment. The survival function represents ",
                        "the probability of surviving past a given age, conditional on ",
                        "being alive at age of enrollment.</p>",
                        "<p>N = ", nrow(mydata),
                        " | Age range: ", round(min(age_entry), 1), " - ",
                        round(max(age_exit), 1), " years</p>",
                        "<p><b>When to use:</b> Preferred for cancer epidemiology where ",
                        "biological age is the primary risk driver. Avoids bias from ",
                        "left truncation in age-related diseases.</p>",
                        "<p><b>Reference:</b> Thiebaut & Benichou, Stat Med 2004; ",
                        "Korn, Graubard & Midthune, Stat Med 1997.</p>")
                    self$results$ageTimeScaleInterpretation$setContent(interpretation)

                }, error = function(e) {
                    self$results$ageTimeScaleTable$setNote("error",
                        jmvcore::format(
                            .("Age-as-time-scale analysis failed: {message}"),
                            message = conditionMessage(e)
                        ))
                })
            }

            # ================================================================
            # Age Standardization (SMR / Direct Standardization)
            # ================================================================
            ,
            .ageStandardization = function(results) {
                tryCatch({
                    mytime <- results$name1time
                    mytime <- jmvcore::constructFormula(terms = mytime)
                    myoutcome <- results$name2outcome
                    myoutcome <- jmvcore::constructFormula(terms = myoutcome)
                    myfactor <- results$name3explanatory
                    myfactor <- jmvcore::constructFormula(terms = myfactor)
                    mydata <- results$cleanData
                    mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])

                    # Pull age variable
                    age_result <- private$.addAgeVariable(mydata)
                    mydata <- age_result$mydata
                    age_col_name <- age_result$age_col_name

                    if (is.null(age_col_name)) {
                        self$results$ageStandardizationTable$setNote("error",
                            .("Age variable not found in data."))
                        return()
                    }

                    # Create age groups
                    cutpoints_str <- self$options$age_group_cutpoints
                    cutpoints <- as.numeric(trimws(strsplit(cutpoints_str, ",")[[1]]))
                    cutpoints <- cutpoints[!is.na(cutpoints)]
                    if (length(cutpoints) == 0) cutpoints <- c(50, 65, 75)
                    cutpoints <- sort(unique(cutpoints))

                    mydata$age_group <- cut(mydata[[age_col_name]],
                        breaks = c(-Inf, cutpoints, Inf),
                        include.lowest = TRUE)

                    event_col <- mydata[[myoutcome]]
                    group_col <- mydata[[myfactor]]
                    groups <- levels(factor(group_col))

                    smrTable <- self$results$ageStandardizationTable
                    method <- self$options$age_standardization_method

                    if (method == "indirect") {
                        # Indirect standardization: compare each group to overall
                        # Expected deaths = sum(age-specific rate in reference * person-time in study)
                        age_groups <- levels(mydata$age_group)

                        # Age-specific reference rates on the PERSON-TIME scale.
                        #
                        # These were previously the proportion of each age group
                        # that ever had an event -- tapply(event == 1, ..., mean).
                        # A proportion is not a rate: it rises simply because
                        # follow-up is longer, and it treats a censored patient
                        # the same as one followed to the end. Expected counts
                        # built from it, and the SMR and Poisson interval built
                        # from those, all inherit that dependence on follow-up
                        # while being presented as age-standardized mortality.
                        # events / person-time is the quantity the SMR needs.
                        time_col_std <- jmvcore::toNumeric(mydata[[mytime]])
                        ref_events <- tapply(event_col == 1, mydata$age_group, sum, na.rm = TRUE)
                        ref_pt     <- tapply(time_col_std,   mydata$age_group, sum, na.rm = TRUE)
                        ref_rates  <- ifelse(!is.na(ref_pt) & ref_pt > 0,
                                             ref_events / ref_pt, NA_real_)

                        for (g_idx in seq_along(groups)) {
                            g <- groups[g_idx]
                            group_mask <- group_col == g
                            group_data <- mydata[group_mask, ]
                            group_time <- time_col_std[group_mask]

                            observed <- sum(group_data[[myoutcome]] == 1, na.rm = TRUE)

                            # Expected: reference rate x this group's person-time
                            # in each age band.
                            expected <- 0
                            for (ag in age_groups) {
                                in_ag <- which(group_data$age_group == ag)
                                pt_in_ag <- sum(group_time[in_ag], na.rm = TRUE)
                                rate <- ref_rates[ag]
                                if (!is.na(rate) && pt_in_ag > 0) {
                                    expected <- expected + pt_in_ag * rate
                                }
                            }

                            if (expected > 0) {
                                smr <- observed / expected

                                # CI using exact Poisson method
                                ci <- stats::poisson.test(observed, expected)
                                smr_lower <- ci$conf.int[1]
                                smr_upper <- ci$conf.int[2]
                                p_val <- ci$p.value
                            } else {
                                smr <- NA
                                smr_lower <- NA
                                smr_upper <- NA
                                p_val <- NA
                            }

                            smrTable$addRow(rowKey = g_idx, values = list(
                                group = g,
                                observed = observed,
                                expected = round(expected, 2),
                                smr = round(smr, 3),
                                smr_ci_lower = round(smr_lower, 3),
                                smr_ci_upper = round(smr_upper, 3),
                                pvalue = p_val
                            ))
                        }

                        smrTable$setNote("internal_ref",
                            "Expected deaths derived from the overall study cohort (internal reference). CIs assume fixed external reference rates and may be anti-conservative for internal comparisons.")

                        # Interpretation
                        interpretation <- paste0(
                            "<h4>Indirect Age Standardization (SMR)</h4>",
                            "<p>The Standardized Mortality Ratio (SMR) compares observed deaths ",
                            "in each group to the number expected based on age-specific rates ",
                            "from the overall study population.</p>",
                            "<ul>",
                            "<li><b>SMR = 1.0:</b> Mortality is as expected for the age distribution</li>",
                            "<li><b>SMR > 1.0:</b> More deaths than expected (excess mortality)</li>",
                            "<li><b>SMR < 1.0:</b> Fewer deaths than expected (protective effect)</li>",
                            "</ul>",
                            "<p>95% CI based on exact Poisson method. ",
                            "Age groups used: ", paste(age_groups, collapse = ", "), "</p>")

                    } else {
                        # Direct standardization: apply group-specific rates to a standard population
                        age_groups <- levels(mydata$age_group)
                        total_n <- nrow(mydata)

                        # Standard weights: the cohort's age distribution of
                        # PERSON-TIME, and group rates on the same scale. As with
                        # the indirect method above, the age-specific quantity was
                        # the proportion ever having an event, which is not a rate
                        # and depends on how long each group happened to be
                        # followed.
                        time_col_std <- jmvcore::toNumeric(mydata[[mytime]])
                        std_pt <- tapply(time_col_std, mydata$age_group, sum, na.rm = TRUE)
                        std_pt[is.na(std_pt)] <- 0
                        total_pt <- sum(std_pt)
                        std_weights <- if (total_pt > 0) std_pt / total_pt else std_pt

                        for (g_idx in seq_along(groups)) {
                            g <- groups[g_idx]
                            group_mask <- group_col == g
                            group_data <- mydata[group_mask, ]
                            group_time <- time_col_std[group_mask]

                            observed <- sum(group_data[[myoutcome]] == 1, na.rm = TRUE)

                            # Age-specific event RATES in this group (events per
                            # unit person-time), not proportions.
                            g_events <- tapply(group_data[[myoutcome]] == 1,
                                               group_data$age_group, sum, na.rm = TRUE)
                            g_pt     <- tapply(group_time, group_data$age_group,
                                               sum, na.rm = TRUE)
                            group_rates <- ifelse(!is.na(g_pt) & g_pt > 0,
                                                  g_events / g_pt, NA_real_)

                            # Directly standardized rate: group rates weighted by
                            # the standard person-time distribution.
                            std_rate <- 0
                            for (ag in names(std_weights)) {
                                rate <- group_rates[ag]
                                if (!is.na(rate)) {
                                    std_rate <- std_rate + rate * std_weights[ag]
                                }
                            }

                            # Report the DIRECTLY STANDARDIZED RATE, not an O/E ratio.
                            #
                            # O/E is the INDIRECT estimand. Computing it here as
                            # observed / (std_rate * total_pt) divided every group by
                            # the same whole-cohort person-time, so the ratio collapsed:
                            # on data where group B's age-specific mortality was exactly
                            # DOUBLE group A's in every band, both groups printed
                            # SMR = 0.5 and the real two-fold difference vanished. The
                            # standardized rates themselves (the quantity direct
                            # standardization exists to produce) were never shown.
                            #
                            # A directly standardized rate is a weighted sum of
                            # independent Poisson rates, so its variance is
                            # sum(w_i^2 * d_i / pt_i^2); the CI is taken on the log
                            # scale to keep it positive.
                            var_std <- 0
                            for (ag in names(std_weights)) {
                                d_ag  <- g_events[ag]; pt_ag <- g_pt[ag]
                                if (!is.na(d_ag) && !is.na(pt_ag) && pt_ag > 0)
                                    var_std <- var_std +
                                        (as.numeric(std_weights[ag])^2) * d_ag / (pt_ag^2)
                            }
                            se_log <- if (std_rate > 0 && var_std > 0)
                                sqrt(var_std) / std_rate else NA_real_
                            rate_lo <- if (is.na(se_log)) NA_real_ else std_rate * exp(-1.96 * se_log)
                            rate_hi <- if (is.na(se_log)) NA_real_ else std_rate * exp( 1.96 * se_log)

                            smrTable$addRow(rowKey = g_idx, values = list(
                                group = g,
                                observed = observed,
                                expected = round(as.numeric(std_rate * sum(g_pt, na.rm = TRUE)), 2),
                                smr = round(as.numeric(std_rate), 5),
                                smr_ci_lower = round(as.numeric(rate_lo), 5),
                                smr_ci_upper = round(as.numeric(rate_hi), 5),
                                pvalue = NA
                            ))
                        }

                        smrTable$setNote("directrate", paste0(
                            "Direct method: the 'SMR' column holds the DIRECTLY STANDARDIZED RATE ",
                            "(events per unit person-time, using the cohort's person-time age ",
                            "distribution as the standard), with a log-scale 95% interval. ",
                            "Compare groups by the ratio of these rates. 'Expected' is the events ",
                            "this group's standardized rate implies over its own person-time. ",
                            "Switch to the indirect method if you want an observed/expected SMR."))

                        interpretation <- paste0(
                            "<h4>Direct Age Standardization</h4>",
                            "<p>Age-specific rates from each group are applied to the overall ",
                            "study population age distribution (as the standard). This produces ",
                            "standardized rates that are comparable across groups regardless of ",
                            "their age composition.</p>",
                            "<p>Standard population: overall study cohort. ",
                            "Age groups: ", paste(age_groups, collapse = ", "), "</p>")
                    }

                    self$results$ageStandardizationInterpretation$setContent(interpretation)

                }, error = function(e) {
                    self$results$ageStandardizationTable$setNote("error",
                        jmvcore::format(
                            .("Age standardization failed: {message}"),
                            message = conditionMessage(e)
                        ))
                })
            }

            # ================================================================
            # Age-Stratified KM Plot
            # ================================================================
            ,
            .plotAgeStratifiedKM = function(image, ggtheme, theme, ...) {
                if (!self$options$age_adjustment || !self$options$age_stratified_km) {
                    return(FALSE)
                }
                # This renderer rebuilds its own 0/1 indicator from self$data
                # rather than the recoded status, so it is not inverted -- but
                # the age-adjusted TABLES it belongs with are refused under
                # competing risks, and a lone surviving curve there reads as a
                # result the analysis is standing behind.
                if (private$.isCompetingRisk())
                    return(private$.competingRiskPlotRefusal(
                        "Age-stratified Kaplan-Meier curves", ggtheme))
                if (is.null(self$options$age_variable)) {
                    return(FALSE)
                }

                tryCatch({
                    mytime <- self$options$elapsedtime
                    myoutcome <- self$options$outcome
                    myfactor <- self$options$explanatory

                    if (is.null(mytime) || is.null(myoutcome) || is.null(myfactor)) {
                        return(FALSE)
                    }

                    # Build data from self$data
                    mydata <- self$data
                    time_col <- jmvcore::toNumeric(mydata[[mytime]])
                    outcome_col <- as.numeric(as.character(mydata[[myoutcome]]))
                    group_col <- as.factor(mydata[[myfactor]])
                    age_col <- jmvcore::toNumeric(mydata[[self$options$age_variable]])

                    # Handle outcomeLevel
                    if (!is.null(self$options$outcomeLevel) && self$options$outcomeLevel != "") {
                        outcome_level <- self$options$outcomeLevel
                        original_outcome <- mydata[[myoutcome]]
                        outcome_col <- as.integer(original_outcome == outcome_level)
                    }

                    plot_data <- data.frame(
                        time = time_col,
                        event = outcome_col,
                        group = group_col,
                        age = age_col,
                        stringsAsFactors = FALSE
                    )
                    plot_data <- plot_data[complete.cases(plot_data), ]

                    if (nrow(plot_data) < 10) return(FALSE)

                    # Create age groups
                    cutpoints_str <- self$options$age_group_cutpoints
                    cutpoints <- as.numeric(trimws(strsplit(cutpoints_str, ",")[[1]]))
                    cutpoints <- cutpoints[!is.na(cutpoints)]
                    if (length(cutpoints) == 0) cutpoints <- c(50, 65, 75)
                    cutpoints <- sort(unique(cutpoints))

                    plot_data$age_group <- cut(plot_data$age,
                        breaks = c(-Inf, cutpoints, Inf),
                        include.lowest = TRUE)

                    # Create combined factor for coloring
                    plot_data$strata <- interaction(plot_data$group, plot_data$age_group,
                        sep = " | Age: ")

                    # Fit survfit
                    fit <- survival::survfit(
                        survival::Surv(time, event) ~ age_group + group,
                        data = plot_data)

                    # Plot using survminer
                    p <- survminer::ggsurvplot(
                        fit,
                        data = plot_data,
                        conf.int = self$options$ci95,
                        risk.table = self$options$risktable,
                        pval = FALSE,
                        legend.title = "Age Group + Group",
                        ggtheme = ggtheme,
                        title = "Age-Stratified Kaplan-Meier Curves",
                        facet.by = "age_group"
                    )

                    print(p)
                    return(TRUE)

                }, error = function(e) {
                    return(FALSE)
                })
            }

            # ================================================================
            # Adjusted Survival Curves (Phase 1A)
            # ================================================================
            # Shows KM-style curves adjusted for age using the Cox model.
            # Uses survminer::ggadjustedcurves() - the #1 most requested
            # survival feature in jamovi forums.
            # ================================================================
            ,
            .plotAdjustedCurves = function(image, ggtheme, theme, ...) {
                if (!self$options$age_adjustment || !self$options$adjusted_curves) {
                    return(FALSE)
                }
                # Same as .plotAgeStratifiedKM: rebuilt from self$data, so not
                # inverted, but it belongs to an age-adjusted block that is
                # refused under competing risks.
                if (private$.isCompetingRisk())
                    return(private$.competingRiskPlotRefusal(
                        "Age-adjusted survival curves", ggtheme))
                if (is.null(self$options$age_variable) || is.null(self$options$explanatory)) {
                    return(FALSE)
                }

                tryCatch({
                    mytime <- self$options$elapsedtime
                    myoutcome <- self$options$outcome
                    myfactor <- self$options$explanatory
                    age_var <- self$options$age_variable

                    if (is.null(mytime) || is.null(myoutcome)) return(FALSE)

                    # Build data
                    mydata <- self$data
                    time_col <- jmvcore::toNumeric(mydata[[mytime]])
                    age_col <- jmvcore::toNumeric(mydata[[age_var]])
                    group_col <- as.factor(mydata[[myfactor]])

                    # Handle outcome level
                    outcome_raw <- mydata[[myoutcome]]
                    if (!is.null(self$options$outcomeLevel) && self$options$outcomeLevel != "") {
                        outcome_col <- as.integer(outcome_raw == self$options$outcomeLevel)
                    } else {
                        outcome_col <- as.numeric(as.character(outcome_raw))
                    }

                    plot_data <- data.frame(
                        time = time_col,
                        event = outcome_col,
                        group = group_col,
                        age = age_col,
                        stringsAsFactors = FALSE
                    )
                    plot_data <- plot_data[complete.cases(plot_data), ]

                    if (nrow(plot_data) < 10) return(FALSE)

                    # Fit age-adjusted Cox model
                    cox_fit <- survival::coxph(
                        survival::Surv(time, event) ~ group + age,
                        data = plot_data
                    )

                    # Generate adjusted curves
                    p <- survminer::ggadjustedcurves(
                        cox_fit,
                        data = plot_data,
                        variable = "group",
                        method = "average",
                        ggtheme = ggtheme,
                        legend.title = myfactor,
                        title = paste0("Survival Curves Adjusted for ", age_var)
                    )

                    # Add subtitle with model info
                    p <- p + ggplot2::labs(
                        subtitle = paste0("Cox model: Surv(time, event) ~ ",
                            myfactor, " + ", age_var, "  (N=", nrow(plot_data), ")")
                    )

                    print(p)
                    return(TRUE)

                }, error = function(e) {
                    return(FALSE)
                })
            }

            # ================================================================
            # REMARK Reporting Checklist (Phase 4)
            # ================================================================
            # Generates an HTML checklist based on REMARK guidelines for
            # tumor marker prognostic studies. Shows which items are addressed.
            # ================================================================
            ,
            .generateRemarkChecklist = function(results) {
                if (!self$options$remark_checklist) return()

                # REMARK checklist items (McShane et al., JNCI 2005)
                # Check which are addressed by current analysis configuration
                items <- list()

                # Item 1: State marker, study objectives, hypothesis
                items[[1]] <- list(
                    item = "1. Introduction",
                    desc = "State the marker examined, study objectives, and any pre-specified hypotheses",
                    status = if (!is.null(self$options$explanatory)) "addressed" else "not_addressed",
                    note = if (!is.null(self$options$explanatory))
                        paste0("Marker/factor: ", self$options$explanatory) else "Select an explanatory variable"
                )

                # Item 2: Patient population
                items[[2]] <- list(
                    item = "2. Patients",
                    desc = "Describe the patient population (eligibility criteria, clinical/pathological characteristics)",
                    status = "user_action",
                    note = "Describe in manuscript Methods section"
                )

                # Item 3: Specimen characteristics
                items[[3]] <- list(
                    item = "3. Specimen characteristics",
                    desc = "Describe type of specimen, handling, and storage conditions",
                    status = "user_action",
                    note = "Describe in manuscript Methods section"
                )

                # Item 4: Assay methods
                items[[4]] <- list(
                    item = "4. Assay methods",
                    desc = "Specify the method used to determine marker status and scoring criteria",
                    status = "user_action",
                    note = "Describe in manuscript Methods section"
                )

                # Item 5: Study design
                items[[5]] <- list(
                    item = "5. Study design",
                    desc = "State the study design (prospective, retrospective, case-control)",
                    status = "user_action",
                    note = "Describe in manuscript Methods section"
                )

                # Item 6: Statistical methods - survival
                has_km <- TRUE  # survival function always shows KM
                has_cox <- TRUE  # Cox is always computed
                has_age_adj <- self$options$age_adjustment
                # Not the option test: an outcomeorganizer 0/1/2 hand-off has
                # multievent = FALSE, so the checklist would claim no competing
                # risks for the one analysis that actually has them.
                has_competing <- private$.isCompetingRisk()

                methods_note <- paste0(
                    "Kaplan-Meier estimation",
                    if (has_cox) "; Cox proportional hazards regression" else "",
                    if (has_age_adj) paste0("; Age-adjusted (", self$options$age_variable, ")") else "",
                    if (has_competing) "; Competing risks analysis" else "",
                    if (self$options$ph_cox) "; Proportional hazards assumption tested" else ""
                )
                items[[6]] <- list(
                    item = "6. Statistical analysis methods",
                    desc = "Specify statistical methods including survival analysis, handling of missing data, multiplicity",
                    status = "addressed",
                    note = methods_note
                )

                # Item 7: Time and endpoint
                items[[7]] <- list(
                    item = "7. Time-to-event endpoint",
                    desc = "Define the primary endpoint and how time was measured",
                    status = if (!is.null(self$options$elapsedtime) && !is.null(self$options$outcome)) "addressed" else "not_addressed",
                    note = if (!is.null(self$options$elapsedtime))
                        paste0("Time: ", self$options$elapsedtime, "; Outcome: ", self$options$outcome,
                            if (!is.null(self$options$outcomeLevel)) paste0(" (level: ", self$options$outcomeLevel, ")") else "")
                    else "Select time and outcome variables"
                )

                # Item 8: Cutpoint determination
                items[[8]] <- list(
                    item = "8. Cutpoint determination",
                    desc = "Explain how marker cutpoints were determined (pre-specified, data-driven, validated)",
                    status = "user_action",
                    note = "If using categorical groups, describe cutpoint rationale in Methods"
                )

                # Item 9: Model building
                items[[9]] <- list(
                    item = "9. Multivariable model building",
                    desc = "Report other variables included in multivariable models and rationale for inclusion",
                    status = if (has_age_adj) "partially_addressed" else "not_addressed",
                    note = if (has_age_adj)
                        paste0("Adjusted for: ", self$options$age_variable, ". Consider additional confounders in multisurvival function.")
                    else "Consider age adjustment or use multisurvival for multivariable analysis"
                )

                # Item 10: Hazard ratio with CI
                items[[10]] <- list(
                    item = "10. Effect estimates (HR with 95% CI)",
                    desc = "Report hazard ratios with confidence intervals for all variables",
                    status = "addressed",
                    note = "Cox regression table provides HR with 95% CI"
                )

                # Item 11: KM curves
                items[[11]] <- list(
                    item = "11. Kaplan-Meier curves",
                    desc = "Provide KM curves showing estimated survival probabilities with numbers at risk",
                    status = if (self$options$sc || self$options$kmunicate) "addressed" else "not_addressed",
                    note = if (self$options$sc) "Survival curves enabled"
                        else if (self$options$kmunicate) "KMunicate plot enabled"
                        else "Enable survival curves in Plots section"
                )

                # Item 12: Survival probabilities at fixed times
                items[[12]] <- list(
                    item = "12. Survival estimates at fixed time points",
                    desc = "Report survival probabilities at clinically relevant time points (e.g., 1, 3, 5 years)",
                    status = if (!is.null(self$options$cutp) && self$options$cutp != "") "addressed" else "partially_addressed",
                    note = paste0("Time points: ", self$options$cutp)
                )

                # Item 13: Validation
                has_bootstrap <- self$options$bootstrapValidation
                has_calibration <- self$options$calibration_curves
                items[[13]] <- list(
                    item = "13. Model validation",
                    desc = "Report internal validation (bootstrap, cross-validation) and/or external validation",
                    status = if (has_bootstrap || has_calibration) "addressed" else "not_addressed",
                    note = paste0(
                        if (has_bootstrap) "Bootstrap internal validation enabled. " else "",
                        if (has_calibration) "Calibration curves enabled. " else "",
                        if (!has_bootstrap && !has_calibration) "Consider enabling bootstrap validation and/or calibration curves" else ""
                    )
                )

                # Item 14: Handling multiple comparisons
                items[[14]] <- list(
                    item = "14. Multiplicity adjustment",
                    desc = "Report adjustment for multiple comparisons if applicable",
                    status = if (self$options$pw) "addressed" else "user_action",
                    note = if (self$options$pw)
                        paste0("Pairwise comparisons with ", self$options$padjustmethod, " correction")
                    else "Enable pairwise comparisons if testing multiple groups"
                )

                # Build HTML
                html_parts <- list("<h3>REMARK Reporting Checklist</h3>")
                html_parts <- c(html_parts, "<p><small>Based on McShane LM et al. <i>JNCI</i> 2005;97(16):1180-1184</small></p>")
                html_parts <- c(html_parts, "<table style='border-collapse: collapse; width: 100%; font-size: 0.9em;'>")
                html_parts <- c(html_parts, "<tr style='background-color: rgba(33, 33, 33, 0.07); color: inherit;'><th style='padding: 6px; text-align: left; border: 1px solid #ddd;'>Item</th><th style='padding: 6px; text-align: left; border: 1px solid #ddd;'>Status</th><th style='padding: 6px; text-align: left; border: 1px solid #ddd;'>Note</th></tr>")

                for (item in items) {
                    icon <- switch(item$status,
                        "addressed" = "&#9989;",          # green check
                        "partially_addressed" = "&#9888;", # warning
                        "not_addressed" = "&#10060;",      # red X
                        "user_action" = "&#9998;"          # pencil
                    )
                    bg <- switch(item$status,
                        "addressed" = "#e8f5e9",
                        "partially_addressed" = "#fff3e0",
                        "not_addressed" = "#ffebee",
                        "user_action" = "#e3f2fd"
                    )
                    html_parts <- c(html_parts, paste0(
                        "<tr style='background: ", bg, ";'>",
                        "<td style='padding: 6px; border: 1px solid #ddd;'><b>", item$item, "</b><br><small>", item$desc, "</small></td>",
                        "<td style='padding: 6px; border: 1px solid #ddd; text-align: center;'>", icon, "</td>",
                        "<td style='padding: 6px; border: 1px solid #ddd;'><small>", htmltools::htmlEscape(item$note), "</small></td>",
                        "</tr>"
                    ))
                }

                html_parts <- c(html_parts, "</table>")
                html_parts <- c(html_parts, "<p><small>",
                    "&#9989; = Addressed by current analysis | ",
                    "&#9888; = Partially addressed | ",
                    "&#10060; = Not yet addressed | ",
                    "&#9998; = Requires user action in manuscript",
                    "</small></p>")

                self$results$remarkChecklist$setContent(paste(html_parts, collapse = "\n"))
            }

        )
    )
