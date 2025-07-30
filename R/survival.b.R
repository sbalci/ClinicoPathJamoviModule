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

survivalClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "survivalClass",
        inherit = survivalBase,
        private = list(

            .init = function() {

                if (self$options$ph_cox) {
                    # Disable tables
                    self$results$cox_ph$setVisible(TRUE)
                }

                if (!(self$options$ph_cox)) {
                    # Disable tables
                    self$results$cox_ph$setVisible(FALSE)
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


                    # Define a mapping from timetypedata to lubridate functions
                    lubridate_functions <- list(
                        ymdhms = lubridate::ymd_hms,
                        ymd = lubridate::ymd,
                        ydm = lubridate::ydm,
                        mdy = lubridate::mdy,
                        myd = lubridate::myd,
                        dmy = lubridate::dmy,
                        dym = lubridate::dym
                    )
                    
                    # Apply the appropriate lubridate function based on timetypedata
                    if (timetypedata %in% names(lubridate_functions)) {
                        date_parser <- lubridate_functions[[timetypedata]]
                        mydata[["start"]] <- date_parser(mydata[[dxdate]])
                        mydata[["end"]] <- date_parser(mydata[[fudate]])
                    } else {
                        stop(paste0("Unknown date format: ", timetypedata, 
                                  ". Supported formats are: ", 
                                  paste(names(lubridate_functions), collapse = ", ")))
                    }


                    if ( sum(!is.na(mydata[["start"]])) == 0 || sum(!is.na(mydata[["end"]])) == 0)  {
                        stop(paste0("Time difference cannot be calculated. Make sure that time type in variables are correct. Currently it is: ", self$options$timetypedata)
                        )
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
                            stop(
                                paste0('Outcome variable must be binary (0/1) for survival analysis.\n',
                                      '- Use 0 for censored observations (alive/disease-free)\n',
                                      '- Use 1 for events (death/recurrence)\n',
                                      'Current values found: ', paste(unique_values, collapse = ", "),
                                      '\n\nFor multi-state outcomes, enable "Multiple Event Levels" option.')
                            )

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
                        stop(
                            paste0('Invalid outcome variable format.\n',
                                  'For survival analysis, the outcome variable must be:\n',
                                  '- Binary numeric (0/1): 0=censored, 1=event\n',
                                  '- Factor variable: Select appropriate event level\n',
                                  '\nCurrent variable type: ', class(outcome1)[1], '\n',
                                  'For complex outcomes with multiple states, enable "Multiple Event Levels" option.')
                        )

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
                    stop("Error: Data could not be cleaned for analysis.")
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


            # Run Analysis ----
            ,
            .run = function() {

                # Errors, Warnings ----

                ## No variable todo ----

                ## Define subconditions ----

                subcondition1a <- !is.null(self$options$outcome)
                subcondition1b1 <- self$options$multievent
                subcondition1b2 <- !is.null(self$options$dod)
                subcondition1b3 <- !is.null(self$options$dooc)
                # subcondition1b4 <- !is.null(self$options$awd)
                # subcondition1b5 <- !is.null(self$options$awod)
                subcondition2a <- !is.null(self$options$elapsedtime)
                subcondition2b1 <- self$options$tint
                subcondition2b2 <- !is.null(self$options$dxdate)
                subcondition2b3 <- !is.null(self$options$fudate)
                condition3 <- !is.null(self$options$explanatory)


                condition1 <- subcondition1a && !subcondition1b1 || subcondition1b1 && subcondition1b2 || subcondition1b1 && subcondition1b3

                condition2 <- subcondition2b1 && subcondition2b2 && subcondition2b3 || subcondition2a && !subcondition2b1 && !subcondition2b2 && !subcondition2b3

                not_continue_analysis <- !(condition1 && condition2 && condition3)

                if (not_continue_analysis) {
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
                    return()
                } else {
                    self$results$todo$setVisible(FALSE)
                }


                # Empty data ----

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

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
                    
                    formula <- paste('survival::Surv(', mytime, ',', myoutcome, ') ~ ', myfactor)
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




                medianTable <- self$results$medianTable
                data_frame <- results2table
                for (i in seq_along(data_frame[, 1, drop = T])) {
                    medianTable$addRow(rowKey = i, values = c(data_frame[i,]))
                }


                ## Median Survival Summary ----

                results1table %>%
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
                        warning(paste("Stratification variable", strata_var, "not found. Using standard Cox regression."))
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
                    coxTable$addRow(rowKey = i, values = c(data_frame[i,]))
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
                    warning(paste("Error calculating residuals:", e$message))
                    return(NULL)
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
                    
                    formula <- paste('survival::Surv(', mytime, ',', myoutcome, ') ~ ', myfactor)
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
                    warning(paste("Error exporting survival data:", e$message))
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
                    survTable$addRow(rowKey = i, values = c(data_frame[i,]))
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

                    pairwiseTable$addRow(rowKey = i, values = c(data_frame[i,]))
                }

                thefactor <-
                    jmvcore::constructFormula(terms = self$options$explanatory)

                title2 <- as.character(thefactor)

                pairwiseTable$setTitle(paste0('Pairwise Comparisons ', title2))

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
                total_events <- sum(mydata[[myoutcome]])

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
                    # Create time intervals
                    breaks <- c(0, time_intervals, max(mydata[[mytime]]) * 1.1)

                    # Loop through intervals
                    for (i in 1:(length(breaks)-1)) {
                        start_time <- breaks[i]
                        end_time <- breaks[i+1]

                        # Add checkpoint for responsiveness
                        if (i %% 5 == 0) {
                            private$.checkpoint(FALSE)
                        }

                        # Filter data for this interval
                        if (i == 1) {
                            # For first interval, include patients from the beginning
                            interval_data <- mydata
                            # But truncate follow-up time to the interval end
                            follow_up_times <- pmin(mydata[[mytime]], end_time)
                            # Count only events that occurred within this interval
                            events_in_interval <- sum(mydata[[myoutcome]] == 1 & mydata[[mytime]] <= end_time)
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
                            events_in_interval <- sum(interval_data[[myoutcome]] == 1 &
                                                          interval_data[[mytime]] <= end_time &
                                                          interval_data[[mytime]] > start_time)
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

                myformula <-
                    paste("survival::Surv(", mytime, ",", myoutcome, ")")

                title2 <- as.character(myfactor)

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
                        title = paste0("Survival curves for ", title2),
                        subtitle = "Based on Kaplan-Meier estimates",
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

                myformula <-
                    paste("survival::Surv(", mytime, ",", myoutcome, ")")

                title2 <- as.character(myfactor)

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

                myformula <-
                    paste("survival::Surv(", mytime, ",", myoutcome, ")")

                title2 <- as.character(myfactor)


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

                title2 <- as.character(myfactor)

                # Create log-log plot
                tryCatch({
                    plot7 <- plotData %>%
                        finalfit::surv_plot(
                            .data = .,
                            dependent = paste("Surv(", mytime, ",", myoutcome, ")"),
                            explanatory = myfactor,
                            xlab = paste0('log(Time) (', self$options$timetypeoutput, ')'),
                            ylab = 'log(-log(Survival))',
                            title = paste0("Log-Log Plot for ", title2),
                            subtitle = "Assessment of Proportional Hazards Assumption",
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
                            title = paste0("Log-Log Plot for ", title2),
                            subtitle = "Parallel lines suggest proportional hazards"
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


                title2 <- as.character(myfactor)


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
                            title = "Cox Model Residual Diagnostics",
                            subtitle = "Martingale residuals should be randomly scattered around zero"
                        ) +
                        ggtheme
                    
                    print(plot9)
                    TRUE
                    
                }, error = function(e) {
                    warning(paste("Error creating residuals plot:", e$message))
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
                    <h4>Understanding Median Survival Analysis</h4>
                    <p><strong>Median Survival Time:</strong> The time point at which 50% of patients have experienced the event (death, relapse, etc.).</p>
                    <ul>
                        <li><strong>Interpretation:</strong> If median survival is 24 months, half the patients survive longer than 24 months</li>
                        <li><strong>Confidence Intervals:</strong> Provide uncertainty range around the median estimate</li>
                        <li><strong>Not Reached (NR):</strong> Indicates that less than 50% of patients experienced the event during follow-up</li>
                        <li><strong>Records vs Events:</strong> Records = total patients, Events = patients who experienced the outcome</li>
                    </ul>
                    <p><em>Clinical significance:</em> Median survival is robust to outliers and provides an easily interpretable summary of survival experience.</p>
                </div>
                ')
                
                # Cox Regression Explanation
                private$.setExplanationContent("coxRegressionExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: #fff3cd; border-left: 4px solid #ffc107;">
                    <h4>Understanding Cox Regression Analysis</h4>
                    <p><strong>Hazard Ratio (HR):</strong> Measures the relative risk of experiencing the event between groups.</p>
                    <ul>
                        <li><strong>HR = 1:</strong> No difference in hazard between groups</li>
                        <li><strong>HR > 1:</strong> Higher hazard (worse prognosis) in the comparison group</li>
                        <li><strong>HR < 1:</strong> Lower hazard (better prognosis) in the comparison group</li>
                        <li><strong>95% CI:</strong> If confidence interval includes 1, the difference is not statistically significant</li>
                        <li><strong>P-value:</strong> Tests whether HR significantly differs from 1</li>
                    </ul>
                    <p><em>Example:</em> HR = 2.0 means the hazard is twice as high in the comparison group (twice the risk of event).</p>
                </div>
                ')
                
                # Survival Tables Explanation
                private$.setExplanationContent("survivalTablesExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: #d4edda; border-left: 4px solid #28a745;">
                    <h4>Understanding Survival Probability Tables</h4>
                    <p><strong>Survival Probabilities:</strong> Estimated proportion of patients surviving at specific time points.</p>
                    <ul>
                        <li><strong>Time:</strong> Specific time points (e.g., 12, 36, 60 months)</li>
                        <li><strong>Number at Risk:</strong> Patients still under observation at that time point</li>
                        <li><strong>Number of Events:</strong> Patients who experienced the outcome by that time</li>
                        <li><strong>Survival %:</strong> Percentage of patients surviving past that time point</li>
                        <li><strong>95% CI:</strong> Confidence interval for the survival estimate</li>
                    </ul>
                    <p><em>Clinical use:</em> These tables provide specific survival rates commonly reported in medical literature (1-year, 3-year, 5-year survival).</p>
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
                    <h4>Understanding Survival Curves and Plots</h4>
                    <p><strong>Survival Curves:</strong> Visual representation of survival probability over time.</p>
                    <ul>
                        <li><strong>Survival Curves:</strong> Kaplan-Meier estimates showing probability of survival</li>
                        <li><strong>Cumulative Events:</strong> Shows cumulative probability of experiencing the event</li>
                        <li><strong>Cumulative Hazard:</strong> Displays cumulative hazard function over time</li>
                        <li><strong>Log-Log Plot:</strong> Tests proportional hazards assumption (parallel lines expected)</li>
                        <li><strong>Risk Tables:</strong> Show number of patients at risk at different time points</li>
                        <li><strong>Confidence Bands:</strong> Indicate uncertainty around survival estimates</li>
                    </ul>
                    <p><em>Interpretation:</em> Curves that separate early suggest different survival experiences between groups.</p>
                </div>
                ')
            }

        )
    )
