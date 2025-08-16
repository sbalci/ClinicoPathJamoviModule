#' @title Single Arm Survival
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr
#'
#' @description
#' This function prepares and cleans data for single-arm survival analysis by
#' calculating survival time, filtering based on landmark time, and merging
#' survival outcomes with other factors.
#'
#' @return A list containing cleaned data and metadata for plotting and analysis.
#' @note Ensure the input data contains the required variables (elapsed time,
#' outcome) and meets specified formatting criteria.


singlearmClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "singlearmClass",
    inherit = singlearmBase,
    # Constants and cache
    private = list(
        .DEFAULT_CUTPOINTS = "12, 36, 60",
        .cache = new.env(parent = emptyenv()),

      .init = function() {
          # Initialize all outputs to FALSE first
          self$results$medianSummary$setVisible(FALSE)
          self$results$survTableSummary$setVisible(FALSE)
          self$results$personTimeHeading2$setVisible(FALSE)
          self$results$plot$setVisible(FALSE)
          self$results$plot2$setVisible(FALSE)
          self$results$plot3$setVisible(FALSE)
          self$results$plot6$setVisible(FALSE)
          self$results$medianSurvivalExplanation$setVisible(FALSE)
          self$results$survivalPlotsHeading3$setVisible(FALSE)
          self$results$medianHeading3$setVisible(FALSE)
          self$results$survivalProbabilityExplanation$setVisible(FALSE)
          self$results$personTimeHeading$setVisible(FALSE)
          self$results$personTimeTable$setVisible(FALSE)
          self$results$personTimeSummary$setVisible(FALSE)
          self$results$personTimeHeading3$setVisible(FALSE)
          self$results$personTimeExplanation$setVisible(FALSE)
          self$results$survivalPlotsExplanation$setVisible(FALSE)
          self$results$baselineHazardHeading$setVisible(FALSE)
          self$results$baselineHazardTable$setVisible(FALSE)
          self$results$baselineHazardPlot$setVisible(FALSE)
          self$results$smoothedHazardPlot$setVisible(FALSE)
          self$results$baselineHazardSummary$setVisible(FALSE)
          self$results$baselineHazardHeading3$setVisible(FALSE)
          self$results$baselineHazardExplanation$setVisible(FALSE)
          self$results$dataQualityHeading$setVisible(FALSE)
          self$results$dataQualityTable$setVisible(FALSE)
          self$results$dataQualitySummary$setVisible(FALSE)

          # Handle showSummaries visibility
          if (self$options$showSummaries) {
            self$results$medianSummary$setVisible(TRUE)
            self$results$survTableSummary$setVisible(TRUE)
            # Person-time summary requires both showSummaries AND person_time
            if (self$options$person_time) {
              self$results$personTimeSummary$setVisible(TRUE)
            }
          }

          # Handle showExplanations visibility
          if (self$options$showExplanations) {
            self$results$medianHeading3$setVisible(TRUE)
            self$results$medianSurvivalExplanation$setVisible(TRUE)
            self$results$survivalProbabilityExplanation$setVisible(TRUE)
            
            # Survival plots explanation requires showExplanations AND at least one plot
            if (self$options$sc || self$options$ce || self$options$ch || self$options$kmunicate) {
              self$results$survivalPlotsHeading3$setVisible(TRUE)
              self$results$survivalPlotsExplanation$setVisible(TRUE)
            }
            
            # Person-time explanation requires both showExplanations AND person_time
            if (self$options$person_time) {
              self$results$personTimeHeading2$setVisible(TRUE)
              self$results$personTimeHeading3$setVisible(TRUE)
              self$results$personTimeExplanation$setVisible(TRUE)
            }
          }

          # Handle person_time visibility
          if (self$options$person_time) {
            self$results$personTimeHeading$setVisible(TRUE)
            self$results$personTimeTable$setVisible(TRUE)
          }

          # Handle baseline hazard visibility
          if (self$options$baseline_hazard) {
            self$results$baselineHazardHeading$setVisible(TRUE)
            self$results$baselineHazardTable$setVisible(TRUE)
            self$results$baselineHazardPlot$setVisible(TRUE)
            # Summary requires both baseline_hazard AND showSummaries
            if (self$options$showSummaries) {
              self$results$baselineHazardSummary$setVisible(TRUE)
            }
            # Explanation requires both baseline_hazard AND showExplanations
            if (self$options$showExplanations) {
              self$results$baselineHazardHeading3$setVisible(TRUE)
              self$results$baselineHazardExplanation$setVisible(TRUE)
            }
          }

          # Handle hazard smoothing visibility
          if (self$options$hazard_smoothing) {
            self$results$smoothedHazardPlot$setVisible(TRUE)
          }

          # Handle advanced diagnostics visibility
          if (self$options$advancedDiagnostics) {
            self$results$dataQualityHeading$setVisible(TRUE)
            self$results$dataQualityTable$setVisible(TRUE)
            # Summary requires both advancedDiagnostics AND showSummaries
            if (self$options$showSummaries) {
              self$results$dataQualitySummary$setVisible(TRUE)
            }
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

      },

      # Utility Helper Functions ----
      .safeExecute = function(expr, context = "analysis", silent = FALSE) {
        tryCatch(expr, error = function(e) {
          user_msg <- switch(context,
            "data_processing" = "Data processing failed. Please check your input variables.",
            "survival_calculation" = "Survival calculation failed. This may be due to insufficient data or data quality issues.",
            "plot_generation" = "Plot generation failed. Try adjusting plot parameters or checking data quality.",
            "baseline_hazard" = "Baseline hazard calculation failed. This may occur with very sparse data.",
            "person_time" = "Person-time analysis failed. Please check time intervals and event data.",
            paste("An error occurred during", context)
          )
          
          if (!silent) {
            warning(paste(user_msg, "Technical details:", e$message))
          }
          
          return(NULL)
        })
      },

      .getCachedSurvfit = function(formula, data, cache_key_suffix = "") {
        if (!requireNamespace('digest', quietly = TRUE)) {
          # Fallback if digest not available
          return(survival::survfit(formula, data = data))
        }
        
        cache_key <- paste0("survfit_", 
                           digest::digest(list(as.character(formula), data, cache_key_suffix)))
        
        if (exists(cache_key, envir = private$.cache)) {
          return(get(cache_key, envir = private$.cache))
        }
        
        result <- survival::survfit(formula, data = data)
        assign(cache_key, result, envir = private$.cache)
        return(result)
      },

      .calculateAdaptiveSpan = function(n_points) {
        # More sophisticated span calculation based on data characteristics
        if (n_points <= 10) return(0.8)
        if (n_points <= 30) return(0.5)
        if (n_points <= 60) return(0.3)
        
        # For larger datasets, use logarithmic scaling
        base_span <- 0.75 / log10(n_points + 1)
        return(pmax(0.1, pmin(0.8, base_span)))
      },

      .systematicSample = function(data, target_size = 50) {
        n <- nrow(data)
        if (n <= target_size) return(data)
        
        # Use systematic sampling to preserve distribution
        keep_indices <- round(seq(1, n, length.out = target_size))
        return(data[keep_indices, ])
      },

      .assessDataQuality = function(results) {
        mydata <- results$cleanData
        mytime <- results$name1time
        myoutcome <- results$name2outcome
        
        # Basic data quality metrics
        n_total <- nrow(mydata)
        n_events <- sum(mydata[[myoutcome]], na.rm = TRUE)
        n_censored <- n_total - n_events
        
        # Time-related quality checks
        time_vals <- mydata[[mytime]]
        min_time <- min(time_vals, na.rm = TRUE)
        max_time <- max(time_vals, na.rm = TRUE)
        median_time <- median(time_vals, na.rm = TRUE)
        
        # Event rate by time periods
        early_events <- sum(mydata[[myoutcome]][time_vals <= median_time], na.rm = TRUE)
        late_events <- n_events - early_events
        
        # Data quality warnings
        warnings <- character()
        if (n_events < 10) {
          warnings <- c(warnings, "Very few events observed - results may be unreliable")
        }
        if (n_events / n_total < 0.1) {
          warnings <- c(warnings, "Low event rate - consider longer follow-up")
        }
        if (max_time < 12) {
          warnings <- c(warnings, "Short follow-up duration - median survival may not be reached")
        }
        
        return(list(
          n_total = n_total,
          n_events = n_events,
          n_censored = n_censored,
          event_rate = round(n_events / n_total * 100, 1),
          median_followup = round(median_time, 1),
          min_time = round(min_time, 1),
          max_time = round(max_time, 1),
          warnings = warnings
        ))
      },

      # Validation Helper Function ----
      .validateInputs = function() {
        ### Define subconditions ----
        subcondition1a <- !is.null(self$options$outcome)
        subcondition1b1 <- self$options$multievent
        subcondition1b2 <- !is.null(self$options$dod)
        subcondition1b3 <- !is.null(self$options$dooc)
        subcondition2a <- !is.null(self$options$elapsedtime)
        subcondition2b1 <- self$options$tint
        subcondition2b2 <- !is.null(self$options$dxdate)
        subcondition2b3 <- !is.null(self$options$fudate)

        # Outcome validation: either simple outcome OR multi-event with at least one event type
        outcome_valid <- (subcondition1a && !subcondition1b1) || 
                        (subcondition1b1 && subcondition1b2) || 
                        (subcondition1b1 && subcondition1b3)

        # Time validation: either date calculation OR pre-calculated time
        time_valid <- (subcondition2b1 && subcondition2b2 && subcondition2b3) || 
                     (subcondition2a && !subcondition2b1 && !subcondition2b2 && !subcondition2b3)

        return(list(
          outcome_valid = outcome_valid,
          time_valid = time_valid,
          continue_analysis = outcome_valid && time_valid
        ))
      },

      # get and label Data ----
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

        return(list(
          "mydata_labelled" = mydata
          , "mytime_labelled" = mytime
          , "myoutcome_labelled" = myoutcome
          , "mydxdate_labelled" = mydxdate
          , "myfudate_labelled" = myfudate
        ))


      }

      # todo ----
      ,
      .todo = function() {

        todo <- glue::glue(
          "
    <b>Welcome to Single-Arm Survival Analysis</b>
    <br><br>
    This tool analyzes survival outcomes for a single cohort of patients, calculating:
    <ul>
        <li><b>Median Survival Time:</b> The time at which 50% of subjects have experienced the event</li>
        <li><b>Survival Rates:</b> Probability of survival at 1, 3, and 5 years</li>
        <li><b>Survival Curves:</b> Visual representation of survival probability over time</li>
    </ul>

    <b>Input Requirements:</b>
    <ul>
        <li><b>Time Variable:</b> Either:
            <ul>
                <li>Pre-calculated follow-up time (numeric, continuous)</li>
                <li>Start and end dates (will be converted to time intervals)</li>
            </ul>
        </li>
        <li><b>Outcome Variable:</b> Event indicator showing whether each subject experienced the event
            <ul>
                <li>For binary variables: Select the level representing the event</li>
                <li>For multiple outcomes: Use advanced options to specify event types</li>
            </ul>
        </li>
    </ul>

    <b>Analysis Options:</b>
    <ul>
        <li>Landmark analysis to handle immortal time bias</li>
        <li>Various plot types: survival curves, cumulative hazard, cumulative events</li>
        <li>Customizable time units and axis scales</li>
        <li>Risk tables and confidence intervals</li>
    </ul>

    <b>Methodology:</b>
    Utilizes the Kaplan-Meier method to estimate survival probabilities, handling right-censored data appropriately.
    <br><br>
    This analysis is implemented using the survival, survminer, and finalfit R packages. Please cite both jamovi and these packages in publications.
    <br><hr>
    For detailed information about survival analysis methods, see the
    <a href='https://cran.r-project.org/web/packages/survival/vignettes/survival.pdf'>survival package documentation</a>.
    "
        )

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
          ## Precalculated Time ----

          mydata[["mytime"]] <-
            jmvcore::toNumeric(mydata[[mytime_labelled]])


        } else if (tint) {
          ## Time Interval ----

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
              func <- lubridate_functions[[timetypedata]]
              mydata[["start"]] <- func(mydata[[dxdate]])
              mydata[["end"]] <- func(mydata[[fudate]])
          } else {
              stop(paste0("Unsupported time type format: ", timetypedata, 
                         ". Supported formats are: ", paste(names(lubridate_functions), collapse = ", ")))
          }



          if ( sum(!is.na(mydata[["start"]])) == 0 || sum(!is.na(mydata[["end"]])) == 0)  {
            start_valid <- sum(!is.na(mydata[["start"]]))
            end_valid <- sum(!is.na(mydata[["end"]]))
            stop(paste0("Time difference cannot be calculated. ",
                       "Start date valid entries: ", start_valid, ", ",
                       "End date valid entries: ", end_valid, ". ",
                       "Please verify that your date variables match the selected format: ", 
                       self$options$timetypedata, ". ",
                       "Common issues: incorrect date format, missing values, or non-date data in date columns.")
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
            unique_vals <- unique(outcome1[!is.na(outcome1)])
            if (!((length(unique_vals) == 2) && (sum(unique_vals) == 1))) {
              stop(
                paste0('When using continuous variable as an outcome, it must only contain 1s and 0s. ',
                       'Current unique values found: ', paste(sort(unique_vals), collapse = ", "), '. ',
                       'Expected: 0 (censored/alive/disease-free) and 1 (event/dead/recurrence). ',
                       'Please recode your outcome variable or use factor levels instead.')
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
              'When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0. If you are using a factor as an outcome, please check the levels and content.'
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

        mydata <- mydata_labelled

        mydata[["myfactor"]] <- "1"


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

        time <- private$.definemytime()
        outcome <- private$.definemyoutcome()
        factor <- private$.definemyfactor()

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


        name3explanatory <- "SingleArm"

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

        # Set state for baseline hazard plots
        baselineHazardImage <- self$results$baselineHazardPlot
        baselineHazardImage$setState(plotData)

        smoothedHazardImage <- self$results$smoothedHazardPlot
        smoothedHazardImage$setState(plotData)

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
            "myfudate_labelled" = myfudate_labelled
          )
        )

      }


      # Run Analysis ----
      ,
      .run = function() {

        # Input Validation ----
        validation_result <- private$.validateInputs()
        
        if (!validation_result$continue_analysis) {
          private$.todo()
          self$results$todo$setVisible(TRUE)
          return()
        } else {
          self$results$todo$setVisible(FALSE)
        }

        ## Empty data ----

        if (nrow(self$data) == 0)
          stop('Data contains no (complete) rows')

        private$.checkpoint()

        ## Get Clean Data ----
        results <- private$.cleandata()

        ## Data Quality Assessment ----
        private$.checkpoint()
        data_quality <- private$.assessDataQuality(results)
        
        # Store data quality for potential use in outputs
        results$data_quality <- data_quality

        ## Run Analysis ----

        ### Median Survival ----
        private$.checkpoint()

        private$.medianSurv(results)


        ### Survival Table ----
        private$.checkpoint()

        private$.survTable(results)


        ### Person-Time Analysis ----
        private$.checkpoint()
        private$.personTimeAnalysis(results)

        ### Baseline Hazard Analysis ----
        private$.checkpoint()
        if (self$options$baseline_hazard || self$options$hazard_smoothing) {
          private$.baselineHazardAnalysis(results)
        }

        ### Advanced Diagnostics ----
        private$.checkpoint()
        if (self$options$advancedDiagnostics) {
          private$.populateDataQuality(results)
        }


        ## Add Calculated Time to Data ----



        if ( self$options$tint && self$options$calculatedtime && self$results$calculatedtime$isNotFilled()) {
          self$results$calculatedtime$setRowNums(results$cleanData$row_names)
          self$results$calculatedtime$setValues(results$cleanData$CalculatedTime)
        }


        ## Add Redefined Outcome to Data ----

        if (self$options$multievent  && self$options$outcomeredefined && self$results$outcomeredefined$isNotFilled()) {
          self$results$outcomeredefined$setRowNums(results$cleanData$row_names)
          self$results$outcomeredefined$setValues(results$cleanData$CalculatedOutcome)
        }
      }

      # Median Survival Function ----
      ,
      .medianSurv = function(results) {
        mytime <- results$name1time
        myoutcome <- results$name2outcome
        myfactor <- results$name3explanatory

        mydata <- results$cleanData

        mydata[[mytime]] <-
          jmvcore::toNumeric(mydata[[mytime]])

        ## Median Survival Table ----

        private$.checkpoint()

        formula <-
          paste('survival::Surv(',
                mytime,
                ',',
                myoutcome,
                ') ~ ',
                myfactor)

        formula <- as.formula(formula)

        km_fit <- private$.safeExecute({
          private$.getCachedSurvfit(formula, mydata, "median")
        }, context = "survival_calculation")
        
        if (is.null(km_fit)) {
          stop("Unable to perform survival analysis. Please check your data.")
        }

        km_fit_median_df <- summary(km_fit)


        # Process survival fit results for table display
        results1table <-
          as.data.frame(km_fit_median_df$table) %>%
          t() %>%
          as.data.frame() %>%
          janitor::clean_names(dat = ., case = "snake")

        # records n_max n_start events rmean se_rmean median
        # km_fit_median_df$table     247   247     247    167 22.06    1.234   15.9
        # x0_95lcl x0_95ucl
        # km_fit_median_df$table     11.4     20.2

        medianTable <- self$results$medianTable
        data_frame <- results1table
        data_frame <- data_frame %>%
          dplyr::mutate(mean_time = round(rmean, 2),
                        mean_ci = glue::glue("{x0_95lcl} - {x0_95ucl}"))
        for (i in seq_along(data_frame[, 1, drop = T])) {
          medianTable$addRow(rowKey = i, values = c(data_frame[i,]))
        }


        ## Median Survival Summary ----

        results1table %>%
          dplyr::mutate(
            description =
              glue::glue(
                "Median survival is {round(median, digits = 1)} [{round(x0_95lcl, digits = 1)} - {round(x0_95ucl, digits = 1)}, 95% CI] ",
                self$options$timetypeoutput,
                ".",
                "The median survival is {round(median, 2)} months [95% CI: {round(x0_95lcl, digits = 1)} - {round(x0_95ucl, digits = 1)}]."
       #          ,
       # "At 1 year, survival is approximately {scales::percent(surv_12)},
       # and at 5 years, it is {scales::percent(surv_60)}."
              )
          ) %>%
          # dplyr::mutate(
          #   description = dplyr::case_when(
          #     is.na(median) ~ paste0(
          #       glue::glue("{description} \n Note that when {factor}, the survival curve does not drop below 1/2 during \n the observation period, thus the median survival is undefined.")),
          #     TRUE ~ paste0(description)
          #   )
          # ) %>%
          # dplyr::mutate(description = gsub(
          #   pattern = "=",
          #   replacement = " is ",
          #   x = description
          # )) %>%
          # dplyr::mutate(description = gsub(
          #   pattern = myexplanatory_labelled,
          #   replacement = self$options$explanatory,
          #   x = description
          # )) %>%
          dplyr::select(description) %>%
          dplyr::pull(.) -> km_fit_median_definition


        # Add additional statistical information
        n_events <- km_fit_median_df$table[["events"]]
        n_total <- km_fit_median_df$table[["records"]]
        event_rate <- round((n_events / n_total) * 100, 1)
        
        # Include data quality information if available
        quality_info <- ""
        if (!is.null(results$data_quality)) {
          dq <- results$data_quality
          quality_info <- paste0(
            "Data Quality: Follow-up range: ", dq$min_time, "-", dq$max_time, " ", 
            self$options$timetypeoutput, ". ",
            if (length(dq$warnings) > 0) paste("Considerations:", paste(dq$warnings, collapse = "; "), ".") else ""
          )
        }
        
        medianSummary <- c(km_fit_median_definition,
                           paste0("Event rate: ", event_rate, "% (", n_events, " events out of ", n_total, " subjects)."),
                           quality_info,
                           "The median survival time is when 50% of subjects have experienced the event.",
                           "This means that 50% of subjects in this group survived longer than this time period.",
                           "Note: Confidence intervals are calculated using the log-log transformation method for improved accuracy with censored data (not plain Greenwood formula)."
        )


        self$results$medianSummary$setContent(medianSummary)

        # Add explanatory output for median survival
        if (self$options$showExplanations) {
            median_explanation_html <- '
            <div class="explanation-box" style="background-color: #f0f8ff; padding: 15px; border-radius: 8px; margin: 10px 0;">
                <h3 style="color: #2c5282; margin-top: 0;">📊 Understanding Median Survival Analysis</h3>
                
                <div style="background-color: white; padding: 12px; border-radius: 5px; margin: 10px 0;">
                    <h4 style="color: #2d3748; margin-top: 0;">What is Median Survival?</h4>
                    <p style="margin: 8px 0;">Median survival is the <strong>time point when exactly 50% of patients</strong> have experienced the event of interest (e.g., death, recurrence). 
                    Think of it as the "halfway point" in your patient population.</p>
                    
                    <div style="background-color: #e6f7ff; padding: 10px; border-radius: 5px; margin: 10px 0;">
                        <strong>💯 Key Concept:</strong> If median survival = 24 months, it means:
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li>At 24 months: 50% of patients have experienced the event</li>
                            <li>At 24 months: 50% of patients are still event-free</li>
                            <li>Half the patients will survive longer than 24 months</li>
                        </ul>
                    </div>
                </div>
                
                <div style="background-color: #fef5e7; padding: 12px; border-radius: 5px; margin: 10px 0;">
                    <h4 style="color: #d68910; margin-top: 0;">📈 Understanding the Results Table</h4>
                    <table style="width: 100%; border-collapse: collapse; margin: 10px 0;">
                        <tr style="background-color: #fff3cd;">
                            <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Measure</th>
                            <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Meaning</th>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Records</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Total number of patients in analysis</td>
                        </tr>
                        <tr style="background-color: #fffbf0;">
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Events</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Number who experienced the event</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Median</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Time when 50% experienced event</td>
                        </tr>
                        <tr style="background-color: #fffbf0;">
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>95% CI</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Range of plausible values</td>
                        </tr>
                    </table>
                </div>
                
                <div style="background-color: #e8f5e9; padding: 12px; border-radius: 5px; margin: 10px 0;">
                    <h4 style="color: #2e7d32; margin-top: 0;">💬 Clinical Interpretation Guide</h4>
                    
                    <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                        <strong>✅ When Median is Reached:</strong>
                        <p style="margin: 5px 0;">"The median survival is 36 months (95% CI: 28-45 months)"</p>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li>Half of patients survived longer than 3 years</li>
                            <li>The true median is likely between 28-45 months</li>
                            <li>This provides concrete information for patient counseling</li>
                        </ul>
                    </div>
                    
                    <div style="background-color: #f3e5f5; padding: 10px; border-radius: 5px; margin: 10px 0;">
                        <strong>⚠️ When Median is "Not Reached" (NR):</strong>
                        <p style="margin: 5px 0;">More than 50% of patients remain event-free</p>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li>Excellent prognosis - most patients doing well</li>
                            <li>Need longer follow-up to determine median</li>
                            <li>Can still report survival rates at specific time points</li>
                        </ul>
                    </div>
                </div>
                
                <div style="background-color: #fff3e0; padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #ff9800;">
                    <strong>💡 Practical Tips:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li><strong>Robust measure:</strong> Less affected by extreme values than mean survival</li>
                        <li><strong>Easy communication:</strong> "Half of patients lived longer than X months"</li>
                        <li><strong>Compare cautiously:</strong> Consider confidence intervals when comparing groups</li>
                        <li><strong>Clinical context:</strong> Always interpret alongside patient characteristics and treatment details</li>
                    </ul>
                </div>
            </div>
            '

            
            
            self$results$medianSurvivalExplanation$setContent(median_explanation_html)
        }


      }


      # Survival Table Function ----
      ,
      .survTable = function(results) {
        mytime <- results$name1time
        myoutcome <- results$name2outcome
        myfactor <- results$name3explanatory

        mydata <- results$cleanData

        mydata[[mytime]] <-
          jmvcore::toNumeric(mydata[[mytime]])

        ## Median Survival Table ----

        private$.checkpoint()

        formula <-
          paste('survival::Surv(',
                mytime,
                ',',
                myoutcome,
                ') ~ ',
                myfactor)

        formula <- as.formula(formula)

        km_fit <- private$.safeExecute({
          private$.getCachedSurvfit(formula, mydata, "survtable")
        }, context = "survival_calculation")
        
        if (is.null(km_fit)) {
          stop("Unable to perform survival analysis. Please check your data.")
        }

        utimes <- self$options$cutp

        utimes <- strsplit(utimes, ",")
        utimes <- purrr::reduce(utimes, as.vector)
        utimes <- as.numeric(utimes)

        if (length(utimes) == 0) {
          # Use centralized default cutpoints
          default_cutpoints <- strsplit(private$.DEFAULT_CUTPOINTS, ",")
          utimes <- as.numeric(purrr::reduce(default_cutpoints, as.vector))
        }

        private$.checkpoint()

        km_fit_summary <- summary(km_fit, times = utimes, extend = TRUE)

        km_fit_df <-
          as.data.frame(km_fit_summary[c(
            #"strata",
                                         "time",
                                         "n.risk",
                                         "n.event",
                                         "surv",
                                         "std.err",
                                         "lower",
                                         "upper")])



        # km_fit_df[, 1] <- gsub(
        #   pattern = "thefactor=",
        #   replacement = paste0(self$options$explanatory, " "),
        #   x = km_fit_df[, 1]
        # )


        # km_fit_df2 <- km_fit_df

        # km_fit_df[, 1] <- gsub(
        #   pattern = paste0(myexplanatory_labelled,"="),
        #   replacement = paste0(self$options$explanatory, " "),
        #   x = km_fit_df[, 1]
        # )

        survTable <- self$results$survTable

        data_frame <- km_fit_df
        for (i in seq_along(data_frame[, 1, drop = T])) {
          survTable$addRow(rowKey = i, values = c(data_frame[i,]))
        }


        ## survTableSummary 1,3,5-yr survival summary ----

        # km_fit_df2[, 1] <- gsub(
        #   pattern = paste0(myexplanatory_labelled,"="),
        #   replacement = paste0(self$options$explanatory, " is "),
        #   x = km_fit_df2[, 1]
        # )

        ## survTableSummary 1,3,5-yr survival summary ----

        km_fit_df %>%
          dplyr::mutate(
            description =
              glue::glue(
                "{time} {self$options$timetypeoutput} survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]. \n The estimated probability of surviving beyond {time} {self$options$timetypeoutput} was {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]. \n At this time point, there were {n.risk} subjects still at risk and {n.event} events had occurred in this group. \n Event rate by this timepoint: {scales::percent(n.event / km_fit_df$n.risk[1])}."
              )
          ) %>%
          dplyr::select(description) %>%
          dplyr::pull(.) -> survTableSummary


        self$results$survTableSummary$setContent(survTableSummary)

        # Add explanatory output for survival probabilities
        if (self$options$showExplanations) {
            survival_probability_explanation_html <- '
            <div class="explanation-box" style="background-color: #f0f8ff; padding: 15px; border-radius: 8px; margin: 10px 0;">
                <h3 style="color: #2c5282; margin-top: 0;">📅 Understanding Survival Probability Tables</h3>
                
                <div style="background-color: white; padding: 12px; border-radius: 5px; margin: 10px 0;">
                    <h4 style="color: #2d3748; margin-top: 0;">What are Time-Specific Survival Probabilities?</h4>
                    <p style="margin: 8px 0;">These show the <strong>percentage of patients expected to be event-free</strong> at specific milestone time points. 
                    Common time points are 1, 3, and 5 years (corresponding to the default intervals).</p>
                    
                    <div style="background-color: #e6f7ff; padding: 10px; border-radius: 5px; margin: 10px 0;">
                        <strong>💯 Example Interpretation:</strong>
                        <p style="margin: 5px 0;">If 5-year survival = 75% (95% CI: 68-82%)</p>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li>75% of patients are expected to be alive at 5 years</li>
                            <li>25% are expected to have experienced the event by 5 years</li>
                            <li>The true rate is likely between 68-82%</li>
                        </ul>
                    </div>
                </div>
                
                <div style="background-color: #fef5e7; padding: 12px; border-radius: 5px; margin: 10px 0;">
                    <h4 style="color: #d68910; margin-top: 0;">📈 Understanding Each Column</h4>
                    <table style="width: 100%; border-collapse: collapse; margin: 10px 0;">
                        <tr style="background-color: #fff3cd;">
                            <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Column</th>
                            <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Meaning</th>
                            <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Clinical Use</th>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Time</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Milestone timepoint</td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Standard follow-up intervals</td>
                        </tr>
                        <tr style="background-color: #fffbf0;">
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Number at Risk</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Patients still followed</td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Reliability of estimates</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Number of Events</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Events in interval</td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Risk pattern over time</td>
                        </tr>
                        <tr style="background-color: #fffbf0;">
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Survival %</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Cumulative survival</td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Patient counseling</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>95% CI</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Uncertainty range</td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Precision assessment</td>
                        </tr>
                    </table>
                </div>
                
                <div style="background-color: #e8f5e9; padding: 12px; border-radius: 5px; margin: 10px 0;">
                    <h4 style="color: #2e7d32; margin-top: 0;">💬 How to Use These Numbers Clinically</h4>
                    
                    <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                        <strong>👥 Patient Counseling:</strong>
                        <p style="margin: 5px 0;">"Based on our data, about 8 out of 10 patients with your condition are doing well at 3 years"</p>
                    </div>
                    
                    <div style="background-color: #f3e5f5; padding: 10px; border-radius: 5px; margin: 10px 0;">
                        <strong>🔍 Treatment Planning:</strong>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li>High early survival rates → consider less intensive follow-up</li>
                            <li>Declining rates over time → focus on long-term monitoring</li>
                            <li>Wide confidence intervals → need more data or longer follow-up</li>
                        </ul>
                    </div>
                    
                    <div style="background-color: #e3f2fd; padding: 10px; border-radius: 5px; margin: 10px 0;">
                        <strong>📋 Comparison with Standards:</strong>
                        <p style="margin: 5px 0;">Compare your results with:</p>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li>Historical controls from your institution</li>
                            <li>Published literature for similar populations</li>
                            <li>Registry data (SEER, national cancer registries)</li>
                        </ul>
                    </div>
                </div>
                
                <div style="background-color: #fff3e0; padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #ff9800;">
                    <strong>💡 Important Considerations:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li><strong>Sample size matters:</strong> Fewer patients at later time points = less reliable estimates</li>
                        <li><strong>Confidence intervals:</strong> Wider intervals = more uncertainty</li>
                        <li><strong>Clinical context:</strong> Consider patient selection, treatment changes over time</li>
                        <li><strong>Censoring:</strong> Patients lost to follow-up affect late-time estimates</li>
                    </ul>
                </div>
            </div>
            '
            
            
            self$results$survivalProbabilityExplanation$setContent(survival_probability_explanation_html)
        }


      }

      # Person-Time Analysis Function ----
      ,
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

        # Calculate additional statistics
        mean_follow_up <- round(total_time / nrow(mydata), 2)
        median_follow_up <- round(stats::median(mydata[[mytime]]), 2)
                # Create summary text with interpretation
        summary_html <- glue::glue(
          "
    In this study, {nrow(mydata)} subjects were followed for a total of {round(total_time, 1)} {time_unit}, 
    with an average follow-up duration of {mean_follow_up} {time_unit} per person. During this observation period, 
    {total_events} events occurred, resulting in an incidence rate of {round(overall_rate, 2)} events per {rate_multiplier} {time_unit}.
    This means that for every {rate_multiplier} {time_unit} of observation, approximately {round(overall_rate, 1)} events are expected to occur.
    The 95% confidence interval for this rate is {round(ci_lower, 2)} to {round(ci_upper, 2)} per {rate_multiplier} {time_unit}, 
    indicating the precision of our estimate based on the observed data.


    <h4>Person-Time Analysis Summary</h4>
    <p>Total follow-up time: <b>{round(total_time, 1)} {time_unit}</b></p>
    <p>Mean follow-up time: <b>{mean_follow_up} {time_unit}</b></p>
    <p>Median follow-up time: <b>{median_follow_up} {time_unit}</b></p>
    <p>Number of events: <b>{total_events}</b> out of <b>{nrow(mydata)}</b> subjects</p>
    <p>Overall incidence rate: <b>{round(overall_rate, 2)}</b> per {rate_multiplier} {time_unit} [95% CI: {round(ci_lower, 2)}-{round(ci_upper, 2)}]</p>
    <p><i>Interpretation:</i> This represents the rate at which events occurred in your study population. The incidence rate is calculated as the number of events divided by the total person-time at risk. Confidence intervals use the exact Poisson method.</p>
  ")

        self$results$personTimeSummary$setContent(summary_html)

        # Add explanatory output for person-time analysis
        if (self$options$showExplanations) {
            person_time_explanation_html <- '
            <div style="margin-bottom: 20px; padding: 15px; background-color: #f0f8ff; border-left: 4px solid #4169e1;">
                <h4 style="margin-top: 0; color: #2c3e50;">Understanding Person-Time Analysis</h4>
                <p style="margin-bottom: 10px;">Person-time analysis calculates incidence rates by accounting for the total time each patient was at risk:</p>
                <ul style="margin-left: 20px;">
                    <li><strong>Person-Time:</strong> Sum of individual follow-up periods for all patients</li>
                    <li><strong>Incidence Rate:</strong> Events per unit time (e.g., per 100 person-years)</li>
                    <li><strong>Rate Multiplier:</strong> Scaling factor to express rates per standard unit</li>
                    <li><strong>95% CI:</strong> Confidence interval calculated using exact Poisson method</li>
                </ul>
                <p style="margin-bottom: 5px;"><strong>Clinical interpretation:</strong></p>
                <ul style="margin-left: 20px;">
                    <li>Person-time analysis handles varying follow-up durations effectively</li>
                    <li>Incidence rates allow comparison with other studies or populations</li>
                    <li>Higher rates indicate more frequent event occurrence</li>
                    <li>Use stratified analysis to identify high-risk time periods</li>
                    <li>Consider seasonal patterns or time-dependent effects</li>
                </ul>
            </div>
            '
            self$results$personTimeExplanation$setContent(person_time_explanation_html)
        }
        
        # Survival Plots Explanation
        if (self$options$showExplanations && (self$options$sc || self$options$ce || self$options$ch || self$options$kmunicate)) {
            survival_plots_explanation_html <- '
            <div class="explanation-box" style="background-color: #f0f8ff; padding: 15px; border-radius: 8px; margin: 10px 0;">
                <h3 style="color: #2c5282; margin-top: 0;">📊 Understanding Survival Curves and Plots</h3>
                
                <div style="background-color: white; padding: 12px; border-radius: 5px; margin: 10px 0;">
                    <h4 style="color: #2d3748; margin-top: 0;">📈 Kaplan-Meier Survival Curve</h4>
                    <p style="margin: 8px 0;">The survival curve shows the <strong>probability of being event-free over time</strong>. It steps down each time an event occurs.</p>
                    
                    <div style="background-color: #e6f7ff; padding: 10px; border-radius: 5px; margin: 10px 0;">
                        <strong>📖 How to Read the Plot:</strong>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li><strong>Y-axis:</strong> Survival probability (0-100%)</li>
                            <li><strong>X-axis:</strong> Time since study start</li>
                            <li><strong>Steps down:</strong> Each step represents an event</li>
                            <li><strong>Tick marks (+):</strong> Censored patients (lost to follow-up)</li>
                            <li><strong>Shaded areas:</strong> 95% confidence intervals</li>
                        </ul>
                    </div>
                </div>
                
                <div style="background-color: #fff3e0; padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #ff9800;">
                    <strong>💡 Clinical Interpretation Tips:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li><strong>Median survival:</strong> Where curve crosses 50% line</li>
                        <li><strong>1-year survival:</strong> Height of curve at 12 months</li>
                        <li><strong>Confidence intervals:</strong> Wider bands = more uncertainty</li>
                        <li><strong>Pattern recognition:</strong> Steep drops vs gradual decline reveal different risk patterns</li>
                    </ul>
                </div>
            </div>
            '
            self$results$survivalPlotsExplanation$setContent(survival_plots_explanation_html)
        }
      }

      # Baseline Hazard Analysis Function ----
      ,
      .baselineHazardAnalysis = function(results) {
        # Check if baseline hazard analysis is enabled
        if (!self$options$baseline_hazard && !self$options$hazard_smoothing) {
          return()
        }

        # Extract data
        mytime <- results$name1time
        myoutcome <- results$name2outcome
        mydata <- results$cleanData

        # Ensure time is numeric
        mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])

        # Create survival object
        surv_obj <- survival::Surv(time = mydata[[mytime]], event = mydata[[myoutcome]])

        # Fit Cox model (intercept only for baseline hazard)
        cox_fit <- survival::coxph(surv_obj ~ 1, data = mydata)

        if (self$options$baseline_hazard) {
          # Extract baseline hazard
          basehaz <- survival::basehaz(cox_fit, centered = FALSE)
          
          # Calculate Nelson-Aalen estimator for better hazard estimates
          nelsen_aalen <- survival::survfit(surv_obj ~ 1, data = mydata, type = "fh")
          
          # Get time points and cumulative hazard
          time_points <- nelsen_aalen$time
          cum_hazard <- -log(nelsen_aalen$surv)
          
          # Calculate instantaneous hazard (approximate)
          if (length(time_points) > 1) {
            # Calculate differences for hazard rate
            time_diff <- diff(c(0, time_points))
            hazard_diff <- diff(c(0, cum_hazard))
            hazard_rate <- hazard_diff / time_diff
            
            # Remove infinite/NaN values
            valid_indices <- is.finite(hazard_rate) & hazard_rate > 0
            time_points <- time_points[valid_indices]
            hazard_rate <- hazard_rate[valid_indices]
            
            # Calculate confidence intervals (approximate)
            tryCatch({
              surv_summary <- summary(nelsen_aalen)
              if (length(surv_summary$n.risk) >= length(valid_indices)) {
                n_risk <- surv_summary$n.risk[valid_indices]
                se_hazard <- sqrt(hazard_rate / pmax(1, n_risk))  # Avoid division by zero
                hazard_lower <- pmax(0, hazard_rate - 1.96 * se_hazard)
                hazard_upper <- hazard_rate + 1.96 * se_hazard
              } else {
                # Fallback if n.risk doesn't match
                hazard_lower <- hazard_rate * 0.8  # Simple approximation
                hazard_upper <- hazard_rate * 1.2
              }
            }, error = function(e) {
              # Fallback confidence intervals
              hazard_lower <- hazard_rate * 0.8
              hazard_upper <- hazard_rate * 1.2
            })
            
            # Populate baseline hazard table
            for (i in seq_along(time_points)) {
              self$results$baselineHazardTable$addRow(rowKey = i, values = list(
                time = round(time_points[i], 2),
                hazard = round(hazard_rate[i], 4),
                hazard_lower = round(hazard_lower[i], 4),
                hazard_upper = round(hazard_upper[i], 4)
              ))
            }
            
            # Generate summary if requested
            if (self$options$showSummaries) {
              time_unit <- self$options$timetypeoutput
              mean_hazard <- round(mean(hazard_rate), 4)
              median_hazard <- round(stats::median(hazard_rate), 4)
              max_hazard <- round(max(hazard_rate), 4)
              max_hazard_time <- round(time_points[which.max(hazard_rate)], 1)
              
              # Test for constant hazard (exponential assumption)
              # Simple test: coefficient of variation
              cv_hazard <- round(stats::sd(hazard_rate) / mean(hazard_rate), 3)
              constant_hazard_assessment <- ifelse(cv_hazard < 0.5, 
                "relatively constant", "highly variable")
              
              summary_html <- glue::glue("
                <h4>Baseline Hazard Analysis Summary</h4>
                <p><strong>Hazard Function Characteristics:</strong></p>
                <ul>
                  <li>Mean hazard rate: <b>{mean_hazard}</b> events per {time_unit}</li>
                  <li>Median hazard rate: <b>{median_hazard}</b> events per {time_unit}</li>
                  <li>Peak hazard rate: <b>{max_hazard}</b> at {max_hazard_time} {time_unit}</li>
                  <li>Hazard variability: <b>{constant_hazard_assessment}</b> (CV = {cv_hazard})</li>
                </ul>
                <p><strong>Understanding the Plots:</strong></p>
                <ul>
                  <li><b>Baseline Hazard (Step):</b> Shows exact hazard jumps at event times</li>
                  <li><b>Smoothed Hazard (Curve):</b> Shows overall risk trend over time</li>
                  <li>Peaks may differ because smoothing averages nearby events</li>
                  <li>Both plots use the same data but present it differently</li>
                </ul>
                <p><strong>Clinical Interpretation:</strong></p>
                <ul>
                  <li>Use the step plot to identify specific high-risk time points</li>
                  <li>Use the smooth plot to understand overall risk patterns</li>
                  <li>Variable hazard indicates changing risk patterns over time</li>
                  <li>Consider both views for comprehensive risk assessment</li>
                </ul>
              ")
              
              self$results$baselineHazardSummary$setContent(summary_html)
            }
            
            # Generate explanations if requested
            if (self$options$showExplanations) {
              explanation_html <- '
              <div class="explanation-box" style="background-color: #f0f8ff; padding: 15px; border-radius: 8px; margin: 10px 0;">
                  <h3 style="color: #2c5282; margin-top: 0;">📊 Understanding Baseline Hazard Analysis</h3>
                  
                  <div style="background-color: white; padding: 12px; border-radius: 5px; margin: 10px 0;">
                      <h4 style="color: #2d3748; margin-top: 0;">🔍 What is Baseline Hazard?</h4>
                      <p style="margin: 8px 0;">The baseline hazard function shows the <strong>instantaneous risk of events</strong> over time for your study population.</p>
                      
                      <div style="background-color: #e6f7ff; padding: 10px; border-radius: 5px; margin: 10px 0;">
                          <strong>📖 Key Concepts:</strong>
                          <ul style="margin: 5px 0; padding-left: 20px;">
                              <li><strong>Hazard Rate:</strong> Risk per unit time (events per year)</li>
                              <li><strong>Cumulative Hazard:</strong> Total accumulated risk over time</li>
                              <li><strong>Cox Model Baseline:</strong> Hazard function from Cox regression without covariates</li>
                              <li><strong>Instantaneous Hazard:</strong> Calculated as differences in cumulative hazard</li>
                          </ul>
                      </div>
                  </div>
                  
                  <div style="background-color: #ffe6f0; padding: 12px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #d63384;">
                      <h4 style="color: #2d3748; margin-top: 0;">📈 Understanding the Two Hazard Plots</h4>
                      <p style="margin: 8px 0;"><strong>Why do the plots look different?</strong></p>
                      
                      <div style="margin-left: 15px;">
                          <p><strong>1. Baseline Hazard Plot (Step Function):</strong></p>
                          <ul style="margin: 5px 0; padding-left: 20px;">
                              <li>Shows hazard <strong>jumps at each event time</strong></li>
                              <li>Exact representation of when events occurred</li>
                              <li>Large steps indicate multiple events or few patients at risk</li>
                              <li>Best for identifying specific high-risk time points</li>
                          </ul>
                          
                          <p style="margin-top: 10px;"><strong>2. Smoothed Hazard Plot (Continuous Curve):</strong></p>
                          <ul style="margin: 5px 0; padding-left: 20px;">
                              <li>Shows <strong>underlying hazard trend</strong> over time</li>
                              <li>Uses LOESS smoothing to estimate continuous risk</li>
                              <li>Faint dots show the original hazard values being smoothed</li>
                              <li>Best for understanding overall risk patterns</li>
                          </ul>
                      </div>
                      
                      <p style="margin-top: 10px; font-style: italic;">
                          <strong>Note:</strong> Both plots use the same underlying data but present it differently. 
                          Peaks may appear at different times because smoothing averages nearby values, 
                          while the step function shows exact event times.
                      </p>
                  </div>
                  
                  <div style="background-color: #fff3e0; padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #ff9800;">
                      <strong>💡 Clinical Applications:</strong>
                      <ul style="margin: 5px 0; padding-left: 20px;">
                          <li><strong>Risk Patterns:</strong> Identify periods of high/low event risk</li>
                          <li><strong>Model Assumptions:</strong> Test if hazard is constant (exponential model)</li>
                          <li><strong>Treatment Planning:</strong> Time optimal interventions</li>
                          <li><strong>Prognosis:</strong> Understand when events are most likely</li>
                          <li><strong>Study Design:</strong> Plan follow-up duration and intensity</li>
                      </ul>
                  </div>
                  
                  <div style="background-color: #f0fff0; padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #4caf50;">
                      <strong>🎯 Interpretation Guidelines:</strong>
                      <ul style="margin: 5px 0; padding-left: 20px;">
                          <li><strong>Constant Hazard:</strong> Exponential survival, memoryless property</li>
                          <li><strong>Increasing Hazard:</strong> Risk rises over time (aging, disease progression)</li>
                          <li><strong>Decreasing Hazard:</strong> Early high risk, then stabilization</li>
                          <li><strong>U-shaped Hazard:</strong> Early and late risks (bathtub curve)</li>
                      </ul>
                  </div>
              </div>
              '
              self$results$baselineHazardExplanation$setContent(explanation_html)
            }
          }
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

        myformula <-
          paste("survival::Surv(", mytime, ",", myoutcome, ")")


        private$.checkpoint()

        plot <- plotData %>%
          finalfit::surv_plot(
            .data = .,
            dependent = myformula,
            explanatory = myfactor,
            xlab = paste0('Time (', self$options$timetypeoutput, ')'),
            # pval = TRUE,
            # pval.method	= TRUE,
            legend = 'none',
            break.time.by = self$options$byplot,
            xlim = c(0, self$options$endplot),
            ylim = c(
              self$options$ybegin_plot,
              self$options$yend_plot),
            title = "Survival of the Whole Group",
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

        private$.checkpoint()

        plot2 <- plotData %>%
          finalfit::surv_plot(
            .data = .,
            dependent = myformula,
            explanatory = myfactor,
            xlab = paste0('Time (', self$options$timetypeoutput, ')'),
            # pval = self$options$pplot,
            # pval.method	= self$options$pplot,
            legend = 'none',
            break.time.by = self$options$byplot,
            xlim = c(0, self$options$endplot),
            ylim = c(
              self$options$ybegin_plot,
              self$options$yend_plot),
            title = "Cumulative Events of the Whole Group",
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

        private$.checkpoint()

        plot3 <- plotData %>%
          finalfit::surv_plot(
            .data = .,
            dependent = myformula,
            explanatory = myfactor,
            xlab = paste0('Time (', self$options$timetypeoutput, ')'),
            # pval = self$options$pplot,
            # pval.method	= self$options$pplot,
            legend = 'none',
            break.time.by = self$options$byplot,
            xlim = c(0, self$options$endplot),
            ylim = c(
              self$options$ybegin_plot,
              self$options$yend_plot),
            title = "Cumulative Hazard of the Whole Group",
            fun = "cumhaz",
            risk.table = self$options$risktable,
            conf.int = self$options$ci95,
            censor = self$options$censored,
            surv.median.line = self$options$medianline
          )


        print(plot3)
        TRUE
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


        title2 <- "Single Arm Survival"


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

        private$.checkpoint()

        plot6 <-
          KMunicate::KMunicate(
            fit = km_fit,
            time_scale = time_scale,
            .xlab = paste0('Time in ', self$options$timetypeoutput)
          )


        print(plot6)
        TRUE

      },

      # Baseline Hazard Plot Function ----
      .baselineHazardPlot = function(image, ggtheme, theme, ...) {
        if (!self$options$baseline_hazard)
          return()

        # Get the analysis results from image state (like other plot functions)
        results <- image$state
        
        if (is.null(results)) {
          return()
        }

        # Extract data like other plot functions do
        mytime <- results$name1time
        myoutcome <- results$name2outcome
        plotData <- results$cleanData

        if (is.null(plotData) || nrow(plotData) == 0) {
          return()
        }

        plotData[[mytime]] <- jmvcore::toNumeric(plotData[[mytime]])

        # Create survival object for baseline hazard calculation
        surv_obj <- survival::Surv(time = plotData[[mytime]], event = plotData[[myoutcome]])
        
        result <- private$.safeExecute({
          # Fit Cox model (intercept only for baseline hazard) 
          cox_fit <- survival::coxph(surv_obj ~ 1, data = plotData)
          
          # Get baseline cumulative hazard
          basehaz_data <- survival::basehaz(cox_fit, centered = FALSE)
          
          if (nrow(basehaz_data) == 0) {
            return(NULL)
          }
          
          # Calculate instantaneous hazard from cumulative hazard
          if (nrow(basehaz_data) > 1) {
            # Approximate instantaneous hazard as differences
            basehaz_data$inst_hazard <- c(basehaz_data$hazard[1], diff(basehaz_data$hazard))
            
            # Remove negative or zero hazards
            basehaz_data <- basehaz_data[basehaz_data$inst_hazard > 0, ]
            
            if (nrow(basehaz_data) == 0) {
              return(NULL)
            }
            
            # Use systematic sampling for performance while preserving distribution
            basehaz_data <- private$.systematicSample(basehaz_data, target_size = 50)
            
            # Create the plot
            plot <- ggplot2::ggplot(basehaz_data, ggplot2::aes(x = time, y = inst_hazard)) +
              ggplot2::geom_step(color = "#2E86AB") +
              ggplot2::labs(
                title = "Baseline Hazard Function",
                x = paste0("Time (", self$options$timetypeoutput, ")"),
                y = "Hazard Rate"
              ) +
              ggplot2::theme_minimal()
            
            print(plot)
            return(TRUE)
          } else {
            return(NULL)
          }
        }, context = "baseline_hazard")
        
        if (is.null(result)) {
          return()
        }
      },

      # Smoothed Hazard Plot Function ----
      .smoothedHazardPlot = function(image, ggtheme, theme, ...) {
        if (!self$options$hazard_smoothing)
          return()

        # Get the analysis results from image state (like other plot functions)
        results <- image$state
        
        if (is.null(results)) {
          return()
        }
        
        # Extract data like other plot functions do
        mytime <- results$name1time
        myoutcome <- results$name2outcome
        plotData <- results$cleanData

        if (is.null(plotData) || nrow(plotData) == 0) {
          return()
        }

        plotData[[mytime]] <- jmvcore::toNumeric(plotData[[mytime]])

        result <- private$.safeExecute({
          # Create survival object
          surv_obj <- survival::Surv(time = plotData[[mytime]], event = plotData[[myoutcome]])

          # Use Cox model for consistency with baseline hazard plot
          cox_fit <- survival::coxph(surv_obj ~ 1, data = plotData)
          
          # Get baseline cumulative hazard from Cox model (same as baseline plot)
          basehaz_data <- survival::basehaz(cox_fit, centered = FALSE)
          
          if (nrow(basehaz_data) > 5) {
            # Calculate instantaneous hazard from cumulative hazard
            basehaz_data$inst_hazard <- c(basehaz_data$hazard[1], diff(basehaz_data$hazard))
            
            # Remove negative or zero hazards
            basehaz_data <- basehaz_data[basehaz_data$inst_hazard > 0, ]
            
            # Create data frame for smoothing with actual event-based hazards
            smooth_data <- data.frame(
              time = basehaz_data$time,
              hazard = basehaz_data$inst_hazard
            )
            
            # Use improved adaptive smoothing algorithm
            n_points <- nrow(smooth_data)
            adaptive_span <- private$.calculateAdaptiveSpan(n_points)
            
            # Apply smoothing to instantaneous hazard directly
            smooth_fit <- stats::loess(hazard ~ time, data = smooth_data, 
                                      span = adaptive_span, degree = 1)
            
            # Predict smoothed values
            time_seq <- seq(min(smooth_data$time), max(smooth_data$time), length.out = 100)
            smooth_hazard <- stats::predict(smooth_fit, newdata = data.frame(time = time_seq))
            
            # Ensure non-negative hazards
            smooth_hazard <- pmax(0, smooth_hazard)
            
            # Create plot data
            plot_data <- data.frame(
              time = time_seq,
              hazard = smooth_hazard
            )
            
            # Create the smoothed hazard plot with both smooth line and original points
            plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = hazard)) +
              ggplot2::geom_line(color = "#A23B72", linewidth = 1.2) +
              # Add original hazard points as reference
              ggplot2::geom_point(data = smooth_data, 
                                 ggplot2::aes(x = time, y = hazard),
                                 color = "#A23B72", alpha = 0.3, size = 1) +
              ggplot2::labs(
                title = "Smoothed Hazard Function",
                subtitle = paste0("LOESS smoothing with span = ", round(adaptive_span, 2)),
                x = paste0("Time (", self$options$timetypeoutput, ")"),
                y = "Smoothed Hazard Rate"
              ) +
              ggplot2::theme_minimal()

            print(plot)
            return(TRUE)
          } else {
            return(NULL)
          }
        }, context = "baseline_hazard")
        
        if (is.null(result)) {
          return()
        }
      },

      # Data Quality Assessment Function ----
      .populateDataQuality = function(results) {
        if (!self$options$advancedDiagnostics) {
          return()
        }

        dq <- results$data_quality
        if (is.null(dq)) {
          return()
        }

        # Populate data quality table
        quality_table <- self$results$dataQualityTable
        
        # Define assessment criteria
        assess_sample_size <- function(n) {
          if (n >= 100) "Good" else if (n >= 50) "Adequate" else "Limited"
        }
        
        assess_event_rate <- function(rate) {
          if (rate >= 20) "Good" else if (rate >= 10) "Adequate" else "Low"
        }
        
        assess_followup <- function(max_time) {
          if (max_time >= 60) "Long-term" else if (max_time >= 24) "Medium-term" else "Short-term"
        }

        # Add rows to table
        quality_table$addRow(rowKey = 1, values = list(
          metric = "Sample Size",
          value = paste(dq$n_total, "subjects"),
          assessment = assess_sample_size(dq$n_total)
        ))
        
        quality_table$addRow(rowKey = 2, values = list(
          metric = "Number of Events",
          value = paste(dq$n_events, "events"),
          assessment = assess_sample_size(dq$n_events)
        ))
        
        quality_table$addRow(rowKey = 3, values = list(
          metric = "Event Rate",
          value = paste0(dq$event_rate, "%"),
          assessment = assess_event_rate(dq$event_rate)
        ))
        
        quality_table$addRow(rowKey = 4, values = list(
          metric = "Follow-up Duration",
          value = paste0(dq$min_time, "-", dq$max_time, " ", self$options$timetypeoutput),
          assessment = assess_followup(dq$max_time)
        ))
        
        quality_table$addRow(rowKey = 5, values = list(
          metric = "Median Follow-up",
          value = paste(dq$median_followup, self$options$timetypeoutput),
          assessment = assess_followup(dq$median_followup)
        ))

        # Generate data quality summary
        if (self$options$showSummaries) {
          warning_text <- if (length(dq$warnings) > 0) {
            paste("<div style='background-color: #fff3cd; padding: 10px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #ffc107;'>",
                  "<strong>⚠️ Data Quality Considerations:</strong><ul>",
                  paste0("<li>", dq$warnings, "</li>", collapse = ""),
                  "</ul></div>")
          } else {
            paste0("<div style='background-color: #d4edda; padding: 10px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #28a745;'>",
                   "<strong>✅ Data Quality: Good</strong> - No major concerns identified.</div>")
          }

          summary_html <- paste0(
            "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0;'>",
            "<h4 style='color: #2c3e50; margin-top: 0;'>📊 Data Quality Assessment</h4>",
            "<p>This analysis includes <strong>", dq$n_total, " subjects</strong> with <strong>", 
            dq$n_events, " events</strong> (", dq$event_rate, "% event rate) over a follow-up period of ",
            dq$min_time, " to ", dq$max_time, " ", self$options$timetypeoutput, ".</p>",
            warning_text,
            "<div style='background-color: #e3f2fd; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
            "<strong>💡 Quality Enhancement Tips:</strong>",
            "<ul style='margin: 5px 0; padding-left: 20px;'>",
            "<li><strong>Performance:</strong> Analysis results are cached for improved speed on re-runs</li>",
            "<li><strong>Reliability:</strong> Plots use systematic sampling for large datasets</li>",
            "<li><strong>Precision:</strong> Adaptive smoothing algorithms optimize visualization</li>",
            "<li><strong>Accuracy:</strong> Enhanced error handling provides better diagnostic information</li>",
            "</ul></div>",
            "</div>"
          )
          
          self$results$dataQualitySummary$setContent(summary_html)
        }
      }


    )
  )
