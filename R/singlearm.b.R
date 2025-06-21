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
    private = list(


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


          # # Define a mapping from timetypedata to lubridate functions
          # lubridate_functions <- list(
          #     ymdhms = lubridate::ymd_hms,
          #     ymd = lubridate::ymd,
          #     ydm = lubridate::ydm,
          #     mdy = lubridate::mdy,
          #     myd = lubridate::myd,
          #     dmy = lubridate::dmy,
          #     dym = lubridate::dym
          # )
          # # Apply the appropriate lubridate function based on timetypedata
          # if (timetypedata %in% names(lubridate_functions)) {
          #     func <- lubridate_functions[[timetypedata]]
          #     mydata[["start"]] <- func(mydata[[dxdate]])
          #     mydata[["end"]] <- func(mydata[[fudate]])
          # }


          if (timetypedata == "ymdhms") {
            mydata[["start"]] <- lubridate::ymd_hms(mydata[[dxdate]])
            mydata[["end"]] <-
              lubridate::ymd_hms(mydata[[fudate]])
          }
          if (timetypedata == "ymd") {
            mydata[["start"]] <- lubridate::ymd(mydata[[dxdate]])
            mydata[["end"]] <-
              lubridate::ymd(mydata[[fudate]])
          }
          if (timetypedata == "ydm") {
            mydata[["start"]] <- lubridate::ydm(mydata[[dxdate]])
            mydata[["end"]] <-
              lubridate::ydm(mydata[[fudate]])
          }
          if (timetypedata == "mdy") {
            mydata[["start"]] <- lubridate::mdy(mydata[[dxdate]])
            mydata[["end"]] <-
              lubridate::mdy(mydata[[fudate]])
          }
          if (timetypedata == "myd") {
            mydata[["start"]] <- lubridate::myd(mydata[[dxdate]])
            mydata[["end"]] <-
              lubridate::myd(mydata[[fudate]])
          }
          if (timetypedata == "dmy") {
            mydata[["start"]] <- lubridate::dmy(mydata[[dxdate]])
            mydata[["end"]] <-
              lubridate::dmy(mydata[[fudate]])
          }
          if (timetypedata == "dym") {
            mydata[["start"]] <- lubridate::dym(mydata[[dxdate]])
            mydata[["end"]] <-
              lubridate::dym(mydata[[fudate]])
          }



          if ( sum(!is.na(mydata[["start"]])) == 0 || sum(!is.na(mydata[["end"]])) == 0)  {
            stop(paste0("Time difference cannot be calculated. Make sure that time type in variables are correct. Currently it is: ", self$options$timetypedata)
            )
          }


          timetypeoutput <-
            jmvcore::constructFormula(terms = self$options$timetypeoutput)


          mydata <- mydata %>%
            dplyr::mutate(interval = lubridate::interval(start, end))


          # self$results$mydataview$setContent(
          #     list(
          #       "mydata" = head(mydata),
          #       "start" = sum(!is.na(mydata[["start"]])),
          #       "end" = sum(!is.na(mydata[["end"]]))
          #     )
          # )

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
            if (!((length(unique(
              outcome1[!is.na(outcome1)]
            )) == 2) && (sum(unique(
              outcome1[!is.na(outcome1)]
            )) == 1))) {
              stop(
                'When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.'
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

        # Errors, Warnings ----

        ## No variable todo ----

        ### Define subconditions ----

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

        condition1 <- subcondition1a && !subcondition1b1 || subcondition1b1 && subcondition1b2 || subcondition1b1 && subcondition1b3

        condition2 <- subcondition2b1 && subcondition2b2 && subcondition2b3 || subcondition2a && !subcondition2b1 && !subcondition2b2 && !subcondition2b3


        not_continue_analysis <- !(condition1 && condition2)

        if (not_continue_analysis) {
          private$.todo()
          self$results$medianSummary$setVisible(FALSE)
          self$results$medianTable$setVisible(FALSE)
          self$results$survTableSummary$setVisible(FALSE)
          self$results$survTable$setVisible(FALSE)
          self$results$plot$setVisible(FALSE)
          self$results$plot2$setVisible(FALSE)
          self$results$plot3$setVisible(FALSE)
          self$results$plot6$setVisible(FALSE)
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




        ## Add Calculated Time to Data ----

        # self$results$mydataview$setContent(
        #     list(
        #         head(results$cleanData)
        #     )
        # )


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

        km_fit <- survival::survfit(formula, data = mydata)

        km_fit_median_df <- summary(km_fit)


        # medianSummary2 <-
        #   as.data.frame(km_fit_median_df$table)
        # self$results$medianSummary2$setContent(medianSummary2)



        results1html <-
          as.data.frame(km_fit_median_df$table) %>%
          t() %>%
          as.data.frame() %>%
          janitor::clean_names(dat = ., case = "snake")

        results1table <- results1html

        # self$results$medianSummary2$setContent(results1table)

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


        medianSummary <- c(km_fit_median_definition,
                           "The median survival time is when 50% of subjects have experienced the event.",
                           "This means that 50% of subjects in this group survived longer than this time period.",
                           "Note: Confidence intervals are calculated using the log-log transformation method for improved accuracy with censored data (not plain Greenwood formula)."
        )


        self$results$medianSummary$setContent(medianSummary)


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

        km_fit <- survival::survfit(formula, data = mydata)

        utimes <- self$options$cutp

        utimes <- strsplit(utimes, ",")
        utimes <- purrr::reduce(utimes, as.vector)
        utimes <- as.numeric(utimes)

        if (length(utimes) == 0) {
          utimes <- c(12, 36, 60)
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

        # self$results$tableview$setContent(km_fit_df)


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
                "{time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]. \n The estimated probability of surviving beyond {time} months was {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]. \n At this time point, there were {n.risk} subjects still at risk and {n.event} events had occurred in this group."
              )
          ) %>%
          dplyr::select(description) %>%
          dplyr::pull(.) -> survTableSummary



        self$results$survTableSummary$setContent(survTableSummary)


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

      }


    )
  )
