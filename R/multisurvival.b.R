#' @title Multivariable Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'

multisurvivalClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "multisurvivalClass",
    inherit = multisurvivalBase,
    private = list(

      .nom_object = NULL,


      # init ----
      .init = function() {
        explanatory_len <- length(self$options$explanatory)
        contexpl_len <- length(self$options$contexpl)

        if (explanatory_len > 0 || contexpl_len > 0) {
          self$results$plot8$setSize((explanatory_len + contexpl_len) * 400,
                                     (explanatory_len + contexpl_len) * 300)
        } else {
          self$results$plot8$setVisible(FALSE)
        }




      }

      # getData ----
      ,
      .getData = function() {
        # Check if data exists and has content
        if (is.null(self$data) || nrow(self$data) == 0) {
          stop('Data contains no (complete) rows')
        }

        # Get the data
        mydata <- self$data


        # Check if data has names
        if (is.null(names(mydata))) {
          stop('Data must have column names')
        }

        # Add row names if missing
        if (is.null(rownames(mydata))) {
          mydata$row_names <- seq_len(nrow(mydata))
        } else {
          mydata$row_names <- rownames(mydata)
        }

        # Get original names
        original_names <- names(mydata)

        # Check if original names exist
        if (length(original_names) == 0) {
          stop('Data must have column names')
        }

        # Create labels vector
        labels <- stats::setNames(original_names, original_names)

        # Clean names safely
        mydata_cleaned <- try({
          janitor::clean_names(mydata)
        }, silent = TRUE)

        # mydata <- mydata %>% janitor::clean_names()


        if (inherits(mydata_cleaned, "try-error")) {
          stop('Error cleaning variable names. Please check column names.')
        }


        # Create corrected labels
        corrected_labels <- stats::setNames(original_names, names(mydata_cleaned))

        # Apply labels
        mydata_labelled <- try({
          labelled::set_variable_labels(.data = mydata_cleaned, .labels = corrected_labels)
        }, silent = TRUE)

        # mydata <- labelled::set_variable_labels(
        #     .data = mydata,
        #     .labels = corrected_labels
        # )


        if (inherits(mydata_labelled, "try-error")) {
          stop('Error setting variable labels')
        }


        # Get all labels
        all_labels <- labelled::var_label(mydata_labelled)

        # all_labels <- labelled::var_label(mydata)


        # Get variable names from labels
        mytime <- try({
          names(all_labels)[all_labels == self$options$elapsedtime]
        }, silent = TRUE)

        # mytime <-
        #     names(all_labels)[all_labels == self$options$elapsedtime]

        myoutcome <- try({
          names(all_labels)[all_labels == self$options$outcome]
        }, silent = TRUE)

        # myoutcome <-
        #     names(all_labels)[all_labels == self$options$outcome]


        mydxdate <- try({
          names(all_labels)[all_labels == self$options$dxdate]
        }, silent = TRUE)

        # mydxdate <-
        #     names(all_labels)[all_labels == self$options$dxdate]


        myfudate <- try({
          names(all_labels)[all_labels == self$options$fudate]
        }, silent = TRUE)

        # myfudate <-
        #     names(all_labels)[all_labels == self$options$fudate]



        labels_explanatory <- self$options$explanatory

        myexplanatory <-
          names(all_labels)[match(labels_explanatory, all_labels)]

        labels_contexpl <- self$options$contexpl

        mycontexpl <-
          names(all_labels)[match(labels_contexpl, all_labels)]


        # Get adjexplanatory only if it exists and ac option is TRUE
        adjexplanatory <- NULL
        if (!is.null(self$options$adjexplanatory) &&
            self$options$ac) {
          adjexplanatory <- names(all_labels)[all_labels == self$options$adjexplanatory]
        }


        mystratvar_labelled <- NULL


        if (self$options$use_stratify && !is.null(self$options$stratvar)) {
        # Add this to get stratification variables
        labels_stratvar <- self$options$stratvar
        mystratvar_labelled <- names(all_labels)[match(labels_stratvar, all_labels)]
        }


        # Check if required variables were found
        if (length(mytime) == 0 &&
            !is.null(self$options$elapsedtime)) {
          stop('Could not find elapsed time variable')
        }
        if (length(myoutcome) == 0 &&
            !is.null(self$options$outcome)) {
          stop('Could not find outcome variable')
        }

        # Return results
        return(
          list(
            "mydata_labelled" = mydata_labelled,
            "mytime_labelled" = mytime,
            "myoutcome_labelled" = myoutcome,
            "mydxdate_labelled" = mydxdate,
            "myfudate_labelled" = myfudate,
            "mycontexpl_labelled" = mycontexpl,
            "myexplanatory_labelled" = myexplanatory,
            "adjexplanatory_labelled" = adjexplanatory,
            "mystratvar_labelled" = mystratvar_labelled


          )
        )



      }

      # todo ----
      ,
      .todo = function() {
        # todo ----

        todo <- glue::glue(
          "
                    <br>Welcome to ClinicoPath
                    <br><br>
                        This tool will help you perform a multivariable survival analysis.
                    <br><br>
                        Explanatory variables can be categorical (ordinal or nominal) or continuous.
                    <br><br>
                    Select outcome level from Outcome variable.
                    <br><br>
                    Outcome Level: if patient is dead or event (recurrence) occured. You may also use advanced outcome options depending on your analysis type.
                    <br><br>
                        Survival time should be numeric, continuous, and in months. You may also use dates to calculate survival time in advanced elapsed time options.
                    <br><br>


        Stratification Variables: Use these when the proportional hazards assumption
        is violated for certain variables. The model will create separate baseline
        hazard functions for each level of the stratification variables, but won't
        estimate their direct effects.
        <br><br>
        Consider using stratification when:
        <br>- A variable fails the proportional hazards test
        <br>- You need to control for a variable's effect but don't need to
        estimate its hazard ratio
        <br>- There are natural differences in baseline risk across groups

<br><br>
                        This function uses finalfit, survival, survminer and ggstatsplot packages. Please cite jamovi and the packages as given below.
                    <br><br>
                    "
        )
        # https://finalfit.org/articles/all_tables_examples.html#cox-proportional-hazards-model-survival-time-to-event


        html <- self$results$todo
        html$setContent(todo)
        return()

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


          if (sum(!is.na(mydata[["start"]])) == 0 ||
              sum(!is.na(mydata[["end"]])) == 0)  {
            stop(
              paste0(
                "Time difference cannot be calculated. Make sure that time type in variables are correct. Currently it is: ",
                self$options$timetypedata
              )
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
            if (!((length(unique(outcome1[!is.na(outcome1)])) == 2) &&
                  (sum(unique(outcome1[!is.na(outcome1)])) == 1))) {
              stop(
                'When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.'
              )

            }

            mydata[["myoutcome"]] <- mydata[[myoutcome_labelled]]
            # mydata[[self$options$outcome]]

          } else if (inherits(outcome1, "factor")) {
            mydata[["myoutcome"]] <-
              ifelse(test = outcome1 == outcomeLevel,
                     yes = 1,
                     no = 0)

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
        myexplanatory_labelled <- labelled_data$myexplanatory_labelled
        mycontexpl_labelled <- labelled_data$mycontexpl_labelled
        adjexplanatory_labelled <- labelled_data$adjexplanatory_labelled

        mydata <- mydata_labelled

        df_factor <- mydata %>%
          jmvcore::select(unique(
            c(
              "row_names",
              myexplanatory_labelled,
              adjexplanatory_labelled,
              mycontexpl_labelled
            )
          ))

        return(df_factor)

      }

      # Clean Data ----
      ,
      .cleandata = function() {
        ## Common Definitions ----

        contin <- c("integer", "numeric", "double")

        ## Read Data ----

        labelled_data <- private$.getData()

        mydata_labelled        <- labelled_data$mydata_labelled
        mytime_labelled        <- labelled_data$mytime_labelled
        myoutcome_labelled     <- labelled_data$myoutcome_labelled
        mydxdate_labelled      <- labelled_data$mydxdate_labelled
        myfudate_labelled      <- labelled_data$myfudate_labelled
        myexplanatory_labelled <- labelled_data$myexplanatory_labelled
        mycontexpl_labelled    <- labelled_data$mycontexpl_labelled
        adjexplanatory_labelled <- labelled_data$adjexplanatory_labelled
        mystratvar_labelled <- labelled_data$mystratvar_labelled

        time <- private$.definemytime()
        outcome <- private$.definemyoutcome()
        factor <- private$.definemyfactor()

        ## Clean Data ----
        cleanData <- dplyr::left_join(time, outcome, by = "row_names") %>%
          dplyr::left_join(factor, by = "row_names")

        ## Landmark ----

        # https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#landmark_method

        if (self$options$uselandmark) {
          landmark <- jmvcore::toNumeric(self$options$landmark)

          cleanData <- cleanData %>%
            dplyr::filter(mytime >= landmark) %>%
            dplyr::mutate(mytime = mytime - landmark)
        }

        ## Names cleanData ----

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

        name3expl <- NULL

        if (!is.null(self$options$explanatory)) {
          name3expl <- myexplanatory_labelled
        }


        name3contexpl <- NULL

        if (!is.null(self$options$contexpl)) {
          name3contexpl <- mycontexpl_labelled
        }

        # Add adjexplanatory name if present
        adjexplanatory_name <- NULL
        if (!is.null(adjexplanatory_labelled)) {
          adjexplanatory_name <- adjexplanatory_labelled
        }


        # naOmit ----

        cleanData <- jmvcore::naOmit(cleanData)




        ## Add Calculated Time to Data ----

        if (self$options$tint &&
            self$options$calculatedtime &&
            self$results$calculatedtime$isNotFilled()) {
          self$results$calculatedtime$setRowNums(cleanData$row_names)
          self$results$calculatedtime$setValues(cleanData$mytime)
        }




        ## Add Redefined Outcome to Data ----

        if (self$options$multievent  &&
            self$options$outcomeredefined &&
            self$results$outcomeredefined$isNotFilled()) {
          self$results$outcomeredefined$setRowNums(cleanData$row_names)
          self$results$outcomeredefined$setValues(cleanData$myoutcome)
        }


        # self$results$mydataview$setContent(
        #   list(
        #     "name1time" = name1time,
        #     "name2outcome" = name2outcome,
        #     "name3contexpl" = name3contexpl,
        #     "name3expl" = name3expl,
        #     "adjexplanatory_name" = adjexplanatory_name,
        #
        #     "cleanData" = cleanData,
        #     "mytime_labelled" = mytime_labelled,
        #     "myoutcome_labelled" = myoutcome_labelled,
        #     "mydxdate_labelled" = mydxdate_labelled,
        #     "myfudate_labelled" = myfudate_labelled,
        #     "myexplanatory_labelled" = myexplanatory_labelled,
        #     "mycontexpl_labelled" = mycontexpl_labelled,
        #     "adjexplanatory_labelled" = adjexplanatory_labelled
        #
        #   )
        # )



        # Return Data ----

        return(
          list(
            "name1time" = name1time,
            "name2outcome" = name2outcome,
            "name3contexpl" = name3contexpl,
            "name3expl" = name3expl,
            "adjexplanatory_name" = adjexplanatory_name,

            "cleanData" = cleanData,
            "mydata_labelled" = mydata_labelled,
            "mytime_labelled" = mytime_labelled,
            "myoutcome_labelled" = myoutcome_labelled,
            "mydxdate_labelled" = mydxdate_labelled,
            "myfudate_labelled" = myfudate_labelled,
            "myexplanatory_labelled" = myexplanatory_labelled,
            "mycontexpl_labelled" = mycontexpl_labelled,
            "adjexplanatory_labelled" = adjexplanatory_labelled,
            "mystratvar_labelled" = mystratvar_labelled

          )
        )

      }



      # run  ----
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
        condition3a <- !is.null(self$options$contexpl)
        condition3b <- !is.null(self$options$explanatory)

        condition1 <- subcondition1a &&
          !subcondition1b1 ||
          subcondition1b1 &&
          subcondition1b2 ||
          subcondition1b1 && subcondition1b3

        condition2 <- subcondition2b1 &&
          subcondition2b2 &&
          subcondition2b3 ||
          subcondition2a &&
          !subcondition2b1 &&
          !subcondition2b2 && !subcondition2b3


        condition3 <- condition3a || condition3b

        not_continue_analysis <- !(condition1 &&
                                     condition2 &&
                                     condition3)


        if (not_continue_analysis) {
          private$.todo()
          self$results$text$setVisible(FALSE)
          self$results$text2$setVisible(FALSE)
          self$results$plot$setVisible(FALSE)
          self$results$plot3$setVisible(FALSE)
          self$results$plot8$setVisible(FALSE)
          self$results$todo$setVisible(TRUE)
          return()
        } else {
          self$results$todo$setVisible(FALSE)
        }


        ## Stop if Empty Data ----

        if (nrow(self$data) == 0)
          stop('Data contains no (complete) rows')

        ## mydata ----

        private$.checkpoint()

        cleaneddata <- private$.cleandata()

        name1time <- cleaneddata$name1time
        name2outcome <- cleaneddata$name2outcome
        name3contexpl <- cleaneddata$name3contexpl
        name3expl <- cleaneddata$name3expl
        adjexplanatory_name <- cleaneddata$adjexplanatory_name

        mydata <- cleanData <- cleaneddata$cleanData

        mytime_labelled <- cleaneddata$mytime_labelled
        myoutcome_labelled <- cleaneddata$myoutcome_labelled
        mydxdate_labelled <- cleaneddata$mydxdate_labelled
        myfudate_labelled <- cleaneddata$myfudate_labelled
        myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
        mycontexpl_labelled <- cleaneddata$mycontexpl_labelled
        adjexplanatory_labelled <- cleaneddata$adjexplanatory_labelled
        mystratvar_labelled <- cleaneddata$mystratvar_labelled



        ## run Cox function ----

        private$.checkpoint()

        private$.final_fit()


        ## generate cox model ----

        if (self$options$ph_cox ||
            self$options$calculateRiskScore ||
            self$options$ac || self$options$showNomogram) {
          private$.checkpoint()
          cox_model <- private$.cox_model()
        }

        ## run coxph ----

        if (self$options$ph_cox) {
          private$.cox_ph(cox_model)
        }


        ## Calculate Risk Score ----

        if (self$options$calculateRiskScore) {
          private$.checkpoint()
          riskData <- private$.calculateRiskScore(cox_model, mydata)

        }


        ## Compare models ----

        # if (self$options$compare_models) {
        #   private$.compare_models()
        # }


        ## Adjusted survival ----

        # if (self$options$ac) {
        #   private$.calculateAdjustedStats()
        #   }



        ## run Cox function .fitModelWithSelection ----

        if (self$options$use_modelSelection) {
          private$.checkpoint()
          private$.final_fit2()
        }



        ## stratification ----
        if (self$options$use_stratify) {
          private$.checkpoint()
          stratify_explanation <- glue::glue("
    <h4>Understanding Stratification in Your Survival Analysis</h4>

    <p>You have chosen to stratify your analysis by: <b>{paste(self$options$stratvar, collapse=', ')}</b></p>

    <h5>What This Means:</h5>
    <ul>
        <li>The model creates separate baseline hazard functions for each level of {paste(self$options$stratvar, collapse=' and ')}</li>
        <li>Hazard ratios are not calculated for stratified variables because they are used to define different baseline hazards</li>
        <li>The effects of other variables are estimated while accounting for these different baseline hazards</li>
    </ul>

    <h5>Interpretation:</h5>
    <ul>
        <li>The hazard ratios shown in the results are adjusted for the stratification</li>
        <li>You can think of this as pooling the effects of your other variables across different strata</li>
        <li>This approach is particularly useful when the effect of a variable changes over time</li>
    </ul>

    <h5>When This is Useful:</h5>
    <ul>
        <li>When variables violate the proportional hazards assumption</li>
        <li>When baseline hazards differ substantially between groups</li>
        <li>When you need to control for a variable but don't need its hazard ratio</li>
    </ul>
")

          self$results$stratificationExplanation$setContent(stratify_explanation)
        }





        ## specific details ph_cox startify ----
        if (self$options$ph_cox && self$options$use_stratify) {
          private$.checkpoint()

          zph <- survival::cox.zph(cox_model)

          # Check if stratification was appropriate
          violations <- which(zph$table[,"p"] < 0.05)
          violation_vars <- rownames(zph$table)[violations]

          additional_info <- ""
          if (length(intersect(mystratvar_labelled, violation_vars)) > 0) {
            additional_info <- glue::glue("
            <h5>Stratification Assessment:</h5>
            <p>Your choice to stratify was supported by the proportional hazards test results.
            The stratified variables showed violations of the proportional hazards assumption.</p>
        ")
          }

          self$results$stratificationExplanation$setContent(
            paste(stratify_explanation, additional_info)
          )
        }



        ## Add the person-time analysis ----
        private$.checkpoint()  # Add checkpoint here

        # Run person-time analysis if enabled
        if (self$options$person_time) {
          private$.personTimeAnalysis()

        }



        ## Nomogram ----

        if (self$options$showNomogram) {
          private$.checkpoint()
          private$.nomogram(cox_model)
        }

        ## Survival Decision Tree ----
        
        if (self$options$use_tree) {
          private$.checkpoint()
          tree <- private$.survivalTree(cleaneddata)
          
          # Prepare data for tree plots
          if (!is.null(tree)) {
            image_tree <- self$results$tree_plot
            image_tree$setState(list(
              "results" = cleaneddata,
              "tree" = tree
            ))
            
            if (self$options$show_terminal_nodes) {
              image_node_survival <- self$results$node_survival_plots
              image_node_survival$setState(list(
                "results" = cleaneddata,
                "tree" = tree
              ))
            }
          }
        }

        # Prepare Data For Plots ----

        image <- self$results$plot
        image$setState(cleaneddata)

        image3 <- self$results$plot3
        image3$setState(cleaneddata)


        # image4 <- self$results$plot4
        # image4$setState(cleaneddata)

        imageKM <- self$results$plotKM
        imageKM$setState(cleaneddata)

        # image7 <- self$results$plot7
        # image7$setState(cleaneddata)


        image_plot_adj <- self$results$plot_adj
        image_plot_adj$setState(cleaneddata)




        if (self$options$calculateRiskScore) {
          image_riskGroupPlot <- self$results$riskGroupPlot
          image_riskGroupPlot$setState(riskData)

        }

        # View plot data
        #         if (self$options$ac) {
        # self$results$mydataview_plot_adj$setContent(
        #   list(
        #     cox_model = cox_model,
        #     mydata = mydata,
        #     adjexplanatory_name = adjexplanatory_labelled
        #   ))
        #         }




      }



      # finalfit  ----
      ,
      .final_fit = function() {
        cleaneddata <- private$.cleandata()

        name1time <- cleaneddata$name1time
        name2outcome <- cleaneddata$name2outcome
        name3contexpl <- cleaneddata$name3contexpl
        name3expl <- cleaneddata$name3expl
        adjexplanatory_name <- cleaneddata$adjexplanatory_name

        mydata <- cleanData <- cleaneddata$cleanData

        mytime_labelled <- cleaneddata$mytime_labelled
        myoutcome_labelled <- cleaneddata$myoutcome_labelled
        mydxdate_labelled <- cleaneddata$mydxdate_labelled
        myfudate_labelled <- cleaneddata$myfudate_labelled
        myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
        mycontexpl_labelled <- cleaneddata$mycontexpl_labelled
        adjexplanatory_labelled <- cleaneddata$adjexplanatory_labelled
        mystratvar_labelled <- cleaneddata$mystratvar_labelled


        ## prepare formula ----

        myexplanatory <- NULL

        if (!is.null(self$options$explanatory)) {
          myexplanatory <- as.vector(myexplanatory_labelled)
        }

        mycontexpl <- NULL
        if (!is.null(self$options$contexpl)) {
          mycontexpl <- as.vector(mycontexpl_labelled)
        }


        formula2 <- c(myexplanatory, mycontexpl)


        # Remove stratification variables from the finalfit output
        if (self$options$use_stratify && !is.null(self$options$stratvar)) {
          # Remove stratified variables from the display
          formula2 <- formula2[!formula2 %in% mystratvar_labelled]
        }



        myformula <-
          paste("Surv( mytime, myoutcome ) ~ ", paste(formula2, collapse = " + "))

        myformula <- as.formula(myformula)

        # self$results$mydataview_finalfit$setContent(
        #     list(
        #         mydata = head(mydata, n = 30),
        #         myformula = myformula,
        #         myexplanatory = myexplanatory,
        #         mycontexpl = mycontexpl,
        #         mystratvar_labelled = mystratvar_labelled,
        #         formula2 = formula2
        #     )
        # )




        ## finalfit Multivariable table ----

        finalfit::finalfit(.data = mydata,
                           formula = myformula,
                           # dependent = myformula,
                           # explanatory = formula2,

                           metrics = TRUE) -> tMultivariable



        text2 <- glue::glue("
                                    <br>
                                    <b>Model Metrics:</b>
                                    ",
                            unlist(tMultivariable[[2]]),
                            "
                                    <br>
                                    ")

        if (self$options$uselandmark) {
          landmark <- jmvcore::toNumeric(self$options$landmark)

          text2 <- glue::glue(text2,
                              "Landmark time used as: ",
                              landmark,
                              " ",
                              self$options$timetypeoutput,
                              ".")
        }



        # Add note about stratified variables if any
        if (self$options$use_stratify && !is.null(self$options$stratvar)) {
          text2 <- glue::glue(text2,

          "
            <b>Note:</b> This model is stratified by: {paste(self$options$stratvar, collapse=', ')}.
            Hazard ratios are not shown for stratification variables as they are used to create separate baseline hazard functions.
            ")
          }

        self$results$text2$setContent(text2)



        results1 <- knitr::kable(
          tMultivariable[[1]],
          row.names = FALSE,
          align = c('l', 'l', 'r', 'r', 'r', 'r'),
          format = "html"
        )

        self$results$text$setContent(results1)

      }

      # cox model  ----
      ,
      .cox_model = function() {
        cleaneddata <- private$.cleandata()

        name1time <- cleaneddata$name1time
        name2outcome <- cleaneddata$name2outcome
        name3contexpl <- cleaneddata$name3contexpl
        name3expl <- cleaneddata$name3expl
        adjexplanatory_name <- cleaneddata$adjexplanatory_name

        mydata <- cleanData <- cleaneddata$cleanData

        mytime_labelled <- cleaneddata$mytime_labelled
        myoutcome_labelled <- cleaneddata$myoutcome_labelled
        mydxdate_labelled <- cleaneddata$mydxdate_labelled
        myfudate_labelled <- cleaneddata$myfudate_labelled
        myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
        mycontexpl_labelled <- cleaneddata$mycontexpl_labelled
        adjexplanatory_labelled <- cleaneddata$adjexplanatory_labelled
        mystratvar_labelled <- cleaneddata$mystratvar_labelled



        # Add stratification variables
        mystratvar <- NULL

        if (self$options$use_stratify && !is.null(self$options$stratvar)) {
          mystratvar <- as.vector(cleaneddata$mystratvar_labelled)
          if (length(mystratvar) > 0) {
            # FIXED: Each strata variable should be in its own strata() function
            mystratvar <- paste(sprintf("survival::strata(%s)", mystratvar), collapse = " + ")

            # # Only create strata terms if we have variables
            # mystratvar <- paste0("survival::strata(", paste(mystratvar, collapse = "+"), ")")
          }

        }



        myexplanatory <- NULL
        if (!is.null(self$options$explanatory)) {
          myexplanatory <- as.vector(myexplanatory_labelled)
        }

        mycontexpl <- NULL
        if (!is.null(self$options$contexpl)) {
          mycontexpl <- as.vector(mycontexpl_labelled)
        }

        # Build formula parts
        formula_parts <- c(myexplanatory, mycontexpl)

        # Add strata term only if it exists
        if (!is.null(mystratvar) && mystratvar != "") {
          formula_parts <- c(formula_parts, mystratvar)
        }
        # formula2 <- c(myexplanatory, mycontexpl, mystratvar)



        LHT <- "survival::Surv(mytime, myoutcome)"

        # RHT <- formula2

        RHT <- paste(formula_parts, collapse = " + ")

        coxformula <- paste0(LHT, " ~ ", RHT)

        coxformula <- as.formula(coxformula)


        # Remove any rows with NA in stratification variables
        # if (self$options$use_stratify && !is.null(self$options$stratvar)) {
        #   complete_cases <- complete.cases(mydata[, mystratvar])
        #   mydata <- mydata[complete_cases, ]
        # }



        # self$results$mydataview_cox$setContent(
        #   list(
        #     mydata = head(mydata, n = 30),
        #     coxformula = coxformula
        #   )
        # )

        # Add checkpoint before the expensive Cox model fitting
        private$.checkpoint()

        # Get all labels for variable name mapping
        mydata_labelled <- cleaneddata$mydata_labelled
        all_labels <- labelled::var_label(mydata_labelled)

        # Handle Time-Dependent Covariates
        if (self$options$use_time_dependent && !is.null(self$options$time_dep_vars)) {
          
          # Get time-dependent variable names
          time_dep_vars <- names(all_labels)[match(self$options$time_dep_vars, all_labels)]
          
          if (self$options$td_format == "wide") {
            # Handle wide format data - convert to long format
            mydata <- private$.convertWideToLong(mydata, time_dep_vars, all_labels)
            
            # Update formula for time-dependent covariates (long format)
            LHT <- "survival::Surv(tstart, tstop, myoutcome)"
            
            # Add time-dependent variables to formula
            formula_parts <- c(formula_parts, time_dep_vars)
            RHT <- paste(formula_parts, collapse = " + ")
            coxformula <- as.formula(paste0(LHT, " ~ ", RHT))
            
          } else if (self$options$td_format == "long") {
            # Handle long format data
            if (!is.null(self$options$start_time_var) && !is.null(self$options$stop_time_var)) {
              start_time_var <- names(all_labels)[all_labels == self$options$start_time_var]
              stop_time_var <- names(all_labels)[all_labels == self$options$stop_time_var]
              
              # Update formula for time-dependent covariates
              LHT <- paste0("survival::Surv(", start_time_var, ", ", stop_time_var, ", myoutcome)")
              
              # Add time-dependent variables to formula
              formula_parts <- c(formula_parts, time_dep_vars)
              RHT <- paste(formula_parts, collapse = " + ")
              coxformula <- as.formula(paste0(LHT, " ~ ", RHT))
            }
          }
        }

        # Handle Frailty Models
        if (self$options$use_frailty && !is.null(self$options$frailty_var)) {
          frailty_var <- names(all_labels)[all_labels == self$options$frailty_var]
          
          # Add frailty term based on distribution
          frailty_term <- switch(self$options$frailty_distribution,
            "gamma" = paste0("frailty(", frailty_var, ", distribution='gamma')"),
            "gaussian" = paste0("frailty(", frailty_var, ", distribution='gaussian')"),
            "logt" = paste0("frailty(", frailty_var, ", distribution='logt')")
          )
          
          formula_parts <- c(formula_parts, frailty_term)
          RHT <- paste(formula_parts, collapse = " + ")
          coxformula <- as.formula(paste0(LHT, " ~ ", RHT))
        }

        # Handle Splines for Non-Proportional Hazards
        if (self$options$use_splines && !is.null(self$options$spline_vars)) {
          spline_vars <- names(all_labels)[match(self$options$spline_vars, all_labels)]
          
          # Create spline terms
          for (var in spline_vars) {
            spline_term <- switch(self$options$spline_type,
              "pspline" = paste0("pspline(", var, ", df=", self$options$spline_df, ")"),
              "ns" = paste0("ns(", var, ", df=", self$options$spline_df, ")"),
              "bs" = paste0("bs(", var, ", df=", self$options$spline_df, ")")
            )
            
            # Replace the linear term with spline term
            formula_parts <- formula_parts[formula_parts != var]
            formula_parts <- c(formula_parts, spline_term)
          }
          
          RHT <- paste(formula_parts, collapse = " + ")
          coxformula <- as.formula(paste0(LHT, " ~ ", RHT))
          
          # Load splines package if needed
          if (self$options$spline_type %in% c("ns", "bs")) {
            requireNamespace("splines", quietly = TRUE)
          }
        }

        cox_model <- survival::coxph(coxformula, data = mydata)


        return(cox_model)

      }



      ,
      # Person-Time Analysis Function ----
      .personTimeAnalysis = function() {
        # Check if person_time option is enabled
        if (!self$options$person_time) {
          return()
        }

        cleaneddata <- private$.cleandata()


        # Extract data
        # mytime <- cleaneddata$mytime_labelled
        # myoutcome <- cleaneddata$myoutcome_labelled
        mydata <- cleaneddata$cleanData


        # Ensure time is numeric
        mydata[["mytime"]] <- jmvcore::toNumeric(mydata[["mytime"]])

        # Get total observed time
        total_time <- sum(mydata[["mytime"]])

        # Get total events
        total_events <- sum(mydata[["myoutcome"]])

        # Get time unit
        time_unit <- self$options$timetypeoutput

        # Get rate multiplier
        rate_multiplier <- self$options$rate_multiplier

        # Calculate overall incidence rate
        overall_rate <- (total_events / total_time) * rate_multiplier

        # Calculate confidence intervals using Poisson exact method
        ci_lower <- (stats::qchisq(0.025, 2*total_events) / 2) / total_time * rate_multiplier
        ci_upper <- (stats::qchisq(0.975, 2*(total_events + 1)) / 2) / total_time * rate_multiplier



        # self$results$mydataview_personTimeAnalysis$setContent(
        #   list(
        #     mydata = head(mydata, n = 10),
        #     # mytime = mytime,
        #     # myoutcome = myoutcome,
        #     total_time = total_time,
        #     total_events = total_events,
        #     overall_rate = overall_rate,
        #     ci_lower = ci_lower,
        #     ci_upper = ci_upper
        #   )
        # )




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
          breaks <- c(0, time_intervals, max(mydata[["mytime"]]) * 1.1)

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
              follow_up_times <- pmin(mydata[["mytime"]], end_time)
              # Count only events that occurred within this interval
              events_in_interval <- sum(mydata[["myoutcome"]] == 1 & mydata[["mytime"]] <= end_time)
            } else {
              # For later intervals, include only patients who survived past the previous cutpoint
              survivors <- mydata[["mytime"]] > start_time
              interval_data <- mydata[survivors, ]

              if (nrow(interval_data) == 0) {
                # Skip if no patients in this interval
                next
              }

              # Adjust entry time and follow-up time
              adjusted_entry_time <- rep(start_time, nrow(interval_data))
              adjusted_exit_time <- pmin(interval_data[["mytime"]], end_time)
              follow_up_times <- adjusted_exit_time - adjusted_entry_time

              # Count only events that occurred within this interval
              events_in_interval <- sum(interval_data[["myoutcome"]] == 1 &
                                          interval_data[["mytime"]] <= end_time &
                                          interval_data[["mytime"]] > start_time)
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







      ,
      # Nomogram ----

      .nomogram = function(cox_model) {

        if (!self$options$showNomogram) {
          return()
        }

        private$.checkpoint()

        # Get cleaned data
        cleaneddata <- private$.cleandata()
        mydata <- cleaneddata$cleanData
        myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
        mycontexpl_labelled <- cleaneddata$mycontexpl_labelled
        mystratvar_labelled <- cleaneddata$mystratvar_labelled

        # Combine variables
        var_names <- c(myexplanatory_labelled, mycontexpl_labelled)

        # Remove stratification variables if needed
        if (self$options$use_stratify && !is.null(self$options$stratvar)) {
          var_names <- var_names[!var_names %in% mystratvar_labelled]
        }

        # First create datadist object
        dd <- rms::datadist(mydata[, var_names])

        # Handle limits for continuous variables properly
        for(var in var_names) {
          if(is.numeric(mydata[[var]])) {
            # Get required dimensions
            needed_cols <- ncol(dd$limits)

            # Calculate basic limits
            basic_limits <- c(
              quantile(mydata[[var]], 0.1, na.rm=TRUE),  # Low
              median(mydata[[var]], na.rm=TRUE),         # Median
              quantile(mydata[[var]], 0.9, na.rm=TRUE)   # High
            )

            # Create full limits vector of correct length
            full_limits <- numeric(needed_cols)
            full_limits[1:3] <- basic_limits  # First 3 are our calculated limits

            if(needed_cols > 3) {
              # Fill remaining positions with median value
              full_limits[4:needed_cols] <- basic_limits[2]
            }

            # Assign to datadist object
            dd$limits[var,] <- full_limits
          }
        }

        # Set datadist globally
        options(datadist = dd)

        # Create formula and fit model
        coxformula <- paste0("survival::Surv(mytime, myoutcome) ~ ",
                             paste(var_names, collapse = " + "))

        # Fit the model
        f <- rms::cph(formula = as.formula(coxformula),
                      data = mydata,
                      x = TRUE,
                      y = TRUE,
                      surv = TRUE)

        # Get prediction timepoints
        pred_times <- as.numeric(unlist(strsplit(self$options$cutp, ",")))
        if(length(pred_times) == 0) pred_times <- c(12, 36, 60)

        # Add checkpoint before creating nomogram
        private$.checkpoint()

        # Create nomogram
        nom <- try({
          base_surv <- survival::survfit(cox_model)
          surv_at_time <- summary(base_surv, times = pred_times[1])$surv[1]

          rms::nomogram(f,
                        fun = function(lp) {
                          1 - surv_at_time^exp(lp - mean(cox_model$linear.predictors))
                        },
                        funlabel = paste("Predicted", pred_times[1], "month risk"),
                        fun.at = seq(0.1, 0.9, by = 0.1))
        })


        # private$.nom_object <- nom

        # Store results
        if (!inherits(nom, "try-error")) {
          private$.nom_object <- nom

          # Create the nomogram points table
          html_display <- private$.create_nomogram_display(nom)

          # mydataview_nomogram
          cox_summary <- cox_model$coefficient
          modelSummary <- summary(cox_model)

          # self$results$mydataview_nomogram$setContent(
          #   list(
          #     cox_model = cox_model,
          #     modelSummary = modelSummary,
          #     coef_table = modelSummary$coefficients,
          #     conf_table = modelSummary$conf.int,
          #     cox_summary = cox_summary,
          #     dd = dd,
          #     f = f,
          #     pred_times = pred_times,
          #     nomogram = if(!inherits(nom, "try-error")) nom else NULL,
          #     error = if(inherits(nom, "try-error")) attr(nom, "condition") else NULL,
          #     html_display = if(exists(html_display)) html_display else NULL
          #
          #   )
          # )

          self$results$nomogram_display$setContent(html_display)

          }





        }






      ,
      # Plotting function
      .plot_nomogram = function(image, ggtheme, theme, ...) {
        if(is.null(private$.nom_object)) {
          return(FALSE)
        }

        par(mar = c(4, 4, 2, 2))
        plot(private$.nom_object)
        return(TRUE)
      }


      ,

      .create_nomogram_display = function(nom) {
        if(is.null(private$.nom_object)) {
          return(FALSE)
        }

        # Capture the nomogram output
        nom_output <- capture.output(print(nom))

        # Extract technical details
        tech_details <- c()
        i <- 1
        while(i <= length(nom_output) && !grepl("Points$", nom_output[i])) {
          if(nzchar(nom_output[i])) {
            tech_details <- c(tech_details, nom_output[i])
          }
          i <- i + 1
        }

        # Initialize data structures
        sections <- list()
        current_section <- NULL
        current_lines <- character(0)
        risk_table <- NULL

        # Process each line
        while(i <= length(nom_output)) {
          line <- nom_output[i]

          # Check for new section or risk table
          if(grepl("Total Points Predicted", line)) {
            # We've hit the risk table - save current section and start collecting risk data
            if(!is.null(current_section)) {
              sections[[current_section]] <- current_lines
            }
            risk_table <- c(line)  # Start risk table with header
            while(i < length(nom_output) && nzchar(trimws(nom_output[i + 1]))) {
              i <- i + 1
              risk_table <- c(risk_table, nom_output[i])
            }
            current_section <- NULL
            current_lines <- character(0)
          } else if(grepl("Points$", line) && !grepl("Total Points", line)) {
            # New variable section
            if(!is.null(current_section)) {
              sections[[current_section]] <- current_lines
            }
            current_section <- trimws(sub("Points$", "", line))
            current_lines <- character(0)
          } else if(nzchar(trimws(line))) {
            current_lines <- c(current_lines, line)
          }
          i <- i + 1
        }

        # Add final section if exists
        if(!is.null(current_section) && length(current_lines) > 0) {
          sections[[current_section]] <- current_lines
        }

        # Create HTML content
        html_content <- paste0('
    <style>
        .nomogram-container {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            max-width: 800px;
            margin: 20px auto;
            padding: 20px;
            background-color: #fff;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            border-radius: 8px;
        }
        .tech-details {
            font-family: "Roboto Mono", monospace;
            background-color: #f8f9fa;
            padding: 15px;
            border-radius: 4px;
            margin: 15px 0;
            color: #666;
        }
        .instructions {
            background-color: #e8f5e9;
            padding: 20px;
            margin: 20px 0;
            border-radius: 8px;
        }
        .inputs-section {
            margin-top: 30px;
            border: 1px solid #e9ecef;
            border-radius: 8px;
            padding: 20px;
        }
        .variable-section {
            margin: 15px 0;
            padding: 15px;
            background-color: #f8f9fa;
            border-left: 4px solid #2196f3;
            border-radius: 4px;
        }
        .section-title {
            font-size: 1.2em;
            font-weight: 600;
            color: #2c3e50;
            margin-bottom: 10px;
        }
        .values {
            font-family: "Roboto Mono", monospace;
            white-space: pre-wrap;
            line-height: 1.5;
            color: #34495e;
            padding-left: 20px;
        }
        .outputs-section {
            margin-top: 30px;
            background-color: #fff3e0;
            border: 1px solid #ffe0b2;
            border-radius: 8px;
            padding: 20px;
        }
        .prediction-table {
            width: 100%;
            margin-top: 15px;
            border-collapse: separate;
            border-spacing: 0;
            font-family: "Roboto Mono", monospace;
        }
        .prediction-table th, .prediction-table td {
            padding: 8px 12px;
            text-align: center;
            border-bottom: 1px solid #ffe0b2;
        }
        .prediction-table th {
            background-color: #fff3e0;
            font-weight: 600;
        }
        .notes {
            background-color: #fffde7;
            padding: 15px;
            margin-top: 20px;
            border-radius: 4px;
        }
    </style>
    <div class="nomogram-container">
        <h2>Nomogram Scoring Guide</h2>

        <div class="tech-details">
            ', paste(tech_details, collapse="<br>"), '
        </div>

        <div class="instructions">
            <h3>How to Use This Nomogram:</h3>
            <ol>
                <li>For each variable below, find your patient\'s value</li>
                <li>Read across to the Points scale to determine points for that variable</li>
                <li>Add up total points from all variables</li>
                <li>Use total points to find predicted risk in the Risk Prediction section</li>
            </ol>
        </div>

        <div class="inputs-section">
            <h3>Input Variables</h3>')

        # Add variable sections
        for(section_name in names(sections)) {
          html_content <- paste0(html_content, '
            <div class="variable-section">
                <div class="section-title">', section_name, '</div>
                <div class="values">',
                                 paste(sections[[section_name]], collapse="<br>"),
                                 '</div>
            </div>')
        }

        # Add risk prediction section with formatted table
        html_content <- paste0(html_content, '
        </div>

        <div class="outputs-section">
            <h3>Risk Prediction</h3>
            <div class="section-title">Points to Risk Conversion</div>
            <table class="prediction-table">
                <tr>
                    <th>Total Points</th>
                    <th>Predicted 12-month Risk</th>
                </tr>')

        # Format risk table into two columns
        if(!is.null(risk_table)) {
          # Skip the header line
          risk_lines <- risk_table[-1]
          for(line in risk_lines) {
            values <- strsplit(trimws(line), "\\s+")[[1]]
            if(length(values) == 2) {
              html_content <- paste0(html_content, '
                <tr>
                    <td>', values[1], '</td>
                    <td>', values[2], '</td>
                </tr>')
            }
          }
        }

        html_content <- paste0(html_content, '
            </table>
        </div>

        <div class="notes">
            <h3>Important Notes:</h3>
            <ul>
                <li>For continuous variables, interpolate between given values</li>
                <li>For categorical variables, use exact points shown</li>
                <li>The predicted risk is based on total points from all variables</li>
                <li>Risk predictions are estimates and should be used in conjunction with clinical judgment</li>
            </ul>
        </div>
    </div>')

        return(html_content)
      }


      # coxph Proportional Hazards Assumption  ----
      ,
      .cox_ph = function(cox_model) {
        # cleaneddata <- private$.cleandata()
        #
        # name1time <- cleaneddata$name1time
        # name2outcome <- cleaneddata$name2outcome
        # name3contexpl <- cleaneddata$name3contexpl
        # name3expl <- cleaneddata$name3expl
        # adjexplanatory_name <- cleaneddata$adjexplanatory_name
        #
        # mydata <- cleanData <- cleaneddata$cleanData
        #
        # mytime_labelled <- cleaneddata$mytime_labelled
        # myoutcome_labelled <- cleaneddata$myoutcome_labelled
        # mydxdate_labelled <- cleaneddata$mydxdate_labelled
        # myfudate_labelled <- cleaneddata$myfudate_labelled
        # myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
        # mycontexpl_labelled <- cleaneddata$mycontexpl_labelled
        # adjexplanatory_labelled <- cleaneddata$adjexplanatory_labelled
        #
        #
        # cox_model <- private$.cox_model()

        private$.checkpoint()

        zph <- survival::cox.zph(cox_model)




        # Add suggestions for stratification
        significant_violations <- which(zph$table[,"p"] < 0.05)
        if (length(significant_violations) > 0) {
          violation_vars <- rownames(zph$table)[significant_violations]
          suggestion <- glue::glue(
            "<br><br>Note: The proportional hazards assumption appears to be
            violated for: {paste(violation_vars, collapse=', ')}.
            Consider using these as stratification variables instead of
            covariates."
          )

          self$results$cox_ph$setContent(
            paste(print(zph), suggestion)
          )
        }



        # Display test results
        self$results$cox_ph$setContent(print(zph))






        # Only create plots if there are variables to plot
        if (!is.null(zph$y)) {
          # Pass zph object to plot function
          image8 <- self$results$plot8
          image8$setState(zph)
        } else {
          # If no variables to plot, hide the plot
          self$results$plot8$setVisible(FALSE)
        }

      }




      # hr_plot ----
      ,
      .plot = function(image, ggtheme, theme, ...) {
        if (!self$options$hr) {
          return()
        }

        if (!(self$options$sty == "t1")) {
          return()
        }

        plotData <- image$state

        if (is.null(plotData)) {
          return()
        }

        name1time <- plotData$name1time
        name2outcome <- plotData$name2outcome
        name3contexpl <- plotData$name3contexpl
        name3expl <- plotData$name3expl

        mydata <- cleanData <- plotData$cleanData

        mytime_labelled <- plotData$mytime_labelled
        myoutcome_labelled <- plotData$myoutcome_labelled
        mydxdate_labelled <- plotData$mydxdate_labelled
        myfudate_labelled <- plotData$myfudate_labelled
        myexplanatory_labelled <- plotData$myexplanatory_labelled
        mycontexpl_labelled <- plotData$mycontexpl_labelled
        mystratvar_labelled <- plotData$mystratvar_labelled


        ### prepare formula ----

        myexplanatory <- NULL
        if (!is.null(self$options$explanatory)) {
          myexplanatory <- as.vector(myexplanatory_labelled)
        }

        mycontexpl <- NULL
        if (!is.null(self$options$contexpl)) {
          mycontexpl <- as.vector(mycontexpl_labelled)
        }

        formula2 <- c(myexplanatory, mycontexpl)


        # Remove stratification variables from the finalfit output
        if (self$options$use_stratify && !is.null(self$options$stratvar)) {
          # Remove stratified variables from the display
          formula2 <- formula2[!formula2 %in% mystratvar_labelled]
        }




        myformula <-
          paste0('Surv( mytime, myoutcome )')


        # hr_plot ----
        # https://finalfit.org/reference/hr_plot.html

        plot <-
          finalfit::hr_plot(
            .data = mydata,
            dependent = myformula,
            explanatory = formula2,
            dependent_label = "Survival",
            table_text_size = 4,
            title_text_size = 14,
            plot_opts = list(
              ggplot2::xlab("HR, 95% CI"),
              ggplot2::theme(axis.title =
                               ggplot2::element_text(size = 12))
            )
          )


        # print plot ----

        print(plot)
        TRUE

      }






      # Forest plot ----
      ,
      .plot3 = function(image3, ggtheme, theme, ...) {
        if (!self$options$hr) {
          return()
        }

        if (!(self$options$sty == "t3")) {
          return()
        }

        plotData <- image3$state

        if (is.null(plotData)) {
          return()
        }

        name1time <- plotData$name1time
        name2outcome <- plotData$name2outcome
        name3contexpl <- plotData$name3contexpl
        name3expl <- plotData$name3expl

        mydata <- cleanData <- plotData$cleanData

        mytime_labelled <- plotData$mytime_labelled
        myoutcome_labelled <- plotData$myoutcome_labelled
        mydxdate_labelled <- plotData$mydxdate_labelled
        myfudate_labelled <- plotData$myfudate_labelled
        myexplanatory_labelled <- plotData$myexplanatory_labelled
        mycontexpl_labelled <- plotData$mycontexpl_labelled
        mystratvar_labelled <- plotData$mystratvar_labelled


        ### prepare formula ----

        myexplanatory <- NULL
        if (!is.null(self$options$explanatory)) {
          myexplanatory <- as.vector(myexplanatory_labelled)
        }

        mycontexpl <- NULL
        if (!is.null(self$options$contexpl)) {
          mycontexpl <- as.vector(mycontexpl_labelled)
        }

        formula2 <- c(myexplanatory, mycontexpl)


        # Remove stratification variables from the finalfit output
        if (self$options$use_stratify && !is.null(self$options$stratvar)) {
          # Remove stratified variables from the display
          formula2 <- formula2[!formula2 %in% mystratvar_labelled]
        }


        myformula <-
          paste("survival::Surv(mytime, myoutcome) ~ ",
                paste(formula2, collapse = " + "))



        myformula <- as.formula(myformula)

        mod <-
          survival::coxph(formula = myformula, data = mydata)


        # ggforest ----

        plot3 <- survminer::ggforest(model = mod, data = mydata)


        # print plot ----

        print(plot3)
        TRUE

      }


      # cox.zph plot8 ----
      ,
      .plot8 = function(image8, ggtheme, theme, ...) {
        if (!self$options$ph_cox)
          return()

        zph <- image8$state

        if (is.null(zph)) {
          return()
        }

        # Check if there are variables to plot
        if (is.null(zph$y)) {
          return()
        }

        # Create plot using survminer
        plot8 <- survminer::ggcoxzph(zph)

        print(plot8)
        TRUE

      }


      # Kaplan-Meier ----
      ,


      .plotKM = function(imageKM, ggtheme, theme, ...) {
        # Check conditions and show message if not met
        if (length(self$options$explanatory) > 2) {
          text_warning <- "Kaplan-Meier plot requires 2 categorical explanatory variables.\nYou have selected more than 2 variables."
          # grid::grid.newpage()
          # grid::grid.text(text_warning, 0.5, 0.5)


        # Create a new page
        grid::grid.newpage()

        # Create a viewport with margins for better readability
        vp <- grid::viewport(
          width = 0.9,    # Wider viewport for left-aligned text
          height = 0.9,   # Keep reasonable margins
          x = 0.5,        # Center the viewport
          y = 0.5         # Center the viewport
        )
        grid::pushViewport(vp)

        # Add the text with left alignment
        grid::grid.text(
          text_warning,
          x = 0.05,           # Move text to the left (5% margin)
          y = 0.95,           # Start from top (5% margin)
          just = c("left", "top"),  # Left align and top justify
          gp = grid::gpar(
            fontsize = 11,        # Maintain readable size
            fontface = "plain",   # Regular font
            lineheight = 1.3      # Slightly increased line spacing for readability
          )
        )

        # Reset viewport
        grid::popViewport()

          return(TRUE)
        }






        if (!is.null(self$options$contexpl)) {
          text_warning <- "Kaplan-Meier plot cannot be created with continuous explanatory variables. Please select only categorical variables."
          grid::grid.newpage()
          grid::grid.text(text_warning, 0.5, 0.5)
          return(TRUE)
        }

        if (length(self$options$explanatory) < 2) {
          text_warning <- "Please select 2 categorical explanatory variables to create the Kaplan-Meier plot."
          grid::grid.newpage()
          grid::grid.text(text_warning, 0.5, 0.5)
          return(TRUE)
        }


        # if (length(self$options$explanatory) > 2)
        #     stop("Kaplan-Meier function allows maximum of 2 explanatory variables")
        #
        # if (!is.null(self$options$contexpl))
        #     stop("Kaplan-Meier function does not use continuous explanatory variables.")





        plotData <- imageKM$state

        if (is.null(plotData)) {
          return()
        }

        name1time <- plotData$name1time
        name2outcome <- plotData$name2outcome
        name3contexpl <- plotData$name3contexpl
        name3expl <- plotData$name3expl

        mydata <- cleanData <- plotData$cleanData

        mytime_labelled <- plotData$mytime_labelled
        myoutcome_labelled <- plotData$myoutcome_labelled
        mydxdate_labelled <- plotData$mydxdate_labelled
        myfudate_labelled <- plotData$myfudate_labelled
        myexplanatory_labelled <- plotData$myexplanatory_labelled
        mycontexpl_labelled <- plotData$mycontexpl_labelled


        ### prepare formula ----

        myexplanatory <- NULL
        if (!is.null(self$options$explanatory)) {
          myexplanatory <- as.vector(myexplanatory_labelled)
        }


        # myformula <-
        #     paste("survival::Surv(mytime, myoutcome) ~ ",
        #           paste(myexplanatory, collapse = " + "))
        #
        #
        # myformula <- as.formula(myformula)
        #


        thefactor <- jmvcore::constructFormula(terms = myexplanatory)


        title2 <- as.character(thefactor)

        plotKM <- mydata %>%
          finalfit::surv_plot(
            .data = .,
            dependent = 'survival::Surv(mytime, myoutcome)',
            explanatory = thefactor,
            xlab = paste0('Time (', self$options$timetypeoutput, ')'),
            pval = self$options$pplot,
            pval.method	= self$options$pplot,
            legend = 'none',
            break.time.by = self$options$byplot,
            xlim = c(0, self$options$endplot),
            title = paste0("Survival curves for ", title2),
            subtitle = "Based on Kaplan-Meier estimates",
            risk.table = self$options$risktable,
            conf.int = self$options$ci95,
            censor = self$options$censored,
            surv.median.line = self$options$medianline

          )

        # plot <- plot + ggtheme

        print(plotKM)
        TRUE



      }












      ,
      # Risk Score Methods ----

      ## Calculate Risk Score ----

      .calculateRiskScore = function(cox_model, mydata) {

        ### Calculate risk scores ----
        risk_scores <- predict(cox_model, type = "risk")

        ### Add risk scores to data ----
        mydata$risk_score <- risk_scores


        ### Add risk scores to output if requested ----
        if (self$options$addRiskScore &&
            self$results$addRiskScore$isNotFilled()) {
          self$results$addRiskScore$setRowNums(mydata$row_names)
          self$results$addRiskScore$setValues(mydata$risk_score)
        }


        # # Create risk groups using quantiles
        # mydata$risk_group <- cut(
        #   mydata$risk_score,
        #   breaks = quantile(mydata$risk_score, probs = seq(0, 1, by = 0.25)),
        #   labels = c(
        #     "Low Risk",
        #     "Intermediate-Low Risk",
        #     "Intermediate-High Risk",
        #     "High Risk"
        #   ),
        #   include.lowest = TRUE
        # )


        ### Function to try creating risk groups ----
        createRiskGroups <- function(n_groups) {
          tryCatch({
            if(n_groups == 2) {
              probs <- c(0, 0.5, 1)
              labels <- c("Low Risk", "High Risk")
            } else if(n_groups == 3) {
              probs <- c(0, 1/3, 2/3, 1)
              labels <- c("Low Risk", "Intermediate Risk", "High Risk")
            } else {
              probs <- c(0, 0.25, 0.5, 0.75, 1)
              labels <- c("Low Risk", "Intermediate-Low Risk",
                          "Intermediate-High Risk", "High Risk")
            }

            groups <- cut(mydata$risk_score,
                          breaks = quantile(mydata$risk_score, probs = probs),
                          labels = labels,
                          include.lowest = TRUE)

            #### Verify we have at least one observation per group ----
            if(any(table(groups) == 0)) {
              stop("Some groups have zero observations")
            }

            return(list(success = TRUE, groups = groups))
          }, error = function(e) {
            return(list(success = FALSE, error = e$message))
          })
        }

        #### Try to create requested number of groups with fallback ----
        desired_groups <- switch(self$options$numRiskGroups,
                                 "four" = 4,
                                 "three" = 3,
                                 "two" = 2)

        result <- NULL
        warning_message <- NULL

        while(desired_groups >= 2 && is.null(result)) {
          attempt <- createRiskGroups(desired_groups)
          if(attempt$success) {
            result <- attempt$groups
            if(desired_groups < switch(self$options$numRiskGroups,
                                       "four" = 4,
                                       "three" = 3,
                                       "two" = 2)) {
              warning_message <- paste("Could not create", self$options$numRiskGroups,
                               "groups. Fell back to", desired_groups, "groups.")
            }
          } else {
            desired_groups <- desired_groups - 1
          }
        }


        mydata$risk_group <- result

        ### Add risk group to output if requested ----
        if (self$options$addRiskGroup &&
            self$results$addRiskGroup$isNotFilled()) {
          self$results$addRiskGroup$setRowNums(mydata$row_names)
          self$results$addRiskGroup$setValues(mydata$risk_group)
        }

        ### Calculate summary statistics ----
        risk_summary <- data.frame(
          group = levels(mydata$risk_group),
          n_patients = as.numeric(table(mydata$risk_group)),
          events = tapply(mydata$myoutcome, mydata$risk_group, sum),
          median_score = tapply(mydata$risk_score, mydata$risk_group, median)
        )

        risk_summary$percent <- (risk_summary$n_patients / sum(risk_summary$n_patients)) * 100

        ### Fill risk score table ----
        riskScoreTable <- self$results$riskScoreTable

        for (i in seq_len(nrow(risk_summary))) {
          riskScoreTable$addRow(
            rowKey = i,
            values = list(
              group = risk_summary$group[i],
              n_patients = risk_summary$n_patients[i],
              # percent = risk_summary$percent[i],
              percent = round(risk_summary$percent[i], 1),  # Round to 1 decimal
              # median_score = risk_summary$median_score[i],
              median_score = round(risk_summary$median_score[i], 3),  # Round to 3 decimals
              events = risk_summary$events[i]
            )
          )
        }

        ### Create metrics summary ----
        c_index <- survival::concordance(cox_model)$concordance

        c_index_formatted <- sprintf("%.3f", c_index)

        # Create dynamic group summary text
        group_summary <- character()
        for(i in seq_len(nrow(risk_summary))) {
          group_summary[i] <- glue::glue("{risk_summary$group[i]}: {risk_summary$n_patients[i]} ({format(risk_summary$percent[i], digits=1, nsmall=1)}%)")

        }
        group_text <- paste(group_summary, collapse = "<br>")

        metrics_html <- glue::glue(
          "
<br>
<b>Risk Score Model Performance:</b><br>
Harrell's C-index: {sprintf('%.3f', c_index)}<br>
<br>"
# Number of patients in risk groups:<br>
# {group_text}<br>
# "
        )

        self$results$riskScoreMetrics$setContent(metrics_html)


        percentile_text <- switch(
          as.character(length(levels(mydata$risk_group))),
          "2" = "50th percentile are classified as Low Risk, above as High Risk",
          "3" = "33rd percentile are Low Risk, between 33rd-67th percentiles are Intermediate Risk, and above 67th percentile are High Risk",
          "4" = "25th percentile are Low Risk, 25th-50th are Intermediate-Low Risk, 50th-75th are Intermediate-High Risk, and above 75th percentile are High Risk"
        )

        message_risk_score_analysis <- glue::glue(
"<b>Risk Scores Were Calculated As Follows:</b><br>
The risk scores were calculated using the coefficients from the Cox proportional hazards model.
These scores represent the predicted risk of the event occurring based on the combined effect of all variables in the model.
A higher score indicates a greater predicted risk.<br>
<br>
Patients were then divided into {as.character(length(levels(mydata$risk_group)))} equal-sized groups based on these risk scores:
 <br>
- Scores below the {percentile_text}.<br>
<br>
The Harrell's C-index of {c_index_formatted} indicates the model's discriminative ability,
where 0.5 suggests no discriminative ability and 1.0 indicates perfect discrimination between risk groups.
<br><br>
"
        )

        if(is.null(result)) {
          message_risk_score_analysis <- "Unable to create risk groups. Check if risk scores have enough variation."
        }


        self$results$risk_score_analysis$setContent(""
          # list(
          #   desired_groups,
          #   percentile_text,
          #   message_risk_score_analysis,
          #   warning_message,
          #   length(levels(mydata$risk_group)),
          #   levels(mydata$risk_group),
          #   c_index,
          #   c_index_formatted
          #   )
        )

        self$results$risk_score_analysis2$setContent(message_risk_score_analysis)


        return(mydata)
      }

      ## Plot Risk Groups ----
      ,
      .plotRiskGroups = function(image_riskGroupPlot, ggtheme, theme, ...) {
        # Check if risk score calculation is enabled
        if (!self$options$calculateRiskScore ||
            !self$options$plotRiskGroups) {
          return()
        }

        # Get data from image state
        riskData <- image_riskGroupPlot$state
        if (is.null(riskData)) {
          return()
        }

        # Keep only needed columns
        plotData <- data.frame(
          time = riskData$mytime,
          status = riskData$myoutcome,
          group = riskData$risk_group
        )

        # Create survival object and fit
        fit <- survival::survfit(survival::Surv(time, status) ~ group, data = plotData)

        # Create plot
        plot <- survminer::ggsurvplot(
          fit = fit,
          data = plotData,
          risk.table.height = 0.3,
          risk.table.y.text.col = TRUE,
          risk.table.y.text = FALSE,
          ncensor.plot = TRUE,
          ncensor.plot.height = 0.25,
          xlab = paste0("Time (", self$options$timetypeoutput, ")"),
          ylab = "Survival probability",

          pval = self$options$pplot,
          pval.method	= self$options$pplot,
          break.time.by = self$options$byplot,
          xlim = c(0, self$options$endplot),
          risk.table = self$options$risktable,
          conf.int = self$options$ci95,
          censor = self$options$censored,




          title = "Survival by Risk Group",
          subtitle = "Based on Cox model risk score quartiles",
          legend.title = "Risk Group",
          palette = "Set2",
          ggtheme = ggplot2::theme_bw() +
            ggplot2::theme(
              plot.title = ggplot2::element_text(size = 14, face = "bold"),
              plot.subtitle = ggplot2::element_text(size = 12),
              axis.title = ggplot2::element_text(size = 12),
              axis.text = ggplot2::element_text(size = 10),
              legend.text = ggplot2::element_text(size = 10)
            )
        )

        print(plot)
        TRUE
      }






      # ,
      # Compare Models ----
    #   .compare_models = function() {
    #     # Get clean data
    #     cleaneddata <- private$.cleandata()
    #     mydata <- cleaneddata$cleanData
    #
    #     # Get full model variables
    #     full_explanatory <- NULL
    #     if (!is.null(self$options$explanatory)) {
    #       full_explanatory <- as.vector(cleaneddata$myexplanatory_labelled)
    #     }
    #
    #     full_contexpl <- NULL
    #     if (!is.null(self$options$contexpl)) {
    #       full_contexpl <- as.vector(cleaneddata$mycontexpl_labelled)
    #     }
    #
    #     # Get reduced model variables
    #     reduced_explanatory <- NULL
    #     if (!is.null(self$options$reduced_explanatory)) {
    #       reduced_explanatory <- names(labelled::var_label(mydata))[match(self$options$reduced_explanatory,
    #                                                                       labelled::var_label(mydata))]
    #     }
    #
    #     # Create formulas
    #     full_formula <- c(full_explanatory, full_contexpl)
    #
    #     # Run finalfit with model comparison
    #     comparison <- finalfit::finalfit(
    #       .data = mydata,
    #       dependent = 'survival::Surv(mytime, myoutcome)',
    #       explanatory = full_formula,
    #       explanatory_multi = reduced_explanatory,
    #       keep_models = TRUE
    #     )
    #
    #     # Create comparison table
    #     html_comparison <- knitr::kable(comparison[[1]], format = 'html', caption = "Full vs Reduced Model Comparison")
    #
    #     # Add metrics
    #     metrics_html <- glue::glue(
    #       "
    #     <br>
    #     <b>Model Comparison Metrics:</b><br>
    #     Full model AIC: {comparison[[2]]$AIC.full}<br>
    #     Reduced model AIC: {comparison[[2]]$AIC.reduced}<br>
    #     Likelihood ratio test p-value: {comparison[[2]]$lrtest.pvalue}
    # "
    #     )
    #
    #     # Set results
    #     self$results$model_comparison$setContent(html_comparison)
    #     self$results$reduced_model_metrics$setContent(metrics_html)
    #   }



      # Adjusted ----


      ,
    ## calculate Adjusted Stats ----
    .calculateAdjustedStats = function() {
      # Skip if adjusted curves not requested
      if (!self$options$ac) return(NULL)

      # Get cleaned data and check requirements
      cleaneddata <- private$.cleandata()
      if (is.null(cleaneddata)) return(NULL)

      data <- cleaneddata$cleanData
      adj_var <- cleaneddata$adjexplanatory_name

      # if (is.null(adj_var)) {
      #   stop('Please select a variable for adjusted curves')
      # }

      todo <- 'Please select a variable for adjusted curves'

      html <- self$results$todo
      html$setContent(todo)
      return()

      # Add checkpoint before calculations
      private$.checkpoint()

      # Get baseline Cox model
      cox_model <- private$.cox_model()

      # Get unique levels and validate
      levels <- sort(unique(data[[adj_var]]))
      # if (length(levels) < 2) {
      #   stop("Adjustment variable must have at least 2 levels")
      # }
      todo <- 'Adjustment variable must have at least 2 levels'

      html <- self$results$todo
      html$setContent(todo)
      return()

      # Get timepoints for summaries
      timepoints <- if (self$options$ac_summary) {
        tryCatch({
          pts <- as.numeric(trimws(unlist(strsplit(self$options$ac_timepoints, ","))))
          pts <- sort(unique(pts[!is.na(pts)]))
          if (length(pts) == 0) c(12, 36, 60) else pts
        }, error = function(e) c(12, 36, 60))
      } else {
        NULL
      }

      # Calculate adjusted curves for each level
      results <- list()
      summary_rows <- list()

      for (level in levels) {
        tryCatch({
          # Create prediction data with mean/mode values for covariates
          pred_df <- data.frame(
            mytime = sort(unique(c(timepoints, data$mytime)))
          )

          # Add averaged covariates
          for (var in names(data)) {
            if (var != "mytime" && var != adj_var && var != "row_names") {
              if (is.numeric(data[[var]])) {
                pred_df[[var]] <- mean(data[[var]], na.rm = TRUE)
              } else if (is.factor(data[[var]])) {
                pred_df[[var]] <- names(which.max(table(data[[var]])))
              }
            }
          }
          pred_df[[adj_var]] <- level

          # Calculate adjusted survival
          pred_surv <- survival::survfit(cox_model, newdata = pred_df)

          # Store curve data
          if (!is.null(pred_surv)) {
            level_stats <- data.frame(
              time = pred_surv$time,
              survival = pred_surv$surv,
              std.err = pred_surv$std.err,
              lower = pred_surv$lower,
              upper = pred_surv$upper,
              n.risk = pred_surv$n.risk
            )

            results[[as.character(level)]] <- list(
              full_curve = level_stats
            )

            # Calculate summary statistics at specified timepoints
            if (!is.null(timepoints)) {
              for (t in timepoints) {
                idx <- which.min(abs(level_stats$time - t))
                if (length(idx) > 0) {
                  summary_row <- list(
                    Level = as.character(level),
                    Timepoint = t,
                    Survival = level_stats$survival[idx],
                    SE = level_stats$std.err[idx],
                    CI_Lower = level_stats$lower[idx],
                    CI_Upper = level_stats$upper[idx],
                    N_at_Risk = level_stats$n.risk[idx]
                  )
                  if (!any(sapply(summary_row, is.null)) &&
                      !any(sapply(summary_row, is.na))) {
                    summary_rows[[length(summary_rows) + 1]] <- summary_row
                  }
                }
              }
            }
          }
        }, error = function(e) {
          warning(paste("Error processing level", level, ":", e$message))
        })
      }

      # Add metadata
      attr(results, "timepoints") <- timepoints
      attr(results, "levels") <- levels
      attr(results, "variable") <- adj_var
      attr(results, "method") <- self$options$ac_method

      # Generate summary table
      if (length(summary_rows) > 0) {
        # Sort by level and timepoint
        sorted_indices <- order(
          sapply(summary_rows, function(x) x$Level),
          sapply(summary_rows, function(x) x$Timepoint)
        )
        summary_rows <- summary_rows[sorted_indices]

        # Add rows to table
        for (i in seq_along(summary_rows)) {
          row <- summary_rows[[i]]
          self$results$adjustedSummaryTable$addRow(
            rowKey = i,
            values = list(
              Level = row$Level,
              Timepoint = row$Timepoint,
              Survival = round(row$Survival, 3),
              SE = round(row$SE, 3),
              CI_Lower = round(row$CI_Lower, 3),
              CI_Upper = round(row$CI_Upper, 3)
            )
          )
        }
      }

      # Run additional analyses
      if (self$options$ac_summary) {
        private$.adjustedSurvTable(results, cox_model)
        private$.adjustedMedianSurv(results, cox_model)
        private$.adjustedCox(results, cox_model)
      }

      if (self$options$ac_compare) {
        private$.adjustedPairwise(results, cox_model)
      }

      return(results)
    }



      ,
    ## Adjusted Survival Table ----
    .adjustedSurvTable = function(results, cox_model) {
      # Get data components
      mytime <- results$name1time
      myoutcome <- results$name2outcome
      adj_var <- results$adjexplanatory_name
      mydata <- results$cleanData

      # Input validation
      if (is.null(mydata) || is.null(cox_model)) {
        return(NULL)
      }

      # Get timepoints
      timepoints <- tryCatch({
        pts <- as.numeric(trimws(unlist(strsplit(self$options$cutp, ","))))
        pts <- sort(unique(pts[!is.na(pts)]))
        if (length(pts) == 0) c(12, 36, 60) else pts
      }, error = function(e) c(12, 36, 60))

      # Get levels
      levels <- sort(unique(mydata[[adj_var]]))

      # Create base prediction data
      pred_base <- list()
      for (var in names(mydata)) {
        if (var != "mytime" && var != adj_var && var != "row_names" && var != myoutcome) {
          if (is.numeric(mydata[[var]])) {
            pred_base[[var]] <- mean(mydata[[var]], na.rm = TRUE)
          } else if (is.factor(mydata[[var]])) {
            pred_base[[var]] <- names(which.max(table(mydata[[var]])))
          }
        }
      }

      # Calculate survival for each level
      all_results <- list()

      for (level in levels) {
        # Create prediction data
        n_times <- length(timepoints)
        pred_data <- data.frame(
          mytime = timepoints
        )

        # Add mean covariates
        for (var in names(pred_base)) {
          pred_data[[var]] <- rep(pred_base[[var]], n_times)
        }

        # Add level
        pred_data[[adj_var]] <- rep(level, n_times)

        # Calculate survival
        surv_fit <- survival::survfit(cox_model, newdata = pred_data)
        surv_summ <- summary(surv_fit, times = timepoints)

        # Store results
        for (i in seq_along(timepoints)) {
          if (i <= length(surv_summ$time)) {
            all_results[[length(all_results) + 1]] <- list(
              Level = level,
              Time = timepoints[i],
              "Number at Risk" = surv_summ$n.risk[i],
              Events = surv_summ$n.event[i],
              "Adjusted Survival" = scales::percent(surv_summ$surv[i], accuracy = 0.1),
              "95% CI Lower" = scales::percent(surv_summ$lower[i], accuracy = 0.1),
              "95% CI Upper" = scales::percent(surv_summ$upper[i], accuracy = 0.1)
            )
          }
        }
      }

      # Add results to table
      if (length(all_results) > 0) {
        # Clear existing rows
        self$results$adjustedSurvTable$setRows(NULL)

        # Add new rows
        for (i in seq_along(all_results)) {
          row <- all_results[[i]]
          self$results$adjustedSurvTable$addRow(
            rowKey = i,
            values = row
          )
        }

        # Generate natural language interpretations
        summaries <- sapply(all_results, function(row) {
          glue::glue(
            "For {row$Level} at {row$Time} months, adjusted survival is {row$`Adjusted Survival`} ",
            "[{row$`95% CI Lower`}-{row$`95% CI Upper`}, 95% CI]. ",
            "At this timepoint, {row$`Number at Risk`} subjects were at risk ",
            "and {row$Events} events had occurred. ",
            "These estimates account for the average values of covariates."
          )
        })

        self$results$adjustedSurvTableSummary$setContent(summaries)
      }

      return(all_results)
    }



      #
      # .calculateAdjustedStats = function() {
      #   if (!self$options$ac) return(NULL)
      #
      #   # Get data and fit model
      #   cleaneddata <- private$.cleandata()
      #   if (is.null(cleaneddata)) return(NULL)
      #
      #   adj_var <- cleaneddata$adjexplanatory_name
      #   if (is.null(adj_var)) {
      #     stop('Please select a variable for adjusted curves')
      #   }
      #
      #   # Fit Cox model
      #   cox_model <- private$.fitCoxModel(cleaneddata)
      #
      #   # Calculate survival tables and summaries
      #   surv_results <- private$.adjustedSurvTable(cleaneddata, cox_model)
      #   median_results <- private$.adjustedMedianSurv(cleaneddata, cox_model)
      #   cox_results <- private$.adjustedCox(cleaneddata, cox_model)
      #
      #   if (self$options$ac_compare) {
      #     pairwise_results <- private$.adjustedPairwise(cleaneddata, cox_model)
      #   }
      #
      #   return(list(
      #     surv = surv_results,
      #     median = median_results,
      #     cox = cox_results
      #   ))
      # }
      #






      # mydataview_calculateAdjustedStats <- self$results$mydataview_calculateAdjustedStats
      # mydataview_calculateAdjustedStats$setContent(
      #   list(
      #     results = results,
      #     summary_rows = summary_rows
      #   )
      # )

      ,
    ## Adjusted Survival Plot ----
      .plot_adj = function(image_plot_adj, ggtheme, theme, ...) {

        if (!self$options$ac) return()

        plotData <- image_plot_adj$state

        if (is.null(plotData)) {
          return()
        }

        name1time <- plotData$name1time
        name2outcome <- plotData$name2outcome
        name3contexpl <- plotData$name3contexpl
        name3expl <- plotData$name3expl
        adjexplanatory_name <- plotData$adjexplanatory_name

        mydata <- cleanData <- plotData$cleanData

        mytime_labelled <- plotData$mytime_labelled
        myoutcome_labelled <- plotData$myoutcome_labelled
        mydxdate_labelled <- plotData$mydxdate_labelled
        myfudate_labelled <- plotData$myfudate_labelled
        myexplanatory_labelled <- plotData$myexplanatory_labelled
        mycontexpl_labelled <- plotData$mycontexpl_labelled
        adjexplanatory_labelled <- plotData$adjexplanatory_labelled


        if (is.null(plotData$adjexplanatory_name)) {
          text_warning <- "Please select a variable for adjusted curves."
          grid::grid.newpage()
          grid::grid.text(text_warning, 0.5, 0.5)
          return(TRUE)
        }








        ### prepare formula ----

        myexplanatory <- NULL
        if (!is.null(self$options$explanatory)) {
          myexplanatory <- as.vector(myexplanatory_labelled)
        }

        mycontexpl <- NULL
        if (!is.null(self$options$contexpl)) {
          mycontexpl <- as.vector(mycontexpl_labelled)
        }

        formula2 <- c(myexplanatory, mycontexpl)

        myformula <-
          paste("survival::Surv(mytime, myoutcome) ~ ",
                paste(formula2, collapse = " + "))

        myformula <- as.formula(myformula)

        # Fit model
        cox_model <- survival::coxph(myformula, data = mydata)

        # Validate method and try fallback if needed
        method <- self$options$ac_method

        # Try to create plot with specified method
        plot <- tryCatch({
          survminer::ggadjustedcurves(
            fit = cox_model,
            data = mydata,
            variable = adjexplanatory_name,
            method = method,
            conf.int = self$options$ci95,
            risk.table = self$options$risktable,
            xlab = paste0('Time (', self$options$timetypeoutput, ')'),
            title = paste0("Adjusted Survival Curves for ", self$options$adjexplanatory,
                           " (", method, " adjustment)"),
            pval = self$options$pplot,
            pval.method = self$options$pplot,
            legend = "none",
            break.time.by = self$options$byplot,
            xlim = c(0, self$options$endplot),
            censor = self$options$censored,
            surv.median.line = self$options$medianline



          )
        }, error = function(e) {
          # If marginal method fails, try average method instead
          if (method == "marginal") {
            warning("Marginal method failed, falling back to average method")
            survminer::ggadjustedcurves(
              fit = cox_model,
              data = mydata,
              variable = adjexplanatory_name,
              method = "average",  # Fallback to average method
              conf.int = self$options$ci95,
              risk.table = self$options$risktable,
              xlab = paste0('Time (', self$options$timetypeoutput, ')'),
              title = paste0("Adjusted Survival Curves for ",
                             self$options$adjexplanatory,
                             " (average adjustment - marginal failed)"),
              pval = self$options$pplot,
              pval.method = self$options$pplot,
              legend = "none",
              break.time.by = self$options$byplot,
              xlim = c(0, self$options$endplot),
              censor = self$options$censored,
              surv.median.line = self$options$medianline
            )
          } else {
            stop(paste("Error creating adjusted curves:", e$message))
          }
        })




        # # Prepare plot parameters
        # plot_params <- list(
        #   fit = cox_model,
        #   data = mydata,
        #   variable = adjexplanatory_name,
        #   method = self$options$ac_method,
        #   conf.int = self$options$ci95,
        #   risk.table = self$options$risktable,
        #   xlab = paste0('Time (', self$options$timetypeoutput, ')'),
        #   title = paste0("Adjusted Survival Curves for ",
        #                  self$options$adjexplanatory,
        #                  " (", self$options$ac_method, " adjustment)"),
        #   pval = self$options$pplot,
        #   pval.method = self$options$pplot,
        #   legend = "none",
        #   break.time.by = self$options$byplot,
        #   xlim = c(0, self$options$endplot),
        #   censor = self$options$censored,
        #   surv.median.line = self$options$medianline,
        #   risk.table.height = 0.25,  # Added for better risk table sizing
        #   risk.table.y.text.col = TRUE,  # Color code risk table text
        #   ncensor.plot = FALSE,  # Turn off censor plot by default
        #   fontsize = 3.5  # Adjust font size
        # )
        # # Try to create plot with specified method
        # plot <- tryCatch({
        #   do.call(survminer::ggadjustedcurves, plot_params)
        # }, error = function(e) {
        #   # If marginal method fails, try average method instead
        #   if (self$options$ac_method == "marginal") {
        #     warning("Marginal method failed, falling back to average method")
        #     plot_params$method <- "average"
        #     plot_params$title <- paste0("Adjusted Survival Curves for ",
        #                                 self$options$adjexplanatory,
        #                                 " (average adjustment - marginal failed)")
        #     do.call(survminer::ggadjustedcurves, plot_params)
        #   } else {
        #     stop(paste("Error creating adjusted curves:", e$message))
        #   }
        # })
        # # Add additional theme elements if needed
        # plot <- plot +
        #   ggplot2::theme(
        #     plot.title = ggplot2::element_text(size = 14, face = "bold"),
        #     plot.subtitle = ggplot2::element_text(size = 12),
        #     axis.title = ggplot2::element_text(size = 12),
        #     axis.text = ggplot2::element_text(size = 10),
        #     legend.text = ggplot2::element_text(size = 10)
        #   )












        print(plot)
        TRUE
      }



      # ,
      # .adjustedSurvTable = function(results, cox_model) {
      #   # Get data components
      #   mytime <- results$name1time
      #   myoutcome <- results$name2outcome
      #   adj_var <- results$adjexplanatory_name
      #   mydata <- results$cleanData
      #
      #   # Verify we have valid data and model
      #   if (is.null(mydata) || is.null(cox_model)) {
      #     return(NULL)
      #   }
      #
      #   # Get timepoints
      #   timepoints <- tryCatch({
      #     pts <- as.numeric(trimws(unlist(strsplit(self$options$ac_timepoints, ","))))
      #     pts <- sort(unique(pts[!is.na(pts)]))
      #     if (length(pts) == 0) c(12, 36, 60) else pts
      #   }, error = function(e) c(12, 36, 60))
      #
      #   # Get levels of adjustment variable
      #   levels <- sort(unique(mydata[[adj_var]]))
      #   if (length(levels) < 1) {
      #     warning("No levels found in adjustment variable")
      #     return(NULL)
      #   }
      #
      #   # Create base prediction dataset
      #   pred_base <- list()
      #   for (var in names(mydata)) {
      #     if (var != "mytime" && var != adj_var && var != "row_names") {
      #       if (is.numeric(mydata[[var]])) {
      #         pred_base[[var]] <- mean(mydata[[var]], na.rm = TRUE)
      #       } else if (is.factor(mydata[[var]])) {
      #         pred_base[[var]] <- levels(mydata[[var]])[which.max(table(mydata[[var]]))]
      #       }
      #     }
      #   }
      #
      #   # Initialize storage for results
      #   all_results <- list()
      #   row_counter <- 1
      #
      #   # Calculate survival for each level and timepoint
      #   for (level in levels) {
      #     # Create prediction data for this level
      #     pred_data <- data.frame(
      #       mytime = timepoints
      #     )
      #
      #     # Add averaged covariates
      #     for (var in names(pred_base)) {
      #       pred_data[[var]] <- pred_base[[var]]
      #     }
      #     pred_data[[adj_var]] <- level
      #
      #     tryCatch({
      #       # Get predicted survival
      #       surv_fit <- survival::survfit(cox_model, newdata = pred_data)
      #       surv_summary <- summary(surv_fit, times = timepoints)
      #
      #       # Extract results for each timepoint
      #       for (i in seq_along(timepoints)) {
      #         if (i <= length(surv_summary$time)) {
      #           all_results[[row_counter]] <- list(
      #             strata = level,
      #             time = timepoints[i],
      #             n.risk = surv_summary$n.risk[i],
      #             n.event = surv_summary$n.event[i],
      #             surv = surv_summary$surv[i],
      #             lower = surv_summary$lower[i],
      #             upper = surv_summary$upper[i]
      #           )
      #           row_counter <- row_counter + 1
      #         }
      #       }
      #     }, error = function(e) {
      #       warning(paste("Error processing level", level, ":", e$message))
      #     })
      #   }
      #
      #   # Convert results to data frame if we have any
      #   if (length(all_results) > 0) {
      #     results_df <- do.call(rbind, lapply(all_results, as.data.frame))
      #
      #     # Add to results table
      #     survTable <- self$results$adjustedSurvTable
      #     survTable$setRows(NULL) # Clear existing rows
      #
      #     for (i in seq_len(nrow(results_df))) {
      #       survTable$addRow(
      #         rowKey = i,
      #         values = list(
      #           strata = results_df$strata[i],
      #           time = results_df$time[i],
      #           n.risk = results_df$n.risk[i],
      #           n.event = results_df$n.event[i],
      #           surv = scales::percent(results_df$surv[i], accuracy = 0.1),
      #           lower = scales::percent(results_df$lower[i], accuracy = 0.1),
      #           upper = scales::percent(results_df$upper[i], accuracy = 0.1)
      #         )
      #       )
      #     }
      #
      #     # Generate summary text
      #     survTableSummary <- sapply(seq_len(nrow(results_df)), function(i) {
      #       glue::glue(
      #         "For {results_df$strata[i]} at {results_df$time[i]} months, ",
      #         "the adjusted survival probability is {scales::percent(results_df$surv[i], accuracy=0.1)} ",
      #         "[{scales::percent(results_df$lower[i], accuracy=0.1)}-",
      #         "{scales::percent(results_df$upper[i], accuracy=0.1)}, 95% CI]. ",
      #         "These estimates account for the average values of all covariates in the model."
      #       )
      #     })
      #
      #     self$results$adjustedSurvTableSummary$setContent(survTableSummary)
      #
      #     return(results_df)
      #   }
      #
      #   return(NULL)
      # }





      ,
    ## Adjusted Pariwise ----
    .adjustedPairwise = function(results, cox_model) {
      # Get components
      mytime <- results$name1time
      myoutcome <- results$name2outcome
      adj_var <- results$adjexplanatory_name
      mydata <- results$cleanData

      # Error checking
      if (is.null(mydata) || is.null(cox_model)) {
        warning("Missing data or model for pairwise comparisons")
        return(NULL)
      }

      # Get levels
      levels <- sort(unique(mydata[[adj_var]]))
      if (length(levels) < 2) {
        warning("Need at least 2 levels for pairwise comparisons")
        return(NULL)
      }

      # Create prediction dataset with average values
      pred_base <- data.frame(mytime = max(mydata[[mytime]]))
      for (var in names(mydata)) {
        if (var != "mytime" && var != adj_var && var != "row_names") {
          if (is.numeric(mydata[[var]])) {
            pred_base[[var]] <- mean(mydata[[var]], na.rm = TRUE)
          } else if (is.factor(mydata[[var]])) {
            pred_base[[var]] <- names(which.max(table(mydata[[var]])))
          }
        }
      }

      # Initialize p-value matrix
      n_levels <- length(levels)
      p_values <- matrix(NA, n_levels, n_levels)
      rownames(p_values) <- levels
      colnames(p_values) <- levels

      # Calculate pairwise comparisons
      for (i in 1:(n_levels-1)) {
        for (j in (i+1):n_levels) {
          tryCatch({
            # Create test dataset
            test_data <- rbind(pred_base, pred_base)
            test_data[[adj_var]] <- factor(c(levels[i], levels[j]))

            # Calculate survival difference
            test_fit <- survival::survfit(cox_model, newdata = test_data)
            surv_diff <- survival::survdiff(Surv(mytime, myoutcome) ~ factor(test_data[[adj_var]]))
            p_val <- 1 - pchisq(surv_diff$chisq, df = 1)

            p_values[i,j] <- p_val
            p_values[j,i] <- p_val
          }, error = function(e) {
            warning(paste("Error comparing levels", levels[i], "and", levels[j], ":", e$message))
          })
        }
      }

      # Adjust p-values
      padjustmethod <- self$options$padjustmethod
      adj_p <- p.adjust(p_values[upper.tri(p_values)], method = padjustmethod)
      p_values[upper.tri(p_values)] <- adj_p
      p_values[lower.tri(p_values)] <- t(p_values)[lower.tri(p_values)]

      # Convert to long format
      comparisons <- list()
      counter <- 1
      for (i in 1:(n_levels-1)) {
        for (j in (i+1):n_levels) {
          if (!is.na(p_values[i,j])) {
            comparisons[[counter]] <- list(
              rowname = levels[i],
              name = levels[j],
              value = p_values[i,j]
            )
            counter <- counter + 1
          }
        }
      }

      # Add results to table
      if (length(comparisons) > 0) {
        # Clear existing rows
        self$results$adjustedPairwiseTable$setRows(NULL)

        # Add new rows
        for (i in seq_along(comparisons)) {
          comp <- comparisons[[i]]
          self$results$adjustedPairwiseTable$addRow(
            rowKey = i,
            values = list(
              rowname = comp$rowname,
              name = comp$name,
              value = format.pval(comp$value, digits = 3)
            )
          )
        }

        # Create natural language summaries
        summaries <- lapply(comparisons, function(comp) {
          glue::glue(
            "The adjusted survival difference between {comp$rowname} and {comp$name} groups ",
            "has a p-value of {format.pval(comp$value, digits=3, eps=0.001)}. ",
            "This comparison accounts for covariates set at their average values. ",
            "{if(comp$value < 0.05) 'This difference is statistically significant' else 'This difference is not statistically significant'} ",
            "after {padjustmethod} adjustment for multiple comparisons."
          )
        })

        self$results$adjustedPairwiseSummary$setContent(unlist(summaries))
      }

      return(comparisons)
    }




      ,
    ## Adjusted Median Survival ----
    .adjustedMedianSurv = function(results, cox_model) {
      # Get required data
      mytime <- results$name1time
      myoutcome <- results$name2outcome
      adj_var <- results$adjexplanatory_name
      mydata <- results$cleanData

      # Get levels of adjustment variable
      levels <- sort(unique(mydata[[adj_var]]))

      # Create prediction data with average covariate values
      pred_data <- data.frame(
        mytime = sort(unique(mydata[[mytime]]))
      )

      # Add mean/mode values for covariates
      for (var in names(mydata)) {
        if (var != "mytime" && var != adj_var && var != "row_names") {
          if (is.numeric(mydata[[var]])) {
            pred_data[[var]] <- mean(mydata[[var]], na.rm = TRUE)
          } else if (is.factor(mydata[[var]])) {
            pred_data[[var]] <- names(which.max(table(mydata[[var]])))
          }
        }
      }

      # Calculate adjusted survival for each level
      results_list <- list()

      for (level in levels) {
        level_data <- pred_data
        level_data[[adj_var]] <- level

        # Calculate adjusted survival
        adj_surv <- survival::survfit(cox_model, newdata = level_data)

        # Get summary stats
        surv_summary <- summary(adj_surv)

        # Extract median and CI
        median_time <- surv_summary$table["median"]
        lcl <- surv_summary$table["0.95LCL"]
        ucl <- surv_summary$table["0.95UCL"]

        results_list[[level]] <- list(
          factor = level,
          median = median_time,
          x0_95lcl = lcl,
          x0_95ucl = ucl,
          records = sum(!is.na(mydata[[mytime]][mydata[[adj_var]] == level])),
          events = sum(mydata[[myoutcome]][mydata[[adj_var]] == level] == 1, na.rm = TRUE)
        )
      }

      # Convert to data frame
      results_df <- do.call(rbind, lapply(results_list, as.data.frame))
      results_df <- as.data.frame(results_df)

      # Add to results table
      medianTable <- self$results$adjustedMedianTable
      for (i in seq_len(nrow(results_df))) {
        medianTable$addRow(
          rowKey = i,
          values = list(
            factor = results_df$factor[i],
            records = results_df$records[i],
            events = results_df$events[i],
            median = round(results_df$median[i], 1),
            x0_95lcl = round(results_df$x0_95lcl[i], 1),
            x0_95ucl = round(results_df$x0_95ucl[i], 1)
          )
        )
      }

      # Create natural language summaries
      summaries <- lapply(levels, function(level) {
        result <- results_df[results_df$factor == level,]

        description <- glue::glue(
          "For {adj_var} = {level}, adjusted median survival is {round(result$median, 1)} ",
          "[{round(result$x0_95lcl, 1)} - {round(result$x0_95ucl, 1)}, 95% CI] ",
          self$options$timetypeoutput, "."
        )

        if (is.na(result$median)) {
          description <- paste0(
            description,
            "\nNote: The adjusted survival curve for this group does not drop below 1/2 during ",
            "the observation period, thus the median survival is undefined."
          )
        }

        return(description)
      })

      # Add general interpretation
      medianSummary <- c(
        unlist(summaries),
        "The median survival time is when 50% of subjects have experienced the event.",
        "These estimates account for the average values of all other covariates in the model."
      )

      self$results$adjustedMedianSummary$setContent(medianSummary)
    }


      ,
    ## Adjusted Cox ----
    .adjustedCox = function(results, cox_model) {
      mydata <- results$cleanData
      adj_var <- results$adjexplanatory_name

      # Get Cox model summary
      cox_summary <- summary(cox_model)

      # Create metrics summary
      tCoxtext2 <- glue::glue("
        <br>
        <b>Model Metrics:</b><br>
        Concordance: {round(cox_summary$concordance[1], 3)} (SE = {round(cox_summary$concordance[2], 3)})<br>
        Likelihood ratio test = {round(cox_summary$logtest[1], 2)}, df = {cox_summary$logtest[2]}, p = {format.pval(cox_summary$logtest[3], digits=3)}<br>
        Wald test = {round(cox_summary$waldtest[1], 2)}, df = {cox_summary$waldtest[2]}, p = {format.pval(cox_summary$waldtest[3], digits=3)}<br>
        Score test = {round(cox_summary$sctest[1], 2)}, df = {cox_summary$sctest[2]}, p = {format.pval(cox_summary$sctest[3], digits=3)}<br>
    ")

      if (self$options$uselandmark) {
        landmark <- jmvcore::toNumeric(self$options$landmark)
        tCoxtext2 <- glue::glue(
          tCoxtext2,
          "Landmark time used as: ", landmark, " ", self$options$timetypeoutput, "."
        )
      }

      self$results$adjustedCoxText$setContent(tCoxtext2)

      # Extract hazard ratios and CIs
      coef_matrix <- cbind(
        exp(cox_summary$coefficients[, 1]),  # HR
        exp(cox_summary$coefficients[, 1] - 1.96 * cox_summary$coefficients[, 3]),  # Lower CI
        exp(cox_summary$coefficients[, 1] + 1.96 * cox_summary$coefficients[, 3]),  # Upper CI
        cox_summary$coefficients[, 5]  # p-value
      )

      # Create Cox table
      coxTable <- self$results$adjustedCoxTable
      rownames <- row.names(cox_summary$coefficients)

      for (i in seq_len(nrow(coef_matrix))) {
        coxTable$addRow(
          rowKey = i,
          values = list(
            Variable = rownames[i],
            HR = sprintf("%.2f (%.2f-%.2f)",
                         coef_matrix[i,1], coef_matrix[i,2], coef_matrix[i,3]),
            Pvalue = format.pval(coef_matrix[i,4], digits=3)
          )
        )
      }

      # Create interpretive summary
      coxSummary <- sapply(seq_len(nrow(coef_matrix)), function(i) {
        hr <- coef_matrix[i,1]
        var_name <- rownames[i]

        glue::glue(
          "For {var_name}, the adjusted hazard ratio is {round(hr,2)} ",
          "({round(coef_matrix[i,2],2)}-{round(coef_matrix[i,3],2)}, 95% CI). ",
          "This means that, after adjusting for other covariates, ",
          "{ifelse(hr > 1,
                paste('there is a', round((hr-1)*100,1), '% increase in hazard'),
                paste('there is a', round((1-hr)*100,1), '% decrease in hazard'))} ",
          "for each unit increase in {var_name}."
        )
      })

      coxSummary <- c(
        unlist(coxSummary),
        "A hazard ratio greater than 1 indicates increased risk, while less than 1 indicates decreased risk.",
        "All estimates are adjusted for other variables in the model."
      )

      self$results$adjustedCoxSummary$setContent(coxSummary)

      # Proportional hazards check if requested
      if (self$options$ph_cox) {
        zph <- survival::cox.zph(cox_model)
        self$results$adjustedCoxPH$setContent(print(zph))

        # Set state for plot
        image8 <- self$results$adjustedCoxPHPlot
        image8$setState(zph)
      }
    }




      # ,
      # .calculateAdjustedCurves = function(cox_model, mydata, adjexplanatory_name, fallback = TRUE) {
      #
      #   method <- self$options$ac_method
      #
      #   # Try to calculate adjusted curves with specified method
      #   adj_curves <-  tryCatch({
      #       survminer::ggadjustedcurves(
      #         fit = cox_model,
      #         data = mydata,
      #         variable = adjexplanatory_name,
      #         method = method,
      #         conf.int = self$options$ci95,
      #         risk.table = self$options$risktable,
      #         xlab = paste0('Time (', self$options$timetypeoutput, ')'),
      #         title = paste0(
      #           "Adjusted Survival Curves for ",
      #           self$options$adjexplanatory,
      #           " (", method, " adjustment)"
      #         ),
      #         pval = self$options$pplot,
      #         pval.method = self$options$pplot,
      #         legend = "none",
      #         break.time.by = self$options$byplot,
      #         xlim = c(0, self$options$endplot),
      #         censored = self$options$censored
      #       )
      #     }, error = function(e) {
      #       # If marginal method fails, try average method instead
      #       if (method == "marginal") {
      #         warning("Marginal method failed, falling back to average method")
      #         survminer::ggadjustedcurves(
      #           fit = cox_model,
      #           data = mydata,
      #           variable = adjexplanatory_name,
      #           method = "average",  # Fallback to average method
      #           conf.int = self$options$ci95,
      #           risk.table = self$options$risktable,
      #           xlab = paste0('Time (', self$options$timetypeoutput, ')'),
      #           title = paste0(
      #             "Adjusted Survival Curves for ",
      #             self$options$adjexplanatory,
      #             " (average adjustment - marginal failed)"
      #           ),
      #           pval = self$options$pplot,
      #           pval.method = self$options$pplot,
      #           legend = "none",
      #           break.time.by = self$options$byplot,
      #           xlim = c(0, self$options$endplot),
      #           censored = self$options$censored
      #         )
      #       } else {
      #         stop(paste("Error creating adjusted curves:", e$message))
      #       }
      #     }
      #       )
      #
      #
      #   # image_plot_adj <- self$results$plot_adj
      #   # image_plot_adj$setState(adj_curves)
      #
      #
      #   # Extract and structure the data
      #   # curve_data <- list(
      #   #   curves = adj_curves,
      #   #   model = cox_model,
      #   #   data = mydata,
      #   #   variable = adjexplanatory_name,
      #   #   method = method
      #   # )
      #
      #   # class(curve_data) <- "adjusted_curves"
      #
      #
      #   # View curve_data
      #   self$results$mydataview_curve_data$setContent(
      #     list(
      #       # curves = adj_curves,
      #       model = cox_model,
      #       data = mydata,
      #       variable = adjexplanatory_name,
      #       method = method
      #     )
      #   )
      #
      #
      #   # return(curve_data)
      # }



      # ,
      # .plot_adj = function(image_plot_adj, ggtheme, theme, ...) {
      #   if (!self$options$ac) {
      #     return()
      #   }
      #   if (is.null(curve_data)) {
      #     return()
      #   }
      #
      #   plot <- image_plot_adj$state
      #
      #
      #   # plot <- survminer::ggadjustedcurves(plot)
      #
      #
      #   print(plot)
      #   TRUE
      #
      #
      # }




      # ,
      # .plot_adj = function(image_plot_adj, ggtheme, theme, ...) {
      #   if (!self$options$ac) {
      #     return()
      #   }
      #
      #   if (!self$options$ac_curve) {
      #     return()
      #   }
      #
      #
      #   # mydata <- image_plot_adj$state$mydata
      #   # cox_model <- image_plot_adj$state$cox_model
      #   # adjexplanatory_name <- image_plot_adj$state$adjexplanatory_name
      #
      #
      #
      #   cleaneddata <- private$.cleandata()
      #
      #   name1time <- cleaneddata$name1time
      #   name2outcome <- cleaneddata$name2outcome
      #   name3contexpl <- cleaneddata$name3contexpl
      #   name3expl <- cleaneddata$name3expl
      #   adjexplanatory_name <- cleaneddata$adjexplanatory_name
      #
      #   mydata <- cleanData <- cleaneddata$cleanData
      #
      #   mytime_labelled <- cleaneddata$mytime_labelled
      #   myoutcome_labelled <- cleaneddata$myoutcome_labelled
      #   mydxdate_labelled <- cleaneddata$mydxdate_labelled
      #   myfudate_labelled <- cleaneddata$myfudate_labelled
      #   myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
      #   mycontexpl_labelled <- cleaneddata$mycontexpl_labelled
      #   adjexplanatory_labelled <- cleaneddata$adjexplanatory_labelled
      #
      #
      #
      #   # Add stratification variables
      #   mystratvar <- NULL
      #   if (self$options$use_stratify && !is.null(self$options$stratvar)) {
      #     mystratvar <- as.vector(cleaneddata$mystratvar_labelled)
      #     # Create strata terms
      #     mystratvar <- paste0("strata(", mystratvar, ")")
      #   }
      #
      #
      #
      #   myexplanatory <- NULL
      #   if (!is.null(self$options$explanatory)) {
      #     myexplanatory <- as.vector(myexplanatory_labelled)
      #   }
      #
      #   mycontexpl <- NULL
      #   if (!is.null(self$options$contexpl)) {
      #     mycontexpl <- as.vector(mycontexpl_labelled)
      #   }
      #
      #
      #   formula2 <- c(myexplanatory, mycontexpl, mystratvar)
      #
      #
      #
      #   LHT <- "survival::Surv(mytime, myoutcome)"
      #
      #   RHT <- formula2
      #
      #   RHT <- paste(RHT, collapse = " + ")
      #
      #   coxformula <- paste0(LHT, " ~ ", RHT)
      #
      #   coxformula <- as.formula(coxformula)
      #
      #   cox_model <- survival::coxph(coxformula, data = mydata)
      #
      #
      #
      #   fallback <- TRUE
      #   method <- self$options$ac_method
      #
      #   # Try to calculate adjusted curves with specified method
      #   adj_curves <-  tryCatch({
      #     survminer::ggadjustedcurves(
      #       fit = cox_model,
      #       data = mydata,
      #       variable = adjexplanatory_name,
      #       method = method,
      #       conf.int = self$options$ci95,
      #       risk.table = self$options$risktable,
      #       xlab = paste0('Time (', self$options$timetypeoutput, ')'),
      #       title = paste0(
      #         "Adjusted Survival Curves for ",
      #         self$options$adjexplanatory,
      #         " (", method, " adjustment)"
      #       ),
      #       pval = self$options$pplot,
      #       pval.method = self$options$pplot,
      #       legend = "none",
      #       break.time.by = self$options$byplot,
      #       xlim = c(0, self$options$endplot),
      #       censored = self$options$censored
      #     )
      #   }, error = function(e) {
      #     # If marginal method fails, try average method instead
      #     if (method == "marginal") {
      #       warning("Marginal method failed, falling back to average method")
      #       survminer::ggadjustedcurves(
      #         fit = cox_model,
      #         data = mydata,
      #         variable = adjexplanatory_name,
      #         method = "average",  # Fallback to average method
      #         conf.int = self$options$ci95,
      #         risk.table = self$options$risktable,
      #         xlab = paste0('Time (', self$options$timetypeoutput, ')'),
      #         title = paste0(
      #           "Adjusted Survival Curves for ",
      #           self$options$adjexplanatory,
      #           " (average adjustment - marginal failed)"
      #         ),
      #         pval = self$options$pplot,
      #         pval.method = self$options$pplot,
      #         legend = "none",
      #         break.time.by = self$options$byplot,
      #         xlim = c(0, self$options$endplot),
      #         censored = self$options$censored
      #       )
      #     } else {
      #       stop(paste("Error creating adjusted curves:", e$message))
      #     }
      #   }
      #   )
      #
      #
      #
      #
      #   print(adj_curves)
      #   TRUE
      #
      #
      # }



      ,
    ## Fit Cox Model with Selection ----
.fitModelWithSelection = function(formula, data) {
  # Get the selection method and criteria from options
  modelSelection <- self$options$modelSelection
  selectionCriteria <- self$options$selectionCriteria
  pEntry <- self$options$pEntry
  pRemoval <- self$options$pRemoval

  # Validation checks
  if (self$options$pEntry >= self$options$pRemoval) {
    stop("Entry significance must be less than removal significance")
  }

  if (self$options$modelSelection != "enter" &&
      length(c(self$options$explanatory, self$options$contexpl)) < 2) {
    stop("Variable selection requires at least 2 predictor variables")
  }


  private$.checkpoint()


  # If no selection requested, return full model
  if (modelSelection == "enter") {
    # Just fit and return the full model with all variables
    full_model <- survival::coxph(formula, data = data)
    return(full_model)
  }

  # For Cox models we need to preserve the exact Surv() object on the left side
  surv_part <- formula[[2]]  # Gets the Surv() expression itself
  pred_part <- attr(terms(formula), "term.labels")  # All predictor variables

  # Create full and null models
  full_model <- survival::coxph(formula, data = data)
  null_formula <- as.formula(paste(deparse(surv_part), "~ 1"))
  null_model <- survival::coxph(null_formula, data = data)

  # For backward selection

  if (modelSelection == "backward") {
    # Start with all variables
    current_vars <- pred_part
    current_model <- full_model

    # Set status to indicate backward selection is starting
    self$results$text_model_selection$setStatus('running')
    self$results$text2_model_selection$setStatus('running')

    # Initial checkpoint to push status to UI
    private$.checkpoint()

    # Track variables removed for reporting
    removed_vars <- character(0)

    # Remove variables one-by-one if they don't contribute significantly
    changed <- TRUE
    iteration <- 0
    while(changed && length(current_vars) > 0) {
      iteration <- iteration + 1
      changed <- FALSE

      # Add checkpoint at beginning of each iteration
      private$.checkpoint(flush=FALSE)

      # Only try to examine p-values if we have variables
      if (length(current_vars) > 0) {
        # Get model summary
        model_summary <- summary(current_model)

        # Check if we have coefficients
        if (!is.null(model_summary$coefficients)) {
          # Store p-values for each variable
          coef_summary <- model_summary$coefficients
          var_p_values <- coef_summary[, "Pr(>|z|)"]

          # Find least significant variable
          max_p <- max(var_p_values)
          if (max_p > pRemoval) {
            # Which variable has highest p-value
            drop_var_idx <- which.max(var_p_values)
            drop_var <- names(var_p_values)[drop_var_idx]

            # Remove this variable
            current_vars <- setdiff(current_vars, drop_var)
            removed_vars <- c(removed_vars, drop_var)

            # Update status with progress information
            status_msg <- paste0("Removing variable: ", drop_var,
                                 " (p=", format.pval(max_p, digits=3), ")")
            self$results$text2_model_selection$setContent(status_msg)

            # Critical checkpoint before expensive operation - always flush here
            private$.checkpoint()

            if (length(current_vars) > 0) {
              # Create new formula without this variable
              new_formula <- as.formula(paste(deparse(surv_part), "~",
                                              paste(current_vars, collapse = " + ")))

              # This is the most computationally expensive step
              current_model <- survival::coxph(new_formula, data = data)
            } else {
              # If no variables left, use null model
              current_model <- null_model
            }

            changed <- TRUE
          }
        }
      }

      # Add checkpoint after expensive operation to show progress
      # Only flush every 2nd iteration to balance responsiveness with performance
      if (iteration %% 2 == 0) {
        private$.checkpoint()
      }
    }

    # Final model is ready - set status to complete
    self$results$text_model_selection$setStatus('complete')
    self$results$text2_model_selection$setStatus('complete')

    # Final checkpoint to push complete results
    private$.checkpoint()

    # Store selection steps for reporting
    attr(current_model, "selection_steps") <- list(
      removed = removed_vars,
      remaining = current_vars
    )

    return(current_model)

      }

  # For forward selection or stepwise (both)
  else if (modelSelection == "forward" || modelSelection == "both") {
    # Start with no variables
    selected_vars <- character(0)
    current_model <- null_model

    # Add variables one by one
    while (length(selected_vars) < length(pred_part)) {

      private$.checkpoint()


      best_var <- NULL
      best_p <- pEntry
      best_improvement <- 0

      # Try adding each variable not already selected
      for (var in setdiff(pred_part, selected_vars)) {
        # Create formula with this variable added
        if (length(selected_vars) == 0) {
          test_formula <- as.formula(paste(deparse(surv_part), "~", var))
        } else {
          test_formula <- as.formula(paste(deparse(surv_part), "~",
                                           paste(c(selected_vars, var), collapse = " + ")))
        }

        # Fit model and check improvement
        test_model <- survival::coxph(test_formula, data = data)

        # Compare models
        if (selectionCriteria == "aic") {
          # Use AIC for comparison
          current_aic <- AIC(current_model)
          test_aic <- AIC(test_model)
          improvement <- current_aic - test_aic

          if (improvement > best_improvement) {
            best_improvement <- improvement
            best_var <- var
          }
        } else {
          # Use likelihood ratio test
          lr_test <- anova(current_model, test_model)
          if (nrow(lr_test) >= 2) {
            p_value <- lr_test$P[2]  # Second row has the p-value for comparison

            if (!is.na(p_value) && p_value < best_p) {
              best_p <- p_value
              best_var <- var
              best_improvement <- 1  # Just a flag
            }
          }
        }
      }

      # If we found a variable to add
      if (!is.null(best_var) && (
        (selectionCriteria == "aic" && best_improvement > 0) ||
        (selectionCriteria == "lrt" && best_p < pEntry)
      )) {
        # Add best variable
        selected_vars <- c(selected_vars, best_var)

        # Update current model
        if (length(selected_vars) > 0) {
          current_formula <- as.formula(paste(deparse(surv_part), "~",
                                              paste(selected_vars, collapse = " + ")))
          current_model <- survival::coxph(current_formula, data = data)
        }

        # For stepwise, check if we should remove any variables
        if (modelSelection == "both" && length(selected_vars) > 1) {

          private$.checkpoint()

          # Get model summary
          model_summary <- summary(current_model)

          # Only check for removal if we have coefficients
          if (!is.null(model_summary$coefficients)) {
            # Check p-values of current variables
            coef_summary <- model_summary$coefficients
            var_p_values <- coef_summary[, "Pr(>|z|)"]

            # Find variables that are no longer significant
            remove_vars <- character(0)
            for (i in seq_along(var_p_values)) {
              if (var_p_values[i] > pRemoval) {
                remove_vars <- c(remove_vars, names(var_p_values)[i])
              }
            }

            if (length(remove_vars) > 0) {
              # Remove these variables
              selected_vars <- setdiff(selected_vars, remove_vars)

              # Update model
              if (length(selected_vars) > 0) {
                current_formula <- as.formula(paste(deparse(surv_part), "~",
                                                    paste(selected_vars, collapse = " + ")))
                current_model <- survival::coxph(current_formula, data = data)
              } else {
                current_model <- null_model
              }
            }
          }
        }
      } else {
        # No more variables to add
        break
      }
    }

    return(current_model)
  }
}

,
    ## Final Fit ----
.final_fit2 = function() {
  # Retrieve cleaned data and variable information
  cleaneddata <- private$.cleandata()

  # Extract necessary data components
  mydata <- cleaneddata$cleanData

  # Extract variable names
  myexplanatory <- NULL
  if (!is.null(self$options$explanatory)) {
    myexplanatory <- as.vector(cleaneddata$myexplanatory_labelled)
  }

  mycontexpl <- NULL
  if (!is.null(self$options$contexpl)) {
    mycontexpl <- as.vector(cleaneddata$mycontexpl_labelled)
  }

  # Prepare the formula for model fitting
  formula2 <- c(myexplanatory, mycontexpl)
  myformula <- paste("survival::Surv(mytime, myoutcome) ~ ", paste(formula2, collapse = " + "))
  myformula <- as.formula(myformula)


  private$.checkpoint()


  # Perform model selection
  model <- private$.fitModelWithSelection(myformula, mydata)

  private$.checkpoint()


  # Get model summary for statistics
  model_summary <- summary(model)


  self$results$mydataview_modelselection$setContent(
    list(
      mydata = head(mydata),
      mytime = cleaneddata$name1time,
      myexplanatory = myexplanatory,
      mycontexpl = mycontexpl,
      myformula = myformula,
      model = model,
      use_modelSelection = self$options$use_modelSelection,
      modelSelection = self$options$modelSelection,
      selectionCriteria = self$options$selectionCriteria,
      pEntry = self$options$pEntry,
      pRemoval = self$options$pRemoval
    )
    )




  # Create metrics text with comprehensive error handling
  metrics_text <- tryCatch({
    # Calculate statistics safely
    logtest_value <- if(is.null(model_summary$logtest) || length(model_summary$logtest) < 1 ||
                        !is.numeric(model_summary$logtest[1])) {
      NA  # Use NA if logtest is missing or invalid
    } else {
      model_summary$logtest[1]
    }

    loglik_value <- if(is.null(model$loglik) || length(model$loglik) < 1 ||
                       !is.numeric(model$loglik[1])) {
      NA  # Use NA if loglik is missing or invalid
    } else {
      model$loglik[1]
    }

    # Calculate R-squared safely
    r_squared <- if(is.na(logtest_value) || is.na(model$n) || model$n == 0) {
      NA  # Use NA if we can't calculate
    } else {
      round(1 - exp(-logtest_value/model$n), 3)
    }

    # Calculate max possible R-squared safely
    max_r_squared <- if(is.na(loglik_value) || is.na(model$n) || model$n == 0) {
      NA  # Use NA if we can't calculate
    } else {
      round(1 - exp(-2 * loglik_value/model$n), 3)
    }

    # Format metrics text with safe fallbacks
    paste0(
      "Number in dataframe = ", nrow(mydata),
      ", Number in model = ", ifelse(is.null(model$n), "Unknown", model$n),
      ", Missing = 0",  # You might want to calculate this if needed
      ", Number of events = ", ifelse(is.null(model$nevent), "Unknown", model$nevent),
      ", Concordance = ", ifelse(is.null(model_summary$concordance) || length(model_summary$concordance) < 1,
                                 "Unknown", round(model_summary$concordance[1], 3)),
      " (SE = ", ifelse(is.null(model_summary$concordance) || length(model_summary$concordance) < 2,
                        "Unknown", round(model_summary$concordance[2], 3)), ")",
      ", R-squared = ", ifelse(is.na(r_squared), "Unknown", r_squared),
      "( Max possible = ", ifelse(is.na(max_r_squared), "Unknown", max_r_squared), ")",
      ", Likelihood ratio test = ", ifelse(is.na(logtest_value), "Unknown", round(logtest_value, 3)),
      " (df = ", ifelse(is.null(model_summary$logtest) || length(model_summary$logtest) < 2,
                        "Unknown", model_summary$logtest[2]),
      ", p = ", ifelse(is.null(model_summary$logtest) || length(model_summary$logtest) < 3,
                       "Unknown", format.pval(model_summary$logtest[3], digits=3)), ")"
    )
  }, error = function(e) {
    # Fallback metrics if calculation fails
    paste0(
      "Number in dataframe = ", nrow(mydata),
      ", Number in model = ", ifelse(is.null(model$n), "Unknown", model$n),
      ", Number of events = ", ifelse(is.null(model$nevent), "Unknown", model$nevent),
      ", Note: Some model metrics could not be calculated."
    )
  })

  # Create the base model metrics text
  text2_model_selection <- glue::glue("
    <br>
    <b>Model Metrics:</b><br>
    {metrics_text}
    <br>
  ")

  # Add landmark information if used
  if (self$options$uselandmark) {
    landmark <- jmvcore::toNumeric(self$options$landmark)

    text2_model_selection <- glue::glue(
      text2_model_selection,
      "Landmark time used as: ",
      landmark,
      " ",
      self$options$timetypeoutput,
      "."
    )
  }

  # Add selection results to the output - only once
  if (self$options$modelSelection != "enter") {
    # Safely extract coefficient names
    coef_names <- tryCatch({
      if (!is.null(model$coefficients) && length(model$coefficients) > 0) {
        paste(names(model$coefficients), collapse = ", ")
      } else {
        "None"
      }
    }, error = function(e) {
      "Unknown (error extracting coefficients)"
    })

    # Use the selection method directly from options
    selection_method <- self$options$modelSelection

    text2_model_selection <- paste0(
      text2_model_selection,
      "\n<br><b>Model Selection Results:</b><br>",
      "Selection method: ", selection_method,  # Use the actual selection method
      "<br>Selection criteria: ", self$options$selectionCriteria,
      "<br>Variables in final model: ", coef_names
    )

    # Add note about stepwise selection
    text2_model_selection <- paste0(
      text2_model_selection,
      "<br><br>Note: Stepwise selection methods should be used with caution. They may not always select the most theoretically meaningful model and can lead to overfitting."
    )
  }

  # Set the content for the model metrics
  self$results$text2_model_selection$setContent(text2_model_selection)

  # Create a formatted table of hazard ratios with error handling
  hr_table <- tryCatch({
    private$.createHRTable(model)
  }, error = function(e) {
    # Create a simple fallback table if the main function fails
    data.frame(
      Variable = "Error creating hazard ratio table",
      "HR (multivariable)" = paste("Error:", e$message)
    )
  })

  # Convert to HTML table and set content
  text_model_selection <- knitr::kable(
    hr_table,
    row.names = FALSE,
    align = c('l', 'l'),
    format = "html"
  )

  # Set the content for the HR table
  self$results$text_model_selection$setContent(text_model_selection)
}

,
# Helper function to create HR table with error handling
.createHRTable = function(model) {
  # If model has no coefficients, return empty table
  if (is.null(model$coefficients) || length(model$coefficients) == 0) {
    return(data.frame(
      Variable = "No variables in model",
      "HR (multivariable)" = "N/A"
    ))
  }

  # Get model summary
  summary_model <- summary(model)

  # Extract coefficients, hazard ratios, and CIs safely
  tryCatch({
    coefs <- summary_model$coefficients
    confint <- summary_model$conf.int

    # Create data frame with variable names and hazard ratios
    hr_table <- data.frame(
      Variable = row.names(coefs),
      HR = round(confint[, 1], 2),
      Lower_CI = round(confint[, 3], 2),
      Upper_CI = round(confint[, 4], 2),
      P_value = format.pval(coefs[, 5], digits = 3)
    )

    # Format the HR with CI
    hr_table$HR_with_CI <- paste0(
      hr_table$HR, " (",
      hr_table$Lower_CI, "-",
      hr_table$Upper_CI, ", p=",
      hr_table$P_value, ")"
    )

    # Return simplified table
    final_table <- data.frame(
      Variable = hr_table$Variable,
      "HR (multivariable)" = hr_table$HR_with_CI
    )

    return(final_table)
  }, error = function(e) {
    # If something goes wrong, return a basic table with the error
    return(data.frame(
      Variable = names(model$coefficients),
      "HR (multivariable)" = "Error calculating hazard ratios"
    ))
  })
}

      # Survival Decision Tree Function ----
      ,
      .survivalTree = function(results) {
        # Skip if tree analysis not requested
        if (!self$options$use_tree) {
          return(NULL)
        }

        # Get cleaned data
        cleanData <- results$cleanData
        mytime <- results$name1time
        myoutcome <- results$name2outcome
        
        # Get explanatory variables
        expl_vars <- NULL
        if (!is.null(self$options$explanatory)) {
          expl_vars <- c(expl_vars, as.vector(results$myexplanatory_labelled))
        }
        if (!is.null(self$options$contexpl)) {
          expl_vars <- c(expl_vars, as.vector(results$mycontexpl_labelled))
        }
        
        # Check for explanatory variables
        if (length(expl_vars) == 0) {
          self$results$tree_summary$setContent(
            "<p>Error: At least one explanatory variable is required for decision tree analysis.</p>"
          )
          return(NULL)
        }

        private$.checkpoint()
        
        # Create formula for rpart
        formula <- paste("survival::Surv(", mytime, ",", myoutcome, ") ~ ", 
                         paste(expl_vars, collapse = " + "))
        formula <- as.formula(formula)
        
        # Fit survival tree using rpart
        tree <- rpart::rpart(
          formula = formula,
          data = cleanData,
          method = "exp",  # exponential survival model
          control = rpart::rpart.control(
            minsplit = 2 * self$options$min_node, 
            minbucket = self$options$min_node,
            cp = self$options$complexity,
            maxdepth = self$options$max_depth
          )
        )
        
        # Create summary text
        if (is.null(tree)) {
          tree_text <- "The survival tree could not be built with the current parameters."
        } else {
          # Get variable importance
          var_imp <- tree$variable.importance
          if (!is.null(var_imp)) {
            var_imp_df <- data.frame(
              Variable = names(var_imp),
              Importance = var_imp
            )
            var_imp_df <- var_imp_df[order(-var_imp_df$Importance), ]
            
            var_imp_html <- paste(
              "<tr>",
              "<td>", var_imp_df$Variable, "</td>",
              "<td>", round(var_imp_df$Importance, 2), "</td>",
              "</tr>", 
              collapse = ""
            )
            
            var_imp_table <- paste0(
              "<table class='jmv-results-table'>",
              "<thead><tr><th>Variable</th><th>Importance</th></tr></thead>",
              "<tbody>", var_imp_html, "</tbody>",
              "</table>"
            )
          } else {
            var_imp_table <- "<p>No variable importance measures available.</p>"
          }
          
          # Get tree statistics
          n_terminal <- sum(tree$frame$var == "<leaf>")
          
          tree_text <- paste0(
            "<h3>Survival Decision Tree Results</h3>",
            "<p>The decision tree identified ", n_terminal, " terminal nodes based on the input variables.</p>",
            "<p>Complexity parameter: ", self$options$complexity, "</p>",
            "<p>Minimum node size: ", self$options$min_node, "</p>",
            "<p>Maximum depth: ", self$options$max_depth, "</p>",
            "<h4>Variable Importance</h4>",
            var_imp_table,
            "<p><i>Note: The decision tree plot visualizes how the variables split the data into groups with different survival outcomes.</i></p>"
          )
        }
        
        self$results$tree_summary$setContent(tree_text)
        
        # Store tree for plotting
        return(tree)
      }

      # Plot Tree ----
      ,
      .plotTree = function(image, ggtheme, theme, ...) {
        # Skip if tree analysis not requested
        if (!self$options$use_tree) {
          return(FALSE)
        }
        
        # Get results and tree
        results <- private$.cleandata()
        tree <- private$.survivalTree(results)
        
        if (is.null(tree)) {
          return(FALSE)
        }
        
        # Plot tree
        rpart.plot::rpart.plot(
          tree,
          main = "Survival Decision Tree",
          extra = 101,  # show fitted risk and percentage of observations
          box.palette = "auto",  # color by fitted risk
          shadow.col = "gray",  # add shadows to the boxes
          nn = TRUE,  # show node numbers
          fallen.leaves = TRUE,  # align leaf nodes
          roundint = FALSE  # don't round integers
        )
        
        return(TRUE)
      }

      # Plot Node Survival ----
      ,
      .plotNodeSurvival = function(image, ggtheme, theme, ...) {
        # Skip if not requested
        if (!self$options$use_tree || !self$options$show_terminal_nodes) {
          return(FALSE)
        }
        
        # Get results and tree
        results <- private$.cleandata()
        tree <- private$.survivalTree(results)
        
        if (is.null(tree)) {
          return(FALSE)
        }
        
        # Get cleaned data
        cleanData <- results$cleanData
        mytime <- results$name1time
        myoutcome <- results$name2outcome
        
        # Get terminal node assignments for each observation
        node_assignments <- tree$where
        cleanData$node <- paste("Node", node_assignments)
        
        # Plot survival curves for each terminal node
        formula <- paste("survival::Surv(", mytime, ",", myoutcome, ") ~ node")
        formula <- as.formula(formula)
        
        fit <- survival::survfit(formula, data = cleanData)
        
        plot <- survminer::ggsurvplot(
          fit,
          data = cleanData,
          risk.table = TRUE,
          pval = TRUE,
          conf.int = TRUE,
          xlab = paste0("Time (", self$options$timetypeoutput, ")"),
          ylab = "Survival probability",
          title = "Survival by Terminal Node",
          legend.title = "Terminal Node",
          risk.table.height = 0.25
        )
        
        print(plot)
        return(TRUE)
      }

      # Convert Wide Format to Long Format for Time-Dependent Covariates ----
      ,
      .convertWideToLong = function(mydata, time_dep_vars, all_labels) {
        
        # Get change time points
        change_times <- self$options$change_times
        if (is.null(change_times) || change_times == "") {
          change_times <- "6, 12, 18"
        }
        
        # Parse change times
        time_points <- as.numeric(trimws(strsplit(change_times, ",")[[1]]))
        time_points <- sort(time_points)
        
        # Get suffix pattern
        suffix_pattern <- self$options$td_suffix_pattern
        if (is.null(suffix_pattern) || suffix_pattern == "") {
          suffix_pattern <- "_t{time}"
        }
        
        # Initialize long format data
        long_data <- data.frame()
        
        for (i in 1:nrow(mydata)) {
          subject_data <- mydata[i, ]
          
          # Get subject's total follow-up time
          total_time <- subject_data$mytime
          
          # Create time intervals: 0, change_times, total_time
          intervals <- c(0, time_points[time_points < total_time], total_time)
          intervals <- unique(sort(intervals))
          
          # If subject has very short follow-up, create just one interval
          if (length(intervals) < 2) {
            intervals <- c(0, total_time)
          }
          
          # Create rows for each interval
          for (j in 1:(length(intervals)-1)) {
            tstart <- intervals[j]
            tstop <- intervals[j+1]
            
            # Status is 1 only in the last interval if subject has event
            status <- ifelse(j == (length(intervals)-1), subject_data$myoutcome, 0)
            
            # Create new row
            new_row <- subject_data
            new_row$tstart <- tstart
            new_row$tstop <- tstop
            new_row$myoutcome <- status
            
            # Update time-dependent variables for this interval
            for (var in time_dep_vars) {
              
              # Determine which time-dependent version to use
              if (tstart == 0) {
                # Use baseline version for first interval
                baseline_var <- paste0(var, "_baseline")
                if (baseline_var %in% names(mydata)) {
                  new_row[[var]] <- subject_data[[baseline_var]]
                } else {
                  # If no baseline version, use the base variable
                  new_row[[var]] <- subject_data[[var]]
                }
              } else {
                # Find appropriate time-dependent version
                applicable_times <- time_points[time_points <= tstart]
                if (length(applicable_times) > 0) {
                  use_time <- max(applicable_times)
                  td_var_name <- gsub("\\{time\\}", use_time, suffix_pattern)
                  full_var_name <- paste0(var, td_var_name)
                  
                  if (full_var_name %in% names(mydata)) {
                    new_row[[var]] <- subject_data[[full_var_name]]
                  } else {
                    # Fall back to previous value or baseline
                    new_row[[var]] <- subject_data[[var]]
                  }
                } else {
                  # Use baseline if no applicable time found
                  new_row[[var]] <- subject_data[[var]]
                }
              }
            }
            
            long_data <- rbind(long_data, new_row)
          }
        }
        
        return(long_data)
      }








)
  )
