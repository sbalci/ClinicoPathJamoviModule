#' @title Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr
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


                # if (self$options$sas) {
                #     # Disable tables
                #     self$results$medianSummary$setVisible(FALSE)
                #     self$results$medianTable$setVisible(FALSE)
                #     self$results$coxSummary$setVisible(FALSE)
                #     self$results$coxTable$setVisible(FALSE)
                #     self$results$tCoxtext2$setVisible(FALSE)
                #     self$results$survTableSummary$setVisible(FALSE)
                #     self$results$survTable$setVisible(FALSE)
                #     self$results$pairwiseSummary$setVisible(FALSE)
                #     self$results$pairwiseTable$setVisible(FALSE)
                # }
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



                    timetypeoutput <-
                        jmvcore::constructFormula(terms = self$options$timetypeoutput)


                    mydata <- mydata %>%
                        dplyr::mutate(interval = lubridate::interval(start, end))

                    stopifnot(lubridate::is.interval(mydata[["interval"]]))

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

                if (self$options$tint) {
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

                ## No variable TODO ----

                ## Define subconditions ----

                subcondition1a <- !is.null(self$options$outcome)
                subcondition1b1 <- !is.null(self$options$multievent)
                subcondition1b2 <- !is.null(self$options$dod)
                subcondition1b3 <- !is.null(self$options$dooc)
                subcondition1b4 <- !is.null(self$options$awd)
                subcondition1b5 <- !is.null(self$options$awod)
                subcondition2a <- !is.null(self$options$elapsedtime)
                subcondition2b1 <- !is.null(self$options$tint)
                subcondition2b2 <- !is.null(self$options$dxdate)
                subcondition2b3 <- !is.null(self$options$fudate)
                condition3 <- !is.null(self$options$explanatory)

                condition1 <- subcondition1a || (subcondition1b1 && (subcondition1b2 || subcondition1b3 || subcondition1b4 || subcondition1b5))

                condition2 <- subcondition2a || (subcondition2b1 && subcondition2b2 && subcondition2b3)

                if (!(condition1 && condition2 && condition3)) {
                    private$.todo()
                    return()
                } else {
                  self$results$todo$setVisible(FALSE)
                }


                # Empty data ----

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                # Get Clean Data ----
                results <- private$.cleandata()

                # Run Analysis ----
                ## Median Survival ----
                    private$.medianSurv(results)
                ## Cox ----
                    private$.cox(results)
                ## Survival Table ----
                    private$.survTable(results)


                ## Pairwise ----
                if (self$options$pw
                    # && !self$options$sas
                    ) {
                    private$.pairwise(results)
                }

                # Add Calculated Time to Data ----

                # self$results$mydataview$setContent(
                #     list(
                #         results
                #     )
                # )


                if (self$options$tint && self$options$calculatedtime && self$results$calculatedtime$isNotFilled()) {
                    self$results$calculatedtime$setRowNums(results$cleanData$row_names)
                    self$results$calculatedtime$setValues(results$cleanData$CalculatedTime)
                }


                # Add Redefined Outcome to Data ----

                if (self$options$multievent  && self$options$outcomeredifened && self$results$outcomeredifened$isNotFilled()) {
                    self$results$outcomeredifened$setRowNums(results$cleanData$row_names)
                    self$results$outcomeredifened$setValues(results$cleanData$CalculatedOutcome)
                }
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

                km_fit <- survival::survfit(formula, data = mydata)

                km_fit_median_df <- summary(km_fit)


                # medianSummary2 <-
                #   as.data.frame(km_fit_median_df$table)
                # self$results$medianSummary2$setContent(medianSummary2)



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


                # self$results$medianSummary2$setContent(results2table)


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

                medianSummary <- km_fit_median_definition


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

                finalfit::finalfit(
                    .data = mydata,
                    dependent = myformula,
                    explanatory = myfactor,
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
                            "When {Explanatory} is {Levels}, there is {HR_univariable} times risk than when {Explanatory} is {firstlevel}."
                        )
                    ) %>%
                    dplyr::filter(HR_univariable != '-') %>%
                    dplyr::pull(coxdescription) -> coxSummary



                coxSummary <- unlist(coxSummary)
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

                    cox_model <- survival::coxph(formula, data = mydata)

                    zph <- survival::cox.zph(cox_model)

                    self$results$cox_ph$setContent(print(zph))

                    image7 <- self$results$plot7
                    image7$setState(zph)

                    }

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

                km_fit <- survival::survfit(formula, data = mydata)

                utimes <- self$options$cutp

                utimes <- strsplit(utimes, ",")
                utimes <- purrr::reduce(utimes, as.vector)
                utimes <- as.numeric(utimes)

                if (length(utimes) == 0) {
                    utimes <- c(12, 36, 60)
                }

                km_fit_summary <- summary(km_fit, times = utimes)

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
                    replacement = paste0(self$options$explanatory, " "),
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
                                "When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]."
                            )
                    ) %>%
                    dplyr::select(description) %>%
                    dplyr::pull(.) -> survTableSummary

                self$results$survTableSummary$setContent(survTableSummary)

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
                                " has a p-value of {format.pval(value, digits = 3, eps = 0.001)}."
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


            # Survival Curve ----
            ,
            .plot = function(image, ggtheme, theme, ...) {
                sc <- self$options$sc

                if (!sc)
                    return()

                results <- image$state

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
                        censor = self$options$censored
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
                        censored = self$options$censored
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
                        censored = self$options$censored
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


            # cox.zph ----
            ,
            .plot7 = function(image7, ggtheme, theme, ...) {

                ph_cox <- self$options$ph_cox

                if (!ph_cox)
                    return()

                zph <- image7$state

                plot7 <- plot(zph)

                print(plot7)
                TRUE

            }

        )
    )
