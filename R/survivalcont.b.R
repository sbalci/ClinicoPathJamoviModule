#' @title Survival Analysis for Continuous Explanatory Variable
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr

survivalcontClass <- if (requireNamespace("jmvcore")) {
    R6::R6Class(
        "survivalcontClass",
        inherit = survivalcontBase,
        private = list(


            # init ----
            .init = function() {
                if (!self$options$findcut) {
                    # Disable other tables
                    self$results$medianSummary$setVisible(FALSE)
                    self$results$medianTable$setVisible(FALSE)
                    self$results$survTableSummary$setVisible(FALSE)
                    self$results$survTable$setVisible(FALSE)
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
                    "mycontexpl_labelled" = mycontexpl
                ))
            }


            # todo ----
            ,
            .todo = function() {
                todo <- glue::glue(
                    "
                <br>Welcome to ClinicoPath
                <br><br>
                This tool will help you calculate a cut-off for a continuous variable based on survival outcome.
                <br>
                After the cut-off is determined median survivals and 1,3,5-yr survivals are calculated.
                <br><br>
                Explanatory variable is continuous.
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


                    if ( sum(!is.na(mydata[["start"]])) == 0 || sum(!is.na(mydata[["end"]])) == 0)  {
                        stop(paste0("Time difference cannot be calculated. Make sure that time type in variables are correct. Currently it is: ", self$options$timetypedata)
                        )
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
            mycontexpl_labelled <- labelled_data$mycontexpl_labelled

            mydata <- mydata_labelled

            mydata[["myfactor"]] <- mydata[[mycontexpl_labelled]]


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
                condition3 <- !is.null(self$options$contexpl)

                condition1 <- subcondition1a && !subcondition1b1 || subcondition1b1 && subcondition1b2 || subcondition1b1 && subcondition1b3

                condition2 <- subcondition2b1 && subcondition2b2 && subcondition2b3 || subcondition2a && !subcondition2b1 && !subcondition2b2 && !subcondition2b3

                not_continue_analysis <- !(condition1 && condition2 && condition3)


                if (not_continue_analysis) {
                    private$.todo()
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
                    return()
                } else {
                  self$results$todo$setVisible(FALSE)
                }


                ## Empty data ----

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')


                private$.checkpoint()

                # Get Clean Data ----
                results <- private$.cleandata()

                # Run Analysis ----

                ## Run Continious Cox Regression ----
                private$.cox(results)



                ## Add the person-time analysis ----
                private$.checkpoint()  # Add checkpoint here

                # Run person-time analysis if enabled
                if (self$options$person_time) {
                    private$.personTimeAnalysis(results)
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

                ## Run Multiple Cut-offs Analysis ----
                if (self$options$multiple_cutoffs) {
                    multicut_results <- private$.multipleCutoffs(results)
                    private$.multipleCutoffTables(multicut_results)
                    
                    # Add multiple cutoff groups to data
                    if (self$options$calculatedmulticut &&
                        self$results$calculatedmulticut$isNotFilled()) {
                        self$results$calculatedmulticut$setValues(multicut_results$risk_groups)
                    }
                }







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
                    results = results
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

            }





            # Continious Cox Regression ----
            ,
            .cox = function(results) {

                private$.checkpoint()

                ## Cox Regression ----

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
                ) -> tCox


                tCoxtext2 <- glue::glue(
                    "
                                <br>
                                <b>Model Metrics:</b>
                                  ",
                    unlist(tCox[[2]]),
                    "
                                <br>
                                "
                )


                if (self$options$uselandmark) {
                    landmark <- jmvcore::toNumeric(self$options$landmark)

                    tCoxtext2 <- glue::glue(
                        tCoxtext2,
                        "Landmark time used as: ",
                        landmark, " ",
                        self$options$timetypeoutput, "."
                    )
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

                tCox_df %>%
                    dplyr::group_by(Explanatory) %>%
                    dplyr::mutate(firstlevel = dplyr::first(Levels)) %>%
                    dplyr::mutate(
                        coxdescription = glue::glue(
                            "When {Explanatory} increases 1 unit, the hazard increases {HR_multivariable} times."
                        )
                    ) %>%
                    dplyr::filter(HR_univariable != "-") %>%
                    dplyr::pull(coxdescription) -> coxSummary



                coxSummary <- unlist(coxSummary)
                self$results$coxSummary$setContent(coxSummary)
            }



                # Continuous Optimal Cut-off ----
            ,
            .cutoff = function(results) {


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

            }

            # Cut-off Table ----
            ,
            .cutoffTable = function(res.cut) {
                rescut_summary <- summary(res.cut)

                rescutTable <- self$results$rescutTable

                rescutTable$setTitle(paste0(
                  "Optimal Cutpoint Analysis for ", self$options$contexpl,
                  " \n The cutpoint maximizes the statistical difference between groups while maintaining sufficient sample sizes"
                ))



                # rescutTable$setTitle(paste0(self$options$contexpl))

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


                formula <-
                    paste('survival::Surv(',
                          mytime,
                          ',',
                          myoutcome,
                          ') ~ ',
                          mycontexpl)

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

                results2table$factor <- gsub(pattern = paste0(mycontexpl,"="),
                                             replacement = "",
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


                # survival table 1,3,5-yr survival ----

                utimes <- self$options$cutp

                utimes <- strsplit(utimes, ",")
                utimes <- purrr::reduce(utimes, as.vector)
                utimes <- as.numeric(utimes)

                if (length(utimes) == 0) {
                    utimes <- c(12, 36, 60)
                }

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


                formula <-
                    paste('survival::Surv(',
                          mytime,
                          ',',
                          myoutcome,
                          ') ~ ',
                          mycontexpl)

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
                                             replacement = "",
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
                            glue::glue(
                                "When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]."
                            )
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


                formula <-
                    paste('survival::Surv(',
                          mytime,
                          ',',
                          myoutcome,
                          ') ~ ',
                          mycontexpl)

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
                mytime <- results$name1time
                myoutcome <- results$name2outcome
                mycontexpl <- results$name3contexpl
                mydata <- results$cleanData
                
                # Convert to numeric
                mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])
                
                # Extract continuous variable values
                cont_var <- mydata[[mycontexpl]]
                cont_var <- cont_var[!is.na(cont_var)]
                
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
                
                # Create risk groups
                risk_groups <- private$.createRiskGroups(mydata[[mycontexpl]], cutoff_values)
                
                # Calculate survival statistics for each group
                group_stats <- private$.calculateGroupStats(mydata, mytime, myoutcome, risk_groups)
                
                return(list(
                    cutoff_values = cutoff_values,
                    risk_groups = risk_groups,
                    group_stats = group_stats,
                    method = self$options$cutoff_method,
                    num_cuts = num_cuts
                ))
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
                    # Prepare survival formula
                    formula_str <- paste0("survival::Surv(", mytime, ", ", myoutcome, ") ~ ", mycontexpl)
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
                        formula_str <- paste0("survival::Surv(", mytime, ", ", myoutcome, ") ~ test_groups")
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
                } else if (length(cutoffs) == 3) {
                    groups <- ifelse(cont_var <= cutoffs[1], "Low Risk",
                                   ifelse(cont_var <= cutoffs[2], "Medium-Low Risk",
                                         ifelse(cont_var <= cutoffs[3], "Medium-High Risk", "High Risk")))
                } else if (length(cutoffs) == 4) {
                    groups <- ifelse(cont_var <= cutoffs[1], "Very Low Risk",
                                   ifelse(cont_var <= cutoffs[2], "Low Risk",
                                         ifelse(cont_var <= cutoffs[3], "Medium Risk",
                                               ifelse(cont_var <= cutoffs[4], "High Risk", "Very High Risk"))))
                }
                
                return(factor(groups, levels = unique(groups[order(cont_var)])))
            }

            # Calculate survival statistics by group ----
            ,
            .calculateGroupStats = function(mydata, mytime, myoutcome, risk_groups) {
                stats_list <- list()
                
                for (group in levels(risk_groups)) {
                    group_data <- mydata[risk_groups == group, ]
                    
                    if (nrow(group_data) > 0) {
                        # Calculate median survival
                        formula_str <- paste0("survival::Surv(", mytime, ", ", myoutcome, ") ~ 1")
                        formula <- as.formula(formula_str)
                        
                        km_fit <- survival::survfit(formula, data = group_data)
                        
                        stats_list[[group]] <- list(
                            group = group,
                            n = nrow(group_data),
                            events = sum(group_data[[myoutcome]], na.rm = TRUE),
                            median_surv = summary(km_fit)$table["median"],
                            median_lower = summary(km_fit)$table["0.95LCL"],
                            median_upper = summary(km_fit)$table["0.95UCL"]
                        )
                    }
                }
                
                return(stats_list)
            }

            # Populate multiple cutoffs tables ----
            ,
            .multipleCutoffTables = function(multicut_results) {
                # Populate cut-off points table
                cutoff_table <- self$results$multipleCutTable
                for (i in seq_along(multicut_results$cutoff_values)) {
                    cutoff_table$addRow(rowKey = i, values = list(
                        cutpoint_number = i,
                        cutpoint_value = multicut_results$cutoff_values[i],
                        group_created = paste("Cut", i),
                        logrank_statistic = NA,  # Will be calculated separately
                        p_value = NA
                    ))
                }
                
                # Populate median survival table
                median_table <- self$results$multipleMedianTable
                for (group_name in names(multicut_results$group_stats)) {
                    stats <- multicut_results$group_stats[[group_name]]
                    median_table$addRow(rowKey = group_name, values = list(
                        risk_group = stats$group,
                        n_patients = stats$n,
                        events = stats$events,
                        median_survival = stats$median_surv,
                        median_lower = stats$median_lower,
                        median_upper = stats$median_upper
                    ))
                }
                
                # TODO: Add survival estimates table population
                # This would require calculating survival at specific time points
            }

            # Multiple cutoffs visualization ----
            ,
            .plotMultipleCutoffs = function(image, ggtheme, theme, ...) {
                if (!self$options$multiple_cutoffs) {
                    return()
                }
                
                # Placeholder for multiple cutoffs visualization
                # This would show the cutoff points on the continuous variable distribution
                plot <- ggplot2::ggplot() + 
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "Multiple Cutoffs Visualization\n(Implementation in progress)"),
                                      size = 6) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::theme_void()
                
                print(plot)
                TRUE
            }

            # Multiple cutoffs survival plot ----
            ,
            .plotMultipleSurvival = function(image, ggtheme, theme, ...) {
                if (!self$options$multiple_cutoffs || !self$options$sc) {
                    return()
                }
                
                # Placeholder for multiple cutoffs survival plot
                plot <- ggplot2::ggplot() + 
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "Multiple Cutoffs Survival Plot\n(Implementation in progress)"),
                                      size = 6) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::theme_void()
                
                print(plot)
                TRUE
            }
        )
    )
}
