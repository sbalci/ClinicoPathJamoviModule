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
                    "mymycontexpl_labelled" = mycontexpl
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



                    timetypeoutput <-
                        jmvcore::constructFormula(terms = self$options$timetypeoutput)


                    mydata <- mydata %>%
                        dplyr::mutate(interval = lubridate::interval(start, end))

                    stopifnot(lubridate::is.interval(mydata[["interval"]]))

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
            mymycontexpl_labelled <- labelled_data$mymycontexpl_labelled

            mydata <- mydata_labelled

            mydata[["myfactor"]] <- mydata[[mymycontexpl_labelled]]


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
            mymycontexpl_labelled <- labelled_data$mymycontexpl_labelled

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

                if (!is.null(self$options$contexpl)
                    ) {
                    name3contexpl <- mymycontexpl_labelled
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
                        "mymycontexpl_labelled" = mymycontexpl_labelled
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
                condition3 <- !is.null(self$options$contexpl)

                condition1 <- subcondition1a || (subcondition1b1 && (subcondition1b2 || subcondition1b3 || subcondition1b4 || subcondition1b5))

                condition2 <- subcondition2a || (subcondition2b1 && subcondition2b2 && subcondition2b3)

                if (!(condition1 && condition2 && condition3)) {
                    private$.todo()
                    return()
                } else {
                  self$results$todo$setVisible(FALSE)
                }


                ## Empty data ----

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                # Get Clean Data ----
                results <- private$.cleandata()

                # Run Analysis ----

                ## Run Continious Cox Regression ----
                private$.cox(results)

                ## Run Cut-off calculation and further analysis ----
                if (!self$options$findcut) {
                    return()
                }


                ## Run Cut-off calculation ----
                # res.cut <- private$.cutoff(results)

                ## Run Cut-off Table ----
                # private$.cutoffTable(res.cut)

                ## Run Categorise Data ----
                # cutoffdata <- private$.cutoff2(res.cut)


                ## Add Calculated Time to Data ----

                # if (self$options$findcut && self$results$calculatedcutoff$isNotFilled()) {
                #     # Set rownames ----

                #     rowNums <- rownames(mydata)
                #     self$results$calculatedcutoff$setRowNums(rowNums)

                #     # Add calculatedcutoff to Data ----

                #     cutoffgr <- cutoffdata[[self$options$contexpl]]

                #     if (self$options$calculatedcutoff &&
                #         self$results$calculatedcutoff$isNotFilled()) {
                #         self$results$calculatedcutoff$setValues(cutoffgr)
                #     }
                # }


                ## Run median cutoff ----

                # private$.mediancutoff(cutoffdata)

                ## Run life table cutoff ----

                # private$.lifetablecutoff(cutoffdata)

                # Prepare Data For Plots ----

                # plotData1 <- res.cut
                # image4 <- self$results$plot4
                # image4$setState(plotData1)

                # plotData2 <- cutoffdata

                # image5 <- self$results$plot5
                # image5$setState(plotData2)

                # image2 <- self$results$plot2
                # image2$setState(plotData2)

                # image3 <- self$results$plot3
                # image3$setState(plotData2)

                # image6 <- self$results$plot6
                # image6$setState(plotData2)




                # # Add Calculated Time to Data ----

                # # self$results$mydataview$setContent(
                # #     list(
                # #         results
                # #     )
                # # )


                # if (self$options$tint && self$options$calculatedtime && self$results$calculatedtime$isNotFilled()) {
                #     self$results$calculatedtime$setRowNums(results$cleanData$row_names)
                #     self$results$calculatedtime$setValues(results$cleanData$CalculatedTime)
                # }


                # # Add Redefined Outcome to Data ----

                # if (self$options$multievent  && self$options$outcomeredifened && self$results$outcomeredifened$isNotFilled()) {
                #     self$results$outcomeredifened$setRowNums(results$cleanData$row_names)
                #     self$results$outcomeredifened$setValues(results$cleanData$CalculatedOutcome)
                # }
            }





            # Continious Cox Regression ----
            ,
            .cox = function(results) {

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


                # https://rpkgs.datanovia.com/survminer/reference/surv_cutpoint.html

                res.cut <- survminer::surv_cutpoint(
                    mydata,
                    time = "mytime",
                    event = "myoutcome",
                    self$options$contexpl,
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

                rescutTable$setTitle(paste0(self$options$contexpl))

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
                mydata <- cutoffdata

                # Median Survival Table ----

                thefactor <-
                    jmvcore::constructFormula(terms = self$options$contexpl)


                formula <-
                    paste("survival::Surv(mytime, myoutcome) ~ ", thefactor)
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

                names(results1table)[1] <- "factor"

                medianTable <- self$results$medianTable
                data_frame <- results1table
                for (i in seq_along(data_frame[, 1, drop = T])) {
                    medianTable$addRow(rowKey = i, values = c(data_frame[i, ]))
                }


                # Median Survival Summary ----

                results1table %>%
                    dplyr::mutate(
                        description =
                            glue::glue(
                                "When {factor}, median survival is {round(median, digits = 1)} [{round(x0_95lcl, digits = 1)} - {round(x0_95ucl, digits = 1)}, 95% CI] ",
                                self$options$timetypeoutput,
                                "."
                            )
                    ) %>%
                    dplyr::mutate(description = gsub(
                        pattern = "=",
                        replacement = " is ",
                        x = description
                    )) %>%
                    dplyr::select(description) %>%
                    dplyr::pull(.) -> km_fit_median_definition

                medianSummary <- km_fit_median_definition


                self$results$medianSummary$setContent(medianSummary)
            }


            # Life Table ----
            ,
            .lifetablecutoff = function(cutoffdata) {
                mydata <- cutoffdata


                # survival table 1,3,5-yr survival ----

                utimes <- self$options$cutp

                utimes <- strsplit(utimes, ",")
                utimes <- purrr::reduce(utimes, as.vector)
                utimes <- as.numeric(utimes)

                if (length(utimes) == 0) {
                    utimes <- c(12, 36, 60)
                }


                thefactor <-
                    jmvcore::constructFormula(terms = self$options$contexpl)


                formula <-
                    paste("survival::Surv(mytime, myoutcome) ~ ", thefactor)
                formula <- as.formula(formula)

                km_fit <- survival::survfit(formula, data = mydata)



                km_fit_summary <- summary(km_fit, times = utimes)

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


                survTable <- self$results$survTable

                data_frame <- km_fit_df
                for (i in seq_along(data_frame[, 1, drop = T])) {
                    survTable$addRow(rowKey = i, values = c(data_frame[i, ]))
                }




                # survTableSummary 1,3,5-yr survival summary ----

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

            # Cut-off Plot ----
            ,
            .plot4 = function(image4, ggtheme, theme, ...) {
                plotData <- image4$state

                res.cut <- plotData

                plot4 <-
                    plot(res.cut, self$options$contexpl, palette = "npg")

                print(plot4)
                TRUE
            }


            # Survival Curve with new cut-off ----
            ,
            .plot5 = function(image5, ggtheme, theme, ...) {
                plotData <- image5$state

                res.cat <- plotData

                contfactor <-
                    jmvcore::constructFormula(terms = self$options$contexpl)

                myformula <-
                    paste0("survival::Surv(mytime, myoutcome) ~ ", contfactor)

                myformula <- as.formula(myformula)

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
                plotData <- image2$state

                contfactor <-
                    jmvcore::constructFormula(terms = self$options$contexpl)

                myformula <- paste0("survival::Surv(mytime, myoutcome)")

                # myformula <- as.formula(myformula)

                title2 <- as.character(contfactor)

                plot2 <- plotData %>%
                    finalfit::surv_plot(
                        .data = .,
                        dependent = myformula,
                        explanatory = contfactor,
                        xlab = paste0("Time (", self$options$timetypeoutput, ")"),
                        # pval = TRUE,
                        legend = "none",
                        break.time.by = self$options$byplot,
                        xlim = c(0, self$options$endplot),
                        title = paste0("Cumulative Events ", title2),
                        fun = "event",
                        risk.table = self$options$risktable,
                        conf.int = self$options$ci95
                    )


                print(plot2)
                TRUE
            }



            # Cumulative Hazard with new cut-off ----
            ,
            .plot3 = function(image3, ggtheme, theme, ...) {
                ch <- self$options$ch

                if (!ch) {
                    return()
                }

                plotData <- image3$state

                contfactor <-
                    jmvcore::constructFormula(terms = self$options$contexpl)

                myformula <-
                    paste0("survival::Surv(mytime, myoutcome)")

                # myformula <- as.formula(myformula)

                title2 <- as.character(contfactor)

                plot3 <- plotData %>%
                    finalfit::surv_plot(
                        .data = .,
                        dependent = myformula,
                        explanatory = contfactor,
                        xlab = paste0("Time (", self$options$timetypeoutput, ")"),
                        # pval = TRUE,
                        legend = "none",
                        break.time.by = self$options$byplot,
                        xlim = c(0, self$options$endplot),
                        title = paste0("Cumulative Hazard ", title2),
                        fun = "cumhaz",
                        risk.table = self$options$risktable,
                        conf.int = self$options$ci95
                    )


                print(plot3)
                TRUE
            }


            # KMunicate Style with new cut-off ----
            ,
            .plot6 = function(image6, ggtheme, theme, ...) {
                kmunicate <- self$options$kmunicate

                if (!kmunicate) {
                    return()
                }

                plotData <- image6$state

                contfactor <-
                    jmvcore::constructFormula(terms = self$options$contexpl)

                myformula <-
                    paste0("survival::Surv(mytime, myoutcome) ~ ", contfactor)

                myformula <- as.formula(myformula)

                km_fit <- survival::survfit(myformula, data = plotData)

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
        )
    )
}
