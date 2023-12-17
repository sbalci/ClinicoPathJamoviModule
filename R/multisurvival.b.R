#' @title Multivariable Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'

multisurvivalClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "multisurvivalClass",
        inherit = multisurvivalBase,
        private = list(


            # init ----
            .init = function() {

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


                labels_explanatory <- self$options$explanatory

                myexplanatory <-
                    names(all_labels)[match(labels_explanatory,
                                            all_labels)]

                labels_contexpl <- self$options$contexpl

                mycontexpl <-
                    names(all_labels)[match(labels_contexpl,
                                            all_labels)]

                return(list(
                    "mydata_labelled" = mydata,
                    "mytime_labelled" = mytime,
                    "myoutcome_labelled" = myoutcome,
                    "mydxdate_labelled" = mydxdate,
                    "myfudate_labelled" = myfudate,
                    "mycontexpl_labelled" = mycontexpl,
                    "myexplanatory_labelled" = myexplanatory
                ))
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
                myexplanatory_labelled <- labelled_data$myexplanatory_labelled
                mycontexpl_labelled <- labelled_data$mycontexpl_labelled

                mydata <- mydata_labelled

                df_factor <- mydata %>%
                    jmvcore::select(c("row_names",
                                      myexplanatory_labelled,
                                      mycontexpl_labelled
                                      )
                                      )

                # self$results$mydataview$setContent(
                #     list(
                #         df_factor = head(df_factor),
                #         myexplanatory_labelled = myexplanatory_labelled,
                #         mycontexpl_labelled = mycontexpl_labelled,
                #         mydata = head(mydata)
                #     )
                # )

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
                mycontexpl_labelled <- labelled_data$mycontexpl_labelled

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

                if (self$options$tint) {
                    name2outcome <- "CalculatedOutcome"
                }

                if (!is.null(self$options$explanatory)
                ) {
                    name3expl <- myexplanatory_labelled
                }


                if (!is.null(self$options$contexpl)
                ) {
                    name3contexpl <- mycontexpl_labelled
                }


                # cleanData <- cleanData %>%
                #     dplyr::rename(
                #         !!name1time := mytime,
                #         !!name2outcome := myoutcome,
                #         !!name3contexpl := myfactor
                #     )

                # naOmit ----

                cleanData <- jmvcore::naOmit(cleanData)

                # Return Data ----

                return(
                    list(
                        "name1time" = name1time,
                        "name2outcome" = name2outcome,
                        "name3contexpl" = name3contexpl,
                        "name3expl" = name3expl,
                        "cleanData" = cleanData,
                        "mytime_labelled" = mytime_labelled,
                        "myoutcome_labelled" = myoutcome_labelled,
                        "mydxdate_labelled" = mydxdate_labelled,
                        "myfudate_labelled" = myfudate_labelled,
                        "myexplanatory_labelled" = myexplanatory_labelled,
                        "mycontexpl_labelled" = mycontexpl_labelled
                    )
                )


                # self$results$mydataview$setContent(
                #     list(
                #         # labelled_data = head(labelled_data),
                #         # time = head(time),
                #         # outcome = head(outcome),
                #         # factor = head(factor),
                #         mydata_labelled = head(mydata_labelled),
                #         mytime_labelled = mytime_labelled,
                #         myoutcome_labelled = myoutcome_labelled,
                #         mydxdate_labelled = mydxdate_labelled,
                #         myfudate_labelled = myfudate_labelled,
                #         myexplanatory_labelled = myexplanatory_labelled,
                #         mycontexpl_labelled = mycontexpl_labelled,
                #         # cleanData = head(cleanData),
                #         name1time = name1time,
                #         name2outcome = name2outcome,
                #         name3expl = name3expl,
                #         name3contexpl = name3contexpl
                #     )
                # )

            }



           # run  ----
            ,
            .run = function() {

                # Errors, Warnings ----

                ## No variable todo ----

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
                condition3a <- !is.null(self$options$contexpl)
                condition3b <- !is.null(self$options$explanatory)

                condition1 <- subcondition1a || (subcondition1b1 && (subcondition1b2 || subcondition1b3 || subcondition1b4 || subcondition1b5))

                condition2 <- subcondition2a || (subcondition2b1 && subcondition2b2 && subcondition2b3)

                condition3 <- condition3a || condition3b

                not_continue_analysis <- !(condition1 && condition2 && condition3)


                if (not_continue_analysis) {
                    private$.todo()
                    self$results$text$setVisible(FALSE)
                    self$results$text2$setVisible(FALSE)
                    self$results$plot$setVisible(FALSE)
                    self$results$plot3$setVisible(FALSE)
                    self$results$todo$setVisible(TRUE)
                    return()
                } else {
                  self$results$todo$setVisible(FALSE)
                }


                ## Stop if Empty Data ----

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                ## mydata ----

                cleaneddata <- private$.cleandata()

                name1time <- cleaneddata$name1time
                name2outcome <- cleaneddata$name2outcome
                name3contexpl <- cleaneddata$name3contexpl
                name3expl <- cleaneddata$name3expl

                mydata <- cleanData <- cleaneddata$cleanData

                mytime_labelled <- cleaneddata$mytime_labelled
                myoutcome_labelled <- cleaneddata$myoutcome_labelled
                mydxdate_labelled <- cleaneddata$mydxdate_labelled
                myfudate_labelled <- cleaneddata$myfudate_labelled
                myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
                mycontexpl_labelled <- cleaneddata$mycontexpl_labelled

                # Cox ----

                private$.cox()




                # Prepare Data For Plots ----

                image <- self$results$plot
                image$setState(cleaneddata)

                image3 <- self$results$plot3
                image3$setState(cleaneddata)


                # image4 <- self$results$plot4
                # image4$setState(mydata)

                # imageKM <- self$results$plotKM
                # imageKM$setState(mydata)

                # image7 <- self$results$plot7
                # image7$setState(mydata)

                # View mydata ----
                # self$results$mydataview$setContent(
                #     list(
                #         head(cleanData)
                #         )
                #     )

                # Add Calculated Time to Data ----

                if (self$options$tint && self$options$calculatedtime && self$results$calculatedtime$isNotFilled()) {
                    self$results$calculatedtime$setRowNums(cleanData$row_names)
                    self$results$calculatedtime$setValues(cleanData$mytime)
                    }




                # Add Redefined Outcome to Data ----

                if (self$options$multievent  && self$options$outcomeredifened && self$results$outcomeredifened$isNotFilled()) {
               self$results$outcomeredifened$setRowNums(cleanData$row_names)
               self$results$outcomeredifened$setValues(cleanData$myoutcome)
                            }





                }


            # cox  ----
        ,
            .cox = function() {


                cleaneddata <- private$.cleandata()

                name1time <- cleaneddata$name1time
                name2outcome <- cleaneddata$name2outcome

                name3contexpl <- cleaneddata$name3contexpl
                name3expl <- cleaneddata$name3expl

                mydata <- cleanData <- cleaneddata$cleanData

                mytime_labelled <- cleaneddata$mytime_labelled
                myoutcome_labelled <- cleaneddata$myoutcome_labelled
                mydxdate_labelled <- cleaneddata$mydxdate_labelled
                myfudate_labelled <- cleaneddata$myfudate_labelled
                myexplanatory_labelled <- cleaneddata$myexplanatory_labelled
                mycontexpl_labelled <- cleaneddata$mycontexpl_labelled


                ### prepare formula ----


                myexplanatory <- NULL

                if(!is.null(self$options$explanatory)) {
                    myexplanatory <- as.vector(myexplanatory_labelled)
                }


                mycontexpl <- NULL
                if(!is.null(self$options$contexpl)) {

                    mycontexpl <- as.vector(mycontexpl_labelled)

                }


                formula2 <- c(myexplanatory, mycontexpl)


                myformula <-
                    paste("Surv( mytime, myoutcome ) ~ ",
                          paste(formula2, collapse = " + ")
                    )

                myformula <- as.formula(myformula)


                # self$results$mydataview$setContent(
                #     list(
                #         mydata = head(mydata, n = 30),
                #         myformula = myformula,
                #         myexplanatory = myexplanatory,
                #         mycontexpl = mycontexpl,
                #         formula2 = formula2
                #         )
                # )


                ## finalfit Multivariable table ----

                finalfit::finalfit(
                    .data = mydata,
                    formula = myformula,
                    # dependent = myformula,
                    # explanatory = formula2,

                    metrics = TRUE
                ) -> tMultivariable


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
                                          landmark, " ",
                                          self$options$timetypeoutput, "."
                  )
                }





                self$results$text2$setContent(text2)



                results1 <- knitr::kable(
                    tMultivariable[[1]],
                    row.names = FALSE,
                    align = c('l', 'l', 'r', 'r', 'r', 'r'),
                    format = "html"
                )

                self$results$text$setContent(results1)

                ## coxph ----

                LHT <- "survival::Surv(mytime, myoutcome)"

                RHT <- formula2

                RHT <- paste(RHT, collapse = " + ")

                coxformula <- paste0(LHT, " ~ ", RHT)

                coxformula <- as.formula(coxformula)

                coxmodel <- survival::coxph(
                  coxformula,
                  data = mydata
                )

                summarycoxmodel <- summary(coxmodel)



                # Diagnostics of Cox Model ----

                # https://forum.jamovi.org/viewtopic.php?t=2563&sid=1e80bc4f5cc91581b11be1ca1b9cc169


                # fit <- coxph(Surv(futime, fustat) ~ age + ecog.ps + rx, data=ovarian)
                # cox_zph_fit <- survival::cox.zph(coxmodel)


                # plot all variables
                # ggcoxzph(cox.zph.fit)

                # plot all variables in specified order
                # ggcoxzph(cox.zph.fit,
                #          var = c("ecog.ps", "rx", "age"))

                # plot specified variables in specified order
                # ggcoxzph(cox.zph.fit,
                #          var = c("ecog.ps", "rx"),
                #          font.main = 12,
                #          caption = "Caption goes here")




                # ggcoxzph(): Graphical test of proportional hazards. Displays a graph of the scaled Schoenfeld residuals, along with a smooth curve using ggplot2.
                # Wrapper around plot.cox.zph().
                # ggcoxdiagnostics(): Displays diagnostics graphs presenting goodness of Cox Proportional Hazards Model fit.
                # ggcoxfunctional(): Displays graphs of continuous explanatory variable against martingale residuals of null cox proportional hazards model. It helps to properly choose the functional form of continuous variable in cox model.















                # https://forum.jamovi.org/viewtopic.php?p=9359&hilit=typeof#p9359

                # # Create a function ----
                # type_info <- function(x) {
                #     c(x,
                #       class = class(x),
                #       typeof = typeof(x),
                #       mode = mode(x),
                #       storage.mode = storage.mode(x)
                #     )
                # }


                # resultsdeneme2 <- list(
                #   headdata = head(mydata),
                #   myexplanatory = myexplanatory,
                #   mycontexpl = mycontexpl,
                #   formula2 = formula2,
                #   RHT = RHT,
                #   LHT = LHT,
                #   coxformula = coxformula,
                #   summarycoxmodel = summarycoxmodel,
                #   coxmodel = coxmodel
                # )

                # resultsdeneme2 <- lapply(resultsdeneme2, type_info)



                # self$results$text3$setContent(resultsdeneme2)













                # Reduced model ----
                # If you are using a backwards selection approach or similar, a reduced model can be directly specified and compared. The full model can be kept or dropped.

                # explanatory_multi = c("age", "thickness", "ulcer")
                # melanoma %>%
                #     finalfit(dependent_os, explanatory, explanatory_multi, keep_models = TRUE) %>%
                #     mykable()


                # Testing for proportional hazards ----
                # An assumption of CPH regression is that the hazard (think risk) associated with a particular variable does not change over time. For example, is the magnitude of the increase in risk of death associated with tumour ulceration the same in the early post-operative period as it is in later years?
                #
                #     The cox.zph() function from the survival package allows us to test this assumption for each variable. The plot of scaled Schoenfeld residuals should be a horizontal line. The included hypothesis test identifies whether the gradient differs from zero for each variable. No variable significantly differs from zero at the 5% significance level.

                # explanatory = c("age", "sex", "thickness", "ulcer", "year")
                # melanoma %>%
                #     coxphmulti(dependent_os, explanatory) %>%
                #     cox.zph() %>%
                #     {zph_result <<- .} %>%
                #     plot(var=5)

                # zph_result
                # #>               rho  chisq      p
                # #> age        0.1633 2.4544 0.1172
                # #> sexMale   -0.0781 0.4473 0.5036
                # #> thickness -0.1493 1.3492 0.2454
                # #> ulcerYes  -0.2044 2.8256 0.0928
                # #> year       0.0195 0.0284 0.8663
                # #> GLOBAL         NA 8.4695 0.1322


                # Stratified models ----
                # One approach to dealing with a violation of the proportional hazards assumption is to stratify by that variable. Including a strata() term will result in a separate baseline hazard function being fit for each level in the stratification variable. It will be no longer possible to make direct inference on the effect associated with that variable.
                #
                # This can be incorporated directly into the explanatory variable list.

                # explanatory= c("age", "sex", "ulcer", "thickness", "strata(year)")
                # melanoma %>%
                #     finalfit(dependent_os, explanatory) %>%
                #     mykable()




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


                ### prepare formula ----

                myexplanatory <- NULL
                if(!is.null(self$options$explanatory)) {
                    myexplanatory <- as.vector(myexplanatory_labelled)
                }

                mycontexpl <- NULL
                if(!is.null(self$options$contexpl)) {
                    mycontexpl <- as.vector(mycontexpl_labelled)
                }

                formula2 <- c(myexplanatory, mycontexpl)

                myformula <-
                    paste0('Surv( mytime, myoutcome )')

                # myformula <- as.formula(myformula)


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
                            ggplot2::theme(
                                axis.title =
                                    ggplot2::element_text(size = 12))
                        )
                    )


                # print plot ----

                print(plot)
                TRUE

            }





            # ,
            # .plot2 = function(image, ggtheme, theme, ...) {  # <-- the plot function
            #
            # # plotData <- image$state
            #
            #     if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$elapsedtime) )
            #         return()
            #
            # if (nrow(self$data) == 0)
            #     stop('Data contains no (complete) rows')
            #
            # # Check if outcome variable is suitable or stop
            # myoutcome2 <- self$options$outcome
            # myoutcome2 <- self$data[[myoutcome2]]
            # myoutcome2 <- na.omit(myoutcome2)
            #
            # if (class(myoutcome2) == "factor")
            #     stop("Please use a continuous variable for outcome.")
            #
            # if (any(myoutcome2 != 0 & myoutcome2 != 1))
            #     stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')
            #
            # # https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcoefstats.html#cox-proportional-hazards-regression-model-coxph
            # # fit a stratified model
            #
            # mydata <- self$data
            #
            # formulaL <- jmvcore::constructFormula(terms = self$options$elapsedtime)
            #
            # formulaL <- jmvcore::toNumeric(formulaL)
            #
            # formulaR <- jmvcore::constructFormula(terms = self$options$outcome)
            #
            # formulaR <- jmvcore::toNumeric(formulaR)
            #
            # formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)
            #
            # formula3 <- paste("survival::Surv(", formulaL, ",", formulaR, ") ~ ", formula2)
            #
            # formula3 <- as.formula(formula3)
            #
            # mod <-
            #     survival::coxph(
            #         formula = formula3,
            #         data = mydata
            #     )
            #
            # # plot
            # plot2 <- ggstatsplot::ggcoefstats(
            #     x = mod,
            #     exponentiate = TRUE,
            #     title = "Cox proportional hazards regression model"
            # )
            #
            # print(plot2)
            # TRUE
            #
            # }


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


                ### prepare formula ----

                myexplanatory <- NULL
                if(!is.null(self$options$explanatory)) {
                    myexplanatory <- as.vector(myexplanatory_labelled)
                }

                mycontexpl <- NULL
                if(!is.null(self$options$contexpl)) {
                    mycontexpl <- as.vector(mycontexpl_labelled)
                }

                formula2 <- c(myexplanatory, mycontexpl)

                myformula <-
                    paste("survival::Surv(mytime, myoutcome) ~ ",
                           paste(formula2, collapse = " + ")
                          )


                # self$results$mydataview$setContent(
                #     list(
                #         "myformula" = myformula,
                #         "mydata" = head(mydata),
                #         myexplanatory = myexplanatory,
                #         mycontexpl = mycontexpl,
                #         formula2 = formula2
                #         )
                # )



                myformula <- as.formula(myformula)

                mod <-
                    survival::coxph(formula = myformula,
                                    data = mydata)

                # plot

                # https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf


                # The function ggforest() from the survminer package creates a forest plot for a Cox regression model fit. Hazard ratio estimates along with confiden- ce intervals and p-values are plotter for each variable.

                # lung$age <- ifelse(lung$age > 70, ">70","<= 70")
                # fit <- coxph( Surv(time, status) ~ sex + ph.ecog + age, data = lung)
                # ggforest(fit)


                # ggforest ----

                plot3 <- survminer::ggforest(model = mod,
                                             data = mydata)


                # print plot ----

                print(plot3)
                TRUE

            }




            # # coxzph plot ----
            # ,
            # .plot4 = function(image4, ggtheme, theme, ...) {
            #
            #     plotData <- image4$state
            #
            #     formula2 <-
            #         jmvcore::constructFormula(terms = c(self$options$explanatory, self$options$contexpl))
            #
            #     formula3 <-
            #         paste("survival::Surv(mytime, myoutcome) ~ ", formula2)
            #
            #     formula3 <- as.formula(formula3)
            #
            #     cox_model <-
            #         survival::coxph(formula = formula3,
            #                         data = plotData)
            #
            #
            #
            #     cox_zph_fit <- survival::cox.zph(cox_model)
            #
            #
            #     # plot all variables
            #     plot4 <- survminer::ggcoxzph(cox_zph_fit)
            #
            #
            #     # print plot ----
            #
            #     print(plot4)
            #     TRUE
            #
            # }






















            # Kaplan-Meier ----

            # ,
            # .plotKM = function(imageKM, ggtheme, theme, ...) {
            #
            #             plotData <- imageKM$state
            #
            #             thefactor <- jmvcore::constructFormula(terms = self$options$explanatory)
            #
            #             if (length(self$options$explanatory) > 2)
            #                 stop("Kaplan-Meier function allows maximum of 2 explanatory variables")
            #
            #             if (!is.null(self$options$contexpl))
            #                 stop("Kaplan-Meier function does not use continuous explanatory variables.")
            #
            #             title2 <- as.character(thefactor)
            #
            #             plotKM <- plotData %>%
            #                 finalfit::surv_plot(.data = .,
            #                                     dependent = 'survival::Surv(mytime, myoutcome)',
            #                                     explanatory = as.vector(self$options$explanatory),
            #                                     xlab = paste0('Time (', self$options$timetypeoutput, ')'),
            #                                     pval = self$options$pplot,
            #                                     pval.method	= self$options$pplot,
            #                                     # pval = TRUE,
            #                                     legend = 'none',
            #                                     break.time.by = self$options$byplot,
            #                                     xlim = c(0,self$options$endplot),
            #                                     title = paste0("Survival curves for ", title2),
            #                                     subtitle = "Based on Kaplan-Meier estimates",
            #                                     risk.table = self$options$risktable,
            #                                     conf.int = self$options$ci95,
            #                                     censored = self$options$censored
            #
            #                 )
            #
            #             # plot <- plot + ggtheme
            #
            #             print(plotKM)
            #             TRUE
            #
            #
            #
            #         }



            # # Adjusted Survival ----
            # ,
            # .plot7 = function(image7, ggtheme, theme, ...) {
            #
            #     plotData <- image7$state
            #
            #
            #     if (!(self$options$adjexplanatory %in% c(self$options$explanatory, self$options$contexpl)))
            #         stop("Please use the explanatory variable used to build the multivariable survival model.")
            #
            #
            #
            #     formula2 <-
            #         jmvcore::constructFormula(terms = c(self$options$explanatory, self$options$contexpl))
            #
            #     formula3 <-
            #         paste("survival::Surv(mytime, myoutcome) ~ ", formula2)
            #
            #     formula3 <- as.formula(formula3)
            #
            #     # get fitted model ----
            #
            #     mod <-
            #         survival::coxph(formula = formula3,
            #                         data = plotData)
            #
            #
            #     # plot
            #
            #     # https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf
            #
            #
            #     # The function ggadjustedcurves() from the survminer package plots Adjusted Survival Curves for Cox Proportional Hazards Model. Adjusted Survival Curves show how a selected factor influences survival estimated from a Cox model.
            #     # Note that these curves differ from Kaplan Meier estimates since they present expected ssurvival based on given Cox model.
            #
            #     # lung$sex <- ifelse(lung$sex == 1, "Male", "Female")
            #
            #     # fit <- coxph(Surv(time, status) ~ sex + ph.ecog + age +
            #     #                  strata(sex), data = lung)
            #     # ggcoxadjustedcurves(fit, data=lung)
            #
            #
            #     # Note that it is not necessary to include the grouping factor in the Cox model. Survival curves are estimated from Cox model for each group defined by the factor independently.
            #
            #     # lung$age3 <- cut(lung$age,
            #     #                  c(35,55,65,85))
            #
            #
            #     # ggcoxadjustedcurves(fit, data=lung,
            #     #                     variable=”lage3”)
            #
            #
            #     # select adjexplanatory ----
            #
            #
            #     # adjexplanatory <- NULL
            #     #
            #     # if (!is.null(self$options$adjexplanatory)) {
            #     #
            #     # adjexplanatory <- self$options$adjexplanatory
            #     #
            #     # adjexplanatory <-
            #     #     jmvcore::composeTerm(components = adjexplanatory)
            #     #
            #     # }
            #
            #     # ggadjustedcurves ----
            #
            #     plot7 <- survminer::ggadjustedcurves(fit = mod,
            #                                          data = plotData,
            #                                          variable = adjexplanatory
            #                                          # method = ,
            #                                          # fun =
            #                                          )
            #
            #
            #     # https://rpkgs.datanovia.com/survminer/reference/ggadjustedcurves.html
            #
            #     # https://stackoverflow.com/questions/55404550/computing-se-or-ci-for-ggadjustedcurves
            #
            #     ## print plot -----
            #
            #     print(plot7)
            #     TRUE
            #
            #     }




            )
        )
