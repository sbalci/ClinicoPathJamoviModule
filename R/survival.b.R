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
                if (self$options$sas) {
                    # Disable tables
                    self$results$medianSummary$setVisible(FALSE)
                    self$results$medianTable$setVisible(FALSE)
                    self$results$coxSummary$setVisible(FALSE)
                    self$results$coxTable$setVisible(FALSE)
                    self$results$tCoxtext2$setVisible(FALSE)
                    self$results$survTableSummary$setVisible(FALSE)
                    self$results$survTable$setVisible(FALSE)
                    self$results$pairwiseSummary$setVisible(FALSE)
                    self$results$pairwiseTable$setVisible(FALSE)
                }

            }
            ,
            .todo = function() {
                if ((is.null(self$options$outcome) && !(self$options$multievent)) ||

                    (self$options$multievent &&
                     (
                         is.null(self$options$dod) &&
                         is.null(self$options$dooc) &&
                         is.null(self$options$awd) && is.null(self$options$awod)
                     )) ||

                    (self$options$tint &&
                     (
                         is.null(self$options$dxdate) || is.null(self$options$fudate)
                     )) ||

                    ((is.null(self$options$explanatory)) || (is.null(self$explanatory$sas)))) {

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

            }



            # Define Survival Time ----
            ,
            .definemytime = function() {
                mydata <- self$data

                tint <- self$options$tint


                if (!tint) {
                    # Precalculated Time ----

                    # mydata[[self$options$elapsedtime]] <- jmvcore::toNumeric(mydata[[self$options$elapsedtime]])

                    mydata[["mytime"]] <-
                        jmvcore::toNumeric(mydata[[self$options$elapsedtime]])


                } else if (tint) {
                    # Time Interval ----

                    dxdate <- self$options$dxdate
                    fudate <- self$options$fudate
                    timetypedata <- self$options$timetypedata


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


                return(mydata[["mytime"]])


            }

            # Add Calculated Time to Data ----
            ,
            .mytimetodata = function() {
                mycalculatedtime <- private$.definemytime()

                if (self$options$calculatedtime &&
                    self$results$calculatedtime$isNotFilled()) {
                    self$results$calculatedtime$setValues(mycalculatedtime)
                }

            }

            # Define Outcome ----
            ,
            .definemyoutcome = function() {
                mydata <- self$data

                contin <- c("integer", "numeric", "double")

                outcomeLevel <- self$options$outcomeLevel
                multievent <- self$options$multievent
                outcome1 <- self$options$outcome
                outcome1 <- self$data[[outcome1]]

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

                        mydata[["myoutcome"]] <-
                            mydata[[self$options$outcome]]

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


                return(mydata[["myoutcome"]])

            }


            # Add Redefined Outcome to Data ----
            ,
            .myoutcometodata = function() {
                mydefinedoutcome <- private$.definemyoutcome()

                if (self$options$outcomeredifened &&
                    self$results$outcomeredifened$isNotFilled()) {
                    self$results$outcomeredifened$setValues(mydefinedoutcome)
                }

            }





            # Define Factor ----
            ,
            .definemyfactor = function() {
                mydata <- self$data

                expl <- self$options$explanatory

                mydata[["myfactor"]] <- mydata[[expl]]

                # single arm ----

                sas <- self$options$sas

                if (sas) {
                    mydata[["myfactor"]] <- "1"
                }

                return(mydata[["myfactor"]])

            }


            # Clean Data For Analysis ----
            ,
            .cleandata = function() {
                time <- private$.definemytime()
                outcome <- private$.definemyoutcome()
                factor <- private$.definemyfactor()

                cleanData <- data.frame("mytime" = time,
                                        "myoutcome" = outcome,
                                        "factor" = factor)

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
                    name1time <- jmvcore::composeTerm(self$options$elapsedtime)
                }

                name2outcome <-
                    jmvcore::composeTerm(self$options$outcome)

                if (self$options$sas) {
                    name3explanatory <- "SingleArm"
                }

                if (!self$options$sas &&
                    !is.null(self$options$explanatory)) {
                    name3explanatory <- jmvcore::composeTerm(self$options$explanatory)
                }

                names(cleanData) <-
                    c(name1time, name2outcome, name3explanatory)

                # naOmit ----

                cleanData <- jmvcore::naOmit(cleanData)






                # # View mydata ----
                # self$results$mydataview$setContent(list(time,
                #                                         outcome,
                #                                         factor,
                #                                         name1time,
                #                                         name2outcome,
                #                                         name3explanatory,
                #                                         head(cleanData, n = 30)))


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
                        "cleanData" = cleanData
                    )
                )

            }


            # Run Analysis ----
            ,
            .run = function() {
                # Common Errors, Warnings ----

                # No variable TODO ----
                if ((is.null(self$options$outcome) &&
                     !(self$options$multievent)) ||

                    (self$options$multievent &&
                     (
                         is.null(self$options$dod) &&
                         is.null(self$options$dooc) &&
                         is.null(self$options$awd) && is.null(self$options$awod)
                     )) ||

                    (self$options$tint &&
                     (
                         is.null(self$options$dxdate) || is.null(self$options$fudate)
                     )) ||

                    is.null(self$options$explanatory)) {
                    private$.todo()

                    return()
                } else {
                  self$results$todo$setVisible(FALSE)
                }

                # Empty data ----
                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                # Add Calculated Time to Data ----

                if (self$options$tint) {
                    private$.mytimetodata()
                }

                # mycalculatedtime <- private$.definemytime()
                #
                # if (self$options$calculatedtime && self$results$calculatedtime$isNotFilled()) {
                #     self$results$calculatedtime$setValues(mycalculatedtime)
                # }

                # Add Redefined Outcome to Data ----

                if (self$options$multievent) {
                    private$.myoutcometodata()
                }


                # Get Clean Data ----
                results <- private$.cleandata()


                # Run Analysis ----
                if (!self$options$sas) {
                    # Median Survival ----
                    private$.medianSurv(results)
                    # Cox ----
                    private$.cox(results)
                    # Survival Table ----
                    private$.survTable(results)
                }

                # Pairwise ----
                if (self$options$pw && !self$options$sas) {
                    private$.pairwise(results)
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

                # Median Survival Table ----

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

                medianTable <- self$results$medianTable
                data_frame <- results1table
                for (i in seq_along(data_frame[, 1, drop = T])) {
                    medianTable$addRow(rowKey = i, values = c(data_frame[i,]))
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
                    dplyr::select(description) %>%
                    dplyr::pull(.) -> km_fit_median_definition

                medianSummary <- km_fit_median_definition


                self$results$medianSummary$setContent(medianSummary)


            }

            # Cox Regression Function ----
            ,
            .cox = function(results) {
                # Cox Regression ----


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

                # myformula <- as.formula(myformula)

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


                # Cox-Regression Table ----

                # tCox_df <- tCox_df[,-(dim(tCox_df)[2])]

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


                # Cox-Table Explanation ----


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

                # Median Survival Table ----

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

                km_fit_df[, 1] <- gsub(
                    pattern = "thefactor=",
                    replacement = paste0(self$options$explanatory, " "),
                    x = km_fit_df[, 1]
                )


                survTable <- self$results$survTable

                data_frame <- km_fit_df
                for (i in seq_along(data_frame[, 1, drop = T])) {
                    survTable$addRow(rowKey = i, values = c(data_frame[i,]))
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


            # Pairwise Function ----
            ,
            .pairwise = function(results) {
                #  pairwise comparison ----


                mytime <- results$name1time
                myoutcome <- results$name2outcome
                myfactor <- results$name3explanatory

                mydata <- results$cleanData

                mydata[[mytime]] <-
                    jmvcore::toNumeric(mydata[[mytime]])

                # Median Survival Table ----

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



                # Pairwise Table ----

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
                                "The difference",
                                title2,
                                "between {rowname} and {name}",
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

                sas <- self$options$sas

                if (sas) {
                    title2 <- "Overall"
                }


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

                sas <- self$options$sas

                if (sas) {
                    title2 <- "Overall"
                }

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

                sas <- self$options$sas

                if (sas) {
                    title2 <- "Overall"
                }


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

                sas <- self$options$sas

                if (sas) {
                    title2 <- "Overall"
                }


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


        )
    )
