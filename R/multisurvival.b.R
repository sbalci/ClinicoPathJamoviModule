#' @title Multivariable Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'

multisurvivalClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "multisurvivalClass",
        inherit = multisurvivalBase,
        private = list(

            .todo = function() {
                # If no variable selected Initial Message ----

                if (

                    (is.null(self$options$outcome) && !(self$options$multievent)) ||

                    (self$options$multievent && (is.null(self$options$dod) && is.null(self$options$dooc) && is.null(self$options$awd) && is.null(self$options$awod))) ||

                    (is.null(self$options$elapsedtime) && !(self$options$tint)) ||

                    (self$options$tint && (is.null(self$options$dxdate) || is.null(self$options$fudate))) ||

                    is.null(self$options$explanatory)

                    # ||

                    # (!is.null(self$options$explanatory) && is.null(self$options$contexpl))


                    )
                {
                    # TODO ----

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

                } else {
                    if (nrow(self$data) == 0)
                        stop('Data contains no (complete) rows')
                }

            }
            ,


            .cleandata = function() {
                # Common Definitions ----

                contin <- c("integer", "numeric", "double")

                # Read Data ----

                mydata <- self$data

                # Read Arguments ----

                elapsedtime <- self$options$elapsedtime
                outcome <- self$options$outcome
                explanatory <- self$options$explanatory
                outcomeLevel <- self$options$outcomeLevel
                tint <- self$options$tint

                # Define Outcome ----

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
                        # mydata[[self$options$outcome]] <-
                        #     ifelse(test = outcome1 == outcomeLevel,
                        #            yes = 1,
                        #            no = 0)



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
                        # (Alive) <=> (Dead of Disease & Dead of Other Causes)


                        mydata[["myoutcome"]] <- NA_integer_

                        mydata[["myoutcome"]][outcome1 == awd] <- 0
                        mydata[["myoutcome"]][outcome1 == awod] <- 0
                        mydata[["myoutcome"]][outcome1 == dod] <- 1
                        mydata[["myoutcome"]][outcome1 == dooc] <- 1



                    } else if (analysistype == 'cause') {
                        # (Alive & Dead of Other Causes) <=> (Dead of Disease)


                        mydata[["myoutcome"]] <- NA_integer_

                        mydata[["myoutcome"]][outcome1 == awd] <- 0
                        mydata[["myoutcome"]][outcome1 == awod] <- 0
                        mydata[["myoutcome"]][outcome1 == dod] <- 1
                        mydata[["myoutcome"]][outcome1 == dooc] <- 0

                    } else if (analysistype == 'compete') {
                        # Alive <=> Dead of Disease accounting for Dead of Other Causes



                        mydata[["myoutcome"]] <- NA_integer_

                        mydata[["myoutcome"]][outcome1 == awd] <- 0
                        mydata[["myoutcome"]][outcome1 == awod] <- 0
                        mydata[["myoutcome"]][outcome1 == dod] <- 1
                        mydata[["myoutcome"]][outcome1 == dooc] <- 2

                    }

                }


                # Define Survival Time ----


                if (!tint) {
                    ## Use precalculated time ----

                    mydata[[self$options$elapsedtime]] <-
                        jmvcore::toNumeric(mydata[[self$options$elapsedtime]])

                    mydata[["mytime"]] <-
                        jmvcore::toNumeric(mydata[[self$options$elapsedtime]])


                } else if (tint) {
                    ## Calculate Time Interval ----

                    dxdate <- self$options$dxdate
                    fudate <- self$options$fudate
                    timetypedata <- self$options$timetypedata

                    # stopifnot(inherits(mydata[[dxdate]], c("POSIXct", "POSIXt", "POSIXlt")))

                    # stopifnot(inherits(mydata[[fudate]], c("POSIXct", "POSIXt", "POSIXlt")))


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




                # Define Explanatory ----

                myexplanatory <- NULL

                if(!is.null(self$options$explanatory)) {

                myexplanatory <- as.vector(self$options$explanatory)

                }


                mycontexpl <- NULL

                if(!is.null(self$options$contexpl)) {

                mycontexpl <- as.vector(self$options$contexpl)

                }


                myfactors <- c(myexplanatory, mycontexpl)



                # Add Redefined Outcome to Data ----

                if (self$options$multievent) {

                    if (self$options$outcomeredifened &&
                        self$results$outcomeredifened$isNotFilled()) {
                        self$results$outcomeredifened$setValues(mydata[["myoutcome"]])
                    }
                }

                # Add Calculated Time to Data ----

                if (self$options$tint) {

                    if (self$options$calculatedtime &&
                        self$results$calculatedtime$isNotFilled()) {
                        self$results$calculatedtime$setValues(mydata[["mytime"]])
                    }
                }




                # Landmark ----
                # https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#landmark_method
                if (self$options$uselandmark) {

                  landmark <- jmvcore::toNumeric(self$options$landmark)

                  mydata <- mydata %>%
                    dplyr::filter(mytime >= landmark) %>%
                    dplyr::mutate(mytime = mytime - landmark)
                }









                # Define Data For Analysis ----

                mydata <- jmvcore::select(df = mydata, columnNames = c("mytime", "myoutcome", myfactors))




                # naOmit ----

                mydata <- jmvcore::naOmit(mydata)


                # Send cleaned mydata to other functions  ----


                return(list("mydata" = mydata))


            }


            ,
            .run = function() {



                # Errors ----
                if (

                    (is.null(self$options$outcome) && !(self$options$multievent)) ||

                    (self$options$multievent && (is.null(self$options$dod) && is.null(self$options$dooc) && is.null(self$options$awd) && is.null(self$options$awod))) ||

                    (self$options$tint && (is.null(self$options$dxdate) || is.null(self$options$fudate))) ||

                    is.null(self$options$explanatory)

                    # ||
                    #
                    # (!is.null(self$options$explanatory) && is.null(self$options$contexpl))

                    ) {
                    private$.todo()
                    return()
                }

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                # Calculate mydata ----

                cleaneddata <- private$.cleandata()

                mydata <- cleaneddata$mydata


                # Cox ----

                private$.cox(mydata)



                # View mydata ----
                # self$results$mydataview$setContent(
                #     list(head(mydata, n = 30))
                #     )


                #             # Prepare Data For Plots ----


                            image <- self$results$plot
                            image$setState(mydata)

                            image3 <- self$results$plot3
                            image3$setState(mydata)

                            imageKM <- self$results$plotKM
                            imageKM$setState(mydata)

                            # image7 <- self$results$plot7
                            # image7$setState(mydata)



            },



            .cox = function(mydata) {


                # prepare formula ----


                myexplanatory <- NULL

                if(!is.null(self$options$explanatory)) {

                    myexplanatory <- as.vector(self$options$explanatory)

                }


                mycontexpl <- NULL

                if(!is.null(self$options$contexpl)) {

                    mycontexpl <- as.vector(self$options$contexpl)

                }


                formula2 <- c(myexplanatory, mycontexpl)

                # formula2 <-c(as.vector(self$options$explanatory),
                #              as.vector(self$options$contexpl)
                # )

                # formulaL <-
                #     jmvcore::constructFormula(terms = self$options$elapsedtime)
                #
                # formulaL <- jmvcore::toNumeric(formulaL)
                #
                # formulaL <-
                #     jmvcore::constructFormula(terms = self$options$elapsedtime)

                # formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

                # formulaR <- jmvcore::toNumeric(formulaR)


                myformula <-
                    paste("Surv(mytime, myoutcome)")

                # finalfit Multivariable table ----

                finalfit::finalfit(
                    .data = mydata,
                    dependent = myformula,
                    explanatory = formula2,

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

                # Cox2 ----

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


                plotData <- image$state

                # prepare formula ----

                formula2 <-
                    jmvcore::constructFormula(terms = c(self$options$explanatory, self$options$contexpl))

                # formula2 <- as.vector(self$options$explanatory)


                myformula <-
                    paste("survival::Surv(mytime, myoutcome)")


                # hr_plot ----
                # https://finalfit.org/reference/hr_plot.html

                plot <-
                    finalfit::hr_plot(
                        .data = plotData,
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

                plotData <- image3$state


                formula2 <-
                    jmvcore::constructFormula(terms = c(self$options$explanatory, self$options$contexpl))

                formula3 <-
                    paste("survival::Surv(mytime, myoutcome) ~ ", formula2)

                formula3 <- as.formula(formula3)

                mod <-
                    survival::coxph(formula = formula3,
                                    data = plotData)

                # plot

                # https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf


                # The function ggforest() from the survminer package creates a forest plot for a Cox regression model fit. Hazard ratio estimates along with confiden- ce intervals and p-values are plotter for each variable.

                # lung$age <- ifelse(lung$age > 70, ">70","<= 70")
                # fit <- coxph( Surv(time, status) ~ sex + ph.ecog + age, data = lung)
                # ggforest(fit)


                # ggforest ----
                plot3 <- survminer::ggforest(model = mod,
                                             data = plotData)


                # print plot ----

                print(plot3)
                TRUE

            }

            # Kaplan-Meier ----

            ,
            .plotKM = function(imageKM, ggtheme, theme, ...) {

                        plotData <- imageKM$state

                        thefactor <- jmvcore::constructFormula(terms = self$options$explanatory)

                        if (length(self$options$explanatory) > 2)
                            stop("Kaplan-Meier function allows maximum of 2 explanatory variables")

                        if (!is.null(self$options$contexpl))
                            stop("Kaplan-Meier function does not use continuous explanatory variables.")

                        title2 <- as.character(thefactor)

                        plotKM <- plotData %>%
                            finalfit::surv_plot(.data = .,
                                                dependent = 'survival::Surv(mytime, myoutcome)',
                                                explanatory = as.vector(self$options$explanatory),
                                                xlab = paste0('Time (', self$options$timetypeoutput, ')'),
                                                pval = self$options$pplot,
                                                pval.method	= self$options$pplot,
                                                # pval = TRUE,
                                                legend = 'none',
                                                break.time.by = self$options$byplot,
                                                xlim = c(0,self$options$endplot),
                                                title = paste0("Survival curves for ", title2),
                                                subtitle = "Based on Kaplan-Meier estimates",
                                                risk.table = self$options$risktable,
                                                conf.int = self$options$ci95,
                                                censored = self$options$censored

                            )

                        # plot <- plot + ggtheme

                        print(plotKM)
                        TRUE



                    }



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
            #     # print plot -----
            #
            #     print(plot7)
            #     TRUE
            #
            #     }




            )
        )





#         # Define Survival Time ----
#
#         .definemytime = function() {
#
#             mydata <- self$data
#
#             tint <- self$options$tint
#
#
#             if (!tint) {
#
#                 # Precalculated Time ----
#
#                 # mydata[[self$options$elapsedtime]] <- jmvcore::toNumeric(mydata[[self$options$elapsedtime]])
#
#                 mydata[["mytime"]] <- jmvcore::toNumeric(mydata[[self$options$elapsedtime]])
#
#
#             } else if (tint) {
#
#                 # Time Interval ----
#
#                 dxdate <- self$options$dxdate
#                 fudate <- self$options$fudate
#                 timetypedata <- self$options$timetypedata
#
#
#                 if (timetypedata == "ymdhms") {
#                     mydata[["start"]] <- lubridate::ymd_hms(mydata[[dxdate]])
#                     mydata[["end"]] <- lubridate::ymd_hms(mydata[[fudate]])
#                 }
#                 if (timetypedata == "ymd") {
#                     mydata[["start"]] <- lubridate::ymd(mydata[[dxdate]])
#                     mydata[["end"]] <- lubridate::ymd(mydata[[fudate]])
#                 }
#                 if (timetypedata == "ydm") {
#                     mydata[["start"]] <- lubridate::ydm(mydata[[dxdate]])
#                     mydata[["end"]] <- lubridate::ydm(mydata[[fudate]])
#                 }
#                 if (timetypedata == "mdy") {
#                     mydata[["start"]] <- lubridate::mdy(mydata[[dxdate]])
#                     mydata[["end"]] <- lubridate::mdy(mydata[[fudate]])
#                 }
#                 if (timetypedata == "myd") {
#                     mydata[["start"]] <- lubridate::myd(mydata[[dxdate]])
#                     mydata[["end"]] <- lubridate::myd(mydata[[fudate]])
#                 }
#                 if (timetypedata == "dmy") {
#                     mydata[["start"]] <- lubridate::dmy(mydata[[dxdate]])
#                     mydata[["end"]] <- lubridate::dmy(mydata[[fudate]])
#                 }
#                 if (timetypedata == "dym") {
#                     mydata[["start"]] <- lubridate::dym(mydata[[dxdate]])
#                     mydata[["end"]] <- lubridate::dym(mydata[[fudate]])
#                 }
#
#
#
#                 timetypeoutput <- jmvcore::constructFormula(terms = self$options$timetypeoutput)
#
#
#                 mydata <- mydata %>%
#                     dplyr::mutate(
#                         interval = lubridate::interval(start, end)
#                     )
#
#                 stopifnot(lubridate::is.interval(mydata[["interval"]]))
#
#                 mydata <- mydata %>%
#                     dplyr::mutate(
#                         mytime = lubridate::time_length(interval, timetypeoutput)
#                     )
#
#
#             }
#
#
#             return(mydata[["mytime"]])
#
#
#         }
#
#         # Define Outcome ----
#         ,
#         .definemyoutcome = function() {
#
#             mydata <- self$data
#
#             contin <- c("integer", "numeric", "double")
#
#             multievent <- self$options$multievent
#             outcome1 <- self$options$outcome
#             outcome1 <- self$data[[outcome1]]
#
#             if (!multievent) {
#
#                 if (inherits(outcome1, contin)) {
#
#                     if (
#                         !((length(unique(outcome1[!is.na(outcome1)])) == 2) && (sum(unique(outcome1[!is.na(outcome1)])) == 1) )
#                     ) {
#                         stop('When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')
#
#                     }
#
#                     mydata[["myoutcome"]] <- mydata[[self$options$outcome]]
#
#                 } else if (inherits(outcome1, "factor")) {
#
#                     mydata[["myoutcome"]] <-
#                         ifelse(test = outcome1 == outcomeLevel,
#                                yes = 1,
#                                no = 0)
#
#                 } else {
#
#                     stop('When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0. If you are using a factor as an outcome, please check the levels and content.')
#
#                 }
#
#             } else if (multievent) {
#
#
#                 analysistype <- self$options$analysistype
#
#                 dod <- self$options$dod
#                 dooc <- self$options$dooc
#                 awd <- self$options$awd
#                 awod <- self$options$awod
#
#                 if (analysistype == 'overall') {
#
#                     # (Alive) <=> (Dead of Disease & Dead of Other Causes)
#
#
#                     mydata[["myoutcome"]] <- NA_integer_
#
#                     mydata[["myoutcome"]][outcome1 == awd] <- 0
#                     mydata[["myoutcome"]][outcome1 == awod] <- 0
#                     mydata[["myoutcome"]][outcome1 == dod] <- 1
#                     mydata[["myoutcome"]][outcome1 == dooc] <- 1
#
#
#
#                 } else if (analysistype == 'cause') {
#
#                     # (Alive & Dead of Other Causes) <=> (Dead of Disease)
#
#
#                     mydata[["myoutcome"]] <- NA_integer_
#
#                     mydata[["myoutcome"]][outcome1 == awd] <- 0
#                     mydata[["myoutcome"]][outcome1 == awod] <- 0
#                     mydata[["myoutcome"]][outcome1 == dod] <- 1
#                     mydata[["myoutcome"]][outcome1 == dooc] <- 0
#
#                 } else if (analysistype == 'compete') {
#
#                     # Alive <=> Dead of Disease accounting for Dead of Other Causes
#
#
#
#                     mydata[["myoutcome"]] <- NA_integer_
#
#                     mydata[["myoutcome"]][outcome1 == awd] <- 0
#                     mydata[["myoutcome"]][outcome1 == awod] <- 0
#                     mydata[["myoutcome"]][outcome1 == dod] <- 1
#                     mydata[["myoutcome"]][outcome1 == dooc] <- 2
#
#                 }
#
#             }
#
#
#             return(mydata[["myoutcome"]])
#
#         }
#
#         # Define Factor ----
#         ,
#         .definemyfactor = function() {
#
#             mydata <- self$data
#
#             # 1 Explanatory Factor ----
#
#             if ( length(self$options$explanatory) == 1 ) {
#
#                 expl <- self$options$explanatory
#
#                 mydata[["myfactor"]] <- mydata[[expl]]
#
#                 return(mydata[["myfactor"]])
#
#             }
#
#             # > 1 Explanatory Factor ----
#
#             if ( length(self$options$explanatory) > 1 ) {
#
#                 thefactor <- jmvcore::constructFormula(terms = self$options$explanatory)
#
#                 return(thefactor)
#
#             }
#
#             # single arm ----
#
#             sas <- self$options$sas
#
#             if (sas) {
#                 thefactor <- 1
#                 return(thefactor)
#             }
#
#
#         }
#
#
#         # Clean Data For Analysis ----
#         ,
#         .cleandata = function() {
#
#
#             time <- private$.definemytime()
#             outcome <- private$.definemyoutcome()
#
#
#             if ( length(self$options$explanatory) == 1 ) {
#                 factor <- private$.definemyfactor()
#
#                 cleanData <- data.frame(
#                     "mytime" = time,
#                     "myoutcome" = outcome,
#                     "factor" = factor
#                 )
#             }
#
#
#             if ( length(self$options$explanatory) > 1 || self$options$sas ) {
#                 factor <- private$.definemyfactor()
#                 factor <- jmvcore::select(df = self$data, columnNames = factor)
#
#                 cleanData <- data.frame(
#                     "mytime" = time,
#                     "myoutcome" = outcome,
#                     factor
#                 )
#
#             }
#
#
#
#             # naOmit ----
#
#             cleanData <- jmvcore::naOmit(cleanData)
#
#
#             # View mydata ----
#
#             self$results$mydataview$setContent(
#                 list(time,
#                      outcome,
#                      factor,
#                      head(cleanData, n = 30)
#                 )
#             )
#
#
#             # Prepare Data For Plots ----
#
#             plotData <- cleanData
#
#             image <- self$results$plot
#             image$setState(plotData)
#
#             image2 <- self$results$plot2
#             image2$setState(plotData)
#
#             image3 <- self$results$plot3
#             image3$setState(plotData)
#
#             image6 <- self$results$plot6
#             image6$setState(plotData)
#
#         }
#
#
#
#
#
#         ,
#         .run = function() {
#
#
#             # Common Errors, Warnings ----
#
#             # No variable ----
#             if ( is.null(self$options$outcome) ||
#
#                  (is.null(self$options$elapsedtime) && !(self$options$tint))
#
#                  || is.null(self$options$explanatory)
#
#             ) {
#
#                 todo <- glue::glue("
#                 <br>Welcome to ClinicoPath
#                 <br><br>
#                 This tool will help you calculate median survivals and 1,3,5-yr survivals for a given fisk factor.
#                 <br><br>
#                 Explanatory variable should be categorical (ordinal or nominal).
#                 <br><br>
#                 Select outcome level from Outcome variable.
#                 <br><br>
#                 Outcome Level: if patient is dead or event (recurrence) occured. You may also use advanced outcome options depending on your analysis type.
#                 <br><br>
#                 Survival time should be numeric and continuous. You may also use dates to calculate survival time in advanced elapsed time options.
#                 <br><br>
#                 This function uses survival, survminer, and finalfit packages. Please cite jamovi and the packages as given below.
#                 <br><hr>
#                 <br>
#                 See details for survival <a href = 'https://cran.r-project.org/web/packages/survival/vignettes/survival.pdf'>here</a>."
#                 )
#
#                 html <- self$results$todo
#                 html$setContent(todo)
#                 return()
#             }
#
#             # More than one explanatory ----
#             if (length(self$options$explanatory) > 1 && !(
#                 is.null(self$options$outcome) ||
#                 (is.null(self$options$elapsedtime) && !(self$options$tint))
#             )
#
#             ) {
#
#
#                 todo <- glue::glue("
#                                    <br>More than one explanatory variable.
#                                    <br>
#                                    <hr>")
#                 html <- self$results$todo
#                 html$setContent(todo)
#
#             }
#
#             # One explanatory ----
#
#             if (
#                 length(self$options$explanatory) == 1 && !(
#                     is.null(self$options$outcome) ||
#                     (is.null(self$options$elapsedtime) && !(self$options$tint))
#                 )
#
#             ) {
#
#                 todo <- glue::glue("
#                                    <br>Analysis with one variable
#                                    <br>
#                                    <hr>")
#                 html <- self$results$todo
#                 html$setContent(todo)
#
#             }
#
#
#             # Empty data ----
#
#             if (nrow(self$data) == 0)
#                 stop('Data contains no (complete) rows')
#
#             # Prepare Clean Data ----
#
#             private$.cleandata()
#
#
#
#
#
#
#
#             # Median Survival Table ----
#
#
#
#             thefactor <- jmvcore::constructFormula(terms = self$options$explanatory)
#
#             sas <- self$options$sas
#
#             if (sas) {
#                 thefactor <- 1
#             }
#
#             formula <- paste('survival::Surv(mytime, myoutcome) ~ ', thefactor)
#             formula <- as.formula(formula)
#
#             km_fit <- survival::survfit(formula, data = mydata)
#
#
#             km_fit_median_df <- summary(km_fit)
#             results1html <- as.data.frame(km_fit_median_df$table) %>%
#                 janitor::clean_names(dat = ., case = "snake") %>%
#                 tibble::rownames_to_column(.data = .)
#
#
#             results1html[,1] <- gsub(pattern = ", ",
#                                      replacement = " and ",
#                                      x = results1html[,1])
#
#             results1table <- results1html
#
#             names(results1table)[1] <- "factor"
#
#             medianTable <- self$results$medianTable
#             data_frame <- results1table
#             for (i in seq_along(data_frame[,1,drop = T])) {
#                 medianTable$addRow(rowKey = i, values = c(data_frame[i,]))
#             }
#
#
#             # Median Survival Summary ----
#
#             results1table %>%
#                 dplyr::mutate(
#                     description =
#                         glue::glue(
#                             "When {factor}, median survival is {round(median, digits = 1)} [{round(x0_95lcl, digits = 1)} - {round(x0_95ucl, digits = 1)}, 95% CI] ", self$options$timetypeoutput, "."
#                         )
#                 ) %>%
#                 dplyr::mutate(
#                     description = gsub(pattern = "=", replacement = " is ", x = description)
#                 ) %>%
#                 dplyr::select(description) %>%
#                 dplyr::pull(.) -> km_fit_median_definition
#
#             medianSummary <- km_fit_median_definition
#
#
#             self$results$medianSummary$setContent(medianSummary)
#
#
#
#
#
#             # Cox Regression ----
#
#
#             formula2 <- as.vector(self$options$explanatory)
#
#             sas <- self$options$sas
#
#             if (sas) {
#                 formula2 <- 1
#             }
#
#             myformula <- paste("Surv(", "mytime", "," , "myoutcome", ")")
#
#             finalfit::finalfit(.data = mydata,
#                                dependent = myformula,
#                                explanatory = formula2,
#
#                                metrics = TRUE
#             ) -> tCox
#
#
#
#             tCoxtext2 <- glue::glue("
#                                 <br>
#                                 <b>Model Metrics:</b>
#                                   ",
#                                 unlist(
#                                     tCox[[2]]
#                                 ),
#                                 "
#                                 <br>
#                                 ")
#
#
#             self$results$tCoxtext2$setContent(tCoxtext2)
#
#
#
#
#             tCox_df <- tibble::as_tibble(tCox[[1]], .name_repair = "minimal") %>%
#                 janitor::clean_names(dat = ., case = "snake")
#
#
#             # Cox-Regression Table ----
#
#             # tCox_df <- tCox_df[,-(dim(tCox_df)[2])]
#
#             coxTable <- self$results$coxTable
#
#             data_frame <- tCox_df
#
#             names(data_frame) <- c(
#                 "Explanatory",
#                 "Levels",
#                 "all",
#                 "HR_univariable",
#                 "HR_multivariable"
#             )
#
#             for (i in seq_along(data_frame[,1,drop = T])) {
#                 coxTable$addRow(rowKey = i, values = c(data_frame[i,]))
#             }
#
#
#             # coxTable explanation ----
#
#
#             tCox_df <- tibble::as_tibble(tCox[[1]], .name_repair = "minimal") %>%
#                 janitor::clean_names(dat = ., case = "snake")
#
#             names(tCox_df) <- names(data_frame) <- c(
#                 "Explanatory",
#                 "Levels",
#                 "all",
#                 "HR_univariable",
#                 "HR_multivariable"
#             )
#
#
#             # https://stackoverflow.com/questions/38470355/r-fill-empty-cell-with-value-of-last-non-empty-cell
#
#             while(length(ind <- which(tCox_df$Explanatory == "")) > 0){
#                 tCox_df$Explanatory[ind] <- tCox_df$Explanatory[ind - 1]
#             }
#
#             # https://stackoverflow.com/questions/51180290/mutate-by-group-in-r
#
#             tCox_df %>%
#                 dplyr::group_by(Explanatory) %>%
#                 dplyr::mutate(firstlevel = first(Levels)) %>%
#                 dplyr::mutate(
#                     coxdescription = glue::glue(
#                         "When {Explanatory} is {Levels}, there is {HR_multivariable} times risk than when {Explanatory} is {firstlevel}."
#                     )
#                 ) %>%
#                 dplyr::filter(HR_univariable != '-') %>%
#                 dplyr::pull(coxdescription) -> coxSummary
#
#
#
#             coxSummary <- unlist(coxSummary)
#             self$results$coxSummary$setContent(coxSummary)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#             # survival table 1,3,5-yr survival ----
#
#             utimes <- self$options$cutp
#
#             utimes <- strsplit(utimes, ",")
#             utimes <- purrr::reduce(utimes, as.vector)
#             utimes <- as.numeric(utimes)
#
#             if (length(utimes) == 0) {
#                 utimes <- c(12,36,60)
#             }
#
#             km_fit_summary <- summary(km_fit, times = utimes)
#
#             km_fit_df <- as.data.frame(km_fit_summary[c("strata", "time", "n.risk", "n.event", "surv", "std.err", "lower", "upper")])
#
#             km_fit_df[,1] <- gsub(pattern = "thefactor=",
#                                   replacement = paste0(self$options$explanatory, " "),
#                                   x = km_fit_df[,1])
#
#
#             survTable <- self$results$survTable
#
#             data_frame <- km_fit_df
#             for(i in seq_along(data_frame[,1,drop=T])) {
#                 survTable$addRow(rowKey = i, values = c(data_frame[i,]))
#             }
#
#
#
#
#             # survTableSummary 1,3,5-yr survival summary ----
#
#             km_fit_df %>%
#                 dplyr::mutate(
#                     description =
#                         glue::glue(
#                             "When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]."
#                         )
#                 ) %>%
#                 dplyr::select(description) %>%
#                 dplyr::pull(.) -> survTableSummary
#
#
#
#             self$results$survTableSummary$setContent(survTableSummary)
#
#
#
#
#
#             if (self$options$pw) {
#
#                 #  pairwise comparison ----
#
#
#                 formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)
#
#
#                 formula_p <- paste0('survival::Surv(', "mytime", ',', "myoutcome", ') ~ ', formula2)
#                 formula_p <- as.formula(formula_p)
#
#                 results_pairwise <-
#                     survminer::pairwise_survdiff(
#                         formula = formula_p,
#                         data = mydata,
#                         p.adjust.method = "BH")
#
#
#                 mypairwise2 <- as.data.frame(results_pairwise[["p.value"]]) %>%
#                     tibble::rownames_to_column(.data = .) %>%
#                     tidyr::pivot_longer(data = ., cols = -rowname) %>%
#                     dplyr::filter(complete.cases(.))
#
#
#
#                 # Pairwise Table ----
#
#                 pairwiseTable <- self$results$pairwiseTable
#
#                 data_frame <- mypairwise2
#                 for (i in seq_along(data_frame[,1,drop = T])) {
#                     pairwiseTable$addRow(rowKey = i, values = c(data_frame[i,]))
#                 }
#
#                 thefactor <- jmvcore::constructFormula(terms = self$options$explanatory)
#
#                 title2 <- as.character(thefactor)
#
#
#                 sas <- self$options$sas
#
#                 if (sas) {
#                     thefactor <- 1
#                     title2 <- "Overall"
#                 }
#
#                 pairwiseTable$setTitle(paste0('Pairwise Comparisons ', title2))
#
#
#                 mypairwise2 %>%
#                     dplyr::mutate(description =
#                                       glue::glue(
#                                           "The difference between ",
#                                           " {rowname} and {name}",
#                                           " has a p-value of {format.pval(value, digits = 3, eps = 0.001)}."
#                                       )
#                     ) %>%
#                     dplyr::pull(description) -> pairwiseSummary
#
#                 pairwiseSummary <- unlist(pairwiseSummary)
#
#
#                 self$results$pairwiseSummary$setContent(pairwiseSummary)
#
#
#                 if ( length(self$options$explanatory) == 1 && dim(mypairwise2)[1] == 1 ) {
#
#                     self$results$pairwiseTable$setVisible(FALSE)
#
#                     pairwiseSummary <- "No pairwise comparison when explanatory variable has < 3 levels."
#                     self$results$pairwiseSummary$setContent(pairwiseSummary)
#
#                 }
#
#             }
#
#
#         }
#
#
#         ,
#         .plot = function(image, ggtheme, theme, ...) {  # <-- the plot function ----
#
#
#             sc <- self$options$sc
#
#             if (!sc)
#                 return()
#
#
#             plotData <- image$state
#
#             thefactor <- jmvcore::constructFormula(terms = self$options$explanatory)
#
#             title2 <- as.character(thefactor)
#
#             sas <- self$options$sas
#
#             if (sas) {
#                 thefactor <- 1
#                 title2 <- "Overall"
#             }
#
#
#             plot <- plotData %>%
#                 finalfit::surv_plot(.data = .,
#                                     dependent = 'survival::Surv(mytime, myoutcome)',
#                                     explanatory = as.vector(self$options$explanatory),
#                                     xlab = paste0('Time (', self$options$timetypeoutput, ')'),
#                                     pval = TRUE,
#                                     legend = 'none',
#                                     break.time.by = self$options$byplot,
#                                     xlim = c(0,self$options$endplot),
#                                     title = paste0("Survival curves for ", title2),
#                                     subtitle = "Based on Kaplan-Meier estimates",
#                                     risk.table = self$options$risktable,
#                                     conf.int = self$options$ci95
#                 )
#
#             # plot <- plot + ggtheme
#
#             print(plot)
#             TRUE
#
#
#
#         }
#
#
#
#         # https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf
#         ,
#         .plot2 = function(image2, ggtheme, theme, ...) {  # <-- the plot2 function ----
#
#
#             ce <- self$options$ce
#
#             if (!ce)
#                 return()
#
#
#             plotData <- image2$state
#
#
#             thefactor <- jmvcore::constructFormula(terms = self$options$explanatory)
#
#             title2 <- as.character(thefactor)
#
#             sas <- self$options$sas
#
#             if (sas) {
#                 thefactor <- 1
#                 title2 <- "Overall"
#             }
#
#             plot2 <- plotData %>%
#                 finalfit::surv_plot(.data = .,
#                                     dependent = 'survival::Surv(mytime, myoutcome)',
#                                     explanatory = as.vector(self$options$explanatory),
#                                     xlab = paste0('Time (', self$options$timetypeoutput, ')'),
#                                     # pval = TRUE,
#                                     legend = 'none',
#                                     break.time.by = self$options$byplot,
#                                     xlim = c(0,self$options$endplot),
#                                     title = paste0("Cumulative Events ", title2),
#                                     fun = "event",
#                                     risk.table = self$options$risktable,
#                                     conf.int = self$options$ci95
#                 )
#
#
#             print(plot2)
#             TRUE
#
#
#
#         }
#
#
#
#         ,
#         .plot3 = function(image3, ggtheme, theme, ...) {  # <-- the plot3 function ----
#
#
#             ch <- self$options$ch
#
#             if (!ch)
#                 return()
#
#             plotData <- image3$state
#
#             thefactor <- jmvcore::constructFormula(terms = self$options$explanatory)
#
#             title2 <- as.character(thefactor)
#
#             sas <- self$options$sas
#
#             if (sas) {
#                 thefactor <- 1
#                 title2 <- "Overall"
#             }
#
#
#             plot3 <- plotData %>%
#                 finalfit::surv_plot(.data = .,
#                                     dependent = 'survival::Surv(mytime, myoutcome)',
#                                     explanatory = as.vector(self$options$explanatory),
#                                     xlab = paste0('Time (', self$options$timetypeoutput, ')'),
#                                     # pval = TRUE,
#                                     legend = 'none',
#                                     break.time.by = self$options$byplot,
#                                     xlim = c(0,self$options$endplot),
#                                     title = paste0("Cumulative Hazard ", title2),
#                                     fun = "cumhaz",
#                                     risk.table = self$options$risktable,
#                                     conf.int = self$options$ci95
#                 )
#
#
#             print(plot3)
#             TRUE
#         }
#
#
#         ,
#         .plot6 = function(image6, ggtheme, theme, ...) {  # <-- the plot6 function ----
#
#
#             kmunicate <- self$options$kmunicate
#
#             if (!kmunicate)
#                 return()
#
#             plotData <- image6$state
#
#             thefactor <- jmvcore::constructFormula(terms = self$options$explanatory)
#
#
#             sas <- self$options$sas
#
#             if (sas) {
#                 thefactor <- 1
#             }
#
#             formula <- paste('survival::Surv(mytime, myoutcome) ~ ', thefactor)
#
#             formula <- as.formula(formula)
#
#             km_fit <- survival::survfit(formula, data = plotData)
#
#             time_scale <- seq(0, self$options$endplot, by = self$options$byplot)
#
#
#             plot6 <-
#                 KMunicate::KMunicate(fit = km_fit,
#                                      time_scale = time_scale,
#                                      .xlab = paste0('Time in ', self$options$timetypeoutput)
#                 )
#
#
#             print(plot6)
#             TRUE
#
#         }
#
#
#
#
#
#
#     )
# )

