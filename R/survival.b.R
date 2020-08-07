#' @title Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr
#'

survivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "survivalClass",
    inherit = survivalBase,
    private = list(
        .run = function() {

            # Initial Message ----

            if ( is.null(self$options$outcome) ||

                 (is.null(self$options$elapsedtime) && !(self$options$tint))

                  || (is.null(self$options$explanatory) && is.null(self$options$contexpl))

                 ) {

                todo <- glue::glue("
                <br>Welcome to ClinicoPath
                <br><br>
                This tool will help you calculate median survivals and 1,3,5-yr survivals for a given fisk factor.
                <br><br>
                Explanatory variable can be categorical (ordinal or nominal), or continuous
                (options under continuous variable collapsebox).
                <br><br>
                Select outcome level from Outcome variable.
                <br><br>
                Outcome Level: if patient is dead or event (recurrence) occured.
                <br><br>
                Survival time should be numeric and continuous.
                <br><br>
                This function uses survival, survminer, and finalfit packages. Please cite jamovi and the packages as given below.
                <br><hr>
                <br>
                See details for survival <a href = 'https://cran.r-project.org/web/packages/survival/vignettes/survival.pdf'>here</a>."
                )

                html <- self$results$todo
                html$setContent(todo)
                return()

            }




            if (length(self$options$explanatory) > 1) {


                todo <- glue::glue("
                                   <br>More than one explanatory variable.
                                   <br>
                                   <hr>")
                html <- self$results$todo
                html$setContent(todo)

            }





                # Common Errors, Warnings ----

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

            if ( !is.null(self$options$explanatory) && !is.null(self$options$contexpl)) {

                stop("If you want to use continuous and categorical variables together as explanatory variables, please use Multivariate Survival Analysis function in jsurvival module.")

            }



            # Common Definitions ----


            contin <- c("integer", "numeric", "double")


                # Read Data ----

                mydata <- self$data




                # Read Arguments ----

                elapsedtime <- self$options$elapsedtime
                outcome <- self$options$outcome
                explanatory <- self$options$explanatory
                contexpl <- self$options$contexpl


                outcomeLevel <- self$options$outcomeLevel

                tint <- self$options$tint






                # Define Outcome ----



                multievent <- self$options$multievent

                outcome1 <- self$options$outcome
                outcome1 <- self$data[[outcome1]]


                if (!multievent) {


                if (inherits(outcome1, contin)) {

                    if (
                !((length(unique(outcome1[!is.na(outcome1)])) == 2) && (sum(unique(outcome1[!is.na(outcome1)])) == 1) )
                         ) {
                        stop('When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')

                    }

                    mydata[["myoutcome"]] <- mydata[[self$options$outcome]]


                } else if (inherits(outcome1, "factor")) {


                    # mydata[[self$options$outcome]] <-
                    #     ifelse(test = outcome1 == outcomeLevel,
                    #            yes = 1,
                    #            no = 0)



                    mydata[["myoutcome"]] <-
                        ifelse(test = outcome1 == outcomeLevel,
                               yes = 1,
                               no = 0)
                } else {

                    stop('When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0. If you are using a factor as an outcome, please check the levels and content.')

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

                mydata[[self$options$elapsedtime]] <- jmvcore::toNumeric(mydata[[self$options$elapsedtime]])

                mydata[["mytime"]] <- jmvcore::toNumeric(mydata[[self$options$elapsedtime]])


                } else if (tint) {

                # Time Interval ----

                dxdate <- self$options$dxdate
                fudate <- self$options$fudate
                timetypedata <- self$options$timetypedata


                # stopifnot(lubridate::is.Date(
                #     lubridate::ymd_hms(
                #         mydata[[fudate]]
                #         )
                #     )
                # )
                #
                # stopifnot(lubridate::is.Date(lubridate::ymd_hms((mydata[[dxdate]]))))

                stopifnot(
                    inherits(mydata[[dxdate]], c("POSIXct","POSIXt", "POSIXlt"))
                )

                stopifnot(
                    inherits(mydata[[fudate]], c("POSIXct","POSIXt", "POSIXlt"))
                )


                if (timetypedata == "ymdhms") {
                mydata[["start"]] <- lubridate::ymd_hms(mydata[[dxdate]])
                mydata[["end"]] <- lubridate::ymd_hms(mydata[[fudate]])
                }
                if (timetypedata == "ymd") {
                    mydata[["start"]] <- lubridate::ymd(mydata[[dxdate]])
                    mydata[["end"]] <- lubridate::ymd(mydata[[fudate]])
                }
                if (timetypedata == "ydm") {
                    mydata[["start"]] <- lubridate::ydm(mydata[[dxdate]])
                    mydata[["end"]] <- lubridate::ydm(mydata[[fudate]])
                }
                if (timetypedata == "mdy") {
                    mydata[["start"]] <- lubridate::mdy(mydata[[dxdate]])
                    mydata[["end"]] <- lubridate::mdy(mydata[[fudate]])
                }
                if (timetypedata == "myd") {
                    mydata[["start"]] <- lubridate::myd(mydata[[dxdate]])
                    mydata[["end"]] <- lubridate::myd(mydata[[fudate]])
                }
                if (timetypedata == "dmy") {
                    mydata[["start"]] <- lubridate::dmy(mydata[[dxdate]])
                    mydata[["end"]] <- lubridate::dmy(mydata[[fudate]])
                }
                if (timetypedata == "dym") {
                    mydata[["start"]] <- lubridate::dym(mydata[[dxdate]])
                    mydata[["end"]] <- lubridate::dym(mydata[[fudate]])
                }






                timetypeoutput <- jmvcore::constructFormula(terms = self$options$timetypeoutput)


                mydata <- mydata %>%
                    dplyr::mutate(
                        interval = lubridate::interval(start, end)
                    )

                stopifnot(lubridate::is.interval(mydata[["interval"]]))

                mydata <- mydata %>%
                    dplyr::mutate(
                        mytime = lubridate::time_length(interval, timetypeoutput)
                    )

                # mydata[["interval"]] <- lubridate::interval(
                #     start = lubridate::ymd_hms(mydata[[dxdate]]),
                #     end = lubridate::ymd_hms(mydata[[fudate]])
                # )

                # mydata[["mytime"]] <- lubridate::time_length(mydata[["interval"]], timetypeoutput)

                }


                # Define =1 Explanatory Factor ----



                if ( length(self$options$explanatory) == 1 ) {

                expl <- self$options$explanatory

                mydata[["myfactor"]] <- mydata[[expl]]

                }



                # Define Data For Analysis


                # naOmit ----

                mydata <- jmvcore::naOmit(mydata)



                # View mydata ----

                self$results$mydataview$setContent(
                    list(
                    "outcome1" = outcome1,
                    dod,
                    dooc,
                    awd,
                    awod,
                    head(mydata, n = 30)
                    )
                    )

                # return()


                # Continious Explanatory ----


                if ( !is.null(self$options$contexpl) ) {


                                    todo <- glue::glue("
                                                       <br>
                                                       Continious Explanatory
                                                       <br>
                                                       <hr>")
                                    html <- self$results$todo
                                    html$setContent(todo)


                                    # Disable other tables
                                    self$results$medianSummary$setVisible(FALSE)
                                    self$results$medianTable$setVisible(FALSE)
                                    self$results$survTableSummary$setVisible(FALSE)
                                    self$results$survTable$setVisible(FALSE)
                                    self$results$pairwiseSummary$setVisible(FALSE)
                                    self$results$pairwiseTable$setVisible(FALSE)





                                    # Continious Cox Regression ----


                                    formula2 <- as.vector(self$options$contexpl)

                                    myformula <- paste("Surv(", "mytime", "," , "myoutcome", ")")

                                    finalfit::finalfit(.data = mydata,
                                                       dependent = myformula,
                                                       explanatory = formula2

                                                       # metrics = TRUE
                                    ) -> tCox

                                    tCox_df <- tibble::as_tibble(tCox, .name_repair = "minimal") %>%
                                        janitor::clean_names(dat = ., case = "snake")


                                    # Continious Cox-Regression Table ----

                                    coxTable <- self$results$coxTable

                                    data_frame <- tCox_df

                                    names(data_frame) <- c(
                                        "Explanatory",
                                        "Levels",
                                        "all",
                                        "HR_univariable",
                                        "HR_multivariable"
                                    )

                                    for(i in seq_along(data_frame[,1,drop=T])) {
                                        coxTable$addRow(rowKey = i, values = c(data_frame[i,]))
                                    }


                                    # Continious coxTable explanation ----


                                    tCox_df <- tibble::as_tibble(tCox, .name_repair = "minimal") %>%
                                        janitor::clean_names(dat = ., case = "snake")

                                    names(tCox_df) <- names(data_frame) <- c(
                                        "Explanatory",
                                        "Levels",
                                        "all",
                                        "HR_univariable",
                                        "HR_multivariable"
                                    )


                                    # https://stackoverflow.com/questions/38470355/r-fill-empty-cell-with-value-of-last-non-empty-cell

                                    while(length(ind <- which(tCox_df$Explanatory == "")) > 0){
                                        tCox_df$Explanatory[ind] <- tCox_df$Explanatory[ind - 1]
                                    }

                                    # https://stackoverflow.com/questions/51180290/mutate-by-group-in-r

                                    tCox_df %>%
                                        dplyr::group_by(Explanatory) %>%
                                        dplyr::mutate(firstlevel = first(Levels)) %>%
                                        dplyr::mutate(
                                            coxdescription = glue::glue(
                                                "When {Explanatory} increases 1 unit, the hazard increases {HR_multivariable} times."
                                            )
                                        ) %>%
                                        dplyr::filter(HR_univariable != '-') %>%
                                        dplyr::pull(coxdescription) -> coxSummary



                                    coxSummary <- unlist(coxSummary)
                                    self$results$coxSummary$setContent(coxSummary)




                                # Continuous Optimal Cut-off ----

                                # https://rpkgs.datanovia.com/survminer/reference/surv_cutpoint.html

                                    findcut <- self$options$findcut

                                    if (findcut) {

                                    res.cut <- survminer::surv_cutpoint(
                                        mydata,
                                        time = "mytime",
                                        event = "myoutcome",
                                        self$options$contexpl,
                                        minprop = 0.1,
                                        progressbar = TRUE
                                    )

                                    # res.cut$Age
                                    # res.cut$data
                                    # res.cut$minprop
                                    # res.cut$cutpoint


                                    # Cut-off Table ----

                                    rescut_summary <- summary(res.cut)

                                    # self$results$rescutTable$setContent(rescut_summary)


                                    rescutTable <- self$results$rescutTable

                                    rescutTable$setTitle(paste0(self$options$contexpl))


                                    data_frame <- rescut_summary
                                    for (i in seq_along(data_frame[,1,drop = T])) {
                                        rescutTable$addRow(rowKey = i, values = c(data_frame[i,]))
                                    }


                                    # categorisation ----

                                    res.cat <- survminer::surv_categorize(res.cut)


                                    # View mydata ----

                                    # self$results$mydataview$setContent(head(res.cat, 20))

                                    # Prepare Data For Continuous Explanatory Plots ----

                                    plotData4 <- res.cut

                                    image4 <- self$results$plot4
                                    image4$setState(plotData4)

                                    plotData5 <- res.cat

                                    image5 <- self$results$plot5
                                    image5$setState(plotData5)


                                    }




                                    return()



                                }






















                # One explanatory ----

                if (
                    length(self$options$explanatory) == 1 && !is.null(self$options$outcome) && !is.null(self$options$elapsedtime)
                    ) {


                # One explanatory message ----


                todo <- glue::glue("
                                   <br>Analysis with one variable
                                   <br>
                                   <hr>")
                html <- self$results$todo
                html$setContent(todo)


                }



                # Median Survival Table ----



                thefactor <- jmvcore::constructFormula(terms = self$options$explanatory)


                formula <- paste('survival::Surv(mytime, myoutcome) ~ ', thefactor)
                formula <- as.formula(formula)

                km_fit <- survival::survfit(formula, data = mydata)

                km_fit_median_df <- summary(km_fit)
                results1html <- as.data.frame(km_fit_median_df$table) %>%
                    janitor::clean_names(dat = ., case = "snake") %>%
                    tibble::rownames_to_column(.data = .)


                results1html[,1] <- gsub(pattern = ", ",
                                         replacement = " and ",
                                         x = results1html[,1])

                results1table <- results1html

                names(results1table)[1] <- "factor"

                medianTable <- self$results$medianTable
                data_frame <- results1table
                for (i in seq_along(data_frame[,1,drop = T])) {
                    medianTable$addRow(rowKey = i, values = c(data_frame[i,]))
                }


                # Median Survival Summary ----

                results1table %>%
                    dplyr::mutate(
                        description =
                            glue::glue(
                                "When {factor}, median survival is {round(median, digits = 1)} [{round(x0_95lcl, digits = 1)} - {round(x0_95ucl, digits = 1)}, 95% CI] ", self$options$timetypeoutput, "."
                            )
                    ) %>%
                    dplyr::mutate(
                        description = gsub(pattern = "=", replacement = " is ", x = description)
                    ) %>%
                    dplyr::select(description) %>%
                    dplyr::pull(.) -> km_fit_median_definition

                medianSummary <- km_fit_median_definition


                self$results$medianSummary$setContent(medianSummary)





                # Cox Regression ----


                formula2 <- as.vector(self$options$explanatory)

                myformula <- paste("Surv(", "mytime", "," , "myoutcome", ")")

                finalfit::finalfit(.data = mydata,
                dependent = myformula,
                explanatory = formula2

                # metrics = TRUE
                ) -> tCox

                tCox_df <- tibble::as_tibble(tCox, .name_repair = "minimal") %>%
                    janitor::clean_names(dat = ., case = "snake")


                # Cox-Regression Table ----

                # tCox_df <- tCox_df[,-(dim(tCox_df)[2])]

                coxTable <- self$results$coxTable

                data_frame <- tCox_df

                names(data_frame) <- c(
                    "Explanatory",
                    "Levels",
                    "all",
                    "HR_univariable",
                    "HR_multivariable"
                    )

                for (i in seq_along(data_frame[,1,drop = T])) {
                    coxTable$addRow(rowKey = i, values = c(data_frame[i,]))
                }


                # coxTable explanation ----


                tCox_df <- tibble::as_tibble(tCox, .name_repair = "minimal") %>%
                    janitor::clean_names(dat = ., case = "snake")

                names(tCox_df) <- names(data_frame) <- c(
                    "Explanatory",
                    "Levels",
                    "all",
                    "HR_univariable",
                    "HR_multivariable"
                )


                # https://stackoverflow.com/questions/38470355/r-fill-empty-cell-with-value-of-last-non-empty-cell

                while(length(ind <- which(tCox_df$Explanatory == "")) > 0){
                    tCox_df$Explanatory[ind] <- tCox_df$Explanatory[ind - 1]
                }

                # https://stackoverflow.com/questions/51180290/mutate-by-group-in-r

                tCox_df %>%
                    dplyr::group_by(Explanatory) %>%
                    dplyr::mutate(firstlevel = first(Levels)) %>%
                    dplyr::mutate(
                        coxdescription = glue::glue(
                            "When {Explanatory} is {Levels}, there is {HR_multivariable} times risk than when {Explanatory} is {firstlevel}."
                        )
                    ) %>%
                    dplyr::filter(HR_univariable != '-') %>%
                    dplyr::pull(coxdescription) -> coxSummary



                coxSummary <- unlist(coxSummary)
                self$results$coxSummary$setContent(coxSummary)
























                # survival table 1,3,5-yr survival ----

                utimes <- self$options$cutp

                utimes <- strsplit(utimes, ",")
                utimes <- purrr::reduce(utimes, as.vector)
                utimes <- as.numeric(utimes)

                if (length(utimes) == 0) {
                utimes <- c(12,36,60)
                }

                km_fit_summary <- summary(km_fit, times = utimes)

                km_fit_df <- as.data.frame(km_fit_summary[c("strata", "time", "n.risk", "n.event", "surv", "std.err", "lower", "upper")])

                km_fit_df[,1] <- gsub(pattern = "thefactor=",
                                      replacement = paste0(self$options$explanatory, " "),
                                      x = km_fit_df[,1])


                survTable <- self$results$survTable

                data_frame <- km_fit_df
                for(i in seq_along(data_frame[,1,drop=T])) {
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





            if (self$options$pw) {

                #  pairwise comparison ----


                formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)


                formula_p <- paste0('survival::Surv(', "mytime", ',', "myoutcome", ') ~ ', formula2)
                formula_p <- as.formula(formula_p)

                results_pairwise <-
                        survminer::pairwise_survdiff(
                            formula = formula_p,
                            data = mydata,
                            p.adjust.method = "BH")


                mypairwise2 <- as.data.frame(results_pairwise[["p.value"]]) %>%
                                tibble::rownames_to_column(.data = .) %>%
                                tidyr::pivot_longer(data = ., cols = -rowname) %>%
                                dplyr::filter(complete.cases(.))



                # Pairwise Table ----

                pairwiseTable <- self$results$pairwiseTable

                data_frame <- mypairwise2
                for (i in seq_along(data_frame[,1,drop = T])) {
                    pairwiseTable$addRow(rowKey = i, values = c(data_frame[i,]))
                }

                thefactor <- jmvcore::constructFormula(terms = self$options$explanatory)

                title2 <- as.character(thefactor)

                pairwiseTable$setTitle(paste0('Pairwise Comparisons ', title2))


                mypairwise2 %>%
                    dplyr::mutate(description =
                                      glue::glue(
                                          "The difference between ",
                                          " {rowname} and {name}",
                                          " has a p-value of {format.pval(value, digits = 3, eps = 0.001)}."
                                          )
                                  ) %>%
                    dplyr::pull(description) -> pairwiseSummary

                pairwiseSummary <- unlist(pairwiseSummary)


                self$results$pairwiseSummary$setContent(pairwiseSummary)


                if ( length(self$options$explanatory) == 1 && dim(mypairwise2)[1] == 1 ) {

                self$results$pairwiseTable$setVisible(FALSE)

                pairwiseSummary <- "No pairwise comparison when explanatory variable has < 3 levels."
                self$results$pairwiseSummary$setContent(pairwiseSummary)

                }





            }









                # View mydata ----

                # self$results$mydataview$setContent(head(mydata, 20))



                        # Prepare Data For Plots ----

                        plotData <- mydata

                        image <- self$results$plot
                        image$setState(plotData)

                        image2 <- self$results$plot2
                        image2$setState(plotData)

                        image3 <- self$results$plot3
                        image3$setState(plotData)



                    }


,
.plot = function(image, ggtheme, theme, ...) {  # <-- the plot function ----


    sc <- self$options$sc

    if (!sc)
        return()

    # if (nrow(self$data) == 0)
    # stop('Data contains no (complete) rows')

    if ( !is.null(self$options$explanatory) && !is.null(self$options$contexpl)) {

        stop("If you want to use continuous and categorical variables together as explanatory variables, please use Multivariate Survival Analysis function in jsurvival module.")

    }


    # if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$elapsedtime) )
    #     return()

    plotData <- image$state

    thefactor <- jmvcore::constructFormula(terms = self$options$explanatory)

    title2 <- as.character(thefactor)

    plot <- plotData %>%
        finalfit::surv_plot(.data = .,
                            dependent = 'survival::Surv(mytime, myoutcome)',
                            explanatory = as.vector(self$options$explanatory),
                            xlab = paste0('Time (', self$options$timetypeoutput, ')'),
                            pval = TRUE,
                            legend = 'none',
                            break.time.by = 12,
                            xlim = c(0,self$options$endplot),
                            title = paste0("Survival curves for ", title2),
                            subtitle = "Based on Kaplan-Meier estimates",
                            risk.table = self$options$risktable,
                            conf.int = self$options$ci95
        )

    # plot <- plot + ggtheme

    print(plot)
    TRUE



}



# https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf
,
.plot2 = function(image2, ggtheme, theme, ...) {  # <-- the plot2 function ----


    ce <- self$options$ce

    if (!ce)
        return()

    # if (nrow(self$data) == 0)
    #     stop('Data contains no (complete) rows')

    if ( !is.null(self$options$explanatory) && !is.null(self$options$contexpl)) {

        stop("If you want to use continuous and categorical variables together as explanatory variables, please use Multivariate Survival Analysis function in jsurvival module.")

    }

    # if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$elapsedtime) )
    #     return()

    plotData <- image2$state


    thefactor <- jmvcore::constructFormula(terms = self$options$explanatory)

    title2 <- as.character(thefactor)


    plot2 <- plotData %>%
        finalfit::surv_plot(.data = .,
                            dependent = 'survival::Surv(mytime, myoutcome)',
                            explanatory = as.vector(self$options$explanatory),
                            xlab = paste0('Time (', self$options$timetypeoutput, ')'),
                            # pval = TRUE,
                            legend = 'none',
                            break.time.by = 12,
                            xlim = c(0,self$options$endplot),
                            title = paste0("Cumulative Events ", title2),
                            fun = "event",
                            risk.table = self$options$risktable,
                            conf.int = self$options$ci95
        )


    print(plot2)
    TRUE



}



,
.plot3 = function(image3, ggtheme, theme, ...) {  # <-- the plot3 function ----


    ch <- self$options$ch

    if (!ch)
        return()

    # if (nrow(self$data) == 0)
    #     stop('Data contains no (complete) rows')

    if ( !is.null(self$options$explanatory) && !is.null(self$options$contexpl)) {

        stop("If you want to use continuous and categorical variables together as explanatory variables, please use Multivariate Survival Analysis function in jsurvival module.")

    }


    # if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$elapsedtime) )
    #     return()

    plotData <- image3$state

    thefactor <- jmvcore::constructFormula(terms = self$options$explanatory)

    title2 <- as.character(thefactor)



    plot3 <- plotData %>%
        finalfit::surv_plot(.data = .,
                            dependent = 'survival::Surv(mytime, myoutcome)',
                            explanatory = as.vector(self$options$explanatory),
                            xlab = paste0('Time (', self$options$timetypeoutput, ')'),
                            # pval = TRUE,
                            legend = 'none',
                            break.time.by = 12,
                            xlim = c(0,self$options$endplot),
                            title = paste0("Cumulative Hazard ", title2),
                            fun = "cumhaz",
                            risk.table = self$options$risktable,
                            conf.int = self$options$ci95
        )


    print(plot3)
    TRUE
}


,
.plot4 = function(image4, ggtheme, theme, ...) {  # <-- the plot4 function ----


    findcut <- self$options$findcut

    if (!findcut)
        return()

    # if (nrow(self$data) == 0)
    #     stop('Data contains no (complete) rows')

    if ( !is.null(self$options$explanatory) && !is.null(self$options$contexpl)) {

        stop("If you want to use continuous and categorical variables together as explanatory variables, please use Multivariate Survival Analysis function in jsurvival module.")

    }


    # if (is.null(self$options$contexpl) || is.null(self$options$outcome) || is.null(self$options$elapsedtime) )
    #     return()

    plotData <- image4$state

    res.cut <- plotData

    plot4 <- plot(res.cut, self$options$contexpl, palette = "npg")


    print(plot4)
    TRUE
}


,
.plot5 = function(image5, ggtheme, theme, ...) {  # <-- the plot5 function ----


    findcut <- self$options$findcut

    if (!findcut)
        return()

    # if (nrow(self$data) == 0)
    #     stop('Data contains no (complete) rows')

    if ( !is.null(self$options$explanatory) && !is.null(self$options$contexpl)) {

        stop("If you want to use continuous and categorical variables together as explanatory variables, please use Multivariate Survival Analysis function in jsurvival module.")

    }


    # if (is.null(self$options$contexpl) || is.null(self$options$outcome) || is.null(self$options$elapsedtime) )
    #     return()

    plotData <- image5$state

    res.cat <- plotData


    contfactor <- jmvcore::constructFormula(terms = self$options$contexpl)

    # contfactor <- as.formula(contfactor)

    myformula <- paste0("survival::Surv(mytime, myoutcome) ~ ", contfactor)

    myformula <- as.formula(myformula)

    fit <- survminer::surv_fit(formula = myformula,
                               data = res.cat
                               )

    plot5 <- survminer::ggsurvplot(fit,
                                   data = res.cat,
                                   risk.table = self$options$risktable,
                                   conf.int = self$options$ci95)


    print(plot5)
    TRUE
}







        )
)
