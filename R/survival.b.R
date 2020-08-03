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

            if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$elapsedtime) ) {

                todo <- glue::glue("
                <br>Welcome to ClinicoPath
                <br><br>
                This tool will help you calculate median survivals and 1,3,5-yr survivals for a given fisk factor.
                <br><br>
                Explanatory variable should be categorical (ordinal or nominal).
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
                                   <br>More than one explanatory variable
                                   <br>
                                   <hr>")
                html <- self$results$todo
                html$setContent(todo)

            }





                # Common Errors, Warnings ----

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')


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


                outcome1 <- self$options$outcome

                outcome1 <- self$data[[outcome1]]

                if (inherits(outcome1, contin)) {

                    if (
                !( (length(unique(outcome1[!is.na(outcome1)])) == 2) && (sum(unique(outcome1[!is.na(outcome1)])) == 1) )
                         ) {
                        stop('When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')

                    }

                    mydata[["myoutcome"]] <- mydata[[self$options$outcome]]


                } else if (inherits(outcome1, "factor")) {


                    mydata[[self$options$outcome]] <-
                        ifelse(test = outcome1 == outcomeLevel,
                               yes = 1,
                               no = 0)



                    mydata[["myoutcome"]] <-
                        ifelse(test = outcome1 == outcomeLevel,
                               yes = 1,
                               no = 0)
                } else {

                    stop('When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0. If you are using a factor as an outcome, please check the levels and content.')

                }



                # Define Survival ----


                if (!tint) {

                mydata[[self$options$elapsedtime]] <- jmvcore::toNumeric(mydata[[self$options$elapsedtime]])

                mydata[["mytime"]] <- jmvcore::toNumeric(mydata[[self$options$elapsedtime]])


                } else if (tint) {

                # Time Interval ----

                # mydata$int <- lubridate::interval(
                #     lubridate::ymd(mydata$SurgeryDate),
                #     lubridate::ymd(mydata$LastFollowUpDate)
                # )
                # mydata$OverallTime <- lubridate::time_length(mydata$int, "month")
                # mydata$OverallTime <- round(mydata$OverallTime, digits = 1)


                }


                # Define Explanatory Factor ----



                if ( length(self$options$explanatory) == 1 ) {

                expl <- self$options$explanatory

                mydata[["myfactor"]] <- mydata[[expl]]

                }





                # Define Data For Analysis


                mydata <- jmvcore::naOmit(mydata)





                # Continious Explanatory ----
                #
                #
                #
                #                 if (length(self$options$explanatory) == 1 && inherits(self$options$explanatory, contin) ) {
                #
                #
                #                     todo <- glue::glue("
                #                                        <br>
                #                                        Continious Explanatory
                #                                        <br>
                #                                        <hr>")
                #                     html <- self$results$todo
                #                     html$setContent(todo)
                #
                #                 # numeric optimal cut-off ----
                #
                #
                #                 }
                #
                #
                #                 if (length(self$options$explanatory) > 1 && inherits(self$options$explanatory, contin) ) {
                #
                #                     todo <- glue::glue("
                #                         <br>Please use Multivariate Survival Analysis Cox-regression in jsurvival.
                #                         <br>
                #                         <hr>")
                #                     html <- self$results$todo
                #                     html$setContent(todo)
                #
                #                     stop("Please use Multivariate Survival Analysis Cox-regression in jsurvival")
                #
                #                 }
                #
                #
                #
                #














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

                for(i in seq_along(data_frame[,1,drop=T])) {
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
            dplyr::pull() -> survTableSummary



            self$results$survTableSummary$setContent(survTableSummary)





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


                if ( length(self$options$explanatory) == 1 && n_level < 3 ) {

                self$results$pairwiseTable$setVisible(FALSE)

                pairwiseSummary <- "No pairwise comparison when explanatory variable has < 3 levels."
                self$results$pairwiseSummary$setContent(pairwiseSummary)

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

    if(!sc)
        return()

    if (nrow(self$data) == 0)
    stop('Data contains no (complete) rows')

    if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$elapsedtime) )
        return()

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
                            subtitle = "Based on Kaplan-Meier estimates"
        )

    # plot <- plot + ggtheme

    print(plot)
    TRUE



}



# https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf
,
.plot2 = function(image2, ggtheme, theme, ...) {  # <-- the plot function ----


    ce <- self$options$ce

    if(!ce)
        return()

    if (nrow(self$data) == 0)
        stop('Data contains no (complete) rows')

    if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$elapsedtime) )
        return()

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
                            xlim = c(0,60),
                            title = paste0("Cumulative Events ", title2)
                            # subtitle = "Based on Kaplan-Meier estimates",
                            , fun = "event"
        )


    print(plot2)
    TRUE



}



,
.plot3 = function(image3, ggtheme, theme, ...) {  # <-- the plot function ----


    ch <- self$options$ch

    if(!ch)
        return()

    if (nrow(self$data) == 0)
        stop('Data contains no (complete) rows')

    if (is.null(self$options$explanatory) || is.null(self$options$outcome) || is.null(self$options$elapsedtime) )
        return()

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
                            xlim = c(0,60),
                            title = paste0("Cumulative Hazard ", title2),
                            fun = "cumhaz"
        )


    print(plot3)
    TRUE
}



        )
)
