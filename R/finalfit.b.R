#' @importFrom R6 R6Class
#' @import jmvcore
#' @import finalfit
#' @import survival

finalfitClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "finalfitClass",
    inherit = finalfitBase,
    private = list(
        .run = function() {


            # TODO

            todo <- glue::glue(
                "This Module is still under development:
                -
                -  "
            )

            self$results$todo$setContent(todo)



            if (length(self$options$explanatory) + length(self$options$outcome) + length(self$options$overalltime) < 3)
                return()

            # Median Survival Table, results1

            mydata <- self$data

            myoveralltime <- self$options$overalltime

            myoveralltime <- jmvcore::toNumeric(self$data[[myoveralltime]])

            thefactor <- self$options$explanatory

            thefactor <- self$data[[thefactor]]

            myoutcome <- self$options$outcome

            myoutcome <- self$data[[myoutcome]]

            km_fit <- survival::survfit(survival::Surv(myoveralltime, myoutcome) ~ thefactor, data = mydata)

            results1 <- summary(km_fit)$table

            # km_fit_median_df <- summary(km_fit)
            # km_fit_median_df <- as.data.frame(km_fit_median_df$table) %>%
            #     janitor::clean_names(dat = ., case = "snake") %>%
            #     tibble::rownames_to_column(.data = .)

            results1 <- tibble::as_tibble(results1,
                                         .name_repair = "minimal") %>%
                    janitor::clean_names(dat = ., case = "snake")


            # Median Survival Table Html Type, results1html


            results1html <- knitr::kable(results1,
                                     row.names = FALSE,
                                     align = c('l', 'l', 'r', 'r', 'r', 'r'),
                                     format = "html")







            # results 2

            km_fit_median_df <- summary(km_fit)
            km_fit_median_df <- as.data.frame(km_fit_median_df$table) %>%
                janitor::clean_names(dat = ., case = "snake") %>%
                tibble::rownames_to_column(.data = .)


            km_fit_median_df %>%
                dplyr::mutate(
                    description =
                        glue::glue(
                            "When {rowname}, median survival is {median} [{x0_95lcl} - {x0_95ucl}, 95% CI] months."
                        )
                ) %>%
                dplyr::select(description) %>%
                pull() -> km_fit_median_definition

            results2 <- km_fit_median_definition




            # results 3

            formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)

            formula2 <- jmvcore::composeTerm(formula2)

            formulaL <- jmvcore::constructFormula(terms = self$options$overalltime)

            formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

            myformula <- paste("Surv(", formulaL, ",", formulaR, ")")




            finalfit::finalfit(.data = mydata,
                               dependent = myformula,
                               explanatory = formula2) -> tUni

            results3 <- tUni


            # results 4



            results4 <- knitr::kable(tUni,
                                     row.names = FALSE,
                                     align = c('l', 'l', 'r', 'r', 'r', 'r'),
                                     format = "html")


            # results 5


            tUni_df <- tibble::as_tibble(tUni, .name_repair = "minimal") %>%
                janitor::clean_names(dat = ., case = "snake")

            tUni_df_descr <- paste0("When ",
                                    tUni_df$dependent_surv_overall_time_outcome[1],
                                    " is ",
                                    tUni_df$x[2],
                                    ", there is ",
                                    tUni_df$hr_univariable[2],
                                    " times risk than ",
                                    "when ",
                                    tUni_df$dependent_surv_overall_time_outcome[1],
                                    " is ",
                                    tUni_df$x[1],
                                    "."
            )

            results5 <- tUni_df_descr



            # results 6


            km_fit_summary <- summary(km_fit, times = c(12,36,60))

            km_fit_df <- as.data.frame(km_fit_summary[c("strata", "time", "n.risk", "n.event", "surv", "std.err", "lower", "upper")])

            results6 <- km_fit_df


            # results 7



            km_fit_df %>%
                dplyr::mutate(
                    description =
                        glue::glue(
                            "When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]."
                        )
                ) %>%
                dplyr::select(description) %>%
                pull() -> km_fit_definition

            results7 <- km_fit_definition


            # results 8


            formula_p1 <- jmvcore::constructFormula(terms = self$options$overalltime)

            formula_p3 <- jmvcore::constructFormula(terms = self$options$explanatory)

            formula_p2 <- jmvcore::constructFormula(terms = self$options$outcome)


            formula_p <- paste('Surv(', formula_p1, ',',  formula_p2, ') ~ ', formula_p3)

            formula_p <- as.formula(formula_p)


            results8 <-
                survminer::pairwise_survdiff(
                formula = formula_p,
                data = self$data,
                p.adjust.method = "BH"
            )


            # results

            self$results$text1$setContent(results1)

            self$results$text1html$setContent(results1html)

            self$results$text2$setContent(results2)

            self$results$text3$setContent(results3)

            self$results$text4$setContent(results4)

            self$results$text5$setContent(results5)

            self$results$text6$setContent(results6)

            self$results$text7$setContent(results7)

            self$results$text8$setContent(results8)



            # Prepare plotData


            my_overalltime <- self$data[[self$options$overalltime]]
            my_outcome <- self$data[[self$options$outcome]]
            my_factor <- self$data[[self$options$explanatory]]


            plotData <- data.frame(ovt = my_overalltime,
                                   out = my_outcome,
                                   fct = my_factor)

            plotData <- jmvcore::naOmit(plotData)

            image <- self$results$plot

            image$setState(plotData)


        },

        .plot=function(image, ...) {  # <-- the plot function

            if (length(self$options$explanatory) + length(self$options$outcome) + length(self$options$overalltime) < 3)
                return()


            plotData <- image$state


            # plotData[['ovt2']] <- jmvcore::toNumeric(plotData[['ovt']])

            # plot1 <- hist(plotData[['ovt2']])

            # plot2 <- hist(plotData[['out']])

            # plot3 <- hist(plotData[['fct']])

            # plot <- list(
            #     plot1,
            #     plot2,
            #     plot3
            # )





            # myexplanatory <- 'fct'
            # mydependent <- 'Surv(ovt, out)'

            # mydependent <- survival::Surv(
            #     jmvcore::toNumeric(plotData[['ovt']]), plotData[['out']])

            # plot <- plotData %>%
            #     finalfit::surv_plot(.data = .,
            #                               dependent = mydependent,
            #                               explanatory = myexplanatory,
            #                               xlab = 'Time (months)',
            #                               pval = TRUE,
            #                               legend = 'none',
            #                               break.time.by = 12,
            #                               xlim = c(0,60)
            # )


            # https://rpkgs.datanovia.com/survminer/reference/ggsurvplot.html
            # dependentKM <- "Surv(OverallTime, Outcome)"
            # explanatoryKM <- "LVI"
            #
            # mydata %>%
            #     finalfit::surv_plot(.data = .,
            #                         dependent = dependentKM,
            #                         explanatory = explanatoryKM,
            #                         xlab='Time (months)',
            #                         pval=TRUE,
            #                         legend = 'none',
            #                         break.time.by = 12,
            #                         xlim = c(0,60)
            #                         # legend.labs = c('a','b')
            #     )


            # print(plot)
            # TRUE


        }


        )
)
