#' @importFrom R6 R6Class
#' @import jmvcore
#' @import finalfit
#' @import survival
#' @import survminer

finalfitClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "finalfitClass",
    inherit = finalfitBase,
    private = list(
        .run = function() {

            if (length(self$options$explanatory) + length(self$options$outcome) + length(self$options$overalltime) < 3)
                return()

            # results 1

            mydata <- self$data

            myoveralltime <- self$options$overalltime

            myoveralltime <- jmvcore::toNumeric(self$data[[myoveralltime]])

            thefactor <- self$options$explanatory

            thefactor <- self$data[[thefactor]]

            myoutcome <- self$options$outcome

            myoutcome <- self$data[[myoutcome]]

            km_fit <- survival::survfit(survival::Surv(myoveralltime, myoutcome) ~ thefactor, data = mydata)

            results1 <- summary(km_fit)$table


            # results 1 summary

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

            results1summary <- km_fit_median_definition




            # results 2

            formula2 <- jmvcore::constructFormula(terms = self$options$explanatory)

            formula2 <- jmvcore::composeTerm(formula2)

            formulaL <- jmvcore::constructFormula(terms = self$options$overalltime)

            formulaR <- jmvcore::constructFormula(terms = self$options$outcome)

            myformula <- paste("Surv(", formulaL, ",", formulaR, ")")

            finalfit::finalfit(.data = mydata,
                               dependent = myformula,
                               explanatory = formula2) -> tUni

            results2 <- tUni


            # results 3



            results3 <- knitr::kable(tUni,
                                     row.names = FALSE,
                                     align = c('l', 'l', 'r', 'r', 'r', 'r'))


            # results 3 summary


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

            results3summary <- tUni_df_descr



            # results 4


            myplot <- finalfit::surv_plot(.data = mydata,
                                    dependent = myformula,
                                    explanatory = formula2,
                                    xlab = 'Time (months)',
                                    pval = TRUE,
                                    legend = 'none',
                                    break.time.by = 12,
                                    xlim = c(0,60)
                )


            results4 <- myplot



            # results 5


            km_fit_summary <- summary(km_fit, times = c(12,36,60))

            km_fit_df <- as.data.frame(km_fit_summary[c("strata", "time", "n.risk", "n.event", "surv", "std.err", "lower", "upper")])

            results5 <- km_fit_df


            # results 5 summary



            km_fit_df %>%
                dplyr::mutate(
                    description =
                        glue::glue(
                            "When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]."
                        )
                ) %>%
                dplyr::select(description) %>%
                pull() -> km_fit_definition

            results5summary <- km_fit_definition


            # results 6


            # pairwiseformula <- paste("Surv(", formulaL, ",", formulaR, ") ~ ", formula2)

            # results6 <-
            #     survminer::pairwise_survdiff(
            #     formula = pairwiseformula,
            #     data = mydata,
            #     p.adjust.method = "BH"
            # )


            # results 7

            results7 <- paste("20.01.2019 12:00")




            # results all


            results <- list(results1,
                            results1summary,
                            results2,
                            results3,
                            results3summary,
                            # results4,
                            results5,
                            results5summary,
                            # results6
                            results7)

            self$results$text$setContent(results)


        }


        )
)
