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

            myexplanatory <- self$options$explanatory

            myexplanatory <- self$data[[myexplanatory]]

            myoutcome <- self$options$outcome

            myoutcome <- self$data[[myoutcome]]

            km_fit <- survival::survfit(survival::Surv(myoveralltime, myoutcome) ~ myexplanatory, data = mydata)

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


            # results


            results <- list(results1,
                            results1summary,
                            results2,
                            results3,
                            results4)

            self$results$text$setContent(results)

        })
)
