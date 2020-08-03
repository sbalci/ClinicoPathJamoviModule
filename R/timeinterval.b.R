
# This file is a generated template, your changes will not be overwritten

timeintervalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "timeintervalClass",
    inherit = timeintervalBase,
    private = list(
        .run = function() {


            mydata <- self$data

            overalltime <- self$options$overalltime
            outcome <- self$options$outcome
            explanatory <- self$options$explanatory

            outcome1 <- mydata[[outcome]]

            contin <- c("integer", "numeric", "double")

            if (inherits(outcome1, contin)) {


                if ( !(
                    (length(unique(outcome1[!is.na(outcome1)])) == 2) && (sum(unique(outcome1[!is.na(outcome1)])) == 1)
                    ) ) {
                    stop('When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')

                }

                mydata[[outcome]] <- jmvcore::toNumeric(mydata[[outcome]])

            } else if (inherits(outcome1, "factor")) {

                outcomeLevel <- self$options$outcomeLevel

                mydata[[outcome]] <-
                    ifelse(test = outcome1 == outcomeLevel,
                           yes = 1,
                           no = 0)
            } else {

                stop('When using continuous variable as an outcome, it must only contain 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0. If you are using a factor as an outcome, please check the levels and content.')

            }


            # Multiexplanatory ----

            # km_fit <- survfit(Surv(OverallTime, Outcome) ~ LVI + PNI, data = deneme)


            thefactor <- jmvcore::constructFormula(terms = explanatory)

            formula <- paste('survival::Surv(', self$options$overalltime, ',', self$options$outcome, ') ~ ', thefactor)
            formula <- as.formula(formula)


            km_fit <- survival::survfit(formula, data = self$data)

            km_fit_median_df <- summary(km_fit)

            results1html <- as.data.frame(km_fit_median_df$table) %>%
                janitor::clean_names(dat = ., case = "snake") %>%
                tibble::rownames_to_column(.data = .)


            results1html[,1] <- gsub(pattern = "thefactor=",
                                     replacement = "",
                                     x = results1html[,1])

            results1table <- results1html



            self$results$text1table$setContent(results1table)



            # Median Table ----


            names(results1table)[1] <- "factor"


            medianTable <- self$results$medianTable

            data_frame <- results1table
            for (i in seq_along(data_frame[,1,drop = T])) {
                medianTable$addRow(rowKey = i, values = c(data_frame[i,]))
            }






            # results 2 Median Survival Summary ----

            km_fit_median_df <- summary(km_fit)
            km_fit_median_df <- as.data.frame(km_fit_median_df$table) %>%
                janitor::clean_names(dat = ., case = "snake") %>%
                tibble::rownames_to_column(.data = .)

            km_fit_median_df %>%
                dplyr::mutate(
                    description =
                        glue::glue(
                            "When {rowname}, median survival is {round(median, digits = 1)} [{round(x0_95lcl, digits = 1)} - {round(x0_95ucl, digits = 1)}, 95% CI] months."
                        )
                ) %>%
                dplyr::select(description) %>%
                dplyr::pull() -> km_fit_median_definition

            results2 <- km_fit_median_definition


            self$results$text2$setContent(results2)









            # Denemeler ----

            # mydata <- jmvcore::select(self$data, c(overalltime, outcome, explanatory))

            denemeler <- list(
                head(mydata),
                typeof(outcome1),
                class(outcome1)
            )



            self$results$text1$setContent(denemeler)

        })
)







# Denemeler ----



# denemeler <- list(
#     names(thefactor),
#     typeof(thefactor),
#     class(thefactor),
#     length(thefactor),
#     jmvcore::constructFormula(terms = thefactor),
#     jmvcore::composeTerms(thefactor),
#     jmvcore::composeFormula(thefactor)
# )


# denemeler <- list(
#     as.vector(time),
#     as.vector(event),
#     as.vector(expl),
#     jmvcore::constructFormula(terms = expl)
# )


# # Check if outcome variable is suitable or stop
# myoutcome2 <- self$options$outcome
# myoutcome2 <- self$data[[myoutcome2]]
# myoutcome2 <- na.omit(myoutcome2)
# # if ( !is.numeric(myoutcome2) || any(myoutcome2 != 0 & myoutcome2 != 1))
# if (any(myoutcome2 != 0 & myoutcome2 != 1))
#     stop('Outcome variable must only contains 1s and 0s. If patient is dead or event (recurrence) occured it is 1. If censored (patient is alive or free of disease) at the last visit it is 0.')
# # self$results$deneme$setContent(head(mydata))
# # self$results$deneme2$setContent(head(mydata))



