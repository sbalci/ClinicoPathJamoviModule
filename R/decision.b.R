#' @importFrom R6 R6Class
#' @import jmvcore
#'
# This file is a generated template, your changes will not be overwritten

decisionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "decisionClass",
    inherit = decisionBase,
    private = list(
        .run = function() {

            # TODO

            todo <- glue::glue(
                "This Module is still under development
                -
                -  "
            )

            self$results$todo$setContent(todo)



            description <- "Description of Module"


            if (length(self$options$testPositive) + length(self$options$newtest) + length(self$options$goldPositive) + length(self$options$gold) < 4)
                return()






            # Data definition
            mydata <- self$data


            testPLevel <- jmvcore::constructFormula(terms = self$options$testPositive)

            testPLevel <- jmvcore::decomposeFormula(formula = testPLevel)

            testPLevel <- unlist(testPLevel)


            testVariable <- jmvcore::constructFormula(terms = self$options$newtest)

            testVariable <- jmvcore::decomposeFormula(formula = testVariable)

            testVariable <- unlist(testVariable)


            goldPLevel <- jmvcore::constructFormula(terms = self$options$goldPositive)

            goldPLevel <- jmvcore::decomposeFormula(formula = goldPLevel)

            goldPLevel <- unlist(goldPLevel)


            goldVariable <- jmvcore::constructFormula(terms = self$options$gold)

            goldVariable <- jmvcore::decomposeFormula(formula = goldVariable)

            goldVariable <- unlist(goldVariable)

            mydata[[testVariable]] <- forcats::as_factor(mydata[[testVariable]])

            mydata[[goldVariable]] <- forcats::as_factor(mydata[[goldVariable]])

            # Table 1

            # Table1 <- table(mydata[[testVariable]], mydata[[goldVariable]])




            Table1 <- mydata %>%
                janitor::tabyl(.data[[testVariable]], .data[[goldVariable]]) %>%
                janitor::adorn_totals(c("row", "col")) %>%
                janitor::adorn_percentages("row") %>%
                janitor::adorn_pct_formatting(rounding = "half up", digits = 1) %>%
                janitor::adorn_ns() %>%
                janitor::adorn_title("combined")
            # %>%
            #     knitr::kable()



            results1 <- Table1

            # Recode

            mydata2 <- mydata

            mydata2 <- mydata2 %>%
                filter(complete.cases(.)) %>%
                dplyr::mutate(
                    testVariable2 =
                    dplyr::case_when(
                        .data[[testVariable]] == self$options$testPositive ~ "Positive",
                        NA ~ NA_character_,
                        TRUE ~ "Negative"
                    )
                ) %>%

                dplyr::mutate(
                    goldVariable2 =
                        dplyr::case_when(
                            .data[[goldVariable]] == self$options$goldPositive ~ "Positive",
                            NA ~ NA_character_,
                            TRUE ~ "Negative"
                        )
                )

            mydata2 <- mydata2 %>%
                dplyr::mutate(
                    testVariable2 = forcats::fct_relevel(testVariable2, "Positive")
                ) %>%
                dplyr::mutate(
                    goldVariable2 = forcats::fct_relevel(goldVariable2, "Positive")
                )


            # Caret

            conf_table <- table(mydata2[["testVariable2"]], mydata2[["goldVariable2"]])


            results_caret <- caret::confusionMatrix(conf_table, positive = "Positive")





            # sens <- caret::sensitivity(mydata2, reference = "goldVariable2", positive = "Positive")

            # caret::specificity(mydata, goldVariable, negative = levels(reference)[2])

            # PPV <- caret::posPredValue(mydata, goldVariable, positive = goldPLevel)

            # caret::negPredValue(mydata, goldVariable, negative = levels(reference)[2])


            # summary_caret <- glue::glue("Sensitivity is {sens}.
                               # PPV is {PPV}.")


            # Results

            results <- list(
                description,
                results1,
                results_caret
            )

            self$results$text1$setContent(results)


            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
