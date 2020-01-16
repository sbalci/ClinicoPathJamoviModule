
# This file is a generated template, your changes will not be overwritten

decisionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "decisionClass",
    inherit = decisionBase,
    private = list(
        .run = function() {


            # Data definition

            mydata <- self$data

            goldVariable <- self$options$gold

            # goldVariable <- self$data[[goldVariable]]

            goldPLevel <- self$options$goldPositive



            testVariable <- self$options$newtest

            # testVariable <- self$data[[testVariable]]

            testPLevel <- self$options$testPositive

            conftable <- table(mydata[[testVariable]], mydata[[goldVariable]])

            conftable_df <- as.data.frame(conftable)


            results1df <- conftable_df

            # results1df <- results1df[[Freq]][results1df[[Var1]] == testVariable]



            # Table



            results1 <- conftable


            # Table 2


            goldPLevel <- jmvcore::constructFormula(terms = "goldPLevel")
            goldPLevel <- jmvcore::decomposeFormula(formula = goldPLevel)
            goldPLevel <- unlist(goldPLevel)


            testPLevel <- jmvcore::constructFormula(terms = "testPLevel")
            testPLevel <- jmvcore::decomposeFormula(formula = testPLevel)
            testPLevel <- unlist(testPLevel)


            TP <- conftable[1,2]

            results1TP <- TP







            # Caret

            mydata2 <- mydata %>%
                mutate(
                    goldVariable = case_when(
                        goldVariable == goldPLevel ~ "Positive",
                        NA ~ NA_character_,
                        TRUE ~ "Negative"
                    )
                ) %>%
                mutate(
                    testVariable = case_when(
                        testVariable == testPLevel ~ "Positive",
                        NA ~ NA_character_,
                        TRUE ~ "Negative"
                    )
                )

            # goldVariable <- forcats::as_factor(goldVariable)
            # testVariable <- forcats::as_factor(testVariable)
            #
            # goldPLevel <- forcats::as_factor(goldPLevel)
            # testPLevel <- forcats::as_factor(testPLevel)





            # sens <- caret::sensitivity(mydata, goldVariable, positive = goldPLevel)

            # caret::specificity(mydata, goldVariable, negative = levels(reference)[2])

            # PPV <- caret::posPredValue(mydata, goldVariable, positive = goldPLevel)

            # caret::negPredValue(mydata, goldVariable, negative = levels(reference)[2])


            # summary_caret <- glue::glue("Sensitivity is {sens}.
                               # PPV is {PPV}.")


            results2 <- table(mydata2[[testVariable]], mydata2[[goldVariable]])

            # results2 <- caret::confusionMatrix(conftable)


            # Results

            results1 <- list(results1,
                             results1df,
                             results1TP)

            self$results$text1$setContent(results2)

            # self$results$text2$setContent(results2)


            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
