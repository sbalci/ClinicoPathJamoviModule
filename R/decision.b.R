
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

            results1df <- results1df[[Freq]][results1df[[Var1]] == testVariable]



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


            # sens <- caret::sensitivity(mydata, goldVariable, positive = goldPLevel)

            # caret::specificity(mydata, goldVariable, negative = levels(reference)[2])

            # PPV <- caret::posPredValue(mydata, goldVariable, positive = goldPLevel)

            # caret::negPredValue(mydata, goldVariable, negative = levels(reference)[2])


            # summary_caret <- glue::glue("Sensitivity is {sens}.
                               # PPV is {PPV}.")



            # results2 <- summary_caret























            # Results

            results1 <- list(results1,
                             results1df,
                             results1TP)

            self$results$text1$setContent(results1)

            # self$results$text2$setContent(results2)









            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
