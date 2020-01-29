
# This file is a generated template, your changes will not be overwritten

decisioncalculatorClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "decisioncalculatorClass",
    inherit = decisioncalculatorBase,
    private = list(
        .run = function() {

            TP <- self$options$TP

            FP <- self$options$FP

            TN <- self$options$TN

            FN <- self$options$FN

            table1 <- matrix(c(TP, FP, FN, TN), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("Test Positive", "Test Negative"), c("Gold Positive","Gold Negative")))


            self$results$text1$setContent(table1)

            table2 <- matrix(c(TP, FP, FN, TN), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("Positive", "Negative"), c("Positive","Negative")))

            table3 <- as.table(table2)

            caretresult <- caret::confusionMatrix(table3)

            self$results$text2$setContent(caretresult)






            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)