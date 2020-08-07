


# This file is a generated template, your changes will not be overwritten

decision2Class <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "decision2Class",
        inherit = decision2Base,
        private = list(


            .init = function() {
                # private$.initcTable()

            },


            .run = function() {

                if (is.null(self$options$newtest) || is.null(self$options$gold) )
                    return()

                if (nrow(self$data) == 0) stop("Data contains no (complete) rows")



                results <- private$.compute()
                private$.populatecTable(results)

            },


            .initcTable = function() {
                cTable <- self$results$cTable

            },


            .compute = function() {
                # Data definition ----
                mydata <- self$data

                mydata <- jmvcore::naOmit(mydata)

                testPLevel <-
                    jmvcore::constructFormula(terms = self$options$testPositive)

                testPLevel <-
                    jmvcore::decomposeFormula(formula = testPLevel)

                testPLevel <- unlist(testPLevel)


                testVariable <-
                    jmvcore::constructFormula(terms = self$options$newtest)

                testVariable <-
                    jmvcore::decomposeFormula(formula = testVariable)

                testVariable <- unlist(testVariable)


                goldPLevel <-
                    jmvcore::constructFormula(terms = self$options$goldPositive)

                goldPLevel <-
                    jmvcore::decomposeFormula(formula = goldPLevel)

                goldPLevel <- unlist(goldPLevel)


                goldVariable <-
                    jmvcore::constructFormula(terms = self$options$gold)

                goldVariable <-
                    jmvcore::decomposeFormula(formula = goldVariable)

                goldVariable <- unlist(goldVariable)

                mydata[[testVariable]] <-
                    forcats::as_factor(mydata[[testVariable]])

                mydata[[goldVariable]] <-
                    forcats::as_factor(mydata[[goldVariable]])


                # Recode ----

                mydata2 <- mydata

                mydata2 <-
                    mydata2 %>% dplyr::mutate(
                        testVariable2 = dplyr::case_when(
                            .data[[testVariable]] ==
                                self$options$testPositive ~ "Positive",
                            NA ~ NA_character_,
                            TRUE ~
                                "Negative"
                        )
                    ) %>%
                    dplyr::mutate(
                        goldVariable2 = dplyr::case_when(
                            .data[[goldVariable]] ==
                                self$options$goldPositive ~ "Positive",
                            NA ~ NA_character_,
                            TRUE ~
                                "Negative"
                        )
                    )

                mydata2 <-
                    mydata2 %>%
                    dplyr::mutate(testVariable2 = forcats::fct_relevel(testVariable2, "Positive")) %>%
                    dplyr::mutate(goldVariable2 = forcats::fct_relevel(goldVariable2, "Positive"))




                # conf_table ----

                conf_table <-
                    table(mydata2[["testVariable2"]], mydata2[["goldVariable2"]])


                # Caret ----


                TP <- conf_table[1, 1]

                FP <- conf_table[1, 2]

                FN <- conf_table[2, 1]

                TN <- conf_table[2, 2]


                return(list(
                    "TP" = TP,
                    "FP" = FP,
                    "FN" = FN,
                    "TN" = TN
                ))

            },





            .populatecTable = function(results) {

                cTable <- self$results$cTable


                cTable$addRow(
                    rowKey = "Test Positive",
                    values = list(
                        newtest = "Test Positive",
                        GP = results$TP,
                        GN = results$FP,
                        Total = results$TP + results$FP
                    )
                )


                cTable$addRow(
                    rowKey = "Test Negative",
                    values = list(
                        newtest = "Test Negative",
                        GP = results$FN,
                        GN = results$TN,
                        Total = results$FN + results$TN
                    )
                )

                cTable$addRow(
                    rowKey = "Total",
                    values = list(
                        newtest = "Total",
                        GP = results$TP + results$FN,
                        GN = results$FP + results$TN,
                        Total = results$TP + results$FP + results$FN + results$TN
                    )
                )



            }


        )
    )
