#' @title Medical Decision Making
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'

decisionClass <- if (requireNamespace("jmvcore")) R6::R6Class("decisionClass",
    inherit = decisionBase, private = list(.run = function() {


        # # Error Message ----
        #
        # if (nrow(self$data) == 0) stop("Data contains no (complete) rows")
        #
        # if ( (is.null(self$options$vars) || is.null(self$options$facs)) && is.null(self$options$target) ) {
        #     # ToDo Message ----
        #     todo <- "
        #         <br>Welcome to ClinicoPath
        #                   <br><br>
        #                   This tool will help you form an Alluvial Plots.
        #                   "
        #     html <- self$results$todo
        #     html$setContent(todo)
        #
        # } else {
        #     todo <- ""
        #     html <- self$results$todo
        #     html$setContent(todo)
        #
        #
        #
        # }








        # TODO

        # todo <- glue::glue( 'This Module is still under development - - ' )
        # self$results$todo$setContent(todo)


        if (length(self$options$testPositive) + length(self$options$newtest) +
            length(self$options$goldPositive) + length(self$options$gold) <
            4) return()

        if (nrow(self$data) == 0) stop("Data contains no (complete) rows")




        # Data definition ----
        mydata <- self$data

        mydata <- jmvcore::naOmit(mydata)

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

        # Table 1 ----

        results1 <- mydata %>% dplyr::select(.data[[testVariable]], .data[[goldVariable]]) %>%
            table()

        self$results$text1$setContent(results1)


        # Recode ----

        mydata2 <- mydata

        mydata2 <- mydata2 %>% dplyr::mutate(testVariable2 = dplyr::case_when(.data[[testVariable]] ==
            self$options$testPositive ~ "Positive", NA ~ NA_character_, TRUE ~
            "Negative")) %>%
        dplyr::mutate(goldVariable2 = dplyr::case_when(.data[[goldVariable]] ==
            self$options$goldPositive ~ "Positive", NA ~ NA_character_, TRUE ~
            "Negative"))

        mydata2 <- mydata2 %>% dplyr::mutate(testVariable2 = forcats::fct_relevel(testVariable2,
            "Positive")) %>% dplyr::mutate(goldVariable2 = forcats::fct_relevel(goldVariable2,
            "Positive"))




        # conf_table ----

        conf_table <- table(mydata2[["testVariable2"]], mydata2[["goldVariable2"]])


        # Caret ----
        # results_caret <- caret::confusionMatrix(conf_table, positive = "Positive")


        # self$results$text2$setContent(
        #     list(
        #         conf_table,
        #         results_caret
        #         )
        # )

        TP <- conf_table[1,1]

        FP <- conf_table[1,2]

        FN <- conf_table[2,1]

        TN <- conf_table[2,2]




        # Cross Table in jamovi style ----

        cTable <- self$results$cTable


        cTable$addRow(rowKey = "Test Positive",
                      values = list(
                          newtest = "Test Positive",
                          GP = TP,
                          GN = FP,
                          Total = TP + FP
                      )
        )


        cTable$addRow(rowKey = "Test Negative",
                      values = list(
                          newtest = "Test Negative",
                          GP = FN,
                          GN = TN,
                          Total = FN + TN
                      )
        )

        cTable$addRow(rowKey = "Total",
                      values = list(
                          newtest = "Total",
                          GP = TP + FN,
                          GN = FP + TN,
                          Total = TP + FP + FN + TN
                      )
        )





        # Self Calculations ----

        # Self Calculation https://cran.r-project.org/web/packages/caret/caret.pdf
        # https://online.stat.psu.edu/stat509/node/150/

        # https://en.wikipedia.org/wiki/Sensitivity_and_specificity

        TotalPop <- TP + TN + FP + FN

        DiseaseP <- TP + FN

        DiseaseN <- TN + FP

        TestP <- TP + FP

        TestN <- TN + FN

        TestT <- TP + TN

        TestW <- FP + FN

        Sens <- TP/DiseaseP

        Spec <- TN/DiseaseN

        AccurT <- TestT/TotalPop

        PrevalenceD <- DiseaseP/TotalPop

        PPV <- TP/TestP

        NPV <- TN/TestN


        pp <- self$options$pp
        pprob <- self$options$pprob

        if (pp) {
            # Known prior probability from population
            PriorProb <- pprob
        } else {
            # From ConfusionMatrix
            PriorProb <- PrevalenceD
        }


        PostTestProbDisease <- (PriorProb * Sens)/((PriorProb * Sens) + ((1 -
                                                                              PriorProb) * (1 - Spec)))



        PostTestProbHealthy <- ((1 - PriorProb) * Spec)/(((1 - PriorProb) *
                                                              Spec) + (PriorProb * (1 - Sens)))




        LRP <- Sens / (1 - Spec)

        LRN <- (1 - Sens) / Spec






        # nTable Populate Table ----

        nTable <- self$results$nTable
        nTable$setRow(rowNo = 1,
                      values = list(
                          tablename = "n",
                          TotalPop = TotalPop,
                          DiseaseP = DiseaseP,
                          DiseaseN = DiseaseN,
                          TestP = TestP,
                          TestN = TestN,
                          TestT = TestT,
                          TestW = TestW
                      )
        )

        # ratioTable Populate Table ----


        ratioTable <- self$results$ratioTable
        ratioTable$setRow(rowNo = 1,
                          values = list(
                              tablename = "Ratios",
                              Sens = Sens,
                              Spec = Spec,
                              AccurT = AccurT,
                              PrevalenceD = PriorProb,
                              PPV = PPV,
                              NPV = NPV,
                              PostTestProbDisease = PostTestProbDisease,
                              PostTestProbHealthy = PostTestProbHealthy,
                              LRP = LRP,
                              LRN = LRN
                          )
        )

        # nTable footnotes ----

        if (self$options$fnote) {

            # nTable$addFootnote(rowKey = "1", col = "TotalPop", "Total Population")

            nTable$addFootnote(rowNo = 1, col = "TotalPop", "Total Number of Subjects")

            nTable$addFootnote(rowNo = 1, col = "DiseaseP", "Total Number of Subjects with Disease")

            nTable$addFootnote(rowNo = 1, col = "DiseaseN", "Total Number of Healthy Subjects")

            nTable$addFootnote(rowNo = 1, col = "TestP", "Total Number of Positive Tests")

            nTable$addFootnote(rowNo = 1, col = "TestN", "Total Number of Negative Tests")

            nTable$addFootnote(rowNo = 1, col = "TestT", "Total Number of True Test Results")

            nTable$addFootnote(rowNo = 1, col = "TestW", "Total Number of Wrong Test Results")


        }


        # ratioTable footnotes ----


        if (self$options$fnote) {

            ratioTable$addFootnote(rowNo = 1, col = "Sens", "Sensitivity (True Positives among Diseased)")

            ratioTable$addFootnote(rowNo = 1, col = "Spec", "Specificity (True Negatives among Healthy)")

            ratioTable$addFootnote(rowNo = 1, col = "AccurT", "Accuracy (True Test Result Ratio)")

            ratioTable$addFootnote(rowNo = 1, col = "PrevalenceD", "Disease Prevalence in this population")

            ratioTable$addFootnote(rowNo = 1, col = "PPV", "Positive Predictive Value (Probability of having disease after a positive test using this experimental population)")

            ratioTable$addFootnote(rowNo = 1, col = "NPV", "Negative Predictive Value (Probability of being healthy after a negative test using this experimental population)")

            ratioTable$addFootnote(rowNo = 1, col = "PostTestProbDisease", "Post-test Probability of Having Disease  (Probability of having disease after a positive test using known Population Prevalence)")

            ratioTable$addFootnote(rowNo = 1, col = "PostTestProbHealthy", "Post-test Probability of Being Healthy (Probability of being healthy after a negative test using known Population Prevalence)")

            # ratioTable$addFootnote(rowNo = 1, col = "LRP", "")

            # ratioTable$addFootnote(rowNo = 1, col = "LRN", "")


        }




        # Reorganize Table



        # caretresult[['positive']]
        # caretresult[['table']]
        # caretresult[['overall']]
        # caretresult[['overall']][['Accuracy']]
        # caretresult[['overall']][['Kappa']]
        # caretresult[['overall']][['AccuracyLower']]
        # caretresult[['overall']][['AccuracyUpper']]
        # caretresult[['overall']][['AccuracyNull']]
        # caretresult[['overall']][['AccuracyPValue']]
        # caretresult[['overall']][['McnemarPValue']]
        # caretresult[['byClass']]
        # caretresult[['byClass']][['Sensitivity']]
        # caretresult[['byClass']][['Specificity']]
        # caretresult[['byClass']][['Pos Pred Value']]
        # caretresult[['byClass']][['Neg Pred Value']]
        # caretresult[['byClass']][['Precision']]
        # caretresult[['byClass']][['Recall']] caretresult[['byClass']][['F1']]
        # caretresult[['byClass']][['Prevalence']]
        # caretresult[['byClass']][['Detection Rate']]
        # caretresult[['byClass']][['Detection Prevalence']]
        # caretresult[['byClass']][['Balanced Accuracy']] caretresult[['mode']]
        # caretresult[['dots']]




        # Write Summary







        # 95% CI ----

        ci <- self$options$ci

        if (ci) {



        # epiR ----


        epirresult <- epiR::epi.tests(dat = conf_table)


        epirresult2 <- summary(epirresult)
        epirresult2 <- as.data.frame(epirresult2) %>%
            tibble::rownames_to_column(.data = ., var = 'statsabv')


        epirresult2$statsnames <-
            c(
                "Apparent prevalence",
                "True prevalence",
                "Test sensitivity",
                "Test specificity",
                "Diagnostic accuracy",
                "Diagnostic odds ratio",
                "Number needed to diagnose",
                "Youden's index",
                "Positive predictive value",
                "Negative predictive value",
                "Likelihood ratio of a positive test",
                "Likelihood ratio of a negative test",
                "Proportion of subjects with the outcome ruled out",
                "Proportion of subjects with the outcome ruled in",
                "Proportion of false positives",
                "Proportion of false negative"
            )

        ratiorows <- c(
            "aprev",
            "tprev",
            "se",
            "sp",
            "diag.acc",
            "ppv",
            "npv",
            "pro",
            "pri",
            "pfp",
            "pfn"
        )


        numberrows <- c(
            "diag.or",
            "nnd",
            "youden",
            "plr",
            "nlr"
        )

        epirresult_number <- epirresult2[epirresult2$statsabv %in% numberrows, ]

        epirresult_ratio <- epirresult2[epirresult2$statsabv %in% ratiorows, ]





        # epirTable_ratio -----

        epirTable_ratio <- self$results$epirTable_ratio

        data_frame <- epirresult_ratio
        for(i in seq_along(data_frame[,1,drop=T])) {
            epirTable_ratio$addRow(rowKey = i, values = c(data_frame[i,])) # This code produces a named vector/list, which is what the values argument expects
        }



        # epirTable_ratio footnotes ----

        if (self$options$fnote) {

            epirTable_ratio$addFootnote(
                rowNo = 5,
                col = "statsnames",
                "Proportion of all tests that give a correct result."
            )
        }






        # epirTable_number ----


        epirTable_number <- self$results$epirTable_number

        data_frame <- epirresult_number
        for(i in seq_along(data_frame[,1,drop=T])) {
            epirTable_number$addRow(rowKey = i, values = c(data_frame[i,]))
        }



        # epirTable_number footnotes ----

        if (self$options$fnote) {


            epirTable_number$addFootnote(
                rowNo = 1,
                col = "statsnames",
                "How much more likely will the test make a correct diagnosis than an incorrect diagnosis in patients with the disease."
            )

            epirTable_number$addFootnote(
                rowNo = 2,
                col = "statsnames",
                "Number of patients that need to be tested to give one correct positive test."
            )


            epirTable_number$addFootnote(
                rowNo = 3,
                col = "statsnames",
                "Youden's index is the difference between the true positive rate and the false positive rate. Youden's index ranges from -1 to +1 with values closer to 1 if both sensitivity and specificity are high (i.e. close to 1)."

            )

        }


        }


        # drafts ----


        # matrixdetails <- list(results_caret[["positive"]], results_caret[["table"]],
        #     results_caret[["overall"]], results_caret[["overall"]][["Accuracy"]],
        #     results_caret[["overall"]][["Kappa"]], results_caret[["overall"]][["AccuracyLower"]],
        #     results_caret[["overall"]][["AccuracyUpper"]], results_caret[["overall"]][["AccuracyNull"]],
        #     results_caret[["overall"]][["AccuracyPValue"]], results_caret[["overall"]][["McnemarPValue"]],
        #     results_caret[["byClass"]], results_caret[["byClass"]][["Sensitivity"]],
        #     results_caret[["byClass"]][["Specificity"]], results_caret[["byClass"]][["Pos Pred Value"]],
        #     results_caret[["byClass"]][["Neg Pred Value"]], results_caret[["byClass"]][["Precision"]],
        #     results_caret[["byClass"]][["Recall"]], results_caret[["byClass"]][["F1"]],
        #     results_caret[["byClass"]][["Prevalence"]], results_caret[["byClass"]][["Detection Rate"]],
        #     results_caret[["byClass"]][["Detection Prevalence"]], results_caret[["byClass"]][["Balanced Accuracy"]],
        #     results_caret[["mode"]], results_caret[["dots"]])

        # self$results$text3$setContent(matrixdetails)


        # Individual analysis

        # sens <- caret::sensitivity(conf_table, positive = 'Positive')

        # PPV <- caret::posPredValue(conf_table, positive = 'Positive')

        # summary_caret <- glue::glue('Sensitivity is {sens}.  PPV is {PPV}.')

        # self$results$text4$setContent(summary_caret)


        # bdpv ---- https://cran.r-project.org/web/packages/bdpv/bdpv.pdf



        # epiR ---- https://cran.r-project.org/web/packages/epiR/epiR.pdf


        # dat <- as.table( matrix(c(670,202,74,640), nrow = 2, byrow = TRUE) )

        # colnames(dat) <- c('Dis+','Dis-') rownames(dat) <- c('Test+','Test-')

        # rval <- epiR::epi.tests(dat, conf.level = 0.95)

        # rval <- list( dat, rval, print(rval), summary(rval) )

        # self$results$text5$setContent(rval)



        # Prior Probability

        # lvs <- c('normal', 'abnormal') truth <- factor(rep(lvs, times = c(86,
        # 258)), levels = rev(lvs)) pred <- factor( c( rep(lvs, times = c(54,
        # 32)), rep(lvs, times = c(27, 231))), levels = rev(lvs)) xtab <-
        # table(pred, truth) confusionMatrix(xtab) confusionMatrix(pred, truth)
        # confusionMatrix(xtab, prevalence = 0.25) ## 3 class example
        # confusionMatrix(iris$Species, sample(iris$Species)) newPrior <- c(.05,
        # .8, .15) names(newPrior) <- levels(iris$Species)
        # confusionMatrix(iris$Species, sample(iris$Species))




    }))
