#' @title Decision Calculator
#'
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom utils data
#'

decisioncalculatorClass <- if (requireNamespace("jmvcore")) R6::R6Class("decisioncalculatorClass",
    inherit = decisioncalculatorBase, private = list(



        .init = function() {

            cTable <- self$results$cTable

            cTable$addRow(rowKey = "Test Positive",
                          values = list(
                              newtest = "Test Positive"
                          )
            )


            cTable$addRow(rowKey = "Test Negative",
                          values = list(
                              newtest = "Test Negative"
                          )
            )




            cTable$addRow(rowKey = "Total",
                          values = list(
                              newtest = "Total"
                          )
            )

        },






        .run = function() {


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


        # read numbers from input ----

        TP <- self$options$TP

        FP <- self$options$FP

        TN <- self$options$TN

        FN <- self$options$FN


        # make table ----

        # table1 <- matrix(c(TP, FP, FN, TN), nrow = 2, ncol = 2, byrow = TRUE,
        # dimnames = list(c('Test Positive', 'Test Negative'), c('Gold
        # Positive','Gold Negative'))) self$results$text1$setContent(table1)

        table2 <- matrix(c(TP, FP, FN, TN), nrow = 2, ncol = 2, byrow = TRUE,
            dimnames = list(c("Positive", "Negative"), c("Positive", "Negative")))

        table3 <- as.table(table2)

        names(attributes(table3)$dimnames) <- c("Test", "Golden Standard")

        # Prior Probability ----

        pp <- self$options$pp

        pprob <- self$options$pprob


        # caret result ----

        # if (pp) {
        #     caretresult <- caret::confusionMatrix(table3, prevalence = pprob)
        #
        # } else {
        #
        #     caretresult <- caret::confusionMatrix(table3)
        #
        # }

        # self$results$text2$setContent(caretresult)



        # Cross Table in jamovi style ----

        cTable <- self$results$cTable


        cTable$setRow(rowKey = "Test Positive",
                      values = list(
                          newtest = "Test Positive",
                          GP = TP,
                          GN = FP,
                          Total = TP + FP
                      )
        )


        cTable$setRow(rowKey = "Test Negative",
                      values = list(
                          newtest = "Test Negative",
                          GP = FN,
                          GN = TN,
                          Total = FN + TN
                      )
        )

        cTable$setRow(rowKey = "Total",
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

        epirresult <- epiR::epi.tests(dat = table3)
        # self$results$text3$setContent(epirresult)



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



        # text4 <-
        #     list(
        #
        #         "summary" = epirresult2
        #
        # # epirresult[[3]]$aprev,
        # # epirresult[[3]]$tprev,
        # # epirresult[[3]]$se,
        # # epirresult[[3]]$sp,
        # # epirresult[[3]]$diag.acc,
        # # epirresult[[3]]$diag.or,
        # # epirresult[[3]]$nnd,
        # # epirresult[[3]]$youden,
        # # epirresult[[3]]$ppv,
        # # epirresult[[3]]$npv,
        # # epirresult[[3]]$plr,
        # # epirresult[[3]]$nlr,
        # # epirresult[[3]]$pro,
        # # epirresult[[3]]$pri,
        # # epirresult[[3]]$pfp,
        # # epirresult[[3]]$pfn
        #     )







        # self$results$text4$setContent(text4)



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




        # Send Data to Plot ----


        plotData1 <- list(
            "Prevalence" = PriorProb,
            "Sens" = Sens,
            "Spec" = Spec,
            "Plr" = LRP,
            "Nlr" = LRN
        )

        image1 <- self$results$plot1
        image1$setState(plotData1)

        # plotData2 <- plotData1
        #
        # image2 <- self$results$plot2
        # image2$setState(plotData2)


            }


        ,

        .plot1 = function(image1, ggtheme, ...) {


            plotData1 <- image1$state

        plot1 <- nomogrammer(Prevalence = plotData1$Prevalence,
                            Sens = plotData1$Sens,
                            Spec = plotData1$Spec,
                            Plr = plotData1$Plr,
                            Nlr = plotData1$Nlr,
                            Detail = TRUE,
                            NullLine = TRUE,
                            LabelSize = (14/5),
                            Verbose = TRUE
                            )

        print(plot1)
        TRUE


        }


        # ,
        # .plot2 = function(image2, ggtheme, ...) {
        #
        #
        #     plotData2 <- image2$state
        #
        #     plot2 <- nomogrammer(Prevalence = plotData2$Prevalence,
        #                          Plr = plotData2$Plr,
        #                          Nlr = plotData2$Nlr,
        #                          Detail = TRUE,
        #                          NullLine = TRUE,
        #                          LabelSize = (14/5),
        #                          Verbose = TRUE
        #     )
        #
        #     print(plot2)
        #     TRUE
        #
        #
        # }
        #



        ))
