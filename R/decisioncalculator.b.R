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
    inherit = decisioncalculatorBase, private = list(.run = function() {


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

        TP <- self$options$TP

        FP <- self$options$FP

        TN <- self$options$TN

        FN <- self$options$FN

        # table1 <- matrix(c(TP, FP, FN, TN), nrow = 2, ncol = 2, byrow = TRUE,
        # dimnames = list(c('Test Positive', 'Test Negative'), c('Gold
        # Positive','Gold Negative'))) self$results$text1$setContent(table1)

        table2 <- matrix(c(TP, FP, FN, TN), nrow = 2, ncol = 2, byrow = TRUE,
            dimnames = list(c("Positive", "Negative"), c("Positive", "Negative")))

        table3 <- as.table(table2)

        names(attributes(table3)$dimnames) <- c("Test", "Gold Standart")

        # Prior Probability ----

        pp <- self$options$pp

        pprob <- self$options$pprob

        if (pp) {
            caretresult <- caret::confusionMatrix(table3, prevalence = pprob)

        } else {

            caretresult <- caret::confusionMatrix(table3)

        }

        self$results$text2$setContent(caretresult)


        # Self Calculation https://cran.r-project.org/web/packages/caret/caret.pdf
        # https://online.stat.psu.edu/stat509/node/150/


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




        LRP <- Sens/(1 - Spec)

        LRN <- (1 - Sens)/Spec






        # Populate Table

        # manualtable <- self$results$manualtable
        # manualtable$setRow(rowNo = 1, values = list(tablename = "Decision Test Statistics",
        #     TotalPop = TotalPop, DiseaseP = DiseaseP, DiseaseN = DiseaseN,
        #     TestP = TestP, TestN = TestN, TestT = TestT, TestW = TestW, Sens = Sens,
        #     Spec = Spec, AccurT = AccurT, PrevalenceD = PrevalenceD, PPV = PPV,
        #     NPV = NPV, PostTestProbDisease = PostTestProbDisease, PostTestProbHealthy = PostTestProbHealthy))








        # Reorganize Table



        # caretresult[['positive']] caretresult[['table']]
        # caretresult[['overall']] caretresult[['overall']][['Accuracy']]
        # caretresult[['overall']][['Kappa']]
        # caretresult[['overall']][['AccuracyLower']]
        # caretresult[['overall']][['AccuracyUpper']]
        # caretresult[['overall']][['AccuracyNull']]
        # caretresult[['overall']][['AccuracyPValue']]
        # caretresult[['overall']][['McnemarPValue']] caretresult[['byClass']]
        # caretresult[['byClass']][['Sensitivity']]
        # caretresult[['byClass']][['Specificity']] caretresult[['byClass']][['Pos
        # Pred Value']] caretresult[['byClass']][['Neg Pred Value']]
        # caretresult[['byClass']][['Precision']]
        # caretresult[['byClass']][['Recall']] caretresult[['byClass']][['F1']]
        # caretresult[['byClass']][['Prevalence']]
        # caretresult[['byClass']][['Detection Rate']]
        # caretresult[['byClass']][['Detection Prevalence']]
        # caretresult[['byClass']][['Balanced Accuracy']] caretresult[['mode']]
        # caretresult[['dots']]




        # Write Summary




        # use epiR



    }))
