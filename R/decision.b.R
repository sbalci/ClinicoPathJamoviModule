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



        # Caret ----

        conf_table <- table(mydata2[["testVariable2"]], mydata2[["goldVariable2"]])


        results_caret <- caret::confusionMatrix(conf_table, positive = "Positive")


        self$results$text2$setContent(results_caret)


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


        # Individual analysis ----

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



        # Prior Probability ----

        # lvs <- c('normal', 'abnormal') truth <- factor(rep(lvs, times = c(86,
        # 258)), levels = rev(lvs)) pred <- factor( c( rep(lvs, times = c(54,
        # 32)), rep(lvs, times = c(27, 231))), levels = rev(lvs)) xtab <-
        # table(pred, truth) confusionMatrix(xtab) confusionMatrix(pred, truth)
        # confusionMatrix(xtab, prevalence = 0.25) ## 3 class example
        # confusionMatrix(iris$Species, sample(iris$Species)) newPrior <- c(.05,
        # .8, .15) names(newPrior) <- levels(iris$Species)
        # confusionMatrix(iris$Species, sample(iris$Species))




    }))
