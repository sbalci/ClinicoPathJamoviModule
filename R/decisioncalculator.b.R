#' @title Decision Calculator
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
            
            # Read numbers from input ----
            TP <- self$options$TP
            FP <- self$options$FP
            TN <- self$options$TN
            FN <- self$options$FN
            
            # Input validation ----
            # Check for non-negative values
            if (TP < 0 || FP < 0 || TN < 0 || FN < 0) {
                stop("All counts must be non-negative. Please check your input values.")
            }
            
            # Check for at least some data
            if (TP + FP + TN + FN == 0) {
                stop("All counts are zero. Please provide valid diagnostic test data.")
            }
            
            # Check for diseased subjects
            if (TP + FN == 0) {
                stop("No diseased subjects (TP + FN = 0). Cannot calculate sensitivity and related metrics.")
            }
            
            # Check for healthy subjects
            if (TN + FP == 0) {
                stop("No healthy subjects (TN + FP = 0). Cannot calculate specificity and related metrics.")
            }
            
            # Check for positive tests
            if (TP + FP == 0) {
                warning("No positive test results (TP + FP = 0). PPV will be undefined.")
            }
            
            # Check for negative tests
            if (TN + FN == 0) {
                warning("No negative test results (TN + FN = 0). NPV will be undefined.")
            }


            # Create confusion matrix ----

        table2 <- matrix(c(TP, FP, FN, TN), nrow = 2, ncol = 2, byrow = TRUE,
            dimnames = list(c("Positive", "Negative"), c("Positive", "Negative")))

        table3 <- as.table(table2)

        names(attributes(table3)$dimnames) <- c("Test", "Golden Standard")

        # Prior Probability ----

        pp <- self$options$pp

        pprob <- self$options$pprob





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

        # Calculate metrics with safe division
        Sens <- if (DiseaseP > 0) TP/DiseaseP else 0
        Spec <- if (DiseaseN > 0) TN/DiseaseN else 0
        AccurT <- if (TotalPop > 0) TestT/TotalPop else 0
        PrevalenceD <- if (TotalPop > 0) DiseaseP/TotalPop else 0
        PPV <- if (TestP > 0) TP/TestP else NA
        NPV <- if (TestN > 0) TN/TestN else NA


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




        # Calculate likelihood ratios with safe division
        LRP <- if ((1 - Spec) > 0 && !is.na(Sens)) Sens / (1 - Spec) else Inf
        LRN <- if (Spec > 0 && !is.na(Sens)) (1 - Sens) / Spec else 0

        # Advanced confidence interval calculations ----
        # Following DiagROC's comprehensive approach
        
        # Clopper-Pearson exact binomial CIs for sensitivity and specificity
        sens_ci <- stats::binom.test(TP, TP + FN, conf.level = 0.95)$conf.int
        spec_ci <- stats::binom.test(TN, TN + FP, conf.level = 0.95)$conf.int
        
        # Logit transformation CIs for PPV and NPV (more accurate)
        # PPV CI using logit transformation
        if (TP > 0 && TestP > 0) {
            ppv_logit <- log(PPV / (1 - PPV))
            ppv_se <- sqrt((1/TP) + (1/FP))
            ppv_ci_logit <- ppv_logit + c(-1.96, 1.96) * ppv_se
            ppv_ci <- exp(ppv_ci_logit) / (1 + exp(ppv_ci_logit))
        } else {
            ppv_ci <- c(0, 1)
        }
        
        # NPV CI using logit transformation
        if (TN > 0 && TestN > 0) {
            npv_logit <- log(NPV / (1 - NPV))
            npv_se <- sqrt((1/TN) + (1/FN))
            npv_ci_logit <- npv_logit + c(-1.96, 1.96) * npv_se
            npv_ci <- exp(npv_ci_logit) / (1 + exp(npv_ci_logit))
        } else {
            npv_ci <- c(0, 1)
        }
        
        # Log-transformed CIs for likelihood ratios
        # PLR CI
        if (TP > 0 && FP > 0) {
            plr_log <- log(LRP)
            plr_se <- sqrt((1/TP) - (1/(TP + FN)) + (1/FP) - (1/(TN + FP)))
            plr_ci_log <- plr_log + c(-1.96, 1.96) * plr_se
            plr_ci <- exp(plr_ci_log)
        } else {
            plr_ci <- c(0, Inf)
        }
        
        # NLR CI
        if (FN > 0 && TN > 0) {
            nlr_log <- log(LRN)
            nlr_se <- sqrt((1/FN) - (1/(TP + FN)) + (1/TN) - (1/(TN + FP)))
            nlr_ci_log <- nlr_log + c(-1.96, 1.96) * nlr_se
            nlr_ci <- exp(nlr_ci_log)
        } else {
            nlr_ci <- c(0, 1)
        }
        
        # Additional diagnostic metrics from DiagROC
        # Diagnostic Odds Ratio
        if (FN > 0 && FP > 0) {
            DOR <- (TP * TN) / (FN * FP)
            # CI for DOR using log transformation
            dor_log <- log(DOR)
            dor_se <- sqrt((1/TP) + (1/TN) + (1/FN) + (1/FP))
            dor_ci_log <- dor_log + c(-1.96, 1.96) * dor_se
            dor_ci <- exp(dor_ci_log)
        } else {
            DOR <- Inf
            dor_ci <- c(0, Inf)
        }
        
        # Youden's Index (optimal cut-off criterion)
        YoudenIndex <- Sens + Spec - 1
        
        # Balanced Accuracy (useful when dealing with imbalanced data)
        BalancedAccuracy <- (Sens + Spec) / 2
        
        # F1 Score (harmonic mean of sensitivity and PPV)
        if (Sens > 0 && PPV > 0) {
            F1Score <- 2 * (Sens * PPV) / (Sens + PPV)
        } else {
            F1Score <- 0
        }
        
        # Matthews Correlation Coefficient (MCC)
        mcc_numerator <- (TP * TN) - (FP * FN)
        mcc_denominator <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
        if (mcc_denominator > 0) {
            MCC <- mcc_numerator / mcc_denominator
        } else {
            MCC <- 0
        }


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

        ratioTable$addFootnote(rowNo = 1, col = "LRP", "Positive Likelihood Ratio: How much more likely a positive result is in diseased vs healthy patients. >10 = strong evidence, >5 = moderate, >2 = weak but potentially useful.")
        
        ratioTable$addFootnote(rowNo = 1, col = "LRN", "Negative Likelihood Ratio: How much more likely a negative result is in diseased vs healthy patients. <0.1 = strong evidence against disease, <0.2 = moderate, <0.5 = weak.")


        }




        # Populate advanced metrics table ----
        advancedMetricsTable <- self$results$advancedMetricsTable
        advancedMetricsTable$setRow(
            rowNo = 1,
            values = list(
                tablename = "Advanced Metrics",
                youdenIndex = YoudenIndex,
                balancedAccuracy = BalancedAccuracy,
                f1Score = F1Score,
                mcc = MCC,
                dor = DOR
            )
        )
        
        # Add footnotes for advanced metrics
        if (self$options$fnote) {
            advancedMetricsTable$addFootnote(rowNo = 1, col = "youdenIndex", 
                "Youden's Index: Discriminatory ability independent of prevalence. >0.5 excellent, 0.3-0.5 good, 0.1-0.3 fair, <0.1 poor.")
            
            advancedMetricsTable$addFootnote(rowNo = 1, col = "balancedAccuracy", 
                "Balanced Accuracy: Average of sensitivity and specificity. Useful for imbalanced datasets.")
            
            advancedMetricsTable$addFootnote(rowNo = 1, col = "f1Score", 
                "F1 Score: Harmonic mean of sensitivity and PPV. Ranges 0-1, higher is better.")
            
            advancedMetricsTable$addFootnote(rowNo = 1, col = "mcc", 
                "Matthews Correlation Coefficient: Overall test quality measure. Ranges -1 to +1, >0.5 is good.")
            
            advancedMetricsTable$addFootnote(rowNo = 1, col = "dor", 
                "Diagnostic Odds Ratio: Overall discriminatory performance. >25 strong, 5-25 moderate, 2-5 weak.")
        }






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
                "Proportion of false negative",
                "False Discovery Rate",
                "False Omission Rate"

            )

        ratiorows <- c(
            "ap",
            "tp",
            "se",
            "sp",
            "diag.ac",
            "pv.pos",
            "pv.neg",
            "p.tpdn",
            "p.tndp",
            "p.dntp",
            "p.dptn"
        )


        numberrows <- c(
            "diag.or",
            "nndx",
            "youden",
            "lr.pos",
            "lr.neg"
        )

        epirresult_number <- epirresult2[epirresult2$statistic %in% numberrows, ]

        epirresult_ratio <- epirresult2[epirresult2$statistic %in% ratiorows, ]
        
        # Enhanced metrics from DiagROC - add to existing results
        # Add Balanced Accuracy
        balanced_acc_row <- data.frame(
            statistic = "bal.acc",
            est = BalancedAccuracy,
            lower = BalancedAccuracy - 1.96*sqrt((Sens*(1-Sens)/(TP+FN) + Spec*(1-Spec)/(TN+FP))/4),
            upper = BalancedAccuracy + 1.96*sqrt((Sens*(1-Sens)/(TP+FN) + Spec*(1-Spec)/(TN+FP))/4),
            statsabv = "bal.acc",
            statsnames = "Balanced accuracy",
            stringsAsFactors = FALSE
        )
        
        # Add F1 Score
        f1_row <- data.frame(
            statistic = "f1.score",
            est = F1Score,
            lower = 0,  # F1 score CI is complex, using simple bounds
            upper = 1,
            statsabv = "f1.score", 
            statsnames = "F1 score",
            stringsAsFactors = FALSE
        )
        
        # Add Matthews Correlation Coefficient
        mcc_row <- data.frame(
            statistic = "mcc",
            est = MCC,
            lower = -1,  # MCC CI is complex, using theoretical bounds
            upper = 1,
            statsabv = "mcc",
            statsnames = "Matthews correlation coefficient", 
            stringsAsFactors = FALSE
        )
        
        # Combine enhanced metrics with existing epiR results
        epirresult_ratio <- rbind(epirresult_ratio, balanced_acc_row, f1_row, mcc_row)



         # text4 <-
         #     list(

                 # "summary" = epirresult2

         # epirresult[[3]]$aprev,
         # epirresult[[3]]$tprev,
         # epirresult[[3]]$se,
         # epirresult[[3]]$sp,
         # epirresult[[3]]$diag.acc,
         # epirresult[[3]]$diag.or,
         # epirresult[[3]]$nnd,
         # epirresult[[3]]$youden,
         # epirresult[[3]]$ppv,
         # epirresult[[3]]$npv,
         # epirresult[[3]]$plr,
         # epirresult[[3]]$nlr,
         # epirresult[[3]]$pro,
         # epirresult[[3]]$pri,
         # epirresult[[3]]$pfp,
         # epirresult[[3]]$pfn
             # )







        # Enhanced functionality: Multiple CI methods applied
        # - Clopper-Pearson exact for sensitivity/specificity  
        # - Logit transformation for PPV/NPV
        # - Log transformation for likelihood ratios
        # - Additional metrics: Balanced Accuracy, F1 Score, MCC
        
        # self$results$text4$setContent(text4)



        # epirTable_ratio -----

        epirTable_ratio <- self$results$epirTable_ratio

        data_frame <- epirresult_ratio
        for(i in seq_along(data_frame[,1,drop=T])) {
            epirTable_ratio$addRow(rowKey = i, values = c(data_frame[i,])) # This code produces a named vector/list, which is what the values argument expects
        }



        # epirTable_ratio footnotes ----

        # if (self$options$fnote) {
        #
        #     epirTable_ratio$addFootnote(
        #         rowNo = 5,
        #         col = "statsnames",
        #         "Proportion of all tests that give a correct result."
        #         )
        # }






        # epirTable_number ----


        epirTable_number <- self$results$epirTable_number

        data_frame <- epirresult_number
        for(i in seq_along(data_frame[,1,drop=T])) {
            epirTable_number$addRow(rowKey = i, values = c(data_frame[i,]))
        }



        # epirTable_number footnotes ----

        # if (self$options$fnote) {
        #
        #
        #     epirTable_number$addFootnote(
        #         rowNo = 1,
        #         col = "statsnames",
        #         "How much more likely will the test make a correct diagnosis than an incorrect diagnosis in patients with the disease."
        #         )
        #
        #     epirTable_number$addFootnote(
        #         rowNo = 2,
        #         col = "statsnames",
        #         "Number of patients that need to be tested to give one correct positive test."
        #     )
        #
        #
        #     epirTable_number$addFootnote(
        #         rowNo = 3,
        #         col = "statsnames",
        #         "Youden's index is the difference between the true positive rate and the false positive rate. Youden's index ranges from -1 to +1 with values closer to 1 if both sensitivity and specificity are high (i.e. close to 1)."
        #
        #     )
        #
        #
        # }

















                        }
        
        # Multiple cut-off evaluation (DiagROC inspired)
        if (self$options$multiplecuts) {
            multipleCutoffTable <- self$results$multipleCutoffTable
            
            # Helper function to calculate metrics for a cut-off
            calculate_cutoff_metrics <- function(tp, fp, tn, fn, cutoff_name) {
                total <- tp + fp + tn + fn
                diseased <- tp + fn
                healthy <- tn + fp
                
                sens <- tp / diseased
                spec <- tn / healthy
                ppv <- tp / (tp + fp)
                npv <- tn / (tn + fn)
                accuracy <- (tp + tn) / total
                youden <- sens + spec - 1
                
                # Clinical recommendation based on Youden index and balanced metrics
                if (youden > 0.8 && accuracy > 0.9) {
                    recommendation <- "Excellent performance - Recommended"
                } else if (youden > 0.6 && accuracy > 0.8) {
                    recommendation <- "Good performance - Consider for use"
                } else if (youden > 0.4) {
                    recommendation <- "Fair performance - Use with caution"
                } else {
                    recommendation <- "Poor performance - Not recommended"
                }
                
                return(list(
                    cutoffName = cutoff_name,
                    sensitivity = sens,
                    specificity = spec,
                    ppv = ppv,
                    npv = npv,
                    accuracy = accuracy,
                    youden = youden,
                    recommendation = recommendation
                ))
            }
            
            # Calculate metrics for both cut-offs
            cutoff1_metrics <- calculate_cutoff_metrics(
                self$options$tp1, self$options$fp1, 
                self$options$tn1, self$options$fn1,
                self$options$cutoff1
            )
            
            cutoff2_metrics <- calculate_cutoff_metrics(
                self$options$tp2, self$options$fp2,
                self$options$tn2, self$options$fn2, 
                self$options$cutoff2
            )
            
            # Add rows to comparison table
            multipleCutoffTable$addRow(
                rowKey = 1,
                values = cutoff1_metrics
            )
            
            multipleCutoffTable$addRow(
                rowKey = 2, 
                values = cutoff2_metrics
            )
            
            # Add optimal cut-off recommendation based on current data
            current_youden <- YoudenIndex
            current_accuracy <- AccurT
            
            if (cutoff1_metrics$youden > current_youden && cutoff1_metrics$accuracy > current_accuracy) {
                optimal_msg <- paste0(cutoff1_metrics$cutoffName, " cut-off performs better than current")
            } else if (cutoff2_metrics$youden > current_youden && cutoff2_metrics$accuracy > current_accuracy) {
                optimal_msg <- paste0(cutoff2_metrics$cutoffName, " cut-off performs better than current")
            } else {
                optimal_msg <- "Current cut-off appears optimal"
            }
            
            multipleCutoffTable$addRow(
                rowKey = 3,
                values = list(
                    cutoffName = "Current (Reference)",
                    sensitivity = Sens,
                    specificity = Spec,
                    ppv = PPV,
                    npv = NPV,
                    accuracy = AccurT,
                    youden = YoudenIndex,
                    recommendation = optimal_msg
                )
            )
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
