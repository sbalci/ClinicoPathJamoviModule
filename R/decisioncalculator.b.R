#' @title Decision Calculator
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom utils data
#'

decisioncalculatorClass <- if (requireNamespace("jmvcore")) R6::R6Class("decisioncalculatorClass",
    inherit = decisioncalculatorBase, private = list(

        # Clinical interpretation thresholds (evidence-based cutoffs)
        # Youden's Index thresholds based on diagnostic test literature
        .YOUDEN_EXCELLENT = 0.8,  # J > 0.8: Excellent discriminatory ability
        .YOUDEN_GOOD = 0.6,       # J > 0.6: Good discriminatory ability
        .YOUDEN_FAIR = 0.4,       # J > 0.4: Fair discriminatory ability
        .ACCURACY_EXCELLENT = 0.9, # Accuracy > 0.9: Excellent overall performance
        .ACCURACY_GOOD = 0.8,      # Accuracy > 0.8: Good overall performance

        .init = function() {

            # Welcome message
            welcome_html <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>
                <div style='background: #f5f5f5; border: 2px solid #333; padding: 20px; margin-bottom: 20px;'>
                <h2 style='margin: 0 0 10px 0; font-size: 18px; color: #333;'>Medical Decision Calculator</h2>
                <p style='margin: 0; font-size: 14px; color: #666;'>
                Comprehensive diagnostic test evaluation for clinical decision-making
                </p>
                </div>

                <div style='font-size: 14px; color: #333;'>
                <p><strong>What this tool does:</strong></p>
                <p>Evaluates diagnostic test performance by calculating sensitivity, specificity,
                predictive values, likelihood ratios, and advanced metrics from a 2×2 confusion matrix.</p>

                <p><strong>To get started:</strong></p>
                <ol style='margin: 10px 0; padding-left: 25px;'>
                    <li>Enter your four counts: TP (True Positives), FP (False Positives), TN (True Negatives), FN (False Negatives)</li>
                    <li>Choose whether to calculate confidence intervals (recommended)</li>
                    <li>Optionally enable summary, glossary, or about panels for additional guidance</li>
                </ol>

                <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 12px; margin: 15px 0;'>
                    <p style='margin: 0; font-size: 13px;'><strong>Quick Example:</strong>
                    If you tested 200 patients (100 diseased, 100 healthy) and your test correctly
                    identified 90 diseased (TP=90) and 80 healthy (TN=80), you have FN=10 and FP=20.</p>
                </div>
                </div>
            </div>
            "

            self$results$welcome$setContent(welcome_html)

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

            # Read analysis options
            pp <- self$options$pp
            pprob <- self$options$pprob
            ci <- self$options$ci

            # Input validation ----
            # Enforce mutual exclusion of CI and custom prevalence (epiR limitation)
            if (ci && pp) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'ciAndPpConflict',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Confidence intervals are unavailable when using population prevalence. • epiR::epi.tests() computes CIs from the study sample, not an externally supplied prevalence. • Disable either "95% Confidence Intervals" or "Use Known Population Prevalence" to proceed. • Point estimates will still be Bayes-adjusted when prevalence is supplied.')
                self$results$insert(999, notice)
                return()
            }

            # Inform user that CIs are unavailable when using population prevalence
            if (!ci && pp) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'ciUnavailableWithPP',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent('Confidence intervals are not calculated when using population prevalence. • epiR::epi.tests() provides CIs only for sample-based prevalence. • Point estimates shown here are Bayes-adjusted to the supplied prevalence without CIs.')
                self$results$insert(999, notice)
            }

            # Validate prevalence when provided programmatically
            if (pp && (is.na(pprob) || pprob <= 0 || pprob >= 1)) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'invalidPriorProb',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Invalid prior probability. • Prior probability must be between 0 and 1 (exclusive). • Update the "Prior Probability (prevalence)" value.')
                self$results$insert(999, notice)
                return()
            }

            # Reject non-finite inputs
            if (any(!is.finite(c(TP, FP, TN, FN)))) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'nonFiniteCounts',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Non-finite counts detected. • TP, FP, TN, and FN must be finite numbers. • Please check your input values.')
                self$results$insert(999, notice)
                return()
            }

            # Check for non-negative values
            if (TP < 0 || FP < 0 || TN < 0 || FN < 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'negativeCountsDetected',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Negative counts detected. • All counts (TP, FP, TN, FN) must be non-negative. • Please check your input values for errors.')
                self$results$insert(999, notice)
                return()
            }
            
            # Check for at least some data
            if (TP + FP + TN + FN == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'allCountsZero',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('All counts are zero. • Please provide valid diagnostic test data. • Ensure TP, FP, TN, and FN values are entered correctly.')
                self$results$insert(999, notice)
                return()
            }
            
            # Check for diseased subjects
            if (TP + FN == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'noDiseasedSubjects',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('No diseased subjects detected (TP + FN = 0). • Cannot calculate sensitivity and related metrics. • Ensure your confusion matrix includes cases with disease present.')
                self$results$insert(999, notice)
                return()
            }

            # Check for healthy subjects
            if (TN + FP == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'noHealthySubjects',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('No healthy subjects detected (TN + FP = 0). • Cannot calculate specificity and related metrics. • Ensure your confusion matrix includes cases without disease.')
                self$results$insert(999, notice)
                return()
            }
            
            # Check for positive tests
            if (TP + FP == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'noPositiveTests',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent('No positive test results detected (TP + FP = 0). • Positive Predictive Value (PPV) is undefined. • Ensure your confusion matrix includes both positive and negative test results.')
                self$results$insert(999, notice)
            }
            
            # Check for negative tests
            if (TN + FN == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'noNegativeTests',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent('No negative test results detected (TN + FN = 0). • Negative Predictive Value (NPV) is undefined. • Ensure your confusion matrix includes both positive and negative test results.')
                self$results$insert(999, notice)
            }


            # Create confusion matrix ----

        table2 <- matrix(c(TP, FP, FN, TN), nrow = 2, ncol = 2, byrow = TRUE,
            dimnames = list(c("Positive", "Negative"), c("Positive", "Negative")))

        table3 <- as.table(table2)

        names(attributes(table3)$dimnames) <- c("Test", "Golden Standard")

        # Prior Probability ----
        # (pp and pprob already read at top of function for validation)





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

        # Warn on non-integer counts (allowed but unusual)
        if (any(abs(c(TP, FP, TN, FN) - round(c(TP, FP, TN, FN))) > 1e-6)) {
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = 'nonIntegerCounts',
                type = jmvcore::NoticeType$WARNING
            )
            notice$setContent('Non-integer counts detected. • Diagnostic test counts are typically whole numbers. • Proceeding with calculations, but verify your inputs.')
            self$results$insert(999, notice)
        }

        # Continuity correction for zero-cell issues (stabilizes LR/DOR and CIs)
        zero_cell <- any(c(TP, FP, TN, FN) == 0)
        TP_cc <- TP
        FP_cc <- FP
        TN_cc <- TN
        FN_cc <- FN

        if (zero_cell) {
            TP_cc <- TP + 0.5
            FP_cc <- FP + 0.5
            TN_cc <- TN + 0.5
            FN_cc <- FN + 0.5

            notice <- jmvcore::Notice$new(
                options = self$options,
                name = 'continuityCorrection',
                type = jmvcore::NoticeType$WARNING
            )
            notice$setContent('Zero cells detected. Applied Haldane-Anscombe 0.5 continuity correction for likelihood ratios, diagnostic odds ratio, and confidence intervals to avoid infinite or undefined estimates.')
            self$results$insert(999, notice)
        }

        # Calculate metrics with safe division
        Sens <- if (DiseaseP > 0) TP/DiseaseP else 0
        Spec <- if (DiseaseN > 0) TN/DiseaseN else 0
        AccurT <- if (TotalPop > 0) TestT/TotalPop else 0
        PrevalenceD <- if (TotalPop > 0) DiseaseP/TotalPop else 0

        # Determine which prevalence to use
        if (pp) {
            # Known prior probability from population
            PriorProb <- pprob
        } else {
            # From ConfusionMatrix
            PriorProb <- PrevalenceD
        }

        # CRITICAL FIX: Calculate PPV and NPV using the selected prevalence
        # When pp=TRUE, PPV/NPV must use population prevalence via Bayes' theorem,
        # NOT the sample-based TP/(TP+FP) calculation
        if (pp) {
            # Population-adjusted PPV/NPV using Bayes' theorem
            PPV <- if (!is.na(Sens) && !is.na(Spec)) {
                numerator <- Sens * PriorProb
                denominator <- (Sens * PriorProb) + ((1 - Spec) * (1 - PriorProb))
                if (denominator > 0) numerator / denominator else NA
            } else NA

            NPV <- if (!is.na(Sens) && !is.na(Spec)) {
                numerator <- Spec * (1 - PriorProb)
                denominator <- (Spec * (1 - PriorProb)) + ((1 - Sens) * PriorProb)
                if (denominator > 0) numerator / denominator else NA
            } else NA
        } else {
            # Sample-based PPV/NPV
            PPV <- if (TestP > 0) TP/TestP else NA
            NPV <- if (TestN > 0) TN/TestN else NA
        }

        # Post-test probabilities (same as PPV/NPV when using population prevalence)
        PostTestProbDisease <- if (!is.na(Sens) && !is.na(Spec)) {
            numerator <- PriorProb * Sens
            denominator <- (PriorProb * Sens) + ((1 - PriorProb) * (1 - Spec))
            if (denominator > 0) numerator / denominator else NA
        } else NA

        PostTestProbHealthy <- if (!is.na(Sens) && !is.na(Spec)) {
            numerator <- (1 - PriorProb) * Spec
            denominator <- ((1 - PriorProb) * Spec) + (PriorProb * (1 - Sens))
            if (denominator > 0) numerator / denominator else NA
        } else NA




        # Calculate likelihood ratios with safe division (corrected if needed)
        LRP <- if ((1 - (TN_cc / (TN_cc + FP_cc))) > 0) {
            (TP_cc / (TP_cc + FN_cc)) / (1 - (TN_cc / (TN_cc + FP_cc)))
        } else Inf

        LRN <- if ((TN_cc / (TN_cc + FP_cc)) > 0) {
            (1 - (TP_cc / (TP_cc + FN_cc))) / (TN_cc / (TN_cc + FP_cc))
        } else 0

        # Advanced confidence interval calculations ----
        # Multiple CI methods for different metrics (following diagnostic test literature):
        # - Clopper-Pearson exact: Gold standard for sensitivity/specificity (binomial proportions)
        # - Logit transformation: Better coverage for PPV/NPV at extreme values
        # - Log transformation: Standard approach for likelihood ratios (multiplicative scale)
        # These supplement epiR::epi.tests() for verification and methodological transparency

        # Clopper-Pearson exact binomial CIs for sensitivity and specificity
        sens_ci <- stats::binom.test(TP_cc, TP_cc + FN_cc, conf.level = 0.95)$conf.int
        spec_ci <- stats::binom.test(TN_cc, TN_cc + FP_cc, conf.level = 0.95)$conf.int
        
        # Logit transformation CIs for PPV and NPV (more accurate)
        # PPV CI using logit transformation
        eps <- 1e-9
        PPV_for_ci <- min(max(PPV, eps), 1 - eps)
        NPV_for_ci <- min(max(NPV, eps), 1 - eps)

        if (TP_cc > 0 && TestP > 0) {
            ppv_logit <- log(PPV_for_ci / (1 - PPV_for_ci))
            ppv_se <- sqrt((1/TP_cc) + (1/FP_cc))
            ppv_ci_logit <- ppv_logit + c(-1.96, 1.96) * ppv_se
            ppv_ci <- exp(ppv_ci_logit) / (1 + exp(ppv_ci_logit))
        } else {
            ppv_ci <- c(0, 1)
        }
        
        # NPV CI using logit transformation
        if (TN_cc > 0 && TestN > 0) {
            npv_logit <- log(NPV_for_ci / (1 - NPV_for_ci))
            npv_se <- sqrt((1/TN_cc) + (1/FN_cc))
            npv_ci_logit <- npv_logit + c(-1.96, 1.96) * npv_se
            npv_ci <- exp(npv_ci_logit) / (1 + exp(npv_ci_logit))
        } else {
            npv_ci <- c(0, 1)
        }
        
        # Log-transformed CIs for likelihood ratios
        # PLR CI
        if (TP_cc > 0 && FP_cc > 0) {
            plr_log <- log(LRP)
            plr_se <- sqrt((1/TP_cc) - (1/(TP_cc + FN_cc)) + (1/FP_cc) - (1/(TN_cc + FP_cc)))
            plr_ci_log <- plr_log + c(-1.96, 1.96) * plr_se
            plr_ci <- exp(plr_ci_log)
        } else {
            plr_ci <- c(0, Inf)
        }
        
        # NLR CI
        if (FN_cc > 0 && TN_cc > 0) {
            nlr_log <- log(LRN)
            nlr_se <- sqrt((1/FN_cc) - (1/(TP_cc + FN_cc)) + (1/TN_cc) - (1/(TN_cc + FP_cc)))
            nlr_ci_log <- nlr_log + c(-1.96, 1.96) * nlr_se
            nlr_ci <- exp(nlr_ci_log)
        } else {
            nlr_ci <- c(0, 1)
        }
        
        # Additional diagnostic metrics from DiagROC
        # Diagnostic Odds Ratio
        if (FN_cc > 0 && FP_cc > 0) {
            DOR <- (TP_cc * TN_cc) / (FN_cc * FP_cc)
            # CI for DOR using log transformation
            dor_log <- log(DOR)
            dor_se <- sqrt((1/TP_cc) + (1/TN_cc) + (1/FN_cc) + (1/FP_cc))
            dor_ci_log <- dor_log + c(-1.96, 1.96) * dor_se
            dor_ci <- exp(dor_ci_log)
        } else {
            DOR <- NA
            dor_ci <- c(NA, NA)
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

        ppv_note <- if (pp) {
            "Positive Predictive Value (Probability of disease after a positive test using supplied population prevalence)"
        } else {
            "Positive Predictive Value (Probability of having disease after a positive test using this study population)"
        }
        npv_note <- if (pp) {
            "Negative Predictive Value (Probability of being healthy after a negative test using supplied population prevalence)"
        } else {
            "Negative Predictive Value (Probability of being healthy after a negative test using this study population)"
        }

        ratioTable$addFootnote(rowNo = 1, col = "PPV", ppv_note)

        ratioTable$addFootnote(rowNo = 1, col = "NPV", npv_note)

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
                "Youden's Index: Discriminatory ability independent of prevalence. >0.8 excellent, 0.6-0.8 good, 0.4-0.6 fair, <0.4 poor.")
            
            advancedMetricsTable$addFootnote(rowNo = 1, col = "balancedAccuracy", 
                "Balanced Accuracy: Average of sensitivity and specificity. Useful for imbalanced datasets.")
            
            advancedMetricsTable$addFootnote(rowNo = 1, col = "f1Score", 
                "F1 Score: Harmonic mean of sensitivity and PPV. Ranges 0-1, higher is better.")
            
            advancedMetricsTable$addFootnote(rowNo = 1, col = "mcc", 
                "Matthews Correlation Coefficient: Overall test quality measure. Ranges -1 to +1. >0.8 excellent, 0.6-0.8 good, 0.4-0.6 fair.")
            
            advancedMetricsTable$addFootnote(rowNo = 1, col = "dor", 
                "Diagnostic Odds Ratio: Overall discriminatory performance. >25 strong, 5-25 moderate, 2-5 weak, <2 poor.")
        }






        # 95% CI ----

        ci <- self$options$ci

        if (ci) {

            # Check if epiR package is available
            if (!requireNamespace("epiR", quietly = TRUE)) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'epiRMissing',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('epiR package is required for confidence intervals. • Install with install.packages("epiR"). • Or disable "95% Confidence Intervals" option.')
                self$results$insert(999, notice)
                return()
            }

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
            lower = NA_real_,
            upper = NA_real_,
            statsabv = "bal.acc",
            statsnames = "Balanced accuracy (CI not computed)",
            stringsAsFactors = FALSE
        )
        
        # Add F1 Score
        f1_row <- data.frame(
            statistic = "f1.score",
            est = F1Score,
            lower = NA_real_,  # CI not computed
            upper = NA_real_,
            statsabv = "f1.score", 
            statsnames = "F1 score (CI not computed)",
            stringsAsFactors = FALSE
        )
        
        # Add Matthews Correlation Coefficient
        mcc_row <- data.frame(
            statistic = "mcc",
            est = MCC,
            lower = NA_real_,  # CI not computed
            upper = NA_real_,
            statsabv = "mcc",
            statsnames = "Matthews correlation coefficient (CI not computed)", 
            stringsAsFactors = FALSE
        )
        
        # Combine enhanced metrics with existing epiR results
        epirresult_ratio <- rbind(epirresult_ratio, balanced_acc_row, f1_row, mcc_row)

        # Enhanced functionality: Multiple CI methods applied
        # - Clopper-Pearson exact for sensitivity/specificity
        # - Logit transformation for PPV/NPV
        # - Log transformation for likelihood ratios
        # - Additional metrics: Balanced Accuracy, F1 Score, MCC



        # epirTable_ratio -----

        epirTable_ratio <- self$results$epirTable_ratio

        data_frame <- epirresult_ratio
        for(i in seq_along(data_frame[,1,drop=T])) {
            epirTable_ratio$addRow(rowKey = i, values = c(data_frame[i,])) # This code produces a named vector/list, which is what the values argument expects
        }




        # epirTable_number ----


        epirTable_number <- self$results$epirTable_number

        data_frame <- epirresult_number
        for(i in seq_along(data_frame[,1,drop=T])) {
            epirTable_number$addRow(rowKey = i, values = c(data_frame[i,]))
        }




















                        }
        
        # Multiple cut-off evaluation (DiagROC inspired)
        if (self$options$multiplecuts) {
            multipleCutoffTable <- self$results$multipleCutoffTable
            
            # Helper function to calculate metrics for a cut-off
            calculate_cutoff_metrics <- function(tp, fp, tn, fn, cutoff_name) {
                # Validate inputs and guard against zero/NA division
                if (any(is.na(c(tp, fp, tn, fn))) || any(c(tp, fp, tn, fn) < 0)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'invalidCutoffInputs',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(sprintf('Invalid inputs for cut-off "%s". • All values (TP=%s, FP=%s, TN=%s, FN=%s) must be non-negative numbers. • Check your input values for errors.', cutoff_name, tp, fp, tn, fn))
                    self$results$insert(999, notice)
                    return(NULL)
                }

                total <- tp + fp + tn + fn
                diseased <- tp + fn
                healthy <- tn + fp

                # Validate that we have cases to analyze
                if (total == 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'noCasesForCutoff',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(sprintf('No cases for cut-off "%s". • Total cases (TP+FP+TN+FN) = 0. • Check your confusion matrix inputs.', cutoff_name))
                    self$results$insert(999, notice)
                    return(NULL)
                }

                # Safe division with appropriate handling for zero denominators
                sens <- if (diseased > 0) tp / diseased else NA_real_
                spec <- if (healthy > 0) tn / healthy else NA_real_
                ppv <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
                npv <- if ((tn + fn) > 0) tn / (tn + fn) else NA_real_
                accuracy <- if (total > 0) (tp + tn) / total else NA_real_

                # Youden index only defined when both sens and spec are available
                youden <- if (!is.na(sens) && !is.na(spec)) sens + spec - 1 else NA_real_

                # Warn if metrics are undefined
                if (diseased == 0 || healthy == 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = sprintf('incompleteCutoff_%s', make.names(cutoff_name)),
                        type = jmvcore::NoticeType$WARNING
                    )
                    msg <- sprintf('Cut-off "%s" has incomplete data.', cutoff_name)
                    if (diseased == 0) msg <- paste0(msg, ' • No diseased cases (TP+FN=0): Sensitivity is undefined.')
                    if (healthy == 0) msg <- paste0(msg, ' • No healthy cases (TN+FP=0): Specificity is undefined.')
                    msg <- paste0(msg, ' • Consider this cut-off unreliable for clinical decisions.')
                    notice$setContent(msg)
                    self$results$insert(999, notice)
                }

                # Clinical recommendation based on Youden index and balanced metrics
                # Only make recommendations when all metrics are valid
                if (is.na(youden) || is.na(accuracy)) {
                    recommendation <- "Incomplete data - Cannot recommend"
                } else if (youden > private$.YOUDEN_EXCELLENT && accuracy > private$.ACCURACY_EXCELLENT) {
                    recommendation <- "Excellent performance - Recommended"
                } else if (youden > private$.YOUDEN_GOOD && accuracy > private$.ACCURACY_GOOD) {
                    recommendation <- "Good performance - Consider for use"
                } else if (youden > private$.YOUDEN_FAIR) {
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

            # Skip table population if validation failed
            if (is.null(cutoff1_metrics) || is.null(cutoff2_metrics)) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'cutoffValidationFailed',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Cut-off comparison cannot be performed due to invalid inputs. • Check the error messages above for specific issues. • Ensure all TP, FP, TN, FN values are non-negative numbers.')
                self$results$insert(999, notice)
                return()
            }

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

            # Safe comparison (handle NA values)
            optimal_msg <- "Current cut-off appears optimal"

            if (!is.na(cutoff1_metrics$youden) && !is.na(cutoff1_metrics$accuracy) &&
                cutoff1_metrics$youden > current_youden && cutoff1_metrics$accuracy > current_accuracy) {
                optimal_msg <- paste0(cutoff1_metrics$cutoffName, " cut-off performs better than current")
            } else if (!is.na(cutoff2_metrics$youden) && !is.na(cutoff2_metrics$accuracy) &&
                       cutoff2_metrics$youden > current_youden && cutoff2_metrics$accuracy > current_accuracy) {
                optimal_msg <- paste0(cutoff2_metrics$cutoffName, " cut-off performs better than current")
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

        # Generate Summary, About, and Glossary panels ----

        # Summary panel
        if (self$options$showSummary) {
            summary_html <- private$.createSummary(
                Sens, Spec, PPV, NPV, LRP, LRN,
                YoudenIndex, AccurT, PriorProb
            )
            self$results$summary$setContent(summary_html)
        }

        # About and Assumptions panels
        if (self$options$showAbout) {
            about_html <- private$.createAboutPanel()
            self$results$about$setContent(about_html)

            assumptions_html <- private$.createAssumptionsPanel(TP, TN, FP, FN, PriorProb)
            self$results$assumptions$setContent(assumptions_html)
        }

        # Glossary panel
        if (self$options$showGlossary) {
            glossary_html <- private$.createGlossary()
            self$results$glossary$setContent(glossary_html)
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


        },

        # Private helper methods for summaries ----

        .createSummary = function(Sens, Spec, PPV, NPV, LRP, LRN, Youden, Accuracy, Prevalence) {

            # Clinical interpretation of performance
            performance <- if (Youden > private$.YOUDEN_EXCELLENT && Accuracy > private$.ACCURACY_EXCELLENT) {
                "excellent discriminatory ability"
            } else if (Youden > private$.YOUDEN_GOOD && Accuracy > private$.ACCURACY_GOOD) {
                "good discriminatory ability"
            } else if (Youden > private$.YOUDEN_FAIR) {
                "fair discriminatory ability"
            } else {
                "limited discriminatory ability"
            }

            # LR interpretation
            lr_interp <- if (LRP > 10) {
                "strong evidence for disease when test positive"
            } else if (LRP > 5) {
                "moderate evidence for disease when test positive"
            } else {
                "weak evidence for disease when test positive"
            }

            # NLR interpretation
            nlr_interp <- if (LRN < 0.1) {
                "strong evidence against disease when test negative"
            } else if (LRN < 0.2) {
                "moderate evidence against disease when test negative"
            } else {
                "weak evidence against disease when test negative"
            }

            # Recommendation
            recommendation <- private$.getRecommendation(Youden, Accuracy, LRP, LRN)

            sprintf(
                "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>
                <div style='background: #f5f5f5; border: 2px solid #333; padding: 15px; margin-bottom: 15px;'>
                <h3 style='margin: 0 0 5px 0; font-size: 16px; color: #333;'>Diagnostic Test Performance Summary</h3>
                </div>

                <div style='font-size: 14px; color: #333;'>
                    <p style='margin: 10px 0;'><strong>Overall Assessment:</strong> This test demonstrates %s (Youden index: %.3f, Accuracy: %.1f%%).</p>

                    <table style='width: 100%%; border-collapse: collapse; margin: 15px 0;'>
                    <tr>
                        <td style='border: 1px solid #ccc; padding: 10px; background: #f9f9f9;'>
                        <strong>Sensitivity</strong><br>
                        <span style='font-size: 18px;'>%.1f%%</span><br>
                        <span style='font-size: 12px; color: #666;'>True positive rate</span>
                        </td>
                        <td style='border: 1px solid #ccc; padding: 10px; background: #f9f9f9;'>
                        <strong>Specificity</strong><br>
                        <span style='font-size: 18px;'>%.1f%%</span><br>
                        <span style='font-size: 12px; color: #666;'>True negative rate</span>
                        </td>
                    </tr>
                    <tr>
                        <td style='border: 1px solid #ccc; padding: 10px; background: #f9f9f9;'>
                        <strong>PPV</strong><br>
                        <span style='font-size: 18px;'>%.1f%%</span><br>
                        <span style='font-size: 12px; color: #666;'>At %.1f%% prevalence</span>
                        </td>
                        <td style='border: 1px solid #ccc; padding: 10px; background: #f9f9f9;'>
                        <strong>NPV</strong><br>
                        <span style='font-size: 18px;'>%.1f%%</span><br>
                        <span style='font-size: 12px; color: #666;'>At %.1f%% prevalence</span>
                        </td>
                    </tr>
                    </table>

                    <p style='margin: 10px 0;'><strong>Clinical Utility:</strong></p>
                    <ul style='margin: 10px 0; padding-left: 25px;'>
                    <li>The positive likelihood ratio of %.2f indicates %s.</li>
                    <li>The negative likelihood ratio of %.3f indicates %s.</li>
                    </ul>

                    <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 12px; margin: 15px 0;'>
                        <p style='margin: 0; font-weight: bold;'>Clinical Recommendation</p>
                        <p style='margin: 5px 0 0 0;'>%s</p>
                    </div>
                </div>
                </div>",
                performance, Youden, Accuracy * 100,
                Sens * 100, Spec * 100,
                PPV * 100, Prevalence * 100,
                NPV * 100, Prevalence * 100,
                LRP, lr_interp,
                LRN, nlr_interp,
                recommendation
            )
        },

        .createAboutPanel = function() {
            "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>
            <div style='background: #f5f5f5; border: 2px solid #333; padding: 15px; margin-bottom: 15px;'>
            <h3 style='margin: 0 0 5px 0; font-size: 16px; color: #333;'>About Diagnostic Test Evaluation</h3>
            </div>

            <div style='font-size: 14px; color: #333;'>
            <p><strong>What does this analysis do?</strong></p>
            <p>This function evaluates the performance of a diagnostic test by comparing test results
            against a gold standard (reference test). It calculates sensitivity, specificity, predictive values,
            and likelihood ratios to help determine how well the test identifies disease.</p>

            <p><strong>When to use it:</strong></p>
            <ul style='margin: 10px 0; padding-left: 25px;'>
            <li>Validating a new diagnostic test against established gold standard</li>
            <li>Comparing different diagnostic methods</li>
            <li>Determining optimal test cut-off values</li>
            <li>Clinical decision-making about test utility</li>
            </ul>

            <p><strong>Key Outputs:</strong></p>
            <ul style='margin: 10px 0; padding-left: 25px;'>
            <li><strong>Sensitivity:</strong> Ability to detect disease when present (avoid false negatives)</li>
            <li><strong>Specificity:</strong> Ability to confirm absence when healthy (avoid false positives)</li>
            <li><strong>PPV/NPV:</strong> Post-test probability after positive/negative result (depends on prevalence)</li>
            <li><strong>Likelihood Ratios:</strong> How much test result changes disease probability</li>
            <li><strong>Youden Index:</strong> Overall discriminatory power (optimal cut-off criterion)</li>
            <li><strong>Advanced Metrics:</strong> Balanced Accuracy, F1 Score, MCC, DOR</li>
            </ul>

            <p><strong>References:</strong></p>
            <ul style='margin: 10px 0; padding-left: 25px; font-size: 13px;'>
            <li>Altman DG, Bland JM. Diagnostic tests. 1: Sensitivity and specificity. BMJ. 1994 Jun 11;308(6943):1552. doi: 10.1136/bmj.308.6943.1552. PMID: 8019315; PMCID: PMC2540489.</li>
            <li>Deeks JJ, Altman DG. Diagnostic tests 4: likelihood ratios. BMJ 2004;329:168-169</li>
            <li>epiR package documentation: <a href='https://cran.r-project.org/package=epiR' target='_blank'>CRAN</a></li>
            </ul>
            </div>
            </div>"
        },

        .createAssumptionsPanel = function(TP, TN, FP, FN, prev) {

            warnings <- character()

            # Check sample size adequacy
            if (TP < 10 || TN < 10) {
                warnings <- c(warnings, sprintf(
                    "<li style='color: #d9534f;'><strong>Small sample size:</strong> TP=%d, TN=%d.
                    Confidence intervals may be unreliable. Consider n ≥ 30 per group.</li>",
                    TP, TN
                ))
            }

            # Check for extreme prevalence
            if (prev < 0.05 || prev > 0.95) {
                warnings <- c(warnings, sprintf(
                    "<li style='color: #f0ad4e;'><strong>Extreme prevalence:</strong> %.1f%%.
                    PPV/NPV estimates may be unstable. Verify in target population.</li>",
                    prev * 100
                ))
            }

            # Check for zero cells
            if (FP == 0 || FN == 0) {
                warnings <- c(warnings,
                    "<li style='color: #f0ad4e;'><strong>Zero cells detected:</strong>
                    Perfect sensitivity or specificity. May indicate overfitting or insufficient validation.</li>"
                )
            }

            # Check for very small error counts
            if ((FP > 0 && FP < 5) || (FN > 0 && FN < 5)) {
                warnings <- c(warnings,
                    "<li style='color: #f0ad4e;'><strong>Very few errors:</strong>
                    Small counts in FP or FN cells may lead to unstable estimates.</li>"
                )
            }

            warning_html <- if (length(warnings) > 0) {
                sprintf("<div style='background: #fff3cd; padding: 15px; margin: 10px 0; border-left: 4px solid #f0ad4e;'>
                <h4 style='margin-top: 0; color: #856404;'>⚠ Warnings</h4>
                <ul style='margin: 10px 0; padding-left: 20px;'>%s</ul>
                </div>", paste(warnings, collapse = "\n"))
            } else {
                "<div style='background: #d4edda; padding: 15px; margin: 10px 0; border-left: 4px solid #28a745;'>
                <p style='margin: 0; color: #155724;'><strong>✓ No issues detected</strong> - Sample size and distribution appear adequate.</p>
                </div>"
            }

            sprintf(
                "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>
                <div style='background: #f5f5f5; border: 2px solid #333; padding: 15px; margin-bottom: 15px;'>
                <h3 style='margin: 0 0 5px 0; font-size: 16px; color: #333;'>Assumptions & Caveats</h3>
                </div>

                <div style='font-size: 14px; color: #333;'>
                <p><strong>Key Assumptions:</strong></p>
                <ul style='margin: 10px 0; padding-left: 25px;'>
                <li><strong>Gold standard validity:</strong> Reference test must be highly accurate (near 100%% sensitivity/specificity)</li>
                <li><strong>Independent assessment:</strong> Test and gold standard should be evaluated independently (blinded)</li>
                <li><strong>Representative sample:</strong> Study population should match intended clinical use population</li>
                <li><strong>Disease spectrum:</strong> Include appropriate mix of disease severity (avoid spectrum bias)</li>
                <li><strong>Prevalence dependence:</strong> PPV/NPV vary with disease prevalence; verify in target setting</li>
                </ul>

                <p><strong>Common Pitfalls:</strong></p>
                <ul style='margin: 10px 0; padding-left: 25px;'>
                <li><strong>Verification bias:</strong> Not all test-positive patients receive gold standard confirmation</li>
                <li><strong>Incorporation bias:</strong> Gold standard includes results of the test being evaluated</li>
                <li><strong>Spectrum bias:</strong> Study population has more severe disease than clinical practice</li>
                <li><strong>Prevalence extrapolation:</strong> Applying PPV/NPV from high-prevalence study to low-prevalence screening</li>
                </ul>

                %s

                <p><strong>Sample Size Guidance:</strong></p>
                <ul style='margin: 10px 0; padding-left: 25px;'>
                <li>Minimum 30-50 diseased cases (for sensitivity estimation)</li>
                <li>Minimum 30-50 healthy controls (for specificity estimation)</li>
                <li>For rare diseases (prevalence < 5%%), consider n ≥ 200 total</li>
                <li>Larger samples needed for precise CI estimation</li>
                </ul>
                </div>
                </div>",
                warning_html
            )
        },

        .createGlossary = function() {
            "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>
            <div style='background: #f5f5f5; border: 2px solid #333; padding: 15px; margin-bottom: 15px;'>
            <h3 style='margin: 0 0 5px 0; font-size: 16px; color: #333;'>Clinical Terms Glossary</h3>
            </div>

            <div style='font-size: 14px; color: #333;'>
            <dl style='margin: 0;'>
            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>Sensitivity (True Positive Rate)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Proportion of diseased patients correctly identified. <em>Clinical use:</em> How good is this test at catching disease?</dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>Specificity (True Negative Rate)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Proportion of healthy patients correctly identified. <em>Clinical use:</em> How good is this test at confirming health?</dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>PPV (Positive Predictive Value)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Probability of disease given positive test. <em>Clinical use:</em> If test is positive, how likely is disease? <strong>Depends on prevalence.</strong></dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>NPV (Negative Predictive Value)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Probability of health given negative test. <em>Clinical use:</em> If test is negative, how likely is patient healthy? <strong>Depends on prevalence.</strong></dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>LR+ (Positive Likelihood Ratio)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>How much positive test increases odds of disease. <em>Interpretation:</em> >10 = strong evidence, 5-10 = moderate, 2-5 = weak, <2 = minimal.</dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>LR- (Negative Likelihood Ratio)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>How much negative test decreases odds of disease. <em>Interpretation:</em> <0.1 = strong evidence against, 0.1-0.2 = moderate, 0.2-0.5 = weak, >0.5 = minimal.</dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>Youden Index (J)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Sensitivity + Specificity - 1. Range: -1 to +1. <em>Clinical use:</em> Optimal cut-off selection. >0.8 = excellent, 0.6-0.8 = good, 0.4-0.6 = fair, <0.4 = poor.</dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>Balanced Accuracy</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Average of sensitivity and specificity. Better than raw accuracy for imbalanced datasets. >0.9 = excellent, 0.8-0.9 = good.</dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>F1 Score</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Harmonic mean of sensitivity and PPV. Useful when false negatives and false positives are equally costly. >0.8 = excellent.</dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>MCC (Matthews Correlation Coefficient)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Balanced measure accounting for class imbalance. Range: -1 to +1. >0.8 = excellent, 0.6-0.8 = good, 0.4-0.6 = fair.</dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>DOR (Diagnostic Odds Ratio)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Odds of positive test in diseased vs healthy. <em>Interpretation:</em> >25 = strong, 5-25 = moderate, 2-5 = weak, <2 = poor discrimination.</dd>
            </dl>
            </div>
            </div>"
        },

        .getRecommendation = function(Youden, Accuracy, LRP, LRN) {
            if (Youden > private$.YOUDEN_EXCELLENT && Accuracy > private$.ACCURACY_EXCELLENT && LRP > 10 && LRN < 0.1) {
                "This test shows excellent performance across all metrics. Recommended for clinical use."
            } else if (Youden > private$.YOUDEN_GOOD && Accuracy > private$.ACCURACY_GOOD) {
                "This test shows good performance. Consider clinical implementation with appropriate quality controls."
            } else if (Youden > private$.YOUDEN_FAIR) {
                "This test shows fair performance. Use with caution; consider combining with other diagnostic information."
            } else {
                "This test shows limited discriminatory ability. Not recommended as standalone diagnostic tool. Consider alternative tests or additional validation."
            }
        }

        ))
