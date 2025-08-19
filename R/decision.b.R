#' @title Medical Decision Analysis
#' @description Implements comprehensive medical decision analysis including:
#' @details This module provides tools for analyzing diagnostic test performance
#'   with options for various visualization methods and statistical comparisons.
#'   - Sensitivity, specificity and predictive values
#' @section Usage:
#'   1. Provide test and reference standard data
#'   2. Select analysis options
#'   3. View results in tables and plots
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import boot
#' @importFrom stats quantile qnorm
#' @importFrom dplyr %>% mutate case_when
#' @importFrom forcats as_factor fct_relevel
#' @importFrom epiR epi.tests


#  @references
#    - DeLong et al. (1988) for ROC comparison
#   - Hanley & McNeil (1982) for AUC confidence intervals
#    - ROC curve analysis with confidence intervals
#    - Multiple test comparison
#    - Bootstrapped confidence intervals



decisionClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "decisionClass",
        inherit = decisionBase,
        private = list(
            # Constants for maintainability
            NOMOGRAM_LABEL_SIZE = 14/5,

            .init = function() {
                cTable <- self$results$cTable
                cTable$addRow(rowKey = "Test Positive", values = list(newtest = "Test Positive"))
                cTable$addRow(rowKey = "Test Negative", values = list(newtest = "Test Negative"))
                cTable$addRow(rowKey = "Total", values = list(newtest = "Total"))
            },

            # Consolidated input validation for categorical diagnostic data
            .validateCategoricalInputs = function() {
                # Check for required variables
                vars_count <- length(self$options$testPositive) + length(self$options$newtest) +
                             length(self$options$goldPositive) + length(self$options$gold)
                if (vars_count < 4) {
                    stop("Missing required variables: both test and gold standard variables must be specified with their positive levels")
                }
                
                # Check data availability
                if (is.null(self$data) || nrow(self$data) == 0) {
                    stop("No data provided for analysis")
                }
                
                # Validate prior probability if specified
                if (self$options$pp && (self$options$pprob <= 0 || self$options$pprob >= 1)) {
                    stop("Prior probability must be between 0 and 1 (current value: ", self$options$pprob, ")")
                }
                
                # Enforce UI business logic - mutual exclusion
                if (self$options$pp && self$options$ci) {
                    stop("Prior probability and confidence intervals cannot both be enabled")
                }
            },

            # Prepare analysis data with efficient processing
            .prepareAnalysisData = function() {
                # Single data processing pipeline to avoid multiple copies
                mydata <- jmvcore::naOmit(self$data)
                
                if (nrow(mydata) < nrow(self$data)) {
                    warning(sprintf("Removed %d rows with missing values", 
                                   nrow(self$data) - nrow(mydata)))
                }
                
                # Get variable names efficiently
                testVar <- jmvcore::constructFormula(terms = self$options$newtest) %>%
                          jmvcore::decomposeFormula() %>% unlist()
                goldVar <- jmvcore::constructFormula(terms = self$options$gold) %>%
                          jmvcore::decomposeFormula() %>% unlist()
                
                # Convert to factors and recode in single pipeline
                mydata[[testVar]] <- forcats::as_factor(mydata[[testVar]])
                mydata[[goldVar]] <- forcats::as_factor(mydata[[goldVar]])
                
                # Efficient recoding with single mutate
                mydata <- mydata %>% 
                    dplyr::mutate(
                        testVariable2 = dplyr::case_when(
                            .data[[testVar]] == self$options$testPositive ~ "Positive",
                            NA ~ NA_character_,
                            TRUE ~ "Negative"
                        ),
                        goldVariable2 = dplyr::case_when(
                            .data[[goldVar]] == self$options$goldPositive ~ "Positive", 
                            NA ~ NA_character_,
                            TRUE ~ "Negative"
                        )
                    ) %>%
                    dplyr::mutate(
                        testVariable2 = forcats::fct_relevel(testVariable2, "Positive"),
                        goldVariable2 = forcats::fct_relevel(goldVariable2, "Positive")
                    )
                
                return(list(data = mydata, testVar = testVar, goldVar = goldVar))
            },

            # Enhanced diagnostic accuracy interpretation helper
            .getDiagnosticInterpretation = function(lr_pos, lr_neg, sens, spec) {
                # Likelihood ratio interpretations based on clinical guidelines
                lr_pos_interp <- dplyr::case_when(
                    lr_pos >= 10 ~ "Large and often conclusive increase in probability of disease",
                    lr_pos >= 5 ~ "Moderate increase in probability of disease",
                    lr_pos >= 2 ~ "Small but potentially important increase in probability",
                    lr_pos > 1 ~ "Minimal increase in probability of disease",
                    TRUE ~ "Decreases probability of disease (test may be flawed)"
                )
                
                lr_neg_interp <- dplyr::case_when(
                    lr_neg <= 0.1 ~ "Large and often conclusive decrease in probability of disease",
                    lr_neg <= 0.2 ~ "Moderate decrease in probability of disease",
                    lr_neg <= 0.5 ~ "Small but potentially important decrease in probability",
                    lr_neg < 1 ~ "Minimal decrease in probability of disease",
                    TRUE ~ "Increases probability of disease (test may be flawed)"
                )
                
                # Overall test utility based on Youden's Index
                youden_index <- sens + spec - 1
                test_utility <- dplyr::case_when(
                    youden_index >= 0.5 ~ "Excellent discriminatory power",
                    youden_index >= 0.3 ~ "Good discriminatory power",
                    youden_index >= 0.1 ~ "Fair discriminatory power",
                    TRUE ~ "Poor discriminatory power - limited clinical utility"
                )
                
                return(list(
                    lr_pos_interp = lr_pos_interp,
                    lr_neg_interp = lr_neg_interp,
                    youden_index = youden_index,
                    test_utility = test_utility
                ))
            },
            
            # Enhanced missing data analysis
            .analyzeMissingData = function(original_data, processed_data) {
                if (nrow(original_data) == nrow(processed_data)) {
                    return("No missing data detected.")
                }
                
                missing_count <- nrow(original_data) - nrow(processed_data)
                missing_percent <- round((missing_count / nrow(original_data)) * 100, 1)
                
                # Get variable names
                testVar <- jmvcore::constructFormula(terms = self$options$newtest) %>%
                          jmvcore::decomposeFormula() %>% unlist()
                goldVar <- jmvcore::constructFormula(terms = self$options$gold) %>%
                          jmvcore::decomposeFormula() %>% unlist()
                
                # Analyze missing patterns
                test_missing <- sum(is.na(original_data[[testVar]]))
                gold_missing <- sum(is.na(original_data[[goldVar]]))
                both_missing <- sum(is.na(original_data[[testVar]]) & is.na(original_data[[goldVar]]))
                
                analysis <- sprintf(
                    "Missing data analysis: %d cases (%s%%) removed.\n" %+%
                    "Missing in %s: %d cases\n" %+%
                    "Missing in %s: %d cases\n" %+%
                    "Missing in both variables: %d cases\n" %+%
                    "Complete case analysis performed. Consider impact on generalizability.",
                    missing_count, missing_percent,
                    testVar, test_missing,
                    goldVar, gold_missing,
                    both_missing
                )
                
                return(analysis)
            },
            
            # Centralized footnote management with clinical interpretation
            .addFootnotes = function() {
                if (!self$options$fnote) return()
                
                # nTable footnotes
                nTable <- self$results$nTable
                footnotes_n <- list(
                    TotalPop = "Total Number of Subjects in complete case analysis",
                    DiseaseP = "Total Number of Subjects with Disease (Gold Standard Positive)", 
                    DiseaseN = "Total Number of Healthy Subjects (Gold Standard Negative)",
                    TestP = "Total Number of Positive Test Results",
                    TestN = "Total Number of Negative Test Results",
                    TestT = "Total Number of True Test Results (TP + TN)",
                    TestW = "Total Number of Wrong Test Results (FP + FN)"
                )
                
                for (col in names(footnotes_n)) {
                    nTable$addFootnote(rowNo = 1, col = col, footnotes_n[[col]])
                }
                
                # ratioTable footnotes with clinical interpretation
                ratioTable <- self$results$ratioTable
                footnotes_ratio <- list(
                    Sens = "Sensitivity: Proportion of diseased patients correctly identified (TP rate). Higher is better for ruling OUT disease when negative.",
                    Spec = "Specificity: Proportion of healthy patients correctly identified (TN rate). Higher is better for ruling IN disease when positive.", 
                    AccurT = "Accuracy: Overall proportion of correct test results. Consider prevalence dependency.",
                    PrevalenceD = "Disease Prevalence: Proportion with disease in this population. Affects predictive values.",
                    PPV = "Positive Predictive Value: Probability of disease given positive test. Depends on prevalence and specificity.",
                    NPV = "Negative Predictive Value: Probability of being healthy given negative test. Depends on prevalence and sensitivity.",
                    PostTestProbDisease = "Post-test Probability (Disease+): Probability of disease after positive test using population prevalence.",
                    PostTestProbHealthy = "Post-test Probability (Disease-): Probability of being healthy after negative test using population prevalence.",
                    LRP = "Positive Likelihood Ratio: How much more likely a positive result is in diseased vs healthy patients. >10 = strong evidence, >5 = moderate, >2 = weak but potentially useful.",
                    LRN = "Negative Likelihood Ratio: How much more likely a negative result is in diseased vs healthy patients. <0.1 = strong evidence against disease, <0.2 = moderate, <0.5 = weak."
                )
                
                for (col in names(footnotes_ratio)) {
                    ratioTable$addFootnote(rowNo = 1, col = col, footnotes_ratio[[col]])
                }
            }




            ,
            .run = function() {
                # Early return if variables not selected
                if (length(self$options$testPositive) + length(self$options$newtest) +
                    length(self$options$goldPositive) + length(self$options$gold) < 4)
                    return()

                # Consolidated input validation
                private$.validateCategoricalInputs()

                # Efficient data preparation with missing data analysis
                original_data <- self$data
                prepared_data <- private$.prepareAnalysisData()
                mydata <- prepared_data$data
                testVariable <- prepared_data$testVar
                goldVariable <- prepared_data$goldVar
                
                # Enhanced missing data reporting
                missing_analysis <- private$.analyzeMissingData(original_data, mydata)
                if (nrow(original_data) != nrow(mydata)) {
                    message(missing_analysis)
                }

                # Table 1 ----

                results1 <- mydata %>% dplyr::select(.data[[testVariable]], .data[[goldVariable]]) %>%
                    table()

                self$results$text1$setContent(results1)


                result2 <- mydata %>%
                    dplyr::group_by_all() %>%
                    dplyr::count() %>%
                    as.data.frame() %>%
                    htmlTable::htmlTable()

                self$results$text2$setContent(result2)


                # Extract and validate variables
                # vars <- list(
                #   test = self$options$newtest,
                #   gold = self$options$gold,
                #   test_pos = self$options$testPositive,
                #   gold_pos = self$options$goldPositive
                # )


                # Prepare data
                # prepared_data <- private$.prepare_data(self$data, vars)
                # self$results$text3$setContent(prepared_data)



                # Calculate basic metrics
                # conf_matrix <- table(prepared_data$test, prepared_data$gold)

                # self$results$text3$setContent(conf_matrix)

                # metrics <- private$.calculate_basic_metrics(
                #   conf_matrix[2,2], # TP
                #   conf_matrix[2,1], # FP
                #   conf_matrix[1,1], # TN
                #   conf_matrix[1,2]  # FN
                # )

                # self$results$text3$setContent(list(conf_matrix, metrics))

                # Populate main results tables
                # private$.populate_main_tables(conf_matrix, metrics)



                # Calculate confidence intervals if requested
                # if (self$options$ci) {
                #   ci <- private$.calculate_confidence_intervals(data, metrics)
                #
                #   self$results$text3$setContent(ci)
                #
                #   # private$.populate_ci_tables(ci)
                #
                #   }

                # Create ROC plot if requested
                # if (self$options$roc) {
                #   roc_plot <- private$.create_roc_plot(
                #     metrics$sensitivity,
                #     metrics$specificity,
                #     ci
                #   )
                #   self$results$plot_roc$setState(roc_plot)
                # }

                # Compare tests if requested
                # if (self$options$compare_tests && !is.null(self$options$additional_test)) {
                #   test_comparison <- private$.compare_tests(
                #     metrics,
                #     private$.calculate_metrics_for_additional_test(),
                #     nrow(data)
                #   )
                #   private$.populate_comparison_tables(test_comparison)
                # }





                # results2 <- as.data.frame(results1)
                #
                # namesfrom <- names(results2)[2]
                #
                # results2 <- results2 %>%
                #         tidyr::pivot_wider(data = .,
                #                        names_from = namesfrom,
                #                        values_from = Freq)
                #
                #
                # self$results$text2$setContent(results2)



                # Original Table -----

                # origTable <- self$results$origTable


                # xnames <- results2[,1]
                #
                #
                # data_frame <- results2
                # for (i in seq_along(data_frame[,1,drop = T])) {
                #     origTable$addRow(rowKey = i, values = c(data_frame[i,]))
                # }


                # conf_table ----
                # Data is already efficiently recoded in .prepareAnalysisData()
                conf_table <- table(mydata[["testVariable2"]], mydata[["goldVariable2"]])


                # Caret ----
                # results_caret <- caret::confusionMatrix(conf_table, positive = "Positive")


                # self$results$text2$setContent(
                #     list(
                #         conf_table,
                #         results_caret
                #         )
                # )

                TP <- conf_table[1, 1]

                FP <- conf_table[1, 2]

                FN <- conf_table[2, 1]

                TN <- conf_table[2, 2]




                # Cross Table in jamovi style ----

                cTable <- self$results$cTable

                cTable$setRow(
                    rowKey = "Test Positive",
                    values = list(
                        newtest = "Test Positive",
                        GP = TP,
                        GN = FP,
                        Total = TP + FP
                    )
                )


                cTable$setRow(
                    rowKey = "Test Negative",
                    values = list(
                        newtest = "Test Negative",
                        GP = FN,
                        GN = TN,
                        Total = FN + TN
                    )
                )

                cTable$setRow(
                    rowKey = "Total",
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

                Sens <- TP / DiseaseP

                Spec <- TN / DiseaseN

                AccurT <- TestT / TotalPop

                PrevalenceD <- DiseaseP / TotalPop

                PPV <- TP / TestP

                NPV <- TN / TestN


                pp <- self$options$pp
                pprob <- self$options$pprob

                if (pp) {
                    # Known prior probability from population
                    PriorProb <- pprob
                } else {
                    # From ConfusionMatrix
                    PriorProb <- PrevalenceD
                }


                PostTestProbDisease <- (PriorProb * Sens) / ((PriorProb * Sens) + ((1 -
                                                                                        PriorProb) * (1 - Spec)))



                PostTestProbHealthy <- ((1 - PriorProb) * Spec) / (((1 - PriorProb) *
                                                                        Spec) + (PriorProb * (1 - Sens)))




                LRP <- Sens / (1 - Spec)

                LRN <- (1 - Sens) / Spec



                # Cache computed values
                # private$.state <- list(
                #     sens = Sens,
                #     spec = Spec,
                #     ppv = PPV,
                #     npv = NPV,
                #     tp = TP,
                #     fp = FP,
                #     tn = TN,
                #     fn = FN,
                #     n_pos = DiseaseP,
                #     n_neg = DiseaseN,
                #     prevalence = PriorProb
                # )



                # self$results$nTable2$setContent(
                #     list(
                #         tablename = "",
                #         TotalPop = TotalPop,
                #         DiseaseP = DiseaseP,
                #         DiseaseN = DiseaseN,
                #         TestP = TestP,
                #         TestN = TestN,
                #         TestT = TestT,
                #         TestW = TestW
                #     )
                # )



                # nTable Populate Table ----

                nTable <- self$results$nTable
                nTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = "",
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
                ratioTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = "",
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

                # Add clinical interpretation to results
                interpretation <- private$.getDiagnosticInterpretation(LRP, LRN, Sens, Spec)
                
                # Create comprehensive clinical interpretation
                clinical_summary <- sprintf(
                    "<div style='margin: 15px; padding: 10px; border-left: 4px solid #2196F3; background: #f8f9fa;'>" %+%
                    "<h4 style='color: #1976D2; margin-top: 0;'>Clinical Interpretation</h4>" %+%
                    "<p><strong>Test Performance Summary:</strong></p>" %+%
                    "<ul>" %+%
                    "<li><strong>Sensitivity:</strong> %.1f%% - %s</li>" %+%
                    "<li><strong>Specificity:</strong> %.1f%% - %s</li>" %+%
                    "<li><strong>Youden's Index:</strong> %.3f - %s</li>" %+%
                    "</ul>" %+%
                    "<p><strong>Likelihood Ratio Interpretation:</strong></p>" %+%
                    "<ul>" %+%
                    "<li><strong>Positive LR (%.1f):</strong> %s</li>" %+%
                    "<li><strong>Negative LR (%.2f):</strong> %s</li>" %+%
                    "</ul>" %+%
                    "<p><strong>Clinical Decision Making:</strong></p>" %+%
                    "<ul>" %+%
                    "<li>Pre-test probability: <strong>%.1f%%</strong></li>" %+%
                    "<li>Post-test probability (if positive): <strong>%.1f%%</strong></li>" %+%
                    "<li>Post-test probability (if negative): <strong>%.1f%%</strong></li>" %+%
                    "</ul></div>",
                    Sens * 100, if (Sens >= 0.9) "Excellent for ruling out disease" else if (Sens >= 0.8) "Good for ruling out disease" else "Limited ability to rule out disease",
                    Spec * 100, if (Spec >= 0.9) "Excellent for ruling in disease" else if (Spec >= 0.8) "Good for ruling in disease" else "Limited ability to rule in disease",
                    interpretation$youden_index, interpretation$test_utility,
                    LRP, interpretation$lr_pos_interp,
                    LRN, interpretation$lr_neg_interp,
                    PriorProb * 100,
                    PostTestProbDisease * 100,
                    PostTestProbHealthy * 100
                )
                
                # Store clinical interpretation for display (if results object exists)
                tryCatch({
                    if ("clinicalInterpretation" %in% names(self$results)) {
                        self$results$clinicalInterpretation$setContent(clinical_summary)
                    }
                }, error = function(e) {
                    # Silently handle if clinical interpretation output doesn't exist
                })
                
                # Add footnotes using centralized method
                private$.addFootnotes()




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


                    numberrows <- c("diag.or", "nndx", "youden", "lr.pos", "lr.neg")

                    epirresult_number <- epirresult2[epirresult2$statistic %in% numberrows, ]

                    epirresult_ratio <- epirresult2[epirresult2$statistic %in% ratiorows, ]





                    # epirTable_ratio -----

                    epirTable_ratio <- self$results$epirTable_ratio

                    data_frame <- epirresult_ratio
                    for (i in seq_along(data_frame[, 1, drop = T])) {
                        epirTable_ratio$addRow(rowKey = i,
                                               values = c(data_frame[i, ])) # This code produces a named vector/list, which is what the values argument expects
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
                    for (i in seq_along(data_frame[, 1, drop = T])) {
                        epirTable_number$addRow(rowKey = i,
                                                values = c(data_frame[i, ]))
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


                # self$results$plotcontent$setContent(plotData1)



                image1 <- self$results$plot1
                image1$setState(plotData1)




                # if (self$options$roc) {
                #     plotData2 <- list(
                #         sens = Sens,
                #         spec = Spec,
                #         tp = TP,
                #         fp = FP,
                #         tn = TN,
                #         fn = FN,
                #         n_pos = DiseaseP,
                #         n_neg = DiseaseN,
                #         thresholds = NULL  # Add thresholds if available
                #     )
                #
                #     image2 <- self$results$plot_roc
                #     image2$setState(plotData2)
                # }



            }


            ,
            .plot1 = function(image1, ggtheme, ...) {
                plotData1 <- image1$state

                plot1 <- nomogrammer(
                    Prevalence = plotData1$Prevalence,
                    Sens = plotData1$Sens,
                    Spec = plotData1$Spec,
                    Plr = plotData1$Plr,
                    Nlr = plotData1$Nlr,
                    Detail = TRUE,
                    NullLine = TRUE,
                    LabelSize = private$NOMOGRAM_LABEL_SIZE,
                    Verbose = TRUE
                )

                print(plot1)
                TRUE

            }



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






            # ,
            # .plot_roc = function(image2, ggtheme, ...) {
            #
            #     plotData2 <- image2$state
            #
            #     if (is.null(plotData2)) return(FALSE)
            #
            #     # Calculate confidence intervals
            #     auc <- 0.5 * (plotData2$sens * (1-plotData2$spec)) +
            #         0.5 * (1 * (1-(1-plotData2$spec))) +
            #         0.5 * ((1-plotData2$sens) * plotData2$spec)
            #
            #     ci <- auc_ci(auc, plotData2$n_pos, plotData2$n_neg)
            #
            #     # Create ROC curve points
            #     roc_points <- data.frame(
            #         fpr = c(0, 1-plotData2$spec, 1),
            #         tpr = c(0, plotData2$sens, 1)
            #     )
            #
            #     # Create plot with confidence band
            #     p <- ggplot(roc_points, aes(x=fpr, y=tpr)) +
            #         geom_line(color="blue", size=1) +
            #         geom_point(data=data.frame(
            #             fpr=1-plotData2$spec,
            #             tpr=plotData2$sens),
            #             color="red", size=3) +
            #         geom_abline(slope=1, intercept=0,
            #                     linetype="dashed", color="gray") +
            #         annotate("text", x=0.75, y=0.25,
            #                  label=sprintf("AUC = %.3f (%.3f-%.3f)",
            #                                auc, ci[1], ci[2])) +
            #         labs(x = "False Positive Rate (1 - Specificity)",
            #              y = "True Positive Rate (Sensitivity)",
            #              title = "ROC Curve") +
            #         theme_minimal() +
            #         coord_equal() +
            #         theme(plot.title = element_text(hjust = 0.5))
            #
            #     print(p)
            #     TRUE
            # }



            # ,
            # .plot_comparative_roc = function(image, ggtheme, ...) {
            #     test_data <- image$state
            #     if (is.null(test_data) || length(test_data) < 2) return(FALSE)
            #
            #     n_tests <- length(test_data)
            #
            #     # Create base plot
            #     p <- create_base_roc_plot(test_data)
            #
            #     # Add statistical tests if there are multiple tests
            #     if(n_tests >= 2) {
            #         p <- add_statistical_comparison(p, test_data)
            #     }
            #
            #     print(p)
            #     TRUE
            # }



        )
    )
