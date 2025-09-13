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
                cTable$addRow(rowKey = "Test Positive", values = list(newtest = .("Test Positive")))
                cTable$addRow(rowKey = "Test Negative", values = list(newtest = .("Test Negative")))
                cTable$addRow(rowKey = "Total", values = list(newtest = .("Total")))
            },

            # Enhanced input validation for categorical diagnostic data
            .validateCategoricalInputs = function() {
                # Check for required variables with helpful messages
                if (length(self$options$gold) == 0) {
                    stop(.("Please select a gold standard (reference) variable"))
                }
                if (length(self$options$newtest) == 0) {
                    stop(.("Please select a test variable to evaluate"))
                }
                if (length(self$options$goldPositive) == 0) {
                    stop(.("Please select the positive level for the gold standard variable"))
                }
                if (length(self$options$testPositive) == 0) {
                    stop(.("Please select the positive level for the test variable"))
                }
                
                # Check data availability
                if (is.null(self$data) || nrow(self$data) == 0) {
                    stop(.("No data available for analysis. Please ensure your data is loaded."))
                }
                
                # Validate data has enough cases
                if (nrow(self$data) < 4) {
                    stop(.("Insufficient data: At least 4 cases are required for diagnostic test analysis"))
                }
                
                # Validate prior probability if specified
                if (self$options$pp && (self$options$pprob <= 0 || self$options$pprob >= 1)) {
                    stop(paste0("Population prevalence must be between 0 and 1 (exclusive). Current value: ", self$options$pprob, ". Please enter a value like 0.05 for 5% prevalence."))
                }
                
                # Enforce UI business logic - mutual exclusion
                if (self$options$pp && self$options$ci) {
                    stop(.("Cannot use both population prevalence and confidence intervals simultaneously. Please choose one option."))
                }
            },

            # Prepare analysis data with efficient processing
            .prepareAnalysisData = function() {
                # Single data processing pipeline to avoid multiple copies
                mydata <- jmvcore::naOmit(self$data)
                
                if (nrow(mydata) < nrow(self$data)) {
                    warning(paste0("Removed ", nrow(self$data) - nrow(mydata), " rows with missing values"))
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
                
                # Validate contingency table structure after data preparation
                test_table <- table(mydata$testVariable2, mydata$goldVariable2)
                
                # Check for empty cells that would cause problems
                if (any(dim(test_table) != c(2, 2))) {
                    stop(.("Invalid data structure: Both test and gold standard variables must have exactly 2 levels each"))
                }
                
                # Check for zero cells that would cause division by zero
                if (any(test_table == 0)) {
                    warning(.("Zero counts detected in contingency table. Results may be unstable. Consider collecting more data."))
                }
                
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
            
            # Basic missing data summary
            .analyzeMissingData = function(original_data, processed_data) {
                if (nrow(original_data) == nrow(processed_data)) {
                    return(.("No missing data detected."))
                }
                
                missing_count <- nrow(original_data) - nrow(processed_data)
                missing_percent <- round((missing_count / nrow(original_data)) * 100, 1)
                
                analysis <- paste0("Missing data summary: ", missing_count, " cases (", missing_percent, "%) with missing values removed. Complete case analysis performed.")
                
                return(analysis)
            },
            
            # Generate natural language summary for clinical use
            .generateNaturalLanguageSummary = function(sens, spec, ppv, npv, lr_pos, lr_neg, 
                                                      prevalence, test_name, gold_name) {
                # Determine test quality
                test_quality <- dplyr::case_when(
                    sens >= 0.9 && spec >= 0.9 ~ "excellent",
                    sens >= 0.8 && spec >= 0.8 ~ "good", 
                    sens >= 0.7 || spec >= 0.7 ~ "moderate",
                    TRUE ~ "limited"
                )
                
                # Determine primary clinical utility
                primary_utility <- dplyr::case_when(
                    sens >= 0.9 && spec < 0.8 ~ "ruling out disease",
                    spec >= 0.9 && sens < 0.8 ~ "confirming disease",
                    sens >= 0.8 && spec >= 0.8 ~ "both ruling out and confirming disease",
                    TRUE ~ "diagnostic screening"
                )
                
                # Generate summary
                summary <- sprintf(
                    .("<div style='margin: 15px; padding: 15px; border-left: 5px solid #4CAF50; background: #f1f8e9;'>" %+%
                    "<h3 style='color: #2E7D32; margin-top: 0;'>📋 Clinical Summary</h3>" %+%
                    "<p style='font-size: 16px;'><strong>Analysis:</strong> Diagnostic test performance evaluation comparing %s against gold standard %s.</p>" %+%
                    "<p><strong>Sample:</strong> %d cases analyzed with %.1f%% disease prevalence.</p>" %+%
                    "<p><strong>Test Performance:</strong> The test shows <strong>%s</strong> discriminatory ability with sensitivity of <strong>%.1f%%</strong> and specificity of <strong>%.1f%%</strong>.</p>" %+%
                    "<p><strong>Clinical Utility:</strong> This test is most useful for <strong>%s</strong> in the clinical setting.</p>" %+%
                    "<p><strong>Key Findings:</strong> When positive, the test increases disease probability to <strong>%.1f%%</strong> (PPV). When negative, it reduces disease probability to <strong>%.1f%%</strong> (NPV).</p>" %+%
                    "</div>"),
                    test_name, gold_name,
                    # Will be filled in by caller
                    0, prevalence * 100,  # Placeholder for sample size
                    test_quality, sens * 100, spec * 100,
                    primary_utility,
                    ppv * 100, (100 - npv * 100)
                )
                
                return(summary)
            },
            
            # Generate copy-ready report template
            .generateReportTemplate = function(sens, spec, ppv, npv, lr_pos, lr_neg, 
                                             sens_ci = NULL, spec_ci = NULL, test_name, gold_name) {
                # Create confidence interval text if available
                ci_text <- if (!is.null(sens_ci) && !is.null(spec_ci)) {
                    sprintf(.("(95%% CI: sensitivity %.1f-%.1f%%, specificity %.1f-%.1f%%)"), 
                           sens_ci[1]*100, sens_ci[2]*100, spec_ci[1]*100, spec_ci[2]*100)
                } else {
                    ""
                }
                
                # Determine clinical interpretation
                interpretation <- dplyr::case_when(
                    lr_pos >= 10 ~ .("strong evidence for disease when positive"),
                    lr_pos >= 5 ~ .("moderate evidence for disease when positive"),
                    lr_pos >= 2 ~ .("weak evidence for disease when positive"),
                    TRUE ~ .("minimal evidence for disease when positive")
                )
                
                # Generate template
                template <- sprintf(
                    .("<div style='margin: 15px; padding: 15px; border: 2px dashed #2196F3; background: #e3f2fd;'>" %+%
                    "<h3 style='color: #1976D2; margin-top: 0;'>📄 Copy-Ready Clinical Report</h3>" %+%
                    "<div style='background: white; padding: 10px; border-radius: 5px; font-family: Arial, sans-serif;'>" %+%
                    "<p><strong>DIAGNOSTIC TEST EVALUATION</strong></p>" %+%
                    "<p>We evaluated the diagnostic performance of %s compared to the gold standard %s. " %+%
                    "The test demonstrated a sensitivity of %.1f%% and specificity of %.1f%% %s. " %+%
                    "The positive predictive value was %.1f%% and negative predictive value was %.1f%%. " %+%
                    "The positive likelihood ratio of %.1f provides %s. " %+%
                    "These results suggest the test may be clinically useful for diagnostic evaluation.</p>" %+%
                    "</div>" %+%
                    "<p style='font-size: 12px; color: #666;'><em>Copy the text above for your clinical report. Modify as needed for your specific context.</em></p>" %+%
                    "</div>"),
                    test_name, gold_name,
                    sens * 100, spec * 100, ci_text,
                    ppv * 100, npv * 100,
                    lr_pos, interpretation
                )
                
                return(template)
            },
            
            # Enhanced misuse detection and warnings
            .detectMisuse = function(conf_table, prevalence, n_total) {
                warnings <- c()
                
                # Check for small cell counts
                if (any(conf_table < 5)) {
                    warnings <- c(warnings, 
                        .("⚠️ Small cell counts detected (< 5). Results may be unstable. Consider collecting more data or using exact methods."))
                }
                
                # Check for extreme prevalence
                if (prevalence < 0.05) {
                    warnings <- c(warnings,
                        .("⚠️ Very low disease prevalence (< 5%). Positive predictive value may be unreliable. Consider the clinical context carefully."))
                }
                
                if (prevalence > 0.95) {
                    warnings <- c(warnings,
                        .("⚠️ Very high disease prevalence (> 95%). Negative predictive value may be unreliable. Verify your data coding."))
                }
                
                # Check for small sample size
                if (n_total < 30) {
                    warnings <- c(warnings,
                        .("⚠️ Small sample size (< 30). Confidence intervals may be wide. Interpret results cautiously."))
                }
                
                # Check for unbalanced data
                pos_ratio <- sum(conf_table[1,]) / n_total
                if (pos_ratio < 0.1 || pos_ratio > 0.9) {
                    warnings <- c(warnings,
                        .("⚠️ Highly unbalanced test results. Consider the appropriateness of the selected positive level."))
                }
                
                return(warnings)
            },
            
            # Generate About This Analysis content
            .generateAboutAnalysis = function() {
                about_content <- paste0(
                    "<div style='margin: 15px; padding: 15px; background: #f5f5f5; border-radius: 8px;'>",
                    "<h3 style='color: #1976D2; margin-top: 0;'>📚 About Diagnostic Test Evaluation</h3>",
                    
                    "<h4 style='color: #424242;'>", .("What This Analysis Does"), "</h4>",
                    "<p>", .("This function evaluates diagnostic test performance by comparing test results to a gold standard (reference). It calculates key diagnostic accuracy measures including sensitivity, specificity, predictive values, and likelihood ratios."), "</p>",
                    
                    "<h4 style='color: #424242;'>", .("When to Use This Analysis"), "</h4>",
                    "<ul>",
                    "<li>", .("Validating new diagnostic tests or biomarkers"), "</li>",
                    "<li>", .("Comparing performance of different diagnostic methods"), "</li>",
                    "<li>", .("Clinical validation studies"), "</li>",
                    "<li>", .("Quality assurance for laboratory tests"), "</li>",
                    "<li>", .("Medical device evaluation"), "</li>",
                    "</ul>",
                    
                    "<h4 style='color: #424242;'>", .("Key Output Measures"), "</h4>",
                    "<ul>",
                    "<li><strong>", .("Sensitivity"), ":</strong> ", .("Ability to correctly identify disease (true positive rate)"), "</li>",
                    "<li><strong>", .("Specificity"), ":</strong> ", .("Ability to correctly identify healthy individuals (true negative rate)"), "</li>",
                    "<li><strong>", .("PPV (Positive Predictive Value)"), ":</strong> ", .("Probability of disease given positive test"), "</li>",
                    "<li><strong>", .("NPV (Negative Predictive Value)"), ":</strong> ", .("Probability of being healthy given negative test"), "</li>",
                    "<li><strong>", .("LR+ (Positive Likelihood Ratio)"), ":</strong> ", .("How much a positive test increases disease odds"), "</li>",
                    "<li><strong>", .("LR- (Negative Likelihood Ratio)"), ":</strong> ", .("How much a negative test decreases disease odds"), "</li>",
                    "</ul>",
                    
                    "<h4 style='color: #424242;'>", .("Clinical Interpretation Guidelines"), "</h4>",
                    "<div style='background: #e8f5e8; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                    "<strong>", .("Excellent Tests"), ":</strong><br>",
                    "• ", .("Sensitivity/Specificity ≥ 90%"), "<br>",
                    "• ", .("LR+ ≥ 10, LR- ≤ 0.1"), "<br>",
                    "</div>",
                    "<div style='background: #fff3cd; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                    "<strong>", .("Good Tests"), ":</strong><br>", 
                    "• ", .("Sensitivity/Specificity 80-89%"), "<br>",
                    "• ", .("LR+ 5-9.9, LR- 0.1-0.2"), "<br>",
                    "</div>",
                    
                    "<h4 style='color: #424242;'>", .("Important Considerations"), "</h4>",
                    "<ul>",
                    "<li>", .("Predictive values depend on disease prevalence in your population"), "</li>",
                    "<li>", .("High sensitivity is crucial for screening tests (rule out disease)"), "</li>",
                    "<li>", .("High specificity is crucial for confirmatory tests (rule in disease)"), "</li>",
                    "<li>", .("Consider clinical consequences of false positives vs false negatives"), "</li>",
                    "<li>", .("Results are only as good as your gold standard"), "</li>",
                    "</ul>",
                    
                    "</div>"
                )
                
                return(about_content)
            },
            
            # Centralized footnote management with clinical interpretation
            .addFootnotes = function() {
                if (!self$options$fnote) return()
                
                # nTable footnotes
                nTable <- self$results$nTable
                footnotes_n <- list(
                    TotalPop = .("Total Number of Subjects in complete case analysis"),
                    DiseaseP = .("Total Number of Subjects with Disease (Gold Standard Positive)"), 
                    DiseaseN = .("Total Number of Healthy Subjects (Gold Standard Negative)"),
                    TestP = .("Total Number of Positive Test Results"),
                    TestN = .("Total Number of Negative Test Results"),
                    TestT = .("Total Number of True Test Results (TP + TN)"),
                    TestW = .("Total Number of Wrong Test Results (FP + FN)")
                )
                
                for (col in names(footnotes_n)) {
                    nTable$addFootnote(rowNo = 1, col = col, footnotes_n[[col]])
                }
                
                # ratioTable footnotes with clinical interpretation
                ratioTable <- self$results$ratioTable
                footnotes_ratio <- list(
                    Sens = .("Sensitivity: Proportion of diseased patients correctly identified (TP rate). Higher is better for ruling OUT disease when negative."),
                    Spec = .("Specificity: Proportion of healthy patients correctly identified (TN rate). Higher is better for ruling IN disease when positive."), 
                    AccurT = .("Accuracy: Overall proportion of correct test results. Consider prevalence dependency."),
                    PrevalenceD = .("Disease Prevalence: Proportion with disease in this population. Affects predictive values."),
                    PPV = .("Positive Predictive Value: Probability of disease given positive test. Depends on prevalence and specificity."),
                    NPV = .("Negative Predictive Value: Probability of being healthy given negative test. Depends on prevalence and sensitivity."),
                    PostTestProbDisease = .("Post-test Probability (Disease+): Probability of disease after positive test using population prevalence."),
                    PostTestProbHealthy = .("Post-test Probability (Disease-): Probability of being healthy after negative test using population prevalence."),
                    LRP = .("Positive Likelihood Ratio: How much more likely a positive result is in diseased vs healthy patients. >10 = strong evidence, >5 = moderate, >2 = weak but potentially useful."),
                    LRN = .("Negative Likelihood Ratio: How much more likely a negative result is in diseased vs healthy patients. <0.1 = strong evidence against disease, <0.2 = moderate, <0.5 = weak.")
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


                # Populate missing data summary if requested
                if (self$options$od) {
                    self$results$missingDataSummary$setContent(missing_analysis)
                }










                # conf_table ----
                # Data is already efficiently recoded in .prepareAnalysisData()
                conf_table <- table(mydata[["testVariable2"]], mydata[["goldVariable2"]])



                # Extract confusion matrix values with error handling
                tryCatch({
                    TP <- conf_table[1, 1]
                    FP <- conf_table[1, 2]
                    FN <- conf_table[2, 1]
                    TN <- conf_table[2, 2]
                    
                    # Validate extracted values
                    if (any(is.na(c(TP, FP, FN, TN))) || any(c(TP, FP, FN, TN) < 0)) {
                        stop(.("Invalid contingency table values detected"))
                    }
                }, error = function(e) {
                    stop(paste0("Error extracting confusion matrix values: ", e$message, ". Please check your data formatting."))
                })




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

                # Calculate diagnostic metrics with division by zero protection
                Sens <- ifelse(DiseaseP > 0, TP / DiseaseP, 0)
                Spec <- ifelse(DiseaseN > 0, TN / DiseaseN, 0)
                AccurT <- ifelse(TotalPop > 0, TestT / TotalPop, 0)
                PrevalenceD <- ifelse(TotalPop > 0, DiseaseP / TotalPop, 0)
                PPV <- ifelse(TestP > 0, TP / TestP, 0)
                NPV <- ifelse(TestN > 0, TN / TestN, 0)


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




                # Calculate likelihood ratios with protection against division by zero
                LRP <- ifelse((1 - Spec) > 0, Sens / (1 - Spec), Inf)
                LRN <- ifelse(Spec > 0, (1 - Sens) / Spec, Inf)
                
                # Handle edge cases for likelihood ratios
                if (is.infinite(LRP) && Sens == 1 && Spec == 1) {
                    LRP <- NA  # Perfect test - undefined LR+
                }
                if (is.infinite(LRN) && Sens == 1 && Spec == 1) {
                    LRN <- NA  # Perfect test - undefined LR-
                }






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

                # Add clinical interpretation to results with error handling
                tryCatch({
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
                        ifelse(is.na(LRP), "undefined", LRP), interpretation$lr_pos_interp,
                        ifelse(is.na(LRN), "undefined", LRN), interpretation$lr_neg_interp,
                        PriorProb * 100,
                        PostTestProbDisease * 100,
                        PostTestProbHealthy * 100
                    )
                }, error = function(e) {
                    clinical_summary <- sprintf(
                        "<div style='margin: 15px; padding: 10px; border-left: 4px solid #ff9800; background: #fff3e0;'>" %+%
                        "<h4 style='color: #f57c00; margin-top: 0;'>Clinical Interpretation</h4>" %+%
                        "<p>Unable to generate detailed clinical interpretation due to data limitations.</p>" %+%
                        "<p><strong>Basic Results:</strong> Sensitivity: %.1f%%, Specificity: %.1f%%</p>" %+%
                        "</div>",
                        Sens * 100, Spec * 100
                    )
                })
                
                # Store clinical interpretation for display (if results object exists)
                tryCatch({
                    if ("clinicalInterpretation" %in% names(self$results)) {
                        self$results$clinicalInterpretation$setContent(clinical_summary)
                    }
                }, error = function(e) {
                    # Silently handle if clinical interpretation output doesn't exist
                })
                
                # Generate and store About This Analysis content
                tryCatch({
                    about_content <- private$.generateAboutAnalysis()
                    
                    if ("aboutAnalysis" %in% names(self$results)) {
                        self$results$aboutAnalysis$setContent(about_content)
                    }
                }, error = function(e) {
                    # Silently handle if about analysis output doesn't exist
                })
                
                # Generate and store natural language summary
                tryCatch({
                    test_name <- self$options$newtest
                    gold_name <- self$options$gold
                    
                    natural_summary <- private$.generateNaturalLanguageSummary(
                        Sens, Spec, PPV, NPV, LRP, LRN, 
                        PriorProb, test_name, gold_name
                    )
                    
                    # Update with actual sample size
                    natural_summary <- gsub("0 cases analyzed", 
                                          sprintf("%d cases analyzed", TotalPop), 
                                          natural_summary)
                    
                    if ("naturalLanguageSummary" %in% names(self$results)) {
                        self$results$naturalLanguageSummary$setContent(natural_summary)
                    }
                }, error = function(e) {
                    # Provide fallback summary
                    fallback_summary <- sprintf(
                        .("<div style='margin: 15px; padding: 15px; border-left: 5px solid #FF9800; background: #fff3e0;'>" %+%
                        "<h3 style='color: #F57C00; margin-top: 0;'>📋 Basic Summary</h3>" %+%
                        "<p>Diagnostic test analysis completed with %d cases. Sensitivity: %.1f%%, Specificity: %.1f%%</p>" %+%
                        "</div>"),
                        TotalPop, Sens * 100, Spec * 100
                    )
                    
                    if ("naturalLanguageSummary" %in% names(self$results)) {
                        self$results$naturalLanguageSummary$setContent(fallback_summary)
                    }
                })
                
                # Generate and store copy-ready report template
                tryCatch({
                    report_template <- private$.generateReportTemplate(
                        Sens, Spec, PPV, NPV, LRP, LRN,
                        NULL, NULL, test_name, gold_name  # CI values would be added if available
                    )
                    
                    if ("reportTemplate" %in% names(self$results)) {
                        self$results$reportTemplate$setContent(report_template)
                    }
                }, error = function(e) {
                    # Provide basic template
                    basic_template <- sprintf(
                        .("<div style='margin: 15px; padding: 15px; border: 2px dashed #FF9800;'>" %+%
                        "<h3>📄 Basic Report Template</h3>" %+%
                        "<p>Test sensitivity: %.1f%%, specificity: %.1f%%, positive predictive value: %.1f%%, negative predictive value: %.1f%%</p>" %+%
                        "</div>"),
                        Sens * 100, Spec * 100, PPV * 100, NPV * 100
                    )
                    
                    if ("reportTemplate" %in% names(self$results)) {
                        self$results$reportTemplate$setContent(basic_template)
                    }
                })
                
                # Detect misuse and display warnings
                tryCatch({
                    warnings <- private$.detectMisuse(conf_table, PrevalenceD, TotalPop)
                    
                    if (length(warnings) > 0) {
                        warning_text <- paste(warnings, collapse = "<br>")
                        warning_panel <- sprintf(
                            "<div style='margin: 10px; padding: 10px; border-left: 4px solid #FF5722; background: #ffebee;'>" %+%
                            "<h4 style='color: #D32F2F; margin-top: 0;'>⚠️ Analysis Warnings</h4>" %+%
                            "%s" %+%
                            "</div>",
                            warning_text
                        )
                        
                        # Prepend warnings to clinical interpretation
                        if ("clinicalInterpretation" %in% names(self$results)) {
                            current_content <- clinical_summary
                            self$results$clinicalInterpretation$setContent(paste0(warning_panel, current_content))
                        }
                    }
                }, error = function(e) {
                    # Silently handle misuse detection errors
                })
                
                # Add footnotes using centralized method
                private$.addFootnotes()





                # 95% CI ----

                ci <- self$options$ci

                if (ci) {
                    # epiR confidence intervals with error handling
                    tryCatch({
                        epirresult <- epiR::epi.tests(dat = conf_table)
                    }, error = function(e) {
                        stop(paste0("Error calculating confidence intervals: ", e$message, ". This may be due to insufficient data or extreme values."))
                    })


                    epirresult2 <- summary(epirresult)
                    epirresult2 <- as.data.frame(epirresult2) %>%
                        tibble::rownames_to_column(.data = ., var = 'statsabv')


                    epirresult2$statsnames <-
                        c(
                            .("Apparent prevalence"),
                            .("True prevalence"),
                            .("Test sensitivity"),
                            .("Test specificity"),
                            .("Diagnostic accuracy"),
                            .("Diagnostic odds ratio"),
                            .("Number needed to diagnose"),
                            .("Youden's index"),
                            .("Positive predictive value"),
                            .("Negative predictive value"),
                            .("Likelihood ratio of a positive test"),
                            .("Likelihood ratio of a negative test"),
                            .("Proportion of subjects with the outcome ruled out"),
                            .("Proportion of subjects with the outcome ruled in"),
                            .("Proportion of false positives"),
                            .("Proportion of false negative"),
                            .("False Discovery Rate"),
                            .("False Omission Rate")
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
                            .("Proportion of all tests that give a correct result.")
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
                            .("How much more likely will the test make a correct diagnosis than an incorrect diagnosis in patients with the disease.")
                        )

                        epirTable_number$addFootnote(
                            rowNo = 2,
                            col = "statsnames",
                            .("Number of patients that need to be tested to give one correct positive test.")
                        )

                        epirTable_number$addFootnote(
                            rowNo = 3,
                            col = "statsnames",
                            .("Youden's index is the difference between the true positive rate and the false positive rate. Youden's index ranges from -1 to +1 with values closer to 1 if both sensitivity and specificity are high (i.e. close to 1).")
                        )

                    }


                }





                # Prepare Fagan Nomogram Data ----
                if (self$options$fagan) {
                    plotData1 <- list(
                        "Prevalence" = PriorProb,
                        "Sens" = Sens,
                        "Spec" = Spec,
                        "Plr" = LRP,
                        "Nlr" = LRN
                    )

                    image1 <- self$results$plot1
                    image1$setState(plotData1)
                }



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












        )
    )
