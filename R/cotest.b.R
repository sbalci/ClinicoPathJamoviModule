#' @title Co-Testing Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'

cotestClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "cotestClass",
        inherit = cotestBase,
        private = list(
            # Constants for better maintainability
            NOMOGRAM_LABEL_SIZE = 14/5,
            NUMERICAL_TOLERANCE = 1e-10,

            # Cache for expensive nomogram calculations
            .nomogramCache = NULL,
            .lastNomogramParams = NULL,
            .init = function() {
                # Initialize tables with row headers
                testParamsTable <- self$results$testParamsTable
                testParamsTable$addRow(rowKey = "test1", values = list(test = "Test 1"))
                testParamsTable$addRow(rowKey = "test2", values = list(test = "Test 2"))

                cotestResultsTable <- self$results$cotestResultsTable
                cotestResultsTable$addRow(rowKey = "test1_pos",
                                          values = list(scenario = "Test 1 Positive Only"))
                cotestResultsTable$addRow(rowKey = "test2_pos",
                                          values = list(scenario = "Test 2 Positive Only"))
                cotestResultsTable$addRow(rowKey = "both_pos",
                                          values = list(scenario = "Both Tests Positive"))
                cotestResultsTable$addRow(rowKey = "both_neg",
                                          values = list(scenario = "Both Tests Negative"))
            },

            .run = function() {
                # Get parameters from user inputs
                test1_sens <- self$options$test1_sens
                test1_spec <- self$options$test1_spec
                test2_sens <- self$options$test2_sens
                test2_spec <- self$options$test2_spec
                indep <- self$options$indep
                cond_dep_pos <- self$options$cond_dep_pos
                cond_dep_neg <- self$options$cond_dep_neg
                prevalence <- self$options$prevalence
                
                # Enhanced input validation with specific guidance
                private$.validateInputParameters(test1_sens, test1_spec, test2_sens, test2_spec, prevalence, indep, cond_dep_pos, cond_dep_neg)

                # Calculate likelihood ratios with numerical stability
                test1_plr <- private$.calculateLikelihoodRatio(test1_sens, (1 - test1_spec), "Test 1 Positive LR")
                test1_nlr <- private$.calculateLikelihoodRatio((1 - test1_sens), test1_spec, "Test 1 Negative LR")
                test2_plr <- private$.calculateLikelihoodRatio(test2_sens, (1 - test2_spec), "Test 2 Positive LR")
                test2_nlr <- private$.calculateLikelihoodRatio((1 - test2_sens), test2_spec, "Test 2 Negative LR")

                # Update test parameters table using helper method
                private$.updateTestParametersTable(test1_sens, test1_spec, test1_plr, test1_nlr, 
                                                   test2_sens, test2_spec, test2_plr, test2_nlr)

                # Calculate post-test probabilities for different scenarios
                results <- private$.calculatePostTestProbabilities(test1_sens, test1_spec, test2_sens, test2_spec, 
                                                                   prevalence, indep, cond_dep_pos, cond_dep_neg,
                                                                   test1_plr, test1_nlr, test2_plr, test2_nlr)
                
                # Extract results for easier access
                postest_prob_t1 <- results$postest_prob_t1
                postest_prob_t2 <- results$postest_prob_t2
                postest_prob_both <- results$postest_prob_both
                postest_prob_both_neg <- results$postest_prob_both_neg
                postest_odds_t1 <- results$postest_odds_t1
                postest_odds_t2 <- results$postest_odds_t2
                postest_odds_both <- results$postest_odds_both
                postest_odds_both_neg <- results$postest_odds_both_neg
                dependence_info <- results$dependence_info

                # Calculate relative probabilities compared to prevalence
                rel_prob_t1 <- postest_prob_t1 / prevalence
                rel_prob_t2 <- postest_prob_t2 / prevalence
                rel_prob_both <- postest_prob_both / prevalence
                rel_prob_both_neg <- postest_prob_both_neg / prevalence

                # Update co-test results table using helper method
                private$.updateCotestResultsTable(postest_prob_t1, postest_prob_t2, postest_prob_both, postest_prob_both_neg,
                                                  rel_prob_t1, rel_prob_t2, rel_prob_both, rel_prob_both_neg,
                                                  postest_odds_t1, postest_odds_t2, postest_odds_both, postest_odds_both_neg)

                # Add footnotes if requested
                if (self$options$fnote) {
                    private$.addFootnotes()
                }

                # Update dependence info if tests are not independent
                if (!indep) {
                    self$results$dependenceInfo$setContent(dependence_info)
                }

                # Create enhanced explanation with clinical interpretation
                plr_interpretation <- private$.interpretPLR(if(indep) test1_plr * test2_plr else results$lr_both_pos)
                explanation <- sprintf(
                    "<p><strong>Clinical Interpretation:</strong></p>
                <p>Disease prevalence (pre-test probability): <strong>%.1f%%</strong></p>
                <p><strong>Both tests positive:</strong> %.1f%% probability (%.1fx increase) - %s</p>
                <p><strong>Both tests negative:</strong> %.1f%% probability (%.2fx change) %s</p>
                <p><strong>Single positive test:</strong></p>
                <ul>
                <li>Test 1 positive only: <strong>%.1f%%</strong> %s</li>
                <li>Test 2 positive only: <strong>%.1f%%</strong> %s</li>
                </ul>
                <div style='background-color: #f0f8ff; padding: 10px; border-radius: 5px; margin-top: 15px;'>
                <p><strong>Copy-ready summary:</strong></p>
                <p style='font-family: monospace; font-size: 12px;'>%s</p>
                </div>",
                    prevalence * 100,
                    postest_prob_both * 100, rel_prob_both, plr_interpretation,
                    postest_prob_both_neg * 100, rel_prob_both_neg,
                    private$.getClinicalSignificance(postest_prob_both_neg, prevalence),
                    postest_prob_t1 * 100, private$.getClinicalSignificance(postest_prob_t1, prevalence),
                    postest_prob_t2 * 100, private$.getClinicalSignificance(postest_prob_t2, prevalence),
                    private$.generateReportSentence(test1_sens, test1_spec, test2_sens, test2_spec,
                                                   prevalence, postest_prob_both, rel_prob_both,
                                                   postest_prob_both_neg, rel_prob_both_neg)
                )

                self$results$explanation$setContent(explanation)

                # Create dependency explanation
                self$results$dependenceExplanation$setContent(private$.buildDependenceExplanation())

                # Store data for Fagan nomogram if requested
                if (self$options$fagan) {
                    private$.prepareFaganPlotData(prevalence, test1_sens, test1_spec, test2_sens, test2_spec, 
                                                 indep, test1_plr, test1_nlr, test2_plr, test2_nlr, results)
                }
            },

            .plot1 = function(image1, ggtheme, ...) {
                plotData <- image1$state

                # Check cache to avoid expensive recalculations
                params_key <- paste(plotData$Prevalence, plotData$Plr_Both, plotData$Nlr_Both, sep="_")

                if (is.null(private$.lastNomogramParams) || private$.lastNomogramParams != params_key) {
                    private$.nomogramCache <- nomogrammer(
                        Prevalence = plotData$Prevalence,
                        Plr = plotData$Plr_Both,
                        Nlr = plotData$Nlr_Both,
                        Detail = TRUE,
                        NullLine = TRUE,
                        LabelSize = private$NOMOGRAM_LABEL_SIZE,
                        Verbose = TRUE
                    )
                    private$.lastNomogramParams <- params_key
                }

                print(private$.nomogramCache)
                TRUE
            },

            # Enhanced input validation with specific guidance
            .validateInputParameters = function(test1_sens, test1_spec, test2_sens, test2_spec, prevalence, indep, cond_dep_pos, cond_dep_neg) {
                if (test1_sens <= 0 || test1_sens >= 1) {
                    stop("Test 1 sensitivity must be between 0 and 1. Typical clinical values range from 0.60-0.95 for most diagnostic tests.")
                }
                if (test1_spec <= 0 || test1_spec >= 1) {
                    stop("Test 1 specificity must be between 0 and 1. Typical clinical values range from 0.70-0.98 for most diagnostic tests.")
                }
                if (test2_sens <= 0 || test2_sens >= 1) {
                    stop("Test 2 sensitivity must be between 0 and 1. Typical clinical values range from 0.60-0.95 for most diagnostic tests.")
                }
                if (test2_spec <= 0 || test2_spec >= 1) {
                    stop("Test 2 specificity must be between 0 and 1. Typical clinical values range from 0.70-0.98 for most diagnostic tests.")
                }
                if (prevalence <= 0 || prevalence >= 1) {
                    stop("Disease prevalence must be between 0 and 1. Consider realistic clinical prevalences: rare diseases (0.001-0.01), common conditions (0.05-0.20).")
                }
                if (!indep && (cond_dep_pos < 0 || cond_dep_pos > 1)) {
                    stop("Conditional dependence for positive cases must be between 0 and 1. Typical values: 0.05 (weak), 0.15 (moderate), 0.30 (strong dependence).")
                }
                if (!indep && (cond_dep_neg < 0 || cond_dep_neg > 1)) {
                    stop("Conditional dependence for negative cases must be between 0 and 1. Typical values: 0.05 (weak), 0.15 (moderate), 0.30 (strong dependence).")
                }
                
                # Additional clinical validity checks
                if (test1_sens + test1_spec < 1.1) {
                    warning("Test 1 has low discriminatory power (sensitivity + specificity < 1.1). Consider if this test adds clinical value.")
                }
                if (test2_sens + test2_spec < 1.1) {
                    warning("Test 2 has low discriminatory power (sensitivity + specificity < 1.1). Consider if this test adds clinical value.")
                }
                
                # Check for extreme prevalence that might cause numerical issues
                if (prevalence < 0.001) {
                    warning("Very low prevalence may lead to unstable results. Consider if co-testing is appropriate for such rare conditions.")
                }
                if (prevalence > 0.5) {
                    warning("High prevalence (>50%) - ensure this reflects your actual clinical population.")
                }
            },

            # Calculate likelihood ratios with numerical stability checks
            .calculateLikelihoodRatio = function(numerator, denominator, scenario_name) {
                if (abs(denominator) < private$NUMERICAL_TOLERANCE) {
                    warning(paste("Very small denominator in", scenario_name, "- results may be unstable. Consider adjusting test parameters."))
                    return(if (numerator > 0) 1e6 else 0)  # Large but finite value
                }
                
                result <- numerator / denominator
                
                # Check for extreme values that might indicate issues
                if (result > 1000) {
                    message(paste("Very high", scenario_name, "ratio (", round(result, 1), ") - indicates highly informative test."))
                }
                
                return(result)
            },

            # Clamp probabilities to valid ranges while providing informative warnings
            .clampProbability = function(value, lower, upper, context) {
                if (is.nan(value) || is.infinite(value)) {
                    stop(sprintf("%s resulted in a non-finite probability.", context))
                }

                adjusted <- value

                if (adjusted < lower - private$NUMERICAL_TOLERANCE) {
                    warning(sprintf(
                        "%s adjusted from %.6f to %.6f to satisfy joint probability bounds.",
                        context, adjusted, lower
                    ))
                    adjusted <- lower
                }

                if (adjusted > upper + private$NUMERICAL_TOLERANCE) {
                    warning(sprintf(
                        "%s adjusted from %.6f to %.6f to satisfy joint probability bounds.",
                        context, adjusted, upper
                    ))
                    adjusted <- upper
                }

                adjusted <- min(max(adjusted, lower), upper)
                return(adjusted)
            },

            # Confirm that joint probability cells sum to unity
            .validateJointDistribution = function(probabilities, label) {
                total <- Reduce(`+`, probabilities)
                if (abs(total - 1) > 1e-6) {
                    warning(sprintf(
                        "Joint probabilities for %s sum to %.6f (expected 1). Results may require review of dependence parameters.",
                        label, total
                    ))
                }
            },

            # Update test parameters table
            .updateTestParametersTable = function(test1_sens, test1_spec, test1_plr, test1_nlr, 
                                                  test2_sens, test2_spec, test2_plr, test2_nlr) {
                testParamsTable <- self$results$testParamsTable
                testParamsTable$setRow(
                    rowKey = "test1",
                    values = list(
                        test = "Test 1",
                        sens = test1_sens,
                        spec = test1_spec,
                        plr = test1_plr,
                        nlr = test1_nlr
                    )
                )

                testParamsTable$setRow(
                    rowKey = "test2",
                    values = list(
                        test = "Test 2",
                        sens = test2_sens,
                        spec = test2_spec,
                        plr = test2_plr,
                        nlr = test2_nlr
                    )
                )
            },

            # Calculate post-test probabilities for all scenarios
            .calculatePostTestProbabilities = function(test1_sens, test1_spec, test2_sens, test2_spec, 
                                                       prevalence, indep, cond_dep_pos, cond_dep_neg,
                                                       test1_plr, test1_nlr, test2_plr, test2_nlr) {
                # Convert prevalence to odds
                pretest_odds <- prevalence / (1 - prevalence)
                
                if (indep) {
                    # Independent tests: combine appropriate likelihood ratios for each outcome
                    lr_t1_only <- test1_plr * test2_nlr
                    lr_t2_only <- test1_nlr * test2_plr
                    lr_both_pos <- test1_plr * test2_plr
                    lr_both_neg <- test1_nlr * test2_nlr

                    postest_odds_t1 <- pretest_odds * lr_t1_only
                    postest_prob_t1 <- postest_odds_t1 / (1 + postest_odds_t1)

                    postest_odds_t2 <- pretest_odds * lr_t2_only
                    postest_prob_t2 <- postest_odds_t2 / (1 + postest_odds_t2)

                    postest_odds_both <- pretest_odds * lr_both_pos
                    postest_prob_both <- postest_odds_both / (1 + postest_odds_both)

                    postest_odds_both_neg <- pretest_odds * lr_both_neg
                    postest_prob_both_neg <- postest_odds_both_neg / (1 + postest_odds_both_neg)

                    dependence_info <- "<p>Tests are assumed to be conditionally independent.</p>"
                } else {
                    # Dependent tests scenario with numerical stability
                    dep_results <- private$.calculateDependentTestProbabilities(test1_sens, test1_spec, test2_sens, test2_spec,
                                                                                cond_dep_pos, cond_dep_neg, pretest_odds)
                    
                    postest_prob_t1 <- dep_results$postest_prob_t1
                    postest_prob_t2 <- dep_results$postest_prob_t2
                    postest_prob_both <- dep_results$postest_prob_both
                    postest_prob_both_neg <- dep_results$postest_prob_both_neg
                    postest_odds_t1 <- dep_results$postest_odds_t1
                    postest_odds_t2 <- dep_results$postest_odds_t2
                    postest_odds_both <- dep_results$postest_odds_both
                    postest_odds_both_neg <- dep_results$postest_odds_both_neg
                    dependence_info <- dep_results$dependence_info
                }
                
                # Store LRs for plot data (set to NA for independent case)
                if (indep) {
                    lr_both_pos <- test1_plr * test2_plr
                    lr_both_neg <- test1_nlr * test2_nlr
                } else {
                    lr_both_pos <- dep_results$lr_both_pos
                    lr_both_neg <- dep_results$lr_both_neg
                }
                
                return(list(
                    postest_prob_t1 = postest_prob_t1,
                    postest_prob_t2 = postest_prob_t2,
                    postest_prob_both = postest_prob_both,
                    postest_prob_both_neg = postest_prob_both_neg,
                    postest_odds_t1 = postest_odds_t1,
                    postest_odds_t2 = postest_odds_t2,
                    postest_odds_both = postest_odds_both,
                    postest_odds_both_neg = postest_odds_both_neg,
                    dependence_info = dependence_info,
                    lr_both_pos = lr_both_pos,
                    lr_both_neg = lr_both_neg
                ))
            },

            # Calculate probabilities for dependent tests
            .calculateDependentTestProbabilities = function(test1_sens, test1_spec, test2_sens, test2_spec,
                                                            cond_dep_pos, cond_dep_neg, pretest_odds) {
                # Helper values for marginal probabilities
                fp_test1 <- 1 - test1_spec
                fp_test2 <- 1 - test2_spec

                # Probability of both tests positive given disease present
                p_both_pos_D_raw <- (test1_sens * test2_sens) + (cond_dep_pos * sqrt(
                    test1_sens * (1 - test1_sens) * test2_sens * (1 - test2_sens)
                ))
                lower_pos_D <- max(0, test1_sens + test2_sens - 1)
                upper_pos_D <- min(test1_sens, test2_sens)
                p_both_pos_D <- private$.clampProbability(p_both_pos_D_raw, lower_pos_D, upper_pos_D,
                                                          "P(Test1+, Test2+ | Disease+)")

                p_t1_only_D <- private$.clampProbability(test1_sens - p_both_pos_D, 0, test1_sens,
                                                         "P(Test1+, Test2- | Disease+)")
                p_t2_only_D <- private$.clampProbability(test2_sens - p_both_pos_D, 0, test2_sens,
                                                         "P(Test1-, Test2+ | Disease+)")
                p_both_neg_D <- 1 - (p_both_pos_D + p_t1_only_D + p_t2_only_D)

                # Probability of both tests positive given disease absent (false positives)
                p_both_pos_nD_raw <- (fp_test1 * fp_test2) + (cond_dep_neg * sqrt(
                    fp_test1 * (1 - fp_test1) * fp_test2 * (1 - fp_test2)
                ))
                lower_pos_nD <- max(0, fp_test1 + fp_test2 - 1)
                upper_pos_nD <- min(fp_test1, fp_test2)
                p_both_pos_nD <- private$.clampProbability(p_both_pos_nD_raw, lower_pos_nD, upper_pos_nD,
                                                          "P(Test1+, Test2+ | Disease-)")

                p_t1_only_nD <- private$.clampProbability(fp_test1 - p_both_pos_nD, 0, fp_test1,
                                                          "P(Test1+, Test2- | Disease-)")
                p_t2_only_nD <- private$.clampProbability(fp_test2 - p_both_pos_nD, 0, fp_test2,
                                                          "P(Test1-, Test2+ | Disease-)")
                p_both_neg_nD <- 1 - (p_both_pos_nD + p_t1_only_nD + p_t2_only_nD)

                # Ensure probability sets sum to 1 within tolerance
                private$.validateJointDistribution(
                    list(p_both_pos_D, p_t1_only_D, p_t2_only_D, p_both_neg_D),
                    "Disease+"
                )
                private$.validateJointDistribution(
                    list(p_both_pos_nD, p_t1_only_nD, p_t2_only_nD, p_both_neg_nD),
                    "Disease-"
                )

                # Calculate likelihood ratios with stability checks
                lr_t1_only <- private$.calculateLikelihoodRatio(p_t1_only_D, p_t1_only_nD, "Test 1 Only LR")
                lr_t2_only <- private$.calculateLikelihoodRatio(p_t2_only_D, p_t2_only_nD, "Test 2 Only LR")
                lr_both_pos <- private$.calculateLikelihoodRatio(p_both_pos_D, p_both_pos_nD, "Both Positive LR")
                lr_both_neg <- private$.calculateLikelihoodRatio(p_both_neg_D, p_both_neg_nD, "Both Negative LR")

                # Calculate post-test odds and probabilities
                postest_odds_t1 <- pretest_odds * lr_t1_only
                postest_prob_t1 <- postest_odds_t1 / (1 + postest_odds_t1)

                postest_odds_t2 <- pretest_odds * lr_t2_only
                postest_prob_t2 <- postest_odds_t2 / (1 + postest_odds_t2)

                postest_odds_both <- pretest_odds * lr_both_pos
                postest_prob_both <- postest_odds_both / (1 + postest_odds_both)

                postest_odds_both_neg <- pretest_odds * lr_both_neg
                postest_prob_both_neg <- postest_odds_both_neg / (1 + postest_odds_both_neg)

                dependence_info <- sprintf(
                    "<p>Tests are modeled with conditional dependence:<br>
                Dependence for subjects with disease: %.2f<br>
                Dependence for subjects without disease: %.2f</p>
                <p>Joint probabilities after accounting for dependence:<br>
                P(Test1+,Test2+ | Disease+): %.4f<br>
                P(Test1+,Test2- | Disease+): %.4f<br>
                P(Test1-,Test2+ | Disease+): %.4f<br>
                P(Test1-,Test2- | Disease+): %.4f<br>
                P(Test1+,Test2+ | Disease-): %.4f<br>
                P(Test1+,Test2- | Disease-): %.4f<br>
                P(Test1-,Test2+ | Disease-): %.4f<br>
                P(Test1-,Test2- | Disease-): %.4f</p>",
                    cond_dep_pos, cond_dep_neg,
                    p_both_pos_D, p_t1_only_D, p_t2_only_D, p_both_neg_D,
                    p_both_pos_nD, p_t1_only_nD, p_t2_only_nD, p_both_neg_nD
                )
                
                return(list(
                    postest_prob_t1 = postest_prob_t1,
                    postest_prob_t2 = postest_prob_t2,
                    postest_prob_both = postest_prob_both,
                    postest_prob_both_neg = postest_prob_both_neg,
                    postest_odds_t1 = postest_odds_t1,
                    postest_odds_t2 = postest_odds_t2,
                    postest_odds_both = postest_odds_both,
                    postest_odds_both_neg = postest_odds_both_neg,
                    dependence_info = dependence_info,
                    lr_both_pos = lr_both_pos,
                    lr_both_neg = lr_both_neg
                ))
            },

            # Update co-test results table with reduced duplication
            .updateCotestResultsTable = function(postest_prob_t1, postest_prob_t2, postest_prob_both, postest_prob_both_neg,
                                                 rel_prob_t1, rel_prob_t2, rel_prob_both, rel_prob_both_neg,
                                                 postest_odds_t1, postest_odds_t2, postest_odds_both, postest_odds_both_neg) {
                cotestResultsTable <- self$results$cotestResultsTable

                # Define scenarios with their data
                scenarios <- list(
                    list(key = "test1_pos", scenario = "Test 1 Positive Only", postProb = postest_prob_t1, 
                         relativeProbability = rel_prob_t1, orValue = postest_odds_t1),
                    list(key = "test2_pos", scenario = "Test 2 Positive Only", postProb = postest_prob_t2, 
                         relativeProbability = rel_prob_t2, orValue = postest_odds_t2),
                    list(key = "both_pos", scenario = "Both Tests Positive", postProb = postest_prob_both, 
                         relativeProbability = rel_prob_both, orValue = postest_odds_both),
                    list(key = "both_neg", scenario = "Both Tests Negative", postProb = postest_prob_both_neg, 
                         relativeProbability = rel_prob_both_neg, orValue = postest_odds_both_neg)
                )

                # Update all rows using loop to reduce duplication
                for (scenario in scenarios) {
                    cotestResultsTable$setRow(
                        rowKey = scenario$key,
                        values = list(
                            scenario = scenario$scenario,
                            postProb = scenario$postProb,
                            relativeProbability = scenario$relativeProbability,
                            orValue = scenario$orValue
                        )
                    )
                }
            },

            # Add footnotes to tables
            .addFootnotes = function() {
                testParamsTable <- self$results$testParamsTable
                cotestResultsTable <- self$results$cotestResultsTable
                
                # Test parameters footnotes
                testParamsTable$addFootnote(
                    rowKey = "test1", col = "sens",
                    "Proportion of diseased patients correctly identified by Test 1"
                )
                testParamsTable$addFootnote(
                    rowKey = "test1", col = "spec",
                    "Proportion of non-diseased patients correctly identified by Test 1"
                )
                testParamsTable$addFootnote(
                    rowKey = "test1", col = "plr",
                    "Positive Likelihood Ratio: how much more likely a positive result is in diseased vs. non-diseased patients"
                )
                testParamsTable$addFootnote(
                    rowKey = "test1", col = "nlr",
                    "Negative Likelihood Ratio: how much more likely a negative result is in diseased vs. non-diseased patients"
                )
                testParamsTable$addFootnote(
                    rowKey = "test2", col = "sens",
                    "Proportion of diseased patients correctly identified by Test 2"
                )
                testParamsTable$addFootnote(
                    rowKey = "test2", col = "spec",
                    "Proportion of non-diseased patients correctly identified by Test 2"
                )
                testParamsTable$addFootnote(
                    rowKey = "test2", col = "plr",
                    "Positive Likelihood Ratio: how much more likely a positive result is in diseased vs. non-diseased patients"
                )
                testParamsTable$addFootnote(
                    rowKey = "test2", col = "nlr",
                    "Negative Likelihood Ratio: how much more likely a negative result is in diseased vs. non-diseased patients"
                )

                # Results footnotes
                for (row_key in c("test1_pos", "test2_pos", "both_pos", "both_neg")) {
                    cotestResultsTable$addFootnote(
                        rowKey = row_key, col = "postProb",
                        "Probability of disease after obtaining this test result combination"
                    )
                    cotestResultsTable$addFootnote(
                        rowKey = row_key, col = "relativeProbability",
                        "How many times more (or less) likely disease is after testing compared to before testing"
                    )
                }
            },

            # Prepare Fagan nomogram plot data
            .prepareFaganPlotData = function(prevalence, test1_sens, test1_spec, test2_sens, test2_spec, 
                                            indep, test1_plr, test1_nlr, test2_plr, test2_nlr, results) {
                # Checkpoint before potentially expensive nomogram calculation
                private$.checkpoint()
                
                plotData <- list(
                    "Prevalence" = prevalence,
                    "Test1Sens" = test1_sens,
                    "Test1Spec" = test1_spec,
                    "Test2Sens" = test2_sens,
                    "Test2Spec" = test2_spec,
                    "Plr_Both" = if (indep) test1_plr * test2_plr else results$lr_both_pos,
                    "Nlr_Both" = if (indep) test1_nlr * test2_nlr else results$lr_both_neg
                )

                image1 <- self$results$plot1
                image1$setState(plotData)
            },

            # Clinical interpretation helpers
            .interpretPLR = function(plr) {
                if (plr > 10) return("strong evidence for disease")
                if (plr > 5) return("moderate evidence for disease")
                if (plr > 2) return("weak evidence for disease")
                if (plr > 1) return("minimal evidence for disease")
                return("no diagnostic value")
            },

            .getClinicalSignificance = function(post_prob, prevalence) {
                change_factor <- post_prob / prevalence
                if (change_factor > 3) return("(major increase)")
                if (change_factor > 1.5) return("(moderate increase)")
                if (change_factor > 1.1) return("(slight increase)")
                if (change_factor < 0.5) return("(major decrease)")
                if (change_factor < 0.8) return("(moderate decrease)")
                return("(minimal change)")
            },

            .generateReportSentence = function(test1_sens, test1_spec, test2_sens, test2_spec,
                                               prevalence, postest_prob_both, rel_prob_both,
                                               postest_prob_both_neg, rel_prob_both_neg) {
                sprintf(
                    "Co-testing with Test 1 (sensitivity %.0f%%, specificity %.0f%%) and Test 2 (sensitivity %.0f%%, specificity %.0f%%) in a population with %.1f%% disease prevalence showed: when both tests are positive, disease probability is %.1f%% (%.1fx increase); when both are negative, disease probability is %.1f%% (%.2fx decrease).",
                    test1_sens * 100, test1_spec * 100,
                    test2_sens * 100, test2_spec * 100,
                    prevalence * 100,
                    postest_prob_both * 100, rel_prob_both,
                    postest_prob_both_neg * 100, rel_prob_both_neg
                )
            },

            # Helper method to build dependence explanation content
            .buildDependenceExplanation = function() {
                explanation <- '
<div style="max-width: 800px;">
<h3>Understanding Test Dependence in Diagnostic Testing</h3>

<h4>What is conditional independence vs. dependence?</h4>
<p>Two diagnostic tests are <strong>conditionally independent</strong> if the result of one test does not influence the result of the other test, <em>given the disease status</em>. In other words, within the diseased population, the probability of Test 1 being positive is not affected by knowing the result of Test 2, and vice versa. The same applies within the non-diseased population.</p>

<p>Tests are <strong>conditionally dependent</strong> when the result of one test affects the probability of the other test result, even when we know the patient\'s true disease status.</p>

<h4>Mathematical Formulation</h4>

<p><strong>Independent Tests:</strong> When tests are independent, joint probabilities are simply the product of individual probabilities:</p>
<ul>
  <li>P(Test1+ and Test2+ | Disease+) = P(Test1+ | Disease+) × P(Test2+ | Disease+) = Sens₁ × Sens₂</li>
  <li>P(Test1+ and Test2+ | Disease−) = P(Test1+ | Disease−) × P(Test2+ | Disease−) = (1−Spec₁) × (1−Spec₂)</li>
  <li>P(Test1− and Test2− | Disease+) = P(Test1− | Disease+) × P(Test2− | Disease+) = (1−Sens₁) × (1−Sens₂)</li>
  <li>P(Test1− and Test2− | Disease−) = P(Test1− | Disease−) × P(Test2− | Disease−) = Spec₁ × Spec₂</li>
</ul>

<p><strong>Dependent Tests:</strong> When tests are dependent, we adjust these probabilities using a correlation parameter (denoted as ρ or ψ) that ranges from 0 (independence) to 1 (maximum possible dependence):</p>
<ul>
  <li>P(Test1+ and Test2+ | Disease+) = (Sens₁ × Sens₂) + ρᵨₒₛ × √(Sens₁ × (1−Sens₁) × Sens₂ × (1−Sens₂))</li>
  <li>P(Test1+ and Test2+ | Disease−) = ((1−Spec₁) × (1−Spec₂)) + ρₙₑ𝑔 × √((1−Spec₁) × Spec₁ × (1−Spec₂) × Spec₂)</li>
</ul>

<p>Note: Similar adjustments are made for the other joint probabilities.</p>

<h4>When to Use Dependent vs. Independent Models</h4>

<p><strong>Use the independence model when:</strong></p>
<ul>
  <li>Tests measure completely different biological phenomena</li>
  <li>Tests use different biological specimens or mechanisms</li>
  <li>You have no evidence of correlation between test results</li>
  <li>You have limited information about how the tests interact</li>
</ul>

<p><strong>Use the dependence model when:</strong></p>
<ul>
  <li>Tests measure the same or similar biological phenomena</li>
  <li>Tests are based on the same biological specimen or mechanism</li>
  <li>Previous studies indicate correlation between test results</li>
  <li>Both tests are affected by the same confounding factors</li>
  <li>You have observed that knowing one test result predicts the other</li>
</ul>

<h4>Real-World Examples of Dependent Tests</h4>
<ul>
  <li>Two imaging tests (e.g., MRI and CT) looking at the same anatomical structure</li>
  <li>Two serological tests that detect different antibodies but against the same pathogen</li>
  <li>Tests that may both be affected by the same confounding factor (e.g., inflammation)</li>
  <li>Multiple readings of the same test by different observers</li>
  <li>Two different molecular tests detecting different genes of the same pathogen</li>
</ul>

<h4>Estimating Dependency Parameters</h4>
<p>The conditional dependence parameters (ρᵨₒₛ for diseased subjects and ρₙₑ𝑔 for non-diseased subjects) ideally should be estimated from paired testing data with known disease status. Values typically range from 0 to 0.5 in practice, with higher values indicating stronger dependence. When no data is available, sensitivity analyses using a range of plausible values (e.g., 0.05, 0.1, 0.2) can reveal how much dependence affects results.</p>

<h4>Impact of Ignoring Dependence</h4>
<p>Ignoring conditional dependence when it exists tends to:</p>
<ul>
  <li>Overestimate the benefit of combined testing</li>
  <li>Exaggerate post-test probabilities (either too high for positive results or too low for negative results)</li>
  <li>Produce unrealistically narrow confidence intervals</li>
  <li>Lead to overly optimistic assessment of diagnostic accuracy</li>
</ul>
</div>'
                
                return(explanation)
            }
        )
    )
