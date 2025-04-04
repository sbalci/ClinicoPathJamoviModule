#' @title Co-Testing Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'

cotestClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "cotestClass",
        inherit = cotestBase,
        private = list(
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

                # Calculate likelihood ratios
                test1_plr <- test1_sens / (1 - test1_spec)  # Positive likelihood ratio
                test1_nlr <- (1 - test1_sens) / test1_spec  # Negative likelihood ratio

                test2_plr <- test2_sens / (1 - test2_spec)  # Positive likelihood ratio
                test2_nlr <- (1 - test2_sens) / test2_spec  # Negative likelihood ratio

                # Update test parameters table
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

                # Convert prevalence to odds
                pretest_odds <- prevalence / (1 - prevalence)

                # Calculate post-test probabilities for different scenarios
                if (indep) {
                    # Independent tests scenario

                    # Test 1 Positive Only
                    postest_odds_t1 <- pretest_odds * test1_plr
                    postest_prob_t1 <- postest_odds_t1 / (1 + postest_odds_t1)

                    # Test 2 Positive Only
                    postest_odds_t2 <- pretest_odds * test2_plr
                    postest_prob_t2 <- postest_odds_t2 / (1 + postest_odds_t2)

                    # Both Tests Positive
                    postest_odds_both <- pretest_odds * test1_plr * test2_plr
                    postest_prob_both <- postest_odds_both / (1 + postest_odds_both)

                    # Both Tests Negative
                    postest_odds_both_neg <- pretest_odds * test1_nlr * test2_nlr
                    postest_prob_both_neg <- postest_odds_both_neg / (1 + postest_odds_both_neg)

                    dependence_info <- "<p>Tests are assumed to be conditionally independent.</p>"
                } else {
                    # Dependent tests scenario
                    # Using the conditional dependence parameters to adjust joint probabilities

                    # Probability of both tests positive for diseased subjects (with dependence)
                    p_both_pos_D <- (test1_sens * test2_sens) + (cond_dep_pos * sqrt(
                        test1_sens * (1 - test1_sens) * test2_sens * (1 - test2_sens)
                    ))

                    # Probability of both tests positive for non-diseased subjects (with dependence)
                    p_both_pos_nD <- ((1 - test1_spec) * (1 - test2_spec)) + (cond_dep_neg * sqrt((1 - test1_spec) * test1_spec * (1 - test2_spec) * test2_spec
                    ))

                    # Probability of both tests negative for diseased subjects (with dependence)
                    p_both_neg_D <- ((1 - test1_sens) * (1 - test2_sens)) + (cond_dep_pos * sqrt((1 - test1_sens) * test1_sens * (1 - test2_sens) * test2_sens
                    ))

                    # Probability of both tests negative for non-diseased subjects (with dependence)
                    p_both_neg_nD <- (test1_spec * test2_spec) + (cond_dep_neg * sqrt(
                        test1_spec * (1 - test1_spec) * test2_spec * (1 - test2_spec)
                    ))

                    # Probability of test1 positive only for diseased subjects
                    p_t1_only_D <- test1_sens - p_both_pos_D

                    # Probability of test1 positive only for non-diseased subjects
                    p_t1_only_nD <- (1 - test1_spec) - p_both_pos_nD

                    # Probability of test2 positive only for diseased subjects
                    p_t2_only_D <- test2_sens - p_both_pos_D

                    # Probability of test2 positive only for non-diseased subjects
                    p_t2_only_nD <- (1 - test2_spec) - p_both_pos_nD

                    # Calculate likelihood ratios for each scenario
                    lr_t1_only <- p_t1_only_D / p_t1_only_nD
                    lr_t2_only <- p_t2_only_D / p_t2_only_nD
                    lr_both_pos <- p_both_pos_D / p_both_pos_nD
                    lr_both_neg <- p_both_neg_D / p_both_neg_nD

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
                    P(Test1+,Test2+ | Disease-): %.4f<br>
                    P(Test1-,Test2- | Disease+): %.4f<br>
                    P(Test1-,Test2- | Disease-): %.4f</p>",
                        cond_dep_pos,
                        cond_dep_neg,
                        p_both_pos_D,
                        p_both_pos_nD,
                        p_both_neg_D,
                        p_both_neg_nD
                    )
                }

                # Calculate relative probabilities compared to prevalence
                rel_prob_t1 <- postest_prob_t1 / prevalence
                rel_prob_t2 <- postest_prob_t2 / prevalence
                rel_prob_both <- postest_prob_both / prevalence
                rel_prob_both_neg <- postest_prob_both_neg / prevalence

                # Update co-test results table
                cotestResultsTable <- self$results$cotestResultsTable

                cotestResultsTable$setRow(
                    rowKey = "test1_pos",
                    values = list(
                        scenario = "Test 1 Positive Only",
                        postProb = postest_prob_t1,
                        relativeProbability = rel_prob_t1,
                        orValue = postest_odds_t1
                    )
                )

                cotestResultsTable$setRow(
                    rowKey = "test2_pos",
                    values = list(
                        scenario = "Test 2 Positive Only",
                        postProb = postest_prob_t2,
                        relativeProbability = rel_prob_t2,
                        orValue = postest_odds_t2
                    )
                )

                cotestResultsTable$setRow(
                    rowKey = "both_pos",
                    values = list(
                        scenario = "Both Tests Positive",
                        postProb = postest_prob_both,
                        relativeProbability = rel_prob_both,
                        orValue = postest_odds_both
                    )
                )

                cotestResultsTable$setRow(
                    rowKey = "both_neg",
                    values = list(
                        scenario = "Both Tests Negative",
                        postProb = postest_prob_both_neg,
                        relativeProbability = rel_prob_both_neg,
                        orValue = postest_odds_both_neg
                    )
                )

                # Add footnotes if requested
                if (self$options$fnote) {
                    testParamsTable$addFootnote(
                        rowKey = "test1",
                        col = "sens",
                        "Proportion of diseased patients correctly identified by Test 1"
                    )
                    testParamsTable$addFootnote(
                        rowKey = "test1",
                        col = "spec",
                        "Proportion of non-diseased patients correctly identified by Test 1"
                    )
                    testParamsTable$addFootnote(
                        rowKey = "test1",
                        col = "plr",
                        "Positive Likelihood Ratio: how much more likely a positive result is in diseased vs. non-diseased patients"
                    )
                    testParamsTable$addFootnote(
                        rowKey = "test1",
                        col = "nlr",
                        "Negative Likelihood Ratio: how much more likely a negative result is in diseased vs. non-diseased patients"
                    )

                    cotestResultsTable$addFootnote(
                        rowKey = "both_pos",
                        col = "postProb",
                        "Probability of disease after obtaining this test result combination"
                    )
                    cotestResultsTable$addFootnote(
                        rowKey = "both_pos",
                        col = "relativeProbability",
                        "How many times more (or less) likely disease is after testing compared to before testing"
                    )
                }

                # Update dependence info if tests are not independent
                if (!indep) {
                    self$results$dependenceInfo$setContent(dependence_info)
                }

                # Create explanation text
                explanation <- sprintf(
                    "<p><strong>Interpretation:</strong></p>
                <p>The disease prevalence (pre-test probability) in this population is <strong>%.1f%%</strong>.</p>
                <p>If both tests are positive, the probability of disease increases to <strong>%.1f%%</strong>
                (%.1f times the pre-test probability).</p>
                <p>If both tests are negative, the probability of disease decreases to <strong>%.1f%%</strong>
                (%.2f times the pre-test probability).</p>
                <p>When only one test is positive, the post-test probabilities are:
                <ul>
                <li>Test 1 positive only: <strong>%.1f%%</strong></li>
                <li>Test 2 positive only: <strong>%.1f%%</strong></li>
                </ul></p>",
                    prevalence * 100,
                    postest_prob_both * 100,
                    rel_prob_both,
                    postest_prob_both_neg * 100,
                    rel_prob_both_neg,
                    postest_prob_t1 * 100,
                    postest_prob_t2 * 100
                )

                self$results$explanation$setContent(explanation)

                # Create dependency explanation
                dependence_explanation <- '
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
</div>
            '

                self$results$dependenceExplanation$setContent(dependence_explanation)

                # Store data for Fagan nomogram if requested
                if (self$options$fagan) {
                    plotData <- list(
                        "Prevalence" = prevalence,
                        "Test1Sens" = test1_sens,
                        "Test1Spec" = test1_spec,
                        "Test2Sens" = test2_sens,
                        "Test2Spec" = test2_spec,
                        "Plr_Both" = if (indep)
                            test1_plr * test2_plr
                        else
                            lr_both_pos,
                        "Nlr_Both" = if (indep)
                            test1_nlr * test2_nlr
                        else
                            lr_both_neg
                    )

                    image1 <- self$results$plot1
                    image1$setState(plotData)
                }
            },

            .plot1 = function(image1, ggtheme, ...) {
                plotData <- image1$state

                plot1 <- nomogrammer(
                    Prevalence = plotData$Prevalence,
                    Plr = plotData$Plr_Both,
                    Nlr = plotData$Nlr_Both,
                    Detail = TRUE,
                    NullLine = TRUE,
                    LabelSize = (14 /
                                     5),
                    Verbose = TRUE
                )

                print(plot1)
                TRUE
            }
        )
    )
