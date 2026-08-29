# ═══════════════════════════════════════════════════════════
# Example Usage: decisioncombine
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates comprehensive clinical applications of the
# decisioncombine function for systematic evaluation of diagnostic test combinations.
#
# The decisioncombine function provides:
# - Systematic evaluation of all test result patterns (2-test: 4 patterns, 3-test: 8 patterns)
# - Performance metrics for each pattern (sensitivity, specificity, PPV, NPV, accuracy)
# - Individual test statistics for comparison
# - Pattern-type filtering (all-positive, all-negative, or mixed exact patterns)
# - Metric filtering for visualizations
# - Multiple visualizations (bar plot, heatmap, forest plot, decision space)
# - A sample-dependent, descriptive ranking of eligible candidate rules

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Two-Test Combination Analysis
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Evaluate pathology rater agreement and combined performance

data(decisioncombine_pathology, package = "ClinicoPath")

basic_combination <- decisioncombine(
  data = decisioncombine_pathology,
  gold = "gold_standard",
  goldPositive = "Malignant",
  test1 = "rater1",
  test1Positive = "Positive",
  test2 = "rater2",
  test2Positive = "Positive"
)

# Interpretation:
# - Four patterns evaluated: +/+, +/-, -/+, -/-
# - Each exact-pattern row treats occurrence of that pattern as a positive classification
# - The named Parallel and Serial rows implement OR and AND rules directly
# - Compare estimates and uncertainty; do not assume a pattern is best from its label alone

# ═══════════════════════════════════════════════════════════
# Example 2: Two-Test Analysis with Individual Statistics
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Show individual test performance alongside combinations

individual_stats <- decisioncombine(
  data = decisioncombine_pathology,
  gold = "gold_standard",
  goldPositive = "Malignant",
  test1 = "rater1",
  test1Positive = "Positive",
  test2 = "rater2",
  test2Positive = "Positive",
  showIndividual = TRUE,
  showFrequency = TRUE
)

# Interpretation:
# - Individual test statistics provide baseline performance
# - Combination rules can be compared descriptively with individual tests
# - Frequency tables show distribution of pattern occurrences
# - Compare individual vs combined performance to justify multi-test strategy

# ═══════════════════════════════════════════════════════════
# Example 3: Three-Test Combination (8 Patterns)
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Combine clinical exam, lab test, and imaging

data(decisioncombine_threetest, package = "ClinicoPath")

threetest_combination <- decisioncombine(
  data = decisioncombine_threetest,
  gold = "gold_standard",
  goldPositive = "Disease",
  test1 = "clinical_exam",
  test1Positive = "Positive",
  test2 = "lab_test",
  test2Positive = "Positive",
  test3 = "imaging",
  test3Positive = "Positive",
  showIndividual = TRUE
)

# Interpretation:
# - Eight patterns: +/+/+, +/+/-, +/-/+, +/-/-, -/+/+, -/+/-, -/-/+, -/-/-
# - Each exact-pattern row is evaluated as a one-versus-rest classification rule
# - Named Parallel, Serial, and Majority rows represent clinically recognizable rules
# - Performance depends on the observed data and requires uncertainty-aware interpretation

# ═══════════════════════════════════════════════════════════
# Example 4: Parallel Testing Strategy (Either Test Positive)
# ═══════════════════════════════════════════════════════════
# Clinical scenario: High-sensitivity screening (call positive if either test positive)

data(decisioncombine_discordant, package = "ClinicoPath")

parallel_strategy <- decisioncombine(
  data = decisioncombine_discordant,
  gold = "gold_standard",
  goldPositive = "Positive",
  test1 = "sensitive_test",
  test1Positive = "Positive",
  test2 = "specific_test",
  test2Positive = "Positive",
  showIndividual = TRUE,
  showBarPlot = TRUE
)

# Interpretation:
# - Parallel = Test1+ OR Test2+ (either positive → call positive)
# - Its sensitivity is at least that of each component test in the same complete cases
# - Its specificity is no greater than that of each component test
# - Inspect the named Parallel row; the pattern filter does not select strategies
# - Combines +/+, +/-, and -/+ patterns

# ═══════════════════════════════════════════════════════════
# Example 5: Serial Testing Strategy (Both Tests Positive)
# ═══════════════════════════════════════════════════════════
# Clinical scenario: High-specificity confirmation (require both tests positive)

data(decisioncombine_screening, package = "ClinicoPath")

serial_strategy <- decisioncombine(
  data = decisioncombine_screening,
  gold = "gold_standard",
  goldPositive = "Disease",
  test1 = "screening_test",
  test1Positive = "Positive",
  test2 = "confirmatory_test",
  test2Positive = "Positive",
  showIndividual = TRUE,
  showBarPlot = TRUE,
  showRecommendation = TRUE
)

# Interpretation:
# - Serial = Test1+ AND Test2+ (both must be positive → call positive)
# - Its specificity is at least that of each component test in the same complete cases
# - Its sensitivity is no greater than that of each component test
# - Inspect the named Serial row; clinical use requires context beyond these estimates
# - Only +/+ pattern considered positive

# ═══════════════════════════════════════════════════════════
# Example 6: Majority Rule Strategy (Three Tests)
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Consensus decision from three independent tests

majority_rule <- decisioncombine(
  data = decisioncombine_threetest,
  gold = "gold_standard",
  goldPositive = "Disease",
  test1 = "clinical_exam",
  test1Positive = "Positive",
  test2 = "lab_test",
  test2Positive = "Positive",
  test3 = "imaging",
  test3Positive = "Positive",
  showIndividual = TRUE,
  showBarPlot = TRUE,
  showRecommendation = TRUE
)

# Interpretation:
# - Majority rule: >=2 of 3 tests positive → call positive
# - Produces a middle decision threshold between Serial and Parallel rules
# - Patterns: +/+/+, +/+/-, +/-/+, -/+/+ are positive
# - Patterns: +/-/-, -/+/-, -/-/+, -/-/- are negative
# - Whether that threshold is useful depends on the tests, population, and consequences

# ═══════════════════════════════════════════════════════════
# Example 7: Display Sensitivity in the Plots
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Compare sensitivity estimates across candidate rules

filter_sensitivity <- decisioncombine(
  data = decisioncombine_pathology,
  gold = "gold_standard",
  goldPositive = "Malignant",
  test1 = "rater1",
  test1Positive = "Positive",
  test2 = "rater2",
  test2Positive = "Positive",
  filterStatistic = "sens",
  showIndividual = TRUE,
  showBarPlot = TRUE
)

# Interpretation:
# - Restricts supported plots to the sensitivity metric
# - It does not select or recommend the row with the largest sensitivity
# - Interpret estimates with their denominators and confidence intervals

# ═══════════════════════════════════════════════════════════
# Example 8: Display Specificity in the Plots
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Compare specificity estimates across candidate rules

filter_specificity <- decisioncombine(
  data = decisioncombine_pathology,
  gold = "gold_standard",
  goldPositive = "Malignant",
  test1 = "rater1",
  test1Positive = "Positive",
  test2 = "rater2",
  test2Positive = "Positive",
  filterStatistic = "spec",
  showIndividual = TRUE,
  showBarPlot = TRUE
)

# Interpretation:
# - Restricts supported plots to the specificity metric
# - It does not select or recommend the row with the largest specificity
# - Interpret estimates with their denominators and confidence intervals

# ═══════════════════════════════════════════════════════════
# Example 9: Display Youden's J in Supported Plots
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Compare observed sensitivity-specificity summaries

filter_youden <- decisioncombine(
  data = decisioncombine_pathology,
  gold = "gold_standard",
  goldPositive = "Malignant",
  test1 = "rater1",
  test1Positive = "Positive",
  test2 = "rater2",
  test2Positive = "Positive",
  filterStatistic = "youden",
  showIndividual = TRUE,
  showBarPlot = TRUE,
  showRecommendation = TRUE
)

# Interpretation:
# - Youden's J = Sensitivity + Specificity - 1
# - The filter displays Youden's J; it does not select a rule
# - The ranking table reports the largest eligible observed value descriptively
# - No significance test or multiplicity correction establishes a superior rule
# - The ranking is not clinical guidance or a validated recommendation

# ═══════════════════════════════════════════════════════════
# Example 10: Comprehensive Visualization (All Plot Types)
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Visual comparison of all patterns

comprehensive_viz <- decisioncombine(
  data = decisioncombine_pathology,
  gold = "gold_standard",
  goldPositive = "Malignant",
  test1 = "rater1",
  test1Positive = "Positive",
  test2 = "rater2",
  test2Positive = "Positive",
  showIndividual = TRUE,
  showBarPlot = TRUE,
  showHeatmap = TRUE,
  showForest = TRUE,
  showDecisionTree = TRUE
)

# Interpretation:
# - Bar plot: Compare metrics across patterns
# - Heatmap: Visual pattern of performance (color-coded)
# - Forest plot: Metrics with confidence intervals
# - Decision space: Sensitivity-versus-specificity positions
# - Multiple visualizations provide complementary insights

# ═══════════════════════════════════════════════════════════
# Example 11: Concordant Tests (High Agreement)
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Evaluate tests that usually agree

data(decisioncombine_concordant, package = "ClinicoPath")

concordant_tests <- decisioncombine(
  data = decisioncombine_concordant,
  gold = "gold_standard",
  goldPositive = "Disease Present",
  test1 = "test_a",
  test1Positive = "Positive",
  test2 = "test_b",
  test2Positive = "Positive",
  showIndividual = TRUE,
  showFrequency = TRUE,
  showBarPlot = TRUE,
  showRecommendation = TRUE
)

# Interpretation:
# - High concordance: most results are +/+ or -/-
# - Few discordant results (+/- or -/+)
# - When tests agree, confidence is high
# - Discordant cases may warrant additional investigation
# - Frequency table shows distribution of agreement patterns

# ═══════════════════════════════════════════════════════════
# Example 12: Discordant Tests (Sensitive vs Specific)
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Combine sensitive screening test with specific confirmatory test

discordant_tests <- decisioncombine(
  data = decisioncombine_discordant,
  gold = "gold_standard",
  goldPositive = "Positive",
  test1 = "sensitive_test",
  test1Positive = "Positive",
  test2 = "specific_test",
  test2Positive = "Positive",
  showIndividual = TRUE,
  showBarPlot = TRUE,
  showHeatmap = TRUE,
  showRecommendation = TRUE
)

# Interpretation:
# - Test1 (sensitive): high sensitivity, moderate specificity
# - Test2 (specific): high specificity, moderate sensitivity
# - Complementary strengths create useful combination patterns
# - Parallel strategy maximizes sensitivity
# - Serial strategy maximizes specificity
# - Mixed patterns offer balanced performance

# ═══════════════════════════════════════════════════════════
# Example 13: Multi-Modal Imaging Comparison
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Compare and combine CT and MRI imaging

data(decisioncombine_imaging, package = "ClinicoPath")

imaging_comparison <- decisioncombine(
  data = decisioncombine_imaging,
  gold = "gold_standard",
  goldPositive = "Malignant",
  test1 = "ct_scan",
  test1Positive = "Positive",
  test2 = "mri_scan",
  test2Positive = "Positive",
  showIndividual = TRUE,
  showBarPlot = TRUE,
  showHeatmap = TRUE,
  showRecommendation = TRUE
)

# Interpretation:
# - Compare individual modality performance
# - Combined reading (+/+ pattern) increases specificity
# - Either positive (parallel) increases sensitivity
# - Cost considerations: is dual imaging worth the improvement?
# - Candidate-rule ranking is descriptive and does not determine clinical use

# ═══════════════════════════════════════════════════════════
# Example 14: Complete Publication-Ready Analysis
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Comprehensive analysis for manuscript publication

publication_analysis <- decisioncombine(
  data = decisioncombine_pathology,
  gold = "gold_standard",
  goldPositive = "Malignant",
  test1 = "rater1",
  test1Positive = "Positive",
  test2 = "rater2",
  test2Positive = "Positive",

  # Show all statistics
  showIndividual = TRUE,
  showFrequency = TRUE,

  # Generate all visualizations
  showBarPlot = TRUE,
  showHeatmap = TRUE,
  showForest = TRUE,
  showDecisionTree = TRUE,

  # Show the descriptive candidate-rule ranking
  showRecommendation = TRUE,

  # Add pattern to dataset for further analysis

)

# Reporting guidelines for publication:
# 1. Report all pattern frequencies (how often each pattern occurs)
# 2. Show individual test performance for context
# 3. Present all patterns with 95% CI for key metrics
# 4. Visualize with multiple plot types (bar, heatmap, forest)
# 5. State clinical context and decision criteria
# 6. Pre-specify and justify any chosen combination strategy
# 7. Discuss trade-offs between sensitivity and specificity
# 8. Consider costs and consequences of false positives/negatives
# 9. Include the decision-space plot when it supports the reporting objective
# 10. Follow STARD guidelines for diagnostic accuracy studies

# ═══════════════════════════════════════════════════════════
# Example 15: Descriptive Comparison of Named Strategies
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Compare rules on one common analysis population. The performance
# table contains exact-pattern rules plus named Parallel and Serial strategies.
strategy_comparison <- decisioncombine(
  data = decisioncombine_discordant,
  gold = "gold_standard",
  goldPositive = "Positive",
  test1 = "sensitive_test",
  test1Positive = "Positive",
  test2 = "specific_test",
  test2Positive = "Positive",
  showIndividual = TRUE,
  showBarPlot = TRUE,
  showRecommendation = TRUE
)

# Interpretation:
# - Parallel calls positive when either test is positive
# - Serial calls positive only when both tests are positive
# - The pattern filter affects visualized exact-pattern types, not named strategies
# - Compare estimates and uncertainty against pre-specified clinical consequences
# - Treat the candidate-rule ranking as exploratory and sample-dependent

# ═══════════════════════════════════════════════════════════
# Additional Notes and Best Practices
# ═══════════════════════════════════════════════════════════

# 1. Pattern Interpretation (Two Tests):
#    - Each row treats one exact observed pattern as a positive classification rule
#    - +/+, -/-, +/-, and -/+ are not interchangeable with strategy labels
#    - Parallel and Serial are reported as separate named rows
#    - Pattern frequency matters: rare patterns may be unreliable

# 2. Pattern Interpretation (Three Tests):
#    - Eight exact patterns are evaluated as one-versus-rest rules
#    - Majority calls positive when at least two of three tests are positive
#    - Parallel and Serial use thresholds of at least one and all three positives
#    - Estimated performance depends on the data; inspect uncertainty and cell counts

# 3. Combination Strategies:
#    - Parallel testing: lower decision threshold
#      * Call positive if ANY test positive
#      * Sensitivity >= individual test sensitivities
#      * Specificity <= individual test specificities
#    - Serial testing: higher decision threshold
#      * Call positive only if ALL tests positive
#      * Specificity >= individual test specificities
#      * Sensitivity <= individual test sensitivities
#    - Majority rule: intermediate threshold
#      * Applicable only with >=3 tests
#      * It does not guarantee improved accuracy or independent errors

# 4. Statistic Filtering:
#    - The option controls which metric supported plots display
#    - It does not optimize, choose, or validate a rule
#    - Predictive values depend on prevalence in the analyzed sample
#    - The forest plot supports only metrics for which this analysis computes intervals

# 5. Clinical Context Considerations:
#    - Screening programs may prioritize sensitivity
#    - Confirmatory diagnosis may prioritize specificity
#    - Resource constraints: Consider cost of additional testing
#    - Prevalence: Affects PPV and NPV interpretation
#    - Consequences: Weight costs of false positives vs false negatives

# 6. Test Selection for Combination:
#    - Independent tests: Reduce correlated errors
#    - Complementary strengths: One sensitive, one specific
#    - Different modalities: Clinical + lab + imaging
#    - Multiple raters: Assess agreement and consensus
#    - Avoid redundant tests: Similar tests add little value

# 7. Sample Size Considerations:
#    - Minimum 10-20 observations per pattern
#    - With 2 tests (4 patterns): need >=40-80 total observations
#    - With 3 tests (8 patterns): need >=80-160 total observations
#    - Rare patterns may have unreliable estimates
#    - Larger samples provide narrower confidence intervals

# 8. Visualization Interpretation:
#    - Bar plot: Easy comparison of metrics across patterns
#    - Heatmap: Quick identification of hot/cold spots
#    - Forest plot: Shows precision with confidence intervals
#    - Decision space: Shows sensitivity-versus-specificity positions
#    - Use multiple visualizations for comprehensive understanding

# 9. Individual Test Statistics:
#    - Always show for context and comparison
#    - A combination is not guaranteed to improve individual-test performance
#    - Compare like-for-like denominators when missing values differ
#    - Consider whether improvement justifies added complexity/cost

# 10. Frequency Analysis:
#     - Pattern frequencies show real-world distribution
#     - Rare patterns (<5% frequency) may be unstable
#     - High concordance (+/+ and -/-) suggests good agreement
#     - High discordance (+/- and -/+) suggests poor agreement
#     - Frequency affects practical utility of patterns

# 11. Candidate-Rule Ranking Interpretation:
#     - Ranks eligible observed Youden values without multiplicity correction
#     - Includes exact-pattern rules and named strategies after duplicate rules are removed
#     - Does not establish superiority, transportability, or clinical utility
#     - Use it as an exploratory summary, not as a recommendation

# 12. Adding Pattern to Dataset:
#     - addedPattern = TRUE creates new variable
#     - Useful for downstream analyses (survival, regression)
#     - Pattern variable shows test combination result
#     - Can be used to stratify or subset analyses
#     - Enables pattern-specific subgroup analyses

# 13. Reporting Standards:
#     - STARD guidelines for diagnostic test accuracy
#     - Report all pattern frequencies
#     - Show metrics with 95% confidence intervals
#     - Include individual test performance
#     - Justify combination strategy selection
#     - Describe clinical decision criteria
#     - Discuss trade-offs and limitations

# 14. Common Pitfalls to Avoid:
#     - Ignoring pattern frequencies (rare patterns unreliable)
#     - Choosing strategy without clinical justification
#     - Not comparing to individual test performance
#     - Overlooking costs of additional testing
#     - Ignoring prevalence effects on PPV/NPV
#     - Over-interpreting small differences in metrics
#     - Using too many tests (diminishing returns)

# 15. Advanced Considerations:
#     - Three-test combinations offer more flexibility
#     - Consider costs of testing in decision algorithm
#     - Temporal aspects: repeat testing over time
#     - Multiple raters: assess inter-rater reliability
#     - Different positive thresholds: optimize each test first
#     - Sequential testing: conditional on previous results
#     - Bayesian approaches: incorporate prior probabilities
