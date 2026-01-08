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
# - Pattern filtering (parallel, serial, majority rule strategies)
# - Statistic filtering (best sensitivity, specificity, Youden's J, etc.)
# - Multiple visualizations (bar plot, heatmap, forest plot, decision tree)
# - Optimal pattern recommendations for clinical decision-making

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
# - +/+ (both positive): highest specificity, lowest sensitivity
# - -/- (both negative): highest sensitivity, lowest specificity
# - Mixed patterns show intermediate performance
# - Compare patterns to identify optimal combination strategy

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
# - Combination patterns may outperform individual tests
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
# - +/+/+ (all positive): very high specificity, may miss some cases
# - -/-/- (all negative): very high sensitivity, may overdiagnose
# - Intermediate patterns balance sensitivity and specificity
# - More patterns provide more granular decision options

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
  filterPattern = "parallel",
  showIndividual = TRUE,
  showBarPlot = TRUE
)

# Interpretation:
# - Parallel = Test1+ OR Test2+ (either positive → call positive)
# - Maximizes sensitivity (few false negatives)
# - Lower specificity (more false positives)
# - Appropriate for screening where missing disease is costly
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
  filterPattern = "serial",
  showIndividual = TRUE,
  showBarPlot = TRUE,
  showRecommendation = TRUE
)

# Interpretation:
# - Serial = Test1+ AND Test2+ (both must be positive → call positive)
# - Maximizes specificity (few false positives)
# - Lower sensitivity (more false negatives)
# - Appropriate for confirmation where false positives are costly
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
  filterPattern = "majority",
  showIndividual = TRUE,
  showBarPlot = TRUE,
  showRecommendation = TRUE
)

# Interpretation:
# - Majority rule: ≥2 of 3 tests positive → call positive
# - Balances sensitivity and specificity
# - Patterns: +/+/+, +/+/-, +/-/+, -/+/+ are positive
# - Patterns: +/-/-, -/+/-, -/-/+, -/-/- are negative
# - Provides democratic decision when tests may disagree

# ═══════════════════════════════════════════════════════════
# Example 7: Filter by Sensitivity (Find Highest Sensitivity Pattern)
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Identify pattern with best sensitivity for screening

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
# - Highlights pattern with highest sensitivity
# - Usually -/- or mixed patterns (lower threshold for calling positive)
# - Trade-off: high sensitivity often means lower specificity
# - Use for screening where missing disease is most costly

# ═══════════════════════════════════════════════════════════
# Example 8: Filter by Specificity (Find Highest Specificity Pattern)
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Identify pattern with best specificity for confirmation

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
# - Highlights pattern with highest specificity
# - Usually +/+ pattern (both tests must agree)
# - Trade-off: high specificity often means lower sensitivity
# - Use for confirmation where false positives are most costly

# ═══════════════════════════════════════════════════════════
# Example 9: Filter by Youden's J (Best Overall Balance)
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Find pattern with best overall balance

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
# - Maximizes sum of sensitivity and specificity
# - Often identifies mixed patterns (+/- or -/+) as optimal
# - Appropriate when sensitivity and specificity equally important
# - Recommendation table shows clinical guidance

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
# - Decision tree: Hierarchical decision pathway
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
# - Pattern recommendation guides which combination strategy to use

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

  # Provide recommendations
  showRecommendation = TRUE,

  # Add pattern to dataset for further analysis
  addPatternToData = TRUE
)

# Reporting guidelines for publication:
# 1. Report all pattern frequencies (how often each pattern occurs)
# 2. Show individual test performance for context
# 3. Present all patterns with 95% CI for key metrics
# 4. Visualize with multiple plot types (bar, heatmap, forest)
# 5. State clinical context and decision criteria
# 6. Justify chosen combination strategy (parallel, serial, etc.)
# 7. Discuss trade-offs between sensitivity and specificity
# 8. Consider costs and consequences of false positives/negatives
# 9. Include decision tree showing clinical pathway
# 10. Follow STARD guidelines for diagnostic accuracy studies

# ═══════════════════════════════════════════════════════════
# Example 15: Sensitivity Analysis - Comparing Strategies
# ═══════════════════════════════════════════════════════════
# Clinical scenario: Compare different combination strategies for same data

# Strategy 1: All positive (any positive result)
all_positive <- decisioncombine(
  data = decisioncombine_discordant,
  gold = "gold_standard",
  goldPositive = "Positive",
  test1 = "sensitive_test",
  test1Positive = "Positive",
  test2 = "specific_test",
  test2Positive = "Positive",
  filterPattern = "allPositive",
  showIndividual = TRUE
)

# Strategy 2: Parallel (either test positive - same as all positive for 2 tests)
parallel <- decisioncombine(
  data = decisioncombine_discordant,
  gold = "gold_standard",
  goldPositive = "Positive",
  test1 = "sensitive_test",
  test1Positive = "Positive",
  test2 = "specific_test",
  test2Positive = "Positive",
  filterPattern = "parallel",
  showIndividual = TRUE
)

# Strategy 3: Serial (both tests positive)
serial <- decisioncombine(
  data = decisioncombine_discordant,
  gold = "gold_standard",
  goldPositive = "Positive",
  test1 = "sensitive_test",
  test1Positive = "Positive",
  test2 = "specific_test",
  test2Positive = "Positive",
  filterPattern = "serial",
  showIndividual = TRUE
)

# Strategy 4: All negative (both tests negative)
all_negative <- decisioncombine(
  data = decisioncombine_discordant,
  gold = "gold_standard",
  goldPositive = "Positive",
  test1 = "sensitive_test",
  test1Positive = "Positive",
  test2 = "specific_test",
  test2Positive = "Positive",
  filterPattern = "allNegative",
  showIndividual = TRUE
)

# Strategy 5: Mixed patterns only
mixed_only <- decisioncombine(
  data = decisioncombine_discordant,
  gold = "gold_standard",
  goldPositive = "Positive",
  test1 = "sensitive_test",
  test1Positive = "Positive",
  test2 = "specific_test",
  test2Positive = "Positive",
  filterPattern = "mixed",
  showIndividual = TRUE
)

# Interpretation of sensitivity analysis:
# - All positive/Parallel: Highest sensitivity, lowest specificity
# - Serial/Both positive: Highest specificity, lowest sensitivity
# - All negative: Opposite interpretation (negative = disease absent)
# - Mixed patterns: Intermediate performance, may be optimal
# - Compare metrics across strategies to inform clinical decision
# - Choose strategy based on clinical consequences:
#   * Screening → maximize sensitivity (parallel)
#   * Confirmation → maximize specificity (serial)
#   * General use → balance (Youden optimal pattern)

# ═══════════════════════════════════════════════════════════
# Additional Notes and Best Practices
# ═══════════════════════════════════════════════════════════

# 1. Pattern Interpretation (Two Tests):
#    - +/+ (Both positive): Highest specificity, use for confirmation
#    - -/- (Both negative): Highest sensitivity when called negative
#    - +/- or -/+ (Mixed): Intermediate performance, may be optimal
#    - Pattern frequency matters: rare patterns may be unreliable

# 2. Pattern Interpretation (Three Tests):
#    - +/+/+ (All positive): Very high specificity
#    - -/-/- (All negative): Very high sensitivity when called negative
#    - 2 of 3 positive: Majority rule, balanced approach
#    - 1 of 3 positive: High sensitivity, lower specificity
#    - More patterns provide more decision granularity

# 3. Combination Strategies:
#    - Parallel testing: Maximize sensitivity (screening)
#      * Call positive if ANY test positive
#      * Sensitivity ≥ individual test sensitivities
#      * Specificity ≤ individual test specificities
#    - Serial testing: Maximize specificity (confirmation)
#      * Call positive only if ALL tests positive
#      * Specificity ≥ individual test specificities
#      * Sensitivity ≤ individual test sensitivities
#    - Majority rule: Balance sensitivity and specificity
#      * Applicable only with ≥3 tests
#      * Democratic decision reduces individual test errors

# 4. Statistic Filtering Guidelines:
#    - Sensitivity: Choose when false negatives most costly (screening)
#    - Specificity: Choose when false positives most costly (confirmation)
#    - PPV: Choose when positive predictive value most important
#    - NPV: Choose when negative predictive value most important
#    - Youden's J: Choose for balanced approach (general use)
#    - Accuracy: Choose when equal costs for FP and FN
#    - Balanced accuracy: Choose with class imbalance

# 5. Clinical Context Considerations:
#    - Screening programs: Prioritize sensitivity (parallel strategy)
#    - Confirmatory diagnosis: Prioritize specificity (serial strategy)
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
#    - With 2 tests (4 patterns): need ≥40-80 total observations
#    - With 3 tests (8 patterns): need ≥80-160 total observations
#    - Rare patterns may have unreliable estimates
#    - Larger samples provide narrower confidence intervals

# 8. Visualization Interpretation:
#    - Bar plot: Easy comparison of metrics across patterns
#    - Heatmap: Quick identification of hot/cold spots
#    - Forest plot: Shows precision with confidence intervals
#    - Decision tree: Clarifies hierarchical decision pathway
#    - Use multiple visualizations for comprehensive understanding

# 9. Individual Test Statistics:
#    - Always show for context and comparison
#    - Combination should improve upon individual tests
#    - If no improvement, reconsider combination strategy
#    - Consider whether improvement justifies added complexity/cost

# 10. Frequency Analysis:
#     - Pattern frequencies show real-world distribution
#     - Rare patterns (<5% frequency) may be unstable
#     - High concordance (+/+ and -/-) suggests good agreement
#     - High discordance (+/- and -/+) suggests poor agreement
#     - Frequency affects practical utility of patterns

# 11. Recommendation Interpretation:
#     - Recommendations based on statistical optimality
#     - May not align with clinical priorities
#     - Consider clinical context when choosing pattern
#     - Trade-offs between sensitivity/specificity are explicit
#     - Use recommendations as starting point, not final decision

# 12. Adding Pattern to Dataset:
#     - addPatternToData = TRUE creates new variable
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
