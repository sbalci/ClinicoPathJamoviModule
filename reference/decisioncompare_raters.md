# decisioncompare Inter-Rater Data

Dataset with 150 patients evaluating three raters (experienced,
moderate, junior) against consensus panel gold standard.

## Usage

``` r
decisioncompare_raters
```

## Format

A data frame with 150 rows and 5 variables:

- patient_id:

  Character: Patient identifier (PT001-PT150)

- ConsensusPanel:

  Factor: Consensus diagnosis ("No Disease", "Disease"), 32% disease

- Rater1:

  Factor: Experienced rater ("No Disease", "Disease"), Sens=0.88,
  Spec=0.90

- Rater2:

  Factor: Moderate experience ("No Disease", "Disease"), Sens=0.82,
  Spec=0.85

- Rater3:

  Factor: Junior rater ("No Disease", "Disease"), Sens=0.75, Spec=0.82

- case_difficulty:

  Factor: Case difficulty (Easy, Moderate, Difficult)

## Source

Generated test data for ClinicoPath package

## Details

Inter-rater reliability study with varying expertise levels.
Demonstrates performance comparison across different experience levels.

## Examples

``` r
data(decisioncompare_raters)
decisioncompare(data = decisioncompare_raters, gold = "ConsensusPanel",
                goldPositive = "Disease", goldNegative = NULL, test1 = "Rater1",
                test1Positive = "Disease", test1Negative = NULL, test2 = "Rater2",
                test2Positive = "Disease", test2Negative = NULL, test3 = "Rater3",
                test3Positive = "Disease", test3Negative = NULL, statComp = TRUE)
#> Performing 3 pairwise comparisons with Holm-Bonferroni correction...
#> Statistical comparisons completed (3 comparisons, Holm-Bonferroni corrected).
#> 
#>  COMPARE MEDICAL DECISION TESTS
#> 
#>  Test 1 - Recoded Data                                            
#>  ──────────────────────────────────────────────────────────────── 
#>                     Gold Positive    Gold Negative    Total       
#>  ──────────────────────────────────────────────────────────────── 
#>    Test Positive        35.000000         9.000000     44.00000   
#>    Test Negative         4.000000       102.000000    106.00000   
#>    Total                39.000000       111.000000    150.00000   
#>  ──────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test 2 - Recoded Data                                            
#>  ──────────────────────────────────────────────────────────────── 
#>                     Gold Positive    Gold Negative    Total       
#>  ──────────────────────────────────────────────────────────────── 
#>    Test Positive         29.00000         11.00000     40.00000   
#>    Test Negative         10.00000        100.00000    110.00000   
#>    Total                 39.00000        111.00000    150.00000   
#>  ──────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test 3 - Recoded Data                                            
#>  ──────────────────────────────────────────────────────────────── 
#>                     Gold Positive    Gold Negative    Total       
#>  ──────────────────────────────────────────────────────────────── 
#>    Test Positive         29.00000         12.00000     41.00000   
#>    Test Negative         10.00000         99.00000    109.00000   
#>    Total                 39.00000        111.00000    150.00000   
#>  ──────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Decision Test Comparison                                                                                                                                                                                                                               
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test                                                                                   Sensitivity    Specificity    Accuracy     Positive Predictive Value    Negative Predictive Value    Positive Likelihood Ratio    Negative Likelihood Ratio   
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Rater1                                                                                    89.74359       91.89189     91.33333                     79.54545                     96.22642                    11.068376                    0.1116139   
#>      → Good balanced performance; Strong positive evidence; Moderate negative evidence                                                                                                                                                                  
#>    Rater2                                                                                    74.35897       90.09009     86.00000                     72.50000                     90.90909                     7.503497                    0.2846154   
#>      → Good specificity for confirmation; Moderate positive evidence                                                                                                                                                                                    
#>    Rater3                                                                                    74.35897       89.18919     85.33333                     70.73171                     90.82569                     6.878205                    0.2874903   
#>      → Good specificity for confirmation; Moderate positive evidence                                                                                                                                                                                    
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Statistical Comparison of Test Accuracy                                                                                     
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Comparison           Chi-squared    df    p-value      Clinical Interpretation                                            
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Overall (3 tests)       2.979592     2    0.2254187    No significant overall difference among tests (p>=0.05)        ᵃ   
#>    Rater1 vs Rater2        1.531250     1    0.4318499    No significant difference (p>=0.1) (Holm-Bonferroni corrected)     
#>    Rater1 vs Rater3        2.206897     1    0.4121845    No significant difference (p>=0.1) (Holm-Bonferroni corrected)     
#>    Rater2 vs Rater3        0.000000     1    1.0000000    No significant difference (p>=0.1) (Holm-Bonferroni corrected)     
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Note. For 2 tests: McNemar's test compares diagnostic CORRECTNESS (agreement with gold standard) between paired
#>    tests. For 3+ tests: Cochran's Q test provides an overall test, followed by pairwise McNemar's tests with
#>    Holm-Bonferroni correction for multiple comparisons. Tests examine discordant pairs (cases where one test is correct
#>    and the other is wrong relative to the gold standard) to determine if differences in accuracy are statistically
#>    significant.
#>    ᵃ Cochran's Q test shows no significant difference. Pairwise comparisons below may not be meaningful.
#> 
#> 
#>  Differences with 95% Confidence Intervals                                    
#>  ──────────────────────────────────────────────────────────────────────────── 
#>    Comparison          Metric         Difference     Lower        Upper       
#>  ──────────────────────────────────────────────────────────────────────────── 
#>    Rater1 vs Rater2    Sensitivity     15.38462 ᵃ     -1.34142     32.11065   
#>    Rater1 vs Rater2    Specificity      1.80180 ᵇ     -6.08768      9.69128   
#>    Rater1 vs Rater2    Accuracy         5.33333 ᵈ     -2.00871     12.67538   
#>    Rater1 vs Rater3    Sensitivity     15.38462 ᵉ      0.24368     30.52555   
#>    Rater1 vs Rater3    Specificity      2.70270 ᶠ     -4.97751     10.38292   
#>    Rater1 vs Rater3    Accuracy         6.00000 ᵍ     -0.97067     12.97067   
#>    Rater2 vs Rater3    Sensitivity      0.00000 ʰ    -20.10219     20.10219   
#>    Rater2 vs Rater3    Specificity      0.90090 ⁱ     -7.18897      8.99077   
#>    Rater2 vs Rater3    Accuracy         0.66667 ʲ     -7.28061      8.61395   
#>  ──────────────────────────────────────────────────────────────────────────── 
#>    Note. Differences are paired (within-subject). Confidence intervals
#>    are 95% normal-approximation (Wald) intervals for the difference
#>    between two correlated proportions, computed from the discordant pair
#>    counts. They are not affected by the "CI Method for Agreement" option,
#>    which applies to the overall percent agreement table.
#>    ᵃ Small paired sample/discordant counts; CI may be unstable (n=39,
#>    discordant counts: 26, 9, 3, 1).
#>    ᵇ Small paired sample/discordant counts; CI may be unstable (n=111,
#>    discordant counts: 91, 11, 9, 0).
#>    ᵈ Small paired sample/discordant counts; CI may be unstable (n=150,
#>    discordant counts: 117, 20, 12, 1).
#>    ᵉ Small paired sample/discordant counts; CI may be unstable (n=39,
#>    discordant counts: 27, 8, 2, 2).
#>    ᶠ Small paired sample/discordant counts; CI may be unstable (n=111,
#>    discordant counts: 91, 11, 8, 1).
#>    ᵍ Small paired sample/discordant counts; CI may be unstable (n=150,
#>    discordant counts: 118, 19, 10, 3).
#>    ʰ Small paired sample/discordant counts; CI may be unstable (n=39,
#>    discordant counts: 21, 8, 8, 2).
#>    ⁱ Small paired sample/discordant counts; CI may be unstable (n=111,
#>    discordant counts: 89, 11, 10, 1).
#>    ʲ Small paired sample/discordant counts; CI may be unstable (n=150,
#>    discordant counts: 110, 19, 18, 3).
#> 
#> 
#>  <div style="font-family: Arial, sans-serif; max-width: 800px; margin:
#>  0 auto; padding: 20px;"><h2 style="color: #2c3e50; border-bottom: 2px
#>  solid #3498db;"> Clinical Summary
#> 
#>  Among the tests evaluated, Rater1 had the highest observed accuracy,
#>  with 89.7% sensitivity (95% CI: 75.8-97.1%), 91.9% specificity (95%
#>  CI: 85.2-96.2%), 79.5% positive predictive value, 96.2% negative
#>  predictive value, and 91.3% overall accuracy. Statistical comparison
#>  (McNemar's/Cochran's test) did not reveal a statistically significant
#>  difference in test performance (detailed results in the comparison
#>  tables). The likelihood ratio for positive results was 11.07 and for
#>  negative results was 0.11. Because the differences between tests were
#>  not statistically significant, this ranking reflects the observed
#>  sample and should not be reported as evidence that one test
#>  outperforms the others.<h3 style="color: #27ae60; margin-top: 30px;">
#>  Report Sentences
#> 
#>  <div style="background-color: #f8f9fa; padding: 15px; border-left: 4px
#>  solid #28a745; margin: 15px 0;"><h4 style="margin-top: 0;">Methods
#>  Section:
#> 
#>  <p style="font-style: italic; line-height: 1.6;">We compared the
#>  diagnostic performance of 3 tests (Rater1, Rater2, Rater3) against the
#>  gold standard reference using diagnostic accuracy analysis. The study
#>  included 150 cases with complete data. Performance metrics calculated
#>  included sensitivity, specificity, positive and negative predictive
#>  values, likelihood ratios, and overall accuracy. Statistical
#>  comparisons between tests were performed using McNemar's test
#>  comparing diagnostic correctness (agreement with gold standard).
#> 
#>  <div style="background-color: #e8f4f8; padding: 15px; border-left: 4px
#>  solid #3498db; margin: 15px 0;"><h4 style="margin-top: 0;">Results
#>  Section:
#> 
#>  <p style="font-style: italic; line-height: 1.6;">Among the tests
#>  evaluated, Rater1 had the highest observed accuracy, with 89.7%
#>  sensitivity (95% CI: 75.8-97.1%), 91.9% specificity (95% CI:
#>  85.2-96.2%), 79.5% positive predictive value, 96.2% negative
#>  predictive value, and 91.3% overall accuracy. Statistical comparison
#>  (McNemar's/Cochran's test) did not reveal a statistically significant
#>  difference in test performance (detailed results in the comparison
#>  tables). The likelihood ratio for positive results was 11.07 and for
#>  negative results was 0.11. Because the differences between tests were
#>  not statistically significant, this ranking reflects the observed
#>  sample and should not be reported as evidence that one test
#>  outperforms the others.
#> 
#>  <h3 style="color: #8e44ad; margin-top: 30px;"> Clinical
#>  Recommendations
#> 
#>  <div style="background-color: #fff3cd; padding: 15px; border-radius:
#>  8px;">
#> 
#>  Clinical Consideration: Consider using Rater1 in combination with
#>  other tests for optimal diagnostic accuracy.
#> 
#>  <p style="background-color: #f8d7da; padding: 10px; border-radius:
#>  4px;">Caution: No statistically significant difference was found
#>  between the tests compared. Rater1 is named here only because it
#>  ranked highest in this sample; the data do not establish that it
#>  outperforms the others. Base any choice between these tests on cost,
#>  availability, turnaround time, and harms as well as on these
#>  estimates.
#> 
#>  Implementation Note: Results should be interpreted in the context of
#>  disease prevalence in your clinical population. Consider local
#>  validation studies before implementation.
#> 
#>  <div style="font-family: Arial, sans-serif; max-width: 900px; margin:
#>  0 auto; padding: 20px;"><h2 style="color: #2c3e50; text-align: center;
#>  border-bottom: 2px solid #3498db; padding-bottom: 10px;"> About
#>  Medical Decision Test Comparison
#> 
#>  <div style="background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb
#>  100%); padding: 20px; border-radius: 10px; margin: 20px 0;"><h3
#>  style="color: #1565c0; margin-top: 0;"> What This Analysis Does
#> 
#>  <p style="line-height: 1.6; color: #333;">This tool compares the
#>  diagnostic performance of multiple medical tests against a gold
#>  standard reference. It systematically evaluates sensitivity,
#>  specificity, predictive values, likelihood ratios, and overall
#>  accuracy to help you determine which test performs best for your
#>  clinical scenario.
#> 
#>  <div style="background-color: #f1f8e9; border: 1px solid #8bc34a;
#>  padding: 20px; border-radius: 8px; margin: 20px 0;"><h3 style="color:
#>  #4a7c59; margin-top: 0;"> When to Use This Analysis
#> 
#>  <ul style="line-height: 1.8; color: #4a7c59;">Test Validation:
#>  Comparing new diagnostic methods against established standardsMethod
#>  Comparison: Evaluating which of several tests performs betterClinical
#>  Research: Validating biomarkers, imaging techniques, or clinical
#>  assessmentsQuality Assessment: Measuring agreement between different
#>  raters or methodsProtocol Development: Optimizing diagnostic
#>  workflows<div style="background-color: #fff3e0; border: 1px solid
#>  #ff9800; padding: 20px; border-radius: 8px; margin: 20px 0;"><h3
#>  style="color: #e65100; margin-top: 0;"> How to Use This Analysis
#> 
#>  <ol style="line-height: 1.8; color: #e65100;">Select Gold Standard:
#>  Choose your most reliable reference test (e.g., biopsy, expert
#>  consensus)Choose Tests to Compare: Select 2-3 diagnostic tests you
#>  want to evaluateDefine Positive Levels: Specify what constitutes a
#>  "positive" result for each testConfigure Options: Enable statistical
#>  comparisons, confidence intervals, or visualizations as neededRun
#>  Analysis: Review results tables and clinical interpretationsCopy
#>  Report: Use the auto-generated sentences for your documentation<div
#>  style="background-color: #f3e5f5; border: 1px solid #9c27b0; padding:
#>  20px; border-radius: 8px; margin: 20px 0;"><h3 style="color: #6a1b9a;
#>  margin-top: 0;"> Key Metrics Explained
#> 
#>  <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 15px;
#>  color: #6a1b9a;">
#> 
#>  Sensitivity: Probability test is positive when disease present
#>  (rule-out ability)
#> 
#>  Specificity: Probability test is negative when disease absent (rule-in
#>  ability)
#> 
#>  PPV: Probability of disease when test positive
#> 
#>  NPV: Probability of no disease when test negative
#> 
#>  LR+: How much positive test increases odds of disease
#> 
#>  LR-: How much negative test decreases odds of disease
#> 
#>  Accuracy: Overall probability of correct classification
#> 
#>  McNemar Test: Statistical comparison between paired tests
#> 
#>  <div style="background-color: #e8f5e8; border: 1px solid #4caf50;
#>  padding: 20px; border-radius: 8px; margin: 20px 0;"><h3 style="color:
#>  #2e7d32; margin-top: 0;"> Clinical Interpretation Guidelines
#> 
#>  <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 15px;
#>  color: #2e7d32;"><h4 style="margin-bottom: 5px;">Screening Tests
#>  (Rule-Out):
#> 
#>  <p style="margin-top: 0;">• Sensitivity >=95%: Excellent
#>  • NPV >=95%: High confidence
#>  • Goal: Minimize false negatives
#> 
#>  <h4 style="margin-bottom: 5px;">Confirmatory Tests (Rule-In):
#> 
#>  <p style="margin-top: 0;">• Specificity >=95%: Excellent
#>  • PPV >=90%: High confidence
#>  • Goal: Minimize false positives
#> 
#>  <div style="background-color: #fff8e1; border: 1px solid #ffc107;
#>  padding: 20px; border-radius: 8px; margin: 20px 0;"><h3 style="color:
#>  #f57f17; margin-top: 0;"> Important Assumptions & Limitations
#> 
#>  <ul style="line-height: 1.6; color: #f57f17;">Gold Standard: Assumes
#>  your reference test is truly accurateSample Size: Results more
#>  reliable with larger, representative samplesPrevalence Dependency: PPV
#>  and NPV vary with disease prevalenceMcNemar Test: Requires
#>  paired/matched data for statistical comparisonsMissing Data: Cases
#>  with incomplete data are excluded from analysisConfidence Intervals:
#>  The per-test CI tables report Clopper-Pearson exact intervals for the
#>  proportions (sensitivity, specificity, PPV, NPV, accuracy and
#>  prevalence), as computed by epiR::epi.tests() with its default
#>  settings. Likelihood ratios are reported as point estimates only,
#>  without confidence intervals. The Overall Percent Agreement (OPA)
#>  table uses the method you select under "CI Method for Agreement"
#>  (Wilson score by default). Paired differences between tests use
#>  normal-approximation (Wald) intervals.
#> 
#>  <div style='margin: 10px 0;'><div style='background-color: #eff6ff;
#>  border-left: 4px solid #93c5fd; padding: 12px; margin: 8px 0;
#>  border-radius: 4px;'><strong style='color: #2563eb;'> Analysis
#>  Completed Successfully
#>  <span style='color: #374151;'>3 diagnostic tests compared using 150
#>  complete cases. Gold standard identified 39 diseased and 111 healthy
#>  cases. Review comparison tables and statistical tests below.
```
