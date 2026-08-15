# decisioncompare Test Data - Basic Two-Test Comparison

Basic dataset with 200 patients for comparing two diagnostic tests
against a gold standard. Test 1 (Sens=0.85, Spec=0.90), Test 2
(Sens=0.80, Spec=0.85).

## Usage

``` r
decisioncompare_test
```

## Format

A data frame with 200 rows and 5 variables:

- patient_id:

  Character: Patient identifier (PT001-PT200)

- GoldStandard:

  Factor: True disease status ("Negative", "Positive"), 30% prevalence

- Test1:

  Factor: First test result ("Negative", "Positive"), Sens=0.85,
  Spec=0.90

- Test2:

  Factor: Second test result ("Negative", "Positive"), Sens=0.80,
  Spec=0.85

- age:

  Numeric: Patient age in years (mean 58, SD 12)

- sex:

  Factor: "Male" or "Female"

## Source

Generated test data for ClinicoPath package

## Details

Simulated with 30% disease prevalence. Tests have good characteristics
with Test1 slightly superior to Test2. Suitable for demonstrating basic
test comparison with confidence intervals and McNemar's test.

## Examples

``` r
data(decisioncompare_test)
decisioncompare(data = decisioncompare_test, gold = "GoldStandard",
                goldPositive = "Positive", goldNegative = NULL, test1 = "Test1",
                test1Positive = "Positive", test1Negative = NULL, test2 = "Test2",
                test2Positive = "Positive", test2Negative = NULL,
                test3Positive = "", test3Negative = NULL,
                ci = TRUE, statComp = TRUE)
#> 
#>  COMPARE MEDICAL DECISION TESTS
#> 
#>  Test 1 - Recoded Data                                            
#>  ──────────────────────────────────────────────────────────────── 
#>                     Gold Positive    Gold Negative    Total       
#>  ──────────────────────────────────────────────────────────────── 
#>    Test Positive        62.000000         12.00000     74.00000   
#>    Test Negative         9.000000        117.00000    126.00000   
#>    Total                71.000000        129.00000    200.00000   
#>  ──────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test 1 - Confidence Intervals                                            
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Decision Statistics              Estimate     Lower        Upper       
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Apparent prevalence               37.00000     30.29698     44.09465   
#>    True prevalence                   35.50000     28.87838     42.55862   
#>    Test sensitivity                  87.32394     77.29595     94.03642   
#>    Test specificity                  90.69767     84.31447     95.10049   
#>    Diagnostic accuracy               89.50000     84.39813     93.38188   
#>    Positive predictive value         83.78378     73.38642     91.33010   
#>    Negative predictive value         92.85714     86.87359     96.68197   
#>    Proportion of false positives      9.30233      4.89951     15.68553   
#>    Proportion of false negative      12.67606      5.96358     22.70405   
#>    False Discovery Rate              16.21622      8.66990     26.61358   
#>    False Omission Rate                7.14286      3.31803     13.12641   
#>  ──────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test 2 - Recoded Data                                            
#>  ──────────────────────────────────────────────────────────────── 
#>                     Gold Positive    Gold Negative    Total       
#>  ──────────────────────────────────────────────────────────────── 
#>    Test Positive         56.00000         18.00000     74.00000   
#>    Test Negative         15.00000        111.00000    126.00000   
#>    Total                 71.00000        129.00000    200.00000   
#>  ──────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test 2 - Confidence Intervals                                            
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Decision Statistics              Estimate     Lower        Upper       
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Apparent prevalence               37.00000     30.29698     44.09465   
#>    True prevalence                   35.50000     28.87838     42.55862   
#>    Test sensitivity                  78.87324     67.55973     87.66594   
#>    Test specificity                  86.04651     78.84665     91.51575   
#>    Diagnostic accuracy               83.50000     77.61565     88.36169   
#>    Positive predictive value         75.67568     64.30857     84.90182   
#>    Negative predictive value         88.09524     81.12595     93.18155   
#>    Proportion of false positives     13.95349      8.48425     21.15335   
#>    Proportion of false negative      21.12676     12.33406     32.44027   
#>    False Discovery Rate              24.32432     15.09818     35.69143   
#>    False Omission Rate               11.90476      6.81845     18.87405   
#>  ──────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Decision Test Comparison                                                                                                                                                                                                                                 
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test                                                                                     Sensitivity    Specificity    Accuracy     Positive Predictive Value    Negative Predictive Value    Positive Likelihood Ratio    Negative Likelihood Ratio   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test1                                                                                       87.32394       90.69767     89.50000                     83.78378                     92.85714                     9.387324                    0.1397616   
#>      → Good balanced performance; Moderate positive evidence; Moderate negative evidence                                                                                                                                                                  
#>    Test2                                                                                       78.87324       86.04651     83.50000                     75.67568                     88.09524                     5.652582                    0.2455272   
#>      → Good specificity for confirmation; Moderate positive evidence                                                                                                                                                                                      
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Statistical Comparison of Test Accuracy                                                                                
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Comparison        Chi-squared    df    p-value      Clinical Interpretation                                          
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test1 vs Test2       2.520833     1    0.1123512    No significant difference (p>=0.1) (Holm-Bonferroni corrected)   
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Note. For 2 tests: McNemar's test compares diagnostic CORRECTNESS (agreement with gold standard) between paired
#>    tests. For 3+ tests: Cochran's Q test provides an overall test, followed by pairwise McNemar's tests with
#>    Holm-Bonferroni correction for multiple comparisons. Tests examine discordant pairs (cases where one test is
#>    correct and the other is wrong relative to the gold standard) to determine if differences in accuracy are
#>    statistically significant.
#> 
#> 
#>  Differences with 95% Confidence Intervals                                  
#>  ────────────────────────────────────────────────────────────────────────── 
#>    Comparison        Metric         Difference     Lower        Upper       
#>  ────────────────────────────────────────────────────────────────────────── 
#>    Test1 vs Test2    Sensitivity      8.45070 ᵃ     -4.34717     21.24858   
#>    Test1 vs Test2    Specificity      4.65116 ᵇ     -3.05435     12.35668   
#>    Test1 vs Test2    Accuracy         6.00000 ᵈ     -0.73840     12.73840   
#>  ────────────────────────────────────────────────────────────────────────── 
#>    Note. Differences are paired (within-subject). Confidence intervals
#>    are 95% normal-approximation (Wald) intervals for the difference
#>    between two correlated proportions, computed from the discordant
#>    pair counts. They are not affected by the "CI Method for Agreement"
#>    option, which applies to the overall percent agreement table.
#>    ᵃ Small paired sample/discordant counts; CI may be unstable (n=71,
#>    discordant counts: 48, 14, 8, 1).
#>    ᵇ Small paired sample/discordant counts; CI may be unstable (n=129,
#>    discordant counts: 101, 16, 10, 2).
#>    ᵈ Small paired sample/discordant counts; CI may be unstable (n=200,
#>    discordant counts: 149, 30, 18, 3).
#> 
#> 
#>  <div style="font-family: Arial, sans-serif; max-width: 800px; margin:
#>  0 auto; padding: 20px;"><h2 style="color: #2c3e50; border-bottom: 2px
#>  solid #3498db;"> Clinical Summary
#> 
#>  Among the tests evaluated, Test1 had the highest observed accuracy,
#>  with 87.3% sensitivity (95% CI: 77.3-94.0%), 90.7% specificity (95%
#>  CI: 84.3-95.1%), 83.8% positive predictive value, 92.9% negative
#>  predictive value, and 89.5% overall accuracy. Statistical comparison
#>  (McNemar's/Cochran's test) did not reveal a statistically significant
#>  difference in test performance (detailed results in the comparison
#>  tables). The likelihood ratio for positive results was 9.39 and for
#>  negative results was 0.14. Because the differences between tests were
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
#>  diagnostic performance of 2 tests (Test1, Test2) against the gold
#>  standard reference using diagnostic accuracy analysis. The study
#>  included 200 cases with complete data. Performance metrics calculated
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
#>  evaluated, Test1 had the highest observed accuracy, with 87.3%
#>  sensitivity (95% CI: 77.3-94.0%), 90.7% specificity (95% CI:
#>  84.3-95.1%), 83.8% positive predictive value, 92.9% negative
#>  predictive value, and 89.5% overall accuracy. Statistical comparison
#>  (McNemar's/Cochran's test) did not reveal a statistically significant
#>  difference in test performance (detailed results in the comparison
#>  tables). The likelihood ratio for positive results was 9.39 and for
#>  negative results was 0.14. Because the differences between tests were
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
#>  Clinical Consideration: Consider using Test1 in combination with other
#>  tests for optimal diagnostic accuracy.
#> 
#>  <p style="background-color: #f8d7da; padding: 10px; border-radius:
#>  4px;">Caution: No statistically significant difference was found
#>  between the tests compared. Test1 is named here only because it ranked
#>  highest in this sample; the data do not establish that it outperforms
#>  the others. Base any choice between these tests on cost, availability,
#>  turnaround time, and harms as well as on these estimates.
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
#>  <span style='color: #374151;'>2 diagnostic tests compared using 200
#>  complete cases. Gold standard identified 71 diseased and 129 healthy
#>  cases. Review comparison tables and statistical tests below.
```
