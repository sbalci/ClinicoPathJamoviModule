# decisioncompare Identical Tests Data

Edge case dataset with 100 patients where Test1 and Test2 are identical.
Tests handling of perfect agreement between tests.

## Usage

``` r
decisioncompare_identical
```

## Format

A data frame with 100 rows and 4 variables:

- patient_id:

  Character: Patient identifier (PT001-PT100)

- GoldStandard:

  Factor: True status ("Negative", "Positive"), 30% positive

- Test1:

  Factor: First test ("Negative", "Positive"), Sens=0.85, Spec=0.88

- Test2:

  Factor: Identical to Test1

- age:

  Numeric: Patient age in years (mean 58, SD 10)

## Source

Generated test data for ClinicoPath package

## Details

Tests are 100% identical. McNemar's test should show no significant
difference (p=1.0).

## Examples

``` r
data(decisioncompare_identical)
decisioncompare(data = decisioncompare_identical, gold = "GoldStandard",
                goldPositive = "Positive", test1 = "Test1",
                test1Positive = "Positive", test2 = "Test2",
                test2Positive = "Positive", test3Positive = "",
                statComp = TRUE)
#> 
#>  COMPARE MEDICAL DECISION TESTS
#> 
#> character(0)
#> 
#>  Test 1 - Recoded Data                                            
#>  ──────────────────────────────────────────────────────────────── 
#>                     Gold Positive    Gold Negative    Total       
#>  ──────────────────────────────────────────────────────────────── 
#>    Test Positive        22.000000         11.00000     33.00000   
#>    Test Negative         3.000000         64.00000     67.00000   
#>    Total                25.000000         75.00000    100.00000   
#>  ──────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test 2 - Recoded Data                                            
#>  ──────────────────────────────────────────────────────────────── 
#>                     Gold Positive    Gold Negative    Total       
#>  ──────────────────────────────────────────────────────────────── 
#>    Test Positive        22.000000         11.00000     33.00000   
#>    Test Negative         3.000000         64.00000     67.00000   
#>    Total                25.000000         75.00000    100.00000   
#>  ──────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test 3 - Recoded Data                                        
#>  ──────────────────────────────────────────────────────────── 
#>                     Gold Positive    Gold Negative    Total   
#>  ──────────────────────────────────────────────────────────── 
#>    Test Positive    .                .                .       
#>    Test Negative    .                .                .       
#>    Total            .                .                .       
#>  ──────────────────────────────────────────────────────────── 
#> 
#> 
#>  Decision Test Comparison                                                                                                                                                                                                                                 
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test                                                                                     Sensitivity    Specificity    Accuracy     Positive Predictive Value    Negative Predictive Value    Positive Likelihood Ratio    Negative Likelihood Ratio   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test1                                                                                       88.00000       85.33333     86.00000                     66.66667                     95.52239                     6.000000                    0.1406250   
#>      → Good balanced performance; Moderate positive evidence; Moderate negative evidence                                                                                                                                                                  
#>    Test2                                                                                       88.00000       85.33333     86.00000                     66.66667                     95.52239                     6.000000                    0.1406250   
#>      → Good balanced performance; Moderate positive evidence; Moderate negative evidence                                                                                                                                                                  
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Stratified Diagnostic Accuracy                                                                              
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Subgroup    N    Test    Sensitivity    Specificity    Accuracy     PPV          NPV          OPA         
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Statistical Comparison of Test Accuracy                                                                                  
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Comparison        Chi-squared    df    p-value        Clinical Interpretation                                          
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test1 vs Test2            NaN     1          NaN ᵃ    No significant difference (p>=0.1) (Holm-Bonferroni corrected)   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Note. For 2 tests: McNemar's test compares diagnostic CORRECTNESS (agreement with gold standard) between paired
#>    tests. For 3+ tests: Cochran's Q test provides an overall test, followed by pairwise McNemar's tests with
#>    Holm-Bonferroni correction for multiple comparisons. Tests examine discordant pairs (cases where one test is
#>    correct and the other is wrong relative to the gold standard) to determine if differences in accuracy are
#>    statistically significant.
#>    ᵃ Small number of discordant pairs (n=0). Results may be unreliable (recommend n>=10).
#> 
#> 
#>  Differences with 95% Confidence Intervals                                  
#>  ────────────────────────────────────────────────────────────────────────── 
#>    Comparison        Metric         Difference     Lower        Upper       
#>  ────────────────────────────────────────────────────────────────────────── 
#>    Test1 vs Test2    Sensitivity      0.00000 ᵃ      0.00000      0.00000   
#>    Test1 vs Test2    Specificity      0.00000 ᵇ      0.00000      0.00000   
#>    Test1 vs Test2    Accuracy         0.00000 ᵈ      0.00000      0.00000   
#>  ────────────────────────────────────────────────────────────────────────── 
#>    ᵃ Small paired sample/discordant counts; CI may be unstable (n=25,
#>    discordant counts: 22, 0, 0, 3).
#>    ᵇ Small paired sample/discordant counts; CI may be unstable (n=75,
#>    discordant counts: 64, 0, 0, 11).
#>    ᵈ Small paired sample/discordant counts; CI may be unstable (n=100,
#>    discordant counts: 86, 0, 0, 14).
#> 
#> 
#>  <div style="font-family: Arial, sans-serif; max-width: 800px; margin:
#>  0 auto; padding: 20px;"><h2 style="color: #2c3e50; border-bottom: 2px
#>  solid #3498db;"> Clinical Summary
#> 
#>  Among the tests evaluated, Test1 demonstrated optimal diagnostic
#>  performance with 88% sensitivity (95% CI: [see confidence interval
#>  table]), 85.3% specificity (95% CI: [see confidence interval table]),
#>  66.7% positive predictive value, 95.5% negative predictive value, and
#>  86% overall accuracy. Statistical comparisons using McNemar's test
#>  revealed significant differences in test performance (detailed results
#>  in comparison tables). The likelihood ratio for positive results was
#>  6.00 and for negative results was 0.14.<h3 style="color: #27ae60;
#>  margin-top: 30px;"> Report Sentences
#> 
#>  <div style="background-color: #f8f9fa; padding: 15px; border-left: 4px
#>  solid #28a745; margin: 15px 0;"><h4 style="margin-top: 0;">Methods
#>  Section:
#> 
#>  <p style="font-style: italic; line-height: 1.6;">We compared the
#>  diagnostic performance of 2 tests (Test1, Test2) against the gold
#>  standard reference using diagnostic accuracy analysis. The study
#>  included 100 cases with complete data. Performance metrics calculated
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
#>  evaluated, Test1 demonstrated optimal diagnostic performance with 88%
#>  sensitivity (95% CI: [see confidence interval table]), 85.3%
#>  specificity (95% CI: [see confidence interval table]), 66.7% positive
#>  predictive value, 95.5% negative predictive value, and 86% overall
#>  accuracy. Statistical comparisons using McNemar's test revealed
#>  significant differences in test performance (detailed results in
#>  comparison tables). The likelihood ratio for positive results was 6.00
#>  and for negative results was 0.14.
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
#>  Calculated using Wilson method for better accuracy
#> 
#>  <div style='margin: 10px 0;'><div style='background-color: #eff6ff;
#>  border-left: 4px solid #93c5fd; padding: 12px; margin: 8px 0;
#>  border-radius: 4px;'><strong style='color: #2563eb;'> Analysis
#>  Completed Successfully
#>  <span style='color: #374151;'>2 diagnostic tests compared using 100
#>  complete cases. Gold standard identified 25 diseased and 75 healthy
#>  cases. Review comparison tables and statistical tests below.
```
