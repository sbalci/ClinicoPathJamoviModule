# decisioncompare Common Disease Data

Dataset with 160 patients and high disease prevalence (60%), typical of
clinical diagnostic setting.

## Usage

``` r
decisioncompare_common
```

## Format

A data frame with 160 rows and 4 variables:

- patient_id:

  Character: Patient identifier (PT001-PT160)

- GoldStandard:

  Factor: True status ("Negative", "Positive"), 60% positive

- Test1:

  Factor: First test ("Negative", "Positive"), Sens=0.85, Spec=0.88

- Test2:

  Factor: Second test ("Negative", "Positive"), Sens=0.82, Spec=0.85

- clinical_setting:

  Factor: Setting (Inpatient, Outpatient)

## Source

Generated test data for ClinicoPath package

## Details

High prevalence (60%) typical of symptomatic clinical populations.
Contrasts with rare disease for prevalence impact on predictive values.

## Examples

``` r
data(decisioncompare_common)
decisioncompare(data = decisioncompare_common, gold = "GoldStandard",
                goldPositive = "Positive", goldNegative = NULL, test1 = "Test1",
                test1Positive = "Positive", test1Negative = NULL, test2 = "Test2",
                test2Positive = "Positive", test2Negative = NULL,
                test3Positive = "", test3Negative = NULL,
                ci = TRUE)
#> 
#>  COMPARE MEDICAL DECISION TESTS
#> 
#>  Test 1 - Recoded Data                                            
#>  ──────────────────────────────────────────────────────────────── 
#>                     Gold Positive    Gold Negative    Total       
#>  ──────────────────────────────────────────────────────────────── 
#>    Test Positive         83.00000         9.000000     92.00000   
#>    Test Negative         21.00000        47.000000     68.00000   
#>    Total                104.00000        56.000000    160.00000   
#>  ──────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test 1 - Confidence Intervals                                            
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Decision Statistics              Estimate     Lower        Upper       
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Apparent prevalence               57.50000     49.44874     65.26819   
#>    True prevalence                   65.00000     57.07166     72.36120   
#>    Test sensitivity                  79.80769     70.80542     87.04455   
#>    Test specificity                  83.92857     71.67203     92.37813   
#>    Diagnostic accuracy               81.25000     74.32805     86.97742   
#>    Positive predictive value         90.21739     82.24035     95.42839   
#>    Negative predictive value         69.11765     56.74392     79.76365   
#>    Proportion of false positives     16.07143      7.62187     28.32797   
#>    Proportion of false negative      20.19231     12.95545     29.19458   
#>    False Discovery Rate               9.78261      4.57161     17.75965   
#>    False Omission Rate               30.88235     20.23635     43.25608   
#>  ──────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test 2 - Recoded Data                                            
#>  ──────────────────────────────────────────────────────────────── 
#>                     Gold Positive    Gold Negative    Total       
#>  ──────────────────────────────────────────────────────────────── 
#>    Test Positive         86.00000         11.00000     97.00000   
#>    Test Negative         18.00000         45.00000     63.00000   
#>    Total                104.00000         56.00000    160.00000   
#>  ──────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test 2 - Confidence Intervals                                            
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Decision Statistics              Estimate     Lower        Upper       
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Apparent prevalence               60.62500     52.60254     68.24621   
#>    True prevalence                   65.00000     57.07166     72.36120   
#>    Test sensitivity                  82.69231     74.03265     89.40758   
#>    Test specificity                  80.35714     67.56670     89.76517   
#>    Diagnostic accuracy               81.87500     75.01803     87.51260   
#>    Positive predictive value         88.65979     80.61461     94.20113   
#>    Negative predictive value         71.42857     58.65390     82.10708   
#>    Proportion of false positives     19.64286     10.23483     32.43330   
#>    Proportion of false negative      17.30769     10.59242     25.96735   
#>    False Discovery Rate              11.34021      5.79887     19.38539   
#>    False Omission Rate               28.57143     17.89292     41.34610   
#>  ──────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Decision Test Comparison                                                                                                                                                                                                                
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test                                                                    Sensitivity    Specificity    Accuracy     Positive Predictive Value    Negative Predictive Value    Positive Likelihood Ratio    Negative Likelihood Ratio   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test1                                                                      79.80769       83.92857     81.25000                     90.21739                     69.11765                     4.965812                    0.2405892   
#>      → Limited diagnostic utility - consider combining with other tests                                                                                                                                                                  
#>    Test2                                                                      82.69231       80.35714     81.87500                     88.65979                     71.42857                     4.209790                    0.2153846   
#>      → Limited diagnostic utility - consider combining with other tests                                                                                                                                                                  
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  <div style="font-family: Arial, sans-serif; max-width: 800px; margin:
#>  0 auto; padding: 20px;"><h2 style="color: #2c3e50; border-bottom: 2px
#>  solid #3498db;"> Clinical Summary
#> 
#>  Among the tests evaluated, Test1 demonstrated optimal diagnostic
#>  performance, with 79.8% sensitivity (95% CI: 70.8-87.0%), 83.9%
#>  specificity (95% CI: 71.7-92.4%), 90.2% positive predictive value,
#>  69.1% negative predictive value, and 81.2% overall accuracy. The
#>  likelihood ratio for positive results was 4.97 and for negative
#>  results was 0.24.<h3 style="color: #27ae60; margin-top: 30px;"> Report
#>  Sentences
#> 
#>  <div style="background-color: #f8f9fa; padding: 15px; border-left: 4px
#>  solid #28a745; margin: 15px 0;"><h4 style="margin-top: 0;">Methods
#>  Section:
#> 
#>  <p style="font-style: italic; line-height: 1.6;">We compared the
#>  diagnostic performance of 2 tests (Test1, Test2) against the gold
#>  standard reference using diagnostic accuracy analysis. The study
#>  included 160 cases with complete data. Performance metrics calculated
#>  included sensitivity, specificity, positive and negative predictive
#>  values, likelihood ratios, and overall accuracy.
#> 
#>  <div style="background-color: #e8f4f8; padding: 15px; border-left: 4px
#>  solid #3498db; margin: 15px 0;"><h4 style="margin-top: 0;">Results
#>  Section:
#> 
#>  <p style="font-style: italic; line-height: 1.6;">Among the tests
#>  evaluated, Test1 demonstrated optimal diagnostic performance, with
#>  79.8% sensitivity (95% CI: 70.8-87.0%), 83.9% specificity (95% CI:
#>  71.7-92.4%), 90.2% positive predictive value, 69.1% negative
#>  predictive value, and 81.2% overall accuracy. The likelihood ratio for
#>  positive results was 4.97 and for negative results was 0.24.
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
#>  <span style='color: #374151;'>2 diagnostic tests compared using 160
#>  complete cases. Gold standard identified 104 diseased and 56 healthy
#>  cases. Review comparison tables and statistical tests below.
```
