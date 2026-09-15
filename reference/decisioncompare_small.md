# decisioncompare Small Sample Data

Small dataset with only 30 patients for testing performance with limited
sample sizes.

## Usage

``` r
decisioncompare_small
```

## Format

A data frame with 30 rows and 4 variables:

- patient_id:

  Character: Patient identifier (PT001-PT030)

- GoldStandard:

  Factor: True status ("Negative", "Positive"), 30% positive

- Test1:

  Factor: First test ("Negative", "Positive"), Sens=0.85, Spec=0.88

- Test2:

  Factor: Second test ("Negative", "Positive"), Sens=0.80, Spec=0.85

- age:

  Numeric: Patient age in years (mean 55, SD 10)

## Source

Generated test data for ClinicoPath package

## Details

Small sample (n=30) tests stability of estimates and wide confidence
intervals.

## Examples

``` r
data(decisioncompare_small)
decisioncompare(data = decisioncompare_small, gold = "GoldStandard",
                goldPositive = "Positive", goldNegative = NULL, test1 = "Test1",
                test1Positive = "Positive", test1Negative = NULL, test2 = "Test2",
                test2Positive = "Positive", test2Negative = NULL,
                test3Positive = "", test3Negative = NULL,
                ci = TRUE)
#> 
#>  COMPARE MEDICAL DECISION TESTS
#> 
#>  Test 1 - Recoded Data                                           
#>  ─────────────────────────────────────────────────────────────── 
#>                     Gold Positive    Gold Negative    Total      
#>  ─────────────────────────────────────────────────────────────── 
#>    Test Positive         9.000000         3.000000    12.00000   
#>    Test Negative         3.000000        15.000000    18.00000   
#>    Total                12.000000        18.000000    30.00000   
#>  ─────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test 1 - Confidence Intervals                                            
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Decision Statistics              Estimate     Lower        Upper       
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Apparent prevalence               40.00000     22.65576     59.39651   
#>    True prevalence                   40.00000     22.65576     59.39651   
#>    Test sensitivity                  75.00000     42.81415     94.51394   
#>    Test specificity                  83.33333     58.58225     96.42149   
#>    Diagnostic accuracy               80.00000     61.43335     92.28645   
#>    Positive predictive value         75.00000     42.81415     94.51394   
#>    Negative predictive value         83.33333     58.58225     96.42149   
#>    Proportion of false positives     16.66667      3.57851     41.41775   
#>    Proportion of false negative      25.00000      5.48606     57.18585   
#>    False Discovery Rate              25.00000      5.48606     57.18585   
#>    False Omission Rate               16.66667      3.57851     41.41775   
#>  ──────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test 2 - Recoded Data                                           
#>  ─────────────────────────────────────────────────────────────── 
#>                     Gold Positive    Gold Negative    Total      
#>  ─────────────────────────────────────────────────────────────── 
#>    Test Positive         7.000000         4.000000    11.00000   
#>    Test Negative         5.000000        14.000000    19.00000   
#>    Total                12.000000        18.000000    30.00000   
#>  ─────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test 2 - Confidence Intervals                                            
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Decision Statistics              Estimate     Lower        Upper       
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Apparent prevalence               36.66667     19.92986     56.14402   
#>    True prevalence                   40.00000     22.65576     59.39651   
#>    Test sensitivity                  58.33333     27.66697     84.83478   
#>    Test specificity                  77.77778     52.36272     93.59080   
#>    Diagnostic accuracy               70.00000     50.60410     85.26548   
#>    Positive predictive value         63.63636     30.79047     89.07366   
#>    Negative predictive value         73.68421     48.79707     90.85342   
#>    Proportion of false positives     22.22222      6.40920     47.63728   
#>    Proportion of false negative      41.66667     15.16522     72.33303   
#>    False Discovery Rate              36.36364     10.92634     69.20953   
#>    False Omission Rate               26.31579      9.14658     51.20293   
#>  ──────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Decision Test Comparison                                                                                                                                                                                                                
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test                                                                    Sensitivity    Specificity    Accuracy     Positive Predictive Value    Negative Predictive Value    Positive Likelihood Ratio    Negative Likelihood Ratio   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test1                                                                      75.00000       83.33333     80.00000                     75.00000                     83.33333                     4.500000                    0.3000000   
#>      → Limited diagnostic utility - consider combining with other tests                                                                                                                                                                  
#>    Test2                                                                      58.33333       77.77778     70.00000                     63.63636                     73.68421                     2.625000                    0.5357143   
#>      → Limited diagnostic utility - consider combining with other tests                                                                                                                                                                  
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  <div style="font-family: Arial, sans-serif; max-width: 800px; margin:
#>  0 auto; padding: 20px;"><h2 style="color: #2c3e50; border-bottom: 2px
#>  solid #3498db;"> Clinical Summary
#> 
#>  Among the tests evaluated, Test1 demonstrated optimal diagnostic
#>  performance, with 75% sensitivity (95% CI: 42.8-94.5%), 83.3%
#>  specificity (95% CI: 58.6-96.4%), 75% positive predictive value, 83.3%
#>  negative predictive value, and 80% overall accuracy. The likelihood
#>  ratio for positive results was 4.50 and for negative results was
#>  0.30.<h3 style="color: #27ae60; margin-top: 30px;"> Report Sentences
#> 
#>  <div style="background-color: #f8f9fa; padding: 15px; border-left: 4px
#>  solid #28a745; margin: 15px 0;"><h4 style="margin-top: 0;">Methods
#>  Section:
#> 
#>  <p style="font-style: italic; line-height: 1.6;">We compared the
#>  diagnostic performance of 2 tests (Test1, Test2) against the gold
#>  standard reference using diagnostic accuracy analysis. The study
#>  included 30 cases with complete data. Performance metrics calculated
#>  included sensitivity, specificity, positive and negative predictive
#>  values, likelihood ratios, and overall accuracy.
#> 
#>  <div style="background-color: #e8f4f8; padding: 15px; border-left: 4px
#>  solid #3498db; margin: 15px 0;"><h4 style="margin-top: 0;">Results
#>  Section:
#> 
#>  <p style="font-style: italic; line-height: 1.6;">Among the tests
#>  evaluated, Test1 demonstrated optimal diagnostic performance, with 75%
#>  sensitivity (95% CI: 42.8-94.5%), 83.3% specificity (95% CI:
#>  58.6-96.4%), 75% positive predictive value, 83.3% negative predictive
#>  value, and 80% overall accuracy. The likelihood ratio for positive
#>  results was 4.50 and for negative results was 0.30.
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
#>  <div style='margin: 10px 0;'><div style='background-color: #fefce8;
#>  border-left: 4px solid #fde047; padding: 12px; margin: 8px 0;
#>  border-radius: 4px;'><strong style='color: #ca8a04;'> Small Sample
#>  Size
#>  <span style='color: #374151;'>Small sample size (n=30). Confidence
#>  intervals may be wide and estimates unstable. Minimum recommended:
#>  n=50-100 for adequate precision in diagnostic accuracy studies.
#>  Consider collecting additional data for reliable assessment.<div
#>  style='background-color: #eff6ff; border-left: 4px solid #93c5fd;
#>  padding: 12px; margin: 8px 0; border-radius: 4px;'><strong
#>  style='color: #2563eb;'> Analysis Completed Successfully
#>  <span style='color: #374151;'>2 diagnostic tests compared using 30
#>  complete cases. Gold standard identified 12 diseased and 18 healthy
#>  cases. Review comparison tables and statistical tests below.
```
