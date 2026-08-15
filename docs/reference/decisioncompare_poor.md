# decisioncompare Poor Performance Data

Dataset with 140 patients featuring two poorly performing tests (Sens:
0.60/0.55, Spec: 0.65/0.70).

## Usage

``` r
decisioncompare_poor
```

## Format

A data frame with 140 rows and 4 variables:

- patient_id:

  Character: Patient identifier (PT001-PT140)

- GoldStandard:

  Factor: True status ("Negative", "Positive"), 30% positive

- PoorTest1:

  Factor: First poor test ("Negative", "Positive"), Sens=0.60, Spec=0.65

- PoorTest2:

  Factor: Second poor test ("Negative", "Positive"), Sens=0.55,
  Spec=0.70

- age:

  Numeric: Patient age in years (mean 57, SD 12)

## Source

Generated test data for ClinicoPath package

## Details

Tests with low accuracy (barely better than chance). Demonstrates
handling of poorly performing diagnostic tests.

## Examples

``` r
data(decisioncompare_poor)
decisioncompare(data = decisioncompare_poor, gold = "GoldStandard",
                goldPositive = "Positive", goldNegative = NULL, test1 = "PoorTest1",
                test1Positive = "Positive", test1Negative = NULL, test2 = "PoorTest2",
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
#>    Test Positive         33.00000         27.00000     60.00000   
#>    Test Negative         18.00000         62.00000     80.00000   
#>    Total                 51.00000         89.00000    140.00000   
#>  ──────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test 1 - Confidence Intervals                                            
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Decision Statistics              Estimate     Lower        Upper       
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Apparent prevalence               42.85714     34.53395     51.48906   
#>    True prevalence                   36.42857     28.46665     44.97790   
#>    Test sensitivity                  64.70588     50.06820     77.56938   
#>    Test specificity                  69.66292     59.00841     78.96506   
#>    Diagnostic accuracy               67.85714     59.44545     75.49469   
#>    Positive predictive value         55.00000     41.61191     67.87785   
#>    Negative predictive value         77.50000     66.79137     86.08628   
#>    Proportion of false positives     30.33708     21.03494     40.99159   
#>    Proportion of false negative      35.29412     22.43062     49.93180   
#>    False Discovery Rate              45.00000     32.12215     58.38809   
#>    False Omission Rate               22.50000     13.91372     33.20863   
#>  ──────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test 2 - Recoded Data                                            
#>  ──────────────────────────────────────────────────────────────── 
#>                     Gold Positive    Gold Negative    Total       
#>  ──────────────────────────────────────────────────────────────── 
#>    Test Positive         30.00000         32.00000     62.00000   
#>    Test Negative         21.00000         57.00000     78.00000   
#>    Total                 51.00000         89.00000    140.00000   
#>  ──────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Test 2 - Confidence Intervals                                            
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Decision Statistics              Estimate     Lower        Upper       
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Apparent prevalence               44.28571     35.90149     52.91687   
#>    True prevalence                   36.42857     28.46665     44.97790   
#>    Test sensitivity                  58.82353     44.16928     72.41570   
#>    Test specificity                  64.04494     53.17822     73.94836   
#>    Diagnostic accuracy               62.14286     53.56271     70.19764   
#>    Positive predictive value         48.38710     35.49728     61.43609   
#>    Negative predictive value         73.07692     61.83560     82.49875   
#>    Proportion of false positives     35.95506     26.05164     46.82178   
#>    Proportion of false negative      41.17647     27.58430     55.83072   
#>    False Discovery Rate              51.61290     38.56391     64.50272   
#>    False Omission Rate               26.92308     17.50125     38.16440   
#>  ──────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Decision Test Comparison                                                                                                                                                                                                                
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test                                                                    Sensitivity    Specificity    Accuracy     Positive Predictive Value    Negative Predictive Value    Positive Likelihood Ratio    Negative Likelihood Ratio   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    PoorTest1                                                                  64.70588       69.66292     67.85714                     55.00000                     77.50000                     2.132898                    0.5066414   
#>      → Limited diagnostic utility - consider combining with other tests                                                                                                                                                                  
#>    PoorTest2                                                                  58.82353       64.04494     62.14286                     48.38710                     73.07692                     1.636029                    0.6429309   
#>      → Limited diagnostic utility - consider combining with other tests                                                                                                                                                                  
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  <div style="font-family: Arial, sans-serif; max-width: 800px; margin:
#>  0 auto; padding: 20px;"><h2 style="color: #2c3e50; border-bottom: 2px
#>  solid #3498db;"> Clinical Summary
#> 
#>  Among the tests evaluated, PoorTest1 demonstrated optimal diagnostic
#>  performance, with 64.7% sensitivity (95% CI: 50.1-77.6%), 69.7%
#>  specificity (95% CI: 59.0-79.0%), 55% positive predictive value, 77.5%
#>  negative predictive value, and 67.9% overall accuracy. The likelihood
#>  ratio for positive results was 2.13 and for negative results was
#>  0.51.<h3 style="color: #27ae60; margin-top: 30px;"> Report Sentences
#> 
#>  <div style="background-color: #f8f9fa; padding: 15px; border-left: 4px
#>  solid #28a745; margin: 15px 0;"><h4 style="margin-top: 0;">Methods
#>  Section:
#> 
#>  <p style="font-style: italic; line-height: 1.6;">We compared the
#>  diagnostic performance of 2 tests (PoorTest1, PoorTest2) against the
#>  gold standard reference using diagnostic accuracy analysis. The study
#>  included 140 cases with complete data. Performance metrics calculated
#>  included sensitivity, specificity, positive and negative predictive
#>  values, likelihood ratios, and overall accuracy.
#> 
#>  <div style="background-color: #e8f4f8; padding: 15px; border-left: 4px
#>  solid #3498db; margin: 15px 0;"><h4 style="margin-top: 0;">Results
#>  Section:
#> 
#>  <p style="font-style: italic; line-height: 1.6;">Among the tests
#>  evaluated, PoorTest1 demonstrated optimal diagnostic performance, with
#>  64.7% sensitivity (95% CI: 50.1-77.6%), 69.7% specificity (95% CI:
#>  59.0-79.0%), 55% positive predictive value, 77.5% negative predictive
#>  value, and 67.9% overall accuracy. The likelihood ratio for positive
#>  results was 2.13 and for negative results was 0.51.
#> 
#>  <h3 style="color: #8e44ad; margin-top: 30px;"> Clinical
#>  Recommendations
#> 
#>  <div style="background-color: #fff3cd; padding: 15px; border-radius:
#>  8px;">
#> 
#>  Clinical Consideration: Consider using PoorTest1 in combination with
#>  other tests for optimal diagnostic accuracy.
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
#>  <span style='color: #374151;'>2 diagnostic tests compared using 140
#>  complete cases. Gold standard identified 51 diseased and 89 healthy
#>  cases. Review comparison tables and statistical tests below.
```
