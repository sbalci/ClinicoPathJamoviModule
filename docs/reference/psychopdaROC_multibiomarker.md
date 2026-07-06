# psychopdaROC Multi-Biomarker Data

Multiple biomarker comparison dataset with 220 patients featuring three
individual markers and a combined score for ROC analysis and marker
comparison.

## Usage

``` r
psychopdaROC_multibiomarker
```

## Format

A data frame with 220 rows and 6 variables:

- patient_id:

  Character: Patient identifier (PT001-PT220)

- diagnosis:

  Factor: "Positive" or "Negative" (35%/65% prevalence)

- marker1:

  Numeric: First biomarker (mean: 100 for positive, 70 for negative)

- marker2:

  Numeric: Second biomarker (mean: 85 for positive, 55 for negative)

- marker3:

  Numeric: Third biomarker (mean: 90 for positive, 65 for negative)

- combined_score:

  Numeric: Average of three markers

## Source

Generated test data for ClinicoPath package

## Details

Designed for comparing individual biomarker performance and evaluating
combined marker strategies. The combined score typically shows improved
discrimination compared to individual markers.

## Examples

``` r
data(psychopdaROC_multibiomarker)
psychopdaROC(data = psychopdaROC_multibiomarker,
             dependentVars = c("marker1", "marker2", "marker3", "combined_score"),
             classVar = "diagnosis", positiveClass = "Positive",
             refVar = "marker1",
             clinicalMode = "comprehensive")
#> Multiple optimal cutpoints found, applying break_ties.
#> Multiple optimal cutpoints found, applying break_ties.
#> Multiple optimal cutpoints found, applying break_ties.
#> Multiple optimal cutpoints found, applying break_ties.
#> 
#>  ADVANCED ROC ANALYSIS
#> 
#> 
#> 
#> 
#>  Procedure Notes
#> 
#> 
#> 
#>  The ROC analysis has been completed using the following
#>  specifications:
#> 
#>  &nbsp;
#> 
#>  Measure Variable(s): marker1, marker2, marker3, combined_score
#> 
#>  Class Variable: diagnosis
#> 
#>  Positive Class: Positive
#> 
#>  &nbsp;
#> 
#>  Method: maximize_metric
#> 
#>  All Observed Cutpoints: FALSE
#> 
#>  Metric: youden
#> 
#>  Direction (relative to cutpoint): >=
#> 
#>  Tie Breakers: mean
#> 
#>  Metric Tolerance: 0.05
#> 
#>  &nbsp;
#> 
#>  <hr />
#> 
#>  <div style='padding: 10px; background-color: #f8f9fa; border: 1px
#>  solid #dee2e6; border-radius: 4px; margin-bottom: 15px;'>
#> 
#>  Analysis Status
#> 
#>  Seed: 123Positive Class: Positive (Prevalence: 33.2%)Analysis Mode:
#>  Comprehensive
#> 
#>  ROC Analysis Summary                                                          
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Variable          AUC          95% CI Lower    95% CI Upper    p-value      
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    marker1           0.8214519       0.7624193       0.8804845    < .0000001   
#>    marker2           0.8895723       0.8429232       0.9362213    < .0000001   
#>    marker3           0.8276023       0.7722325       0.8829720    < .0000001   
#>    combined_score    0.9729755       0.9516340       0.9943170    < .0000001   
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong method.
#> 
#> 
#>  Clinical Interpretation                                                                                                                                                                                                                                                
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test              Performance Level    Clinical Recommendation                                Detailed Interpretation                                                                                                                                                
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    marker1           Good                 Suitable for clinical use with appropriate cutpoint    The test 'marker1' has an AUC of 0.821 indicating good discriminatory ability. This test performs well for clinical decision making.                                   
#>    marker2           Good                 Suitable for clinical use with appropriate cutpoint    The test 'marker2' has an AUC of 0.890 indicating good discriminatory ability. This test performs well for clinical decision making.                                   
#>    marker3           Good                 Suitable for clinical use with appropriate cutpoint    The test 'marker3' has an AUC of 0.828 indicating good discriminatory ability. This test performs well for clinical decision making.                                   
#>    combined_score    Excellent            Suitable for clinical use with appropriate cutpoint    The test 'combined_score' has an AUC of 0.973 indicating excellent discriminatory ability. This test can reliably distinguish between diseased and healthy patients.   
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  OPTIMAL CUTPOINTS AND PERFORMANCE
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    87.2496589       78.08219       77.55102     63.33333     87.69231     0.5563321    0.8214519       0.5563321   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    72.2080895       79.45205       82.31293     69.04762     88.97059     0.6176498    0.8895723       0.6176498   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    75.9021153       82.19178       75.51020     62.50000     89.51613     0.5770198    0.8276023       0.5770198   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    78.7908214       87.67123       93.19728     86.48649     93.83562     0.8086851    0.9729755       0.8086851   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                      
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Variable          AUC          95% CI Lower    95% CI Upper    p-value      
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    marker1           0.8214519       0.7624193       0.8804845    < .0000001   
#>    marker2           0.8895723       0.8429232       0.9362213    < .0000001   
#>    marker3           0.8276023       0.7722325       0.8829720    < .0000001   
#>    combined_score    0.9729755       0.9516340       0.9943170    < .0000001   
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong method.
#> 

```
