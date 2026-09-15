# psychopdaROC Poor Discrimination Data

Dataset with 150 patients showing no discrimination between case and
control groups (AUC ~0.50), useful for testing handling of ineffective
biomarkers.

## Usage

``` r
psychopdaROC_poor
```

## Format

A data frame with 150 rows and 3 variables:

- patient_id:

  Character: Patient identifier (PT001-PT150)

- status:

  Factor: "Case" or "Control" (50%/50% prevalence)

- poor_marker:

  Numeric: Biomarker with no discriminatory value (mean 50, SD 15)

## Source

Generated test data for ClinicoPath package

## Details

Both cases and controls have identical distributions (normal, mean=50,
SD=15). Tests proper handling and warning messages for biomarkers with
no diagnostic value.

## Examples

``` r
data(psychopdaROC_poor)
psychopdaROC(data = psychopdaROC_poor, dependentVars = "poor_marker",
             classVar = "status", positiveClass = "Case",
             refVar = "poor_marker")
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
#>   
#> 
#>  Measure Variable(s): poor_marker
#> 
#>  Class Variable: status
#> 
#>  Positive Class: Case
#> 
#>   
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
#>  Metric Tolerance: 1e-06
#> 
#>   
#> 
#>  <hr />
#> 
#>  <div style='padding: 10px; background-color: #f8f9fa; border: 1px
#>  solid #dee2e6; border-radius: 4px; margin-bottom: 15px;'>
#> 
#>  Analysis Status
#> 
#>  Seed: 123Positive Class: Case (Prevalence: 43.3%)Analysis Mode: Basic
#> 
#>  ROC Analysis Summary                                                      
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Variable       AUC          95% CI Lower    95% CI Upper    p-value     
#>  ───────────────────────────────────────────────────────────────────────── 
#>    poor_marker    0.4761991       0.3826024       0.5697958    0.6181997   
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Note. Reading of the test values: <b>HIGHER values were taken to
#>    indicate Case</b> (Classification Direction = ">="). If that is the
#>    wrong way round for this marker, every sensitivity, specificity,
#>    cutpoint and AUC below is reversed — switch Classification
#>    Direction to "<=" and the AUC becomes 1 minus the value shown.
#>    Note. WARNING: AUC below 0.5 (worse than chance) for: poor_marker.
#>    An AUC below 0.5 almost always means the marker is being read the
#>    wrong way round rather than that it is useless: it separates the
#>    groups, but in the opposite direction to the one assumed.
#>    Classification Direction is currently "&gt;="; switching it to
#>    "&lt;=" will give an AUC of 1 minus the value shown, with
#>    sensitivity and specificity swapped accordingly. Change it only if
#>    that matches what the marker means clinically.
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 
#> 
#>  Clinical Interpretation                                                                                                                                                                                                               
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test           Performance Level    Clinical Recommendation                            Detailed Interpretation                                                                                                                      
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    poor_marker    Poor                 Not recommended as standalone diagnostic marker    The test 'poor_marker' has an AUC of 0.476 indicating poor discriminatory ability. Alternative diagnostic approaches should be considered.   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  OPTIMAL CUTPOINTS AND PERFORMANCE
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    22.9658885      100.00000        5.88235     44.82759    100.00000     0.0588235    0.4761991       0.0588235   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                  
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Variable       AUC          95% CI Lower    95% CI Upper    p-value     
#>  ───────────────────────────────────────────────────────────────────────── 
#>    poor_marker    0.4761991       0.3826024       0.5697958    0.6181997   
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Note. Reading of the test values: <b>HIGHER values were taken to
#>    indicate Case</b> (Classification Direction = ">="). If that is the
#>    wrong way round for this marker, every sensitivity, specificity,
#>    cutpoint and AUC below is reversed — switch Classification
#>    Direction to "<=" and the AUC becomes 1 minus the value shown.
#>    Note. WARNING: AUC below 0.5 (worse than chance) for: poor_marker.
#>    An AUC below 0.5 almost always means the marker is being read the
#>    wrong way round rather than that it is useless: it separates the
#>    groups, but in the opposite direction to the one assumed.
#>    Classification Direction is currently "&gt;="; switching it to
#>    "&lt;=" will give an AUC of 1 minus the value shown, with
#>    sensitivity and specificity swapped accordingly. Change it only if
#>    that matches what the marker means clinically.
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 

```
