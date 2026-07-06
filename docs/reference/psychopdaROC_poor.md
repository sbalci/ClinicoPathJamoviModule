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
#>  Measure Variable(s): poor_marker
#> 
#>  Class Variable: status
#> 
#>  Positive Class: Case
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
#>  Seed: 123Positive Class: Case (Prevalence: 43.3%)Analysis Mode: Basic
#> 
#>  ROC Analysis Summary                                                      
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Variable       AUC          95% CI Lower    95% CI Upper    p-value     
#>  ───────────────────────────────────────────────────────────────────────── 
#>    poor_marker    0.4761991       0.3826024       0.5697958    0.6181997   
#>  ───────────────────────────────────────────────────────────────────────── 
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
#>    35.5500932       83.07692       16.47059     43.20000     56.00000    -0.0045249    0.4761991      -0.0045249   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                  
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Variable       AUC          95% CI Lower    95% CI Upper    p-value     
#>  ───────────────────────────────────────────────────────────────────────── 
#>    poor_marker    0.4761991       0.3826024       0.5697958    0.6181997   
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 

```
