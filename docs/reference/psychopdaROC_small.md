# psychopdaROC Small Sample Data

Small dataset with only 30 patients for testing performance with limited
sample sizes and assessing stability of cutpoint estimates.

## Usage

``` r
psychopdaROC_small
```

## Format

A data frame with 30 rows and 3 variables:

- patient_id:

  Character: Patient identifier (PT001-PT030)

- class:

  Factor: "Positive" or "Negative" (50%/50% prevalence)

- marker:

  Numeric: Biomarker value (mean: 65 for positive, 45 for negative)

## Source

Generated test data for ClinicoPath package

## Details

Limited sample size (n=30) tests stability of ROC analysis and cutpoint
determination with small datasets. Wide confidence intervals expected.

## Examples

``` r
data(psychopdaROC_small)
psychopdaROC(data = psychopdaROC_small, dependentVars = "marker",
             classVar = "class", positiveClass = "Positive",
             refVar = "marker")
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
#>  Measure Variable(s): marker
#> 
#>  Class Variable: class
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
#>  Seed: 123Positive Class: Positive (Prevalence: 70%)Analysis Mode:
#>  Basic
#> 
#>  ROC Analysis Summary                                                    
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Variable    AUC          95% CI Lower    95% CI Upper    p-value      
#>  ─────────────────────────────────────────────────────────────────────── 
#>    marker      0.8835979       0.7541782       1.0000000    < .0000001   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 
#> 
#>  Clinical Interpretation                                                                                                                                                                                                       
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test      Performance Level    Clinical Recommendation                                Detailed Interpretation                                                                                                               
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    marker    Good                 Suitable for clinical use with appropriate cutpoint    The test 'marker' has an AUC of 0.884 indicating good discriminatory ability. This test performs well for clinical decision making.   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  OPTIMAL CUTPOINTS AND PERFORMANCE
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    56.2681789       76.19048       77.77778     88.88889     58.33333     0.5396825    0.8835979       0.5396825   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Variable    AUC          95% CI Lower    95% CI Upper    p-value      
#>  ─────────────────────────────────────────────────────────────────────── 
#>    marker      0.8835979       0.7541782       1.0000000    < .0000001   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 

```
