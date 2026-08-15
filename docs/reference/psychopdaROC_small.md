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
#>  Measure Variable(s): marker
#> 
#>  Class Variable: class
#> 
#>  Positive Class: Positive
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
#>  Seed: 123Positive Class: Positive (Prevalence: 70%)Analysis Mode:
#>  Basic
#> 
#>  ROC Analysis Summary                                                    
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Variable    AUC          95% CI Lower    95% CI Upper    p-value      
#>  ─────────────────────────────────────────────────────────────────────── 
#>    marker      0.8835979       0.7541782       1.0000000    < .0000001   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Note. Reading of the test values: <b>HIGHER values were taken to
#>    indicate Positive</b> (Classification Direction = ">="). If that
#>    is the wrong way round for this marker, every sensitivity,
#>    specificity, cutpoint and AUC below is reversed — switch
#>    Classification Direction to "<=" and the AUC becomes 1 minus the
#>    value shown.
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
#>    53.3094338       85.71429       77.77778     90.00000     70.00000     0.6349206    0.8835979       0.6349206   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Variable    AUC          95% CI Lower    95% CI Upper    p-value      
#>  ─────────────────────────────────────────────────────────────────────── 
#>    marker      0.8835979       0.7541782       1.0000000    < .0000001   
#>  ─────────────────────────────────────────────────────────────────────── 
#>    Note. Reading of the test values: <b>HIGHER values were taken to
#>    indicate Positive</b> (Classification Direction = ">="). If that
#>    is the wrong way round for this marker, every sensitivity,
#>    specificity, cutpoint and AUC below is reversed — switch
#>    Classification Direction to "<=" and the AUC becomes 1 minus the
#>    value shown.
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 

```
