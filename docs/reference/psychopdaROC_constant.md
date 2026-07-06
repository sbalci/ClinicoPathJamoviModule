# psychopdaROC Constant Predictor Data

Dataset with 80 patients where the biomarker has no variation (constant
value), testing handling of predictors with zero variance.

## Usage

``` r
psychopdaROC_constant
```

## Format

A data frame with 80 rows and 3 variables:

- patient_id:

  Character: Patient identifier (PT001-PT080)

- outcome:

  Factor: "Positive" or "Negative" (50%/50% prevalence)

- constant_marker:

  Numeric: Constant value of 50 for all patients

## Source

Generated test data for ClinicoPath package

## Details

All patients have identical biomarker value (50). Tests error handling
for zero-variance predictors that cannot produce ROC curves.

## Examples

``` r
data(psychopdaROC_constant)
psychopdaROC(data = psychopdaROC_constant, dependentVars = "constant_marker",
             classVar = "outcome", positiveClass = "Positive",
             refVar = "constant_marker")
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
#>  Measure Variable(s): constant_marker
#> 
#>  Class Variable: outcome
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
#>  Seed: 123Positive Class: Positive (Prevalence: 61.3%)Analysis Mode:
#>  Basic
#> 
#>  ROC Analysis Summary                                                          
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Variable           AUC          95% CI Lower    95% CI Upper    p-value     
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    constant_marker    0.5000000       0.5000000       0.5000000                
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong method.
#> 
#> 
#>  Clinical Interpretation                                                                                                                                                                                                                       
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test               Performance Level    Clinical Recommendation                            Detailed Interpretation                                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    constant_marker    Poor                 Not recommended as standalone diagnostic marker    The test 'constant_marker' has an AUC of 0.500 indicating poor discriminatory ability. Alternative diagnostic approaches should be considered.   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  OPTIMAL CUTPOINTS AND PERFORMANCE
#> 
#>  no title                                                                                                         
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint     Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>          Inf      100.00000        0.00000     61.25000      0.00000     0.0000000    0.5000000       0.0000000   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                      
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Variable           AUC          95% CI Lower    95% CI Upper    p-value     
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    constant_marker    0.5000000       0.5000000       0.5000000                
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong method.
#> 

```
