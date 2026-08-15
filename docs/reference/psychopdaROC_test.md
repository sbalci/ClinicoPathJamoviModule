# psychopdaROC Test Data - Basic Diagnostic Test

Basic diagnostic test dataset with 200 patients for ROC analysis.
Contains binary disease status and a continuous biomarker with moderate
discrimination (AUC ~0.75-0.80).

## Usage

``` r
psychopdaROC_test
```

## Format

A data frame with 200 rows and 5 variables:

- patient_id:

  Character: Patient identifier (PT001-PT200)

- disease_status:

  Factor: "Disease" or "Healthy" (30%/70% prevalence)

- biomarker:

  Numeric: Continuous biomarker value (mean: 75 for diseased, 50 for
  healthy)

- age:

  Numeric: Patient age in years (mean 60, SD 12)

- sex:

  Factor: "Male" or "Female"

## Source

Generated test data for ClinicoPath package

## Details

Biomarker values follow normal distributions with clear separation
between disease groups, suitable for demonstrating basic ROC curve
analysis and optimal cutpoint determination using Youden index or other
metrics.

## Examples

``` r
data(psychopdaROC_test)
psychopdaROC(data = psychopdaROC_test, dependentVars = "biomarker",
             classVar = "disease_status", positiveClass = "Disease",
             refVar = "biomarker")
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
#>  Measure Variable(s): biomarker
#> 
#>  Class Variable: disease_status
#> 
#>  Positive Class: Disease
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
#>  Seed: 123Positive Class: Disease (Prevalence: 35.5%)Analysis Mode:
#>  Basic
#> 
#>  ROC Analysis Summary                                                     
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Variable     AUC          95% CI Lower    95% CI Upper    p-value      
#>  ──────────────────────────────────────────────────────────────────────── 
#>    biomarker    0.8998799       0.8585411       0.9412187    < .0000001   
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Note. Reading of the test values: <b>HIGHER values were taken to
#>    indicate Disease</b> (Classification Direction = ">="). If that is
#>    the wrong way round for this marker, every sensitivity,
#>    specificity, cutpoint and AUC below is reversed — switch
#>    Classification Direction to "<=" and the AUC becomes 1 minus the
#>    value shown.
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 
#> 
#>  Clinical Interpretation                                                                                                                                                                                                             
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test         Performance Level    Clinical Recommendation                                Detailed Interpretation                                                                                                                  
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    biomarker    Good                 Suitable for clinical use with appropriate cutpoint    The test 'biomarker' has an AUC of 0.900 indicating good discriminatory ability. This test performs well for clinical decision making.   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  OPTIMAL CUTPOINTS AND PERFORMANCE
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    54.2904443       94.36620       68.99225     62.61682     95.69892     0.6335845    0.8998799       0.6335845   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                 
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Variable     AUC          95% CI Lower    95% CI Upper    p-value      
#>  ──────────────────────────────────────────────────────────────────────── 
#>    biomarker    0.8998799       0.8585411       0.9412187    < .0000001   
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Note. Reading of the test values: <b>HIGHER values were taken to
#>    indicate Disease</b> (Classification Direction = ">="). If that is
#>    the wrong way round for this marker, every sensitivity,
#>    specificity, cutpoint and AUC below is reversed — switch
#>    Classification Direction to "<=" and the AUC becomes 1 minus the
#>    value shown.
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 

```
