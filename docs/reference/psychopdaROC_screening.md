# psychopdaROC Screening Data - Cancer Detection

Cancer screening dataset with 250 patients featuring multiple biomarkers
(PSA and CA125) for evaluating screening test performance with low
disease prevalence (15%).

## Usage

``` r
psychopdaROC_screening
```

## Format

A data frame with 250 rows and 6 variables:

- patient_id:

  Character: Patient identifier (PT001-PT250)

- cancer:

  Factor: "Cancer" or "No_Cancer" (15%/85% prevalence)

- psa_level:

  Numeric: PSA level (ng/mL), log-normal distribution

- ca125:

  Numeric: CA125 level (U/mL), higher in cancer cases

- age:

  Numeric: Patient age in years (mean 65, SD 10)

- risk_factors:

  Factor: "None", "Family_History", or "Multiple"

## Source

Generated test data for ClinicoPath package

## Details

Designed for evaluating screening test characteristics where high
sensitivity is prioritized. PSA levels are log-normally distributed
(median: 12 for cancer, 4 for no cancer). CA125 shows normal
distribution with higher values in cancer cases (mean: 65 vs 25).

## Examples

``` r
data(psychopdaROC_screening)
psychopdaROC(data = psychopdaROC_screening,
             dependentVars = c("psa_level", "ca125"),
             classVar = "cancer", positiveClass = "Cancer",
             refVar = "psa_level",
             clinicalPreset = "screening")
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
#>  Measure Variable(s): psa_level, ca125
#> 
#>  Class Variable: cancer
#> 
#>  Positive Class: Cancer
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
#>  Seed: 123Positive Class: Cancer (Prevalence: 13.2%)Analysis Mode:
#>  Basic
#> 
#>  ROC Analysis Summary                                                     
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Variable     AUC          95% CI Lower    95% CI Upper    p-value      
#>  ──────────────────────────────────────────────────────────────────────── 
#>    psa_level    0.8762044       0.8174093       0.9349996    < .0000001   
#>    ca125        0.8883536       0.8199794       0.9567277    < .0000001   
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 
#> 
#>  Clinical Interpretation                                                                                                                                                                                                             
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test         Performance Level    Clinical Recommendation                                Detailed Interpretation                                                                                                                  
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    psa_level    Good                 Suitable for clinical use with appropriate cutpoint    The test 'psa_level' has an AUC of 0.876 indicating good discriminatory ability. This test performs well for clinical decision making.   
#>    ca125        Good                 Suitable for clinical use with appropriate cutpoint    The test 'ca125' has an AUC of 0.888 indicating good discriminatory ability. This test performs well for clinical decision making.       
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  OPTIMAL CUTPOINTS AND PERFORMANCE
#> 
#>  no title                                                                                                         
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint     Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    8.2015789       78.78788       80.64516     38.23529     96.15385     0.5943304    0.8762044       0.5943304   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    43.0666667       78.78788       85.71429     45.61404     96.37306     0.6450216    0.8883536       0.6450216   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                 
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Variable     AUC          95% CI Lower    95% CI Upper    p-value      
#>  ──────────────────────────────────────────────────────────────────────── 
#>    psa_level    0.8762044       0.8174093       0.9349996    < .0000001   
#>    ca125        0.8883536       0.8199794       0.9567277    < .0000001   
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 

```
