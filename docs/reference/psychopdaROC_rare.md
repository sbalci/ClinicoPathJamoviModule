# psychopdaROC Rare Disease Data

Dataset with 300 patients and very low disease prevalence (5%),
representing rare disease screening scenarios where prevalence affects
predictive values.

## Usage

``` r
psychopdaROC_rare
```

## Format

A data frame with 300 rows and 3 variables:

- patient_id:

  Character: Patient identifier (PT001-PT300)

- rare_disease:

  Factor: "Disease" or "No_Disease" (5%/95% prevalence)

- biomarker:

  Numeric: Biomarker value (mean: 80 for disease, 45 for no disease)

## Source

Generated test data for ClinicoPath package

## Details

Low prevalence (5%) with good biomarker discrimination. Demonstrates
impact of prevalence on positive and negative predictive values even
with good sensitivity and specificity.

## Examples

``` r
data(psychopdaROC_rare)
psychopdaROC(data = psychopdaROC_rare, dependentVars = "biomarker",
             classVar = "rare_disease", positiveClass = "Disease",
             refVar = "biomarker")
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
#>  Measure Variable(s): biomarker
#> 
#>  Class Variable: rare_disease
#> 
#>  Positive Class: Disease
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
#>  Seed: 123Positive Class: Disease (Prevalence: 4.3%)Analysis Mode:
#>  Basic<div style='background-color: #fff3cd; color: #856404; padding:
#>  10px; border-radius: 4px; margin-top: 10px;'>Warnings:Class imbalance
#>  detected (Prevalence: 4.3%). Consider using Precision-Recall curves.
#> 
#>  ROC Analysis Summary                                                     
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Variable     AUC          95% CI Lower    95% CI Upper    p-value      
#>  ──────────────────────────────────────────────────────────────────────── 
#>    biomarker    0.9790941       0.9506579       1.0000000    < .0000001   
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 
#> 
#>  Clinical Interpretation                                                                                                                                                                                                                                      
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test         Performance Level    Clinical Recommendation                                Detailed Interpretation                                                                                                                                           
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    biomarker    Excellent            Suitable for clinical use with appropriate cutpoint    The test 'biomarker' has an AUC of 0.979 indicating excellent discriminatory ability. This test can reliably distinguish between diseased and healthy patients.   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  OPTIMAL CUTPOINTS AND PERFORMANCE
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    66.6279881       92.30769       93.03136     37.50000     99.62687     0.8533905    0.9790941       0.8533905   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                 
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Variable     AUC          95% CI Lower    95% CI Upper    p-value      
#>  ──────────────────────────────────────────────────────────────────────── 
#>    biomarker    0.9790941       0.9506579       1.0000000    < .0000001   
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 

```
