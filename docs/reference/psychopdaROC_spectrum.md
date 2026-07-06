# psychopdaROC Disease Spectrum Data

Dataset with 170 patients across a disease severity spectrum (Mild,
Moderate, Severe) collapsed into binary classification for ROC analysis.

## Usage

``` r
psychopdaROC_spectrum
```

## Format

A data frame with 170 rows and 4 variables:

- patient_id:

  Character: Patient identifier (PT001-PT170)

- severity:

  Factor: "Mild", "Moderate", or "Severe"

- binary_status:

  Factor: "Negative" (Mild) or "Positive" (Moderate/Severe)

- continuous_marker:

  Numeric: Marker increasing with severity (40/60/85)

## Source

Generated test data for ClinicoPath package

## Details

Represents continuous disease spectrum with graded marker values: Mild
(mean 40), Moderate (mean 60), Severe (mean 85). Binary classification
treats Mild as Negative, Moderate/Severe as Positive.

## Examples

``` r
data(psychopdaROC_spectrum)
psychopdaROC(data = psychopdaROC_spectrum, dependentVars = "continuous_marker",
             classVar = "binary_status", positiveClass = "Positive",
             refVar = "continuous_marker")
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
#>  Measure Variable(s): continuous_marker
#> 
#>  Class Variable: binary_status
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
#>  Seed: 123Positive Class: Positive (Prevalence: 67.6%)Analysis Mode:
#>  Basic
#> 
#>  ROC Analysis Summary                                                             
#>  ──────────────────────────────────────────────────────────────────────────────── 
#>    Variable             AUC          95% CI Lower    95% CI Upper    p-value      
#>  ──────────────────────────────────────────────────────────────────────────────── 
#>    continuous_marker    0.9549407       0.9258927       0.9839887    < .0000001   
#>  ──────────────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong method.
#> 
#> 
#>  Clinical Interpretation                                                                                                                                                                                                                                                      
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test                 Performance Level    Clinical Recommendation                                Detailed Interpretation                                                                                                                                                   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    continuous_marker    Excellent            Suitable for clinical use with appropriate cutpoint    The test 'continuous_marker' has an AUC of 0.955 indicating excellent discriminatory ability. This test can reliably distinguish between diseased and healthy patients.   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  OPTIMAL CUTPOINTS AND PERFORMANCE
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    53.7661964       86.95652       94.54545     97.08738     77.61194     0.8150198    0.9549407       0.8150198   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                         
#>  ──────────────────────────────────────────────────────────────────────────────── 
#>    Variable             AUC          95% CI Lower    95% CI Upper    p-value      
#>  ──────────────────────────────────────────────────────────────────────────────── 
#>    continuous_marker    0.9549407       0.9258927       0.9839887    < .0000001   
#>  ──────────────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong method.
#> 

```
