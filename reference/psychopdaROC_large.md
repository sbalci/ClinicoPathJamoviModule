# psychopdaROC Large Sample Data

Large dataset with 500 patients and multiple biomarkers for testing
computational efficiency and performance with substantial sample sizes.

## Usage

``` r
psychopdaROC_large
```

## Format

A data frame with 500 rows and 8 variables:

- patient_id:

  Character: Patient identifier (PT0001-PT0500)

- disease_status:

  Factor: "Disease" or "No_Disease" (30%/70% prevalence)

- biomarker1:

  Numeric: First biomarker (mean: 75 for disease, 50 for no disease)

- biomarker2:

  Numeric: Second biomarker (mean: 68 for disease, 48 for no disease)

- age:

  Numeric: Patient age in years (mean 62, SD 13)

- sex:

  Factor: "Male" or "Female"

- site:

  Factor: Research site (Site_1 through Site_10)

- risk_category:

  Factor: "Low", "Intermediate", or "High"

## Source

Generated test data for ClinicoPath package

## Details

Large sample (n=500) with multiple biomarkers and stratification
variables. Tests computational efficiency and stability of estimates
with adequate sample sizes. Includes multi-site and risk stratification
for subgroup analysis.

## Examples

``` r
data(psychopdaROC_large)
psychopdaROC(data = psychopdaROC_large,
             dependentVars = c("biomarker1", "biomarker2"),
             classVar = "disease_status", positiveClass = "Disease",
             refVar = "biomarker1")
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
#>  Measure Variable(s): biomarker1, biomarker2
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
#>  Seed: 123Positive Class: Disease (Prevalence: 32.8%)Analysis Mode:
#>  Basic
#> 
#>  ROC Analysis Summary                                                      
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Variable      AUC          95% CI Lower    95% CI Upper    p-value      
#>  ───────────────────────────────────────────────────────────────────────── 
#>    biomarker1    0.8080539       0.7676279       0.8484798    < .0000001   
#>    biomarker2    0.7915941       0.7512573       0.8319309    < .0000001   
#>  ───────────────────────────────────────────────────────────────────────── 
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
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test          Performance Level    Clinical Recommendation                                Detailed Interpretation                                                                                                                   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    biomarker1    Good                 Suitable for clinical use with appropriate cutpoint    The test 'biomarker1' has an AUC of 0.808 indicating good discriminatory ability. This test performs well for clinical decision making.   
#>    biomarker2    Fair                 May be useful in combination with other markers        The test 'biomarker2' has an AUC of 0.792 indicating fair discriminatory ability. Consider combining with other clinical information.     
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  OPTIMAL CUTPOINTS AND PERFORMANCE
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    65.0099686       64.02439       82.73810     64.41718     82.49258     0.4676249    0.8080539       0.4676249   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    48.7346730       87.80488       55.35714     48.97959     90.29126     0.4316202    0.7915941       0.4316202   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                  
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Variable      AUC          95% CI Lower    95% CI Upper    p-value      
#>  ───────────────────────────────────────────────────────────────────────── 
#>    biomarker1    0.8080539       0.7676279       0.8484798    < .0000001   
#>    biomarker2    0.7915941       0.7512573       0.8319309    < .0000001   
#>  ───────────────────────────────────────────────────────────────────────── 
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
