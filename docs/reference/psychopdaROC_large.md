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
#>  Measure Variable(s): biomarker1, biomarker2
#> 
#>  Class Variable: disease_status
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
#>    63.8980591       65.24390       80.65476     62.20930     82.62195     0.4589866    0.8080539       0.4589866   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    55.1077643       69.51220       67.85714     51.35135     82.01439     0.3736934    0.7915941       0.3736934   
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
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 

```
