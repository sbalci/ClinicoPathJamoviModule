# psychopdaROC Cardiac Data - Myocardial Infarction Biomarkers

Cardiac biomarker dataset with 180 patients for ROC analysis of MI
diagnosis. Features three key cardiac markers: troponin, creatinine, and
BNP with realistic clinical distributions.

## Usage

``` r
psychopdaROC_cardiac
```

## Format

A data frame with 180 rows and 5 variables:

- patient_id:

  Character: Patient identifier (PT001-PT180)

- mi_status:

  Factor: "MI" or "No_MI" (25%/75% prevalence)

- troponin:

  Numeric: Troponin level (ng/mL), mean: 2.5 for MI, 0.3 for No_MI

- creatinine:

  Numeric: Creatinine level (mg/dL), mean: 1.3 for MI, 0.9 for No_MI

- bnp:

  Numeric: BNP level (pg/mL), mean: 850 for MI, 200 for No_MI

## Source

Generated test data for ClinicoPath package

## Details

Realistic cardiac biomarker distributions for evaluating diagnostic
performance in acute MI. Troponin shows strong discrimination, while
creatinine and BNP provide complementary diagnostic information.

## Examples

``` r
data(psychopdaROC_cardiac)
psychopdaROC(data = psychopdaROC_cardiac,
             dependentVars = c("troponin", "creatinine", "bnp"),
             classVar = "mi_status", positiveClass = "MI",
             refVar = "troponin",
             method = "maximize_metric", metric = "youden")
#> Multiple optimal cutpoints found, applying break_ties.
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
#>  Measure Variable(s): troponin, creatinine, bnp
#> 
#>  Class Variable: mi_status
#> 
#>  Positive Class: MI
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
#>  Seed: 123Positive Class: MI (Prevalence: 30.6%)Analysis Mode: Basic
#> 
#>  ROC Analysis Summary                                                      
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Variable      AUC          95% CI Lower    95% CI Upper    p-value      
#>  ───────────────────────────────────────────────────────────────────────── 
#>    troponin      0.8961455       0.8481203       0.9441706    < .0000001   
#>    creatinine    0.8808727       0.8291091       0.9326363    < .0000001   
#>    bnp           0.9294545       0.8894481       0.9694609    < .0000001   
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 
#> 
#>  Clinical Interpretation                                                                                                                                                                                                                                 
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test          Performance Level    Clinical Recommendation                                Detailed Interpretation                                                                                                                                     
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    troponin      Good                 Suitable for clinical use with appropriate cutpoint    The test 'troponin' has an AUC of 0.896 indicating good discriminatory ability. This test performs well for clinical decision making.                       
#>    creatinine    Good                 Suitable for clinical use with appropriate cutpoint    The test 'creatinine' has an AUC of 0.881 indicating good discriminatory ability. This test performs well for clinical decision making.                     
#>    bnp           Excellent            Suitable for clinical use with appropriate cutpoint    The test 'bnp' has an AUC of 0.929 indicating excellent discriminatory ability. This test can reliably distinguish between diseased and healthy patients.   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  OPTIMAL CUTPOINTS AND PERFORMANCE
#> 
#>  no title                                                                                                         
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint     Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    1.7027586       76.36364       82.40000     65.62500     88.79310     0.5876364    0.8961455       0.5876364   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  no title                                                                                                         
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint     Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    1.1500000       80.00000       80.80000     64.70588     90.17857     0.6080000    0.8808727       0.6080000   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  no title                                                                                                           
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint       Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    565.0294118       76.36364       87.20000     72.41379     89.34426     0.6356364    0.9294545       0.6356364   
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                  
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Variable      AUC          95% CI Lower    95% CI Upper    p-value      
#>  ───────────────────────────────────────────────────────────────────────── 
#>    troponin      0.8961455       0.8481203       0.9441706    < .0000001   
#>    creatinine    0.8808727       0.8291091       0.9326363    < .0000001   
#>    bnp           0.9294545       0.8894481       0.9694609    < .0000001   
#>  ───────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 

```
