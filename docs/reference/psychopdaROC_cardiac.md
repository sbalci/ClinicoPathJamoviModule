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
#>  Measure Variable(s): troponin, creatinine, bnp
#> 
#>  Class Variable: mi_status
#> 
#>  Positive Class: MI
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
#>    Note. Reading of the test values: <b>HIGHER values were taken to
#>    indicate MI</b> (Classification Direction = ">="). If that is the
#>    wrong way round for this marker, every sensitivity, specificity,
#>    cutpoint and AUC below is reversed — switch Classification
#>    Direction to "<=" and the AUC becomes 1 minus the value shown.
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
#>    2.1400000       70.90909       91.20000     78.00000     87.69231     0.6210909    0.8961455       0.6210909   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  no title                                                                                                         
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint     Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    1.1200000       85.45455       79.20000     64.38356     92.52336     0.6465455    0.8808727       0.6465455   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  no title                                                                                                           
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint       Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    622.0000000       76.36364       93.60000     84.00000     90.00000     0.6996364    0.9294545       0.6996364   
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
#>    Note. Reading of the test values: <b>HIGHER values were taken to
#>    indicate MI</b> (Classification Direction = ">="). If that is the
#>    wrong way round for this marker, every sensitivity, specificity,
#>    cutpoint and AUC below is reversed — switch Classification
#>    Direction to "<=" and the AUC becomes 1 minus the value shown.
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 

```
