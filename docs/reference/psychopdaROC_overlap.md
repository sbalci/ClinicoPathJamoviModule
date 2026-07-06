# psychopdaROC Overlapping Distributions Data

Dataset with 190 patients showing moderate overlap between diseased and
non-diseased groups, representing realistic clinical scenarios with
imperfect discrimination.

## Usage

``` r
psychopdaROC_overlap
```

## Format

A data frame with 190 rows and 3 variables:

- patient_id:

  Character: Patient identifier (PT001-PT190)

- diagnosis:

  Factor: "Diseased" or "Non_Diseased" (40%/60% prevalence)

- test_value:

  Numeric: Test values with moderate overlap (mean: 60 vs 55, SD 18)

## Source

Generated test data for ClinicoPath package

## Details

Small mean difference (5 units) with substantial overlap between groups.
Represents realistic clinical scenarios where diagnostic tests show
moderate but imperfect discrimination (AUC ~0.60-0.65).

## Examples

``` r
data(psychopdaROC_overlap)
psychopdaROC(data = psychopdaROC_overlap, dependentVars = "test_value",
             classVar = "diagnosis", positiveClass = "Diseased",
             refVar = "test_value")
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
#>  Measure Variable(s): test_value
#> 
#>  Class Variable: diagnosis
#> 
#>  Positive Class: Diseased
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
#>  Seed: 123Positive Class: Diseased (Prevalence: 38.9%)Analysis Mode:
#>  Basic
#> 
#>  ROC Analysis Summary                                                     
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Variable      AUC          95% CI Lower    95% CI Upper    p-value     
#>  ──────────────────────────────────────────────────────────────────────── 
#>    test_value    0.5994874       0.5176626       0.6813122    0.0171704   
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 
#> 
#>  Clinical Interpretation                                                                                                                                                                                                             
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test          Performance Level    Clinical Recommendation                            Detailed Interpretation                                                                                                                     
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    test_value    Poor                 Not recommended as standalone diagnostic marker    The test 'test_value' has an AUC of 0.599 indicating poor discriminatory ability. Alternative diagnostic approaches should be considered.   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  OPTIMAL CUTPOINTS AND PERFORMANCE
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    55.4732866       64.86486       55.17241     48.00000     71.11111     0.2003728    0.5994874       0.2003728   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                 
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Variable      AUC          95% CI Lower    95% CI Upper    p-value     
#>  ──────────────────────────────────────────────────────────────────────── 
#>    test_value    0.5994874       0.5176626       0.6813122    0.0171704   
#>  ──────────────────────────────────────────────────────────────────────── 
#>    Note. AUC 95% confidence intervals computed using the DeLong
#>    method.
#> 

```
