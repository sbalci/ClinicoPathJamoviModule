# psychopdaROC Time-Dependent Biomarker Data

Dataset with 140 patients featuring baseline and follow-up biomarker
measurements for evaluating time-dependent diagnostic performance.

## Usage

``` r
psychopdaROC_timedep
```

## Format

A data frame with 140 rows and 5 variables:

- patient_id:

  Character: Patient identifier (PT001-PT140)

- outcome:

  Factor: "Event" or "No_Event" (32%/68% prevalence)

- baseline_marker:

  Numeric: Baseline biomarker (mean: 70 for event, 50 for no event)

- followup_marker:

  Numeric: Follow-up biomarker (increases for events, decreases for no
  events)

- time_to_outcome:

  Numeric: Time to outcome in months (1-36)

## Source

Generated test data for ClinicoPath package

## Details

Follow-up marker changes from baseline: increases by ~15 for events,
decreases by ~5 for no events. Enables ROC analysis with change scores
or follow-up values.

## Examples

``` r
data(psychopdaROC_timedep)
psychopdaROC(data = psychopdaROC_timedep,
             dependentVars = c("baseline_marker", "followup_marker"),
             classVar = "outcome", positiveClass = "Event",
             refVar = "baseline_marker")
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
#>  Measure Variable(s): baseline_marker, followup_marker
#> 
#>  Class Variable: outcome
#> 
#>  Positive Class: Event
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
#>  Seed: 123Positive Class: Event (Prevalence: 27.9%)Analysis Mode: Basic
#> 
#>  ROC Analysis Summary                                                           
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Variable           AUC          95% CI Lower    95% CI Upper    p-value      
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    baseline_marker    0.8565626       0.7834935       0.9296316    < .0000001   
#>    followup_marker    0.9548109       0.9158621       0.9937596    < .0000001   
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Note. Reading of the test values: <b>HIGHER values were taken to
#>    indicate Event</b> (Classification Direction = ">="). If that is the
#>    wrong way round for this marker, every sensitivity, specificity,
#>    cutpoint and AUC below is reversed — switch Classification Direction to
#>    "<=" and the AUC becomes 1 minus the value shown.
#>    Note. AUC 95% confidence intervals computed using the DeLong method.
#> 
#> 
#>  Clinical Interpretation                                                                                                                                                                                                                                                  
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Test               Performance Level    Clinical Recommendation                                Detailed Interpretation                                                                                                                                                 
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    baseline_marker    Good                 Suitable for clinical use with appropriate cutpoint    The test 'baseline_marker' has an AUC of 0.857 indicating good discriminatory ability. This test performs well for clinical decision making.                            
#>    followup_marker    Excellent            Suitable for clinical use with appropriate cutpoint    The test 'followup_marker' has an AUC of 0.955 indicating excellent discriminatory ability. This test can reliably distinguish between diseased and healthy patients.   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  OPTIMAL CUTPOINTS AND PERFORMANCE
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    67.1808226       74.35897       88.11881     70.73171     89.89899     0.6247779    0.8565626       0.6247779   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    66.7645316       97.43590       86.13861     73.07692     98.86364     0.8357451    0.9548109       0.8357451   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Area Under the ROC Curve                                                       
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Variable           AUC          95% CI Lower    95% CI Upper    p-value      
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    baseline_marker    0.8565626       0.7834935       0.9296316    < .0000001   
#>    followup_marker    0.9548109       0.9158621       0.9937596    < .0000001   
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Note. Reading of the test values: <b>HIGHER values were taken to
#>    indicate Event</b> (Classification Direction = ">="). If that is the
#>    wrong way round for this marker, every sensitivity, specificity,
#>    cutpoint and AUC below is reversed — switch Classification Direction to
#>    "<=" and the AUC becomes 1 minus the value shown.
#>    Note. AUC 95% confidence intervals computed using the DeLong method.
#> 

```
