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
#>  Measure Variable(s): baseline_marker, followup_marker
#> 
#>  Class Variable: outcome
#> 
#>  Positive Class: Event
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
#>  Seed: 123Positive Class: Event (Prevalence: 27.9%)Analysis Mode: Basic
#> 
#>  ROC Analysis Summary                                                           
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Variable           AUC          95% CI Lower    95% CI Upper    p-value      
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    baseline_marker    0.8565626       0.7834935       0.9296316    < .0000001   
#>    followup_marker    0.9548109       0.9158621       0.9937596    < .0000001   
#>  ────────────────────────────────────────────────────────────────────────────── 
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
#>    64.6200867       76.92308       82.17822     62.50000     90.21739     0.5910129    0.8565626       0.5910129   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  no title                                                                                                          
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Cutpoint      Sensitivity    Specificity    PPV          NPV          Youden's J    AUC          Metric Score   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    65.5593033       97.43590       84.15842     70.37037     98.83721     0.8159431    0.9548109       0.8159431   
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
#>    Note. AUC 95% confidence intervals computed using the DeLong method.
#> 

```
