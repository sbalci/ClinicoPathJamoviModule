# Basic Swimmer Plot Example Data

A simple dataset for demonstrating basic swimmer plot functionality.
Contains patient timelines with treatment responses and basic
categorization.

## Usage

``` r
data(swimmer_unified_basic)
```

## Format

A data frame with 10 rows and 6 variables:

- PatientID:

  Character. Unique patient identifiers (PT001-PT010)

- StartTime:

  Numeric. Treatment start time (all patients start at 0)

- EndTime:

  Numeric. Treatment end time in months (5-15 months)

- Response:

  Character. Best response (CR, PR, SD, PD)

- Treatment:

  Character. Treatment type (Immunotherapy, Chemotherapy, Combination,
  Targeted)

- Priority:

  Character. Patient priority level (High, Medium, Low)

## Source

Generated for ClinicoPath package demonstration

## See also

Other swimmer plot datasets:
[`swimmer_unified_comprehensive`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/swimmer_unified_comprehensive.md),
[`swimmer_unified_datetime`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/swimmer_unified_datetime.md),
[`swimmer_unified_events`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/swimmer_unified_events.md),
[`swimmer_unified_oncology`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/swimmer_unified_oncology.md)

## Examples

``` r
data(swimmer_unified_basic)

# Basic swimmer plot
swimmerplot(
  data = swimmer_unified_basic,
  patientID = "PatientID",
  startTime = "StartTime", 
  endTime = "EndTime",
  responseVar = "Response"
)
#> 
#>  SWIMMER PLOT
#> NOTE: Ongoing-treatment arrows not drawn
#> Ongoing-status arrows require a censoring/event status variable. Without one, whether a patient was still on treatment at data cutoff cannot be determined from the timeline alone, so no arrows are drawn. Supply a censoring variable (0/FALSE/no/censored/alive for ongoing, 1/TRUE/yes/event/dead for completed) to show them.
#>  <div style='color: #8a6d00; background-color: #fff8e1; padding: 15px;
#>  border: 1px solid #ffc107; border-radius: 5px; margin: 10px;'>
#> 
#>  Analysis Information
#> 
#>  Some response categories have <3 patients. Consider grouping
#>  categories for meaningful analysis.
#> 
#>  Timeline Summary Statistics                 
#>  ─────────────────────────────────────────── 
#>    Metric                        Value       
#>  ─────────────────────────────────────────── 
#>    Number of Patients            10.000000   
#>    Total Observations            10.000000   
#>    Median Duration (observed)     9.500000   
#>    Mean Duration                  9.700000   
#>    Total Person-Time             97.000000   
#>    Mean Follow-up                 9.700000   
#>    CR Rate (%)                   30.000000   
#>    PD Rate (%)                   20.000000   
#>    PR Rate (%)                   30.000000   
#>    SD Rate (%)                   20.000000   
#>  ─────────────────────────────────────────── 
#> 
#> 
#>  <div style='background-color: #e8f5e8; padding: 15px; border-radius:
#>  5px; margin: 10px 0;'>
#> 
#>  Clinical Interpretation
#> 
#>  <div style='margin: 10px 0;'><h5 style='color: #2e7d32;'>Timeline
#>  Analysis:
#> 
#>  Study included 10 patients with 10 timeline observations. Median
#>  observed duration was 9.5 months (range: 5.0 to 15.0 months).
#> 
#>  <div style='margin: 10px 0;'><h5 style='color: #2e7d32;'>Person-Time
#>  Analysis:
#> 
#>  Total person-time: 97.0 months. Average follow-up per patient: 9.7
#>  months.
#> 
#>  <div style='margin: 10px 0;'><h5 style='color: #2e7d32;'>Response
#>  Pattern Analysis:
#> 
#>  Most common response was CR (30.0% of patients). Response distribution
#>  shows clinical patterns suitable for efficacy analysis.
#> 
#>  Person-Time Analysis                                                          
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    Response Type    Patients    Total Time    Mean Time    Follow-up Density   
#>  ───────────────────────────────────────────────────────────────────────────── 
#>    CR                      3      35.00000    11.670000             8.571000   
#>    PD                      2      16.00000     8.000000            12.500000   
#>    PR                      3      24.00000     8.000000            12.500000   
#>    SD                      2      22.00000    11.000000             9.091000   
#>  ───────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  Milestone Event Summary                              
#>  ──────────────────────────────────────────────────── 
#>    Milestone    Events    Median Time    Time Range   
#>  ──────────────────────────────────────────────────── 
#>  ──────────────────────────────────────────────────── 
#> 
#> 
#>  Advanced Clinical Metrics                                                                                                                                                                              
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Metric                                                                  Value        95% CI         Unit                 Clinical Interpretation                                                     
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Median Follow-up Time (observed durations; no censoring information)     9.500000                   months               Central tendency of patient follow-up duration                              
#>    Interquartile Range (observed durations)                                 4.500000                   months               Middle 50% of follow-up duration range                                      
#>    Total Study Person-Time                                                 97.000000                   months cumulative    Total observation time across all patients                                  
#>    Follow-up Density                                                       10.309000                   per 100 months       Number of patients per 100 units of observation time (descriptive metric)   
#>    Objective Response Rate (ORR)                                           60.000000    26.2 - 87.8    percent              Proportion with complete or partial response                                
#>    Disease Control Rate (DCR)                                              80.000000    44.4 - 97.5    percent              Proportion with response or stable disease                                  
#>  ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 

```
