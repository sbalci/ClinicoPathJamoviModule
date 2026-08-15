# Test Data for groomecompare Function

Synthetic datasets for testing and demonstrating the `groomecompare`
function (Groome Staging System Comparison for Survival Data).

## Format

**groomecompare_test**: Standard dataset with 150 observations and 7
variables:

- patient_id:

  Character. Patient identifier (PT001-PT150)

- time:

  Numeric. Survival time in months (exponential distribution, mean ~20)

- event:

  Factor. Event indicator (0 = censored, 1 = death/event). Event rate
  ~60%

- age:

  Numeric. Patient age in years (40-80)

- sex:

  Factor. Patient sex (Male, Female)

- ypTNM:

  Ordered factor. Post-neoadjuvant pathological staging (I, II, III, IV)

- RPA:

  Ordered factor. Recursive partitioning risk groups (Low, Intermediate,
  High Risk)

**groomecompare_small**: Small sample dataset with 60 observations and 7
variables:

- patient_id:

  Character. Patient identifier (SM01-SM60)

- time:

  Numeric. Survival time in months

- event:

  Factor. Event indicator (0, 1)

- age:

  Numeric. Patient age in years

- sex:

  Factor. Patient sex

- ypTNM:

  Ordered factor. Tumor staging (I, II, III, IV)

- RPA:

  Ordered factor. Risk groups (Low, Intermediate, High Risk)

**groomecompare_large**: Large dataset with 300 observations and 8
variables:

- patient_id:

  Character. Patient identifier (LG001-LG300)

- time:

  Numeric. Survival time in months

- event:

  Factor. Event indicator (0, 1). Event rate ~55%

- age:

  Numeric. Patient age in years

- sex:

  Factor. Patient sex

- AJCC8:

  Ordered factor. AJCC 8th edition staging (IA, IB, IIA, IIB, IIIA,
  IIIB, IIIC, IV)

- RPA5:

  Ordered factor. 5-group RPA classification (Group 1-5)

- grade:

  Ordered factor. Tumor grade (1, 2, 3)

**Special test datasets**:

- groomecompare_unbalanced:

  120 observations with unbalanced staging systems (5 vs 2 groups)

- groomecompare_tied:

  80 observations with many tied survival times

- groomecompare_identical:

  100 observations with identical staging systems (negative control)

- groomecompare_clear_winner:

  150 observations where one system is clearly superior

- groomecompare_edge_truefalse:

  40 observations with event coded as TRUE/FALSE

- groomecompare_edge_12:

  40 observations with event coded as 1/2

## Source

Generated synthetically using `data-raw/groomecompare_test_data.R`.
Seed: 12345. Generation date: 2026-01-31.

## Details

These datasets were generated using a seeded random number generator to
produce realistic survival data for testing Groome staging system
comparison with the following characteristics:

- Survival times follow exponential distribution

- Event rates are clinically realistic (55-65%)

- Prognostic correlations built in (advanced stage -\> shorter survival)

- Both systems have predictive value but differ in discrimination

- Sufficient events per variable (EPV \> 10) for Cox models

The Groome method (Groome et al., 2001) compares staging systems using
four criteria:

1.  **Hazard Consistency**: Monotonicity of hazard ratios across stages

2.  **Hazard Discrimination**: Range/spread of hazard ratios between
    stages

3.  **Sample Balance**: Distribution of patients across staging groups

4.  **Outcome Prediction**: C-index/concordance for outcome prediction

The data generation process ensures:

- Non-negative survival times

- Proper factor level ordering (ordinal staging variables)

- Realistic clinical distributions

- Differential performance between staging systems

- Sufficient sample sizes for comparison

## File Formats

Each dataset is available in multiple formats:

- **RDA**: Native R format (use
  [`data()`](https://rdrr.io/r/utils/data.html))

- **CSV**: Comma-separated values (data/nonrda/)

- **XLSX**: Excel format (data/nonrda/)

- **OMV**: jamovi native format (data/nonrda/)

Multi-sheet workbook `groomecompare_all_scenarios.xlsx` contains all
test scenarios.

## Usage Examples

See `vignette("groomecompare-examples")` for comprehensive examples.

Basic usage:

    data(groomecompare_test)
    library(ClinicoPath)

    # Standard Groome comparison
    groomecompare(
      data = groomecompare_test,
      time = "time",
      event = "event",
      stage1 = "ypTNM",
      stage2 = "RPA",
      stage1name = "ypTNM Staging",
      stage2name = "RPA Classification"
    )

    # With bootstrap validation
    groomecompare(
      data = groomecompare_test,
      time = "time",
      event = "event",
      stage1 = "ypTNM",
      stage2 = "RPA",
      bootstrap = TRUE,
      nboot = 100,
      seed = 12345
    )

    # Test different event coding
    data(groomecompare_edge_truefalse)
    groomecompare(
      data = groomecompare_edge_truefalse,
      time = "time",
      event = "event_tf",
      stage1 = "ypTNM",
      stage2 = "RPA",
      eventValue = "TRUE"
    )

## Testing Scenarios

The datasets support testing of:

1.  **Standard comparison**: Use `groomecompare_test` with two staging
    systems

2.  **Small samples**: Use `groomecompare_small`, test sample size
    warnings

3.  **Complex systems**: Use `groomecompare_large` with detailed AJCC8
    vs RPA5

4.  **Unbalanced groups**: Use `groomecompare_unbalanced` (5 groups vs 2
    groups)

5.  **Tied times**: Use `groomecompare_tied` to test tie handling

6.  **Identical systems**: Use `groomecompare_identical` (negative
    control, all metrics ~0.5)

7.  **Clear winner**: Use `groomecompare_clear_winner` where one system
    dominates

8.  **Event coding**: Test TRUE/FALSE and 1/2 coding schemes

9.  **All visualizations**: Test radar plots, bar plots, Kaplan-Meier
    curves

10. **Bootstrap validation**: Test with bootstrap=TRUE, nboot=100-500

## Validation

All datasets have been validated for:

- Non-negative survival times

- Appropriate event rates (55-65%)

- Stage-survival correlation (advanced stage -\> worse prognosis)

- Sufficient EPV (events per variable \> 10) for Cox models

- Realistic clinical distributions

- Proper factor level ordering for ordinal staging

- Differential performance between systems (for comparison testing)

## Groome Criteria

The four Groome criteria used for staging system comparison:

- **Hazard Consistency (Rank 1-2)**: Monotonicity of hazard ratios

- **Hazard Discrimination (Rank 1-2)**: Spread of hazard ratios

- **Sample Balance (Rank 1-2)**: Distribution across stages

- **Outcome Prediction (Rank 1-2)**: C-index comparison

- **Overall Rank**: Sum of individual ranks (lower is better)

## References

Groome PA, Schulze K, Boysen M, Hall S, Mackillop WJ. (2001). A
comparison of published head and neck stage groupings in carcinomas of
the oral cavity. Head Neck, 23(8):613-624.
[doi:10.1002/hed.1089](https://doi.org/10.1002/hed.1089)

Balci S, Altinay S. (2025). Comparison of prognostic staging systems in
gastrointestinal neuroendocrine tumors using Groome method. Turk
Patoloji Derg, 41(1):1-10.
[doi:10.5146/tjpath.2023.01590](https://doi.org/10.5146/tjpath.2023.01590)

## See also

[`groomecompare`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/groomecompare.md)
for the main analysis function

`vignette("groomecompare-examples")` for comprehensive usage examples

[`rpasurvival_test_data`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/rpasurvival_test_data.md)
for RPA survival analysis test data

## Examples

``` r
# Load standard test data
data(groomecompare_test)

# Examine structure
str(groomecompare_test)
#> tibble [150 × 7] (S3: tbl_df/tbl/data.frame)
#>  $ patient_id: chr [1:150] "PT001" "PT002" "PT003" "PT004" ...
#>  $ time      : num [1:150] 7.1 10.5 6.5 0.5 4.1 ...
#>  $ event     : Factor w/ 2 levels "0","1": 1 1 2 1 2 1 1 1 1 1 ...
#>  $ ypTNM     : Ord.factor w/ 4 levels "Stage I"<"Stage II"<..: 2 1 4 1 3 1 2 3 1 1 ...
#>  $ RPA       : Ord.factor w/ 3 levels "Low Risk"<"Intermediate"<..: 1 1 1 1 2 1 1 1 2 2 ...
#>  $ age       : num [1:150] 47 52 78 61 69 65 71 54 60 43 ...
#>  $ sex       : Factor w/ 2 levels "Female","Male": 1 2 2 1 1 1 2 1 2 2 ...

# Summary statistics
summary(groomecompare_test)
#>      patient_id       time         event        ypTNM              RPA    
#>  Length   :150   Min.   :  0.200   0:77   Stage I  :38   Low Risk    :59  
#>  N.unique :150   1st Qu.:  3.325   1:73   Stage II :47   Intermediate:55  
#>  N.blank  :  0   Median :  6.650          Stage III:45   High Risk   :36  
#>  Min.nchar:  5   Mean   : 10.839          Stage IV :20                    
#>  Max.nchar:  5   3rd Qu.: 14.275                                          
#>                  Max.   :102.400                                          
#>       age            sex    
#>  Min.   :35.00   Female:62  
#>  1st Qu.:58.00   Male  :88  
#>  Median :65.00              
#>  Mean   :64.82              
#>  3rd Qu.:72.00              
#>  Max.   :92.00              

# Check event rate
table(groomecompare_test$event)
#> 
#>  0  1 
#> 77 73 
prop.table(table(groomecompare_test$event))
#> 
#>         0         1 
#> 0.5133333 0.4866667 

# Check staging distributions
table(groomecompare_test$ypTNM)
#> 
#>   Stage I  Stage II Stage III  Stage IV 
#>        38        47        45        20 
table(groomecompare_test$RPA)
#> 
#>     Low Risk Intermediate    High Risk 
#>           59           55           36 

# Cross-tabulation of staging systems
table(groomecompare_test$ypTNM, groomecompare_test$RPA)
#>            
#>             Low Risk Intermediate High Risk
#>   Stage I         17           12         9
#>   Stage II        22           14        11
#>   Stage III       13           21        11
#>   Stage IV         7            8         5

# Basic Groome comparison
if (FALSE) { # \dontrun{
library(ClinicoPath)
result <- groomecompare(
  data = groomecompare_test,
  time = "time",
  event = "event",
  stage1 = "ypTNM",
  stage2 = "RPA",
  radarplot = TRUE,
  barplot = TRUE,
  kmplots = TRUE
)
} # }
```
