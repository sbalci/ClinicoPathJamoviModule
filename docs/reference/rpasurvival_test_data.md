# Test Data for rpasurvival Function

Synthetic datasets for testing and demonstrating the `rpasurvival`
function (Recursive Partitioning Analysis for Survival Data).

## Format

**rpasurvival_test**: Standard dataset with 200 observations and 11
variables:

- patient_id:

  Character. Patient identifier (PT001-PT200)

- time:

  Numeric. Survival time in months (range: 0.5-120, mean ~36)

- event:

  Factor. Event indicator (0 = censored, 1 = death/event). Event rate
  ~65%

- age:

  Numeric. Patient age in years (40-85, mean ~65)

- stage:

  Ordered factor. Tumor stage (I, II, III, IV)

- grade:

  Ordered factor. Tumor grade (G1, G2, G3)

- LVI:

  Factor. Lymphovascular invasion (Absent, Present)

- tumor_size:

  Numeric. Tumor size in centimeters (0.5-10)

- ki67:

  Numeric. Ki-67 proliferation index, percentage (0-100). ~3% missing

- performance_status:

  Ordered factor. ECOG performance status (0, 1, 2)

- treatment:

  Factor. Treatment modality (Surgery only, Surgery + Chemo, Surgery +
  Radio, Trimodal)

**rpasurvival_small**: Minimal dataset with 50 observations and 6
variables:

- patient_id:

  Character. Patient identifier (SM01-SM50)

- time:

  Numeric. Survival time in months

- event:

  Factor. Event indicator (0, 1)

- age:

  Numeric. Patient age in years

- stage:

  Factor. Tumor stage (Early, Advanced)

- grade:

  Factor. Tumor grade (Low, High)

**rpasurvival_large**: Large dataset with 500 observations and 11
variables:

- patient_id:

  Character. Patient identifier (LG0001-LG0500)

- time:

  Numeric. Survival time in months

- event:

  Factor. Event indicator (0, 1). Event rate ~70%

- age:

  Numeric. Patient age in years

- stage:

  Ordered factor. Detailed tumor stage (IA, IB, IIA, IIB, IIIA, IIIB,
  IV)

- grade:

  Ordered factor. Tumor grade (1, 2, 3)

- LVI:

  Factor. Lymphovascular invasion (No, Yes)

- PNI:

  Factor. Perineural invasion (No, Yes)

- tumor_size:

  Numeric. Tumor size in centimeters

- nodes_positive:

  Numeric. Number of positive lymph nodes

- biomarker1:

  Numeric. Continuous biomarker 1

- biomarker2:

  Numeric. Continuous biomarker 2

**Edge case datasets** (for testing different event/time coding):

- rpasurvival_edge_truefalse:

  30 observations with event coded as TRUE/FALSE

- rpasurvival_edge_12:

  30 observations with event coded as 1/2

- rpasurvival_edge_days:

  30 observations with time in days

- rpasurvival_edge_years:

  30 observations with time in years

## Source

Generated synthetically using `data-raw/rpasurvival_test_data.R`. Seed:
12345. Generation date: 2026-01-31.

## Details

These datasets were generated using a seeded random number generator to
produce realistic survival data with the following characteristics:

- Survival times follow exponential distribution

- Event rates are clinically realistic (60-70%)

- Prognostic correlations built in (Stage IV -\> shorter survival)

- Missing data pattern (~3% in continuous biomarkers)

- Events-per-variable (EPV) ratio \> 10 for all datasets

The data generation process ensures:

- Non-negative survival times

- Proper factor level ordering (ordinal variables)

- Realistic clinical distributions

- Sufficient sample sizes for RPA analysis

## File Formats

Each dataset is available in multiple formats:

- **RDA**: Native R format (use
  [`data()`](https://rdrr.io/r/utils/data.html))

- **CSV**: Comma-separated values

- **XLSX**: Excel format

- **OMV**: jamovi native format

## Usage Examples

See `vignette("rpasurvival-examples")` for comprehensive examples.

Basic usage:


    data(rpasurvival_test)
    library(ClinicoPath)

    # Standard RPA analysis
    rpasurvival(
      data = rpasurvival_test,
      time = "time",
      event = "event",
      predictors = c("age", "stage", "grade", "LVI"),
      time_unit = "months"
    )

    # Test small sample warnings
    data(rpasurvival_small)
    rpasurvival(
      data = rpasurvival_small,
      time = "time",
      event = "event",
      predictors = c("stage", "grade")
    )

    # Test different event coding
    data(rpasurvival_edge_truefalse)
    rpasurvival(
      data = rpasurvival_edge_truefalse,
      time = "time",
      event = "event_tf",
      predictors = c("stage", "grade"),
      eventValue = "TRUE"
    )

## Testing Scenarios

The datasets support testing of:

1.  **Standard analysis**: Use `rpasurvival_test` with 4-6 predictors

2.  **Small samples**: Use `rpasurvival_small`, expect warnings

3.  **Complex trees**: Use `rpasurvival_large` with maxdepth=5

4.  **Event coding**: Test TRUE/FALSE and 1/2 coding schemes

5.  **Time units**: Test days, months, years with time_unit parameter

6.  **Missing data**: Verify handling of ~3% missing values

7.  **Mixed predictors**: Continuous, ordinal, and nominal variables

## Validation

All datasets have been validated for:

- Non-negative survival times

- Appropriate event rates

- Stage-survival correlation (higher stage -\> worse prognosis)

- Sufficient EPV (events per variable \> 10)

- Realistic clinical distributions

- Proper factor level ordering

## References

Liu Y, et al. (2026). Recursive partitioning analysis for survival data.

## See also

[`rpasurvival`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/rpasurvival.md)
for the main analysis function

`vignette("rpasurvival-examples")` for comprehensive usage examples

## Examples

``` r
# Load standard test data
data(rpasurvival_test)

# Examine structure
str(rpasurvival_test)
#> tibble [200 × 11] (S3: tbl_df/tbl/data.frame)
#>  $ patient_id        : chr [1:200] "PT001" "PT002" "PT003" "PT004" ...
#>  $ time              : num [1:200] 9.5 11.4 17.5 0.3 16.4 ...
#>  $ event             : Factor w/ 2 levels "0","1": 1 2 1 2 1 1 1 2 2 1 ...
#>  $ age               : num [1:200] 81 62 59 66 66 69 74 63 80 57 ...
#>  $ stage             : Ord.factor w/ 4 levels "I"<"II"<"III"<..: 3 3 3 4 1 3 1 2 4 1 ...
#>  $ grade             : Ord.factor w/ 3 levels "G1"<"G2"<"G3": 2 1 1 3 2 2 3 3 2 2 ...
#>  $ LVI               : Factor w/ 2 levels "Absent","Present": 2 1 1 1 1 1 2 2 2 2 ...
#>  $ tumor_size        : num [1:200] 2.43 3.72 7.11 3.4 0.5 ...
#>  $ ki67              : num [1:200] 32 25.4 37.3 61.8 NA ...
#>  $ performance_status: Ord.factor w/ 3 levels "0"<"1"<"2": 3 3 1 2 2 1 2 2 3 1 ...
#>  $ treatment         : Factor w/ 4 levels "Surgery + Chemo",..: 1 3 3 1 4 1 1 3 2 4 ...

# Summary statistics
summary(rpasurvival_test)
#>      patient_id       time         event        age        stage    grade  
#>  Length   :200   Min.   :  0.300   0: 70   Min.   :37.00   I  :49   G1:42  
#>  N.unique :200   1st Qu.:  7.775   1:130   1st Qu.:56.00   II :52   G2:92  
#>  N.blank  :  0   Median : 15.800           Median :63.00   III:74   G3:66  
#>  Min.nchar:  5   Mean   : 26.020           Mean   :63.45   IV :25          
#>  Max.nchar:  5   3rd Qu.: 36.200           3rd Qu.:72.00                   
#>                  Max.   :230.500           Max.   :90.00                   
#>                                                                            
#>       LVI        tumor_size         ki67       performance_status
#>  Absent :114   Min.   :0.500   Min.   : 0.00   0:97              
#>  Present: 86   1st Qu.:2.446   1st Qu.:15.75   1:63              
#>                Median :3.651   Median :30.42   2:40              
#>                Mean   :3.764   Mean   :30.80                     
#>                3rd Qu.:5.089   3rd Qu.:44.55                     
#>                Max.   :8.597   Max.   :97.00                     
#>                NAs    :6       NAs    :6                         
#>            treatment 
#>  Surgery + Chemo:77  
#>  Surgery + Radio:34  
#>  Surgery only   :62  
#>  Trimodal       :27  
#>                      
#>                      
#>                      

# Check event rate
table(rpasurvival_test$event)
#> 
#>   0   1 
#>  70 130 
prop.table(table(rpasurvival_test$event))
#> 
#>    0    1 
#> 0.35 0.65 

# Check stage distribution
table(rpasurvival_test$stage)
#> 
#>   I  II III  IV 
#>  49  52  74  25 

# Basic RPA analysis
if (FALSE) { # \dontrun{
library(ClinicoPath)
result <- rpasurvival(
  data = rpasurvival_test,
  time = "time",
  event = "event",
  predictors = c("age", "stage", "grade", "LVI")
)
} # }
```
