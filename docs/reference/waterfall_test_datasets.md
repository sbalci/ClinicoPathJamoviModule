# Test Datasets for Waterfall Function

A collection of test datasets for the waterfall function, which creates
waterfall and spider plots for tumor response analysis following RECIST
v1.1 criteria.

## Usage

``` r
waterfall_test

waterfall_spider_test

waterfall_raw_test

waterfall_small

waterfall_large

waterfall_missing

waterfall_extreme

waterfall_no_baseline

waterfall_phase2
```

## Format

### `waterfall_test`

Main test dataset with pre-calculated percentage changes (30 patients):

- patientID:

  Character. Unique patient identifier (PT001-PT030)

- best_response:

  Numeric. Percentage change from baseline (-100% to +180%)

- treatment:

  Character. Treatment group (Monotherapy, Combination, Control)

- disease_subtype:

  Character. Disease classification (Type A/B/C)

- prior_lines:

  Integer. Number of prior therapy lines (0-3)

### `waterfall_spider_test`

Longitudinal data for spider plot testing (120 observations: 20 patients
x 6 timepoints):

- patientID:

  Character. Patient identifier (PT001-PT020)

- treatment:

  Character. Treatment group (Experimental, Standard)

- time:

  Numeric. Months from baseline (0, 2, 4, 6, 8, 12)

- pct_change:

  Numeric. Percentage change at each timepoint

- response_category:

  Character. RECIST category (CR/PR/SD/PD)

### `waterfall_raw_test`

Raw tumor measurements for percentage calculation (125 observations: 25
patients x 5 timepoints):

- patientID:

  Character. Patient identifier (PT001-PT025)

- time:

  Numeric. Months from baseline (0, 1, 2, 4, 6)

- tumor_size:

  Numeric. Sum of target lesion diameters (mm)

- treatment:

  Character. Treatment arm (Drug A, Drug B, Placebo)

### `waterfall_small`

Minimal viable dataset (5 patients):

- patientID:

  Character. Patient identifier

- best_response:

  Numeric. Percentage change

- treatment:

  Character. Treatment group (A, B, Control)

### `waterfall_large`

Large dataset for performance testing (200 patients):

- patientID:

  Character. Patient identifier

- best_response:

  Numeric. Percentage change

- treatment:

  Character. Treatment arm (8 levels)

- biomarker_status:

  Character. Biomarker status (Positive, Negative)

### `waterfall_missing`

Dataset with missing values (20 patients, ~15% missing):

- patientID:

  Character. Patient identifier

- best_response:

  Numeric. Percentage change (some NA)

- treatment:

  Character. Treatment group (some NA)

### `waterfall_extreme`

Dataset with extreme values and outliers (15 patients):

- patientID:

  Character. Patient identifier

- best_response:

  Numeric. Percentage change (includes CRs and extreme PD)

- treatment:

  Character. Treatment group

### `waterfall_no_baseline`

Dataset missing baseline measurements for error testing (20
observations):

- patientID:

  Character. Patient identifier

- treatment:

  Character. Treatment group

- time:

  Numeric. Time from baseline (no time=0 values)

- pct_change:

  Numeric. Percentage change

### `waterfall_phase2`

Realistic Phase II oncology trial simulation (50 patients):

- patientID:

  Character. Study identifier (STUDY001-001 to STUDY001-050)

- best_response:

  Numeric. Percentage change (ORR ~30%, DCR ~60%)

- cohort:

  Character. Dose level (1, 2, 3)

- age:

  Numeric. Patient age (years)

- ecog_ps:

  Integer. ECOG performance status (0-2)

- pdl1_status:

  Character. PD-L1 expression level (\<1%, 1-49%, \>=50%)

- time_on_treatment:

  Numeric. Duration on treatment (months)

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 120
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 125
rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 5
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 200
rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 20
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 15
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 20
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 50
rows and 7 columns.

## Source

Generated using data-raw/waterfall_test_data.R (seed = 42)

## RECIST v1.1 Criteria

Response categories are based on RECIST v1.1 thresholds:

- CR (Complete Response): \<= -100% (complete disappearance)

- PR (Partial Response): -99% to -30% (significant shrinkage)

- SD (Stable Disease): -29% to +19% (minimal change)

- PD (Progressive Disease): \>= +20% (tumor growth)

Clinical metrics:

- ORR (Objective Response Rate) = (CR + PR) / Total

- DCR (Disease Control Rate) = (CR + PR + SD) / Total

## Data Generation

All datasets were generated with seed 42 for reproducibility. Values are
clinically realistic and incorporate appropriate correlations (e.g.,
treatment effects, baseline characteristics).

## Use Cases

- **waterfall_test**: Basic waterfall plot testing, RECIST
  categorization

- **waterfall_spider_test**: Spider plot generation, longitudinal
  analysis

- **waterfall_raw_test**: Raw measurement processing, percentage
  calculation

- **waterfall_small**: Minimal dataset, edge case testing

- **waterfall_large**: Performance testing, scalability validation

- **waterfall_missing**: Missing data handling

- **waterfall_extreme**: Outlier handling, extreme values

- **waterfall_no_baseline**: Error validation, missing baseline
  detection

- **waterfall_phase2**: Complete clinical trial workflow

## See also

[`waterfall`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/waterfall.md)
for the waterfall analysis function

## Examples

``` r
# Load main test dataset
data(waterfall_test)
head(waterfall_test)
#> # A tibble: 6 × 5
#>   patientID best_response treatment   disease_subtype prior_lines
#>   <chr>             <dbl> <chr>       <chr>                 <int>
#> 1 PT001               -90 Control     Type B                    1
#> 2 PT002               -90 Control     Type C                    0
#> 3 PT003              -100 Combination Type B                    1
#> 4 PT004               -65 Control     Type A                    2
#> 5 PT005               -65 Monotherapy Type B                    1
#> 6 PT006               -55 Monotherapy Type B                    1

# Basic waterfall plot
if (FALSE) { # \dontrun{
waterfall(
    data = waterfall_test,
    patientID = "patientID",
    responseVar = "best_response",
    inputType = "percentage"
)
} # }

# Spider plot with longitudinal data
if (FALSE) { # \dontrun{
data(waterfall_spider_test)
waterfall(
    data = waterfall_spider_test,
    patientID = "patientID",
    responseVar = "pct_change",
    timeVar = "time",
    showSpiderPlot = TRUE,
    timeUnitLabel = "months"
)
} # }

# Raw measurements with automatic calculation
if (FALSE) { # \dontrun{
data(waterfall_raw_test)
waterfall(
    data = waterfall_raw_test,
    patientID = "patientID",
    responseVar = "tumor_size",
    timeVar = "time",
    inputType = "raw",
    groupVar = "treatment"
)
} # }

# Clinical trial analysis
if (FALSE) { # \dontrun{
data(waterfall_phase2)
waterfall(
    data = waterfall_phase2,
    patientID = "patientID",
    responseVar = "best_response",
    groupVar = "cohort",
    generateCopyReadyReport = TRUE,
    showConfidenceIntervals = TRUE
)
} # }
```
