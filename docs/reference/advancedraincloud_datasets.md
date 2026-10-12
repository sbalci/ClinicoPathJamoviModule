# Clinical Trial Test Datasets for Advanced Raincloud Plot

Comprehensive test datasets designed to demonstrate and test all
features of the Advanced Raincloud Plot module. These datasets simulate
realistic clinical trial scenarios with various outcome measures and
missing data patterns.

## Usage

``` r
advancedraincloud_data

advancedraincloud_baseline

advancedraincloud_endpoint

advancedraincloud_change
```

## Format

- advancedraincloud_data:

  Longitudinal dataset with 900 rows (300 patients x 3 visits) and 15
  variables

- advancedraincloud_baseline:

  Cross-sectional baseline data with 300 rows and 13 variables

- advancedraincloud_endpoint:

  Cross-sectional endpoint data with 300 rows and 13 variables

- advancedraincloud_change:

  Change score data with 130 complete cases and 6 variables

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 900
rows and 15 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 300
rows and 13 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 300
rows and 13 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 130
rows and 6 columns.

## Source

Generated using the script in
data-raw/create_advancedraincloud_testdata.R

## Variables

**Patient Identifiers:**

- patient_id:

  Unique patient identifier (PT001-PT300)

- treatment_arm:

  Treatment group - factor with levels "Placebo", "Drug A"

- time_point:

  Visit timepoint - factor with levels "Baseline", "Week 4", "Week 12"

- visit_number:

  Numeric visit number (1-3)

**Demographics:**

- age:

  Patient age in years (normally distributed, mean=55, sd=12)

- gender:

  Patient gender - factor with levels "Female", "Male"

- disease_stage:

  Disease stage - factor with levels "Early", "Advanced"

- biomarker_status:

  Biomarker status - factor with levels "Negative", "Positive"

- age_group:

  Age stratification - factor "\< 65 years", "\>= 65 years"

- baseline_biomarker_high:

  Baseline biomarker level - factor "High", "Normal"

**Primary Outcomes:**

- tumor_size_change:

  Percent change in tumor size from baseline (negative = shrinkage)

- biomarker_level:

  Biomarker concentration (log-normally distributed, ~90 units baseline)

- qol_score:

  Quality of life score (0-100 scale, higher = better)

- pain_score:

  Pain intensity score - ordered factor (0-10 scale, higher = worse
  pain)

**Derived Variables:**

- tumor_responder:

  Response classification - factor with levels:

  - "Progressive Disease" (\>10% increase)

  - "Stable Disease" (-10% to +10%)

  - "Partial Response" (-30% to -10%)

  - "Complete Response" (\<=-30%)

**Change Score Variables (advancedraincloud_change only):**

- tumor_change:

  Change in tumor size (Week 12 - Baseline)

- biomarker_change:

  Change in biomarker (Baseline - Week 12, positive = reduction)

- qol_change:

  Change in quality of life (Week 12 - Baseline)

- pain_change:

  Change in pain score (Baseline - Week 12, positive = improvement)

## Clinical Trial Design

This simulates a randomized, placebo-controlled trial with:

- **N = 300 patients** (150 per arm)

- **3 timepoints:** Baseline, Week 4, Week 12

- **Primary endpoint:** Tumor size reduction

- **Secondary endpoints:** Biomarker levels, quality of life, pain
  scores

- **Realistic dropout:** ~15% overall, higher in placebo group

- **Missing data:** Increases over time, varies by treatment

## Treatment Effects

The simulated treatment effects are:

- Placebo:

  Slight tumor progression (+5% at Week 4, +8% at Week 12)

- Drug A:

  Tumor regression (-15% at Week 4, -25% at Week 12) with 30%
  non-responders

- Biomarkers:

  Drug A reduces levels by ~40%, Placebo shows slight increase

- Quality of Life:

  Drug A improves scores (+8 points), Placebo slight decline (-2)

- Pain Scores:

  Drug A reduces pain (-1.5 points), Placebo slight increase (+0.5)

## Testing Features

These datasets are designed to test all Advanced Raincloud Plot
features:

**Clinical Significance:**

- Clinical cutoffs (e.g., tumor_size_change = -30 for response)

- Reference ranges (e.g., biomarker_level: 10-50 normal range)

- MCID values (e.g., qol_score MCID = 10 points)

**Effect Size Analysis:**

- Between-group comparisons (Drug A vs Placebo)

- Cohen's d, Hedges' g, Glass's delta calculations

- Multiple timepoint comparisons

**Change Score Analysis:**

- Longitudinal change from baseline

- Responder classifications (20% threshold default)

- Missing data handling

**Biomarker Features:**

- Log-normal distribution requiring transformation

- Outliers (10 extreme values) for outlier handling tests

- CV bands for assay variability assessment

**Publication Features:**

- Sample size annotations

- Missing data reporting

- Statistical comparisons

- Journal-specific formatting

## Usage Examples


    # Basic raincloud plot with clinical cutoff
    advancedraincloud(
      data = advancedraincloud_baseline,
      y_var = "biomarker_level",
      x_var = "treatment_arm",
      clinical_cutoff = 100,
      show_sample_size = TRUE
    )

    # Longitudinal analysis with change scores
    advancedraincloud(
      data = advancedraincloud_data,
      y_var = "tumor_size_change",
      x_var = "time_point",
      id_var = "patient_id",
      fill_var = "treatment_arm",
      show_longitudinal = TRUE,
      show_change_scores = TRUE,
      baseline_group = "Baseline",
      clinical_cutoff = -30,
      show_effect_size = TRUE
    )

    # Biomarker analysis with log transformation
    advancedraincloud(
      data = advancedraincloud_endpoint,
      y_var = "biomarker_level",
      x_var = "treatment_arm",
      log_transform = TRUE,
      outlier_method = "winsorize",
      reference_range_min = 10,
      reference_range_max = 50,
      show_cv_bands = TRUE,
      journal_style = "nature"
    )

    # Quality of life with MCID analysis
    advancedraincloud(
      data = advancedraincloud_data,
      y_var = "qol_score",
      x_var = "time_point",
      fill_var = "treatment_arm",
      show_mcid = TRUE,
      mcid_value = 10,
      show_change_scores = TRUE,
      generate_report = TRUE
    )

    # Likert scale analysis for pain scores
    advancedraincloud(
      data = advancedraincloud_data,
      y_var = "pain_score",
      x_var = "treatment_arm",
      likert_mode = TRUE,
      show_comparisons = TRUE,
      p_value_position = "above"
    )

## Data Generation

These datasets were generated using realistic assumptions:

- Reproducible random seed (42) for consistent results

- Clinically plausible effect sizes and variability

- Realistic missing data patterns typical of clinical trials

- Appropriate correlation structures for repeated measures

- Standard clinical trial demographic distributions

## See also

- [`advancedraincloud`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/advancedraincloud.md)
  for the analysis function

- `vignette("advancedraincloud_examples")` for detailed examples

## Examples

``` r
# Load the datasets
data("advancedraincloud_data")
data("advancedraincloud_baseline")
data("advancedraincloud_endpoint")
data("advancedraincloud_change")

# Explore the structure
str(advancedraincloud_data)
#> tibble [900 × 15] (S3: tbl_df/tbl/data.frame)
#>  $ patient_id             : chr [1:900] "PT001" "PT001" "PT001" "PT002" ...
#>  $ treatment_arm          : Factor w/ 2 levels "Placebo","Drug A": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ time_point             : Factor w/ 3 levels "Baseline","Week 4",..: 1 2 3 1 2 3 1 2 3 1 ...
#>  $ visit_number           : int [1:900] 1 2 3 1 2 3 1 2 3 1 ...
#>  $ age                    : num [1:900] 71 71 71 48 48 48 59 59 59 63 ...
#>  $ gender                 : Factor w/ 2 levels "Female","Male": 2 2 2 2 2 2 1 1 1 2 ...
#>  $ disease_stage          : Factor w/ 2 levels "Early","Advanced": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ biomarker_status       : Factor w/ 2 levels "Negative","Positive": 1 1 1 1 1 1 2 2 2 1 ...
#>  $ tumor_size_change      : num [1:900] 0 NA -21.2 0 NA ...
#>  $ biomarker_level        : num [1:900] 55.6 42.6 42.6 80.7 NA ...
#>  $ qol_score              : num [1:900] 69.9 63.6 63.6 41 37.9 ...
#>  $ pain_score             : Ord.factor w/ 11 levels "0"<"1"<"2"<"3"<..: 5 5 NA 5 5 NA 6 6 NA 9 ...
#>  $ tumor_responder        : Factor w/ 4 levels "Progressive Disease",..: 2 NA 3 2 NA 1 2 3 NA 2 ...
#>  $ age_group              : Factor w/ 2 levels "< 65 years","≥ 65 years": 2 2 2 1 1 1 1 1 1 1 ...
#>  $ baseline_biomarker_high: Factor w/ 2 levels "High","Normal": 2 2 2 2 2 2 2 2 2 1 ...
summary(advancedraincloud_baseline)
#>      patient_id  treatment_arm      age           gender     disease_stage
#>  Length   :300   Placebo:150   Min.   :19.00   Female:116   Early   :212  
#>  N.unique :300   Drug A :150   1st Qu.:47.00   Male  :184   Advanced: 88  
#>  N.blank  :  0                 Median :55.00                              
#>  Min.nchar:  5                 Mean   :54.73                              
#>  Max.nchar:  5                 3rd Qu.:63.00                              
#>                                Max.   :87.00                              
#>                                                                           
#>  biomarker_status tumor_size_change biomarker_level     qol_score    
#>  Negative:183     Min.   :0         Min.   :  11.12   Min.   :20.00  
#>  Positive:117     1st Qu.:0         1st Qu.:  53.02   1st Qu.:42.75  
#>                   Median :0         Median :  92.23   Median :52.28  
#>                   Mean   :0         Mean   : 141.50   Mean   :50.91  
#>                   3rd Qu.:0         3rd Qu.: 167.30   3rd Qu.:60.43  
#>                   Max.   :0         Max.   :1584.03   Max.   :80.00  
#>                                                                      
#>    pain_score            tumor_responder      age_group  
#>  6      :57   Progressive Disease:  0    < 65 years:239  
#>  7      :52   Stable Disease     :300    ≥ 65 years: 61  
#>  5      :50   Partial Response   :  0                    
#>  8      :37   Complete Response  :  0                    
#>  4      :35                                              
#>  9      :31                                              
#>  (Other):38                                              
#>  baseline_biomarker_high
#>  High  :136             
#>  Normal:164             
#>                         
#>                         
#>                         
#>                         
#>                         

# Check missing data patterns
table(
    is.na(advancedraincloud_data$tumor_size_change),
    advancedraincloud_data$time_point,
    advancedraincloud_data$treatment_arm
)
#> , ,  = Placebo
#> 
#>        
#>         Baseline Week 4 Week 12
#>   FALSE      150    130      97
#>   TRUE         0     20      53
#> 
#> , ,  = Drug A
#> 
#>        
#>         Baseline Week 4 Week 12
#>   FALSE      150    134     119
#>   TRUE         0     16      31
#> 

# View response rates by treatment
with(
    advancedraincloud_endpoint,
    table(tumor_responder, treatment_arm, useNA = "ifany")
)
#>                      treatment_arm
#> tumor_responder       Placebo Drug A
#>   Progressive Disease      42      2
#>   Stable Disease           44     28
#>   Partial Response          7     59
#>   Complete Response         4     30
#>   <NA>                     53     31

# Check change score completeness
nrow(advancedraincloud_change) # Complete cases for change analysis
#> [1] 130
```
