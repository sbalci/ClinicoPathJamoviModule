# Example Liver Pathology Data for GEE Analysis

Simulated dataset of liver biopsies from dogs with multiple samples per
subject. This dataset is designed to demonstrate Generalized Estimating
Equations (GEE) analysis for correlated/clustered data common in
pathology studies.

## Usage

``` r
gee_liver_data
```

## Format

A data frame with 135 observations and 10 variables:

- dog_id:

  Factor. Unique identifier for each dog (cluster ID)

- sample_number:

  Integer. Sequential sample number for each dog (1-4)

- time_point:

  Integer. Time sequence for longitudinal analysis

- age:

  Numeric. Dog's age in years

- breed:

  Factor. Dog breed (Labrador, German Shepherd, Poodle, Mixed)

- sample_method:

  Factor. Biopsy method (Fine Needle, Core Biopsy, Surgical)

- pathologist_experience:

  Numeric. Years of pathologist experience (5, 10, 15, 20)

- fibrosis_score:

  Numeric. Liver fibrosis score (0-6 scale, continuous)

- diagnosis:

  Factor. Binary diagnosis (Negative, Positive)

- cell_count:

  Integer. Inflammatory cell count (Poisson distributed)

## Source

Simulated data generated using `data-raw/gee_example_data.R`

## Details

**Study Design:**

- 50 dogs with 2-4 liver samples each (unbalanced design)

- Samples from the same dog are correlated

- Represents typical veterinary pathology study with multiple biopsies

**Clinical Context:** In veterinary pathology, multiple liver samples
are often taken from the same animal to assess disease distribution and
severity. These samples are not independent, requiring GEE or mixed
models for proper statistical analysis.

**Correlation Structure:**

- **Within-dog correlation**: Samples from the same dog share baseline
  characteristics

- **Exchangeable structure**: Recommended for this data (samples equally
  correlated)

- **AR(1) structure**: Can be used if time_point represents sequential
  sampling

**Example Analyses:**

1.  **Binary Outcome (Diagnosis):**

        # Predicting diagnosis from fibrosis score and age
        geemodel(
          data = gee_liver_data,
          outcome = 'diagnosis',
          predictors = c('fibrosis_score', 'age', 'sample_method'),
          cluster_id = 'dog_id',
          family = 'binomial',
          corstr = 'exchangeable',
          robust_se = TRUE
        )

2.  **Count Outcome (Cell Count):**

        # Modeling inflammatory cell count
        geemodel(
          data = gee_liver_data,
          outcome = 'cell_count',
          predictors = c('fibrosis_score', 'breed'),
          cluster_id = 'dog_id',
          family = 'poisson',
          corstr = 'exchangeable',
          robust_se = TRUE
        )

3.  **Continuous Outcome (Fibrosis Score):**

        # Predicting fibrosis from age and sample method
        geemodel(
          data = gee_liver_data,
          outcome = 'fibrosis_score',
          predictors = c('age', 'sample_method', 'pathologist_experience'),
          cluster_id = 'dog_id',
          family = 'gaussian',
          corstr = 'exchangeable',
          robust_se = TRUE
        )

**Why GEE is Needed:**

- Standard regression assumes independent observations

- Multiple samples per dog violate independence assumption

- GEE accounts for within-dog correlation

- Provides valid standard errors and inference

**Data Generation:** Data was simulated with realistic correlations:

- Dogs with higher age tend to have higher fibrosis scores

- Samples from the same dog share baseline disease severity

- Diagnosis probability increases with fibrosis score and age

- Sample method affects diagnostic accuracy

## See also

- [`geemodel()`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/geemodel.md)
  for GEE analysis function

- [`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html) for
  the underlying GEE implementation

## Examples

``` r
# \donttest{
# Load data
data(gee_liver_data, package = "ClinicoPath")

# View structure
str(gee_liver_data)
#> 'data.frame':    125 obs. of  10 variables:
#>  $ dog_id                : Factor w/ 50 levels "Dog_001","Dog_002",..: 1 1 2 2 3 3 4 4 5 5 ...
#>  $ sample_number         : int  1 2 1 2 1 2 1 2 1 2 ...
#>  $ time_point            : int  1 2 1 2 1 2 1 2 1 2 ...
#>  $ age                   : num  8.7 8.7 3.6 3.6 0.7 0.7 5.2 5.2 7 7 ...
#>  $ breed                 : Factor w/ 4 levels "German Shepherd",..: 2 2 3 3 2 2 3 3 2 2 ...
#>  $ sample_method         : Factor w/ 3 levels "Core Biopsy",..: 1 1 1 2 1 2 1 3 2 1 ...
#>  $ pathologist_experience: num  20 15 15 5 10 5 15 10 10 15 ...
#>  $ fibrosis_score        : num  3.6 3.9 3.5 2.3 0.1 0.9 0.2 0.4 1 0 ...
#>  $ diagnosis             : Factor w/ 2 levels "Negative","Positive": 1 1 2 1 2 1 1 1 1 1 ...
#>  $ cell_count            : int  8 15 12 6 2 3 7 5 4 4 ...

# Check cluster sizes
table(table(gee_liver_data$dog_id))
#> 
#>  2  3  4 
#> 30 15  5 

# Summary statistics by diagnosis
aggregate(fibrosis_score ~ diagnosis, data = gee_liver_data,
          FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                              sd = sd(x, na.rm = TRUE)))
#>   diagnosis fibrosis_score.mean fibrosis_score.sd
#> 1  Negative            1.535211          1.065444
#> 2  Positive            2.255769          1.112745
# }
```
