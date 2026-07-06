# Cancer Biomarker Diagnosis Dataset

Simulated prostate cancer diagnosis dataset with comprehensive biomarker
panels, patient demographics, and clinical variables. Designed to test
biomarker-based decision tree classification, feature importance
analysis, and clinical interpretation in oncology diagnostics.

## Usage

``` r
cancer_biomarkers
```

## Format

A data frame with 500 patients and 11 variables:

- patient_id:

  Character. Unique patient identifier (PAT_0001 to PAT_0500)

- PSA:

  Numeric. Prostate-specific antigen level (ng/mL)

- age:

  Integer. Patient age at diagnosis (years)

- tumor_size:

  Numeric. Tumor size measurement (cm)

- grade:

  Factor. Tumor grade classification ("Low", "Intermediate", "High")

- stage:

  Factor. Cancer stage (I, II, III, IV)

- diagnosis:

  Factor. Primary outcome - disease classification ("benign", "cancer")

- cohort:

  Factor. Study cohort for train/test split ("discovery", "validation")

- sex:

  Factor. Patient sex ("Male", "Female")

- biopsy_gleason:

  Integer. Gleason score from biopsy (6-10, cancer cases only)

- x_coord, y_coord:

  Numeric. Spatial coordinates for autocart spatial analysis

## Source

Simulated data generated using create_tree_test_data.R

## Details

This dataset simulates a comprehensive prostate cancer biomarker study
with realistic distributions of PSA levels, patient demographics, and
clinical characteristics. The dataset includes both benign and malignant
cases with appropriate biomarker patterns for testing medical decision
tree algorithms.

**Clinical Context:**

- Prostate cancer screening and diagnosis

- Biomarker-based clinical decision support

- Multi-modal diagnostic approach combining clinical and laboratory data

- Risk stratification for treatment planning

**Key Features:**

- Realistic PSA distributions (normal, elevated, very high)

- Age-appropriate patient demographics

- Tumor grading and staging information

- Discovery/validation cohort split for model testing

- Spatial coordinates for autocart spatial analysis

- Realistic missing data patterns (5-15% across variables)

**Recommended Analysis Scenarios:**

- Basic decision tree classification for diagnosis

- Feature importance analysis for biomarker selection

- Cross-validation performance assessment

- Clinical interpretation of decision rules

- Risk stratification based on biomarker combinations

- Spatial analysis using autocart methodology

## See also

[`tree_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tree_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(cancer_biomarkers)

# Basic tree analysis
result <- tree(
  data = cancer_biomarkers,
  vars = c("PSA", "age", "tumor_size"),
  facs = c("grade", "stage"),
  target = "diagnosis",
  targetLevel = "cancer",
  train = "cohort",
  trainLevel = "discovery",
  clinicalMetrics = TRUE,
  featureImportance = TRUE,
  showInterpretation = TRUE
)

# Advanced analysis with spatial coordinates
result_spatial <- tree(
  data = cancer_biomarkers,
  vars = c("PSA", "age"),
  facs = c("grade", "stage"),
  target = "diagnosis",
  targetLevel = "cancer",
  spatialCoords = c("x_coord", "y_coord"),
  useAutocart = TRUE,
  compareModels = TRUE
)
} # }
```
