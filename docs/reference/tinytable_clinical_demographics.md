# Clinical Research Demographics Test Dataset

Simulated clinical research study demographics dataset with treatment
groups, clinical variables, and laboratory values. Designed to test
demographic table generation, grouped summaries, and publication-ready
formatting typical in clinical research publications.

## Usage

``` r
tinytable_clinical_demographics
```

## Format

A data frame with 250 observations and 14 variables:

- patient_id:

  Character. Unique patient identifier (PT_001 to PT_250)

- age:

  Integer. Patient age at enrollment (18-90 years)

- sex:

  Factor. Patient sex ("Male", "Female")

- treatment_group:

  Factor. Treatment assignment ("Control", "Treatment A", "Treatment B")

- study_site:

  Factor. Study enrollment site ("Site_A" to "Site_F")

- bmi:

  Numeric. Body mass index (16-45 kg/m^2) with ~3% missing values

- systolic_bp:

  Integer. Systolic blood pressure (90-200 mmHg)

- diastolic_bp:

  Integer. Diastolic blood pressure (60-120 mmHg)

- diabetes:

  Factor. Diabetes status ("No", "Type 1", "Type 2")

- smoking_status:

  Factor. Smoking history ("Never", "Former", "Current")

- education_level:

  Factor. Education ("Less than HS", "High School", "Some College",
  "Bachelor's", "Graduate")

- hemoglobin:

  Numeric. Hemoglobin level (8-18 g/dL) with sex-based differences

- glucose:

  Integer. Fasting glucose (70-400 mg/dL)

- cholesterol:

  Integer. Total cholesterol (120-350 mg/dL) with ~5% missing values

## Source

Simulated data generated using create_tinytable_test_data.R

## Details

This dataset simulates a typical clinical research study baseline
characteristics table. It includes realistic distributions for
demographic and clinical variables commonly reported in medical
publications, with appropriate missing data patterns and clinical
correlations.

**Key Features:**

- Realistic clinical variable distributions

- Multiple treatment groups for comparison tables

- Sex-specific laboratory value ranges

- Appropriate missing data patterns (3-5%)

- Multiple study sites for subgroup analysis

**Recommended TinyTable Usage:**

- Table Type: "Grouped Summary" or "Descriptive Statistics"

- Grouping Variable: treatment_group or study_site

- Variables: age, sex, bmi, systolic_bp, diabetes, smoking_status

- Themes: "Clinical" or "Publication" for professional appearance

## See also

[`tinytable`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tinytable.md),
[`tinytable_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tinytable_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(tinytable_clinical_demographics)

# Basic demographic summary table
result <- tinytable(
  data = tinytable_clinical_demographics,
  vars = c("age", "sex", "bmi", "diabetes"),
  table_type = "summary",
  table_theme = "clinical"
)

# Grouped comparison by treatment
result_grouped <- tinytable(
  data = tinytable_clinical_demographics,
  vars = c("age", "bmi", "systolic_bp", "hemoglobin"),
  group_var = "treatment_group",
  table_type = "grouped",
  table_theme = "publication"
)
} # }
```
