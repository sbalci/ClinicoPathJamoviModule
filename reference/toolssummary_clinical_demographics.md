# Clinical Research Demographics with Treatment Groups

Simulated clinical research study demographics dataset with multiple
treatment groups, comprehensive clinical variables, and realistic
missing data patterns. Designed to test demographic table generation,
grouped summaries, cross-tabulations, and publication-ready formatting
typical in clinical research using summarytools enhanced features.

## Usage

``` r
toolssummary_clinical_demographics
```

## Format

A data frame with 300 observations and 16 variables:

- patient_id:

  Character. Unique patient identifier (PT_0001 to PT_0300)

- age:

  Integer. Patient age at enrollment (18-85 years)

- sex:

  Factor. Patient sex ("Male", "Female")

- treatment_group:

  Factor. Treatment assignment ("Control", "Treatment A", "Treatment B")

- study_site:

  Factor. Study enrollment site ("Site_A" to "Site_F")

- bmi:

  Numeric. Body mass index (16-45 kg/m^2) with ~5% missing values

- systolic_bp:

  Integer. Systolic blood pressure (90-200 mmHg)

- diastolic_bp:

  Integer. Diastolic blood pressure (60-120 mmHg)

- diabetes:

  Factor. Diabetes status ("No", "Type 1", "Type 2")

- smoking_status:

  Factor. Smoking history ("Never", "Former", "Current")

- education:

  Ordered Factor. Education level ("Less than HS" \< "High School" \<
  "Some College" \< "Bachelor's" \< "Graduate")

- hemoglobin:

  Numeric. Hemoglobin level (8-18 g/dL) with ~3% missing values

- glucose:

  Integer. Fasting glucose (70-400 mg/dL)

- cholesterol:

  Integer. Total cholesterol (120-350 mg/dL) with ~8% missing values

- enrollment_date:

  Date. Study enrollment date (2023-01-01 to 2024-12-31)

- followup_months:

  Integer. Follow-up duration (1-24 months)

## Source

Simulated data generated using create_toolssummary_test_data.R

## Details

This dataset simulates a comprehensive clinical research study baseline
characteristics table with realistic distributions and appropriate
missing data patterns. It's specifically designed to showcase
summarytools capabilities including dfSummary comprehensive overviews,
freq tables for categorical variables, descr for numeric summaries, and
ctable for cross-tabulations by treatment groups.

**Key Features:**

- Realistic clinical variable distributions with physiological ranges

- Balanced treatment groups for meaningful comparisons

- Multiple categorical variables for frequency analysis

- Ordered factors for proper categorical handling

- Missing data patterns reflecting real clinical studies

- Date variables for temporal analysis

**summarytools Integration Testing:**

- **dfSummary**: Complete data frame overview with variable
  distributions

- **freq**: Categorical variable frequency tables (sex, treatment_group,
  diabetes)

- **descr**: Comprehensive descriptive statistics for numeric variables

- **ctable**: Cross-tabulations by treatment group or study site

**Recommended Usage Scenarios:**

- Grouped summaries by treatment_group or study_site

- Cross-tabulation analysis for categorical associations

- Missing data pattern assessment

- Publication-ready demographic tables

## See also

[`toolssummary`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toolssummary.md),
[`toolssummary_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toolssummary_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(toolssummary_clinical_demographics)

# Basic enhanced summary with summarytools
result <- toolssummary(
  data = toolssummary_clinical_demographics,
  vars = c("age", "sex", "bmi", "diabetes", "treatment_group"),
  useSummarytools = TRUE,
  showDfSummary = TRUE,
  showDescr = TRUE,
  showFreq = TRUE
)

# Grouped analysis by treatment
result_grouped <- toolssummary(
  data = toolssummary_clinical_demographics,
  vars = c("age", "bmi", "systolic_bp", "diabetes"),
  groupVar = "treatment_group",
  useSummarytools = TRUE,
  showCrosstabs = TRUE
)
} # }
```
