# Cardiovascular Risk Assessment Dataset

Simulated cardiovascular risk assessment dataset with traditional risk
factors, patient demographics, and 5-year cardiovascular event outcomes.
Designed to test risk prediction models, clinical decision support, and
cardiovascular screening decision trees.

## Usage

``` r
cardiovascular_risk
```

## Format

A data frame with 400 patients and 13 variables:

- patient_id:

  Character. Unique patient identifier (CVD_0001 to CVD_0400)

- systolic_bp:

  Numeric. Systolic blood pressure (mmHg)

- diastolic_bp:

  Numeric. Diastolic blood pressure (mmHg)

- cholesterol:

  Numeric. Total cholesterol level (mg/dL)

- hdl:

  Numeric. HDL cholesterol level (mg/dL)

- ldl:

  Numeric. LDL cholesterol level (mg/dL)

- triglycerides:

  Numeric. Triglyceride level (mg/dL)

- bmi:

  Numeric. Body mass index (kg/m^2)

- age:

  Integer. Patient age (years)

- smoking:

  Factor. Smoking status ("Never", "Former", "Current")

- diabetes:

  Factor. Diabetes status ("No", "Yes")

- family_history:

  Factor. Family history of cardiovascular disease ("No", "Yes")

- cv_event:

  Factor. Primary outcome - cardiovascular event within 5 years ("No",
  "Yes")

- study_cohort:

  Factor. Study cohort ("training", "testing")

- sex:

  Factor. Patient sex ("Male", "Female")

- ethnicity:

  Factor. Patient ethnicity ("White", "Black", "Hispanic", "Asian",
  "Other")

- x_coord, y_coord:

  Numeric. Spatial coordinates for geographic analysis

## Source

Simulated data generated using create_tree_test_data.R

## Details

This dataset simulates a comprehensive cardiovascular risk assessment
study with traditional risk factors and demographic variables. The
dataset follows established epidemiological patterns for cardiovascular
disease risk factors and outcomes.

**Clinical Context:**

- Cardiovascular disease risk prediction

- Primary prevention screening

- Clinical decision support for risk stratification

- Population health management

**Key Features:**

- Traditional Framingham Risk Score variables

- Realistic distributions of cardiovascular risk factors

- Diverse patient demographics and ethnicities

- Geographic variation modeling with spatial coordinates

- Appropriate missing data patterns (6-12% across variables)

**Recommended Analysis Scenarios:**

- Risk prediction model development

- Clinical decision thresholds optimization

- Population-based risk stratification

- Geographic variation analysis

- Multi-ethnic risk assessment

- Bootstrap validation for confidence intervals

## See also

[`tree_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tree_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(cardiovascular_risk)

# Risk stratification analysis
result <- tree(
  data = cardiovascular_risk,
  vars = c("systolic_bp", "cholesterol", "hdl", "bmi", "age"),
  facs = c("smoking", "diabetes", "family_history"),
  target = "cv_event",
  targetLevel = "Yes",
  train = "study_cohort",
  trainLevel = "training",
  clinicalContext = "screening",
  riskStratification = TRUE,
  showInterpretation = TRUE,
  crossValidation = TRUE
)
} # }
```
