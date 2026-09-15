# Pediatric Growth Assessment Dataset

Simulated pediatric growth and development assessment dataset with
growth measurements, developmental scores, and demographic factors.
Designed to test pediatric-specific decision trees, growth delay
prediction, and developmental assessment models.

## Usage

``` r
pediatric_growth
```

## Format

A data frame with 200 children and 12 variables:

- child_id:

  Character. Unique child identifier (PED_0001 to PED_0200)

- height_cm:

  Numeric. Height measurement (cm)

- weight_kg:

  Numeric. Weight measurement (kg)

- head_circumference:

  Numeric. Head circumference (cm)

- age_months:

  Integer. Age in months

- motor_score:

  Integer. Motor development score

- cognitive_score:

  Integer. Cognitive development score

- sex:

  Factor. Child sex ("Male", "Female")

- birth_weight:

  Factor. Birth weight category ("Normal", "Low", "Very Low")

- gestational_age:

  Factor. Gestational age ("Term", "Preterm")

- growth_delay:

  Factor. Primary outcome - growth delay status ("Normal", "Delayed")

- study_site:

  Factor. Study site ("site_A", "site_B")

- maternal_age:

  Integer. Maternal age at birth (years)

- socioeconomic_status:

  Factor. Socioeconomic status ("Low", "Medium", "High")

- x_coord, y_coord:

  Numeric. Geographic coordinates for population analysis

## Source

Simulated data generated using create_tree_test_data.R

## Details

This dataset simulates a comprehensive pediatric growth and development
study with anthropometric measurements, developmental assessments, and
demographic factors. The dataset reflects realistic patterns of
pediatric growth and development outcomes.

**Clinical Context:**

- Pediatric growth monitoring

- Developmental delay screening

- Early childhood assessment

- Population health surveillance

**Key Features:**

- Age-appropriate growth measurements

- Developmental assessment scores

- Birth history and demographic factors

- Socioeconomic considerations

- Multi-site study design

- Geographic variation modeling

**Recommended Analysis Scenarios:**

- Growth delay prediction models

- Developmental screening tools

- Multi-site validation studies

- Socioeconomic impact analysis

- Age-stratified decision trees

- Missing data handling in pediatric studies

## See also

[`tree_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tree_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(pediatric_growth)

# Growth delay prediction
result <- tree(
  data = pediatric_growth,
  vars = c("height_cm", "weight_kg", "head_circumference", "age_months"),
  facs = c("sex", "birth_weight", "gestational_age"),
  target = "growth_delay",
  targetLevel = "Delayed",
  train = "study_site",
  trainLevel = "site_A",
  clinicalContext = "screening",
  imputeMissing = TRUE,
  showInterpretation = TRUE
)
} # }
```
