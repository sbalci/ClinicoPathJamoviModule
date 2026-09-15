# Laboratory Results Test Dataset

Simulated laboratory test results with multiple visit timepoints, test
panels, and clinical categories. Designed to test descriptive statistics
tables, visit-based grouping, and laboratory data presentation formats.

## Usage

``` r
tinytable_laboratory_results
```

## Format

A data frame with 180 observations and 15 variables:

- subject_id:

  Character. Unique subject identifier (LAB_0001 to LAB_0180)

- visit:

  Factor. Study visit ("Baseline", "Week 4", "Week 8", "Week 12")

- visit_date:

  Date. Date of laboratory assessment

- lab_category:

  Factor. Clinical interpretation ("Normal", "Borderline", "Abnormal")

- urgency:

  Factor. Test urgency ("Routine", "STAT", "Priority")

- wbc:

  Numeric. White blood cell count (2.0-15.0 x 10^3/uL)

- rbc:

  Numeric. Red blood cell count (3.5-6.0 x 10^6/uL)

- hematocrit:

  Numeric. Hematocrit percentage (30-55%)

- platelets:

  Integer. Platelet count (100-500 x 10^3/uL)

- sodium:

  Integer. Serum sodium (130-150 mEq/L)

- potassium:

  Numeric. Serum potassium (3.0-5.5 mEq/L)

- creatinine:

  Numeric. Serum creatinine (0.5-3.0 mg/dL)

- bun:

  Integer. Blood urea nitrogen (7-50 mg/dL)

- alt:

  Integer. Alanine aminotransferase (5-100 U/L) with ~2% missing values

- ast:

  Integer. Aspartate aminotransferase (5-120 U/L)

## Source

Simulated data generated using create_tinytable_test_data.R

## Details

This dataset represents laboratory test results from a longitudinal
clinical study with multiple visit timepoints. Laboratory values are
within realistic clinical ranges with appropriate inter-test
correlations and visit-to-visit variability.

**Key Features:**

- Complete blood count (CBC) panel

- Basic metabolic panel (chemistry)

- Liver function tests

- Multiple visit timepoints for longitudinal analysis

- Clinical categorization of results

- Date variables for temporal analysis

**Recommended TinyTable Usage:**

- Table Type: "Descriptive Statistics" for laboratory value summaries

- Grouping Variable: visit or lab_category

- Variables: wbc, rbc, sodium, potassium, alt, ast

- Themes: "Clinical" for medical context

## See also

[`tinytable`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tinytable.md),
[`tinytable_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tinytable_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(tinytable_laboratory_results)

# Laboratory values descriptive statistics
result <- tinytable(
  data = tinytable_laboratory_results,
  vars = c("wbc", "rbc", "sodium", "potassium"),
  table_type = "descriptive",
  table_theme = "clinical",
  precision_digits = 2
)

# Laboratory results by visit
result_visit <- tinytable(
  data = tinytable_laboratory_results,
  vars = c("wbc", "hemoglobin", "platelets"),
  group_var = "visit",
  table_type = "grouped"
)
} # }
```
