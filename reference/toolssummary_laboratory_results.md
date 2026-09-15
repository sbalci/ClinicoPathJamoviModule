# Laboratory Results with Longitudinal Structure

Simulated laboratory test results dataset with multiple visit
timepoints, comprehensive lab panels, and clinical categories. Designed
to test descriptive statistics tables, visit-based grouping,
longitudinal data presentation, and laboratory data summaries using
summarytools enhanced functionality.

## Usage

``` r
toolssummary_laboratory_results
```

## Format

A data frame with 200 observations and 15 variables:

- subject_id:

  Character. Unique subject identifier (LAB_0001 to LAB_0050, repeated)

- visit:

  Factor. Study visit ("Baseline", "Week 4", "Week 8", "Week 12")

- visit_date:

  Date. Date of laboratory assessment

- lab_category:

  Factor. Clinical interpretation ("Normal", "Borderline", "Abnormal")

- urgency:

  Factor. Test urgency level ("Routine", "STAT", "Priority")

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

  Numeric. Serum creatinine (0.5-3.0 mg/dL) with ~4% missing

- bun:

  Integer. Blood urea nitrogen (7-50 mg/dL)

- alt:

  Integer. Alanine aminotransferase (5-100 U/L) with ~2% missing

- ast:

  Integer. Aspartate aminotransferase (5-120 U/L)

## Source

Simulated data generated using create_toolssummary_test_data.R

## Details

This dataset represents laboratory test results from a longitudinal
clinical study with 50 subjects followed across 4 timepoints. Laboratory
values are within realistic clinical ranges with appropriate inter-test
correlations and visit-to-visit variability. Perfect for testing
summarytools capabilities with repeated measures data.

**Key Features:**

- Complete blood count (CBC) and chemistry panels

- Longitudinal structure with 4 visit timepoints

- Clinical categorization and urgency levels

- Realistic laboratory value ranges and correlations

- Date variables for temporal analysis

- Missing data patterns reflecting real laboratory studies

**summarytools Integration Testing:**

- **dfSummary**: Laboratory panel overview with value distributions

- **freq**: Categorical analysis of lab categories and urgency levels

- **descr**: Comprehensive statistics for all laboratory values

- **ctable**: Cross-tabulations by visit or clinical categories

**Recommended Usage Scenarios:**

- Laboratory value summaries by visit timepoint

- Normal vs abnormal result analysis

- Longitudinal laboratory trend assessment

- Clinical decision support data presentation

## See also

[`toolssummary`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toolssummary.md),
[`toolssummary_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toolssummary_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(toolssummary_laboratory_results)

# Laboratory values summary with enhanced statistics
result <- toolssummary(
  data = toolssummary_laboratory_results,
  vars = c("wbc", "rbc", "sodium", "potassium", "alt"),
  useSummarytools = TRUE,
  showDescr = TRUE,
  showDfSummary = TRUE
)

# Analysis by visit timepoint
result_visit <- toolssummary(
  data = toolssummary_laboratory_results,
  vars = c("wbc", "hematocrit", "platelets", "creatinine"),
  groupVar = "visit",
  useSummarytools = TRUE,
  showCrosstabs = TRUE
)
} # }
```
