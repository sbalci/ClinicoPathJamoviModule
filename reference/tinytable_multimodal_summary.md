# Multi-Modal Data Summary Test Dataset

Mixed data types dataset with continuous, ordinal, categorical, and date
variables. Designed to test comprehensive table formatting with diverse
variable types, complex missing patterns, and mixed-scale measurements.

## Usage

``` r
tinytable_multimodal_summary
```

## Format

A data frame with 200 observations and 13 variables:

- record_id:

  Integer. Record identifier (1-200)

- data_source:

  Factor. Data origin ("EHR", "Registry", "Clinical_Trial", "Survey")

- region:

  Factor. Geographic region ("North", "South", "East", "West",
  "Central")

- score_1:

  Numeric. Scale score 0-100 with 1 decimal place

- score_2:

  Numeric. Rating scale 1-5 with 2 decimal places and ~8% missing

- measurement_a:

  Numeric. Measurement in arbitrary units (50-300) with ~4% missing

- severity:

  Ordered Factor. Severity level ("Mild" \< "Moderate" \< "Severe")

- priority:

  Ordered Factor. Priority level ("Low" \< "Medium" \< "High" \<
  "Critical")

- flag_positive:

  Factor. Binary flag ("Yes", "No")

- quality_check:

  Factor. Quality assessment ("Pass", "Fail")

- category_type:

  Factor. Category classification ("Type_A" to "Type_H")

- start_date:

  Date. Study start date

- end_date:

  Date. Study end date

## Source

Simulated data generated using create_tinytable_test_data.R

## Details

This dataset is specifically designed to test tinytable's ability to
handle mixed data types in a single table. It includes variables with
different scales, ordinal factors, dates, and complex missing patterns.

**Key Features:**

- Multiple continuous scales (0-100, 1-5, 50-300)

- Ordinal factors with proper level ordering

- Binary and multi-category factors

- Date variables for temporal data

- Complex missing data patterns

- Multiple data sources for subgroup analysis

**Recommended TinyTable Usage:**

- Table Type: "Data Summary" or "Custom Format"

- Grouping Variable: data_source or region

- Variables: score_1, score_2, severity, priority

- Themes: "Modern" for contemporary aesthetics

## See also

[`tinytable`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tinytable.md),
[`tinytable_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tinytable_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(tinytable_multimodal_summary)

# Mixed data types summary
result <- tinytable(
  data = tinytable_multimodal_summary,
  vars = c("score_1", "score_2", "severity", "priority"),
  table_type = "summary",
  show_missing = TRUE
)

# Data source comparison
result_source <- tinytable(
  data = tinytable_multimodal_summary,
  vars = c("score_1", "measurement_a"),
  group_var = "data_source",
  table_type = "descriptive"
)
} # }
```
