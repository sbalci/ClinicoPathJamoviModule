# Mixed Data Types with Complex Categorical Variables

Comprehensive dataset containing continuous, ordinal, categorical, and
date variables with complex missing patterns and diverse scales.
Designed to test enhanced table formatting capabilities with mixed
variable types, ordered factors, and sophisticated categorical analysis
using summarytools comprehensive functionality.

## Usage

``` r
toolssummary_mixed_datatypes
```

## Format

A data frame with 250 observations and 15 variables:

- record_id:

  Integer. Record identifier (1-250)

- data_source:

  Factor. Data origin ("EHR", "Registry", "Clinical_Trial", "Survey")

- region:

  Factor. Geographic region ("North", "South", "East", "West",
  "Central")

- score_continuous:

  Numeric. Continuous scale score (0-100) with 1 decimal place

- score_ordinal:

  Integer. Ordinal rating scale (1-5) with ~8% missing

- measurement_value:

  Numeric. Measurement in arbitrary units (50-300) with ~5% missing

- severity:

  Ordered Factor. Severity level ("Mild" \< "Moderate" \< "Severe")

- priority_level:

  Ordered Factor. Priority ("Low" \< "Medium" \< "High" \< "Critical")

- binary_flag:

  Factor. Binary indicator ("Yes", "No")

- quality_status:

  Factor. Quality assessment ("Pass", "Fail", "Pending")

- category_multi:

  Factor. Multi-level category ("Type_A" to "Type_H")

- assessment_date:

  Date. Assessment date (2024-01-01 to 2024-12-31)

- completion_date:

  Date. Completion date (2024-02-01 to 2025-01-31) with ~12% missing

- count_variable:

  Integer. Count data (0-20)

- percentage_score:

  Numeric. Percentage score (0-100) with 1 decimal place

## Source

Simulated data generated using create_toolssummary_test_data.R

## Details

This dataset is specifically designed to test summarytools ability to
handle diverse data types in a single comprehensive analysis. It
includes variables with different scales, measurement levels, and
complex categorical structures that challenge standard summary
approaches.

**Key Features:**

- Multiple continuous scales with different ranges and precisions

- Properly ordered factors with meaningful level hierarchies

- Binary and multi-category nominal factors

- Date variables for temporal analysis

- Complex missing data patterns across variable types

- Count data and percentage scores

**summarytools Integration Testing:**

- **dfSummary**: Comprehensive overview handling all data types

- **freq**: Advanced frequency analysis for categorical and ordinal
  variables

- **descr**: Extended descriptive statistics for continuous variables

- **ctable**: Cross-tabulations across different categorical structures

**Recommended Usage Scenarios:**

- Mixed data type comprehensive summaries

- Ordered factor analysis and presentation

- Multi-source data integration assessment

- Quality control and data validation

## See also

[`toolssummary`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toolssummary.md),
[`toolssummary_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toolssummary_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(toolssummary_mixed_datatypes)

# Comprehensive mixed data analysis
result <- toolssummary(
  data = toolssummary_mixed_datatypes,
  vars = c("score_continuous", "severity", "priority_level", "binary_flag"),
  useSummarytools = TRUE,
  showDfSummary = TRUE,
  showDescr = TRUE,
  showFreq = TRUE
)

# Analysis by data source
result_source <- toolssummary(
  data = toolssummary_mixed_datatypes,
  vars = c("score_continuous", "measurement_value", "severity"),
  groupVar = "data_source",
  useSummarytools = TRUE,
  showCrosstabs = TRUE
)
} # }
```
