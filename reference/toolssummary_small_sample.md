# Small Sample Size Testing Dataset

Minimal dataset with very small sample size to test edge cases with
limited data, small group sizes, and basic functionality with minimal
observations. Designed to validate graceful handling of small samples
and ensure summarytools functions work appropriately with limited data
scenarios.

## Usage

``` r
toolssummary_small_sample
```

## Format

A data frame with 20 observations and 7 variables:

- id:

  Integer. Simple identifier (1-20)

- group:

  Factor. Three-level grouping variable ("A", "B", "C")

- value_numeric:

  Numeric. Primary numeric variable with 1 missing value

- value_integer:

  Integer. Integer variable (1-100 range)

- category_binary:

  Factor. Binary categorical variable ("Yes", "No")

- category_small:

  Factor. Three-level categorical variable ("X", "Y", "Z")

- score:

  Numeric. Score variable (0-10 range) with 1 decimal place

## Source

Simulated data generated using create_toolssummary_test_data.R

## Details

This minimal dataset tests enhanced summary functionality with very
small sample sizes, which can reveal edge cases in statistical
calculations, grouping operations, and summary presentation algorithms.
It validates summarytools behavior with limited data.

**Key Features:**

- Only 20 observations total for minimal data testing

- Simple variable structure with basic data types

- Single missing value for missing data handling

- Small group sizes when stratified (6-7 observations per group)

- Basic data types only (numeric, integer, factor)

**Common Use Cases:**

- Testing minimum sample size handling

- Validating small group statistics

- Edge case detection in grouping algorithms

- Minimum viable summary generation

**summarytools Integration Testing:**

- **dfSummary**: Minimal data overview and visualization

- **freq**: Small sample frequency analysis

- **descr**: Descriptive statistics with limited observations

- **ctable**: Cross-tabulation with small cell counts

**Recommended Usage Scenarios:**

- Small sample behavior validation

- Edge case testing for grouping operations

- Minimum data requirements assessment

- Quality control for small datasets

## See also

[`toolssummary`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toolssummary.md),
[`toolssummary_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toolssummary_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(toolssummary_small_sample)

# Basic small sample analysis
result <- toolssummary(
  data = toolssummary_small_sample,
  vars = c("value_numeric", "category_binary", "score"),
  useSummarytools = TRUE,
  showDfSummary = TRUE
)

# Small group analysis
result_grouped <- toolssummary(
  data = toolssummary_small_sample,
  vars = c("value_numeric", "score"),
  groupVar = "group",
  useSummarytools = TRUE,
  showCrosstabs = TRUE
)
} # }
```
