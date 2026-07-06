# Edge Cases and Robustness Testing Dataset

Specialized dataset containing various edge cases, extreme values,
special characters, and challenging data patterns. Designed to test the
robustness of enhanced summary implementations, error handling
capabilities, and graceful degradation with problematic data using
summarytools comprehensive error management.

## Usage

``` r
toolssummary_edge_cases
```

## Format

A data frame with 150 observations and 12 variables:

- id:

  Character. Unique identifier (EDGE_001 to EDGE_150)

- scenario:

  Factor. Test scenario type (1-6)

- numeric_extreme:

  Numeric. Variable with extreme values, zeros, and systematic missing

- numeric_decimal:

  Numeric. Variable with high-precision decimals (8 decimal places)

- categorical_many:

  Factor. Categorical variable with 30 different levels

- categorical_few:

  Factor. Binary categorical variable ("A", "B")

- categorical_special:

  Factor. Categories with missing-like values ("N/A", "Missing", "")

- constant_numeric:

  Numeric. Constant value (all 42) for edge case testing

- constant_factor:

  Factor. Constant factor (all "SAME") for edge case testing

- text_variable:

  Character. Text with varying lengths and special characters

- date_variable:

  Date. Date variable spanning 2020-2025

- binary_numeric:

  Integer. Binary numeric variable (0, 1)

## Source

Simulated data generated using create_toolssummary_test_data.R

## Details

This dataset is designed to stress-test enhanced summary implementations
with various edge cases and challenging data patterns that might cause
failures in standard analysis approaches. It tests summarytools
robustness and error handling capabilities.

**Scenario Types:**

- **Scenario 1**: Normal case (baseline comparison)

- **Scenario 2**: Extreme values (very large positive and negative
  numbers)

- **Scenario 3**: Very small values (near-zero decimals)

- **Scenario 4**: High precision decimals and complex numbers

- **Scenario 5**: Zero and edge numeric values

- **Scenario 6**: Systematic missing values

**Quality Challenges:**

- Extreme numeric ranges and precision requirements

- Categorical variables with many levels (30 categories)

- Special characters and empty strings in categorical data

- Systematic missing patterns (every 6th and 7th observation)

- Constant values across all observations

- Text variables with varying lengths and special characters

**summarytools Integration Testing:**

- **dfSummary**: Robustness with problematic data structures

- **freq**: Handling of many categories and special characters

- **descr**: Extreme value processing and missing data management

- **ctable**: Edge case cross-tabulation scenarios

**Recommended Usage Scenarios:**

- Robustness testing for all summary functions

- Error handling validation

- Special character and encoding testing

- Missing data pattern analysis

## See also

[`toolssummary`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toolssummary.md),
[`toolssummary_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/toolssummary_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(toolssummary_edge_cases)

# Robustness testing with edge cases
result <- toolssummary(
  data = toolssummary_edge_cases,
  vars = c("numeric_extreme", "categorical_many", "text_variable"),
  useSummarytools = TRUE,
  showDfSummary = TRUE,
  showFreq = TRUE
)

# Scenario-based analysis
result_scenario <- toolssummary(
  data = toolssummary_edge_cases,
  vars = c("numeric_extreme", "categorical_special"),
  groupVar = "scenario",
  useSummarytools = TRUE,
  showCrosstabs = TRUE
)
} # }
```
