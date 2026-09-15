# Edge Cases and Quality Testing Dataset

Specialized dataset containing various edge cases, extreme values,
special characters, and challenging data patterns. Designed to test the
robustness of table formatting implementations and error handling
capabilities.

## Usage

``` r
tinytable_edge_cases
```

## Format

A data frame with 100 observations and 10 variables:

- id:

  Character. Unique identifier (EDGE_001 to EDGE_100)

- scenario:

  Factor. Test scenario type (1-6)

- numeric_var_1:

  Numeric. Variable with extreme values, zeros, and systematic missing

- numeric_var_2:

  Numeric. Variable with negative values, small decimals, and irrational
  numbers

- categorical_var_1:

  Factor. Categorical variable with missing and extreme categories

- categorical_var_2:

  Factor. Standard categorical variable

- special_characters:

  Factor. Categories with special characters and Unicode

- constant_numeric:

  Numeric. Constant value (all 42) for edge case testing

- constant_factor:

  Factor. Constant factor (all "SAME") for edge case testing

- many_categories:

  Factor. Factor with 25 different levels

## Source

Simulated data generated using create_tinytable_test_data.R

## Details

This dataset is designed to stress-test table formatting implementations
with various edge cases and challenging data patterns:

**Scenario Types:**

- **Scenario 1**: Normal case (baseline comparison)

- **Scenario 2**: Extreme values (very large positive and negative
  numbers)

- **Scenario 3**: Very small values (near-zero decimals)

- **Scenario 4**: Many decimal places and irrational numbers

- **Scenario 5**: Zero and negative zero edge cases

- **Scenario 6**: All missing values

**Quality Issues:**

- Special characters in category names (@#\$, Unicode, spaces)

- Systematic missing patterns (every 5th and 7th observation)

- Constant values across all observations

- Factors with many levels (25 categories)

- Extreme numeric ranges (0.0001 to 10,000)

**Recommended TinyTable Usage:**

- All table types for robustness testing

- Error handling validation

- Special character handling

- Missing data pattern testing

## See also

[`tinytable`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tinytable.md),
[`tinytable_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tinytable_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(tinytable_edge_cases)

# Test robustness with edge cases
result <- tinytable(
  data = tinytable_edge_cases,
  vars = c("numeric_var_1", "numeric_var_2", "categorical_var_1"),
  table_type = "summary",
  show_missing = TRUE,
  precision_digits = 4
)

# Test scenario-based grouping
result_scenario <- tinytable(
  data = tinytable_edge_cases,
  vars = c("numeric_var_1", "special_characters"),
  group_var = "scenario",
  table_type = "raw"
)
} # }
```
