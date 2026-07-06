# Small Sample Size Test Dataset

Minimal dataset with very small sample size to test edge cases with
minimal data, small group sizes, and basic table formatting
functionality.

## Usage

``` r
tinytable_small_sample
```

## Format

A data frame with 15 observations and 6 variables:

- id:

  Integer. Simple identifier (1-15)

- group:

  Factor. Binary grouping variable ("A", "B")

- value_1:

  Numeric. Primary numeric variable with 1 missing value

- value_2:

  Numeric. Secondary numeric variable (0-100 range)

- category:

  Factor. Three-level factor ("X", "Y", "Z")

- flag:

  Factor. Binary flag ("Yes", "No")

## Source

Simulated data generated using create_tinytable_test_data.R

## Details

This minimal dataset tests table formatting with very small sample
sizes, which can reveal edge cases in statistical calculations, grouping
operations, and table layout algorithms.

**Key Features:**

- Only 15 observations total

- Simple variable structure

- Single missing value for testing

- Small group sizes when stratified

- Basic data types only

**Common Use Cases:**

- Testing minimum sample size handling

- Validating small group statistics

- Edge case detection in grouping algorithms

- Minimum viable table formatting

**Recommended TinyTable Usage:**

- Table Type: "Summary" or "Raw Data Display"

- Variables: value_1, value_2, category

- Grouping: group (results in very small subgroups)

- Themes: Any theme for basic formatting testing

## See also

[`tinytable`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tinytable.md),
[`tinytable_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tinytable_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(tinytable_small_sample)

# Basic small sample table
result <- tinytable(
  data = tinytable_small_sample,
  vars = c("value_1", "value_2", "category"),
  table_type = "summary"
)

# Small group analysis
result_grouped <- tinytable(
  data = tinytable_small_sample,
  vars = c("value_1", "value_2"),
  group_var = "group",
  table_type = "descriptive"
)
} # }
```
