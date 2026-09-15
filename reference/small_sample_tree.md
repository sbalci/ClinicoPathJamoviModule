# Small Sample Edge Case Dataset

Minimal dataset with very small sample size designed for edge case
testing, validation of statistical methods with limited data, and
assessment of function robustness with minimal observations. Essential
for testing graceful degradation and error handling in medical decision
tree analysis.

## Usage

``` r
small_sample_tree
```

## Format

A data frame with 25 patients and 8 variables:

- patient_id:

  Character. Simple patient identifier (SM_01 to SM_25)

- biomarker_1, biomarker_2:

  Numeric. Simple biomarker measurements

- age:

  Integer. Patient age (years)

- treatment:

  Factor. Treatment assignment ("A", "B")

- stage:

  Factor. Disease stage ("Early", "Advanced")

- outcome:

  Factor. Primary outcome ("No", "Yes")

- cohort:

  Factor. Study cohort ("train", "test")

- sex:

  Factor. Patient sex ("Male", "Female")

- x_coord, y_coord:

  Numeric. Spatial coordinates for testing

## Source

Simulated data generated using create_tree_test_data.R

## Details

This minimal dataset tests the robustness of medical decision tree
analysis with very small sample sizes, which can reveal edge cases in
statistical calculations, visualization algorithms, and clinical
interpretation algorithms.

**Clinical Context:**

- Rare disease studies

- Pilot studies and proof-of-concept

- Method validation with limited data

- Edge case testing and quality assurance

**Key Features:**

- Minimal sample size (N=25)

- Simple variable structure

- Basic categorical and continuous variables

- Limited treatment groups

- Small cohort sizes for testing

**Testing Scenarios:**

- Statistical method robustness with small samples

- Visualization algorithm edge cases

- Clinical interpretation with limited data

- Error handling and graceful degradation

- Minimum sample size requirements

- Algorithm stability testing

**Expected Behaviors:**

- Appropriate handling of small sample statistics

- Clear visualization despite limited data

- Robust clinical interpretation

- Appropriate warnings for limited statistical power

- Graceful handling of edge cases

## See also

[`tree_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tree_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(small_sample_tree)

# Edge case testing
result <- tree(
  data = small_sample_tree,
  vars = c("biomarker_1", "biomarker_2", "age"),
  facs = c("treatment", "stage"),
  target = "outcome",
  targetLevel = "Yes",
  train = "cohort",
  trainLevel = "train",
  clinicalMetrics = TRUE,
  showInterpretation = TRUE
)
} # }
```
