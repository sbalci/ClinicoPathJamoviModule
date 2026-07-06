# Laboratory Method Validation Protocols

Comprehensive analytical method validation following CLSI EP15-A3,
EP05-A3, and ISO 15189 guidelines. Validates precision, accuracy,
linearity, measuring interval, and analytical measurement range for
clinical laboratory methods. Supports regulatory submissions and
laboratory accreditation. Essential for establishing method performance
characteristics.

## Usage

``` r
methodvalidation(
  data,
  measurement,
  reference_value,
  replicate,
  concentration_level,
  operator,
  instrument,
  validation_type = "full_validation",
  precision_design = "ep15a3",
  replicates_per_run = 3,
  number_of_runs = 20,
  confidence_level = 0.95,
  acceptance_criteria = "biological_variation",
  custom_cv_limit = 10,
  custom_bias_limit = 5,
  linearity_points = 7,
  outlier_detection = TRUE,
  precision_analysis = TRUE,
  accuracy_analysis = TRUE,
  linearity_analysis = TRUE,
  limit_of_detection = FALSE,
  limit_of_quantitation = FALSE,
  interference_testing = FALSE,
  carryover_assessment = FALSE,
  reference_interval = FALSE,
  uncertainty_budget = TRUE,
  validation_plots = TRUE,
  method_comparison_plot = TRUE,
  export_report = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- measurement:

  Test method measurement values

- reference_value:

  Reference method or target concentration values

- replicate:

  Replicate measurement identifier

- concentration_level:

  Concentration level identifier (e.g., Low, Medium, High)

- operator:

  Operator or analyst identifier for intermediate precision

- instrument:

  Instrument identifier for ruggedness testing

- validation_type:

  Type of validation study to perform

- precision_design:

  Experimental design for precision evaluation

- replicates_per_run:

  Number of replicates per run for precision study

- number_of_runs:

  Number of runs for precision study

- confidence_level:

  Confidence level for statistical calculations

- acceptance_criteria:

  Source for method acceptance criteria

- custom_cv_limit:

  Custom coefficient of variation acceptance limit

- custom_bias_limit:

  Custom bias acceptance limit

- linearity_points:

  Number of concentration points for linearity study

- outlier_detection:

  Apply statistical outlier detection methods

- precision_analysis:

  Perform comprehensive precision evaluation

- accuracy_analysis:

  Perform bias and accuracy assessment

- linearity_analysis:

  Perform linearity and measuring interval evaluation

- limit_of_detection:

  Estimate analytical limit of detection

- limit_of_quantitation:

  Estimate analytical limit of quantitation

- interference_testing:

  Evaluate potential analytical interferences

- carryover_assessment:

  Assess sample carryover effects

- reference_interval:

  Verify established reference intervals

- uncertainty_budget:

  Calculate comprehensive uncertainty budget

- validation_plots:

  Generate validation study visualizations

- method_comparison_plot:

  Create method comparison visualizations

- export_report:

  Generate comprehensive validation report

## Value

A results object containing:

|                             |     |     |     |     |         |
|-----------------------------|-----|-----|-----|-----|---------|
| `results$instructions`      |     |     |     |     | a html  |
| `results$summary`           |     |     |     |     | a table |
| `results$precisionResults`  |     |     |     |     | a table |
| `results$accuracyResults`   |     |     |     |     | a table |
| `results$methodExplanation` |     |     |     |     | a html  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
