# Measurement Uncertainty Estimation

Comprehensive measurement uncertainty evaluation following ISO/IEC Guide
98-3 (GUM), and JCGM 100:2008 guidelines. Calculates Type A and Type B
uncertainties, combined and expanded uncertainty, and uncertainty
budgets for clinical laboratory measurements. Essential for laboratory
accreditation and quality assurance.

## Usage

``` r
measurementuncertainty(
  data,
  measurement,
  reference_value,
  replicate_group,
  calibrator,
  operator,
  instrument,
  uncertainty_method = "gum_approach",
  confidence_level = 0.95,
  coverage_factor = 2,
  type_a_components = "repeatability,intermediate_precision,operator_variability",
  type_b_components =
    "calibration_uncertainty,reference_material_uncertainty,temperature_effects",
  calibration_uncertainty = 1,
  reference_material_uncertainty = 0.5,
  temperature_uncertainty = 0.1,
  pipetting_uncertainty = 0.2,
  sample_stability_uncertainty = 0.3,
  matrix_effects_uncertainty = 0.2,
  monte_carlo_simulations = 1e+05,
  distribution_types = "normal,uniform,triangular",
  correlation_analysis = TRUE,
  sensitivity_analysis = TRUE,
  budget_optimization = TRUE,
  measurement_model = TRUE,
  validation_experiments = FALSE,
  proficiency_testing_data = FALSE,
  interlaboratory_comparison = FALSE,
  clinical_significance = TRUE,
  iso15189_compliance = TRUE,
  uncertainty_plots = TRUE,
  budget_plots = TRUE,
  monte_carlo_plots = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- measurement:

  Laboratory measurement values for uncertainty analysis

- reference_value:

  Reference or certified values for bias estimation

- replicate_group:

  Identifier for measurement replicates or runs

- calibrator:

  Calibrator or standard identifier

- operator:

  Operator identifier for operator variability

- instrument:

  Instrument identifier for instrument variability

- uncertainty_method:

  Method for measurement uncertainty evaluation

- confidence_level:

  Confidence level for expanded uncertainty

- coverage_factor:

  Coverage factor for expanded uncertainty calculation

- type_a_components:

  Comma-separated list of Type A uncertainty components

- type_b_components:

  Comma-separated list of Type B uncertainty components

- calibration_uncertainty:

  Relative uncertainty from calibration

- reference_material_uncertainty:

  Uncertainty of reference materials

- temperature_uncertainty:

  Uncertainty due to temperature variations

- pipetting_uncertainty:

  Uncertainty from pipetting operations

- sample_stability_uncertainty:

  Uncertainty due to sample stability

- matrix_effects_uncertainty:

  Uncertainty from matrix effects

- monte_carlo_simulations:

  Number of Monte Carlo simulations

- distribution_types:

  Distribution types for uncertainty components

- correlation_analysis:

  Analyze correlations between uncertainty components

- sensitivity_analysis:

  Perform sensitivity analysis of uncertainty components

- budget_optimization:

  Optimize uncertainty budget for cost-effectiveness

- measurement_model:

  Define and validate measurement model

- validation_experiments:

  Design experiments to validate uncertainty estimates

- proficiency_testing_data:

  Incorporate proficiency testing results

- interlaboratory_comparison:

  Compare uncertainty with other laboratories

- clinical_significance:

  Evaluate clinical significance of uncertainty

- iso15189_compliance:

  Verify compliance with laboratory accreditation requirements

- uncertainty_plots:

  Generate uncertainty analysis plots

- budget_plots:

  Create uncertainty budget visualizations

- monte_carlo_plots:

  Generate Monte Carlo simulation plots

## Value

A results object containing:

|                              |     |     |     |     |         |
|------------------------------|-----|-----|-----|-----|---------|
| `results$instructions`       |     |     |     |     | a html  |
| `results$summary`            |     |     |     |     | a table |
| `results$uncertaintyResults` |     |     |     |     | a table |
| `results$budgetTable`        |     |     |     |     | a table |
| `results$methodExplanation`  |     |     |     |     | a html  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
