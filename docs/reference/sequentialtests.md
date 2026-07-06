# Sequential Testing Analysis

Analyzes how diagnostic accuracy changes when applying two tests in
sequence, comparing three different testing strategies: serial positive
(confirmation), serial negative (exclusion), and parallel testing.
Provides comprehensive analysis including population flow, cost
implications, and Fagan nomograms.

## Usage

``` r
sequentialtests(
  preset = "custom",
  test1_name = "Screening Test",
  test1_sens = 0.95,
  test1_spec = 0.7,
  test1_cost = 0,
  test2_name = "Confirmatory Test",
  test2_sens = 0.8,
  test2_spec = 0.98,
  test2_cost = 0,
  strategy = "serial_positive",
  prevalence = 0.1,
  population_size = 1000,
  show_explanation = FALSE,
  show_formulas = FALSE,
  show_cost_analysis = FALSE,
  show_nomogram = FALSE
)
```

## Arguments

- preset:

  Select a clinical preset or use custom values. Presets load
  evidence-based test parameters and optimal strategies from medical
  literature.

- test1_name:

  .

- test1_sens:

  .

- test1_spec:

  .

- test1_cost:

  .

- test2_name:

  .

- test2_sens:

  .

- test2_spec:

  .

- test2_cost:

  .

- strategy:

  .

- prevalence:

  .

- population_size:

  Population size used to illustrate population flow counts. Does not
  affect probabilities.

- show_explanation:

  .

- show_formulas:

  .

- show_cost_analysis:

  .

- show_nomogram:

  .

## Value

A results object containing:

|                                     |     |     |     |     |                |
|-------------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`                   |     |     |     |     | a preformatted |
| `results$plain_summary`             |     |     |     |     | a html         |
| `results$summary_table`             |     |     |     |     | a table        |
| `results$individual_tests_table`    |     |     |     |     | a table        |
| `results$population_flow_table`     |     |     |     |     | a table        |
| `results$cost_analysis_table`       |     |     |     |     | a table        |
| `results$explanation_text`          |     |     |     |     | a html         |
| `results$formulas_text`             |     |     |     |     | a html         |
| `results$plot_flow_diagram`         |     |     |     |     | an image       |
| `results$plot_performance`          |     |     |     |     | an image       |
| `results$plot_probability`          |     |     |     |     | an image       |
| `results$plot_population_flow`      |     |     |     |     | an image       |
| `results$plot_sensitivity_analysis` |     |     |     |     | an image       |
| `results$clinical_guidance`         |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary_table$asDF`

`as.data.frame(results$summary_table)`

## Details

This analysis is particularly useful for: • Designing diagnostic
protocols and clinical pathways • Optimizing test sequencing for
specific clinical contexts • Understanding trade-offs between
sensitivity and specificity • Evaluating cost-effectiveness of different
testing strategies • Teaching sequential testing concepts and Bayesian
probability
