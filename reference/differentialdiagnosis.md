# Differential Diagnosis Assistance

Differential Diagnosis Assistance provides Bayesian diagnostic reasoning
with multi-factorial clinical presentation analysis to generate ranked
differential diagnoses with likelihood ratios and diagnostic
probabilities for evidence-based clinical decision support and
diagnostic accuracy optimization.

## Usage

``` r
differentialdiagnosis(
  data,
  clinicalFindings,
  confirmedDiagnosis,
  demographicVars,
  labResults,
  imagingFindings,
  diagnostic_probability = TRUE,
  likelihood_ratios = TRUE,
  prevalence_adjustment = TRUE,
  differential_ranking = TRUE,
  bayesian_network = FALSE,
  reasoning_method = "naive_bayes",
  prevalence_source = "population_based",
  confidence_threshold = 0.5,
  max_diagnoses = 10,
  clinical_context = TRUE,
  uncertainty_analysis = TRUE,
  sensitivity_analysis = FALSE,
  confidence_level = 0.95,
  diagnostic_plots = TRUE,
  network_diagram = FALSE,
  probability_heatmap = FALSE,
  clinical_guidelines = TRUE,
  differential_explanation = TRUE
)
```

## Arguments

- data:

  .

- clinicalFindings:

  Clinical findings, symptoms, signs, and test results for diagnostic
  reasoning

- confirmedDiagnosis:

  Confirmed diagnosis variable for model training and validation
  (optional)

- demographicVars:

  Patient demographic variables (age, gender, ethnicity) affecting
  disease prevalence

- labResults:

  Laboratory test results for diagnostic probability calculation
  (optional)

- imagingFindings:

  Imaging findings and radiological results (optional)

- diagnostic_probability:

  Calculate Bayesian diagnostic probabilities for differential diagnoses

- likelihood_ratios:

  Calculate positive and negative likelihood ratios for diagnostic tests

- prevalence_adjustment:

  Adjust probabilities based on disease prevalence and demographics

- differential_ranking:

  Rank differential diagnoses by probability and clinical significance

- bayesian_network:

  Advanced Bayesian network modeling for complex diagnostic
  relationships

- reasoning_method:

  Statistical method for diagnostic probability calculation

- prevalence_source:

  Source of disease prevalence estimates for prior probability
  calculation

- confidence_threshold:

  Minimum confidence threshold for diagnostic recommendations

- max_diagnoses:

  Maximum number of differential diagnoses to display in results

- clinical_context:

  Integrate clinical context and patient history in diagnostic reasoning

- uncertainty_analysis:

  Analyze and report diagnostic uncertainty and confidence intervals

- sensitivity_analysis:

  Perform sensitivity analysis for key diagnostic parameters

- confidence_level:

  Confidence level for diagnostic probability intervals

- diagnostic_plots:

  Create diagnostic probability and likelihood ratio visualizations

- network_diagram:

  Create network diagram of diagnostic relationships

- probability_heatmap:

  Create heatmap of diagnostic probabilities across findings

- clinical_guidelines:

  Integrate evidence-based diagnostic guidelines and recommendations

- differential_explanation:

  Provide detailed explanation of diagnostic reasoning process

## Value

A results object containing:

|                                   |     |     |     |     |          |
|-----------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                    |     |     |     |     | a html   |
| `results$summary`                 |     |     |     |     | a html   |
| `results$diagnosticProbabilities` |     |     |     |     | a table  |
| `results$likelihoodRatios`        |     |     |     |     | a table  |
| `results$bayesianAnalysis`        |     |     |     |     | a table  |
| `results$uncertaintyAnalysis`     |     |     |     |     | a table  |
| `results$clinicalContext`         |     |     |     |     | a table  |
| `results$sensitivityAnalysis`     |     |     |     |     | a table  |
| `results$modelPerformance`        |     |     |     |     | a table  |
| `results$diagnosticPlots`         |     |     |     |     | an image |
| `results$networkDiagram`          |     |     |     |     | an image |
| `results$probabilityHeatmap`      |     |     |     |     | an image |
| `results$clinicalGuidelines`      |     |     |     |     | a html   |
| `results$differentialExplanation` |     |     |     |     | a html   |
| `results$technicalNotes`          |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$diagnosticProbabilities$asDF`

`as.data.frame(results$diagnosticProbabilities)`
