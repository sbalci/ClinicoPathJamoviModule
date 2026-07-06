# Decision Curve Analysis

Decision Curve Analysis for evaluating the clinical utility of
prediction models and diagnostic tests. Calculates net benefit across
threshold probabilities to determine if using a model provides more
benefit than default strategies.

## Usage

``` r
decisioncurve(
  data,
  outcome = NULL,
  outcomePositive,
  models,
  modelNames = "",
  thresholdRange = "clinical",
  thresholdMin = 0.05,
  thresholdMax = 0.5,
  thresholdStep = 0.01,
  showTable = FALSE,
  selectedThresholds = "0.05, 0.10, 0.15, 0.20, 0.25, 0.30",
  showPlot = FALSE,
  plotStyle = "standard",
  showReferenceLinesLabels = FALSE,
  highlightRange = FALSE,
  highlightMin = 0.1,
  highlightMax = 0.3,
  calculateClinicalImpact = FALSE,
  populationSize = 1000,
  showInterventionAvoided = FALSE,
  confidenceIntervals = FALSE,
  bootReps = 1000,
  ciLevel = 0.95,
  showOptimalThreshold = FALSE,
  compareModels = FALSE,
  weightedAUC = FALSE,
  clinicalDecisionRule = FALSE,
  decisionRuleVar = NULL,
  decisionRulePositive,
  decisionRuleLabel = "Clinical Rule",
  showClinicalImpactPlot = FALSE,
  showNetBenefitCI = FALSE,
  costBenefitAnalysis = FALSE,
  testCost = 100,
  treatmentCost = 1000,
  benefitCorrectTreatment = 10000,
  harmFalseTreatment = 500,
  showStandardizedNetBenefit = FALSE,
  multiModelComparison = FALSE,
  comparisonMethod = "bootstrap",
  showDecisionConsequences = FALSE,
  resourceUtilization = FALSE,
  showRelativeUtility = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- outcome:

  Binary outcome variable (0/1 or FALSE/TRUE). This represents the
  condition or event you want to predict.

- outcomePositive:

  Which level of the outcome variable represents the positive case
  (presence of condition/event).

- models:

  Variables representing predicted probabilities or risk scores from
  different models. Can include multiple models for comparison.

- modelNames:

  Optional comma-separated list of names for the models. If not
  provided, variable names will be used.

- thresholdRange:

  Range of threshold probabilities to evaluate.

- thresholdMin:

  Minimum threshold probability when using custom range.

- thresholdMax:

  Maximum threshold probability when using custom range.

- thresholdStep:

  Step size between threshold probabilities.

- showTable:

  Display table with net benefit values at selected thresholds.

- selectedThresholds:

  Comma-separated list of threshold probabilities to display in table.

- showPlot:

  Display the decision curve plot.

- plotStyle:

  Style of the decision curve plot.

- showReferenceLinesLabels:

  Show labels for "Treat All" and "Treat None" reference lines.

- highlightRange:

  Highlight a clinically relevant threshold range on the plot.

- highlightMin:

  Minimum threshold for highlighted range.

- highlightMax:

  Maximum threshold for highlighted range.

- calculateClinicalImpact:

  Calculate clinical impact metrics (number needed to screen, etc.).

- populationSize:

  Population size for calculating clinical impact metrics.

- showInterventionAvoided:

  Show how many unnecessary interventions are avoided compared to
  treat-all.

- confidenceIntervals:

  Calculate bootstrap confidence intervals for net benefit curves.

- bootReps:

  Number of bootstrap replications for confidence intervals.

- ciLevel:

  Confidence level for bootstrap confidence intervals.

- showOptimalThreshold:

  Identify and display optimal threshold probabilities for each model.

- compareModels:

  Calculate statistical tests for comparing model performance.

- weightedAUC:

  Calculate weighted area under the decision curve.

- clinicalDecisionRule:

  Add clinical decision rule analysis and comparison with models.

- decisionRuleVar:

  Binary variable indicating whether the clinical rule recommends
  intervention (1/0 or yes/no).

- decisionRulePositive:

  Which level of the rule variable indicates a positive recommendation
  (treat/intervene).

- decisionRuleLabel:

  Label for the clinical decision rule in plots and tables.

- showClinicalImpactPlot:

  Display clinical impact plot showing number of true positives and
  false positives per population unit across threshold probabilities.

- showNetBenefitCI:

  Display confidence intervals around net benefit curves in the decision
  curve plot.

- costBenefitAnalysis:

  Perform cost-benefit analysis incorporating costs of testing,
  treatment, and outcomes.

- testCost:

  Cost per test or screening procedure (in currency units).

- treatmentCost:

  Cost of treatment or intervention (in currency units).

- benefitCorrectTreatment:

  Monetary benefit or QALY-equivalent value of correctly treating a true
  positive.

- harmFalseTreatment:

  Cost or harm of treating a false positive (unnecessary treatment).

- showStandardizedNetBenefit:

  Calculate and display standardized net benefit (net benefit per 100
  patients).

- multiModelComparison:

  Perform comprehensive pairwise comparisons between all models with
  statistical tests.

- comparisonMethod:

  Statistical method for comparing decision curves between models.

- showDecisionConsequences:

  Show detailed table of decision consequences (TP, FP, TN, FN) at
  selected thresholds.

- resourceUtilization:

  Calculate resource utilization metrics (tests performed, treatments
  administered).

- showRelativeUtility:

  Display relative utility curve comparing each model to default
  strategies.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | a html |
| `results$procedureNotes` |  |  |  |  | a html |
| `results$notices` |  |  |  |  | a html |
| `results$resultsTable` |  |  |  |  | a table |
| `results$optimalTable` |  |  |  |  | a table |
| `results$clinicalImpactTable` |  |  |  |  | a table |
| `results$comparisonTable` |  |  |  |  | a table |
| `results$weightedAUCTable` |  |  |  |  | a table |
| `results$dcaPlot` |  |  |  |  | an image |
| `results$clinicalImpactPlot` |  |  |  |  | Number of true/false positives per population unit |
| `results$interventionsAvoidedPlot` |  |  |  |  | an image |
| `results$summaryText` |  |  |  |  | a html |
| `results$costBenefitTable` |  |  |  |  | Cost-benefit analysis at selected thresholds |
| `results$decisionConsequencesTable` |  |  |  |  | Detailed consequences at selected thresholds |
| `results$modelComparisonEnhanced` |  |  |  |  | Pairwise statistical comparison of models |
| `results$resourceUtilizationTable` |  |  |  |  | Resource utilization at selected thresholds |
| `results$relativeUtilityPlot` |  |  |  |  | Relative utility compared to default strategies |
| `results$standardizedNetBenefitPlot` |  |  |  |  | Net benefit per 100 patients |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$resultsTable$asDF`

`as.data.frame(results$resultsTable)`
