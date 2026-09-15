# Decision Tree Graph for Cost-Effectiveness Analysis

Creates interactive decision tree visualizations for medical
cost-effectiveness analysis

Creates decision tree graphs for cost-effectiveness analysis with
typical decision nodes, chance nodes, and terminal nodes. Supports
visualization of treatment pathways, probabilities, costs, and outcomes.

## Usage

``` r
decisiongraph(
  data,
  treeType = "costeffectiveness",
  clinicalPreset = "none",
  decisions,
  healthStates,
  transitionProbs,
  cycleLength = 1,
  probabilities,
  costs,
  utilities,
  outcomes,
  layout = "horizontal",
  nodeShapes = TRUE,
  showProbabilities = TRUE,
  showCosts = TRUE,
  showUtilities = TRUE,
  calculateExpectedValues = TRUE,
  calculateNMB = TRUE,
  willingnessToPay = 50000,
  sensitivityAnalysis = FALSE,
  discountRate = 0.03,
  separateDiscountRates = FALSE,
  discountRateCost = 0.03,
  discountRateUtility = 0.015,
  timeHorizon = 10,
  nodeLabels = TRUE,
  branchLabels = TRUE,
  colorScheme = "medical",
  summaryTable = TRUE,
  tornado = FALSE,
  decisionComparison = TRUE,
  incrementalAnalysis = TRUE,
  dominanceAnalysis = TRUE,
  icer_confidence_intervals = FALSE,
  probabilisticAnalysis = FALSE,
  psa_advanced_outputs = FALSE,
  numSimulations = 1000,
  markovAdvanced = FALSE,
  tunnelStates,
  timeVaryingTransitions = FALSE,
  ageSpecificTransitions,
  chanceNodeMethod = "simple",
  riskAdjustment = FALSE,
  cycleCorrection = TRUE,
  cohortTrace = FALSE,
  cohortSize = 1000,
  valueOfInformation = FALSE,
  evpi_parameters,
  budgetImpactAnalysis = FALSE,
  targetPopulationSize = 10000,
  marketPenetration = 0.5,
  ceacThresholds = "0,100000,5000",
  psa_distributions = "normal",
  correlatedParameters = FALSE,
  correlationMatrix,
  performanceMode = "standard",
  memoryOptimization = TRUE,
  parallelProcessing = FALSE
)
```

## Arguments

- data:

  The data as a data frame containing decision tree parameters.

- treeType:

  Type of decision tree to create.

- clinicalPreset:

  Pre-configured analysis templates for common clinical decision
  analysis scenarios.

- decisions:

  Variables representing decision nodes (treatment options).

- healthStates:

  Variables defining health states for Markov model.

- transitionProbs:

  Variables containing transition probabilities between health states.

- cycleLength:

  Length of each Markov cycle in years.

- probabilities:

  Variables containing probability values for chance nodes.

- costs:

  Variables containing cost values for terminal nodes.

- utilities:

  Variables containing utility/effectiveness values.

- outcomes:

  Variables representing clinical outcomes.

- layout:

  Layout orientation for the decision tree.

- nodeShapes:

  Use different shapes for different node types.

- showProbabilities:

  Display probability values on tree branches.

- showCosts:

  Display cost values at terminal nodes.

- showUtilities:

  Display utility/effectiveness values.

- calculateExpectedValues:

  Calculate and display expected costs and utilities.

- calculateNMB:

  Calculate net monetary benefit (NMB) for decision paths using
  threshold analysis.

- willingnessToPay:

  Willingness to pay threshold (cost per QALY) for NMB calculations.

- sensitivityAnalysis:

  Perform one-way sensitivity analysis on key parameters.

- discountRate:

  Annual discount rate for future costs and benefits when same rate for
  both.

- separateDiscountRates:

  Use separate discount rates for costs and utilities.

- discountRateCost:

  Annual discount rate for costs.

- discountRateUtility:

  Annual discount rate for utilities and QALYs.

- timeHorizon:

  Time horizon for the analysis in years.

- nodeLabels:

  Display descriptive labels for nodes.

- branchLabels:

  Display labels on tree branches.

- colorScheme:

  Color scheme for the decision tree visualization.

- summaryTable:

  Show summary table with expected values and cost-effectiveness ratios.

- tornado:

  Create tornado diagram for sensitivity analysis.

- decisionComparison:

  Show comparison table with NMB scores for all decision paths.

- incrementalAnalysis:

  Perform incremental cost-effectiveness ratio (ICER) calculations.

- dominanceAnalysis:

  Identify dominated and extendedly dominated strategies in ICER
  analysis.

- icer_confidence_intervals:

  Calculate confidence intervals for ICERs using bootstrap or PSA
  results.

- probabilisticAnalysis:

  Perform probabilistic sensitivity analysis using Monte Carlo
  simulation.

- psa_advanced_outputs:

  Generate advanced PSA outputs (CEAC, EVPI, net benefit distributions).

- numSimulations:

  Number of Monte Carlo simulations for probabilistic analysis.

- markovAdvanced:

  Enable advanced Markov modeling features.

- tunnelStates:

  Define tunnel states for temporary health states.

- timeVaryingTransitions:

  Allow transition probabilities to change over time.

- ageSpecificTransitions:

  Variables defining age-specific transition rates.

- chanceNodeMethod:

  Method for calculating probabilities at chance nodes.

- riskAdjustment:

  Apply risk adjustment to probabilities based on patient
  characteristics.

- cycleCorrection:

  Apply half-cycle correction for more accurate discounting in Markov
  models.

- cohortTrace:

  Generate cohort trace showing population distribution across health
  states over time.

- cohortSize:

  Size of hypothetical cohort for trace analysis and budget impact
  modeling.

- valueOfInformation:

  Perform Expected Value of Perfect Information (EVPI) and partial EVPI
  analysis.

- evpi_parameters:

  Parameters for partial EVPI analysis.

- budgetImpactAnalysis:

  Perform budget impact analysis comparing total costs of alternative
  strategies.

- targetPopulationSize:

  Size of target population for budget impact calculations.

- marketPenetration:

  Expected market penetration rate for new intervention.

- ceacThresholds:

  Cost-effectiveness acceptability curve thresholds (min,max,step).

- psa_distributions:

  Default probability distribution for uncertain parameters in PSA.

- correlatedParameters:

  Account for correlations between uncertain parameters in PSA.

- correlationMatrix:

  Variables defining correlation structure between uncertain parameters.

- performanceMode:

  Balance between speed and completeness of analysis.

- memoryOptimization:

  Enable memory-efficient processing for large simulations.

- parallelProcessing:

  Enable parallel processing for faster PSA calculations.

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$treeplot` |  |  |  |  | Decision tree visualization with nodes and branches |
| `results$summaryTable` |  |  |  |  | a table |
| `results$nodeTable` |  |  |  |  | a table |
| `results$tornadoplot` |  |  |  |  | Sensitivity analysis tornado diagram |
| `results$sensitivityTable` |  |  |  |  | a table |
| `results$text1` |  |  |  |  | a html |
| `results$text2` |  |  |  |  | a html |
| `results$markovTable` |  |  |  |  | a table |
| `results$markovCohortTable` |  |  |  |  | a table |
| `results$markovPlot` |  |  |  |  | Markov state transition diagram |
| `results$decisionComparisonTable` |  |  |  |  | a table |
| `results$nmbAnalysis` |  |  |  |  | Net monetary benefit calculation details |
| `results$icerTable` |  |  |  |  | a table |
| `results$psaResults` |  |  |  |  | Monte Carlo simulation results |
| `results$ceacPlot` |  |  |  |  | CEAC showing probability of cost-effectiveness |
| `results$scatterPlot` |  |  |  |  | Cost-effectiveness plane scatter plot |
| `results$executiveSummary` |  |  |  |  | Executive summary of analysis results and key findings |
| `results$glossary` |  |  |  |  | Comprehensive glossary of decision analysis terminology |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summaryTable$asDF`

`as.data.frame(results$summaryTable)`

## Details

This module provides comprehensive decision tree visualization
capabilities including:

- Decision nodes (square), chance nodes (circle), and terminal nodes
  (triangle)

- Cost-effectiveness analysis with expected value calculations

- Sensitivity analysis with tornado diagrams

- Multiple layout options and customizable visualization

## Usage

1.  Define decision variables, probabilities, costs, and utilities

2.  Select tree type and layout options

3.  View decision tree graph with calculated expected values
