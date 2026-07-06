# Decision Panel Optimization

Optimize diagnostic test panels by evaluating various combination
strategies including parallel testing (cotest), sequential testing, and
repeated tests. Creates decision trees to minimize cost while maximizing
accuracy.

## Usage

``` r
decisionpanel(
  data,
  test1,
  test1Positive,
  test2,
  test2Positive,
  test3,
  test3Positive,
  test4,
  test4Positive,
  test5,
  test5Positive,
  gold,
  goldPositive,
  useCosts = FALSE,
  testCosts = "",
  fpCost = 100,
  fnCost = 1000,
  strategies = "all",
  parallelRules = "any",
  customThreshold = 2,
  maxTests = 3,
  sequentialStop = "positive",
  optimizationCriteria = "accuracy",
  panelOptimization = "forward",
  minImprovement = 0.01,
  redundancyThreshold = 0.7,
  minSensitivity = 0.8,
  minSpecificity = 0.8,
  createTree = FALSE,
  treeMethod = "costSensitive",
  maxDepth = 5,
  minSplit = 20,
  showAllCombinations = FALSE,
  topN = 10,
  compareStrategies = TRUE,
  bootstrap = FALSE,
  bootReps = 1000,
  plotTree = TRUE,
  plotComparison = TRUE,
  plotCostEffect = TRUE,
  plotROC = FALSE,
  prevalence = 0,
  crossValidate = FALSE,
  nFolds = 5,
  seed = 12345,
  showProgress = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- test1:

  First diagnostic test variable.

- test1Positive:

  The positive level for Test 1.

- test2:

  Second diagnostic test variable.

- test2Positive:

  The positive level for Test 2.

- test3:

  Third diagnostic test variable (optional).

- test3Positive:

  The positive level for Test 3.

- test4:

  Fourth diagnostic test variable (optional).

- test4Positive:

  The positive level for Test 4.

- test5:

  Fifth diagnostic test variable (optional).

- test5Positive:

  The positive level for Test 5.

- gold:

  The gold standard variable for disease classification.

- goldPositive:

  The level of the gold standard variable that indicates disease
  presence.

- useCosts:

  Include cost considerations in the optimization process.

- testCosts:

  Comma-separated costs for each test in the same order as selected
  tests. Example: "10,25,50" for three tests.

- fpCost:

  Cost or harm associated with a false positive result.

- fnCost:

  Cost or harm associated with a false negative result.

- strategies:

  Which testing strategies to evaluate in the analysis.

- parallelRules:

  Rule for combining results in parallel testing.

- customThreshold:

  Number of positive tests required for overall positive result (when
  using custom rule).

- maxTests:

  Maximum number of tests to combine in any strategy.

- sequentialStop:

  When to stop testing in sequential strategies.

- optimizationCriteria:

  Primary criterion for optimizing test panels.

- panelOptimization:

  Method for building optimal test panels.

- minImprovement:

  Minimum improvement in performance metric required to add a test to
  the panel.

- redundancyThreshold:

  Correlation threshold above which tests are considered redundant.

- minSensitivity:

  Minimum sensitivity constraint for panel selection.

- minSpecificity:

  Minimum specificity constraint for panel selection.

- createTree:

  Generate an optimal decision tree for test sequencing.

- treeMethod:

  Method for constructing the decision tree.

- maxDepth:

  Maximum depth of the decision tree.

- minSplit:

  Minimum number of observations required to split a node.

- showAllCombinations:

  Display performance metrics for all possible test combinations.

- topN:

  Number of best-performing panels to display in results.

- compareStrategies:

  Show comparative analysis of different testing strategies.

- bootstrap:

  Calculate bootstrap confidence intervals for performance metrics.

- bootReps:

  Number of bootstrap replications for confidence intervals.

- plotTree:

  Display visual representation of the optimal decision tree.

- plotComparison:

  Create comparison plots for different testing strategies.

- plotCostEffect:

  Create cost-effectiveness frontier plot.

- plotROC:

  Display ROC curves for top performing panels.

- prevalence:

  Known disease prevalence (0 = use sample prevalence).

- crossValidate:

  Perform k-fold cross-validation for panel performance.

- nFolds:

  Number of folds for cross-validation.

- seed:

  Random seed for reproducibility in bootstrap and cross-validation.

- showProgress:

  Display progress indicators for long-running operations like bootstrap
  and cross-validation.

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$summary`                |     |     |     |     | a html   |
| `results$clinicalSummary`        |     |     |     |     | a html   |
| `results$clinicalInterpretation` |     |     |     |     | a html   |
| `results$clinicalReport`         |     |     |     |     | a html   |
| `results$clinicalWarnings`       |     |     |     |     | a html   |
| `results$optimalPanel`           |     |     |     |     | a table  |
| `results$strategyComparison`     |     |     |     |     | a table  |
| `results$individualTests`        |     |     |     |     | a table  |
| `results$panelBuilding`          |     |     |     |     | a table  |
| `results$optimalPanelsBySize`    |     |     |     |     | a table  |
| `results$testRedundancy`         |     |     |     |     | a table  |
| `results$incrementalValue`       |     |     |     |     | a table  |
| `results$resultImportance`       |     |     |     |     | a table  |
| `results$allCombinations`        |     |     |     |     | a table  |
| `results$treeStructure`          |     |     |     |     | a html   |
| `results$treePerformance`        |     |     |     |     | a table  |
| `results$crossValidation`        |     |     |     |     | a table  |
| `results$bootstrapResults`       |     |     |     |     | a table  |
| `results$treeVisualization`      |     |     |     |     | an image |
| `results$strategyComparisonPlot` |     |     |     |     | an image |
| `results$costEffectivenessPlot`  |     |     |     |     | an image |
| `results$rocCurvesPlot`          |     |     |     |     | an image |
| `results$recommendations`        |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$optimalPanel$asDF`

`as.data.frame(results$optimalPanel)`
