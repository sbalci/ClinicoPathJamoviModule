# Combine Medical Decision Tests

Systematic evaluation of diagnostic test combinations. Analyzes all
possible test result patterns (2-test: 4 patterns, 3-test: 8 patterns)
against a gold standard to identify optimal testing strategies.
Calculates sensitivity, specificity, PPV, NPV, and accuracy for each
pattern combination.

## Usage

``` r
decisioncombine(
  data,
  gold = NULL,
  goldPositive,
  test1 = NULL,
  test1Positive,
  test2 = NULL,
  test2Positive,
  test3 = NULL,
  test3Positive,
  showIndividual = FALSE,
  showFrequency = FALSE,
  showBarPlot = FALSE,
  showHeatmap = FALSE,
  showForest = FALSE,
  showDecisionTree = FALSE,
  showRecommendation = FALSE,
  addPatternToData = FALSE,
  filterStatistic = "all",
  filterPattern = "all"
)
```

## Arguments

- data:

  The data as a data frame.

- gold:

  The gold standard reference variable representing true disease status.

- goldPositive:

  The level indicating presence of disease in the gold standard.

- test1:

  The first diagnostic test variable.

- test1Positive:

  The level representing a positive result for Test 1.

- test2:

  The second diagnostic test variable for combination analysis.

- test2Positive:

  The level representing a positive result for Test 2.

- test3:

  The third diagnostic test variable for 3-way combination analysis.

- test3Positive:

  The level representing a positive result for Test 3.

- showIndividual:

  Boolean to show individual test performance tables.

- showFrequency:

  Boolean to show frequency tables.

- showBarPlot:

  Boolean to display bar chart visualization.

- showHeatmap:

  Boolean to display heatmap visualization.

- showForest:

  Boolean to display forest plot.

- showDecisionTree:

  Boolean to display the decision-space (sensitivity vs specificity)
  scatter plot.

- showRecommendation:

  Boolean to show optimal pattern recommendation table.

- addPatternToData:

  Boolean to add test pattern column to the dataset.

- filterStatistic:

  Character indicating which statistic to display in the plots (default:
  all).

- filterPattern:

  Character indicating which pattern type to display in the plots
  (default: all).

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$combinationTable` |  |  |  |  | Counts and diagnostic performance metrics for each test combination pattern and clinical strategy, including prevalence, balanced accuracy, Youden's J, likelihood ratios, and diagnostic odds ratios |
| `results$combinationTableCI` |  |  |  |  | Wilson score 95 percent confidence intervals for sensitivity, specificity, PPV, NPV and accuracy, shown as percentages to match the combination table above. Likelihood ratios and the diagnostic odds ratio are unbounded ratios rather than proportions, so they appear in their own table below. |
| `results$combinationTableCIRatios` |  |  |  |  | Log-scale 95 percent confidence intervals for LR+, LR- and the diagnostic odds ratio. These are ratios on an unbounded scale, so they are reported separately from the proportions above rather than sharing a column with them. |
| `results$goldFreqTable` |  |  |  |  | Frequency distribution of the gold standard (reference) test showing counts and percentages for each level |
| `results$crossTabTable` |  |  |  |  | Cross-tabulation showing how test combination patterns align with gold standard results |
| `results$individualTest1$test1Contingency` |  |  |  |  | a table |
| `results$individualTest1$test1Stats` |  |  |  |  | a table |
| `results$individualTest2$test2Contingency` |  |  |  |  | a table |
| `results$individualTest2$test2Stats` |  |  |  |  | a table |
| `results$individualTest3$test3Contingency` |  |  |  |  | a table |
| `results$individualTest3$test3Stats` |  |  |  |  | a table |
| `results$barPlot` |  |  |  |  | Grouped bar chart comparing sensitivity, specificity, PPV, NPV, and accuracy across test combinations |
| `results$heatmapPlot` |  |  |  |  | Color-coded heatmap showing all diagnostic metrics for each test pattern |
| `results$forestPlot` |  |  |  |  | Forest plot displaying 95 percent confidence intervals for key diagnostic metrics |
| `results$decisionTreePlot` |  |  |  |  | Decision-space scatter plot positioning each test pattern by its sensitivity and specificity, with point size scaled by Youden's J |
| `results$recommendationTable` |  |  |  |  | Recommended optimal test combination pattern based on Youden index and clinical performance metrics |
| `results$addedPattern` |  |  |  |  | an output |
| `results$notices` |  |  |  |  | a html |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$combinationTable$asDF`

`as.data.frame(results$combinationTable)`
