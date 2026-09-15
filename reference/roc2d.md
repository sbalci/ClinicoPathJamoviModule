# Two-Dimensional ROC Analysis

Two-dimensional ROC analysis for evaluating diagnostic accuracy of dual
biomarker combinations or multi-parametric tests. When two biomarkers
are measured simultaneously (e.g., ER and PR in breast cancer, dual IHC
markers, imaging plus biomarker), 2D ROC analysis finds the optimal
combined decision rule in two-dimensional marker space. Unlike analyzing
markers separately and combining results, 2D ROC jointly optimizes both
thresholds to maximize diagnostic accuracy. Provides joint
sensitivity/specificity, optimal threshold pairs, 3D ROC surface
visualization, and comparison to single-marker performance. Essential
for multiplex assays, dual biomarker panels, and situations where
neither marker alone is sufficient but combination improves diagnosis.
Applications include ER/PR combinations, dual IHC (e.g., p16/Ki67),
imaging radiomics plus molecular markers, and any scenario with two
continuous diagnostic measurements.

## Usage

``` r
roc2d(
  data,
  marker1,
  marker2,
  outcome,
  positive_level,
  decision_rule = "linear",
  compare_single_markers = TRUE,
  plot_2d_roc_surface = TRUE,
  plot_threshold_region = TRUE,
  show_interpretation = TRUE
)
```

## Arguments

- data:

  .

- marker1:

  .

- marker2:

  .

- outcome:

  .

- positive_level:

  .

- decision_rule:

  .

- compare_single_markers:

  .

- plot_2d_roc_surface:

  .

- plot_threshold_region:

  .

- show_interpretation:

  .

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`       |     |     |     |     | a html   |
| `results$individualAUCs`     |     |     |     |     | a table  |
| `results$combinationRules`   |     |     |     |     | a table  |
| `results$optimalCombination` |     |     |     |     | a table  |
| `results$roc2dPlot`          |     |     |     |     | an image |
| `results$interpretation`     |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$individualAUCs$asDF`

`as.data.frame(results$individualAUCs)`
