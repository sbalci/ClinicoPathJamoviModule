# Combine Medical Decision Tests - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `decisioncombine`
- **Module**: `meddecide`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `gold` | UI Control `gold` | `self$options$gold` | Output item / Table |
| `goldPositive` | UI Control `goldPositive` | `self$options$goldPositive` | Output item / Table |
| `test1` | UI Control `test1` | `self$options$test1` | Output item / Table |
| `test1Positive` | UI Control `test1Positive` | `self$options$test1Positive` | Output item / Table |
| `test2` | UI Control `test2` | `self$options$test2` | Output item / Table |
| `test2Positive` | UI Control `test2Positive` | `self$options$test2Positive` | Output item / Table |
| `test3` | UI Control `test3` | `self$options$test3` | Output item / Table |
| `test3Positive` | UI Control `test3Positive` | `self$options$test3Positive` | Output item / Table |
| `showIndividual` | UI Control `showIndividual` | `self$options$showIndividual` | Output item / Table |
| `showFrequency` | UI Control `showFrequency` | `self$options$showFrequency` | Output item / Table |
| `showBarPlot` | UI Control `showBarPlot` | `self$options$showBarPlot` | Output item / Table |
| `showHeatmap` | UI Control `showHeatmap` | `self$options$showHeatmap` | Output item / Table |
| `showForest` | UI Control `showForest` | `self$options$showForest` | Output item / Table |
| `showDecisionTree` | UI Control `showDecisionTree` | `self$options$showDecisionTree` | Output item / Table |
| `showRecommendation` | UI Control `showRecommendation` | `self$options$showRecommendation` | Output item / Table |
| `addedPattern` | UI Control `addedPattern` | `self$options$addedPattern` | Output item / Table |
| `showAbout` | UI Control `showAbout` | `self$options$showAbout` | Output item / Table |
| `filterStatistic` | UI Control `filterStatistic` | `self$options$filterStatistic` | Output item / Table |
| `filterPattern` | UI Control `filterPattern` | `self$options$filterPattern` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/decisioncombine.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

