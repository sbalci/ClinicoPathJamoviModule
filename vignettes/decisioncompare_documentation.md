# Compare Medical Decision Tests - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `decisioncompare`
- **Module**: `meddecide`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `gold` | UI Control `gold` | `self$options$gold` | Output item / Table |
| `goldPositive` | UI Control `goldPositive` | `self$options$goldPositive` | Output item / Table |
| `goldNegative` | UI Control `goldNegative` | `self$options$goldNegative` | Output item / Table |
| `test1` | UI Control `test1` | `self$options$test1` | Output item / Table |
| `test1Positive` | UI Control `test1Positive` | `self$options$test1Positive` | Output item / Table |
| `test1Negative` | UI Control `test1Negative` | `self$options$test1Negative` | Output item / Table |
| `test2` | UI Control `test2` | `self$options$test2` | Output item / Table |
| `test2Positive` | UI Control `test2Positive` | `self$options$test2Positive` | Output item / Table |
| `test2Negative` | UI Control `test2Negative` | `self$options$test2Negative` | Output item / Table |
| `test3` | UI Control `test3` | `self$options$test3` | Output item / Table |
| `test3Positive` | UI Control `test3Positive` | `self$options$test3Positive` | Output item / Table |
| `test3Negative` | UI Control `test3Negative` | `self$options$test3Negative` | Output item / Table |
| `pp` | UI Control `pp` | `self$options$pp` | Output item / Table |
| `pprob` | UI Control `pprob` | `self$options$pprob` | Output item / Table |
| `od` | UI Control `od` | `self$options$od` | Output item / Table |
| `fnote` | UI Control `fnote` | `self$options$fnote` | Output item / Table |
| `ci` | UI Control `ci` | `self$options$ci` | Output item / Table |
| `plot` | UI Control `plot` | `self$options$plot` | Output item / Table |
| `excludeIndeterminate` | UI Control `excludeIndeterminate` | `self$options$excludeIndeterminate` | Output item / Table |
| `radarplot` | UI Control `radarplot` | `self$options$radarplot` | Output item / Table |
| `heatmap` | UI Control `heatmap` | `self$options$heatmap` | Output item / Table |
| `opa` | UI Control `opa` | `self$options$opa` | Output item / Table |
| `niMargin` | UI Control `niMargin` | `self$options$niMargin` | Output item / Table |
| `useOpaCriterion` | UI Control `useOpaCriterion` | `self$options$useOpaCriterion` | Output item / Table |
| `ciMethod` | UI Control `ciMethod` | `self$options$ciMethod` | Output item / Table |
| `stratify` | UI Control `stratify` | `self$options$stratify` | Output item / Table |
| `statComp` | UI Control `statComp` | `self$options$statComp` | Output item / Table |
| `showSummary` | UI Control `showSummary` | `self$options$showSummary` | Output item / Table |
| `showExplanations` | UI Control `showExplanations` | `self$options$showExplanations` | Output item / Table |
| `showReportSentence` | UI Control `showReportSentence` | `self$options$showReportSentence` | Output item / Table |
| `showDescriptiveReport` | UI Control `showDescriptiveReport` | `self$options$showDescriptiveReport` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/decisioncompare.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

