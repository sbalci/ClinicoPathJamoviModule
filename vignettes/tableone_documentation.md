# Table One - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `tableone`
- **Module**: `ExplorationT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `vars` | UI Control `vars` | `self$options$vars` | Output item / Table |
| `sty` | UI Control `sty` | `self$options$sty` | Output item / Table |
| `excl` | UI Control `excl` | `self$options$excl` | Output item / Table |
| `showSummary` | UI Control `showSummary` | `self$options$showSummary` | Output item / Table |
| `showAbout` | UI Control `showAbout` | `self$options$showAbout` | Output item / Table |
| `showReportSentence` | UI Control `showReportSentence` | `self$options$showReportSentence` | Output item / Table |
| `nonnormal` | UI Control `nonnormal` | `self$options$nonnormal` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/tableone.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

