# Confidence Interval Approach for the Number of Subjects Required - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `kappaSizeCI`
- **Module**: `meddecide` (`menuGroup: Power #meddecide`)

| Feature / Option | UI Binding | Backend Handler | Results affected (notices, text1, text_summary, text2, methodology) |
| :--- | :--- | :--- | :--- |
| `outcome` | UI Control `outcome` | `self$options$outcome` | all panes; engine choice |
| `citype` | UI Control `citype` | `self$options$citype` | all panes; critical value |
| `kappa0` | UI Control `kappa0` | `self$options$kappa0` | all panes |
| `kappaL` | UI Control `kappaL` | `self$options$kappaL` | all panes |
| `kappaU` | UI Control `kappaU` | `self$options$kappaU` | all panes (two-sided only) |
| `props` | UI Control `props` | `self$options$props` | all panes; Rare category panel |
| `raters` | UI Control `raters` | `self$options$raters` | all panes |
| `alpha` | UI Control `alpha` | `self$options$alpha` | all panes; confidence level |
| `showExplanations` | UI Control `showExplanations` | `self$options$showExplanations` | `text2`, `methodology` |
| `showSummary` | UI Control `showSummary` | `self$options$showSummary` | `text_summary` |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/kappaSizeCI.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

