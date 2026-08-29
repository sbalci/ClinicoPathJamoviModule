# Summary of Continuous Variables - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `summarydata`
- **Module**: `ExplorationT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `vars` | UI Control `vars` | `self$options$vars` | Output item / Table |
| `distr` | UI Control `distr` | `self$options$distr` | Output item / Table |
| `decimal_places` | UI Control `decimal_places` | `self$options$decimal_places` | Output item / Table |
| `outliers` | UI Control `outliers` | `self$options$outliers` | Output item / Table |
| `report_sentences` | UI Control `report_sentences` | `self$options$report_sentences` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/summarydata.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

