# Automatic Plot Selection - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `statsplot2`
- **Module**: `JJStatsPlot`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `dep` | UI Control `dep` | `self$options$dep` | Output item / Table |
| `group` | UI Control `group` | `self$options$group` | Output item / Table |
| `grvar` | UI Control `grvar` | `self$options$grvar` | Output item / Table |
| `direction` | UI Control `direction` | `self$options$direction` | Output item / Table |
| `distribution` | UI Control `distribution` | `self$options$distribution` | Output item / Table |
| `alluvsty` | UI Control `alluvsty` | `self$options$alluvsty` | Output item / Table |
| `excl` | UI Control `excl` | `self$options$excl` | Output item / Table |
| `sampleLarge` | UI Control `sampleLarge` | `self$options$sampleLarge` | Output item / Table |
| `sampleThreshold` | UI Control `sampleThreshold` | `self$options$sampleThreshold` | Output item / Table |
| `sampleSize` | UI Control `sampleSize` | `self$options$sampleSize` | Output item / Table |
| `seed` | UI Control `seed` | `self$options$seed` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/statsplot2.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

