# Benford Analysis - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `benford`
- **Module**: `ExplorationT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `var` | UI Control `var` | `self$options$var` | Output item / Table |
| `digits` | UI Control `digits` | `self$options$digits` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/benford.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

