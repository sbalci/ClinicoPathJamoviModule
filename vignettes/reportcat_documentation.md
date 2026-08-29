# Summary of Categorical Variables - Feature Mapping Specification

## Feature-to-Code Mapping

- **Analysis Function**: `reportcat`
- **Module**: `ExplorationT`

| Feature / Option | UI Binding | Backend Handler | Target Result Item |
| :--- | :--- | :--- | :--- |
| `data` | UI Control `data` | `self$options$data` | Output item / Table |
| `vars` | UI Control `vars` | `self$options$vars` | Output item / Table |

## Verification Checklist

- [x] All options defined in `.a.yaml` have matching UI bindings in `.u.yaml`.
- [x] Backend `R/reportcat.b.R` references all declared options safely.
- [x] Results definitions in `.r.yaml` correspond to populated output containers.

